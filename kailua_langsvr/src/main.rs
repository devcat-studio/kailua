#[macro_use] extern crate log;
extern crate env_logger;
extern crate serde;
#[macro_use] extern crate serde_derive;
extern crate serde_json;
extern crate url;
extern crate futures;
extern crate futures_cpupool;
extern crate tokio_timer;
extern crate owning_ref;
extern crate num_cpus;
extern crate parking_lot;
#[macro_use] extern crate errln;
extern crate walkdir;
#[macro_use] extern crate parse_generics_shim;
extern crate kailua_env;
#[macro_use] extern crate kailua_diag;
extern crate kailua_syntax;
extern crate kailua_check;
extern crate kailua_workspace;
extern crate kailua_langsvr_protocol as protocol;

mod fmtutils;
pub mod server;
pub mod diags;
pub mod workspace;
pub mod futureutils;
pub mod message;
pub mod ops;

use std::io;
use std::sync::Arc;
use parking_lot::RwLock;
use futures::{Future, BoxFuture};
use tokio_timer::Timer;

use server::Server;
use futureutils::{CancelToken, CancelError};
use diags::ReportTree;
use workspace::{Workspace, WorkspaceFile};

fn connect_to_client() -> Server {
    use std::env;
    use std::net::{SocketAddr, TcpStream};
    use std::process;
    use server::Server;

    let mut server = None;
    if let Some(firstopt) = env::args().nth(1) {
        if firstopt == "--packets-via-stdio" {
            server = Some(Server::from_stdio());
        } else if firstopt.starts_with("--packets-via-tcp=") {
            if let Ok(ip) = firstopt["--packets-via-tcp=".len()..].parse::<SocketAddr>() {
                match TcpStream::connect(ip).and_then(Server::from_tcp_stream) {
                    Ok(s) => server = Some(s),
                    Err(e) => {
                        errln!("*** Couldn't connect to the client: {}", e);
                        process::exit(1);
                    }
                }
            }
        }
    }

    if server.is_none() {
        errln!("Kailua language server is intended to be used with VS Code. \
                Use the Kailua extension instead.");
        process::exit(1);
    }

    server.unwrap()
}

// initialization options supposed to be sent from the extension
struct InitOptions {
    default_locale: kailua_diag::Locale,
}

impl Default for InitOptions {
    fn default() -> InitOptions {
        InitOptions { default_locale: kailua_diag::Locale::from("en") }
    }
}

fn parse_init_options(opts: Option<serde_json::Value>) -> InitOptions {
    #[derive(Deserialize)]
    struct Options {
        default_locale: String,
    }

    if let Some(opts) = opts {
        if let Ok(opts) = serde_json::from_value::<Options>(opts) {
            if let Some(locale) = kailua_diag::Locale::new(&opts.default_locale) {
                return InitOptions { default_locale: locale };
            }
        }
    }
    InitOptions::default()
}

// both show the message (more visible but will be dismissed after a few seconds)
// and the diagnostics (less visible but will last for this session)
fn cannot_read_config(workspace: &Workspace, server: &Server, msg: &kailua_diag::Localize) {
    use url::Url;
    use protocol::*;

    let _ = server.send_notify(
        Method::ShowMessage,
        ShowMessageParams {
            type_: MessageType::Warning,
            message: workspace.localize(msg).to_string(),
        },
    );

    if let Ok(uri) = Url::from_file_path(&workspace.config_path_or_default()) {
        let _ = server.send_notify(
            Method::PublishDiagnostics,
            PublishDiagnosticsParams {
                uri: uri.to_string(),
                diagnostics: vec![
                    protocol::Diagnostic {
                        range: protocol::Range {
                            start: protocol::Position { line: 0, character: 0 },
                            end: protocol::Position { line: 0, character: 0 },
                        },
                        severity: Some(protocol::DiagnosticSeverity::Error),
                        code: None,
                        source: None,
                        message: workspace.localize(&message::RestartRequired {}).to_string(),
                    },
                ],
            }
        );
    }
}

fn initialize_workspace(server: &Server) -> Workspace {
    use std::path::PathBuf;
    use futures_cpupool::CpuPool;
    use protocol::*;
    use server::Received;
    use message as m;

    loop {
        let res = server.recv().unwrap();
        let req = if let Some(req) = res { req } else { continue };
        debug!("pre-init read: {:#?}", req);

        match req {
            Received::Request(id, Request::Initialize(params)) => {
                if let Some(dir) = params.rootPath {
                    let initopts = parse_init_options(params.initializationOptions);

                    // due to the current architecture, chained futures take the worker up
                    // without doing any work (fortunately, no CPU time as well).
                    // therefore we should prepare enough workers for concurrent execution.
                    //
                    // since the longest-running chain would be span-tokens-chunk-output,
                    // we need at least 4x the number of CPUs to use all CPUs at the worst case.
                    let nworkers = num_cpus::get() * 4;
                    let pool = Arc::new(CpuPool::new(nworkers));

                    let mut workspace = Workspace::new(PathBuf::from(dir), pool.clone(),
                                                       initopts.default_locale);

                    // try to read the config...
                    let config_res = workspace.read_config();
                    if config_res.is_ok() {
                        // ...then try to initially scan the directory (should happen before sending
                        // an initialize response so that changes reported later are not dups)
                        workspace.populate_watchlist();
                    }

                    let _ = server.send_ok(id, InitializeResult {
                        capabilities: ServerCapabilities {
                            /*
                            textDocumentSync: Some(TextDocumentSyncOptions {
                                openClose: true,
                                change: TextDocumentSyncKind::Full,
                                willSave: false,
                                willSaveWaitUntil: false,
                                save: Some(SaveOptions { includeText: false }),
                            }),
                            */
                            textDocumentSync: TextDocumentSyncKind::Full,
                            completionProvider: Some(CompletionOptions {
                                resolveProvider: false,
                                triggerCharacters:
                                    ".:ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                                       abcdefghijklmnopqrstuvwxyz".chars()
                                                                  .map(|c| c.to_string())
                                                                  .collect(),
                            }),
                            hoverProvider: true,
                            signatureHelpProvider: Some(SignatureHelpOptions {
                                triggerCharacters: vec!["(".to_string(), ",".to_string()],
                            }),
                            definitionProvider: true,
                            renameProvider: true,
                            ..Default::default()
                        },
                    });

                    // configuration error may include diagnostics and should be sent after init
                    if let Err(e) = workspace.read_config() {
                        cannot_read_config(&workspace, &server, &m::CannotReadConfig { error: &e });
                    }

                    return workspace;
                } else {
                    let _ = server.send_err(Some(id.clone()),
                                            error_codes::INTERNAL_ERROR,
                                            "no folder open, retry with an open folder",
                                            InitializeError { retry: true });
                }
            }

            // reply an error to the request (notifications are ignored)
            Received::Request(id, _) => {
                let _ = server.send_err(Some(id.clone()),
                                        error_codes::SERVER_NOT_INITIALIZED,
                                        "server hasn't been initialized yet",
                                        InitializeError { retry: false });
            }
            Received::Notification(_) => {}
        }
    }
}

fn send_diagnostics(server: Server, root: ReportTree) -> io::Result<()> {
    use std::path::Path;
    use std::collections::HashMap;
    use url::Url;
    use protocol::*;

    let mut diags = HashMap::new();
    for tree in root.trees() {
        if let Some(path) = tree.path() {
            diags.entry(path.to_owned()).or_insert(Vec::new());
        }
        for (path, diag) in tree.diagnostics() {
            diags.entry(path).or_insert(Vec::new()).push(diag);
        }
    }

    for (path, diags) in diags.into_iter() {
        let uri = Url::from_file_path(&Path::new(&path)).expect("no absolute path");
        server.send_notify(
            Method::PublishDiagnostics,
            PublishDiagnosticsParams { uri: uri.to_string(), diagnostics: diags }
        )?;
    }

    Ok(())
}

fn send_diagnostics_when_available<T, F>(server: Server,
                                         pool: &futures_cpupool::CpuPool,
                                         fut: futures::future::Shared<F>)
    where T: Send + Sync + 'static,
          F: Send + 'static + Future<Item=(T, ReportTree), Error=CancelError<ReportTree>>
{
    let fut = fut.then(move |res| {
        let diags = match res {
            Ok(ref value_and_diags) => &value_and_diags.1,
            Err(ref e) => match **e {
                CancelError::Canceled => return Ok(()),
                CancelError::Error(ref diags) => diags,
            },
        };
        send_diagnostics(server, diags.clone())
    });

    // this should be forgotten as we won't make use of its result
    // TODO chain to the currently running future + cancel token
    pool.spawn(fut).forget();
}

fn on_file_changed(file: &WorkspaceFile, server: Server, pool: &futures_cpupool::CpuPool) {
    send_diagnostics_when_available(server.clone(), pool, file.ensure_tokens());
    send_diagnostics_when_available(server, pool, file.ensure_chunk());
}

// in the reality, the "loop" is done via a chain of futures and the function immediately returns
fn checking_loop(server: Server, workspace: Arc<RwLock<Workspace>>,
                 timer: Timer) -> BoxFuture<(), ()> {
    use std::time::Duration;
    use futures;
    use futureutils::FutureExt;
    use message as m;

    let cancel_future = workspace.read().cancel_future();

    // wait for a bit before actually starting the check (if not requested by completion etc).
    // if the cancel was requested during the wait we quickly restart the loop.
    const DELAY_MILLIS: u64 = 750;
    cancel_future.clone().map(|_| true).erase_err().select({
        Future::map(timer.sleep(Duration::from_millis(DELAY_MILLIS)), |_| false).erase_err()
    }).erase_err().and_then(move |(canceled, _next)| {
        if canceled {
            // immediately restart the loop with a fresh CancelFuture
            return checking_loop(server, workspace, timer);
        }

        let output_fut = workspace.read().ensure_check_output();
        if let Ok(fut) = output_fut {
            let server_ = server.clone();
            fut.then(move |res| {
                // send diagnostics for this check
                let diags = match res {
                    Ok(ref value_and_diags) => Some(&value_and_diags.1),
                    Err(ref e) => match **e {
                        CancelError::Canceled => None,
                        CancelError::Error(ref diags) => Some(diags),
                    },
                };
                if let Some(diags) = diags {
                    let _ = send_diagnostics(server_, diags.clone());
                }
                Ok(())
            }).and_then(move |_| {
                // when the cancel was properly requested (even after the completion),
                // restart the loop; if there were any error (most possibly the panic) stop it.
                cancel_future
            }).and_then(move |_| {
                checking_loop(server, workspace, timer)
            }).boxed()
        } else {
            // the loop terminates, possibly with a message
            if workspace.read().has_read_config() {
                // avoid a duplicate message if kailua.json is missing
                cannot_read_config(&workspace.read(), &server, &m::NoStartPath {});
            }
            futures::finished(()).boxed()
        }
    }).erase_err().boxed()
}

fn main_loop(server: Server, workspace: Arc<RwLock<Workspace>>) {
    use std::collections::HashMap;
    use workspace::WorkspaceError;
    use protocol::*;
    use server::Received;

    let mut cancel_tokens: HashMap<Id, CancelToken> = HashMap::new();
    let timer = Timer::default();

    // launch the checking future, which will be executed throughout the entire loop
    let checking_fut = checking_loop(server.clone(), workspace.clone(), timer.clone());
    workspace.read().pool().spawn(checking_fut).forget();

    'restart: loop {
        let res = server.recv().unwrap();
        let req = if let Some(req) = res { req } else { continue };
        debug!("read: {:#?}", req);

        macro_rules! try_or_notify {
            ($e:expr) => (match $e {
                Ok(v) => v,
                Err(e) => {
                    let _ = server.send_err(None, error_codes::INTERNAL_ERROR, e.0, ());
                    continue 'restart;
                }
            })
        }

        match req {
            Received::Request(id, Request::Initialize(_)) => {
                let _ = server.send_err(Some(id), error_codes::INTERNAL_ERROR,
                                        "already initialized", InitializeError { retry: false });
            }

            Received::Notification(Notification::CancelRequest(params)) => {
                if let Some(token) = cancel_tokens.remove(&params.id) {
                    token.cancel();
                }
            }

            Received::Notification(Notification::DidOpenTextDocument(params)) => {
                let uri = params.textDocument.uri.clone();

                let ws = workspace.write();
                try_or_notify!(ws.open_file(params.textDocument));
                trace!("workspace: {:#?}", *ws);

                let pool = ws.pool().clone();
                let file = ws.file(&uri).unwrap();
                on_file_changed(&file, server.clone(), &pool);
            }

            Received::Notification(Notification::DidChangeTextDocument(params)) => {
                let uri = params.textDocument.uri;

                let ws = workspace.write();
                {
                    let pool = ws.pool().clone();
                    let mut file = try_or_notify!(ws.file(&uri).ok_or_else(|| {
                        WorkspaceError("file does not exist for changes")
                    }));

                    let mut e = Ok(());
                    for change in params.contentChanges {
                        e = e.or(file.apply_change(params.textDocument.version, change));
                    }
                    try_or_notify!(e);

                    on_file_changed(&file, server.clone(), &pool);
                }
                trace!("workspace: {:#?}", *ws);
            }

            Received::Notification(Notification::DidCloseTextDocument(params)) => {
                let ws = workspace.write();
                try_or_notify!(ws.close_file(&params.textDocument.uri));
                trace!("workspace: {:#?}", *ws);
            }

            Received::Notification(Notification::DidChangeWatchedFiles(params)) => {
                let ws = workspace.write();
                for ev in params.changes {
                    match ev.type_ {
                        FileChangeType::Created => { ws.on_file_created(&ev.uri); }
                        FileChangeType::Changed => { ws.on_file_changed(&ev.uri); }
                        FileChangeType::Deleted => { ws.on_file_deleted(&ev.uri); }
                    }
                }
                trace!("workspace: {:#?}", *ws);
            }

            Received::Request(id, Request::Completion(params)) => {
                let token = CancelToken::new();
                cancel_tokens.insert(id.clone(), token.clone());

                let uri = &params.textDocument.uri;
                let file = try_or_notify!(workspace.read().file(uri).ok_or_else(|| {
                    WorkspaceError("file does not exist for completion")
                }));

                complete(server.clone(), workspace.clone(), id, file, token, &params.position);
            }

            Received::Request(id, Request::Hover(params)) => {
                let token = CancelToken::new();
                cancel_tokens.insert(id.clone(), token.clone());

                let uri = &params.textDocument.uri;
                let file = try_or_notify!(workspace.read().file(uri).ok_or_else(|| {
                    WorkspaceError("file does not exist for hover help")
                }));

                hover(server.clone(), workspace.clone(), id, file, token, &params.position);
            }

            Received::Request(id, Request::SignatureHelp(params)) => {
                let token = CancelToken::new();
                cancel_tokens.insert(id.clone(), token.clone());

                let uri = &params.textDocument.uri;
                let file = try_or_notify!(workspace.read().file(uri).ok_or_else(|| {
                    WorkspaceError("file does not exist for signature help")
                }));

                signature(server.clone(), workspace.clone(), id, file, token, &params.position);
            }

            Received::Request(id, Request::GotoDefinition(params)) => {
                let token = CancelToken::new();
                cancel_tokens.insert(id.clone(), token.clone());

                let uri = &params.textDocument.uri;
                let file = try_or_notify!(workspace.read().file(uri).ok_or_else(|| {
                    WorkspaceError("file does not exist for go to definition")
                }));

                definition(server.clone(), workspace.clone(), id, file, token, &params.position);
            }

            Received::Request(id, Request::Rename(params)) => {
                let token = CancelToken::new();
                cancel_tokens.insert(id.clone(), token.clone());

                let uri = &params.textDocument.uri;
                let file = try_or_notify!(workspace.read().file(uri).ok_or_else(|| {
                    WorkspaceError("file does not exist for renaming")
                }));

                rename(server.clone(), workspace.clone(), id, file, token,
                       &params.position, params.newName);
            }

            _ => {}
        }
    }
}

fn complete(server: Server, workspace: Arc<RwLock<Workspace>>, id: protocol::Id,
            file: WorkspaceFile, cancel_token: CancelToken, position: &protocol::Position) {
    use futures::future;
    use ops::completion;

    let tokens_fut = file.ensure_tokens().map_err(|e| e.as_ref().map(|_| ()));
    let pos_fut = file.translate_position(position);

    let spare_workspace = workspace.clone();
    let fut = tokens_fut.join(pos_fut).and_then(move |(tokens, pos)| {
        if let Err(e) = cancel_token.keep_going() {
            return future::err(e).boxed();
        }

        let workspace = spare_workspace;

        let class = completion::classify(&tokens.0, pos);
        debug!("completion: {:?} {:#?}", class, pos);

        fn send_items(server: Server, id: protocol::Id, items: Vec<protocol::CompletionItem>) {
            debug!("completion items: {:?}",
                   items.iter().map(|i| i.label.clone()).collect::<Vec<_>>());
            let _ = server.send_ok(id, items);
        }

        // we will try twice, first with previous parsing or checking outputs,
        // second with actual parsing or checking outputs (when the first failed).
        match class {
            Some(completion::Class::Name(idx, category)) => {
                let complete = move |chunk: &kailua_syntax::Chunk| {
                    let ws = workspace.read();

                    // the list of all chunks is used to get the global names
                    // TODO make last_global_names and optimize for that
                    let all_chunks: Vec<_> =
                        ws.files().values().flat_map(|f| f.last_chunk()).collect();

                    let items = completion::complete_name(&tokens.0, idx, category, pos,
                                                          chunk, &all_chunks, &ws.source());
                    send_items(server, id, items);
                };

                if let Some(chunk) = file.last_chunk() {
                    complete(&chunk);
                    future::ok(()).boxed()
                } else {
                    file.ensure_chunk().map_err(|e| e.as_ref().map(|_| ())).and_then(move |chunk| {
                        cancel_token.keep_going()?;
                        complete(&chunk.0);
                        Ok(())
                    }).boxed()
                }
            },

            Some(completion::Class::Field(idx)) => {
                let output = workspace.read().last_check_output();
                let items = output.and_then(|output| {
                    completion::complete_field(&tokens.0, idx, &output)
                });

                if let Some(items) = items {
                    send_items(server, id, items);
                    future::ok(()).boxed()
                } else if let Ok(output_fut) = workspace.read().ensure_check_output() {
                    output_fut.map_err(|e| e.as_ref().map(|_| ())).and_then(move |output| {
                        cancel_token.keep_going()?;

                        let items = completion::complete_field(&tokens.0, idx, &output.0);
                        send_items(server, id, items.unwrap_or(Vec::new()));
                        Ok(())
                    }).boxed()
                } else {
                    // checking couldn't be started, `checking_loop` will notify the incident
                    // so we don't have to do anything
                    future::ok(()).boxed()
                }
            },

            None => {
                send_items(server, id, Vec::new());
                future::ok(()).boxed()
            },
        }
    });

    workspace.read().pool().spawn(fut).forget();
}

fn hover(server: Server, workspace: Arc<RwLock<Workspace>>, id: protocol::Id,
         file: WorkspaceFile, cancel_token: CancelToken, position: &protocol::Position) {
    use futures::future;
    use protocol::*;

    let spare_workspace = workspace.clone();
    let fut = file.translate_position(position).and_then(move |pos| {
        if let Err(e) = cancel_token.keep_going() {
            return future::err(e).boxed();
        }

        let workspace = spare_workspace.clone();
        let output = workspace.read().last_check_output();
        let info = output.and_then(|output| {
            let ws = workspace.read();
            let info = ops::hover::help(&output, pos, &ws.source(), |s| ws.localize(s));
            info
        });
        if let Some(info) = info {
            let _ = server.send_ok(id, info);
            future::ok(()).boxed()
        } else if let Ok(output_fut) = workspace.read().ensure_check_output() {
            output_fut.map_err(|e| e.as_ref().map(|_| ())).and_then(move |output| {
                cancel_token.keep_going()?;

                let ws = spare_workspace.read();
                let info = ops::hover::help(&output.0, pos, &ws.source(), |s| ws.localize(s));
                let info = info.unwrap_or_else(|| Hover { contents: Vec::new(), range: None });
                let _ = server.send_ok(id, info);
                Ok(())
            }).boxed()
        } else {
            future::ok(()).boxed()
        }
    });

    workspace.read().pool().spawn(fut).forget();
}

fn signature(server: Server, workspace: Arc<RwLock<Workspace>>, id: protocol::Id,
             file: WorkspaceFile, cancel_token: CancelToken, position: &protocol::Position) {
    use futures::future;
    use protocol::*;

    let empty_signature = || {
        SignatureHelp { signatures: Vec::new(), activeSignature: None, activeParameter: None }
    };

    let tokens_fut = file.ensure_tokens().map_err(|e| e.as_ref().map(|_| ()));
    let pos_fut = file.translate_position(position);

    let spare_workspace = workspace.clone();
    let fut = tokens_fut.join(pos_fut).and_then(move |(tokens, pos)| {
        if let Err(e) = cancel_token.keep_going() {
            return future::err(e).boxed();
        }

        let loc = ops::signature::locate(&tokens.0, pos);
        debug!("signature: {:?} {:#?}", loc, pos);

        let loc = if let Some(loc) = loc {
            loc
        } else {
            let _ = server.send_ok(id, empty_signature());
            return future::ok(()).boxed();
        };

        let workspace = spare_workspace.clone();
        let output = workspace.read().last_check_output();
        let info = output.and_then(|output| {
            let ws = workspace.read();
            let info = ops::signature::help(&tokens.0, &loc, &output, |s| ws.localize(s));
            info
        });
        if let Some(info) = info {
            let _ = server.send_ok(id, info);
            future::ok(()).boxed()
        } else if let Ok(output_fut) = workspace.read().ensure_check_output() {
            output_fut.map_err(|e| e.as_ref().map(|_| ())).and_then(move |output| {
                cancel_token.keep_going()?;

                let ws = spare_workspace.read();
                let info = ops::signature::help(&tokens.0, &loc, &output.0, |s| ws.localize(s));
                let info = info.unwrap_or_else(|| empty_signature());
                let _ = server.send_ok(id, info);
                Ok(())
            }).boxed()
        } else {
            future::ok(()).boxed()
        }
    });

    workspace.read().pool().spawn(fut).forget();
}

fn definition(server: Server, workspace: Arc<RwLock<Workspace>>, id: protocol::Id,
              file: WorkspaceFile, cancel_token: CancelToken, position: &protocol::Position) {
    use std::path::Path;
    use futures::{future, stream, Stream};
    use url::Url;
    use kailua_env::{Span, Source};
    use kailua_syntax::NameRef;
    use ops::definition;
    use protocol::*;

    let tokens_fut = file.ensure_tokens().map_err(|e| e.as_ref().map(|_| ()));
    let chunk_fut = file.ensure_chunk().map_err(|e| e.as_ref().map(|_| ()));
    let pos_fut = file.translate_position(position);

    let spare_workspace = workspace.clone();
    let fut = tokens_fut.join(chunk_fut).join(pos_fut).and_then(move |((tokens, chunk), pos)| {
        if let Err(e) = cancel_token.keep_going() {
            return future::err(e).boxed();
        }

        let class = definition::classify(&tokens.0, &chunk.0, pos);
        debug!("definition: {:?} {:#?}", class, pos);

        fn send_spans(server: Server, id: protocol::Id, spans: &[Span], source: &Source) {
            debug!("definition spans: {:#?}", spans);
            let locs: Vec<_> = spans.iter().filter_map(|&span| {
                diags::translate_span(span, source).and_then(|(path, range)| {
                    Url::from_file_path(Path::new(&path)).ok().map(|url| {
                        Location { uri: url.to_string(), range: range }
                    })
                })
            }).collect();
            let _ = server.send_ok(id, locs);
        }

        let workspace = spare_workspace.clone();
        let ws = workspace.read();
        match class {
            Some(definition::Class::Var(_, NameRef::Local(scoped_id))) => {
                if let Some(span) = definition::local_var_definition(&chunk.0, &scoped_id) {
                    send_spans(server, id, &[span], &ws.source());
                } else {
                    send_spans(server, id, &[], &ws.source());
                }
                future::ok(()).boxed()
            },

            Some(definition::Class::Var(_, NameRef::Global(name))) => {
                let spare_cancel_token = cancel_token.clone();

                // should wait for all chunks being parsed
                let chunk_futs: Vec<_> =
                    ws.files().values().map(|f| Ok(f.ensure_chunk())).collect();
                let chunk_stream = stream::iter(chunk_futs.into_iter()).and_then(|fut| fut);
                let spans_fut = chunk_stream.filter_map(move |chunk| {
                    if spare_cancel_token.is_canceled() {
                        None
                    } else {
                        definition::global_var_definition(&chunk.0, &name)
                    }
                }).collect();

                spans_fut.map_err(|e| e.as_ref().map(|_| ())).and_then(move |spans| {
                    cancel_token.keep_going()?;
                    let ws = spare_workspace.read();
                    send_spans(server, id, &spans, &ws.source());
                    Ok(())
                }).boxed()
            },

            // XXX PossiblyRequire depends on package.path/cpath, which can change in runtime
            Some(definition::Class::PossiblyRequire(_, _, _)) | None => {
                send_spans(server, id, &[], &ws.source());
                future::ok(()).boxed()
            },
        }
    });

    workspace.read().pool().spawn(fut).forget();
}

fn rename(server: Server, workspace: Arc<RwLock<Workspace>>, id: protocol::Id,
          file: WorkspaceFile, cancel_token: CancelToken,
          position: &protocol::Position, new_name: String) {
    use std::collections::HashMap;
    use std::path::Path;
    use futures::{future, stream, Stream};
    use url::Url;
    use kailua_env::{Span, Source};
    use kailua_syntax::NameRef;
    use ops::definition;
    use protocol::*;

    let tokens_fut = file.ensure_tokens().map_err(|e| e.as_ref().map(|_| ()));
    let chunk_fut = file.ensure_chunk().map_err(|e| e.as_ref().map(|_| ()));
    let pos_fut = file.translate_position(position);

    let spare_workspace = workspace.clone();
    let fut = tokens_fut.join(chunk_fut).join(pos_fut).and_then(move |((tokens, chunk), pos)| {
        if let Err(e) = cancel_token.keep_going() {
            return future::err(e).boxed();
        }

        let class = definition::classify(&tokens.0, &chunk.0, pos);
        debug!("rename: {:?} {:#?}", class, pos);

        fn send_spans(server: Server, id: protocol::Id, spans: &[Span],
                      new_name: String, source: &Source) {
            debug!("rename spans: {:#?}", spans);

            let mut spansmap = HashMap::new();
            for &span in spans {
                let mut perunit = spansmap.entry(span.unit()).or_insert_with(|| {
                    (source.get_file(span.unit()), Vec::new())
                });
                perunit.1.push(span);
            }

            let changes = spansmap.into_iter().flat_map(|(_unit, (file, spans))| {
                file.and_then(|file| {
                    Url::from_file_path(Path::new(file.path())).ok().map(|url| {
                        let edits: Vec<_> = spans.into_iter().filter_map(|span| {
                            diags::translate_span_without_path(span, file).map(|range| {
                                TextEdit { range: range, newText: new_name.clone() }
                            })
                        }).collect();
                        (url.to_string(), edits)
                    })
                })
            }).collect();

            let wsedit = WorkspaceEdit { changes: changes, documentChanges: Vec::new() };
            let _ = server.send_ok(id, wsedit);
        }

        let workspace = spare_workspace.clone();
        let ws = workspace.read();
        match class {
            Some(definition::Class::Var(_, NameRef::Local(scoped_id))) => {
                let spans = definition::local_var_uses(&tokens.0, &chunk.0, &scoped_id);
                send_spans(server, id, &spans, new_name, &ws.source());
                future::ok(()).boxed()
            },

            Some(definition::Class::Var(_, NameRef::Global(name))) => {
                let spare_cancel_token = cancel_token.clone();

                // should wait for all chunks being parsed
                let tokens_and_chunk_futs: Vec<_> = ws.files().values().map(|f| {
                    Ok(f.ensure_tokens().join(f.ensure_chunk()))
                }).collect();
                let tokens_and_chunk_stream =
                    stream::iter(tokens_and_chunk_futs.into_iter()).and_then(|fut| fut);
                let spans_fut = tokens_and_chunk_stream.filter_map(move |tokens_and_chunk| {
                    if spare_cancel_token.is_canceled() {
                        None
                    } else {
                        let (tokens, chunk) = tokens_and_chunk;
                        Some(definition::global_var_uses(&tokens.0, &chunk.0, &name))
                    }
                }).fold(Vec::new(), |mut a, b| { a.extend_from_slice(&b); Ok(a) });

                spans_fut.map_err(|e| e.as_ref().map(|_| ())).and_then(move |spans| {
                    cancel_token.keep_going()?;
                    let ws = spare_workspace.read();
                    send_spans(server, id, &spans, new_name, &ws.source());
                    Ok(())
                }).boxed()
            },

            _ => {
                let _ = server.send_notify(
                    Method::ShowMessage,
                    ShowMessageParams {
                        type_: MessageType::Warning,
                        message: ws.localize(&message::CannotRename {}).to_string(),
                    },
                );
                send_spans(server, id, &[], new_name, &ws.source());
                future::ok(()).boxed()
            },
        }
    });

    workspace.read().pool().spawn(fut).forget();
}

pub fn main() {
    env_logger::init().unwrap();
    let server = connect_to_client();
    info!("established connection");
    let workspace = Arc::new(RwLock::new(initialize_workspace(&server)));
    info!("initialized workspace, starting a main loop");
    main_loop(server, workspace);
}

