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
#[macro_use] extern crate parse_generics_shim;
extern crate kailua_env;
#[macro_use] extern crate kailua_diag;
extern crate kailua_syntax;
extern crate kailua_check;

pub mod protocol;
pub mod server;
pub mod diags;
pub mod workspace;
pub mod futureutils;
pub mod message;
pub mod completion;

use std::io;
use std::sync::Arc;
use parking_lot::RwLock;
use futures::{Future, BoxFuture};
use tokio_timer::Timer;

use server::Server;
use futureutils::CancelError;
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
#[derive(Deserialize)]
struct InitOptions {
    default_locale: String,
}

impl Default for InitOptions {
    fn default() -> InitOptions {
        InitOptions { default_locale: "en".to_string() }
    }
}

fn parse_init_options(opts: Option<serde_json::Value>) -> InitOptions {
    use std::ascii::AsciiExt;

    let opts = opts.and_then(|opts| serde_json::from_value::<InitOptions>(opts).ok());
    let mut opts = opts.unwrap_or_else(InitOptions::default);
    opts.default_locale = opts.default_locale.to_ascii_lowercase();
    opts
}

fn initialize_workspace(server: &Server) -> Workspace {
    use std::path::PathBuf;
    use futures_cpupool::CpuPool;
    use workspace::Workspace;
    use protocol::*;
    use message as m;

    loop {
        let res = server.recv().unwrap();
        let req = if let Some(req) = res { req } else { continue };
        errln!("read: {:#?}", req);

        match req {
            Request::Initialize(id, params) => {
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

                    let mut workspace = Workspace::new(PathBuf::from(dir), pool,
                                                       initopts.default_locale);

                    let _ = server.send_ok(id, InitializeResult {
                        capabilities: ServerCapabilities {
                            textDocumentSync: TextDocumentSyncKind::Full,
                            completionProvider: Some(CompletionOptions {
                                resolveProvider: false,
                                triggerCharacters:
                                    ".:ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                                       abcdefghijklmnopqrstuvwxyz".chars()
                                                                  .map(|c| c.to_string())
                                                                  .collect(),
                            }),
                            ..Default::default()
                        },
                    });

                    // try to read the config
                    if let Err(e) = workspace.read_config() {
                        let _ = server.send_notify("window/showMessage", ShowMessageParams {
                            type_: MessageType::Warning,
                            message: workspace.localize(&m::CannotReadConfig { error: &e }),
                        });
                    }

                    return workspace;
                } else {
                    let _ = server.send_err(Some(id.clone()),
                                            error_codes::INTERNAL_ERROR,
                                            "no folder open, retry with an open folder",
                                            InitializeError { retry: true });
                }
            }

            req => {
                // reply an error to the request (notifications are ignored)
                if let Some(id) = req.id() {
                    let _ = server.send_err(Some(id.clone()),
                                            error_codes::SERVER_NOT_INITIALIZED,
                                            "server hasn't been initialized yet",
                                            InitializeError { retry: false });
                }
            }
        }
    }
}

fn send_diagnostics(server: Arc<Server>, root: ReportTree) -> io::Result<()> {
    use std::path::Path;
    use std::collections::HashMap;
    use url::Url;

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
            "textDocument/publishDiagnostics",
            protocol::PublishDiagnosticsParams { uri: uri.to_string(), diagnostics: diags }
        )?;
    }

    Ok(())
}

fn send_diagnostics_when_available<T, F>(server: Arc<Server>,
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

fn on_file_changed(file: &WorkspaceFile, server: Arc<Server>, pool: &futures_cpupool::CpuPool) {
    send_diagnostics_when_available(server.clone(), pool, file.ensure_tokens());
    send_diagnostics_when_available(server, pool, file.ensure_chunk());
}

// in the reality, the "loop" is done via a chain of futures and the function immediately returns
fn checking_loop(server: Arc<Server>, workspace: Arc<RwLock<Workspace>>,
                 timer: Timer) -> BoxFuture<(), ()> {
    use std::time::Duration;
    use futures;
    use futureutils::FutureExt;
    use protocol::*;
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
                let _ = server.send_notify("window/showMessage", ShowMessageParams {
                    type_: MessageType::Warning,
                    message: workspace.read().localize(&m::NoStartPath {}),
                });
            }
            futures::finished(()).boxed()
        }
    }).erase_err().boxed()
}

fn main_loop(server: Arc<Server>, workspace: Arc<RwLock<Workspace>>) {
    use std::collections::HashMap;
    use futureutils::CancelToken;
    use workspace::WorkspaceError;
    use completion::{self, CompletionClass};
    use protocol::*;

    let mut cancel_tokens: HashMap<Id, CancelToken> = HashMap::new();
    let timer = Timer::default();

    // launch the checking future, which will be executed throughout the entire loop
    let checking_fut = checking_loop(server.clone(), workspace.clone(), timer.clone());
    workspace.read().pool().spawn(checking_fut).forget();

    'restart: loop {
        let res = server.recv().unwrap();
        let req = if let Some(req) = res { req } else { continue };
        errln!("read: {:#?}", req);

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
            Request::Initialize(id, ..) => {
                let _ = server.send_err(Some(id), error_codes::INTERNAL_ERROR,
                                        "already initialized", InitializeError { retry: false });
            }

            Request::CancelRequest(params) => {
                if let Some(token) = cancel_tokens.remove(&params.id) {
                    token.cancel();
                }
            }

            Request::DidOpenTextDocument(params) => {
                let uri = params.textDocument.uri.clone();

                let ws = workspace.write();
                try_or_notify!(ws.open_file(params.textDocument));
                errln!("workspace: {:#?}", *ws);

                let pool = ws.pool().clone();
                let file = ws.file(&uri).unwrap();
                on_file_changed(&file, server.clone(), &pool);
            }

            Request::DidChangeTextDocument(params) => {
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
                errln!("workspace: {:#?}", *ws);
            }

            Request::DidCloseTextDocument(params) => {
                let ws = workspace.write();
                try_or_notify!(ws.close_file(&params.textDocument.uri));
                errln!("workspace: {:#?}", *ws);
            }

            Request::Completion(id, params) => {
                let token = CancelToken::new();
                cancel_tokens.insert(id.clone(), token.clone());

                let spare_workspace = workspace.clone();

                let ws = workspace.read();
                let file = try_or_notify!(ws.file(&params.textDocument.uri).ok_or_else(|| {
                    WorkspaceError("file does not exist for completion")
                }));

                let tokens_fut = file.ensure_tokens().map_err(|e| e.as_ref().map(|_| ()));
                let pos_fut = file.translate_position(&params.position);

                let server = server.clone();
                let fut = tokens_fut.join(pos_fut).and_then(move |(tokens, pos)| {
                    let workspace = spare_workspace;
                    let tokens = &tokens.0;

                    let class = completion::classify(tokens, pos);
                    errln!("completion: {:?} {:#?}", class, pos);
                    let items = match class {
                        Some(CompletionClass::Name(idx, category)) => {
                            file.last_chunk().map(|chunk| {
                                completion::complete_name(tokens, idx, category, pos, chunk)
                            })
                        },
                        Some(CompletionClass::Field(idx)) => {
                            let output = workspace.read().last_check_output();
                            output.map(|output| completion::complete_field(tokens, idx, output))
                        },
                        None => None,
                    };

                    let _ = server.send_ok(id, items.unwrap_or(Vec::new()));
                    Ok(())
                });

                ws.pool().spawn(fut).forget();
            }

            _ => {}
        }
    }
}

pub fn main() {
    let server = connect_to_client();
    let workspace = Arc::new(RwLock::new(initialize_workspace(&server)));
    let server = Arc::new(server);
    main_loop(server, workspace);
}
