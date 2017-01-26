extern crate serde;
#[macro_use] extern crate serde_derive;
extern crate serde_json;
extern crate url;
extern crate futures;
extern crate futures_cpupool;
extern crate owning_ref;
extern crate num_cpus;
extern crate parking_lot;
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

use std::io;
use std::sync::Arc;
use parking_lot::RwLock;
use futures::Future;

use server::Server;
use futureutils::CancelError;
use diags::ReportTree;
use workspace::{Workspace, WorkspaceFile};

fn connect_to_client() -> Server {
    use std::env;
    use std::net::{SocketAddr, TcpStream};
    use std::io::{self, Write};
    use std::process;
    use server::Server;

    let mut stderr = io::stderr();

    let mut server = None;
    if let Some(firstopt) = env::args().nth(1) {
        if firstopt == "--packets-via-stdio" {
            server = Some(Server::from_stdio());
        } else if firstopt.starts_with("--packets-via-tcp=") {
            if let Ok(ip) = firstopt["--packets-via-tcp=".len()..].parse::<SocketAddr>() {
                match TcpStream::connect(ip).and_then(Server::from_tcp_stream) {
                    Ok(s) => server = Some(s),
                    Err(e) => {
                        let _ = writeln!(stderr, "*** Couldn't connect to the client: {}", e);
                        process::exit(1);
                    }
                }
            }
        }
    }

    if server.is_none() {
        let _ = writeln!(stderr, "Kailua language server is intended to be used with VS Code. \
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
    use std::cmp;
    use std::path::PathBuf;
    use std::io::{self, Write};
    use futures_cpupool::CpuPool;
    use workspace::Workspace;
    use protocol::*;
    use message as m;

    let mut stderr = io::stderr();

    loop {
        let res = server.recv().unwrap();
        let req = if let Some(req) = res { req } else { continue };
        writeln!(stderr, "read: {:#?}", req).unwrap();

        match req {
            Request::Initialize(id, params) => {
                if let Some(dir) = params.rootPath {
                    let initopts = parse_init_options(params.initializationOptions);
                    let ncpus = cmp::max(num_cpus::get(), 2); // 2+ workers required by checker
                    let pool = Arc::new(CpuPool::new(ncpus));
                    let mut workspace = Workspace::new(PathBuf::from(dir), pool,
                                                       initopts.default_locale);

                    let _ = server.send_ok(id, InitializeResult {
                        capabilities: ServerCapabilities {
                            textDocumentSync: TextDocumentSyncKind::Full,
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
fn force_checking_loop(server: Arc<Server>, workspace: Arc<RwLock<Workspace>>) {
    use protocol::*;
    use message as m;

    let pool;
    let cancel_future;
    let output_future;
    {
        let ws = workspace.read();
        pool = ws.pool().clone();
        // cancel_future should be earlier than output_future;
        // otherwise it is possible that output_future is immediately canceled before cancel_future
        cancel_future = ws.cancel_future();
        output_future = ws.ensure_check_output();
    };

    if let Ok(fut) = output_future {
        let server = server.clone();
        let workspace = workspace.clone();
        let fut = fut.then(move |res| {
            // send diagnostics for this check
            let diags = match res {
                Ok(ref value_and_diags) => Some(&value_and_diags.1),
                Err(ref e) => match **e {
                    CancelError::Canceled => None,
                    CancelError::Error(ref diags) => Some(diags),
                },
            };
            if let Some(diags) = diags {
                let _ = send_diagnostics(server.clone(), diags.clone());
            }

            // when the cancel was properly requested (even after the completion), restart the loop;
            // if there were any error (most possibly the panic) stop it.
            cancel_future.map(move |_| force_checking_loop(server, workspace))
        });

        // this should be forgotten as we won't make use of its result
        pool.spawn(fut).forget();
    } else if workspace.read().has_read_config() {
        // avoid a duplicate message if kailua.json is missing
        let _ = server.send_notify("window/showMessage", ShowMessageParams {
            type_: MessageType::Warning,
            message: workspace.read().localize(&m::NoStartPath {}),
        });
    }
}

fn main_loop(server: Arc<Server>, workspace: Arc<RwLock<Workspace>>) {
    use std::io::{self, Write};
    use workspace::WorkspaceError;
    use protocol::*;

    let mut stderr = io::stderr();

    force_checking_loop(server.clone(), workspace.clone());

    'restart: loop {
        let res = server.recv().unwrap();
        let req = if let Some(req) = res { req } else { continue };
        writeln!(stderr, "read: {:#?}", req).unwrap();

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

            Request::DidOpenTextDocument(params) => {
                let uri = params.textDocument.uri.clone();

                let ws = workspace.write();
                try_or_notify!(ws.open_file(params.textDocument));
                writeln!(stderr, "workspace: {:#?}", *ws).unwrap();

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
                writeln!(stderr, "workspace: {:#?}", *ws).unwrap();
            }

            Request::DidCloseTextDocument(params) => {
                let ws = workspace.write();
                try_or_notify!(ws.close_file(&params.textDocument.uri));
                writeln!(stderr, "workspace: {:#?}", *ws).unwrap();
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
