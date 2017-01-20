use std::fmt;
use std::io;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use futures::{future, Future, BoxFuture};
use futures_cpupool::CpuPool;
use serde_json;
use url::Url;
use parking_lot::{RwLock, RwLockWriteGuard};

use kailua_env::{Unit, Span, Source, SourceFile};
use kailua_diag::{self, Report};
use kailua_syntax::{Lexer, Nest, NestedToken, Parser, Chunk};
use kailua_check::{self, FsSource, FsOptions, Context, Output};

use diags::{self, ReportTree};
use futureutils::{CancelError, CancelToken};
use protocol;

#[derive(Clone, Debug)]
pub struct WorkspaceError(pub &'static str);

pub type WorkspaceResult<T> = Result<T, WorkspaceError>;

fn uri_to_path(uri: &str) -> WorkspaceResult<PathBuf> {
    let url = Url::parse(uri).map_err(|_| WorkspaceError("invalid URI"))?;
    if url.scheme() != "file" {
        return Err(WorkspaceError("non-file URI"));
    }
    if let Ok(path) = url.to_file_path() {
        return Ok(path);
    }

    #[cfg(windows)]
    {
        use std::ffi::OsString;
        use std::path::Component;
        use url::Host;

        // Url::to_file_path only handles no host or localhost, which is different from vscode-uri
        // we first try localhost then retry by temporarily setting the authority part on windows
        let host = match url.host() {
            Some(Host::Domain(name)) => name.to_string(),
            Some(Host::Ipv4(addr)) => addr.to_string(),
            Some(Host::Ipv6(addr)) => {
                // an "official" hack for UNC
                // https://msdn.microsoft.com/en-us/library/aa385353.aspx
                let s = &addr.segments();
                format!("{:x}-{:x}-{:x}-{:x}-{:x}-{:x}-{:x}-{:x}.ipv6-literal.net",
                        s[0], s[1], s[2], s[3], s[4], s[5], s[6], s[7])
            },
            None => return Err(WorkspaceError("non-file URI")),
        };

        // convert file://host/path to file:///z:/path (z: is required for rust-url)
        let url = Url::parse(&format!("file:///z:{}", url.path())).unwrap();
        if let Ok(path) = url.to_file_path() {
            // now path starts with z:\foo\bar, so replace z: by \\host to complete it
            let mut components = path.components();
            let _prefix = components.next();
            assert!(match _prefix { Some(Component::Prefix(..)) => true, _ => false });

            let mut pathstr = OsString::from("\\\\");
            pathstr.push(&host);
            pathstr.push(components.as_path());
            return Ok(PathBuf::from(pathstr));
        }
    }

    Err(WorkspaceError("non-file URI"))
}

fn collect_tokens(source: &Source, span: Span, report: &Report) -> Vec<NestedToken> {
    let mut iter = source.iter_from_span(span).unwrap();
    let tokens = {
        let mut lexer = Lexer::new(&mut iter, report);
        let nest = Nest::new(&mut lexer);
        nest.collect::<Vec<_>>()
    };
    assert!(!tokens.is_empty()); // should include EOF
    tokens
}

fn parse_to_chunk(tokens: Vec<NestedToken>, report: &Report) -> kailua_diag::Result<Chunk> {
    let mut tokens = tokens.into_iter();
    let chunk = Parser::new(&mut tokens, report).into_chunk();
    chunk
}

// the expected contents of kailua.json at the project root
#[derive(Deserialize, Clone, Debug)]
struct WorkspaceConfig {
    start_path: PathBuf,
    message_lang: Option<String>, // defaults to VS Code UI locale
}

impl WorkspaceConfig {
    fn read(base_dir: &Path) -> io::Result<WorkspaceConfig> {
        let f = File::open(base_dir.join("kailua.json"))?;
        let mut config: WorkspaceConfig = serde_json::de::from_reader(f).map_err(|e| {
            io::Error::new(io::ErrorKind::InvalidData, e)
        })?;
        config.start_path = base_dir.join(config.start_path);
        Ok(config)
    }
}

#[derive(Clone, Debug)]
pub struct OpenDocument {
    uri: String,
    lang_id: String,
    last_version: u64,
    last_text: String,
}

impl OpenDocument {
    fn new(item: protocol::TextDocumentItem) -> OpenDocument {
        OpenDocument {
            uri: item.uri,
            lang_id: item.languageId,
            last_version: item.version,
            last_text: item.text,
        }
    }
}

// clonable, externally visible future type at work
pub type IoFuture<T> =
    future::Shared<BoxFuture<T, CancelError<io::Error>>>;
pub type ReportFuture<T> =
    future::Shared<BoxFuture<(T, ReportTree), CancelError<ReportTree>>>;

struct WorkspaceFileInner {
    workspace: Arc<RwLock<WorkspaceShared>>,
    pool: Arc<CpuPool>,
    cancel_token: CancelToken,

    path: PathBuf,
    unit: Unit,

    // if Some, the file is managed by the client and the text is synchronized
    document: Option<OpenDocument>,

    // each parts are calculated on demand; in either case diagnostics are produced
    span: Option<IoFuture<Span>>,
    tokens: Option<ReportFuture<Vec<NestedToken>>>,
    chunk: Option<ReportFuture<Chunk>>,
}

type Inner = Arc<RwLock<WorkspaceFileInner>>;
type InnerWrite<'a> = RwLockWriteGuard<'a, WorkspaceFileInner>;

#[derive(Clone)]
pub struct WorkspaceFile {
    inner: Inner,
}

impl fmt::Debug for WorkspaceFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let inner = self.inner.read();
        f.debug_struct("WorkspaceFile")
         .field("workspace", &Ellipsis) // avoid excess output
         .field("pool", &Ellipsis)
         .field("cancel_token", &inner.cancel_token)
         .field("path", &inner.path)
         .field("unit", &inner.unit)
         .field("document", &inner.document)
         .field("span", &inner.span.as_ref().map(|_| Ellipsis))
         .field("tokens", &inner.tokens.as_ref().map(|_| Ellipsis))
         .field("chunk", &inner.chunk.as_ref().map(|_| Ellipsis))
         .finish()
    }
}

impl WorkspaceFile {
    fn new(shared: &Arc<RwLock<WorkspaceShared>>, pool: &Arc<CpuPool>,
           path: PathBuf) -> WorkspaceFile {
        WorkspaceFile {
            inner: Arc::new(RwLock::new(WorkspaceFileInner {
                workspace: shared.clone(),
                pool: pool.clone(),
                cancel_token: CancelToken::new(),
                path: path,
                unit: Unit::dummy(),
                document: None,
                span: None,
                tokens: None,
                chunk: None,
            })),
        }
    }

    fn cancel(&self) {
        let mut inner = self.inner.write();

        inner.cancel_token.cancel();
        inner.cancel_token = CancelToken::new();

        inner.span = None;
        inner.tokens = None;
        inner.chunk = None;

        // also signal the workspace to cancel jobs
        inner.workspace.write().cancel();
    }

    pub fn path(&self) -> PathBuf {
        self.inner.read().path.clone()
    }

    fn update_document<F, E>(&self, f: F) -> Result<(), E>
        where F: FnOnce(Option<OpenDocument>) -> Result<Option<OpenDocument>, E>
    {
        self.cancel();

        let mut inner = self.inner.write();
        inner.document = f(inner.document.take())?;

        Ok(())
    }

    fn ensure_span_with_inner(spare_inner: Inner, inner: &mut InnerWrite) -> IoFuture<Span> {
        if inner.span.is_none() {
            let fut = future::lazy(move || -> Result<Span, CancelError<io::Error>> {
                let inner = spare_inner.read();
                inner.cancel_token.keep_going()?;

                let file = if let Some(ref doc) = inner.document {
                    SourceFile::from_u8(inner.path.display().to_string(),
                                        doc.last_text.as_bytes().to_owned())
                } else {
                    SourceFile::from_file(&inner.path)?
                };
                let mut ws = inner.workspace.write();
                let span = ws.source.add(file);
                Ok(span)
            });

            inner.span = Some(inner.pool.spawn(fut).boxed().shared());
        }

        inner.span.as_ref().unwrap().clone()
    }

    pub fn ensure_span(&self) -> IoFuture<Span> {
        let cloned = self.inner.clone();
        Self::ensure_span_with_inner(cloned, &mut self.inner.write())
    }

    fn ensure_tokens_with_inner(spare_inner: Inner,
                                inner: &mut InnerWrite) -> ReportFuture<Vec<NestedToken>> {
        if inner.tokens.is_none() {
            let span_fut = Self::ensure_span_with_inner(spare_inner.clone(), inner);

            // important: the task has to be spawned outside of the future.
            // this is because, otherwise for the thread pool of n workers
            // the future chain of n+1 or more tasks will block as the i-th task
            // will spawn the (i+1)-th task without removing itself from the pool queue!
            // chaining the already-spawned future will ensure that
            // the task body will be only spawned after the last future has been finished.
            let fut = span_fut.map_err(|_| {
                CancelError::Error(ReportTree::new("", None))
            }).and_then(move |span| {
                let span = *span;

                let inner = spare_inner.read();
                inner.cancel_token.keep_going()?;

                let ws = inner.workspace.read();
                let source = &ws.source;

                let path = source.file(span.unit()).map(|f| f.path());
                let diags = ReportTree::new(&ws.message_lang, path);

                let report = diags.report(|r| diags::translate_diag(r, source));
                let tokens = collect_tokens(source, span, &report);
                Ok((tokens, diags))
            });

            inner.tokens = Some(inner.pool.spawn(fut).boxed().shared());
        }

        inner.tokens.as_ref().unwrap().clone()
    }

    pub fn ensure_tokens(&self) -> ReportFuture<Vec<NestedToken>> {
        let cloned = self.inner.clone();
        Self::ensure_tokens_with_inner(cloned, &mut self.inner.write())
    }

    fn ensure_chunk_with_inner(spare_inner: Inner,
                               inner: &mut InnerWrite) -> ReportFuture<Chunk> {
        if inner.chunk.is_none() {
            let tokens_fut = Self::ensure_tokens_with_inner(spare_inner.clone(), inner);

            let fut = tokens_fut.map_err(|e| (*e).clone()).and_then(move |tokens_ret| {
                let tokens = tokens_ret.0.clone();
                let parent_diags = tokens_ret.1.clone();

                let inner = spare_inner.read();
                inner.cancel_token.keep_going()?;

                let ws = inner.workspace.read();
                let diags = ReportTree::new(&ws.message_lang, None);
                diags.add_parent(parent_diags);

                let report = diags.report(|r| diags::translate_diag(r, &ws.source));
                match parse_to_chunk(tokens, &report) {
                    Ok(chunk) => Ok((chunk, diags)),
                    Err(_) => Err(From::from(diags)),
                }
            });

            inner.chunk = Some(inner.pool.spawn(fut).boxed().shared());
        }

        inner.chunk.as_ref().unwrap().clone()
    }

    pub fn ensure_chunk(&self) -> ReportFuture<Chunk> {
        let cloned = self.inner.clone();
        Self::ensure_chunk_with_inner(cloned, &mut self.inner.write())
    }

    pub fn apply_change(&mut self, version: u64,
                        event: protocol::TextDocumentContentChangeEvent) -> WorkspaceResult<()> {
        // TODO, there are several ambiguities with offsets?
        if event.range.is_some() || event.rangeLength.is_some() {
            return Err(WorkspaceError("incremental edits not yet supported"));
        }

        self.update_document(move |doc| {
            if let Some(mut doc) = doc {
                if doc.last_version >= version {
                    return Err(WorkspaceError("non-increasing version"));
                }

                doc.last_version = version;
                doc.last_text = event.text;
                Ok(Some(doc))
            } else {
                Err(WorkspaceError("change notification with non-existent or non-open file"))
            }
        })
    }
}

// a portion of Workspace that should be shared across WorkspaceFile
struct WorkspaceShared {
    message_lang: String,
    source: Source,

    cancel_token: CancelToken, // used for stopping ongoing checks

    check_output: Option<ReportFuture<Output>>,
}

impl fmt::Debug for WorkspaceShared {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("WorkspaceShared")
         .field("message_lang", &self.message_lang)
         .field("source", &Ellipsis)
         .field("cancel_token", &self.cancel_token)
         .field("check_output", &self.check_output.as_ref().map(|_| Ellipsis))
         .finish()
    }
}

impl WorkspaceShared {
    fn cancel(&mut self) {
        self.cancel_token.cancel();
        self.cancel_token = CancelToken::new();

        self.check_output = None;
    }
}

struct WorkspaceFsSourceInner {
    shared: Arc<RwLock<WorkspaceShared>>,
    cancel_token: CancelToken, // will be used independently of shared
    files: Arc<RwLock<HashMap<PathBuf, WorkspaceFile>>>,

    temp_units: Vec<Unit>, // will be gone after checking
    temp_files: HashMap<PathBuf, Chunk>,

    root_report: ReportTree,
}

#[derive(Clone)]
struct WorkspaceFsSource {
    inner: Rc<RefCell<WorkspaceFsSourceInner>>,
}

impl FsSource for WorkspaceFsSource {
    fn chunk_from_path(&self, path: &Path) -> Result<Option<Chunk>, String> {
        let mut fssource = self.inner.borrow_mut();

        fssource.cancel_token.keep_going::<()>().map_err(|_| "cancel requested".to_string())?;

        // try to use the client-maintained text as a source code
        let files = fssource.files.clone();
        let files = files.read();
        if let Some(file) = files.get(path) {
            let (chunk, diags) = match file.ensure_chunk().wait() {
                Ok(res) => {
                    let (ref chunk, ref diags) = *res;
                    (Some(chunk.clone()), diags.clone())
                },
                Err(res) => match *res {
                    CancelError::Canceled => return Err("cancel requested".to_string()),
                    CancelError::Error(ref diags) => (None, diags.clone())
                },
            };

            // this can be called multiple times, which ReportTree handles correctly
            fssource.root_report.add_parent(diags);
            return Ok(chunk);
        }
        drop(files); // avoid prolonged lock

        // try to use the already-read temporary chunk
        if let Some(chunk) = fssource.temp_files.get(path) {
            return Ok(Some(chunk.clone()));
        }

        // try to read the file (and finally raise an error if it can't be read)

        let shared = fssource.shared.clone();
        let mut shared = shared.write();

        let sourcefile = SourceFile::from_file(path).map_err(|e| e.to_string())?;
        let span = shared.source.add(sourcefile);
        fssource.temp_units.push(span.unit());

        let diags = ReportTree::new(&shared.message_lang, path.to_str());
        fssource.root_report.add_parent(diags.clone());

        let report = diags.report(|r| diags::translate_diag(r, &shared.source));
        let tokens = collect_tokens(&shared.source, span, &report);
        match parse_to_chunk(tokens, &report) {
            Ok(chunk) => {
                fssource.temp_files.insert(path.to_owned(), chunk.clone());
                Ok(Some(chunk))
            },
            Err(_) => Err("failed to parse chunk".to_string()),
        }
    }
}

pub struct Workspace {
    base_dir: PathBuf,
    start_path: Option<PathBuf>,
    config_read: bool,

    pool: Arc<CpuPool>,
    files: Arc<RwLock<HashMap<PathBuf, WorkspaceFile>>>,

    shared: Arc<RwLock<WorkspaceShared>>,
}

impl fmt::Debug for Workspace {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Workspace")
         .field("base_dir", &self.base_dir)
         .field("start_path", &self.start_path)
         .field("config_read", &self.config_read)
         .field("pool", &Ellipsis)
         .field("files", &self.files)
         .field("shared", &self.shared)
         .finish()
    }
}

impl Workspace {
    pub fn new(base_dir: PathBuf, pool: Arc<CpuPool>, default_lang: String) -> Workspace {
        Workspace {
            base_dir: base_dir,
            start_path: None,
            config_read: false,
            pool: pool,
            files: Arc::new(RwLock::new(HashMap::new())),
            shared: Arc::new(RwLock::new(WorkspaceShared {
                message_lang: default_lang,
                source: Source::new(),
                cancel_token: CancelToken::new(),
                check_output: None,
            })),
        }
    }

    pub fn pool(&self) -> &Arc<CpuPool> {
        &self.pool
    }

    pub fn has_read_config(&self) -> bool {
        self.config_read
    }

    pub fn read_config(&mut self) -> io::Result<()> {
        let config = WorkspaceConfig::read(&self.base_dir)?;
        self.start_path = Some(config.start_path);
        if let Some(lang) = config.message_lang {
            let mut shared = self.shared.write();
            shared.message_lang = lang;
        }
        self.config_read = true;
        Ok(())
    }

    pub fn file<'a>(&'a self, uri: &str) -> Option<WorkspaceFile> {
        match uri_to_path(uri) {
            Ok(path) => self.files.read().get(&path).cloned(),
            Err(_) => None,
        }
    }

    pub fn open_file(&self, item: protocol::TextDocumentItem) -> WorkspaceResult<()> {
        let path = uri_to_path(&item.uri)?;

        let shared = &self.shared;
        let pool = &self.pool;
        let mut files = self.files.write();
        let file = files.entry(path.clone()).or_insert_with(|| {
            WorkspaceFile::new(shared, pool, path)
        });

        file.update_document(|doc| {
            if doc.is_some() {
                Err(WorkspaceError("open notification with duplicate file"))
            } else {
                Ok(Some(OpenDocument::new(item)))
            }
        })
    }

    fn ensure_file(&self, path: &Path) -> WorkspaceFile {
        let shared = &self.shared;
        let pool = &self.pool;
        let mut files = self.files.write();
        files.entry(path.to_owned()).or_insert_with(|| {
            WorkspaceFile::new(shared, pool, path.to_owned())
        }).clone()
    }

    pub fn close_file(&self, uri: &str) -> WorkspaceResult<()> {
        let path = uri_to_path(uri)?;

        // closing file breaks the synchronization so the file should be re-read from fs
        let mut files = self.files.write();
        let ok = if let Some(file) = files.remove(&path) {
            file.cancel();
            let file = file.inner.read();
            let sourcefile = self.shared.write().source.remove(file.unit);
            file.document.is_some() && sourcefile.is_some()
        } else {
            false
        };

        if ok {
            Ok(())
        } else {
            Err(WorkspaceError("close notification with non-existent or non-open file"))
        }
    }

    pub fn cancel(&self) {
        self.shared.write().cancel();
    }

    pub fn ensure_check_output(&self) -> WorkspaceResult<ReportFuture<Output>> {
        let start_path = match self.start_path {
            Some(ref path) => path,
            None => {
                return Err(WorkspaceError("cannot start checking without a start file specified"));
            },
        };

        let spare_shared = self.shared.clone();
        let mut shared = self.shared.write();

        if shared.check_output.is_none() {
            // get a future for the entrypoint's chunk
            let start_chunk_fut = self.ensure_file(start_path).ensure_chunk();
            let base_dir = self.base_dir.clone();
            let files = self.files.clone();
            let cancel_token = shared.cancel_token.clone();
            let message_lang = shared.message_lang.clone();

            let fut = start_chunk_fut.map_err(|e| (*e).clone()).and_then(move |chunk_ret| {
                cancel_token.keep_going()?;

                let start_chunk = chunk_ret.0.clone();
                let diags = ReportTree::new(&message_lang, None);
                diags.add_parent(chunk_ret.1.clone());

                // the actual checking process.
                //
                // this will routinely lock the shared, so we avoid locking it from the caller
                // by cloning required values prematurely.
                let fssource = WorkspaceFsSource {
                    inner: Rc::new(RefCell::new(WorkspaceFsSourceInner {
                        shared: spare_shared.clone(),
                        cancel_token: cancel_token.clone(),
                        files: files,
                        temp_units: Vec::new(),
                        temp_files: HashMap::new(),
                        root_report: diags.clone(),
                    })),
                };

                let opts = Rc::new(RefCell::new(FsOptions::new(fssource.clone(), base_dir)));
                let (ok, output) = {
                    // the translation should NOT lock the entire WorkspaceShared.
                    // we also want to drop the proxy report as fast as possible.
                    let mut context = Context::new(diags.report(|r| {
                        diags::translate_diag(r, &spare_shared.read().source)
                    }));
                    let ok = kailua_check::check_from_chunk(&mut context, start_chunk,
                                                            opts).is_ok();
                    (ok, context.into_output())
                };

                // fssource should be owned only by this function; the following should not fail
                let fssource = Rc::try_unwrap(fssource.inner).ok().expect("no single owner");
                let fssource = fssource.into_inner();

                // *now* we can get the write lock and do the cleanup
                let mut shared = spare_shared.write();

                // remove all temporarily added chunks from the source
                // XXX ideally this should be cached as much as possible though
                for unit in fssource.temp_units {
                    let sourcefile = shared.source.remove(unit);
                    assert!(sourcefile.is_some());
                }

                // FsSource may have failed from the cancel request, so we should catch it here
                cancel_token.keep_going()?;

                if ok {
                    Ok((output, diags))
                } else {
                    Err(From::from(diags))
                }
            });

            shared.check_output = Some(self.pool.spawn(fut).boxed().shared());
        }

        Ok(shared.check_output.as_ref().unwrap().clone())
    }
}

struct Ellipsis;

impl fmt::Debug for Ellipsis {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "...") }
}
