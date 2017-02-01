use std::mem;
use std::fmt;
use std::io;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::ffi::OsStr;
use std::collections::{hash_map, HashMap};
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use futures::{future, Future, BoxFuture};
use futures_cpupool::CpuPool;
use serde_json;
use url::Url;
use parking_lot::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use walkdir::WalkDir;

use kailua_env::{Unit, Pos, Span, Source, SourceFile, SourceSlice};
use kailua_diag::{self, Report, Localize, Localized};
use kailua_syntax::{Lexer, Nest, NestedToken, Parser, Chunk};
use kailua_check::{self, FsSource, FsOptions, Context, Output};

use diags::{self, ReportTree};
use futureutils::{CancelError, CancelToken, CancelFuture};
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

fn position_to_pos(file: &SourceFile, pos: &protocol::Position) -> Pos {
    if let Some(mut span) = file.line_spans().nth(pos.line as usize) {
        let begin = span.begin().to_usize();
        let end = span.end().to_usize();

        let mut k = pos.character as usize;
        match file.data() {
            SourceSlice::U8(s) => {
                // locate k-th non-continuation byte in s where k is the 0-based column index.
                //
                // this code seems to be overly complicated. this is necessary because
                // we need to detect the end of the line, and a plain .nth(k) cannot determine
                // if the line has k exact scalar values or k is just out of bound.
                let iter = span.zip(s[begin..end].iter());
                for (p, _) in iter.filter(|&(_, &b)| b & 0b1100_0000 != 0b1000_0000) {
                    if k == 0 { return p; }
                    k -= 1;
                }
                if k == 0 { return span.end(); }
                Pos::dummy()
            },

            SourceSlice::U16(_) => {
                // same here, but the logic is much simpler
                if span.len() == k {
                    span.end()
                } else if let Some(p) = span.nth(k) {
                    p
                } else {
                    Pos::dummy()
                }
            },
        }
    } else {
        Pos::dummy()
    }
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

    source: Arc<RwLock<Source>>,
    message_lang: String,

    path: PathBuf,
    unit: Unit,

    // if Some, the file is managed by the client and the text is synchronized
    document: Option<OpenDocument>,

    // each parts are calculated on demand; in either case diagnostics are produced
    span: Option<IoFuture<Span>>,
    tokens: Option<ReportFuture<Arc<Vec<NestedToken>>>>,
    chunk: Option<ReportFuture<Arc<Chunk>>>,

    last_chunk: Option<Arc<Chunk>>,
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
         .field("source", &Ellipsis)
         .field("message_lang", &inner.message_lang)
         .field("path", &inner.path)
         .field("unit", &inner.unit)
         .field("document", &inner.document)
         .field("span", &inner.span.as_ref().map(|_| Ellipsis))
         .field("tokens", &inner.tokens.as_ref().map(|_| Ellipsis))
         .field("chunk", &inner.chunk.as_ref().map(|_| Ellipsis))
         .field("last_chunk", &inner.last_chunk.as_ref().map(|_| Ellipsis))
         .finish()
    }
}

impl WorkspaceFile {
    fn new(shared: &Arc<RwLock<WorkspaceShared>>, pool: &Arc<CpuPool>,
           source: &Arc<RwLock<Source>>, message_lang: &str, path: PathBuf) -> WorkspaceFile {
        WorkspaceFile {
            inner: Arc::new(RwLock::new(WorkspaceFileInner {
                workspace: shared.clone(),
                pool: pool.clone(),
                cancel_token: CancelToken::new(),
                source: source.clone(),
                message_lang: message_lang.to_owned(),
                path: path,
                unit: Unit::dummy(),
                document: None,
                span: None,
                tokens: None,
                chunk: None,
                last_chunk: None,
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
                let mut inner = spare_inner.write();
                inner.cancel_token.keep_going()?;

                let file = if let Some(ref doc) = inner.document {
                    SourceFile::from_u8(inner.path.display().to_string(),
                                        doc.last_text.as_bytes().to_owned())
                } else {
                    SourceFile::from_file(&inner.path)?
                };

                let span = if inner.unit.is_dummy() {
                    let span = inner.source.write().add(file);
                    inner.unit = span.unit();
                    span
                } else {
                    inner.source.write().replace(inner.unit, file).unwrap()
                };
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
                                inner: &mut InnerWrite) -> ReportFuture<Arc<Vec<NestedToken>>> {
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

                let source = inner.source.read();

                let path = source.file(span.unit()).map(|f| f.path());
                let diags = ReportTree::new(&inner.message_lang, path);

                let report = diags.report(|r| diags::translate_diag(r, &source));
                let tokens = collect_tokens(&source, span, &report);
                Ok((Arc::new(tokens), diags))
            });

            inner.tokens = Some(inner.pool.spawn(fut).boxed().shared());
        }

        inner.tokens.as_ref().unwrap().clone()
    }

    pub fn ensure_tokens(&self) -> ReportFuture<Arc<Vec<NestedToken>>> {
        let cloned = self.inner.clone();
        Self::ensure_tokens_with_inner(cloned, &mut self.inner.write())
    }

    fn ensure_chunk_with_inner(spare_inner: Inner,
                               inner: &mut InnerWrite) -> ReportFuture<Arc<Chunk>> {
        if inner.chunk.is_none() {
            let tokens_fut = Self::ensure_tokens_with_inner(spare_inner.clone(), inner);

            let fut = tokens_fut.map_err(|e| (*e).clone()).and_then(move |tokens_ret| {
                let tokens = (*tokens_ret.0).clone();
                let parent_diags = tokens_ret.1.clone();

                let mut inner = spare_inner.write();
                inner.cancel_token.keep_going()?;

                let diags = ReportTree::new(&inner.message_lang, None);
                diags.add_parent(parent_diags);

                // in this future source access is only needed for reporting
                let chunk = {
                    let report = diags.report(|r| diags::translate_diag(r, &inner.source.read()));
                    parse_to_chunk(tokens, &report)
                };
                match chunk {
                    Ok(chunk) => {
                        let chunk = Arc::new(chunk);
                        inner.last_chunk = Some(chunk.clone());
                        Ok((chunk, diags))
                    },
                    Err(_) => Err(From::from(diags)),
                }
            });

            inner.chunk = Some(inner.pool.spawn(fut).boxed().shared());
        }

        inner.chunk.as_ref().unwrap().clone()
    }

    pub fn ensure_chunk(&self) -> ReportFuture<Arc<Chunk>> {
        let cloned = self.inner.clone();
        Self::ensure_chunk_with_inner(cloned, &mut self.inner.write())
    }

    pub fn last_chunk(&self) -> Option<Arc<Chunk>> {
        self.inner.read().last_chunk.clone()
    }

    pub fn translate_position(&self, pos: &protocol::Position) -> BoxFuture<Pos, CancelError<()>> {
        let pos = pos.clone();
        let source = self.inner.read().source.clone();
        self.ensure_span().then(move |res| {
            match res {
                Ok(span) => {
                    let source = source.read();
                    if let Some(file) = source.file(span.unit()) {
                        Ok(position_to_pos(file, &pos))
                    } else {
                        Ok(Pos::dummy())
                    }
                },
                Err(e) => Err(e.as_ref().map(|_| ()))
            }
        }).boxed()
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

// a portion of Workspace that should be shared across WorkspaceFile.
// this should not be modified in the normal cases (otherwise it can be easily deadlocked),
// with an exception of cascading cancellation.
struct WorkspaceShared {
    cancel_token: CancelToken, // used for stopping ongoing checks

    check_output: Option<ReportFuture<Arc<Output>>>,
    last_check_output: Option<Arc<Output>>,
}

impl fmt::Debug for WorkspaceShared {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("WorkspaceShared")
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
    cancel_token: CancelToken, // will be used independently of WorkspaceShared
    files: Arc<RwLock<HashMap<PathBuf, WorkspaceFile>>>,

    source: Arc<RwLock<Source>>,
    temp_units: Vec<Unit>, // will be gone after checking
    temp_files: HashMap<PathBuf, Chunk>,

    message_lang: String,
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
                    (Some((**chunk).clone()), diags.clone())
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

        let sourcefile = match SourceFile::from_file(path) {
            Ok(f) => f,
            Err(ref e) if e.kind() == io::ErrorKind::NotFound => return Ok(None),
            Err(e) => return Err(e.to_string()),
        };
        let span = fssource.source.write().add(sourcefile);
        fssource.temp_units.push(span.unit());

        let diags = ReportTree::new(&fssource.message_lang, path.to_str());
        fssource.root_report.add_parent(diags.clone());

        let chunk = {
            let source = fssource.source.read();
            let report = diags.report(|r| diags::translate_diag(r, &source));
            let tokens = collect_tokens(&source, span, &report);
            parse_to_chunk(tokens, &report)
        };
        match chunk {
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
    message_lang: String,
    config_read: bool,

    pool: Arc<CpuPool>,
    files: Arc<RwLock<HashMap<PathBuf, WorkspaceFile>>>,

    // conceptually this belongs to shared, but it is frequently updated by futures
    // unlike all other fields in shared, so getting this out avoids deadlock
    source: Arc<RwLock<Source>>,

    shared: Arc<RwLock<WorkspaceShared>>,
}

impl fmt::Debug for Workspace {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Workspace")
         .field("base_dir", &self.base_dir)
         .field("start_path", &self.start_path)
         .field("message_lang", &self.message_lang)
         .field("config_read", &self.config_read)
         .field("pool", &Ellipsis)
         .field("files", &self.files)
         .field("source", &Ellipsis)
         .field("shared", &self.shared)
         .finish()
    }
}

impl Workspace {
    pub fn new(base_dir: PathBuf, pool: Arc<CpuPool>, default_lang: String) -> Workspace {
        Workspace {
            base_dir: base_dir,
            start_path: None,
            message_lang: default_lang,
            config_read: false,
            pool: pool,
            files: Arc::new(RwLock::new(HashMap::new())),
            source: Arc::new(RwLock::new(Source::new())),
            shared: Arc::new(RwLock::new(WorkspaceShared {
                cancel_token: CancelToken::new(),
                check_output: None,
                last_check_output: None,
            })),
        }
    }

    pub fn pool(&self) -> &Arc<CpuPool> {
        &self.pool
    }

    pub fn source<'a>(&'a self) -> RwLockReadGuard<'a, Source> {
        self.source.read()
    }

    pub fn has_read_config(&self) -> bool {
        self.config_read
    }

    pub fn read_config(&mut self) -> io::Result<()> {
        let config = WorkspaceConfig::read(&self.base_dir)?;
        self.start_path = Some(config.start_path);
        if let Some(lang) = config.message_lang {
            self.message_lang = lang;
        }
        self.config_read = true;
        Ok(())
    }

    pub fn populate_watchlist(&mut self) {
        for e in WalkDir::new(&self.base_dir).follow_links(true) {
            // we don't care about I/O errors and (in Unix) symlink loops
            let e = if let Ok(e) = e { e } else { continue };

            let ext = e.path().extension();
            if ext == Some(OsStr::new("lua")) || ext == Some(OsStr::new("kailua")) {
                // TODO probably this should be of the lower priority
                let _ = self.ensure_file(e.path()).ensure_chunk();
            }
        }
    }

    pub fn localize(&self, msg: &Localize) -> String {
        Localized::new(&msg, &self.message_lang).to_string()
    }

    pub fn files<'a>(&'a self) -> RwLockReadGuard<'a, HashMap<PathBuf, WorkspaceFile>> {
        self.files.read()
    }

    pub fn file<'a>(&'a self, uri: &str) -> Option<WorkspaceFile> {
        match uri_to_path(uri) {
            Ok(path) => self.files.read().get(&path).cloned(),
            Err(_) => None,
        }
    }

    fn make_file(&self, path: PathBuf) -> WorkspaceFile {
        WorkspaceFile::new(&self.shared, &self.pool, &self.source, &self.message_lang, path)
    }

    fn destroy_file(&self, file: WorkspaceFile) -> bool {
        file.cancel();
        let file = file.inner.read();
        let sourcefile = self.source.write().remove(file.unit);
        file.document.is_some() && sourcefile.is_some()
    }

    pub fn open_file(&self, item: protocol::TextDocumentItem) -> WorkspaceResult<()> {
        let path = uri_to_path(&item.uri)?;

        let mut files = self.files.write();
        let file = files.entry(path.clone()).or_insert_with(|| self.make_file(path));

        file.update_document(|doc| {
            if doc.is_some() {
                Err(WorkspaceError("open notification with duplicate file"))
            } else {
                Ok(Some(OpenDocument::new(item)))
            }
        })
    }

    fn ensure_file(&self, path: &Path) -> WorkspaceFile {
        let mut files = self.files.write();
        files.entry(path.to_owned()).or_insert_with(|| self.make_file(path.to_owned())).clone()
    }

    pub fn close_file(&self, uri: &str) -> WorkspaceResult<()> {
        let path = uri_to_path(uri)?;

        // closing file breaks the synchronization so the file should be re-read from fs
        let mut files = self.files.write();
        let ok = if let hash_map::Entry::Occupied(mut e) = files.entry(path.clone()) {
            // replace the previous WorkspaceFile by a fresh WorkspaceFile
            let file = mem::replace(e.get_mut(), self.make_file(path));
            self.destroy_file(file)
        } else {
            false
        };

        if ok {
            Ok(())
        } else {
            Err(WorkspaceError("close notification with non-existent or non-open file"))
        }
    }

    pub fn on_file_created(&self, uri: &str) {
        if let Ok(path) = uri_to_path(uri) {
            let file = self.ensure_file(&path);
            let _ = file.ensure_chunk();
        }
    }

    pub fn on_file_changed(&self, uri: &str) {
        if let Ok(path) = uri_to_path(uri) {
            let file = self.ensure_file(&path);
            file.cancel();
            let _ = file.ensure_chunk();
        }
    }

    pub fn on_file_deleted(&self, uri: &str) {
        if let Ok(path) = uri_to_path(uri) {
            let mut files = self.files.write();
            if let Some(file) = files.remove(&path) {
                self.destroy_file(file);
            }
        }
    }

    pub fn cancel(&self) {
        self.shared.write().cancel();
    }

    pub fn cancel_future(&self) -> CancelFuture {
        self.shared.read().cancel_token.future()
    }

    pub fn ensure_check_output(&self) -> WorkspaceResult<ReportFuture<Arc<Output>>> {
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
            let source = self.source.clone();
            let cancel_token = shared.cancel_token.clone();
            let message_lang = self.message_lang.clone();

            let fut = start_chunk_fut.map_err(|e| (*e).clone()).and_then(move |chunk_ret| {
                cancel_token.keep_going()?;

                let start_chunk = (*chunk_ret.0).clone();
                let diags = ReportTree::new(&message_lang, None);
                diags.add_parent(chunk_ret.1.clone());

                // the actual checking process.
                //
                // this will routinely lock the shared, so we avoid locking it from the caller
                // by cloning required values prematurely.
                let fssource = WorkspaceFsSource {
                    inner: Rc::new(RefCell::new(WorkspaceFsSourceInner {
                        cancel_token: cancel_token.clone(),
                        files: files,
                        source: source.clone(),
                        temp_units: Vec::new(),
                        temp_files: HashMap::new(),
                        message_lang: message_lang.clone(),
                        root_report: diags.clone(),
                    })),
                };

                let opts = Rc::new(RefCell::new(FsOptions::new(fssource.clone(), base_dir)));
                let (ok, output) = {
                    // the translation should NOT lock the source (read or write) indefinitely.
                    // we also want to drop the proxy report as fast as possible.
                    let mut context = Context::new(diags.report(|r| {
                        diags::translate_diag(r, &source.read())
                    }));
                    let ok = kailua_check::check_from_chunk(&mut context, start_chunk,
                                                            opts).is_ok();
                    (ok, context.into_output())
                };

                // fssource should be owned only by this function; the following should not fail
                let fssource = Rc::try_unwrap(fssource.inner).ok().expect("no single owner");
                let fssource = fssource.into_inner();

                // remove all temporarily added chunks from the source
                // XXX ideally this should be cached as much as possible though
                let mut source = source.write();
                for unit in fssource.temp_units {
                    let sourcefile = source.remove(unit);
                    assert!(sourcefile.is_some());
                }

                // FsSource may have failed from the cancel request, so we should catch it here
                cancel_token.keep_going()?;

                if ok {
                    let output = Arc::new(output);
                    spare_shared.write().last_check_output = Some(output.clone());
                    Ok((output, diags))
                } else {
                    Err(From::from(diags))
                }
            });

            shared.check_output = Some(self.pool.spawn(fut).boxed().shared());
        }

        Ok(shared.check_output.as_ref().unwrap().clone())
    }

    pub fn last_check_output(&self) -> Option<Arc<Output>> {
        self.shared.read().last_check_output.clone()
    }
}

struct Ellipsis;

impl fmt::Debug for Ellipsis {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "...") }
}

