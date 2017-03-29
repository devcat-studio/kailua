use std::str;
use std::ascii::AsciiExt;
use std::path::{Path, PathBuf, MAIN_SEPARATOR};

use kailua_diag::Stop;
use kailua_syntax::Chunk;

pub trait Options {
    // all of those methods return `Err(None)` if the error occurred and has not been reported,
    // or `Err(Some(Stop))` if the error occurred and the caller wants to bail out.
    // if the implementation has a handle to the reporter and wants to do its own reporting,
    // it can report and return `Ok` (recovery) or `Err(Some(Stop))` (error propagation).

    fn set_package_path(&mut self, _path: &[u8]) -> Result<(), Option<Stop>> { Ok(()) }
    fn set_package_cpath(&mut self, _path: &[u8]) -> Result<(), Option<Stop>> { Ok(()) }

    fn require_chunk(&mut self, _path: &[u8]) -> Result<Chunk, Option<Stop>> {
        Err(None)
    }
}

pub trait FsSource {
    // `Ok(Some(chunk))` normally; `Ok(None)` if path doesn't exist and search should continue;
    // `Err(None)` and `Err(Some(Stop))` follow the convention of `Options`.
    fn chunk_from_path(&self, resolved_path: &Path) -> Result<Option<Chunk>, Option<Stop>>;

    fn to_path_buf(&self, path: &[u8]) -> Result<PathBuf, Option<Stop>> {
        // by default we avoid parsing multibyte paths as it depends on the system encoding
        if path.is_ascii() {
            Ok(Path::new(str::from_utf8(path).unwrap()).to_owned())
        } else {
            Err(None)
        }
    }
}

pub struct FsOptions<S> {
    source: S,
    root: PathBuf,
    package_path: Vec<Vec<u8>>,
    package_cpath: Vec<Vec<u8>>,
}

impl<S: FsSource> FsOptions<S> {
    pub fn new(source: S, root: PathBuf) -> FsOptions<S> {
        FsOptions {
            source: source,
            root: root,

            // by default, local files only
            package_path: vec![b"?.lua".to_vec()],
            package_cpath: vec![],
        }
    }

    fn search_file(&self, path: &[u8], search_paths: &[Vec<u8>],
                   suffix: &[u8]) -> Result<Option<Chunk>, Option<Stop>> {
        for template in search_paths {
            let mut newpath = Vec::new();
            let mut newpathdot = Vec::new();
            for (i, e) in template.split(|&b| b == b'?').enumerate() {
                if i > 0 {
                    newpath.extend(path.iter().map(|&b| {
                        if b == b'.' { MAIN_SEPARATOR as u8 } else { b }
                    }));
                    newpathdot.extend(path.iter().cloned());
                }
                newpath.extend_from_slice(e);
                newpathdot.extend_from_slice(e);
            }
            newpath.extend_from_slice(suffix);
            newpathdot.extend_from_slice(suffix);

            let resolved_path = self.root.join(self.source.to_path_buf(&newpath)?);
            trace!("trying to load {:?}", resolved_path);
            if let Some(chunk) = self.source.chunk_from_path(&resolved_path)? {
                return Ok(Some(chunk));
            }

            // also try to load a dotted path
            let resolved_path = self.root.join(self.source.to_path_buf(&newpathdot)?);
            trace!("trying to load {:?}", resolved_path);
            if let Some(chunk) = self.source.chunk_from_path(&resolved_path)? {
                return Ok(Some(chunk));
            }
        }

        Ok(None)
    }
}

impl<S: FsSource> Options for FsOptions<S> {
    fn set_package_path(&mut self, path: &[u8]) -> Result<(), Option<Stop>> {
        self.package_path = path.split(|&b| b == b';').map(|s| s.to_owned()).collect();
        Ok(())
    }

    fn set_package_cpath(&mut self, path: &[u8]) -> Result<(), Option<Stop>> {
        self.package_cpath = path.split(|&b| b == b';').map(|s| s.to_owned()).collect();
        Ok(())
    }

    fn require_chunk(&mut self, path: &[u8]) -> Result<Chunk, Option<Stop>> {
        if let Some(chunk) = self.search_file(&path, &self.package_path, b".kailua")? {
            return Ok(chunk);
        }
        if let Some(chunk) = self.search_file(&path, &self.package_path, b"")? {
            return Ok(chunk);
        }
        if let Some(chunk) = self.search_file(&path, &self.package_cpath, b".kailua")? {
            return Ok(chunk);
        }
        // avoid loading the native libraries as is

        Err(None)
    }
}

