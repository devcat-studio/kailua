//! The user-configurable portion of the type checker.

use std::str;
use std::ascii::AsciiExt;
use std::path::{Path, PathBuf, MAIN_SEPARATOR};

use kailua_env::{Spanned, WithLoc};
use kailua_diag::{Report, Stop};
use kailua_syntax::Chunk;

/// Options for customizing the type checker.
///
/// All of those methods return `Err(None)` if the error occurred and has not been reported,
/// or `Err(Some(Stop))` if the error occurred and the caller wants to bail out.
/// If the implementation has a handle to the reporter and wants to do its own reporting,
/// it can report and return `Ok` (recovery) or `Err(Some(Stop))` (error propagation).
pub trait Options {
    /// Called when `package.path` gets assigned to a string literal type.
    ///
    /// Does nothing by default.
    fn set_package_path(&mut self, _path: Spanned<&[u8]>,
                        _report: &Report) -> Result<(), Option<Stop>> {
        Ok(())
    }

    /// Called when `package.cpath` gets assigned to a string literal type.
    ///
    /// Does nothing by default.
    fn set_package_cpath(&mut self, _path: Spanned<&[u8]>,
                         _report: &Report) -> Result<(), Option<Stop>> {
        Ok(())
    }

    /// Called when `require` is called with a string literal type.
    ///
    /// Errors by default; the checker will use its own error message.
    fn require_chunk(&mut self, _path: Spanned<&[u8]>,
                     _report: &Report) -> Result<Chunk, Option<Stop>> {
        Err(None)
    }
}

/// Checker options that are tailored to loading from the file system.
///
/// Follows the same error conventions as `Options`.
pub trait FsSource {
    /// Should try to load a given fully resolved path and return a chunk or `None`.
    ///
    /// `None` here means that the path doesn't exist and the search should continue.
    /// Other error cases can be reported as `Err(..)` as with `Options`.
    fn chunk_from_path(&self, resolved_path: Spanned<&Path>,
                       report: &Report) -> Result<Option<Chunk>, Option<Stop>>;

    /// Should try to parse a byte string containing a relative path to a path buffer.
    ///
    /// Used to delegate the encoding decision to the user.
    /// This will only parse ASCII paths by default, in order to avoid the confusion.
    fn to_path_buf(&self, path: Spanned<&[u8]>, _report: &Report) -> Result<PathBuf, Option<Stop>> {
        if path.is_ascii() {
            Ok(Path::new(str::from_utf8(&path).unwrap()).to_owned())
        } else {
            Err(None)
        }
    }
}

/// An implementation of `Options` that loads from the file system.
///
/// The user should provide `FsSource`, which provides a simpler interface for this use case.
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

    fn search_file(&self, path: Spanned<&[u8]>, search_paths: &[Vec<u8>], suffix: &[u8],
                   report: &Report) -> Result<Option<Chunk>, Option<Stop>> {
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

            let newpath = (&newpath[..]).with_loc(path);
            let resolved_path = self.root.join(self.source.to_path_buf(newpath, report)?);
            let resolved_path = (&*resolved_path).with_loc(path);
            trace!("trying to load {:?}", resolved_path);
            if let Some(chunk) = self.source.chunk_from_path(resolved_path, report)? {
                return Ok(Some(chunk));
            }

            // also try to load a dotted path
            let newpathdot = (&newpathdot[..]).with_loc(path);
            let resolved_path = self.root.join(self.source.to_path_buf(newpathdot, report)?);
            let resolved_path = (&*resolved_path).with_loc(path);
            trace!("trying to load {:?}", resolved_path);
            if let Some(chunk) = self.source.chunk_from_path(resolved_path, report)? {
                return Ok(Some(chunk));
            }
        }

        Ok(None)
    }
}

impl<S: FsSource> Options for FsOptions<S> {
    fn set_package_path(&mut self, path: Spanned<&[u8]>,
                        _report: &Report) -> Result<(), Option<Stop>> {
        self.package_path = path.split(|&b| b == b';').map(|s| s.to_owned()).collect();
        Ok(())
    }

    fn set_package_cpath(&mut self, path: Spanned<&[u8]>,
                         _report: &Report) -> Result<(), Option<Stop>> {
        self.package_cpath = path.split(|&b| b == b';').map(|s| s.to_owned()).collect();
        Ok(())
    }

    fn require_chunk(&mut self, path: Spanned<&[u8]>,
                     report: &Report) -> Result<Chunk, Option<Stop>> {
        if let Some(chunk) = self.search_file(path, &self.package_path, b".kailua", report)? {
            return Ok(chunk);
        }
        if let Some(chunk) = self.search_file(path, &self.package_path, b"", report)? {
            return Ok(chunk);
        }
        if let Some(chunk) = self.search_file(path, &self.package_cpath, b".kailua", report)? {
            return Ok(chunk);
        }
        // avoid loading the native libraries as is

        Err(None)
    }
}

