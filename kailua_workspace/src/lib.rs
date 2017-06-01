//! Workspace support for Kailua.
//!
//! While the type checker itself processes files organically, starting from a start file,
//! many Kailua projects are organized as a workspace---source files and an optional configuration.
//! This crate abstracts the common procedure for determining such configurations.

extern crate serde;
#[macro_use] extern crate serde_derive;
extern crate serde_json;
#[macro_use] extern crate parse_generics_shim;
extern crate regex;
#[macro_use] extern crate lazy_static;
extern crate kailua_env;
#[macro_use] extern crate kailua_diag;
extern crate kailua_syntax;
extern crate kailua_check;

use std::io::{self, Read};
use std::fs::File;
use std::path::{Path, PathBuf};
use kailua_env::{Spanned, WithLoc};
use kailua_diag::{Report, NoReport, Reporter, Stop, Locale};
use kailua_syntax::Chunk;
use kailua_check::options::{Options, FsSource, FsOptions};

mod message;

/// A configuration being built.
///
/// A configuration can be either built incrementally (e.g. from command-line options),
/// or automatically built from a configuration file.
/// There is a set of known paths for the configuration file
/// (currently `BASE_DIR/kailua.json` and `BASE_DIR/.vscode/kailua.json`),
/// so the caller can simply call `Config::use_default_config_paths`
/// when no configuration file is given.
#[derive(Clone, Debug)]
pub struct Config {
    /// A base dir (the workspace root).
    base_dir: PathBuf,

    /// A path to the configuration file, if read. Used for diagnostics.
    config_path: Option<PathBuf>,

    /// Paths to the start file, if any.
    pub start_paths: Vec<PathBuf>,

    /// The explicit value of `package.path`, if any.
    ///
    /// If this value is set, assigning to `package.path` does *not* change
    /// the checker's behavior and will rather issue an warning.
    pub package_path: Option<Vec<u8>>,

    /// The explicit value of `package.cpath`, if any.
    ///
    /// If this value is set, assigning to `package.cpath` does *not* change
    /// the checker's behavior and will rather issue an warning.
    pub package_cpath: Option<Vec<u8>>,

    /// A preferred message locale, if any.
    pub message_locale: Option<Locale>,
}

impl Config {
    pub fn from_start_path(start_path: PathBuf) -> Config {
        let base_dir = start_path.parent().unwrap_or(&Path::new("..")).to_owned();
        Config {
            base_dir: base_dir,
            config_path: None,
            start_paths: vec![start_path],
            package_path: None,
            package_cpath: None,
            message_locale: None,
        }
    }

    pub fn from_base_dir(base_dir: PathBuf) -> Config {
        Config {
            base_dir: base_dir,
            config_path: None,
            start_paths: Vec::new(),
            package_path: None,
            package_cpath: None,
            message_locale: None,
        }
    }

    pub fn base_dir(&self) -> &Path { &self.base_dir }

    pub fn config_path(&self) -> Option<&Path> {
        self.config_path.as_ref().map(|p| &**p)
    }

    pub fn set_config_path(&mut self, path: PathBuf) -> io::Result<bool> {
        if self.config_path.is_some() { return Ok(false); }

        #[derive(Deserialize, Clone, Debug)]
        struct ConfigData {
            start_path: StartPath,
            package_path: Option<String>,
            package_cpath: Option<String>,
            message_lang: Option<String>,
        }

        #[derive(Deserialize, Clone, Debug)]
        #[serde(untagged)]
        enum StartPath { Single(PathBuf), Multi(Vec<PathBuf>) }

        let mut data = String::new();
        File::open(&path)?.read_to_string(&mut data)?;
        let data = dehumanize_json(&data);
        let data: ConfigData = serde_json::de::from_str(&data).map_err(|e| {
            io::Error::new(io::ErrorKind::InvalidData, e)
        })?;
        self.config_path = Some(path);
        self.start_paths = match data.start_path {
            StartPath::Single(p) => vec![self.base_dir.join(p)],
            StartPath::Multi(pp) => pp.into_iter().map(|p| self.base_dir.join(p)).collect(),
        };
        self.package_path = data.package_path.map(|s| s.into_bytes());
        self.package_cpath = data.package_cpath.map(|s| s.into_bytes());
        self.message_locale = if let Some(lang) = data.message_lang {
            if let Some(locale) = Locale::new(&lang) {
                Some(locale)
            } else {
                return Err(io::Error::new(io::ErrorKind::InvalidData, "invalid message language"));
            }
        } else {
            None
        };

        Ok(true)
    }

    pub fn use_default_config_paths(&mut self) {
        let config_path = self.base_dir.join("kailua.json");
        let _ = self.set_config_path(config_path);

        let config_path = self.base_dir.join(".vscode").join("kailua.json");
        let _ = self.set_config_path(config_path);
    }
}

/// A workspace.
///
/// The configuration has been resolved and can be used to make `Options` for the type checker.
#[derive(Clone, Debug)]
pub struct Workspace {
    base_dir: PathBuf,
    config_path: Option<PathBuf>,
    start_paths: Vec<PathBuf>,
    package_path: Option<Vec<u8>>,
    package_cpath: Option<Vec<u8>>,
    message_locale: Locale,
}

impl Workspace {
    pub fn new(config: &Config, default_locale: Locale) -> Option<Workspace> {
        if config.start_paths.is_empty() {
            return None;
        }

        Some(Workspace {
            base_dir: config.base_dir.clone(),
            config_path: config.config_path.clone(),
            start_paths: config.start_paths.clone(),
            package_path: config.package_path.clone(),
            package_cpath: config.package_cpath.clone(),
            message_locale: config.message_locale.unwrap_or(default_locale),
        })
    }

    pub fn base_dir(&self) -> &Path {
        &self.base_dir
    }

    pub fn config_path(&self) -> Option<&Path> {
        self.config_path.as_ref().map(|p| &**p)
    }

    pub fn start_paths(&self) -> &[PathBuf] {
        &self.start_paths
    }

    pub fn message_locale(&self) -> Locale {
        self.message_locale
    }
}

/// An extension to `FsOptions` that is initialized from an workspace.
pub struct WorkspaceOptions<S> {
    options: FsOptions<S>,
    can_update_package_path: bool,
    can_update_package_cpath: bool,
}

impl<S: FsSource> WorkspaceOptions<S> {
    pub fn new(source: S, workspace: &Workspace) -> WorkspaceOptions<S> {
        let mut options = FsOptions::new(source, workspace.base_dir.clone());
        if let Some(ref path) = workspace.package_path {
            let _ = options.set_package_path((&path[..]).without_loc(), &NoReport);
        }
        if let Some(ref path) = workspace.package_cpath {
            let _ = options.set_package_cpath((&path[..]).without_loc(), &NoReport);
        }

        WorkspaceOptions {
            options: options,
            can_update_package_path: workspace.package_path.is_none(),
            can_update_package_cpath: workspace.package_cpath.is_none(),
        }
    }
}

impl<S: FsSource> Options for WorkspaceOptions<S> {
    fn set_package_path(&mut self, path: Spanned<&[u8]>,
                        report: &Report) -> Result<(), Option<Stop>> {
        if self.can_update_package_path {
            self.options.set_package_path(path, report)
        } else {
            report.warn(path.span, message::PackagePathIsExplicitlySet {}).done()?;
            Ok(())
        }
    }

    fn set_package_cpath(&mut self, path: Spanned<&[u8]>,
                         report: &Report) -> Result<(), Option<Stop>> {
        if self.can_update_package_cpath {
            self.options.set_package_cpath(path, report)
        } else {
            report.warn(path.span, message::PackageCpathIsExplicitlySet {}).done()?;
            Ok(())
        }
    }

    fn require_chunk(&mut self, path: Spanned<&[u8]>,
                     report: &Report) -> Result<Chunk, Option<Stop>> {
        self.options.require_chunk(path, report)
    }
}

// serde-json does not allow comments that we really need to...
// this will roughly "tokenize" (seemingly) JSON and remove comments as much as possible.
// also a stray comma before `]` or `}` will be removed.
fn dehumanize_json(s: &str) -> String {
    use regex::Regex;

    lazy_static! {
        static ref TOKEN_PATTERN: Regex =
            Regex::new(r#"(?xs)
                          "(?:\\.|[^"])*" |  # strings should be skipped altogether
                          //[^\r\n]* |       # single-line comment
                          /\*.*?\*/ |        # possibly-multi-line comment
                          .                  # others are simply passed through
                          "#).unwrap();
    }

    let mut out = String::new();
    let mut prev_was_comma = false;
    for tok in TOKEN_PATTERN.find_iter(s) {
        let tok = tok.as_str();
        if tok.starts_with("//") || tok.starts_with("/*") {
            out.push(' ');
        } else if tok == " " || tok == "\t" || tok == "\n" || tok == "\r" {
            out.push_str(tok);
        } else {
            if prev_was_comma && !(tok == "]" || tok == "}") {
                // ignore `,` before `]` or `}`, ignoring comments or whitespaces
                out.push(',');
            }
            prev_was_comma = tok == ",";
            if !prev_was_comma {
                out.push_str(tok);
            }
        }
    }
    if prev_was_comma {
        out.push(',');
    }
    out
}

#[test]
fn test_dehumanize_json() {
    // it is expected that commas go after whitespaces.
    assert_eq!(dehumanize_json("[3, 4/*5*/6]"), "[3 ,4 6]");
    assert_eq!(dehumanize_json("[3, 4//5, 6]\n7"), "[3 ,4 \n7");
    assert_eq!(dehumanize_json(r#"[3, "4//5", "/*6*/"]"#), r#"[3 ,"4//5" ,"/*6*/"]"#);
    assert_eq!(dehumanize_json("[3, 4, 5,\n/*wat*/\n// ???\n]"), "[3 ,4 ,5\n \n \n]");
}

