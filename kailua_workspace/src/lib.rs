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
use kailua_check::{Options, FsSource, FsOptions};

mod message;

#[derive(Clone, Debug)]
pub struct Config {
    base_dir: PathBuf,
    config_path: Option<PathBuf>, // for diagnostics
    pub start_path: Option<PathBuf>,
    pub package_path: Option<Vec<u8>>,
    pub package_cpath: Option<Vec<u8>>,
    pub message_locale: Option<Locale>,
}

impl Config {
    pub fn from_start_path(start_path: PathBuf) -> Config {
        let base_dir = start_path.parent().unwrap_or(&Path::new("..")).to_owned();
        Config {
            base_dir: base_dir,
            config_path: None,
            start_path: Some(start_path),
            package_path: None,
            package_cpath: None,
            message_locale: None,
        }
    }

    pub fn from_base_dir(base_dir: PathBuf) -> Config {
        Config {
            base_dir: base_dir,
            config_path: None,
            start_path: None,
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
            start_path: PathBuf,
            package_path: Option<String>,
            package_cpath: Option<String>,
            message_lang: Option<String>,
        }

        let mut data = String::new();
        File::open(&path)?.read_to_string(&mut data)?;
        let data = dehumanize_json(&data);
        let data: ConfigData = serde_json::de::from_str(&data).map_err(|e| {
            io::Error::new(io::ErrorKind::InvalidData, e)
        })?;
        self.config_path = Some(path);
        self.start_path = Some(self.base_dir.join(data.start_path));
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

#[derive(Clone, Debug)]
pub struct Workspace {
    base_dir: PathBuf,
    config_path: Option<PathBuf>,
    start_path: PathBuf,
    package_path: Option<Vec<u8>>,
    package_cpath: Option<Vec<u8>>,
    message_locale: Locale,
}

impl Workspace {
    pub fn new(config: &Config, default_locale: Locale) -> io::Result<Workspace> {
        let start_path = if let Some(ref path) = config.start_path {
            path.clone()
        } else {
            return Err(io::Error::new(io::ErrorKind::Other, "start path is missing"));
        };

        Ok(Workspace {
            base_dir: config.base_dir.clone(),
            config_path: config.config_path.clone(),
            start_path: start_path,
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

    pub fn start_path(&self) -> &Path {
        &self.start_path
    }

    pub fn message_locale(&self) -> Locale {
        self.message_locale
    }
}

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

