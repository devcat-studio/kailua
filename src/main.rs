//! 🌴 Type Checker and IDE Support for Lua.

extern crate env_logger;
#[macro_use] extern crate clap;
extern crate kailua_env;
extern crate kailua_diag;
extern crate kailua_syntax;
extern crate kailua_check;
extern crate kailua_workspace;
extern crate kailua_langsvr;

use std::io;
use std::path::Path;
use clap::{App, Error, ErrorKind};
use kailua_workspace::Workspace;

fn parse_and_check(workspace: &Workspace, quiet: bool) -> Result<(), String> {
    use std::str;
    use std::io;
    use std::cell::RefCell;
    use std::rc::Rc;
    use std::path::Path;

    use kailua_env::{Span, Spanned, Source, SourceFile, WithLoc};
    use kailua_diag::message::{Locale, Localize};
    use kailua_diag::report::{Stop, Kind, Report, ConsoleReport, TrackMaxKind};
    use kailua_syntax::{parse_chunk, Chunk};
    use kailua_check::check_from_chunk_with_preloading;
    use kailua_check::env::Context;
    use kailua_check::options::FsSource;
    use kailua_workspace::WorkspaceOptions;

    struct LocalFsSource {
        source: Rc<RefCell<Source>>,
    }

    impl FsSource for LocalFsSource {
        fn chunk_from_path(&self, resolved_path: Spanned<&Path>,
                           report: &Report) -> Result<Option<Chunk>, Option<Stop>> {
            match SourceFile::from_file(&resolved_path) {
                Ok(file) => {
                    let span = self.source.borrow_mut().add(file);
                    if let Ok(chunk) = parse_chunk(&self.source.borrow(), span, report) {
                        Ok(Some(chunk))
                    } else {
                        Err(Some(Stop)) // we have already reported parsing errors
                    }
                }
                Err(e) => {
                    if e.kind() == io::ErrorKind::NotFound {
                        Ok(None)
                    } else {
                        Err(None)
                    }
                }
            }
        }
    }

    struct OptionalConsoleReport {
        quiet: bool,
        report: ConsoleReport,
    }

    impl Report for OptionalConsoleReport {
        fn message_locale(&self) -> Locale {
            self.report.message_locale()
        }

        fn add_span(&self, kind: Kind, span: Span, msg: &Localize) -> kailua_diag::Result<()> {
            if self.quiet {
                Ok(())
            } else {
                self.report.add_span(kind, span, msg)
            }
        }
    }

    let source = Rc::new(RefCell::new(Source::new()));
    let report = Rc::new(TrackMaxKind::new(OptionalConsoleReport {
        quiet: quiet,
        report: ConsoleReport::with_locale(source.clone(), workspace.message_locale()),
    }));

    // TODO multiple outputs should deduplicate warnings if possible
    for start_path in workspace.start_paths() {
        let mut context = Context::new(report.clone());

        let fssource = LocalFsSource { source: source.clone() };
        let filechunk = match fssource.chunk_from_path((**start_path).without_loc(), &report) {
            Ok(Some(chunk)) => chunk,
            _ => {
                return Err(format!("Couldn't open a start path `{}`", start_path.display()));
            }
        };

        // stop after parsing errors (not very useful for CLI usage)
        if !report.can_continue() {
            return Err(format!("Stopped due to prior errors"));
        }

        let opts = Rc::new(RefCell::new(WorkspaceOptions::new(fssource, start_path, workspace)));

        let output = check_from_chunk_with_preloading(&mut context, filechunk, opts,
                                                      workspace.preload());
        if !(output.is_ok() && report.can_continue()) {
            return Err(format!("Stopped due to prior errors"));
        }
    }

    Ok(())
}

fn build_app() -> App<'static, 'static> {
    clap_app!(kailua =>
        (@setting SubcommandRequiredElseHelp)
        (@setting UnifiedHelpMessage)
        (@setting NextLineHelp)
        (@setting VersionlessSubcommands)
        (version: option_env!("CARGO_PKG_VERSION").unwrap_or("(version unknown)"))
        (about:
            "\u{1f334} Type Checker and IDE Support for Lua.\n\
             https://github.com/devcat-studio/kailua/")
        (max_term_width: 100)
        (@subcommand check =>
            (@setting UnifiedHelpMessage)
            (@setting NextLineHelp)
            (about:
                "Performs type checking in the workspace.\n\
                 \n\
                 Prints reports (can be suppressed with `-q`) to the standard error,\n\
                 then terminates with an exit code 1 on error.\n\
                 The configuration can be either given as JSON or command-line options.")
            (@arg config: -c --config [PATH]
                "Overrides a default configuration path.\n\
                 Defaults to `BASE_DIR/kailua.json` or `BASE_DIR/.vscode/kailua.json`, \
                 whichever comes first.")
            (@arg add_package_path: -p --("add-package-path") [TEMPLATE] +multiple
                conflicts_with[set_package_path]
                "Adds a given template (a path with a hole `?`) to `package.path`.\n\
                 This will make dynamic assignments to `package.path` \
                 unable to affect the type checking (will warn instead).")
            (@arg add_package_cpath: --("add-package-cpath") [TEMPLATE] +multiple
                conflicts_with[set_package_cpath]
                "Adds a given template (a path with a hole `?`) to `package.cpath`.\n\
                 This will make dynamic assignments to `package.cpath` \
                 unable to affect the type checking (will warn instead).")
            (@arg set_package_path: --("set-package-path") [TEMPLATES]
                "Sets `package.path` to the exact string given (including separator `;`).\n\
                 Similar to `--add-package-path` but will ignore the default values.")
            (@arg set_package_cpath: --("set-package-cpath") [TEMPLATES]
                "Sets `package.cpath` to the exact string given (including separator `;`).\n\
                 Similar to `--add-package-cpath` but will ignore the default values.")
            (@arg quiet: -q --quiet
                "Suppresses all reports.")
            (@arg message_locale: -l --("message-locale") [LOCALE]
                "Sets the message locales. Defaults to the system language.")
            (@arg path:
                "A path to start checking. \
                 This can be either a path to the base directory \
                 (which may contain configuration files), \
                 or a path to the Lua file in which case the configuration path should be given. \
                 Defaults to the current directory.")
        )
        (@subcommand langsvr =>
            (about: "Launches a language server. Not to be used directly.")
            (@group target =>
                (@attributes +required)
                (@arg tcp: --tcp [ADDR]
                    "Connect to the client via given `host:port`.")
                (@arg stdio: --stdio
                    "Connect to the client via standard input and output.")
            )
        )
    )
}

fn invalid_value(s: &str) -> ! {
    Error::with_description(s, ErrorKind::InvalidValue).exit();
}

fn io_error(s: &str) -> ! {
    Error::with_description(s, ErrorKind::Io).exit();
}

fn io_error_while(e: io::Error, s: &str) -> ! {
    let mut e = Error::from(e);
    e.message = format!("{} (while {})", e.message, s);
    e.exit();
}

pub fn main() {
    use kailua_diag::message::{Locale, get_message_locale};
    use kailua_workspace::{Config, Workspace};
    use kailua_langsvr::Target;

    env_logger::init().unwrap();

    let matches = build_app().get_matches();

    if let Some(ref matches) = matches.subcommand_matches("check") {
        let path = Path::new(matches.value_of("path").unwrap_or("."));

        let mut config = if path.is_dir() {
            Config::from_base_dir(path.to_owned())
        } else {
            Config::from_start_path(path.to_owned())
        };

        if let Some(config_path) = matches.value_of("config") {
            match config.set_config_path(Path::new(config_path).to_owned()) {
                Ok(true) => {}
                Ok(false) => {
                    io_error(&format!("Couldn't open a configuration file `{}`", config_path));
                }
                Err(e) => {
                    io_error_while(e, &format!("opening a configuration file `{}`", config_path));
                }
            }
        } else {
            config.use_default_config_paths();
        }

        let parse_package_paths = |set: &str, add: &str| {
            if let Some(path) = matches.value_of(set) {
                Some(path.to_owned().into_bytes())
            } else if let Some(paths) = matches.values_of(add) {
                let mut search_paths = b"?.lua".to_vec();
                for path in paths {
                    if !search_paths.is_empty() {
                        search_paths.push(b';');
                    }
                    search_paths.extend_from_slice(path.as_bytes());
                }
                Some(search_paths)
            } else {
                None
            }
        };

        config.package_path =
            parse_package_paths("set_package_path", "add_package_path").or(config.package_path);
        config.package_cpath =
            parse_package_paths("set_package_cpath", "add_package_cpath").or(config.package_cpath);

        let quiet = matches.is_present("quiet");

        let message_locale = if let Some(locale) = matches.value_of("message_locale") {
            if let Some(locale) = Locale::new(locale) {
                locale
            } else {
                invalid_value(&format!("Unrecognized message locale `{}`", locale))
            }
        } else {
            get_message_locale().unwrap_or_else(|| Locale::dummy())
        };

        if let Some(workspace) = Workspace::new(&config, message_locale) {
            if let Err(e) = parse_and_check(&workspace, quiet) {
                // clap does not have something like ErrorKind::Other :(
                io_error(&e);
            }
        } else {
            assert!(config.config_path().is_none());
            io_error(
                &format!("Couldn't open a default configuration file at `{}` or `{}`",
                         config.base_dir().join("kailua.json").display(),
                         config.base_dir().join(".vscode").join("kailua.json").display())
            );
        }

        return;
    }

    if let Some(ref matches) = matches.subcommand_matches("langsvr") {
        // delegate to kailua_langsvr
        let target = if let Some(addr) = matches.value_of("tcp") {
            if let Ok(addr) = addr.parse() {
                Target::TCP(addr)
            } else {
                invalid_value(&format!("Invalid TCP socket address `{}`", addr))
            }
        } else {
            assert!(matches.is_present("stdio"));
            Target::Stdio
        };

        if let Err(e) = kailua_langsvr::main(target) {
            io_error_while(e, "launching the language server");
        }

        return;
    }
}

