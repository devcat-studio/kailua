#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Builtin {
    // function(string, ...) -> table
    // also loads a code specified by the string literal (if any).
    Require,

    // function(any) -> string
    // used by assertions. has very limited return values.
    Type,

    // function(any, ...) -> any
    // also recognizes the following kinds of expressions:
    // - `<expr>` asserts that the corresponding type is truthy
    // - `not <expr>` asserts that the corresponding type is falsy
    // - `<type>(<expr>) == <string>`, where <type> is a value with Type built-in tag
    // expressions can be chained by `and` or `or`, subject to De Morgan's law.
    // any unrecognized expression or non-definitive conditions are ignored.
    Assert,

    // function(any, ...) -> any
    // same to `assert(not <expr>, ...)`.
    AssertNot,

    // function(any, string, ...) -> any
    // basically same to `assert(type(<expr>) == <string>, ...)`.
    // also accepts additional string literals:
    // - `"integer"` for an integer.
    AssertType,

    // function(table, ...) -> (function(table, any) -> (any?, any), table, any, ...)
    // the first argument is exactly resolved first and the return values are updated
    // to fit the following generic signature if possible (not yet directly supported):
    //     ({T => U}, ...) -> (function({T => U}, T) -> (T?, U), {T => U}, T, ...)
    GenericPairs,

    // XXX the following built-ins are not yet implemented

    // table
    // a table mirroring the global environment.
    GlobalEnv,

    // function(...) -> (...)
    // calling this function will alter the global environment in unspecified way,
    // so it is no longer assumed to be known after the call.
    GlobalEval,

    // function(string, ...) -> (...)
    // calling this function will enable the "module" mode in Lua 5.1.
    // XXX probably requires ModuleEnv built-in
    BecomeModule,

    // var string
    // assigning to these variables will update the `require` paths (in the order).
    // they are distinct to each other, but otherwise have the same syntax to Lua's.
    PackagePath,
    PackageCpath,

    // table
    // the metatable for all strings. the method calls for string go through this table,
    // and any modification to this table is immediately available to subsequent code.
    StringMeta,
}

impl Builtin {
    pub fn from_name(name: &[u8]) -> Option<Builtin> {
        match name {
            b"require"       => Some(Builtin::Require),
            b"type"          => Some(Builtin::Type),
            b"assert"        => Some(Builtin::Assert),
            b"assert_not"    => Some(Builtin::AssertNot),
            b"assert_type"   => Some(Builtin::AssertType),
            b"generic_pairs" => Some(Builtin::GenericPairs),
            b"genv"          => Some(Builtin::GlobalEnv),
            b"geval"         => Some(Builtin::GlobalEval),
            b"become_module" => Some(Builtin::BecomeModule),
            b"package_path"  => Some(Builtin::PackagePath),
            b"package_cpath" => Some(Builtin::PackageCpath),
            b"string_meta"   => Some(Builtin::StringMeta),
            _ => None,
        }
    }

    pub fn name(&self) -> &'static str {
        match *self {
            Builtin::Require      => "require",
            Builtin::Type         => "type",
            Builtin::Assert       => "assert",
            Builtin::AssertNot    => "assert_not",
            Builtin::AssertType   => "assert_type",
            Builtin::GenericPairs => "generic_pairs",
            Builtin::GlobalEnv    => "genv",
            Builtin::GlobalEval   => "geval",
            Builtin::BecomeModule => "become_module",
            Builtin::PackagePath  => "package_path",
            Builtin::PackageCpath => "package_cpath",
            Builtin::StringMeta   => "string_meta",
        }
    }

    // is the effect of this built-in local to the current scope?
    // non-scope-local built-ins have a limited ability inside non-global scopes.
    // (exception: the once function called from the global scope counts as global.)
    pub fn scope_local(&self) -> bool {
        match *self {
            Builtin::Type |
            Builtin::Assert |
            Builtin::AssertNot |
            Builtin::AssertType |
            Builtin::GenericPairs => true,
            _ => false,
        }
    }

    // does this built-in need the strict subtyping rule?
    // for example, it is NOT possible to update `var [type] function(any) -> string` with
    // a plain `function(any) -> string`, it has to be `[type] function(any) -> string`.
    // generally the built-in working via assignment should have this false.
    pub fn needs_subtype(&self) -> bool {
        match *self {
            Builtin::PackagePath |
            Builtin::PackageCpath => false,
            _ => true,
        }
    }
}

