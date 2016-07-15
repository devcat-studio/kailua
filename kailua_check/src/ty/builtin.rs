#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Builtin {
    // (string, ...) -> table
    // also loads a code specified by the string literal (if any).
    Require,

    // (any) -> string
    // used by assertions.
    Type,

    // (any, ...) -> any
    // also recognizes the following kinds of expressions:
    // - `<expr>` asserts that the corresponding type is truthy
    // - `not <expr>` asserts that the corresponding type is falsy
    // - `<type>(<expr>) == <string>`, where <type> is a value with Type built-in tag
    // expressions can be chained by `and` or `or`, subject to De Morgan's law.
    // any unrecognized expression or non-definitive conditions are ignored.
    Assert,

    // (any, ...) -> any
    // same to `assert(not <expr>, ...)`.
    AssertNot,

    // (any, string, ...) -> any
    // basically same to `assert(type(<expr>) == <string>, ...)`.
    // also accepts additional string literals:
    // - `"integer"` for an integer.
    AssertType,

    // (table, ...) -> (function(table, any) -> (any?, any), table, any, ...)
    // the first argument is exactly resolved first and the return values are updated
    // to fit the following generic signature if possible (not yet directly supported):
    //     ({T => U}, ...) -> (function({T => U}, T) -> (T?, U), {T => U}, T, ...)
    GenericPairs,
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
            _ => None,
        }
    }

    pub fn name(&self) -> &'static str {
        match *self {
            Builtin::Require => "require",
            Builtin::Type => "type",
            Builtin::Assert => "assert",
            Builtin::AssertNot => "assert_not",
            Builtin::AssertType => "assert_type",
            Builtin::GenericPairs => "generic_pairs",
        }
    }
}

