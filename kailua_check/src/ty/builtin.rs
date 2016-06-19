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
}

impl Builtin {
    pub fn from_name(name: &[u8]) -> Option<Builtin> {
        match name {
            b"require"     => Some(Builtin::Require),
            b"type"        => Some(Builtin::Type),
            b"assert"      => Some(Builtin::Assert),
            b"assert-not"  => Some(Builtin::AssertNot),
            b"assert-type" => Some(Builtin::AssertType),
            _ => None,
        }
    }

    pub fn name(&self) -> &'static str {
        match *self {
            Builtin::Require => "require",
            Builtin::Type => "type",
            Builtin::Assert => "assert",
            Builtin::AssertNot => "assert-not",
            Builtin::AssertType => "assert-type",
        }
    }
}

