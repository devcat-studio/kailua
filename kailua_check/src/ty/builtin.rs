use kailua_diag::Reporter;
use kailua_syntax::Attr;
use diag::CheckResult;
use super::TypeResolver;
use message as m;

// TODO will be renamed to "tag", the "builtin" is very confusing name
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Builtin {
    // only used to test built-ins requiring subtypes and those not.
    _Subtype,
    _NoSubtype,

    // function(string, ...) -> table
    //
    // also loads a code specified by the string literal (if any).
    Require,

    // function(any) -> string
    //
    // used by assertions. has very limited return values.
    Type,

    // function(any, ...) -> any
    //
    // also recognizes the following kinds of expressions:
    // - `<expr>` asserts that the corresponding type is truthy
    // - `not <expr>` asserts that the corresponding type is falsy
    // - `<type>(<expr>) == <string>`, where <type> is a value with Type built-in tag
    //
    // expressions can be chained by `and` or `or`, subject to De Morgan's law.
    // any unrecognized expression or non-definitive conditions are ignored.
    Assert,

    // function(any, ...) -> any
    //
    // same to `assert(not <expr>, ...)`.
    AssertNot,

    // function(any, string, ...) -> any
    //
    // basically same to `assert(type(<expr>) == <string>, ...)`.
    // also accepts additional string literals:
    // - `"integer"` for an integer.
    AssertType,

    // function(table, ...) -> (function(table, any) -> (any?, any), table, any, ...)
    //
    // the first argument is exactly resolved first and the return values are updated
    // to fit the following generic signature if possible (not yet directly supported):
    //     ({T => U}, ...) -> (function({T => U}, T) -> (T?, U), {T => U}, T, ...)
    GenericPairs,

    // table
    //
    // a table mirroring the global environment.
    //
    // XXX not yet implemented
    GlobalEnv,

    // function(...) -> (...)
    //
    // calling this function will alter the global environment in unspecified way,
    // so it is no longer assumed to be known after the call.
    //
    // XXX not yet implemented
    GlobalEval,

    // function(string, ...) -> (...)
    //
    // calling this function will enable the "module" mode in Lua 5.1.
    //
    // XXX not yet implemented
    // XXX probably requires ModuleEnv built-in
    BecomeModule,

    // var string
    //
    // assigning to these variables will update the `require` paths (in the order).
    // they are distinct to each other, but otherwise have the same syntax to Lua's.
    PackagePath,
    PackageCpath,

    // table
    //
    // the metatable for all strings. the method calls for string go through this table,
    // and any modification to this table is immediately available to subsequent code.
    //
    // note: this table is linked to the current environment only by `--# assume`;
    //       there is no other valid way to get a table with such a type.
    StringMeta,

    // function(<class prototype type>?) -> <class prototype type>
    //
    // a function that makes a class prototype. if the argument is given, it should be
    // another existing prototype that acts as a parent of a newly created prototype.
    //
    // the prototype is initially unnamed; the first assignment to a local or global variable
    // will set its name, and the name cannot be changed thereafter.
    MakeClass,

    // currently <class instance type>
    //
    // this is a type of `self` in the constructor method. normally it is set to a var slot,
    // but the constructor is special and has to update the type of `self` on the fly.
    // at the end of the constructor, the checker collects the current type of `self` and
    // uses it as a template for every other usage of given class instance type.
    //
    // there are a couple of strong restrictions:
    // - unlike normal currently slot, the type cannot be assigned directly; only can be updated.
    // - the table adaptation is restricted and only the record is supported.
    //
    // this is internally created and cannot be constructed in normal ways.
    Constructible,

    // var function([constructible] <class instance type>, ...) -> any
    //
    // the constructor method is marked specially so that the (likely only) assignment to
    // that method collects the function signature and create an appropriate `new` method.
    //
    // this is internally created and cannot be constructed in normal ways.
    Constructor,
}

impl Builtin {
    pub fn from(attr: &Attr, resolv: &mut TypeResolver) -> CheckResult<Option<Builtin>> {
        match &attr.name.base[..] {
            b"internal subtype"    => Ok(Some(Builtin::_Subtype)),
            b"internal no_subtype" => Ok(Some(Builtin::_NoSubtype)),

            b"require"       => Ok(Some(Builtin::Require)),
            b"type"          => Ok(Some(Builtin::Type)),
            b"assert"        => Ok(Some(Builtin::Assert)),
            b"assert_not"    => Ok(Some(Builtin::AssertNot)),
            b"assert_type"   => Ok(Some(Builtin::AssertType)),
            b"generic_pairs" => Ok(Some(Builtin::GenericPairs)),
            b"genv"          => Ok(Some(Builtin::GlobalEnv)),
            b"geval"         => Ok(Some(Builtin::GlobalEval)),
            b"become_module" => Ok(Some(Builtin::BecomeModule)),
            b"package_path"  => Ok(Some(Builtin::PackagePath)),
            b"package_cpath" => Ok(Some(Builtin::PackageCpath)),
            b"string_meta"   => Ok(Some(Builtin::StringMeta)),
            b"make_class"    => Ok(Some(Builtin::MakeClass)),

            _ => {
                try!(resolv.warn(&attr.name, m::UnknownAttrName { name: &attr.name.base }).done());
                Ok(None)
            }
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
            Builtin::MakeClass    => "make_class",

            Builtin::_Subtype      => "internal subtype",
            Builtin::_NoSubtype    => "internal no_subtype",
            Builtin::Constructible => "internal constructible",
            Builtin::Constructor   => "internal constructor",
        }
    }

    // is the effect of this built-in local to the current scope?
    // non-scope-local built-ins have a limited ability inside non-global scopes.
    // (exception: the once function called from the global scope counts as global.)
    // XXX seems that it needs more accurate definition
    pub fn scope_local(&self) -> bool {
        match *self {
            Builtin::Type |
            Builtin::Assert |
            Builtin::AssertNot |
            Builtin::AssertType |
            Builtin::GenericPairs |
            Builtin::MakeClass |
            Builtin::Constructible |
            Builtin::Constructor => true,
            _ => false,
        }
    }

    // does this built-in need the strict subtyping rule?
    // for example, it is NOT possible to update `var [type] function(any) -> string` with
    // a plain `function(any) -> string`, it has to be `[type] function(any) -> string`.
    // generally the built-in working via assignment should have this false.
    pub fn needs_subtype(&self) -> bool {
        match *self {
            // only used for testing
            Builtin::_Subtype => true,
            Builtin::_NoSubtype => false,

            Builtin::PackagePath |
            Builtin::PackageCpath |
            Builtin::Constructible |
            Builtin::Constructor => false,
            _ => true,
        }
    }
}

