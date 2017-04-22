use kailua_diag::{Result, Reporter};
use kailua_syntax::ast::Attr;
use super::TypeResolver;
use message as m;

/// A type tag for giving a type special meanings.
///
/// Generally a type with a tag is equal to or a subtype of a specific form of types;
/// it is safe to put a tag to an non-conforming type, but that won't work well in general.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Tag {
    // only used to test tags requiring subtypes and those not.
    #[doc(hidden)] _Subtype,
    #[doc(hidden)] _NoSubtype,
    #[doc(hidden)] _NoSubtype2,

    /// `function(string, ...) -> table`
    ///
    /// Also loads a code specified by the string literal (if any).
    /// Any exported types in that cdoe will be also brought to the current local scope.
    Require,

    /// `function(any) -> string`
    ///
    /// Used by assertions but does nothing by its own. Has very limited return values.
    Type,

    /// `function(any, ...) -> any`
    ///
    /// Also recognizes the following kinds of expressions:
    ///
    /// - `<expr>` asserts that the corresponding type is truthy.
    /// - `not <expr>` asserts that the corresponding type is falsy.
    /// - `<type>(<expr>) == <string>`, where `<type>` is a value with `Type` tag.
    ///
    /// Expressions can be chained by `and` or `or`, subject to De Morgan's law.
    /// Any unrecognized expression or non-definitive conditions are ignored.
    Assert,

    /// `function(any, ...) -> any`
    ///
    /// Same to `assert(not <expr>, ...)`.
    AssertNot,

    /// `function(any, string, ...) -> any`
    ///
    /// Basically same to `assert(type(<expr>) == <string>, ...)`.
    ///
    /// Also accepts additional string literals:
    ///
    /// - `"integer"` for an integer.
    AssertType,

    /// `function(table, ...) -> (function(table, any) -> (any?, any), table, any, ...)`
    ///
    /// A hack for supporting generic `pairs` and `ipairs` functions.
    /// The first argument is exactly resolved first and the return values are updated
    /// to fit the following generic signature if possible (not yet directly supported):
    ///
    /// ```text
    /// ({T => U}, ...) -> (function({T => U}, T) -> (T?, U), {T => U}, T, ...)
    /// ```
    ///
    /// ...except when the third return type is not any, in which case T is replaced with
    /// *that* parameter so that `ipairs` (which T should be integer) works for any table types.
    GenericPairs,

    /// `table`
    ///
    /// A table mirroring the global environment.
    //
    // XXX not yet implemented
    GlobalEnv,

    /// `function(...) -> (...)`
    ///
    /// Calling this function will alter the global environment in unspecified way,
    /// so it is no longer assumed to be known after the call.
    //
    // XXX not yet implemented
    GlobalEval,

    /// `function(string, ...) -> (...)`
    ///
    /// Calling this function will enable the "module" mode in Lua 5.1.
    //
    // XXX not yet implemented
    // XXX probably requires ModuleEnv tag
    BecomeModule,

    /// `var string`
    ///
    /// Assigning to these variables will update the `require` paths (in the order).
    /// They are distinct to each other, but otherwise have the same syntax to Lua's.
    PackagePath,
    PackageCpath,

    /// `table`
    ///
    /// The metatable for all strings. The method calls for string go through this table,
    /// and any modification to this table is immediately available to subsequent code.
    ///
    /// Note that this table is linked to the current environment only by `--# assume`;
    /// there is no other valid way to get a table with such a type.
    StringMeta,

    /// `function(<class prototype type>?) -> <class prototype type>`
    ///
    /// A function that makes a class prototype. If the argument is given, it should be
    /// another existing prototype that acts as a parent of a newly created prototype.
    ///
    /// The prototype is initially unnamed; the first assignment to a local or global variable
    /// will set its name, and the name cannot be changed thereafter.
    MakeClass,

    /// `<class instance type>`
    ///
    /// This is a type of `self` in the constructor method.
    /// At the end of the constructor, the checker collects the current type of `self` and
    /// uses it as a template for every other usage of given class instance type.
    ///
    /// This is internally created and cannot be constructed in normal ways.
    Constructible,

    /// `function([constructible] <class instance type>, ...) -> any`
    ///
    /// The constructor method is marked specially so that the (likely only) assignment to
    /// that method collects the function signature and create an appropriate `new` method.
    ///
    /// This is internally created and cannot be constructed in normal ways.
    Constructor,

    /// `function() -> any`
    ///
    /// Issues a fresh type variable for each use. The return type is ignored.
    /// This is strictly for testing.
    KailuaGenTvar,

    /// `function(any)`
    ///
    /// Fails when given type is not a type variable (no matter it is bounded or not).
    /// This is used for tests requiring a type variable to be resolved;
    /// By using this function we can ensure that we are indeed testing against a type variable.
    KailuaAssertTvar,
}

impl Tag {
    pub fn from(attr: &Attr, resolv: &mut TypeResolver) -> Result<Option<Tag>> {
        match &attr.name.base[..] {
            b"internal subtype"     => Ok(Some(Tag::_Subtype)),
            b"internal no_subtype"  => Ok(Some(Tag::_NoSubtype)),
            b"internal no_subtype2" => Ok(Some(Tag::_NoSubtype2)),

            b"require"       => Ok(Some(Tag::Require)),
            b"type"          => Ok(Some(Tag::Type)),
            b"assert"        => Ok(Some(Tag::Assert)),
            b"assert_not"    => Ok(Some(Tag::AssertNot)),
            b"assert_type"   => Ok(Some(Tag::AssertType)),
            b"generic_pairs" => Ok(Some(Tag::GenericPairs)),
            b"genv"          => Ok(Some(Tag::GlobalEnv)),
            b"geval"         => Ok(Some(Tag::GlobalEval)),
            b"become_module" => Ok(Some(Tag::BecomeModule)),
            b"package_path"  => Ok(Some(Tag::PackagePath)),
            b"package_cpath" => Ok(Some(Tag::PackageCpath)),
            b"string_meta"   => Ok(Some(Tag::StringMeta)),
            b"make_class"    => Ok(Some(Tag::MakeClass)),

            b"internal kailua_gen_tvar"    => Ok(Some(Tag::KailuaGenTvar)),
            b"internal kailua_assert_tvar" => Ok(Some(Tag::KailuaAssertTvar)),

            _ => {
                resolv.warn(&attr.name, m::UnknownAttrName { name: &attr.name.base }).done()?;
                Ok(None)
            }
        }
    }

    pub fn name(&self) -> &'static str {
        match *self {
            Tag::Require      => "require",
            Tag::Type         => "type",
            Tag::Assert       => "assert",
            Tag::AssertNot    => "assert_not",
            Tag::AssertType   => "assert_type",
            Tag::GenericPairs => "generic_pairs",
            Tag::GlobalEnv    => "genv",
            Tag::GlobalEval   => "geval",
            Tag::BecomeModule => "become_module",
            Tag::PackagePath  => "package_path",
            Tag::PackageCpath => "package_cpath",
            Tag::StringMeta   => "string_meta",
            Tag::MakeClass    => "make_class",

            Tag::_Subtype         => "internal subtype",
            Tag::_NoSubtype       => "internal no_subtype",
            Tag::_NoSubtype2      => "internal no_subtype2",
            Tag::Constructible    => "internal constructible",
            Tag::Constructor      => "internal constructor",
            Tag::KailuaGenTvar    => "internal kailua_gen_tvar",
            Tag::KailuaAssertTvar => "internal kailua_assert_tvar",
        }
    }

    /// Returns true if the effect of this tag is local to the current scope.
    ///
    /// Non-scope-local tags have a limited ability inside non-global scopes.
    //
    // XXX seems that it needs more accurate definition
    pub fn scope_local(&self) -> bool {
        match *self {
            Tag::Type |
            Tag::Assert |
            Tag::AssertNot |
            Tag::AssertType |
            Tag::GenericPairs |
            Tag::MakeClass |
            Tag::Constructible |
            Tag::Constructor |
            Tag::KailuaGenTvar |
            Tag::KailuaAssertTvar => true,
            _ => false,
        }
    }

    /// Returns true if this tag needs the strict subtyping rule.
    ///
    /// For example, it is NOT possible to update `[type] function(any) -> string` with
    /// a plain `function(any) -> string`, it has to be `[type] function(any) -> string`.
    /// Generally the tag working via assignment should have this false.
    pub fn needs_subtype(&self) -> bool {
        match *self {
            // only used for testing
            Tag::_Subtype => true,
            Tag::_NoSubtype => false,
            Tag::_NoSubtype2 => false,

            Tag::PackagePath |
            Tag::PackageCpath |
            Tag::Constructible |
            Tag::Constructor => false,
            _ => true,
        }
    }
}

