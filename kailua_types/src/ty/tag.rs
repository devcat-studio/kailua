use std::fmt;
use kailua_env::Spanned;
use kailua_diag::{Result, Reporter};
use kailua_syntax::ast::{Attr, AttrValue};
use super::{Display, DisplayState, TypeResolver, ClassSystemId};
use message as m;

/// A type tag for giving a type special meanings.
///
/// Generally a type with a tag is equal to or a subtype of a specific form of types;
/// it is safe to put a tag to an non-conforming type, but that won't work well in general.
#[derive(Copy, Clone, PartialEq, Eq)]
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
    ///
    /// There may be additional behaviors depending on the class system used.
    MakeClass(ClassSystemId),

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
        let no_values = |resolv: &mut TypeResolver, tag| {
            if let Some(ref values) = attr.values {
                resolv.error(values, m::AttrCannotHaveAnyValues { name: &attr.name }).done()?;
            }
            Ok(Some(tag))
        };

        let values = |resolv: &mut TypeResolver, count| {
            if let Some(ref values) = attr.values {
                if values.len() != count {
                    resolv.error(values, m::AttrRequiresFixedNumOfValues { name: &attr.name,
                                                                           count: count })
                          .done()?;
                }
                Ok(&values[..])
            } else {
                // since we have no usable values span, use the name span instead
                resolv.error(&attr.name, m::AttrRequiresFixedNumOfValues { name: &attr.name,
                                                                           count: count })
                      .done()?;
                const EMPTY: &'static [Spanned<AttrValue>] = &[];
                Ok(EMPTY)
            }
        };

        match &attr.name.base[..] {
            b"internal subtype"     => no_values(resolv, Tag::_Subtype),
            b"internal no_subtype"  => no_values(resolv, Tag::_NoSubtype),
            b"internal no_subtype2" => no_values(resolv, Tag::_NoSubtype2),

            b"require"       => no_values(resolv, Tag::Require),
            b"type"          => no_values(resolv, Tag::Type),
            b"assert"        => no_values(resolv, Tag::Assert),
            b"assert_not"    => no_values(resolv, Tag::AssertNot),
            b"assert_type"   => no_values(resolv, Tag::AssertType),
            b"generic_pairs" => no_values(resolv, Tag::GenericPairs),
            b"genv"          => no_values(resolv, Tag::GlobalEnv),
            b"geval"         => no_values(resolv, Tag::GlobalEval),
            b"become_module" => no_values(resolv, Tag::BecomeModule),
            b"package_path"  => no_values(resolv, Tag::PackagePath),
            b"package_cpath" => no_values(resolv, Tag::PackageCpath),
            b"string_meta"   => no_values(resolv, Tag::StringMeta),

            b"make_class" => {
                let values = values(resolv, 1)?;
                if let Some(&AttrValue::Name(ref system)) = values.get(0).map(|v| &v.base) {
                    if let Some(system) = resolv.class_system_from_name(system)? {
                        return Ok(Some(Tag::MakeClass(system)));
                    }
                }
                Ok(None)
            },

            b"internal kailua_gen_tvar"    => no_values(resolv, Tag::KailuaGenTvar),
            b"internal kailua_assert_tvar" => no_values(resolv, Tag::KailuaAssertTvar),

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
            Tag::MakeClass(_) => "make_class",

            Tag::_Subtype         => "internal subtype",
            Tag::_NoSubtype       => "internal no_subtype",
            Tag::_NoSubtype2      => "internal no_subtype2",
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
            Tag::MakeClass(_) |
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
            Tag::PackageCpath => false,
            _ => true,
        }
    }
}

impl fmt::Debug for Tag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name())?;

        match *self {
            Tag::MakeClass(csid) => {
                write!(f, "({:?})", csid)?;
            }
            _ => {}
        }

        Ok(())
    }
}

impl Display for Tag {
    fn fmt_displayed(&self, f: &mut fmt::Formatter, st: &DisplayState) -> fmt::Result {
        write!(f, "{}", self.name())?;

        match *self {
            Tag::MakeClass(csid) => {
                write!(f, "(")?;
                st.context.fmt_class_system_name(csid, f, st)?;
                write!(f, ")")?;
            }
            _ => {}
        }

        Ok(())
    }
}

