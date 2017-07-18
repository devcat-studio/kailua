//! The abstract syntax tree (AST).
//!
//! The basic AST roughly follows Lua's own [syntax description][lua51-syntax].
//! All Kailua-specific variants have names starting with `Kailua`.
//!
//! [lua51-syntax]: https://www.lua.org/manual/5.1/manual.html#8

use std::fmt;
use std::cell::Cell;
use std::collections::HashMap;
use kailua_env::{Span, Spanned, Scope, ScopedId, ScopeMap};

use string::{Str, Name};

// a helper type for printing commas
struct Comma(Cell<bool>);
impl Comma {
    fn new() -> Comma { Comma(Cell::new(true)) }
}
impl fmt::Display for Comma {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0.get() {
            self.0.set(false);
            Ok(())
        } else {
            write!(f, ", ")
        }
    }
}

/// A resolved reference to the name, either local or global.
//
// why don't we use scoped ids everywhere? scoped ids are bound to the scope map,
// and we may have multiple ASTs (thus multiple scope maps) there!
// scoped ids can be paired with the scope map (implicitly), but this only works for local names.
// therefore global names should be stored as their names, even though there are
// also associated (however non-unique) scoped ids for them.
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum NameRef {
    /// A local name, represented by a scoped identifier bound to the scope map.
    /// A (portion of) `Chunk` is required for mapping.
    Local(ScopedId),

    /// A global name, represented by a name.
    Global(Name),
}

/// In the debugging output the name reference is denoted
/// either <code>&lt;<i>id</i>&gt;</code> (local) or <code>`<i>Name</i>`_</code> (global).
///
/// Note that `kailua_test` will automatically convert it to the more readable form
/// for local names: <code>`<i>Name</i>`$<i>scope</i></code>.
impl fmt::Debug for NameRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            NameRef::Local(ref scoped_id) => write!(f, "{:?}", scoped_id),
            NameRef::Global(ref name) => write!(f, "{:?}_", name),
        }
    }
}

/// Similar to `NameRef` but redefines a local name in the current scope.
///
/// This is required for `--# assume`-like statements,
/// where the same local name (if any) can be redefined for the purpose of typing.
/// It is also possible to redefine global as local and vice versa,
/// though local to global would be essentially forbidden by Lua semantics.
#[derive(Clone, PartialEq, Eq)]
pub struct RenameRef {
    pub before: NameRef,
    pub after: NameRef,
}

impl fmt::Debug for RenameRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} => {:?}", self.before, self.after)
    }
}

/// An `[attribute]` syntax in the Kailua types.
#[derive(Clone, PartialEq)]
pub struct Attr {
    pub name: Spanned<Name>,
    pub values: Option<Spanned<Vec<Spanned<AttrValue>>>>,
}

impl fmt::Debug for Attr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref values) = self.values {
            write!(f, "[{:?}(", self.name)?;
            let comma = Comma::new();
            for v in &values.base { write!(f, "{}{:?}", comma, v)?; }
            write!(f, "){:?}]", values.span)
        } else {
            write!(f, "[{:?}]", self.name)
        }
    }
}

/// Any value that can be in the attribute.
#[derive(Clone, PartialEq)]
pub enum AttrValue {
    /// A name, as like `foo` in `[make_class(foo)]`.
    Name(Spanned<Name>),
}

impl fmt::Debug for AttrValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            AttrValue::Name(ref name) => write!(f, "{:?}", name),
        }
    }
}

/// A sequence of items, optionally having a "tail" item for the remainder (e.g. varargs).
#[derive(Clone, PartialEq)]
pub struct Seq<Head, Tail=Head> {
    pub head: Vec<Head>,
    pub tail: Option<Tail>,
}

impl<Head, Tail> Seq<Head, Tail> {
    pub fn empty() -> Seq<Head, Tail> {
        Seq { head: Vec::new(), tail: None }
    }

    pub fn map<U, V, F: FnMut(Head) -> U, G: FnOnce(Tail) -> V>(self, f: F, g: G) -> Seq<U, V> {
        Seq { head: self.head.into_iter().map(f).collect(), tail: self.tail.map(g) }
    }
}

impl<Head: fmt::Debug, Tail: fmt::Debug> fmt::Debug for Seq<Head, Tail> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !f.sign_minus() { write!(f, "[")?; }
        let comma = Comma::new();
        for e in &self.head { write!(f, "{}{:?}", comma, e)?; }
        if let Some(ref e) = self.tail { write!(f, "{}{:?}...", comma, e)?; }
        if !f.sign_minus() { write!(f, "]")?; }
        Ok(())
    }
}

/// A left-hand side of the assignment.
#[derive(Clone, PartialEq)]
pub enum Var {
    /// `name`.
    Name(Spanned<NameRef>),

    /// `exp[exp]`.
    Index(Spanned<Exp>, Spanned<Exp>),

    /// `exp.name`. Distinguished from `Var::Index` for the purpose of IDE support.
    IndexName(Spanned<Exp>, Spanned<Name>),
}

impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Var::Name(ref id) => write!(f, "{:?}", id),
            Var::Index(ref lhs, ref rhs) => write!(f, "{:?}[{:?}]", lhs, rhs),
            Var::IndexName(ref lhs, ref name) => write!(f, "{:?}.{:?}", lhs, name),
        }
    }
}

/// Any node that can be optionally annotated with a Kailua type.
#[derive(Clone, PartialEq)]
pub struct TypeSpec<T> {
    /// The base node.
    pub base: T,

    /// An extended modifier (e.g. `const` or `module`) for the type.
    ///
    /// Defaults to `MM::None` when the annotation is absent.
    /// This is distinct from the type because `--: const` etc. are allowed.
    pub modf: MM,

    /// The type, if explicitly given.
    pub kind: Option<Spanned<Kind>>,
}

impl<T> TypeSpec<T> {
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> TypeSpec<U> {
        TypeSpec { base: f(self.base), modf: self.modf, kind: self.kind }
    }
}

impl<T: fmt::Debug> fmt::Debug for TypeSpec<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.base)?;
        match (self.modf, &self.kind) {
            (MM::None, &None) => Ok(()),
            (modf, &None) => write!(f, ": {:?}", modf),
            (modf, &Some(ref kind)) => write!(f, ": {:?} {:?}", modf, kind),
        }
    }
}

/// A return type of a function in the Kailua type.
#[derive(Clone, PartialEq)]
pub enum Returns {
    /// `--> type` or `--> (type, type...)`.
    Seq(Seq<Spanned<Kind>>),

    /// `--> !`.
    ///
    /// The span points to a token `!`.
    Never(Span),
}

impl fmt::Debug for Returns {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Returns::Seq(ref seq) => {
                if seq.head.len() == 1 && seq.tail.is_none() {
                    write!(f, "{:?}", seq.head[0])
                } else {
                    write!(f, "({:-?})", seq)
                }
            },
            Returns::Never(span) => {
                write!(f, "!")?;
                fmt::Debug::fmt(&span, f)
            },
        }
    }
}

/// A Kailua type for variadic arguments.
#[derive(Clone, PartialEq)]
pub struct Varargs {
    /// A type of each variadic argument. Inferred if missing.
    pub kind: Option<Spanned<Kind>>,

    /// A scoped identifier for `arg` (an implicit variable for varargs in Lua 5.0).
    /// This is only used in Lua 5.1 for the compatibility.
    pub legacy_arg: Option<Spanned<ScopedId>>,
}

impl fmt::Debug for Varargs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "...")?;
        if let Some(ref scoped_id) = self.legacy_arg {
            write!(f, "(arg={:?})", scoped_id)?;
        }
        write!(f, ": ")?;
        if let Some(ref kind) = self.kind {
            write!(f, "{:?}", *kind)?;
        } else {
            write!(f, "_")?;
        }
        Ok(())
    }
}

/// A Kailua-specific function signature.
#[derive(Clone, PartialEq)]
pub struct Sig {
    /// A list of attributes.
    pub attrs: Vec<Spanned<Attr>>,

    /// A list of arguments (resolved to scoped identifiers) and associated types if any.
    pub args: Spanned<Seq<TypeSpec<Spanned<ScopedId>>, Varargs>>,

    /// A list of return types, if explicitly given. Inferred if missing.
    pub returns: Option<Returns>,
}

impl fmt::Debug for Sig {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for attr in &self.attrs {
            write!(f, "{:?} ", attr)?;
        }
        write!(f, "[")?;
        let comma = Comma::new();
        for namespec in &self.args.head {
            write!(f, "{}{:?}", comma, namespec)?;
        }
        if let Some(ref varargs) = self.args.tail {
            write!(f, "{}{:?}", comma, varargs)?;
        }
        write!(f, "]")?;
        match self.returns {
            Some(Returns::Seq(Seq { ref head, tail: None })) if head.is_empty() => Ok(()),
            Some(Returns::Seq(Seq { ref head, tail: None })) if head.len() == 1 => {
                write!(f, " --> {:?}", head[0])
            },
            Some(Returns::Seq(ref seq)) => {
                write!(f, " --> {:?}", seq)
            },
            Some(Returns::Never(span)) => {
                write!(f, " --> !")?;
                fmt::Debug::fmt(&span, f)
            },
            None => {
                write!(f, " --> _")
            },
        }
    }
}

/// A table constructor.
#[derive(Clone, PartialEq)]
pub struct Table {
    /// An ordered list of items, which may or may not have an index.
    pub items: Vec<(Option<Spanned<Exp>>, Spanned<Exp>)>,
}

impl fmt::Debug for Table {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;
        let comma = Comma::new();
        for &(ref k, ref v) in &self.items {
            write!(f, "{}", comma)?;
            if let Some(ref k) = *k { write!(f, "[{:?}] = ", k)?; }
            write!(f, "{:?}", v)?;
        }
        write!(f, "}}")
    }
}

/// Arguments to a function call.
#[derive(Clone, PartialEq)]
pub enum Args {
    /// `f(a, b, c)` (span doesn't include `f`).
    List(Vec<Spanned<Exp>>),

    /// `f"string"` or `f[[string]]` (span doesn't include `f`).
    /// Distinguished from `Args::List` for the purpose of IDE support.
    Str(Str),

    /// `f{1, 2, 3}` (span doesn't include `f`).
    /// Distinguished from `Args::List` for the purpose of IDE support.
    Table(Table),
}

impl fmt::Debug for Args {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Args::List(ref args) => {
                write!(f, "(")?;
                let comma = Comma::new();
                for arg in args { write!(f, "{}{:?}", comma, arg)?; }
                write!(f, ")")
            },
            Args::Str(ref s) => write!(f, "{:?}", s),
            Args::Table(ref tab) => write!(f, "{:?}", tab),
        }
    }
}

/// An expression.
#[derive(Clone, PartialEq)]
pub enum Ex {
    /// A dummy node resulting from a parsing error.
    ///
    /// Technically the type checker "evaluates" this node into an error type.
    Oops,

    /// `nil`.
    Nil,

    /// `false`.
    False,

    /// `true`.
    True,

    /// A number literal.
    Num(f64),

    /// A string literal.
    ///
    /// A difference between `"string"` and `[[string]]` is not recorded.
    Str(Str),

    /// `...`.
    Varargs,

    /// A function literal (`function() ... end`).
    ///
    /// An associated scope is for the function body.
    Func(Sig, Scope, Spanned<Block>),

    /// `{1, 2, 3}`.
    Table(Table),

    /// A variable reference.
    Var(Spanned<NameRef>),

    /// Another expression.
    ///
    /// This is primarily used to keep parentheses,
    /// so that `(3)` is composed of two expression nodes with two different spans.
    Exp(Spanned<Exp>),

    /// `f()` or `f.g()` (the latter will have an `Ex::IndexName` node).
    FuncCall(Spanned<Exp>, Spanned<Args>),

    /// `f:g()` or `f.g:h()` (the latter will have an `Ex::IndexName` node).
    ///
    /// A span for the entire `f:g` or `f.g:h` part is separately recorded for autocompletion.
    MethodCall(Spanned<(Spanned<Exp>, Spanned<Name>)>, Spanned<Args>),

    /// `exp[exp]`.
    Index(Spanned<Exp>, Spanned<Exp>),

    /// `exp.name`. Distinguished from `Ex::Index` for the purpose of IDE support.
    IndexName(Spanned<Exp>, Spanned<Name>),

    /// `unop exp`.
    Un(Spanned<UnOp>, Spanned<Exp>),

    /// `exp binop exp`.
    Bin(Spanned<Exp>, Spanned<BinOp>, Spanned<Exp>),
}

impl fmt::Debug for Ex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Ex::Oops => write!(f, "Oops"),
            Ex::Nil => write!(f, "nil"),
            Ex::False => write!(f, "false"),
            Ex::True => write!(f, "true"),
            Ex::Num(v) => write!(f, "{:?}", v),
            Ex::Str(ref s) => write!(f, "{:?}", *s),
            Ex::Varargs => write!(f, "..."),
            Ex::Func(ref p, bs, ref b) => write!(f, "Func({:?}, {:?}{:?})", *p, bs, *b),
            Ex::Table(ref tab) => write!(f, "{:?}", tab),

            Ex::Var(ref id) => write!(f, "{:?}", id),
            Ex::Exp(ref e) => write!(f, "({:?})", e),
            Ex::FuncCall(ref e, ref args) => write!(f, "{:?}{:?}", e, args),
            Ex::MethodCall(Spanned { base: (ref e, ref n), span }, ref args) =>
                write!(f, "({:?}:{:?}){:?}{:?}", e, n, span, args),
            Ex::Index(ref e, ref i) => write!(f, "{:?}[{:?}]", *e, *i),
            Ex::IndexName(ref e, ref i) => write!(f, "{:?}.{:?}", e, i),
            Ex::Un(op, ref e) => write!(f, "({} {:?})", op.symbol(), *e),
            Ex::Bin(ref l, op, ref r) => write!(f, "({:?} {} {:?})", *l, op.symbol(), *r),
        }
    }
}

/// A boxed expression node.
pub type Exp = Box<Ex>;

/// A unary operator.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnOp {
    /// `-`.
    Neg,
    /// `not`.
    Not,
    /// `#`.
    Len,
}

impl UnOp {
    pub fn symbol(&self) -> &'static str {
        match *self {
            UnOp::Neg => "-",
            UnOp::Not => "not",
            UnOp::Len => "#",
        }
    }
}

/// A bunary operator.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    /// `+`.
    Add,
    /// `-`.
    Sub,
    /// `*`.
    Mul,
    /// `/`.
    Div,
    /// `^`.
    Pow,
    /// `%`.
    Mod,
    /// `..`.
    Cat,
    /// `<`.
    Lt,
    /// `<=`.
    Le,
    /// `>`.
    Gt,
    /// `>=`.
    Ge,
    /// `==`.
    Eq,
    /// `~=`.
    Ne,
    /// `and`.
    And,
    /// `or`.
    Or,
}

impl BinOp {
    pub fn symbol(&self) -> &'static str {
        match *self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Pow => "^",
            BinOp::Mod => "%",
            BinOp::Cat => "..",
            BinOp::Lt  => "<",
            BinOp::Le  => "<=",
            BinOp::Gt  => ">",
            BinOp::Ge  => ">=",
            BinOp::Eq  => "==",
            BinOp::Ne  => "~=",
            BinOp::And => "and",
            BinOp::Or  => "or",
        }
    }
}

/// A scoped identifier for the implicit `self` parameter.
#[derive(Clone, PartialEq, Eq)]
pub struct SelfParam(pub ScopedId);

impl fmt::Debug for SelfParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "self={:?}", self.0)
    }
}

/// A scope of the named Kailua type (from `--# type`).
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TypeScope {
    /// A type name is local to the current (Lua) scope.
    Local,

    /// A type name is globally usable.
    Global,

    /// A type name is local to the current (Lua) scope,
    /// but `require`ing the current module will copy it to the callee's local scope.
    Exported,
}

/// A statement.
///
/// Many statement nodes have associated scopes.
/// The scope, if present, is associated to the following node,
/// except when the scope is the last field,
/// in which case it is a "sibling" scope applied to the remaining statements in the block.
/// Note that not all statements introducing scopes have them,
/// as local names will have associated sibling scopes anyway. 
#[derive(Clone, PartialEq)]
pub enum St {
    /// A dummy node resulting from a parsing error.
    ///
    /// Technically the type checker "evaluates" this node by ignoring it.
    Oops,

    /// An expression used as a statement.
    ///
    /// The actual Lua syntax only allows for a function call here,
    /// but the node itself allows for any expression to account for the partially valid code.
    Void(Spanned<Exp>),

    /// `var, exp.name, exp[exp] = exp, exp... --: type, type...`.
    ///
    /// The right-hand side can be missing if l-values are not followed by `=`.
    /// The parser only tries to recognize l-values when there is a following comma,
    /// so `3` or `f` is a (invalid) `St::Void` while `a,` or `a,b` is a (invalid) `St::Assign`.
    Assign(Spanned<Vec<TypeSpec<Spanned<Var>>>>, Option<Spanned<Vec<Spanned<Exp>>>>),

    /// `do ... end`.
    Do(Spanned<Block>),

    /// `while exp do ... end`.
    While(Spanned<Exp>, Spanned<Block>),

    /// `repeat ... until exp`.
    Repeat(Spanned<Block>, Spanned<Exp>),

    /// `if exp then ... else if exp then ... else ... end`.
    If(Vec<Spanned<(Spanned<Exp>, Spanned<Block>)>>, Option<Spanned<Block>>),

    /// `for name = exp, exp[, exp] do ... end`.
    For(Spanned<ScopedId>, Spanned<Exp>, Spanned<Exp>, Option<Spanned<Exp>>, Scope, Spanned<Block>),

    /// `for name, ... in exp, ... do ... end`.
    ForIn(Spanned<Vec<Spanned<ScopedId>>>, Spanned<Vec<Spanned<Exp>>>, Scope, Spanned<Block>),

    /// `[local] function name(...) do ... end`.
    FuncDecl(Spanned<NameRef>, Sig, Scope, Spanned<Block>, Option<Scope>),

    /// `function name.field...field.method(...) do ... end` (when `SelfParam` is missing) or
    /// `function name.field...field:method(...) do ... end` (when `SelfParam` is given).
    MethodDecl(Spanned<(Spanned<NameRef>, Vec<Spanned<Name>>)>,
               Option<Spanned<SelfParam>>, Sig, Scope, Spanned<Block>),

    /// `local name, ... = exp, ...`.
    Local(Spanned<Vec<TypeSpec<Spanned<ScopedId>>>>, Spanned<Vec<Spanned<Exp>>>, Scope),

    /// `return exp, ...`.
    Return(Spanned<Vec<Spanned<Exp>>>),

    /// `break`.
    Break,

    /// `--# open name`.
    KailuaOpen(Spanned<Name>),

    /// `--# type [scope] name = type`.
    KailuaType(TypeScope, Spanned<Name>, Spanned<Kind>),

    /// `--# assume [global] name: type`.
    ///
    /// The sibling scope only exists when the statement is redefining a local name.
    KailuaAssume(Spanned<RenameRef>, M, Spanned<Kind>, Option<Scope>),

    /// `--# assume [static] name.field.field: type`.
    ///
    /// The first `bool` is true when `static` is present.
    KailuaAssumeField(bool /*static*/, Spanned<(Spanned<NameRef>, Vec<Spanned<Name>>)>,
                      M, Spanned<Kind>),

    /// `--# assume name.field.field: method(...) --> ...`.
    ///
    /// This is distinct from `St::KailuaAssumeField` because it is not possible to
    /// desugar it without knowing the type of `self`.
    KailuaAssumeMethod(Spanned<(Spanned<NameRef>, Vec<Spanned<Name>>)>, M, Spanned<FuncKind>),
}

/// In the debugging output scopes are printed in two ways:
///
/// * Scopes associated to a nested block are printed *before* that block:
///   <code>$<i>scope</i>[...]</code>.
///
/// * Scopes associated to the remaining statements in the current block ("sibling scope")
///   are printed *after* the closing parenthesis: <code><i>Node</i>(...)$<i>scope</i></code>.
impl fmt::Debug for St {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            St::Oops => write!(f, "Oops"),

            St::Void(ref e) => write!(f, "Void({:?})", e),
            St::Assign(ref l, Some(ref r)) => write!(f, "Assign({:?}, {:?})", l, r),
            St::Assign(ref l, None) => write!(f, "Assign({:?}, _)", l),
            St::Do(ref b) => write!(f, "Do({:?})", b),
            St::While(ref e, ref b) => write!(f, "While({:?}, {:?})", e, b),
            St::Repeat(ref b, ref e) => write!(f, "Repeat({:?}, {:?})", b, e),
            St::If(ref cases, ref else_) => {
                write!(f, "If(")?;
                let comma = Comma::new();
                for &Spanned { base: (ref e, ref b), span } in cases {
                    write!(f, "{}({:?} => {:?}){:?}", comma, e, b, span)?;
                }
                if let Some(ref b) = *else_ {
                    write!(f, "{}{:?}", comma, b)?;
                }
                write!(f, ")")
            },
            St::For(ref i, ref start, ref end, ref step, bs, ref b) =>
                write!(f, "For({:?}, {:?}, {:?}, {:?}, {:?}{:?})", i, start, end, step, bs, b),
            St::ForIn(ref ii, ref ee, bs, ref b) =>
                write!(f, "ForIn({:?}, {:?}, {:?}{:?})", ii, ee, bs, b),
            St::FuncDecl(ref i, ref sig, bs, ref b, is) => {
                write!(f, "FuncDecl({:?}, {:?}, {:?}{:?})", i, sig, bs, b)?;
                if let Some(is) = is { write!(f, "{:?}", is)?; }
                Ok(())
            },
            St::MethodDecl(Spanned { base: (ref i, ref ii), span },
                           ref selfparam, ref sig, bs, ref b) => {
                write!(f, "MethodDecl(({:?}", i)?;
                for i in ii { write!(f, ".{:?}", i)?; }
                write!(f, "){:?}, {:?}, {:?}, {:?}{:?})", span, selfparam, sig, bs, b)
            },
            St::Local(ref ii, ref ee, is) => write!(f, "Local({:?}, {:?}){:?}", ii, ee, is),
            St::Return(ref ee) => write!(f, "Return({:?})", ee),
            St::Break => write!(f, "Break"),

            St::KailuaOpen(ref lib) => write!(f, "KailuaOpen({:?})", lib),
            St::KailuaType(scope, ref t, ref k) =>
                write!(f, "KailuaType({:?}, {:?}, {:?})", scope, t, k),
            St::KailuaAssume(ref i, m, ref k, is) => {
                write!(f, "KailuaAssume({:?}, {:?}, {:?})", i, m, k)?;
                if let Some(is) = is { write!(f, "{:?}", is)?; }
                Ok(())
            },
            St::KailuaAssumeField(static_, Spanned { base: (ref i, ref ii), span }, m, ref k) => {
                write!(f, "KailuaAssumeField({}, ({:?}", static_, i)?;
                for i in ii { write!(f, ".{:?}", i)?; }
                write!(f, "){:?}, {:?}, {:?})", span, m, k)
            },
            St::KailuaAssumeMethod(Spanned { base: (ref i, ref ii), span }, m, ref fk) => {
                write!(f, "KailuaAssumeMethod(({:?}", i)?;
                for i in ii { write!(f, ".{:?}", i)?; }
                write!(f, "){:?}, {:?}, {:?})", span, m, fk)
            },
        }
    }
}

/// A boxed statement node.
pub type Stmt = Box<St>;

/// A sequence of spanned statements.
pub type Block = Vec<Spanned<Stmt>>;

/// A type modifier for Kailua.
///
/// A modifier primarily determines whether a field or variable can be modified or not.
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum M {
    /// Mutable (default, no separate keyword exists).
    None,

    /// Immutable (`const`).
    Const,
}

impl fmt::Debug for M {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            M::None => write!(f, "_"),
            M::Const => write!(f, "Const"),
        }
    }
}

/// An extended type modifier for Kailua.
///
/// This is a superset of `M` used only for variables.
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum MM {
    /// Mutable (default, no separate keyword exists).
    None,

    /// Immutable (`const`).
    Const,

    /// Mutable, but registers for the delayed type checking (`module`).
    ///
    /// Indexing such variables will require an explicit type,
    /// and assignments to resulting fields are stored to the current scope (and not checked).
    /// Stored nodes are checked at the end of the scope where variables were registered.
    Module,
}

impl fmt::Debug for MM {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            MM::None => write!(f, "_"),
            MM::Const => write!(f, "Const"),
            MM::Module => write!(f, "Module"),
        }
    }
}

/// A type with a modifier, used for nested field types ("slot types").
#[derive(Clone, PartialEq)]
pub struct SlotKind {
    /// A modifier.
    pub modf: M,

    /// A type.
    pub kind: Spanned<Kind>,
}

impl fmt::Debug for SlotKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {:?}", self.modf, self.kind)
    }
}

/// A function type for Kailua.
#[derive(Clone, PartialEq)]
pub struct FuncKind {
    /// A list of argument types with optional names.
    ///
    /// The name is purely for description and has no effect in the type.
    /// The parser will issue an error if some arguments have names and others don't
    /// or names are not distinct, but the type checker should not care.
    pub args: Seq<(Option<Spanned<Name>>, Spanned<Kind>), Spanned<Kind>>,

    /// A return type.
    pub returns: Returns,
}

impl fmt::Debug for FuncKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        let comma = Comma::new();
        for &(ref name, ref arg) in &self.args.head {
            write!(f, "{}", comma)?;
            if let &Some(ref name) = name {
                write!(f, "{:?}: ", name)?;
            }
            write!(f, "{:?}", arg)?;
        }
        if let Some(ref varargs) = self.args.tail {
            write!(f, "{}{:?}...", comma, varargs)?;
        }
        write!(f, ") --> {:?}", self.returns)
    }
}

/// A Kailua type.
///
/// The syntax-level type is actually termed a "kind",
/// because this and the actual type are frequently used altogether,
/// and it is not really a "type" but a specification that leads to a type.
#[derive(Clone, PartialEq)]
pub enum K {
    /// An error type resulting from a parsing error.
    Oops,

    /// `WHATEVER`.
    Dynamic,

    /// `any`.
    Any,

    /// `nil`.
    Nil,

    /// `boolean` or `bool`.
    Boolean,

    /// A boolean literal type (`true` or `false`).
    BooleanLit(bool),

    /// `number`.
    Number,

    /// `integer` or `int`.
    Integer,

    /// An integral literal type.
    IntegerLit(i32),

    /// `string`.
    String,

    /// A string literal type.
    StringLit(Str),

    /// `table`.
    Table,

    /// `{}`.
    EmptyTable,

    /// `{a: T, b: U}` (inextensible) or `{a: T, b: U, ...}` (extensible).
    ///
    /// The second `bool` is true if the record type is extensible.
    Record(Vec<(Spanned<Str>, Spanned<SlotKind>)>, bool /*extensible*/),

    /// `{T, U}`.
    ///
    /// The tuple type is never extensible,
    /// though the checker can internally have extensible tuples.
    Tuple(Vec<Spanned<SlotKind>>),

    /// `vector<T>`. (The name "array" is a historic artifact.)
    Array(Spanned<SlotKind>),

    /// `map<T, U>`.
    Map(Spanned<Kind>, Spanned<SlotKind>),

    /// `function`.
    Function,

    /// `function(...) -> ...`.
    Func(Spanned<FuncKind>),

    /// `thread`.
    Thread,

    /// `userdata`.
    UserData,

    /// A named type.
    Named(Spanned<Name>),

    /// `T?`.
    ///
    /// The checker distinguishes a plain type and a "nilable" type,
    /// but in the syntax `?` is considered a (non-associative) type operator.
    WithNil(Spanned<Kind>),

    /// `T!`.
    ///
    /// The checker distinguishes a plain type and a "nilable" type,
    /// but in the syntax `!` is considered a (non-associative) type operator.
    WithoutNil(Spanned<Kind>),

    /// `T | U | ...`.
    Union(Vec<Spanned<Kind>>),

    /// `[attribute] T`.
    Attr(Spanned<Kind>, Spanned<Attr>),

    /// `error "message"`. Currently parsed but not checked.
    Error(Option<Spanned<Str>>),
}

impl fmt::Debug for K {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            K::Oops               => write!(f, "Oops"),
            K::Dynamic            => write!(f, "Dynamic"),
            K::Any                => write!(f, "Any"),
            K::Nil                => write!(f, "Nil"),
            K::Boolean            => write!(f, "Boolean"),
            K::BooleanLit(true)   => write!(f, "True"),
            K::BooleanLit(false)  => write!(f, "False"),
            K::Number             => write!(f, "Number"),
            K::Integer            => write!(f, "Integer"),
            K::IntegerLit(v)      => write!(f, "Integer({})", v),
            K::String             => write!(f, "String"),
            K::StringLit(ref s)   => write!(f, "String({:?})", *s),
            K::Table              => write!(f, "Table"),
            K::EmptyTable         => write!(f, "EmptyTable"),
            K::Array(ref v)       => write!(f, "Array({:?})", *v),
            K::Map(ref k, ref v)  => write!(f, "Map({:?}, {:?})", *k, *v),
            K::Function           => write!(f, "Function"),
            K::Named(ref name)    => write!(f, "{:?}", *name),
            K::WithNil(ref k)     => write!(f, "{:?}?", *k),
            K::WithoutNil(ref k)  => write!(f, "{:?}!", *k),
            K::Thread             => write!(f, "Thread"),
            K::UserData           => write!(f, "UserData"),
            K::Error(None)        => write!(f, "Error"),
            K::Error(Some(ref s)) => write!(f, "Error({:?})", *s),

            K::Record(ref fields, extensible) => {
                write!(f, "Record([")?;
                let comma = Comma::new();
                for &(ref name, ref value) in fields {
                    write!(f, "{}{:?}: {:?}", comma, name, value)?;
                }
                if extensible {
                    write!(f, "{}...", comma)?;
                }
                write!(f, "])")?;
                Ok(())
            },
            K::Tuple(ref fields) => write!(f, "Tuple({:?})", *fields),
            K::Func(ref func) => write!(f, "Func({:?})", *func),
            K::Union(ref kinds) => write!(f, "Union({:?})", *kinds),
            K::Attr(ref k, ref a) => write!(f, "{:?} {:?}", a, k),
        }
    }
}

/// A boxed Kailua type node.
pub type Kind = Box<K>;

/// The category of local names.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LocalNameKind {
    /// Explicitly defined.
    User,

    /// Implicitly created from method declarations.
    ImplicitSelf,

    /// Implicitly created from variadic arguments (Lua 5.0 compatibility).
    ImplicitLegacyArg,

    /// Created by `--# assume` to a local name with given scoped identifier,
    /// which itself is defined otherwise.
    AssumedToLocal(ScopedId),

    /// Created by `--# assume` to a global name with the same name.
    AssumedToGlobal,
}

/// Resolved information about each local name.
#[derive(Clone, Debug, PartialEq)]
pub struct LocalName {
    /// The span of the first occurrence of the name (i.e. from its definition).
    pub def_span: Span,

    /// A category of the local name.
    pub kind: LocalNameKind,
}

/// An auxiliary information generated for each token.
///
/// Each information is assumed to be interpreted with the corresponding token.
#[derive(Clone, Debug, PartialEq)]
pub enum TokenAux {
    /// No additional information.
    None,

    /// The token is a `Tok::Name` referring to a local variable with given scoped identifier.
    LocalVarName(ScopedId),

    /// The token is a `Tok::Name` referring to a global variable with the same name.
    GlobalVarName,
}

/// The parsed chunk, representing a single source file with associated side informations.
#[derive(Clone)]
pub struct Chunk {
    /// The top-level block.
    pub block: Spanned<Block>,

    /// An associated scope map for local names.
    pub map: ScopeMap<Name>,

    /// A map from globally assigned names to spans to their first occurrences.
    ///
    /// Global names that has been used are not recorded.
    pub global_scope: HashMap<Name, Span>,

    /// A map from local names (in the form of scoped identifiers) to the resolved information.
    pub local_names: HashMap<ScopedId, LocalName>,

    /// Auxiliary informations for each input token (including `Tok::EOF`), in the order.
    pub token_aux: Vec<TokenAux>,
}

