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

// why don't we use scoped ids everywhere? scoped ids are bound to the scope map,
// and we may have multiple ASTs (thus multiple scope maps) there!
// scoped ids can be paired with the scope map (implicitly), but this only works for local names.
// therefore global names should be stored as their names, even though there are
// also associated (however non-unique) scoped ids for them.
#[derive(Clone, PartialEq, Eq, Hash)]
pub enum NameRef {
    Local(ScopedId),
    Global(Name),
}

impl fmt::Debug for NameRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            NameRef::Local(ref scoped_id) => write!(f, "{:?}", scoped_id),
            NameRef::Global(ref name) => write!(f, "{:?}_", name),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Attr {
    pub name: Spanned<Name>,
}

impl fmt::Debug for Attr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{:?}]", self.name)
    }
}

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

#[derive(Clone, PartialEq)]
pub enum Var {
    Name(Spanned<NameRef>),
    Index(Spanned<Exp>, Spanned<Exp>),
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

#[derive(Clone, PartialEq)]
pub struct TypeSpec<T> {
    pub base: T,
    pub modf: MM, // allows for modules
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

#[derive(Clone, PartialEq)]
pub enum Returns {
    Seq(Seq<Spanned<Kind>>),
    Never(Span), // a span for `!`
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

#[derive(Clone, PartialEq)]
pub struct Varargs {
    pub kind: Option<Spanned<Kind>>,
    // if Lua 5.0 compat is enabled, a scoped id for `arg` (takes precedence over normal args!)
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

#[derive(Clone, PartialEq)]
pub struct Sig {
    pub attrs: Vec<Spanned<Attr>>,
    pub args: Spanned<Seq<TypeSpec<Spanned<ScopedId>>, Varargs>>, // may have to be inferred
    pub returns: Option<Returns>, // may have to be inferred
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

#[derive(Clone, PartialEq)]
pub enum Args {
    List(Vec<Spanned<Exp>>), // f(...)
    Str(Str), // f"string", f[[string]]
    Table(Vec<(Option<Spanned<Exp>>, Spanned<Exp>)>), // f{1, 2, 3}
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
            Args::Table(ref fs) => {
                write!(f, "{{")?;
                let comma = Comma::new();
                for &(ref k, ref v) in fs {
                    write!(f, "{}", comma)?;
                    if let Some(ref k) = *k { write!(f, "[{:?}] = ", k)?; }
                    write!(f, "{:?}", v)?;
                }
                write!(f, "}}")
            },
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Ex {
    Oops,

    // literals
    Nil,
    False,
    True,
    Num(f64),
    Str(Str),
    Varargs,
    Func(Sig, Scope, Spanned<Block>),
    Table(Vec<(Option<Spanned<Exp>>, Spanned<Exp>)>),

    // expressions
    Var(Spanned<NameRef>),
    Exp(Spanned<Exp>), // mostly for parentheses
    FuncCall(Spanned<Exp>, Spanned<Args>),
    MethodCall(Spanned<(Spanned<Exp>, Spanned<Name>)>, Spanned<Args>),
    Index(Spanned<Exp>, Spanned<Exp>),
    IndexName(Spanned<Exp>, Spanned<Name>),
    Un(Spanned<UnOp>, Spanned<Exp>),
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
            Ex::Table(ref fs) => {
                write!(f, "{{")?;
                let comma = Comma::new();
                for &(ref k, ref v) in fs {
                    write!(f, "{}", comma)?;
                    if let Some(ref k) = *k { write!(f, "[{:?}] = ", k)?; }
                    write!(f, "{:?}", v)?;
                }
                write!(f, "}}")
            },

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

pub type Exp = Box<Ex>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    Cat,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    And,
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

#[derive(Clone, PartialEq, Eq)]
pub struct SelfParam(pub ScopedId);

impl fmt::Debug for SelfParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "self={:?}", self.0)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TypeScope {
    Local,
    Global,
    Exported,
}

#[derive(Clone, PartialEq)]
pub enum St {
    Oops,

    // every scope is associated to the following node, except when the scope is the last field,
    // in which case it is a sibling scope applied to the following stmts.

    Void(Spanned<Exp>), // technically not every Exp is valid here, but for simplicity.
    Assign(Spanned<Vec<TypeSpec<Spanned<Var>>>>, Option<Spanned<Vec<Spanned<Exp>>>>),
    Do(Spanned<Block>),
    While(Spanned<Exp>, Spanned<Block>),
    Repeat(Spanned<Block>, Spanned<Exp>),
    If(Vec<Spanned<(Spanned<Exp>, Spanned<Block>)>>, Option<Spanned<Block>>),
    For(Spanned<ScopedId>, Spanned<Exp>, Spanned<Exp>, Option<Spanned<Exp>>, Scope, Spanned<Block>),
    ForIn(Spanned<Vec<Spanned<ScopedId>>>, Spanned<Vec<Spanned<Exp>>>, Scope, Spanned<Block>),
    FuncDecl(Spanned<NameRef>, Sig, Scope, Spanned<Block>, Option<Scope>),
    MethodDecl(Spanned<(Spanned<NameRef>, Vec<Spanned<Name>>)>,
               Option<Spanned<SelfParam>>, Sig, Scope, Spanned<Block>),
    Local(Spanned<Vec<TypeSpec<Spanned<ScopedId>>>>, Spanned<Vec<Spanned<Exp>>>, Scope),
    Return(Spanned<Vec<Spanned<Exp>>>),
    Break,

    // Kailua extensions
    KailuaOpen(Spanned<Name>),
    KailuaType(TypeScope, Spanned<Name>, Spanned<Kind>),
    KailuaAssume(NameRef, // a final name reference to be created
                 Spanned<NameRef>, // a name being updated
                 M, Spanned<Kind>, Option<Scope>),
    KailuaAssumeField(bool /*static*/, Spanned<(Spanned<NameRef>, Vec<Spanned<Name>>)>,
                      M, Spanned<Kind>),
    KailuaAssumeMethod(Spanned<(Spanned<NameRef>, Vec<Spanned<Name>>)>, M, Spanned<FuncKind>),
}

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
            St::KailuaAssume(ref i_, ref i, m, ref k, is) => {
                write!(f, "KailuaAssume({:?}, {:?}, {:?}, {:?})", i_, i, m, k)?;
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

pub type Stmt = Box<St>;
pub type Block = Vec<Spanned<Stmt>>;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum M {
    None,
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

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum MM {
    None,
    Const,
    Module, // a special modifier (same to None otherwise) for delayed type checking
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

#[derive(Clone, PartialEq)]
pub struct SlotKind {
    pub modf: M,
    pub kind: Spanned<Kind>,
}

impl fmt::Debug for SlotKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {:?}", self.modf, self.kind)
    }
}

#[derive(Clone, PartialEq)]
pub struct FuncKind {
    // the name is purely for description and has no effect in the type.
    // in a valid code all of them (and all unique) or none of them would be present.
    pub args: Seq<(Option<Spanned<Name>>, Spanned<Kind>), Spanned<Kind>>,
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

// not "type" to avoid a conflict (and it's not really a type but a spec that leads to a type)
#[derive(Clone, PartialEq)]
pub enum K {
    Oops,
    Dynamic,
    Any,
    Nil,
    Boolean,
    BooleanLit(bool),
    Number,
    Integer,
    IntegerLit(i32),
    String,
    StringLit(Str),
    Table,
    EmptyTable,
    Record(Vec<(Spanned<Str>, Spanned<SlotKind>)>, bool /*extensible*/),
    Tuple(Vec<Spanned<SlotKind>>),
    Array(Spanned<SlotKind>),
    Map(Spanned<Kind>, Spanned<SlotKind>),
    Function,
    Func(Spanned<FuncKind>),
    Thread,
    UserData,
    Named(Spanned<Name>),
    WithNil(Spanned<Kind>),
    WithoutNil(Spanned<Kind>),
    Union(Vec<Spanned<Kind>>),
    Attr(Spanned<Kind>, Spanned<Attr>),
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

pub type Kind = Box<K>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LocalNameKind {
    User,

    // implicitly created from method declarations
    ImplicitSelf,

    // implicitly created from variadic arguments (Lua 5.0 compat)
    ImplicitLegacyArg,

    // assumed to a local name (which itself is not assumed)
    AssumedToLocal(ScopedId),

    // assumed to a global name with the same name
    AssumedToGlobal,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LocalName {
    pub def_span: Span,
    pub kind: LocalNameKind,
}

// additional informations generated for each token
#[derive(Clone, Debug, PartialEq)]
pub enum TokenAux {
    None,

    // the token is a Name referring to a local variable with given id
    LocalVarName(ScopedId),

    // the token is a Name referring to a global variable with the same name
    GlobalVarName,
}

#[derive(Clone)]
pub struct Chunk {
    // the top-level block
    pub block: Spanned<Block>,

    // scope map for this chunk
    pub map: ScopeMap<Name>,

    // globally defined names with the first definition span (names only used are not included)
    pub global_scope: HashMap<Name, Span>,

    // local name informations for scoped local ids in this chunk
    pub local_names: HashMap<ScopedId, LocalName>,

    // auxiliary information for each input token in the order
    pub token_aux: Vec<TokenAux>,
}

