use std::fmt;
use kailua_env::{Spanned, Scope, ScopeMap};

fn format_ascii_vec(f: &mut fmt::Formatter, s: &[u8]) -> fmt::Result {
    for &c in s {
        match c {
            b'\t' => try!(write!(f, "\\t")),
            b'\n' => try!(write!(f, "\\n")),
            b'\r' => try!(write!(f, "\\r")),
            b'"' | b'\'' | b'`' | b'\\' => try!(write!(f, "\\{}", c as char)),
            b'\x20'...b'\x7e' => try!(write!(f, "{}", c as char)),
            _ => try!(write!(f, "\\x{:02x}", c)),
        }
    }
    Ok(())
}

custom_derive! {
    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, NewtypeFrom, NewtypeDeref)]
    pub struct Name(Vec<u8>);
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !f.sign_minus() { try!(write!(f, "`")); }
        try!(format_ascii_vec(f, &self.0));
        if !f.sign_minus() { try!(write!(f, "`")); }
        Ok(())
    }
}

custom_derive! {
    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, NewtypeFrom, NewtypeDeref)]
    pub struct Str(Vec<u8>);
}

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl fmt::Debug for Str {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !f.sign_minus() { try!(write!(f, "\"")); }
        try!(format_ascii_vec(f, &self.0));
        if !f.sign_minus() { try!(write!(f, "\"")); }
        Ok(())
    }
}

impl<'a> From<&'a [u8]> for Name {
    fn from(s: &'a [u8]) -> Name { Name(s.to_owned()) }
}

impl<'a> From<&'a [u8]> for Str {
    fn from(s: &'a [u8]) -> Str { Str(s.to_owned()) }
}

impl From<Str> for Name {
    fn from(Str(s): Str) -> Name { Name(s) }
}

impl From<Name> for Str {
    fn from(Name(n): Name) -> Str { Str(n) }
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

impl<Head: fmt::Debug, Tail: fmt::Debug> fmt::Debug for Seq<Head, Tail> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !f.sign_minus() { try!(write!(f, "[")); }
        let mut first = true;
        for e in &self.head {
            if first { first = false; } else { try!(write!(f, ", ")); }
            try!(write!(f, "{:?}", *e));
        }
        if let Some(ref e) = self.tail {
            if !first { try!(write!(f, ", ")); }
            try!(write!(f, "{:?}...", *e));
        }
        if !f.sign_minus() { try!(write!(f, "]")); }
        Ok(())
    }
}

#[derive(Clone, PartialEq)]
pub enum Var {
    Name(Spanned<Name>),
    Index(Spanned<Exp>, Spanned<Exp>),
}

impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Var::Name(ref name) => write!(f, "{:?}", name),
            Var::Index(ref lhs, ref rhs) => write!(f, "({:?})[{:?}]", lhs, rhs),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct TypeSpec<T> {
    pub base: T,
    pub modf: M,
    pub kind: Option<Spanned<Kind>>,
}

impl<T: fmt::Debug> fmt::Debug for TypeSpec<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "{:?}", self.base));
        match (self.modf, &self.kind) {
            (M::None, &None) => Ok(()),
            (modf, &None) => write!(f, ": {:?}", modf),
            (modf, &Some(ref kind)) => write!(f, ": {:?} {:?}", modf, kind),
        }
    }
}

// more spanned version of Sig, only used during the parsing
#[derive(Clone, PartialEq)]
pub struct Presig {
    pub args: Spanned<Seq<Spanned<TypeSpec<Spanned<Name>>>, Spanned<Option<Spanned<Kind>>>>>,
    pub returns: Option<Seq<Spanned<Kind>>>,
}

impl Presig {
    pub fn to_sig(self, attrs: Vec<Spanned<Attr>>) -> Sig {
        Sig {
            attrs: attrs,
            args: self.args.map(|args| {
                Seq { head: args.head.into_iter().map(|arg| arg.base).collect(),
                      tail: args.tail.map(|arg| arg.base) }
            }),
            returns: self.returns,
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Sig {
    pub attrs: Vec<Spanned<Attr>>,
    pub args: Spanned<Seq<TypeSpec<Spanned<Name>>,
                      Option<Spanned<Kind>>>>, // may have to be inferred; Const only
    pub returns: Option<Seq<Spanned<Kind>>>, // may have to be inferred
}

impl fmt::Debug for Sig {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for attr in &self.attrs {
            try!(write!(f, "{:?} ", attr));
        }
        try!(write!(f, "["));
        let mut first = true;
        for namespec in &self.args.head {
            if first { first = false; } else { try!(write!(f, ", ")); }
            try!(write!(f, "{:?}", *namespec));
        }
        if let Some(ref varargs) = self.args.tail {
            if !first { try!(write!(f, ", ")); }
            try!(write!(f, "...: "));
            if let Some(ref kind) = *varargs {
                try!(write!(f, "{:?}", *kind));
            } else {
                try!(write!(f, "_"));
            }
        }
        try!(write!(f, "]"));
        if let Some(ref returns) = self.returns {
            if returns.head.len() == 1 && returns.tail.is_none() {
                try!(write!(f, " --> {:?}", returns.head[0]));
            } else if !(returns.head.is_empty() && returns.tail.is_none()) {
                try!(write!(f, " --> {:?}", *returns));
            }
        } else {
            try!(write!(f, " --> _"));
        }
        Ok(())
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
    Var(Spanned<Name>),
    FuncCall(Spanned<Exp>, Spanned<Vec<Spanned<Exp>>>), // desugared form
    MethodCall(Spanned<Exp>, Spanned<Name>, Spanned<Vec<Spanned<Exp>>>),
    Index(Spanned<Exp>, Spanned<Exp>),
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
            Ex::Table(ref fs) => write!(f, "Table({:?})", *fs),

            Ex::Var(ref n) => write!(f, "{:?}", *n),
            Ex::FuncCall(ref e, ref args) => {
                try!(write!(f, "{:?}(", *e));
                let mut first = true;
                for arg in &args.base {
                    if first { first = false; } else { try!(write!(f, ", ")); }
                    try!(write!(f, "{:?}", arg));
                }
                try!(write!(f, ")"));
                fmt::Debug::fmt(&args.span, f)
            },
            Ex::MethodCall(ref e, ref n, ref args) => {
                try!(write!(f, "{:?}{:?}(", *e, *n));
                let mut first = true;
                for arg in &args.base {
                    if first { first = false; } else { try!(write!(f, ", ")); }
                    try!(write!(f, "{:?}", arg));
                }
                try!(write!(f, ")"));
                fmt::Debug::fmt(&args.span, f)
            },
            Ex::Index(ref e, ref i) => write!(f, "{:?}[{:?}]", *e, *i),
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Vis {
    Local,
    Global,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct SelfParam;

impl fmt::Debug for SelfParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "self") }
}

#[derive(Clone, PartialEq)]
pub enum St {
    Oops,

    Void(Spanned<Exp>), // technically not every Exp is valid here, but for simplicity.
    Assign(Spanned<Vec<TypeSpec<Spanned<Var>>>>, Spanned<Vec<Spanned<Exp>>>),
    Do(Spanned<Block>),
    While(Spanned<Exp>, Spanned<Block>),
    Repeat(Spanned<Block>, Spanned<Exp>),
    If(Vec<Spanned<(Spanned<Exp>, Spanned<Block>)>>, Option<Spanned<Block>>),
    For(Spanned<Name>, Spanned<Exp>, Spanned<Exp>, Option<Spanned<Exp>>, Scope, Spanned<Block>),
    ForIn(Spanned<Vec<Spanned<Name>>>, Spanned<Vec<Spanned<Exp>>>, Scope, Spanned<Block>),
    FuncDecl(Vis, Spanned<Name>, Sig, Scope, Spanned<Block>, Option<Scope>),
    MethodDecl(Vec<Spanned<Name>>, Option<TypeSpec<Spanned<SelfParam>>>,
               Sig, Scope, Spanned<Block>),
    Local(Spanned<Vec<TypeSpec<Spanned<Name>>>>, Spanned<Vec<Spanned<Exp>>>, Scope),
    Return(Spanned<Vec<Spanned<Exp>>>),
    Break,

    // Kailua extensions
    KailuaOpen(Spanned<Name>),
    KailuaType(Spanned<Name>, Spanned<Kind>),
    KailuaAssume(Vis, Spanned<Name>, M, Spanned<Kind>, Option<Scope>),
}

impl fmt::Debug for St {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            St::Oops => write!(f, "Oops"),

            St::Void(ref e) => write!(f, "Void({:?})", e),
            St::Assign(ref l, ref r) => write!(f, "Assign({:?}, {:?})", l, r),
            St::Do(ref b) => write!(f, "Do({:?})", b),
            St::While(ref e, ref b) => write!(f, "While({:?}, {:?})", e, b),
            St::Repeat(ref b, ref e) => write!(f, "Repeat({:?}, {:?})", b, e),
            St::If(ref cases, ref else_) => {
                try!(write!(f, "If("));
                let mut first = true;
                for &Spanned { base: (ref e, ref b), span } in cases {
                    if first { first = false; } else { try!(write!(f, ", ")); }
                    try!(write!(f, "({:?} => {:?}){:?}", e, b, span));
                }
                if let Some(ref b) = *else_ {
                    if !first { try!(write!(f, ", ")); }
                    try!(write!(f, "{:?}", b));
                }
                write!(f, ")")
            },
            St::For(ref i, ref start, ref end, ref step, bs, ref b) =>
                write!(f, "For({:?}, {:?}, {:?}, {:?}, {:?}{:?})", i, start, end, step, bs, b),
            St::ForIn(ref ii, ref ee, bs, ref b) =>
                write!(f, "ForIn({:?}, {:?}, {:?}{:?})", ii, ee, bs, b),
            St::FuncDecl(ns, ref i, ref sig, bs, ref b, Some(is)) =>
                write!(f, "FuncDecl({:?}, {:?}, {:?}, {:?}{:?}){:?}", ns, i, sig, bs, b, is),
            St::FuncDecl(ns, ref i, ref sig, bs, ref b, None) =>
                write!(f, "FuncDecl({:?}, {:?}, {:?}, {:?}{:?})", ns, i, sig, bs, b),
            St::MethodDecl(ref ii, ref self_, ref sig, bs, ref b) =>
                write!(f, "MethodDecl({:?}, {:?}, {:?}, {:?}{:?})", ii, self_, sig, bs, b),
            St::Local(ref ii, ref ee, is) => write!(f, "Local({:?}, {:?}){:?}", ii, ee, is),
            St::Return(ref ee) => write!(f, "Return({:?})", ee),
            St::Break => write!(f, "Break"),

            St::KailuaOpen(ref lib) => write!(f, "KailuaOpen({:?})", lib),
            St::KailuaType(ref t, ref k) => write!(f, "KailuaType({:?}, {:?})", t, k),
            St::KailuaAssume(ns, ref i, m, ref k, Some(is)) =>
                write!(f, "KailuaAssume({:?}, {:?}, {:?}, {:?}){:?}", ns, i, m, k, is),
            St::KailuaAssume(ns, ref i, m, ref k, None) =>
                write!(f, "KailuaAssume({:?}, {:?}, {:?}, {:?})", ns, i, m, k),
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
    pub args: Seq<Spanned<Kind>>,
    pub returns: Seq<Spanned<Kind>>,
}

impl fmt::Debug for FuncKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "({:-?})", self.args));
        if self.returns.head.len() == 1 && self.returns.tail.is_none() {
            try!(write!(f, " --> {:?}", self.returns.head[0]));
        } else {
            try!(write!(f, " --> ({:-?})", self.returns));
        }
        Ok(())
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
    Record(Vec<(Spanned<Str>, Spanned<SlotKind>)>),
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

            K::Record(ref fields) => {
                try!(write!(f, "Record(["));
                let mut first = true;
                for &(ref name, ref value) in fields {
                    if first { first = false; } else { try!(write!(f, ", ")); }
                    try!(write!(f, "{:?}: {:?}", *name, *value));
                }
                try!(write!(f, "])"));
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

pub struct Chunk {
    pub block: Spanned<Block>,
    pub global_scope: Scope,
    pub map: ScopeMap<Name>,
}

