use std::fmt;
use kailua_diag::Spanned;

fn format_ascii_vec(f: &mut fmt::Formatter, s: &[u8]) -> fmt::Result {
    for &c in s {
        match c {
            b'\t' => try!(write!(f, "\\t")),
            b'\n' => try!(write!(f, "\\n")),
            b'\r' => try!(write!(f, "\\r")),
            b'"' | b'\'' | b'`' | b'\\' => try!(write!(f, "\\{}", c as char)),
            b'\x20'...b'\x7e' => try!(write!(f, "{}", c as char)),
            _ => try!(write!(f, "\\x{:02}", c)),
        }
    }
    Ok(())
}

custom_derive! {
    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, NewtypeFrom, NewtypeDeref)]
    pub struct Name(Vec<u8>);
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "`"));
        try!(format_ascii_vec(f, &self.0));
        try!(write!(f, "`"));
        Ok(())
    }
}

custom_derive! {
    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, NewtypeFrom, NewtypeDeref)]
    pub struct Str(Vec<u8>);
}

impl fmt::Debug for Str {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "\""));
        try!(format_ascii_vec(f, &self.0));
        try!(write!(f, "\""));
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

#[derive(Clone, Debug, PartialEq)]
pub enum Var {
    Name(Spanned<Name>),
    Index(Spanned<Exp>, Spanned<Exp>),
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
    pub args: Vec<Spanned<TypeSpec<Spanned<Name>>>>,
    pub varargs: Option<Spanned<Option<Spanned<Kind>>>>,
    pub returns: Option<Vec<Spanned<Kind>>>,
}

impl Presig {
    pub fn to_sig(self) -> Sig {
        Sig {
            args: self.args.into_iter().map(|arg| arg.base).collect(),
            varargs: self.varargs.map(|varargs| varargs.base),
            returns: self.returns,
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Sig {
    pub args: Vec<TypeSpec<Spanned<Name>>>,
    pub varargs: Option<Option<Spanned<Kind>>>, // may have to be inferred; Const only
    pub returns: Option<Vec<Spanned<Kind>>>, // may have to be inferred
}

impl fmt::Debug for Sig {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref varargs) = self.varargs {
            try!(write!(f, "["));
            let mut first = true;
            for namespec in &self.args {
                if first { first = false; } else { try!(write!(f, ", ")); }
                try!(write!(f, "{:?}", *namespec));
            }
            if !first { try!(write!(f, ", ")); }
            try!(write!(f, "...: "));
            if let Some(ref kind) = *varargs {
                try!(write!(f, "{:?}", *kind));
            } else {
                try!(write!(f, "_"));
            }
            try!(write!(f, "]"));
        } else {
            try!(fmt::Debug::fmt(&self.args, f));
        }
        if let Some(ref returns) = self.returns {
            if returns.len() == 1 {
                try!(write!(f, " -> {:?}", returns[0]));
            } else if !returns.is_empty() {
                try!(write!(f, " -> {:?}", *returns));
            }
        } else {
            try!(write!(f, " -> _"));
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq)]
pub enum Ex {
    // literals
    Nil,
    False,
    True,
    Num(f64),
    Str(Str),
    Varargs,
    Func(Sig, Spanned<Block>),
    Table(Vec<(Option<Spanned<Exp>>, Spanned<Exp>)>),

    // expressions
    Var(Spanned<Name>),
    FuncCall(Spanned<Exp>, Vec<Spanned<Exp>>), // desugared form
    MethodCall(Spanned<Exp>, Spanned<Name>, Vec<Spanned<Exp>>),
    Index(Spanned<Exp>, Spanned<Exp>),
    Un(Spanned<UnOp>, Spanned<Exp>),
    Bin(Spanned<Exp>, Spanned<BinOp>, Spanned<Exp>),
}

impl fmt::Debug for Ex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Ex::Nil => write!(f, "nil"),
            Ex::False => write!(f, "false"),
            Ex::True => write!(f, "true"),
            Ex::Num(v) => write!(f, "{:?}", v),
            Ex::Str(ref s) => write!(f, "{:?}", *s),
            Ex::Varargs => write!(f, "..."),
            Ex::Func(ref p, ref b) => write!(f, "Func({:?}, {:?})", *p, *b),
            Ex::Table(ref fs) => write!(f, "Table({:?})", *fs),

            Ex::Var(ref n) => write!(f, "{:?}", *n),
            Ex::FuncCall(ref e, ref args) => {
                try!(write!(f, "{:?}(", *e));
                let mut first = true;
                for arg in args {
                    if first { first = false; } else { try!(write!(f, ", ")); }
                    try!(write!(f, "{:?}", arg));
                }
                write!(f, ")")
            },
            Ex::MethodCall(ref e, ref n, ref args) => {
                try!(write!(f, "{:?}:{:?}(", *e, *n));
                let mut first = true;
                for arg in args {
                    if first { first = false; } else { try!(write!(f, ", ")); }
                    try!(write!(f, "{:?}", arg));
                }
                write!(f, ")")
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
pub enum FuncScope {
    Local,
    Global,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct SelfParam;

impl fmt::Debug for SelfParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "self") }
}

#[derive(Clone, Debug, PartialEq)]
pub enum St {
    Void(Spanned<Exp>), // technically not every Exp is valid here, but for simplicity.
    Assign(Vec<TypeSpec<Spanned<Var>>>, Vec<Spanned<Exp>>),
    Do(Spanned<Block>),
    While(Spanned<Exp>, Spanned<Block>),
    Repeat(Spanned<Block>, Spanned<Exp>),
    If(Vec<(Spanned<Exp>, Spanned<Block>)>, Option<Spanned<Block>>),
    For(Spanned<Name>, Spanned<Exp>, Spanned<Exp>, Option<Spanned<Exp>>, Spanned<Block>),
    ForIn(Vec<Spanned<Name>>, Vec<Spanned<Exp>>, Spanned<Block>),
    FuncDecl(FuncScope, Spanned<Name>, Sig, Spanned<Block>),
    MethodDecl(Vec<Spanned<Name>>, Option<Spanned<SelfParam>>, Sig, Spanned<Block>),
    Local(Vec<TypeSpec<Spanned<Name>>>, Vec<Spanned<Exp>>),
    Return(Vec<Spanned<Exp>>),
    Break,

    // Kailua extensions
    KailuaAssume(Spanned<Name>, M, Spanned<Kind>, Option<Spanned<Str>>),
}

pub type Stmt = Box<St>;
pub type Block = Vec<Spanned<Stmt>>;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum M {
    None,
    Var,
    Const,
}

impl fmt::Debug for M {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            M::None => write!(f, "_"),
            M::Var => write!(f, "Var"),
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
    pub args: Vec<Spanned<Kind>>,
    pub varargs: Option<Spanned<Kind>>,
    pub returns: Vec<Spanned<Kind>>,
}

impl fmt::Debug for FuncKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "("));
        let mut first = true;
        for kind in &self.args {
            if first { first = false; } else { try!(write!(f, ", ")); }
            try!(write!(f, "{:?}", *kind));
        }
        if let Some(ref kind) = self.varargs {
            if !first { try!(write!(f, ", ")); }
            try!(write!(f, "{:?}...", *kind));
        }
        try!(write!(f, ") -> "));
        if self.returns.len() == 1 {
            try!(write!(f, "{:?}", self.returns[0]));
        } else {
            try!(write!(f, "("));
            let mut first = true;
            for kind in &self.returns {
                if first { first = false; } else { try!(write!(f, ", ")); }
                try!(write!(f, "{:?}", *kind));
            }
            try!(write!(f, ")"));
        }
        Ok(())
    }
}

// not "type" to avoid a conflict (and it's not really a type but a spec that leads to a type)
#[derive(Clone, PartialEq)]
pub enum K {
    Dynamic,
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
    Func(Vec<Spanned<FuncKind>>),
    Union(Vec<Spanned<Kind>>),
}

impl fmt::Debug for K {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            K::Dynamic           => write!(f, "Dynamic"),
            K::Nil               => write!(f, "Nil"),
            K::Boolean           => write!(f, "Boolean"),
            K::BooleanLit(true)  => write!(f, "True"),
            K::BooleanLit(false) => write!(f, "False"),
            K::Number            => write!(f, "Number"),
            K::Integer           => write!(f, "Integer"),
            K::IntegerLit(v)     => write!(f, "Integer({})", v),
            K::String            => write!(f, "String"),
            K::StringLit(ref s)  => write!(f, "String({:?})", *s),
            K::Table             => write!(f, "Table"),
            K::EmptyTable        => write!(f, "EmptyTable"),
            K::Array(ref v)      => write!(f, "Array({:?})", *v),
            K::Map(ref k, ref v) => write!(f, "Map({:?}, {:?})", *k, *v),
            K::Function          => write!(f, "Function"),

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
            K::Func(ref funcs) => write!(f, "Func({:?})", *funcs),
            K::Union(ref kinds) => write!(f, "Union({:?})", *kinds),
        }
    }
}

pub type Kind = Box<K>;

