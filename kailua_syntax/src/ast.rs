use std::fmt;

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
    Name(Name),
    Index(Exp, Exp),
}

impl From<Name> for Var {
    fn from(n: Name) -> Var { Var::Name(n) }
}

#[derive(Clone, PartialEq)]
pub struct Params {
    pub args: Vec<Name>,
    pub variadic: bool,
}

impl fmt::Debug for Params {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.variadic {
            try!(write!(f, "["));
            let mut first = true;
            for name in &self.args {
                if first { first = false; } else { try!(write!(f, ", ")); }
                try!(write!(f, "{:?}", name));
            }
            if !first { try!(write!(f, ", ")); }
            try!(write!(f, "...]"));
            Ok(())
        } else {
            fmt::Debug::fmt(&self.args, f)
        }
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
    Func(Params, Block),
    Table(Vec<(Option<Exp>, Exp)>),

    // expressions
    Var(Name),
    FuncCall(Exp, Vec<Exp>), // desugared form
    MethodCall(Exp, Name, Vec<Exp>),
    Index(Exp, Exp),
    Un(UnOp, Exp),
    Bin(Exp, BinOp, Exp),
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

impl From<f64> for Ex { fn from(v: f64) -> Ex { Ex::Num(v) } }
impl From<Str> for Ex { fn from(s: Str) -> Ex { Ex::Str(s) } }
impl From<Name> for Ex { fn from(n: Name) -> Ex { Ex::Var(n.into()) } }

pub type Exp = Box<Ex>;

impl From<f64> for Exp { fn from(x: f64) -> Exp { Box::new(From::from(x)) } }
impl From<Str> for Exp { fn from(x: Str) -> Exp { Box::new(From::from(x)) } }
impl From<Name> for Exp { fn from(x: Name) -> Exp { Box::new(From::from(x)) } }

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
pub enum SelfParam {
    No,
    Yes,
}

impl fmt::Debug for SelfParam {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SelfParam::No => write!(f, "()"),
            SelfParam::Yes => write!(f, "(self)"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum St {
    Void(Exp), // technically not every Exp is valid here, but for simplicity.
    Assign(Vec<Var>, Vec<Exp>),
    Do(Block),
    While(Exp, Block),
    Repeat(Block, Exp),
    If(Vec<(Exp, Block)>, Option<Block>),
    For(Name, Exp, Exp, Option<Exp>, Block),
    ForIn(Vec<Name>, Vec<Exp>, Block),
    FuncDecl(FuncScope, Name, Params, Block),
    MethodDecl(Vec<Name>, SelfParam, Params, Block),
    Local(Vec<Name>, Vec<Exp>),
    Return(Vec<Exp>),
    Break,

    // Kailua extensions
    KailuaAssume(Name, Kind, Option<Str>),
}

impl From<Exp> for St { fn from(e: Exp) -> St { St::Void(e) } }
impl From<Block> for St { fn from(block: Block) -> St { St::Do(block) } }

pub type Stmt = Box<St>;

impl From<Exp> for Stmt { fn from(x: Exp) -> Stmt { Box::new(From::from(x)) } }
impl From<Block> for Stmt { fn from(x: Block) -> Stmt { Box::new(From::from(x)) } }

pub type Block = Vec<Stmt>;

// not "type" to avoid a conflict (and it's not really a type but a spec that leads to a type)
#[derive(Clone, Debug, PartialEq)]
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
    Function,
    Union(Vec<Kind>),
}

pub type Kind = Box<K>;

