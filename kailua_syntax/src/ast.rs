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
    #[derive(Clone, PartialEq, NewtypeFrom, NewtypeDeref)]
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
    #[derive(Clone, PartialEq, NewtypeFrom, NewtypeDeref)]
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
pub struct Params(pub Vec<Name>, pub bool /*varargs?*/);

impl fmt::Debug for Params {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.1 {
            try!(write!(f, "["));
            let mut first = true;
            for name in &self.0 {
                if first { first = false; } else { try!(write!(f, ", ")); }
                try!(write!(f, "{:?}", name));
            }
            if !first { try!(write!(f, ", ")); }
            try!(write!(f, "...]"));
            Ok(())
        } else {
            fmt::Debug::fmt(&self.0, f)
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum E {
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

impl fmt::Debug for E {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            E::Nil => write!(f, "nil"),
            E::False => write!(f, "false"),
            E::True => write!(f, "true"),
            E::Num(v) => write!(f, "{:?}", v),
            E::Str(ref s) => write!(f, "{:?}", *s),
            E::Varargs => write!(f, "..."),
            E::Func(ref p, ref b) => write!(f, "Func({:?}, {:?})", *p, *b),
            E::Table(ref fs) => write!(f, "Table({:?})", *fs),

            E::Var(ref n) => write!(f, "{:?}", *n),
            E::FuncCall(ref e, ref args) => {
                try!(write!(f, "{:?}(", *e));
                let mut first = true;
                for arg in args {
                    if first { first = false; } else { try!(write!(f, ", ")); }
                    try!(write!(f, "{:?}", arg));
                }
                write!(f, ")")
            },
            E::MethodCall(ref e, ref n, ref args) => {
                try!(write!(f, "{:?}:{:?}(", *e, *n));
                let mut first = true;
                for arg in args {
                    if first { first = false; } else { try!(write!(f, ", ")); }
                    try!(write!(f, "{:?}", arg));
                }
                write!(f, ")")
            },
            E::Index(ref e, ref i) => write!(f, "{:?}[{:?}]", *e, *i),
            E::Un(op, ref e) => write!(f, "({} {:?})", op.symbol(), *e),
            E::Bin(ref l, op, ref r) => write!(f, "({:?} {} {:?})", *l, op.symbol(), *r),
        }
    }
}

impl From<f64> for E { fn from(v: f64) -> E { E::Num(v) } }
impl From<Str> for E { fn from(s: Str) -> E { E::Str(s) } }
impl From<Name> for E { fn from(n: Name) -> E { E::Var(n.into()) } }

pub type Exp = Box<E>;

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
pub enum S {
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
}

impl From<Exp> for S { fn from(e: Exp) -> S { S::Void(e) } }
impl From<Block> for S { fn from(block: Block) -> S { S::Do(block) } }

pub type Stmt = Box<S>;

impl From<Exp> for Stmt { fn from(x: Exp) -> Stmt { Box::new(From::from(x)) } }
impl From<Block> for Stmt { fn from(x: Block) -> Stmt { Box::new(From::from(x)) } }

pub type Block = Vec<Stmt>;

