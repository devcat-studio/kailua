use std::fmt;

fn format_ascii_vec(f: &mut fmt::Formatter, s: &[u8]) -> fmt::Result {
    try!(write!(f, "b\""));
    for &c in s {
        match c {
            b'\t' => try!(write!(f, "\\t")),
            b'\n' => try!(write!(f, "\\n")),
            b'\r' => try!(write!(f, "\\r")),
            b'"' | b'\\' => try!(write!(f, "\\{}", c as char)),
            b'\x20'...b'\x7e' => try!(write!(f, "{}", c as char)),
            _ => try!(write!(f, "\\x{:02}", c)),
        }
    }
    try!(write!(f, "\""));
    Ok(())
}

custom_derive! {
    #[derive(Clone, PartialEq, NewtypeFrom, NewtypeDeref)]
    pub struct Name(Vec<u8>);
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "Name("));
        try!(format_ascii_vec(f, &self.0));
        try!(write!(f, ")"));
        Ok(())
    }
}

custom_derive! {
    #[derive(Clone, PartialEq, NewtypeFrom, NewtypeDeref)]
    pub struct Str(Vec<u8>);
}

impl fmt::Debug for Str {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "Str("));
        try!(format_ascii_vec(f, &self.0));
        try!(write!(f, ")"));
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Var {
    Name(Name),
    Index(Exp, Exp),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Params(pub Vec<Name>, pub bool /*varargs?*/);

#[derive(Clone, Debug, PartialEq)]
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
    Var(Var),
    FuncCall(Exp, Vec<Exp>), // desugared form
    MethodCall(Exp, Name, Vec<Exp>),
    Index(Exp, Exp),
    Un(UnOp, Exp),
    Bin(Exp, BinOp, Exp),
}

pub type Exp = Box<E>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
    Len,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Exp,
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

#[derive(Clone, Debug, PartialEq)]
pub enum S {
    Exp(Exp), // technically not every Exp is valid, but for simplicity.
    Assign(Vec<Var>, Vec<Exp>),
    Do(Block),
    While(Exp, Block),
    Repeat(Block, Exp),
    If(Vec<(Exp, Block)>, Option<Block>),
    For(Name, Exp, Exp, Option<Exp>, Block),
    ForIn(Vec<Name>, Vec<Exp>, Block),
    FuncDecl(bool /*local?*/, Name, Params, Block),
    MethodDecl(Vec<Name>, bool /*self?*/, Params, Block),
    Local(Vec<Name>, Vec<Exp>),
    Return(Vec<Exp>),
    Break,
}

pub type Stmt = Box<S>;

pub type Block = Vec<Stmt>;

