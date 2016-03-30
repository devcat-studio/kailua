use std::iter;
use std::i32;

use lex::{Tok, Punct, Keyword};
use ast::{Name, Str, Var, Params, Ex, Exp, UnOp, BinOp, FuncScope, SelfParam, St, Stmt, Block};
use ast::{M, K, Kind};

pub struct Parser<T> {
    iter: iter::Fuse<T>,
    lookahead: Option<Tok>,
}

pub type Error = ::lex::Error;

pub type ParseResult<T> = Result<T, Error>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct EOF; // a placeholder arg to `expect`

trait Expectable {
    fn check_token(&self, tok: &Tok) -> bool;
}

impl Expectable for Punct {
    fn check_token(&self, tok: &Tok) -> bool { tok == &Tok::Punct(*self) }
}

impl Expectable for Keyword {
    fn check_token(&self, tok: &Tok) -> bool { tok == &Tok::Keyword(*self) }
}

impl Expectable for EOF {
    fn check_token(&self, tok: &Tok) -> bool { tok == &Tok::EOF }
}

impl<T: Iterator<Item=Tok>> Parser<T> {
    pub fn new(iter: T) -> Parser<T> {
        Parser { iter: iter.fuse(), lookahead: None }
    }

    fn read(&mut self) -> Tok {
        self.lookahead.take().or_else(|| self.iter.next()).unwrap_or(Tok::EOF)
    }

    fn unread(&mut self, tok: Tok) {
        assert!(self.lookahead.is_none(), "only one lookahead token is supported");
        self.lookahead = Some(tok);
    }

    fn peek<'a>(&'a mut self) -> &'a Tok {
        let tok = self.read();
        self.unread(tok);
        self.lookahead.as_ref().unwrap()
    }

    fn expect<Tok: Expectable + ::std::fmt::Debug>(&mut self, tok: Tok) -> ParseResult<()> {
        if !tok.check_token(&self.read()) { return Err("expect failed"); }
        Ok(())
    }

    fn lookahead<Tok: Expectable>(&mut self, tok: Tok) -> bool {
        tok.check_token(self.peek())
    }

    fn try_parse_name(&mut self) -> ParseResult<Option<Name>> {
        let tok = self.read();
        if let Tok::Name(name) = tok {
            Ok(Some(name.into()))
        } else {
            self.unread(tok);
            Ok(None)
        }
    }

    fn parse_name(&mut self) -> ParseResult<Name> {
        if let Some(name) = try!(self.try_parse_name()) {
            Ok(name)
        } else {
            Err("expected name")
        }
    }

    fn parse_block(&mut self) -> ParseResult<Block> {
        let mut stmts = Vec::new();
        while let Some(stmt) = try!(self.try_parse_stmt()) {
            if self.lookahead(Punct::Semicolon) {
                self.read();
            }

            // if the statement is the final one, stop parsing
            let last = match &*stmt {
                &St::Return(..) | &St::Break => true,
                _ => false,
            };

            stmts.push(stmt);
            if last { break; }
        }
        Ok(stmts)
    }

    fn try_parse_stmt(&mut self) -> ParseResult<Option<Stmt>> {
        // if there exists a spec stmt return it first.
        // a spec may be empty, so loop until no spec exists or a spec is found.
        loop {
            match try!(self.try_parse_kailua_spec()) {
                Some(Some(spec)) => return Ok(Some(spec)),
                Some(None) => continue,
                None => break,
            }
        }

        match self.read() {
            Tok::Keyword(Keyword::Do) => {
                let block = try!(self.parse_block());
                try!(self.expect(Keyword::End));
                Ok(Some(Box::new(St::Do(block))))
            }

            Tok::Keyword(Keyword::While) => {
                let cond = try!(self.parse_exp());
                try!(self.expect(Keyword::Do));
                let block = try!(self.parse_block());
                try!(self.expect(Keyword::End));
                Ok(Some(Box::new(St::While(cond, block))))
            }

            Tok::Keyword(Keyword::Repeat) => {
                let block = try!(self.parse_block());
                try!(self.expect(Keyword::Until));
                let cond = try!(self.parse_exp());
                Ok(Some(Box::new(St::Repeat(block, cond))))
            }

            Tok::Keyword(Keyword::If) => {
                let cond = try!(self.parse_exp());
                try!(self.expect(Keyword::Then));
                let block = try!(self.parse_block());
                let mut blocks = vec![(cond, block)];
                while self.lookahead(Keyword::Elseif) {
                    self.read();
                    let cond = try!(self.parse_exp());
                    try!(self.expect(Keyword::Then));
                    let block = try!(self.parse_block());
                    blocks.push((cond, block));
                }
                let lastblock = if self.lookahead(Keyword::Else) {
                    self.read();
                    Some(try!(self.parse_block()))
                } else {
                    None
                };
                try!(self.expect(Keyword::End));
                Ok(Some(Box::new(St::If(blocks, lastblock))))
            }

            Tok::Keyword(Keyword::For) => {
                let name = try!(self.parse_name());
                try!(self.try_parse_kailua_type_spec());
                match self.read() {
                    // for NAME "=" ...
                    Tok::Punct(Punct::Eq) => {
                        let start = try!(self.parse_exp());
                        try!(self.expect(Punct::Comma));
                        let end = try!(self.parse_exp());
                        let step = if self.lookahead(Punct::Comma) {
                            self.read();
                            Some(try!(self.parse_exp()))
                        } else {
                            None
                        };
                        try!(self.expect(Keyword::Do));
                        let block = try!(self.parse_block());
                        try!(self.expect(Keyword::End));
                        Ok(Some(Box::new(St::For(name, start, end, step, block))))
                    }

                    // for NAME in ...
                    Tok::Keyword(Keyword::In) => {
                        Ok(Some(try!(self.parse_stmt_for_in(vec![name]))))
                    }

                    // for NAME "," ... in ...
                    Tok::Punct(Punct::Comma) => {
                        let mut vars = vec![name.into()];
                        try!(self.scan_namelist(|name| vars.push(name.into())));
                        try!(self.expect(Keyword::In));
                        Ok(Some(try!(self.parse_stmt_for_in(vars))))
                    }

                    _ => return Err("unexpected token after `for NAME`"),
                }
            }

            Tok::Keyword(Keyword::Function) => {
                let mut names = vec![try!(self.parse_name())];
                let mut selfparam = SelfParam::No;
                while self.lookahead(Punct::Dot) {
                    self.read();
                    names.push(try!(self.parse_name()));
                }
                if self.lookahead(Punct::Colon) {
                    self.read();
                    names.push(try!(self.parse_name()));
                    selfparam = SelfParam::Yes;
                }
                let (params, body) = try!(self.parse_func_body());
                if names.len() == 1 {
                    assert!(selfparam == SelfParam::No,
                            "ordinary function cannot have an implicit self");
                    let name = names.pop().unwrap();
                    Ok(Some(Box::new(St::FuncDecl(FuncScope::Global, name, params, body))))
                } else {
                    Ok(Some(Box::new(St::MethodDecl(names, selfparam, params, body))))
                }
            }

            Tok::Keyword(Keyword::Local) => {
                match self.read() {
                    // local function ...
                    Tok::Keyword(Keyword::Function) => {
                        let name = try!(self.parse_name());
                        let (params, body) = try!(self.parse_func_body());
                        Ok(Some(Box::new(St::FuncDecl(FuncScope::Local, name, params, body))))
                    }

                    // local NAME ...
                    tok @ Tok::Name(_) => {
                        self.unread(tok);

                        let mut names = Vec::new();
                        let mut exps = Vec::new();
                        try!(self.scan_namelist(|name| names.push(name)));
                        if self.lookahead(Punct::Eq) {
                            try!(self.try_parse_kailua_type_spec());
                            self.read();
                            try!(self.try_parse_kailua_type_spec());
                            try!(self.scan_explist(|exp| exps.push(exp)));
                        }
                        try!(self.try_parse_kailua_type_spec());
                        Ok(Some(Box::new(St::Local(names, exps))))
                    }

                    _ => return Err("unexpected token after `local`"),
                }
            }

            Tok::Keyword(Keyword::Return) => {
                let mut exps = Vec::new();
                try!(self.try_scan_explist(|exp| exps.push(exp)));
                Ok(Some(Box::new(St::Return(exps))))
            }

            Tok::Keyword(Keyword::Break) => {
                Ok(Some(Box::new(St::Break)))
            }

            tok => {
                self.unread(tok);

                if let Some(exp) = try!(self.try_parse_prefix_exp(false)) {
                    // prefixexp consumes pretty much everything.
                    // it might be a single statement as whole,
                    // or the beginning of `varlist "=" explist`.
                    // determine if prefixexp is a function call or an indexing op,
                    // and convert it to `Var` for the latter.
                    match self.convert_prefix_exp_to_var(exp) {
                        // var {"," var} "=" explist
                        Ok(var) => {
                            let mut lhs = vec![var];
                            try!(self.try_parse_kailua_type_spec());
                            if self.lookahead(Punct::Comma) {
                                self.read();
                                try!(self.scan_varlist(|var| lhs.push(var)));
                            }
                            try!(self.expect(Punct::Eq));
                            try!(self.try_parse_kailua_type_spec());

                            let mut rhs = Vec::new();
                            try!(self.scan_explist(|exp| rhs.push(exp)));

                            try!(self.try_parse_kailua_type_spec());

                            return Ok(Some(Box::new(St::Assign(lhs, rhs))));
                        }

                        // prefixexp
                        Err(exp) => {
                            return Ok(Some(Box::new(St::Void(exp))));
                        }
                    }
                }

                Ok(None)
            }
        }
    }

    fn parse_stmt_for_in(&mut self, names: Vec<Name>) -> ParseResult<Stmt> {
        let mut exps = Vec::new();
        try!(self.scan_explist(|exp| exps.push(exp)));
        try!(self.expect(Keyword::Do));
        let block = try!(self.parse_block());
        try!(self.expect(Keyword::End));
        Ok(Box::new(St::ForIn(names, exps, block)))
    }

    fn parse_func_body(&mut self) -> ParseResult<(Params, Block)> {
        let mut params = Params { args: Vec::new(), variadic: false };

        try!(self.expect(Punct::LParen));
        match self.read() {
            Tok::Punct(Punct::DotDotDot) => {
                params.variadic = true;
                try!(self.try_parse_kailua_type_spec());
            }
            Tok::Name(name) => {
                params.args.push(name.into());
                try!(self.try_parse_kailua_type_spec());
                while self.lookahead(Punct::Comma) {
                    self.read();
                    try!(self.try_parse_kailua_type_spec());
                    if self.lookahead(Punct::DotDotDot) {
                        self.read();
                        try!(self.try_parse_kailua_type_spec());
                        params.variadic = true;
                        break;
                    } else {
                        params.args.push(try!(self.parse_name()));
                        try!(self.try_parse_kailua_type_spec());
                    }
                }
            }
            tok @ Tok::Punct(Punct::RParen) => {
                self.unread(tok);
            }
            _ => return Err("unexpected token after `function ... (`")
        }
        try!(self.expect(Punct::RParen));
        if try!(self.try_parse_kailua_rettype_spec()).is_none() {
            try!(self.try_parse_kailua_type_spec());
        }
        let block = try!(self.parse_block());
        try!(self.expect(Keyword::End));

        Ok((params, block))
    }

    fn parse_table(&mut self) -> ParseResult<Vec<(Option<Exp>, Exp)>> {
        let mut fields = Vec::new();

        try!(self.expect(Punct::LBrace));
        loop {
            let key;
            let value;
            match self.read() {
                Tok::Punct(Punct::RBrace) => break,

                Tok::Punct(Punct::LBracket) => {
                    key = Some(try!(self.parse_exp()));
                    try!(self.expect(Punct::RBracket));
                    try!(self.expect(Punct::Eq));
                    value = try!(self.parse_exp());
                }

                tok => {
                    self.unread(tok);

                    // it is hard to disambiguiate `NAME "=" exp` and `exp`,
                    // so parse `exp` first and check if it's a `NAME` followed by `=`.
                    let exp = try!(self.parse_exp());
                    let name_or_exp = if self.lookahead(Punct::Eq) {
                        match *exp {
                            Ex::Var(name) => Ok(name),
                            exp => Err(Box::new(exp)),
                        }
                    } else {
                        Err(exp)
                    };
                    match name_or_exp {
                        Ok(name) => {
                            let s: Str = name.into();
                            key = Some(s.into());
                            try!(self.expect(Punct::Eq));
                            value = try!(self.parse_exp());
                        }
                        Err(exp) => {
                            key = None;
                            value = exp;
                        }
                    }
                }
            }

            fields.push((key, value));

            match self.read() {
                Tok::Punct(Punct::Comma) | Tok::Punct(Punct::Semicolon) => {}
                Tok::Punct(Punct::RBrace) => break,
                _ => return Err("unexpected token in the table constructor")
            }
        }

        Ok(fields)
    }

    fn try_parse_args(&mut self) -> ParseResult<Option<Vec<Exp>>> {
        match self.read() {
            Tok::Punct(Punct::LParen) => {
                let mut args = Vec::new();
                try!(self.try_scan_explist(|exp| args.push(exp)));
                try!(self.expect(Punct::RParen));
                Ok(Some(args))
            }
            Tok::Str(s) => {
                Ok(Some(vec![Str::from(s).into()]))
            }
            tok @ Tok::Punct(Punct::LBrace) => {
                self.unread(tok);
                let exp = Box::new(Ex::Table(try!(self.parse_table())));
                Ok(Some(vec![exp]))
            }
            tok => {
                self.unread(tok);
                Ok(None)
            }
        }
    }

    fn try_parse_prefix_exp(&mut self, var_only: bool) -> ParseResult<Option<Exp>> {
        // any prefixexp starts with name or parenthesized exp
        let mut exp;
        match self.read() {
            Tok::Punct(Punct::LParen) => {
                exp = try!(self.parse_exp());
                try!(self.expect(Punct::RParen));
            }
            Tok::Name(name) => {
                exp = Box::new(Ex::Var(name.into()));
            }
            tok => {
                self.unread(tok);
                return Ok(None);
            }
        }

        // parse any postfix attachments
        loop {
            match self.read() {
                // prefixexp "." ...
                Tok::Punct(Punct::Dot) => {
                    if let Tok::Name(name) = self.read() {
                        exp = Box::new(Ex::Index(exp, Str::from(name).into()));
                    } else {
                        return Err("unexpected token after `<expression> .`");
                    }
                }

                // prefixexp "[" ...
                Tok::Punct(Punct::LBracket) => {
                    let exp2 = try!(self.parse_exp());
                    try!(self.expect(Punct::RBracket));
                    exp = Box::new(Ex::Index(exp, exp2));
                }

                // prefixexp ":" ...
                Tok::Punct(Punct::Colon) if !var_only => {
                    let name = try!(self.parse_name());
                    if let Some(args) = try!(self.try_parse_args()) {
                        exp = Box::new(Ex::MethodCall(exp, name, args));
                    } else {
                        return Err("unexpected token after `<expression> : <name>`");
                    }
                }

                // prefixexp STR
                // prefixexp "("
                // prefixexp "{"
                tok => {
                    // try to parse as args (if !var_only), or bail out
                    self.unread(tok);
                    if !var_only {
                        if let Some(args) = try!(self.try_parse_args()) {
                            exp = Box::new(Ex::FuncCall(exp, args));
                            continue;
                        }
                    }
                    break;
                }
            }
        }
        Ok(Some(exp))
    }

    fn convert_prefix_exp_to_var(&self, exp: Exp) -> Result<Var, Exp> {
        let exp = *exp;
        match exp {
            Ex::Var(name) => Ok(Var::Name(name)),
            Ex::Index(e1, e2) => Ok(Var::Index(e1, e2)),
            exp => Err(Box::new(exp)),
        }
    }

    fn try_parse_atomic_exp(&mut self) -> ParseResult<Option<Exp>> {
        match self.read() {
            Tok::Keyword(Keyword::Nil) => Ok(Some(Box::new(Ex::Nil))),
            Tok::Keyword(Keyword::False) => Ok(Some(Box::new(Ex::False))),
            Tok::Keyword(Keyword::True) => Ok(Some(Box::new(Ex::True))),
            Tok::Num(v) => Ok(Some(Box::new(Ex::Num(v)))),
            Tok::Str(s) => Ok(Some(Box::new(Ex::Str(s.into())))),
            Tok::Punct(Punct::DotDotDot) => Ok(Some(Box::new(Ex::Varargs))),

            Tok::Keyword(Keyword::Function) => {
                let (params, body) = try!(self.parse_func_body());
                Ok(Some(Box::new(Ex::Func(params, body))))
            }

            tok @ Tok::Punct(Punct::LBrace) => {
                self.unread(tok);
                Ok(Some(Box::new(Ex::Table(try!(self.parse_table())))))
            }

            tok => {
                self.unread(tok);
                self.try_parse_prefix_exp(false)
            }
        }
    }

    fn try_parse_prefix_unary_exp<Term, Op>(&mut self,
                                            mut check_op: Op,
                                            mut try_parse_term: Term) -> ParseResult<Option<Exp>>
            where Term: FnMut(&mut Self) -> ParseResult<Option<Exp>>,
                  Op: FnMut(&Tok) -> Option<UnOp> {
        let mut ops = Vec::new();
        while let Some(op) = check_op(self.peek()) {
            self.read();
            ops.push(op);
        }
        if let Some(exp) = try!(try_parse_term(self)) {
            let mut exp = exp;
            while let Some(op) = ops.pop() {
                exp = Box::new(Ex::Un(op, exp));
            }
            Ok(Some(exp))
        } else if ops.is_empty() {
            Ok(None)
        } else {
            Err("expected expression after a unary operator")
        }
    }

    fn try_parse_left_assoc_binary_exp<Term, Op>(&mut self,
                                                 mut check_op: Op,
                                                 mut try_parse_term: Term)
            -> ParseResult<Option<Exp>>
            where Term: FnMut(&mut Self) -> ParseResult<Option<Exp>>,
                  Op: FnMut(&Tok) -> Option<BinOp> {
        if let Some(exp) = try!(try_parse_term(self)) {
            let mut exp = exp;
            while let Some(op) = check_op(self.peek()) {
                self.read();
                let exp2 = try!(try!(try_parse_term(self)).ok_or("expected expression"));
                exp = Box::new(Ex::Bin(exp, op, exp2));
            }
            Ok(Some(exp))
        } else {
            Ok(None)
        }
    }

    fn try_parse_right_assoc_binary_exp<Term, Op>(&mut self,
                                                  mut check_op: Op,
                                                  mut try_parse_term: Term)
            -> ParseResult<Option<Exp>>
            where Term: FnMut(&mut Self) -> ParseResult<Option<Exp>>,
                  Op: FnMut(&Tok) -> Option<BinOp> {
        if let Some(exp) = try!(try_parse_term(self)) {
            // store the terms and process in the reverse order
            // e.g. <exp:terms[0].0> <op:terms[0].1> <exp:terms[1].0> <op:terms[1].1> <exp:last_exp>
            let mut exp = exp;
            let mut terms = vec![];
            while let Some(op) = check_op(self.peek()) {
                self.read();
                terms.push((exp, op));
                exp = try!(try!(try_parse_term(self)).ok_or("expected expression"));
            }
            while let Some((exp1, op)) = terms.pop() {
                exp = Box::new(Ex::Bin(exp1, op, exp));
            }
            Ok(Some(exp))
        } else {
            Ok(None)
        }
    }

    fn try_parse_exp(&mut self) -> ParseResult<Option<Exp>> {
        macro_rules! make_check_ops {
            ($($name:ident: $ty:ident { $($tokty:ident::$tok:ident => $op:ident),+ $(,)* };)*) => (
                $(
                    fn $name(tok: &Tok) -> Option<$ty> {
                        match *tok {
                            $(Tok::$tokty($tokty::$tok) => Some($ty::$op),)*
                            _ => None,
                        }
                    }
                )*
            )
        }

        make_check_ops! {
            check_pow_op: BinOp {
                Punct::Caret => Pow,
            };
            check_un_op: UnOp {
                Keyword::Not => Not,
                Punct::Hash => Len,
                Punct::Dash => Neg,
            };
            check_mul_op: BinOp {
                Punct::Star => Mul,
                Punct::Slash => Div,
                Punct::Percent => Mod,
            };
            check_add_op: BinOp {
                Punct::Plus => Add,
                Punct::Dash => Sub,
            };
            check_cat_op: BinOp {
                Punct::DotDot => Cat,
            };
            check_comp_op: BinOp {
                Punct::Lt => Lt,
                Punct::Gt => Gt,
                Punct::LtEq => Le,
                Punct::GtEq => Ge,
                Punct::TildeEq => Ne,
                Punct::EqEq => Eq,
            };
            check_and_op: BinOp {
                Keyword::And => And,
            };
            check_or_op: BinOp {
                Keyword::Or => Or,
            };
        }

        let mut parser = self;
        parser.try_parse_left_assoc_binary_exp(check_or_op, |parser|
            parser.try_parse_left_assoc_binary_exp(check_and_op, |parser|
                parser.try_parse_left_assoc_binary_exp(check_comp_op, |parser|
                    parser.try_parse_right_assoc_binary_exp(check_cat_op, |parser|
                        parser.try_parse_left_assoc_binary_exp(check_add_op, |parser|
                            parser.try_parse_left_assoc_binary_exp(check_mul_op, |parser|
                                parser.try_parse_prefix_unary_exp(check_un_op, |parser|
                                    parser.try_parse_right_assoc_binary_exp(check_pow_op, |parser|
                                        parser.try_parse_atomic_exp()))))))))
    }

    fn parse_exp(&mut self) -> ParseResult<Exp> {
        if let Some(exp) = try!(self.try_parse_exp()) {
            Ok(exp)
        } else {
            Err("expected expression")
        }
    }

    fn try_parse_var(&mut self) -> ParseResult<Option<Var>> {
        if let Some(exp) = try!(self.try_parse_prefix_exp(true)) {
            let var = self.convert_prefix_exp_to_var(exp).unwrap();
            try!(self.try_parse_kailua_type_spec());
            Ok(Some(var))
        } else {
            Ok(None)
        }
    }

    fn parse_var(&mut self) -> ParseResult<Var> {
        if let Some(var) = try!(self.try_parse_var()) {
            Ok(var)
        } else {
            Err("expected variable")
        }
    }

    fn scan_varlist<F>(&mut self, mut f: F) -> ParseResult<()> where F: FnMut(Var) {
        f(try!(self.parse_var()));
        while self.lookahead(Punct::Comma) {
            self.read();
            f(try!(self.parse_var()));
        }
        Ok(())
    }

    fn scan_namelist<F>(&mut self, mut f: F) -> ParseResult<()> where F: FnMut(Name) {
        f(try!(self.parse_name()));
        try!(self.try_parse_kailua_type_spec());
        while self.lookahead(Punct::Comma) {
            self.read();
            try!(self.try_parse_kailua_type_spec());
            f(try!(self.parse_name()));
            try!(self.try_parse_kailua_type_spec());
        }
        Ok(())
    }

    fn scan_explist<F>(&mut self, mut f: F) -> ParseResult<()> where F: FnMut(Exp) {
        f(try!(self.parse_exp()));
        while self.lookahead(Punct::Comma) {
            self.read();
            f(try!(self.parse_exp()));
        }
        Ok(())
    }

    fn try_scan_explist<F>(&mut self, mut f: F) -> ParseResult<bool> where F: FnMut(Exp) {
        if let Some(exp) = try!(self.try_parse_exp()) {
            f(exp);
            while self.lookahead(Punct::Comma) {
                self.read();
                f(try!(self.parse_exp()));
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    // Kailua-specific syntaxes

    fn parse_kailua_mut(&mut self) -> ParseResult<M> {
        match self.read() {
            Tok::Keyword(Keyword::Var) => Ok(M::Var),
            Tok::Keyword(Keyword::Const) => Ok(M::Const),
            tok => {
                self.unread(tok);
                return Ok(M::None);
            }
        }
    }

    fn try_parse_kailua_atomic_kind(&mut self) -> ParseResult<Option<Kind>> {
        let mut kind = match self.read() {
            Tok::Punct(Punct::Ques) => {
                Box::new(K::Dynamic)
            }
            Tok::Punct(Punct::LParen) => {
                let kind = try!(self.parse_kailua_kind());
                try!(self.expect(Punct::RParen));
                kind
            }
            Tok::Keyword(Keyword::Nil)      => Box::new(K::Nil),
            Tok::Keyword(Keyword::Boolean)  => Box::new(K::Boolean),
            Tok::Keyword(Keyword::True)     => Box::new(K::BooleanLit(true)),
            Tok::Keyword(Keyword::False)    => Box::new(K::BooleanLit(false)),
            Tok::Keyword(Keyword::Number)   => Box::new(K::Number),
            Tok::Keyword(Keyword::Integer)  => Box::new(K::Integer),
            Tok::Keyword(Keyword::String)   => Box::new(K::String),
            Tok::Keyword(Keyword::Table)    => Box::new(K::Table),
            Tok::Keyword(Keyword::Function) => Box::new(K::Function),
            Tok::Name(_name) => {
                return Err("unknown type name"); // for now
            }
            Tok::Num(v) if i32::MIN as f64 <= v && v <= i32::MAX as f64 && v.floor() == v => {
                Box::new(K::IntegerLit(v as i32))
            }
            Tok::Str(ref s) => {
                Box::new(K::StringLit(s.to_owned().into()))
            }
            tok => {
                self.unread(tok);
                return Ok(None);
            }
        };

        // a following `?` are equal to `| nil`
        if self.lookahead(Punct::Ques) {
            self.read();
            kind = Box::new(K::Union(vec![kind, Box::new(K::Nil)]));
        }
        Ok(Some(kind))
    }

    fn try_parse_kailua_kind(&mut self) -> ParseResult<Option<Kind>> {
        if let Some(kind) = try!(self.try_parse_kailua_atomic_kind()) {
            let mut kind = kind;
            if self.lookahead(Punct::Pipe) { // A | B | ...
                let mut kinds = vec![kind];
                while self.lookahead(Punct::Pipe) {
                    self.read();
                    let kind2 = try!(try!(self.try_parse_kailua_atomic_kind())
                                     .ok_or("expected type"));
                    kinds.push(kind2);
                }
                kind = Box::new(K::Union(kinds));
            }
            Ok(Some(kind))
        } else {
            Ok(None)
        }
    }

    fn parse_kailua_kind(&mut self) -> ParseResult<Kind> {
        if let Some(kind) = try!(self.try_parse_kailua_kind()) {
            Ok(kind)
        } else {
            Err("expected type")
        }
    }

    fn try_parse_kailua_type_spec(&mut self) -> ParseResult<Option<()>> {
        if self.lookahead(Punct::DashDashColon) {
            self.read();
            loop {
                match self.read() {
                    Tok::Error => return Err("token error"),
                    Tok::Punct(Punct::Newline) => break,
                    _ => {}
                }
            }
            Ok(Some(()))
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_rettype_spec(&mut self) -> ParseResult<Option<()>> {
        if self.lookahead(Punct::DashDashGt) {
            self.read();
            loop {
                match self.read() {
                    Tok::Error => return Err("token error"),
                    Tok::Punct(Punct::Newline) => break,
                    _ => {}
                }
            }
            Ok(Some(()))
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_spec(&mut self) -> ParseResult<Option<Option<Stmt>>> {
        if self.lookahead(Punct::DashDashHash) {
            self.read();

            // assume NAME ":" KIND
            // assume NAME ":" KIND "=" STR (for builtin spec)
            if self.lookahead(Keyword::Assume) {
                self.read();
                let name = try!(self.parse_name());
                try!(self.expect(Punct::Colon));
                let kindm = try!(self.parse_kailua_mut());
                let kind = try!(self.parse_kailua_kind());
                let builtin;
                if self.lookahead(Punct::Eq) {
                    self.read();
                    if let Tok::Str(s) = self.read() {
                        builtin = Some(s.into());
                    } else {
                        return Err("string expected after `assume NAME : KIND =`");
                    }
                } else {
                    builtin = None;
                }
                try!(self.expect(Punct::Newline));
                return Ok(Some(Some(Box::new(St::KailuaAssume(name, kindm, kind, builtin)))));
            }

            loop {
                match self.read() {
                    Tok::Error => return Err("token error"),
                    Tok::Punct(Punct::Newline) => break,
                    _ => {}
                }
            }
            Ok(Some(None))
        } else {
            Ok(None)
        }
    }

    pub fn into_chunk(mut self) -> ParseResult<Block> {
        let chunk = try!(self.parse_block());
        try!(self.expect(EOF));
        Ok(chunk)
    }
}

