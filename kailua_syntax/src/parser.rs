use std::iter;
use std::i32;
use std::collections::HashSet;

use lex::{Tok, Punct, Keyword};
use ast::{Name, Str, Var, Sig, Ex, Exp, UnOp, BinOp, FuncScope, SelfParam, St, Stmt, Block};
use ast::{M, K, Kind, FuncKind, TypeSpec};

pub struct Parser<T> {
    iter: iter::Fuse<T>,

    // the lookahead stream (in this order)
    elided_newline: bool,
    lookahead: Option<Tok>,
    lookahead2: Option<Tok>,
    // ...follows self.iter.next()

    ignore_after_newline: Option<Punct>,
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct ElidedTokens(bool);

impl<T: Iterator<Item=Tok>> Parser<T> {
    pub fn new(iter: T) -> Parser<T> {
        Parser { iter: iter.fuse(), elided_newline: false, lookahead: None, lookahead2: None,
                 ignore_after_newline: None }
    }

    fn read(&mut self) -> (ElidedTokens, Tok) {
        let mut next = self.lookahead.take().or_else(|| self.lookahead2.take())
                                            .or_else(|| self.iter.next());

        let mut elided = self.elided_newline;
        self.elided_newline = false;
        if let Some(meta) = self.ignore_after_newline {
            // lookahead2 is guaranteed to be empty, let's simplify the assumption
            assert_eq!(self.lookahead, None);
            self.lookahead = self.lookahead2.take();

            while next == Some(Tok::Punct(Punct::Newline)) {
                let next2 = self.lookahead.take().or_else(|| self.iter.next());
                if next2 == Some(Tok::Punct(meta)) {
                    // we can ignore them, but we may have another ignorable tokens there
                    elided = true;
                    assert_eq!(self.lookahead, None); // yeah, we are now sure
                    next = self.iter.next();
                } else {
                    // return next, store next2 into the lookahead
                    self.lookahead = next2;
                    break;
                }
            }
        } else {
            // token elision should have been handled by self.end_meta_comment
            assert_eq!(elided, false);
        }

        (ElidedTokens(elided), next.unwrap_or(Tok::EOF))
    }

    fn unread(&mut self, (ElidedTokens(elided), tok): (ElidedTokens, Tok)) {
        assert!(!self.elided_newline && (self.lookahead.is_none() || self.lookahead2.is_none()),
                "at most two lookahead tokens are supported");
        self.elided_newline = elided;
        if let Some(tok) = self.lookahead.take() {
            self.lookahead2 = Some(tok);
        }
        self.lookahead = Some(tok);
    }

    fn peek<'a>(&'a mut self) -> &'a Tok {
        let tok = self.read();
        self.unread(tok);
        self.lookahead.as_ref().unwrap()
    }

    fn expect<Tok: Expectable + ::std::fmt::Debug>(&mut self, tok: Tok) -> ParseResult<()> {
        if !tok.check_token(&self.read().1) { return Err("expect failed"); }
        Ok(())
    }

    fn lookahead<Tok: Expectable>(&mut self, tok: Tok) -> bool {
        tok.check_token(self.peek())
    }

    fn may_expect<Tok: Expectable>(&mut self, tok: Tok) -> bool {
        if self.lookahead(tok) {
            self.read();
            true
        } else {
            false
        }
    }

    // does *not* consume the meta comment!
    fn begin_meta_comment(&mut self, meta: Punct) {
        self.ignore_after_newline = Some(meta);
    }

    // also consumes a newline
    fn end_meta_comment(&mut self, meta: Punct) -> ParseResult<()> {
        assert_eq!(self.ignore_after_newline, Some(meta));
        self.ignore_after_newline = None;

        // a newline token may have been elided. try to reconstruct if possible
        if self.elided_newline {
            // newline (implicitly consumed) - meta - original lookahead
            assert_eq!(self.lookahead2, None);
            self.lookahead2 = self.lookahead.take();
            self.lookahead = Some(Tok::Punct(meta));
            self.elided_newline = false;
        } else {
            try!(self.expect(Punct::Newline));
        }

        Ok(())
    }

    fn try_parse_name(&mut self) -> ParseResult<Option<Name>> {
        let tok = self.read();
        if let Tok::Name(name) = tok.1 {
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
            self.may_expect(Punct::Semicolon);

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

        let presig = try!(self.try_parse_kailua_func_spec());
        if presig.is_some() {
            // limit the possible lookahead
            match self.peek() {
                &Tok::Keyword(Keyword::Function) => {}
                &Tok::Keyword(Keyword::Local) => {} // `local NAME = ...` is filtered later
                _ => return Err("`--v` type spec not followed by function declaration"),
            }
        }

        match self.read() {
            (_, Tok::Keyword(Keyword::Do)) => {
                let block = try!(self.parse_block());
                try!(self.expect(Keyword::End));
                Ok(Some(Box::new(St::Do(block))))
            }

            (_, Tok::Keyword(Keyword::While)) => {
                let cond = try!(self.parse_exp());
                try!(self.expect(Keyword::Do));
                let block = try!(self.parse_block());
                try!(self.expect(Keyword::End));
                Ok(Some(Box::new(St::While(cond, block))))
            }

            (_, Tok::Keyword(Keyword::Repeat)) => {
                let block = try!(self.parse_block());
                try!(self.expect(Keyword::Until));
                let cond = try!(self.parse_exp());
                Ok(Some(Box::new(St::Repeat(block, cond))))
            }

            (_, Tok::Keyword(Keyword::If)) => {
                let cond = try!(self.parse_exp());
                try!(self.expect(Keyword::Then));
                let block = try!(self.parse_block());
                let mut blocks = vec![(cond, block)];
                while self.may_expect(Keyword::Elseif) {
                    let cond = try!(self.parse_exp());
                    try!(self.expect(Keyword::Then));
                    let block = try!(self.parse_block());
                    blocks.push((cond, block));
                }
                let lastblock = if self.may_expect(Keyword::Else) {
                    Some(try!(self.parse_block()))
                } else {
                    None
                };
                try!(self.expect(Keyword::End));
                Ok(Some(Box::new(St::If(blocks, lastblock))))
            }

            (_, Tok::Keyword(Keyword::For)) => {
                let name = try!(self.parse_name());
                try!(self.try_parse_kailua_type_spec());
                match self.read() {
                    // for NAME "=" ...
                    (_, Tok::Punct(Punct::Eq)) => {
                        let start = try!(self.parse_exp());
                        try!(self.expect(Punct::Comma));
                        let end = try!(self.parse_exp());
                        let step = if self.may_expect(Punct::Comma) {
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
                    (_, Tok::Keyword(Keyword::In)) => {
                        Ok(Some(try!(self.parse_stmt_for_in(vec![name]))))
                    }

                    // for NAME [SPEC] "," ... in ...
                    (_, Tok::Punct(Punct::Comma)) => {
                        let mut vars = vec![name.into()];
                        try!(self.scan_namelist(|name| vars.push(name.into())));
                        try!(self.expect(Keyword::In));
                        Ok(Some(try!(self.parse_stmt_for_in(vars))))
                    }

                    _ => return Err("unexpected token after `for NAME`"),
                }
            }

            (_, Tok::Keyword(Keyword::Function)) => {
                let mut names = vec![try!(self.parse_name())];
                let mut selfparam = SelfParam::No;
                while self.may_expect(Punct::Dot) {
                    names.push(try!(self.parse_name()));
                }
                if self.may_expect(Punct::Colon) {
                    names.push(try!(self.parse_name()));
                    selfparam = SelfParam::Yes;
                }
                let (sig, body) = try!(self.parse_func_body(presig));
                if names.len() == 1 {
                    assert!(selfparam == SelfParam::No,
                            "ordinary function cannot have an implicit self");
                    let name = names.pop().unwrap();
                    Ok(Some(Box::new(St::FuncDecl(FuncScope::Global, name, sig, body))))
                } else {
                    Ok(Some(Box::new(St::MethodDecl(names, selfparam, sig, body))))
                }
            }

            (_, Tok::Keyword(Keyword::Local)) => {
                match self.read() {
                    // local function ...
                    (_, Tok::Keyword(Keyword::Function)) => {
                        let name = try!(self.parse_name());
                        let (sig, body) = try!(self.parse_func_body(presig));
                        Ok(Some(Box::new(St::FuncDecl(FuncScope::Local, name, sig, body))))
                    }

                    // local NAME ...
                    tok @ (_, Tok::Name(_)) => {
                        self.unread(tok);

                        // forbid `--v ...` then `local NAME ...`
                        if presig.is_some() {
                            return Err("`--v` type spec not followed by function declaration");
                        }

                        let mut names = Vec::new();
                        let mut exps = Vec::new();
                        try!(self.scan_namelist_with_spec(|namespec| names.push(namespec)));
                        try!(self.try_parse_kailua_type_spec()); // XXX
                        if self.may_expect(Punct::Eq) {
                            try!(self.try_parse_kailua_type_spec()); // XXX
                            try!(self.scan_explist(|exp| exps.push(exp)));
                        }
                        try!(self.try_parse_kailua_type_spec()); // XXX
                        Ok(Some(Box::new(St::Local(names, exps))))
                    }

                    _ => return Err("unexpected token after `local`"),
                }
            }

            (_, Tok::Keyword(Keyword::Return)) => {
                let mut exps = Vec::new();
                try!(self.try_scan_explist(|exp| exps.push(exp)));
                Ok(Some(Box::new(St::Return(exps))))
            }

            (_, Tok::Keyword(Keyword::Break)) => {
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
                            let spec = try!(self.try_parse_kailua_type_spec());
                            let mut lhs = vec![self.make_kailua_typespec(var, spec)];
                            if self.may_expect(Punct::Comma) {
                                try!(self.scan_varlist_with_spec(|varspec| lhs.push(varspec)));
                            }
                            try!(self.expect(Punct::Eq));
                            try!(self.try_parse_kailua_type_spec()); // XXX

                            let mut rhs = Vec::new();
                            try!(self.scan_explist(|exp| rhs.push(exp)));

                            try!(self.try_parse_kailua_type_spec()); // XXX

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

    fn parse_func_body(&mut self, presig: Option<Sig>) -> ParseResult<(Sig, Block)> {
        let mut args = Vec::new();
        let mut variadic = false;
        let returns; // to check the error case

        try!(self.expect(Punct::LParen));
        let mut name = None;
        let mut spec = None;
        match self.read() {
            (_, Tok::Punct(Punct::DotDotDot)) => {
                variadic = true;
                try!(self.try_parse_kailua_type_spec()); // XXX
            }
            (_, Tok::Name(name0)) => {
                name = Some(name0.into());
                spec = try!(self.try_parse_kailua_type_spec());
                while self.may_expect(Punct::Comma) {
                    // try to read the type spec after a comma if there was no prior spec
                    if spec.is_none() { spec = try!(self.try_parse_kailua_type_spec()); }
                    if self.may_expect(Punct::DotDotDot) {
                        try!(self.try_parse_kailua_type_spec()); // XXX
                        variadic = true;
                        break;
                    } else {
                        args.push(self.make_kailua_typespec(name.take().unwrap(), spec.take()));
                        name = Some(try!(self.parse_name()));
                        spec = try!(self.try_parse_kailua_type_spec());
                    }
                }
            }
            tok @ (_, Tok::Punct(Punct::RParen)) => {
                self.unread(tok);
            }
            _ => return Err("unexpected token after `function ... (`")
        }
        try!(self.expect(Punct::RParen));
        // the right parenthesis may be followed by the last type spec
        if let Some(name) = name {
            if spec.is_none() { spec = try!(self.try_parse_kailua_type_spec()); }
            args.push(self.make_kailua_typespec(name, spec));
        }
        returns = try!(self.try_parse_kailua_rettype_spec());

        // if there is a function spec, any mismatching parameter or
        // inline argument type spec is an error
        let sig;
        if let Some(presig) = presig {
            if args.len() != presig.args.len() {
                return Err("mismatching number of arguments in the function spec");
            }
            if variadic != presig.variadic {
                return Err("mismatching variadic argument in the function spec");
            }
            for (arg, sigarg) in args.iter().zip(presig.args.iter()) {
                if arg.base != sigarg.base { // TODO might not be needed
                    return Err("mismatching argument name in the function spec");
                }
                if arg.modf != M::None || arg.kind.is_some() {
                    return Err("inline argument type spec cannot appear with the function spec");
                }
            }
            if returns.is_some() {
                return Err("inline return type spec cannot appear with the function spec");
            }
            sig = presig;
        } else {
            sig = Sig { args: args, variadic: variadic,
                        returns: returns.unwrap_or(Vec::new()) };
        }

        let block = try!(self.parse_block());
        try!(self.expect(Keyword::End));

        Ok((sig, block))
    }

    fn parse_table(&mut self) -> ParseResult<Vec<(Option<Exp>, Exp)>> {
        let mut fields = Vec::new();

        try!(self.expect(Punct::LBrace));
        loop {
            let key;
            let value;
            match self.read() {
                (_, Tok::Punct(Punct::RBrace)) => break,

                (_, Tok::Punct(Punct::LBracket)) => {
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
                (_, Tok::Punct(Punct::Comma)) | (_, Tok::Punct(Punct::Semicolon)) => {}
                (_, Tok::Punct(Punct::RBrace)) => break,
                _ => return Err("unexpected token in the table constructor")
            }
        }

        Ok(fields)
    }

    fn try_parse_args(&mut self) -> ParseResult<Option<Vec<Exp>>> {
        match self.read() {
            (_, Tok::Punct(Punct::LParen)) => {
                let mut args = Vec::new();
                try!(self.try_scan_explist(|exp| args.push(exp)));
                try!(self.expect(Punct::RParen));
                Ok(Some(args))
            }
            (_, Tok::Str(s)) => {
                Ok(Some(vec![Str::from(s).into()]))
            }
            tok @ (_, Tok::Punct(Punct::LBrace)) => {
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
            (_, Tok::Punct(Punct::LParen)) => {
                exp = try!(self.parse_exp());
                try!(self.expect(Punct::RParen));
            }
            (_, Tok::Name(name)) => {
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
                (_, Tok::Punct(Punct::Dot)) => {
                    if let Tok::Name(name) = self.read().1 {
                        exp = Box::new(Ex::Index(exp, Str::from(name).into()));
                    } else {
                        return Err("unexpected token after `<expression> .`");
                    }
                }

                // prefixexp "[" ...
                (_, Tok::Punct(Punct::LBracket)) => {
                    let exp2 = try!(self.parse_exp());
                    try!(self.expect(Punct::RBracket));
                    exp = Box::new(Ex::Index(exp, exp2));
                }

                // prefixexp ":" ...
                (_, Tok::Punct(Punct::Colon)) if !var_only => {
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
        let presig = try!(self.try_parse_kailua_func_spec());
        if presig.is_some() && !self.lookahead(Keyword::Function) {
            // limit the possible lookahead
            return Err("`--v` type specifier not followed by function literal");
        }

        match self.read() {
            (_, Tok::Keyword(Keyword::Nil)) => Ok(Some(Box::new(Ex::Nil))),
            (_, Tok::Keyword(Keyword::False)) => Ok(Some(Box::new(Ex::False))),
            (_, Tok::Keyword(Keyword::True)) => Ok(Some(Box::new(Ex::True))),
            (_, Tok::Num(v)) => Ok(Some(Box::new(Ex::Num(v)))),
            (_, Tok::Str(s)) => Ok(Some(Box::new(Ex::Str(s.into())))),
            (_, Tok::Punct(Punct::DotDotDot)) => Ok(Some(Box::new(Ex::Varargs))),

            (_, Tok::Keyword(Keyword::Function)) => {
                let (sig, body) = try!(self.parse_func_body(presig));
                Ok(Some(Box::new(Ex::Func(sig, body))))
            }

            tok @ (_, Tok::Punct(Punct::LBrace)) => {
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

    fn scan_varlist_with_spec<F>(&mut self, mut f: F) -> ParseResult<()>
            where F: FnMut(TypeSpec<Var>) {
        let mut var = try!(self.parse_var());
        let mut spec = try!(self.try_parse_kailua_type_spec());
        while self.may_expect(Punct::Comma) {
            // try to read the type spec after a comma if there was no prior spec
            if spec.is_none() { spec = try!(self.try_parse_kailua_type_spec()); }
            f(self.make_kailua_typespec(var, spec));
            var = try!(self.parse_var());
            spec = try!(self.try_parse_kailua_type_spec());
        }
        f(self.make_kailua_typespec(var, spec));
        Ok(())
    }

    fn scan_namelist<F>(&mut self, mut f: F) -> ParseResult<()> where F: FnMut(Name) {
        f(try!(self.parse_name()));
        while self.may_expect(Punct::Comma) {
            f(try!(self.parse_name()));
        }
        Ok(())
    }

    fn scan_namelist_with_spec<F>(&mut self, mut f: F) -> ParseResult<()>
            where F: FnMut(TypeSpec<Name>) {
        let mut name = try!(self.parse_name());
        let mut spec = try!(self.try_parse_kailua_type_spec());
        while self.may_expect(Punct::Comma) {
            // try to read the type spec after a comma if there was no prior spec
            if spec.is_none() { spec = try!(self.try_parse_kailua_type_spec()); }
            f(self.make_kailua_typespec(name, spec));
            name = try!(self.parse_name());
            spec = try!(self.try_parse_kailua_type_spec());
        }
        f(self.make_kailua_typespec(name, spec));
        Ok(())
    }

    fn scan_explist<F>(&mut self, mut f: F) -> ParseResult<()> where F: FnMut(Exp) {
        f(try!(self.parse_exp()));
        while self.may_expect(Punct::Comma) {
            f(try!(self.parse_exp()));
        }
        Ok(())
    }

    fn try_scan_explist<F>(&mut self, mut f: F) -> ParseResult<bool> where F: FnMut(Exp) {
        if let Some(exp) = try!(self.try_parse_exp()) {
            f(exp);
            while self.may_expect(Punct::Comma) {
                f(try!(self.parse_exp()));
            }
            Ok(true)
        } else {
            Ok(false)
        }
    }

    // Kailua-specific syntaxes

    fn make_kailua_typespec<Base>(&self, base: Base, spec: Option<(M, Kind)>) -> TypeSpec<Base> {
        if let Some((modf, kind)) = spec {
            TypeSpec { base: base, modf: modf, kind: Some(kind) }
        } else {
            TypeSpec { base: base, modf: M::None, kind: None }
        }
    }

    fn parse_kailua_mod(&mut self) -> ParseResult<M> {
        match self.read() {
            (_, Tok::Keyword(Keyword::Var)) => Ok(M::Var),
            (_, Tok::Keyword(Keyword::Const)) => Ok(M::Const),
            tok => {
                self.unread(tok);
                return Ok(M::None);
            }
        }
    }

    // may contain ellipsis, the caller is expected to error on unwanted cases
    fn parse_kailua_kindlist(&mut self) -> ParseResult<(bool /*variadic*/, Vec<Kind>)> {
        let mut variadic = false;
        let mut kinds = Vec::new();
        if self.may_expect(Punct::DotDotDot) { // "..."
            variadic = true;
        } else { // KIND {"," KIND} ["," "..."] or empty
            // try to read the first KIND
            let kind = match try!(self.try_parse_kailua_kind()) {
                Some(kind) => kind,
                None => return Ok((variadic, kinds)),
            };
            kinds.push(kind);
            while self.may_expect(Punct::Comma) {
                if self.may_expect(Punct::DotDotDot) {
                    variadic = true;
                    break;
                }
                let kind = try!(self.parse_kailua_kind());
                kinds.push(kind);
            }
        }
        Ok((variadic, kinds))
    }

    // may contain ellipsis, the caller is expected to error on unwanted cases
    fn parse_kailua_namekindlist(&mut self) -> ParseResult<(bool /*variadic*/,
                                                            Vec<TypeSpec<Name>>)> {
        let mut variadic = false;
        let mut specs = Vec::new();
        if self.may_expect(Punct::DotDotDot) { // "..."
            variadic = true;
        } else { // NAME ":" KIND {"," NAME ":" KIND} ["," "..."] or empty
            let name = match try!(self.try_parse_name()) {
                Some(name) => name,
                None => return Ok((variadic, specs)),
            };
            try!(self.expect(Punct::Colon));
            let modf = try!(self.parse_kailua_mod());
            let kind = try!(self.parse_kailua_kind());
            specs.push(self.make_kailua_typespec(name, Some((modf, kind))));
            while self.may_expect(Punct::Comma) {
                if self.may_expect(Punct::DotDotDot) {
                    variadic = true;
                    break;
                }
                let name = try!(self.parse_name());
                try!(self.expect(Punct::Colon));
                let modf = try!(self.parse_kailua_mod());
                let kind = try!(self.parse_kailua_kind());
                specs.push(self.make_kailua_typespec(name, Some((modf, kind))));
            }
        }
        Ok((variadic, specs))
    }

    fn parse_kailua_funckind(&mut self) -> ParseResult<FuncKind> {
        try!(self.expect(Punct::LParen));
        let (variadic, args) = try!(self.parse_kailua_kindlist());
        try!(self.expect(Punct::RParen));
        let returns = if self.may_expect(Punct::DashGt) {
            // "(" ... ")" "->" ...
            try!(self.parse_kailua_kind_seq())
        } else {
            // "(" ... ")"
            Vec::new()
        };
        Ok(FuncKind { args: args, variadic: variadic, returns: returns })
    }

    fn try_parse_kailua_atomic_kind_seq(&mut self) -> ParseResult<Option<Vec<Kind>>> {
        let mut kind = match self.read() {
            (_, Tok::Keyword(Keyword::Function)) => {
                // either a "function" type or a list of function signatures
                if self.lookahead(Punct::LParen) {
                    // function "(" ... ")" ["->" ...] {"&" "(" ... ")" ["->" ...]}
                    let mut funcs = vec![try!(self.parse_kailua_funckind())];
                    while self.may_expect(Punct::Amp) {
                        funcs.push(try!(self.parse_kailua_funckind()));
                    }
                    // cannot be followed by postfix operators
                    return Ok(Some(vec![Box::new(K::Func(funcs))]));
                } else {
                    Box::new(K::Function)
                }
            }

            (_, Tok::Punct(Punct::LParen)) => {
                let (variadic, mut args) = try!(self.parse_kailua_kindlist());
                try!(self.expect(Punct::RParen));
                if variadic {
                    return Err("variadic argument can only be used as a function argument");
                } else if args.len() != 1 {
                    // cannot be followed by postfix operators - XXX really?
                    return Ok(Some(args));
                } else {
                    args.pop().unwrap()
                }
            }

            (_, Tok::Punct(Punct::LBrace)) => {
                match self.read() {
                    // "{" NAME ...
                    tok @ (_, Tok::Name(_)) => {
                        self.unread(tok);
                        let mut seen = HashSet::new();
                        let mut fields = Vec::new();
                        loop {
                            let name = Str::from(try!(self.parse_name()));
                            if !seen.insert(name.clone()) {
                                return Err("duplicate record field in the type spec");
                            }
                            try!(self.expect(Punct::Eq));
                            let modf = try!(self.parse_kailua_mod());
                            let kind = try!(self.parse_kailua_kind());
                            fields.push((name, modf, kind));
                            // ";" - "," - ";" "}" - "," "}" - "}"
                            match self.read() {
                                (_, Tok::Punct(Punct::Comma)) => {}
                                (_, Tok::Punct(Punct::Semicolon)) => {}
                                (_, Tok::Punct(Punct::RBrace)) => break,
                                _ => return Err("unexpected token in the table type spec"),
                            }
                            if self.may_expect(Punct::RBrace) { break; }
                        }
                        Box::new(K::Record(fields))
                    }

                    // "{" "}" ...
                    (_, Tok::Punct(Punct::RBrace)) => {
                        Box::new(K::Record(Vec::new()))
                    }

                    _ => return Err("invalid table type"),
                }
            }

            (_, Tok::Punct(Punct::Ques))         => Box::new(K::Dynamic),
            (_, Tok::Keyword(Keyword::Nil))      => Box::new(K::Nil),
            (_, Tok::Keyword(Keyword::Boolean))  => Box::new(K::Boolean),
            (_, Tok::Keyword(Keyword::True))     => Box::new(K::BooleanLit(true)),
            (_, Tok::Keyword(Keyword::False))    => Box::new(K::BooleanLit(false)),
            (_, Tok::Keyword(Keyword::Number))   => Box::new(K::Number),
            (_, Tok::Keyword(Keyword::Integer))  => Box::new(K::Integer),
            (_, Tok::Keyword(Keyword::String))   => Box::new(K::String),
            (_, Tok::Keyword(Keyword::Table))    => Box::new(K::Table),

            (_, Tok::Name(_name)) => {
                return Err("unknown type name"); // for now
            }

            (_, Tok::Num(v)) if i32::MIN as f64 <= v && v <= i32::MAX as f64 &&
                                v.floor() == v => {
                Box::new(K::IntegerLit(v as i32))
            }

            (_, Tok::Str(ref s)) => {
                Box::new(K::StringLit(s.to_owned().into()))
            }

            tok => {
                self.unread(tok);
                return Ok(None);
            }
        };

        // a following `?` are equal to `| nil`
        if self.may_expect(Punct::Ques) {
            kind = Box::new(K::Union(vec![kind, Box::new(K::Nil)]));
        }
        Ok(Some(vec![kind]))
    }

    fn try_parse_kailua_kind_seq(&mut self) -> ParseResult<Option<Vec<Kind>>> {
        if let Some(mut kindseq) = try!(self.try_parse_kailua_atomic_kind_seq()) {
            if kindseq.len() != 1 { return Ok(Some(kindseq)); }
            let mut kind = kindseq.pop().unwrap();
            if self.lookahead(Punct::Pipe) { // A | B | ...
                let mut kinds = vec![kind];
                while self.may_expect(Punct::Pipe) {
                    let mut kindseq2 = try!(try!(self.try_parse_kailua_atomic_kind_seq())
                                            .ok_or("expected type"));
                    if kindseq2.len() != 1 {
                        return Err("a sequence of types cannot be inside a union");
                    }
                    kinds.push(kindseq2.pop().unwrap());
                }
                kind = Box::new(K::Union(kinds));
            }
            Ok(Some(vec![kind]))
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_kind(&mut self) -> ParseResult<Option<Kind>> {
        if let Some(mut kindseq) = try!(self.try_parse_kailua_kind_seq()) {
            if kindseq.len() == 1 {
                Ok(Some(kindseq.pop().unwrap()))
            } else {
                Err("expected a single type, not type sequence")
            }
        } else {
            Ok(None)
        }
    }

    fn parse_kailua_kind_seq(&mut self) -> ParseResult<Vec<Kind>> {
        if let Some(kindseq) = try!(self.try_parse_kailua_kind_seq()) {
            Ok(kindseq)
        } else {
            Err("expected a single type or type sequence")
        }
    }

    fn parse_kailua_kind(&mut self) -> ParseResult<Kind> {
        if let Some(kind) = try!(self.try_parse_kailua_kind()) {
            Ok(kind)
        } else {
            Err("expected a single type")
        }
    }

    fn try_parse_kailua_type_spec(&mut self) -> ParseResult<Option<(M, Kind)>> {
        if self.may_expect(Punct::DashDashColon) {
            // allow for `--: { a = foo,
            //            --:   b = bar }`
            self.begin_meta_comment(Punct::DashDashColon);
            let modf = try!(self.parse_kailua_mod());
            let kind = try!(self.parse_kailua_kind());
            // allow for `--: last type --> return type` in the function decl
            if !self.lookahead(Punct::DashDashGt) {
                try!(self.end_meta_comment(Punct::DashDashColon));
            } else {
                assert_eq!(self.ignore_after_newline, Some(Punct::DashDashColon));
                self.ignore_after_newline = None;
                // it is possible to have `--: { a = foo,
                //                         --:   b = bar }
                //                         --: --> return_type`. seems harmless though.
                self.elided_newline = false;
            }
            Ok(Some((modf, kind)))
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_rettype_spec(&mut self) -> ParseResult<Option<Vec<Kind>>> {
        if self.may_expect(Punct::DashDashGt) {
            self.begin_meta_comment(Punct::DashDashGt);
            let kinds = try!(self.parse_kailua_kind_seq());
            try!(self.end_meta_comment(Punct::DashDashGt));

            Ok(Some(kinds))
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_func_spec(&mut self) -> ParseResult<Option<Sig>> {
        if self.may_expect(Punct::DashDashV) {
            // "(" [NAME ":" KIND] {"," NAME ":" KIND} ["," "..."] ")" ["->" KIND]
            self.begin_meta_comment(Punct::DashDashV);
            try!(self.expect(Punct::LParen));
            let (variadic, args) = try!(self.parse_kailua_namekindlist());
            try!(self.expect(Punct::RParen));
            let returns = if self.may_expect(Punct::DashGt) {
                try!(self.parse_kailua_kind_seq())
            } else {
                Vec::new()
            };
            try!(self.end_meta_comment(Punct::DashDashV));

            Ok(Some(Sig { args: args, variadic: variadic, returns: returns }))
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_spec(&mut self) -> ParseResult<Option<Option<Stmt>>> {
        if self.may_expect(Punct::DashDashHash) {
            // assume NAME ":" MODF KIND
            // assume NAME ":" MODF KIND "=" STR (for builtin spec)
            if self.may_expect(Keyword::Assume) {
                // should be inside here, empty `--#` is valid
                self.begin_meta_comment(Punct::DashDashHash);
                let name = try!(self.parse_name());
                try!(self.expect(Punct::Colon));
                let modf = try!(self.parse_kailua_mod());
                let kind = try!(self.parse_kailua_kind());

                let builtin;
                if self.may_expect(Punct::Eq) {
                    if let Tok::Str(s) = self.read().1 {
                        builtin = Some(s.into());
                    } else {
                        return Err("string expected after `assume NAME : KIND =`");
                    }
                } else {
                    builtin = None;
                }

                try!(self.end_meta_comment(Punct::DashDashHash));
                return Ok(Some(Some(Box::new(St::KailuaAssume(name, modf, kind, builtin)))));
            }

            loop {
                match self.read() {
                    (_, Tok::Error) => return Err("token error"),
                    (_, Tok::Punct(Punct::Newline)) => break,
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

