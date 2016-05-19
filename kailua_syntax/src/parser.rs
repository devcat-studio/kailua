use std::iter;
use std::i32;
use std::fmt;
use std::collections::{hash_map, HashMap};

use kailua_diag as diag;
use kailua_diag::{Pos, Span, Spanned, WithLoc, Report, Reporter};
use lex::{Tok, Punct, Keyword};
use ast::{Name, Str, Var, Sig, Ex, Exp, UnOp, BinOp, FuncScope, SelfParam, St, Stmt, Block};
use ast::{M, K, Kind, SlotKind, FuncKind, TypeSpec};

pub struct Parser<'a, T> {
    iter: iter::Fuse<T>,

    // the lookahead stream (in this order)
    elided_newline: bool,
    lookahead: Option<Spanned<Tok>>,
    lookahead2: Option<Spanned<Tok>>,
    // ...follows self.iter.next()

    // the spans for the most recent `read()` tokens
    last_span: Span,
    last_span2: Span,

    ignore_after_newline: Option<Punct>,
    report: &'a Report,
}

pub type Error = ::lex::Error;

pub type ParseResult<T> = diag::Result<T>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct EOF; // a placeholder arg to `expect`

impl fmt::Display for EOF {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt("end of file", f)
    }
}

trait Expectable: fmt::Display {
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

impl<'a, T: Iterator<Item=Spanned<Tok>>> Parser<'a, T> {
    pub fn new(iter: T, report: &'a Report) -> Parser<'a, T> {
        let mut parser = Parser {
            iter: iter.fuse(),
            elided_newline: false,
            lookahead: None,
            lookahead2: None,
            last_span: Span::dummy(),
            last_span2: Span::dummy(),
            ignore_after_newline: None,
            report: report,
        };
        // read the first token and fill the last_span
        let first = parser._next().expect("lexer gave no token");
        parser.last_span = first.span.begin().into();
        parser.lookahead = Some(first);
        parser
    }

    fn _next(&mut self) -> Option<Spanned<Tok>> {
        let next = self.iter.next();
        if false { // useful for debugging
            if let Some(ref t) = next {
                println!("got {:?}", *t);
                let _ = self.report.note(t.span, format!("got {:?}", t.base)).done();
            }
        }
        next
    }

    fn _read(&mut self) -> (ElidedTokens, Spanned<Tok>) {
        let mut next = self.lookahead.take().or_else(|| self.lookahead2.take())
                                            .or_else(|| self._next());

        let mut elided = self.elided_newline;
        self.elided_newline = false;
        if let Some(meta) = self.ignore_after_newline {
            // lookahead2 can definitely be made empty, let's simplify the assumption
            assert_eq!(self.lookahead, None);
            self.lookahead = self.lookahead2.take();

            while next.as_ref().map(|t| &t.base) == Some(&Tok::Punct(Punct::Newline)) {
                let next2 = self.lookahead.take().or_else(|| self._next());
                if next2.as_ref().map(|t| &t.base) == Some(&Tok::Punct(meta)) {
                    // we can ignore them, but we may have another ignorable tokens there
                    elided = true;
                    assert_eq!(self.lookahead, None); // yeah, we are now sure
                    next = self._next();
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

        (ElidedTokens(elided), next.expect("Parser::read tried to read past EOF"))
    }

    fn read(&mut self) -> diag::Result<(ElidedTokens, Spanned<Tok>)> {
        let (elided, next) = self._read();
        if next.base == Tok::Error {
            // the lexer should have issued an error already
            Err("lexer error")
        } else {
            self.last_span2 = self.last_span;
            self.last_span = next.span;
            Ok((elided, next))
        }
    }

    fn _unread(&mut self, (ElidedTokens(elided), tok): (ElidedTokens, Spanned<Tok>)) {
        assert!(self.lookahead.is_none() || self.lookahead2.is_none(),
                "at most two lookahead tokens are supported");

        // this overwrites the prior value, i.e. newlines elided between the first lookahead
        // and the second lookahead will be ignored.
        // this is fine, because the secondary lookahead is only used
        // in `try_parse_kailua_atomic_kind_seq` and is thus fine to be ignored.
        self.elided_newline = elided;
        if let Some(tok) = self.lookahead.take() {
            self.lookahead2 = Some(tok);
        }
        self.lookahead = Some(tok);
    }

    fn unread(&mut self, tok: (ElidedTokens, Spanned<Tok>)) {
        self._unread(tok);
        self.last_span = self.last_span2;
        self.last_span2 = Span::dummy();
    }

    fn peek<'b>(&'b mut self) -> &'b Spanned<Tok> {
        if self.lookahead.is_none() {
            // do not use `self.read()`; it may fail on an error token, but we can delay that
            let tok = self._read();
            self._unread(tok);
        }
        self.lookahead.as_ref().unwrap()
    }

    fn expect<Tok: Expectable>(&mut self, tok: Tok) -> ParseResult<()> {
        let read = try!(self.read()).1;
        if !tok.check_token(&read.base) {
            self.report.fatal(read.span, format!("Expected {}, got {}", tok, read.base)).done()
        } else {
            Ok(())
        }
    }

    fn lookahead<Tok: Expectable>(&mut self, tok: Tok) -> bool {
        tok.check_token(self.peek())
    }

    fn may_expect<Tok: Expectable>(&mut self, tok: Tok) -> bool {
        if self.lookahead(tok) {
            let (_elided, next) = self._read();
            self.last_span2 = self.last_span;
            self.last_span = next.span;
            true
        } else {
            false
        }
    }

    fn pos(&mut self) -> Pos {
        self.peek().span.begin()
    }

    fn last_pos(&self) -> Pos {
        assert!(!self.last_span.is_dummy(), "Parser::last_pos has lost prior span information");
        self.last_span.end()
    }

    // does *not* consume the meta comment!
    fn begin_meta_comment(&mut self, meta: Punct) {
        self.ignore_after_newline = Some(meta);
    }

    // also consumes a newline
    fn end_meta_comment(&mut self, meta: Punct) -> ParseResult<()> {
        assert_eq!(self.ignore_after_newline, Some(meta));

        // if the next token (sans elided newline-meta pairs) is a newline, it's the end.
        // otherwise a newline token may have been elided; try to reconstruct if possible.
        let elided = self.elided_newline;
        self.elided_newline = false; // may_expect requires this
        if !self.may_expect(Punct::Newline) {
            // ...and may_expect may have set this again!
            let elided = elided || self.elided_newline;
            self.elided_newline = false;

            if !elided {
                let (span, msg) = {
                    let next = self.peek();
                    (next.span, format!("Expected a newline, got {}", next.base))
                };
                try!(self.report.error(span, msg).done());

                // skip until the end of meta block if we continue
                while try!(self.read()).1.base != Tok::Punct(Punct::Newline) {}
                self.ignore_after_newline = None;
                return Ok(());
            }

            // newline (implicitly consumed) - meta - original lookahead
            assert_eq!(self.lookahead2, None);
            self.lookahead2 = self.lookahead.take();
            self.lookahead = Some(Tok::Punct(meta).with_loc(Span::dummy()));
        }

        self.ignore_after_newline = None;
        Ok(())
    }

    fn dummy_kind(&self) -> Spanned<Kind> {
        Box::new(K::Dynamic).with_loc(Span::dummy())
    }

    fn try_parse_name(&mut self) -> ParseResult<Option<Spanned<Name>>> {
        let tok = try!(self.read());
        if let Tok::Name(name) = tok.1.base {
            let name: Name = name.into();
            Ok(Some(name.with_loc(tok.1.span)))
        } else {
            self.unread(tok);
            Ok(None)
        }
    }

    fn parse_name(&mut self) -> ParseResult<Spanned<Name>> {
        let tok = try!(self.read());
        if let Tok::Name(name) = tok.1.base {
            let name: Name = name.into();
            Ok(name.with_loc(tok.1.span))
        } else {
            self.report.fatal(tok.1.span, format!("Expected a name, got {}", tok.1.base)).done()
        }
    }

    fn parse_block(&mut self) -> ParseResult<Spanned<Block>> {
        let begin = self.pos();
        let mut stmts = Vec::new();
        while let Some(stmt) = try!(self.try_parse_stmt()) {
            self.may_expect(Punct::Semicolon);

            // if the statement is the final one, stop parsing
            let last = match &*stmt.base {
                &St::Return(..) | &St::Break => true,
                _ => false,
            };

            stmts.push(stmt);
            if last { break; }
        }
        Ok(stmts.with_loc(begin..self.last_pos()))
    }

    fn try_parse_stmt(&mut self) -> ParseResult<Option<Spanned<Stmt>>> {
        // if there exists a spec stmt return it first.
        // a spec may be empty, so loop until no spec exists or a spec is found.
        loop {
            match try!(self.try_parse_kailua_spec()) {
                Some(Some(spec)) => return Ok(Some(spec)),
                Some(None) => continue,
                None => break,
            }
        }

        let begin = self.pos();

        let presig = try!(self.try_parse_kailua_func_spec());
        if let Some(ref presig) = presig {
            // limit the possible lookahead
            let allowed = match self.peek().base {
                Tok::Keyword(Keyword::Function) => true,
                Tok::Keyword(Keyword::Local) => true, // `local NAME = ...` is filtered later
                _ => false,
            };
            if !allowed {
                try!(self.report.error(presig.span, "No function declaration after \
                                                     the function specification")
                                .done());
            }
        }

        let stmt = match try!(self.read()) {
            (_, Spanned { base: Tok::Keyword(Keyword::Do), .. }) => {
                let block = try!(self.parse_block());
                try!(self.expect(Keyword::End));
                Box::new(St::Do(block))
            }

            (_, Spanned { base: Tok::Keyword(Keyword::While), .. }) => {
                let cond = try!(self.parse_exp());
                try!(self.expect(Keyword::Do));
                let block = try!(self.parse_block());
                try!(self.expect(Keyword::End));
                Box::new(St::While(cond, block))
            }

            (_, Spanned { base: Tok::Keyword(Keyword::Repeat), .. }) => {
                let block = try!(self.parse_block());
                try!(self.expect(Keyword::Until));
                let cond = try!(self.parse_exp());
                Box::new(St::Repeat(block, cond))
            }

            (_, Spanned { base: Tok::Keyword(Keyword::If), .. }) => {
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
                Box::new(St::If(blocks, lastblock))
            }

            (_, Spanned { base: Tok::Keyword(Keyword::For), .. }) => {
                let name = try!(self.parse_name());
                try!(self.try_parse_kailua_type_spec());
                match try!(self.read()) {
                    // for NAME "=" ...
                    (_, Spanned { base: Tok::Punct(Punct::Eq), .. }) => {
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
                        Box::new(St::For(name, start, end, step, block))
                    }

                    // for NAME in ...
                    (_, Spanned { base: Tok::Keyword(Keyword::In), .. }) => {
                        try!(self.parse_stmt_for_in(vec![name]))
                    }

                    // for NAME [SPEC] "," ... in ...
                    (_, Spanned { base: Tok::Punct(Punct::Comma), .. }) => {
                        let mut vars = vec![name];
                        try!(self.scan_namelist(|name| vars.push(name)));
                        try!(self.expect(Keyword::In));
                        try!(self.parse_stmt_for_in(vars))
                    }

                    (_, tok) => {
                        return self.report.fatal(tok.span,
                                                 "Expected `=`, `,`, `in` or `--:` \
                                                  after `for NAME`")
                                          .done();
                    }
                }
            }

            (_, Spanned { base: Tok::Keyword(Keyword::Function), .. }) => {
                let mut names = vec![try!(self.parse_name())];
                let mut selfparam = None;
                while self.may_expect(Punct::Dot) {
                    names.push(try!(self.parse_name()));
                }
                if self.may_expect(Punct::Colon) {
                    names.push(try!(self.parse_name()));
                    selfparam = Some(SelfParam.with_loc(Span::dummy()));
                }
                let (sig, body) = try!(self.parse_func_body(presig));
                if names.len() == 1 {
                    assert!(selfparam.is_none(), "ordinary function cannot have an implicit self");
                    let name = names.pop().unwrap();
                    Box::new(St::FuncDecl(FuncScope::Global, name, sig, body))
                } else {
                    Box::new(St::MethodDecl(names, selfparam, sig, body))
                }
            }

            (_, Spanned { base: Tok::Keyword(Keyword::Local), .. }) => {
                match try!(self.read()) {
                    // local function ...
                    (_, Spanned { base: Tok::Keyword(Keyword::Function), .. }) => {
                        let name = try!(self.parse_name());
                        let (sig, body) = try!(self.parse_func_body(presig));
                        Box::new(St::FuncDecl(FuncScope::Local, name, sig, body))
                    }

                    // local NAME ...
                    tok @ (_, Spanned { base: Tok::Name(_), .. }) => {
                        self.unread(tok);

                        // forbid `--v ...` then `local NAME ...`
                        if let Some(ref presig) = presig {
                            try!(self.report.error(presig.span,
                                                   "No function declaration after \
                                                    the function specification")
                                            .done());
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
                        Box::new(St::Local(names, exps))
                    }

                    (_, tok) => {
                        return self.report.fatal(tok.span,
                                                 "Expected a name or `function` \
                                                  after `local`")
                                          .done();
                    }
                }
            }

            (_, Spanned { base: Tok::Keyword(Keyword::Return), .. }) => {
                let mut exps = Vec::new();
                try!(self.try_scan_explist(|exp| exps.push(exp)));
                Box::new(St::Return(exps))
            }

            (_, Spanned { base: Tok::Keyword(Keyword::Break), .. }) => {
                Box::new(St::Break)
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

                            Box::new(St::Assign(lhs, rhs))
                        }

                        // prefixexp
                        Err(exp) => {
                            Box::new(St::Void(exp))
                        }
                    }
                } else {
                    return Ok(None);
                }
            }
        };

        Ok(Some(stmt.with_loc(begin..self.last_pos())))
    }

    fn parse_stmt_for_in(&mut self, names: Vec<Spanned<Name>>) -> ParseResult<Stmt> {
        let mut exps = Vec::new();
        try!(self.scan_explist(|exp| exps.push(exp)));
        try!(self.expect(Keyword::Do));
        let block = try!(self.parse_block());
        try!(self.expect(Keyword::End));
        Ok(Box::new(St::ForIn(names, exps, block)))
    }

    fn parse_func_body(&mut self,
                       presig: Option<Spanned<Sig>>) -> ParseResult<(Sig, Spanned<Block>)> {
        let mut args = Vec::new();
        let mut varargs = None;
        let returns; // to check the error case

        // the type specifier may come at any reasonable positions:
        //
        // 1) `name <spec> ,`
        // 2) `name , <spec>`
        // 3) `name ) <spec>`
        // 4) `... <spec> )`
        // 5) `... ) <spec>`
        //
        // each parsing code is scattered throughout the following block.

        try!(self.expect(Punct::LParen));
        let mut name = None;
        let mut spec = None;
        let mut variadic = false;
        match try!(self.read()) {
            (_, Spanned { base: Tok::Punct(Punct::DotDotDot), .. }) => {
                variadic = true;
            }
            (_, Spanned { base: Tok::Name(name0), span }) => {
                let name0: Name = name0.into();
                name = Some(name0.with_loc(span));
                spec = try!(self.try_parse_kailua_type_spec()); // 1)
                while self.may_expect(Punct::Comma) {
                    // try to read the type spec after a comma if there was no prior spec
                    if spec.is_none() { spec = try!(self.try_parse_kailua_type_spec()); }
                    args.push(self.make_kailua_typespec(name.take().unwrap(), spec.take())); // 2)
                    if self.may_expect(Punct::DotDotDot) {
                        variadic = true;
                        break;
                    } else {
                        name = Some(try!(self.parse_name()));
                        spec = try!(self.try_parse_kailua_type_spec()); // 1)
                    }
                }
            }
            tok @ (_, Spanned { base: Tok::Punct(Punct::RParen), .. }) => {
                self.unread(tok);
            }
            (_, tok) => {
                return self.report.fatal(tok.span, "Expected a name, `)` or `...`").done();
            }
        }
        if variadic {
            // we've already read `...` and flushed the name-spec pair
            let mut varargs_ = try!(self.try_parse_kailua_type_spec_with_spanned_modf()); // 4)
            try!(self.expect(Punct::RParen));
            if varargs_.is_none() {
                varargs_ = try!(self.try_parse_kailua_type_spec_with_spanned_modf()); // 5)
            }
            if let Some((m, kind)) = varargs_ {
                if m.base != M::None {
                    try!(self.report.error(m.span, "Variadic argument specifier \
                                                    cannot have modifiers")
                                    .done());
                }
                varargs = Some(Some(kind));
            } else {
                varargs = Some(None);
            }
        } else {
            try!(self.expect(Punct::RParen));
            if let Some(name) = name {
                // the last type spec may follow the right parenthesis
                if spec.is_none() { spec = try!(self.try_parse_kailua_type_spec()); } // 3)
                args.push(self.make_kailua_typespec(name, spec));
            }
        }
        returns = try!(self.try_parse_kailua_rettype_spec());

        // if there is a function spec, any mismatching parameter or
        // inline argument type spec is an error
        let sig;
        if let Some(presig) = presig {
            let presig = presig.base;
            if args.len() != presig.args.len() {
                return Err("mismatching number of arguments in the function spec");
            }
            match (&varargs, &presig.varargs) {
                (&Some(_), &None) | (&None, &Some(_)) => {
                    return Err("mismatching variadic arguments in the function spec");
                }
                (&Some(Some(_)), &Some(_)) => {
                    return Err("inline variadic argument type spec cannot appear \
                                with the function spec");
                }
                (_, _) => {}
            }
            for (arg, sigarg) in args.iter().zip(presig.args.iter()) {
                if arg.base.base != sigarg.base.base { // TODO might not be needed
                    return Err("mismatching argument name in the function spec");
                }
                if arg.modf != M::None || arg.kind.is_some() {
                    return Err("inline argument type spec cannot appear with the function spec");
                }
            }
            if let Some(returns) = returns {
                try!(self.report.error(returns.span,
                                       "Inline return type spec cannot appear \
                                        with the function spec")
                                .done());
            }
            sig = presig;
        } else {
            sig = Sig { args: args, varargs: varargs, returns: returns.map(|tt| tt.base) };
        }

        let block = try!(self.parse_block());
        try!(self.expect(Keyword::End));

        Ok((sig, block))
    }

    fn parse_table(&mut self) -> ParseResult<Vec<(Option<Spanned<Exp>>, Spanned<Exp>)>> {
        let mut fields = Vec::new();

        try!(self.expect(Punct::LBrace));
        loop {
            let key;
            let value;
            match try!(self.read()) {
                (_, Spanned { base: Tok::Punct(Punct::RBrace), .. }) => break,

                (_, Spanned { base: Tok::Punct(Punct::LBracket), .. }) => {
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
                        let span = exp.span;
                        match *exp.base {
                            Ex::Var(name) => Ok(name),
                            exp => Err(Box::new(exp).with_loc(span)),
                        }
                    } else {
                        Err(exp)
                    };
                    match name_or_exp {
                        Ok(name) => {
                            let s: Str = name.base.into();
                            key = Some(Box::new(Ex::Str(s)).with_loc(name.span));
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

            match try!(self.read()) {
                (_, Spanned { base: Tok::Punct(Punct::Comma), .. }) |
                (_, Spanned { base: Tok::Punct(Punct::Semicolon), .. }) => {}
                (_, Spanned { base: Tok::Punct(Punct::RBrace), .. }) => break,
                (_, tok) => {
                    return self.report.fatal(tok.span,
                                             format!("expected `,`, `;` or `}}`, got {}",
                                                     tok.base))
                                      .done();
                }
            }
        }

        Ok(fields)
    }

    fn try_parse_args(&mut self) -> ParseResult<Option<Vec<Spanned<Exp>>>> {
        let begin = self.pos();
        match try!(self.read()) {
            (_, Spanned { base: Tok::Punct(Punct::LParen), .. }) => {
                let mut args = Vec::new();
                try!(self.try_scan_explist(|exp| args.push(exp)));
                try!(self.expect(Punct::RParen));
                Ok(Some(args))
            }
            (_, Spanned { base: Tok::Str(s), span }) => {
                Ok(Some(vec![Box::new(Ex::Str(Str::from(s))).with_loc(span)]))
            }
            tok @ (_, Spanned { base: Tok::Punct(Punct::LBrace), .. }) => {
                self.unread(tok);
                let exp = Box::new(Ex::Table(try!(self.parse_table())));
                Ok(Some(vec![exp.with_loc(begin..self.last_pos())]))
            }
            tok => {
                self.unread(tok);
                Ok(None)
            }
        }
    }

    fn try_parse_prefix_exp(&mut self, var_only: bool) -> ParseResult<Option<Spanned<Exp>>> {
        // any prefixexp starts with name or parenthesized exp
        let mut exp;
        let begin = self.pos();
        match try!(self.read()) {
            (_, Spanned { base: Tok::Punct(Punct::LParen), .. }) => {
                // TODO should we ignore the span for parentheses?
                exp = try!(self.parse_exp());
                try!(self.expect(Punct::RParen));
            }
            (_, Spanned { base: Tok::Name(name), span }) => {
                let name: Name = name.into();
                exp = Box::new(Ex::Var(name.with_loc(span))).with_loc(span);
            }
            tok => {
                self.unread(tok);
                return Ok(None);
            }
        }

        // parse any postfix attachments
        loop {
            match try!(self.read()) {
                // prefixexp "." ...
                (_, Spanned { base: Tok::Punct(Punct::Dot), .. }) => {
                    let tok = try!(self.read()).1;
                    if let Tok::Name(name) = tok.base {
                        let name = Box::new(Ex::Str(Str::from(name))).with_loc(tok.span);
                        let span = begin..self.last_pos();
                        exp = Box::new(Ex::Index(exp, name)).with_loc(span);
                    } else {
                        return self.report.fatal(tok.span,
                                                 format!("Expected a name after \
                                                          `<expression> .`, got {}",
                                                          tok.base))
                                          .done();
                    }
                }

                // prefixexp "[" ...
                (_, Spanned { base: Tok::Punct(Punct::LBracket), .. }) => {
                    let exp2 = try!(self.parse_exp());
                    try!(self.expect(Punct::RBracket));
                    let span = begin..self.last_pos();
                    exp = Box::new(Ex::Index(exp, exp2)).with_loc(span);
                }

                // prefixexp ":" ...
                (_, Spanned { base: Tok::Punct(Punct::Colon), .. }) if !var_only => {
                    let name = try!(self.parse_name());
                    if let Some(args) = try!(self.try_parse_args()) {
                        let span = begin..self.last_pos();
                        exp = Box::new(Ex::MethodCall(exp, name, args)).with_loc(span);
                    } else {
                        let tok = try!(self.read()).1;
                        return self.report.fatal(tok.span,
                                                 format!("Expected argument(s) after \
                                                          `<expression> : <name>`, \
                                                          got {}", tok.base))
                                          .done();
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
                            let span = begin..self.last_pos();
                            exp = Box::new(Ex::FuncCall(exp, args)).with_loc(span);
                            continue;
                        }
                    }
                    break;
                }
            }
        }
        Ok(Some(exp))
    }

    fn convert_prefix_exp_to_var(&self, exp: Spanned<Exp>) -> Result<Spanned<Var>, Spanned<Exp>> {
        let span = exp.span;
        let base = *exp.base;
        match base {
            Ex::Var(name) => Ok(Var::Name(name).with_loc(span)),
            Ex::Index(e1, e2) => Ok(Var::Index(e1, e2).with_loc(span)),
            base => Err(Box::new(base).with_loc(span)),
        }
    }

    fn try_parse_atomic_exp(&mut self) -> ParseResult<Option<Spanned<Exp>>> {
        let begin = self.pos();

        let presig = try!(self.try_parse_kailua_func_spec());
        if let Some(ref presig) = presig {
            if !self.lookahead(Keyword::Function) {
                // limit the possible lookahead
                try!(self.report.error(presig.span,
                                       "No function literal after \
                                        the function specification")
                                .done());
            }
        }

        match try!(self.read()) {
            (_, Spanned { base: Tok::Keyword(Keyword::Nil), span }) =>
                Ok(Some(Box::new(Ex::Nil).with_loc(span))),
            (_, Spanned { base: Tok::Keyword(Keyword::False), span }) =>
                Ok(Some(Box::new(Ex::False).with_loc(span))),
            (_, Spanned { base: Tok::Keyword(Keyword::True), span }) =>
                Ok(Some(Box::new(Ex::True).with_loc(span))),
            (_, Spanned { base: Tok::Num(v), span }) =>
                Ok(Some(Box::new(Ex::Num(v)).with_loc(span))),
            (_, Spanned { base: Tok::Str(s), span }) =>
                Ok(Some(Box::new(Ex::Str(s.into())).with_loc(span))),
            (_, Spanned { base: Tok::Punct(Punct::DotDotDot), span }) =>
                Ok(Some(Box::new(Ex::Varargs).with_loc(span))),

            (_, Spanned { base: Tok::Keyword(Keyword::Function), .. }) => {
                let (sig, body) = try!(self.parse_func_body(presig));
                Ok(Some(Box::new(Ex::Func(sig, body)).with_loc(begin..self.last_pos())))
            }

            tok @ (_, Spanned { base: Tok::Punct(Punct::LBrace), .. }) => {
                self.unread(tok);
                let table = try!(self.parse_table());
                Ok(Some(Box::new(Ex::Table(table)).with_loc(begin..self.last_pos())))
            }

            tok => {
                self.unread(tok);
                self.try_parse_prefix_exp(false)
            }
        }
    }

    fn try_parse_prefix_unary_exp<Term, Op>(&mut self,
                                            mut check_op: Op,
                                            mut try_parse_term: Term)
            -> ParseResult<Option<Spanned<Exp>>>
            where Term: FnMut(&mut Self) -> ParseResult<Option<Spanned<Exp>>>,
                  Op: FnMut(&Tok) -> Option<UnOp> {
        let mut ops = Vec::new();
        while let Some(op) = check_op(self.peek()) {
            let opspan = try!(self.read()).1.span;
            ops.push(op.with_loc(opspan));
        }
        if let Some(exp) = try!(try_parse_term(self)) {
            let mut exp = exp;
            while let Some(op) = ops.pop() {
                let span = op.span | exp.span;
                exp = Box::new(Ex::Un(op, exp)).with_loc(span);
            }
            Ok(Some(exp))
        } else if ops.is_empty() {
            Ok(None)
        } else {
            let tok = try!(self.read()).1;
            self.report.fatal(tok.span,
                              format!("Expected a name after a unary operator, got {}", tok.base))
                       .done()
        }
    }

    fn try_parse_left_assoc_binary_exp<Term, Op>(&mut self,
                                                 mut check_op: Op,
                                                 mut try_parse_term: Term)
            -> ParseResult<Option<Spanned<Exp>>>
            where Term: FnMut(&mut Self) -> ParseResult<Option<Spanned<Exp>>>,
                  Op: FnMut(&Tok) -> Option<BinOp> {
        if let Some(exp) = try!(try_parse_term(self)) {
            let mut exp = exp;
            while let Some(op) = check_op(self.peek()) {
                let opspan = try!(self.read()).1.span;
                let exp2 = try!(try!(try_parse_term(self)).ok_or("expected expression"));
                let span = exp.span | opspan | exp2.span;
                exp = Box::new(Ex::Bin(exp, op.with_loc(opspan), exp2)).with_loc(span);
            }
            Ok(Some(exp))
        } else {
            Ok(None)
        }
    }

    fn try_parse_right_assoc_binary_exp<Term, Op>(&mut self,
                                                  mut check_op: Op,
                                                  mut try_parse_term: Term)
            -> ParseResult<Option<Spanned<Exp>>>
            where Term: FnMut(&mut Self) -> ParseResult<Option<Spanned<Exp>>>,
                  Op: FnMut(&Tok) -> Option<BinOp> {
        if let Some(exp) = try!(try_parse_term(self)) {
            // store the terms and process in the reverse order
            // e.g. <exp:terms[0].0> <op:terms[0].1> <exp:terms[1].0> <op:terms[1].1> <exp:last_exp>
            let mut exp = exp;
            let mut terms = vec![];
            while let Some(op) = check_op(self.peek()) {
                let opspan = try!(self.read()).1.span;
                terms.push((exp, op.with_loc(opspan)));
                exp = try!(try!(try_parse_term(self)).ok_or("expected expression"));
            }
            while let Some((exp1, op)) = terms.pop() {
                let span = exp1.span | op.span | exp.span;
                exp = Box::new(Ex::Bin(exp1, op, exp)).with_loc(span);
            }
            Ok(Some(exp))
        } else {
            Ok(None)
        }
    }

    fn try_parse_exp(&mut self) -> ParseResult<Option<Spanned<Exp>>> {
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

    fn parse_exp(&mut self) -> ParseResult<Spanned<Exp>> {
        if let Some(exp) = try!(self.try_parse_exp()) {
            Ok(exp)
        } else {
            let tok = try!(self.read()).1;
            self.report.fatal(tok.span, format!("Expected an expression, got {}", tok.base)).done()
        }
    }

    fn try_parse_var(&mut self) -> ParseResult<Option<Spanned<Var>>> {
        if let Some(exp) = try!(self.try_parse_prefix_exp(true)) {
            let var = self.convert_prefix_exp_to_var(exp).unwrap();
            try!(self.try_parse_kailua_type_spec());
            Ok(Some(var))
        } else {
            Ok(None)
        }
    }

    fn parse_var(&mut self) -> ParseResult<Spanned<Var>> {
        if let Some(var) = try!(self.try_parse_var()) {
            Ok(var)
        } else {
            let tok = try!(self.read()).1;
            self.report.fatal(tok.span, format!("Expected a variable, got {}", tok.base)).done()
        }
    }

    fn scan_varlist_with_spec<F>(&mut self, mut f: F) -> ParseResult<()>
            where F: FnMut(TypeSpec<Spanned<Var>>) {
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

    fn scan_namelist<F>(&mut self, mut f: F) -> ParseResult<()>
            where F: FnMut(Spanned<Name>) {
        f(try!(self.parse_name()));
        while self.may_expect(Punct::Comma) {
            f(try!(self.parse_name()));
        }
        Ok(())
    }

    fn scan_namelist_with_spec<F>(&mut self, mut f: F) -> ParseResult<()>
            where F: FnMut(TypeSpec<Spanned<Name>>) {
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

    fn scan_explist<F>(&mut self, mut f: F) -> ParseResult<()>
            where F: FnMut(Spanned<Exp>) {
        f(try!(self.parse_exp()));
        while self.may_expect(Punct::Comma) {
            f(try!(self.parse_exp()));
        }
        Ok(())
    }

    fn try_scan_explist<F>(&mut self, mut f: F) -> ParseResult<bool>
            where F: FnMut(Spanned<Exp>) {
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

    fn make_kailua_typespec<Base>(&self, base: Base,
                                  spec: Option<(M, Spanned<Kind>)>) -> TypeSpec<Base> {
        if let Some((modf, kind)) = spec {
            TypeSpec { base: base, modf: modf, kind: Some(kind) }
        } else {
            TypeSpec { base: base, modf: M::None, kind: None }
        }
    }

    fn parse_kailua_mod(&mut self) -> ParseResult<M> {
        match try!(self.read()) {
            (_, Spanned { base: Tok::Keyword(Keyword::Var), .. }) => Ok(M::Var),
            (_, Spanned { base: Tok::Keyword(Keyword::Const), .. }) => Ok(M::Const),
            tok => {
                self.unread(tok);
                return Ok(M::None);
            }
        }
    }

    fn parse_kailua_slotkind(&mut self) -> ParseResult<Spanned<SlotKind>> {
        let begin = self.pos();
        let modf = try!(self.parse_kailua_mod());
        let kind = try!(self.parse_kailua_kind());
        Ok(SlotKind { modf: modf, kind: kind }.with_loc(begin..self.last_pos()))
    }

    // may contain ellipsis, the caller is expected to error on unwanted cases
    fn parse_kailua_kindlist(&mut self) -> ParseResult<(Vec<Spanned<Kind>>,
                                                        Option<Spanned<Kind>>)> {
        let mut kinds = Vec::new();
        // KIND {"," KIND} ["..."] or empty
        // try to read the first KIND
        let kind = match try!(self.try_parse_kailua_kind()) {
            Some(kind) => kind,
            None => match try!(self.read()) {
                (_, Spanned { base: Tok::Punct(Punct::DotDotDot), span }) => {
                    try!(self.report.error(span, "`...` should be preceded with a kind \
                                                  in the ordinary kinds")
                                    .done());
                    // pretend that (an invalid) `(...)` is `(?...)`
                    return Ok((kinds, Some(Box::new(K::Dynamic).with_loc(span))));
                }
                tok => {
                    self.unread(tok);
                    return Ok((kinds, None));
                }
            },
        };
        kinds.push(kind);
        while self.may_expect(Punct::Comma) {
            let kind = match try!(self.try_parse_kailua_kind()) {
                Some(kind) => kind,
                None => match try!(self.read()) {
                    (_, Spanned { base: Tok::Punct(Punct::DotDotDot), span }) => {
                        try!(self.report.error(span, "`...` should be preceded with a kind \
                                                      in the ordinary kinds")
                                        .done());
                        // pretend that (an invalid) `(<explist>, ...)` is `(<explist>, ?...)`
                        return Ok((kinds, Some(Box::new(K::Dynamic).with_loc(span))));
                    }
                    (_, tok) => {
                        return self.report.fatal(tok.span,
                                                 format!("Expected a kind, got {}", tok.base))
                                          .done();
                    }
                },
            };
            kinds.push(kind);
        }
        if self.may_expect(Punct::DotDotDot) {
            let last = kinds.pop();
            Ok((kinds, last))
        } else {
            Ok((kinds, None))
        }
    }

    // may contain ellipsis, the caller is expected to error on unwanted cases
    fn parse_kailua_namekindlist(&mut self) -> ParseResult<(Vec<TypeSpec<Spanned<Name>>>,
                                                            Option<Option<Spanned<Kind>>>)> {
        let mut variadic = false;
        let mut specs = Vec::new();
        if self.may_expect(Punct::DotDotDot) { // "..." [":" KIND]
            variadic = true;
        } else { // NAME ":" KIND {"," NAME ":" KIND} ["," "..." [":" KIND]] or empty
            let name = match try!(self.try_parse_name()) {
                Some(name) => name,
                None => return Ok((specs, None)),
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
        if variadic {
            // we've already read `...`
            if self.may_expect(Punct::Colon) {
                let kind = try!(self.parse_kailua_kind());
                Ok((specs, Some(Some(kind))))
            } else {
                Ok((specs, Some(None)))
            }
        } else {
            Ok((specs, None))
        }
    }

    fn parse_kailua_funckind(&mut self) -> ParseResult<Spanned<FuncKind>> {
        let begin = self.pos();
        try!(self.expect(Punct::LParen));
        let (args, varargs) = try!(self.parse_kailua_kindlist());
        try!(self.expect(Punct::RParen));
        let returns = if self.may_expect(Punct::DashGt) {
            // "(" ... ")" "->" ...
            try!(self.parse_kailua_kind_seq())
        } else {
            // "(" ... ")"
            Vec::new()
        };
        let span = begin..self.last_pos();
        Ok(FuncKind { args: args, varargs: varargs, returns: returns }.with_loc(span))
    }

    fn try_parse_kailua_atomic_kind_seq(&mut self) -> ParseResult<Option<Vec<Spanned<Kind>>>> {
        let begin = self.pos();

        let mut kind = match try!(self.read()) {
            (_, Spanned { base: Tok::Keyword(Keyword::Function), span }) => {
                // either a "function" type or a list of function signatures
                if self.lookahead(Punct::LParen) {
                    // function "(" ... ")" ["->" ...] {"&" "(" ... ")" ["->" ...]}
                    let mut funcs = vec![try!(self.parse_kailua_funckind())];
                    while self.may_expect(Punct::Amp) {
                        funcs.push(try!(self.parse_kailua_funckind()));
                    }
                    // cannot be followed by postfix operators
                    let span = begin..self.last_pos();
                    return Ok(Some(vec![Box::new(K::Func(funcs)).with_loc(span)]));
                } else {
                    Box::new(K::Function).with_loc(span)
                }
            }

            (_, Spanned { base: Tok::Punct(Punct::LParen), .. }) => {
                let (mut args, varargs) = try!(self.parse_kailua_kindlist());
                try!(self.expect(Punct::RParen));
                if let Some(ref varargs) = varargs {
                    try!(self.report.error(varargs.span,
                                           "Variadic argument can only be used \
                                            as a function argument")
                                    .done());
                    self.dummy_kind()
                } else if args.len() != 1 {
                    // cannot be followed by postfix operators - XXX really?
                    return Ok(Some(args));
                } else {
                    args.pop().unwrap()
                }
            }

            (_, Spanned { base: Tok::Punct(Punct::LBrace), .. }) => {
                let kind = match try!(self.read()) {
                    // "{" "}"
                    (_, Spanned { base: Tok::Punct(Punct::RBrace), .. }) =>
                        Box::new(K::EmptyTable),

                    // "{" "[" KIND "]" "=" MODF KIND "}"
                    (_, Spanned { base: Tok::Punct(Punct::LBracket), .. }) => {
                        let key = try!(self.parse_kailua_kind());
                        try!(self.expect(Punct::RBracket));
                        try!(self.expect(Punct::Eq));
                        let value = try!(self.parse_kailua_slotkind());
                        try!(self.expect(Punct::RBrace));
                        Box::new(K::Map(key, value))
                    }

                    // tuple, array or record -- distinguished by the secondary lookahead
                    tok => {
                        let is_record = if let (_, Spanned { base: Tok::Name(_), .. }) = tok {
                            self.lookahead(Punct::Eq)
                        } else {
                            false
                        };
                        self.unread(tok);

                        if is_record {
                            // "{" NAME "=" MODF KIND {"," NAME "=" MODF KIND} "}"
                            let mut seen = HashMap::new(); // value denotes the first span
                            let mut fields = Vec::new();
                            loop {
                                let name = try!(self.parse_name());
                                match seen.entry(name.base.clone()) {
                                    hash_map::Entry::Occupied(e) => {
                                        try!(self.report.error(name.span,
                                                               "Duplicate record field in \
                                                                the type specification")
                                                        .note(*e.get(),
                                                              "The first duplicate here")
                                                        .done());
                                    }
                                    hash_map::Entry::Vacant(e) => {
                                        e.insert(name.span);
                                    }
                                }
                                let name = Str::from(name.base).with_loc(name.span);
                                try!(self.expect(Punct::Eq));
                                let slotkind = try!(self.parse_kailua_slotkind());
                                fields.push((name, slotkind));
                                // ";" - "," - ";" "}" - "," "}" - "}"
                                match try!(self.read()).1 {
                                    Spanned { base: Tok::Punct(Punct::Comma), .. } => {}
                                    Spanned { base: Tok::Punct(Punct::Semicolon), .. } => {}
                                    Spanned { base: Tok::Punct(Punct::RBrace), .. } => break,
                                    tok => {
                                        return self.report.fatal(tok.span,
                                                                 format!("expected `,`, `;` or \
                                                                          `}}`, got {}", tok.base))
                                                          .done();
                                    }
                                }
                                if self.may_expect(Punct::RBrace) { break; }
                            }
                            Box::new(K::Record(fields))
                        } else {
                            // array - "{" MODF KIND "}"
                            // tuple - "{" MODF KIND "," [MODF KIND {"," MODF KIND}] "}"
                            let mut fields = Vec::new();
                            let mut sep = false;
                            loop {
                                fields.push(try!(self.parse_kailua_slotkind()));
                                // ";" - "," - ";" "}" - "," "}" - "}"
                                match try!(self.read()).1 {
                                    Spanned { base: Tok::Punct(Punct::Comma), .. } => {}
                                    Spanned { base: Tok::Punct(Punct::Semicolon), .. } => {}
                                    Spanned { base: Tok::Punct(Punct::RBrace), .. } => break,
                                    tok => {
                                        return self.report.fatal(tok.span,
                                                                 format!("expected `,`, `;` or \
                                                                          `}}`, got {}", tok.base))
                                                          .done();
                                    }
                                }
                                sep = true;
                                if self.may_expect(Punct::RBrace) { break; }
                            }
                            if fields.len() == 1 && !sep {
                                Box::new(K::Array(fields.pop().unwrap()))
                            } else {
                                Box::new(K::Tuple(fields))
                            }
                        }
                    }
                };
                kind.with_loc(begin..self.last_pos())
            }

            (_, Spanned { base: Tok::Punct(Punct::Ques), span }) =>
                Box::new(K::Dynamic).with_loc(span),
            (_, Spanned { base: Tok::Keyword(Keyword::Nil), span }) =>
                Box::new(K::Nil).with_loc(span),
            (_, Spanned { base: Tok::Keyword(Keyword::True), span }) =>
                Box::new(K::BooleanLit(true)).with_loc(span),
            (_, Spanned { base: Tok::Keyword(Keyword::False), span }) =>
                Box::new(K::BooleanLit(false)).with_loc(span),

            (_, Spanned { base: Tok::Name(name), span }) => {
                let kind = match &*name {
                    b"boolean"  => K::Boolean,
                    b"number"   => K::Number,
                    b"integer"  => K::Integer,
                    b"string"   => K::String,
                    b"table"    => K::Table,
                    b"function" => K::Function, // allow for quoted `function` too
                    _ => {
                        try!(self.report.error(span, "Unknown type name").done()); // for now
                        K::Dynamic
                    }
                };
                Box::new(kind).with_loc(span)
            }

            (_, Spanned { base: Tok::Num(v), span })
                    if i32::MIN as f64 <= v && v <= i32::MAX as f64 && v.floor() == v => {
                Box::new(K::IntegerLit(v as i32)).with_loc(span)
            }

            (_, Spanned { base: Tok::Str(ref s), span }) => {
                Box::new(K::StringLit(s.to_owned().into())).with_loc(span)
            }

            tok => {
                self.unread(tok);
                return Ok(None);
            }
        };

        // a following `?` are equal to `| nil`
        if self.may_expect(Punct::Ques) {
            let nil = Box::new(K::Nil).with_loc(Span::dummy());
            kind = Box::new(K::Union(vec![kind, nil])).with_loc(begin..self.last_pos());
        }
        Ok(Some(vec![kind]))
    }

    fn try_parse_kailua_kind_seq(&mut self) -> ParseResult<Option<Spanned<Vec<Spanned<Kind>>>>> {
        let begin = self.pos();
        if let Some(mut kindseq) = try!(self.try_parse_kailua_atomic_kind_seq()) {
            if kindseq.len() != 1 {
                return Ok(Some(kindseq.with_loc(begin..self.last_pos())));
            }
            let mut kind = kindseq.pop().unwrap();
            if self.lookahead(Punct::Pipe) { // A | B | ...
                // TODO the current parser is massively ambiguous about pipes in atomic types
                let mut kinds = vec![kind];
                while self.may_expect(Punct::Pipe) {
                    let mut kindseq2 = try!(try!(self.try_parse_kailua_atomic_kind_seq())
                                            .ok_or("expected type"));
                    if kindseq2.len() != 1 {
                        return Err("a sequence of types cannot be inside a union");
                    }
                    kinds.push(kindseq2.pop().unwrap());
                }
                kind = Box::new(K::Union(kinds)).with_loc(begin..self.last_pos());
            }
            Ok(Some(vec![kind].with_loc(begin..self.last_pos())))
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_kind(&mut self) -> ParseResult<Option<Spanned<Kind>>> {
        if let Some(mut kindseq) = try!(self.try_parse_kailua_kind_seq()) {
            if kindseq.base.len() == 1 {
                let first = kindseq.base.pop().unwrap();
                let span = kindseq.span | first.span; // overwrite the span
                Ok(Some(first.base.with_loc(span)))
            } else {
                try!(self.report.error(kindseq.span,
                                       "Expected a single type, not type sequence")
                                .done());
                Ok(Some(self.dummy_kind()))
            }
        } else {
            Ok(None)
        }
    }

    fn parse_kailua_kind_seq(&mut self) -> ParseResult<Vec<Spanned<Kind>>> {
        if let Some(kindseq) = try!(self.try_parse_kailua_kind_seq()) {
            Ok(kindseq.base)
        } else {
            let tok = try!(self.read()).1;
            try!(self.report.error(tok.span,
                                   format!("Expected a single type or type sequence, \
                                            got {}", tok.base))
                            .done());
            Ok(vec![self.dummy_kind()])
        }
    }

    fn parse_kailua_kind(&mut self) -> ParseResult<Spanned<Kind>> {
        if let Some(kind) = try!(self.try_parse_kailua_kind()) {
            Ok(kind)
        } else {
            let tok = try!(self.read()).1;
            try!(self.report.error(tok.span,
                                   format!("Expected a single type, got {}", tok.base))
                            .done());
            Ok(self.dummy_kind())
        }
    }

    fn try_parse_kailua_type_spec_with_spanned_modf(&mut self)
            -> ParseResult<Option<(Spanned<M>, Spanned<Kind>)>> {
        if self.may_expect(Punct::DashDashColon) {
            // allow for `--: { a = foo,
            //            --:   b = bar }`
            self.begin_meta_comment(Punct::DashDashColon);
            let begin = self.pos();
            let modf = match try!(self.parse_kailua_mod()) {
                M::None => M::None.with_loc(begin), // i.e. the beginning of the kind
                modf => modf.with_loc(begin..self.last_pos()),
            };
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

    fn try_parse_kailua_type_spec(&mut self) -> ParseResult<Option<(M, Spanned<Kind>)>> {
        let spec = try!(self.try_parse_kailua_type_spec_with_spanned_modf());
        Ok(spec.map(|(m,k)| (m.base, k)))
    }

    fn try_parse_kailua_rettype_spec(&mut self)
            -> ParseResult<Option<Spanned<Vec<Spanned<Kind>>>>> {
        let begin = self.pos();
        if self.may_expect(Punct::DashDashGt) {
            self.begin_meta_comment(Punct::DashDashGt);
            let kinds = try!(self.parse_kailua_kind_seq());
            let end = self.last_pos();
            try!(self.end_meta_comment(Punct::DashDashGt));

            Ok(Some(kinds.with_loc(begin..end)))
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_func_spec(&mut self) -> ParseResult<Option<Spanned<Sig>>> {
        let begin = self.pos();
        if self.may_expect(Punct::DashDashV) {
            // "(" [NAME ":" KIND] {"," NAME ":" KIND} ["," "..."] ")" ["->" KIND]
            self.begin_meta_comment(Punct::DashDashV);
            try!(self.expect(Punct::LParen));
            let (args, varargs) = try!(self.parse_kailua_namekindlist());
            try!(self.expect(Punct::RParen));
            let returns = if self.may_expect(Punct::DashGt) {
                try!(self.parse_kailua_kind_seq())
            } else {
                Vec::new()
            };
            let end = self.last_pos();
            try!(self.end_meta_comment(Punct::DashDashV));

            let sig = Sig { args: args, varargs: varargs, returns: Some(returns) };
            Ok(Some(sig.with_loc(begin..end)))
        } else {
            Ok(None)
        }
    }

    fn try_parse_kailua_spec(&mut self) -> ParseResult<Option<Option<Spanned<Stmt>>>> {
        let begin = self.pos();
        if self.may_expect(Punct::DashDashHash) {
            self.begin_meta_comment(Punct::DashDashHash);

            // assume NAME ":" MODF KIND
            // assume NAME ":" MODF KIND "=" STR (for builtin spec)
            if self.may_expect(Keyword::Assume) {
                let name = try!(self.parse_name());
                try!(self.expect(Punct::Colon));
                let modf = try!(self.parse_kailua_mod());
                let kind = try!(self.parse_kailua_kind());

                let builtin;
                if self.may_expect(Punct::Eq) {
                    let tok = try!(self.read()).1;
                    if let Tok::Str(s) = tok.base {
                        builtin = Some(Str::from(s).with_loc(tok.span));
                    } else {
                        return Err("string expected after `assume NAME : KIND =`");
                    }
                } else {
                    builtin = None;
                }

                let end = self.last_pos();
                try!(self.end_meta_comment(Punct::DashDashHash));

                let assume = Box::new(St::KailuaAssume(name, modf, kind, builtin));
                return Ok(Some(Some(assume.with_loc(begin..end))));
            }

            // empty `--#` is valid
            try!(self.end_meta_comment(Punct::DashDashHash));
            Ok(Some(None))
        } else {
            Ok(None)
        }
    }

    pub fn into_chunk(mut self) -> ParseResult<Spanned<Block>> {
        let chunk = try!(self.parse_block());
        try!(self.expect(EOF));
        if self.report.can_continue() {
            Ok(chunk)
        } else {
            // the report had recoverable error(s), but we now stop here
            Err("error reported")
        }
    }
}

