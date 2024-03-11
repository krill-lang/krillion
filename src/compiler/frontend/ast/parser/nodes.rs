use super::*;

pub fn parse(buf: &mut Buffer<AToken>, src: &str) -> (UntypedAst, Errors) {
    let mut ast = UntypedAst::new();
    let mut errs = Errors::new();

    let mut parser = Parser { buf, src, errs: &mut errs };

    parser.parse_more(&mut ast, 0);

    errs.push((
        ParseError::YourMom,
        Span {
            start: 0,
            end: src.len(),
        },
    ));

    (ast, errs)
}

macro_rules! consider_error {
    ($expr: expr, $self: expr) => {
        match $expr {
            Err((e, s)) => {
                $self.buf.rewind();
                error!(e, s, $self);
            },
            Ok(a) => a,
        }
    };
}

macro_rules! unwrap_or_return_set_buf {
    ($expr: expr, $buf: expr) => {
        match $expr {
            Some(a) => a,
            _ => {
                while let Some((t, _)) = $buf.next() {
                    if matches!(t, Token::Semicolon | Token::CuBracketS) {
                        break;
                    }
                }

                $buf.rewind();
                return;
            },
        }
    };
}

macro_rules! error {
    ($reason: expr, $span: expr, $self: expr) => {{
        $self.errs.push(($reason, $span));
        while let Some((t, _)) = $self.buf.next() {
            if matches!(t, Token::Semicolon | Token::CuBracketS) {
                break;
            }
        }

        $self.buf.rewind();
        return;
    }};
}

macro_rules! unwrap_ident {
    ($self: expr) => {
        match $self.buf.next() {
            Some((Token::Ident, s)) => ($self.src[s.start..s.end].to_string(), s.clone()),
            Some((_, s)) => error!(
                ParseError::UnexpectedToken,
                s.clone(),
                $self
            ),
            None => error!(
                ParseError::RanOutTokens,
                $self.buf.prev().unwrap().1.clone(),
                $self
            ),
        }
    };
}

macro_rules! assert_token {
    ($intended: pat, $self: expr) => {
        match $self.buf.next() {
            Some(($intended, s)) => s.clone(),
            Some((_, s)) => error!(
                ParseError::UnexpectedToken,
                s.clone(),
                $self
            ),
            None => error!(
                ParseError::RanOutTokens,
                $self.buf.prev().unwrap().1.clone(),
                $self
            ),
        }
    };
}

macro_rules! vis {
    (disable $vis: expr, $self: expr) => {{
        if let Some(vis) = $vis {
            $self.errs.push((ParseError::UnexpectedVisibility, vis.1));
        }
    }};
    (root $vis: expr, $self: expr, $depth: expr) => {{
        if let (Some(vis), true) = (&$vis, $depth != 0) {
            $self.errs.push((
                ParseError::UnexpectedVisibility,
                vis.1.clone(),
            ));
            $self.errs.push((ParseError::OnlyWorkInRoot, vis.1.clone()));
        }
    }};
}

macro_rules! link {
    (disable $link: expr, $self: expr) => {{
        if let Some(link) = $link {
            $self.errs.push((ParseError::UnexpectedLinkage, link.1));
        }
    }};
    (root $link: expr, $self: expr, $depth: expr) => {{
        if let (Some(link), true) = (&$link, $depth != 0) {
            $self.errs.push((
                ParseError::UnexpectedLinkage,
                link.1.clone(),
            ));
            $self.errs.push((
                ParseError::OnlyWorkInRoot,
                link.1.clone(),
            ));
        }
    }};
}

impl<'a> Parser<'a> {
    fn parse_more(
        &mut self,
        ast: &mut UntypedAst,
        depth: usize
    ) {
        // while !matches!(self.buf.peek(), Some((Token::CuBracketE, _)) | None) {
        //     self.parse_inner(ast, depth)
        // }
        while self.parse_inner(ast, depth) {}
    }

    fn parse_inner(
        &mut self,
        ast: &mut UntypedAst,
        depth: usize
    ) -> bool {
        #[cfg(feature = "unstable")]
        println!("{}", self.buf.idx);

        let visibility = self.parse_visibility();
        let linkage = self.parse_linkage();

        (match self.buf.peek() {
            Some((Token::Semicolon, _)) => Self::parse_nothing,
            Some((Token::Let, _)) => Self::parse_let,
            Some((Token::CuBracketS, _)) => Self::parse_scope,
            Some((Token::CuBracketE, _)) => return false,
            Some((Token::While, _)) => Self::parse_while,
            Some((Token::Fn, _)) => Self::parse_fn,
            Some((Token::Return, _)) => Self::parse_return,
            Some((Token::If, _)) => Self::parse_if,
            Some(_) => Self::parse_expr,
            None => return false,
        })(
            self,
            ast,
            visibility,
            linkage,
            NodeExtra::default(),
            depth,
        );

        true
    }

    fn parse_visibility(&mut self) -> Option<(Visibility, Span)> {
        match self.buf.next() {
            Some((Token::Pub, span)) => Some((Visibility::Public, span.clone())),
            _ => {
                self.buf.rewind();
                None
            },
        }
    }

    fn parse_linkage(&mut self) -> Option<(Linkage, Span)> {
        match self.buf.next() {
            Some((Token::Extern, span)) => Some((Linkage::External, span.clone())),
            Some((Token::Static, span)) => Some((Linkage::Static, span.clone())),
            _ => {
                self.buf.rewind();
                None
            },
        }
    }

    fn parse_expr(
        &mut self,
        ast: &mut UntypedAst,
        vis: Option<(Visibility, Span)>,
        link: Option<(Linkage, Span)>,
        extra: NodeExtra,
        _depth: usize,
    ) {
        vis!(disable vis, self);
        link!(disable link, self);

        let expr = consider_error!(exprs::parse(self.buf, self.src, false), self);
        let span = expr.1.clone();

        ast.push(Node {
            kind: NodeKind::Expr(expr),
            span: Span {
                start: span.start,
                end: self.buf.current().unwrap().1.start,
            },
            extra,
        });
    }

    fn parse_nothing(
        &mut self,
        _ast: &mut UntypedAst,
        vis: Option<(Visibility, Span)>,
        link: Option<(Linkage, Span)>,
        _extra: NodeExtra,
        _depth: usize,
    ) {
        vis!(disable vis, self);
        link!(disable link, self);

        self.buf.next();
    }

    fn parse_let(
        &mut self,
        ast: &mut UntypedAst,
        vis: Option<(Visibility, Span)>,
        link: Option<(Linkage, Span)>,
        extra: NodeExtra,
        depth: usize,
    ) {
        vis!(root vis, self, depth);
        link!(root link, self, depth);

        let span = self.buf.next().unwrap().1.clone();

        let (ident, id_span) = unwrap_ident!(self);

        let typ = match self.buf.peek() {
            Some((Token::Operator(Operator::Assign), _)) => None,
            Some((Token::Semicolon, _)) => None,
            Some(_) => Some(unwrap_or_return_set_buf!(
                types::parse(
                    self.buf,
                    self.src,
                    self.errs,
                ),
                self.buf
            )),
            None => error!(ParseError::RanOutTokens, span, self),
        };

        let (expr, end) = match self.buf.next() {
            Some((Token::Operator(Operator::Assign), _)) => {
                let expr = consider_error!(exprs::parse(self.buf, self.src, false), self);
                (Some(expr), self.buf.current().unwrap().1.start)
            },
            Some((Token::Semicolon, _)) => (None, self.buf.current().unwrap().1.start),
            Some((_, s)) => error!(ParseError::UnexpectedToken, s.clone(), self),
            None => error!(ParseError::RanOutTokens, span, self),
        };

        ast.push(Node {
            kind: NodeKind::VarDeclare {
                vis,
                link,
                ident: (ident, id_span),
                typ,
                expr,
            },
            span: Span {
                start: span.start,
                end,
            },
            extra,
        });
    }

    fn parse_scope(
        &mut self,
        ast: &mut UntypedAst,
        vis: Option<(Visibility, Span)>,
        link: Option<(Linkage, Span)>,
        extra: NodeExtra,
        depth: usize,
    ) {
        vis!(disable vis, self);
        link!(disable link, self);

        let scope = if let Some((a, _)) = self.parse_scope_impl(depth, extra) {
            a
        } else {
            return
        };

        ast.push(scope);
    }

    fn parse_scope_impl(
        &mut self, depth: usize, extra: NodeExtra
    ) -> Option<(Node<UntypedNode>, Span)> {
        let start = match self.buf.next() {
            Some((Token::CuBracketS, s)) => s.start,
            _ => return None,
        };

        let mut new_ast = UntypedAst::new();
        self.parse_more(&mut new_ast, depth + 1);

        let end = match self.buf.next() {
            Some((Token::CuBracketE, s)) => s.end,
            _ => return None,
        };

        Some((Node {
            kind: NodeKind::Scope {
                body: new_ast,
                span: Span { start, end },
            },
            span: Span { start, end },
            extra,
        }, Span { start, end }))
    }

    fn parse_while(
        &mut self,
        ast: &mut UntypedAst,
        vis: Option<(Visibility, Span)>,
        link: Option<(Linkage, Span)>,
        extra: NodeExtra,
        depth: usize,
    ) {
        vis!(disable vis, self);
        link!(disable link, self);

        let start = self.buf.next().unwrap().1.clone().start;

        let expr = consider_error!(exprs::parse(self.buf, self.src, true), self);

        self.buf.rewind();

        let (body, span) = if let Some(a) = self.parse_scope_impl(depth, NodeExtra::default()) {
            a
        } else {
            return
        };
        let body = Box::new(body);

        ast.push(Node {
            kind: NodeKind::While {
                cond: expr,
                body,
                // span: Span {
                //     start: scope_start,
                //     end,
                // },
            },
            span: Span { start, end: span.end },
            extra,
        });
    }

    fn parse_return(
        &mut self,
        ast: &mut UntypedAst,
        vis: Option<(Visibility, Span)>,
        link: Option<(Linkage, Span)>,
        extra: NodeExtra,
        depth: usize,
    ) {
        vis!(disable vis, self);
        link!(disable link, self);

        let span = self.buf.next().unwrap().1.clone();

        let value = match self.buf.peek() {
            Some((Token::Semicolon, _)) | None => None,
            _ => Some(consider_error!(exprs::parse(self.buf, self.src, true), self)),
        };

        let end = value.as_ref().map_or(span.end, |a| a.1.end);
        ast.push(Node {
            kind: NodeKind::Return(value),
            span: Span {
                start: span.start,
                end,
            },
            extra,
        });
    }

    fn parse_if(
        &mut self,
        ast: &mut UntypedAst,
        vis: Option<(Visibility, Span)>,
        link: Option<(Linkage, Span)>,
        extra: NodeExtra,
        depth: usize,
    ) {
        vis!(disable vis, self);
        link!(disable link, self);

        let start = self.buf.next().unwrap().1.clone().start;

        let expr = consider_error!(exprs::parse(self.buf, self.src, true), self);

        self.buf.rewind();

        let (body, span) = if let Some(a) = self.parse_scope_impl(depth, NodeExtra::default()) {
            a
        } else {
            return
        };
        let body = Box::new(body);

        let els = match self.buf.peek() {
            Some((Token::Else, _)) => {
                self.buf.next();
                let start = assert_token!(Token::If | Token::CuBracketS, self).start;
                self.buf.rewind();
                let mut body = Vec::with_capacity(1);
                self.parse_inner(&mut body, depth + 1);
                let end = self.buf.next().map_or_else(Span::default, |a| a.1.clone()).end;
                Some((Box::new(body[0].clone()), Span { start, end }))
            },
            _ => None,
        };

        ast.push(Node {
            kind: NodeKind::If {
                main: (
                    expr,
                    body,
                    span.clone(),
                ),
                els,
            },
            span: Span { start, end: span.end },
            extra,
        });
    }

    fn parse_fn(
        &mut self,
        ast: &mut UntypedAst,
        vis: Option<(Visibility, Span)>,
        link: Option<(Linkage, Span)>,
        extra: NodeExtra,
        depth: usize,
    ) {
        vis!(root vis, self, depth);
        link!(root link, self, depth);

        let span = self.buf.next().unwrap().1.clone();

        let ident = unwrap_ident!(self);
        assert_token!(Token::RoBracketS, self).start;

        let mut params = Vec::new();
        while !matches!(self.buf.peek(), Some((Token::RoBracketE, _))) {
            let param = self.parse_fn_param();
            param.map_or_else(|e| {
                error!(e.0, e.1, self);
            }, |param| {
                params.push(param);
            });
        }

        self.buf.next();

        let return_type = match self.buf.next() {
            Some((Token::CuBracketS, span)) => (
                Type::BuiltIn(BuiltInType::Unit),
                Span {
                    start: span.start,
                    end: span.end
                }
            ),
            Some(_) => {
                self.buf.rewind();
                unwrap_or_return_set_buf!(
                    types::parse(
                        self.buf,
                        self.src,
                        self.errs,
                    ),
                    self.buf
                )
            },
            _ => todo!(),
        };

        self.buf.rewind();

        let (body, body_span) = if let Some(a) = self.parse_scope_impl(depth, NodeExtra::default()) {
            a
        } else {
            return
        };
        let body = Box::new(body);

        ast.push(Node {
            kind: NodeKind::FunctionDeclare {
                vis,
                link,
                ident,
                params,
                return_type,
                body,
                span: body_span.clone()
            },
            span: Span {
                start: span.start,
                end: body_span.end,
            },
            extra,
        });
    }

    fn parse_fn_param(&mut self) -> Result<(AString, AType, Span), AError<ParseError>> {
        let ident = match self.buf.next() {
            Some((Token::Ident, s)) => (self.src[s.start..s.end].to_string(), s.clone()),
            Some((_, s)) => return Err((ParseError::UnexpectedToken, s.clone())),
            None => return Err((ParseError::RanOutTokens, self.buf.prev().unwrap().1.clone())),
        };

        let typ = types::parse(
            self.buf,
            self.src,
            self.errs,
        ).unwrap();

        match self.buf.next() {
            Some((Token::Comma, _)) => (),
            Some((Token::RoBracketE, _)) => self.buf.rewind(),
            Some((_, s)) => return Err((ParseError::UnexpectedToken, s.clone())),
            None => return Err((ParseError::RanOutTokens, self.buf.prev().unwrap().1.clone())),
        }

        let start = ident.1.start;
        let end = typ.1.end;

        Ok((ident, typ, Span { start, end }))
    }
}




// fn parse_scope_end(start: usize) -> Box<ShouldEndFnInner> {
//     Box::new(move |buf, errs| match buf.peek() {
//         Some((Token::CuBracketE, _)) => false,
//         None => {
//             let end = buf.buf.last().unwrap().1.clone().end;
//             errs.push((ParseError::UnendedScope, Span { start, end }));
//             false
//         },
//         Some(_) => true,
//     })
// }

// fn parse_file_end(buf: &mut Buffer<AToken>, errs: &mut Errors) -> bool {
//     match buf.peek() {
//         None => false,
//         Some((Token::CuBracketE, span)) => {
//             errs.push((ParseError::UnexpectedDelimiter, span.clone()));
//             true
//         },
//         Some(_) => true,
//     }
// }
