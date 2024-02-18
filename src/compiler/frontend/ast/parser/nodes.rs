use super::*;

pub fn parse(buf: &mut Buffer<AToken>, src: &str) -> (UntypedAst, Errors) {
    let mut ast = UntypedAst::new();
    let mut errs = Errors::new();

    parse_more(buf, src, &mut ast, &mut errs, &parse_file_end, 0);

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
        match $buf.next() {
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
        if $vis.is_some() {
            $self.errs.push((ParseError::UnexpectedVisibility, $vis.unwrap().1));
        }
    }};
    (root $vis: expr, $self: expr, $depth: expr) => {{
        if $depth != 0 && $vis.is_some() {
            $self.errs.push((
                ParseError::UnexpectedVisibility,
                $vis.as_ref().unwrap().1.clone(),
            ));
            $self.errs.push((ParseError::OnlyWorkInRoot, $vis.as_ref().unwrap().1.clone()));
        }
    }};
}

macro_rules! link {
    (disable $link: expr, $self: expr) => {{
        if $link.is_some() {
            $self.errs.push((ParseError::UnexpectedLinkage, $link.unwrap().1));
        }
    }};
    (root $link: expr, $self: expr, $depth: expr) => {{
        if $depth != 0 && $link.is_some() {
            $self.errs.push((
                ParseError::UnexpectedLinkage,
                $link.as_ref().unwrap().1.clone(),
            ));
            $self.errs.push((
                ParseError::OnlyWorkInRoot,
                $link.as_ref().unwrap().1.clone(),
            ));
        }
    }};
}

impl<'a> Parser<'a> {
    fn parse_more(
        &mut self,
        ast: &'a mut UntypedAst,
        depth: usize
    ) {
        while !matches!(self.buf.peek(), Some((Token::CuBracketE, _)) | None) {
            self.parse_inner(ast, depth)
        }
    }

    fn parse_inner(
        &mut self,
        ast: &'a mut UntypedAst,
        depth: usize
    ) {
        let visibility = self.parse_visibility();
        let linkage = self.parse_linkage();

        (match self.buf.peek() {
            Some((Token::Semicolon, _)) => Self::parse_end,
            Some((Token::Let, _)) => Self::parse_let,
            Some((Token::CuBracketS, _)) => Self::parse_scope,
            Some((Token::CuBracketE, _)) => Self::parse_end,
            Some((Token::While, _)) => Self::parse_while,
            Some((Token::Fn, _)) => Self::parse_fn,
            Some((Token::Return, _)) => Self::parse_return,
            Some((Token::If, _)) => Self::parse_if,
            Some(_) => Self::parse_expr,
            None => Self::parse_end,
        })(
            self,
            ast,
            visibility,
            linkage,
            NodeExtra::default(),
            depth,
        )
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
        ast: &'a mut UntypedAst,
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

    fn parse_end(
        &mut self,
        ast: &'a mut UntypedAst,
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
        ast: &'a mut UntypedAst,
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
        ast: &'a mut UntypedAst,
        vis: Option<(Visibility, Span)>,
        link: Option<(Linkage, Span)>,
        extra: NodeExtra,
        depth: usize,
    ) {
        vis!(disable vis, self);
        link!(disable link, self);

        let start = self.buf.next().unwrap().1.clone();

        let mut new_ast = UntypedAst::new();
        self.parse_more(&mut new_ast, depth + 1);

        let end = self.buf.next().map_or_else(Span::default, |a| a.1.clone());

        ast.push(Node {
            kind: NodeKind::Scope {
                body: new_ast,
                span: Span {
                    start: start.start,
                    end: end.end,
                },
            },
            span: Span {
                start: start.start,
                end: end.end,
            },
            extra,
        });
    }
}

#[parser_fn]
fn parse_while() {
    vis!(disable vis, errs, should_end);
    link!(disable link, errs, should_end);

    let start = buf.next().unwrap().1.clone().start;

    let expr = consider_error!(exprs::parse(buf, src, true), buf, errs, should_end);

    buf.rewind();
    let scope_start = assert_token!(Token::CuBracketS, buf, errs, should_end).start;

    let mut new_ast = UntypedAst::new();
    parse_more(
        buf,
        src,
        &mut new_ast,
        errs,
        &*parse_scope_end(scope_start),
        depth + 1,
    );

    let end = buf.next().map_or_else(Span::default, |a| a.1.clone()).end;

    ast.push(Node {
        kind: NodeKind::While {
            cond: expr,
            body: new_ast,
            span: Span {
                start: scope_start,
                end,
            },
        },
        span: Span { start, end },
        extra,
    });

    should_end(buf, errs)
}

#[parser_fn]
fn parse_fn() {
    vis!(root vis, errs, should_end, depth);
    link!(root link, errs, should_end, depth);

    let span = buf.next().unwrap().1.clone();

    let ident = unwrap_ident!(buf, src, errs, should_end);
    assert_token!(Token::RoBracketS, buf, errs, should_end).start;

    let mut params = Vec::new();
    while !matches!(buf.peek(), Some((Token::RoBracketE, _))) {
        let param = parse_fn_param(buf, src, errs, should_end);
        if let Ok(param) = param {
            params.push(param);
        } else {
            return unsafe { param.unwrap_err_unchecked() };
        }
    }

    buf.next();

    let (return_type, scope_start) = match buf.next() {
        Some((Token::CuBracketS, span)) => (
            (
                Type::BuiltIn(BuiltInType::Unit),
                Span {
                    start: span.start,
                    end: span.start,
                },
            ),
            span.start,
        ),
        Some(_) => {
            buf.rewind();
            let typ = unwrap_or_return_set_buf!(
                types::parse(
                    buf,
                    src,
                    errs,
                ),
                buf,
                should_end(buf, errs)
            );
            let start = assert_token!(Token::CuBracketS, buf, errs, should_end).start;
            (typ, start)
        },
        _ => todo!(),
    };

    let mut new_ast = UntypedAst::new();
    parse_more(
        buf,
        src,
        &mut new_ast,
        errs,
        &*parse_scope_end(scope_start),
        depth + 1,
    );

    let end = buf.next().map_or_else(Span::default, |a| a.1.clone()).end;

    ast.push(Node {
        kind: NodeKind::FunctionDeclare {
            vis,
            link,
            ident,
            params,
            return_type,
            body: new_ast,
            span: Span {
                start: scope_start,
                end,
            },
        },
        span: Span {
            start: span.start,
            end,
        },
        extra,
    });

    should_end(buf, errs)
}

fn parse_fn_param(
    buf: &mut Buffer<AToken>,
    src: &str,
    errs: &mut Errors,
    _should_end: ShouldEndFn<'_>,
) -> Result<(AString, AType, Span), bool> {
    let should_end = |buf, errs| Err(_should_end(buf, errs));

    let ident = unwrap_ident!(buf, src, errs, should_end);
    let typ = unwrap_or_return_set_buf!(
        types::parse(
            buf,
            src,
            errs,
        ),
        buf,
        should_end(buf, errs)
    );

    match buf.next() {
        Some((Token::Comma, _)) => (),
        Some((Token::RoBracketE, _)) => buf.rewind(),
        Some((_, s)) => error!(
            ParseError::UnexpectedToken,
            s.clone(),
            buf,
            errs,
            should_end
        ),
        None => error!(
            ParseError::RanOutTokens,
            buf.prev().unwrap().1.clone(),
            buf,
            errs,
            should_end
        ),
    }

    let start = ident.1.start;
    let end = typ.1.end;

    Ok((ident, typ, Span { start, end }))
}

#[parser_fn]
fn parse_return() {
    vis!(disable vis, errs, should_end);
    link!(disable link, errs, should_end);

    let span = buf.next().unwrap().1.clone();

    let value = match buf.peek() {
        Some((Token::Semicolon, _)) | None => None,
        _ => Some(consider_error!(
            exprs::parse(buf, src, true),
            buf,
            errs,
            should_end
        )),
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

    should_end(buf, errs)
}

#[parser_fn]
fn parse_if() {
    vis!(disable vis, errs, should_end);
    link!(disable link, errs, should_end);

    let start = buf.next().unwrap().1.clone().start;

    let expr = consider_error!(exprs::parse(buf, src, true), buf, errs, should_end);

    buf.rewind();
    let scope_start = assert_token!(Token::CuBracketS, buf, errs, should_end).start;

    let mut new_ast = UntypedAst::new();
    parse_more(
        buf,
        src,
        &mut new_ast,
        errs,
        &*parse_scope_end(start),
        depth + 1,
    );

    let end = buf.next().map_or_else(Span::default, |a| a.1.clone()).end;

    let els = match buf.peek() {
        Some((Token::Else, _)) => {
            buf.next();
            let start = assert_token!(Token::If | Token::CuBracketS, buf, errs, should_end).start;
            buf.rewind();
            let mut body = Vec::with_capacity(1);
            parse_inner(buf, src, &mut body, errs, should_end, depth);
            let end = buf.next().map_or_else(Span::default, |a| a.1.clone()).end;
            Some((Box::new(body[0].clone()), Span { start, end }))
        },
        _ => None,
    };

    ast.push(Node {
        kind: NodeKind::If {
            main: (
                expr,
                new_ast,
                Span {
                    start: scope_start,
                    end,
                },
            ),
            els,
        },
        span: Span { start, end },
        extra,
    });

    should_end(buf, errs)
}

fn parse_scope_end(start: usize) -> Box<ShouldEndFnInner> {
    Box::new(move |buf, errs| match buf.peek() {
        Some((Token::CuBracketE, _)) => false,
        None => {
            let end = buf.buf.last().unwrap().1.clone().end;
            errs.push((ParseError::UnendedScope, Span { start, end }));
            false
        },
        Some(_) => true,
    })
}

fn parse_file_end(buf: &mut Buffer<AToken>, errs: &mut Errors) -> bool {
    match buf.peek() {
        None => false,
        Some((Token::CuBracketE, span)) => {
            errs.push((ParseError::UnexpectedDelimiter, span.clone()));
            true
        },
        Some(_) => true,
    }
}
