use super::super::*;
use crate::compiler::util::*;

type Errors = Vec<AError<ParseError>>;

pub fn parse(buf: &mut Buffer<AToken>, src: &str) -> (UntypedAst, Errors) {
    let mut ast = UntypedAst::new();
    let mut errs = Errors::new();

    parse_more(buf, src, &mut ast, &mut errs, &parse_file_end);

    errs.push((ParseError::YourMom, Span {
        start: 0,
        end: src.len()
    }));

    (ast, errs)
}

macro_rules! consider_error {
    ($expr: expr, $b: expr, $e: expr, $se: expr) => {
        match $expr {
            Err((e, s)) => {
                $b.rewind();
                error!(e, s, $b, $e, $se);
            },
            Ok(a) => a,
        }
    };
}

macro_rules! error {
    ($r: expr, $s: expr, $b: expr, $e: expr, $se: expr) => {{
        $e.push(($r, $s));
        while let Some((t, _)) = $b.next() {
            if matches!(t, Token::Semicolon | Token::CuBracketS) {
                break;
            }
        }

        $b.rewind();
        return $se($b, $e);
    }};
}

macro_rules! unwrap_ident {
    ($span: expr, $buf: expr, $src: expr, $errs: expr, $should_end: expr) => {
        match $buf.next() {
            Some((Token::Ident, s)) => ($src[s.start..s.end].to_string(), s.clone()),
            Some((_, s)) => error!(ParseError::UnexpectedToken, s.clone(), $buf, $errs, $should_end),
            None => error!(ParseError::RanOutTokens, $span, $buf, $errs, $should_end),
        }
    };
}

macro_rules! assert_token {
    ($intended: pat, $buf: expr, $errs: expr, $should_end: expr) => {
        match $buf.next() {
            Some(($intended, _)) => (),
            Some((_, s)) => error!(
                ParseError::UnexpectedToken, s.clone(),
                $buf,
                $errs,
                $should_end
            ),
            None => error!(
                ParseError::RanOutTokens,
                $buf.prev().unwrap().1.clone(),
                $buf,
                $errs,
                $should_end
            ),
        }
    };
}

macro_rules! disable_vis {
    ($vis: expr, $buf: expr, $err: expr, $should_end: expr) => {{
        if $vis.is_some() {
            error!(ParseError::UnexpectedVisibility, $vis.unwrap().1, $buf, $err, $should_end);
        }
    }};
}

macro_rules! disable_link {
    ($link: expr, $buf: expr, $err: expr, $should_end: expr) => {{
        if $link.is_some() {
            error!(ParseError::UnexpectedLinkage, $link.unwrap().1, $buf, $err, $should_end);
        }
    }};
}

type ShouldEndFn<'a> = &'a ShouldEndFnInner;
type ShouldEndFnInner = dyn Fn(
    &mut Buffer<AToken>,
    &mut Errors
) -> bool;

fn parse_more(
    buf: &mut Buffer<AToken>,
    src: &str,
    ast: &mut UntypedAst,
    errs: &mut Errors,
    should_end: ShouldEndFn<'_>,
) {
    while parse_inner(buf, src, ast, errs, should_end) {
    }
}

fn parse_inner(
    buf: &mut Buffer<AToken>,
    src: &str,
    ast: &mut UntypedAst,
    errs: &mut Errors,
    should_end: ShouldEndFn<'_>,
) -> bool {
    let visibility = parse_visibility(buf, src, ast, errs, should_end);
    let linkage = parse_linkage(buf, src, ast, errs, should_end);

    (match buf.peek() {
        Some((Token::Semicolon, _)) => parse_empty,
        Some((Token::Let, _)) => parse_let,
        Some((Token::CuBracketS, _)) => parse_scope,
        Some((Token::While, _)) => parse_while,
        Some(_) => parse_expr,
        None => parse_empty,
    })(buf, src, ast, visibility, linkage, NodeExtra::default(), errs, should_end)
}

fn parse_expr(
    buf: &mut Buffer<AToken>,
    src: &str,
    ast: &mut UntypedAst,
    vis: Option<(Visibility, Span)>,
    _link: Option<(Linkage, Span)>,
    extra: NodeExtra,
    errs: &mut Errors,
    should_end: ShouldEndFn<'_>,
) -> bool {
    disable_vis!(vis, buf, errs, should_end);

    let expr = consider_error!(
        exprs::parse(buf, src, false),
        buf,
        errs,
        should_end
    );
    let span = expr.1.clone();
    ast.push(
        Node {
            kind: NodeKind::Expr(expr),
            span: Span {
                start: span.start,
                end: buf.current().unwrap().1.start
            },
            extra,
        }
    );

    should_end(buf, errs)
}

fn parse_empty(
    buf: &mut Buffer<AToken>,
    _src: &str,
    _ast: &mut UntypedAst,
    vis: Option<(Visibility, Span)>,
    link: Option<(Linkage, Span)>,
    _extra: NodeExtra,
    errs: &mut Errors,
    should_end: ShouldEndFn<'_>,
) -> bool {
    disable_vis!(vis, buf, errs, should_end);
    disable_link!(vis, buf, errs, should_end);

    buf.next();
    should_end(buf, errs)
}

fn parse_let(
    buf: &mut Buffer<AToken>,
    src: &str,
    ast: &mut UntypedAst,
    vis: Option<(Visibility, Span)>,
    link: Option<(Linkage, Span)>,
    extra: NodeExtra,
    errs: &mut Errors,
    should_end: ShouldEndFn<'_>,
) -> bool {
    let span = buf.next().unwrap().1.clone();

    let (ident, idspan) = unwrap_ident!(span, buf, src, errs, should_end);

    let typ = match buf.peek() {
        Some((Token::Operator(Operator::Assign), _)) => None,
        Some((Token::Semicolon, _)) => None,
        Some(_) => Some(consider_error!(types::parse(buf, src), buf, errs, should_end)),
        None => error!(ParseError::RanOutTokens, span, buf, errs, should_end),
    };

    let (expr, end) = match buf.next() {
        Some((Token::Operator(Operator::Assign), _)) => {
            let expr = consider_error!(exprs::parse(buf, src, false), buf, errs, should_end);
            (Some(expr), buf.current().unwrap().1.start)
        },
        Some((Token::Semicolon, _)) => (None, buf.current().unwrap().1.start),
        Some((_, s)) => error!(ParseError::UnexpectedToken, s.clone(), buf, errs, should_end),
        None => error!(ParseError::RanOutTokens, span, buf, errs, should_end),
    };

    ast.push(
        Node {
            kind: NodeKind::VarDeclare {
                vis,
                link,
                ident: (ident, idspan),
                typ,
                expr
            },
            span: Span {
                start: span.start,
                end,
            },
            extra
        }
    );

    should_end(buf, errs)
}

fn parse_scope(
    buf: &mut Buffer<AToken>,
    src: &str,
    ast: &mut UntypedAst,
    vis: Option<(Visibility, Span)>,
    link: Option<(Linkage, Span)>,
    extra: NodeExtra,
    errs: &mut Errors,
    should_end: ShouldEndFn<'_>,
) -> bool {
    disable_vis!(vis, buf, errs, should_end);
    disable_link!(link, buf, errs, should_end);

    let start = buf.next().unwrap().1.clone();

    let mut new_ast = UntypedAst::new();
    parse_more(buf, src, &mut new_ast, errs, &*parse_scope_end(start.start));

    let end = buf.next().map_or_else(Span::default, |a| a.1.clone());

    ast.push(
        Node {
            kind: NodeKind::Scope {
                body: new_ast,
                span: Span {
                    start: start.start,
                    end: end.end,
                }
            },
            span: Span {
                start: start.start,
                end: end.end,
            },
            extra
        }
    );

    should_end(buf, errs)
}

fn parse_while(
    buf: &mut Buffer<AToken>,
    src: &str,
    ast: &mut UntypedAst,
    vis: Option<(Visibility, Span)>,
    link: Option<(Linkage, Span)>,
    extra: NodeExtra,
    errs: &mut Errors,
    should_end: ShouldEndFn<'_>,
) -> bool {
    disable_vis!(vis, buf, errs, should_end);
    disable_link!(link, buf, errs, should_end);

    let start = buf.next().unwrap().1.clone();

    let expr = consider_error!(
        exprs::parse(buf, src, true),
        buf,
        errs,
        should_end
    );
    let expr_span = expr.1.clone();

    buf.rewind();
    assert_token!(Token::CuBracketS, buf, errs, should_end);

    let mut new_ast = UntypedAst::new();
    parse_more(buf, src, &mut new_ast, errs, &*parse_scope_end(start.start));

    let end = buf.next().map_or_else(Span::default, |a| a.1.clone());

    ast.push(
        Node {
            kind: NodeKind::While {
                cond: expr,
                body: new_ast,
                span: Span {
                    start: expr_span.end,
                    end: end.end
                }
            },
            span: Span {
                start: start.start,
                end: end.end,
            },
            extra
        }
    );

    should_end(buf, errs)
}

fn parse_visibility(
    buf: &mut Buffer<AToken>,
    _src: &str,
    _ast: &mut UntypedAst,
    _errs: &mut Errors,
    _should_end: ShouldEndFn<'_>,
) -> Option<(Visibility, Span)> {
    match buf.next() {
        Some((Token::Pub, span)) => {
            Some((Visibility::Public, span.clone()))
        },
        _ => {
            buf.rewind();
            None
        },
    }
}

fn parse_linkage(
    buf: &mut Buffer<AToken>,
    _src: &str,
    _ast: &mut UntypedAst,
    _errs: &mut Errors,
    _should_end: ShouldEndFn<'_>,
) -> Option<(Linkage, Span)> {
    match buf.next() {
        Some((Token::Extern, span)) => {
            Some((Linkage::External, span.clone()))
        },
        Some((Token::Static, span)) => {
            Some((Linkage::Static, span.clone()))
        },
        _ => {
            buf.rewind();
            None
        },
    }
}

fn parse_scope_end(start: usize) -> Box<ShouldEndFnInner> {
    Box::new(move |buf, errs| {
        match buf.peek() {
            Some((Token::CuBracketE, _)) => false,
            None => {
                let end = buf.buf.last().unwrap().1.clone().end;
                errs.push((ParseError::UnendedScope, Span { start, end }));
                false
            },
            Some(_) => true,
        }
    })
}

fn parse_file_end(
    buf: &mut Buffer<AToken>,
    errs: &mut Errors,
) -> bool {
    match buf.peek() {
        None => false,
        Some((Token::CuBracketE, span)) => {
            errs.push((ParseError::UnexpectedDelimiter, span.clone()));
            true
        },
        Some(_) => true,
    }
}

/*
pub fn parse(buf: &mut Buffer<AToken>, src: &str) -> Result<UntypedAst, Vec<ACompileError>> {
    let ast = UntypedAst::new();
    let mut _span1 = Span::default();
    let _span2 = Span::default();

    let mut comp_errs: Vec<ACompileError> = Vec::new();

    fn get_ast<'a>(
        ast: &'a UntypedAst,
        span1: &'a Span,
        span2: Span,
    ) -> Vec<(
        &'a mut UntypedAst,
        &'a mut Span,
        Span,
        Option<&'a mut AUntypedNode>,
    )> {
        let mut scope = unsafe { vec![(as_mut(ast), as_mut(span1), span2, None)] };
        while let Some(a) = unsafe {
            let last = core::mem::transmute_copy::<
                    _,
                    &Vec<(&mut UntypedAst, &mut Span, Span, Option<&mut AUntypedNode>)>,
                >(&&scope)
                .last()
                .unwrap();
            match last.0.last() {
                Some((
                    Node::FunctionDeclare {
                        body,
                        span: span2,
                        ended: false,
                        ..
                    }
                    | Node::Scope {
                        body,
                        span: span2,
                        ended: false,
                    }
                    | Node::While {
                        body,
                        span: span2,
                        ended: false,
                        ..
                    },
                    span1,
                )) => Some((
                    body as *const Vec<_>,
                    span1,
                    span2.clone(),
                    Some(as_mut(last.0.last().unwrap())),
                )),
                Some((
                    Node::If {
                        main,
                        els,
                        ended: false,
                    },
                    span1,
                )) => {
                    let l = main.last().unwrap();
                    els.as_ref().map_or_else(
                        || {
                            Some((
                                &l.1 as *const Vec<_>,
                                span1,
                                l.2.clone(),
                                Some(as_mut(last.0.last().unwrap())),
                            ))
                        },
                        |body| {
                            Some((
                                &body.0 as *const Vec<_>,
                                span1,
                                body.1.clone(),
                                Some(as_mut(last.0.last().unwrap())),
                            ))
                        },
                    )
                },
                _ => None,
            }
        } {
            scope.push((unsafe { as_mut(&*a.0) }, unsafe { as_mut(a.1) }, a.2, a.3));
        }

        scope
    }

    macro_rules! ast {
        () => {
            ast!(all).last_mut().unwrap()
        };
        (all) => {
            get_ast(&ast, &_span1, _span2.clone())
        };
    }

    macro_rules! ast_push {
        ($val: expr, $span: expr) => {
            ast!().0.push(($val, $span.clone()));
            for i in ast!(all).iter_mut() {
                i.1.end = $span.end;
            }
        };
    }

    'main_loop: while let Some((tok, span)) = buf.next().cloned() {
        macro_rules! consider_error {
            ($expr: expr) => {
                match $expr {
                    Err((e, s)) => {
                        buf.rewind();
                        error!(e, s);
                    },
                    Ok(a) => a,
                }
            };
        }

        macro_rules! error {
            ($r: expr, $s: expr) => {{
                comp_errs.push((Box::new($r), $s));
                while let Some((t, _)) = buf.next() {
                    if matches!(t, Token::Semicolon | Token::CuBracketS) {
                        buf.rewind();
                        break;
                    }
                }
                continue 'main_loop;
            }};
        }

        macro_rules! unwrap_ident {
            ($span: expr) => {
                match buf.next() {
                    Some((Token::Ident, s)) => (src[s.start..s.end].to_string(), s.clone()),
                    Some((_, s)) => error!(ParseError::UnexpectedToken, s.clone()),
                    None => error!(ParseError::RanOutTokens, $span),
                }
            };
        }

        macro_rules! assert_token {
            ($intended: ident) => {
                match buf.next() {
                    Some((Token::$intended, _)) => (),
                    Some((_, s)) => error!(ParseError::UnexpectedToken, s.clone()),
                    None => error!(ParseError::RanOutTokens, buf.prev().unwrap().1.clone()),
                }
            };
        }

        match tok {
            Token::Semicolon => {},
            Token::Let => {
                let (ident, idspan) = unwrap_ident!(span);
                let typ = match buf.peek() {
                    Some((Token::Operator(Operator::Assign), _)) => None,
                    Some((Token::Semicolon, _)) => None,
                    Some(_) => Some(consider_error!(types::parse(buf, src))),
                    None => error!(ParseError::RanOutTokens, span),
                };
                let (expr, end) = match buf.next() {
                    Some((Token::Operator(Operator::Assign), _)) => {
                        let expr = consider_error!(exprs::parse(buf, src, false));
                        (Some(expr), buf.current().unwrap().1.start)
                    },
                    Some((Token::Semicolon, _)) => (None, buf.current().unwrap().1.start),
                    Some((_, s)) => error!(ParseError::UnexpectedToken, s.clone()),
                    None => error!(ParseError::RanOutTokens, span),
                };
                let s = Span {
                    start: span.start,
                    end,
                };
                ast_push!(
                    Node::VarDeclare {
                        ident: (ident, idspan),
                        typ,
                        expr
                    },
                    s
                );
            },
            Token::Return => {
                let (expr, end) = match buf.next() {
                    Some((Token::Semicolon, _)) => (None, buf.current().unwrap().1.start),
                    Some(_) => {
                        buf.rewind();
                        let expr = consider_error!(exprs::parse(buf, src, false));
                        (Some(expr), buf.current().unwrap().1.start)
                    },
                    None => error!(ParseError::RanOutTokens, span),
                };
                let span = Span {
                    start: span.start,
                    end,
                };
                ast_push!(Node::Return(expr), span);
            },
            Token::Fn => {
                let (ident, idspan) = unwrap_ident!(span);
                assert_token!(RoBracketS);

                let mut params = Vec::new();
                while let Some((tok, sp)) = buf.next().cloned() {
                    match tok {
                        Token::RoBracketE => break,
                        Token::Ident => {
                            let id = &src[sp.start..sp.end];
                            let typ = consider_error!(types::parse(buf, src));
                            let end = typ.1.end;
                            params.push((
                                (id.to_string(), sp.clone()),
                                typ,
                                Span {
                                    start: sp.start,
                                    end,
                                },
                            ));

                            match buf.peek() {
                                Some((Token::Comma, _)) => buf.idx += 1,
                                None => {
                                    error!(ParseError::RanOutTokens, buf.prev().unwrap().1.clone())
                                },
                                _ => (),
                            }
                        },
                        _ => error!(ParseError::UnexpectedToken, sp.clone()),
                    }
                }

                let return_type = match buf.next() {
                    Some((Token::CuBracketS, s)) => (Type::BuiltIn(BuiltInType::Unit), s.clone()),
                    Some(_) => {
                        buf.rewind();
                        let t = consider_error!(types::parse(buf, src));
                        assert_token!(CuBracketS);
                        t
                    },
                    None => error!(ParseError::RanOutTokens, buf.prev().unwrap().1.clone()),
                };
                let body = UntypedAst::new();
                let span = Span {
                    start: span.start,
                    end: buf.current().unwrap().1.end,
                };
                ast_push!(
                    Node::FunctionDeclare {
                        ident: (ident, idspan),
                        params,
                        return_type,
                        body,
                        ended: false,
                        span: span.clone()
                    },
                    span
                );
            },
            Token::If => {
                let cond = consider_error!(exprs::parse(buf, src, true));
                let body = UntypedAst::new();
                ast_push!(
                    Node::If {
                        main: vec![(cond, body, span.clone())],
                        els: None,
                        ended: false
                    },
                    span
                );
            },
            Token::While => {
                let cond = consider_error!(exprs::parse(buf, src, true));
                let body = UntypedAst::new();
                ast_push!(
                    Node::While {
                        cond,
                        body,
                        span: Span {
                            start: span.start,
                            end: buf.current().unwrap().1.end
                        },
                        ended: false
                    },
                    span
                );
            },
            Token::CuBracketS => {
                let body = UntypedAst::new();
                ast_push!(
                    Node::Scope {
                        body,
                        ended: false,
                        span: span.clone()
                    },
                    span
                );
            },
            Token::CuBracketE => match ast!() {
                (
                    _,
                    sp,
                    _,
                    Some((
                        Node::FunctionDeclare { ended, .. }
                        | Node::Scope { ended, .. }
                        | Node::If { ended, .. }
                        | Node::While { ended, .. },
                        _,
                    )),
                ) => {
                    sp.end = span.end;
                    *ended = true;
                },
                (_, _, _, None) => {
                    error!(ParseError::UnstartedBracket, span);
                },
                _ => unreachable!(),
            },
            _ => {
                buf.rewind();
                let expr = consider_error!(exprs::parse(buf, src, false));
                let span = expr.1.clone();
                ast_push!(
                    Node::Expr(expr),
                    Span {
                        start: span.start,
                        end: buf.current().unwrap().1.start
                    }
                );
            },
        }
    }

    for i in ast!(all).iter().skip(1) {
        comp_errs.push((Box::new(ParseError::UnendedScope), i.1.clone()));
    }

    if comp_errs.is_empty() {
        Ok(ast)
    } else {
        Err(comp_errs)
    }
}
*/
