use super::*;

impl<'a> super::Parser<'a> {
    // https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing
    #[inline(always)]
    pub(super) fn parse_expr(&mut self) -> Option<AExpr> { self.parse_expr_climb(0) }

    fn parse_expr_climb(&mut self, percedence: usize) -> Option<AExpr> {
        let mut rest = self.parse_single()?;

        while let Some((tok, _)) = self.buf.next() {
            match tok {
                Token::Operator(op) if op.is_binary() && op.percedence(false) >= percedence => {
                    let op = op.clone();

                    let right =
                        self.parse_expr_climb(op.percedence(false) + op.is_left(false) as usize)?;

                    let start = rest.1.start;
                    let end = right.1.end;

                    if op.is_left(false) {
                        rest = (
                            Expr::BiOp {
                                lhs: Box::new(rest),
                                rhs: Box::new(right),
                                op: Box::new(op),
                            },
                            Span { start, end },
                        );
                    }
                },
                Token::RoBracketS => {
                    let mut op = Vec::new();

                    let start = rest.1.start;
                    let end = assert_token!(Token::RoBracketE, "end of function call", self).end;

                    rest = (
                        Expr::FnCall {
                            id: Box::new(rest),
                            op,
                        },
                        Span { start, end },
                    );
                },
                Token::SqBracketS => {
                    let idx = self.parse_expr()?;

                    let start = rest.1.start;
                    let end =
                        assert_token!(Token::SqBracketE, "end of index square bracket", self).end;

                    rest = (
                        Expr::BiOp {
                            lhs: Box::new(rest),
                            rhs: Box::new(idx),
                            op: Box::new(Operator::Index),
                        },
                        Span { start, end },
                    );
                },
                _ => {
                    self.buf.rewind();
                    break;
                },
            }
        }

        Some(rest)
    }

    fn parse_single(&mut self) -> Option<AExpr> {
        let token = self.buf.next();
        match token {
            Some((Token::Integer(v), span)) => Some((Expr::Integer(*v as i128), span.clone())),
            Some((Token::Ident, span)) => Some((
                Expr::Ident(vec![self.src.slice(span.clone()).unwrap().to_string()]),
                span.clone(),
            )), // TODO: scopes
            Some((Token::RoBracketS, Span { start, .. })) => {
                let start = *start;
                let inner = self.parse_expr()?;
                let end = match self.buf.next() {
                    Some((Token::RoBracketE, span)) => span.end,
                    Some((t, span)) => {
                        self.errs.push((
                            ParseError::UnexpectedToken {
                                expected: Some("end of bracket"),
                                found: t.clone(),
                            },
                            span.clone(),
                        ));
                        span.end
                    }, // TODO:
                    None => {
                        self.errs.push((
                            ParseError::UnendedBracket,
                            self.last_token().unwrap().1.clone(),
                        ));

                        0
                    },
                };

                Some((inner.0, Span { start, end }))
            },
            Some((Token::Operator(op), span)) => {
                if op.is_unary() {
                    let start = span.start;
                    let op = op.clone();
                    let inner = self.parse_expr_climb(op.percedence(true))?;
                    let end = inner.1.end;
                    Some((
                        Expr::UnOp {
                            opr: Box::new(inner),
                            op: Box::new(op),
                        },
                        Span { start, end },
                    ))
                } else {
                    self.errs.push((
                        ParseError::UnexpectedToken {
                            expected: Some("unary operator"),
                            found: token.unwrap().0.clone(),
                        },
                        span.clone(),
                    ));

                    None
                }
            },
            Some((t, span)) => {
                self.errs.push((
                    ParseError::UnexpectedToken {
                        expected: Some("value, unary operator or brackets"),
                        found: t.clone(),
                    },
                    span.clone(),
                ));

                None
            },
            None => {
                self.errs.push((
                    ParseError::RanOutTokens,
                    self.last_token().unwrap().1.clone(),
                ));

                None
            }, // TODO:
        }
    }
}

/*
use super::super::*;

pub(super) fn parse(
    buf: &mut Buffer<AToken>,
    src: &str,
    ends_when_curly: bool,
) -> Result<AExpr, (ParseError, Span)> {
    let mut out = Vec::new();
    let mut ops: Vec<(Operator, Span, bool)> = Vec::new();
    let mut last = Token::None;
    let mut fn_args = Vec::new();

    macro_rules! pop_oper_to_out {
        ($op: expr, $span: expr, $unary:expr) => {
            match &$op {
                Operator::FnCall(id) => {
                    let n = fn_args.pop().unwrap();
                    let mut op = Vec::with_capacity(n);
                    let mut span = $span.clone();

                    for _ in 0..n {
                        let (o, s) = out
                            .pop()
                            .ok_or((ParseError::RanOutOperands, $span.clone()))?;
                        span.start = span.start.min(s.start);
                        span.end = span.end.max(s.end);
                        op.push((o, s));
                    }

                    op.reverse();
                    out.push((Expr::FnCall { id: id.clone(), op }, span));
                    ops.pop();
                },
                Operator::RoBracketS => {
                    ops.pop();
                },
                _ => {
                    pop_oper_to_out_no_fn!($op, $span, $unary);
                },
            }
        };
    }

    macro_rules! pop_oper_to_out_no_fn {
        ($op: expr, $span: expr, $unary:expr) => {
            if !$unary {
                let rhs = out
                    .pop()
                    .ok_or((ParseError::RanOutOperands, $span.clone()))?;
                let lhs = out
                    .pop()
                    .ok_or((ParseError::RanOutOperands, $span.clone()))?;
                let s = $span.start.min(lhs.1.start).min(rhs.1.start);
                let e = $span.end.max(lhs.1.end).max(rhs.1.end);
                let span = Span { start: s, end: e };
                out.push((
                    Expr::BiOp {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op: Box::new($op.clone()),
                    },
                    span,
                ));
            } else {
                let opr = out
                    .pop()
                    .ok_or((ParseError::RanOutOperands, $span.clone()))?;
                let s = $span.start.min(opr.1.start);
                let e = $span.end.max(opr.1.end);
                let span = Span { start: s, end: e };
                out.push((
                    Expr::UnOp {
                        opr: Box::new(opr),
                        op: Box::new($op.clone()),
                    },
                    span,
                ));
            }
            ops.pop();
        };
    }

    macro_rules! infers {
        () => {
            matches!(last, Token::RoBracketE | Token::SqBracketE | Token::Ident)
        };
    }

    macro_rules! get_ident {
        ($e: expr) => {{
            let e = $e;
            match e.0 {
                Expr::Ident(id) => id,
                _ => return Err((ParseError::ExpectingIdentifier, e.1)),
            }
        }};
    }

    let mut expr_span = buf.peek().map_or_else(
        || buf.current().map_or_else(Span::default, |a| a.1.clone()),
        |a| a.1.clone(),
    );

    while let Some((tok, span)) = buf.next() {
        let tok = tok.clone();
        let span = span.clone();

        match &tok {
            Token::Semicolon => break,
            Token::Integer(i) => {
                if matches!(
                    last,
                    Token::Integer(_) | Token::Ident | Token::RoBracketE | Token::SqBracketE
                ) {
                    return Err((ParseError::UnexpectedTokenLegacy, span));
                }
                out.push((Expr::Integer(*i as i128), span.clone()));
            },
            Token::Ident => {
                if matches!(
                    last,
                    Token::Integer(_) | Token::Ident | Token::RoBracketE | Token::SqBracketE
                ) {
                    return Err((ParseError::UnexpectedTokenLegacy, span));
                } else {
                    out.push((
                        Expr::Ident(vec![src[span.start..span.end].to_string()]),
                        span.clone(),
                    ))
                }
            },
            Token::Operator(o1) => {
                let un = matches!(last, Token::Operator(_) | Token::None);
                while let Some((o2, span, unary)) = ops.last() {
                    if matches!(o2, Operator::RoBracketS | Operator::FnCall(_))
                        || !(o2.percedence(*unary) > o1.percedence(un)
                            || (o1.percedence(un) == o2.percedence(*unary) && o1.is_left()))
                    {
                        break;
                    }

                    pop_oper_to_out_no_fn!(o2, span, unary);
                }
                ops.push((o1.clone(), span.clone(), un));
            },
            Token::ScopeOf => {
                let p = out
                    .pop()
                    .ok_or((ParseError::RanOutOperands, span.clone()))?;
                let start = p.1.start;
                let mut prev = get_ident!(p);
                let next = match buf
                    .next()
                    .ok_or((ParseError::RanOutOperands, span.clone()))?
                {
                    (Token::Ident, span) => (src[span.start..span.end].to_string(), span.clone()),
                    (_, span) => return Err((ParseError::UnexpectedTokenLegacy, span.clone())),
                };
                let end = next.1.end;
                prev.push(next.0);
                out.push((Expr::Ident(prev), Span { start, end }));
            },
            Token::Comma => {
                while let Some((op, span, unary)) = ops.last() {
                    if matches!(op, Operator::FnCall(_)) {
                        break;
                    }

                    pop_oper_to_out_no_fn!(op, span, unary);
                }
                if !matches!(buf.peek(), Some((Token::RoBracketE, _))) {
                    *fn_args
                        .last_mut()
                        .ok_or((ParseError::UnexpectedTokenLegacy, span.clone()))? += 1;
                }
            },
            Token::RoBracketS => {
                if matches!(last, Token::Integer(_)) {
                    return Err((ParseError::UnexpectedTokenLegacy, span));
                } else if infers!() {
                    // fn calls
                    let func = out.pop().unwrap();
                    let start = func.1.start;
                    ops.push((
                        Operator::FnCall(get_ident!(func)),
                        Span {
                            start,
                            end: span.end,
                        },
                        false,
                    ));
                    fn_args.push(!matches!(
                        buf.peek().unwrap_or(&(Token::None, Span::default())).0,
                        Token::RoBracketE
                    ) as _);
                } else {
                    ops.push((Operator::RoBracketS, span.clone(), false));
                }
            },
            Token::RoBracketE => {
                let mut errs = true;
                while let Some((op, s, unary)) = ops.last().cloned() {
                    pop_oper_to_out!(op, s, unary);
                    if matches!(op, Operator::FnCall(_) | Operator::RoBracketS) {
                        errs = false;
                        break;
                    } else if matches!(op, Operator::Index) {
                        return Err((ParseError::BracketNotMatch, span));
                    }
                }

                if errs {
                    return Err((ParseError::UnstartedBracket, span));
                }
            },
            Token::SqBracketS => {
                if infers!() {
                    ops.push((Operator::Index, span.clone(), false));
                } else {
                    return Err((ParseError::UnexpectedTokenLegacy, span));
                }
            },
            Token::SqBracketE => {
                let mut errs = true;
                while let Some((op, s, unary)) = ops.last().cloned() {
                    let s = Span {
                        start: s.start,
                        end: span.end,
                    };
                    pop_oper_to_out!(op, s, unary);
                    if matches!(op, Operator::Index) {
                        errs = false;
                        break;
                    } else if matches!(op, Operator::FnCall(_) | Operator::RoBracketS) {
                        return Err((ParseError::BracketNotMatch, span));
                    }
                }

                if errs {
                    return Err((ParseError::UnstartedBracket, span));
                }
            },
            Token::CuBracketS => {
                if ends_when_curly {
                    break;
                } else {
                    return Err((ParseError::UnexpectedTokenLegacy, span));
                }
            },
            _ => return Err((ParseError::UnexpectedTokenLegacy, span)),
        }

        expr_span.start = expr_span.start.min(span.start);
        expr_span.end = expr_span.end.max(span.end);
        last = buf.current().unwrap().0.clone();
    }

    while let Some((op, span, unary)) = ops.last().cloned() {
        if matches!(op, Operator::RoBracketS | Operator::Index) {
            return Err((ParseError::UnendedBracket, span));
        } else if matches!(op, Operator::FnCall(_)) {
            return Err((ParseError::UnendedFnCall, span));
        }

        pop_oper_to_out!(op, span, unary);
    }

    if out.len() == 1 {
        Ok(out[0].clone())
    } else {
        Err((ParseError::ExprParseError, expr_span))
    }
} */
