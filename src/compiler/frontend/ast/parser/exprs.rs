use super::super::*;
use crate::compiler::util::*;

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
                        let (o, s) = wrap_option!(out.pop(), (ParseError::RanOutOperands, $span.clone()))?;
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
                _ => { pop_oper_to_out_no_fn!($op, $span, $unary); },
            }
        };
    }

    macro_rules! pop_oper_to_out_no_fn {
        ($op: expr, $span: expr, $unary:expr) => {
            if !$unary {
                let rhs = wrap_option!(out.pop(), (ParseError::RanOutOperands, $span.clone()))?;
                let lhs = wrap_option!(out.pop(), (ParseError::RanOutOperands, $span.clone()))?;
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
                let opr = wrap_option!(out.pop(), (ParseError::RanOutOperands, $span.clone()))?;
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

    let mut expr_span = Span {
        start: usize::MAX,
        end: usize::MIN,
    };

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
                    return Err((ParseError::UnexpectedToken, span));
                }
                out.push((Expr::Integer(*i as i128), span.clone()));
            },
            Token::Ident => {
                if matches!(
                    last,
                    Token::Integer(_) | Token::Ident | Token::RoBracketE | Token::SqBracketE
                ) {
                    return Err((ParseError::UnexpectedToken, span));
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
                let p = wrap_option!(out.pop(), (ParseError::RanOutOperands, span.clone()))?;
                let start = p.1.start;
                let mut prev = get_ident!(p);
                let next =
                    match wrap_option!(buf.next(), (ParseError::RanOutOperands, span.clone()))? {
                        (Token::Ident, span) => {
                            (src[span.start..span.end].to_string(), span.clone())
                        },
                        (_, span) => return Err((ParseError::UnexpectedToken, span.clone())),
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
                    *wrap_option!(
                        fn_args.last_mut(),
                        (ParseError::UnexpectedToken, span.clone())
                    )? += 1;
                }
            },
            Token::RoBracketS => {
                if matches!(last, Token::Integer(_)) {
                    return Err((ParseError::UnexpectedToken, span));
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
                    return Err((ParseError::UnexpectedToken, span));
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
                    return Err((ParseError::UnexpectedToken, span));
                }
            },
            _ => return Err((ParseError::UnexpectedToken, span)),
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
}
