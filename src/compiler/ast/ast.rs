use super::super::*;

macro_rules! wrap_result {
    ($res: expr, $fail: expr) => {
        match $res {
            Ok(a) => Ok(a),
            Err(_) => Err($fail),
        }
    };
}

macro_rules! wrap_option {
    ($res: expr, $fail: expr) => {
        match $res {
            Some(a) => Ok(a),
            None => Err($fail),
        }
    };
}

pub fn parse(buf: &mut Buffer<AToken>, src: &str) -> Result<AST, AParseError> {
    let mut ast = AST::new();

    while let Some((tok, span)) = buf.next() {
        let tok  = tok.clone();
        let span = span.clone();
        match tok {
            Token::Semicolon => {},
            Token::Var => {
                let ident = match buf.next() {
        Some((Token::Ident, s)) => src[s.start..s.end].to_string(),
                    _ => panic!()
                };
                let (expr, end) = match buf.next() {
                    Some((Token::Operator(Operator::Assign), _)) => {
                        let expr = parse_expr(buf, src)?;
                        (Some(expr), buf.current().unwrap().1.start)
                    },
                    Some((Token::Semicolon, _)) => (None, buf.current().unwrap().1.start),
                    _ => panic!()
                };
                let span = Span { start: span.start, end };
                ast.push((Node::VarDeclare { ident, expr }, span));
            },
            _ => {
                buf.rewind();
                let expr = parse_expr(buf, src)?;
                let span = expr.1.clone();
                ast.push((Node::Expr(expr), Span { start: span.start, end: buf.current().unwrap().1.start }));
            },
        }
    }

    Ok(ast)
}

fn parse_expr(buf: &mut Buffer<AToken>, src: &str) -> Result<AExpr, AParseError> {
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
                        let (o, s) = out.pop().unwrap();
                        span.start = span.start.min(s.start);
                        span.end = span.end.max(s.end);
                        op.push((o, s));
                    }


                    op.reverse();
                    out.push((Expr::FnCall { id: id.clone(), op }, span));
                    ops.pop();
                },
                _ => (),
            }
            pop_oper_to_out_no_fn!($op, $span, $unary);
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
                out.push((Expr::BiOp { lhs: Box::new(lhs), rhs: Box::new(rhs), op: $op.clone() }, span));
            } else {
                let opr = wrap_option!(out.pop(), (ParseError::RanOutOperands, $span.clone()))?;
                let s = $span.start.min(opr.1.start);
                let e = $span.end.max(opr.1.end);
                let span = Span { start: s, end: e };
                out.push((Expr::UnOp { opr: Box::new(opr), op: $op.clone() }, span));
            }
            ops.pop();
        };
    }

    let mut expr_span = Span { start: usize::MAX, end: usize::MIN };

    while let Some((tok, span)) = buf.next() {
        let tok = tok.clone();
        let span = span.clone();
        expr_span.start = expr_span.start.min(span.start);
        expr_span.end = expr_span.end.max(span.end);

        match &tok {
            Token::Semicolon => break,
            Token::Integer(i) => out.push((Expr::Integer(*i), span.clone())),
            Token::Ident => {
                if matches!(buf.peek().unwrap_or(&(Token::None, Span::default())).0, Token::RoBracketS) {
                    let (_, nspan) = buf.next().unwrap();
                    ops.push((Operator::FnCall(src[span.start..span.end].to_string()), Span { start: span.start, end: nspan.end }, false));
                    fn_args.push(if matches!(buf.peek().unwrap_or(&(Token::None, Span::default())).0, Token::RoBracketE) { 0 } else { 1 });
                } else {
                    out.push((Expr::Ident(src[span.start..span.end].to_string()), span.clone()))
                }
            },
            Token::Operator(o1) => {
                let un = matches!(last, Token::Operator(_) | Token::None);
                while let Some((o2, span, unary)) = ops.last() {
                    if !(o2.percedence(*unary) > o1.percedence(un) || (o1.percedence(un) == o2.percedence(*unary) && o1.is_left())) {
                        break;
                    }

                    pop_oper_to_out_no_fn!(o2, span, unary);
                }
                ops.push((o1.clone(), span.clone(), un));
            },
            Token::Comma => {
                while let Some((op, span, unary)) = ops.last() {
                    if matches!(op, Operator::FnCall(_)) {
                        break;
                    }

                    pop_oper_to_out_no_fn!(op, span, unary);
                }
                *wrap_option!(fn_args.last_mut(), (ParseError::UnexpectedToken(Token::Comma), span))? += 1;
            },
            Token::RoBracketS => ops.push((Operator::LBrack, span.clone(), false)),
            Token::RoBracketE => {
                while let Some((op, span, unary)) = ops.last().cloned() {
                    if matches!(op, Operator::LBrack) {
                        ops.pop();
                        break;
                    }
                    pop_oper_to_out!(op, span, unary);
                    if matches!(op, Operator::FnCall(_)) {
                        break;
                    }
                }
            },
            _ => todo!("{tok:?}"),
        }
        last = tok.clone();
    }

    while let Some((op, span, unary)) = ops.last().cloned() {
        if matches!(op, Operator::LBrack) {
            return Err((ParseError::UnendedBracket, span.clone()));
        } else if matches!(op, Operator::FnCall(_)) {
            return Err((ParseError::UnendedFnCall, span.clone()));
        }

        pop_oper_to_out!(op, span, unary);
    }

    if out.len() == 1 {
        Ok(out[0].clone())
    } else {
        Err((ParseError::ExprParseError, expr_span))
    }
}
