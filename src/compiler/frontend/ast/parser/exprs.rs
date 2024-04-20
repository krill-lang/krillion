use super::*;

impl<'a> super::Parser<'a> {
    // https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing
    #[inline(always)]
    pub(super) fn parse_expr(&mut self) -> Option<AExpr> { self.parse_expr_climb(0) }

    fn parse_expr_climb(&mut self, percedence: usize) -> Option<AExpr> {
        let mut rest = self.parse_single()?;

        while let Some((tok, _)) = self.buf.next() {
            match tok {
                Token::Operator(op) if op.is_binary() && op.percedence() >= percedence => {
                    let op = op.clone();

                    let right = self.parse_expr_climb(op.percedence() + op.is_left() as usize)?;

                    let start = rest.1.start;
                    let end = right.1.end;

                    if op.is_left() {
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

                    while !matches!(self.buf.peek(), Some((Token::RoBracketE, _))) {
                        op.push(self.parse_expr()?);

                        match self.buf.peek() {
                            Some((Token::Comma, _)) => self.buf.idx += 1,
                            Some((Token::RoBracketE, _)) => break,
                            Some((t, s)) => self.errs.push((
                                ParseError::UnexpectedToken {
                                    expected: Some("comma or end of function call"),
                                    found: t.clone(),
                                },
                                s.clone(),
                            )),
                            None => self.errs.push((
                                ParseError::RanOutTokens,
                                self.last_token().unwrap().1.clone(),
                            )),
                        }
                    }

                    let end = self.buf.next().map_or(0, |a| a.1.end);

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
            Some((Token::Ident, span)) => {
                let mut total_span = span.clone();
                let mut segs = vec![(
                    self.src.slice(span.clone()).unwrap().to_string(),
                    span.clone(),
                )];

                while let Some((Token::ModSep, _)) = self.buf.peek() {
                    self.buf.next();

                    match self.buf.next() {
                        Some((Token::Ident, span)) => {
                            total_span.end = span.end;
                            segs.push((
                                self.src.slice(span.clone()).unwrap().to_string(),
                                span.clone(),
                            ))
                        },
                        Some((t, span)) => {
                            self.errs.push((
                                ParseError::UnexpectedToken {
                                    expected: Some("identifier"),
                                    found: t.clone(),
                                },
                                span.clone(),
                            ));

                            return None;
                        },
                        None => {
                            self.errs.push((
                                ParseError::RanOutTokens,
                                self.last_token().unwrap().1.clone(),
                            ));

                            return None;
                        },
                    }
                }

                Some((Expr::Ident(segs), total_span))

                // Some((
                //     Expr::Ident(vec![self.src.slice(span.clone()).unwrap().to_string()]),
                //     span.clone(),
                // )),
            },
            Some((Token::RoBracketS, Span { start, .. })) => {
                let start = *start;
                let inner = self.parse_expr()?;
                let end = match self.buf.next() {
                    Some((Token::RoBracketE, span)) => span.end,
                    Some((t, span)) => {
                        self.errs.push((
                            ParseError::UnexpectedToken {
                                expected: Some("end of round bracket"),
                                found: t.clone(),
                            },
                            span.clone(),
                        ));
                        span.end
                    },
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
                let span = span.clone();
                let this = token.unwrap().clone();
                let op = op.clone();
                let single_op = &[op.clone()];
                let ops = op.break_down().unwrap_or(single_op);

                let mut acc = self.parse_expr_climb(ops[0].to_unary().map_or(0, |a| a.percedence()))?;

                for op in ops.iter() {
                    if let Some(op) = op.to_unary() {
                        let start = span.start;
                        let op = op.clone();
                        let end = acc.1.end;
                        acc = (
                            Expr::UnOp {
                                opr: Box::new(acc),
                                op: Box::new(op),
                            },
                            Span { start, end },
                        );
                    } else {
                        self.errs.push((
                            ParseError::UnexpectedToken {
                                expected: Some("unary operator"),
                                found: this.0,
                            },
                            span,
                        ));

                        return None;
                    }
                }

                Some(acc)
            },
            Some((t, span)) => {
                self.errs.push((
                    ParseError::UnexpectedToken {
                        expected: Some("value, unary operator or start of round bracket"),
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
            },
        }
    }
}
