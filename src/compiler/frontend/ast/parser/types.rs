use super::*;

impl<'a> Parser<'a> {
    pub(super) fn parse_type(&mut self) -> Option<AType> {
        match self.buf.next() {
            Some((Token::Ident, span)) => {
                let typ = Type::from_str(self.src.slice(span.clone()).unwrap());
                Some((typ, span.clone()))
            },
            Some((Token::Operator(Operator::And), Span { start, .. })) => {
                let start = *start;
                let inner = self.parse_type();
                inner.map(|inner| {
                    let end = inner.1.end;
                    (Type::Pointer(Box::new(inner)), start..end)
                })
            },
            Some((Token::Operator(Operator::AndAnd), span)) => {
                let start_1 = span.start;
                let start_2 = span.start + 1;
                let inner = self.parse_type()?;
                let end = inner.1.end;
                Some((
                    Type::Pointer(Box::new((
                        Type::Pointer(Box::new(inner)),
                        start_2..end,
                    ))),
                    start_1..end,
                ))
            },
            Some((Token::SqBracketS, Span { start, .. })) => {
                let start = *start;

                let inner = self.parse_type()?;
                match self.buf.next() {
                    Some((Token::SqBracketE, end_span)) => Some((
                        Type::Slice(Box::new(inner)),
                        start..end_span.end,
                    )),
                    Some((Token::Operator(Operator::Mlt), _)) => {
                        let size = match self.buf.next() {
                            Some((Token::Integer(size), span)) => (*size, span.clone()),
                            Some((t, span)) => {
                                self.errs.push((
                                    ParseError::UnexpectedToken {
                                        expected: Some("integer"),
                                        found: t.clone(),
                                    },
                                    span.clone(),
                                ));

                                (0, span.clone())
                            },
                            None => {
                                self.errs.push((
                                    ParseError::RanOutTokens,
                                    self.last_token().unwrap().1.clone(),
                                ));

                                (0, self.last_token().unwrap().1.clone())
                            },
                        };

                        let end = match self.buf.next() {
                            Some((Token::SqBracketE, span)) => span.end,
                            Some((t, span)) => {
                                self.errs.push((
                                    ParseError::UnexpectedToken {
                                        expected: Some("end of array"),
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
                        };

                        Some((Type::Array(Box::new(inner), size), start..end))
                    },
                    Some((t, span)) => {
                        self.errs.push((
                            ParseError::UnexpectedToken {
                                expected: Some("end of slice or asterisk"),
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
            },
            Some((Token::Fn, span)) => {
                let start = span.start;

                match self.buf.next() {
                    Some((Token::RoBracketS, _)) => {},
                    Some((t, span)) => {
                        self.errs.push((
                            ParseError::UnexpectedToken {
                                expected: Some("start of argument list"),
                                found: t.clone(),
                            },
                            span.clone(),
                        ));
                        return None;
                    },
                    None => {
                        let prev = self.buf.prev().map_or_else(Span::default, |a| a.1.clone());
                        self.errs.push((ParseError::RanOutTokens, prev));
                        return None;
                    },
                }

                let end;
                let mut args = Vec::new();

                loop {
                    match self.buf.peek() {
                        Some((Token::RoBracketE, span)) => {
                            end = span.end;
                            break;
                        },
                        Some(_) => {},
                        None => {
                            let prev = self.buf.prev().map_or_else(Span::default, |a| a.1.clone());
                            self.errs.push((ParseError::RanOutTokens, prev));
                            return None;
                        },
                    }

                    args.push(self.parse_type()?);

                    match self.buf.next() {
                        Some((Token::RoBracketE, span)) => {
                            end = span.end;
                            break;
                        },
                        Some((Token::Comma, _)) => {},
                        Some((t, span)) => {
                            self.errs.push((
                                ParseError::UnexpectedToken {
                                    expected: Some("comma"),
                                    found: t.clone(),
                                },
                                span.clone(),
                            )); // intentional no `return None`: error recover
                        },
                        None => {
                            let prev = self.buf.prev().map_or_else(Span::default, |a| a.1.clone());
                            self.errs.push((ParseError::RanOutTokens, prev));
                            return None;
                        },
                    }
                }

                match self.buf.peek() {
                    Some((Token::Ident | Token::Fn | Token::RoBracketS | Token::SqBracketS, _)) => {
                        let ret = self.parse_type()?;
                        let end = ret.1.end;
                        Some((Type::Function(args, Box::new(ret)), start..end))
                    },
                    _ => Some((Type::Function(args, Box::new((Type::BuiltIn(BuiltInType::Unit), start..end))), start..end)),
                }
            },
            Some((Token::RoBracketS, span)) => {
                let start = span.start;
                let typ = self.parse_type()?;
                match self.buf.next() {
                    Some((Token::RoBracketE, span)) => {
                        Some((typ.0, start..span.end))
                    },
                    Some((t, span)) => {
                        self.errs.push((
                            ParseError::UnexpectedToken {
                                expected: None,
                                found: t.clone(),
                            },
                            span.clone(),
                        ));
                        None
                    },
                    None => {
                        let prev = self.buf.prev().map_or_else(Span::default, |a| a.1.clone());
                        self.errs.push((ParseError::RanOutTokens, prev));
                        None
                    },
                }
            },
            Some((t, span)) => {
                self.errs.push((
                    ParseError::UnexpectedToken {
                        expected: None,
                        found: t.clone(),
                    },
                    span.clone(),
                ));
                None
            },
            None => {
                let prev = self.buf.prev().map_or_else(Span::default, |a| a.1.clone());
                self.errs.push((ParseError::RanOutTokens, prev));
                None
            },
        }
    }
}
