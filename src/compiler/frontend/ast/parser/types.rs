use super::*;

impl<'a> Parser<'a> {
    pub(super) fn parse_type(&mut self) -> Option<AType> {
        match self.buf.next() {
            Some((Token::Ident, span)) => {
                let typ = Type::from_str(self.src.slice(span.clone()).unwrap());
                Some((typ, span.clone()))
            },
            Some((Token::Operator(Operator::BAnd), Span { start, .. })) => {
                let start = *start;
                let inner = self.parse_type();
                inner.map(|inner| {
                    let end = inner.1.end;
                    (Type::Pointer(Box::new(inner)), Span { start, end })
                })
            },
            Some((Token::Operator(Operator::LAnd), span)) => {
                let start_1 = span.start;
                let start_2 = span.start + 1;
                let inner = self.parse_type()?;
                let end = inner.1.end;
                Some((
                    Type::Pointer(Box::new((
                        Type::Pointer(Box::new(inner)),
                        Span {
                            start: start_2,
                            end,
                        },
                    ))),
                    Span {
                        start: start_1,
                        end,
                    },
                ))
            },
            Some((Token::SqBracketS, Span { start, .. })) => {
                let start = *start;

                let inner = self.parse_type()?;
                match self.buf.next() {
                    Some((Token::SqBracketE, end_span)) => Some((
                        Type::Slice(Box::new(inner)),
                        Span {
                            start,
                            end: end_span.end,
                        },
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

                        Some((Type::Array(Box::new(inner), size), Span { start, end }))
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

/* pub fn parse(buf: &mut Buffer<AToken>, src: &str) -> Result<AType, (ParseError, Span)> {
    let mut typ_opers: Vec<(TypeOperators, Span)> = Vec::new();

    macro_rules! assert_token {
        ($intended: ident) => {
            match buf.next() {
                Some((Token::$intended, _)) => (),
                Some((_, s)) => return Err((ParseError::UnexpectedToken, s.clone())),
                None => return Err((ParseError::RanOutTokens, buf.prev().unwrap().1.clone())),
            }
        };
    }

    while let Some((tok, span)) = buf.next().cloned() {
        match tok {
            Token::Ident => {
                let mut typ = (Type::from_str(&src[span.start..span.end]), span);
                for (op, sp) in typ_opers.iter().rev() {
                    match op {
                        TypeOperators::Pointer => typ = (Type::Pointer(Box::new(typ)), sp.clone()),
                        TypeOperators::Slice => typ = (Type::Slice(Box::new(typ)), sp.clone()),
                        TypeOperators::Array(s) => {
                            typ = (Type::Array(Box::new(typ), *s), sp.clone())
                        },
                    }
                }
                return Ok(typ);
            },
            Token::Operator(Operator::BAnd) => typ_opers.push((TypeOperators::Pointer, span)),
            Token::Operator(Operator::LAnd) => {
                typ_opers.push((
                    TypeOperators::Pointer,
                    Span {
                        start: span.start,
                        end: span.start + 1,
                    },
                ));
                typ_opers.push((
                    TypeOperators::Pointer,
                    Span {
                        start: span.start + 1,
                        end: span.end,
                    },
                ));
            },
            Token::SqBracketS => match buf.next() {
                Some((Token::SqBracketE, sp)) => {
                    typ_opers.push((
                        TypeOperators::Slice,
                        Span {
                            start: span.start,
                            end: sp.end,
                        },
                    ));
                },
                Some((Token::Integer(i), sp)) => {
                    typ_opers.push((
                        TypeOperators::Array(*i),
                        Span {
                            start: span.start,
                            end: sp.end,
                        },
                    ));
                    assert_token!(SqBracketE);
                },
                Some((_, sp)) => return Err((ParseError::UnexpectedToken, sp.clone())),
                None => return Err((ParseError::RanOutTokens, buf.prev().unwrap().1.clone())),
            },
            _ => return Err((ParseError::UnexpectedToken, span)),
        }
    }

    Err((ParseError::RanOutTokens, buf.prev().unwrap().1.clone()))
} */
