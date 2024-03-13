use super::*;

pub fn parse(buf: &mut Buffer<AToken>, src: &str, errs: &mut Errors) -> Option<AType> {
    match buf.next() {
        Some((Token::Ident, span)) => {
            let typ = Type::from_str(src.slice(span.clone()).unwrap());
            Some((typ, span.clone()))
        },
        Some((Token::Operator(Operator::BAnd), Span { start, .. })) => {
            let start = *start;
            let inner = parse(buf, src, errs);
            inner.map(|inner| {
                let end = inner.1.end;
                (Type::Pointer(Box::new(inner)), Span { start, end })
            })
        },
        Some((Token::Operator(Operator::LAnd), span)) => {
            let start_1 = span.start;
            let start_2 = span.start + 1;
            let inner = parse(buf, src, errs);
            inner.map(|inner| {
                let end = inner.1.end;
                (
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
                )
            })
        },
        Some((t, span)) => {
            errs.push((
                ParseError::UnexpectedToken {
                    expected: None,
                    found: t.clone(),
                },
                span.clone(),
            ));
            None
        },
        None => {
            let prev = buf.prev().map_or_else(Span::default, |a| a.1.clone());
            errs.push((ParseError::RanOutTokens, prev));
            None
        },
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
