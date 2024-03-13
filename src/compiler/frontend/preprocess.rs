use super::*;

pub fn preprocess(buf: &mut Buffer<AToken>) -> (Buffer<AToken>, Vec<AError<LexerError>>) {
    let mut new = Buffer::with_capacity(buf.buf.len());
    let mut last = Token::None;
    while let Some(tok) = buf.next() {
        let tok = tok.clone();
        if matches!(tok.0, Token::NewLine) {
            if matches!(
                last,
                Token::Ident
                    | Token::Integer(_)
                    | Token::Break
                    | Token::Continue
                    | Token::Return
                    | Token::RoBracketE
                    | Token::SqBracketE
                    | Token::CuBracketE
            ) && !matches!(
                buf.peek().unwrap_or(&(Token::None, Span::default())).0,
                Token::Of | Token::RoBracketE,
            ) {
                new.push((Token::Semicolon, Span {
                    start: tok.1.start,
                    end: tok.1.start,
                }));
            }
        } else {
            new.push(tok.clone());
        }
        last = tok.0;
    }

    (new, Vec::new())
}
