use super::*;

pub fn preprocess(buf: &mut Buffer<AToken>) -> Result<Buffer<AToken>, Vec<ACompileError>> {
    let mut new  = Buffer::empty();
    let mut last = Token::None;
    while let Some(tok) = buf.next() {
        let tok = tok.clone();
        if matches!(tok.0, Token::NewLine) {
            if matches!(
                last,
                Token::Ident | Token::Integer(_) |
                Token::Break | Token::Continue | Token::Return |
                Token::RoBracketE | Token::SqBracketE |
                Token::CuBracketE
            ) && !matches!(
                buf.peek().unwrap_or(&(Token::None, Span::default())).0,
                Token::Operator(_) |
                Token::RoBracketE,
            ) {
                new.push((Token::Semicolon, tok.1));
            }
        } else {
            new.push(tok.clone());
        }
        last = tok.0;
    }
    
    Ok(new)
}
