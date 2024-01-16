pub use logos::*;

#[derive(Debug, Clone, Logos)]
pub enum HighlightToken {
    #[regex(r"([\d_]+|0x[\da-fA-F_]+|0b[01_]+)", priority = 10)]
    Integer,
    #[regex(r"[_a-zA-Z0-9\u0100-\x{fffff}]+", priority = 2)]
    Ident,

    #[token("str")]
    #[token("int")]
    #[token("uint")]
    #[token("unit")]
    #[regex(r"[iu](8|16|32|64|128)", priority = 10)]
    BuiltIn,

    #[token(";")]
    #[regex(r"[\s]+", priority = 3)]
    #[regex(r"//[^\n]*")]
    #[regex(r"[^\\]?\\\n")]
    #[token(r"/*")]
    #[token(r"*/")]
    Unused,

    #[token("(")]
    RoBracketS,

    #[token(")")]
    #[token("[")]
    #[token("]")]
    #[token("{")]
    #[token("}")]
    Brackets,

    #[regex(r"(\+|\-|\*|/|%|&|\||\^|<<|>>)(=)?", priority = 3)]
    #[regex(r"(<|>|!|==|!=|<=|>=|&&|\|\||=)", priority = 3)]
    Operator,

    #[token("let")]
    #[token("fn")]
    #[token("return")]
    #[token("pub")]
    #[token("extern")]
    #[token("static")]
    #[token("if")]
    #[token("else")]
    #[token("for")]
    #[token("while")]
    #[token("break")]
    #[token("continue")]
    Keyword,

    #[token(",")]
    Comma,
    #[token("::")]
    Scope,
    #[token(".")]
    Of,
}

pub fn highlight(
    this: Result<HighlightToken, ()>,
    next: &Option<Result<HighlightToken, ()>>,
    text: &str
) -> String {
    use HighlightToken::*;
    String::new()
        + "\x1b["
        + match (this, next) {
            (Ok(Ident), Some(Ok(RoBracketS))) => "34",
            (Ok(Ident | RoBracketS | Brackets | Comma | Of), _) => "37",
            (Ok(Integer | BuiltIn), _) => "33",
            (Ok(Keyword | Operator), _) => "35",
            (Ok(Unused | Scope), _) => "90",
            (Err(_), _) => "1;31",
        }
        + "m"
        + text
}
