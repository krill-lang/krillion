pub use logos::*;

#[derive(Debug, Clone, Logos)]
pub enum HighlightToken {
    #[regex(r"([\d_]+|0x[\da-fA-F_]+|0b[01_]+)", priority = 2)]
    Integer,
    #[regex(r"[_\p{L}\p{S}\p{N}]+", priority = 1)]
    Ident,

    #[token("str")]
    #[token("int")]
    #[token("uint")]
    #[regex(r"[iu](8|16|32|64|128)", priority = 3)]
    BuiltIn,

    #[token(";")]
    #[regex(r"[\s]+")]
    #[regex(r"//[^\n]*")]
    #[regex(r"/\*([^*]|\*[^/])*\*/")]
    Unused,

    #[token("(")]
    RoBracketS,

    #[token(")")]
    #[token("[")]
    #[token("]")]
    #[token("{")]
    #[token("}")]
    Brackets,

    #[regex(r"(\+|\-|\*|/|%|&|\||\^|<<|>>)(=)?", priority = 2)]
    #[regex(r"(<|>|!|==|!=|<=|>=|&&|\|\||=|\.|::)", priority = 2)]
    Operator,

    #[token("let")]
    #[token("fn")]
    #[token("return")]
    #[token("if")]
    #[token("else")]
    #[token("for")]
    #[token("while")]
    #[token("break")]
    #[token("continue")]
    Keyword,

    #[token(",")]
    Comma,

    #[regex(r".", priority = 0)]
    Unknown,
}

impl HighlightToken {
    pub fn highlight(&self, next: Option<&Self>, text: &str) -> String {
        use HighlightToken::*;
        String::new() + "\x1b[" +
        match (self, next) {
            (Ident, Some(RoBracketS))
                => "34",
            (Ident | RoBracketS | Brackets | Comma, _)
                => "37",
            (Integer | BuiltIn, _)
                => "33",
            (Keyword | Operator, _)
                => "35",
            (Unused, _)
                => "90",
            (Unknown, _)
                => "1;31",
        } + "m" + text + "\x1b[0m"
    }
}
