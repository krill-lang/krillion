pub use logos::*;
use super::*;

#[derive(Debug, Clone, Logos)]
pub enum HighlightToken {
    #[regex(r"([\d_]+|0x[\da-fA-F_]+|0b[01_]+)", priority = 2)]
    Integer,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", priority = 1)]
    Ident,

    #[token("str")]
    #[token("int")]
    #[token("uint")]
    #[regex(r"[iu](8|16|32|64|128)")]
    BuiltIn,

    #[token(";")]
    #[regex(r"//[^\n]*")]
    #[regex(r"[\s]")]
    Unused,

    #[token("(")]
    RoBracketS,

    #[token(")")]
    #[token("[")]
    #[token("]")]
    #[token("{")]
    #[token("}")]
    Brackets,

    #[regex(r"(\+|\-|\*|/|%|&|\||\^|<<|>>)(=)?")]
    #[regex(r"(<|>|!|==|!=|<=|>=|&&|\|\||=|\.|::)")]
    Operator,

    #[token("let")]
    #[token("break")]
    #[token("continue")]
    #[token("return")]
    #[token("fn")]
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
            (Ident | RoBracketS | Brackets | Comma | Unknown, _)
                => "0",
            (Integer | BuiltIn, _)
                => "33",
            (Keyword | Operator, _)
                => "35",
            (Unused, _)
                => "90",
        } + "m" + text + "\x1b[0m"
    }
}
