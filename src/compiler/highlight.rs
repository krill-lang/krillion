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
    #[regex(r"[iu](8|16|32|64|128)", priority = 3)]
    BuiltIn,

    #[token(";")]
    #[regex(r"[\s]+", priority = 3)]
    #[regex(r"//[^\n]*")]
    #[regex(r"[^\\]?\\\n")]
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

    #[regex(r"(\+|\-|\*|/|%|&|\||\^|<<|>>)(=)?", priority = 3)]
    #[regex(r"(<|>|!|==|!=|<=|>=|&&|\|\||=)", priority = 3)]
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
    #[token("::")]
    Scope,
    #[token(".")]
    Of,

    #[regex(r".", priority = 1)]
    Unknown,
}

impl HighlightToken {
    pub fn highlight(&self, next: Option<&Self>, text: &str) -> String {
        use HighlightToken::*;
        String::new()
            + "\x1b["
            + match (self, next) {
                (Ident, Some(RoBracketS)) => "34",
                (Ident | RoBracketS | Brackets | Comma | Of, _) => "37",
                (Integer | BuiltIn, _) => "33",
                (Keyword | Operator, _) => "35",
                (Unused | Scope, _) => "90",
                (Unknown, _) => "1;31",
            }
            + "m"
            + text
    }
}
