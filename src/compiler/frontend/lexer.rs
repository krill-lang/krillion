use super::*;
pub use logos::*;
use std::fmt;

#[derive(Debug, Clone, Logos)]
#[logos(skip r"\s")]
#[logos(skip r"//[^\n]*")]
#[logos(skip r"[^\\]?\\\n")]
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]
pub enum Token {
    #[regex(r"([\d_]+|0x[\da-fA-F_]+|0b[01_]+)", callback = parse_int)]
    Integer(u128),
    #[regex(r"[_a-zA-Z0-9\u0100-\x{fffff}]+", priority = 0)]
    Ident,

    #[token(";")]
    Semicolon,

    #[token("(")]
    RoBracketS,
    #[token(")")]
    RoBracketE,

    #[token("[")]
    SqBracketS,
    #[token("]")]
    SqBracketE,

    #[token("{")]
    CuBracketS,
    #[token("}")]
    CuBracketE,

    #[regex(r"(\+|\-|\*|/|%|&|\||\^|<<|>>)(=)?", callback = parse_operator)]
    #[regex(r"(<|>|!|==|!=|<=|>=|&&|\|\||=)", callback = parse_operator)]
    Operator(Operator),

    #[token("::")]
    ScopeOf,
    #[token(".")]
    Of,

    #[token("let")]
    Let,

    #[token("fn")]
    Fn,
    #[token("return")]
    Return,

    #[token("pub")]
    Pub,

    #[token("extern")]
    Extern,
    #[token("static")]
    Static,

    #[token("if")]
    If,
    #[token("else")]
    Else,

    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,

    #[token(",")]
    Comma,

    #[regex(r"[\n]+", priority = 10)]
    NewLine,

    None,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Integer(_) => write!(f, "integer"),
            Self::Ident => write!(f, "identifier"),
            Self::Semicolon => write!(f, "semicolon"),
            Self::RoBracketS => write!(f, "bracket"),
            Self::RoBracketE => write!(f, "bracket"),
            Self::SqBracketS => write!(f, "square bracket"),
            Self::SqBracketE => write!(f, "square bracket"),
            Self::CuBracketS => write!(f, "curly bracket"),
            Self::CuBracketE => write!(f, "curly bracket"),
            Self::Operator(_) => write!(f, "operator"),
            Self::ScopeOf => write!(f, "scope separator"),
            Self::Of => write!(f, "dot"),
            Self::Comma | Self::NewLine | Self::None => {
                write!(f, "{}", format!("{self:?}").to_lowercase())
            },
            _ => write!(f, "`{}`", format!("{self:?}").to_lowercase()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mlt,
    Div,
    Mod,
    Assign,
    OpAssign(Box<Operator>),

    Index,
    Eq,
    NE,
    GT,
    GE,
    LT,
    LE,
    BAnd,
    BOr,
    BXOr,
    Not,
    LAnd,
    LOr,

    LSh,
    RSh,
}

impl Operator {
    pub fn percedence(&self, unary: bool) -> usize {
        match (self, unary) {
            (Self::Assign | Self::OpAssign(_), false) => 1,
            (Self::LT | Self::LE | Self::GT | Self::GE, false) => 9,
            (Self::LSh | Self::RSh, false) => 10,
            (Self::Add | Self::Sub, false) => 11,
            (Self::Mlt | Self::Div | Self::Mod, false) => 12,
            (Self::Add | Self::Sub, true) => 15,
            _ => todo!()
        }
    }

    pub fn is_left(&self, unary: bool) -> bool {
        match (self, unary) {
            (Self::Assign | Self::OpAssign(_), _) | (Self::Add | Self::Sub, true) => false,
            _ => true,
        }
    }

    pub fn is_binary(&self) -> bool {
        match self {
            _ => true,
        }
    }

    pub fn is_unary(&self) -> bool {
        match self {
            Self::Add | Self::Sub => true,
            _ => false,
        }
    }
}

fn parse_int(lex: &Lexer<Token>) -> u128 {
    let s = lex.slice().replace('_', "");
    match s.chars().nth(1).unwrap_or(' ') {
        'x' => u128::from_str_radix(&s[2..], 16),
        'b' => u128::from_str_radix(&s[2..], 2),
        _ => s.parse(),
    }
    .unwrap()
}

fn parse_operator(lex: &Lexer<Token>) -> Operator {
    let s = lex.slice();

    use Operator::*;
    match s {
        "==" => Eq,
        "!=" => NE,
        "<" => LT,
        "<=" => LE,
        ">" => GT,
        ">=" => GE,
        "!" => Not,
        "&&" => LAnd,
        "||" => LOr,

        _ => {
            if s.ends_with('=') && s.len() > 1 {
                Operator::OpAssign(Box::new(_parse_oper(&s[0..s.len() - 1])))
            } else {
                _parse_oper(s)
            }
        },
    }
}

fn _parse_oper(s: &str) -> Operator {
    use Operator::*;
    match s {
        "+" => Add,
        "-" => Sub,
        "*" => Mlt,
        "/" => Div,
        "%" => Mod,
        "=" => Assign,
        "|" => BOr,
        "&" => BAnd,
        "^" => BXOr,
        "<<" => LSh,
        ">>" => RSh,
        _ => unreachable!(),
    }
}

pub type AToken = Annotated<Token>;
