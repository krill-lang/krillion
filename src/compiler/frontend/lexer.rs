use super::*;
pub use logos::*;
use std::fmt;

#[derive(Debug, Clone, Logos)]
#[logos(skip r"\s")]
#[logos(skip r"//[^\n]*")]
#[logos(skip r"[^\\]?\\\n")]
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]
pub enum Token {
    #[regex(r"([\d][\d_]*|0x[\da-fA-F_]+|0b[01_]+)", priority = 10, callback = parse_int)]
    Integer(u128),
    #[regex(r"[_a-zA-Z0-9\u0100-\x{fffff}]+", priority = 2)]
    Ident,

    #[token(";", callback = |_| false)]
    Semicolon(bool),

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
    ModSep,
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
            Self::Semicolon(true) => write!(f, "newline"),
            Self::Semicolon(false) => write!(f, "semicolon"),
            Self::RoBracketS => write!(f, "start of round bracket"),
            Self::RoBracketE => write!(f, "end of round bracket"),
            Self::SqBracketS => write!(f, "start ofsquare bracket"),
            Self::SqBracketE => write!(f, "end of square bracket"),
            Self::CuBracketS => write!(f, "start of curly bracket"),
            Self::CuBracketE => write!(f, "end of curly bracket"),
            Self::Operator(_) => write!(f, "operator"),
            Self::ModSep => write!(f, "module qualifier"),
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
    // binary
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
    And,
    Or,
    Xor,
    Not,
    AndAnd,
    OrOr,

    LSh,
    RSh,

    // unary
    Plus,
    Minus,
    Deref,
    Ref,
}

impl Operator {
    pub const fn percedence(&self) -> usize {
        match self {
            Self::Plus | Self::Minus | Self::Deref | Self::Ref | Self::Not | Self::Index => 15,
            Self::Mlt | Self::Div | Self::Mod => 12,
            Self::Add | Self::Sub => 11,
            Self::LSh | Self::RSh => 10,
            Self::LT | Self::LE | Self::GT | Self::GE => 9,
            Self::Eq | Self::NE => 8,
            Self::And => 7,
            Self::Xor => 6,
            Self::Or => 5,
            Self::AndAnd => 4,
            Self::OrOr => 3,
            Self::Assign | Self::OpAssign(_) => 1,
        }
    }

    pub const fn is_left(&self) -> bool {
        !matches!(
            self,
            Self::Assign
                | Self::OpAssign(_)
                | Self::Plus
                | Self::Minus
                | Self::Deref
                | Self::Ref
                | Self::Not
        )
    }

    pub const fn to_unary(&self) -> Option<Self> {
        match self {
            Self::Add => Some(Self::Plus),
            Self::Sub => Some(Self::Minus),
            Self::Mlt => Some(Self::Deref),
            Self::And => Some(Self::Ref),
            Self::Not => Some(Self::Not),
            _ => None,
        }
    }

    pub const fn is_binary(&self) -> bool { !self.is_unary() }

    pub const fn is_unary(&self) -> bool {
        matches!(
            self,
            Self::Plus | Self::Minus | Self::Deref | Self::Ref | Self::Not
        )
    }

    pub const fn break_down(&self) -> Option<&'static [Self]> {
        match self {
            Self::AndAnd => Some(&[Self::And, Self::And]),
            Self::OrOr => Some(&[Self::Or, Self::Or]),
            _ => None,
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
        "&&" => AndAnd,
        "||" => OrOr,

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
        "|" => Or,
        "&" => And,
        "^" => Xor,
        "<<" => LSh,
        ">>" => RSh,
        _ => unreachable!(),
    }
}

pub type AToken = Annotated<Token>;
