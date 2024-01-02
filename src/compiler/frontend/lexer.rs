pub use logos::*;
use super::*;

#[derive(Debug, Clone, Logos)]
#[logos(skip r"\s")]
#[logos(skip r"//[^\n]*")]
#[logos(skip r"[^\\]?\\\n")]
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]
pub enum Token {
    #[regex(r"([\d_]+|0x[\da-fA-F_]+|0b[01_]+)", callback = parse_int)]
    Integer(u128),
    #[regex(r"[_\p{L}\p{S}\p{N}]+", priority = 0)]
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

    None
}

#[derive(Debug, Clone)]
pub enum Operator {
    Add, Sub, Mlt, Div, Mod,
    Assign, OpAssign(Box<Operator>),

    RoBracketS, Index,
    Eq, NE, GT, GE, LT, LE,
    BAnd, BOr, BXOr, Not,
    LAnd, LOr,

    LSh, RSh,
    FnCall(Identifier),
}

impl Operator {
    pub const fn percedence(&self, unary: bool) -> usize {
        use Operator::*;
        if !unary {
            match self {
                Index
                    => 14,
                RoBracketS | FnCall(_)
                    => 13,
                Mlt | Div | Mod
                    => 11,
                Add | Sub
                    => 10,
                LSh | RSh
                    => 9,
                LT | LE | GT | GE
                    => 8,
                Eq | NE
                    => 7,
                BAnd
                    => 6,
                BXOr
                    => 5,
                BOr
                    => 4,
                LAnd
                    => 3,
                LOr
                    => 2,
                Assign | OpAssign(_)
                    => 1,
                _ => 0,
            }
        } else {
            match self {
                Add | Sub | Mlt | Not
                    => 12,
                _ => 0,
            }
        }
    }
    pub const fn is_left(&self) -> bool {
        use Operator::*;
        !matches!(self, Assign | OpAssign(_))
    }
}

fn parse_int(lex: &Lexer<Token>) -> u128 {
    let s = lex.slice().replace('_', "");
    match s.chars().nth(1).unwrap_or(' ') {
        'x' => u128::from_str_radix(&s[2..], 16),
        'b' => u128::from_str_radix(&s[2..], 2),
        _   => s.parse(),
    }.unwrap()
}

fn parse_operator(lex: &Lexer<Token>) -> Operator {
    let s = lex.slice();

    use Operator::*;
    match s {
        "==" => Eq,
        "!=" => NE,
        "<"  => LT,
        "<=" => LE,
        ">"  => GT,
        ">=" => GE,
        "!"  => Not,
        "&&" => LAnd,
        "||" => LOr,

        _ => if s.ends_with('=') && s.len() > 1 {
            Operator::OpAssign(Box::new(_parse_oper(&s[0..s.len()-1])))
        } else {
            _parse_oper(s)
        }
    }
}

fn _parse_oper(s: &str) -> Operator {
    use Operator::*;
    match s {
        "+" => Add, "-" => Sub,
        "*" => Mlt, "/" => Div,
        "%" => Mod, "=" => Assign,
        "|" => BOr, "&" => BAnd,
        "^" => BXOr,
        "<<" => LSh, ">>" => RSh,
        _ => unreachable!()
    }
}

pub type AToken = (Token, Span);
