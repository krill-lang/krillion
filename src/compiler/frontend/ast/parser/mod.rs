pub mod exprs;
pub mod nodes;
pub mod types;

use super::*;
pub type Errors = Vec<AError<ParseError>>;

struct Parser<'a> {
    pub buf: &'a mut Buffer<AToken>,
    pub src: &'a str,
    pub errs: &'a mut Errors,
}

impl<'a> Parser<'a> {
    fn last_token(&self) -> Option<&AToken> {
        for i in self.buf.buf.iter().rev() {
            if i.1.start != i.1.end {
                return Some(i);
            }
        }

        None
    }
}

#[macro_export]
macro_rules! consider_error {
    ($expr: expr, $self: expr) => {
        match $expr {
            Err((e, s)) => {
                $self.buf.rewind();
                error!(e, s, $self);
            },
            Ok(a) => a,
        }
    };
}

#[macro_export]
macro_rules! unwrap_or_return_set_buf {
    ($expr: expr, $buf: expr) => {
        match $expr {
            Some(a) => a,
            _ => {
                while let Some((t, _)) = $buf.next() {
                    if matches!(t, Token::Semicolon | Token::CuBracketS) {
                        break;
                    }
                }

                $buf.rewind();
                return;
            },
        }
    };
}

#[macro_export]
macro_rules! error {
    ($reason: expr, $span: expr, $self: expr) => {{
        $self.errs.push(($reason, $span));
        while let Some((t, _)) = $self.buf.next() {
            if matches!(t, Token::Semicolon | Token::CuBracketS) {
                break;
            }
        }

        $self.buf.rewind();
        return;
    }};
}

#[macro_export]
macro_rules! unwrap_ident {
    ($self: expr) => {
        match $self.buf.next() {
            Some((Token::Ident, s)) => ($self.src[s.start..s.end].to_string(), s.clone()),
            Some((t, s)) => error!(
                ParseError::UnexpectedToken {
                    expected: Some("identifier"),
                    found: t.clone(),
                },
                s.clone(),
                $self
            ),
            None => error!(
                ParseError::RanOutTokens,
                $self.buf.prev().unwrap().1.clone(),
                $self
            ),
        }
    };
}

#[macro_export]
macro_rules! assert_token {
    ($intended: pat, $expected: expr, $self: expr) => {
        match $self.buf.next() {
            Some(($intended, s)) => s.clone(),
            Some((t, s)) => error!(
                ParseError::UnexpectedToken {
                    expected: Some($expected),
                    found: t.clone(),
                },
                s.clone(),
                $self
            ),
            None => error!(
                ParseError::RanOutTokens,
                $self.buf.prev().unwrap().1.clone(),
                $self
            ),
        }
    };
}

pub use {consider_error, unwrap_or_return_set_buf, error, unwrap_ident, assert_token};
