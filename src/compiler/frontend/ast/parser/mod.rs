pub mod exprs;
pub mod nodes;
pub mod types;

pub use nodes::parse;

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
