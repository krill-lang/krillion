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
