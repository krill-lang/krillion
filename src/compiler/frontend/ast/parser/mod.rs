pub mod exprs;
pub mod nodes;
pub mod types;

pub use nodes::parse;

use super::*;
pub type Errors = Vec<AError<ParseError>>;

type ShouldEndFn<'a> = &'a ShouldEndFnInner;
type ShouldEndFnInner = dyn Fn(&mut Buffer<AToken>, &mut Errors) -> bool;

struct Parser<'a> {
    pub buf: &'a mut Buffer<AToken>,
    pub src: &'a str,
    // pub ast: &'a mut UntypedAst,
    pub extra: NodeExtra,
    pub errs: &'a mut Errors,
}
