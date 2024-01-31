pub mod exprs;
pub mod nodes;
pub mod types;

pub use nodes::parse;

use super::*;
pub type Errors = Vec<AError<ParseError>>;

type ShouldEndFn<'a> = &'a ShouldEndFnInner;
type ShouldEndFnInner = dyn Fn(&mut Buffer<AToken>, &mut Errors) -> bool;

