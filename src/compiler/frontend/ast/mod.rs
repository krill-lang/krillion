use super::*;

#[allow(clippy::cognitive_complexity)]
pub mod parser;
pub use parser::*;
pub mod node;
pub use node::*;
// pub mod typecheck;
// pub use typecheck::*;

pub type AUntypedNode = (UntypedNode, Span);
pub type AExpr = (Expr, Span);
pub type AType = (Type, Span);
pub type AString = (String, Span);
