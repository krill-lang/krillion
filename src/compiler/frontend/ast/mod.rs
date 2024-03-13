use super::*;

pub mod node;
#[allow(clippy::cognitive_complexity)]
pub mod parser;
pub use node::*;
pub use parser::nodes::parse;
// pub mod typecheck;
// pub use typecheck::*;

pub type Annotated<T> = (T, Span);
pub type AExpr = Annotated<Expr>;
pub type AType = Annotated<Type>;
pub type AString = Annotated<String>;
