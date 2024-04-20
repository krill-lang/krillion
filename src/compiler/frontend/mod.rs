pub mod lexer;
pub use lexer::*;
pub mod buffer;
pub use buffer::*;
pub mod preprocess;
pub use preprocess::*;
pub mod ast;
pub use super::error::*;
pub use ast::*;

pub type Annotation = Span;
pub type Annotated<T> = (T, Annotation);
