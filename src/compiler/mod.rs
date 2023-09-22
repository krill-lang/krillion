pub mod lexer;
pub use lexer::*;
pub mod buffer;
pub use buffer::*;
pub mod preprocess;
pub use preprocess::*;
pub mod ast;
pub use ast::*;
#[macro_use]
pub mod error;
pub use error::*;

pub type AToken = (Token, Span);
