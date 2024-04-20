use super::*;

pub mod node;
#[allow(clippy::cognitive_complexity)]
pub mod parser;
pub use node::*;
pub use parser::nodes::parse;
pub mod numerator;
pub use numerator::*;
pub mod typecheck;
pub use typecheck::*;

pub type Ident<S> = Vec<S>;

pub type AExpr = Annotated<Expr<Annotation, AIdent>>;
pub type AType = Annotated<Type>;
pub type AString = Annotated<String>;
pub type AIdent = Ident<AString>;

pub type NExpr = ANumerated<Expr<ANumeration, NIdent>>;
pub type NType = ANumerated<Type>;
pub type NString = ANumerated<String>;
pub type NIdent = Numerated<Ident<AString>>;

pub type Ast<Kind> = Vec<Node<Kind>>;
pub type UntypedAst = Ast<UntypedNode>;
pub type UntypedNode = NodeKind<AExpr, AString>;

pub type NumeratedAst = Ast<NumeratedNode>;
pub type NumeratedNode = NodeKind<NExpr, NString>;

pub type TypedAst = Ast<TypedNode>;
pub type TypedNode = NodeKind<(AExpr, Type), NString>;
