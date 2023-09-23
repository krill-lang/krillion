use super::*;

pub type AST = Vec<ANode>;

#[derive(Debug, Clone)]
pub enum Node {
    VarDeclare {
        ident: String,
        expr: Option<AExpr>,
    },
    Return(Option<AExpr>),
    Expr(AExpr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Integer(i128),
    Ident(String),
    BiOp {
        lhs: Box<AExpr>,
        rhs: Box<AExpr>,
        op: Box<Operator>,
    },
    UnOp {
        opr: Box<AExpr>,
        op: Box<Operator>,
    },
    FnCall {
        id: Box<AExpr>,
        op: Vec<AExpr>,
    },
    Index {
        lhs: Box<AExpr>,
        rhs: Box<AExpr>,
    },
}
