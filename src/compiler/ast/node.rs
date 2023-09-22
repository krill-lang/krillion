use super::*;

pub type AST = Vec<ANode>;

#[derive(Debug, Clone)]
pub enum Node {
    VarDeclare {
        ident: String,
        expr: Option<AExpr>,
    },
    Expr(AExpr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Integer(i128),
    Ident(String),
    BiOp {
        lhs: Box<AExpr>,
        rhs: Box<AExpr>,
        op: Operator
    },
    UnOp {
        opr: Box<AExpr>,
        op: Operator,
    },
    FnCall {
        id: String,
        op: Vec<AExpr>,
    }
}
