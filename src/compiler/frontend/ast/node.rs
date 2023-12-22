use super::*;

pub type GenericAst<Node> = Vec<(Node, Span)>;
pub type UntypedAst = GenericAst<UntypedNode>;
pub type TypedAst = GenericAst<TypedNode>;

pub type UntypedNode = Node<AExpr>;
pub type TypedNode = Node<(AExpr, Type)>;

#[derive(Debug, Clone)]
pub enum Node<Expr: std::fmt::Debug + Clone> {
    VarDeclare {
        ident: AString,
        typ: Option<AType>,
        expr: Option<Expr>,
    },
    Return(Option<Expr>),
    Expr(AExpr),
    Scope {
        body: GenericAst<Self>,
        span: Span,
        ended: bool,
    },
    FunctionDeclare {
        ident: AString,
        params: Vec<(AString, AType, Span)>,
        return_type: Option<AType>,
        body: GenericAst<Self>,
        span: Span,
        ended: bool,
    },
    If {
        main: Vec<(Expr, GenericAst<Self>, Span)>,
        els: Option<Box<(GenericAst<Self>, Span)>>,
        ended: bool,
    },
    While {
        cond: Expr,
        body: GenericAst<Self>,
        span: Span,
        ended: bool,
    }
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

#[derive(Debug, Clone)]
pub enum Type {
    Pointer(Box<AType>),
    Slice(Box<AType>),
    Array(Box<AType>, u128),

    BuiltIn(BuiltInType),
    Unknown(String),
}

#[derive(Debug, Clone)]
pub enum TypeOperators {
    Pointer, Slice, Array(u128),
}

#[derive(Debug, Clone)]
pub enum BuiltInType {
    U8, U16, U32, U64, U128, Int,
    I8, I16, I32, I64, I128, Uint,
    F32, F64,
    Str, Char,
}

impl Type {
    pub fn from_str(s: &str) -> Self {
        use Type::*;
        use BuiltInType::*;
        match s {
            "u8"    => BuiltIn(U8),
            "i8"    => BuiltIn(I8),
            "u16"   => BuiltIn(U16), 
            "i16"   => BuiltIn(I16), 
            "u32"   => BuiltIn(U32),
            "i32"   => BuiltIn(I32),
            "u64"   => BuiltIn(U64),
            "i64"   => BuiltIn(I64),
            "u128"  => BuiltIn(U128),
            "i128"  => BuiltIn(I128),
            "uint"  => BuiltIn(Uint),
            "int"   => BuiltIn(Int),
            "f32"   => BuiltIn(F32),
            "f64"   => BuiltIn(F64),
            "str"   => BuiltIn(Str),
            "char"  => BuiltIn(Char),
            _ => Unknown(s.to_string()),
        }
    }
}
