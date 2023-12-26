use super::*;

pub type Ast<Node> = Vec<(Node, Span)>;
pub type UntypedAst = Ast<UntypedNode>;
pub type TypedAst = Ast<TypedNode>;

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
        body: Ast<Self>,
        span: Span,
        ended: bool,
    },
    FunctionDeclare {
        ident: AString,
        params: Vec<(AString, AType, Span)>,
        return_type: Option<AType>,
        body: Ast<Self>,
        span: Span,
        ended: bool,
    },
    If {
        main: Vec<(Expr, Ast<Self>, Span)>,
        els: Option<Box<(Ast<Self>, Span)>>,
        ended: bool,
    },
    While {
        cond: Expr,
        body: Ast<Self>,
        span: Span,
        ended: bool,
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Integer(i128),
    Ident(Identifier),
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
        id: Identifier,
        op: Vec<AExpr>,
    },
/*    Index {
        lhs: Box<AExpr>,
        rhs: Box<AExpr>,
    },*/
}

pub type Identifier = Vec<String>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Pointer(Box<AType>),
    Slice(Box<AType>),
    Array(Box<AType>, u128),

    BuiltIn(BuiltInType),
    Unknown(String),

    OneOf(Vec<Type>),
    Any,
    Integer,
}

#[derive(Debug, Clone)]
pub enum TypeOperators {
    Pointer, Slice, Array(u128),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltInType {
    U8, U16, U32, U64, U128, Int,
    I8, I16, I32, I64, I128, Uint,
    F32, F64,
    Str, Char,
    Unit,
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
            "unit"  => BuiltIn(Unit),
            _ => Unknown(s.to_string()),
        }
    }

    pub fn is_integer(&self) -> bool {
        use Type::*;
        use BuiltInType::*;
        match self {
            Any | BuiltIn(I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | I128 | U128) | Integer => true,
            OneOf(t) => t.iter().any(|e| e.is_integer()),
            _ => false,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Type::*;
        match self {
            Pointer(t) => write!(f, "&{}", t.0),
            Slice(t) => write!(f, "[]{}", t.0),
            Array(t, s) => write!(f, "[{s}]{}", t.0),
            BuiltIn(b) => write!(f, "{b}"),
            Unknown(t) => write!(f, "{t}"),
            OneOf(t) => write!(
                f, "({})",
                t   .iter()
                    .map(|t| format!("{t}"))
                    .collect::<Vec<_>>()
                    .join(" | ")
            ),
            Any => write!(f, "_"),
            Integer => write!(f, "{{int}}"),
        }
    }
}

impl std::fmt::Display for BuiltInType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use BuiltInType::*;
        write!(f, "{}", match self {
            U8   => "u8",
            I8   => "i8",
            U16  => "u16",
            I16  => "i16",
            U32  => "u32",
            I32  => "i32",
            U64  => "u64",
            I64  => "i64",
            U128 => "u128",
            I128 => "i128",
            Uint => "uint",
            Int  => "int",
            F32 => "f32",
            F64 => "f64",
            Str => "str",
            Char => "char",
            Unit => "unit",
        })
    }
}
