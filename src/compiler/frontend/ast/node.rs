use super::*;

#[derive(Debug, Clone)]
pub struct Node<Kind> {
    pub kind: Kind,
    pub span: Span,
    pub extra: NodeExtra,
}

#[derive(Debug, Default, Clone)]
pub struct NodeExtra {}

#[derive(Debug, Clone)]
pub enum Visibility {
    Public,
}

#[derive(Debug, Clone)]
pub enum Linkage {
    External,
    Static,
}

#[derive(Debug, Clone)]
pub enum NodeKind<Expr: std::fmt::Debug + Clone, ShortIdent: std::fmt::Debug + Clone> {
    VarDeclare {
        vis: Option<(Visibility, Span)>,
        link: Option<(Linkage, Span)>,
        ident: ShortIdent,
        typ: Option<AType>,
        expr: Option<Expr>,
    },
    Return(Option<Expr>),
    Expr(Expr),
    Scope {
        body: Ast<Self>,
        span: Span,
    },
    FunctionDeclare {
        vis: Option<(Visibility, Span)>,
        link: Option<(Linkage, Span)>,
        ident: ShortIdent,
        params: Vec<(ShortIdent, AType, Span)>,
        return_type: AType,
        body: Box<Node<Self>>,
        span: Span,
    },
    If {
        main: (Expr, Box<Node<Self>>, Span),
        els: Option<(Box<Node<Self>>, Span)>,
    },
    While {
        cond: Expr,
        body: Box<Node<Self>>,
    },
}

#[derive(Debug, Clone)]
pub enum Expr<Extra: std::fmt::Debug + Clone, Identifier: std::fmt::Debug + Clone> {
    Integer(i128),
    Ident(Identifier),
    BiOp {
        lhs: Box<(Self, Extra)>,
        rhs: Box<(Self, Extra)>,
        op: Operator,
    },
    UnOp {
        opr: Box<(Self, Extra)>,
        op: Operator,
    },
    FnCall {
        id: Box<(Self, Extra)>,
        op: Vec<(Self, Extra)>,
    },
}

#[derive(Debug, Clone)]
pub enum Type {
    Pointer(Box<AType>),
    Slice(Box<AType>),
    Array(Box<AType>, Annotated<u128>),

    BuiltIn(BuiltInType),
    Unknown(String),

    Any,
    Integer,
    UnsignedInteger,

    Function(Vec<Type>, Option<Box<Type>>),

    LValue(Box<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltInType {
    Bool,
    U8,
    U16,
    U32,
    U64,
    U128,
    Int,
    I8,
    I16,
    I32,
    I64,
    I128,
    Uint,
    F32,
    F64,
    Str,
    Char,
    Unit,
}

impl std::cmp::PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Pointer(l), Self::Pointer(r)) => l.0 == r.0,
            (Self::Slice(l), Self::Slice(r)) => l.0 == r.0,
            (Self::Array(lt, ls), Self::Array(rt, rs)) => ls.0 == rs.0 && lt.0 == rt.0,

            (Self::BuiltIn(l), Self::BuiltIn(r)) => l == r,
            (Self::Unknown(l), Self::Unknown(r)) => l == r,

            (Self::Any, Self::Any) => true,
            (Self::Integer, Self::Integer) => true,
            (Self::UnsignedInteger, Self::UnsignedInteger) => true,

            (Self::Function(la, lr), Self::Function(ra, rr)) => lr == rr && la == ra,

            (Self::LValue(l), Self::LValue(r)) => l == r,
            _ => false,
        }
    }
}

impl Type {
    pub fn from_str(s: &str) -> Self {
        use BuiltInType::*;
        use Type::*;
        match s {
            "bool" => BuiltIn(Bool),
            "u8" => BuiltIn(U8),
            "i8" => BuiltIn(I8),
            "u16" => BuiltIn(U16),
            "i16" => BuiltIn(I16),
            "u32" => BuiltIn(U32),
            "i32" => BuiltIn(I32),
            "u64" => BuiltIn(U64),
            "i64" => BuiltIn(I64),
            "u128" => BuiltIn(U128),
            "i128" => BuiltIn(I128),
            "uint" => BuiltIn(Uint),
            "int" => BuiltIn(Int),
            "f32" => BuiltIn(F32),
            "f64" => BuiltIn(F64),
            "str" => BuiltIn(Str),
            "char" => BuiltIn(Char),
            "unit" => BuiltIn(Unit),
            _ => Unknown(s.to_string()),
        }
    }

    pub fn is_integer(&self) -> bool {
        use BuiltInType::*;
        use Type::*;
        match self {
            Any
            | BuiltIn(I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | I128 | U128 | Int | Uint)
            | Integer | UnsignedInteger => true,
            _ => false,
        }
    }

    pub fn specificness(&self) -> usize {
        match self {
            Self::Pointer(t) | Self::Slice(t) | Self::Array(t, _) => t.0.specificness().saturating_sub(1),
            Self::Any => 1000,
            Self::Integer => 12,
            Self::UnsignedInteger => 6,
            _ => 1,
        }
    }

    pub fn constrain(&self, other: &Type) -> Result<Type, TypeCheckError> {
        if self == other {
            return Ok(other.clone());
        }

        match (self, other) {
            (Self::Any, _) => return Ok(other.clone()),
            (_, Self::Any) => return Ok(self.clone()),
            (Self::LValue(t), _) => return t.constrain(other),
            (_, Self::LValue(t)) => return self.constrain(t),
            (Self::Pointer(l), Self::Pointer(r)) => return l.0.constrain(&r.0),
            (Self::Slice(l), Self::Slice(r)) => return l.0.constrain(&r.0),
            (Self::Array(l, ls), Self::Array(r, rs)) if ls == rs => return l.0.constrain(&r.0),
            _ => {},
        }

        let l_anyint = matches!(self, Type::Integer);
        let r_anyint = matches!(other, Type::Integer);
        if (l_anyint && other.is_integer()) || (r_anyint && self.is_integer()) {
            return Ok(if self.specificness() < other.specificness() {
                self.clone()
            } else {
                other.clone()
            })
        }

        // TODO: check unsigned

        Err(TypeCheckError::TypeMismatch { expected: self.clone(), found: other.clone() })
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Type::*;
        match self {
            Pointer(t) => write!(f, "&{}", t.0),
            Slice(t) => write!(f, "[{}]", t.0),
            Array(t, s) => write!(f, "[{} * {}]", t.0, s.0),
            BuiltIn(b) => write!(f, "{b}"),
            Unknown(t) => write!(f, "{t}"),
            Any => write!(f, "_"),
            Integer => write!(f, "{{integer}}"),
            UnsignedInteger => write!(f, "{{unsigned integer}}"),
            Function(args, ret) => {
                write!(f, "(fn({})", args.iter().map(|a| a.to_string()).collect::<Vec<String>>().join(","))?;
                if let Some(ret) = ret {
                    write!(f, "-> {ret}")?;
                }

                Ok(())
            },
            LValue(t) => write!(f, "lvalue {t}"),
        }
    }
}

impl std::fmt::Display for BuiltInType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use BuiltInType::*;
        write!(
            f,
            "{}",
            match self {
                Bool => "bool",
                U8 => "u8",
                I8 => "i8",
                U16 => "u16",
                I16 => "i16",
                U32 => "u32",
                I32 => "i32",
                U64 => "u64",
                I64 => "i64",
                U128 => "u128",
                I128 => "i128",
                Uint => "uint",
                Int => "int",
                F32 => "f32",
                F64 => "f64",
                Str => "str",
                Char => "char",
                Unit => "unit",
            }
        )
    }
}
