use super::*;

pub type Ast<Kind> = Vec<Node<Kind>>;
pub type UntypedAst = Ast<UntypedNode>;
pub type TypedAst = Ast<TypedNode>;

pub type UntypedNode = NodeKind<AExpr>;
pub type TypedNode = NodeKind<(AExpr, Type)>;

#[derive(Clone)]
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

#[derive(Clone)]
pub enum NodeKind<Expr: std::fmt::Debug + Clone> {
    VarDeclare {
        vis: Option<(Visibility, Span)>,
        link: Option<(Linkage, Span)>,
        ident: AString,
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
        ident: AString,
        params: Vec<(AString, AType, Span)>,
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
        body: Box<Node<Self>>, // gonna be a scope
        // span: Span,
    },
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
}

pub type Identifier = Vec<String>;

#[derive(Clone, PartialEq, Eq)]
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

/* #[derive(Clone)]
pub enum TypeOperators {
    Pointer,
    Slice,
    Array(u128),
} */

#[derive(Clone, PartialEq, Eq)]
pub enum BuiltInType {
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

impl Type {
    pub fn from_str(s: &str) -> Self {
        use BuiltInType::*;
        use Type::*;
        match s {
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
            | Integer => true,
            OneOf(t) => t.iter().any(|e| e.is_integer()),
            _ => false,
        }
    }

    pub fn specificness(&self) -> usize {
        use Type::*;
        match self {
            OneOf(t) => t.iter().fold(0, |a, e| a + e.specificness()),
            Pointer(t) | Slice(t) | Array(t, _) => t.0.specificness().saturating_sub(1),
            Any => 1000,
            Integer => 12,
            _ => 1,
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
                f,
                "({})",
                t.iter()
                    .map(|t| format!("{t}"))
                    .collect::<Vec<_>>()
                    .join(" | ")
            ),
            Any => write!(f, "_"),
            Integer => write!(f, "{{integer}}"),
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

pub struct AstFormatter<'a, N: DebugDepth>(pub &'a Ast<N>);

impl<'a, N: DebugDepth> DebugDepth for AstFormatter<'a, N> {
    fn format(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
        for (i, n) in self.0.iter().enumerate() {
            writeln!(f, "{:│>1$}┬ {i}:", "├", depth)?;
            n.format(f, depth + 1)?;
        }

        Ok(())
    }
}

pub trait DebugDepth
where
    Self: Sized,
{
    fn format(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result;

    fn formatter(&self, depth: usize) -> Formatter<'_, Self> { Formatter { depth, inner: self } }
}

pub struct Formatter<'a, I: DebugDepth> {
    depth: usize,
    inner: &'a I,
}

impl<'a, I: DebugDepth> std::fmt::Debug for Formatter<'a, I> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.inner.format(f, self.depth)
    }
}

impl<K: DebugDepth> DebugDepth for Node<K> {
    fn format(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
        writeln!(f, "{:│>1$}┬ kind:", "├", depth)?;
        self.kind.format(f, depth + 1)?;
        writeln!(f, "{:│>1$}─ span: {2:?}", "├", depth, self.span)?;
        writeln!(f, "{:│>1$}─ extra: {2:?}", "├", depth, self.extra)?;

        Ok(())
    }
}

impl<E: std::fmt::Debug + Clone> DebugDepth for NodeKind<E> {
    fn format(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
        match self {
            Self::VarDeclare {
                vis,
                link,
                ident,
                typ,
                expr,
            } => {
                writeln!(f, "{:│>1$}┬ let:", "├", depth)?;
                if let Some((vis, span)) = vis {
                    writeln!(f, "{:│>1$}┬ vis: {vis:?}", "├", depth + 1)?;
                    writeln!(f, "{:│>1$}─ span: {span:?}", "├", depth + 2)?;
                }
                if let Some((link, span)) = link {
                    writeln!(f, "{:│>1$}┬ link: {link:?}", "├", depth + 1)?;
                    writeln!(f, "{:│>1$}─ span: {span:?}", "├", depth + 2)?;
                }
                writeln!(f, "{:│>1$}─ ident: {ident:?}", "├", depth + 1)?;
                if let Some((typ, span)) = typ {
                    writeln!(f, "{:│>1$}┬ type: {typ}", "├", depth + 1)?;
                    writeln!(f, "{:│>1$}─ span: {span:?}", "├", depth + 2)?;
                }
                if let Some(expr) = expr {
                    writeln!(f, "{:│>1$}─ expr: {expr:?}", "├", depth + 1)?;
                }
            },
            Self::Return(Some(rt)) => writeln!(f, "{:│>1$}─ return: {rt:?}", "├", depth)?,
            Self::Return(None) => writeln!(f, "{:│>1$}─ return", "├", depth)?,
            Self::Expr(expr) => writeln!(f, "{:│>1$}─ expr: {expr:?}", "├", depth)?,
            Self::Scope { body, span } => {
                writeln!(f, "{:│>1$}┬ scope:", "├", depth)?;
                writeln!(f, "{:│>1$}─ span: {span:?}", "├", depth + 1)?;
                writeln!(f, "{:│>1$}┬ body:", "├", depth + 1)?;
                AstFormatter(body).format(f, depth + 2)?;
            },
            Self::FunctionDeclare {
                vis,
                link,
                ident,
                params,
                return_type,
                body,
                span,
            } => {
                writeln!(f, "{:│>1$}┬ fn:", "├", depth)?;
                if let Some((vis, span)) = vis {
                    writeln!(f, "{:│>1$}┬ vis: {vis:?}", "├", depth + 1)?;
                    writeln!(f, "{:│>1$}─ span: {span:?}", "├", depth + 2)?;
                }
                if let Some((link, span)) = link {
                    writeln!(f, "{:│>1$}┬ link: {link:?}", "├", depth + 1)?;
                    writeln!(f, "{:│>1$}─ span: {span:?}", "├", depth + 2)?;
                }
                writeln!(f, "{:│>1$}─ ident: {ident:?}", "├", depth + 1)?;
                writeln!(f, "{:│>1$}┬ params:", "├", depth + 1)?;
                for (i, p) in params.iter().enumerate() {
                    writeln!(f, "{:│>1$}┬ {i}:", "├", depth + 2)?;
                    writeln!(f, "{:│>1$}─ span: {2:?}", "├", depth + 3, p.2)?;
                    writeln!(f, "{:│>1$}┬ ident: {2}", "├", depth + 3, p.0 .0)?;
                    writeln!(f, "{:│>1$}─ span: {2:?}", "├", depth + 4, p.0 .1)?;
                    writeln!(f, "{:│>1$}┬ type: {2}", "├", depth + 3, p.1 .0)?;
                    writeln!(f, "{:│>1$}─ span: {2:?}", "├", depth + 4, p.1 .1)?;
                }
                writeln!(
                    f,
                    "{:│>1$}┬ return_type: {2}",
                    "├",
                    depth + 1,
                    return_type.0
                )?;
                writeln!(f, "{:│>1$}─ span: {2:?}", "├", depth + 2, return_type.1)?;
                writeln!(f, "{:│>1$}─ span: {span:?}", "├", depth + 1)?;
                writeln!(f, "{:│>1$}┬ body:", "├", depth + 1)?;
                AstFormatter(body).format(f, depth + 2)?;
            },
            Self::If { main, els } => {
                writeln!(f, "{:│>1$}┬ if:", "├", depth)?;
                writeln!(f, "{:│>1$}┬ main:", "├", depth + 1)?;
                writeln!(f, "{:│>1$}─ expr: {2:?}", "├", depth + 2, main.0)?;
                writeln!(f, "{:│>1$}─ span: {2:?}", "├", depth + 2, main.2)?;
                writeln!(f, "{:│>1$}┬ body:", "├", depth + 2)?;
                AstFormatter(&main.1).format(f, depth + 3)?;
                if let Some(els) = els {
                    writeln!(f, "{:│>1$}┬ else:", "├", depth + 1)?;
                    writeln!(f, "{:│>1$}─ span: {2:?}", "├", depth + 2, els.1)?;
                    writeln!(f, "{:│>1$}┬ body:", "├", depth + 2)?;
                    els.0.format(f, depth + 3)?;
                }
            },
            Self::While { cond, body } => {
                writeln!(f, "{:│>1$}┬ while:", "├", depth)?;
                writeln!(f, "{:│>1$}─ cond: {cond:?}", "├", depth + 1)?;
                writeln!(f, "{:│>1$}┬ body:", "├", depth + 1)?;
                body.format(f, depth + 2)?;
            },
        }

        Ok(())
    }
}
