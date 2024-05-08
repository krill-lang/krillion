use super::*;

pub fn typecheck(ast: &NumeratedAst, count: usize) -> (Vec<AType>, Vec<AError<TypeCheckError>>) {
    let mut typechecker = Typechecker {
        types: vec![CheckingBaseType::Any.expand(0..0); count],
        errs: Vec::new(),
    };

    typechecker.typecheck_ast(ast);
    typechecker.finalize()

    // (typechecker.types, typechecker.errs)
}

#[derive(Debug, Clone)]
struct Typechecker {
    types: Vec<CheckingType>,
    errs: Vec<AError<TypeCheckError>>,
}

impl Typechecker {
    fn finalize(&mut self) -> (Vec<AType>, Vec<AError<TypeCheckError>>) {
        for i in 0..self.types.len() {
            self.finalize_id(i);
        }

        todo!("{:#?}\n{:#?}", self.errs, self.types);
    }

    fn typecheck_ast(&mut self, ast: &NumeratedAst) {
        for n in ast.iter() {
            self.typecheck_node(n);
        }
    }

    fn finalize_id(&mut self, id: usize) {
        // self._finalize_single(id, &mut vec![]);
        self._finalize_single(id, &mut vec![]);
    }

    fn finalize_type(&mut self, t: CheckingType) -> CheckingType {
        self.types.push(t);
        self.finalize_id(self.types.len() - 1);
        self.types.pop().unwrap()
    }

    fn _finalize_single(&mut self, id: usize, hist: &mut Vec<usize>) {
        if hist.contains(&id) {
            return;
        }

        for i in self.types[id].links_to.clone().iter() {
            hist.push(id);
            self._finalize_single(*i, hist);
            hist.pop();

            match self.types[id].base.constrain(&self.types[*i].base) {
                Ok(t) => {
                    self.types[id].base = t.clone();
                    self.types[*i].base = t;

                    let lv = self.is_lvalue(id) || self.is_lvalue(*i);
                    self.types[id].is_lvalue = lv;
                    self.types[*i].is_lvalue = lv;
                },
                Err(e) => self.errs.push((e, 0..0)), // TODO: fix span
            }

            let t = match (core::mem::take(&mut self.types[id].base), core::mem::take(&mut self.types[*i].base)) {
                (CheckingBaseType::Pointer(t1), CheckingBaseType::Pointer(_)) => {
                    CheckingBaseType::Pointer(Box::new(self.finalize_type(*t1)))
                },
                (a, _) => a.clone(),
            };

            self.types[id].base = t.clone();
            self.types[*i].base = t;
        }
    }

    fn constrain_as_type(&mut self, id: usize, t: &CheckingType) {
        self.def_in(id, t.derived_from.clone());
        self.constrain_as_base_type(id, &t.base);
    }

    fn constrain_as_base_type(&mut self, id: usize, t: &CheckingBaseType) {
        self.finalize_id(id);

        match self.types[id].base.constrain(&t) {
            Ok(t) => self.types[id].base = t,
            Err(e) => self.errs.push((e, 0..0)),
        }
    }

    fn link(&mut self, id: usize, c: usize) {
        self.def_in(id, self.types[c].derived_from.clone());
        self.types[id].links_to.push(c);
        self.types[c].links_to.push(id);
    }

    /* fn constrain(&mut self, id: usize, t: AType) {
        let int = self.types[id].0.constrain(&t.0);
        if let Ok(int) = int {
            match (&mut self.types[id].0, int) {
                (TypeKind::LValue(t), TypeKind::LValue(n)) => **t = *n,
                (TypeKind::LValue(t), n) => **t = n,
                (t, n) => *t = n,
            }
        } else {
            self.errs.push((int.unwrap_err(), t.1));
            self.errs.push((TypeCheckError::RequiredBecauseOf, self.types[id].1.clone()));
        }
    } */

    fn def_in(&mut self, id: usize, span: Span) {
        if span != (0..0) && self.types[id].derived_from == (0..0) {
            self.types[id].derived_from = span;
        }
    }

    fn is_lvalue(&mut self, id: usize) -> bool {
        self.types[id].is_lvalue
    }

    fn set_lvalue(&mut self, id: usize) {
        self.types[id].is_lvalue = true;
    }

    fn constrain_lvalue(&mut self, id: usize, span: &Span) {
        if !self.types[id].is_lvalue {
            self.errs.push((TypeCheckError::ExpectedLvalue, span.clone()));
        }
    }

    fn typecheck_node(&mut self, n: &Node<NumeratedNode>) {
        match &n.kind {
            NodeKind::VarDeclare { ident, typ, expr, .. } => {
                if let Some(t) = typ {
                    self.def_in(ident.1.1, t.1.clone());
                    self.constrain_as_type(ident.1.1, &CheckingType::from_atype(&t));
                }

                if let Some(expr) = expr {
                    self.def_in(ident.1.1, expr.1.0.clone());
                    self.typecheck_expr(expr);
                    self.link(expr.1.1, ident.1.1);
                }
            },
            NodeKind::Expr(expr) => self.typecheck_expr(expr),
            _ => todo!("{n:?}"),
        }
    }

    fn typecheck_expr(&mut self, expr: &NExpr) {
        match &expr.0 {
            Expr::Integer(_) => {
                self.def_in(expr.1.1, expr.1.0.clone());
                self.constrain_as_base_type(expr.1.1, &CheckingBaseType::Integer);
            },
            Expr::Ident(id) => {
                self.link(expr.1.1, id.1);
                self.set_lvalue(expr.1.1);
            },
            Expr::UnOp { op: Operator::Deref, opr } => {
                self.typecheck_expr(opr);
                self.constrain_as_base_type(opr.1.1, &CheckingBaseType::Pointer(Box::new(CheckingType { base: CheckingBaseType::Any, is_lvalue: false, derived_from: expr.1.0.clone(), links_to: vec![expr.1.1] })));
                self.finalize_id(opr.1.1);
                if matches!(&self.types[opr.1.1].base, CheckingBaseType::Pointer(i) if matches!(&**i, CheckingType { is_lvalue: true, .. })) {
                    self.set_lvalue(expr.1.1);
                }
            },
            Expr::UnOp { op: Operator::Ref, opr } => {
                self.typecheck_expr(opr);
                self.constrain_as_base_type(expr.1.1, &CheckingBaseType::Pointer(Box::new(CheckingType { base: CheckingBaseType::Any, is_lvalue: false, derived_from: expr.1.0.clone(), links_to: vec![opr.1.1] })));
            },
            Expr::UnOp { opr, .. } => {
                self.typecheck_expr(opr);
                self.link(expr.1.1, opr.1.1);
            },
            Expr::BiOp { lhs, rhs, op: Operator::Assign | Operator::OpAssign(_) } => {
                self.typecheck_expr(&lhs);
                self.typecheck_expr(&rhs);
                self.constrain_lvalue(lhs.1.1, &lhs.1.0);

                self.link(lhs.1.1, rhs.1.1);

                self.constrain_as_base_type(expr.1.1, &CheckingBaseType::BuiltIn(BuiltInType::Unit));
                self.def_in(expr.1.1, expr.1.0.clone());
            },
            // Expr::BiOp { lhs, rhs, op: Operator::Eq | Operator::NE | Operator::GT | Operator::GE | Operator::LT | Operator::LE | Operator::AndAnd | Operator::OrOr } => {
            //     self.typecheck_expr(&lhs, Some(self.types[rhs.1.1].clone()));
            //     self.typecheck_expr(&rhs, Some(self.types[lhs.1.1].clone()));
            //     self.constrain(expr.1.1, (TypeKind::BuiltIn(BuiltInType::Bool), expr.1.0.clone()));
            // },
            // Expr::BiOp { lhs, rhs, op: Operator::Index } => {
            //     self.typecheck_expr(&lhs, Some((TypeKind::Slice(Box::new(self.types[expr.1.1].clone())), expr.1.0.clone())));
            //     self.typecheck_expr(&rhs, Some((TypeKind::UnsignedInteger, expr.1.0.clone())));

            //     self.lvalue(expr.1.1);
            // },
            Expr::BiOp { lhs, rhs, op: _ } => {
                self.typecheck_expr(&lhs);
                self.typecheck_expr(&rhs);
                self.link(lhs.1.1, rhs.1.1);
                self.link(expr.1.1, lhs.1.1);
            },
            _ => todo!("{expr:?}"),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct CheckingType {
    base: CheckingBaseType,
    is_lvalue: bool,
    derived_from: Span,

    links_to: Vec<usize>,
}

#[derive(Clone, Debug, Default)]
pub enum CheckingBaseType {
    Pointer(Box<CheckingType>),
    Slice(Box<CheckingType>),
    Array(Box<CheckingType>, Annotated<u128>),
    Function(Vec<CheckingType>, Box<CheckingType>),

    BuiltIn(BuiltInType),

    #[default]
    Any,
    Integer,
    UnsignedInteger,
}

impl CheckingType {
    fn from_atype(at: &AType) -> Self {
        Self {
            base: CheckingBaseType::from_type(&at.0),
            is_lvalue: false,
            derived_from: at.1.clone(),
            links_to: vec![],
        }
    }
}

impl CheckingBaseType {
    fn from_type(at: &Type) -> Self {
        match &at {
            Type::Pointer(t) => CheckingBaseType::Pointer(Box::new(CheckingType::from_atype(&*t))),
            Type::Slice(t) => CheckingBaseType::Slice(Box::new(CheckingType::from_atype(&*t))),
            Type::Array(t, s) => CheckingBaseType::Array(Box::new(CheckingType::from_atype(&*t)), s.clone()),
            Type::Function(a, r) => CheckingBaseType::Function(a.iter().map(|a| CheckingType::from_atype(a)).collect(), Box::new(CheckingType::from_atype(&*r))),

            Type::BuiltIn(t) => CheckingBaseType::BuiltIn(t.clone()),

            Type::Any => CheckingBaseType::Any,

            Type::Unknown(t) => todo!("unknown type {t} to CheckingType"),
        }
    }

    fn is_int(&self) -> bool {
        use BuiltInType::*;
        use CheckingBaseType::*;
        match self {
            Any
            | BuiltIn(I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | I128 | U128 | Int | Uint)
            | Integer | UnsignedInteger => true,
            _ => false,
        }
    }

    fn is_uint(&self) -> bool {
        use BuiltInType::*;
        use CheckingBaseType::*;
        match self {
            Any
            | BuiltIn(U8 | U16 | U32 | U64 | U128 | Uint)
            | UnsignedInteger => true,
            _ => false,
        }
    }

    fn specificness(&self) -> usize {
        match self {
            Self::Pointer(t) | Self::Slice(t) | Self::Array(t, _) => t.base.specificness().saturating_sub(1),
            Self::Any => 1000,
            Self::Integer => 12,
            Self::UnsignedInteger => 6,
            _ => 1,
        }
    }

    fn expand(self, from: Span) -> CheckingType {
        CheckingType {
            base: self,
            is_lvalue: false,
            derived_from: from,
            links_to: vec![],
        }
    }

    fn constrain(&self, other: &Self) -> Result<Self, TypeCheckError> {
        if self == other {
            return Ok(other.clone());
        }

        match (self, other) {
            (Self::Any, _) => return Ok(other.clone()),
            (_, Self::Any) => return Ok(self.clone()),
            (Self::Pointer(l), Self::Pointer(r)) => return l.base.constrain(&r.base).map(|a| Self::Pointer(Box::new(a.expand(l.derived_from.clone())))),
            (Self::Slice(l), Self::Slice(r)) => return l.base.constrain(&r.base).map(|a| Self::Slice(Box::new(a.expand(l.derived_from.clone())))),
            (Self::Array(l, ls), Self::Array(r, rs)) if ls == rs => return l.base.constrain(&r.base).map(|a| Self::Array(Box::new(a.expand(l.derived_from.clone())), ls.clone())),
            _ => {},
        }

        let l_anyint = matches!(self, CheckingBaseType::Integer);
        let r_anyint = matches!(other, CheckingBaseType::Integer);
        if (l_anyint && other.is_int()) || (r_anyint && self.is_int()) {
            return Ok(if self.specificness() < other.specificness() {
                self.clone()
            } else {
                other.clone()
            })
        }

        let l_anyuint = matches!(self, CheckingBaseType::UnsignedInteger);
        let r_anyuint = matches!(other, CheckingBaseType::UnsignedInteger);
        if (l_anyuint && other.is_uint()) || (r_anyuint && self.is_uint()) {
            return Ok(if self.specificness() < other.specificness() {
                self.clone()
            } else {
                other.clone()
            })
        }

        Err(TypeCheckError::TypeMismatch { expected: self.clone(), found: other.clone() })
    }
}

impl std::cmp::PartialEq for CheckingBaseType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Pointer(l), Self::Pointer(r)) => l.base == r.base,
            (Self::Slice(l), Self::Slice(r)) => l.base == r.base,
            (Self::Array(lt, ls), Self::Array(rt, rs)) => ls.0 == rs.0 && lt.base == rt.base,

            (Self::BuiltIn(l), Self::BuiltIn(r)) => l == r,

            (Self::Function(la, lr), Self::Function(ra, rr)) => lr.base == rr.base && la.iter().zip(ra.iter()).fold(true, |a, (b, c)| a && b.base == c.base),

            (Self::Any, Self::Any) => true,
            (Self::Integer, Self::Integer) => true,
            (Self::UnsignedInteger, Self::UnsignedInteger) => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for CheckingBaseType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
            Self::Pointer(t) => write!(f, "&{}", t.base),
            Self::Slice(t) => write!(f, "[{}]", t.base),
            Self::Array(t, s) => write!(f, "[{} * {}]", t.base, s.0),
            Self::BuiltIn(b) => write!(f, "{b}"),
            Self::Any => write!(f, "_"),
            Self::Function(args, ret) => {
                write!(f, "(fn({}) -> {}", args.iter().map(|a| a.base.to_string()).collect::<Vec<String>>().join(","), ret.base)?;

                Ok(())
            },
            Self::Integer => write!(f, "{{int}}"),
            Self::UnsignedInteger => write!(f, "{{uint}}"),
        }
    }
}
