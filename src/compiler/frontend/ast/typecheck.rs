use super::*;

pub fn typecheck(ast: &NumeratedAst, count: usize) -> (Vec<AType>, Vec<AError<TypeCheckError>>) {
    let mut typechecker = Typechecker {
        types: vec![CheckingBaseType::Any.expand(0..0); count],
        errs: Vec::new(),
    };

    typechecker.typecheck_ast(ast);
    typechecker.finalize(count)
}

#[derive(Debug, Clone)]
struct Typechecker {
    types: Vec<CheckingType>,
    errs: Vec<AError<TypeCheckError>>,
}

impl Typechecker {
    fn finalize(mut self, count: usize) -> (Vec<AType>, Vec<AError<TypeCheckError>>) {
        println!("{:#?}", self.types);

        for i in 0..count {
            self.finalize_id(i);

            println!("{:?}", self.types[i]);
        }

        let mut types = Vec::with_capacity(count);
        let mut marker = vec![false; self.types.len()];
        for i in 0..count {
            types.push(self.output_type(i, &mut marker));
        }

        (types, self.errs)
    }

    fn typecheck_ast(&mut self, ast: &NumeratedAst) {
        for n in ast.iter() {
            self.typecheck_node(n);
        }
    }

    fn finalize_id(&mut self, id: usize) {
        self._finalize_single(id, &mut vec![]);
    }

    fn _finalize_single(&mut self, id: usize, hist: &mut Vec<usize>) {
        if hist.contains(&id) {
            return;
        }

        hist.push(id);

        for i in self.types[id].links_to.clone().iter() {
            self._finalize_single(*i, hist);

            match self.constrain_ids(id, *i) {
                Ok(()) => {},
                Err(e) => {
                    self.types[id].base = self.types[*i].base.clone();
                    self.errs.push((e, self.types[*i].derived_from.clone()))
                },
            }
        }

        hist.pop();
    }

    fn link(&mut self, id: usize, c: usize) {
        self.def_in(id, self.types[c].derived_from.clone());
        self.types[id].links_to.push(c);
        self.types[c].links_to.push(id);
    }

    fn def_in(&mut self, id: usize, span: Span) {
        if span != (0..0) && self.types[id].derived_from == (0..0) {
            self.types[id].derived_from = span;
        }
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
                self.def_in(ident.1.1, ident.1.0.clone());

                if let Some(t) = typ {
                    let t = self.id_from_atype(&t);
                    self.link(ident.1.1, t);
                }

                if let Some(expr) = expr {
                    self.typecheck_expr(expr);
                    self.link(ident.1.1, expr.1.1);
                }
            },
            NodeKind::Expr(expr) => self.typecheck_expr(expr),
            _ => todo!("{n:?}"),
        }
    }

    fn typecheck_expr(&mut self, expr: &NExpr) {
        self.def_in(expr.1.1, expr.1.0.clone());

        match &expr.0 {
            Expr::Integer(_) => {
                let int = self.id_from_type(CheckingBaseType::Integer.expand(expr.1.0.clone()));
                self.link(expr.1.1, int);
            },
            Expr::Ident(id) => {
                self.link(expr.1.1, id.1);
                self.set_lvalue(expr.1.1);
            },
            Expr::UnOp { op: Operator::Deref, opr } => {
                self.typecheck_expr(opr);
                let pi = self.id_from_type(CheckingBaseType::Pointer(expr.1.1).expand(expr.1.0.clone()));
                self.link(opr.1.1, pi);
                self.set_lvalue(expr.1.1);
            },
            Expr::UnOp { op: Operator::Ref, opr } => {
                self.typecheck_expr(opr);
                let pi = self.id_from_type(CheckingBaseType::Pointer(opr.1.1).expand(expr.1.0.clone()));
                self.link(expr.1.1, pi);
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

                let unit = self.id_from_type(CheckingBaseType::BuiltIn(BuiltInType::Unit).expand(expr.1.0.clone()));
                self.link(expr.1.1, unit);
                self.def_in(expr.1.1, expr.1.0.clone());
            },
            Expr::BiOp { lhs, rhs, op: Operator::Eq | Operator::NE | Operator::GT | Operator::GE | Operator::LT | Operator::LE | Operator::AndAnd | Operator::OrOr } => {
                self.typecheck_expr(&lhs);
                self.typecheck_expr(&rhs);
                self.link(lhs.1.1, rhs.1.1);

                let b = self.id_from_type(CheckingBaseType::BuiltIn(BuiltInType::Bool).expand(expr.1.0.clone()));
                self.link(expr.1.1, b);
            },
            Expr::BiOp { lhs, rhs, op: Operator::Index } => {
                self.typecheck_expr(lhs);
                self.typecheck_expr(rhs);

                let a = self.id_from_type(CheckingBaseType::Slice(expr.1.1).expand(expr.1.0.clone()));
                self.link(lhs.1.1, a);

                let u = self.id_from_type(CheckingBaseType::UnsignedInteger.expand(expr.1.0.clone()));
                self.link(rhs.1.1, u);

                self.set_lvalue(expr.1.1);
            },
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
    Pointer(usize),
    Slice(usize),
    Array(usize, Annotated<u128>),
    Function(Vec<usize>, usize),

    BuiltIn(BuiltInType),

    #[default]
    Any,
    Integer,
    UnsignedInteger,
}

impl CheckingBaseType {
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

    fn expand(self, from: Span) -> CheckingType {
        CheckingType {
            base: self,
            is_lvalue: false,
            derived_from: from,
            links_to: vec![],
        }
    }

}

// impl std::fmt::Display for CheckingBaseType {
//     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//         match &self {
//             Self::Pointer(t) => write!(f, "&{}", t.base),
//             Self::Slice(t) => write!(f, "[{}]", t.base),
//             Self::Array(t, s) => write!(f, "[{} * {}]", t.base, s.0),
//             Self::BuiltIn(b) => write!(f, "{b}"),
//             Self::Any => write!(f, "_"),
//             Self::Function(args, ret) => {
//                 write!(f, "(fn({}) -> {}", args.iter().map(|a| a.base.to_string()).collect::<Vec<String>>().join(","), ret.base)?;
// 
//                 Ok(())
//             },
//             Self::Integer => write!(f, "{{int}}"),
//             Self::UnsignedInteger => write!(f, "{{uint}}"),
//         }
//     }
// }

impl Typechecker {
    fn id_from_type(&mut self, t: CheckingType) -> usize {
        self.types.push(t);
        self.types.len() - 1
    }

    fn id_from_atype(&mut self, at: &AType) -> usize {
        let t = match &at.0 {
            Type::Pointer(t) => CheckingBaseType::Pointer(self.id_from_atype(&t)),
            Type::Slice(t) => CheckingBaseType::Slice(self.id_from_atype(&t)),
            Type::Array(t, s) => CheckingBaseType::Array(self.id_from_atype(&t), s.clone()),
            Type::Function(a, r) => CheckingBaseType::Function(a.iter().map(|a| self.id_from_atype(a)).collect(), self.id_from_atype(&r)),

            Type::BuiltIn(t) => CheckingBaseType::BuiltIn(t.clone()),

            Type::Any => CheckingBaseType::Any,

            Type::Unknown(t) => todo!("unknown type {t} to CheckingType"),
        }.expand(at.1.clone());
        self.id_from_type(t)
    }

    fn specificness_of(&self, t: usize) -> usize {
        match &self.types[t].base {
            CheckingBaseType::Pointer(t) | CheckingBaseType::Slice(t) | CheckingBaseType::Array(t, _) => self.specificness_of(*t).saturating_sub(1),
            CheckingBaseType::Any => 1000,
            CheckingBaseType::Integer => 12,
            CheckingBaseType::UnsignedInteger => 6,
            _ => 1,
        }
    }

    fn types_eq(&self, l: usize, r: usize, hist: &mut Vec<(usize, usize)>) -> bool {
        if hist.iter().find(|(l2, r2)| (l == *l2 && r == *r2) || (l == *r2 && r == *l2)).is_some() {
            return false;
        }

        hist.push((l, r));
        struct HistPop<'a>(&'a mut Vec<(usize, usize)>);
        impl<'a> Drop for HistPop<'a> {
            fn drop(&mut self) {
                self.0.pop();
            }
        }

        match (&self.types[l].base, &self.types[r].base) {
            (CheckingBaseType::Pointer(l), CheckingBaseType::Pointer(r)) => self.types_eq(*l, *r, hist),
            (CheckingBaseType::Slice(l), CheckingBaseType::Slice(r)) => self.types_eq(*l, *r, hist),
            (CheckingBaseType::Array(lt, ls), CheckingBaseType::Array(rt, rs)) => ls.0 == rs.0 && self.types_eq(*lt, *rt, hist),

            (CheckingBaseType::BuiltIn(l), CheckingBaseType::BuiltIn(r)) => l == r,

            (CheckingBaseType::Function(la, lr), CheckingBaseType::Function(ra, rr)) => self.types_eq(*lr, *rr, hist) && la.iter().zip(ra.iter()).fold(true, |a, (b, c)| a && self.types_eq(*b, *c, hist)),

            (CheckingBaseType::Any, CheckingBaseType::Any) => true,
            (CheckingBaseType::Integer, CheckingBaseType::Integer) => true,
            (CheckingBaseType::UnsignedInteger, CheckingBaseType::UnsignedInteger) => true,
            _ => false,
        }
    }

    fn constrain_ids(&mut self, l: usize, r: usize) -> Result<(), TypeCheckError> {
        self._constrain_ids(l, r, &mut vec![])
    }

    fn _constrain_ids(&mut self, l: usize, r: usize, hist: &mut Vec<(usize, usize)>) -> Result<(), TypeCheckError> {
        if hist.iter().find(|(l2, r2)| (l == *l2 && r == *r2) || (l == *r2 && r == *l2)).is_some() {
            self.types[l].base = CheckingBaseType::Any;
            self.types[r].base = CheckingBaseType::Any;

            return Err(TypeCheckError::CyclicType);
        }

        let set = |s: &mut Self| if s.specificness_of(l) < s.specificness_of(r) {
            s.types[r].base = s.types[l].base.clone();

            s.types[l].is_lvalue |= s.types[r].is_lvalue;
            s.types[r].is_lvalue |= s.types[l].is_lvalue;
        } else {
            s.types[l].base = s.types[r].base.clone();

            s.types[l].is_lvalue |= s.types[r].is_lvalue;
            s.types[r].is_lvalue |= s.types[l].is_lvalue;
        };

        if self.types_eq(l, r, hist) {
            return Ok(());
        }

        hist.push((l, r));
        struct HistPop<'a>(&'a mut Vec<(usize, usize)>);
        impl<'a> Drop for HistPop<'a> {
            fn drop(&mut self) {
                self.0.pop();
            }
        }

        let hist = HistPop(hist);

        match (&self.types[l].base, &self.types[r].base) {
            (CheckingBaseType::Any, _) | (_, CheckingBaseType::Any) => {
                set(self);
                return Ok(());
            },
            (CheckingBaseType::Pointer(l), CheckingBaseType::Pointer(r)) => return self._constrain_ids(*l, *r, hist.0),
            (CheckingBaseType::Slice(l), CheckingBaseType::Slice(r)) => return self._constrain_ids(*l, *r, hist.0),
            (CheckingBaseType::Array(l, ls), CheckingBaseType::Array(r, rs)) if ls == rs => return self._constrain_ids(*l, *r, hist.0),
            _ => {},
        }

        let l_anyint = matches!(self.types[l].base, CheckingBaseType::Integer);
        let r_anyint = matches!(self.types[r].base, CheckingBaseType::Integer);
        if (l_anyint && self.types[r].base.is_int()) || (r_anyint && self.types[l].base.is_int()) {
            set(self);
            return Ok(());
        }

        let l_anyuint = matches!(self.types[l].base, CheckingBaseType::UnsignedInteger);
        let r_anyuint = matches!(self.types[r].base, CheckingBaseType::UnsignedInteger);
        if (l_anyuint && self.types[r].base.is_uint()) || (r_anyuint && self.types[l].base.is_uint()) {
            set(self);
            return Ok(());
        }

        Err(TypeCheckError::TypeMismatch { expected: self.format_id(l), found: self.format_id(r) })
    }

    fn format_id(&mut self, id: usize) -> String {
        // self.finalize_id(id);
        let t = &self.types[id];

        let mut acc = String::new();

        // if t.is_lvalue {
        //     acc += "lvalue ";
        // }

        // for i in t.links_to.iter() {
        //     acc += &format!("link {i} ");
        // }

        match &t.base {
            CheckingBaseType::Pointer(t) => {
                acc += "&";
                acc += &self.format_id(*t);
            },
            CheckingBaseType::Slice(t) => {
                acc += "[";
                // acc += &t.to_string();
                // acc += " ";
                acc += &self.format_id(*t);
                acc += "]";
            },
            CheckingBaseType::Array(t, s) => {
                let s = s.0;
                acc += "[";
                acc += &self.format_id(*t);
                acc += &format!(" * {}]", s);
            },
            CheckingBaseType::BuiltIn(b) => acc += &b.to_string(),
            CheckingBaseType::Any => acc += "_",
            CheckingBaseType::Function(args, ret) => {
                let ret = *ret;
                acc += "(fn(";
                acc += &args.clone().into_iter().map(|a| self.format_id(a)).collect::<Vec<String>>().join(", ");
                acc += ") -> ";
                acc += &self.format_id(ret);
            },
            CheckingBaseType::Integer => acc += "{int}",
            CheckingBaseType::UnsignedInteger => acc += "{uint}",
        }

        acc
    }

    fn output_type(&mut self, ti: usize, marker: &mut [bool]) -> AType {
        self._output_type(ti, marker, &mut vec![])
    }

    fn _output_type(&mut self, ti: usize, marker: &mut [bool], hist: &mut Vec<usize>) -> AType {
        if hist.contains(&ti) {
            return (Type::Any, 0..0);
        }

        hist.push(ti);
        let t = self.types[ti].clone();
        let r = (match t.base {
            CheckingBaseType::Pointer(t) => Type::Pointer(Box::new(self._output_type(t, marker, hist))),
            CheckingBaseType::Slice(t) => Type::Slice(Box::new(self._output_type(t, marker, hist))),
            CheckingBaseType::Array(t, s) => Type::Array(Box::new(self._output_type(t, marker, hist)), s.clone()),

            CheckingBaseType::Function(a, r) => Type::Function(a.into_iter().map(|a| self._output_type(a, marker, hist)).collect(), Box::new(self._output_type(r, marker, hist))),

            CheckingBaseType::BuiltIn(b) => Type::BuiltIn(b.clone()),

            CheckingBaseType::Integer => Type::BuiltIn(BuiltInType::Int),
            CheckingBaseType::UnsignedInteger => Type::BuiltIn(BuiltInType::Uint),
            CheckingBaseType::Any => {
                if !marker[ti] {
                    self.errs.push((TypeCheckError::UnresolvedType, t.derived_from.clone()));
                    marker[ti] = true;
                }
                Type::Any
            },
        }, t.derived_from);

        hist.pop();
        r
    }
}
