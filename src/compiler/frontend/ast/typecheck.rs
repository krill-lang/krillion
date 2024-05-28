use super::*;

pub fn typecheck(ast: &NumeratedAst, count: usize) -> (Vec<AType>, Vec<AError<TypeCheckError>>) {
    let mut typechecker = Typechecker {
        types: vec![CheckingBaseType::Any.expand(0..0); count],
        errs: Vec::new(),
    };

    typechecker.typecheck_ast(ast, None);
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

        for i in 0..self.types.len() {
            self.finalize_id(i);
        }

        println!("{:#?}", self.types);

        let mut types = Vec::with_capacity(count);
        for i in 0..count {
            types.push(self.output_type(i));
        }

        (types, self.errs)
    }

    fn typecheck_ast(&mut self, ast: &NumeratedAst, ret: Option<usize>) {
        for n in ast.iter() {
            self.typecheck_node(n, ret);
        }
    }

    fn finalize_id(&mut self, id: usize) { self._finalize_single(id, &mut vec![]); }

    fn _finalize_single(&mut self, id: usize, hist: &mut Vec<usize>) {
        if hist.contains(&id) {
            return;
        }

        hist.push(id);

        for i in self.types[id].links_to.clone().into_iter().chain(self.types[id].links_to.clone()) {
            self._finalize_single(i, hist);

            match self.constrain_ids(id, i) {
                Ok(()) => {},
                Err(()) => {
                    self.recursive_error(id);
                    self.recursive_error(i);
                },
            }
        }

        hist.pop();
    }

    fn link(&mut self, id: usize, c: usize) {
        self.types[id].is_forced |= self.types[c].is_forced;
        self.def_in(id, self.types[c].derived_from.clone());
        self.link_chain1(id, c);
        self.link_chain2(c, id);
    }

    fn link_chain1(&mut self, id: usize, tar: usize) {
        if id == tar || self.types[id].links_to.contains(&tar) {
            return;
        }

        self.types[id].links_to.push(tar);

        for c in self.types[id].links_to.clone().into_iter() {
            self.link_chain1(c, tar);
        }
    }

    fn link_chain2(&mut self, id: usize, tar: usize) {
        if id == tar || self.types[id].linked_from.contains(&tar) {
            return;
        }

        self.types[id].linked_from.push(tar);

        for c in self.types[id].linked_from.clone().into_iter() {
            self.link_chain2(c, tar);
        }
    }

    fn def_in(&mut self, id: usize, span: Span) {
        if span != (0..0) && self.types[id].derived_from == (0..0) {
            self.types[id].derived_from = span;
        }
    }

    fn set_lvalue(&mut self, id: usize) { self.types[id].is_lvalue = true; }

    fn enforce(&mut self, id: usize) { self.types[id].is_forced = true; }

    fn constrain_lvalue(&mut self, id: usize, span: &Span) {
        if !self.types[id].is_lvalue {
            self.errs
                .push((TypeCheckError::ExpectedLvalue, span.clone()));
        }
    }

    fn recursive_error(&mut self, id: usize) {
        match &self.types[id].base {
            CheckingBaseType::Slice(s)
            | CheckingBaseType::Array(s, _)
            | CheckingBaseType::Pointer(s)
                => self.recursive_error(*s),
            CheckingBaseType::Function(a, r) => {
                let r = *r;

                for a in a.clone().iter() {
                    self.recursive_error(*a);
                }

                self.recursive_error(r);
            },
            CheckingBaseType::BuiltIn(_) | CheckingBaseType::Any | CheckingBaseType::Error | CheckingBaseType::Integer | CheckingBaseType::UnsignedInteger => {},
        }

        if !self.types[id].is_forced {
            self.types[id].base = CheckingBaseType::Error;
        }
    }

    fn typecheck_node(&mut self, n: &Node<NumeratedNode>, ret: Option<usize>) {
        match &n.kind {
            NodeKind::VarDeclare {
                ident, typ, expr, ..
            } => {
                self.def_in(ident.1.1, ident.1.0.clone());

                if let Some(t) = typ {
                    let t = self.id_from_atype(t);
                    self.enforce(t);
                    self.link(ident.1.1, t);
                }

                if let Some(expr) = expr {
                    self.typecheck_expr(expr);
                    self.link(ident.1.1, expr.1.1);
                }
            },
            NodeKind::Expr(expr) => self.typecheck_expr(expr),
            NodeKind::Scope { body, .. } => self.typecheck_ast(body, ret),
            NodeKind::If { main, els } => {
                self.typecheck_expr(&main.0);
                let b = self.id_from_type(
                    CheckingBaseType::BuiltIn(BuiltInType::Bool).expand(n.span.clone()),
                );
                self.enforce(b);
                self.link(b, main.0.1.1);

                self.typecheck_node(&main.1, ret);
                if let Some(els) = els {
                    self.typecheck_node(&els.0, ret);
                }
            },
            NodeKind::While { cond, body } => {
                self.typecheck_expr(cond);
                let b = self.id_from_type(
                    CheckingBaseType::BuiltIn(BuiltInType::Bool).expand(n.span.clone()),
                );
                self.enforce(b);
                self.link(b, cond.1.1);
                self.typecheck_node(body, ret);
            },
            NodeKind::Return(expr) => {
                if let Some(ret) = ret {
                    let id = if let Some(expr) = expr {
                        self.typecheck_expr(expr);
                        expr.1.1
                    } else {
                        self.id_from_type(
                            CheckingBaseType::BuiltIn(BuiltInType::Unit).expand(n.span.clone()),
                        )
                    };

                    self.link(ret, id);
                    self.link(id, ret);
                } else {
                    self.errs
                        .push((TypeCheckError::UnexpectedReturn, n.span.clone()));
                }
            },
            NodeKind::FunctionDeclare {
                ident,
                params,
                return_type,
                body,
                span,
                ..
            } => {
                let mut p = Vec::with_capacity(params.len());
                for a in params.iter() {
                    let t = self.id_from_atype(&a.1);
                    self.enforce(t);
                    self.link(a.0.1.1, t);
                    p.push(t);
                }

                let r = self.id_from_atype(return_type);
                self.enforce(r);
                let f = self.id_from_type(CheckingBaseType::Function(p, r).expand(span.clone()));
                self.enforce(f);
                self.enforce(ident.1.1);
                self.link(ident.1.1, f);

                self.typecheck_node(body, Some(r));
            },
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
            Expr::UnOp {
                op: Operator::Deref,
                opr,
            } => {
                self.typecheck_expr(opr);
                let pi =
                    self.id_from_type(CheckingBaseType::Pointer(expr.1.1).expand(expr.1.0.clone()));
                self.link(pi, opr.1.1);
                self.set_lvalue(expr.1.1);
            },
            Expr::UnOp {
                op: Operator::Ref,
                opr,
            } => {
                self.typecheck_expr(opr);
                let pi =
                    self.id_from_type(CheckingBaseType::Pointer(opr.1.1).expand(expr.1.0.clone()));
                self.link(expr.1.1, pi);
            },
            Expr::UnOp { opr, .. } => {
                self.typecheck_expr(opr);
                self.link(expr.1.1, opr.1.1);
                self.link(opr.1.1, expr.1.1);
            },
            Expr::BiOp {
                lhs,
                rhs,
                op: Operator::Assign | Operator::OpAssign(_),
            } => {
                self.typecheck_expr(lhs);
                self.typecheck_expr(rhs);
                self.constrain_lvalue(lhs.1.1, &lhs.1.0);

                self.link(lhs.1.1, rhs.1.1);

                let unit = self.id_from_type(
                    CheckingBaseType::BuiltIn(BuiltInType::Unit).expand(expr.1.0.clone()),
                );
                self.link(expr.1.1, unit);
                self.def_in(expr.1.1, expr.1.0.clone());
            },
            Expr::BiOp {
                lhs,
                rhs,
                op:
                    Operator::Eq
                    | Operator::NE
                    | Operator::GT
                    | Operator::GE
                    | Operator::LT
                    | Operator::LE
                    | Operator::AndAnd
                    | Operator::OrOr,
            } => {
                self.typecheck_expr(lhs);
                self.typecheck_expr(rhs);
                self.link(lhs.1.1, rhs.1.1);

                let b = self.id_from_type(
                    CheckingBaseType::BuiltIn(BuiltInType::Bool).expand(expr.1.0.clone()),
                );
                self.link(expr.1.1, b);
            },
            Expr::BiOp {
                lhs,
                rhs,
                op: Operator::Index,
            } => {
                self.typecheck_expr(lhs);
                self.typecheck_expr(rhs);

                let a =
                    self.id_from_type(CheckingBaseType::Slice(expr.1.1).expand(expr.1.0.clone()));
                self.link(a, lhs.1.1);

                let u =
                    self.id_from_type(CheckingBaseType::UnsignedInteger.expand(expr.1.0.clone()));
                self.link(u, rhs.1.1);

                self.set_lvalue(expr.1.1);
            },
            Expr::BiOp { lhs, rhs, op: _ } => {
                self.typecheck_expr(lhs);
                self.typecheck_expr(rhs);
                self.link(lhs.1.1, rhs.1.1);
                self.link(rhs.1.1, lhs.1.1);
                self.link(expr.1.1, lhs.1.1);
                self.link(lhs.1.1, expr.1.1);
            },
            Expr::FnCall { id, op } => {
                let mut a = Vec::with_capacity(op.len());
                for i in op.iter() {
                    self.typecheck_expr(i);
                    a.push(i.1.1);
                }

                self.typecheck_expr(id);

                let t = self
                    .id_from_type(CheckingBaseType::Function(a, expr.1.1).expand(expr.1.0.clone()));
                self.link(t, id.1.1);
            },
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct CheckingType {
    base: CheckingBaseType,
    is_lvalue: bool,
    is_forced: bool,
    derived_from: Span,

    links_to: Vec<usize>,
    linked_from: Vec<usize>,
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

    Error,
}

impl CheckingBaseType {
    const fn is_int(&self) -> bool {
        use BuiltInType::*;
        use CheckingBaseType::*;
        matches!(
            self,
            Any | Error
                | BuiltIn(I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | I128 | U128 | Int | Uint)
                | Integer
                | UnsignedInteger
        )
    }

    const fn is_uint(&self) -> bool {
        use BuiltInType::*;
        use CheckingBaseType::*;
        matches!(
            self,
            Any | Error | BuiltIn(U8 | U16 | U32 | U64 | U128 | Uint) | UnsignedInteger
        )
    }

    fn expand(self, from: Span) -> CheckingType {
        CheckingType {
            base: self,
            is_lvalue: false,
            is_forced: false,
            derived_from: from,
            links_to: vec![],
            linked_from: vec![],
        }
    }
}

impl Typechecker {
    fn id_from_type(&mut self, t: CheckingType) -> usize {
        self.types.push(t);
        self.types.len() - 1
    }

    fn id_from_atype(&mut self, at: &AType) -> usize {
        let t = match &at.0 {
            Type::Pointer(t) => CheckingBaseType::Pointer(self.id_from_atype(t)),
            Type::Slice(t) => CheckingBaseType::Slice(self.id_from_atype(t)),
            Type::Array(t, s) => CheckingBaseType::Array(self.id_from_atype(t), s.clone()),
            Type::Function(a, r) => CheckingBaseType::Function(
                a.iter().map(|a| self.id_from_atype(a)).collect(),
                self.id_from_atype(r),
            ),

            Type::BuiltIn(t) => CheckingBaseType::BuiltIn(t.clone()),

            Type::Any => CheckingBaseType::Any,

            Type::Unknown(t) => todo!("unknown type {t} to CheckingType"),
        }
        .expand(at.1.clone());
        self.id_from_type(t)
    }

    fn specificness_of(&self, t: usize) -> usize {
        match &self.types[t].base {
            CheckingBaseType::Pointer(t)
            | CheckingBaseType::Slice(t)
            | CheckingBaseType::Array(t, _) => self.specificness_of(*t).saturating_sub(1),
            CheckingBaseType::Error => 9000,
            CheckingBaseType::Any => 1000,
            CheckingBaseType::Integer => 12,
            CheckingBaseType::UnsignedInteger => 6,
            _ => 1,
        }
    }

    fn types_eq(&self, l: usize, r: usize, hist: &mut Vec<(usize, usize)>) -> bool {
        if hist
            .iter()
            .any(|(l2, r2)| (l == *l2 && r == *r2) || (l == *r2 && r == *l2))
        {
            return false;
        }

        hist.push((l, r));
        struct HistPop<'a>(&'a mut Vec<(usize, usize)>);
        impl<'a> Drop for HistPop<'a> {
            fn drop(&mut self) { self.0.pop(); }
        }

        let hist = HistPop(hist);

        match (&self.types[l].base, &self.types[r].base) {
            (CheckingBaseType::Pointer(l), CheckingBaseType::Pointer(r)) => {
                self.types_eq(*l, *r, hist.0)
            },
            (CheckingBaseType::Slice(l), CheckingBaseType::Slice(r)) => {
                self.types_eq(*l, *r, hist.0)
            },
            (CheckingBaseType::Array(lt, ls), CheckingBaseType::Array(rt, rs)) => {
                ls.0 == rs.0 && self.types_eq(*lt, *rt, hist.0)
            },

            (CheckingBaseType::BuiltIn(l), CheckingBaseType::BuiltIn(r)) => l == r,

            (CheckingBaseType::Function(la, lr), CheckingBaseType::Function(ra, rr)) if la.len() == ra.len() => {
                self.types_eq(*lr, *rr, hist.0)
                    && la
                        .iter()
                        .zip(ra.iter())
                        .fold(true, |a, (b, c)| a && self.types_eq(*b, *c, hist.0))
            },

            (CheckingBaseType::Any, CheckingBaseType::Any) => true,
            (CheckingBaseType::Error, CheckingBaseType::Error) => true,
            (CheckingBaseType::Integer, CheckingBaseType::Integer) => true,
            (CheckingBaseType::UnsignedInteger, CheckingBaseType::UnsignedInteger) => true,
            _ => false,
        }
    }

    fn constrain_ids(&mut self, l: usize, r: usize) -> Result<(), ()> {
        self._constrain_ids(l, r, &mut vec![], 0)
    }

    fn _constrain_ids(
        &mut self,
        l: usize,
        r: usize,
        hist: &mut Vec<(usize, usize)>,
        base: usize,
    ) -> Result<(), ()> {
        // let pre_err = |s: &mut Self| {
        //     if !s.types[l].is_forced {
        //         s.types[l].base = CheckingBaseType::Error;
        //     }
        //     if !s.types[r].is_forced {
        //         s.types[r].base = CheckingBaseType::Error;
        //     }
        // };

        if hist
            .iter()
            .any(|(l2, r2)| (l == *l2 && r == *r2) || (l == *r2 && r == *l2))
        {
            self.errs.push((TypeCheckError::CyclicType, self.types[r].derived_from.clone()));
            // pre_err(self);
            return Err(());
        }

        let set = |s: &mut Self| {
            if s.specificness_of(l) < s.specificness_of(r) {
                s.types[r].base = s.types[l].base.clone();
            } else {
                s.types[l].base = s.types[r].base.clone();
            }

            s.types[l].is_lvalue |= s.types[r].is_lvalue;
            s.types[r].is_lvalue |= s.types[l].is_lvalue;
        };

        if self.types_eq(l, r, hist) {
            return Ok(());
        }

        hist.push((l, r));
        struct HistPop<'a>(&'a mut Vec<(usize, usize)>);
        impl<'a> Drop for HistPop<'a> {
            fn drop(&mut self) { self.0.pop(); }
        }

        let hist = HistPop(hist);

        match (&self.types[l].base, &self.types[r].base) {
            (CheckingBaseType::Error, _) | (_, CheckingBaseType::Error) => {
                self.recursive_error(l);
                self.recursive_error(r);

                return Ok(());
            },
            (CheckingBaseType::Any, _) | (_, CheckingBaseType::Any) => {
                self.link(l, r);
                set(self);
                return Ok(());
            },
            (CheckingBaseType::Pointer(l), CheckingBaseType::Pointer(r)) => {
                return self._constrain_ids(*l, *r, hist.0, base);
            },
            (CheckingBaseType::Slice(l), CheckingBaseType::Slice(r)) => {
                return self._constrain_ids(*l, *r, hist.0, base);
            },
            (CheckingBaseType::Array(l, ls), CheckingBaseType::Array(r, rs)) if ls == rs => {
                return self._constrain_ids(*l, *r, hist.0, base);
            },
            (CheckingBaseType::Function(lp, la), CheckingBaseType::Function(rp, ra))
                if lp.len() == rp.len() =>
            {
                let la = *la;
                let ra = *ra;
                let base2 = hist.0.len();

                // HACK: `r` and `l` intentionally swapped for better error messages
                // why it works like that? idk /shrug, probably some programming errs previously
                // and i am too lazy so i didn't fix it
                let mut errors = false;
                for (l, r) in lp.clone().into_iter().zip(rp.clone()) {
                    errors |= self._constrain_ids(r, l, hist.0, base2).is_err();
                }

                errors |= self._constrain_ids(ra, la, hist.0, base2).is_err();

                return (!errors).then_some(()).ok_or_else(|| ());
            },
            _ => {},
        }

        let l_anyint = matches!(self.types[l].base, CheckingBaseType::Integer);
        let r_anyint = matches!(self.types[r].base, CheckingBaseType::Integer);
        if (l_anyint && self.types[r].base.is_int()) || (r_anyint && self.types[l].base.is_int()) {
            self.link(l, r);
            set(self);
            return Ok(());
        }

        let l_anyuint = matches!(self.types[l].base, CheckingBaseType::UnsignedInteger);
        let r_anyuint = matches!(self.types[r].base, CheckingBaseType::UnsignedInteger);
        if (l_anyuint && self.types[r].base.is_uint())
            || (r_anyuint && self.types[l].base.is_uint())
        {
            self.link(l, r);
            set(self);
            return Ok(());
        }

        if hist.0.len() == base + 1 {
            self.errs.push((TypeCheckError::TypeMismatch {
                expected: self.format_id(hist.0[base].0),
                found: self.format_id(hist.0[base].1),
                because: self.types[hist.0[base].0].derived_from.clone(),
            }, self.types[hist.0[base].1].derived_from.clone()));

            self.recursive_error(l);
            self.recursive_error(r);
        }

        Err(())
    }

    fn format_id(&self, id: usize) -> String {
        let t = &self.types[id];

        let mut acc = String::new();

        #[cfg(debug_assertions)]
        if t.is_forced {
            acc += "strict ";
        }

        match &t.base {
            CheckingBaseType::Pointer(t) => {
                acc += "&";
                acc += &self.format_id(*t);
            },
            CheckingBaseType::Slice(t) => {
                acc += "[";
                acc += &self.format_id(*t);
                acc += "]";
            },
            CheckingBaseType::Array(t, s) => {
                let s = s.0;
                acc += "[";
                acc += &self.format_id(*t);
                acc += &format!(" * {s}]");
            },
            CheckingBaseType::BuiltIn(b) => acc += &b.to_string(),
            CheckingBaseType::Any => acc += "_",
            CheckingBaseType::Error => acc += "{err}",
            CheckingBaseType::Function(args, ret) => {
                let ret = *ret;
                acc += "(fn(";
                acc += &args
                    .clone()
                    .into_iter()
                    .map(|a| self.format_id(a))
                    .collect::<Vec<String>>()
                    .join(", ");
                acc += ") ";
                acc += &self.format_id(ret);
                acc += ")";
            },
            CheckingBaseType::Integer => acc += "{int}",
            CheckingBaseType::UnsignedInteger => acc += "{uint}",
        }

        acc
    }

    fn output_type(&mut self, ti: usize) -> AType { self._output_type(ti, &mut vec![]) }

    fn _output_type(&mut self, ti: usize, hist: &mut Vec<usize>) -> AType {
        if hist.contains(&ti) {
            return (Type::Any, 0..0);
        }

        hist.push(ti);
        let t = self.types[ti].clone();
        let r = (
            match t.base {
                CheckingBaseType::Pointer(t) => Type::Pointer(Box::new(self._output_type(t, hist))),
                CheckingBaseType::Slice(t) => Type::Slice(Box::new(self._output_type(t, hist))),
                CheckingBaseType::Array(t, s) => {
                    Type::Array(Box::new(self._output_type(t, hist)), s)
                },

                CheckingBaseType::Function(a, r) => Type::Function(
                    a.into_iter().map(|a| self._output_type(a, hist)).collect(),
                    Box::new(self._output_type(r, hist)),
                ),

                CheckingBaseType::BuiltIn(b) => Type::BuiltIn(b),

                CheckingBaseType::Integer => Type::BuiltIn(BuiltInType::Int),
                CheckingBaseType::UnsignedInteger => Type::BuiltIn(BuiltInType::Uint),
                CheckingBaseType::Any => {
                    // HACK: maybe error made it unable to properly link types
                    if !self.errs.iter().any(|(e, _)| !matches!(e, TypeCheckError::UnresolvedType)) {
                        self.errs
                            .push((TypeCheckError::UnresolvedType, t.derived_from.clone()));
                        self.types[ti].base = CheckingBaseType::Error;

                        let t = &mut self.types[ti];
                        t.links_to.append(&mut t.linked_from);
                        self.finalize_id(ti);
                    }

                    Type::Any
                },
                CheckingBaseType::Error => {
                    Type::Any
                },
            },
            t.derived_from,
        );

        hist.pop();
        r
    }
}
