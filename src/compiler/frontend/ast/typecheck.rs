use super::*;

pub fn typecheck(ast: &NumeratedAst, count: usize) -> (Vec<AType>, Vec<AError<TypeCheckError>>) {
    let mut typechecker = Typechecker {
        types: vec![(Type::Any, 0..0); count],
        errs: Vec::new(),
    };

    typechecker.typecheck_ast(ast);

    (typechecker.types, typechecker.errs)
}

struct Typechecker {
    types: Vec<AType>,
    errs: Vec<AError<TypeCheckError>>,
}

impl Typechecker {
    fn typecheck_ast(&mut self, ast: &NumeratedAst) {
        for n in ast.iter() {
            self.typecheck_node(n);
        }
    }

    fn constrain(&mut self, id: usize, t: AType) {
        let int = self.types[id].0.constrain(&t.0);
        if let Ok(int) = int {
            match &mut self.types[id].0 {
                Type::LValue(t) => **t = int,
                t => *t = int,
            }
        } else {
            self.errs.push((int.unwrap_err(), t.1));
            self.errs.push((TypeCheckError::RequiredBecauseOf, self.types[id].1.clone()));
        }
    }

    fn def_in(&mut self, id: usize, span: Span) {
        if self.types[id].1 == (0..0) {
            self.types[id].1 = span;
        }
    }

    fn lvalue(&mut self, id: usize) {
        self.types[id].0 = Type::LValue(Box::new(self.types[id].0.clone()));
    }

    fn is_lvalue(&mut self, id: usize, span: &Span) {
        if !matches!(&self.types[id].0, Type::LValue(_)) {
            self.errs.push((TypeCheckError::TypeMismatch { expected: Type::LValue(Box::new(Type::Any)), found: self.types[id].0.clone() }, span.clone()));
        }
    }

    fn typecheck_node(&mut self, n: &Node<NumeratedNode>) {
        match &n.kind {
            NodeKind::VarDeclare { ident, typ, expr, .. } => {
                if let Some(t) = typ {
                    self.def_in(ident.1.1, t.1.clone());
                    self.constrain(ident.1.1, t.clone());
                }

                if let Some(expr) = expr {
                    self.def_in(ident.1.1, expr.1.0.clone());
                    self.typecheck_expr(expr, Some(self.types[ident.1.1].clone()));
                    self.constrain(ident.1.1, self.types[expr.1.1].clone());
                }
            },
            NodeKind::Expr(expr) => self.typecheck_expr(expr, None),
            _ => todo!("{n:?}"),
        }
    }

    fn typecheck_expr(&mut self, expr: &NExpr, constraint: Option<AType>) {
        if let Some((c, s)) = constraint.clone() {
            self.def_in(expr.1.1, s.clone());
            self.constrain(expr.1.1, (c, expr.1.0.clone()));
        }

        self.def_in(expr.1.1, expr.1.0.clone());

        match &expr.0 {
            Expr::Integer(_) => self.constrain(expr.1.1, (Type::Integer, expr.1.0.clone())),
            Expr::Ident(id) => {
                self.constrain(expr.1.1, self.types[id.1].clone());
                self.constrain(id.1, self.types[expr.1.1].clone());
                self.lvalue(expr.1.1);
            },
            Expr::UnOp { op: Operator::Deref, opr } => self.typecheck_expr(opr, Some((Type::Pointer(Box::new(self.types[expr.1.1].clone())), expr.1.0.clone()))),
            Expr::UnOp { op: Operator::Ref, opr } => {
                let t = match &self.types[expr.1.1].0 {
                    Type::Pointer(p) => (**p).clone(),
                    _ => (Type::Any, expr.1.0.clone()),
                };
                self.typecheck_expr(opr, Some(t));
                self.constrain(expr.1.1, (Type::Pointer(Box::new(self.types[opr.1.1].clone())), expr.1.0.clone()));
            },
            Expr::UnOp { opr, .. } => {
                self.typecheck_expr(opr, constraint);
                self.constrain(expr.1.1, self.types[opr.1.1].clone());
            },
            Expr::BiOp { lhs, rhs, op: Operator::Assign | Operator::OpAssign(_) } => {
                self.typecheck_expr(&lhs, Some(self.types[rhs.1.1].clone()));
                self.typecheck_expr(&rhs, Some(self.types[lhs.1.1].clone()));
                self.typecheck_expr(&lhs, Some(self.types[rhs.1.1].clone()));

                self.is_lvalue(lhs.1.1, &lhs.1.0);
                self.constrain(expr.1.1, (Type::BuiltIn(BuiltInType::Unit), expr.1.0.clone()));
            },
            Expr::BiOp { lhs, rhs, op: Operator::Eq | Operator::NE | Operator::GT | Operator::GE | Operator::LT | Operator::LE | Operator::AndAnd | Operator::OrOr } => {
                self.typecheck_expr(&lhs, Some(self.types[rhs.1.1].clone()));
                self.typecheck_expr(&rhs, Some(self.types[lhs.1.1].clone()));
                self.constrain(expr.1.1, (Type::BuiltIn(BuiltInType::Bool), expr.1.0.clone()));
            },
            Expr::BiOp { lhs, rhs, op: Operator::Index } => {
                self.typecheck_expr(&lhs, Some((Type::Slice(Box::new(self.types[expr.1.1].clone())), expr.1.0.clone())));
                self.typecheck_expr(&rhs, Some((Type::UnsignedInteger, expr.1.0.clone())));

                self.lvalue(expr.1.1);
            },
            Expr::BiOp { lhs, rhs, op: _ } => {
                self.typecheck_expr(&lhs, Some(self.types[rhs.1.1].clone()));
                self.typecheck_expr(&rhs, Some(self.types[lhs.1.1].clone()));
                self.constrain(expr.1.1, self.types[lhs.1.1].clone());
            },
            _ => todo!("{expr:?}"),
        }
    }
}
