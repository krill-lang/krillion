use super::*;

pub fn typecheck(ast: &NumeratedAst, count: usize) -> (Vec<(Type, Span, Vec<Span>)>, Vec<AError<TypeCheckError>>) {
    let mut types = vec![(Type::Any, 0..0, Vec::new()); count];
    let mut typechecker = Typechecker {
        types,
        errs: Vec::new(),
    };

    typechecker.typecheck_ast(ast);

    (typechecker.types, typechecker.errs)
}

struct Typechecker {
    types: Vec<(Type, Span, Vec<Span>)>,
    errs: Vec<AError<TypeCheckError>>,
}

impl Typechecker {
    fn typecheck_ast(&mut self, ast: &NumeratedAst) {
        for n in ast.iter() {
            self.typecheck_node(n);
        }
    }

    fn constrain_on(&mut self, id: usize, t: Type, span: Span) {
        let int = self.types[id].0.clone().constrain(Type::Integer);
        if let Ok(int) = int {
            self.types[id].0 = int;
            self.types[id].2.push(span);
        } else {
            self.errs.push((int.unwrap_err(), span));
        }
    }

    fn def_in(&mut self, id: usize, span: Span) {
        self.types[id].1 = span;
    }

    fn typecheck_node(&mut self, n: &Node<NumeratedNode>) {
        match &n.kind {
            NodeKind::VarDeclare { ident, typ, expr, .. } => {
                if let Some(t) = typ {
                    self.def_in(ident.1.1, n.span.clone());
                    self.constrain_on(ident.1.1, t.0.clone(), t.1.clone());
                }

                if let Some(expr) = expr {
                    if typ.is_none() {
                        self.def_in(ident.1.1, expr.1.0.clone());
                    }
                    self.typecheck_expr(expr, Some(self.types[ident.1.1].0.clone()));
                }
            },
            NodeKind::Expr(expr) => self.typecheck_expr(expr, None),
            _ => todo!("{n:?}"),
        }
    }

    fn typecheck_expr(&mut self, expr: &NExpr, constraint: Option<Type>) {
        match &expr.0 {
            Expr::Integer(int) => {
                self.def_in(expr.1.1, expr.1.0.clone());
                self.constrain_on(expr.1.1, Type::Integer, expr.1.0.clone());
                if let Some(c) = constraint {
                    self.constrain_on(expr.1.1, c, expr.1.0.clone());
                }
            },
            _ => todo!("{expr:?}"),
        }
    }
}
