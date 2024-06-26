use super::*;
use std::collections::HashMap;

pub type Errors = Vec<AError<NumerateError>>;

pub type ANumeration = (Span, usize);
pub type ANumerated<T> = (T, ANumeration);
pub type Numeration = usize;
pub type Numerated<T> = (T, Numeration);

struct Numerator {
    errs: Errors,
    index: usize,
}

pub fn numerate(ast: UntypedAst) -> ((NumeratedAst, usize), Errors) {
    let mut numerator = Numerator {
        errs: Vec::new(),
        index: 0,
    };

    (
        (numerator.numerate(ast, HashMap::new()), numerator.index),
        numerator.errs,
    )
}

impl Numerator {
    fn numerate(&mut self, ast: UntypedAst, mut idents: HashMap<String, usize>) -> NumeratedAst {
        self.resolve_globals(&ast, &mut idents);

        let mut new = NumeratedAst::with_capacity(ast.len());

        for n in ast.into_iter() {
            new.push(self.numerate_single(n, &mut idents));
        }

        new
    }

    fn numerate_single(
        &mut self,
        n: Node<UntypedNode>,
        idents: &mut HashMap<String, usize>,
    ) -> Node<NumeratedNode> {
        match n.kind {
            NodeKind::Expr(expr) => Node {
                kind: NodeKind::Expr(self.numerate_expr(expr, idents)),
                span: n.span,
                extra: n.extra,
            },
            NodeKind::VarDeclare {
                vis,
                link,
                ident,
                typ,
                expr,
            } => {
                let expr = expr.map(|expr| self.numerate_expr(expr, idents));

                let id = self.assign();
                idents.insert(ident.0.clone(), id);

                Node {
                    kind: NodeKind::VarDeclare {
                        vis,
                        link,

                        ident: (ident.0, (ident.1, id)),
                        typ, // TODO:
                        expr,
                    },
                    span: n.span,
                    extra: n.extra,
                }
            },
            NodeKind::FunctionDeclare {
                vis,
                link,
                ident,
                params,
                return_type,
                body,
                span,
            } => {
                let (kb, ks) = match body.kind {
                    NodeKind::Scope { body, span } => (body, span),
                    _ => unreachable!(),
                };

                let mut new_params = Vec::with_capacity(params.len());
                let mut inner_idents = idents.clone();
                for p in params.into_iter() {
                    let p_id = self.assign();
                    new_params.push(((p.0.0.clone(), (p.0.1, p_id)), p.1, p.2));
                    inner_idents.insert(p.0.0, p_id);
                }

                let ident_id = *idents.get(&ident.0).unwrap();

                Node {
                    kind: NodeKind::FunctionDeclare {
                        vis,
                        link,
                        ident: (ident.0, (ident.1, ident_id)),
                        params: new_params,
                        return_type,
                        span,
                        body: Box::new(Node {
                            kind: NodeKind::Scope {
                                body: self.numerate(kb, inner_idents),
                                span: ks,
                            },
                            span: body.span,
                            extra: body.extra,
                        }),
                    },
                    span: n.span,
                    extra: n.extra,
                }
            },
            NodeKind::Return(expr) => Node {
                kind: NodeKind::Return(expr.map(|expr| self.numerate_expr(expr, idents))),
                span: n.span,
                extra: n.extra,
            },
            NodeKind::Scope { body, span } => Node {
                kind: NodeKind::Scope {
                    body: self.numerate(body, idents.clone()),
                    span,
                },
                span: n.span,
                extra: n.extra,
            },
            NodeKind::While { cond, body } => {
                let (kb, ks) = match body.kind {
                    NodeKind::Scope { body, span } => (body, span),
                    _ => unreachable!(),
                };

                Node {
                    kind: NodeKind::While {
                        cond: self.numerate_expr(cond, idents),
                        body: Box::new(Node {
                            kind: NodeKind::Scope {
                                body: self.numerate(kb, idents.clone()),
                                span: ks,
                            },
                            span: body.span,
                            extra: body.extra,
                        }),
                    },
                    span: n.span,
                    extra: n.extra,
                }
            },
            NodeKind::If { main, els } => {
                let (mb, ms) = match main.1.kind {
                    NodeKind::Scope { body, span } => (body, span),
                    _ => unreachable!(),
                };

                Node {
                    kind: NodeKind::If {
                        main: (
                            self.numerate_expr(main.0, idents),
                            Box::new(Node {
                                kind: NodeKind::Scope {
                                    body: self.numerate(mb, idents.clone()),
                                    span: ms,
                                },
                                span: main.1.span,
                                extra: main.1.extra,
                            }),
                            main.2,
                        ),
                        els: els.map(|els| match els.0.kind {
                            NodeKind::Scope { body, span } => (
                                Box::new(Node {
                                    kind: NodeKind::Scope {
                                        body: self.numerate(body, idents.clone()),
                                        span,
                                    },
                                    span: els.0.span,
                                    extra: els.0.extra,
                                }),
                                els.1,
                            ),
                            _ => (Box::new(self.numerate_single(*els.0, idents)), els.1),
                        }),
                    },
                    span: n.span,
                    extra: n.extra,
                }
            },
        }
    }

    fn resolve_globals(&mut self, ast: &UntypedAst, idents: &mut HashMap<String, usize>) {
        for n in ast.iter() {
            if let NodeKind::FunctionDeclare { ident, .. } = &n.kind {
                idents.insert(ident.0.clone(), self.assign());
            }
        }
    }

    fn numerate_expr(&mut self, expr: AExpr, idents: &HashMap<String, usize>) -> NExpr {
        match expr.0 {
            Expr::Integer(int) => (Expr::Integer(int), (expr.1, self.assign())),
            Expr::Ident(id) => (
                Expr::Ident(self.numerate_ident(id, &expr.1, idents)),
                (expr.1, self.assign()),
            ),
            Expr::BiOp { lhs, rhs, op } => (
                Expr::BiOp {
                    lhs: Box::new(self.numerate_expr(*lhs, idents)),
                    rhs: Box::new(self.numerate_expr(*rhs, idents)),
                    op,
                },
                (expr.1, self.assign()),
            ),
            Expr::UnOp { opr, op } => (
                Expr::UnOp {
                    opr: Box::new(self.numerate_expr(*opr, idents)),
                    op,
                },
                (expr.1, self.assign()),
            ),
            Expr::FnCall { id, op } => (
                Expr::FnCall {
                    id: Box::new(self.numerate_expr(*id, idents)),
                    op: op
                        .into_iter()
                        .map(|e| self.numerate_expr(e, idents))
                        .collect(),
                },
                (expr.1, self.assign()),
            ),
        }
    }

    fn numerate_ident(
        &mut self,
        ident: AIdent,
        span: &Span,
        idents: &HashMap<String, usize>,
    ) -> NIdent {
        if ident.len() == 1 {
            let id = *idents.get(&ident[0].0).unwrap_or_else(|| {
                self.errs.push((NumerateError::NameUndefined, span.clone()));
                &0
            });

            (ident, id)
        } else {
            todo!()
        }
    }

    fn assign(&mut self) -> usize {
        let i = self.index;
        self.index += 1;
        i
    }
}
