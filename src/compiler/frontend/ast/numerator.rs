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
    idents: HashMap<AIdent, usize>,
}

pub fn numerate(ast: UntypedAst) -> (NumeratedAst, Errors) {
    let mut new = NumeratedAst::with_capacity(ast.len());
    let mut numerator = Numerator {
        errs: Vec::new(),
        index: 0,
        idents: HashMap::new(),
    };

    for n in ast.into_iter() {
        match n.kind {
            NodeKind::Expr(expr) => new.push(Node {
                kind: NodeKind::Expr(numerator.numerate_expr(expr)),
                span: n.span.clone(),
                extra: n.extra.clone(),
            }),
            _ => todo!(),
        }
    }

    (new, numerator.errs)
}

impl Numerator {
    fn numerate_expr(&mut self, expr: AExpr) -> NExpr {
        match expr.0 {
            Expr::Integer(int) => {
                let i = self.index;
                self.index += 1;

                (Expr::Integer(int), (expr.1, i))
            },
            Expr::Ident(id) => {
                let i = self.index;
                self.index += 1;

                (Expr::Ident(self.get_numerate_ident(id)), (expr.1, i))
            },
            Expr::BiOp { lhs, rhs, op } => {
                let i = self.index;
                self.index += 1;

                (
                    Expr::BiOp {
                        lhs: Box::new(self.numerate_expr(*lhs)),
                        rhs: Box::new(self.numerate_expr(*rhs)),
                        op,
                    },
                    (expr.1, i),
                )
            },
            Expr::UnOp { opr, op } => {
                let i = self.index;
                self.index += 1;

                (
                    Expr::UnOp {
                        opr: Box::new(self.numerate_expr(*opr)),
                        op,
                    },
                    (expr.1, i),
                )
            },
            _ => todo!(),
        }
    }

    fn get_numerate_ident(&mut self, expr: AIdent) -> NIdent {
    }
}
