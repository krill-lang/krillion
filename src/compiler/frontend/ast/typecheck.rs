use super::*;
use std::collections::HashMap;

pub fn typecheck(ast: &UntypedAst) -> Result<TypedAst, Vec<ACompileError>> {
    let mut err: Vec<ACompileError> = Vec::new();

    let mut fn_signatures = HashMap::new();
    let mut globals = HashMap::new();
    for n in ast.iter() {
        match &n.0 {
            Node::FunctionDeclare { ident, params, return_type, .. } => {
                fn_signatures.insert(ident.0.clone(), (params, return_type));
            },
            Node::VarDeclare { ident, typ, expr } => {
                globals.insert(ident.0.clone(),
                    typ.clone().map_or_else(|| {
                        match expr {
                            Some(e) => typeof_expr(e, &mut err, &fn_signatures, &globals),
                            None => Some(Type::Any),
                        }
                    }, |a| Some(a.0))
                );
            },
            _ => err.push((Box::new(TypeCheckError::GlobalNode), n.1.clone())),
        }
    }

    println!("{fn_signatures:?}");

    if !err.is_empty() {
        return Err(err)
    }

    let mut ta = TypedAst::with_capacity(ast.len());

    for n in ast.iter() {
        typecheck_node(n, &mut ta, &mut err, &fn_signatures, &mut globals.clone());
    }

    if err.is_empty() {
        Ok(ta)
    } else {
        Err(err)
    }
}

fn typecheck_node(node: &AUntypedNode, ast: &mut TypedAst, err: &mut Vec<ACompileError>, fn_signatures: &HashMap<String, (&Vec<(AString, AType, Span)>, &Option<AType>)>, scope: &mut HashMap<String, Option<Type>>) {
    match &node.0 {
        Node::FunctionDeclare { body, .. } => {
            let mut scope = scope.clone();
            for n in body.iter() {
                typecheck_node(n, ast, err, &fn_signatures, &mut scope);
            }
        },
        _ => todo!("{node:?}"),
    }
}

fn typeof_expr(expr: &AExpr, err: &mut Vec<ACompileError>, fn_signatures: &HashMap<String, (&Vec<(AString, AType, Span)>, &Option<AType>)>, scope: &HashMap<String, Option<Type>>) -> Option<Type> {
    match &expr.0 {
        Expr::BiOp { lhs, rhs, op } => {
            let l = typeof_expr(lhs, err, fn_signatures, scope);
            let r = typeof_expr(rhs, err, fn_signatures, scope);
            if l == r {
                l
            } else {
                err.push((Box::new(TypeCheckError::TypeMismatch { expected: l, found: r }), rhs.1));
                Some(Type::Any)
            }
        },
        Expr::UnOp { opr, op } => {
            typeof_expr(opr, err, fn_signatures, scope)
        },
        Expr::Ident(id) => {
            scope.get(id).cloned().unwrap_or_else(|| {
                err.push((Box::new(TypeCheckError::UnknownIdent), expr.1));
                Some(Type::Any)
            })
        },
        Expr::FnCall { id, op } => {
            // TODO: check is args valid
            fn_signatures.get(id).cloned().map_or_else(|| {
                err.push((Box::new(TypeCheckError::UnknownIdent), expr.1));
                Some(Type::Any)
            }, |a| a.1.map(|a| a.0).clone())
        },
        Expr::Integer(_) => Some(Type::Integer),
    }
}
