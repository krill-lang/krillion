use super::*;
use std::collections::HashMap;

pub fn typecheck(ast: &UntypedAst) -> Result<TypedAst, Vec<ACompileError>> {
    let mut err: Vec<ACompileError> = Vec::new();

    let mut fn_signatures = HashMap::new();
    let mut globals = HashMap::new();
    for n in ast.iter() {
        match &n.0 {
            Node::FunctionDeclare { ident, params, return_type, .. } => {
                fn_signatures.insert(vec![ident.0.clone()], (
                    params.iter().map(|a| a.1.0.clone()).collect::<Vec<Type>>(),
                    return_type.clone().map_or(Type::BuiltIn(BuiltInType::Unit), |a| a.0)
                ));
            },
            Node::VarDeclare { ident, typ, expr } => {
                globals.insert(vec![ident.0.clone()],
                    typ.clone().map_or_else(|| {
                        match expr {
                            Some(e) => typeof_expr(e, &mut err, &fn_signatures, &globals),
                            None => Type::Any,
                        }
                    }, |a| a.0)
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

fn typecheck_node(node: &AUntypedNode, ast: &mut TypedAst, err: &mut Vec<ACompileError>, fn_signatures: &HashMap<Identifier, (Vec<Type>, Type)>, scope: &mut HashMap<Identifier, Type>) {
    match node.0.clone() {
        Node::FunctionDeclare { ident, params, return_type, body, span, ended } => {
            let mut scope = scope.clone();
            for p in params.iter() {
                scope.insert(vec![p.0.0.clone()], p.1.0.clone());
            }
            let mut b = TypedAst::new();
            for n in body.iter() {
                typecheck_node(n, &mut b, err, &fn_signatures, &mut scope);
            }
            ast.push((Node::FunctionDeclare { ident, params, return_type, body: b, span, ended }, node.1.clone()));
        },
        Node::VarDeclare { ident, typ, ref expr } => {
            let typ = typ.clone().map_or_else(|| {
                match expr {
                    Some(e) => typeof_expr(e, err, fn_signatures, &*scope),
                    None => Type::Any,
                }
            }, |a| a.0);
            scope.insert(vec![ident.0.clone()], typ.clone());
            ast.push((Node::VarDeclare { ident: ident.clone(), typ: Some((typ.clone(), Span::default())), expr: expr.clone().map(|a| (a, typ)) }, node.1.clone()));
        },
        Node::Return(ref expr) => {
            // TODO: check return type
            let typ = match expr {
                Some(e) => typeof_expr(e, err, fn_signatures, &*scope),
                None => Type::BuiltIn(BuiltInType::Unit),
            };
            ast.push((Node::Return(expr.clone().map(|a| (a, typ))), node.1.clone()));
        },
        _ => todo!("{node:?}"),
    }
}

fn typeof_expr(expr: &AExpr, err: &mut Vec<ACompileError>, fn_signatures: &HashMap<Identifier, (Vec<Type>, Type)>, scope: &HashMap<Identifier, Type>) -> Type {
    match &expr.0 {
        Expr::BiOp { lhs, rhs, op } => {
            let l = typeof_expr(lhs, err, fn_signatures, scope);
            let r = typeof_expr(rhs, err, fn_signatures, scope);
            match **op {
                Operator::Index => {
                    match l {
                        Type::Slice(item) | Type::Array(item, _) => if r.is_integer() {
                            item.0
                        } else {
                            err.push((Box::new(TypeCheckError::TypeMismatch { expected: Type::Integer, found: r }), rhs.1.clone()));
                            item.0
                        },
                        _ => {
                            err.push((Box::new(TypeCheckError::TypeMismatch { expected: Type::Slice(Box::new((Type::Any, Span::default()))), found: l }), lhs.1.clone()));
                            Type::Any
                        },
                    }
                },
                _ => if l == r {
                    l
                } else {
                    err.push((Box::new(TypeCheckError::TypeMismatch { expected: l, found: r }), rhs.1.clone()));
                    Type::Any
                },
            }
        },
        Expr::UnOp { opr, .. } => {
            typeof_expr(opr, err, fn_signatures, scope)
        },
        Expr::Ident(id) => {
            scope.get(id).cloned().unwrap_or_else(|| {
                err.push((Box::new(TypeCheckError::UnknownIdent), expr.1.clone()));
                Type::Any
            })
        },
        Expr::FnCall { id, op } => {
            // TODO: check is args valid
            fn_signatures.get(id).cloned().map_or_else(|| {
                err.push((Box::new(TypeCheckError::UnknownIdent), expr.1.clone()));
                Type::Any
            }, |a| a.1.clone())
        },
        Expr::Integer(_) => Type::Integer,
    }
}
