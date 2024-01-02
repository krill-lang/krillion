use super::*;
use crate::compiler::util::*;
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
                    return_type.0.clone()
                ));
            },
            Node::VarDeclare { ident, typ, expr } => {
                let typ = typ.clone().map_or_else(|| {
                    let mut expr = expr.clone();

                    expr.as_mut().map_or(
                        Type::Any,
                        |e| typeof_expr(e, &mut err, &fn_signatures, &mut globals)
                        )
                }, |a| a.0);
                globals.insert(vec![ident.0.clone()], typ);
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
        typecheck_node(n, &mut ta, &mut err, &fn_signatures, &mut globals.clone(), &Type::BuiltIn(BuiltInType::Unit));
    }

    if err.is_empty() {
        Ok(ta)
    } else {
        Err(err)
    }
}

fn type_matches_collapse(lhs: &MaybeMutable<Type>, rhs: &MaybeMutable<Type>) -> bool {
    if *lhs.force_immut() == *rhs.force_immut() {
        return true;
    }

    let order = lhs.force_immut().specificness() <= rhs.force_immut().specificness();
    let (lhs, rhs) = if order { (lhs, rhs) } else { (rhs, lhs) };

    match (lhs.force_immut(), rhs.force_immut()) {
        (int, Type::Integer) if int.is_integer() => {
            rhs.map_mut(|a| *a = int.clone());
            true
        },
        (Type::Array(a, c), Type::Array(b, d)) => {
            type_matches_collapse(&MaybeMutable::Immutable(&a.0), &MaybeMutable::Immutable(&b.0)) && *c == *d
        },
        (Type::Slice(a) | Type::Array(a, _), Type::Slice(b) | Type::Array(b, _)) => {
            type_matches_collapse(&MaybeMutable::Immutable(&a.0), &MaybeMutable::Immutable(&b.0))
        },
        (t, Type::Any) => {
            rhs.map_mut(|a| *a = t.clone());
            true
        },
        (Type::OneOf(a), Type::OneOf(b)) => todo!(),
        (t, Type::OneOf(a)) => {
            let ret = a.contains(&t);
            rhs.map_mut(|a| *a = t.clone());
            ret
        },
        _ => {
            println!("typecheck eq collapse `{}` `{}` (assume false)", lhs.force_immut(), rhs.force_immut());
            false
        }
    }
}

fn typecheck_node(node: &AUntypedNode, ast: &mut TypedAst, err: &mut Vec<ACompileError>, fn_signatures: &HashMap<Identifier, (Vec<Type>, Type)>, scope: &mut HashMap<Identifier, Type>, ret_type: &Type) {
    macro_rules! typecheck_body {
        ($body: expr, $scope: ident, $ret_type: expr) => {{
            let mut b = TypedAst::new();
            for n in $body.iter() {
                typecheck_node(n, &mut b, err, fn_signatures, &mut $scope, $ret_type);
            }

            println!("{:?}", $scope);
            b
        }};
        ($body: expr, $scope: ident) => {{
            typecheck_body!($body, $scope, ret_type)
        }};
        ($body: expr) => {{
            let mut s = scope.clone();
            typecheck_body!($body, s)
        }};
    }

    match node.0.clone() {
        Node::FunctionDeclare { ident, params, return_type, body, span, ended } => {
            let mut scope = scope.clone();
            for p in params.iter() {
                scope.insert(vec![p.0.0.clone()], p.1.0.clone());
            }
            let body = typecheck_body!(body, scope, &return_type.0);
            ast.push((Node::FunctionDeclare { ident, params, return_type, body, span, ended }, node.1.clone()));
        },
        Node::VarDeclare { ident, typ, ref expr } => {
            let mut expr = expr.clone();
            let typ = typ.map_or_else(|| {
                expr.as_mut().map_or(
                    Type::Any,
                    |e| typeof_expr(e, err, fn_signatures, scope),
                )
            }, |a| a.0);
            scope.insert(vec![ident.0.clone()], typ.clone());
            ast.push((Node::VarDeclare { ident, typ: Some((typ.clone(), Span::default())), expr: expr.map(|a| (a, typ)) }, node.1.clone()));
        },
        Node::Expr(expr) => {
            let typ = typeof_expr(&expr, err, fn_signatures, scope);
            ast.push((Node::Expr((expr, typ)), node.1.clone()));
        },
        Node::Return(ref expr) => {
            let typ = expr.as_ref().map_or(
                Type::BuiltIn(BuiltInType::Unit),
                |e| typeof_expr(e, err, fn_signatures, scope)
            );

            if let Some(ref expr) = expr {
                if !type_matches_collapse(&ref_to_type(&expr, scope, &typ), &MaybeMutable::Immutable(ret_type)) {
                    err.push((Box::new(TypeCheckError::TypeMismatch { expected: ret_type.clone(), found: typ.clone() }), expr.1.clone()));
                }
            } else if !matches!(ret_type, Type::BuiltIn(BuiltInType::Unit)) {
                err.push((Box::new(TypeCheckError::TypeMismatch { expected: ret_type.clone(), found: typ.clone() }), node.1.clone()));
            }

            ast.push((Node::Return(expr.clone().map(|a| (a, typ))), node.1.clone()));
        },
        Node::Scope { body, span, ended } => {
            let body = typecheck_body!(body);
            ast.push((Node::Scope { body, span, ended }, node.1.clone()));
        },
        Node::While { cond, body, span, ended } => {
            let typ = typeof_expr(&cond, err, fn_signatures, scope);
            let body = typecheck_body!(body);
            ast.push((Node::While { cond: (cond, typ), body, span, ended }, node.1.clone()));
        },
        _ => todo!("{node:?}"),
    }
}

fn typeof_expr(expr: &AExpr, err: &mut Vec<ACompileError>, fn_signatures: &HashMap<Identifier, (Vec<Type>, Type)>, scope: &mut HashMap<Identifier, Type>) -> Type {
    match &expr.0 {
        Expr::BiOp { lhs, rhs, op } => {
            let l = typeof_expr(lhs, err, fn_signatures, scope);
            let r = typeof_expr(rhs, err, fn_signatures, scope);
            match **op {
                Operator::Index => {
                    let l_may_mut = ref_to_type(lhs, unsafe { as_mut(scope) }, &l);
                    let r_may_mut = ref_to_type(rhs, unsafe { as_mut(scope) }, &r);

                    if !type_matches_collapse(&l_may_mut, &MaybeMutable::Immutable(&Type::Slice(Box::new((Type::Any, Span::default()))))) {
                        err.push((Box::new(TypeCheckError::TypeMismatch { expected: Type::Slice(Box::new((Type::Any, Span::default()))), found: l_may_mut.force_immut().clone() }), lhs.1.clone()));
                    }

                    if !type_matches_collapse(&r_may_mut, &MaybeMutable::Immutable(&Type::Integer)) {
                        err.push((Box::new(TypeCheckError::TypeMismatch { expected: Type::Integer, found: r_may_mut.force_immut().clone() }), rhs.1.clone()));
                    }

                    match l_may_mut.force_immut() {
                        Type::Slice(item) | Type::Array(item, _) => item.0.clone(),
                        _ => Type::Any,
                    }
                },
                _ => {
                    let l_may_mut = ref_to_type(lhs, unsafe { as_mut(scope) }, &l);
                    let r_may_mut = ref_to_type(rhs, unsafe { as_mut(scope) }, &r);
                    if type_matches_collapse(&l_may_mut, &r_may_mut) {
                        l
                    } else {
                        err.push((Box::new(TypeCheckError::TypeMismatch { expected: l, found: r }), rhs.1.clone()));
                        Type::Any
                    }
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
            let func = fn_signatures.get(id).cloned().unwrap_or_else(|| {
                err.push((Box::new(TypeCheckError::UnknownIdent), expr.1.clone()));
                (vec![], Type::Any)
            });

            if op.len() != func.0.len() {
                err.push((Box::new(TypeCheckError::FnArgCountNotMatch { expected: func.0.len(), found: op.len() }), expr.1.clone()));
            } else {
                for i in op.iter().zip(func.0.iter()) {
                    let f_typ = typeof_expr(i.0, err, fn_signatures, scope);
                    if !type_matches_collapse(&ref_to_type(i.0, scope, &f_typ), &MaybeMutable::Immutable(i.1)) {
                        err.push((Box::new(TypeCheckError::TypeMismatch { expected: i.1.clone(), found: f_typ }), i.0.1.clone()));
                    }
                }
            }

            func.1
        },
        Expr::Integer(_) => Type::Integer,
    }
}

fn ref_to_type<'a>(expr: &AExpr, scope: &'a mut HashMap<Identifier, Type>, typ: &'a Type) -> MaybeMutable<'a, Type> {
    match &expr.0 {
        Expr::Ident(id) => MaybeMutable::Mutable(scope.get_mut(id).unwrap()),
        _ => MaybeMutable::Immutable(typ)
    }
}
