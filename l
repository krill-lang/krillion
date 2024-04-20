[
    Node {
        kind: FunctionDeclare {
            vis: None,
            link: None,
            ident: (
                "a",
                (
                    3..4,
                    0,
                ),
            ),
            params: [],
            return_type: (
                BuiltIn(
                    Unit,
                ),
                7..8,
            ),
            body: Node {
                kind: Scope {
                    body: [
                        Node {
                            kind: VarDeclare {
                                vis: None,
                                link: None,
                                ident: (
                                    "b",
                                    (
                                        14..15,
                                        1,
                                    ),
                                ),
                                typ: None,
                                expr: Some(
                                    (
                                        Integer(
                                            0,
                                        ),
                                        (
                                            18..19,
                                            2,
                                        ),
                                    ),
                                ),
                            },
                            span: 10..18,
                            extra: NodeExtra,
                        },
                    ],
                    span: 7..21,
                },
                span: 7..21,
                extra: NodeExtra,
            },
            span: 7..21,
        },
        span: 0..21,
        extra: NodeExtra,
    },
    Node {
        kind: VarDeclare {
            vis: None,
            link: None,
            ident: (
                "b",
                (
                    26..27,
                    3,
                ),
            ),
            typ: None,
            expr: Some(
                (
                    Integer(
                        1,
                    ),
                    (
                        30..31,
                        4,
                    ),
                ),
            ),
        },
        span: 22..30,
        extra: NodeExtra,
    },
    Node {
        kind: Expr(
            (
                BiOp {
                    lhs: (
                        Ident(
                            (
                                [
                                    (
                                        "b",
                                        32..33,
                                    ),
                                ],
                                3,
                            ),
                        ),
                        (
                            32..33,
                            6,
                        ),
                    ),
                    rhs: (
                        Integer(
                            2,
                        ),
                        (
                            36..37,
                            7,
                        ),
                    ),
                    op: Add,
                },
                (
                    32..37,
                    5,
                ),
            ),
        ),
        span: 32..36,
        extra: NodeExtra,
    },
]
0
5
9
10
11
12
18
19
22
23
[1;33mWarning: your mom is waiting you for dinner
[1;34m  ┌─[0;1m In: [0ma.kr [90m(1:1 to 5:5)[0m
[1;34m  │[0m
[1;34m1 │[0m [35mfn[0m[90m [0m[34ma[0m[37m([0m[37m)[0m[90m [0m[37m{[0m[90m[0m
[1;34m  │ [0;1;33m^^^^^^^^[0m
[1;34m2 │[0m [90m    [0m[35mlet[0m[90m [0m[37mb[0m[90m [0m[35m=[0m[90m [0m[33m0[0m[90m[0m
[1;34m  │ [0;1;33m^^^^^^^^^^^^^[0m
[1;34m3 │[0m [37m}[0m[90m[0m
[1;34m  │ [0;1;33m^[0m
[1;34m4 │[0m [35mlet[0m[90m [0m[37mb[0m[90m [0m[35m=[0m[90m [0m[34ma[0m[37m([0m[37m)[0m[90m[0m
[1;34m  │ [0;1;33m^^^^^^^^^^^[0m
[1;34m5 │[0m [37mb[0m[90m [0m[35m+[0m[90m [0m[33m2[0m[90m[0m
[1;34m  │ [0;1;33m^^^^^[0m
[1;34m  │[0m
[1;34m  └─ [0;1mConsider:[0m have dinner

root:
[
    Node {
        kind: FunctionDeclare {
            vis: None,
            link: None,
            ident: (
                "a",
                3..4,
            ),
            params: [],
            return_type: (
                BuiltIn(
                    Unit,
                ),
                7..8,
            ),
            body: Node {
                kind: Scope {
                    body: [
                        Node {
                            kind: VarDeclare {
                                vis: None,
                                link: None,
                                ident: (
                                    "b",
                                    14..15,
                                ),
                                typ: None,
                                expr: Some(
                                    (
                                        Integer(
                                            0,
                                        ),
                                        18..19,
                                    ),
                                ),
                            },
                            span: 10..18,
                            extra: NodeExtra,
                        },
                    ],
                    span: 7..21,
                },
                span: 7..21,
                extra: NodeExtra,
            },
            span: 7..21,
        },
        span: 0..21,
        extra: NodeExtra,
    },
    Node {
        kind: VarDeclare {
            vis: None,
            link: None,
            ident: (
                "b",
                26..27,
            ),
            typ: None,
            expr: Some(
                (
                    FnCall {
                        id: (
                            Ident(
                                [
                                    (
                                        "a",
                                        30..31,
                                    ),
                                ],
                            ),
                            30..31,
                        ),
                        op: [],
                    },
                    30..33,
                ),
            ),
        },
        span: 22..32,
        extra: NodeExtra,
    },
    Node {
        kind: Expr(
            (
                BiOp {
                    lhs: (
                        Ident(
                            [
                                (
                                    "b",
                                    34..35,
                                ),
                            ],
                        ),
                        34..35,
                    ),
                    rhs: (
                        Integer(
                            2,
                        ),
                        38..39,
                    ),
                    op: Add,
                },
                34..39,
            ),
        ),
        span: 34..38,
        extra: NodeExtra,
    },
]
