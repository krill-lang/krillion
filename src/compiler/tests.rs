use super::frontend::*;
use crate::args::Args;

macro_rules! unwrap {
    ($a: expr, $src: expr) => {{
        let a = $a;
        let (msg, err) = report(a.1, "unit test", $src, &Args::default());
        print!("{}", msg);
        if err {
            std::process::exit(1);
        }

        a.0
    }};
}

fn compile(src: &str) -> (UntypedAst, Vec<AError<ParseError>>) {
    let mut l = Token::lexer(&src);
    let mut buf = unwrap!(to_atoken_buf(&mut l), src);
    let mut pp = unwrap!(preprocess(&mut buf), src);

    parse(&mut pp, &src)
}

macro_rules! test_on {
    ($name: ident : $src: tt -> $exp: tt) => {
        #[test]
        fn $name() {
            let src = $src;
            assert_eq!(format!("{:?}", unwrap!(compile(src), src)), $exp);
        }
    };
}

test_on!(functions:
    r#"
        fn main(argc int, argv []str) u8 {
        }

        fn nothing() {}
        fn something() int {}
        fn the_wall(a int) {}
    "#
    -> "[(FunctionDeclare { ident: (\"main\", 12..16), params: [((\"argc\", 17..21), (BuiltIn(Int)\
    , 22..25), 17..25), ((\"argv\", 27..31), (Slice((BuiltIn(Str), 34..37)), 32..34), 27..34)], ret\
    urn_type: (BuiltIn(U8), 39..41), body: [], span: 9..43, ended: true }, 9..53), (FunctionDeclar\
    e { ident: (\"nothing\", 66..73), params: [], return_type: (BuiltIn(Unit), 76..77), body: [], s\
    pan: 63..77, ended: true }, 63..78), (FunctionDeclare { ident: (\"something\", 90..99), params\
    : [], return_type: (BuiltIn(Int), 102..105), body: [], span: 87..107, ended: true }, 87..108), \
    (FunctionDeclare { ident: (\"the_wall\", 120..128), params: [((\"a\", 129..130), (BuiltIn(Int)\
    , 131..134), 129..134)], return_type: (BuiltIn(Unit), 136..137), body: [], span: 117..137, ende\
    d: true }, 117..138)]"
);

test_on!(let_declartions:
    r#"
        let untyped
        let inited = 1
        let typed int
        let typed_inited int = 2
    "#
    -> "[(VarDeclare { ident: (\"untyped\", 13..20), typ: None, expr: None }, 9..20), (VarDeclare \
    { ident: (\"inited\", 33..39), typ: None, expr: Some((Integer(1), 42..43)) }, 29..43), (VarDecl\
    are { ident: (\"typed\", 56..61), typ: Some((BuiltIn(Int), 62..65)), expr: None }, 52..65), (Va\
    rDeclare { ident: (\"typed_inited\", 78..90), typ: Some((BuiltIn(Int), 91..94)), expr: Some((In\
    teger(2), 97..98)) }, 74..98)]"
);

test_on!(expr_precidence:
    r#"
        1 + 2 * 3
        1 + 1 > 2 * 2
        a = 1 > 2
        b += (c = 0)
    "#
    -> "[(Expr((BiOp { lhs: (Integer(1), 9..10), rhs: (BiOp { lhs: (Integer(2), 13..14), rhs: (Inte\
    ger(3), 17..18), op: Mlt }, 13..18), op: Add }, 9..18)), 9..18), (Expr((BiOp { lhs: (BiOp { lhs\
    : (Integer(1), 27..28), rhs: (Integer(1), 31..32), op: Add }, 27..32), rhs: (BiOp { lhs: (Integ\
    er(2), 35..36), rhs: (Integer(2), 39..40), op: Mlt }, 35..40), op: GT }, 27..40)), 27..40), (Ex\
    pr((BiOp { lhs: (Ident([\"a\"]), 49..50), rhs: (BiOp { lhs: (Integer(1), 53..54), rhs: (Integer\
    (2), 57..58), op: GT }, 53..58), op: Assign }, 49..58)), 49..58), (Expr((BiOp { lhs: (Ident([\"\
    b\"]), 67..68), rhs: (BiOp { lhs: (Ident([\"c\"]), 73..74), rhs: (Integer(0), 77..78), op: Assi\
    gn }, 73..78), op: OpAssign(Add) }, 67..78)), 67..79)]"
);

test_on!(scopes:
    r#"
        let a = 0
        while a {
            let a = 0
        }
        {
            let b = 0
        }
        if a == 1 {
            let c = 0
        }
        return a
    "#
    -> "[(VarDeclare { ident: (\"a\", 13..14), typ: None, expr: Some((Integer(0), 17..18)) }, 9..18\
    ), (While { cond: (Ident([\"a\"]), 33..34), body: [(VarDeclare { ident: (\"a\", 53..54), typ: N\
    one, expr: Some((Integer(0), 57..58)) }, 49..58)], span: 27..36, ended: true }, 27..68), (Scop\
    e { body: [(VarDeclare { ident: (\"b\", 95..96), typ: None, expr: Some((Integer(0), 99..100)) }\
    , 91..100)], span: 77..78, ended: true }, 77..110), (If { main: [((BiOp { lhs: (Ident([\"a\"])\
    , 122..123), rhs: (Integer(1), 127..128), op: Eq }, 122..128), [(VarDeclare { ident: (\"c\", 14\
    7..148), typ: None, expr: Some((Integer(0), 151..152)) }, 143..152)], 119..121)], els: None, en\
    ded: true }, 119..162), (Return(Some((Ident([\"a\"]), 178..179))), 171..179)]"
);
