mod compiler;
use compiler::*;

fn main() {
    let filename = "test.funn++";
    let f = std::fs::read_to_string(filename).unwrap();
    let mut l = Token::lexer(&f);
    let mut buf = to_atoken_buf(&mut l).unwrap();
    let mut pp = preprocess(&mut buf);

    let ast = parse(&mut pp, &f);
    match ast {
        Ok(ast) => println!("{ast:#?}"),
        Err(err) => {
            println!("{}", reports(err, filename, &f));
        },
    }

    // while let Some(tok) = pp.next() {
    //     println!("{tok:?}");
    // }
}
