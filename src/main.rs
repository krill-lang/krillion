mod compiler;
use compiler::*;
mod args;
use args::*;
use std::process::exit;

macro_rules! unwrap_or_report {
    ($i: expr, $filename: expr, $src: expr) => {
        match $i {
            Ok(i) => i,
            Err(err) => {
                print!("{}", reports(err, $filename, &$src));
                exit(1);
            },
        }
    };
}

fn main() {
    let args = Args::parse();
    let filename = &args.input;

    let src = std::fs::read_to_string(filename).unwrap();
    let mut l = Token::lexer(&src);
    let mut buf = unwrap_or_report!(to_atoken_buf(&mut l), filename, src);
    let mut pp = unwrap_or_report!(preprocess(&mut buf), filename, src);

    let ast = unwrap_or_report!(parse(&mut pp, &src), filename, src);
    println!("{ast:#?}");
}
