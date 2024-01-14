#![warn(
    clippy::complexity,
    clippy::correctness,
    clippy::perf,
    clippy::nursery,
    clippy::suspicious,
    clippy::style
)]
#![allow(
    clippy::semicolon_inside_block,
    clippy::just_underscores_and_digits,
    unknown_lints,
    cast_ref_to_mut,
    invalid_reference_casting,
    mutable_transmutes
)]
#![feature(mem_copy_fn)]

mod compiler;
use compiler::frontend::*;
mod args;
use args::*;
use std::process::exit;

fn main() {
    let args = Args::parse();
    let filename = &args.input;

    let src = std::fs::read_to_string(filename).unwrap();

    macro_rules! unwrap_or_report {
        ($i: expr) => {{
            let i = $i;
            let (msg, err) = report(i.1, filename, &src, &args);
            print!("{}", msg);
            if err {
                exit(1);
            }

            i.0
        }};
    }

    let mut l = Token::lexer(&src);
    let mut buf = unwrap_or_report!(to_atoken_buf(&mut l));
    let mut pp = unwrap_or_report!(preprocess(&mut buf));

    let ast = unwrap_or_report!(parse(&mut pp, &src));
    println!("{ast:#?}");
    // let typed = unwrap_or_report!(typecheck(&ast));
    // println!("{typed:#?}");
}
