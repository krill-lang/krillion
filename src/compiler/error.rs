use super::*;

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken(Token),
    UnstartedBracket,
    UnendedBracket,
    UnendedFnCall,
    ExprParseError,
    RanOutOperands,
}

#[derive(Debug, Clone)]
pub struct ErrorContext<'a> {
    pub filename: &'a str,
    pub source: &'a str,
    pub cat: Vec<usize>
}

pub trait CompilerError where Self: std::fmt::Debug {
    fn report<'a>(&self, ctx: &ErrorContext<'a>, span: Span) -> String;
}

impl CompilerError for ParseError {
    fn report<'a>(&self, ctx: &ErrorContext<'a>, span: Span) -> String {
        let mut fin = format!("\x1b[1;31mError:\x1b[0;1m {self}\n \x1b[1;34m-->\x1b[0m {} {span:?}\n", ctx.filename);

        let mut i = ctx.source[..span.start].matches('\n').count()+1;
        let end = ctx.source[..span.end].matches('\n').count()+1;
        let chw = format!("{end}").len();
        fin += &format!("\x1b[34m{} |\x1b[0m\n", " ".repeat(chw));

        for el in ctx.source.lines().skip(i-1) {
            let spaces = span.start.checked_sub(ctx.cat[i-1]).unwrap_or(0);
            fin += &format!(
                "\x1b[34m{i}{} |\x1b[0m {}\n\x1b[34m{} | \x1b[0;1;31m{}{}\x1b[0m\n",
                " ".repeat(chw - format!("{i}").len()),
                el.trim_end(),
                " ".repeat(chw),
                " ".repeat(spaces),
                "^".repeat(el.len()-spaces-ctx.cat.get(i).unwrap_or(&0).checked_sub(span.end).unwrap_or(0)+1),
            );

            if i >= end {
                break;
            }
            i += 1;
        }

        fin += &format!("\x1b[34m{} |\x1b[0m\n", " ".repeat(chw));

        fin
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ParseError::UnendedFnCall  => write!(f, "function call have no ending bracket"),
            ParseError::UnendedBracket => write!(f, "starting bracket have no matching ending bracket"),
            ParseError::UnstartedBracket => write!(f, "ending bracket have no matching starting bracket"),
            ParseError::RanOutOperands => write!(f, "ran out of operands while parsing expression"),
            ParseError::UnexpectedToken(t) => write!(f, "unexpected token {t:?}"),
            ParseError::ExprParseError => write!(f, "unknown expression parsing error"),
        }
    }
}

pub fn reports(errs: Vec<ACompileError>, filename: &str, src: &str) -> String {
    let mut cat = Vec::with_capacity(src.split('\n').count());
    let mut j = 0;
    for el in src.split('\n') {
        cat.push(j);
        j += el.len()+1;
    }

    let ctx = ErrorContext { cat, filename, source: src };

    let mut out = String::new();
    for (err, span) in errs {
        out += &err.report(&ctx, span);
    }

    out
}

pub type ACompileError = (Box<dyn CompilerError>, Span);
