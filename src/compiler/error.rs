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
}

impl ParseError {
    pub fn report<'a>(&self, ctx: ErrorContext<'a>, span: Span) -> String {
        let mut fin = format!("\x1b[1;31mError:\x1b[0;1m the quick brown fox jumps over the lazy dog\n \x1b[1;34m-->\x1b[0m {} {span:?}\n", ctx.filename);

        let mut i = ctx.source[..span.start].matches('\n').count()+1;
        let src = &ctx.source[span.start..span.end].trim_end();
        let max_lines = i + src.matches('\n').count();
        for el in ctx.source[span.start..span.end].lines() {
            fin += &format!(" {i} | {el}\n");
            i += 1;
        }

        fin
    }
}

pub type AParseError = (ParseError, Span);
