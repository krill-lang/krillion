mod kinds;
pub use kinds::*;

use super::*;
use crate::args::{Args, ErrorStyle};
use frontend::*;
use core::fmt;
use unicode_width::UnicodeWidthStr;

#[derive(Debug, Clone)]
pub struct ErrorContext<'a> {
    pub filename: &'a str,
    pub source: &'a str,
    pub cat: Vec<usize>,
    pub args: &'a Args,
}

pub type AError<E> = (E, Span);

#[derive(Debug, Clone)]
pub enum Severeness {
    Error,
    Warning,
    Info,
}

#[derive(Debug, Clone)]
pub struct Marker {
    pub message: String,
    pub span: Span,
    pub style: MarkerStyle,
}

#[derive(Debug, Clone)]
pub enum MarkerStyle {
    Primary,
    Secondary,
}

impl Severeness {
    const fn as_str(&self, args: &Args) -> &'static str {
        match (self, args.alt_color) {
            (Self::Error, false) => "\x1b[1;31mError",
            (Self::Error, true) => "\x1b[1;97mError",
            (Self::Warning, _) => "\x1b[1;33mWarning",
            (Self::Info, false) => "\x1b[1;32mInfo",
            (Self::Info, true) => "\x1b[1;34mInfo",
        }
    }
}

pub fn report<E: CompilerError>(
    errs: Vec<AError<E>>,
    filename: &str,
    src: &str,
    args: &Args,
) -> (String, bool) {
    let mut cat = Vec::with_capacity(src.matches('\n').count() + 1);
    let mut j = 0;
    for el in src.split('\n') {
        cat.push(j);
        j += el.len() + 1;
    }
    cat.push(j - 1);

    let ctx = ErrorContext {
        cat,
        filename,
        source: src,
        args,
    };

    let mut out = String::new();
    let mut have_error = false;

    for (err, span) in errs {
        report_single(&err, &ctx, span, &mut out).unwrap();
        have_error |= matches!(err.severeness(), Severeness::Error);
    }

    (out, have_error)
}

fn report_single<E: CompilerError, W: fmt::Write>(
    err: &E,
    ctx: &ErrorContext<'_>,
    span: Span,
    out: &mut W,
) -> fmt::Result {
    let markers = err.markers(span.clone());

    let endest = markers.last().unwrap().span.end;
    let line_no_len = endest.ilog10() as usize + 1;

    let start = byte_to_position(ctx, span.start);
    let end = byte_to_position(ctx, span.end);

    let mut important_lines = Vec::new();
    for m in markers.iter() {
        important_lines.push(byte_to_position(ctx, m.span.start).line);
        important_lines.push(byte_to_position(ctx, m.span.end).line);
    }

    important_lines.sort();
    important_lines.dedup();

    writeln!(
        out,
        "{}: {}",
        err.severeness().as_str(ctx.args),
        err.message()
    )?;

    let empty = "";
    writeln!(
        out,
        "\x1b[1;34m{empty:<line_no_len$} ┌─\x1b[0;1m In: \x1b[0m{} \x1b[90m({start} to {end})\x1b[0m",
        ctx.filename,
    )?;
    writeln!(out, "\x1b[1;34m{empty:<line_no_len$} │\x1b[0m")?;

    for (i, l) in important_lines.iter().enumerate() {
        let l_s1 = l + 1;
        write!(out, "\x1b[1;34m{l_s1:<line_no_len$} │ \x1b[0m")?;

        if !matches!(ctx.args.error_style, ErrorStyle::NoHighlight) {
            let hl = ctx.source[ctx.cat[*l]..ctx.cat[*l + 1]].trim_end().to_string() + "\n";
            let mut hl = HighlightToken::lexer(&hl);
            let mut t0 = hl.next();

            while let Some(t) = t0 {
                let chunk = hl.slice().replace('\t', "    ").replace('\n', "");
                let t1 = hl.next();
                write!(out, "{}", highlight(&t, &t1, &chunk))?;

                t0 = t1;
            }

            writeln!(out, "\x1b[0m")?;
        } else {
            writeln!(out, "{}", ctx.source[ctx.cat[*l]..ctx.cat[*l + 1]].trim_end())?;
        }

        for m in markers.iter() {
            m.mark_line(ctx, line_no_len, *l, out)?;
        }

        if important_lines.len() > (i + 1) && important_lines[i + 1] != l_s1 {
            let omitted = important_lines[i + 1] - l_s1;
            let s = if omitted != 1 { "s" } else { "" };
            writeln!(out, "\x1b[0;90m({omitted} line{s} omitted)")?;
        }
    }

    writeln!(out, "\x1b[1;34m{empty:<line_no_len$} │\x1b[0m")?;

    if let Some(c) = err.consider() {
        writeln!(out, "\x1b[1;34m{empty:<line_no_len$} └─ \x1b[0;1mConsider:\x1b[0m {c}")?;
    }

    writeln!(out)
}

fn byte_to_position(ctx: &ErrorContext<'_>, pos: usize) -> Position {
    let line = ctx.source[..pos]
        .matches('\n')
        .count();
    let rel = &ctx.source[ctx.cat[line]..pos];

    Position {
        line,
        column: UnicodeWidthStr::width_cjk(rel) + rel.matches('\t').count() * 4,
    }
}

struct Position {
    line: usize,
    column: usize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.column + 1)
    }
}

impl Marker {
    fn mark_line<W: fmt::Write>(&self, ctx: &ErrorContext<'_>, offset: usize, line: usize, out: &mut W) -> fmt::Result {
        if self.span.start > ctx.cat[line + 1] || ctx.cat[line] > self.span.end {
            return Ok(());
        }

        let start = byte_to_position(ctx, self.span.start.max(ctx.cat[line]));
        let end = byte_to_position(ctx, self.span.end.min(ctx.cat[line + 1] - 1));

        let empty = "";

        let prepending = start.column;
        let length = end.column - start.column;

        write!(out, "\x1b[1;34m{empty:<offset$} │ \x1b[0;")?;
        self.style.mark(prepending, length, out)?;

        if self.span.end < ctx.cat[line + 1] {
            writeln!(out, " {}", self.message)
        } else {
            writeln!(out)
        }
    }
}

impl MarkerStyle {
    fn mark<W: fmt::Write>(&self, prepending: usize, length: usize, out: &mut W) -> fmt::Result {
        let empty = "";

        match self {
            Self::Primary => write!(out, "1;33m{empty:<prepending$}{empty:^<length$}"),
            Self::Secondary => write!(out, "1;34m{empty:<prepending$}{empty:─<length$}"),
        }
    }
}
