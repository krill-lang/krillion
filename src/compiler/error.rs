use std::fmt::Write;
use super::*;
use crate::args::*;
use frontend::*;
use unicode_width::UnicodeWidthStr;

#[derive(Debug, Clone)]
pub struct ErrorContext<'a> {
    pub filename: &'a str,
    pub source: &'a str,
    pub cat: Vec<usize>,
    pub args: &'a Args,
}

pub type AError<E> = (E, Span);

pub trait CompilerError {
    fn message(&self) -> String;
    fn consider(&self) -> Option<String>;
    fn severeness(&self) -> Severeness;
}

pub enum Severeness {
    Error, Warning
}

pub fn report<E: CompilerError>(
    errs: Vec<AError<E>>,
    filename: &str,
    src: &str,
    args: &Args
) -> (String, bool) {
    let mut cat = Vec::with_capacity(src.split('\n').count());
    let mut j = 0;
    for el in src.split('\n') {
        cat.push(j);
        j += el.len() + 1;
    }

    let ctx = ErrorContext {
        cat,
        filename,
        source: src,
        args,
    };

    let mut out = String::new();
    let mut have_error = false;

    for (err, span) in errs {
        writeln!(out, "{}", report_single(&err, &ctx, span).unwrap()).unwrap();
        have_error |= matches!(err.severeness(), Severeness::Error);
    }

    (out, have_error)
}

fn report_single<E: CompilerError>(
    err: &E,
    ctx: &ErrorContext<'_>,
    span: Span
) -> Result<String, Box<dyn std::error::Error>> {
    let is_simple = matches!(ctx.args.error_style, ErrorStyle::Simple);

    let start = ctx.source[..span.start].matches('\n').count() + 1;
    let end = ctx.source[..span.end].matches('\n').count() + 1;
    let lines = end - start;

    let chw = format!("{end}").len();
    let start_cols = &ctx.source[ctx.cat[start - 1]..span.start];
    let end_cols = &ctx.source[ctx.cat[end - 1]..span.end];
    let mut fin = String::new();
    writeln!(
        fin,
        "{}: {}",
        match err.severeness() {
            Severeness::Error => "\x1b[1;31mError",
            Severeness::Warning => "\x1b[1;33mWarning",
        },
        err.message()
    )?;
    writeln!(
        fin,
        "\x1b[1;34m {}\u{2500}\x1b[0;1m In: \x1b[0m{} \x1b[90m({start}:{} to {end}:{})\x1b[0m",
        if is_simple {
            "\u{2514}".to_string()
        } else {
            " ".repeat(chw) + "\u{250c}"
        },
        ctx.filename,
        UnicodeWidthStr::width_cjk(start_cols) + start_cols.matches('\t').count() * 4 + 1,
        UnicodeWidthStr::width_cjk(end_cols)   + end_cols  .matches('\t').count() * 4,
    )?;

    if is_simple {
        return Ok(fin);
    }

    writeln!(fin, "\x1b[1;34m{} \u{2502}\x1b[0m", " ".repeat(chw))?;

    for (i, el) in ctx
        .source
        .lines()
        .enumerate()
        .skip(start - 1)
        .take(lines + 1)
    {
        let i = i + 1;
        if end - start >= 6 && start + 3 == i {
            writeln!(fin, "\x1b[90m({} lines omited)\n", end - start - 5)?;
        }
        if end - start >= 6 && (start + 3..=end.saturating_sub(3)).contains(&i) {
            continue;
        }

        write!(
            fin,
            "\x1b[1;34m{i}{} \u{2502}\x1b[0m ",
            " ".repeat(chw - format!("{i}").len())
        )?;

        let trim_el = el.trim_end();
        let hl_content = trim_el.to_string() + "\n";

        let mut hl = HighlightToken::lexer(&hl_content);
        let mut hl = to_atoken_buf(&mut hl).0;

        let spaces = span.start.saturating_sub(ctx.cat[i - 1]);

        let in_tabs = el[spaces..(spaces + span.end - span.start).min(el.len())]
            .matches('\t')
            .count();
        let bef_tabs = el[..spaces].matches('\t').count();

        while let Some((tok, span)) = hl.next().cloned() {
            let src = &hl_content.slice(span).unwrap();
            let src = src.replace('\t', "    ").replace('\n', "");
            write!(fin, "{}", tok.highlight(hl.peek().map(|(t, _)| t), &src))?;
        }

        let spaces = UnicodeWidthStr::width_cjk(&el[..spaces]);
        let start_idx = *ctx.cat.get(i - 1).unwrap();
        let end_idx = *ctx.cat.get(i).unwrap_or(&ctx.source.len());

        writeln!(
            fin,
            "\n\x1b[1;34m{} \u{2502} \x1b[0;1;33m{}{}\x1b[0m",
            " ".repeat(chw),
            " ".repeat(spaces + bef_tabs * 4),
            "^".repeat(
                UnicodeWidthStr::width_cjk(
                    &ctx.source[span.start.max(start_idx)..span.end.min(end_idx)]
                ) + in_tabs * 4
            ),
        )?;
    }

    writeln!(fin, "\x1b[1;34m{} \u{2502}\x1b[0m", " ".repeat(chw))?;
    if let Some(c) = err.consider() {
        writeln!(
            fin,
            "\x1b[1;34m{} \u{2514}\u{2500} \x1b[0;1mConsider:\x1b[0m {c}",
            " ".repeat(chw)
        )?;
    }

    Ok(fin)
}

#[derive(Debug, Clone)]
#[allow(clippy::enum_variant_names)]
pub enum ParseError {
    UnexpectedToken,
    UnstartedBracket,
    UnendedBracket,
    UnendedFnCall,
    UnexpectedDelimiter,
    UnendedScope,
    BracketNotMatch,
    ExprParseError,
    RanOutOperands,
    RanOutTokens,
    ExpectingIdentifier,

    YourMom,
}

impl CompilerError for ParseError {
    fn message(&self) -> String {
        match self {
            Self::UnexpectedToken => "unexpected token".to_string(),
            Self::UnstartedBracket => "ending bracket have no matching starting bracket".to_string(),
            Self::UnendedBracket => "starting bracket have no matching ending bracket".to_string(),
            Self::UnendedFnCall => "function call have no ending bracket".to_string(),
            Self::UnendedScope => "scope is not ended".to_string(),
            Self::UnexpectedDelimiter => "unexpected delimiter".to_string(),
            Self::BracketNotMatch => "starting bracket does not match ending bracket".to_string(),
            Self::ExprParseError => "unknown expression parsing error".to_string(),
            Self::RanOutOperands => "ran out of operands while parsing expression".to_string(),
            Self::RanOutTokens => "ran out of tokens".to_string(),
            Self::ExpectingIdentifier => "expecting identifier".to_string(),

            Self::YourMom => "your mom is waiting you for dinner".to_string(),
        }
    }

    fn consider(&self) -> Option<String> {
        match self {
            Self::UnexpectedToken => {
                Some("add a semicolon to end the current statement".to_string())
            },
            Self::UnendedFnCall | Self::UnendedBracket => {
                Some("add a ending bracket".to_string())
            },
            Self::UnendedScope => {
                Some("add a delimiter `}`".to_string())
            },
            Self::UnexpectedDelimiter => {
                Some("remove this delimiter".to_string())
            },
            Self::UnstartedBracket => Some("add a starting bracket".to_string()),
            Self::YourMom => Some("have dinner".to_string()),
            _ => None,
        }
    }

    fn severeness(&self) -> Severeness {
        match self {
            Self::YourMom => Severeness::Warning,
            _ => Severeness::Error,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LexerError;

impl CompilerError for LexerError {
    fn message(&self) -> String { "lexer error".to_string() }
    fn consider(&self) -> Option<String> { None }
    fn severeness(&self) -> Severeness { Severeness::Error }
}

/*
#[derive(Debug, Clone)]
pub enum TypeCheckError {
    UnresolvedType,
    TypeMismatch { expected: Type, found: Type },
    GlobalNode,
    UnknownIdent,
    FnArgCountNotMatch { expected: usize, found: usize },
}

impl std::fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnresolvedType => write!(f, "unable to resolve type for expression"),
            Self::TypeMismatch {
                expected, found
            } => write!(f, "mismatched types (expecting `{expected}`, found `{found}`)"),
            Self::GlobalNode => write!(f, "unsupported statement in global scope"),
            Self::UnknownIdent => write!(f, "unknown identifier"),
            Self::FnArgCountNotMatch {
                expected, found
            } => write!(f, "function call argument count does not match definition argument count (expecting {expected}, found {found})"),
        }
    }
}

impl CompilerError for TypeCheckError {
    fn consider(&self, _ctx: &ErrorContext<'_>, _span: Span) -> Option<String> {
        match self {
            Self::UnresolvedType => Some("specify the type of the variable".to_string()),
            _ => None,
        }
    }
}

*/
