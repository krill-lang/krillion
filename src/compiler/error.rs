use super::*;

#[derive(Debug, Clone)]
pub struct ErrorContext<'a> {
    pub filename: &'a str,
    pub source: &'a str,
    pub cat: Vec<usize>
}

pub type ACompileError = (Box<dyn CompilerError>, Span);
pub trait CompilerError {
    fn report(&self, ctx: &ErrorContext<'_>, span: Span) -> String;
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

pub trait ErrorTips {
    fn consider(&self, ctx: &ErrorContext<'_>, span: Span) -> Option<String>;
}

impl<T> CompilerError for T where T: std::fmt::Display + ErrorTips {
    fn report(&self, ctx: &ErrorContext<'_>, span: Span) -> String {
        let start = ctx.source[..span.start].matches('\n').count()+1;
        let end = ctx.source[..span.end].matches('\n').count()+1;
        let lines = end - start;

        let chw = format!("{end}").len();
        let mut fin = format!(
            "\x1b[1;31mError: {self}\n\x1b[1;34m{} \u{250c}\u{2500}\x1b[0;1m In: \x1b[0m{}\n",
            " ".repeat(chw),
            ctx.filename
        );
        fin += &format!("\x1b[1;34m{} \u{2502}\x1b[0m\n", " ".repeat(chw));

        for (i, el) in ctx.source.lines().enumerate().skip(start-1).take(lines+1) {
            let i = i + 1;
            if end - start >= 6 && start+3 == i {
                fin += &format!(
                    "\x1b[1;33m{}...\n",
                    " ".repeat(chw),
                );
            }
            if end - start >= 6 && (start+3..=end.saturating_sub(3)).contains(&i) {
                continue;
            }

            fin += &format!(
                "\x1b[1;34m{i}{} \u{2502}\x1b[0m ",
                " ".repeat(chw - format!("{i}").len()),
            );

            let mut hl = HighlightToken::lexer(el.trim_end());
            let mut hl = to_atoken_buf(&mut hl).unwrap_or_else(|_| unreachable!());

            let mut tabs = 0;
            while let Some((tok, span)) = hl.next().cloned() {
                let src = &el[span.start..span.end];
                tabs += src.matches('\t').count();
                let src = src.replace('\t', "    ");
                fin += &tok.highlight(hl.peek().map(|(t, _)| t), &src);
            }

            fin += &format!(
                "\n\x1b[1;34m{} \u{2502} \x1b[0;1;33m{}\x1b[0m\n",
                " ".repeat(chw),
                "^".repeat(el.len()+tabs*3-ctx.cat.get(start).unwrap_or(&0).checked_sub(span.end).unwrap_or(0)+1),
            );
        }

        fin += &format!("\x1b[1;34m{} \u{2502}\x1b[0m\n", " ".repeat(chw));
        if let Some(c) = self.consider(ctx, span) {
            fin += &format!("\x1b[1;34m{} \u{2514}\u{2500} \x1b[0;1mConsider:\x1b[0m {c}\n", " ".repeat(chw));
        }

        fin + "\n"
    }
}

#[derive(Debug, Clone)]
#[allow(clippy::enum_variant_names)]
pub enum ParseError {
    UnexpectedToken,
    UnstartedBracket,
    UnendedBracket,
    UnendedFnCall,
    UnendedScope,
    BracketNotMatch,
    ExprParseError,
    RanOutOperands,
    RanOutTokens,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken => write!(f, "unexpected token"),
            Self::UnstartedBracket => write!(f, "ending bracket have no matching starting bracket"),
            Self::UnendedBracket => write!(f, "starting bracket have no matching ending bracket"),
            Self::UnendedFnCall  => write!(f, "function call have no ending bracket"),
            Self::UnendedScope => write!(f, "scope is not ended"),
            Self::BracketNotMatch => write!(f, "starting bracket does not match ending bracket"),
            Self::ExprParseError => write!(f, "unknown expression parsing error"),
            Self::RanOutOperands => write!(f, "ran out of operands while parsing expression"),
            Self::RanOutTokens => write!(f, "ran out of tokens"),
        }
    }
}

impl ErrorTips for ParseError {
    fn consider(&self, _ctx: &ErrorContext<'_>, _span: Span) -> Option<String> {
        match self {
            Self::UnexpectedToken
                => Some("add a semicolon to end the current statement".to_string()),
            Self::UnendedFnCall | Self::UnendedBracket | Self::UnendedScope
                => Some("add a ending bracket".to_string()),
            Self::UnstartedBracket
                => Some("add a starting bracket".to_string()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct LexerError();

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "lexer error")
    }
}

impl ErrorTips for LexerError {
    fn consider(&self, _ctx: &ErrorContext<'_>, _span: Span) -> Option<String> {
        None
    }
}

