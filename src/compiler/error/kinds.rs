use super::*;

pub fn default_markers(span: Span) -> Vec<Marker> {
    vec![Marker {
        message: "".to_string(),
        span,
        style: MarkerStyle::Primary,
    }]
}

pub trait CompilerError {
    fn message(&self) -> String;
    fn consider(&self) -> Option<String>;
    fn severeness(&self) -> Severeness;

    fn markers(&self, span: Span) -> Vec<Marker> { default_markers(span) }
}

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken {
        expected: Option<&'static str>,
        found: Token,
    },
    UnexpectedVisibility,
    UnexpectedLinkage,
    UnendedBracket,
    UnexpectedDelimiter,
    UnendedScope,
    RanOutTokens,

    YourMom,

    OnlyWorkInRoot,
}

impl CompilerError for ParseError {
    fn message(&self) -> String {
        match self {
            Self::UnexpectedToken {
                expected: Some(expected),
                found,
            } => format!("expected {expected}, found {found}"),
            Self::UnexpectedToken {
                expected: None,
                found,
            } => format!("unexpected {found}"),
            Self::UnexpectedVisibility => "unexpected visibility qualifier".to_string(),
            Self::UnexpectedLinkage => "unexpected linkage specifier".to_string(),
            Self::UnendedBracket => "starting bracket have no matching ending bracket".to_string(),
            Self::UnendedScope => "scope is not ended".to_string(),
            Self::UnexpectedDelimiter => "unexpected delimiter".to_string(),
            Self::RanOutTokens => "ran out of tokens".to_string(),

            Self::YourMom => "your mom is waiting you for dinner".to_string(),

            Self::OnlyWorkInRoot => "this can only be used in module root".to_string(),
        }
    }

    fn consider(&self) -> Option<String> {
        match self {
            Self::UnexpectedVisibility | Self::UnexpectedLinkage => {
                Some("remove this token".to_string())
            },
            Self::UnendedBracket => Some("add a ending bracket".to_string()),
            Self::UnendedScope => Some("add a delimiter `}`".to_string()),
            Self::UnexpectedDelimiter => Some("remove this delimiter".to_string()),
            Self::YourMom => Some("have dinner".to_string()),
            _ => None,
        }
    }

    fn severeness(&self) -> Severeness {
        match self {
            Self::OnlyWorkInRoot => Severeness::Info,
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

#[derive(Debug, Clone)]
pub enum NumerateError {
    NameUndefined,
}

impl CompilerError for NumerateError {
    fn message(&self) -> String {
        match self {
            Self::NameUndefined => "cannot find this name in the current scope".to_string(),
        }
    }

    fn consider(&self) -> Option<String> { None }

    fn severeness(&self) -> Severeness { Severeness::Error }
}

#[derive(Debug, Clone)]
pub enum TypeCheckError {
    UnresolvedType,
    TypeMismatch { expected: String, found: String, because: Span },
    ExpectedLvalue,
    CyclicType,
    UnexpectedReturn,
}

impl CompilerError for TypeCheckError {
    fn message(&self) -> String {
        match self {
            Self::UnresolvedType => "unable to resolve type for expression".to_string(),
            Self::TypeMismatch { .. } => "mismatched types".to_string(),
            Self::ExpectedLvalue => "expected lvalue".to_string(),
            Self::CyclicType => "cyclic type".to_string(),
            Self::UnexpectedReturn => "unexpected return".to_string(),
        }
    }

    fn consider(&self) -> Option<String> {
        /* match self {
            Self::UnresolvedType => Some("specify the type of the variable".to_string()),
            _ => None,
        } */
        None
    }

    fn severeness(&self) -> Severeness {
        Severeness::Error
    }

    fn markers(&self, span: Span) -> Vec<Marker> {
        match self {
            Self::TypeMismatch { expected, found, because } => vec![Marker {
                message: format!("expected `{expected}` because of this"),
                span: because.clone(),
                style: MarkerStyle::Secondary,
            }, Marker {
                message: format!("but found `{found}` here"),
                span,
                style: MarkerStyle::Primary,
            }],
            _ => default_markers(span),
        }
    }
}
