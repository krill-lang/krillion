pub use clap::Parser;
use clap::*;

#[derive(Clone, Debug, Default, Parser)]
pub struct Args {
    #[arg(help = "The file to be compiled")]
    pub input: String,

    #[arg(short, long, help = "Output file's filename", default_value = "a.out")]
    pub output: String,

    #[arg(long, help = "The style for error reporting", value_enum, default_value_t = ErrorStyle::Normal)]
    pub error_style: ErrorStyle,

    #[arg(long, help = "Change error message colors for colorblind people", action = ArgAction::SetTrue)]
    pub alt_color: bool,

    #[cfg(debug_assertions)]
    #[arg(long, help = "Panic on startup")]
    pub panics: Option<String>,
}

#[derive(Clone, Debug, Default, ValueEnum)]
pub enum ErrorStyle {
    #[default]
    Normal,
    NoHighlight,
}
