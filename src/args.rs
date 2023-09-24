pub use clap::Parser;

#[derive(Clone, Debug, Parser)]
pub struct Args {
    #[arg(help = "The file to be compiled")]
    pub input: String,

    #[arg(short, long, help = "Output file's filename", default_value = "a.out")]
    pub output: String,
}
