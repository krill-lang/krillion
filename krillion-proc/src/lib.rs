use proc_macro::TokenStream;

mod parser_fn;

#[proc_macro_attribute]
pub fn parser_fn(args: TokenStream, item: TokenStream) -> TokenStream {
    parser_fn::parse(args, item)
}
