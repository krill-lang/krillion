use proc_macro::TokenStream;
use quote::*;
use syn::*;

pub fn parse(_args: TokenStream, item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item as ItemFn);

    let ident = &ast.sig.ident;
    let body = &ast.block;

    quote! {
        fn #ident(
            buf: &mut Buffer<AToken>,
            src: &str,
            ast: &mut UntypedAst,
            vis: Option<(Visibility, Span)>,
            link: Option<(Linkage, Span)>,
            extra: NodeExtra,
            errs: &mut Errors,
            should_end: ShouldEndFn<'_>,
            depth: usize,
        ) -> bool #body
    }.into()
}
