use proc_macro::TokenStream;
use quote::*;
use syn::*;

pub fn parse_node(_args: TokenStream, item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item as ItemFn);

    let ident = &ast.sig.ident;
    let body = &ast.block;

    quote! {
        pub(crate) fn #ident(
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

pub fn parse_type(_args: TokenStream, item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item as ItemFn);

    let ident = &ast.sig.ident;
    let body = &ast.block;

    quote! {
        pub(crate) fn #ident(
            buf: &mut Buffer<AToken>,
            src: &str,
            errs: &mut Errors,
        ) -> Option<AType> #body
    }.into()
}
