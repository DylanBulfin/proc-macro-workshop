use core::slice::SlicePattern;
use std::iter::Map;

use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    braced,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    token::Brace,
    AngleBracketedGenericArguments, Attribute, Data, DataStruct, DeriveInput, Fields,
    GenericArgument, Ident, Path, PathArguments, PathSegment, Token, Type, TypePath, Visibility,
};

struct StructItem {
    //_attr: Vec<Attribute>,
    vis: Visibility,
    //_struct: Token![struct],
    ident: Ident,
    //_brace: Brace,
    fields: Punctuated<StructField, Token![,]>,
}

struct StructField {
    attr: Vec<Attribute>,
    //_vis: Visibility,
    ident: Ident,
    //_colon: Token![:],

    // This marks whether or not the base field was optional
    // If so we handle it specially
    opt: bool,
    // For base types that are not options, this is the base type. For base types that are
    // options we store the inside of the Option instead. E.g. for type `String` it is
    // String, and for type `Option<Vec<String>>` it is `Vec<String>`
    ty: Type,
}

impl Parse for StructItem {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let _attr = input.call(Attribute::parse_outer)?;
        let vis = input.parse()?;
        let _struct: Token![struct] = input.parse()?;
        let ident = input.parse()?;

        let content;
        let _brace: Brace = braced!(content in input);
        let fields = content.parse_terminated(Parse::parse, Token![,])?;

        Ok(Self { vis, ident, fields })
    }
}

impl Parse for StructField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attr = input.call(Attribute::parse_outer)?;
        let _vis: Visibility = input.parse()?;
        let ident = input.parse()?;
        let _colon: Token![:] = input.parse()?;
        let ty = input.parse()?;

        if let Some(inner_ty) = check_field_optional(&ty) {
            Ok(Self {
                attr,
                ident,
                ty: inner_ty,
                opt: true,
            })
        } else {
            Ok(Self {
                attr,
                ident,
                ty,
                opt: false,
            })
        }
    }
}

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(input as StructItem);

    let ident = item.ident;
    let builder_ident = format_ident!("{}Builder", ident);
    // Consider if this is necessary
    let field_vec = item.fields.into_iter().collect::<Vec<_>>();

    let builder_def = builder_definition(&builder_ident, &item.vis, field_vec.as_slice());
    let builder_impl = builder_impl(&ident, &builder_ident, field_vec.as_slice());

    quote! {
        #builder_impl

        #builder_def
    }
    .into()
}

fn base_impl<'a, I>(ident: &Ident, builder_ident: &Ident, fields: I) -> TokenStream
where
    I: IntoIterator<Item = &'a StructField>,
{
}

fn builder_impl<'a, I>(ident: &Ident, builder_ident: &Ident, fields: I) -> TokenStream
where
    I: IntoIterator<Item = &'a StructField>,
{
    let mut setters: Vec<_> = Vec::new();
    let mut field_inits: Vec<_> = Vec::new();

    for f in fields {
        let ty = &f.ty;
        let id = &f.ident;

        setters.push(quote! {
            pub fn #id(&mut self, #id: #ty) -> &mut Self {
                self.#id = Some(#ty);
            }
        });

        field_inits.push(quote! {
            #ty: None
        });
    }

    quote! {
        impl #builder_ident {
            #(
                #setters
            )*

            pub fn build(&mut self) -> Result<#ident, Box<dyn Error>> {
                #(
                    #field_inits
                ),*
            }
        }
    }
}

fn builder_definition<'a, I>(builder_ident: &Ident, vis: &Visibility, fields: I) -> TokenStream
where
    I: IntoIterator<Item = &'a StructField>,
{
    let builder_fields = fields.into_iter().map(|f| {
        let id = &f.ident;
        let ty = &f.ty;

        let span = id.span();

        quote_spanned! {span=>
            #id: Option<#ty>
        }
    });

    quote! {
        #vis struct #builder_ident {
            #(
                #builder_fields
            ),*
        }
    }
}

//     Type::Path(
//         TypePath {
//             qself: None,
//             path: Path {
//                 segments: [
//                     PathSegment {
//                         ident: "Option",
//                         arguments: PathArguments::AngleBracketed(
//                             AngleBracketedGenericArguments {
//                                 args: [
//                                     GenericArgument::Type(
//                                         ...
//                                     ),
//                                 ],
//                             },
//                         ),
//                     },
//                 ],
//             },
//         },
//     )
fn check_field_optional(ty: &Type) -> Option<Type> {
    if let Type::Path(TypePath {
        qself: None,
        path: Path { segments, .. },
        ..
    }) = ty
    {
        if segments.len() == 1 && segments[0].ident == "Option" {
            if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
                &segments[0].arguments
            {
                if args.len() == 1 {
                    if let GenericArgument::Type(t) = &args[0] {
                        return Some(t.clone());
                    }
                }
            }
        }
    }

    None
}
