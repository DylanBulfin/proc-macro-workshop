use std::iter::Map;

use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{
    braced,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    token::Brace,
    AngleBracketedGenericArguments, Attribute, Data, DataStruct, DeriveInput, Expr, Fields,
    GenericArgument, Ident, LitStr, Path, PathArguments, PathSegment, Token, Type, TypePath,
    Visibility,
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

    // If this struct contains a
    each_attr: Option<EachAttr>,
}

struct EachAttr(LitStr);

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
        let attrs = input.call(Attribute::parse_outer)?;
        let _vis: Visibility = input.parse()?;
        let ident = input.parse()?;
        let _colon: Token![:] = input.parse()?;
        let ty = input.parse()?;

        let each_attr = if let Some(a) = attrs.into_iter().find(|a| a.path().is_ident("builder")) {
            let res: EachAttr = a.parse_args()?;
            Some(res)
        } else {
            None
        };

        if let Some(inner_ty) = check_field_optional(&ty) {
            Ok(Self {
                ident,
                ty: inner_ty,
                opt: true,
                each_attr,
            })
        } else {
            Ok(Self {
                ident,
                ty,
                opt: false,
                each_attr,
            })
        }
    }
}

impl Parse for EachAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lhs: Ident = input.parse()?;
        let _equal: Token![=] = input.parse()?;
        let lit: LitStr = input.parse()?;

        if lhs == "each" {
            Ok(Self(lit))
        } else {
            Err(syn::Error::new(
                lhs.span(),
                format!("Expected attr 'each', found {}", lhs),
            ))
        }
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(input as StructItem);

    let ident = item.ident;
    let builder_ident = format_ident!("{}Builder", ident);
    // Consider if this is necessary
    let field_vec = item.fields.into_iter().collect::<Vec<_>>();

    let builder_def = builder_definition(&builder_ident, &item.vis, field_vec.as_slice());
    let builder_impl = builder_impl(&ident, &builder_ident, field_vec.as_slice());
    let base_impl = base_impl(&ident, &builder_ident, field_vec.as_slice());

    quote! {
        #base_impl

        #builder_impl

        #builder_def
    }
    .into()
}

fn base_impl<'a, I>(ident: &Ident, builder_ident: &Ident, fields: I) -> TokenStream
where
    I: IntoIterator<Item = &'a StructField>,
{
    let builder_fields = fields.into_iter().map(|field| {
        let id = &field.ident;

        if field.each_attr.is_some() {
            quote! {
                #id: std::option::Option::Some(std::vec::Vec::new())
            }
        } else {
            quote! {
                #id: std::option::Option::None
            }
        }
    });

    quote! {
        impl #ident {
            fn builder() -> #builder_ident {
                #builder_ident {
                    #(
                        #builder_fields
                    ),*
                }
            }
        }
    }
}

fn builder_impl<'a, I>(ident: &Ident, builder_ident: &Ident, fields: I) -> TokenStream
where
    I: IntoIterator<Item = &'a StructField>,
{
    let mut setters: Vec<_> = Vec::new();
    let mut field_checks: Vec<_> = Vec::new();
    let mut field_inits: Vec<_> = Vec::new();
    let mut each_funcs: Vec<_> = Vec::new();

    for field in fields {
        let ty = &field.ty;
        let id = &field.ident;
        let opt = field.opt;

        if let Some(EachAttr(val)) = &field.each_attr {
            let each_id = Ident::new(val.value().as_str(), ty.span());
            let inner_ty = get_vec_inner_type(ty);

            if &each_id != id {
                setters.push(quote! {
                    pub fn #id(&mut self, #id: #ty) -> &mut Self {
                        self.#id = std::option::Option::Some(#id);
                        self
                    }
                });
            }

            each_funcs.push(quote! {
                pub fn #each_id(&mut self, #each_id: #inner_ty) -> &mut Self {
                    match self.#id {
                        std::option::Option::Some(ref mut v) => v.push(#each_id),
                        None => {
                            let mut new_val = std::vec::Vec::new();
                            new_val.push(#each_id);

                            self.#id = std::option::Option::Some(new_val);
                        }
                    }

                    self
                }
            });
        } else {
            setters.push(quote! {
                pub fn #id(&mut self, #id: #ty) -> &mut Self {
                    self.#id = std::option::Option::Some(#id);
                    self
                }
            });
        }

        if !opt {
            field_checks.push(quote! {
                if self.#id.is_none() {
                    return std::result::Result::Err(format!("Field {} is unexpectedly None", stringify!(#id)).into());
                }
            })
        }

        if opt {
            field_inits.push(quote! {
                #id: self.#id.clone()
            })
        } else {
            field_inits.push(quote! {
                #id: self.#id.clone().unwrap()
            });
        }
    }

    quote! {
        impl #builder_ident {
            #(
                #setters
            )*

            #(
                #each_funcs
            )*

            pub fn build(&mut self) -> std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> {
                #(
                    #field_checks
                )*

                Ok(
                    #ident {
                        #(
                            #field_inits
                        ),*
                    }
                )
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
            #id: std::option::Option<#ty>
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

fn get_vec_inner_type(ty: &Type) -> &Type {
    if let Type::Path(TypePath {
        qself: None,
        path: Path { segments, .. },
        ..
    }) = ty
    {
        if segments.len() == 1 && segments[0].ident == "Vec" {
            if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
                &segments[0].arguments
            {
                if args.len() == 1 {
                    if let GenericArgument::Type(t) = &args[0] {
                        return t;
                    }
                }
            }
        }
    }

    panic!("Can't use each attribute on non-Vec fields")
}

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
