use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, Data, DataStruct,
    DeriveInput, Fields, GenericArgument, Ident, Path, PathArguments, PathSegment, Type, TypePath,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let tree = parse_macro_input!(input as DeriveInput);

    let ident = &tree.ident;
    let builder_ident = format_ident!("{}Builder", ident);

    let mut arg_id = Vec::new();
    let mut arg_ty = Vec::new();
    let mut arg_op = Vec::new();

    match tree.data {
        Data::Struct(data) => match data.fields {
            Fields::Named(fields) => {
                for f in fields.named {
                    arg_id.push(f.ident);

                    let (ty, opt) = try_strip_arg(f.ty);
                    arg_ty.push(ty);
                    arg_op.push(opt);
                }
            }
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    };

    let required_checks = arg_id.iter().zip(arg_op.iter()).map(|(id, op)| {
        let span = id.span();

        if !op {
            quote_spanned! {span=>
                if self.#id.is_none() {
                    return Err(format!("Field {} is None", stringify!(#id)).into());
                }
            }
        } else {
            quote! {}
        }
    });

    let constructor_fields = arg_id.iter().zip(arg_op.iter()).map(|(id, op)| {
        let span = id.span();

        if *op {
            quote_spanned! {span=> #id: self.#id.take()}
        } else {
            quote_spanned! {span=>
                #id: self.#id.take().unwrap()
            }
        }
    });

    let res: proc_macro::TokenStream = quote! {
        pub struct #builder_ident {
            #(
                #arg_id: Option<#arg_ty>
            ),*
        }

        impl #builder_ident {
            #(
                pub fn #arg_id(&mut self, #arg_id: #arg_ty) -> &mut Self {
                    self.#arg_id = Some(#arg_id);
                    self
                }
            )*

            pub fn build(&mut self) -> Result<#ident, Box<dyn std::error::Error>> {
                #(#required_checks)*

                Ok(
                    #ident {
                        #(
                            #constructor_fields
                        ),*
                    }
                )
            }
        }

        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(
                        #arg_id: None
                    ),*
                }
            }
        }
    }
    .into();

    res
    //panic!("{}", res.to_string());
}

fn try_strip_arg(arg_ty: Type) -> (Type, bool) {
    'outer: {
        if let Type::Path(TypePath {
            qself: None,
            path: Path { ref segments, .. },
            ..
        }) = arg_ty
        {
            if segments.len() == 1 && segments[0].ident == "Option" {
                if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    ref args,
                    ..
                }) = segments[0].arguments
                {
                    if args.len() == 1 {
                        if let GenericArgument::Type(ref t) = args[0] {
                            break 'outer (t.to_owned(), true);
                        }
                    }
                }
            }
        }

        break 'outer (arg_ty, false);
    }
}
