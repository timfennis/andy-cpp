mod r#match;

extern crate proc_macro;
extern crate proc_macro2;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;

use crate::r#match::*;
use syn::parse_macro_input;

#[proc_macro_attribute]
pub fn export_function(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as syn::ItemFn);
    let identifier = input.sig.ident.clone();

    let mut inner = input.clone();
    let inner_ident = Ident::new(&format!("{}_inner", identifier), identifier.span());
    inner.sig.ident = inner_ident.clone();

    let temp_var_assignments = input
        .sig
        .inputs
        .iter()
        .enumerate()
        .filter_map(|(position, input)| {
            let temp_var = Ident::new(&format!("temp_{position}"), identifier.span());
            if let syn::FnArg::Typed(pat_type) = input {

                let ty = &*pat_type.ty;
                // Special case for &str because we need to create a temporary binding for the borrow
                if is_str_ref(ty) {
                    let rc_temp_var = Ident::new(&format!("temp_{temp_var}"), identifier.span());
                    return Some(quote! {
                        let crate::interpreter::value::Value::Sequence(crate::interpreter::value::Sequence::String(#rc_temp_var)) = &values[#position] else {
                            panic!("Value #position needed to be a Sequence::String but wasn't");
                        };
                        let #rc_temp_var = #rc_temp_var.borrow();
                        let #temp_var = #rc_temp_var.as_ref();
                    });
                }
                // The pattern is something like `i64`
                else if let syn::Type::Path(path) = ty {
                    return Some(quote! {
                        let #temp_var = #path :: try_from(&values[#position]).map_err(|err| crate::interpreter::function::FunctionCarrier::ArgumentError(format!("{err}")))?
                    });
                }
                // The pattern is something like '&Number'
                else if let syn::Type::Reference(type_ref) = &*pat_type.ty {
                    return Some(quote! {
                        let #temp_var = <#type_ref as TryFrom<&crate::interpreter::value::Value>> :: try_from(&values[#position]).map_err(|err| crate::interpreter::function::FunctionCarrier::ArgumentError(format!("{err}")))?
                    })
                } else {
                    panic!("not sure how to handle this type of thing");
                }
            }

            None
        })
        .collect::<Vec<_>>();

    let temp_vars = input
        .sig
        .inputs
        .iter()
        .enumerate()
        .map(|(position, _input)| {
            let temp_var = Ident::new(&format!("temp_{position}"), identifier.span());
            quote! {#temp_var}
        });

    let tokens = quote! {
        fn #identifier (
            values: &[crate::interpreter::value::Value],
            environment: &crate::interpreter::environment::EnvironmentRef
        ) -> crate::interpreter::evaluate::EvaluationResult {
            #inner

            #(#temp_var_assignments; )*

            let result = #inner_ident (#(#temp_vars, )*);
            let result = crate::interpreter::value::Value::from(result);
            return Ok(result);
        }

    };

    tokens.into()
}
