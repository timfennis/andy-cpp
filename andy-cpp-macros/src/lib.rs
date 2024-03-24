extern crate proc_macro;
extern crate proc_macro2;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;

use syn::parse_macro_input;

#[proc_macro_attribute]
pub fn andycpp_function(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as syn::ItemFn);
    let identifier = input.sig.ident.clone();

    let mut inner = input.clone();
    let inner_ident = Ident::new(&format!("{}_inner", identifier), identifier.span());
    inner.sig.ident = inner_ident.clone();

    let params = input
        .sig
        .inputs
        .iter()
        .enumerate()
        .filter_map(|(position, input)| {
            if let syn::FnArg::Typed(pat_type) = input {
                if let syn::Type::Path(path) = &*pat_type.ty {
                    return Some(quote! {
                        #path :: try_from(&values[#position]).map_err(|err| crate::interpreter::function::FunctionCarrier::ArgumentError(format!("{err}")))?
                    });
                } else if let syn::Type::Reference(type_ref) = &*pat_type.ty {
                    return Some(quote! {
                        <#type_ref as TryFrom<&Value>> :: try_from(&values[#position]).map_err(|err| crate::interpreter::function::FunctionCarrier::ArgumentError(format!("{err}")))?
                    })
                }
            }

            None
        })
        .collect::<Vec<_>>();

    let tokens = quote! {
        fn #identifier (
            values: &[crate::interpreter::value::Value],
            environment: &crate::interpreter::environment::EnvironmentRef
        ) -> crate::interpreter::evaluate::EvaluationResult {
            #inner

            let result = #inner_ident (#(#params, )*);
            let result = crate::interpreter::value::Value::from(result);
            return Ok(result);
            // Ok(result?)
            // Ok(crate::interpreter::value::Value::Unit)
        }

    };

    tokens.into()
}
