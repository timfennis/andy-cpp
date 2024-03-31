use crate::r#match::{
    is_owned_string, is_ref_mut, is_ref_of_bigint, is_ref_of_slice_of_value, is_str_ref,
    path_ends_with,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub struct WrappedFunction {
    pub function_declaration: TokenStream,
    pub function_registration: TokenStream,
}

pub fn wrap_function(function: syn::ItemFn) -> WrappedFunction {
    let identifier = function.sig.ident.clone();

    let mut register_as_function_name = proc_macro2::Literal::string(&identifier.to_string());

    for attr in &function.attrs {
        if attr.path().is_ident("function") {
            attr.parse_nested_meta(|meta| {
                // #[function(name = "...")]
                if meta.path.is_ident("name") {
                    register_as_function_name = meta.value()?.parse()?;
                    Ok(())
                } else {
                    Err(meta.error("unsupported property on function"))
                }
            })
            .expect("invalid function attribute");
        }
    }

    match &function.vis {
        syn::Visibility::Public(_) => {}
        syn::Visibility::Restricted(_) => panic!("only public functions can be wrapped for now"),
        syn::Visibility::Inherited => panic!("only public functions can be wrapped for now"),
    }

    let inner_ident = format_ident!("{}_inner", identifier);
    let inner = {
        let mut inner = function.clone();
        // Remove attributes to prevent compilation errors
        inner.attrs.clear();

        // Change the name
        inner.sig.ident = inner_ident.clone();
        inner
    };

    let mut temp_var_init = Vec::new();
    let mut temp_vars = Vec::new();
    let mut param_types: Vec<TokenStream> = Vec::new();

    for (position, fn_arg) in function.sig.inputs.iter().enumerate() {
        if let Some(x) = create_temp_variable(position, fn_arg, &identifier) {
            temp_var_init.push(x.initialize_code);
            temp_vars.push(x.temp_var);
            param_types.push(x.param_type);
        }
    }

    let function_declaration = quote! {
        pub fn #identifier (
            values: &[crate::interpreter::value::Value],
            environment: &crate::interpreter::environment::EnvironmentRef
        ) -> crate::interpreter::evaluate::EvaluationResult {
            #inner

            #(#temp_var_init; )*

            let result = #inner_ident (#(#temp_vars, )*);
            let result = crate::interpreter::value::Value::from(result);
            return Ok(result);
        }
    };

    let function_registration = quote! {
        env.declare_function(#register_as_function_name, crate::interpreter::function::Function::GenericFunction {
            function: #identifier,
            type_signature: crate::interpreter::function::TypeSignature::Exact(vec![
                #(#param_types, )*
            ]),
        });
    };

    WrappedFunction {
        function_declaration,
        function_registration,
    }
}

struct TempVar {
    param_type: TokenStream,
    temp_var: TokenStream,
    initialize_code: TokenStream,
}

// TODO this style of path matching only works if types are just identifiers which is a weakness I'd
//      like to fix
fn into_param_type(ty: &syn::Type) -> TokenStream {
    if path_ends_with(ty, "Vec") {
        return quote! { crate::interpreter::function::ParamType::List };
    }

    match ty {
        syn::Type::Reference(syn::TypeReference { elem, .. }) => into_param_type(elem),
        syn::Type::Path(syn::TypePath { path, .. }) => match path {
            _ if path.is_ident("i64") => quote! { create::interpreter::function::ParamType::Int },
            _ if path.is_ident("f64") => quote! { create::interpreter::function::ParamType::Float },
            _ if path.is_ident("Value") => {
                quote! { crate::interpreter::function::ParamType::Any }
            }
            _ if path.is_ident("Number") => {
                quote! { crate::interpreter::function::ParamType::Number }
            }
            _ if path.is_ident("Sequence") => {
                quote! { crate::interpreter::function::ParamType::Sequence }
            }
            _ => panic!("Don't know how to convert PATH into ParamType\n\n{path:?}"),
        },
        x => panic!("Don't know how to convert {x:?} into ParamType"),
    }
}

fn create_temp_variable(
    position: usize,
    input: &syn::FnArg,
    identifier: &syn::Ident,
) -> Option<TempVar> {
    let temp_var = syn::Ident::new(&format!("temp_{position}"), identifier.span());
    if let syn::FnArg::Typed(pat_type) = input {
        let ty = &*pat_type.ty;
        // Special case for &str because we need to create a temporary binding for the borrow

        // The pattern is exactly &mut String
        if is_ref_mut(ty) && is_owned_string(ty) {
            let rc_temp_var = syn::Ident::new(&format!("temp_{temp_var}"), identifier.span());
            return Some(TempVar {
                param_type: quote! { crate::interpreter::function::ParamType::String },
                temp_var: quote! { #temp_var },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::value::Sequence::String(#rc_temp_var)) = &values[#position] else {
                        panic!("Value #position needed to be a Sequence::String but wasn't");
                    };
                    let #temp_var = &mut *#rc_temp_var.borrow_mut();
                    // let #temp_var = #rc_temp_var.as_ref();
                },
            });
        }
        // The pattern is exactly &str
        else if is_str_ref(ty) {
            let rc_temp_var = syn::Ident::new(&format!("temp_{temp_var}"), identifier.span());
            return Some(TempVar {
                param_type: quote! { crate::interpreter::function::ParamType::String },
                temp_var: quote! { #temp_var },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::value::Sequence::String(#rc_temp_var)) = &values[#position] else {
                        panic!("Value #position needed to be a Sequence::String but wasn't");
                    };
                    let #rc_temp_var = #rc_temp_var.borrow();
                    let #temp_var = #rc_temp_var.as_ref();
                },
            });
        }
        // The pattern is &BigInt
        else if is_ref_of_bigint(ty) {
            let big_int = syn::Ident::new(&format!("temp_{temp_var}"), identifier.span());
            return Some(TempVar {
                param_type: quote! { crate::interpreter::function::ParamType::Int },
                temp_var: quote! { #temp_var },
                initialize_code: quote! {
                    let #big_int = if let crate::interpreter::value::Value::Number(crate::interpreter::num::Number::Int(crate::interpreter::int::Int::Int64(smol))) = &values[#position] {
                        Some(num::BigInt::from(*smol))
                    } else {
                        None
                    };

                    let #temp_var = match &values[#position] {
                        crate::interpreter::value::Value::Number(crate::interpreter::num::Number::Int(crate::interpreter::int::Int::BigInt(big))) => big,
                        crate::interpreter::value::Value::Number(crate::interpreter::num::Number::Int(crate::interpreter::int::Int::Int64(smoll))) => #big_int.as_ref().unwrap(),
                        _ => panic!("Value #position need to be an Int but wasn't"),
                    }
                },
            });
        }
        // The pattern is &[Value]
        else if is_ref_of_slice_of_value(ty) {
            let rc_temp_var = syn::Ident::new(&format!("temp_{temp_var}"), identifier.span());
            return Some(TempVar {
                param_type: quote! { crate::interpreter::function::ParamType::List },
                temp_var: quote! { #temp_var },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::value::Sequence::List(#rc_temp_var)) = &values[#position] else {
                        panic!("Value #position needed to be a Sequence::List but wasn't");
                    };
                    let #temp_var = &*#rc_temp_var.borrow();
                },
            });
        }
        // The pattern is something like `i64`
        else if let syn::Type::Path(path) = ty {
            return Some(TempVar {
                param_type: into_param_type(ty),
                temp_var: quote! { #temp_var },
                initialize_code: quote! {
                    let #temp_var = #path :: try_from(&values[#position]).map_err(|err| crate::interpreter::function::FunctionCarrier::ArgumentError(format!("{err}")))?
                },
            });
        }
        // The pattern is something like '&Number'
        else if let syn::Type::Reference(type_ref) = &*pat_type.ty {
            return Some(TempVar {
                param_type: into_param_type(ty),
                temp_var: quote! { #temp_var },
                initialize_code: quote! {
                    let #temp_var = <#type_ref as TryFrom<&crate::interpreter::value::Value>> :: try_from(&values[#position]).map_err(|err| crate::interpreter::function::FunctionCarrier::ArgumentError(format!("{err}")))?
                },
            });
        } else {
            panic!("not sure how to handle this type of thing");
        }
    }

    panic!("Not sure how to handle receivers");
}
