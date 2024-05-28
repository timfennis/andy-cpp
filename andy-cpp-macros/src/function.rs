use crate::r#match::{
    is_ref, is_ref_mut, is_ref_of_bigint, is_ref_of_slice_of_value, is_str_ref, is_string,
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

    let mut argument_init_code_blocks = Vec::new();
    let mut arguments = Vec::new();
    let mut param_types: Vec<TokenStream> = Vec::new();

    for (position, fn_arg) in function.sig.inputs.iter().enumerate() {
        if let Some(x) = create_temp_variable(position, fn_arg, &identifier) {
            argument_init_code_blocks.push(x.initialize_code);
            arguments.push(x.argument);
            param_types.push(x.param_type);
        }
    }

    let return_expr = match function.sig.output {
        syn::ReturnType::Default => quote! {
            return Ok(crate::interpreter::value::Value::Unit);
        },
        syn::ReturnType::Type(_, typ) => match &*typ {
            // If the function returns a result we unpack it using the question mark operator
            ty @ syn::Type::Path(_) if path_ends_with(ty, "Result") => quote! {
                let value = result.map_err(|err| crate::interpreter::function::FunctionCarrier::IntoEvaluationError(Box::new(err)))?;
                return Ok(Value::from(value));
            },
            _ => quote! {
                let result = crate::interpreter::value::Value::from(result);
                return Ok(result);
            },
        },
    };

    // This generates a function declaration from a rust function
    // The expansion looks something like this
    //
    // fn wrapper_function(values: &[Value]) -> EvaluationResult {
    //     fn original_function(....) { .... }
    //     let arg0 = ....; // from values[0]
    //     let arg1 = ....; // from values[1]
    //
    //     return original_function(arg0, arg1);
    // }
    let function_declaration = quote! {
        pub fn #identifier (
            values: &[crate::interpreter::value::Value],
            environment: &crate::interpreter::environment::EnvironmentRef
        ) -> crate::interpreter::evaluate::EvaluationResult {
            // Define the inner function that has the rust type signature
            #inner

            // Initialize the arguments and map them from the Andy C types to the rust types
            #(#argument_init_code_blocks; )*

            // Call the inner function with the unpacked arguments
            let result = #inner_ident (#(#arguments, )*);

            // Return the result (Possibly by unpacking errors)
            #return_expr
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

struct Argument {
    param_type: TokenStream,
    argument: TokenStream,
    initialize_code: TokenStream,
}

fn into_param_type(ty: &syn::Type) -> TokenStream {
    if path_ends_with(ty, "Vec") {
        return quote! { crate::interpreter::function::ParamType::List };
    }

    if path_ends_with(ty, "DefaultMap")
        || path_ends_with(ty, "DefaultMapMut")
        || path_ends_with(ty, "HashMap")
    {
        return quote! { crate::interpreter::function::ParamType::Map };
    }

    match ty {
        syn::Type::Reference(syn::TypeReference { elem, .. }) => into_param_type(elem),
        syn::Type::Path(syn::TypePath { path, .. }) => match path {
            _ if path.is_ident("i64") => quote! { crate::interpreter::function::ParamType::Int },
            _ if path.is_ident("usize") => quote! { crate::interpreter::function::ParamType::Int },
            _ if path.is_ident("f64") => quote! { crate::interpreter::function::ParamType::Float },
            _ if path.is_ident("bool") => quote! { crate::interpreter::function::ParamType::Bool },
            _ if path.is_ident("Value") => {
                quote! { crate::interpreter::function::ParamType::Any }
            }
            _ if path.is_ident("Number") => {
                quote! { crate::interpreter::function::ParamType::Number }
            }
            _ if path.is_ident("Sequence") => {
                quote! { crate::interpreter::function::ParamType::Sequence }
            }
            _ => panic!("Don't know how to convert Path into ParamType\n\n{path:?}"),
        },
        x => panic!("Don't know how to convert {x:?} into ParamType"),
    }
}

fn create_temp_variable(
    position: usize,
    input: &syn::FnArg,
    identifier: &syn::Ident,
) -> Option<Argument> {
    let argument_var_name = syn::Ident::new(&format!("arg{position}"), identifier.span());
    if let syn::FnArg::Typed(pat_type) = input {
        let ty = &*pat_type.ty;

        // The pattern is exactly &mut String
        if is_ref_mut(ty) && is_string(ty) {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return Some(Argument {
                param_type: quote! { crate::interpreter::function::ParamType::String },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::String(#rc_temp_var)) = &values[#position] else {
                        panic!("Value #position needed to be a Sequence::String but wasn't");
                    };
                    let #argument_var_name = &mut *#rc_temp_var.try_borrow_mut()?;
                },
            });
        }
        // The pattern is &HashMap<Value, Value>
        else if is_ref(ty) && path_ends_with(ty, "HashMap") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return Some(Argument {
                param_type: quote! { crate::interpreter::function::ParamType::Map },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Map(#rc_temp_var, _default)) = &values[#position] else {
                        panic!("Value #position needed to be a Sequence::Map but wasn't");
                    };
                    let #argument_var_name = &*#rc_temp_var.borrow();
                },
            });
        }
        // The pattern is &mut HashMap<Value, Value>
        else if is_ref_mut(ty) && path_ends_with(ty, "HashMap") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return Some(Argument {
                param_type: quote! { crate::interpreter::function::ParamType::Map },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Map(#rc_temp_var, _default)) = &values[#position] else {
                        panic!("Value #position needed to be a Sequence::Map but wasn't");
                    };
                    let #argument_var_name = &mut *#rc_temp_var.try_borrow_mut()?;
                },
            });
        } else if path_ends_with(ty, "DefaultMap") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return Some(Argument {
                param_type: quote! { crate::interpreter::function::ParamType::Map },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Map(#rc_temp_var, default)) = &values[#position] else {
                        panic!("Value #position needed to be a Sequence::Map but wasn't");
                    };
                    let #argument_var_name = (&*#rc_temp_var.borrow(), default.to_owned());
                },
            });
        }
        // The pattern is &mut HashMap<Value, Value>
        else if path_ends_with(ty, "DefaultMapMut") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return Some(Argument {
                param_type: quote! { crate::interpreter::function::ParamType::Map },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Map(#rc_temp_var, default)) = &values[#position] else {
                        panic!("Value #position needed to be a Sequence::Map but wasn't");
                    };
                    let #argument_var_name = (&mut *#rc_temp_var.try_borrow_mut()?, default.to_owned());
                },
            });
        }
        // The pattern is exactly &mut Vec
        else if is_ref_mut(ty) && path_ends_with(ty, "Vec") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return Some(Argument {
                param_type: quote! { crate::interpreter::function::ParamType::List },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::List(#rc_temp_var)) = &values[#position] else {
                        panic!("Value #position needed to be a Sequence::List but wasn't");
                    };
                    let #argument_var_name = &mut *#rc_temp_var.try_borrow_mut()?;
                },
            });
        }
        // The pattern is exactly &str
        else if is_str_ref(ty) {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return Some(Argument {
                param_type: quote! { crate::interpreter::function::ParamType::String },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::String(#rc_temp_var)) = &values[#position] else {
                        panic!("Value #position needed to be a Sequence::String but wasn't");
                    };
                    let #rc_temp_var = #rc_temp_var.borrow();
                    let #argument_var_name = #rc_temp_var.as_ref();
                },
            });
        }
        // The pattern is &BigInt
        else if is_ref_of_bigint(ty) {
            let big_int = syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return Some(Argument {
                param_type: quote! { crate::interpreter::function::ParamType::Int },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let #big_int = if let crate::interpreter::value::Value::Number(crate::interpreter::num::Number::Int(crate::interpreter::int::Int::Int64(smol))) = &values[#position] {
                        Some(num::BigInt::from(*smol))
                    } else {
                        None
                    };

                    let #argument_var_name = match &values[#position] {
                        crate::interpreter::value::Value::Number(crate::interpreter::num::Number::Int(crate::interpreter::int::Int::BigInt(big))) => big,
                        crate::interpreter::value::Value::Number(crate::interpreter::num::Number::Int(crate::interpreter::int::Int::Int64(smoll))) => #big_int.as_ref().unwrap(),
                        _ => panic!("Value #position need to be an Int but wasn't"),
                    }
                },
            });
        }
        // If we need an owned Value
        else if path_ends_with(ty, "Value") && !is_ref(ty) {
            return Some(Argument {
                param_type: quote! { crate::interpreter::function::ParamType::Any },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let #argument_var_name = values[#position].clone();
                },
            });
        }
        // The pattern is &[Value]
        else if is_ref_of_slice_of_value(ty) {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return Some(Argument {
                param_type: quote! { crate::interpreter::function::ParamType::List },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::List(#rc_temp_var)) = &values[#position] else {
                        panic!("Value #position needed to be a Sequence::List but wasn't");
                    };
                    let #argument_var_name = &*#rc_temp_var.borrow();
                },
            });
        }
        // The pattern is something like `i64`
        else if let syn::Type::Path(path) = ty {
            return Some(Argument {
                param_type: into_param_type(ty),
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let #argument_var_name = #path :: try_from(&values[#position]).map_err(|err| crate::interpreter::function::FunctionCallError::ConvertToNativeTypeError(format!("{err}")))?
                },
            });
        }
        // The pattern is something like '&Number'
        else if let syn::Type::Reference(type_ref) = &*pat_type.ty {
            return Some(Argument {
                param_type: into_param_type(ty),
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let #argument_var_name = <#type_ref as TryFrom<&crate::interpreter::value::Value>> :: try_from(&values[#position]).map_err(|err| crate::interpreter::function::FunctionCallError::ConvertToNativeTypeError(format!("{err}")))?
                },
            });
        } else if let syn::Type::ImplTrait(syn::TypeImplTrait { .. }) = &*pat_type.ty {
            // TODO: we should perform a type check, but in order to get results quick we can just assume that all impl blocks are iterators

            todo!("the pattern is 'impl SomeTrait' but this hasn't been implemented")
        } else {
            panic!("not sure how to handle this type of thing:\n|---> {:?}", ty);
        }
    }

    panic!("Not sure how to handle receivers");
}
