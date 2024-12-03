use std::hash::DefaultHasher;

use crate::r#match::{
    is_ref, is_ref_mut, is_ref_of_bigint, is_ref_of_slice_of_value, is_str_ref, is_string,
    path_ends_with,
};
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub struct WrappedFunction {
    pub function_declaration: TokenStream,
    pub function_registration: TokenStream,
}

pub fn wrap_function(function: syn::ItemFn) -> Vec<WrappedFunction> {
    let original_identifier = function.sig.ident.clone();

    let mut register_as_function_name =
        proc_macro2::Literal::string(&original_identifier.to_string());

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

    // When we call create_temp_variable we can get multiple definitions for a variable
    // For instance when a rust function is `fn foo(list: &[Value])` we can define two internal functions for both Tuple and List
    let functions = function
        .sig
        .inputs
        .iter()
        .enumerate()
        .map(|(position, fn_arg)| create_temp_variable(position, fn_arg, &original_identifier))
        .multi_cartesian_product()
        .enumerate()
        .map(|(variation_id, args)| {
            wrap_single(
                function.clone(),
                format_ident!("{}_{}", original_identifier, variation_id),
                &register_as_function_name,
                args,
            )
        })
        .collect::<Vec<_>>();

    functions
}

/// Wraps an original rust function `function` in an outer function with the identifier `identifier`
/// It's registered with the environment as `register_as_function_name`
/// The argument translations mapping is defined by `input_arguments`
fn wrap_single(
    function: syn::ItemFn,
    identifier: syn::Ident,
    register_as_function_name: &proc_macro2::Literal,
    input_arguments: Vec<Argument>,
) -> WrappedFunction {
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

    for Argument {
        argument,
        initialize_code,
        param_type,
    } in input_arguments.into_iter()
    {
        arguments.push(argument);
        argument_init_code_blocks.push(initialize_code);
        param_types.push(param_type);
    }

    let return_expr = match function.sig.output {
        syn::ReturnType::Default => quote! {
            return Ok(crate::interpreter::value::Value::unit());
        },
        syn::ReturnType::Type(_, typ) => match &*typ {
            // If the function returns a result we unpack it using the question mark operator
            ty @ syn::Type::Path(_) if path_ends_with(ty, "EvaluationResult") => quote! {
                return result;
            },
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
            values: &mut [crate::interpreter::value::Value],
            environment: &crate::interpreter::environment::EnvironmentRef
        ) -> crate::interpreter::evaluate::EvaluationResult {
            // Define the inner function that has the rust type signature
            #inner

            // Initialize the arguments and map them from the Andy C types to the rust types
            let [#(#arguments, )*] = values else { panic!("actual argument count did not match expected argument count when calling native method, this should be prevented by the runtime") };
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

#[derive(Debug, Clone)]
struct Argument {
    param_type: TokenStream,
    argument: TokenStream,
    initialize_code: TokenStream,
}

fn into_param_type(ty: &syn::Type) -> TokenStream {
    match ty {
        ty if path_ends_with(ty, "Vec") => quote! { crate::interpreter::function::ParamType::List },
        ty if path_ends_with(ty, "VecDeque") => {
            quote! { crate::interpreter::function::ParamType::Deque }
        }
        ty if path_ends_with(ty, "DefaultMap")
            || path_ends_with(ty, "DefaultMapMut")
            || path_ends_with(ty, "HashMap") =>
        {
            quote! { crate::interpreter::function::ParamType::Map }
        }
        ty if path_ends_with(ty, "MinHeap") => {
            quote! { crate::interpreter::function::ParamType::MinHeap }
        }
        ty if path_ends_with(ty, "MaxHeap") => {
            quote! { crate::interpreter::function::ParamType::MaxHeap }
        }
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
            _ if path.is_ident("Callable") => {
                quote! { crate::interpreter::function::ParamType::Function }
            }
            _ => panic!("Don't know how to convert Path into ParamType\n\n{path:?}"),
        },
        syn::Type::ImplTrait(_) => quote! { crate::interpreter::function::ParamType::Iterator },
        x => panic!("Don't know how to convert {x:?} into ParamType"),
    }
}

fn create_temp_variable(
    position: usize,
    input: &syn::FnArg,
    identifier: &syn::Ident,
) -> Vec<Argument> {
    let argument_var_name = syn::Ident::new(&format!("arg{position}"), identifier.span());
    if let syn::FnArg::Typed(pat_type) = input {
        let ty = &*pat_type.ty;

        // The pattern is exactly &mut String
        if is_ref_mut(ty) && is_string(ty) {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::ParamType::String },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::String(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::String but wasn't");
                    };
                    let #argument_var_name = &mut *#rc_temp_var.try_borrow_mut()?;
                },
            }];
        }
        // The pattern is Callable
        else if path_ends_with(ty, "Callable") {
            let temp_var = syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::ParamType::Function },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Function(#temp_var) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::Map but wasn't");
                    };
                    let #argument_var_name = &Callable {
                        function: Rc::clone(#temp_var),
                        environment: environment
                    };
                },
            }];
        }
        // The pattern is &HashMap<Value, Value>
        else if is_ref(ty) && path_ends_with(ty, "HashMap") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::ParamType::Map },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Map(#rc_temp_var, _default)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::Map but wasn't");
                    };
                    let #argument_var_name = &*#rc_temp_var.borrow();
                },
            }];
        }
        // The pattern is &mut HashMap<Value, Value>
        else if is_ref_mut(ty) && path_ends_with(ty, "HashMap") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::ParamType::Map },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Map(#rc_temp_var, _default)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::Map but wasn't");
                    };
                    let #argument_var_name = &mut *#rc_temp_var.try_borrow_mut()?;
                },
            }];
        }
        // The pattern is DefaultMap
        else if path_ends_with(ty, "DefaultMap") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::ParamType::Map },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Map(#rc_temp_var, default)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::Map but wasn't");
                    };
                    let #argument_var_name = (&*#rc_temp_var.borrow(), default.to_owned());
                },
            }];
        }
        // The pattern is DefaultMapMut
        else if path_ends_with(ty, "DefaultMapMut") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::ParamType::Map },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Map(#rc_temp_var, default)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::Map but wasn't");
                    };
                    let #argument_var_name = (&mut *#rc_temp_var.try_borrow_mut()?, default.to_owned());
                },
            }];
        }
        // The pattern is exactly &mut Vec
        // TODO: support this for tuple
        else if is_ref_mut(ty) && path_ends_with(ty, "Vec") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::ParamType::List },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::List(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::List but wasn't");
                    };
                    let #argument_var_name = &mut *#rc_temp_var.try_borrow_mut()?;
                },
            }];
        }
        // The pattern is exactly &mut VecDeque
        else if is_ref_mut(ty) && path_ends_with(ty, "VecDeque") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return Some(Argument {
                param_type: into_param_type(ty),
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Deque(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::List but wasn't");
                    };
                    let #argument_var_name = &mut *#rc_temp_var.try_borrow_mut()?;
                },
            });
        }
        // The pattern is exactly &VecDeque
        else if is_ref(ty) && path_ends_with(ty, "VecDeque") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return Some(Argument {
                param_type: into_param_type(ty),
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Deque(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::List but wasn't");
                    };
                    let #argument_var_name = &*#rc_temp_var.try_borrow()?;
                },
            });
        }
        // The pattern is exactly &mut MaxHeap
        else if is_ref_mut(ty) && path_ends_with(ty, "MaxHeap") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return Some(Argument {
                param_type: into_param_type(ty),
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::MaxHeap(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::MaxHeap but wasn't");
                    };
                    let #argument_var_name = &mut *#rc_temp_var.try_borrow_mut()?;
                },
            });
        }
        // The pattern is exactly &MaxHeap
        else if is_ref(ty) && path_ends_with(ty, "MaxHeap") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return Some(Argument {
                param_type: into_param_type(ty),
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::MaxHeap(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::MaxHeap but wasn't");
                    };
                    let #argument_var_name = &*#rc_temp_var.try_borrow()?;
                },
            });
        }
        // The pattern is exactly &mut MinHeap
        else if is_ref_mut(ty) && path_ends_with(ty, "MinHeap") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return Some(Argument {
                param_type: into_param_type(ty),
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::MinHeap(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::MinHeap but wasn't");
                    };
                    let #argument_var_name = &mut *#rc_temp_var.try_borrow_mut()?;
                },
            });
        }
        // The pattern is exactly &MinHeap
        else if is_ref(ty) && path_ends_with(ty, "MinHeap") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return Some(Argument {
                param_type: into_param_type(ty),
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::MinHeap(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::MinHeap but wasn't");
                    };
                    let #argument_var_name = &*#rc_temp_var.try_borrow()?;
                },
            });
        }
        // The pattern is exactly &str
        else if is_str_ref(ty) {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::ParamType::String },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::String(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::String but wasn't");
                    };
                    let #rc_temp_var = #rc_temp_var.borrow();
                    let #argument_var_name = #rc_temp_var.as_ref();
                },
            }];
        }
        // The pattern is &BigInt
        else if is_ref_of_bigint(ty) {
            let big_int = syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::ParamType::Int },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let #big_int = if let crate::interpreter::value::Value::Number(crate::interpreter::num::Number::Int(crate::interpreter::int::Int::Int64(smol))) = #argument_var_name {
                        Some(num::BigInt::from(*smol))
                    } else {
                        None
                    };

                    let #argument_var_name = match #argument_var_name {
                        crate::interpreter::value::Value::Number(crate::interpreter::num::Number::Int(crate::interpreter::int::Int::BigInt(big))) => big,
                        crate::interpreter::value::Value::Number(crate::interpreter::num::Number::Int(crate::interpreter::int::Int::Int64(smoll))) => #big_int.as_ref().unwrap(),
                        _ => panic!("Value #position need to be an Int but wasn't"),
                    }
                },
            }];
        }
        // If we need an owned Value
        else if path_ends_with(ty, "Value") && !is_ref(ty) {
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::ParamType::Any },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let #argument_var_name = #argument_var_name.clone();
                },
            }];
        }
        // The pattern is &[Value]
        else if is_ref_of_slice_of_value(ty) {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![
                Argument {
                    param_type: quote! { crate::interpreter::function::ParamType::List },
                    argument: quote! { #argument_var_name },
                    initialize_code: quote! {
                        let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::List(#rc_temp_var)) = #argument_var_name else {
                            panic!("Value #position needed to be a Sequence::List but wasn't");
                        };
                        let #argument_var_name = &*#rc_temp_var.borrow();
                    },
                },
                Argument {
                    param_type: quote! { crate::interpreter::function::ParamType::Tuple },
                    argument: quote! { #argument_var_name },
                    initialize_code: quote! {
                        let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Tuple(#rc_temp_var)) = #argument_var_name else {
                            panic!("Value #position needed to be a Sequence::List but wasn't");
                        };
                        let #argument_var_name = &#rc_temp_var;
                    },
                },
            ];
        }
        // The pattern is &BigRational
        else if path_ends_with(ty, "BigRational") && is_ref(ty) {
            return Some(Argument {
                param_type: quote! { crate::interpreter::function::ParamType::Rational },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Number(crate::interpreter::num::Number::Rational(#argument_var_name)) = #argument_var_name else {
                        panic!("VValue #position needs to be Rational but wasn't");
                    };

                    let #argument_var_name = &#argument_var_name.clone();
                },
            });
        }
        // The pattern is BigRational
        else if path_ends_with(ty, "BigRational") && !is_ref(ty) {
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::ParamType::Rational },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Number(crate::interpreter::num::Number::Rational(#argument_var_name)) = #argument_var_name else {
                        panic!("VValue #position needs to be Rational but wasn't");
                    };

                    let #argument_var_name = *#argument_var_name.clone();
                },
            }];
        }
        // The pattern is Complex64
        else if path_ends_with(ty, "Complex64") && !is_ref(ty) {
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::ParamType::Complex },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Number(crate::interpreter::num::Number::Complex(#argument_var_name)) = #argument_var_name else {
                        panic!("VValue #position needs to be Complex64 but wasn't");
                    };

                    let #argument_var_name = #argument_var_name.clone();
                },
            }];
        }
        // The pattern is something like `i64` (but also matches other concrete types)
        else if let syn::Type::Path(path) = ty {
            return vec![Argument {
                param_type: into_param_type(ty),
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let #argument_var_name = #path :: try_from(#argument_var_name).map_err(|err| crate::interpreter::function::FunctionCallError::ConvertToNativeTypeError(format!("{err}")))?
                },
            }];
        }
        // The pattern is something like '&Number'
        else if let syn::Type::Reference(type_ref) = &*pat_type.ty {
            return vec![Argument {
                param_type: into_param_type(ty),
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let #argument_var_name = <#type_ref as TryFrom<&mut crate::interpreter::value::Value>> :: try_from(#argument_var_name).map_err(|err| crate::interpreter::function::FunctionCallError::ConvertToNativeTypeError(format!("{err}")))?
                },
            }];
        }
        // The pattern is a trait implementation TODO: this is not implemented
        else if let syn::Type::ImplTrait(syn::TypeImplTrait { .. }) = &*pat_type.ty {
            // TODO: we should perform a type check, but in order to get results quick we can just assume that all impl blocks are iterators

            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: into_param_type(ty),
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Iterator(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::Iterator but wasn't");
                    };

                    let #argument_var_name = crate::interpreter::iterator::RcIter::new(Rc::clone(#rc_temp_var));
                },
            }];
        } else {
            panic!("not sure how to handle this type of thing:\n|---> {:?}", ty);
        }
    }

    panic!("Not sure how to handle receivers");
}
