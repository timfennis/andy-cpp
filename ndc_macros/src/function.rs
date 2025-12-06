use crate::convert::{Argument, TypeConverter, build};
use crate::r#match::{
    is_ref, is_ref_mut, is_ref_mut_of_slice_of_value, is_ref_of_bigint, is_ref_of_slice_of_value,
    is_str_ref, path_ends_with,
};
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::fmt::Write;

pub struct WrappedFunction {
    pub function_declaration: TokenStream,
    pub function_registration: TokenStream,
}

pub fn wrap_function(function: &syn::ItemFn) -> Vec<WrappedFunction> {
    let original_identifier = function.sig.ident.clone();

    let mut function_names = vec![proc_macro2::Literal::string(
        &original_identifier.to_string(),
    )];

    let mut return_type = None;

    let mut documentation_buffer = String::new();
    for attr in &function.attrs {
        if attr.path().is_ident("function") {
            attr.parse_nested_meta(|meta| {
                // #[function(name = "...")]
                if meta.path.is_ident("name") {
                    function_names = vec![meta.value()?.parse()?];
                    // register_as_function_name = meta.value()?.parse()?;
                    Ok(())
                } else if meta.path.is_ident("alias") {
                    function_names.push(meta.value()?.parse()?);
                    Ok(())
                } else if meta.path.is_ident("return_type") {
                    let value: syn::Type = meta.value()?.parse()?;
                    return_type = Some(map_type(&value));
                    Ok(())
                } else {
                    Err(meta.error("unsupported property on function"))
                }
            })
            .expect("invalid function attribute");
        } else if attr.path().is_ident("doc")
            && let syn::Meta::NameValue(meta) = &attr.meta
            && let syn::Expr::Lit(expr) = &meta.value
            && let syn::Lit::Str(lit_str) = &expr.lit
        {
            writeln!(documentation_buffer, "{}", lit_str.value().trim())
                .expect("failed to write docs");
        }
    }

    let return_type = return_type.unwrap_or_else(|| map_return_type(&function.sig.output));

    match &function.vis {
        syn::Visibility::Public(_) => {}
        syn::Visibility::Restricted(_) | syn::Visibility::Inherited => {
            panic!("only public functions can be wrapped for now")
        }
    }

    // If the function has no argument then the cartesian product stuff below doesn't work
    if function.sig.inputs.is_empty() {
        return function_names
            .iter()
            .map(|function_name| {
                wrap_single(
                    function.clone(),
                    &original_identifier,
                    function_name,
                    vec![],
                    &return_type,
                    &documentation_buffer,
                )
            })
            .collect();
    }

    // When we call create_temp_variable we can get multiple definitions for a variable
    // For instance when a rust function is `fn foo(list: &[Value])` we can define two internal functions for both Tuple and List
    let mut variation_id = 0usize;
    function_names
        .iter()
        .flat_map(|function_name| {
            function
                .sig
                .inputs
                .iter()
                .enumerate()
                .map(|(position, fn_arg)| {
                    let name = match fn_arg {
                        syn::FnArg::Receiver(_) => "self".to_string(),
                        syn::FnArg::Typed(syn::PatType { pat, .. }) => match &**pat {
                            syn::Pat::Ident(syn::PatIdent { ident, .. }) => ident.to_string(),
                            _ => panic!("don't know how to process this"),
                        },
                    };
                    create_temp_variable(position, fn_arg, &original_identifier, &name)
                })
                .multi_cartesian_product()
                .map(|args| {
                    let wrapped = wrap_single(
                        function.clone(),
                        &format_ident!("{original_identifier}_{variation_id}"),
                        function_name,
                        args,
                        &return_type,
                        &documentation_buffer,
                    );
                    variation_id += 1;
                    wrapped
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

fn map_return_type(output: &syn::ReturnType) -> TokenStream {
    match output {
        syn::ReturnType::Default => {
            // in case return type is not specified (for closures rust defaults to type inference which doesn't help us here)
            quote! { crate::interpreter::function::StaticType::Tuple(vec![]) }
        }
        syn::ReturnType::Type(_, ty) => map_type(ty),
    }
}

fn map_type(ty: &syn::Type) -> TokenStream {
    match ty {
        syn::Type::Path(p) => map_type_path(p),
        syn::Type::Reference(r) => map_type(r.elem.as_ref()),
        syn::Type::Tuple(t) => {
            let inner = t.elems.iter().map(map_type);
            quote::quote! {
                crate::interpreter::function::StaticType::Tuple(vec![
                    #(#inner),*
                ])
            }
        }
        _ => {
            panic!("unmapped type: {ty:?}");
        }
    }
}

fn map_type_path(p: &syn::TypePath) -> TokenStream {
    let segment = p.path.segments.last().unwrap();

    match segment.ident.to_string().as_str() {
        // Primitive single identifiers
        "i32" | "i64" | "isize" | "u32" | "u64" | "usize" | "BigInt" => {
            quote::quote! { crate::interpreter::function::StaticType::Int }
        }
        "f32" | "f64" => {
            quote::quote! { crate::interpreter::function::StaticType::Float }
        }
        "bool" => {
            quote::quote! { crate::interpreter::function::StaticType::Bool }
        }
        "String" | "str" => {
            quote::quote! { crate::interpreter::function::StaticType::String }
        }

        "Vec" => {
            quote::quote! { crate::interpreter::function::StaticType::List }
        }
        "DefaultMap" | "HashMap" => {
            quote::quote! { crate::interpreter::function::StaticType::Map }
        }
        "Number" => {
            quote::quote! { crate::interpreter::function::StaticType::Number }
        }
        "VecDeque" => {
            quote::quote! { crate::interpreter::function::StaticType::Deque }
        }
        "MinHeap" => {
            quote::quote! { crate::interpreter::function::StaticType::MinHeap }
        }
        "MaxHeap" => {
            quote::quote! { crate::interpreter::function::StaticType::MaxHeap }
        }
        "Iterator" => {
            quote::quote! { crate::interpreter::function::StaticType::Iterator }
        }
        "Option" => {
            // TODO: in the future add generic types
            quote::quote! { crate::interpreter::function::StaticType::Option }
        }
        // Generic wrappers like anyhow::Result<T>, std::result::Result<T>
        "Result" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                // Extract the first generic argument
                if let Some(syn::GenericArgument::Type(inner_ty)) = args.args.first() {
                    // Recurse on T
                    map_type(inner_ty)
                } else {
                    panic!("Result without generic arguments");
                }
            }

            _ => {
                panic!("Result return type found without syn::PathArguments::AngleBracketed");
            }
        },
        "Value" | "EvaluationResult" => {
            quote::quote! { crate::interpreter::function::StaticType::Any }
        }
        // Fallback
        unmatched => {
            panic!("Cannot map type string \"{unmatched}\" to StaticType");
        }
    }
}

/// Wraps an original rust function `function` in an outer function with the identifier `identifier`
/// It's registered with the environment as `register_as_function_name`
/// The argument translations mapping is defined by `input_arguments`
fn wrap_single(
    function: syn::ItemFn,
    identifier: &syn::Ident,
    register_as_function_name: &proc_macro2::Literal,
    input_arguments: Vec<Argument>,
    return_type: &TokenStream,
    docs: &str,
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
    let mut param_names: Vec<TokenStream> = Vec::new();

    for input_arg in input_arguments {
        let Argument {
            argument,
            initialize_code,
            param_type,
            param_name,
        } = input_arg;

        arguments.push(argument);
        argument_init_code_blocks.push(initialize_code);
        param_types.push(param_type);
        param_names.push(param_name);
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
                return Ok(crate::interpreter::value::Value::from(value));
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
            environment: &std::rc::Rc<std::cell::RefCell<crate::interpreter::environment::Environment>>
        ) -> crate::interpreter::evaluate::EvaluationResult {
            // Define the inner function that has the rust type signature
            #[inline]
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
        let func = crate::interpreter::function::FunctionBuilder::default()
            .body(crate::interpreter::function::FunctionBody::GenericFunction {
                function: #identifier,
                type_signature: crate::interpreter::function::TypeSignature::Exact(vec![
                    #( crate::interpreter::function::Parameter::new(#param_names, #param_types,) ),*
                ]),
                return_type: #return_type,
            })
            .name(String::from(#register_as_function_name))
            .documentation(String::from(#docs))
            .build()
            .expect("expected function creation in proc macro to always succeed");

        env.declare_global_fn(func);
    };

    WrappedFunction {
        function_declaration,
        function_registration,
    }
}

fn into_param_type(ty: &syn::Type) -> TokenStream {
    match ty {
        ty if path_ends_with(ty, "Vec") => {
            quote! { crate::interpreter::function::StaticType::List }
        }
        ty if path_ends_with(ty, "VecDeque") => {
            quote! { crate::interpreter::function::StaticType::Deque }
        }
        ty if path_ends_with(ty, "DefaultMap")
            || path_ends_with(ty, "DefaultMapMut")
            || path_ends_with(ty, "HashMap") =>
        {
            quote! { crate::interpreter::function::StaticType::Map }
        }
        ty if path_ends_with(ty, "MinHeap") => {
            quote! { crate::interpreter::function::StaticType::MinHeap }
        }
        ty if path_ends_with(ty, "MaxHeap") => {
            quote! { crate::interpreter::function::StaticType::MaxHeap }
        }
        ty if path_ends_with(ty, "ListRepr") => {
            quote! { crate::interpreter::function::StaticType::List }
        }
        ty if path_ends_with(ty, "MapRepr") => {
            quote! { crate::interpreter::function::StaticType::Map }
        }
        syn::Type::Reference(syn::TypeReference { elem, .. }) => into_param_type(elem),
        syn::Type::Path(syn::TypePath { path, .. }) => match path {
            _ if path.is_ident("i64") => quote! { crate::interpreter::function::StaticType::Int },
            _ if path.is_ident("usize") => quote! { crate::interpreter::function::StaticType::Int },
            _ if path.is_ident("f64") => quote! { crate::interpreter::function::StaticType::Float },
            _ if path.is_ident("bool") => quote! { crate::interpreter::function::StaticType::Bool },
            _ if path.is_ident("Value") => {
                quote! { crate::interpreter::function::StaticType::Any }
            }
            _ if path.is_ident("Number") => {
                quote! { crate::interpreter::function::StaticType::Number }
            }
            _ if path.is_ident("Sequence") => {
                quote! { crate::interpreter::function::StaticType::Sequence }
            }
            _ if path.is_ident("Callable") => {
                quote! { crate::interpreter::function::StaticType::Function }
            }
            _ => panic!("Don't know how to convert Path into StaticType\n\n{path:?}"),
        },
        syn::Type::ImplTrait(_) => quote! { crate::interpreter::function::StaticType::Iterator },
        x => panic!("Don't know how to convert {x:?} into StaticType"),
    }
}

fn create_temp_variable(
    position: usize,
    input: &syn::FnArg,
    identifier: &syn::Ident,
    original_name: &str,
) -> Vec<Argument> {
    let argument_var_name = syn::Ident::new(&format!("arg{position}"), identifier.span());
    if let syn::FnArg::Typed(pat_type) = input {
        let ty = &*pat_type.ty;

        let converters: Vec<Box<dyn TypeConverter>> = build();

        let temp_var = syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());

        for converter in converters {
            if converter.matches(ty) {
                return converter.convert(temp_var, original_name, argument_var_name);
            }
        }

        // The pattern is Callable
        if path_ends_with(ty, "Callable") {
            let temp_var = syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::StaticType::Function },
                param_name: quote! { #original_name },
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
                param_type: quote! { crate::interpreter::function::StaticType::Map },
                param_name: quote! { #original_name },
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
                param_type: quote! { crate::interpreter::function::StaticType::Map },
                param_name: quote! { #original_name },
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
                param_type: quote! { crate::interpreter::function::StaticType::Map },
                param_name: quote! { #original_name },
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
                param_type: quote! { crate::interpreter::function::StaticType::Map },
                param_name: quote! { #original_name },
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
                param_type: quote! { crate::interpreter::function::StaticType::List },
                param_name: quote! { #original_name },
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
            return vec![Argument {
                param_type: into_param_type(ty),
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Deque(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::List but wasn't");
                    };
                    let #argument_var_name = &mut *#rc_temp_var.try_borrow_mut()?;
                },
            }];
        }
        // The pattern is exactly &VecDeque
        else if is_ref(ty) && path_ends_with(ty, "VecDeque") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: into_param_type(ty),
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Deque(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::List but wasn't");
                    };
                    let #argument_var_name = &*#rc_temp_var.try_borrow()?;
                },
            }];
        }
        // The pattern is exactly &mut MaxHeap
        else if is_ref_mut(ty) && path_ends_with(ty, "MaxHeap") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: into_param_type(ty),
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::MaxHeap(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::MaxHeap but wasn't");
                    };
                    let #argument_var_name = &mut *#rc_temp_var.try_borrow_mut()?;
                },
            }];
        }
        // The pattern is exactly &MaxHeap
        else if is_ref(ty) && path_ends_with(ty, "MaxHeap") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: into_param_type(ty),
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::MaxHeap(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::MaxHeap but wasn't");
                    };
                    let #argument_var_name = &*#rc_temp_var.try_borrow()?;
                },
            }];
        }
        // The pattern is exactly &mut MinHeap
        else if is_ref_mut(ty) && path_ends_with(ty, "MinHeap") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: into_param_type(ty),
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::MinHeap(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::MinHeap but wasn't");
                    };
                    let #argument_var_name = &mut *#rc_temp_var.try_borrow_mut()?;
                },
            }];
        }
        // The pattern is exactly &MinHeap
        else if is_ref(ty) && path_ends_with(ty, "MinHeap") {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: into_param_type(ty),
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::MinHeap(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::MinHeap but wasn't");
                    };
                    let #argument_var_name = &*#rc_temp_var.try_borrow()?;
                },
            }];
        }
        // The pattern is exactly &str
        else if is_str_ref(ty) {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::StaticType::String },
                param_name: quote! { #original_name },
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
                param_type: quote! { crate::interpreter::function::StaticType::Int },
                param_name: quote! { #original_name },
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
                param_type: quote! { crate::interpreter::function::StaticType::Any },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let #argument_var_name = #argument_var_name.clone();
                },
            }];
        }
        // The pattern is &mut [Value]
        else if is_ref_mut_of_slice_of_value(ty) {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::StaticType::List },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::List(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::List but wasn't");
                    };
                    let #argument_var_name = &mut *#rc_temp_var.borrow_mut();
                },
            }];
        }
        // The pattern is &[Value]
        else if is_ref_of_slice_of_value(ty) {
            let rc_temp_var =
                syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![
                Argument {
                    param_type: quote! { crate::interpreter::function::StaticType::List },
                    param_name: quote! { #original_name },
                    argument: quote! { #argument_var_name },
                    initialize_code: quote! {
                        let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::List(#rc_temp_var)) = #argument_var_name else {
                            panic!("Value #position needed to be a Sequence::List but wasn't");
                        };
                        let #argument_var_name = &*#rc_temp_var.borrow();
                    },
                },
                // Argument {
                //     param_type: quote! { crate::interpreter::function::StaticType::Tuple },
                //     param_name: quote! { #original_name },
                //     argument: quote! { #argument_var_name },
                //     initialize_code: quote! {
                //         let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Tuple(#rc_temp_var)) = #argument_var_name else {
                //             panic!("Value #position needed to be a Sequence::List but wasn't");
                //         };
                //         let #argument_var_name = &#rc_temp_var;
                //     },
                // },
            ];
        }
        // The pattern is &BigRational
        else if path_ends_with(ty, "BigRational") && is_ref(ty) {
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::StaticType::Rational },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Number(crate::interpreter::num::Number::Rational(#argument_var_name)) = #argument_var_name else {
                        panic!("Value #position needs to be Rational but wasn't");
                    };

                    let #argument_var_name = &#argument_var_name.clone();
                },
            }];
        }
        // The pattern is BigRational
        else if path_ends_with(ty, "BigRational") && !is_ref(ty) {
            return vec![Argument {
                param_type: quote! { crate::interpreter::function::StaticType::Rational },
                param_name: quote! { #original_name },
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
                param_type: quote! { crate::interpreter::function::StaticType::Complex },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let crate::interpreter::value::Value::Number(crate::interpreter::num::Number::Complex(#argument_var_name)) = #argument_var_name else {
                        panic!("Value #position needs to be Complex64 but wasn't");
                    };

                    let #argument_var_name = #argument_var_name.clone();
                },
            }];
        }
        // The pattern is something like `i64` (but also matches other concrete types)
        else if let syn::Type::Path(path) = ty {
            return vec![Argument {
                param_type: into_param_type(ty),
                param_name: quote! { #original_name },
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
                param_name: quote! { #original_name },
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
                param_name: quote! { #original_name },
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
