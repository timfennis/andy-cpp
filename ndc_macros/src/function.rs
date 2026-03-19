use crate::convert::{Argument, TypeConverter, build};
use crate::r#match::{
    is_ndc_vm_map_value, is_ndc_vm_seq_value, is_ndc_vm_value, is_ref, is_ref_mut,
    is_ref_mut_of_hashmap_of_ndc_vm_value, is_ref_mut_of_max_heap, is_ref_mut_of_min_heap,
    is_ref_mut_of_slice_of_value, is_ref_mut_of_vec_of_ndc_vm_value,
    is_ref_mut_of_vecdeque_of_ndc_vm_value, is_ref_of_bigint, is_ref_of_hashmap_of_ndc_vm_value,
    is_ref_of_slice_of_ndc_vm_value, is_ref_of_slice_of_value, is_ref_of_vecdeque_of_ndc_vm_value,
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
            quote! { ndc_core::StaticType::Tuple(vec![]) }
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
                ndc_core::StaticType::Tuple(vec![
                    #(#inner),*
                ])
            }
        }
        syn::Type::Infer(_) => {
            quote::quote! { ndc_core::StaticType::Any }
        }
        _ => {
            panic!("unmapped type: {ty:?}");
        }
    }
}

#[allow(clippy::single_match_else)]
fn map_type_path(p: &syn::TypePath) -> TokenStream {
    let segment = p.path.segments.last().unwrap();

    match segment.ident.to_string().as_str() {
        "i32" | "i64" | "isize" | "u32" | "u64" | "usize" | "BigInt" => {
            quote::quote! { ndc_core::StaticType::Int }
        }
        "f32" | "f64" => {
            quote::quote! { ndc_core::StaticType::Float }
        }
        "bool" => {
            quote::quote! { ndc_core::StaticType::Bool }
        }
        "String" | "str" => {
            quote::quote! { ndc_core::StaticType::String }
        }
        "Vec" | "List" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                let inner = args.args.first().expect("Vec<> requires inner type");
                if let syn::GenericArgument::Type(inner_ty) = inner {
                    let mapped = map_type(inner_ty);
                    quote::quote! { ndc_core::StaticType::List(Box::new(#mapped)) }
                } else {
                    panic!("Vec inner not a type");
                }
            }
            _ => {
                quote::quote! { ndc_core::StaticType::List(Box::new(ndc_core::StaticType::Any)) }
            }
        },
        "VecDeque" | "Deque" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                let inner = args.args.first().expect("VecDeque<> requires inner type");
                if let syn::GenericArgument::Type(inner_ty) = inner {
                    let mapped = map_type(inner_ty);
                    quote::quote! { ndc_core::StaticType::Deque(Box::new(#mapped)) }
                } else {
                    panic!("VecDeque inner not a type");
                }
            }
            _ => quote::quote! {
                ndc_core::StaticType::Deque(Box::new(
                    ndc_core::StaticType::Any
                ))
            },
        },
        "DefaultMap" | "HashMap" | "Map" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                let mut iter = args.args.iter();
                let Some(key) = iter.next() else {
                    return temp_create_map_any();
                };
                let Some(val) = iter.next() else {
                    return temp_create_map_any();
                };
                let key_ty = match key {
                    syn::GenericArgument::Type(t) => t,
                    _ => panic!("Invalid map key"),
                };
                let val_ty = match val {
                    syn::GenericArgument::Type(t) => t,
                    _ => panic!("Invalid map value"),
                };
                let key_mapped = map_type(key_ty);
                let val_mapped = map_type(val_ty);
                quote::quote! { ndc_core::StaticType::Map { key: Box::new(#key_mapped), value: Box::new(#val_mapped) } }
            }
            _ => temp_create_map_any(),
        },
        "MinHeap" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                let inner = args.args.first().expect("MinHeap requires inner");
                if let syn::GenericArgument::Type(inner_ty) = inner {
                    let mapped = map_type(inner_ty);
                    quote::quote! { ndc_core::StaticType::MinHeap(Box::new(#mapped)) }
                } else {
                    panic!("MinHeap inner invalid");
                }
            }
            _ => quote::quote! {
                ndc_core::StaticType::MinHeap(Box::new(
                    ndc_core::StaticType::Any
                ))
            },
        },
        "MaxHeap" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                let inner = args.args.first().expect("MaxHeap requires inner");
                if let syn::GenericArgument::Type(inner_ty) = inner {
                    let mapped = map_type(inner_ty);
                    quote::quote! { ndc_core::StaticType::MaxHeap(Box::new(#mapped)) }
                } else {
                    panic!("MaxHeap inner invalid");
                }
            }
            _ => panic!("MaxHeap without generics"),
        },
        "Iterator" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                let inner = args.args.first().expect("Iterator requires inner");
                if let syn::GenericArgument::Type(inner_ty) = inner {
                    let mapped = map_type(inner_ty);
                    quote::quote! { ndc_core::StaticType::Iterator(Box::new(#mapped)) }
                } else {
                    panic!("Iterator inner invalid");
                }
            }
            _ => {
                quote::quote! { ndc_core::StaticType::Iterator(Box::new(ndc_core::StaticType::Any)) }
            }
        },
        "Option" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                let inner = args.args.first().expect("Option requires inner type");
                if let syn::GenericArgument::Type(inner_ty) = inner {
                    let mapped = map_type(inner_ty);
                    quote::quote! { ndc_core::StaticType::Option(Box::new(#mapped)) }
                } else {
                    panic!("Option inner invalid");
                }
            }
            _ => panic!("Option without generics"),
        },
        "Result" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                if let Some(syn::GenericArgument::Type(inner_ty)) = args.args.first() {
                    map_type(inner_ty)
                } else {
                    panic!("Result without generic arguments");
                }
            }
            _ => panic!("Result without angle bracketed args"),
        },
        "Number" => quote::quote! { ndc_core::StaticType::Number },
        "MapValue" => quote::quote! {
            ndc_core::StaticType::Map {
                key: Box::new(ndc_core::StaticType::Any),
                value: Box::new(ndc_core::StaticType::Any),
            }
        },
        "Value" | "EvaluationResult" | "SeqValue" => {
            quote::quote! { ndc_core::StaticType::Any }
        }
        unmatched => panic!("Cannot map type string '{unmatched}' to StaticType"),
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
    _return_type: &TokenStream,
    _docs: &str,
) -> WrappedFunction {
    let inner_ident = format_ident!("{}_inner", identifier);
    let inner = {
        let mut inner = function.clone();
        // Remove attributes to prevent compilation errors
        inner.attrs.clear();
        // Make public so the vm_native closure in register() can call it
        inner.vis = syn::Visibility::Public(syn::token::Pub::default());
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
            return Ok(ndc_interpreter::value::Value::unit());
        },
        syn::ReturnType::Type(_, ref typ) => match &**typ {
            // If the function returns a result we unpack it using the question mark operator
            ty @ syn::Type::Path(_) if path_ends_with(ty, "EvaluationResult") => quote! {
                return result;
            },
            // ndc_vm::value::Value / SeqValue / MapValue — dead code for VmNative body but must compile.
            ty @ syn::Type::Path(_)
                if is_ndc_vm_value(ty) || is_ndc_vm_seq_value(ty) || is_ndc_vm_map_value(ty) =>
            {
                quote! {
                    return Ok(ndc_interpreter::vm_bridge::vm_to_interp(&result));
                }
            }
            // Result<ndc_vm::value::Value / SeqValue / MapValue> — dead code for VmNative body but must compile.
            ty @ syn::Type::Path(_)
                if path_ends_with(ty, "Result") && result_inner_is_ndc_vm_value(ty) =>
            {
                quote! {
                    let value = result.map_err(|err| ndc_interpreter::function::FunctionCarrier::IntoEvaluationError(Box::new(err)))?;
                    return Ok(ndc_interpreter::vm_bridge::vm_to_interp(&value));
                }
            }
            ty @ syn::Type::Path(_) if path_ends_with(ty, "Result") => quote! {
                let value = result.map_err(|err| ndc_interpreter::function::FunctionCarrier::IntoEvaluationError(Box::new(err)))?;
                return Ok(ndc_interpreter::value::Value::from(value));
            },
            _ => quote! {
                let result = ndc_interpreter::value::Value::from(result);
                return Ok(result);
            },
        },
    };

    // Try to generate vm_native tokens. When successful, the body becomes
    // `FunctionBody::VmNative` instead of `GenericFunction`.
    let vm = try_generate_vm_native(&function, &inner_ident, register_as_function_name)
        .expect("always vm right?");

    // The inner helper is hoisted to module scope (not nested inside the wrapper)
    // so that both the tree-walk wrapper and the vm_native closure can call it.
    //
    // Expansion:
    //   pub fn wrapper_inner(...) { /* original body */ }
    //
    //   pub fn wrapper(values: &mut [Value], env: ...) -> EvaluationResult {
    //       let arg0 = ...; // from values[0]
    //       ...
    //       let result = wrapper_inner(arg0, ...);
    //       return Ok(Value::from(result));
    //   }
    let function_declaration = quote! {
        #[inline]
        #inner

        #[allow(unused_variables, unused_assignments)]
        pub fn #identifier (
            values: &mut [ndc_interpreter::value::Value],
            environment: &std::rc::Rc<std::cell::RefCell<ndc_interpreter::environment::Environment>>
        ) -> ndc_interpreter::evaluate::EvaluationResult {
            // Initialize the arguments and map them from the Andy C types to the rust types
            let [#(#arguments, )*] = values else { panic!("actual argument count did not match expected argument count when calling native method, this should be prevented by the runtime") };
            #(#argument_init_code_blocks; )*

            // Call the inner function with the unpacked arguments
            let result = #inner_ident (#(#arguments, )*);

            // Return the result (Possibly by unpacking errors)
            #return_expr
        }
    };

    let VmNativeTokens {
        native_let,
        param_types: _vm_param_types,
        param_names: _vm_param_names,
    } = vm;

    let function_registration = quote! {
        #native_let
        env.declare_global_fn(native);
    };
    WrappedFunction {
        function_declaration,
        function_registration,
    }
}

/// Tokens emitted by `try_generate_vm_native` when vm_native is possible.
struct VmNativeTokens {
    /// `let native: Rc<NativeFunction> = Rc::new(NativeFunction { ... });`
    native_let: TokenStream,
    /// StaticType expressions for each parameter (used in `TypeSignature::Exact`)
    param_types: Vec<TokenStream>,
    /// Parameter name strings (used in `Parameter::new`)
    param_names: Vec<TokenStream>,
}

/// Attempt to generate vm_native tokens for a function.
///
/// Returns `None` when any parameter or the return type cannot be expressed in
/// VM-native terms. In that case the function falls back to the interpreter bridge.
fn try_generate_vm_native(
    function: &syn::ItemFn,
    inner_ident: &syn::Ident,
    fn_name: &proc_macro2::Literal,
) -> Option<VmNativeTokens> {
    use crate::vm_convert::{try_vm_input, try_vm_return};

    let mut extracts = Vec::new();
    let mut passes = Vec::new();
    let mut param_types = Vec::new();
    let mut param_names = Vec::new();
    let mut has_vm_callable = false;
    let raw_args: Vec<_> = (0..function.sig.inputs.len())
        .map(|i| format_ident!("vm_raw{i}"))
        .collect();

    for (i, arg) in function.sig.inputs.iter().enumerate() {
        let pat_ty = match arg {
            syn::FnArg::Typed(pat_ty) => pat_ty,
            syn::FnArg::Receiver(_) => return None,
        };
        let ty = &*pat_ty.ty;
        if path_ends_with(ty, "VmCallable") && is_ref(ty) {
            has_vm_callable = true;
        }
        let conv = try_vm_input(ty, i)?;
        extracts.push(conv.extract);
        passes.push(conv.pass);
        param_types.push(conv.static_type);
        let name = match &*pat_ty.pat {
            syn::Pat::Ident(ident) => ident.ident.to_string(),
            _ => format!("arg{i}"),
        };
        param_names.push(quote! { #name });
    }

    let (return_code, return_static_type) = try_vm_return(&function.sig.output)?;
    let n = function.sig.inputs.len();

    let func_variant = if has_vm_callable {
        quote! {
            ndc_vm::value::NativeFunc::WithVm(Box::new(|args, _vm| match args {
                [#(#raw_args),*] => {
                    #(#extracts)*
                    let result = #inner_ident(#(#passes),*);
                    #return_code
                }
                _ => Err(ndc_vm::error::VmError::native(format!("expected {} arguments, got {}", #n, args.len()))),
            }))
        }
    } else {
        quote! {
            ndc_vm::value::NativeFunc::Simple(Box::new(|args| match args {
                [#(#raw_args),*] => {
                    #(#extracts)*
                    let result = #inner_ident(#(#passes),*);
                    #return_code
                }
                _ => Err(ndc_vm::error::VmError::native(format!("expected {} arguments, got {}", #n, args.len()))),
            }))
        }
    };

    // TODO: figure out how to get the documentation in
    let native_let = quote! {
        let native: std::rc::Rc<ndc_vm::value::NativeFunction> =
            std::rc::Rc::new(ndc_vm::value::NativeFunction {
                name: String::from(#fn_name),
                documentation: None,
                static_type: ndc_core::StaticType::Function {
                    parameters: Some(vec![#(#param_types.clone()),*]),
                    return_type: Box::new(#return_static_type),
                },
                func: #func_variant,
            });
    };

    Some(VmNativeTokens {
        native_let,
        param_types,
        param_names,
    })
}

fn into_param_type(ty: &syn::Type) -> TokenStream {
    match ty {
        ty if path_ends_with(ty, "Vec") => {
            quote! { ndc_core::StaticType::List(Box::new(ndc_core::StaticType::Any)) }
        }
        ty if path_ends_with(ty, "VecDeque") => {
            quote! { ndc_core::StaticType::Deque(Box::new(ndc_core::StaticType::Any)) }
        }
        ty if path_ends_with(ty, "DefaultMap")
            || path_ends_with(ty, "DefaultMapMut")
            || path_ends_with(ty, "HashMap") =>
        {
            temp_create_map_any()
        }
        ty if path_ends_with(ty, "MinHeap") => {
            quote! { ndc_core::StaticType::MinHeap(Box::new(ndc_core::StaticType::Any)) }
        }
        ty if path_ends_with(ty, "MaxHeap") => {
            quote! { ndc_core::StaticType::MaxHeap(Box::new(ndc_core::StaticType::Any)) }
        }
        ty if path_ends_with(ty, "ListRepr") => {
            quote! { ndc_core::StaticType::List(Box::new(ndc_core::StaticType::Any)) }
        }
        ty if path_ends_with(ty, "MapRepr") => temp_create_map_any(),
        syn::Type::Reference(syn::TypeReference { elem, .. }) => into_param_type(elem),
        syn::Type::Path(syn::TypePath { path, .. }) => match path {
            _ if path.is_ident("i64") => quote! { ndc_core::StaticType::Int },
            _ if path.is_ident("usize") => {
                quote! { ndc_core::StaticType::Int }
            }
            _ if path.is_ident("f64") => {
                quote! { ndc_core::StaticType::Float }
            }
            _ if path.is_ident("bool") => {
                quote! { ndc_core::StaticType::Bool }
            }
            _ if path.is_ident("Value") => {
                quote! { ndc_core::StaticType::Any }
            }
            _ if path.is_ident("Number") => {
                quote! { ndc_core::StaticType::Number }
            }
            _ if path.is_ident("Sequence") => {
                quote! { ndc_core::StaticType::Sequence(Box::new(ndc_core::StaticType::Any)) }
            }
            _ if path.is_ident("Callable") => {
                quote! {
                    ndc_core::StaticType::Function {
                        parameters: None,
                        return_type: Box::new(ndc_core::StaticType::Any)
                    }
                }
            }
            _ => panic!("Don't know how to convert Path into StaticType\n\n{path:?}"),
        },
        syn::Type::ImplTrait(_) => {
            quote! { ndc_core::StaticType::Iterator(Box::new(ndc_core::StaticType::Any)) }
        }
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

        // &[ndc_vm::value::Value] — only used in VmNative functions.
        // Dead-code stub; must come before is_ref_of_slice_of_value which also matches this type.
        if is_ref_of_slice_of_ndc_vm_value(ty) {
            let tmp_ident = syn::Ident::new(
                &format!("tmp_{argument_var_name}"),
                argument_var_name.span(),
            );
            return vec![Argument {
                param_type: quote! {
                    ndc_core::StaticType::List(Box::new(
                        ndc_core::StaticType::Any,
                    ))
                },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let #tmp_ident: Vec<ndc_vm::value::Value> = Vec::new();
                    let #argument_var_name = #tmp_ident.as_slice();
                },
            }];
        }

        // &mut Vec<ndc_vm::value::Value> — only used in VmNative functions.
        // Dead-code stub for the interpreter wrapper: bind a fresh empty Vec and
        // re-bind argument_var_name as &mut so the function call site compiles.
        if is_ref_mut_of_vec_of_ndc_vm_value(ty) {
            let tmp_ident = syn::Ident::new(
                &format!("tmp_{argument_var_name}"),
                argument_var_name.span(),
            );
            return vec![Argument {
                param_type: quote! {
                    ndc_core::StaticType::List(Box::new(
                        ndc_core::StaticType::Any,
                    ))
                },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let mut #tmp_ident: Vec<ndc_vm::value::Value> = Vec::new();
                    let #argument_var_name = &mut #tmp_ident;
                },
            }];
        }

        // &mut HashMap<ndc_vm::value::Value, ndc_vm::value::Value> — only used in VmNative functions.
        // Dead-code stub for the interpreter wrapper.
        if is_ref_mut_of_hashmap_of_ndc_vm_value(ty) {
            let tmp_ident = syn::Ident::new(
                &format!("tmp_{argument_var_name}"),
                argument_var_name.span(),
            );
            return vec![Argument {
                param_type: quote! {
                    ndc_core::StaticType::Map {
                        key: Box::new(ndc_core::StaticType::Any),
                        value: Box::new(ndc_core::StaticType::Any),
                    }
                },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let mut #tmp_ident: ndc_interpreter::hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value> = ndc_interpreter::hash_map::HashMap::default();
                    let #argument_var_name = &mut #tmp_ident;
                },
            }];
        }

        // &HashMap<ndc_vm::value::Value, ndc_vm::value::Value> — only used in VmNative functions.
        // Dead-code stub for the interpreter wrapper.
        if is_ref_of_hashmap_of_ndc_vm_value(ty) {
            let tmp_ident = syn::Ident::new(
                &format!("tmp_{argument_var_name}"),
                argument_var_name.span(),
            );
            return vec![Argument {
                param_type: quote! {
                    ndc_core::StaticType::Map {
                        key: Box::new(ndc_core::StaticType::Any),
                        value: Box::new(ndc_core::StaticType::Any),
                    }
                },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let #tmp_ident: ndc_interpreter::hash_map::HashMap<ndc_vm::value::Value, ndc_vm::value::Value> = ndc_interpreter::hash_map::HashMap::default();
                    let #argument_var_name = &#tmp_ident;
                },
            }];
        }

        // &mut BinaryHeap<Reverse<OrdValue>> — only used in VmNative functions (min-heap).
        // Dead-code stub for the interpreter wrapper.
        if is_ref_mut_of_min_heap(ty) {
            let tmp_ident = syn::Ident::new(
                &format!("tmp_{argument_var_name}"),
                argument_var_name.span(),
            );
            return vec![Argument {
                param_type: quote! {
                    ndc_core::StaticType::MinHeap(Box::new(
                        ndc_core::StaticType::Any,
                    ))
                },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let mut #tmp_ident: std::collections::BinaryHeap<std::cmp::Reverse<ndc_vm::value::OrdValue>> = std::collections::BinaryHeap::new();
                    let #argument_var_name = &mut #tmp_ident;
                },
            }];
        }

        // &mut BinaryHeap<OrdValue> — only used in VmNative functions (max-heap).
        // Dead-code stub for the interpreter wrapper.
        if is_ref_mut_of_max_heap(ty) {
            let tmp_ident = syn::Ident::new(
                &format!("tmp_{argument_var_name}"),
                argument_var_name.span(),
            );
            return vec![Argument {
                param_type: quote! {
                    ndc_core::StaticType::MaxHeap(Box::new(
                        ndc_core::StaticType::Any,
                    ))
                },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let mut #tmp_ident: std::collections::BinaryHeap<ndc_vm::value::OrdValue> = std::collections::BinaryHeap::new();
                    let #argument_var_name = &mut #tmp_ident;
                },
            }];
        }

        // &mut VecDeque<ndc_vm::value::Value> — only used in VmNative functions.
        // Dead-code stub for the interpreter wrapper.
        if is_ref_mut_of_vecdeque_of_ndc_vm_value(ty) {
            let tmp_ident = syn::Ident::new(
                &format!("tmp_{argument_var_name}"),
                argument_var_name.span(),
            );
            return vec![Argument {
                param_type: quote! {
                    ndc_core::StaticType::Deque(Box::new(
                        ndc_core::StaticType::Any,
                    ))
                },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let mut #tmp_ident: std::collections::VecDeque<ndc_vm::value::Value> = std::collections::VecDeque::new();
                    let #argument_var_name = &mut #tmp_ident;
                },
            }];
        }

        // &VecDeque<ndc_vm::value::Value> — only used in VmNative functions.
        // Dead-code stub for the interpreter wrapper.
        if is_ref_of_vecdeque_of_ndc_vm_value(ty) {
            let tmp_ident = syn::Ident::new(
                &format!("tmp_{argument_var_name}"),
                argument_var_name.span(),
            );
            return vec![Argument {
                param_type: quote! {
                    ndc_core::StaticType::Deque(Box::new(
                        ndc_core::StaticType::Any,
                    ))
                },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let #tmp_ident: std::collections::VecDeque<ndc_vm::value::Value> = std::collections::VecDeque::new();
                    let #argument_var_name = &#tmp_ident;
                },
            }];
        }

        // ndc_vm::value::Value — only used in VmNative functions.
        // The interpreter wrapper is dead code for VmNative bodies but must compile;
        // generate a stub that converts through the bridge (never actually executed).
        if is_ndc_vm_value(ty) {
            return vec![Argument {
                param_type: quote! { ndc_core::StaticType::Any },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let #argument_var_name =
                        ndc_interpreter::vm_bridge::interp_to_vm(#argument_var_name.clone());
                },
            }];
        }

        // ndc_vm::value::SeqValue — same bridge stub as Value but with Sequence static type.
        if is_ndc_vm_seq_value(ty) {
            return vec![Argument {
                param_type: quote! { ndc_core::StaticType::Sequence(Box::new(ndc_core::StaticType::Any)) },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let #argument_var_name =
                        ndc_interpreter::vm_bridge::interp_to_vm(#argument_var_name.clone());
                },
            }];
        }

        // ndc_vm::value::MapValue — same bridge stub as Value but with Map static type.
        if is_ndc_vm_map_value(ty) {
            return vec![Argument {
                param_type: quote! {
                    ndc_core::StaticType::Map {
                        key: Box::new(ndc_core::StaticType::Any),
                        value: Box::new(ndc_core::StaticType::Any),
                    }
                },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let #argument_var_name =
                        ndc_interpreter::vm_bridge::interp_to_vm(#argument_var_name.clone());
                },
            }];
        }

        // &VmCallable — only used in VmNative functions (HOF path).
        // Dead-code stub for the interpreter wrapper: extract function from InterpValue,
        // convert to VmValue, then construct a VmCallable with empty globals.
        // This code path is unreachable at runtime since VmNative bodies bypass the wrapper.
        if path_ends_with(ty, "VmCallable") {
            let tmp_ident = syn::Ident::new(
                &format!("tmp_{argument_var_name}"),
                argument_var_name.span(),
            );
            return vec![Argument {
                param_type: quote! {
                    ndc_core::StaticType::Function {
                        parameters: None,
                        return_type: Box::new(ndc_core::StaticType::Any),
                    }
                },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    // Dead-code stub: VmCallable params only appear in functions with
                    // FunctionBody::VmNative, so this interpreter wrapper is never called.
                    let #tmp_ident =
                        ndc_interpreter::vm_bridge::interp_to_vm(#argument_var_name.clone());
                    let mut vm_stub = ndc_vm::Vm::stub();
                    let #argument_var_name = if let ndc_vm::value::Value::Object(obj) = &#tmp_ident {
                        if let ndc_vm::value::Object::Function(f) = obj.as_ref() {
                            ndc_vm::VmCallable { function: f.clone(), vm: std::cell::RefCell::new(&mut vm_stub) }
                        } else {
                            panic!("VmCallable stub: expected Function variant");
                        }
                    } else {
                        panic!("VmCallable stub: expected function object");
                    };
                    let #argument_var_name = &#argument_var_name;
                },
            }];
        }

        // The pattern is Callable
        if path_ends_with(ty, "Callable") {
            let temp_var = syn::Ident::new(&format!("temp_{argument_var_name}"), identifier.span());
            return vec![Argument {
                param_type: quote! {
                    // TODO: how are we going to figure out the exact type of function here
                    ndc_core::StaticType::Function {
                        parameters: None,
                        return_type: Box::new(ndc_core::StaticType::Any)
                    }
                },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let ndc_interpreter::value::Value::Function(#temp_var) = #argument_var_name else {
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
                param_type: temp_create_map_any(),
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let ndc_interpreter::value::Value::Sequence(ndc_interpreter::sequence::Sequence::Map(#rc_temp_var, _default)) = #argument_var_name else {
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
                param_type: temp_create_map_any(),
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let ndc_interpreter::value::Value::Sequence(ndc_interpreter::sequence::Sequence::Map(#rc_temp_var, _default)) = #argument_var_name else {
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
                param_type: temp_create_map_any(),
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let ndc_interpreter::value::Value::Sequence(ndc_interpreter::sequence::Sequence::Map(#rc_temp_var, default)) = #argument_var_name else {
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
                param_type: temp_create_map_any(),
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let ndc_interpreter::value::Value::Sequence(ndc_interpreter::sequence::Sequence::Map(#rc_temp_var, default)) = #argument_var_name else {
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
                param_type: quote! { ndc_core::StaticType::List(Box::new(ndc_core::StaticType::Any)) },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let ndc_interpreter::value::Value::Sequence(ndc_interpreter::sequence::Sequence::List(#rc_temp_var)) = #argument_var_name else {
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
                    let ndc_interpreter::value::Value::Sequence(ndc_interpreter::sequence::Sequence::Deque(#rc_temp_var)) = #argument_var_name else {
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
                    let ndc_interpreter::value::Value::Sequence(ndc_interpreter::sequence::Sequence::Deque(#rc_temp_var)) = #argument_var_name else {
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
                    let ndc_interpreter::value::Value::Sequence(ndc_interpreter::sequence::Sequence::MaxHeap(#rc_temp_var)) = #argument_var_name else {
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
                    let ndc_interpreter::value::Value::Sequence(ndc_interpreter::sequence::Sequence::MaxHeap(#rc_temp_var)) = #argument_var_name else {
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
                    let ndc_interpreter::value::Value::Sequence(ndc_interpreter::sequence::Sequence::MinHeap(#rc_temp_var)) = #argument_var_name else {
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
                    let ndc_interpreter::value::Value::Sequence(ndc_interpreter::sequence::Sequence::MinHeap(#rc_temp_var)) = #argument_var_name else {
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
                param_type: quote! { ndc_core::StaticType::String },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let ndc_interpreter::value::Value::Sequence(ndc_interpreter::sequence::Sequence::String(#rc_temp_var)) = #argument_var_name else {
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
                param_type: quote! { ndc_core::StaticType::Int },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let #big_int = if let ndc_interpreter::value::Value::Number(ndc_interpreter::num::Number::Int(ndc_interpreter::int::Int::Int64(smol))) = #argument_var_name {
                        Some(num::BigInt::from(*smol))
                    } else {
                        None
                    };

                    let #argument_var_name = match #argument_var_name {
                        ndc_interpreter::value::Value::Number(ndc_interpreter::num::Number::Int(ndc_interpreter::int::Int::BigInt(big))) => big,
                        ndc_interpreter::value::Value::Number(ndc_interpreter::num::Number::Int(ndc_interpreter::int::Int::Int64(smoll))) => #big_int.as_ref().unwrap(),
                        _ => panic!("Value #position need to be an Int but wasn't"),
                    }
                },
            }];
        }
        // If we need an owned Value
        else if path_ends_with(ty, "Value") && !is_ref(ty) {
            return vec![Argument {
                param_type: quote! { ndc_core::StaticType::Any },
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
                param_type: quote! { ndc_core::StaticType::List(Box::new(ndc_core::StaticType::Any)) },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let ndc_interpreter::value::Value::Sequence(ndc_interpreter::sequence::Sequence::List(#rc_temp_var)) = #argument_var_name else {
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
                    param_type: quote! { ndc_core::StaticType::List(Box::new(ndc_core::StaticType::Any)) },
                    param_name: quote! { #original_name },
                    argument: quote! { #argument_var_name },
                    initialize_code: quote! {
                        let ndc_interpreter::value::Value::Sequence(ndc_interpreter::sequence::Sequence::List(#rc_temp_var)) = #argument_var_name else {
                            panic!("Value #position needed to be a Sequence::List but wasn't");
                        };
                        let #argument_var_name = &*#rc_temp_var.borrow();
                    },
                },
                // Argument {
                //     param_type: quote! { ndc_core::StaticType::Tuple },
                //     param_name: quote! { #original_name },
                //     argument: quote! { #argument_var_name },
                //     initialize_code: quote! {
                //         let ndc_interpreter::value::Value::Sequence(ndc_interpreter::sequence::Sequence::Tuple(#rc_temp_var)) = #argument_var_name else {
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
                param_type: quote! { ndc_core::StaticType::Rational },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let ndc_interpreter::value::Value::Number(ndc_interpreter::num::Number::Rational(#argument_var_name)) = #argument_var_name else {
                        panic!("Value #position needs to be Rational but wasn't");
                    };

                    let #argument_var_name = &#argument_var_name.clone();
                },
            }];
        }
        // The pattern is BigRational
        else if path_ends_with(ty, "BigRational") && !is_ref(ty) {
            return vec![Argument {
                param_type: quote! { ndc_core::StaticType::Rational },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let ndc_interpreter::value::Value::Number(ndc_interpreter::num::Number::Rational(#argument_var_name)) = #argument_var_name else {
                        panic!("VValue #position needs to be Rational but wasn't");
                    };

                    let #argument_var_name = *#argument_var_name.clone();
                },
            }];
        }
        // The pattern is Complex64
        else if path_ends_with(ty, "Complex64") && !is_ref(ty) {
            return vec![Argument {
                param_type: quote! { ndc_core::StaticType::Complex },
                param_name: quote! { #original_name },
                argument: quote! { #argument_var_name },
                initialize_code: quote! {
                    let ndc_interpreter::value::Value::Number(ndc_interpreter::num::Number::Complex(#argument_var_name)) = #argument_var_name else {
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
                    let #argument_var_name = #path :: try_from(#argument_var_name).map_err(|err| ndc_interpreter::function::FunctionCallError::ConvertToNativeTypeError(format!("{err}")))?
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
                    let #argument_var_name = <#type_ref as TryFrom<&mut ndc_interpreter::value::Value>> :: try_from(#argument_var_name).map_err(|err| ndc_interpreter::function::FunctionCallError::ConvertToNativeTypeError(format!("{err}")))?
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
                    let ndc_interpreter::value::Value::Sequence(ndc_interpreter::sequence::Sequence::Iterator(#rc_temp_var)) = #argument_var_name else {
                        panic!("Value #position needed to be a Sequence::Iterator but wasn't");
                    };

                    let #argument_var_name = ndc_interpreter::iterator::RcIter::new(Rc::clone(#rc_temp_var));
                },
            }];
        } else {
            panic!("not sure how to handle this type of thing:\n|---> {:?}", ty);
        }
    }

    panic!("Not sure how to handle receivers");
}

/// Returns true if the return type is `Result<ndc_vm::value::Value / SeqValue / MapValue, _>`,
/// so the interpreter wrapper can use `vm_to_interp` instead of `Value::from`.
fn result_inner_is_ndc_vm_value(ty: &syn::Type) -> bool {
    if let syn::Type::Path(tp) = ty {
        if let Some(last) = tp.path.segments.last() {
            if let syn::PathArguments::AngleBracketed(ab) = &last.arguments {
                if let Some(syn::GenericArgument::Type(inner)) = ab.args.first() {
                    return is_ndc_vm_value(inner)
                        || is_ndc_vm_seq_value(inner)
                        || is_ndc_vm_map_value(inner);
                }
            }
        }
    }
    false
}

// TODO: just adding Any as type here is lazy AF but CBA fixing generics
pub fn temp_create_map_any() -> TokenStream {
    quote! {
       ndc_core::StaticType::Map {
           key: Box::new(ndc_core::StaticType::Any),
           value: Box::new(ndc_core::StaticType::Any)
       }
    }
}
