//! Function wrapping logic for `#[export_module]`.
//!
//! Takes a `syn::ItemFn` and produces a `WrappedFunction` containing
//! the inner implementation and its `FunctionRegistry` registration code.

use crate::types::{NdcType, classify};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::fmt::Write;

pub struct WrappedFunction {
    pub function_declaration: TokenStream,
    pub function_registration: TokenStream,
}

pub fn wrap_function(function: &syn::ItemFn) -> syn::Result<Vec<WrappedFunction>> {
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
                    Ok(())
                } else if meta.path.is_ident("alias") {
                    function_names.push(meta.value()?.parse()?);
                    Ok(())
                } else if meta.path.is_ident("return_type") {
                    let value: syn::Type = meta.value()?.parse()?;
                    return_type = Some(map_type(&value)?);
                    Ok(())
                } else {
                    Err(meta.error("unsupported property on function"))
                }
            })?;
        } else if attr.path().is_ident("doc")
            && let syn::Meta::NameValue(meta) = &attr.meta
            && let syn::Expr::Lit(expr) = &meta.value
            && let syn::Lit::Str(lit_str) = &expr.lit
        {
            writeln!(documentation_buffer, "{}", lit_str.value().trim())
                .expect("failed to write docs");
        }
    }

    let return_type = match return_type {
        Some(t) => t,
        None => map_return_type(&function.sig.output)?,
    };

    if !matches!(function.vis, syn::Visibility::Public(_)) {
        return Err(syn::Error::new_spanned(
            &function.sig.ident,
            "only public functions can be exported",
        ));
    }

    function_names
        .iter()
        .enumerate()
        .map(|(i, function_name)| {
            let ident = format_ident!("{original_identifier}_{i}");
            wrap_single(
                function.clone(),
                &ident,
                function_name,
                &return_type,
                &documentation_buffer,
            )
        })
        .collect()
}

fn map_return_type(output: &syn::ReturnType) -> syn::Result<TokenStream> {
    match output {
        syn::ReturnType::Default => Ok(quote! { ndc_core::StaticType::Tuple(vec![]) }),
        syn::ReturnType::Type(_, ty) => map_type(ty),
    }
}

fn map_type(ty: &syn::Type) -> syn::Result<TokenStream> {
    match ty {
        syn::Type::Path(p) => map_type_path(p),
        syn::Type::Reference(r) => map_type(r.elem.as_ref()),
        syn::Type::Tuple(t) => {
            let inner = t
                .elems
                .iter()
                .map(map_type)
                .collect::<syn::Result<Vec<_>>>()?;
            Ok(quote! {
                ndc_core::StaticType::Tuple(vec![
                    #(#inner),*
                ])
            })
        }
        syn::Type::Infer(_) => Ok(quote! { ndc_core::StaticType::Any }),
        _ => Err(syn::Error::new_spanned(
            ty,
            "cannot map type to StaticType".to_string(),
        )),
    }
}

#[allow(clippy::single_match_else)]
fn map_type_path(p: &syn::TypePath) -> syn::Result<TokenStream> {
    let segment = p
        .path
        .segments
        .last()
        .ok_or_else(|| syn::Error::new_spanned(p, "empty type path"))?;

    match segment.ident.to_string().as_str() {
        "i32" | "i64" | "isize" | "u32" | "u64" | "usize" | "BigInt" => {
            Ok(quote! { ndc_core::StaticType::Int })
        }
        "f32" | "f64" => Ok(quote! { ndc_core::StaticType::Float }),
        "bool" => Ok(quote! { ndc_core::StaticType::Bool }),
        "String" | "str" => Ok(quote! { ndc_core::StaticType::String }),
        "Vec" | "List" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                let inner = args
                    .args
                    .first()
                    .ok_or_else(|| syn::Error::new_spanned(segment, "Vec<> requires inner type"))?;
                if let syn::GenericArgument::Type(inner_ty) = inner {
                    let mapped = map_type(inner_ty)?;
                    Ok(quote! { ndc_core::StaticType::List(Box::new(#mapped)) })
                } else {
                    Err(syn::Error::new_spanned(inner, "Vec inner not a type"))
                }
            }
            _ => Ok(quote! { ndc_core::StaticType::List(Box::new(ndc_core::StaticType::Any)) }),
        },
        "VecDeque" | "Deque" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                let inner = args.args.first().ok_or_else(|| {
                    syn::Error::new_spanned(segment, "VecDeque<> requires inner type")
                })?;
                if let syn::GenericArgument::Type(inner_ty) = inner {
                    let mapped = map_type(inner_ty)?;
                    Ok(quote! { ndc_core::StaticType::Deque(Box::new(#mapped)) })
                } else {
                    Err(syn::Error::new_spanned(inner, "VecDeque inner not a type"))
                }
            }
            _ => Ok(quote! {
                ndc_core::StaticType::Deque(Box::new(
                    ndc_core::StaticType::Any
                ))
            }),
        },
        "DefaultMap" | "HashMap" | "Map" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                let mut iter = args.args.iter();
                let Some(syn::GenericArgument::Type(key_ty)) = iter.next() else {
                    return Ok(
                        quote! { ndc_core::StaticType::Map { key: Box::new(ndc_core::StaticType::Any), value: Box::new(ndc_core::StaticType::Any) } },
                    );
                };
                let Some(syn::GenericArgument::Type(val_ty)) = iter.next() else {
                    return Ok(
                        quote! { ndc_core::StaticType::Map { key: Box::new(ndc_core::StaticType::Any), value: Box::new(ndc_core::StaticType::Any) } },
                    );
                };
                let key_mapped = map_type(key_ty)?;
                let val_mapped = map_type(val_ty)?;
                Ok(
                    quote! { ndc_core::StaticType::Map { key: Box::new(#key_mapped), value: Box::new(#val_mapped) } },
                )
            }
            _ => Ok(
                quote! { ndc_core::StaticType::Map { key: Box::new(ndc_core::StaticType::Any), value: Box::new(ndc_core::StaticType::Any) } },
            ),
        },
        "MinHeap" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                let inner = args.args.first().ok_or_else(|| {
                    syn::Error::new_spanned(segment, "MinHeap requires inner type")
                })?;
                if let syn::GenericArgument::Type(inner_ty) = inner {
                    let mapped = map_type(inner_ty)?;
                    Ok(quote! { ndc_core::StaticType::MinHeap(Box::new(#mapped)) })
                } else {
                    Err(syn::Error::new_spanned(inner, "MinHeap inner not a type"))
                }
            }
            _ => Ok(quote! {
                ndc_core::StaticType::MinHeap(Box::new(
                    ndc_core::StaticType::Any
                ))
            }),
        },
        "MaxHeap" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                let inner = args.args.first().ok_or_else(|| {
                    syn::Error::new_spanned(segment, "MaxHeap requires inner type")
                })?;
                if let syn::GenericArgument::Type(inner_ty) = inner {
                    let mapped = map_type(inner_ty)?;
                    Ok(quote! { ndc_core::StaticType::MaxHeap(Box::new(#mapped)) })
                } else {
                    Err(syn::Error::new_spanned(inner, "MaxHeap inner not a type"))
                }
            }
            _ => Err(syn::Error::new_spanned(
                segment,
                "MaxHeap requires generic arguments",
            )),
        },
        "Iterator" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                let inner = args.args.first().ok_or_else(|| {
                    syn::Error::new_spanned(segment, "Iterator requires inner type")
                })?;
                if let syn::GenericArgument::Type(inner_ty) = inner {
                    let mapped = map_type(inner_ty)?;
                    Ok(quote! { ndc_core::StaticType::Iterator(Box::new(#mapped)) })
                } else {
                    Err(syn::Error::new_spanned(inner, "Iterator inner not a type"))
                }
            }
            _ => Ok(quote! { ndc_core::StaticType::Iterator(Box::new(ndc_core::StaticType::Any)) }),
        },
        "Option" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                let inner = args.args.first().ok_or_else(|| {
                    syn::Error::new_spanned(segment, "Option requires inner type")
                })?;
                if let syn::GenericArgument::Type(inner_ty) = inner {
                    let mapped = map_type(inner_ty)?;
                    Ok(quote! { ndc_core::StaticType::Option(Box::new(#mapped)) })
                } else {
                    Err(syn::Error::new_spanned(inner, "Option inner not a type"))
                }
            }
            _ => Err(syn::Error::new_spanned(
                segment,
                "Option requires generic arguments",
            )),
        },
        "Result" => match &segment.arguments {
            syn::PathArguments::AngleBracketed(args) => {
                if let Some(syn::GenericArgument::Type(inner_ty)) = args.args.first() {
                    map_type(inner_ty)
                } else {
                    Err(syn::Error::new_spanned(
                        segment,
                        "Result requires generic arguments",
                    ))
                }
            }
            _ => Err(syn::Error::new_spanned(
                segment,
                "Result requires angle bracketed arguments",
            )),
        },
        "Number" => Ok(quote! { ndc_core::StaticType::Number }),
        "MapValue" => Ok(quote! {
            ndc_core::StaticType::Map {
                key: Box::new(ndc_core::StaticType::Any),
                value: Box::new(ndc_core::StaticType::Any),
            }
        }),
        "Value" | "EvaluationResult" | "SeqValue" => Ok(quote! { ndc_core::StaticType::Any }),
        unmatched => Err(syn::Error::new_spanned(
            segment,
            format!("cannot map type '{unmatched}' to StaticType"),
        )),
    }
}

fn wrap_single(
    function: syn::ItemFn,
    identifier: &syn::Ident,
    register_as_function_name: &proc_macro2::Literal,
    return_type: &TokenStream,
    docs: &str,
) -> syn::Result<WrappedFunction> {
    let inner_ident = format_ident!("{}_inner", identifier);
    let inner = {
        let mut inner = function.clone();
        inner.attrs.clear();
        inner.vis = syn::Visibility::Public(syn::token::Pub::default());
        inner.sig.ident = inner_ident.clone();
        inner
    };

    let vm = try_generate_vm_native(
        &function,
        &inner_ident,
        register_as_function_name,
        docs,
        return_type,
    )
    .ok_or_else(|| {
        syn::Error::new_spanned(
            &function.sig,
            "unsupported parameter or return type for VM native",
        )
    })?;

    let function_declaration = quote! {
        #[inline]
        #inner
    };

    let VmNativeTokens {
        native_let,
        param_types: _,
        param_names: _,
    } = vm;

    let function_registration = quote! {
        #native_let
        env.declare_global_fn(native);
    };
    Ok(WrappedFunction {
        function_declaration,
        function_registration,
    })
}

/// Tokens emitted by `try_generate_vm_native` when `vm_native` is possible.
#[allow(dead_code)]
struct VmNativeTokens {
    /// `let native: Rc<NativeFunction> = Rc::new(NativeFunction { ... });`
    native_let: TokenStream,
    /// `StaticType` expressions for each parameter
    param_types: Vec<TokenStream>,
    /// Parameter name strings
    param_names: Vec<TokenStream>,
}

/// Attempt to generate `vm_native` tokens for a function.
///
/// Returns `None` when any parameter or the return type cannot be expressed in
/// VM-native terms.
fn try_generate_vm_native(
    function: &syn::ItemFn,
    inner_ident: &syn::Ident,
    fn_name: &proc_macro2::Literal,
    docs: &str,
    return_type_override: &TokenStream,
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
        if classify(ty) == Some(NdcType::MutVmCallable) {
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

    let (return_code, _) = try_vm_return(&function.sig.output)?;
    let return_static_type = return_type_override;
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

    let documentation_tokens = if docs.is_empty() {
        quote! { None }
    } else {
        quote! { Some(String::from(#docs)) }
    };
    let native_let = quote! {
        let native: std::rc::Rc<ndc_vm::value::NativeFunction> =
            std::rc::Rc::new(ndc_vm::value::NativeFunction {
                name: String::from(#fn_name),
                documentation: #documentation_tokens,
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
