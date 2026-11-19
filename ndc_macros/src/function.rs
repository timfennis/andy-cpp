//! Function wrapping logic for `#[export_module]`.
//!
//! Takes a `syn::ItemFn` and produces a `WrappedFunction` containing
//! the inner implementation and its `FunctionRegistry` registration code.

use crate::types::{NdcType, classify};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::fmt::Write;

/// A native's body, once, and its registration under each of its names and
/// signatures.
pub struct WrappedFunction {
    pub function_declaration: TokenStream,
    pub function_registrations: Vec<TokenStream>,
}

pub fn wrap_function(function: &syn::ItemFn) -> syn::Result<WrappedFunction> {
    let original_identifier = function.sig.ident.clone();

    let mut function_names = vec![proc_macro2::Literal::string(
        &original_identifier.to_string(),
    )];

    let mut return_type = None;
    let mut signatures: Vec<syn::LitStr> = Vec::new();

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
                } else if meta.path.is_ident("signature") {
                    // #[function(signature = "<T>([T], T) -> Option<T>")], once
                    // for each overload the one body serves.
                    signatures.push(meta.value()?.parse()?);
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

    for signature in &signatures {
        if return_type.is_some() {
            return Err(syn::Error::new_spanned(
                signature,
                "a signature already has a return type: drop `return_type`",
            ));
        }
        check_signature(signature, function.sig.inputs.len())?;
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

    let inner_ident = format_ident!("{original_identifier}_inner");
    let inner = {
        let mut inner = function.clone();
        inner.attrs.clear();
        inner.vis = syn::Visibility::Public(syn::token::Pub::default());
        inner.sig.ident = inner_ident.clone();
        inner
    };

    let signatures: Vec<Option<&syn::LitStr>> = if signatures.is_empty() {
        vec![None]
    } else {
        signatures.iter().map(Some).collect()
    };
    let function_registrations = function_names
        .iter()
        .flat_map(|name| signatures.iter().map(move |signature| (name, *signature)))
        .map(|(function_name, signature)| {
            register(
                function,
                &inner_ident,
                function_name,
                &return_type,
                signature,
                &documentation_buffer,
            )
        })
        .collect::<syn::Result<_>>()?;
    Ok(WrappedFunction {
        function_declaration: quote! {
            #[inline]
            #inner
        },
        function_registrations,
    })
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
        "AdvancedNumber" => Ok(quote! { ndc_core::StaticType::Number }),
        "Value" | "EvaluationResult" => Ok(quote! { ndc_core::StaticType::Any }),
        unmatched => Err(syn::Error::new_spanned(
            segment,
            format!("cannot map type '{unmatched}' to StaticType"),
        )),
    }
}

/// Checks that a signature such as `<T>([T], T)` parses, and that it
/// declares as many parameters as the Rust function takes.
fn check_signature(signature: &syn::LitStr, expected: usize) -> syn::Result<()> {
    let parsed = ndc_core::StaticType::signature(&signature.value())
        .map_err(|error| syn::Error::new_spanned(signature, error))?;
    let ndc_core::StaticType::Function {
        parameters: Some(parameters),
        ..
    } = parsed
    else {
        return Err(syn::Error::new_spanned(
            signature,
            "a native function takes a fixed number of arguments, so `(*)` doesn't fit",
        ));
    };
    if parameters.len() == expected {
        Ok(())
    } else {
        Err(syn::Error::new_spanned(
            signature,
            format!(
                "the signature declares {} parameters but the function takes {expected}",
                parameters.len()
            ),
        ))
    }
}

/// Registers the native `inner_ident` as `register_as_function_name`.
fn register(
    function: &syn::ItemFn,
    inner_ident: &syn::Ident,
    register_as_function_name: &proc_macro2::Literal,
    return_type: &TokenStream,
    signature: Option<&syn::LitStr>,
    docs: &str,
) -> syn::Result<TokenStream> {
    let native_let = try_generate_vm_native(
        function,
        inner_ident,
        register_as_function_name,
        docs,
        return_type,
        signature,
    )
    .ok_or_else(|| {
        syn::Error::new_spanned(
            &function.sig,
            "unsupported parameter or return type for VM native",
        )
    })?;
    Ok(quote! {
        #native_let
        env.declare_global_fn(native);
    })
}

/// `let native = NativeSpec { … };` for a function, or `None` when a
/// parameter or the return type can't be expressed in VM-native terms.
fn try_generate_vm_native(
    function: &syn::ItemFn,
    inner_ident: &syn::Ident,
    fn_name: &proc_macro2::Literal,
    docs: &str,
    return_type_override: &TokenStream,
    signature: Option<&syn::LitStr>,
) -> Option<TokenStream> {
    use crate::vm_convert::{Borrow, try_vm_input, try_vm_return};

    let mut extracts = Vec::new();
    let mut borrows = Vec::new();
    let mut passes = Vec::new();
    let mut param_types = Vec::new();
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
        borrows.push(conv.borrow);
        extracts.push((conv.borrow, conv.extract));
        passes.push(conv.pass);
        param_types.push(conv.static_type);
    }

    // Every argument that's only read is taken before any that's changed,
    // and copied if it's one of them, so `m.remove(m)` reads a copy of `m`
    // before it borrows `m` to remove from it. Two arguments the call
    // changes can't be the same value: `xs.append(xs)` would have to move
    // the elements of `xs` and keep them.
    extracts.sort_by_key(|(borrow, _)| *borrow == Borrow::Write);
    let changed = (0..borrows.len())
        .filter(|&i| borrows[i] == Borrow::Write)
        .collect::<Vec<_>>();
    let same_value = |a: usize, b: usize| {
        let (a, b) = (&raw_args[a], &raw_args[b]);
        quote! {
            matches!(
                (#a, #b),
                (ndc_vm::value::Value::Object(a), ndc_vm::value::Value::Object(b))
                    if std::rc::Rc::ptr_eq(a, b)
            )
        }
    };
    let mut alias_checks = Vec::new();
    for (i, borrow) in borrows.iter().enumerate() {
        if *borrow != Borrow::Read {
            continue;
        }
        let alias = format_ident!("vm_alias{i}");
        let aliases = changed.iter().map(|&w| same_value(i, w));
        alias_checks.push(quote! { let #alias = false #(|| #aliases)*; });
    }
    for (index, &first) in changed.iter().enumerate() {
        for &second in &changed[index + 1..] {
            let same = same_value(first, second);
            let message =
                format!("arg {second} is the same value as arg {first}, and the call changes both");
            alias_checks.push(quote! {
                if #same {
                    return Err(ndc_vm::error::VmError::native(#message.to_string()));
                }
            });
        }
    }
    let holds_a_borrow = extracts.iter().any(|(borrow, _)| *borrow != Borrow::None);
    let extracts = extracts
        .into_iter()
        .map(|(_, extract)| extract)
        .collect::<Vec<_>>();

    let (return_code, _) = try_vm_return(&function.sig.output)?;
    let return_static_type = return_type_override;
    let n = function.sig.inputs.len();

    // A callback is the program's code, which may use what the native holds
    // a borrow of, as a `sort_by` comparator may read the list it sorts:
    // holding one while calling back would panic there.
    if has_vm_callable && holds_a_borrow {
        let message = format!(
            "{}: a native that takes a callback can't borrow its other arguments; take them as `Value`s and copy what it needs",
            function.sig.ident
        );
        return Some(quote! { compile_error!(#message); });
    }

    let func_variant = if has_vm_callable {
        quote! {
            ndc_vm::value::NativeFunc::WithVm(Box::new(|args, _vm| match args {
                [#(#raw_args),*] => {
                    #(#alias_checks)*
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
                    #(#alias_checks)*
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
        quote! { Some(std::borrow::Cow::Borrowed(#docs)) }
    };
    // A signature replaces the types read off the Rust parameters.
    let static_type = if let Some(signature) = signature {
        quote! {
            ndc_core::StaticType::signature(#signature).unwrap_or_else(|error| {
                panic!("invalid signature of {}: {error}", #fn_name)
            })
        }
    } else {
        quote! {
            ndc_core::StaticType::Function {
                parameters: Some(vec![#(#param_types.clone()),*]),
                return_type: Box::new(#return_static_type),
            }
        }
    };
    // The type and the function are made when the native is first used.
    Some(quote! {
        let native = ndc_vm::NativeSpec {
            name: #fn_name,
            documentation: #documentation_tokens,
            static_type: Box::new(|| #static_type),
            func: Box::new(|| #func_variant),
        };
    })
}
