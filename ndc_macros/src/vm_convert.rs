//! Helpers for generating `vm_native` closures from Rust function signatures.
//!
//! Each `try_vm_*` function returns `None` when the type cannot be represented
//! without the interpreter bridge, causing `export_module` to silently skip
//! vm_native generation for that function.

use crate::r#match::{is_ref, is_ref_mut, is_str_ref, path_ends_with};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Extraction of a single argument from a `&[VmValue]` slice.
pub struct VmInputArg {
    /// Code that reads `vm_raw{N}` (a `&VmValue`) and binds a converted local.
    pub extract: TokenStream,
    /// Expression to pass as the corresponding argument to the inner function.
    pub pass: TokenStream,
    /// `StaticType` token for this parameter.
    pub static_type: TokenStream,
}

/// Try to generate extraction code for a single parameter type.
///
/// Supported: `Number`, `&Number`, `f64`, `bool`, `&str`, `String`, `i64`.
/// Returns `None` for everything else (Sequence, BigInt, Value, …).
pub fn try_vm_input(ty: &syn::Type, position: usize) -> Option<VmInputArg> {
    let raw = format_ident!("vm_raw{position}");
    let temp = format_ident!("vm_temp{position}");

    if path_ends_with(ty, "Number") {
        let pass = if is_ref(ty) {
            quote! { &#temp }
        } else {
            quote! { #temp }
        };
        return Some(VmInputArg {
            extract: quote! {
                let #temp = #raw
                    .to_number()
                    .ok_or_else(|| format!("arg {}: expected number, got {}", #position, #raw.static_type()))?;
            },
            pass,
            static_type: quote! { ndc_interpreter::function::StaticType::Number },
        });
    }

    if path_ends_with(ty, "f64") {
        return Some(VmInputArg {
            extract: quote! {
                let #temp = #raw
                    .to_f64()
                    .ok_or_else(|| format!("arg {}: expected float, got {}", #position, #raw.static_type()))?;
            },
            pass: quote! { #temp },
            static_type: quote! { ndc_interpreter::function::StaticType::Float },
        });
    }

    if path_ends_with(ty, "bool") {
        return Some(VmInputArg {
            extract: quote! {
                let ndc_vm::value::Value::Bool(#temp) = #raw else {
                    return Err(format!("arg {}: expected bool, got {}", #position, #raw.static_type()));
                };
                let #temp = *#temp;
            },
            pass: quote! { #temp },
            static_type: quote! { ndc_interpreter::function::StaticType::Bool },
        });
    }

    // &str or owned String — both extracted as an owned String, then passed as &str or String.
    // Skip &mut String — mutation syncing is not supported.
    if (is_str_ref(ty) || path_ends_with(ty, "String")) && !is_ref_mut(ty) {
        let pass = if is_str_ref(ty) {
            quote! { &#temp }
        } else {
            quote! { #temp }
        };
        return Some(VmInputArg {
            extract: quote! {
                let #temp = match #raw {
                    ndc_vm::value::Value::Object(obj) => match obj.as_ref() {
                        ndc_vm::value::Object::String(s) => s.borrow().clone(),
                        _ => return Err(format!("arg {}: expected string, got {}", #position, #raw.static_type())),
                    },
                    _ => return Err(format!("arg {}: expected string, got {}", #position, #raw.static_type())),
                };
            },
            pass,
            static_type: quote! { ndc_interpreter::function::StaticType::String },
        });
    }

    if path_ends_with(ty, "i64") {
        return Some(VmInputArg {
            extract: quote! {
                let #temp = match #raw {
                    ndc_vm::value::Value::Int(i) => *i,
                    _ => return Err(format!("arg {}: expected int, got {}", #position, #raw.static_type())),
                };
            },
            pass: quote! { #temp },
            static_type: quote! { ndc_interpreter::function::StaticType::Int },
        });
    }

    None
}

/// Try to generate the return expression (producing `Result<VmValue, String>`)
/// and the `StaticType` token for the return type.
///
/// Supported: `Number`, `f64`, `bool`, `String`, `&str`, `i64`, `()` / `Default`,
/// `Result<T>` wrapping any of those.
/// Returns `None` for unsupported types.
pub fn try_vm_return(output: &syn::ReturnType) -> Option<(TokenStream, TokenStream)> {
    match output {
        syn::ReturnType::Default => Some((
            quote! { Ok(ndc_vm::value::Value::unit()) },
            quote! { ndc_interpreter::function::StaticType::Tuple(vec![]) },
        )),
        syn::ReturnType::Type(_, ty) => try_vm_return_type(ty),
    }
}

fn try_vm_return_type(ty: &syn::Type) -> Option<(TokenStream, TokenStream)> {
    if path_ends_with(ty, "Number") {
        return Some((
            quote! { Ok(ndc_vm::value::Value::from_number(result)) },
            quote! { ndc_interpreter::function::StaticType::Number },
        ));
    }
    if path_ends_with(ty, "f64") {
        return Some((
            quote! { Ok(ndc_vm::value::Value::Float(result)) },
            quote! { ndc_interpreter::function::StaticType::Float },
        ));
    }
    if path_ends_with(ty, "bool") {
        return Some((
            quote! { Ok(ndc_vm::value::Value::Bool(result)) },
            quote! { ndc_interpreter::function::StaticType::Bool },
        ));
    }
    // &str return — borrow tied to input, must convert to owned before returning
    if is_str_ref(ty) {
        return Some((
            quote! { Ok(ndc_vm::value::Value::string(result.to_owned())) },
            quote! { ndc_interpreter::function::StaticType::String },
        ));
    }
    if path_ends_with(ty, "String") {
        return Some((
            quote! { Ok(ndc_vm::value::Value::string(result)) },
            quote! { ndc_interpreter::function::StaticType::String },
        ));
    }
    if path_ends_with(ty, "i64") {
        return Some((
            quote! { Ok(ndc_vm::value::Value::Int(result)) },
            quote! { ndc_interpreter::function::StaticType::Int },
        ));
    }

    // Result<T, _> or anyhow::Result<T> — unwrap the Ok type and recurse
    if path_ends_with(ty, "Result") {
        if let syn::Type::Path(p) = ty {
            if let Some(last) = p.path.segments.last() {
                if let syn::PathArguments::AngleBracketed(args) = &last.arguments {
                    if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                        let (inner_code, inner_type) = try_vm_return_inner(inner)?;
                        return Some((
                            quote! {
                                let result = result.map_err(|e| e.to_string())?;
                                #inner_code
                            },
                            inner_type,
                        ));
                    }
                }
            }
        }
    }

    None
}

/// Like `try_vm_return_type` but also handles `()` (unit), which only makes
/// sense as the Ok type inside a `Result<()>`.
fn try_vm_return_inner(ty: &syn::Type) -> Option<(TokenStream, TokenStream)> {
    if let syn::Type::Tuple(t) = ty {
        if t.elems.is_empty() {
            return Some((
                quote! { Ok(ndc_vm::value::Value::unit()) },
                quote! { ndc_interpreter::function::StaticType::Tuple(vec![]) },
            ));
        }
    }
    try_vm_return_type(ty)
}
