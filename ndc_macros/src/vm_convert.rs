//! Code generation for VM-native function parameter extraction and return conversion.
//!
//! Each `try_vm_*` function returns `None` when the type cannot be represented
//! without the interpreter bridge, causing `export_module` to silently skip
//! vm_native generation for that function.

use crate::types::{NdcType, classify, unwrap_result};
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

/// Build the `VmError::native(format!("arg {pos}: expected {expected}, got {actual}"))` expression.
fn arg_error(position: usize, expected: &str) -> TokenStream {
    let raw = format_ident!("vm_raw{position}");
    quote! {
        ndc_vm::error::VmError::native(format!(
            "arg {}: expected {}, got {}",
            #position, #expected, #raw.static_type()
        ))
    }
}

/// Generate extraction code for a RefCell-based collection parameter.
///
/// Covers List, Deque, MinHeap, MaxHeap (but not Map, which has different destructuring).
fn refcell_collection_arg(
    position: usize,
    expected: &str,
    variant: &str,
    mutable: bool,
    static_type: TokenStream,
) -> VmInputArg {
    let raw = format_ident!("vm_raw{position}");
    let temp = format_ident!("vm_temp{position}");
    let err = arg_error(position, expected);
    let rc = format_ident!("{temp}_rc");
    let guard = format_ident!("{temp}_guard");
    let variant_ident = format_ident!("{variant}");

    let (borrow, pass) = if mutable {
        (
            quote! { let mut #guard = #rc.borrow_mut(); },
            quote! { &mut *#guard },
        )
    } else {
        (quote! { let #guard = #rc.borrow(); }, quote! { &*#guard })
    };

    VmInputArg {
        extract: quote! {
            let ndc_vm::value::Value::Object(ref #rc) = *#raw else {
                return Err(#err);
            };
            let ndc_vm::value::Object::#variant_ident(ref #rc) = *#rc.as_ref() else {
                return Err(#err);
            };
            #borrow
        },
        pass,
        static_type,
    }
}

/// Try to generate extraction code for a single parameter type.
///
/// Returns `None` for types that cannot be expressed in VM-native terms.
pub fn try_vm_input(ty: &syn::Type, position: usize) -> Option<VmInputArg> {
    let ndc_type = classify(ty)?;
    let raw = format_ident!("vm_raw{position}");
    let temp = format_ident!("vm_temp{position}");

    let result = match ndc_type {
        NdcType::Number | NdcType::NumberRef => {
            let err = arg_error(position, "number");
            let pass = if ndc_type == NdcType::NumberRef {
                quote! { &#temp }
            } else {
                quote! { #temp }
            };
            VmInputArg {
                extract: quote! {
                    let #temp = #raw.to_number().ok_or_else(|| #err)?;
                },
                pass,
                static_type: quote! { ndc_core::StaticType::Number },
            }
        }

        NdcType::F64 => {
            let err = arg_error(position, "float");
            VmInputArg {
                extract: quote! {
                    let #temp = #raw.to_f64().ok_or_else(|| #err)?;
                },
                pass: quote! { #temp },
                static_type: quote! { ndc_core::StaticType::Float },
            }
        }

        NdcType::Bool => {
            let err = arg_error(position, "bool");
            VmInputArg {
                extract: quote! {
                    let ndc_vm::value::Value::Bool(#temp) = #raw else {
                        return Err(#err);
                    };
                    let #temp = *#temp;
                },
                pass: quote! { #temp },
                static_type: quote! { ndc_core::StaticType::Bool },
            }
        }

        NdcType::String | NdcType::StrRef => {
            let err = arg_error(position, "string");
            let pass = if ndc_type == NdcType::StrRef {
                quote! { &#temp }
            } else {
                quote! { #temp }
            };
            VmInputArg {
                extract: quote! {
                    let #temp = match #raw {
                        ndc_vm::value::Value::Object(obj) => match obj.as_ref() {
                            ndc_vm::value::Object::String(s) => s.borrow().clone(),
                            _ => return Err(#err),
                        },
                        _ => return Err(#err),
                    };
                },
                pass,
                static_type: quote! { ndc_core::StaticType::String },
            }
        }

        NdcType::I64 => {
            let err = arg_error(position, "int");
            VmInputArg {
                extract: quote! {
                    let #temp = match #raw {
                        ndc_vm::value::Value::Int(i) => *i,
                        _ => return Err(#err),
                    };
                },
                pass: quote! { #temp },
                static_type: quote! { ndc_core::StaticType::Int },
            }
        }

        NdcType::Usize => {
            let err = arg_error(position, "int");
            VmInputArg {
                extract: quote! {
                    let #temp = match #raw {
                        ndc_vm::value::Value::Int(i) => *i as usize,
                        _ => return Err(#err),
                    };
                },
                pass: quote! { #temp },
                static_type: quote! { ndc_core::StaticType::Int },
            }
        }

        NdcType::BigIntRef => {
            let err = arg_error(position, "int");
            VmInputArg {
                extract: quote! {
                    let #temp = {
                        let num = #raw.to_number().ok_or_else(|| #err)?;
                        match num {
                            ndc_core::num::Number::Int(i) => i.to_bigint(),
                            _ => return Err(#err),
                        }
                    };
                },
                pass: quote! { &#temp },
                static_type: quote! { ndc_core::StaticType::Int },
            }
        }

        NdcType::BigRationalRef => {
            let err = arg_error(position, "rational");
            VmInputArg {
                extract: quote! {
                    let #temp = {
                        let num = #raw.to_number().ok_or_else(|| #err)?;
                        match num {
                            ndc_core::num::Number::Rational(r) => *r,
                            _ => return Err(#err),
                        }
                    };
                },
                pass: quote! { &#temp },
                static_type: quote! { ndc_core::StaticType::Rational },
            }
        }

        NdcType::Complex64 => {
            let err = arg_error(position, "complex");
            VmInputArg {
                extract: quote! {
                    let #temp = {
                        let num = #raw.to_number().ok_or_else(|| #err)?;
                        match num {
                            ndc_core::num::Number::Complex(c) => c,
                            _ => return Err(#err),
                        }
                    };
                },
                pass: quote! { #temp },
                static_type: quote! { ndc_core::StaticType::Complex },
            }
        }

        NdcType::MutString => {
            let err = arg_error(position, "string");
            let rc = format_ident!("{temp}_rc");
            let guard = format_ident!("{temp}_guard");
            VmInputArg {
                extract: quote! {
                    let ndc_vm::value::Value::Object(ref #rc) = *#raw else {
                        return Err(#err);
                    };
                    let ndc_vm::value::Object::String(ref #rc) = *#rc.as_ref() else {
                        return Err(#err);
                    };
                    let mut #guard = #rc.borrow_mut();
                },
                pass: quote! { &mut *#guard },
                static_type: quote! { ndc_core::StaticType::String },
            }
        }

        NdcType::StringRepr | NdcType::MutStringRepr => {
            let err = arg_error(position, "string");
            let obj = format_ident!("{temp}_obj");
            let inner = format_ident!("{temp}_inner");
            let pass = if ndc_type == NdcType::MutStringRepr {
                quote! { &mut #temp }
            } else {
                quote! { &#temp }
            };
            VmInputArg {
                extract: quote! {
                    let ndc_vm::value::Value::Object(ref #obj) = *#raw else {
                        return Err(#err);
                    };
                    let ndc_vm::value::Object::String(ref #inner) = *#obj.as_ref() else {
                        return Err(#err);
                    };
                    let mut #temp = #inner.clone();
                },
                pass,
                static_type: quote! { ndc_core::StaticType::String },
            }
        }

        NdcType::MutVecOfValue => refcell_collection_arg(
            position,
            "list",
            "List",
            true,
            quote! { ndc_core::StaticType::List(Box::new(ndc_core::StaticType::Any)) },
        ),

        NdcType::MutHashMap => {
            let err = arg_error(position, "map");
            let rc = format_ident!("{temp}_rc");
            let guard = format_ident!("{temp}_guard");
            VmInputArg {
                extract: quote! {
                    let ndc_vm::value::Value::Object(ref #rc) = *#raw else {
                        return Err(#err);
                    };
                    let ndc_vm::value::Object::Map { entries: ref #rc, .. } = *#rc.as_ref() else {
                        return Err(#err);
                    };
                    let mut #guard = #rc.borrow_mut();
                },
                pass: quote! { &mut *#guard },
                static_type: quote! {
                    ndc_core::StaticType::Map {
                        key: Box::new(ndc_core::StaticType::Any),
                        value: Box::new(ndc_core::StaticType::Any),
                    }
                },
            }
        }

        NdcType::RefHashMap => {
            let err = arg_error(position, "map");
            let rc = format_ident!("{temp}_rc");
            let guard = format_ident!("{temp}_guard");
            VmInputArg {
                extract: quote! {
                    let ndc_vm::value::Value::Object(ref #rc) = *#raw else {
                        return Err(#err);
                    };
                    let ndc_vm::value::Object::Map { entries: ref #rc, .. } = *#rc.as_ref() else {
                        return Err(#err);
                    };
                    let #guard = #rc.borrow();
                },
                pass: quote! { &*#guard },
                static_type: quote! {
                    ndc_core::StaticType::Map {
                        key: Box::new(ndc_core::StaticType::Any),
                        value: Box::new(ndc_core::StaticType::Any),
                    }
                },
            }
        }

        NdcType::MutMinHeap => refcell_collection_arg(
            position,
            "min heap",
            "MinHeap",
            true,
            quote! { ndc_core::StaticType::MinHeap(Box::new(ndc_core::StaticType::Any)) },
        ),

        NdcType::MutMaxHeap => refcell_collection_arg(
            position,
            "max heap",
            "MaxHeap",
            true,
            quote! { ndc_core::StaticType::MaxHeap(Box::new(ndc_core::StaticType::Any)) },
        ),

        NdcType::MutVecDeque => refcell_collection_arg(
            position,
            "deque",
            "Deque",
            true,
            quote! { ndc_core::StaticType::Deque(Box::new(ndc_core::StaticType::Any)) },
        ),

        NdcType::RefVecDeque => refcell_collection_arg(
            position,
            "deque",
            "Deque",
            false,
            quote! { ndc_core::StaticType::Deque(Box::new(ndc_core::StaticType::Any)) },
        ),

        NdcType::VmValue => VmInputArg {
            extract: quote! {},
            pass: quote! { #raw.clone() },
            static_type: quote! { ndc_core::StaticType::Any },
        },

        NdcType::SeqValue => VmInputArg {
            extract: quote! {},
            pass: quote! { #raw.clone() },
            static_type: quote! { ndc_core::StaticType::Sequence(Box::new(ndc_core::StaticType::Any)) },
        },

        NdcType::MapValue => VmInputArg {
            extract: quote! {},
            pass: quote! { #raw.clone() },
            static_type: quote! {
                ndc_core::StaticType::Map {
                    key: Box::new(ndc_core::StaticType::Any),
                    value: Box::new(ndc_core::StaticType::Any),
                }
            },
        },

        NdcType::SliceOfValue => {
            let vec = format_ident!("{temp}_vec");
            let err = arg_error(position, "list");
            VmInputArg {
                extract: quote! {
                    let #vec: Vec<ndc_vm::value::Value> = match #raw {
                        ndc_vm::value::Value::Object(obj) => match obj.as_ref() {
                            ndc_vm::value::Object::List(list) => list.borrow().clone(),
                            ndc_vm::value::Object::Tuple(tuple) => tuple.clone(),
                            _ => return Err(#err),
                        },
                        _ => return Err(#err),
                    };
                },
                pass: quote! { &#vec },
                static_type: quote! {
                    ndc_core::StaticType::List(Box::new(ndc_core::StaticType::Any))
                },
            }
        }

        NdcType::MutVmCallable => {
            let err = arg_error(position, "function");
            VmInputArg {
                extract: quote! {
                    let mut #temp = match #raw {
                        ndc_vm::value::Value::Object(_obj) => match _obj.as_ref() {
                            ndc_vm::value::Object::Function(f) => ndc_vm::VmCallable {
                                function: f.clone(),
                                vm: _vm,
                            },
                            _ => return Err(#err),
                        },
                        _ => return Err(#err),
                    };
                },
                pass: quote! { &mut #temp },
                static_type: quote! {
                    ndc_core::StaticType::Function {
                        parameters: None,
                        return_type: Box::new(ndc_core::StaticType::Any),
                    }
                },
            }
        }

        // Return-only types — not valid as function parameters
        NdcType::BigInt | NdcType::BigRational => return None,
    };

    Some(result)
}

/// Try to generate the return expression (producing `Result<VmValue, VmError>`)
/// and the `StaticType` token for the return type.
///
/// Returns `None` for unsupported types.
pub fn try_vm_return(output: &syn::ReturnType) -> Option<(TokenStream, TokenStream)> {
    match output {
        syn::ReturnType::Default => Some((
            quote! { Ok(ndc_vm::value::Value::unit()) },
            quote! { ndc_core::StaticType::Tuple(vec![]) },
        )),
        syn::ReturnType::Type(_, ty) => try_vm_return_type(ty),
    }
}

fn try_vm_return_type(ty: &syn::Type) -> Option<(TokenStream, TokenStream)> {
    // Handle Result<T> wrapper — unwrap and recurse into the inner type
    if let Some(inner) = unwrap_result(ty) {
        let (inner_code, inner_type) = try_vm_return_inner(inner)?;
        return Some((
            quote! {
                let result = result.map_err(|e| ndc_vm::error::VmError::native(e.to_string()))?;
                #inner_code
            },
            inner_type,
        ));
    }

    vm_return_for_classified(ty)
}

/// Like `try_vm_return_type` but also handles `()` (unit), which only makes
/// sense as the Ok type inside a `Result<()>`.
fn try_vm_return_inner(ty: &syn::Type) -> Option<(TokenStream, TokenStream)> {
    if let syn::Type::Tuple(t) = ty {
        if t.elems.is_empty() {
            return Some((
                quote! { Ok(ndc_vm::value::Value::unit()) },
                quote! { ndc_core::StaticType::Tuple(vec![]) },
            ));
        }
    }
    vm_return_for_classified(ty)
}

fn vm_return_for_classified(ty: &syn::Type) -> Option<(TokenStream, TokenStream)> {
    let ndc_type = classify(ty)?;
    match ndc_type {
        NdcType::VmValue | NdcType::SeqValue => {
            Some((quote! { Ok(result) }, quote! { ndc_core::StaticType::Any }))
        }
        NdcType::MapValue => Some((
            quote! { Ok(result) },
            quote! {
                ndc_core::StaticType::Map {
                    key: Box::new(ndc_core::StaticType::Any),
                    value: Box::new(ndc_core::StaticType::Any),
                }
            },
        )),
        NdcType::Number | NdcType::NumberRef => Some((
            quote! { Ok(ndc_vm::value::Value::from_number(result)) },
            quote! { ndc_core::StaticType::Number },
        )),
        NdcType::F64 => Some((
            quote! { Ok(ndc_vm::value::Value::Float(result)) },
            quote! { ndc_core::StaticType::Float },
        )),
        NdcType::Bool => Some((
            quote! { Ok(ndc_vm::value::Value::Bool(result)) },
            quote! { ndc_core::StaticType::Bool },
        )),
        NdcType::StrRef => Some((
            quote! { Ok(ndc_vm::value::Value::string(result.to_owned())) },
            quote! { ndc_core::StaticType::String },
        )),
        NdcType::String => Some((
            quote! { Ok(ndc_vm::value::Value::string(result)) },
            quote! { ndc_core::StaticType::String },
        )),
        NdcType::I64 => Some((
            quote! { Ok(ndc_vm::value::Value::Int(result)) },
            quote! { ndc_core::StaticType::Int },
        )),
        NdcType::Usize => Some((
            quote! { Ok(ndc_vm::value::Value::Int(result as i64)) },
            quote! { ndc_core::StaticType::Int },
        )),
        NdcType::BigInt => Some((
            quote! {
                Ok(ndc_vm::value::Value::from_number(
                    ndc_core::num::Number::Int(
                        ndc_core::int::Int::BigInt(result).simplified()
                    )
                ))
            },
            quote! { ndc_core::StaticType::Int },
        )),
        NdcType::BigRational => Some((
            quote! {
                Ok(ndc_vm::value::Value::from_number(
                    ndc_core::num::Number::Rational(Box::new(result))
                ))
            },
            quote! { ndc_core::StaticType::Rational },
        )),
        // Input-only types — not valid as return types
        _ => None,
    }
}
