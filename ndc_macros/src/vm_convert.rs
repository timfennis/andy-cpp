//! Code generation for VM-native function parameter extraction and return conversion.
//!
//! Each `try_vm_*` function returns `None` when the type has no conversion to
//! or from a VM value, and `export_module` then rejects the function with a
//! compile error.

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
    /// What the argument holds a borrow of while the native runs.
    pub borrow: Borrow,
}

/// What an argument holds a borrow of while the native runs. One that's
/// only read is copied instead when the call changes the same value as
/// another argument, so `m.remove(m)` reads a copy of `m` while it removes
/// from it. A native that takes a callback holds none, as the callback is
/// the program's code, which may use the same value.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Borrow {
    /// It's copied out, or isn't in a `RefCell`.
    None,
    /// Its `RefCell` is borrowed to be read, or copied when `vm_alias{N}`
    /// says an argument the call changes is the same value.
    Read,
    /// Its `RefCell` is borrowed to be changed.
    Write,
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

/// The error for an argument a call can't borrow, as another call that's
/// still running is changing it: `sort_by` changes the list whose elements
/// its comparator gets, so the comparator can't use that list.
fn in_use_error(position: usize, expected: &str) -> TokenStream {
    quote! {
        ndc_vm::error::VmError::native(format!(
            "arg {}: the {} can't be used here, as a call that's still running is changing it, as `sort_by` changes the list its comparator compares the elements of",
            #position, #expected
        ))
    }
}

/// Generate extraction code for a RefCell-based collection parameter.
///
/// Covers List, Deque, `MinHeap`, `MaxHeap` (but not Map, which has different destructuring).
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

    let in_use = in_use_error(position, expected);
    let (borrow, pass) = if mutable {
        (
            quote! { let mut #guard = #rc.try_borrow_mut().map_err(|_| #in_use)?; },
            quote! { &mut *#guard },
        )
    } else {
        read_borrow(position, &rc, &in_use)
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
        borrow: if mutable { Borrow::Write } else { Borrow::Read },
    }
}

/// The code that borrows the `RefCell` `rc` of the argument at `position`
/// to be read, and what's passed for it: a copy of what it holds when
/// `vm_alias{position}` says the call changes it as another argument.
fn read_borrow(
    position: usize,
    rc: &proc_macro2::Ident,
    in_use: &TokenStream,
) -> (TokenStream, TokenStream) {
    let alias = format_ident!("vm_alias{position}");
    let guard = format_ident!("vm_temp{position}_guard");
    let copy = format_ident!("vm_temp{position}_copy");
    let read = format_ident!("vm_temp{position}_read");
    (
        quote! {
            let #copy;
            let #guard;
            let #read = if #alias {
                #copy = (*#rc.try_borrow().map_err(|_| #in_use)?).clone();
                &#copy
            } else {
                #guard = #rc.try_borrow().map_err(|_| #in_use)?;
                &*#guard
            };
        },
        quote! { #read },
    )
}

/// Try to generate extraction code for a single parameter type.
///
/// Returns `None` for types that cannot be expressed in VM-native terms.
pub fn try_vm_input(ty: &syn::Type, position: usize) -> Option<VmInputArg> {
    let ndc_type = classify(ty)?;
    let raw = format_ident!("vm_raw{position}");
    let temp = format_ident!("vm_temp{position}");

    let result = match ndc_type {
        NdcType::Number => {
            let err = arg_error(position, "number");
            VmInputArg {
                extract: quote! {
                    let #temp = #raw.to_number().ok_or_else(|| #err)?;
                },
                pass: quote! { #temp },
                static_type: quote! { ndc_core::StaticType::Number },
                borrow: Borrow::None,
            }
        }

        NdcType::NumberRef => {
            let err = arg_error(position, "number");
            VmInputArg {
                extract: quote! {
                    let #temp = #raw.as_number().ok_or_else(|| #err)?;
                },
                pass: quote! { #temp },
                static_type: quote! { ndc_core::StaticType::Number },
                borrow: Borrow::None,
            }
        }

        NdcType::F64 => {
            let err = arg_error(position, "float");
            VmInputArg {
                extract: quote! {
                    let ndc_vm::value::Value::Float(#temp) = #raw else {
                        return Err(#err);
                    };
                    let #temp = *#temp;
                },
                pass: quote! { #temp },
                static_type: quote! { ndc_core::StaticType::Float },
                borrow: Borrow::None,
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
                borrow: Borrow::None,
            }
        }

        NdcType::String | NdcType::StrRef => {
            let err = arg_error(position, "string");
            let in_use = in_use_error(position, "string");
            let pass = if ndc_type == NdcType::StrRef {
                quote! { &#temp }
            } else {
                quote! { #temp }
            };
            VmInputArg {
                extract: quote! {
                    let #temp = match #raw {
                        ndc_vm::value::Value::Object(obj) => match obj.as_ref() {
                            ndc_vm::value::Object::String(s) => s.try_borrow().map_err(|_| #in_use)?.clone(),
                            _ => return Err(#err),
                        },
                        _ => return Err(#err),
                    };
                },
                pass,
                static_type: quote! { ndc_core::StaticType::String },
                borrow: Borrow::None,
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
                borrow: Borrow::None,
            }
        }

        NdcType::Usize => {
            let err = arg_error(position, "int");
            VmInputArg {
                extract: quote! {
                    let #temp = match #raw {
                        ndc_vm::value::Value::Int(value) => usize::try_from(*value).map_err(|_| {
                            ndc_vm::error::VmError::native(format!(
                                "arg {}: expected a non-negative integer, but the value was negative",
                                #position,
                            ))
                        })?,
                        _ => return Err(#err),
                    };
                },
                pass: quote! { #temp },
                static_type: quote! { ndc_core::StaticType::Int },
                borrow: Borrow::None,
            }
        }

        NdcType::BigIntRef => {
            let err = arg_error(position, "int");
            VmInputArg {
                extract: quote! {
                    let #temp = match #raw {
                        ndc_vm::value::Value::Int(value) => num::BigInt::from(*value),
                        _ => return Err(#err),
                    };
                },
                pass: quote! { &#temp },
                static_type: quote! { ndc_core::StaticType::Int },
                borrow: Borrow::None,
            }
        }

        NdcType::BigRationalRef => {
            let err = arg_error(position, "an exact integer or rational Number");
            VmInputArg {
                extract: quote! {
                    let #temp = {
                        let num = #raw.as_number().ok_or_else(|| #err)?;
                        num.to_rational().ok_or_else(|| #err)?
                    };
                },
                pass: quote! { &#temp },
                static_type: quote! { ndc_core::StaticType::Number },
                borrow: Borrow::None,
            }
        }

        NdcType::Complex64 => {
            let err = arg_error(position, "a Number");
            VmInputArg {
                extract: quote! {
                    let #temp = {
                        let num = #raw.as_number().ok_or_else(|| #err)?;
                        num.to_complex()
                    };
                },
                pass: quote! { #temp },
                static_type: quote! { ndc_core::StaticType::Number },
                borrow: Borrow::None,
            }
        }

        NdcType::MutString => {
            let err = arg_error(position, "string");
            let in_use = in_use_error(position, "string");
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
                    let mut #guard = #rc.try_borrow_mut().map_err(|_| #in_use)?;
                },
                pass: quote! { &mut *#guard },
                static_type: quote! { ndc_core::StaticType::String },
                borrow: Borrow::Write,
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
                borrow: Borrow::None,
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
            let in_use = in_use_error(position, "map");
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
                    let mut #guard = #rc.try_borrow_mut().map_err(|_| #in_use)?;
                },
                pass: quote! { &mut *#guard },
                static_type: quote! {
                    ndc_core::StaticType::Map {
                        key: Box::new(ndc_core::StaticType::Any),
                        value: Box::new(ndc_core::StaticType::Any),
                    }
                },
                borrow: Borrow::Write,
            }
        }

        NdcType::RefHashMap => {
            let err = arg_error(position, "map");
            let in_use = in_use_error(position, "map");
            let rc = format_ident!("{temp}_rc");
            let (borrow, pass) = read_borrow(position, &rc, &in_use);
            VmInputArg {
                extract: quote! {
                    let ndc_vm::value::Value::Object(ref #rc) = *#raw else {
                        return Err(#err);
                    };
                    let ndc_vm::value::Object::Map { entries: ref #rc, .. } = *#rc.as_ref() else {
                        return Err(#err);
                    };
                    #borrow
                },
                pass,
                static_type: quote! {
                    ndc_core::StaticType::Map {
                        key: Box::new(ndc_core::StaticType::Any),
                        value: Box::new(ndc_core::StaticType::Any),
                    }
                },
                borrow: Borrow::Read,
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
            borrow: Borrow::None,
        },

        NdcType::SliceOfValue => {
            let vec = format_ident!("{temp}_vec");
            let err = arg_error(position, "list");
            let in_use = in_use_error(position, "list");
            VmInputArg {
                extract: quote! {
                    let #vec: Vec<ndc_vm::value::Value> = match #raw {
                        ndc_vm::value::Value::Object(obj) => match obj.as_ref() {
                            ndc_vm::value::Object::List(list) => list.try_borrow().map_err(|_| #in_use)?.clone(),
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
                borrow: Borrow::None,
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
                borrow: Borrow::None,
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
    if let syn::Type::Tuple(t) = ty
        && t.elems.is_empty()
    {
        return Some((
            quote! { Ok(ndc_vm::value::Value::unit()) },
            quote! { ndc_core::StaticType::Tuple(vec![]) },
        ));
    }
    vm_return_for_classified(ty)
}

fn vm_return_for_classified(ty: &syn::Type) -> Option<(TokenStream, TokenStream)> {
    let ndc_type = classify(ty)?;
    match ndc_type {
        NdcType::VmValue => Some((quote! { Ok(result) }, quote! { ndc_core::StaticType::Any })),
        NdcType::Number | NdcType::NumberRef => Some((
            quote! { Ok(ndc_vm::value::Value::number(result)) },
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
                Ok(ndc_vm::value::Value::number(
                    ndc_vm::value::AdvancedNumber::Int(result)
                ))
            },
            quote! { ndc_core::StaticType::Number },
        )),
        NdcType::BigRational => Some((
            quote! {
                Ok(ndc_vm::value::Value::number(
                    ndc_vm::value::AdvancedNumber::rational(result)
                ))
            },
            quote! { ndc_core::StaticType::Number },
        )),
        // Input-only types — not valid as return types
        _ => None,
    }
}
