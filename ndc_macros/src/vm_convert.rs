//! Helpers for generating `vm_native` closures from Rust function signatures.
//!
//! Each `try_vm_*` function returns `None` when the type cannot be represented
//! without the interpreter bridge, causing `export_module` to silently skip
//! vm_native generation for that function.

use crate::r#match::{
    is_ndc_vm_value, is_ref, is_ref_mut, is_ref_mut_of_hashmap_of_ndc_vm_value,
    is_ref_mut_of_max_heap, is_ref_mut_of_min_heap, is_ref_mut_of_vec_of_ndc_vm_value,
    is_ref_mut_of_vecdeque_of_ndc_vm_value, is_ref_of_bigint, is_ref_of_hashmap_of_ndc_vm_value,
    is_ref_of_slice_of_ndc_vm_value, is_ref_of_slice_of_value, is_ref_of_vecdeque_of_ndc_vm_value,
    is_str_ref, is_string, path_ends_with,
};
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
/// Supported: `Number`, `&Number`, `f64`, `bool`, `&str`, `String`, `i64`,
/// `&Value`, `&mut String`, `StringRepr`, `&mut StringRepr`, `&mut Sequence`.
/// Returns `None` for everything else (BigInt, owned Sequence, …).
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
                    .ok_or_else(|| ndc_vm::error::VmError::native(format!("arg {}: expected number, got {}", #position, #raw.static_type())))?;
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
                    .ok_or_else(|| ndc_vm::error::VmError::native(format!("arg {}: expected float, got {}", #position, #raw.static_type())))?;
            },
            pass: quote! { #temp },
            static_type: quote! { ndc_interpreter::function::StaticType::Float },
        });
    }

    if path_ends_with(ty, "bool") {
        return Some(VmInputArg {
            extract: quote! {
                let ndc_vm::value::Value::Bool(#temp) = #raw else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected bool, got {}", #position, #raw.static_type())));
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
                        _ => return Err(ndc_vm::error::VmError::native(format!("arg {}: expected string, got {}", #position, #raw.static_type()))),
                    },
                    _ => return Err(ndc_vm::error::VmError::native(format!("arg {}: expected string, got {}", #position, #raw.static_type()))),
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
                    _ => return Err(ndc_vm::error::VmError::native(format!("arg {}: expected int, got {}", #position, #raw.static_type()))),
                };
            },
            pass: quote! { #temp },
            static_type: quote! { ndc_interpreter::function::StaticType::Int },
        });
    }

    if path_ends_with(ty, "usize") {
        return Some(VmInputArg {
            extract: quote! {
                let #temp = match #raw {
                    ndc_vm::value::Value::Int(i) => *i as usize,
                    _ => return Err(ndc_vm::error::VmError::native(format!("arg {}: expected int, got {}", #position, #raw.static_type()))),
                };
            },
            pass: quote! { #temp },
            static_type: quote! { ndc_interpreter::function::StaticType::Int },
        });
    }

    // &BigInt — extract from Number::Int, converting Int64 to BigInt as needed.
    if is_ref_of_bigint(ty) {
        return Some(VmInputArg {
            extract: quote! {
                let #temp = {
                    let num = #raw.to_number()
                        .ok_or_else(|| ndc_vm::error::VmError::native(format!("arg {}: expected int, got {}", #position, #raw.static_type())))?;
                    match num {
                        ndc_interpreter::num::Number::Int(i) => i.to_bigint(),
                        _ => return Err(ndc_vm::error::VmError::native(format!("arg {}: expected int, got {}", #position, #raw.static_type()))),
                    }
                };
            },
            pass: quote! { &#temp },
            static_type: quote! { ndc_interpreter::function::StaticType::Int },
        });
    }

    // &BigRational — extract from Number::Rational.
    if path_ends_with(ty, "BigRational") && is_ref(ty) {
        return Some(VmInputArg {
            extract: quote! {
                let #temp = {
                    let num = #raw.to_number()
                        .ok_or_else(|| ndc_vm::error::VmError::native(format!("arg {}: expected rational, got {}", #position, #raw.static_type())))?;
                    match num {
                        ndc_interpreter::num::Number::Rational(r) => *r,
                        _ => return Err(ndc_vm::error::VmError::native(format!("arg {}: expected rational, got {}", #position, #raw.static_type()))),
                    }
                };
            },
            pass: quote! { &#temp },
            static_type: quote! { ndc_interpreter::function::StaticType::Rational },
        });
    }

    // Complex64 (owned) — extract from Number::Complex.
    if path_ends_with(ty, "Complex64") && !is_ref(ty) && !is_ref_mut(ty) {
        return Some(VmInputArg {
            extract: quote! {
                let #temp = {
                    let num = #raw.to_number()
                        .ok_or_else(|| ndc_vm::error::VmError::native(format!("arg {}: expected complex, got {}", #position, #raw.static_type())))?;
                    match num {
                        ndc_interpreter::num::Number::Complex(c) => c,
                        _ => return Err(ndc_vm::error::VmError::native(format!("arg {}: expected complex, got {}", #position, #raw.static_type()))),
                    }
                };
            },
            pass: quote! { #temp },
            static_type: quote! { ndc_interpreter::function::StaticType::Complex },
        });
    }

    // Value (owned interpreter type) — convert any VmValue to an interpreter Value.
    if path_ends_with(ty, "Value") && !is_ref(ty) && !is_ref_mut(ty) && !is_ndc_vm_value(ty) {
        return Some(VmInputArg {
            extract: quote! {
                let #temp = ndc_interpreter::vm_bridge::vm_to_interp(#raw);
            },
            pass: quote! { #temp },
            static_type: quote! { ndc_interpreter::function::StaticType::Any },
        });
    }

    // &Value — convert any VmValue to an interpreter Value on the fly.
    if path_ends_with(ty, "Value") && is_ref(ty) {
        return Some(VmInputArg {
            extract: quote! {
                let #temp = ndc_interpreter::vm_bridge::vm_to_interp(#raw);
            },
            pass: quote! { &#temp },
            static_type: quote! { ndc_interpreter::function::StaticType::Any },
        });
    }

    // &mut String — mutate the inner String through the Rc<RefCell<String>> stored in
    // Object::String. Interior mutability means the original VmValue sees the change.
    if is_ref_mut(ty) && is_string(ty) {
        let rc_ident = format_ident!("{temp}_rc");
        let guard_ident = format_ident!("{temp}_guard");
        return Some(VmInputArg {
            extract: quote! {
                let ndc_vm::value::Value::Object(ref #rc_ident) = *#raw else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected string, got {}", #position, #raw.static_type())));
                };
                let ndc_vm::value::Object::String(ref #rc_ident) = *#rc_ident.as_ref() else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected string, got {}", #position, #raw.static_type())));
                };
                let mut #guard_ident = #rc_ident.borrow_mut();
            },
            pass: quote! { &mut *#guard_ident },
            static_type: quote! { ndc_interpreter::function::StaticType::String },
        });
    }

    // StringRepr (&Rc<RefCell<String>>) / &mut StringRepr — clone the Rc so both
    // the VM value and the Rc local point at the same allocation. Any interior
    // mutation (via borrow_mut) is immediately visible through the original VmValue.
    if path_ends_with(ty, "StringRepr") {
        let obj_ident = format_ident!("{temp}_obj");
        let inner_ident = format_ident!("{temp}_inner");
        let pass = if is_ref_mut(ty) {
            quote! { &mut #temp }
        } else {
            quote! { &#temp }
        };
        return Some(VmInputArg {
            extract: quote! {
                let ndc_vm::value::Value::Object(ref #obj_ident) = *#raw else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected string, got {}", #position, #raw.static_type())));
                };
                let ndc_vm::value::Object::String(ref #inner_ident) = *#obj_ident.as_ref() else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected string, got {}", #position, #raw.static_type())));
                };
                let mut #temp = #inner_ident.clone();
            },
            pass,
            static_type: quote! { ndc_interpreter::function::StaticType::String },
        });
    }

    // &mut Sequence — convert VmValue to an interpreter Sequence. Functions that take
    // &mut Sequence here only iterate (join, unlines, …) and do not mutate back, so
    // no sync step is needed.
    if path_ends_with(ty, "Sequence") && is_ref_mut(ty) {
        let interp_ident = format_ident!("{temp}_interp");
        return Some(VmInputArg {
            extract: quote! {
                let #interp_ident = ndc_interpreter::vm_bridge::vm_to_interp(#raw);
                let ndc_interpreter::value::Value::Sequence(mut #temp) = #interp_ident else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected sequence, got {}", #position, #raw.static_type())));
                };
            },
            pass: quote! { &mut #temp },
            static_type: quote! {
                ndc_interpreter::function::StaticType::Sequence(Box::new(
                    ndc_interpreter::function::StaticType::Any
                ))
            },
        });
    }

    // &Sequence (immutable) — same conversion as &mut Sequence but passed as a shared ref.
    if path_ends_with(ty, "Sequence") && is_ref(ty) {
        let interp_ident = format_ident!("{temp}_interp");
        return Some(VmInputArg {
            extract: quote! {
                let #interp_ident = ndc_interpreter::vm_bridge::vm_to_interp(#raw);
                let ndc_interpreter::value::Value::Sequence(#temp) = #interp_ident else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected sequence, got {}", #position, #raw.static_type())));
                };
            },
            pass: quote! { &#temp },
            static_type: quote! {
                ndc_interpreter::function::StaticType::Sequence(Box::new(
                    ndc_interpreter::function::StaticType::Any
                ))
            },
        });
    }

    // &mut Vec<ndc_vm::value::Value> — mutate the inner list via Rc<RefCell<Vec<VmValue>>>.
    // Interior mutability means the original VmValue sees changes immediately; no external
    // sync step is needed on the VM path.
    if is_ref_mut_of_vec_of_ndc_vm_value(ty) {
        let rc_ident = format_ident!("{temp}_rc");
        let guard_ident = format_ident!("{temp}_guard");
        return Some(VmInputArg {
            extract: quote! {
                let ndc_vm::value::Value::Object(ref #rc_ident) = *#raw else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected list, got {}", #position, #raw.static_type())));
                };
                let ndc_vm::value::Object::List(ref #rc_ident) = *#rc_ident.as_ref() else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected list, got {}", #position, #raw.static_type())));
                };
                let mut #guard_ident = #rc_ident.borrow_mut();
            },
            pass: quote! { &mut *#guard_ident },
            static_type: quote! {
                ndc_interpreter::function::StaticType::List(Box::new(
                    ndc_interpreter::function::StaticType::Any
                ))
            },
        });
    }

    // &mut HashMap<ndc_vm::value::Value, ndc_vm::value::Value> — mutate map entries via RefCell.
    if is_ref_mut_of_hashmap_of_ndc_vm_value(ty) {
        let rc_ident = format_ident!("{temp}_rc");
        let guard_ident = format_ident!("{temp}_guard");
        return Some(VmInputArg {
            extract: quote! {
                let ndc_vm::value::Value::Object(ref #rc_ident) = *#raw else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected map, got {}", #position, #raw.static_type())));
                };
                let ndc_vm::value::Object::Map { entries: ref #rc_ident, .. } = *#rc_ident.as_ref() else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected map, got {}", #position, #raw.static_type())));
                };
                let mut #guard_ident = #rc_ident.borrow_mut();
            },
            pass: quote! { &mut *#guard_ident },
            static_type: quote! {
                ndc_interpreter::function::StaticType::Map {
                    key: Box::new(ndc_interpreter::function::StaticType::Any),
                    value: Box::new(ndc_interpreter::function::StaticType::Any),
                }
            },
        });
    }

    // &HashMap<ndc_vm::value::Value, ndc_vm::value::Value> — immutable borrow of map entries.
    if is_ref_of_hashmap_of_ndc_vm_value(ty) {
        let rc_ident = format_ident!("{temp}_rc");
        let guard_ident = format_ident!("{temp}_guard");
        return Some(VmInputArg {
            extract: quote! {
                let ndc_vm::value::Value::Object(ref #rc_ident) = *#raw else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected map, got {}", #position, #raw.static_type())));
                };
                let ndc_vm::value::Object::Map { entries: ref #rc_ident, .. } = *#rc_ident.as_ref() else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected map, got {}", #position, #raw.static_type())));
                };
                let #guard_ident = #rc_ident.borrow();
            },
            pass: quote! { &*#guard_ident },
            static_type: quote! {
                ndc_interpreter::function::StaticType::Map {
                    key: Box::new(ndc_interpreter::function::StaticType::Any),
                    value: Box::new(ndc_interpreter::function::StaticType::Any),
                }
            },
        });
    }

    // &mut BinaryHeap<Reverse<OrdValue>> — min-heap (uses Reverse<OrdValue> for ordering).
    if is_ref_mut_of_min_heap(ty) {
        let rc_ident = format_ident!("{temp}_rc");
        let guard_ident = format_ident!("{temp}_guard");
        return Some(VmInputArg {
            extract: quote! {
                let ndc_vm::value::Value::Object(ref #rc_ident) = *#raw else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected min heap, got {}", #position, #raw.static_type())));
                };
                let ndc_vm::value::Object::MinHeap(ref #rc_ident) = *#rc_ident.as_ref() else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected min heap, got {}", #position, #raw.static_type())));
                };
                let mut #guard_ident = #rc_ident.borrow_mut();
            },
            pass: quote! { &mut *#guard_ident },
            static_type: quote! {
                ndc_interpreter::function::StaticType::MinHeap(Box::new(
                    ndc_interpreter::function::StaticType::Any
                ))
            },
        });
    }

    // &mut BinaryHeap<OrdValue> — max-heap.
    if is_ref_mut_of_max_heap(ty) {
        let rc_ident = format_ident!("{temp}_rc");
        let guard_ident = format_ident!("{temp}_guard");
        return Some(VmInputArg {
            extract: quote! {
                let ndc_vm::value::Value::Object(ref #rc_ident) = *#raw else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected max heap, got {}", #position, #raw.static_type())));
                };
                let ndc_vm::value::Object::MaxHeap(ref #rc_ident) = *#rc_ident.as_ref() else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected max heap, got {}", #position, #raw.static_type())));
                };
                let mut #guard_ident = #rc_ident.borrow_mut();
            },
            pass: quote! { &mut *#guard_ident },
            static_type: quote! {
                ndc_interpreter::function::StaticType::MaxHeap(Box::new(
                    ndc_interpreter::function::StaticType::Any
                ))
            },
        });
    }

    // &mut VecDeque<ndc_vm::value::Value> — mutate via RefCell inside VmObject::Deque.
    if is_ref_mut_of_vecdeque_of_ndc_vm_value(ty) {
        let rc_ident = format_ident!("{temp}_rc");
        let guard_ident = format_ident!("{temp}_guard");
        return Some(VmInputArg {
            extract: quote! {
                let ndc_vm::value::Value::Object(ref #rc_ident) = *#raw else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected deque, got {}", #position, #raw.static_type())));
                };
                let ndc_vm::value::Object::Deque(ref #rc_ident) = *#rc_ident.as_ref() else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected deque, got {}", #position, #raw.static_type())));
                };
                let mut #guard_ident = #rc_ident.borrow_mut();
            },
            pass: quote! { &mut *#guard_ident },
            static_type: quote! {
                ndc_interpreter::function::StaticType::Deque(Box::new(
                    ndc_interpreter::function::StaticType::Any
                ))
            },
        });
    }

    // &VecDeque<ndc_vm::value::Value> — immutable borrow via RefCell inside VmObject::Deque.
    if is_ref_of_vecdeque_of_ndc_vm_value(ty) {
        let rc_ident = format_ident!("{temp}_rc");
        let guard_ident = format_ident!("{temp}_guard");
        return Some(VmInputArg {
            extract: quote! {
                let ndc_vm::value::Value::Object(ref #rc_ident) = *#raw else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected deque, got {}", #position, #raw.static_type())));
                };
                let ndc_vm::value::Object::Deque(ref #rc_ident) = *#rc_ident.as_ref() else {
                    return Err(ndc_vm::error::VmError::native(format!("arg {}: expected deque, got {}", #position, #raw.static_type())));
                };
                let #guard_ident = #rc_ident.borrow();
            },
            pass: quote! { &*#guard_ident },
            static_type: quote! {
                ndc_interpreter::function::StaticType::Deque(Box::new(
                    ndc_interpreter::function::StaticType::Any
                ))
            },
        });
    }

    // ndc_vm::value::Value — pass through directly (zero conversion on the VM path).
    // The interpreter path uses call_vm_native which handles the interp→vm conversion.
    // Only matches the fully-qualified path ndc_vm::value::Value.
    if is_ndc_vm_value(ty) {
        return Some(VmInputArg {
            extract: quote! {},
            pass: quote! { #raw.clone() },
            static_type: quote! { ndc_interpreter::function::StaticType::Any },
        });
    }

    // &[ndc_vm::value::Value] — pass a borrow of the inner Vec directly, zero conversion.
    // Must come before the &[Value] check because is_ref_of_slice_of_value also matches this type.
    if is_ref_of_slice_of_ndc_vm_value(ty) {
        let vec_ident = format_ident!("{temp}_vec");
        return Some(VmInputArg {
            extract: quote! {
                let #vec_ident: Vec<ndc_vm::value::Value> = match #raw {
                    ndc_vm::value::Value::Object(obj) => match obj.as_ref() {
                        ndc_vm::value::Object::List(list) => list.borrow().clone(),
                        ndc_vm::value::Object::Tuple(tuple) => tuple.clone(),
                        _ => return Err(ndc_vm::error::VmError::native(format!(
                            "arg {}: expected list, got {}",
                            #position, #raw.static_type()
                        ))),
                    },
                    _ => return Err(ndc_vm::error::VmError::native(format!(
                        "arg {}: expected list, got {}",
                        #position, #raw.static_type()
                    ))),
                };
            },
            pass: quote! { &#vec_ident },
            static_type: quote! {
                ndc_interpreter::function::StaticType::List(Box::new(
                    ndc_interpreter::function::StaticType::Any
                ))
            },
        });
    }

    // &VmCallable — extract a VM function and wrap it with the current globals for HOF dispatch.
    // `globals` is the second parameter of the generated `|args, globals|` closure and is in scope.
    if path_ends_with(ty, "VmCallable") && is_ref(ty) {
        return Some(VmInputArg {
            extract: quote! {
                let #temp = match #raw {
                    ndc_vm::value::Value::Object(_obj) => match _obj.as_ref() {
                        ndc_vm::value::Object::Function(f) => ndc_vm::vm::VmCallable {
                            function: f.clone(),
                            globals: _globals,
                        },
                        _ => return Err(ndc_vm::error::VmError::native(format!(
                            "arg {}: expected function, got {}",
                            #position, #raw.static_type()
                        ))),
                    },
                    _ => return Err(ndc_vm::error::VmError::native(format!(
                        "arg {}: expected function, got {}",
                        #position, #raw.static_type()
                    ))),
                };
            },
            pass: quote! { &#temp },
            static_type: quote! {
                ndc_interpreter::function::StaticType::Function {
                    parameters: None,
                    return_type: Box::new(ndc_interpreter::function::StaticType::Any),
                }
            },
        });
    }

    // &[Value] — convert a VM list or tuple to a Vec of interpreter Values and pass as a slice.
    // Read-only; no mutation sync needed.
    if is_ref_of_slice_of_value(ty) {
        let vec_ident = format_ident!("{temp}_vec");
        return Some(VmInputArg {
            extract: quote! {
                let #vec_ident: Vec<ndc_interpreter::value::Value> = match #raw {
                    ndc_vm::value::Value::Object(obj) => match obj.as_ref() {
                        ndc_vm::value::Object::List(list) => {
                            list.borrow().iter().map(ndc_interpreter::vm_bridge::vm_to_interp).collect()
                        }
                        ndc_vm::value::Object::Tuple(tuple) => {
                            tuple.iter().map(ndc_interpreter::vm_bridge::vm_to_interp).collect()
                        }
                        _ => return Err(ndc_vm::error::VmError::native(format!(
                            "arg {}: expected list, got {}",
                            #position, #raw.static_type()
                        ))),
                    },
                    _ => return Err(ndc_vm::error::VmError::native(format!(
                        "arg {}: expected list, got {}",
                        #position, #raw.static_type()
                    ))),
                };
            },
            pass: quote! { &#vec_ident },
            static_type: quote! {
                ndc_interpreter::function::StaticType::List(Box::new(
                    ndc_interpreter::function::StaticType::Any
                ))
            },
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
    // ndc_vm::value::Value — already a VmValue, return as-is.
    if is_ndc_vm_value(ty) {
        return Some((
            quote! { Ok(result) },
            quote! { ndc_interpreter::function::StaticType::Any },
        ));
    }

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
    if path_ends_with(ty, "usize") {
        return Some((
            quote! { Ok(ndc_vm::value::Value::Int(result as i64)) },
            quote! { ndc_interpreter::function::StaticType::Int },
        ));
    }
    if path_ends_with(ty, "BigInt") {
        return Some((
            quote! {
                Ok(ndc_vm::value::Value::from_number(
                    ndc_interpreter::num::Number::Int(
                        ndc_interpreter::int::Int::BigInt(result).simplified()
                    )
                ))
            },
            quote! { ndc_interpreter::function::StaticType::Int },
        ));
    }
    if path_ends_with(ty, "BigRational") {
        return Some((
            quote! {
                Ok(ndc_vm::value::Value::from_number(
                    ndc_interpreter::num::Number::Rational(Box::new(result))
                ))
            },
            quote! { ndc_interpreter::function::StaticType::Rational },
        ));
    }

    // Value (interpreter type) — convert to VmValue via interp_to_vm.
    // Handles functions like paragraphs/lines/words/split that build and return a Value.
    if path_ends_with(ty, "Value") {
        return Some((
            quote! { Ok(ndc_interpreter::vm_bridge::interp_to_vm(result)) },
            quote! { ndc_interpreter::function::StaticType::Any },
        ));
    }

    // EvaluationResult = Result<Value, FunctionCarrier> — treat as Result<Value>.
    if path_ends_with(ty, "EvaluationResult") {
        return Some((
            quote! {
                let result = result.map_err(|e| ndc_vm::error::VmError::native(e.to_string()))?;
                Ok(ndc_interpreter::vm_bridge::interp_to_vm(result))
            },
            quote! { ndc_interpreter::function::StaticType::Any },
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
                                let result = result.map_err(|e| ndc_vm::error::VmError::native(e.to_string()))?;
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
    // ndc_vm::value::Value inside Result<ndc_vm::value::Value> — pass through.
    if is_ndc_vm_value(ty) {
        return Some((
            quote! { Ok(result) },
            quote! { ndc_interpreter::function::StaticType::Any },
        ));
    }
    try_vm_return_type(ty)
}
