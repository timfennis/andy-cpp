//! Type classification for NDC macro parameter and return types.
//!
//! The [`classify`] function maps `syn::Type` values to [`NdcType`] variants,
//! providing a single point of truth for type recognition. Both parameter
//! extraction (`vm_convert`) and function wrapping (`function`) use this
//! classification instead of ad-hoc predicate chains.

/// A recognized NDC type from a Rust function signature.
///
/// Variants encode both the base type and its reference/mutability wrapper,
/// since the generated code differs based on ownership.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NdcType {
    /// Owned `Number`
    Number,
    /// `&Number`
    NumberRef,
    /// `f64`
    F64,
    /// `bool`
    Bool,
    /// `i64`
    I64,
    /// `usize`
    Usize,
    /// Owned `BigInt` (return types)
    BigInt,
    /// `&BigInt` (input parameters)
    BigIntRef,
    /// Owned `BigRational` (return types)
    BigRational,
    /// `&BigRational` (input parameters)
    BigRationalRef,
    /// Owned `Complex64`
    Complex64,
    /// Owned `String` (also matches `&String`)
    String,
    /// `&str`
    StrRef,
    /// `&mut String`
    MutString,
    /// `&StringRepr` or owned `StringRepr`
    StringRepr,
    /// `&mut StringRepr`
    MutStringRepr,
    /// `ndc_vm::value::Value`
    VmValue,
    /// `ndc_vm::value::SeqValue`
    SeqValue,
    /// `ndc_vm::value::MapValue`
    MapValue,
    /// `&[ndc_vm::value::Value]`
    SliceOfValue,
    /// `&mut Vec<ndc_vm::value::Value>`
    MutVecOfValue,
    /// `&HashMap<Value, Value>`
    RefHashMap,
    /// `&mut HashMap<Value, Value>`
    MutHashMap,
    /// `&VecDeque<Value>`
    RefVecDeque,
    /// `&mut VecDeque<Value>`
    MutVecDeque,
    /// `&mut BinaryHeap<Reverse<OrdValue>>`
    MutMinHeap,
    /// `&mut BinaryHeap<OrdValue>`
    MutMaxHeap,
    /// `&mut VmCallable`
    MutVmCallable,
}

/// Classify a `syn::Type` into a recognized NDC type.
///
/// Returns `None` for types that cannot be directly handled by the macro
/// system (e.g. custom structs, unsupported generic combinations).
pub fn classify(ty: &syn::Type) -> Option<NdcType> {
    match ty {
        syn::Type::Reference(r) => {
            if r.mutability.is_some() {
                classify_ref_mut(&r.elem)
            } else {
                classify_ref(&r.elem)
            }
        }
        _ => classify_owned(ty),
    }
}

/// Unwrap a `Result<T, _>` type, returning the inner `T`.
pub fn unwrap_result(ty: &syn::Type) -> Option<&syn::Type> {
    let syn::Type::Path(p) = ty else { return None };
    let last = p.path.segments.last()?;
    if last.ident != "Result" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(args) = &last.arguments else {
        return None;
    };
    match args.args.first() {
        Some(syn::GenericArgument::Type(inner)) => Some(inner),
        _ => None,
    }
}

// --- Classify by ownership ---

fn classify_owned(ty: &syn::Type) -> Option<NdcType> {
    let syn::Type::Path(type_path) = ty else {
        return None;
    };
    let segments: Vec<_> = type_path.path.segments.iter().collect();

    let last = segments.last()?;
    match last.ident.to_string().as_str() {
        "Number" => Some(NdcType::Number),
        "f64" => Some(NdcType::F64),
        "bool" => Some(NdcType::Bool),
        "i64" => Some(NdcType::I64),
        "usize" => Some(NdcType::Usize),
        "Complex64" => Some(NdcType::Complex64),
        "String" => Some(NdcType::String),
        "StringRepr" => Some(NdcType::StringRepr),
        "BigInt" => Some(NdcType::BigInt),
        "BigRational" => Some(NdcType::BigRational),
        "Value" => Some(NdcType::VmValue),
        "SeqValue" => Some(NdcType::SeqValue),
        "MapValue" => Some(NdcType::MapValue),
        _ => None,
    }
}

fn classify_ref(inner: &syn::Type) -> Option<NdcType> {
    // &str
    if is_path_ident(inner, "str") {
        return Some(NdcType::StrRef);
    }

    // &[ndc_vm::value::Value]
    if let syn::Type::Slice(slice) = inner
        && classify_owned(&slice.elem) == Some(NdcType::VmValue)
    {
        return Some(NdcType::SliceOfValue);
    }

    // &HashMap<Value, Value>
    if is_collection_of_vm_value(inner, "HashMap") {
        return Some(NdcType::RefHashMap);
    }

    // &VecDeque<Value>
    if is_collection_of_vm_value(inner, "VecDeque") {
        return Some(NdcType::RefVecDeque);
    }

    // Simple &T references — map to appropriate variant
    match classify_owned(inner)? {
        NdcType::Number => Some(NdcType::NumberRef),
        NdcType::BigInt => Some(NdcType::BigIntRef),
        NdcType::BigRational => Some(NdcType::BigRationalRef),
        NdcType::String => Some(NdcType::String),
        NdcType::StringRepr => Some(NdcType::StringRepr),
        _ => None,
    }
}

fn classify_ref_mut(inner: &syn::Type) -> Option<NdcType> {
    if is_path_ident(inner, "VmCallable") {
        return Some(NdcType::MutVmCallable);
    }
    if is_path_ident(inner, "String") {
        return Some(NdcType::MutString);
    }
    if is_path_ident(inner, "StringRepr") {
        return Some(NdcType::MutStringRepr);
    }
    if is_collection_of_vm_value(inner, "Vec") {
        return Some(NdcType::MutVecOfValue);
    }
    if is_collection_of_vm_value(inner, "HashMap") {
        return Some(NdcType::MutHashMap);
    }
    if is_collection_of_vm_value(inner, "VecDeque") {
        return Some(NdcType::MutVecDeque);
    }
    if is_binary_heap_of(inner, is_reverse_of_ord_value) {
        return Some(NdcType::MutMinHeap);
    }
    if is_binary_heap_of(inner, |t| is_path_ident(t, "OrdValue")) {
        return Some(NdcType::MutMaxHeap);
    }
    None
}

fn is_path_ident(ty: &syn::Type, ident: &str) -> bool {
    let syn::Type::Path(p) = ty else { return false };
    p.path.segments.last().is_some_and(|s| s.ident == ident)
}

/// Check if `ty` is `Collection<ndc_vm::value::Value>` (for Vec, `VecDeque`)
/// or `Collection<ndc_vm::value::Value, ndc_vm::value::Value>` (for `HashMap`).
fn is_collection_of_vm_value(ty: &syn::Type, collection_name: &str) -> bool {
    let syn::Type::Path(p) = ty else { return false };
    let Some(last) = p.path.segments.last() else {
        return false;
    };
    if last.ident != collection_name {
        return false;
    }
    let syn::PathArguments::AngleBracketed(args) = &last.arguments else {
        return false;
    };
    if collection_name == "HashMap" {
        let mut iter = args.args.iter();
        let Some(syn::GenericArgument::Type(key)) = iter.next() else {
            return false;
        };
        let Some(syn::GenericArgument::Type(val)) = iter.next() else {
            return false;
        };
        classify_owned(key) == Some(NdcType::VmValue)
            && classify_owned(val) == Some(NdcType::VmValue)
    } else {
        let Some(syn::GenericArgument::Type(elem)) = args.args.first() else {
            return false;
        };
        classify_owned(elem) == Some(NdcType::VmValue)
    }
}

fn is_binary_heap_of(ty: &syn::Type, check: impl Fn(&syn::Type) -> bool) -> bool {
    let syn::Type::Path(p) = ty else { return false };
    let Some(last) = p.path.segments.last() else {
        return false;
    };
    if last.ident != "BinaryHeap" {
        return false;
    }
    let syn::PathArguments::AngleBracketed(args) = &last.arguments else {
        return false;
    };
    let Some(syn::GenericArgument::Type(inner)) = args.args.first() else {
        return false;
    };
    check(inner)
}

fn is_reverse_of_ord_value(ty: &syn::Type) -> bool {
    let syn::Type::Path(p) = ty else { return false };
    let Some(last) = p.path.segments.last() else {
        return false;
    };
    if last.ident != "Reverse" {
        return false;
    }
    let syn::PathArguments::AngleBracketed(args) = &last.arguments else {
        return false;
    };
    let Some(syn::GenericArgument::Type(inner)) = args.args.first() else {
        return false;
    };
    is_path_ident(inner, "OrdValue")
}
