/// Checks if a type ends with a string, returns false for slices
pub fn path_ends_with(ty: &syn::Type, ident: &str) -> bool {
    match ty {
        syn::Type::Path(syn::TypePath {
            path: syn::Path { segments, .. },
            ..
        }) => {
            let Some(last_segment) = segments.last() else {
                return false;
            };

            last_segment.ident == ident
        }
        syn::Type::Reference(syn::TypeReference { elem, .. }) => path_ends_with(elem, ident),
        _ => false,
    }
}

pub fn is_ref(ty: &syn::Type) -> bool {
    matches!(
        ty,
        syn::Type::Reference(syn::TypeReference {
            mutability: None,
            ..
        })
    )
}
pub fn is_ref_of(ty: &syn::Type, f: fn(&syn::Type) -> bool) -> bool {
    match ty {
        syn::Type::Reference(syn::TypeReference {
            elem, mutability, ..
        }) if mutability.is_none() => f(elem.as_ref()),
        _ => false,
    }
}

pub fn is_ref_mut_of(ty: &syn::Type, f: fn(&syn::Type) -> bool) -> bool {
    match ty {
        syn::Type::Reference(syn::TypeReference {
            elem, mutability, ..
        }) if mutability.is_some() => f(elem.as_ref()),
        _ => false,
    }
}

pub fn is_ref_mut_of_slice_of_value(ty: &syn::Type) -> bool {
    is_ref_mut_of(ty, |ty| match ty {
        syn::Type::Slice(syn::TypeSlice { elem, .. }) => has_path_match(elem.as_ref(), "Value"),
        _ => false,
    })
}

pub fn is_ref_of_slice_of_value(ty: &syn::Type) -> bool {
    is_ref_of(ty, |ty| match ty {
        syn::Type::Slice(syn::TypeSlice { elem, .. }) => has_path_match(elem.as_ref(), "Value"),
        _ => false,
    })
}

pub fn is_ref_of_bigint(ty: &syn::Type) -> bool {
    is_ref_of(ty, |ty| has_path_match(ty, "BigInt"))
}

pub fn is_ref_mut(ty: &syn::Type) -> bool {
    matches!(
        ty,
        syn::Type::Reference(syn::TypeReference {
            mutability: Some(_),
            ..
        })
    )
}

pub fn is_str_ref(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Reference(syn::TypeReference { elem: ty, .. }) => has_path_match(ty, "str"),
        _ => false,
    }
}

fn has_path_match(ty: &syn::Type, ident: &str) -> bool {
    match ty {
        syn::Type::Path(syn::TypePath {
            path: syn::Path { segments, .. },
            ..
        }) => segments.iter().any(|seg| seg.ident == ident),
        _ => false,
    }
}

/// Returns true if the type is the fully-qualified `ndc_vm::value::Value`.
/// Single-segment `Value` is NOT matched (ambiguous — could be interpreter Value).
pub fn is_ndc_vm_value(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(syn::TypePath { path, .. }) => {
            let segs: Vec<_> = path.segments.iter().collect();
            matches!(
                segs.as_slice(),
                [a, b, c] if a.ident == "ndc_vm" && b.ident == "value" && c.ident == "Value"
            )
        }
        _ => false,
    }
}

/// Returns true if the type is `&[ndc_vm::value::Value]`.
pub fn is_ref_of_slice_of_ndc_vm_value(ty: &syn::Type) -> bool {
    is_ref_of(ty, |inner| match inner {
        syn::Type::Slice(syn::TypeSlice { elem, .. }) => is_ndc_vm_value(elem.as_ref()),
        _ => false,
    })
}

/// Returns true if the type is `&mut Vec<ndc_vm::value::Value>`.
pub fn is_ref_mut_of_vec_of_ndc_vm_value(ty: &syn::Type) -> bool {
    is_ref_mut_of(ty, |inner| match inner {
        syn::Type::Path(syn::TypePath { path, .. }) => {
            if let Some(last) = path.segments.last() {
                if last.ident != "Vec" {
                    return false;
                }
                if let syn::PathArguments::AngleBracketed(args) = &last.arguments {
                    if let Some(syn::GenericArgument::Type(elem)) = args.args.first() {
                        return is_ndc_vm_value(elem);
                    }
                }
            }
            false
        }
        _ => false,
    })
}

fn is_hashmap_of_ndc_vm_value(ty: &syn::Type) -> bool {
    let syn::Type::Path(syn::TypePath { path, .. }) = ty else {
        return false;
    };
    let Some(last) = path.segments.last() else {
        return false;
    };
    if last.ident != "HashMap" {
        return false;
    }
    let syn::PathArguments::AngleBracketed(args) = &last.arguments else {
        return false;
    };
    let mut iter = args.args.iter();
    let Some(syn::GenericArgument::Type(key)) = iter.next() else {
        return false;
    };
    let Some(syn::GenericArgument::Type(val)) = iter.next() else {
        return false;
    };
    is_ndc_vm_value(key) && is_ndc_vm_value(val)
}

/// Returns true if the type is `&mut HashMap<ndc_vm::value::Value, ndc_vm::value::Value>`.
pub fn is_ref_mut_of_hashmap_of_ndc_vm_value(ty: &syn::Type) -> bool {
    is_ref_mut_of(ty, is_hashmap_of_ndc_vm_value)
}

/// Returns true if the type is `&HashMap<ndc_vm::value::Value, ndc_vm::value::Value>`.
pub fn is_ref_of_hashmap_of_ndc_vm_value(ty: &syn::Type) -> bool {
    is_ref_of(ty, is_hashmap_of_ndc_vm_value)
}

fn is_vecdeque_of_ndc_vm_value(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(syn::TypePath { path, .. }) => {
            if let Some(last) = path.segments.last() {
                if last.ident != "VecDeque" {
                    return false;
                }
                if let syn::PathArguments::AngleBracketed(args) = &last.arguments {
                    if let Some(syn::GenericArgument::Type(elem)) = args.args.first() {
                        return is_ndc_vm_value(elem);
                    }
                }
            }
            false
        }
        _ => false,
    }
}

/// Returns true if the type is `&mut VecDeque<ndc_vm::value::Value>`.
pub fn is_ref_mut_of_vecdeque_of_ndc_vm_value(ty: &syn::Type) -> bool {
    is_ref_mut_of(ty, is_vecdeque_of_ndc_vm_value)
}

/// Returns true if the type is `&VecDeque<ndc_vm::value::Value>`.
pub fn is_ref_of_vecdeque_of_ndc_vm_value(ty: &syn::Type) -> bool {
    is_ref_of(ty, is_vecdeque_of_ndc_vm_value)
}

fn is_binaryheap_of<F: Fn(&syn::Type) -> bool>(ty: &syn::Type, check_inner: F) -> bool {
    let syn::Type::Path(syn::TypePath { path, .. }) = ty else {
        return false;
    };
    let Some(last) = path.segments.last() else {
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
    check_inner(inner)
}

fn is_reverse_of_ord_value(ty: &syn::Type) -> bool {
    let syn::Type::Path(syn::TypePath { path, .. }) = ty else {
        return false;
    };
    let Some(last) = path.segments.last() else {
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
    path_ends_with(inner, "OrdValue")
}

/// Returns true if the type is `&mut BinaryHeap<Reverse<OrdValue>>` (min-heap).
pub fn is_ref_mut_of_min_heap(ty: &syn::Type) -> bool {
    is_ref_mut_of(ty, |inner| is_binaryheap_of(inner, is_reverse_of_ord_value))
}

/// Returns true if the type is `&mut BinaryHeap<OrdValue>` (max-heap).
pub fn is_ref_mut_of_max_heap(ty: &syn::Type) -> bool {
    is_ref_mut_of(ty, |inner| {
        is_binaryheap_of(inner, |t| path_ends_with(t, "OrdValue"))
    })
}

pub fn is_string(ty: &syn::Type) -> bool {
    match ty {
        // If ref just recurse :haha:
        syn::Type::Reference(syn::TypeReference { elem: ty, .. }) => is_string(ty),
        syn::Type::Path(syn::TypePath {
            path: syn::Path { segments, .. },
            ..
        }) => segments.iter().any(|seg| seg.ident == "String"),
        _ => false,
    }
}
