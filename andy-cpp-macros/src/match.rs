pub fn is_ref_of(ty: &syn::Type, f: fn(&syn::Type) -> bool) -> bool {
    match ty {
        syn::Type::Reference(syn::TypeReference { elem, .. }) => f(elem.as_ref()),
        _ => false,
    }
}

pub fn is_ref_of_slice_with_value(ty: &syn::Type) -> bool {
    is_ref_of(ty, |ty| match ty {
        syn::Type::Slice(syn::TypeSlice { elem, .. }) => has_path_match(elem.as_ref(), "Value"),
        _ => false,
    })
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

pub fn is_owned_string(ty: &syn::Type) -> bool {
    match ty {
        // If ref just recurse :haha:
        syn::Type::Reference(syn::TypeReference { elem: ty, .. }) => is_owned_string(ty),
        syn::Type::Path(syn::TypePath {
            path: syn::Path { segments, .. },
            ..
        }) => segments.iter().any(|seg| seg.ident == "String"),
        _ => false,
    }
}
