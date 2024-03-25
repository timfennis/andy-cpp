pub fn is_ref(ty: &syn::Type) -> bool {
    matches!(ty, syn::Type::Reference(syn::TypeReference { .. }))
}

pub fn is_str_ref(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Reference(syn::TypeReference { elem: ty, .. }) => {
            let ty = &*ty;
            is_str(ty)
        }
        _ => false,
    }
}

fn is_str(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Path(syn::TypePath {
            path: syn::Path { segments, .. },
            ..
        }) => segments.iter().any(|seg| seg.ident == "str"),
        _ => false,
    }
}
