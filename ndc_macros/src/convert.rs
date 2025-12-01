use crate::r#match::{is_ref_mut, is_string, path_ends_with};
use proc_macro2::TokenStream;
use quote::quote;

#[derive(Debug, Clone)]
pub struct Argument {
    // TODO: remove these pub
    pub param_type: TokenStream,
    pub param_name: TokenStream,
    pub argument: TokenStream,
    pub initialize_code: TokenStream,
}
pub trait TypeConverter {
    fn matches(&self, ty: &syn::Type) -> bool;
    fn convert(
        &self,
        temp_var: syn::Ident,
        original_name: &str,
        argument_var_name: syn::Ident,
    ) -> Vec<Argument>;
}

struct MutRefString;
impl TypeConverter for MutRefString {
    fn matches(&self, ty: &syn::Type) -> bool {
        is_ref_mut(ty) && is_string(ty)
    }

    fn convert(
        &self,
        temp_var: syn::Ident,
        original_name: &str,
        argument_var_name: syn::Ident,
    ) -> Vec<Argument> {
        vec![Argument {
            param_type: quote! { crate::interpreter::function::ParamType::String },
            param_name: quote! { #original_name },
            argument: quote! { #argument_var_name },
            initialize_code: quote! {
                let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::String(#temp_var)) = #argument_var_name else {
                    panic!("Value #position needed to be a Sequence::String but wasn't");
                };
                let #argument_var_name = &mut *#temp_var.try_borrow_mut()?;
            },
        }]
    }
}

/// Matches `Rc<RefCell<HashMap<Value, Value>>>`
struct InternalMap;
impl TypeConverter for InternalMap {
    fn matches(&self, ty: &syn::Type) -> bool {
        path_ends_with(ty, "MapRepr")
    }

    fn convert(
        &self,
        temp_var: syn::Ident,
        original_name: &str,
        argument_var_name: syn::Ident,
    ) -> Vec<Argument> {
        vec![Argument {
            param_type: quote! { crate::interpreter::function::ParamType::Map },
            param_name: quote! { #original_name },
            argument: quote! { #argument_var_name },
            initialize_code: quote! {
                let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Map(#temp_var, _)) = #argument_var_name else {
                    panic!("Value #position needed to be Sequence::Map but wasn't");
                };

                let #argument_var_name = #temp_var;
            },
        }]
    }
}
/// Matches `Rc<RefCell<Vec<Value>>>`
struct InternalList;
impl TypeConverter for InternalList {
    fn matches(&self, ty: &syn::Type) -> bool {
        path_ends_with(ty, "ListRepr")
    }

    fn convert(
        &self,
        temp_var: syn::Ident,
        original_name: &str,
        argument_var_name: syn::Ident,
    ) -> Vec<Argument> {
        vec![Argument {
            param_type: quote! { crate::interpreter::function::ParamType::List },
            param_name: quote! { #original_name },
            argument: quote! { #argument_var_name },
            initialize_code: quote! {
                let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::List(#temp_var)) = #argument_var_name else {
                    panic!("Value #position needed to be Sequence::List but wasn't");
                };

                let #argument_var_name = #temp_var;
            },
        }]
    }
}

/// Matches `Rc<RefCell<Vec<Value>>>`
struct InternalTuple;
impl TypeConverter for InternalTuple {
    fn matches(&self, ty: &syn::Type) -> bool {
        path_ends_with(ty, "TupleRepr")
    }

    fn convert(
        &self,
        temp_var: syn::Ident,
        original_name: &str,
        argument_var_name: syn::Ident,
    ) -> Vec<Argument> {
        vec![Argument {
            param_type: quote! { crate::interpreter::function::ParamType::Tuple },
            param_name: quote! { #original_name },
            argument: quote! { #argument_var_name },
            initialize_code: quote! {
                let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Tuple(#temp_var)) = #argument_var_name else {
                    panic!("blaap");
                };

                let #argument_var_name = std::mem::take(#temp_var); // TODO: is std::mem::take appropriate here?
            },
        }]
    }
}

pub fn build() -> Vec<Box<dyn TypeConverter>> {
    vec![
        Box::new(InternalList),
        Box::new(MutRefString),
        Box::new(InternalTuple),
        Box::new(InternalMap),
    ]
}
