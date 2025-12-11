use crate::function::temp_create_map_any;
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
    fn static_type(&self) -> TokenStream;
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

    fn static_type(&self) -> TokenStream {
        quote! { crate::interpreter::function::StaticType::String }
    }

    fn convert(
        &self,
        temp_var: syn::Ident,
        original_name: &str,
        argument_var_name: syn::Ident,
    ) -> Vec<Argument> {
        vec![Argument {
            param_type: self.static_type(),
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

    fn static_type(&self) -> TokenStream {
        temp_create_map_any()
    }

    fn convert(
        &self,
        temp_var: syn::Ident,
        original_name: &str,
        argument_var_name: syn::Ident,
    ) -> Vec<Argument> {
        vec![Argument {
            param_type: self.static_type(),
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
struct InternalString;
impl TypeConverter for InternalString {
    fn matches(&self, ty: &syn::Type) -> bool {
        path_ends_with(ty, "StringRepr")
    }

    fn static_type(&self) -> TokenStream {
        quote! { crate::interpreter::function::StaticType::String }
    }

    fn convert(
        &self,
        temp_var: syn::Ident,
        original_name: &str,
        argument_var_name: syn::Ident,
    ) -> Vec<Argument> {
        vec![Argument {
            param_type: self.static_type(),
            param_name: quote! { #original_name },
            argument: quote! { #argument_var_name },
            initialize_code: quote! {
                let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::String(#temp_var)) = #argument_var_name else {
                    panic!("Value #position needed to be Sequence::List but wasn't");
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

    fn static_type(&self) -> TokenStream {
        // TODO: just hardcoding Any here is lazy
        quote! {
            crate::interpreter::function::StaticType::List(Box::new(
                crate::interpreter::function::StaticType::Any
            ))
        }
    }

    fn convert(
        &self,
        temp_var: syn::Ident,
        original_name: &str,
        argument_var_name: syn::Ident,
    ) -> Vec<Argument> {
        vec![Argument {
            param_type: self.static_type(),
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

// Losing tuple concatenation is a price we might have to pay
// /// Matches `Rc<RefCell<Vec<Value>>>`
// struct InternalTuple;
// impl TypeConverter for InternalTuple {
//     fn matches(&self, ty: &syn::Type) -> bool {
//         path_ends_with(ty, "TupleRepr")
//     }
//
//     fn convert(
//         &self,
//         temp_var: syn::Ident,
//         original_name: &str,
//         argument_var_name: syn::Ident,
//     ) -> Vec<Argument> {
//         vec![Argument {
//             param_type: quote! { crate::interpreter::function::StaticType::Tuple },
//             param_name: quote! { #original_name },
//             argument: quote! { #argument_var_name },
//             initialize_code: quote! {
//                 let crate::interpreter::value::Value::Sequence(crate::interpreter::sequence::Sequence::Tuple(#temp_var)) = #argument_var_name else {
//                     panic!("Value #position needed to be Sequence::Tuple but wasn't");
//                 };
//
//                 // TODO: is std::mem::take appropriate here?
//                 let #argument_var_name = std::mem::take(#temp_var);
//             },
//         }]
//     }
// }

pub fn build() -> Vec<Box<dyn TypeConverter>> {
    vec![
        Box::new(InternalList),
        Box::new(MutRefString),
        // Box::new(InternalTuple),
        Box::new(InternalMap),
        Box::new(InternalString),
    ]
}
