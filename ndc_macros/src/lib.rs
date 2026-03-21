//! Procedural macros for registering NDC standard library functions.
//!
//! The primary entry point is [`export_module`], which wraps public functions
//! inside a module into VM-native closures and generates registration code.

mod function;
mod types;
mod vm_convert;

use proc_macro::TokenStream;
use quote::quote;

use crate::function::wrap_function;
use syn::{Item, parse_macro_input};

#[proc_macro_attribute]
pub fn export_module(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let module = parse_macro_input!(item as syn::ItemMod);
    let module_name = module.ident;
    let module_vis = module.vis;

    let Some((_, items)) = module.content else {
        return syn::Error::new(module_name.span(), "exported module has no content")
            .to_compile_error()
            .into();
    };

    let mut declarations = Vec::new();
    let mut registrations = Vec::new();
    let mut uses = Vec::new();

    for item in items {
        match item {
            Item::Fn(f) if matches!(f.vis, syn::Visibility::Public(_)) => match wrap_function(&f) {
                Ok(fns) => {
                    for fun in fns {
                        declarations.push(fun.function_declaration);
                        registrations.push(fun.function_registration);
                    }
                }
                Err(e) => return e.to_compile_error().into(),
            },
            Item::Use(u) => {
                uses.push(u);
            }
            item => declarations.push(quote! { #item }),
        }
    }

    let register_function = quote! {
        pub fn register(env: &mut ndc_core::FunctionRegistry<std::rc::Rc<ndc_vm::value::NativeFunction>>) {
            #(#registrations)*
        }
    };

    let module = quote! {
        #module_vis mod #module_name {
            use super::*;
            #(#uses)*
            #(#declarations)*
        }
        use self::#module_name::*;
        #register_function
    };

    module.into()
}
