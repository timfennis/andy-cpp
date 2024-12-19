mod function;
mod r#match;

extern crate proc_macro;
extern crate proc_macro2;

use proc_macro::TokenStream;
use quote::quote;

use crate::function::wrap_function;
use syn::{parse_macro_input, Item};

#[proc_macro_attribute]
pub fn function(_attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}

#[proc_macro_attribute]
pub fn export_module(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let module = parse_macro_input!(item as syn::ItemMod);
    let module_name = module.ident;
    let module_vis = module.vis;

    let Some((_, items)) = module.content else {
        panic!("exported module has no content");
    };

    let mut declarations = Vec::new();
    let mut registrations = Vec::new();
    let mut uses = Vec::new();

    for item in items {
        match item {
            Item::Fn(f) => {
                for fun in wrap_function(f) {
                    declarations.push(fun.function_declaration);
                    registrations.push(fun.function_registration);
                }
            }
            Item::Use(u) => {
                uses.push(u);
            }
            _ => panic!("not sure how to deal with this thing"),
        }
    }

    let register_function = quote! {
        pub fn register(env: &mut crate::interpreter::environment::Environment) {
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
