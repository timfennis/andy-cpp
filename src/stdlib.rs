pub mod cmp;
pub mod file;
pub mod hash_map;
pub mod list;
pub mod math;
pub mod regex;
pub mod sequence;
pub mod string;
pub mod value;

#[macro_export]
macro_rules! register_fn {
    ($env:expr, $function:ident) => {
        $env.declare(
            stringify!($function),
            $crate::interpreter::value::Value::from(
                $crate::interpreter::function::Function::GenericFunction {
                    function: $function,
                    type_signature: $crate::interpreter::function::TypeSignature::Variadic, // TODO: NOOOOOOOOOOOOOOOOOOOO!!!
                },
            ),
        );
    };
}
