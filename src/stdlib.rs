pub mod cmp;
pub mod file;
pub mod list;
pub mod map;
pub mod math;
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
