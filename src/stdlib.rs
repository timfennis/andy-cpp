pub mod file;
pub mod math;
pub mod string;

#[macro_export]
macro_rules! register_fn {
    ($env:expr, $function:ident) => {
        $env.declare(
            stringify!($function),
            Value::from(Function::GenericFunction {
                function: $function,
            }),
        );
    };
}
