use crate::interpreter::environment::Environment;
use crate::interpreter::function::{FunctionBody, FunctionBuilder, FunctionCarrier, TypeSignature};
use crate::interpreter::value::Value;
use ndc_macros::export_module;
use std::fs::read_to_string;

#[export_module]
mod inner {
    use anyhow::Context;
    use std::path::PathBuf;

    pub fn read_file(file_path: &str) -> anyhow::Result<String> {
        read_to_string(file_path.parse::<PathBuf>().context("invalid file path")?)
            .context("failed to read file")
    }
}

pub fn register_variadic(env: &mut Environment) {
    env.declare(
        "print",
        Value::function(
            FunctionBuilder::default()
                .name("print".to_string())
                .documentation("Print the value.".to_string())
                .body(FunctionBody::GenericFunction {
                    function: |args, env| {
                        env.borrow_mut()
                            .with_output(|output| {
                                let mut iter = args.iter().peekable();

                                // If no arguments are passed to the print function just print an empty line
                                if iter.peek().is_none() {
                                    writeln!(output)?;
                                    return Ok(());
                                }

                                // Otherwise
                                while let Some(arg) = iter.next() {
                                    if iter.peek().is_some() {
                                        write!(output, "{arg} ")?;
                                    } else {
                                        writeln!(output, "{arg}")?;
                                    }
                                }
                                Ok(())
                            })
                            .map_err(|err| FunctionCarrier::IntoEvaluationError(Box::new(err)))?;
                        Ok(Value::unit())
                    },
                    type_signature: TypeSignature::Variadic,
                })
                .build()
                .expect("function definition defined in code must be valid"),
        ),
    );

    env.declare(
        "dbg",
        Value::function(
            FunctionBuilder::default()
                .name("dbg".to_string())
                .documentation("Prints the values for quick and dirty debugging (using the value's debug representation).".to_string())
                .body(FunctionBody::GenericFunction {
                    function: |args, env| {
                        env.borrow_mut()
                            .with_output(|output| {
                                let mut iter = args.iter().peekable();
                                while let Some(arg) = iter.next() {
                                    if iter.peek().is_some() {
                                        write!(output, "{arg:?} ")?;
                                    } else {
                                        writeln!(output, "{arg:?}")?;
                                    }
                                }
                                Ok(())
                            })
                            .map_err(|err| FunctionCarrier::IntoEvaluationError(Box::new(err)))?;
                        Ok(Value::unit())
                    },
                    type_signature: TypeSignature::Variadic,
                })
                .build()
                .expect("function definition defined in code must be valid"),
        ),
    );
}
