use ndc_interpreter::environment::Environment;
use ndc_interpreter::function::{
    FunctionBody, FunctionBuilder, FunctionCarrier, StaticType, TypeSignature,
};
use ndc_interpreter::value::Value;
use ndc_macros::export_module;
use ndc_vm::error::VmError;
use ndc_vm::value::{NativeFunc, NativeFunction, Value as VmValue};
use std::fmt::Write as FmtWrite;
use std::fs::read_to_string;
use std::rc::Rc;

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
    let print_native = Rc::new(NativeFunction {
        name: "print".to_string(),
        static_type: StaticType::unit(),
        func: NativeFunc::WithVm(Box::new(|args, vm| {
            let mut buf = String::new();
            let mut iter = args.iter().peekable();
            if iter.peek().is_none() {
                buf.push('\n');
            } else {
                while let Some(arg) = iter.next() {
                    if iter.peek().is_some() {
                        write!(buf, "{arg} ").map_err(|e| VmError::native(e.to_string()))?;
                    } else {
                        writeln!(buf, "{arg}").map_err(|e| VmError::native(e.to_string()))?;
                    }
                }
            }
            vm.write_output(&buf)?;
            Ok(VmValue::unit())
        })),
    });

    let dbg_native = Rc::new(NativeFunction {
        name: "dbg".to_string(),
        static_type: StaticType::unit(),
        func: NativeFunc::WithVm(Box::new(|args, vm| {
            let mut buf = String::new();
            let mut iter = args.iter().peekable();
            while let Some(arg) = iter.next() {
                if iter.peek().is_some() {
                    write!(buf, "{arg:?} ").map_err(|e| VmError::native(e.to_string()))?;
                } else {
                    writeln!(buf, "{arg:?}").map_err(|e| VmError::native(e.to_string()))?;
                }
            }
            vm.write_output(&buf)?;
            Ok(VmValue::unit())
        })),
    });

    env.declare_global_fn(
        FunctionBuilder::default()
            .name("print".to_string())
            .documentation("Print the value.".to_string())
            .body(FunctionBody::GenericFunction {
                function: |args, env| {
                    env.borrow_mut()
                        .with_output(|output| {
                            let mut iter = args.iter().peekable();
                            if iter.peek().is_none() {
                                writeln!(output)?;
                                return Ok(());
                            }
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
                return_type: StaticType::unit(),
            })
            .vm_native(print_native)
            .build()
            .expect("function definition defined in code must be valid"),
    );

    env.declare_global_fn(
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
                return_type: StaticType::unit(),
            })
            .vm_native(dbg_native)
            .build()
            .expect("function definition defined in code must be valid"),
    );
}
