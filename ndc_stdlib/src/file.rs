use ndc_core::{FunctionRegistry, StaticType};
use ndc_macros::export_module;
use ndc_vm::NativeSpec;
use ndc_vm::error::VmError;
use ndc_vm::value::{NativeFunc, Value};
use std::fmt::Write as FmtWrite;
use std::fs::read_to_string;

#[export_module]
mod inner {
    use anyhow::Context;
    use std::path::PathBuf;

    /// Reads the entire contents of a file into a string.
    pub fn read_file(file_path: &str) -> anyhow::Result<String> {
        read_to_string(file_path.parse::<PathBuf>().context("invalid file path")?)
            .context("failed to read file")
    }
}

/// The natives that use the VM for input and output.
pub fn register_io(env: &mut FunctionRegistry<NativeSpec>) {
    let print_native = NativeSpec::new(
        "print",
        "Prints its arguments to standard output, separated by spaces, followed by a newline.",
        move || StaticType::Function {
            parameters: None,
            return_type: Box::new(StaticType::unit()),
        },
        move || {
            NativeFunc::WithVm(Box::new(|args, vm| {
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
                Ok(Value::unit())
            }))
        },
    );

    let dbg_native = NativeSpec::new(
        "dbg",
        "Prints its arguments in debug format to standard output, separated by spaces, followed by a newline.",
        move || StaticType::Function {
            parameters: None,
            return_type: Box::new(StaticType::unit()),
        },
        move || {
            NativeFunc::WithVm(Box::new(|args, vm| {
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
                Ok(Value::unit())
            }))
        },
    );

    let read_stdin_native = NativeSpec::new(
        "read_stdin",
        "Reads all of standard input and returns it as a string. A second call returns an empty string, as the first read everything.",
        move || StaticType::Function {
            parameters: Some(vec![]),
            return_type: Box::new(StaticType::String),
        },
        move || {
            NativeFunc::WithVm(Box::new(|args, vm| {
                if !args.is_empty() {
                    return Err(VmError::native(format!(
                        "expected 0 arguments, got {}",
                        args.len()
                    )));
                }
                Ok(Value::string(vm.read_input()?))
            }))
        },
    );

    env.declare_global_fn(print_native);
    env.declare_global_fn(dbg_native);
    env.declare_global_fn(read_stdin_native);
}
