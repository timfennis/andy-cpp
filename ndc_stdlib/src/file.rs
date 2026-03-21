use ndc_core::{FunctionRegistry, StaticType};
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

pub fn register_variadic(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    let print_native = Rc::new(NativeFunction {
        name: "print".to_string(),
        documentation: Some("Prints its arguments to standard output, separated by spaces, followed by a newline.".to_string()),
        static_type: StaticType::Function {
            parameters: None,
            return_type: Box::new(StaticType::unit()),
        },
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
        documentation: Some("Prints its arguments in debug format to standard output, separated by spaces, followed by a newline.".to_string()),
        static_type: StaticType::Function {
            parameters: None,
            return_type: Box::new(StaticType::unit()),
        },
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

    env.declare_global_fn(print_native);
    env.declare_global_fn(dbg_native);
}
