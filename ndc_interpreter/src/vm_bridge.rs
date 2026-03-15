use std::cell::RefCell;
use std::rc::Rc;

use ndc_core::int::Int;
use ndc_core::num::Number;
use ndc_vm::VmIterator;
use ndc_vm::value::{Function as VmFunction, NativeFunction, Object as VmObject, Value as VmValue};

use crate::environment::Environment;
use crate::evaluate::EvaluationResult;
use crate::function::{
    Function as InterpFunction, FunctionBody, FunctionBuilder, FunctionCarrier, VmFunctionWrapper,
};
use crate::iterator::ValueIterator;
use crate::sequence::Sequence;
use crate::value::Value as InterpValue;
use ndc_vm::vm::Vm;

/// Adapter that wraps an interpreter ValueIterator as a VM VmIterator
struct InterpIteratorAdapter {
    inner: Rc<RefCell<ValueIterator>>,
}

impl VmIterator for InterpIteratorAdapter {
    fn next(&mut self) -> Option<VmValue> {
        let val = self.inner.borrow_mut().next()?;
        Some(interp_to_vm(val))
    }
}

pub fn make_vm_globals(env: &Rc<RefCell<Environment>>) -> Vec<VmValue> {
    // The globals cell is initially empty; wrappers capture it by Rc.
    // After all wrappers are built, we fill it in. Callbacks that need
    // globals (for re-entrant VM calls) only run after this point.
    let globals_cell: Rc<RefCell<Vec<VmValue>>> = Rc::new(RefCell::new(Vec::new()));
    let globals: Vec<VmValue> = env
        .borrow()
        .get_all_functions()
        .into_iter()
        .map(|func| wrap_function(func, Rc::clone(env), Rc::clone(&globals_cell)))
        .collect();
    *globals_cell.borrow_mut() = globals.clone();
    globals
}

fn wrap_function(
    func: InterpFunction,
    dummy_env: Rc<RefCell<Environment>>,
    globals_cell: Rc<RefCell<Vec<VmValue>>>,
) -> VmValue {
    let static_type = func.static_type();
    let native = move |args: &[VmValue]| -> Result<VmValue, String> {
        let globals = Rc::new(globals_cell.borrow().clone());
        let mut interp_args: Vec<InterpValue> = args
            .iter()
            .map(|arg| vm_to_interp_callable(arg, Rc::clone(&globals)))
            .collect();
        match func.call(&mut interp_args, &dummy_env) {
            Ok(result) => {
                for (vm_arg, interp_arg) in args.iter().zip(interp_args.iter()) {
                    sync_list_mutations(vm_arg, interp_arg);
                    sync_map_mutations(vm_arg, interp_arg);
                }
                Ok(interp_to_vm(result))
            }
            Err(FunctionCarrier::IntoEvaluationError(e)) => return Err(e.to_string()),
            Err(e) => return Err(e.to_string()),
        }
    };
    VmValue::Object(Box::new(VmObject::Function(VmFunction::Native(Rc::new(
        NativeFunction {
            func: Box::new(native),
            static_type,
        },
    )))))
}

pub fn vm_to_interp(value: &VmValue) -> InterpValue {
    match value {
        VmValue::Int(i) => InterpValue::Number(Number::Int(Int::Int64(*i))),
        VmValue::Float(f) => InterpValue::Number(Number::Float(*f)),
        VmValue::Bool(b) => InterpValue::Bool(*b),
        VmValue::None => InterpValue::Option(None),
        VmValue::Object(obj) => match obj.as_ref() {
            VmObject::Some(inner) => InterpValue::Option(Some(Box::new(vm_to_interp(inner)))),
            VmObject::BigInt(b) => InterpValue::Number(Number::Int(Int::BigInt(b.clone()))),
            VmObject::Complex(c) => InterpValue::Number(Number::Complex(*c)),
            VmObject::Rational(r) => InterpValue::Number(Number::Rational(Box::new(r.clone()))),
            VmObject::String(s) => InterpValue::Sequence(Sequence::String(s.clone())),
            VmObject::List(vs) => InterpValue::Sequence(Sequence::List(Rc::new(RefCell::new(
                vs.borrow().iter().map(vm_to_interp).collect(),
            )))),
            VmObject::Tuple(vs) => InterpValue::Sequence(Sequence::Tuple(Rc::new(
                vs.iter().map(vm_to_interp).collect(),
            ))),
            VmObject::Map { entries, default } => {
                let converted_entries: ndc_core::hash_map::HashMap<InterpValue, InterpValue> =
                    entries
                        .borrow()
                        .iter()
                        .map(|(k, v)| (vm_to_interp(k), vm_to_interp(v)))
                        .collect();
                let converted_default = default.as_ref().map(|d| Box::new(vm_to_interp(d)));
                InterpValue::Sequence(Sequence::Map(
                    Rc::new(RefCell::new(converted_entries)),
                    converted_default,
                ))
            }
            VmObject::Function(f) => {
                let static_type = f.static_type();
                let identity = f.prototype().map(|p| Rc::as_ptr(p) as usize);
                let vm_value = VmValue::Object(Box::new(VmObject::Function(f.clone())));
                let data: Rc<dyn std::any::Any> = Rc::new(VmFunctionWrapper {
                    vm_value,
                    identity,
                    call: None,
                });
                InterpValue::function(
                    FunctionBuilder::default()
                        .body(FunctionBody::Opaque { data, static_type })
                        .build()
                        .expect("must succeed"),
                )
            }
            VmObject::Iterator(iter) => {
                // If it's a range, preserve it as an interpreter range iterator so that
                // value_to_evaluated_index can recognise it for slicing (e.g. list[0..3]).
                // TODO: remove once the VM bridge (vm_bridge.rs) is gone.
                if let Some((start, end, inclusive)) = iter.borrow().range_bounds() {
                    let range_iter = if inclusive {
                        ValueIterator::ValueRangeInclusive(crate::iterator::ValueRangeInclusive(
                            start..=end,
                        ))
                    } else {
                        ValueIterator::ValueRange(crate::iterator::ValueRange(start..end))
                    };
                    return InterpValue::Sequence(Sequence::Iterator(Rc::new(RefCell::new(
                        range_iter,
                    ))));
                }
                // Materialize other iterators into a list for the interpreter side
                let mut values = Vec::new();
                let mut iter = iter.borrow_mut();
                if let Some(len) = iter.len() {
                    values.reserve(len);
                }
                while let Some(v) = iter.next() {
                    values.push(vm_to_interp(&v));
                }
                InterpValue::Sequence(Sequence::List(Rc::new(RefCell::new(values))))
            }
            VmObject::OverloadSet(slots) => InterpValue::function(
                FunctionBuilder::default()
                    .body(FunctionBody::Opaque {
                        data: Rc::new(VmValue::Object(Box::new(VmObject::OverloadSet(
                            slots.clone(),
                        )))),
                        static_type: ndc_parser::StaticType::Any,
                    })
                    .build()
                    .expect("must succeed"),
            ),
        },
    }
}

pub fn interp_to_vm(value: InterpValue) -> VmValue {
    match value {
        InterpValue::Number(Number::Int(Int::Int64(i))) => VmValue::Int(i),
        InterpValue::Number(Number::Int(Int::BigInt(b))) => {
            VmValue::Object(Box::new(VmObject::BigInt(b)))
        }
        InterpValue::Number(Number::Float(f)) => VmValue::Float(f),
        InterpValue::Number(Number::Rational(r)) => {
            VmValue::Object(Box::new(VmObject::Rational(*r)))
        }
        InterpValue::Number(Number::Complex(c)) => VmValue::Object(Box::new(VmObject::Complex(c))),
        InterpValue::Bool(b) => VmValue::Bool(b),
        InterpValue::Option(None) => VmValue::None,
        InterpValue::Option(Some(inner)) => {
            VmValue::Object(Box::new(VmObject::Some(interp_to_vm(*inner))))
        }
        InterpValue::Sequence(Sequence::String(s)) => {
            VmValue::Object(Box::new(VmObject::String(s)))
        }
        InterpValue::Sequence(Sequence::List(list)) => VmValue::Object(Box::new(VmObject::list(
            list.borrow()
                .iter()
                .map(|v| interp_to_vm(v.clone()))
                .collect(),
        ))),
        InterpValue::Sequence(Sequence::Tuple(tuple)) => VmValue::Object(Box::new(
            VmObject::Tuple(tuple.iter().map(|v| interp_to_vm(v.clone())).collect()),
        )),
        InterpValue::Sequence(Sequence::Map(map, default)) => {
            let entries = map
                .borrow()
                .iter()
                .map(|(k, v)| (interp_to_vm(k.clone()), interp_to_vm(v.clone())))
                .collect();
            let default = default.as_ref().map(|d| Box::new(interp_to_vm(*d.clone())));
            VmValue::Object(Box::new(VmObject::Map {
                entries: Rc::new(RefCell::new(entries)),
                default,
            }))
        }
        InterpValue::Sequence(Sequence::Iterator(iter)) => {
            let adapter = InterpIteratorAdapter { inner: iter };
            VmValue::iterator(Rc::new(RefCell::new(adapter)))
        }
        InterpValue::Sequence(seq) => {
            panic!("cannot convert {} to vm value", seq.static_type())
        }
        InterpValue::Function(f) => {
            if let FunctionBody::Opaque { data, .. } = f.body() {
                if let Some(wrapper) = data.downcast_ref::<VmFunctionWrapper>() {
                    return wrapper.vm_value.clone();
                }
            }
            panic!("cannot convert interpreter function to vm value")
        }
    }
}

/// Like `vm_to_interp` but converts VM function values into interpreter `NativeClosure`s
/// that can be called back by stdlib HOFs (e.g. `all`, `map`, `filter`).
fn vm_to_interp_callable(value: &VmValue, globals: Rc<Vec<VmValue>>) -> InterpValue {
    if let VmValue::Object(obj) = value {
        if let VmObject::Function(f) = obj.as_ref() {
            let identity = f.prototype().map(|p| Rc::as_ptr(p) as usize);
            let f = f.clone();
            let static_type = f.static_type();
            let vm_value = value.clone();
            let call: Rc<dyn Fn(&mut [InterpValue]) -> EvaluationResult> =
                Rc::new(move |args: &mut [InterpValue]| {
                    let vm_args: Vec<VmValue> =
                        args.iter().map(|a| interp_to_vm(a.clone())).collect();
                    let result = Vm::call_function(f.clone(), vm_args, (*globals).clone())
                        .map_err(|e| anyhow::anyhow!(e))?;
                    Ok(vm_to_interp(&result))
                });
            let data: Rc<dyn std::any::Any> = Rc::new(VmFunctionWrapper {
                vm_value,
                identity,
                call: Some(call),
            });
            return InterpValue::function(
                FunctionBuilder::default()
                    .body(FunctionBody::Opaque { data, static_type })
                    .build()
                    .expect("must succeed"),
            );
        }
    }
    vm_to_interp(value)
}

fn sync_map_mutations(vm_arg: &VmValue, interp_arg: &InterpValue) {
    let VmValue::Object(vm_obj) = vm_arg else {
        return;
    };
    let VmObject::Map {
        entries: vm_entries,
        ..
    } = vm_obj.as_ref()
    else {
        return;
    };
    let InterpValue::Sequence(Sequence::Map(interp_map, _)) = interp_arg else {
        return;
    };

    *vm_entries.borrow_mut() = interp_map
        .borrow()
        .iter()
        .map(|(k, v)| (interp_to_vm(k.clone()), interp_to_vm(v.clone())))
        .collect();
}

fn sync_list_mutations(vm_arg: &VmValue, interp_arg: &InterpValue) {
    let VmValue::Object(vm_obj) = vm_arg else {
        return;
    };
    let VmObject::List(vm_list) = vm_obj.as_ref() else {
        return;
    };
    let InterpValue::Sequence(Sequence::List(interp_list)) = interp_arg else {
        return;
    };

    *vm_list.borrow_mut() = interp_list
        .borrow()
        .iter()
        .map(|v| interp_to_vm(v.clone()))
        .collect();
}
