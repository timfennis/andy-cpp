use std::cell::RefCell;
use std::rc::Rc;

use ndc_core::int::Int;
use ndc_core::num::Number;
use ndc_vm::value::{Function as VmFunction, NativeFunction, Object as VmObject, Value as VmValue};

use crate::environment::Environment;
use crate::function::Function as InterpFunction;
use crate::sequence::Sequence;
use crate::value::Value as InterpValue;

pub fn make_vm_globals(env: &Rc<RefCell<Environment>>) -> Vec<VmValue> {
    env.borrow()
        .get_all_functions()
        .into_iter()
        .map(|func| wrap_function(func, Rc::clone(env)))
        .collect()
}

fn wrap_function(func: InterpFunction, dummy_env: Rc<RefCell<Environment>>) -> VmValue {
    let static_type = func.static_type();
    let native = move |args: &[VmValue]| -> VmValue {
        let mut interp_args: Vec<InterpValue> = args.iter().map(vm_to_interp).collect();
        match func.call(&mut interp_args, &dummy_env) {
            Ok(result) => {
                for (vm_arg, interp_arg) in args.iter().zip(interp_args.iter()) {
                    sync_list_mutations(vm_arg, interp_arg);
                }
                interp_to_vm(result)
            }
            Err(e) => panic!("stdlib function failed: {:?}", e),
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
            VmObject::String(s) => {
                InterpValue::Sequence(Sequence::String(Rc::new(RefCell::new(s.clone()))))
            }
            VmObject::List(vs) => InterpValue::Sequence(Sequence::List(Rc::new(RefCell::new(
                vs.borrow().iter().map(vm_to_interp).collect(),
            )))),
            VmObject::Tuple(vs) => InterpValue::Sequence(Sequence::Tuple(Rc::new(
                vs.iter().map(vm_to_interp).collect(),
            ))),
            VmObject::Function(_) => panic!("cannot convert vm function to interpreter value"),
            VmObject::OverloadSet(_) => panic!("cannot convert overload set to interpreter value"),
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
            VmValue::Object(Box::new(VmObject::String(s.borrow().clone())))
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
        InterpValue::Sequence(seq) => {
            panic!("cannot convert {} to vm value", seq.static_type())
        }
        InterpValue::Function(_) => panic!("cannot convert interpreter function to vm value"),
    }
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
