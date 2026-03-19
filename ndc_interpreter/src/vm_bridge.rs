use std::cell::RefCell;
use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::collections::VecDeque;
use std::io::Write;
use std::rc::Rc;

use ndc_core::StaticType;
use ndc_core::int::Int;
use ndc_core::num::Number;
use ndc_vm::Vm;
use ndc_vm::VmError;
use ndc_vm::VmIterator;
use ndc_vm::value::{
    Function as VmFunction, NativeFunc, NativeFunction, Object as VmObject, OrdValue,
    Value as VmValue,
};

use crate::environment::{Environment, InterpreterOutput};

/// Wraps a shared interpreter output `Rc` as a plain `Write` for the VM.
/// Writes go to the same buffer that `Environment::get_output()` reads from,
/// so output captured by VM-native built-ins (e.g. `print`) is visible after
/// the VM finishes running.
pub(crate) struct WriteSink(pub Rc<RefCell<Box<dyn InterpreterOutput>>>);

impl Write for WriteSink {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.borrow_mut().write(buf)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.0.borrow_mut().flush()
    }
}
use crate::evaluate::EvaluationResult;
use crate::function::{FunctionBody, FunctionBuilder, FunctionCarrier, VmFunctionWrapper};
use crate::iterator::ValueIterator;
use crate::sequence::Sequence;
use crate::value::Value as InterpValue;

/// Adapter that wraps an interpreter ValueIterator as a VM VmIterator
struct InterpIteratorAdapter {
    inner: Rc<RefCell<ValueIterator>>,
}

impl VmIterator for InterpIteratorAdapter {
    fn next(&mut self) -> Option<VmValue> {
        let val = self.inner.borrow_mut().next()?;
        Some(interp_to_vm(val))
    }

    fn range_bounds(&self) -> Option<(i64, i64, bool)> {
        let iter = self.inner.borrow();
        match &*iter {
            ValueIterator::ValueRange(r) => Some((r.0.start, r.0.end, false)),
            ValueIterator::ValueRangeInclusive(r) => Some((*r.0.start(), *r.0.end(), true)),
            _ => None,
        }
    }

    fn unbounded_range_start(&self) -> Option<i64> {
        let iter = self.inner.borrow();
        match &*iter {
            ValueIterator::ValueRangeFrom(r) => Some(r.0.start),
            _ => None,
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn deep_copy(&self) -> Option<ndc_vm::SharedIterator> {
        let cloned = ValueIterator::clone(&*self.inner.borrow());
        Some(Rc::new(RefCell::new(InterpIteratorAdapter {
            inner: Rc::new(RefCell::new(cloned)),
        })))
    }
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
                let vm_value = VmValue::Object(Rc::new(VmObject::Function(f.clone())));
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
                // If this is an interpreter iterator that was wrapped for the VM, unwrap it
                // directly to preserve shared iterator state (e.g. for repeat().transposed).
                // TODO: remove once the VM bridge (vm_bridge.rs) is gone.
                {
                    let borrowed = iter.borrow();
                    if let Some(adapter) = borrowed.as_any().downcast_ref::<InterpIteratorAdapter>()
                    {
                        return InterpValue::Sequence(Sequence::Iterator(Rc::clone(
                            &adapter.inner,
                        )));
                    }
                }
                // If it's a RepeatIter, convert back to an interpreter Repeat iterator.
                // TODO: remove once the VM bridge (vm_bridge.rs) is gone.
                {
                    let borrowed = iter.borrow();
                    if let Some(rep) = borrowed.as_any().downcast_ref::<ndc_vm::RepeatIter>() {
                        let value = vm_to_interp(rep.value());
                        let limit = rep.remaining();
                        let repeat = crate::iterator::Repeat {
                            value,
                            cur: 0,
                            limit,
                        };
                        return InterpValue::Sequence(Sequence::Iterator(Rc::new(RefCell::new(
                            ValueIterator::Repeat(repeat),
                        ))));
                    }
                }
                // If it's a range, preserve it as an interpreter range iterator so that
                // value_to_evaluated_index can recognise it for slicing (e.g. list[0..3]).
                // TODO: remove once the VM bridge (vm_bridge.rs) is gone.
                if let Some(start) = iter.borrow().unbounded_range_start() {
                    let range_iter =
                        ValueIterator::ValueRangeFrom(crate::iterator::ValueRangeFrom(start..));
                    return InterpValue::Sequence(Sequence::Iterator(Rc::new(RefCell::new(
                        range_iter,
                    ))));
                }
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
            VmObject::Deque(d) => InterpValue::Sequence(Sequence::Deque(Rc::new(RefCell::new(
                d.borrow().iter().map(vm_to_interp).collect::<VecDeque<_>>(),
            )))),
            VmObject::MinHeap(h) => {
                use crate::heap::MinHeap;
                InterpValue::Sequence(Sequence::MinHeap(Rc::new(RefCell::new(
                    h.borrow()
                        .iter()
                        .map(|Reverse(v)| vm_to_interp(&v.0))
                        .collect::<MinHeap>(),
                ))))
            }
            VmObject::MaxHeap(h) => {
                use crate::heap::MaxHeap;
                InterpValue::Sequence(Sequence::MaxHeap(Rc::new(RefCell::new(
                    h.borrow()
                        .iter()
                        .map(|v| vm_to_interp(&v.0))
                        .collect::<MaxHeap>(),
                ))))
            }
            VmObject::OverloadSet(slots) => InterpValue::function(
                FunctionBuilder::default()
                    .body(FunctionBody::Opaque {
                        data: Rc::new(VmValue::Object(Rc::new(VmObject::OverloadSet(
                            slots.clone(),
                        )))),
                        static_type: StaticType::Any,
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
            VmValue::Object(Rc::new(VmObject::BigInt(b)))
        }
        InterpValue::Number(Number::Float(f)) => VmValue::Float(f),
        InterpValue::Number(Number::Rational(r)) => {
            VmValue::Object(Rc::new(VmObject::Rational(*r)))
        }
        InterpValue::Number(Number::Complex(c)) => VmValue::Object(Rc::new(VmObject::Complex(c))),
        InterpValue::Bool(b) => VmValue::Bool(b),
        InterpValue::Option(None) => VmValue::None,
        InterpValue::Option(Some(inner)) => {
            VmValue::Object(Rc::new(VmObject::Some(interp_to_vm(*inner))))
        }
        InterpValue::Sequence(Sequence::String(s)) => VmValue::Object(Rc::new(VmObject::String(s))),
        InterpValue::Sequence(Sequence::List(list)) => VmValue::Object(Rc::new(VmObject::list(
            list.borrow()
                .iter()
                .map(|v| interp_to_vm(v.clone()))
                .collect(),
        ))),
        InterpValue::Sequence(Sequence::Tuple(tuple)) => VmValue::Object(Rc::new(VmObject::Tuple(
            tuple.iter().map(|v| interp_to_vm(v.clone())).collect(),
        ))),
        InterpValue::Sequence(Sequence::Map(map, default)) => {
            let entries = map
                .borrow()
                .iter()
                .map(|(k, v)| (interp_to_vm(k.clone()), interp_to_vm(v.clone())))
                .collect();
            let default = default.as_ref().map(|d| interp_to_vm(*d.clone()));
            VmValue::Object(Rc::new(VmObject::Map {
                entries: RefCell::new(entries),
                default,
            }))
        }
        InterpValue::Sequence(Sequence::Iterator(iter)) => {
            let adapter = InterpIteratorAdapter { inner: iter };
            VmValue::iterator(Rc::new(RefCell::new(adapter)))
        }
        InterpValue::Sequence(Sequence::Deque(d)) => {
            VmValue::Object(Rc::new(VmObject::Deque(RefCell::new(
                d.borrow()
                    .iter()
                    .map(|v| interp_to_vm(v.clone()))
                    .collect::<VecDeque<_>>(),
            ))))
        }
        InterpValue::Sequence(Sequence::MinHeap(h)) => {
            let entries: BinaryHeap<Reverse<OrdValue>> = h
                .borrow()
                .iter()
                .map(|v| Reverse(OrdValue(interp_to_vm(v.0.0.clone()))))
                .collect();
            VmValue::Object(Rc::new(VmObject::MinHeap(RefCell::new(entries))))
        }
        InterpValue::Sequence(Sequence::MaxHeap(h)) => {
            let entries: BinaryHeap<OrdValue> = h
                .borrow()
                .iter()
                .map(|v| OrdValue(interp_to_vm(v.0.clone())))
                .collect();
            VmValue::Object(Rc::new(VmObject::MaxHeap(RefCell::new(entries))))
        }
        InterpValue::Function(f) => {
            if let FunctionBody::VmNative { native, .. } = f.body() {
                return VmValue::Object(Rc::new(VmObject::Function(VmFunction::Native(
                    Rc::clone(native),
                ))));
            }
            if let FunctionBody::Opaque { data, .. } = f.body() {
                if let Some(wrapper) = data.downcast_ref::<VmFunctionWrapper>() {
                    return wrapper.vm_value.clone();
                }
            }
            panic!("cannot convert interpreter function to vm value")
        }
    }
}

/// Convert an interpreter value to a VM value for the inverted bridge
/// (`call_vm_native`). Unlike `interp_to_vm`, this handles interpreter-side
/// closures by wrapping them in a `VmNativeFunction` that calls back into the
/// interpreter. Containers (Tuple, List) are converted element-wise using this
/// function so nested closures are also handled.
fn interp_to_vm_for_inverted_bridge(value: &InterpValue) -> VmValue {
    if let InterpValue::Function(f) = value {
        // VmNative — lossless round-trip: extract the native directly
        if let FunctionBody::VmNative { native, .. } = f.body() {
            return VmValue::Object(Rc::new(VmObject::Function(VmFunction::Native(Rc::clone(
                native,
            )))));
        }
        // Opaque wrapping a VmFunctionWrapper — already a VM value
        if let FunctionBody::Opaque { data, .. } = f.body() {
            if let Some(wrapper) = data.downcast_ref::<VmFunctionWrapper>() {
                return wrapper.vm_value.clone();
            }
        }
        // Interpreter closure passed as a callback to a VmNative HOF.
        // Wrap it so the VM can call it by routing back through the interpreter.
        let f = f.clone();
        let name = f.name().to_string();
        let static_type = f.static_type();
        let callback = Rc::new(NativeFunction {
            name,
            documentation: None, // TODO: figure this out
            static_type,
            func: NativeFunc::Simple(Box::new(move |vm_args: &[VmValue]| {
                let mut interp_args: Vec<InterpValue> = vm_args.iter().map(vm_to_interp).collect();
                let dummy_env = Rc::new(RefCell::new(Environment::new(Box::new(Vec::<u8>::new()))));
                f.call(&mut interp_args, &dummy_env)
                    .map(|v| interp_to_vm(v))
                    .map_err(|e| VmError::native(e.to_string()))
            })),
        });
        return VmValue::Object(Rc::new(VmObject::Function(VmFunction::Native(callback))));
    }
    // Containers may contain closures — recurse element-wise.
    if let InterpValue::Sequence(seq) = value {
        match seq {
            Sequence::Tuple(rc) => {
                return VmValue::Object(Rc::new(VmObject::Tuple(
                    rc.iter().map(interp_to_vm_for_inverted_bridge).collect(),
                )));
            }
            Sequence::List(rc) => {
                return VmValue::Object(Rc::new(VmObject::list(
                    rc.borrow()
                        .iter()
                        .map(interp_to_vm_for_inverted_bridge)
                        .collect(),
                )));
            }
            _ => {}
        }
    }
    interp_to_vm(value.clone())
}

/// Call a `VmNativeFunction` from the interpreter side (the inverted bridge).
///
/// Converts interpreter args → VM values, invokes the native closure,
/// syncs back any mutations on heap-allocated containers, then converts
/// the result back to an interpreter value.
pub(crate) fn call_vm_native(
    native: &Rc<NativeFunction>,
    args: &mut [InterpValue],
) -> EvaluationResult {
    // 1. Convert each arg to VmValue
    let vm_args: Vec<VmValue> = args.iter().map(interp_to_vm_for_inverted_bridge).collect();

    // 2. Call the vm_native closure
    let vm_result = match &native.func {
        NativeFunc::Simple(f) => f(&vm_args),
        NativeFunc::WithVm(f) => f(&vm_args, &mut Vm::stub()),
    }
    .map_err(|e| FunctionCarrier::IntoEvaluationError(Box::new(anyhow::anyhow!(e.message))))?;

    // 3. Sync mutations back (vm → interp direction).
    //    Strings do NOT need syncing — interp_to_vm_for_inverted_bridge shares the
    //    Rc<RefCell<String>>, so mutations are already visible on both sides.
    for (vm_arg, interp_arg) in vm_args.iter().zip(args.iter_mut()) {
        sync_list_mutations_to_interp(vm_arg, interp_arg);
        sync_map_mutations_to_interp(vm_arg, interp_arg);
        sync_deque_mutations_to_interp(vm_arg, interp_arg);
        sync_min_heap_mutations_to_interp(vm_arg, interp_arg);
        sync_max_heap_mutations_to_interp(vm_arg, interp_arg);
    }

    // 4. Convert result back
    Ok(vm_to_interp(&vm_result))
}

fn sync_list_mutations_to_interp(vm_arg: &VmValue, interp_arg: &mut InterpValue) {
    let VmValue::Object(vm_obj) = vm_arg else {
        return;
    };
    let VmObject::List(vm_list) = vm_obj.as_ref() else {
        return;
    };
    let InterpValue::Sequence(Sequence::List(interp_list)) = interp_arg else {
        return;
    };
    *interp_list.borrow_mut() = vm_list.borrow().iter().map(vm_to_interp).collect();
}

fn sync_map_mutations_to_interp(vm_arg: &VmValue, interp_arg: &mut InterpValue) {
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
    *interp_map.borrow_mut() = vm_entries
        .borrow()
        .iter()
        .map(|(k, v)| (vm_to_interp(k), vm_to_interp(v)))
        .collect();
}

fn sync_deque_mutations_to_interp(vm_arg: &VmValue, interp_arg: &mut InterpValue) {
    let VmValue::Object(vm_obj) = vm_arg else {
        return;
    };
    let VmObject::Deque(vm_deque) = vm_obj.as_ref() else {
        return;
    };
    let InterpValue::Sequence(Sequence::Deque(interp_deque)) = interp_arg else {
        return;
    };
    *interp_deque.borrow_mut() = vm_deque.borrow().iter().map(vm_to_interp).collect();
}

fn sync_min_heap_mutations_to_interp(vm_arg: &VmValue, interp_arg: &mut InterpValue) {
    let VmValue::Object(vm_obj) = vm_arg else {
        return;
    };
    let VmObject::MinHeap(vm_heap) = vm_obj.as_ref() else {
        return;
    };
    let InterpValue::Sequence(Sequence::MinHeap(interp_heap)) = interp_arg else {
        return;
    };
    use crate::heap::MinHeap;
    *interp_heap.borrow_mut() = vm_heap
        .borrow()
        .iter()
        .map(|Reverse(v)| vm_to_interp(&v.0))
        .collect::<MinHeap>();
}

fn sync_max_heap_mutations_to_interp(vm_arg: &VmValue, interp_arg: &mut InterpValue) {
    let VmValue::Object(vm_obj) = vm_arg else {
        return;
    };
    let VmObject::MaxHeap(vm_heap) = vm_obj.as_ref() else {
        return;
    };
    let InterpValue::Sequence(Sequence::MaxHeap(interp_heap)) = interp_arg else {
        return;
    };
    use crate::heap::MaxHeap;
    *interp_heap.borrow_mut() = vm_heap
        .borrow()
        .iter()
        .map(|v| vm_to_interp(&v.0))
        .collect::<MaxHeap>();
}
