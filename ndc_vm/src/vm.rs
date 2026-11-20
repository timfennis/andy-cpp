use crate::error::VmError;
use crate::hash_map::{HashMap, hasher};
use crate::io::{InputSource, OutputSink};
use crate::iterator::{
    HeapIter, MapIter, RangeInclusiveIter, RangeIter, SeqIter, SharedIterator, StringIter,
    UnboundedRangeIter, VmIterator,
};
use crate::value::{CompiledFunction, Function};
use crate::{Natives, Object, Value};
use ndc_core::StaticType;
use ndc_lexer::Span;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::io::Write;
use std::rc::Rc;

/// Runs compiled code: stack code on a value stack, or register code (see
/// [`crate::register`]) on a register stack, which a VM made for register
/// code holds instead. The natives that take a VM are given this one, so a
/// native that calls back into the program, as `map` does, runs the
/// callback on the same VM, whichever code the program was compiled to.
pub struct Vm {
    /// The stack VM's value stack, or the register VM's registers: every
    /// running call's frame, one above the other, with
    /// [`crate::register::WINDOW`] values past the top frame's first
    /// register.
    pub(crate) stack: Vec<Value>,
    pub(crate) globals: Vec<Value>,
    /// The register VM's running calls.
    pub(crate) register_frames: crate::register::Frames,
    /// What the program's last expression gave, once register code halts.
    pub(crate) result: Value,
    /// Output sink for `print`, `dbg`, and similar I/O built-ins.
    output: OutputSink,
    /// Input source for `read_stdin`.
    input: InputSource,
    #[cfg(feature = "trace")]
    pub(crate) source: Option<String>,
    #[cfg(feature = "trace")]
    pub(crate) tracer: Option<Box<dyn crate::tracer::VmTracer>>,
}

/// The cache of a memoised function's results, and the key of the call
/// whose result goes in it when the call returns.
pub(crate) type MemoCache = (Rc<RefCell<HashMap<u64, Value>>>, u64);

/// How many calls deep a program can go. A frame takes about a hundred
/// bytes, so this is about 100 MB of frames.
pub const MAX_CALL_DEPTH: usize = 1_000_000;

/// The VM's globals for `natives`: native `i` is global slot `i`, and the
/// intrinsics follow. The analyser numbers them the same way. Each holds unit
/// until [`Vm::new`] puts the native there for code that reads it: a native
/// nothing uses is never made. An intrinsic's slot keeps unit: the compiler
/// writes it as bytecode where it's used and never reads its slot.
pub fn native_globals(natives: &Natives) -> Vec<Value> {
    vec![Value::unit(); natives.len() + natives.intrinsics().len()]
}

/// Puts each native `code` reads in its global slot, making the ones that
/// aren't made yet, before `code` runs at the top level.
pub(crate) fn fill_natives(globals: &mut [Value], natives: &Natives, code: &CompiledFunction) {
    for &slot in &code.natives {
        // An intrinsic's slot is never read: see `native_globals`.
        if slot >= natives.len() {
            continue;
        }
        let filled = matches!(&globals[slot], Value::Object(object)
            if matches!(object.as_ref(), Object::Function(Function::Native(_))));
        if !filled {
            globals[slot] = Value::Object(Rc::new(Object::Function(Function::Native(Rc::clone(
                natives.function(slot),
            )))));
        }
    }
}

impl Vm {
    /// A VM that runs `function` at the top level with `globals`, which
    /// [`native_globals`] starts, and which a VM that ran earlier code hands
    /// on. Each native `function` reads is put in its slot first, made from
    /// `natives` if it isn't made yet.
    pub fn new(function: CompiledFunction, mut globals: Vec<Value>, natives: &Natives) -> Self {
        // Every global the program declares has a slot, holding unit until its
        // declaration runs, like a local.
        if globals.len() < function.num_globals {
            globals.resize(function.num_globals, Value::unit());
        }
        fill_natives(&mut globals, natives, &function);
        Self {
            stack: vec![Value::None; crate::register::WINDOW],
            globals,
            register_frames: crate::register::Frames::top_level(Rc::new(function)),
            result: Value::unit(),
            output: OutputSink::Stdout,
            input: InputSource::Stdin,
            #[cfg(feature = "trace")]
            source: None,
            #[cfg(feature = "trace")]
            tracer: None,
        }
    }

    /// Replace the output sink used by I/O built-ins (e.g. `print`, `dbg`).
    /// Call this before `run()` to redirect output away from stdout.
    pub fn with_output(mut self, output: OutputSink) -> Self {
        self.output = output;
        self
    }

    /// Returns the captured output bytes, or `None` if this VM writes to stdout.
    pub fn get_output(&self) -> Option<&[u8]> {
        match &self.output {
            OutputSink::Stdout => None,
            OutputSink::Buffer(buf) => Some(buf),
        }
    }

    /// Replace the input source used by `read_stdin`. Call this before
    /// `run()` to read from somewhere other than stdin.
    pub fn with_input(mut self, input: InputSource) -> Self {
        self.input = input;
        self
    }

    /// The input source, with what is left of it to read.
    pub fn take_input(&mut self) -> InputSource {
        std::mem::replace(&mut self.input, InputSource::None)
    }

    /// Reads everything left on the VM's input source.
    pub fn read_input(&mut self) -> Result<String, VmError> {
        self.input.read_to_end()
    }

    /// Write a pre-formatted string to the VM's output sink.
    pub fn write_output(&mut self, s: &str) -> Result<(), VmError> {
        self.output
            .write_all(s.as_bytes())
            .map_err(|e| VmError::native(e.to_string()))
    }

    #[cfg(feature = "trace")]
    pub fn with_source(mut self, source: impl Into<String>) -> Self {
        self.source = Some(source.into());
        self
    }

    #[cfg(feature = "trace")]
    pub fn set_source(&mut self, source: impl Into<String>) {
        self.source = Some(source.into());
    }

    #[cfg(feature = "trace")]
    pub fn with_tracer(mut self, tracer: Box<dyn crate::tracer::VmTracer>) -> Self {
        self.tracer = Some(tracer);
        self
    }

    /// Runs the program until it ends or fails, and then tells the tracer,
    /// if there is one, that it's done.
    pub fn run(&mut self) -> Result<(), VmError> {
        let result = self.run_registers(0);
        #[cfg(feature = "trace")]
        if let Some(tracer) = &mut self.tracer {
            tracer.on_complete();
        }
        result
    }

    /// Call a function inline on this VM, without spawning a child VM.
    /// Used by `VmCallable::call` so stdlib HOFs run their predicates/mappers
    /// directly on the parent stack — zero allocation per callback invocation.
    pub fn call_callback(&mut self, func: Function, args: &[Value]) -> Result<Value, VmError> {
        self.register_callback(func, args)
    }

    /// The value of the program's last expression, once it ran: unit if
    /// that's a `;`-statement or a declaration.
    pub fn last_value(&self) -> Value {
        self.result.clone()
    }

    /// The globals as the program left them, for the next REPL line, and
    /// what is left on the stack: the locals of the top-level function, and
    /// of every function and block the program was in if it failed.
    pub fn into_globals_and_stack(self) -> (Vec<Value>, Vec<Value>) {
        (self.globals, self.stack)
    }

    /// Calls `callee`, an operator the analyser picked for each element
    /// position of the tuples among `arguments`, element by element, and
    /// returns the tuple of the results: `(1, 2) + (3, 4)` is
    /// `(1 + 3, 2 + 4)`. The callee is one function for every position, or
    /// a tuple of one for each. An argument that isn't a tuple is used at
    /// every position: `(1, 2) * 3` is `(3, 6)`. An error is at `span`, the
    /// call's, unless it has a place of its own.
    pub(crate) fn call_vec(
        &mut self,
        callee: &Value,
        arguments: &[Value],
        span: Span,
    ) -> Result<Value, VmError> {
        let unable = |callee: StaticType| {
            VmError::new(format!("Unable to invoke {callee} as a function."), span)
        };
        let Value::Object(callee) = callee else {
            return Err(unable(callee.static_type()));
        };
        let at_position = |i: usize| -> Option<&Function> {
            match callee.as_ref() {
                Object::Function(f) => Some(f),
                Object::Tuple(overloads) => match overloads.get(i)? {
                    Value::Object(obj) => match obj.as_ref() {
                        Object::Function(f) => Some(f),
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            }
        };
        let Some(first) = at_position(0) else {
            return Err(unable(callee.as_ref().static_type()));
        };
        let name = first.name();
        let Some(axis_len) = vec_axis_len(arguments) else {
            return Err(VmError::new(
                format!(
                    "vec call '{}' requires tuple arguments of equal non-zero length",
                    name.unwrap_or("?")
                ),
                span,
            ));
        };

        let mut elem_args: Vec<Value> = Vec::with_capacity(arguments.len());
        let mut results: Vec<Value> = Vec::with_capacity(axis_len);
        for i in 0..axis_len {
            elem_args.clear();
            elem_args.extend(arguments.iter().map(|arg| vec_element_at(arg, i)));
            let scalar = at_position(i)
                .cloned()
                .ok_or_else(|| VmError::new(format!("no function for tuple position {i}"), span))?;
            let result = self.call_callback(scalar, &elem_args).map_err(|mut e| {
                let prefix = match name {
                    Some(name) => format!("while vectorising '{name}' at index {i}: "),
                    None => format!("while vectorising at index {i}: "),
                };
                e.message = format!("{prefix}{}", e.message);
                e.span.get_or_insert(span);
                e
            })?;
            results.push(result);
        }
        Ok(Value::Object(Rc::new(Object::Tuple(results))))
    }
}

/// Writes the `out.len()` elements of `value` to `out`, in order, or says
/// why it can't: `(1, 2)` unpacks into two values, but not into three.
pub(crate) fn unpack_into(value: &Value, out: &mut [Value]) -> Result<(), String> {
    let size = out.len();
    let Value::Object(obj) = value else {
        return Err("cannot unpack: expected a list, tuple, or string".to_string());
    };
    match obj.as_ref() {
        Object::List(_) | Object::Tuple(_) => {
            let type_name = match obj.as_ref() {
                Object::List(_) => "list",
                Object::Tuple(_) => "tuple",
                _ => unreachable!(),
            };
            let len = match obj.as_ref() {
                Object::List(seq) => seq.borrow().len(),
                Object::Tuple(seq) => seq.len(),
                _ => unreachable!(),
            };
            if len != size {
                return Err(format!(
                    "cannot unpack a {type_name} of length {len} into {size} variables",
                ));
            }
            match obj.as_ref() {
                Object::List(seq) => out.clone_from_slice(&seq.borrow()),
                Object::Tuple(seq) => out.clone_from_slice(seq),
                _ => unreachable!(),
            }
        }
        Object::String(s) => {
            let s = s.borrow();
            let mut iter = s.chars();
            for (unpacked, slot) in out.iter_mut().enumerate() {
                match iter.next() {
                    Some(c) => *slot = Value::string(c.to_string()),
                    None => {
                        return Err(format!(
                            "cannot unpack a string of length {unpacked} into {size} variables",
                        ));
                    }
                }
            }
            if iter.next().is_some() {
                return Err(format!(
                    "cannot unpack a string into {size} variables: string is too long",
                ));
            }
        }
        // A deque, heap or iterator unpacks the elements it gives, one
        // past the variables at most, as an iterator may not end.
        _ => {
            let Some(iter) = value.clone().try_into_iter() else {
                return Err(
                    "cannot unpack: expected a list, tuple, string, deque, heap or iterator"
                        .to_string(),
                );
            };
            let elements: Vec<Value> = iter.take(size + 1).collect();
            if elements.len() != size {
                return Err(if elements.len() > size {
                    format!("cannot unpack more than {size} values into {size} variables")
                } else {
                    format!(
                        "cannot unpack {} values into {size} variables",
                        elements.len()
                    )
                });
            }
            for (slot, element) in out.iter_mut().zip(elements) {
                *slot = element;
            }
        }
    }
    Ok(())
}

/// An iterator over `value`: the value itself if it's an iterator, and one
/// over its elements if it's a list, tuple, deque, string, map or heap.
pub(crate) fn iterator_of(value: &Value) -> Result<Value, String> {
    let not_iterable = || format!("{} is not iterable", value.static_type());
    let Value::Object(rc) = value else {
        return Err(not_iterable());
    };
    let iter: SharedIterator = match rc.as_ref() {
        Object::Iterator(_) => return Ok(value.clone()),
        Object::List(_) | Object::Tuple(_) | Object::Deque(_) => {
            Rc::new(RefCell::new(SeqIter::new(Rc::clone(rc))))
        }
        Object::String(s) => Rc::new(RefCell::new(StringIter::new(Rc::clone(s)))),
        Object::Map { .. } => Rc::new(RefCell::new(MapIter::new(Rc::clone(rc)))),
        Object::MinHeap(h) => Rc::new(RefCell::new(HeapIter::new_min(h))),
        Object::MaxHeap(h) => Rc::new(RefCell::new(HeapIter::new_max(h))),
        _ => return Err(not_iterable()),
    };
    Ok(Value::iterator(iter))
}

/// The key a memoised function's result for `arguments` is cached under.
/// Arguments `==` calls equal share a result: a `pure fn` takes only values
/// it compares by what they are, which hash alike when they're equal.
pub(crate) fn memo_key(arguments: &[Value]) -> u64 {
    let mut hasher = hasher();
    for argument in arguments {
        argument.hash(&mut hasher);
    }
    hasher.finish()
}

/// Find the broadcast-axis length for a vec call: the shared length of every
/// tuple-shaped argument. Empty tuples and length mismatches return `None`,
/// which `CallVec` reports as an error.
fn vec_axis_len(args: &[Value]) -> Option<usize> {
    let mut axis: Option<usize> = None;
    for arg in args {
        if let Value::Object(obj) = arg
            && let Object::Tuple(elems) = obj.as_ref()
        {
            if elems.is_empty() {
                return None;
            }
            match axis {
                None => axis = Some(elems.len()),
                Some(n) if n == elems.len() => {}
                _ => return None,
            }
        }
    }
    axis
}

/// Pick the per-element value at vec position `i`. Tuple args contribute
/// `tuple[i]`; non-tuple args broadcast unchanged.
fn vec_element_at(arg: &Value, i: usize) -> Value {
    if let Value::Object(obj) = arg
        && let Object::Tuple(elems) = obj.as_ref()
    {
        return elems[i].clone();
    }
    arg.clone()
}

/// A callable VM function bound to the parent VM, for use in `VmNative` HOF
/// implementations. Produced by the `&mut VmCallable` input-type handler in
/// `ndc_macros::vm_convert`.
///
/// Calling `call()` runs the function inline on the parent VM via
/// `Vm::call_callback` — no child VM is allocated.
pub struct VmCallable<'a> {
    pub function: Function,
    pub vm: &'a mut Vm,
}

impl VmCallable<'_> {
    /// Call this function with the given arguments, running inline on the
    /// parent VM.
    pub fn call(&mut self, args: &[Value]) -> Result<Value, VmError> {
        self.vm.call_callback(self.function.clone(), args)
    }
}

/// What a call fails with when it would go more than [`MAX_CALL_DEPTH`]
/// calls deep.
#[cold]
pub(crate) fn calls_too_deep() -> VmError {
    VmError::native(format!(
        "calls go more than {MAX_CALL_DEPTH} deep, which usually means a function calls itself without end"
    ))
}

/// What a call of a [`Function::Checked`] fails with when the function it
/// checks doesn't take `arguments`.
#[cold]
pub(crate) fn refused_arguments(arguments: &[Value]) -> VmError {
    VmError::native(format!(
        "the function doesn't take the arguments ({})",
        arguments
            .iter()
            .map(|argument| argument.diagnostic_type().to_string())
            .collect::<Vec<_>>()
            .join(", ")
    ))
}

/// The analyser makes a condition a `Bool`; this is what a VM that's given
/// something else says.
#[cold]
pub(crate) fn not_a_bool(value: &Value) -> String {
    format!(
        "mismatched types: expected Bool, found {}",
        value.diagnostic_type()
    )
}

/// The analyser makes a range bound an `Int`; this is what a VM that's
/// given something else says.
#[cold]
fn bad_range_bound(value: &Value) -> String {
    format!("range bounds must be Int, got {}", value.diagnostic_type())
}

/// The range from `start` to `end`, which includes `end` if `inclusive`,
/// or which doesn't end without one: `1..4`, `1..=3`, `1..`. The bounds
/// are `Int`s; `end` is checked first.
pub(crate) fn make_range(
    start: &Value,
    end: Option<&Value>,
    inclusive: bool,
) -> Result<Value, String> {
    let end = match end {
        Some(Value::Int(end)) => Some(*end),
        Some(end) => return Err(bad_range_bound(end)),
        None => None,
    };
    let Value::Int(start) = *start else {
        return Err(bad_range_bound(start));
    };
    let iter: Rc<RefCell<dyn VmIterator>> = match (inclusive, end) {
        (_, None) => Rc::new(RefCell::new(UnboundedRangeIter::new(start))),
        (false, Some(end)) => Rc::new(RefCell::new(RangeIter::new(start, end))),
        (true, Some(end)) => Rc::new(RefCell::new(RangeInclusiveIter::new(start, end))),
    };
    Ok(Value::iterator(iter))
}

/// Replaces `value` with `Some` of it, made the `target` as
/// [`Value::converted_to`] does, or with `None` when it isn't one.
#[cold]
pub(crate) fn try_cast_value(value: &mut Value, target: &StaticType) {
    *value = Value::option(value.converted_to(target));
}

/// The element of `tuple` at `index`, counting a negative index from the
/// end: `-1` is the last element.
pub(crate) fn tuple_element(tuple: &Value, index: i64) -> Result<Value, String> {
    if let Value::Object(object) = tuple
        && let Object::Tuple(elements) = object.as_ref()
    {
        let position = if index < 0 {
            elements.len().checked_sub(index.unsigned_abs() as usize)
        } else {
            usize::try_from(index).ok()
        };
        return match position.and_then(|position| elements.get(position)) {
            Some(element) => Ok(element.clone()),
            None => Err(tuple_index_out_of_bounds(index, elements.len())),
        };
    }
    Err(format!("cannot index into {}", tuple.static_type()))
}

/// The analyser rejects an index written in the code that's outside the
/// tuple; this is what a VM that's given one says.
#[cold]
fn tuple_index_out_of_bounds(index: i64, length: usize) -> String {
    let plural = if length == 1 { "" } else { "s" };
    format!("index {index} is out of bounds for a tuple of {length} element{plural}")
}

/// Checks that `value` is a `target`, and makes it one: a function that may
/// be a function `target` gets its calls checked (see
/// [`Value::converted_to`]).
pub(crate) fn convert(value: &mut Value, target: &StaticType) -> bool {
    if !matches!(target, StaticType::Function { .. }) {
        return value.conforms_to(target);
    }
    match value.converted_to(target) {
        Some(converted) => {
            *value = converted;
            true
        }
        None => false,
    }
}

/// Checks that `returned`, the result of a call of a [`Function::Checked`],
/// is a `returns`, and makes it one, as [`convert`] does: a function it
/// returns may get its own calls checked.
pub(crate) fn convert_returned(returned: &mut Value, returns: &StaticType) -> Result<(), String> {
    if convert(returned, returns) {
        return Ok(());
    }
    Err(format!(
        "expected the function to return {returns} but it returned {}",
        returned.diagnostic_type()
    ))
}

/// What a cast of `value` that fails says: `cannot cast String to Int`.
#[cold]
pub(crate) fn cannot_cast(value: &Value, target: &StaticType) -> String {
    format!("cannot cast {} to {target}", value.diagnostic_type())
}
