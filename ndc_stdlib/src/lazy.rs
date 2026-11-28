//! Lazy sequences: `lazy(xs)`, the functions that add a step to one, and
//! the functions that read one.
//!
//! A function that adds a step (`map`, `filter`, `take`, …) takes a `Lazy`
//! or a range without an end and gives a `Lazy`, without making any
//! elements. A function that reads one makes its elements with the VM, as
//! the steps may call functions: one that needs every element (`list`,
//! `sum`, …) makes them all first, and one that may stop early (`find`,
//! `any`, …) or calls a function of its own (`fold`, `count`, …) makes them
//! one at a time, so the steps' functions and its own run element by
//! element.

use crate::math::aggregate_values;
use crate::sequence::{try_sort_by, vm_try_extreme};
use crate::string::join_to_string;
use ndc_core::{FunctionRegistry, StaticType};
use ndc_vm::hash_map::HashMap;
use ndc_vm::value::{Function, NativeFunc, NumericMode, Object, Value};
use ndc_vm::{Lazy, LazyCursor, NativeSpec, Step, Vm, VmError};
use std::cmp::Ordering;
use std::rc::Rc;

/// A step that takes a function: its name, documentation, signatures and
/// how it's made from the function.
type FunctionStep = (
    &'static str,
    &'static str,
    [&'static str; 2],
    fn(Function) -> Step,
);

/// A step that takes a count of elements.
type CountStep = (&'static str, &'static str, fn(usize) -> Step);

pub fn register(env: &mut FunctionRegistry<NativeSpec>) {
    register_lazy(env);
    register_steps(env);
    register_readers(env);
}

/// `lazy(xs)`.
fn register_lazy(env: &mut FunctionRegistry<NativeSpec>) {
    let doc = "A lazy sequence of the elements of `seq`: functions like `map` and `filter` add a step to it without making any elements, and a `for` loop or a function that reads it makes them, each time it reads it. `[1, 2, 3].lazy.map(f).take(2)` calls `f` twice when it's read.";
    for signature in ["<T>(Iterable<T>) -> Lazy<T>", "<T>(Lazy<T>) -> Lazy<T>"] {
        simple(env, "lazy", doc, signature, |args| {
            Ok(lazy_value(lazy_of(&args[0])?))
        });
    }
}

/// The functions that add a step: each takes a `Lazy` or a `RangeFrom`,
/// which can't be read to its end, and gives a `Lazy`.
fn register_steps(env: &mut FunctionRegistry<NativeSpec>) {
    let with_function: [FunctionStep; 5] = [
        (
            "map",
            "Applies the function to each element of the lazy sequence, as it's read.",
            [
                "<T, U>(Lazy<T>, fn(T) -> U) -> Lazy<U>",
                "<U>(RangeFrom, fn(Int) -> U) -> Lazy<U>",
            ],
            Step::Map,
        ),
        (
            "filter",
            "Keeps the elements of the lazy sequence the function gives `true` for, as it's read.",
            [
                "<T>(Lazy<T>, fn(T) -> Bool) -> Lazy<T>",
                "(RangeFrom, fn(Int) -> Bool) -> Lazy<Int>",
            ],
            Step::Filter,
        ),
        (
            "flat_map",
            "Gives the elements of what the function gives for each element of the lazy sequence, in turn, as it's read.",
            [
                "<T, U>(Lazy<T>, fn(T) -> Iterable<U>) -> Lazy<U>",
                "<U>(RangeFrom, fn(Int) -> Iterable<U>) -> Lazy<U>",
            ],
            Step::FlatMap,
        ),
        (
            "take_while",
            "Keeps the elements of the lazy sequence up to the first the function gives `false` for.",
            [
                "<T>(Lazy<T>, fn(T) -> Bool) -> Lazy<T>",
                "(RangeFrom, fn(Int) -> Bool) -> Lazy<Int>",
            ],
            Step::TakeWhile,
        ),
        (
            "skip_while",
            "Leaves out the elements of the lazy sequence before the first the function gives `false` for.",
            [
                "<T>(Lazy<T>, fn(T) -> Bool) -> Lazy<T>",
                "(RangeFrom, fn(Int) -> Bool) -> Lazy<Int>",
            ],
            Step::SkipWhile,
        ),
    ];
    for (name, doc, signatures, step) in with_function {
        for signature in signatures {
            simple(env, name, doc, signature, move |args| {
                let function = function_of(&args[1])?;
                Ok(lazy_value(lazy_of(&args[0])?.then(step(function))))
            });
        }
    }

    // These pick elements without calling a function, so they're lazy for
    // any sequence, and a `for` loop reads one without leaving the VM's
    // dispatch loop: `for (i, x) in xs.enumerate()` makes no list.
    let with_count: [CountStep; 2] = [
        (
            "take",
            "A lazy sequence of the first `n` elements of `seq`.",
            Step::Take,
        ),
        (
            "skip",
            "A lazy sequence of the elements of `seq` after the first `n`.",
            Step::Skip,
        ),
    ];
    for (name, doc, step) in with_count {
        for signature in [
            "<T>(Lazy<T>, Int) -> Lazy<T>",
            "<T>(Iterable<T>, Int) -> Lazy<T>",
        ] {
            simple(env, name, doc, signature, move |args| {
                let count = count_of(name, &args[1])?;
                Ok(lazy_value(lazy_of(&args[0])?.then(step(count))))
            });
        }
    }

    for signature in [
        "<T>(Lazy<T>, Int) -> Lazy<[T]>",
        "<T>(Iterable<T>, Int) -> Lazy<[T]>",
    ] {
        simple(
            env,
            "combinations",
            "A lazy sequence of each list of `k` elements of `seq`, in the order they come, reading `seq` only as far as each list needs: `[1, 2, 3].combinations(2)` gives `[1, 2]`, `[1, 3]`, `[2, 3]`.",
            signature,
            |args| {
                let k = count_of("combinations", &args[1])?;
                Ok(lazy_value(lazy_of(&args[0])?.then(Step::Combinations(k))))
            },
        );
    }

    simple(
        env,
        "repeat",
        "A lazy sequence that gives `value` again and again, without end: `repeat(0).take(3)` gives `0`, `0`, `0`.",
        "<T>(T) -> Lazy<T>",
        |args| Ok(lazy_value(Lazy::repeat(args[0].clone()))),
    );

    for signature in [
        "<T>(Lazy<T>) -> Lazy<(Int, T)>",
        "<T>(Iterable<T>) -> Lazy<(Int, T)>",
    ] {
        simple(
            env,
            "enumerate",
            "A lazy sequence of each element of `seq` with its position: `(0, a)`, `(1, b)`, …",
            signature,
            |args| Ok(lazy_value(lazy_of(&args[0])?.then(Step::Enumerate))),
        );
    }
}

/// The functions that read a lazy sequence.
fn register_readers(env: &mut FunctionRegistry<NativeSpec>) {
    with_vm(
        env,
        "list",
        "Makes every element of the lazy sequence and gives them as a list.",
        "<T>(Lazy<T>) -> [T]",
        |args, vm| Ok(Value::list(lazy_of(&args[0])?.collect(vm)?)),
    );
    with_vm(
        env,
        "set",
        "Makes every element of the lazy sequence and gives them as a set.",
        "<T>(Lazy<T>) -> %{T}",
        |args, vm| {
            let entries: HashMap<Value, Value> = lazy_of(&args[0])?
                .collect(vm)?
                .into_iter()
                .map(|value| (value, Value::unit()))
                .collect();
            Ok(Value::Object(Rc::new(Object::map(entries, None))))
        },
    );
    with_vm(
        env,
        "sorted",
        "Makes every element of the lazy sequence and gives them sorted, as a list.",
        "<T>(Lazy<T>) -> [T]",
        |args, vm| {
            let mut values = lazy_of(&args[0])?.collect(vm)?;
            try_sort_by(&mut values, |a, b| a.try_cmp(b).map_err(VmError::native))?;
            Ok(Value::list(values))
        },
    );
    for (name, better) in [("max", Ordering::Greater), ("min", Ordering::Less)] {
        with_vm(
            env,
            name,
            "Makes every element of the lazy sequence and gives the largest (`max`) or smallest (`min`).",
            "<T>(Lazy<T>) -> T",
            move |args, vm| {
                let values = lazy_of(&args[0])?.collect(vm)?;
                vm_try_extreme(values.into_iter(), better)
                    .map_err(|error| VmError::native(error.to_string()))
            },
        );
    }
    for mode in NumericMode::ALL {
        for (name, product) in [("sum", false), ("product", true)] {
            env.declare_global_fn(NativeSpec::new(
                name,
                "Makes every element of the lazy sequence and gives their sum (`sum`) or product (`product`).",
                move || StaticType::Function {
                    parameters: Some(vec![StaticType::Lazy(Box::new(mode.static_type()))]),
                    return_type: Box::new(mode.static_type()),
                },
                move || {
                    NativeFunc::WithVm(Box::new(move |args, vm| {
                        let values = lazy_of(&args[0])?.collect(vm)?;
                        aggregate_values(values.iter(), mode, product)
                    }))
                },
            ));
        }
    }
    with_vm(
        env,
        "join",
        "Makes every element of the lazy sequence and joins them into a string, with `separator` between them.",
        "<T>(Lazy<T>, String) -> String",
        |args, vm| {
            let values = Value::list(lazy_of(&args[0])?.collect(vm)?);
            let separator = args[1].to_string();
            join_to_string(&values, &separator)
                .map(Value::string)
                .map_err(|error| VmError::native(error.to_string()))
        },
    );
    with_vm(
        env,
        "count",
        "Makes every element of the lazy sequence and gives how many there are.",
        "<T>(Lazy<T>) -> Int",
        |args, vm| {
            let mut cursor = lazy_of(&args[0])?.cursor();
            let mut count = 0;
            while cursor.next(vm)?.is_some() {
                count += 1;
            }
            Ok(Value::Int(count))
        },
    );
    with_vm(
        env,
        "count",
        "Gives how many elements of the lazy sequence the function gives `true` for.",
        "<T>(Lazy<T>, fn(T) -> Bool) -> Int",
        |args, vm| {
            let predicate = function_of(&args[1])?;
            let mut cursor = lazy_of(&args[0])?.cursor();
            let mut count = 0;
            while let Some(value) = cursor.next(vm)? {
                if test(vm, &predicate, &value)? {
                    count += 1;
                }
            }
            Ok(Value::Int(count))
        },
    );
    register_ends(env);
    for (name, doc, stop_on, found) in [
        (
            "any",
            "Whether the function gives `true` for an element of the lazy sequence, making no elements after the first it does.",
            true,
            true,
        ),
        (
            "all",
            "Whether the function gives `true` for every element of the lazy sequence, making no elements after the first it doesn't.",
            false,
            false,
        ),
        (
            "none",
            "Whether the function gives `false` for every element of the lazy sequence, making no elements after the first it doesn't.",
            true,
            false,
        ),
    ] {
        with_vm(
            env,
            name,
            doc,
            "<T>(Lazy<T>, fn(T) -> Bool) -> Bool",
            move |args, vm| {
                let predicate = function_of(&args[1])?;
                let mut cursor = lazy_of(&args[0])?.cursor();
                while let Some(value) = cursor.next(vm)? {
                    if test(vm, &predicate, &value)? == stop_on {
                        return Ok(Value::Bool(found));
                    }
                }
                Ok(Value::Bool(!found))
            },
        );
    }
    with_vm(
        env,
        "first_or",
        "Gives the first element of the lazy sequence, making no others, or `default` if it has none.",
        "<T>(Lazy<T>, T) -> T",
        |args, vm| {
            Ok(lazy_of(&args[0])?
                .cursor()
                .next(vm)?
                .unwrap_or_else(|| args[1].clone()))
        },
    );
    with_vm(
        env,
        "in",
        "Whether the element is one of the lazy sequence's, making no elements after it.",
        "<T>(T, Lazy<T>) -> Bool",
        |args, vm| {
            let mut cursor = lazy_of(&args[1])?.cursor();
            while let Some(value) = cursor.next(vm)? {
                if value == args[0] {
                    return Ok(Value::Bool(true));
                }
            }
            Ok(Value::Bool(false))
        },
    );
    with_vm(
        env,
        "fold",
        "Combines the elements of the lazy sequence one at a time with the function, starting from `initial`.",
        "<T, A>(Lazy<T>, A, fn(A, T) -> A) -> A",
        |args, vm| {
            let function = function_of(&args[2])?;
            let cursor = lazy_of(&args[0])?.cursor();
            fold(vm, cursor, args[1].clone(), &function)
        },
    );
    with_vm(
        env,
        "reduce",
        "Combines the elements of the lazy sequence one at a time with the function, starting from the first.",
        "<T>(Lazy<T>, fn(T, T) -> T) -> T",
        |args, vm| {
            let function = function_of(&args[1])?;
            let mut cursor = lazy_of(&args[0])?.cursor();
            let first = cursor
                .next(vm)?
                .ok_or_else(|| VmError::native("first argument to reduce must not be empty"))?;
            fold(vm, cursor, first, &function)
        },
    );
}

/// `initial` combined with each element `cursor` gives, by `function`.
fn fold(
    vm: &mut Vm,
    mut cursor: LazyCursor,
    initial: Value,
    function: &Function,
) -> Result<Value, VmError> {
    let mut accumulator = initial;
    while let Some(value) = cursor.next(vm)? {
        accumulator = vm.call_callback(function.clone(), &[accumulator, value])?;
    }
    Ok(accumulator)
}

/// An element of a lazy sequence that `first` or `last` reads: what it's
/// called, its documentation, its signature, whether it takes a function,
/// whether it reads to the end, and the error it gives when it finds no
/// element, if it doesn't give an `Option`.
type End = (
    &'static str,
    &'static str,
    &'static str,
    bool,
    bool,
    Option<&'static str>,
);

/// `first`, `last`, their `?` forms, each with and without a function, and
/// `find`, the same as `first` with a function.
fn register_ends(env: &mut FunctionRegistry<NativeSpec>) {
    let ends: [End; 9] = [
        (
            "first",
            "Gives the first element of the lazy sequence, making no others, or results in an error if it has none.",
            "<T>(Lazy<T>) -> T",
            false,
            false,
            Some("collection is empty"),
        ),
        (
            "first?",
            "Gives the first element of the lazy sequence, making no others, or `None` if it has none.",
            "<T>(Lazy<T>) -> Option<T>",
            false,
            false,
            None,
        ),
        (
            "last",
            "Gives the last element of the lazy sequence, making all of them, or results in an error if it has none.",
            "<T>(Lazy<T>) -> T",
            false,
            true,
            Some("collection is empty"),
        ),
        (
            "last?",
            "Gives the last element of the lazy sequence, making all of them, or `None` if it has none.",
            "<T>(Lazy<T>) -> Option<T>",
            false,
            true,
            None,
        ),
        (
            "first",
            "Gives the first element of the lazy sequence the function gives `true` for, making no elements after it, or results in an error if there's none.",
            "<T>(Lazy<T>, fn(T) -> Bool) -> T",
            true,
            false,
            Some("first did not find anything"),
        ),
        (
            "first?",
            "Gives the first element of the lazy sequence the function gives `true` for, making no elements after it, or `None` if there's none.",
            "<T>(Lazy<T>, fn(T) -> Bool) -> Option<T>",
            true,
            false,
            None,
        ),
        (
            "last",
            "Gives the last element of the lazy sequence the function gives `true` for, making all of them, or results in an error if there's none.",
            "<T>(Lazy<T>, fn(T) -> Bool) -> T",
            true,
            true,
            Some("last did not find anything"),
        ),
        (
            "last?",
            "Gives the last element of the lazy sequence the function gives `true` for, making all of them, or `None` if there's none.",
            "<T>(Lazy<T>, fn(T) -> Bool) -> Option<T>",
            true,
            true,
            None,
        ),
        (
            "find",
            "The same as `first` with a function: gives the first element of the lazy sequence the function gives `true` for, making no elements after it.",
            "<T>(Lazy<T>, fn(T) -> Bool) -> T",
            true,
            false,
            Some("find did not find anything"),
        ),
    ];
    for (name, doc, signature, takes_function, to_the_end, error) in ends {
        with_vm(env, name, doc, signature, move |args, vm| {
            let predicate = if takes_function {
                Some(function_of(&args[1])?)
            } else {
                None
            };
            let mut cursor = lazy_of(&args[0])?.cursor();
            let mut found = None;
            while let Some(value) = cursor.next(vm)? {
                let matches = match &predicate {
                    Some(predicate) => test(vm, predicate, &value)?,
                    None => true,
                };
                if matches {
                    found = Some(value);
                    if !to_the_end {
                        break;
                    }
                }
            }
            match error {
                Some(error) => found.ok_or_else(|| VmError::native(error)),
                None => Ok(Value::option(found)),
            }
        });
    }
}

/// What `predicate`, a function its type says gives a `Bool`, gives for
/// `value`.
fn test(vm: &mut Vm, predicate: &Function, value: &Value) -> Result<bool, VmError> {
    match vm.call_callback(predicate.clone(), std::slice::from_ref(value))? {
        Value::Bool(answer) => Ok(answer),
        _ => Err(VmError::native(
            "return value of predicate must be a boolean",
        )),
    }
}

/// The lazy sequence `value` is, or the one of its elements: a `Lazy`, or a
/// range without an end, which the steps' signatures take too.
fn lazy_of(value: &Value) -> Result<Lazy, VmError> {
    Lazy::new(value.clone())
        .ok_or_else(|| VmError::native(format!("{} is not a sequence", value.static_type())))
}

fn lazy_value(lazy: Lazy) -> Value {
    Value::Object(Rc::new(Object::Lazy(lazy)))
}

fn function_of(value: &Value) -> Result<Function, VmError> {
    match value {
        Value::Object(object) => match object.as_ref() {
            Object::Function(function) => Ok(function.clone()),
            _ => Err(VmError::native("expected a function")),
        },
        _ => Err(VmError::native("expected a function")),
    }
}

/// The `Int` in `value`, as a count of elements for `name`.
fn count_of(name: &str, value: &Value) -> Result<usize, VmError> {
    match value {
        Value::Int(n) => usize::try_from(*n).map_err(|_negative| {
            VmError::native(format!(
                "{name} expected a non-negative integer, but the value was negative: {n}"
            ))
        }),
        _ => Err(VmError::native(format!("{name} takes an Int count"))),
    }
}

fn simple(
    env: &mut FunctionRegistry<NativeSpec>,
    name: &'static str,
    documentation: &'static str,
    signature: &'static str,
    function: impl Fn(&[Value]) -> Result<Value, VmError> + 'static,
) {
    env.declare_global_fn(NativeSpec::with_signature(
        name,
        documentation,
        signature,
        || NativeFunc::Simple(Box::new(function)),
    ));
}

fn with_vm(
    env: &mut FunctionRegistry<NativeSpec>,
    name: &'static str,
    documentation: &'static str,
    signature: &'static str,
    function: impl Fn(&[Value], &mut Vm) -> Result<Value, VmError> + 'static,
) {
    env.declare_global_fn(NativeSpec::with_signature(
        name,
        documentation,
        signature,
        || NativeFunc::WithVm(Box::new(function)),
    ));
}
