//! Calls every native function with sample values of the types its signature
//! takes, and fails on a runtime error about types: the type checker trusts
//! the signatures, so a native that takes less than its signature says lets
//! a program pass the checker and then fail as it runs. Errors about values,
//! as a division by zero or an index out of bounds, are what values can do.

use ndc_interpreter::{Interpreter, InterpreterError, StaticType};
use std::panic::{AssertUnwindSafe, catch_unwind};

/// Natives this test doesn't call: they print, read files, stop the program,
/// or take any number of arguments.
const SKIPPED: &[&str] = &["print", "dbg", "read_file", "panic", "docs", "[any]", "zip"];

/// What a runtime error about a value's type says, as opposed to one about
/// its value: the natives' own words, and, with [`is_about_a_type`], the
/// argument errors the stdlib's macros and operators make.
const TYPE_ERRORS: &[&str] = &[
    "cannot compare",
    "requires a sequence",
    "sequence is required",
    "expected a sequence",
    "must return a number",
    "cannot determine the length",
    "is not supported for",
    "must be iterable",
    "cannot convert",
    "cannot use non-range iterator",
    "cannot unpack",
    "mismatched types",
    "expected a tuple",
    "expected one tuple",
    "expected the function to return",
];

/// Whether `message` is about a value's type rather than its value. Besides
/// [`TYPE_ERRORS`], an argument a native didn't take says what it expected
/// and what it got: `arg 1: expected List, got Int` from the macros,
/// `expected (Int, Int), got (Int, String)` from the operators, `expected 2
/// arguments, got 3`. An error about a value says what's wrong with it
/// instead: `arg 2: expected a non-negative integer, but the value was
/// negative`.
fn is_about_a_type(message: &str) -> bool {
    TYPE_ERRORS.iter().any(|error| message.contains(error))
        || (message.contains("expected ") && message.contains(", got "))
}

/// The types each type parameter is tried as.
fn choices() -> Vec<StaticType> {
    vec![
        StaticType::Int,
        StaticType::String,
        StaticType::Float,
        StaticType::Bool,
        StaticType::List(Box::new(StaticType::Int)),
    ]
}

/// Source for a few values of type `typ`, or none when it has none this
/// test can write.
fn samples(typ: &StaticType, depth: usize) -> Vec<String> {
    let block = |name: &str, body: String| format!("{{ let {name}: {typ} = {body}; {name} }}");
    match typ {
        StaticType::Int => vec!["3".into(), "0".into(), "-2".into()],
        StaticType::Float => vec!["1.5".into(), "-0.5".into()],
        StaticType::Number => vec!["Number(3)".into(), "(1n / 3n)".into()],
        StaticType::String => vec!["\"ab\"".into(), "\"12\"".into(), "\"\"".into()],
        StaticType::Bool => vec!["true".into()],
        StaticType::Any => vec![
            "{ let a: Any = 3; a }".into(),
            "{ let a: Any = \"s\"; a }".into(),
            "{ let a: Any = [1, 2]; a }".into(),
        ],
        _ if depth > 2 => Vec::new(),
        StaticType::List(element) => {
            let elements = first(samples(element, depth + 1), 2);
            let mut out = vec![block("l", "[]".into())];
            if !elements.is_empty() {
                out.push(block("l", format!("[{}]", elements.join(", "))));
            }
            out
        }
        StaticType::Option(inner) => {
            let mut out = vec![block("o", "None".into())];
            out.extend(
                first(samples(inner, depth + 1), 1)
                    .into_iter()
                    .map(|v| format!("Some({v})")),
            );
            out
        }
        StaticType::Tuple(elements) if elements.is_empty() => vec!["()".into()],
        StaticType::Tuple(elements) => {
            let parts = elements
                .iter()
                .map(|element| first(samples(element, depth + 1), 1).pop())
                .collect::<Option<Vec<_>>>();
            parts.map_or_else(Vec::new, |parts| {
                let comma = if parts.len() == 1 { "," } else { "" };
                vec![format!("({}{comma})", parts.join(", "))]
            })
        }
        StaticType::Map { key, value } => {
            let mut out = vec![block("m", "%{}".into())];
            if let (Some(k), Some(v)) = (
                first(samples(key, depth + 1), 1).pop(),
                first(samples(value, depth + 1), 1).pop(),
            ) {
                out.push(block("m", format!("%{{{k}: {v}}}")));
            }
            out
        }
        StaticType::Deque(element) => {
            let pushes = first(samples(element, depth + 1), 2)
                .into_iter()
                .map(|v| format!("d.push_back({v}); "))
                .collect::<String>();
            vec![format!("{{ let d: {typ} = Deque(); {pushes}d }}")]
        }
        StaticType::MinHeap(element) | StaticType::MaxHeap(element) => {
            let make = if matches!(typ, StaticType::MinHeap(_)) {
                "MinHeap"
            } else {
                "MaxHeap"
            };
            let pushes = first(samples(element, depth + 1), 2)
                .into_iter()
                .map(|v| format!("h.push({v}); "))
                .collect::<String>();
            vec![format!("{{ let h: {typ} = {make}(); {pushes}h }}")]
        }
        StaticType::Iterator(element) => first(samples(element, depth + 1), 1)
            .into_iter()
            .map(|v| format!("repeat({v}).take(2)"))
            .collect(),
        StaticType::Iterable(element) => {
            let mut out = Vec::new();
            for container in [
                StaticType::List(element.clone()),
                StaticType::Deque(element.clone()),
                StaticType::MinHeap(element.clone()),
                StaticType::Iterator(element.clone()),
            ] {
                out.extend(samples(&container, depth).pop());
            }
            match element.as_ref() {
                StaticType::Int => out.push("(0..3)".into()),
                StaticType::String => out.push("\"abc\"".into()),
                _ => {}
            }
            out
        }
        StaticType::Function {
            parameters: Some(parameters),
            return_type,
        } => first(samples(return_type, depth + 1), 1)
            .into_iter()
            .map(|result| {
                let names = (0..parameters.len())
                    .map(|i| format!("p{i}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("fn({names}) => {result}")
            })
            .collect(),
        _ => Vec::new(),
    }
}

fn first(mut values: Vec<String>, count: usize) -> Vec<String> {
    values.truncate(count);
    values
}

/// `typ` with each type parameter named in `names` as the type at the same
/// place in `types`.
fn instantiate(typ: &StaticType, names: &[Box<str>], types: &[StaticType]) -> StaticType {
    typ.replace(&|part| match part {
        StaticType::Generic(name) => names
            .iter()
            .position(|named| named == name)
            .map(|index| types[index].clone()),
        _ => None,
    })
}

/// A call of the native `name` with these arguments, written the way the
/// language writes it.
fn call(name: &str, arguments: &[String]) -> Option<String> {
    const BINARY: &[&str] = &[
        "in", "!=", "%", "%%", "&", "*", "+", "++", "-", "/", "<", "<<", "<=", "<=>", "<>", "==",
        ">", ">=", ">=<", ">>", "\\", "^", "|", "~",
    ];
    const ASSIGNING: &[&str] = &["&=", "++=", "-=", "|=", "~="];
    Some(match (name, arguments) {
        (op, [left, right]) if BINARY.contains(&op) => format!("(({left}) {op} ({right}))"),
        ("!" | "-" | "~", [operand]) => format!("({name}({operand}))"),
        ("[]", [value, index]) => format!("(({value})[{index}])"),
        ("[]=", [value, index, new]) => format!("{{ let t = {value}; t[{index}] = {new}; t }}"),
        (op, [target, value]) if ASSIGNING.contains(&op) => {
            format!("{{ let t = {target}; t {op} {value}; t }}")
        }
        _ if name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '?') =>
        {
            format!("{name}({})", arguments.join(", "))
        }
        _ => return None,
    })
}

/// Every combination of one value from each of `pools`, at most `limit`.
fn combinations(pools: &[Vec<String>], limit: usize) -> Vec<Vec<String>> {
    let mut out = vec![Vec::new()];
    for pool in pools {
        out = out
            .into_iter()
            .flat_map(|prefix| {
                pool.iter().map(move |value| {
                    let mut next = prefix.clone();
                    next.push(value.clone());
                    next
                })
            })
            .take(limit)
            .collect();
    }
    out
}

/// Whether `message`, an error from a call of `name` whose signature is
/// `signature`, is one this test allows: one about a value, or one a known
/// gap explains.
fn is_allowed(name: &str, signature: &StaticType, message: &str) -> bool {
    if !is_about_a_type(message) {
        return true;
    }
    // An `Any` is checked as the program runs, which is what it's for.
    if signature.contains(&|part| *part == StaticType::Any) {
        return true;
    }
    // A range's type is an `Iterator<Int>` until ranges have one of their own: #400.
    matches!(name, "[]" | "[]=") && message.contains("cannot use non-range iterator")
}

#[test]
fn every_native_takes_what_its_signature_says() {
    let mut interpreter = Interpreter::one_shot();
    interpreter.configure(ndc_stdlib::register);
    let natives = interpreter
        .functions()
        .filter(|native| !SKIPPED.contains(&native.name.as_str()))
        .map(|native| (native.name.clone(), native.static_type.clone()))
        .collect::<Vec<_>>();

    let mut failures = Vec::new();
    let mut calls = 0;
    for (name, signature) in &natives {
        let StaticType::Function {
            parameters: Some(parameters),
            ..
        } = signature
        else {
            continue;
        };
        let names = signature.type_parameters();
        // Each type parameter as each choice, and, for two or more, each as
        // a different one, which `<>(A, B)` takes.
        let choices = choices();
        let instances = if names.is_empty() {
            vec![Vec::new()]
        } else {
            let mut instances = choices
                .iter()
                .map(|choice| vec![choice.clone(); names.len()])
                .collect::<Vec<_>>();
            if names.len() > 1 {
                instances.extend((1..choices.len()).map(|shift| {
                    (0..names.len())
                        .map(|index| choices[(index * shift + shift) % choices.len()].clone())
                        .collect()
                }));
            }
            instances
        };
        for types in instances {
            let pools = parameters
                .iter()
                .map(|parameter| samples(&instantiate(parameter, &names, &types), 0))
                .collect::<Vec<_>>();
            if pools.iter().any(Vec::is_empty) {
                continue;
            }
            for arguments in combinations(&pools, 4) {
                let Some(expression) = call(name, &arguments) else {
                    continue;
                };
                let program = format!("let result = {expression};");
                calls += 1;
                match catch_unwind(AssertUnwindSafe(|| interpreter.eval(&program))) {
                    Err(_) => {
                        failures.push(format!("{name}: panicked on `{program}`"));
                        interpreter = Interpreter::one_shot();
                        interpreter.configure(ndc_stdlib::register);
                    }
                    Ok(Err(InterpreterError::Vm(error)))
                        if !is_allowed(name, signature, &error.message) =>
                    {
                        failures.push(format!(
                            "{name} {signature}: {} on `{program}`",
                            error.message
                        ));
                    }
                    Ok(_) => {}
                }
            }
        }
    }
    assert!(calls > 1000, "only {calls} calls were made");
    assert!(
        failures.is_empty(),
        "{} of {calls} calls failed with a type error:\n{}",
        failures.len(),
        failures.join("\n")
    );
}

#[test]
fn an_error_about_a_type_is_told_from_one_about_a_value() {
    for about_a_type in [
        "arg 1: expected List, got Int",
        "expected (Int, Int), got (Int, String)",
        "expected 2 arguments, got 3",
        "cannot compare Int and String",
        "sum requires a sequence",
        "comparator must return a number, got Bool",
        "mismatched types: expected Bool, found Int",
    ] {
        assert!(is_about_a_type(about_a_type), "{about_a_type}");
    }
    for about_a_value in [
        "arg 2: expected a non-negative integer, but the value was negative",
        "expected an exact integer Number",
        "division by zero",
        "index out of bounds",
        "comparator must return a real number, got a complex number",
        "empty input to max",
    ] {
        assert!(!is_about_a_type(about_a_value), "{about_a_value}");
    }
}
