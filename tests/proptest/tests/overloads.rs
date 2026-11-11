//! Property-based test for the analyser's overload choices.
//!
//! The analyser commits a call or a function value to an overload before
//! the types involved are fully known, so that information can flow into
//! lambdas and empty literals. That's only right if it's the overload the
//! call or value would get once the types are known, and in debug builds
//! the analyser asserts this for every early commit (`check_early_picks`).
//! It also asserts that no type variable is left unsettled.
//!
//! The token fuzzer in `panic.rs` rarely gets past the parser, so this one
//! generates well-formed programs instead, from the pieces that make the
//! analyser commit early: overloaded functions taking `Int`, `Float`,
//! `String`, lists and anything, empty literals settled by later uses,
//! lambdas, function values, `map`, `if`, index writes, `op=` and
//! annotations. Analysing each one must not panic. Type errors are
//! expected and ignored.
//!
//! * `known_overload_regressions_do_not_panic` replays the seeds saved in
//!   `overloads.regressions` and runs as part of `cargo test`.
//! * `fuzz_overload_choices` generates fresh programs and is `#[ignore]`d:
//!
//!   ```sh
//!   cargo test -p proptest_tests --test overloads -- --ignored
//!   PROPTEST_CASES=100000 cargo test -p proptest_tests --test overloads -- --ignored
//!   ```

use ndc_analyser::Analyser;
use ndc_core::FunctionRegistry;
use ndc_lexer::{Lexer, SourceId};
use ndc_parser::Parser;
use proptest::prelude::*;
use proptest::test_runner::FileFailurePersistence;
use std::fmt;
use std::panic;
use std::sync::Once;
use std::thread;
// The other harnesses in this crate run programs; this one only analyses.
use ndc_vm as _;
use num as _;

/// A generated program, printed as its source so a failure can be pasted
/// into a file and run.
#[derive(Clone)]
struct Program(String);

impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\n{}\n", self.0)
    }
}

/// The parameter types an overload can declare, `""` being none (`Any`).
const PARAMETER_TYPES: [&str; 5] = ["Int", "Float", "String", "List<Int>", ""];

/// What an overload can return, by the name of its parameter `x`.
const BODIES: [&str; 6] = ["1", "2.5", "\"s\"", "x", "[x]", "true"];

/// Overloads of `name` taking one parameter each, of different types so they
/// aren't redefinitions, and maybe one taking two.
fn arb_overloads(name: &'static str) -> impl Strategy<Value = String> {
    (
        prop::sample::subsequence(PARAMETER_TYPES.to_vec(), 1..=3),
        prop::collection::vec(prop::sample::select(BODIES.to_vec()), 3),
        prop::option::of(prop::sample::select(BODIES.to_vec())),
    )
        .prop_map(move |(types, bodies, two)| {
            let mut source = String::new();
            for (typ, body) in types.iter().zip(bodies) {
                let annotation = if typ.is_empty() {
                    String::new()
                } else {
                    format!(": {typ}")
                };
                source.push_str(&format!("fn {name}(x{annotation}) => {body};\n"));
            }
            if let Some(body) = two {
                source.push_str(&format!("fn {name}(x, y: Int) => {body};\n"));
            }
            source
        })
}

/// The names a value can be: variables, overloaded functions declared by
/// the program, and overloaded or generic built-ins.
const VALUES: [&str; 15] = [
    "v0", "v1", "v2", "v3", "p", "q", "abs", "split", "push", "len", "max", "first", "sum", "n",
    "S",
];

const LITERALS: [&str; 13] = [
    "1",
    "2.5",
    "\"s\"",
    "[]",
    "[1]",
    "[\"s\"]",
    "%{}",
    "%{\"k\": 1}",
    "None",
    "(1, 2)",
    "S(1)",
    "T(\"s\")",
    "[S(1)]",
];

/// A lambda taking `a`, whose body uses `a` in ways that need its type.
fn arb_lambda() -> impl Strategy<Value = String> {
    prop::sample::select(vec![
        "a",
        "p(a)",
        "q(a)",
        "a + 1",
        "a ++ \"!\"",
        "[a]",
        "a.len()",
        "if true { a } else { 1 }",
        "if true { a } else { [] }",
        "p(a, 1)",
        "a[0]",
        "a.n",
        "a.n + 1",
        "a as Int",
        "a.first()",
    ])
    .prop_map(|body| format!("fn(a) => {body}"))
}

/// An expression, nesting a few levels deep.
fn arb_expression() -> impl Strategy<Value = String> {
    let leaf = prop_oneof![
        3 => prop::sample::select(LITERALS.to_vec()).prop_map(str::to_string),
        3 => prop::sample::select(VALUES.to_vec()).prop_map(str::to_string),
        2 => arb_lambda(),
    ];
    leaf.prop_recursive(3, 16, 2, |inner| {
        prop_oneof![
            (
                prop::sample::select(vec!["p", "q", "abs", "len"]),
                inner.clone()
            )
                .prop_map(|(f, e)| format!("{f}({e})")),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("p({a}, {b})")),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("({a} + {b})")),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("[{a}, {b}]")),
            (inner.clone(), inner.clone())
                .prop_map(|(a, b)| format!("(if true {{ {a} }} else {{ {b} }})")),
            (
                inner.clone(),
                prop_oneof![
                    arb_lambda(),
                    prop::sample::select(VALUES.to_vec()).prop_map(str::to_string)
                ]
            )
                .prop_map(|(xs, f)| format!("{xs}.map({f})")),
            inner.clone().prop_map(|e| format!("{e}[0]")),
            inner.clone().prop_map(|e| format!("{e}.n")),
            inner
                .clone()
                .prop_map(|e| format!("{e}.fold(0, fn(acc, a) => acc + a)")),
            inner.clone().prop_map(|e| format!("({e} as List<Int>)")),
            inner.clone().prop_map(|e| format!("{e}[\"k\"]")),
            (inner.clone(), inner).prop_map(|(f, e)| format!("({f})({e})")),
        ]
    })
}

/// Annotations a `let` can have.
const ANNOTATIONS: [&str; 8] = [
    "Int",
    "String",
    "List<Int>",
    "List<String>",
    "Fn(Int) -> Int",
    "Fn(String) -> List<String>",
    "List<Fn(Int) -> Int>",
    "Any",
];

const VARIABLES: [&str; 4] = ["v0", "v1", "v2", "v3"];

/// A statement on the variables `v0` to `v3`.
fn arb_statement() -> impl Strategy<Value = String> {
    let variable = || prop::sample::select(VARIABLES.to_vec());
    prop_oneof![
        3 => (variable(), arb_expression()).prop_map(|(v, e)| format!("{v}.push({e});")),
        2 => (variable(), arb_expression()).prop_map(|(v, e)| format!("{v}[0] = {e};")),
        1 => (variable(), arb_expression()).prop_map(|(v, e)| format!("{v}[\"k\"] = {e};")),
        1 => (variable(), arb_expression()).prop_map(|(v, e)| format!("{v}.n = {e};")),
        1 => (variable(), arb_expression()).prop_map(|(v, e)| format!("{v}[0] += {e};")),
        2 => (variable(), arb_expression()).prop_map(|(v, e)| format!("{v} += {e};")),
        2 => (variable(), arb_expression()).prop_map(|(v, e)| format!("{v} = {e};")),
        2 => (prop::sample::select(ANNOTATIONS.to_vec()), arb_expression())
            .prop_map(|(t, e)| format!("let w: {t} = {e};")),
        2 => arb_expression().prop_map(|e| format!("{e};")),
        1 => (variable(), arb_expression()).prop_map(|(v, e)| format!("for e in {v} {{ {e}; }}")),
    ]
}

fn arb_program() -> impl Strategy<Value = Program> {
    (
        arb_overloads("p"),
        arb_overloads("q"),
        prop::collection::vec(prop::sample::select(LITERALS.to_vec()), 4),
        prop::collection::vec(arb_statement(), 1..=6),
    )
        .prop_map(|(p, q, initial, statements)| {
            // Two structs with a field of the same name, so `x.n` and the
            // getter `n` have overloads to choose from.
            let mut source = format!("struct S {{ n: Int }}\nstruct T {{ n: String }}\n{p}{q}");
            for (variable, value) in VARIABLES.iter().zip(initial) {
                source.push_str(&format!("let {variable} = {value};\n"));
            }
            for statement in statements {
                source.push_str(&statement);
                source.push('\n');
            }
            Program(source)
        })
}

const WORKER_THREAD_NAME: &str = "proptest-overload-worker";

/// Silences panics on the worker thread, so shrinking doesn't print each
/// attempt's panic; the final one is resumed on the test thread.
fn install_quiet_panic_hook() {
    static HOOK: Once = Once::new();
    HOOK.call_once(|| {
        let default_hook = panic::take_hook();
        panic::set_hook(Box::new(move |info| {
            if thread::current().name() != Some(WORKER_THREAD_NAME) {
                default_hook(info);
            }
        }));
    });
}

/// Analyses `program` on a worker thread, so its panic can be silenced and
/// resumed here for proptest to shrink.
fn analyse_quietly(program: Program) {
    install_quiet_panic_hook();
    let result = thread::Builder::new()
        .name(WORKER_THREAD_NAME.into())
        .spawn(move || analyse(&program))
        .expect("spawn worker thread")
        .join();
    if let Err(payload) = result {
        panic::resume_unwind(payload);
    }
}

/// Analyses `program` with the standard library's functions, as the
/// interpreter would. Analysis panics on a failed debug assertion.
fn analyse(program: &Program) {
    let Ok(tokens) = Lexer::new(&program.0, SourceId::SYNTHETIC).collect::<Result<Vec<_>, _>>()
    else {
        return;
    };
    let Ok(expressions) = Parser::from_tokens(tokens).parse() else {
        return;
    };
    let mut registry = FunctionRegistry::default();
    ndc_stdlib::register(&mut registry);
    let natives = registry
        .iter()
        .map(|function| (function.name.clone(), function.static_type.clone()))
        .collect();
    let mut analyser = Analyser::new(natives);
    for expression in &expressions {
        analyser.analyse(expression);
    }
    analyser.take_result();
}

fn persistence_config(cases: u32) -> ProptestConfig {
    ProptestConfig {
        cases,
        max_shrink_iters: 4096,
        failure_persistence: Some(Box::new(FileFailurePersistence::WithSource("regressions"))),
        ..ProptestConfig::default()
    }
}

proptest! {
    // cases=0 only replays the seeds in `overloads.regressions`.
    #![proptest_config(persistence_config(0))]

    #[test]
    fn known_overload_regressions_do_not_panic(program in arb_program()) {
        analyse_quietly(program);
    }
}

proptest! {
    #![proptest_config(persistence_config(1024))]

    #[test]
    #[ignore = "fuzz test; opt-in via `cargo test ... -- --ignored`"]
    fn fuzz_overload_choices(program in arb_program()) {
        analyse_quietly(program);
    }
}
