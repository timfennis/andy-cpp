//! Andy C++ in the browser.
//!
//! ```js
//! import init, { Session } from "./pkg/ndc_wasm.js";
//! await init();
//! const session = new Session();
//! session.eval("let x = 20;");
//! const result = session.eval("print(x); x + 22");
//! result.output; // "20\n"
//! result.value;  // "42"
//! ```

use ndc_interpreter::Interpreter;
use wasm_bindgen::prelude::*;

// Linked only for the `wasm_js` features turned on in Cargo.toml.
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
use {getrandom_03 as _, getrandom_04 as _};

/// A REPL-like session: each evaluation sees what earlier ones declared.
#[wasm_bindgen]
pub struct Session {
    interpreter: Interpreter,
    /// How many bytes of the interpreter's captured output were already
    /// handed out by earlier evaluations.
    printed: usize,
    evaluations: usize,
}

/// What one evaluation printed and produced.
#[wasm_bindgen]
pub struct Evaluation {
    /// What the code printed.
    #[wasm_bindgen(getter_with_clone)]
    pub output: String,
    /// The value of the last expression, as the REPL would show it; empty for
    /// statements and unit.
    #[wasm_bindgen(getter_with_clone)]
    pub value: String,
    /// The rendered diagnostics, if the code failed.
    #[wasm_bindgen(getter_with_clone)]
    pub error: Option<String>,
    /// True when the code failed past parsing, so the session forgot what
    /// earlier evaluations declared.
    pub started_over: bool,
}

#[wasm_bindgen]
impl Session {
    #[wasm_bindgen(constructor)]
    #[must_use]
    pub fn new() -> Self {
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        console_error_panic_hook::set_once();

        let mut interpreter = Interpreter::capturing();
        interpreter.configure(ndc_stdlib::register);
        Self {
            interpreter,
            printed: 0,
            evaluations: 0,
        }
    }

    pub fn eval(&mut self, code: &str) -> Evaluation {
        self.evaluations += 1;
        let name = format!("<cell:{}>", self.evaluations);
        let result = self.interpreter.eval_named(name, code);
        let output = self.take_output();
        match result {
            Ok(value) => Evaluation {
                output,
                value: value.to_string(),
                error: None,
                started_over: false,
            },
            Err(err) => {
                let started_over = !matches!(
                    err,
                    ndc_interpreter::InterpreterError::Lexer { .. }
                        | ndc_interpreter::InterpreterError::Parser { .. }
                );
                Evaluation {
                    output,
                    value: String::new(),
                    error: Some(ndc_diagnostics::render(self.interpreter.source_db(), err)),
                    started_over,
                }
            }
        }
    }

    /// What was printed since the last call.
    fn take_output(&mut self) -> String {
        let all = self.interpreter.get_output().unwrap_or_default();
        let new = String::from_utf8_lossy(&all[self.printed..]).into_owned();
        self.printed = all.len();
        new
    }
}

impl Default for Session {
    fn default() -> Self {
        Self::new()
    }
}

/// Runs a whole program in a fresh session.
#[wasm_bindgen]
#[must_use]
pub fn run(code: &str) -> Evaluation {
    Session::new().eval(code)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_session_remembers_declarations_and_splits_output() {
        let mut session = Session::new();
        let first = session.eval("let x = 20; print(\"hi\");");
        assert_eq!(first.output, "hi\n");
        assert!(first.error.is_none());

        let second = session.eval("print(x); x + 22");
        assert_eq!(second.output, "20\n");
        assert_eq!(second.value, "42");
    }

    #[test]
    fn errors_are_rendered_with_the_source() {
        let result = run("let x = 1 / 0;");
        let error = result.error.expect("dividing by zero should fail");
        assert!(error.contains("<cell:1>"), "{error}");
    }
}
