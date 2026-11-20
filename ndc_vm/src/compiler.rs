use crate::value::CompiledFunction;
use ndc_analysis::Analysis;
use ndc_core::NodeId;
use ndc_core::collections::HashMap;
use ndc_lexer::Span;
use ndc_parser::ExpressionLocation;
use std::cell::RefCell;
use std::rc::Rc;

/// What compiling the earlier lines of a REPL session leaves for the lines
/// after them, which can call the functions those declared with new types.
/// After `fn inc(x) => x + 1`, the line `inc(2.5)` compiles the instance
/// `inc<Float>` from `inc`'s body and what the analyser worked out about it.
#[derive(Default)]
pub struct Session {
    /// What the analyser worked out about every line so far.
    pub(crate) analysis: Rc<Analysis>,
    /// The functions the lines declared in globals, by declaration.
    pub(crate) functions: HashMap<NodeId, DeclaredFunction>,
    /// As `Compiler::prototypes`, for every line so far.
    pub(crate) prototypes: Rc<RefCell<HashMap<NodeId, Rc<CompiledFunction>>>>,
}

/// A function declaration a REPL line made, which a later line may compile
/// instances of.
#[derive(Clone)]
pub(crate) struct DeclaredFunction {
    pub(crate) name: Option<String>,
    pub(crate) body: ExpressionLocation,
    pub(crate) pure: bool,
    pub(crate) span: Span,
}

#[derive(thiserror::Error, Debug)]
#[error("{text}")]
pub struct CompileError {
    text: String,
    span: Span,
}

impl CompileError {
    /// What the register compiler fails with for code that makes instances
    /// of a function declared before it, outside of a REPL line.
    pub(crate) fn uncompiled_instances() -> Self {
        Self {
            text: "encountered instances of a function compiled earlier outside of a REPL line, this is probably an internal error".to_string(),
            span: Span::synthetic(),
        }
    }

    /// What the register compiler fails with for a function whose values
    /// don't fit in the registers a frame can have.
    pub(crate) fn too_many_registers(span: Span) -> Self {
        Self {
            text: format!(
                "this function needs more than {} registers, which is more than the register VM has",
                crate::register::WINDOW
            ),
            span,
        }
    }

    pub(crate) fn unresolved_binding(span: Span) -> Self {
        Self {
            text: "encountered unresolved binding during compilation, this is probably an internal error".to_string(),
            span,
        }
    }

    /// What the register compiler fails with for a tuple index written in
    /// the code that doesn't fit its instruction, which no tuple type the
    /// analyser accepts has.
    pub(crate) fn index_too_large(span: Span) -> Self {
        Self {
            text: "this tuple index is larger than the register VM can say".to_string(),
            span,
        }
    }

    pub(crate) fn unresolved_cast(span: Span) -> Self {
        Self {
            text:
                "encountered unresolved cast during compilation, this is probably an internal error"
                    .to_string(),
            span,
        }
    }

    pub(crate) fn unnamed_intrinsic(span: Span) -> Self {
        Self {
            text: "encountered an intrinsic used other than by name during compilation, this is probably an internal error".to_string(),
            span,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }
}
