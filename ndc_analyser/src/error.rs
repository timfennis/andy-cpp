use itertools::Itertools;
use ndc_core::StaticType;
use ndc_core::static_type::StaticTypeConstructionError;
use ndc_lexer::Span;
use ndc_resolver::NameResolutionError;

#[derive(thiserror::Error, Debug)]
#[error("{text}")]
pub struct AnalysisError {
    text: String,
    span: Span,
    help_text: Option<String>,
    /// Other places that explain the error, each with what it says there.
    notes: Vec<(Span, String)>,
}

impl From<NameResolutionError> for AnalysisError {
    fn from(error: NameResolutionError) -> Self {
        Self {
            text: error.to_string(),
            span: error.span(),
            help_text: error.help_text(),
            notes: Vec::new(),
        }
    }
}

impl AnalysisError {
    pub fn span(&self) -> Span {
        self.span
    }

    /// Advice on how to fix the error, rendered as a separate note under the
    /// snippet rather than crammed into the message.
    pub fn help_text(&self) -> Option<&str> {
        self.help_text.as_deref()
    }

    /// Other places that explain the error, each with what it says there.
    pub fn notes(&self) -> &[(Span, String)] {
        &self.notes
    }

    /// This error, found in a generic function's body while checking a call
    /// of it, as the call's error. `calls` are the calls it came through,
    /// outermost first, each with the function it calls: in
    /// `fn yy(l) => l.len(); yy(5)`, the call `yy(5)` gets the error that
    /// there's no `len(Int)`, with a note at `l.len()`.
    pub(crate) fn through_calls<'a>(
        mut self,
        calls: impl IntoIterator<Item = (&'a str, Span)>,
    ) -> Self {
        let calls = calls.into_iter().collect::<Vec<_>>();
        let Some(&(_, outermost)) = calls.first() else {
            return self;
        };
        let mut notes = calls
            .windows(2)
            .map(|pair| {
                let [(caller, _), (callee, call)] = pair else {
                    unreachable!("windows of two");
                };
                (*call, format!("`{caller}` calls `{callee}` here"))
            })
            .collect::<Vec<_>>();
        let (innermost, _) = calls[calls.len() - 1];
        notes.push((self.span, format!("`{innermost}` needs this")));
        notes.append(&mut self.notes);
        self.notes = notes;
        self.span = outermost;
        self
    }

    pub(crate) fn invalid_type_annotation(err: &StaticTypeConstructionError, span: Span) -> Self {
        Self {
            text: err.to_string(),
            span,
            help_text: Some(err.help_text().to_string()),
            notes: Vec::new(),
        }
    }

    pub(crate) fn type_does_not_take_generic_args(name: &str, span: Span) -> Self {
        Self {
            text: format!("type `{name}` does not take generic arguments"),
            span,
            help_text: None,
            notes: Vec::new(),
        }
    }

    pub(crate) fn tuple_arity_mismatch(
        ident_len: usize,
        annotation_len: usize,
        span: Span,
    ) -> Self {
        Self {
            text: format!(
                "mismatched tuple arity: found a len={ident_len} identifier and a len={annotation_len} annotation."
            ),
            span,
            help_text: None,
            notes: Vec::new(),
        }
    }

    pub(crate) fn mismatched_types(found: &StaticType, expected: &StaticType, span: Span) -> Self {
        Self {
            text: format!("mismatched types: found {found} but expected {expected}"),
            span,
            help_text: None,
            notes: Vec::new(),
        }
    }

    pub(crate) fn impossible_cast(found: &StaticType, target: &StaticType, span: Span) -> Self {
        Self {
            text: format!("invalid cast: {found} can never be {target}"),
            span,
            help_text: None,
            notes: Vec::new(),
        }
    }

    pub(crate) fn function_redefinition(
        name: &str,
        parameters: Option<&[StaticType]>,
        span: Span,
    ) -> Self {
        let parameters_desc = match parameters {
            Some(types) => format!("({})", types.iter().join(", ")),
            None => "variadic parameters".to_string(),
        };
        Self {
            text: format!(
                "Illegal redefinition of function '{name}' with parameters {parameters_desc} in the same scope"
            ),
            span,
            help_text: Some(
                "Overloads in one scope need different parameter types, so annotate a parameter's type to tell them apart".to_string(),
            ),
            notes: Vec::new(),
        }
    }

    pub(crate) fn tied_overloads(
        name: &str,
        parameters: &[String],
        earlier: &[String],
        span: Span,
    ) -> Self {
        Self {
            text: format!(
                "A call could fit both {name}({}) and {name}({}), declared earlier in the same scope, and neither is more specific",
                parameters.join(", "),
                earlier.join(", "),
            ),
            span,
            help_text: Some(
                "A parameter without a type annotation (`_` here) takes any argument the body can work with. Annotate the parameters so that one overload is more specific, or so that they take different arguments".to_string(),
            ),
            notes: Vec::new(),
        }
    }

    pub(crate) fn unable_to_index_into(typ: &StaticType, span: Span) -> Self {
        Self {
            text: format!("Unable to index into {typ}"),
            span,
            help_text: None,
            notes: Vec::new(),
        }
    }
    pub(crate) fn unable_to_unpack_type(typ: &StaticType, span: Span) -> Self {
        Self {
            text: format!("Invalid unpacking of {typ}"),
            span,
            help_text: None,
            notes: Vec::new(),
        }
    }

    /// `hides_functions` says whether the variable hides functions with its
    /// name further out, which the help then explains.
    pub(crate) fn variable_rejects_arguments(
        name: &str,
        variable_type: &StaticType,
        types: &[StaticType],
        hides_functions: bool,
        span: Span,
    ) -> Self {
        let text = match variable_type {
            StaticType::Function { .. } => format!(
                "The variable '{name}' holds a function that doesn't accept the arguments '{}'",
                types.iter().join(", ")
            ),
            other => format!("The variable '{name}' is {other}, which can't be called"),
        };
        Self {
            text,
            span,
            help_text: hides_functions.then(|| {
                format!(
                    "A variable hides the functions named '{name}' further out: it doesn't join their overloads"
                )
            }),
            notes: Vec::new(),
        }
    }

    pub(crate) fn function_not_found(ident: &str, types: &[StaticType], span: Span) -> Self {
        Self {
            text: format!(
                "No function called '{ident}' found that matches the arguments '{}'",
                types.iter().join(", ")
            ),
            span,
            help_text: None,
            notes: Vec::new(),
        }
    }

    /// Same as [`Self::function_not_found`], for a call an overload would
    /// accept if the arguments were known more precisely.
    pub(crate) fn function_not_found_suggest_cast(
        ident: &str,
        types: &[StaticType],
        span: Span,
    ) -> Self {
        Self {
            help_text: Some(
                concat!(
                    "An overload would accept a narrower argument type. ",
                    "Cast to say what the value holds, as in `value as List<Int>`.",
                )
                .to_string(),
            ),
            ..Self::function_not_found(ident, types, span)
        }
    }

    /// `example` is one of the overloads' types, to show how to say which
    /// one is meant.
    pub(crate) fn ambiguous_function_value(
        name: &str,
        expected: &StaticType,
        example: &StaticType,
        span: Span,
    ) -> Self {
        let text = if *expected == StaticType::Any {
            format!("'{name}' has several overloads, and nothing here says which one is meant")
        } else {
            format!(
                "'{name}' has several overloads that are a {expected}, and nothing here says which one is meant"
            )
        };
        Self {
            text,
            span,
            help_text: Some(format!(
                "Say which one with a function type, as in `let f: {example} = {name}`"
            )),
            notes: Vec::new(),
        }
    }

    pub(crate) fn no_overload_fits(name: &str, expected: &StaticType, span: Span) -> Self {
        Self {
            text: format!("No overload of '{name}' is a {expected}"),
            span,
            help_text: None,
            notes: Vec::new(),
        }
    }

    pub(crate) fn break_outside_loop(span: Span) -> Self {
        Self {
            text: "unexpected break statement outside of loop".to_string(),
            span,
            help_text: None,
            notes: Vec::new(),
        }
    }

    pub(crate) fn continue_outside_loop(span: Span) -> Self {
        Self {
            text: "unexpected continue statement outside of loop".to_string(),
            span,
            help_text: None,
            notes: Vec::new(),
        }
    }

    pub(crate) fn return_outside_function(span: Span) -> Self {
        Self {
            text: "unexpected return statement outside of function body".to_string(),
            span,
            help_text: None,
            notes: Vec::new(),
        }
    }

    pub(crate) fn callee_rejects_arguments(
        callee: &StaticType,
        types: &[StaticType],
        span: Span,
    ) -> Self {
        Self {
            text: format!(
                "A function of type {callee} doesn't accept the arguments '{}'",
                types.iter().join(", ")
            ),
            span,
            help_text: None,
            notes: Vec::new(),
        }
    }

    pub(crate) fn not_callable(typ: &StaticType, span: Span) -> Self {
        Self {
            text: format!("Unable to invoke {typ} as a function."),
            span,
            help_text: None,
            notes: Vec::new(),
        }
    }
}
