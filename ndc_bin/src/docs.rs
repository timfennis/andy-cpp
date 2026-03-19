use ndc_core::{Parameter, StaticType, TypeSignature};
use ndc_interpreter::Interpreter;
use ndc_stdlib::WithStdlib;
use std::cmp::Ordering;
use std::fmt::Write;
use strsim::normalized_damerau_levenshtein;
use termimad::crossterm::style::Stylize;
use termimad::{Alignment, MadSkin};

/// Returns `true` if `needle` is a substring of `haystack` or if they are at least 80% similar
fn string_match(needle: &str, haystack: &str) -> bool {
    haystack.contains(needle) || normalized_damerau_levenshtein(needle, haystack) > 0.8
}

pub fn docs(query: Option<&str>) -> anyhow::Result<()> {
    let interpreter = Interpreter::new(Vec::new()) // Discard the output
        .with_stdlib();

    let mut functions: Vec<_> = interpreter
        .functions()
        .filter(|func| {
            if let Some(query) = query {
                string_match(query, &func.name)
            } else {
                true
            }
        })
        .collect();

    functions.sort_by(|l, r| {
        if let Some(query) = query {
            normalized_damerau_levenshtein(&l.name, query)
                .partial_cmp(&normalized_damerau_levenshtein(&r.name, query))
                .unwrap_or(Ordering::Equal)
                .reverse()
        } else {
            l.name.cmp(&r.name)
        }
    });

    let mut skin = MadSkin::default();

    skin.headers[0].align = Alignment::Left;
    skin.headers[1].align = Alignment::Left;
    skin.headers[2].align = Alignment::Left;

    for function in functions {
        let (type_sig, return_type) = match &function.static_type {
            StaticType::Function {
                parameters,
                return_type,
            } => {
                let sig = match parameters {
                    None => TypeSignature::Variadic,
                    Some(types) => TypeSignature::Exact(
                        types
                            .iter()
                            .enumerate()
                            .map(|(i, t)| Parameter::new(format!("arg{i}"), t.clone()))
                            .collect(),
                    ),
                };
                (sig, return_type.as_ref())
            }
            other => (TypeSignature::Variadic, other),
        };

        let mut signature = String::new();
        match &type_sig {
            TypeSignature::Variadic => {
                write!(signature, "(*args**)")?;
            }
            TypeSignature::Exact(params) => {
                write!(signature, "(")?;
                let mut param_iter = params.iter().peekable();
                while let Some(Parameter { name, type_name }) = param_iter.next() {
                    write!(
                        signature,
                        "*{name}*: **{}**",
                        format!("{}", type_name).green()
                    )?;
                    if param_iter.peek().is_some() {
                        write!(signature, ", ")?;
                    }
                }
                write!(signature, ")")?;
            }
        }

        let name = function.name.clone();
        let documentation = function
            .documentation
            .as_deref()
            .unwrap_or("")
            .trim()
            .to_string();
        let markdown = format!(
            "---\n\n## **{}**{signature} -> {}\n\n{documentation}{}",
            name.green(),
            format!("{}", return_type).green().bold(),
            if documentation.is_empty() { "" } else { "\n\n" }
        );

        skin.print_text(&markdown);
    }

    Ok(())
}
