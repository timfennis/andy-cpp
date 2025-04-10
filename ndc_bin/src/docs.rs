use ndc_lib::interpreter::Interpreter;
use ndc_lib::interpreter::function::{Parameter, TypeSignature};
use std::cmp::Ordering;
use std::fmt::Write;
use strsim::normalized_damerau_levenshtein;
use tap::Tap;
use termimad::crossterm::style::Color::{Cyan, Green};
use termimad::crossterm::style::Stylize;
use termimad::*;

/// Returns `true` if `needle` is a substring of `haystack` or if they are at least 80% similar
fn string_match(needle: &str, haystack: &str) -> bool {
    haystack.contains(needle) || normalized_damerau_levenshtein(needle, haystack) > 0.8
}

pub fn docs(query: Option<&str>) -> anyhow::Result<()> {
    let interpreter = Interpreter::new(Vec::new()); // Discard the output
    let functions = interpreter.environment().borrow().get_all_functions();

    let matched_functions = functions
        .into_iter()
        .flat_map(|x| x.implementations().collect::<Vec<_>>())
        .filter(|(_, func)| {
            if let Some(query) = query {
                string_match(query, func.name())
            } else {
                true
            }
        })
        .collect::<Vec<_>>()
        .tap_mut(|list| {
            list.sort_by(|(_, l), (_, r)| {
                if let Some(query) = query {
                    normalized_damerau_levenshtein(l.name(), query)
                        .partial_cmp(&normalized_damerau_levenshtein(r.name(), query))
                        .unwrap_or(Ordering::Equal)
                        .reverse()
                } else {
                    l.name().cmp(r.name())
                }
            })
        });

    let mut skin = MadSkin::default();

    skin.headers[0].align = Alignment::Left;
    skin.headers[1].align = Alignment::Left;
    skin.headers[2].align = Alignment::Left;

    for (type_sig, function) in matched_functions {
        let mut signature = String::new();
        match type_sig {
            TypeSignature::Variadic => {
                writeln!(signature, "(*args**)")?;
            }
            TypeSignature::Exact(params) => {
                write!(signature, "(")?;
                let mut param_iter = params.iter().peekable();
                while let Some(Parameter { name, type_name }) = param_iter.next() {
                    write!(signature, "*{name}*: **{}**", type_name.as_str().green())?;

                    if param_iter.peek().is_some() {
                        write!(signature, ", ")?;
                    }
                }

                writeln!(signature, ")")?;
            }
        }
        let name = function.name();
        let documentation = function.documentation().trim();
        let markdown = format!(
            "---\n\n## **{}**{signature}\n{documentation}{}",
            name.green(),
            if documentation.is_empty() { "" } else { "\n\n" }
        );

        skin.print_text(&markdown);
    }

    Ok(())
}
