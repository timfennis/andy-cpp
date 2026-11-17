use ndc_core::StaticType;
use ndc_interpreter::Interpreter;
use std::cmp::Ordering;
use std::fmt::Write;
use strsim::normalized_damerau_levenshtein;
use yansi::Paint;

fn string_match(needle: &str, haystack: &str) -> bool {
    haystack.contains(needle) || normalized_damerau_levenshtein(needle, haystack) > 0.8
}

fn terminal_width() -> usize {
    termimad::crossterm::terminal::size()
        .map_or(80, |(w, _)| w as usize)
        .min(120)
}

/// Wraps `text` to `max_width` columns, indenting every line with `indent`.
/// Blank lines in the input are treated as paragraph breaks and preserved.
fn wrap_text(text: &str, max_width: usize, indent: &str) -> String {
    let available = max_width.saturating_sub(indent.len());
    let mut result = String::new();

    for (i, paragraph) in text.split("\n\n").enumerate() {
        if i > 0 {
            result.push('\n');
            result.push_str(indent);
            result.push('\n');
        }
        result.push_str(indent);
        let mut col = 0usize;
        for word in paragraph.split_whitespace() {
            if col > 0 && col + 1 + word.len() > available {
                result.push('\n');
                result.push_str(indent);
                col = 0;
            } else if col > 0 {
                result.push(' ');
                col += 1;
            }
            result.push_str(word);
            col += word.len();
        }
    }

    result
}

pub fn docs(query: Option<&str>, no_color: bool) -> anyhow::Result<()> {
    if no_color {
        yansi::disable();
    } else {
        yansi::whenever(yansi::Condition::TTY_AND_COLOR);
    }

    let mut interpreter = Interpreter::capturing();
    interpreter.configure(ndc_stdlib::register);

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

    let width = terminal_width();

    for (i, function) in functions.iter().enumerate() {
        if i > 0 {
            println!();
        }

        // A function that takes any number of arguments has no parameter types.
        let (parameters, return_type) = match &function.static_type {
            StaticType::Function {
                parameters,
                return_type,
            } => (parameters.as_deref(), return_type.as_ref()),
            other => (None, other),
        };

        let mut line = String::new();
        write!(line, "{}", function.name.bold().yellow())?;
        match parameters {
            None => {
                write!(line, "{}", "(...)".dim())?;
            }
            Some(parameters) => {
                write!(line, "(")?;
                for (i, parameter) in parameters.iter().enumerate() {
                    if i > 0 {
                        write!(line, "{}", ", ".dim())?;
                    }
                    write!(line, "{}", format!("arg{i}").italic())?;
                    write!(line, "{}", ":".dim())?;
                    write!(line, " {}", parameter.to_string().cyan())?;
                }
                write!(line, ")")?;
            }
        }
        write!(line, " {} ", "->".dim())?;
        write!(line, "{}", return_type.to_string().bold().cyan())?;
        println!("{line}");

        if let Some(docs) = &function.documentation {
            let docs = docs.trim();
            if !docs.is_empty() {
                let wrapped = wrap_text(docs, width, "  ");
                println!("{}", wrapped.dim());
            }
        }
    }

    Ok(())
}
