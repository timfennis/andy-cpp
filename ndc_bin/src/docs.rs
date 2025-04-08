use ndc_lib::interpreter::Interpreter;
use ndc_lib::interpreter::function::{Parameter, TypeSignature};
use owo_colors::OwoColorize;
use std::fmt::Write;
use tap::Tap;

fn chunk_text(text: &str) -> Vec<(String, bool)> {
    let mut chunks = Vec::new();
    let mut current_chunk = String::new();
    let mut inside_quotes = false;

    for c in text.chars() {
        if c == '`' {
            // Toggle the quote flag when encountering backticks
            chunks.push((current_chunk.clone(), inside_quotes));
            inside_quotes = !inside_quotes;
            current_chunk.clear();
        } else {
            current_chunk.push(c)
        }
    }

    // Add the last chunk if there's any remaining
    if !current_chunk.is_empty() {
        chunks.push((current_chunk, inside_quotes));
    }

    chunks
}

pub fn docs(mut stdout: impl std::io::Write) -> anyhow::Result<()> {
    let interpreter = Interpreter::new(Vec::new()); // Discard the output
    let functions = interpreter.environment().borrow().get_all_functions();

    // let mut stdout = std::io::stdout();
    let str = format!(
        "\n{} Function Reference {}\n",
        "═".repeat(20),
        "═".repeat(20),
    );
    writeln!(stdout, "{}", str.bold().white())?;

    let mut iter = functions
        .into_iter()
        .flat_map(|x| x.implementations().collect::<Vec<_>>())
        .collect::<Vec<_>>()
        .tap_mut(|list| list.sort_by(|(_, l), (_, r)| l.name().cmp(r.name())))
        .into_iter()
        .peekable();

    while let Some((type_sig, function)) = iter.next() {
        write!(stdout, "{}", function.name().bold().bright_blue(),)?;

        match type_sig {
            TypeSignature::Variadic => {
                writeln!(stdout, "({})", "args*".white())?;
            }
            TypeSignature::Exact(params) => {
                write!(stdout, "{}", "(".bold())?;
                let mut param_iter = params.iter().peekable();
                while let Some(Parameter { name, type_name }) = param_iter.next() {
                    write!(
                        stdout,
                        "{}: {}",
                        name.white(),
                        type_name.as_str().bright_yellow()
                    )?;

                    if param_iter.peek().is_some() {
                        write!(stdout, ", ")?;
                    }
                }

                writeln!(stdout, "{}", ")".bold())?;
            }
        }

        let mut documentation = String::new();
        for (chunk, quoted) in chunk_text(function.documentation()) {
            if quoted {
                write!(documentation, "{}", chunk.bright_white())?;
            } else {
                write!(documentation, "{}", chunk.bright_green())?;
            }
        }

        for line in documentation.lines() {
            writeln!(stdout, "  {}", line.trim())?;
        }

        if documentation.is_empty() {
            writeln!(stdout)?;
        }

        if iter.peek().is_some() {
            writeln!(stdout, "{}", "─".repeat(50).bright_black())?;
            writeln!(stdout)?;
        }
    }

    Ok(())
}
