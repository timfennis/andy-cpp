use super::instr::Instr;
use crate::value::CompiledFunction;
use std::fmt::Write;
use std::rc::Rc;

/// The register code of `function` and of the functions it declares, one
/// instruction per line, with the constant an instruction loads or the
/// index a jump goes to, and the source it came from when `source` is given:
///
/// ```text
/// == f == (2 registers)
/// 0000  LoadK                   r1, k0            1                1
/// 0001  JumpIfFalse             r0, +1            -> 0003          c
/// 0002  AddInt                  r1, r0, r1                         +
/// 0003  Return                  r1
/// ```
pub fn disassemble(function: &CompiledFunction, source: Option<&str>) -> String {
    let mut out = String::new();
    disassemble_function(function, source, &mut out);
    out
}

fn disassemble_function(function: &CompiledFunction, source: Option<&str>, out: &mut String) {
    let name = function.name.as_deref().unwrap_or("<script>");
    let registers = function.frame_size;
    writeln!(out, "== {name} == ({registers} registers)").unwrap();

    let chunk = &function.registers;
    let mut nested: Vec<Rc<CompiledFunction>> = Vec::new();
    for (ip, &instr) in chunk.code().iter().enumerate() {
        let annotation = match instr {
            Instr::LoadK { k, .. } | Instr::MakeString { k, .. } => {
                let constant = chunk.constant(k);
                match constant.function_prototype() {
                    Some(prototype) => {
                        nested.push(Rc::clone(prototype));
                        let name = prototype.name.as_deref().unwrap_or("?");
                        format!("<fn {name}>")
                    }
                    None => constant.to_string(),
                }
            }
            _ => match chunk.jump_target(ip) {
                Some(target) => format!("-> {target:04}"),
                None => String::new(),
            },
        };
        let excerpt = source
            .and_then(|source| source.get(chunk.span(ip).range()))
            .map(|excerpt| excerpt.trim().replace('\n', "↵"))
            .unwrap_or_default();
        let line = format!(
            "{ip:04}  {:<22}  {:<16}  {annotation:<15}  {excerpt}",
            instr.name(),
            instr.to_string()
        );
        writeln!(out, "{}", line.trim_end()).unwrap();
    }

    for function in nested {
        writeln!(out).unwrap();
        disassemble_function(&function, source, out);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Value;
    use crate::register::{Builder, Reg};
    use ndc_core::StaticType;
    use ndc_lexer::Span;

    #[test]
    fn prints_one_instruction_per_line() {
        let mut builder = Builder::default();
        let end = builder.label();
        let k = builder.constant(Value::Int(1));
        builder.emit(Instr::LoadK { dst: Reg(1), k }, Span::synthetic());
        builder.jump(
            Instr::JumpIfFalse {
                cond: Reg(0),
                offset: 0,
            },
            end,
            Span::synthetic(),
        );
        builder.emit(
            Instr::AddInt {
                dst: Reg(1),
                left: Reg(0),
                right: Reg(1),
            },
            Span::synthetic(),
        );
        builder.bind(end);
        builder.emit(Instr::Return { src: Reg(1) }, Span::synthetic());
        let function = CompiledFunction {
            name: Some("f".to_string()),
            static_type: StaticType::Any,
            body: crate::chunk::Chunk::default(),
            num_locals: 0,
            num_globals: 0,
            natives: Vec::new(),
            registers: builder.finish(),
            frame_size: 2,
        };
        assert_eq!(
            disassemble(&function, None),
            "== f == (2 registers)\n\
             0000  LoadK                   r1, k0            1\n\
             0001  JumpIfFalse             r0, +1            -> 0003\n\
             0002  AddInt                  r1, r0, r1\n\
             0003  Return                  r1\n"
        );
    }
}
