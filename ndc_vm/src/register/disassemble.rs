//! How [`crate::disassemble`] lists register code: one instruction per line,
//! its name and then its operands, `AddIntI r0, r0, #1`, with a global
//! named as the stack VM's listing names it (`GetGlobal r2, print`, or `g0`
//! for the program's first declaration) and a native call naming the native
//! (`CallNative r0, len, r1, 1`).

use super::chunk::Chunk;
use super::instr::Instr;
use crate::disassemble::{Disassembler, Line};
use ndc_analysis::CaptureSource;

/// The instruction at `ip` of `chunk`, with the constant it loads, the
/// function it makes a closure of and what that captures, the type it casts
/// to, or where it jumps:
///
/// ```text
/// LoadK r1, k0                         "abc"
/// Closure r2, c0                       <fn #1 inner> captures r0, u1
/// CheckType r0, t0                     Int
/// JumpIfFalse r0, +1                   -> 0003
/// ```
pub(crate) fn line(disassembler: &mut Disassembler<'_>, chunk: &Chunk, ip: usize) -> Line {
    let instr = chunk.code()[ip];
    let instruction = match instr {
        Instr::GetGlobal { dst, slot } => {
            format!("GetGlobal {dst}, {}", disassembler.global(slot as usize))
        }
        Instr::SetGlobal { slot, src } => {
            format!("SetGlobal {}, {src}", disassembler.global(slot as usize))
        }
        Instr::CallNative {
            dst,
            base,
            slot,
            args,
        } => format!(
            "CallNative {dst}, {}, {base}, {args}",
            disassembler.global(usize::from(slot))
        ),
        _ => instr.to_string(),
    };
    let annotation = match instr {
        Instr::LoadK { k, .. } | Instr::MakeString { k, .. } => {
            Some(disassembler.constant(chunk.constant(k)))
        }
        Instr::Closure { index, .. } => {
            let closure = &chunk.closures()[index as usize];
            let function = disassembler.function(&closure.function);
            let captures = closure
                .captures
                .iter()
                .map(|capture| match capture {
                    CaptureSource::Local(slot) => format!("r{slot}"),
                    CaptureSource::Upvalue(index) => format!("u{index}"),
                })
                .collect::<Vec<_>>();
            Some(if captures.is_empty() {
                function
            } else {
                format!("{function} captures {}", captures.join(", "))
            })
        }
        Instr::CheckType { typ, .. } | Instr::TryCast { typ, .. } => {
            Some(chunk.types()[typ as usize].to_string())
        }
        _ => chunk
            .jump_target(ip)
            .map(|target| format!("-> {target:04}")),
    };
    Line {
        instruction,
        annotation,
        span: chunk.span(ip),
    }
}

#[cfg(test)]
mod tests {
    use crate::Natives;
    use crate::Value;
    use crate::disassemble::disassemble;
    use crate::register::{Builder, Instr, Reg};
    use crate::value::CompiledFunction;
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
            Instr::AddIntI {
                dst: Reg(1),
                left: Reg(1),
                imm: 2,
            },
            Span::synthetic(),
        );
        builder.bind(end);
        builder.emit(Instr::Return { src: Reg(1) }, Span::synthetic());
        let function = CompiledFunction {
            name: Some("f".to_string()),
            static_type: StaticType::Any,
            num_globals: 0,
            natives: Vec::new(),
            registers: builder.finish(),
            frame_size: 2,
        };
        assert_eq!(
            disassemble(&function, &Natives::default(), None),
            "== f (registers: 2, globals: 0) ==\n\
             0000  LoadK r1, k0                         1\n\
             0001  JumpIfFalse r0, +1                   -> 0003\n\
             0002  AddIntI r1, r1, #2\n\
             0003  Return r1\n"
        );
    }
}
