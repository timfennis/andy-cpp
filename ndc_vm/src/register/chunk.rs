use super::instr::{FAR, Instr};
use crate::Value;
use crate::value::CompiledFunction;
use ndc_analysis::CaptureSource;
use ndc_lexer::Span;
use std::rc::Rc;

/// A function the code makes closures of, with what they capture, in the
/// order the closure's upvalues are in: `Closure` makes one.
#[derive(Clone)]
pub struct ClosureCode {
    pub function: Rc<CompiledFunction>,
    pub captures: Rc<[CaptureSource]>,
}

/// The code of one function for the register VM: its instructions, the
/// constants they load, and the span of source each instruction came from,
/// at the same index.
#[derive(Default, Clone)]
pub struct Chunk {
    code: Vec<Instr>,
    constants: Vec<Value>,
    /// The functions `Closure` makes closures of, by index.
    closures: Vec<ClosureCode>,
    spans: Vec<Span>,
    /// Where each jump whose offset is [`FAR`] goes, by the jump's index,
    /// in order: a loop whose condition is a fused comparison and whose body
    /// is more than 32,767 instructions long.
    far_jumps: Vec<(usize, usize)>,
}

impl Chunk {
    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }

    pub fn code(&self) -> &[Instr] {
        &self.code
    }

    pub fn constants(&self) -> &[Value] {
        &self.constants
    }

    pub fn constant(&self, k: u32) -> &Value {
        &self.constants[k as usize]
    }

    /// The functions `Closure` makes closures of, by index.
    pub fn closures(&self) -> &[ClosureCode] {
        &self.closures
    }

    /// The span of the instruction at `ip`.
    pub fn span(&self, ip: usize) -> Span {
        self.spans[ip]
    }

    /// Where each jump whose offset is [`FAR`] goes, by the jump's index.
    pub fn far_jumps(&self) -> &[(usize, usize)] {
        &self.far_jumps
    }

    /// The index the jump at `ip` goes to, if it jumps.
    pub fn jump_target(&self, ip: usize) -> Option<usize> {
        let offset = self.code[ip].jump_offset()?;
        if self.code[ip].is_far_jump() {
            return Some(far_target(&self.far_jumps, ip));
        }
        Some((ip + 1).wrapping_add_signed(offset as isize))
    }
}

/// Where the jump at `ip`, whose offset is [`FAR`], goes, as `far_jumps`
/// says.
#[cold]
#[inline(never)]
pub(crate) fn far_target(far_jumps: &[(usize, usize)], ip: usize) -> usize {
    let index = far_jumps
        .binary_search_by_key(&ip, |&(jump, _)| jump)
        .expect("internal error: a far jump is in the chunk's far jumps");
    far_jumps[index].1
}

/// A place in the code a jump goes to, made before the code it marks is
/// written: a loop's end is a label until the loop's body is compiled.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Label(usize);

/// Writes a [`Chunk`]. Jumps name a [`Label`], which [`Self::finish`]
/// turns into an offset once every label is bound:
///
/// ```text
/// let end = builder.label();
/// builder.jump(Instr::Jump { offset: 0 }, end, span);
/// ...
/// builder.bind(end);
/// ```
#[derive(Default)]
pub struct Builder {
    code: Vec<Instr>,
    constants: Vec<Value>,
    closures: Vec<ClosureCode>,
    spans: Vec<Span>,
    /// Where each label is bound: the index of the instruction it marks.
    labels: Vec<Option<usize>>,
    /// Each jump written so far, with the label it goes to.
    jumps: Vec<(usize, Label)>,
}

impl Builder {
    /// Writes `instr`, which came from `span`.
    pub fn emit(&mut self, instr: Instr, span: Span) {
        debug_assert!(
            instr.jump_offset().is_none(),
            "internal error: a jump is written with `Builder::jump`"
        );
        self.code.push(instr);
        self.spans.push(span);
    }

    /// Writes `jump`, a jumping instruction whose offset doesn't matter,
    /// going to `target`.
    pub fn jump(&mut self, jump: Instr, target: Label, span: Span) {
        debug_assert!(jump.jump_offset().is_some(), "internal error: not a jump");
        self.jumps.push((self.code.len(), target));
        self.code.push(jump);
        self.spans.push(span);
    }

    /// A new label, bound nowhere yet.
    pub fn label(&mut self) -> Label {
        self.labels.push(None);
        Label(self.labels.len() - 1)
    }

    /// Binds `label` to the next instruction written.
    pub fn bind(&mut self, label: Label) {
        debug_assert!(
            self.labels[label.0].is_none(),
            "internal error: a label is bound once"
        );
        self.labels[label.0] = Some(self.code.len());
    }

    /// The index of a new constant holding `value`.
    pub fn constant(&mut self, value: Value) -> u32 {
        self.constants.push(value);
        u32::try_from(self.constants.len() - 1)
            .expect("internal error: a function has fewer than 2^32 constants")
    }

    /// The index of a new closure of `function`, which captures `captures`.
    pub fn closure(&mut self, function: Rc<CompiledFunction>, captures: &[CaptureSource]) -> u32 {
        self.closures.push(ClosureCode {
            function,
            captures: captures.into(),
        });
        u32::try_from(self.closures.len() - 1)
            .expect("internal error: a function has fewer than 2^32 closures")
    }

    /// The chunk, each jump given the offset to its label, or [`FAR`] and
    /// an entry in the chunk's far jumps if its offset doesn't fit.
    pub fn finish(mut self) -> Chunk {
        let mut far_jumps = Vec::new();
        for (at, label) in std::mem::take(&mut self.jumps) {
            let target = self.labels[label.0].expect("internal error: every label is bound");
            let offset = i32::try_from(target as isize - (at as isize + 1))
                .expect("internal error: a jump goes fewer than 2^31 instructions");
            self.code[at] = if let Some(jump) = self.code[at].with_jump_offset(offset) {
                jump
            } else {
                far_jumps.push((at, target));
                self.code[at].with_short_offset(FAR)
            };
        }
        far_jumps.sort_unstable();
        Chunk {
            code: self.code,
            constants: self.constants,
            closures: self.closures,
            spans: self.spans,
            far_jumps,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::register::instr::Reg;

    #[test]
    fn labels_become_offsets_from_the_next_instruction() {
        let mut builder = Builder::default();
        let start = builder.label();
        let end = builder.label();
        builder.bind(start);
        builder.jump(
            Instr::JumpIfFalse {
                cond: Reg(0),
                offset: 0,
            },
            end,
            Span::synthetic(),
        );
        builder.emit(Instr::LoadUnit { dst: Reg(1) }, Span::synthetic());
        builder.jump(Instr::Jump { offset: 0 }, start, Span::synthetic());
        builder.bind(end);
        builder.emit(Instr::Halt { src: Reg(1) }, Span::synthetic());
        let chunk = builder.finish();
        assert_eq!(chunk.code()[0].jump_offset(), Some(2));
        assert_eq!(chunk.code()[2].jump_offset(), Some(-3));
    }
}
