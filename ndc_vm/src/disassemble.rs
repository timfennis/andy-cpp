//! A readable listing of compiled code, as `ndc disassemble` prints it, for
//! either VM. `fn f() { fn g() => len([1]); g } let h = print;` lists as:
//!
//! ```text
//! == <script> (locals: 0, globals: 2, natives: len, print) ==
//! 0000  Constant(0)                          <fn #1 f>
//! 0001  SetGlobal(g0)
//! 0002  Constant(1)                          ()
//! 0003  GetGlobal(print)
//! 0004  SetGlobal(g1)
//! 0005  Constant(2)                          ()
//! 0006  Pop
//! 0007  Halt
//!
//! == #1 f (locals: 1) ==
//! 0000  Constant(0)                          <fn #2 g>
//! 0001  SetLocal(0)
//! 0002  Constant(1)                          ()
//! 0003  Pop
//! 0004  GetLocal(0)
//! 0005  Return
//!
//! == #2 g (locals: 0) ==
//! 0000  Constant(0)                          1
//! 0001  MakeList(1)
//! 0002  CallNative(len, 1)
//! 0003  Return
//! ```
//!
//! and compiled for the register VM, one instruction per line with the
//! registers it reads and writes, as:
//!
//! ```text
//! == <script> (registers: 2, globals: 2, natives: len, print) ==
//! 0000  LoadK r1, k0                         <fn #1 f>
//! 0001  SetGlobal g0, r1
//! 0002  LoadUnit r0
//! 0003  GetGlobal r1, print
//! 0004  SetGlobal g1, r1
//! 0005  Halt r0
//!
//! == #1 f (registers: 1) ==
//! 0000  LoadK r0, k0                         <fn #2 g>
//! 0001  Return r0
//!
//! == #2 g (registers: 2) ==
//! 0000  LoadK r1, k0                         1
//! 0001  MakeList r0, r1, 1
//! 0002  CallNative r0, len, r0, 1
//! 0003  Return r0
//! ```
//!
//! Each function's first line gives how many local slots (stack VM) or
//! registers (register VM) it uses, and the top-level code's also how many
//! globals the program declares and the natives it reads. A function is
//! numbered where it's first loaded and listed once, after the functions
//! before it, so two that load the same function show the same number. A
//! jump shows where it lands (`-> 0007`), and an instruction that loads a
//! constant shows it in its debug form (`"abc"` for a string).
//!
//! A global slot reads as the name of the native or intrinsic in it (`len`),
//! or as `g` and the position among the program's own globals (`g0` for its
//! first declaration), so the listing of a program doesn't change when the
//! standard library gains a native.

use crate::Natives;
use crate::value::{CompiledFunction, Value};
use ndc_core::Builtins;
use ndc_core::collections::HashMap;
use ndc_lexer::Span;
use std::collections::VecDeque;
use std::fmt::Write;
use std::rc::Rc;

/// The width the instruction column is padded to when an annotation follows.
const INSTRUCTION_WIDTH: usize = 35;

/// The listing of `function`, the top-level code of a program compiled with
/// `natives` for either VM, followed by each function it declares, in the
/// order they're found, so each comes after the function that declares it.
/// With `source`, each instruction shows the code it was compiled from.
pub fn disassemble(function: &CompiledFunction, natives: &Natives, source: Option<&str>) -> String {
    let mut disassembler = Disassembler {
        natives,
        source,
        numbers: HashMap::default(),
        pending: VecDeque::new(),
        out: String::new(),
    };
    disassembler.top_level(function);
    while let Some((function, number)) = disassembler.pending.pop_front() {
        disassembler.nested(&function, number);
    }
    disassembler.out
}

/// One instruction as the listing shows it: `CallNative(len, 1)` or
/// `LoadK r1, k0`, with what it loads or where it jumps, if anything, and
/// the source it came from.
pub(crate) struct Line {
    pub instruction: String,
    pub annotation: Option<String>,
    pub span: Span,
}

/// Writes a listing, numbering each function the first time it's found so
/// a function two others share is listed once, under one number.
pub(crate) struct Disassembler<'a> {
    natives: &'a Natives,
    source: Option<&'a str>,
    numbers: HashMap<*const CompiledFunction, usize>,
    /// The functions found but not listed yet, in the order they were found.
    pending: VecDeque<(Rc<CompiledFunction>, usize)>,
    out: String,
}

impl Disassembler<'_> {
    fn top_level(&mut self, function: &CompiledFunction) {
        let own_globals = function.num_globals.saturating_sub(self.natives.count());
        let mut natives = function
            .natives()
            .iter()
            .map(|&slot| self.natives.name(slot))
            .collect::<Vec<_>>();
        natives.sort_unstable();
        let name = function.name.as_deref().unwrap_or("<script>");
        write!(
            self.out,
            "== {name} ({}, globals: {own_globals}",
            Self::frame(function)
        )
        .unwrap();
        if !natives.is_empty() {
            write!(self.out, ", natives: {}", natives.join(", ")).unwrap();
        }
        writeln!(self.out, ") ==").unwrap();
        self.body(function);
    }

    fn nested(&mut self, function: &CompiledFunction, number: usize) {
        writeln!(self.out).unwrap();
        writeln!(
            self.out,
            "== #{number} {} ({}) ==",
            Self::name(function),
            Self::frame(function)
        )
        .unwrap();
        self.body(function);
    }

    /// How big a call of `function` is: `registers: 5`.
    fn frame(function: &CompiledFunction) -> String {
        format!("registers: {}", function.frame_size)
    }

    /// Lists `function`'s instructions.
    fn body(&mut self, function: &CompiledFunction) {
        for index in 0..function.registers.len() {
            let Line {
                instruction,
                annotation,
                span,
            } = crate::register::disassemble::line(self, &function.registers, index);
            let excerpt = self
                .source
                .and_then(|source| source.get(span.range()))
                .map(|excerpt| excerpt.trim().replace('\n', "↵"));

            let line = match (annotation, excerpt) {
                (Some(a), Some(e)) => format!("{instruction:<INSTRUCTION_WIDTH$}  {a:<20}  {e}"),
                (Some(a), None) => format!("{instruction:<INSTRUCTION_WIDTH$}  {a}"),
                (None, Some(e)) => format!("{instruction:<INSTRUCTION_WIDTH$}  {e}"),
                (None, None) => instruction,
            };
            writeln!(self.out, "{index:04}  {}", line.trim_end()).unwrap();
        }
    }

    /// The name of global `slot`: `len` for a native's, `g0` for the
    /// program's first declaration.
    pub(crate) fn global(&self, slot: usize) -> String {
        match slot.checked_sub(self.natives.count()) {
            None => self.natives.name(slot).to_string(),
            Some(own) => format!("g{own}"),
        }
    }

    /// A constant as it reads in the listing: `"abc"` for a string, `<fn #1 f>`
    /// for a function, which is listed later if this is the first to load it.
    pub(crate) fn constant(&mut self, value: &Value) -> String {
        match value.function_prototype() {
            Some(function) => self.function(function),
            None => format!("{value:?}"),
        }
    }

    /// A function as it reads in the listing, `<fn #1 f>`, numbered the
    /// first time it's found and listed later.
    pub(crate) fn function(&mut self, function: &Rc<CompiledFunction>) -> String {
        let next = self.numbers.len() + 1;
        let number = *self.numbers.entry(Rc::as_ptr(function)).or_insert_with(|| {
            self.pending.push_back((Rc::clone(function), next));
            next
        });
        format!("<fn #{number} {}>", Self::name(function))
    }

    fn name(function: &CompiledFunction) -> &str {
        function.name.as_deref().unwrap_or("<lambda>")
    }
}
