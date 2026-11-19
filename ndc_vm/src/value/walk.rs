//! A depth budget for the native walks over values: equality, ordering,
//! hashing and display. These are std trait impls that can't return an error,
//! so a walk that goes too deep unwinds with [`TooDeep`], which the VM turns
//! into an error at the instruction that started the walk.

use std::cell::Cell;

pub const MAX_VALUE_DEPTH: usize = 2_000;

pub const TOO_DEEP: &str = "a value is nested more than 2000 deep, or contains itself";

/// The payload a walk unwinds with when it passes [`MAX_VALUE_DEPTH`].
pub struct TooDeep;

thread_local! {
    static DEPTH: Cell<usize> = const { Cell::new(0) };
}

/// Runs `f` one level deeper into a value.
#[inline]
pub(crate) fn nested<R>(f: impl FnOnce() -> R) -> R {
    let depth = DEPTH.get();
    if depth >= MAX_VALUE_DEPTH {
        too_deep();
    }
    DEPTH.set(depth + 1);
    let result = f();
    DEPTH.set(depth);
    result
}

#[cold]
#[inline(never)]
fn too_deep() -> ! {
    std::panic::resume_unwind(Box::new(TooDeep))
}

pub(crate) fn depth() -> usize {
    DEPTH.get()
}

pub(crate) fn reset(depth: usize) {
    DEPTH.set(depth);
}
