use crate::{Intrinsic, NativeOp, StaticType};
use std::borrow::Cow;

/// The natives and intrinsics a program can call, in global slot order: the
/// natives, then the intrinsics. What a native's type is may only be worked
/// out when it's first asked for.
pub trait Builtins {
    /// How many there are, intrinsics included.
    fn count(&self) -> usize;

    /// The name of the built-in in `slot`.
    fn name(&self, slot: usize) -> &str;

    /// The type of the built-in in `slot`.
    fn static_type(&self, slot: usize) -> &StaticType;

    /// The type parameters of the built-in in `slot`, in the order its
    /// signature declares them, which is the order `f::<…>` binds them in:
    /// `T` and `U` in `<T, U>([T], fn(T) -> U) -> [U]`.
    fn type_parameters(&self, slot: usize) -> Cow<'_, [StaticType]> {
        Cow::Owned(self.static_type(slot).declared_type_parameters())
    }

    /// Which intrinsic the built-in in `slot` is, if it's one.
    fn intrinsic(&self, slot: usize) -> Option<Intrinsic>;

    /// The instruction that does what the native in `slot` does, if the VM
    /// has one.
    fn native_op(&self, _slot: usize) -> Option<NativeOp> {
        None
    }
}

/// Built-ins listed with their types, as tests give them.
impl Builtins for Vec<(String, StaticType, Option<Intrinsic>)> {
    fn count(&self) -> usize {
        self.len()
    }

    fn name(&self, slot: usize) -> &str {
        &self[slot].0
    }

    fn static_type(&self, slot: usize) -> &StaticType {
        &self[slot].1
    }

    fn intrinsic(&self, slot: usize) -> Option<Intrinsic> {
        self[slot].2
    }
}
