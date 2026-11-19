//! The natives a program can call, each built the first time something asks
//! for it: a program that only prints builds `print` and none of the others.

use crate::operator::Operator;
use crate::value::{NativeFunc, NativeFunction};
use ndc_core::{Builtins, FunctionRegistry, Intrinsic, IntrinsicFunction, StaticType};
use std::borrow::Cow;
use std::cell::{Cell, OnceCell};
use std::rc::Rc;

type MakeType = Box<dyn FnOnce() -> StaticType>;
type MakeFunc = Box<dyn FnOnce() -> NativeFunc>;

/// A native as it's registered: its name and documentation, and how to make
/// its type and its function, which [`Natives`] does when first asked.
pub struct NativeSpec {
    pub name: &'static str,
    pub documentation: Option<Cow<'static, str>>,
    pub static_type: MakeType,
    pub func: MakeFunc,
    /// What the native does, when it's an operator the compiler writes as
    /// its own instruction.
    pub operator: Option<Operator>,
}

impl NativeSpec {
    /// A native whose type `static_type` makes and whose function `func`
    /// makes.
    pub fn new(
        name: &'static str,
        documentation: impl Into<Cow<'static, str>>,
        static_type: impl FnOnce() -> StaticType + 'static,
        func: impl FnOnce() -> NativeFunc + 'static,
    ) -> Self {
        Self {
            name,
            documentation: Some(documentation.into()),
            static_type: Box::new(static_type),
            func: Box::new(func),
            operator: None,
        }
    }

    /// A native whose type is `signature`, such as `<T>([T], T) -> T`,
    /// parsed when it's first asked for.
    pub fn with_signature(
        name: &'static str,
        documentation: impl Into<Cow<'static, str>>,
        signature: impl AsRef<str> + 'static,
        func: impl FnOnce() -> NativeFunc + 'static,
    ) -> Self {
        Self::new(
            name,
            documentation,
            move || {
                StaticType::signature(signature.as_ref()).unwrap_or_else(|error| {
                    panic!("internal error: invalid signature of {name}: {error}")
                })
            },
            func,
        )
    }

    /// A native whose type `static_type` makes that does `operator`, which
    /// the compiler writes as its own instruction where a call resolves to
    /// it.
    pub fn operator(
        name: &'static str,
        documentation: impl Into<Cow<'static, str>>,
        static_type: impl FnOnce() -> StaticType + 'static,
        operator: Operator,
    ) -> Self {
        Self {
            operator: Some(operator),
            ..Self::new(name, documentation, static_type, move || {
                NativeFunc::Simple(Box::new(move |args| operator.apply(args)))
            })
        }
    }
}

/// A registered native, and what has been made of it so far.
struct Native {
    name: &'static str,
    documentation: Option<Cow<'static, str>>,
    make_type: Cell<Option<MakeType>>,
    make_func: Cell<Option<MakeFunc>>,
    static_type: OnceCell<StaticType>,
    /// The type parameters of its type, made with it.
    type_parameters: OnceCell<Vec<StaticType>>,
    function: OnceCell<Rc<NativeFunction>>,
}

/// The natives, in global slot order, followed by the language's intrinsics.
/// A native's type and function are made the first time they're asked for,
/// and kept.
pub struct Natives {
    natives: Vec<Native>,
    intrinsics: Vec<IntrinsicFunction>,
    /// The operator each native does, if it's one, in global slot order.
    operators: Rc<[Option<Operator>]>,
}

impl Default for Natives {
    /// No natives, and the intrinsics.
    fn default() -> Self {
        Self {
            natives: Vec::new(),
            intrinsics: IntrinsicFunction::all(),
            operators: Rc::new([]),
        }
    }
}

impl Natives {
    /// Adds the natives of `registry` after the ones already here.
    pub fn extend(&mut self, registry: FunctionRegistry<NativeSpec>) {
        let specs = registry.into_vec();
        self.operators = self
            .operators
            .iter()
            .copied()
            .chain(specs.iter().map(|spec| spec.operator))
            .collect();
        self.natives.extend(specs.into_iter().map(|spec| Native {
            name: spec.name,
            documentation: spec.documentation,
            make_type: Cell::new(Some(spec.static_type)),
            make_func: Cell::new(Some(spec.func)),
            static_type: OnceCell::new(),
            type_parameters: OnceCell::new(),
            function: OnceCell::new(),
        }));
    }

    /// How many natives there are, not counting the intrinsics.
    #[must_use]
    pub fn len(&self) -> usize {
        self.natives.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.natives.is_empty()
    }

    #[must_use]
    pub fn documentation(&self, slot: usize) -> Option<&str> {
        self.natives[slot].documentation.as_deref()
    }

    /// The native in `slot`, made the first time it's asked for.
    #[must_use]
    pub fn function(&self, slot: usize) -> &Rc<NativeFunction> {
        let native = &self.natives[slot];
        native.function.get_or_init(|| {
            let make = native
                .make_func
                .take()
                .expect("internal error: a native's function is made once");
            Rc::new(NativeFunction {
                name: native.name.to_string(),
                documentation: native.documentation.as_deref().map(str::to_string),
                static_type: Self::native_type(native).clone(),
                func: make(),
            })
        })
    }

    /// The operator each native does, if it's one, in global slot order.
    #[must_use]
    pub fn operators(&self) -> Rc<[Option<Operator>]> {
        Rc::clone(&self.operators)
    }

    /// The language's intrinsics, whose global slots follow the natives'.
    #[must_use]
    pub fn intrinsics(&self) -> &[IntrinsicFunction] {
        &self.intrinsics
    }

    /// The type of `native`, made the first time it's asked for.
    fn native_type(native: &Native) -> &StaticType {
        native.static_type.get_or_init(|| {
            let make = native
                .make_type
                .take()
                .expect("internal error: a native's type is made once");
            make()
        })
    }
}

/// A native's slot is its position among the natives, and the intrinsics'
/// slots follow them.
impl Builtins for Natives {
    fn count(&self) -> usize {
        self.natives.len() + self.intrinsics.len()
    }

    fn name(&self, slot: usize) -> &str {
        match slot.checked_sub(self.natives.len()) {
            None => self.natives[slot].name,
            Some(index) => &self.intrinsics[index].name,
        }
    }

    fn static_type(&self, slot: usize) -> &StaticType {
        match slot.checked_sub(self.natives.len()) {
            None => Self::native_type(&self.natives[slot]),
            Some(index) => &self.intrinsics[index].static_type,
        }
    }

    fn type_parameters(&self, slot: usize) -> Cow<'_, [StaticType]> {
        match self.natives.get(slot) {
            Some(native) => Cow::Borrowed(
                native
                    .type_parameters
                    .get_or_init(|| Self::native_type(native).declared_type_parameters()),
            ),
            None => Cow::Owned(self.static_type(slot).declared_type_parameters()),
        }
    }

    fn intrinsic(&self, slot: usize) -> Option<Intrinsic> {
        let index = slot.checked_sub(self.natives.len())?;
        Some(self.intrinsics[index].intrinsic)
    }
}
