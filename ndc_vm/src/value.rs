use crate::chunk::{Chunk, OpCode};
use crate::error::VmError;
use crate::iterator::SharedIterator;
use ndc_core::compare::FallibleOrd;
use ndc_core::hash_map::{DefaultHasher, HashMap};
use ndc_core::int::Int;
use ndc_core::num::Number;
use ndc_parser::{ResolvedVar, StaticType, TypeSignature};
use ordered_float::OrderedFloat;
use std::cell::RefCell;
use std::cmp::{Ordering, Reverse};
use std::collections::BinaryHeap;
use std::collections::VecDeque;
use std::fmt;
use std::fmt::Formatter;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

/// Enumerates all the different types of values that exist in the language
/// All values should be pretty cheap to clone because the bigger ones are wrapped using Rc's
#[derive(Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    None,
    Object(Rc<Object>),
}

#[derive(Clone)]
pub enum Object {
    Some(Value),
    BigInt(num::BigInt),
    Complex(num::Complex<f64>),
    Rational(num::BigRational),
    // String keeps its inner Rc<RefCell<String>> so the vm_bridge can share the
    // allocation directly with the interpreter side without a separate sync step.
    String(Rc<RefCell<String>>),
    List(RefCell<Vec<Value>>),
    Tuple(Vec<Value>),
    Map {
        entries: RefCell<HashMap<Value, Value>>,
        default: Option<Value>,
    },
    Function(Function),
    OverloadSet(Vec<ResolvedVar>),
    Iterator(SharedIterator),
    Deque(RefCell<VecDeque<Value>>),
    MinHeap(RefCell<BinaryHeap<Reverse<OrdValue>>>),
    MaxHeap(RefCell<BinaryHeap<OrdValue>>),
}

/// Newtype wrapper around `Value` that imposes a total order so values can be
/// stored in a `BinaryHeap`.  The ordering mirrors the interpreter's `HeapValue`.
#[derive(Clone)]
pub struct OrdValue(pub Value);

impl PartialEq for OrdValue {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl Eq for OrdValue {}
impl PartialOrd for OrdValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for OrdValue {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.partial_cmp(&other.0).unwrap_or(Ordering::Equal)
    }
}

/// Type alias for [`Value`] that signals `StaticType::Sequence` to the `#[export_module]` macro.
///
/// Use this instead of `ndc_vm::value::Value` in stdlib function signatures when the parameter
/// must accept any iterable sequence. The extraction is identical (zero-copy pass-through on the
/// VM path) but the macro emits `StaticType::Sequence(Any)` instead of `StaticType::Any`, which
/// preserves the correct type for dispatch and static analysis.
pub type SeqValue = Value;

/// An iterator that yields VM [`Value`]s from any iterable object.
///
/// Created by [`Value::try_into_iter`]. If the caller holds the only `Rc`
/// reference to the underlying collection it is moved out in O(1) via
/// `Rc::unwrap_or_clone`; otherwise it is cloned first.
pub enum ValueIter {
    List(std::vec::IntoIter<Value>),
    Deque(std::collections::vec_deque::IntoIter<Value>),
    Map(std::collections::hash_map::IntoIter<Value, Value>),
    Shared(SharedIterator),
}

impl Iterator for ValueIter {
    type Item = Value;

    fn next(&mut self) -> Option<Value> {
        match self {
            Self::List(i) => i.next(),
            Self::Deque(i) => i.next(),
            Self::Map(i) => i.next().map(|(k, v)| Value::tuple(vec![k, v])),
            Self::Shared(i) => i.borrow_mut().next(),
        }
    }
}

#[derive(Clone)]
pub enum Function {
    Closure(ClosureFunction),
    Compiled(Rc<CompiledFunction>),
    Native(Rc<NativeFunction>),
    Memoized {
        cache: Rc<RefCell<HashMap<u64, Value>>>,
        function: Box<Function>,
    },
}

pub struct NativeFunction {
    pub name: String,
    pub func: Box<dyn Fn(&[Value], &[Value]) -> Result<Value, VmError>>,
    pub static_type: StaticType,
}

pub struct CompiledFunction {
    pub name: Option<String>,
    pub(crate) type_signature: TypeSignature,
    pub(crate) body: Chunk,
    pub(crate) return_type: StaticType,
    pub(crate) num_locals: usize,
}

#[derive(Clone)]
pub struct ClosureFunction {
    pub(crate) prototype: Rc<CompiledFunction>,
    pub(crate) upvalues: Vec<Rc<RefCell<UpvalueCell>>>,
}

pub enum UpvalueCell {
    Open(usize),
    Closed(Value),
}

impl CompiledFunction {
    pub fn opcodes(&self) -> &[OpCode] {
        self.body.opcodes()
    }
}

impl Value {
    pub fn unit() -> Self {
        Self::Object(Rc::new(Object::Tuple(vec![])))
    }

    pub fn function(function: Function) -> Self {
        Self::Object(Rc::new(Object::Function(function)))
    }

    pub fn string<S: Into<String>>(string: S) -> Self {
        Self::Object(Rc::new(Object::String(Rc::new(RefCell::new(
            string.into(),
        )))))
    }

    pub fn iterator(iter: SharedIterator) -> Self {
        Self::Object(Rc::new(Object::Iterator(iter)))
    }

    pub fn list(values: Vec<Value>) -> Self {
        Self::Object(Rc::new(Object::list(values)))
    }

    pub fn tuple(values: Vec<Value>) -> Self {
        Self::Object(Rc::new(Object::Tuple(values)))
    }

    pub fn static_type(&self) -> StaticType {
        match self {
            Self::Int(_) => StaticType::Int,
            Self::Float(_) => StaticType::Float,
            Self::Bool(_) => StaticType::Bool,
            Self::None => StaticType::Option(Box::new(StaticType::Any)),
            Self::Object(obj) => obj.static_type(),
        }
    }
}

impl Object {
    pub fn list(values: Vec<Value>) -> Self {
        Self::List(RefCell::new(values))
    }

    pub fn map(entries: HashMap<Value, Value>, default: Option<Value>) -> Self {
        Self::Map {
            entries: RefCell::new(entries),
            default,
        }
    }

    // TODO: why are we returning stupid types
    pub fn static_type(&self) -> StaticType {
        match self {
            Self::Some(inner) => StaticType::Option(Box::new(inner.static_type())),
            Self::BigInt(_) => StaticType::Int,
            Self::Complex(_) => StaticType::Complex,
            Self::Rational(_) => StaticType::Rational,
            Self::String(_) => StaticType::String,
            Self::List(_) => StaticType::List(Box::new(StaticType::Any)),
            Self::Tuple(_) => StaticType::Tuple(Vec::new()),
            Self::Map { .. } => StaticType::Map {
                key: Box::new(StaticType::Any),
                value: Box::new(StaticType::Any),
            },
            Self::Function(f) => f.static_type(),
            Self::OverloadSet(_) => StaticType::Any,
            Self::Iterator(_) => StaticType::Iterator(Box::new(StaticType::Any)),
            Self::Deque(_) => StaticType::Deque(Box::new(StaticType::Any)),
            Self::MinHeap(_) => StaticType::MinHeap(Box::new(StaticType::Any)),
            Self::MaxHeap(_) => StaticType::MaxHeap(Box::new(StaticType::Any)),
        }
    }
}

impl Value {
    /// Consume this value and produce an iterator over its elements.
    ///
    /// Returns `None` for non-iterable types (`Int`, `Float`, `Bool`, `None`,
    /// functions, numbers, …).
    ///
    /// For `Map`, yields `(key, value)` tuples — the same behaviour as
    /// iterating a map in a for-loop.
    ///
    /// Uses `Rc::unwrap_or_clone` so no extra clone occurs when this value
    /// holds the sole reference to its object.
    pub fn try_into_iter(self) -> Option<ValueIter> {
        let Value::Object(obj) = self else {
            return None;
        };
        match Rc::unwrap_or_clone(obj) {
            Object::List(l) => Some(ValueIter::List(l.into_inner().into_iter())),
            Object::Tuple(t) => Some(ValueIter::List(t.into_iter())),
            Object::Deque(d) => Some(ValueIter::Deque(d.into_inner().into_iter())),
            Object::Iterator(i) => Some(ValueIter::Shared(i)),
            Object::String(s) => {
                let chars: Vec<Value> = s
                    .borrow()
                    .chars()
                    .map(|c| Value::string(c.to_string()))
                    .collect();
                Some(ValueIter::List(chars.into_iter()))
            }
            Object::Map { entries, .. } => Some(ValueIter::Map(entries.into_inner().into_iter())),
            _ => None,
        }
    }

    pub fn function_prototype(&self) -> Option<&Rc<CompiledFunction>> {
        let Self::Object(obj) = self else { return None };
        obj.function_prototype()
    }

    /// Convert a numeric VM value to a `ndc_core::Number`.
    /// Returns `None` for non-numeric values (Bool, None, String, List, …).
    pub fn to_number(&self) -> Option<Number> {
        vm_value_to_number(self)
    }

    /// Convert a `ndc_core::Number` to a VM value.
    /// `Int64` maps to `Value::Int`; all other variants become `Value::Object`.
    pub fn from_number(n: Number) -> Self {
        match n {
            Number::Int(Int::Int64(i)) => Value::Int(i),
            Number::Int(Int::BigInt(b)) => Value::Object(Rc::new(Object::BigInt(b))),
            Number::Float(f) => Value::Float(f),
            Number::Rational(r) => Value::Object(Rc::new(Object::Rational(*r))),
            Number::Complex(c) => Value::Object(Rc::new(Object::Complex(c))),
        }
    }

    /// Extract an integer VM value as a `ndc_core::Int`.
    /// Returns `None` for non-integer values.
    pub fn to_int(&self) -> Option<Int> {
        match self {
            Value::Int(i) => Some(Int::Int64(*i)),
            Value::Object(obj) => match obj.as_ref() {
                Object::BigInt(b) => Some(Int::BigInt(b.clone())),
                _ => None,
            },
            _ => None,
        }
    }

    /// Convert a `ndc_core::Int` to a VM value.
    pub fn from_int(i: Int) -> Self {
        match i {
            Int::Int64(n) => Value::Int(n),
            Int::BigInt(b) => Value::Object(Rc::new(Object::BigInt(b))),
        }
    }

    /// Convert a numeric VM value to `f64`, coercing integers and rationals.
    /// Returns `None` for non-numeric values (Bool, None, String, …).
    pub fn to_f64(&self) -> Option<f64> {
        use num::ToPrimitive;
        match self {
            Value::Float(f) => Some(*f),
            Value::Int(i) => Some(*i as f64),
            Value::Object(obj) => match obj.as_ref() {
                Object::BigInt(b) => b.to_f64(),
                Object::Rational(r) => r.to_f64(),
                _ => None,
            },
            _ => None,
        }
    }
}

impl Object {
    pub fn function_prototype(&self) -> Option<&Rc<CompiledFunction>> {
        let Self::Function(f) = self else { return None };
        f.prototype()
    }
}

impl Function {
    pub fn prototype(&self) -> Option<&Rc<CompiledFunction>> {
        match self {
            Self::Compiled(f) => Some(f),
            Self::Closure(c) => Some(&c.prototype),
            Self::Native(_) => None,
            Self::Memoized { function, .. } => function.prototype(),
        }
    }

    /// Returns true if this function (or the inner function of a memoized wrapper)
    /// is a native function. Native functions bridge to the tree-walk interpreter,
    /// which may call closures back via `Vm::call_function` in a fresh VM context,
    /// where open upvalues pointing to the current stack would be invalid.
    pub fn is_native(&self) -> bool {
        match self {
            Self::Native(_) => true,
            Self::Memoized { function, .. } => function.is_native(),
            _ => false,
        }
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            Self::Compiled(f) => f.name.as_deref(),
            Self::Native(f) => Some(&f.name),
            Self::Closure(c) => c.prototype.name.as_deref(),
            Self::Memoized { function, .. } => function.name(),
        }
    }

    pub fn static_type(&self) -> StaticType {
        match self {
            Self::Compiled(f) => StaticType::Function {
                parameters: match &f.type_signature {
                    TypeSignature::Variadic => None,
                    TypeSignature::Exact(types) => {
                        Some(types.iter().map(|x| x.type_name.clone()).collect())
                    }
                },
                return_type: Box::new(f.return_type.clone()),
            },
            Self::Native(f) => f.static_type.clone(),
            Self::Closure(c) => Function::Compiled(c.prototype.clone()).static_type(),
            Self::Memoized { function, .. } => function.static_type(),
        }
    }
}

impl From<Object> for Value {
    fn from(value: Object) -> Self {
        Self::Object(Rc::new(value))
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Compiled(func) => write!(f, "function {:?}", func.name),
            Self::Native(native) => write!(f, "<native function {:?}>", native.static_type),
            Self::Closure(closure) => write!(f, "<closure over {:?}>", closure.prototype.name),
            Self::Memoized { function, .. } => write!(f, "<memoized {:?}>", function),
        }
    }
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn {:?}>", self.static_type)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(n) => write!(f, "{n}"),
            Self::Float(n) => write!(f, "{n}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::None => write!(f, "None"),
            Self::Object(obj) => write!(f, "{obj}"),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Some(v) => write!(f, "Some({v})"),
            Self::BigInt(n) => write!(f, "{n}"),
            Self::Complex(c) => write!(f, "{c}"),
            Self::Rational(r) => write!(f, "{r}"),
            // Strings display without quotes at the top level.
            Self::String(s) => write!(f, "{}", s.borrow()),
            Self::List(vs) => {
                let vs = vs.borrow();
                write!(f, "[")?;
                for (i, v) in vs.iter().enumerate() {
                    // Use Debug (repr) for elements so strings appear quoted inside lists.
                    write!(f, "{v:?}")?;
                    if i + 1 < vs.len() {
                        write!(f, ",")?;
                    }
                }
                write!(f, "]")
            }
            // Empty tuple is the unit value — prints as nothing, matching the interpreter.
            Self::Tuple(vs) if vs.is_empty() => Ok(()),
            Self::Tuple(vs) => {
                write!(f, "(")?;
                for (i, v) in vs.iter().enumerate() {
                    write!(f, "{v:?}")?;
                    if i + 1 < vs.len() {
                        write!(f, ",")?;
                    }
                }
                write!(f, ")")
            }
            Self::Map { entries, .. } => {
                write!(f, "%{{")?;
                let entries = entries.borrow();
                for (i, (k, v)) in entries.iter().enumerate() {
                    write!(f, "{k:?}: {v:?}")?;
                    if i + 1 < entries.len() {
                        write!(f, ",")?;
                    }
                }
                write!(f, "}}")
            }
            Self::Function(func) => write!(f, "{func}"),
            Self::OverloadSet(slots) => write!(f, "<overload set ({} candidates)>", slots.len()),
            Self::Iterator(iter) => match iter.borrow().len() {
                Some(n) => write!(f, "<iterator (len={n})>"),
                None => write!(f, "<iterator>"),
            },
            Self::Deque(d) => write!(f, "Deque(len={})", d.borrow().len()),
            Self::MinHeap(h) => write!(f, "MinHeap(len={})", h.borrow().len()),
            Self::MaxHeap(h) => write!(f, "MaxHeap(len={})", h.borrow().len()),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            // Strings in repr/debug context are quoted.
            Self::Object(obj) if matches!(obj.as_ref(), Object::String(_)) => {
                write!(f, "{obj:?}")
            }
            // Everything else uses Display.
            _ => write!(f, "{self}"),
        }
    }
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Some(v) => f.debug_tuple("Some").field(v).finish(),
            Self::BigInt(n) => f.debug_tuple("BigInt").field(n).finish(),
            Self::Complex(c) => f.debug_tuple("Complex").field(c).finish(),
            Self::Rational(r) => f.debug_tuple("Rational").field(r).finish(),
            // Strings in debug/repr context are shown quoted.
            Self::String(s) => write!(f, "\"{}\"", s.borrow()),
            Self::List(vs) => f.debug_tuple("List").field(&vs.borrow()).finish(),
            Self::Tuple(vs) => f.debug_tuple("Tuple").field(vs).finish(),
            Self::Map { entries, default } => f
                .debug_struct("Map")
                .field("entries", &*entries.borrow())
                .field("default", default)
                .finish(),
            Self::Function(func) => write!(f, "{func:?}"),
            Self::OverloadSet(slots) => f.debug_tuple("OverloadSet").field(slots).finish(),
            Self::Iterator(iter) => match iter.borrow().len() {
                Some(n) => write!(f, "<iterator (len={n})>"),
                None => write!(f, "<iterator>"),
            },
            Self::Deque(d) => write!(f, "Deque(len={})", d.borrow().len()),
            Self::MinHeap(h) => write!(f, "MinHeap(len={})", h.borrow().len()),
            Self::MaxHeap(h) => write!(f, "MaxHeap(len={})", h.borrow().len()),
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Compiled(func) => {
                let name = func.name.as_deref().unwrap_or("?");
                write!(f, "<fn {name}>")
            }
            Self::Native(native) => write!(f, "<native fn {:?}>", native.static_type),
            Self::Closure(closure) => write!(f, "<closure over {:?}>", closure.prototype.name),
            Self::Memoized { function, .. } => write!(f, "<memoized {function}>"),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            // Same-type fast paths
            (Self::Int(a), Self::Int(b)) => Some(a.cmp(b)),
            (Self::Float(a), Self::Float(b)) => OrderedFloat(*a).partial_cmp(&OrderedFloat(*b)),
            (Self::Bool(a), Self::Bool(b)) => a.partial_cmp(b),
            (Self::Object(a), Self::Object(b)) => a.partial_cmp(b),
            // Any numeric cross-type comparison (Int/Float vs BigInt/Rational/Complex etc.)
            // delegates to ndc_core::Number which handles all cases the interpreter does.
            (a, b) => vm_value_to_number(a)?.partial_cmp(&vm_value_to_number(b)?),
        }
    }
}

/// Convert a VM numeric value to a `ndc_core::Number` for cross-type comparison.
/// Returns `None` for non-numeric values (Bool, None, String, List, …).
fn vm_value_to_number(v: &Value) -> Option<Number> {
    match v {
        Value::Int(i) => Some(Number::Int(Int::Int64(*i))),
        Value::Float(f) => Some(Number::Float(*f)),
        Value::Object(obj) => match obj.as_ref() {
            Object::BigInt(b) => Some(Number::Int(Int::BigInt(b.clone()))),
            Object::Rational(r) => Some(Number::Rational(Box::new(r.clone()))),
            Object::Complex(c) => Some(Number::Complex(*c)),
            _ => None,
        },
        _ => None,
    }
}

/// Convert an `Object` numeric value to a `ndc_core::Number` for cross-type comparison.
fn obj_to_number(obj: &Object) -> Option<Number> {
    match obj {
        Object::BigInt(b) => Some(Number::Int(Int::BigInt(b.clone()))),
        Object::Rational(r) => Some(Number::Rational(Box::new(r.clone()))),
        Object::Complex(c) => Some(Number::Complex(*c)),
        _ => None,
    }
}

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::BigInt(a), Self::BigInt(b)) => a.partial_cmp(b),
            (Self::Rational(a), Self::Rational(b)) => a.partial_cmp(b),
            (Self::String(a), Self::String(b)) => a.borrow().partial_cmp(&*b.borrow()),
            (Self::List(a), Self::List(b)) => a.borrow().partial_cmp(&*b.borrow()),
            (Self::Tuple(a), Self::Tuple(b)) => a.partial_cmp(b),
            // Cross-type numeric: BigInt vs Rational, BigInt vs Complex, Rational vs Complex, etc.
            (a, b) => obj_to_number(a)?.partial_cmp(&obj_to_number(b)?),
        }
    }
}

impl FallibleOrd for Value {
    type Error = String;

    fn try_cmp(&self, other: &Self) -> Result<Ordering, String> {
        self.partial_cmp(other).ok_or_else(|| {
            format!(
                "{} cannot be compared to {}",
                self.static_type(),
                other.static_type()
            )
        })
    }
}

impl Value {
    /// Compare a comparator return value to zero, returning the ordering.
    /// Comparators return a number: negative means less, zero means equal, positive means greater.
    pub fn cmp_to_zero(&self) -> Result<Ordering, String> {
        match self {
            Self::Int(n) => Ok(n.cmp(&0)),
            Self::Float(f) => f
                .partial_cmp(&0.0)
                .ok_or_else(|| "NaN in comparator result".to_string()),
            Self::Object(obj) => match obj.as_ref() {
                Object::BigInt(b) => Ok(b.cmp(&num::BigInt::from(0))),
                Object::Rational(r) => Ok(r.cmp(&num::BigRational::from(num::BigInt::from(0)))),
                _ => Err(format!(
                    "comparator must return a number, got {}",
                    self.static_type()
                )),
            },
            _ => Err(format!(
                "comparator must return a number, got {}",
                self.static_type()
            )),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => OrderedFloat(*a) == OrderedFloat(*b),
            // Cross-type numeric equality: consistent with PartialOrd's cross-numeric path.
            (Self::Int(_), Self::Float(_)) | (Self::Float(_), Self::Int(_)) => {
                vm_value_to_number(self) == vm_value_to_number(other)
            }
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::None, Self::None) => true,
            (Self::Object(a), Self::Object(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Int(n) => {
                state.write_u8(1);
                n.hash(state);
            }
            Self::Float(f) => {
                // Normalise whole-number floats to the same hash as their integer equivalent,
                // so that Int(1) and Float(1.0) hash identically (consistent with PartialEq).
                match Int::from_f64_if_int(*f) {
                    Some(i) => {
                        state.write_u8(1);
                        i.hash(state);
                    }
                    None => {
                        state.write_u8(2);
                        OrderedFloat(*f).hash(state);
                    }
                }
            }
            Self::Bool(true) => state.write_u8(3),
            Self::Bool(false) => state.write_u8(4),
            Self::None => state.write_u8(5),
            Self::Object(o) => {
                state.write_u8(6);
                o.hash(state);
            }
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Some(a), Self::Some(b)) => a == b,
            (Self::BigInt(a), Self::BigInt(b)) => a == b,
            (Self::Complex(a), Self::Complex(b)) => a == b,
            (Self::Rational(a), Self::Rational(b)) => a == b,
            (Self::String(a), Self::String(b)) => a.borrow().eq(&*b.borrow()),
            (Self::List(a), Self::List(b)) => a.borrow().eq(&*b.borrow()),
            (Self::Tuple(a), Self::Tuple(b)) => a == b,
            (
                Self::Map {
                    entries: a_entries, ..
                },
                Self::Map {
                    entries: b_entries, ..
                },
            ) => a_entries.borrow().eq(&*b_entries.borrow()),
            (Self::Function(a), Self::Function(b)) => {
                // Compare function values by identity (pointer equality)
                match (a, b) {
                    (Function::Compiled(a), Function::Compiled(b)) => {
                        std::ptr::addr_eq(Rc::as_ptr(a), Rc::as_ptr(b))
                    }
                    (Function::Native(a), Function::Native(b)) => {
                        std::ptr::addr_eq(Rc::as_ptr(a), Rc::as_ptr(b))
                    }
                    (Function::Closure(a), Function::Closure(b)) => {
                        Rc::as_ptr(&a.prototype) == Rc::as_ptr(&b.prototype)
                    }
                    (Function::Memoized { cache: a, .. }, Function::Memoized { cache: b, .. }) => {
                        Rc::ptr_eq(a, b)
                    }
                    _ => false,
                }
            }
            (Self::OverloadSet(a), Self::OverloadSet(b)) => a == b,
            (Self::Iterator(a), Self::Iterator(b)) => {
                // Compare iterators by pointer identity
                std::ptr::addr_eq(Rc::as_ptr(a), Rc::as_ptr(b))
            }
            (Self::Deque(a), Self::Deque(b)) => a.borrow().eq(&*b.borrow()),
            // Heaps: pointer identity (no meaningful value equality).
            // The RefCell lives inside the Rc<Object> allocation, so comparing its
            // address is equivalent to comparing the outer Rc pointers.
            (Self::MinHeap(a), Self::MinHeap(b)) => std::ptr::eq(a, b),
            (Self::MaxHeap(a), Self::MaxHeap(b)) => std::ptr::eq(a, b),
            _ => false,
        }
    }
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Some(v) => {
                state.write_u8(1);
                v.hash(state);
            }
            Self::BigInt(n) => {
                state.write_u8(2);
                n.hash(state);
            }
            Self::Complex(c) => {
                state.write_u8(3);
                c.re.to_bits().hash(state);
                c.im.to_bits().hash(state);
            }
            Self::Rational(r) => {
                state.write_u8(4);
                r.hash(state);
            }
            Self::String(s) => {
                state.write_u8(5);
                s.borrow().hash(state);
            }
            Self::List(vs) => {
                state.write_u8(6);
                for v in vs.borrow().iter() {
                    v.hash(state);
                }
            }
            Self::Tuple(vs) => {
                state.write_u8(7);
                for v in vs.iter() {
                    v.hash(state);
                }
            }
            Self::Map { entries, .. } => {
                state.write_u8(8);
                // Order-independent hash: XOR-fold of pair hashes
                // (same approach as the non-VM interpreter)
                // NOTE: the default value is not part of the identity of the map
                let mut acc = 0u64;
                let mut cube_acc = 0u64;
                for (key, value) in entries.borrow().iter() {
                    let mut hasher = DefaultHasher::default();
                    key.hash(&mut hasher);
                    value.hash(&mut hasher);

                    let f = hasher.finish();
                    acc = acc.wrapping_add(f);
                    cube_acc = cube_acc.wrapping_add(f.wrapping_mul(f));
                }
                state.write_u64(acc);
                state.write_u64(cube_acc);
            }
            Self::Function(f) => {
                state.write_u8(9);
                match f {
                    Function::Compiled(func) => {
                        Rc::as_ptr(func).hash(state);
                    }
                    Function::Native(native) => {
                        Rc::as_ptr(native).hash(state);
                    }
                    Function::Closure(closure) => {
                        Rc::as_ptr(&closure.prototype).hash(state);
                    }
                    Function::Memoized { cache, .. } => {
                        Rc::as_ptr(cache).hash(state);
                    }
                }
            }
            Self::OverloadSet(_) => {
                // OverloadSet hashing is skipped since ResolvedVar doesn't implement Hash
                // Treat them as opaque value types, hash by pointer identity
                state.write_u8(10);
            }
            Self::Iterator(iter) => {
                state.write_u8(11);
                Rc::as_ptr(iter).hash(state);
            }
            Self::Deque(d) => {
                state.write_u8(12);
                for v in d.borrow().iter() {
                    v.hash(state);
                }
            }
            Self::MinHeap(h) => {
                state.write_u8(13);
                // Hash by address: the RefCell lives inside the Rc<Object> allocation,
                // so this is equivalent to hashing the outer Rc pointer.
                (h as *const RefCell<BinaryHeap<Reverse<OrdValue>>>).hash(state);
            }
            Self::MaxHeap(h) => {
                state.write_u8(14);
                (h as *const RefCell<BinaryHeap<OrdValue>>).hash(state);
            }
        }
    }
}
