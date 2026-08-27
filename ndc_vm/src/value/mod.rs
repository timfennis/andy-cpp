mod function;

pub use function::*;

use crate::iterator::SharedIterator;
use ndc_core::StaticType;
use ndc_core::compare::FallibleOrd;
use ndc_core::hash_map::{DefaultHasher, HashMap};
use ndc_core::int::Int;
use ndc_core::num::AdvancedNumber;
use ndc_core::r#struct::StructInfo;
use ndc_parser::ResolvedVar;
use std::cell::RefCell;
use std::cmp::{Ordering, Reverse};
use std::collections::BinaryHeap;
use std::collections::VecDeque;
use std::fmt;
use std::fmt::Formatter;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

thread_local! {
    static UNIT: Value = Value::Object(Rc::new(Object::Tuple(vec![])));
}

const DIAGNOSTIC_TYPE_MAX_DEPTH: usize = 4;
const DIAGNOSTIC_TYPE_VALUE_BUDGET: usize = 256;
type ConformanceKey = (*const Object, *const StaticType);
type ConformanceCache = HashMap<ConformanceKey, bool>;

fn bounded_element_type<'a>(
    values: impl Iterator<Item = &'a Value>,
    depth: usize,
    budget: &mut usize,
) -> StaticType {
    let mut element_type: Option<StaticType> = None;
    for value in values {
        if *budget == 0 {
            return StaticType::Any;
        }
        let found = value.static_type_with_budget(depth, budget);
        element_type = Some(match element_type {
            Some(previous) => previous.lub(&found),
            None => found,
        });
    }
    element_type.unwrap_or(StaticType::Any)
}

/// Enumerates all the different types of values that exist in the language
/// All values should be pretty cheap to clone because the bigger ones are wrapped using Rc's
#[derive(Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Number(Rc<AdvancedNumber>),
    Bool(bool),
    None,
    Object(Rc<Object>),
}

#[derive(Clone)]
pub enum Object {
    Some(Value),
    String(Rc<RefCell<String>>),
    List(RefCell<Vec<Value>>),
    Tuple(Vec<Value>),
    Map {
        entries: RefCell<HashMap<Value, Value>>,
        default: Option<Value>,
    },
    Function(Function),
    Struct {
        info: Rc<StructInfo>,
        fields: RefCell<Vec<Value>>,
    },
    /// A set of overload candidates the runtime narrows per call. Scalars are
    /// walked first (first-match-wins, same shape as the master baseline);
    /// `vec_candidates` is only consulted as a fallback for operator-form
    /// calls when no scalar accepts the args. `vec_candidates` is empty for
    /// non-operator call sites, so the hot path stays a single `Vec<ResolvedVar>`
    /// walk identical to scalar-only dispatch.
    OverloadSet {
        scalars: Vec<ResolvedVar>,
        vec_candidates: Vec<ResolvedVar>,
    },
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

/// Type alias for [`Value`] that signals `StaticType::Map` to the `#[export_module]` macro.
///
/// Use this instead of `ndc_vm::value::Value` in stdlib function signatures when the parameter
/// must be a map or set. The extraction is identical (zero-copy pass-through on the VM path) but
/// the macro emits `StaticType::Map { key: Any, value: Any }` instead of `StaticType::Any`, which
/// preserves the correct type for dispatch and static analysis.
pub type MapValue = Value;

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

impl Value {
    pub fn unit() -> Self {
        UNIT.with(Clone::clone)
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, Self::Object(obj) if matches!(&**obj, Object::Tuple(v) if v.is_empty()))
    }

    pub fn function(function: Function) -> Self {
        Self::Object(Rc::new(Object::Function(function)))
    }

    pub fn string<S: Into<String>>(string: S) -> Self {
        Self::Object(Rc::new(Object::String(Rc::new(RefCell::new(
            string.into(),
        )))))
    }

    pub fn from_string_rc(rc: Rc<RefCell<String>>) -> Self {
        Self::Object(Rc::new(Object::String(rc)))
    }

    pub fn iterator(iter: SharedIterator) -> Self {
        Self::Object(Rc::new(Object::Iterator(iter)))
    }

    pub fn list(values: Vec<Self>) -> Self {
        Self::Object(Rc::new(Object::list(values)))
    }

    pub fn tuple(values: Vec<Self>) -> Self {
        Self::Object(Rc::new(Object::Tuple(values)))
    }

    pub fn int(i: i64) -> Self {
        Self::Int(i)
    }

    pub fn float(f: f64) -> Self {
        Self::Float(f)
    }

    pub fn bigint(i: num::BigInt) -> Self {
        Self::number(AdvancedNumber::Int(Int::BigInt(i)))
    }

    pub fn complex(c: num::complex::Complex64) -> Self {
        Self::number(AdvancedNumber::Complex(c))
    }

    pub fn number(number: AdvancedNumber) -> Self {
        Self::Number(Rc::new(number))
    }

    /// Creates a shallow copy: scalars are copied by value; mutable collection
    /// types (String, List, Map, Deque, heaps, structs) get new independent
    /// containers with cloned contents; immutable / identity types (Tuple,
    /// Function, Iterator, Some, `OverloadSet`) share the Rc.
    pub fn shallow_clone(&self) -> Self {
        match self {
            Self::Object(obj) => match obj.as_ref() {
                Object::String(rc) => Self::Object(Rc::new(Object::String(Rc::new(RefCell::new(
                    rc.borrow().clone(),
                ))))),
                Object::List(refcell) => Self::Object(Rc::new(Object::List(RefCell::new(
                    refcell.borrow().clone(),
                )))),
                Object::Deque(refcell) => Self::Object(Rc::new(Object::Deque(RefCell::new(
                    refcell.borrow().clone(),
                )))),
                Object::MinHeap(refcell) => Self::Object(Rc::new(Object::MinHeap(RefCell::new(
                    refcell.borrow().clone(),
                )))),
                Object::MaxHeap(refcell) => Self::Object(Rc::new(Object::MaxHeap(RefCell::new(
                    refcell.borrow().clone(),
                )))),
                Object::Map { entries, default } => Self::Object(Rc::new(Object::Map {
                    entries: RefCell::new(entries.borrow().clone()),
                    default: default.clone(),
                })),
                Object::Struct { info, fields } => Self::Object(Rc::new(Object::Struct {
                    info: Rc::clone(info),
                    fields: RefCell::new(fields.borrow().clone()),
                })),
                _ => Self::Object(obj.clone()),
            },
            // Scalars are already independent — just copy.
            other => other.clone(),
        }
    }

    /// Creates a deep copy: all nested mutable containers are recursively
    /// duplicated so the result shares no mutable state with the original.
    pub fn deep_copy(&self) -> Self {
        match self {
            Self::Object(obj) => Self::Object(Rc::new(obj.deep_copy())),
            other => other.clone(),
        }
    }

    /// Returns the static type of this value.
    ///
    /// # Performance
    ///
    /// **O(n) for `List`, `Map`, and `Deque`** — these variants iterate all
    /// elements to compute the element type via `lub`.  Avoid calling this in
    /// hot paths on container values.  Use the dedicated helpers instead:
    ///
    /// - [`Value::is_number`] — O(1) check for numeric types
    pub fn static_type(&self) -> StaticType {
        let mut budget = usize::MAX;
        self.static_type_with_budget(usize::MAX, &mut budget)
    }

    /// Returns a runtime type description whose recursion depth and total
    /// number of inspected values are bounded. Once either limit is reached,
    /// the remaining subtree is widened to `Any`.
    pub(crate) fn diagnostic_type(&self) -> StaticType {
        let mut budget = DIAGNOSTIC_TYPE_VALUE_BUDGET;
        self.static_type_with_budget(DIAGNOSTIC_TYPE_MAX_DEPTH, &mut budget)
    }

    fn static_type_with_budget(&self, depth: usize, budget: &mut usize) -> StaticType {
        if *budget == 0 {
            return StaticType::Any;
        }
        *budget -= 1;

        match self {
            Self::Int(_) => StaticType::Int,
            Self::Float(_) => StaticType::Float,
            Self::Number(_) => StaticType::Number,
            Self::Bool(_) => StaticType::Bool,
            Self::None => StaticType::Option(Box::new(StaticType::Any)),
            Self::Object(obj) => obj.static_type_with_budget(depth, budget),
        }
    }

    /// Returns `true` if this value is an Int, Float, or Number.
    ///
    /// Prefer this over `self.static_type().is_number()` in hot paths — this is O(1)
    /// and never allocates, whereas `static_type()` on containers is O(n).
    pub fn is_number(&self) -> bool {
        matches!(self, Self::Int(_) | Self::Float(_) | Self::Number(_))
    }

    /// Check whether this value satisfies a function parameter type at runtime,
    /// avoiding full container iteration for common cases.
    ///
    /// When `param` is a container type whose inner types are all `Any` (e.g.
    /// `Sequence(Any)`, `Map { Any, Any }`, `List(Any)`), only the outer kind is
    /// checked — no element iteration occurs.  All other cases fall back to
    /// `self.static_type().is_subtype(param)`.
    pub fn matches_param(&self, param: &StaticType) -> bool {
        match param {
            StaticType::Any => true,
            StaticType::Int => {
                matches!(self, Self::Int(_))
            }
            StaticType::Float => matches!(self, Self::Float(_)),
            StaticType::Bool => matches!(self, Self::Bool(_)),
            StaticType::Number => matches!(self, Self::Number(_)),
            StaticType::String => {
                matches!(self, Self::Object(o) if matches!(o.as_ref(), Object::String(_)))
            }
            StaticType::List(t) if matches!(t.as_ref(), StaticType::Any) => {
                matches!(self, Self::Object(o) if matches!(o.as_ref(), Object::List(_)))
            }
            StaticType::Deque(t) if matches!(t.as_ref(), StaticType::Any) => {
                matches!(self, Self::Object(o) if matches!(o.as_ref(), Object::Deque(_)))
            }
            StaticType::Map { key, value }
                if matches!(
                    (key.as_ref(), value.as_ref()),
                    (StaticType::Any, StaticType::Any)
                ) =>
            {
                matches!(self, Self::Object(o) if matches!(o.as_ref(), Object::Map { .. }))
            }
            StaticType::Sequence(t) if matches!(t.as_ref(), StaticType::Any) => {
                matches!(self, Self::Object(o) if matches!(
                    o.as_ref(),
                    Object::List(_)
                        | Object::Tuple(_)
                        | Object::String(_)
                        | Object::Deque(_)
                        | Object::Map { .. }
                        | Object::Iterator(_)
                        | Object::MinHeap(_)
                        | Object::MaxHeap(_)
                ))
            }
            // These container params require element-type scanning to verify — skip to
            // avoid O(N) cost. The analyser should resolve typed container params at
            // compile time; if it falls through to dynamic dispatch, return false.
            StaticType::List(_)
            | StaticType::Deque(_)
            | StaticType::Sequence(_)
            | StaticType::Map { .. }
            | StaticType::Tuple(_) => false,
            _ => self.static_type().is_subtype(param),
        }
    }

    /// Check whether this value conforms to `target`, scanning container
    /// elements recursively. Empty containers conform to any element type.
    ///
    /// Unlike [`Value::matches_param`] this scans the value by design: it backs
    /// the runtime check of `as` casts, where the caller explicitly opted into
    /// the scan. Shared container aliases are memoized by object and target
    /// type. Iterators can't be inspected without consuming them, so they only
    /// conform to targets with `Any` elements.
    pub fn conforms_to(&self, target: &StaticType) -> bool {
        self.conforms_to_cached(target, &mut ConformanceCache::default())
    }

    fn conforms_to_cached(&self, target: &StaticType, cache: &mut ConformanceCache) -> bool {
        let cache_key = match self {
            Self::Object(object) => Some((Rc::as_ptr(object), std::ptr::from_ref(target))),
            _ => None,
        };
        if let Some(cache_key) = cache_key
            && let Some(result) = cache.get(&cache_key)
        {
            return *result;
        }

        // Static types are finite trees, so this pair cannot be re-entered
        // before its result is stored.
        let result = match target {
            StaticType::Any => true,
            StaticType::Option(inner) => match self {
                Self::None => true,
                Self::Object(object) => match object.as_ref() {
                    Object::Some(value) => value.conforms_to_cached(inner, cache),
                    _ => false,
                },
                _ => false,
            },
            StaticType::List(element) => matches!(
                self,
                Self::Object(object) if matches!(object.as_ref(), Object::List(values)
                    if values.borrow().iter().all(|value| value.conforms_to_cached(element, cache)))
            ),
            StaticType::Deque(element) => matches!(
                self,
                Self::Object(object) if matches!(object.as_ref(), Object::Deque(values)
                    if values.borrow().iter().all(|value| value.conforms_to_cached(element, cache)))
            ),
            StaticType::Tuple(elements) => matches!(
                self,
                Self::Object(object) if matches!(object.as_ref(), Object::Tuple(values)
                    if values.len() == elements.len()
                        && values.iter().zip(elements).all(|(value, element)| value.conforms_to_cached(element, cache)))
            ),
            StaticType::Map { key, value } => match self {
                Self::Object(object) => match object.as_ref() {
                    Object::Map { entries, default } => {
                        // A missing-key lookup inserts the default (or the
                        // result of calling it), so the default must conform
                        // to the value type as well. A default function's
                        // results can't be verified without calling it.
                        let default_conforms = match default {
                            None => true,
                            Some(Self::Object(object))
                                if matches!(object.as_ref(), Object::Function(_)) =>
                            {
                                matches!(value.as_ref(), StaticType::Any)
                            }
                            Some(default) => default.conforms_to_cached(value, cache),
                        };
                        default_conforms
                            && entries.borrow().iter().all(|(entry_key, entry_value)| {
                                entry_key.conforms_to_cached(key, cache)
                                    && entry_value.conforms_to_cached(value, cache)
                            })
                    }
                    _ => false,
                },
                _ => false,
            },
            StaticType::MinHeap(element) => matches!(
                self,
                Self::Object(object) if matches!(object.as_ref(), Object::MinHeap(values)
                    if values.borrow().iter().all(|Reverse(OrdValue(value))| value.conforms_to_cached(element, cache)))
            ),
            StaticType::MaxHeap(element) => matches!(
                self,
                Self::Object(object) if matches!(object.as_ref(), Object::MaxHeap(values)
                    if values.borrow().iter().all(|OrdValue(value)| value.conforms_to_cached(element, cache)))
            ),
            StaticType::Sequence(element) => match self {
                Self::Object(object) => match object.as_ref() {
                    Object::List(values) => values
                        .borrow()
                        .iter()
                        .all(|value| value.conforms_to_cached(element, cache)),
                    Object::Tuple(values) => values
                        .iter()
                        .all(|value| value.conforms_to_cached(element, cache)),
                    Object::Deque(values) => values
                        .borrow()
                        .iter()
                        .all(|value| value.conforms_to_cached(element, cache)),
                    Object::String(_) => StaticType::String.is_subtype(element),
                    Object::Map { entries, .. } => {
                        entries
                            .borrow()
                            .iter()
                            .all(|(key, value)| match element.as_ref() {
                                StaticType::Any => true,
                                StaticType::Tuple(elements) if elements.len() == 2 => {
                                    key.conforms_to_cached(&elements[0], cache)
                                        && value.conforms_to_cached(&elements[1], cache)
                                }
                                StaticType::Sequence(inner) => {
                                    key.conforms_to_cached(inner, cache)
                                        && value.conforms_to_cached(inner, cache)
                                }
                                _ => false,
                            })
                    }
                    Object::MinHeap(values) => values
                        .borrow()
                        .iter()
                        .all(|Reverse(OrdValue(value))| value.conforms_to_cached(element, cache)),
                    Object::MaxHeap(values) => values
                        .borrow()
                        .iter()
                        .all(|OrdValue(value)| value.conforms_to_cached(element, cache)),
                    Object::Iterator(_) => matches!(element.as_ref(), StaticType::Any),
                    _ => false,
                },
                _ => false,
            },
            // The remaining targets are non-container types (Never, Bool,
            // numerics, String, Function, Struct, Iterator). Container values
            // conform to none of them; answering that without `static_type`
            // keeps recursion bounded by the target's depth, so conformance
            // checks terminate even on self-referential containers.
            _ => match self {
                Self::Object(object)
                    if matches!(
                        object.as_ref(),
                        Object::List(_)
                            | Object::Tuple(_)
                            | Object::Map { .. }
                            | Object::Deque(_)
                            | Object::Some(_)
                    ) =>
                {
                    false
                }
                _ => self.static_type().is_subtype(target),
            },
        };

        if let Some(cache_key) = cache_key {
            cache.insert(cache_key, result);
        }
        result
    }

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
        let Self::Object(obj) = self else {
            return None;
        };
        match Rc::unwrap_or_clone(obj) {
            Object::List(l) => Some(ValueIter::List(l.into_inner().into_iter())),
            Object::Tuple(t) => Some(ValueIter::List(t.into_iter())),
            Object::Deque(d) => Some(ValueIter::Deque(d.into_inner().into_iter())),
            Object::Iterator(i) => Some(ValueIter::Shared(i)),
            Object::String(s) => {
                let chars: Vec<Self> = s
                    .borrow()
                    .chars()
                    .map(|c| Self::string(c.to_string()))
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

    /// Clone the payload of a Number value.
    pub fn to_number(&self) -> Option<AdvancedNumber> {
        self.as_number().cloned()
    }

    pub fn as_number(&self) -> Option<&AdvancedNumber> {
        match self {
            Self::Number(number) => Some(number.as_ref()),
            _ => None,
        }
    }

    pub fn to_advanced_number(&self) -> Option<AdvancedNumber> {
        vm_value_to_number(self)
    }

    /// Wrap an advanced numeric payload as a Number value.
    pub fn from_number(n: AdvancedNumber) -> Self {
        Self::number(n)
    }

    /// Extract an integer VM value as a `ndc_core::Int`.
    /// Returns `None` for non-integer values.
    pub fn to_int(&self) -> Option<Int> {
        match self {
            Self::Int(i) => Some(Int::Int64(*i)),
            _ => None,
        }
    }

    /// Convert a `ndc_core::Int` to a VM value.
    pub fn from_int(i: Int) -> Self {
        match i {
            Int::Int64(n) => Self::Int(n),
            Int::BigInt(b) => Self::number(AdvancedNumber::Int(Int::BigInt(b))),
        }
    }

    /// Convert a numeric VM value to `f64`, coercing integers and rationals.
    /// Returns `None` for non-numeric values (Bool, None, String, …).
    pub fn to_f64(&self) -> Option<f64> {
        use num::ToPrimitive;
        match self {
            Self::Float(f) => Some(*f),
            Self::Int(i) => i.to_f64(),
            Self::Number(number) => number.to_f64(),
            _ => None,
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

    /// Recursively deep-copies all mutable containers.
    pub fn deep_copy(&self) -> Self {
        match self {
            Self::Some(v) => Self::Some(v.deep_copy()),
            Self::String(rc) => Self::String(Rc::new(RefCell::new(rc.borrow().clone()))),
            Self::List(refcell) => Self::List(RefCell::new(
                refcell.borrow().iter().map(Value::deep_copy).collect(),
            )),
            Self::Tuple(v) => Self::Tuple(v.iter().map(Value::deep_copy).collect()),
            Self::Map { entries, default } => Self::Map {
                entries: RefCell::new(
                    entries
                        .borrow()
                        .iter()
                        .map(|(k, v)| (k.deep_copy(), v.deep_copy()))
                        .collect(),
                ),
                default: default.as_ref().map(Value::deep_copy),
            },
            Self::Deque(refcell) => Self::Deque(RefCell::new(
                refcell.borrow().iter().map(Value::deep_copy).collect(),
            )),
            Self::MinHeap(refcell) => Self::MinHeap(RefCell::new(refcell.borrow().clone())),
            Self::MaxHeap(refcell) => Self::MaxHeap(RefCell::new(refcell.borrow().clone())),
            Self::Struct { info, fields } => Self::Struct {
                info: Rc::clone(info),
                fields: RefCell::new(fields.borrow().iter().map(Value::deep_copy).collect()),
            },
            // Iterator: deep_copy if supported, otherwise share the Rc.
            Self::Iterator(shared) => {
                if let Some(copy) = shared.borrow().deep_copy() {
                    Self::Iterator(copy)
                } else {
                    Self::Iterator(Rc::clone(shared))
                }
            }
            // Immutable / identity types: clone the Rc via Object's derive.
            other => other.clone(),
        }
    }

    /// Returns the best static type descriptor for this runtime value.
    ///
    /// `[1, 2]` reports `List<Int>` and `%{"a": 1}` reports `Map<String, Int>`:
    /// lists and deques join their element types into a least upper bound, while
    /// maps do so separately for keys and values. Tuples preserve each position's
    /// type, and `Some` wraps the type of its inner value. Empty lists, deques, and
    /// maps report `Any` for the types they cannot infer.
    ///
    /// Iterators and heaps always report `Any` elements, because inspecting them
    /// would consume or expose their internal values.
    pub fn static_type(&self) -> StaticType {
        let mut budget = usize::MAX;
        self.static_type_with_budget(usize::MAX, &mut budget)
    }

    fn static_type_with_budget(&self, depth: usize, budget: &mut usize) -> StaticType {
        match self {
            Self::Some(inner) => {
                if depth == 0 {
                    return StaticType::Option(Box::new(StaticType::Any));
                }
                StaticType::Option(Box::new(inner.static_type_with_budget(depth - 1, budget)))
            }
            Self::String(_) => StaticType::String,
            Self::List(elements) => {
                if depth == 0 {
                    return StaticType::List(Box::new(StaticType::Any));
                }
                let elements = elements.borrow();
                let elem_type = bounded_element_type(elements.iter(), depth - 1, budget);
                StaticType::List(Box::new(elem_type))
            }
            Self::Tuple(elements) => {
                if depth == 0 {
                    if elements.len() > *budget {
                        return StaticType::Any;
                    }
                    *budget -= elements.len();
                    return StaticType::Tuple(vec![StaticType::Any; elements.len()]);
                }
                if elements.len() > *budget {
                    return StaticType::Any;
                }
                let mut types = Vec::with_capacity(elements.len());
                for element in elements {
                    if *budget == 0 {
                        return StaticType::Any;
                    }
                    types.push(element.static_type_with_budget(depth - 1, budget));
                }
                StaticType::Tuple(types)
            }
            Self::Map { entries, .. } => {
                if depth == 0 {
                    return StaticType::Map {
                        key: Box::new(StaticType::Any),
                        value: Box::new(StaticType::Any),
                    };
                }
                let entries = entries.borrow();
                let mut key_type: Option<StaticType> = None;
                let mut value_type: Option<StaticType> = None;
                for (key, value) in entries.iter() {
                    if *budget == 0 {
                        return StaticType::Map {
                            key: Box::new(StaticType::Any),
                            value: Box::new(StaticType::Any),
                        };
                    }
                    let found_key = key.static_type_with_budget(depth - 1, budget);
                    if *budget == 0 {
                        return StaticType::Map {
                            key: Box::new(StaticType::Any),
                            value: Box::new(StaticType::Any),
                        };
                    }
                    let found_value = value.static_type_with_budget(depth - 1, budget);
                    key_type = Some(match key_type {
                        Some(previous) => previous.lub(&found_key),
                        None => found_key,
                    });
                    value_type = Some(match value_type {
                        Some(previous) => previous.lub(&found_value),
                        None => found_value,
                    });
                }
                StaticType::Map {
                    key: Box::new(key_type.unwrap_or(StaticType::Any)),
                    value: Box::new(value_type.unwrap_or(StaticType::Any)),
                }
            }
            Self::Function(f) => f.static_type(),
            Self::OverloadSet { .. } => StaticType::Any,
            Self::Iterator(_) => StaticType::Iterator(Box::new(StaticType::Any)),
            Self::Deque(elements) => {
                if depth == 0 {
                    return StaticType::Deque(Box::new(StaticType::Any));
                }
                let elements = elements.borrow();
                let elem_type = bounded_element_type(elements.iter(), depth - 1, budget);
                StaticType::Deque(Box::new(elem_type))
            }
            Self::MinHeap(_) => StaticType::MinHeap(Box::new(StaticType::Any)),
            Self::MaxHeap(_) => StaticType::MaxHeap(Box::new(StaticType::Any)),
            Self::Struct { info, .. } => info.static_type(),
        }
    }

    pub fn function_prototype(&self) -> Option<&Rc<CompiledFunction>> {
        let Self::Function(f) = self else { return None };
        f.prototype()
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(n) => write!(f, "{n}"),
            Self::Float(n) => write!(f, "{n}"),
            Self::Number(n) => write!(f, "{n}"),
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
            Self::OverloadSet {
                scalars,
                vec_candidates,
            } => write!(
                f,
                "<overload set ({} candidates)>",
                scalars.len() + vec_candidates.len()
            ),
            Self::Iterator(iter) => match iter.borrow().len() {
                Some(n) => write!(f, "<iterator (len={n})>"),
                None => write!(f, "<iterator>"),
            },
            Self::Deque(d) => write!(f, "Deque(len={})", d.borrow().len()),
            Self::MinHeap(h) => write!(f, "MinHeap(len={})", h.borrow().len()),
            Self::MaxHeap(h) => write!(f, "MaxHeap(len={})", h.borrow().len()),
            Self::Struct { info, fields } => {
                let length = info.fields.len();
                write!(f, "{} {{", info.name)?;
                for (i, ((field, _type), v)) in
                    info.fields.iter().zip(fields.borrow().iter()).enumerate()
                {
                    write!(f, "{field}: {v:?}")?;
                    if i + 1 < length {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
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
            Self::OverloadSet {
                scalars,
                vec_candidates,
            } => f
                .debug_struct("OverloadSet")
                .field("scalars", scalars)
                .field("vec_candidates", vec_candidates)
                .finish(),
            Self::Iterator(iter) => match iter.borrow().len() {
                Some(n) => write!(f, "<iterator (len={n})>"),
                None => write!(f, "<iterator>"),
            },
            Self::Deque(d) => write!(f, "Deque(len={})", d.borrow().len()),
            Self::MinHeap(h) => write!(f, "MinHeap(len={})", h.borrow().len()),
            Self::MaxHeap(h) => write!(f, "MaxHeap(len={})", h.borrow().len()),
            Self::Struct { info, fields } => {
                let values = fields.borrow();
                let mut s = f.debug_struct(&info.name);
                for ((name, _), value) in info.fields.iter().zip(values.iter()) {
                    s.field(name, value);
                }
                s.finish()
            }
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.is_number() && other.is_number() {
            return vm_value_to_number(self)?.partial_cmp(&vm_value_to_number(other)?);
        }

        match (self, other) {
            (Self::Bool(a), Self::Bool(b)) => a.partial_cmp(b),
            (Self::Object(a), Self::Object(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

/// Convert a VM numeric value to an `AdvancedNumber` for cross-type comparison.
/// Returns `None` for non-numeric values (Bool, None, String, List, …).
fn vm_value_to_number(v: &Value) -> Option<AdvancedNumber> {
    match v {
        Value::Int(i) => Some(AdvancedNumber::Int(Int::Int64(*i))),
        Value::Float(f) => Some(AdvancedNumber::Float(*f)),
        Value::Number(number) => Some(number.as_ref().clone()),
        _ => None,
    }
}

impl PartialOrd for Object {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::String(a), Self::String(b)) => a.borrow().partial_cmp(&*b.borrow()),
            (Self::List(a), Self::List(b)) => a.borrow().partial_cmp(&*b.borrow()),
            (Self::Tuple(a), Self::Tuple(b)) => a.partial_cmp(b),
            _ => None,
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
            Self::Number(number) => match number.as_ref() {
                AdvancedNumber::Int(i) => Ok(i.cmp(&Int::Int64(0))),
                AdvancedNumber::Rational(r) => Ok(r
                    .as_ref()
                    .cmp(&num::BigRational::from(num::BigInt::from(0)))),
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
        if self.is_number() && other.is_number() {
            return match (vm_value_to_number(self), vm_value_to_number(other)) {
                (Some(left), Some(right)) => left == right,
                _ => false,
            };
        }

        match (self, other) {
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
        if let Some(number) = vm_value_to_number(self) {
            state.write_u8(1);
            number.hash(state);
            return;
        }

        match self {
            Self::Bool(true) => state.write_u8(3),
            Self::Bool(false) => state.write_u8(4),
            Self::None => state.write_u8(5),
            Self::Object(o) => {
                state.write_u8(6);
                o.hash(state);
            }
            Self::Int(_) | Self::Float(_) | Self::Number(_) => unreachable!("handled above"),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Some(a), Self::Some(b)) => a == b,
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
            (
                Self::Struct {
                    info: a_info,
                    fields: a_fields,
                },
                Self::Struct {
                    info: b_info,
                    fields: b_fields,
                },
            ) => Rc::ptr_eq(a_info, b_info) && a_fields.borrow().eq(&*b_fields.borrow()),
            (Self::OverloadSet { .. }, Self::OverloadSet { .. }) => {
                panic!("OverloadSet cannot be used as a map key")
            }
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
                        state.write_u8(1);
                        Rc::as_ptr(func).hash(state);
                    }
                    Function::Native(native) => {
                        state.write_u8(2);
                        Rc::as_ptr(native).hash(state);
                    }
                    Function::Closure(closure) => {
                        state.write_u8(3);
                        Rc::as_ptr(&closure.prototype).hash(state);
                    }
                    Function::Memoized { cache, .. } => {
                        state.write_u8(4);
                        Rc::as_ptr(cache).hash(state);
                    }
                }
            }
            Self::OverloadSet { .. } => {
                panic!("OverloadSet cannot be used as a map key")
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
            Self::Struct { info, fields } => {
                state.write_u8(15);
                Rc::as_ptr(info).hash(state);
                for v in fields.borrow().iter() {
                    v.hash(state);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn conformance_memoizes_aliased_containers() {
        let list = Rc::new(Object::List(RefCell::new(Vec::new())));
        let value = Value::Object(Rc::clone(&list));
        let Object::List(elements) = list.as_ref() else {
            unreachable!();
        };
        elements
            .borrow_mut()
            .extend(std::iter::repeat_n(value.clone(), 100));

        let target = StaticType::List(Box::new(StaticType::List(Box::new(StaticType::List(
            Box::new(StaticType::Any),
        )))));
        let mut cache = ConformanceCache::default();

        assert!(value.conforms_to_cached(&target, &mut cache));
        assert_eq!(cache.len(), 4);

        // Break the reference cycle so the test does not leak its allocations.
        elements.borrow_mut().clear();
    }
}
