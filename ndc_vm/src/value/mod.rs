mod function;
mod number;
mod numeric;
mod string;

pub use function::*;
pub use number::{AdvancedNumber, BinaryOperatorError, ExactFraction, NumberToFloatError};
pub use numeric::{NumericMode, NumericRef};
pub use string::VmString;

use crate::hash_map::{HashMap, hasher};
use crate::iterator::SharedIterator;
use ndc_core::StaticType;
use ndc_core::r#struct::StructInfo;
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
            Some(previous) => previous.join(&found),
            None => found,
        });
    }
    element_type.unwrap_or(StaticType::Any)
}

/// What [`Value::deep_copy`] has copied so far: each original container's
/// address, with its copy.
type DeepCopies = HashMap<*const Object, Rc<Object>>;

fn deep_copy_all<'a, C: FromIterator<Value>>(
    values: impl Iterator<Item = &'a Value>,
    copies: &mut DeepCopies,
) -> C {
    values.map(|value| value.deep_copy_with(copies)).collect()
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
    /// A captured local, shared by the frame that declares it and the
    /// closures that capture it. It lives in a stack slot or a closure's
    /// upvalues and is never a value a program can see.
    Cell(Rc<RefCell<Self>>),
}

#[derive(Clone)]
pub enum Object {
    Some(Value),
    String(Rc<VmString>),
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
    Iterator(SharedIterator),
    Deque(RefCell<VecDeque<Value>>),
    MinHeap(RefCell<BinaryHeap<Reverse<OrdValue>>>),
    MaxHeap(RefCell<BinaryHeap<OrdValue>>),
}

/// Newtype wrapper around `Value` that imposes a total order so values can be
/// stored in a `BinaryHeap`.
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
        Self::Object(Rc::new(Object::String(Rc::new(VmString::new(
            string.into(),
        )))))
    }

    pub fn from_string_rc(rc: Rc<VmString>) -> Self {
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

    /// `Some(value)`, or `None` when there's no value.
    pub fn option(value: Option<Self>) -> Self {
        value.map_or(Self::None, |value| {
            Self::Object(Rc::new(Object::Some(value)))
        })
    }

    pub fn int(i: i64) -> Self {
        Self::Int(i)
    }

    pub fn float(f: f64) -> Self {
        Self::Float(f)
    }

    pub fn bigint(i: num::BigInt) -> Self {
        Self::number(AdvancedNumber::Int(i))
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
    /// Function, Iterator, Some) share the Rc.
    pub fn shallow_clone(&self) -> Self {
        match self {
            Self::Object(obj) => match obj.as_ref() {
                Object::String(rc) => Self::Object(Rc::new(Object::String(Rc::new(
                    VmString::new(rc.borrow().clone()),
                )))),
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
    ///
    /// Each container is copied once, so the copy has the same shape as the
    /// original: `[a, a]` copies to `[b, b]` with one new `b`, and a list
    /// that holds itself copies to a new list that holds itself.
    pub fn deep_copy(&self) -> Self {
        self.deep_copy_with(&mut HashMap::default())
    }

    /// [`Self::deep_copy`], where `copies` maps each container copied so far
    /// to its copy.
    fn deep_copy_with(&self, copies: &mut DeepCopies) -> Self {
        match self {
            Self::Object(obj) => Self::Object(Object::deep_copy(obj, copies)),
            other => other.clone(),
        }
    }

    /// Returns the static type of this value.
    ///
    /// # Performance
    ///
    /// **O(n) for `List`, `Map`, and `Deque`** — these variants iterate all
    /// elements to join their types into the element type.  Avoid calling this in
    /// hot paths on container values.
    pub fn static_type(&self) -> StaticType {
        let mut budget = usize::MAX;
        self.static_type_with_budget(usize::MAX, &mut budget)
    }

    /// Returns a runtime type description whose recursion depth and total
    /// number of inspected values are bounded. Once either limit is reached,
    /// the remaining subtree is widened to `Any`.
    pub fn diagnostic_type(&self) -> StaticType {
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
            Self::Cell(cell) => cell.borrow().static_type_with_budget(depth, budget),
        }
    }

    /// Whether this value is a `target`, a type that holds no other values
    /// as a container does: a scalar, a struct, a function or an iterator.
    /// A function is one if its own type says so (see [`Function::fit`]):
    /// only [`Self::converted_to`] can make one that may be one a function
    /// whose calls are checked. What an iterator makes can't be checked
    /// without making it, so an iterator is only an `Iterator<Any>`.
    fn is_a(&self, target: &StaticType) -> bool {
        if let StaticType::Function { .. } = target
            && let Self::Object(object) = self
            && let Object::Function(function) = object.as_ref()
        {
            return function.fit(target) == FunctionFit::Proven;
        }
        self.static_type() == *target
    }

    /// This value as a `target`, if it is one, checking every element of a
    /// container: a function that may be a function `target` is wrapped so
    /// that each call checks what it returns.
    pub fn converted_to(&self, target: &StaticType) -> Option<Self> {
        if let StaticType::Function { .. } = target
            && let Self::Object(object) = self
            && let Object::Function(function) = object.as_ref()
        {
            return match function.fit(target) {
                FunctionFit::Proven => Some(self.clone()),
                FunctionFit::IfChecked => Some(Self::function(Function::Checked {
                    function: Rc::new(function.unchecked().clone()),
                    typ: Box::new(target.clone()),
                })),
                FunctionFit::No => None,
            };
        }
        self.conforms_to(target).then(|| self.clone())
    }

    /// Check whether this value conforms to `target`, scanning container
    /// elements recursively. Empty containers conform to any element type.
    ///
    /// It backs the runtime check of casts, where the caller explicitly
    /// opted into the scan. Shared container aliases are memoized by object and target
    /// type. Iterators can't be inspected without consuming them, so they only
    /// conform to targets with `Any` elements.
    pub fn conforms_to(&self, target: &StaticType) -> bool {
        self.conforms_to_cached(target, &mut ConformanceCache::default())
    }

    fn conforms_to_cached(&self, target: &StaticType, cache: &mut ConformanceCache) -> bool {
        // Only a container target looks inside the value, which may be shared
        // or cyclic; checking against another type takes no time to redo.
        let looks_inside = matches!(
            target,
            StaticType::Option(_)
                | StaticType::List(_)
                | StaticType::Deque(_)
                | StaticType::Tuple(_)
                | StaticType::Map { .. }
                | StaticType::MinHeap(_)
                | StaticType::MaxHeap(_)
                | StaticType::Iterable(_)
        );
        let cache_key = match self {
            Self::Object(object) if looks_inside => {
                Some((Rc::as_ptr(object), std::ptr::from_ref(target)))
            }
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
            target if target.is_unknown() => true,
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
            StaticType::Iterable(element) => match self {
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
                    Object::String(_) => StaticType::String.fits(element),
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
                                StaticType::Iterable(inner) => {
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
                _ => self.is_a(target),
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
    /// For `Map`, yields `(key, value)` tuples, and for a heap the elements
    /// in the order the heap gives them, smallest first for a `MinHeap` and
    /// largest first for a `MaxHeap`: the same behaviour as iterating them
    /// in a for-loop.
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
            Object::MinHeap(heap) => {
                // Sorted by `Reverse`, so the largest first.
                let mut values: Vec<Self> = heap
                    .into_inner()
                    .into_sorted_vec()
                    .into_iter()
                    .map(|Reverse(OrdValue(value))| value)
                    .collect();
                values.reverse();
                Some(ValueIter::List(values.into_iter()))
            }
            Object::MaxHeap(heap) => {
                let mut values: Vec<Self> = heap
                    .into_inner()
                    .into_sorted_vec()
                    .into_iter()
                    .map(|OrdValue(value)| value)
                    .collect();
                values.reverse();
                Some(ValueIter::List(values.into_iter()))
            }
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
        self.numeric_ref().and_then(NumericRef::as_number)
    }

    pub fn numeric_ref(&self) -> Option<NumericRef<'_>> {
        match self {
            Self::Int(value) => Some(NumericRef::Int(*value)),
            Self::Float(value) => Some(NumericRef::Float(*value)),
            Self::Number(value) => Some(NumericRef::Number(value.as_ref())),
            Self::Bool(_) | Self::None | Self::Object(_) | Self::Cell(_) => None,
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

    /// The copy of `object` in a [`Value::deep_copy`], made the first time
    /// it's reached. A mutable container is in `copies` before its contents
    /// are copied, so the contents can lead back to it.
    fn deep_copy(object: &Rc<Self>, copies: &mut DeepCopies) -> Rc<Self> {
        let key = Rc::as_ptr(object);
        if let Some(copy) = copies.get(&key) {
            return Rc::clone(copy);
        }
        let copy = match object.as_ref() {
            Self::List(values) => {
                let copy = Rc::new(Self::list(Vec::new()));
                copies.insert(key, Rc::clone(&copy));
                let values = deep_copy_all(values.borrow().iter(), copies);
                if let Self::List(target) = copy.as_ref() {
                    *target.borrow_mut() = values;
                }
                copy
            }
            Self::Deque(values) => {
                let copy = Rc::new(Self::Deque(RefCell::default()));
                copies.insert(key, Rc::clone(&copy));
                let values = deep_copy_all(values.borrow().iter(), copies);
                if let Self::Deque(target) = copy.as_ref() {
                    *target.borrow_mut() = values;
                }
                copy
            }
            Self::Struct { info, fields } => {
                let copy = Rc::new(Self::Struct {
                    info: Rc::clone(info),
                    fields: RefCell::default(),
                });
                copies.insert(key, Rc::clone(&copy));
                let values = deep_copy_all(fields.borrow().iter(), copies);
                if let Self::Struct { fields, .. } = copy.as_ref() {
                    *fields.borrow_mut() = values;
                }
                copy
            }
            // The default can't change, so it's copied before the map. If it
            // leads back to the map, the map was copied along the way.
            Self::Map { entries, default } => {
                let default = default.as_ref().map(|value| value.deep_copy_with(copies));
                if let Some(copy) = copies.get(&key) {
                    return Rc::clone(copy);
                }
                let copy = Rc::new(Self::map(HashMap::default(), default));
                copies.insert(key, Rc::clone(&copy));
                let pairs = entries
                    .borrow()
                    .iter()
                    .map(|(k, v)| (k.deep_copy_with(copies), v.deep_copy_with(copies)))
                    .collect();
                if let Self::Map { entries, .. } = copy.as_ref() {
                    *entries.borrow_mut() = pairs;
                }
                copy
            }
            // These can't hold themselves except through a mutable container,
            // which is in `copies` by the time it's reached again.
            Self::Some(value) => Rc::new(Self::Some(value.deep_copy_with(copies))),
            Self::Tuple(values) => Rc::new(Self::Tuple(deep_copy_all(values.iter(), copies))),
            Self::String(string) => Rc::new(Self::String(Rc::new(VmString::new(
                string.borrow().clone(),
            )))),
            Self::MinHeap(heap) => Rc::new(Self::MinHeap(RefCell::new(heap.borrow().clone()))),
            Self::MaxHeap(heap) => Rc::new(Self::MaxHeap(RefCell::new(heap.borrow().clone()))),
            // Iterator: deep_copy if supported, otherwise share the Rc.
            Self::Iterator(shared) => match shared.borrow().deep_copy() {
                Some(iterator) => Rc::new(Self::Iterator(iterator)),
                None => Rc::clone(object),
            },
            // A function is shared.
            Self::Function(_) => Rc::clone(object),
        };
        copies.insert(key, Rc::clone(&copy));
        copy
    }

    /// Returns the best static type descriptor for this runtime value.
    ///
    /// `[1, 2]` reports `[Int]` and `%{"a": 1}` reports `%{String: Int}`:
    /// lists and deques join their element types, the type they share or else
    /// `Any`, while maps do so separately for keys and values. Tuples preserve each position's
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
                        Some(previous) => previous.join(&found_key),
                        None => found_key,
                    });
                    value_type = Some(match value_type {
                        Some(previous) => previous.join(&found_value),
                        None => found_value,
                    });
                }
                StaticType::Map {
                    key: Box::new(key_type.unwrap_or(StaticType::Any)),
                    value: Box::new(value_type.unwrap_or(StaticType::Any)),
                }
            }
            Self::Function(f) => f.static_type(),
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
            Self::Float(n) => number::format_float(*n, f),
            Self::Number(n) => write!(f, "{n}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::None => write!(f, "None"),
            Self::Object(obj) => write!(f, "{obj}"),
            Self::Cell(cell) => write!(f, "{}", cell.borrow()),
        }
    }
}

thread_local! {
    /// The containers being displayed, outermost first, so that one that
    /// holds itself displays as `[...]` there instead of without end.
    static DISPLAYING: RefCell<Vec<*const Object>> = const { RefCell::new(Vec::new()) };
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let this = std::ptr::from_ref(self);
        if DISPLAYING.with_borrow(|displaying| displaying.contains(&this)) {
            return f.write_str(match self {
                Self::List(_) => "[...]",
                Self::Tuple(_) => "(...)",
                Self::Map { .. } => "%{...}",
                Self::Some(_) => "Some(...)",
                _ => "{...}",
            });
        }
        DISPLAYING.with_borrow_mut(|displaying| displaying.push(this));
        let result = display_contents(self, f);
        DISPLAYING.with_borrow_mut(|displaying| displaying.pop());
        result
    }
}

/// What [`Object`]'s `Display` shows of `object`, which isn't one it's
/// displaying already.
fn display_contents(object: &Object, f: &mut Formatter<'_>) -> fmt::Result {
    match object {
        Object::Some(v) => write!(f, "Some({v:?})"),
        // Strings display without quotes at the top level.
        Object::String(s) => write!(f, "{}", s.borrow()),
        Object::List(vs) => {
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
        // Empty tuple is the unit value, which prints as nothing on its
        // own; inside another value it shows as `()`.
        Object::Tuple(vs) if vs.is_empty() => Ok(()),
        Object::Tuple(vs) => {
            write!(f, "(")?;
            for (i, v) in vs.iter().enumerate() {
                write!(f, "{v:?}")?;
                if i + 1 < vs.len() {
                    write!(f, ",")?;
                }
            }
            // `(1,)` is a tuple, `(1)` is just 1.
            if vs.len() == 1 {
                write!(f, ",")?;
            }
            write!(f, ")")
        }
        Object::Map { entries, .. } => {
            write!(f, "%{{")?;
            let entries = entries.borrow();
            // A set is a map whose values are all `()`: `%{1,2}`.
            let is_set = entries.values().all(Value::is_unit);
            for (i, (k, v)) in entries.iter().enumerate() {
                if is_set {
                    write!(f, "{k:?}")?;
                } else {
                    write!(f, "{k:?}: {v:?}")?;
                }
                if i + 1 < entries.len() {
                    write!(f, ",")?;
                }
            }
            write!(f, "}}")
        }
        Object::Function(func) => write!(f, "{func}"),
        Object::Iterator(iter) => match iter.borrow().len() {
            Some(n) => write!(f, "<iterator (len={n})>"),
            None => write!(f, "<iterator>"),
        },
        Object::Deque(d) => write!(f, "Deque(len={})", d.borrow().len()),
        Object::MinHeap(h) => write!(f, "MinHeap(len={})", h.borrow().len()),
        Object::MaxHeap(h) => write!(f, "MaxHeap(len={})", h.borrow().len()),
        Object::Struct { info, fields } => {
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

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            // Strings in repr/debug context are quoted.
            Self::Object(obj) if matches!(obj.as_ref(), Object::String(_)) => {
                write!(f, "{obj:?}")
            }
            // Unit displays as nothing on its own, but as an element it has
            // to show: `[()]`, not `[]`.
            _ if self.is_unit() => write!(f, "()"),
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
        if let (Some(left), Some(right)) = (self.numeric_ref(), other.numeric_ref()) {
            return Some(left.compare(right));
        }

        match (self, other) {
            (Self::Bool(a), Self::Bool(b)) => a.partial_cmp(b),
            (Self::Object(a), Self::Object(b)) => a.partial_cmp(b),
            _ => None,
        }
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

impl Value {
    /// Like `partial_cmp`, but says why two values can't be compared.
    #[inline]
    pub fn try_cmp(&self, other: &Self) -> Result<Ordering, String> {
        self.partial_cmp(other).ok_or_else(|| {
            format!(
                "cannot compare {} and {}",
                self.diagnostic_type(),
                other.diagnostic_type()
            )
        })
    }

    /// Compare a comparator return value to zero, returning the ordering.
    /// Comparators return a number: negative means less, zero means equal, positive means greater.
    pub fn cmp_to_zero(&self) -> Result<Ordering, String> {
        match self {
            Self::Int(n) => Ok(n.cmp(&0)),
            Self::Float(f) => f
                .partial_cmp(&0.0)
                .ok_or_else(|| "NaN in comparator result".to_string()),
            Self::Number(number) => match number.as_ref() {
                AdvancedNumber::Int(i) => Ok(i.cmp(&num::BigInt::from(0))),
                AdvancedNumber::Float(f) => f
                    .partial_cmp(&0.0)
                    .ok_or_else(|| "NaN in comparator result".to_string()),
                AdvancedNumber::Rational(r) => Ok(r
                    .as_ref()
                    .cmp(&num::BigRational::from(num::BigInt::from(0)))),
                AdvancedNumber::Complex(_) => {
                    Err("comparator must return a real number, got a complex number".to_string())
                }
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
        if let (Some(left), Some(right)) = (self.numeric_ref(), other.numeric_ref()) {
            return left == right;
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
        if let Some(number) = self.numeric_ref() {
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
            Self::Cell(_) => unreachable!("internal error: a cell is never hashed"),
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
                match (a.unchecked(), b.unchecked()) {
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

/// A sequence hashes its length first, so that `[[[]]]` and `[[], []]` hash
/// apart.
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
                let vs = vs.borrow();
                state.write_usize(vs.len());
                for v in vs.iter() {
                    v.hash(state);
                }
            }
            Self::Tuple(vs) => {
                state.write_u8(7);
                state.write_usize(vs.len());
                for v in vs {
                    v.hash(state);
                }
            }
            Self::Map { entries, .. } => {
                state.write_u8(8);
                // Order-independent hash: XOR-fold of pair hashes
                // NOTE: the default value is not part of the identity of the map
                let entries = entries.borrow();
                state.write_usize(entries.len());
                let mut acc = 0u64;
                let mut cube_acc = 0u64;
                for (key, value) in entries.iter() {
                    let mut hasher = hasher();
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
                match f.unchecked() {
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
                    Function::Checked { .. } => unreachable!("`unchecked` strips the checks"),
                }
            }
            Self::Iterator(iter) => {
                state.write_u8(11);
                Rc::as_ptr(iter).hash(state);
            }
            Self::Deque(d) => {
                state.write_u8(12);
                let d = d.borrow();
                state.write_usize(d.len());
                for v in d.iter() {
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

    fn value_hash(value: &Value) -> u64 {
        let mut hasher = hasher();
        value.hash(&mut hasher);
        hasher.finish()
    }

    #[test]
    fn numeric_ref_reports_its_mode_and_converts_checked() {
        let integer = Value::Int(42);
        let float = Value::Float(42.75);
        let rational = Value::number(AdvancedNumber::rational(num::BigRational::new(
            7.into(),
            2.into(),
        )));
        let complex = Value::complex(num::Complex::new(1.0, 2.0));

        assert_eq!(integer.numeric_ref().unwrap().mode(), NumericMode::Int);
        assert_eq!(float.numeric_ref().unwrap().mode(), NumericMode::Float);
        assert_eq!(rational.numeric_ref().unwrap().mode(), NumericMode::Number);
        assert_eq!(float.numeric_ref().unwrap().to_i64_truncating(), Some(42));
        assert_eq!(rational.numeric_ref().unwrap().to_i64_truncating(), Some(3));
        assert_eq!(rational.numeric_ref().unwrap().to_f64(), Some(3.5));
        assert_eq!(complex.numeric_ref().unwrap().to_f64(), None);
        assert!(Value::Bool(true).numeric_ref().is_none());
    }

    #[test]
    fn equal_numbers_hash_alike_in_every_representation() {
        let values = [
            Value::Int(1),
            Value::Float(1.0),
            Value::number(AdvancedNumber::Int(1.into())),
            Value::number(AdvancedNumber::Float(1.0)),
            // the constructor collapses this to an integer rather than
            // leaving a whole fraction in the rational variant
            Value::number(AdvancedNumber::rational(num::BigRational::from_integer(
                1.into(),
            ))),
            Value::complex(num::Complex::new(1.0, 0.0)),
        ];

        for left in &values {
            for right in &values {
                assert_eq!(left, right);
                assert_eq!(left.partial_cmp(right), Some(Ordering::Equal));
                assert_eq!(value_hash(left), value_hash(right));
            }
        }
    }

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
        // One entry per list level; the `Any` inside needs no looking into.
        assert_eq!(cache.len(), 3);

        // Break the reference cycle so the test does not leak its allocations.
        elements.borrow_mut().clear();
    }

    #[test]
    fn numbers_of_one_representation_compare_by_value() {
        assert!(Value::int(-1) < Value::int(1));
        assert_eq!(Value::float(-0.0), Value::float(0.0));
        assert_eq!(Value::float(f64::NAN), Value::float(f64::NAN));
        assert!(Value::float(f64::NAN) > Value::float(f64::INFINITY));

        let one = Value::bigint(num::BigInt::from(1));
        let two = Value::bigint(num::BigInt::from(2));
        assert!(one < two);
    }

    #[test]
    fn int_float_comparison_is_exact() {
        let integers = [
            i64::MIN,
            i64::MIN + 1,
            -9_007_199_254_740_993,
            -1,
            0,
            1,
            9_007_199_254_740_993,
            i64::MAX,
        ];
        let floats = [
            f64::NEG_INFINITY,
            i64::MIN as f64,
            -9_007_199_254_740_992.0,
            -1.5,
            -0.0,
            0.5,
            1.0,
            9_007_199_254_740_992.0,
            i64::MAX as f64,
            f64::INFINITY,
            f64::NAN,
        ];

        for integer in integers {
            for float in floats {
                let fast_integer = Value::int(integer);
                let fast_float = Value::float(float);
                let canonical_integer = AdvancedNumber::Int(num::BigInt::from(integer));
                let canonical_float = AdvancedNumber::Float(float);

                assert_eq!(
                    fast_integer.partial_cmp(&fast_float),
                    canonical_integer.partial_cmp(&canonical_float),
                    "ordering differs for {integer} and {float}"
                );
                assert_eq!(
                    fast_integer == fast_float,
                    canonical_integer == canonical_float,
                    "equality differs for {integer} and {float}"
                );
            }
        }
    }
}
