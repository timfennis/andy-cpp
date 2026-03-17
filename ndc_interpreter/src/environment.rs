use crate::function::{Function, StaticType};
use crate::hash_map::HashMap;
use crate::value::Value;
use ndc_parser::{CaptureSource, ResolvedVar};
use std::cell::RefCell;
use std::fmt;
use std::fmt::Formatter;
use std::io::{Stdout, Write, stdout};
use std::rc::Rc;

pub struct RootEnvironment {
    pub output: Box<dyn InterpreterOutput>,
    // These are global values
    global_functions: Vec<Function>,
}

pub struct Environment {
    root: Rc<RefCell<RootEnvironment>>,
    parent: Option<Rc<RefCell<Self>>>,
    values: Vec<Value>,
    upvalues: Vec<Rc<RefCell<Value>>>,
    /// Locals that have been boxed into shared cells because a closure captured them.
    /// Keyed by `local_idx = slot - base_offset`. See `get_or_create_local_cell`.
    local_cells: HashMap<usize, Rc<RefCell<Value>>>,
    base_offset: usize,
}

impl fmt::Debug for Environment {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Environment[has_parent: {:?}, values.len(): {}]",
            self.parent.is_some(),
            self.values.len(),
        )
    }
}

impl Environment {
    pub fn with_output<F>(&mut self, f: F) -> Result<(), std::io::Error>
    where
        F: FnOnce(&mut Box<dyn InterpreterOutput>) -> Result<(), std::io::Error>,
    {
        let mut root = self.root.borrow_mut();
        let output = &mut root.output;
        f(output)
    }

    #[must_use]
    pub fn get_output(&self) -> Option<Vec<u8>> {
        let root = self.root.clone();
        let root = root.borrow();
        let output = root.output.get_output();
        output.cloned()
    }

    #[must_use]
    pub fn new(writer: Box<dyn InterpreterOutput>) -> Self {
        let root = RootEnvironment {
            output: writer,
            global_functions: Default::default(),
        };

        Self {
            root: Rc::new(RefCell::new(root)),
            parent: None,
            values: Default::default(),
            upvalues: Default::default(),
            local_cells: Default::default(),
            base_offset: 0,
        }
    }

    pub fn get_global_identifiers(&self) -> Vec<(String, StaticType)> {
        self.root
            .borrow()
            .global_functions
            .iter()
            .map(|function| (function.name().to_string(), function.static_type()))
            .collect::<Vec<(String, StaticType)>>()
    }

    #[must_use]
    pub fn get_all_functions(&self) -> Vec<Function> {
        self.root.borrow().global_functions.clone()
    }

    pub fn set(&mut self, var: ResolvedVar, value: Value) {
        match var {
            ResolvedVar::Local { slot } if slot >= self.base_offset => {
                let local_idx = slot - self.base_offset;
                if let Some(cell) = self.local_cells.get(&local_idx) {
                    *cell.borrow_mut() = value;
                } else if local_idx < self.values.len() {
                    self.values[local_idx] = value;
                } else {
                    debug_assert!(local_idx == self.values.len());
                    self.values.push(value);
                }
            }
            ResolvedVar::Local { .. } => {
                self.parent
                    .clone()
                    .expect("Local slot below base_offset but no parent")
                    .borrow_mut()
                    .set(var, value);
            }
            ResolvedVar::Upvalue { slot } => {
                if let Some(cell) = self.upvalues.get(slot) {
                    *cell.borrow_mut() = value;
                } else {
                    self.parent
                        .clone()
                        .expect("Upvalue slot not in current scope and no parent")
                        .borrow_mut()
                        .set(var, value);
                }
            }
            ResolvedVar::Global { .. } => {
                unreachable!("cannot assign value to global")
            }
        }
    }

    /// Declare a function globally using its self-exposed name, if there already exists a function
    /// with the same name it's simply overloaded.
    pub fn declare_global_fn(&mut self, function: impl Into<Function>) {
        let new_function = function.into();

        let root: &mut RootEnvironment = &mut self.root.borrow_mut();

        root.global_functions.push(new_function.clone());
    }


    #[must_use]
    pub fn get(&self, var: ResolvedVar) -> Value {
        match var {
            ResolvedVar::Local { slot } if slot >= self.base_offset => {
                let local_idx = slot - self.base_offset;
                if let Some(cell) = self.local_cells.get(&local_idx) {
                    return cell.borrow().clone();
                }
                self.values[local_idx].clone()
            }
            ResolvedVar::Local { .. } => self
                .parent
                .as_ref()
                .expect("Local slot below base_offset but no parent")
                .borrow()
                .get(var),
            ResolvedVar::Upvalue { slot } => {
                if let Some(cell) = self.upvalues.get(slot) {
                    cell.borrow().clone()
                } else {
                    self.parent
                        .as_ref()
                        .expect("Upvalue slot not in current scope and no parent")
                        .borrow()
                        .get(var)
                }
            }
            ResolvedVar::Global { slot } => {
                Value::function(self.root.borrow().global_functions[slot].clone())
            }
        }
    }

    /// Takes the named variable from memory and leaves `Value::unit()` in its place
    #[must_use]
    pub fn take(&mut self, var: ResolvedVar) -> Option<Value> {
        match var {
            ResolvedVar::Local { slot } if slot >= self.base_offset => {
                let local_idx = slot - self.base_offset;
                if let Some(cell) = self.local_cells.get(&local_idx) {
                    return Some(std::mem::replace(&mut *cell.borrow_mut(), Value::unit()));
                }
                Some(std::mem::replace(
                    self.values.get_mut(local_idx).expect("slot can't be empty"),
                    Value::unit(),
                ))
            }
            ResolvedVar::Local { .. } => self
                .parent
                .clone()
                .expect("Local slot below base_offset but no parent")
                .borrow_mut()
                .take(var),
            ResolvedVar::Upvalue { slot } => {
                if let Some(cell) = self.upvalues.get(slot) {
                    Some(std::mem::replace(&mut *cell.borrow_mut(), Value::unit()))
                } else {
                    self.parent
                        .clone()
                        .expect("Upvalue slot not in current scope and no parent")
                        .borrow_mut()
                        .take(var)
                }
            }
            ResolvedVar::Global { .. } => panic!("cannot take global variable from environment"),
        }
    }

    /// Creates a new environment for a closure call.
    ///
    /// `upvalue_cells` are the pre-resolved shared cells built by `resolve_captures` at the
    /// time the closure was defined. They are indexed by upvalue slot and map directly to
    /// `ResolvedVar::Upvalue { slot }` accesses inside the closure body.
    ///
    /// Using pre-resolved cells (rather than resolving captures on every call) is what makes
    /// upvalue mutation persistent across calls: all invocations of the same closure share the
    /// same `Rc<RefCell<Value>>` instances, so a write in one call is visible to the next.
    pub fn new_function_scope_with_cells(
        parent: &Rc<RefCell<Self>>,
        upvalue_cells: &[Rc<RefCell<Value>>],
    ) -> Self {
        let root_ref = Rc::clone(&parent.borrow().root);
        Self {
            parent: Some(parent.clone()),
            root: root_ref,
            values: Default::default(),
            upvalues: upvalue_cells.to_vec(),
            local_cells: Default::default(),
            base_offset: 0,
        }
    }

    /// Creates a new environment for a single iteration of a for-loop.
    ///
    /// Unlike function scopes, iteration scopes do not reset `base_offset` to zero — they
    /// continue the flat slot numbering of the enclosing scope. This means that the loop
    /// variable (`i` in `for i in xs`) lives at a slot directly above the parent's locals, and
    /// variables declared outside the loop are accessed as `ResolvedVar::Local` through the
    /// parent chain rather than as upvalues.
    ///
    /// Each iteration of the loop creates a fresh environment (a new call to this function),
    /// so closures defined inside the loop body capture the loop variable's value for that
    /// specific iteration.
    pub fn new_iteration_scope(parent: &Rc<RefCell<Self>>, base_offset: usize) -> Self {
        let root_ref = Rc::clone(&parent.borrow().root);
        Self {
            parent: Some(parent.clone()),
            root: root_ref,
            values: Default::default(),
            upvalues: Default::default(),
            local_cells: Default::default(),
            base_offset,
        }
    }

    /// Resolves a closure's capture list into shared `Rc<RefCell<Value>>` cells.
    ///
    /// This is called exactly once, when the `FunctionDeclaration` expression is evaluated
    /// (i.e. when the closure value is created), not on every call. The resulting cells are
    /// stored in `FunctionBody::Closure::upvalue_cells` and reused for the lifetime of the
    /// closure.
    ///
    /// Each `CaptureSource` describes where the value comes from in the parent environment:
    /// - `Local(slot)` — the variable lives directly in the parent's locals. We call
    ///   `get_or_create_local_cell` to box it: both the parent and the closure then share the
    ///   same cell, so mutations made by either party are immediately visible to the other.
    /// - `Upvalue(idx)` — the variable was already captured by the parent closure. We just
    ///   clone the `Rc`, sharing the existing cell without creating a new layer of indirection.
    pub fn resolve_captures(
        parent: &Rc<RefCell<Self>>,
        captures: &[CaptureSource],
    ) -> Vec<Rc<RefCell<Value>>> {
        let mut parent_env = parent.borrow_mut();
        captures
            .iter()
            .map(|cap| match cap {
                CaptureSource::Local(slot) => parent_env.get_or_create_local_cell(*slot),
                CaptureSource::Upvalue(idx) => Rc::clone(&parent_env.upvalues[*idx]),
            })
            .collect()
    }

    /// Returns the shared cell for a local variable slot, creating one if it doesn't exist yet.
    ///
    /// "Boxing" a local means replacing the plain `values[local_idx]` entry with an
    /// `Rc<RefCell<Value>>` that both this environment and any capturing closure share.
    /// After boxing, `get`/`set`/`take` on that slot always go through the cell so that
    /// writes from inside a closure are visible in the original scope and vice versa.
    ///
    /// If the slot hasn't been written yet (e.g. a recursive function capturing its own name
    /// before the `set` that stores it), the cell is pre-populated with `Value::unit()` as a
    /// placeholder. The subsequent `set` call will write the real value through the cell.
    ///
    /// If the slot falls below `base_offset`, the variable lives in a parent environment
    /// (e.g. the outer iteration of a nested for-loop), so we recurse up the parent chain.
    fn get_or_create_local_cell(&mut self, slot: usize) -> Rc<RefCell<Value>> {
        if slot >= self.base_offset {
            let local_idx = slot - self.base_offset;
            if let Some(cell) = self.local_cells.get(&local_idx) {
                return Rc::clone(cell);
            }
            while self.values.len() <= local_idx {
                self.values.push(Value::unit());
            }
            let cell = Rc::new(RefCell::new(self.values[local_idx].clone()));
            self.local_cells.insert(local_idx, Rc::clone(&cell));
            cell
        } else {
            self.parent
                .as_ref()
                .expect("Local slot below base_offset but no parent")
                .clone()
                .borrow_mut()
                .get_or_create_local_cell(slot)
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new(Box::new(stdout()))
    }
}

pub trait InterpreterOutput: Write {
    fn get_output(&self) -> Option<&Vec<u8>>;
}

impl InterpreterOutput for Vec<u8> {
    fn get_output(&self) -> Option<&Vec<u8>> {
        Some(self)
    }
}

impl InterpreterOutput for Stdout {
    fn get_output(&self) -> Option<&Vec<u8>> {
        None
    }
}
