use crate::ast::{Expression, ExpressionLocation, ForBody, ForIteration, Lvalue, ResolvedVar};
use crate::interpreter::function::StaticType;
use crate::lexer::Span;
use itertools::Itertools;

pub struct Analyser {
    scope_tree: ScopeTree,
}

impl Analyser {
    pub fn from_scope_tree(scope_tree: ScopeTree) -> Self {
        Self { scope_tree }
    }
}

impl Analyser {
    pub fn analyse(
        &mut self,
        ExpressionLocation { expression, span }: &mut ExpressionLocation,
    ) -> Result<StaticType, AnalysisError> {
        match expression {
            Expression::BoolLiteral(_) => Ok(StaticType::Bool),
            Expression::StringLiteral(_) => Ok(StaticType::String),
            Expression::Int64Literal(_) | Expression::BigIntLiteral(_) => Ok(StaticType::Int),
            Expression::Float64Literal(_) => Ok(StaticType::Float),
            Expression::ComplexLiteral(_) => Ok(StaticType::Complex),
            Expression::Continue | Expression::Break => Ok(StaticType::unit()), // TODO: change to never type?
            Expression::Identifier {
                name: ident,
                resolved,
            } => {
                if ident == "None" {
                    // THIS IS VERY UNHINGED
                    return Ok(StaticType::Option);
                }
                let binding = self.scope_tree.get_binding_any(ident).ok_or_else(|| {
                    AnalysisError::identifier_not_previously_declared(ident, *span)
                })?;

                *resolved = Some(binding);

                Ok(self.scope_tree.get_type(binding)) // are we guaranteed to even have a type
            }
            Expression::Statement(inner) => {
                self.analyse(inner)?;
                Ok(StaticType::unit())
            }
            Expression::Logical { left, right, .. } => {
                self.analyse(left)?; // TODO: throw error if type does not match bool?
                self.analyse(right)?; // TODO: throw error if type does not match bool?
                Ok(StaticType::Bool)
            }
            Expression::Grouping(expr) => self.analyse(expr),
            Expression::VariableDeclaration { l_value, value } => {
                let typ = self.analyse(value)?;
                self.resolve_lvalue_declarative(l_value, typ)?;
                Ok(StaticType::unit()) // TODO: never type here?
            }
            Expression::Assignment { l_value, r_value } => {
                self.resolve_lvalue(l_value, *span)?;
                self.analyse(r_value)?;
                Ok(StaticType::unit())
            }
            Expression::OpAssignment {
                l_value,
                r_value,
                operation,
                resolved_assign_operation,
                resolved_operation,
            } => {
                self.resolve_lvalue(l_value, *span)?;
                self.analyse(r_value)?;

                // In this future should be able to use our type system to figure out which specific
                // instance of the function we need. When this is the case we can stop inserting both
                // versions in the OpAssignment expression and just figure out the correct one or error
                // if there is none
                let op_assign_ident = LexicalIdentifier::Function {
                    name: format!("{operation}="),
                    arity: Some(2),
                };

                if let Some(binding) = self.scope_tree.get_binding(&op_assign_ident) {
                    *resolved_assign_operation = Some(binding);
                }

                let operation_ident = LexicalIdentifier::Function {
                    name: (*operation).clone(),
                    arity: Some(2),
                };

                if let Some(binding) = self.scope_tree.get_binding(&operation_ident) {
                    *resolved_operation = Some(binding);

                    Ok(StaticType::unit())
                } else {
                    // For now, we require that the normal operation is present and the special assignment operation is optional
                    Err(AnalysisError::identifier_not_previously_declared(
                        operation, *span,
                    ))
                }
            }
            Expression::FunctionDeclaration {
                name,
                resolved_name,
                parameters,
                body,
                ..
            } => {
                if let Some(name) = name {
                    let function_ident = LexicalIdentifier::Function {
                        name: (*name).clone(),
                        arity: Some(extract_argument_arity(parameters)),
                    };

                    *resolved_name =
                        Some(self.scope_tree.get_or_create_local_binding(
                            function_ident.clone(),
                            StaticType::Function,
                        ))
                }

                self.scope_tree.new_scope();
                self.resolve_parameters_declarative(parameters);

                self.analyse(body)?;
                self.scope_tree.destroy_scope();

                Ok(StaticType::Function)
            }
            Expression::Block { statements } => {
                self.scope_tree.new_scope();
                let mut last = None;
                for s in statements {
                    last = Some(self.analyse(s)?);
                }
                self.scope_tree.destroy_scope();

                Ok(last.unwrap_or_else(StaticType::unit))
            }
            Expression::If {
                condition,
                on_true,
                on_false,
            } => {
                self.analyse(condition)?;
                let true_type = self.analyse(on_true)?;
                let false_typ = if let Some(on_false) = on_false {
                    Some(self.analyse(on_false)?)
                } else {
                    None
                };

                if false_typ.is_none() {
                    if true_type != StaticType::unit() {
                        // TODO: Emit warning for not using a semicolon in this if
                    }

                    Ok(StaticType::unit())
                } else if let Some(false_type) = false_typ
                    && false_type == true_type
                {
                    Ok(true_type)
                } else {
                    // TODO: maybe create a warning to show the user they're doing something cursed
                    // TODO: figure out the nearest common ancestor for true_type and false_type
                    Ok(StaticType::Any)
                }
            }
            Expression::While {
                expression,
                loop_body,
            } => {
                self.analyse(expression)?;
                self.analyse(loop_body)?;
                Ok(StaticType::unit())
            }
            Expression::For { iterations, body } => {
                self.resolve_for_iterations(iterations, body)?;

                match &**body {
                    // for now this is good enough?
                    ForBody::Block(_) => Ok(StaticType::unit()),
                    ForBody::List(_) => Ok(StaticType::List),
                    ForBody::Map { .. } => Ok(StaticType::Map),
                }
            }
            Expression::Call {
                function,
                arguments,
            } => {
                let _the_type_of_the_function_which_doesnt_help_us_now =
                    self.resolve_function_ident_arity(function, arguments.len(), *span)?;
                for a in arguments {
                    self.analyse(a)?;
                }
                // Maybe we can use _the_type_of_the_function_which_doesnt_help_us_now to find the return value?!?!
                // TODO: figure out the type
                Ok(StaticType::Any)
            }
            Expression::Index { index, value } => {
                self.analyse(index)?;
                self.analyse(value)?;
                // TODO: figure out the type here
                Ok(StaticType::Any)
            }
            Expression::Tuple { values } => {
                let mut types = Vec::with_capacity(values.len());
                for v in values {
                    types.push(self.analyse(v)?);
                }

                Ok(StaticType::Tuple(types))
            }
            Expression::List { values } => {
                for v in values {
                    self.analyse(v)?;
                }

                Ok(StaticType::List)
            }
            Expression::Map { values, default } => {
                for (key, value) in values {
                    self.analyse(key)?;
                    if let Some(value) = value {
                        self.analyse(value)?;
                    }
                }

                if let Some(default) = default {
                    self.analyse(default)?;
                }

                Ok(StaticType::Map)
            }
            Expression::Return { value } => {
                self.analyse(value)?;
                Ok(StaticType::unit()) // TODO or never?
            }
            Expression::RangeInclusive { start, end }
            | Expression::RangeExclusive { start, end } => {
                if let Some(start) = start {
                    self.analyse(start)?;
                }
                if let Some(end) = end {
                    self.analyse(end)?;
                }

                Ok(StaticType::Iterator)
            }
        }
    }
    fn resolve_function_ident_arity(
        &mut self,
        ident: &mut ExpressionLocation,
        arity: usize,
        span: Span,
    ) -> Result<StaticType, AnalysisError> {
        let ExpressionLocation {
            expression: Expression::Identifier { name, resolved },
            ..
        } = ident
        else {
            // It's possible that we're not trying to invoke an identifier `foo()` but instead we're
            // invoking a value like `get_function()()` so in this case we just continue like normal?
            return self.analyse(ident);
        };

        let binding = self
            .scope_tree
            .get_binding(&LexicalIdentifier::Function {
                name: name.clone(),
                arity: Some(arity),
            })
            .or_else(|| {
                self.scope_tree.get_binding(&LexicalIdentifier::Function {
                    name: name.clone(),
                    arity: None,
                })
            })
            // NOTE: for now if we can't find a function that matches we just bind to a variable with that name (if possible)
            //       this fixes some cases until we have full type checking
            .or_else(|| (self.scope_tree).get_binding_any(name))
            .ok_or_else(|| AnalysisError::identifier_not_previously_declared(name, span))?;

        *resolved = Some(binding);

        Ok(StaticType::Function)
    }
    fn resolve_for_iterations(
        &mut self,
        iterations: &mut [ForIteration],
        body: &mut ForBody,
    ) -> Result<(), AnalysisError> {
        let Some((iteration, tail)) = iterations.split_first_mut() else {
            unreachable!("because this function is never called with an empty slice");
        };

        let mut do_destroy = false;
        match iteration {
            ForIteration::Iteration { l_value, sequence } => {
                self.analyse(sequence)?;

                self.scope_tree.new_scope();

                // TODO: inferring this as any is a massive cop-out
                self.resolve_lvalue_declarative(l_value, StaticType::Any)?;
                do_destroy = true;
            }
            ForIteration::Guard(expr) => {
                self.analyse(expr)?;
            }
        }

        if !tail.is_empty() {
            self.resolve_for_iterations(tail, body)?
        } else {
            match body {
                ForBody::Block(block) => {
                    self.analyse(block)?;
                }
                ForBody::List(list) => {
                    self.analyse(list)?;
                }
                ForBody::Map {
                    key,
                    value,
                    default,
                } => {
                    self.analyse(key)?;
                    if let Some(value) = value {
                        self.analyse(value)?;
                    }

                    if let Some(default) = default {
                        self.analyse(default)?;
                    }
                }
            }
        }

        if do_destroy {
            self.scope_tree.destroy_scope();
        }

        Ok(())
    }

    fn resolve_lvalue(&mut self, lvalue: &mut Lvalue, span: Span) -> Result<(), AnalysisError> {
        match lvalue {
            Lvalue::Identifier {
                identifier,
                resolved,
            } => {
                let Some(target) = self.scope_tree.get_binding(&identifier.clone().into()) else {
                    return Err(AnalysisError::identifier_not_previously_declared(
                        identifier, span,
                    ));
                };

                *resolved = Some(target);
            }
            Lvalue::Index { index, value } => {
                self.analyse(index)?;
                self.analyse(value)?;
            }
            Lvalue::Sequence(seq) => {
                for sub_lvalue in seq {
                    self.resolve_lvalue(sub_lvalue, span)?
                }
            }
        }

        Ok(())
    }

    /// Resolve expressions as arguments to a function and return the function arity
    fn resolve_parameters_declarative(&mut self, arguments: &mut ExpressionLocation) {
        let ExpressionLocation {
            expression: Expression::Tuple { values },
            ..
        } = arguments
        else {
            panic!("expected arguments to be tuple");
        };

        for arg in values {
            let ExpressionLocation {
                expression: Expression::Identifier { name, resolved },
                ..
            } = arg
            else {
                panic!("expected tuple values to be ident");
            };

            *resolved = Some(self.scope_tree.create_local_binding(
                LexicalIdentifier::Variable {
                    name: (*name).clone(),
                },
                StaticType::Any,
            ));
        }
    }
    fn resolve_lvalue_declarative(
        &mut self,
        lvalue: &mut Lvalue,
        typ: StaticType,
    ) -> Result<(), AnalysisError> {
        match lvalue {
            Lvalue::Identifier {
                identifier,
                resolved,
            } => {
                *resolved = Some(self.scope_tree.create_local_binding(
                    LexicalIdentifier::Variable {
                        name: (*identifier).clone(),
                    },
                    typ,
                ));
            }
            Lvalue::Index { index, value } => {
                self.analyse(index)?;
                self.analyse(value)?;
            }
            Lvalue::Sequence(seq) => {
                for sub_lvalue in seq {
                    self.resolve_lvalue_declarative(sub_lvalue, typ.clone())?
                }
            }
        }

        Ok(())
    }

    // fn resolve_type(
    //     &mut self,
    //     ExpressionLocation { expression, .. }: &ExpressionLocation,
    // ) -> StaticType {
    //     match expression {
    //         Expression::BoolLiteral(_) | Expression::Logical { .. } => StaticType::Bool,
    //         Expression::StringLiteral(_) => StaticType::String,
    //         Expression::Int64Literal(_) | Expression::BigIntLiteral(_) => StaticType::Int,
    //         Expression::Float64Literal(_) => StaticType::Float,
    //         Expression::ComplexLiteral(_) => StaticType::Complex,
    //         Expression::Identifier { resolved, name } => {
    //             println!(
    //                 "resolving name: {name}, {resolved:?}\n\n{}\n\n{:?}",
    //                 self.scope_tree.current_scope_idx, self.scope_tree.scopes
    //             );
    //             self
    //             .scope_tree
    //             .get_type(resolved.unwrap_or_else(|| {
    //             panic!(
    //                 "previously mentioned identifier {name} was not resolved during type resolution"
    //             )
    //         }))
    //         }
    //         Expression::Statement(_)
    //         | Expression::While { .. }
    //         | Expression::Break
    //         | Expression::Assignment { .. } => StaticType::unit(),
    //         Expression::Grouping(expr) | Expression::Return { value: expr } => {
    //             self.resolve_type(expr)
    //         }
    //         Expression::VariableDeclaration { .. } => {
    //             debug_assert!(
    //                 false,
    //                 "trying to get type of variable declaration, does this make sense?"
    //             );
    //             StaticType::unit() // specifically unit tuple
    //         }
    //         Expression::OpAssignment { .. } => {
    //             debug_assert!(
    //                 false,
    //                 "trying to get type of op assignment, does this make sense?"
    //             );
    //             StaticType::unit() // specifically unit tuple
    //         }
    //         Expression::FunctionDeclaration { .. } => StaticType::Function,
    //         Expression::Block { statements } => statements
    //             .iter()
    //             .last()
    //             .map_or(StaticType::unit(), |last| self.resolve_type(last)),
    //         Expression::If {
    //             on_true, on_false, ..
    //         } => {
    //             let on_false_type = on_false
    //                 .as_ref()
    //                 .map_or(StaticType::unit(), |expr| self.resolve_type(expr));
    //
    //             assert_eq!(
    //                 self.resolve_type(on_true),
    //                 on_false_type,
    //                 "if branches have different types"
    //             );
    //             on_false_type
    //         }
    //         Expression::For { body, .. } => ,
    //         Expression::Call {
    //             function: _,
    //             arguments: _,
    //         } => {
    //             // TODO: Okay this is actually hard
    //             StaticType::Any
    //         }
    //         Expression::Index { .. } => {
    //             // TODO: this is also hard since we don't have generics
    //             StaticType::Any
    //         }
    //         Expression::Tuple { .. } => {
    //             // TODO: this is hard
    //             StaticType::Any
    //         }
    //         Expression::List { .. } => StaticType::List,
    //         Expression::Map { .. } => StaticType::Map,
    //         Expression::Continue => StaticType::Any, // Maybe we need a Never type?
    //         Expression::RangeInclusive { .. } | Expression::RangeExclusive { .. } => {
    //             StaticType::Iterator
    //         }
    //     }
    // }
}
fn extract_argument_arity(arguments: &ExpressionLocation) -> usize {
    let ExpressionLocation {
        expression: Expression::Tuple { values },
        ..
    } = arguments
    else {
        panic!("expected arguments to be tuple");
    };

    values.len()
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum LexicalIdentifier {
    Variable { name: String },
    Function { name: String, arity: Option<usize> },
}

impl LexicalIdentifier {
    pub fn name(&self) -> &str {
        match self {
            Self::Variable { name } | Self::Function { name, .. } => name,
        }
    }
}

impl From<&str> for LexicalIdentifier {
    fn from(value: &str) -> Self {
        Self::Variable { name: value.into() }
    }
}
impl From<String> for LexicalIdentifier {
    fn from(value: String) -> Self {
        Self::Variable { name: value }
    }
}

#[derive(Debug)]
pub struct ScopeTree {
    current_scope_idx: usize,
    global_scope: Scope,
    scopes: Vec<Scope>,
}

impl ScopeTree {
    pub fn from_global_scope(global_scope_map: Vec<LexicalIdentifier>) -> Self {
        Self {
            current_scope_idx: 0,
            global_scope: Scope {
                parent_idx: None,
                //TODO: maybe the line below is more of temporary solution
                identifiers: global_scope_map
                    .into_iter()
                    .map(|x| (x, StaticType::Function))
                    .collect_vec(),
            },
            scopes: vec![Scope::new(None)],
        }
    }

    fn get_type(&self, res: ResolvedVar) -> StaticType {
        // dbg!(self, res);
        match res {
            ResolvedVar::Captured { slot, depth } => {
                let mut scope_idx = self.current_scope_idx;
                let mut depth = depth;
                while depth > 0 {
                    depth -= 1;
                    scope_idx = self.scopes[scope_idx]
                        .parent_idx
                        .expect("parent_idx was None while traversing the scope tree");
                }
                self.scopes[scope_idx].identifiers[slot].1.clone()
            }
            // for now all globals are functions
            ResolvedVar::Global { .. } => StaticType::Function,
        }
    }

    fn new_scope(&mut self) -> &Scope {
        let old_scope_idx = self.current_scope_idx;
        self.current_scope_idx = self.scopes.len();
        let new_scope = Scope::new(Some(old_scope_idx));
        self.scopes.push(new_scope);
        &self.scopes[self.current_scope_idx]
    }

    fn destroy_scope(&mut self) {
        let next = self.scopes[self.current_scope_idx]
            .parent_idx
            .expect("tried to destroy scope while there were none");
        self.current_scope_idx = next;
    }

    fn get_or_create_local_binding(
        &mut self,
        ident: LexicalIdentifier,
        typ: StaticType,
    ) -> ResolvedVar {
        ResolvedVar::Captured {
            slot: self.scopes[self.current_scope_idx]
                .get_slot(&ident)
                .unwrap_or_else(|| self.scopes[self.current_scope_idx].allocate(ident, typ)),
            depth: 0,
        }
    }

    fn get_binding_any(&mut self, ident: &str) -> Option<ResolvedVar> {
        let mut depth = 0;
        let mut scope_ptr = self.current_scope_idx;

        loop {
            if let Some(slot) = self.scopes[scope_ptr].get_slot_by_name(ident) {
                return Some(ResolvedVar::Captured { slot, depth });
            } else if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                depth += 1;
                scope_ptr = parent_idx;
            } else {
                return Some(ResolvedVar::Global {
                    slot: self.global_scope.get_slot_by_name(ident)?,
                });
            }
        }
    }

    fn get_binding(&mut self, name: &LexicalIdentifier) -> Option<ResolvedVar> {
        let mut depth = 0;
        let mut scope_ptr = self.current_scope_idx;

        loop {
            if let Some(slot) = self.scopes[scope_ptr].get_slot(name) {
                return Some(ResolvedVar::Captured { slot, depth });
            } else if let Some(parent_idx) = self.scopes[scope_ptr].parent_idx {
                depth += 1;
                scope_ptr = parent_idx;
            } else {
                return Some(ResolvedVar::Global {
                    slot: self.global_scope.get_slot(name)?,
                });
            }
        }
    }

    fn create_local_binding(&mut self, ident: LexicalIdentifier, typ: StaticType) -> ResolvedVar {
        ResolvedVar::Captured {
            slot: self.scopes[self.current_scope_idx].allocate(ident, typ),
            depth: 0,
        }
    }
}

#[derive(Debug)]
struct Scope {
    parent_idx: Option<usize>,
    identifiers: Vec<(LexicalIdentifier, StaticType)>,
}

impl Scope {
    fn new(parent_idx: Option<usize>) -> Self {
        Self {
            parent_idx,
            identifiers: Default::default(),
        }
    }

    pub fn get_slot_by_name(&self, find_ident: &str) -> Option<usize> {
        self.identifiers
            .iter()
            .rposition(|(ident, _)| ident.name() == find_ident)
    }

    fn get_slot(&mut self, find_ident: &LexicalIdentifier) -> Option<usize> {
        self.identifiers
            .iter()
            .rposition(|(ident, _type)| ident == find_ident)
    }

    fn allocate(&mut self, name: LexicalIdentifier, typ: StaticType) -> usize {
        self.identifiers.push((name, typ));
        // Slot is just the length of the list minus one
        self.identifiers.len() - 1
    }
}
#[derive(thiserror::Error, miette::Diagnostic, Debug)]
#[error("{text}")]
pub struct AnalysisError {
    text: String,
    #[label("related to this")]
    span: Span,
}

impl AnalysisError {
    fn identifier_not_previously_declared(ident: &str, span: Span) -> Self {
        Self {
            text: format!("Identifier {ident} has not previously been declared"),
            span,
        }
    }
}
