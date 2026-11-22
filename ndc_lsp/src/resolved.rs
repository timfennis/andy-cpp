//! What each name in an analysed document refers to, worked out once per
//! analysis from the resolver's symbols and the symbols the analyser picked
//! ([`ndc_analyser::AnalysisResult::picks`]).

use ndc_analysis::Binding;
use ndc_core::NodeId;
use ndc_core::collections::HashMap;
use ndc_layout::Picks;
use ndc_resolver::{NameResolver, SymbolId};

/// Where the program declares something a name refers to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Definition {
    /// A binding pattern's identifier, a named function, a struct (its type
    /// and constructor) or a type parameter, by its node.
    Node(NodeId),
    /// A field of the struct declared at `declaration`, which declares its
    /// getter and setter: `index` is its position among the fields.
    Field { declaration: NodeId, index: usize },
}

/// What the names of one document refer to.
#[derive(Debug, Default)]
pub struct Resolved {
    definitions: HashMap<NodeId, Vec<Definition>>,
}

impl Resolved {
    /// Reads what the names in a document refer to off its `resolver` and
    /// the `picks` the analyser made: an identifier, a member access, an
    /// assignment target, a binding pattern or a name in a type annotation.
    pub fn new(resolver: &NameResolver, picks: &Picks) -> Self {
        let declared = declarations(resolver, picks);
        let mut definitions: HashMap<NodeId, Vec<Definition>> = HashMap::default();
        // Every name settled here gets an entry, empty for a built-in.
        let mut add = |node: NodeId, symbol: SymbolId| {
            let found = definitions.entry(node).or_default();
            if let Some(&definition) = declared.get(&symbol)
                && !found.contains(&definition)
            {
                found.push(definition);
            }
        };

        // In the body of a generic function, each instance picks its own.
        let own = [&picks.bindings, &picks.getters, &picks.setters];
        let instances = picks
            .instances
            .values()
            .flat_map(|instance| [&instance.bindings, &instance.getters, &instance.setters]);
        for table in own.into_iter().chain(instances) {
            for (&node, binding) in table {
                match binding {
                    Binding::None => {}
                    Binding::Resolved(candidate) => add(node, candidate.var()),
                    Binding::PerPosition(overloads) => {
                        for &symbol in overloads {
                            add(node, symbol);
                        }
                    }
                }
            }
        }
        // These mean one symbol, which the resolver settles on its own, even
        // in a statement the analyser skips for an error elsewhere in it.
        let settled = resolver
            .type_names()
            .chain(resolver.assignments())
            .chain(resolver.declarations());
        for (node, symbol) in settled {
            add(node, symbol);
        }

        // A name the analyser picked nothing for, as one in the body of a
        // generic function nothing calls yet, could mean what the nearest
        // scope that declares it declares: every overload there.
        for (node, candidates) in resolver.references() {
            if definitions.contains_key(&node) {
                continue;
            }
            let nearest = candidates
                .iter()
                .map(|group| {
                    group
                        .iter()
                        .filter_map(|symbol| declared.get(symbol).copied())
                        .collect::<Vec<_>>()
                })
                .find(|found| !found.is_empty())
                .unwrap_or_default();
            definitions.insert(node, nearest);
        }
        Self { definitions }
    }

    /// Where the name at `node` is declared: empty for a built-in, and
    /// `None` for a name the resolver didn't get to, as one after an unknown
    /// name in the same statement.
    pub fn definitions_of(&self, node: NodeId) -> Option<&[Definition]> {
        self.definitions.get(&node).map(Vec::as_slice)
    }
}

/// Where each symbol the program declares is declared. An instance of a
/// generic function is declared where the function is.
fn declarations(resolver: &NameResolver, picks: &Picks) -> HashMap<SymbolId, Definition> {
    let mut declared: HashMap<SymbolId, Definition> = resolver
        .declarations()
        .chain(resolver.type_parameters())
        .map(|(node, symbol)| (symbol, Definition::Node(node)))
        .collect();
    for (node, symbols) in resolver.structs() {
        declared.insert(symbols.type_symbol, Definition::Node(node));
        declared.insert(symbols.constructor, Definition::Node(node));
        for (index, &(getter, setter)) in symbols.accessors.iter().enumerate() {
            let field = Definition::Field {
                declaration: node,
                index,
            };
            declared.insert(getter, field);
            declared.insert(setter, field);
        }
    }
    for (&symbol, instance) in &picks.instances {
        declared.insert(symbol, Definition::Node(instance.declaration));
    }
    declared
}
