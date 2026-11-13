use ndc_core::FunctionRegistry;
use ndc_vm::NativeFunction;
use std::rc::Rc;

pub mod aoc;
pub mod cmp;
pub mod deque;
pub mod file;
pub mod hash_map;
pub mod heap;
pub mod index;
pub mod list;
pub mod math;
pub mod sequence;
pub mod string;
pub mod value;

#[cfg(feature = "crypto")]
pub mod crypto;
#[cfg(feature = "rand")]
pub mod rand;
#[cfg(feature = "regex")]
pub mod regex;
#[cfg(feature = "serde")]
pub mod serde;

pub fn register(env: &mut FunctionRegistry<Rc<NativeFunction>>) {
    aoc::register(env);
    cmp::register(env);
    #[cfg(feature = "crypto")]
    crypto::register(env);
    deque::register(env);
    file::register(env);
    file::register_variadic(env);
    hash_map::register(env);
    heap::register(env);
    index::register(env);
    list::ops::register(env);
    list::register(env);
    math::register(env);
    #[cfg(feature = "rand")]
    rand::register(env);
    #[cfg(feature = "regex")]
    regex::register(env);
    sequence::extra::register(env);
    sequence::register(env);
    #[cfg(feature = "serde")]
    serde::register(env);
    string::register(env);
    value::register(env);
}

#[cfg(test)]
mod tests {
    use super::*;
    use ndc_core::StaticType;

    /// A call that fits two overloads in one scope must find one of them
    /// more specific, so it never has to choose between generic overloads.
    #[test]
    fn no_two_overloads_tie() {
        let mut registry = FunctionRegistry::default();
        register(&mut registry);
        let natives = registry
            .iter()
            .map(|function| (function.name.clone(), function.static_type.clone()))
            .collect::<Vec<_>>();
        assert_eq!(ndc_analyser::tied_natives(&natives), Vec::<String>::new());
    }

    /// `Iterable<T>` takes any kind of sequence, which only a parameter can
    /// do, or what a function passed for one returns, as `flat_map`'s
    /// callback does. What a function returns is one kind of sequence.
    #[test]
    fn an_iterable_is_only_a_parameter() {
        let iterable =
            |typ: &StaticType| typ.contains(&|part| matches!(part, StaticType::Iterable(_)));
        let mut registry = FunctionRegistry::default();
        register(&mut registry);
        for function in registry.iter() {
            let StaticType::Function {
                parameters,
                return_type,
            } = &function.static_type
            else {
                continue;
            };
            assert!(
                !iterable(return_type),
                "{} returns an Iterable",
                function.name
            );
            for parameter in parameters.iter().flatten() {
                let nested = match parameter {
                    StaticType::Iterable(element) => iterable(element),
                    StaticType::Function {
                        parameters,
                        return_type,
                    } => {
                        parameters.iter().flatten().any(iterable)
                            || matches!(&**return_type, StaticType::Iterable(element) if iterable(element))
                            || (!matches!(&**return_type, StaticType::Iterable(_))
                                && iterable(return_type))
                    }
                    other => iterable(other),
                };
                assert!(
                    !nested,
                    "{} takes an Iterable inside another type",
                    function.name
                );
            }
        }
    }
}
