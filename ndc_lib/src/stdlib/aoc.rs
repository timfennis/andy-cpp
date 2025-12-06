use ndc_macros::export_module;
#[export_module]
mod inner {
    use std::cell::RefCell;
    use std::rc::Rc;

    use crate::hash_map::HashMap;
    use crate::interpreter::iterator::mut_seq_to_iterator;
    use crate::interpreter::sequence::Sequence;
    use crate::interpreter::value::Value;

    /// Counts the occurrences of each item in a sequence and returns a map with the frequencies.
    #[function(return_type = DefaultMap<'_>)]
    pub fn frequencies(seq: &mut Sequence) -> Value {
        let mut out_map = HashMap::new();

        for item in mut_seq_to_iterator(seq) {
            *out_map.entry(item).or_insert(0i64) += 1;
        }

        let out_map = out_map
            .into_iter()
            .map(|(key, value)| (key, Value::from(value)))
            .collect::<HashMap<Value, Value>>();

        Value::Sequence(Sequence::Map(
            Rc::new(RefCell::new(out_map)),
            Some(Box::new(Value::from(0))),
        ))
    }
}
