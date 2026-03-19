use ndc_core::StaticType;
use ndc_macros::export_module;

#[export_module]
mod inner {
    use ndc_core::hash_map::HashMap;
    use ndc_vm::value::{Object, Value};
    use std::rc::Rc;

    /// Counts the occurrences of each item in a sequence and returns a map with the frequencies.
    pub fn frequencies(seq: ndc_vm::value::SeqValue) -> anyhow::Result<ndc_vm::value::Value> {
        let mut counts: HashMap<Value, i64> = HashMap::new();

        for item in seq
            .try_into_iter()
            .ok_or_else(|| anyhow::anyhow!("frequencies requires a sequence"))?
        {
            *counts.entry(item).or_insert(0i64) += 1;
        }

        let entries: HashMap<Value, Value> = counts
            .into_iter()
            .map(|(k, v)| (k, Value::Int(v)))
            .collect();

        Ok(Value::Object(Rc::new(Object::map(
            entries,
            Some(Value::Int(0)),
        ))))
    }
}
