use ndc_macros::export_module;
use ndc_vm::value::{Object, Value};

#[export_module]
mod inner {
    use ndc_vm::hash_map::HashMap;
    use std::rc::Rc;

    /// Counts the occurrences of each item in a sequence and returns a map with the frequencies.
    #[function(signature = "<T>(Iterable<T>) -> %{T: Int}")]
    pub fn frequencies(seq: Value) -> anyhow::Result<Value> {
        let mut counts: HashMap<Value, i64> = HashMap::default();

        for item in seq
            .iter()
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
