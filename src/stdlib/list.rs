#[andy_cpp_macros::export_module]
mod inner {
    use crate::interpreter::value::Value;
    pub fn contains(list: &[Value], elem: &Value) -> bool {
        list.contains(elem)
    }
}
