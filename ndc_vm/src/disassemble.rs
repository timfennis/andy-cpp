use crate::chunk::OpCode;
use crate::value::{CompiledFunction, Value};
use std::fmt::Write;
use std::rc::Rc;

pub fn disassemble(function: &CompiledFunction, source: Option<&str>) -> String {
    let mut out = String::new();
    disassemble_function(function, source, &mut out);
    out
}

fn disassemble_function(function: &CompiledFunction, source: Option<&str>, out: &mut String) {
    let name = function.name.as_deref().unwrap_or("<script>");
    writeln!(out, "== {name} ==").unwrap();

    let mut nested: Vec<Rc<CompiledFunction>> = Vec::new();

    for (i, op, constant) in function.body.iter() {
        let annotation = constant_annotation(op, constant, &mut nested);
        let excerpt = source
            .and_then(|src| src.get(function.body.span(i).range()))
            .map(|s| s.trim().replace('\n', "↵"));

        let op_str = format!("{op:?}");
        match (annotation, excerpt) {
            (Some(a), Some(e)) => writeln!(out, "{i:04}  {op_str:<35}  {a:<20}  {e}").unwrap(),
            (Some(a), None) => writeln!(out, "{i:04}  {op_str:<35}  {a}").unwrap(),
            (None, Some(e)) => writeln!(out, "{i:04}  {op_str:<35}  {e}").unwrap(),
            (None, None) => writeln!(out, "{i:04}  {op_str}").unwrap(),
        }
    }

    for func in nested {
        writeln!(out).unwrap();
        disassemble_function(&func, source, out);
    }
}

fn constant_annotation(
    op: &OpCode,
    constant: Option<&Value>,
    nested: &mut Vec<Rc<CompiledFunction>>,
) -> Option<String> {
    let val = constant?;
    if let Some(proto) = val.function_prototype() {
        let fn_name = proto.name.as_deref().unwrap_or("?");
        nested.push(Rc::clone(proto));
        return Some(format!("<fn {fn_name}>"));
    }
    match op {
        OpCode::Constant(_) | OpCode::Closure { .. } => Some(format!("{val}")),
        _ => None,
    }
}
