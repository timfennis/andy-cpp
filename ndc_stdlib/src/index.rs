use ndc_interpreter::environment::Environment;
use ndc_interpreter::evaluate::index::{
    IndexError, get_at_index, set_at_index, value_to_evaluated_index,
};
use ndc_interpreter::function::{
    FunctionBody, FunctionBuilder, FunctionCarrier, StaticType, TypeSignature,
};
use ndc_interpreter::value::Value;
use std::cell::RefCell;
use std::rc::Rc;

pub fn register(env: &mut Environment) {
    env.declare_global_fn(
        FunctionBuilder::default()
            .name("[]".to_string())
            .body(FunctionBody::GenericFunction {
                type_signature: TypeSignature::Variadic,
                function: index_function,
                return_type: StaticType::Any,
            })
            .build()
            .expect("must succeed"),
    );
    env.declare_global_fn(
        FunctionBuilder::default()
            .name("[]=".to_string())
            .body(FunctionBody::GenericFunction {
                type_signature: TypeSignature::Variadic,
                function: set_index_function,
                return_type: StaticType::Tuple(vec![]),
            })
            .build()
            .expect("must succeed"),
    );
}

fn set_index_function(
    args: &mut [Value],
    _env: &Rc<RefCell<Environment>>,
) -> Result<Value, FunctionCarrier> {
    let [container, index_value, rhs] = args else {
        return Err(IndexError::new(format!(
            "[]= requires exactly 3 arguments, got {}",
            args.len()
        ))
        .into());
    };
    let evaluated_index = value_to_evaluated_index(index_value.clone());
    let rhs = rhs.clone();
    set_at_index(container, rhs, evaluated_index)?;
    Ok(Value::unit())
}

fn index_function(
    args: &mut [Value],
    env: &Rc<RefCell<Environment>>,
) -> Result<Value, FunctionCarrier> {
    let [container, index_value] = args else {
        return Err(IndexError::new(format!(
            "[] requires exactly 2 arguments, got {}",
            args.len()
        ))
        .into());
    };
    let evaluated_index = value_to_evaluated_index(index_value.clone());
    get_at_index(container, evaluated_index, env)
}
