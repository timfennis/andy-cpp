mod environment;
mod evaluate;

use crate::ast::{Expression, Literal, Operator, Statement};
use crate::interpreter::environment::Environment;
pub use evaluate::EvaluationError;
use std::ops::Neg;

#[derive(Default)]
pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn interpret(
        &mut self,
        statements: impl Iterator<Item = Statement>,
    ) -> Result<Literal, EvaluationError> {
        // TODO: The interpreter defaults to returning unit if there are no statements, this makes no sense.
        let mut value = Literal::Unit;

        for statement in statements {
            value = self.evaluate(statement)?;
        }

        Ok(value)
    }

    fn evaluate(&mut self, statement: Statement) -> Result<Literal, EvaluationError> {
        match statement {
            Statement::Print(expr) => {
                println!("{}", self.evaluate_expression(expr)?);
                Ok(Literal::Unit)
            }
            Statement::Expression(expr) => Ok(self.evaluate_expression(expr)?),
            Statement::VariableDeclaration { identifier, expr } => {
                let value = self.evaluate_expression(expr)?;
                // TODO: declarations evaluating to their RHS means we have to clone here, is that worth it?
                self.environment.declare(&identifier, value.clone());
                Ok(value)
            }
        }
    }
    fn evaluate_expression(&self, expr: Expression) -> Result<Literal, EvaluationError> {
        let literal = match expr {
            Expression::Literal(l) => l.clone(),
            Expression::Unary {
                expression,
                operator_token,
            } => {
                let value = self.evaluate_expression(*expression)?;
                let operator: Operator = (&operator_token).try_into()?;
                match (value, operator) {
                    (Literal::Integer(n), Operator::Minus) => Literal::Integer(n.neg()),
                    (Literal::True, Operator::Bang) => Literal::False,
                    (Literal::False, Operator::Bang) => Literal::True,
                    (_, Operator::Bang) => {
                        return Err(EvaluationError::TypeError {
                            message: "the '!' operator cannot be applied to this type".to_string(),
                        });
                    }
                    (_, Operator::Minus) => {
                        return Err(EvaluationError::TypeError {
                            message: "this type cannot be negated".to_string(),
                        });
                    }
                    _ => panic!("invalid unary operator encountered"),
                }
            }
            Expression::Binary {
                left,
                operator_token,
                right,
            } => {
                let left = self.evaluate_expression(*left)?;
                let right = self.evaluate_expression(*right)?;
                evaluate::apply_operator(left, &operator_token, right)?
            }
            Expression::Grouping(expr) => self.evaluate_expression(*expr)?,
            // TODO: big FIXME, figure out if we can somehow return a reference instead of having to clone here
            //       does returning a reference make sense though since we're interested in the result at this point?
            Expression::Variable(name) => self
                .environment
                .get(&name)
                .expect("TODO: handle error, undeclared variable")
                .clone(),
        };

        Ok(literal)
    }
}
