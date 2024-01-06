mod environment;
mod evaluate;

use crate::ast::{Expression, Literal, Operator, Statement};
use crate::interpreter::environment::Environment;
use crate::lexer::{Lexer, Token};
use crate::{ast, InterpreterError};
pub use evaluate::EvaluationError;
use std::ops::Neg;

#[derive(Default)]
pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn run_str(&mut self, input: &str, debug: bool) -> Result<String, InterpreterError> {
        let scanner = Lexer::from_str(input);
        let tokens = scanner.collect::<Result<Vec<Token>, _>>()?;

        if debug {
            for token in &tokens {
                eprintln!("{token:?}");
            }
        }

        let mut parser = ast::Parser::from_tokens(tokens);
        let statements = parser.parse()?;

        let final_value = self.interpret(statements.into_iter())?;

        Ok(format!("{final_value}"))
    }
    pub fn interpret(
        &mut self,
        statements: impl Iterator<Item = Statement>,
    ) -> Result<Literal, EvaluationError> {
        // TODO: The interpreter defaults to returning unit if there are no statements, this makes no sense.
        let mut value = Literal::Unit;

        for statement in statements {
            value = self.evaluate_statement(statement)?;
        }

        Ok(value)
    }

    fn evaluate_statement(&mut self, statement: Statement) -> Result<Literal, EvaluationError> {
        match statement {
            Statement::Print(expr) => {
                println!("{}", self.evaluate_expression(expr)?);
                Ok(Literal::Unit)
            }
            Statement::Expression(expr) => Ok(self.evaluate_expression(expr)?),
            Statement::VariableDeclaration { identifier, expr } => {
                let value = self.evaluate_expression(expr)?;
                // TODO: declarations evaluating to their RHS means we have to clone here, is that worth it?
                self.environment.declare(&identifier.name, value.clone());
                Ok(value)
            }
            Statement::VariableAssignment { identifier, expr } => {
                let value = self.evaluate_expression(expr)?;

                if !self.environment.assign(&identifier.name, value.clone()) {
                    return Err(EvaluationError::UndefinedVariable { token: identifier });
                }

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
                match (value, operator_token.operator) {
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
                evaluate::apply_operator(left, operator_token, right)?
            }
            Expression::Grouping(expr) => self.evaluate_expression(*expr)?,
            Expression::Variable { token } => self
                .environment
                .get(&token.name)
                .ok_or(EvaluationError::UndefinedVariable { token })?
                // TODO: big FIXME, figure out if we can somehow return a reference instead of having to clone here
                //       does returning a reference make sense though since we're interested in the result at this point?
                .clone(),
        };

        Ok(literal)
    }
}
