use std::io::Write;

use crate::ast::{stmt::Function, value::Value};

use super::{
    ast_interpreter::{Interpreter, InterpreterError},
    environment::Environment,
    lox_callable::LoxCall,
};

#[derive(Debug, Clone, PartialEq)]
pub struct LoxFn {
    pub declaration: Function,
    pub closure: Environment,
}

impl LoxFn {
    pub fn new(declaration: Function, closure: Environment) -> Self {
        Self {
            declaration,
            closure,
        }
    }
}

impl LoxCall for LoxFn {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn call<W: Write>(
        &mut self,
        interpreter: &mut Interpreter<W>,
        arguments: Vec<Value>,
    ) -> Result<Value, InterpreterError> {
        if self.arity() != arguments.len() {
            return Err(InterpreterError::runtime_error(
                "Incorrect number or arguments for function",
                self.declaration.name.clone(),
            ));
        }

        let mut environment = self.closure.extend();

        self.declaration
            .params
            .iter()
            .zip(arguments)
            .for_each(|(param, arg)| {
                environment.set(&param.lexeme, arg);
            });

        let ret = interpreter.execute_block(&self.declaration.body, environment);

        match ret {
            Err(InterpreterError::Return(v)) => Ok(v),
            Err(..) => Err(InterpreterError::runtime_error(
                "Error in function body",
                self.declaration.name.clone(),
            )),
            _ => Ok(Value::Nil),
        }
    }
}
