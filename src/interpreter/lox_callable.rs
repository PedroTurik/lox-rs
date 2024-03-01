use std::io::Write;

use crate::ast::value::Value;

use super::ast_interpreter::{Interpreter, InterpreterError};

pub trait LoxCall {
    fn arity(&self) -> usize;
    fn call<W: Write>(
        &mut self,
        interpreter: &mut Interpreter<W>,
        arguments: Vec<Value>,
    ) -> Result<Value, InterpreterError>;
}
