use crate::interpreter::lox_function::LoxFn;
use crate::lexer::Number;

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Number(Number),
    String(String),
    Function(LoxFn),
    Bool(bool),
    Nil,
}
