use std::io::Write;

use crate::{
    ast::{expr::Expr, stmt::Stmt, value::Value},
    lexer::{Token, TokenType},
};

use super::{
    environment::Environment, lox_callable::LoxCall, lox_function::LoxFn,
    runtime_error::RuntimeError,
};

pub struct Interpreter<W: Write> {
    environment: Environment,
    out_stream: W,
}

#[derive(Debug)]
pub enum InterpreterError {
    RuntimeError(RuntimeError),
    Return(Value),
}

impl InterpreterError {
    pub fn runtime_error(message: &str, token: Token) -> Self {
        Self::RuntimeError(RuntimeError::new(message.to_owned(), token))
    }

    pub fn lox_return(value: Value) -> Self {
        Self::Return(value)
    }
}

impl<W: Write> Interpreter<W> {
    pub fn new(out_stream: W) -> Self {
        let environment = Environment::new();
        Self {
            environment,
            out_stream,
        }
    }

    pub fn interpret(&mut self, program: Vec<Stmt>) -> Result<(), InterpreterError> {
        for stmt in program {
            self.execute(&stmt)?;
        }

        Ok(())
    }

    pub fn execute_block(
        &mut self,
        stmts: &[Stmt],
        environment: Environment,
    ) -> Result<(), InterpreterError> {
        let previous = self.environment.clone();

        self.environment = environment;

        for stmt in stmts {
            if let Err(x) = self.execute(stmt) {
                self.environment = previous;
                return Err(x);
            }
        }

        self.environment = previous;
        Ok(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), InterpreterError> {
        match stmt {
            Stmt::Expression(expr) => {
                self.evaluate(&expr.expr)?;
            }
            Stmt::Print(print) => {
                let value = Self::lox_stringify(self.evaluate(&print.expr)?);
                writeln!(self.out_stream, "{}", value).unwrap();
            }
            Stmt::Var(var) => {
                let value = match &var.initializer {
                    Some(expr) => self.evaluate(expr)?,
                    None => Value::Nil,
                };
                self.environment.set(&var.name.lexeme, value);
            }
            Stmt::Block(block) => {
                let environment = self.environment.extend();
                self.execute_block(&block.statements, environment)?;
            }
            Stmt::If(lox_if) => {
                let condition = self.evaluate(&lox_if.condition)?;
                if Self::is_truthy(&condition) {
                    self.execute(&lox_if.then_branch)?;
                } else if let Some(else_branch) = &lox_if.else_branch {
                    self.execute(else_branch)?;
                }
            }
            Stmt::While(lox_while) => {
                while Self::is_truthy(&self.evaluate(&lox_while.condition)?) {
                    self.execute(&lox_while.body)?;
                }
            }
            Stmt::Function(function) => {
                // let ident = function.name.lexeme.clone();
                let lox_function = LoxFn::new(function.clone(), self.environment.clone());
                self.environment
                    .set(&function.name.lexeme, Value::Function(lox_function));
            }
            Stmt::Return(lox_return) => {
                return Err(InterpreterError::lox_return(match &lox_return.value {
                    Some(expr) => self.evaluate(expr)?,
                    None => Value::Nil,
                }))
            }
        }

        Ok(())
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Value, InterpreterError> {
        match expr {
            Expr::Literal(literal) => Ok(literal.value.clone()),
            Expr::Grouping(grouping) => self.evaluate(&grouping.expr),
            Expr::Unary(unary) => {
                let right = self.evaluate(&unary.right)?;
                match unary.operator.token_type {
                    TokenType::Minus => match right {
                        Value::Number(n) => Ok(Value::Number(-n)),
                        _ => Err(InterpreterError::runtime_error(
                            "Unary operand must be a number",
                            unary.operator.clone(),
                        )),
                    },
                    TokenType::Bang => Ok(Value::Bool(!Self::is_truthy(&right))),
                    _ => unreachable!(),
                }
            }
            Expr::Binary(binary) => {
                let left = self.evaluate(&binary.left)?;
                let right = self.evaluate(&binary.right)?;

                match binary.operator.token_type {
                    TokenType::Minus => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
                        _ => Err(InterpreterError::runtime_error(
                            "Binary operands must be numbers",
                            binary.operator.clone(),
                        )),
                    },
                    TokenType::Slash => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),
                        _ => Err(InterpreterError::runtime_error(
                            "Binary operands must be numbers",
                            binary.operator.clone(),
                        )),
                    },
                    TokenType::Star => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
                        _ => Err(InterpreterError::runtime_error(
                            "Binary operands must be numbers",
                            binary.operator.clone(),
                        )),
                    },
                    TokenType::Plus => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                        (Value::String(l), Value::String(r)) => Ok(Value::String(l + &r)),
                        _ => Err(InterpreterError::runtime_error(
                            "Binary operands must be numbers or strings",
                            binary.operator.clone(),
                        )),
                    },
                    TokenType::Greater => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l > r)),
                        _ => Err(InterpreterError::runtime_error(
                            "Binary operands must be numbers",
                            binary.operator.clone(),
                        )),
                    },
                    TokenType::GreaterEqual => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l >= r)),
                        _ => Err(InterpreterError::runtime_error(
                            "Binary operands must be numbers",
                            binary.operator.clone(),
                        )),
                    },
                    TokenType::Less => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l < r)),
                        _ => Err(InterpreterError::runtime_error(
                            "Binary operands must be numbers",
                            binary.operator.clone(),
                        )),
                    },
                    TokenType::LessEqual => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l <= r)),
                        _ => Err(InterpreterError::runtime_error(
                            "Binary operands must be numbers",
                            binary.operator.clone(),
                        )),
                    },
                    TokenType::EqualEqual => Ok(Value::Bool(left == right)),
                    TokenType::BangEqual => Ok(Value::Bool(left != right)),
                    _ => unreachable!(),
                }
            }
            Expr::Variable(variable) => Ok(self
                .environment
                .get_var(variable)
                .expect("Variable not correctly resolved at compile time")),
            Expr::Assign(assign) => {
                let value = self.evaluate(&assign.value)?;
                let success = self.environment.assign(assign, value.clone());

                if !success {
                    return Err(InterpreterError::runtime_error(
                        "Undefined variable",
                        assign.name.clone(),
                    ));
                }

                Ok(value)
            }
            Expr::Logical(logical) => {
                let left = self.evaluate(&logical.left)?;
                if logical.operator.token_type == TokenType::Or {
                    if Self::is_truthy(&left) {
                        return Ok(left);
                    }
                } else if !Self::is_truthy(&left) {
                    return Ok(left);
                }
                self.evaluate(&logical.right)
            }
            Expr::Call(call) => {
                let callee = self.evaluate(&call.callee)?;
                let mut arguments = Vec::new();
                for arg in &call.arguments {
                    arguments.push(self.evaluate(arg)?);
                }

                match callee {
                    Value::Function(mut f) => f.call(self, arguments),
                    _ => Err(InterpreterError::runtime_error(
                        "Can only call functions and classes",
                        call.paren.clone(),
                    )),
                }
            }
        }
    }

    fn lox_stringify(value: Value) -> String {
        match value {
            Value::Nil => "nil".to_owned(),
            Value::Bool(c) => c.to_string(),
            Value::Number(n) => {
                if n.fract() == 0. {
                    format!("{:.0}", n)
                } else {
                    n.to_string()
                }
            }
            Value::String(s) => s,
            Value::Function(f) => format!("<fn {}>", f.declaration.name.lexeme),
        }
    }

    fn is_truthy(value: &Value) -> bool {
        match *value {
            Value::Nil => false,
            Value::Bool(x) => x,
            Value::Number(n) => n != 0.,
            _ => true,
        }
    }
}
