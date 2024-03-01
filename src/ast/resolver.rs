use std::{collections::HashMap, usize};

use crate::lexer::Token;

use super::{
    expr::{Assign, Expr, Variable},
    stmt::{Function, Stmt},
};

#[derive(Debug)]
pub enum ResolveErrorTypes {
    VariableAlreadyDeclared { line: usize, message: String },
    TopLevelReturn { line: usize, message: String },
    VariableInItsInitializer { line: usize, message: String },
}

#[derive(Clone, Copy)]
enum FunctionType {
    Function,
}

trait Resolve {
    fn lexeme(&self) -> &str;
    fn resolve(&mut self, depth: usize);
}

impl Resolve for Variable {
    fn lexeme(&self) -> &str {
        &self.name.lexeme
    }

    fn resolve(&mut self, depth: usize) {
        self.resolving_depth = Some(depth);
    }
}
impl Resolve for Assign {
    fn lexeme(&self) -> &str {
        &self.name.lexeme
    }

    fn resolve(&mut self, depth: usize) {
        self.resolving_depth = Some(depth);
    }
}

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    current_function: Option<FunctionType>,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            current_function: None,
        }
    }

    pub fn resolve(&mut self, stmts: &mut Vec<Stmt>) -> Result<(), ResolveErrorTypes> {
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }

        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &mut Stmt) -> Result<(), ResolveErrorTypes> {
        match stmt {
            Stmt::Var(var) => {
                self.declare(&var.name)?;
                if let Some(value) = &mut var.initializer {
                    self.resolve_expr(value)?;
                }
                self.define(&var.name);
            }
            Stmt::Block(block) => {
                self.begin_scope();
                self.resolve(&mut block.statements)?;
                self.end_scope();
            }
            Stmt::Function(func) => {
                self.declare(&func.name)?;
                self.define(&func.name);
                self.resolve_function(func, FunctionType::Function)?;
            }
            Stmt::Return(ret) => {
                if self.current_function.is_none() {
                    return Err(ResolveErrorTypes::TopLevelReturn {
                        line: ret.keyword.line,
                        message: "Cannot return from top-level code".to_owned(),
                    });
                }
                if let Some(value) = &mut ret.value {
                    self.resolve_expr(value)?;
                }
            }
            Stmt::Expression(expr) => {
                self.resolve_expr(&mut expr.expr)?;
            }
            Stmt::If(if_stmt) => {
                self.resolve_expr(&mut if_stmt.condition)?;
                self.resolve_stmt(&mut if_stmt.then_branch)?;
                if let Some(else_branch) = &mut if_stmt.else_branch {
                    self.resolve_stmt(else_branch)?;
                }
            }
            Stmt::While(while_stmt) => {
                self.resolve_expr(&mut while_stmt.condition)?;
                self.resolve_stmt(&mut while_stmt.body)?;
            }
            Stmt::Print(print) => {
                self.resolve_expr(&mut print.expr)?;
            }
        }

        Ok(())
    }

    fn resolve_expr(&mut self, expr: &mut Expr) -> Result<(), ResolveErrorTypes> {
        match expr {
            Expr::Assign(ref mut assign) => {
                self.resolve_expr(&mut assign.value)?;
                self.resolve_local(assign);
            }
            Expr::Binary(binary) => {
                self.resolve_expr(&mut binary.left)?;
                self.resolve_expr(&mut binary.right)?;
            }
            Expr::Call(call) => {
                self.resolve_expr(&mut call.callee)?;
                for arg in &mut call.arguments {
                    self.resolve_expr(arg)?;
                }
            }
            Expr::Grouping(grouping) => {
                self.resolve_expr(&mut grouping.expr)?;
            }
            Expr::Literal(_) => {}
            Expr::Logical(logical) => {
                self.resolve_expr(&mut logical.left)?;
                self.resolve_expr(&mut logical.right)?;
            }
            Expr::Unary(unary) => {
                self.resolve_expr(&mut unary.right)?;
            }
            Expr::Variable(ref mut var) => {
                if let Some(scope) = self.scopes.last() {
                    if let Some(declared) = scope.get(&var.name.lexeme) {
                        if !declared {
                            return Err(ResolveErrorTypes::VariableInItsInitializer {
                                line: var.name.line,
                                message: "Cannot read local variable in its own initializer"
                                    .to_owned(),
                            });
                        }
                    }
                }
                self.resolve_local(var);
            }
        }

        Ok(())
    }

    fn resolve_function(
        &mut self,
        func: &mut Function,
        function_type: FunctionType,
    ) -> Result<(), ResolveErrorTypes> {
        let enclosing_function = self.current_function;
        self.current_function = Some(function_type);
        self.begin_scope();
        for param in &func.params {
            self.declare(param)?;
            self.define(param);
        }
        self.resolve(&mut func.body)?;
        self.end_scope();
        self.current_function = enclosing_function;

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) -> Result<(), ResolveErrorTypes> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                return Err(ResolveErrorTypes::VariableAlreadyDeclared {
                    line: name.line,
                    message: format!(
                        "Variable with name {} already declared in this scope",
                        name.lexeme
                    ),
                });
            }
            scope.insert(name.lexeme.to_owned(), false);
        }

        Ok(())
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.to_owned(), true);
        }
    }

    fn resolve_local<T: Resolve>(&mut self, local: &mut T) {
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(local.lexeme()) {
                local.resolve(self.scopes.len() - 1 - i);
                return;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    use crate::ast::value::Value;
    use crate::lexer::{Token, TokenType};

    #[test]
    fn test_resolver() {
        let mut resolver = Resolver::new();
        let mut stmts = vec![Stmt::block(vec![
            Stmt::var(
                Token::new(TokenType::Identifier, "x", 1),
                Some(Expr::literal(Value::Number(5.))),
            ),
            Stmt::block(vec![Stmt::block(vec![Stmt::print(Expr::variable(
                Token::new(TokenType::Identifier, "x", 1),
            ))])]),
        ])];
        let result = resolver.resolve(&mut stmts);

        assert!(result.is_ok());
    }
}
