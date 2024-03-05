use std::{
    cell::UnsafeCell,
    fmt::Debug,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use crate::lexer::Token;

use super::expr::Expr;

#[derive(Clone, PartialEq)]
pub enum Stmt {
    Expression(Expression),
    Print(Print),
    Var(Var),
    Block(Block),
    If(If),
    While(While),
    Function(Function),
    Return(Return),
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Expression(arg0) => arg0.fmt(f),
            Self::Print(arg0) => arg0.fmt(f),
            Self::Var(arg0) => arg0.fmt(f),
            Self::Block(arg0) => arg0.fmt(f),
            Self::If(arg0) => arg0.fmt(f),
            Self::While(arg0) => arg0.fmt(f),
            Self::Function(arg0) => arg0.fmt(f),
            Self::Return(arg0) => arg0.fmt(f),
        }
    }
}

impl Stmt {
    pub fn expression(expr: Expr) -> Self {
        Stmt::Expression(Expression { expr })
    }

    pub fn print(expr: Expr) -> Self {
        Stmt::Print(Print { expr })
    }

    pub fn var(name: Token, initializer: Option<Expr>) -> Self {
        Stmt::Var(Var { name, initializer })
    }

    pub fn block(statements: Vec<Stmt>) -> Self {
        Stmt::Block(Block { statements })
    }

    pub fn if_stmt(
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    ) -> Self {
        Stmt::If(If {
            condition,
            then_branch,
            else_branch,
        })
    }

    pub fn while_stmt(condition: Expr, body: Box<Stmt>) -> Self {
        Stmt::While(While { condition, body })
    }

    pub fn function(name: Token, params: Vec<Token>, body: Vec<Stmt>) -> Self {
        Stmt::Function(Function {
            function: Rc::new(UnsafeCell::new(FunctionDecl { name, params, body })),
        })
    }

    pub fn return_stmt(keyword: Token, value: Option<Expr>) -> Self {
        Stmt::Return(Return { keyword, value })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Print {
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub name: Token,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub function: Rc<UnsafeCell<FunctionDecl>>,
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.function, &other.function)
    }
}

impl Deref for Function {
    type Target = FunctionDecl;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.function.get() }
    }
}

impl DerefMut for Function {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.function.get() }
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionDecl {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub keyword: Token,
    pub value: Option<Expr>,
}
