use std::fmt::Debug;

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
        Stmt::Function(Function { name, params, body })
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

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub keyword: Token,
    pub value: Option<Expr>,
}
