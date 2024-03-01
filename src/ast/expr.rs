use std::fmt::Debug;

use super::value::Value;
use crate::lexer::Token;

#[derive(Clone, PartialEq)]
pub enum Expr {
    Binary(Binary),
    Grouping(Grouping),
    Literal(Literal),
    Unary(Unary),
    Variable(Variable),
    Assign(Assign),
    Logical(Logical),
    Call(Call),
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Binary(arg0) => arg0.fmt(f),
            Self::Grouping(arg0) => arg0.fmt(f),
            Self::Literal(arg0) => arg0.fmt(f),
            Self::Unary(arg0) => arg0.fmt(f),
            Self::Variable(arg0) => arg0.fmt(f),
            Self::Assign(arg0) => arg0.fmt(f),
            Self::Logical(arg0) => arg0.fmt(f),
            Self::Call(arg0) => arg0.fmt(f),
        }
    }
}

impl Expr {
    pub fn binary(left: Expr, operator: Token, right: Expr) -> Self {
        Self::Binary(Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    pub fn grouping(value: Expr) -> Self {
        Self::Grouping(Grouping {
            expr: Box::new(value),
        })
    }

    pub fn unary(operator: Token, right: Expr) -> Self {
        Self::Unary(Unary {
            operator,
            right: Box::new(right),
        })
    }

    pub fn literal(value: Value) -> Self {
        Self::Literal(Literal { value })
    }

    pub fn variable(name: Token) -> Self {
        Self::Variable(Variable {
            name,
            resolving_depth: None,
        })
    }

    pub fn assign(name: Token, value: Expr) -> Self {
        Self::Assign(Assign {
            name,
            value: Box::new(value),
            resolving_depth: None,
        })
    }

    pub fn logical(left: Expr, operator: Token, right: Expr) -> Self {
        Self::Logical(Logical {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    pub fn call(callee: Expr, paren: Token, arguments: Vec<Expr>) -> Self {
        Self::Call(Call {
            callee: Box::new(callee),
            paren,
            arguments,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Grouping {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub value: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: Token,
    pub resolving_depth: Option<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub name: Token,
    pub value: Box<Expr>,
    pub resolving_depth: Option<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Logical {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Box<Expr>,
    pub paren: Token,
    pub arguments: Vec<Expr>,
}
