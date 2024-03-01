use std::{fs, io};

use ast::parser::Parser;
use interpreter::ast_interpreter::Interpreter;
use lexer::Scanner;

use crate::ast::resolver::Resolver;

pub mod ast;
pub mod interpreter;
pub mod lexer;
pub mod lox;

pub fn run() {
    let source = fs::read_to_string("x.lox").unwrap();
    let tokens = Scanner::new(&source).scan_tokens().unwrap();
    let mut ast = Parser::new(tokens).parse().unwrap();
    let mut resolver = Resolver::new();
    resolver.resolve(&mut ast).unwrap();

    let mut interpreter = Interpreter::new(io::stdout().lock());

    // dbg!(ast.clone());

    let ans = interpreter.interpret(ast);

    // dbg!(ans);
}
