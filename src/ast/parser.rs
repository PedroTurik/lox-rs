use std::{error::Error, fmt::Display};

use crate::{
    ast::value::Value,
    lexer::{Token, TokenType},
};

use super::{expr::Expr, stmt::Stmt};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParserError {
    token: Token,
    message: &'static str,
}

impl Error for ParserError {}
impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[Line {}] Parsing Error {}: {}",
            self.token.line,
            if self.token.token_type == TokenType::Eof {
                "at end".to_owned()
            } else {
                format!("at {}", self.token.lexeme)
            },
            self.message
        )
    }
}

type Result<T> = ::core::result::Result<T, ParserError>;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> ::core::result::Result<Vec<Stmt>, Vec<ParserError>> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();
        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => {
                    self.synchronize();
                    errors.push(err)
                }
            }
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(statements)
        }
    }

    fn declaration(&mut self) -> Result<Stmt> {
        if self.match_token(&TokenType::Var) {
            return self.var_declaration();
        }

        if self.match_token(&TokenType::Fun) {
            return self.function();
        }

        self.statement()
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        let name = self
            .consume(&TokenType::Identifier, "Expect variable name")?
            .clone();

        let initializer = if self.match_token(&TokenType::Equal) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            &TokenType::Semicolon,
            "Expect ';' after variable declaration",
        )?;

        Ok(Stmt::var(name.clone(), initializer))
    }

    fn function(&mut self) -> Result<Stmt> {
        let name = self
            .consume(&TokenType::Identifier, "Expect function name")?
            .clone();
        self.consume(&TokenType::LeftParen, "Expect '(' after function")?;
        let mut params = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    return Err(ParserError {
                        token: self.peek().clone(),
                        message: "Cannot have more than 255 parameters",
                    });
                }

                params.push(
                    self.consume(&TokenType::Identifier, "Expect parameter name")?
                        .clone(),
                );

                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::RightParen, "Expect ')' after parameters")?;
        self.consume(&TokenType::LeftBrace, "Expect '{{' before function body")?;
        let body = self.block()?;
        Ok(Stmt::function(name.clone(), params, body))
    }

    fn statement(&mut self) -> Result<Stmt> {
        match self.advance().token_type {
            TokenType::Print => self.print_statement(),
            TokenType::LeftBrace => Ok(Stmt::block(self.block()?)),
            TokenType::If => self.if_statement(),
            TokenType::While => self.while_statement(),
            TokenType::For => self.for_statement(),
            TokenType::Return => self.return_statement(),
            _ => {
                self.step_back();
                self.expression_statement()
            }
        }
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let value = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expect ';' after value")?;
        Ok(Stmt::print(value))
    }

    fn block(&mut self) -> Result<Vec<Stmt>> {
        let mut statements = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        self.consume(&TokenType::RightBrace, "Expect '}' after block")?;
        Ok(statements)
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'if'")?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen, "Expect ')' after if condition")?;
        let then_branch = Box::new(self.statement()?);
        let else_branch = if self.match_token(&TokenType::Else) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };
        Ok(Stmt::if_stmt(condition, then_branch, else_branch))
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'while'")?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen, "Expect ')' after while condition")?;
        let body = Box::new(self.statement()?);
        Ok(Stmt::while_stmt(condition, body))
    }

    fn for_statement(&mut self) -> Result<Stmt> {
        self.consume(&TokenType::LeftParen, "Expect ( after 'for'")?;
        let initializer = match self.advance().token_type {
            TokenType::Semicolon => None,
            TokenType::Var => Some(self.var_declaration()?),
            _ => {
                self.step_back();
                Some(self.expression_statement()?)
            }
        };

        let condition = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&TokenType::Semicolon, "Expect ';' after loop condition")?;

        let increment = if !self.check(&TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&TokenType::RightParen, "Expect ')' after for clauses")?;

        let mut body = Box::new(self.statement()?);

        if let Some(increment) = increment {
            body = Box::new(Stmt::block(vec![*body, Stmt::expression(increment)]));
        };

        body = Box::new(Stmt::while_stmt(
            match condition {
                Some(condition) => condition,
                None => Expr::literal(Value::Bool(true)),
            },
            body,
        ));

        if let Some(initializer) = initializer {
            body = Box::new(Stmt::block(vec![initializer, *body]));
        };

        Ok(*body)
    }

    fn return_statement(&mut self) -> Result<Stmt> {
        let keyword = self.previous().clone();
        let value = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&TokenType::Semicolon, "Expect ';' after return value")?;
        Ok(Stmt::return_stmt(keyword.clone(), value))
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expect ';' after expression")?;
        Ok(Stmt::expression(expr))
    }

    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.or()?;

        if self.match_token(&TokenType::Equal) {
            let equals = self.previous().clone();
            let value = self.assignment()?;

            if let Expr::Variable(var) = expr {
                return Ok(Expr::assign(var.name, value));
            }

            return Err(ParserError {
                token: equals.clone(),
                message: "Invalid assignment target",
            });
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr> {
        let mut expr = self.and()?;

        while self.match_token(&TokenType::Or) {
            let operator = self.previous().clone();
            let right = self.and()?;
            expr = Expr::logical(expr, operator.clone(), right);
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr> {
        let mut expr = self.equality()?;

        while self.match_token(&TokenType::And) {
            let operator = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::logical(expr, operator, right);
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;
        while self.match_tokens(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous();
            expr = Expr::binary(expr, operator.clone(), self.comparison()?)
        }

        Ok(expr)
    }
    fn comparison(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;

        while self.match_tokens(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous();
            expr = Expr::binary(expr, operator.clone(), self.term()?)
        }

        Ok(expr)
    }
    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;

        while self.match_tokens(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous();
            expr = Expr::binary(expr, operator.clone(), self.factor()?)
        }

        Ok(expr)
    }
    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;

        while self.match_tokens(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            expr = Expr::binary(expr, operator.clone(), self.unary()?)
        }

        Ok(expr)
    }
    fn unary(&mut self) -> Result<Expr> {
        if self.match_tokens(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();
            return Ok(Expr::unary(operator.clone(), self.unary()?));
        }

        self.call()
    }

    fn call(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;

        loop {
            if self.match_token(&TokenType::LeftParen) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr> {
        self.advance();

        use TokenType::*;
        match &self.previous().token_type {
            False => Ok(Expr::literal(Value::Bool(false))),
            True => Ok(Expr::literal(Value::Bool(true))),
            Nil => Ok(Expr::literal(Value::Nil)),
            Number { literal } => Ok(Expr::literal(Value::Number(*literal))),
            String { literal } => Ok(Expr::literal(Value::String(literal.clone()))),
            Identifier => Ok(Expr::variable(self.previous().clone())),
            LeftParen => {
                let expr = self.expression()?;
                self.consume(&RightParen, "Expect ')' after expression")?;
                Ok(Expr::grouping(expr))
            }

            _ => {
                self.step_back();

                Err(ParserError {
                    token: self.peek().clone(),
                    message: "expected expression",
                })
            }
        }
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr> {
        let mut arguments = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    return Err(ParserError {
                        token: self.peek().clone(),
                        message: "Cannot have more than 255 arguments",
                    });
                }
                arguments.push(self.expression()?);
                if !self.match_token(&TokenType::Comma) {
                    break;
                }
            }
        }

        let paren = self.consume(&TokenType::RightParen, "Expect ')' after arguments")?;
        Ok(Expr::call(callee, paren.clone(), arguments))
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn step_back(&mut self) {
        if self.current > 0 {
            self.current -= 1;
        }
    }

    fn match_tokens(&mut self, args: &[TokenType]) -> bool {
        args.iter().any(|e| {
            if self.check(e) {
                self.advance();
                return true;
            }

            false
        })
    }

    fn match_token(&mut self, token_type: &TokenType) -> bool {
        if self.check(token_type) {
            self.advance();
            return true;
        }

        false
    }

    fn check(&self, token_type: &TokenType) -> bool {
        !self.is_at_end() && self.peek().token_type == *token_type
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        self.tokens
            .get(self.current)
            .expect("Token pointer should always be valid")
    }

    fn previous(&self) -> &Token {
        self.tokens
            .get(self.current - 1)
            .expect("Token pointer should always be valid")
    }

    fn consume(&mut self, token_type: &TokenType, message: &'static str) -> Result<&Token> {
        if self.check(token_type) {
            return Ok(self.advance());
        }

        Err(ParserError {
            token: self.peek().clone(),
            message,
        })
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            use TokenType::*;
            match self.peek().token_type {
                Fun | Var | For | If | While | Print | Return => return,
                _ => {
                    self.advance();
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::stmt::{Block, Var, While};

    use super::*;

    #[test]
    fn test_parser() {
        let tokens = vec![
            Token {
                token_type: TokenType::Var,
                lexeme: "var".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Identifier,
                lexeme: "x".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Equal,
                lexeme: "=".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Number { literal: 1.0 },
                lexeme: "1".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Eof,
                lexeme: "".to_owned(),
                line: 1,
            },
        ];
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_ok());
        let result = result.unwrap();
        assert_eq!(result.len(), 1);
        let stmt = result.get(0).unwrap();
        match stmt {
            Stmt::Var(var) => {
                assert_eq!(var.name.lexeme, "x");
                assert_eq!(var.initializer, Some(Expr::literal(Value::Number(1.0))));
            }
            _ => panic!("Expected Var statement"),
        }
    }

    #[test]
    fn test_parser_error() {
        let tokens = vec![
            Token {
                token_type: TokenType::Var,
                lexeme: "var".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Equal,
                lexeme: "=".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Number { literal: 1.0 },
                lexeme: "1".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Eof,
                lexeme: "".to_owned(),
                line: 1,
            },
        ];
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_err());
        let result = result.unwrap_err();
        assert_eq!(result.len(), 1);
        let err = result.get(0).unwrap();
        assert_eq!(err.message, "Expect variable name");
    }

    #[test]
    fn test_for_statement() {
        let tokens = vec![
            Token {
                token_type: TokenType::For,
                lexeme: "for".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::LeftParen,
                lexeme: "(".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Var,
                lexeme: "var".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Identifier,
                lexeme: "x".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Equal,
                lexeme: "=".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Number { literal: 0.0 },
                lexeme: "0".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Identifier,
                lexeme: "x".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Less,
                lexeme: "<".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Number { literal: 10.0 },
                lexeme: "10".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Identifier,
                lexeme: "x".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Equal,
                lexeme: "=".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Identifier,
                lexeme: "x".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Plus,
                lexeme: "+".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Number { literal: 1.0 },
                lexeme: "1".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::RightParen,
                lexeme: ")".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::LeftBrace,
                lexeme: "{".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Print,
                lexeme: "print".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Number { literal: 1.0 },
                lexeme: "1".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::RightBrace,
                lexeme: "}".to_owned(),
                line: 1,
            },
            Token {
                token_type: TokenType::Eof,
                lexeme: "".to_owned(),
                line: 1,
            },
        ];
        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert!(result.is_ok());
        let result = result.unwrap();
        assert_eq!(result.len(), 1);
        let stmt = result.get(0).unwrap();
        assert_eq!(
            stmt,
            &Stmt::Block(Block {
                statements: vec![
                    Stmt::Var(Var {
                        name: Token {
                            token_type: TokenType::Identifier,
                            lexeme: "x".to_owned(),
                            line: 1,
                        },
                        initializer: Some(Expr::literal(Value::Number(0.0))),
                    }),
                    Stmt::While(While {
                        condition: Expr::binary(
                            Expr::variable(Token {
                                token_type: TokenType::Identifier,
                                lexeme: "x".to_owned(),
                                line: 1,
                            }),
                            Token {
                                token_type: TokenType::Less,
                                lexeme: "<".to_owned(),
                                line: 1,
                            },
                            Expr::literal(Value::Number(10.0))
                        ),
                        body: Box::new(Stmt::block(vec![
                            Stmt::block(vec![Stmt::print(Expr::literal(Value::Number(1.0)))]),
                            Stmt::expression(Expr::assign(
                                Token::new(TokenType::Identifier, "x", 1),
                                Expr::binary(
                                    Expr::variable(Token::new(TokenType::Identifier, "x", 1)),
                                    Token::new(TokenType::Plus, "+", 1),
                                    Expr::literal(Value::Number(1.0))
                                )
                            ))
                        ])),
                    })
                ]
            })
        )
    }
}
