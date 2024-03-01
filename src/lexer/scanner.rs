use std::{error::Error, fmt::Display};

use super::keywords::KEYWORDS;
use crate::lexer::TokenType;

use super::token::Token;

#[derive(Debug, PartialEq)]
pub struct TokenError {
    lexeme: String,
    line: usize,
    message: String,
}
impl Error for TokenError {}
impl Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[Line {}] Error at {}: {}",
            self.line, self.lexeme, self.message
        )
    }
}

impl TokenError {
    fn new(lexeme: String, line: usize, message: String) -> Self {
        Self {
            lexeme,
            line,
            message,
        }
    }
}

pub struct Scanner {
    source: Vec<char>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    errors: Vec<TokenError>,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            tokens: vec![],
            line: 1,
            start: 0,
            current: 0,
            errors: vec![],
        }
    }

    /// Returns the scan tokens of this [`Scanner`].
    pub fn scan_tokens(mut self) -> Result<Vec<Token>, Vec<TokenError>> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(TokenType::Eof, "", self.line));

        match self.errors.is_empty() {
            true => Ok(self.tokens),
            false => Err(self.errors),
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '%' => self.add_token(TokenType::Modulo),
            '!' => {
                if self.advance_if_match('=') {
                    self.add_token(TokenType::BangEqual)
                } else {
                    self.add_token(TokenType::Bang)
                }
            }
            '=' => {
                if self.advance_if_match('=') {
                    self.add_token(TokenType::EqualEqual)
                } else {
                    self.add_token(TokenType::Equal)
                }
            }
            '<' => {
                if self.advance_if_match('=') {
                    self.add_token(TokenType::LessEqual)
                } else {
                    self.add_token(TokenType::Less)
                }
            }
            '>' => {
                if self.advance_if_match('=') {
                    self.add_token(TokenType::GreaterEqual)
                } else {
                    self.add_token(TokenType::Greater)
                }
            }
            '/' => {
                if self.advance_if_match('/') {
                    // A comment goes until the end of the line.
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash)
                }
            }
            '"' => self.parse_string(),
            c if c.is_ascii_digit() => self.parse_number(),
            c if c.is_alphabetic() => self.parse_identifier(),
            '\n' => self.line += 1,

            ' ' | '\t' | '\r' => {} // Ignore whitespace
            c => {
                self.errors.push(TokenError::new(
                    c.to_string(),
                    self.line,
                    format!("Unexpected character {c}."),
                ));
                while !self.is_at_end() && !self.peek().is_whitespace() {
                    self.advance();
                }
            }
        }
    }

    fn advance(&mut self) -> char {
        let res = self.source[self.current];
        self.current += 1;
        res
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token::new(
            token_type,
            self.source[self.start..self.current]
                .iter()
                .collect::<String>(),
            self.line,
        ))
    }

    fn advance_if_match(&mut self, expected: char) -> bool {
        if self.is_at_end() || expected != self.source[self.current] {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source[self.current + 1]
        }
    }

    fn parse_string(&mut self) {
        let start_line = self.line;
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.errors.push(TokenError::new(
                "\"".to_owned(),
                start_line,
                "Unterminated string.".to_owned(),
            ));
            return;
        }

        self.advance();

        let value = &self.source[self.start + 1..self.current - 1];
        self.add_token(TokenType::String {
            literal: value.iter().collect(),
        })
    }

    fn parse_number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        self.add_token(TokenType::Number {
            literal: self.source[self.start..self.current]
                .iter()
                .collect::<String>()
                .parse()
                .unwrap(),
        });
    }

    fn parse_identifier(&mut self) {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        let text: String = self.source[self.start..self.current].iter().collect();

        let token_type = match KEYWORDS.get(text.as_str()) {
            Some(x) => x.clone(),
            None => TokenType::Identifier,
        };

        self.add_token(token_type);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenType::*;

    #[test]
    fn test1() {
        let checks = [
            (
                r#""""#,
                vec![
                    Token::new(
                        String {
                            literal: "".to_owned(),
                        },
                        r#""""#.to_owned(),
                        1,
                    ),
                    Token::new(Eof, "", 1),
                ],
            ),
            (
                r#" console.log( x + "!=87" > 89 != 7547.436)"#,
                vec![
                    Token::new(Identifier, "console", 1),
                    Token::new(Dot, ".", 1),
                    Token::new(Identifier, "log", 1),
                    Token::new(LeftParen, "(", 1),
                    Token::new(Identifier, "x", 1),
                    Token::new(Plus, "+", 1),
                    Token::new(
                        String {
                            literal: "!=87".to_owned(),
                        },
                        r#""!=87""#.to_owned(),
                        1,
                    ),
                    Token::new(Greater, ">", 1),
                    Token::new(Number { literal: 89. }, "89", 1),
                    Token::new(BangEqual, "!=", 1),
                    Token::new(Number { literal: 7547.436 }, "7547.436", 1),
                    Token::new(RightParen, ")", 1),
                    Token::new(Eof, "", 1),
                ],
            ),
            (
                r#"
                    import 124

                    fun fib(a) {
                        return a - 1;
                    } 

                    print fib( if true { this."123"; } else if false { 5 * 5 } )
                "#,
                vec![
                    Token::new(Identifier, "import", 2),
                    Token::new(Number { literal: 124. }, "124", 2),
                    Token::new(Fun, "fun", 4),
                    Token::new(Identifier, "fib", 4),
                    Token::new(LeftParen, "(", 4),
                    Token::new(Identifier, "a", 4),
                    Token::new(RightParen, ")", 4),
                    Token::new(LeftBrace, "{", 4),
                    Token::new(Return, "return", 5),
                    Token::new(Identifier, "a", 5),
                    Token::new(Minus, "-", 5),
                    Token::new(Number { literal: 1. }, "1", 5),
                    Token::new(Semicolon, ";", 5),
                    Token::new(RightBrace, "}", 6),
                    Token::new(Print, "print", 8),
                    Token::new(Identifier, "fib", 8),
                    Token::new(LeftParen, "(", 8),
                    Token::new(If, "if", 8),
                    Token::new(True, "true", 8),
                    Token::new(LeftBrace, "{", 8),
                    Token::new(This, "this", 8),
                    Token::new(Dot, ".", 8),
                    Token::new(
                        String {
                            literal: "123".to_owned(),
                        },
                        r#""123""#,
                        8,
                    ),
                    Token::new(Semicolon, ";", 8),
                    Token::new(RightBrace, "}", 8),
                    Token::new(Else, "else", 8),
                    Token::new(If, "if", 8),
                    Token::new(False, "false", 8),
                    Token::new(LeftBrace, "{", 8),
                    Token::new(Number { literal: 5. }, "5", 8),
                    Token::new(Star, "*", 8),
                    Token::new(Number { literal: 5. }, "5", 8),
                    Token::new(RightBrace, "}", 8),
                    Token::new(RightParen, ")", 8),
                    Token::new(Eof, "", 9),
                ],
            ),
        ];

        for (left, right) in checks {
            let scan = Scanner::new(left);
            let tokens = scan.scan_tokens();
            assert!(tokens.is_ok());
            assert_eq!(tokens.unwrap(), right);
        }
    }

    #[test]
    fn test_error() {
        let checks = [(
            r#"
            
            "
            
            "#,
            vec![TokenError::new(
                "\"".to_owned(),
                3,
                "Unterminated string.".to_owned(),
            )],
        )];

        for (left, right) in checks {
            let scan = Scanner::new(left);
            let tokens = scan.scan_tokens();
            assert!(tokens.is_err());
            assert_eq!(tokens.unwrap_err(), right);
        }
    }
}
