use std::{collections::HashMap, io::Seek, ops::Index};

use crate::lexer::TokenType;

use super::token::{self, Token};
use lazy_static::lazy_static;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut map = HashMap::new();
        map.insert("and", TokenType::AND);
        map.insert("class", TokenType::CLASS);
        map.insert("else", TokenType::ELSE);
        map.insert("false", TokenType::FALSE);
        map.insert("for", TokenType::FOR);
        map.insert("fun", TokenType::FUN);
        map.insert("if", TokenType::IF);
        map.insert("nil", TokenType::NIL);
        map.insert("or", TokenType::OR);
        map.insert("print", TokenType::PRINT);
        map.insert("return", TokenType::RETURN);
        map.insert("super", TokenType::SUPER);
        map.insert("this", TokenType::THIS);
        map.insert("true", TokenType::TRUE);
        map.insert("var", TokenType::VAR);
        map.insert("while", TokenType::WHILE);
        map
    };
}

pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
        }
    }

    /// Returns the scan tokens of this [`Scanner`].
    pub fn scan_tokens(mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens
            .push(Token::new(TokenType::EOF, "".to_owned(), self.line));

        self.tokens
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            "(" => panic!(),
            _ => panic!(),
        }
    }

    fn advance(&mut self) -> &str {
        let res = &self.source[self.current..self.current + 1];
        self.current += 1;
        res
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token::new(
            token_type,
            self.source[self.start..self.current].to_owned(),
            self.line,
        ))
    }

    fn advance_if_match(&mut self, expected: &str) -> bool {
        if self.is_at_end() || *expected != self.source[self.current..self.current + 1] {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn peek(&self) -> &str {
        if self.is_at_end() {
            "\0"
        } else {
            &self.source[self.current..self.current + 1]
        }
    }

    fn peek_next(&self) -> &str {
        if self.is_at_end() {
            "\0"
        } else {
            &self.source[self.current..self.current + 1]
        }
    }

    fn parse_string(&mut self) {
        while self.peek() != "\"" && !self.is_at_end() {
            if self.peek() == "\n" {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            todo!("ERROR");
        }

        self.advance();

        let value = &self.source[self.start + 1..self.current - 1];
        self.add_token(TokenType::STRING {
            literal: value.to_owned(),
        })
    }

    fn parse_number(&mut self) {
        while Scanner::is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == "." && Scanner::is_digit(self.peek_next()) {
            self.advance();

            while Scanner::is_digit(self.peek()) {
                self.advance();
            }
        }

        self.add_token(TokenType::NUMBER {
            literal: self.source[self.start..self.current].parse().unwrap(),
        })
    }

    fn is_digit(c: &str) -> bool {
        if let Some(x) = c.chars().next() {
            x.is_ascii_digit()
        } else {
            false
        }
    }
}
