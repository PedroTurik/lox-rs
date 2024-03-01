use crate::lexer::Token;

#[derive(Debug)]
pub struct RuntimeError {
    message: String,
    token: Token,
}

impl RuntimeError {
    pub fn new(message: String, token: Token) -> Self {
        Self { message, token }
    }
}
