use std::{collections::HashMap, sync::OnceLock};

use super::TokenType;

pub(super) struct Keywords {
    lock: OnceLock<HashMap<&'static str, TokenType>>,
}

impl Keywords {
    pub(super) fn get(&self, s: &str) -> Option<&TokenType> {
        self.lock
            .get_or_init(|| {
                let mut map = HashMap::new();
                map.insert("and", TokenType::And);
                map.insert("class", TokenType::Class);
                map.insert("else", TokenType::Else);
                map.insert("false", TokenType::False);
                map.insert("for", TokenType::For);
                map.insert("fun", TokenType::Fun);
                map.insert("if", TokenType::If);
                map.insert("nil", TokenType::Nil);
                map.insert("or", TokenType::Or);
                map.insert("print", TokenType::Print);
                map.insert("return", TokenType::Return);
                map.insert("super", TokenType::Super);
                map.insert("this", TokenType::This);
                map.insert("true", TokenType::True);
                map.insert("var", TokenType::Var);
                map.insert("while", TokenType::While);
                map
            })
            .get(s)
    }
}

pub(super) static KEYWORDS: Keywords = Keywords {
    lock: OnceLock::new(),
};
