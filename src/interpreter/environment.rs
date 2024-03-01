use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::ast::{
    expr::{Assign, Variable},
    value::Value,
};

#[derive(Debug)]
struct EnvNode {
    map: HashMap<String, Value>,
    parent: Option<Rc<RefCell<EnvNode>>>,
}

impl EnvNode {
    fn new() -> Self {
        EnvNode {
            map: HashMap::new(),
            parent: None,
        }
    }

    fn with_parent(parent: Rc<RefCell<EnvNode>>) -> Self {
        EnvNode {
            map: HashMap::new(),
            parent: Some(parent),
        }
    }

    fn get(&self, key: &str) -> Option<Value> {
        self.map.get(key).map(Clone::clone)
    }

    fn set(&mut self, key: &str, value: Value) -> bool {
        self.map.insert(key.to_owned(), value).is_some()
    }
}

// TODO: Add a wrapper environment that knows about globals for the interpreter
// so that get/sets are uniform wrt the Variable type.
#[derive(Clone, Debug)]
pub struct Environment {
    global: Rc<RefCell<EnvNode>>,
    node: Rc<RefCell<EnvNode>>,
}

impl PartialEq for Environment {
    fn eq(&self, other: &Self) -> bool {
        let ptr = &*self.node as *const _;
        let other_ptr = &*other.node as *const _;
        ptr == other_ptr
    }
}

pub trait Lexeme<'a> {
    fn lexeme(&'a self) -> &'a str;
    fn local_depth(&self) -> Option<usize>;
}

impl<'a> Lexeme<'a> for Variable {
    fn lexeme(&'a self) -> &'a str {
        &self.name.lexeme
    }

    fn local_depth(&self) -> Option<usize> {
        self.resolving_depth
    }
}

impl<'a> Lexeme<'a> for Assign {
    fn lexeme(&'a self) -> &'a str {
        &self.name.lexeme
    }

    fn local_depth(&self) -> Option<usize> {
        self.resolving_depth
    }
}

impl Environment {
    pub fn new() -> Self {
        let node = Rc::new(RefCell::new(EnvNode::new()));
        Environment {
            global: node.clone(),
            node,
        }
    }

    pub fn get_var<'a, T: Lexeme<'a>>(&'a self, key: &'a T) -> Option<Value> {
        match key.local_depth() {
            Some(local_depth) => self.get_at(key.lexeme(), local_depth),
            None => self.global.borrow().get(key.lexeme()),
        }
    }

    pub fn assign<'a, T: Lexeme<'a>>(&'a mut self, var: &'a T, value: Value) -> bool {
        match var.local_depth() {
            Some(local_depth) => self.set_at(var.lexeme(), value, local_depth),
            None => self.global.borrow_mut().set(var.lexeme(), value),
        }
    }

    pub fn set(&mut self, ident: &str, value: Value) {
        self.node.borrow_mut().set(ident, value);
    }

    fn get_at(&self, key: &str, depth: usize) -> Option<Value> {
        self.ancestor(depth)
            .and_then(|ancestor| ancestor.borrow().get(key).clone())
    }

    fn set_at(&mut self, key: &str, value: Value, depth: usize) -> bool {
        self.ancestor(depth)
            .map(|ancestor| ancestor.borrow_mut().set(key, value))
            .unwrap_or(false)
    }

    pub fn extend(&self) -> Self {
        let node = Rc::new(RefCell::new(EnvNode::with_parent(self.node.clone())));
        Environment {
            node,
            global: self.global.clone(),
        }
    }

    fn ancestor(&self, depth: usize) -> Option<Rc<RefCell<EnvNode>>> {
        let mut node = Some(self.node.clone());

        for _ in 0..depth {
            // Temporary introduced to get around borrowck
            let mut next = None;
            if let Some(ref node) = node {
                let bn = node.borrow();
                next = bn.parent.clone();
            }

            next.as_ref()?;

            node = next;
        }
        node
    }
}
