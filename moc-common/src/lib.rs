pub mod error;
pub mod token;
pub mod debug_utils;
pub mod decl;
pub mod stmt;
pub mod expr;
pub mod ast;

use std::fmt::{Debug, Display};

use derive_more::Display;

use crate::{stmt::Stmt};

#[derive(Debug, Clone, Copy)]
pub struct CodeLocation {
    pub line: u32,
    pub column: u32,
}

impl Default for CodeLocation {
    fn default() -> Self {
        Self { line: 1, column: 1 }
    }
}

impl Display for CodeLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.line, self.column))
    }
}

#[derive(Debug, Clone)]
pub struct CodeBlock {
    pub stmts: Vec<Stmt>,
}

impl CodeBlock {
    pub fn new() -> Self {
        Self { stmts: Vec::new() }
    }
}

#[derive(Clone, Debug)]
pub struct ModuleIdentifier(pub Vec<String>);

impl ModuleIdentifier {
    pub fn new(module_path: Vec<String>) -> Self {
        ModuleIdentifier(module_path)
    }
    
    pub fn from_slice(module_path: &[String]) -> Self {
        ModuleIdentifier(module_path.iter().cloned().collect())
    }
}

impl Display for ModuleIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("ModuleIdentifier")?;
        self.0.fmt(f)
    }
}

#[derive(Clone, Copy, Debug, Display)]
pub enum BinaryOp {
    Add,
    Sub,           // Subtract
    Mult,          // Multiply
    Div,           // Divide
    Mod,           // Modulo
    BitShiftLeft,  // Bitshift left
    BitShiftRight, // Bitshift right
    BitOr,         // Bitwise OR
    BitAnd,        // Bitwise AND
    BitXor,        // Bitwise XOR
    BitNot,        // Bitwise NOT
}

#[derive(Debug, Clone)]
pub struct TypedVar {
    /// the type identifier
    type_ident: String,
    /// the identifier of the actual variable
    ident: String,
}

impl TypedVar {
    pub fn new(type_ident: String, ident: String) -> Self {
        Self { type_ident, ident }
    }
}
