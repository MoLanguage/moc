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

impl CodeLocation {
    pub fn is_in_same_line(&self, other: &Self) -> bool {
        self.line == other.line
    }
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

#[derive(Debug, Clone, Copy, Default)]
pub struct CodeSpan {
    pub start: CodeLocation,
    pub end: CodeLocation
}

impl CodeSpan {
    // Shouldnt ever be true really >:(
    pub fn spans_across_lines(&self) -> bool {
        !self.start.is_in_same_line(&self.end)
    }
    pub fn length(&self) -> u32 {
        self.end.column - self.start.column
    }
}

impl From<(CodeLocation, CodeLocation)> for CodeSpan {
    fn from(value: (CodeLocation, CodeLocation)) -> Self {
        Self { start: value.0, end: value.1 }
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
pub struct ModIdent(pub Vec<String>);

impl ModIdent {
    pub fn new(module_path: Vec<String>) -> Self {
        ModIdent(module_path)
    }

    pub fn from_slice(module_path: &[String]) -> Self {
        ModIdent(module_path.iter().cloned().collect())
    }

    /// Like for example: "mod:submod:my_function" will yield a ModuleIdentifier with dirs: mod:submod
    pub fn from_qualified_item_identifier(ident: &str) -> Self {
        let dirs: Vec<String> = ident.split_terminator(":").map(|path_dir| path_dir.to_string()).collect();
        let dirs = &dirs[0..dirs.len()-1]; // cut off non-module identifier part
        ModIdent(dirs.iter().cloned().collect())
    }
    pub fn from_string(ident: &str) -> Self {
        let dirs = ident.split_terminator(":").map(|path_dir| path_dir.to_string()).collect();
        ModIdent(dirs)
    }
}

impl Display for ModIdent {
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
    /// the variable's identifier
    ident: String,
    /// the type identifier
    type_ident: String,
}

impl TypedVar {
    pub fn new(ident: String, type_ident: String) -> Self {
        Self { ident, type_ident }
    }
}
