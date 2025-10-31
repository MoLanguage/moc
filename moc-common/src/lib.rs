pub mod ast;
pub mod debug_utils;
pub mod decl;
pub mod error;
pub mod expr;
pub mod stmt;
pub mod token;

use std::{
    collections::VecDeque,
    fmt::{Debug, Display},
};

use derive_more::Display;
use serde::Serialize;

use crate::{expr::TypeExpr, stmt::Stmt};

#[derive(Debug, Clone, Copy, Serialize)]
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

#[derive(Debug, Clone, Copy, Default, Serialize)]
pub struct CodeSpan {
    pub start: CodeLocation,
    pub end: CodeLocation,
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
        Self {
            start: value.0,
            end: value.1,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct CodeBlock {
    pub stmts: Vec<Stmt>,
}

impl CodeBlock {
    pub fn new() -> Self {
        Self { stmts: Vec::new() }
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct ModulePath {
    pub path: VecDeque<String>,
}

impl ModulePath {
    pub fn new(module_path: VecDeque<String>) -> Self {
        ModulePath { path: module_path }
    }

    pub fn from_slice(module_path: &[String]) -> Self {
        ModulePath {
            path: module_path.iter().cloned().collect(),
        }
    }

    /// Like for example: The string "mod:submod:my_function" will yield a ModuleIdentifier with dirs: mod:submod
    pub fn from_qualified_item_identifier(ident: &str) -> Self {
        let mut path: VecDeque<String> = ident
            .split_terminator(":")
            .map(|path_dir| path_dir.to_string())
            .collect();
        path.remove(path.len() - 1);
        ModulePath { path }
    }
    pub fn from_string(ident: &str) -> Self {
        let path = ident
            .split_terminator(":")
            .map(|path_dir| path_dir.to_string())
            .collect();
        ModulePath { path }
    }

    pub fn remove_and_get_last_path(&mut self) -> String {
        // Safety: If we check the length of the array it should always yield an element. Therefore the unchecked unwrap shouldn't fail ;)
        let suffix  = unsafe { self.path.remove(self.path.len() - 1).unwrap_unchecked() };
        suffix
    }
}

impl Display for ModulePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("ModuleIdentifier")?;
        self.path.fmt(f)
    }
}

#[derive(Clone, Copy, Debug, Display, Serialize)]
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

#[derive(Debug, Clone, Serialize)]
pub struct TypedVar {
    /// the variable's identifier
    ident: String,
    /// the type identifier
    type_expr: TypeExpr,
}

impl TypedVar {
    pub fn new(ident: String, type_expr: TypeExpr) -> Self {
        Self { ident, type_expr }
    }
}
