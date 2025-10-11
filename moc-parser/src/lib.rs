use std::fmt::{Debug, Display};

use derive_more::Display;

pub mod lexer;
pub mod parser;

#[derive(Debug, Clone)]
pub struct Token {
    pub r#type: TokenType,
    value: Option<String>,
    pub location: CodeLocation,
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum TokenType {
    AddAssign,
    Assign,
    At,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    BitAnd,
    BitOr,
    BitXor, // replace with proper function call statement
    Break,
    CloseBrace,
    CloseParen,
    Colon,
    Comma,
    DeclareAssign,
    Defer,
    DivAssign,
    Dot,
    EqualTo,
    Excl,
    Else,
    EndOfFile,
    False,
    Fn,
    For,
    Greater,
    GreaterOrEqual,
    Ident,
    If,
    In,
    Is,
    Less,
    LessOrEqual,
    LineBreak, // encompassing CRLF and LF in one token.
    Loop,
    Minus,
    Percent,
    ModAssign,
    MultAssign,
    NotEqualTo,
    NumberLiteral,
    OpenBrace,
    OpenParen,
    Plus,
    Ret,
    Semicolon,
    Slash,
    StringLiteral,
    Star,
    Struct,
    SubAssign,
    Sum,
    True,
    Use,
}

impl Token {
    pub fn string_literal(literal: String, location: CodeLocation) -> Self {
        Self {
            r#type: TokenType::StringLiteral,
            value: Some(literal.into()),
            location,
        }
    }
    pub fn new_ident(ident: String, location: CodeLocation) -> Self {
        Self {
            r#type: TokenType::Ident,
            value: Some(ident.into()),
            location,
        }
    }
    pub fn number_literal(literal: String, location: CodeLocation) -> Self {
        Self {
            r#type: TokenType::NumberLiteral,
            value: Some(literal.into()),
            location,
        }
    }
    pub fn new(_type: TokenType, location: CodeLocation) -> Self {
        Self {
            r#type: _type,
            value: None,
            location,
        }
    }
    pub fn value(&self) -> Option<&String> {
        self.value.as_ref()
    }

    /// # Panics
    /// Panics if no value is present
    pub fn unwrap_value(&self) -> String {
        self.value.as_ref().expect("Expected value").clone()
    }
}

#[derive(Debug)]
pub struct TypedVar {
    /// the type identifier
    type_ident: String,
    /// the identifier of the actual variable
    ident: String,
}

impl TypedVar {
    fn new(data_type: String, ident: String) -> Self {
        Self {
            type_ident: data_type,
            ident,
        }
    }
}
#[derive(Debug)]
pub struct FnCall {
    ident: String,
    args: Vec<Expr>
}

#[derive(Debug)]
pub enum Expr {
    // Expressions
    Binary {
        left_expr: Box<Expr>,
        operator: Token,
        right_expr: Box<Expr>,
    },
    BoolLiteral(bool),
    FnCall(FnCall), // maybe this is shit, but we'll just solve the problem first.
    Grouping(Box<Expr>),
    VariableIdent(String),

    Loop(Vec<Expr>), // simple infinite loop
    NumberLiteral(String),

    StringLiteral(String),
    Unary(Token, Box<Expr>), // Operator followed by another expr
    //IfIs
    EndOfFile,
}

pub struct CodeBlock {
    pub stmts: Vec<Stmt>,
}

impl CodeBlock {
    fn new() -> Self {
        Self { stmts: Vec::new() }
    }
}

pub struct ModuleIdentifier(Vec<String>);

impl Display for ModuleIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("ModuleIdentifier")?;
        self.0.fmt(f)
    }
}

pub enum Stmt {
    Print(Expr), // probably dont wanna have this as inbuilt function
    // Int32 a (declaring variable)
    VarDecl {       
        type_ident: String,
        var_ident: String,
    },
    // a = 10 (updating value)
    VarAssign {     
        ident: String,
        value: Expr,
    },
    // Int32 a := 10 OR a := 10 (infers type)
    VarDeclAssign {
        type_ident: Option<String>,
        ident: String,
        value: Expr,
    },
    Break,
    UseDecl {
        module_ident: ModuleIdentifier,
        module_alias: Option<String>,
    },
    FnDecl { // function declaration
        ident: String,
        params: Vec<TypedVar>,
        return_type: Option<String>,
        body: CodeBlock,
    },
    FnCall(FnCall), // maybe this is shit, but we'll just solve the problem first.
    ForLoop {
        condition: Expr,
        code_block: CodeBlock
    },
    If {
        condition: Expr,
        if_block: CodeBlock,
        else_block: Option<CodeBlock>,
    },
    Ret(Expr), // return statement
    StructDecl {
        ident: String,
        fields: Vec<TypedVar>,
    },
}

impl Stmt {
    fn print(&self) -> String {
        let mut s = String::new();
        self.print_inner(0, &mut s);
        return s;
    }

    fn print_inner(&self, depth: usize, result: &mut String) {
        let depth = depth + 1;
        result.push_str(&format!("{}{}", "\n", INDENT.repeat(depth)));
        match self {
            Stmt::Print(astnode) => {
                result.push_str("Print");
                astnode.print_inner(depth, result);
            }
            Stmt::VarDecl { type_ident, var_ident } => {
                result.push_str(&format!("VarDecl: \"{}\" {}", var_ident, type_ident));
                
            }
            Stmt::Break => {
                result.push_str("Break");
            }
            Stmt::If {
                condition,
                if_block,
                else_block,
            } => {
                result.push_str("If: ");
                condition.print_inner(depth, result);
                for if_block_stmt in &if_block.stmts {
                    if_block_stmt.print_inner(depth, result);
                }
                if let Some(else_block) = else_block {
                    for else_block_stmt in &else_block.stmts {
                        else_block_stmt.print_inner(depth, result);
                    }
                }
            }
            Stmt::UseDecl {
                module_ident,
                module_alias,
            } => {
                result.push_str(&format!("Use {}", module_ident));
                if let Some(mod_rename) = module_alias {
                    result.push_str(&format!(" Alias: \"{}\"", mod_rename));
                }
            }
            Stmt::FnDecl {
                ident,
                body,
                return_type,
                params,
            } => {
                result.push_str(&format!("FnDecl: {}(", ident));
                for (i, param) in params.iter().enumerate() {
                    result.push_str(&format!("{} {}", param.type_ident, param.ident));
                    if i < params.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push_str(")");
                if let Some(return_type) = return_type {
                    result.push_str(&format!(" {}", return_type));
                }
                for stmt in &body.stmts {
                    stmt.print_inner(depth, result);
                }
            }
            Stmt::StructDecl { ident, fields: body } => {
                result.push_str(&format!("StructDecl: {}", ident));
                for field_decl in body {
                    result.push_str(&format!(
                        "{:?} \"{}\"",
                        field_decl.type_ident, field_decl.ident
                    ));
                }
            }
            Stmt::Ret(expr) => {
                result.push_str("Ret: ");
                expr.print_inner(depth, result);
            }
            Stmt::VarAssign { ident, value } => {
                result.push_str(&format!("VarAssign \"{}\"", ident));
                value.print_inner(depth, result);
            }
            Stmt::VarDeclAssign {
                type_ident,
                ident,
                value,
            } => {
                result.push_str(&format!("VarDeclAssign \"{}\"", ident));
                if let Some(type_ident) = type_ident {
                    result.push_str(&format!(" {}", type_ident));
                }
                value.print_inner(depth, result);
            },
            Stmt::FnCall(fn_call) => {
                result.push_str(&format!("FnCall: {}", fn_call.ident));
                for arg in &fn_call.args {
                    arg.print_inner(depth, result);
                }
            }
            Stmt::ForLoop { condition, code_block } => {
                result.push_str("ForLoop: ");
                condition.print_inner(depth, result);
                for stmt in &code_block.stmts {
                    stmt.print_inner(depth, result);
                }
            }
        }   
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.print())
    }
}

const INDENT: &str = "  ";

impl Expr {
    pub fn binary(left: Self, operator: Token, right: Self) -> Self {
        Self::Binary {
            left_expr: Box::new(left),
            operator,
            right_expr: Box::new(right),
        }
    }
    pub fn grouping(expr: Self) -> Self {
        Self::Grouping(Box::new(expr))
    }
    pub fn unary(operator: Token, right: Self) -> Self {
        Self::Unary(operator, Box::new(right))
    }

    fn print(&self) -> String {
        let mut s = String::new();
        self.print_inner(0, &mut s);
        return s;
    }

    fn print_inner(&self, depth: usize, result: &mut String) {
        let depth = depth + 1;
        result.push_str(&format!("{}{}", "\n", INDENT.repeat(depth)));
        match self {
            /* Expr::Print(astnode) => {
                result.push_str("Print");
                astnode.print_inner(depth, result);
            }
            Expr::VarDecl(ident, astnode) => {
                result.push_str(&format!("VarDecl \"{}\"", ident));
                astnode.print_inner(depth, result);
            }
            Expr::Break => {
                result.push_str("Break");
            } */
            Expr::Binary {
                left_expr,
                operator,
                right_expr,
            } => {
                result.push_str(&format!("Binary: {:?}", operator.r#type));
                left_expr.print_inner(depth, result);
                right_expr.print_inner(depth, result);
            }
            Expr::BoolLiteral(bool) => result.push_str(&bool.to_string()),
            Expr::Grouping(astnode) => {
                result.push_str("Grouping: ");
                astnode.print_inner(depth, result);
            }
            Expr::VariableIdent(ident) => {
                result.push_str(&format!("VariableIdent: \"{}\"", ident));
            }
            Expr::FnCall(fn_call) => {
                result.push_str(&format!("FnCall: {}", fn_call.ident));
                for arg in &fn_call.args {
                    arg.print_inner(depth, result);
                }
            }
            /* Expr::If { condition, if_block, else_block } => {
                result.push_str("If: ");
                condition.print_inner(depth, result);
                for if_block_stmt in if_block {
                    if_block_stmt.print_inner(depth, result);
                }
                if let Some(else_block) = else_block {
                    for else_block_stmt in else_block {
                        else_block_stmt.print_inner(depth, result);
                    }
                }
            } */
            Expr::Loop(astnodes) => {
                result.push_str("Loop");
                for stmt in astnodes {
                    stmt.print_inner(depth, result);
                }
            }
            Expr::NumberLiteral(num) => {
                result.push_str(&format!("NumberLiteral: {}", num));
            }
            Expr::StringLiteral(str) => {
                result.push_str(&format!("StringLiteral: {}", str));
            }
            Expr::Unary(op, astnode) => {
                result.push_str(&format!("Unary: {:?}", op));
                astnode.print_inner(depth, result);
            }
            /* Expr::UseDecl {
                module_ident,
                module_rename,
            } => {
                result.push_str(&format!("Use \"{}\"", module_ident));
                if let Some(mod_rename) = module_rename {
                    result.push_str(&format!(" {}", mod_rename));
                }
            }
            Expr::FnDecl { ident, body } => {
                result.push_str(&format!("FnDecl: {}", ident));
                for stmt in body {
                    stmt.print_inner(depth, result);
                }
            }
            Expr::StructDecl { ident, body } => {
                result.push_str(&format!("StructDecl: {}", ident));
                for field_decl in body {
                    result.push_str(&format!(
                        "{:?} \"{}\"",
                        field_decl.data_type, field_decl.ident
                    ));
                }
            } */
            Expr::EndOfFile => {
                result.push_str("EOF");
            }
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.print())
    }
}

#[cfg(test)]
mod test {
  use super::*;
  
  #[test]
  fn token_type_eq() {
      assert!(TokenType::At == TokenType::At)
  }
}