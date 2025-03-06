use std::fmt::{Display, format};

pub mod lexer;
pub mod parser;

#[derive(Debug, Clone)]
pub struct Token {
    pub _type: TokenType,
    value: Option<String>,
    pub location: CodeLocation,
}

#[derive(Debug, Clone, Copy)]
pub struct CodeLocation {
    line: u32,
    column: u32,
}

impl Default for CodeLocation {
    fn default() -> Self {
        Self { line: 1, column: 1 }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    Declare,
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
    Mod,
    ModAssign,
    MultAssign,
    NotEqualTo,
    NumberLiteral,
    OpenBrace,
    OpenParen,
    Plus,
    Print,
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
            _type: TokenType::StringLiteral,
            value: Some(literal.into()),
            location,
        }
    }
    pub fn new_ident(ident: String, location: CodeLocation) -> Self {
        Self {
            _type: TokenType::Ident,
            value: Some(ident.into()),
            location,
        }
    }
    pub fn number_literal(literal: String, location: CodeLocation) -> Self {
        Self {
            _type: TokenType::NumberLiteral,
            value: Some(literal.into()),
            location,
        }
    }
    pub fn new(_type: TokenType, location: CodeLocation) -> Self {
        Self {
            _type,
            value: None,
            location,
        }
    }
    pub fn value(&self) -> Option<&String> {
        self.value.as_ref()
    }
}

#[derive(Debug)]
pub enum ASTNode {
    // Stmt
    Print(Box<ASTNode>), // probably dont wanna have this as inbuilt function
    VarDecl(String, Box<ASTNode>),
    Break,

    // Expressions
    Binary(Box<ASTNode>, Token, Box<ASTNode>),
    BoolLiteral(bool),
    Grouping(Box<ASTNode>),
    VariableIdent(String),
    If(Box<ASTNode>, Vec<ASTNode>, Option<Vec<ASTNode>>), // boolean expr, if-case, optional else-case
    Loop(Vec<ASTNode>),                                   // simple infinite loop
    NumberLiteral(String),

    StringLiteral(String),
    Unary(Token, Box<ASTNode>), // Operator followed by another expr

    // Items
    UseDecl {
        module_ident: String,
        module_rename: Option<String>,
    },
    FnDecl {
        ident: String,
        body: Vec<ASTNode>,
    },
    StructDecl {
        ident: String,
        body: Vec<StructFieldDecl>,
    },

    EndOfFile,
}

#[derive(Debug)]
enum DataType {
    Int32,
    Flt32,
    String,
    StructType {
        ident: String,
        body: Vec<StructFieldDecl>,
    },
}

#[derive(Debug)]
pub struct StructFieldDecl {
    ident: String,
    data_type: DataType,
}

impl ASTNode {
    pub fn binary(left: Self, operator: Token, right: Self) -> Self {
        Self::Binary(Box::new(left), operator, Box::new(right))
    }
    pub fn grouping(expr: Self) -> Self {
        Self::Grouping(Box::new(expr))
    }
    pub fn unary(operator: Token, right: Self) -> Self {
        Self::Unary(operator, Box::new(right))
    }

    fn print(&self) -> String {
        let mut s = String::new();
        s.push_str("AST: ");
        self.print_inner(0, &mut s);
        return s;
    }

    fn print_inner(&self, depth: usize, result: &mut String) {
        const INDENT: &str = "  ";
        let depth = depth + 1;
        result.push_str(&format!("{}{}", "\n", INDENT.repeat(depth)));
        match self {
            ASTNode::Print(astnode) => {
                result.push_str("Print");
                astnode.print_inner(depth, result);
            }
            ASTNode::VarDecl(ident, astnode) => {
                result.push_str(&format!("VarDecl \"{}\"", ident));
                astnode.print_inner(depth, result);
            }
            ASTNode::Break => {
                result.push_str("Break");
            }
            ASTNode::Binary(astnode, op, astnode1) => {
                result.push_str(&format!("Binary: {:?}", op._type));
                astnode.print_inner(depth, result);
                astnode1.print_inner(depth, result);
            }
            ASTNode::BoolLiteral(bool) => result.push_str(&bool.to_string()),
            ASTNode::Grouping(astnode) => {
                result.push_str("Grouping: ");
                astnode.print_inner(depth, result);
            }
            ASTNode::VariableIdent(ident) => {
                result.push_str(&format!("VariableIdent: \"{}\"", ident));
            }
            ASTNode::If(bool_expr, if_case, else_case) => {
                result.push_str("If: ");
                bool_expr.print_inner(depth, result);
                for if_case_stmt in if_case {
                    if_case_stmt.print_inner(depth, result);
                }
                if let Some(else_case) = else_case {
                    for else_case_stmt in else_case {
                        else_case_stmt.print_inner(depth, result);
                    }
                }
            }
            ASTNode::Loop(astnodes) => {
                result.push_str("Loop");
                for stmt in astnodes {
                    stmt.print_inner(depth, result);
                }
            }
            ASTNode::NumberLiteral(num) => {
                result.push_str(&format!("NumberLiteral: {}", num));
            }
            ASTNode::StringLiteral(str) => {
                result.push_str(&format!("StringLiteral: {}", str));
            }
            ASTNode::Unary(op, astnode) => {
                result.push_str(&format!("Unary: {:?}", op));
                astnode.print_inner(depth, result);
            }
            ASTNode::UseDecl {
                module_ident,
                module_rename,
            } => {
                result.push_str(&format!("Use \"{}\"", module_ident));
                if let Some(mod_rename) = module_rename {
                    result.push_str(&format!(" {}", mod_rename));
                }
            }
            ASTNode::FnDecl { ident, body } => {
                result.push_str(&format!("FnDecl: {}", ident));
                for stmt in body {
                    stmt.print_inner(depth, result);
                }
            }
            ASTNode::StructDecl { ident, body } => {
                result.push_str(&format!("StructDecl: {}", ident));
                for field_decl in body {
                    result.push_str(&format!(
                        "{:?} \"{}\"",
                        field_decl.data_type, field_decl.ident
                    ));
                }
            }
            ASTNode::EndOfFile => {
                result.push_str("EOF");
            }
        }
    }
}

impl Display for ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.print())
    }
}
