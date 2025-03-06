use std::fmt::{Display, format};

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
    Percent,
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
    pub fn expect_value(&self) -> String {
        self.value.as_ref().expect("Expected value").clone()
    }
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

pub enum Stmt {
    Print(Box<Expr>), // probably dont wanna have this as inbuilt function
    VarDecl {
        ident: String,
        value: Box<Expr>,
    },
    Break,
    UseDecl {
        module_ident: String,
        module_rename: Option<String>,
    },
    FnDecl {
        ident: String,
        params: Vec<TypedVar>,
        return_type: Option<String>,
        body: CodeBlock,
    },
    If {
        condition: Box<Expr>,
        if_block: Vec<Expr>,
        else_block: Option<Vec<Expr>>,
    }, // boolean expr, if-case, optional else-case
    Ret(Expr), // return statement
    StructDecl {
        ident: String,
        body: Vec<TypedVar>,
    },
}

#[derive(Debug)]
pub struct TypedVar {
    /// the type identifier
    type_ident: String,
    /// the identifier of the actual variable} 
    ident: String, 
}

impl TypedVar {
    fn new(data_type: String, ident: String) -> Self {
        Self { type_ident: data_type, ident }
    }
}

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
        s.push_str("AST: ");
        self.print_inner(0, &mut s);
        return s;
    }

    fn print_inner(&self, depth: usize, result: &mut String) {
        const INDENT: &str = "  ";
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

#[test]
fn token_type_eq() {
    assert!(TokenType::At == TokenType::At)
}
