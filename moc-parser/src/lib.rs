pub mod lexer;
pub mod parser;

#[derive(Debug, Clone)]
pub struct Token {
    pub _type: TokenType,
    value: Option<String>,
    pub location: CodeLocation
}

#[derive(Debug, Clone, Copy)]
pub struct CodeLocation {
    line: u32, 
    column: u32
}

impl Default for CodeLocation {
    fn default() -> Self {
        Self { line: 1, column: 1 }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Ident,
    NumberLiteral,
    StringLiteral,
    True,
    False,
    Excl,
    At,
    Comma,
    Dot,
    For,
    In,
    NotEqualTo,
    EqualTo,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
    Plus,
    Minus,
    Slash,
    Star,
    Semicolon,
    Mod,
    AddAssign,
    SubAssign,
    DivAssign,
    MultAssign,
    ModAssign,
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    Colon,
    LineBreak, // encompassing CRLF and LF in one token.
    Declare,
    Assign,
    Struct,
    Sum,
    Use,
    Ret,
    If,
    Is,
    Else,
    Loop,
    Break,
    Fn,
    Print,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    BitAnd,
    BitOr,
    BitXor, // replace with proper function call statement
    EndOfFile
}

impl Token {
    pub fn string_literal(literal: String, location: CodeLocation) -> Self {
        Self { _type: TokenType::StringLiteral, value: Some(literal.into()), location }
    }
    pub fn new_ident(ident: String, location: CodeLocation) -> Self {
        Self { _type: TokenType::Ident, value: Some(ident.into()), location }
    }
    pub fn number_literal(literal: String, location: CodeLocation) -> Self {
        Self { _type: TokenType::NumberLiteral, value: Some(literal.into()), location }
    }
    pub fn new(_type: TokenType, location: CodeLocation) -> Self {
        Self { _type, value: None, location }
    }
    pub fn value(&self) -> Option<&String> {
        self.value.as_ref()
    }
}

#[derive(Debug)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Unary(Token, Box<Expr>), // Operator followed by another expr
    Grouping(Box<Expr>),
    NumberLiteral(String),
    StringLiteral(String),
    BoolLiteral(bool),
    VarDecl(String, Box<Expr>),
    Ident(String),
    If(Box<Expr>, Vec<Expr>, Option<Vec<Expr>>), // boolean expr, if-case, optional else-case
    Loop(Vec<Expr>),                             // simple infinite loop
    Break,
    FnDef(String, Vec<String>, Vec<Expr>),
    Print(Box<Expr>),
    Use(String, Option<String>), // module name, optional second string for module renaming
}

impl Expr {
    pub fn binary(left: Self, operator: Token, right: Self) -> Self {
        Self::Binary(Box::new(left), operator, Box::new(right))
    }
    pub fn unary(operator: Token, right: Self) -> Self {
        Self::Unary(operator, Box::new(right))
    }
    pub fn grouping(expr: Self) -> Self {
        Self::Grouping(Box::new(expr))
    }
}
