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
    BoolLiteral(bool),
    Break,
    FnDef(String, Vec<String>, Vec<Expr>),
    Grouping(Box<Expr>),
    Ident(String),
    If(Box<Expr>, Vec<Expr>, Option<Vec<Expr>>), // boolean expr, if-case, optional else-case
    Loop(Vec<Expr>),                             // simple infinite loop
    NumberLiteral(String),
    Print(Box<Expr>),
    StringLiteral(String),
    Unary(Token, Box<Expr>), // Operator followed by another expr
    Use(String, Option<String>), // module name, optional second string for module renaming
    VarDecl(String, Box<Expr>),
}

impl Expr {
    pub fn binary(left: Self, operator: Token, right: Self) -> Self {
        Self::Binary(Box::new(left), operator, Box::new(right))
    }
    pub fn grouping(expr: Self) -> Self {
        Self::Grouping(Box::new(expr))
    }
    pub fn unary(operator: Token, right: Self) -> Self {
        Self::Unary(operator, Box::new(right))
    }
}
