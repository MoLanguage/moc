pub mod lexer;
pub mod parser;

#[derive(Debug, Clone)]
pub enum Token {
    Ident(String),
    NumberLiteral(String),
    StringLiteral(String),
    True,
    False,
    Excl,
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
    EndOfStatement, // change to semicolon and \n
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
}

impl Token {
    fn string_literal(literal: &str) -> Self {
        Self::StringLiteral(literal.into())
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
