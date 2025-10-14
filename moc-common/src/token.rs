use derive_more::Display;

use crate::{BinaryOp, CodeLocation};

#[derive(Debug, Clone)]
pub struct Token {
    pub r#type: TokenType,
    pub value: Option<String>,
    pub location: CodeLocation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Display)]
pub enum TokenType {
    AddAssign,           // +=
    Ampersand,           // &
    Assign,              // =
    At,                  // @
    BitAndAssign,        // &=
    BitOrAssign,         //  |=
    BitXorAssign,        // ^=
    BitNotAssign,        // ~=
    BitShiftLeft,        // <<
    BitShiftRight,       // >>
    BitShiftLeftAssign,  // <<=
    BitShiftRightAssign, // >>=
    Break,
    Caret, // ^
    CloseBrace,
    CloseParen,
    Colon,
    Comma,
    DeclareAssign, // :=
    Defer,
    DivAssign, // /=
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
    Pipe,
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
    Tilde, // ~
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

    pub fn is_operator_assign(&self) -> bool {
        match self.r#type {
            TokenType::AddAssign
            | TokenType::SubAssign
            | TokenType::MultAssign
            | TokenType::DivAssign
            | TokenType::BitAndAssign
            | TokenType::BitXorAssign
            | TokenType::BitOrAssign => true,
            _ => false,
        }
    }

    pub fn is_binary_op(&self) -> bool {
        TryInto::<BinaryOp>::try_into(self.r#type).is_ok()
    }
}

#[derive(Debug)]
pub struct NonOperatorTokenError; // error if token is a non-operator token
impl TryFrom<TokenType> for BinaryOp {
    type Error = NonOperatorTokenError;

    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::AddAssign | TokenType::Plus => Ok(BinaryOp::Add),
            TokenType::SubAssign | TokenType::Minus => Ok(BinaryOp::Sub),
            TokenType::MultAssign | TokenType::Star => Ok(BinaryOp::Mult),
            TokenType::DivAssign | TokenType::Slash => Ok(BinaryOp::Div),
            TokenType::BitAndAssign | TokenType::Ampersand => Ok(BinaryOp::BitAnd),
            TokenType::BitXorAssign | TokenType::Caret => Ok(BinaryOp::BitXor),
            TokenType::BitOrAssign | TokenType::Pipe => Ok(BinaryOp::BitOr),
            TokenType::Tilde | TokenType::BitNotAssign => Ok(BinaryOp::BitNot),
            TokenType::BitShiftLeftAssign | TokenType::BitShiftLeft => Ok(BinaryOp::BitShiftLeft),
            TokenType::BitShiftRightAssign | TokenType::BitShiftRight => {
                Ok(BinaryOp::BitShiftRight)
            }
            _ => Err(NonOperatorTokenError),
        }
    }
}