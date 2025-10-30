use std::fmt::Display;

use derive_more::Display;
use serde::Serialize;

use crate::{BinaryOp, CodeSpan};

#[macro_export]
macro_rules! token {
    ($token_type:ident, $start:expr, $end:expr) => {
        $crate::token::Token::new($crate::token::TokenType::$token_type, CodeSpan::from(($start, $end)))
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Token {
    pub r#type: TokenType,
    pub value: Option<String>,
    pub span: CodeSpan,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(value) = self.value() {
            write!(f, "{} from {} to {}, value: \"{}\"", self.r#type, self.span.start, self.span.end, value.escape_default())
        } else {
            write!(f, "{} from {} to {}", self.r#type, self.span.start, self.span.end)
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, Serialize)]
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
    ModIdent,
    MultAssign,
    NotEqualTo,
    DecimalIntegerNumberLiteral,
    DecimalPointNumberLiteral,
    HexadecimalIntegerNumberLiteral,
    OctalIntegerNumberLiteral,
    BinaryIntegerNumberLiteral,
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

#[derive(Debug, Clone, Display, Serialize)]
pub enum NumberLiteralType {
    DecimalInteger,
    DecimalPoint, // like floating point
    BinaryInteger,
    OctalInteger,
    HexadecimalInteger
}

impl NumberLiteralType {
    pub fn get_radix(&self) -> u32 {
        match self {
            NumberLiteralType::DecimalInteger => 10,
            NumberLiteralType::DecimalPoint => 10,
            NumberLiteralType::BinaryInteger => 2,
            NumberLiteralType::OctalInteger => 8,
            NumberLiteralType::HexadecimalInteger => 16,
        }
    }

    pub fn get_token_type(&self) -> TokenType {
        match self {
            NumberLiteralType::DecimalInteger => TokenType::DecimalIntegerNumberLiteral,
            NumberLiteralType::DecimalPoint => TokenType::DecimalPointNumberLiteral,
            NumberLiteralType::BinaryInteger => TokenType::BinaryIntegerNumberLiteral,
            NumberLiteralType::OctalInteger => TokenType::OctalIntegerNumberLiteral,
            NumberLiteralType::HexadecimalInteger => TokenType::HexadecimalIntegerNumberLiteral,
        }
    }
}

impl Token {
    pub fn string_literal(literal: String, span: CodeSpan) -> Self {
        Self {
            r#type: TokenType::StringLiteral,
            value: Some(literal.into()),
            span,
        }
    }
    pub fn ident(ident: String, span: CodeSpan) -> Self {
        Self {
            r#type: TokenType::Ident,
            value: Some(ident.into()),
            span,
        }
    }
    pub fn mod_ident(ident: String, span: CodeSpan) -> Self {
        Self {
            r#type: TokenType::ModIdent,
            value: Some(ident.into()),
            span,
        }
    }
    pub fn integer(value: String, span: CodeSpan) -> Self {
        Self {
            r#type: TokenType::DecimalIntegerNumberLiteral,
            value: Some(value),
            span,
        }
    }
    pub fn number_literal(value: String, number_literal_type: NumberLiteralType, span: CodeSpan) -> Self {
        Self { r#type: number_literal_type.get_token_type(), value: Some(value), span }
    }
    pub fn new(r#type: TokenType, span: CodeSpan) -> Self {
        Self {
            r#type,
            value: None,
            span,
        }
    }
    pub fn with_value(r#type: TokenType, value: String, span: CodeSpan) -> Self {
        Self { r#type, value: Some(value), span }
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

    pub fn is_of_type(&self, r#type: TokenType) -> bool {
        self.r#type == r#type
    }

    pub fn is_of_types(&self, types: &[TokenType]) -> bool {
        for r#type in types {
            if self.is_of_type(*r#type) {
                return true;
            }
        }
        false
    }

    pub fn is_number_literal(&self) -> bool {
        TryInto::<NumberLiteralType>::try_into(self.r#type).is_ok()
    }
}

impl From<TokenType> for Token {
    fn from(r#type: TokenType) -> Self {
        Self { r#type, value: None, span: CodeSpan::default() }
    }
}

impl From<NumberLiteralType> for TokenType {
    fn from(value: NumberLiteralType) -> Self {
        value.get_token_type()
    }
}

#[derive(Debug)]
pub struct NonNumberLiteralTokenTypeError;
impl TryFrom<TokenType> for NumberLiteralType {
    type Error = NonNumberLiteralTokenTypeError;

    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::DecimalIntegerNumberLiteral => Ok(NumberLiteralType::DecimalInteger),
            TokenType::DecimalPointNumberLiteral => Ok(NumberLiteralType::DecimalPoint),
            TokenType::BinaryIntegerNumberLiteral => Ok(NumberLiteralType::BinaryInteger),
            TokenType::OctalIntegerNumberLiteral => Ok(NumberLiteralType::OctalInteger),
            TokenType::HexadecimalIntegerNumberLiteral => Ok(NumberLiteralType::HexadecimalInteger),
            _ => Err(NonNumberLiteralTokenTypeError)
        }
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
            TokenType::ModAssign | TokenType::Percent => Ok(BinaryOp::Mod),
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

#[cfg(test)]
mod tests {
    use crate::CodeLocation;

    use super::*;

    #[test]
    fn is_of_type_s() {
        let token = token!(Plus, CodeLocation::default(), CodeLocation::default());
        assert!(token.is_of_type(TokenType::Plus));
        assert!(token.is_of_types(&[TokenType::Plus, TokenType::Minus]));
        assert!(token.is_of_types(&[TokenType::Minus, TokenType::Plus]));
    }
}
