use std::fmt::Display;

use derive_more::Display;
use serde::Serialize;

use crate::{BinaryOp, CodeSpan};

#[macro_export]
macro_rules! token {
    ($token_kind:ident, $start:expr, $end:expr) => {
        $crate::token::Token::new($crate::token::TokenKind::$token_kind, CodeSpan::from(($start, $end)))
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Token {
    pub kind: TokenKind,
    pub value: Option<String>,
    pub span: CodeSpan,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(value) = self.value() {
            write!(f, "{} from {} to {}, value: \"{}\"", self.kind, self.span.start, self.span.end, value.escape_default())
        } else {
            write!(f, "{} from {} to {}", self.kind, self.span.start, self.span.end)
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Display, Serialize)]
pub enum TokenKind {
    AddAssign,           // +=
    Ampersand,           // &
    Equals,              // =
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
    OpenBrack,
    CloseBrack,
    Colon,
    Comma,
    DeclareAssign, // :=
    Defer,
    DivAssign, // /=
    Dot,
    DoubleEquals,
    Excl, // !
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
    ModulePath,
    MultAssign,
    ExclEquals,
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

    pub fn get_token_type(&self) -> TokenKind {
        match self {
            NumberLiteralType::DecimalInteger => TokenKind::DecimalIntegerNumberLiteral,
            NumberLiteralType::DecimalPoint => TokenKind::DecimalPointNumberLiteral,
            NumberLiteralType::BinaryInteger => TokenKind::BinaryIntegerNumberLiteral,
            NumberLiteralType::OctalInteger => TokenKind::OctalIntegerNumberLiteral,
            NumberLiteralType::HexadecimalInteger => TokenKind::HexadecimalIntegerNumberLiteral,
        }
    }
}

impl Token {
    pub fn string_literal(literal: String, span: CodeSpan) -> Self {
        Self {
            kind: TokenKind::StringLiteral,
            value: Some(literal.into()),
            span,
        }
    }
    pub fn ident(ident: String, span: CodeSpan) -> Self {
        Self {
            kind: TokenKind::Ident,
            value: Some(ident.into()),
            span,
        }
    }
    pub fn module_path(ident: String, span: CodeSpan) -> Self {
        Self {
            kind: TokenKind::ModulePath,
            value: Some(ident.into()),
            span,
        }
    }
    pub fn integer(value: String, span: CodeSpan) -> Self {
        Self {
            kind: TokenKind::DecimalIntegerNumberLiteral,
            value: Some(value),
            span,
        }
    }
    pub fn number_literal(value: String, number_literal_type: NumberLiteralType, span: CodeSpan) -> Self {
        Self { kind: number_literal_type.get_token_type(), value: Some(value), span }
    }
    pub fn new(r#type: TokenKind, span: CodeSpan) -> Self {
        Self {
            kind: r#type,
            value: None,
            span,
        }
    }
    pub fn with_value(r#type: TokenKind, value: String, span: CodeSpan) -> Self {
        Self { kind: r#type, value: Some(value), span }
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
        match self.kind {
            TokenKind::AddAssign
            | TokenKind::SubAssign
            | TokenKind::MultAssign
            | TokenKind::DivAssign
            | TokenKind::BitAndAssign
            | TokenKind::BitXorAssign
            | TokenKind::BitOrAssign => true,
            _ => false,
        }
    }

    pub fn is_binary_op(&self) -> bool {
        TryInto::<BinaryOp>::try_into(self.kind).is_ok()
    }

    pub fn is_of_type(&self, r#type: TokenKind) -> bool {
        self.kind == r#type
    }

    pub fn is_of_types(&self, types: &[TokenKind]) -> bool {
        for r#type in types {
            if self.is_of_type(*r#type) {
                return true;
            }
        }
        false
    }

    pub fn is_number_literal(&self) -> bool {
        TryInto::<NumberLiteralType>::try_into(self.kind).is_ok()
    }
}

impl From<TokenKind> for Token {
    fn from(r#type: TokenKind) -> Self {
        Self { kind: r#type, value: None, span: CodeSpan::default() }
    }
}

impl From<NumberLiteralType> for TokenKind {
    fn from(value: NumberLiteralType) -> Self {
        value.get_token_type()
    }
}

#[derive(Debug)]
pub struct NonNumberLiteralTokenTypeError;
impl TryFrom<TokenKind> for NumberLiteralType {
    type Error = NonNumberLiteralTokenTypeError;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::DecimalIntegerNumberLiteral => Ok(NumberLiteralType::DecimalInteger),
            TokenKind::DecimalPointNumberLiteral => Ok(NumberLiteralType::DecimalPoint),
            TokenKind::BinaryIntegerNumberLiteral => Ok(NumberLiteralType::BinaryInteger),
            TokenKind::OctalIntegerNumberLiteral => Ok(NumberLiteralType::OctalInteger),
            TokenKind::HexadecimalIntegerNumberLiteral => Ok(NumberLiteralType::HexadecimalInteger),
            _ => Err(NonNumberLiteralTokenTypeError)
        }
    }
}

#[derive(Debug)]
pub struct NonOperatorTokenError; // error if token is a non-operator token
impl TryFrom<TokenKind> for BinaryOp {
    type Error = NonOperatorTokenError;

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::AddAssign | TokenKind::Plus => Ok(BinaryOp::Add),
            TokenKind::SubAssign | TokenKind::Minus => Ok(BinaryOp::Sub),
            TokenKind::MultAssign | TokenKind::Star => Ok(BinaryOp::Mult),
            TokenKind::DivAssign | TokenKind::Slash => Ok(BinaryOp::Div),
            TokenKind::ModAssign | TokenKind::Percent => Ok(BinaryOp::Mod),
            TokenKind::BitAndAssign | TokenKind::Ampersand => Ok(BinaryOp::BitAnd),
            TokenKind::BitXorAssign | TokenKind::Caret => Ok(BinaryOp::BitXor),
            TokenKind::BitOrAssign | TokenKind::Pipe => Ok(BinaryOp::BitOr),
            TokenKind::Tilde | TokenKind::BitNotAssign => Ok(BinaryOp::BitNot),
            TokenKind::BitShiftLeftAssign | TokenKind::BitShiftLeft => Ok(BinaryOp::BitShiftLeft),
            TokenKind::BitShiftRightAssign | TokenKind::BitShiftRight => {
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
        assert!(token.is_of_type(TokenKind::Plus));
        assert!(token.is_of_types(&[TokenKind::Plus, TokenKind::Minus]));
        assert!(token.is_of_types(&[TokenKind::Minus, TokenKind::Plus]));
    }
}
