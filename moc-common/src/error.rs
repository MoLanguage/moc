use std::io;

use crate::{expr::Expr, stmt::Stmt, token::Token, CodeLocation};

pub enum CompilerError {
    ParserError(ParserError),
    LexerError(LexerError),
    FileNotFound(io::Error)
}

#[derive(Debug)]
pub enum LexerError {
    UnterminatedStringLiteral(CodeLocation),
    InvalidCharacter(char),
    UnknownEscapeCharacter,
    UnknownToken,
}

pub type LexerResult = Result<Token, LexerError>;
pub type ParseResult = Result<Vec<Stmt>, ParserError>;
pub type ExprParseResult = Result<Expr, ParserError>;

#[derive(Debug)]
pub struct ParserError {
    pub msg: String,
    pub last_token: Option<Token>,
}

impl ParserError {
    pub fn new(msg: &str, last_token: Option<Token>) -> Self {
        Self {
            msg: msg.into(),
            last_token,
        }
    }

    pub fn wrap<T>(self) -> Result<T, ParserError> {
        Err(self)
    }
}