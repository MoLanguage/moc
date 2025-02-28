use std::{iter::Peekable, mem};

use crate::{lexer::Lexer, Expr, Token};

#[derive(Debug)]
pub struct ParseError {
    msg: String,
    last_token: Option<Token>,
}

impl ParseError {
    fn new(msg: &str, last_token: &Option<Token>) -> Self {
        Self { msg: msg.into(), last_token: last_token.clone() }
    }
    
    fn wrap<T>(self) -> Result<T, ParseError> {
        Err(self)
    }
}

pub type ParseResult = Result<Expr, ParseError>;
pub struct Parser<'a> {
    token_stream: Peekable<Lexer<'a>>,
    current_token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let parser = Self {
            token_stream: lexer.peekable(),
            current_token: None,
        };
        parser
    }

    pub fn parse(&mut self) -> ParseResult {
        self.expression()
    }

    fn advance(&mut self) {
        self.current_token = self.token_stream.next();
    }

    fn peek(&mut self) -> Option<&Token> {
        self.token_stream.peek()
    }

    fn expression(&mut self) -> ParseResult {
        self.equality()
    }

    fn equality(&mut self) -> ParseResult {
        let mut expr = self.comparison()?;
        while self.matches(&[Token::NotEqualTo, Token::EqualTo]) {
            let right = self.comparison()?;
            expr = Expr::binary(expr, self.current_token.clone().unwrap(), right);
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> ParseResult {
        let mut expr = self.term()?;
        let tokens = &[
            Token::Greater,
            Token::GreaterOrEqual,
            Token::Less,
            Token::LessOrEqual,
        ];
        while self.matches(tokens) {
            let operator = self
                .current_token
                .clone()
                .expect("Operator should be here.");
            let right = self.term()?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    fn term(&mut self) -> ParseResult {
        let mut expr = self.factor()?;
        while self.matches(&[Token::Minus, Token::Plus]) {
            let operator = self
                .current_token
                .clone()
                .expect("Operator should be here.");
            let right = self.factor()?;
            expr = Expr::binary(expr, operator, right)
        }
        Ok(expr)
    }

    fn factor(&mut self) -> ParseResult {
        let mut expr = self.unary()?;
        while self.matches(&[Token::Slash, Token::Star]) {
            let operator = self
                .current_token
                .clone()
                .expect("Operator should be here.");
            let right = self.unary()?;
            expr = Expr::binary(expr, operator, right)
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult {
        while self.matches(&[Token::Excl, Token::Minus]) {
            let operator = self
                .current_token
                .clone()
                .expect("Operator should be here.");
            let right = self.primary()?;
            return Ok(Expr::unary(operator, right));
        }
        self.primary()
    }
    // literals and function calls
    fn primary(&mut self) -> ParseResult {
        if self.matches(&[Token::True]) {
            return Ok(Expr::BoolLiteral(true));
        }
        if self.matches(&[Token::False]) {
            return Ok(Expr::BoolLiteral(false));
        }
        if self.matches(&[Token::string_literal("")]) { // ugly lmao
            if let Token::StringLiteral(literal) = self.current_token.clone().expect("Token should be here.") {
                return Ok(Expr::StringLiteral(literal))
            }
            unreachable!()
        }
        if self.matches(&[Token::NumberLiteral("".into())]) { // ugly lmao
            if let Token::NumberLiteral(literal) = self.current_token.clone().expect("Token should be here.") {
                return Ok(Expr::NumberLiteral(literal))
            }
            unreachable!()
        }
        if self.matches(&[Token::OpenParen]) {
            let expr = self.expression()?;
            self.consume(&Token::CloseParen, "Expected '(' after expression.")?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }
        ParseError::new("Expected an expression", &self.token_stream.peek().cloned()).wrap()
    }
    /// checks if next token is one of the given tokens, then moves on to that token
    fn matches(&mut self, tokens: &[Token]) -> bool {
        for token in tokens {
            if self.is_next_of_type(token) {
                self.advance();
                return true;
            }
        }
        false
    }

    // checks if the following token is of the type of the given token.
    fn is_next_of_type(&mut self, token: &Token) -> bool {
        if let Some(current_token) = self.peek() {
            // checks if enum instances are of same variant
            if mem::discriminant(token) == mem::discriminant(current_token) {
                return true;
            }
        }
        false
    }

    // if next token is of given type, advances. If not, return an error.
    fn consume(&mut self, token: &Token, msg: &str) -> Result<(), ParseError> {
        if self.is_next_of_type(token) {
            self.advance();
            return Ok(());
        }
        Err(ParseError::new(msg, &self.peek().cloned()))
    }
}