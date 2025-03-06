use std::iter::Peekable;

use crate::{ASTNode, Token, TokenType, lexer::Lexer};

#[derive(Debug)]
pub struct ParseError {
    msg: String,
    last_token: Option<Token>,
}

impl ParseError {
    fn new(msg: &str, last_token: &Option<Token>) -> Self {
        Self {
            msg: msg.into(),
            last_token: last_token.clone(),
        }
    }

    fn wrap<T>(self) -> Result<T, ParseError> {
        Err(self)
    }
}

pub type ParseResult = Result<ASTNode, ParseError>;
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

    /// Gets and clones current token.
    /// # Panics
    /// If current token is None
    fn current_token(&self) -> Token {
        self.current_token.clone().unwrap()
    }

    fn advance(&mut self) {
        self.current_token = self.token_stream.next();
    }

    fn peek(&mut self) -> Option<&Token> {
        self.token_stream.peek()
    }

    pub fn parse(&mut self) -> ParseResult {
        self.use_stmt()
    }

    pub fn parse_item(&mut self) -> ParseResult {
        todo!()
    }

    // use io | use io "foo"
    fn use_stmt(&mut self) -> ParseResult {
        if self.matches(TokenType::Use) {
            self.consume(TokenType::Ident, "Expected module identifier")?;
            //let identifier = self.current_token.as_ref().and_then(|t| t.value()).unwrap().clone();
            let identifier = self.current_token().value.unwrap();
            if self.matches(TokenType::StringLiteral) {
                return Ok(ASTNode::UseDecl {
                    module_ident: identifier,
                    module_rename: Some(self.current_token().value().unwrap().clone()),
                });
            } else {
                return Ok(ASTNode::UseDecl {
                    module_ident: identifier,
                    module_rename: None,
                });
            }
        } else {
            self.fn_decl()
        }
    }

    fn fn_decl(&mut self) -> ParseResult {
        if self.matches(TokenType::Fn) {
            todo!()
        } else {
            self.struct_decl()
        }
    }

    fn struct_decl(&mut self)  -> ParseResult {
        if self.matches(TokenType::Struct) {
            todo!()
        } else {
            self.expression()
        }
    }

    fn expression(&mut self) -> ParseResult {
        self.equality()
    }

    // a != b   a == b
    fn equality(&mut self) -> ParseResult {
        let mut expr = self.comparison()?;
        while self.matches_any(&[TokenType::NotEqualTo, TokenType::EqualTo]) {
            let right = self.comparison()?;
            expr = ASTNode::binary(expr, self.current_token.clone().unwrap(), right);
        }
        Ok(expr)
    }

    // a > b   a >= b   a < b   a <= b
    fn comparison(&mut self) -> ParseResult {
        let mut expr = self.term()?;
        let tokens = &[
            TokenType::Greater,
            TokenType::GreaterOrEqual,
            TokenType::Less,
            TokenType::LessOrEqual,
        ];
        while self.matches_any(tokens) {
            let operator = self
                .current_token
                .clone()
                .expect("Operator should be here.");
            let right = self.term()?;
            expr = ASTNode::binary(expr, operator, right);
        }
        Ok(expr)
    }

    // a - b   a + b
    fn term(&mut self) -> ParseResult {
        let mut expr = self.factor()?;
        while self.matches_any(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self
                .current_token
                .clone()
                .expect("Operator should be here.");
            let right = self.factor()?;
            expr = ASTNode::binary(expr, operator, right)
        }
        Ok(expr)
    }

    // a / b   a * b
    fn factor(&mut self) -> ParseResult {
        let mut expr = self.unary()?;
        while self.matches_any(&[TokenType::Slash, TokenType::Star]) {
            let operator = self
                .current_token
                .clone()
                .expect("Operator should be here.");
            let right = self.unary()?;
            expr = ASTNode::binary(expr, operator, right)
        }
        Ok(expr)
    }

    // !a   -a
    fn unary(&mut self) -> ParseResult {
        while self.matches_any(&[TokenType::Excl, TokenType::Minus]) {
            let operator = self
                .current_token
                .clone()
                .expect("Operator should be here.");
            let right = self.primary()?;
            return Ok(ASTNode::unary(operator, right));
        }
        self.primary()
    }
    // literals and function calls
    fn primary(&mut self) -> ParseResult {
        if self.matches(TokenType::True) {
            return Ok(ASTNode::BoolLiteral(true));
        }
        if self.matches(TokenType::False) {
            return Ok(ASTNode::BoolLiteral(false));
        }
        if self.matches(TokenType::StringLiteral) {
            if let Some(literal) = self.current_token.as_ref().and_then(|t| t.value.clone()) {
                return Ok(ASTNode::StringLiteral(literal));
            }
            unreachable!() // we're checking if the token is a literal so this should never be reached
        }
        if self.matches(TokenType::NumberLiteral) {
            if let Some(literal) = self.current_token.as_ref().and_then(|t| t.value.clone()) {
                return Ok(ASTNode::NumberLiteral(literal));
            }
            unreachable!() // we're checking if the token is a literal so this should never be reached
        }
        if self.matches(TokenType::OpenParen) {
            let expr = self.expression()?;
            self.consume(TokenType::CloseParen, "Expected '(' after expression.")?;
            return Ok(ASTNode::Grouping(Box::new(expr)));
        }
        if self.matches(TokenType::EndOfFile) {
            return Ok(ASTNode::EndOfFile);
        }
        ParseError::new("Expected an expression", &self.token_stream.peek().cloned()).wrap()
    }
    /// checks if next token is of one of the given types, then moves on to that token
    fn matches_any(&mut self, tokens: &[TokenType]) -> bool {
        for token in tokens {
            if self.is_next_of_type(*token) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn matches(&mut self, token: TokenType) -> bool {
        if self.is_next_of_type(token) {
            self.advance();
            return true;
        }
        false
    }

    // checks if the following token is of the given type.
    fn is_next_of_type(&mut self, token_type: TokenType) -> bool {
        if let Some(current_token) = self.peek() {
            if token_type == current_token._type {
                return true;
            }
        }
        false
    }

    // if next token is of given type, advances. If not, return an error.
    fn consume(&mut self, token_type: TokenType, msg: &str) -> Result<(), ParseError> {
        if self.is_next_of_type(token_type) {
            self.advance();
            return Ok(());
        }
        Err(ParseError::new(msg, &self.peek().cloned()))
    }
}
