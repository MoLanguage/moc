use std::iter::Peekable;

use crate::{CodeBlock, Expr, Stmt, Token, TokenType, TypedVar, lexer::Lexer};

#[derive(Debug)]
pub struct ParseError {
    msg: String,
    last_token: Option<Token>,
}

impl ParseError {
    fn new(msg: &str, last_token: Option<&Token>) -> Self {
        Self {
            msg: msg.into(),
            last_token: last_token.and_then(|t| Some(t.clone())),
        }
    }

    fn wrap<T>(self) -> Result<T, ParseError> {
        Err(self)
    }
}

pub type ParseResult = Result<Vec<Stmt>, ParseError>;

pub type ExprParseResult = Result<Expr, ParseError>;
pub struct Parser<'a> {
    token_stream: Peekable<Lexer<'a>>,
    current_token: Option<Token>,
    stmts: Vec<Stmt>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let parser = Self {
            token_stream: lexer.peekable(),
            current_token: None,
            stmts: Vec::new(),
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
        if let Some(token) = self.token_stream.peek() {
            if token.r#type != TokenType::EndOfFile {
                self.current_token = self.token_stream.next();
                //dbg!(&self.current_token);
            }
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.token_stream.peek()
    }

    pub fn parse(mut self) -> ParseResult {
        let borrowed = &mut self;
        borrowed.parse_top_level_stmts()?;
        Ok(self.stmts)
    }

    /// Parses the top level statements that are in a .mo code file also called "Items".
    fn parse_top_level_stmts(&mut self) -> Result<(), ParseError> {
        while let Some(token) = self.peek() {
            match token.r#type {
                TokenType::Use => {
                    self.parse_use_stmt()?;
                }
                TokenType::Struct => {
                    self.parse_struct_decl()?;
                }
                TokenType::Fn => {
                    self.parse_fn_decl()?;
                }
                _ => return Err(ParseError::new("Unexpected token", Some(token))),
            }
        }
        Ok(())
    }

    // use io | use io "foo"
    fn parse_use_stmt(&mut self) -> Result<(), ParseError> {
        self.advance();
        self.try_consume_token(TokenType::Ident, "Expected module identifier")?;
        //let identifier = self.current_token.as_ref().and_then(|t| t.value()).unwrap().clone();
        let identifier = self.current_token().expect_value();
        let stmt;
        if self.matches(TokenType::StringLiteral) {
            stmt = Stmt::UseDecl {
                module_ident: identifier,
                module_rename: Some(self.current_token().expect_value()),
            };
        } else {
            stmt = Stmt::UseDecl {
                module_ident: identifier,
                module_rename: None,
            };
        }
        self.stmts.push(stmt);

        Ok(())
    }

    fn parse_fn_decl(&mut self) -> Result<(), ParseError> {
        if self.matches(TokenType::Fn) {
            self.try_consume_token(TokenType::Ident, "Expected identifier")?;
            let fn_ident = self.current_token().expect_value();
            self.try_consume_token(TokenType::OpenParen, "Expected open parenthesis")?;

            // parse parameters
            let mut params = Vec::new();
            if !self.matches(TokenType::CloseParen) {
                loop {
                    self.try_consume_token(TokenType::Ident, "Expected type identifier")?;
                    let type_ident = self.current_token().expect_value();
                    self.try_consume_token(TokenType::Ident, "Expected variable identifier")?;
                    let var_ident = self.current_token().expect_value();
                    params.push(TypedVar::new(type_ident, var_ident));

                    if self.matches(TokenType::CloseParen) {
                        break;
                    }
                    if self.matches(TokenType::Comma) {
                        continue;
                    }
                    return ParseError::new(
                        "Expected argument delimiter ',' or closed parenthesis ')'",
                        self.peek(),
                    )
                    .wrap();
                }
            }
            // parse return type
            let mut return_type = None;
            if self.matches(TokenType::Ident) {
                return_type = Some(self.current_token().expect_value());
            }
            let body = self.parse_code_block()?;
            self.stmts.push(Stmt::FnDecl {
                ident: fn_ident,
                params: vec![],
                return_type,
                body,
            });
        } else {
            self.parse_struct_decl()?
        }
        Ok(())
    }

    fn parse_code_block(&mut self) -> Result<CodeBlock, ParseError> {
        self.try_consume_token(TokenType::OpenBrace, "Expected open brace")?;
        // Variable Decl
        // if(-else) statement
        // if is statement (switch or match)
        // function call
        // return statement
        if let Some(token) = self.peek() {}
        todo!()
    }

    fn parse_ret_stmt(&mut self) -> Result<(), ParseError> {
        if self.matches(TokenType::Ret) {
            let expr = self.parse_expression()?;
            self.stmts.push(Stmt::Ret(expr));
        }
        Ok(())
    }

    fn parse_struct_decl(&mut self) -> Result<(), ParseError> {
        self.advance();
        todo!()
    }

    fn parse_expression(&mut self) -> ExprParseResult {
        self.parse_equality_expr()
    }

    // a != b   a == b
    fn parse_equality_expr(&mut self) -> ExprParseResult {
        let mut expr = self.parse_comparison_expr()?;
        while self.matches_any(&[TokenType::NotEqualTo, TokenType::EqualTo]) {
            let right = self.parse_comparison_expr()?;
            expr = Expr::binary(expr, self.current_token.clone().unwrap(), right);
        }
        Ok(expr)
    }

    // a > b   a >= b   a < b   a <= b
    fn parse_comparison_expr(&mut self) -> ExprParseResult {
        let mut expr = self.parse_term_expr()?;
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
            let right = self.parse_term_expr()?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    // a - b   a + b
    fn parse_term_expr(&mut self) -> ExprParseResult {
        let mut expr = self.parse_factor_expr()?;
        while self.matches_any(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self
                .current_token
                .clone()
                .expect("Operator should be here.");
            let right = self.parse_factor_expr()?;
            expr = Expr::binary(expr, operator, right)
        }
        Ok(expr)
    }

    // a / b   a * b   a % b
    fn parse_factor_expr(&mut self) -> ExprParseResult {
        let mut expr = self.parse_unary_expr()?;
        while self.matches_any(&[TokenType::Slash, TokenType::Star, TokenType::Percent]) {
            let operator = self
                .current_token
                .clone()
                .expect("Operator should be here.");
            let right = self.parse_unary_expr()?;
            expr = Expr::binary(expr, operator, right)
        }
        Ok(expr)
    }

    // !a   -a
    fn parse_unary_expr(&mut self) -> ExprParseResult {
        while self.matches_any(&[TokenType::Excl, TokenType::Minus]) {
            let operator = self
                .current_token
                .clone()
                .expect("Operator should be here.");
            let right = self.parse_primary_expr()?;
            return Ok(Expr::unary(operator, right));
        }
        self.parse_primary_expr()
    }
    // literals and function calls
    fn parse_primary_expr(&mut self) -> ExprParseResult {
        if self.matches(TokenType::True) {
            return Ok(Expr::BoolLiteral(true));
        }
        if self.matches(TokenType::False) {
            return Ok(Expr::BoolLiteral(false));
        }
        if self.matches(TokenType::StringLiteral) {
            return Ok(Expr::StringLiteral(self.current_token().expect_value()));
        }
        if self.matches(TokenType::NumberLiteral) {
            return Ok(Expr::NumberLiteral(self.current_token().expect_value()));
        }
        if self.matches(TokenType::OpenParen) {
            let expr = self.parse_expression()?;
            self.try_consume_token(TokenType::CloseParen, "Expected '(' after expression.")?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        ParseError::new("Expected an expression", self.peek()).wrap()
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
            if token_type == current_token.r#type && token_type != TokenType::EndOfFile {
                return true;
            }
        }
        false
    }

    // if next token is of given type, advances. If not, return an error with given message.
    fn try_consume_token(&mut self, token_type: TokenType, msg: &str) -> Result<(), ParseError> {
        if self.is_next_of_type(token_type) {
            self.advance();
            return Ok(());
        }
        Err(ParseError::new(msg, self.peek()))
    }
}
