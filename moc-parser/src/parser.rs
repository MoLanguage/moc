use std::{iter::Peekable};

use crate::{CodeBlock, Expr, ModuleIdentifier, Stmt, Token, TokenType, TypedVar, lexer::Lexer};

#[derive(Debug)]
pub struct ParserError {
    pub msg: String,
    pub last_token: Option<Token>,
}

impl ParserError {
    fn new(msg: &str, last_token: Option<Token>) -> Self {
        Self {
            msg: msg.into(),
            last_token,
        }
    }

    fn wrap<T>(self) -> Result<T, ParserError> {
        Err(self)
    }
}

pub type ParseResult = Result<Vec<Stmt>, ParserError>;

pub type ExprParseResult = Result<Expr, ParserError>;
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

    // Int32 a
    //   ^
    /// Gets and clones current token.
    /// # Panics
    /// If current token is None
    fn current_token(&self) -> Token {
        self.current_token.clone().unwrap()
    }
    #[track_caller]
    fn advance(&mut self) {
        // maybe this is useless... maybe remove in lexing step?
        if let Some(_) = self.token_stream.peek() {
            //dbg!(std::panic::Location::caller());
            if let Some(current_token) = &self.current_token {
                println!("advancing from {}", current_token.r#type);
            }
            self.current_token = self.token_stream.next();

            // if current token is a line break and the before token was a line break, skip it.
            self.skip_tokens_of_same_type(TokenType::LineBreak);
            self.skip_tokens_of_same_type(TokenType::Semicolon);
        }
    }

    fn skip_tokens_of_same_type(&mut self, token_type: TokenType) {
        if self.current_token.as_ref().unwrap().r#type == token_type {
            if let Some(token) = self.token_stream.peek() {
                if token.r#type == token_type {
                    self.advance();
                }
            }
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        match self.token_stream.peek() {
            Some(token) => {
                if token.r#type == TokenType::EndOfFile {
                    None
                } else {
                    Some(token)
                }
            }
            None => None,
        }
    }

    pub fn parse(mut self) -> ParseResult {
        let borrowed = &mut self;
        borrowed.parse_top_level_stmts()?;
        Ok(self.stmts)
    }

    /// Parses the top level statements that are in a .mo code file also called "Items".
    fn parse_top_level_stmts(&mut self) -> Result<(), ParserError> {
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
                TokenType::LineBreak => {
                    self.advance();
                    continue; // continue for now
                }
                TokenType::EndOfFile => {
                    self.advance();
                    break;
                }
                _ => {
                    return Err(ParserError::new(
                        "Unexpected token parsing top-level statements",
                        Some(token.clone()),
                    ));
                }
            }
        }
        Ok(())
    }

    fn parse_module_identifier(&mut self) -> Result<ModuleIdentifier, ParserError> {
        let mut module_dirs = Vec::with_capacity(8);
        if self.matches(TokenType::Ident) {
            loop {
                module_dirs.push(self.current_token().unwrap_value());
                if self.matches(TokenType::Colon) {
                    if self.matches(TokenType::Ident) {
                        continue;
                    } else {
                        return ParserError::new(
                            "Expected module directory",
                            self.current_token.clone(),
                        )
                        .wrap();
                    }
                } else {
                    break;
                }
            }
        } else {
            return ParserError::new("Expected module identifier", self.current_token.clone())
                .wrap();
        }
        Ok(ModuleIdentifier(module_dirs))
    }
    // use io | use io "foo"
    // use io:print | s
    fn parse_use_stmt(&mut self) -> Result<(), ParserError> {
        self.advance();
        let identifier = self.parse_module_identifier()?;
        let stmt;
        if self.matches(TokenType::StringLiteral) {
            stmt = Stmt::UseDecl {
                module_ident: identifier,
                module_alias: Some(self.current_token().unwrap_value()),
            };
        } else {
            stmt = Stmt::UseDecl {
                module_ident: identifier,
                module_alias: None,
            };
        }
        self.stmts.push(stmt);

        Ok(())
    }

    fn parse_fn_decl(&mut self) -> Result<(), ParserError> {
        self.advance();
        
        self.try_consume_token(TokenType::Ident, "Expected function identifier")?;
        let fn_ident = self.current_token().unwrap_value();
        self.try_consume_token(TokenType::OpenParen, "Expected open parenthesis")?;

        // parse parameters
        let mut params = Vec::new();
        if !self.matches(TokenType::CloseParen) {
            loop {
                self.try_consume_token(TokenType::Ident, "Expected type identifier")?;
                let type_ident = self.current_token().unwrap_value();

                self.try_consume_token(TokenType::Ident, "Expected variable identifier")?;
                let var_ident = self.current_token().unwrap_value();

                params.push(TypedVar::new(type_ident, var_ident));

                if self.matches(TokenType::CloseParen) {
                    break;
                }
                if self.matches(TokenType::Comma) {
                    continue;
                }
                return ParserError::new(
                    "Expected argument delimiter ',' or closed parenthesis ')'",
                    self.peek().cloned(),
                )
                .wrap();
            }
        }
        // parse return type
        let mut return_type = None;
        if self.matches(TokenType::Ident) {
            return_type = Some(self.current_token().unwrap_value());
        }
        // parse body / code
        let body = self.parse_code_block()?;
        self.stmts.push(Stmt::FnDecl {
            ident: fn_ident,
            params,
            return_type,
            body,
        });

        Ok(())
    }

    fn parse_code_block(&mut self) -> Result<CodeBlock, ParserError> {
        println!("parsing code block");
        self.try_consume_token(TokenType::OpenBrace, "Expected open brace")?;
        self.skip_tokens_of_same_type(TokenType::LineBreak); // move on if there's a linebreak.

        // what can we expect within a code block?
        // variable declaration
        // if statement
        // for loop
        // function call
        // return statement

        // what tokens can they each begin with?
        // variable declaration: type identifier
        // if statement: if
        // for loop: for
        // function call: identifier
        // return statement: ret

        let mut code_block = CodeBlock::new();
        loop {
            if let Some(token) = self.peek().cloned() {
                match token.r#type {
                    // Problem to solve: Is a simple function call a statement or an expression?
                    // We omit function call expressions for now.
                    TokenType::Ident => {
                        // ambiguity: function call vs variable declaration
                        self.advance(); // advancing to be able to check if next token is a open parenthesis
                        let ident = token.unwrap_value(); // save the identifier value
                        if self.matches(TokenType::OpenParen) {
                            let stmt = self.parse_fn_call(ident)?;
                            code_block.stmts.push(stmt);
                        } else {
                            let stmt = self.parse_var_decl_assignmt()?;
                            code_block.stmts.push(stmt);
                        }
                    }
                    TokenType::For => code_block.stmts.push(self.parse_for_loop()?),
                    TokenType::If => code_block.stmts.push(self.parse_if_else_stmt()?),
                    TokenType::Ret => code_block.stmts.push(self.parse_ret_stmt()?),
                    TokenType::CloseBrace => {
                        self.advance();
                        break;
                    }
                    TokenType::LineBreak => {
                        self.advance();
                        continue;
                    }
                    _ => {
                        return ParserError::new(
                            "Unexpected token parsing code block",
                            Some(token),
                        )
                        .wrap();
                    }
                }
            } else {
                break;
            }
        }

        // DONE Variable Decl
        // if(-else) statement
        // if is statement (switch or match)
        // loops
        // function call
        // return statement

        Ok(code_block)
    }

    /*
    Parses statements of this form:
    Int32 a := 10 | DeclAssignmt
    a := 10       | DeclAssignmt (no type identifier. type to be inferred)
    Int32 a       | Decl
    a = 10        | Assignmt
     */
    fn parse_var_decl_assignmt(&mut self) -> Result<Stmt, ParserError> {
        assert_eq!(self.current_token().r#type, TokenType::Ident);
        let ident1 = self.current_token().unwrap_value();
        if self.matches(TokenType::Ident) {
            let ident2 = self.current_token().unwrap_value();
            if self.matches(TokenType::DeclareAssign) {
                let expr = self.parse_expression()?;
                self.consume_line_terminator()?;
                return Ok(Stmt::VarDeclAssign {
                    type_ident: Some(ident1),
                    ident: ident2,
                    value: expr,
                });
            } else if self.matches_any(&[TokenType::LineBreak, TokenType::Semicolon]) {
                return Ok(Stmt::VarDecl {
                    type_ident: ident1,
                    var_ident: ident2,
                });
            }
        } else if self.matches(TokenType::Assign) {
            let expr = self.parse_expression()?;
            self.consume_line_terminator()?;
            return Ok(Stmt::VarAssign {
                ident: ident1,
                value: expr,
            });
        } else if self.matches(TokenType::DeclareAssign) {
            let expr = self.parse_expression()?;
            self.consume_line_terminator()?;
            return Ok(Stmt::VarDeclAssign {
                type_ident: None,
                ident: ident1,
                value: expr,
            });
        }
        ParserError::new(
            "Unexpected token parsing variable declaration/assigning",
            self.peek().cloned(),
        )
        .wrap()
    }

    fn consume_line_terminator(&mut self) -> Result<(), ParserError> {
        self.try_consume_token2(
            &[TokenType::LineBreak, TokenType::Semicolon],
            "Expected line break or ;",
        )
    }

    // when this is called, the current token is the function identifier
    fn parse_fn_call(&mut self, fn_ident: String) -> Result<Stmt, ParserError> {
        // parse arguments
        let mut args = Vec::new();
        if !self.matches(TokenType::CloseParen) {
            loop {
                let expr = self.parse_expression()?;
                args.push(expr);
                if self.matches(TokenType::CloseParen) {
                    break;
                }
                if self.matches(TokenType::Comma) {
                    continue;
                }
                return ParserError::new(
                    "Expected argument delimiter ',' or closed parenthesis ')'",
                    self.peek().cloned(),
                )
                .wrap();
            }
        }
        self.consume_line_terminator()?;
        let stmt = Stmt::FnCall(crate::FnCall {
            ident: fn_ident,
            args,
        });
        Ok(stmt)
    }

    /*
    for <bool expr> <code block>
     */
    fn parse_for_loop(&mut self) -> Result<Stmt, ParserError> {
        // parse for loop
        self.advance();
        let condition = self.parse_expression()?;
        let code_block = self.parse_code_block()?;
        let stmt = Stmt::ForLoop {
            condition,
            code_block,
        };
        Ok(stmt)
    }

    // if <bool expr> <code block>
    fn parse_if_else_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.advance();
        let condition = self.parse_expression()?;
        let if_block = self.parse_code_block()?;
        let mut else_block = None;
        if self.matches(TokenType::Else) {
            else_block = Some(self.parse_code_block()?);
        }
        Ok(Stmt::If {
            condition,
            if_block,
            else_block,
        })
    }

    fn parse_ret_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.advance();
        let expr = self.parse_expression()?;
        Ok(Stmt::Ret(expr))
    }

    /*
    Parse struct declaration of form:
    struct Foo {
        Int32 a
        Int32 b
    }
    unit struct:
    struct Foo {}
     */
    fn parse_struct_decl(&mut self) -> Result<Stmt, ParserError> {
        let stmt;
        self.advance();
        self.try_consume_token(TokenType::Ident, "Expected struct identifier")?;
        let struct_ident = self.current_token().unwrap_value();
        self.try_consume_token(TokenType::OpenBrace, "Expected open brace")?;
        let mut fields = Vec::new();
        loop {
            if self.matches(TokenType::CloseBrace) {
                break;
            }
            self.try_consume_token(TokenType::Ident, "Expected type identifier")?;
            let type_ident = self.current_token().unwrap_value();
            self.try_consume_token(TokenType::Ident, "Expected variable identifier")?;
            let var_ident = self.current_token().unwrap_value();
            fields.push(TypedVar::new(type_ident, var_ident));
            self.try_consume_token2(
                &[TokenType::LineBreak, TokenType::Semicolon],
                "Expected line break or ;",
            )?;
        }
        stmt = Stmt::StructDecl {
            ident: struct_ident,
            fields,
        };
        Ok(stmt)
    }

    // :Expressions

    fn parse_expression(&mut self) -> ExprParseResult {
        println!("parsing expression {}", self.parser_state_dbg_info());
        self.parse_equality_expr()
    }

    // a != b   a == b
    fn parse_equality_expr(&mut self) -> ExprParseResult {
        println!("parsing equality expr {}", self.parser_state_dbg_info());

        let mut expr = self.parse_comparison_expr()?;
        if self.matches_any(&[TokenType::NotEqualTo, TokenType::EqualTo]) {
            let operator = self.current_token();
            println!("equality expr: chosen operator {}", operator.r#type);
            let right = self.parse_comparison_expr()?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    // a > b   a >= b   a < b   a <= b
    fn parse_comparison_expr(&mut self) -> ExprParseResult {
        println!("parsing comparison expr {}", self.parser_state_dbg_info());
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
        println!("parsing term expression (- +) {}", self.parser_state_dbg_info());
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
        println!("Parsing factor expr (/ * %) {}", self.parser_state_dbg_info());
        let mut expr = self.parse_unary_expr()?;
        while self.matches_any(&[TokenType::Slash, TokenType::Star, TokenType::Percent]) {
            let operator = self
                .current_token
                .clone()
                .expect("Operator should be here.");
            let right = self.parse_unary_expr()?;
            println!("Ok, returning binary factor expr");
            expr = Expr::binary(expr, operator, right)
        }
        Ok(expr)
    }

    // !a   -a
    fn parse_unary_expr(&mut self) -> ExprParseResult {
        println!("parsing unary expr {}", self.parser_state_dbg_info());
        if self.matches_any(&[TokenType::Excl, TokenType::Minus]) {
            let operator = self
                .current_token
                .clone()
                .expect("Operator should be here.");
            let right = self.parse_primary_expr()?;
            println!("Ok, returning unary expr");
            return Ok(Expr::unary(operator, right));
        }
        self.parse_primary_expr()
    }
    // literals, variables and function calls
    fn parse_primary_expr(&mut self) -> ExprParseResult {
        println!("parsing primary expr {}", self.parser_state_dbg_info());
        //dbg!(&self.current_token);
        //dbg!(&self.peek());
        if self.matches(TokenType::Ident) {
            // todo: parse function call
            println!("Ok, returning variable ident expr");
            return Ok(Expr::VariableIdent(self.current_token().unwrap_value()));
        }
        if self.matches(TokenType::True) {
            println!("Ok, returning boolean literal expr");
            return Ok(Expr::BoolLiteral(true));
        }
        if self.matches(TokenType::False) {
            println!("Ok, returning boolean literal expr");
            return Ok(Expr::BoolLiteral(false));
        }
        if self.matches(TokenType::StringLiteral) {
            println!("Ok, returning string literal expr");
            return Ok(Expr::StringLiteral(self.current_token().unwrap_value()));
        }
        if self.matches(TokenType::NumberLiteral) {
            println!("Ok, returning number literal expr");
            return Ok(Expr::NumberLiteral(self.current_token().unwrap_value()));
        }
        if self.matches(TokenType::OpenParen) {
            let expr = self.parse_expression()?;
            self.try_consume_token(TokenType::CloseParen, "Expected '(' after expression.")?;
            println!("Ok, returning grouping expr");
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        ParserError::new("Expected an expression", self.peek().cloned()).wrap()
    }

    // :Utils
    /// checks if next token is of one of the given types, then moves on to that token
    fn matches_any(&mut self, tokens: &[TokenType]) -> bool {
        if self.is_next_of_types(tokens) {
            self.advance();
            return true;
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

    // checks if the following token is of one of the given types.
    fn is_next_of_types(&mut self, tokens: &[TokenType]) -> bool {
        for token in tokens {
            if self.is_next_of_type(*token) {
                return true;
            }
        }
        false
    }

    // if next token is of given type, advances. If not, return an error with given message.
    fn try_consume_token(&mut self, token_type: TokenType, msg: &str) -> Result<(), ParserError> {
        if self.is_next_of_type(token_type) {
            self.advance();
            return Ok(());
        }
        Err(ParserError::new(msg, self.peek().cloned()))
    }

    // if next token is of any of given types, advances. If not, return an error with given message.
    fn try_consume_token2(&mut self, tokens: &[TokenType], msg: &str) -> Result<(), ParserError> {
        if self.is_next_of_types(tokens) {
            self.advance();
            return Ok(());
        }
        Err(ParserError::new(msg, self.peek().cloned()))
    }
    
    fn parser_state_dbg_info(&mut self) -> String {
        let current = self.current_token();
        let line = current.location.line;
        let col = current.location.column;
        let r#type = current.r#type;
        let mut str = String::new();
        
        str.push_str(&format!("Current: \"{}\", loc: {}:{}", r#type, line, col)); 
        
        if let Some(next) = self.peek() {
            let line = next.location.line;
            let col = next.location.column;
            let r#type = next.r#type;
            str.push_str(&format!(" --- Next: \"{}\", loc: {}:{}", r#type, line, col));
        }
        str
    }
}
