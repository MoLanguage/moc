use std::{iter::Peekable, vec::IntoIter};

use log::debug;
use moc_common::{
    CodeBlock, ModuleIdentifier, TypedVar,
    ast::Ast,
    decl::Decl,
    error::{ExprParseResult, ParseResult, ParserError},
    expr::Expr,
    stmt::Stmt,
    token::{Token, TokenType},
};

pub struct Parser {
    token_stream: Peekable<IntoIter<Token>>,
    current_token: Option<Token>,
    ast: Ast,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let parser = Self {
            token_stream: tokens.into_iter().peekable(),
            current_token: None,
            ast: Ast::new(),
        };
        parser
    }

    // Int32 a
    //   ^
    /// Gets and clones current token.
    /// # Panics
    /// If current token is None
    fn unwrap_current_token(&self) -> Token {
        self.current_token.clone().unwrap()
    }

    fn current_token(&self) -> Option<Token> {
        self.current_token.clone()
    }

    #[track_caller]
    fn advance(&mut self) {
        self.current_token = self.token_stream.next();
        
        // Debug info:
        if let Some(current_token) = self.current_token() {
            if let Some(peeked_token) = self.token_stream.peek().cloned() {
                debug!(
                    "{:?} advanced from {}, peeking {}",
                    std::panic::Location::caller(),
                    current_token.r#type,
                    peeked_token.r#type
                );
            }
        }
    }

    /// Skips tokens that are of the same type as given.
    fn skip_tokens_of_type(&mut self, token_type: TokenType) {
        loop {
            if let Some(token) = self.peek() {
                if token.is_of_type(token_type) {
                    debug!("Skipping token: {}", token);
                    self.advance();
                } else {
                    break;
                }
            }
        }
    }
    /// Skips tokens that are of any of the types given.
    fn skip_tokens_of_types(&mut self, token_types: &[TokenType]) {
        loop {
            if let Some(token) = self.peek() {
                if token.is_of_types(token_types) {
                    debug!("Peeking token: {}, skipping", token);
                    self.advance();
                } else {
                    break;
                }
            }
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.token_stream.peek().and_then(|token| {
            if token.r#type == TokenType::EndOfFile {
                None
            } else {
                Some(token)
            }
        })
    }

    pub fn parse(mut self) -> ParseResult {
        let borrowed = &mut self;
        borrowed.parse_top_level_decls()?;
        Ok(self.ast)
    }

    /// Parses the top level declarations within .mo files that declare items.
    fn parse_top_level_decls(&mut self) -> Result<(), ParserError> {
        while let Some(token) = self.peek() {
            match token.r#type {
                TokenType::Use => {
                    let use_decl = self.parse_use_decl()?;
                    self.ast.push(use_decl);
                }
                TokenType::Struct => {
                    let struct_decl = self.parse_struct_decl()?;
                    self.ast.push(struct_decl);
                }
                TokenType::Fn => {
                    let fn_decl = self.parse_fn_decl()?;
                    self.ast.push(fn_decl);
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
                        "Unexpected token parsing top-level declarations",
                        Some(token.clone()),
                    ));
                }
            }
        }
        Ok(())
    }

    fn parse_module_identifier(&mut self) -> Result<ModuleIdentifier, ParserError> {
        let mut module_dirs = Vec::with_capacity(16);
        if self.matches(TokenType::Ident) {
            loop {
                module_dirs.push(self.unwrap_current_token().unwrap_value());
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
    fn parse_use_decl(&mut self) -> Result<Decl, ParserError> {
        self.advance();
        let identifier = self.parse_module_identifier()?;
        let decl;
        if self.matches(TokenType::StringLiteral) {
            decl = Decl::Use {
                module_ident: identifier,
                module_alias: Some(self.unwrap_current_token().unwrap_value()),
            };
        } else {
            decl = Decl::Use {
                module_ident: identifier,
                module_alias: None,
            };
        }

        Ok(decl)
    }

    fn parse_fn_decl(&mut self) -> Result<Decl, ParserError> {
        self.advance();

        self.try_consume_token(TokenType::Ident, "Expected function identifier")?;
        let fn_ident = self.unwrap_current_token().unwrap_value();
        self.try_consume_token(TokenType::OpenParen, "Expected open parenthesis")?;

        // parse parameters
        let mut params = Vec::new();
        if !self.matches(TokenType::CloseParen) {
            loop {
                self.try_consume_token(TokenType::Ident, "Expected type identifier")?;
                let type_ident = self.unwrap_current_token().unwrap_value();

                self.try_consume_token(TokenType::Ident, "Expected variable identifier")?;
                let var_ident = self.unwrap_current_token().unwrap_value();

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
            return_type = Some(self.unwrap_current_token().unwrap_value());
        }
        // parse body / code
        let body = self.parse_code_block()?;
        let fn_decl = Decl::Fn {
            ident: fn_ident,
            params,
            return_type,
            body,
        };

        Ok(fn_decl)
    }

    // TODO:
    // if is statement (switch or match)
    // for in
    fn parse_code_block(&mut self) -> Result<CodeBlock, ParserError> {
        debug!("parsing code block");
        self.try_consume_token(TokenType::OpenBrace, "Expected open brace")?;

        self.skip_tokens_of_type(TokenType::LineBreak); // move on if there's a linebreak.

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
                            let fn_call = self.parse_fn_call(ident)?;
                            code_block.stmts.push(Stmt::Expr(fn_call));
                        } else if self.matches_predicate(|parser| {
                            parser.peek().is_some_and(|token| token.is_binary_op())
                        }) {
                            let operator = self.unwrap_current_token().r#type.try_into().unwrap();
                            let value = self.parse_expression()?;
                            code_block.stmts.push(Stmt::VarOperatorAssign {
                                ident,
                                operator,
                                value,
                            });
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
        assert_eq!(self.unwrap_current_token().r#type, TokenType::Ident);
        let ident1 = self.unwrap_current_token().unwrap_value();
        if self.matches(TokenType::Ident) {
            let ident2 = self.unwrap_current_token().unwrap_value();
            if self.matches(TokenType::DeclareAssign) {
                let expr = self.parse_expression()?;
                self.consume_line_terminator()?;
                return Ok(Stmt::VarDeclAssign {
                    type_ident: Some(ident1),
                    ident: ident2,
                    value: expr,
                });
            } else if self.matches_any(&[TokenType::LineBreak, TokenType::Semicolon]) {
                return Ok(Stmt::LocalVarDecl {
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
    // TODO: Rewrite to return expr instead
    fn parse_fn_call(&mut self, fn_ident: String) -> Result<Expr, ParserError> {
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
        //self.consume_line_terminator()?;
        let fn_call = Expr::FnCall {
            ident: fn_ident,
            args,
        };
        Ok(fn_call)
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
    fn parse_struct_decl(&mut self) -> Result<Decl, ParserError> {
        let decl;
        self.advance();
        self.try_consume_token(TokenType::Ident, "Expected struct identifier")?;
        let struct_ident = self.unwrap_current_token().unwrap_value();
        self.try_consume_token(TokenType::OpenBrace, "Expected open brace")?;
        self.skip_tokens_of_types(&[TokenType::LineBreak, TokenType::Semicolon]);
        let mut fields = Vec::new();
        loop {
            if self.matches(TokenType::CloseBrace) {
                break;
            }
            self.try_consume_token(TokenType::Ident, "Expected type identifier")?;
            let type_ident = self.unwrap_current_token().unwrap_value();
            self.try_consume_token(TokenType::Ident, "Expected variable identifier")?;
            let var_ident = self.unwrap_current_token().unwrap_value();
            fields.push(TypedVar::new(type_ident, var_ident));
            self.try_consume_token2(&[TokenType::LineBreak, TokenType::Comma], "Expected comma ',' or linebreak")?;
        }
        decl = Decl::Struct {
            ident: struct_ident,
            fields,
        };
        Ok(decl)
    }

    // :Expressions

    fn parse_expression(&mut self) -> ExprParseResult {
        debug!("parsing expression {}", self.parser_state_dbg_info());
        self.parse_equality_expr()
    }

    // a != b   a == b
    fn parse_equality_expr(&mut self) -> ExprParseResult {
        debug!("parsing equality expr {}", self.parser_state_dbg_info());

        let mut expr = self.parse_comparison_expr()?;
        if self.matches_any(&[TokenType::NotEqualTo, TokenType::EqualTo]) {
            let operator = self.unwrap_current_token();
            println!("equality expr: chosen operator {}", operator.r#type);
            let right = self.parse_comparison_expr()?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    // a > b   a >= b   a < b   a <= b
    fn parse_comparison_expr(&mut self) -> ExprParseResult {
        debug!("parsing comparison expr {}", self.parser_state_dbg_info());

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
        debug!(
            "parsing term expression (- +) {}",
            self.parser_state_dbg_info()
        );
        let mut expr = self.parse_factor_and_bitwise_expr()?;
        while self.matches_any(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self
                .current_token
                .clone()
                .expect("Operator should be here.");
            let right = self.parse_factor_and_bitwise_expr()?;
            expr = Expr::binary(expr, operator, right)
        }
        Ok(expr)
    }

    // a / b   a * b   a % b   a ~ b   a << b   a >> b   a ^ b   a | b
    fn parse_factor_and_bitwise_expr(&mut self) -> ExprParseResult {
        debug!(
            "Parsing factor expr (/ * %) {}",
            self.parser_state_dbg_info()
        );
        let mut expr = self.parse_unary_expr()?;
        while self.matches_any(&[
            TokenType::Slash,
            TokenType::Star,
            TokenType::Percent,
            TokenType::Ampersand,
            TokenType::Tilde,
            TokenType::BitShiftLeft,
            TokenType::BitShiftRight,
            TokenType::Caret,
            TokenType::Pipe,
        ]) {
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
        debug!("parsing unary expr {}", self.parser_state_dbg_info());
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
        debug!("parsing primary expr {}", self.parser_state_dbg_info());
        //dbg!(&self.current_token);
        //dbg!(&self.peek());
        if self.matches(TokenType::Ident) {
            let ident = self.unwrap_current_token().unwrap_value();
            //TODO: Parse function calls
            if self.matches(TokenType::OpenParen) {
                let fn_call = self.parse_fn_call(ident)?;
                return Ok(fn_call);
            }
            debug!("Ok, returning variable ident expr");
            return Ok(Expr::VariableIdent(ident));
        }
        if self.matches(TokenType::True) {
            debug!("Ok, returning boolean literal expr");
            return Ok(Expr::BoolLiteral(true));
        }
        if self.matches(TokenType::False) {
            debug!("Ok, returning boolean literal expr");
            return Ok(Expr::BoolLiteral(false));
        }
        if self.matches(TokenType::StringLiteral) {
            debug!("Ok, returning string literal expr");
            return Ok(Expr::StringLiteral(
                self.unwrap_current_token().unwrap_value(),
            ));
        }
        if self.matches(TokenType::NumberLiteral) {
            debug!("Ok, returning number literal expr");
            return Ok(Expr::NumberLiteral(
                self.unwrap_current_token().unwrap_value(),
            ));
        }
        if self.matches(TokenType::OpenParen) {
            let expr = self.parse_expression()?;
            self.try_consume_token(TokenType::CloseParen, "Expected '(' after expression.")?;
            debug!("Ok, returning grouping expr");
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

    fn matches_predicate<P>(&mut self, predicate: P) -> bool
    where
        P: FnOnce(&mut Parser) -> bool,
    {
        if predicate(self) {
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
        let current = self.unwrap_current_token();
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
