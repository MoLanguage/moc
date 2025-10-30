use std::{collections::VecDeque, vec::IntoIter};

use itertools::{MultiPeek, PeekNth, multipeek, peek_nth};
use log::debug;
use moc_common::{
    CodeBlock, ModulePath, TypedVar,
    ast::Ast,
    decl::Decl,
    error::{ExprParseResult, ParseResult, ParserError},
    expr::{DotExpr, Expr, FnCall},
    stmt::Stmt,
    token::{Token, TokenType},
};

pub struct Parser {
    token_stream: PeekNth<IntoIter<Token>>,
    current_token: Option<Token>,
    ast: Ast,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let parser = Self {
            token_stream: peek_nth(tokens),
            current_token: None,
            ast: Ast::new(),
        };
        parser
    }

    pub fn parse(mut self) -> ParseResult {
        (&mut self).parse_top_level_decls()?;
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

    fn parse_module_path(&mut self) -> Result<ModulePath, ParserError> {
        let mut module_dirs = VecDeque::new();
        if self.matches(TokenType::Ident) {
            loop {
                module_dirs.push_back(self.unwrap_current_token().unwrap_value());
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
            return ParserError::new("Expected module identifier", self.peek().cloned()).wrap();
        }
        Ok(ModulePath::new(module_dirs))
    }

    // Parsing:
    // use <module_path_element>(:<module_path_element>)* ("<alias>")?
    //
    // # Examples:
    // use io | use io "foo"
    // use io:print | use io:print "printie"
    fn parse_use_decl(&mut self) -> Result<Decl, ParserError> {
        // Just peeked Use token
        self.advance();
        self.try_consume_token(TokenType::ModIdent, "Expected module identifier")?;
        let identifier = self.unwrap_current_token().unwrap_value();
        let module_ident = ModulePath::from_string(&identifier);
        let decl;
        if self.matches(TokenType::StringLiteral) {
            decl = Decl::Use {
                module_ident,
                module_alias: Some(self.unwrap_current_token().unwrap_value()),
            };
        } else {
            decl = Decl::Use {
                module_ident,
                module_alias: None,
            };
        }

        Ok(decl)
    }

    fn parse_fn_decl(&mut self) -> Result<Decl, ParserError> {
        // Just peeked Fn token
        self.advance();

        self.try_consume_token(TokenType::Ident, "Expected function identifier")?;
        let fn_ident = self.unwrap_current_token().unwrap_value();
        self.try_consume_token(TokenType::OpenParen, "Expected open parenthesis")?;

        // Parse parameters
        let mut params = Vec::new();
        if !self.matches(TokenType::CloseParen) {
            loop {
                self.try_consume_token(TokenType::Ident, "Expected variable identifier")?;
                let var_ident = self.unwrap_current_token().unwrap_value();

                self.try_consume_token(TokenType::Ident, "Expected type identifier")?;
                let type_ident = self.unwrap_current_token().unwrap_value();
                params.push(TypedVar::new(var_ident, type_ident));

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
        // Parse return type
        let mut return_type = None;
        if self.matches(TokenType::Ident) {
            return_type = Some(self.unwrap_current_token().unwrap_value());
        }
        // Parse body / code
        self.skip(TokenType::LineBreak);
        let body = self.parse_code_block()?;
        let fn_decl = Decl::Fn {
            ident: fn_ident,
            params,
            return_type,
            body,
        };

        Ok(fn_decl)
    }

    fn parse_code_block(&mut self) -> Result<CodeBlock, ParserError> {
        debug!("parsing code block");
        self.try_consume_token(TokenType::OpenBrace, "Expected open brace")?;

        self.skip(TokenType::LineBreak); // move on if there's a linebreak.

        let mut code_block = CodeBlock::new();
        loop {
            if self.matches(TokenType::CloseBrace) {
                self.skip(TokenType::LineBreak);
                debug!("matched closebrace, returning codeblock");
                return Ok(code_block);
            }
            let stmt = self.parse_stmt()?;
            code_block.stmts.push(stmt);
            self.skip(TokenType::LineBreak);
        }
    }

    // TODO: Rework
    //
    // Stmts with identifier in the beginning:
    //
    // Variable declaration assignment
    // foo type := expr
    //
    //
    // struct_field_assignment:
    // dot().expr().field = expr
    //
    // fn call stmt:
    // calling()
    //
    fn parse_stmt(&mut self) -> Result<Stmt, ParserError> {
        if let Some(token) = self.peek().cloned() {
            match token.r#type {
                TokenType::For => return self.parse_for_loop(),
                TokenType::If => return self.parse_if_else_stmt(),
                TokenType::Ret => return self.parse_ret_stmt(),
                TokenType::Ident | TokenType::ModIdent => {
                    // Parse variable declarations
                    if let Some(var_decl) = self.parse_var_decl()? {
                        return Ok(var_decl);
                    }

                    // Parse assignments
                    let dot_expr = self.parse_dot_expr()?;
                    if self.matches(TokenType::Equals) {
                        let new_value = self.parse_expression()?;
                        //self.consume_line_terminator()?; // we'll see if we need this.. keeping this commented out for now.
                        return Ok(Stmt::Assignmt {
                            assignee: dot_expr,
                            new_value,
                        });
                    }
                    if self.peek().is_some_and(|token| token.is_binary_op()) {
                        // handling operator assign statements
                        // e.g. a += 10
                        self.advance();
                        let operator = self.unwrap_current_token().r#type.try_into().unwrap();
                        let value = self.parse_expression()?;
                        return Ok(Stmt::VarOperatorAssign {
                            assignee: dot_expr,
                            operator,
                            value,
                        });
                    }
                    return Ok(Stmt::Expr(dot_expr));
                }
                _ => {}
            }
        }
        Err(ParserError::new(
            "Couldn't parse statement",
            self.current_token(),
        ))
    }

    /*
    Parses statements of this form:
    a i32 := 10   | DeclAssignmt
    a := 10       | DeclAssignmt (no type identifier. type to be inferred)
    a i32         | Decl
     */
    fn parse_var_decl(&mut self) -> Result<Option<Stmt>, ParserError> {
        if self.matches_in_row(&[TokenType::Ident, TokenType::DeclareAssign]) {
            // type inferred declaration
            self.advance();
            let ident = self.unwrap_current_token().unwrap_value();
            self.advance();
            let value = self.parse_expression()?;
            return Ok(Some(Stmt::LocalVarDeclAssign { ident, type_ident: None, value }))
        }
        if self.matches_in_row(&[TokenType::Ident, TokenType::Ident, TokenType::DeclareAssign]) {
            // declaration with type
            self.advance();
            let ident = self.unwrap_current_token().unwrap_value();

            self.advance();
            let type_ident = self.unwrap_current_token().unwrap_value();

            self.advance(); // skip over DeclareAssign

            let value = self.parse_expression()?;
            return Ok(Some(Stmt::LocalVarDeclAssign { ident, type_ident: Some(type_ident), value }))
        }
        Ok(None)
    }

    fn consume_line_terminator(&mut self) -> Result<(), ParserError> {
        self.try_consume_token2(
            &[TokenType::LineBreak, TokenType::Semicolon],
            "Expected line break or ;",
        )
    }

    // when this is called, the current token is the function identifier
    fn parse_fn_call(
        &mut self,
        mod_ident: Option<ModulePath>,
        fn_ident: String,
    ) -> Result<FnCall, ParserError> {
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
        let fn_call = FnCall {
            mod_ident, // TODO: Parse module path inside this method
            ident: fn_ident,
            args,
        };
        Ok(fn_call)
    }

    // TODO:
    // for-in loop for collections, for loop without condition (like loop keyword in Rust)
    //
    // Parsing:
    // for <bool expr> <code block>
    fn parse_for_loop(&mut self) -> Result<Stmt, ParserError> {
        self.advance();
        let condition = self.parse_expression()?;
        let code_block = self.parse_code_block()?;
        let stmt = Stmt::ForLoop {
            condition,
            code_block,
        };
        Ok(stmt)
    }

    // TODO:
    // if is statement (like switch or match)
    //
    // Parsing:
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
        a int
        b int
    }
    unit struct:
    struct Foo {}
     */
    fn parse_struct_decl(&mut self) -> Result<Decl, ParserError> {
        self.advance();
        self.try_consume_token(TokenType::Ident, "Expected struct identifier")?;
        let struct_ident = self.unwrap_current_token().unwrap_value();
        self.skip(TokenType::LineBreak);
        self.try_consume_token(TokenType::OpenBrace, "Expected open brace")?;
        let mut fields = Vec::new();
        loop {
            self.skip(TokenType::LineBreak);
            if self.matches(TokenType::CloseBrace) {
                break; // Struct without fields
            }
            self.try_consume_token(TokenType::Ident, "Expected variable identifier")?;
            let var_ident = self.unwrap_current_token().unwrap_value();
            self.try_consume_token(TokenType::Ident, "Expected type identifier")?;
            let type_ident = self.unwrap_current_token().unwrap_value();
            fields.push(TypedVar::new(var_ident, type_ident));
            if !self
                .peek()
                .is_some_and(|t| t.is_of_type(TokenType::CloseBrace))
            {
                // If next isn't token CloseBrace, means we are expecting next struct field declaration
                self.try_consume_token2(
                    &[TokenType::LineBreak, TokenType::Comma],
                    "Expected comma ',' or linebreak",
                )?;
            }
        }
        Ok(Decl::Struct {
            ident: struct_ident,
            fields,
        })
    }

    // :Expressions

    fn parse_expression(&mut self) -> ExprParseResult {
        debug!("parsing expression {}", self.parser_state_dbg_info());
        self.parse_equality_expr()
    }

    // a != b   a == b
    // <expr> (( != | == ) <expr>)*
    fn parse_equality_expr(&mut self) -> ExprParseResult {
        debug!("parsing equality expr {}", self.parser_state_dbg_info());

        let mut expr = self.parse_comparison_expr()?;
        while self.matches_any(&[TokenType::ExclEquals, TokenType::DoubleEquals]) {
            let operator = self.unwrap_current_token();
            println!("equality expr: chosen operator {}", operator.r#type);
            let right = self.parse_comparison_expr()?;
            expr = Expr::binary(expr, operator, right);
        }
        Ok(expr)
    }

    // Parsing:
    // <expr> (( > | >= | < | <= ) <expr>)*
    //
    // # Examples:
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

    // Parsing:
    // <expr> (( - | + ) <expr>)*
    //
    // # Examples:
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

    // Parsing:
    // <expr> (( / | * | % | & | ~ | << | >> | ^ | '|' ) <expr>)*
    //
    // # Examples:
    // a / b   a * b   a % b   a & b   a ~ b   a << b   a >> b   a ^ b   a | b
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

    // Parsing:
    // (! | -)?<expr>
    //
    // # Examples:
    // !a   -a
    fn parse_unary_expr(&mut self) -> ExprParseResult {
        debug!("parsing unary expr {}", self.parser_state_dbg_info());
        if self.matches_any(&[TokenType::Excl, TokenType::Minus]) {
            let operator = self
                .current_token
                .clone()
                .expect("Operator should be here.");
            let right = self.parse_dot_expr()?;
            println!("Ok, returning unary expr");
            return Ok(Expr::unary(operator, right));
        }
        self.parse_dot_expr()
    }

    #[inline]
    fn mod_path_split_from_ident_token(&self) -> (Option<ModulePath>, String) {
        let token = self.unwrap_current_token();
        let ident = token.unwrap_value();
        return match token.r#type {
            TokenType::Ident => (None, ident),
            TokenType::ModIdent => {
                let mut module_path_prefix = ModulePath::from_string(&ident);
                let ident = module_path_prefix.remove_and_get_last_path();

                (Some(module_path_prefix), ident)
            }
            _ => panic!("Given token needs to be of type ModIdent or Ident"),
        };
    }

    // Parsing:
    // <expr>(.<fn_call>|<field>)*
    //
    // Examples:
    // a.b().c() | a.b.c() | a.b.c(args) etc.
    fn parse_dot_expr(&mut self) -> ExprParseResult {
        // start from the base (primary expression)
        let mut expr = self.parse_primary_expr()?;

        // then, while there's a '.', chain
        while self.matches(TokenType::Dot) {
            // we're already past the '.'
            self.try_consume_token2(&[TokenType::Ident, TokenType::ModIdent], "Expected identifier after '.'")?;
            let (module_path_prefix, ident) = self.mod_path_split_from_ident_token();
            if self.matches(TokenType::OpenParen) {
                let fn_call = self.parse_fn_call(module_path_prefix, ident)?;
                expr = Expr::DotExpr(DotExpr::FnCall {
                    called_on: Box::new(expr),
                    fn_call,
                });
            } else {
                expr = Expr::DotExpr(DotExpr::FieldAccess {
                    called_on: Box::new(expr),
                    field_ident: ident,
                });
            }
        }

        Ok(expr)
    }

    // Parsing:
    // Literals, variables and function calls
    fn parse_primary_expr(&mut self) -> ExprParseResult {
        debug!("parsing primary expr {}", self.parser_state_dbg_info());

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
        if self.peek().is_some_and(|t| t.is_number_literal()) {
            self.advance();
            debug!("Ok, returning number literal expr");
            let token = self.unwrap_current_token();
            return Ok(Expr::NumberLiteral(
                token.unwrap_value(),
                token.r#type.try_into().unwrap(),
            ));
        }
        if self.matches(TokenType::OpenParen) {
            let expr = self.parse_expression()?;
            self.try_consume_token(TokenType::CloseParen, "Expected ')' after expression.")?;
            debug!("Ok, returning grouping expr");
            return Ok(Expr::Grouping(Box::new(expr)));
        }
        if self.matches_any(&[TokenType::Ident, TokenType::ModIdent]) {
            let (prefix, ident) = self.mod_path_split_from_ident_token();
            if self.matches(TokenType::OpenParen) {
                let fn_call = self.parse_fn_call(prefix, ident)?;
                return Ok(Expr::FnCall(fn_call));
            }
            return Ok(Expr::Variable { module_path_prefix: prefix, ident });
        }
        //return Ok(Expr::Empty);
        ParserError::new("Expected an expression", self.peek().cloned()).wrap()
    }

    // :Utils

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
                    "{} advanced from {}, peeking {}",
                    std::panic::Location::caller(),
                    current_token,
                    peeked_token
                );
            }
        }
    }

    /// Advances n times.
    #[allow(dead_code)]
    fn advance_n(&mut self, n: usize) {
        for _ in 0..n {
            self.advance();
        }
    }

    /// Skips tokens that are of the same type as given.
    fn skip(&mut self, token_type: TokenType) {
        loop {
            if let Some(token) = self.peek() {
                if token.is_of_type(token_type) {
                    let token = token.clone();
                    self.advance();
                    debug!("Skipped token: {}", token);
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    /// Skips tokens that are of any of the types given.
    #[allow(dead_code)]
    fn skip_any(&mut self, token_types: &[TokenType]) {
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
        self.token_stream.peek()
    }

    #[allow(dead_code)]
    fn peek_nth(&mut self, n: usize) -> Option<&Token> {
        self.token_stream.peek_nth(n) // Note: maybe check if this causes problems at the end of files...
    }

    /// checks if next token is of one of the given types, then moves on to that token
    fn matches_any(&mut self, tokens: &[TokenType]) -> bool {
        if self.is_next_of_types(tokens) {
            self.advance();
            return true;
        }
        false
    }

    #[track_caller]
    fn matches(&mut self, token: TokenType) -> bool {
        debug!("{}", std::panic::Location::caller());
        if self.is_next_of_type(token) {
            self.advance();
            return true;
        }
        false
    }

    /// peeks multiple tokens to see if they match the given token types in row.
    #[allow(dead_code)]
    fn matches_in_row(&mut self, tokens: &[TokenType]) -> bool {
        for (n, token_type) in tokens.iter().enumerate() {
            if let Some(peeked_token) = self.peek_nth(n) {
                if &peeked_token.r#type == token_type {
                    continue;
                } else {
                    return false;
                }
            }
        }
        true
    }

    #[allow(dead_code)]
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

    /// Prints some debug info about the current state of the parser
    fn parser_state_dbg_info(&mut self) -> String {
        let current = self.unwrap_current_token();
        let line = current.span.start.line;
        let col = current.span.end.column;
        let r#type = current.r#type;
        let mut str = String::new();

        str.push_str(&format!("Current: \"{}\", loc: {}:{}", r#type, line, col));

        if let Some(next) = self.peek() {
            let line = next.span.end.line;
            let col = next.span.end.column;
            let r#type = next.r#type;
            str.push_str(&format!(" --- Next: \"{}\", loc: {}:{}", r#type, line, col));
        }
        str
    }
}
