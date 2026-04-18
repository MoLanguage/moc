use std::vec::IntoIter;

use itertools::{PeekNth, peek_nth};
use log::debug;
use moc_common::{
    CodeBlock, ModulePath, TypedVar,
    ast::Ast,
    decl::Decl,
    error::{ExprParseResult, ParseResult, ParserError},
    expr::{DotExpr, Expr, FnCall, Ident, TypeExpr},
    op::{BinaryOp, UnaryOp},
    stmt::Stmt,
    token::{Token, TokenKind},
};

pub struct Parser {
    token_stream: PeekNth<IntoIter<Token>>,
    current_token: Option<Token>,
    ast: Ast,
    is_pratt: bool, // if false uses recursive descent for expr parsing
}

impl Parser {
    pub fn new(tokens: Vec<Token>, is_pratt: bool) -> Self {
        let parser = Self {
            token_stream: peek_nth(tokens),
            current_token: None,
            ast: Ast::new(),
            is_pratt,
        };
        parser
    }

    pub fn parse(mut self) -> ParseResult {
        if self.is_pratt {
            let expr = self.expr(0)?;
            self.ast.push(Decl::LooseExpr(expr));
        } else {
            (&mut self).parse_top_level_decls()?;
        }
        Ok(self.ast)
    }

    // PRATT PARSING >>>
    fn parse_prefix(&mut self) -> ExprParseResult {
        let token = self.advance().unwrap();
        use TokenKind::*;
        match token.kind {
            DecimalIntegerNumberLiteral
            | DecimalPointNumberLiteral
            | BinaryIntegerNumberLiteral
            | OctalIntegerNumberLiteral
            | HexadecimalIntegerNumberLiteral => Ok(Expr::NumberLiteral(
                token.unwrap_value(),
                token.kind.try_into().unwrap(), // converts the TokenKind into the NumberLiteralKind. Because we know the TokenKind is a number literal, the conversion shouldn't panic.
            )),
            Ident | ModulePath => {
                let ident = self.ident_token_to_ident();
                Ok(Expr::Variable {
                    ident, // could "evolve into" function call, module identifier or just stay a simple variable. 
                })
            }
            Minus | Excl | Tilde | Ampersand => {
                let operator = UnaryOp::try_from(token.kind).unwrap();
                let bp = operator.prefix_binding_power();

                // We call expr recursively to get what this operator applies to
                let right = self.expr(bp)?;
                Ok(Expr::Unary {
                    operator,
                    expr: Box::new(right),
                })
            }
            True => Ok(Expr::BoolLiteral(true)),
            False => Ok(Expr::BoolLiteral(false)),
            StringLiteral => Ok(Expr::StringLiteral(token.unwrap_value())),
            OpenParen => {
                let inner = self.expr(0)?;
                self.try_consume_token(CloseParen, "Expected ')'")?;
                Ok(Expr::grouping(inner))
            },
            TokenKind::OpenBrack => {
                self.parse_array_literal() 
            }
            _ => Err(ParserError::new(
                "Expected an expression",
                self.peek().cloned(),
            )),
        }
    }

    fn expr(&mut self, min_bp: u8) -> ExprParseResult {
        // 2 + 2 * 3
        // * (3,4) // higher precedence means the (sub)expression is evaluated before the lower precedence operators. That means it's nested deeper in the AST.
        // + (1,2)
        let mut left = self.parse_prefix()?; // handles literals, (groups), and prefix ! - *

        loop {
            let op_token = match self.peek() {
                Some(t) => t.clone(),
                None => break,
            };

            // postfix operator "function call"
            // a simple function call like print() can be seen like this: print is the variable of type function and '()' is a special operator that calls a function. '()' can also contain args like this '(args)'
            if op_token.kind == TokenKind::OpenParen {
                let (l_bp, _) = (110, 0);
                if l_bp < min_bp {
                    break;
                }

                self.advance(); // consume '('

                let args = self.parse_fn_call_args()?;
                left = Expr::FnCall(FnCall {
                    callee: Box::new(left),
                    args,
                });
                continue;
            }

            // infix operator "dot" / field access
            if op_token.kind == TokenKind::Dot {
                let (l_bp, _) = (120, 0);
                if l_bp < min_bp {
                    break;
                }
                self.advance(); // consume '.'

                // Zig-style postfix deref operator *
                // Example: <expr>.* 
                if self.matches_advance(TokenKind::Star) {
                    left = Expr::Unary {
                        operator: UnaryOp::Deref,
                        expr: Box::new(left),
                    };
                    continue;
                }

                self.try_consume_token2(
                    &[TokenKind::Ident, TokenKind::ModulePath],
                    "Expected identifier or '*' after '.'",
                )?;
                let member_ident = self.ident_token_to_ident();
                left = Expr::FieldAccess {
                    called_on: Box::new(left),
                    member_ident,
                };
            }
            
            if op_token.kind == TokenKind::OpenBrack {
                let (l_bp, _) = (110, 0); // High binding power, similar to function calls
                if l_bp < min_bp { // if min_bp <= 110 continue
                    break;
                }
                
                self.advance(); // consume '['
                
                let index_expr = self.expr(0)?; 
                
                self.try_consume_token(TokenKind::CloseBrack, "Expected ']' after array index")?;
                
                left = Expr::ArrayAccessor {
                    array: Box::new(left),
                    index: Box::new(index_expr),
                };
                continue;
            }

            // Try to turn the token into a BinaryOp
            let op = match BinaryOp::try_from(op_token.kind) {
                Ok(op) => op,
                Err(_) => break, // Not a binary operator, we're done with this sub-expression
            };

            let (l_bp, r_bp) = op.infix_binding_power();
            if l_bp < min_bp {
                break;
            }

            self.advance(); // consume the operator
            let right = self.expr(r_bp)?;

            left = Expr::Binary {
                left_expr: Box::new(left),
                operator: op,
                right_expr: Box::new(right),
            };
        }

        Ok(left)
    }

    // <<< PRATT PARSING END

    /// Parses the top level declarations within .mo files that declare items.
    fn parse_top_level_decls(&mut self) -> Result<(), ParserError> {
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Use => {
                    let use_decl = self.parse_use_decl()?;
                    self.ast.push(use_decl);
                }
                TokenKind::Struct => {
                    let struct_decl = self.parse_struct_decl()?;
                    self.ast.push(struct_decl);
                }
                TokenKind::Fn => {
                    let fn_decl = self.parse_fn_decl()?;
                    self.ast.push(fn_decl);
                }
                TokenKind::LineBreak => {
                    self.advance();
                    continue; // continue for now
                }
                TokenKind::EndOfFile => {
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

    // Parsing:
    // use <module_path_element>(:<module_path_element>)* ("<alias>")?
    //
    // # Examples:
    // use io | use io "foo"
    // use io:print | use io:print "printie"
    fn parse_use_decl(&mut self) -> Result<Decl, ParserError> {
        // Just peeked Use token
        self.advance();
        self.try_consume_token(TokenKind::ModulePath, "Expected module identifier")?;
        let identifier = self.unwrap_current_token().unwrap_value();
        let module_ident = ModulePath::from_string(&identifier);
        let decl;
        if self.matches_advance(TokenKind::StringLiteral) {
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

        self.try_consume_token(TokenKind::Ident, "Expected function identifier")?;
        let fn_ident = self.unwrap_current_token().unwrap_value();
        self.try_consume_token(TokenKind::OpenParen, "Expected open parenthesis")?;

        // Parse parameters
        let mut params = Vec::new();
        if !self.matches_advance(TokenKind::CloseParen) {
            loop {
                self.try_consume_token(TokenKind::Ident, "Expected variable identifier")?;
                let var_ident = self.unwrap_current_token().unwrap_value();

                let type_expr = self.parse_type_expr()?;
                params.push(TypedVar::new(var_ident, type_expr));

                if self.matches_advance(TokenKind::CloseParen) {
                    break;
                }
                if self.matches_advance(TokenKind::Comma) {
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
        if !self.matches_any(&[TokenKind::LineBreak, TokenKind::OpenBrace]) {
            let type_expr = self.parse_type_expr()?;
            return_type = Some(type_expr);
        }
        // Parse body / code
        self.skip_tokens(TokenKind::LineBreak);
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
        self.try_consume_token(TokenKind::OpenBrace, "Expected open brace")?;

        self.skip_tokens(TokenKind::LineBreak); // move on if there's a linebreak.

        let mut code_block = CodeBlock::new();
        loop {
            if self.matches_advance(TokenKind::CloseBrace) {
                self.skip_tokens(TokenKind::LineBreak);
                debug!("matched closebrace, returning codeblock");
                return Ok(code_block);
            }
            let stmt = self.parse_stmt()?;
            code_block.stmts.push(stmt);
            self.skip_tokens(TokenKind::LineBreak);
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
            match token.kind {
                TokenKind::For => return self.parse_for_loop(),
                TokenKind::If => return self.parse_if_else_stmt(),
                TokenKind::Ret => return self.parse_ret_stmt(),
                TokenKind::Defer => return self.parse_defer_stmt(),
                TokenKind::OpenBrace => return Ok(Stmt::CodeBlock(self.parse_code_block()?)),
                TokenKind::Ident | TokenKind::ModulePath | TokenKind::Star => {
                    // Parse variable declarations
                    if let Some(var_decl) = self.parse_var_decl()? {
                        return Ok(var_decl);
                    }

                    // Parse assignments
                    let dot_expr = self.parse_unary_expr()?; // parsing unary expr because of pointer deref operator *, maybe add seperate parsing step?
                    if self.matches_advance(TokenKind::Equals) {
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
                        let operator = self.unwrap_current_token().kind.try_into().unwrap();
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
    //TODO: Rework without multi lookaheads
    fn parse_var_decl(&mut self) -> Result<Option<Stmt>, ParserError> {
        use TokenKind::*;
        if self.matches_in_row(&[Ident, DeclareAssign]) {
            // type inferred declaration
            self.advance();
            let ident = self.unwrap_current_token().unwrap_value();
            self.advance();
            let value = self.parse_expression()?;
            return Ok(Some(Stmt::LocalVarDeclAssign {
                ident,
                type_expr: None,
                value,
            }));
        }

        // Declaration with simple type
        if self.matches_in_row(&[Ident, Ident, DeclareAssign]) {
            self.advance();
            let ident = self.unwrap_current_token().unwrap_value();

            let type_expr = self.parse_type_expr()?;

            self.advance(); // skip over DeclareAssign

            let value = self.parse_expression()?;
            return Ok(Some(Stmt::LocalVarDeclAssign {
                ident,
                type_expr: Some(type_expr),
                value,
            }));
        }
        // Declaration with pointer type or array type
        if self.matches_in_row(&[Ident, Star]) || self.matches_in_row(&[Ident, OpenBrack]) {
            self.advance();
            let ident = self.unwrap_current_token().unwrap_value();

            let type_expr = self.parse_type_expr()?;

            self.try_consume_token(DeclareAssign, "Expected ':='")?;

            let value = self.parse_expression()?;
            return Ok(Some(Stmt::LocalVarDeclAssign {
                ident,
                type_expr: Some(type_expr),
                value,
            }));
        }
        Ok(None)
    }

    // like '[1,2,3,4,5,6,7,8]'
    fn parse_array_literal(&mut self) -> Result<Expr, ParserError> {
        let mut elements = Vec::new();
        if !self.matches_advance(TokenKind::CloseBrack) {
            loop {
                let element = self.parse_expression()?;
                elements.push(element);

                if self.matches_advance(TokenKind::CloseBrack) {
                    break;
                }
                if self.matches_advance(TokenKind::Comma) {
                    continue;
                } else {
                    return ParserError::new(
                        "Expected comma ',' or closed bracket ']'",
                        self.peek().cloned(),
                    )
                    .wrap();
                }
            }
        }
        return Ok(Expr::ArrayLiteral {
            elements,
        });
    }

    fn parse_type_expr(&mut self) -> Result<TypeExpr, ParserError> {
        // array type
        let mut array_length = None;
        if self.matches_advance(TokenKind::OpenBrack) {
            if self.matches_any_advance(&[TokenKind::DecimalIntegerNumberLiteral]) {
                let length: usize = self
                    .unwrap_current_token()
                    .unwrap_value()
                    .parse()
                    .expect("If this panics, there is a lexer bug");
                array_length = Some(length);
            }

            self.try_consume_token(TokenKind::CloseBrack, "Expected closed bracket '['")?;
            return Ok(TypeExpr::Array {
                length: array_length,
                type_expr: Box::new(self.parse_type_expr()?),
            });
        }

        // pointer type
        if self.matches_advance(TokenKind::Star) {
            return Ok(TypeExpr::pointer(self.parse_type_expr()?));
        }

        // identifier with optional module path prefix
        if self.matches_any_advance(&[TokenKind::Ident, TokenKind::ModulePath]) {
            return Ok(TypeExpr::Ident(self.ident_token_to_ident()));
        }
        ParserError::new("Expected type expression", self.peek().cloned()).wrap()
    }

    #[allow(dead_code)]
    fn consume_line_terminator(&mut self) -> Result<(), ParserError> {
        self.try_consume_token2(
            &[TokenKind::LineBreak, TokenKind::Semicolon],
            "Expected line break or ;",
        )
    }

    fn parse_fn_call_args(&mut self) -> Result<Vec<Expr>, ParserError> {
        // parse arguments
        let mut args = Vec::new();

        if !self.matches_advance(TokenKind::CloseParen) {
            loop {
                let expr = self.parse_expression()?;
                args.push(expr);
                if self.matches_advance(TokenKind::CloseParen) {
                    break;
                }
                if self.matches_advance(TokenKind::Comma) {
                    continue;
                }
                return ParserError::new(
                    "Expected argument delimiter ',' or closed parenthesis ')'",
                    self.peek().cloned(),
                )
                .wrap();
            }
        }
        Ok(args)
    }
    // when this is called, the current token is the function identifier
    #[deprecated]
    fn parse_fn_call(&mut self, ident: Ident) -> Result<FnCall, ParserError> {
        // parse arguments
        let mut args = Vec::new();

        if !self.matches_advance(TokenKind::CloseParen) {
            loop {
                let expr = self.parse_expression()?;
                args.push(expr);
                if self.matches_advance(TokenKind::CloseParen) {
                    break;
                }
                if self.matches_advance(TokenKind::Comma) {
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
            callee: Box::new(Expr::Variable { ident }),
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
        if self.matches_advance(TokenKind::Else) {
            else_block = Some(self.parse_code_block()?);
        }
        Ok(Stmt::If {
            condition,
            if_block,
            else_block,
        })
    }

    fn parse_defer_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.advance();
        let stmt = self.parse_stmt()?;
        Ok(Stmt::Defer(Box::new(stmt)))
    }

    fn parse_ret_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.advance();
        if self.matches_advance(TokenKind::LineBreak) {
            Ok(Stmt::Ret(None))
        } else {
            let expr = self.parse_expression()?;
            Ok(Stmt::Ret(Some(expr)))
        }
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
        self.try_consume_token(TokenKind::Ident, "Expected struct identifier")?;
        let struct_ident = self.unwrap_current_token().unwrap_value();
        self.skip_tokens(TokenKind::LineBreak);
        self.try_consume_token(TokenKind::OpenBrace, "Expected open brace")?;
        let mut fields = Vec::new();
        loop {
            self.skip_tokens(TokenKind::LineBreak);
            if self.matches_advance(TokenKind::CloseBrace) {
                break; // Struct without fields
            }
            self.try_consume_token(TokenKind::Ident, "Expected variable identifier")?;
            let var_ident = self.unwrap_current_token().unwrap_value();

            let type_expr = self.parse_type_expr()?;
            fields.push(TypedVar::new(var_ident, type_expr));
            if !self
                .peek()
                .is_some_and(|t| t.is_of_kind(TokenKind::CloseBrace))
            {
                // If next isn't token CloseBrace, means we are expecting next struct field declaration
                self.try_consume_token2(
                    &[TokenKind::LineBreak, TokenKind::Comma],
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
        if self.is_pratt {
            self.expr(0)
        } else {
            self.parse_equality_expr()
        }
    }

    // :Recursive Descent Expressions

    // a != b   a == b
    // <expr> (( != | == ) <expr>)*
    #[deprecated]
    fn parse_equality_expr(&mut self) -> ExprParseResult {
        debug!("parsing equality expr {}", self.parser_state_dbg_info());

        let mut expr = self.parse_comparison_expr()?;
        while self.matches_any_advance(&[TokenKind::ExclEquals, TokenKind::DoubleEquals]) {
            let operator: BinaryOp = self.unwrap_current_token().kind.try_into().unwrap();
            println!("equality expr: chosen operator {}", operator);
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
    #[deprecated]
    fn parse_comparison_expr(&mut self) -> ExprParseResult {
        debug!("parsing comparison expr {}", self.parser_state_dbg_info());

        let mut expr = self.parse_term_expr()?;
        let tokens = &[
            TokenKind::Greater,
            TokenKind::GreaterOrEqual,
            TokenKind::Less,
            TokenKind::LessOrEqual,
        ];
        while self.matches_any_advance(tokens.as_slice()) {
            let operator = self.unwrap_current_token().kind.try_into().unwrap();
            let right = self.parse_term_expr()?;
            expr = Expr::binary(expr, operator, right); // NEXT UP: Fix these errors. Make compile. Then build basic pratt expression parser
        }
        Ok(expr)
    }

    // Parsing:
    // <expr> (( - | + ) <expr>)*
    //
    // # Examples:
    // a - b   a + b
    #[deprecated]
    fn parse_term_expr(&mut self) -> ExprParseResult {
        debug!(
            "parsing term expression (- +) {}",
            self.parser_state_dbg_info()
        );
        let mut expr = self.parse_factor_and_bitwise_expr()?;
        while self.matches_any_advance(&[TokenKind::Minus, TokenKind::Plus]) {
            let operator = self.unwrap_current_token().kind.try_into().unwrap();
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
    #[deprecated]
    fn parse_factor_and_bitwise_expr(&mut self) -> ExprParseResult {
        debug!(
            "Parsing factor expr (/ * %) {}",
            self.parser_state_dbg_info()
        );
        let mut expr = self.parse_unary_expr()?;
        while self.matches_any_advance(&[
            TokenKind::Slash,
            TokenKind::Star,
            TokenKind::Percent,
            TokenKind::Ampersand,
            TokenKind::Tilde,
            TokenKind::BitShiftLeft,
            TokenKind::BitShiftRight,
            TokenKind::Caret,
            TokenKind::Pipe,
        ]) {
            let operator = self.unwrap_current_token().kind.try_into().unwrap();
            let right = self.parse_unary_expr()?;
            println!("Ok, returning binary factor expr");
            expr = Expr::binary(expr, operator, right)
        }
        Ok(expr)
    }

    // Parsing:
    // (! | - | ~ | &)?<expr>
    //
    // # Examples:
    // !a   -a   ~a   &a   *a
    #[deprecated]
    fn parse_unary_expr(&mut self) -> ExprParseResult {
        debug!("parsing unary expr {}", self.parser_state_dbg_info());
        if self.matches_any_advance(&[TokenKind::Excl, TokenKind::Minus, TokenKind::Ampersand]) {
            let operator = self
                .unwrap_current_token()
                .kind
                .try_into()
                .expect("operator should be here");
            let right = self.parse_dot_expr()?;
            println!("Ok, returning unary expr");
            return Ok(Expr::unary(operator, right));
        }
        self.parse_dot_expr()
    }

    /// Tries to convert the current Ident/ModulePath token into an Ident
    /// Panics: If current token is not of kind Ident or ModulePath
    #[inline]
    fn ident_token_to_ident(&self) -> Ident {
        let token = self.unwrap_current_token();
        let ident = token.unwrap_value();
        return match token.kind {
            TokenKind::Ident => Ident::Simple(ident),
            TokenKind::ModulePath => {
                let mut module_path_prefix = ModulePath::from_string(&ident);
                let ident = module_path_prefix.remove_and_get_last_path();

                Ident::WithModulePrefix(module_path_prefix, ident)
            }
            _ => panic!("Given token needs to be of type ModulePath or Ident"),
        };
    }

    // Parsing:
    // <expr>(.<fn_call>|<field>)*
    //
    // Examples:
    // a.b().c() | a.b.c() | a.b.c(args) etc.
    #[deprecated]
    fn parse_dot_expr(&mut self) -> ExprParseResult {
        // start from the base (primary expression)
        let mut expr = self.parse_primary_expr()?;

        // then, while there's a '.', chain
        while self.matches_advance(TokenKind::Dot) {
            // we're already past the '.'
            self.try_consume_token2(
                &[TokenKind::Ident, TokenKind::ModulePath],
                "Expected identifier after '.'",
            )?;
            let ident = self.ident_token_to_ident();
            if self.matches_advance(TokenKind::OpenParen) {
                let fn_call = self.parse_fn_call(ident)?;
                expr = Expr::DotExpr(DotExpr::FnCall {
                    called_on: Box::new(expr),
                    fn_call,
                });
            } else {
                expr = Expr::DotExpr(DotExpr::FieldAccess {
                    called_on: Box::new(expr),
                    member_ident: ident,
                });
            }
        }

        Ok(expr)
    }

    // Parsing:
    // Literals, variables and function calls
    #[deprecated]
    fn parse_primary_expr(&mut self) -> ExprParseResult {
        debug!("parsing primary expr {}", self.parser_state_dbg_info());

        if self.matches_advance(TokenKind::True) {
            debug!("Ok, returning boolean literal expr");
            return Ok(Expr::BoolLiteral(true));
        }
        if self.matches_advance(TokenKind::False) {
            debug!("Ok, returning boolean literal expr");
            return Ok(Expr::BoolLiteral(false));
        }
        if self.matches_advance(TokenKind::StringLiteral) {
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
                token.kind.try_into().unwrap(),
            ));
        }
        if self.matches_advance(TokenKind::OpenParen) {
            let expr = self.parse_expression()?;
            self.try_consume_token(TokenKind::CloseParen, "Expected ')' after expression.")?;
            debug!("Ok, returning grouping expr");
            return Ok(Expr::Grouping(Box::new(expr)));
        }
        if self.matches_any_advance(&[TokenKind::Ident, TokenKind::ModulePath]) {
            let ident = self.ident_token_to_ident();
            if self.matches_advance(TokenKind::OpenBrack) {
                let index = self.parse_expression()?;
                if self.matches_advance(TokenKind::CloseBrack) {
                    return Ok(Expr::ArrayAccessor {
                        array: Box::new(Expr::Variable { ident }),
                        index: Box::new(index),
                    });
                } else {
                    return Err(ParserError::new(
                        "Expected closed bracket ']'",
                        self.peek().cloned(),
                    ));
                }
            }
            if self.matches_advance(TokenKind::OpenParen) {
                let fn_call = self.parse_fn_call(ident)?;
                return Ok(Expr::FnCall(fn_call));
            }
            return Ok(Expr::Variable { ident });
        }
        if self.matches_advance(TokenKind::OpenBrack) {
            return self.parse_array_literal();
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
    fn advance(&mut self) -> Option<Token> {
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
        return self.current_token();
    }

    /// Advances n times.
    #[allow(dead_code)]
    fn advance_n(&mut self, n: usize) {
        for _ in 0..n {
            self.advance();
        }
    }

    /// Skips tokens that are of the same kind as given.
    fn skip_tokens(&mut self, token_kind: TokenKind) {
        loop {
            if let Some(token) = self.peek() {
                if token.is_of_kind(token_kind) {
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

    /// Skips tokens that are of any of the kinds given.
    #[allow(dead_code)]
    fn skip_tokens_of_any_kinds(&mut self, token_kinds: &[TokenKind]) {
        loop {
            if let Some(token) = self.peek() {
                if token.is_of_any_kinds(token_kinds) {
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
    fn matches_any_advance(&mut self, tokens: &[TokenKind]) -> bool {
        if self.is_next_of_types(tokens) {
            self.advance();
            return true;
        }
        false
    }

    #[track_caller]
    fn matches_advance(&mut self, token: TokenKind) -> bool {
        debug!("{}", std::panic::Location::caller());
        if self.is_next_of_type(token) {
            self.advance();
            return true;
        }
        false
    }

    /// checks if next token is of one of the given types
    fn matches_any(&mut self, tokens: &[TokenKind]) -> bool {
        if self.is_next_of_types(tokens) {
            return true;
        }
        false
    }

    #[allow(dead_code)]
    #[track_caller]
    fn matches(&mut self, token: TokenKind) -> bool {
        debug!("{}", std::panic::Location::caller());
        if self.is_next_of_type(token) {
            return true;
        }
        false
    }

    /// peeks multiple tokens to see if they match the given token types in row.
    #[allow(dead_code)]
    fn matches_in_row(&mut self, tokens: &[TokenKind]) -> bool {
        for (n, token_type) in tokens.iter().enumerate() {
            if let Some(peeked_token) = self.peek_nth(n) {
                if &peeked_token.kind == token_type {
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
    fn is_next_of_type(&mut self, token_type: TokenKind) -> bool {
        if let Some(current_token) = self.peek() {
            if token_type == current_token.kind && token_type != TokenKind::EndOfFile {
                return true;
            }
        }
        false
    }

    // checks if the following token is of one of the given types.
    fn is_next_of_types(&mut self, tokens: &[TokenKind]) -> bool {
        for token in tokens {
            if self.is_next_of_type(*token) {
                return true;
            }
        }
        false
    }

    // if next token is of given type, advances. If not, return an error with given message.
    fn try_consume_token(&mut self, token_type: TokenKind, msg: &str) -> Result<(), ParserError> {
        if self.is_next_of_type(token_type) {
            self.advance();
            return Ok(());
        }
        Err(ParserError::new(msg, self.peek().cloned()))
    }

    /// If next token is of any of given types, advances. If not, return an error with given message.
    fn try_consume_token2(&mut self, tokens: &[TokenKind], msg: &str) -> Result<(), ParserError> {
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
        let r#type = current.kind;
        let mut str = String::new();

        str.push_str(&format!("Current: \"{}\", loc: {}:{}", r#type, line, col));

        if let Some(next) = self.peek() {
            let line = next.span.end.line;
            let col = next.span.end.column;
            let r#type = next.kind;
            str.push_str(&format!(" --- Next: \"{}\", loc: {}:{}", r#type, line, col));
        }
        str
    }
}
