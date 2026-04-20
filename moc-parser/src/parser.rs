use std::{usize, vec::IntoIter};

use itertools::{PeekNth, peek_nth};
use log::debug;
use moc_common::{
    CodeBlock, ModulePath, TypedVar,
    ast::Ast,
    decl::{Decl, FnSignature, Variant, VariantData},
    error::{ExprParseResult, ParseResult, ParserError},
    expr::{Expr, FnCall, GenericParam, Ident, TraitBound, TypeExpr},
    op::{BinaryOp, UnaryOp},
    stmt::Stmt,
    token::{Token, TokenKind},
};

pub struct Parser {
    token_stream: PeekNth<IntoIter<Token>>,
    current_token: Option<Token>,
    ast: Ast,
    only_expr: bool, // if true only parses an expresion, if false, parses full program structure.
}

impl Parser {
    pub fn new(tokens: Vec<Token>, only_expr: bool) -> Self {
        let parser = Self {
            token_stream: peek_nth(tokens),
            current_token: None,
            ast: Ast::new(),
            only_expr,
        };
        parser
    }

    pub fn parse(mut self) -> ParseResult {
        if self.only_expr {
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
            For => return self.parse_for_loop(),
            True => Ok(Expr::BoolLiteral(true)),
            False => Ok(Expr::BoolLiteral(false)),
            StringLiteral => Ok(Expr::StringLiteral(token.unwrap_value())),
            OpenParen => {
                let inner = self.expr(0)?;
                self.try_consume_token(CloseParen, "Expected ')'")?;
                Ok(Expr::grouping(inner))
            }
            OpenBrack => self.parse_array_literal(),
            If => self.parse_if_else_expr(),
            _ => Err(ParserError::new(
                "Expected an expression",
                Some(token.clone()),
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
            let (l_bp, r_bp) = match op_token.infix_binding_power() {
                Some(bp) => bp,
                None => break,
            };
            if l_bp < min_bp {
                break;
            }

            self.advance();

            if op_token.is_assignment_operator() {
                let right = self.expr(r_bp)?;
                left = Expr::Assign {
                    assignee: Box::new(left),
                    operator: op_token.kind,
                    value: Box::new(right),
                };
                continue;
            }

            match op_token.kind {
                TokenKind::OpenParen => {
                    let args = self.parse_fn_call_args()?;
                    left = Expr::FnCall(FnCall {
                        callee: Box::new(left),
                        args,
                    });
                    continue;
                }
                TokenKind::Dot => {
                    // Zig-style postfix deref operator *
                    // Example: <expr>.*
                    if self.matches_advance(TokenKind::Star) {
                        left = Expr::Unary {
                            operator: UnaryOp::Deref,
                            expr: Box::new(left),
                        };
                        continue;
                    }

                    self.skip_line_terminators();

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
                TokenKind::OpenBrack => {
                    let index_expr = self.expr(0)?;

                    self.try_consume_token(
                        TokenKind::CloseBrack,
                        "Expected ']' after array index",
                    )?;

                    left = Expr::ArrayAccessor {
                        array: Box::new(left),
                        index: Box::new(index_expr),
                    };
                    continue;
                }
                _ => {
                    // Try to turn the token into a BinaryOp
                    let op = BinaryOp::try_from(op_token.kind)
                        .expect("should've been handled in infix_binding_power");
                    // We can unwrap/expect because infix_binding_power returns None and breaks the loop before we get here.

                    self.skip_line_terminators(); // allows to have binary expressions span across multiple lines

                    let right = self.expr(r_bp)?;
                    left = Expr::Binary {
                        left_expr: Box::new(left),
                        operator: op,
                        right_expr: Box::new(right),
                    };
                }
            }
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
                TokenKind::Sum => {
                    let sum_decl = self.parse_sum_decl()?;
                    self.ast.push(sum_decl);
                }
                TokenKind::Fn => {
                    let fn_decl = self.parse_fn_decl()?;
                    self.ast.push(fn_decl);
                }
                TokenKind::Trait => {
                    let trait_decl = self.parse_trait_decl()?;
                    self.ast.push(trait_decl);
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

    fn parse_fn_signature(&mut self) -> Result<FnSignature, ParserError> {
        // Just peeked Fn token
        self.advance();

        self.try_consume_token(TokenKind::Ident, "Expected function identifier")?;
        let ident = self.unwrap_current_token().unwrap_value();

        let generics = self.parse_generic_params()?;

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
        Ok(FnSignature {
            ident,
            generics,
            params,
            return_type,
        })
    }

    // fn (T) abc() ret_type { /*...*/ }
    fn parse_fn_decl(&mut self) -> Result<Decl, ParserError> {
        let signature = self.parse_fn_signature()?;
        self.skip_tokens(TokenKind::LineBreak);
        let body = self.parse_code_block()?;
        let fn_decl = Decl::Fn { signature, body };
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

    /// Parses optional generic parameters: '[T, U]' or '[T impl Trait1 Trait2]
    fn parse_generic_params(&mut self) -> Result<Vec<GenericParam>, ParserError> {
        let mut generics = Vec::new();
        // Check if there is an open paren. If not, return empty vec!
        if self.matches_advance(TokenKind::OpenBrack) {
            if !self.matches(TokenKind::CloseBrack) {
                loop {
                    self.try_consume_token(TokenKind::Ident, "Expected generic type identifier")?;
                    let generic_ident = self.unwrap_current_token().unwrap_value();

                    let mut bounds = Vec::new();

                    // If we see a colon, parse the trait bounds!
                    if self.matches_advance(TokenKind::Colon) {
                        loop {
                            self.try_consume_token2(
                                &[TokenKind::Ident, TokenKind::ModulePath],
                                "Expected trait identifier",
                            )?;
                            bounds.push(self.ident_token_to_ident());

                            // If there's a '+', there are more bounds for this generic
                            if self.matches_advance(TokenKind::Plus) {
                                continue;
                            }
                            break; // No more '+', we are done with bounds for this generic
                        }
                    }

                    // Add the generic_ident and its bounds to your list
                    generics.push(GenericParam {
                        ident: generic_ident,
                        bounds: Some(bounds),
                    });

                    if self.matches_advance(TokenKind::CloseBrack) {
                        break;
                    }
                    // Allow trailing commas!
                    if self.matches_advance(TokenKind::Comma) {
                        self.skip_line_terminators();
                        if self.matches_advance(TokenKind::CloseBrack) {
                            break;
                        }
                        continue;
                    }
                    return Err(ParserError::new(
                        "Expected ',' or ')'",
                        self.peek().cloned(),
                    ));
                }
            }
        }
        Ok(generics)
    }

    /// Parses optional trait implementations: 'impl Trait1, Trait2' or 'impl Trait1[i32], Trait2[string]
    fn parse_impl_traits(&mut self) -> Result<Vec<TraitBound>, ParserError> {
        let mut traits = Vec::new();
        if self.matches_advance(TokenKind::Impl) {
            loop {
                self.try_consume_token2(
                    &[TokenKind::Ident, TokenKind::ModulePath],
                    "Expected trait identifier",
                )?;
                let ident = self.ident_token_to_ident();

                let mut args = Vec::new();
                if self.matches_advance(TokenKind::OpenBrack) {
                    if !self.matches_advance(TokenKind::CloseBrack) {
                        loop {
                            self.skip_line_terminators();

                            // Trait arguments are full type expressions
                            args.push(self.parse_type_expr()?);

                            self.skip_line_terminators();

                            if self.matches_advance(TokenKind::CloseBrack) {
                                break;
                            }
                            if self.matches_advance(TokenKind::Comma) {
                                continue;
                            }
                            return Err(ParserError::new(
                                "Expected ',' or ']'",
                                self.peek().cloned(),
                            ));
                        }
                    }
                }

                traits.push(TraitBound { ident, args });
                if !self.matches_advance(TokenKind::Comma) {
                    break;
                }
                self.skip_line_terminators();
            }
        }
        Ok(traits)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParserError> {
        if let Some(token) = self.peek().cloned() {
            match token.kind {
                TokenKind::Ret => return self.parse_ret_stmt(),
                TokenKind::Defer => return self.parse_defer_stmt(),
                TokenKind::Break => return self.parse_break_stmt(),
                TokenKind::Next => return Ok(Stmt::Next),
                TokenKind::OpenBrace => return Ok(Stmt::CodeBlock(self.parse_code_block()?)),
                TokenKind::Ident | TokenKind::ModulePath => {
                    // Parse variable declarations
                    if let Some(var_decl) = self.parse_var_decl()? {
                        // Make sure to consume the line break / semicolon here if needed
                        self.skip_tokens(TokenKind::LineBreak);
                        return Ok(var_decl);
                    }
                }
                _ => {}
            }
            // If it wasn't a dedicated statement or a variable declaration,
            // it MUST be an expression statement (e.g. `foo();` or `a = 10;` or `a.b += 5;`)
            let expr = self.parse_expression()?;

            // If the next token isn't a valid terminator or the end of a block, throw an error!
            if !self.matches_any(&[TokenKind::LineBreak, TokenKind::Semicolon])
                && !self
                    .peek()
                    .is_some_and(|t| t.is_of_kind(TokenKind::CloseBrace))
            {
                return Err(ParserError::new(
                    "Expected newline or ';' after expression",
                    self.peek().cloned(),
                ));
            }

            self.skip_line_terminators();
            return Ok(Stmt::Expr(expr));
        }
        Err(ParserError::new(
            "Couldn't parse statement",
            self.current_token(),
        ))
    }

    // Parses statements of this form:
    // a i32 := 10   | DeclAssignmt
    // a := 10       | DeclAssignmt (no type identifier. type to be inferred)
    // a [3]i32 := 10| DeclAssignmt with complex type
    fn parse_var_decl(&mut self) -> Result<Option<Stmt>, ParserError> {
        // 1. Scan ahead to find out if this statement contains a ':='
        let mut i = 0;
        let mut is_decl = false;

        loop {
            match self.peek_nth(i) {
                Some(token) if token.kind == TokenKind::DeclareAssign => {
                    is_decl = true;
                    break;
                }
                Some(token)
                    if token.kind == TokenKind::LineBreak
                        || token.kind == TokenKind::Semicolon
                        || token.kind == TokenKind::Equals
                        || token.kind == TokenKind::OpenBrace =>
                {
                    // We hit a statement boundary or an assignment, so it's not a var decl
                    break;
                }
                None => break, // EOF
                _ => i += 1,
            }
        }

        // 2. If no ':=' was found, bail out so parse_stmt can parse it as an expression.
        if !is_decl {
            return Ok(None);
        }

        // 3. We are guaranteed this is a variable declaration now!

        // consume the variable identifier
        let ident = self.advance().unwrap().unwrap_value();

        // 4. Parse the optional type expression
        let mut type_expr = None;
        if !self.matches(TokenKind::DeclareAssign) {
            // If the next token isn't ':=', there must be a type expression here
            type_expr = Some(self.parse_type_expr()?);
        }

        // 5. Consume ':=' and parse the value
        self.try_consume_token(TokenKind::DeclareAssign, "Expected ':='")?;
        let value = self.parse_expression()?;

        Ok(Some(Stmt::LocalVarDeclAssign {
            ident,
            type_expr,
            value,
        }))
    }

    // like '[1,2,3,4,5,6,7,8]'
    fn parse_array_literal(&mut self) -> Result<Expr, ParserError> {
        let mut elements = Vec::new();
        if !self.matches_advance(TokenKind::CloseBrack) {
            loop {
                let element = self.parse_expression()?;
                elements.push(element);

                self.skip_line_terminators();

                if self.matches_advance(TokenKind::CloseBrack) {
                    break;
                }
                if self.matches_advance(TokenKind::Comma) {
                    self.skip_line_terminators();

                    if self.matches_advance(TokenKind::CloseBrack) {
                        break;
                    }
                    continue;
                }
                return ParserError::new(
                    "Expected comma ',' or closed bracket ']'",
                    self.peek().cloned(),
                )
                .wrap();
            }
        }
        return Ok(Expr::ArrayLiteral { elements });
    }

    #[track_caller]
    fn parse_type_expr(&mut self) -> Result<TypeExpr, ParserError> {
        debug!("parsing type expr from: {}", std::panic::Location::caller());
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

            self.try_consume_token(TokenKind::CloseBrack, "Expected closed bracket ']'")?;
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
            let base_ident = self.ident_token_to_ident();

            // generic type
            if self.matches_advance(TokenKind::OpenBrack) {
                let mut generic_params = Vec::new();
                if !self.matches(TokenKind::CloseBrack) {
                    loop {
                        self.skip_line_terminators();
                        self.try_consume_token(
                            TokenKind::Ident,
                            "Expected generic type identifier",
                        )?;
                        generic_params.push(self.parse_type_expr()?);
                        self.skip_line_terminators();

                        if self.matches_advance(TokenKind::CloseBrack) {
                            break;
                        }
                        if self.matches_advance(TokenKind::Comma) {
                            continue;
                        }
                        return ParserError::new(
                            "Expected ',' or ']' after generic type argument",
                            self.peek().cloned(),
                        )
                        .wrap();
                    }
                }
                return Ok(TypeExpr::Generic {
                    ident: base_ident,
                    params: generic_params,
                });
            }

            // not generic, just simple ident.
            return Ok(TypeExpr::Ident(base_ident));
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

                self.skip_line_terminators();

                if self.matches_advance(TokenKind::CloseParen) {
                    break;
                }
                if self.matches_advance(TokenKind::Comma) {
                    self.skip_line_terminators(); // added this

                    // trailing commas
                    // If we skipped the comma/newlines and hit a ')', we are done.
                    if self.matches_advance(TokenKind::CloseParen) {
                        break;
                    }
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

    // TODO:
    // for-in loop for collections, for loop without condition (like loop keyword in Rust)
    // TODO: Make it be an expression for break with value.
    //
    // Parsing:
    // for <bool expr>? <code block>
    fn parse_for_loop(&mut self) -> Result<Expr, ParserError> {
        if let Some(token) = self.peek() {
            if token.kind == TokenKind::OpenBrace {
                let code_block = self.parse_code_block()?;
                return Ok(Expr::ForLoop {
                    condition: None,
                    code_block,
                });
            }
        }
        let condition = Some(self.parse_expression()?.boxed());
        let code_block = self.parse_code_block()?;
        let stmt = Expr::ForLoop {
            condition,
            code_block,
        };
        Ok(stmt)
    }

    // TODO:
    // if is expr (like switch or match)
    fn parse_if_else_expr(&mut self) -> Result<Expr, ParserError> {
        let condition = self.parse_expression()?.boxed();
        let if_block = self.parse_code_block()?;
        let else_block = if self.matches_advance(TokenKind::Else) {
            Some(self.parse_code_block()?)
        } else {
            None
        };
        Ok(Expr::If {
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

    fn parse_break_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.advance();
        if self.matches_any(&[TokenKind::LineBreak, TokenKind::Semicolon]) {
            return Ok(Stmt::Break { value: None });
        }
        let expr = self.parse_expression()?;
        Ok(Stmt::Break { value: Some(expr) })
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

        let generics = self.parse_generic_params()?;
        let impl_traits = self.parse_impl_traits()?;

        self.skip_tokens(TokenKind::LineBreak);
        self.try_consume_token(TokenKind::OpenBrace, "Expected open brace or impl")?; // this error message is weird
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
            generics,
            impl_traits,
        })
    }

    fn parse_sum_decl(&mut self) -> Result<Decl, ParserError> {
        self.advance(); // consume 'sum' (assuming TokenKind::Sum)

        self.try_consume_token(TokenKind::Ident, "Expected sum type identifier")?;
        let sum_ident = self.unwrap_current_token().unwrap_value();

        let generics = self.parse_generic_params()?;
        let impl_traits = self.parse_impl_traits()?;

        self.skip_line_terminators();
        self.try_consume_token(TokenKind::OpenBrace, "Expected '{'")?;

        let mut variants = Vec::new();

        loop {
            self.skip_line_terminators();
            if self.matches_advance(TokenKind::CloseBrace) {
                break;
            }

            self.try_consume_token(TokenKind::Ident, "Expected variant identifier")?;
            let variant_ident = self.unwrap_current_token().unwrap_value();

            // Figure out which kind of variant this is
            let data = if self.matches_advance(TokenKind::OpenParen) {
                // 1. Tuple Variant
                let mut types = Vec::new();
                if !self.matches_advance(TokenKind::CloseParen) {
                    loop {
                        self.skip_line_terminators();
                        types.push(self.parse_type_expr()?);
                        self.skip_line_terminators();

                        if self.matches_advance(TokenKind::CloseParen) {
                            break;
                        }
                        self.try_consume_token(TokenKind::Comma, "Expected ',' or ')'")?;
                    }
                }
                VariantData::Tuple(types)
            } else if self.matches_advance(TokenKind::OpenBrace) {
                // 2. Struct Variant
                let mut fields = Vec::new();
                loop {
                    self.skip_line_terminators();
                    if self.matches_advance(TokenKind::CloseBrace) {
                        break;
                    }

                    self.try_consume_token(TokenKind::Ident, "Expected field identifier")?;
                    let field_ident = self.unwrap_current_token().unwrap_value();
                    let type_expr = self.parse_type_expr()?;

                    fields.push(TypedVar::new(field_ident, type_expr));

                    // Fields can be separated by commas or linebreaks
                    self.matches_any_advance(&[
                        TokenKind::Comma,
                        TokenKind::LineBreak,
                        TokenKind::Semicolon,
                    ]);
                }
                VariantData::Struct(fields)
            } else {
                // 3. Unit Variant
                VariantData::Unit
            };

            variants.push(Variant {
                ident: variant_ident,
                data,
            });

            // Variants themselves must be separated by commas or linebreaks
            if !self
                .peek()
                .is_some_and(|t| t.is_of_kind(TokenKind::CloseBrace))
            {
                self.try_consume_token2(
                    &[TokenKind::LineBreak, TokenKind::Comma, TokenKind::Semicolon],
                    "Expected ',', or linebreak between variants",
                )?;
            }
        }

        Ok(Decl::Sum {
            ident: sum_ident,
            generics,
            impl_traits,
            variants,
        })
    }

    fn parse_trait_decl(&mut self) -> Result<Decl, ParserError> {
        self.advance(); // Consume 'trait' keyword

        self.try_consume_token(TokenKind::Ident, "Expected trait identifier")?;
        let ident = self.unwrap_current_token().unwrap_value();

        let generics = self.parse_generic_params()?;

        self.skip_line_terminators();
        self.try_consume_token(TokenKind::OpenBrace, "Expected '{'")?;

        let mut methods = Vec::new();

        loop {
            self.skip_line_terminators();
            if self.matches_advance(TokenKind::CloseBrace) {
                break;
            }

            methods.push(self.parse_fn_signature()?);

            // Allow (but don't strictly require) linebreaks/semicolons between methods
            self.skip_line_terminators();
        }

        Ok(Decl::Trait {
            ident,
            generics,
            methods,
        })
    }

    // :Expressions

    fn parse_expression(&mut self) -> ExprParseResult {
        debug!("parsing expression {}", self.parser_state_dbg_info());
        self.expr(0)
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
    fn skip_tokens_of_kinds(&mut self, token_kinds: &[TokenKind]) {
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

    fn skip_line_terminators(&mut self) {
        self.skip_tokens_of_kinds(&[TokenKind::LineBreak, TokenKind::Semicolon]);
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
