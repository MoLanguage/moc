use std::fmt::Display;

use crate::{debug_utils::{create_indent}, token::Token};

#[derive(Debug)]
pub enum Expr {
    // Expressions
    Binary {
        left_expr: Box<Expr>,
        operator: Token,
        right_expr: Box<Expr>,
    },
    BoolLiteral(bool),
    FnCall {
        ident: String,
        args: Vec<Expr>,
    },
    Grouping(Box<Expr>),
    VariableIdent(String),

    Loop(Vec<Expr>), // simple infinite loop
    NumberLiteral(String),

    StringLiteral(String),
    Unary(Token, Box<Expr>), // Operator followed by another expr

    EndOfFile,
}

impl Expr {
    pub fn binary(left: Self, operator: Token, right: Self) -> Self {
        Self::Binary {
            left_expr: Box::new(left),
            operator,
            right_expr: Box::new(right),
        }
    }
    pub fn grouping(expr: Self) -> Self {
        Self::Grouping(Box::new(expr))
    }
    pub fn unary(operator: Token, right: Self) -> Self {
        Self::Unary(operator, Box::new(right))
    }

    fn print(&self) -> String {
        let mut s = String::new();
        self.print_inner(0, &mut s);
        return s;
    }

    pub fn print_inner(&self, depth: usize, result: &mut String) {
        let depth = depth + 1;
        result.push_str(&create_indent(depth));
        match self {
            /* Expr::Print(astnode) => {
                result.push_str("Print");
                astnode.print_inner(depth, result);
            }
            Expr::VarDecl(ident, astnode) => {
                result.push_str(&format!("VarDecl \"{}\"", ident));
                astnode.print_inner(depth, result);
            }
            Expr::Break => {
                result.push_str("Break");
            } */
            Expr::Binary {
                left_expr,
                operator,
                right_expr,
            } => {
                result.push_str(&format!("Binary: {:?}", operator.r#type));
                left_expr.print_inner(depth, result);
                right_expr.print_inner(depth, result);
            }
            Expr::BoolLiteral(bool) => result.push_str(&bool.to_string()),
            Expr::Grouping(astnode) => {
                result.push_str("Grouping: ");
                astnode.print_inner(depth, result);
            }
            Expr::VariableIdent(ident) => {
                result.push_str(&format!("VariableIdent: \"{}\"", ident));
            }
            Expr::FnCall { ident, args } => {
                result.push_str(&format!("FnCall: {}", ident));
                for arg in args {
                    arg.print_inner(depth, result);
                }
            }
            /* Expr::If { condition, if_block, else_block } => {
                result.push_str("If: ");
                condition.print_inner(depth, result);
                for if_block_stmt in if_block {
                    if_block_stmt.print_inner(depth, result);
                }
                if let Some(else_block) = else_block {
                    for else_block_stmt in else_block {
                        else_block_stmt.print_inner(depth, result);
                    }
                }
            } */
            Expr::Loop(astnodes) => {
                result.push_str("Loop");
                for stmt in astnodes {
                    stmt.print_inner(depth, result);
                }
            }
            Expr::NumberLiteral(num) => {
                result.push_str(&format!("NumberLiteral: {}", num));
            }
            Expr::StringLiteral(str) => {
                result.push_str(&format!("StringLiteral: {}", str));
            }
            Expr::Unary(op, astnode) => {
                result.push_str(&format!("Unary: {:?}", op));
                astnode.print_inner(depth, result);
            }
            /* Expr::UseDecl {
                module_ident,
                module_rename,
            } => {
                result.push_str(&format!("Use \"{}\"", module_ident));
                if let Some(mod_rename) = module_rename {
                    result.push_str(&format!(" {}", mod_rename));
                }
            }
            Expr::FnDecl { ident, body } => {
                result.push_str(&format!("FnDecl: {}", ident));
                for stmt in body {
                    stmt.print_inner(depth, result);
                }
            }
            Expr::StructDecl { ident, body } => {
                result.push_str(&format!("StructDecl: {}", ident));
                for field_decl in body {
                    result.push_str(&format!(
                        "{:?} \"{}\"",
                        field_decl.data_type, field_decl.ident
                    ));
                }
            } */
            Expr::EndOfFile => {
                result.push_str("EOF");
            }
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.print())
    }
}