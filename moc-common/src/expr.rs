use std::fmt::Display;

use crate::{debug_utils::{create_indent}, token::Token};

#[derive(Debug, Clone)]
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

    fn display(&self) -> String {
        let mut s = String::new();
        self.display_inner(0, &mut s);
        return s;
    }

    pub fn display_inner(&self, depth: usize, result: &mut String) {
        let depth = depth + 1;
        result.push_str(&create_indent(depth));
        match self {
            Expr::Binary {
                left_expr,
                operator,
                right_expr,
            } => {
                result.push_str(&format!("Binary: {:?}", operator.r#type));
                left_expr.display_inner(depth, result);
                right_expr.display_inner(depth, result);
            }
            Expr::BoolLiteral(bool) => result.push_str(&bool.to_string()),
            Expr::Grouping(astnode) => {
                result.push_str("Grouping: ");
                astnode.display_inner(depth, result);
            }
            Expr::VariableIdent(ident) => {
                result.push_str(&format!("VariableIdent: \"{}\"", ident));
            }
            Expr::FnCall { ident, args } => {
                result.push_str(&format!("FnCall: {}", ident));
                for arg in args {
                    arg.display_inner(depth, result);
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
                astnode.display_inner(depth, result);
            }
            Expr::EndOfFile => {
                result.push_str("EOF");
            }
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.display())
    }
}