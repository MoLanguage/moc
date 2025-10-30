use std::collections::VecDeque;

use serde::Serialize;

use crate::{
    token::{NumberLiteralType, Token}, ModulePath
};

#[derive(Debug, Clone, Serialize)]
pub enum Expr {
    // Expressions
    Binary {
        left_expr: Box<Expr>,
        operator: Token,
        right_expr: Box<Expr>,
    },
    DotExpr(DotExpr),
    DotExprChain(VecDeque<DotExpr>),
    BoolLiteral(bool),
    FnCall(FnCall),
    Grouping(Box<Expr>),
    Variable {
        module_path_prefix: Option<ModulePath>, // for e.g. constants imported from other module
        ident: String,
    },
    NumberLiteral(String, NumberLiteralType),

    StringLiteral(String),
    Unary(Token, Box<Expr>), // Operator followed by another expr
    Empty
}

#[derive(Debug, Clone, Serialize)]
pub struct FnCall {
    pub mod_ident: Option<ModulePath>,
    pub ident: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, Serialize)]
pub enum DotExpr {
    FnCall { called_on: Box<Expr>, fn_call: FnCall },
    FieldAccess { called_on: Box<Expr>, field_ident: String },
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
}
