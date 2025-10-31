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
        ident: Ident
    },
    NumberLiteral(String, NumberLiteralType),

    StringLiteral(String),
    Unary(Token, Box<Expr>), // Operator followed by another expr
    Empty
}

#[derive(Debug, Clone, Serialize)]
pub struct FnCall {
    pub ident: Ident,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, Serialize)]
pub enum DotExpr {
    FnCall { called_on: Box<Expr>, fn_call: FnCall },
    FieldAccess { called_on: Box<Expr>, field_ident: String },
}

#[derive(Debug, Clone, Serialize)]
pub enum Ident {
    Simple(String),
    WithModulePrefix(ModulePath, String)
}

impl Ident {
    /// Gets the ident if of variant Simple, else gets the suffix.
    pub fn base(&self) -> &String {
        match self {
            Ident::Simple(ident) => &ident,
            Ident::WithModulePrefix(_, suffix) => &suffix,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum TypeExpr {
    Ident(Ident),
    Pointer(Box<TypeExpr>)
}

impl TypeExpr {
    pub fn pointer(type_expr: Self) -> Self {
        Self::Pointer(Box::new(type_expr))
    }
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
