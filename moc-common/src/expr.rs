use std::{collections::VecDeque, fmt::Display};

use serde::Serialize;

use crate::{
    debug_utils::create_indent, token::{NumberLiteralType, Token}, ModulePath
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

impl FnCall {
    pub fn display_inner(&self, depth: usize, result: &mut String) {
        result.push_str(&create_indent(depth));
        if let Some(mod_ident) = &self.mod_ident {
            result.push_str(&format!("FnCall: {}:\"{}\"", mod_ident, self.ident));
        } else {
            result.push_str(&format!("FnCall: \"{}\"", self.ident));
        }
        for arg in &self.args {
            arg.display_inner(depth, result);
        }
    }
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
            Expr::DotExpr(dot_expr) => {
                result.push_str(&format!("DotExpr: "));
                match dot_expr {
                    DotExpr::FnCall{fn_call, called_on} => {
                        result.push_str(&format!("{}Called on: ", &create_indent(depth + 1)));
                        called_on.display_inner(depth + 1, result);
                        fn_call.display_inner(depth + 1, result);
                    },
                    DotExpr::FieldAccess { field_ident, called_on } => {
                        result.push_str(&format!("{}Called on: ", &create_indent(depth + 1)));
                        called_on.display_inner(depth + 1, result);
                        result.push_str(&format!("{}Field access: \"{}\"", create_indent(depth + 1), field_ident));
                    }
                }
            }
            Expr::DotExprChain(chain) => {
                result.push_str(&format!("DotExprChain"));
                for expr in chain {
                    Expr::DotExpr(expr.clone()).display_inner(depth, result);
                }
            }
            Expr::Empty => {
                result.push_str(&format!("{}Empty", create_indent(depth)));
            }
            Expr::Grouping(astnode) => {
                result.push_str(&format!("{}Grouping: ", create_indent(depth)));
                astnode.display_inner(depth, result);
            }
            Expr::Variable { ident, module_path_prefix: mod_ident } => {
                if let Some(mod_ident) = mod_ident {
                    result.push_str(&format!("Variable: {}:\"{}\"", mod_ident, ident));
                } else {
                    result.push_str(&format!("Variable: \"{}\"", ident));
                }
            }
            Expr::FnCall(fn_call) => {
                fn_call.display_inner(depth, result);
            }
            Expr::NumberLiteral(num, r#type) => {
                result.push_str(&format!("NumberLiteral: {} {}", num, r#type));
            }
            Expr::StringLiteral(str) => {
                result.push_str(&format!(
                    "StringLiteral: {}",
                    str.escape_default().to_string()
                ));
            }
            Expr::Unary(op, astnode) => {
                result.push_str(&format!("Unary: {:?}", op));
                astnode.display_inner(depth, result);
            }
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.display())
    }
}
