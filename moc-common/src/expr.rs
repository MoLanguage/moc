use serde::Serialize;

use crate::{
    ModulePath, op::{BinaryOp, UnaryOp}, token::NumberLiteralKind
};

#[derive(Debug, Clone, Serialize)]
pub enum Expr {
    // Expressions
    Binary {
        left_expr: Box<Expr>,
        operator: BinaryOp
        ,
        right_expr: Box<Expr>,
    },
    #[deprecated]
    DotExpr(DotExpr),
    FieldAccess {
        called_on: Box<Expr>,
        member_ident: Ident,
    },
    ArrayLiteral {
        elements: Vec<Expr>,
        type_expr: Option<TypeExpr>,
    },
    ArrayAccessor {
        ident: Ident,
        index: Box<Expr>,
    },
    BoolLiteral(bool),
    FnCall(FnCall),
    Grouping(Box<Expr>),
    Variable {
        ident: Ident,
    },
    NumberLiteral(String, NumberLiteralKind),

    StringLiteral(String),
    Unary {
        operator: UnaryOp,
        expr: Box<Expr>,
    }, // Operator followed by another expr
    Empty,
}

#[derive(Debug, Clone, Serialize)]
pub struct FnCall {
    pub callee: Box<Expr>, // callee / what value, what expression the function is being called on.
    pub args: Vec<Expr>,
}   

#[derive(Debug, Clone, Serialize)]
#[deprecated]
pub enum DotExpr {
    
    FnCall {
        called_on: Box<Expr>,
        fn_call: FnCall,
    },

    FieldAccess {
        called_on: Box<Expr>,
        member_ident: Ident,
    },
}

#[derive(Debug, Clone, Serialize)]
pub enum Ident {
    Simple(String),
    WithModulePrefix(ModulePath, String),
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
    Pointer(Box<TypeExpr>),
    Array {
        length: Option<usize>,
        type_expr: Box<TypeExpr>,
    },
}

impl TypeExpr {
    pub fn pointer(type_expr: Self) -> Self {
        Self::Pointer(Box::new(type_expr))
    }
}

impl Expr {
    pub fn binary(left: Self, operator: BinaryOp, right: Self) -> Self {
        Self::Binary {
            left_expr: Box::new(left),
            operator,
            right_expr: Box::new(right),
        }
    }
    pub fn grouping(expr: Self) -> Self {
        Self::Grouping(Box::new(expr))
    }
    pub fn unary(operator: UnaryOp, right: Self) -> Self {
        Self::Unary {
            operator,
            expr: Box::new(right),
        }
    }
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}
