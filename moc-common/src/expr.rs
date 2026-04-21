use serde::Serialize;

use crate::{
    CodeBlock, ModulePath,
    op::{BinaryOp, UnaryOp},
    token::{NumberLiteralKind, TokenKind},
};

#[derive(Debug, Clone, Serialize)]
pub enum Expr {
    // Expressions
    Assign {
        assignee: Box<Expr>,
        operator: TokenKind, // TODO: maybe make this use it's own enum type like AssignmentOp?
        value: Box<Expr>,
    },
    Binary {
        left_expr: Box<Expr>,
        operator: BinaryOp,
        right_expr: Box<Expr>,
    },
    FieldAccess {
        called_on: Box<Expr>,
        member_ident: Ident,
    },
    ArrayLiteral {
        elements: Vec<Expr>,
    },
    ArrayAccessor {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    ForLoop {
        condition: Option<Box<Expr>>, // if None, is infinite loop
        code_block: CodeBlock,
    },
    If {
        condition: Box<Expr>,
        if_block: CodeBlock,
        else_block: Option<CodeBlock>,
    },
    BoolLiteral(bool),
    Grouping(Box<Expr>),
    FnCall {
        callee: Box<Expr>, // callee / what value, what expression the function is being called on. 
        args: Vec<Expr>,
    },
    
    // this is only part. A full generic FnCall consists of FnCall with callee being an expr of type GenericFnCallPart.
    GenericFnCallPart {
        callee: Box<Expr>,
        type_args: Option<Vec<TypeExpr>>, 
    },
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
    Generic {
        ident: Ident,
        params: Vec<TypeExpr>, // identifiers of generic type parameters 
    },
}


// used in struct/sum declarations impl items.
// struct A impl Trait[i32] 
// (the [T] is optional. only for generic traits)
// maybe rename to TraitImplDecl or something. I need to sleep.
#[derive(Debug, Clone, Serialize)]
pub struct TraitBound {
    pub ident: Ident,
    pub args: Vec<TypeExpr>,
}

#[derive(Debug, Clone, Serialize)]
pub struct GenericParam {
    pub ident: String,
    pub bounds: Option<Vec<Ident>>,
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
