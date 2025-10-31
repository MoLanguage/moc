use serde::Serialize;

use crate::{BinaryOp, CodeBlock, expr::{Expr, TypeExpr}};

#[derive(Clone, Debug, Serialize)]
pub enum Stmt {
    Print(Expr), // probably dont wanna have this as inbuilt function
    // a i32 (declaring variable)
    LocalVarDecl {
        ident: String,
        type_expr: TypeExpr
    },
    // <expr> = <expr> (updating value)
    Assignmt {
        assignee: Expr,
        new_value: Expr
    },
    // a += 10, a -= 10 etc. (operating and assigning)
    VarOperatorAssign {
        assignee: Expr,
        operator: BinaryOp,
        value: Expr,
    },
    // a i32 := 10 OR a := 10 (infers type)
    LocalVarDeclAssign {
        ident: String,
        type_expr: Option<TypeExpr>,
        value: Expr,
    },
    Break,
    Expr(Expr), // expression statement (like function call)
    ForLoop {
        condition: Expr,
        code_block: CodeBlock,
    },
    If {
        condition: Expr,
        if_block: CodeBlock,
        else_block: Option<CodeBlock>,
    },
    Ret(Expr), // return statement
}
