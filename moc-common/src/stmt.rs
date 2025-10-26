use std::fmt::Display;

use serde::Serialize;

use crate::{debug_utils::create_indent, expr::Expr, BinaryOp, CodeBlock};

#[derive(Clone, Debug, Serialize)]
pub enum Stmt {
    Print(Expr), // probably dont wanna have this as inbuilt function
    // a int32 (declaring variable)
    LocalVarDecl {
        ident: String,
        type_ident: String
    },
    // a = 10 (updating value)
    LocalVarAssign {
        ident: String,
        value: Expr,
    },
    // a += 10, a -= 10 etc. (operating and assigning)
    VarOperatorAssign {
        ident: String,
        operator: BinaryOp,
        value: Expr,
    },
    // a int32 := 10 OR a := 10 (infers type)
    LocalVarDeclAssign {
        ident: String,
        type_ident: Option<String>,
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

impl Stmt {
    fn display(&self) -> String {
        let mut s = String::new();
        self.display_inner(0, &mut s);
        return s;
    }

    pub fn display_inner(&self, depth: usize, result: &mut String) {
        let depth = depth + 1;
        result.push_str(&create_indent(depth));
        match self {
            Stmt::Print(astnode) => {
                result.push_str("Print");
                astnode.display_inner(depth, result);
            }
            Stmt::LocalVarDecl {
                ident: var_ident,
                type_ident,
            } => {
                result.push_str(&format!("VarDecl: \"{}\" {}", type_ident, var_ident));
            }
            Stmt::Break => {
                result.push_str("Break");
            }
            Stmt::If {
                condition,
                if_block,
                else_block,
            } => {
                result.push_str("If: ");
                condition.display_inner(depth, result);
                for if_block_stmt in &if_block.stmts {
                    if_block_stmt.display_inner(depth, result);
                }
                if let Some(else_block) = else_block {
                    result.push_str(&create_indent(depth));
                    result.push_str("Else: ");
                    for else_block_stmt in &else_block.stmts {
                        else_block_stmt.display_inner(depth, result);
                    }
                }
            }
            Stmt::Ret(expr) => {
                result.push_str("Ret: ");
                expr.display_inner(depth, result);
            }
            Stmt::LocalVarAssign { ident, value } => {
                result.push_str(&format!("VarAssign \"{}\"", ident));
                value.display_inner(depth, result);
            }
            Stmt::LocalVarDeclAssign {
                ident,
                type_ident,
                value,
            } => {
                result.push_str(&format!("VarDeclAssign \"{}\"", ident));
                if let Some(type_ident) = type_ident {
                    result.push_str(&format!(" {}", type_ident));
                }
                value.display_inner(depth, result);
            }
            Stmt::Expr(expr) => {
                result.push_str("Expr statement: ");
                expr.display_inner(depth, result);
            }
            Stmt::ForLoop {
                condition,
                code_block,
            } => {
                result.push_str("ForLoop: ");
                condition.display_inner(depth, result);
                for stmt in &code_block.stmts {
                    stmt.display_inner(depth, result);
                }
            }
            Stmt::VarOperatorAssign {
                ident,
                operator,
                value,
            } => {
                result.push_str("VarOperatorAssign: ");
                result.push_str(&format!("Ident: \"{}\" ", ident));
                result.push_str(&format!("Operator: \"{}\" ", operator));

                //result.push_str(&create_indent(depth + 1));
                result.push_str("Value: ");

                value.display_inner(depth, result);
            }
            //_ => result.push_str("ERR: Stmt not yet registered in print function"),
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.display())
    }
}
