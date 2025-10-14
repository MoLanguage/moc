use std::fmt::Display;

use crate::{debug_utils::create_indent, expr::Expr, BinaryOp, CodeBlock, ModuleIdentifier, TypedVar};

#[derive(Clone)]
pub enum Stmt {
    Print(Expr), // probably dont wanna have this as inbuilt function
    // Int32 a (declaring variable)
    VarDecl {
        type_ident: String,
        var_ident: String,
    },
    // a = 10 (updating value)
    VarAssign {
        ident: String,
        value: Expr,
    },
    // a += 10, a -= 10 etc. (operating and assigning)
    VarOperatorAssign {
        ident: String,
        operator: BinaryOp,
        value: Expr,
    },
    // Int32 a := 10 OR a := 10 (infers type)
    VarDeclAssign {
        type_ident: Option<String>,
        ident: String,
        value: Expr,
    },
    Break,
    Expr(Expr), // expression statement (like function call)
    UseDecl {
        module_ident: ModuleIdentifier,
        module_alias: Option<String>,
    },
    FnDecl {
        // function declaration
        ident: String,
        params: Vec<TypedVar>,
        return_type: Option<String>,
        body: CodeBlock,
    },
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
    StructDecl {
        ident: String,
        fields: Vec<TypedVar>,
    },
}

impl Stmt {
    fn print(&self) -> String {
        let mut s = String::new();
        self.print_inner(0, &mut s);
        return s;
    }

    fn print_inner(&self, depth: usize, result: &mut String) {
        let depth = depth + 1;
        result.push_str(&create_indent(depth));
        match self {
            Stmt::Print(astnode) => {
                result.push_str("Print");
                astnode.print_inner(depth, result);
            }
            Stmt::VarDecl {
                type_ident,
                var_ident,
            } => {
                result.push_str(&format!("VarDecl: \"{}\" {}", var_ident, type_ident));
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
                condition.print_inner(depth, result);
                for if_block_stmt in &if_block.stmts {
                    if_block_stmt.print_inner(depth, result);
                }
                if let Some(else_block) = else_block {
                    for else_block_stmt in &else_block.stmts {
                        else_block_stmt.print_inner(depth, result);
                    }
                }
            }
            Stmt::UseDecl {
                module_ident,
                module_alias,
            } => {
                result.push_str(&format!("Use {}", module_ident));
                if let Some(mod_rename) = module_alias {
                    result.push_str(&format!(" Alias: \"{}\"", mod_rename));
                }
            }
            Stmt::FnDecl {
                ident,
                body,
                return_type,
                params,
            } => {
                result.push_str(&format!("FnDecl: {}(", ident));
                for (i, param) in params.iter().enumerate() {
                    result.push_str(&format!("{} {}", param.type_ident, param.ident));
                    if i < params.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push_str(")");
                if let Some(return_type) = return_type {
                    result.push_str(&format!(" {}", return_type));
                }
                for stmt in &body.stmts {
                    stmt.print_inner(depth, result);
                }
            }
            Stmt::StructDecl {
                ident,
                fields: body,
            } => {
                result.push_str(&format!("StructDecl: {}", ident));
                for field_decl in body {
                    result.push_str(&format!(
                        "{:?} \"{}\"",
                        field_decl.type_ident, field_decl.ident
                    ));
                }
            }
            Stmt::Ret(expr) => {
                result.push_str("Ret: ");
                expr.print_inner(depth, result);
            }
            Stmt::VarAssign { ident, value } => {
                result.push_str(&format!("VarAssign \"{}\"", ident));
                value.print_inner(depth, result);
            }
            Stmt::VarDeclAssign {
                type_ident,
                ident,
                value,
            } => {
                result.push_str(&format!("VarDeclAssign \"{}\"", ident));
                if let Some(type_ident) = type_ident {
                    result.push_str(&format!(" {}", type_ident));
                }
                value.print_inner(depth, result);
            }
            /*
            Stmt::FnCall(fn_call) => {
                result.push_str(&format!("FnCall: {}", fn_call.ident));
                for arg in &fn_call.args {
                    arg.print_inner(depth, result);
                }
            }
            */
            Stmt::Expr(expr) => {
                result.push_str("Expr statement: ");
                expr.print_inner(depth, result);
            }
            Stmt::ForLoop {
                condition,
                code_block,
            } => {
                result.push_str("ForLoop: ");
                condition.print_inner(depth, result);
                for stmt in &code_block.stmts {
                    stmt.print_inner(depth, result);
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

                value.print_inner(depth, result);
            }
            //_ => result.push_str("ERR: Stmt not yet registered in print function"),
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.print())
    }
}
