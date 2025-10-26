use std::fmt::Display;

use serde::Serialize;

use crate::{debug_utils::create_indent, CodeBlock, ModIdent, TypedVar};

#[derive(Debug, Clone, Serialize)]
pub enum Decl {
    Fn {
        // function declaration
        ident: String,
        params: Vec<TypedVar>,
        return_type: Option<String>,
        body: CodeBlock,
    },
    Use {
        module_ident: ModIdent,
        module_alias: Option<String>,
    },
    Struct {
        ident: String,
        fields: Vec<TypedVar>,
    }, // variable?
}

impl Decl {
    pub fn display(&self) -> String {
        let mut result = String::new();
        self.display_inner(0, &mut result);
        result
    }
    
    pub fn display_inner(&self, depth: usize, result: &mut String) {
        match self {
            Decl::Use {
                module_ident,
                module_alias,
            } => {
                result.push_str(&format!("Use {}", module_ident));
                if let Some(mod_rename) = module_alias {
                    result.push_str(&format!(" Alias: \"{}\"", mod_rename));
                }
            }
            Decl::Fn {
                ident,
                body,
                return_type,
                params,
            } => {
                result.push_str(&format!("FnDecl: {}(", ident));
                for (i, param) in params.iter().enumerate() {
                    result.push_str(&format!("{} {}", param.ident, param.type_ident));
                    if i < params.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push_str(")");
                if let Some(return_type) = return_type {
                    result.push_str(&format!(" {}", return_type));
                }
                for stmt in &body.stmts {
                    stmt.display_inner(depth, result);
                }
            }
            Decl::Struct {
                ident,
                fields: body,
            } => {
                result.push_str(&format!("StructDecl: {}", ident));
                for field_decl in body {
                    result.push_str(&format!(
                        "{}{:?} \"{}\"",
                        create_indent(depth + 1), field_decl.type_ident, field_decl.ident
                    ));
                }
            }
        }
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.display())
    }
}
