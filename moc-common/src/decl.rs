use serde::Serialize;

use crate::{CodeBlock, ModulePath, TypedVar, expr::TypeExpr};

#[derive(Debug, Clone, Serialize)]
pub enum Decl {
    Fn {
        // function declaration
        ident: String,
        params: Vec<TypedVar>,
        return_type: Option<TypeExpr>,
        body: CodeBlock,
    },
    Use {
        module_ident: ModulePath,
        module_alias: Option<String>,
    },
    Struct {
        ident: String,
        fields: Vec<TypedVar>,
    }, // variable?
}
