use serde::Serialize;

use crate::{CodeBlock, ModulePath, TypedVar};

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
        module_ident: ModulePath,
        module_alias: Option<String>,
    },
    Struct {
        ident: String,
        fields: Vec<TypedVar>,
    }, // variable?
}
