use serde::Serialize;

use crate::{
    CodeBlock, TypedVar,
    expr::{Expr, GenericParam, Ident, TraitBound, TypeExpr},
};

#[derive(Debug, Clone, Serialize)]
pub struct FnSignature {
    pub ident: String,
    pub generics: Vec<GenericParam>, // Using the struct from our last step!
    pub params: Vec<TypedVar>,
    pub return_type: Option<TypeExpr>,
}

#[derive(Debug, Clone, Serialize)]
pub enum Decl {
    Fn {
        // function declaration
        signature: FnSignature,
        body: CodeBlock,
    },
    Use {
        path: Ident,
        alias: Option<String>,
    },
    Struct {
        ident: String,
        fields: Vec<TypedVar>,
        generics: Vec<GenericParam>, // e.g., ["T", "U"]
        impl_traits: Vec<TraitBound>,     // NEW: e.g., [PartialOrd]
    },
    Sum {
        ident: String,
        generics: Vec<GenericParam>,
        impl_traits: Vec<TraitBound>,
        variants: Vec<Variant>,
    },
    Trait {
        ident: String,
        generics: Vec<GenericParam>, // Traits can be generic too!
        methods: Vec<FnSignature>,
    },
    // global variable/constant?
    // Only for debugging stuff! If I want to just test parsing expressions without all the other shebang.
    LooseExpr(Expr),
}

// sum type variant stuff
#[derive(Debug, Clone, Serialize)]
pub enum VariantData {
    Unit,
    Tuple(Vec<TypeExpr>),
    Struct(Vec<TypedVar>),
}

#[derive(Debug, Clone, Serialize)]
pub struct Variant {
    pub ident: String,
    pub data: VariantData,
}
