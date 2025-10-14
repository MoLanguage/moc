use std::collections::HashMap;

// builtin types:
// Int8, Int16, Int32, Int64
// 

pub enum Type {
    Builtin(BuiltinType),
    Custom(CustomType),
    Array(Box<Type>),
    Pointer(Box<Type>)
}

pub struct CustomType {
    pub name: String,
    pub byte_width: usize
}

pub enum BuiltinType {
    Int8, Int16, Int32, Int64, Int128,
    Bool, // in the future maybe bools of other widths
    Unknown,
    Empty // for expression statements with no return type
}

pub struct Function {
    pub name: String,
    pub return_type: Type
}

pub enum Symbol {
    Function {
        name: String,
        params: Vec<(String, Type)>
    }
}

pub struct SymbolTable {
    pub functions: HashMap<String, Function>
}

pub struct ScopeStack {
    
}