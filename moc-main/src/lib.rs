use std::{fs::File, io::Read, path::Path};

use moc_common::{error::CompilerError, stmt::Stmt, token::Token};
use moc_parser::{lexer::Lexer, parser::Parser};

pub struct CompileResultMetaData {
    pub tokens: Option<Vec<Token>>,
    pub ast: Option<Vec<Stmt>>,
}

impl CompileResultMetaData {
    fn new() -> Self {
        Self { tokens: None, ast: None }
    }
}

pub fn compile_file(path: impl AsRef<Path>) -> Result<CompileResultMetaData, CompilerError>{
    let mut src = String::new();
    let mut meta_data = CompileResultMetaData::new();
    match File::open(path) {
        Ok(mut file) => {
            file.read_to_string(&mut src).unwrap();
            let lexer = Lexer::new(&src);
            let parser = Parser::new(lexer);
            match parser.parse() {
                Ok(ast) => {
                    meta_data.ast = Some(ast);
                },
                Err(err) => { return Err(CompilerError::ParserError(err)) }
            }
            
        }
        Err(io_error) => {
            return Err(CompilerError::FileNotFound(io_error))
        }
    };
    Ok(meta_data)
}
