use std::{fs::File, io::Read, path::Path};

use moc_common::{error::CompilerError, stmt::Stmt, token::Token};
use moc_parser::{lexer::Lexer, parser::Parser};

pub struct CompileResultMetaData {
    pub tokens: Option<Vec<Token>>,
    pub ast: Option<Vec<Stmt>>,
}

impl CompileResultMetaData {
    pub fn new() -> Self {
        Self { tokens: None, ast: None }
    }
}

pub struct CompilerOptions {
    pub emit_ast: bool,
    pub emit_tokens: bool,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            emit_ast: false,
            emit_tokens: false,
        }
    }
}

pub fn compile_file(path: impl AsRef<Path>, options: CompilerOptions) -> Result<CompileResultMetaData, CompilerError> {
    let mut src = String::new();
    let mut meta_data = CompileResultMetaData::new();
    match File::open(path) {
        Ok(mut file) => {
            file.read_to_string(&mut src).unwrap();
            let lexer = Lexer::new(&src);
            let tokens = lexer.tokens();
            if options.emit_tokens {
                meta_data.tokens = Some(tokens.clone());
            }
            let parser = Parser::new(tokens);
            match parser.parse() {
                Ok(ast) => {
                    if options.emit_ast {
                        meta_data.ast = Some(ast.clone());
                    }
                    // TODO: continue into semantic analysis
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
