use std::{fs::File, io::Read, path::Path};

use log::debug;
use moc_common::{ast::Ast, debug_utils, error::CompilerError, token::Token};
use moc_parser::{lexer::Lexer, parser::Parser};

pub struct CompileResultData {
    pub tokens: Option<Vec<Token>>,
    pub ast: Option<Ast>,
    pub errors: Vec<CompilerError>,
}

impl CompileResultData {
    pub fn new() -> Self {
        Self { tokens: None, ast: None, errors: Vec::new() }
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

pub fn compile_file(path: impl AsRef<Path>, options: CompilerOptions) -> CompileResultData {
    let mut src = String::new();
    let mut meta_data = CompileResultData::new();
    match File::open(path) {
        Ok(mut file) => {
            file.read_to_string(&mut src).unwrap(); // TO-DO: Handle error
            let lexer = Lexer::new(&src);
            let tokens = lexer.tokens();
            match tokens {
                Ok(tokens) => {
                    if options.emit_tokens {
                        meta_data.tokens = Some(tokens.clone());
                    }
                    println!("Parsing now");
                    let parser = Parser::new(tokens);
                    match parser.parse() {
                        Ok(ast) => {
                            println!("Done parsing");
                            if options.emit_ast {
                                meta_data.ast = Some(ast.clone());
                            }
                            // TODO: continue into semantic analysis
                        },
                        Err(err) => { meta_data.errors.push(CompilerError::ParserError(err)); }
                    }
                },
                Err(lexer_err) => meta_data.errors.push(CompilerError::LexerError(lexer_err)),
            }
        }
        Err(io_error) => {
            meta_data.errors.push(CompilerError::FileNotFound(io_error));
        }
    }
    meta_data
}
