use std::{fs::File, io::Read, path::Path};

use moc_common::{ast::Ast, error::CompilerError, token::Token};
use moc_parser::{lexer::Lexer, parser::Parser};

pub struct CompileResultData {
    pub tokens: Option<Vec<Token>>,
    pub ast: Option<Ast>,
    pub errors: Vec<CompilerError>,
}

impl CompileResultData {
    pub fn new() -> Self {
        Self {
            tokens: None,
            ast: None,
            errors: Vec::new(),
        }
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
            let mut lexer = Lexer::new(&src);
            let tokens = lexer.tokens();
            for lexer_error in lexer.errors {
                meta_data
                    .errors
                    .push(CompilerError::LexerError(lexer_error));
            }
            if options.emit_tokens {
                meta_data.tokens = Some(tokens.clone()); // TODO: Find out how how to not clone all the tokens
            }
            println!("Parsing now");

            let only_expr = false; // TODO: Make this a compiler option
            let mut parser = Parser::new(tokens, only_expr);
            let ast = parser.parse();
            println!("Done parsing");

            if options.emit_ast {
                meta_data.ast = Some(ast.clone());
            }
            for parser_error in &parser.errors {
                meta_data
                    .errors
                    .push(CompilerError::ParserError(parser_error.clone()));
            }

            if parser.errors.len() == 0 {
                // TODO: continue into semantic analysis
            }
        }
        Err(io_error) => {
            meta_data.errors.push(CompilerError::FileNotFound(io_error));
        }
    }
    meta_data
}
