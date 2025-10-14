use clap::Parser;
use moc_common::{debug_utils, error::CompilerError};
use moc_main::CompilerOptions;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path to .mo file
    #[arg(short, long)]
    path: String,
    /// Whether to print the token output of the lexer
    #[arg(long)]
    print_tokens: bool,
    /// Whether to print the AST output of the parser
    #[arg(long)]
    print_ast: bool,
}

fn main() {
    simple_logger::init_with_env().unwrap(); // just for debug toggleable debug logging
    
    let args = Args::parse();
    let mut options = CompilerOptions::default();
    options.emit_tokens = args.print_tokens;
    options.emit_ast = args.print_ast;
    match moc_main::compile_file(args.path, options) {
        Ok(meta) => {
            if let Some(ast) = meta.ast {
                for stmt in ast {
                    println!("{}", stmt);
                }
            }
            if let Some(tokens) = meta.tokens {
                debug_utils::print_tokens(&tokens);
            }
        }
        Err(err) => match err {
            CompilerError::ParserError(parser_error) => {
                println!("{:?}", parser_error);
            }
            CompilerError::LexerError(lexer_error) => {
                println!("{:?}", lexer_error);
            }
            CompilerError::FileNotFound(error) => {
                println!("{:?}", error);
            }
        },
    }
}
