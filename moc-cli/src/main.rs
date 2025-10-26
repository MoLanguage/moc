use clap::Parser;
use moc_common::{debug_utils};
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
    let result = moc_main::compile_file(args.path, options);

    if let Some(tokens) = result.tokens {
        debug_utils::print_tokens(&tokens);
    }
    if let Some(ast) = result.ast {
        for stmt in ast {
            println!("{}", stmt);
        }
    }
    if !result.errors.is_empty() {
        for error in result.errors {
            println!("Compiler error: {:?}", error)
        }
    }
}
