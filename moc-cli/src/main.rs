use std::{env, fs::File, io::Read};

use clap::Parser;
use moc_parser::{CodeLocation, Expr, Token, TokenType, lexer::Lexer, parser};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Name of the person to greet
    #[arg(short, long)]
    path: String,
}

fn main() {
    let args = Args::parse();

    let mut src = String::new();
    match File::open(args.path) {
        Ok(mut file) => {
            file.read_to_string(&mut src).unwrap();
            let mut lexer = Lexer::new(&src);
            loop {
                let token = lexer.next_token();
                match token {
                    Ok(token) => {
                        println!("{:?}", token);
                        if token.r#type == TokenType::EndOfFile {
                            break;
                        }
                    }
                    Err(err) => {
                        eprintln!("{:?}", err);
                        break;
                    }
                }
            }

            let parser = parser::Parser::new(Lexer::new(&src));
            let ast = parser.parse().unwrap();
            for stmt in ast {
                println!("{}", stmt);
            }
        }
        Err(err) => {
            eprintln!("{}", err)
        }
    };
}
