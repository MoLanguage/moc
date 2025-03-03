use std::{fs::File, io::Read};

use clap::Parser;
use moc_parser::{lexer::Lexer, parser, CodeLocation, Token, TokenType};

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
                        if token._type == TokenType::EndOfFile {
                            break;
                        }
                    }
                    Err(err) => {
                        eprintln!("{:?}", err);
                        break;
                    }
                }
            }

            //let mut parser = parser::Parser::new(lexer);
            //println!("{:?}", parser.parse().unwrap());
        }
        Err(err) => {
            eprintln!("{}", err)
        }
    };
}
