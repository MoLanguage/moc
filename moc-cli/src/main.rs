use std::{fs::File, io::Read};

use clap::Parser;
use moc_parser::{
    lexer::Lexer, parser::{self, ParserError}, TokenType
};

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
                        //dbg!(&token);
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
            match parser.parse() {
                Ok(ast) => {
                  for stmt in ast {
                      println!("{}", stmt);
                  }
                },
                Err(parse_error) => {
                  print_parser_error(parse_error);
                },
            }
            
        }
        Err(err) => {
            eprintln!("{}", err)
        }
    };
}

fn print_parser_error(parser_error: ParserError) {
    if let Some(token) = parser_error.last_token {
      eprintln!("Parser error at line {}, column {}: {}", token.location.line, token.location.column, parser_error.msg);
      eprintln!("Last token: {}", token.r#type)
    } else {
      eprintln!("Parse error: {}", parser_error.msg);
    }
}
