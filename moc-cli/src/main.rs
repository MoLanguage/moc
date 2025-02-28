use std::{fs::File, io::Read};

use moc_parser::{lexer::Lexer, parser};
use clap::Parser;


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
            let lexer = Lexer::new(&src);
            lexer.clone().for_each(|token| println!("{:?}", token));
            let mut parser = parser::Parser::new(lexer);
            println!("{:?}", parser.parse().unwrap());
        },
        Err(err) => {
            eprintln!("{}", err)
        },
    };

}
