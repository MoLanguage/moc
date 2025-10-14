use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Name of the person to greet
    #[arg(short, long)]
    path: String,
    print_tokens: bool,
}

fn main() {
    let args = Args::parse();
    match moc_main::compile_file(args.path) {
        Ok(meta) => {
            
        },
        Err(compiler_error) => {
            
        },
    }
    
}
