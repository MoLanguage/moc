use crate::token::{Token, TokenType};

pub fn print_tokens(tokens: &[Token]) {
    for token in tokens {
        dbg!(&token);
        if token.r#type == TokenType::EndOfFile {
            break;
        }
    }
}

pub const INDENT: &str = "  ";
pub fn create_indent(depth: usize) -> String {
    format!("{}{}", "\n", INDENT.repeat(depth))
}
