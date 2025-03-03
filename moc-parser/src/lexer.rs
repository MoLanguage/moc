use std::iter::Peekable;
use std::str::Chars;

use crate::{Token, TokenLocation, TokenType};

#[derive(Clone)]
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    location: TokenLocation
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
            location: TokenLocation::default()
        }
    }

    fn line_break(&mut self) {
        self.location.column = 0;
        self.location.line += 1;
    }

    fn advance(&mut self) -> Option<char> {
        if self.chars.peek() == Some(&'\r') { // windows CRLF
            self.chars.next();
            if self.chars.peek() == Some(&'\n') {
                self.line_break();
            }
        } else if self.chars.peek() == Some(&'\n') { // linux and mac LF
            self.line_break();
        } else {
            self.location.column += 1; // no line break
        }
        self.chars.next()
    }

    pub fn next_token(&mut self) -> Option<Token> {
        while let Some(ch) = self.chars.peek() {
            let ch = *ch;
            let token = match ch {
                '/' => {
                    self.advance()?;
                    if self.chars.peek() == Some(&'/') {
                        // two slashes means a comment
                        self.skip_comment();
                        continue;
                    }
                    self.lex_operator(ch) // if we have a single slash, it's a divide operator
                }
                ' ' | '\t' | '\r' => {
                    self.advance()?;
                    continue;
                } // Skip whitespace
                '{' => {
                    self.advance()?;
                    Some(Token::new(TokenType::OpenBrace, self.location))
                }
                '}' => {
                    self.advance()?;
                    Some(Token::new(TokenType::CloseBrace, self.location))
                }
                '(' => {
                    self.advance()?;
                    Some(Token::new(TokenType::OpenParen, self.location))
                }
                ')' => {
                    self.advance()?;
                    Some(Token::new(TokenType::CloseParen, self.location))
                }
                ':' => {
                    self.advance()?;
                    if self.chars.peek() == Some(&'=') {
                        self.advance()?;
                        return Some(Token::new(TokenType::Declare, self.location));
                    }
                    Some(Token::new(TokenType::Colon, self.location))
                }
                ';' | '\n' => {
                    self.advance()?;
                    Some(Token::new(TokenType::EndOfStatement, self.location))
                }
                '=' => {
                    self.advance()?;
                    if self.chars.peek() == Some(&'=') {
                        self.advance()?;
                        return Some(Token::new(TokenType::EqualTo, self.location));
                    }
                    Some(Token::new(TokenType::Assign, self.location))
                }
                '+' | '-' | '*' | '%' | '&' | '|' | '^' | '<' | '>' => {
                    self.advance()?;
                    self.lex_operator(ch)
                }
                '0'..='9' => return Some(self.lex_number()),
                'a'..='z' | 'A'..='Z' | '_' => return Some(self.lex_ident()),
                '\"' => return self.lex_string_literal(),
                _ => None, // Ignore unknown characters for now
            };
            return token;
        }
        None
    }

    fn lex_operator(&mut self, ch: char) -> Option<Token> {
        let token;
        if self.chars.peek() == Some(&'=') {
            match ch {
                '+' => token = Some(Token::new(TokenType::AddAssign, self.location)),
                '-' => token = Some(Token::new(TokenType::SubAssign, self.location)),
                '*' => token = Some(Token::new(TokenType::MultAssign, self.location)),
                '%' => token = Some(Token::new(TokenType::ModAssign, self.location)),
                '&' => token = Some(Token::new(TokenType::BitAndAssign, self.location)),
                '|' => token = Some(Token::new(TokenType::BitOrAssign, self.location)),
                '^' => token = Some(Token::new(TokenType::BitXorAssign, self.location)),
                _ => token = None,
            }
        } else {
            match ch {
                '+' => token = Some(Token::new(TokenType::Plus, self.location)),
                '-' => token = Some(Token::new(TokenType::Minus, self.location)),
                '*' => token = Some(Token::new(TokenType::Star, self.location)),
                '%' => token = Some(Token::new(TokenType::Mod, self.location)),
                '&' => token = Some(Token::new(TokenType::BitAnd, self.location)),
                '|' => token = Some(Token::new(TokenType::BitOr, self.location)),
                '^' => token = Some(Token::new(TokenType::BitXor, self.location)),
                '<' => token = Some(Token::new(TokenType::Less, self.location)),
                '>' => token = Some(Token::new(TokenType::Greater, self.location)),
                _ => token = None,
            }
        }
        return token;
    }

    fn lex_number(&mut self) -> Token {
        let mut num = String::new();

        while let Some(&ch) = self.chars.peek() {
            if ch.is_digit(10) {
                num.push(ch);
                self.advance();
            } else if ch == '_' {
                self.advance();
                continue;
            } else {
                break;
            }
        }
        Token::number_literal(num, self.location)
    }

    fn lex_ident(&mut self) -> Token {
        let mut ident = String::new();

        while let Some(&ch) = self.chars.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                ident.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        match ident.as_str() {
            "if" => Token::new(TokenType::If, self.location),
            "is" => Token::new(TokenType::Is, self.location),
            "else" => Token::new(TokenType::Else, self.location),
            "loop" => Token::new(TokenType::Loop, self.location),
            "break" => Token::new(TokenType::Break, self.location),
            "fn" => Token::new(TokenType::Fn, self.location),
            "struct" => Token::new(TokenType::Struct, self.location),
            "sum" => Token::new(TokenType::Sum, self.location),
            "print" => Token::new(TokenType::Print, self.location),
            "use" => Token::new(TokenType::Use, self.location),
            "ret" => Token::new(TokenType::Ret, self.location),
            "true" => Token::new(TokenType::True, self.location),
            "false" => Token::new(TokenType::False, self.location),
            _ => Token::new_ident(ident, self.location),
        }
    }

    fn skip_comment(&mut self) {
        while let Some(&ch) = self.chars.peek() {
            self.advance();
            if ch == '\r' || ch == '\n' {
                break;
            }
        }
    }

    // still need to add escaping of special characters.
    fn lex_string_literal(&mut self) -> Option<Token> {
        let mut literal = String::new();
        self.advance();
        let mut is_valid = false;
        while let Some(&ch) = self.chars.peek() {
            if ch != '\"' {
                literal.push(ch);
                self.advance();
            } else {
                is_valid = true;
                self.advance();
                break;
            }
        }
        if is_valid { Some(Token::string_literal(literal, self.location)) } else { None }
    }
}
