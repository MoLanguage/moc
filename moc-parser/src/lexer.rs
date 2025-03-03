use std::iter::Peekable;
use std::str::Chars;

use crate::{Token, TokenType};

#[derive(Clone)]
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
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
        }
    }

    fn advance(&mut self) -> Option<char> {
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
                    Some(Token::new(TokenType::OpenBrace))
                }
                '}' => {
                    self.advance()?;
                    Some(Token::new(TokenType::CloseBrace))
                }
                '(' => {
                    self.advance()?;
                    Some(Token::new(TokenType::OpenParen))
                }
                ')' => {
                    self.advance()?;
                    Some(Token::new(TokenType::CloseParen))
                }
                ':' => {
                    self.advance()?;
                    if self.chars.peek() == Some(&'=') {
                        self.chars.next();
                        return Some(Token::new(TokenType::Declare));
                    }
                    Some(Token::new(TokenType::Colon))
                }
                ';' | '\n' => {
                    self.advance()?;
                    Some(Token::new(TokenType::EndOfStatement))
                }
                '=' => {
                    self.advance()?;
                    if self.chars.peek() == Some(&'=') {
                        self.chars.next();
                        return Some(Token::new(TokenType::EqualTo));
                    }
                    Some(Token::new(TokenType::Assign))
                }
                '+' | '-' | '*' | '%' | '&' | '|' | '^' | '<' | '>' => {
                    self.advance()?;
                    self.lex_operator(ch)
                }
                '0'..='9' => return Some(self.lex_number()),
                'a'..='z' | 'A'..='Z' | '_' => return Some(self.lex_ident()),
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
                '+' => token = Some(Token::new(TokenType::AddAssign)),
                '-' => token = Some(Token::new(TokenType::SubAssign)),
                '*' => token = Some(Token::new(TokenType::MultAssign)),
                '%' => token = Some(Token::new(TokenType::ModAssign)),
                '&' => token = Some(Token::new(TokenType::BitAndAssign)),
                '|' => token = Some(Token::new(TokenType::BitOrAssign)),
                '^' => token = Some(Token::new(TokenType::BitXorAssign)),
                _ => token = None,
            }
        } else {
            match ch {
                '+' => token = Some(Token::new(TokenType::Plus)),
                '-' => token = Some(Token::new(TokenType::Minus)),
                '*' => token = Some(Token::new(TokenType::Star)),
                '%' => token = Some(Token::new(TokenType::Mod)),
                '&' => token = Some(Token::new(TokenType::BitAnd)),
                '|' => token = Some(Token::new(TokenType::BitOr)),
                '^' => token = Some(Token::new(TokenType::BitXor)),
                '<' => token = Some(Token::new(TokenType::Less)),
                '>' => token = Some(Token::new(TokenType::Greater)),
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
                self.chars.next();
            } else if ch == '_' {
                self.chars.next();
                continue;
            } else {
                break;
            }
        }
        Token::number_literal(num)
    }

    fn lex_ident(&mut self) -> Token {
        let mut ident = String::new();

        while let Some(&ch) = self.chars.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                ident.push(ch);
                self.chars.next();
            } else {
                break;
            }
        }
        match ident.as_str() {
            "if" => Token::new(TokenType::If),
            "is" => Token::new(TokenType::Is),
            "else" => Token::new(TokenType::Else),
            "loop" => Token::new(TokenType::Loop),
            "break" => Token::new(TokenType::Break),
            "fn" => Token::new(TokenType::Fn),
            "struct" => Token::new(TokenType::Struct),
            "sum" => Token::new(TokenType::Sum),
            "print" => Token::new(TokenType::Print),
            "use" => Token::new(TokenType::Use),
            "ret" => Token::new(TokenType::Ret),
            "true" => Token::new(TokenType::True),
            "false" => Token::new(TokenType::False),
            _ => Token::new_ident(ident),
        }
    }

    fn skip_comment(&mut self) {
        while let Some(&ch) = self.chars.peek() {
            if ch != '\n' {
                self.chars.next();
            } else {
                break;
            }
        }
    }
}
