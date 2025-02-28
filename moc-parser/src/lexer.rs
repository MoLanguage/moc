use std::iter::Peekable;
use std::str::Chars;

use crate::Token;

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
                    Some(Token::OpenBrace)
                }
                '}' => {
                    self.advance()?;
                    Some(Token::CloseBrace)
                }
                '(' => {
                    self.advance()?;
                    Some(Token::OpenParen)
                }
                ')' => {
                    self.advance()?;
                    Some(Token::CloseParen)
                }
                ':' => {
                    self.advance()?;
                    if self.chars.peek() == Some(&'=') {
                        self.chars.next();
                        return Some(Token::Declare);
                    }
                    Some(Token::Colon)
                }
                ';' | '\n' => {
                    self.advance()?;
                    Some(Token::EndOfStatement)
                }
                '=' => {
                    self.advance()?;
                    if self.chars.peek() == Some(&'=') {
                        self.chars.next();
                        return Some(Token::EqualTo);
                    }
                    Some(Token::Assign)
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
                '+' => token = Some(Token::AddAssign),
                '-' => token = Some(Token::SubAssign),
                '*' => token = Some(Token::MultAssign),
                '%' => token = Some(Token::ModAssign),
                '&' => token = Some(Token::BitAndAssign),
                '|' => token = Some(Token::BitOrAssign),
                '^' => token = Some(Token::BitXorAssign),
                _ => token = None,
            }
        } else {
            match ch {
                '+' => token = Some(Token::Plus),
                '-' => token = Some(Token::Minus),
                '*' => token = Some(Token::Star),
                '%' => token = Some(Token::Mod),
                '&' => token = Some(Token::BitAnd),
                '|' => token = Some(Token::BitOr),
                '^' => token = Some(Token::BitXor),
                '<' => token = Some(Token::Less),
                '>' => token = Some(Token::Greater),
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
        Token::NumberLiteral(num)
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
            "if" => Token::If,
            "is" => Token::Is,
            "else" => Token::Else,
            "loop" => Token::Loop,
            "break" => Token::Break,
            "fn" => Token::Fn,
            "struct" => Token::Struct,
            "sum" => Token::Sum,
            "print" => Token::Print,
            "use" => Token::Use,
            "ret" => Token::Ret,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Ident(ident),
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
