use std::iter::Peekable;
use std::str::Chars;

use crate::{CodeLocation, Token, TokenType};

#[derive(Clone)]
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    location: CodeLocation,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token().ok()
    }
}

#[derive(Debug)]
pub enum LexerError {
    UnendingStringLiteral(CodeLocation),
    InvalidCharacter(char),
    UnknownEscapeCharacter,
    UnknownToken,
}

type LexerResult = Result<Token, LexerError>;

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
            location: CodeLocation::default(),
        }
    }

    fn line_break(&mut self) {
        self.location.column = 1;
        self.location.line += 1;
    }

    fn advance(&mut self) -> Option<char> {
        self.location.column += 1;
        self.chars.next()
    }

    fn lex_crlf(&mut self) -> Option<Token> {
        self.advance();
        if self.chars.peek() == Some(&'\n') {
            self.advance();
            self.line_break();
            return Some(Token::new(TokenType::LineBreak, self.location));
        }
        None
    }

    fn lex_lf(&mut self) -> Token {
        self.advance();
        self.line_break();
        Token::new(TokenType::LineBreak, self.location)
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        while let Some(ch) = self.chars.peek() {
            let ch = *ch;
            let token = match ch {
                '.' => {
                    self.advance();
                    Ok(Token::new(TokenType::Dot, self.location))
                }
                '!' => {
                    self.advance();
                    Ok(Token::new(TokenType::Excl, self.location))
                }
                '@' => {
                    self.advance();
                    Ok(Token::new(TokenType::At, self.location))
                }
                ',' => {
                    self.advance();
                    Ok(Token::new(TokenType::Comma, self.location))
                }
                '/' => {
                    self.advance();
                    if self.chars.peek() == Some(&'/') {
                        // two slashes means a comment
                        self.skip_comment();
                        continue;
                    }
                    self.lex_operator(ch) // if we have a single slash, it's a divide operator
                }
                ' ' | '\t' => {
                    self.advance();
                    continue;
                } // Skip whitespace
                '\r' => {
                    if let Some(token) = self.lex_crlf() { 
                        return Ok(token);
                    } else {
                        // maybe emit warning here if only single \r found
                        continue;
                    };
                } 
                '\n' => Ok(self.lex_lf()),
                '{' => {
                    self.advance();
                    Ok(Token::new(TokenType::OpenBrace, self.location))
                }
                '}' => {
                    self.advance();
                    Ok(Token::new(TokenType::CloseBrace, self.location))
                }
                '(' => {
                    self.advance();
                    Ok(Token::new(TokenType::OpenParen, self.location))
                }
                ')' => {
                    self.advance();
                    Ok(Token::new(TokenType::CloseParen, self.location))
                }
                ':' => {
                    self.advance();
                    if self.chars.peek() == Some(&'=') {
                        self.advance();
                        return Ok(Token::new(TokenType::Declare, self.location));
                    }
                    Ok(Token::new(TokenType::Colon, self.location))
                }
                ';' => {
                    self.advance();
                    Ok(Token::new(TokenType::Semicolon, self.location))
                }
                '=' => {
                    self.advance();
                    if self.chars.peek() == Some(&'=') {
                        self.advance();
                        return Ok(Token::new(TokenType::EqualTo, self.location));
                    }
                    Ok(Token::new(TokenType::Assign, self.location))
                }
                '+' | '-' | '*' | '%' | '&' | '|' | '^' | '<' | '>' => self.lex_operator(ch),
                '0'..='9' => return Ok(self.lex_number()),
                'a'..='z' | 'A'..='Z' | '_' => return Ok(self.lex_ident()),
                '\"' => return self.lex_string_literal(),
                _ => Err(LexerError::InvalidCharacter(ch)), // TODO: return proper LexerError
            };
            return token;
        }
        Ok(Token::new(TokenType::EndOfFile, self.location))
    }

    fn lex_operator(&mut self, ch: char) -> LexerResult {
        self.advance();
        if self.chars.peek() == Some(&'=') {
            let token_type = match ch {
                '+' => TokenType::AddAssign,
                '-' => TokenType::SubAssign,
                '*' => TokenType::MultAssign,
                '/' => TokenType::DivAssign,
                '%' => TokenType::ModAssign,
                '&' => TokenType::BitAndAssign,
                '|' => TokenType::BitOrAssign,
                '^' => TokenType::BitXorAssign,
                '<' => TokenType::LessOrEqual,
                '>' => TokenType::GreaterOrEqual,
                _ => unreachable!(),
            };

            // is a 2 character operator
            self.advance();
            return Ok(Token::new(token_type, self.location));
        } else {
            let token_type = match ch {
                '+' => TokenType::Plus,
                '-' => TokenType::Minus,
                '*' => TokenType::Star,
                '/' => TokenType::Slash,
                '%' => TokenType::Mod,
                '&' => TokenType::BitAnd,
                '|' => TokenType::BitOr,
                '^' => TokenType::BitXor,
                '<' => TokenType::Less,
                '>' => TokenType::Greater,
                _ => unreachable!(),
            };
            return Ok(Token::new(token_type, self.location));
        }
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
            "for" => Token::new(TokenType::For, self.location),
            "in" => Token::new(TokenType::In, self.location),
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

    // still need to add escaping of special characters. could do that later tho.
    fn lex_string_literal(&mut self) -> Result<Token, LexerError> {
        self.advance();
        let mut literal = String::new();
        let mut ends_by_quote = false; // valid if string literal ends with another quote symbol "
        while let Some(&ch) = self.chars.peek() {
            self.advance();
            match ch {
                '\\' => {
                    self.handle_escape_character(&mut literal)?;
                }
                '\"' => {
                    ends_by_quote = true;
                    break;
                }
                _ => {
                    literal.push(ch);
                }
            }
        }
        if ends_by_quote {
            Ok(Token::string_literal(literal, self.location))
        } else {
            Err(LexerError::UnendingStringLiteral(self.location))
        }
    }

    fn handle_escape_character(&mut self, literal: &mut String) -> Result<(), LexerError> {
        if let Some(ch) = self.chars.peek() {
            let replacement = match ch {
                '\\' => Some('\\'),
                '\"' => Some('\"'),
                'r' => Some('\r'),
                'n' => Some('\n'),
                '0' => Some('\0'),
                't' => Some('\t'),
                '\'' => Some('\''),
                _ => None, // replace with proper error
            };
            if let Some(replacement) = replacement {
                self.advance();
                literal.push(replacement);
                return Ok(());
            }
        }
        Err(LexerError::UnknownEscapeCharacter)
    }
}
