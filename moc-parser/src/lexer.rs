use std::iter::Peekable;
use std::str::Chars;

use moc_common::error::{LexerError, LexerResult};
use moc_common::token::{Token, TokenType};
use moc_common::CodeLocation;

#[derive(Clone)]
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    location: CodeLocation,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token().ok() // discards error... :/ when peeking char, the lexer error is just discarded
    }
}


impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
            location: CodeLocation::default(),
        }
    }

    fn count_line_break(&mut self) {
        self.location.column = 1;
        self.location.line += 1;
    }

    fn advance(&mut self) -> Option<char> {
        self.location.column += 1;
        self.chars.next()
    }
    
    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().cloned()
    }
    
    fn create_token(&self, r#type: TokenType) -> Token {
        Token::new(r#type, self.location)
    }

    fn lex_crlf(&mut self) -> Option<Token> {
        self.advance();
        if self.peek_char() == Some('\n') {
            self.advance();
            self.count_line_break();
            return Some(Token::new(TokenType::LineBreak, self.location));
        }
        None
    }

    fn lex_lf(&mut self) -> Token {
        self.advance();
        self.count_line_break();
        Token::new(TokenType::LineBreak, self.location)
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        while let Some(ch) = self.peek_char() {
            let token = match ch {
                '.' => {
                    self.advance();
                    Ok(Token::new(TokenType::Dot, self.location))
                }
                '!' => {
                    self.advance();
                    if self.peek_char() == Some('=') {
                        self.advance();
                        return Ok(Token::new(TokenType::NotEqualTo, self.location));
                    }
                    Ok(Token::new(TokenType::Excl, self.location))
                }
                '@' => {
                    self.advance();
                    Ok(self.create_token(TokenType::At))
                }
                ',' => {
                    self.advance();
                    Ok(self.create_token(TokenType::Comma))
                }
                '/' => {
                    self.advance();
                    if self.peek_char() == Some('/') {
                        // two slashes means a comment
                        self.skip_comment();
                        continue;
                    }
                    self.lex_operator(ch) // if we have a single slash, it's a divide operator
                }
                ' ' | '\t' => {
                    self.advance();
                    continue;
                } // Skip non-relevant whitespace
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
                    if self.peek_char() == Some('=') {
                        self.advance();
                        return Ok(Token::new(TokenType::DeclareAssign, self.location));
                    }
                    Ok(Token::new(TokenType::Colon, self.location))
                }
                ';' => {
                    self.advance();
                    Ok(Token::new(TokenType::Semicolon, self.location))
                }
                '=' => {
                    self.advance();
                    if self.peek_char() == Some('=') {
                        self.advance();
                        return Ok(Token::new(TokenType::EqualTo, self.location));
                    }
                    Ok(Token::new(TokenType::Assign, self.location))
                }
                '+' | '-' | '*' | '%' | '&' | '|' | '~' | '^' | '<' | '>' => self.lex_operator(ch),
                '0'..='9' => return Ok(self.lex_number()),
                'a'..='z' | 'A'..='Z' | '_' => return Ok(self.lex_ident()),
                '\"' => return self.lex_string_literal(),
                _ => Err(LexerError::InvalidCharacter(ch)), // TODO: return proper LexerError
            };
            return token;
        };
        Ok(Token::new(TokenType::EndOfFile, self.location))
    }
    
    fn lex_operator(&mut self, ch: char) -> LexerResult {
        self.advance();
        if ch == '<' {
            if self.peek_char() == Some('<') {
                // bit-shift left
                self.advance();
                if self.peek_char() == Some('=') {
                    self.advance();
                    return Ok(self.create_token(TokenType::BitShiftLeftAssign));
                } 
                return Ok(self.create_token(TokenType::BitShiftLeft));
            }
        }
        if ch == '>' {
            if self.peek_char() == Some('>') {
                // bit-shift right
                self.advance();
                if self.peek_char() == Some('=') {
                    self.advance();
                    return Ok(self.create_token(TokenType::BitShiftRightAssign));
                } 
                return Ok(self.create_token(TokenType::BitShiftRight));
            }
        }
        if self.peek_char() == Some('=') {
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
                '~' => TokenType::BitNotAssign,
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
                '%' => TokenType::Percent,
                '&' => TokenType::Ampersand,
                '|' => TokenType::Pipe,
                '^' => TokenType::Caret,
                '<' => TokenType::Less,
                '>' => TokenType::Greater,
                '~' => TokenType::Tilde,
                _ => unreachable!(),
            };
            return Ok(Token::new(token_type, self.location));
        }
    }

    fn lex_number(&mut self) -> Token {
        let mut num = String::new();

        while let Some(ch) = self.peek_char() {
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

        while let Some(ch) = self.peek_char() {
            if ch.is_alphanumeric() || ch == '_' {
                ident.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        match ident.as_str() {
            "break" => Token::new(TokenType::Break, self.location),
            "defer" => Token::new(TokenType::Defer, self.location),
            "else" => Token::new(TokenType::Else, self.location),
            "false" => Token::new(TokenType::False, self.location),
            "fn" => Token::new(TokenType::Fn, self.location),
            "for" => Token::new(TokenType::For, self.location),
            "if" => Token::new(TokenType::If, self.location),
            "in" => Token::new(TokenType::In, self.location),
            "is" => Token::new(TokenType::Is, self.location),
            "loop" => Token::new(TokenType::Loop, self.location),
            "ret" => Token::new(TokenType::Ret, self.location),
            "struct" => Token::new(TokenType::Struct, self.location),
            "sum" => Token::new(TokenType::Sum, self.location),
            "true" => Token::new(TokenType::True, self.location),
            "use" => Token::new(TokenType::Use, self.location),
            _ => Token::new_ident(ident, self.location),
        }
    }

    fn skip_comment(&mut self) {
        while let Some(ch) = self.peek_char() {
            self.advance();
            if ch == '\r' || ch == '\n' {
                break;
            }
        }
    }

    // still need to add escaping of special characters. could do that later tho.
    fn lex_string_literal(&mut self) -> Result<Token, LexerError> {
        self.advance();
        let mut value = String::new();
        let mut ends_by_quote = false; // valid if string literal ends with another quote symbol "
        while let Some(ch) = self.peek_char() {
            self.advance();
            match ch {
                '\\' => {
                    self.handle_escape_character(&mut value)?;
                }
                '\"' => {
                    ends_by_quote = true;
                    break;
                }
                _ => {
                    value.push(ch);
                }
            }
        }
        if ends_by_quote {
            Ok(Token::string_literal(value, self.location))
        } else {
            Err(LexerError::UnterminatedStringLiteral(self.location))
        }
    }

    fn handle_escape_character(&mut self, literal: &mut String) -> Result<(), LexerError> {
        if let Some(ch) = self.peek_char() {
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
