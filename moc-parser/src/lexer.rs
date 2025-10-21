use std::iter::Peekable;
use std::str::Chars;

use moc_common::error::{LexerError, LexerResult};
use moc_common::token::{Token, TokenType};
use moc_common::{CodeLocation, CodeSpan};

#[derive(Clone)]
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    last_token_end: CodeLocation,
    location: CodeLocation,
}

macro_rules! token {
    ($lexer:expr, $token_type:ident) => {
        Token::new(
            TokenType::$token_type,
            CodeSpan::from(($lexer.last_token_end, $lexer.location)),
        )
    };
}

macro_rules! ok_token {
    ($lexer:expr, $token_type:ident) => {
        Ok(token!($lexer, $token_type))
    };
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexerResult;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next_token()) // discards error... :/ when peeking char, the lexer error is just discarded
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
            location: CodeLocation::default(),
            last_token_end: CodeLocation::default(),
        }
    }

    pub fn tokens(mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::with_capacity(256);
        loop {
            let token = self.next_token()?;
            if token.r#type == TokenType::EndOfFile {
                break;
            } else {
                tokens.push(token);
            }
        }
        Ok(tokens)
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

    fn lex_crlf(&mut self) -> Option<Token> {
        self.advance();
        if self.peek_char() == Some('\n') {
            self.advance();
            self.count_line_break();
            return Some(token!(self, LineBreak));
        }
        None
    }

    fn lex_lf(&mut self) -> Token {
        self.advance();
        self.count_line_break();
        token!(self, LineBreak)
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.last_token_end = self.location;
        let lexer_result = loop {
            if let Some(ch) = self.peek_char() {
                match ch {
                    '.' => {
                        self.advance();
                        break ok_token!(self, Dot);
                    }
                    '!' => {
                        self.advance();
                        if self.peek_char() == Some('=') {
                            self.advance();
                            break ok_token!(self, NotEqualTo);
                        }
                        break ok_token!(self, Excl);
                    }
                    '@' => {
                        self.advance();
                        break ok_token!(self, At);
                    }
                    ',' => {
                        self.advance();
                        break ok_token!(self, Comma);
                    }
                    '/' => {
                        self.advance();
                        if self.peek_char() == Some('/') {
                            // two slashes means a comment
                            self.skip_comment();
                            continue;
                        }
                        break self.lex_operator(ch); // if we have a single slash, it's a divide operator
                    }
                    ' ' | '\t' => {
                        self.advance();
                        self.last_token_end = self.location; // for every 'skipped' chararacter that doesnt result a token, we need to move the "last_token_end" cursor to the lexer's position.
                        continue;
                    } // Skip non-relevant whitespace
                    '\r' => {
                        if let Some(token) = self.lex_crlf() {
                            break Ok(token);
                        } else {
                            // maybe emit warning here if only single \r found
                            self.last_token_end = self.location;
                            continue;
                        };
                    }
                    '\n' => {
                        break Ok(self.lex_lf());
                    }
                    '{' => {
                        self.advance();
                        break ok_token!(self, OpenBrace);
                    }
                    '}' => {
                        self.advance();
                        break ok_token!(self, CloseBrace);
                    }
                    '(' => {
                        self.advance();
                        break ok_token!(self, OpenParen);
                    }
                    ')' => {
                        self.advance();
                        break ok_token!(self, CloseParen);
                    }
                    ':' => {
                        self.advance();
                        if self.peek_char() == Some('=') {
                            self.advance();
                            break ok_token!(self, DeclareAssign);
                        }
                        break ok_token!(self, Colon);
                    }
                    ';' => {
                        self.advance();
                        break ok_token!(self, Semicolon);
                    }
                    '=' => {
                        self.advance();
                        if self.peek_char() == Some('=') {
                            self.advance();
                            break ok_token!(self, EqualTo);
                        }
                        break ok_token!(self, Assign);
                    }
                    '+' | '-' | '*' | '%' | '&' | '|' | '~' | '^' | '<' | '>' => {
                        break self.lex_operator(ch);
                    }
                    '0'..='9' => break Ok(self.lex_number()),
                    'a'..='z' | 'A'..='Z' | '_' => break Ok(self.lex_ident()),
                    '\"' => break self.lex_string_literal(),
                    _ => {
                        break Err(LexerError::InvalidCharacter(ch));
                    } // TODO: return proper LexerError
                }
            } else {
                return ok_token!(self, EndOfFile)
            }
        };

        return lexer_result;
    }

    fn lex_operator(&mut self, ch: char) -> LexerResult {
        self.advance();
        if ch == '<' {
            if self.peek_char() == Some('<') {
                // bit-shift left
                self.advance();
                if self.peek_char() == Some('=') {
                    self.advance();
                    return ok_token!(self, BitShiftLeftAssign);
                }
                return ok_token!(self, BitShiftLeft);
            }
        }
        if ch == '>' {
            if self.peek_char() == Some('>') {
                // bit-shift right
                self.advance();
                if self.peek_char() == Some('=') {
                    self.advance();
                    return ok_token!(self, BitShiftRightAssign);
                }
                return ok_token!(self, BitShiftRight);
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
            return Ok(Token::new(
                token_type,
                (self.last_token_end, self.location).into(),
            ));
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
            return Ok(Token::new(
                token_type,
                (self.last_token_end, self.location).into(),
            ));
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
        Token::number_literal(num, (self.last_token_end, self.location).into())
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
            "break" => token!(self, Break),
            "defer" => token!(self, Defer),
            "else" => token!(self, Else),
            "false" => token!(self, False),
            "fn" => token!(self, Fn),
            "for" => token!(self, For),
            "if" => token!(self, If),
            "in" => token!(self, In),
            "is" => token!(self, Is),
            "loop" => token!(self, Loop),
            "ret" => token!(self, Ret),
            "struct" => token!(self, Struct),
            "sum" => token!(self, Sum),
            "true" => token!(self, True),
            "use" => token!(self, Use),
            _ => Token::new_ident(ident, (self.last_token_end, self.location).into()),
        }
    }

    fn skip_comment(&mut self) {
        while let Some(ch) = self.peek_char() {
            self.advance();
            if ch == '\r' || ch == '\n' {
                break;
            }
        }
        self.last_token_end = self.location;
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
            Ok(Token::string_literal(
                value,
                (self.last_token_end, self.location).into(),
            ))
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
