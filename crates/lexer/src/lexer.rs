use crate::errors::{SingleTokenError, StringTerminationError};
use crate::token::{Token, TokenKind};
use miette::{Error, LabeledSpan, SourceSpan};

pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    c_onwards: &'de str,
    byte: usize,
    peeked: Option<Result<Token<'de>, miette::Error>>,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            rest: input,
            c_onwards: input,
            byte: 0,
            peeked: None,
        }
    }

    fn just(
        &self,
        kind: TokenKind,
        offset: usize,
        origin: &'de str,
    ) -> Option<Result<Token<'de>, miette::Error>> {
        Some(Ok(Token {
            kind,
            offset,
            origin,
        }))
    }

    fn match_punctuator(
        &self,
        c: char,
        offset: usize,
        origin: &'de str,
    ) -> Option<Result<Token<'de>, miette::Error>> {
        match c {
            '(' => self.just(TokenKind::LParen, offset, origin),
            ')' => self.just(TokenKind::RParen, offset, origin),
            '{' => self.just(TokenKind::LBrace, offset, origin),
            '}' => self.just(TokenKind::RBrace, offset, origin),
            '[' => self.just(TokenKind::LBracket, offset, origin),
            ']' => self.just(TokenKind::RBracket, offset, origin),
            ',' => self.just(TokenKind::Comma, offset, origin),
            ':' => self.just(TokenKind::Colon, offset, origin),
            ';' => self.just(TokenKind::SemiColon, offset, origin),
            '.' => self.just(TokenKind::Dot, offset, origin),
            _ => None,
        }
    }

    fn match_arithmetic(
        &mut self,
        c: char,
        offset: usize,
        origin: &'de str,
    ) -> Option<Result<Token<'de>, miette::Error>> {
        match c {
            '+' => self.just(TokenKind::Plus, offset, origin),
            '-' => self.just(TokenKind::Minus, offset, origin),
            '*' => self.just(TokenKind::Star, offset, origin),
            '%' => self.just(TokenKind::Percent, offset, origin),
            '/' => {
                // Check for single-line or multi-line comments, or return division token
                let full_origin = &self.whole[offset..offset + 2];
                if self.rest.starts_with('/') {
                    self.skip_singleline_comment();
                    return self.just(TokenKind::LineComment, offset, full_origin);
                } else if self.rest.starts_with('*') {
                    self.skip_multiline_comment();
                    return self.just(TokenKind::BlockComment, offset, full_origin);
                } else {
                    return self.just(TokenKind::Slash, offset, origin); // Otherwise, it's division
                }
            }
            _ => None,
        }
    }

    fn skip_singleline_comment(&mut self) {
        if let Some(line_end) = self.rest.find('\n') {
            self.byte += line_end + 1;
            self.rest = &self.rest[line_end + 1..];
        } else {
            // Consume the rest if no newline is found
            self.byte += self.rest.len();
            self.rest = "";
        }
    }

    fn skip_multiline_comment(&mut self) {
        if let Some(end) = self.rest.find("*/") {
            self.byte += end + 2; // Account for "*/"
            self.rest = &self.rest[end + 2..];
        } else {
            // Handle error: unterminated multi-line comment
            self.peeked = Some(Err(StringTerminationError {
                src: self.whole.to_string(),
                err_span: SourceSpan::from(self.byte..self.byte + self.rest.len()),
            }
            .into()));
            self.rest = "";
        }
    }

    fn match_comparison_token(
        &mut self,
        c: char,
        single_kind: TokenKind,
        double_kind: TokenKind,
        offset: usize,
        origin: &'de str,
    ) -> Option<Result<Token<'de>, miette::Error>> {
        self.rest = self.rest.trim_start();
        let trimmed = self.c_onwards.len() - self.rest.len() - 1;
        self.byte += trimmed;
        if self.rest.starts_with('=') {
            let span: &str = &self.c_onwards[..c.len_utf8() + trimmed + 1];
            self.rest = &self.rest[1..];
            self.byte += 1;
            return self.just(double_kind, offset, span);
        } else {
            return self.just(single_kind, offset, origin);
        }
    }

    fn match_comparison(
        &mut self,
        c: char,
        offset: usize,
        origin: &'de str,
    ) -> Option<Result<Token<'de>, miette::Error>> {
        match c {
            '=' => self.match_comparison_token(
                c,
                TokenKind::Equal,
                TokenKind::EqualEqual,
                offset,
                origin,
            ),
            '!' => self.match_comparison_token(
                c,
                TokenKind::Bang,
                TokenKind::BangEqual,
                offset,
                origin,
            ),
            '<' => self.match_comparison_token(
                c,
                TokenKind::Less,
                TokenKind::LessEqual,
                offset,
                origin,
            ),
            '>' => self.match_comparison_token(
                c,
                TokenKind::Greater,
                TokenKind::GreaterEqual,
                offset,
                origin,
            ),
            _ => None,
        }
    }

    fn match_identifier_keyword(
        &mut self,
        c: char,
        offset: usize,
    ) -> Option<Result<Token<'de>, miette::Error>> {
        match c {
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut literal = c.to_string();
                let start_offset = self.byte - c.len_utf8();

                while let Some(next_char) = self.rest.chars().next() {
                    if next_char.is_alphanumeric() || next_char == '_' {
                        literal.push(next_char);
                        self.rest = &self.rest[next_char.len_utf8()..];
                        self.byte += next_char.len_utf8();
                    } else {
                        break;
                    }
                }

                // Determine TokenKind based on identifier
                let kind = match literal.as_str() {
                    "if" => TokenKind::If,
                    "and" => TokenKind::And,
                    "else" => TokenKind::Else,
                    "false" => TokenKind::False,
                    "for" => TokenKind::For,
                    "fun" => TokenKind::Fun,
                    "nil" => TokenKind::Nil,
                    "not" => TokenKind::Not,
                    "in" => TokenKind::In,
                    "or" => TokenKind::Or,
                    "return" => TokenKind::Return,
                    "true" => TokenKind::True,
                    "while" => TokenKind::While,
                    "split" => TokenKind::Split,
                    "join" => TokenKind::Join,
                    "map" => TokenKind::Map,
                    "filter" => TokenKind::Filter,
                    "reduce" => TokenKind::Reduce,
                    "replace" => TokenKind::Replace,
                    "extract" => TokenKind::Extract,
                    "length" => TokenKind::Length,
                    "typeOf" => TokenKind::TypeOf,
                    "range" => TokenKind::Range,
                    "concat" => TokenKind::Concatenate,
                    _ => TokenKind::Ident,
                };

                // Correct offset handling for `full_origin`
                let end_offset = self.byte;
                let full_origin = &self.whole[start_offset..end_offset];
                return self.just(kind, offset, full_origin);
            }
            _ => None,
        }
    }

    fn match_string(
        &mut self,
        c: char,
        offset: usize,
    ) -> Option<Result<Token<'de>, miette::Error>> {
        if c == '"' {
            let mut literal = String::from("\""); // Start with the opening quote
            let mut escaped = false;

            for (i, ch) in self.rest.chars().enumerate() {
                literal.push(ch);

                if escaped {
                    escaped = false;
                } else if ch == '\\' {
                    escaped = true; // Escape the next character
                } else if ch == '\n' {
                    // Found an unescaped newline, which is not allowed
                    let err = StringTerminationError {
                        src: self.whole.to_string(),
                        err_span: SourceSpan::from(self.byte + i..self.byte + i + 1),
                    };
                    self.byte += i + 1;
                    self.rest = &self.rest[i + 1..];
                    return Some(Err(err.into()));
                } else if ch == '"' {
                    // Found the closing quote
                    self.byte += i + 1;
                    self.rest = &self.rest[i + 1..];
                    return Some(Ok(Token {
                        origin: &self.c_onwards[..literal.len()],
                        offset,
                        kind: TokenKind::String,
                    }));
                }
            }

            // If we exit the loop without finding a closing quote, it’s an unterminated string error
            let err = StringTerminationError {
                src: self.whole.to_string(),
                err_span: SourceSpan::from(self.byte..self.whole.len()), // adjusted span
            };
            self.byte += self.rest.len();
            self.rest = "";
            Some(Err(err.into()))
        } else {
            None
        }
    }

    #[allow(clippy::while_let_on_iterator)]
    fn match_number(
        &mut self,
        c: char,
        offset: usize,
    ) -> Option<Result<Token<'de>, miette::Error>> {
        if c.is_ascii_digit() || c == '.' {
            let start_offset = self.byte - c.len_utf8();
            let mut literal = c.to_string();
            let mut chars_iter = self.rest.char_indices();
            let mut chars_consumed = 0;
            let mut has_dot = c == '.';
            let mut digits_before_dot = c.is_ascii_digit();
            let mut digits_after_dot = false;
            let mut has_invalid_chars = false;

            while let Some((_, ch)) = chars_iter.next() {
                if ch.is_ascii_digit() {
                    literal.push(ch);
                    chars_consumed += ch.len_utf8();
                    if has_dot {
                        digits_after_dot = true;
                    } else {
                        digits_before_dot = true;
                    }
                } else if ch == '.' {
                    if has_dot {
                        // Second dot encountered, invalid number
                        literal.push(ch);
                        chars_consumed += ch.len_utf8();
                        has_invalid_chars = true;
                    } else {
                        has_dot = true;
                        literal.push(ch);
                        chars_consumed += ch.len_utf8();
                    }
                } else if ch.is_alphabetic() || ch == '_' {
                    // Include letters in the literal to produce an error
                    literal.push(ch);
                    chars_consumed += ch.len_utf8();
                    has_invalid_chars = true;
                } else {
                    break;
                }
            }

            // Advance the iterator
            self.byte += chars_consumed;
            self.rest = &self.rest[chars_consumed..];

            // Validate the number
            if has_invalid_chars || (has_dot && (!digits_before_dot || !digits_after_dot)) {
                return Some(Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(start_offset..self.byte, "this numeric literal"),
                    ],
                    "Invalid number literal",
                }
                .with_source_code(self.whole.to_string())));
            }

            // Validate that the literal is a valid number
            match literal.parse::<f64>() {
                Ok(n) => Some(Ok(Token {
                    origin: &self.whole[start_offset..self.byte],
                    offset,
                    kind: TokenKind::Number(n),
                })),
                Err(_) => Some(Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(start_offset..self.byte, "this numeric literal"),
                    ],
                    "Invalid number literal",
                }
                .with_source_code(self.whole.to_string()))),
            }
        } else {
            None
        }
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.peeked.take() {
            return Some(next);
        }

        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let c_at = self.byte;
            let c_str = &self.rest[..c.len_utf8()];
            self.c_onwards = self.rest;
            self.rest = chars.as_str();
            self.byte += c.len_utf8();

            // Match punctuators and delimiters
            if let Some(kind) = self.match_punctuator(c, c_at, c_str) {
                return Some(kind);
            }

            // Match arithmetic operators, handling comments or division
            if let Some(kind) = self.match_arithmetic(c, c_at, c_str) {
                return Some(kind);
            }

            if let Some(kind) = self.match_comparison(c, c_at, c_str) {
                return Some(kind);
            }

            if let Some(kind) = self.match_identifier_keyword(c, c_at) {
                return Some(kind);
            }

            if let Some(kind) = self.match_string(c, c_at) {
                return Some(kind);
            }

            if let Some(kind) = self.match_number(c, c_at) {
                return Some(kind);
            }

            // Skip whitespace
            if c.is_whitespace() {
                continue;
            }

            // Handle unrecognized tokens
            return Some(Err(SingleTokenError {
                src: self.whole.to_string(),
                token: c,
                err_span: SourceSpan::from(self.byte - c.len_utf8()..self.byte),
            }
            .into()));
        }
    }
}
