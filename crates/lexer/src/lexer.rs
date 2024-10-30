use crate::errors::StringTerminationError;
use crate::token::{Token, TokenKind};
use miette::{Error, LabeledSpan, Report, SourceSpan};
use text::span::TextSpan;

pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
    peeked: Option<Result<Token<'de>, miette::Error>>,
    emitted_eof: bool,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            rest: input,
            byte: 0,
            peeked: None,
            emitted_eof: false,
        }
    }

    #[allow(clippy::while_let_on_iterator)]
    pub fn collect_tokens(&mut self) -> (Vec<Token<'de>>, Vec<Report>) {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while let Some(item) = self.next() {
            match item {
                Ok(token) => tokens.push(token),
                Err(error) => errors.push(error),
            }
        }

        (tokens, errors)
    }

    fn just(
        &self,
        kind: TokenKind,
        start: usize,
        end: usize,
    ) -> Option<Result<Token<'de>, miette::Error>> {
        let literal = &self.whole[start..end];
        Some(Ok(Token {
            kind,
            span: TextSpan::new(start, end, literal),
        }))
    }

    fn match_punctuator(
        &self,
        c: char,
        start: usize,
        end: usize,
    ) -> Option<Result<Token<'de>, miette::Error>> {
        match c {
            '(' => self.just(TokenKind::LParen, start, end),
            ')' => self.just(TokenKind::RParen, start, end),
            '{' => self.just(TokenKind::LBrace, start, end),
            '}' => self.just(TokenKind::RBrace, start, end),
            '[' => self.just(TokenKind::LBracket, start, end),
            ']' => self.just(TokenKind::RBracket, start, end),
            ',' => self.just(TokenKind::Comma, start, end),
            ':' => self.just(TokenKind::Colon, start, end),
            ';' => self.just(TokenKind::SemiColon, start, end),
            '.' => self.just(TokenKind::Dot, start, end),
            _ => None,
        }
    }

    fn match_arithmetic(
        &mut self,
        c: char,
        start: usize,
        end: usize,
    ) -> Option<Result<Token<'de>, miette::Error>> {
        match c {
            '+' => self.just(TokenKind::Plus, start, end),
            '-' => self.just(TokenKind::Minus, start, end),
            '*' => self.just(TokenKind::Star, start, end),
            '%' => self.just(TokenKind::Percent, start, end),
            '/' => {
                if self.rest.starts_with('/') {
                    // It's a line comment "//"
                    self.rest = &self.rest[1..]; // Slice off only the second '/'
                    self.byte += 1; // Increment by 1 byte
                    let new_end = end + 1;
                    let literal = &self.whole[start..start + 2];
                    self.skip_singleline_comment();
                    Some(Ok(Token {
                        kind: TokenKind::LineComment,
                        span: TextSpan::new(start, new_end, literal),
                    }))
                } else if self.rest.starts_with('*') {
                    // It's a block comment "/*"
                    self.rest = &self.rest[1..]; // Slice off only the '*'
                    self.byte += 1; // Increment by 1 byte
                    let block_start = start;
                    if self.skip_multiline_comment() {
                        let block_end = self.byte;
                        let literal = &self.whole[block_start..block_start + 2];
                        Some(Ok(Token {
                            kind: TokenKind::BlockComment,
                            span: TextSpan::new(block_start, block_end, literal),
                        }))
                    } else {
                        // Unterminated block comment
                        let err = StringTerminationError {
                            src: self.whole.to_string(),
                            err_span: SourceSpan::from(start..self.byte),
                        };
                        Some(Err(err.into()))
                    }
                } else {
                    self.just(TokenKind::Slash, start, end)
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

    fn skip_multiline_comment(&mut self) -> bool {
        if let Some(idx) = self.rest.find("*/") {
            self.byte += idx + 2;
            self.rest = &self.rest[idx + 2..];
        } else {
            // Unterminated multi-line comment
            self.byte += self.rest.len();
            self.rest = "";
            return false;
        }
        true
    }

    fn match_comparison(
        &mut self,
        c: char,
        start: usize,
        end: usize,
    ) -> Option<Result<Token<'de>, miette::Error>> {
        match c {
            '=' => self.match_comparison_token(TokenKind::Equal, TokenKind::EqualEqual, start, end),
            '!' => self.match_comparison_token(TokenKind::Bang, TokenKind::BangEqual, start, end),
            '<' => self.match_comparison_token(TokenKind::Less, TokenKind::LessEqual, start, end),
            '>' => {
                self.match_comparison_token(TokenKind::Greater, TokenKind::GreaterEqual, start, end)
            }
            _ => None,
        }
    }

    fn match_comparison_token(
        &mut self,
        single_kind: TokenKind,
        double_kind: TokenKind,
        start: usize,
        end: usize,
    ) -> Option<Result<Token<'de>, miette::Error>> {
        if self.rest.starts_with('=') {
            self.rest = &self.rest[1..];
            self.byte += 1;
            let new_end = end + 1;
            self.just(double_kind, start, new_end)
        } else {
            self.just(single_kind, start, end)
        }
    }

    fn match_identifier_keyword(&mut self, c: char) -> Option<Result<Token<'de>, miette::Error>> {
        if c.is_ascii_alphabetic() || c == '_' {
            let start = self.byte - c.len_utf8();
            let mut end = self.byte;

            while let Some(next_char) = self.rest.chars().next() {
                if next_char.is_alphanumeric() || next_char == '_' {
                    self.rest = &self.rest[next_char.len_utf8()..];
                    end += next_char.len_utf8();
                    self.byte += next_char.len_utf8();
                } else {
                    break;
                }
            }

            let literal_slice = &self.whole[start..end];
            let kind = match literal_slice {
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

            Some(Ok(Token {
                kind,
                span: TextSpan::new(start, end, literal_slice),
            }))
        } else {
            None
        }
    }

    fn match_string(&mut self, c: char) -> Option<Result<Token<'de>, miette::Error>> {
        if c == '"' {
            let start = self.byte - c.len_utf8();
            let mut end = self.byte;
            let mut escaped = false;

            for (i, ch) in self.rest.char_indices() {
                end += ch.len_utf8();
                self.byte += ch.len_utf8();

                if escaped {
                    escaped = false;
                } else if ch == '\\' {
                    escaped = true;
                } else if ch == '"' {
                    let literal_slice = &self.whole[start..end];
                    self.rest = &self.rest[i + ch.len_utf8()..];
                    return Some(Ok(Token {
                        kind: TokenKind::String,
                        span: TextSpan::new(start, end, literal_slice),
                    }));
                } else if ch == '\n' {
                    // Unescaped newline in string
                    let err = StringTerminationError {
                        src: self.whole.to_string(),
                        err_span: SourceSpan::from(start..end),
                    };
                    self.rest = &self.rest[i + ch.len_utf8()..];
                    return Some(Err(err.into()));
                }
            }

            // Unterminated string
            let err = StringTerminationError {
                src: self.whole.to_string(),
                err_span: SourceSpan::from(start..end),
            };
            self.byte = end;
            self.rest = "";
            Some(Err(err.into()))
        } else {
            None
        }
    }

    fn match_number(&mut self, c: char) -> Option<Result<Token<'de>, miette::Error>> {
        if c.is_ascii_digit() || c == '.' {
            let start = self.byte - c.len_utf8();
            let mut end = self.byte;
            let mut has_dot = c == '.';
            let mut has_invalid_chars = false;

            while let Some(next_char) = self.rest.chars().next() {
                if next_char.is_ascii_digit() {
                    end += next_char.len_utf8();
                    self.byte += next_char.len_utf8();
                    self.rest = &self.rest[next_char.len_utf8()..];
                } else if next_char == '.' {
                    if has_dot {
                        has_invalid_chars = true;
                    } else {
                        has_dot = true;
                    }
                    end += next_char.len_utf8();
                    self.byte += next_char.len_utf8();
                    self.rest = &self.rest[next_char.len_utf8()..];
                } else if next_char.is_alphabetic() || next_char == '_' {
                    has_invalid_chars = true;
                    end += next_char.len_utf8();
                    self.byte += next_char.len_utf8();
                    self.rest = &self.rest[next_char.len_utf8()..];
                } else {
                    break;
                }
            }

            let literal_slice = &self.whole[start..end];

            if has_invalid_chars
                || (has_dot && (!literal_slice.contains('.') || literal_slice.ends_with('.')))
            {
                return Some(Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(start..end, "invalid number literal"),
                    ],
                    "Invalid number literal: `{}`",
                    literal_slice,
                }
                .with_source_code(self.whole.to_string())));
            }

            match literal_slice.parse::<f64>() {
                Ok(n) => Some(Ok(Token {
                    kind: TokenKind::Number(n),
                    span: TextSpan::new(start, end, literal_slice),
                })),
                Err(_) => Some(Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(start..end, "invalid number literal"),
                    ],
                    "Invalid number literal: `{}`",
                    literal_slice,
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

        // Skip over any whitespace
        while let Some(c) = self.rest.chars().next() {
            if c.is_whitespace() {
                self.byte += c.len_utf8();
                self.rest = &self.rest[c.len_utf8()..];
            } else {
                break;
            }
        }

        // Check if we've reached the end of input
        if self.rest.is_empty() {
            if !self.emitted_eof {
                self.emitted_eof = true;
                return Some(Ok(Token {
                    kind: TokenKind::EOF,
                    span: TextSpan::new(self.byte, self.byte, ""),
                }));
            } else {
                return None;
            }
        }

        let c = self.rest.chars().next().unwrap();
        let start = self.byte;
        let end = start + c.len_utf8();
        self.rest = &self.rest[c.len_utf8()..];
        self.byte += c.len_utf8();

        // Match punctuators and delimiters
        if let Some(kind) = self.match_punctuator(c, start, end) {
            return Some(kind);
        }

        // Match arithmetic operators, handling comments or division
        if let Some(kind) = self.match_arithmetic(c, start, end) {
            return Some(kind);
        }

        // Match comparison operators
        if let Some(kind) = self.match_comparison(c, start, end) {
            return Some(kind);
        }

        // Match identifiers and keywords
        if let Some(kind) = self.match_identifier_keyword(c) {
            return Some(kind);
        }

        // Match string literals
        if let Some(kind) = self.match_string(c) {
            return Some(kind);
        }

        // Match numbers
        if let Some(kind) = self.match_number(c) {
            return Some(kind);
        }

        // Handle unrecognized tokens
        return self.just(TokenKind::Bad, start, end);
    }
}
