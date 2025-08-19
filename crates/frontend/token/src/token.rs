use std::fmt;
use text::span::TextSpan;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    // Literals
    String,
    Number(f64),
    // Punctuators and Delimiters
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Colon,
    SemiColon,
    Dot,
    // Keywords
    If,
    Else,
    While,
    Break,
    For,
    Fun,
    Return,
    True,
    False,
    Nil,
    And,
    Or,
    Not,
    In,
    Let,
    // Built-in Functions
    // Arithmetic Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    DoubleStar,
    // Comparison Operators
    Equal,
    Bang,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Others
    Arrow,
    Identifier,
    LineComment,
    BlockComment,
    Bad,
    EOF,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::LParen => write!(f, "LEFT_PAREN"),
            TokenKind::RParen => write!(f, "RIGHT_PAREN"),
            TokenKind::LBrace => write!(f, "LEFT_BRACE"),
            TokenKind::RBrace => write!(f, "RIGHT_BRACE"),
            TokenKind::LBracket => write!(f, "LEFT_BRACKET"),
            TokenKind::RBracket => write!(f, "RIGHT_BRACKET"),
            TokenKind::Comma => write!(f, "COMMA"),
            TokenKind::Colon => write!(f, "COLON"),
            TokenKind::SemiColon => write!(f, "SEMICOLON"),
            TokenKind::Dot => write!(f, "DOT"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::DoubleStar => write!(f, "**"),
            TokenKind::Percent => write!(f, "PERCENT"),
            TokenKind::Equal => write!(f, "EQUAL"),
            TokenKind::Bang => write!(f, "BANG"),
            TokenKind::BangEqual => write!(f, "BANG_EQUAL"),
            TokenKind::EqualEqual => write!(f, "EQUAL_EQUAL"),
            TokenKind::Greater => write!(f, "GREATER"),
            TokenKind::GreaterEqual => write!(f, "GREATER_EQUAL"),
            TokenKind::Less => write!(f, "LESS"),
            TokenKind::LessEqual => write!(f, "LESS_EQUAL"),
            TokenKind::If => write!(f, "IF"),
            TokenKind::Else => write!(f, "ELSE"),
            TokenKind::While => write!(f, "WHILE"),
            TokenKind::Break => write!(f, "BREAK"),
            TokenKind::For => write!(f, "FOR"),
            TokenKind::Return => write!(f, "RETURN"),
            TokenKind::Fun => write!(f, "FUNC"),
            TokenKind::True => write!(f, "TRUE"),
            TokenKind::False => write!(f, "FALSE"),
            TokenKind::Nil => write!(f, "NIL"),
            TokenKind::String => write!(f, "STRING"),
            TokenKind::Number(_) => write!(f, "NUMBER"),
            TokenKind::Identifier => write!(f, "IDENTIFIER"),
            TokenKind::And => write!(f, "AND"),
            TokenKind::Or => write!(f, "OR"),
            TokenKind::Not => write!(f, "NOT"),
            TokenKind::In => write!(f, "IN"),
            TokenKind::Arrow => write!(f, "ARROW"),
            TokenKind::LineComment => write!(f, "LINE_COMMENT"),
            TokenKind::BlockComment => write!(f, "BLOCK_COMMENT"),
            TokenKind::Let => write!(f, "LET"),
            TokenKind::Bad => write!(f, "BAD"),
            TokenKind::EOF => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'de> {
    pub span: TextSpan<'de>,
    pub kind: TokenKind,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let literal = self.span.literal();
        match self.kind {
            // Literals
            TokenKind::String => write!(f, "STRING {literal} null"),
            TokenKind::Number(n) => {
                if n == n.trunc() {
                    // tests require that integers are printed as N.0
                    write!(f, "NUMBER {literal} {n}.0")
                } else {
                    write!(f, "NUMBER {literal} {n}")
                }
            }
            // Punctuators and Delimiters
            TokenKind::LParen => write!(f, "LEFT_PAREN {literal} null"),
            TokenKind::RParen => write!(f, "RIGHT_PAREN {literal} null"),
            TokenKind::LBrace => write!(f, "LEFT_BRACE {literal} null"),
            TokenKind::RBrace => write!(f, "RIGHT_BRACE {literal} null"),
            TokenKind::LBracket => write!(f, "LEFT_BRACKET {literal} null"),
            TokenKind::RBracket => write!(f, "RIGHT_BRACKET {literal} null"),
            TokenKind::Comma => write!(f, "COMMA {literal} null"),
            TokenKind::Colon => write!(f, "COLON {literal} null"),
            TokenKind::SemiColon => write!(f, "SEMICOLON {literal} null"),
            TokenKind::Dot => write!(f, "DOT {literal} null"),
            // Keywords
            TokenKind::If => write!(f, "IF {literal} null"),
            TokenKind::Else => write!(f, "ELSE {literal} null"),
            TokenKind::While => write!(f, "WHILE {literal} null"),
            TokenKind::Break => write!(f, "BREAK {literal} null"),
            TokenKind::For => write!(f, "FOR {literal} null"),
            TokenKind::Return => write!(f, "RETURN {literal} null"),
            TokenKind::Fun => write!(f, "FUNC {literal} null"),
            TokenKind::True => write!(f, "TRUE {literal} null"),
            TokenKind::False => write!(f, "FALSE {literal} null"),
            TokenKind::Nil => write!(f, "NIL {literal} null"),
            TokenKind::And => write!(f, "AND {literal} null"),
            TokenKind::Or => write!(f, "OR {literal} null"),
            TokenKind::Not => write!(f, "NOT {literal} null"),
            TokenKind::In => write!(f, "IN {literal} null"),
            // Built-in Functions
            TokenKind::Identifier => write!(f, "{literal}"),
            TokenKind::Let => write!(f, "LET {literal} null"),
            // Arithmetic Operators
            TokenKind::Plus => write!(f, "PLUS {literal} null"),
            TokenKind::Minus => write!(f, "MINUS {literal} null"),
            TokenKind::Star => write!(f, "STAR {literal} null"),
            TokenKind::Slash => write!(f, "SLASH {literal} null"),
            TokenKind::Percent => write!(f, "PERCENT {literal} null"),
            TokenKind::DoubleStar => write!(f, "DOUBLE_STAR {literal} null"),
            // Comparison operators
            TokenKind::Equal => write!(f, "EQUAL {literal} null"),
            TokenKind::Bang => write!(f, "BANG {literal} null"),
            TokenKind::BangEqual => write!(f, "BANG_EQUAL {literal} null"),
            TokenKind::EqualEqual => write!(f, "EQUAL_EQUAL {literal} null"),
            TokenKind::Greater => write!(f, "GREATER {literal} null"),
            TokenKind::GreaterEqual => write!(f, "GREATER_EQUAL {literal} null"),
            TokenKind::Less => write!(f, "LESS {literal} null"),
            TokenKind::LessEqual => write!(f, "LESS_EQUAL {literal} null"),
            // Others
            TokenKind::Arrow => write!(f, "ARROW {literal} null"),
            TokenKind::LineComment => write!(f, "LINE_COMMENT {literal} null"),
            TokenKind::BlockComment => write!(f, "BLOCK_COMMENT {literal} null"),
            TokenKind::Bad => write!(f, "BAD {literal} null"),
            TokenKind::EOF => write!(f, "EOF {literal} null"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt::Write as _;

    fn mk_span(lit: &str) -> TextSpan<'_> {
        TextSpan::new(0, lit.len(), lit)
    }

    fn fmt<T: std::fmt::Display>(x: T) -> String {
        let mut s = String::new();
        write!(&mut s, "{x}").unwrap();
        s
    }

    #[test]
    fn token_kind_display_punct_delims() {
        assert_eq!(fmt(TokenKind::LParen), "LEFT_PAREN");
        assert_eq!(fmt(TokenKind::RParen), "RIGHT_PAREN");
        assert_eq!(fmt(TokenKind::LBrace), "LEFT_BRACE");
        assert_eq!(fmt(TokenKind::RBrace), "RIGHT_BRACE");
        assert_eq!(fmt(TokenKind::LBracket), "LEFT_BRACKET");
        assert_eq!(fmt(TokenKind::RBracket), "RIGHT_BRACKET");
        assert_eq!(fmt(TokenKind::Comma), "COMMA");
        assert_eq!(fmt(TokenKind::Colon), "COLON");
        assert_eq!(fmt(TokenKind::SemiColon), "SEMICOLON");
        assert_eq!(fmt(TokenKind::Dot), "DOT");
    }

    #[test]
    fn token_kind_display_keywords_and_ops() {
        // Keywords
        assert_eq!(fmt(TokenKind::If), "IF");
        assert_eq!(fmt(TokenKind::Else), "ELSE");
        assert_eq!(fmt(TokenKind::While), "WHILE");
        assert_eq!(fmt(TokenKind::Break), "BREAK");
        assert_eq!(fmt(TokenKind::For), "FOR");
        assert_eq!(fmt(TokenKind::Fun), "FUNC");
        assert_eq!(fmt(TokenKind::Return), "RETURN");
        assert_eq!(fmt(TokenKind::True), "TRUE");
        assert_eq!(fmt(TokenKind::False), "FALSE");
        assert_eq!(fmt(TokenKind::Nil), "NIL");
        assert_eq!(fmt(TokenKind::And), "AND");
        assert_eq!(fmt(TokenKind::Or), "OR");
        assert_eq!(fmt(TokenKind::Not), "NOT");
        assert_eq!(fmt(TokenKind::In), "IN");
        assert_eq!(fmt(TokenKind::Let), "LET");

        // Arithmetic
        assert_eq!(fmt(TokenKind::Plus), "+");
        assert_eq!(fmt(TokenKind::Minus), "-");
        assert_eq!(fmt(TokenKind::Star), "*");
        assert_eq!(fmt(TokenKind::Slash), "/");
        assert_eq!(fmt(TokenKind::Percent), "PERCENT");
        assert_eq!(fmt(TokenKind::DoubleStar), "**");

        // Comparison
        assert_eq!(fmt(TokenKind::Equal), "EQUAL");
        assert_eq!(fmt(TokenKind::Bang), "BANG");
        assert_eq!(fmt(TokenKind::BangEqual), "BANG_EQUAL");
        assert_eq!(fmt(TokenKind::EqualEqual), "EQUAL_EQUAL");
        assert_eq!(fmt(TokenKind::Greater), "GREATER");
        assert_eq!(fmt(TokenKind::GreaterEqual), "GREATER_EQUAL");
        assert_eq!(fmt(TokenKind::Less), "LESS");
        assert_eq!(fmt(TokenKind::LessEqual), "LESS_EQUAL");

        // Others
        assert_eq!(fmt(TokenKind::Arrow), "ARROW");
        assert_eq!(fmt(TokenKind::Identifier), "IDENTIFIER");
        assert_eq!(fmt(TokenKind::LineComment), "LINE_COMMENT");
        assert_eq!(fmt(TokenKind::BlockComment), "BLOCK_COMMENT");
        assert_eq!(fmt(TokenKind::Bad), "BAD");
        assert_eq!(fmt(TokenKind::EOF), "EOF");

        // Literals
        assert_eq!(fmt(TokenKind::String), "STRING");
        assert_eq!(fmt(TokenKind::Number(123.0)), "NUMBER");
    }

    #[test]
    fn token_display_keywords_and_punct() {
        let cases: &[(TokenKind, &str, &str)] = &[
            (TokenKind::If, "if", "IF if null"),
            (TokenKind::Else, "else", "ELSE else null"),
            (TokenKind::LParen, "(", "LEFT_PAREN ( null"),
            (TokenKind::RParen, ")", "RIGHT_PAREN ) null"),
            (TokenKind::Comma, ",", "COMMA , null"),
            (TokenKind::Colon, ":", "COLON : null"),
            (TokenKind::SemiColon, ";", "SEMICOLON ; null"),
            (TokenKind::Dot, ".", "DOT . null"),
            (TokenKind::Plus, "+", "PLUS + null"),
            (TokenKind::BangEqual, "!=", "BANG_EQUAL != null"),
            (TokenKind::EqualEqual, "==", "EQUAL_EQUAL == null"),
            (TokenKind::Arrow, "=>", "ARROW => null"),
            (TokenKind::Let, "let", "LET let null"),
            (TokenKind::EOF, "", "EOF  null"),
        ];

        for (kind, lit, expected) in cases {
            let tok = Token {
                span: mk_span(lit),
                kind: *kind,
            };
            assert_eq!(fmt(tok), *expected, "failed for literal `{lit}`");
        }
    }

    #[test]
    fn token_display_identifier_and_string_literal() {
        let ident = Token {
            span: mk_span("myVar"),
            kind: TokenKind::Identifier,
        };
        assert_eq!(fmt(ident), "myVar");

        let str_tok = Token {
            span: mk_span("\"hello\""),
            kind: TokenKind::String,
        };
        assert_eq!(fmt(str_tok), "STRING \"hello\" null");
    }

    #[test]
    fn token_display_number_formats() {
        let n_int = Token {
            span: mk_span("123"),
            kind: TokenKind::Number(123.0),
        };
        assert_eq!(fmt(n_int), "NUMBER 123 123.0");

        let n_dec = Token {
            span: mk_span("3.14"),
            kind: TokenKind::Number(3.14),
        };
        assert_eq!(fmt(n_dec), "NUMBER 3.14 3.14");
    }

    #[test]
    fn token_display_comments_and_bad() {
        let line_c = Token {
            span: mk_span("// hi"),
            kind: TokenKind::LineComment,
        };
        assert_eq!(fmt(line_c), "LINE_COMMENT // hi null");

        let block_c = Token {
            span: mk_span("/* x */"),
            kind: TokenKind::BlockComment,
        };
        assert_eq!(fmt(block_c), "BLOCK_COMMENT /* x */ null");

        let bad = Token {
            span: mk_span("@"),
            kind: TokenKind::Bad,
        };
        assert_eq!(fmt(bad), "BAD @ null");
    }
}
