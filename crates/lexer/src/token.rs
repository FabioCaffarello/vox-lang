use std::fmt;
use text::span::TextSpan;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
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
    For,
    Return,
    Fun,
    True,
    False,
    Nil,
    String,
    Number(f64),
    // Array,
    // Dict,
    And,
    Or,
    Not,
    In,

    // Text Processing Keywords
    Split,
    Join,
    Map,
    Filter,
    Reduce,
    Replace,
    Extract,

    // Literals
    Ident,

    // Built-in Functions
    Length,
    TypeOf,
    Range,
    Concatenate,
    // ToNumber, ToString,
    // UpperCase, LowerCase, Trim, Contains, Substring, IndexOf,

    // Special Symbols
    // Pipeline,

    // Arithmetic Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    // Comparison Operators
    Equal,
    Bang,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Comments
    LineComment,
    BlockComment,

    EOF,
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
            TokenKind::For => write!(f, "FOR {literal} null"),
            TokenKind::Return => write!(f, "RETURN {literal} null"),
            TokenKind::Fun => write!(f, "FUN {literal} null"),
            TokenKind::True => write!(f, "TRUE {literal} null"),
            TokenKind::False => write!(f, "FALSE {literal} null"),
            TokenKind::Nil => write!(f, "NIL {literal} null"),
            TokenKind::And => write!(f, "AND {literal} null"),
            TokenKind::Or => write!(f, "OR {literal} null"),
            TokenKind::Not => write!(f, "NOT {literal} null"),
            TokenKind::In => write!(f, "IN {literal} null"),
            // Text Processing Keywords
            TokenKind::Split => write!(f, "SPLIT {literal} null"),
            TokenKind::Join => write!(f, "JOIN {literal} null"),
            TokenKind::Map => write!(f, "MAP {literal} null"),
            TokenKind::Filter => write!(f, "FILTER {literal} null"),
            TokenKind::Reduce => write!(f, "REDUCE {literal} null"),
            TokenKind::Replace => write!(f, "REPLACE {literal} null"),
            TokenKind::Extract => write!(f, "EXTRACT {literal} null"),
            // Strings and Numbers
            TokenKind::String => write!(f, "STRING {literal} null"),
            TokenKind::Number(n) => {
                if n == n.trunc() {
                    // tests require that integers are printed as N.0
                    write!(f, "NUMBER {literal} {n}.0")
                } else {
                    write!(f, "NUMBER {literal} {n}")
                }
            }
            // TokenKind::Array => write!(f, "ARRAY {literal} null"),
            // TokenKind::Dict => write!(f, "DICT {literal} null"),
            // Literals
            TokenKind::Ident => write!(f, "IDENTIFIER {literal} null"),
            // Built-in Functions
            TokenKind::Length => write!(f, "LENGTH {literal} null"),
            TokenKind::TypeOf => write!(f, "TYPE_OF {literal} null"),
            TokenKind::Range => write!(f, "RANGE {literal} null"),
            TokenKind::Concatenate => write!(f, "CONCATENATE {literal} null"),
            // TokenKind::ToNumber => write!(f, "TO_NUMBER {literal} null"),
            // TokenKind::ToString => write!(f, "TO_STRING {literal} null"),
            // TokenKind::UpperCase => write!(f, "UPPER_CASE {literal} null"),
            // TokenKind::LowerCase => write!(f, "LOWER_CASE {literal} null"),
            // TokenKind::Trim => write!(f, "TRIM {literal} null"),
            // TokenKind::Contains => write!(f, "CONTAINS {literal} null"),
            // TokenKind::Substring => write!(f, "SUBSTRING {literal} null"),
            // TokenKind::IndexOf => write!(f, "INDEX_OF {literal} null"),
            // Special Symbols
            // TokenKind::Pipeline => write!(f, "PIPELINE {literal} null"),
            // Arithmetic Operators
            TokenKind::Plus => write!(f, "PLUS {literal} null"),
            TokenKind::Minus => write!(f, "MINUS {literal} null"),
            TokenKind::Star => write!(f, "STAR {literal} null"),
            TokenKind::Slash => write!(f, "SLASH {literal} null"),
            TokenKind::Percent => write!(f, "PERCENT {literal} null"),
            // Comparison operators
            TokenKind::Equal => write!(f, "EQUAL {literal} null"),
            TokenKind::Bang => write!(f, "BANG {literal} null"),
            TokenKind::BangEqual => write!(f, "BANG_EQUAL {literal} null"),
            TokenKind::EqualEqual => write!(f, "EQUAL_EQUAL {literal} null"),
            TokenKind::Greater => write!(f, "GREATER {literal} null"),
            TokenKind::GreaterEqual => write!(f, "GREATER_EQUAL {literal} null"),
            TokenKind::Less => write!(f, "LESS {literal} null"),
            TokenKind::LessEqual => write!(f, "LESS_EQUAL {literal} null"),
            // Comments
            TokenKind::LineComment => write!(f, "LINE_COMMENT {literal} null"),
            TokenKind::BlockComment => write!(f, "BLOCK_COMMENT {literal} null"),

            TokenKind::EOF => write!(f, "EOF {literal} null"),
        }
    }
}
