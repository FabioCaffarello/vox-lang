use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    // Punctuators and Delimiters
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Comma, Colon, SemiColon, Dot,

    // Keywords
    If, Else, While, For, Return, Fun,
    True, False, Nil,
    String, Number(f64), Array, Dict,
    And, Or, Not, In,

    // Text Processing Keywords
    Split, Join, Map, Filter, Reduce, Replace, Extract,

    // Literals
    Ident,

    // Built-in Functions
    Length, TypeOf, 
    // ToNumber, ToString,
    // UpperCase, LowerCase, Trim, Contains, Substring, IndexOf,

    // Special Symbols
    Pipeline, Range, Concatenate,

    // Arithmetic Operators
    Plus, Minus, Star, Slash, Percent,

    // Comparison Operators
    Equal, Bang, BangEqual, EqualEqual, Greater, GreaterEqual, Less, LessEqual,

    // Comments
    LineComment, BlockComment,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'de> {
    pub origin: &'de str,
    pub offset: usize,
    pub kind: TokenKind,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let origin = self.origin;
        match self.kind {
            // Punctuators and Delimiters
            TokenKind::LParen => write!(f, "LEFT_PAREN {origin} null"),
            TokenKind::RParen => write!(f, "RIGHT_PAREN {origin} null"),
            TokenKind::LBrace => write!(f, "LEFT_BRACE {origin} null"),
            TokenKind::RBrace => write!(f, "RIGHT_BRACE {origin} null"),
            TokenKind::LBracket => write!(f, "LEFT_BRACKET {origin} null"),
            TokenKind::RBracket => write!(f, "RIGHT_BRACKET {origin} null"),
            TokenKind::Comma => write!(f, "COMMA {origin} null"),
            TokenKind::Colon => write!(f, "COLON {origin} null"),
            TokenKind::SemiColon => write!(f, "SEMICOLON {origin} null"),
            TokenKind::Dot => write!(f, "DOT {origin} null"),
            // Keywords
            TokenKind::If => write!(f, "IF {origin} null"),
            TokenKind::Else => write!(f, "ELSE {origin} null"),
            TokenKind::While => write!(f, "WHILE {origin} null"),
            TokenKind::For => write!(f, "FOR {origin} null"),
            TokenKind::Return => write!(f, "RETURN {origin} null"),
            TokenKind::Fun => write!(f, "FUN {origin} null"),
            TokenKind::True => write!(f, "TRUE {origin} null"),
            TokenKind::False => write!(f, "FALSE {origin} null"),
            TokenKind::Nil => write!(f, "NIL {origin} null"),
            TokenKind::String => write!(f, "STRING {origin} null"),
            TokenKind::Number(n) => {
                if n == n.trunc() {
                    // tests require that integers are printed as N.0
                    write!(f, "NUMBER {origin} {n}.0")
                } else {
                    write!(f, "NUMBER {origin} {n}")
                }
            }
            TokenKind::Array => write!(f, "ARRAY {origin} null"),
            TokenKind::Dict => write!(f, "DICT {origin} null"),
            TokenKind::And => write!(f, "AND {origin} null"),
            TokenKind::Or => write!(f, "OR {origin} null"),
            TokenKind::Not => write!(f, "NOT {origin} null"),
            TokenKind::In => write!(f, "IN {origin} null"),
            // Text Processing Keywords
            TokenKind::Split => write!(f, "SPLIT {origin} null"),
            TokenKind::Join => write!(f, "JOIN {origin} null"),
            TokenKind::Map => write!(f, "MAP {origin} null"),
            TokenKind::Filter => write!(f, "FILTER {origin} null"),
            TokenKind::Reduce => write!(f, "REDUCE {origin} null"),
            TokenKind::Replace => write!(f, "REPLACE {origin} null"),
            TokenKind::Extract => write!(f, "EXTRACT {origin} null"),
            // Literals
            TokenKind::Ident => write!(f, "IDENTIFIER {origin} null"),
            // Built-in Functions
            TokenKind::Length => write!(f, "LENGTH {origin} null"),
            TokenKind::TypeOf => write!(f, "TYPE_OF {origin} null"),
            // TokenKind::ToNumber => write!(f, "TO_NUMBER {origin} null"),
            // TokenKind::ToString => write!(f, "TO_STRING {origin} null"),
            // TokenKind::UpperCase => write!(f, "UPPER_CASE {origin} null"),
            // TokenKind::LowerCase => write!(f, "LOWER_CASE {origin} null"),
            // TokenKind::Trim => write!(f, "TRIM {origin} null"),
            // TokenKind::Contains => write!(f, "CONTAINS {origin} null"),
            // TokenKind::Substring => write!(f, "SUBSTRING {origin} null"),
            // TokenKind::IndexOf => write!(f, "INDEX_OF {origin} null"),
            // Special Symbols
            TokenKind::Pipeline => write!(f, "PIPELINE {origin} null"),
            TokenKind::Range => write!(f, "RANGE {origin} null"),
            TokenKind::Concatenate => write!(f, "CONCATENATE {origin} null"),
            // Arithmetic Operators
            TokenKind::Plus => write!(f, "PLUS {origin} null"),
            TokenKind::Minus => write!(f, "MINUS {origin} null"),
            TokenKind::Star => write!(f, "STAR {origin} null"),
            TokenKind::Slash => write!(f, "SLASH {origin} null"),
            TokenKind::Percent => write!(f, "PERCENT {origin} null"),
            // Comparison operators
            TokenKind::Equal => write!(f, "EQUAL {origin} null"),
            TokenKind::Bang => write!(f, "BANG {origin} null"),
            TokenKind::BangEqual => write!(f, "BANG_EQUAL {origin} null"),
            TokenKind::EqualEqual => write!(f, "EQUAL_EQUAL {origin} null"),
            TokenKind::Greater => write!(f, "GREATER {origin} null"),
            TokenKind::GreaterEqual => write!(f, "GREATER_EQUAL {origin} null"),
            TokenKind::Less => write!(f, "LESS {origin} null"),
            TokenKind::LessEqual => write!(f, "LESS_EQUAL {origin} null"),
            // Comments
            TokenKind::LineComment => write!(f, "LINE_COMMENT {origin} null"),
            TokenKind::BlockComment => write!(f, "BLOCK_COMMENT {origin} null"),
        }
    }
}
