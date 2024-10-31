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
    Identifier,
    Let,

    // Built-in Functions
    Length,
    TypeOf,
    Range,
    Concatenate,

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
            TokenKind::For => write!(f, "FOR"),
            TokenKind::Return => write!(f, "RETURN"),
            TokenKind::Fun => write!(f, "FUN"),
            TokenKind::True => write!(f, "TRUE"),
            TokenKind::False => write!(f, "FALSE"),
            TokenKind::Nil => write!(f, "NIL"),
            TokenKind::String => write!(f, "STRING"),
            TokenKind::Number(_) => write!(f, "NUMBER"),
            TokenKind::Identifier => write!(f, "IDENTIFIER"),
            TokenKind::Length => write!(f, "LENGTH"),
            TokenKind::TypeOf => write!(f, "TYPE_OF"),
            TokenKind::Range => write!(f, "RANGE"),
            TokenKind::Concatenate => write!(f, "CONCATENATE"),
            TokenKind::And => write!(f, "AND"),
            TokenKind::Or => write!(f, "OR"),
            TokenKind::Not => write!(f, "NOT"),
            TokenKind::In => write!(f, "IN"),
            TokenKind::Split => write!(f, "SPLIT"),
            TokenKind::Join => write!(f, "JOIN"),
            TokenKind::Map => write!(f, "MAP"),
            TokenKind::Filter => write!(f, "FILTER"),
            TokenKind::Reduce => write!(f, "REDUCE"),
            TokenKind::Replace => write!(f, "REPLACE"),
            TokenKind::Extract => write!(f, "EXTRACT"),
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
            // Literals
            TokenKind::Identifier => write!(f, "{literal}"),
            TokenKind::Let => write!(f, "LET {literal} null"),
            // Built-in Functions
            TokenKind::Length => write!(f, "LENGTH {literal} null"),
            TokenKind::TypeOf => write!(f, "TYPE_OF {literal} null"),
            TokenKind::Range => write!(f, "RANGE {literal} null"),
            TokenKind::Concatenate => write!(f, "CONCATENATE {literal} null"),
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
            TokenKind::Bad => write!(f, "BAD {literal} null"),
            TokenKind::EOF => write!(f, "EOF {literal} null"),
        }
    }
}
