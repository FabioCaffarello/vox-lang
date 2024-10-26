// tests/lexer_tests.rs

use lexer::{Lexer, Token, TokenKind};

// Removed unused import: use miette::Error;

// Helper function to create tokens
fn create_token(origin: &str, offset: usize, kind: TokenKind) -> Token {
    Token {
        origin,
        offset,
        kind,
    }
}

#[test]
fn test_empty_input() {
    let input = "";
    let mut lexer = Lexer::new(input);
    assert!(
        lexer.next().is_none(),
        "Lexer should return None for empty input"
    );
}

#[test]
fn test_punctuators() {
    let input = "(){},[].:"; // LParen, RParen, LBrace, RBrace, Comma
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("(", 0, TokenKind::LParen),
        create_token(")", 1, TokenKind::RParen),
        create_token("{", 2, TokenKind::LBrace),
        create_token("}", 3, TokenKind::RBrace),
        create_token(",", 4, TokenKind::Comma),
        create_token("[", 5, TokenKind::LBracket),
        create_token("]", 6, TokenKind::RBracket),
        create_token(".", 7, TokenKind::Dot),
        create_token(":", 8, TokenKind::Colon),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(token, expected, "Token does not match expected value");
    }

    assert!(
        lexer.next().is_none(),
        "Lexer should return None after all tokens are consumed"
    );
}

#[test]
fn test_identifiers_and_keywords() {
    let input = "and nil variable_1 _private var123 or true false";
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("and", 0, TokenKind::And),
        create_token("nil", 4, TokenKind::Nil),
        create_token("variable_1", 8, TokenKind::Ident),
        create_token("_private", 19, TokenKind::Ident),
        create_token("var123", 28, TokenKind::Ident),
        create_token("or", 35, TokenKind::Or),
        create_token("true", 38, TokenKind::True),
        create_token("false", 43, TokenKind::False),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(token, expected, "Token does not match expected value");
    }

    assert!(
        lexer.next().is_none(),
        "Lexer should return None after all tokens are consumed"
    );
}

#[test]
fn test_numbers() {
    let input = "123 456.789 0.001 42.0";
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("123", 0, TokenKind::Number(123.0)),
        create_token("456.789", 4, TokenKind::Number(456.789)),
        create_token("0.001", 12, TokenKind::Number(0.001)),
        create_token("42.0", 18, TokenKind::Number(42.0)),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(
            token.kind, expected.kind,
            "Token kind does not match expected kind"
        );
        assert_eq!(
            token.origin, expected.origin,
            "Token origin does not match expected origin"
        );
    }

    assert!(
        lexer.next().is_none(),
        "Lexer should return None after all tokens are consumed"
    );
}

#[test]
fn test_strings() {
    let input = r#""hello" "world" "unterminated"#;
    let mut lexer = Lexer::new(input);

    // First string
    let token1 = lexer.next();
    assert!(token1.is_some(), "Expected first string token but got None");
    let token1 = token1.unwrap();
    assert!(
        token1.is_ok(),
        "Expected Ok(Token) for first string but got Err"
    );
    let token1 = token1.unwrap();
    assert_eq!(
        token1.kind,
        TokenKind::String,
        "First token kind is not String"
    );
    assert_eq!(
        token1.origin, "\"hello\"",
        "First token origin does not match"
    );

    // Second string
    let token2 = lexer.next();
    assert!(
        token2.is_some(),
        "Expected second string token but got None"
    );
    let token2 = token2.unwrap();
    assert!(
        token2.is_ok(),
        "Expected Ok(Token) for second string but got Err"
    );
    let token2 = token2.unwrap();
    assert_eq!(
        token2.kind,
        TokenKind::String,
        "Second token kind is not String"
    );
    assert_eq!(
        token2.origin, "\"world\"",
        "Second token origin does not match"
    );

    // Third string (unterminated)
    let error = lexer.next();
    assert!(
        error.is_some(),
        "Expected an error for unterminated string but got None"
    );
    let error = error.unwrap();
    assert!(
        error.is_err(),
        "Expected Err for unterminated string but got Ok"
    );
    let error = error.unwrap_err();
    let error_message = format!("{}", error); // Corrected format! usage
    assert!(
        error_message.contains("Unterminated string"),
        "Error message does not contain 'Unterminated string'"
    );

    // No more tokens
    assert!(
        lexer.next().is_none(),
        "Lexer should return None after all tokens are consumed"
    );
}

#[test]
fn test_whitespace_variations() {
    let input = "  \tlet\nx 42\r\n";
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("let", 3, TokenKind::Ident),
        create_token("x", 7, TokenKind::Ident),
        create_token("42", 9, TokenKind::Number(42.0)),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(token, expected, "Token does not match expected value");
    }

    assert!(
        lexer.next().is_none(),
        "Lexer should return None after all tokens are consumed"
    );
}

#[test]
fn test_operators() {
    let input = "+ - * / %";
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("+", 0, TokenKind::Plus),
        create_token("-", 2, TokenKind::Minus),
        create_token("*", 4, TokenKind::Star),
        create_token("/", 6, TokenKind::Slash),
        create_token("%", 8, TokenKind::Percent),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(token, expected, "Token does not match expected value");
    }

    assert!(
        lexer.next().is_none(),
        "Lexer should return None after all tokens are consumed"
    );
}

#[test]
fn test_comments() {
    let input = r#"// This is a single-line comment
x = 42; // Another comment
/* This is a block comment 
Need to skip the entire block*/
"#;
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("//", 0, TokenKind::LineComment),
        create_token("x", 33, TokenKind::Ident),
        create_token("=", 35, TokenKind::Equal),
        create_token("42", 37, TokenKind::Number(42.0)),
        create_token(";", 39, TokenKind::SemiColon),
        create_token("//", 41, TokenKind::LineComment),
        create_token("/*", 60, TokenKind::BlockComment),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(token, expected, "Token does not match expected value");
    }

    // Ensure no more tokens
    assert!(
        lexer.next().is_none(),
        "Lexer should return None after all tokens are consumed"
    );
}

#[test]
fn test_unrecognized_characters() {
    let input = "@ # $ ^ &";
    let mut lexer = Lexer::new(input);

    while let Some(result) = lexer.next() {
        assert!(result.is_err(), "Expected an error for unrecognized characters");
    }
}

#[test]
fn test_nested_comments() {
    let input = "/* outer /* inner */ still outer */";
    let mut lexer = Lexer::new(input);

    let token_result = lexer.next();
    assert!(token_result.is_some(), "Expected a token but got None");
    assert!(
        token_result.unwrap().is_ok(),
        "Expected Ok for nested comment handling"
    );
}

#[test]
fn test_strings_with_escapes() {
    let input = r#""hello\nworld" "escaped_quote\"inside""#;
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("\"hello\\nworld\"", 0, TokenKind::String),
        create_token("\"escaped_quote\\\"inside\"", 14, TokenKind::String),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(token.kind, expected.kind, "Token kind does not match expected kind");
        assert_eq!(token.origin, expected.origin, "Token origin does not match expected origin");
    }
}

#[test]
fn test_adjacent_operators_and_punctuation() {
    let input = "a=b+c;";
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("a", 0, TokenKind::Ident),
        create_token("=", 1, TokenKind::Equal),
        create_token("b", 2, TokenKind::Ident),
        create_token("+", 3, TokenKind::Plus),
        create_token("c", 4, TokenKind::Ident),
        create_token(";", 5, TokenKind::SemiColon),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(token, expected, "Token does not match expected value");
    }

    assert!(lexer.next().is_none(), "Lexer should return None after all tokens are consumed");
}

#[test]
fn test_identifiers_with_keywords() {
    let input = "or_1 and2";
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("or_1", 0, TokenKind::Ident),
        create_token("and2", 5, TokenKind::Ident),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(token, expected, "Token does not match expected value");
    }

    assert!(lexer.next().is_none(), "Lexer should return None after all tokens are consumed");
}

#[test]
fn test_complex_nested_expressions() {
    let input = "((a + b) * (c - d))";
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("(", 0, TokenKind::LParen),
        create_token("(", 1, TokenKind::LParen),
        create_token("a", 2, TokenKind::Ident),
        create_token("+", 4, TokenKind::Plus),
        create_token("b", 6, TokenKind::Ident),
        create_token(")", 7, TokenKind::RParen),
        create_token("*", 9, TokenKind::Star),
        create_token("(", 11, TokenKind::LParen),
        create_token("c", 12, TokenKind::Ident),
        create_token("-", 14, TokenKind::Minus),
        create_token("d", 16, TokenKind::Ident),
        create_token(")", 17, TokenKind::RParen),
        create_token(")", 18, TokenKind::RParen),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(token, expected, "Token does not match expected value");
    }

    assert!(lexer.next().is_none(), "Lexer should return None after all tokens are consumed");
}
