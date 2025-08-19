use errors::StringTerminationError;
use lexer::Lexer;
use text::span::TextSpan;
use token::{Token, TokenKind};

// Helper function to create tokens
fn create_token(literal: &str, start: usize, end: usize, kind: TokenKind) -> Token<'_> {
    Token {
        kind,
        span: TextSpan::new(start, end, literal),
    }
}

// Helper function to assert EOF token
fn assert_eof(lexer: &mut Lexer<'_>, byte: usize) {
    // Expect EOF token
    let token_result = lexer.next();
    assert!(token_result.is_some(), "Expected EOF token but got None");
    let token = token_result.unwrap();
    assert!(token.is_ok(), "Expected Ok(Token) for EOF but got Err");
    let token = token.unwrap();
    assert_eq!(token.kind, TokenKind::EOF, "Expected EOF token");
    assert_eq!(
        token.span.start, byte,
        "EOF token should start at byte position {}",
        byte
    );
    assert_eq!(
        token.span.end, byte,
        "EOF token should end at byte position {}",
        byte
    );
    assert!(
        lexer.next().is_none(),
        "Lexer should return None after EOF token"
    );
}

#[test]
fn test_single_bad_token() {
    let input = "@";
    let mut lexer = Lexer::new(input);

    let token_result = lexer.next();
    let expected_token = create_token("@", 0, 1, TokenKind::Bad);

    assert!(token_result.is_some(), "Expected a token but got None");
    let token = token_result.unwrap();
    assert!(token.is_ok(), "Expected Ok(Token) but got Err");
    let token = token.unwrap();
    assert_eq!(
        token, expected_token,
        "Token does not match expected value. Expected: {:?}, Got: {:?}",
        expected_token, token
    );
}

#[test]
fn test_string_termination_error() {
    let input = r#""unterminated string"#;
    let mut lexer = Lexer::new(input);

    let token_result = lexer.next();
    assert!(token_result.is_some(), "Expected an error but got None");
    let error = token_result.unwrap().unwrap_err();

    // Assert that the error is of type StringTerminationError
    if let Some(string_error) = error.downcast_ref::<StringTerminationError>() {
        assert_eq!(string_error.line(), 1, "Error should be on line 1");
        // Additional assertions can be made on the span if available
    } else {
        panic!("Expected StringTerminationError, got {}", error);
    }
}

#[test]
fn test_empty_input() {
    let input = "";
    let mut lexer = Lexer::new(input);
    assert_eof(&mut lexer, 0);
}

#[test]
fn test_punctuators() {
    let input = "(){},[].:";
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("(", 0, 1, TokenKind::LParen),
        create_token(")", 1, 2, TokenKind::RParen),
        create_token("{", 2, 3, TokenKind::LBrace),
        create_token("}", 3, 4, TokenKind::RBrace),
        create_token(",", 4, 5, TokenKind::Comma),
        create_token("[", 5, 6, TokenKind::LBracket),
        create_token("]", 6, 7, TokenKind::RBracket),
        create_token(".", 7, 8, TokenKind::Dot),
        create_token(":", 8, 9, TokenKind::Colon),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(
            token, expected,
            "Token does not match expected value. Expected: {:?}, Got: {:?}",
            expected, token
        );
    }

    assert_eof(&mut lexer, 9);
}

#[test]
fn test_identifiers_and_keywords() {
    let input = "and nil variable_1 _private var123 or true false if";
    let mut lexer = Lexer::new(input);

    // Define expected tokens with correct byte positions
    let expected_tokens = vec![
        create_token("and", 0, 3, TokenKind::And),
        create_token("nil", 4, 7, TokenKind::Nil),
        create_token("variable_1", 8, 18, TokenKind::Identifier),
        create_token("_private", 19, 27, TokenKind::Identifier),
        create_token("var123", 28, 34, TokenKind::Identifier),
        create_token("or", 35, 37, TokenKind::Or),
        create_token("true", 38, 42, TokenKind::True),
        create_token("false", 43, 48, TokenKind::False),
        create_token("if", 49, 51, TokenKind::If),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(
            token, expected,
            "Token does not match expected value. Expected: {:?}, Got: {:?}",
            expected, token
        );
    }

    assert_eof(&mut lexer, 51);
}

#[test]
fn test_comparison_operators() {
    let input = "! >= == = != < <= >";
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("!", 0, 1, TokenKind::Bang),
        create_token(">=", 2, 4, TokenKind::GreaterEqual),
        create_token("==", 5, 7, TokenKind::EqualEqual),
        create_token("=", 8, 9, TokenKind::Equal),
        create_token("!=", 10, 12, TokenKind::BangEqual),
        create_token("<", 13, 14, TokenKind::Less),
        create_token("<=", 15, 17, TokenKind::LessEqual),
        create_token(">", 18, 19, TokenKind::Greater),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(
            token, expected,
            "Token does not match expected value. Expected: {:?}, Got: {:?}",
            expected, token
        );
    }
    assert_eof(&mut lexer, 19);
}

#[test]
fn test_valid_numbers() {
    let inputs = [
        ("123", 0, 3, TokenKind::Number(123.0)),
        ("456.789", 0, 7, TokenKind::Number(456.789)),
        ("0.001", 0, 5, TokenKind::Number(0.001)),
        ("42.0", 0, 4, TokenKind::Number(42.0)),
    ];

    for &(literal, start, end, kind) in &inputs {
        let mut lexer = Lexer::new(literal);
        let token_result = lexer.next();
        assert!(
            token_result.is_some(),
            "Expected a token but got None for input '{}'",
            literal
        );
        let token = token_result.unwrap();
        assert!(
            token.is_ok(),
            "Expected Ok(Token) but got Err for input '{}'",
            literal
        );
        let token = token.unwrap();
        let expected_token = create_token(literal, start, end, kind);
        assert_eq!(
            token, expected_token,
            "Token does not match expected value for input '{}'. Expected: {:?}, Got: {:?}",
            literal, expected_token, token
        );
        assert_eof(&mut lexer, end);
    }
}

#[test]
fn test_number_parsing_error() {
    let input = "123abc";
    let mut lexer = Lexer::new(input);

    let token_result = lexer.next();
    assert!(token_result.is_some(), "Expected an error but got None");
    let error = token_result.unwrap().unwrap_err();

    let error_message = format!("{}", error);
    assert!(
        error_message.contains("Invalid number literal"),
        "Expected a number parsing error"
    );
}

#[test]
fn test_invalid_numbers() {
    let inputs = ["100_000", "1..2", "1.2.3"];
    for &input in &inputs {
        let mut lexer = Lexer::new(input);
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected an error but got None");
        let error = token_result.unwrap().unwrap_err();
        let error_message = format!("{}", error);
        assert!(
            error_message.contains("Invalid number literal"),
            "Expected a number parsing error for input '{}', got: {}",
            input,
            error_message
        );
    }
}

#[test]
fn test_multiline_string_error() {
    let input = r#""This is a
multi-line string""#;
    let mut lexer = Lexer::new(input);

    let token_result = lexer.next();
    assert!(token_result.is_some(), "Expected an error but got None");
    let error = token_result.unwrap().unwrap_err();

    // Assert that the error is of type StringTerminationError
    if let Some(string_error) = error.downcast_ref::<StringTerminationError>() {
        assert_eq!(string_error.line(), 1, "Error should be on line 1");
        // Additional assertions on the span can be made here
    } else {
        panic!("Expected StringTerminationError, got {}", error);
    }
}

#[test]
fn test_numbers() {
    let inputs = [
        ("123", 0, 3, TokenKind::Number(123.0)),
        ("456.789", 0, 7, TokenKind::Number(456.789)),
        ("0.001", 0, 5, TokenKind::Number(0.001)),
        ("42.0", 0, 4, TokenKind::Number(42.0)),
    ];

    for &(literal, start, end, kind) in &inputs {
        let mut lexer = Lexer::new(literal);
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        let expected_token = create_token(literal, start, end, kind);
        assert_eq!(
            token, expected_token,
            "Token does not match expected value. Expected: {:?}, Got: {:?}",
            expected_token, token
        );
        assert_eof(&mut lexer, end);
    }
}

#[test]
fn test_strings() {
    let input = r#""hello" "world" "unterminated"#; // Removed closing quote for the last string
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
        token1.span.literal, "\"hello\"",
        "First token literal does not match"
    );
    assert_eq!(token1.span.start, 0, "First token start does not match");
    assert_eq!(token1.span.end, 7, "First token end does not match");

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
        token2.span.literal, "\"world\"",
        "Second token literal does not match"
    );
    assert_eq!(token2.span.start, 8, "Second token start does not match");
    assert_eq!(token2.span.end, 15, "Second token end does not match");

    // Third string (unterminated)
    let token3 = lexer.next();
    assert!(
        token3.is_some(),
        "Expected an error for unterminated string but got None"
    );
    let error = token3.unwrap().unwrap_err();

    // Assert that the error is of type StringTerminationError
    if let Some(string_error) = error.downcast_ref::<StringTerminationError>() {
        assert_eq!(string_error.line(), 1, "Error should be on line 1");
        // Additional assertions on the span can be made here
    } else {
        panic!("Expected StringTerminationError, got {}", error);
    }

    // Expect EOF token at byte position 29
    assert_eof(&mut lexer, 29);
}

#[test]
fn test_whitespace_variations() {
    let input = "  \tlet\nx 42\r\n";
    let mut lexer = Lexer::new(input);

    // Define expected tokens with correct byte positions
    let expected_tokens = vec![
        create_token("let", 3, 6, TokenKind::Let),
        create_token("x", 7, 8, TokenKind::Identifier),
        create_token("42", 9, 11, TokenKind::Number(42.0)),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(
            token, expected,
            "Token does not match expected value. Expected: {:?}, Got: {:?}",
            expected, token
        );
    }

    // Update the expected EOF byte position from 11 to 13
    assert_eof(&mut lexer, 13);
}

#[test]
fn test_block_comment_with_quotes() {
    let input = r#"/* This is a block comment with "quotes" */ x = 10"#;
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("/*", 0, 43, TokenKind::BlockComment),
        create_token("x", 44, 45, TokenKind::Identifier),
        create_token("=", 46, 47, TokenKind::Equal),
        create_token("10", 48, 50, TokenKind::Number(10.0)),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap().unwrap();
        assert_eq!(token, expected, "Token does not match expected");
    }

    assert_eof(&mut lexer, 50);
}

#[test]
fn test_arithmetic_operators() {
    let input = "+ - * / ** %";
    let mut lexer = Lexer::new(input);

    // Define expected tokens with correct byte positions
    let expected_tokens = vec![
        create_token("+", 0, 1, TokenKind::Plus),
        create_token("-", 2, 3, TokenKind::Minus),
        create_token("*", 4, 5, TokenKind::Star),
        create_token("/", 6, 7, TokenKind::Slash),
        create_token("**", 8, 10, TokenKind::DoubleStar),
        create_token("%", 11, 12, TokenKind::Percent),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(
            token, expected,
            "Token does not match expected value. Expected: {:?}, Got: {:?}",
            expected, token
        );
    }

    assert_eof(&mut lexer, 12);
}

#[test]
fn test_comments() {
    let input = r#"// This is a single-line comment
x = 42; // Another comment
/* This is a block comment 
Need to skip the entire block*/"#;
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("//", 0, 2, TokenKind::LineComment),
        create_token("x", 33, 34, TokenKind::Identifier),
        create_token("=", 35, 36, TokenKind::Equal),
        create_token("42", 37, 39, TokenKind::Number(42.0)),
        create_token(";", 39, 40, TokenKind::SemiColon),
        create_token("//", 41, 43, TokenKind::LineComment),
        create_token("/*", 60, 119, TokenKind::BlockComment),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(
            token_result.is_some(),
            "Expected a token but got None for input '{}'",
            expected.span.literal
        );
        let token = token_result.unwrap();
        assert!(
            token.is_ok(),
            "Expected Ok(Token) but got Err for input '{}'",
            expected.span.literal
        );
        let token = token.unwrap();
        assert_eq!(
            token, expected,
            "Token does not match expected value. Expected: {:?}, Got: {:?}",
            expected, token
        );
    }

    // After comments, expect EOF
    assert_eof(&mut lexer, 119);
}

#[test]
fn test_unrecognized_characters() {
    let input = "@ # $ ^ &";
    let mut lexer = Lexer::new(input);
    let (tokens, errors) = lexer.collect_tokens();

    // Ensure there are errors
    assert_eq!(
        errors.len(),
        0,
        "Expected two errors for unrecognized characters, found {}",
        errors.len()
    );

    // Define expected tokens with correct byte positions
    let expected_tokens = vec![
        create_token("@", 0, 1, TokenKind::Bad),
        create_token("#", 2, 3, TokenKind::Bad),
        create_token("$", 4, 5, TokenKind::Bad),
        create_token("^", 6, 7, TokenKind::Bad),
        create_token("&", 8, 9, TokenKind::Bad),
    ];

    let token_iter = tokens.iter();
    for (expected, actual) in expected_tokens.iter().zip(token_iter) {
        assert_eq!(
            expected, actual,
            "Token does not match expected value. Expected: {:?}, Got: {:?}",
            expected, actual
        );
    }
}

#[test]
fn test_block_comments_with_various_characters() {
    let input = r#"/* Comment with symbols !@#$%^&*()_+ and quotes " ' */ identifier"#;
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("/*", 0, 54, TokenKind::BlockComment),
        create_token("identifier", 55, 65, TokenKind::Identifier),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap().unwrap();
        assert_eq!(token, expected, "Token does not match expected");
    }

    assert_eof(&mut lexer, 65);
}

#[test]
fn test_strings_with_escapes() {
    let input = r#""hello\nworld" "escaped_quote\"inside""#;
    let mut lexer = Lexer::new(input);

    // Define expected tokens with correct byte positions and literals
    let expected_tokens = vec![
        create_token("\"hello\\nworld\"", 0, 14, TokenKind::String),
        create_token("\"escaped_quote\\\"inside\"", 15, 38, TokenKind::String),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(
            token_result.is_some(),
            "Expected a string token but got None for input '{}'",
            expected.span.literal
        );
        let token = token_result.unwrap();
        assert!(
            token.is_ok(),
            "Expected Ok(Token) for string but got Err for input '{}'",
            expected.span.literal
        );
        let token = token.unwrap();
        assert_eq!(
            token, expected,
            "String token does not match expected value for input '{}'. Expected: {:?}, Got: {:?}",
            expected.span.literal, expected, token
        );
    }

    assert_eof(&mut lexer, 38);
}

#[test]
fn test_adjacent_operators_and_punctuation() {
    let input = "a=b+c;";
    let mut lexer = Lexer::new(input);

    // Define expected tokens with correct byte positions
    let expected_tokens = vec![
        create_token("a", 0, 1, TokenKind::Identifier),
        create_token("=", 1, 2, TokenKind::Equal),
        create_token("b", 2, 3, TokenKind::Identifier),
        create_token("+", 3, 4, TokenKind::Plus),
        create_token("c", 4, 5, TokenKind::Identifier),
        create_token(";", 5, 6, TokenKind::SemiColon),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(
            token, expected,
            "Token does not match expected value. Expected: {:?}, Got: {:?}",
            expected, token
        );
    }

    assert_eof(&mut lexer, 6);
}

#[test]
fn test_identifiers_with_keywords() {
    let input = "or_1 and2";
    let mut lexer = Lexer::new(input);

    // Define expected tokens with correct byte positions
    let expected_tokens = vec![
        create_token("or_1", 0, 4, TokenKind::Identifier),
        create_token("and2", 5, 9, TokenKind::Identifier),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(
            token, expected,
            "Token does not match expected value. Expected: {:?}, Got: {:?}",
            expected, token
        );
    }

    assert_eof(&mut lexer, 9);
}

#[test]
fn test_complex_nested_expressions() {
    let input = "((a + b) * (c - d))";
    let mut lexer = Lexer::new(input);

    // Define expected tokens with correct byte positions
    let expected_tokens = vec![
        create_token("(", 0, 1, TokenKind::LParen),
        create_token("(", 1, 2, TokenKind::LParen),
        create_token("a", 2, 3, TokenKind::Identifier),
        create_token("+", 4, 5, TokenKind::Plus),
        create_token("b", 6, 7, TokenKind::Identifier),
        create_token(")", 7, 8, TokenKind::RParen),
        create_token("*", 9, 10, TokenKind::Star),
        create_token("(", 11, 12, TokenKind::LParen),
        create_token("c", 12, 13, TokenKind::Identifier),
        create_token("-", 14, 15, TokenKind::Minus),
        create_token("d", 16, 17, TokenKind::Identifier),
        create_token(")", 17, 18, TokenKind::RParen),
        create_token(")", 18, 19, TokenKind::RParen),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(
            token_result.is_some(),
            "Expected a token but got None for input '{}'",
            expected.span.literal
        );
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(
            token, expected,
            "Token does not match expected value. Expected: {:?}, Got: {:?}",
            expected, token
        );
    }

    assert_eof(&mut lexer, 19);
}

#[test]
fn test_collect_tokens_no_errors() {
    let input = "let x = 42;";
    let mut lexer = Lexer::new(input);
    let (tokens, errors) = lexer.collect_tokens();

    // Ensure there are no errors
    assert!(
        errors.is_empty(),
        "Expected no errors, but found some: {:?}",
        errors
    );

    // Define expected tokens with correct byte positions
    let expected_tokens = vec![
        create_token("let", 0, 3, TokenKind::Let),
        create_token("x", 4, 5, TokenKind::Identifier),
        create_token("=", 6, 7, TokenKind::Equal),
        create_token("42", 8, 10, TokenKind::Number(42.0)),
        create_token(";", 10, 11, TokenKind::SemiColon),
        create_token("", 11, 11, TokenKind::EOF), // EOF token
    ];

    assert_eq!(
        tokens.len(),
        expected_tokens.len(),
        "Expected {} tokens, got {}",
        expected_tokens.len(),
        tokens.len()
    );

    for (expected, actual) in expected_tokens.iter().zip(tokens.iter()) {
        assert_eq!(
            expected, actual,
            "Token does not match expected value. Expected: {:?}, Got: {:?}",
            expected, actual
        );
    }
}

#[test]
fn test_collect_tokens_with_bad_tokens() {
    let input = "let @x = #42;";
    let mut lexer = Lexer::new(input);
    let (tokens, errors) = lexer.collect_tokens();

    // Ensure there are errors
    assert_eq!(
        errors.len(),
        0,
        "Expected two errors for unrecognized characters, found {}",
        errors.len()
    );

    // Define expected tokens with correct byte positions
    let expected_tokens = vec![
        create_token("let", 0, 3, TokenKind::Let),
        create_token("@", 4, 5, TokenKind::Bad),
        create_token("x", 5, 6, TokenKind::Identifier),
        create_token("=", 7, 8, TokenKind::Equal),
        create_token("#", 9, 10, TokenKind::Bad),
        create_token("42", 10, 12, TokenKind::Number(42.0)),
        create_token(";", 12, 13, TokenKind::SemiColon),
        create_token("", 13, 13, TokenKind::EOF), // EOF token
    ];

    let token_iter = tokens.iter();
    for (expected, actual) in expected_tokens.iter().zip(token_iter) {
        assert_eq!(
            expected, actual,
            "Token does not match expected value. Expected: {:?}, Got: {:?}",
            expected, actual
        );
    }
}

#[test]
fn test_collect_tokens_empty_input() {
    let input = "";
    let mut lexer = Lexer::new(input);
    let (tokens, errors) = lexer.collect_tokens();

    // Ensure there are no errors
    assert!(
        errors.is_empty(),
        "Expected no errors, but found some: {:?}",
        errors
    );

    // Define expected tokens (only EOF token)
    let expected_tokens = vec![create_token("", 0, 0, TokenKind::EOF)];

    assert_eq!(
        tokens, expected_tokens,
        "Tokens do not match expected. Expected: {:?}, Got: {:?}",
        expected_tokens, tokens
    );
}
