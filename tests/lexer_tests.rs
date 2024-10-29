use lexer::errors::{SingleTokenError, StringTerminationError};
use lexer::{Lexer, Token, TokenKind};
use text::span::TextSpan;

// Helper function to create tokens
fn create_token<'de>(literal: &'de str, start: usize, end: usize, kind: TokenKind) -> Token<'de> {
    Token {
        kind,
        span: TextSpan::new(start, end, literal),
    }
}

// Helper function to assert EOF token
fn assert_eof<'de>(lexer: &mut Lexer<'de>, byte: usize) {
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
fn test_single_token_error() {
    let input = "@";
    let mut lexer = Lexer::new(input);

    let token_result = lexer.next();
    assert!(token_result.is_some(), "Expected an error but got None");
    let error = token_result.unwrap().unwrap_err();

    // Assert that the error is of type SingleTokenError
    if let Some(single_token_error) = error.downcast_ref::<SingleTokenError>() {
        assert_eq!(single_token_error.token, '@', "Unexpected token character");
        // Assuming `SingleTokenError` has a method `line()` to get the line number
        assert_eq!(single_token_error.line(), 1, "Error should be on line 1");
    } else {
        panic!("Expected SingleTokenError, got {}", error);
    }
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
        create_token("variable_1", 8, 18, TokenKind::Ident),
        create_token("_private", 19, 27, TokenKind::Ident),
        create_token("var123", 28, 34, TokenKind::Ident),
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

    // Define expected tokens with correct byte positions and literals
    let expected_tokens = vec![
        create_token("\"hello\"", 0, 7, TokenKind::String),
        create_token("\"world\"", 8, 15, TokenKind::String),
        // The third string is unterminated; expect an error
    ];

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
        create_token("let", 3, 6, TokenKind::Ident),
        create_token("x", 7, 8, TokenKind::Ident),
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
        create_token("x", 44, 45, TokenKind::Ident),
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
fn test_operators() {
    let input = "+ - * / %";
    let mut lexer = Lexer::new(input);

    // Define expected tokens with correct byte positions
    let expected_tokens = vec![
        create_token("+", 0, 1, TokenKind::Plus),
        create_token("-", 2, 3, TokenKind::Minus),
        create_token("*", 4, 5, TokenKind::Star),
        create_token("/", 6, 7, TokenKind::Slash),
        create_token("%", 8, 9, TokenKind::Percent),
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
fn test_comments() {
    let input = r#"// This is a single-line comment
x = 42; // Another comment
/* This is a block comment 
Need to skip the entire block*/"#;
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("//", 0, 2, TokenKind::LineComment),
        create_token("x", 33, 34, TokenKind::Ident),
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
    let mut errors_found = 0;

    // Define expected positions for unrecognized characters
    let unrecognized_positions = vec![
        ('@', 0, 1),
        ('#', 2, 3),
        ('$', 4, 5),
        ('^', 6, 7),
        ('&', 8, 9),
    ];

    for &(char, start, end) in &unrecognized_positions {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected an error but got None");
        let error = token_result.unwrap().unwrap_err();
        let error_message = format!("{}", error);

        // Assert that the error is of type SingleTokenError
        if let Some(single_token_error) = error.downcast_ref::<SingleTokenError>() {
            assert_eq!(single_token_error.token, char, "Unexpected token character");
            // Assuming `SingleTokenError` has a method `line()` to get the line number
            assert_eq!(single_token_error.line(), 1, "Error should be on line 1");
            errors_found += 1;
        } else {
            panic!("Expected SingleTokenError, got {}", error);
        }
    }

    assert_eq!(
        errors_found, 5,
        "Expected 5 errors for unrecognized characters, found {}",
        errors_found
    );

    // After errors, expect EOF
    assert_eof(&mut lexer, 9);
}

#[test]
fn test_block_comments_with_various_characters() {
    let input = r#"/* Comment with symbols !@#$%^&*()_+ and quotes " ' */ identifier"#;
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("/*", 0, 54, TokenKind::BlockComment),
        create_token("identifier", 55, 65, TokenKind::Ident),
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
        create_token("a", 0, 1, TokenKind::Ident),
        create_token("=", 1, 2, TokenKind::Equal),
        create_token("b", 2, 3, TokenKind::Ident),
        create_token("+", 3, 4, TokenKind::Plus),
        create_token("c", 4, 5, TokenKind::Ident),
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
        create_token("or_1", 0, 4, TokenKind::Ident),
        create_token("and2", 5, 9, TokenKind::Ident),
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
        create_token("a", 2, 3, TokenKind::Ident),
        create_token("+", 4, 5, TokenKind::Plus),
        create_token("b", 6, 7, TokenKind::Ident),
        create_token(")", 7, 8, TokenKind::RParen),
        create_token("*", 9, 10, TokenKind::Star),
        create_token("(", 11, 12, TokenKind::LParen),
        create_token("c", 12, 13, TokenKind::Ident),
        create_token("-", 14, 15, TokenKind::Minus),
        create_token("d", 16, 17, TokenKind::Ident),
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
        create_token("let", 0, 3, TokenKind::Ident),
        create_token("x", 4, 5, TokenKind::Ident),
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
fn test_collect_tokens_with_errors() {
    let input = "let @x = #42;";
    let mut lexer = Lexer::new(input);
    let (tokens, errors) = lexer.collect_tokens();

    // Ensure there are errors
    assert_eq!(
        errors.len(),
        2,
        "Expected two errors for unrecognized characters, found {}",
        errors.len()
    );

    // Define expected tokens with correct byte positions
    let expected_tokens = vec![
        create_token("let", 0, 3, TokenKind::Ident),
        // '@' is an invalid character, error occurs here
        create_token("x", 5, 6, TokenKind::Ident),
        create_token("=", 7, 8, TokenKind::Equal),
        // '#' is an invalid character, error occurs here
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

    // Check error messages
    assert_eq!(
        errors.len(),
        2,
        "Expected two errors, but found {}",
        errors.len()
    );

    for error in errors {
        let error_message = format!("{}", error);
        assert!(
            error_message.contains("Unexpected token"),
            "Expected an unrecognized token error, got: {}",
            error_message
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

#[test]
fn test_collect_tokens_multiple_errors() {
    let input = "let @x = #42;";
    let mut lexer = Lexer::new(input);
    let (tokens, errors) = lexer.collect_tokens();

    // Ensure there are two errors
    assert_eq!(
        errors.len(),
        2,
        "Expected two errors, but found {}",
        errors.len()
    );

    // Define expected tokens with correct byte positions
    let expected_tokens = vec![
        create_token("let", 0, 3, TokenKind::Ident),
        create_token("x", 5, 6, TokenKind::Ident),
        create_token("=", 7, 8, TokenKind::Equal),
        create_token("42", 10, 12, TokenKind::Number(42.0)),
        create_token(";", 12, 13, TokenKind::SemiColon),
        create_token("", 13, 13, TokenKind::EOF),
    ];

    let token_iter = tokens.iter();
    for (expected, actual) in expected_tokens.iter().zip(token_iter) {
        assert_eq!(
            expected, actual,
            "Token does not match expected value. Expected: {:?}, Got: {:?}",
            expected, actual
        );
    }

    // Check error messages
    for error in errors {
        let error_message = format!("{}", error);
        assert!(
            error_message.contains("Unexpected token"),
            "Expected an unrecognized token error, got: {}",
            error_message
        );
    }
}

#[test]
fn test_token_display() {
    let test_cases = vec![
        // Punctuators and Delimiters
        (TokenKind::LParen, "(", 0, 1, "LEFT_PAREN ( null"),
        (TokenKind::RParen, ")", 1, 2, "RIGHT_PAREN ) null"),
        (TokenKind::LBrace, "{", 2, 3, "LEFT_BRACE { null"),
        (TokenKind::RBrace, "}", 3, 4, "RIGHT_BRACE } null"),
        (TokenKind::LBracket, "[", 4, 5, "LEFT_BRACKET [ null"),
        (TokenKind::RBracket, "]", 5, 6, "RIGHT_BRACKET ] null"),
        (TokenKind::Comma, ",", 6, 7, "COMMA , null"),
        (TokenKind::Colon, ":", 7, 8, "COLON : null"),
        (TokenKind::SemiColon, ";", 8, 9, "SEMICOLON ; null"),
        (TokenKind::Dot, ".", 9, 10, "DOT . null"),
        // Keywords
        (TokenKind::If, "if", 10, 12, "IF if null"),
        (TokenKind::Else, "else", 13, 17, "ELSE else null"),
        (TokenKind::While, "while", 18, 23, "WHILE while null"),
        (TokenKind::For, "for", 24, 27, "FOR for null"),
        (TokenKind::Return, "return", 28, 34, "RETURN return null"),
        (TokenKind::Fun, "fun", 35, 38, "FUN fun null"),
        (TokenKind::True, "true", 39, 43, "TRUE true null"),
        (TokenKind::False, "false", 44, 49, "FALSE false null"),
        (TokenKind::Nil, "nil", 50, 53, "NIL nil null"),
        (TokenKind::And, "and", 54, 57, "AND and null"),
        (TokenKind::Or, "or", 58, 60, "OR or null"),
        (TokenKind::Not, "not", 61, 64, "NOT not null"),
        (TokenKind::In, "in", 65, 67, "IN in null"),
        // Text Processing Keywords
        (TokenKind::Split, "split", 68, 73, "SPLIT split null"),
        (TokenKind::Join, "join", 74, 78, "JOIN join null"),
        (TokenKind::Map, "map", 79, 82, "MAP map null"),
        (TokenKind::Filter, "filter", 83, 89, "FILTER filter null"),
        (TokenKind::Reduce, "reduce", 90, 96, "REDUCE reduce null"),
        (
            TokenKind::Replace,
            "replace",
            97,
            104,
            "REPLACE replace null",
        ),
        (
            TokenKind::Extract,
            "extract",
            105,
            113,
            "EXTRACT extract null",
        ),
        // Strings and Numbers
        (
            TokenKind::String,
            "\"hello\"",
            114,
            121,
            "STRING \"hello\" null",
        ),
        (TokenKind::Number(42.0), "42", 122, 124, "NUMBER 42 42.0"),
        (
            TokenKind::Ident,
            "identifier",
            125,
            135,
            "IDENTIFIER identifier null",
        ),
        // Built-in Functions
        (TokenKind::Length, "length", 136, 142, "LENGTH length null"),
        (TokenKind::TypeOf, "typeOf", 143, 149, "TYPE_OF typeOf null"),
        (TokenKind::Range, "range", 150, 155, "RANGE range null"),
        (
            TokenKind::Concatenate,
            "concat",
            156,
            162,
            "CONCATENATE concat null",
        ),
        // Arithmetic Operators
        (TokenKind::Plus, "+", 163, 164, "PLUS + null"),
        (TokenKind::Minus, "-", 165, 166, "MINUS - null"),
        (TokenKind::Star, "*", 167, 168, "STAR * null"),
        (TokenKind::Slash, "/", 169, 170, "SLASH / null"),
        (TokenKind::Percent, "%", 171, 172, "PERCENT % null"),
        // Comparison operators
        (TokenKind::Equal, "=", 173, 174, "EQUAL = null"),
        (TokenKind::Bang, "!", 175, 176, "BANG ! null"),
        (TokenKind::BangEqual, "!=", 177, 179, "BANG_EQUAL != null"),
        (TokenKind::EqualEqual, "==", 180, 182, "EQUAL_EQUAL == null"),
        (TokenKind::Greater, ">", 183, 184, "GREATER > null"),
        (
            TokenKind::GreaterEqual,
            ">=",
            185,
            187,
            "GREATER_EQUAL >= null",
        ),
        (TokenKind::Less, "<", 188, 189, "LESS < null"),
        (TokenKind::LessEqual, "<=", 190, 192, "LESS_EQUAL <= null"),
        // Comments
        (
            TokenKind::LineComment,
            "//",
            193,
            195,
            "LINE_COMMENT // null",
        ),
        (
            TokenKind::BlockComment,
            "/*",
            196,
            198,
            "BLOCK_COMMENT /* null",
        ),
        // EOF
        (TokenKind::EOF, "", 198, 198, "EOF  null"),
    ];

    for (kind, literal, start, end, expected_display) in test_cases {
        let token = create_token(literal, start, end, kind);
        assert_eq!(
            format!("{}", token),
            expected_display,
            "Token display does not match expected. Expected: '{}', Got: '{}'",
            expected_display,
            format!("{}", token)
        );
    }
}
