use lexer::errors::{SingleTokenError, StringTerminationError};
use lexer::{Lexer, Token, TokenKind};

// Helper function to create tokens
fn create_token(origin: &str, offset: usize, kind: TokenKind) -> Token {
    Token {
        origin,
        offset,
        kind,
    }
}

fn assert_eof(lexer: &mut Lexer) {
    // Expect EOF token
    let token_result = lexer.next();
    assert!(token_result.is_some(), "Expected EOF token but got None");
    let token = token_result.unwrap();
    assert!(token.is_ok(), "Expected Ok(Token) for EOF but got Err");
    let token = token.unwrap();
    assert_eq!(token.kind, TokenKind::EOF, "Expected EOF token");
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
        assert_eq!(single_token_error.token, '@');
        assert_eq!(single_token_error.line(), 1);
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
        assert_eq!(string_error.line(), 1);
    } else {
        panic!("Expected StringTerminationError, got {}", error);
    }
}

#[test]
fn test_empty_input() {
    let input = "";
    let mut lexer = Lexer::new(input);
    assert_eof(&mut lexer);
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

    assert_eof(&mut lexer);
}

#[test]
fn test_identifiers_and_keywords() {
    let input = "and nil variable_1 _private var123 or true false if";
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
        create_token("if", 49, TokenKind::If),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap();
        assert!(token.is_ok(), "Expected Ok(Token) but got Err");
        let token = token.unwrap();
        assert_eq!(token, expected, "Token does not match expected value");
    }

    assert_eof(&mut lexer);
}

#[test]
fn test_comparison_operators() {
    let input = "! >= == = != < <= >";
    let mut lexer = Lexer::new(input);

    let expected_tokens = vec![
        create_token("!", 0, TokenKind::Bang),
        create_token(">=", 2, TokenKind::GreaterEqual),
        create_token("==", 5, TokenKind::EqualEqual),
        create_token("=", 8, TokenKind::Equal),
        create_token("!=", 10, TokenKind::BangEqual),
        create_token("<", 13, TokenKind::Less),
        create_token("<=", 15, TokenKind::LessEqual),
        create_token(">", 18, TokenKind::Greater),
    ];

    for expected in expected_tokens {
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap().unwrap();
        assert_eq!(token, expected, "Token does not match expected value");
    }
    assert_eof(&mut lexer);
}

#[test]
fn test_valid_numbers() {
    let inputs = ["123", "456.789", "0.001", "42.0"];
    for input in &inputs {
        let mut lexer = Lexer::new(input);
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected a token but got None");
        let token = token_result.unwrap().unwrap();
        assert_eq!(token.kind, TokenKind::Number(input.parse::<f64>().unwrap()));
        assert_eof(&mut lexer);
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
    for input in &inputs {
        let mut lexer = Lexer::new(input);
        let token_result = lexer.next();
        assert!(token_result.is_some(), "Expected an error but got None");
        let error = token_result.unwrap().unwrap_err();
        let error_message = format!("{}", error);
        assert!(
            error_message.contains("Invalid number literal"),
            "Expected a number parsing error for input '{}'",
            input
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
        assert!(
            string_error.to_string().contains("Unterminated string"),
            "Error message does not contain 'Unterminated string'"
        );
    } else {
        panic!("Expected StringTerminationError, got {}", error);
    }
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

    assert_eof(&mut lexer);
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

    assert_eof(&mut lexer);
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

    assert_eof(&mut lexer);
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

    assert_eof(&mut lexer);
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

    assert_eof(&mut lexer);
}

#[test]
fn test_unrecognized_characters() {
    let input = "@ # $ ^ &";
    let mut lexer = Lexer::new(input);
    let mut errors_found = 0;

    while let Some(result) = lexer.next() {
        match result {
            Err(_) => errors_found += 1,
            Ok(token) => {
                // Expect only EOF token as Ok(Token)
                assert_eq!(token.kind, TokenKind::EOF, "Expected only errors and EOF");
            }
        }
    }

    assert_eq!(
        errors_found, 5,
        "Expected 5 errors for unrecognized characters"
    );
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
        assert_eq!(
            token.kind, expected.kind,
            "Token kind does not match expected kind"
        );
        assert_eq!(
            token.origin, expected.origin,
            "Token origin does not match expected origin"
        );
    }

    assert_eof(&mut lexer);
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

    assert_eof(&mut lexer);
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

    assert_eof(&mut lexer);
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

    assert_eof(&mut lexer);
}

#[test]
fn test_collect_tokens_no_errors() {
    let input = "let x = 42;";
    let mut lexer = Lexer::new(input);
    let (tokens, errors) = lexer.collect_tokens();

    // Ensure there are no errors
    assert!(errors.is_empty(), "Expected no errors, but found some");
    assert_eq!(tokens.len(), 6, "Expected 5 tokens"); // 5 tokens + EOF

    // Expected tokens
    let expected_tokens = vec![
        create_token("let", 0, TokenKind::Ident),
        create_token("x", 4, TokenKind::Ident),
        create_token("=", 6, TokenKind::Equal),
        create_token("42", 8, TokenKind::Number(42.0)),
        create_token(";", 10, TokenKind::SemiColon),
        create_token("", 11, TokenKind::EOF), // EOF token
    ];

    for (expected, actual) in expected_tokens.iter().zip(tokens.iter()) {
        assert_eq!(expected, actual, "Token does not match expected value");
    }
}

#[test]
fn test_collect_tokens_with_errors() {
    let input = "let x = 42$;";
    let mut lexer = Lexer::new(input);
    let (tokens, errors) = lexer.collect_tokens();

    // Ensure there is at least one error
    assert!(!errors.is_empty(), "Expected errors, but found none");

    // Expected tokens (the lexer continues after errors)
    let expected_tokens = vec![
        create_token("let", 0, TokenKind::Ident),
        create_token("x", 4, TokenKind::Ident),
        create_token("=", 6, TokenKind::Equal),
        create_token("42", 8, TokenKind::Number(42.0)),
        // The '$' character is invalid and produces an error
        // The lexer continues and collects the semicolon
        create_token(";", 11, TokenKind::SemiColon),
        create_token("", 12, TokenKind::EOF), // EOF token
    ];

    for (expected, actual) in expected_tokens.iter().zip(tokens.iter()) {
        assert_eq!(expected, actual, "Token does not match expected value");
    }

    // Check that the error is as expected
    assert_eq!(errors.len(), 1, "Expected one error");

    let error_message = format!("{}", errors[0]);
    assert!(
        error_message.contains("Unexpected token"),
        "Expected an unrecognized token error, got: {}",
        error_message
    );
}

#[test]
fn test_collect_tokens_empty_input() {
    let input = "";
    let mut lexer = Lexer::new(input);
    let (tokens, errors) = lexer.collect_tokens();

    // Ensure there are no errors
    assert!(errors.is_empty(), "Expected no errors, but found some");

    // Expected tokens (only EOF token)
    let expected_tokens = vec![create_token("", 0, TokenKind::EOF)];

    assert_eq!(tokens, expected_tokens, "Tokens do not match expected");
}

#[test]
fn test_collect_tokens_multiple_errors() {
    let input = "let @x = #42;";
    let mut lexer = Lexer::new(input);
    let (tokens, errors) = lexer.collect_tokens();

    // Ensure there are errors
    assert_eq!(errors.len(), 2, "Expected two errors");

    // Expected tokens (lexer continues after errors)
    let expected_tokens = vec![
        create_token("let", 0, TokenKind::Ident),
        // '@' is an invalid character, error occurs here
        create_token("x", 5, TokenKind::Ident),
        create_token("=", 7, TokenKind::Equal),
        // '#' is an invalid character, error occurs here
        create_token("42", 10, TokenKind::Number(42.0)),
        create_token(";", 12, TokenKind::SemiColon),
        create_token("", 13, TokenKind::EOF),
    ];

    for (expected, actual) in expected_tokens.iter().zip(tokens.iter()) {
        assert_eq!(expected, actual, "Token does not match expected value");
    }

    // Check error messages
    let error_messages: Vec<String> = errors.iter().map(|e| format!("{}", e)).collect();
    for message in &error_messages {
        assert!(
            message.contains("Unexpected token"),
            "Expected an unrecognized token error, got: {}",
            message
        );
    }
}

#[test]
fn test_token_display() {
    let test_cases = vec![
        // Punctuators and Delimiters
        (TokenKind::LParen, "(", "LEFT_PAREN ( null"),
        (TokenKind::RParen, ")", "RIGHT_PAREN ) null"),
        (TokenKind::LBrace, "{", "LEFT_BRACE { null"),
        (TokenKind::RBrace, "}", "RIGHT_BRACE } null"),
        (TokenKind::LBracket, "[", "LEFT_BRACKET [ null"),
        (TokenKind::RBracket, "]", "RIGHT_BRACKET ] null"),
        (TokenKind::Comma, ",", "COMMA , null"),
        (TokenKind::Colon, ":", "COLON : null"),
        (TokenKind::SemiColon, ";", "SEMICOLON ; null"),
        (TokenKind::Dot, ".", "DOT . null"),
        // Keywords
        (TokenKind::If, "if", "IF if null"),
        (TokenKind::Else, "else", "ELSE else null"),
        (TokenKind::While, "while", "WHILE while null"),
        (TokenKind::For, "for", "FOR for null"),
        (TokenKind::Return, "return", "RETURN return null"),
        (TokenKind::Fun, "fun", "FUN fun null"),
        (TokenKind::True, "true", "TRUE true null"),
        (TokenKind::False, "false", "FALSE false null"),
        (TokenKind::Nil, "nil", "NIL nil null"),
        (TokenKind::And, "and", "AND and null"),
        (TokenKind::Or, "or", "OR or null"),
        (TokenKind::Not, "not", "NOT not null"),
        (TokenKind::In, "in", "IN in null"),
        // Text Processing Keywords
        (TokenKind::Split, "split", "SPLIT split null"),
        (TokenKind::Join, "join", "JOIN join null"),
        (TokenKind::Map, "map", "MAP map null"),
        (TokenKind::Filter, "filter", "FILTER filter null"),
        (TokenKind::Reduce, "reduce", "REDUCE reduce null"),
        (TokenKind::Replace, "replace", "REPLACE replace null"),
        (TokenKind::Extract, "extract", "EXTRACT extract null"),
        // Strings and Numbers
        (TokenKind::String, "\"hello\"", "STRING \"hello\" null"),
        (TokenKind::Number(42.0), "42", "NUMBER 42 42.0"),
        (TokenKind::Ident, "identifier", "IDENTIFIER identifier null"),
        // Built-in Functions
        (TokenKind::Length, "length", "LENGTH length null"),
        (TokenKind::TypeOf, "typeOf", "TYPE_OF typeOf null"),
        (TokenKind::Range, "range", "RANGE range null"),
        (TokenKind::Concatenate, "concat", "CONCATENATE concat null"),
        // Arithmetic Operators
        (TokenKind::Plus, "+", "PLUS + null"),
        (TokenKind::Minus, "-", "MINUS - null"),
        (TokenKind::Star, "*", "STAR * null"),
        (TokenKind::Slash, "/", "SLASH / null"),
        (TokenKind::Percent, "%", "PERCENT % null"),
        // Comparison operators
        (TokenKind::Equal, "=", "EQUAL = null"),
        (TokenKind::Bang, "!", "BANG ! null"),
        (TokenKind::BangEqual, "!=", "BANG_EQUAL != null"),
        (TokenKind::EqualEqual, "==", "EQUAL_EQUAL == null"),
        (TokenKind::Greater, ">", "GREATER > null"),
        (TokenKind::GreaterEqual, ">=", "GREATER_EQUAL >= null"),
        (TokenKind::Less, "<", "LESS < null"),
        (TokenKind::LessEqual, "<=", "LESS_EQUAL <= null"),
        // Comments
        (TokenKind::LineComment, "//", "LINE_COMMENT // null"),
        (TokenKind::BlockComment, "/*", "BLOCK_COMMENT /* null"),
        // EOF
        (TokenKind::EOF, "", "EOF  null"),
    ];

    for (kind, origin, expected_display) in test_cases {
        let token = create_token(origin, 0, kind);
        assert_eq!(format!("{}", token), expected_display);
    }
}
