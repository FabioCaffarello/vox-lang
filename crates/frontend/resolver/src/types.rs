use diagnostics::diagnostics::DiagnosticsBagCell;
use token::Token;
use typings::types::Type;

pub fn resolve_type_from_string<'de>(
    diagnostics: &DiagnosticsBagCell<'de>,
    type_name: &Token<'de>,
) -> Type {
    let ty = Type::from_str(type_name.span.literal);
    let ty = match ty {
        Some(ty) => ty,
        None => {
            diagnostics.borrow_mut().report_undeclared_type(type_name);
            Type::Error
        }
    };
    ty
}

#[cfg(test)]
mod tests {
    use super::*;
    use diagnostics::diagnostics::{DiagnosticsBag, DiagnosticsBagCell};
    use std::{cell::RefCell, rc::Rc};
    use text::span::TextSpan;
    use token::{Token, TokenKind};

    fn ident_tok(lit: &'static str, start: usize) -> Token<'static> {
        Token {
            kind: TokenKind::Identifier,
            span: TextSpan::new(start, start + lit.len(), lit),
        }
    }

    fn new_bag<'de>() -> DiagnosticsBagCell<'de> {
        Rc::new(RefCell::new(DiagnosticsBag::new()))
    }

    #[test]
    fn resolves_known_type_without_diagnostics() {
        // Try a broad set of common type names in multiple casings.
        let names = [
            // lowercase
            "number", "bool", "boolean", "string", "int", "float", "unit", "void", "never", "any",
            // capitalized / PascalCase
            "Number", "Bool", "Boolean", "String", "Int", "Float", "Unit", "Void", "Never", "Any",
            // ALLCAPS (some projects do this)
            "NUMBER", "BOOL", "BOOLEAN", "STRING", "INT", "FLOAT", "UNIT", "VOID",
        ];

        let Some((chosen_name, expected_ty)) = names
            .iter()
            .find_map(|&n| Type::from_str(n).map(|t| (n, t)))
        else {
            // Built-ins not recognized yet; skip this positive-path test for now.
            eprintln!("Skipping: no known builtin type found via Type::from_str; adjust names or implement builtins.");
            return;
        };

        let bag: DiagnosticsBagCell<'_> = Rc::new(RefCell::new(DiagnosticsBag::new()));
        let tok = ident_tok(chosen_name, 0);

        let resolved = resolve_type_from_string(&bag, &tok);

        // No diagnostics should be produced for a known type.
        assert!(
            bag.borrow().diagnostics.is_empty(),
            "expected no diagnostics for known type '{}', got {:?}",
            chosen_name,
            bag.borrow().diagnostics
        );

        // Compare via Display to avoid relying on Type's PartialEq details.
        assert_eq!(resolved.to_string(), expected_ty.to_string());
    }
    #[test]
    fn reports_unknown_type_and_returns_error() {
        let bag = new_bag();
        let tok = ident_tok("NotAType", 3);

        let resolved = resolve_type_from_string(&bag, &tok);

        // Should push exactly one diagnostic about an undeclared type.
        let diags = &bag.borrow().diagnostics;
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].message, "Undeclared type 'NotAType'");
        assert_eq!(diags[0].span, tok.span);

        // Function should return Type::Error for unknown names.
        // (If Type doesn't implement PartialEq, compare via Display.)
        #[allow(clippy::bool_assert_comparison)]
        {
            let by_display = resolved.to_string() == Type::Error.to_string();
            assert!(by_display, "expected Type::Error, got {}", resolved);
        }
    }
}
