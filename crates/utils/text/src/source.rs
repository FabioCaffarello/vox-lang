pub struct SourceText {
    text: String,
}

impl SourceText {
    pub fn new(text: String) -> Self {
        Self { text }
    }

    pub fn line_index(&self, position: usize) -> usize {
        let capped = position.min(self.text.len());
        self.text[..capped].lines().count().saturating_sub(1)
    }

    pub fn get_line(&self, index: usize) -> &str {
        self.text.lines().nth(index).unwrap()
    }

    pub fn line_start(&self, index: usize) -> usize {
        self.text
            .lines()
            .take(index)
            .map(|line| line.len() + 1)
            .sum()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn line_index_basic_multiline() {
        let s = SourceText::new("a\nbb\nccc".to_string());
        assert_eq!(s.line_index(1), 0); // dentro da 1ª linha
        assert_eq!(s.line_index(1), 0); // no limite do '\n' após 'a'
        assert_eq!(s.line_index(3), 1); // dentro da 2ª linha
        assert_eq!(s.line_index(6), 2); // dentro da 3ª linha
    }

    #[test]
    fn line_index_on_newline_boundaries() {
        let input = "ab\ncd\nef";
        let s = SourceText::new(input.to_string());
        assert_eq!(s.line_index(2), 0); // no 1º '\n'
        assert_eq!(s.line_index(5), 1); // no 2º '\n'
        assert_eq!(s.line_index(input.len()), 2); // fim do texto
    }

    #[test]
    fn get_line_valid_indices() {
        let s = SourceText::new("a\nbb\nccc".to_string());
        assert_eq!(s.get_line(0), "a");
        assert_eq!(s.get_line(1), "bb");
        assert_eq!(s.get_line(2), "ccc");
    }

    #[test]
    #[should_panic]
    fn get_line_out_of_bounds_panics() {
        let s = SourceText::new("x\ny".to_string());
        let _ = s.get_line(2); // unwrap() panica
    }

    #[test]
    fn line_start_values_no_trailing_newline() {
        let s = SourceText::new("a\nbb\nccc".to_string());
        // inícios esperados: 0, 2, 5
        assert_eq!(s.line_start(0), 0);
        assert_eq!(s.line_start(1), 2); // "a\n"
        assert_eq!(s.line_start(2), 5); // "a\nbb\n"
    }

    #[test]
    fn line_start_with_trailing_newline() {
        let input = "a\nbb\nccc\n";
        let s = SourceText::new(input.to_string());
        // inícios: 0 ("a"), 2 ("bb"), 5 ("ccc"), 9 (linha vazia após '\n' final)
        assert_eq!(s.line_start(0), 0);
        assert_eq!(s.line_start(1), 2);
        assert_eq!(s.line_start(2), 5);
        assert_eq!(s.line_start(3), 9);
        assert_eq!(input.len(), 9); // comprimento correto do literal
    }

    #[test]
    fn line_index_zero_position_empty_text_is_zero() {
        // comportamento "safe": sem panic e retorna 0
        let s = SourceText::new(String::new());
        assert_eq!(s.line_index(0), 0);
        assert_eq!(s.line_index(100), 0); // posição maior que len é capada
    }

    #[test]
    fn line_index_start_of_first_line_nonempty_text() {
        let s = SourceText::new("hello".to_string());
        assert_eq!(s.line_index(0), 0); // já é seguro com sua versão
        assert_eq!(s.get_line(0), "hello");
        assert_eq!(s.line_start(0), 0);
    }
}
