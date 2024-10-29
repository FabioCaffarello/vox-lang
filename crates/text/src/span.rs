#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TextSpan<'de> {
    pub start: usize,
    pub end: usize,
    pub literal: &'de str,
}

impl<'de> TextSpan<'de> {
    pub fn new(start: usize, end: usize, literal: &'de str) -> Self {
        Self {
            start,
            end,
            literal,
        }
    }

    pub fn combine(spans: Vec<TextSpan<'de>>) -> TextSpan<'de> {
        // TODO: This is a naive implementation
        todo!()
    }

    pub fn length(&self) -> usize {
        self.end - self.start
    }

    pub fn literal(&self) -> &str {
        self.literal
    }
}