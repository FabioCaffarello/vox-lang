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

    pub fn combine(mut spans: Vec<TextSpan<'de>>) -> TextSpan<'de> {
        spans.sort_by(|a, b| a.start.cmp(&b.start));

        let start = spans.first().unwrap().start;
        let end = spans.last().unwrap().end;

        let mut literal = String::new();
        for (index, span) in spans.iter().enumerate() {
            if index > 0 {
                let last = spans.get(index - 1).unwrap();
                let diff = span.start - last.end;
                for _ in 0..diff {
                    literal.push_str(&" ".repeat(diff));
                }
            }
            literal.push_str(span.literal);
        }
        TextSpan::from_owned_string(start, end, literal)
    }

    pub fn length(&self) -> usize {
        self.end - self.start
    }

    pub fn literal(&self) -> &str {
        self.literal
    }

    pub fn from_owned_string(start: usize, end: usize, literal: String) -> Self {
        let leaked_literal = Box::leak(literal.into_boxed_str());
        Self {
            start,
            end,
            literal: leaked_literal,
        }
    }
}
