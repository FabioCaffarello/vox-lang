use std::collections::HashSet;

#[derive(Debug)]
pub struct Loops {
    loop_stack: Vec<Option<String>>,
    active_labels: HashSet<String>,
}

impl Loops {
    pub fn new() -> Self {
        Loops {
            loop_stack: Vec::new(),
            active_labels: HashSet::new(),
        }
    }

    pub fn push(&mut self, label: Option<String>) -> Result<(), String> {
        if let Some(ref lbl) = label {
            if self.active_labels.contains(lbl) {
                return Err(format!("Duplicate loop label '{}'.", lbl));
            }
            self.active_labels.insert(lbl.clone());
        }
        self.loop_stack.push(label);
        Ok(())
    }

    pub fn pop(&mut self) {
        if let Some(Some(lbl)) = self.loop_stack.pop() {
            self.active_labels.remove(&lbl);
        }
    }

    pub fn find_label(&self, label: &str) -> Option<usize> {
        self.loop_stack.iter().rposition(|lbl| {
            lbl.as_ref()
                .is_some_and(|existing_label| existing_label == label)
        })
    }

    pub fn is_inside_loop(&self) -> bool {
        !self.loop_stack.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::Loops;

    #[test]
    fn new_is_outside_loop() {
        let loops = Loops::new();
        assert!(!loops.is_inside_loop());
    }

    #[test]
    fn push_unlabeled_multiple_times() {
        let mut loops = Loops::new();
        assert!(loops.push(None).is_ok());
        assert!(loops.push(None).is_ok());
        assert!(loops.is_inside_loop());
        assert_eq!(loops.find_label("nope"), None);
    }

    #[test]
    fn push_labeled_and_find_indices() {
        let mut loops = Loops::new();
        assert!(loops.push(Some("outer".to_string())).is_ok()); // index 0
        assert!(loops.push(None).is_ok()); // index 1 (unlabeled)
        assert!(loops.push(Some("inner".to_string())).is_ok()); // index 2

        assert_eq!(loops.find_label("outer"), Some(0));
        assert_eq!(loops.find_label("inner"), Some(2));
        assert_eq!(loops.find_label("missing"), None);
    }

    #[test]
    fn duplicate_label_is_rejected() {
        let mut loops = Loops::new();
        assert!(loops.push(Some("A".to_string())).is_ok());
        let err = loops.push(Some("A".to_string())).unwrap_err();
        assert_eq!(err, "Duplicate loop label 'A'.");
        // Different label still ok
        assert!(loops.push(Some("B".to_string())).is_ok());
        assert_eq!(loops.find_label("A"), Some(0));
        assert_eq!(loops.find_label("B"), Some(1));
    }

    #[test]
    fn pop_removes_label_and_allows_reuse() {
        let mut loops = Loops::new();
        assert!(loops.push(Some("X".to_string())).is_ok());
        assert_eq!(loops.find_label("X"), Some(0));
        loops.pop();
        assert_eq!(loops.find_label("X"), None);
        // Reuse after pop should be allowed
        assert!(loops.push(Some("X".to_string())).is_ok());
        assert_eq!(loops.find_label("X"), Some(0));
    }

    #[test]
    fn pop_on_empty_does_not_panic_and_state_ok() {
        let mut loops = Loops::new();
        loops.pop(); // should be a no-op
        assert!(!loops.is_inside_loop());
        // Afterward we can still push
        assert!(loops.push(None).is_ok());
        assert!(loops.is_inside_loop());
    }

    #[test]
    fn nested_labels_and_unlabeled_mixed() {
        let mut loops = Loops::new();
        assert!(loops.push(Some("a".to_string())).is_ok()); // 0
        assert!(loops.push(None).is_ok()); // 1
        assert!(loops.push(Some("b".to_string())).is_ok()); // 2
        assert!(loops.push(None).is_ok()); // 3
        assert!(loops.push(Some("c".to_string())).is_ok()); // 4

        assert_eq!(loops.find_label("a"), Some(0));
        assert_eq!(loops.find_label("b"), Some(2));
        assert_eq!(loops.find_label("c"), Some(4));

        // Pop last two (unlabeled then "c")
        loops.pop(); // removes "c"
        assert_eq!(loops.find_label("c"), None);
        loops.pop(); // unlabeled
        assert_eq!(loops.find_label("b"), Some(2));
        assert!(loops.is_inside_loop());
    }

    #[test]
    fn duplicate_empty_string_label_is_rejected() {
        let mut loops = Loops::new();
        assert!(loops.push(Some("".to_string())).is_ok());
        let err = loops.push(Some("".to_string())).unwrap_err();
        assert_eq!(err, "Duplicate loop label ''.");
    }

    #[test]
    fn is_inside_loop_reflects_stack() {
        let mut loops = Loops::new();
        assert!(!loops.is_inside_loop());
        assert!(loops.push(None).is_ok());
        assert!(loops.is_inside_loop());
        assert!(loops.push(Some("L".to_string())).is_ok());
        assert!(loops.is_inside_loop());
        loops.pop();
        assert!(loops.is_inside_loop()); // one still active
        loops.pop();
        assert!(!loops.is_inside_loop()); // now empty
    }
}
