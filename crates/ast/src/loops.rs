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
                .map_or(false, |existing_label| existing_label == label)
        })
    }

    pub fn is_inside_loop(&self) -> bool {
        !self.loop_stack.is_empty()
    }
}
