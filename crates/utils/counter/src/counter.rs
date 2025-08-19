use std::cell::Cell;

#[derive(Debug, Clone)]
pub struct Counter {
    value: Cell<usize>,
}

impl Default for Counter {
    fn default() -> Self {
        Self::new()
    }
}

impl Counter {
    pub fn new() -> Self {
        Self {
            value: Cell::new(0),
        }
    }

    pub fn increment(&self) {
        let current_value = self.value.get();
        self.value.set(current_value + 1);
    }

    pub fn get_value(&self) -> usize {
        self.value.get()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_and_default_start_at_zero() {
        let c1 = Counter::new();
        let c2: Counter = Default::default();
        assert_eq!(c1.get_value(), 0);
        assert_eq!(c2.get_value(), 0);
    }

    #[test]
    fn increment_increases_by_one_without_mut() {
        // increment usa &self, então não precisamos de &mut Counter
        let c = Counter::new();
        c.increment();
        assert_eq!(c.get_value(), 1);
    }

    #[test]
    fn multiple_increments_accumulate() {
        let c = Counter::new();
        for _ in 0..5 {
            c.increment();
        }
        assert_eq!(c.get_value(), 5);
    }

    #[test]
    fn clone_is_independent_snapshot_not_shared_cell() {
        let c1 = Counter::new();
        c1.increment(); // c1 = 1

        // Clone faz uma nova Cell com o mesmo valor atual (snapshot),
        // não compartilha estado com c1.
        let c2 = c1.clone();
        assert_eq!(c1.get_value(), 1);
        assert_eq!(c2.get_value(), 1);

        // Alterar c1 não afeta c2, e vice-versa
        c1.increment(); // c1 = 2
        assert_eq!(c1.get_value(), 2);
        assert_eq!(c2.get_value(), 1);

        c2.increment(); // c2 = 2
        assert_eq!(c1.get_value(), 2);
        assert_eq!(c2.get_value(), 2);
    }
}
