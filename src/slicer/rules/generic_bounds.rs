//! Rule: generic_bounds

use std::collections::HashSet;
use super::Rule;

pub struct GenericBoundsRule;

impl Rule for GenericBoundsRule {
    fn name(&self) -> &str {
        "generic_bounds"
    }

    fn apply(&self, _item: &str, _used: &HashSet<String>) -> bool {
        // TODO: Implement rule logic
        false
    }
}
