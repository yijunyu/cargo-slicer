//! Rule: impl_blocks

use std::collections::HashSet;
use super::Rule;

pub struct ImplBlocksRule;

impl Rule for ImplBlocksRule {
    fn name(&self) -> &str {
        "impl_blocks"
    }

    fn apply(&self, _item: &str, _used: &HashSet<String>) -> bool {
        // TODO: Implement rule logic
        false
    }
}
