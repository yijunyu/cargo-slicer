//! Rule: trait_impls

use std::collections::HashSet;
use super::Rule;

pub struct TraitImplsRule;

impl Rule for TraitImplsRule {
    fn name(&self) -> &str {
        "trait_impls"
    }

    fn apply(&self, _item: &str, _used: &HashSet<String>) -> bool {
        // TODO: Implement rule logic
        false
    }
}
