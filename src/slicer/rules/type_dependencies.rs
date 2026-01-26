//! Rule: type_dependencies

use std::collections::HashSet;
use super::Rule;

pub struct TypeDependenciesRule;

impl Rule for TypeDependenciesRule {
    fn name(&self) -> &str {
        "type_dependencies"
    }

    fn apply(&self, _item: &str, _used: &HashSet<String>) -> bool {
        // TODO: Implement rule logic
        false
    }
}
