//! Rule: direct_usage

use std::collections::HashSet;
use super::Rule;

pub struct DirectUsageRule;

impl Rule for DirectUsageRule {
    fn name(&self) -> &str {
        "direct_usage"
    }

    fn apply(&self, _item: &str, _used: &HashSet<String>) -> bool {
        // TODO: Implement rule logic
        false
    }
}
