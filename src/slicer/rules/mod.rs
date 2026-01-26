//! Heuristic rules for determining what to keep

pub mod direct_usage;
pub mod impl_blocks;
pub mod type_dependencies;
pub mod trait_impls;
pub mod generic_bounds;

use std::collections::HashSet;

/// Trait for all slicing rules
pub trait Rule {
    fn name(&self) -> &str;
    fn apply(&self, item: &str, used: &HashSet<String>) -> bool;
}
