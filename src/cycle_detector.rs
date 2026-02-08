use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EdgeType {
    Normal,      // [dependencies]
    Dev,         // [dev-dependencies]
    Build,       // [build-dependencies]
}

#[derive(Debug, Clone)]
pub struct DependencyEdge {
    pub from: String,
    pub to: String,
    pub edge_type: EdgeType,
}

#[derive(Debug, Clone)]
pub struct Cycle {
    pub crates: Vec<String>,
    pub edges: Vec<DependencyEdge>,
}

pub struct CycleDetector {
    graph: HashMap<String, Vec<DependencyEdge>>,
    index: HashMap<String, usize>,
    lowlink: HashMap<String, usize>,
    on_stack: HashSet<String>,
    stack: Vec<String>,
    sccs: Vec<Vec<String>>,
    counter: usize,
}

impl CycleDetector {
    pub fn new() -> Self {
        CycleDetector {
            graph: HashMap::new(),
            index: HashMap::new(),
            lowlink: HashMap::new(),
            on_stack: HashSet::new(),
            stack: Vec::new(),
            sccs: Vec::new(),
            counter: 0,
        }
    }

    /// Add an edge to the dependency graph
    pub fn add_dependency(&mut self, from: &str, to: &str, edge_type: EdgeType) {
        let edge = DependencyEdge {
            from: from.to_string(),
            to: to.to_string(),
            edge_type,
        };

        self.graph.entry(from.to_string())
            .or_insert_with(Vec::new)
            .push(edge);

        // Ensure 'to' node exists in graph even if it has no outgoing edges
        self.graph.entry(to.to_string())
            .or_insert_with(Vec::new);
    }

    /// Run Tarjan's algorithm to find all SCCs (cycles)
    pub fn detect_cycles(&mut self) -> Vec<Cycle> {
        self.sccs.clear();
        self.index.clear();
        self.lowlink.clear();
        self.on_stack.clear();
        self.stack.clear();
        self.counter = 0;

        // Run Tarjan's algorithm on all nodes
        let nodes: Vec<String> = self.graph.keys().cloned().collect();
        for node in nodes {
            if !self.index.contains_key(&node) {
                self.strongconnect(&node);
            }
        }

        // Filter SCCs to only include actual cycles (size > 1)
        let mut cycles = Vec::new();
        for scc in &self.sccs {
            if scc.len() > 1 {
                // Extract edges involved in this cycle
                let edges = self.extract_edges_in_cycle(scc);
                cycles.push(Cycle {
                    crates: scc.clone(),
                    edges,
                });
            } else if scc.len() == 1 {
                // Check for self-loop
                let node = &scc[0];
                if let Some(edges) = self.graph.get(node) {
                    for edge in edges {
                        if &edge.to == node {
                            // Self-loop detected
                            cycles.push(Cycle {
                                crates: scc.clone(),
                                edges: vec![edge.clone()],
                            });
                            break;
                        }
                    }
                }
            }
        }

        cycles
    }

    /// Core Tarjan's algorithm implementation
    fn strongconnect(&mut self, v: &str) {
        // Set the depth index for v
        self.index.insert(v.to_string(), self.counter);
        self.lowlink.insert(v.to_string(), self.counter);
        self.counter += 1;
        self.stack.push(v.to_string());
        self.on_stack.insert(v.to_string());

        // Consider successors of v
        if let Some(edges) = self.graph.get(v).cloned() {
            for edge in edges {
                let w = &edge.to;
                if !self.index.contains_key(w) {
                    // Successor w has not yet been visited; recurse on it
                    self.strongconnect(w);
                    let w_lowlink = *self.lowlink.get(w).unwrap();
                    let v_lowlink = *self.lowlink.get(v).unwrap();
                    self.lowlink.insert(v.to_string(), v_lowlink.min(w_lowlink));
                } else if self.on_stack.contains(w) {
                    // Successor w is in stack S and hence in the current SCC
                    let w_index = *self.index.get(w).unwrap();
                    let v_lowlink = *self.lowlink.get(v).unwrap();
                    self.lowlink.insert(v.to_string(), v_lowlink.min(w_index));
                }
            }
        }

        // If v is a root node, pop the stack and generate an SCC
        let v_lowlink = *self.lowlink.get(v).unwrap();
        let v_index = *self.index.get(v).unwrap();
        if v_lowlink == v_index {
            let mut scc = Vec::new();
            loop {
                let w = self.stack.pop().unwrap();
                self.on_stack.remove(&w);
                scc.push(w.clone());
                if w == v {
                    break;
                }
            }
            self.sccs.push(scc);
        }
    }

    /// Extract edges involved in a specific cycle
    fn extract_edges_in_cycle(&self, crates: &[String]) -> Vec<DependencyEdge> {
        let crate_set: HashSet<_> = crates.iter().collect();
        let mut edges = Vec::new();

        for crate_name in crates {
            if let Some(crate_edges) = self.graph.get(crate_name) {
                for edge in crate_edges {
                    // Include edge if both endpoints are in the cycle
                    if crate_set.contains(&edge.to) {
                        edges.push(edge.clone());
                    }
                }
            }
        }

        edges
    }

    /// Check if two crates form a cycle (after running detect_cycles)
    pub fn is_cyclic(&self, crate_a: &str, crate_b: &str) -> bool {
        for scc in &self.sccs {
            if scc.contains(&crate_a.to_string()) && scc.contains(&crate_b.to_string()) {
                return true;
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_cycle() {
        let mut detector = CycleDetector::new();
        detector.add_dependency("A", "B", EdgeType::Normal);
        detector.add_dependency("B", "C", EdgeType::Normal);
        detector.add_dependency("C", "A", EdgeType::Normal);

        let cycles = detector.detect_cycles();
        assert_eq!(cycles.len(), 1);
        assert_eq!(cycles[0].crates.len(), 3);
        assert!(cycles[0].crates.contains(&"A".to_string()));
        assert!(cycles[0].crates.contains(&"B".to_string()));
        assert!(cycles[0].crates.contains(&"C".to_string()));
    }

    #[test]
    fn test_dev_dep_cycle() {
        let mut detector = CycleDetector::new();
        detector.add_dependency("proc-macro2", "quote", EdgeType::Dev);
        detector.add_dependency("quote", "proc-macro2", EdgeType::Normal);

        let cycles = detector.detect_cycles();
        assert_eq!(cycles.len(), 1);
        assert_eq!(cycles[0].crates.len(), 2);
    }

    #[test]
    fn test_no_cycle() {
        let mut detector = CycleDetector::new();
        detector.add_dependency("A", "B", EdgeType::Normal);
        detector.add_dependency("B", "C", EdgeType::Normal);
        detector.add_dependency("C", "D", EdgeType::Normal);

        let cycles = detector.detect_cycles();
        assert_eq!(cycles.len(), 0);
    }

    #[test]
    fn test_self_loop() {
        let mut detector = CycleDetector::new();
        detector.add_dependency("A", "A", EdgeType::Normal);

        let cycles = detector.detect_cycles();
        assert_eq!(cycles.len(), 1);
        assert_eq!(cycles[0].crates.len(), 1);
        assert_eq!(cycles[0].crates[0], "A");
    }

    #[test]
    fn test_multiple_cycles() {
        let mut detector = CycleDetector::new();
        // First cycle: A -> B -> A
        detector.add_dependency("A", "B", EdgeType::Normal);
        detector.add_dependency("B", "A", EdgeType::Normal);

        // Second cycle: C -> D -> E -> C
        detector.add_dependency("C", "D", EdgeType::Normal);
        detector.add_dependency("D", "E", EdgeType::Normal);
        detector.add_dependency("E", "C", EdgeType::Normal);

        let cycles = detector.detect_cycles();
        assert_eq!(cycles.len(), 2);
    }

    #[test]
    fn test_is_cyclic() {
        let mut detector = CycleDetector::new();
        detector.add_dependency("A", "B", EdgeType::Normal);
        detector.add_dependency("B", "C", EdgeType::Normal);
        detector.add_dependency("C", "A", EdgeType::Normal);
        detector.add_dependency("D", "E", EdgeType::Normal);

        detector.detect_cycles();

        assert!(detector.is_cyclic("A", "B"));
        assert!(detector.is_cyclic("B", "C"));
        assert!(detector.is_cyclic("C", "A"));
        assert!(!detector.is_cyclic("A", "D"));
        assert!(!detector.is_cyclic("D", "E"));
    }
}
