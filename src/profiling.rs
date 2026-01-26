// Profiling infrastructure for cargo-slicer
// Provides hierarchical timing data collection with minimal overhead

use std::cell::RefCell;
use std::collections::HashMap;
use std::time::{Duration, Instant};
use serde::{Serialize, Deserialize};

thread_local! {
    static PROFILING_ENABLED: RefCell<bool> = RefCell::new(false);
    static PROFILE_COLLECTOR: RefCell<ProfileCollector> = RefCell::new(ProfileCollector::new());
}

/// Enable or disable profiling globally
pub fn set_profiling_enabled(enabled: bool) {
    PROFILING_ENABLED.with(|e| *e.borrow_mut() = enabled);
}

/// Check if profiling is enabled
pub fn is_profiling_enabled() -> bool {
    PROFILING_ENABLED.with(|e| *e.borrow())
}

/// Hierarchical timing data for a single operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimingData {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub duration_ms: Option<f64>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub children: Vec<TimingData>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub metadata: HashMap<String, String>,
}

impl TimingData {
    pub fn new(name: String) -> Self {
        Self {
            name,
            duration_ms: None,
            children: Vec::new(),
            metadata: HashMap::new(),
        }
    }

    pub fn with_duration(mut self, duration: Duration) -> Self {
        self.duration_ms = Some(duration.as_secs_f64() * 1000.0);
        self
    }

    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }

    pub fn add_child(&mut self, child: TimingData) {
        self.children.push(child);
    }
}

/// Collects profiling data in a hierarchical structure
#[derive(Debug)]
pub struct ProfileCollector {
    stack: Vec<TimingEntry>,
    completed: Vec<TimingData>,
}

#[derive(Debug)]
struct TimingEntry {
    name: String,
    start: Instant,
    metadata: HashMap<String, String>,
}

impl ProfileCollector {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            completed: Vec::new(),
        }
    }

    /// Start timing an operation
    pub fn start(&mut self, name: String) {
        self.stack.push(TimingEntry {
            name,
            start: Instant::now(),
            metadata: HashMap::new(),
        });
    }

    /// Start timing an operation with metadata
    pub fn start_with_metadata(&mut self, name: String, metadata: HashMap<String, String>) {
        self.stack.push(TimingEntry {
            name,
            start: Instant::now(),
            metadata,
        });
    }

    /// End timing the current operation
    pub fn end(&mut self) -> Option<TimingData> {
        if let Some(entry) = self.stack.pop() {
            let duration = entry.start.elapsed();
            let mut timing = TimingData::new(entry.name)
                .with_duration(duration);
            timing.metadata = entry.metadata;

            // If there's a parent, add as child; otherwise add to completed
            if let Some(_parent_entry) = self.stack.last_mut() {
                // We'll collect children when the parent ends
                // For now, temporarily store in completed
                self.completed.push(timing.clone());
            } else {
                self.completed.push(timing.clone());
            }

            Some(timing)
        } else {
            None
        }
    }

    /// Get all completed timing data
    pub fn get_completed(&self) -> &[TimingData] {
        &self.completed
    }

    /// Clear all collected data
    pub fn clear(&mut self) {
        self.stack.clear();
        self.completed.clear();
    }

    /// Get timing data as hierarchical structure
    pub fn into_hierarchical(self) -> Vec<TimingData> {
        self.completed
    }
}

/// Start timing an operation (no-op if profiling disabled)
pub fn start_timing(name: &str) {
    if is_profiling_enabled() {
        PROFILE_COLLECTOR.with(|c| {
            c.borrow_mut().start(name.to_string());
        });
    }
}

/// Start timing with metadata
pub fn start_timing_with_metadata(name: &str, metadata: HashMap<String, String>) {
    if is_profiling_enabled() {
        PROFILE_COLLECTOR.with(|c| {
            c.borrow_mut().start_with_metadata(name.to_string(), metadata);
        });
    }
}

/// End timing the current operation
pub fn end_timing() -> Option<TimingData> {
    if is_profiling_enabled() {
        PROFILE_COLLECTOR.with(|c| {
            c.borrow_mut().end()
        })
    } else {
        None
    }
}

/// Get all completed timing data
pub fn get_timing_data() -> Vec<TimingData> {
    if is_profiling_enabled() {
        PROFILE_COLLECTOR.with(|c| {
            c.borrow().get_completed().to_vec()
        })
    } else {
        Vec::new()
    }
}

/// Clear all timing data
pub fn clear_timing_data() {
    PROFILE_COLLECTOR.with(|c| {
        c.borrow_mut().clear();
    });
}

/// Profile report summarizing timing data
#[derive(Debug, Serialize, Deserialize)]
pub struct ProfileReport {
    pub crate_name: Option<String>,
    pub total_time_ms: f64,
    pub phases: Vec<TimingData>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub summary: HashMap<String, String>,
}

impl ProfileReport {
    pub fn new() -> Self {
        Self {
            crate_name: None,
            total_time_ms: 0.0,
            phases: Vec::new(),
            summary: HashMap::new(),
        }
    }

    pub fn from_timing_data(data: Vec<TimingData>) -> Self {
        let total_time = data.iter()
            .filter_map(|t| t.duration_ms)
            .sum();

        Self {
            crate_name: None,
            total_time_ms: total_time,
            phases: data,
            summary: HashMap::new(),
        }
    }

    pub fn with_crate_name(mut self, name: String) -> Self {
        self.crate_name = Some(name);
        self
    }

    pub fn add_summary(mut self, key: String, value: String) -> Self {
        self.summary.insert(key, value);
        self
    }

    /// Export to JSON
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }

    /// Write to file
    pub fn write_to_file(&self, path: &std::path::Path) -> Result<(), std::io::Error> {
        let json = self.to_json()
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
        std::fs::write(path, json)
    }

    /// Print human-readable summary
    pub fn print_summary(&self) {
        if let Some(crate_name) = &self.crate_name {
            println!("\n=== Profile Report: {} ===", crate_name);
        } else {
            println!("\n=== Profile Report ===");
        }

        println!("  Total time: {:.3}s", self.total_time_ms / 1000.0);

        if !self.phases.is_empty() {
            println!("\nPhase Breakdown:");
            for phase in &self.phases {
                if let Some(duration_ms) = phase.duration_ms {
                    let pct = (duration_ms / self.total_time_ms) * 100.0;
                    println!("  {:30} {:8.1}ms  ({:5.1}%)",
                        phase.name, duration_ms, pct);

                    // Print children with indentation
                    self.print_children(&phase.children, 4, self.total_time_ms);
                }
            }
        }

        if !self.summary.is_empty() {
            println!("\nSummary:");
            for (key, value) in &self.summary {
                println!("  {}: {}", key, value);
            }
        }
    }

    fn print_children(&self, children: &[TimingData], indent: usize, total_ms: f64) {
        for child in children {
            if let Some(duration_ms) = child.duration_ms {
                let pct = (duration_ms / total_ms) * 100.0;
                println!("{:indent$}{:30} {:8.1}ms  ({:5.1}%)",
                    "", child.name, duration_ms, pct, indent = indent);

                if !child.children.is_empty() {
                    self.print_children(&child.children, indent + 2, total_ms);
                }
            }
        }
    }
}

/// Macro to time a code block
#[macro_export]
macro_rules! time_operation {
    ($name:expr, $block:expr) => {{
        $crate::profiling::start_timing($name);
        let result = $block;
        $crate::profiling::end_timing();
        result
    }};
}

/// Macro to time a phase (top-level operation)
#[macro_export]
macro_rules! time_phase {
    ($name:expr, $block:expr) => {{
        $crate::profiling::start_timing($name);
        let result = $block;
        $crate::profiling::end_timing();
        result
    }};
}

/// Macro to time a function call
#[macro_export]
macro_rules! time_function {
    ($name:expr, $block:expr) => {{
        $crate::profiling::start_timing($name);
        let result = $block;
        $crate::profiling::end_timing();
        result
    }};
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn test_basic_timing() {
        set_profiling_enabled(true);
        clear_timing_data();

        start_timing("test_operation");
        thread::sleep(Duration::from_millis(10));
        end_timing();

        let data = get_timing_data();
        assert_eq!(data.len(), 1);
        assert_eq!(data[0].name, "test_operation");
        assert!(data[0].duration_ms.unwrap() >= 10.0);
    }

    #[test]
    fn test_profiling_disabled() {
        set_profiling_enabled(false);
        clear_timing_data();

        start_timing("should_not_record");
        end_timing();

        let data = get_timing_data();
        assert_eq!(data.len(), 0);
    }

    #[test]
    fn test_profile_report() {
        let data = vec![
            TimingData::new("Phase 1".to_string())
                .with_duration(Duration::from_millis(100)),
            TimingData::new("Phase 2".to_string())
                .with_duration(Duration::from_millis(200)),
        ];

        let report = ProfileReport::from_timing_data(data)
            .with_crate_name("test-crate".to_string());

        assert_eq!(report.total_time_ms, 300.0);
        assert_eq!(report.phases.len(), 2);

        let json = report.to_json().unwrap();
        assert!(json.contains("test-crate"));
        assert!(json.contains("Phase 1"));
    }
}
