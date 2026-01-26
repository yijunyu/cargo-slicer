//! Debug logging module for cargo-slicer.
//!
//! Logs debug messages to a file when CARGO_SLICER_DEBUG_LOG environment variable is set.

use std::fs::OpenOptions;
use std::io::Write;
use std::sync::Mutex;
use std::sync::OnceLock;

static DEBUG_FILE: OnceLock<Mutex<Option<std::fs::File>>> = OnceLock::new();

/// Initialize debug logging if enabled
pub fn init() {
    if let Ok(log_path) = std::env::var("CARGO_SLICER_DEBUG_LOG") {
        let file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&log_path)
            .ok();

        let is_enabled = file.is_some();
        DEBUG_FILE.get_or_init(|| Mutex::new(file));

        if is_enabled {
            eprintln!("Debug logging enabled: {}", log_path);
        }
    } else {
        DEBUG_FILE.get_or_init(|| Mutex::new(None));
    }
}

/// Log a debug message if debug logging is enabled
#[macro_export]
macro_rules! debug_log {
    ($($arg:tt)*) => {
        $crate::debug_log::log_message(&format!($($arg)*))
    };
}

/// Internal function to write to debug log
pub fn log_message(msg: &str) {
    if let Some(file_mutex) = DEBUG_FILE.get() {
        if let Ok(mut guard) = file_mutex.lock() {
            if let Some(ref mut file) = *guard {
                let _ = writeln!(file, "{}", msg);
                let _ = file.flush();
            }
        }
    }
}

/// Check if debug logging is enabled
pub fn is_enabled() -> bool {
    if let Some(file_mutex) = DEBUG_FILE.get() {
        if let Ok(guard) = file_mutex.lock() {
            guard.is_some()
        } else {
            false
        }
    } else {
        false
    }
}
