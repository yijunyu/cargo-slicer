//! Debug logging for cargo-slicer rustc driver
//!
//! Logs to a file when CARGO_SLICER_DEBUG=1 is set.
//! Silent otherwise.

#![cfg(feature = "rustc-driver")]

use std::fs::{File, OpenOptions};
use std::io::Write;
use std::sync::OnceLock;
use std::sync::Mutex;

/// Global debug log file handle
static DEBUG_LOG: OnceLock<Option<Mutex<File>>> = OnceLock::new();

/// Initialize the debug logger
fn init_logger() -> Option<Mutex<File>> {
    if std::env::var("CARGO_SLICER_DEBUG").ok().as_deref() == Some("1") {
        let log_path = std::env::var("CARGO_SLICER_DEBUG_LOG")
            .unwrap_or_else(|_| ".cargo-slicer-debug.log".to_string());

        OpenOptions::new()
            .create(true)
            .append(true)
            .open(&log_path)
            .ok()
            .map(Mutex::new)
    } else {
        None
    }
}

/// Check if debug logging is enabled
pub fn is_debug_enabled() -> bool {
    DEBUG_LOG.get_or_init(init_logger).is_some()
}

/// Log a debug message to file (if enabled)
pub fn debug_log(message: &str) {
    if let Some(Some(file)) = DEBUG_LOG.get_or_init(init_logger).as_ref().map(Some) {
        if let Ok(mut f) = file.lock() {
            let _ = writeln!(f, "{}", message);
        }
    }
}

