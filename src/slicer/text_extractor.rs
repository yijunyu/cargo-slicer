// text_extractor.rs - Extract source code text from line-based tag information
//
// This module provides text extraction capabilities for ctags-based slicing.
// It converts line numbers from ctags into byte offsets and extracts complete
// item source text using a brace-counting state machine.

use std::sync::Arc;

/// Extract source code text using line-based tag information
///
/// This is the core component for text-based slicing with ctags.
/// Instead of manipulating AST nodes, we extract raw source text
/// based on line numbers and structural analysis.
pub struct TextExtractor {
    /// The full source code as a string
    source: Arc<String>,

    /// Byte offset for the start of each line (1-indexed to match ctags)
    /// line_offsets[0] is unused, line_offsets[1] is start of line 1, etc.
    line_offsets: Vec<usize>,
}

impl TextExtractor {
    /// Create a new TextExtractor from source code
    ///
    /// This precomputes line-to-byte offset mapping for fast random access.
    /// Time complexity: O(n) where n is the number of bytes in source.
    pub fn new(source: String) -> Self {
        let mut offsets = vec![0]; // line_offsets[0] is unused (lines are 1-indexed)

        // Compute byte offset for start of each line
        for (i, byte) in source.bytes().enumerate() {
            if byte == b'\n' {
                offsets.push(i + 1); // Start of next line is after the newline
            }
        }

        // Add end-of-file offset for convenience
        offsets.push(source.len());

        Self {
            source: Arc::new(source),
            line_offsets: offsets,
        }
    }

    /// Get byte offset for a given line number (1-indexed)
    ///
    /// Returns None if line number is out of range.
    pub fn line_to_offset(&self, line: usize) -> Option<usize> {
        if line == 0 || line >= self.line_offsets.len() {
            None
        } else {
            Some(self.line_offsets[line])
        }
    }

    /// Get line number for a given byte offset
    ///
    /// Uses binary search. Time complexity: O(log n) where n is number of lines.
    pub fn offset_to_line(&self, offset: usize) -> usize {
        match self.line_offsets.binary_search(&offset) {
            Ok(line) => line,
            Err(line) => line.saturating_sub(1),
        }
    }

    /// Get a reference to the source code
    ///
    /// This allows reading the source without copying.
    pub fn source_ref(&self) -> &str {
        &self.source
    }

    /// Extract the complete source text for an item starting at the given line
    ///
    /// This uses a state machine to find the item's end by tracking:
    /// - Brace depth (count { and })
    /// - String literals (ignore braces inside strings)
    /// - Comments (ignore braces inside comments)
    /// - Attribute macros (skip #[...] before item)
    ///
    /// Returns the extracted source text, or None if extraction fails.
    pub fn extract_item(&self, start_line: usize, kind: &str) -> Option<String> {
        let start_offset = self.line_to_offset(start_line)?;

        // Find the actual start (skip leading attributes and doc comments)
        let actual_start = self.find_item_start(start_offset);

        // Find the end using brace counting or semicolon detection
        let end_offset = match kind {
            // Items that end with semicolon
            "type" | "constant" => self.find_semicolon_end(actual_start)?,

            // Items that use braces
            "function" | "struct" | "enum" | "trait" | "impl" | "module" => {
                self.find_brace_end(actual_start)?
            }

            // Unknown kind - try brace first, fallback to semicolon
            _ => self.find_brace_end(actual_start)
                .or_else(|| self.find_semicolon_end(actual_start))?,
        };

        Some(self.source[actual_start..end_offset].to_string())
    }

    /// Find the actual start of an item, skipping attributes and doc comments
    ///
    /// Ctags reports the line where the item keyword appears, but Rust items
    /// can have attributes (#[...]) and doc comments (///, /** */) before them.
    /// We need to include these in the extracted text.
    fn find_item_start(&self, offset: usize) -> usize {
        let bytes = self.source.as_bytes();
        let mut pos = offset;

        // Scan backwards to find attributes and doc comments
        while pos > 0 {
            // Skip whitespace backwards
            while pos > 0 && bytes[pos - 1].is_ascii_whitespace() {
                pos -= 1;
            }

            if pos == 0 {
                break;
            }

            // Check for attribute #[...]
            if bytes[pos - 1] == b']' {
                if let Some(attr_start) = self.find_attribute_start(pos - 1) {
                    pos = attr_start;
                    continue;
                }
            }

            // Check for doc comment /// or //!
            if pos >= 3 && &bytes[pos-3..pos] == b"///" {
                // Find start of line
                while pos > 0 && bytes[pos - 1] != b'\n' {
                    pos -= 1;
                }
                continue;
            }

            // Check for doc comment /** */ or /*! */
            if pos >= 2 && &bytes[pos-2..pos] == b"*/" {
                if let Some(comment_start) = self.find_block_comment_start(pos - 2) {
                    // Check if it's a doc comment (starts with /** or /*!)
                    if comment_start >= 3 {
                        let prefix = &bytes[comment_start-3..comment_start];
                        if prefix == b"/**" || prefix == b"/*!" {
                            pos = comment_start - 3;
                            continue;
                        }
                    }
                }
            }

            // No more attributes or doc comments found
            break;
        }

        pos
    }

    /// Find the start of an attribute (#[...]), given the position of ]
    fn find_attribute_start(&self, end_pos: usize) -> Option<usize> {
        let bytes = self.source.as_bytes();
        let mut depth = 1; // We start at ]
        let mut pos = end_pos;

        while pos > 0 && depth > 0 {
            pos -= 1;
            match bytes[pos] {
                b']' => depth += 1,
                b'[' => depth -= 1,
                _ => {}
            }
        }

        // Check if [ is preceded by #
        if depth == 0 && pos > 0 && bytes[pos - 1] == b'#' {
            Some(pos - 1)
        } else {
            None
        }
    }

    /// Find the start of a block comment (/* or /**), given position before */
    fn find_block_comment_start(&self, end_pos: usize) -> Option<usize> {
        let bytes = self.source.as_bytes();
        let mut pos = end_pos;

        // Scan backwards looking for /*
        while pos >= 2 {
            if &bytes[pos-2..pos] == b"/*" {
                return Some(pos);
            }
            pos -= 1;
        }

        None
    }

    /// Find the end of an item that uses braces (functions, structs, etc.)
    ///
    /// Uses a state machine to track:
    /// - Brace depth (starts at 0, ends when we close the first brace)
    /// - String literals (ignore braces inside strings)
    /// - Comments (ignore braces inside comments)
    fn find_brace_end(&self, start_offset: usize) -> Option<usize> {
        let bytes = self.source.as_bytes();
        let mut pos = start_offset;
        let mut brace_depth = 0;
        let mut in_string = false;
        let mut in_char = false;
        let mut in_line_comment = false;
        let mut in_block_comment = false;
        let mut escape_next = false;

        while pos < bytes.len() {
            let byte = bytes[pos];

            // Handle escape sequences in strings/chars
            if escape_next {
                escape_next = false;
                pos += 1;
                continue;
            }

            if byte == b'\\' && (in_string || in_char) {
                escape_next = true;
                pos += 1;
                continue;
            }

            // Handle line comments
            if in_line_comment {
                if byte == b'\n' {
                    in_line_comment = false;
                }
                pos += 1;
                continue;
            }

            // Handle block comments
            if in_block_comment {
                if pos + 1 < bytes.len() && &bytes[pos..pos+2] == b"*/" {
                    in_block_comment = false;
                    pos += 2;
                    continue;
                }
                pos += 1;
                continue;
            }

            // Start of comment?
            if pos + 1 < bytes.len() && &bytes[pos..pos+2] == b"//" {
                in_line_comment = true;
                pos += 2;
                continue;
            }

            if pos + 1 < bytes.len() && &bytes[pos..pos+2] == b"/*" {
                in_block_comment = true;
                pos += 2;
                continue;
            }

            // Handle strings
            if byte == b'"' && !in_char {
                in_string = !in_string;
                pos += 1;
                continue;
            }

            // Handle char literals
            if byte == b'\'' && !in_string {
                in_char = !in_char;
                pos += 1;
                continue;
            }

            // Track braces (only when not in string/char/comment)
            if !in_string && !in_char {
                if byte == b'{' {
                    brace_depth += 1;
                } else if byte == b'}' {
                    if brace_depth == 0 {
                        // Found closing brace before any opening - likely a syntax error
                        return None;
                    }
                    brace_depth -= 1;

                    // If we've closed all braces, we found the end
                    if brace_depth == 0 {
                        return Some(pos + 1);
                    }
                }
            }

            pos += 1;
        }

        // Reached EOF without closing all braces
        None
    }

    /// Find the end of an item that ends with semicolon (type aliases, constants)
    fn find_semicolon_end(&self, start_offset: usize) -> Option<usize> {
        let bytes = self.source.as_bytes();
        let mut pos = start_offset;
        let mut in_string = false;
        let mut in_char = false;
        let mut in_line_comment = false;
        let mut in_block_comment = false;
        let mut escape_next = false;

        while pos < bytes.len() {
            let byte = bytes[pos];

            // Handle escape sequences
            if escape_next {
                escape_next = false;
                pos += 1;
                continue;
            }

            if byte == b'\\' && (in_string || in_char) {
                escape_next = true;
                pos += 1;
                continue;
            }

            // Handle comments (same logic as find_brace_end)
            if in_line_comment {
                if byte == b'\n' {
                    in_line_comment = false;
                }
                pos += 1;
                continue;
            }

            if in_block_comment {
                if pos + 1 < bytes.len() && &bytes[pos..pos+2] == b"*/" {
                    in_block_comment = false;
                    pos += 2;
                    continue;
                }
                pos += 1;
                continue;
            }

            if pos + 1 < bytes.len() && &bytes[pos..pos+2] == b"//" {
                in_line_comment = true;
                pos += 2;
                continue;
            }

            if pos + 1 < bytes.len() && &bytes[pos..pos+2] == b"/*" {
                in_block_comment = true;
                pos += 2;
                continue;
            }

            // Handle strings/chars
            if byte == b'"' && !in_char {
                in_string = !in_string;
                pos += 1;
                continue;
            }

            if byte == b'\'' && !in_string {
                in_char = !in_char;
                pos += 1;
                continue;
            }

            // Look for semicolon (only when not in string/char/comment)
            if byte == b';' && !in_string && !in_char {
                return Some(pos + 1);
            }

            pos += 1;
        }

        // Reached EOF without finding semicolon
        None
    }

    /// Get a reference to the full source code
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Get the number of lines in the source
    pub fn line_count(&self) -> usize {
        self.line_offsets.len().saturating_sub(2) // Subtract unused [0] and EOF entry
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_to_offset() {
        let source = "line 1\nline 2\nline 3\n".to_string();
        let extractor = TextExtractor::new(source);

        assert_eq!(extractor.line_to_offset(1), Some(0));
        assert_eq!(extractor.line_to_offset(2), Some(7));
        assert_eq!(extractor.line_to_offset(3), Some(14));
        assert_eq!(extractor.line_to_offset(4), Some(21)); // EOF
        assert_eq!(extractor.line_to_offset(5), None);     // Out of range
    }

    #[test]
    fn test_offset_to_line() {
        let source = "line 1\nline 2\nline 3\n".to_string();
        let extractor = TextExtractor::new(source);

        assert_eq!(extractor.offset_to_line(0), 1);
        assert_eq!(extractor.offset_to_line(6), 1);
        assert_eq!(extractor.offset_to_line(7), 2);
        assert_eq!(extractor.offset_to_line(13), 2);
        assert_eq!(extractor.offset_to_line(14), 3);
    }

    #[test]
    fn test_extract_simple_function() {
        let source = "pub fn hello() {\n    println!(\"Hello\");\n}\n".to_string();
        let extractor = TextExtractor::new(source);

        let extracted = extractor.extract_item(1, "function").unwrap();
        assert_eq!(extracted, "pub fn hello() {\n    println!(\"Hello\");\n}");
    }

    #[test]
    fn test_extract_function_with_attribute() {
        let source = "#[inline]\npub fn test() {\n    42\n}\n".to_string();
        let extractor = TextExtractor::new(source);

        let extracted = extractor.extract_item(2, "function").unwrap();
        assert_eq!(extracted, "#[inline]\npub fn test() {\n    42\n}");
    }

    #[test]
    fn test_extract_type_alias() {
        let source = "pub type Result<T> = std::result::Result<T, Error>;\n".to_string();
        let extractor = TextExtractor::new(source);

        let extracted = extractor.extract_item(1, "type").unwrap();
        assert_eq!(extracted, "pub type Result<T> = std::result::Result<T, Error>;");
    }

    #[test]
    fn test_extract_struct_with_nested_braces() {
        let source = "pub struct Point {\n    x: i32,\n    y: i32,\n}\n".to_string();
        let extractor = TextExtractor::new(source);

        let extracted = extractor.extract_item(1, "struct").unwrap();
        assert_eq!(extracted, "pub struct Point {\n    x: i32,\n    y: i32,\n}");
    }

    #[test]
    fn test_skip_braces_in_strings() {
        let source = "pub fn test() {\n    let s = \"{ not a brace }\";\n}\n".to_string();
        let extractor = TextExtractor::new(source);

        let extracted = extractor.extract_item(1, "function").unwrap();
        assert_eq!(extracted, "pub fn test() {\n    let s = \"{ not a brace }\";\n}");
    }

    #[test]
    fn test_skip_braces_in_comments() {
        let source = "pub fn test() {\n    // { comment brace }\n    /* { block comment } */\n}\n".to_string();
        let extractor = TextExtractor::new(source);

        let extracted = extractor.extract_item(1, "function").unwrap();
        assert_eq!(extracted, "pub fn test() {\n    // { comment brace }\n    /* { block comment } */\n}");
    }
}
