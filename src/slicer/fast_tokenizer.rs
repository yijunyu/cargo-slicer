///! Lightweight Rust tokenizer optimized for extracting item boundaries.
///!
///! This tokenizer is designed to be FAST, not 100% correct. It's good enough
///! for item extraction and significantly faster than full parsing.

// use std::collections::HashMap;  // Not needed
use crate::types::{ParsedItemKind, ItemVisibility};

/// A token in the Rust source code
#[derive(Debug, Clone, PartialEq)]
enum Token {
    /// Keyword (fn, struct, enum, etc.)
    Keyword(&'static str),
    /// Identifier
    Ident(String),
    /// Opening brace {
    OpenBrace,
    /// Closing brace }
    CloseBrace,
    /// Visibility modifier (pub, pub(crate), etc.)
    Vis(ItemVisibility),
    /// Attribute #[...]
    Attribute,
    /// Other tokens we don't care about
    Other,
}

/// Lightweight item with byte ranges
#[derive(Debug, Clone)]
pub struct ItemWithRange {
    pub name: String,
    pub kind: ParsedItemKind,
    pub visibility: ItemVisibility,
    pub start_byte: usize,
    pub end_byte: usize,
    pub file: String,
}

/// Fast tokenizer that extracts items with byte ranges in a single pass
pub struct FastTokenizer<'a> {
    source: &'a str,
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> FastTokenizer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            bytes: source.as_bytes(),
            pos: 0,
        }
    }

    /// Parse source file and extract all items with byte ranges
    pub fn parse_file<'src>(source: &'src str, file_path: &str) -> Vec<ItemWithRange> {
        let mut tokenizer = FastTokenizer::new(source);
        let mut items = Vec::new();
        let mut current_vis = ItemVisibility::Private;

        while tokenizer.pos < tokenizer.bytes.len() {
            tokenizer.skip_whitespace_and_comments();

            if tokenizer.pos >= tokenizer.bytes.len() {
                break;
            }

            let start_pos = tokenizer.pos;

            // Check for visibility modifiers
            if tokenizer.match_keyword("pub") {
                tokenizer.pos += 3;  // Consume "pub"
                current_vis = tokenizer.parse_visibility();
                continue;
            }

            // Check for attributes (skip them)
            if tokenizer.peek() == b'#' {
                tokenizer.skip_attribute();
                continue;
            }

            // Check for item keywords
            if let Some((keyword, kind)) = tokenizer.match_item_keyword() {
                if let Some(item) = tokenizer.parse_item(keyword, kind, current_vis, file_path) {
                    items.push(item);
                }
                current_vis = ItemVisibility::Private; // Reset after item
            } else {
                // Skip unknown token
                tokenizer.skip_token();
            }

            // Safety check: ensure we're making progress
            if tokenizer.pos == start_pos {
                // Force advance to prevent infinite loop
                tokenizer.pos += 1;
            }
        }

        items
    }

    /// Skip whitespace and comments
    fn skip_whitespace_and_comments(&mut self) {
        while self.pos < self.bytes.len() {
            match self.peek() {
                b' ' | b'\t' | b'\n' | b'\r' => {
                    self.pos += 1;
                }
                b'/' if self.peek_ahead(1) == Some(b'/') => {
                    // Line comment
                    while self.pos < self.bytes.len() && self.peek() != b'\n' {
                        self.pos += 1;
                    }
                }
                b'/' if self.peek_ahead(1) == Some(b'*') => {
                    // Block comment
                    self.pos += 2;
                    while self.pos + 1 < self.bytes.len() {
                        if self.peek() == b'*' && self.peek_ahead(1) == Some(b'/') {
                            self.pos += 2;
                            break;
                        }
                        self.pos += 1;
                    }
                }
                _ => break,
            }
        }
    }

    /// Skip attribute like #[derive(Debug)]
    fn skip_attribute(&mut self) {
        if self.peek() != b'#' {
            return;
        }

        self.pos += 1; // skip #

        // Skip potential !
        if self.peek() == b'!' {
            self.pos += 1;
        }

        // Skip [...]
        if self.peek() == b'[' {
            let mut depth = 1;
            self.pos += 1;

            while self.pos < self.bytes.len() && depth > 0 {
                match self.peek() {
                    b'[' => depth += 1,
                    b']' => depth -= 1,
                    b'"' => self.skip_string(),
                    _ => {}
                }
                self.pos += 1;
            }
        }
    }

    /// Skip a string literal (to avoid confusing braces inside strings)
    fn skip_string(&mut self) {
        if self.peek() != b'"' {
            return;
        }

        self.pos += 1; // skip opening "

        while self.pos < self.bytes.len() {
            match self.peek() {
                b'"' => {
                    self.pos += 1;
                    return;
                }
                b'\\' => {
                    // Skip escape sequence
                    self.pos += 2;
                }
                _ => {
                    self.pos += 1;
                }
            }
        }
    }

    /// Parse visibility modifier
    fn parse_visibility(&mut self) -> ItemVisibility {
        // Already matched "pub"
        self.skip_whitespace_and_comments();

        if self.peek() == b'(' {
            // pub(crate), pub(super), etc.
            self.pos += 1;
            let start = self.pos;

            while self.pos < self.bytes.len() && self.peek() != b')' {
                self.pos += 1;
            }

            let vis_str = &self.source[start..self.pos];

            if self.peek() == b')' {
                self.pos += 1;
            }

            match vis_str.trim() {
                "crate" => ItemVisibility::Crate,
                "super" => ItemVisibility::Super,
                _ if vis_str.trim().starts_with("in ") => ItemVisibility::InPath(vis_str.trim()[3..].to_string()),
                _ => ItemVisibility::Public,
            }
        } else {
            ItemVisibility::Public
        }
    }

    /// Try to match an item keyword (fn, struct, enum, etc.)
    fn match_item_keyword(&mut self) -> Option<(&'static str, ParsedItemKind)> {
        let keywords = [
            ("fn", ParsedItemKind::Function),
            ("struct", ParsedItemKind::Struct),
            ("enum", ParsedItemKind::Enum),
            ("trait", ParsedItemKind::Trait),
            ("impl", ParsedItemKind::Impl),
            ("mod", ParsedItemKind::Mod),
            ("const", ParsedItemKind::Const),
            ("static", ParsedItemKind::Static),
            ("type", ParsedItemKind::TypeAlias),
            ("macro_rules", ParsedItemKind::Macro),
        ];

        for (keyword, kind) in &keywords {
            if self.match_keyword(keyword) {
                return Some((keyword, kind.clone()));
            }
        }

        None
    }

    /// Check if next token matches a keyword
    fn match_keyword(&self, keyword: &str) -> bool {
        let keyword_bytes = keyword.as_bytes();

        if self.pos + keyword_bytes.len() > self.bytes.len() {
            return false;
        }

        // Check keyword matches
        if &self.bytes[self.pos..self.pos + keyword_bytes.len()] != keyword_bytes {
            return false;
        }

        // Check it's followed by non-identifier char
        let next_pos = self.pos + keyword_bytes.len();
        if next_pos < self.bytes.len() {
            let next_char = self.bytes[next_pos];
            if next_char.is_ascii_alphanumeric() || next_char == b'_' {
                return false;
            }
        }

        true
    }

    /// Parse an item starting with the given keyword
    fn parse_item(
        &mut self,
        keyword: &str,
        kind: ParsedItemKind,
        visibility: ItemVisibility,
        file_path: &str,
    ) -> Option<ItemWithRange> {
        let start_byte = self.pos;

        // Skip keyword
        self.pos += keyword.len();
        self.skip_whitespace_and_comments();

        // Extract item name
        let name = match kind {
            ParsedItemKind::Impl => {
                // impl has no name, use "impl" as placeholder
                self.find_item_end(start_byte)?;
                return Some(ItemWithRange {
                    name: "impl".to_string(),
                    kind,
                    visibility,
                    start_byte,
                    end_byte: self.pos,
                    file: file_path.to_string(),
                });
            }
            _ => {
                self.parse_ident()?
            }
        };

        // Find end of item
        self.find_item_end(start_byte)?;

        Some(ItemWithRange {
            name,
            kind,
            visibility,
            start_byte,
            end_byte: self.pos,
            file: file_path.to_string(),
        })
    }

    /// Parse an identifier
    fn parse_ident(&mut self) -> Option<String> {
        let start = self.pos;

        // First char must be alphabetic or _
        if self.pos >= self.bytes.len() {
            return None;
        }

        let first_char = self.bytes[self.pos];
        if !first_char.is_ascii_alphabetic() && first_char != b'_' {
            return None;
        }

        self.pos += 1;

        // Rest can be alphanumeric or _
        while self.pos < self.bytes.len() {
            let ch = self.bytes[self.pos];
            if ch.is_ascii_alphanumeric() || ch == b'_' {
                self.pos += 1;
            } else {
                break;
            }
        }

        Some(self.source[start..self.pos].to_string())
    }

    /// Find the end of an item by matching braces
    fn find_item_end(&mut self, _start_byte: usize) -> Option<()> {
        self.skip_whitespace_and_comments();

        // Some items end with semicolon (use, type alias, etc.)
        if self.peek() == b';' {
            self.pos += 1;
            return Some(());
        }

        // Skip generic parameters <T>
        if self.peek() == b'<' {
            self.skip_generics();
        }

        self.skip_whitespace_and_comments();

        // Skip where clause
        if self.match_keyword("where") {
            self.skip_where_clause();
        }

        self.skip_whitespace_and_comments();

        // Find opening brace — scan forward past params, return type, etc.
        if self.peek() != b'{' {
            // Skip forward looking for '{' or ';'
            while self.pos < self.bytes.len() {
                match self.peek() {
                    b'{' => break,     // Found body — fall through to brace matching
                    b';' => {
                        self.pos += 1;
                        return Some(());
                    }
                    b'"' => self.skip_string(),
                    b'\'' => self.skip_char_literal(),
                    _ => self.pos += 1,
                }
            }
            if self.pos >= self.bytes.len() || self.peek() != b'{' {
                return Some(());
            }
        }

        // Match braces
        let mut depth = 0;

        while self.pos < self.bytes.len() {
            match self.peek() {
                b'{' => {
                    depth += 1;
                    self.pos += 1;
                }
                b'}' => {
                    depth -= 1;
                    self.pos += 1;
                    if depth == 0 {
                        return Some(());
                    }
                }
                b'"' => {
                    self.skip_string();
                }
                b'\'' => {
                    self.skip_char_literal();
                }
                b'/' if self.peek_ahead(1) == Some(b'/') => {
                    self.skip_line_comment();
                }
                b'/' if self.peek_ahead(1) == Some(b'*') => {
                    self.skip_block_comment();
                }
                _ => {
                    self.pos += 1;
                }
            }
        }

        // Reached end without matching braces - still valid, use current position
        Some(())
    }

    /// Skip generic parameters <T, U: Trait>
    fn skip_generics(&mut self) {
        if self.peek() != b'<' {
            return;
        }

        let mut depth = 1;
        self.pos += 1;

        while self.pos < self.bytes.len() && depth > 0 {
            match self.peek() {
                b'<' => depth += 1,
                b'>' => depth -= 1,
                b'"' => self.skip_string(),
                b'\'' => self.skip_char_literal(),
                _ => {}
            }
            self.pos += 1;
        }
    }

    /// Skip where clause
    fn skip_where_clause(&mut self) {
        // Skip "where" keyword
        self.pos += 5;
        self.skip_whitespace_and_comments();

        // Skip until we hit { or ;
        while self.pos < self.bytes.len() {
            match self.peek() {
                b'{' | b';' => return,
                b'"' => self.skip_string(),
                b'\'' => self.skip_char_literal(),
                _ => self.pos += 1,
            }
        }
    }

    /// Skip character literal
    fn skip_char_literal(&mut self) {
        if self.peek() != b'\'' {
            return;
        }

        self.pos += 1; // skip opening '

        // Handle escape
        if self.peek() == b'\\' {
            self.pos += 2;
        } else {
            self.pos += 1;
        }

        // Skip closing '
        if self.peek() == b'\'' {
            self.pos += 1;
        }
    }

    /// Skip line comment
    fn skip_line_comment(&mut self) {
        while self.pos < self.bytes.len() && self.peek() != b'\n' {
            self.pos += 1;
        }
    }

    /// Skip block comment
    fn skip_block_comment(&mut self) {
        self.pos += 2; // skip /*

        while self.pos + 1 < self.bytes.len() {
            if self.peek() == b'*' && self.peek_ahead(1) == Some(b'/') {
                self.pos += 2;
                return;
            }
            self.pos += 1;
        }
    }

    /// Skip any token (fallback)
    fn skip_token(&mut self) {
        if self.pos >= self.bytes.len() {
            return;
        }

        // Skip single char
        self.pos += 1;
    }

    /// Peek at current byte
    fn peek(&self) -> u8 {
        if self.pos < self.bytes.len() {
            self.bytes[self.pos]
        } else {
            0
        }
    }

    /// Peek ahead by n bytes
    fn peek_ahead(&self, n: usize) -> Option<u8> {
        let pos = self.pos + n;
        if pos < self.bytes.len() {
            Some(self.bytes[pos])
        } else {
            None
        }
    }
}

/// Scan function bodies for call edges (lightweight byte-level scanning).
///
/// For each function item (identified by byte range), scans the body text for
/// `identifier(` patterns, resolves identifiers via the imports table, and
/// returns (caller_path, callee_path) pairs.
///
/// Over-reports call edges (false positives are safe — they just mark more items
/// as reachable). Under-reports method calls (no type info for receiver).
pub fn scan_call_edges(
    source: &str,
    items: &[ItemWithRange],
    imports: &std::collections::HashMap<String, String>,
    crate_name: &str,
    module_path: &str,
) -> Vec<(String, String)> {
    let bytes = source.as_bytes();
    let mut edges = Vec::new();

    for item in items {
        if item.kind != ParsedItemKind::Function {
            continue;
        }

        let caller_path = format!("{}::{}", module_path, item.name);

        // Find the function body: first '{' after start_byte, then match braces
        let body_start = match find_body_start(bytes, item.start_byte, item.end_byte) {
            Some(pos) => pos,
            None => continue,
        };

        // Scan the body for identifier( patterns
        let mut pos = body_start;
        while pos < item.end_byte && pos < bytes.len() {
            // Skip whitespace
            if bytes[pos].is_ascii_whitespace() {
                pos += 1;
                continue;
            }

            // Skip strings
            if bytes[pos] == b'"' {
                pos = skip_string_bytes(bytes, pos);
                continue;
            }

            // Skip char literals
            if bytes[pos] == b'\'' {
                pos = skip_char_bytes(bytes, pos);
                continue;
            }

            // Skip line comments
            if pos + 1 < bytes.len() && bytes[pos] == b'/' && bytes[pos + 1] == b'/' {
                while pos < bytes.len() && bytes[pos] != b'\n' {
                    pos += 1;
                }
                continue;
            }

            // Skip block comments
            if pos + 1 < bytes.len() && bytes[pos] == b'/' && bytes[pos + 1] == b'*' {
                pos += 2;
                while pos + 1 < bytes.len() {
                    if bytes[pos] == b'*' && bytes[pos + 1] == b'/' {
                        pos += 2;
                        break;
                    }
                    pos += 1;
                }
                continue;
            }

            // Try to match an identifier
            if bytes[pos].is_ascii_alphabetic() || bytes[pos] == b'_' {
                let ident_start = pos;
                while pos < bytes.len()
                    && (bytes[pos].is_ascii_alphanumeric() || bytes[pos] == b'_')
                {
                    pos += 1;
                }
                let ident = &source[ident_start..pos];

                // Skip keywords that aren't call targets
                if is_rust_keyword(ident) {
                    continue;
                }

                // Check for path separator :: (qualified call like Foo::bar())
                let mut full_path = ident.to_string();
                while pos + 1 < bytes.len() && bytes[pos] == b':' && bytes[pos + 1] == b':' {
                    pos += 2; // skip ::
                    let seg_start = pos;
                    while pos < bytes.len()
                        && (bytes[pos].is_ascii_alphanumeric() || bytes[pos] == b'_')
                    {
                        pos += 1;
                    }
                    if pos > seg_start {
                        full_path.push_str("::");
                        full_path.push_str(&source[seg_start..pos]);
                    }
                }

                // Skip whitespace before potential '('
                let mut peek = pos;
                while peek < bytes.len() && bytes[peek].is_ascii_whitespace() {
                    peek += 1;
                }

                // Check if followed by '(' — this is a call
                if peek < bytes.len() && bytes[peek] == b'(' {
                    if let Some(callee) = resolve_call_ident(
                        &full_path, imports, crate_name, module_path,
                    ) {
                        edges.push((caller_path.clone(), callee));
                    }
                }

                continue;
            }

            pos += 1;
        }
    }

    edges
}

/// Find the start of a function body (position after the opening '{').
fn find_body_start(bytes: &[u8], start: usize, end: usize) -> Option<usize> {
    let mut pos = start;
    while pos < end && pos < bytes.len() {
        if bytes[pos] == b'{' {
            return Some(pos + 1);
        }
        pos += 1;
    }
    None
}

/// Skip a string literal in byte array, returning position after closing quote.
fn skip_string_bytes(bytes: &[u8], start: usize) -> usize {
    let mut pos = start + 1; // skip opening "
    while pos < bytes.len() {
        match bytes[pos] {
            b'"' => return pos + 1,
            b'\\' => pos += 2,
            _ => pos += 1,
        }
    }
    pos
}

/// Skip a char literal in byte array.
fn skip_char_bytes(bytes: &[u8], start: usize) -> usize {
    let mut pos = start + 1; // skip opening '
    if pos < bytes.len() && bytes[pos] == b'\\' {
        pos += 2;
    } else {
        pos += 1;
    }
    if pos < bytes.len() && bytes[pos] == b'\'' {
        pos += 1;
    }
    pos
}

/// Check if an identifier is a Rust keyword (not a call target).
fn is_rust_keyword(ident: &str) -> bool {
    matches!(
        ident,
        "as" | "break" | "const" | "continue" | "crate" | "else" | "enum"
            | "extern" | "false" | "fn" | "for" | "if" | "impl" | "in"
            | "let" | "loop" | "match" | "mod" | "move" | "mut" | "pub"
            | "ref" | "return" | "self" | "Self" | "static" | "struct"
            | "super" | "trait" | "true" | "type" | "unsafe" | "use"
            | "where" | "while" | "async" | "await" | "dyn" | "yield"
    )
}

/// Resolve a call identifier to a fully-qualified path.
fn resolve_call_ident(
    ident: &str,
    imports: &std::collections::HashMap<String, String>,
    crate_name: &str,
    module_path: &str,
) -> Option<String> {
    let first_segment = ident.split("::").next().unwrap_or(ident);

    // Check imports table (both with module prefix and bare)
    let with_mod = format!("{}::{}", module_path, first_segment);
    if let Some(resolved) = imports.get(&with_mod) {
        if ident.contains("::") {
            let rest = &ident[first_segment.len() + 2..];
            return Some(format!("{}::{}", resolved, rest));
        }
        return Some(resolved.clone());
    }
    if let Some(resolved) = imports.get(first_segment) {
        if ident.contains("::") {
            let rest = &ident[first_segment.len() + 2..];
            return Some(format!("{}::{}", resolved, rest));
        }
        return Some(resolved.clone());
    }

    // crate:: prefix
    if first_segment == "crate" {
        let rest = &ident["crate::".len()..];
        return Some(format!("{}::{}", crate_name, rest));
    }

    // self:: prefix
    if first_segment == "self" {
        let rest = &ident["self::".len()..];
        return Some(format!("{}::{}", module_path, rest));
    }

    // Single identifier → local call within current module
    if !ident.contains("::") {
        return Some(format!("{}::{}", module_path, ident));
    }

    // Multi-segment unresolved → prepend current module as best effort
    Some(format!("{}::{}", module_path, ident))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_function() {
        let source = r#"
            pub fn hello() {
                println!("Hello");
            }
        "#;

        let items = FastTokenizer::parse_file(source, "test.rs");
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].name, "hello");
        assert_eq!(items[0].kind, ParsedItemKind::Function);
        assert_eq!(items[0].visibility, ItemVisibility::Public);
        assert!(items[0].end_byte > items[0].start_byte);
    }

    #[test]
    fn test_struct_with_braces_in_string() {
        let source = r#"
            struct Foo {
                s: &'static str = "{ not a brace }",
            }
        "#;

        let items = FastTokenizer::parse_file(source, "test.rs");
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].name, "Foo");
        assert_eq!(items[0].kind, ParsedItemKind::Struct);
    }

    #[test]
    fn test_nested_items() {
        let source = r#"
            mod outer {
                pub struct Inner {
                    field: i32,
                }

                fn helper() {}
            }
        "#;

        let items = FastTokenizer::parse_file(source, "test.rs");
        // Should extract outer mod and nested items
        assert!(items.len() >= 1);
        assert_eq!(items[0].name, "outer");
        assert_eq!(items[0].kind, ParsedItemKind::Mod);
    }

    #[test]
    fn test_with_attributes() {
        let source = r#"
            #[derive(Debug, Clone)]
            #[cfg(test)]
            pub struct Foo {
                x: i32,
            }
        "#;

        let items = FastTokenizer::parse_file(source, "test.rs");
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].name, "Foo");
    }

    #[test]
    fn test_scan_call_edges_simple() {
        let source = "fn hello() { helper(); }\nfn helper() {}";
        let items = FastTokenizer::parse_file(source, "test.rs");
        let imports = std::collections::HashMap::new();
        let edges = scan_call_edges(source, &items, &imports, "mylib", "mylib");

        // hello → helper edge
        assert!(
            edges.iter().any(|(c, e)| c == "mylib::hello" && e == "mylib::helper"),
            "missing hello→helper edge: {:?}", edges
        );
    }

    #[test]
    fn test_scan_call_edges_qualified() {
        let source = "fn main() { Foo::bar(); }";
        let items = FastTokenizer::parse_file(source, "test.rs");
        let imports = std::collections::HashMap::new();
        let edges = scan_call_edges(source, &items, &imports, "mylib", "mylib");

        assert!(
            edges.iter().any(|(c, e)| c == "mylib::main" && e == "mylib::Foo::bar"),
            "missing qualified call edge: {:?}", edges
        );
    }

    #[test]
    fn test_scan_call_edges_skip_strings() {
        let source = r#"fn main() { let s = "not_a_call()"; real_call(); }"#;
        let items = FastTokenizer::parse_file(source, "test.rs");
        let imports = std::collections::HashMap::new();
        let edges = scan_call_edges(source, &items, &imports, "mylib", "mylib");

        // Should NOT have not_a_call as a callee
        assert!(
            !edges.iter().any(|(_, e)| e.contains("not_a_call")),
            "false positive from string: {:?}", edges
        );
        // Should have real_call
        assert!(
            edges.iter().any(|(_, e)| e.contains("real_call")),
            "missing real_call edge: {:?}", edges
        );
    }

    #[test]
    fn test_scan_call_edges_with_imports() {
        let source = "fn main() { do_thing(); }";
        let items = FastTokenizer::parse_file(source, "test.rs");
        let mut imports = std::collections::HashMap::new();
        imports.insert("do_thing".to_string(), "other_crate::utils::do_thing".to_string());
        let edges = scan_call_edges(source, &items, &imports, "mylib", "mylib");

        assert!(
            edges.iter().any(|(_, e)| e == "other_crate::utils::do_thing"),
            "import not resolved: {:?}", edges
        );
    }
}
