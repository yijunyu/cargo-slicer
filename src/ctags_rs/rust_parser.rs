//! Pure-Rust parser for extracting ctags-style tags from Rust source code
//!
//! This module implements a lightweight parser that extracts only tag information
//! (item names, kinds, line numbers, visibility) without building a full AST.
//!
//! Goals:
//! - 3-10x faster than syn's full AST parsing
//! - Extract only what's needed for dependency tracking
//! - No macro expansion, no type checking
//!
//! Phase 1 MVP: Parse top-level items only (fn, struct, enum, trait, const, type, mod)

use std::path::Path;
use std::fs;

/// Tag kind for Rust items
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TagKind {
    Function,
    Struct,
    Enum,
    Trait,
    Constant,
    Static,
    TypeAlias,
    Module,
    Impl,
    Macro,
}

/// Visibility level
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Visibility {
    Public,           // pub
    PublicCrate,      // pub(crate)
    PublicSuper,      // pub(super)
    PublicIn(String), // pub(in path)
    Private,          // no pub keyword
}

/// A tag extracted from Rust source code
#[derive(Debug, Clone)]
pub struct RustTag {
    pub name: String,
    pub kind: TagKind,
    pub line_number: usize,
    pub visibility: Visibility,
    pub scope: Option<String>,
    pub file_path: String,
}

impl RustTag {
    fn new(
        name: String,
        kind: TagKind,
        line_number: usize,
        visibility: Visibility,
        file_path: String,
    ) -> Self {
        RustTag {
            name,
            kind,
            line_number,
            visibility,
            scope: None,
            file_path,
        }
    }
}

/// Simple token for parsing
#[derive(Debug, Clone, PartialEq)]
enum Token {
    Keyword(String),
    Ident(String),
    Punct(char),
    Whitespace,
    Comment,
    String,
    Char,
    Number,
    Eof,
}

/// Lightweight tokenizer for Rust source
struct Tokenizer {
    chars: Vec<char>,
    pos: usize,
    line: usize,
}

impl Tokenizer {
    fn new(source: &str) -> Self {
        Tokenizer {
            chars: source.chars().collect(),
            pos: 0,
            line: 1,
        }
    }

    fn current(&self) -> char {
        self.chars.get(self.pos).copied().unwrap_or('\0')
    }

    fn peek(&self, offset: usize) -> char {
        self.chars.get(self.pos + offset).copied().unwrap_or('\0')
    }

    fn advance(&mut self) -> char {
        let ch = self.current();
        if ch == '\n' {
            self.line += 1;
        }
        self.pos += 1;
        ch
    }

    fn skip_whitespace(&mut self) {
        while self.current().is_whitespace() {
            self.advance();
        }
    }

    fn skip_line_comment(&mut self) {
        // Skip //
        self.advance();
        self.advance();
        while self.current() != '\n' && self.current() != '\0' {
            self.advance();
        }
    }

    fn skip_block_comment(&mut self) {
        // Skip /*
        self.advance();
        self.advance();
        while !(self.current() == '*' && self.peek(1) == '/') && self.current() != '\0' {
            self.advance();
        }
        // Skip */
        if self.current() == '*' {
            self.advance();
            self.advance();
        }
    }

    fn read_string(&mut self) {
        let quote = self.advance(); // Skip opening quote
        while self.current() != quote && self.current() != '\0' {
            if self.current() == '\\' {
                self.advance(); // Skip escape
                if self.current() != '\0' {
                    self.advance();
                }
            } else {
                self.advance();
            }
        }
        if self.current() == quote {
            self.advance(); // Skip closing quote
        }
    }

    fn read_char(&mut self) {
        self.advance(); // Skip opening '
        if self.current() == '\\' {
            self.advance(); // Skip escape
            if self.current() != '\0' {
                self.advance();
            }
        } else if self.current() != '\0' {
            self.advance();
        }
        if self.current() == '\'' {
            self.advance(); // Skip closing '
        }
    }

    fn read_ident(&mut self) -> String {
        let start = self.pos;
        while self.current().is_alphanumeric() || self.current() == '_' {
            self.advance();
        }
        self.chars[start..self.pos].iter().collect()
    }

    fn read_number(&mut self) {
        while self.current().is_numeric() || self.current() == '_' || self.current() == '.' {
            self.advance();
        }
        // Skip type suffix (e.g., u32, i64, f64)
        if self.current().is_alphabetic() {
            while self.current().is_alphanumeric() {
                self.advance();
            }
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let ch = self.current();
        if ch == '\0' {
            return Token::Eof;
        }

        // Comments
        if ch == '/' && self.peek(1) == '/' {
            self.skip_line_comment();
            return Token::Comment;
        }
        if ch == '/' && self.peek(1) == '*' {
            self.skip_block_comment();
            return Token::Comment;
        }

        // Strings
        if ch == '"' {
            self.read_string();
            return Token::String;
        }

        // Raw strings (r#"..."#, r##"..."##, etc.)
        if ch == 'r' && self.peek(1) == '#' {
            while self.current() == 'r' || self.current() == '#' {
                self.advance();
            }
            if self.current() == '"' {
                self.read_string();
            }
            return Token::String;
        }

        // Chars
        if ch == '\'' {
            self.read_char();
            return Token::Char;
        }

        // Numbers
        if ch.is_numeric() {
            self.read_number();
            return Token::Number;
        }

        // Identifiers and keywords
        if ch.is_alphabetic() || ch == '_' {
            let ident = self.read_ident();
            return if is_keyword(&ident) {
                Token::Keyword(ident)
            } else {
                Token::Ident(ident)
            };
        }

        // Punctuation
        self.advance();
        Token::Punct(ch)
    }

    fn current_line(&self) -> usize {
        self.line
    }
}

/// Check if a string is a Rust keyword
fn is_keyword(s: &str) -> bool {
    matches!(
        s,
        "pub" | "fn" | "struct" | "enum" | "trait" | "impl" | "const" | "static" | "type"
            | "mod" | "use" | "let" | "mut" | "if" | "else" | "match" | "for" | "while"
            | "loop" | "return" | "break" | "continue" | "as" | "in" | "crate" | "self"
            | "super" | "async" | "await" | "unsafe" | "extern" | "where" | "ref" | "move"
    )
}

/// Parse visibility modifier
fn parse_visibility(_tokenizer: &mut Tokenizer, tokens: &mut Vec<(Token, usize)>) -> Visibility {
    if tokens.is_empty() {
        return Visibility::Private;
    }

    if let Token::Keyword(kw) = &tokens[0].0 {
        if kw == "pub" {
            tokens.remove(0); // Consume 'pub'

            // Check for pub(...)
            if !tokens.is_empty() {
                if let Token::Punct('(') = tokens[0].0 {
                    tokens.remove(0); // Consume '('

                    if !tokens.is_empty() {
                        match &tokens[0].0 {
                            Token::Ident(path) if path == "crate" => {
                                tokens.remove(0); // Consume 'crate'
                                if !tokens.is_empty() && tokens[0].0 == Token::Punct(')') {
                                    tokens.remove(0); // Consume ')'
                                }
                                return Visibility::PublicCrate;
                            }
                            Token::Keyword(kw) if kw == "super" => {
                                tokens.remove(0); // Consume 'super'
                                if !tokens.is_empty() && tokens[0].0 == Token::Punct(')') {
                                    tokens.remove(0); // Consume ')'
                                }
                                return Visibility::PublicSuper;
                            }
                            Token::Keyword(kw) if kw == "in" => {
                                tokens.remove(0); // Consume 'in'
                                                  // Read path
                                let mut path = String::new();
                                while !tokens.is_empty() && tokens[0].0 != Token::Punct(')') {
                                    if let Token::Ident(id) = &tokens[0].0 {
                                        path.push_str(id);
                                    } else if let Token::Punct(p) = &tokens[0].0 {
                                        path.push(*p);
                                    }
                                    tokens.remove(0);
                                }
                                if !tokens.is_empty() && tokens[0].0 == Token::Punct(')') {
                                    tokens.remove(0); // Consume ')'
                                }
                                return Visibility::PublicIn(path);
                            }
                            _ => {}
                        }
                    }

                    // Consume rest of pub(...) if we didn't match above
                    while !tokens.is_empty() && tokens[0].0 != Token::Punct(')') {
                        tokens.remove(0);
                    }
                    if !tokens.is_empty() && tokens[0].0 == Token::Punct(')') {
                        tokens.remove(0);
                    }
                }
            }

            return Visibility::Public;
        }
    }

    Visibility::Private
}

/// Parse a single top-level item
fn parse_item(
    _tokenizer: &mut Tokenizer,
    tokens: &mut Vec<(Token, usize)>,
    file_path: &str,
) -> Option<RustTag> {
    // Parse visibility
    let visibility = parse_visibility(_tokenizer, tokens);

    if tokens.is_empty() {
        return None;
    }

    // Look for item keyword
    let (item_token, line_number) = tokens[0].clone();
    if let Token::Keyword(kw) = &item_token {
        match kw.as_str() {
            "fn" => {
                tokens.remove(0); // Consume 'fn'
                if !tokens.is_empty() {
                    if let Token::Ident(name) = &tokens[0].0 {
                        let name = name.clone();
                        return Some(RustTag::new(
                            name,
                            TagKind::Function,
                            line_number,
                            visibility,
                            file_path.to_string(),
                        ));
                    }
                }
            }
            "struct" => {
                tokens.remove(0); // Consume 'struct'
                if !tokens.is_empty() {
                    if let Token::Ident(name) = &tokens[0].0 {
                        let name = name.clone();
                        return Some(RustTag::new(
                            name,
                            TagKind::Struct,
                            line_number,
                            visibility,
                            file_path.to_string(),
                        ));
                    }
                }
            }
            "enum" => {
                tokens.remove(0); // Consume 'enum'
                if !tokens.is_empty() {
                    if let Token::Ident(name) = &tokens[0].0 {
                        let name = name.clone();
                        return Some(RustTag::new(
                            name,
                            TagKind::Enum,
                            line_number,
                            visibility,
                            file_path.to_string(),
                        ));
                    }
                }
            }
            "trait" => {
                tokens.remove(0); // Consume 'trait'
                if !tokens.is_empty() {
                    if let Token::Ident(name) = &tokens[0].0 {
                        let name = name.clone();
                        return Some(RustTag::new(
                            name,
                            TagKind::Trait,
                            line_number,
                            visibility,
                            file_path.to_string(),
                        ));
                    }
                }
            }
            "const" => {
                tokens.remove(0); // Consume 'const'
                if !tokens.is_empty() {
                    if let Token::Ident(name) = &tokens[0].0 {
                        let name = name.clone();
                        return Some(RustTag::new(
                            name,
                            TagKind::Constant,
                            line_number,
                            visibility,
                            file_path.to_string(),
                        ));
                    }
                }
            }
            "static" => {
                tokens.remove(0); // Consume 'static'
                if !tokens.is_empty() {
                    if let Token::Ident(name) = &tokens[0].0 {
                        let name = name.clone();
                        return Some(RustTag::new(
                            name,
                            TagKind::Static,
                            line_number,
                            visibility,
                            file_path.to_string(),
                        ));
                    }
                }
            }
            "type" => {
                tokens.remove(0); // Consume 'type'
                if !tokens.is_empty() {
                    if let Token::Ident(name) = &tokens[0].0 {
                        let name = name.clone();
                        return Some(RustTag::new(
                            name,
                            TagKind::TypeAlias,
                            line_number,
                            visibility,
                            file_path.to_string(),
                        ));
                    }
                }
            }
            "mod" => {
                tokens.remove(0); // Consume 'mod'
                if !tokens.is_empty() {
                    if let Token::Ident(name) = &tokens[0].0 {
                        let name = name.clone();
                        return Some(RustTag::new(
                            name,
                            TagKind::Module,
                            line_number,
                            visibility,
                            file_path.to_string(),
                        ));
                    }
                }
            }
            "impl" => {
                tokens.remove(0); // Consume 'impl'
                                  // For now, skip impl blocks (Phase 2)
                return None;
            }
            _ => {}
        }
    }

    None
}

/// Parse an item from a token slice (zero-copy version)
/// Returns (RustTag, tokens_consumed) if successful
fn parse_item_slice(tokens: &[(Token, usize)], file_path: &str) -> Option<(RustTag, usize)> {
    if tokens.is_empty() {
        return None;
    }

    // Parse visibility
    let (visibility, mut idx) = match &tokens[0].0 {
        Token::Keyword(kw) if kw == "pub" => {
            // Check for pub(crate), pub(super), pub(in path)
            if tokens.len() > 1 {
                if let Token::Punct('(') = &tokens[1].0 {
                    // Skip for MVP - treat as pub
                    (Visibility::Public, 1)
                } else {
                    (Visibility::Public, 1)
                }
            } else {
                (Visibility::Public, 1)
            }
        }
        _ => (Visibility::Private, 0),
    };

    if idx >= tokens.len() {
        return None;
    }

    // Parse item kind and name
    let line_number = tokens[idx].1;

    if let Token::Keyword(kw) = &tokens[idx].0 {
        let kw_str = kw.as_str();
        idx += 1; // Consume keyword

        if idx >= tokens.len() {
            return None;
        }

        let (kind, name) = match kw_str {
            "fn" => {
                if let Token::Ident(name) = &tokens[idx].0 {
                    (TagKind::Function, name.clone())
                } else {
                    return None;
                }
            }
            "struct" => {
                if let Token::Ident(name) = &tokens[idx].0 {
                    (TagKind::Struct, name.clone())
                } else {
                    return None;
                }
            }
            "enum" => {
                if let Token::Ident(name) = &tokens[idx].0 {
                    (TagKind::Enum, name.clone())
                } else {
                    return None;
                }
            }
            "trait" => {
                if let Token::Ident(name) = &tokens[idx].0 {
                    (TagKind::Trait, name.clone())
                } else {
                    return None;
                }
            }
            "const" => {
                if let Token::Ident(name) = &tokens[idx].0 {
                    (TagKind::Constant, name.clone())
                } else {
                    return None;
                }
            }
            "static" => {
                if let Token::Ident(name) = &tokens[idx].0 {
                    (TagKind::Static, name.clone())
                } else {
                    return None;
                }
            }
            "type" => {
                if let Token::Ident(name) = &tokens[idx].0 {
                    (TagKind::TypeAlias, name.clone())
                } else {
                    return None;
                }
            }
            "mod" => {
                if let Token::Ident(name) = &tokens[idx].0 {
                    (TagKind::Module, name.clone())
                } else {
                    return None;
                }
            }
            "impl" => {
                // Skip impl blocks for Phase 1 MVP
                return None;
            }
            _ => return None,
        };

        let tag = RustTag::new(name, kind, line_number, visibility, file_path.to_string());
        idx += 1; // Consume name
        Some((tag, idx))
    } else {
        None
    }
}

/// Parse a Rust file and extract tags
pub fn parse_rust_file(file_path: &Path) -> Result<Vec<RustTag>, String> {
    let (tags, _content) = parse_rust_file_with_content(file_path)?;
    Ok(tags)
}

/// Parse a Rust file and return both tags and the file content
///
/// This is more efficient than calling parse_rust_file() followed by reading the file again,
/// as it returns the content that was already read during parsing.
pub fn parse_rust_file_with_content(file_path: &Path) -> Result<(Vec<RustTag>, String), String> {
    let source = fs::read_to_string(file_path)
        .map_err(|e| format!("Failed to read {}: {}", file_path.display(), e))?;

    let mut tokenizer = Tokenizer::new(&source);
    let mut tags = Vec::new();

    // Tokenize entire file first (simple approach for MVP)
    let mut all_tokens = Vec::new();
    loop {
        let line = tokenizer.current_line();
        let token = tokenizer.next_token();

        if token == Token::Eof {
            break;
        }

        // Skip comments and whitespace
        if token == Token::Comment || token == Token::Whitespace {
            continue;
        }

        all_tokens.push((token, line));
    }

    // Parse items from token stream (zero-copy approach using slices)
    let mut i = 0;
    while i < all_tokens.len() {
        if let Some((tag, consumed)) = parse_item_slice(&all_tokens[i..], file_path.to_str().unwrap()) {
            tags.push(tag);
            i += consumed.max(1);
        } else {
            i += 1;
        }
    }

    Ok((tags, source))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenizer_basic() {
        let mut tokenizer = Tokenizer::new("pub fn foo() {}");
        assert_eq!(tokenizer.next_token(), Token::Keyword("pub".to_string()));
        tokenizer.skip_whitespace();
        assert_eq!(tokenizer.next_token(), Token::Keyword("fn".to_string()));
        tokenizer.skip_whitespace();
        assert_eq!(tokenizer.next_token(), Token::Ident("foo".to_string()));
    }

    #[test]
    fn test_parse_simple_function() {
        let source = "pub fn test() {}\n";
        let mut tokenizer = Tokenizer::new(source);

        let mut tokens = vec![
            (Token::Keyword("pub".to_string()), 1),
            (Token::Keyword("fn".to_string()), 1),
            (Token::Ident("test".to_string()), 1),
        ];

        let tag = parse_item(&mut tokenizer, &mut tokens, "test.rs");
        assert!(tag.is_some());
        let tag = tag.unwrap();
        assert_eq!(tag.name, "test");
        assert_eq!(tag.kind, TagKind::Function);
        assert_eq!(tag.visibility, Visibility::Public);
    }

    #[test]
    fn test_parse_struct() {
        let source = "struct Foo {}\n";
        let mut tokenizer = Tokenizer::new(source);

        let mut tokens = vec![
            (Token::Keyword("struct".to_string()), 1),
            (Token::Ident("Foo".to_string()), 1),
        ];

        let tag = parse_item(&mut tokenizer, &mut tokens, "test.rs");
        assert!(tag.is_some());
        let tag = tag.unwrap();
        assert_eq!(tag.name, "Foo");
        assert_eq!(tag.kind, TagKind::Struct);
        assert_eq!(tag.visibility, Visibility::Private);
    }
}
