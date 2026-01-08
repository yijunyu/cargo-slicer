//! Phase 3: AST parsing using syn.
//!
//! This module contains the core parsing logic for analyzing Rust crate source code.

use std::collections::HashSet;
use std::fs;
use std::path::Path;

use quote::ToTokens;
use syn::{self, Item, Type, visit::Visit};

use super::types::{ParsedItem, ParsedItemKind, CrateIndex, ExtractedDeps, UseStatement, InternalAlias};
use super::arch::{escape_keyword, should_skip_arch_file};
use super::usage::find_rust_files;

/// Visitor to extract type and function references from syn AST
pub struct TypeRefVisitor<'a> {
    pub types: HashSet<String>,
    pub functions: HashSet<String>,
    pub external_crates: HashSet<String>,
    pub known_crates: &'a HashSet<String>,
}

#[allow(dead_code)]
impl<'a> TypeRefVisitor<'a> {
    pub fn with_known_crates(known_crates: &'a HashSet<String>) -> Self {
        Self {
            types: HashSet::new(),
            functions: HashSet::new(),
            external_crates: HashSet::new(),
            known_crates,
        }
    }

    fn is_external_crate_ref(&self, name: &str) -> bool {
        if matches!(name, "self" | "super" | "crate" | "Self") {
            return false;
        }
        if is_primitive_type(name) {
            return false;
        }
        if matches!(name,
            "debug_assert" | "debug_assert_eq" | "debug_assert_ne" |
            "assert" | "assert_eq" | "assert_ne" |
            "macro_rules" | "cfg_attr" | "track_caller" |
            "thread_local" | "format" | "println" | "eprintln" |
            "vec" | "panic" | "unreachable" | "todo" | "unimplemented"
        ) {
            return false;
        }
        if self.known_crates.contains(name) {
            return true;
        }
        false
    }

    fn extract_type_name(ty: &Type) -> Option<String> {
        match ty {
            Type::Path(type_path) => {
                type_path.path.segments.last().map(|seg| seg.ident.to_string())
            }
            Type::Reference(type_ref) => Self::extract_type_name(&type_ref.elem),
            Type::Slice(type_slice) => Self::extract_type_name(&type_slice.elem),
            Type::Array(type_array) => Self::extract_type_name(&type_array.elem),
            Type::Ptr(type_ptr) => Self::extract_type_name(&type_ptr.elem),
            Type::Tuple(type_tuple) => {
                type_tuple.elems.first().and_then(Self::extract_type_name)
            }
            _ => None,
        }
    }
}

impl<'a, 'ast> Visit<'ast> for TypeRefVisitor<'a> {
    fn visit_type(&mut self, ty: &'ast Type) {
        if let Some(name) = Self::extract_type_name(ty) {
            if !is_primitive_type(&name) {
                self.types.insert(name);
            }
        }
        syn::visit::visit_type(self, ty);
    }

    fn visit_path(&mut self, path: &'ast syn::Path) {
        if let Some(first) = path.segments.first() {
            let first_name = first.ident.to_string();
            if self.is_external_crate_ref(&first_name) {
                self.external_crates.insert(first_name);
            }
        }

        for segment in &path.segments {
            let name = segment.ident.to_string();
            if !is_primitive_type(&name) {
                if name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                    self.types.insert(name);
                } else {
                    self.functions.insert(name);
                }
            }
        }
        syn::visit::visit_path(self, path);
    }

    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if let syn::Expr::Path(expr_path) = &*call.func {
            if let Some(last) = expr_path.path.segments.last() {
                let name = last.ident.to_string();
                if !is_primitive_type(&name) {
                    self.functions.insert(name);
                }
            }
        }
        syn::visit::visit_expr_call(self, call);
    }

    fn visit_expr_method_call(&mut self, method: &'ast syn::ExprMethodCall) {
        let name = method.method.to_string();
        if !is_primitive_type(&name) {
            self.functions.insert(name);
        }
        syn::visit::visit_expr_method_call(self, method);
    }

    fn visit_type_param_bound(&mut self, bound: &'ast syn::TypeParamBound) {
        if let syn::TypeParamBound::Trait(trait_bound) = bound {
            if let Some(last) = trait_bound.path.segments.last() {
                let name = last.ident.to_string();
                if !is_primitive_type(&name) {
                    self.types.insert(name);
                }
            }
        }
        syn::visit::visit_type_param_bound(self, bound);
    }

    fn visit_item_impl(&mut self, item_impl: &'ast syn::ItemImpl) {
        if let Some((_, trait_path, _)) = &item_impl.trait_ {
            if let Some(last) = trait_path.segments.last() {
                let name = last.ident.to_string();
                if !is_primitive_type(&name) {
                    self.types.insert(name);
                }
            }
        }
        syn::visit::visit_item_impl(self, item_impl);
    }

    fn visit_item_trait(&mut self, item_trait: &'ast syn::ItemTrait) {
        for bound in &item_trait.supertraits {
            if let syn::TypeParamBound::Trait(trait_bound) = bound {
                if let Some(last) = trait_bound.path.segments.last() {
                    let name = last.ident.to_string();
                    if !is_primitive_type(&name) {
                        self.types.insert(name);
                    }
                }
            }
        }
        syn::visit::visit_item_trait(self, item_trait);
    }

    fn visit_pat(&mut self, pat: &'ast syn::Pat) {
        match pat {
            syn::Pat::TupleStruct(tuple_struct) => {
                for segment in &tuple_struct.path.segments {
                    let name = segment.ident.to_string();
                    if !is_primitive_type(&name) && name != "crate" && name != "self" && name != "super" {
                        if name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                            self.types.insert(name);
                        }
                    }
                }
            }
            syn::Pat::Path(pat_path) => {
                for segment in &pat_path.path.segments {
                    let name = segment.ident.to_string();
                    if !is_primitive_type(&name) && name != "crate" && name != "self" && name != "super" {
                        if name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                            self.types.insert(name);
                        }
                    }
                }
            }
            syn::Pat::Struct(pat_struct) => {
                for segment in &pat_struct.path.segments {
                    let name = segment.ident.to_string();
                    if !is_primitive_type(&name) && name != "crate" && name != "self" && name != "super" {
                        if name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                            self.types.insert(name);
                        }
                    }
                }
            }
            _ => {}
        }
        syn::visit::visit_pat(self, pat);
    }

    fn visit_expr(&mut self, expr: &'ast syn::Expr) {
        if let syn::Expr::Path(expr_path) = expr {
            let segments: Vec<_> = expr_path.path.segments.iter().collect();
            for (i, segment) in segments.iter().enumerate() {
                let name = segment.ident.to_string();
                if !is_primitive_type(&name) && name != "crate" && name != "self" && name != "super" {
                    let is_last = i == segments.len() - 1;
                    if name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                        if !is_last || segments.len() == 1 {
                            self.types.insert(name);
                        }
                    }
                }
            }
        }
        syn::visit::visit_expr(self, expr);
    }

    fn visit_macro(&mut self, mac: &'ast syn::Macro) {
        // Extract macro name to include macro_rules! definitions as dependencies
        if let Some(last) = mac.path.segments.last() {
            let name = last.ident.to_string();
            // Skip common std/built-in macros
            let skip = matches!(name.as_str(),
                "vec" | "format" | "println" | "eprintln" | "print" | "eprint" |
                "write" | "writeln" | "panic" | "unreachable" | "todo" | "unimplemented" |
                "assert" | "assert_eq" | "assert_ne" |
                "debug_assert" | "debug_assert_eq" | "debug_assert_ne" |
                "cfg" | "cfg_attr" | "include" | "include_str" | "include_bytes" |
                "env" | "option_env" | "concat" | "stringify" | "line" | "column" | "file" |
                "module_path" | "compile_error" | "matches" | "thread_local" |
                "macro_rules" | "dbg" | "try" | "const_format_args" | "format_args" |
                "log" | "trace" | "debug" | "info" | "warn" | "error"
            );
            if !skip && !is_primitive_type(&name) {
                self.functions.insert(name);
            }
        }
        syn::visit::visit_macro(self, mac);
    }
}

/// Check if a type name is a Rust primitive or std trait
pub fn is_primitive_type(name: &str) -> bool {
    matches!(name,
        "bool" | "char" | "str" | "String" |
        "i8" | "i16" | "i32" | "i64" | "i128" | "isize" |
        "u8" | "u16" | "u32" | "u64" | "u128" | "usize" |
        "f32" | "f64" |
        "Vec" | "Option" | "Result" | "Box" | "Rc" | "Arc" |
        "HashMap" | "HashSet" | "BTreeMap" | "BTreeSet" |
        "Cell" | "RefCell" | "Mutex" | "RwLock" | "Cow" |
        "PhantomData" | "NonNull" | "UnsafeCell" |
        "Clone" | "Copy" | "Debug" | "Display" | "Default" |
        "PartialEq" | "Eq" | "PartialOrd" | "Ord" | "Hash" |
        "From" | "Into" | "TryFrom" | "TryInto" | "AsRef" | "AsMut" |
        "Deref" | "DerefMut" | "Drop" | "Sized" | "Send" | "Sync" |
        "Iterator" | "IntoIterator" | "FromIterator" | "Extend" |
        "Read" | "Write" | "Seek" | "BufRead" |
        "Add" | "Sub" | "Mul" | "Div" | "Rem" | "Neg" |
        "AddAssign" | "SubAssign" | "MulAssign" | "DivAssign" |
        "BitAnd" | "BitOr" | "BitXor" | "Not" | "Shl" | "Shr" |
        "Index" | "IndexMut" | "Fn" | "FnMut" | "FnOnce" |
        "ToOwned" | "ToString" | "FromStr" | "Error" |
        "Future" | "Stream" | "Sink" | "Unpin" |
        "Self" | "self"
    )
}

/// Extract dependencies from a syn Item
pub fn extract_dependencies(item: &Item, _item_name: &str, known_crates: &HashSet<String>) -> ExtractedDeps {
    let mut visitor = TypeRefVisitor::with_known_crates(known_crates);
    visitor.visit_item(item);

    let mut deps = visitor.types;
    deps.extend(visitor.functions);

    if let Item::Macro(m) = item {
        let macro_path = m.mac.path.segments.iter()
            .map(|s| s.ident.to_string())
            .collect::<Vec<_>>()
            .join("::");

        if macro_path == "thread_local" {
            let tokens_str = m.mac.tokens.to_string();
            for token in tokens_str.split(|c: char| !c.is_alphanumeric() && c != '_') {
                let token = token.trim();
                if token.is_empty() {
                    continue;
                }
                let keywords = ["static", "let", "mut", "ref", "const", "fn", "pub", "use", "mod", "struct", "enum", "impl", "trait", "type", "where", "for", "in", "if", "else", "match", "loop", "while", "return", "break", "continue", "move", "async", "await", "unsafe", "dyn"];
                if keywords.contains(&token) {
                    continue;
                }
                if token.chars().all(|c| c.is_uppercase() || c == '_') && token.len() > 1 {
                    deps.insert(token.to_string());
                } else if token.chars().next().map(|c| c.is_uppercase()).unwrap_or(false)
                    && !token.chars().all(|c| c.is_uppercase() || c == '_')
                {
                    deps.insert(token.to_string());
                }
            }
        }
    }

    ExtractedDeps {
        internal: deps,
        external_crates: visitor.external_crates,
    }
}

/// Extract source code for an item
pub fn extract_source_from_span(item: &Item, _source_content: &str) -> String {
    let tokens = item.to_token_stream();
    fix_quote_formatting(&tokens.to_string())
}

/// Fix common formatting issues from quote's tokenization
pub fn fix_quote_formatting(source: &str) -> String {
    let mut source = source.replace("# [", "#[");
    source = source.replace("# !", "#!");
    source = source.replace("derive (", "derive(");
    source = source.replace("doc (", "doc(");
    source = source.replace("cfg (", "cfg(");
    source = source.replace("not (", "not(");
    source = source.replace("allow (", "allow(");
    source = source.replace("inline (", "inline(");
    source
}

/// Extract #[cfg(...)] attribute from a list of attributes
pub fn extract_cfg_attr(attrs: &[syn::Attribute]) -> Option<String> {
    for attr in attrs {
        if attr.path().is_ident("cfg") {
            return Some(attr.to_token_stream().to_string());
        }
    }
    None
}

/// Check if visibility is accessible from within the crate (pub or pub(crate))
fn is_crate_visible(vis: &syn::Visibility) -> bool {
    match vis {
        syn::Visibility::Public(_) => true,
        syn::Visibility::Restricted(r) => {
            // pub(crate) and pub(in crate) are crate-visible
            if let Some(first) = r.path.segments.first() {
                first.ident == "crate"
            } else {
                false
            }
        }
        syn::Visibility::Inherited => false,
    }
}

/// Parse a single item from the AST
pub fn parse_item(item: &Item, module_path: &str, source_content: &str, file: &Path, known_crates: &HashSet<String>) -> Option<ParsedItem> {
    let (name, kind, is_pub, attrs) = match item {
        Item::Struct(s) => (s.ident.to_string(), ParsedItemKind::Struct, is_crate_visible(&s.vis), &s.attrs),
        Item::Enum(e) => (e.ident.to_string(), ParsedItemKind::Enum, is_crate_visible(&e.vis), &e.attrs),
        Item::Fn(f) => (f.sig.ident.to_string(), ParsedItemKind::Function, is_crate_visible(&f.vis), &f.attrs),
        Item::Trait(t) => (t.ident.to_string(), ParsedItemKind::Trait, is_crate_visible(&t.vis), &t.attrs),
        Item::Impl(i) => {
            // Extract a name for the impl block that encodes self type and optional trait
            let self_type_name = match &*i.self_ty {
                Type::Path(tp) => tp.path.segments.last()
                    .map(|s| s.ident.to_string())
                    .unwrap_or_default(),
                Type::Ptr(ptr) => {
                    // Handle *const T and *mut T
                    let inner = match &*ptr.elem {
                        Type::Path(tp) => tp.path.segments.last()
                            .map(|s| s.ident.to_string())
                            .unwrap_or_else(|| "T".to_string()),
                        _ => "T".to_string(),
                    };
                    if ptr.mutability.is_some() {
                        format!("*mut {}", inner)
                    } else {
                        format!("*const {}", inner)
                    }
                }
                Type::Reference(r) => {
                    // Handle &T and &mut T
                    let inner = match &*r.elem {
                        Type::Path(tp) => tp.path.segments.last()
                            .map(|s| s.ident.to_string())
                            .unwrap_or_else(|| "T".to_string()),
                        _ => "T".to_string(),
                    };
                    if r.mutability.is_some() {
                        format!("&mut {}", inner)
                    } else {
                        format!("&{}", inner)
                    }
                }
                Type::Slice(s) => {
                    // Handle [T]
                    let inner = match &*s.elem {
                        Type::Path(tp) => tp.path.segments.last()
                            .map(|s| s.ident.to_string())
                            .unwrap_or_else(|| "T".to_string()),
                        _ => "T".to_string(),
                    };
                    format!("[{}]", inner)
                }
                Type::Tuple(t) if t.elems.is_empty() => "()".to_string(),
                Type::TraitObject(to) => {
                    // Handle dyn Trait (trait object types)
                    // Extract the first trait name
                    let trait_name = to.bounds.iter().find_map(|bound| {
                        if let syn::TypeParamBound::Trait(trait_bound) = bound {
                            trait_bound.path.segments.last().map(|s| s.ident.to_string())
                        } else {
                            None
                        }
                    }).unwrap_or_else(|| "dyn".to_string());
                    format!("dyn {}", trait_name)
                }
                _ => return None,
            };

            // If this implements a trait, include the trait name
            let type_name = if let Some((_, trait_path, _)) = &i.trait_ {
                let trait_name = trait_path.segments.last()
                    .map(|s| s.ident.to_string())
                    .unwrap_or_default();
                format!("{}:{}", self_type_name, trait_name)
            } else {
                self_type_name
            };

            (type_name, ParsedItemKind::Impl, true, &i.attrs)
        }
        Item::Type(t) => (t.ident.to_string(), ParsedItemKind::TypeAlias, is_crate_visible(&t.vis), &t.attrs),
        Item::Const(c) => (c.ident.to_string(), ParsedItemKind::Const, is_crate_visible(&c.vis), &c.attrs),
        Item::Static(s) => (s.ident.to_string(), ParsedItemKind::Static, is_crate_visible(&s.vis), &s.attrs),
        Item::Macro(m) => {
            let macro_name = m.ident.as_ref().map(|i| i.to_string()).unwrap_or_default();
            if macro_name.is_empty() {
                return None;
            }
            (macro_name, ParsedItemKind::Macro, true, &m.attrs)
        }
        Item::Mod(m) => {
            if m.content.is_some() {
                (m.ident.to_string(), ParsedItemKind::Mod, is_crate_visible(&m.vis), &m.attrs)
            } else {
                return None;
            }
        }
        Item::ForeignMod(fm) => {
            // Extract function names from extern block for dependency tracking
            let fn_names: Vec<String> = fm.items.iter().filter_map(|item| {
                if let syn::ForeignItem::Fn(f) = item {
                    Some(f.sig.ident.to_string())
                } else {
                    None
                }
            }).collect();
            // Use first function name or synthetic name for the block
            let name = fn_names.first().cloned().unwrap_or_else(|| "__extern_block".to_string());
            (name, ParsedItemKind::ExternBlock, true, &fm.attrs)
        }
        _ => return None,
    };

    let source = extract_source_from_span(item, source_content);
    let deps = extract_dependencies(item, &name, known_crates);
    let cfg_attr = extract_cfg_attr(attrs);

    let path = format!("{}::{}", module_path, escape_keyword(&name));
    Some(ParsedItem {
        name,
        path,
        kind,
        source,
        dependencies: deps.internal,
        external_crates: deps.external_crates,
        module_path: module_path.to_string(),
        is_pub,
        file: file.to_path_buf(),
        cfg_attr,
    })
}

/// Result of parsing a single file - contains all extracted data
pub struct ParsedFileData {
    pub items: Vec<ParsedItem>,
    pub use_statements: Vec<UseStatement>,
    pub internal_aliases: Vec<InternalAlias>,
}

/// Parse a single file once and extract all data (items, use statements, aliases)
/// This is more efficient than calling parse_rust_file, extract_use_statements,
/// and extract_internal_aliases separately as it reads and parses the file only once.
pub fn parse_file_all(
    path: &Path,
    module_path: &str,
    known_crates: &HashSet<String>,
) -> ParsedFileData {
    let mut result = ParsedFileData {
        items: Vec::new(),
        use_statements: Vec::new(),
        internal_aliases: Vec::new(),
    };

    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => return result,
    };

    let syntax = match syn::parse_file(&content) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("  Warning: Failed to parse {}: {}", path.display(), e);
            return result;
        }
    };

    // Extract all data in a single pass through syntax.items
    for item in &syntax.items {
        // Extract parsed items (including inline modules)
        let is_inline_module = matches!(item, syn::Item::Mod(m) if m.content.is_some());

        if let Some(parsed) = parse_item(item, module_path, &content, path, known_crates) {
            result.items.push(parsed);
        }

        // Recursively parse inline module contents
        if is_inline_module {
            if let syn::Item::Mod(m) = item {
                if let Some((_, ref inline_items)) = m.content {
                    let inline_module_path = if module_path.is_empty() {
                        m.ident.to_string()
                    } else {
                        format!("{}::{}", module_path, m.ident)
                    };
                    for inline_item in inline_items {
                        if let Some(parsed) = parse_item(inline_item, &inline_module_path, &content, path, known_crates) {
                            result.items.push(parsed);
                        }
                        parse_inline_module_recursive(inline_item, &inline_module_path, &content, path, known_crates, &mut result.items);
                    }
                }
            }
        }

        // Extract use statements (external only)
        if let syn::Item::Use(use_item) = item {
            if let Some(stmt) = parse_use_item(use_item, &content, known_crates, module_path) {
                result.use_statements.push(stmt);
            }
            // Extract internal aliases
            if !is_external_use(&use_item.tree, known_crates) {
                collect_internal_aliases(&use_item.tree, "", &mut result.internal_aliases);
            }
        }
    }

    result
}

/// Parse a single Rust source file and extract all items
pub fn parse_rust_file(path: &Path, module_path: &str, known_crates: &HashSet<String>) -> Vec<ParsedItem> {
    let mut items = Vec::new();

    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => return items,
    };

    let syntax = match syn::parse_file(&content) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("  Warning: Failed to parse {}: {}", path.display(), e);
            return items;
        }
    };

    for item in &syntax.items {
        // For inline modules, we extract their contents separately
        // We also parse the module declaration itself to capture cfg attributes
        let is_inline_module = matches!(item, syn::Item::Mod(m) if m.content.is_some());

        // Parse all items, including inline modules (to capture their cfg_attr)
        if let Some(parsed) = parse_item(item, module_path, &content, path, known_crates) {
            items.push(parsed);
        }

        // Recursively parse inline module contents
        if is_inline_module {
            if let syn::Item::Mod(m) = item {
                if let Some((_, ref inline_items)) = m.content {
                    let inline_module_path = if module_path.is_empty() {
                        m.ident.to_string()
                    } else {
                        format!("{}::{}", module_path, m.ident)
                    };
                    for inline_item in inline_items {
                        if let Some(parsed) = parse_item(inline_item, &inline_module_path, &content, path, known_crates) {
                            items.push(parsed);
                        }
                        // Recurse into nested inline modules
                        parse_inline_module_recursive(inline_item, &inline_module_path, &content, path, known_crates, &mut items);
                    }
                }
            }
        }
    }

    items
}

/// Recursively parse nested inline modules
fn parse_inline_module_recursive(
    item: &syn::Item,
    module_path: &str,
    source_content: &str,
    file: &Path,
    known_crates: &HashSet<String>,
    items: &mut Vec<ParsedItem>,
) {
    if let syn::Item::Mod(m) = item {
        if let Some((_, ref inline_items)) = m.content {
            let inline_module_path = format!("{}::{}", module_path, m.ident);

            // Also parse the module declaration itself to capture cfg_attr
            if let Some(parsed) = parse_item(item, module_path, source_content, file, known_crates) {
                items.push(parsed);
            }

            for inline_item in inline_items {
                if let Some(parsed) = parse_item(inline_item, &inline_module_path, source_content, file, known_crates) {
                    items.push(parsed);
                }
                // Continue recursion
                parse_inline_module_recursive(inline_item, &inline_module_path, source_content, file, known_crates, items);
            }
        }
    }
}

/// Extract use statements from a Rust source file
pub fn extract_use_statements(path: &Path, known_crates: &HashSet<String>, module_path: &str) -> Vec<UseStatement> {
    let mut statements = Vec::new();

    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => return statements,
    };

    let syntax = match syn::parse_file(&content) {
        Ok(s) => s,
        Err(_) => return statements,
    };

    for item in &syntax.items {
        if let syn::Item::Use(use_item) = item {
            if let Some(stmt) = parse_use_item(use_item, &content, known_crates, module_path) {
                statements.push(stmt);
            }
        }
    }

    statements
}

/// Parse a single use item into a UseStatement
fn parse_use_item(use_item: &syn::ItemUse, _source: &str, known_crates: &HashSet<String>, module_path: &str) -> Option<UseStatement> {
    // Get the source code for this use statement
    let use_source = use_item.to_token_stream().to_string();
    let use_source = fix_quote_formatting(&use_source);

    // Normalize the source (remove extra spaces from syn's output)
    let use_source = normalize_use_source(&use_source);

    // Extract the symbols imported by this statement
    let symbols = extract_symbols_from_use(&use_item.tree);

    // Determine if this is an external import
    let is_external = is_external_use(&use_item.tree, known_crates);

    // Skip internal imports entirely - let generate_internal_imports handle them
    // Internal imports may have invalid paths after slicing (e.g., re-exports not included)
    // Even aliases like `use crate::Foo as Bar` won't work if the path is invalid
    if !is_external {
        return None;
    }

    Some(UseStatement {
        source: use_source,
        symbols,
        is_external,
        module_path: module_path.to_string(),
    })
}

/// Normalize use statement source (fix syn's extra spacing)
fn normalize_use_source(source: &str) -> String {
    source
        .replace(" :: ", "::")
        .replace(" ::", "::")
        .replace(":: ", "::")
        .replace(" < ", "<")
        .replace("< ", "<")
        .replace(" <", "<")
        .replace(" > ", ">")
        .replace("> ", ">")
        .replace(" >", ">")
        .replace(" ;", ";")
}

/// Extract symbols from a use tree
fn extract_symbols_from_use(tree: &syn::UseTree) -> Vec<String> {
    let mut symbols = Vec::new();
    collect_use_symbols(tree, &mut symbols);
    symbols
}

/// Recursively collect symbols from a use tree
fn collect_use_symbols(tree: &syn::UseTree, symbols: &mut Vec<String>) {
    match tree {
        syn::UseTree::Name(name) => {
            symbols.push(name.ident.to_string());
        }
        syn::UseTree::Rename(rename) => {
            // use foo as bar; - the local name is 'bar'
            symbols.push(rename.rename.to_string());
        }
        syn::UseTree::Glob(_) => {
            // use foo::*; - we can't know what's imported
            symbols.push("*".to_string());
        }
        syn::UseTree::Path(path) => {
            collect_use_symbols(&path.tree, symbols);
        }
        syn::UseTree::Group(group) => {
            for tree in &group.items {
                collect_use_symbols(tree, symbols);
            }
        }
    }
}

/// Check if a use statement imports from an external crate
fn is_external_use(tree: &syn::UseTree, known_crates: &HashSet<String>) -> bool {
    // Get the root path segment
    let root = get_use_root(tree);

    // Check if it's a known external crate or std/core/alloc
    matches!(root.as_str(), "std" | "core" | "alloc") || known_crates.contains(&root)
}

/// Get the root segment of a use tree
fn get_use_root(tree: &syn::UseTree) -> String {
    match tree {
        syn::UseTree::Path(path) => path.ident.to_string(),
        syn::UseTree::Name(name) => name.ident.to_string(),
        syn::UseTree::Rename(rename) => rename.ident.to_string(),
        syn::UseTree::Glob(_) => String::new(),
        syn::UseTree::Group(_) => String::new(),
    }
}

/// Extract internal import aliases from a file (e.g., `use crate::Foo as Bar`)
/// Returns a list of (alias, original_name, source_path) tuples
pub fn extract_internal_aliases(path: &Path, known_crates: &std::collections::HashSet<String>) -> Vec<super::types::InternalAlias> {
    let mut aliases = Vec::new();

    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => return aliases,
    };

    let syntax = match syn::parse_file(&content) {
        Ok(s) => s,
        Err(_) => return aliases,
    };

    for item in &syntax.items {
        if let syn::Item::Use(use_item) = item {
            // Only process internal (crate/self/super) imports
            if !is_external_use(&use_item.tree, known_crates) {
                // Extract the path prefix and collect aliases
                collect_internal_aliases(&use_item.tree, "", &mut aliases);
            }
        }
    }

    aliases
}

/// Recursively collect internal aliases from a use tree
fn collect_internal_aliases(tree: &syn::UseTree, prefix: &str, aliases: &mut Vec<super::types::InternalAlias>) {
    match tree {
        syn::UseTree::Rename(rename) => {
            // Found an alias: `Foo as Bar`
            let original = rename.ident.to_string();
            let alias = rename.rename.to_string();
            let source_path = if prefix.is_empty() {
                original.clone()
            } else {
                format!("{}::{}", prefix, original)
            };
            aliases.push(super::types::InternalAlias {
                alias,
                original,
                source_path,
            });
        }
        syn::UseTree::Path(path) => {
            // Continue down the path
            let new_prefix = if prefix.is_empty() {
                path.ident.to_string()
            } else {
                format!("{}::{}", prefix, path.ident)
            };
            collect_internal_aliases(&path.tree, &new_prefix, aliases);
        }
        syn::UseTree::Group(group) => {
            // Process each item in the group
            for tree in &group.items {
                collect_internal_aliases(tree, prefix, aliases);
            }
        }
        syn::UseTree::Name(_) | syn::UseTree::Glob(_) => {
            // Not an alias, skip
        }
    }
}

/// Extract known external crates from Cargo.toml
pub fn extract_known_crates(crate_path: &Path) -> HashSet<String> {
    let mut crates = HashSet::new();
    let cargo_path = crate_path.join("Cargo.toml");

    if let Ok(content) = fs::read_to_string(&cargo_path) {
        let mut in_deps = false;
        for line in content.lines() {
            let trimmed = line.trim();

            // Handle [dependencies] section
            if trimmed.starts_with("[dependencies]") || trimmed.starts_with("[dev-dependencies]") {
                in_deps = true;
                continue;
            }

            // Handle [dependencies.crate_name] format (normalized Cargo.toml)
            if trimmed.starts_with("[dependencies.") {
                if let Some(name) = trimmed.strip_prefix("[dependencies.") {
                    let name = name.trim_end_matches(']').replace('-', "_");
                    if !name.is_empty() {
                        crates.insert(name);
                    }
                }
                in_deps = false;
                continue;
            }

            // Handle [dev-dependencies.crate_name] format
            if trimmed.starts_with("[dev-dependencies.") {
                if let Some(name) = trimmed.strip_prefix("[dev-dependencies.") {
                    let name = name.trim_end_matches(']').replace('-', "_");
                    if !name.is_empty() {
                        crates.insert(name);
                    }
                }
                in_deps = false;
                continue;
            }

            if trimmed.starts_with('[') {
                in_deps = false;
                continue;
            }
            if in_deps {
                if let Some(name) = trimmed.split('=').next() {
                    let name = name.trim().replace('-', "_");
                    if !name.is_empty() && !name.starts_with('#') {
                        crates.insert(name);
                    }
                }
            }
        }
    }

    crates
}

/// Parse all source files in a crate and build an index
pub fn parse_crate(crate_path: &Path, crate_name: &str) -> CrateIndex {
    let mut index = CrateIndex::new();
    let known_crates = extract_known_crates(crate_path);

    let src_path = crate_path.join("src");
    let search_path = if src_path.exists() { src_path } else { crate_path.to_path_buf() };

    for file in find_rust_files(&search_path) {
        if should_skip_arch_file(&file) {
            continue;
        }

        let module_path = compute_module_path(&file, crate_name);

        // Parse file once and extract all data (3x more efficient than separate calls)
        let parsed = parse_file_all(&file, &module_path, &known_crates);

        for item in parsed.items {
            index.add_item(item);
        }

        for stmt in parsed.use_statements {
            index.add_use_statement(stmt);
        }

        index.add_internal_aliases(&module_path, parsed.internal_aliases);
    }

    index
}

/// Compute the module path for a file
/// For src/arch/generic/memchr.rs, returns "arch::generic::memchr"
/// For src/arch/generic/mod.rs, returns "arch::generic"
/// For src/lib.rs, returns "" (root module)
pub fn compute_module_path(file_path: &Path, _crate_name: &str) -> String {
    // Find the LAST "src" component in the path and compute relative path from there
    // This handles paths like .cargo/registry/src/index.crates.io-.../crate/src/module.rs
    let components: Vec<_> = file_path.components().collect();
    let mut src_idx = None;
    for (i, comp) in components.iter().enumerate() {
        if let std::path::Component::Normal(name) = comp {
            if name.to_str() == Some("src") {
                src_idx = Some(i); // Keep updating to find the LAST src
            }
        }
    }

    let src_idx = match src_idx {
        Some(idx) => idx,
        None => return String::new(), // No src directory found
    };

    // Get path components after "src"
    let mut module_parts: Vec<String> = Vec::new();
    for comp in &components[src_idx + 1..] {
        if let std::path::Component::Normal(name) = comp {
            if let Some(name_str) = name.to_str() {
                module_parts.push(name_str.to_string());
            }
        }
    }

    // Handle lib.rs and mod.rs - remove the filename
    if let Some(last) = module_parts.last() {
        if last == "lib.rs" || last == "mod.rs" {
            module_parts.pop();
        } else if last.ends_with(".rs") {
            // Replace "foo.rs" with "foo"
            if let Some(stem) = last.strip_suffix(".rs") {
                let len = module_parts.len();
                module_parts[len - 1] = escape_keyword(stem);
            }
        }
    }

    // Join with :: and escape keywords
    let escaped_parts: Vec<String> = module_parts.iter()
        .map(|p| escape_keyword(p))
        .collect();

    escaped_parts.join("::")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::TempDir;

    /// Test that extract_known_crates handles normalized Cargo.toml format
    /// with [dependencies.crate_name] sections.
    /// This test would FAIL on the original implementation that only parsed
    /// the simple format "crate = version" under [dependencies].
    #[test]
    fn test_extract_known_crates_normalized_format() {
        let temp_dir = TempDir::new().unwrap();
        let cargo_toml = temp_dir.path().join("Cargo.toml");

        // Write normalized Cargo.toml format (as produced by cargo publish)
        let content = r#"
[package]
name = "test"
version = "0.1.0"

[dependencies.once_cell]
version = "1.9.0"

[dependencies.bytes]
version = "1.0"
optional = true

[dev-dependencies.tempfile]
version = "3.2"
"#;
        std::fs::write(&cargo_toml, content).unwrap();

        let crates = extract_known_crates(temp_dir.path());

        // This would fail on the old implementation that didn't parse
        // [dependencies.crate_name] format
        assert!(crates.contains("once_cell"), "once_cell should be extracted from normalized format");
        assert!(crates.contains("bytes"), "bytes should be extracted from normalized format");
        assert!(crates.contains("tempfile"), "tempfile should be extracted from dev-dependencies normalized format");
    }

    /// Test that extract_known_crates handles simple Cargo.toml format
    #[test]
    fn test_extract_known_crates_simple_format() {
        let temp_dir = TempDir::new().unwrap();
        let cargo_toml = temp_dir.path().join("Cargo.toml");

        // Write simple Cargo.toml format
        let content = r#"
[package]
name = "test"
version = "0.1.0"

[dependencies]
once_cell = "1.9.0"
some-crate = "1.0"

[dev-dependencies]
tempfile = "3.2"
"#;
        std::fs::write(&cargo_toml, content).unwrap();

        let crates = extract_known_crates(temp_dir.path());

        assert!(crates.contains("once_cell"), "once_cell should be extracted");
        assert!(crates.contains("some_crate"), "some-crate should be normalized to some_crate");
        assert!(crates.contains("tempfile"), "tempfile should be extracted from dev-dependencies");
    }

    /// Test that extract_known_crates handles mixed format (both simple and normalized)
    #[test]
    fn test_extract_known_crates_mixed_format() {
        let temp_dir = TempDir::new().unwrap();
        let cargo_toml = temp_dir.path().join("Cargo.toml");

        let content = r#"
[package]
name = "test"
version = "0.1.0"

[dependencies]
serde = "1.0"

[dependencies.once_cell]
version = "1.9.0"

[dependencies.protobuf-support]
version = "3.7.2"
"#;
        std::fs::write(&cargo_toml, content).unwrap();

        let crates = extract_known_crates(temp_dir.path());

        assert!(crates.contains("serde"), "serde from simple format");
        assert!(crates.contains("once_cell"), "once_cell from normalized format");
        assert!(crates.contains("protobuf_support"), "protobuf-support normalized to protobuf_support");
    }
}
