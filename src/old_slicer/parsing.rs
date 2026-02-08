//! Phase 3: AST parsing using syn.
//!
//! This module contains the core parsing logic for analyzing Rust crate source code.

use std::collections::{HashSet, HashMap};
use std::fs;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};

use quote::ToTokens;
use syn::{self, Item, Type, visit::Visit};

use crate::types::{ParsedItem, ParsedItemKind, CrateIndex, ExtractedDeps, UseStatement, InternalAlias, TypeDependency, DependencyContext};
use crate::common::arch::{escape_keyword, should_skip_arch_file};
use crate::usage::find_rust_files;

/// Global flag for verbose mode (shows parsing warnings)
static VERBOSE_MODE: AtomicBool = AtomicBool::new(false);

/// Enable verbose mode (shows parsing warnings)
pub fn set_verbose(enabled: bool) {
    VERBOSE_MODE.store(enabled, Ordering::Relaxed);
}

/// Check if verbose mode is enabled
fn is_verbose() -> bool {
    VERBOSE_MODE.load(Ordering::Relaxed)
}

/// Visitor to extract type and function references from syn AST
pub struct TypeRefVisitor<'a> {
    pub types: HashSet<String>,
    pub functions: HashSet<String>,
    pub external_crates: HashSet<String>,
    pub known_crates: &'a HashSet<String>,
    /// Phase 6.3: Type dependencies with context for transitive closure
    pub typed_dependencies: Vec<TypeDependency>,
}

#[allow(dead_code)]
impl<'a> TypeRefVisitor<'a> {
    pub fn with_known_crates(known_crates: &'a HashSet<String>) -> Self {
        Self {
            types: HashSet::new(),
            functions: HashSet::new(),
            external_crates: HashSet::new(),
            known_crates,
            typed_dependencies: Vec::new(),
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
            "vec" | "panic" | "unreachable" | "todo" | "unimplemented" |
            "loom"  // Exclude loom - used in cfg-guarded macros, not needed as dependency
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

    /// Phase 6.3: Add a typed dependency with context
    fn add_typed_dependency(&mut self, typename: String, context: DependencyContext) {
        if !is_primitive_type(&typename) {
            self.typed_dependencies.push(TypeDependency::new_required(typename, context));
        }
    }

    // Helper to extract all types from a function signature
    // RULE-001 implementation helper
    fn extract_signature_types(&mut self, sig: &syn::Signature) {
        // 1. Extract from parameters (including nested/wrapped types)
        // Phase 6.3: Track with FunctionParameter context
        for input in &sig.inputs {
            match input {
                syn::FnArg::Receiver(_) => {
                    // self parameters don't introduce type dependencies
                }
                syn::FnArg::Typed(pat_type) => {
                    self.extract_type_with_context(&pat_type.ty, DependencyContext::FunctionParameter);
                    // Also extract with old method for backward compatibility
                    self.extract_type_recursively(&pat_type.ty);
                }
            }
        }

        // 2. Extract from return type
        // Phase 6.3: Track with FunctionReturn context
        if let syn::ReturnType::Type(_, return_type) = &sig.output {
            self.extract_type_with_context(return_type, DependencyContext::FunctionReturn);
            // Also extract with old method for backward compatibility
            self.extract_type_recursively(return_type);
        }

        // 3. Extract from generic parameters and bounds
        for param in &sig.generics.params {
            match param {
                syn::GenericParam::Type(type_param) => {
                    // Extract types from trait bounds
                    for bound in &type_param.bounds {
                        if let syn::TypeParamBound::Trait(trait_bound) = bound {
                            // Extract trait name
                            if let Some(last) = trait_bound.path.segments.last() {
                                let name = last.ident.to_string();
                                if !is_primitive_type(&name) {
                                    self.types.insert(name);
                                }
                            }
                            // Extract associated type bindings (e.g., Iterator<Item = T>)
                            for segment in &trait_bound.path.segments {
                                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                                    for arg in &args.args {
                                        match arg {
                                            syn::GenericArgument::AssocType(assoc) => {
                                                self.extract_type_recursively(&assoc.ty);
                                            }
                                            syn::GenericArgument::Type(ty) => {
                                                self.extract_type_recursively(ty);
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                syn::GenericParam::Lifetime(_) => {
                    // Lifetimes don't introduce type dependencies
                }
                syn::GenericParam::Const(_) => {
                    // Const generics might have type dependencies
                    // But usually primitive, can expand if needed
                }
            }
        }

        // 4. Extract from where clause
        if let Some(where_clause) = &sig.generics.where_clause {
            for predicate in &where_clause.predicates {
                if let syn::WherePredicate::Type(predicate_type) = predicate {
                    // Extract the bounded type
                    self.extract_type_recursively(&predicate_type.bounded_ty);
                    // Extract types from bounds
                    for bound in &predicate_type.bounds {
                        if let syn::TypeParamBound::Trait(trait_bound) = bound {
                            if let Some(last) = trait_bound.path.segments.last() {
                                let name = last.ident.to_string();
                                if !is_primitive_type(&name) {
                                    self.types.insert(name);
                                }
                            }
                            // Extract associated type bindings
                            for segment in &trait_bound.path.segments {
                                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                                    for arg in &args.args {
                                        match arg {
                                            syn::GenericArgument::AssocType(assoc) => {
                                                self.extract_type_recursively(&assoc.ty);
                                            }
                                            syn::GenericArgument::Type(ty) => {
                                                self.extract_type_recursively(ty);
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Phase 6.3: Extract types with context tracking
    fn extract_type_with_context(&mut self, ty: &syn::Type, context: DependencyContext) {
        match ty {
            syn::Type::Path(type_path) => {
                // Extract main type name
                if let Some(last) = type_path.path.segments.last() {
                    let name = last.ident.to_string();
                    if !is_primitive_type(&name) {
                        self.types.insert(name.clone());
                        self.add_typed_dependency(name, context.clone());
                    }

                    // Extract types from generic arguments
                    if let syn::PathArguments::AngleBracketed(args) = &last.arguments {
                        for arg in &args.args {
                            match arg {
                                syn::GenericArgument::Type(inner_ty) => {
                                    self.extract_type_with_context(inner_ty, DependencyContext::GenericConstraint);
                                }
                                syn::GenericArgument::AssocType(assoc) => {
                                    self.extract_type_with_context(&assoc.ty, DependencyContext::AssociatedType);
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            syn::Type::Reference(type_ref) => {
                self.extract_type_with_context(&type_ref.elem, context);
            }
            syn::Type::Ptr(type_ptr) => {
                self.extract_type_with_context(&type_ptr.elem, context);
            }
            syn::Type::Slice(type_slice) => {
                self.extract_type_with_context(&type_slice.elem, context);
            }
            syn::Type::Array(type_array) => {
                self.extract_type_with_context(&type_array.elem, context);
            }
            syn::Type::Tuple(type_tuple) => {
                for elem in &type_tuple.elems {
                    self.extract_type_with_context(elem, context.clone());
                }
            }
            syn::Type::TraitObject(trait_object) => {
                for bound in &trait_object.bounds {
                    if let syn::TypeParamBound::Trait(trait_bound) = bound {
                        if let Some(last) = trait_bound.path.segments.last() {
                            let name = last.ident.to_string();
                            if !is_primitive_type(&name) {
                                self.types.insert(name.clone());
                                self.add_typed_dependency(name, DependencyContext::TraitBound);
                            }
                        }
                    }
                }
            }
            syn::Type::ImplTrait(impl_trait) => {
                for bound in &impl_trait.bounds {
                    if let syn::TypeParamBound::Trait(trait_bound) = bound {
                        if let Some(last) = trait_bound.path.segments.last() {
                            let name = last.ident.to_string();
                            if !is_primitive_type(&name) {
                                self.types.insert(name.clone());
                                self.add_typed_dependency(name, DependencyContext::TraitBound);
                            }
                        }
                        // Extract associated type bindings
                        for segment in &trait_bound.path.segments {
                            if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                                for arg in &args.args {
                                    match arg {
                                        syn::GenericArgument::AssocType(assoc) => {
                                            self.extract_type_with_context(&assoc.ty, DependencyContext::AssociatedType);
                                        }
                                        syn::GenericArgument::Type(ty) => {
                                            self.extract_type_with_context(ty, DependencyContext::GenericConstraint);
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                }
            }
            _ => {
                // For other types, delegate to the old method
            }
        }
    }

    // Helper to recursively extract types, including through wrappers
    // RULE-001 implementation helper
    fn extract_type_recursively(&mut self, ty: &syn::Type) {
        match ty {
            syn::Type::Path(type_path) => {
                // Extract main type name
                if let Some(last) = type_path.path.segments.last() {
                    let name = last.ident.to_string();
                    if !is_primitive_type(&name) {
                        self.types.insert(name);
                    }

                    // Extract types from generic arguments
                    if let syn::PathArguments::AngleBracketed(args) = &last.arguments {
                        for arg in &args.args {
                            match arg {
                                syn::GenericArgument::Type(inner_ty) => {
                                    self.extract_type_recursively(inner_ty);
                                }
                                syn::GenericArgument::AssocType(assoc) => {
                                    self.extract_type_recursively(&assoc.ty);
                                }
                                syn::GenericArgument::AssocConst(_) => {
                                    // Const associations don't introduce type dependencies
                                }
                                syn::GenericArgument::Constraint(constraint) => {
                                    for bound in &constraint.bounds {
                                        if let syn::TypeParamBound::Trait(trait_bound) = bound {
                                            if let Some(last) = trait_bound.path.segments.last() {
                                                let name = last.ident.to_string();
                                                if !is_primitive_type(&name) {
                                                    self.types.insert(name);
                                                }
                                            }
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            syn::Type::Reference(type_ref) => {
                // Recurse through reference
                self.extract_type_recursively(&type_ref.elem);
            }
            syn::Type::Ptr(type_ptr) => {
                // Recurse through pointer
                self.extract_type_recursively(&type_ptr.elem);
            }
            syn::Type::Slice(type_slice) => {
                // Recurse through slice
                self.extract_type_recursively(&type_slice.elem);
            }
            syn::Type::Array(type_array) => {
                // Recurse through array
                self.extract_type_recursively(&type_array.elem);
            }
            syn::Type::Tuple(type_tuple) => {
                // Recurse through tuple elements
                for elem in &type_tuple.elems {
                    self.extract_type_recursively(elem);
                }
            }
            syn::Type::Paren(type_paren) => {
                // Recurse through parentheses
                self.extract_type_recursively(&type_paren.elem);
            }
            syn::Type::Group(type_group) => {
                // Recurse through group
                self.extract_type_recursively(&type_group.elem);
            }
            syn::Type::TraitObject(trait_object) => {
                // Extract types from trait bounds
                for bound in &trait_object.bounds {
                    if let syn::TypeParamBound::Trait(trait_bound) = bound {
                        if let Some(last) = trait_bound.path.segments.last() {
                            let name = last.ident.to_string();
                            if !is_primitive_type(&name) {
                                self.types.insert(name);
                            }
                        }
                    }
                }
            }
            syn::Type::ImplTrait(impl_trait) => {
                // Extract types from trait bounds
                for bound in &impl_trait.bounds {
                    if let syn::TypeParamBound::Trait(trait_bound) = bound {
                        if let Some(last) = trait_bound.path.segments.last() {
                            let name = last.ident.to_string();
                            if !is_primitive_type(&name) {
                                self.types.insert(name);
                            }
                        }
                        // Extract associated type bindings
                        for segment in &trait_bound.path.segments {
                            if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                                for arg in &args.args {
                                    match arg {
                                        syn::GenericArgument::AssocType(assoc) => {
                                            self.extract_type_recursively(&assoc.ty);
                                        }
                                        syn::GenericArgument::Type(ty) => {
                                            self.extract_type_recursively(ty);
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                }
            }
            _ => {
                // For other types, we've covered the main cases
                // Default visitor will handle the rest
            }
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

    // RULE-001: Extract ALL types from function signatures
    // Phase 3 Option E - Phase 4: Rule Encoding
    // Ensures types used in function signatures are included in dependencies
    fn visit_item_fn(&mut self, item_fn: &'ast syn::ItemFn) {
        // Extract all types from function signature comprehensively
        self.extract_signature_types(&item_fn.sig);

        // Continue with default visitor to handle function body
        syn::visit::visit_item_fn(self, item_fn);
    }

    // RULE-003: Extract ALL types from extern function signatures (FFI boundary)
    // Phase 3 Option E - Phase 4: Rule Encoding
    // Similar to RULE-001 but for extern "C" / extern "system" functions
    fn visit_item_foreign_mod(&mut self, foreign_mod: &'ast syn::ItemForeignMod) {
        for item in &foreign_mod.items {
            if let syn::ForeignItem::Fn(foreign_fn) = item {
                // Extract all types from extern function signature
                self.extract_signature_types(&foreign_fn.sig);
            }
            // Static items in extern blocks also have types
            else if let syn::ForeignItem::Static(foreign_static) = item {
                self.extract_type_recursively(&foreign_static.ty);
            }
        }

        // Continue with default visitor
        syn::visit::visit_item_foreign_mod(self, foreign_mod);
    }

    fn visit_path(&mut self, path: &'ast syn::Path) {
        if let Some(first) = path.segments.first() {
            let first_name = first.ident.to_string();
            if self.is_external_crate_ref(&first_name) {
                self.external_crates.insert(first_name);
            }
        }

        // RULE-004: Extract enum names from paths like EnumName::VARIANT
        // Heuristic: If a lowercase segment is followed by an uppercase segment,
        // the first segment is likely an enum or type with constants
        let segments: Vec<_> = path.segments.iter().collect();
        for (i, segment) in segments.iter().enumerate() {
            let name = segment.ident.to_string();
            if !is_primitive_type(&name) {
                let is_uppercase = name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false);
                let is_lowercase = name.chars().next().map(|c| c.is_lowercase()).unwrap_or(false);

                // Check if next segment exists and is uppercase
                let next_is_uppercase = if i + 1 < segments.len() {
                    segments[i + 1].ident.to_string()
                        .chars().next().map(|c| c.is_uppercase()).unwrap_or(false)
                } else {
                    false
                };

                if is_uppercase {
                    // Uppercase names are types
                    self.types.insert(name);
                } else if is_lowercase && next_is_uppercase {
                    // Lowercase followed by uppercase: likely enum or type
                    // e.g., nfsstat::NFSERR_WFLUSH
                    self.types.insert(name);
                } else if is_lowercase {
                    // Lowercase without uppercase following: function or module
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

    fn visit_expr_struct(&mut self, expr_struct: &'ast syn::ExprStruct) {
        // Extract struct type name from struct literal expressions like `ThreadIndices { ... }`
        if let Some(last) = expr_struct.path.segments.last() {
            let name = last.ident.to_string();
            if !is_primitive_type(&name) {
                // Extracted type from struct literal
                self.types.insert(name);
            }
        }
        // Continue with default visitor behavior to visit field expressions
        syn::visit::visit_expr_struct(self, expr_struct);
    }

    fn visit_item_static(&mut self, item_static: &'ast syn::ItemStatic) {
        // Phase 6.4: Extract type dependencies from static variable's type with context
        // This ensures types used in statics (e.g., Align64<[bool; 128]>) are included
        self.extract_type_with_context(&item_static.ty, DependencyContext::StaticType);
        // Also extract with old method for backward compatibility
        self.extract_type_recursively(&item_static.ty);

        // Visit the initializer expression
        syn::visit::visit_expr(self, &item_static.expr);
        // Call default implementation to visit other parts
        syn::visit::visit_item_static(self, item_static);
    }

    fn visit_stmt(&mut self, stmt: &'ast syn::Stmt) {
        // Visit statements to catch function-local statics and other nested items
        if let syn::Stmt::Item(item) = stmt {
            // Visit nested items (including static declarations inside functions)
            syn::visit::visit_item(self, item);
        }
        // Continue with default visitor behavior
        syn::visit::visit_stmt(self, stmt);
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
        // Extract the type being implemented (the Self type)
        // For `impl Buffer { ... }` or `impl Trait for Buffer { ... }`
        self.extract_type_recursively(&item_impl.self_ty);

        // Extract trait name for trait impls
        if let Some((_, trait_path, _)) = &item_impl.trait_ {
            if let Some(last) = trait_path.segments.last() {
                let name = last.ident.to_string();
                if !is_primitive_type(&name) {
                    self.types.insert(name);
                }
            }
        }

        // RULE-001: Extract types from method signatures in impl blocks
        for item in &item_impl.items {
            if let syn::ImplItem::Fn(impl_fn) = item {
                self.extract_signature_types(&impl_fn.sig);
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

        // RULE-001: Extract types from method signatures in trait definitions
        for item in &item_trait.items {
            if let syn::TraitItem::Fn(trait_fn) = item {
                self.extract_signature_types(&trait_fn.sig);
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

/// Convert a syn::Type to a string representation
fn type_to_string(ty: &syn::Type) -> String {
    use quote::ToTokens;
    ty.to_token_stream().to_string()
}

/// Extract full trait path from a trait bound
/// e.g., std::fmt::Debug -> "std::fmt::Debug"
fn extract_full_trait_path(trait_bound: &syn::TraitBound) -> String {
    trait_bound.path.segments.iter()
        .map(|seg| seg.ident.to_string())
        .collect::<Vec<_>>()
        .join("::")
}

/// Extract generic arguments from a trait bound
/// Returns: (generic_types, associated_types)
/// e.g., Iterator<Item=String> -> ([], {Item: String})
fn extract_generic_args(trait_bound: &syn::TraitBound) -> (Vec<String>, HashMap<String, String>) {
    let mut generic_types = Vec::new();
    let mut associated_types = HashMap::new();

    if let Some(last_segment) = trait_bound.path.segments.last() {
        if let syn::PathArguments::AngleBracketed(args) = &last_segment.arguments {
            for arg in &args.args {
                match arg {
                    syn::GenericArgument::Type(ty) => {
                        generic_types.push(type_to_string(ty));
                    }
                    syn::GenericArgument::Constraint(constraint) => {
                        let name = constraint.ident.to_string();
                        // For now, just record the constraint exists
                        // In the future, we could parse the bounds more deeply
                        associated_types.insert(name, "constraint".to_string());
                    }
                    syn::GenericArgument::AssocType(assoc_type) => {
                        let name = assoc_type.ident.to_string();
                        let value = type_to_string(&assoc_type.ty);
                        associated_types.insert(name, value);
                    }
                    _ => {}
                }
            }
        }
    }

    (generic_types, associated_types)
}

/// Extract generic information from where clause
fn extract_where_clause_bounds(where_clause: &Option<syn::WhereClause>) -> Vec<crate::types::GenericInfo> {
    let mut generics = Vec::new();

    if let Some(where_clause) = where_clause {
        for predicate in &where_clause.predicates {
            if let syn::WherePredicate::Type(type_predicate) = predicate {
                let param_name = type_to_string(&type_predicate.bounded_ty);

                let mut trait_bounds = Vec::new();
                let mut associated_types = HashMap::new();
                let mut bound_generics = Vec::new();

                for bound in &type_predicate.bounds {
                    if let syn::TypeParamBound::Trait(trait_bound) = bound {
                        let trait_path = extract_full_trait_path(trait_bound);
                        trait_bounds.push(trait_path);

                        let (gen_types, assoc) = extract_generic_args(trait_bound);
                        bound_generics.extend(gen_types);
                        associated_types.extend(assoc);
                    }
                }

                generics.push(crate::types::GenericInfo {
                    param_name,
                    trait_bounds,
                    associated_types,
                    bound_generics,
                });
            }
        }
    }

    generics
}

/// Extract generic parameter information from an item's generics
fn extract_generic_params(generics: &syn::Generics) -> Vec<crate::types::GenericInfo> {
    let mut result = Vec::new();

    // Extract from generic parameter declarations
    for param in &generics.params {
        if let syn::GenericParam::Type(type_param) = param {
            let param_name = type_param.ident.to_string();
            let mut trait_bounds = Vec::new();
            let mut associated_types = HashMap::new();
            let mut bound_generics = Vec::new();

            for bound in &type_param.bounds {
                if let syn::TypeParamBound::Trait(trait_bound) = bound {
                    let trait_path = extract_full_trait_path(trait_bound);
                    trait_bounds.push(trait_path);

                    let (gen_types, assoc) = extract_generic_args(trait_bound);
                    bound_generics.extend(gen_types);
                    associated_types.extend(assoc);
                }
            }

            result.push(crate::types::GenericInfo {
                param_name,
                trait_bounds,
                associated_types,
                bound_generics,
            });
        }
    }

    // Also extract from where clause
    result.extend(extract_where_clause_bounds(&generics.where_clause));

    result
}

/// Extract complete generic information from an item
pub fn extract_generic_info(item: &Item) -> Vec<crate::types::GenericInfo> {
    match item {
        Item::Fn(func) => extract_generic_params(&func.sig.generics),
        Item::Struct(s) => extract_generic_params(&s.generics),
        Item::Enum(e) => extract_generic_params(&e.generics),
        Item::Trait(t) => extract_generic_params(&t.generics),
        Item::Impl(i) => extract_generic_params(&i.generics),
        Item::Type(t) => extract_generic_params(&t.generics),
        _ => Vec::new(),
    }
}

/// Extract dependencies from a syn Item
pub fn extract_dependencies(item: &Item, _item_name: &str, known_crates: &HashSet<String>) -> ExtractedDeps {
    let mut visitor = TypeRefVisitor::with_known_crates(known_crates);
    visitor.visit_item(item);

    let mut deps = visitor.types;
    deps.extend(visitor.functions);

    // Extract dependencies from macro_rules! definitions
    // Look specifically for turbofish generic function calls like can_transmute::<T, U>()
    let item_str = item.to_token_stream().to_string();
    if item_str.starts_with("macro_rules!") {
        // Use a more specific pattern: function_name followed by ::<
        // This catches generic function calls like can_transmute::<T, U>()
        if let Ok(re) = regex::Regex::new(r"\b([a-z_][a-z0-9_]*)\s*::\s*<") {
            for cap in re.captures_iter(&item_str) {
                if let Some(func_name) = cap.get(1) {
                    let name = func_name.as_str();
                    // More extensive keyword filter
                    let keywords = [
                        "if", "for", "while", "match", "let", "mut", "ref", "fn",
                        "unsafe", "loop", "const", "static", "impl", "trait", "type",
                        "as", "in", "where", "dyn"
                    ];
                    if !keywords.contains(&name) {
                        deps.insert(name.to_string());
                    }
                }
            }
        }
    } else if let Item::Macro(m) = item {
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

    // Phase 2: Extract trait bounds from generic parameters
    let generic_info = extract_generic_info(item);
    for gen in &generic_info {
        // Add trait bounds to dependencies
        for trait_bound in &gen.trait_bounds {
            // Extract just the trait name (last segment)
            if let Some(trait_name) = trait_bound.split("::").last() {
                if !is_primitive_type(trait_name) {
                    deps.insert(trait_name.to_string());
                }
            }
        }

        // Add associated type values to dependencies
        for (_assoc_name, assoc_value) in &gen.associated_types {
            // Extract type names from associated type values
            // e.g., "String" or "Vec<T>" - extract the main type
            if let Some(type_name) = assoc_value.split(&['<', '>', ' ', ','][..]).next() {
                if !type_name.is_empty() && !is_primitive_type(type_name) {
                    deps.insert(type_name.to_string());
                }
            }
        }

        // Add generic argument types to dependencies
        for bound_generic in &gen.bound_generics {
            // Extract type names from bound generics
            if let Some(type_name) = bound_generic.split(&['<', '>', ' ', ','][..]).next() {
                if !type_name.is_empty() && !is_primitive_type(type_name) {
                    deps.insert(type_name.to_string());
                }
            }
        }
    }

    ExtractedDeps {
        internal: deps,
        external_crates: visitor.external_crates,
        typed_dependencies: visitor.typed_dependencies,
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

    // Fix E0034 fmt ambiguity in Debug/Display trait impls
    // When quote! converts impl blocks, method calls like `.fmt(f)` become ambiguous
    // if the type implements both Debug and Display traits
    source = fix_fmt_ambiguity(&source);

    source
}

/// Fix E0034 ambiguity for fmt method calls in Debug/Display trait impls
/// Replaces `.fmt(f)` with qualified trait method calls to avoid ambiguity
fn fix_fmt_ambiguity(source: &str) -> String {
    use regex::Regex;
    use once_cell::sync::Lazy;

    // Pattern to match complete impl blocks for Debug/Display
    // Captures: (1) generics+bounds, (2) trait name, (3) type signature, (4) body content
    static IMPL_FMT_PATTERN: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"impl\s+([^{]*?)\bfmt\s*::\s*(Debug|Display)\s+for\s+([^{]+)\{(.*?)\}\s*\}").unwrap()
    });

    // Pattern to match method calls: <expr> . fmt ( <param> )
    // We want to match things like: err.fmt(f), msg.fmt(f), self.key.fmt(f)
    // But NOT: fn fmt(&self, ...) or Type::Path.fmt (like fmt::Error.fmt)
    // Key insight: we only want to replace .fmt( when it's followed by a typical
    // formatter parameter name (f, formatter, _f, etc.)
    static FMT_CALL: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"((?:\w+\s*\.\s*)*\w+)\s*\.\s*fmt\s*\(\s*([f_]\w*)\s*\)").unwrap()
    });

    let result = IMPL_FMT_PATTERN.replace_all(source, |caps: &regex::Captures| {
        let generics = &caps[1];   // Generic parameters like "<T>" or "<'a, T: Bound>"
        let trait_name = &caps[2]; // "Debug" or "Display"
        let type_sig = &caps[3];   // Type signature like "Key<'k>"
        let body = &caps[4];       // Body content

        // Fix fmt calls in the body, but skip the method definition line
        let fixed_body = if body.contains("fn fmt") {
            // Split into parts: before fn fmt, fn fmt signature, after fn fmt
            let parts: Vec<&str> = body.split("fn fmt").collect();
            if parts.len() >= 2 {
                let before = parts[0];
                let rest = parts[1..].join("fn fmt");

                // Fix fmt calls in the rest (after "fn fmt" definition)
                // Find where the method body starts (after the ->)
                if let Some(arrow_pos) = rest.find("->") {
                    if let Some(brace_pos) = rest[arrow_pos..].find('{') {
                        let signature_part = &rest[..arrow_pos + brace_pos + 1];
                        let method_body = &rest[arrow_pos + brace_pos + 1..];

                        // Fix fmt calls in method body
                        let fixed_method_body = FMT_CALL.replace_all(method_body, |c: &regex::Captures| {
                            let var = &c[1];
                            let param = &c[2];
                            format!("{}::fmt(&{}, {})", trait_name, var, param)
                        });

                        format!("{}fn fmt{}{}", before, signature_part, fixed_method_body)
                    } else {
                        body.to_string()
                    }
                } else {
                    body.to_string()
                }
            } else {
                body.to_string()
            }
        } else {
            // No "fn fmt" found, just fix all fmt calls
            FMT_CALL.replace_all(body, |c: &regex::Captures| {
                let var = &c[1];
                let param = &c[2];
                format!("{}::fmt(&{}, {})", trait_name, var, param)
            }).to_string()
        };

        format!("impl {}fmt::{}  for {}{{ {} }} }}", generics, trait_name, type_sig, fixed_body)
    });

    result.to_string()
}

/// Fix E0425 MAX_LEVEL_INNER error by adding default definition when it's missing
/// The log crate uses cfg_if! macro to define MAX_LEVEL_INNER, but this macro invocation
/// isn't included in sliced output. Add a default definition when STATIC_MAX_LEVEL uses it.
pub fn fix_max_level_inner(source: &str) -> String {
    // Check if STATIC_MAX_LEVEL references MAX_LEVEL_INNER
    if source.contains("STATIC_MAX_LEVEL") && source.contains("MAX_LEVEL_INNER") {
        // Check if MAX_LEVEL_INNER is already defined
        if !source.contains("const MAX_LEVEL_INNER") {
            // Add default definition before STATIC_MAX_LEVEL
            // Use the default case from cfg_if! block: LevelFilter::Trace
            let definition = "\n// Default MAX_LEVEL_INNER definition (from cfg_if! else branch)\nconst MAX_LEVEL_INNER: LevelFilter = LevelFilter::Trace;\n\n";

            // Find STATIC_MAX_LEVEL and insert before it
            if let Some(pos) = source.find("pub const STATIC_MAX_LEVEL") {
                let mut result = String::with_capacity(source.len() + definition.len());
                result.push_str(&source[..pos]);
                result.push_str(definition);
                result.push_str(&source[pos..]);
                return result;
            }
        }
    }
    source.to_string()
}

/// Fix E0433 ArrayVecImpl errors by adding missing trait import
/// The arrayvec crate uses ArrayVecImpl trait with qualified syntax (ArrayVecImpl::push),
/// but when arrayvec.rs is inlined into lib.rs, the internal import is lost.
/// This function adds the import when ArrayVecImpl is used but not imported.
pub fn fix_arrayvec_impl_import(source: &str) -> String {
    fix_missing_internal_import(source, "ArrayVecImpl", "arrayvec_impl")
}

/// Fix E0433 MakeMaybeUninit errors by adding missing struct import
/// The arrayvec crate uses MakeMaybeUninit struct with qualified syntax (MakeMaybeUninit::ARRAY),
/// but when arrayvec.rs is inlined into lib.rs, the internal import is lost.
/// This function adds the import when MakeMaybeUninit is used but not imported.
pub fn fix_make_maybe_uninit_import(source: &str) -> String {
    fix_missing_internal_import(source, "MakeMaybeUninit", "utils")
}

/// Generic function to fix missing internal imports
/// Adds `use crate::module::Type;` when Type is used but not imported
fn fix_missing_internal_import(source: &str, type_name: &str, module_name: &str) -> String {
    // Check if the type is used in qualified syntax (with or without spaces before ::)
    let pattern1 = format!("{}::", type_name);
    let pattern2 = format!("{} ::", type_name);

    if source.contains(&pattern1) || source.contains(&pattern2) {
        // Check if the import is already present
        let import_pattern1 = format!("use crate::{}::{}", module_name, type_name);
        let import_pattern2 = format!("use crate :: {} :: {}", module_name, type_name);

        if !source.contains(&import_pattern1) && !source.contains(&import_pattern2) {
            // Find a good insertion point - after other use statements
            // Look for the first use statement and insert after it
            if let Some(first_use_pos) = source.find("\nuse ") {
                // Find the end of the use block by looking for the first non-use line
                let after_first_use = &source[first_use_pos + 1..];

                // Find where use statements end (look for first line that doesn't start with "use" or "pub use")
                let mut insert_pos = first_use_pos + 1;
                for line in after_first_use.lines() {
                    let trimmed = line.trim();
                    if trimmed.starts_with("use ") || trimmed.starts_with("pub use ") || trimmed.is_empty() {
                        insert_pos += line.len() + 1; // +1 for newline
                    } else {
                        break;
                    }
                }

                // Insert the import at the end of the use block
                let import = format!("use crate::{}::{};\n", module_name, type_name);
                let mut result = String::with_capacity(source.len() + import.len());
                result.push_str(&source[..insert_pos]);
                result.push_str(&import);
                result.push_str(&source[insert_pos..]);
                return result;
            }
        }
    }
    source.to_string()
}

/// Fix E0433 module resolution errors by replacing partial module paths
/// Converts paths like `kv::Value` to just `Value` when re-exports are available
/// This function should be called AFTER re-exports are added to the file
pub fn fix_partial_paths_in_generated_code(source: &str) -> String {
    use regex::Regex;
    use once_cell::sync::Lazy;

    // Pattern to detect re-exports at the top of files
    // Match both "pub use crate::..." and "pub use self::..."
    // Use [^:\n] to prevent matching across lines
    static RE_EXPORT: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"pub use (?:crate|self)::(?:[^:\n]+::)*([^;\n:]+);").unwrap()
    });

    // Collect all re-exported type names
    let mut reexported_types = std::collections::HashSet::new();
    for cap in RE_EXPORT.captures_iter(source) {
        if let Some(type_name) = cap.get(1) {
            reexported_types.insert(type_name.as_str().to_string());
        }
    }

    let mut result = source.to_string();

    // List of standard library and common modules that should not be replaced
    let std_modules: std::collections::HashSet<&str> = [
        "crate", "std", "core", "alloc", "self", "super", "fmt", "str",
        "option", "result", "vec", "string", "collections", "ops", "iter",
        "io", "path", "sync", "thread", "time", "error", "hash", "mem",
        "ptr", "slice", "any", "borrow", "boxed", "cell", "char", "cmp",
        "convert", "default", "env", "ffi", "fs", "marker", "net", "num",
        "process", "rc", "task", "prelude", "f32", "f64", "i8", "i16",
        "i32", "i64", "i128", "isize", "u8", "u16", "u32", "u64", "u128", "usize"
    ].iter().copied().collect();

    // Pattern to match module::Type paths
    // Matches: lowercase_word :: UppercaseWord
    static PARTIAL_PATH: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"\b([a-z][a-z0-9_]*)\s*::\s*([A-Z][a-zA-Z0-9_]*)").unwrap()
    });

    result = PARTIAL_PATH.replace_all(&result, |caps: &regex::Captures| {
        let module = &caps[1];  // e.g., "kv"
        let type_name = &caps[2];  // e.g., "Value"

        // Don't replace if it's a standard library module
        if std_modules.contains(module) {
            caps[0].to_string()
        } else if reexported_types.contains(type_name) {
            // If this type is re-exported, use just the type name
            type_name.to_string()
        } else {
            // Keep original if not re-exported
            caps[0].to_string()
        }
    }).to_string();

    result
}

/// Extract #[cfg(...)] attribute from a list of attributes
/// Concatenates all cfg attributes to distinguish between multiple variants
pub fn extract_cfg_attr(attrs: &[syn::Attribute]) -> Option<String> {
    let mut cfg_attrs = Vec::new();
    for attr in attrs {
        if attr.path().is_ident("cfg") {
            cfg_attrs.push(attr.to_token_stream().to_string());
        }
    }
    if cfg_attrs.is_empty() {
        None
    } else {
        // Concatenate all cfg attributes to create unique key
        Some(cfg_attrs.join(" "))
    }
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

/// Extract detailed visibility level from syn::Visibility
fn extract_visibility(vis: &syn::Visibility) -> crate::types::ItemVisibility {
    use crate::types::ItemVisibility;
    match vis {
        syn::Visibility::Public(_) => ItemVisibility::Public,
        syn::Visibility::Restricted(r) => {
            if let Some(first) = r.path.segments.first() {
                let ident_str = first.ident.to_string();
                match ident_str.as_str() {
                    "crate" => ItemVisibility::Crate,
                    "super" => ItemVisibility::Super,
                    _ => {
                        // pub(in path::to::module) - capture the full path
                        let path = r.path.segments.iter()
                            .map(|s| s.ident.to_string())
                            .collect::<Vec<_>>()
                            .join("::");
                        ItemVisibility::InPath(path)
                    }
                }
            } else {
                ItemVisibility::Private
            }
        }
        syn::Visibility::Inherited => ItemVisibility::Private,
    }
}

/// Parse a single item from the AST
pub fn parse_item(item: &Item, module_path: &str, source_content: &str, file: &Path, known_crates: &HashSet<String>) -> Option<ParsedItem> {
    let (name, kind, is_pub, visibility, attrs) = match item {
        Item::Struct(s) => {
            (s.ident.to_string(), ParsedItemKind::Struct, is_crate_visible(&s.vis), extract_visibility(&s.vis), &s.attrs)
        },
        Item::Enum(e) => (e.ident.to_string(), ParsedItemKind::Enum, is_crate_visible(&e.vis), extract_visibility(&e.vis), &e.attrs),
        Item::Fn(f) => (f.sig.ident.to_string(), ParsedItemKind::Function, is_crate_visible(&f.vis), extract_visibility(&f.vis), &f.attrs),
        Item::Trait(t) => (t.ident.to_string(), ParsedItemKind::Trait, is_crate_visible(&t.vis), extract_visibility(&t.vis), &t.attrs),
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

            (type_name, ParsedItemKind::Impl, true, crate::types::ItemVisibility::Public, &i.attrs)
        }
        Item::Type(t) => (t.ident.to_string(), ParsedItemKind::TypeAlias, is_crate_visible(&t.vis), extract_visibility(&t.vis), &t.attrs),
        Item::Const(c) => (c.ident.to_string(), ParsedItemKind::Const, is_crate_visible(&c.vis), extract_visibility(&c.vis), &c.attrs),
        Item::Static(s) => (s.ident.to_string(), ParsedItemKind::Static, is_crate_visible(&s.vis), extract_visibility(&s.vis), &s.attrs),
        Item::Macro(m) => {
            let macro_name = m.ident.as_ref().map(|i| i.to_string()).unwrap_or_default();

            // Handle thread_local! macro without ident - extract static name from tokens
            if macro_name.is_empty() {
                let macro_path = m.mac.path.segments.iter()
                    .map(|s| s.ident.to_string())
                    .collect::<Vec<_>>()
                    .join("::");

                if macro_path == "thread_local" || macro_path == "std::thread_local" {
                    // Extract static name from tokens like: "static REGISTRATION : Registration = { ... }"
                    let tokens_str = m.mac.tokens.to_string();
                    // Look for pattern: "static NAME :"
                    if let Some(static_pos) = tokens_str.find("static") {
                        let after_static = &tokens_str[static_pos + 6..].trim_start();
                        if let Some(colon_pos) = after_static.find(':') {
                            let static_name = after_static[..colon_pos].trim();
                            if !static_name.is_empty() && !static_name.contains(' ') {
                                // Treat thread_local! as a static item for dependency tracking
                                (static_name.to_string(), ParsedItemKind::Static, false, crate::types::ItemVisibility::Private, &m.attrs)
                            } else {
                                return None;
                            }
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                } else {
                    return None;
                }
            } else {
                (macro_name, ParsedItemKind::Macro, true, crate::types::ItemVisibility::Public, &m.attrs)
            }
        }
        Item::Mod(m) => {
            if m.content.is_some() {
                (m.ident.to_string(), ParsedItemKind::Mod, is_crate_visible(&m.vis), extract_visibility(&m.vis), &m.attrs)
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
            (name, ParsedItemKind::ExternBlock, true, crate::types::ItemVisibility::Public, &fm.attrs)
        }
        _ => return None,
    };

    let source = extract_source_from_span(item, source_content);
    let deps = extract_dependencies(item, &name, known_crates);
    let cfg_attr = extract_cfg_attr(attrs);

    // Filter out items with cfg attributes that evaluate to false
    // EXCEPT for macros and inline modules - keep all variants regardless of cfg
    // Macros and inline modules with cfg attributes will be conditionally compiled by rustc
    // Inline modules must be kept because they may contain cfg-gated items that are referenced
    if kind != ParsedItemKind::Macro && kind != ParsedItemKind::Mod {
        if let Some(ref cfg) = cfg_attr {
            use crate::common::cfg_eval::parse_cfg_attribute;
            // Enable "std" feature by default since we're slicing for std environments
            // This ensures helper functions like take_unchecked are included
            let mut enabled_features = std::collections::HashSet::new();
            enabled_features.insert("std".to_string());

            // Split concatenated cfg attributes and evaluate each one
            for single_cfg in cfg.split(" # [cfg") {
                let cfg_to_parse = if single_cfg.starts_with("# [cfg") {
                    single_cfg.to_string()
                } else if !single_cfg.is_empty() {
                    format!("# [cfg{}", single_cfg)
                } else {
                    continue;
                };

                if let Some(cfg_expr) = parse_cfg_attribute(&cfg_to_parse) {
                    if !cfg_expr.evaluate(&enabled_features) {
                        return None; // Skip this item - cfg doesn't match
                    }
                }
            }
        }
    }

    let path = format!("{}::{}", module_path, escape_keyword(&name));

    // Phase 2: Extract generic type parameter information
    let generics = extract_generic_info(&item);

    Some(ParsedItem {
        name,
        path,
        kind,
        source,
        dependencies: deps.internal,
        typed_dependencies: deps.typed_dependencies,
        external_crates: deps.external_crates,
        module_path: module_path.to_string(),
        is_pub,
        visibility,
        file: file.to_path_buf(),
        line: None, // Not available from syn parser
        cfg_attr,
        generics,
    })
}

/// Result of parsing a single file - contains all extracted data
pub struct ParsedFileData {
    pub items: Vec<ParsedItem>,
    pub use_statements: Vec<UseStatement>,
    pub internal_aliases: Vec<InternalAlias>,
    pub macro_use_crates: Vec<String>,
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
        macro_use_crates: Vec::new(),
    };

    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => return result,
    };

    let syntax = match syn::parse_file(&content) {
        Ok(s) => s,
        Err(e) => {
            if is_verbose() {
                eprintln!("  Warning: Failed to parse {}: {}", path.display(), e);
            }
            return result;
        }
    };

    // Extract all data in a single pass through syntax.items
    for item in &syntax.items {
        // Extract parsed items (including inline modules)
        let is_inline_module = matches!(item, syn::Item::Mod(m) if m.content.is_some());

        let module_was_parsed = if let Some(parsed) = parse_item(item, module_path, &content, path, known_crates) {
            result.items.push(parsed);
            true
        } else {
            false
        };

        // Recursively parse inline module contents ONLY if the module itself wasn't filtered by cfg
        if is_inline_module && module_was_parsed {
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

        // Extract #[macro_use] extern crate declarations
        if let syn::Item::ExternCrate(extern_crate) = item {
            // Check for #[macro_use] attribute
            let has_macro_use = extern_crate.attrs.iter().any(|attr| {
                attr.path().is_ident("macro_use")
            });

            if has_macro_use {
                let crate_name = extern_crate.ident.to_string();
                result.macro_use_crates.push(crate_name);
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
            if is_verbose() {
                eprintln!("  Warning: Failed to parse {}: {}", path.display(), e);
            }
            return items;
        }
    };

    for item in &syntax.items {
        // For inline modules, we extract their contents separately
        // We also parse the module declaration itself to capture cfg attributes
        let is_inline_module = matches!(item, syn::Item::Mod(m) if m.content.is_some());

        // Parse all items, including inline modules (to capture their cfg_attr)
        let module_was_parsed = if let Some(parsed) = parse_item(item, module_path, &content, path, known_crates) {
            items.push(parsed);
            true
        } else {
            false
        };

        // Recursively parse inline module contents ONLY if the module itself wasn't filtered by cfg
        if is_inline_module && module_was_parsed {
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
            // Only recurse if the module wasn't filtered by cfg
            let module_was_parsed = if let Some(parsed) = parse_item(item, module_path, source_content, file, known_crates) {
                items.push(parsed);
                true
            } else {
                false
            };

            if module_was_parsed {
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
    // Get the source code for this use statement, including attributes (like #[cfg(...)])
    let mut use_source = String::new();

    // Include attributes in the source (important for platform-specific imports)
    for attr in &use_item.attrs {
        use_source.push_str(&attr.to_token_stream().to_string());
        use_source.push('\n');
    }

    use_source.push_str(&use_item.to_token_stream().to_string());
    let use_source = fix_quote_formatting(&use_source);

    // Normalize the source (remove extra spaces from syn's output)
    let use_source = normalize_use_source(&use_source);

    // Extract the symbols imported by this statement
    let symbols = extract_symbols_from_use(&use_item.tree);

    // Determine if this is an external import
    let is_external = is_external_use(&use_item.tree, known_crates);

    // Skip internal imports entirely - let generate_internal_imports handle them
    // Internal imports may have invalid paths after slicing (e.g., re-exports not included)
    // EXCEPT:
    // 1. Preserve module aliases like `use unix as imp;` which are critical for platform code
    // 2. Preserve `use crate::` imports since they reference crate modules explicitly
    //    (e.g., `use crate::tables::{CHUNK, LEAF};` for constants/statics)
    // 3. Preserve `use self::` imports for child module items
    //    (e.g., `use self::internal::{Inner, Visitor}`)
    // 4. Preserve `use super::` imports for parent module items (Phase 6.5a fix)
    //    (e.g., `use super::{ToValue, Value, Primitive}`)
    let is_module_alias = use_source.contains(" as ");
    let is_crate_import = !is_external && use_source.contains("use crate::");
    let is_self_import = !is_external && use_source.contains("use self::");
    let is_super_import = !is_external && use_source.contains("use super::");

    if !is_external && !is_module_alias && !is_crate_import && !is_self_import && !is_super_import {
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
pub fn extract_internal_aliases(path: &Path, known_crates: &std::collections::HashSet<String>) -> Vec<crate::types::InternalAlias> {
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
fn collect_internal_aliases(tree: &syn::UseTree, prefix: &str, aliases: &mut Vec<crate::types::InternalAlias>) {
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
            aliases.push(crate::types::InternalAlias {
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

        // Collect #[macro_use] extern crate declarations
        for crate_name in parsed.macro_use_crates {
            index.macro_use_crates.insert(crate_name);
        }
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

    #[test]
    fn test_fix_partial_paths() {
        let input = r#"// Auto-generated re-exports for crate:: references
pub use crate::kv::value::Value;

use crate::kv::error::Error;

impl < 'v > kv :: Value < 'v > {
    pub fn test() -> kv::Value {
        kv::Value::new()
    }
}
"#;
        let output = fix_partial_paths_in_generated_code(input);
        println!("=== OUTPUT ===\n{}", output);

        // Should replace kv::Value and kv :: Value with just Value
        assert!(!output.contains("kv :: Value"), "Should not contain 'kv :: Value'");
        assert!(output.contains("impl < 'v > Value < 'v >"), "Should contain 'impl < 'v > Value < 'v >'");
    }
}
