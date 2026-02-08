//! HIR visitor for collecting item definitions and references
//!
//! This implementation uses the query-based API pattern from clippy.
//! Instead of tcx.hir(), we use tcx.hir_free_items() and tcx.hir_item().
//!
//! Optimizations:
//! - Single pass over hir_free_items() (definitions + MIR references together)
//! - Uses mir_built() instead of optimized_mir() to avoid MIR optimization cost
//! - Reduced string allocations in MIR visitor

#![cfg(feature = "rustc-driver")]

use super::{ItemInfo, ItemKind, UsageData, Visibility};
use super::debug_log::{debug_log, is_debug_enabled};
use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::LOCAL_CRATE;
use rustc_hir::def_id::LocalDefId;
use rustc_span::Span;
use rustc_middle::mir::{self, visit::Visitor as MirVisitor};

/// Collect usage data from HIR using clippy's pattern
///
/// Single-pass: collects definitions AND visits MIR bodies in one iteration.
pub fn collect_from_hir<'tcx>(tcx: TyCtxt<'tcx>) -> UsageData {
    let crate_name = tcx.crate_name(LOCAL_CRATE).to_string();
    let mut usage_data = UsageData::new(crate_name.clone());

    let debug = is_debug_enabled();

    if debug {
        debug_log(&format!("[hir_visitor] Collecting from crate: {}", crate_name));
    }

    // Single pass: collect definitions and visit MIR bodies together
    for id in tcx.hir_free_items() {
        let item = tcx.hir_item(id);
        let def_id = item.owner_id.def_id;

        // Handle impl blocks specially - collect their methods and visit MIR
        if let rustc_hir::ItemKind::Impl(impl_item) = &item.kind {
            collect_and_analyze_impl(tcx, impl_item, &mut usage_data, debug);
            continue;
        }

        // Get the full path for this item
        let path = tcx.def_path_str(def_id.to_def_id());
        let location = span_to_string(tcx, item.span);

        // Determine item kind
        let kind = match &item.kind {
            rustc_hir::ItemKind::Fn { .. } => ItemKind::Function,
            rustc_hir::ItemKind::TyAlias { .. } => ItemKind::Type,
            rustc_hir::ItemKind::Struct(..) => ItemKind::Type,
            rustc_hir::ItemKind::Enum(..) => ItemKind::Type,
            rustc_hir::ItemKind::Union(..) => ItemKind::Type,
            rustc_hir::ItemKind::Trait(..) => ItemKind::Trait,
            rustc_hir::ItemKind::Const(..) => ItemKind::Const,
            rustc_hir::ItemKind::Static(..) => ItemKind::Static,
            rustc_hir::ItemKind::Macro(..) => ItemKind::Macro,
            rustc_hir::ItemKind::Mod(..) => ItemKind::Mod,
            _ => continue, // Skip other kinds
        };

        // Determine visibility (simplified)
        let visibility = if item.vis_span.is_empty() {
            Visibility::Private
        } else {
            Visibility::Public
        };

        let item_info = ItemInfo {
            path: path.clone(),
            kind,
            visibility,
            location,
        };

        usage_data.add_item(path.clone(), item_info);

        // Visit MIR body for functions (in the same pass)
        if matches!(&item.kind, rustc_hir::ItemKind::Fn { .. }) {
            if debug {
                debug_log(&format!("[hir_visitor] Analyzing MIR for: {}", path));
            }

            // Use optimized_mir (mir_built is stolen by after_analysis time)
            let body = tcx.optimized_mir(def_id.to_def_id());

            let mut visitor = MirReferenceVisitor {
                tcx,
                current_item: &path,
                usage_data: &mut usage_data,
                local_decls: &body.local_decls,
                debug,
            };

            visitor.visit_body(body);
        }
    }

    // Second pass: visit closure MIR bodies.
    // Closures are separate DefIds not covered by hir_free_items().
    // Their call targets are invisible without visiting their MIR.
    for &local_def_id in tcx.mir_keys(()) {
        let def_id = local_def_id.to_def_id();
        let def_kind = tcx.def_kind(def_id);

        // Only visit closures (not AnonConst/InlineConst which use mir_for_ctfe)
        if !matches!(def_kind, rustc_hir::def::DefKind::Closure) {
            continue;
        }

        let path = tcx.def_path_str(def_id);

        if debug {
            debug_log(&format!("[hir_visitor] Analyzing MIR for closure: {}", path));
        }

        let body = tcx.optimized_mir(def_id);
        let mut visitor = MirReferenceVisitor {
            tcx,
            current_item: &path,
            usage_data: &mut usage_data,
            local_decls: &body.local_decls,
            debug,
        };
        visitor.visit_body(body);
    }

    if debug {
        debug_log(&format!("[hir_visitor] Collected {} items, {} reference sites",
                 usage_data.defined_items.len(),
                 usage_data.referenced_items.len()));
    }

    usage_data
}

/// Collect items from an impl block AND analyze their MIR in one pass
fn collect_and_analyze_impl<'tcx>(
    tcx: TyCtxt<'tcx>,
    impl_block: &rustc_hir::Impl<'tcx>,
    usage_data: &mut UsageData,
    debug: bool,
) {
    for &impl_item_id in impl_block.items {
        let def_id = impl_item_id.owner_id.def_id;
        let full_path = tcx.def_path_str(def_id.to_def_id());

        // Get span for location
        let span = tcx.def_span(def_id.to_def_id());
        let location = span_to_string(tcx, span);

        // Get associated item info to determine kind
        let assoc_item = tcx.associated_item(def_id.to_def_id());

        let is_fn = matches!(assoc_item.kind, rustc_middle::ty::AssocKind::Fn { .. });

        // Determine item kind
        let kind = match assoc_item.kind {
            rustc_middle::ty::AssocKind::Fn { .. } => ItemKind::Function,
            rustc_middle::ty::AssocKind::Const { .. } => ItemKind::Const,
            rustc_middle::ty::AssocKind::Type { .. } => ItemKind::Type,
        };

        // Determine visibility
        let visibility = if tcx.visibility(def_id).is_public() {
            Visibility::Public
        } else {
            Visibility::Private
        };

        let item_info = ItemInfo {
            path: full_path.clone(),
            kind,
            visibility,
            location,
        };

        usage_data.add_item(full_path.clone(), item_info);

        // Visit MIR body for methods (in the same pass)
        if is_fn {
            if debug {
                debug_log(&format!("[hir_visitor] Analyzing MIR for method: {}", full_path));
            }

            // Use optimized_mir (mir_built is stolen by after_analysis time)
            let body = tcx.optimized_mir(def_id.to_def_id());

            let mut visitor = MirReferenceVisitor {
                tcx,
                current_item: &full_path,
                usage_data,
                local_decls: &body.local_decls,
                debug,
            };

            visitor.visit_body(body);
        }
    }
}

/// MIR visitor that walks function bodies to find references
///
/// Uses borrowed &str for current_item to avoid per-function cloning.
/// Tracks all function references (calls, closures, fn pointers, casts)
/// and type references (construction, Drop impls).
struct MirReferenceVisitor<'a, 'tcx> {
    tcx: TyCtxt<'tcx>,
    current_item: &'a str,
    usage_data: &'a mut UsageData,
    /// Local variable declarations from the body (for place type lookups)
    local_decls: &'a rustc_middle::mir::LocalDecls<'tcx>,
    debug: bool,
}

impl<'a, 'tcx> MirVisitor<'tcx> for MirReferenceVisitor<'a, 'tcx> {
    fn visit_terminator(&mut self, terminator: &mir::Terminator<'tcx>, location: mir::Location) {
        // Track function calls in terminator
        if let mir::TerminatorKind::Call { func, .. } = &terminator.kind {
            if let mir::Operand::Constant(constant) = func {
                // Extract type from ANY Const variant (not just Const::Val)
                let ty = Self::const_ty(&constant.const_);
                if let rustc_middle::ty::TyKind::FnDef(def_id, _) = ty.kind() {
                    self.record_fn_reference(*def_id, "calls");
                }
            }
        }

        // Track Drop calls (implicit destructor calls)
        if let mir::TerminatorKind::Drop { place, .. } = &terminator.kind {
            let place_ty = place.ty(self.local_decls, self.tcx).ty;
            self.record_type_references(place_ty, "drops");
        }

        self.super_terminator(terminator, location);
    }

    fn visit_const_operand(&mut self, constant: &mir::ConstOperand<'tcx>, location: mir::Location) {
        // Track ALL function/closure references in constants, not just calls.
        // This catches: function pointers, closures used as values, generic instantiations.
        let ty = Self::const_ty(&constant.const_);
        match ty.kind() {
            rustc_middle::ty::TyKind::FnDef(def_id, _) => {
                self.record_fn_reference(*def_id, "references");
            }
            rustc_middle::ty::TyKind::Closure(def_id, _) => {
                self.record_fn_reference(*def_id, "references closure");
            }
            _ => {}
        }

        self.super_const_operand(constant, location);
    }

    fn visit_rvalue(&mut self, rvalue: &mir::Rvalue<'tcx>, location: mir::Location) {
        match rvalue {
            // Track type usage in rvalues (struct construction, casts, etc.)
            mir::Rvalue::Aggregate(aggregate_kind, _) => {
                if let mir::AggregateKind::Adt(def_id, _, _, _, _) = **aggregate_kind {
                    let type_path = self.tcx.def_path_str(def_id);
                    self.record_reference(&type_path);
                    // Also track Drop impl for this type
                    self.record_drop_for_adt(def_id);
                }
                if let mir::AggregateKind::Closure(def_id, _) = **aggregate_kind {
                    self.record_fn_reference(def_id, "constructs closure");
                }
            }
            // Track function pointers in casts
            mir::Rvalue::Cast(_, operand, cast_ty) => {
                // Track the source operand's type for fn references
                if let mir::Operand::Constant(constant) = operand {
                    let src_ty = Self::const_ty(&constant.const_);
                    if let rustc_middle::ty::TyKind::FnDef(def_id, _) = src_ty.kind() {
                        self.record_fn_reference(*def_id, "casts fn");
                    }
                }
                // Track the target type for fn pointer targets
                self.record_type_references(*cast_ty, "cast target");
            }
            // Track references to values (function items used as values)
            mir::Rvalue::Ref(_, _, place) | mir::Rvalue::RawPtr(_, place) => {
                let place_ty = place.ty(self.local_decls, self.tcx).ty;
                self.record_type_references(place_ty, "ref");
            }
            _ => {}
        }

        self.super_rvalue(rvalue, location);
    }
}

impl<'a, 'tcx> MirReferenceVisitor<'a, 'tcx> {
    fn record_reference(&mut self, referenced_path: &str) {
        // Add dependency: current_item depends on referenced_path
        self.usage_data.add_dependency(
            self.current_item.to_owned(),
            referenced_path.to_owned()
        );

        // Track where this item is referenced (just the caller path, no "in " prefix)
        self.usage_data.add_reference(
            referenced_path.to_owned(),
            self.current_item.to_owned()
        );
    }

    /// Extract the type from any mir::Const variant
    fn const_ty(c: &mir::Const<'tcx>) -> rustc_middle::ty::Ty<'tcx> {
        match *c {
            mir::Const::Val(_, ty) => ty,
            mir::Const::Ty(ty, _) => ty,
            mir::Const::Unevaluated(_, ty) => ty,
        }
    }

    /// Record a function/closure reference by DefId, filtering std library
    fn record_fn_reference(&mut self, def_id: rustc_span::def_id::DefId, kind: &str) {
        let path = self.tcx.def_path_str(def_id);

        // Filter out std library
        if path.starts_with("std::") ||
           path.starts_with("core::") ||
           path.starts_with("alloc::") {
            return;
        }

        self.record_reference(&path);

        if self.debug {
            debug_log(&format!("[reference] {} {} {}", self.current_item, kind, path));
        }
    }

    /// Record references to Drop implementations for a type
    fn record_drop_for_adt(&mut self, def_id: rustc_span::def_id::DefId) {
        // Check if this ADT type has a Drop impl
        let ty = self.tcx.type_of(def_id).instantiate_identity();
        if let Some(drop_def_id) = ty.ty_adt_def().and_then(|adt| {
            self.tcx.adt_destructor(adt.did()).map(|dtor| dtor.did)
        }) {
            self.record_fn_reference(drop_def_id, "drops");
        }
    }

    /// Record references for types that contain function defs or need Drop
    fn record_type_references(&mut self, ty: rustc_middle::ty::Ty<'tcx>, kind: &str) {
        match ty.kind() {
            rustc_middle::ty::TyKind::FnDef(def_id, _) => {
                self.record_fn_reference(*def_id, kind);
            }
            rustc_middle::ty::TyKind::Closure(def_id, _) => {
                self.record_fn_reference(*def_id, kind);
            }
            rustc_middle::ty::TyKind::Adt(adt_def, _) => {
                // Track Drop impl for this ADT
                if let Some(dtor) = self.tcx.adt_destructor(adt_def.did()) {
                    self.record_fn_reference(dtor.did, kind);
                }
            }
            _ => {}
        }
    }
}

/// Get source location as a string
fn span_to_string(tcx: TyCtxt<'_>, span: Span) -> String {
    let source_map = tcx.sess.source_map();
    let lo = source_map.lookup_char_pos(span.lo());
    format!("{}:{}:{}",
        lo.file.name.prefer_local(),
        lo.line,
        lo.col.0 + 1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hir_visitor_structure() {
        // Just verify the module compiles
    }
}
