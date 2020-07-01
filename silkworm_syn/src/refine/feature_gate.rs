use silkworm_err::ErrorCtx;
use silkworm_features::Features;

use crate::ast::visit::VisitMut;
use crate::ast::*;
use crate::parse::ParseCtx;
use crate::symbol::{Interner, Symbol};

/// Validation transform that checks for things that require disabled feature flags.
pub struct FeatureGate<'a> {
    errors: &'a ErrorCtx,
    interner: &'a Interner,
    features: Features,
}

impl<'a> FeatureGate<'a> {
    pub fn new(ctx: &'a ParseCtx<'a>) -> Self {
        FeatureGate {
            errors: ctx.errors,
            interner: ctx.interner,
            features: ctx.features.clone(),
        }
    }

    fn is_ascii(&self, sym: Symbol) -> bool {
        self.interner.read(sym).chars().all(|c| c.is_ascii())
    }
}

impl<'a> VisitMut for FeatureGate<'a> {
    fn visit_var_mut(&mut self, var: &mut Var) {
        if !self.features.scoped_variables {
            match var.sigil {
                Sigil::Node | Sigil::File => {
                    self.errors
                        .error("scoped sigils can only be used when `scoped_variables` is enabled")
                        .span(var.span);
                }
                // Local sigils are only disallowed in declarations (may be used as receivers)
                _ => {}
            }
        }

        if !self.features.unicode_identifiers && !self.is_ascii(var.symbol) {
            self.errors
                .error("unicode characters can only be used when `unicode_identifiers` is enabled")
                .span(var.span);
        }

        if let Some(keyword) = var.keyword {
            self.errors
                .error(format!(
                    "the keyword `{}` cannot be used in identifiers",
                    keyword.into_canonical_str()
                ))
                .span(var.span);
        }
    }

    fn visit_path_segment_mut(&mut self, segment: &mut PathSegment) {
        if !self.features.unicode_identifiers && !self.is_ascii(segment.symbol) {
            self.errors
                .error("unicode characters can only be used when `unicode_identifiers` is enabled")
                .span(segment.span);
        }

        if let Some(keyword) = segment.keyword {
            self.errors
                .error(format!(
                    "the keyword `{}` cannot be used in identifiers",
                    keyword.into_canonical_str()
                ))
                .span(segment.span);
        }
    }

    fn visit_flow_target_sub_routine_mut(
        &mut self,
        flow_target_sub_routine: &mut FlowTargetSubRoutine,
    ) {
        if !self.features.subroutine {
            self.errors
                .error("subroutine flow targets are only available when `subroutine` is enabled")
                .span(flow_target_sub_routine.span);
        }

        self.visit_path_mut(&mut flow_target_sub_routine.path);
        for argument in &mut flow_target_sub_routine.arguments {
            self.visit_expr_mut(argument);
        }
    }

    fn visit_lit_mut(&mut self, lit: &mut Lit) {
        match &mut lit.kind {
            LitKind::Str(body) => self.visit_str_body_mut(body),
            LitKind::InterpolatedStr(body) => {
                if self.features.string_interpolation {
                    self.visit_str_body_mut(body)
                } else {
                    self.errors
                        .error("interpolated literals are only available when `string_interpolation` is enabled")
                        .span(lit.span);
                }
            }
            LitKind::Int(..)
            | LitKind::Float(..)
            | LitKind::True
            | LitKind::False
            | LitKind::Null => {}
        }
    }

    fn visit_str_body_mut(&mut self, str_body: &mut StrBody) {
        if !self.features.extended_escape {
            let mut rewritten = false;

            for segment in &mut str_body.segments {
                if let StrSegment::Escape(escape) = &segment {
                    if escape.kind.is_extended() {
                        *segment = StrSegment::Text(Text { span: escape.span });
                        rewritten = true;
                    }
                }
            }

            if rewritten {
                let mut segments = Vec::with_capacity(str_body.segments.len());
                let mut building_text = None;
                for segment in str_body.segments.drain(..) {
                    match segment {
                        StrSegment::Text(text) => {
                            let span = text.span;
                            let building_text = building_text.get_or_insert(text);
                            building_text.span = building_text.span.union(span);
                        }
                        _ => {
                            if let Some(text) = building_text.take() {
                                segments.push(StrSegment::Text(text));
                            }
                            segments.push(segment);
                        }
                    }
                }

                if let Some(text) = building_text.take() {
                    segments.push(StrSegment::Text(text));
                }

                str_body.segments = segments;
            }
        }

        for segment in &mut str_body.segments {
            self.visit_str_segment_mut(segment);
        }
    }

    fn visit_file_mut(&mut self, file: &mut File) {
        if let Some(features) = file.features.clone() {
            self.features = features;
        }

        for pragma in &mut file.pragmas {
            self.visit_pragma_mut(pragma);
        }
        for node in &mut file.nodes {
            self.visit_node_mut(node);
        }
    }
}
