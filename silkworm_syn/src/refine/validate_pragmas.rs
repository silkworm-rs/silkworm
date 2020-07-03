use retain_mut::RetainMut;

use silkworm_err::ErrorCtx;
use silkworm_features::Features;

use crate::ast::visit::VisitMut;
use crate::ast::*;
use crate::token::{self, FeatureName, Keyword};

/// Validation transform that checks pragmas for invalid ones, and modifies the feature set.
pub struct ValidatePragmas<'a> {
    errors: &'a ErrorCtx,
    features: Features,
}

impl<'a> ValidatePragmas<'a> {
    pub fn new(errors: &'a ErrorCtx, features: &'a Features) -> Self {
        ValidatePragmas {
            errors,
            features: features.clone(),
        }
    }

    fn set_feature(&mut self, feature: FeatureName, value: bool) {
        match feature {
            FeatureName::UnicodeIdentifiers => self.features.unicode_identifiers = value,
            FeatureName::ScopedNodes => self.features.scoped_nodes = value,
            FeatureName::ScopedVariables => self.features.scoped_variables = value,
            FeatureName::Subroutine => self.features.subroutine = value,
            FeatureName::StringInterpolation => self.features.string_interpolation = value,
            FeatureName::ExtendedEscape => self.features.extended_escape = value,
        }
    }
}

impl<'a> VisitMut for ValidatePragmas<'a> {
    fn visit_file_mut(&mut self, file: &mut File) {
        file.pragmas.retain_mut(|pragma| {
            pragma.meta.retain_mut(|meta| match meta.path.as_keyword() {
                Some(Keyword::Pragma(pragma_kind)) => {
                    let is_enable = match pragma_kind {
                        token::Pragma::Feature => true,
                        token::Pragma::DisableFeature => false,
                        _ => {
                            self.errors
                                .error(format!(
                                    "the `{}` pragma may not be used at the file level",
                                    Keyword::Pragma(pragma_kind).into_canonical_str()
                                ))
                                .span(meta.path.span);
                            return false;
                        }
                    };

                    let args = match meta.args.as_mut() {
                        Some(args) => args,
                        None => {
                            self.errors
                                .error("feature pragmas must contain an argument list")
                                .span(meta.path.span);

                            return false;
                        }
                    };

                    args.retain_mut(|arg| {
                        if arg.args.take().is_some() {
                            self.errors
                                .error("feature names may not contain nested arguments")
                                .span(arg.path.span);
                        }

                        match arg.path.as_keyword() {
                            Some(Keyword::FeatureName(feature_name)) => {
                                self.set_feature(feature_name, is_enable);
                                true
                            }
                            _ => {
                                self.errors
                                    .error("not a valid feature name")
                                    .span(arg.path.span);
                                false
                            }
                        }
                    });

                    !args.is_empty()
                }
                _ => {
                    self.errors.warn("unknown pragma").span(meta.path.span);

                    false
                }
            });

            !pragma.meta.is_empty()
        });

        file.features = Some(self.features.clone());

        for node in &mut file.nodes {
            self.visit_node_mut(node);
        }
    }

    fn visit_node_mut(&mut self, node: &mut Node) {
        node.pragmas.append(&mut node.body.pragmas);

        node.pragmas.retain_mut(|pragma| {
            pragma.meta.retain_mut(|meta| match meta.path.as_keyword() {
                Some(Keyword::Pragma(pragma_kind)) => {
                    match pragma_kind {
                        token::Pragma::Private => {
                            if !self.features.scoped_nodes {
                                self.errors
                                    .error("the `scoped_nodes` feature is not enabled")
                                    .span(meta.path.span);

                                return false;
                            }

                            if meta.args.take().is_some() {
                                self.errors
                                    .error("the `private` pragma has no arguments")
                                    .span(meta.path.span);
                            }
                        }
                        token::Pragma::Sub => {
                            if !self.features.subroutine {
                                self.errors
                                    .error("the `subroutine` feature is not enabled")
                                    .span(meta.path.span);

                                return false;
                            }
                        }
                        _ => {
                            self.errors
                                .error(format!(
                                    "the `{}` pragma may not be used at the node level",
                                    Keyword::Pragma(pragma_kind).into_canonical_str()
                                ))
                                .span(meta.path.span);

                            return false;
                        }
                    };

                    true
                }
                _ => {
                    self.errors.warn("unknown pragma").span(meta.path.span);

                    false
                }
            });

            !pragma.meta.is_empty()
        });

        for node_header in &mut node.headers {
            self.visit_node_header_mut(node_header);
        }
        self.visit_block_mut(&mut node.body);
    }

    fn visit_pragma_mut(&mut self, pragma: &mut Pragma) {
        // Only unhandled pragmas will be visited this way
        self.errors.warn("unknown pragma").span(pragma.span);
    }
}
