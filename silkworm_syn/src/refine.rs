use crate::parse::ParseCtx;

mod block_regroup;
mod deny_pragmas;
mod duplicate_headers;
mod feature_gate;
mod validate_pragmas;

use block_regroup::BlockRegroup;
use deny_pragmas::DenyPragmas;
use duplicate_headers::DuplicateHeaders;
use feature_gate::FeatureGate;
use validate_pragmas::ValidatePragmas;

/// Run all refine transforms on a `Visitable` AST type.
pub fn refine<A: crate::ast::visit::Visitable>(ctx: &ParseCtx<'_>, ast: &mut A) {
    if !ctx.features.pragmas {
        ast.visit_mut_with(&mut DenyPragmas::new(ctx.errors));
    }
    ast.visit_mut_with(&mut BlockRegroup::new(ctx.errors));
    ast.visit_mut_with(&mut ValidatePragmas::new(ctx.errors, ctx.features));
    ast.visit_mut_with(&mut FeatureGate::new(ctx));
    ast.visit_mut_with(&mut DuplicateHeaders::new(ctx));
}
