use silkworm_err::ErrorCtx;

mod block_regroup;

use block_regroup::BlockRegroup;

#[derive(Debug)]
pub struct RefineCtx<'a> {
    pub errors: &'a ErrorCtx,
}

/// Run all refine transforms on a `Visitable` AST type..
pub fn refine<A: crate::ast::visit::Visitable>(ctx: &RefineCtx<'_>, ast: &mut A) {
    ast.visit_mut_with(&mut BlockRegroup::new(ctx.errors));
}
