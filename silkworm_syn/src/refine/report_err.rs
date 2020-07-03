use silkworm_err::ErrorCtx;

use crate::ast::visit::Visit;
use crate::ast::*;
use crate::parse::ParseCtx;

/// Validation transform that reports remaining `Err` AST nodes that might have remained after prior transforms passes.
pub struct ReportErr<'a> {
    errors: &'a ErrorCtx,
}

impl<'a> ReportErr<'a> {
    pub fn new(ctx: &'a ParseCtx<'a>) -> Self {
        ReportErr { errors: ctx.errors }
    }
}

impl<'a, 'ast> Visit<'ast> for ReportErr<'a> {
    fn visit_expr(&mut self, expr: &'ast Expr) {
        match &expr.kind {
            ExprKind::Var(var) => self.visit_var(var),
            ExprKind::Call(receiver, args) => {
                self.visit_expr(receiver);
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            ExprKind::Unary(_, operand) => self.visit_expr(operand),
            ExprKind::Binary(_, left, right) => {
                self.visit_expr(left);
                self.visit_expr(right);
            }
            ExprKind::Lit(lit) => self.visit_lit(lit),
            ExprKind::Err => {
                self.errors.bug("remaining Err expression").span(expr.span);
            }
        }
    }

    fn visit_stmt_body(&mut self, stmt_body: &'ast StmtBody) {
        match &stmt_body.kind {
            StmtKind::Text(str_body) => self.visit_str_body(str_body),
            StmtKind::Command(command) => self.visit_command(command),
            StmtKind::Flow(flow) => self.visit_flow(flow),
            StmtKind::ShortcutOption(shortcut_option) => {
                self.visit_shortcut_option(shortcut_option)
            }
            StmtKind::Block(block) => self.visit_block(block),
            StmtKind::If(if_stmt) => self.visit_if_stmt(if_stmt),
            StmtKind::Shortcuts(shortcuts_stmt) => self.visit_shortcuts_stmt(shortcuts_stmt),
            StmtKind::Err => {
                self.errors
                    .bug("remaining Err statement")
                    .span(stmt_body.span);
            }
        }
    }
}
