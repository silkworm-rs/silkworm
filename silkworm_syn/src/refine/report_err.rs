use silkworm_err::ErrorCtx;

use crate::ast::visit::Visit;
use crate::ast::*;
use crate::parse::ParseCtx;

/// Validation transform that reports remaining invalid AST nodes that might have remained after prior transforms passes.
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
            StmtKind::If(if_stmt) => self.visit_if_stmt(if_stmt),
            StmtKind::Shortcuts(shortcuts_stmt) => self.visit_shortcuts_stmt(shortcuts_stmt),

            StmtKind::Block(_) => {
                self.errors
                    .bug("remaining raw Block statement")
                    .span(stmt_body.span);
            }
            StmtKind::ShortcutOption(_) => {
                self.errors
                    .bug("remaining raw ShortcutOption statement")
                    .span(stmt_body.span);
            }
            StmtKind::Err => {
                self.errors
                    .bug("remaining Err statement")
                    .span(stmt_body.span);
            }
        }
    }

    fn visit_command(&mut self, command: &'ast Command) {
        match &command.kind {
            CommandKind::Set(var, value) => {
                self.visit_var(var);
                self.visit_expr(value);
            }
            CommandKind::Call(expr) => self.visit_expr(expr),
            CommandKind::Return(Some(expr)) => self.visit_expr(expr),
            CommandKind::Return(None) => {}
            CommandKind::Custom(str_body) => self.visit_str_body(str_body),

            CommandKind::If(_)
            | CommandKind::ElseIf(_)
            | CommandKind::Else
            | CommandKind::EndIf => {
                self.errors.bug("remaining if-command").span(command.span);
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        for pragma in &stmt.pragmas {
            self.visit_pragma(pragma);
        }

        self.visit_stmt_body(&stmt.body);

        if let Some(command) = stmt.decorator_command.as_ref() {
            self.errors
                .bug("remaining decorator command")
                .span(command.span);
        }

        for hashtag in &stmt.hashtags {
            self.visit_hashtag(hashtag);
        }
    }
}
