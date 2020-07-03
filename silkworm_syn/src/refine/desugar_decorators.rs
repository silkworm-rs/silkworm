use silkworm_err::ErrorCtx;

use crate::ast::visit::VisitMut;
use crate::ast::*;
use crate::parse::ParseCtx;

/// Validation transform that desugars decorator commands like `if`s.
pub struct DesugarDecorators<'a> {
    errors: &'a ErrorCtx,
}

impl<'a> DesugarDecorators<'a> {
    pub fn new(ctx: &'a ParseCtx<'a>) -> Self {
        DesugarDecorators { errors: ctx.errors }
    }
}

impl<'a> VisitMut for DesugarDecorators<'a> {
    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        if let Some(decorator_command) = stmt.decorator_command.take() {
            if let CommandKind::If(expr) = decorator_command.kind {
                let original_stmt = std::mem::replace(
                    stmt,
                    Stmt {
                        span: stmt.span,
                        pragmas: Vec::new(),
                        body: StmtBody {
                            span: stmt.span,
                            kind: StmtKind::Err,
                        },
                        decorator_command: None,
                        hashtags: Vec::new(),
                    },
                );

                let mut block = Block::empty(original_stmt.span);
                block.push(original_stmt);

                stmt.body.kind = StmtKind::If(IfStmt {
                    span: stmt.span,
                    if_clause: IfClause {
                        span: stmt.span,
                        condition: expr,
                        block,
                    },
                    else_if_clauses: Vec::new(),
                    else_block: None,
                });
            } else {
                self.errors
                    .error("only if-commands can be used as decorators")
                    .span(decorator_command.span)
                    .annotate_span(stmt.span, "for this statement");
            }
        }

        self.visit_stmt_body_mut(&mut stmt.body);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Use pretty_assertions for `assert_eq` diffs.
    use pretty_assertions::assert_eq;

    use silkworm_err::ErrorCtx;

    use crate::Span;

    #[test]
    fn test_desugar_decorators() {
        let mut source_stmt = Stmt {
            span: Span::new(0, 24),
            pragmas: Vec::new(),
            body: StmtBody {
                span: Span::new(0, 6),
                kind: StmtKind::Err,
            },
            decorator_command: Some(Command {
                span: Span::new(6, 6),
                kind: CommandKind::If(Box::new(Expr {
                    span: Span::new(6, 6),
                    kind: ExprKind::Err,
                })),
            }),
            hashtags: vec![Hashtag {
                span: Span::new(12, 12),
                text: Text {
                    span: Span::new(12, 12),
                },
            }],
        };

        let errors = ErrorCtx::new();
        let mut pass = DesugarDecorators { errors: &errors };
        pass.visit_stmt_mut(&mut source_stmt);
        assert!(errors.is_empty(), "should not emit any errors");

        let expected_stmt = Stmt {
            span: Span::new(0, 24),
            pragmas: Vec::new(),
            body: StmtBody {
                span: Span::new(0, 24),
                kind: StmtKind::If(IfStmt {
                    span: Span::new(0, 24),
                    if_clause: IfClause {
                        span: Span::new(0, 24),
                        condition: Box::new(Expr {
                            span: Span::new(6, 6),
                            kind: ExprKind::Err,
                        }),
                        block: Block {
                            span: Span::new(0, 24),
                            pragmas: Vec::new(),
                            stmts: vec![Stmt {
                                span: Span::new(0, 24),
                                pragmas: Vec::new(),
                                body: StmtBody {
                                    span: Span::new(0, 6),
                                    kind: StmtKind::Err,
                                },
                                decorator_command: None,
                                hashtags: vec![Hashtag {
                                    span: Span::new(12, 12),
                                    text: Text {
                                        span: Span::new(12, 12),
                                    },
                                }],
                            }],
                        },
                    },
                    else_if_clauses: Vec::new(),
                    else_block: None,
                }),
            },
            decorator_command: None,
            hashtags: Vec::new(),
        };

        assert_eq!(expected_stmt, source_stmt);
    }
}
