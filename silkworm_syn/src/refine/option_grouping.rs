use std::vec;

use silkworm_err::ErrorCtx;

use crate::ast::visit::VisitMut;
use crate::ast::*;

/// Option grouping transform that groups consecutive `Flow` statements with options.
pub struct OptionGrouping<'a> {
    errors: &'a ErrorCtx,
}

impl<'a> OptionGrouping<'a> {
    pub fn new(error_ctx: &'a ErrorCtx) -> Self {
        OptionGrouping { errors: error_ctx }
    }
}

impl<'a> VisitMut for OptionGrouping<'a> {
    fn visit_block_mut(&mut self, block: &mut Block) {
        let raw_stmts = std::mem::replace(&mut block.stmts, Vec::new());
        let mut stmts = Vec::with_capacity(raw_stmts.len());
        let mut state = State::new(self.errors, raw_stmts);

        while state.stmt.is_some() {
            if state.stmt_is_option() {
                stmts.push(state.parse_group());
            } else {
                stmts.push(state.bump().expect("current stmt is checked"));
            }
        }

        block.stmts = stmts;

        for stmt in &mut block.stmts {
            self.visit_stmt_mut(stmt);
        }
    }
}

struct State<'a> {
    errors: &'a ErrorCtx,
    iter: vec::IntoIter<Stmt>,
    stmt: Option<Stmt>,
}

impl<'a> State<'a> {
    fn new(errors: &'a ErrorCtx, raw_stmts: Vec<Stmt>) -> Self {
        let iter = raw_stmts.into_iter();

        let mut this = State {
            errors,
            iter,
            stmt: None,
        };

        this.bump();
        this
    }

    fn bump(&mut self) -> Option<Stmt> {
        std::mem::replace(&mut self.stmt, self.iter.next())
    }

    fn stmt_is_option(&self) -> bool {
        let stmt = if let Some(stmt) = self.stmt.as_ref() {
            stmt
        } else {
            return false;
        };

        if let StmtKind::Flow(flow) = &stmt.body.kind {
            flow.option_text.is_some()
        } else {
            false
        }
    }

    fn parse_group(&mut self) -> Stmt {
        let mut all_span = self
            .stmt
            .as_ref()
            .expect("current statement should be checked")
            .span;

        let mut options = Vec::new();
        let mut group_kind = None;

        while self.stmt_is_option() {
            let Stmt {
                span,
                pragmas,
                body,
                decorator_command,
                hashtags,
            } = self.bump().expect("should be a current stmt");

            all_span = all_span.union(span);

            for pragma in pragmas {
                self.errors
                    .warn("pragmas on options are ignored")
                    .span(pragma.span)
                    .annotate_span(span, "on this statement");
            }

            let (option, target) = if let StmtKind::Flow(flow) = body.kind {
                (
                    flow.option_text.expect("stmt kind should be checked"),
                    flow.target,
                )
            } else {
                panic!("stmt kind should be checked");
            };

            let expected_group_kind = match &target {
                FlowTarget::Path(_) => OptionGroupKind::Jump,
                FlowTarget::SubRoutine(..) | FlowTarget::SubRoutineSet(..) => {
                    OptionGroupKind::Subroutine
                }
            };

            if group_kind.get_or_insert(expected_group_kind) != &expected_group_kind {
                group_kind = Some(OptionGroupKind::Mixed);
            }

            let condition = decorator_command.and_then(|command| {
                if let CommandKind::If(condition) = command.kind {
                    Some(condition)
                } else {
                    self.errors
                        .error("only if-commands are allowed as decorators for options")
                        .span(command.span)
                        .annotate_span(span, "on this statement");
                    None
                }
            });

            options.push(OptionClause {
                span,
                option,
                condition,
                hashtags,
                target: OptionTarget::FlowTarget(target),
            })
        }

        let group_kind = group_kind.expect("there should be at least one option");
        if group_kind == OptionGroupKind::Mixed {
            self.errors
                .error("cannot mix jump and subroutine options")
                .span(all_span);
        }

        Stmt {
            span: all_span,
            pragmas: Vec::new(),
            body: StmtBody {
                span: all_span,
                kind: StmtKind::Options(OptionsStmt {
                    span: all_span,
                    kind: group_kind,
                    options,
                }),
            },
            decorator_command: None,
            hashtags: Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Use pretty_assertions for `assert_eq` diffs.
    use pretty_assertions::assert_eq;

    use crate::symbol::Interner;
    use crate::Span;

    fn stmt(span: Span, body: StmtKind) -> Stmt {
        Stmt {
            span,
            pragmas: Vec::new(),
            body: StmtBody { span, kind: body },
            decorator_command: None,
            hashtags: Vec::new(),
        }
    }

    fn option_stmt(option_span: Span, target: FlowTarget) -> Stmt {
        stmt(
            option_span,
            StmtKind::Flow(Flow {
                span: option_span,
                option_text: Some(StrBody::empty(option_span)),
                target,
            }),
        )
    }

    fn text_stmt(text_span: Span) -> Stmt {
        stmt(text_span, StmtKind::Text(StrBody::empty(text_span)))
    }

    #[test]
    fn test_option_grouping() {
        let itn = Interner::with_max_bins(1);

        let mut source_block = Block {
            span: Span::new(0, 0),
            pragmas: Vec::new(),
            stmts: vec![
                text_stmt(Span::new(0, 0)),
                option_stmt(
                    Span::new(1, 0),
                    FlowTarget::Path(Path::new(&itn, "foo", Span::new(1, 0))),
                ),
                option_stmt(
                    Span::new(2, 0),
                    FlowTarget::Path(Path::new(&itn, "bar", Span::new(2, 0))),
                ),
                option_stmt(
                    Span::new(3, 0),
                    FlowTarget::Path(Path::new(&itn, "baz", Span::new(3, 0))),
                ),
                text_stmt(Span::new(10, 0)),
                option_stmt(
                    Span::new(11, 0),
                    FlowTarget::SubRoutine(FlowTargetSubRoutine {
                        span: Span::new(11, 0),
                        path: Path::new(&itn, "foo", Span::new(11, 0)),
                        arguments: Vec::new(),
                    }),
                ),
                option_stmt(
                    Span::new(12, 0),
                    FlowTarget::SubRoutineSet(
                        Span::new(12, 0),
                        Var {
                            sigil: Sigil::Global,
                            symbol: itn.intern("foo"),
                            keyword: None,
                            span: Span::new(12, 0),
                        },
                        FlowTargetSubRoutine {
                            span: Span::new(12, 0),
                            path: Path::new(&itn, "foo", Span::new(12, 0)),
                            arguments: Vec::new(),
                        },
                    ),
                ),
                text_stmt(Span::new(20, 0)),
            ],
        };

        let errors = ErrorCtx::new();
        OptionGrouping::new(&errors).visit_block_mut(&mut source_block);
        assert!(errors.is_empty(), "should not emit any errors");

        let expected_block = Block {
            span: Span::new(0, 0),
            pragmas: Vec::new(),
            stmts: vec![
                text_stmt(Span::new(0, 0)),
                stmt(
                    Span::new(1, 2),
                    StmtKind::Options(OptionsStmt {
                        span: Span::new(1, 2),
                        kind: OptionGroupKind::Jump,
                        options: vec![
                            OptionClause {
                                span: Span::new(1, 0),
                                option: StrBody::empty(Span::new(1, 0)),
                                condition: None,
                                hashtags: Vec::new(),
                                target: OptionTarget::FlowTarget(FlowTarget::Path(Path::new(
                                    &itn,
                                    "foo",
                                    Span::new(1, 0),
                                ))),
                            },
                            OptionClause {
                                span: Span::new(2, 0),
                                option: StrBody::empty(Span::new(2, 0)),
                                condition: None,
                                hashtags: Vec::new(),
                                target: OptionTarget::FlowTarget(FlowTarget::Path(Path::new(
                                    &itn,
                                    "bar",
                                    Span::new(2, 0),
                                ))),
                            },
                            OptionClause {
                                span: Span::new(3, 0),
                                option: StrBody::empty(Span::new(3, 0)),
                                condition: None,
                                hashtags: Vec::new(),
                                target: OptionTarget::FlowTarget(FlowTarget::Path(Path::new(
                                    &itn,
                                    "baz",
                                    Span::new(3, 0),
                                ))),
                            },
                        ],
                    }),
                ),
                text_stmt(Span::new(10, 0)),
                stmt(
                    Span::new(11, 1),
                    StmtKind::Options(OptionsStmt {
                        span: Span::new(11, 1),
                        kind: OptionGroupKind::Subroutine,
                        options: vec![
                            OptionClause {
                                span: Span::new(11, 0),
                                option: StrBody::empty(Span::new(11, 0)),
                                condition: None,
                                hashtags: Vec::new(),
                                target: OptionTarget::FlowTarget(FlowTarget::SubRoutine(
                                    FlowTargetSubRoutine {
                                        span: Span::new(11, 0),
                                        path: Path::new(&itn, "foo", Span::new(11, 0)),
                                        arguments: Vec::new(),
                                    },
                                )),
                            },
                            OptionClause {
                                span: Span::new(12, 0),
                                option: StrBody::empty(Span::new(12, 0)),
                                condition: None,
                                hashtags: Vec::new(),
                                target: OptionTarget::FlowTarget(FlowTarget::SubRoutineSet(
                                    Span::new(12, 0),
                                    Var {
                                        sigil: Sigil::Global,
                                        symbol: itn.intern("foo"),
                                        keyword: None,
                                        span: Span::new(12, 0),
                                    },
                                    FlowTargetSubRoutine {
                                        span: Span::new(12, 0),
                                        path: Path::new(&itn, "foo", Span::new(12, 0)),
                                        arguments: Vec::new(),
                                    },
                                )),
                            },
                        ],
                    }),
                ),
                text_stmt(Span::new(20, 0)),
            ],
        };

        assert_eq!(expected_block, source_block);
    }
}
