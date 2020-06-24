use std::vec;

use silkworm_err::ErrorCtx;

use crate::ast::visit::VisitMut;
use crate::ast::{
    Block, Command, CommandKind, Hashtag, IfClause, IfStmt, Pragma, ShortcutOption,
    ShortcutOptionClause, ShortcutsStmt, Stmt, StmtBody, StmtKind,
};
use crate::Span;

/// Block regrouping transform that flattens nested blocks, and creates `if` statements and
/// shortcut groups.
pub struct BlockRegroup<'a> {
    errors: &'a ErrorCtx,
}

impl<'a> BlockRegroup<'a> {
    pub fn new(error_ctx: &'a ErrorCtx) -> Self {
        BlockRegroup { errors: error_ctx }
    }
}

impl<'a> VisitMut for BlockRegroup<'a> {
    fn visit_block_mut(&mut self, block: &mut Block) {
        let raw_stmts = std::mem::replace(&mut block.stmts, Vec::new());
        let mut stmts = Vec::with_capacity(raw_stmts.len());
        let mut state = IfGroupState::new(self.errors, raw_stmts);

        while state.stmt.is_some() {
            if state.stmt_is_if() {
                stmts.push(state.group_if_statement());
            } else {
                stmts.push(state.bump().expect("current stmt is checked"));
            }
        }

        block.stmts = stmts;

        // don't need to visit blocks recursively here for blocks associated with shortcut
        // options, since that's done in OptionGroupIter
    }
}

struct IfGroupState<'a> {
    errors: &'a ErrorCtx,
    iter: OptionGroupIter<'a>,
    stmt: Option<Stmt>,
}

impl<'a> IfGroupState<'a> {
    fn new(errors: &'a ErrorCtx, raw_stmts: Vec<Stmt>) -> Self {
        let iter = OptionGroupIter::new(errors, FlattenIter::new(raw_stmts.into_iter()));

        let mut this = IfGroupState {
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

    fn stmt_is_if(&self) -> bool {
        let stmt = if let Some(stmt) = self.stmt.as_ref() {
            stmt
        } else {
            return false;
        };

        if let StmtKind::Command(command) = &stmt.body.kind {
            if let CommandKind::If(_) = &command.kind {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn stmt_is_else_if(&self) -> bool {
        let stmt = if let Some(stmt) = self.stmt.as_ref() {
            stmt
        } else {
            return false;
        };

        if let StmtKind::Command(command) = &stmt.body.kind {
            if let CommandKind::ElseIf(_) = &command.kind {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn stmt_is_else(&self) -> bool {
        let stmt = if let Some(stmt) = self.stmt.as_ref() {
            stmt
        } else {
            return false;
        };

        if let StmtKind::Command(command) = &stmt.body.kind {
            if let CommandKind::Else = &command.kind {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn stmt_is_end_if(&self) -> bool {
        let stmt = if let Some(stmt) = self.stmt.as_ref() {
            stmt
        } else {
            return false;
        };

        if let StmtKind::Command(command) = &stmt.body.kind {
            if let CommandKind::EndIf = &command.kind {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn verify_if_commands(&self, stmt: &Stmt) {
        if let Some(decorator_command) = stmt.decorator_command.as_ref() {
            self.errors
                .error("decorator commands are not allowed on if-commands")
                .span(decorator_command.span);
        }

        for hashtag in &stmt.hashtags {
            self.errors
                .warn("hashtags on if-commands are ignored")
                .span(hashtag.span);
        }
    }

    fn group_if_statement(&mut self) -> Stmt {
        let mut all_span = self
            .stmt
            .as_ref()
            .expect("current statement should be checked")
            .span;

        let if_clause = {
            let mut stmt = self.bump().expect("should be a current stmt");
            let span = stmt.span;

            self.verify_if_commands(&stmt);

            let condition = if let StmtKind::Command(command) = stmt.body.kind {
                if let CommandKind::If(condition) = command.kind {
                    Some(condition)
                } else {
                    None
                }
            } else {
                None
            };
            let condition = condition.expect("statement kind should be checked");

            let pragmas = std::mem::replace(&mut stmt.pragmas, Vec::new());
            let block = self.group_clause_block(stmt.span.empty_end(), pragmas);
            let span = span.union(block.span);

            IfClause {
                span,
                condition,
                block,
            }
        };

        all_span = all_span.union(if_clause.span);

        let mut else_if_clauses = Vec::new();
        while self.stmt_is_else_if() {
            let mut stmt = self.bump().expect("should be a current stmt");
            let span = stmt.span;

            self.verify_if_commands(&stmt);

            let condition = if let StmtKind::Command(command) = stmt.body.kind {
                if let CommandKind::ElseIf(condition) = command.kind {
                    Some(condition)
                } else {
                    None
                }
            } else {
                None
            };
            let condition = condition.expect("statement kind should be checked");

            let pragmas = std::mem::replace(&mut stmt.pragmas, Vec::new());
            let block = self.group_clause_block(stmt.span.empty_end(), pragmas);
            let span = span.union(block.span);
            all_span = all_span.union(span);

            else_if_clauses.push(IfClause {
                span,
                condition,
                block,
            });
        }

        let else_block = if self.stmt_is_else() {
            let else_stmt = self.bump().expect("current stmt checked");
            self.verify_if_commands(&else_stmt);
            let block = self.group_clause_block(all_span.empty_end(), Vec::new());
            all_span = all_span.union(block.span);
            Some(block)
        } else {
            None
        };

        let invalid = if self.stmt.is_none() || self.stmt_is_end_if() {
            None
        } else {
            let mut block = Block::empty(all_span.empty_end());
            while self.stmt.is_some() && !self.stmt_is_end_if() {
                block.push(self.bump().expect("current stmt checked"));
            }
            all_span = all_span.union(block.span);
            Some(block)
        };

        // end-if command or nothing
        if let Some(end_if_stmt) = self.bump() {
            self.verify_if_commands(&end_if_stmt);
        } else {
            self.errors
                .error("this if statement is not closed")
                .span(all_span);
        }

        let body = IfStmt {
            span: all_span,
            if_clause,
            else_if_clauses,
            else_block,
            invalid,
        };

        Stmt {
            span: all_span,
            pragmas: Vec::new(),
            body: StmtBody {
                span: all_span,
                kind: StmtKind::If(body),
            },
            decorator_command: None,
            hashtags: Vec::new(),
        }
    }

    fn group_clause_block(&mut self, start_span: Span, pragmas: Vec<Pragma>) -> Block {
        let mut block = Block::empty(start_span);
        block.pragmas = pragmas;

        while let Some(stmt) = self.stmt.as_ref() {
            if let StmtKind::Command(command) = &stmt.body.kind {
                match &command.kind {
                    CommandKind::If(_) => {
                        let if_stmt = self.group_if_statement();
                        block.push(if_stmt);
                    }
                    CommandKind::ElseIf(_) | CommandKind::Else | CommandKind::EndIf => {
                        break;
                    }
                    _ => {}
                }
            }

            block.push(self.bump().expect("current stmt should exist"));
        }

        block
    }
}

#[derive(Debug)]
struct OptionGroupIter<'a> {
    errors: &'a ErrorCtx,
    iter: FlattenIter,
    shortcut_group: Option<Vec<ShortcutOptionClausePart>>,
}

impl<'a> OptionGroupIter<'a> {
    fn new(errors: &'a ErrorCtx, iter: FlattenIter) -> Self {
        OptionGroupIter {
            errors,
            iter,
            shortcut_group: None,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct ShortcutOptionClausePart {
    span: Span,
    option: ShortcutOption,
    decorator_command: Option<Command>,
    hashtags: Vec<Hashtag>,
    pragmas: Vec<Pragma>,
    block: Option<Block>,
}

impl<'a> Iterator for OptionGroupIter<'a> {
    type Item = Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(stmt) = self.iter.peek() {
            // Try building current shortcut group
            if let Some(shortcut_group) = self.shortcut_group.as_mut() {
                let last = shortcut_group
                    .last_mut()
                    .expect("there should at least one option in group");

                match &stmt.body.kind {
                    StmtKind::ShortcutOption(_) => {
                        let last_span = last.span;
                        last.block
                            .get_or_insert_with(|| Block::empty(last_span.empty_end()));

                        let stmt = self.iter.next().expect("value guaranteed by peek");
                        let span = stmt.span;
                        let option = if let StmtKind::ShortcutOption(option) = stmt.body.kind {
                            option
                        } else {
                            unreachable!("pattern checked above");
                        };

                        shortcut_group.push(ShortcutOptionClausePart {
                            span,
                            option,
                            pragmas: stmt.pragmas,
                            hashtags: stmt.hashtags,
                            decorator_command: stmt.decorator_command,
                            block: None,
                        });
                        continue;
                    }
                    StmtKind::Block(_) if last.block.is_none() => {
                        let stmt = self.iter.next().expect("value guaranteed by peek");
                        last.span = last.span.union(stmt.span);
                        let mut block = if let StmtKind::Block(block) = stmt.body.kind {
                            block
                        } else {
                            unreachable!("pattern checked above");
                        };

                        block.pragmas.append(&mut last.pragmas);

                        // Visit associated blocks recursively
                        BlockRegroup::new(self.errors).visit_block_mut(&mut block);

                        last.block = Some(block);
                        continue;
                    }
                    _ => {}
                };

                let shortcut_group = self.shortcut_group.take().expect("group should exist");
                let shortcuts_stmt = make_shortcuts_stmt(shortcut_group);

                return Some(Stmt {
                    span: shortcuts_stmt.span,
                    pragmas: Vec::new(),
                    body: StmtBody {
                        span: shortcuts_stmt.span,
                        kind: StmtKind::Shortcuts(shortcuts_stmt),
                    },
                    decorator_command: None,
                    hashtags: Vec::new(),
                });
            }

            match &stmt.body.kind {
                // Flatten blocks
                StmtKind::Block(_) => {
                    let stmt = self.iter.next().expect("stmt should exist");
                    let block = if let StmtKind::Block(block) = stmt.body.kind {
                        block
                    } else {
                        unreachable!("pattern guaranteed by code above");
                    };

                    for pragma in &stmt.pragmas {
                        self.errors
                            .warn("outer pragmas on syntactic blocks are ignored")
                            .span(pragma.span)
                            .annotate_span(
                                block.span,
                                "this block will be flattened during parsing",
                            );
                    }

                    for pragma in &block.pragmas {
                        self.errors
                            .warn("inner pragmas on syntactic blocks are ignored")
                            .span(pragma.span)
                            .annotate_span(
                                block.span,
                                "this block will be flattened during parsing",
                            );
                    }

                    self.iter.push_iter(block.stmts.into_iter());
                }
                // Start a new shortcut option group
                StmtKind::ShortcutOption(_) => {
                    let stmt = self.iter.next().expect("value guaranteed by peek");
                    let span = stmt.span;
                    let option = if let StmtKind::ShortcutOption(option) = stmt.body.kind {
                        option
                    } else {
                        unreachable!("pattern checked above");
                    };

                    let old = self.shortcut_group.replace(vec![ShortcutOptionClausePart {
                        span,
                        option,
                        pragmas: stmt.pragmas,
                        hashtags: stmt.hashtags,
                        decorator_command: stmt.decorator_command,
                        block: None,
                    }]);

                    assert!(old.is_none());
                }
                _ => return Some(self.iter.next().expect("stmt should exist")),
            }
        }

        if let Some(shortcut_group) = self.shortcut_group.take() {
            if !shortcut_group.is_empty() {
                let shortcuts_stmt = make_shortcuts_stmt(shortcut_group);

                return Some(Stmt {
                    span: shortcuts_stmt.span,
                    pragmas: Vec::new(),
                    body: StmtBody {
                        span: shortcuts_stmt.span,
                        kind: StmtKind::Shortcuts(shortcuts_stmt),
                    },
                    decorator_command: None,
                    hashtags: Vec::new(),
                });
            }
        }

        None
    }
}

fn make_shortcuts_stmt(shortcut_group: Vec<ShortcutOptionClausePart>) -> ShortcutsStmt {
    let mut stmt_span = None;

    let options = shortcut_group
        .into_iter()
        .map(|part| {
            let span = part.span;
            let stmt_span = stmt_span.get_or_insert(span);
            *stmt_span = stmt_span.union(span);

            let block = part.block.unwrap_or_else(|| Block::empty(span));
            ShortcutOptionClause {
                span,
                option: part.option,
                decorator_command: part.decorator_command,
                hashtags: part.hashtags,
                block,
            }
        })
        .collect::<Vec<_>>();

    ShortcutsStmt {
        span: stmt_span.expect("there should be at least one option"),
        options,
    }
}

#[derive(Debug)]
struct FlattenIter {
    next: Option<Stmt>,
    stack: Vec<vec::IntoIter<Stmt>>,
}

impl FlattenIter {
    fn new(iter: vec::IntoIter<Stmt>) -> Self {
        FlattenIter {
            next: None,
            stack: vec![iter],
        }
    }

    fn peek(&mut self) -> Option<&Stmt> {
        if self.next.is_none() {
            self.next = self.source_next();
        }

        self.next.as_ref()
    }

    fn next(&mut self) -> Option<Stmt> {
        if self.next.is_none() {
            self.next = self.source_next();
        }

        self.next.take()
    }

    fn push_iter(&mut self, iter: vec::IntoIter<Stmt>) {
        self.stack.push(iter);
    }

    fn source_next(&mut self) -> Option<Stmt> {
        while let Some(source) = self.stack.last_mut() {
            if let Some(stmt) = source.next() {
                return Some(stmt);
            }

            self.stack.pop();
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Use pretty_assertions for `assert_eq` diffs.
    use pretty_assertions::assert_eq;

    fn stmt(body: StmtKind) -> Stmt {
        Stmt {
            span: Span::new(0, 0),
            pragmas: Vec::new(),
            body: StmtBody {
                span: Span::new(0, 0),
                kind: body,
            },
            decorator_command: None,
            hashtags: Vec::new(),
        }
    }

    fn command(kind: CommandKind) -> Stmt {
        stmt(StmtKind::Command(Command {
            span: Span::new(0, 0),
            kind,
        }))
    }

    fn expr() -> Box<crate::ast::Expr> {
        Box::new(crate::ast::Expr {
            kind: crate::ast::ExprKind::Err,
            span: Span::new(0, 0),
        })
    }

    fn block(stmts: Vec<Stmt>) -> Block {
        Block {
            span: Span::new(0, 0),
            pragmas: Vec::new(),
            stmts,
        }
    }

    fn block_stmt(stmts: Vec<Stmt>) -> Stmt {
        stmt(StmtKind::Block(block(stmts)))
    }

    fn shortcut_option() -> Stmt {
        stmt(StmtKind::ShortcutOption(ShortcutOption {
            span: Span::new(0, 0),
            text: crate::ast::StrBody {
                segments: Vec::new(),
                span: Span::new(0, 0),
            },
        }))
    }

    fn shortcut_option_clause(block: Block) -> ShortcutOptionClause {
        ShortcutOptionClause {
            span: Span::new(0, 0),
            option: ShortcutOption {
                span: Span::new(0, 0),
                text: crate::ast::StrBody {
                    segments: Vec::new(),
                    span: Span::new(0, 0),
                },
            },
            decorator_command: None,
            hashtags: Vec::new(),
            block,
        }
    }

    #[test]
    fn test_block_regroup() {
        let mut source_block = Block {
            span: Span::new(0, 0),
            pragmas: Vec::new(),
            stmts: vec![
                command(CommandKind::If(expr())),
                block_stmt(vec![
                    command(CommandKind::ElseIf(expr())),
                    shortcut_option(),
                    block_stmt(vec![
                        command(CommandKind::If(expr())),
                        block_stmt(vec![command(CommandKind::Else)]),
                        block_stmt(vec![command(CommandKind::EndIf)]),
                    ]),
                    shortcut_option(),
                    block_stmt(vec![shortcut_option(), shortcut_option()]),
                    shortcut_option(),
                ]),
                command(CommandKind::EndIf),
                command(CommandKind::If(expr())),
                block_stmt(vec![
                    block_stmt(vec![command(CommandKind::Else)]),
                    command(CommandKind::EndIf),
                ]),
            ],
        };

        let errors = ErrorCtx::new();
        BlockRegroup::new(&errors).visit_block_mut(&mut source_block);
        assert!(errors.is_empty(), "should not emit any errors");

        let expected_block = Block {
            span: Span::new(0, 0),
            pragmas: Vec::new(),
            stmts: vec![
                stmt(StmtKind::If(IfStmt {
                    span: Span::new(0, 0),
                    if_clause: IfClause {
                        span: Span::new(0, 0),
                        condition: expr(),
                        block: block(vec![]),
                    },
                    else_if_clauses: vec![IfClause {
                        span: Span::new(0, 0),
                        condition: expr(),
                        block: block(vec![stmt(StmtKind::Shortcuts(ShortcutsStmt {
                            span: Span::new(0, 0),
                            options: vec![
                                shortcut_option_clause(block(vec![stmt(StmtKind::If(IfStmt {
                                    span: Span::new(0, 0),
                                    if_clause: IfClause {
                                        span: Span::new(0, 0),
                                        condition: expr(),
                                        block: block(vec![]),
                                    },
                                    else_if_clauses: Vec::new(),
                                    else_block: Some(block(vec![])),
                                    invalid: None,
                                }))])),
                                shortcut_option_clause(block(vec![stmt(StmtKind::Shortcuts(
                                    ShortcutsStmt {
                                        span: Span::new(0, 0),
                                        options: vec![
                                            shortcut_option_clause(block(vec![])),
                                            shortcut_option_clause(block(vec![])),
                                        ],
                                    },
                                ))])),
                                shortcut_option_clause(block(vec![])),
                            ],
                        }))]),
                    }],
                    else_block: None,
                    invalid: None,
                })),
                stmt(StmtKind::If(IfStmt {
                    span: Span::new(0, 0),
                    if_clause: IfClause {
                        span: Span::new(0, 0),
                        condition: expr(),
                        block: block(vec![]),
                    },
                    else_if_clauses: Vec::new(),
                    else_block: Some(block(vec![])),
                    invalid: None,
                })),
            ],
        };

        assert_eq!(expected_block, source_block);
    }
}
