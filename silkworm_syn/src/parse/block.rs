use crate::ast;
use crate::ptr::P;
use crate::token::{BinOp, Delim, Keyword as K, Kind as T, PragmaStyle, Token};

use super::{PResult, Parser};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    /// Parse a block with a terminator without consuming it.
    pub fn parse_block(&mut self, term: T) -> PResult<'a, ast::Block> {
        let span = self.token.span.empty();

        let mut pragmas = Vec::new();
        while let Some(style) = self.check_pragma() {
            if let Ok(pragma) = self.parse_pragma_line() {
                if style == PragmaStyle::Inner {
                    pragmas.push(pragma);
                } else {
                    break;
                }
            }
        }

        let mut stmts = Vec::new();
        while !self.is_eof() && !self.check(term) {
            stmts.extend(self.parse_or_eat_till(term, Self::parse_stmt).ok());

            if self.eat(T::LineBreak).is_none() {
                self.expect(T::LineBreak);
            }
        }

        if self.is_eof() {
            self.expect(term);
        }

        let span = span.union(self.token.span);

        Ok(ast::Block {
            span,
            inner_pragmas: pragmas,
            stmts,
        })
    }

    /// Parse a full statement, without consuming the line-break.
    pub fn parse_stmt(&mut self) -> PResult<'a, ast::Stmt> {
        let mut pragmas = Vec::new();
        while let Some(style) = self.check_pragma() {
            if let Ok(pragma) = self.parse_pragma_line() {
                if style == PragmaStyle::Outer {
                    pragmas.push(pragma);
                } else {
                    self.ctx
                        .errors
                        .error("inner pragmas are only allowed at start of blocks")
                        .span(pragma.span);
                }
            }
        }

        let body = self.parse_stmt_body()?;

        let decorator_command = self
            .eat(T::OpenDelim(Delim::DoubleAngleBracket))
            .and_then(|_| {
                let command = self.parse_command().ok()?;
                if self.eat(T::CloseDelim(Delim::DoubleAngleBracket)).is_none() {
                    self.expect(T::CloseDelim(Delim::DoubleAngleBracket));
                    return None;
                }
                Some(command)
            });

        let mut hashtags = Vec::new();
        while self.check(T::Hash) {
            hashtags.extend(self.parse_hashtag().ok());
        }

        if !self.is_eof() && !self.check(T::LineBreak) {
            let span = self.eat_until_end_of_line();
            self.expect(T::LineBreak)
                .maybe_annotate_span(span, "extra tokens in statement");
        }

        let associated_block = if Some(true) == self.check_nth(0, T::Whitespace)
            && Some(true) == self.check_nth(1, T::Indent)
        {
            self.eat(T::LineBreak)
                .expect("the code above should guarantee a line-break");
            self.eat(T::Indent)
                .expect("the code above should guarantee an indent");

            self.parse_block(T::UnIndent).ok().and_then(|block| {
                if self.eat(T::UnIndent).is_some() {
                    Some(block)
                } else {
                    self.expect(T::UnIndent);
                    None
                }
            })
        } else {
            None
        };

        Ok(ast::Stmt {
            pragmas,
            body,
            decorator_command,
            hashtags,
            associated_block,
        })
    }

    pub fn parse_stmt_body(&mut self) -> PResult<'a, ast::StmtBody> {
        match self.token.kind {
            T::OpenDelim(Delim::DoubleAngleBracket) => {
                let span = self.bump().span;

                let command = self.parse_or_eat_till(
                    T::CloseDelim(Delim::DoubleAngleBracket),
                    Self::parse_command,
                )?;

                if let Some(delim) = self.eat(T::CloseDelim(Delim::DoubleAngleBracket)) {
                    Ok(ast::StmtBody {
                        span: span.union(delim.span),
                        kind: ast::StmtKind::Command(command),
                    })
                } else {
                    Err(self.expect(T::CloseDelim(Delim::DoubleAngleBracket)))
                }
            }
            T::OpenDelim(Delim::DoubleBracket) => {
                let span = self.bump().span;

                let flow =
                    self.parse_or_eat_till(T::CloseDelim(Delim::DoubleBracket), Self::parse_flow)?;

                if let Some(delim) = self.eat(T::CloseDelim(Delim::DoubleBracket)) {
                    Ok(ast::StmtBody {
                        span: span.union(delim.span),
                        kind: ast::StmtKind::Flow(flow),
                    })
                } else {
                    Err(self.expect(T::CloseDelim(Delim::DoubleBracket)))
                }
            }
            T::Arrow => {
                let shortcut_option = self.parse_shortcut_option()?;
                Ok(ast::StmtBody {
                    span: shortcut_option.span,
                    kind: ast::StmtKind::ShortcutOption(shortcut_option),
                })
            }
            T::Indent => {
                let block = self.parse_block(T::UnIndent)?;
                Ok(ast::StmtBody {
                    span: block.span,
                    kind: ast::StmtKind::Block(block),
                })
            }
            _ => {
                let text = self.parse_stmt_text_body()?;

                Ok(ast::StmtBody {
                    span: text.span,
                    kind: ast::StmtKind::Text(text),
                })
            }
        }
    }

    pub fn parse_hashtag(&mut self) -> PResult<'a, ast::Hashtag> {
        let hash = self.eat(T::Hash).ok_or_else(|| self.expect(T::Hash))?;
        let text = self.parse_text()?;

        Ok(ast::Hashtag {
            span: hash.span.union(text.span),
            text,
        })
    }

    pub fn parse_shortcut_option(&mut self) -> PResult<'a, ast::ShortcutOption> {
        let span = self
            .eat(T::Arrow)
            .ok_or_else(|| self.expect(T::Arrow))?
            .span;

        let text = self.parse_stmt_text_body()?;
        let span = span.union(text.span);

        Ok(ast::ShortcutOption { span, text })
    }

    pub fn parse_command(&mut self) -> PResult<'a, ast::Command> {
        match self.token.kind {
            T::Keyword(K::Set) => self.parse_set_command(),
            T::Keyword(K::Call) => self.parse_expr_command(ast::CommandKind::Call),
            T::Keyword(K::If) => self.parse_expr_command(ast::CommandKind::If),
            T::Keyword(K::ElseIf) => self.parse_expr_command(ast::CommandKind::ElseIf),
            T::Keyword(K::Else) => Ok(ast::Command {
                span: self.bump().span,
                kind: ast::CommandKind::Else,
            }),
            T::Keyword(K::EndIf) => Ok(ast::Command {
                span: self.bump().span,
                kind: ast::CommandKind::EndIf,
            }),
            T::Keyword(K::Return) => self.parse_return_command(),
            _ => self.parse_custom_command(),
        }
    }

    fn parse_set_command(&mut self) -> PResult<'a, ast::Command> {
        let span = self
            .eat(T::Keyword(K::Set))
            .ok_or_else(|| self.expect(T::Keyword(K::Return)))?
            .span;

        let var = self.parse_var()?;

        match self.token.kind {
            T::Eq | T::Keyword(K::To) => {
                self.bump();
            }
            _ => return Err(self.expect_one_of(&[T::Eq, T::Keyword(K::To)])),
        }

        let expr = self.parse_expr()?;

        Ok(ast::Command {
            span: span.union(expr.span),
            kind: ast::CommandKind::Set(var, P(expr)),
        })
    }

    fn parse_expr_command<F>(&mut self, ctor: F) -> PResult<'a, ast::Command>
    where
        F: FnOnce(P<ast::Expr>) -> ast::CommandKind,
    {
        let span = self.bump().span;

        let expr =
            self.parse_or_eat_till(T::CloseDelim(Delim::DoubleAngleBracket), Self::parse_expr)?;

        Ok(ast::Command {
            span: span.union(expr.span),
            kind: ctor(P(expr)),
        })
    }

    fn parse_return_command(&mut self) -> PResult<'a, ast::Command> {
        let span = self
            .eat(T::Keyword(K::Return))
            .ok_or_else(|| self.expect(T::Keyword(K::Return)))?
            .span;

        if self.token.kind == T::CloseDelim(Delim::DoubleAngleBracket) {
            return Ok(ast::Command {
                span,
                kind: ast::CommandKind::Return(None),
            });
        }

        let expr =
            self.parse_or_eat_till(T::CloseDelim(Delim::DoubleAngleBracket), Self::parse_expr)?;

        Ok(ast::Command {
            span: span.union(expr.span),
            kind: ast::CommandKind::Return(Some(P(expr))),
        })
    }

    fn parse_custom_command(&mut self) -> PResult<'a, ast::Command> {
        use crate::lex;

        // re-interpret tokens as string
        let (_, span) = self.eat_until_with_or_end_of_line(|p| {
            if p.check(T::CloseDelim(Delim::DoubleAngleBracket)) {
                Some(())
            } else {
                None
            }
        });

        let span = span.ok_or_else(|| {
            self.ctx
                .errors
                .error("expecting custom command body")
                .span(self.token.span)
        })?;

        let str_body = self
            .parse_reinterpret(
                span,
                lex::BlockMode::Body,
                lex::InlineMode::InterpolatedStringLiteral,
            )
            .map_err(|_| {
                self.ctx
                    .errors
                    .error("invalid custom command body")
                    .span(span)
            })?;

        Ok(ast::Command {
            span,
            kind: ast::CommandKind::Custom(str_body),
        })
    }

    fn parse_stmt_text_body(&mut self) -> PResult<'a, ast::StrBody> {
        let (str_body, _) = self.parse_str_body_with_terminator_parser(|p| match p.token.kind {
            T::LineBreak | T::OpenDelim(Delim::DoubleAngleBracket) | T::Hash => Some(()),
            _ => None,
        })?;

        Ok(str_body)
    }

    pub fn parse_flow(&mut self) -> PResult<'a, ast::Flow> {
        // Need to re-interpret tokens depending on the presence and position of `|`.
        let mut text_span = None;
        while self.token.kind != T::Pipe && self.token.kind != T::CloseDelim(Delim::DoubleBracket) {
            let text_span = text_span.get_or_insert(self.token.span);
            *text_span = text_span.union(self.token.span);
            self.bump();
        }

        let target_span = {
            if self.token.kind == T::CloseDelim(Delim::DoubleBracket) {
                text_span.take()
            } else {
                self.bump();

                let mut target_span = None;
                while self.token.kind != T::CloseDelim(Delim::DoubleBracket) {
                    let target_span = target_span.get_or_insert(self.token.span);
                    *target_span = target_span.union(self.token.span);
                    self.bump();
                }

                target_span
            }
        };

        let (target_span, span) = if let Some(span) = target_span {
            if let Some(text_span) = text_span {
                (span, span.union(text_span))
            } else {
                (span, span)
            }
        } else {
            return Err(self
                .ctx
                .errors
                .error("expecting flow command body")
                .span(self.token.span.empty()));
        };

        let option_text = text_span.and_then(|span| {
            match self.parse_reinterpret(
                span,
                crate::lex::BlockMode::Body,
                crate::lex::InlineMode::InterpolatedStringLiteral,
            ) {
                Ok(str_body) => Some(str_body),
                Err(_) => {
                    self.ctx.errors.error("invalid option text").span(span);
                    None
                }
            }
        });

        let target = self
            .parse_reinterpret(
                target_span,
                crate::lex::BlockMode::Body,
                crate::lex::InlineMode::OptionTarget,
            )
            .map_err(|_| {
                self.ctx
                    .errors
                    .error("invalid flow target")
                    .span(target_span)
            })?;

        Ok(ast::Flow {
            span,
            option_text,
            target,
        })
    }

    pub fn parse_flow_target(&mut self) -> PResult<'a, ast::FlowTarget> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use silkworm_sourcemap::Span;

    use crate::parse::test_utils::assert_parse;
    use crate::parse::Parse;

    #[test]
    fn can_parse_stmt() {
        assert_parse(
            concat!(
                "//# foo(bar)\n",
                "-> Foo <<if $foo is $bar>>\n",
                "    <<call foo()>>\n",
            ),
            |itn| ast::Stmt {
                pragmas: vec![ast::Pragma::parse_with_interner("//# foo(bar)", 0, itn).unwrap()],
                body: ast::StmtBody {
                    span: Span::new(13, 7),
                    kind: ast::StmtKind::ShortcutOption(
                        ast::ShortcutOption::parse_with_interner("-> Foo ", 13, itn).unwrap(),
                    ),
                },
                decorator_command: Some(
                    ast::Command::parse_with_interner("if $foo is $bar", 22, itn).unwrap(),
                ),
                hashtags: Vec::new(),
                associated_block: Some(ast::Block {
                    span: Span::new(44, 15),
                    inner_pragmas: Vec::new(),
                    stmts: vec![ast::Stmt::parse_with_interner("<<call foo()>>", 44, itn).unwrap()],
                }),
            },
        );
    }

    #[test]
    fn can_parse_hashtag() {
        assert_parse("#foo: bar", |_itn| ast::Hashtag {
            span: Span::new(0, 9),
            text: ast::Text {
                span: Span::new(1, 8),
            },
        });
    }

    #[test]
    fn can_parse_shortcut_option() {
        assert_parse("-> foo {$bar}", |itn| ast::ShortcutOption {
            span: Span::new(0, 13),
            text: ast::StrBody::parse_with_interner(" foo {$bar}", 2, itn).unwrap(),
        });
    }

    #[test]
    fn can_parse_set_command() {
        assert_parse("set $foo = bar()", |itn| ast::Command {
            kind: ast::CommandKind::Set(
                ast::Var::parse_with_interner("$foo", 4, itn).unwrap(),
                P(ast::Expr::parse_with_interner("bar()", 11, itn).unwrap()),
            ),
            span: Span::new(0, 16),
        });

        assert_parse("set $foo to 42", |itn| ast::Command {
            kind: ast::CommandKind::Set(
                ast::Var::parse_with_interner("$foo", 4, itn).unwrap(),
                P(ast::Expr::parse_with_interner("42", 12, itn).unwrap()),
            ),
            span: Span::new(0, 14),
        });
    }

    #[test]
    fn can_parse_expr_command() {
        assert_parse("call foo($bar, @baz)", |itn| ast::Command {
            kind: ast::CommandKind::Call(P(ast::Expr::parse_with_interner(
                "foo($bar, @baz)",
                5,
                itn,
            )
            .unwrap())),
            span: Span::new(0, 20),
        });

        assert_parse("return 1 + foo($bar, @baz)", |itn| ast::Command {
            kind: ast::CommandKind::Return(Some(P(ast::Expr::parse_with_interner(
                "1 + foo($bar, @baz)",
                7,
                itn,
            )
            .unwrap()))),
            span: Span::new(0, 26),
        });
    }

    #[test]
    fn can_parse_custom_command() {
        assert_parse("expression {1+1}", |itn| ast::Command {
            kind: ast::CommandKind::Custom(
                ast::StrBody::parse_with_interner("expression {1+1}", 0, itn).unwrap(),
            ),
            span: Span::new(0, 16),
        });

        ast::Path::parse("expression {1+", 0).unwrap_err();
    }

    #[test]
    fn can_parse_if_command() {
        assert_parse("if $foo", |itn| ast::Command {
            kind: ast::CommandKind::If(P(ast::Expr::parse_with_interner("$foo", 3, itn).unwrap())),
            span: Span::new(0, 7),
        });

        assert_parse("elseif $foo", |itn| ast::Command {
            kind: ast::CommandKind::ElseIf(P(
                ast::Expr::parse_with_interner("$foo", 7, itn).unwrap()
            )),
            span: Span::new(0, 11),
        });

        assert_parse("else", |_itn| ast::Command {
            kind: ast::CommandKind::Else,
            span: Span::new(0, 4),
        });

        assert_parse("endif", |_itn| ast::Command {
            kind: ast::CommandKind::EndIf,
            span: Span::new(0, 5),
        });
    }
}
