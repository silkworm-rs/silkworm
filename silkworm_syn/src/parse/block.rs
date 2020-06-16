use crate::ast;
use crate::ptr::P;
use crate::token::{BinOp, Delim, Keyword as K, Kind as T, PragmaStyle, Token};

use super::{PResult, Parser};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    pub fn parse_block(&mut self, _term: T) -> PResult<'a, ast::Block> {
        unimplemented!()
    }

    pub fn parse_stmt(&mut self) -> PResult<'a, ast::Stmt> {
        let mut pragmas = Vec::new();
        while self.check(T::Pragma(PragmaStyle::Outer)) {
            pragmas.push(self.parse_pragma()?);
        }

        let body = self.parse_stmt_body()?;

        let decorator_command = self
            .eat(T::OpenDelim(Delim::DoubleAngleBracket))
            .map(|_| {
                let command = self.parse_command()?;
                if self.eat(T::CloseDelim(Delim::DoubleAngleBracket)).is_none() {
                    return Err(self.expect(T::CloseDelim(Delim::DoubleAngleBracket)));
                }
                Ok(command)
            })
            .transpose()?;

        let mut hashtags = Vec::new();
        while self.check(T::Hash) {
            hashtags.push(self.parse_hashtag()?);
        }

        if self.eat(T::LineBreak).is_none() {
            return Err(self.expect(T::LineBreak));
        }

        let associated_block = self
            .eat(T::Indent)
            .map(|_| {
                let block = self.parse_block(T::UnIndent)?;
                if self.eat(T::UnIndent).is_none() {
                    return Err(self.expect(T::UnIndent));
                }
                let _ = self.eat(T::LineBreak);
                Ok(block)
            })
            .transpose()?;

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
        use super::{Parse, ParseCtx};
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

        let source = span.read(self.ctx.source, self.ctx.span_base);

        let errors = self.ctx.errors;

        let ctx = ParseCtx {
            errors,
            interner: &mut self.ctx.interner,
            source,
            span_base: span.base,
        };

        let lex_stream = lex::LexStream::with_modes(
            source,
            span.base,
            lex::BlockMode::Body,
            lex::InlineMode::InterpolatedStringLiteral,
        );

        let str_body = ast::StrBody::parse_with_ctx(
            ctx,
            lex_stream.filter_map(|result| match result {
                Ok(tok) => Some(tok),
                Err(err) => {
                    errors.bug(format!("fatal lexer error: {}", err));
                    None
                }
            }),
        )
        .map_err(|_| errors.error("invalid custom command body").span(span))?;

        Ok(ast::Command {
            span,
            kind: ast::CommandKind::Custom(str_body),
        })
    }

    pub fn parse_flow(&mut self) -> PResult<'a, ast::Flow> {
        unimplemented!()
    }

    fn parse_stmt_text_body(&mut self) -> PResult<'a, ast::StrBody> {
        let (str_body, _) = self.parse_str_body_with_terminator_parser(|p| match p.token.kind {
            T::LineBreak | T::OpenDelim(Delim::DoubleAngleBracket) | T::Hash => Some(()),
            _ => None,
        })?;

        Ok(str_body)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use silkworm_sourcemap::Span;

    use crate::parse::test_utils::assert_parse;
    use crate::parse::Parse;

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
