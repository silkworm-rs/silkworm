use crate::ast;
use crate::token::{Delim, Kind as T, PragmaStyle as Style, Token};
use crate::Span;

use super::list::parse_list_sep_with_term;
use super::{PResult, Parser};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    pub fn parse_pragma(&mut self) -> PResult<'a, ast::Pragma> {
        let base_span = self.token.span;

        let style = match self.token.kind {
            T::Pragma(style) => style,
            _ => {
                return Err(self.expect_one_of(&[T::Pragma(Style::Outer), T::Pragma(Style::Inner)]))
            }
        };

        self.bump();

        let (meta, meta_span) = self.parse_meta_list(T::LineBreak);
        let span = base_span.union(meta_span);

        if meta.is_empty() {
            return Err(self
                .ctx
                .errors
                .error("expecting at least one meta item")
                .span(meta_span));
        }

        Ok(ast::Pragma {
            span,
            style: style.into(),
            meta,
        })
    }

    /// Parses a list of inner pragmas. Terminates when outer ones are found.
    pub fn parse_inner_pragmas(&mut self) -> (Vec<ast::Pragma>, Span) {
        let mut pragmas = Vec::new();
        let span = self.token.span.empty();

        while let Some(style) = self.check_pragma() {
            if let Ok(pragma) = self.parse_pragma_line() {
                if style == Style::Inner {
                    pragmas.push(pragma);
                } else {
                    break;
                }
            }
        }

        let span = span.union(self.token.span.empty());
        (pragmas, span)
    }

    /// Parses a list of outer pragmas and errors if inner ones are found.
    pub fn parse_outer_pragmas(&mut self) -> (Vec<ast::Pragma>, Span) {
        let mut pragmas = Vec::new();
        let span = self.token.span.empty();

        while let Some(style) = self.check_pragma() {
            if let Ok(pragma) = self.parse_pragma_line() {
                if style == Style::Outer {
                    pragmas.push(pragma);
                } else {
                    self.ctx
                        .errors
                        .error("inner pragmas are only allowed at start of blocks and files")
                        .span(pragma.span);
                }
            }
        }

        let span = span.union(self.token.span.empty());
        (pragmas, span)
    }

    /// Parse pragma lines consuming the terminator symbol
    pub fn parse_pragma_line(&mut self) -> PResult<'a, ast::Pragma> {
        let pragma = self.parse_pragma();

        if self.eat(T::LineBreak).is_none() {
            let span = self.eat_until_end_of_line();
            self.expect(T::LineBreak)
                .maybe_annotate_span(span, "expected meta list");

            if self.eat(T::LineBreak).is_none() {
                return Err(self
                    .ctx
                    .errors
                    .error("expected statement")
                    .span(self.token.span));
            }
        }

        pragma
    }

    pub fn parse_meta(&mut self) -> PResult<'a, ast::Meta> {
        let path = self.parse_path()?;

        if self.eat(T::OpenDelim(Delim::Paren)).is_some() {
            let (args, _) = self.parse_meta_list(T::CloseDelim(Delim::Paren));

            Ok(ast::Meta {
                path,
                args: Some(args),
            })
        } else {
            Ok(ast::Meta { path, args: None })
        }
    }

    fn parse_meta_list(&mut self, terminator: T) -> (Vec<ast::Meta>, Span) {
        self.parse_list_with(
            true,
            parse_list_sep_with_term(true, T::Comma, terminator),
            |p, span| {
                p.expect_one_of(&[T::Comma, terminator]).span(span);
            },
            |p| p.parse_meta().ok(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parse::test_utils::assert_parse;
    use crate::parse::Parse;

    #[test]
    fn can_parse_meta() {
        assert_parse("foo", |itn| ast::Meta {
            path: ast::Path::parse_with_interner("foo", 0, itn).unwrap(),
            args: None,
        });

        assert_parse("foo.bar", |itn| ast::Meta {
            path: ast::Path::parse_with_interner("foo.bar", 0, itn).unwrap(),
            args: None,
        });

        assert_parse("foo.bar ( foo( foo .baz() ))", |itn| ast::Meta {
            path: ast::Path::parse_with_interner("foo.bar", 0, itn).unwrap(),
            args: Some(vec![ast::Meta {
                path: ast::Path::parse_with_interner("foo", 10, itn).unwrap(),
                args: Some(vec![ast::Meta {
                    path: ast::Path::parse_with_interner("foo .baz", 15, itn).unwrap(),
                    args: Some(vec![]),
                }]),
            }]),
        });

        assert_parse("foo.bar(foo, foo.baz(quux))", |itn| ast::Meta {
            path: ast::Path::parse_with_interner("foo.bar", 0, itn).unwrap(),
            args: Some(vec![
                ast::Meta {
                    path: ast::Path::parse_with_interner("foo", 8, itn).unwrap(),
                    args: None,
                },
                ast::Meta {
                    path: ast::Path::parse_with_interner("foo.baz", 13, itn).unwrap(),
                    args: Some(vec![ast::Meta {
                        path: ast::Path::parse_with_interner("quux", 21, itn).unwrap(),
                        args: None,
                    }]),
                },
            ]),
        });
    }

    #[test]
    fn can_parse_pragma() {
        assert_parse("//# foo(bar), bar, baz(foo)", |itn| ast::Pragma {
            span: Span::new(0, 27),
            style: ast::PragmaStyle::Outer,
            meta: vec![
                ast::Meta::parse_with_interner("foo(bar)", 4, itn).unwrap(),
                ast::Meta::parse_with_interner("bar", 14, itn).unwrap(),
                ast::Meta::parse_with_interner("baz(foo)", 42, itn).unwrap(),
            ],
        });

        assert_parse("//#! foo, bar, ", |itn| ast::Pragma {
            span: Span::new(0, 15),
            style: ast::PragmaStyle::Inner,
            meta: vec![
                ast::Meta::parse_with_interner("foo", 4, itn).unwrap(),
                ast::Meta::parse_with_interner("bar", 10, itn).unwrap(),
            ],
        });

        ast::Pragma::parse("//#", 0).unwrap_err();
        ast::Pragma::parse("//# //#! foo", 0).unwrap_err();
        ast::Pragma::parse("//#! foo((bar(baz), bar, )", 0).unwrap_err();
    }
}
