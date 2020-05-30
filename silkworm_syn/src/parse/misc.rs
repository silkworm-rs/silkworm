use crate::ast;

use crate::token::{Kind as T, Token};

use super::list::ListSep;
use super::{PResult, Parser};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    pub fn parse_path(&mut self) -> PResult<'a, ast::Path> {
        let (segments, span) = self.parse_list_with(
            false,
            |p| {
                if p.eat(T::Period).is_some() {
                    Some(ListSep::Sep)
                } else {
                    Some(ListSep::Term)
                }
            },
            |p, span| {
                p.expect(T::Period).span(span);
            },
            |p| match p.eat_symbol() {
                Some((symbol, span)) => Some(ast::PathSegment { symbol, span }),
                None => {
                    let builder = p.expect(T::Ident);
                    if p.last_token.kind == T::Period {
                        builder.annotate_span(
                            p.last_token.span,
                            "trailing periods not allowed in paths",
                        );
                    }

                    None
                }
            },
        );

        if segments.is_empty() {
            return Err(self.expect(T::Ident));
        }

        Ok(ast::Path { span, segments })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use silkworm_sourcemap::Span;

    use crate::parse::test_utils::assert_parse;
    use crate::parse::Parse;

    #[test]
    fn can_parse_path() {
        assert_parse("foo", |itn| ast::Path {
            span: Span::new(0, 3),
            segments: vec![ast::PathSegment::new(itn, "foo", Span::new(0, 3))],
        });

        assert_parse("foo.bar.baz", |itn| ast::Path {
            span: Span::new(0, 11),
            segments: vec![
                ast::PathSegment::new(itn, "foo", Span::new(0, 3)),
                ast::PathSegment::new(itn, "bar", Span::new(4, 3)),
                ast::PathSegment::new(itn, "baz", Span::new(8, 3)),
            ],
        });

        assert_parse("foo. bar. baz . quux", |itn| ast::Path {
            span: Span::new(0, 20),
            segments: vec![
                ast::PathSegment::new(itn, "foo", Span::new(0, 3)),
                ast::PathSegment::new(itn, "bar", Span::new(5, 3)),
                ast::PathSegment::new(itn, "baz", Span::new(10, 3)),
                ast::PathSegment::new(itn, "quux", Span::new(16, 4)),
            ],
        });

        ast::Path::parse("", 0).unwrap_err();
        ast::Path::parse(".", 0).unwrap_err();
        ast::Path::parse("foo.bar.baz.", 0).unwrap_err();
    }
}
