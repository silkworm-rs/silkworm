use crate::ast;
use crate::token::{HeaderKey as H, Keyword as K, Kind as T, Token};

use super::{PResult, Parser};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    /// Parse a file until EoF.
    pub fn parse_file(&mut self) -> PResult<'a, ast::File> {
        // Skip outermost indentations
        let _ = self.eat(T::Indent);

        let (pragmas, span) = self.parse_inner_pragmas();

        let mut nodes = Vec::new();
        while !self.is_almost_eof() {
            nodes.extend(self.parse_node().ok());

            if let Some(err_span) = self.eat_lines_until(T::TripleEq) {
                self.expect(T::TripleEq).span(err_span);
            }

            if self.eat(T::TripleEq).is_some()
                && (!self.is_eof() && self.eat(T::LineBreak).is_none())
            {
                if let Some(err_span) = self.eat_until_end_of_line() {
                    self.expect(T::LineBreak).span(err_span);
                }
                let _ = self.eat(T::LineBreak);
            }
        }

        // Skip outermost indentations
        let _ = self.eat(T::UnIndent);

        if let Some(err_span) = self.eat_lines_until(T::Eof) {
            self.expect(T::Eof).span(err_span);
        }

        let span = span.union(self.token.span.empty());

        Ok(ast::File {
            inner_pragmas: pragmas,
            span,
            nodes,
        })
    }

    /// Parse a node until right before the terminator (`===` or EoF).
    pub fn parse_node(&mut self) -> PResult<'a, ast::Node> {
        let (pragmas, span) = self.parse_outer_pragmas();

        let mut headers = Vec::new();
        while !self.is_eof() && !self.check(T::TripleDash) {
            headers.extend(
                self.parse_or_eat_till(T::LineBreak, Self::parse_node_header)
                    .ok(),
            );
            let _ = self.eat(T::LineBreak);
        }

        if self.eat(T::TripleDash).is_none() {
            return Err(self.expect(T::TripleDash));
        }

        if self.eat(T::LineBreak).is_none() {
            self.expect(T::LineBreak);
        }

        let body = match self.parse_block(T::TripleEq) {
            Ok(body) => body,
            Err(_) => ast::Block::empty(span.empty()),
        };

        let span = span.union(self.token.span.empty());

        Ok(ast::Node {
            span,
            outer_pragmas: pragmas,
            headers,
            body,
        })
    }

    pub fn parse_node_header(&mut self) -> PResult<'a, ast::NodeHeader> {
        let key = self.parse_path()?;

        if self.eat(T::Colon).is_none() {
            return Err(self.expect(T::Colon));
        }

        match key.as_segment() {
            Some(seg) if seg.keyword == Some(K::HeaderKey(H::Title)) => {
                let path = self.parse_or_eat_till(T::LineBreak, Self::parse_path)?;
                Ok(ast::NodeHeader::Title(path))
            }
            Some(seg) if seg.keyword == Some(K::HeaderKey(H::Tags)) => {
                let mut tags = Vec::new();
                while !self.is_eof() && self.token.kind != T::LineBreak {
                    tags.extend(self.parse_path().ok());
                }
                if !self.is_end_of_line() {
                    let span = self.eat_until_end_of_line();
                    self.expect(T::LineBreak)
                        .maybe_annotate_span(span, "expected paths as tags");
                }
                Ok(ast::NodeHeader::Tags(tags))
            }
            _ => {
                if let Some(span) = self.eat_until_end_of_line() {
                    Ok(ast::NodeHeader::Custom(key, span))
                } else {
                    Err(self
                        .ctx
                        .errors
                        .error("expecting header value")
                        .span(self.token.span.empty()))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parse::test_utils::assert_parse;
    use crate::parse::Parse;
    use crate::Span;

    #[test]
    fn can_parse_node_header() {
        assert_parse("title: fish.life", |itn| {
            ast::NodeHeader::Title(ast::Path::parse_with_interner("fish.life", 7, itn).unwrap())
        });

        assert_parse("tags: fish.life foo bar.baz", |itn| {
            ast::NodeHeader::Tags(vec![
                ast::Path::parse_with_interner("fish.life", 6, itn).unwrap(),
                ast::Path::parse_with_interner("foo", 16, itn).unwrap(),
                ast::Path::parse_with_interner("bar.baz", 20, itn).unwrap(),
            ])
        });

        assert_parse("custom.header: wow such text", |itn| {
            ast::NodeHeader::Custom(
                ast::Path::parse_with_interner("custom.header", 0, itn).unwrap(),
                Span::new(14, 14),
            )
        });
    }

    #[test]
    #[rustfmt::skip::macros(concat)]
    fn can_parse_node() {
        assert_parse(
            concat!(
                "title: fish.life\n",
                "---\n",
                "Foo: bar\n",
            ),
            |itn| ast::Node {
                span: Span::new(0, 30),
                outer_pragmas: Vec::new(),
                headers: vec![
                    ast::NodeHeader::parse_with_interner("title: fish.life", 0, itn).unwrap(),
                ],
                body: ast::Block {
                    span: Span::new(21, 9),
                    inner_pragmas: Vec::new(),
                    stmts: vec![ast::Stmt::parse_with_interner("Foo: bar", 21, itn).unwrap()],
                },
            },
        );
    }

    #[test]
    #[rustfmt::skip::macros(concat)]
    fn can_parse_file() {
        assert_parse(
            concat!(
                "//#! feature(fish)\n",
                "title: fish.life\n",
                "---\n",
                "Foo: bar\n",
                "===\n",
                "title: wow\n",
                "---\n",
                "wow\n",
            ),
            |itn| ast::File {
                inner_pragmas: vec![
                    ast::Pragma::parse_with_interner("//#! feature(fish)", 0, itn).unwrap(),
                ],
                span: Span::new(0, 72),
                nodes: vec![
                    ast::Node::parse_with_interner(
                        concat!(
                            "title: fish.life\n",
                            "---\n",
                            "Foo: bar\n",
                        ),
                        19,
                        itn,
                    )
                    .unwrap(),
                    ast::Node::parse_with_interner(
                        concat!(
                            "title: wow\n",
                            "---\n",
                            "wow\n",
                        ),
                        53,
                        itn,
                    )
                    .unwrap(),
                ],
            },
        );
    }
}
