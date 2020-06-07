use crate::ast;

use crate::token::{Delim, EscapeChar, Kind as T, Token};
use crate::Span;

use super::{PResult, Parser};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    /// Parses a `StrBody` with the given terminator, consuming it.
    pub fn parse_str_body_with_terminator(&mut self, terminator: T) -> PResult<'a, ast::StrBody> {
        let mut segments = Vec::new();
        let span = self.token.span;

        while {
            if self.token.kind == terminator {
                false
            } else {
                match self.token.kind {
                    T::Text
                    | T::EscapeChar(_)
                    | T::EscapeByte
                    | T::EscapeUnicode
                    | T::OpenDelim(Delim::Brace)
                    | T::OpenDelim(Delim::Bracket) => true,
                    _ => false,
                }
            }
        } {
            segments.push(self.parse_str_segment()?);
        }

        if self.eat(terminator).is_none() {
            return Err(self.expect(terminator));
        }

        let span = if let Some(last) = segments.last() {
            span.union(last.span())
        } else {
            span.empty()
        };

        Ok(ast::StrBody { segments, span })
    }

    pub fn parse_str_segment(&mut self) -> PResult<'a, ast::StrSegment> {
        match self.token.kind {
            T::Text => self.parse_str_segment_text(),
            T::EscapeChar(esc_char) => self.parse_str_segment_escape_char(esc_char),
            T::EscapeByte => self.parse_str_segment_escape_byte(),
            T::EscapeUnicode => self.parse_str_segment_escape_unicode(),
            T::OpenDelim(Delim::Brace) => self.parse_str_segment_expr(),
            T::OpenDelim(Delim::Bracket) => self.parse_str_segment_format_func(),
            _ => Err(self.expect_one_of(&[
                T::Text,
                // All `Escape*` tokens show up as "escape sequence", so just choosing a random
                // one here.
                T::EscapeByte,
                T::OpenDelim(Delim::Brace),
                T::OpenDelim(Delim::Bracket),
            ])),
        }
    }

    fn parse_str_segment_text(&mut self) -> PResult<'a, ast::StrSegment> {
        let mut span = self.token.span;
        while let Some(token) = self.eat(T::Text) {
            span = span.union(token.span);
        }
        Ok(ast::StrSegment::Text(ast::Text { span }))
    }

    fn parse_str_segment_escape_char(
        &mut self,
        esc_char: EscapeChar,
    ) -> PResult<'a, ast::StrSegment> {
        let token = self.bump();
        if esc_char == EscapeChar::Invalid {
            Err(self
                .ctx
                .errors
                .error("invalid escape sequence")
                .span(token.span))
        } else {
            Ok(ast::StrSegment::Escape(ast::Escape {
                kind: ast::EscapeKind::Char(esc_char),
                span: token.span,
            }))
        }
    }

    fn parse_str_segment_escape_byte(&mut self) -> PResult<'a, ast::StrSegment> {
        let token = self.bump();
        if token.span.len != 4 {
            Err(self
                .ctx
                .errors
                .error("invalid byte escape sequence (e.g. `\\x41`)")
                .span(token.span))
        } else {
            let seq_span = Span::new(token.span.base + 2, 2);
            let seq = seq_span.read(self.ctx.source, self.ctx.span_base);

            match u8::from_str_radix(seq, 16) {
                Ok(byte) => Ok(ast::StrSegment::Escape(ast::Escape {
                    kind: ast::EscapeKind::Byte(byte.into()),
                    span: token.span,
                })),
                Err(_) => Err(self
                    .ctx
                    .errors
                    .error(format!(
                        "invalid byte escape sequence (0x{} is not a valid hex number)",
                        seq
                    ))
                    .span(seq_span)),
            }
        }
    }

    fn parse_str_segment_escape_unicode(&mut self) -> PResult<'a, ast::StrSegment> {
        let token = self.bump();

        let valid_seq = {
            if token.span.len > 4 {
                let seq_span = Span::new(token.span.base + 2, token.span.len - 2);
                let seq = seq_span.read(self.ctx.source, self.ctx.span_base);
                assert!(!seq.is_empty(), "seq should be non-empty");

                let first = seq.chars().next().unwrap();
                let last = seq.chars().next_back().unwrap();

                if first == '{' && last == '}' {
                    Some(&seq[1..seq.len() - 1])
                } else {
                    None
                }
            } else {
                None
            }
        };

        match valid_seq {
            None => Err(self
                .ctx
                .errors
                .error("invalid unicode escape sequence (e.g. `\\u{1F980}`)")
                .span(token.span)),
            Some(seq) => match u32::from_str_radix(seq, 16) {
                Ok(code_point) => {
                    let chr = std::char::from_u32(code_point).ok_or_else(|| {
                        self.ctx
                            .errors
                            .error(format!(
                                "0x{:X} is not a valid Unicode code-point",
                                code_point
                            ))
                            .span(token.span)
                    })?;

                    let span = Span::new(token.span.base + 3, seq.len() as u32);

                    Ok(ast::StrSegment::Escape(ast::Escape {
                        kind: ast::EscapeKind::Unicode(chr, span),
                        span: token.span,
                    }))
                }
                Err(_) => Err(self
                    .ctx
                    .errors
                    .error(format!(
                        "invalid byte escape sequence (0x{} is not a valid hex number)",
                        seq
                    ))
                    .span(token.span)),
            },
        }
    }

    fn parse_str_segment_expr(&mut self) -> PResult<'a, ast::StrSegment> {
        if self.eat(T::OpenDelim(Delim::Brace)).is_none() {
            return Err(self.expect(T::OpenDelim(Delim::Brace)));
        }

        let expr = self.parse_expr()?;

        if self.eat(T::CloseDelim(Delim::Brace)).is_none() {
            return Err(self.expect(T::CloseDelim(Delim::Brace)));
        }

        Ok(ast::StrSegment::Expr(expr))
    }

    fn parse_str_segment_format_func(&mut self) -> PResult<'a, ast::StrSegment> {
        unimplemented!()
    }

    pub fn parse_format_func(&mut self) -> PResult<'a, ast::FormatFunc> {
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
    fn can_parse_str_segment_text() {
        assert_parse(r#"foo bar"#, |_itn| {
            ast::StrSegment::Text(ast::Text {
                span: Span::new(0, 7),
            })
        });

        assert_parse(r#">v0 0v<"#, |_itn| {
            ast::StrSegment::Text(ast::Text {
                span: Span::new(0, 7),
            })
        });
    }

    #[test]
    fn can_parse_str_segment_escapes() {
        assert_parse(r#"\n"#, |_itn| {
            ast::StrSegment::Escape(ast::Escape {
                kind: ast::EscapeKind::Char(EscapeChar::Newline),
                span: Span::new(0, 2),
            })
        });

        assert_parse(r#"\x41"#, |_itn| {
            ast::StrSegment::Escape(ast::Escape {
                kind: ast::EscapeKind::Byte('\x41'),
                span: Span::new(0, 4),
            })
        });

        assert_parse(r#"\u{1F980}"#, |_itn| {
            ast::StrSegment::Escape(ast::Escape {
                kind: ast::EscapeKind::Unicode('\u{1F980}', Span::new(3, 5)),
                span: Span::new(0, 9),
            })
        });

        ast::StrSegment::parse("\\p", 0).unwrap_err();
        ast::StrSegment::parse("\\xKK", 0).unwrap_err();
        ast::StrSegment::parse("\\x3", 0).unwrap_err();
        ast::StrSegment::parse("\\u{}", 0).unwrap_err();
        ast::StrSegment::parse("\\u{", 0).unwrap_err();
        ast::StrSegment::parse("\\u1F980", 0).unwrap_err();
        ast::StrSegment::parse("\\u{KZI27}", 0).unwrap_err();
    }

    #[test]
    fn can_parse_str_segment_expr() {
        assert_parse(r#"{33 - 4}"#, |itn| {
            ast::StrSegment::Expr(ast::Expr::parse_with_interner("33 - 4", 1, itn).unwrap())
        });

        ast::StrSegment::parse("{1 + 3 <<&&& foo >> bar}", 0).unwrap_err();
        ast::StrSegment::parse("{ 42", 0).unwrap_err();
    }

    #[test]
    fn can_parse_str_segment_format_func() {
        assert_parse(r#"[format_func foo="bar"]"#, |itn| {
            ast::StrSegment::FormatFunc(
                ast::FormatFunc::parse_with_interner(r#"format_func foo="bar""#, 1, itn).unwrap(),
            )
        });

        ast::StrSegment::parse("[foo bar=\"baz]", 0).unwrap_err();
    }
}
