use crate::ast;
use crate::ptr::P;
use crate::token::{Delim, EscapeChar, Kind as T, Token};
use crate::Span;

use super::list::ListSep;
use super::{PResult, Parser};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    /// Parses a `StrBody` until end of input. Used for `Parse` implementation.
    pub fn parse_str_body(&mut self) -> PResult<'a, ast::StrBody> {
        let (str_body, _): (_, Option<()>) =
            self.parse_str_body_with_terminator_parser(|_| None)?;
        Ok(str_body)
    }

    /// Parses a `StrBody` with the given terminator, consuming it.
    pub fn parse_str_body_with_terminator(&mut self, terminator: T) -> PResult<'a, ast::StrBody> {
        let (str_body, term) = self.parse_str_body_with_terminator_parser(|p| p.eat(terminator))?;

        if term.is_some() {
            Ok(str_body)
        } else {
            Err(self.expect(terminator))
        }
    }

    /// Parses a `StrBody` with the given terminator, consuming it.
    pub fn parse_str_body_with_terminator_parser<F, U>(
        &mut self,
        mut terminator: F,
    ) -> PResult<'a, (ast::StrBody, Option<U>)>
    where
        F: FnMut(&mut Self) -> Option<U>,
    {
        let mut segments = Vec::new();
        let span = self.token.span;

        let mut term = None;

        while {
            if let Some(terminator) = terminator(self) {
                term = Some(terminator);
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

        let span = if let Some(last) = segments.last() {
            span.union(last.span())
        } else {
            span.empty()
        };

        Ok((ast::StrBody { segments, span }, term))
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

    pub fn parse_text(&mut self) -> PResult<'a, ast::Text> {
        let mut span = self.token.span;
        while let Some(token) = self.eat(T::Text) {
            span = span.union(token.span);
        }
        Ok(ast::Text { span })
    }

    fn parse_str_segment_text(&mut self) -> PResult<'a, ast::StrSegment> {
        self.parse_text().map(ast::StrSegment::Text)
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
        let open = self
            .eat(T::OpenDelim(Delim::Brace))
            .ok_or_else(|| self.expect(T::OpenDelim(Delim::Brace)))?;

        let expr = self.parse_or_eat_till(T::CloseDelim(Delim::Brace), Self::parse_expr)?;

        let close = self
            .eat(T::CloseDelim(Delim::Brace))
            .ok_or_else(|| self.expect(T::CloseDelim(Delim::Brace)))?;

        Ok(ast::StrSegment::Expr(open.span.union(close.span), P(expr)))
    }

    fn parse_str_segment_format_func(&mut self) -> PResult<'a, ast::StrSegment> {
        let open = self
            .eat(T::OpenDelim(Delim::Bracket))
            .ok_or_else(|| self.expect(T::OpenDelim(Delim::Bracket)))?;

        let format_func =
            self.parse_or_eat_till(T::CloseDelim(Delim::Bracket), Self::parse_format_func)?;

        let close = self
            .eat(T::CloseDelim(Delim::Bracket))
            .ok_or_else(|| self.expect(T::CloseDelim(Delim::Bracket)))?;

        Ok(ast::StrSegment::FormatFunc(
            open.span.union(close.span),
            format_func,
        ))
    }

    pub fn parse_format_func(&mut self) -> PResult<'a, ast::FormatFunc> {
        let path = self.parse_path()?;
        let span = path.span;

        let expr = if let ast::StrSegment::Expr(_, expr) = self.parse_str_segment_expr()? {
            expr
        } else {
            panic!("parse_str_segment_expr should only return the Expr variant");
        };

        let (args, args_span) = self.parse_list_with(
            true,
            |p| {
                if p.check(T::CloseDelim(Delim::Bracket)) || p.is_end_of_line() {
                    Some(ListSep::Term)
                } else if p.check(T::Number) || p.check(T::Ident) {
                    Some(ListSep::Sep)
                } else {
                    None
                }
            },
            |p, span| {
                p.expect_one_of(&[T::Ident, T::Number, T::CloseDelim(Delim::Bracket)])
                    .span(span);
            },
            |p| p.parse_format_func_arg().ok(),
        );

        let span = span.union(args_span);

        Ok(ast::FormatFunc {
            path,
            expr,
            args,
            span,
        })
    }

    pub fn parse_format_func_arg(&mut self) -> PResult<'a, ast::FormatFuncArg> {
        let key = self.parse_format_func_arg_key()?;

        if self.eat(T::Eq).is_none() {
            let err = self.expect_one_of(&[T::Eq]);
            let (_, span) =
                self.eat_until_with_or_end_of_line(|p| p.eat(T::Eq).map(|tok| ((), tok.span)));
            if let Some(span) = span {
                err.span(span);
            }
        }

        let value = P(self.parse_unary_expr_or_higher()?);

        Ok(ast::FormatFuncArg { key, value })
    }

    pub fn parse_format_func_arg_key(&mut self) -> PResult<'a, ast::FormatFuncArgKey> {
        match self.token.kind {
            T::Number => {
                let token = self.bump();
                Ok(ast::FormatFuncArgKey::Num(token.span))
            }
            T::Ident => {
                let path = self.parse_path()?;
                Ok(ast::FormatFuncArgKey::Path(path))
            }
            _ => Err(self.expect_one_of(&[T::Number, T::Ident])),
        }
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
            ast::StrSegment::Expr(
                Span::new(0, 8),
                P(ast::Expr::parse_with_interner("33 - 4", 1, itn).unwrap()),
            )
        });

        ast::StrSegment::parse("{1 + 3 <<&&& foo >> bar}", 0).unwrap_err();
        ast::StrSegment::parse("{ 42", 0).unwrap_err();
    }

    #[test]
    fn can_parse_str_segment_format_func() {
        assert_parse(
            r#"[format_func {$foo + bar} foo="bar" 42=`baz{$foo}`]"#,
            |itn| {
                ast::StrSegment::FormatFunc(
                    Span::new(0, 51),
                    ast::FormatFunc {
                        path: ast::Path::parse_with_interner("format_func", 1, itn).unwrap(),
                        expr: P(ast::Expr::parse_with_interner("$foo + bar", 14, itn).unwrap()),
                        args: vec![
                            ast::FormatFuncArg {
                                key: ast::FormatFuncArgKey::Path(
                                    ast::Path::parse_with_interner("foo", 26, itn).unwrap(),
                                ),
                                value: P(ast::Lit::parse_with_interner(r#""bar""#, 30, itn)
                                    .unwrap()
                                    .into()),
                            },
                            ast::FormatFuncArg {
                                key: ast::FormatFuncArgKey::Num(Span::new(36, 2)),
                                value: P(ast::Lit::parse_with_interner(r#"`baz{$foo}`"#, 39, itn)
                                    .unwrap()
                                    .into()),
                            },
                        ],
                        span: Span::new(1, 49),
                    },
                )
            },
        );

        ast::StrSegment::parse("[foo {$bar} bar=\"baz]", 0).unwrap_err();
    }
}
