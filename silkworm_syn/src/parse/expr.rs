use crate::ast;
use crate::ptr::P;
use crate::token::{BinOp, Delim, Keyword, Kind as T, Token};
use crate::Span;

use super::{PResult, Parser};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    pub fn parse_expr(&mut self) -> PResult<'a, ast::Expr> {
        self.parse_expr_with_precedence(0)
    }

    pub fn parse_expr_with_precedence(&mut self, min_precedence: u32) -> PResult<'a, ast::Expr> {
        let mut left = self.parse_unary_expr_or_higher()?;

        while let Some(bin_op_kind) = self.peek_bin_op()? {
            let precedence = binary_precedence(bin_op_kind);
            if precedence <= min_precedence {
                break;
            }

            let bin_op = ast::BinOp {
                kind: bin_op_kind,
                span: self.bump().span,
            };

            let right = self.parse_expr_with_precedence(precedence)?;

            left = ast::Expr {
                span: left.span.union(right.span),
                kind: ast::ExprKind::Binary(bin_op, P(left), P(right)),
            };
        }

        Ok(left)
    }

    fn peek_bin_op(&mut self) -> PResult<'a, Option<ast::BinOpKind>> {
        use ast::BinOpKind as O;

        let bin_op = match self.token.kind {
            T::EqEq | T::Keyword(Keyword::Is) | T::Keyword(Keyword::Eq) => Some(O::Eq),
            T::Neq | T::Keyword(Keyword::Neq) => Some(O::Neq),
            T::Lt | T::Keyword(Keyword::Lt) => Some(O::Lt),
            T::Lte | T::Keyword(Keyword::Lte) => Some(O::Lte),
            T::Gt | T::Keyword(Keyword::Gt) => Some(O::Gt),
            T::Gte | T::Keyword(Keyword::Gte) => Some(O::Gte),

            T::AndAnd | T::Keyword(Keyword::And) => Some(O::And),
            T::OrOr | T::Keyword(Keyword::Or) => Some(O::Or),
            T::Xor | T::Keyword(Keyword::Xor) => Some(O::Xor),

            T::BinOp(BinOp::Plus) => Some(O::Add),
            T::BinOp(BinOp::Minus) => Some(O::Sub),
            T::BinOp(BinOp::Star) => Some(O::Mul),
            T::BinOp(BinOp::Slash) => Some(O::Div),
            T::BinOp(BinOp::Percent) => Some(O::Mod),

            T::Eq | T::Keyword(Keyword::To) => Some(O::Assign),
            T::BinOpEq(BinOp::Plus) => Some(O::AddAssign),
            T::BinOpEq(BinOp::Minus) => Some(O::SubAssign),
            T::BinOpEq(BinOp::Star) => Some(O::MulAssign),
            T::BinOpEq(BinOp::Slash) => Some(O::DivAssign),
            T::BinOpEq(BinOp::Percent) => Some(O::ModAssign),

            _ => None,
        };

        Ok(bin_op)
    }

    pub fn parse_unary_expr_or_higher(&mut self) -> PResult<'a, ast::Expr> {
        let (kind, span) = match self.token.kind {
            T::BinOp(BinOp::Minus) => (ast::UnOpKind::Neg, self.bump().span),
            T::Not | T::Keyword(Keyword::Not) => (ast::UnOpKind::Not, self.bump().span),
            _ => {
                return {
                    if let Ok(expr) = self.parse_call_expr_or_atom() {
                        Ok(expr)
                    } else {
                        Err(self.expect_one_of(&[T::Not, T::BinOp(BinOp::Minus)]))
                    }
                }
            }
        };

        let op = ast::UnOp { kind, span };
        let operand = self.parse_call_expr_or_atom()?;
        let span = op.span.union(operand.span);

        Ok(ast::Expr {
            kind: ast::ExprKind::Unary(op, P(operand)),
            span,
        })
    }

    pub fn parse_call_expr_or_atom(&mut self) -> PResult<'a, ast::Expr> {
        let receiver = self.parse_atom()?;

        let (args, span) = match self.parse_call_arg_list() {
            Some(tup) => tup,
            None => return Ok(receiver),
        };

        Ok(ast::Expr {
            span: receiver.span.union(span),
            kind: ast::ExprKind::Call(P(receiver), args),
        })
    }

    pub fn parse_call_arg_list(&mut self) -> Option<(Vec<ast::Expr>, Span)> {
        self.eat(T::OpenDelim(Delim::Paren))?;

        Some(self.parse_list_with(
            true,
            super::list::parse_list_sep_with_term(true, T::Comma, T::CloseDelim(Delim::Paren)),
            |p, span| {
                p.expect_one_of(&[T::Comma, T::CloseDelim(Delim::Paren)])
                    .span(span);
            },
            |p| p.parse_expr().ok(),
        ))
    }

    pub fn parse_atom(&mut self) -> PResult<'a, ast::Expr> {
        match self.token.kind {
            T::OpenDelim(Delim::Paren) => {
                let span = self.bump().span;
                let mut expr = self.parse_expr()?;
                if let Some(paren) = self.eat(T::CloseDelim(Delim::Paren)) {
                    expr.span = span.union(paren.span);
                    Ok(expr)
                } else {
                    Err(self.expect_one_of(&[]))
                }
            }
            T::Number
            | T::Keyword(Keyword::True)
            | T::Keyword(Keyword::False)
            | T::Keyword(Keyword::Null)
            | T::OpenDelim(Delim::DoubleQuote)
            | T::OpenDelim(Delim::Backtick) => {
                let lit = self.parse_lit()?;
                Ok(ast::Expr {
                    span: lit.span,
                    kind: ast::ExprKind::Lit(lit),
                })
            }
            T::Dollar | T::At | T::AtAt | T::Ident | T::Keyword(_) => {
                let var = self.parse_var()?;
                Ok(ast::Expr {
                    span: var.span,
                    kind: ast::ExprKind::Var(var),
                })
            }
            _ => Err(self.expect_one_of(&[
                T::OpenDelim(Delim::Paren),
                T::Number,
                T::Keyword(Keyword::True),
                T::Keyword(Keyword::False),
                T::Keyword(Keyword::Null),
                T::OpenDelim(Delim::DoubleQuote),
                T::OpenDelim(Delim::Backtick),
                T::Dollar,
                T::At,
                T::AtAt,
                T::Ident,
            ])),
        }
    }

    pub fn parse_var(&mut self) -> PResult<'a, ast::Var> {
        let span = self.token.span;

        let sigil = if self.eat(T::Dollar).is_some() {
            ast::Sigil::Global
        } else if self.eat(T::At).is_some() {
            ast::Sigil::Node
        } else if self.eat(T::AtAt).is_some() {
            ast::Sigil::File
        } else {
            ast::Sigil::Local
        };

        let (ident_span, keyword) = match self.token.kind {
            T::Ident => (self.bump().span, None),
            T::Keyword(kw) => (self.bump().span, Some(kw)),
            _ => return Err(self.expect(T::Ident)),
        };

        let symbol = self.ctx.intern_span(ident_span);

        Ok(ast::Var {
            sigil,
            symbol,
            keyword,
            span: span.union(ident_span),
        })
    }

    pub fn parse_lit(&mut self) -> PResult<'a, ast::Lit> {
        let (kind, span) = match self.token.kind {
            T::Number => (ast::LitKind::Number, self.bump().span),
            T::Keyword(Keyword::True) => (ast::LitKind::True, self.bump().span),
            T::Keyword(Keyword::False) => (ast::LitKind::False, self.bump().span),
            T::Keyword(Keyword::Null) => (ast::LitKind::Null, self.bump().span),
            T::OpenDelim(Delim::DoubleQuote) => {
                self.bump();
                let body =
                    self.parse_str_body_with_terminator(T::CloseDelim(Delim::DoubleQuote))?;
                let span = body.span;
                (ast::LitKind::Str(body), span)
            }
            T::OpenDelim(Delim::Backtick) => {
                self.bump();
                let body = self.parse_str_body_with_terminator(T::CloseDelim(Delim::Backtick))?;
                let span = body.span;
                (ast::LitKind::Str(body), span)
            }
            _ => {
                return Err(self.expect_one_of(&[
                    T::Number,
                    T::Keyword(Keyword::True),
                    T::Keyword(Keyword::False),
                    T::Keyword(Keyword::Null),
                    T::OpenDelim(Delim::DoubleQuote),
                    T::OpenDelim(Delim::Backtick),
                ]))
            }
        };

        Ok(ast::Lit { kind, span })
    }
}

fn binary_precedence(op: ast::BinOpKind) -> u32 {
    use ast::BinOpKind as O;
    match op {
        O::Mul => 8,
        O::Div => 8,
        O::Mod => 8,
        O::Add => 7,
        O::Sub => 7,
        O::Lt => 6,
        O::Lte => 6,
        O::Gt => 6,
        O::Gte => 6,
        O::Eq => 5,
        O::Neq => 5,
        O::MulAssign => 4,
        O::DivAssign => 4,
        O::ModAssign => 4,
        O::AddAssign => 3,
        O::SubAssign => 3,
        O::And => 2,
        O::Or => 2,
        O::Xor => 2,
        O::Assign => 1,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parse::test_utils::assert_parse;
    use crate::parse::Parse;
    use crate::Span;

    #[test]
    fn can_parse_lit() {
        assert_parse("123", |_itn| ast::Lit {
            kind: ast::LitKind::Number,
            span: Span::new(0, 3),
        });

        assert_parse("true", |_itn| ast::Lit {
            kind: ast::LitKind::True,
            span: Span::new(0, 4),
        });

        assert_parse("false", |_itn| ast::Lit {
            kind: ast::LitKind::False,
            span: Span::new(0, 5),
        });

        assert_parse("null", |_itn| ast::Lit {
            kind: ast::LitKind::Null,
            span: Span::new(0, 4),
        });
    }

    #[test]
    fn can_parse_var() {
        assert_parse("$foo", |itn| ast::Var {
            sigil: ast::Sigil::Global,
            symbol: itn.intern("foo"),
            keyword: None,
            span: Span::new(0, 4),
        });

        assert_parse("if", |itn| ast::Var {
            sigil: ast::Sigil::Local,
            symbol: itn.intern("if"),
            keyword: Some(Keyword::If),
            span: Span::new(0, 2),
        });

        assert_parse("@@bar", |itn| ast::Var {
            sigil: ast::Sigil::File,
            symbol: itn.intern("bar"),
            keyword: None,
            span: Span::new(0, 5),
        });

        assert_parse("@baz", |itn| ast::Var {
            sigil: ast::Sigil::Node,
            symbol: itn.intern("baz"),
            keyword: None,
            span: Span::new(0, 4),
        });
    }

    #[test]
    fn can_parse_expr() {
        use ast::BinOpKind as O;

        assert_parse("1 + 2 * 3", |itn| ast::Expr {
            kind: ast::ExprKind::Binary(
                ast::BinOp {
                    kind: O::Add,
                    span: Span::new(2, 1),
                },
                P(ast::Expr {
                    kind: ast::ExprKind::Lit(ast::Lit::parse_with_interner("1", 0, itn).unwrap()),
                    span: Span::new(0, 1),
                }),
                P(ast::Expr {
                    kind: ast::ExprKind::Binary(
                        ast::BinOp {
                            kind: O::Mul,
                            span: Span::new(6, 1),
                        },
                        P(ast::Expr {
                            kind: ast::ExprKind::Lit(
                                ast::Lit::parse_with_interner("2", 4, itn).unwrap(),
                            ),
                            span: Span::new(4, 1),
                        }),
                        P(ast::Expr {
                            kind: ast::ExprKind::Lit(
                                ast::Lit::parse_with_interner("3", 8, itn).unwrap(),
                            ),
                            span: Span::new(8, 1),
                        }),
                    ),
                    span: Span::new(4, 5),
                }),
            ),
            span: Span::new(0, 9),
        });

        assert_parse("(1 + 2) * 3", |itn| ast::Expr {
            kind: ast::ExprKind::Binary(
                ast::BinOp {
                    kind: O::Mul,
                    span: Span::new(8, 1),
                },
                P(ast::Expr {
                    kind: ast::ExprKind::Binary(
                        ast::BinOp {
                            kind: O::Add,
                            span: Span::new(3, 1),
                        },
                        P(ast::Expr {
                            kind: ast::ExprKind::Lit(
                                ast::Lit::parse_with_interner("1", 1, itn).unwrap(),
                            ),
                            span: Span::new(1, 1),
                        }),
                        P(ast::Expr {
                            kind: ast::ExprKind::Lit(
                                ast::Lit::parse_with_interner("2", 5, itn).unwrap(),
                            ),
                            span: Span::new(5, 1),
                        }),
                    ),
                    span: Span::new(0, 7),
                }),
                P(ast::Expr {
                    kind: ast::ExprKind::Lit(ast::Lit::parse_with_interner("3", 10, itn).unwrap()),
                    span: Span::new(10, 1),
                }),
            ),
            span: Span::new(0, 11),
        });

        assert_parse("bar()", |itn| ast::Expr {
            kind: ast::ExprKind::Call(
                P(ast::Expr {
                    kind: ast::ExprKind::Var(ast::Var::parse_with_interner("bar", 0, itn).unwrap()),
                    span: Span::new(0, 3),
                }),
                Vec::new(),
            ),
            span: Span::new(0, 5),
        });

        assert_parse("$foo = bar(42, 1 + 2 + @@baz(quux, )) or false", |itn| {
            ast::Expr {
                kind: ast::ExprKind::Binary(
                    ast::BinOp {
                        kind: O::Assign,
                        span: Span::new(5, 1),
                    },
                    P(ast::Expr {
                        kind: ast::ExprKind::Var(
                            ast::Var::parse_with_interner("$foo", 0, itn).unwrap(),
                        ),
                        span: Span::new(0, 4),
                    }),
                    P(ast::Expr {
                        kind: ast::ExprKind::Binary(
                            ast::BinOp {
                                kind: O::Or,
                                span: Span::new(38, 2),
                            },
                            P(ast::Expr {
                                kind: ast::ExprKind::Call(
                                    P(ast::Expr {
                                        kind: ast::ExprKind::Var(
                                            ast::Var::parse_with_interner("bar", 7, itn).unwrap(),
                                        ),
                                        span: Span::new(7, 3),
                                    }),
                                    vec![
                                        ast::Expr::parse_with_interner("42", 11, itn).unwrap(),
                                        ast::Expr::parse_with_interner(
                                            "1 + 2 + @@baz(quux, )",
                                            15,
                                            itn,
                                        )
                                        .unwrap(),
                                    ],
                                ),
                                span: Span::new(7, 31),
                            }),
                            P(ast::Expr {
                                kind: ast::ExprKind::Lit(
                                    ast::Lit::parse_with_interner("false", 41, itn).unwrap(),
                                ),
                                span: Span::new(41, 5),
                            }),
                        ),
                        span: Span::new(7, 39),
                    }),
                ),
                span: Span::new(0, 46),
            }
        });
    }
}
