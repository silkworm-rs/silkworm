use silkworm_sourcemap::Span;

use crate::token::{Kind as TokenKind, Token};

use super::Parser;

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum ListSep {
    Sep,
    Term,
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    /// Parse a list of items using `op` until `separator` returns `Some(Term)`. Returns the
    /// list of valid items and the full span consumed.
    pub fn parse_list_with<F, U, PSep, ESep>(
        &mut self,
        allow_empty: bool,
        mut sep: PSep,
        mut sep_err: ESep,
        mut op: F,
    ) -> (Vec<U>, Span)
    where
        PSep: FnMut(&mut Self) -> Option<ListSep>,
        ESep: FnMut(&mut Self, Span),
        F: FnMut(&mut Self) -> Option<U>,
    {
        let mut items = Vec::new();
        let base_span = self.token.span.empty();

        if allow_empty && Some(ListSep::Term) == sep(self) {
            return (Vec::new(), base_span);
        }

        loop {
            items.extend(op(self));

            let cont = match sep(self) {
                Some(ListSep::Sep) => true,
                Some(ListSep::Term) => false,
                None => {
                    let (maybe_sep, consumed) = self.eat_until_with_or_end_of_line(&mut sep);
                    let consumed = consumed.unwrap_or(self.token.span);
                    sep_err(self, consumed);
                    match maybe_sep {
                        Some(ListSep::Sep) => true,
                        Some(ListSep::Term) | None => false,
                    }
                }
            };

            if !cont {
                break;
            }
        }

        let span = base_span.union(self.token.span.empty());
        (items, span)
    }
}

pub(super) fn parse_list_sep_with_term<I>(
    allow_trailing: bool,
    sep: TokenKind,
    term: TokenKind,
) -> impl for<'a> FnMut(&mut Parser<'a, I>) -> Option<ListSep>
where
    I: Iterator<Item = Token>,
{
    parse_list_sep_with_term_with(
        allow_trailing,
        move |p| p.eat(sep).is_some(),
        move |p| p.eat(term).is_some(),
    )
}

pub(super) fn parse_list_sep_with_term_with<'f, I, PSep, PTerm>(
    allow_trailing: bool,
    mut sep: PSep,
    mut term: PTerm,
) -> impl for<'a> FnMut(&mut Parser<'a, I>) -> Option<ListSep> + 'f
where
    I: Iterator<Item = Token>,
    PSep: for<'a> FnMut(&mut Parser<'a, I>) -> bool + 'f,
    PTerm: for<'a> FnMut(&mut Parser<'a, I>) -> bool + 'f,
{
    move |p| {
        if sep(p) {
            if allow_trailing
                && (p.check(TokenKind::Eof) || p.check(TokenKind::LineBreak) || term(p))
            {
                Some(ListSep::Term)
            } else {
                Some(ListSep::Sep)
            }
        } else if p.check(TokenKind::Eof) || p.check(TokenKind::LineBreak) || term(p) {
            Some(ListSep::Term)
        } else {
            None
        }
    }
}
