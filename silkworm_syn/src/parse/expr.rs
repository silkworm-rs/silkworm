use crate::ast;

use crate::token::Token;

use super::{PResult, Parser};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    pub fn parse_expr(&mut self) -> PResult<'a, ast::Expr> {
        unimplemented!()
    }
}
