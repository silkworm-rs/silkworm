use crate::ast;

use crate::token::{Kind as TokenKind, Token};

use super::{PResult, Parser};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    pub fn parse_block(&mut self, _term: TokenKind) -> PResult<'a, ast::Block> {
        unimplemented!()
    }
}
