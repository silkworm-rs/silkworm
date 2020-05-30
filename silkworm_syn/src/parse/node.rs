use crate::ast;
use crate::ptr::P;
use crate::token::Token;

use super::{PResult, Parser};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    /// Parse a file until EoF.
    pub fn parse_file(&mut self) -> PResult<'a, ast::File> {
        unimplemented!()
    }

    fn parse_inner_meta(&mut self) -> PResult<'a, Vec<P<ast::Pragma>>> {
        unimplemented!()
    }

    fn parse_outer_meta(&mut self) -> PResult<'a, Vec<P<ast::Pragma>>> {
        unimplemented!()
    }

    /// Parse a node until right before the terminator (`===` or EoF).
    pub fn parse_node(&mut self) -> PResult<'a, ast::Node> {
        unimplemented!()
    }

    pub fn parse_node_header(&mut self) -> PResult<'a, ast::NodeHeader> {
        unimplemented!()
    }
}
