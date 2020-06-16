use crate::ast;
use crate::ptr::P;
use crate::token::{BinOp, Delim, Keyword, Kind as T, PragmaStyle, Token};

use super::{PResult, Parser};

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    pub fn parse_block(&mut self, _term: T) -> PResult<'a, ast::Block> {
        unimplemented!()
    }

    pub fn parse_stmt(&mut self) -> PResult<'a, ast::Stmt> {
        let mut pragmas = Vec::new();
        while self.check(T::Pragma(PragmaStyle::Outer)) {
            pragmas.push(P(self.parse_pragma()?));
        }

        let body = self.parse_stmt_body()?;

        let decorator_command = self
            .eat(T::OpenDelim(Delim::DoubleAngleBracket))
            .map(|_| {
                let command = self.parse_command()?;
                if self.eat(T::CloseDelim(Delim::DoubleAngleBracket)).is_none() {
                    return Err(self.expect(T::CloseDelim(Delim::DoubleAngleBracket)));
                }
                Ok(command)
            })
            .transpose()?
            .map(P);

        let mut hashtags = Vec::new();
        while self.check(T::Hash) {
            hashtags.push(self.parse_hashtag()?);
        }

        if self.eat(T::LineBreak).is_none() {
            return Err(self.expect(T::LineBreak));
        }

        let associated_block = self
            .eat(T::Indent)
            .map(|_| {
                let block = self.parse_block(T::UnIndent)?;
                if self.eat(T::UnIndent).is_none() {
                    return Err(self.expect(T::UnIndent));
                }
                let _ = self.eat(T::LineBreak);
                Ok(block)
            })
            .transpose()?
            .map(P);

        Ok(ast::Stmt {
            pragmas,
            body,
            decorator_command,
            hashtags,
            associated_block,
        })
    }

    pub fn parse_stmt_body(&mut self) -> PResult<'a, ast::StmtBody> {
        unimplemented!()
    }

    pub fn parse_command(&mut self) -> PResult<'a, ast::Command> {
        unimplemented!()
    }

    pub fn parse_hashtag(&mut self) -> PResult<'a, ast::Hashtag> {
        unimplemented!()
    }
}
