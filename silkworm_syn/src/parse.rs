use std::fmt::{self, Display};

use hashbrown::HashSet;
use silkworm_err::{Error, ErrorBuilder, ErrorCtx};

use crate::ast;
use crate::lex::{self, LexStream};
use crate::symbol::{Interner, Symbol};
use crate::token::{Kind as TokenKind, Token};
use crate::Span;

mod block;
mod expr;
mod list;
mod misc;
mod node;
mod pragma;
mod str;

#[derive(Debug)]
pub struct ParseCtx<'a> {
    pub errors: &'a ErrorCtx,
    pub interner: &'a mut Interner,
    pub source: &'a str,
    pub span_base: u32,
}

#[derive(Debug)]
struct Parser<'a, I> {
    token: Token,
    last_token: Token,
    expected_tokens: Vec<TokenKind>,
    token_stream: Lookahead<I>,
    ctx: ParseCtx<'a>,
}

type PResult<'a, T> = std::result::Result<T, &'a mut ErrorBuilder>;

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Token>,
{
    fn new(ctx: ParseCtx<'a>, token_stream: I) -> Self {
        let mut parser = Parser {
            token: Token::new(TokenKind::Unknown, Span::nil()),
            last_token: Token::new(TokenKind::Unknown, Span::nil()),
            expected_tokens: Vec::new(),
            token_stream: Lookahead::new(token_stream),
            ctx,
        };

        parser.bump();
        parser
    }

    fn bump(&mut self) -> Token {
        if self.token.kind == TokenKind::Eof {
            panic!("bump called after EOF");
        }

        self.last_token = self.token;

        while {
            self.token = match self.token_stream.next() {
                Some(token) => token,
                None => {
                    self.ctx
                        .errors
                        .bug("unexpected end of token stream")
                        .span(self.token.span);
                    Token::new(TokenKind::Eof, self.token.span)
                }
            };

            // Skip comments, whitespace, and unknown tokens
            match self.token.kind {
                TokenKind::Comment | TokenKind::Whitespace => true,
                TokenKind::Unknown => {
                    self.ctx
                        .errors
                        .error("unparsable characters")
                        .span(self.token.span);
                    true
                }
                _ => false,
            }
        } {}

        self.expected_tokens.clear();

        self.last_token
    }

    fn is_eof(&self) -> bool {
        self.token.kind == TokenKind::Eof
    }

    /// Checks if the current token matches `kind`. This adds the token to `expected_tokens`
    /// on failure.
    #[must_use]
    fn check(&mut self, kind: TokenKind) -> bool {
        let is_current = self.token.kind == kind;
        if !is_current {
            self.expected_tokens.push(kind);
        }
        is_current
    }

    /// Checks if the current token is a `Pragma` and returns the style if true. This adds
    /// the token to `expected_tokens` on failure.
    #[must_use]
    fn check_pragma(&mut self) -> Option<crate::token::PragmaStyle> {
        match self.token.kind {
            TokenKind::Pragma(style) => Some(style),
            _ => {
                use crate::token::PragmaStyle as Style;

                self.expected_tokens.extend(&[
                    TokenKind::Pragma(Style::Outer),
                    TokenKind::Pragma(Style::Inner),
                ]);

                None
            }
        }
    }

    /// Eats a token if it matches `kind` and returns it. This adds the token to
    /// `expected_tokens` on failure.
    #[must_use]
    fn eat(&mut self, kind: TokenKind) -> Option<Token> {
        if self.check(kind) {
            self.bump();
            Some(self.last_token)
        } else {
            None
        }
    }

    /// Eats an `Ident` and interns it, returning the symbol and span. This adds the token
    /// to `expected_tokens` on failure.
    #[must_use]
    fn eat_symbol(&mut self) -> Option<(Symbol, Span)> {
        self.eat(TokenKind::Ident).map(|ident| {
            let sym = self.ctx.intern_span(ident.span);
            (sym, ident.span)
        })
    }

    /// Eat all tokens until end of line, without consuming the terminating token.
    /// Returns the span of all tokens eaten this way, or `None` if there
    /// is none.
    fn eat_until_end_of_line(&mut self) -> Option<Span> {
        let (_, span): (Option<()>, _) = self.eat_until_with_or_end_of_line(|_| None);
        span
    }

    /// Eat all tokens until `op` returns `Some` or end of line, without consuming the
    /// terminating token if the latter. Returns the return value of `op`.
    ///
    /// The span returned is the span of all tokens eaten this way, or `None` if there
    /// is none.
    fn eat_until_with_or_end_of_line<F, U>(&mut self, mut op: F) -> (Option<U>, Option<Span>)
    where
        F: FnMut(&mut Self) -> Option<U>,
    {
        let mut span = None;

        loop {
            if let Some(ret) = op(self) {
                return (Some(ret), span);
            } else {
                match self.token.kind {
                    TokenKind::LineBreak | TokenKind::Eof => return (None, span),
                    _ => {}
                }
            }

            let span = span.get_or_insert(self.token.span);
            *span = span.union(self.token.span);

            self.bump();
        }
    }

    /// Parse using a method. If `parse` has failed, it will then eat all tokens until
    /// `terminator` or end of line, without consuming the terminator if found.
    fn parse_or_eat_till<F, U>(&mut self, terminator: TokenKind, parse: F) -> PResult<'a, U>
    where
        F: FnOnce(&mut Self) -> PResult<'a, U>,
    {
        parse(self).map_err(|err| {
            let (_, span) = self.eat_until_with_or_end_of_line(|p| {
                if p.check(terminator) {
                    Some(())
                } else {
                    None
                }
            });
            if let Some(span) = span {
                err.annotate_span(span, "extra tokens");
            }
            err
        })
    }

    /// Peeks the nth token *after* the current token. Returns `None` if beyond EOF.
    fn peek_nth(&mut self, n: usize) -> Option<Token> {
        self.token_stream.peek_nth(n)
    }

    /// Checks if the nth token *after* the current token is of `kind`. Returns `None` if
    /// beyond EOF.
    fn check_nth(&mut self, n: usize, kind: TokenKind) -> Option<bool> {
        self.peek_nth(n).map(|tok| tok.kind == kind)
    }

    /// Checks if the current token or the next token is of `kind`. Returns `None` if
    /// the current token is not `kind` and the next token is beyond EOF.
    fn check_or_next(&mut self, kind: TokenKind) -> Option<bool> {
        if self.check(kind) {
            Some(true)
        } else {
            self.check_nth(0, kind)
        }
    }

    fn expect(&mut self, kind: TokenKind) -> &'a mut ErrorBuilder {
        self.expect_one_of(&[kind])
    }

    fn expect_one_of(&mut self, expected: &[TokenKind]) -> &'a mut ErrorBuilder {
        let mut expected = expected
            .iter()
            .copied()
            .chain(self.expected_tokens.drain(..))
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();

        expected.sort();

        if expected.is_empty() {
            panic!("must expect at least one token");
        }

        let actual = self.token.kind;

        let msg = if expected.len() == 1 {
            format!("expected {}, found {}", TokenChoice(&expected), actual)
        } else {
            format!(
                "expected one of {}, found {}",
                TokenChoice(&expected),
                actual
            )
        };

        self.ctx.errors.error(msg).span(self.token.span)
    }
}

#[derive(Copy, Clone, Debug, Default)]
struct TokenChoice<'a>(&'a [TokenKind]);

impl<'a> Display for TokenChoice<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.0.iter();

        if let Some(first) = iter.next() {
            write!(f, "{}", first)?;
        } else {
            return Ok(());
        }

        for t in iter {
            write!(f, ", {}", t)?;
        }

        Ok(())
    }
}

impl<'a> ParseCtx<'a> {
    fn intern_span(&mut self, span: Span) -> Symbol {
        let sym = span.read(self.source, self.span_base);
        self.interner.intern(sym)
    }
}

const LOOKAHEAD: usize = 2;

#[derive(Debug)]
struct Lookahead<I> {
    iter: I,
    buf: arraydeque::ArrayDeque<[Token; LOOKAHEAD], arraydeque::behavior::Wrapping>,
}

impl<I> Iterator for Lookahead<I>
where
    I: Iterator<Item = Token>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.buf.pop_front().or_else(|| self.iter.next())
    }
}

impl<I> Lookahead<I>
where
    I: Iterator<Item = Token>,
{
    fn new(iter: I) -> Self {
        Lookahead {
            iter,
            buf: Default::default(),
        }
    }

    /// Get the nth token, buffering these tokens from the underlying iterator when necessary.
    /// Returns `None` if the underlying stream is empty.
    fn peek_nth(&mut self, n: usize) -> Option<Token> {
        assert!(
            n < LOOKAHEAD,
            "n must be less than or equal to LOOKAHEAD = {}",
            LOOKAHEAD
        );
        self.fill_buf();
        self.buf.get(n).copied()
    }

    fn fill_buf(&mut self) {
        while {
            !self.buf.is_full() && {
                if let Some(tok) = self.iter.next() {
                    let is_not_full = self.buf.push_back(tok).is_none();
                    assert!(is_not_full, "buf should not be full");
                    true
                } else {
                    false
                }
            }
        } {}
    }
}

/// Trait for AST types that can be parsed.
pub trait Parse: Sized + private::Sealed {
    /// Default block mode to use when parsing from string sources.
    #[doc(hidden)]
    const SOURCE_BLOCK_MODE: lex::BlockMode;

    /// Default inline mode to use when parsing from string sources.
    #[doc(hidden)]
    const SOURCE_INLINE_MODE: lex::InlineMode;

    /// Parse an AST node with a shared parsing context. Errors are emitted into `ctx`.
    ///
    /// This emits an error if `partial` is `false` and the input is not completely consumed.
    fn partial_parse_with_ctx<I: IntoIterator<Item = Token>>(
        partial: bool,
        ctx: ParseCtx<'_>,
        token_stream: I,
    ) -> Result<Self, ()>;

    /// Parse an AST node with a shared parsing context. Errors are emitted into `ctx`.
    ///
    /// This emits an error if the input is not completely consumed.
    fn parse_with_ctx<I: IntoIterator<Item = Token>>(
        ctx: ParseCtx<'_>,
        token_stream: I,
    ) -> Result<Self, ()> {
        Self::partial_parse_with_ctx(false, ctx, token_stream)
    }

    /// Convenience method for parsing source without a shared context. Errors are returned
    /// in the `Err` variant.
    ///
    /// This emits an error if `partial` is `false` and the input is not completely consumed.
    ///
    /// # Errors
    ///
    /// If any errors are emitted by the parser.
    fn partial_parse_with_interner(
        partial: bool,
        source: &str,
        span_base: u32,
        interner: &mut Interner,
    ) -> Result<Self, Vec<Error>> {
        let errors = ErrorCtx::new();
        let lex_stream = LexStream::with_modes(
            source,
            span_base,
            Self::SOURCE_BLOCK_MODE,
            Self::SOURCE_INLINE_MODE,
        );

        let ast = Self::partial_parse_with_ctx(
            partial,
            ParseCtx {
                errors: &errors,
                interner,
                source,
                span_base,
            },
            lex_stream.filter_map(|result| match result {
                Ok(tok) => Some(tok),
                Err(err) => {
                    errors.bug(format!("fatal lexer error: {}", err));
                    None
                }
            }),
        );

        let errors = errors
            .into_vec()
            .into_iter()
            .map(ErrorBuilder::done)
            .collect::<Vec<_>>();

        ast.and_then(|ast| if errors.is_empty() { Ok(ast) } else { Err(()) })
            .map_err(|_| errors)
    }

    /// Convenience method for parsing source without a shared context. Errors are returned
    /// in the `Err` variant.
    ///
    /// This emits an error if the input is not completely consumed.
    ///
    /// # Errors
    ///
    /// If any errors are emitted by the parser.
    fn parse_with_interner(
        source: &str,
        span_base: u32,
        interner: &mut Interner,
    ) -> Result<Self, Vec<Error>> {
        Self::partial_parse_with_interner(false, source, span_base, interner)
    }

    /// Convenience method for parsing source without a shared context. Errors are returned
    /// in the `Err` variant.
    ///
    /// Symbols will be interned using a new interner, which will be returned on success.
    ///
    /// This emits an error if `partial` is `false` and the input is not completely consumed.
    ///
    /// # Errors
    ///
    /// If any errors are emitted by the parser.
    fn partial_parse(
        partial: bool,
        source: &str,
        span_base: u32,
    ) -> Result<(Self, Interner), Vec<Error>> {
        let mut interner = Interner::new();
        Self::partial_parse_with_interner(partial, source, span_base, &mut interner)
            .map(|ast| (ast, interner))
    }

    /// Convenience method for parsing source without a shared context. Errors are returned
    /// in the `Err` variant.
    ///
    /// Symbols will be interned using a new interner, which will be returned on success.
    ///
    /// This emits an error if the input is not completely consumed.
    ///
    /// # Errors
    ///
    /// If any errors are emitted by the parser.
    fn parse(source: &str, span_base: u32) -> Result<(Self, Interner), Vec<Error>> {
        Self::partial_parse(false, source, span_base)
    }
}

macro_rules! impl_parse {
    {
        $(
            impl Parse for ast::$ast_type:ident => $parse_method:ident {
                const SOURCE_BLOCK_MODE: lex::BlockMode = $block_mode:expr;
                const SOURCE_INLINE_MODE: lex::InlineMode = $inline_mode:expr;
                [ .. ]
            }
        )*
    } => {
        $(
            impl private::Sealed for ast::$ast_type {}
            impl Parse for ast::$ast_type {
                const SOURCE_BLOCK_MODE: lex::BlockMode = $block_mode;
                const SOURCE_INLINE_MODE: lex::InlineMode = $inline_mode;
                fn partial_parse_with_ctx<I: IntoIterator<Item = Token>>(
                    partial: bool,
                    ctx: ParseCtx<'_>,
                    token_stream: I,
                ) -> Result<Self, ()> {
                    let mut parser = Parser::new(ctx, token_stream.into_iter());
                    let result = parser.$parse_method().map_err(|_| ());
                    if !partial && !parser.is_eof() {
                        parser.expect(TokenKind::Eof);
                    }
                    result
                }
            }
        )*
    }
}

impl_parse! {
    impl Parse for ast::Path => parse_path {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Body;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::Interpolation;
        [ .. ]
    }
    impl Parse for ast::Pragma => parse_pragma {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Header;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::Meta;
        [ .. ]
    }
    impl Parse for ast::Meta => parse_meta {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Header;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::Meta;
        [ .. ]
    }
    impl Parse for ast::StrBody => parse_str_body {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Body;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::InterpolatedStringLiteral;
        [ .. ]
    }
    impl Parse for ast::StrSegment => parse_str_segment {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Body;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::InterpolatedStringLiteral;
        [ .. ]
    }
    impl Parse for ast::Text => parse_text {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Body;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::InterpolatedStringLiteral;
        [ .. ]
    }
    impl Parse for ast::Expr => parse_expr {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Body;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::Interpolation;
        [ .. ]
    }
    impl Parse for ast::FormatFunc => parse_format_func {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Body;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::FormatFunction;
        [ .. ]
    }
    impl Parse for ast::FormatFuncArg => parse_format_func_arg {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Body;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::FormatFunction;
        [ .. ]
    }
    impl Parse for ast::FormatFuncArgKey => parse_format_func_arg_key {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Body;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::FormatFunction;
        [ .. ]
    }
    impl Parse for ast::Var => parse_var {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Body;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::Interpolation;
        [ .. ]
    }
    impl Parse for ast::Lit => parse_lit {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Body;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::Interpolation;
        [ .. ]
    }
    impl Parse for ast::Stmt => parse_stmt {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Body;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::StartOfLine;
        [ .. ]
    }
    impl Parse for ast::StmtBody => parse_stmt_body {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Body;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::StartOfLine;
        [ .. ]
    }
    impl Parse for ast::Command => parse_command {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Body;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::Command;
        [ .. ]
    }
    impl Parse for ast::Flow => parse_flow {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Body;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::OptionTextOrTarget;
        [ .. ]
    }
    impl Parse for ast::ShortcutOption => parse_shortcut_option {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Body;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::StartOfLine;
        [ .. ]
    }
    impl Parse for ast::Hashtag => parse_hashtag {
        const SOURCE_BLOCK_MODE: lex::BlockMode = lex::BlockMode::Body;
        const SOURCE_INLINE_MODE: lex::InlineMode = lex::InlineMode::Hashtag;
        [ .. ]
    }
}

mod private {
    pub trait Sealed {}
}

#[cfg(test)]
mod test_utils {
    use super::*;

    // Use pretty_assertions for `assert_eq` diffs.
    use pretty_assertions::assert_eq;

    pub fn assert_partial_parse_with<T, F>(partial: bool, source: &str, op: F)
    where
        T: Parse,
        F: FnOnce(T, Interner) -> (),
    {
        let (ast, interner) = T::partial_parse(partial, source, 0).unwrap_or_else(|err| {
            panic!(
                "errors parsing source:```\n{}\n```\nerrors: {:#?}",
                source, err
            );
        });
        op(ast, interner);
    }

    pub fn assert_parse_with<T, F>(source: &str, op: F)
    where
        T: Parse,
        F: FnOnce(T, Interner) -> (),
    {
        assert_partial_parse_with(false, source, op)
    }

    pub fn assert_partial_parse<T, F>(partial: bool, source: &str, expected: F)
    where
        T: Parse + Eq + fmt::Debug,
        F: FnOnce(&mut Interner) -> T,
    {
        assert_partial_parse_with(partial, source, |ast, mut interner| {
            let expected = expected(&mut interner);
            assert_eq!(expected, ast);
        });
    }

    pub fn assert_parse<T, F>(source: &str, expected: F)
    where
        T: Parse + Eq + fmt::Debug,
        F: FnOnce(&mut Interner) -> T,
    {
        assert_partial_parse(false, source, expected)
    }
}
