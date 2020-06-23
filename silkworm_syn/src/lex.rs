use arrayvec::ArrayVec;
use thiserror::Error;

use crate::token::{self, KeywordClass, Token};
use crate::Span;

mod parse;

use self::parse::parse_token;

const MODE_STACK_CAPACITY: usize = 256;

/// Block-level modes used to modify lexer behavior.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
#[repr(u8)]
#[doc(hidden)]
pub enum BlockMode {
    Header,
    Body,
}

/// Inline modes used to modify lexer behavior.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
#[repr(u8)]
#[doc(hidden)]
pub enum InlineMode {
    /// After start of line and before any content.
    StartOfLine,
    /// Header key.
    HeaderKey,
    /// Title or tags header values after their respective keywords.
    HeaderTitleOrTagsValue,
    /// Unspecified line body.
    FreeText,
    /// Hashtag
    Hashtag,
    /// After `//#` and before line break.
    Meta,
    /// Between `<<` and `>>`.
    Command,
    /// Right after `[[`., before `->`, `|`, or `|>`.
    OptionTextOrTarget,
    /// Between `[[` and `]]`, after `->`, `|`, or `|>`.
    OptionTarget,
    /// Between `[` and `]`.
    FormatFunction,
    /// After an opening `"` and before a closing one.
    StringLiteral,
    /// After an opening `` ` `` and before a closing one.
    InterpolatedStringLiteral,
    /// Between `{` and `}`.
    Interpolation,
}

impl InlineMode {
    fn may_contain_text(self) -> bool {
        use InlineMode as I;

        match self {
            I::FreeText
            | I::Hashtag
            | I::OptionTextOrTarget
            | I::StringLiteral
            | I::InterpolatedStringLiteral => true,
            _ => false,
        }
    }

    fn may_contain_escape(self) -> bool {
        use InlineMode as I;

        match self {
            I::Hashtag | I::StringLiteral | I::InterpolatedStringLiteral => true,
            _ => false,
        }
    }

    fn may_contain_text_interpolation(self) -> bool {
        use InlineMode as I;

        match self {
            I::FreeText | I::OptionTextOrTarget | I::InterpolatedStringLiteral => true,
            _ => false,
        }
    }

    fn may_contain_commands(self) -> bool {
        use InlineMode as I;

        match self {
            I::StartOfLine | I::FreeText => true,
            _ => false,
        }
    }

    fn may_contain_whitespace(self) -> bool {
        !self.may_contain_text()
    }

    fn is_text_interrupting_symbol(self, c: char) -> bool {
        use InlineMode as I;

        match self {
            I::FreeText => match c {
                '\\' | '/' | '[' | '{' | '#' | '<' => true,
                _ => false,
            },
            I::Hashtag => match c {
                '\\' | '/' | '#' => true,
                _ => false,
            },
            I::OptionTextOrTarget => match c {
                '\\' | '-' | '[' | '{' | '|' | ']' => true,
                _ => false,
            },
            I::StringLiteral => match c {
                '\\' | '"' => true,
                _ => false,
            },
            I::InterpolatedStringLiteral => match c {
                '\\' | '[' | '{' | '`' => true,
                _ => false,
            },
            _ => panic!("{:?} is not a textual mode", self),
        }
    }
}

impl KeywordClass {
    fn may_appear_in(self, block_mode: BlockMode, inline_mode: InlineMode) -> bool {
        match self {
            KeywordClass::Used | KeywordClass::Reserved => true,
            KeywordClass::HeaderKey => {
                block_mode == BlockMode::Header && inline_mode == InlineMode::StartOfLine
            }
            KeywordClass::Pragma | KeywordClass::FeatureName => inline_mode == InlineMode::Meta,
        }
    }
}

/// A fatal lexing error.
#[derive(Debug, Error)]
pub enum ErrorKind {
    #[error("the inline mode stack is too deep, use less nested expressions")]
    StackTooDeep,
}

/// A spanned fatal lexing error.
#[derive(Debug, Error)]
#[error("error {kind} at {token:?}")]
pub struct Error {
    pub kind: ErrorKind,
    pub token: Token,
}

/// A lexing stream over a source string that can be consumed as an iterator.
#[derive(Clone, Debug)]
pub struct LexStream<'a> {
    src: &'a str,
    pos: u32,

    fatal: bool,
    eof_emitted: EofState,

    block_mode: BlockMode,
    inline_stack: ArrayVec<[InlineMode; MODE_STACK_CAPACITY]>,

    last_indent: u32,
    current_line_indent: u32,

    delayed: Option<Token>,
}

#[derive(Copy, Clone, Debug)]
enum EofState {
    NotEmitted,
    LastUnIndentEmitted,
    Emitted,
}

impl<'a> LexStream<'a> {
    /// Creates a new lexing stream.
    pub fn new(src: &'a str, pos: u32) -> Self {
        Self::with_modes(src, pos, BlockMode::Header, InlineMode::StartOfLine)
    }

    /// Creates a new lexing stream with given block and inline modes. This can cause
    /// surprising behavior outside very specific situations. Used for convenience methods
    /// in `Parse`.
    pub(crate) fn with_modes(
        src: &'a str,
        pos: u32,
        block_mode: BlockMode,
        inline_mode: InlineMode,
    ) -> Self {
        let mut inline_stack = ArrayVec::new();
        inline_stack.push(inline_mode);

        LexStream {
            src,
            pos,

            fatal: false,
            eof_emitted: EofState::NotEmitted,

            block_mode,
            inline_stack,

            last_indent: 0,
            current_line_indent: 0,

            delayed: None,
        }
    }

    /// Returns the current position of this stream in bytes into the source string.
    pub fn pos(&self) -> u32 {
        self.pos
    }

    fn push_inline_mode(&mut self, mode: InlineMode) -> Result<(), ErrorKind> {
        if let Some(inline_mode) = self.inline_stack.last_mut() {
            if *inline_mode == InlineMode::StartOfLine {
                *inline_mode = InlineMode::FreeText;
            }
        }

        self.inline_stack
            .try_push(mode)
            .map_err(|_| ErrorKind::StackTooDeep)
    }

    fn delay_token(&mut self, token: Token, new_kind: token::Kind) -> Token {
        if let Some(other_delayed) = self.delayed.replace(token) {
            panic!("already delayed token: {:?}", other_delayed);
        }

        let mut token = token;
        token.kind = new_kind;
        token.span.len = 0;

        token
    }

    fn indent_token(&mut self, current_token: Token) -> Option<Token> {
        use std::cmp::Ordering;

        let kind = match self.current_line_indent.cmp(&self.last_indent) {
            Ordering::Greater => Some(token::Kind::Indent),
            Ordering::Less => Some(token::Kind::UnIndent),
            Ordering::Equal => None,
        };

        kind.map(|kind| {
            let token = self.delay_token(current_token, kind);
            self.last_indent = self.current_line_indent;
            token
        })
    }
}

impl<'a> Iterator for LexStream<'a> {
    type Item = Result<Token, Error>;
    fn next(&mut self) -> Option<Result<Token, Error>> {
        use token::Delim as D;
        use token::Kind as T;

        #[derive(Copy, Clone, Debug)]
        enum ModeChange {
            Push(InlineMode),
            Replace(InlineMode),
            ReplaceBlock(BlockMode),
            Keep,
            Pop,
            EndLine,
        }

        if self.fatal {
            return None;
        }

        if let Some(delayed) = self.delayed.take() {
            return Some(Ok(delayed));
        }

        if self.src.is_empty() {
            match self.eof_emitted {
                EofState::NotEmitted if self.last_indent > 0 => {
                    self.eof_emitted = EofState::LastUnIndentEmitted;
                    return Some(Ok(Token::new(T::UnIndent, Span::new(self.pos, 0))));
                }
                EofState::NotEmitted | EofState::LastUnIndentEmitted => {
                    self.eof_emitted = EofState::Emitted;
                    return Some(Ok(Token::new(T::Eof, Span::new(self.pos, 0))));
                }
                EofState::Emitted => return None,
            }
        }

        let inline_mode = self
            .inline_stack
            .last()
            .copied()
            .expect("stack should not be empty");
        let token = parse_token(self.src, self.pos, self.block_mode, inline_mode);
        self.pos += token.span.len;
        self.src = &self.src[token.span.len as usize..];

        if let T::Eof = token.kind {
            return Some(Ok(token));
        }

        let mode_change: ModeChange = {
            use InlineMode as I;
            use ModeChange as C;

            match token.kind {
                T::LineBreak => C::EndLine,
                T::Text => {
                    if inline_mode == I::StartOfLine {
                        C::Replace(I::FreeText)
                    } else {
                        C::Keep
                    }
                }
                T::Hash => {
                    if inline_mode == I::StartOfLine || inline_mode == I::FreeText {
                        C::Replace(I::Hashtag)
                    } else {
                        C::Keep
                    }
                }
                T::TripleDash => C::ReplaceBlock(BlockMode::Body),
                T::TripleEq => C::ReplaceBlock(BlockMode::Header),

                T::Pragma(_) => C::Push(I::Meta),

                T::OpenDelim(D::Bracket) => C::Push(I::FormatFunction),
                T::OpenDelim(D::DoubleBracket) => C::Push(I::OptionTextOrTarget),
                T::OpenDelim(D::DoubleAngleBracket) => C::Push(I::Command),
                T::OpenDelim(D::Brace) => C::Push(I::Interpolation),
                T::OpenDelim(D::DoubleQuote) => C::Push(I::StringLiteral),
                T::OpenDelim(D::Backtick) => C::Push(I::InterpolatedStringLiteral),

                T::CloseDelim(delim) => {
                    let pop_delim = match inline_mode {
                        I::FormatFunction => Some(D::Bracket),
                        I::OptionTextOrTarget => Some(D::DoubleBracket),
                        I::OptionTarget => Some(D::DoubleBracket),
                        I::Command => Some(D::DoubleAngleBracket),
                        I::Interpolation => Some(D::Brace),
                        I::StringLiteral => Some(D::DoubleQuote),
                        I::InterpolatedStringLiteral => Some(D::Backtick),
                        _ => None,
                    };

                    if pop_delim == Some(delim) {
                        C::Pop
                    } else {
                        C::Keep
                    }
                }

                T::Arrow => match inline_mode {
                    I::StartOfLine => C::Replace(I::FreeText),
                    I::OptionTextOrTarget => C::Replace(I::OptionTarget),
                    _ => C::Keep,
                },

                T::Pipe | T::PipeArrow => {
                    if inline_mode == I::OptionTextOrTarget {
                        C::Replace(I::OptionTarget)
                    } else {
                        C::Keep
                    }
                }

                T::Keyword(token::Keyword::HeaderKey(header_key)) => match inline_mode {
                    I::StartOfLine | I::HeaderKey => match header_key {
                        token::HeaderKey::Title | token::HeaderKey::Tags => {
                            C::Replace(I::HeaderTitleOrTagsValue)
                        }
                    },
                    _ => panic!("header key keywords should not be parsed out of context"),
                },

                T::Colon => {
                    if inline_mode == I::HeaderKey {
                        C::Replace(I::FreeText)
                    } else {
                        C::Keep
                    }
                }

                _ => match inline_mode {
                    I::StartOfLine => {
                        if let T::Whitespace = token.kind {
                            self.current_line_indent += token.span.len;
                            C::Keep
                        } else {
                            match self.block_mode {
                                BlockMode::Header => C::Replace(I::HeaderKey),
                                BlockMode::Body => C::Replace(I::FreeText),
                            }
                        }
                    }
                    _ => C::Keep,
                },
            }
        };

        match mode_change {
            ModeChange::Push(mode) => {
                if let Err(kind) = self.push_inline_mode(mode) {
                    self.fatal = true;
                    return Some(Err(Error { kind, token }));
                }
            }
            ModeChange::Replace(mode) => {
                *self
                    .inline_stack
                    .last_mut()
                    .expect("stack should not be empty") = mode;
            }
            ModeChange::ReplaceBlock(mode) => self.block_mode = mode,
            ModeChange::Pop => {
                self.inline_stack.pop();
            }
            ModeChange::EndLine => {
                self.inline_stack.clear();
                self.inline_stack.push(InlineMode::StartOfLine);
                self.current_line_indent = 0;
            }
            ModeChange::Keep => {}
        }

        if inline_mode == InlineMode::StartOfLine {
            if let Some(new_mode) = self.inline_stack.last().copied() {
                if new_mode != inline_mode {
                    if let Some(token) = self.indent_token(token) {
                        return Some(Ok(token));
                    }
                }
            }
        }

        Some(Ok(token))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A simple sanity test indicating that the lexer works at least somewhat normal.
    /// Tests with proper output validation should be created later.
    #[test]
    fn it_works_at_all() {
        use super::token::Kind as T;

        let code = r#"
            //#! feature(foo, bar, baz)

            title: lex_sanity_test.foo.bar
            tags: foo bar baz すまん_不都合なことばかりで
            non_special_key: non special value
            ---
            No escapes: ¯\_(ツ)_/¯

            //# disable_feature(foo, bar, baz)
            <<if it_parses_strings("Foo\nBar\x3A baz\"\\\u{1234}") is true>>
                Yay {$yay}! #yay!
            <<else>>
                Oops {$oops}! //Oops!
            <<endif>>

            //# private
            <<if what_about_interpolation(`{$foo} + {$bar}`) is true>>
                Yay {what(`about {$here}?`)}! #yay!
            <<else>>
                Oops {$oops}! <<if $foo>> //Oops!
            <<endif>>

            [format_func {$expr} foo="bar"]
            
            <<move camera left>>
            <<unlockAchievement beganAdventure>>
            <<set $答え to 42>>

            <<set $private = $title is not $special>>

            [[Wow.Wow]] # foo // bar
            [[-> Wow.Wow]] # foo // bar
            [[Wow wow | Fish.Life]] # foo // bar
            [[Wow wow |> Fish.Life]] # foo // bar
            ===
        "#;

        let mut has_unknowns = false;

        let mut indent_pairing = 0;

        for token in LexStream::new(code, 0).take(1000) {
            let token = token.expect("should raise no internal errors");

            let spanned_code =
                &code[token.span.base as usize..(token.span.base + token.span.len) as usize];

            match token.kind {
                T::LineBreak => {
                    println!();
                }
                T::Whitespace => {
                    print!("{}", spanned_code);
                }
                T::Indent => {
                    indent_pairing += 1;
                    print!("<IDT> ");
                }
                T::UnIndent => {
                    indent_pairing -= 1;
                    print!("<UDT> ");
                }
                _ => {
                    if let T::Unknown = token.kind {
                        has_unknowns = true;
                    }

                    print!("{} ({:?}) ", spanned_code, token.kind);
                }
            }
        }

        if indent_pairing != 0 {
            panic!("indent unbalanced");
        }

        if has_unknowns {
            panic!("parsed tokens contained unknowns");
        }
    }
}
