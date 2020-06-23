use std::str::Chars;

use unicode_xid::UnicodeXID;

use super::token::*;
use super::{BlockMode, InlineMode, Span};

pub(super) fn parse_token(
    input: &str,
    span_base: u32,
    block_mode: BlockMode,
    inline_mode: InlineMode,
) -> Token {
    let mut cursor = Cursor::new(input, block_mode, inline_mode);
    let mut kind = cursor.parse_kind();
    let len = cursor.len_consumed();

    if let Kind::Ident = kind {
        if let Some(keyword) = Keyword::identify(&input[..len]) {
            if keyword.class().may_appear_in(block_mode, inline_mode) {
                kind = Kind::Keyword(keyword);
            }
        }
    }

    Token::new(kind, Span::new(span_base, len as u32))
}

const EOF_CHAR: char = '\0';

#[derive(Clone, Debug)]
struct Cursor<'a> {
    chars: Chars<'a>,
    initial_len: usize,
    block_mode: BlockMode,
    inline_mode: InlineMode,
}

impl<'a> Cursor<'a> {
    fn new(input: &'a str, block_mode: BlockMode, inline_mode: InlineMode) -> Self {
        Cursor {
            chars: input.chars(),
            initial_len: input.len(),
            block_mode,
            inline_mode,
        }
    }

    fn nth_char(&self, n: usize) -> char {
        self.chars.clone().nth(n).unwrap_or(EOF_CHAR)
    }

    fn len_consumed(&self) -> usize {
        self.initial_len - self.chars.as_str().len()
    }

    fn bump(&mut self) -> char {
        self.chars.next().unwrap_or(EOF_CHAR)
    }

    fn consume(&mut self, x: usize) -> usize {
        for consumed in 0..x {
            if self.chars.next().is_none() {
                return consumed;
            }
        }

        x
    }

    fn parse_bin_op_eq(&mut self, bin_op: BinOp) -> Kind {
        use Kind as T;

        if self.nth_char(0) == '=' {
            self.bump();
            T::BinOpEq(bin_op)
        } else {
            T::BinOp(bin_op)
        }
    }

    fn consume_number(&mut self) {
        let mut point_encountered = false;

        while {
            let peek = self.nth_char(0);
            if !point_encountered && peek == '.' {
                point_encountered = true;
                true
            } else {
                peek.is_ascii_digit()
            }
        } {
            self.bump();
        }
    }

    fn consume_identifier(&mut self) {
        while self.nth_char(0).is_xid_continue() {
            self.bump();
        }
    }

    fn consume_until_line_break(&mut self) {
        while self.nth_char(0) != '\n' {
            self.bump();
        }
    }

    fn consume_hex(&mut self, max_len: usize, term: Option<char>) {
        for _ in 0..max_len {
            let peek = self.nth_char(0);

            if Some(peek) == term {
                self.bump();
                return;
            }

            if !peek.is_ascii_hexdigit() {
                return;
            }

            self.bump();
        }
    }

    fn consume_escape(&mut self) -> Kind {
        let chr = self.bump();

        match chr {
            'x' => {
                self.consume_hex(2, None);
                Kind::EscapeByte
            }
            'u' => {
                self.bump();
                self.consume_hex(6, Some('}'));
                Kind::EscapeUnicode
            }
            _ => Kind::EscapeChar(EscapeChar::identify(chr)),
        }
    }

    fn consume_comment_or_pragma(&mut self) -> Kind {
        let third = self.nth_char(0);
        if third == '#' {
            self.bump();
            let forth = self.nth_char(0);
            if forth == '!' {
                self.bump();
                Kind::Pragma(PragmaStyle::Inner)
            } else {
                Kind::Pragma(PragmaStyle::Outer)
            }
        } else {
            self.consume_until_line_break();
            Kind::Comment
        }
    }

    fn parse_kind(&mut self) -> Kind {
        use BinOp as B;
        use Delim as D;
        use InlineMode as M;
        use Kind as T;

        let cur = self.bump();
        if cur == EOF_CHAR {
            return T::Eof;
        } else if cur == '\n' || cur == '\r' {
            return T::LineBreak;
        }

        let next = self.nth_char(0);

        if self.inline_mode.may_contain_whitespace() && cur.is_whitespace() {
            while {
                let next = self.nth_char(0);
                next != EOF_CHAR && next.is_whitespace()
            } {
                self.bump();
            }

            return T::Whitespace;
        }

        if self.inline_mode.may_contain_commands() {
            match cur {
                '<' => {
                    if next == '<' {
                        self.bump();
                        return T::OpenDelim(D::DoubleAngleBracket);
                    }
                }
                '[' => {
                    if next == '[' {
                        self.bump();
                        return T::OpenDelim(D::DoubleBracket);
                    }
                }
                _ => {}
            }
        }

        if self.inline_mode == M::StartOfLine {
            if cur == '-' && self.nth_char(0) == '>' {
                self.bump();
                return T::Arrow;
            }

            self.inline_mode = match self.block_mode {
                BlockMode::Header => M::HeaderKey,
                BlockMode::Body => M::FreeText,
            }
        }

        if self.inline_mode.may_contain_text() {
            if cur == '\\' && self.inline_mode.may_contain_escape() {
                return self.consume_escape();
            }

            if self.inline_mode.may_contain_text_interpolation() {
                match cur {
                    '{' => return T::OpenDelim(D::Brace),
                    '[' => return T::OpenDelim(D::Bracket),
                    _ => {}
                }
            }

            match self.inline_mode {
                M::FreeText | M::Hashtag => match cur {
                    '#' => return T::Hash,
                    '/' => {
                        if next == '/' {
                            self.bump();
                            return self.consume_comment_or_pragma();
                        }
                    }
                    _ => {}
                },
                M::OptionTextOrTarget => match cur {
                    '-' => {
                        if next == '>' {
                            self.bump();
                            return T::Arrow;
                        }
                    }
                    '|' => {
                        return T::Pipe;
                    }
                    ']' => {
                        if next == ']' {
                            self.bump();
                            return T::CloseDelim(D::DoubleBracket);
                        }
                    }
                    _ => {}
                },
                M::StringLiteral => {
                    if cur == '"' {
                        return T::CloseDelim(D::DoubleQuote);
                    }
                }
                M::InterpolatedStringLiteral => {
                    if cur == '`' {
                        return T::CloseDelim(D::Backtick);
                    }
                }
                _ => panic!("textual mode {:?} not covered", self.inline_mode),
            }

            while {
                let peek = self.nth_char(0);
                peek != EOF_CHAR
                    && peek != '\n'
                    && peek != '\r'
                    && !self.inline_mode.is_text_interrupting_symbol(peek)
            } {
                self.bump();
            }

            T::Text
        } else {
            match cur {
                ':' => T::Colon,
                '=' => {
                    if next == '=' {
                        self.bump();
                        let third = self.nth_char(1);
                        if third == '=' {
                            self.bump();
                            T::TripleEq
                        } else {
                            T::EqEq
                        }
                    } else {
                        T::Eq
                    }
                }
                '+' => self.parse_bin_op_eq(B::Plus),
                '-' => match next {
                    '>' => {
                        self.bump();
                        T::Arrow
                    }
                    '-' => {
                        let third = self.nth_char(1);
                        if third == '-' {
                            self.consume(2);
                            T::TripleDash
                        } else {
                            T::BinOp(B::Minus)
                        }
                    }
                    _ => self.parse_bin_op_eq(B::Minus),
                },
                '*' => self.parse_bin_op_eq(B::Star),
                '/' => match next {
                    '/' => {
                        self.bump();
                        self.consume_comment_or_pragma()
                    }
                    _ => self.parse_bin_op_eq(B::Slash),
                },
                '%' => self.parse_bin_op_eq(B::Percent),
                '|' => match next {
                    '|' => {
                        self.bump();
                        T::OrOr
                    }
                    _ => T::Pipe,
                },
                '!' => {
                    if next == '=' {
                        T::Neq
                    } else {
                        T::Not
                    }
                }
                '~' => T::Tilde,
                '>' => match next {
                    '>' => {
                        self.bump();
                        T::CloseDelim(D::DoubleAngleBracket)
                    }
                    '=' => T::Gte,
                    _ => T::Gt,
                },
                '<' => match next {
                    '<' => {
                        self.bump();
                        T::OpenDelim(D::DoubleAngleBracket)
                    }
                    '=' => T::Lte,
                    _ => T::Lt,
                },
                ',' => T::Comma,
                '.' => T::Period,
                '[' => {
                    if next == '[' {
                        self.bump();
                        T::OpenDelim(D::DoubleBracket)
                    } else {
                        T::OpenDelim(D::Bracket)
                    }
                }
                ']' => {
                    if next == ']' {
                        self.bump();
                        T::CloseDelim(D::DoubleBracket)
                    } else {
                        T::CloseDelim(D::Bracket)
                    }
                }
                '{' => T::OpenDelim(D::Brace),
                '}' => T::CloseDelim(D::Brace),
                '"' => T::OpenDelim(D::DoubleQuote),
                '`' => T::OpenDelim(D::Backtick),
                '(' => T::OpenDelim(D::Paren),
                ')' => T::CloseDelim(D::Paren),
                '$' => T::Dollar,
                '@' => {
                    if next == '@' {
                        self.bump();
                        T::AtAt
                    } else {
                        T::At
                    }
                }
                _ => {
                    if cur.is_ascii_digit() {
                        self.consume_number();
                        return T::Number;
                    }

                    // Keyword or identifier
                    if cur.is_xid_start() {
                        self.consume_identifier();
                        return T::Ident;
                    }

                    // Sequence of chars that can't be explained as a token
                    T::Unknown
                }
            }
        }
    }
}
