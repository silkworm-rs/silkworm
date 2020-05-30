use std::fmt;

use super::Span;

/// Binary operators that can be part of an assignment.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
#[repr(u8)]
pub enum BinOp {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
}

/// Delimiters
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
#[repr(u8)]
pub enum Delim {
    /// Round parenthesis (i.e., `(` or `)`).
    Paren,
    /// Square bracket (i.e., `[` or `]`).
    Bracket,
    /// Double square brackets (i.e., `[[` or `]]`).
    DoubleBracket,
    /// Double angle brackets (i.e., `<<` or `>>`).
    DoubleAngleBracket,
    /// Curly brace (i.e., `{` or `}`).
    Brace,
    /// Double quote (i.e., `"`).
    DoubleQuote,
    /// Backtick (i.e., `` ` ``).
    Backtick,
}

/// Class of keywords.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
#[repr(u8)]
pub enum KeywordClass {
    /// Strict keywords that are currently used.
    Used,
    /// Strict keywords that are reserved for forward compatibility.
    Reserved,
    /// Special header keys. Weak keywords that are only special in headers.
    HeaderKey,
    /// Pragma names. Weak keywords that are only special in pragmas.
    Pragma,
    /// Language feature names. Weak keywords that are only special in pragmas.
    FeatureName,
}

macro_rules! decl_keyword {
    {
        pub enum Keyword {
            $(
                inline $class:ident {
                    $(
                        $(#[$attr:meta])*
                        $keyword:ident = $value:ident $( | $alt_value:ident )*,
                    )*
                }
            )*

            $(
                $(#[$enum_class_attr:meta])*
                pub enum $enum_class:ident {
                    $(
                        $(#[$enum_attr:meta])*
                        $enum_keyword:ident = $enum_value:ident $( | $enum_alt_value:ident )*,
                    )*
                }
            )*
        }
    } => {
        /// Language keywords.
        #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
        #[repr(u8)]
        pub enum Keyword {
            $(
                $(
                    $(#[$attr])*
                    $keyword,
                )*
            )*
            $(
                $enum_class($enum_class),
            )*
        }

        $(
            #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
            #[repr(u8)]
            $(#[$enum_class_attr])*
            pub enum $enum_class {
                $(
                    $(#[$enum_attr])*
                    $enum_keyword,
                )*
            }
        )*

        impl Keyword {
            pub fn identify(s: &str) -> Option<Self> {
                $(
                    $(
                        if (stringify!($value)) == s {
                            return Some(Self::$keyword);
                        }
                        $(
                            if (stringify!($alt_value)) == s {
                                return Some(Self::$keyword);
                            }
                        )*
                    )*
                )*
                $(
                    $(
                        if (stringify!($enum_value)) == s {
                            return Some(Self::$enum_class($enum_class::$enum_keyword));
                        }
                        $(
                            if (stringify!($enum_alt_value)) == s {
                                return Some(Self::$enum_class($enum_class::$enum_keyword));
                            }
                        )*
                    )*
                )*
                return None;
            }

            pub fn into_canonical_str(self) -> &'static str {
                match self {
                    $(
                        $(
                            Self::$keyword => stringify!($value),
                        )*
                    )*
                    $(
                        $(
                            Self::$enum_class($enum_class::$enum_keyword) => stringify!($enum_value),
                        )*
                    )*
                }
            }

            pub fn class(self) -> KeywordClass {
                match self {
                    $(
                        $(
                            Self::$keyword => KeywordClass::$class,
                        )*
                    )*
                    $(
                        $(
                            Self::$enum_class($enum_class::$enum_keyword) => KeywordClass::$enum_class,
                        )*
                    )*
                }
            }
        }

        impl fmt::Display for Keyword {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str(self.into_canonical_str())
            }
        }
    };
}

decl_keyword! {
    pub enum Keyword {
        inline Used {
            /// `set`.
            Set = set,
            /// `call`.
            Call = call,
            /// Boolean literal `true`.
            True = true,
            /// Boolean literal `false`.
            False = false,
            /// Null literal `null`.
            Null = null,
            /// `if`.
            If = if,
            /// `elseif`.
            ElseIf = elseif,
            /// `else`.
            Else = else,
            /// `endif`.
            EndIf = endif,
            /// `to` operator.
            To = to,
            /// `is` operator.
            Is = is,
            /// `eq` operator.
            Eq = eq,
            /// `neq` operator.
            Neq = neq,
            /// `lt` operator.
            Lt = lt,
            /// `lte` operator.
            Lte = lte,
            /// `gt` operator.
            Gt = gt,
            /// `gte` operator.
            Gte = gte,
            /// `and` operator.
            And = and,
            /// `or` operator.
            Or = or,
            /// `xor` operator.
            Xor = xor,
            /// `not` operator.
            Not = not,
            /// `return`
            Return = return,
        }

        inline Reserved {
            /// `for`
            For = for,
            /// `loop`
            Loop = loop,
            /// `while`
            While = while,
            /// `do`
            Do = do,
            /// `next`
            Done = done,
            /// `continue`
            Continue = continue,
            /// `break`
            Break = break,
            /// `in` operator.
            In = in,
        }

        pub enum HeaderKey {
            /// The `title` header key. This is only parsed as a keyword when in header mode.
            Title = title,
            /// The `tags` header key. This is only parsed as a keyword when in header mode.
            Tags = tags,
        }

        /// Pragma names. Weak keywords that are only special in pragmas.
        pub enum Pragma {
            /// `feature`
            Feature = feature,
            /// `disable_feature`
            DisableFeature = disable_feature,
            /// `private`
            Private = private,
            /// `sub`
            Sub = sub,
        }

        /// Language feature names. Weak keywords that are only special in pragmas.
        pub enum FeatureName {
            /// `unicode_identifiers`
            UnicodeIdentifiers = unicode_identifiers,
            /// `scoped_nodes`
            ScopedNodes = scoped_nodes,
            /// `scoped_variables`
            ScopedVariables = scoped_variables,
            /// `subroutine`
            Subroutine = subroutine,
            /// `string_interpolation`
            StringInterpolation = string_interpolation,
            /// `extended_escape`
            ExtendedEscape = extended_escape,
        }
    }
}

macro_rules! decl_escape_char {
    {
        pub enum EscapeChar {
            $(#[$invalid_attr:meta])*
            Invalid,
            $(
                $(#[$attr:meta])*
                $name:ident = $chr:expr,
            )*
        }
    } => {
        /// Escape characters
        #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
        #[repr(u8)]
        pub enum EscapeChar {
            $(#[$invalid_attr])*
            Invalid,
            $(
                $(#[$attr])*
                $name,
            )*
        }

        impl EscapeChar {
            pub fn identify(s: char) -> Self {
                match s {
                    $(
                        $chr => Self::$name,
                    )*
                    _ => Self::Invalid,
                }
            }

            /// Returns the escaped character if valid.
            pub fn chr(self) -> Option<char> {
                match self {
                    $(
                        Self::$name => Some($chr),
                    )*
                    Self::Invalid => None,
                }
            }
        }
    };
}

decl_escape_char! {
    pub enum EscapeChar {
        /// Invalid escape
        Invalid,
        /// Backslash
        Backslash = '\\',
        /// Double quote
        DoubleQuote = '"',
        /// Left bracket
        LeftBracket = '[',
        /// Left angle bracket
        LeftAngleBracket = '<',
        /// Backtick (`string_interpolation`)
        Backtick = '`',
        /// Left brace (`string_interpolation`)
        Brace = '{',
        /// Newline (`extended_escape`)
        Newline = 'n',
        /// Tab (`extended_escape`)
        Tab = 't',
        /// Hashtag (`extended_escape`)
        Hashtag = '#',
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum PragmaStyle {
    /// `//#`
    Outer,
    /// `//#!`
    Inner,
}

/// Token kind.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum Kind {
    /* Binary operators */
    /* Assignment operators */
    Eq,
    BinOpEq(BinOp),

    /* Logical operators */
    /// `||`
    OrOr,
    /// `&&`
    AndAnd,
    /// `^`
    Xor,

    /* Comparison operators */
    /// `==`
    EqEq,
    /// `!=`
    Neq,
    /// `>`
    Gt,
    /// `<`
    Lt,
    /// `>=`
    Gte,
    /// `<=`
    Lte,

    /* Binary operators that can be part of assignment */
    BinOp(BinOp),

    /* Unary operators */
    /// `!`
    Not,
    /// `~`
    Tilde,

    /* Structural symbols */
    /// Line break
    LineBreak,
    /// End-of-file
    Eof,
    /// `//#` or `//#!`
    Pragma(PragmaStyle),
    /// `,`
    Comma,
    /// `.`
    Period,
    /// `->`
    Arrow,
    /// `|`
    Pipe,
    /// `|>`
    PipeArrow,
    /// Hash symbol `#`
    Hash,
    /// `---`
    TripleDash,
    /// `===`
    TripleEq,
    /// Header key-value separator `:`
    Colon,
    /// Opening delimiter
    OpenDelim(Delim),
    /// Closing delimiter
    CloseDelim(Delim),
    /// Indent token for shortcut options
    Indent,
    /// Un-indent token for shortcut options
    UnIndent,

    /* Identifiers */
    /// Keyword
    Keyword(Keyword),
    /// Identifier
    Ident,
    /// `$` sigil.
    Dollar,
    /// `@` sigil.
    At,
    /// `@@` sigil.
    AtAt,

    /* Literals and text */
    /// Decimal number.
    Number,
    /// Plain text (dialogue or inside string literals).
    Text,
    /// Single escaped character within text
    EscapeChar(EscapeChar),
    /// Byte escape
    EscapeByte,
    /// Unicode escape
    EscapeUnicode,

    /* Junk */
    /// Whitespace
    Whitespace,
    /// Non-pragma comment
    Comment,
    /// Characters that are not understood
    Unknown,
}

/// A locally spanned token.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Token {
    pub kind: Kind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: Kind, span: Span) -> Self {
        Token { kind, span }
    }

    pub fn with_kind(mut self, kind: Kind) -> Self {
        self.kind = kind;
        self
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BinOp as B;

        let show_as = match self {
            B::Plus => "+",
            B::Minus => "-",
            B::Star => "*",
            B::Slash => "/",
            B::Percent => "%",
        };

        f.write_str(show_as)
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Kind as K;

        match self {
            K::Eq => write!(f, "="),
            K::BinOpEq(bin_op) => write!(f, "{}=", bin_op),
            K::OrOr => write!(f, "||"),
            K::AndAnd => write!(f, "&&"),
            K::Xor => write!(f, "^"),
            K::EqEq => write!(f, "=="),
            K::Neq => write!(f, "!="),
            K::Gt => write!(f, ">"),
            K::Lt => write!(f, "<"),
            K::Gte => write!(f, ">="),
            K::Lte => write!(f, "<="),
            K::BinOp(bin_op) => write!(f, "{}", bin_op),
            K::Not => write!(f, "!"),
            K::Tilde => write!(f, "~"),
            K::LineBreak => write!(f, "line break"),
            K::Eof => write!(f, "end of file"),
            K::Pragma(PragmaStyle::Inner) => write!(f, "//#!"),
            K::Pragma(PragmaStyle::Outer) => write!(f, "//#"),
            K::Comma => write!(f, "','"),
            K::Period => write!(f, "'.'"),
            K::Arrow => write!(f, "->"),
            K::Pipe => write!(f, "|"),
            K::PipeArrow => write!(f, "|>"),
            K::Hash => write!(f, "#"),
            K::TripleDash => write!(f, "---"),
            K::TripleEq => write!(f, "==="),
            K::Colon => write!(f, ":"),

            K::OpenDelim(Delim::DoubleQuote) => write!(f, "\""),
            K::CloseDelim(Delim::DoubleQuote) => write!(f, "\""),
            K::OpenDelim(Delim::Backtick) => write!(f, "`"),
            K::CloseDelim(Delim::Backtick) => write!(f, "`"),
            K::OpenDelim(Delim::DoubleBracket) => write!(f, "[["),
            K::CloseDelim(Delim::DoubleBracket) => write!(f, "]]"),
            K::OpenDelim(Delim::DoubleAngleBracket) => write!(f, "<<"),
            K::CloseDelim(Delim::DoubleAngleBracket) => write!(f, ">>"),
            K::OpenDelim(Delim::Bracket) => write!(f, "["),
            K::CloseDelim(Delim::Bracket) => write!(f, "]"),
            K::OpenDelim(Delim::Paren) => write!(f, "("),
            K::CloseDelim(Delim::Paren) => write!(f, ")"),
            K::OpenDelim(Delim::Brace) => write!(f, "{{"),
            K::CloseDelim(Delim::Brace) => write!(f, "}}"),

            K::Indent => write!(f, "indent"),
            K::UnIndent => write!(f, "unindent"),
            K::Keyword(kw) => write!(f, "{}", kw),
            K::Ident => write!(f, "identifier"),
            K::Dollar => write!(f, "$"),
            K::At => write!(f, "@"),
            K::AtAt => write!(f, "@@"),
            K::Number => write!(f, "decimal number"),
            K::Text => write!(f, "text"),
            K::EscapeChar(_) | K::EscapeByte | K::EscapeUnicode => write!(f, "escape sequence"),

            K::Whitespace => write!(f, "whitespace"),
            K::Comment => write!(f, "comment"),
            K::Unknown => write!(f, "unparsable characters"),
        }
    }
}
