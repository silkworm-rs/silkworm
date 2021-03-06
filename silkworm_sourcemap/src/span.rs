use std::fmt::Display;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Span {
    pub base: u32,
    pub len: u32,
}

impl Span {
    pub fn new(base: u32, len: u32) -> Self {
        Span { base, len }
    }

    /// Returns a `Span { base: 0xFFFF_FFFF, len: 0 }`. This is a special value
    /// that is reserved for invalid `Span`s.
    pub fn nil() -> Self {
        Span {
            base: 0xFFFF_FFFF,
            len: 0,
        }
    }

    /// Returns `true` if `self` is the `nil` span.
    pub fn is_nil(self) -> bool {
        self.base == 0xFFFF_FFFF && self.len == 0
    }

    /// Returns the union between two spans.
    ///
    /// # Panics
    ///
    /// If `self` or `rhs` is the nil span.
    pub fn union(self, rhs: Span) -> Span {
        assert!(!self.is_nil());
        assert!(!rhs.is_nil());

        let base = u32::min(self.base, rhs.base);
        let end = u32::max(self.base + self.len, rhs.base + rhs.len);

        Span::new(base, end - base)
    }

    /// Returns an empty span at `self.base`.
    pub fn empty(self) -> Span {
        Span::new(self.base, 0)
    }

    /// Returns an empty span at the end of the current span.
    pub fn empty_end(self) -> Span {
        Span::new(self.base + self.len, 0)
    }

    /// Returns the corresponding slice from source.
    ///
    /// # Panics
    ///
    /// If the span is out=of-bounds for source.
    pub fn read(self, source: &str, span_base: u32) -> &str {
        let base = self
            .base
            .checked_sub(span_base)
            .expect("self.base should >= span_base");
        &source[base as usize..(base + self.len) as usize]
    }
}

impl Default for Span {
    fn default() -> Self {
        Span::nil()
    }
}

#[derive(Clone, Debug)]
pub struct ErrorSpan {
    pub main: Span,
    pub annotations: Vec<(Span, String)>,
}

impl ErrorSpan {
    pub fn new(main: Span) -> Self {
        ErrorSpan {
            main,
            annotations: Vec::new(),
        }
    }

    pub fn annotate<S: Display>(&mut self, span: Span, msg: S) -> &mut Self {
        self.annotations.push((span, msg.to_string()));
        self
    }

    pub fn with_annotation<S: Display>(mut self, span: Span, msg: S) -> Self {
        self.annotate(span, msg);
        self
    }
}

impl From<Span> for ErrorSpan {
    fn from(span: Span) -> Self {
        ErrorSpan::new(span)
    }
}
