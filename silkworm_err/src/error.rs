use std::fmt::{self, Debug, Display};

use backtrace::Backtrace;

use silkworm_sourcemap::{ErrorSpan, Span};

use super::Level;

#[derive(Clone)]
pub struct Error {
    level: Level,
    msg: String,
    span: Option<ErrorSpan>,
    backtrace: Option<Backtrace>,
}

impl Error {
    pub fn level(&self) -> Level {
        self.level
    }

    pub fn message<'this>(&'this self) -> impl Display + 'this {
        &self.msg
    }

    pub fn span(&self) -> Option<Span> {
        self.span.as_ref().map(|s| s.main)
    }

    pub fn annotated_spans(&self) -> Option<&ErrorSpan> {
        self.span.as_ref()
    }

    /// Returns the backtrace of this error if available.
    ///
    /// In debug builds (`#[cfg(debug_assertions)]`), backtraces are always captured. Otherwise,
    /// backtraces are only captured if `level` is `Bug`.
    pub fn backtrace(&self) -> Option<&Backtrace> {
        self.backtrace.as_ref()
    }
}

impl Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "Error {{\n    [{level}] {msg}",
            level = self.level,
            msg = &self.msg
        )?;
        if let Some(span) = self.span() {
            writeln!(f, "        at raw position: {:?}", span)?;
        }
        if let Some(backtrace) = self.backtrace() {
            let mut backtrace = backtrace.clone();
            backtrace.resolve();
            writeln!(f, "        source backtrace:\n{:#?}", backtrace)?;
        }
        write!(f, "}}")?;

        Ok(())
    }
}

#[cfg(debug_assertions)]
fn should_backtrace(_level: Level) -> bool {
    true
}

#[cfg(not(debug_assertions))]
fn should_backtrace(level: Level) -> bool {
    match level {
        Level::Warning | Level::Error => false,
        Level::Bug => true,
    }
}

#[derive(Clone, Debug)]
pub struct ErrorBuilder {
    error: Error,
}

impl ErrorBuilder {
    pub fn new<S: Display>(level: Level, msg: S) -> Self {
        let backtrace = if should_backtrace(level) {
            Some(Backtrace::new_unresolved())
        } else {
            None
        };

        ErrorBuilder {
            error: Error {
                level,
                msg: msg.to_string(),
                span: None,
                backtrace,
            },
        }
    }

    pub fn done(self) -> Error {
        self.error
    }

    pub fn msg<S: Display>(&mut self, msg: S) -> &mut Self {
        self.error.msg = msg.to_string();
        self
    }

    pub fn map_msg<F>(&mut self, op: F) -> &mut Self
    where
        F: FnOnce(&mut String),
    {
        op(&mut self.error.msg);
        self
    }

    pub fn span<S: Into<ErrorSpan>>(&mut self, span: S) -> &mut Self {
        self.error.span = Some(span.into());
        self
    }

    pub fn annotate_span<S: Display>(&mut self, span: Span, msg: S) -> &mut Self {
        let error_span = self.error.span.get_or_insert_with(|| ErrorSpan::new(span));
        error_span.annotate(span, msg);
        self
    }
}
