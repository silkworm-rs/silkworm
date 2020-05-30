//! Compiler diagnostics for `silkworm`.
//!
//! This crate is mostly intended to be an internal dependency of the
//! [`silkworm`](https://github.com/silkworm-rs/silkworm) project, an implementation of the
//! [Yarn](https://github.com/YarnSpinnerTool/YarnSpinner/) interactive dialogue language in
//! pure Rust. End users are expected to use `silkworm`, the user-facing API, instead of
//! depending on this crate directly.
//!
//! This crate defines the types used to construct compiler diagnostic info across the
//! `silkworm` crates.

use std::fmt::{self, Display};

use typed_arena::Arena;

mod error;

#[doc(inline)]
pub use error::{Error, ErrorBuilder};

/// Compiler error levels.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum Level {
    /// Warnings are diagnostic messages that don't prevent compilation.
    Warning,
    /// Errors are issues that prevent compilation.
    Error,
    /// Bugs indicate programming errors in the compiler. They prevent compilation.
    ///
    /// If you encounter a `Bug` level error when using `silkworm`, we would appreciate
    /// an error report at https://github.com/silkworm-rs/silkworm/issues.
    Bug,
}

impl Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Level::Warning => f.write_str("warning"),
            Level::Error => f.write_str("error"),
            Level::Bug => f.write_str("bug"),
        }
    }
}

/// A context for emitting errors.
#[derive(Default)]
pub struct ErrorCtx {
    errors: Arena<ErrorBuilder>,
}

impl fmt::Debug for ErrorCtx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("ErrorCtx { .. }")
    }
}

// This is safe because `Arena::alloc` always allocates new memory. No aliases to the
// same memory locations are ever created.
#[allow(clippy::mut_from_ref)]
impl ErrorCtx {
    /// Create a new empty `ErrorCtx`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns `true` if no errors have been emitted.
    pub fn is_empty(&self) -> bool {
        self.errors.len() == 0
    }

    /// Add a new owned `ErrorBuilder`.
    pub fn push(&self, err: ErrorBuilder) {
        self.errors.alloc(err);
    }

    /// Add a new warning.
    pub fn warn<S: Display>(&self, msg: S) -> &mut ErrorBuilder {
        self.errors.alloc(ErrorBuilder::new(Level::Warning, msg))
    }

    /// Add a new error.
    pub fn error<S: Display>(&self, msg: S) -> &mut ErrorBuilder {
        self.errors.alloc(ErrorBuilder::new(Level::Error, msg))
    }

    /// Add a new bug.
    pub fn bug<S: Display>(&self, msg: S) -> &mut ErrorBuilder {
        self.errors.alloc(ErrorBuilder::new(Level::Bug, msg))
    }

    pub fn into_vec(self) -> Vec<ErrorBuilder> {
        self.errors.into_vec()
    }
}
