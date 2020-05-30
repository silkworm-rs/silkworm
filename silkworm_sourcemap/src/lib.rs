//! Source-mapping for `silkworm`.
//!
//! This crate is mostly intended to be an internal dependency of the
//! [`silkworm`](https://github.com/silkworm-rs/silkworm) project, an implementation of the
//! [Yarn](https://github.com/YarnSpinnerTool/YarnSpinner/) interactive dialogue language in
//! pure Rust. End users are expected to use `silkworm`, the user-facing API, instead of
//! depending on this crate directly.
//!
//! Most of the internals of `silkworm` is developed with the assumption that all code will
//! reside in continuous memory addressable with bytes. This simplifies parsing, but erases
//! information about source location. `silkworm_sourcemap` contains the necessary facility
//! for converting byte-positions to locations in file and back.

mod span;

#[doc(inline)]
pub use span::{ErrorSpan, Span};
