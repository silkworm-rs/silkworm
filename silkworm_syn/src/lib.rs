//! Parser for the Yarn interactive dialogue language.
//!
//! This crate is mostly intended to be an internal dependency of the
//! [`silkworm`](https://github.com/silkworm-rs/silkworm) project, an implementation of the
//! [Yarn](https://github.com/YarnSpinnerTool/YarnSpinner/) interactive dialogue language in
//! pure Rust. End users are expected to use `silkworm`, the user-facing API, instead of
//! depending on this crate directly.
//!
//! This crate contains the AST definitions for the language, a lexer, and a parser. All
//! components assume that all sources logically reside in a continuous space that can be
//! indexed using byte positions.

pub mod ast;
pub mod lex;
pub mod parse;
pub mod ptr;
pub mod span;
pub mod symbol;
pub mod token;
