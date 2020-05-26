//! The AST pointer.
//!
//! Provides `P<T>`, a frozen owned smart pointer.
//!
//! # License
//!
//! This is mostly stolen from rustc, licensed under the MIT License.

use std::fmt::{self, Debug, Display};
use std::iter::FromIterator;
use std::ops::{Deref, DerefMut};
use std::{slice, vec};

/// An owned smart pointer.
pub struct P<T: ?Sized> {
    ptr: Box<T>,
}

/// Construct a `P<T>` from a `T` value.
#[allow(non_snake_case)]
pub fn P<T: 'static>(value: T) -> P<T> {
    P {
        ptr: Box::new(value),
    }
}

impl<T: 'static> P<T> {
    /// Move out of the pointer.
    /// Intended for chaining transformations not covered by `map`.
    pub fn and_then<U, F>(self, f: F) -> U
    where
        F: FnOnce(T) -> U,
    {
        f(*self.ptr)
    }

    /// Equivalent to `and_then(|x| x)`.
    pub fn into_inner(self) -> T {
        *self.ptr
    }

    /// Produce a new `P<T>` from `self` without reallocating.
    pub fn map<F>(mut self, f: F) -> P<T>
    where
        F: FnOnce(T) -> T,
    {
        let x = f(*self.ptr);
        *self.ptr = x;

        self
    }

    /// Optionally produce a new `P<T>` from `self` without reallocating.
    pub fn filter_map<F>(mut self, f: F) -> Option<P<T>>
    where
        F: FnOnce(T) -> Option<T>,
    {
        *self.ptr = f(*self.ptr)?;
        Some(self)
    }
}

impl<T: ?Sized> Deref for P<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.ptr
    }
}

impl<T: ?Sized> DerefMut for P<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.ptr
    }
}

impl<T: 'static + Clone> Clone for P<T> {
    fn clone(&self) -> P<T> {
        P((**self).clone())
    }
}

impl<T: ?Sized + Debug> Debug for P<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.ptr, f)
    }
}

impl<T: Display> Display for P<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&**self, f)
    }
}

impl<T> fmt::Pointer for P<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Pointer::fmt(&self.ptr, f)
    }
}

impl<T> P<[T]> {
    pub fn new() -> P<[T]> {
        P { ptr: Box::new([]) }
    }

    #[inline(never)]
    pub fn from_vec(v: Vec<T>) -> P<[T]> {
        P {
            ptr: v.into_boxed_slice(),
        }
    }

    #[inline(never)]
    pub fn into_vec(self) -> Vec<T> {
        self.ptr.into_vec()
    }
}

impl<T> Default for P<[T]> {
    /// Creates an empty `P<[T]>`.
    fn default() -> P<[T]> {
        P::new()
    }
}

impl<T: Clone> Clone for P<[T]> {
    fn clone(&self) -> P<[T]> {
        P::from_vec(self.to_vec())
    }
}

impl<T> From<Vec<T>> for P<[T]> {
    fn from(v: Vec<T>) -> Self {
        P::from_vec(v)
    }
}

impl<T> Into<Vec<T>> for P<[T]> {
    fn into(self) -> Vec<T> {
        self.into_vec()
    }
}

impl<T> FromIterator<T> for P<[T]> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> P<[T]> {
        P::from_vec(iter.into_iter().collect())
    }
}

impl<T> IntoIterator for P<[T]> {
    type Item = T;
    type IntoIter = vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_vec().into_iter()
    }
}

impl<'a, T> IntoIterator for &'a P<[T]> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.ptr.into_iter()
    }
}
