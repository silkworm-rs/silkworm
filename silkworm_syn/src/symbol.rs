//! Interned symbols

use std::mem;
use std::num::NonZeroU32;
use std::sync::atomic::{AtomicU32, Ordering};

use hashbrown::HashMap;

mod pin_buf;

use self::pin_buf::PinBuf;

/// An interned symbol.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Symbol {
    idx: SymbolIndex,
}

impl Symbol {
    fn new(brand: NonZeroU32, idx: usize) -> Self {
        Symbol {
            idx: SymbolIndex::new(brand, idx),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
struct SymbolIndex {
    brand: NonZeroU32,
    idx: u32,
}

impl SymbolIndex {
    fn new(brand: NonZeroU32, idx: usize) -> Self {
        debug_assert!(idx < u32::max_value() as usize);
        SymbolIndex {
            brand,
            idx: idx as u32,
        }
    }

    fn index(self) -> usize {
        self.idx as usize
    }
}

/// String interner
///
/// The interner places strings into non-moving buffers to reduce allocations
/// for small input.
///
/// The `&'static str`s in this structure actually points into `buf`.
#[derive(Debug)]
pub struct Interner {
    buf: PinBuf,

    /// "Branding" constant to avoid mixing of symbols from other instances.
    brand: NonZeroU32,

    lookup: HashMap<&'static str, Symbol>,
    strings: Vec<&'static str>,
}

static BRAND_COUNTER: AtomicU32 = AtomicU32::new(1);

impl Interner {
    pub fn new() -> Self {
        let brand = BRAND_COUNTER.fetch_add(1, Ordering::AcqRel);
        let brand =
            NonZeroU32::new(brand).expect("amount of interners created should be reasonable");

        Interner {
            buf: PinBuf::new(),
            brand,
            lookup: HashMap::new(),
            strings: Vec::new(),
        }
    }

    /// Get or intern a symbol.
    pub fn intern(&mut self, string: &str) -> Symbol {
        // SAFETY: The str returned from PinBuf is equal to input. Static
        // lifetime restricted to this type.
        unsafe {
            let Interner {
                buf,
                brand,
                lookup,
                strings,
            } = self;

            let (_, sym) = lookup.raw_entry_mut().from_key(string).or_insert_with(|| {
                let ptr = buf.push_str(string);
                let key = mem::transmute::<*const str, &'static str>(ptr);
                let idx = strings.len();
                strings.push(key);
                (key, Symbol::new(*brand, idx))
            });

            *sym
        }
    }

    /// Get the string value of an interned symbol.
    ///
    /// # Panics
    ///
    /// If `symbol` was not interned in this specific instance of `Interner`.
    pub fn read(&self, symbol: Symbol) -> &str {
        assert_eq!(self.brand, symbol.idx.brand, "should be the same brand");
        self.strings[symbol.idx.index()]
    }
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_intern_symbols() {
        let mut interner = Interner::new();

        let sym_1_a = interner.intern("foo");
        let sym_2_a = interner.intern("bar");
        let sym_3_a = interner.intern("baz");
        let sym_1_b = interner.intern("foo");
        let sym_2_b = interner.intern("bar");
        let sym_3_b = interner.intern("baz");

        assert_eq!(sym_1_a, sym_1_b);
        assert_eq!(sym_2_a, sym_2_b);
        assert_eq!(sym_3_a, sym_3_b);

        assert_ne!(sym_1_a, sym_2_a);
        assert_ne!(sym_2_a, sym_3_a);
        assert_ne!(sym_1_a, sym_3_a);

        assert_eq!("foo", interner.read(sym_1_a));
        assert_eq!("bar", interner.read(sym_2_a));
        assert_eq!("baz", interner.read(sym_3_a));
    }
}
