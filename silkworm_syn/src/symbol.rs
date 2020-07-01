//! Interned symbols

use std::fmt;
use std::hash::{BuildHasher, Hash, Hasher};
use std::num::NonZeroU32;
use std::sync::atomic::{AtomicU32, Ordering};

use hashbrown::HashMap;
use parking_lot::RwLock;

mod pin_buf;

use self::pin_buf::PinBuf;

/// An interned symbol.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Symbol {
    idx: SymbolIndex,
}

impl Symbol {
    fn new(brand: NonZeroU32, bin: u8, idx: usize) -> Self {
        Symbol {
            idx: SymbolIndex::new(brand, bin, idx),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
struct SymbolIndex {
    brand_bin: NonZeroU32,
    idx: u32,
}

impl SymbolIndex {
    fn new(brand: NonZeroU32, bin: u8, idx: usize) -> Self {
        let brand_bin = NonZeroU32::new(brand.get() << 8 | bin as u32).unwrap();
        SymbolIndex {
            brand_bin,
            idx: idx as u32,
        }
    }

    fn brand(self) -> NonZeroU32 {
        NonZeroU32::new(self.brand_bin.get() >> 8).unwrap()
    }

    fn bin(self) -> u8 {
        (self.brand_bin.get() & 0xFF) as u8
    }

    fn check(self, brand: NonZeroU32, bin: u8) -> bool {
        let brand_bin = NonZeroU32::new(brand.get() << 8 | bin as u32).unwrap();
        self.brand_bin == brand_bin
    }

    fn index(self) -> usize {
        self.idx as usize
    }
}

/// Multi-threaded string interner.
///
/// The interner supports multi-thread use cases. and places strings into
/// non-moving buffers to reduce allocations for small input.
pub struct Interner {
    /// "Branding" constant to avoid mixing of symbols from other instances.
    brand: NonZeroU32,
    bin_mask: u8,
    build_hasher: ahash::RandomState,
    bins: Vec<RwLock<ThreadInterner>>,
}

impl fmt::Debug for Interner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Interner {{ bin_count = {} }}", self.bins.len())
    }
}

static BRAND_COUNTER: AtomicU32 = AtomicU32::new(1);
const MAX_BRAND: u32 = 0x00FF_FFFF;
const MAX_BINS: usize = 256;
const MIN_BINS: usize = 1;

impl Interner {
    pub fn new() -> Self {
        Self::with_max_bins(MAX_BINS)
    }

    pub fn with_max_bins(max_bins: usize) -> Self {
        assert!(
            max_bins <= MAX_BINS,
            "given bin cap must be at most {}",
            MAX_BINS
        );

        let brand = BRAND_COUNTER.fetch_add(1, Ordering::AcqRel);
        assert!(
            brand != 0 && (brand & !MAX_BRAND == 0),
            "amount of interners created should be reasonable"
        );
        let brand = NonZeroU32::new(brand).unwrap();

        let bins = create_bins(brand, max_bins);
        let count = bins.len();
        assert!(count.is_power_of_two() && count > 0 && count <= 256);
        let bin_mask = (count - 1) as u8;

        let build_hasher = ahash::RandomState::new();

        Interner {
            brand,
            bins,
            build_hasher,
            bin_mask,
        }
    }

    /// Get or intern a symbol.
    pub fn intern(&self, string: &str) -> Symbol {
        let mut hasher = self.build_hasher.build_hasher();
        string.hash(&mut hasher);

        let bin_hash = hasher.finish();
        let bin = ((bin_hash >> 17) as u8) & self.bin_mask;

        let mut interner = self.bins[bin as usize].write();
        interner.intern(string)
    }

    /// Get the string value of an interned symbol.
    ///
    /// # Panics
    ///
    /// If `symbol` was not interned in this specific instance of `ThreadInterner`.
    pub fn read(&self, symbol: Symbol) -> &str {
        assert_eq!(self.brand, symbol.idx.brand(), "should be the same brand");
        let bin = symbol.idx.bin();
        let interner = self.bins[bin as usize].read();
        let s = interner.read(symbol);

        // SAFETY: ThreadInterner never moves the strings it contain, and live as long
        // as `self`.
        unsafe { &*(s as *const str) }
    }
}

fn create_bins(brand: NonZeroU32, max_bins: usize) -> Vec<RwLock<ThreadInterner>> {
    let count = num_cpus::get()
        .next_power_of_two()
        .max(MIN_BINS)
        .min(max_bins);
    let mut bins = Vec::with_capacity(count);
    for bin in 0..count {
        bins.push(RwLock::new(ThreadInterner::new(brand, bin as u8)));
    }
    bins
}

/// Single-threaded string interner
///
/// The interner places strings into non-moving buffers to reduce allocations
/// for small input.
///
/// The `&'static str`s in this structure actually points into `buf`.
#[derive(Debug)]
struct ThreadInterner {
    buf: PinBuf,
    brand: NonZeroU32,
    bin: u8,
    lookup: HashMap<&'static str, Symbol>,
    strings: Vec<&'static str>,
}

impl ThreadInterner {
    fn new(brand: NonZeroU32, bin: u8) -> Self {
        ThreadInterner {
            buf: PinBuf::new(),
            brand,
            bin,
            lookup: HashMap::new(),
            strings: Vec::new(),
        }
    }

    /// Get or intern a symbol.
    fn intern(&mut self, string: &str) -> Symbol {
        // SAFETY: The str returned from PinBuf is equal to input. Static
        // lifetime restricted to this type.
        unsafe {
            let ThreadInterner {
                buf,
                brand,
                bin,
                lookup,
                strings,
            } = self;

            let (_, sym) = lookup.raw_entry_mut().from_key(string).or_insert_with(|| {
                let ptr = buf.push_str(string);
                let key = &*ptr;
                let idx = strings.len();
                strings.push(key);
                (key, Symbol::new(*brand, *bin, idx))
            });

            *sym
        }
    }

    /// Get the string value of an interned symbol.
    ///
    /// # Panics
    ///
    /// If `symbol` was not interned in this specific instance of `ThreadInterner`.
    fn read(&self, symbol: Symbol) -> &str {
        assert!(
            symbol.idx.check(self.brand, self.bin),
            "should be the same brand"
        );
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
        let interner = Interner::new();

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
