use std::{alloc, fmt, mem};

/// Pinned string buffers.
///
/// `PinBuf` is guaranteed to never move any data, ever. Raw boxed slices have
/// to be used instead of `Vec`s or `String`s to guarantee against reallocation.
/// The reallocation strategy of `Vec`s (and consequently `String`s) is problematic
/// because it will try to be clever and reallocate even when unnecessary while
/// bulk-inserting bytes.
pub(super) struct PinBuf {
    cur_len: usize,
    cur_buf: Box<[u8]>,
    full: Vec<Box<[u8]>>,
}

pub const DEFAULT_INITIAL_CAPACITY: usize = 1024;
pub const BYTE_ALIGN: usize = 1;

impl PinBuf {
    /// Creates an instance with the default initial capacity.
    ///
    /// Subsequent buffer allocations are doubles of the last.
    pub fn new() -> Self {
        Self::with_initial_capacity(DEFAULT_INITIAL_CAPACITY)
    }

    /// Creates an instance with at least the given initial capacity.
    ///
    /// Subsequent buffer allocations are doubles of the last.
    ///
    /// # Panics
    ///
    /// If `initial_capacity` is zero.
    pub fn with_initial_capacity(initial_capacity: usize) -> Self {
        let cur_buf = alloc_buf(initial_capacity);
        PinBuf {
            cur_len: 0,
            cur_buf,
            full: Vec::new(),
        }
    }

    /// Pushes a string into the buffer and returns its pointer.
    /// The returned pointers are valid until self is dropped.
    pub fn push_str(&mut self, s: &str) -> *const str {
        if self.cur_len + s.len() > self.cur_buf.len() {
            let cap = self
                .cur_buf
                .len()
                .checked_shl(1)
                .expect("capacity should not exceed usize");
            let cap = usize::max(s.len(), cap);
            let full_buf = mem::replace(&mut self.cur_buf, alloc_buf(cap));
            self.full.push(full_buf);
            self.cur_len = 0;
        }

        let offset = self.cur_len;
        self.cur_len += s.len();

        // SAFETY: bounds are checked above
        unsafe {
            let dst = self.cur_buf.get_unchecked_mut(offset..offset + s.len());
            dst.copy_from_slice(s.as_bytes());
            std::str::from_utf8_unchecked(dst) as *const str
        }
    }
}

impl Default for PinBuf {
    fn default() -> Self {
        Self::new()
    }
}

fn alloc_buf(size: usize) -> Box<[u8]> {
    let size = size.next_power_of_two();
    if size == 0 {
        panic!("size must be greater than 0");
    }

    // SAFETY: BYTE_ALIGN meets all the conditions for `from_size_align`.
    let layout = unsafe { alloc::Layout::from_size_align_unchecked(size, BYTE_ALIGN) };

    // SAFETY: layout has the same length as size
    let slice = unsafe { std::slice::from_raw_parts_mut(alloc::alloc_zeroed(layout), size) };

    // SAFETY: u8 is Copy, and the allocated memory is zeroed.
    unsafe { Box::from_raw(slice) }
}

impl fmt::Debug for PinBuf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "PinBuf {{ cur_len: {}, .. }}", self.cur_len)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_push_strings() {
        let mut pin_buf = PinBuf::with_initial_capacity(2);

        let strings = &["foo", "bar", "baz", "42"];

        let mut ptrs = Vec::new();

        for s in strings {
            ptrs.push(pin_buf.push_str(s));
        }

        for (i, &p) in ptrs.iter().enumerate() {
            let ptr_str = unsafe { &*p };
            assert_eq!(strings[i], ptr_str);
        }

        drop(pin_buf);
    }
}
