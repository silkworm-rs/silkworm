#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Span {
    pub base: u32,
    pub len: u32,
}

impl Span {
    pub fn new(base: u32, len: u32) -> Self {
        Span { base, len }
    }
}
