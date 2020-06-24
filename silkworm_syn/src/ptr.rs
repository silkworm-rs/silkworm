/// Shorthand type alias for `Box`.
pub type P<T> = Box<T>;

#[allow(non_snake_case)]
pub fn P<T>(value: T) -> P<T> {
    Box::new(value)
}
