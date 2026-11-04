#[derive(Clone, Copy, Debug, PartialEq)]
pub struct T {
    pub x: f64,
    pub y: f64,
}

pub fn create(x: f64, y: f64) -> T {
    T { x, y }
}

const ZERO: T = T { x: 0.0, y: 0.0 };
pub fn zero() -> T { ZERO }

pub fn add(a: T, b: T) -> T {
    T { x: a.x + b.x, y: a.y + b.y }
}

pub fn sub(a: T, b: T) -> T {
    T { x: a.x - b.x, y: a.y - b.y }
}

pub fn to_string(a: T) -> String {
    format!("({}, {})", crate::format_g(a.x), crate::format_g(a.y))
}
