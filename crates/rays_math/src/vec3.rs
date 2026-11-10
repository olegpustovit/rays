#[derive(Clone, Copy, Debug, PartialEq)]
pub struct T {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

pub fn create(x: f64, y: f64, z: f64) -> T {
    T { x, y, z }
}

const ZERO: T = T { x: 0.0, y: 0.0, z: 0.0 };
const UNIT_X: T = T { x: 1.0, y: 0.0, z: 0.0 };
const UNIT_Y: T = T { x: 0.0, y: 1.0, z: 0.0 };
const UNIT_Z: T = T { x: 0.0, y: 0.0, z: 1.0 };
pub fn zero() -> T { ZERO }
pub fn unit_x() -> T { UNIT_X }
pub fn unit_y() -> T { UNIT_Y }
pub fn unit_z() -> T { UNIT_Z }

pub fn add(a: T, b: T) -> T {
    T { x: a.x + b.x, y: a.y + b.y, z: a.z + b.z }
}

pub fn sub(a: T, b: T) -> T {
    T { x: a.x - b.x, y: a.y - b.y, z: a.z - b.z }
}

pub fn neg(a: T) -> T {
    T { x: -a.x, y: -a.y, z: -a.z }
}

pub fn scale(a: T, value: f64) -> T {
    T { x: a.x * value, y: a.y * value, z: a.z * value }
}

pub fn dot(a: T, b: T) -> f64 {
    (a.x * b.x) + (a.y * b.y) + (a.z * b.z)
}

pub fn cross(a: T, b: T) -> T {
    T {
        x: (a.y * b.z) - (a.z * b.y),
        y: (a.z * b.x) - (a.x * b.z),
        z: (a.x * b.y) - (a.y * b.x),
    }
}

pub fn length_sq(a: T) -> f64 { dot(a, a) }
pub fn length(a: T) -> f64 { length_sq(a).sqrt() }

pub fn normalize(a: T) -> T {
    let value = length(a);
    if value == 0.0 { ZERO } else { scale(a, 1.0 / value) }
}

pub fn nearly_equal(a: T, b: T, eps: f64) -> bool {
    (a.x - b.x).abs() <= eps && (a.y - b.y).abs() <= eps && (a.z - b.z).abs() <= eps
}

pub fn to_triple(a: T) -> (f64, f64, f64) { (a.x, a.y, a.z) }
pub fn to_string(a: T) -> String {
    format!("({}, {}, {})", a.x, a.y, a.z)
}
