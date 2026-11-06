#[derive(Clone, Copy, Debug, PartialEq)]
pub struct T {
    pub x: f64,
    pub y: f64,
    pub z: f64,
    pub w: f64,
}

pub fn create(x: f64, y: f64, z: f64, w: f64) -> T { T { x, y, z, w } }
const IDENTITY: T = T { x: 0.0, y: 0.0, z: 0.0, w: 1.0 };

fn length_squared(value: T) -> f64 {
    (value.x * value.x) + (value.y * value.y) + (value.z * value.z) + (value.w * value.w)
}

pub fn normalize(value: T) -> T {
    let length = length_squared(value).sqrt();
    if length <= 1e-12 {
        IDENTITY
    } else {
        T { x: value.x / length, y: value.y / length, z: value.z / length, w: value.w / length }
    }
}

pub fn mul(left: T, right: T) -> T {
    T {
        w: (left.w * right.w) - (left.x * right.x) - (left.y * right.y) - (left.z * right.z),
        x: (left.w * right.x) + (left.x * right.w) + (left.y * right.z) - (left.z * right.y),
        y: (left.w * right.y) - (left.x * right.z) + (left.y * right.w) + (left.z * right.x),
        z: (left.w * right.z) + (left.x * right.y) - (left.y * right.x) + (left.z * right.w),
    }
}
