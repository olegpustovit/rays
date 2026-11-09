//! OCaml numeric helpers the modules share: `Float.min`/`Float.max` propagate NaN (`f64::min`
//! does not), and `int_of_float` truncates (Rust's `as` saturates; out of range is not relied on).

/// OCaml `Float.min`.
pub fn fmin(x: f64, y: f64) -> f64 {
    if x.is_nan() || y.is_nan() {
        return f64::NAN;
    }
    if y < x || (y == x && y.is_sign_negative()) { y } else { x }
}

/// OCaml `Float.max`.
pub fn fmax(x: f64, y: f64) -> f64 {
    if x.is_nan() || y.is_nan() {
        return f64::NAN;
    }
    if y > x || (y == x && !y.is_sign_negative()) { y } else { x }
}

/// OCaml `int_of_float`.
pub(crate) fn int_of_float(x: f64) -> i64 {
    x as i64
}
