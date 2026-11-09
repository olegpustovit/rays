//! Numeric helpers shared by runtime modules.
pub use rays_math::float::{min as fmin, max as fmax};
/// OCaml `int_of_float`.
pub(crate) fn int_of_float(x: f64) -> i64 {
    x as i64
}
