//! Platform-independent trigonometry: the FreeBSD msun / musl routines as ported by rust-lang/libm
//! (MIT; Sun's notices kept in each file), copied from the toolchain's
//! `library/compiler-builtins/libm/src/math` (Rust 1.99.1). `f64::sin` and friends call the
//! platform libm, whose last bits differ between macOS releases and Windows' ucrtbase, so Lisp
//! trig and path arcs read these instead: the same bits on every platform (< 1 ulp, not always
//! correctly rounded). Only pure IEEE operations are used; Rust never contracts them into FMA.
//! ponytail: trig only. exp/log/pow and the rdk/editor/world trig still call the platform libm;
//! route them here when a cross-platform golden trips on them.
#![allow(clippy::all, unused_unsafe)]

macro_rules! force_eval {
    ($e:expr) => {
        let _ = $e;
    };
}

macro_rules! i {
    ($array:expr, $index:expr) => { $array[$index] };
    ($array:expr, $index:expr, = , $rhs:expr) => { $array[$index] = $rhs; };
    ($array:expr, $index:expr, -= , $rhs:expr) => { $array[$index] -= $rhs; };
    ($array:expr, $index:expr, += , $rhs:expr) => { $array[$index] += $rhs; };
    ($array:expr, $index:expr, &= , $rhs:expr) => { $array[$index] &= $rhs; };
    ($array:expr, $index:expr, == , $rhs:expr) => { $array[$index] == $rhs };
}

mod acos;
mod asin;
mod atan;
mod atan2;
mod cos;
mod k_cos;
mod k_sin;
mod k_tan;
mod rem_pio2;
mod rem_pio2_large;
mod sin;
mod tan;

pub use acos::acos;
pub use asin::asin;
pub use atan::atan;
pub use atan2::atan2;
pub use cos::cos;
pub use sin::sin;
pub use tan::tan;
use k_cos::k_cos;
use k_sin::k_sin;
use k_tan::k_tan;
use rem_pio2::rem_pio2;
use rem_pio2_large::rem_pio2_large;

fn fabs(x: f64) -> f64 {
    x.abs()
}
fn sqrt(x: f64) -> f64 {
    x.sqrt()
}
fn get_high_word(x: f64) -> u32 {
    (x.to_bits() >> 32) as u32
}
fn get_low_word(x: f64) -> u32 {
    x.to_bits() as u32
}
fn with_set_low_word(f: f64, lo: u32) -> f64 {
    f64::from_bits((f.to_bits() & 0xffffffff_00000000) | lo as u64)
}
/// x * 2^n, exact unless it over- or underflows (musl's scalbn)
fn scalbn(mut x: f64, mut n: i32) -> f64 {
    let x1p1023 = f64::from_bits(0x7fe0000000000000);
    let x1p53 = f64::from_bits(0x4340000000000000);
    let x1p_1022 = f64::from_bits(0x0010000000000000);
    if n > 1023 {
        x *= x1p1023;
        n -= 1023;
        if n > 1023 {
            x *= x1p1023;
            n = (n - 1023).min(1023);
        }
    } else if n < -1022 {
        x *= x1p_1022 * x1p53;
        n += 1022 - 53;
        if n < -1022 {
            x *= x1p_1022 * x1p53;
            n = (n + 1022 - 53).max(-1022);
        }
    }
    x * f64::from_bits(((0x3ff + n) as u64) << 52)
}

#[cfg(test)]
mod tests {
    // Bits this module returns on every platform, from the inputs that diverged between macOS 27
    // and Windows ucrtbase (particles' cos/sin(19 * 0.1)), pi, a 2^20 pi/2 reduction and the large
    // reduction of sketches' hash idiom.
    #[test]
    fn fixed_bits() {
        let cases = [
            (1.9000000000000001, 0x3fee48171f1a3857, 0xbfd4b0c6b93445ae),
            (std::f64::consts::PI, 0x3ca1a62633145c07, 0xbff0000000000000),
            (1647099.3291652855, 0xbdd1a62633145c07, 0x3ff0000000000000),
            (16383.0 * 4096.0, 0xbfc9bf7746538789, 0xbfef588cc209694f),
            (1e300, 0xbfea2c16b010e385, 0xbfe2699022adc4c1),
        ];
        for (x, s, c) in cases {
            assert_eq!((super::sin(x).to_bits(), super::cos(x).to_bits()), (s, c), "{x}");
        }
    }
}
