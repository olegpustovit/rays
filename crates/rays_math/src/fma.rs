/// Correctly rounded fused multiply-add. Windows explicitly binds UCRT because Rust's GNU
/// target can use MinGW's inexact `fma` (rust-lang/rust#140515). The original subnormal
/// regression came from OCaml's emulation, not a measured Rust failure.
#[cfg(windows)]
pub fn fma(a: f64, b: f64, c: f64) -> f64 { rays_math_sys::fma(a, b, c) }
#[cfg(not(windows))]
pub fn fma(a: f64, b: f64, c: f64) -> f64 { a.mul_add(b, c) }

#[cfg(test)]
mod tests {
    #[test]
    fn subnormal_result_rounds_to_nearest() {
        // OCaml's former Windows emulation returned ...680 for the positive case.
        for (a, b, c, expected) in [(0.1, 1e-308, 1e-308, 0x0007_e8eb_ea82_e681), (-0.1, 1e-308, -1e-308, 0x8007_e8eb_ea82_e681)] {
            let (a, b, c) = std::hint::black_box((a, b, c));
            assert_eq!(super::fma(a, b, c).to_bits(), expected);
        }
        assert_eq!(super::fma(3.0, 5.0, 7.0), 22.0);
        assert_eq!(
            super::fma(1.0000000000000002, 1.0000000000000002, -1.0000000000000004),
            4.930380657631324e-32,
        );
    }

    #[test]
    fn finite_rounding_overflow_and_nonfinite_operands() {
        // MinGW#848's finite double-rounding witness: a nonfinite-result guard cannot fix it.
        let (a, b, c) = std::hint::black_box((f64::from_bits(0x5f30_0000_0000_0008), f64::from_bits(0x5f31_0000_0000_0000), f64::from_bits(1)));
        assert_eq!(super::fma(a, b, c).to_bits(), 0x7e71_0000_0000_0009);
        for (a, b, c, expected) in [(f64::MAX, 2., -f64::MAX, f64::MAX), (f64::MAX, 2., f64::NEG_INFINITY, f64::NEG_INFINITY)] {
            let (a, b, c) = std::hint::black_box((a, b, c));
            assert_eq!(super::fma(a, b, c).to_bits(), expected.to_bits());
        }
        for (a, b, c) in [(f64::INFINITY, 0., 1.), (f64::INFINITY, 1., f64::NEG_INFINITY)] {
            let (a, b, c) = std::hint::black_box((a, b, c));
            assert!(super::fma(a, b, c).is_nan());
        }
    }
}
