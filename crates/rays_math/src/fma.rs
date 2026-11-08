/// Correctly rounded fused multiply-add. `f64::mul_add` is a single instruction on aarch64; the
/// Windows target keeps the C `fma` because its software fallback rounded subnormals wrongly.
#[cfg(windows)]
pub fn fma(a: f64, b: f64, c: f64) -> f64 { rays_math_sys::fma(a, b, c) }
#[cfg(not(windows))]
pub fn fma(a: f64, b: f64, c: f64) -> f64 { a.mul_add(b, c) }

#[cfg(test)]
mod tests {
    #[test]
    fn subnormal_result_rounds_to_nearest() {
        // This exact FMA rounds up to ...681; the Windows software fallback returned ...680.
        assert_eq!(super::fma(0.1, 1e-308, 1e-308).to_bits(), 0x0007_e8eb_ea82_e681);
        assert_eq!(super::fma(-0.1, 1e-308, -1e-308).to_bits(), 0x8007_e8eb_ea82_e681);
        assert_eq!(super::fma(3.0, 5.0, 7.0), 22.0);
        assert_eq!(
            super::fma(1.0000000000000002, 1.0000000000000002, -1.0000000000000004),
            4.930380657631324e-32,
        );
    }
}
