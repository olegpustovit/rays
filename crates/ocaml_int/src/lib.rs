/// Wraps an `i64` to OCaml's signed 63-bit `int` range.
#[inline]
pub fn wrap_int63(value: i64) -> i64 {
    let low = (value as u64) & ((1_u64 << 63) - 1);
    ((low << 1) as i64) >> 1
}

/// Performs OCaml's logical right shift on a signed 63-bit integer.
/// OCaml masks the shift count to the low six bits of the machine word.
#[inline]
pub fn lsr(value: i64, amount: i64) -> i64 {
    let bits = (wrap_int63(value) as u64) & ((1_u64 << 63) - 1);
    wrap_int63((bits >> ((amount as u64 & 63) as u32)) as i64)
}

#[cfg(test)]
mod tests {
    use super::{lsr, wrap_int63};

    #[test]
    fn wraps_to_signed_63_bit_range() {
        assert_eq!(wrap_int63(i64::MAX), -1);
        assert_eq!(wrap_int63(i64::MIN), 0);
        assert_eq!(wrap_int63(1_i64 << 62), -(1_i64 << 62));
        assert_eq!(wrap_int63((1_i64 << 62) - 1), (1_i64 << 62) - 1);
        assert_eq!(wrap_int63(-(1_i64 << 62) - 1), (1_i64 << 62) - 1);
    }

    #[test]
    fn logical_shift_matches_ocaml_for_negative_values_and_edge_counts() {
        let cases = [
            (0, -1),
            (1, (1_i64 << 62) - 1),
            (2, (1_i64 << 61) - 1),
            (62, 1),
            (63, 0),
            (64, -1),
            (65, (1_i64 << 62) - 1),
            (-1, 0),
            (-2, 1),
        ];
        for (amount, expected) in cases {
            assert_eq!(lsr(-1, amount), expected, "-1 lsr {amount}");
        }
        assert_eq!(lsr(-(1_i64 << 62), 1), 1_i64 << 61);
        assert_eq!(lsr(i64::MAX, 0), -1);
    }
}
