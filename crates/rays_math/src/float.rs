//! Shared float policy: extrema propagate a canonical NaN and choose −0 for minimum,
//! +0 for maximum. Ordering follows IEEE total order, including NaN signs and payloads.
use std::cmp::Ordering;

#[inline]
pub fn compare(left: f64, right: f64) -> Ordering { left.total_cmp(&right) }

#[inline]
pub fn min(x: f64, y: f64) -> f64 {
    if x.is_nan() || y.is_nan() {
        return f64::NAN;
    }
    if x == 0.0 && y == 0.0 {
        return if x.is_sign_negative() { x } else { y };
    }
    if x < y { x } else { y }
}

#[inline]
pub fn max(x: f64, y: f64) -> f64 {
    if x.is_nan() || y.is_nan() {
        return f64::NAN;
    }
    if x == 0.0 && y == 0.0 {
        return if x.is_sign_negative() { y } else { x };
    }
    if x > y { x } else { y }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nan_signed_zero_extrema_and_total_order() {
        let negative_nan = f64::from_bits(0xfff8_0000_0000_0001);
        let nan = f64::from_bits(0x7ff8_0000_0000_0001);
        let next_nan = f64::from_bits(0x7ff8_0000_0000_0002);
        for a in [negative_nan, nan, next_nan] {
            for b in [f64::NEG_INFINITY, -0., 0., 1., f64::INFINITY, nan] {
                for result in [min(a, b), min(b, a), max(a, b), max(b, a)] {
                    assert_eq!(result.to_bits(), f64::NAN.to_bits());
                }
            }
        }
        for (a, b) in [(-0., 0.), (0., -0.)] {
            assert_eq!(min(a, b).to_bits(), (-0f64).to_bits());
            assert_eq!(max(a, b).to_bits(), 0f64.to_bits());
        }
        let ordered = [negative_nan, f64::NEG_INFINITY, -1., -0., 0., 1., f64::INFINITY, nan, next_nan];
        let mut values = ordered;
        values.reverse();
        values.sort_by(|a, b| compare(*a, *b));
        assert_eq!(values.map(f64::to_bits), ordered.map(f64::to_bits));
        assert_eq!(min(-1., f64::INFINITY), -1.);
        assert_eq!(max(1., f64::NEG_INFINITY), 1.);
    }
}
