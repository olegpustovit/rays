//! exact dyadic arithmetic for binary64 predicate
//! fallback. Magnitudes are little-endian base-2^64 limbs. `Invalid_argument` becomes a panic.
use std::cell::RefCell;

const LIMB_BITS: i64 = 64;
const LIMB_BASE: f64 = 18_446_744_073_709_551_616.;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct T {
    negative: bool,
    magnitude: Vec<u64>,
    exponent: i64,
}

fn zero() -> T {
    T { negative: false, magnitude: Vec::new(), exponent: 0 }
}

pub fn is_zero(value: &T) -> bool {
    value.magnitude.is_empty()
}

fn float_parts(value: f64) -> (bool, u64, i64) {
    assert!(value.is_finite(), "Rdk exact arithmetic: values must be finite");
    let bits = value.to_bits();
    let negative = bits >> 63 != 0;
    let exponent = ((bits >> 52) & 0x7ff) as i64;
    let fraction = bits & 0x000f_ffff_ffff_ffff;
    if exponent == 0 {
        (negative, fraction, -1074)
    } else {
        (negative, fraction | (1 << 52), exponent - 1023 - 52)
    }
}

pub fn of_float(value: f64) -> T {
    let (negative, magnitude, exponent) = float_parts(value);
    if magnitude == 0 { zero() } else { T { negative, magnitude: vec![magnitude], exponent } }
}

pub fn add(left: &T, right: &T) -> T {
    let scope = scratch::Scope::new();
    scope.export(scope.add(&scope.of_exact(left), &scope.of_exact(right)))
}

pub fn subtract(left: &T, right: &T) -> T {
    let scope = scratch::Scope::new();
    scope.export(scope.subtract(&scope.of_exact(left), &scope.of_exact(right)))
}

pub fn multiply(left: &T, right: &T) -> T {
    let scope = scratch::Scope::new();
    scope.export(scope.multiply(&scope.of_exact(left), &scope.of_exact(right)))
}

pub fn negate(value: &T) -> T {
    T { negative: !value.negative && !is_zero(value), ..value.clone() }
}

/// Return -1, 0, or 1.
pub fn compare_zero(value: &T) -> i64 {
    if is_zero(value) {
        0
    } else if value.negative {
        -1
    } else {
        1
    }
}

/// `value = mantissa * 2 ** exponent` approximately; the mantissa is scaled by a whole number of limbs.
/// Only for spatial acceleration and final materialization, never for combinatorial decisions.
fn scaled_mantissa(negative: bool, magnitude: &[u64]) -> f64 {
    let count = magnitude.len();
    let first = count.saturating_sub(4);
    let mut accumulator = 0.;
    let mut scale = 1. / LIMB_BASE as f64;
    let mut index = count;
    while index > first {
        index -= 1;
        accumulator += magnitude[index] as f64 * scale;
        scale /= LIMB_BASE as f64;
    }
    let mantissa = if negative { -accumulator } else { accumulator };
    mantissa
}

pub fn to_scaled_float(value: &T) -> (f64, i64) {
    (scaled_mantissa(value.negative, &value.magnitude), value.exponent + value.magnitude.len() as i64 * LIMB_BITS)
}

/// Certified outward-rounded `(lower, upper, exponent)`, both scaled by `2 ** exponent`.
pub fn to_scaled_interval(value: &T) -> (f64, f64, i64) {
    if is_zero(value) {
        return (0., 0., 0);
    }
    let count = value.magnitude.len();
    let mut lower = 0.;
    let mut upper = 0.;
    let mut scale = 1. / LIMB_BASE as f64;
    let mut lost_positive_tail = false;
    for index in (0..count).rev() {
        let limb = value.magnitude[index];
        if scale == 0. {
            if limb != 0 {
                lost_positive_tail = true;
            }
        } else if limb != 0 {
            let term = limb as f64 * scale;
            lower = (lower + term).next_down();
            upper = (upper + term).next_up();
        }
        scale /= LIMB_BASE as f64;
    }
    if lost_positive_tail {
        upper = upper.next_up();
    }
    let exponent = value.exponent + count as i64 * LIMB_BITS;
    if value.negative { (-upper, -lower, exponent) } else { (lower, upper, exponent) }
}

/// Scale by a power of two with nearest-even rounding into subnormals.
pub fn ldexp(value: f64, exponent: i64) -> f64 {
    let bits = value.to_bits();
    let sign = bits & 0x8000_0000_0000_0000;
    let encoded = ((bits >> 52) & 0x7ff) as i64;
    let fraction = bits & 0x000f_ffff_ffff_ffff;
    if encoded == 0x7ff || value == 0. {
        return value;
    }
    let (significand, base) = if encoded == 0 {
        let high = 63 - fraction.leading_zeros() as i64;
        let shift = 52 - high;
        (fraction << shift, high - 1074)
    } else {
        (fraction | (1 << 52), encoded - 1023)
    };
    let target = base as i128 + exponent as i128;
    if target > 1023 {
        return f64::from_bits(sign | (0x7ffu64 << 52));
    }
    if target >= -1022 {
        return f64::from_bits(
            sign | (((target + 1023) as u64) << 52) | (significand & 0x000f_ffff_ffff_ffff),
        );
    }
    if target < -1075 {
        return f64::from_bits(sign);
    }
    let shift = (-1022 - target) as u32;
    let quotient = significand >> shift;
    let remainder = significand & ((1u64 << shift) - 1);
    let halfway = 1u64 << (shift - 1);
    let up = remainder > halfway || (remainder == halfway && quotient & 1 != 0);
    f64::from_bits(sign | (quotient + if up { 1 } else { 0 }))
}

pub mod scratch {
    //! Domain-local packed exact predicate temporaries. Results are comparison signs -1, 0, 1;
    //! no arena-backed value escapes the call.
    use super::*;

    /// Index of a value in the arena.
    pub(crate) type V = usize;

    #[derive(Default)]
    pub struct Arena {
        limbs: Vec<u64>,
        limb_count: usize,
        negative: Vec<bool>,
        exponents: Vec<i64>,
        offsets: Vec<usize>,
        lengths: Vec<usize>,
        value_count: usize,
    }

    fn create() -> Arena {
        Arena {
            limbs: vec![0; 4_096],
            limb_count: 0,
            negative: vec![false; 256],
            exponents: vec![0; 256],
            offsets: vec![0; 256],
            lengths: vec![0; 256],
            value_count: 0,
        }
    }

    thread_local! {
        static POOL: RefCell<Vec<Arena>> = const { RefCell::new(Vec::new()) };
    }

    fn acquire() -> Arena {
        let pooled = POOL.with(|pool| pool.borrow_mut().pop());
        let mut arena = pooled.unwrap_or_else(create);
        arena.limb_count = 0;
        arena.value_count = 0;
        arena
    }

    fn release(arena: Arena) {
        POOL.with(|pool| pool.borrow_mut().push(arena));
    }


    /// Construction temporaries share the predicate engine. Only exported persistent
    /// homogeneous coordinates allocate owned storage; intermediate values stay in the arena.
    pub(crate) struct Scope(RefCell<Arena>);

    impl Scope {
        pub(crate) fn new() -> Self { Self(RefCell::new(acquire())) }
        pub(crate) fn of_float(&self, value: f64) -> V { self.0.borrow_mut().of_float(value) }
        pub(crate) fn of_exact(&self, value: &T) -> V { self.0.borrow_mut().of_exact(value) }
        pub(crate) fn add(&self, left: &V, right: &V) -> V { self.0.borrow_mut().add(*left, *right) }
        pub(crate) fn subtract(&self, left: &V, right: &V) -> V { self.0.borrow_mut().subtract(*left, *right) }
        pub(crate) fn multiply(&self, left: &V, right: &V) -> V { self.0.borrow_mut().multiply(*left, *right) }
        pub(crate) fn negate(&self, value: &V) -> V { self.0.borrow_mut().negate(*value) }
        pub(crate) fn is_zero(&self, value: &V) -> bool { self.0.borrow().is_zero(*value) }
        pub(crate) fn compare_zero(&self, value: &V) -> i64 { self.0.borrow().compare_zero(*value) }
        pub(crate) fn export(&self, value: V) -> T {
            let arena = self.0.borrow();
            let offset = arena.offsets[value];
            T {
                negative: arena.negative[value],
                exponent: arena.exponents[value],
                magnitude: arena.limbs[offset..offset + arena.lengths[value]].to_vec(),
            }
        }
    }

    impl Drop for Scope {
        fn drop(&mut self) { release(std::mem::take(self.0.get_mut())); }
    }

    impl Arena {
        fn value(&mut self, negative: bool, exponent: i64, offset: usize, length: usize) -> V {
            let needed = self.value_count + 1;
            if needed > self.lengths.len() {
                let mut capacity = self.lengths.len();
                while capacity < needed {
                    capacity *= 2;
                }
                self.negative.resize(capacity, false);
                self.exponents.resize(capacity, 0);
                self.offsets.resize(capacity, 0);
                self.lengths.resize(capacity, 0);
            }
            let result = self.value_count;
            self.value_count = result + 1;
            self.negative[result] = negative && length > 0;
            self.exponents[result] = if length == 0 { 0 } else { exponent };
            self.offsets[result] = offset;
            self.lengths[result] = length;
            result
        }

        fn zero_value(&mut self) -> V {
            let offset = self.limb_count;
            self.value(false, 0, offset, 0)
        }

        fn reserve_limbs(&mut self, count: usize) -> usize {
            let offset = self.limb_count;
            let needed = offset + count;
            if needed > self.limbs.len() {
                let mut capacity = self.limbs.len();
                while capacity < needed {
                    capacity *= 2;
                }
                self.limbs.resize(capacity, 0);
            }
            self.limb_count = needed;
            offset
        }

        fn trim(&self, offset: usize, length: usize) -> usize {
            let mut length = length;
            while length > 0 && self.limbs[offset + length - 1] == 0 {
                length -= 1;
            }
            length
        }

        fn of_float(&mut self, number: f64) -> V {
            let (negative, magnitude, exponent) = float_parts(number);
            if magnitude == 0 {
                self.zero_value()
            } else {
                let offset = self.reserve_limbs(1);
                self.limbs[offset] = magnitude;
                self.value(negative, exponent, offset, 1)
            }
        }

        fn of_exact(&mut self, number: &T) -> V {
            let length = number.magnitude.len();
            if length == 0 {
                self.zero_value()
            } else {
                let offset = self.reserve_limbs(length);
                self.limbs[offset..offset + length].copy_from_slice(&number.magnitude);
                self.value(number.negative, number.exponent, offset, length)
            }
        }

        fn is_zero(&self, number: V) -> bool {
            self.lengths[number] == 0
        }

        fn negate(&mut self, number: V) -> V {
            let (negative, exponent, offset, length) = (
                !self.negative[number],
                self.exponents[number],
                self.offsets[number],
                self.lengths[number],
            );
            self.value(negative, exponent, offset, length)
        }

        fn shifted_limb(&self, number: V, shift: usize, index: usize) -> u64 {
            let word_shift = (shift / 64) as i64;
            let bit_shift = (shift % 64) as i64;
            let source = index as i64 - word_shift;
            let length = self.lengths[number] as i64;
            let offset = self.offsets[number];
            if bit_shift == 0 {
                if source >= 0 && source < length {
                    self.limbs[offset + source as usize]
                } else {
                    0
                }
            } else {
                let low = if source >= 0 && source < length {
                    self.limbs[offset + source as usize] << bit_shift
                } else {
                    0
                };
                let carry = if source > 0 && source - 1 < length {
                    self.limbs[offset + source as usize - 1] >> (LIMB_BITS - bit_shift)
                } else {
                    0
                };
                low | carry
            }
        }

        fn shifted_length(&self, number: V, shift: usize) -> usize {
            let length = self.lengths[number];
            if length == 0 {
                0
            } else {
                let words = shift / 64;
                let bits = (shift % 64) as i64;
                let extra = if bits == 0 {
                    0
                } else {
                    let high = self.limbs[self.offsets[number] + length - 1];
                    if (high >> (LIMB_BITS - bits)) == 0 { 0 } else { 1 }
                };
                words + length + extra
            }
        }

        fn compare_shifted(&self, left: V, left_shift: usize, right: V, right_shift: usize) -> i64 {
            let left_length = self.shifted_length(left, left_shift);
            let right_length = self.shifted_length(right, right_shift);
            if left_length != right_length {
                return if left_length < right_length { -1 } else { 1 };
            }
            let mut index = left_length;
            while index > 0 {
                index -= 1;
                let l = self.shifted_limb(left, left_shift, index);
                let r = self.shifted_limb(right, right_shift, index);
                if l != r {
                    return if l < r { -1 } else { 1 };
                }
            }
            0
        }

        fn add(&mut self, left: V, right: V) -> V {
            if self.is_zero(left) {
                right
            } else if self.is_zero(right) {
                left
            } else {
                let exponent = self.exponents[left].min(self.exponents[right]);
                let left_shift = (self.exponents[left] - exponent) as usize;
                let right_shift = (self.exponents[right] - exponent) as usize;
                let left_length = self.shifted_length(left, left_shift);
                let right_length = self.shifted_length(right, right_shift);
                if self.negative[left] == self.negative[right] {
                    let count = left_length.max(right_length);
                    let offset = self.reserve_limbs(count + 1);
                    let mut carry = 0u128;
                    for index in 0..count {
                        let sum = self.shifted_limb(left, left_shift, index) as u128
                            + self.shifted_limb(right, right_shift, index) as u128 + carry;
                        self.limbs[offset + index] = sum as u64;
                        carry = sum >> LIMB_BITS;
                    }
                    self.limbs[offset + count] = carry as u64;
                    let negative = self.negative[left];
                    let length = if carry == 0 { count } else { count + 1 };
                    self.value(negative, exponent, offset, length)
                } else {
                    let comparison = self.compare_shifted(left, left_shift, right, right_shift);
                    if comparison == 0 {
                        self.zero_value()
                    } else {
                        let (larger, larger_shift, smaller, smaller_shift, negative) =
                            if comparison > 0 {
                                (left, left_shift, right, right_shift, self.negative[left])
                            } else {
                                (right, right_shift, left, left_shift, self.negative[right])
                            };
                        let count = self.shifted_length(larger, larger_shift);
                        let offset = self.reserve_limbs(count);
                        let mut borrow = 0;
                        for index in 0..count {
                            let (difference, first_borrow) = self.shifted_limb(larger, larger_shift, index)
                                .overflowing_sub(self.shifted_limb(smaller, smaller_shift, index));
                            let (difference, second_borrow) = difference.overflowing_sub(borrow);
                            self.limbs[offset + index] = difference;
                            borrow = u64::from(first_borrow || second_borrow);
                        }
                        assert!(borrow == 0);
                        let length = self.trim(offset, count);
                        self.value(negative, exponent, offset, length)
                    }
                }
            }
        }

        fn subtract(&mut self, left: V, right: V) -> V {
            let negated = self.negate(right);
            self.add(left, negated)
        }

        fn multiply(&mut self, left: V, right: V) -> V {
            if self.is_zero(left) || self.is_zero(right) {
                return self.zero_value();
            }
            let left_length = self.lengths[left];
            let right_length = self.lengths[right];
            let count = left_length + right_length;
            let offset = self.reserve_limbs(count);
            self.limbs[offset..offset + count].fill(0);
            let left_offset = self.offsets[left];
            let right_offset = self.offsets[right];
            for left_index in 0..left_length {
                let mut carry = 0u128;
                for right_index in 0..right_length {
                    let output_index = offset + left_index + right_index;
                    let product = self.limbs[left_offset + left_index] as u128
                        * self.limbs[right_offset + right_index] as u128
                        + self.limbs[output_index] as u128 + carry;
                    self.limbs[output_index] = product as u64;
                    carry = product >> LIMB_BITS;
                }
                self.limbs[offset + left_index + right_length] = carry as u64;
            }
            let negative = self.negative[left] != self.negative[right];
            let exponent = self.exponents[left] + self.exponents[right];
            let length = self.trim(offset, count);
            self.value(negative, exponent, offset, length)
        }

        fn compare_zero(&self, number: V) -> i64 {
            if self.is_zero(number) {
                0
            } else if self.negative[number] {
                -1
            } else {
                1
            }
        }

        /// a*b - c*d
        fn det2(&mut self, a: V, b: V, c: V, d: V) -> V {
            let left = self.multiply(a, b);
            let right = self.multiply(c, d);
            self.subtract(left, right)
        }

        /// a*b + c*d
        fn dot2(&mut self, a: V, b: V, c: V, d: V) -> V {
            let left = self.multiply(a, b);
            let right = self.multiply(c, d);
            self.add(left, right)
        }

        /// a*w - b
        fn scaled_minus(&mut self, a: V, w: V, b: V) -> V {
            let scaled = self.multiply(a, w);
            self.subtract(scaled, b)
        }

        /// a * (b*c - d*e)
        fn times_det2(&mut self, a: V, b: V, c: V, d: V, e: V) -> V {
            let inner = self.det2(b, c, d, e);
            self.multiply(a, inner)
        }

        /// coordinate * other_weight - other_coordinate * weight
        fn homogeneous_difference(
            &mut self,
            coordinate: V,
            weight: V,
            other_coordinate: V,
            other_weight: V,
        ) -> V {
            self.det2(coordinate, other_weight, other_coordinate, weight)
        }

        fn dot3(&mut self, ax: V, ay: V, az: V, bx: V, by: V, bz: V) -> V {
            let xy = self.dot2(ax, bx, ay, by);
            let z = self.multiply(az, bz);
            self.add(xy, z)
        }

        fn sum3(&mut self, first: V, second: V, third: V) -> V {
            let partial = self.add(first, second);
            self.add(partial, third)
        }

        fn scaled_mantissa(&self, number: V) -> f64 {
            let offset = self.offsets[number];
            super::scaled_mantissa(self.negative[number], &self.limbs[offset..offset + self.lengths[number]])
        }

        fn scaled_exponent(&self, number: V) -> i64 {
            self.exponents[number] + self.lengths[number] as i64 * LIMB_BITS
        }

        fn approximate_ratio(&self, numerator: V, denominator: V) -> f64 {
            if self.is_zero(numerator) {
                0.
            } else {
                ldexp(
                    self.scaled_mantissa(numerator) / self.scaled_mantissa(denominator),
                    self.scaled_exponent(numerator) - self.scaled_exponent(denominator),
                )
            }
        }
    }

    pub fn orient2d(ax: f64, ay: f64, bx: f64, by: f64, cx: f64, cy: f64) -> i64 {
        let mut ar = acquire();
        let ax = ar.of_float(ax);
        let ay = ar.of_float(ay);
        let bx = ar.of_float(bx);
        let by = ar.of_float(by);
        let cx = ar.of_float(cx);
        let cy = ar.of_float(cy);
        let acx = ar.subtract(ax, cx);
        let bcy = ar.subtract(by, cy);
        let acy = ar.subtract(ay, cy);
        let bcx = ar.subtract(bx, cx);
        let determinant = ar.det2(acx, bcy, acy, bcx);
        let result = ar.compare_zero(determinant);
        release(ar);
        result
    }

    /// Exact sign of twice a polygon's shoelace area.
    pub fn polygon_area_packed(x: &[f64], y: &[f64], points: &[i64], first: i64, count: i64) -> i64 {
        let mut ar = acquire();
        let mut sum = ar.of_float(0.);
        for local in 0..count {
            let next = if local + 1 == count { 0 } else { local + 1 };
            let a = points[(first + local) as usize] as usize;
            let b = points[(first + next) as usize] as usize;
            let ax = ar.of_float(x[a]);
            let ay = ar.of_float(y[a]);
            let bx = ar.of_float(x[b]);
            let by = ar.of_float(y[b]);
            let term = ar.det2(ax, by, bx, ay);
            sum = ar.add(sum, term);
        }
        let result = ar.compare_zero(sum);
        release(ar);
        result
    }

    #[allow(clippy::too_many_arguments)]
    pub fn orient3d(
        ax: f64,
        ay: f64,
        az: f64,
        bx: f64,
        by: f64,
        bz: f64,
        cx: f64,
        cy: f64,
        cz: f64,
        dx: f64,
        dy: f64,
        dz: f64,
    ) -> i64 {
        let mut ar = acquire();
        let ax = ar.of_float(ax);
        let ay = ar.of_float(ay);
        let az = ar.of_float(az);
        let bx = ar.of_float(bx);
        let by = ar.of_float(by);
        let bz = ar.of_float(bz);
        let cx = ar.of_float(cx);
        let cy = ar.of_float(cy);
        let cz = ar.of_float(cz);
        let dx = ar.of_float(dx);
        let dy = ar.of_float(dy);
        let dz = ar.of_float(dz);
        let adx = ar.subtract(ax, dx);
        let ady = ar.subtract(ay, dy);
        let adz = ar.subtract(az, dz);
        let bdx = ar.subtract(bx, dx);
        let bdy = ar.subtract(by, dy);
        let bdz = ar.subtract(bz, dz);
        let cdx = ar.subtract(cx, dx);
        let cdy = ar.subtract(cy, dy);
        let cdz = ar.subtract(cz, dz);
        let first = ar.times_det2(adx, bdy, cdz, bdz, cdy);
        let second = ar.times_det2(ady, bdz, cdx, bdx, cdz);
        let third = ar.times_det2(adz, bdx, cdy, bdy, cdx);
        let total = ar.sum3(first, second, third);
        let result = ar.compare_zero(total);
        release(ar);
        result
    }

    #[allow(clippy::too_many_arguments)]
    pub fn incircle(ax: f64, ay: f64, bx: f64, by: f64, cx: f64, cy: f64, dx: f64, dy: f64) -> i64 {
        let mut ar = acquire();
        let ax = ar.of_float(ax);
        let ay = ar.of_float(ay);
        let bx = ar.of_float(bx);
        let by = ar.of_float(by);
        let cx = ar.of_float(cx);
        let cy = ar.of_float(cy);
        let dx = ar.of_float(dx);
        let dy = ar.of_float(dy);
        let adx = ar.subtract(ax, dx);
        let ady = ar.subtract(ay, dy);
        let bdx = ar.subtract(bx, dx);
        let bdy = ar.subtract(by, dy);
        let cdx = ar.subtract(cx, dx);
        let cdy = ar.subtract(cy, dy);
        let alift = ar.dot2(adx, adx, ady, ady);
        let blift = ar.dot2(bdx, bdx, bdy, bdy);
        let clift = ar.dot2(cdx, cdx, cdy, cdy);
        let bcdet = ar.det2(bdx, cdy, cdx, bdy);
        let cadet = ar.det2(cdx, ady, adx, cdy);
        let abdet = ar.det2(adx, bdy, bdx, ady);
        let first = ar.multiply(alift, bcdet);
        let second = ar.multiply(blift, cadet);
        let third = ar.multiply(clift, abdet);
        let total = ar.sum3(first, second, third);
        let result = ar.compare_zero(total);
        release(ar);
        result
    }

    pub fn compare_products(
        left_first: &T,
        left_second: &T,
        right_first: &T,
        right_second: &T,
    ) -> i64 {
        let mut ar = acquire();
        let left_first = ar.of_exact(left_first);
        let left_second = ar.of_exact(left_second);
        let right_first = ar.of_exact(right_first);
        let right_second = ar.of_exact(right_second);
        let difference = ar.det2(left_first, left_second, right_first, right_second);
        let result = ar.compare_zero(difference);
        release(ar);
        result
    }

    pub fn compare_homogeneous_float(numerator: &T, denominator: &T, value: f64) -> i64 {
        let mut ar = acquire();
        let numerator = ar.of_exact(numerator);
        let denominator = ar.of_exact(denominator);
        let value = ar.of_float(value);
        let scaled = ar.multiply(value, denominator);
        let difference = ar.subtract(numerator, scaled);
        let result = ar.compare_zero(difference);
        release(ar);
        result
    }

    pub fn orient2d_ee_homogeneous(
        ax: f64,
        ay: f64,
        bx: f64,
        by: f64,
        cx: &T,
        cy: &T,
        cw: &T,
    ) -> i64 {
        let mut ar = acquire();
        let ax = ar.of_float(ax);
        let ay = ar.of_float(ay);
        let bx = ar.of_float(bx);
        let by = ar.of_float(by);
        let cx = ar.of_exact(cx);
        let cy = ar.of_exact(cy);
        let cw = ar.of_exact(cw);
        let acx = ar.scaled_minus(ax, cw, cx);
        let acy = ar.scaled_minus(ay, cw, cy);
        let bcx = ar.scaled_minus(bx, cw, cx);
        let bcy = ar.scaled_minus(by, cw, cy);
        let determinant = ar.det2(acx, bcy, acy, bcx);
        let result = ar.compare_zero(determinant);
        release(ar);
        result
    }

    /// Panics (`Invalid_argument`) when the point is not coplanar with the triangle or the
    /// triangle is degenerate.
    #[allow(clippy::too_many_arguments)]
    pub fn barycentric_tri_homogeneous(
        ax: f64,
        ay: f64,
        az: f64,
        bx: f64,
        by: f64,
        bz: f64,
        cx: f64,
        cy: f64,
        cz: f64,
        px: &T,
        py: &T,
        pz: &T,
        pw: &T,
    ) -> (f64, f64, f64) {
        let mut ar = acquire();
        let ax = ar.of_float(ax);
        let ay = ar.of_float(ay);
        let az = ar.of_float(az);
        let bx = ar.of_float(bx);
        let by = ar.of_float(by);
        let bz = ar.of_float(bz);
        let cx = ar.of_float(cx);
        let cy = ar.of_float(cy);
        let cz = ar.of_float(cz);
        let px = ar.of_exact(px);
        let py = ar.of_exact(py);
        let pz = ar.of_exact(pz);
        let pw = ar.of_exact(pw);
        let apx = ar.scaled_minus(ax, pw, px);
        let apy = ar.scaled_minus(ay, pw, py);
        let apz = ar.scaled_minus(az, pw, pz);
        let bpx = ar.scaled_minus(bx, pw, px);
        let bpy = ar.scaled_minus(by, pw, py);
        let bpz = ar.scaled_minus(bz, pw, pz);
        let cpx = ar.scaled_minus(cx, pw, px);
        let cpy = ar.scaled_minus(cy, pw, py);
        let cpz = ar.scaled_minus(cz, pw, pz);
        let c1 = ar.times_det2(apx, bpy, cpz, bpz, cpy);
        let c2 = ar.times_det2(apy, bpz, cpx, bpx, cpz);
        let c3 = ar.times_det2(apz, bpx, cpy, bpy, cpx);
        let coplanar = ar.sum3(c1, c2, c3);
        if ar.compare_zero(coplanar) != 0 {
            unreachable!("Rdk barycentric point is not coplanar with its source triangle");
        }
        let result = match barycentric_weights(&mut ar, [ax, ay, bx, by, cx, cy, px, py], pw) {
            Some(result) => result,
            None => match barycentric_weights(&mut ar, [ay, az, by, bz, cy, cz, py, pz], pw) {
                Some(result) => result,
                None => match barycentric_weights(&mut ar, [az, ax, bz, bx, cz, cx, pz, px], pw) {
                    Some(result) => result,
                    None => unreachable!("Rdk barycentric source triangle is degenerate"),
                },
            },
        };
        release(ar);
        result
    }

    /// `v` is `[af, as_, bf, bs, cf, cs, pf, ps]`.
    fn barycentric_weights(ar: &mut Arena, v: [V; 8], pw: V) -> Option<(f64, f64, f64)> {
        let [af, as_, bf, bs, cf, cs, pf, ps] = v;
        let u = ar.subtract(af, cf);
        let w = ar.subtract(bs, cs);
        let u2 = ar.subtract(as_, cs);
        let w2 = ar.subtract(bf, cf);
        let base = ar.det2(u, w, u2, w2);
        if ar.is_zero(base) {
            return None;
        }
        let pw2 = ar.multiply(pw, pw);
        let denominator = ar.multiply(base, pw2);
        let first = oriented(ar, [bf, bs, cf, cs, pf, ps], pw);
        let second = oriented(ar, [cf, cs, af, as_, pf, ps], pw);
        let third = oriented(ar, [af, as_, bf, bs, pf, ps], pw);
        Some((
            ar.approximate_ratio(first, denominator),
            ar.approximate_ratio(second, denominator),
            ar.approximate_ratio(third, denominator),
        ))
    }

    /// `v` is `[ux, us, vx, vs, pf, ps]`.
    fn oriented(ar: &mut Arena, v: [V; 6], pw: V) -> V {
        let [ux, us, vx, vs, pf, ps] = v;
        let a = ar.scaled_minus(ux, pw, pf);
        let b = ar.scaled_minus(vs, pw, ps);
        let c = ar.scaled_minus(us, pw, ps);
        let d = ar.scaled_minus(vx, pw, pf);
        ar.det2(a, b, c, d)
    }

    #[allow(clippy::too_many_arguments)]
    pub fn orient2d_homogeneous(
        ax: &T,
        ay: &T,
        aw: &T,
        bx: &T,
        by: &T,
        bw: &T,
        cx: &T,
        cy: &T,
        cw: &T,
    ) -> i64 {
        let mut ar = acquire();
        let ax = ar.of_exact(ax);
        let ay = ar.of_exact(ay);
        let aw = ar.of_exact(aw);
        let bx = ar.of_exact(bx);
        let by = ar.of_exact(by);
        let bw = ar.of_exact(bw);
        let cx = ar.of_exact(cx);
        let cy = ar.of_exact(cy);
        let cw = ar.of_exact(cw);
        let acx = ar.homogeneous_difference(ax, aw, cx, cw);
        let acy = ar.homogeneous_difference(ay, aw, cy, cw);
        let bcx = ar.homogeneous_difference(bx, bw, cx, cw);
        let bcy = ar.homogeneous_difference(by, bw, cy, cw);
        let determinant = ar.det2(acx, bcy, acy, bcx);
        let result = ar.compare_zero(determinant);
        release(ar);
        result
    }

    #[allow(clippy::too_many_arguments)]
    pub fn orient3d_homogeneous(
        ax: &T,
        ay: &T,
        az: &T,
        aw: &T,
        bx: &T,
        by: &T,
        bz: &T,
        bw: &T,
        cx: &T,
        cy: &T,
        cz: &T,
        cw: &T,
        dx: &T,
        dy: &T,
        dz: &T,
        dw: &T,
    ) -> i64 {
        let mut ar = acquire();
        let ax = ar.of_exact(ax);
        let ay = ar.of_exact(ay);
        let az = ar.of_exact(az);
        let aw = ar.of_exact(aw);
        let bx = ar.of_exact(bx);
        let by = ar.of_exact(by);
        let bz = ar.of_exact(bz);
        let bw = ar.of_exact(bw);
        let cx = ar.of_exact(cx);
        let cy = ar.of_exact(cy);
        let cz = ar.of_exact(cz);
        let cw = ar.of_exact(cw);
        let dx = ar.of_exact(dx);
        let dy = ar.of_exact(dy);
        let dz = ar.of_exact(dz);
        let dw = ar.of_exact(dw);
        let adx = ar.homogeneous_difference(ax, aw, dx, dw);
        let ady = ar.homogeneous_difference(ay, aw, dy, dw);
        let adz = ar.homogeneous_difference(az, aw, dz, dw);
        let bdx = ar.homogeneous_difference(bx, bw, dx, dw);
        let bdy = ar.homogeneous_difference(by, bw, dy, dw);
        let bdz = ar.homogeneous_difference(bz, bw, dz, dw);
        let cdx = ar.homogeneous_difference(cx, cw, dx, dw);
        let cdy = ar.homogeneous_difference(cy, cw, dy, dw);
        let cdz = ar.homogeneous_difference(cz, cw, dz, dw);
        let first = ar.times_det2(adx, bdy, cdz, bdz, cdy);
        let second = ar.times_det2(ady, bdz, cdx, bdx, cdz);
        let third = ar.times_det2(adz, bdx, cdy, bdy, cdx);
        let total = ar.sum3(first, second, third);
        let result = ar.compare_zero(total);
        release(ar);
        result
    }

    #[allow(clippy::too_many_arguments)]
    pub fn incircle_homogeneous(
        ax: &T,
        ay: &T,
        aw: &T,
        bx: &T,
        by: &T,
        bw: &T,
        cx: &T,
        cy: &T,
        cw: &T,
        dx: &T,
        dy: &T,
        dw: &T,
    ) -> i64 {
        let mut ar = acquire();
        let ax = ar.of_exact(ax);
        let ay = ar.of_exact(ay);
        let aw = ar.of_exact(aw);
        let bx = ar.of_exact(bx);
        let by = ar.of_exact(by);
        let bw = ar.of_exact(bw);
        let cx = ar.of_exact(cx);
        let cy = ar.of_exact(cy);
        let cw = ar.of_exact(cw);
        let dx = ar.of_exact(dx);
        let dy = ar.of_exact(dy);
        let dw = ar.of_exact(dw);
        let adx = ar.homogeneous_difference(ax, aw, dx, dw);
        let ady = ar.homogeneous_difference(ay, aw, dy, dw);
        let bdx = ar.homogeneous_difference(bx, bw, dx, dw);
        let bdy = ar.homogeneous_difference(by, bw, dy, dw);
        let cdx = ar.homogeneous_difference(cx, cw, dx, dw);
        let cdy = ar.homogeneous_difference(cy, cw, dy, dw);
        let adenominator = ar.multiply(aw, dw);
        let bdenominator = ar.multiply(bw, dw);
        let cdenominator = ar.multiply(cw, dw);
        let row_ax = ar.multiply(adx, adenominator);
        let row_ay = ar.multiply(ady, adenominator);
        let row_bx = ar.multiply(bdx, bdenominator);
        let row_by = ar.multiply(bdy, bdenominator);
        let row_cx = ar.multiply(cdx, cdenominator);
        let row_cy = ar.multiply(cdy, cdenominator);
        let row_alift = ar.dot2(adx, adx, ady, ady);
        let row_blift = ar.dot2(bdx, bdx, bdy, bdy);
        let row_clift = ar.dot2(cdx, cdx, cdy, cdy);
        let first = ar.times_det2(row_ax, row_by, row_clift, row_blift, row_cy);
        let second = ar.times_det2(row_ay, row_blift, row_cx, row_bx, row_clift);
        let third = ar.times_det2(row_alift, row_bx, row_cy, row_by, row_cx);
        let total = ar.sum3(first, second, third);
        let result = ar.compare_zero(total);
        release(ar);
        result
    }

    /// Exact sign of the triangle normal dotted with the positive-infinitesimal direction
    /// `(1, epsilon, epsilon^2)`.
    #[allow(clippy::too_many_arguments)]
    pub fn normal_dot_symbolic_homogeneous(
        fx: &T,
        fy: &T,
        fz: &T,
        fw: &T,
        sx: &T,
        sy: &T,
        sz: &T,
        sw: &T,
        tx: &T,
        ty: &T,
        tz: &T,
        tw: &T,
    ) -> i64 {
        let mut ar = acquire();
        let fx = ar.of_exact(fx);
        let fy = ar.of_exact(fy);
        let fz = ar.of_exact(fz);
        let fw = ar.of_exact(fw);
        let sx = ar.of_exact(sx);
        let sy = ar.of_exact(sy);
        let sz = ar.of_exact(sz);
        let sw = ar.of_exact(sw);
        let tx = ar.of_exact(tx);
        let ty = ar.of_exact(ty);
        let tz = ar.of_exact(tz);
        let tw = ar.of_exact(tw);
        let second_x = ar.homogeneous_difference(sx, sw, fx, fw);
        let second_y = ar.homogeneous_difference(sy, sw, fy, fw);
        let second_z = ar.homogeneous_difference(sz, sw, fz, fw);
        let third_x = ar.homogeneous_difference(tx, tw, fx, fw);
        let third_y = ar.homogeneous_difference(ty, tw, fy, fw);
        let third_z = ar.homogeneous_difference(tz, tw, fz, fw);
        let cross_x = ar.det2(second_y, third_z, second_z, third_y);
        let cross_y = ar.det2(second_z, third_x, second_x, third_z);
        let cross_z = ar.det2(second_x, third_y, second_y, third_x);
        let result = first_nonzero(&ar, cross_x, cross_y, cross_z);
        release(ar);
        result
    }

    fn first_nonzero(ar: &Arena, x: V, y: V, z: V) -> i64 {
        let first = ar.compare_zero(x);
        if first != 0 {
            first
        } else {
            let second = ar.compare_zero(y);
            if second != 0 { second } else { ar.compare_zero(z) }
        }
    }

    fn directed_axis_sign(ar: &Arena, axis: i64, positive: bool, x: V, y: V, z: V) -> i64 {
        let sign = ar.compare_zero(if axis == 0 {
            x
        } else if axis == 1 {
            y
        } else if axis == 2 {
            z
        } else {
            unreachable!("Rdk exact ray axis is out of bounds")
        });
        if positive { sign } else { -sign }
    }

    fn opposite(left: i64, right: i64) -> bool {
        left != 0 && right != 0 && left != right
    }

    /// Codes as `symbolic_ray_tri_homogeneous`, with the ray along `axis` (0..2) in the positive
    /// or negative direction.
    #[allow(clippy::too_many_arguments)]
    pub fn axis_ray_tri_homogeneous(
        axis: i64,
        positive: bool,
        ax: f64,
        ay: f64,
        az: f64,
        bx: f64,
        by: f64,
        bz: f64,
        cx: f64,
        cy: f64,
        cz: f64,
        qx: &T,
        qy: &T,
        qz: &T,
        qw: &T,
    ) -> i64 {
        let mut ar = acquire();
        let ax = ar.of_float(ax);
        let ay = ar.of_float(ay);
        let az = ar.of_float(az);
        let bx = ar.of_float(bx);
        let by = ar.of_float(by);
        let bz = ar.of_float(bz);
        let cx = ar.of_float(cx);
        let cy = ar.of_float(cy);
        let cz = ar.of_float(cz);
        let qx = ar.of_exact(qx);
        let qy = ar.of_exact(qy);
        let qz = ar.of_exact(qz);
        let qw = ar.of_exact(qw);
        let one = ar.of_float(1.);
        let bax = ar.subtract(bx, ax);
        let bay = ar.subtract(by, ay);
        let baz = ar.subtract(bz, az);
        let cax = ar.subtract(cx, ax);
        let cay = ar.subtract(cy, ay);
        let caz = ar.subtract(cz, az);
        let normal_x = ar.det2(bay, caz, baz, cay);
        let normal_y = ar.det2(baz, cax, bax, caz);
        let normal_z = ar.det2(bax, cay, bay, cax);
        let normal = directed_axis_sign(&ar, axis, positive, normal_x, normal_y, normal_z);
        let result = if normal == 0 {
            4
        } else {
            let aqx = ar.homogeneous_difference(ax, one, qx, qw);
            let aqy = ar.homogeneous_difference(ay, one, qy, qw);
            let aqz = ar.homogeneous_difference(az, one, qz, qw);
            let bqx = ar.homogeneous_difference(bx, one, qx, qw);
            let bqy = ar.homogeneous_difference(by, one, qy, qw);
            let bqz = ar.homogeneous_difference(bz, one, qz, qw);
            let cqx = ar.homogeneous_difference(cx, one, qx, qw);
            let cqy = ar.homogeneous_difference(cy, one, qy, qw);
            let cqz = ar.homogeneous_difference(cz, one, qz, qw);
            let edge_sign = |ar: &mut Arena, u: [V; 3], v: [V; 3]| -> i64 {
                let cross_x = ar.det2(u[1], v[2], u[2], v[1]);
                let cross_y = ar.det2(u[2], v[0], u[0], v[2]);
                let cross_z = ar.det2(u[0], v[1], u[1], v[0]);
                directed_axis_sign(ar, axis, positive, cross_x, cross_y, cross_z)
            };
            let first = edge_sign(&mut ar, [aqx, aqy, aqz], [bqx, bqy, bqz]);
            let second = edge_sign(&mut ar, [bqx, bqy, bqz], [cqx, cqy, cqz]);
            let third = edge_sign(&mut ar, [cqx, cqy, cqz], [aqx, aqy, aqz]);
            let plane_value = ar.dot3(normal_x, normal_y, normal_z, aqx, aqy, aqz);
            let plane = ar.compare_zero(plane_value);
            if opposite(first, normal) || opposite(second, normal) || opposite(third, normal) {
                0
            } else if plane == 0 {
                3
            } else if plane != normal {
                0
            } else if first == 0 || second == 0 || third == 0 {
                3
            } else if normal < 0 {
                1
            } else {
                2
            }
        };
        release(ar);
        result
    }

    /// Codes: 0 miss, 1 negative-oriented hit, 2 positive-oriented hit,
    /// 3 boundary-degenerate ray, 4 parallel carrier.
    #[allow(clippy::too_many_arguments)]
    pub fn symbolic_ray_tri_homogeneous(
        ax: f64,
        ay: f64,
        az: f64,
        bx: f64,
        by: f64,
        bz: f64,
        cx: f64,
        cy: f64,
        cz: f64,
        qx: &T,
        qy: &T,
        qz: &T,
        qw: &T,
    ) -> i64 {
        let mut ar = acquire();
        let ax = ar.of_float(ax);
        let ay = ar.of_float(ay);
        let az = ar.of_float(az);
        let bx = ar.of_float(bx);
        let by = ar.of_float(by);
        let bz = ar.of_float(bz);
        let cx = ar.of_float(cx);
        let cy = ar.of_float(cy);
        let cz = ar.of_float(cz);
        let qx = ar.of_exact(qx);
        let qy = ar.of_exact(qy);
        let qz = ar.of_exact(qz);
        let qw = ar.of_exact(qw);
        let one = ar.of_float(1.);
        let bax = ar.subtract(bx, ax);
        let bay = ar.subtract(by, ay);
        let baz = ar.subtract(bz, az);
        let cax = ar.subtract(cx, ax);
        let cay = ar.subtract(cy, ay);
        let caz = ar.subtract(cz, az);
        let normal_x = ar.det2(bay, caz, baz, cay);
        let normal_y = ar.det2(baz, cax, bax, caz);
        let normal_z = ar.det2(bax, cay, bay, cax);
        let normal = first_nonzero(&ar, normal_x, normal_y, normal_z);
        let result = if normal == 0 {
            4
        } else {
            let aqx = ar.homogeneous_difference(ax, one, qx, qw);
            let aqy = ar.homogeneous_difference(ay, one, qy, qw);
            let aqz = ar.homogeneous_difference(az, one, qz, qw);
            let bqx = ar.homogeneous_difference(bx, one, qx, qw);
            let bqy = ar.homogeneous_difference(by, one, qy, qw);
            let bqz = ar.homogeneous_difference(bz, one, qz, qw);
            let cqx = ar.homogeneous_difference(cx, one, qx, qw);
            let cqy = ar.homogeneous_difference(cy, one, qy, qw);
            let cqz = ar.homogeneous_difference(cz, one, qz, qw);
            let edge_sign = |ar: &mut Arena, u: [V; 3], v: [V; 3]| -> i64 {
                let cross_x = ar.det2(u[1], v[2], u[2], v[1]);
                let cross_y = ar.det2(u[2], v[0], u[0], v[2]);
                let cross_z = ar.det2(u[0], v[1], u[1], v[0]);
                first_nonzero(ar, cross_x, cross_y, cross_z)
            };
            let first = edge_sign(&mut ar, [aqx, aqy, aqz], [bqx, bqy, bqz]);
            let second = edge_sign(&mut ar, [bqx, bqy, bqz], [cqx, cqy, cqz]);
            let third = edge_sign(&mut ar, [cqx, cqy, cqz], [aqx, aqy, aqz]);
            let plane_value = ar.dot3(normal_x, normal_y, normal_z, aqx, aqy, aqz);
            let plane = ar.compare_zero(plane_value);
            if opposite(first, normal) || opposite(second, normal) || opposite(third, normal) {
                0
            } else if plane == 0 || first == 0 || second == 0 || third == 0 {
                3
            } else if plane == normal {
                if normal < 0 { 1 } else { 2 }
            } else {
                0
            }
        };
        release(ar);
        result
    }

    /// Hit codes: 0 miss, 1 negative orientation, 2 positive orientation, 3 query on closed triangle, 4 degenerate triangle.
    #[allow(clippy::too_many_arguments)]
    pub fn radial_dot_homogeneous(
        ex0: &T,
        ey0: &T,
        ez0: &T,
        ew0: &T,
        ex1: &T,
        ey1: &T,
        ez1: &T,
        ew1: &T,
        lx: &T,
        ly: &T,
        lz: &T,
        lw: &T,
        rx: &T,
        ry: &T,
        rz: &T,
        rw: &T,
    ) -> i64 {
        let mut ar = acquire();
        let ex0 = ar.of_exact(ex0);
        let ey0 = ar.of_exact(ey0);
        let ez0 = ar.of_exact(ez0);
        let ew0 = ar.of_exact(ew0);
        let ex1 = ar.of_exact(ex1);
        let ey1 = ar.of_exact(ey1);
        let ez1 = ar.of_exact(ez1);
        let ew1 = ar.of_exact(ew1);
        let lx = ar.of_exact(lx);
        let ly = ar.of_exact(ly);
        let lz = ar.of_exact(lz);
        let lw = ar.of_exact(lw);
        let rx = ar.of_exact(rx);
        let ry = ar.of_exact(ry);
        let rz = ar.of_exact(rz);
        let rw = ar.of_exact(rw);
        let edge_x = ar.homogeneous_difference(ex1, ew1, ex0, ew0);
        let edge_y = ar.homogeneous_difference(ey1, ew1, ey0, ew0);
        let edge_z = ar.homogeneous_difference(ez1, ew1, ez0, ew0);
        let left_x = ar.homogeneous_difference(lx, lw, ex0, ew0);
        let left_y = ar.homogeneous_difference(ly, lw, ey0, ew0);
        let left_z = ar.homogeneous_difference(lz, lw, ez0, ew0);
        let right_x = ar.homogeneous_difference(rx, rw, ex0, ew0);
        let right_y = ar.homogeneous_difference(ry, rw, ey0, ew0);
        let right_z = ar.homogeneous_difference(rz, rw, ez0, ew0);
        let lr = ar.dot3(left_x, left_y, left_z, right_x, right_y, right_z);
        let ee = ar.dot3(edge_x, edge_y, edge_z, edge_x, edge_y, edge_z);
        let le = ar.dot3(left_x, left_y, left_z, edge_x, edge_y, edge_z);
        let re = ar.dot3(right_x, right_y, right_z, edge_x, edge_y, edge_z);
        let difference = ar.det2(lr, ee, le, re);
        let result = ar.compare_zero(difference);
        release(ar);
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exact_64_bit_carries_borrows_products_and_intervals() {
        let integer = |value: i128| T {
            negative: value < 0,
            magnitude: if value == 0 { Vec::new() } else { vec![value.unsigned_abs() as u64] },
            exponent: 0,
        };
        let value = |number: &T| {
            assert_eq!(number.exponent, 0);
            let magnitude = number.magnitude.iter().rev().fold(0u128, |acc, limb| (acc << 64) | *limb as u128);
            if number.negative { -(magnitude as i128) } else { magnitude as i128 }
        };
        for index in 0..2_000u64 {
            let left = index.wrapping_mul(6_364_136_223_846_793_005).wrapping_add(1) as i64 as i128;
            let right = index.wrapping_mul(1_442_695_040_888_963_407).wrapping_add(97) as i64 as i128;
            let (a, b) = (integer(left), integer(right));
            assert_eq!(value(&add(&a, &b)), left + right);
            assert_eq!(value(&subtract(&a, &b)), left - right);
            assert_eq!(value(&multiply(&a, &b)), left * right);
        }
        let maximum = T { negative: false, magnitude: vec![u64::MAX], exponent: 0 };
        assert_eq!(add(&maximum, &maximum).magnitude, [u64::MAX - 1, 1]);
        let wide = T { negative: false, magnitude: vec![u64::MAX, u64::MAX], exponent: 0 };
        assert_eq!(multiply(&wide, &wide).magnitude, [1, 0, u64::MAX - 1, u64::MAX]);
        let power = T { negative: false, magnitude: vec![0, 0, 1], exponent: 0 };
        assert_eq!(subtract(&power, &integer(1)).magnitude, wide.magnitude);
        assert!(is_zero(&subtract(&power, &power)));
        for index in 0..256u64 {
            let coordinate = |salt: u64| {
                let bits = index.wrapping_mul(6_364_136_223_846_793_005).wrapping_add(salt);
                of_float(f64::from_bits((bits & 0x800f_ffff_ffff_ffff) | ((bits % 2047) << 52)))
            };
            let exact = add(&multiply(&coordinate(97), &coordinate(65_537)), &coordinate(1_000_003));
            let (lower, upper, exponent) = to_scaled_interval(&exact);
            let mut lower = of_float(lower);
            let mut upper = of_float(upper);
            lower.exponent += exponent;
            upper.exponent += exponent;
            assert!(compare_zero(&subtract(&lower, &exact)) <= 0, "certified lower bound");
            assert!(compare_zero(&subtract(&upper, &exact)) >= 0, "certified upper bound");
        }
        for exponent in [-1074, -1022, -53, 0, 53, 700, 1023] {
            for significand in [1., -1., 1.5, -1.5] {
                let number = ldexp(significand, exponent);
                if !number.is_finite() { continue; }
                let exact = of_float(number);
                let (lower, upper, scale) = to_scaled_interval(&exact);
                assert!(ldexp(lower, scale) <= number && number <= ldexp(upper, scale));
            }
        }
    }
}
