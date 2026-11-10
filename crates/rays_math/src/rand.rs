use crate::hash::{avalanche, GAMMA};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct T(u64);

pub fn seed(value: i64) -> T { T(value as u64) }

fn next(state: T) -> (u64, T) {
    let state = state.0.wrapping_add(GAMMA);
    (avalanche(state), T(state))
}
pub fn float(state: T) -> (f64, T) {
    let (value, state) = next(state);
    ((value >> 11) as f64 * (1.0 / 9_007_199_254_740_992.0), state)
}

/// Random access to the same SplitMix64 stream; the index uses all 64 two's-complement bits.
pub fn float_at_int(seed: i64, index: i64) -> f64 {
    let state = (seed as u64).wrapping_add(GAMMA.wrapping_mul((index as u64).wrapping_add(1)));
    (avalanche(state) >> 11) as f64 * (1.0 / 9_007_199_254_740_992.0)
}

pub fn float_at(state: T, index: i64) -> f64 { float_at_int(state.0 as i64, index) }

fn int_unchecked(bound: i64, state: T) -> (i64, T) {
    let (value, state) = next(state);
    ((value % bound as u64) as i64, state)
}

pub fn int(bound: i64, state: T) -> Result<(i64, T), String> {
    if bound <= 0 {
        Err("Rand.int: bound must be positive".to_owned())
    } else {
        Ok(int_unchecked(bound, state))
    }
}

pub fn shuffle(mut values: Vec<i64>, state: T) -> (Vec<i64>, T) {
    let mut state = state;
    let length = i64::try_from(values.len()).expect("Rand.shuffle: list too long");
    for index in (1..length).rev() {
        let (other, next_state) = int_unchecked(index + 1, state);
        state = next_state;
        values.swap(index as usize, other as usize);
    }
    (values, state)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn splitmix64_vectors_and_indexed_stream_agree() {
        let mut state = seed(0);
        let (value, after) = next(state); assert_eq!(value, 0xe220a8397b1dcdaf);
        assert_eq!(float_at(seed(0), 0).to_bits(), 0x3fec4415072f63b9); state = after;
        let (value, after) = next(state); assert_eq!(value, 0x6e789e6aa1b965f4);
        assert_eq!(float_at(seed(0), 1).to_bits(), 0x3fdb9e279aa86e58); state = after;
        let (value, after) = next(state); assert_eq!(value, 0x06c45d188009454f);
        assert_eq!(float_at(seed(0), 2).to_bits(), 0x3f9b117462002500); state = after;
        let (value, after) = next(state); assert_eq!(value, 0xf88bb8a8724c81ec);
        assert_eq!(float_at(seed(0), 3).to_bits(), 0x3fef1177150e4990); state = after;
        let (value, after) = next(state); assert_eq!(value, 0x1b39896a51a8749b);
        assert_eq!(float_at(seed(0), 4).to_bits(), 0x3fbb39896a51a870); state = after;
        let (value, after) = next(state); assert_eq!(value, 0x53cb9f0c747ea2ea);
        assert_eq!(float_at(seed(0), 5).to_bits(), 0x3fd4f2e7c31d1fa8); state = after;
        let (value, after) = next(state); assert_eq!(value, 0x2c829abe1f4532e1);
        assert_eq!(float_at(seed(0), 6).to_bits(), 0x3fc6414d5f0fa298); state = after;
        let (value, _) = next(state); assert_eq!(value, 0xc584133ac916ab3c);
        assert_eq!(float_at(seed(0), 7).to_bits(), 0x3fe8b082675922d5);
        assert_eq!(float_at_int(0, 0).to_bits(), 0x3fec4415072f63b9);
        assert_eq!(float_at_int(1, 0).to_bits(), 0x3fe22145bd91204b);
        assert_eq!(float_at_int(-1, 0).to_bits(), 0x3fec9b2e2ee36ca5);
        assert_eq!(float_at_int(4611686018427387904, 0).to_bits(), 0x3f654a1d51c1f500);
        assert_eq!(float_at_int(-4611686018427387904, 0).to_bits(), 0x3fa2b21e4fe2a180);
        assert_eq!(float_at_int(9223372036854775807, 0).to_bits(), 0x3fc533ebaa9701cc);
        assert_eq!(float_at_int(i64::MIN, 0).to_bits(), 0x3fd207b02884aa7c);
        assert_eq!(float_at_int(0, -1).to_bits(), 0x0000000000000000);
        assert_eq!(float_at_int(0, 9223372036854775807).to_bits(), 0x3fc2e13752bce754);
        assert_eq!(float_at_int(0, i64::MIN).to_bits(), 0x3fd207b02884aa7c);
        assert_eq!(float_at_int(-1, -1).to_bits(), 0x3fe69a0abf9e5977);
        assert_eq!(float_at_int(i64::MIN, 9223372036854775807).to_bits(), 0x0000000000000000);
        assert_eq!(float_at_int(9223372036854775807, i64::MIN).to_bits(), 0x3fec9b2e2ee36ca5);
        let mut stream = seed(i64::MIN);
        for index in 0..32 { let (v, after) = float(stream); assert_eq!(v.to_bits(), float_at(seed(i64::MIN), index).to_bits()); stream = after; }
        assert_ne!(float_at(seed(0), 0), float_at(seed(i64::MIN), 0));
    }
}
