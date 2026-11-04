use ocaml_int::{lsr, wrap_int63};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct T(i64);

pub fn seed(value: i64) -> T { T(wrap_int63(value)) }
pub fn seed64(value: i64) -> T { T(value) }

fn next(state: T) -> (i64, T) {
    let state = state.0.wrapping_add(0x9E3779B97F4A7C15_u64 as i64);
    let mut value = state ^ ((state as u64 >> 30) as i64);
    value = value.wrapping_mul(0xBF58476D1CE4E5B9_u64 as i64);
    value ^= (value as u64 >> 27) as i64;
    value = value.wrapping_mul(0x94D049BB133111EB_u64 as i64);
    (value ^ ((value as u64 >> 31) as i64), T(state))
}
pub fn float(state: T) -> (f64, T) {
    let (value, state) = next(state);
    let mantissa = (value as u64) >> 11;
    (mantissa as f64 / 9_007_199_254_740_992.0, state)
}

fn mix_index(value: i64) -> i64 {
    let mut value = wrap_int63(value) ^ lsr(value, 30);
    value = wrap_int63(value.wrapping_mul(0x3f58476d1ce4e5b9));
    value ^= lsr(value, 27);
    value = wrap_int63(value.wrapping_mul(0x14d049bb133111eb));
    wrap_int63(value ^ lsr(value, 31))
}

pub fn float_at_int(seed: i64, index: i64) -> f64 {
    let index = wrap_int63(wrap_int63(index).wrapping_add(0x11b54a32d192ed03));
    let keyed = wrap_int63(wrap_int63(seed) ^ wrap_int63(index.wrapping_mul(0x1e3779b97f4a7c15)));
    let mantissa = lsr(mix_index(keyed), 10);
    mantissa as f64 * (1.0 / 9_007_199_254_740_992.0)
}

pub fn float_at(state: T, index: i64) -> f64 {
    float_at_int(wrap_int63(state.0), index)
}

fn int_unchecked(bound: i64, state: T) -> (i64, T) {
    let (value, state) = next(state);
    (((value as u64) % bound as u64) as i64, state)
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
