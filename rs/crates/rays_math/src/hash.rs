use ocaml_int::wrap_int63;

pub fn mix_with(multiplier: i64, hash: i64, value: i64) -> i64 {
    wrap_int63(wrap_int63(hash).wrapping_mul(wrap_int63(multiplier)) ^ wrap_int63(value))
}

pub fn mix(hash: i64, value: i64) -> i64 { mix_with(65599, hash, value) }

pub fn fnv_int_mix(hash: i64, value: i64) -> i64 {
    wrap_int63(wrap_int63(hash) ^ wrap_int63(value).wrapping_mul(0x100000001b3))
}

pub fn fnv_int_range(seed: i64, text: &str, first: i64, last: i64) -> i64 {
    let mut hash = wrap_int63(seed);
    if first >= last { return hash; }
    let first = usize::try_from(first).expect("Hash.fnv_int_range: invalid range");
    let last = usize::try_from(last).expect("Hash.fnv_int_range: invalid range");
    for &byte in &text.as_bytes()[first..last] {
        hash = fnv_int_mix(hash, i64::from(byte));
    }
    hash
}

pub fn fnv64_mix(hash: i64, value: i64) -> i64 {
    ((hash as u64 ^ value as u64).wrapping_mul(0x100000001b3)) as i64
}

fn to_uint32(value: f64) -> u32 {
    if !value.is_finite() { return 0; }
    let value = value.trunc() % 4_294_967_296.0;
    (if value < 0.0 { value + 4_294_967_296.0 } else { value }) as u32
}

fn imul(a: u32, b: u32) -> u32 { a.wrapping_mul(b) }

pub fn float_sequence32(values: &[f64]) -> i64 {
    let mut hash = 0x9e3779b9_u32;
    for &value in values {
        let key = to_uint32((value * 1_000_003.0).floor());
        hash = imul(hash ^ key, 0x85ebca6b);
        hash ^= hash >> 13;
        hash = imul(hash, 0xc2b2ae35);
        hash ^= hash >> 16;
    }
    i64::from(hash)
}

pub fn string(text: &str) -> i64 {
    text.as_bytes().iter().fold(0xcbf29ce484222325_u64, |hash, byte| {
        (hash ^ u64::from(*byte)).wrapping_mul(0x100000001b3)
    }) as i64
}
