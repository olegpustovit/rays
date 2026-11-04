use ocaml_int::wrap_int63;

pub fn mix_with(multiplier: i64, hash: i64, value: i64) -> i64 {
    wrap_int63(wrap_int63(hash).wrapping_mul(wrap_int63(multiplier)) ^ wrap_int63(value))
}

pub fn mix(hash: i64, value: i64) -> i64 { mix_with(65599, hash, value) }

/// OCaml `hash lxor value * 0x100000001b3`: `lxor` and `*` share a precedence level and associate
/// left, so this is `(hash lxor value) * prime`, the FNV-1a step.
pub fn fnv_int_mix(hash: i64, value: i64) -> i64 {
    wrap_int63(wrap_int63(hash ^ value).wrapping_mul(0x100000001b3))
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

#[cfg(test)]
mod tests {
    use super::{fnv_int_mix, fnv_int_range};

    // Expected values are OCaml 5.3 `lib/rays_math/hash.ml` run as a toplevel script.
    #[test]
    fn fnv_int_mix_matches_ocaml_precedence_and_63_bit_wrap() {
        assert_eq!(fnv_int_mix(0, 1), 1_099_511_628_211);
        assert_eq!(fnv_int_mix(1, 0), 1_099_511_628_211);
        assert_eq!(fnv_int_mix(i64::MAX, 5), 4_611_679_421_357_618_638);
        assert_eq!(fnv_int_mix(i64::MIN, -3), 4_611_682_719_892_503_271);
        assert_eq!(fnv_int_mix(-1, 200), -221_001_837_270_411);
        assert_eq!(fnv_int_mix(0x2bf29ce484222325, 0x7fffffff), 800_922_855_557_449_326);
    }

    #[test]
    fn fnv_int_range_matches_ocaml_and_is_order_sensitive() {
        assert_eq!(fnv_int_range(0, "51", 0, 2), 50_750_158_213_306_202);
        assert_eq!(fnv_int_range(0, "15", 0, 2), 46_928_255_794_401_666);
        assert_ne!(fnv_int_range(0, "51", 0, 2), fnv_int_range(0, "15", 0, 2));
        assert_eq!(fnv_int_range(-7, "rays", 0, 4), -1_326_547_373_507_272_024);
        assert_eq!(fnv_int_range(0x2bf29ce484222325, "hello world", 0, 11), 1_700_783_852_135_174_887);
        assert_eq!(fnv_int_range(12345, "abcdef", 2, 5), -3_363_903_129_284_365_343);
        assert_eq!(fnv_int_range(0, "AZ\u{1}~", 0, 4), 2_135_968_585_004_218_080);
        assert_eq!(fnv_int_range(99, "abc", 2, 2), 99);
    }
}
