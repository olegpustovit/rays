/// Wraps an `i64` to the former signed 63-bit range. The named hash functions (`rand/at`, `Rand.float_at`,
/// the geometry hashes) keep this wrap so their bits never change; Lisp `Int` itself is a plain `i64`.
#[inline]
pub fn wrap_int63(value: i64) -> i64 {
    let low = (value as u64) & ((1_u64 << 63) - 1);
    ((low << 1) as i64) >> 1
}

/// Logical right shift of a signed 63-bit integer; the shift count is masked to its low six bits.
#[inline]
pub fn lsr(value: i64, amount: i64) -> i64 {
    let bits = (wrap_int63(value) as u64) & ((1_u64 << 63) - 1);
    wrap_int63((bits >> ((amount as u64 & 63) as u32)) as i64)
}

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

/// MD5 (RFC 1321), lowercase hex. Matches OCaml `Digest.to_hex (Digest.string s)`.
pub fn md5_hex(input: &[u8]) -> String {
    const S: [u32; 64] = [7,12,17,22,7,12,17,22,7,12,17,22,7,12,17,22,5,9,14,20,5,9,14,20,5,9,14,20,5,9,14,20,4,11,16,23,4,11,16,23,4,11,16,23,4,11,16,23,6,10,15,21,6,10,15,21,6,10,15,21,6,10,15,21];
    const K: [u32; 64] = [0xd76aa478,0xe8c7b756,0x242070db,0xc1bdceee,0xf57c0faf,0x4787c62a,0xa8304613,0xfd469501,0x698098d8,0x8b44f7af,0xffff5bb1,0x895cd7be,0x6b901122,0xfd987193,0xa679438e,0x49b40821,0xf61e2562,0xc040b340,0x265e5a51,0xe9b6c7aa,0xd62f105d,0x02441453,0xd8a1e681,0xe7d3fbc8,0x21e1cde6,0xc33707d6,0xf4d50d87,0x455a14ed,0xa9e3e905,0xfcefa3f8,0x676f02d9,0x8d2a4c8a,0xfffa3942,0x8771f681,0x6d9d6122,0xfde5380c,0xa4beea44,0x4bdecfa9,0xf6bb4b60,0xbebfbc70,0x289b7ec6,0xeaa127fa,0xd4ef3085,0x04881d05,0xd9d4d039,0xe6db99e5,0x1fa27cf8,0xc4ac5665,0xf4292244,0x432aff97,0xab9423a7,0xfc93a039,0x655b59c3,0x8f0ccc92,0xffeff47d,0x85845dd1,0x6fa87e4f,0xfe2ce6e0,0xa3014314,0x4e0811a1,0xf7537e82,0xbd3af235,0x2ad7d2bb,0xeb86d391];
    let mut data = input.to_vec();
    let bit_len = (data.len() as u64).wrapping_mul(8);
    data.push(0x80);
    while data.len() % 64 != 56 {
        data.push(0);
    }
    data.extend_from_slice(&bit_len.to_le_bytes());
    let (mut a0, mut b0, mut c0, mut d0) = (0x67452301u32, 0xefcdab89u32, 0x98badcfeu32, 0x10325476u32);
    for chunk in data.chunks_exact(64) {
        let mut words = [0u32; 16];
        for (index, bytes) in chunk.chunks_exact(4).enumerate() {
            words[index] = u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
        }
        let (mut a, mut b, mut c, mut d) = (a0, b0, c0, d0);
        for i in 0..64 {
            let (f, g) = if i < 16 {
                ((b & c) | (!b & d), i)
            } else if i < 32 {
                ((d & b) | (!d & c), (5 * i + 1) % 16)
            } else if i < 48 {
                (b ^ c ^ d, (3 * i + 5) % 16)
            } else {
                (c ^ (b | !d), (7 * i) % 16)
            };
            let next = b.wrapping_add(
                a.wrapping_add(f)
                    .wrapping_add(K[i])
                    .wrapping_add(words[g])
                    .rotate_left(S[i]),
            );
            (a, b, c, d) = (d, next, b, c);
        }
        a0 = a0.wrapping_add(a);
        b0 = b0.wrapping_add(b);
        c0 = c0.wrapping_add(c);
        d0 = d0.wrapping_add(d);
    }
    let mut output = String::with_capacity(32);
    const HEX: &[u8; 16] = b"0123456789abcdef";
    for word in [a0, b0, c0, d0] {
        for byte in word.to_le_bytes() {
            output.push(HEX[(byte >> 4) as usize] as char);
            output.push(HEX[(byte & 0xf) as usize] as char);
        }
    }
    output
}

#[cfg(test)]
mod tests {
    use super::{fnv_int_mix, fnv_int_range, lsr, md5_hex, wrap_int63};

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
        let cases = [(0, -1), (1, (1_i64 << 62) - 1), (2, (1_i64 << 61) - 1), (62, 1), (63, 0), (64, -1), (65, (1_i64 << 62) - 1), (-1, 0), (-2, 1)];
        for (amount, expected) in cases {
            assert_eq!(lsr(-1, amount), expected, "-1 lsr {amount}");
        }
        assert_eq!(lsr(-(1_i64 << 62), 1), 1_i64 << 61);
        assert_eq!(lsr(i64::MAX, 0), -1);
    }

    // Expected values are OCaml 5.3 `lib/rays_math/hash.ml` run as a toplevel script.
    // OCaml max_int/min_int are 63-bit: 2^62-1 and -2^62 (not i64::MAX/MIN, which wrap to -1/0).
    #[test]
    fn fnv_int_mix_matches_ocaml_precedence_and_63_bit_wrap() {
        assert_eq!(fnv_int_mix(0, 1), 1_099_511_628_211);
        assert_eq!(fnv_int_mix(1, 0), 1_099_511_628_211);
        assert_eq!(fnv_int_mix((1_i64 << 62) - 1, 5), 4_611_679_421_357_618_638);
        assert_eq!(fnv_int_mix(-(1_i64 << 62), -3), 4_611_682_719_892_503_271);
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

    #[test]
    fn md5_known_answers() {
        assert_eq!(md5_hex(b""), "d41d8cd98f00b204e9800998ecf8427e");
        assert_eq!(md5_hex(b"abc"), "900150983cd24fb0d6963f7d28e17f72");
    }
}
