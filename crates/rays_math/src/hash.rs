/// Shared deterministic, non-cryptographic hash. Bytes are length-framed little-endian words.
pub const SEED: u64 = 0x243f6a8885a308d3;
pub(crate) const GAMMA: u64 = 0x9e3779b97f4a7c15;
pub const SEED2: u64 = SEED ^ GAMMA;

/// SplitMix64 finalizer, shared by word hashing and the immutable generator.
#[inline]
pub(crate) fn avalanche(mut value: u64) -> u64 {
    value = (value ^ (value >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
    value = (value ^ (value >> 27)).wrapping_mul(0x94d049bb133111eb);
    value ^ (value >> 31)
}

#[inline]
pub fn word(hash: u64, value: u64) -> u64 { avalanche((hash ^ value).wrapping_add(GAMMA)) }

pub fn bytes(seed: u64, bytes: &[u8]) -> u64 {
    let mut hash = word(seed, bytes.len() as u64);
    let mut chunks = bytes.chunks_exact(8);
    for chunk in &mut chunks { hash = word(hash, u64::from_le_bytes(chunk.try_into().unwrap())); }
    if !chunks.remainder().is_empty() {
        let mut tail = [0; 8];
        tail[..chunks.remainder().len()].copy_from_slice(chunks.remainder());
        hash = word(hash, u64::from_le_bytes(tail));
    }
    hash
}

/// Two independently seeded lanes for existing 128-bit content identities.
pub fn digest(bytes: &[u8]) -> u128 {
    (u128::from(self::bytes(SEED, bytes)) << 64) | u128::from(self::bytes(SEED2, bytes))
}

#[inline]
pub fn mix(hash: i64, value: i64) -> i64 { word(hash as u64, value as u64) as i64 }
pub fn string(text: &str) -> i64 { bytes(SEED, text.as_bytes()) as i64 }

/// Numeric keys normalize signed zero and NaN; cache payloads hash their raw bits instead.
pub fn float_sequence(values: &[f64]) -> f64 {
    let mut hash = word(SEED, values.len() as u64);
    for &value in values {
        let bits = if value == 0.0 { 0 } else if value.is_nan() { 0x7ff8000000000000 } else { value.to_bits() };
        hash = word(hash, bits);
    }
    (hash >> 11) as f64 * (1.0 / 9_007_199_254_740_992.0)
}

/// MD5 (RFC 1321), lowercase hex, for serialized integrity checks and frozen artifact checksums.
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
    use super::*;
    #[test]
    fn deterministic_word_and_byte_vectors() {
        assert_eq!(word(0x0000000000000000, 0x0000000000000000), 0xe220a8397b1dcdaf);
        assert_eq!(word(0x0000000000000001, 0x0000000000000000), 0x910a2dec89025cc1);
        assert_eq!(word(0xffffffffffffffff, 0x0000000000000000), 0xe4d971771b652c20);
        assert_eq!(word(0x4000000000000000, 0x0000000000000000), 0x00aa50ea8e0fa9eb);
        assert_eq!(word(0xc000000000000000, 0x0000000000000000), 0x09590f27f150c2b1);
        assert_eq!(word(0x7fffffffffffffff, 0x0000000000000000), 0x2a67d7552e039ea7);
        assert_eq!(word(0x8000000000000000, 0x0000000000000000), 0x481ec0a212a9f3db);
        assert_eq!(word(0x0000000000000000, 0xffffffffffffffff), 0xe4d971771b652c20);
        assert_eq!(word(0x0000000000000000, 0x7fffffffffffffff), 0x2a67d7552e039ea7);
        assert_eq!(word(0x0000000000000000, 0x8000000000000000), 0x481ec0a212a9f3db);
        assert_eq!(word(0xffffffffffffffff, 0xffffffffffffffff), 0xe220a8397b1dcdaf);
        assert_eq!(word(0x8000000000000000, 0x7fffffffffffffff), 0xe4d971771b652c20);
        assert_eq!(word(0x7fffffffffffffff, 0x8000000000000000), 0xe4d971771b652c20);
        assert_eq!(bytes(0x243f6a8885a308d3, &[]), 0x2cb0f69f4abea221);
        assert_eq!(bytes(0x243f6a8885a308d3, &[0]), 0xd02b8570928d570f);
        assert_eq!(bytes(0x243f6a8885a308d3, &[0, 0, 0, 0, 0, 0, 0, 0]), 0x1b1474194bb1e2ab);
        assert_eq!(bytes(0x243f6a8885a308d3, &[0, 0, 0, 0, 0, 0, 0, 0, 0]), 0x42b0674064bbb47e);
        assert_eq!(bytes(0x243f6a8885a308d3, &[49, 50, 51, 52, 53, 54, 55]), 0xd40cd33c2d6af098);
        assert_eq!(bytes(0x243f6a8885a308d3, &[49, 50, 51, 52, 53, 54, 55, 56]), 0xa93a9484b24292d5);
        assert_eq!(bytes(0x243f6a8885a308d3, &[49, 50, 51, 52, 53, 54, 55, 56, 57]), 0x03056eca5e365037);
        assert_eq!(bytes(0x0000000000000000, &[114, 97, 121, 115]), 0x857698a7212d65d4);
        assert_eq!(bytes(0xffffffffffffffff, &[197, 190, 240, 159, 153, 130]), 0x3794a99e18641753);
        assert_eq!(bytes(0x243f6a8885a308d3, &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30]), 0x4d75af0cba23969b);
        assert_eq!(float_sequence(&[0.0]).to_bits(), float_sequence(&[-0.0]).to_bits());
        assert_eq!(float_sequence(&[f64::NAN]).to_bits(), float_sequence(&[f64::from_bits(0x7ff0000000000001)]).to_bits());
        assert_eq!(float_sequence(&[1.0, 2.0]).to_bits(), 0x3fdc00e7fb3c1606);
    }
    #[test]
    fn md5_known_answers() {
        assert_eq!(md5_hex(b""), "d41d8cd98f00b204e9800998ecf8427e");
        assert_eq!(md5_hex(b"abc"), "900150983cd24fb0d6963f7d28e17f72");
    }
}
