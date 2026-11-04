use std::hash::{BuildHasher, Hasher};

const K: u64 = 0x517c_c1b7_2722_0a95;

#[derive(Clone, Copy, Default)]
pub struct Fx(u64);

impl Fx {
    #[inline]
    fn add(&mut self, value: u64) {
        self.0 = (self.0.rotate_left(5) ^ value).wrapping_mul(K);
    }
}

impl BuildHasher for Fx {
    type Hasher = Self;

    fn build_hasher(&self) -> Self::Hasher {
        Self::default()
    }
}

impl Hasher for Fx {
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, bytes: &[u8]) {
        let mut chunks = bytes.chunks_exact(8);
        for chunk in &mut chunks {
            self.add(u64::from_le_bytes(chunk.try_into().unwrap()));
        }
        let tail = chunks.remainder();
        if !tail.is_empty() {
            let mut last = [0; 8];
            last[..tail.len()].copy_from_slice(tail);
            self.add(u64::from_le_bytes(last));
        }
    }

    fn write_u8(&mut self, value: u8) {
        self.add(value.into());
    }

    fn write_u16(&mut self, value: u16) {
        self.add(value.into());
    }

    fn write_u32(&mut self, value: u32) {
        self.add(value.into());
    }

    fn write_u64(&mut self, value: u64) {
        self.add(value);
    }

    fn write_u128(&mut self, value: u128) {
        self.add(value as u64);
        self.add((value >> 64) as u64);
    }

    fn write_usize(&mut self, value: usize) {
        self.add(value as u64);
    }

    fn write_i8(&mut self, value: i8) {
        self.add(value as u64);
    }

    fn write_i16(&mut self, value: i16) {
        self.add(value as u64);
    }

    fn write_i32(&mut self, value: i32) {
        self.add(value as u64);
    }

    fn write_i64(&mut self, value: i64) {
        self.add(value as u64);
    }

    fn write_i128(&mut self, value: i128) {
        self.write_u128(value as u128);
    }

    fn write_isize(&mut self, value: isize) {
        self.add(value as u64);
    }
}

#[cfg(test)]
mod tests {
    use super::Fx;
    use std::hash::Hasher;

    #[test]
    fn hashes_bytes_with_fixed_little_endian_output() {
        let mut hasher = Fx::default();
        hasher.write(b"FxHasher!");
        assert_eq!(hasher.finish(), 0xe460_8f44_c070_ee6f);
    }
}
