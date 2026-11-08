//! Rows of bytes as one offsets vector plus one byte buffer: N rows cost two allocations, not N + 1.
//! Attribute storage and Flow values share it (`Arc<TextArray>`), so reads and writes clone a pointer.
use std::ops::Index;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextArray {
    /// `len() + 1` monotone row boundaries into `bytes`, starting at zero.
    offsets: Vec<usize>,
    bytes: Vec<u8>,
}

impl Default for TextArray {
    fn default() -> Self { Self::new() }
}

impl TextArray {
    pub fn new() -> Self { Self { offsets: vec![0], bytes: Vec::new() } }

    pub fn with_capacity(rows: usize, bytes: usize) -> Self {
        let mut offsets = Vec::with_capacity(rows + 1);
        offsets.push(0);
        Self { offsets, bytes: Vec::with_capacity(bytes) }
    }

    /// Reserves for `rows` rows, reporting allocation failure instead of aborting.
    pub fn try_with_capacity(rows: usize) -> Result<Self, std::collections::TryReserveError> {
        let mut offsets = Vec::new();
        offsets.try_reserve_exact(rows + 1)?;
        offsets.push(0);
        Ok(Self { offsets, bytes: Vec::new() })
    }

    pub fn len(&self) -> usize { self.offsets.len() - 1 }

    pub fn is_empty(&self) -> bool { self.offsets.len() == 1 }

    /// Total bytes over all rows.
    pub fn byte_len(&self) -> usize { self.bytes.len() }

    /// Heap bytes held by the offsets and the byte buffer.
    pub fn payload_bytes(&self) -> usize { self.offsets.len() * size_of::<usize>() + self.bytes.len() }

    pub fn get(&self, index: usize) -> Option<&[u8]> { (index < self.len()).then(|| &self[index]) }

    pub fn push(&mut self, row: &[u8]) {
        self.bytes.extend_from_slice(row);
        self.offsets.push(self.bytes.len());
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &[u8]> + ExactSizeIterator {
        self.offsets.windows(2).map(|pair| &self.bytes[pair[0]..pair[1]])
    }

    /// The rows at `indices`, in that order (repeats allowed).
    pub fn gather(&self, indices: impl IntoIterator<Item = usize>) -> Self { indices.into_iter().map(|index| &self[index]).collect() }

    pub fn to_rows(&self) -> Vec<Vec<u8>> { self.iter().map(<[u8]>::to_vec).collect() }
}

impl Index<usize> for TextArray {
    type Output = [u8];
    fn index(&self, index: usize) -> &[u8] { &self.bytes[self.offsets[index]..self.offsets[index + 1]] }
}

impl<B: AsRef<[u8]>> FromIterator<B> for TextArray {
    fn from_iter<I: IntoIterator<Item = B>>(rows: I) -> Self {
        let mut out = Self::new();
        out.extend(rows);
        out
    }
}

impl<B: AsRef<[u8]>> Extend<B> for TextArray {
    fn extend<I: IntoIterator<Item = B>>(&mut self, rows: I) {
        for row in rows {
            self.push(row.as_ref());
        }
    }
}

impl<B: AsRef<[u8]>> From<Vec<B>> for TextArray {
    fn from(rows: Vec<B>) -> Self { rows.into_iter().collect() }
}

impl<B: AsRef<[u8]>> PartialEq<Vec<B>> for TextArray {
    fn eq(&self, rows: &Vec<B>) -> bool { self.len() == rows.len() && self.iter().zip(rows).all(|(a, b)| a == b.as_ref()) }
}

#[cfg(test)]
mod tests {
    use super::TextArray;

    #[test]
    fn rows_round_trip_with_empty_and_binary_rows() {
        let rows: Vec<Vec<u8>> = vec![b"abc".to_vec(), Vec::new(), vec![0xff, 0, 0x80], Vec::new()];
        let array: TextArray = rows.clone().into();
        assert_eq!((array.len(), array.byte_len()), (4, 6));
        assert_eq!(array, rows);
        assert_eq!(array.to_rows(), rows);
        assert_eq!(&array[2], &[0xff, 0, 0x80][..]);
        assert_eq!(array.get(4), None);
        assert_eq!(array.iter().rev().next(), Some(&b""[..]));
        assert_eq!(array.gather([2, 0, 0]), vec![vec![0xff, 0, 0x80], b"abc".to_vec(), b"abc".to_vec()]);
        assert!(TextArray::new().is_empty() && TextArray::default().len() == 0);
    }
}
