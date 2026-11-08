//! Rows of bytes as one offsets vector plus one byte buffer: N rows cost two allocations, not N + 1.
//! Attribute storage and Flow values share it (`Arc<TextArray>`), so reads and writes clone a pointer.
use std::ops::Index;

#[derive(Clone, Debug)]
pub struct TextArray {
    /// Monotone row boundaries into `bytes`, starting at zero and ending at `bytes.len()`. Rows past
    /// them are empty: a builder filling empty rows in order appends instead of shifting.
    offsets: Vec<usize>,
    bytes: Vec<u8>,
    rows: usize,
}

impl Default for TextArray {
    fn default() -> Self { Self::new() }
}

impl TextArray {
    pub fn new() -> Self { Self { offsets: vec![0], bytes: Vec::new(), rows: 0 } }

    pub fn with_capacity(rows: usize, bytes: usize) -> Self {
        let mut offsets = Vec::with_capacity(rows + 1);
        offsets.push(0);
        Self { offsets, bytes: Vec::with_capacity(bytes), rows: 0 }
    }

    /// Reserves for `rows` rows, reporting allocation failure instead of aborting.
    pub fn try_with_capacity(rows: usize) -> Result<Self, std::collections::TryReserveError> {
        let mut offsets = Vec::new();
        offsets.try_reserve_exact(rows + 1)?;
        offsets.push(0);
        Ok(Self { offsets, bytes: Vec::new(), rows: 0 })
    }

    pub fn len(&self) -> usize { self.rows }

    pub fn is_empty(&self) -> bool { self.rows == 0 }

    fn bound(&self, row: usize) -> usize { self.offsets.get(row).copied().unwrap_or(self.bytes.len()) }

    /// Total bytes over all rows.
    pub fn byte_len(&self) -> usize { self.bytes.len() }

    /// Heap bytes held by the offsets and the byte buffer.
    pub fn payload_bytes(&self) -> usize { self.offsets.len() * size_of::<usize>() + self.bytes.len() }

    pub fn get(&self, index: usize) -> Option<&[u8]> { (index < self.len()).then(|| &self[index]) }

    pub fn push(&mut self, row: &[u8]) {
        self.offsets.resize(self.rows + 1, self.bytes.len());
        self.bytes.extend_from_slice(row);
        self.offsets.push(self.bytes.len());
        self.rows += 1;
    }

    /// Replaces row `index` in place: a row of the same length overwrites its bytes; a row whose
    /// successors are all empty appends (filling in order is linear); another shifts the bytes and
    /// boundaries after it.
    pub fn replace(&mut self, index: usize, row: &[u8]) {
        assert!(index < self.rows, "TextArray row {index} is outside its {} rows", self.rows);
        let (start, end) = (self.bound(index), self.bound(index + 1));
        if end - start == row.len() {
            self.bytes[start..end].copy_from_slice(row);
            return;
        }
        if start == self.bytes.len() {
            self.offsets.resize(index + 1, start);
            self.bytes.extend_from_slice(row);
            self.offsets.push(self.bytes.len());
            return;
        }
        self.bytes.splice(start..end, row.iter().copied());
        for offset in &mut self.offsets[index + 1..] {
            *offset = *offset - (end - start) + row.len();
        }
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &[u8]> + ExactSizeIterator {
        (0..self.rows).map(|row| &self.bytes[self.bound(row)..self.bound(row + 1)])
    }

    /// The rows at `indices`, in that order (repeats allowed).
    pub fn gather(&self, indices: impl IntoIterator<Item = usize>) -> Self { indices.into_iter().map(|index| &self[index]).collect() }

    pub fn to_rows(&self) -> Vec<Vec<u8>> { self.iter().map(<[u8]>::to_vec).collect() }
}

impl Index<usize> for TextArray {
    type Output = [u8];
    fn index(&self, index: usize) -> &[u8] {
        assert!(index < self.rows, "TextArray row {index} is outside its {} rows", self.rows);
        &self.bytes[self.bound(index)..self.bound(index + 1)]
    }
}

impl PartialEq for TextArray {
    fn eq(&self, other: &Self) -> bool { self.rows == other.rows && self.iter().eq(other.iter()) }
}

impl Eq for TextArray {}

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

    #[test]
    fn replace_fills_in_order_and_shifts_otherwise() {
        let mut array: TextArray = vec![b""; 5].into();
        for (i, row) in [&b"ab"[..], b"", b"cde", b"f"].into_iter().enumerate() { array.replace(i, row); }
        assert_eq!(array, vec![&b"ab"[..], b"", b"cde", b"f", b""]);
        array.replace(1, b"xyz");
        array.replace(0, b"q");
        array.replace(2, b"CDE");
        assert_eq!(array, vec![&b"q"[..], b"xyz", b"CDE", b"f", b""]);
        array.push(b"g");
        assert_eq!((array.len(), array.byte_len(), &array[5]), (6, 9, &b"g"[..]));
        assert_eq!(array, TextArray::from(vec![&b"q"[..], b"xyz", b"CDE", b"f", b"", b"g"]));
        assert_eq!(array.iter().rev().nth(1), Some(&b""[..]));
    }
}
