//! Reusable kernel work buffers: borrowed, cleared and refilled per call instead of allocated.
//! Idle capacity is charged to the memory ledger and dropped when the ledger asks for room.
//! One concrete buffer type per element (the product-crate shape rules forbid generics and macros).
use std::ops::{Deref, DerefMut};
use std::sync::{Arc, LazyLock, Mutex, MutexGuard};

struct Pools {
    i64s: Vec<Vec<i64>>,
    u8s: Vec<Vec<u8>>,
    usizes: Vec<Vec<usize>>,
    f64s: Vec<Vec<f64>>,
}

// ponytail: one process-wide LIFO pool behind a mutex; per-worker pools if lock contention shows in a profile.
static POOLS: Mutex<Pools> = Mutex::new(Pools { i64s: Vec::new(), u8s: Vec::new(), usizes: Vec::new(), f64s: Vec::new() });

static LEDGER: LazyLock<mem::Owner> = LazyLock::new(|| {
    let owner = mem::cache("kernel scratch");
    let held = owner.clone();
    mem::register_evictor(&owner, mem::EvictorOptions::default(), Arc::new(move || {
        let freed = std::mem::replace(&mut *lock(), Pools { i64s: Vec::new(), u8s: Vec::new(), usizes: Vec::new(), f64s: Vec::new() });
        mem::release(&held, held.bytes());
        drop(freed);
        true
    }));
    owner
});

fn lock() -> MutexGuard<'static, Pools> {
    POOLS.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
}

/// A pooled `Vec<i64>`: contents are always freshly filled, capacity is the pool's high-water.
pub struct I64(Vec<i64>);

impl Deref for I64 {
    type Target = Vec<i64>;
    fn deref(&self) -> &Vec<i64> { &self.0 }
}
impl DerefMut for I64 {
    fn deref_mut(&mut self) -> &mut Vec<i64> { &mut self.0 }
}
impl<'a> IntoIterator for &'a I64 {
    type Item = &'a i64;
    type IntoIter = std::slice::Iter<'a, i64>;
    fn into_iter(self) -> Self::IntoIter { self.0.iter() }
}
impl Drop for I64 {
    fn drop(&mut self) {
        let mut v = std::mem::take(&mut self.0);
        if v.capacity() == 0 {
            return;
        }
        v.clear();
        mem::charge(&LEDGER, (v.capacity() * size_of::<i64>()) as i64);
        lock().i64s.push(v);
    }
}

/// Borrow an empty vector (capacity retained from earlier uses).
pub fn i64_empty() -> I64 {
    let reused = lock().i64s.pop();
    match reused {
        Some(v) => {
            mem::release(&LEDGER, (v.capacity() * size_of::<i64>()) as i64);
            I64(v)
        }
        None => I64(Vec::new()),
    }
}

/// Borrow a vector of `len` copies of `fill`.
pub fn i64_filled(len: usize, fill: i64) -> I64 {
    let mut buf = i64_empty();
    buf.0.resize(len, fill);
    buf
}

/// A pooled `Vec<u8>`: contents are always freshly filled, capacity is the pool's high-water.
pub struct U8(Vec<u8>);

impl Deref for U8 {
    type Target = Vec<u8>;
    fn deref(&self) -> &Vec<u8> { &self.0 }
}
impl DerefMut for U8 {
    fn deref_mut(&mut self) -> &mut Vec<u8> { &mut self.0 }
}
impl<'a> IntoIterator for &'a U8 {
    type Item = &'a u8;
    type IntoIter = std::slice::Iter<'a, u8>;
    fn into_iter(self) -> Self::IntoIter { self.0.iter() }
}
impl Drop for U8 {
    fn drop(&mut self) {
        let mut v = std::mem::take(&mut self.0);
        if v.capacity() == 0 {
            return;
        }
        v.clear();
        mem::charge(&LEDGER, (v.capacity() * size_of::<u8>()) as i64);
        lock().u8s.push(v);
    }
}

/// Borrow an empty vector (capacity retained from earlier uses).
pub fn u8_empty() -> U8 {
    let reused = lock().u8s.pop();
    match reused {
        Some(v) => {
            mem::release(&LEDGER, (v.capacity() * size_of::<u8>()) as i64);
            U8(v)
        }
        None => U8(Vec::new()),
    }
}

/// Borrow a vector of `len` copies of `fill`.
pub fn u8_filled(len: usize, fill: u8) -> U8 {
    let mut buf = u8_empty();
    buf.0.resize(len, fill);
    buf
}

/// A pooled `Vec<usize>`: contents are always freshly filled, capacity is the pool's high-water.
pub struct Usize(Vec<usize>);

impl Deref for Usize {
    type Target = Vec<usize>;
    fn deref(&self) -> &Vec<usize> { &self.0 }
}
impl DerefMut for Usize {
    fn deref_mut(&mut self) -> &mut Vec<usize> { &mut self.0 }
}
impl<'a> IntoIterator for &'a Usize {
    type Item = &'a usize;
    type IntoIter = std::slice::Iter<'a, usize>;
    fn into_iter(self) -> Self::IntoIter { self.0.iter() }
}
impl Drop for Usize {
    fn drop(&mut self) {
        let mut v = std::mem::take(&mut self.0);
        if v.capacity() == 0 {
            return;
        }
        v.clear();
        mem::charge(&LEDGER, (v.capacity() * size_of::<usize>()) as i64);
        lock().usizes.push(v);
    }
}

/// Borrow an empty vector (capacity retained from earlier uses).
pub fn usize_empty() -> Usize {
    let reused = lock().usizes.pop();
    match reused {
        Some(v) => {
            mem::release(&LEDGER, (v.capacity() * size_of::<usize>()) as i64);
            Usize(v)
        }
        None => Usize(Vec::new()),
    }
}

/// Borrow a vector of `len` copies of `fill`.
pub fn usize_filled(len: usize, fill: usize) -> Usize {
    let mut buf = usize_empty();
    buf.0.resize(len, fill);
    buf
}

/// A pooled `Vec<f64>`: contents are always freshly filled, capacity is the pool's high-water.
pub struct F64(Vec<f64>);

impl Deref for F64 {
    type Target = Vec<f64>;
    fn deref(&self) -> &Vec<f64> { &self.0 }
}
impl DerefMut for F64 {
    fn deref_mut(&mut self) -> &mut Vec<f64> { &mut self.0 }
}
impl<'a> IntoIterator for &'a F64 {
    type Item = &'a f64;
    type IntoIter = std::slice::Iter<'a, f64>;
    fn into_iter(self) -> Self::IntoIter { self.0.iter() }
}
impl Drop for F64 {
    fn drop(&mut self) {
        let mut v = std::mem::take(&mut self.0);
        if v.capacity() == 0 {
            return;
        }
        v.clear();
        mem::charge(&LEDGER, (v.capacity() * size_of::<f64>()) as i64);
        lock().f64s.push(v);
    }
}

/// Borrow an empty vector (capacity retained from earlier uses).
pub fn f64_empty() -> F64 {
    let reused = lock().f64s.pop();
    match reused {
        Some(v) => {
            mem::release(&LEDGER, (v.capacity() * size_of::<f64>()) as i64);
            F64(v)
        }
        None => F64(Vec::new()),
    }
}

/// Borrow a vector of `len` copies of `fill`.
pub fn f64_filled(len: usize, fill: f64) -> F64 {
    let mut buf = f64_empty();
    buf.0.resize(len, fill);
    buf
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn borrowed_buffers_are_always_freshly_filled() {
        let mut first = i64_filled(1000, 7);
        first[3] = 99;
        drop(first);
        let again = i64_filled(500, -1);
        assert!(again.len() == 500 && again.iter().all(|&x| x == -1));
    }
}
