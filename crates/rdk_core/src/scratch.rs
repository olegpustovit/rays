//! Reusable kernel work buffers: borrowed, cleared and refilled per call instead of allocated.
//! Idle capacity is charged to the memory ledger and dropped when the ledger asks for room.
use std::ops::{Deref, DerefMut};
use std::sync::{Arc, LazyLock, Mutex, MutexGuard};

#[derive(Default)]
pub struct Pools {
    i64s: Vec<Vec<i64>>,
    u32s: Vec<Vec<u32>>,
    u8s: Vec<Vec<u8>>,
    f64s: Vec<Vec<f64>>,
    usizes: Vec<Vec<usize>>,
}

// ponytail: one process-wide LIFO pool behind a mutex; per-worker pools if lock contention shows in a profile.
static POOLS: Mutex<Pools> = Mutex::new(Pools { i64s: Vec::new(), u32s: Vec::new(), u8s: Vec::new(), f64s: Vec::new(), usizes: Vec::new() });

static LEDGER: LazyLock<mem::Owner> = LazyLock::new(|| {
    let owner = mem::cache("kernel scratch");
    let held = owner.clone();
    mem::register_evictor(&owner, mem::EvictorOptions::default(), Arc::new(move || {
        let freed = std::mem::take(&mut *lock());
        mem::release(&held, held.bytes());
        drop(freed);
        true
    }));
    owner
});

fn lock() -> MutexGuard<'static, Pools> {
    POOLS.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
}

pub trait Elem: Copy + 'static {
    fn pool(pools: &mut Pools) -> &mut Vec<Vec<Self>>;
}

macro_rules! elem {
    ($t:ty, $f:ident) => {
        impl Elem for $t {
            fn pool(pools: &mut Pools) -> &mut Vec<Vec<Self>> { &mut pools.$f }
        }
    };
}
elem!(i64, i64s);
elem!(u32, u32s);
elem!(u8, u8s);
elem!(f64, f64s);
elem!(usize, usizes);

/// A pooled vector: contents are always freshly filled, capacity is whatever the pool's high-water gave.
pub struct Buf<T: Elem>(Vec<T>);

impl<T: Elem> Deref for Buf<T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Vec<T> { &self.0 }
}
impl<T: Elem> DerefMut for Buf<T> {
    fn deref_mut(&mut self) -> &mut Vec<T> { &mut self.0 }
}

impl<'a, T: Elem> IntoIterator for &'a Buf<T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter { self.0.iter() }
}

/// Borrow a vector of `len` copies of `fill`.
pub fn filled<T: Elem>(len: usize, fill: T) -> Buf<T> {
    let mut buf = empty::<T>();
    buf.0.resize(len, fill);
    buf
}

/// Borrow an empty vector (capacity retained from earlier uses).
pub fn empty<T: Elem>() -> Buf<T> {
    let reused = T::pool(&mut lock()).pop();
    match reused {
        Some(v) => {
            mem::release(&LEDGER, (v.capacity() * size_of::<T>()) as i64);
            Buf(v)
        }
        None => Buf(Vec::new()),
    }
}

impl<T: Elem> Drop for Buf<T> {
    fn drop(&mut self) {
        let mut v = std::mem::take(&mut self.0);
        if v.capacity() == 0 {
            return;
        }
        v.clear();
        mem::charge(&LEDGER, (v.capacity() * size_of::<T>()) as i64);
        T::pool(&mut lock()).push(v);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn borrowed_buffers_are_always_freshly_filled() {
        let mut first = filled::<i64>(1000, 7);
        first[3] = 99;
        drop(first);
        let again = filled::<i64>(500, -1);
        assert!(again.len() == 500 && again.iter().all(|&x| x == -1));
    }
}
