//! A bounded recency list with a release callback: the part of `Lru.Make` /
//! `Lru.Identity_list` that flow_gpu uses, for values that are not `Send` (OGPU handles are `Rc`).
//! Eviction releases the oldest entry; every list has an explicit capacity.

use std::collections::VecDeque;

pub struct List<K, V> {
    capacity: usize,
    /// oldest first
    entries: VecDeque<(K, V)>,
    release: Box<dyn Fn(&K, V)>,
}

impl<K, V> List<K, V> {
    pub fn new(capacity: usize, release: Box<dyn Fn(&K, V)>) -> List<K, V> {
        List { capacity, entries: VecDeque::new(), release }
    }
    pub fn length(&self) -> usize {
        self.entries.len()
    }
    /// Adds as the newest; the oldest entry past capacity is released.
    pub fn add(&mut self, key: K, value: V) {
        self.entries.push_back((key, value));
        while self.entries.len() > self.capacity {
            let (k, v) = self.entries.pop_front().unwrap();
            (self.release)(&k, v);
        }
    }
    /// The newest entry satisfying `pred`, made the newest.
    pub fn find(&mut self, pred: &dyn Fn(&K, &V) -> bool) -> Option<&mut V> {
        let at = self.entries.iter().rposition(|(k, v)| pred(k, v))?;
        let entry = self.entries.remove(at)?;
        self.entries.push_back(entry);
        self.entries.back_mut().map(|e| &mut e.1)
    }
    /// As `find`, without touching the order.
    pub fn peek(&self, pred: &dyn Fn(&K, &V) -> bool) -> Option<&V> {
        self.entries.iter().rev().find(|(k, v)| pred(k, v)).map(|(_, v)| v)
    }
    pub fn peek_mut(&mut self, pred: &dyn Fn(&K, &V) -> bool) -> Option<&mut V> {
        self.entries.iter_mut().rev().find(|(k, v)| pred(k, v)).map(|(_, v)| v)
    }
    /// Newest first.
    pub fn iter(&self) -> impl Iterator<Item = &(K, V)> {
        self.entries.iter().rev()
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut (K, V)> {
        self.entries.iter_mut().rev()
    }
    pub fn remove(&mut self, pred: &dyn Fn(&K, &V) -> bool) {
        if let Some(at) = self.entries.iter().rposition(|(k, v)| pred(k, v)) {
            let (k, v) = self.entries.remove(at).unwrap();
            (self.release)(&k, v);
        }
    }
    /// Releases every entry, oldest first.
    pub fn clear(&mut self) {
        while let Some((k, v)) = self.entries.pop_front() {
            (self.release)(&k, v);
        }
    }
}
