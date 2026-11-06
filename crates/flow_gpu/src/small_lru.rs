//! A bounded most-recent-first list with a release callback: the part of `Lru.Make` /
//! `Lru.Identity_list` that flow_gpu uses, for values that are not `Send` (OGPU handles are `Rc`).
//! Eviction releases the oldest entry; every list has an explicit capacity.

pub struct List<K, V> {
    capacity: usize,
    /// newest first
    entries: Vec<(K, V)>,
    release: Box<dyn Fn(&K, V)>,
}

impl<K, V> List<K, V> {
    pub fn new(capacity: usize, release: Box<dyn Fn(&K, V)>) -> List<K, V> {
        List { capacity, entries: Vec::new(), release }
    }
    pub fn length(&self) -> usize {
        self.entries.len()
    }
    /// Adds at the front; the oldest entry past capacity is released.
    pub fn add(&mut self, key: K, value: V) {
        self.entries.insert(0, (key, value));
        while self.entries.len() > self.capacity {
            let (k, v) = self.entries.pop().unwrap();
            (self.release)(&k, v);
        }
    }
    /// The first entry satisfying `pred`, moved to the front.
    pub fn find(&mut self, pred: &dyn Fn(&K, &V) -> bool) -> Option<&mut V> {
        let at = self.entries.iter().position(|(k, v)| pred(k, v))?;
        let entry = self.entries.remove(at);
        self.entries.insert(0, entry);
        Some(&mut self.entries[0].1)
    }
    /// As `find`, without touching the order.
    pub fn peek(&self, pred: &dyn Fn(&K, &V) -> bool) -> Option<&V> {
        self.entries.iter().find(|(k, v)| pred(k, v)).map(|(_, v)| v)
    }
    pub fn peek_mut(&mut self, pred: &dyn Fn(&K, &V) -> bool) -> Option<&mut V> {
        self.entries.iter_mut().find(|(k, v)| pred(k, v)).map(|(_, v)| v)
    }
    pub fn iter(&self) -> impl Iterator<Item = &(K, V)> {
        self.entries.iter()
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut (K, V)> {
        self.entries.iter_mut()
    }
    pub fn remove(&mut self, pred: &dyn Fn(&K, &V) -> bool) {
        if let Some(at) = self.entries.iter().position(|(k, v)| pred(k, v)) {
            let (k, v) = self.entries.remove(at);
            (self.release)(&k, v);
        }
    }
    /// Releases every entry, oldest first.
    pub fn clear(&mut self) {
        while let Some((k, v)) = self.entries.pop() {
            (self.release)(&k, v);
        }
    }
}
