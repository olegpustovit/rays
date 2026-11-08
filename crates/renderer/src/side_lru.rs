//! Local keys and values over the shared `lru::Cache` engine. The shared cache stores integer
//! IDs; `Rc` resources and custom key equality stay on their owning thread. Default string/int
//! keys keep second-chance eviction; identity keys promote hits for exact MRU ordering.
use lru::{Cache, CacheOptions, Evictable, Key as LruKey, Value};
use std::any::Any;
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::sync::{Arc, Mutex};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TableKey {
    Str(String),
    Int(i64),
}

pub const UNBOUNDED_BYTES: i64 = (1 << 62) - 1;
pub type Item = Rc<dyn Any>;
pub type Release<K = TableKey> = Box<dyn Fn(&K, Item)>;
pub type Equal<K> = Box<dyn Fn(&K, &K) -> bool>;

pub struct Table<K = TableKey> {
    cache: Cache,
    items: HashMap<i64, (K, Item)>,
    next: i64,
    evicted: Arc<Mutex<Vec<i64>>>,
    pinned: Arc<Mutex<HashSet<i64>>>,
    release: Release<K>,
    hash: fn(&K) -> u64,
    equal: Equal<K>,
    promote: bool,
}

fn hash_of(key: &TableKey) -> u64 {
    let mut hasher = DefaultHasher::new();
    key.hash(&mut hasher);
    hasher.finish()
}

fn id_of(value: &Value) -> i64 {
    *value.downcast_ref::<i64>().expect("table value is an id")
}

fn key_id(key: &LruKey) -> i64 {
    *key.as_any().downcast_ref::<i64>().expect("table key is an id")
}

fn same_id(stored: &LruKey, probe: &(dyn Any + Send + Sync)) -> bool {
    probe.downcast_ref::<i64>().is_some_and(|id| key_id(stored) == *id)
}

pub fn create(owner: lru::Owner, charges: bool, byte_capacity: i64, release: Release, capacity: i64) -> Table {
    create_inner(owner, capacity, release, hash_of, Box::new(|a, b| a == b), false, CacheOptions { charges, byte_capacity, ..CacheOptions::default() })
}

pub fn create_identity(owner: lru::Owner, capacity: i64, equal: Equal<Item>, release: Release<Item>) -> Table<Item> {
    // ponytail: comparator-only keys share one bucket (O(n)); supply hashes if these small caches grow.
    // These caches previously had no pressure eviction. Their entries currently charge zero bytes.
    create_inner(owner, capacity, release, |_| 0, equal, true, CacheOptions { shed: lru::Shed::Never, byte_capacity: i64::MAX, ..CacheOptions::default() })
}

fn create_inner<K>(owner: lru::Owner, capacity: i64, release: Release<K>, hash: fn(&K) -> u64, equal: Equal<K>, promote: bool, mut options: CacheOptions) -> Table<K> {
    let evicted: Arc<Mutex<Vec<i64>>> = Arc::new(Mutex::new(Vec::new()));
    let pinned: Arc<Mutex<HashSet<i64>>> = Arc::new(Mutex::new(HashSet::new()));
    let sink = evicted.clone();
    let guard = pinned.clone();
    let evictable: Evictable = Arc::new(move |_, value| !guard.lock().expect("pinned ids").contains(&id_of(value)));
    options.release = Some(Arc::new(move |_, value| sink.lock().expect("evicted ids").push(id_of(value))));
    options.evictable = Some(evictable);
    Table { cache: Cache::with_options(owner, capacity, Arc::new(same_id), options), items: HashMap::new(), next: 1, evicted, pinned, release, hash, equal, promote }
}

fn settle<K>(table: &mut Table<K>) {
    let ids = std::mem::take(&mut *table.evicted.lock().expect("evicted ids"));
    for id in ids {
        table.pinned.lock().expect("pinned ids").remove(&id);
        if let Some((key, item)) = table.items.remove(&id) { (table.release)(&key, item); }
    }
}

fn matches<K>(table: &Table<K>, stored: &LruKey, probe: &K) -> bool {
    table.items.get(&key_id(stored)).is_some_and(|(key, _)| (table.equal)(key, probe))
}

pub fn find<K>(table: &mut Table<K>, key: &K) -> Option<Item> {
    settle(table);
    let value = table.cache.find_with((table.hash)(key), &|stored| matches(table, stored, key), table.promote)?;
    table.items.get(&id_of(&value)).map(|(_, item)| item.clone())
}

pub fn peek<K>(table: &mut Table<K>, key: &K) -> Option<Item> {
    settle(table);
    let value = table.cache.peek_with((table.hash)(key), &|stored| matches(table, stored, key), table.promote)?;
    table.items.get(&id_of(&value)).map(|(_, item)| item.clone())
}

pub fn add<K>(table: &mut Table<K>, bytes: i64, key: K, item: Item) {
    add_pinned(table, bytes, key, item, false);
}

pub fn add_pinned<K>(table: &mut Table<K>, bytes: i64, key: K, item: Item, pinned: bool) {
    remove(table, &key);
    let id = table.next;
    table.next = table.next.checked_add(1).expect("cache entry IDs exhausted");
    let hash = (table.hash)(&key);
    table.items.insert(id, (key, item));
    if pinned { table.pinned.lock().expect("pinned ids").insert(id); }
    let value: Value = Arc::new(id);
    table.cache.add(LruKey::new(hash, value.clone()), value, bytes);
    settle(table);
}

pub fn set_pinned<K>(table: &mut Table<K>, key: &K, pinned: bool) {
    if let Some(value) = table.cache.peek_with((table.hash)(key), &|stored| matches(table, stored, key), table.promote) {
        let id = id_of(&value);
        let mut set = table.pinned.lock().expect("pinned ids");
        if pinned { set.insert(id); } else { set.remove(&id); }
    }
}

pub fn remove<K>(table: &mut Table<K>, key: &K) {
    settle(table);
    let hash = (table.hash)(key);
    if let Some(value) = table.cache.peek_with(hash, &|stored| matches(table, stored, key), table.promote) {
        table.cache.remove_by(hash, &id_of(&value));
    }
    settle(table);
}

pub fn take<K>(table: &mut Table<K>, key: &K) -> Option<Item> {
    settle(table);
    let hash = (table.hash)(key);
    let value = table.cache.peek_with(hash, &|stored| matches(table, stored, key), table.promote)?;
    let id = id_of(&table.cache.take_by(hash, &id_of(&value))?);
    table.pinned.lock().expect("pinned ids").remove(&id);
    table.items.remove(&id).map(|(_, item)| item)
}

/// Oldest first in the cache's order, without reordering.
pub fn find_first<K>(table: &mut Table<K>, predicate: &dyn Fn(&K, &Item) -> bool) -> Option<Item> {
    settle(table);
    let value = table.cache.find_first(&|_, value| {
        table.items.get(&id_of(value)).is_some_and(|(key, item)| predicate(key, item))
    })?;
    table.items.get(&id_of(&value)).map(|(_, item)| item.clone())
}

pub fn iter<K>(table: &mut Table<K>, f: &mut dyn FnMut(&K, &Item)) {
    settle(table);
    table.cache.iter(&mut |_, value| {
        if let Some((key, item)) = table.items.get(&id_of(value)) { f(key, item); }
    });
}

pub fn iter_recent<K>(table: &mut Table<K>, f: &mut dyn FnMut(&K, &Item)) {
    settle(table);
    table.cache.iter_recent(&mut |_, value| {
        if let Some((key, item)) = table.items.get(&id_of(value)) { f(key, item); }
    });
}

pub fn head_recent<K: Clone>(table: &mut Table<K>) -> Option<(K, Item)> {
    settle(table);
    let (_, value) = table.cache.head_recent()?;
    table.items.get(&id_of(&value)).cloned()
}

pub fn filter<K>(table: &mut Table<K>, keep: &mut dyn FnMut(&K, &Item) -> bool) {
    settle(table);
    table.cache.retain_recent(&mut |_, value| {
        table.items.get(&id_of(value)).is_some_and(|(key, item)| keep(key, item))
    });
    settle(table);
}

pub fn length<K>(table: &mut Table<K>) -> i64 {
    settle(table);
    table.cache.length()
}

pub fn bytes<K>(table: &Table<K>) -> i64 { table.cache.bytes() }

pub fn clear<K>(table: &mut Table<K>) {
    if table.promote { table.cache.retain_recent(&mut |_, _| false); } else { table.cache.clear(); }
    settle(table);
}

impl<K> Drop for Table<K> {
    fn drop(&mut self) { clear(self); }
}
