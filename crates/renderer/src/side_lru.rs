//! The OCaml `Lru.Make (K)` table for values that are not `Send` (OGPU handles are `Rc`). The shared
//! `lru::Cache` keeps ordering, capacities and the ledger, with an id as its value; the values live
//! here, and the ids a release names are settled into the caller's release function.
use lru::{Cache, CacheOptions, Key as LruKey, Value};
use std::any::Any;
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::sync::{Arc, Mutex};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TableKey {
    Str(String),
    Int(i64),
}

pub type Item = Rc<dyn Any>;
pub type Release = Box<dyn Fn(&TableKey, Item)>;

pub struct Table {
    cache: Cache,
    items: HashMap<i64, (TableKey, Item)>,
    next: i64,
    evicted: Arc<Mutex<Vec<i64>>>,
    release: Release,
}

fn hash_of(key: &TableKey) -> u64 {
    let mut hasher = DefaultHasher::new();
    key.hash(&mut hasher);
    hasher.finish()
}

fn lru_key(key: &TableKey) -> LruKey {
    LruKey::new(hash_of(key), Arc::new(key.clone()))
}

fn id_of(value: &Value) -> i64 {
    *value.downcast_ref::<i64>().expect("table value is an id")
}

fn same_key(stored: &LruKey, probe: &(dyn Any + Send + Sync)) -> bool {
    match (stored.as_any().downcast_ref::<TableKey>(), probe.downcast_ref::<TableKey>()) {
        (Some(a), Some(b)) => a == b,
        _ => false,
    }
}

/// `create ~owner ?charges ?byte_capacity ?release capacity`
pub fn create(owner: lru::Owner, charges: bool, byte_capacity: i64, release: Release, capacity: i64) -> Table {
    let evicted: Arc<Mutex<Vec<i64>>> = Arc::new(Mutex::new(Vec::new()));
    let sink = Arc::clone(&evicted);
    let options = CacheOptions {
        charges,
        byte_capacity,
        release: Some(Arc::new(move |_key: &LruKey, value: &Value| {
            sink.lock().expect("evicted ids").push(id_of(value));
        })),
        ..CacheOptions::default()
    };
    Table {
        cache: Cache::with_options(owner, capacity, Arc::new(same_key), options),
        items: HashMap::new(),
        next: 1,
        evicted,
        release,
    }
}

/// Runs the release function for every entry the cache has let go since the last call.
fn settle(table: &mut Table) {
    let ids: Vec<i64> = std::mem::take(&mut *table.evicted.lock().expect("evicted ids"));
    for id in ids {
        if let Some((key, item)) = table.items.remove(&id) {
            (table.release)(&key, item);
        }
    }
}

pub fn find(table: &mut Table, key: &TableKey) -> Option<Item> {
    settle(table);
    let hash = hash_of(key);
    let value = table.cache.find_opt_by(hash, key)?;
    table.items.get(&id_of(&value)).map(|(_, item)| item.clone())
}

pub fn peek(table: &mut Table, key: &TableKey) -> Option<Item> {
    settle(table);
    let hash = hash_of(key);
    let value = table.cache.peek_by(hash, key)?;
    table.items.get(&id_of(&value)).map(|(_, item)| item.clone())
}

pub fn add(table: &mut Table, bytes: i64, key: TableKey, item: Item) {
    settle(table);
    let id = table.next;
    table.next += 1;
    table.items.insert(id, (key.clone(), item));
    table.cache.add(lru_key(&key), Arc::new(id), bytes);
    settle(table);
}

pub fn remove(table: &mut Table, key: &TableKey) {
    let hash = hash_of(key);
    table.cache.remove_by(hash, key);
    settle(table);
}

/// Removes without releasing: the caller now owns the value.
pub fn take(table: &mut Table, key: &TableKey) -> Option<Item> {
    settle(table);
    let hash = hash_of(key);
    let value = table.cache.take_by(hash, key)?;
    table.items.remove(&id_of(&value)).map(|(_, item)| item)
}

/// Oldest inserted first, without reordering.
pub fn find_first(table: &mut Table, predicate: &dyn Fn(&TableKey, &Item) -> bool) -> Option<Item> {
    settle(table);
    let items = &table.items;
    let value = table.cache.find_first(&|_, value| {
        items.get(&id_of(value)).is_some_and(|(key, item)| predicate(key, item))
    })?;
    table.items.get(&id_of(&value)).map(|(_, item)| item.clone())
}

/// Oldest inserted first. `f` must not add or remove entries.
pub fn iter(table: &mut Table, f: &mut dyn FnMut(&TableKey, &Item)) {
    settle(table);
    let items = &table.items;
    table.cache.iter(&mut |_, value| {
        if let Some((key, item)) = items.get(&id_of(value)) {
            f(key, item);
        }
    });
}

pub fn length(table: &mut Table) -> i64 {
    settle(table);
    table.cache.length()
}

pub fn bytes(table: &Table) -> i64 {
    table.cache.bytes()
}

pub fn clear(table: &mut Table) {
    table.cache.clear();
    settle(table);
}
