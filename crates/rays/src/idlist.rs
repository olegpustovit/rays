//! `Lru.Identity_list` for keys and values that are not `Send` (scenes, meshes and prepared draws
//! are `Rc`): a bounded most-recently-used list with a caller-supplied equality. Keys and values
//! are `Rc<dyn Any>`; callers downcast in their `equal`. The ledger owner is charged for the
//! entries' bytes until they leave; unlike the shared `lru::IdentityList` it registers no
//! `Mem.retreat` evictor (a documented gap).
use std::any::Any;
use std::collections::VecDeque;
use std::rc::Rc;

pub type Item = Rc<dyn Any>;
pub type Equal = Box<dyn Fn(&Item, &Item) -> bool>;
pub type Release = Box<dyn Fn(&Item, &Item)>;

struct entry {
    key: Item,
    value: Item,
    bytes: i64,
}

pub struct IdList {
    owner: lru::Owner,
    capacity: usize,
    equal: Equal,
    release: Option<Release>,
    entries: VecDeque<entry>,
}

pub fn create(owner: lru::Owner, capacity: i64, equal: Equal, release: Option<Release>) -> IdList {
    IdList { owner, capacity: capacity.max(0) as usize, equal, release, entries: VecDeque::new() }
}

fn drop_entry(list: &IdList, e: entry) {
    mem::release(&list.owner, e.bytes);
    if let Some(release) = &list.release {
        release(&e.key, &e.value);
    }
}

fn index_of(list: &IdList, key: &Item) -> Option<usize> {
    list.entries.iter().position(|e| (list.equal)(&e.key, key))
}

/// A hit moves the entry to the front.
pub fn find(list: &mut IdList, key: &Item) -> Option<Item> {
    let i = index_of(list, key)?;
    let e = list.entries.remove(i)?;
    let value = e.value.clone();
    list.entries.push_front(e);
    Some(value)
}

/// Look up without changing recency.
pub fn peek(list: &IdList, key: &Item) -> Option<Item> {
    index_of(list, key).map(|i| list.entries[i].value.clone())
}

/// `bytes`, when positive, charges the owner until the entry leaves.
pub fn add(list: &mut IdList, bytes: i64, key: Item, value: Item) {
    if let Some(i) = index_of(list, &key) {
        if let Some(old) = list.entries.remove(i) {
            drop_entry(list, old);
        }
    }
    let bytes = bytes.max(0);
    mem::charge(&list.owner, bytes);
    list.entries.push_front(entry { key, value, bytes });
    while list.entries.len() > list.capacity {
        if let Some(old) = list.entries.pop_back() {
            drop_entry(list, old);
        }
    }
}

pub fn remove(list: &mut IdList, key: &Item) {
    if let Some(i) = index_of(list, key) {
        if let Some(old) = list.entries.remove(i) {
            drop_entry(list, old);
        }
    }
}

pub fn head(list: &IdList) -> Option<(Item, Item)> {
    list.entries.front().map(|e| (e.key.clone(), e.value.clone()))
}

/// Front (most recent) to back.
pub fn iter(list: &IdList, f: &mut dyn FnMut(&Item, &Item)) {
    for e in list.entries.iter() {
        f(&e.key, &e.value);
    }
}

/// Keeps the entries `keep` accepts, front to back.
pub fn filter(list: &mut IdList, keep: &mut dyn FnMut(&Item, &Item) -> bool) {
    let entries = std::mem::take(&mut list.entries);
    for e in entries {
        if keep(&e.key, &e.value) {
            list.entries.push_back(e);
        } else {
            drop_entry(list, e);
        }
    }
}

pub fn length(list: &IdList) -> i64 {
    list.entries.len() as i64
}

pub fn clear(list: &mut IdList) {
    let entries = std::mem::take(&mut list.entries);
    for e in entries {
        drop_entry(list, e);
    }
}
