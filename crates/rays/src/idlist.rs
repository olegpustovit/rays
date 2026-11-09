//! Local identity keys over the renderer's shared cache adapter. Ordering, capacity and ledger
//! accounting are owned by `lru::Cache`; no second queue or cache implementation lives here.
use renderer::side_lru;
pub(crate) use side_lru::{Item, find, remove};
#[cfg(test)]
pub(crate) use side_lru::{clear, length, head_recent as head};
pub(crate) use side_lru::{iter_recent as iter, filter, peek};
pub(crate) type IdList = side_lru::Table<Item>;
pub(crate) type Equal = side_lru::Equal<Item>;
pub(crate) type Release = Box<dyn Fn(&Item, &Item)>;

pub(crate) fn create(owner: lru::Owner, capacity: i64, equal: Equal, release: Option<Release>) -> IdList {
    side_lru::create_identity(owner, capacity, equal, Box::new(move |key, value| {
        if let Some(release) = &release { release(key, &value); }
    }))
}

pub(crate) fn add(list: &mut IdList, bytes: i64, key: Item, value: Item) {
    side_lru::add(list, bytes.max(0), key, value);
}
