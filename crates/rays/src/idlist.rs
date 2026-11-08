//! Local identity keys over the renderer's shared cache adapter. Ordering, capacity and ledger
//! accounting are owned by `lru::Cache`; no second queue or cache implementation lives here.
use renderer::side_lru;
pub use side_lru::{Item, clear, find, length, remove};
pub use side_lru::{head_recent as head, iter_recent as iter, filter, peek};
pub type IdList = side_lru::Table<Item>;
pub type Equal = side_lru::Equal<Item>;
pub type Release = Box<dyn Fn(&Item, &Item)>;

pub fn create(owner: lru::Owner, capacity: i64, equal: Equal, release: Option<Release>) -> IdList {
    side_lru::create_identity(owner, capacity, equal, Box::new(move |key, value| {
        if let Some(release) = &release { release(key, &value); }
    }))
}

pub fn add(list: &mut IdList, bytes: i64, key: Item, value: Item) {
    side_lru::add(list, bytes.max(0), key, value);
}
