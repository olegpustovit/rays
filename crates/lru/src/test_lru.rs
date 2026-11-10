use super::*;
use std::any::Any;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicI64, AtomicUsize, Ordering};
use std::sync::{LazyLock, Mutex, MutexGuard};

static TEST_LOCK: LazyLock<Mutex<()>> = LazyLock::new(|| Mutex::new(()));

fn setup() -> MutexGuard<'static, ()> {
    let guard = TEST_LOCK.lock().unwrap_or_else(|error| error.into_inner());
    mem::reset();
    guard
}

// Integer log entries are newest-first key/value pairs.
type Log = Arc<Mutex<Vec<(i32, String)>>>;

fn int_hash(value: i32) -> u64 {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

fn int_key(value: i32) -> Key {
    Key::new(int_hash(value), Arc::new(value))
}

fn ikey(key: &Key) -> i32 {
    *key.as_any().downcast_ref::<i32>().expect("int key")
}

fn int_equal(left: &Key, right: &(dyn Any + Send + Sync)) -> bool {
    matches!((left.as_any().downcast_ref::<i32>(), right.downcast_ref::<i32>()), (Some(a), Some(b)) if a == b)
}

// Identity keys: `[| v |]` arrays compared by physical equality.
fn kv(value: i32) -> Key {
    Key::from_value(Arc::new([value]))
}

fn array_key_value(key: &Key) -> i32 {
    key.as_any().downcast_ref::<[i32; 1]>().expect("array key")[0]
}

fn array_equal(left: &Key, right: &(dyn Any + Send + Sync)) -> bool {
    match (left.as_any().downcast_ref::<[i32; 1]>(), right.downcast_ref::<[i32; 1]>()) {
        (Some(a), Some(b)) => std::ptr::eq(a, b),
        _ => false,
    }
}

fn sv(value: &str) -> Value {
    Arc::new(value.to_owned())
}

fn text(value: &Value) -> &str {
    value.downcast_ref::<String>().expect("string value")
}

fn keys(cache: &Cache) -> Vec<i32> {
    let mut out = Vec::new();
    cache.iter(&mut |key: &Key, _: &Value| out.push(ikey(key)));
    out
}

fn cache_with(owner: Owner, capacity: i64, options: CacheOptions) -> Cache {
    Cache::with_options(owner, capacity, Arc::new(int_equal), options)
}

fn recorder(log: &Log) -> Release {
    let log = Arc::clone(log);
    Arc::new(move |key: &Key, value: &Value| {
        log.lock().unwrap().insert(0, (ikey(key), text(value).to_owned()));
    })
}

fn noop_release() -> Release {
    Arc::new(|_: &Key, _: &Value| {})
}

fn pairs(log: &Log) -> Vec<(i32, String)> {
    log.lock().unwrap().clone()
}

fn pair(key: i32, value: &str) -> (i32, String) {
    (key, value.to_owned())
}

#[test]
fn order_hits_copy_replace_take_remove_and_drop_oldest() {
    let _guard = setup();
    let released: Log = Arc::default();
    let cache = cache_with(
        mem::cache("test.lru.release"),
        3,
        CacheOptions { release: Some(recorder(&released)), ..CacheOptions::default() },
    );
    cache.add(int_key(1), sv("a"), 0);
    cache.add(int_key(2), sv("b"), 0);
    cache.add(int_key(3), sv("c"), 0);
    assert_eq!(keys(&cache), vec![1, 2, 3], "insertion order oldest first");
    assert_eq!(text(&cache.find(&int_key(1)).unwrap()), "a", "hit");
    assert!(cache.find_opt(&int_key(9)).is_none() && cache.find(&int_key(9)).is_err(), "miss");
    assert_eq!(keys(&cache), vec![1, 2, 3], "hit only marks the entry");
    assert!(
        cache.peek(&int_key(2)).as_ref().map(text) == Some("b") && keys(&cache) == vec![1, 2, 3],
        "peek keeps order"
    );

    let snapshot_released: Log = Arc::default();
    let snapshot = cache.copy(recorder(&snapshot_released));
    snapshot.add(int_key(4), sv("d"), 0);
    assert_eq!(keys(&snapshot), vec![3, 4, 1]);
    assert_eq!(pairs(&snapshot_released), vec![pair(2, "b")]);
    assert_eq!(
        keys(&cache),
        vec![1, 2, 3],
        "snapshot preserves touch bits and second-chance eviction"
    );
    assert!(pairs(&released).is_empty(), "snapshot owns independent metadata and release");

    cache.add(int_key(4), sv("d"), 0);
    assert_eq!(keys(&cache), vec![3, 4, 1]);
    assert_eq!(pairs(&released), vec![pair(2, "b")], "the found entry gets a second chance; the untouched oldest goes");

    cache.add(int_key(3), sv("C"), 0);
    assert_eq!(keys(&cache), vec![4, 1, 3]);
    assert_eq!(pairs(&released)[0], pair(3, "c"), "replace releases the old value");
    assert_eq!(
        cache.find_first(&|_, value| text(value) == "d").as_ref().map(text),
        Some("d"),
        "find_first"
    );
    assert_eq!(cache.take(&int_key(3)).as_ref().map(text), Some("C"));
    assert_eq!(cache.length(), 2);
    assert_ne!(pairs(&released)[0], pair(3, "C"), "take does not release");

    cache.add(int_key(3), sv("C"), 0);
    cache.remove(&int_key(4));
    assert_eq!(cache.length(), 2);
    assert_eq!(pairs(&released)[0], pair(4, "d"), "remove releases");

    assert!(cache.drop_oldest());
    assert_eq!(keys(&cache), vec![3]);
    assert!(cache.drop_oldest());
    assert!(!cache.drop_oldest(), "drop_oldest");
}

#[test]
fn byte_capacity_pinned_entries_and_clear_release() {
    let _guard = setup();
    let pinned = Arc::new(AtomicI64::new(10));
    let evictable_pinned = Arc::clone(&pinned);
    let cache = cache_with(
        mem::cache("test.lru.pin"),
        100,
        CacheOptions {
            byte_capacity: 10,
            evictable: Some(Arc::new(move |key: &Key, _: &Value| ikey(key) as i64 != evictable_pinned.load(Ordering::Relaxed))),
            ..CacheOptions::default()
        },
    );
    cache.add(int_key(10), sv("p"), 4);
    cache.add(int_key(11), sv("q"), 4);
    cache.add(int_key(12), sv("r"), 4);
    assert_eq!(keys(&cache), vec![12, 10]);
    assert_eq!(cache.bytes(), 8, "byte eviction skips the pinned entry (it moves to newest)");

    cache.add(int_key(13), sv("big"), 11);
    assert!(cache.peek(&int_key(13)).is_none());
    assert_eq!(cache.bytes(), 4);
    assert_eq!(keys(&cache), vec![10], "oversize entry leaves at once after evicting what it can");

    pinned.store(-1, Ordering::Relaxed);
    cache.add(int_key(14), sv("s"), 8);
    assert_eq!(keys(&cache), vec![14]);
    assert_eq!(cache.bytes(), 8, "unpinned entry evicts");

    let count = Arc::new(AtomicUsize::new(0));
    let counter = Arc::clone(&count);
    let cache = cache_with(
        mem::cache("test.lru.count"),
        8,
        CacheOptions {
            release: Some(Arc::new(move |_: &Key, _: &Value| {
                counter.fetch_add(1, Ordering::Relaxed);
            })),
            ..CacheOptions::default()
        },
    );
    for i in 1..=20 {
        cache.add(int_key(i), Arc::new(i), 0);
    }
    cache.clear();
    assert_eq!(count.load(Ordering::Relaxed), 20);
    assert_eq!(cache.length(), 0);
    assert_eq!(cache.bytes(), 0, "clear releases everything");
}

#[test]
fn identity_list_recency_release_copy_and_clear() {
    let _guard = setup();
    let evicted: Log = Arc::default();
    let log = Arc::clone(&evicted);
    let identity = IdentityList::with_options(
        mem::cache("test.lru.identity"),
        2,
        Arc::new(array_equal),
        IdentityListOptions {
            release: Some(Arc::new(move |key: &Key, value: &Value| {
                log.lock().unwrap().insert(0, (array_key_value(key), text(value).to_owned()));
            })),
            ..IdentityListOptions::default()
        },
    );
    let a = kv(1);
    let b = kv(2);
    let c = kv(3);
    identity.add(a.clone(), sv("a"), 0);
    identity.add(b.clone(), sv("b"), 0);
    assert!(
        identity.find(&a).as_ref().map(text) == Some("a") && identity.peek(&b).as_ref().map(text) == Some("b"),
        "identity list finds typed physical keys"
    );
    identity.add(c.clone(), sv("c"), 0);
    assert!(
        identity.peek(&b).is_none()
            && identity.peek(&a).as_ref().map(text) == Some("a")
            && pairs(&evicted) == vec![pair(2, "b")],
        "identity list touches hits and releases the least recent entry"
    );
    identity.add(a.clone(), sv("updated"), 0);
    assert!(
        pairs(&evicted) == vec![pair(1, "a"), pair(2, "b")] && identity.length() == 2,
        "identity list replacement releases once and stays bounded"
    );
    identity.clear();
    assert!(identity.length() == 0 && pairs(&evicted).len() == 4, "identity list clear releases its entries");

    identity.add(a.clone(), sv("shared"), 0);
    let copied_releases = Arc::new(AtomicUsize::new(0));
    let counter = Arc::clone(&copied_releases);
    let copy = identity.copy(Arc::new(move |_: &Key, _: &Value| {
        counter.fetch_add(1, Ordering::Relaxed);
    }));
    copy.clear();
    assert!(
        copied_releases.load(Ordering::Relaxed) == 1
            && pairs(&evicted).len() == 4
            && identity.peek(&a).as_ref().map(text) == Some("shared"),
        "identity copy uses its explicit release policy without changing the source"
    );
    identity.clear();
    assert_eq!(pairs(&evicted).len(), 5, "identity source keeps its original release policy");
}

#[test]
fn steady_state_hits_do_not_allocate() {
    let _guard = setup();
    let cache = cache_with(mem::cache("test.lru.alloc"), 64, CacheOptions::default());
    let keys: Vec<_> = (0..64).map(int_key).collect();
    for (index, key) in keys.iter().enumerate() {
        cache.add(key.clone(), Arc::new(index as i32), 0);
    }
    for round in 0..1000 {
        assert!(cache.find_opt(&keys[round & 63]).is_some());
    }
    let before = mem_sys::alloc_bytes_total();
    for round in 0..1000 {
        let index = (round * 7) & 63;
        let value = cache.find_opt(&keys[index]).unwrap();
        assert_eq!(value.downcast_ref::<i32>(), Some(&(index as i32)));
    }
    let allocated = mem_sys::alloc_bytes_total() - before;
    assert_eq!(allocated, 0, "test.lru.alloc steady-state hits allocated Rust heap bytes");
}

#[test]
fn owner_charges_eviction_clear_copies_and_resizes() {
    let _guard = setup();
    let owner = mem::cache("test.lru");
    let charged = cache_with(
        owner.clone(),
        8,
        CacheOptions { byte_capacity: 10, ..CacheOptions::default() },
    );
    charged.add(int_key(1), sv("a"), 4);
    charged.add(int_key(2), sv("b"), 4);
    assert_eq!(owner.bytes(), 8, "owner tracks cache bytes");
    charged.add(int_key(3), sv("c"), 4);
    assert!(owner.bytes() == 8 && charged.bytes() == 8, "eviction releases the owner charge");
    charged.clear();
    assert_eq!(owner.bytes(), 0, "clear releases the owner charge");

    // A copy shares its source's values: no second charge, no evictor, and dropping it changes nothing.
    charged.add(int_key(1), sv("a"), 4);
    let attributed = mem::attributed();
    let evictors = mem::evictor_count();
    let copied = charged.copy(noop_release());
    assert_eq!(mem::attributed(), attributed, "a copy charges nothing");
    assert_eq!(mem::evictor_count(), evictors, "a copy registers no evictor");
    copied.clear();
    assert_eq!(mem::attributed(), attributed, "clearing a copy releases nothing");
    drop(copied);
    assert!(mem::attributed() == attributed && mem::evictor_count() == evictors, "a dropped copy leaves no trace");

    let list_owner = mem::cache("test.lru.list");
    let listed = IdentityList::new(list_owner.clone(), 4, Arc::new(array_equal));
    let key = kv(0);
    listed.add(key.clone(), sv("a"), 5);
    let attributed = mem::attributed();
    let evictors = mem::evictor_count();
    let list_copy = listed.copy(noop_release());
    list_copy.clear();
    assert!(
        mem::attributed() == attributed && mem::evictor_count() == evictors,
        "nor does an identity list's"
    );
    assert_eq!(list_owner.bytes(), 5, "the source still holds its charge");

    // The same value added again changes its size, it does not leave.
    let released = Arc::new(AtomicUsize::new(0));
    let counter = Arc::clone(&released);
    let resized = cache_with(
        owner.clone(),
        8,
        CacheOptions {
            release: Some(Arc::new(move |_: &Key, _: &Value| {
                counter.fetch_add(1, Ordering::Relaxed);
            })),
            ..CacheOptions::default()
        },
    );
    let value: Value = sv("kept");
    resized.add(int_key(1), Arc::clone(&value), 1);
    let before = owner.bytes();
    resized.add(int_key(1), Arc::clone(&value), 9);
    assert!(
        released.load(Ordering::Relaxed) == 0
            && resized.peek(&int_key(1)).as_ref().map(text) == Some("kept"),
        "re-adding a value does not release it"
    );
    assert_eq!(owner.bytes(), before + 8, "re-adding a value charges the difference");
    resized.add(int_key(1), sv("other"), 1);
    assert_eq!(released.load(Ordering::Relaxed), 1, "a different value does");
    resized.clear();
}

#[test]
fn table_dies_with_its_evictor_and_its_bytes() {
    let _guard = setup();
    let dying = mem::cache("test.lru.dying");
    {
        let table = cache_with(dying.clone(), 4, CacheOptions::default());
        table.add(int_key(1), sv("x"), 64);
        assert!(dying.bytes() == 64 && mem::evictor_count() == 1, "a table charges and registers");
    }
    mem::report();
    mem::report();
    assert_eq!(mem::evictor_count(), 0, "a dead table's evictor leaves");
    assert_eq!(dying.bytes(), 0, "a dead table's bytes leave");
}

// The retreat follows a table's own rule: Home, Locked, Never and the thread boundary.
fn budgeted(name: &str, shed: Option<Shed>) -> (Owner, Cache) {
    mem::reset();
    let owner = mem::cache(name);
    let options = CacheOptions { shed: shed.unwrap_or_default(), ..CacheOptions::default() };
    let table = cache_with(owner.clone(), 4, options);
    table.add(int_key(1), sv("x"), 64);
    mem::set_budget(1);
    (owner, table)
}

fn usage() -> Arc<dyn Fn() -> i64 + Send + Sync> {
    Arc::new(|| 1 + mem::attributed())
}

fn retreat(usage: &Arc<dyn Fn() -> i64 + Send + Sync>) -> mem::Verdict {
    mem::retreat_with(mem::RetreatOptions { usage: Some(Arc::clone(usage)), ..mem::RetreatOptions::default() })
}

#[test]
fn home_table_is_dropped_from_its_domain() {
    let _guard = setup();
    let usage = usage();
    let (owner, table) = budgeted("test.lru.home", None);
    retreat(&usage);
    assert!(table.length() == 0 && owner.bytes() == 0, "Home: dropped from its domain");
}

#[test]
fn home_table_is_never_dropped_from_another_thread() {
    let _guard = setup();
    let usage = usage();
    let (owner, table) = budgeted("test.lru.away", None);
    let other_usage = Arc::clone(&usage);
    std::thread::spawn(move || {
        retreat(&other_usage);
    })
    .join()
    .unwrap();
    assert!(table.length() == 1 && owner.bytes() == 64, "Home: never from another domain");
}

#[test]
fn locked_table_waits_for_its_lock_then_drops() {
    let _guard = setup();
    let usage = usage();
    let lock = Arc::new(Mutex::new(()));
    let (owner, table) = budgeted("test.lru.locked", Some(Shed::Locked(Arc::clone(&lock))));
    let held = lock.lock().unwrap();
    retreat(&usage);
    assert_eq!(table.length(), 1, "Locked: skipped while its user holds the lock");
    drop(held);
    retreat(&usage);
    assert!(table.length() == 0 && owner.bytes() == 0, "Locked: dropped once free");
    // The lock is given back.
    assert!(lock.try_lock().is_ok(), "Locked: the lock given back");
}

#[test]
fn never_table_is_not_dropped() {
    let _guard = setup();
    let usage = usage();
    let (owner, table) = budgeted("test.lru.never", Some(Shed::Never));
    retreat(&usage);
    assert!(table.length() == 1 && owner.bytes() == 64, "Never: not dropped");
}

#[test]
fn memo_is_a_bounded_typed_map() {
    let _guard = setup();
    let memo: Memo<String, i32> = Memo::new("test.memo", 2);
    memo.insert("a".into(), 1);
    memo.insert("b".into(), 2);
    assert_eq!(memo.get(&"a".into()).as_deref(), Some(&1));
    memo.insert("c".into(), 3);
    let kept = ["a", "b", "c"].iter().filter(|k| memo.get(&k.to_string()).is_some()).count();
    assert_eq!(kept, 2, "capacity bounds the entries");
    assert_eq!(memo.get(&"c".into()).as_deref(), Some(&3));
    memo.insert("c".into(), 4);
    assert_eq!(memo.get(&"c".into()).as_deref(), Some(&4), "insert replaces");
    memo.remove_where(|v| *v == 4);
    assert!(memo.get(&"c".into()).is_none());
}
