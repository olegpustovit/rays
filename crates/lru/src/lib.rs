use std::any::Any;
use std::collections::{HashMap, VecDeque};
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, Mutex, MutexGuard, Weak};
use std::thread::ThreadId;

use mem::{AccountedSize, EvictorOptions};
pub use mem::{Owner, Pool};

pub fn cache(name: &str) -> Owner {
    mem::cache(name)
}

pub fn cache_in(pool: Pool, name: &str) -> Owner {
    mem::cache_in(pool, name)
}

pub fn plan(name: &str) -> Owner {
    mem::plan(name)
}

pub fn plan_in(pool: Pool, name: &str) -> Owner {
    mem::plan_in(pool, name)
}

pub fn document(name: &str) -> Owner {
    mem::document(name)
}

pub fn document_in(pool: Pool, name: &str) -> Owner {
    mem::document_in(pool, name)
}

pub type Value = Arc<dyn Any + Send + Sync>;
pub type KeyLookupEqual = dyn Fn(&Key, &(dyn Any + Send + Sync)) -> bool + Send + Sync + 'static;
pub type Release = Arc<dyn Fn(&Key, &Value) + Send + Sync + 'static>;
pub type Evictable = Arc<dyn Fn(&Key, &Value) -> bool + Send + Sync + 'static>;

#[derive(Clone)]
pub struct Key {
    hash: u64,
    value: Value,
}

impl Key {
    pub fn new(hash: u64, value: Value) -> Self {
        Self { hash, value }
    }

    pub fn from_value(value: Value) -> Self {
        Self::new(0, value)
    }

    pub fn hash(&self) -> u64 {
        self.hash
    }

    pub fn as_any(&self) -> &(dyn Any + Send + Sync) {
        self.value.as_ref()
    }
    pub fn value_arc(&self) -> Value {
        Arc::clone(&self.value)
    }

    pub fn same_identity(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.value, &other.value)
    }
}

#[derive(Clone, Default)]
pub enum Shed {
    #[default]
    Home,
    Locked(Arc<Mutex<()>>),
    Never,
}

pub struct CacheOptions {
    pub shed: Shed,
    pub rebuild: f64,
    pub charges: bool,
    pub byte_capacity: i64,
    pub evictable: Option<Evictable>,
    pub release: Option<Release>,
}

impl Default for CacheOptions {
    fn default() -> Self {
        Self {
            shed: Shed::Home,
            rebuild: 1.0,
            charges: true,
            byte_capacity: i64::MAX,
            evictable: None,
            release: None,
        }
    }
}

struct Entry {
    key: Key,
    value: Value,
    size: i64,
    touched: bool,
    newer: usize,
    older: usize,
}
fn initial_capacity(capacity: i64) -> usize {
    usize::try_from(capacity.max(0).min(1024)).expect("initial capacity is capped")
}
fn nonnegative_bytes(bytes: i64) -> usize {
    usize::try_from(bytes.max(0)).unwrap_or(usize::MAX)
}

struct CacheState {
    capacity: i64,
    byte_capacity: i64,
    owner: Owner,
    charges: bool,
    lookup_equal: Arc<KeyLookupEqual>,
    evictable: Evictable,
    release: Release,
    buckets: HashMap<u64, Vec<usize>>,
    entries: Vec<Option<Entry>>,
    free: Vec<usize>,
    oldest: Option<usize>,
    length: usize,
    charged: Arc<AtomicI64>,
    last_use: i64,
}

impl CacheState {
    fn lookup(&self, hash: u64, key: &(dyn Any + Send + Sync)) -> Option<usize> {
        self.lookup_with(hash, &|stored| (self.lookup_equal)(stored, key), false)
    }

    fn lookup_with(&self, hash: u64, matches: &dyn Fn(&Key) -> bool, recent_first: bool) -> Option<usize> {
        if recent_first {
            let mut index = self.entries[self.oldest?].as_ref().expect("oldest cache entry").older;
            for _ in 0..self.length {
                let entry = self.entries[index].as_ref().expect("present cache entry");
                if entry.key.hash == hash && matches(&entry.key) { return Some(index); }
                index = entry.older;
            }
            return None;
        }
        self.buckets.get(&hash)?.iter().copied().find(|index| {
            matches(&self.entries[*index].as_ref().expect("present cache entry").key)
        })
    }

    fn add_charge(&mut self, size: i64) {
        self.charged.fetch_add(size, Ordering::Relaxed);
        if self.charges {
            mem::charge(&self.owner, size);
        }
    }

    fn remove_charge(&mut self, size: i64) {
        self.charged.fetch_sub(size, Ordering::Relaxed);
        if self.charges {
            mem::release(&self.owner, size);
        }
    }

    fn unlink(&mut self, index: usize) {
        let entry = self.entries[index].as_ref().expect("linked cache entry");
        if entry.newer == index {
            self.oldest = None;
            return;
        }
        let (newer, older) = (entry.newer, entry.older);
        self.entries[older].as_mut().expect("linked older entry").newer = newer;
        self.entries[newer].as_mut().expect("linked newer entry").older = older;
        if self.oldest == Some(index) {
            self.oldest = Some(newer);
        }
    }

    fn link_newest(&mut self, index: usize) {
        let Some(oldest) = self.oldest else {
            let entry = self.entries[index].as_mut().expect("new cache entry");
            entry.newer = index;
            entry.older = index;
            self.oldest = Some(index);
            return;
        };
        let newest = self.entries[oldest].as_ref().expect("oldest entry").older;
        {
            let entry = self.entries[index].as_mut().expect("new cache entry");
            entry.older = newest;
            entry.newer = oldest;
        }
        self.entries[newest].as_mut().expect("newest entry").newer = index;
        self.entries[oldest].as_mut().expect("oldest entry").older = index;
    }

    fn move_newest(&mut self, index: usize) {
        let Some(oldest) = self.oldest else {
            return;
        };
        let newest = self.entries[oldest].as_ref().expect("oldest entry").older;
        if newest != index {
            self.unlink(index);
            self.link_newest(index);
        }
    }

    fn insert(&mut self, key: Key, value: Value, size: i64, touched: bool) {
        let index = self.free.pop().unwrap_or_else(|| {
            self.entries.push(None);
            self.entries.len() - 1
        });
        self.entries[index] = Some(Entry {
            key: key.clone(),
            value,
            size,
            touched,
            newer: index,
            older: index,
        });
        self.buckets.entry(key.hash).or_default().push(index);
        self.length += 1;
        self.link_newest(index);
        self.add_charge(size);
    }

    fn remove_index(&mut self, index: usize) -> Entry {
        self.unlink(index);
        let entry = self.entries[index].take().expect("present cache entry");
        let hash = entry.key.hash;
        let empty = {
            let bucket = self.buckets.get_mut(&hash).expect("cache hash bucket");
            let position = bucket.iter().position(|candidate| *candidate == index).expect("cache bucket index");
            bucket.remove(position);
            bucket.is_empty()
        };
        if empty {
            self.buckets.remove(&hash);
        }
        self.length -= 1;
        self.free.push(index);
        self.remove_charge(entry.size);
        entry
    }

    fn over(&self) -> bool {
        i64::try_from(self.length).unwrap_or(i64::MAX) > self.capacity
            || self.charged.load(Ordering::Relaxed) > self.byte_capacity
    }

    fn victim(&mut self) -> Option<usize> {
        let mut index = self.oldest?;
        let mut remaining = self.length;
        while remaining > 0 {
            remaining -= 1;
            let entry = self.entries[index].as_ref().expect("linked cache entry");
            let next = entry.newer;
            if !entry.touched && (self.evictable)(&entry.key, &entry.value) {
                return Some(index);
            }
            self.entries[index].as_mut().expect("linked cache entry").touched = false;
            self.move_newest(index);
            if next == index {
                return None;
            }
            index = next;
        }
        None
    }

    fn evict_to_limit(&mut self, removed: &mut Vec<Entry>) {
        while self.over() {
            let Some(index) = self.victim() else {
                break;
            };
            removed.push(self.remove_index(index));
        }
    }

    fn drop_oldest(&mut self) -> Option<Entry> {
        let index = self.victim()?;
        Some(self.remove_index(index))
    }

    fn order(&self) -> Vec<usize> {
        let Some(oldest) = self.oldest else {
            return Vec::new();
        };
        let mut order = Vec::with_capacity(self.length);
        let mut index = oldest;
        loop {
            order.push(index);
            index = self.entries[index].as_ref().expect("linked cache entry").newer;
            if index == oldest {
                return order;
            }
        }
    }

    fn clear(&mut self) -> Vec<Entry> {
        let mut removed = Vec::with_capacity(self.length);
        while let Some(index) = self.oldest {
            removed.push(self.remove_index(index));
        }
        removed
    }

    fn owned_bytes(&self) -> usize {
        self.entries
            .iter()
            .filter_map(Option::as_ref)
            .map(|entry| nonnegative_bytes(entry.size))
            .fold(0usize, usize::saturating_add)
    }
}

struct IdentityState {
    capacity: i64,
    owner: Owner,
    lookup_equal: Arc<KeyLookupEqual>,
    release: Release,
    entries: VecDeque<Entry>,
    charged: Arc<AtomicI64>,
    last_use: i64,
    charges: bool,
}

impl IdentityState {
    fn add_charge(&mut self, size: i64) {
        self.charged.fetch_add(size, Ordering::Relaxed);
        if self.charges {
            mem::charge(&self.owner, size);
        }
    }

    fn remove_charge(&mut self, size: i64) {
        self.charged.fetch_sub(size, Ordering::Relaxed);
        if self.charges {
            mem::release(&self.owner, size);
        }
    }

    fn lookup(&self, key: &(dyn Any + Send + Sync)) -> Option<usize> {
        self.entries
            .iter()
            .position(|entry| (self.lookup_equal)(&entry.key, key))
    }

    fn remove_index(&mut self, index: usize) -> Entry {
        let entry = self.entries.remove(index).expect("present identity-list entry");
        self.remove_charge(entry.size);
        entry
    }

    fn drop_oldest(&mut self) -> Option<Entry> {
        let entry = self.entries.pop_back()?;
        self.remove_charge(entry.size);
        Some(entry)
    }

    fn clear(&mut self) -> Vec<Entry> {
        let mut removed = Vec::with_capacity(self.entries.len());
        while let Some(entry) = self.entries.pop_front() {
            self.remove_charge(entry.size);
            removed.push(entry);
        }
        removed
    }

    fn owned_bytes(&self) -> usize {
        self.entries
            .iter()
            .map(|entry| nonnegative_bytes(entry.size))
            .fold(0usize, usize::saturating_add)
    }
}

enum TableState {
    Cache(CacheState),
    Identity(IdentityState),
}

impl TableState {
    fn owned_bytes(&self) -> usize {
        match self {
            Self::Cache(state) => state.owned_bytes(),
            Self::Identity(state) => state.owned_bytes(),
        }
    }

    fn last_use(&self) -> i64 {
        match self {
            Self::Cache(state) => state.last_use,
            Self::Identity(state) => state.last_use,
        }
    }

    fn release(&self) -> Release {
        match self {
            Self::Cache(state) => Arc::clone(&state.release),
            Self::Identity(state) => Arc::clone(&state.release),
        }
    }

    fn drop_oldest(&mut self) -> Option<Entry> {
        match self {
            Self::Cache(state) => state.drop_oldest(),
            Self::Identity(state) => state.drop_oldest(),
        }
    }
}

struct SharedTable {
    state: Mutex<TableState>,
    owner: Owner,
    charged: Arc<AtomicI64>,
    charges: bool,
}

impl AccountedSize for SharedTable {
    // Entry payloads are type-erased; their caller-measured total is one explicit table aggregate.
    fn visit_allocations(&self, visitor: &mut mem::AllocationVisitor) {
        visitor.exclusive(lock_state(&self.state).owned_bytes());
    }
}

impl SharedTable {
    fn drop_oldest(&self) -> bool {
        let (entry, release) = {
            let mut state = lock_state(&self.state);
            let Some(entry) = state.drop_oldest() else {
                return false;
            };
            (entry, state.release())
        };
        release(&entry.key, &entry.value);
        true
    }
}

fn lock_state(state: &Mutex<TableState>) -> MutexGuard<'_, TableState> {
    state.lock().unwrap_or_else(std::sync::PoisonError::into_inner)
}

fn release_entries(release: &Release, entries: Vec<Entry>) {
    for entry in entries {
        release(&entry.key, &entry.value);
    }
}

fn register_table(state: &Arc<SharedTable>, shed: Shed, rebuild: f64) {
    let weak: Weak<SharedTable> = Arc::downgrade(state);
    let charged_for_walk = Arc::clone(&state.charged);
    let directly_charged = state.charges;
    mem::walk_table(
        &state.owner,
        state.clone(),
        Arc::new(move || !directly_charged || charged_for_walk.load(Ordering::Relaxed) == 0),
    );
    if matches!(&shed, Shed::Never) {
        return;
    }

    let home: ThreadId = std::thread::current().id();
    let held_bytes = Arc::clone(&state.charged);
    let held = Arc::new(move || held_bytes.load(Ordering::Relaxed));
    let weak_for_use = weak.clone();
    let last_use = Arc::new(move || {
        weak_for_use
            .upgrade()
            .map(|table| lock_state(&table.state).last_use())
            .unwrap_or(0)
    });
    let weak_for_live = weak.clone();
    let live = Arc::new(move || weak_for_live.strong_count() > 0);
    let owner = state.owner.clone();
    let charged = Arc::clone(&state.charged);
    let charges = state.charges;
    let forget = Arc::new(move || {
        let bytes = charged.swap(0, Ordering::Relaxed);
        if charges {
            mem::release(&owner, bytes);
        }
    });
    let shed = Arc::new(shed);
    let drop_entry = move || {
        let drop_one = || weak.upgrade().is_some_and(|table| table.drop_oldest());
        match &*shed {
            Shed::Home if std::thread::current().id() == home => drop_one(),
            Shed::Home | Shed::Never => false,
            Shed::Locked(lock) => match lock.try_lock() {
                Ok(_guard) => drop_one(),
                Err(std::sync::TryLockError::Poisoned(error)) => {
                    let _guard = error.into_inner();
                    drop_one()
                }
                Err(std::sync::TryLockError::WouldBlock) => false,
            },
        }
    };
    mem::register_evictor(
        &state.owner,
        EvictorOptions {
            rebuild,
            held: Some(held),
            last_use: Some(last_use),
            live: Some(live),
            forget: Some(forget),
        },
        Arc::new(drop_entry),
    );
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NotFound;

pub struct Cache {
    state: Arc<SharedTable>,
}

impl Cache {
    pub fn new(owner: Owner, capacity: i64, lookup_equal: Arc<KeyLookupEqual>) -> Self {
        Self::with_options(owner, capacity, lookup_equal, CacheOptions::default())
    }

    pub fn with_options(owner: Owner, capacity: i64, lookup_equal: Arc<KeyLookupEqual>, options: CacheOptions) -> Self {
        let charged = Arc::new(AtomicI64::new(0));
        let state = Arc::new(SharedTable {
            state: Mutex::new(TableState::Cache(CacheState {
                capacity: capacity.max(0),
                byte_capacity: options.byte_capacity,
                owner: owner.clone(),
                charges: options.charges,
                lookup_equal,
                evictable: options.evictable.unwrap_or_else(|| Arc::new(|_, _| true)),
                release: options.release.unwrap_or_else(|| Arc::new(|_, _| {})),
                buckets: HashMap::with_capacity(initial_capacity(capacity)),
                entries: Vec::new(),
                free: Vec::new(),
                oldest: None,
                length: 0,
                charged: Arc::clone(&charged),
                last_use: 0,
            })),
            owner,
            charged,
            charges: options.charges,
        });
        register_table(&state, options.shed, options.rebuild);
        Self { state }
    }

    pub fn find(&self, key: &Key) -> Result<Value, NotFound> {
        self.find_by(key.hash, key.as_any())
    }

    pub fn find_opt(&self, key: &Key) -> Option<Value> {
        self.find_opt_by(key.hash, key.as_any())
    }

    pub fn find_by(
        &self,
        hash: u64,
        key: &(dyn Any + Send + Sync),
    ) -> Result<Value, NotFound> {
        self.find_opt_by(hash, key).ok_or(NotFound)
    }

    pub fn find_opt_by(
        &self,
        hash: u64,
        key: &(dyn Any + Send + Sync),
    ) -> Option<Value> {
        let mut state = lock_state(&self.state.state);
        let TableState::Cache(cache) = &mut *state else {
            unreachable!("cache state")
        };
        let index = cache.lookup(hash, key)?;
        let entry = cache.entries[index].as_mut().expect("present cache entry");
        entry.touched = true;
        cache.last_use = mem::epoch();
        Some(Arc::clone(&entry.value))
    }

    /// The predicate is borrowed for this lookup; local keys need not be Send. `promote` moves
    /// the hit to newest and resolves equivalent keys newest first; otherwise the hit receives
    /// the normal second chance at eviction.
    pub fn find_with(&self, hash: u64, matches: &dyn Fn(&Key) -> bool, promote: bool) -> Option<Value> {
        let mut state = lock_state(&self.state.state);
        let TableState::Cache(cache) = &mut *state else { unreachable!("cache state") };
        let index = cache.lookup_with(hash, matches, promote)?;
        if promote { cache.move_newest(index); }
        cache.entries[index].as_mut().expect("present cache entry").touched = !promote;
        cache.last_use = mem::epoch();
        Some(Arc::clone(&cache.entries[index].as_ref().expect("present cache entry").value))
    }

    /// A borrowed-key lookup without changing recency.
    pub fn peek_with(&self, hash: u64, matches: &dyn Fn(&Key) -> bool, recent_first: bool) -> Option<Value> {
        let state = lock_state(&self.state.state);
        let TableState::Cache(cache) = &*state else { unreachable!("cache state") };
        let index = cache.lookup_with(hash, matches, recent_first)?;
        Some(Arc::clone(&cache.entries[index].as_ref().expect("present cache entry").value))
    }

    pub fn peek_by(
        &self,
        hash: u64,
        key: &(dyn Any + Send + Sync),
    ) -> Option<Value> {
        let state = lock_state(&self.state.state);
        let TableState::Cache(cache) = &*state else {
            unreachable!("cache state")
        };
        let index = cache.lookup(hash, key)?;
        Some(Arc::clone(&cache.entries[index].as_ref().expect("present cache entry").value))
    }

    pub fn peek(&self, key: &Key) -> Option<Value> {
        self.peek_by(key.hash, key.as_any())
    }

    pub fn add(&self, key: Key, value: Value, bytes: i64) {
        // Make preserves signed sizes; IdentityList.add clamps them to zero.
        let (removed, release) = {
            let mut table = lock_state(&self.state.state);
            let TableState::Cache(cache) = &mut *table else {
                unreachable!("cache state")
            };
            let mut removed = Vec::new();
            if let Some(index) = cache.lookup(key.hash, key.as_any()) {
                let same = Arc::ptr_eq(&cache.entries[index].as_ref().expect("present cache entry").value, &value);
                let old = cache.remove_index(index);
                if !same {
                    removed.push(old);
                }
            }
            cache.insert(key, value, bytes, false);
            cache.last_use = mem::epoch();
            cache.evict_to_limit(&mut removed);
            (removed, Arc::clone(&cache.release))
        };
        release_entries(&release, removed);
    }

    pub fn remove(&self, key: &Key) {
        let (removed, release) = {
            let mut table = lock_state(&self.state.state);
            let TableState::Cache(cache) = &mut *table else {
                unreachable!("cache state")
            };
            let Some(index) = cache.lookup(key.hash, key.as_any()) else {
                return;
            };
            (cache.remove_index(index), Arc::clone(&cache.release))
        };
        release(&removed.key, &removed.value);
    }
    pub fn remove_by(&self, hash: u64, key: &(dyn Any + Send + Sync)) {
        let (removed, release) = {
            let mut table = lock_state(&self.state.state);
            let TableState::Cache(cache) = &mut *table else {
                unreachable!("cache state")
            };
            let Some(index) = cache.lookup(hash, key) else {
                return;
            };
            (cache.remove_index(index), Arc::clone(&cache.release))
        };
        release(&removed.key, &removed.value);
    }

    pub fn take(&self, key: &Key) -> Option<Value> {
        self.take_by(key.hash, key.as_any())
    }

    pub fn take_by(&self, hash: u64, key: &(dyn Any + Send + Sync)) -> Option<Value> {
        let mut table = lock_state(&self.state.state);
        let TableState::Cache(cache) = &mut *table else {
            unreachable!("cache state")
        };
        let index = cache.lookup(hash, key)?;
        Some(cache.remove_index(index).value)
    }

    pub fn drop_oldest(&self) -> bool {
        self.state.drop_oldest()
    }

    pub fn find_first(&self, predicate: &dyn Fn(&Key, &Value) -> bool) -> Option<Value> {
        let table = lock_state(&self.state.state);
        let TableState::Cache(cache) = &*table else {
            unreachable!("cache state")
        };
        cache.order().into_iter().find_map(|index| {
            let entry = cache.entries[index].as_ref().expect("present cache entry");
            predicate(&entry.key, &entry.value).then(|| Arc::clone(&entry.value))
        })
    }

    fn snapshot(&self, recent_first: bool) -> Vec<(Key, Value)> {
        let state = lock_state(&self.state.state);
        let TableState::Cache(cache) = &*state else { unreachable!("cache state") };
        let mut entries = Vec::with_capacity(cache.length);
        let mut cursor = cache.oldest;
        if recent_first { cursor = cursor.map(|i| cache.entries[i].as_ref().expect("oldest cache entry").older); }
        for _ in 0..cache.length {
            let entry = cache.entries[cursor.expect("linked cache entry")].as_ref().expect("present cache entry");
            entries.push((entry.key.clone(), Arc::clone(&entry.value)));
            cursor = Some(if recent_first { entry.older } else { entry.newer });
        }
        entries
    }

    pub fn iter(&self, f: &mut dyn FnMut(&Key, &Value)) {
        for (key, value) in self.snapshot(false) { f(&key, &value); }
    }

    pub fn iter_recent(&self, f: &mut dyn FnMut(&Key, &Value)) {
        for (key, value) in self.snapshot(true) { f(&key, &value); }
    }

    pub fn head_recent(&self) -> Option<(Key, Value)> {
        let state = lock_state(&self.state.state);
        let TableState::Cache(cache) = &*state else { unreachable!("cache state") };
        let newest = cache.entries[cache.oldest?].as_ref()?.older;
        let entry = cache.entries[newest].as_ref()?;
        Some((entry.key.clone(), Arc::clone(&entry.value)))
    }

    /// Visit newest first; release rejected entries after unlocking. `keep` must not reenter
    /// this cache. Keeping every entry allocates nothing.
    pub fn retain_recent(&self, keep: &mut dyn FnMut(&Key, &Value) -> bool) {
        let (removed, release) = {
            let mut state = lock_state(&self.state.state);
            let TableState::Cache(cache) = &mut *state else { unreachable!("cache state") };
            let mut removed = Vec::new();
            let mut cursor = cache.oldest.map(|i| cache.entries[i].as_ref().expect("oldest cache entry").older);
            for _ in 0..cache.length {
                let index = cursor.expect("linked cache entry");
                let entry = cache.entries[index].as_ref().expect("present cache entry");
                cursor = Some(entry.older);
                if !keep(&entry.key, &entry.value) { removed.push(cache.remove_index(index)); }
            }
            (removed, Arc::clone(&cache.release))
        };
        release_entries(&release, removed);
    }

    pub fn copy(&self, release: Release) -> Self {
        let table = lock_state(&self.state.state);
        let TableState::Cache(source) = &*table else {
            unreachable!("cache state")
        };
        let mut result = CacheState {
            capacity: source.capacity,
            byte_capacity: source.byte_capacity,
            owner: source.owner.clone(),
            charges: false,
            lookup_equal: Arc::clone(&source.lookup_equal),
            evictable: Arc::clone(&source.evictable),
            release,
            buckets: HashMap::with_capacity(source.length),
            entries: Vec::with_capacity(source.length),
            free: Vec::new(),
            oldest: None,
            length: 0,
            charged: Arc::new(AtomicI64::new(0)),
            last_use: source.last_use,
        };
        for index in source.order() {
            let entry = source.entries[index].as_ref().expect("present cache entry");
            result.insert(entry.key.clone(), Arc::clone(&entry.value), entry.size, entry.touched);
        }
        let charged = Arc::clone(&result.charged);
        Self {
            state: Arc::new(SharedTable {
                state: Mutex::new(TableState::Cache(result)),
                owner: source.owner.clone(),
                charged,
                charges: false,
            }),
        }
    }

    pub fn length(&self) -> i64 {
        let table = lock_state(&self.state.state);
        let TableState::Cache(cache) = &*table else {
            unreachable!("cache state")
        };
        i64::try_from(cache.length).unwrap_or(i64::MAX)
    }

    pub fn bytes(&self) -> i64 {
        self.state.charged.load(Ordering::Relaxed)
    }

    pub fn clear(&self) {
        let (removed, release) = {
            let mut table = lock_state(&self.state.state);
            let TableState::Cache(cache) = &mut *table else {
                unreachable!("cache state")
            };
            (cache.clear(), Arc::clone(&cache.release))
        };
        release_entries(&release, removed);
    }
}

/// A typed, bounded map of shared values for process-wide memo tables: `Cache` keyed by `K: Hash + Eq`,
/// without the byte ledger (the bound is the entry count).
pub struct Memo<K, V> {
    cache: Cache,
    marker: std::marker::PhantomData<fn(K) -> V>,
}

fn hash_of<K: std::hash::Hash>(key: &K) -> u64 {
    use std::hash::Hasher;
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    key.hash(&mut hasher);
    hasher.finish()
}

impl<K: std::hash::Hash + Eq + Send + Sync + 'static, V: Send + Sync + 'static> Memo<K, V> {
    pub fn new(name: &str, capacity: i64) -> Self {
        let equal = |stored: &Key, probe: &(dyn Any + Send + Sync)| {
            matches!((stored.as_any().downcast_ref::<K>(), probe.downcast_ref::<K>()), (Some(a), Some(b)) if a == b)
        };
        let options = CacheOptions { charges: false, ..CacheOptions::default() };
        Self { cache: Cache::with_options(cache(name), capacity, Arc::new(equal), options), marker: std::marker::PhantomData }
    }

    /// The value under `key`, now the newest.
    pub fn get(&self, key: &K) -> Option<Arc<V>> {
        self.cache.find_opt_by(hash_of(key), key)?.downcast::<V>().ok()
    }

    /// Stores `value` under `key` (replacing any) and returns it shared.
    pub fn insert(&self, key: K, value: V) -> Arc<V> {
        let value = Arc::new(value);
        self.cache.add(Key::new(hash_of(&key), Arc::new(key)), value.clone(), 0);
        value
    }

    /// Drops every entry whose value satisfies `doomed`.
    pub fn remove_where(&self, doomed: impl Fn(&V) -> bool) {
        let mut keys = Vec::new();
        self.cache.iter(&mut |key, value| {
            if value.downcast_ref::<V>().is_some_and(&doomed) {
                keys.push(key.clone());
            }
        });
        for key in keys {
            self.cache.remove(&key);
        }
    }
}

impl Drop for Cache {
    fn drop(&mut self) {
        self.clear();
    }
}

pub struct IdentityListOptions {
    pub shed: Shed,
    pub rebuild: f64,
    pub release: Option<Release>,
}

impl Default for IdentityListOptions {
    fn default() -> Self {
        Self {
            shed: Shed::Home,
            rebuild: 1.0,
            release: None,
        }
    }
}

pub struct IdentityList {
    state: Arc<SharedTable>,
}

impl IdentityList {
    pub fn new(
        owner: Owner,
        capacity: i64,
        lookup_equal: Arc<KeyLookupEqual>,
    ) -> Self {
        Self::with_options(owner, capacity, lookup_equal, IdentityListOptions::default())
    }

    pub fn with_options(
        owner: Owner,
        capacity: i64,
        lookup_equal: Arc<KeyLookupEqual>,
        options: IdentityListOptions,
    ) -> Self {
        let charged = Arc::new(AtomicI64::new(0));
        let state = Arc::new(SharedTable {
            state: Mutex::new(TableState::Identity(IdentityState {
                capacity: capacity.max(0),
                owner: owner.clone(),
                lookup_equal,
                release: options.release.unwrap_or_else(|| Arc::new(|_, _| {})),
                entries: VecDeque::with_capacity(initial_capacity(capacity)),
                charged: Arc::clone(&charged),
                last_use: 0,
                charges: true,
            })),
            owner,
            charged,
            charges: true,
        });
        register_table(&state, options.shed, options.rebuild);
        Self { state }
    }

    pub fn find(&self, key: &Key) -> Option<Value> {
        self.find_by(key.as_any())
    }

    pub fn find_by(&self, key: &(dyn Any + Send + Sync)) -> Option<Value> {
        let mut table = lock_state(&self.state.state);
        let TableState::Identity(list) = &mut *table else {
            unreachable!("identity-list state")
        };
        let index = list.lookup(key)?;
        let entry = list.entries.remove(index).expect("present identity-list entry");
        list.entries.push_front(entry);
        list.last_use = mem::epoch();
        Some(Arc::clone(&list.entries.front().expect("moved identity-list entry").value))
    }

    pub fn find_opt_by(&self, key: &(dyn Any + Send + Sync)) -> Option<Value> {
        self.find_by(key)
    }

    pub fn peek_by(&self, key: &(dyn Any + Send + Sync)) -> Option<Value> {
        let table = lock_state(&self.state.state);
        let TableState::Identity(list) = &*table else {
            unreachable!("identity-list state")
        };
        list.lookup(key).map(|index| Arc::clone(&list.entries[index].value))
    }

    pub fn peek(&self, key: &Key) -> Option<Value> {
        self.peek_by(key.as_any())
    }

    pub fn add(&self, key: Key, value: Value, bytes: i64) {
        let (removed, release) = {
            let mut table = lock_state(&self.state.state);
            let TableState::Identity(list) = &mut *table else {
                unreachable!("identity-list state")
            };
            let mut removed = Vec::new();
            if let Some(index) = list.lookup(key.as_any()) {
                removed.push(list.remove_index(index));
            }
            let size = bytes.max(0);
            list.add_charge(size);
            list.last_use = mem::epoch();
            list.entries.push_front(Entry {
                key,
                value,
                size,
                touched: false,
                newer: 0,
                older: 0,
            });
            while i64::try_from(list.entries.len()).unwrap_or(i64::MAX) > list.capacity {
                removed.push(list.drop_oldest().expect("over-capacity identity list"));
            }
            (removed, Arc::clone(&list.release))
        };
        release_entries(&release, removed);
    }

    pub fn remove(&self, key: &Key) {
        let (removed, release) = {
            let mut table = lock_state(&self.state.state);
            let TableState::Identity(list) = &mut *table else {
                unreachable!("identity-list state")
            };
            let Some(index) = list.lookup(key.as_any()) else {
                return;
            };
            (list.remove_index(index), Arc::clone(&list.release))
        };
        release(&removed.key, &removed.value);
    }
    pub fn remove_by(&self, key: &(dyn Any + Send + Sync)) {
        let (removed, release) = {
            let mut table = lock_state(&self.state.state);
            let TableState::Identity(list) = &mut *table else {
                unreachable!("identity-list state")
            };
            let Some(index) = list.lookup(key) else {
                return;
            };
            (list.remove_index(index), Arc::clone(&list.release))
        };
        release(&removed.key, &removed.value);
    }

    pub fn head(&self) -> Option<(Key, Value)> {
        let table = lock_state(&self.state.state);
        let TableState::Identity(list) = &*table else {
            unreachable!("identity-list state")
        };
        list.entries
            .front()
            .map(|entry| (entry.key.clone(), Arc::clone(&entry.value)))
    }

    pub fn copy(&self, release: Release) -> Self {
        let table = lock_state(&self.state.state);
        let TableState::Identity(source) = &*table else {
            unreachable!("identity-list state")
        };
        let result = IdentityState {
            capacity: source.capacity,
            owner: source.owner.clone(),
            lookup_equal: Arc::clone(&source.lookup_equal),
            release,
            entries: source
                .entries
                .iter()
                .map(|entry| Entry {
                    key: entry.key.clone(),
                    value: Arc::clone(&entry.value),
                    size: entry.size,
                    touched: false,
                    newer: 0,
                    older: 0,
                })
                .collect(),
            charged: Arc::new(AtomicI64::new(source.charged.load(Ordering::Relaxed))),
            last_use: source.last_use,
            charges: false,
        };
        let charged = Arc::clone(&result.charged);
        Self {
            state: Arc::new(SharedTable {
                state: Mutex::new(TableState::Identity(result)),
                owner: source.owner.clone(),
                charged,
                charges: false,
            }),
        }
    }

    pub fn iter(&self, f: &mut dyn FnMut(&Key, &Value)) {
        let entries = {
            let table = lock_state(&self.state.state);
            let TableState::Identity(list) = &*table else {
                unreachable!("identity-list state")
            };
            list.entries
                .iter()
                .rev()
                .map(|entry| (entry.key.clone(), Arc::clone(&entry.value)))
                .collect::<Vec<_>>()
        };
        for (key, value) in entries {
            f(&key, &value);
        }
    }

    pub fn filter(&self, keep: &dyn Fn(&Key, &Value) -> bool) {
        let (removed, release) = {
            let mut table = lock_state(&self.state.state);
            let TableState::Identity(list) = &mut *table else {
                unreachable!("identity-list state")
            };
            let mut kept = VecDeque::with_capacity(list.entries.len());
            let mut removed = Vec::new();
            while let Some(entry) = list.entries.pop_front() {
                if keep(&entry.key, &entry.value) {
                    kept.push_back(entry);
                } else {
                    list.remove_charge(entry.size);
                    removed.push(entry);
                }
            }
            list.entries = kept;
            (removed, Arc::clone(&list.release))
        };
        release_entries(&release, removed);
    }

    pub fn length(&self) -> i64 {
        let table = lock_state(&self.state.state);
        let TableState::Identity(list) = &*table else {
            unreachable!("identity-list state")
        };
        i64::try_from(list.entries.len()).unwrap_or(i64::MAX)
    }

    pub fn clear(&self) {
        let (removed, release) = {
            let mut table = lock_state(&self.state.state);
            let TableState::Identity(list) = &mut *table else {
                unreachable!("identity-list state")
            };
            (list.clear(), Arc::clone(&list.release))
        };
        release_entries(&release, removed);
    }
}

impl Drop for IdentityList {
    fn drop(&mut self) {
        self.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::hash::{Hash, Hasher};
    use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};
    use std::sync::LazyLock;

    static TEST_LOCK: LazyLock<Mutex<()>> = LazyLock::new(|| Mutex::new(()));

    fn setup() -> MutexGuard<'static, ()> {
        let guard = TEST_LOCK.lock().unwrap();
        mem::reset();
        guard
    }

    fn int_hash(value: i32) -> u64 {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        value.hash(&mut hasher);
        hasher.finish()
    }

    fn int_key(value: i32) -> Key {
        Key::new(int_hash(value), Arc::new(value))
    }

    fn int_equal(left: &Key, right: &(dyn Any + Send + Sync)) -> bool {
        matches!((left.as_any().downcast_ref::<i32>(), right.downcast_ref::<i32>()), (Some(a), Some(b)) if a == b)
    }

    fn array_equal(left: &Key, right: &(dyn Any + Send + Sync)) -> bool {
        match (left.as_any().downcast_ref::<[i32; 1]>(), right.downcast_ref::<[i32; 1]>()) {
            (Some(a), Some(b)) => std::ptr::eq(a, b),
            _ => false,
        }
    }

    fn string_value(value: &str) -> Value {
        Arc::new(value.to_owned())
    }

    fn text(value: &Value) -> &str {
        value.downcast_ref::<String>().expect("string value")
    }

    fn keys(cache: &Cache) -> Vec<i32> {
        let mut result = Vec::new();
        let mut collect = |key: &Key, _: &Value| {
            result.push(*key.as_any().downcast_ref::<i32>().expect("integer key"));
        };
        cache.iter(&mut collect);
        result
    }

    fn key_array(value: i32) -> Key {
        Key::from_value(Arc::new([value]))
    }

    #[test]
    fn clock_order_copy_replace_take_and_remove() {
        let _guard = setup();
        let released = Arc::new(Mutex::new(Vec::new()));
        let cache = Cache::with_options(
            super::cache("test.lru.order"),
            3,
            Arc::new(int_equal),
            CacheOptions {
                release: Some({
                    let released = Arc::clone(&released);
                    Arc::new(move |key, value| {
                        released.lock().unwrap().push((
                            *key.as_any().downcast_ref::<i32>().unwrap(),
                            text(value).to_owned(),
                        ));
                    })
                }),
                ..CacheOptions::default()
            },
        );
        cache.add(int_key(1), string_value("a"), 0);
        cache.add(int_key(2), string_value("b"), 0);
        cache.add(int_key(3), string_value("c"), 0);
        assert_eq!(keys(&cache), vec![1, 2, 3]);
        assert_eq!(text(&cache.find_by(int_hash(1), &1).unwrap()), "a");
        assert_eq!(cache.bytes(), 0);
        assert_eq!(keys(&cache), vec![1, 2, 3]);
        assert!(cache.find_opt_by(int_hash(9), &9).is_none());
        assert!(cache.find_by(int_hash(9), &9).is_err());
        let wrong_type = Key::new(int_key(1).hash(), string_value("not an integer"));
        assert!(cache.find_opt(&wrong_type).is_none());
        assert_eq!(cache.peek_by(int_hash(2), &2).as_ref().map(text), Some("b"));
        assert_eq!(keys(&cache), vec![1, 2, 3]);

        let copied_releases = Arc::new(Mutex::new(Vec::new()));
        let copy = cache.copy({
            let copied_releases = Arc::clone(&copied_releases);
            Arc::new(move |key, value| {
                copied_releases.lock().unwrap().push((
                    *key.as_any().downcast_ref::<i32>().unwrap(),
                    text(value).to_owned(),
                ));
            })
        });
        copy.add(int_key(4), string_value("d"), 0);
        assert_eq!(keys(&copy), vec![3, 4, 1]);
        assert_eq!(copied_releases.lock().unwrap().as_slice(), &[(2, "b".to_owned())]);
        assert_eq!(keys(&cache), vec![1, 2, 3]);
        assert!(released.lock().unwrap().is_empty());

        cache.add(int_key(4), string_value("d"), 0);
        assert_eq!(keys(&cache), vec![3, 4, 1]);
        cache.add(int_key(3), string_value("C"), 0);
        assert_eq!(keys(&cache), vec![4, 1, 3]);
        assert_eq!(released.lock().unwrap().first(), Some(&(2, "b".to_owned())));
        assert_eq!(cache.find_first(&|_, value| text(value) == "d").as_ref().map(text), Some("d"));
        let taken = cache.take_by(int_hash(3), &3).unwrap();
        assert_eq!(text(&taken), "C");
        assert_eq!(cache.length(), 2);
        assert!(!released.lock().unwrap().iter().any(|(key, value)| *key == 3 && value == "C"));
        cache.add(int_key(3), string_value("C"), 0);
        cache.remove_by(int_hash(4), &4);
        assert_eq!(cache.length(), 2);
        assert_eq!(released.lock().unwrap().last(), Some(&(4, "d".to_owned())));
        assert!(cache.drop_oldest());
        assert!(cache.drop_oldest());
        assert!(!cache.drop_oldest());
    }

    #[test]
    fn default_byte_capacity_uses_native_int_max() {
        assert_eq!(CacheOptions::default().byte_capacity, i64::MAX);
    }

    #[test]
    fn byte_limits_and_pinned_entries() {
        let _guard = setup();
        let pinned = Arc::new(AtomicUsize::new(10));
        let owner = super::cache("test.lru.bytes");
        let cache = Cache::with_options(
            owner.clone(),
            100,
            Arc::new(int_equal),
            CacheOptions {
                byte_capacity: 10,
                evictable: Some({
                    let pinned = Arc::clone(&pinned);
                    Arc::new(move |key, _| *key.as_any().downcast_ref::<i32>().unwrap() != pinned.load(AtomicOrdering::Relaxed) as i32)
                }),
                ..CacheOptions::default()
            },
        );
        cache.add(int_key(10), string_value("p"), 4);
        cache.add(int_key(11), string_value("q"), 4);
        cache.add(int_key(12), string_value("r"), 4);
        assert_eq!(keys(&cache), vec![12, 10]);
        assert_eq!(cache.bytes(), 8);
        assert_eq!(owner.bytes(), 8);
        cache.add(int_key(13), string_value("big"), 11);
        assert!(cache.peek(&int_key(13)).is_none());
        assert_eq!(cache.bytes(), 4);
        assert_eq!(owner.bytes(), 4);
        assert_eq!(keys(&cache), vec![10]);
        pinned.store(0, AtomicOrdering::Relaxed);
        cache.add(int_key(14), string_value("s"), 8);
        assert_eq!(keys(&cache), vec![14]);
        assert_eq!(cache.bytes(), 8);
        assert_eq!(owner.bytes(), 8);
    }

    #[test]
    fn identity_list_recency_release_copy_and_clear() {
        let _guard = setup();
        let evicted = Arc::new(Mutex::new(Vec::new()));
        let list = IdentityList::with_options(
            super::cache("test.lru.identity"),
            2,
            Arc::new(array_equal),
            IdentityListOptions {
                release: Some({
                    let evicted = Arc::clone(&evicted);
                    Arc::new(move |key, value| {
                        evicted.lock().unwrap().push((
                            *key.as_any().downcast_ref::<[i32; 1]>().unwrap(),
                            text(value).to_owned(),
                        ));
                    })
                }),
                ..IdentityListOptions::default()
            },
        );
        let a = key_array(1);
        let b = key_array(2);
        let c = key_array(3);
        list.add(a.clone(), string_value("a"), 0);
        list.add(b.clone(), string_value("b"), 0);
        assert_eq!(text(&list.find_by(a.as_any()).unwrap()), "a");
        assert_eq!(text(&list.peek_by(b.as_any()).unwrap()), "b");
        let mut order = Vec::new();
        let mut collect = |key: &Key, _: &Value| order.push(*key.as_any().downcast_ref::<[i32; 1]>().unwrap());
        list.iter(&mut collect);
        assert_eq!(order, vec![[2], [1]]);
        list.add(c.clone(), string_value("c"), 0);
        assert!(list.peek(&b).is_none());
        assert!(list.peek(&a).is_some());
        assert_eq!(evicted.lock().unwrap().as_slice(), &[([2], "b".to_owned())]);
        list.add(a.clone(), string_value("updated"), 0);
        assert_eq!(evicted.lock().unwrap().last(), Some(&([1], "a".to_owned())));
        assert_eq!(list.length(), 2);
        let head = list.head().unwrap();
        assert_eq!(text(&head.1), "updated");
        list.clear();
        assert_eq!(list.length(), 0);
        assert_eq!(evicted.lock().unwrap().len(), 4);

        list.add(a.clone(), string_value("shared"), 0);
        let copied = list.copy(Arc::new(|_, _| {}));
        copied.clear();
        assert_eq!(list.peek(&a).as_ref().map(text), Some("shared"));
        list.clear();
        list.add(a.clone(), string_value("filter-a"), 0);
        list.add(b.clone(), string_value("filter-b"), 0);
        list.filter(&|key, _| key.same_identity(&a));
        assert!(list.peek(&a).is_some());
        assert!(list.peek(&b).is_none());
        list.remove_by(a.as_any());
        assert_eq!(list.length(), 0);
    }

    #[test]
    fn accounting_copy_identity_and_release_once() {
        let _guard = setup();
        let owner = super::cache("test.lru.accounting");
        let released = Arc::new(AtomicUsize::new(0));
        let cache = Cache::with_options(
            owner.clone(),
            8,
            Arc::new(int_equal),
            CacheOptions {
                release: Some({
                    let released = Arc::clone(&released);
                    Arc::new(move |_, _| { released.fetch_add(1, AtomicOrdering::Relaxed); })
                }),
                ..CacheOptions::default()
            },
        );
        let value = string_value("kept");
        cache.add(int_key(1), Arc::clone(&value), 1);
        let before = owner.bytes();
        cache.add(int_key(1), Arc::clone(&value), 9);
        assert_eq!(released.load(AtomicOrdering::Relaxed), 0);
        assert_eq!(owner.bytes(), before + 8);
        cache.add(int_key(1), string_value("other"), 1);
        assert_eq!(released.load(AtomicOrdering::Relaxed), 1);

        let attributed = mem::attributed();
        let evictors = mem::evictor_count();
        let copy = cache.copy(Arc::new(|_, _| {}));
        assert_eq!(mem::attributed(), attributed);
        assert_eq!(mem::evictor_count(), evictors);
        copy.clear();
        assert_eq!(mem::attributed(), attributed);
        assert_eq!(owner.bytes(), 1);
        cache.clear();
        assert_eq!(owner.bytes(), 0);
        assert_eq!(released.load(AtomicOrdering::Relaxed), 2);

        let list_owner = super::cache("test.lru.list_accounting");
        let list = IdentityList::new(list_owner.clone(), 4, Arc::new(array_equal));
        let key = key_array(10);
        list.add(key, string_value("a"), 5);
        assert_eq!(list_owner.bytes(), 5);
        let list_copy = list.copy(Arc::new(|_, _| {}));
        let attributed = mem::attributed();
        let evictors = mem::evictor_count();
        list_copy.clear();
        assert_eq!(list_owner.bytes(), 5);
        assert_eq!(mem::attributed(), attributed);
        assert_eq!(mem::evictor_count(), evictors);
        list.clear();
        assert_eq!(list_owner.bytes(), 0);

        let count_owner = super::cache("test.lru.count_only");
        let count_only = Cache::new(count_owner.clone(), 4, Arc::new(int_equal));
        count_only.add(int_key(1), Arc::new(String::from("unmeasured")), 0);
        mem::report();
        assert_eq!(count_only.bytes(), 0);
        assert_eq!(count_owner.bytes(), 0);

        let walked_owner = super::cache("test.lru.walked");
        let walked = Cache::with_options(
            walked_owner.clone(), 4, Arc::new(int_equal),
            CacheOptions { charges: false, ..CacheOptions::default() },
        );
        let value = Arc::new(String::with_capacity(32));
        let retained = value.capacity() as i64;
        walked.add(int_key(1), value, retained);
        assert_eq!(walked_owner.bytes(), 0);
        mem::report();
        assert_eq!(walked_owner.bytes(), retained);
        walked.clear();
        mem::report();
        assert_eq!(walked_owner.bytes(), 0);
    }

    #[test]
    fn clear_releases_every_entry_once() {
        let _guard = setup();
        let owner = super::cache("test.lru.clear");
        let released = Arc::new(AtomicUsize::new(0));
        let cache = Cache::with_options(
            owner.clone(),
            8,
            Arc::new(int_equal),
            CacheOptions {
                release: Some({
                    let released = Arc::clone(&released);
                    Arc::new(move |_, _| { released.fetch_add(1, AtomicOrdering::Relaxed); })
                }),
                ..CacheOptions::default()
            },
        );
        for key in 1..=20 {
            cache.add(int_key(key), Arc::new(key), 1);
        }
        assert_eq!(cache.length(), 8);
        assert_eq!(owner.bytes(), 8);
        cache.clear();
        assert_eq!(released.load(AtomicOrdering::Relaxed), 20);
        assert_eq!(cache.length(), 0);
        assert_eq!(owner.bytes(), 0);
        cache.clear();
        assert_eq!(released.load(AtomicOrdering::Relaxed), 20);
    }

    #[test]
    fn negative_byte_sizes_follow_source_contract() {
        let _guard = setup();
        let owner = super::cache("test.lru.negative");
        let cache = Cache::new(owner.clone(), 4, Arc::new(int_equal));
        cache.add(int_key(1), string_value("negative"), -7);
        assert_eq!(cache.bytes(), -7);
        assert_eq!(owner.bytes(), 0);
        cache.remove(&int_key(1));
        assert_eq!(cache.bytes(), 0);
        assert_eq!(owner.bytes(), 0);

        let list_owner = super::cache("test.lru.identity_negative");
        let list = IdentityList::new(list_owner.clone(), 4, Arc::new(array_equal));
        list.add(key_array(1), string_value("negative"), -7);
        assert_eq!(list.length(), 1);
        assert_eq!(list_owner.bytes(), 0);
        list.clear();
        assert_eq!(list_owner.bytes(), 0);
    }

    #[test]
    fn retreat_shed_policies_and_dead_table_cleanup() {
        let _guard = setup();
        let usage: Arc<dyn Fn() -> i64 + Send + Sync> = Arc::new(|| 1 + mem::attributed());
        let before = mem::evictor_count();
        let dead_owner = super::cache("test.lru.dead");
        {
            let dying = Cache::new(dead_owner.clone(), 4, Arc::new(int_equal));
            dying.add(int_key(1), string_value("dead"), 64);
            assert_eq!(mem::evictor_count(), before + 1);
        }
        assert_eq!(dead_owner.bytes(), 0);
        mem::report();
        assert_eq!(mem::evictor_count(), before);


        let home_owner = super::cache("test.lru.home");
        let home = Cache::new(home_owner.clone(), 4, Arc::new(int_equal));
        home.add(int_key(1), string_value("home"), 64);
        mem::set_budget(1);
        mem::retreat_with(mem::RetreatOptions { usage: Some(Arc::clone(&usage)), ..mem::RetreatOptions::default() });
        assert_eq!(home.length(), 0);
        assert_eq!(home_owner.bytes(), 0);

        mem::reset();
        let away_owner = super::cache("test.lru.away");
        let away = Arc::new(Cache::new(away_owner.clone(), 4, Arc::new(int_equal)));
        away.add(int_key(1), string_value("away"), 64);
        mem::set_budget(1);
        std::thread::spawn(|| {
            let usage: Arc<dyn Fn() -> i64 + Send + Sync> = Arc::new(|| 1 + mem::attributed());
            mem::retreat_with(mem::RetreatOptions { usage: Some(usage), ..mem::RetreatOptions::default() });
        }).join().unwrap();
        assert_eq!(away.length(), 1);
        assert_eq!(away_owner.bytes(), 64);

        mem::reset();
        let lock = Arc::new(Mutex::new(()));
        let locked_owner = super::cache("test.lru.locked");
        let locked = Cache::with_options(
            locked_owner.clone(),
            4,
            Arc::new(int_equal),
            CacheOptions { shed: Shed::Locked(Arc::clone(&lock)), ..CacheOptions::default() },
        );
        locked.add(int_key(1), string_value("locked"), 64);
        mem::set_budget(1);
        let held = lock.lock().unwrap();
        mem::retreat_with(mem::RetreatOptions { usage: Some(Arc::clone(&usage)), ..mem::RetreatOptions::default() });
        assert_eq!(locked.length(), 1);
        drop(held);
        let probe = lock.try_lock().unwrap();
        drop(probe);
        mem::retreat_with(mem::RetreatOptions { usage: Some(Arc::clone(&usage)), ..mem::RetreatOptions::default() });
        assert_eq!(locked.length(), 0);
        assert_eq!(locked_owner.bytes(), 0);

        mem::reset();
        let never_owner = super::cache("test.lru.never");
        let never = Cache::with_options(
            never_owner.clone(),
            4,
            Arc::new(int_equal),
            CacheOptions { shed: Shed::Never, ..CacheOptions::default() },
        );
        never.add(int_key(1), string_value("never"), 64);
        mem::set_budget(1);
        mem::retreat_with(mem::RetreatOptions { usage: Some(Arc::clone(&usage)), ..mem::RetreatOptions::default() });
        assert_eq!(never.length(), 1);
        assert_eq!(never_owner.bytes(), 64);

    }

}

#[cfg(test)]
mod test_lru;
