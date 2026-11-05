use std::any::Any;
use std::borrow::Borrow;
use std::cell::RefCell;
use rays_hash::Fx;
use std::collections::{HashMap, VecDeque};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem::size_of;
use std::sync::{Arc, Mutex};

pub struct OrderedMap<K, V> {
    indices: HashMap<K, usize>,
    entries: Vec<(K, V)>,
}

impl<K, V> Default for OrderedMap<K, V> {
    fn default() -> Self {
        Self {
            indices: HashMap::new(),
            entries: Vec::new(),
        }
    }
}

impl<K, V> OrderedMap<K, V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            indices: HashMap::with_capacity(capacity),
            entries: Vec::with_capacity(capacity),
        }
    }

    pub fn iter(&self) -> impl DoubleEndedIterator<Item = (&K, &V)> + ExactSizeIterator + '_ {
        self.entries.iter().map(|(key, value)| (key, value))
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

impl<K, V> OrderedMap<K, V> {
    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q> + Eq + Hash,
        Q: Eq + Hash,
    {
        self.indices.get(key).map(|&index| &self.entries[index].1)
    }

    pub fn get_mut<Q: ?Sized>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q> + Eq + Hash,
        Q: Eq + Hash,
    {
        let index = *self.indices.get(key)?;
        Some(&mut self.entries[index].1)
    }

    pub fn remove<Q: ?Sized>(&mut self, key: &Q) -> Option<V>
    where
        K: Borrow<Q> + Eq + Hash,
        Q: Eq + Hash,
    {
        let index = self.indices.remove(key)?;
        let (_, value) = self.entries.remove(index);
        // ponytail: removal shifts and reindexes the suffix; use stable slots if removal-heavy profiles justify it.
        for (next, (key, _)) in self.entries.iter().enumerate().skip(index) {
            *self.indices.get_mut::<K>(key).expect("ordered map index") = next;
        }
        Some(value)
    }
}

impl<K: Eq + Hash + Clone, V> OrderedMap<K, V> {
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        if let Some(&index) = self.indices.get(&key) {
            return Some(std::mem::replace(&mut self.entries[index].1, value));
        }
        let index = self.entries.len();
        self.indices.insert(key.clone(), index);
        self.entries.push((key, value));
        None
    }
}

/// Reports owned heap storage, excluding inline storage and allocator metadata. Opaque layouts
/// and shared graphs require an explicit local implementation.
pub trait HeapSize {
    fn heap_size(&self) -> usize;
}

impl HeapSize for bool {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for char {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for () {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for u8 {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for u16 {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for u32 {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for u64 {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for u128 {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for usize {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for i8 {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for i16 {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for i32 {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for i64 {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for i128 {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for isize {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for f32 {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for f64 {
    fn heap_size(&self) -> usize { 0 }
}

impl HeapSize for String {
    fn heap_size(&self) -> usize {
        self.capacity()
    }
}

impl HeapSize for str {
    fn heap_size(&self) -> usize {
        0
    }
}

impl<T: ?Sized> HeapSize for &T {
    fn heap_size(&self) -> usize {
        0
    }
}

impl<T: ?Sized> HeapSize for &mut T {
    fn heap_size(&self) -> usize {
        0
    }
}

impl<T: HeapSize> HeapSize for [T] {
    fn heap_size(&self) -> usize {
        self.iter()
            .fold(0usize, |size, value| size.saturating_add(value.heap_size()))
    }
}

impl<T: HeapSize, const N: usize> HeapSize for [T; N] {
    fn heap_size(&self) -> usize {
        self.as_slice().heap_size()
    }
}

impl<T: HeapSize> HeapSize for Vec<T> {
    fn heap_size(&self) -> usize {
        self.capacity()
            .saturating_mul(size_of::<T>())
            .saturating_add(self.as_slice().heap_size())
    }
}

impl<T: HeapSize> HeapSize for VecDeque<T> {
    fn heap_size(&self) -> usize {
        self.capacity()
            .saturating_mul(size_of::<T>())
            .saturating_add(
                self.iter()
                    .fold(0usize, |size, value| size.saturating_add(value.heap_size())),
            )
    }
}

impl<T: HeapSize> HeapSize for Box<T> {
    fn heap_size(&self) -> usize {
        size_of::<T>().saturating_add(self.as_ref().heap_size())
    }
}

impl<T: HeapSize> HeapSize for Box<[T]> {
    fn heap_size(&self) -> usize {
        self.len()
            .saturating_mul(size_of::<T>())
            .saturating_add(self.as_ref().heap_size())
    }
}

impl HeapSize for Box<str> {
    fn heap_size(&self) -> usize {
        self.len()
    }
}

impl<T: HeapSize> HeapSize for Option<T> {
    fn heap_size(&self) -> usize {
        self.as_ref().map_or(0, HeapSize::heap_size)
    }
}

impl<T: HeapSize, E: HeapSize> HeapSize for Result<T, E> {
    fn heap_size(&self) -> usize {
        match self {
            Ok(value) => value.heap_size(),
            Err(error) => error.heap_size(),
        }
    }
}

impl<A: HeapSize, B: HeapSize> HeapSize for (A, B) {
    fn heap_size(&self) -> usize {
        self.0.heap_size().saturating_add(self.1.heap_size())
    }
}

impl<A: HeapSize, B: HeapSize, C: HeapSize> HeapSize for (A, B, C) {
    fn heap_size(&self) -> usize {
        self.0
            .heap_size()
            .saturating_add(self.1.heap_size())
            .saturating_add(self.2.heap_size())
    }
}

pub struct Accounted<T>(pub T);

impl<T: HeapSize> HeapSize for Accounted<T> {
    fn heap_size(&self) -> usize {
        self.0.heap_size()
    }
}

impl<T: mem::AccountedSize> mem::AccountedSize for Accounted<T> {
    fn visit_allocations(&self, visitor: &mut mem::AllocationVisitor) {
        self.0.visit_allocations(visitor);
    }
}

#[derive(Clone)]
pub struct Key {
    inner: lru::Key,
}

impl Key {
    pub fn new<T: Hash + Eq + Send + Sync + 'static>(value: T) -> Self {
        let mut hasher = Fx::default();
        value.hash(&mut hasher);
        Self::with_hash(value, hasher.finish())
    }

    pub fn with_hash<T: Any + Send + Sync + 'static>(value: T, hash: u64) -> Self {
        let value: lru::Value = Arc::new(value);
        Self {
            inner: lru::Key::new(hash, value),
        }
    }

    pub fn with_hash_and_equality<T, F>(
        value: T,
        hash: u64,
        equal: F,
    ) -> (Self, Arc<lru::KeyLookupEqual>)
    where
        T: Any + Send + Sync + 'static,
        F: Fn(&T, &T) -> bool + Send + Sync + 'static,
    {
        (Self::with_hash(value, hash), Self::custom_equality::<T, F>(equal))
    }

    pub fn identity<T: Any + Send + Sync + 'static>(value: Arc<T>) -> Self {
        let hash = Arc::as_ptr(&value) as *const () as usize as u64;
        let value: lru::Value = value;
        Self {
            inner: lru::Key::new(hash, value),
        }
    }

    pub fn equality<T: Any + Eq + Send + Sync + 'static>() -> Arc<lru::KeyLookupEqual> {
        Self::custom_equality::<T, _>(|left, right| left == right)
    }

    pub fn identity_equality<T: Any + Send + Sync + 'static>() -> Arc<lru::KeyLookupEqual> {
        Arc::new(|stored, query| {
            stored
                .as_any()
                .downcast_ref::<T>()
                .zip(query.downcast_ref::<T>())
                .is_some_and(|(stored, query)| std::ptr::eq(stored, query))
        })
    }

    pub fn custom_equality<T, F>(equal: F) -> Arc<lru::KeyLookupEqual>
    where
        T: Any + Send + Sync + 'static,
        F: Fn(&T, &T) -> bool + Send + Sync + 'static,
    {
        Arc::new(move |stored, query| {
            stored
                .as_any()
                .downcast_ref::<T>()
                .zip(query.downcast_ref::<T>())
                .is_some_and(|(stored, query)| equal(stored, query))
        })
    }

    pub fn from_lru(key: &lru::Key) -> Self {
        Self { inner: key.clone() }
    }

    pub fn hash(&self) -> u64 {
        self.inner.hash()
    }

    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.inner.as_any().downcast_ref()
    }

    pub fn downcast<T: Any + Send + Sync + 'static>(&self) -> Option<Arc<T>> {
        Arc::downcast(self.inner.value_arc()).ok()
    }

    pub fn as_lru(&self) -> &lru::Key {
        &self.inner
    }

    pub fn into_lru(self) -> lru::Key {
        self.inner
    }
}

impl From<&lru::Key> for Key {
    fn from(key: &lru::Key) -> Self {
        Self::from_lru(key)
    }
}
impl From<lru::Key> for Key {
    fn from(key: lru::Key) -> Self {
        Self { inner: key }
    }
}

impl AsRef<lru::Key> for Key {
    fn as_ref(&self) -> &lru::Key {
        self.as_lru()
    }
}

impl From<Key> for lru::Key {
    fn from(key: Key) -> Self {
        key.into_lru()
    }
}

#[derive(Clone)]
pub struct Value(Arc<dyn Any + Send + Sync>);

impl Value {
    pub fn new<T: Any + Send + Sync + 'static>(value: T) -> Self {
        Self(Arc::new(value))
    }

    pub fn from_arc<T: Any + Send + Sync + 'static>(value: Arc<T>) -> Self {
        Self(value)
    }

    pub fn from_lru(value: lru::Value) -> Self {
        Self(value)
    }

    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.0.downcast_ref()
    }

    pub fn downcast<T: Any + Send + Sync + 'static>(&self) -> Option<Arc<T>> {
        Arc::clone(&self.0).downcast().ok()
    }

    pub fn try_downcast<T: Any + Send + Sync + 'static>(self) -> Result<Arc<T>, Self> {
        Arc::downcast(self.0).map_err(Self)
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }

    pub fn as_lru(&self) -> &lru::Value {
        &self.0
    }

    pub fn into_lru(self) -> lru::Value {
        self.0
    }
}

impl<T: Any + Send + Sync + 'static> From<Arc<T>> for Value {
    fn from(value: Arc<T>) -> Self {
        Self::from_arc(value)
    }
}

impl From<lru::Value> for Value {
    fn from(value: lru::Value) -> Self {
        Self::from_lru(value)
    }
}

impl From<Value> for lru::Value {
    fn from(value: Value) -> Self {
        value.into_lru()
    }
}

pub struct CacheOptions<K, V> {
    pub shed: Shed,
    pub rebuild: f64,
    pub charges: bool,
    pub byte_capacity: i64,
    pub evictable: Option<Arc<dyn Fn(&K, &V) -> bool + Send + Sync + 'static>>,
    pub release: Option<Arc<dyn Fn(&K, &V) + Send + Sync + 'static>>,
}

impl<K, V> Default for CacheOptions<K, V> {
    fn default() -> Self {
        Self {
            shed: Shed::Home,
            rebuild: 1.0,
            charges: true,
            byte_capacity: (1_i64 << 62) - 1,
            evictable: None,
            release: None,
        }
    }
}

pub struct IdentityListOptions<K, V> {
    pub shed: Shed,
    pub rebuild: f64,
    pub release: Option<Arc<dyn Fn(&K, &V) + Send + Sync + 'static>>,
}

impl<K, V> Default for IdentityListOptions<K, V> {
    fn default() -> Self {
        Self {
            shed: Shed::Home,
            rebuild: 1.0,
            release: None,
        }
    }
}

pub use lru::{NotFound, Shed};

fn hash_key<K: Hash>(key: &K) -> u64 {
    let mut hasher = Fx::default();
    key.hash(&mut hasher);
    hasher.finish()
}

fn erase_release<K, V>(
    release: Arc<dyn Fn(&K, &V) + Send + Sync + 'static>,
) -> lru::Release
where
    K: Any + Send + Sync + 'static,
    V: Any + Send + Sync + 'static,
{
    Arc::new(move |key, value| {
        if let (Some(key), Some(value)) = (
            key.as_any().downcast_ref::<K>(),
            value.downcast_ref::<V>(),
        ) {
            release(key, value);
        }
    })
}

fn erase_evictable<K, V>(
    evictable: Arc<dyn Fn(&K, &V) -> bool + Send + Sync + 'static>,
) -> lru::Evictable
where
    K: Any + Send + Sync + 'static,
    V: Any + Send + Sync + 'static,
{
    Arc::new(move |key, value| {
        match (
            key.as_any().downcast_ref::<K>(),
            value.downcast_ref::<V>(),
        ) {
            (Some(key), Some(value)) => evictable(key, value),
            _ => false,
        }
    })
}

fn downcast_value<V: Any + Send + Sync + 'static>(value: lru::Value) -> Option<Arc<V>> {
    Arc::downcast(value).ok()
}

pub struct Cache<K, V> {
    inner: lru::Cache,
    hash: Arc<dyn Fn(&K) -> u64 + Send + Sync + 'static>,
    _types: PhantomData<fn() -> V>,
}

impl<K, V> Cache<K, V>
where
    K: Any + Send + Sync + 'static,
    V: Any + Send + Sync + 'static,
{
    pub fn with_key_functions<H, E>(
        owner: mem::Owner,
        capacity: i64,
        hash: H,
        equal: E,
        options: CacheOptions<K, V>,
    ) -> Self
    where
        H: Fn(&K) -> u64 + Send + Sync + 'static,
        E: Fn(&K, &K) -> bool + Send + Sync + 'static,
    {
        let hash: Arc<dyn Fn(&K) -> u64 + Send + Sync + 'static> = Arc::new(hash);
        let lookup_equal = Key::custom_equality::<K, E>(equal);
        let core_options = lru::CacheOptions {
            shed: options.shed,
            rebuild: options.rebuild,
            charges: options.charges,
            byte_capacity: options.byte_capacity,
            evictable: options.evictable.map(erase_evictable::<K, V>),
            release: options.release.map(erase_release::<K, V>),
        };
        Self {
            inner: lru::Cache::with_options(owner, capacity, lookup_equal, core_options),
            hash,
            _types: PhantomData,
        }
    }

    pub fn find(&self, key: &K) -> Result<Arc<V>, NotFound> {
        downcast_value(self.inner.find_by((self.hash)(key), key)?).ok_or(NotFound)
    }

    pub fn find_opt(&self, key: &K) -> Option<Arc<V>> {
        self.inner
            .find_opt_by((self.hash)(key), key)
            .and_then(downcast_value)
    }

    pub fn peek(&self, key: &K) -> Option<Arc<V>> {
        self.inner
            .peek_by((self.hash)(key), key)
            .and_then(downcast_value)
    }

    pub fn add(&self, key: K, value: V, bytes: i64) {
        self.add_arc(key, Arc::new(value), bytes);
    }

    pub fn add_arc(&self, key: K, value: Arc<V>, bytes: i64) {
        let hash = (self.hash)(&key);
        let key = Key::with_hash(key, hash).into_lru();
        let value: lru::Value = value;
        self.inner.add(key, value, bytes);
    }

    pub fn remove(&self, key: &K) {
        self.inner.remove_by((self.hash)(key), key);
    }

    pub fn take(&self, key: &K) -> Option<Arc<V>> {
        self.inner
            .take_by((self.hash)(key), key)
            .and_then(downcast_value)
    }

    pub fn drop_oldest(&self) -> bool {
        self.inner.drop_oldest()
    }

    pub fn find_first(&self, predicate: &dyn Fn(&K, &V) -> bool) -> Option<Arc<V>> {
        self.inner
            .find_first(&|key, value| match (
                key.as_any().downcast_ref::<K>(),
                value.downcast_ref::<V>(),
            ) {
                (Some(key), Some(value)) => predicate(key, value),
                _ => false,
            })
            .and_then(downcast_value)
    }

    pub fn iter(&self, f: &mut dyn FnMut(&K, &V)) {
        self.inner.iter(&mut |key, value| {
            if let (Some(key), Some(value)) = (
                key.as_any().downcast_ref::<K>(),
                value.downcast_ref::<V>(),
            ) {
                f(key, value);
            }
        });
    }

    pub fn copy<F>(&self, release: F) -> Self
    where
        F: Fn(&K, &V) + Send + Sync + 'static,
    {
        let release: Arc<dyn Fn(&K, &V) + Send + Sync + 'static> = Arc::new(release);
        Self {
            inner: self.inner.copy(erase_release::<K, V>(release)),
            hash: Arc::clone(&self.hash),
            _types: PhantomData,
        }
    }

    pub fn length(&self) -> i64 {
        self.inner.length()
    }

    pub fn bytes(&self) -> i64 {
        self.inner.bytes()
    }

    pub fn clear(&self) {
        self.inner.clear();
    }
}

impl<K, V> Cache<K, V>
where
    K: Hash + Eq + Send + Sync + 'static,
    V: Any + Send + Sync + 'static,
{
    pub fn new(owner: mem::Owner, capacity: i64) -> Self {
        Self::with_options(owner, capacity, CacheOptions::default())
    }

    pub fn with_options(owner: mem::Owner, capacity: i64, options: CacheOptions<K, V>) -> Self {
        Self::with_key_functions(owner, capacity, hash_key::<K>, |left, right| left == right, options)
    }
}

pub struct IdentityList<K, V> {
    inner: lru::IdentityList,
    _types: PhantomData<fn() -> (K, V)>,
}

impl<K, V> IdentityList<K, V>
where
    K: Any + Send + Sync + 'static,
    V: Any + Send + Sync + 'static,
{
    pub fn new<F>(owner: mem::Owner, capacity: i64, equal: F) -> Self
    where
        F: Fn(&K, &K) -> bool + Send + Sync + 'static,
    {
        Self::with_options(owner, capacity, equal, IdentityListOptions::default())
    }

    pub fn with_options<F>(
        owner: mem::Owner,
        capacity: i64,
        equal: F,
        options: IdentityListOptions<K, V>,
    ) -> Self
    where
        F: Fn(&K, &K) -> bool + Send + Sync + 'static,
    {
        let equal = Key::custom_equality::<K, _>(equal);
        let core_options = lru::IdentityListOptions {
            shed: options.shed,
            rebuild: options.rebuild,
            release: options.release.map(erase_release::<K, V>),
        };
        Self {
            inner: lru::IdentityList::with_options(owner, capacity, equal, core_options),
            _types: PhantomData,
        }
    }

    pub fn find(&self, key: &K) -> Option<Arc<V>> {
        self.inner.find_by(key).and_then(downcast_value)
    }

    pub fn peek(&self, key: &K) -> Option<Arc<V>> {
        self.inner.peek_by(key).and_then(downcast_value)
    }

    pub fn add(&self, key: K, value: V, bytes: i64) {
        self.add_arc(key, Arc::new(value), bytes);
    }

    pub fn add_arc(&self, key: K, value: Arc<V>, bytes: i64) {
        let key = Key::with_hash(key, 0).into_lru();
        let value: lru::Value = value;
        self.inner.add(key, value, bytes);
    }

    pub fn remove(&self, key: &K) {
        self.inner.remove_by(key);
    }

    pub fn head(&self) -> Option<(Arc<K>, Arc<V>)> {
        let (key, value) = self.inner.head()?;
        Some((Key::from(key).downcast::<K>()?, downcast_value(value)?))
    }

    pub fn copy<F>(&self, release: F) -> Self
    where
        F: Fn(&K, &V) + Send + Sync + 'static,
    {
        let release: Arc<dyn Fn(&K, &V) + Send + Sync + 'static> = Arc::new(release);
        Self {
            inner: self.inner.copy(erase_release::<K, V>(release)),
            _types: PhantomData,
        }
    }

    pub fn iter(&self, f: &mut dyn FnMut(&K, &V)) {
        self.inner.iter(&mut |key, value| {
            if let (Some(key), Some(value)) = (
                key.as_any().downcast_ref::<K>(),
                value.downcast_ref::<V>(),
            ) {
                f(key, value);
            }
        });
    }

    pub fn filter(&self, keep: &dyn Fn(&K, &V) -> bool) {
        self.inner.filter(&|key, value| match (
            key.as_any().downcast_ref::<K>(),
            value.downcast_ref::<V>(),
        ) {
            (Some(key), Some(value)) => keep(key, value),
            _ => false,
        });
    }

    pub fn length(&self) -> i64 {
        self.inner.length()
    }

    pub fn clear(&self) {
        self.inner.clear();
    }
}

pub fn run<T, F>(domains: Option<i64>, operation: F) -> Result<T, String>
where
    F: FnOnce() -> Result<T, String>,
{
    let operation = RefCell::new(Some(operation));
    let result = RefCell::new(None);
    parallel::run_unit(domains, &|| {
        let operation = operation
            .borrow_mut()
            .take()
            .ok_or_else(|| String::from("Parallel.run: operation invoked more than once"))?;
        let value = operation()?;
        *result.borrow_mut() = Some(value);
        Ok(())
    })?;
    result
        .into_inner()
        .ok_or_else(|| String::from("Parallel.run: operation did not run"))
}

pub fn both<A, B, L, R>(left: L, right: R) -> Result<(A, B), String>
where
    A: Send,
    B: Send,
    L: FnOnce() -> Result<A, String> + Send,
    R: FnOnce() -> Result<B, String> + Send,
{
    let left_operation = Mutex::new(Some(left));
    let right_operation = Mutex::new(Some(right));
    let left_result = Mutex::new(None);
    let right_result = Mutex::new(None);
    let left_work = || {
        let operation = left_operation
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take()
            .ok_or_else(|| String::from("Parallel.both: left callback invoked more than once"))?;
        let value = operation()?;
        *left_result
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(value);
        Ok(())
    };
    let right_work = || {
        let operation = right_operation
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take()
            .ok_or_else(|| String::from("Parallel.both: right callback invoked more than once"))?;
        let value = operation()?;
        *right_result
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(value);
        Ok(())
    };
    parallel::both_unit(&left_work, &right_work)?;
    let left = left_result
        .into_inner()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .ok_or_else(|| String::from("Parallel.both: left result missing"))?;
    let right = right_result
        .into_inner()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .ok_or_else(|| String::from("Parallel.both: right result missing"))?;
    Ok((left, right))
}

const DEFAULT_GRAIN: i64 = 64;

fn collect_range<T, F>(
    start: i64,
    finish: i64,
    grain: i64,
    make: F,
) -> Result<Vec<T>, String>
where
    T: Send,
    F: Fn(i64) -> Result<T, String> + Sync,
{
    if finish < start {
        return Ok(Vec::new());
    }

    let chunks = Mutex::new(Vec::<(i64, Vec<T>)>::new());
    parallel::run_chunks(grain, start, finish, &|first, last| {
        let capacity = usize::try_from(last - first + 1)
            .map_err(|_| String::from("Parallel.run_chunks: invalid chunk bounds"))?;
        let mut values = Vec::new();
        values
            .try_reserve_exact(capacity)
            .map_err(|_| String::from("Array length is outside native storage bounds."))?;
        let mut index = first;
        loop {
            values.push(make(index)?);
            if index == last {
                break;
            }
            index += 1;
        }
        let mut chunks = chunks
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        chunks
            .try_reserve(1)
            .map_err(|_| String::from("Array length is outside native storage bounds."))?;
        chunks.push((first, values));
        Ok(())
    })?;

    let mut chunks = chunks
        .into_inner()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    chunks.sort_unstable_by_key(|(first, _)| *first);
    let length = chunks.iter().map(|(_, values)| values.len()).sum();
    let mut result = Vec::new();
    result
        .try_reserve_exact(length)
        .map_err(|_| String::from("Array length is outside native storage bounds."))?;
    for (_, values) in chunks {
        result.extend(values);
    }
    Ok(result)
}

pub fn map_array<T, U, F>(values: &[T], grain: Option<i64>, map: F) -> Result<Vec<U>, String>
where
    T: Sync,
    U: Send,
    F: Fn(&T) -> Result<U, String> + Sync,
{
    let grain = grain.unwrap_or(DEFAULT_GRAIN);
    if grain <= 0 {
        return Err("Parallel.map_array: grain must be positive".into());
    }
    if values.is_empty() {
        return Ok(Vec::new());
    }
    let finish = i64::try_from(values.len() - 1)
        .map_err(|_| String::from("Array length is outside native storage bounds."))?;
    collect_range(0, finish, grain, |index| map(&values[index as usize]))
}

pub fn init_array<T, F>(length: i64, grain: Option<i64>, init: F) -> Result<Vec<T>, String>
where
    T: Send,
    F: Fn(i64) -> Result<T, String> + Sync,
{
    let grain = grain.unwrap_or(DEFAULT_GRAIN);
    if grain <= 0 {
        return Err("Parallel.init_array: grain must be positive".into());
    }
    if length < 0 {
        return Err("Parallel.init_array: negative length".into());
    }
    let length = usize::try_from(length)
        .ok()
        .filter(|&length| length <= isize::MAX as usize)
        .ok_or_else(|| String::from("Array length is outside native storage bounds."))?;
    if length == 0 {
        return Ok(Vec::new());
    }
    let finish = i64::try_from(length - 1)
        .map_err(|_| String::from("Array length is outside native storage bounds."))?;
    collect_range(0, finish, grain, init)
}

pub fn for_<F>(
    start: i64,
    finish: i64,
    chunk_size: Option<i64>,
    body: F,
) -> Result<(), String>
where
    F: Fn(i64) -> Result<(), String> + Send + Sync,
{
    let chunk_size = chunk_size.unwrap_or(DEFAULT_GRAIN);
    if chunk_size <= 0 {
        return Err("Parallel.for_: chunk_size must be positive".into());
    }
    parallel::for_(chunk_size, start, finish, &body)
}


#[cfg(test)]
mod tests {
    use super::{
        Accounted, Cache, CacheOptions, HeapSize, IdentityList, IdentityListOptions, Key,
        OrderedMap, Value, both, for_, init_array, map_array, run,
    };
    use std::sync::{Arc, Mutex};

    #[test]
    fn ordered_map_replacement_and_removal_keep_order_and_indices_valid() {
        let mut map = OrderedMap::new();
        assert_eq!(map.insert(String::from("a"), 1), None);
        assert_eq!(map.insert(String::from("b"), 2), None);
        assert_eq!(map.insert(String::from("c"), 3), None);
        assert_eq!(map.insert(String::from("b"), 20), Some(2));
        assert_eq!(map.remove("a"), Some(1));
        assert_eq!(map.get("b"), Some(&20));
        *map.get_mut("b").unwrap() = 21;
        assert_eq!(map.insert(String::from("d"), 4), None);
        assert_eq!(
            map.iter().map(|(key, value)| (key.as_str(), *value)).collect::<Vec<_>>(),
            vec![("b", 21), ("c", 3), ("d", 4)]
        );
        assert_eq!(map.len(), 3);
        assert!(!map.is_empty());
    }

    #[test]
    fn erased_keys_values_keep_identity_and_fail_type_mismatches() {
        let string = Key::new(String::from("same"));
        let equal = Key::new(String::from("same"));
        let other_type = Key::with_hash(5_u32, string.hash());
        let key_equal = Key::equality::<String>();
        assert!(key_equal(string.as_lru(), equal.as_lru().as_any()));
        assert!(!key_equal(string.as_lru(), other_type.as_lru().as_any()));

        let (custom, custom_equal) = Key::with_hash_and_equality(
            String::from("Case"),
            7,
            |left, right| left.eq_ignore_ascii_case(right),
        );
        let other_custom = Key::with_hash(String::from("case"), 7);
        assert!(custom_equal(custom.as_lru(), other_custom.as_lru().as_any()));

        let object = Arc::new(String::from("identity"));
        let identity = Key::identity(Arc::clone(&object));
        let same_identity = Key::identity(Arc::clone(&object));
        let same_value = Key::identity(Arc::new(String::from("identity")));
        let identity_equal = Key::identity_equality::<String>();
        assert!(identity_equal(identity.as_lru(), same_identity.as_lru().as_any()));
        assert!(!identity_equal(identity.as_lru(), same_value.as_lru().as_any()));
        assert!(Arc::ptr_eq(&identity.downcast::<String>().unwrap(), &object));

        let owned = Arc::new(String::from("retained by key"));
        let weak = Arc::downgrade(&owned);
        let retaining_key = Key::identity(Arc::clone(&owned));
        drop(owned);
        assert!(weak.upgrade().is_some());
        drop(retaining_key);
        assert!(weak.upgrade().is_none());

        let value = Value::from_arc(Arc::clone(&object));
        let cloned = value.clone();
        assert!(value.ptr_eq(&cloned));
        assert!(Arc::ptr_eq(&value.downcast::<String>().unwrap(), &object));
        assert!(value.clone().try_downcast::<u32>().is_err());
        assert_eq!(value.downcast_ref::<String>().map(String::as_str), Some("identity"));
    }

    #[test]
    fn typed_cache_preserves_release_copy_and_value_identity() {
        assert_eq!(
            CacheOptions::<String, String>::default().byte_capacity,
            (1_i64 << 62) - 1
        );
        let released = Arc::new(Mutex::new(Vec::new()));
        let on_release = Arc::clone(&released);
        let cache: Cache<String, String> = Cache::with_options(
            lru::cache("rays_collections.cache"),
            2,
            CacheOptions {
                release: Some(Arc::new(move |key, value| {
                    on_release
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        .push((key.clone(), value.clone()));
                })),
                ..CacheOptions::default()
            },
        );
        cache.add("a".into(), "old".into(), 3);
        cache.add("a".into(), "new".into(), 5);
        assert_eq!(cache.find_opt(&"a".into()).as_deref().map(String::as_str), Some("new"));
        assert_eq!(
            released.lock().unwrap().as_slice(),
            &[("a".to_owned(), "old".to_owned())]
        );

        let shared = Arc::new(String::from("shared"));
        cache.add_arc("b".into(), Arc::clone(&shared), 7);
        cache.add_arc("b".into(), Arc::clone(&shared), 9);
        assert!(Arc::ptr_eq(&cache.peek(&"b".into()).unwrap(), &shared));
        assert_eq!(cache.bytes(), 14);
        assert_eq!(cache.take(&"a".into()).as_deref().map(String::as_str), Some("new"));
        assert_eq!(released.lock().unwrap().len(), 1);

        let copied_releases = Arc::new(Mutex::new(Vec::new()));
        let copy_release = Arc::clone(&copied_releases);
        let copy = cache.copy(move |key, value| {
            copy_release
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push((key.clone(), value.clone()));
        });
        copy.clear();
        cache.clear();
        assert_eq!(
            copied_releases.lock().unwrap().as_slice(),
            &[("b".to_owned(), "shared".to_owned())]
        );
        assert_eq!(released.lock().unwrap().len(), 2);
        let touched: Cache<i64, i64> =
            Cache::new(lru::cache("rays_collections.cache.touched"), 2);
        touched.add(1, 1, 0);
        touched.add(2, 2, 0);
        assert_eq!(*touched.find_opt(&1).unwrap(), 1);
        touched.add(3, 3, 0);
        assert!(touched.find_opt(&1).is_some());
        assert!(touched.find_opt(&2).is_none());

        let peeked: Cache<i64, i64> =
            Cache::new(lru::cache("rays_collections.cache.peeked"), 2);
        peeked.add(1, 1, 0);
        peeked.add(2, 2, 0);
        assert_eq!(*peeked.peek(&1).unwrap(), 1);
        peeked.add(3, 3, 0);
        assert!(peeked.find_opt(&1).is_none());
        assert!(peeked.find_opt(&2).is_some());
    }

    #[test]
    fn typed_cache_accepts_custom_hash_and_equality_vtables() {
        struct Key(u8);
        let cache: Cache<Key, String> = Cache::with_key_functions(
            lru::cache("rays_collections.cache.custom"),
            2,
            |key| u64::from(key.0 % 2),
            |left, right| left.0 % 4 == right.0 % 4,
            CacheOptions::default(),
        );
        cache.add(Key(2), String::from("two"), 0);
        cache.add(Key(4), String::from("four"), 0);
        assert_eq!(
            cache.find_opt(&Key(6)).as_deref().map(String::as_str),
            Some("two")
        );
        assert_eq!(
            cache.find_opt(&Key(4)).as_deref().map(String::as_str),
            Some("four")
        );
        assert!(cache.find_opt(&Key(3)).is_none());
    }

    #[test]
    fn typed_identity_list_uses_custom_equality_and_copy_release() {
        let released = Arc::new(Mutex::new(Vec::new()));
        let on_release = Arc::clone(&released);
        let list: IdentityList<String, String> = IdentityList::with_options(
            lru::cache("rays_collections.identity-list"),
            4,
            |left, right| left.eq_ignore_ascii_case(right),
            IdentityListOptions {
                release: Some(Arc::new(move |key, value| {
                    on_release
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        .push((key.clone(), value.clone()));
                })),
                ..IdentityListOptions::default()
            },
        );
        list.add("Alpha".into(), "old".into(), 2);
        list.add("Beta".into(), "second".into(), 1);
        assert_eq!(list.head().unwrap().0.as_str(), "Beta");
        assert_eq!(&*list.peek(&"ALPHA".into()).unwrap(), "old");
        assert_eq!(list.head().unwrap().0.as_str(), "Beta");
        assert_eq!(&*list.find(&"ALPHA".into()).unwrap(), "old");
        assert_eq!(list.head().unwrap().0.as_str(), "Alpha");
        list.add("alpha".into(), "new".into(), 3);
        assert_eq!(released.lock().unwrap().len(), 1);
        list.remove(&"Beta".into());
        assert_eq!(released.lock().unwrap().len(), 2);
        let copy_released = Arc::new(Mutex::new(Vec::new()));
        let copy_release = Arc::clone(&copy_released);
        let copy = list.copy(move |key, value| {
            copy_release
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push((key.clone(), value.clone()));
        });
        copy.clear();
        list.clear();
        assert_eq!(
            copy_released.lock().unwrap().as_slice(),
            &[("alpha".to_owned(), "new".to_owned())]
        );
        assert_eq!(released.lock().unwrap().len(), 3);
    }

    #[test]
    fn typed_lru_facades_preserve_order_and_owned_results() {
        let cache_released = Arc::new(Mutex::new(Vec::new()));
        let on_cache_release = Arc::clone(&cache_released);
        let cache: Cache<String, String> = Cache::with_options(
            lru::cache("rays_collections.cache.order"),
            3,
            CacheOptions {
                release: Some(Arc::new(move |key, value| {
                    on_cache_release
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        .push((key.clone(), value.clone()));
                })),
                ..CacheOptions::default()
            },
        );
        cache.add("a".into(), "A".into(), 0);
        cache.add("b".into(), "B".into(), 0);
        cache.add("c".into(), "C".into(), 0);
        assert_eq!(
            cache.find_opt(&"a".into()).as_deref().map(String::as_str),
            Some("A")
        );
        cache.add("b".into(), "B2".into(), 0);
        let mut cache_order = Vec::new();
        cache.iter(&mut |key, value| cache_order.push((key.clone(), value.clone())));
        assert_eq!(
            cache_order,
            vec![
                ("a".to_owned(), "A".to_owned()),
                ("c".to_owned(), "C".to_owned()),
                ("b".to_owned(), "B2".to_owned()),
            ]
        );
        let taken = cache.take(&"a".into()).unwrap();
        cache.clear();
        assert_eq!(taken.as_str(), "A");
        let cache_released = cache_released.lock().unwrap();
        assert_eq!(cache_released.len(), 3);
        assert!(cache_released.contains(&("b".into(), "B".into())));
        assert!(cache_released.contains(&("c".into(), "C".into())));
        assert!(cache_released.contains(&("b".into(), "B2".into())));

        let list_released = Arc::new(Mutex::new(Vec::new()));
        let on_list_release = Arc::clone(&list_released);
        let list: IdentityList<String, String> = IdentityList::with_options(
            lru::cache("rays_collections.identity-list.order"),
            3,
            |left, right| left == right,
            IdentityListOptions {
                release: Some(Arc::new(move |key, value| {
                    on_list_release
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        .push((key.clone(), value.clone()));
                })),
                ..IdentityListOptions::default()
            },
        );
        list.add("a".into(), "A".into(), 0);
        list.add("b".into(), "B".into(), 0);
        list.add("c".into(), "C".into(), 0);
        assert_eq!(
            list.peek(&"a".into()).as_deref().map(String::as_str),
            Some("A")
        );
        assert_eq!(list.head().unwrap().0.as_str(), "c");
        assert_eq!(
            list.find(&"a".into()).as_deref().map(String::as_str),
            Some("A")
        );
        let mut list_order = Vec::new();
        list.iter(&mut |key, value| list_order.push((key.clone(), value.clone())));
        assert_eq!(
            list_order,
            vec![
                ("b".to_owned(), "B".to_owned()),
                ("c".to_owned(), "C".to_owned()),
                ("a".to_owned(), "A".to_owned()),
            ]
        );
        let held = list.head().unwrap();
        list.clear();
        assert_eq!(held.0.as_str(), "a");
        assert_eq!(held.1.as_str(), "A");
        assert_eq!(list_released.lock().unwrap().len(), 3);
    }

    #[test]
    fn heap_size_counts_owned_container_storage_and_accounted_delegates() {
        let values = vec![String::with_capacity(16), String::with_capacity(32)];
        let expected = values
            .capacity()
            .saturating_mul(std::mem::size_of::<String>())
            .saturating_add(values.iter().map(String::capacity).sum::<usize>());
        assert_eq!(values.heap_size(), expected);
        let accounted = Accounted(values);
        assert_eq!(mem::reachable_bytes(&accounted), expected);

        let boxed = Box::new(String::with_capacity(8));
        assert_eq!(
            boxed.heap_size(),
            std::mem::size_of::<String>() + boxed.capacity()
        );
        let string = String::with_capacity(7);
        let expected = string.capacity();
        assert_eq!((string, 1_u64).heap_size(), expected);
        let mut deque = std::collections::VecDeque::new();
        deque.push_back(String::with_capacity(12));
        let expected = deque
            .capacity()
            .saturating_mul(std::mem::size_of::<String>())
            .saturating_add(deque.front().unwrap().capacity());
        assert_eq!(deque.heap_size(), expected);
    }

    #[test]
    fn typed_parallel_adapters_return_values_and_match_one_and_many_domains() {
        let input: Vec<i64> = (0..257).collect();
        let execute = |domains| {
            run(Some(domains), || {
                let mapped = map_array(&input, Some(32), |value| Ok(value * value))?;
                let initialized = init_array(257, None, |index| Ok(index * 3))?;
                let branches = both(|| Ok(21), || Ok(21 * 2))?;
                let indices = Mutex::new(Vec::new());
                for_(0, 256, None, |index| {
                    indices
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        .push(index);
                    Ok(())
                })?;
                let mut indices = std::mem::take(
                    &mut *indices
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner),
                );
                indices.sort_unstable();
                Ok((mapped, initialized, branches, indices))
            })
            .unwrap()
        };
        assert_eq!(execute(1), execute(4));

        let captured = String::from("run result");
        assert_eq!(run(Some(1), move || Ok(captured)).unwrap(), "run result");
        let captured = String::from("both result");
        assert_eq!(
            both(move || Ok(captured), || Ok(42)).unwrap(),
            (String::from("both result"), 42)
        );
    }

    #[test]
    fn parallel_adapters_return_source_errors_for_invalid_boundaries() {
        assert_eq!(
            map_array(&[1_i64], Some(0), |value| Ok(*value)).unwrap_err(),
            "Parallel.map_array: grain must be positive"
        );
        assert_eq!(
            init_array(0, Some(0), |_| Ok(0)).unwrap_err(),
            "Parallel.init_array: grain must be positive"
        );
        assert_eq!(
            init_array(-1, None, |_| Ok(0)).unwrap_err(),
            "Parallel.init_array: negative length"
        );
        assert_eq!(
            for_(0, -1, Some(0), |_| Ok(())).unwrap_err(),
            "Parallel.for_: chunk_size must be positive"
        );
        assert_eq!(run(None, || Err::<(), String>("run error".into())), Err("run error".into()));
        assert_eq!(
            map_array(&[1_i64], Some(1), |_| Err::<i64, String>("map error".into())),
            Err("map error".into())
        );
        assert_eq!(
            both(
                || Err::<(), String>("both error".into()),
                || Ok::<(), String>(())
            ),
            Err("both error".into())
        );
        assert_eq!(
            init_array(3, Some(1), |index| {
                if index == 1 {
                    Err("init error".into())
                } else {
                    Ok(index)
                }
            }),
            Err("init error".into())
        );
        assert_eq!(
            for_(0, 2, Some(1), |index| {
                if index == 1 {
                    Err("for error".into())
                } else {
                    Ok(())
                }
            }),
            Err("for error".into())
        );
    }

}
