use std::cmp::Ordering;
use std::collections::HashMap;
use std::sync::atomic::{AtomicI64, AtomicU64, Ordering as AtomicOrdering};
use std::sync::{Arc, Mutex, Weak};

use crate::{cancel, error};
use crate::error::Code;

static NEXT_DATA_ID: AtomicI64 = AtomicI64::new(0);
static NEXT_CACHE_ID: AtomicU64 = AtomicU64::new(0);

pub fn fresh_data_id() -> i64 {
    NEXT_DATA_ID.fetch_add(1, AtomicOrdering::Relaxed)
}

/// OCaml `String.trim` (space, tab, newline, form feed, carriage return only).
pub fn ocaml_trim(value: &str) -> &str {
    value.trim_matches(|c| matches!(c, ' ' | '\t' | '\n' | '\r' | '\x0c'))
}

/// OCaml `String.trim s = ""`.
pub fn ocaml_trim_is_empty(value: &str) -> bool {
    ocaml_trim(value).is_empty()
}

#[allow(non_snake_case)]
pub mod Bits {
    pub fn mem(bits: &[u8], index: i64) -> bool {
        let index = usize::try_from(index).expect("bit index must be nonnegative");
        bits[index >> 3] & (1 << (index & 7)) != 0
    }

    pub fn set(bits: &mut [u8], index: i64) {
        let index = usize::try_from(index).expect("bit index must be nonnegative");
        bits[index >> 3] |= 1 << (index & 7);
    }
}

#[derive(Clone)]
pub enum IdentityValue {
    PointIndex(crate::point_index::T),
    TopologyIndex(crate::topology_index::T),
}

struct Stored {
    value: IdentityValue,
    owner: Option<mem::Owner>,
    bytes: i64,
}

pub struct IdentityToken {
    id: i64,
    values: Mutex<HashMap<u64, Stored>>,
}

impl IdentityToken {
    pub fn new(data_id: i64) -> Arc<IdentityToken> {
        Arc::new(Self {
            id: data_id,
            values: Mutex::new(HashMap::new()),
        })
    }
}

impl Drop for IdentityToken {
    fn drop(&mut self) {
        let values = self
            .values
            .get_mut()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        for (_, stored) in values.drain() {
            if let Some(owner) = stored.owner {
                mem::release(&owner, stored.bytes);
            }
        }
    }
}

struct Entry {
    id: i64,
    token: Weak<IdentityToken>,
}

struct CacheState {
    slots: Vec<Option<Entry>>,
    next: usize,
}

pub struct IdentityCache {
    id: u64,
    owner: Option<mem::Owner>,
    bytes: Option<fn(&IdentityValue) -> i64>,
    state: Mutex<CacheState>,
}

impl mem::AccountedSize for IdentityCache {
    fn visit_allocations(&self, visitor: &mut mem::AllocationVisitor) {
        let state = lock_state(&self.state);
        visitor.allocation(state.slots.as_ptr() as usize, state.slots.capacity() * std::mem::size_of::<Option<Entry>>());
    }
}

fn lock_state(state: &Mutex<CacheState>) -> std::sync::MutexGuard<'_, CacheState> {
    state
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
}

fn lock_values(
    values: &Mutex<HashMap<u64, Stored>>,
) -> std::sync::MutexGuard<'_, HashMap<u64, Stored>> {
    values
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
}

fn release_token_value(id: u64, token: &Arc<IdentityToken>) {
    if let Some(stored) = lock_values(&token.values).remove(&id) {
        if let Some(owner) = stored.owner {
            mem::release(&owner, stored.bytes);
        }
    }
}

impl IdentityCache {
    pub fn create(
        owner: Option<mem::Owner>,
        bytes: Option<fn(&IdentityValue) -> i64>,
        capacity: i64,
    ) -> Result<Arc<IdentityCache>, error::T> {
        if capacity <= 0 {
            return Err(error::new(
                "Support.Identity_cache.create",
                Code::InvalidArgument,
                "Identity_cache.create: capacity must be positive".to_owned(),
            ));
        }
        let capacity = usize::try_from(capacity).map_err(|_| {
            error::new(
                "Support.Identity_cache.create",
                Code::InvalidArgument,
                "Identity_cache.create: capacity is out of range".to_owned(),
            )
        })?;
        let cache = Arc::new(IdentityCache {
            id: NEXT_CACHE_ID.fetch_add(1, AtomicOrdering::Relaxed),
            owner,
            bytes,
            state: Mutex::new(CacheState {
                slots: (0..capacity).map(|_| None).collect(),
                next: 0,
            }),
        });
        if let Some(owner) = &cache.owner {
            let table: Arc<dyn mem::AccountedSize + Send + Sync> = cache.clone();
            mem::walk_table(owner, table, Arc::new(|| false));
            let weak = Arc::downgrade(&cache);
            let weak_live = weak.clone();
            mem::register_evictor(
                owner,
                mem::EvictorOptions {
                    rebuild: 2.0,
                    live: Some(Arc::new(move || weak_live.strong_count() > 0)),
                    ..mem::EvictorOptions::default()
                },
                Arc::new(move || weak.upgrade().is_some_and(|cache| cache.drop_oldest())),
            );
        }
        Ok(cache)
    }

    fn find_locked(
        &self,
        state: &mut CacheState,
        token: &Arc<IdentityToken>,
    ) -> Option<IdentityValue> {
        for index in 0..state.slots.len() {
            let (entry_id, key) = match &state.slots[index] {
                Some(entry) => (entry.id, entry.token.upgrade()),
                None => continue,
            };
            let Some(key) = key else {
                state.slots[index] = None;
                continue;
            };
            if entry_id == token.id && Arc::ptr_eq(&key, token) {
                let value = lock_values(&key.values)
                    .get(&self.id)
                    .map(|stored| stored.value.clone());
                if value.is_some() {
                    return value;
                }
                state.slots[index] = None;
            }
        }
        None
    }

    fn drop_oldest_locked(&self, state: &mut CacheState) -> bool {
        let count = state.slots.len();
        for offset in 0..count {
            let index = (state.next + offset) % count;
            if let Some(entry) = state.slots[index].take() {
                if let Some(token) = entry.token.upgrade() {
                    release_token_value(self.id, &token);
                }
                return true;
            }
        }
        false
    }

    fn drop_oldest(&self) -> bool {
        let mut state = match self.state.try_lock() {
            Ok(state) => state,
            Err(std::sync::TryLockError::Poisoned(error)) => error.into_inner(),
            Err(std::sync::TryLockError::WouldBlock) => return false,
        };
        self.drop_oldest_locked(&mut state)
    }

    pub fn find_or_add(
        &self,
        token: &Arc<IdentityToken>,
        build: &dyn Fn() -> Result<IdentityValue, error::T>,
    ) -> Result<IdentityValue, error::T> {
        if let Some(value) = self.find_locked(&mut lock_state(&self.state), token) {
            return Ok(value);
        }
        let built = build()?;
        let mut state = lock_state(&self.state);
        if let Some(value) = self.find_locked(&mut state, token) {
            return Ok(value);
        }

        let index = state.next;
        if let Some(old) = state.slots[index].take() {
            if let Some(old_token) = old.token.upgrade() {
                release_token_value(self.id, &old_token);
            }
        }
        let bytes = self.bytes.map_or(0, |size| size(&built)).max(0);
        if bytes > 0 {
            if let Some(owner) = &self.owner {
                mem::charge(owner, bytes);
            }
        }
        lock_values(&token.values).insert(
            self.id,
            Stored {
                value: built.clone(),
                owner: self.owner.clone(),
                bytes,
            },
        );
        state.slots[index] = Some(Entry {
            id: token.id,
            token: Arc::downgrade(token),
        });
        state.next = (index + 1) % state.slots.len();
        Ok(built)
    }
}

impl Drop for IdentityCache {
    fn drop(&mut self) {
        let id = self.id;
        let state = self
            .state
            .get_mut()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        for entry in state.slots.iter_mut().filter_map(Option::take) {
            if let Some(token) = entry.token.upgrade() {
                release_token_value(id, &token);
            }
        }
    }
}

fn compare(keys: &[f64], left: i64, right: i64) -> Ordering {
    let left_key = keys[usize::try_from(left).expect("nonnegative selection index")];
    let right_key = keys[usize::try_from(right).expect("nonnegative selection index")];
    match (left_key.is_nan(), right_key.is_nan()) {
        (true, true) => left.cmp(&right),
        (true, false) => Ordering::Less,
        (false, true) => Ordering::Greater,
        (false, false) => left_key
            .partial_cmp(&right_key)
            .unwrap_or(Ordering::Equal)
            .then_with(|| left.cmp(&right)),
    }
}

fn median_pivot(keys: &[f64], order: &[i64], first: i64, middle: i64, last: i64) -> i64 {
    let a = order[usize::try_from(first).expect("nonnegative first")];
    let b = order[usize::try_from(middle).expect("nonnegative middle")];
    let c = order[usize::try_from(last).expect("nonnegative last")];
    if compare(keys, a, b) == Ordering::Less {
        if compare(keys, b, c) == Ordering::Less {
            b
        } else if compare(keys, a, c) == Ordering::Less {
            c
        } else {
            a
        }
    } else if compare(keys, a, c) == Ordering::Less {
        a
    } else if compare(keys, b, c) == Ordering::Less {
        c
    } else {
        b
    }
}

pub fn select(
    cancel_value: Option<&cancel::T>,
    keys: &[f64],
    order: &mut [i64],
    first: i64,
    last: i64,
    selected: i64,
) -> Result<(), error::T> {
    let order_len = i64::try_from(order.len()).unwrap_or(i64::MAX);
    let key_len = i64::try_from(keys.len()).unwrap_or(i64::MAX);
    if first < 0
        || last < first
        || last >= order_len
        || selected < first
        || selected > last
        || order[first as usize..=last as usize]
            .iter()
            .any(|&index| index < 0 || index >= key_len)
    {
        return Err(error::new(
            "Support.select",
            Code::InvalidArgument,
            "selection range or index out of bounds".to_owned(),
        ));
    }
    let mut lower = first;
    let mut upper = last;
    while lower < upper {
        cancel::check_opt(cancel_value).map_err(|_| {
            error::new(
                "Support.select",
                Code::Cancelled,
                "geometry operation was cancelled".to_owned(),
            )
        })?;
        let middle = lower + (upper - lower) / 2;
        let pivot = median_pivot(keys, order, lower, middle, upper);
        let mut left = lower;
        let mut right = upper;
        while left <= right {
            while compare(
                keys,
                order[usize::try_from(left).expect("nonnegative selection index")],
                pivot,
            ) == Ordering::Less
            {
                left += 1;
            }
            while compare(
                keys,
                order[usize::try_from(right).expect("nonnegative selection index")],
                pivot,
            ) == Ordering::Greater
            {
                right -= 1;
            }
            if left <= right {
                let left_index = usize::try_from(left).expect("nonnegative selection index");
                let right_index = usize::try_from(right).expect("nonnegative selection index");
                order.swap(left_index, right_index);
                left += 1;
                right -= 1;
            }
        }
        if selected <= right {
            upper = right;
        } else if selected >= left {
            lower = left;
        } else {
            lower = selected;
            upper = selected;
        }
    }
    Ok(())
}

pub fn ceiling_div(value: i64, divisor: i64) -> Result<i64, error::T> {
    if value < 0 || divisor <= 0 {
        return Err(error::new(
            "Support.ceiling_div",
            Code::InvalidArgument,
            "ceiling_div requires a nonnegative value and positive divisor".to_owned(),
        ));
    }
    let quotient = value / divisor;
    Ok(if value % divisor == 0 {
        quotient
    } else {
        quotient + 1
    })
}

fn run_tasks(
    count: i64,
    task: &(dyn Fn(i64) -> Result<(), error::T> + Sync),
) -> Result<(), error::T> {
    if count <= 0 {
        return Ok(());
    }
    let failure = Mutex::new(None);
    let result = parallel::for_(1, 0, count - 1, &|index| {
        if let Err(error) = task(index) {
            let mut failure = lock_failure(&failure);
            let replace = match failure.as_ref() {
                Some((first, _)) => index < *first,
                None => true,
            };
            if replace {
                *failure = Some((index, error));
            }
        }
        Ok(())
    });
    match lock_failure(&failure).take() {
        Some((_, error)) => Err(error),
        None => result.map_err(|message| {
            error::new("Support.run_ranges", Code::ParallelError, message)
        }),
    }
}

fn lock_failure(
    value: &Mutex<Option<(i64, error::T)>>,
) -> std::sync::MutexGuard<'_, Option<(i64, error::T)>> {
    value
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
}


fn cancelled(operation: &str) -> error::T {
    error::new(operation, Code::Cancelled, "geometry operation was cancelled".to_owned())
}

pub fn parallel_for(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    count: i64,
    work: &(dyn Fn(i64) -> Result<(), error::T> + Sync),
) -> Result<(), error::T> {
    if count <= 0 {
        return Ok(());
    }
    let grain = grain.max(1);
    let ranges = (count - 1) / grain + 1;
    run_tasks(ranges, &|range| {
        let first = range.saturating_mul(grain);
        let last = count.min(first.saturating_add(grain));
        for element in first..last {
            cancel::poll(cancel_value, element, 4095).map_err(|_| cancelled("Support.parallel_for"))?;
            work(element)?;
        }
        Ok(())
    })
}

/// Number of `grain`-sized ranges `run_indexed_ranges` splits `count` elements into.
pub fn range_count(count: i64, grain: i64) -> i64 {
    if count <= 0 { 0 } else { (count - 1) / grain.max(1) + 1 }
}

/// Cancellation poll shared by every element loop (every 4096th element).
pub fn poll(cancel_value: Option<&cancel::T>, element: i64) -> Result<(), error::T> {
    cancel::poll(cancel_value, element, 4095).map_err(|_| cancelled("Support.parallel_for"))
}

/// Disjoint output windows for `run_indexed_ranges` closures: range `r` owns `out[bound(r)..bound(r + 1)]`
/// (bounds nondecreasing; elements outside `bound(0)..bound(ranges)` are never handed out).
/// Boundaries are fixed by the caller, so sequential and parallel runs write identical bytes.
pub struct Windows<'a, T>(Vec<Mutex<Option<&'a mut [T]>>>);

impl<'a, T: Send> Windows<'a, T> {
    pub fn new(out: &'a mut [T], ranges: i64, bound: impl Fn(i64) -> usize) -> Self {
        let mut previous = bound(0);
        let mut rest = &mut out[previous..];
        let windows = (0..ranges)
            .map(|range| {
                let next = bound(range + 1);
                let (window, tail) = std::mem::take(&mut rest).split_at_mut(next - previous);
                (rest, previous) = (tail, next);
                Mutex::new(Some(window))
            })
            .collect();
        Windows(windows)
    }

    /// `grain`-sized windows aligned with `run_indexed_ranges(grain, out.len())`.
    pub fn even(out: &'a mut [T], grain: i64) -> Self {
        let (len, grain) = (out.len(), grain.max(1) as usize);
        Self::new(out, range_count(len as i64, grain as i64), |range| (range as usize).saturating_mul(grain).min(len))
    }

    /// The window of `range`; each range runs once, so a second take is a bug.
    pub fn take(&self, range: i64) -> &'a mut [T] {
        let mut window = self.0[range as usize].lock().unwrap_or_else(std::sync::PoisonError::into_inner);
        window.take().expect("output window taken twice")
    }
}

/// `out[i] = work(i)` over `grain`-sized ranges, polling cancellation like `parallel_for`.
pub fn par_fill<T: Send>(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    out: &mut [T],
    work: &(dyn Fn(i64) -> Result<T, error::T> + Sync),
) -> Result<(), error::T> {
    let (count, grain) = (out.len() as i64, grain.max(1));
    let windows = Windows::even(out, grain);
    run_indexed_ranges(grain, count, &|range, first, last| {
        for (element, slot) in (first..last).zip(windows.take(range)) {
            poll(cancel_value, element)?;
            *slot = work(element)?;
        }
        Ok(())
    })
}

pub fn run_indexed_ranges(
    grain: i64,
    count: i64,
    operation: &(dyn Fn(i64, i64, i64) -> Result<(), error::T> + Sync),
) -> Result<(), error::T> {
    if grain <= 0 {
        return Err(error::new(
            "Support.run_ranges",
            Code::InvalidArgument,
            "grain must be positive".to_owned(),
        ));
    }
    if count <= 0 {
        return Ok(());
    }
    let ranges = (count - 1) / grain + 1;
    run_tasks(ranges, &|range| {
        let first = range.saturating_mul(grain);
        let last = count.min(first.saturating_add(grain));
        operation(range, first, last)
    })
}

pub fn run_ranges(
    grain: Option<i64>,
    count: i64,
    operation: &(dyn Fn(i64, i64) -> Result<(), error::T> + Sync),
) -> Result<(), error::T> {
    let grain = grain.unwrap_or(16_384);
    run_indexed_ranges(grain, count, &|_, first, last| operation(first, last))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};

    fn value_bytes(_: &IdentityValue) -> i64 {
        8
    }

    #[test]
    fn bit_access_selection_and_ranges_preserve_order_contracts() {
        let mut bits = [0_u8; 2];
        Bits::set(&mut bits, 9);
        assert!(Bits::mem(&bits, 9));
        assert!(!Bits::mem(&bits, 8));

        let keys = [3.0, 1.0, 2.0, 1.0];
        let mut order = [0, 1, 2, 3];
        select(None, &keys, &mut order, 0, 3, 1).unwrap();
        assert_eq!(order[1], 3);
        assert!(select(None, &keys, &mut order, 0, 4, 1).is_err());
        assert!(compare(&keys, order[0], order[1]) != Ordering::Greater);
        assert!(compare(&keys, order[1], order[2]) != Ordering::Greater);

        let seen = AtomicUsize::new(0);
        run_ranges(Some(2), 7, &|first, last| {
            seen.fetch_add(usize::try_from(last - first).unwrap(), AtomicOrdering::Relaxed);
            Ok(())
        })
        .unwrap();
        assert_eq!(seen.load(AtomicOrdering::Relaxed), 7);
    }

    #[test]
    fn identity_values_follow_token_lifetime_and_fifo_capacity() {
        let topology = crate::topology::empty(1).unwrap();
        let index = crate::point_index::create(None, &topology).unwrap();
        let owner = mem::cache("rdk.test.identity_cache");
        let cache = IdentityCache::create(Some(owner.clone()), Some(value_bytes), 1).unwrap();
        let first = IdentityToken::new(10);
        let built_count = AtomicUsize::new(0);
        let built = || {
            built_count.fetch_add(1, AtomicOrdering::Relaxed);
            Ok(IdentityValue::PointIndex(index.clone()))
        };
        let value = cache.find_or_add(&first, &built).unwrap();
        let again = cache.find_or_add(&first, &built).unwrap();
        assert!(matches!(value, IdentityValue::PointIndex(_)));
        assert!(matches!(again, IdentityValue::PointIndex(_)));
        assert_eq!(built_count.load(AtomicOrdering::Relaxed), 1);
        assert_eq!(owner.bytes(), 8);

        let second = IdentityToken::new(11);
        let _ = cache
            .find_or_add(&second, &|| Ok(IdentityValue::PointIndex(index.clone())))
            .unwrap();
        assert_eq!(owner.bytes(), 8);
        drop(first);
        assert_eq!(owner.bytes(), 8);
        drop(second);
        assert_eq!(owner.bytes(), 0);
    }
    #[test]
    fn par_fill_and_windows_are_deterministic_across_domains() {
        let run = |domains| {
            let result = Mutex::new((Vec::new(), Vec::new()));
            parallel::run_unit(Some(domains), &|| {
                let mut squares = vec![0_i64; 10_007];
                par_fill(None, 64, &mut squares, &|i| Ok(i * i)).unwrap();
                // ragged: row r owns r % 3 + 1 values, ranges of 5 rows
                let rows = 1_000_i64;
                let starts: Vec<usize> = (0..=rows).scan(0, |at, r| { let start = *at; *at += (r % 3 + 1) as usize; Some(start) }).collect();
                let mut values = vec![0_i64; starts[rows as usize]];
                let windows = Windows::new(&mut values, range_count(rows, 7), |range| starts[(range * 7).min(rows) as usize]);
                run_indexed_ranges(7, rows, &|range, first, last| {
                    let window = windows.take(range);
                    let base = starts[first as usize];
                    for row in first..last {
                        for at in starts[row as usize]..starts[row as usize + 1] {
                            window[at - base] = row * 10 + (at - starts[row as usize]) as i64;
                        }
                    }
                    Ok(())
                })
                .unwrap();
                *result.lock().unwrap() = (squares, values);
                Ok(())
            })
            .unwrap();
            result.into_inner().unwrap()
        };
        let (squares, values) = run(1);
        assert_eq!(squares[10_006], 10_006 * 10_006);
        assert_eq!(values[0], 0);
        assert_eq!((squares.clone(), values.clone()), run(4));
    }

    #[test]
    fn run_tasks_returns_the_lowest_failing_range() {
        let result = Mutex::new(None);
        parallel::run_unit(Some(4), &|| {
            *result.lock().unwrap() = Some(run_tasks(3, &|range| {
                if range == 0 {
                    std::thread::sleep(std::time::Duration::from_millis(40));
                    Err(error::new("test.run_tasks", Code::InvalidArgument, "range 0".to_owned()))
                } else if range == 1 {
                    Err(error::new("test.run_tasks", Code::InvalidArgument, "range 1".to_owned()))
                } else {
                    Ok(())
                }
            }));
            Ok(())
        })
        .unwrap();
        let error = result.into_inner().unwrap().unwrap().unwrap_err();
        assert_eq!(error::message(&error), "range 0");
    }

    struct RestoreBudget(i64);

    impl Drop for RestoreBudget {
        fn drop(&mut self) {
            mem::set_budget(self.0);
        }
    }

    fn large_value_bytes(_: &IdentityValue) -> i64 {
        1_i64 << 50
    }

    #[test]
    fn memory_retreat_evicts_and_releases_an_owner_charged_entry() {
        const CHARGE: i64 = 1_i64 << 50;
        let _restore_budget = RestoreBudget(mem::budget());
        mem::set_budget(1);

        let topology = crate::topology::empty(1).unwrap();
        let index = crate::point_index::create(None, &topology).unwrap();
        let owner = mem::cache("rdk.test.identity_cache_eviction");
        let cache = IdentityCache::create(Some(owner.clone()), Some(large_value_bytes), 1).unwrap();
        let token = IdentityToken::new(20);
        let builds = AtomicUsize::new(0);
        let build = || {
            builds.fetch_add(1, AtomicOrdering::Relaxed);
            Ok(IdentityValue::PointIndex(index.clone()))
        };
        cache.find_or_add(&token, &build).unwrap();
        assert_eq!(owner.bytes(), CHARGE);

        let usage: Arc<dyn Fn() -> i64 + Send + Sync> = Arc::new(|| CHARGE + 1);
        assert_eq!(
            mem::retreat_with(mem::RetreatOptions {
                usage: Some(usage),
                ..mem::RetreatOptions::default()
            }),
            mem::Verdict::Within
        );
        assert_eq!(owner.bytes(), 0);

        cache.find_or_add(&token, &build).unwrap();
        assert_eq!(builds.load(AtomicOrdering::Relaxed), 2);
        assert_eq!(owner.bytes(), CHARGE);
        drop(token);
        assert_eq!(owner.bytes(), 0);
    }
}
