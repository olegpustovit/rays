use std::collections::{HashMap, HashSet};
use std::fmt;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::{Arc, LazyLock, Mutex, Weak};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Kind {
    Cache,
    Plan,
    Document,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Pool {
    Host,
    Device,
}

struct Account {
    name: String,
    kind: Kind,
    pool: Pool,
    bytes: AtomicI64,
    peak: AtomicI64,
}

#[derive(Clone)]
pub struct Owner(Arc<Account>);

impl Owner {
    pub fn name(&self) -> &str {
        &self.0.name
    }

    pub fn kind(&self) -> Kind {
        self.0.kind
    }

    pub fn pool(&self) -> Pool {
        self.0.pool
    }

    pub fn bytes(&self) -> i64 {
        self.0.bytes.load(Ordering::Relaxed)
    }

    pub fn peak(&self) -> i64 {
        self.0.peak.load(Ordering::Relaxed)
    }

    fn id(&self) -> usize {
        Arc::as_ptr(&self.0) as usize
    }
}

impl fmt::Debug for Owner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Owner")
            .field("name", &self.name())
            .field("kind", &self.kind())
            .field("pool", &self.pool())
            .field("bytes", &self.bytes())
            .finish()
    }
}

impl PartialEq for Owner {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Owner {}


#[derive(Default)]
struct State {
    accounts: HashMap<(Kind, Pool), HashMap<String, Owner>>,
    evictors: Vec<Arc<Evictor>>,
    evictor_mark: usize,
    roots: HashMap<usize, (Owner, Arc<dyn Fn() -> usize + Send + Sync>)>,
    tables: Vec<Table>,
    walked: HashMap<usize, (Owner, i64)>,
}

static STATE: LazyLock<Mutex<State>> = LazyLock::new(|| {
    Mutex::new(State {
        evictor_mark: 16,
        ..State::default()
    })
});

fn state() -> std::sync::MutexGuard<'static, State> {
    STATE.lock().unwrap_or_else(|error| error.into_inner())
}
// ponytail: serialize root walks to keep ledger deltas coherent; partition by owner if reports contend.

static WALK_LOCK: Mutex<()> = Mutex::new(());

fn get(pool: Pool, kind: Kind, name: &str) -> Owner {
    let mut state = state();
    let accounts = state.accounts.entry((kind, pool)).or_default();
    if let Some(owner) = accounts.get(name) {
        return owner.clone();
    }
    let owner = Owner(Arc::new(Account {
        name: name.to_owned(),
        kind,
        pool,
        bytes: AtomicI64::new(0),
        peak: AtomicI64::new(0),
    }));
    accounts.insert(name.to_owned(), owner.clone());
    owner
}

pub fn cache(name: &str) -> Owner {
    cache_in(Pool::Host, name)
}

pub fn cache_in(pool: Pool, name: &str) -> Owner {
    get(pool, Kind::Cache, name)
}

pub fn plan(name: &str) -> Owner {
    plan_in(Pool::Host, name)
}

pub fn plan_in(pool: Pool, name: &str) -> Owner {
    get(pool, Kind::Plan, name)
}

pub fn document(name: &str) -> Owner {
    document_in(Pool::Host, name)
}

pub fn document_in(pool: Pool, name: &str) -> Owner {
    get(pool, Kind::Document, name)
}

pub fn name(owner: &Owner) -> &str {
    owner.name()
}

pub fn kind(owner: &Owner) -> Kind {
    owner.kind()
}

pub fn pool(owner: &Owner) -> Pool {
    owner.pool()
}

pub fn bytes(owner: &Owner) -> i64 {
    owner.bytes()
}

pub fn peak(owner: &Owner) -> i64 {
    owner.peak()
}

fn raise_peak(account: &Account, value: i64) {
    let mut seen = account.peak.load(Ordering::Relaxed);
    while value > seen {
        match account.peak.compare_exchange_weak(seen, value, Ordering::Relaxed, Ordering::Relaxed) {
            Ok(_) => break,
            Err(now) => seen = now,
        }
    }
}

pub fn charge(owner: &Owner, n: i64) {
    if n <= 0 {
        return;
    }
    let before = owner.0.bytes.try_update(Ordering::Relaxed, Ordering::Relaxed, |bytes| {
        Some(bytes.saturating_add(n))
    }).unwrap_or_else(|bytes| bytes);
    raise_peak(&owner.0, before.saturating_add(n));
}

pub fn release(owner: &Owner, n: i64) {
    if n > 0 {
        let _ = owner.0.bytes.try_update(Ordering::Relaxed, Ordering::Relaxed, |bytes| {
            Some(bytes.saturating_sub(n).max(0))
        });
    }
}

/// Explicitly measured bytes owned by a value. Implement this for project types whose retained heap
/// storage is known; Rust has no safe general-purpose equivalent of an OCaml heap graph walk.
pub trait AccountedSize: Send + Sync {
    fn owned_bytes(&self) -> usize;
}

impl AccountedSize for bool {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for char {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for u8 {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for u16 {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for u32 {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for u64 {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for u128 {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for usize {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for i8 {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for i16 {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for i32 {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for i64 {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for i128 {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for isize {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for f32 {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for f64 {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for () {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for str {
    fn owned_bytes(&self) -> usize { 0 }
}

impl AccountedSize for String {
    fn owned_bytes(&self) -> usize { self.capacity() }
}

pub fn reachable_bytes(value: &dyn AccountedSize) -> usize {
    value.owned_bytes()
}

/// Replace an owner's explicit size callback. Shared allocations must be assigned to one owner.
pub fn root(owner: &Owner, roots: Arc<dyn Fn() -> usize + Send + Sync + 'static>) {
    let old = state().roots.insert(owner.id(), (owner.clone(), roots));
    drop(old);
}

struct Table {
    owner: Owner,
    value: Weak<dyn AccountedSize + Send + Sync>,
    uncharged: Arc<dyn Fn() -> bool + Send + Sync>,
}

/// Track an uncharged table weakly. Its explicit size is charged while `uncharged` is true.
pub fn walk_table(
    owner: &Owner,
    table: Arc<dyn AccountedSize + Send + Sync + 'static>,
    uncharged: Arc<dyn Fn() -> bool + Send + Sync + 'static>,
) {
    let weak = Arc::downgrade(&table);
    let mut state = state();
    state.tables.push(Table {
        owner: owner.clone(),
        value: weak,
        uncharged,
    });
    if state.tables.len() % 256 == 0 {
        state.tables.retain(|table| table.value.strong_count() > 0);
    }
}

fn walk_roots() {
    let _walk = WALK_LOCK.lock().unwrap_or_else(|error| error.into_inner());

    let (roots, tables) = {
        let state = state();
        (
            state.roots.values().cloned().collect::<Vec<_>>(),
            state
                .tables
                .iter()
                .map(|table| Table {
                    owner: table.owner.clone(),
                    value: table.value.clone(),
                    uncharged: table.uncharged.clone(),
                })
                .collect::<Vec<_>>(),
        )
    };
    let mut current: HashMap<usize, (Owner, i64)> = HashMap::new();
    for (owner, size) in roots {
        let bytes = size().min(i64::MAX as usize) as i64;
        let id = owner.id();
        current
            .entry(id)
            .and_modify(|entry| entry.1 = entry.1.saturating_add(bytes))
            .or_insert((owner, bytes));
    }
    for table in &tables {
        if !(table.uncharged)() {
            continue;
        }
        if let Some(value) = table.value.upgrade() {
            let bytes = value.owned_bytes().min(i64::MAX as usize) as i64;
            current
                .entry(table.owner.id())
                .and_modify(|entry| entry.1 = entry.1.saturating_add(bytes))
                .or_insert((table.owner.clone(), bytes));
        }
    }
    let previous = {
        let mut state = state();
        state.tables.retain(|table| table.value.strong_count() > 0);
        std::mem::replace(&mut state.walked, current.clone())
    };
    for (id, (owner, before)) in &previous {
        let now = current.get(id).map_or(0, |(_, bytes)| *bytes);
        if *before > now {
            release(owner, *before - now);
        }
    }
    for (id, (owner, now)) in &current {
        let before = previous.get(id).map_or(0, |(_, bytes)| *bytes);
        if *now > before {
            charge(owner, *now - before);
        }
    }
}
fn accounts_sum(pool: Option<Pool>) -> i64 {
    state()
        .accounts
        .values()
        .flat_map(|accounts| accounts.values())
        .filter(|owner| pool.is_none_or(|pool| owner.pool() == pool))
        .map(Owner::bytes)
        .fold(0, i64::saturating_add)
}

pub fn attributed() -> i64 {
    accounts_sum(None)
}

pub fn attributed_device() -> i64 {
    accounts_sum(Some(Pool::Device))
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Source {
    Machine,
    Preference,
    Override,
}

impl Source {
    fn rank(self) -> u8 {
        match self {
            Source::Machine => 1,
            Source::Preference => 2,
            Source::Override => 3,
        }
    }
}

#[derive(Default)]
struct Control {
    budget: i64,
    budget_rank: u8,
    device_budget: i64,
    device_rank: u8,
    clobber: bool,
    lowered: i64,
    seen_pressure: i64,
    overdraft: Option<Overdraft>,
    refused_under: i64,
    refused_device_under: i64,
    shed_bytes: i64,
    shed_epoch: i64,
    near_line: Option<String>,
}

static CONTROL: LazyLock<Mutex<Control>> = LazyLock::new(|| Mutex::new(Control::default()));

fn control() -> std::sync::MutexGuard<'static, Control> {
    CONTROL.lock().unwrap_or_else(|error| error.into_inner())
}

pub fn set_budget(n: i64) {
    set_budget_from(Source::Override, n);
}

pub fn set_budget_from(source: Source, n: i64) {
    let mut control = control();
    if source.rank() >= control.budget_rank {
        control.budget_rank = source.rank();
        control.budget = n.max(0);
    }
}

pub fn set_device_budget(n: i64) {
    set_device_budget_from(Source::Override, n);
}

pub fn set_device_budget_from(source: Source, n: i64) {
    let mut control = control();
    if source.rank() >= control.device_rank {
        control.device_rank = source.rank();
        control.device_budget = n.max(0);
    }
}

pub fn budget() -> i64 {
    control().budget
}


pub fn device_budget() -> i64 {
    control().device_budget
}

pub fn machine_budget(working_set: i64) -> i64 {
    let ram = mem_sys::physical_ram().saturating_mul(6) / 10;
    match (working_set > 0, ram > 0) {
        (true, true) => working_set.min(ram),
        (true, false) => working_set,
        (false, _) => ram,
    }
}

fn parse_budget(text: &str) -> Option<i64> {
    if text.is_empty() {
        return None;
    }
    let (digits, scale) = match text.as_bytes()[text.len() - 1].to_ascii_uppercase() {
        b'G' => (&text[..text.len() - 1], 1_000_000_000),
        b'M' => (&text[..text.len() - 1], 1_000_000),
        b'K' => (&text[..text.len() - 1], 1_000),
        _ => (text, 1),
    };
    let clean = digits.replace('_', "");
    let (negative, digits) = match clean.strip_prefix('-') {
        Some(digits) => (true, digits),
        None => (false, clean.strip_prefix('+').unwrap_or(&clean)),
    };
    let (radix, digits) = if digits.starts_with("0x") || digits.starts_with("0X") {
        (16, &digits[2..])
    } else if digits.starts_with("0o") || digits.starts_with("0O") {
        (8, &digits[2..])
    } else if digits.starts_with("0b") || digits.starts_with("0B") {
        (2, &digits[2..])
    } else {
        (10, digits)
    };
    let number = u64::from_str_radix(digits, radix).ok()?;
    let max_int = (i64::MAX >> 1) as u64;
    if number > max_int || (negative && number != 0) {
        return None;
    }
    (number as i64).checked_mul(scale).filter(|n| *n <= max_int as i64)
}

pub fn configure(read: &dyn Fn(&str) -> Option<String>) -> Result<(), String> {
    let clobber = read("RAYS_MEM_CLOBBER").as_deref() == Some("1");
    control().clobber = clobber;
    match read("RAYS_MEM_BUDGET") {
        None => Ok(()),
        Some(text) if text.is_empty() => Ok(()),
        Some(text) => match parse_budget(&text) {
            Some(n) => {
                set_budget(n);
                Ok(())
            }
            None => Err(format!("RAYS_MEM_BUDGET: {text:?} is not a size like 4G, 200M or 1000")),
        },
    }
}

pub fn clobber() -> bool {
    control().clobber
}

pub struct EvictorOptions {
    pub rebuild: f64,
    pub held: Option<Arc<dyn Fn() -> i64 + Send + Sync>>,
    pub last_use: Option<Arc<dyn Fn() -> i64 + Send + Sync>>,
    pub live: Option<Arc<dyn Fn() -> bool + Send + Sync>>,
    pub forget: Option<Arc<dyn Fn() + Send + Sync>>,
}

impl Default for EvictorOptions {
    fn default() -> Self {
        Self {
            rebuild: 1.0,
            held: None,
            last_use: None,
            live: None,
            forget: None,
        }
    }
}

struct Evictor {
    owner: Owner,
    rebuild: f64,
    held: Arc<dyn Fn() -> i64 + Send + Sync>,
    last_use: Arc<dyn Fn() -> i64 + Send + Sync>,
    live: Arc<dyn Fn() -> bool + Send + Sync>,
    forget: Arc<dyn Fn() + Send + Sync>,
    drop_entry: Arc<dyn Fn() -> bool + Send + Sync>,
}

pub fn register_evictor(
    owner: &Owner,
    options: EvictorOptions,
    drop_entry: Arc<dyn Fn() -> bool + Send + Sync + 'static>,
) {
    if owner.kind() != Kind::Cache {
        return;
    }
    let evictor = Arc::new(Evictor {
        owner: owner.clone(),
        rebuild: options.rebuild,
        held: options.held.unwrap_or_else(|| {
            let owner = owner.clone();
            Arc::new(move || owner.bytes())
        }),
        last_use: options.last_use.unwrap_or_else(|| Arc::new(|| 0)),
        live: options.live.unwrap_or_else(|| Arc::new(|| true)),
        forget: options.forget.unwrap_or_else(|| Arc::new(|| {})),
        drop_entry,
    });
    let should_prune = {
        let mut state = state();
        state.evictors.insert(0, evictor);
        state.evictors.len() > state.evictor_mark
    };
    if should_prune {
        prune_evictors();
    }
}

fn prune_evictors() {
    let snapshot = state().evictors.clone();
    let mut dead = snapshot.into_iter().filter(|entry| !(entry.live)()).collect::<Vec<_>>();
    {
        let mut state = state();
        dead.retain(|entry| state.evictors.iter().any(|item| Arc::ptr_eq(item, entry)));
        state.evictors.retain(|entry| !dead.iter().any(|item| Arc::ptr_eq(item, entry)));
        state.evictor_mark = 16.max(2 * state.evictors.len());
    }
    for entry in dead {
        (entry.forget)();
    }
}

pub fn evictor_count() -> usize {
    state().evictors.len()
}

static EPOCH: AtomicI64 = AtomicI64::new(0);

pub fn epoch() -> i64 {
    EPOCH.load(Ordering::Relaxed)
}

fn top_owners(pool: Option<Pool>, n: usize) -> Vec<(String, i64)> {
    let mut owners = state()
        .accounts
        .values()
        .flat_map(|accounts| accounts.values())
        .filter(|owner| pool.is_none_or(|pool| owner.pool() == pool))
        .map(|owner| (owner.name().to_owned(), owner.bytes()))
        .filter(|(_, bytes)| *bytes > 0)
        .collect::<Vec<_>>();
    owners.sort_by(|(name_a, bytes_a), (name_b, bytes_b)| {
        bytes_b.cmp(bytes_a).then_with(|| name_a.cmp(name_b))
    });
    owners.truncate(n);
    owners
}

pub fn format_bytes(n: i64) -> String {
    if n >= 1_000_000_000 {
        format!("{:.2} GB", n as f64 / 1e9)
    } else if n >= 1_000_000 {
        format!("{:.1} MB", n as f64 / 1e6)
    } else if n >= 1_000 {
        format!("{:.1} KB", n as f64 / 1e3)
    } else {
        format!("{n} B")
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Overdraft {
    pub used: i64,
    pub limit: i64,
    pub top: Vec<(String, i64)>,
}

pub fn describe(overdraft: &Overdraft) -> String {
    let top = if overdraft.top.is_empty() {
        "none attributed".to_owned()
    } else {
        overdraft
            .top
            .iter()
            .map(|(name, bytes)| format!("{name} {}", format_bytes(*bytes)))
            .collect::<Vec<_>>()
            .join(", ")
    };
    format!(
        "{} in use over the {} budget; largest owners: {}",
        format_bytes(overdraft.used),
        format_bytes(overdraft.limit),
        top
    )
}

pub fn total_owner() -> Option<(String, i64)> {
    top_owners(None, 1).into_iter().next()
}

fn measured(device: Option<&(dyn Fn() -> i64 + Send + Sync)>) -> i64 {
    mem_sys::phys_footprint().unwrap_or_else(|| {
        accounts_sum(Some(Pool::Host)).saturating_add(device.map_or(0, |read| read().max(0)))
    })
}

fn credit() -> i64 {
    let mut control = control();
    if control.shed_bytes > 0 && control.shed_epoch == epoch() {
        control.shed_bytes
    } else {
        control.shed_bytes = 0;
        0
    }
}

fn note_shed(n: i64) {
    if n <= 0 {
        return;
    }
    let now = epoch();
    let mut control = control();
    if control.shed_epoch != now {
        control.shed_bytes = 0;
        control.shed_epoch = now;
    }
    control.shed_bytes = control.shed_bytes.saturating_add(n);
}

fn limit() -> i64 {
    let control = control();
    if control.lowered > 0 && (control.budget == 0 || control.lowered < control.budget) {
        control.lowered
    } else {
        control.budget
    }
}

fn apply_pressure() -> bool {
    let state = mem_sys::pressure_state();
    let mut control = control();
    if state == control.seen_pressure {
        return false;
    }
    control.seen_pressure = state;
    match state & 3 {
        0 => control.lowered = 0,
        level => {
            control.lowered = measured(None).saturating_mul(9) / 10;
            control.lowered = control.lowered.max(1);
            return level == 2;
        }
    }
    false
}

fn note_near(used: i64, bound: i64) {
    let mut control = control();
    control.near_line = if bound > 0 && used / 3 >= bound / 4 {
        let owner = total_owner()
            .map(|(name, bytes)| format!(" · top {name} {}", format_bytes(bytes)))
            .unwrap_or_default();
        Some(format!("mem {} of {}{}", format_bytes(used), format_bytes(bound), owner))
    } else {
        None
    };
}

pub fn status_line() -> Option<String> {
    control().near_line.clone()
}

fn drop_coldest(pool: Option<Pool>, refused: &mut HashSet<usize>) -> bool {
    let now = epoch();
    let evictors = state().evictors.clone();
    let mut ranked = evictors
        .into_iter()
        .filter_map(|entry| {
            let id = Arc::as_ptr(&entry) as usize;
            if refused.contains(&id)
                || pool.is_some_and(|pool| entry.owner.pool() != pool)
                || !(entry.live)()
            {
                return None;
            }
            let held = (entry.held)().max(0);
            let age = now.saturating_sub((entry.last_use)()).max(0);
            let score = (1.0 + age as f64) * held as f64 / entry.rebuild;
            (score > 0.0).then_some((score, id, entry))
        })
        .collect::<Vec<_>>();
    ranked.sort_by(|a, b| b.0.total_cmp(&a.0));
    for (_, id, entry) in ranked {
        if (entry.drop_entry)() {
            return true;
        }
        refused.insert(id);
    }
    false
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Verdict {
    Within,
    Over(Overdraft),
}

#[derive(Default)]
pub struct RetreatOptions {
    pub device: Option<Arc<dyn Fn() -> i64 + Send + Sync>>,
    pub usage: Option<Arc<dyn Fn() -> i64 + Send + Sync>>,
    pub later: Vec<Arc<dyn Fn() -> bool + Send + Sync>>,
}

pub fn retreat() -> Verdict {
    retreat_with(RetreatOptions::default())
}

pub fn retreat_with(options: RetreatOptions) -> Verdict {
    EPOCH.fetch_add(1, Ordering::Relaxed);
    let critical = apply_pressure();
    let bound = limit();
    let device_bound = device_budget();
    let host_pool = (device_bound > 0).then_some(Pool::Host);
    let held = || accounts_sum(host_pool);
    let read_usage = || match &options.usage {
        Some(read) => read(),
        None => measured(if device_bound > 0 { None } else { options.device.as_deref() }),
    };

    let host = if bound <= 0 {
        control().near_line = None;
        Verdict::Within
    } else {
        let used = read_usage().saturating_sub(if options.usage.is_none() { credit() } else { 0 });
        note_near(used, bound);
        if used <= bound {
            Verdict::Within
        } else {
            let before = held();
            let excess = used.saturating_sub(bound);
            let mut refused = HashSet::new();
            while before.saturating_sub(held()) < excess && drop_coldest(host_pool, &mut refused) {}
            for step in &options.later {
                while before.saturating_sub(held()) < excess && step() {}
            }
            let dropped = before.saturating_sub(held()).max(0);
            note_shed(dropped);
            if dropped >= excess {
                Verdict::Within
            } else {
                let used = used.saturating_sub(dropped);
                Verdict::Over(Overdraft { used, limit: bound, top: top_owners(None, 4) })
            }
        }
    };

    let verdict = match host {
        over @ Verdict::Over(_) => over,
        Verdict::Within if device_bound > 0 => match &options.device {
            Some(read) => {
                let used = read();
                if used <= device_bound {
                    Verdict::Within
                } else {
                    let before = attributed_device();
                    let excess = used.saturating_sub(device_bound);
                    let mut refused = HashSet::new();
                    while before.saturating_sub(attributed_device()) < excess
                        && drop_coldest(Some(Pool::Device), &mut refused)
                    {}
                    let used = used.saturating_sub(before.saturating_sub(attributed_device()).max(0));
                    if used <= device_bound {
                        Verdict::Within
                    } else {
                        Verdict::Over(Overdraft {
                            used,
                            limit: device_bound,
                            top: top_owners(Some(Pool::Device), 4),
                        })
                    }
                }
            }
            None => Verdict::Within,
        },
        Verdict::Within => Verdict::Within,
    };

    if critical {
        mem_sys::pressure_relief();
    }
    control().overdraft = match &verdict {
        Verdict::Within => None,
        Verdict::Over(overdraft) => Some(overdraft.clone()),
    };
    verdict
}

pub fn overdraft() -> Option<Overdraft> {
    control().overdraft.clone()
}

pub fn retry_worthwhile() -> bool {
    let control = control();
    let bound = if control.lowered > 0 && (control.budget == 0 || control.lowered < control.budget) {
        control.lowered
    } else {
        control.budget
    };
    (bound <= 0 || bound > control.refused_under)
        && (control.device_budget <= 0 || control.device_budget > control.refused_device_under)
}

pub fn admit() -> Result<(), Overdraft> {
    let mut shed = || false;
    admit_with(&mut shed)
}

pub fn admit_with(shed: &mut dyn FnMut() -> bool) -> Result<(), Overdraft> {
    let bound = limit();
    if bound <= 0 || measured(None) <= bound {
        return Ok(());
    }
    let before = attributed();
    while before.saturating_sub(attributed()) < measured(None).saturating_sub(bound) && shed() {}
    note_shed(before.saturating_sub(attributed()).max(0));
    let used = measured(None);
    if used <= bound {
        Ok(())
    } else {
        let overdraft = Overdraft { used, limit: bound, top: top_owners(None, 4) };
        control().refused_under = bound;
        Err(overdraft)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Refused(pub Overdraft);

impl fmt::Display for Refused {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&describe(&self.0))
    }
}

impl std::error::Error for Refused {}

pub fn admit_bytes(pool: Pool, n: i64) -> Result<(), Refused> {
    let device_bound = device_budget();
    if pool == Pool::Device && device_bound > 0 {
        let used = attributed_device().saturating_add(n);
        if used > device_bound {
            let overdraft = Overdraft { used, limit: device_bound, top: top_owners(Some(Pool::Device), 4) };
            control().refused_device_under = device_bound;
            return Err(Refused(overdraft));
        }
        return Ok(());
    }
    let bound = limit();
    if bound <= 0 || measured(None).saturating_add(n) <= bound {
        return Ok(());
    }
    let used = measured(None).saturating_add(n);
    if used > bound {
        let overdraft = Overdraft { used, limit: bound, top: top_owners(None, 4) };
        control().refused_under = bound;
        Err(Refused(overdraft))
    } else {
        Ok(())
    }
}

pub struct ReportOptions {
    pub walk: bool,
    pub device: i64,
    pub budget: Option<i64>,
}

impl Default for ReportOptions {
    fn default() -> Self {
        Self { walk: true, device: 0, budget: None }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReportEntry {
    pub name: String,
    pub kind: Kind,
    pub pool: Pool,
    pub bytes: i64,
    pub peak: i64,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Report {
    pub owners: Vec<ReportEntry>,
    pub attributed: i64,
    pub attributed_device: i64,
    /// Rust allocator-requested live bytes.
    pub live_bytes: i64,
    pub device: i64,
    pub footprint: i64,
    pub unattributed: i64,
    pub overclaimed: i64,
    pub budget: i64,
}

impl Report {
    fn kind_label(kind: Kind) -> &'static str {
        match kind {
            Kind::Cache => "cache",
            Kind::Plan => "plan",
            Kind::Document => "document",
        }
    }

    fn pool_label(pool: Pool) -> &'static str {
        match pool {
            Pool::Host => "host",
            Pool::Device => "device",
        }
    }

    pub fn format_bytes(n: i64) -> String {
        format_bytes(n)
    }

    pub fn pp(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for entry in &self.owners {
            writeln!(
                f,
                "{:<8}  {:<6}  {:<30}  {:>10}  {:>10}",
                Self::kind_label(entry.kind),
                Self::pool_label(entry.pool),
                entry.name,
                format_bytes(entry.bytes),
                format_bytes(entry.peak),
            )?;
        }
        let explicit = self.live_bytes.saturating_add(self.device);
        let pct = if explicit <= 0 {
            0
        } else {
            (100.0 * self.unattributed as f64 / explicit as f64) as i64
        };
        writeln!(
            f,
            "live bytes {} · device {} · footprint {} · unattributed {} ({}% of explicit){} · budget {}",
            format_bytes(self.live_bytes),
            format_bytes(self.device),
            format_bytes(self.footprint),
            format_bytes(self.unattributed),
            pct,
            if self.overclaimed == 0 { String::new() } else { format!(" · overclaimed {}", format_bytes(self.overclaimed)) },
            format_bytes(self.budget),
        )
    }

    pub fn to_string(&self) -> String {
        format!("{self}")
    }
}

pub fn report() -> Report {
    report_with(ReportOptions::default())
}

pub fn report_with(options: ReportOptions) -> Report {
    let footprint = mem_sys::phys_footprint();
    let live = mem_sys::allocated_bytes().min(i64::MAX as u64) as i64;
    if options.walk {
        walk_roots();
    }
    prune_evictors();
    let accounts = state()
        .accounts
        .values()
        .flat_map(|accounts| accounts.values())
        .cloned()
        .collect::<Vec<_>>();
    let mut owners = accounts
        .into_iter()
        .filter(|owner| owner.peak() != 0 || owner.bytes() != 0)
        .map(|owner| ReportEntry {
            name: owner.name().to_owned(),
            kind: owner.kind(),
            pool: owner.pool(),
            bytes: owner.bytes(),
            peak: owner.peak(),
        })
        .collect::<Vec<_>>();
    owners.sort_by(|a, b| b.bytes.cmp(&a.bytes).then_with(|| a.name.cmp(&b.name)));
    let host = owners.iter().filter(|entry| entry.pool == Pool::Host).map(|e| e.bytes).fold(0, i64::saturating_add);
    let attributed_device = owners.iter().filter(|entry| entry.pool == Pool::Device).map(|e| e.bytes).fold(0, i64::saturating_add);
    let device = options.device.max(0);
    let footprint = footprint.unwrap_or_else(|| live.saturating_add(device));
    Report {
        attributed: host.saturating_add(attributed_device),
        attributed_device,
        live_bytes: live,
        device,
        footprint,
        unattributed: live.saturating_sub(host).max(0).saturating_add(device.saturating_sub(attributed_device).max(0)),
        overclaimed: host.saturating_sub(live).max(0).saturating_add(attributed_device.saturating_sub(device).max(0)),
        budget: options.budget.unwrap_or_else(budget).max(0),
        owners,
    }
}

impl fmt::Display for Report {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pp(f)
    }
}

pub fn reset() {
    let discarded = {
        let mut state = state();
        for accounts in state.accounts.values() {
            for owner in accounts.values() {
                owner.0.bytes.store(0, Ordering::Relaxed);
                owner.0.peak.store(0, Ordering::Relaxed);
            }
        }
        state.walked.clear();
        (
            std::mem::take(&mut state.evictors),
            std::mem::take(&mut state.roots),
            std::mem::take(&mut state.tables),
        )
    };
    drop(discarded);
    *control() = Control::default();
}

pub fn phys_footprint() -> Option<i64> {
    mem_sys::phys_footprint()
}

pub fn peak_footprint() -> Option<i64> {
    mem_sys::peak_footprint()
}

pub fn instructions() -> Option<i64> {
    mem_sys::instructions()
}

pub fn physical_ram() -> i64 {
    mem_sys::physical_ram()
}

pub fn pressure_start() -> bool {
    mem_sys::pressure_start()
}

pub fn allocated_bytes() -> i64 {
    mem_sys::allocated_bytes().min(i64::MAX as u64) as i64
}

pub fn peak_allocated_bytes() -> i64 {
    mem_sys::peak_allocated_bytes().min(i64::MAX as u64) as i64
}

pub mod private {
    pub fn pressure_inject(level: i32) {
        mem_sys::pressure_inject(level);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering as AtomicOrdering};
    use std::sync::{Barrier, MutexGuard};
    static TEST_LOCK: Mutex<()> = Mutex::new(());

    fn setup() -> MutexGuard<'static, ()> {
        let guard = TEST_LOCK.lock().unwrap_or_else(|error| error.into_inner());
        reset();
        guard
    }

    #[test]
    fn owner_sharing_and_atomic_charges() {
        let _guard = setup();
        let owner = cache("ledger.shared");
        assert_eq!(owner, cache("ledger.shared"));
        assert_ne!(owner, plan("ledger.shared"));
        assert_ne!(owner, cache_in(Pool::Device, "ledger.shared"));
        assert_eq!(name(&owner), "ledger.shared");
        assert_eq!(kind(&owner), Kind::Cache);
        assert_eq!(pool(&owner), Pool::Host);
        let threads = (0..4)
            .map(|_| {
                let owner = owner.clone();
                std::thread::spawn(move || {
                    for _ in 0..20_000 {
                        charge(&owner, 3);
                        release(&owner, 1);
                    }
                })
            })
            .collect::<Vec<_>>();
        for thread in threads {
            thread.join().unwrap();
        }
        assert_eq!(owner.bytes(), 160_000);
        assert!((160_000..=240_000).contains(&owner.peak()));
        let peak_before_release = owner.peak();
        release(&owner, i64::MAX);
        assert_eq!(owner.bytes(), 0);
        assert_eq!(owner.peak(), peak_before_release);
        let peak_owner = cache("peak");
        charge(&peak_owner, 1_000);
        charge(&peak_owner, 500);
        release(&peak_owner, 400);
        assert_eq!(peak_owner.bytes(), 1_100);
        assert_eq!(peak_owner.peak(), 1_500);

        let workers = (0..4)
            .map(|_| {
                std::thread::spawn(|| {
                    for i in 0..2_000 {
                        let owner = cache(&format!("shared.{}", i % 7));
                        charge(&owner, 1);
                        if i % 100 == 0 {
                            register_evictor(&owner, EvictorOptions::default(), Arc::new(|| false));
                        }
                        let _ = attributed();
                    }
                })
            })
            .collect::<Vec<_>>();
        for worker in workers {
            worker.join().unwrap();
        }
        assert_eq!(cache("shared.0").bytes(), 4 * (2_000 / 7 + 1));
        assert_eq!(evictor_count(), 80);
    }
    #[test]
    fn explicit_roots_and_weak_tables() {
        let _guard = setup();
        struct Bytes(usize);
        impl AccountedSize for Bytes {
            fn owned_bytes(&self) -> usize { self.0 }
        }
        let named = plan("rooted");
        root(&named, Arc::new(|| 200));
        let table_owner = cache("table");
        let table = Arc::new(Bytes(300));
        walk_table(&table_owner, table.clone(), Arc::new(|| true));
        report();
        assert_eq!(named.bytes(), 200);
        assert_eq!(table_owner.bytes(), 300);
        root(&named, Arc::new(|| 0));
        drop(table);
        report();
        assert_eq!(named.bytes(), 0);
        assert_eq!(table_owner.bytes(), 0);

        let charged = cache("charged-table");
        charge(&charged, 400);
        let charged_table = Arc::new(Bytes(999));
        walk_table(&charged, charged_table.clone(), Arc::new(|| false));
        report();
        assert_eq!(charged.bytes(), 400);
        drop(charged_table);
        report();
        assert_eq!(charged.bytes(), 400);

        let measured = Bytes(500);
        assert_eq!(reachable_bytes(&measured), 500);
    }

    #[test]
    fn concurrent_root_walks_apply_one_consistent_snapshot() {
        let _guard = setup();
        let owner = plan("concurrent.roots");
        let calls = Arc::new(AtomicUsize::new(0));
        let callback_calls = calls.clone();
        let lock_checks = Arc::new(AtomicUsize::new(0));
        let callback_lock_checks = lock_checks.clone();
        root(&owner, Arc::new(move || {
            if WALK_LOCK.try_lock().is_err() {
                callback_lock_checks.fetch_add(1, AtomicOrdering::Relaxed);
            }
            if callback_calls.fetch_add(1, AtomicOrdering::Relaxed) == 0 {
                0
            } else {
                100
            }
        }));
        let start = Arc::new(Barrier::new(3));
        let workers = (0..2).map(|_| {
            let start = start.clone();
            std::thread::spawn(move || {
                start.wait();
                report();
            })
        }).collect::<Vec<_>>();
        start.wait();
        for worker in workers {
            worker.join().unwrap();
        }
        assert_eq!(owner.bytes(), 100);
        assert_eq!(lock_checks.load(AtomicOrdering::Relaxed), 2);
    }

    #[test]
    fn budget_precedence_and_configuration() {
        let _guard = setup();
        set_budget_from(Source::Machine, 100);
        set_budget_from(Source::Preference, 200);
        set_budget_from(Source::Machine, 300);
        assert_eq!(budget(), 200);
        set_device_budget_from(Source::Machine, 100);
        set_device_budget_from(Source::Preference, 200);
        set_device_budget_from(Source::Machine, 300);
        assert_eq!(device_budget(), 200);
        assert_eq!(configure(&|name| (name == "RAYS_MEM_BUDGET").then(|| "4G".to_owned())), Ok(()));
        assert_eq!(budget(), 4_000_000_000);
        set_budget_from(Source::Preference, 1);
        assert_eq!(budget(), 4_000_000_000);
        assert_eq!(
            configure(&|name| (name == "RAYS_MEM_BUDGET").then(|| "lots".to_owned())),
            Err("RAYS_MEM_BUDGET: \"lots\" is not a size like 4G, 200M or 1000".to_owned())
        );
        assert_eq!(parse_budget("1_000"), Some(1_000));
        assert_eq!(parse_budget("+0x10"), Some(16));
        assert_eq!(parse_budget("2K"), Some(2_000));
        assert_eq!(parse_budget("-1"), None);
        assert_eq!(parse_budget("5_000_000_000G"), None);
        assert_eq!(machine_budget(0), physical_ram().saturating_mul(6) / 10);
    }

    #[test]
    fn owner_bytes_drive_report_and_formatting() {
        let _guard = setup();
        let owner = cache("report.cache");
        charge(&owner, 1_500_000);
        let document = document("report.document");
        charge(&document, 12);
        let result = report_with(ReportOptions { walk: false, device: 64, budget: Some(1_000_000_000) });
        assert_eq!(result.attributed, 1_500_012);
        assert_eq!(result.attributed_device, 0);
        assert_eq!(result.device, 64);
        assert_eq!(result.budget, 1_000_000_000);
        assert_eq!(result.owners.len(), 2);
        assert!(result.live_bytes > 0);
        assert!(result.footprint > 0);
        assert_eq!(format_bytes(999), "999 B");
        assert_eq!(format_bytes(1_000), "1.0 KB");
        assert!(result.to_string().contains("live bytes"));
        assert!(result.owners.iter().any(|entry| entry.name == "report.cache" && entry.peak == 1_500_000));
        reset();
        assert_eq!(owner.bytes(), 0);
        assert_eq!(owner.peak(), 0);
        assert_eq!(report().attributed, 0);
    }

    #[test]
    fn budget_retreat_latches_and_recovers() {
        let _guard = setup();
        set_budget(1_000);
        let owner = cache("retreat.cache");
        let left = Arc::new(AtomicUsize::new(3));
        charge(&owner, 1_200);
        let left_for_drop = left.clone();
        let drop_owner = owner.clone();
        let drop_entry: Arc<dyn Fn() -> bool + Send + Sync> = Arc::new(move || {
            if left_for_drop
                .try_update(AtomicOrdering::Relaxed, AtomicOrdering::Relaxed, |n| {
                    if n > 0 { Some(n - 1) } else { None }
                })
                .is_ok()
            {
                release(&drop_owner, 400);
                true
            } else {
                false
            }
        });
        register_evictor(&owner, EvictorOptions::default(), drop_entry);
        let usage: Arc<dyn Fn() -> i64 + Send + Sync> = Arc::new(|| attributed());
        assert_eq!(retreat_with(RetreatOptions { usage: Some(usage.clone()), ..RetreatOptions::default() }), Verdict::Within);
        assert_eq!(owner.bytes(), 800);
        set_budget(100);
        left.store(0, AtomicOrdering::Relaxed);
        assert_eq!(retreat_with(RetreatOptions { usage: Some(usage.clone()), ..RetreatOptions::default() }), Verdict::Over(Overdraft { used: 800, limit: 100, top: vec![("retreat.cache".to_owned(), 800)] }));
        assert!(overdraft().is_some());
        set_budget(5_000);
        assert_eq!(retreat_with(RetreatOptions { usage: Some(usage), ..RetreatOptions::default() }), Verdict::Within);
        assert!(overdraft().is_none());
    }

    #[test]
    fn eviction_order_busy_skip_and_later_steps() {
        let _guard = setup();
        for _ in 0..10 { retreat(); }
        let now = epoch();
        let order = Arc::new(Mutex::new(Vec::<String>::new()));
        let mut owners = Vec::new();
        for (name, size, rebuild, last_use) in [
            ("warm", 1_000, 1.0, now),
            ("cold", 200, 1.0, now - 10),
            ("costly", 1_000, 100.0, now),
        ] {
            let owner = cache(name);
            charge(&owner, size);
            let order_drop = order.clone();
            let owner_drop = owner.clone();
            register_evictor(&owner, EvictorOptions {
                rebuild,
                last_use: Some(Arc::new(move || last_use)),
                ..EvictorOptions::default()
            }, Arc::new(move || {
                order_drop.lock().unwrap().push(owner_drop.name().to_owned());
                release(&owner_drop, size);
                true
            }));
            owners.push(owner);
        }
        set_budget(1);
        let step = |name: &'static str| -> Arc<dyn Fn() -> bool + Send + Sync> {
            let order = order.clone();
            Arc::new(move || {
                let mut order = order.lock().unwrap();
                if order.iter().any(|item| item.as_str() == name) {
                    false
                } else {
                    order.push(name.to_owned());
                    true
                }
            })
        };
        let plans = step("plans");
        let undo = step("undo");
        let _ = retreat_with(RetreatOptions {
            usage: Some(Arc::new(|| 2_300)),
            later: vec![plans, undo],
            ..RetreatOptions::default()
        });
        assert_eq!(*order.lock().unwrap(), vec!["cold".to_owned(), "warm".to_owned(), "costly".to_owned(), "plans".to_owned(), "undo".to_owned()]);
        assert_eq!(owners.iter().map(Owner::bytes).sum::<i64>(), 0);

        reset();
        set_budget(1);
        let busy = cache("busy");
        let idle = cache("idle");
        charge(&busy, 900);
        charge(&idle, 100);
        let tried = Arc::new(AtomicUsize::new(0));
        let tried_ref = tried.clone();
        register_evictor(&busy, EvictorOptions::default(), Arc::new(move || {
            tried_ref.fetch_add(1, AtomicOrdering::Relaxed);
            false
        }));
        let idle_ref = idle.clone();
        register_evictor(&idle, EvictorOptions::default(), Arc::new(move || {
            release(&idle_ref, 100);
            true
        }));
        let _ = retreat_with(RetreatOptions {
            usage: Some(Arc::new(|| 2)),
            ..RetreatOptions::default()
        });
        assert_eq!(tried.load(AtomicOrdering::Relaxed), 1);
        assert_eq!(idle.bytes(), 0);
        assert_eq!(busy.bytes(), 900);
    }

    #[test]
    fn device_budget_isolated_from_host_pool() {
        let _guard = setup();
        let vram = cache_in(Pool::Device, "vram");
        let ram = cache("ram");
        charge(&vram, 1_200);
        charge(&ram, 1_200);
        let drops = Arc::new(AtomicUsize::new(0));
        let drops_ref = drops.clone();
        let vram_ref = vram.clone();
        register_evictor(&vram, EvictorOptions::default(), Arc::new(move || {
            drops_ref.fetch_add(1, AtomicOrdering::Relaxed);
            release(&vram_ref, 400);
            true
        }));
        let ram_drops = Arc::new(AtomicUsize::new(0));
        let ram_count = ram_drops.clone();
        register_evictor(&ram, EvictorOptions::default(), Arc::new(move || {
            ram_count.fetch_add(1, AtomicOrdering::Relaxed);
            false
        }));
        set_device_budget(1_000);
        let device_owner = vram.clone();
        let device: Arc<dyn Fn() -> i64 + Send + Sync> = Arc::new(move || device_owner.bytes());
        let usage: Arc<dyn Fn() -> i64 + Send + Sync> = Arc::new(|| 0);
        assert_eq!(
            retreat_with(RetreatOptions { device: Some(device), usage: Some(usage), ..RetreatOptions::default() }),
            Verdict::Within
        );
        assert_eq!(drops.load(AtomicOrdering::Relaxed), 1);
        assert_eq!(ram_drops.load(AtomicOrdering::Relaxed), 0);
        set_device_budget(100);
        assert!(matches!(admit_bytes(Pool::Device, 1), Err(Refused(Overdraft { limit: 100, .. }))));
        assert!(admit_bytes(Pool::Host, 1).is_ok());

        reset();
        let vram = cache_in(Pool::Device, "unowned-vram");
        charge(&vram, 1_200);
        let vram_ref = vram.clone();
        register_evictor(&vram, EvictorOptions::default(), Arc::new(move || {
            let held = vram_ref.bytes();
            if held > 0 {
                release(&vram_ref, 400);
                true
            } else {
                false
            }
        }));
        set_device_budget(100);
        assert_eq!(
            retreat_with(RetreatOptions {
                device: Some(Arc::new(|| 5_000)),
                usage: Some(Arc::new(|| 0)),
                ..RetreatOptions::default()
            }),
            Verdict::Over(Overdraft { used: 3_800, limit: 100, top: vec![] })
        );
    }

    #[test]
    fn admission_and_retry_errors_are_deterministic() {
        let _guard = setup();
        assert!(admit().is_ok());
        let owner = cache("cook.session");
        charge(&owner, 123);
        set_budget(1);
        let calls = AtomicUsize::new(0);
        let mut shed = || {
            calls.fetch_add(1, AtomicOrdering::Relaxed);
            false
        };
        let result = admit_with(&mut shed);
        assert_eq!(calls.load(AtomicOrdering::Relaxed), 1);
        if let Err(overdraft) = result {
            assert_eq!(overdraft.limit, 1);
            assert_eq!(overdraft.top, vec![("cook.session".to_owned(), 123)]);
            assert!(describe(&overdraft).contains("largest owners: cook.session 123 B"));
        } else {
            assert!(mem_sys::phys_footprint().is_some_and(|value| value <= 1));
        }
        assert!(admit_bytes(Pool::Host, i64::MAX).is_err());
        assert!(!retry_worthwhile());
        set_budget(2);
        assert!(retry_worthwhile());
    }

    #[test]
    fn pressure_events_lower_and_restore_limit() {
        let _guard = setup();
        let owner = cache("pressure.cache");
        charge(&owner, 100);
        let gone = owner.clone();
        register_evictor(&owner, EvictorOptions::default(), Arc::new(move || {
            let bytes = gone.bytes();
            release(&gone, bytes);
            bytes > 0
        }));
        private::pressure_inject(1);
        let usage: Arc<dyn Fn() -> i64 + Send + Sync> = Arc::new(|| limit() + 1);
        assert_eq!(
            retreat_with(RetreatOptions { usage: Some(usage.clone()), ..RetreatOptions::default() }),
            Verdict::Within
        );
        assert_eq!(owner.bytes(), 0);
        assert!(status_line().is_some());
        private::pressure_inject(0);
        assert_eq!(retreat(), Verdict::Within);
        charge(&owner, 100);
        private::pressure_inject(2);
        assert_eq!(
            retreat_with(RetreatOptions { usage: Some(usage), ..RetreatOptions::default() }),
            Verdict::Within
        );
        assert_eq!(owner.bytes(), 0);
        private::pressure_inject(0);
        assert_eq!(retreat(), Verdict::Within);
        assert_eq!(device_budget(), 0);
        assert!(!pressure_start() || mem_sys::phys_footprint().is_some());
    }

    #[test]
    fn process_metrics_and_machine_budget_are_available_when_supported() {
        let _guard = setup();
        assert!(physical_ram() >= 0);
        if cfg!(windows) {
            assert!(physical_ram() > 0);
            assert!(phys_footprint().is_some());
        }
        if physical_ram() > 0 {
            assert_eq!(machine_budget(0), physical_ram().saturating_mul(6) / 10);
            assert_eq!(machine_budget(1_000), 1_000);
        }
        assert!(phys_footprint().is_none_or(|value| value > 0));
        assert!(peak_footprint().is_none_or(|value| value > 0));
        if cfg!(target_os = "macos") {
            assert!(instructions().is_some_and(|value| value > 0));
        } else {
            assert!(instructions().is_none());
        }
        let before = allocated_bytes();
        let block = Vec::<u8>::with_capacity(32_768);
        let after = allocated_bytes();
        assert!(after >= before + 32_768);
        assert!(peak_allocated_bytes() >= after);
        drop(block);
        assert!(report().live_bytes > 0);
    }
    #[cfg(any(windows, target_os = "macos"))]
    #[test]
    fn footprint_tracks_touched_allocations() {
        let _guard = setup();
        let before = phys_footprint().expect("process footprint");
        let block = vec![b'x'; 100_000_000];
        let after = phys_footprint().expect("process footprint");
        std::hint::black_box(&block);
        assert!(((after - before) - 100_000_000).abs() < 10_000_000);
    }

    #[test]
    fn dead_evictors_are_forgotten_once() {
        let _guard = setup();
        let owner = cache("dead");
        charge(&owner, 77);
        let forgotten = Arc::new(AtomicUsize::new(0));
        let forgotten_ref = forgotten.clone();
        let owner_forget = owner.clone();
        register_evictor(&owner, EvictorOptions {
            live: Some(Arc::new(|| false)),
            forget: Some(Arc::new(move || {
                forgotten_ref.fetch_add(1, AtomicOrdering::Relaxed);
                release(&owner_forget, 77);
            })),
            ..EvictorOptions::default()
        }, Arc::new(|| true));
        report();
        report();
        assert_eq!(evictor_count(), 0);
        assert_eq!(forgotten.load(AtomicOrdering::Relaxed), 1);
        assert_eq!(owner.bytes(), 0);
    }
    #[test]
    fn concurrent_pruning_forgets_only_removed_evictors() {
        let _guard = setup();
        let owner = cache("dead.concurrent");
        charge(&owner, 77);
        let barrier = Arc::new(Barrier::new(2));
        let barrier_live = barrier.clone();
        let forgotten = Arc::new(AtomicUsize::new(0));
        let forgotten_ref = forgotten.clone();
        let owner_forget = owner.clone();
        register_evictor(&owner, EvictorOptions {
            live: Some(Arc::new(move || {
                barrier_live.wait();
                false
            })),
            forget: Some(Arc::new(move || {
                forgotten_ref.fetch_add(1, AtomicOrdering::Relaxed);
                release(&owner_forget, 77);
            })),
            ..EvictorOptions::default()
        }, Arc::new(|| true));

        let pruners = (0..2).map(|_| std::thread::spawn(prune_evictors)).collect::<Vec<_>>();
        for pruner in pruners {
            pruner.join().unwrap();
        }

        assert_eq!(evictor_count(), 0);
        assert_eq!(forgotten.load(AtomicOrdering::Relaxed), 1);
        assert_eq!(owner.bytes(), 0);
    }

}
