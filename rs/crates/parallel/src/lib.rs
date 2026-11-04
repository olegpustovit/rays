use std::cell::Cell;
use std::mem;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Condvar, LazyLock, Mutex, MutexGuard, OnceLock};
use std::thread;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Context {
    Outside,
    Sequential,
    Pooled(usize),
}

thread_local! {
    static ACTIVE_CONTEXT: Cell<Context> = const { Cell::new(Context::Outside) };
}

struct RestoreContext(Context);

impl Drop for RestoreContext {
    fn drop(&mut self) {
        ACTIVE_CONTEXT.with(|context| context.set(self.0));
    }
}

fn context() -> Context {
    ACTIVE_CONTEXT.with(Cell::get)
}

fn with_context(next: Context, work: &dyn Fn() -> Result<(), String>) -> Result<(), String> {
    let previous = ACTIVE_CONTEXT.with(|context| context.replace(next));
    let _restore = RestoreContext(previous);
    work()
}

pub fn recommended_domains() -> i64 {
    thread::available_parallelism()
        .map(|count| i64::try_from(count.get()).unwrap_or(i64::MAX))
        .unwrap_or(1)
        .max(1)
}

pub fn run_unit(domains: Option<i64>, work: &dyn Fn() -> Result<(), String>) -> Result<(), String> {
    match context() {
        Context::Outside => {
            let available =
                usize::try_from(recommended_domains()).unwrap_or(usize::MAX);
            let requested = domains.map_or(available, |domains| {
                if domains <= 1 {
                    1
                } else {
                    usize::try_from(domains).unwrap_or(usize::MAX)
                }
            });
            let next = if requested <= 1 {
                Context::Sequential
            } else {
                Context::Pooled(requested.min(available))
            };
            with_context(next, work)
        }
        Context::Sequential | Context::Pooled(_) => work(),
    }
}

struct JobHeader {
    chunks: usize,
    width: usize,
    next: AtomicUsize,
    finished: AtomicUsize,
    helpers: AtomicUsize,
    active: AtomicUsize,
    cancelled: AtomicBool,
    failed: Mutex<Option<Failure>>,
    completion_lock: Mutex<()>,
    completion: Condvar,
}

enum FailureKind {
    Callback(String),
    WorkerPanicked,
}

struct Failure {
    chunk: usize,
    kind: FailureKind,
}

impl JobHeader {
    fn new(chunks: usize, width: usize) -> Self {
        Self {
            chunks,
            width,
            next: AtomicUsize::new(0),
            finished: AtomicUsize::new(0),
            helpers: AtomicUsize::new(0),
            active: AtomicUsize::new(0),
            cancelled: AtomicBool::new(false),
            failed: Mutex::new(None),
            completion_lock: Mutex::new(()),
            completion: Condvar::new(),
        }
    }
}

struct Job {
    header: JobHeader,
    callback: *const (dyn Fn(usize) -> Result<(), String> + Send + Sync + 'static),
}

// SAFETY: the callback points to a `Send + Sync` closure borrowed by the owner and is only called
// while the registered job is alive; the header contains only synchronized shared state.
unsafe impl Sync for Job {}

impl Job {
    fn new(
        chunks: usize,
        width: usize,
        callback: &(dyn Fn(usize) -> Result<(), String> + Send + Sync),
    ) -> Self {
        let callback: *const (dyn Fn(usize) -> Result<(), String> + Send + Sync + '_) = callback;
        // SAFETY: the owner keeps the borrowed callback alive until this job is deregistered and
        // every worker that could call it has left.
        let callback = unsafe {
            mem::transmute::<
                *const (dyn Fn(usize) -> Result<(), String> + Send + Sync + '_),
                *const (dyn Fn(usize) -> Result<(), String> + Send + Sync + 'static),
            >(callback)
        };
        Self {
            header: JobHeader::new(chunks, width),
            callback,
        }
    }

    fn run_chunk(&self, chunk: usize) -> Result<(), String> {
        // SAFETY: `Job::new` only stores the borrowed callback for the registration lifetime.
        unsafe { (&*self.callback)(chunk) }
    }
}

#[derive(Clone, Copy)]
struct JobPtr(*const Job);

// SAFETY: the pointer is shared, never moved as an owned `Job`, and stays valid while registered.
// Registration removes it under the pool lock, then drains active helpers before dropping `Job`.
unsafe impl Send for JobPtr {}

impl JobPtr {
    fn header(&self) -> &JobHeader {
        // SAFETY: access is valid while the owner holds the job or while registered; teardown
        // deregisters under the lock and drains active users before the stack job is dropped.
        unsafe { &(*self.0).header }
    }

    fn run_chunk(&self, chunk: usize) -> Result<(), String> {
        // SAFETY: `help` is entered only with the job's active reference held.
        unsafe { (&*self.0).run_chunk(chunk) }
    }

    fn same(self, other: Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

struct State {
    jobs: Vec<JobPtr>,
    stopping: bool,
}

struct Shared {
    state: Mutex<State>,
    wake: Condvar,
}

struct Pool {
    shared: Arc<Shared>,
    worker_count: usize,
    workers: Mutex<Vec<thread::JoinHandle<()>>>,
}

static POOL: LazyLock<Result<Pool, String>> = LazyLock::new(Pool::new);
static SHUTDOWN: OnceLock<Result<(), String>> = OnceLock::new();

unsafe extern "C" {
    fn atexit(callback: extern "C" fn()) -> i32;
}

fn pool() -> Result<&'static Pool, String> {
    let pool = (&*POOL).as_ref().map_err(|error| error.clone())?;
    let shutdown = SHUTDOWN.get_or_init(|| {
        // SAFETY: the callback is registered after the lazy pool has finished initialization.
        if unsafe { atexit(shutdown_at_exit) } == 0 {
            Ok(())
        } else {
            shutdown_pool(pool);
            Err("Parallel: failed to register atexit shutdown callback".to_owned())
        }
    });
    shutdown.as_ref().map(|_| pool).map_err(|error| error.clone())
}

extern "C" fn shutdown_at_exit() {
    // Runs during process teardown, after the main thread's thread-locals may be gone: it must not
    // touch thread-locals or `thread::current()`, and must not force the pool to initialize.
    // Setting the stop flag is enough; the idle workers exit on their own and the OS reaps them.
    if let Some(Ok(pool)) = LazyLock::get(&POOL) {
        stop_workers(&pool.shared);
    }
}

impl Pool {
    fn new() -> Result<Self, String> {
        let shared = Arc::new(Shared {
            state: Mutex::new(State {
                jobs: Vec::new(),
                stopping: false,
            }),
            wake: Condvar::new(),
        });
        let requested = usize::try_from(recommended_domains().saturating_sub(1))
            .unwrap_or(usize::MAX);
        let mut workers = Vec::new();
        for index in 0..requested {
            let worker_shared = Arc::clone(&shared);
            match thread::Builder::new()
                .name(format!("parallel-{index}"))
                .spawn(move || worker(worker_shared))
            {
                Ok(handle) => workers.push(handle),
                Err(error) => {
                    stop_workers(&shared);
                    join_workers(workers);
                    return Err(format!("Parallel: failed to start worker: {error}"));
                }
            }
        }
        Ok(Self {
            shared,
            worker_count: workers.len(),
            workers: Mutex::new(workers),
        })
    }
}

fn lock_state(mutex: &Mutex<State>) -> MutexGuard<'_, State> {
    mutex.lock().unwrap_or_else(std::sync::PoisonError::into_inner)
}
fn lock_workers(
    mutex: &Mutex<Vec<thread::JoinHandle<()>>>,
) -> MutexGuard<'_, Vec<thread::JoinHandle<()>>> {
    mutex.lock().unwrap_or_else(std::sync::PoisonError::into_inner)
}

fn stop_workers(shared: &Shared) {
    lock_state(&shared.state).stopping = true;
    shared.wake.notify_all();
}

fn join_workers(workers: Vec<thread::JoinHandle<()>>) {
    let current = thread::current().id();
    for worker in workers {
        if worker.thread().id() == current {
            // An atexit callback cannot join its own worker; process teardown terminates it.
            continue;
        }
        let _ = worker.join();
    }
}

fn shutdown_pool(pool: &Pool) {
    stop_workers(&pool.shared);
    let workers = {
        let mut handles = lock_workers(&pool.workers);
        mem::take(&mut *handles)
    };
    join_workers(workers);
}

fn lock_failure(mutex: &Mutex<Option<Failure>>) -> MutexGuard<'_, Option<Failure>> {
    mutex.lock().unwrap_or_else(std::sync::PoisonError::into_inner)
}

fn lock_completion(mutex: &Mutex<()>) -> MutexGuard<'_, ()> {
    mutex.lock().unwrap_or_else(std::sync::PoisonError::into_inner)
}

fn claimable(state: &State, wake: &Condvar) -> Option<JobPtr> {
    for job in state.jobs.iter().rev().copied() {
        let header = job.header();
        if !header.cancelled.load(Ordering::Acquire)
            && header.next.load(Ordering::Relaxed) < header.chunks
            && header.helpers.load(Ordering::Relaxed) < header.width - 1
        {
            let helpers = header.helpers.fetch_add(1, Ordering::Relaxed) + 1;
            header.active.fetch_add(1, Ordering::AcqRel);
            if header.next.load(Ordering::Relaxed) < header.chunks
                && helpers < header.width - 1
            {
                wake.notify_one();
            }
            return Some(job);
        }
    }
    None
}

fn worker(shared: Arc<Shared>) {
    loop {
        let job = {
            let mut state = lock_state(&shared.state);
            loop {
                if let Some(job) = claimable(&state, &shared.wake) {
                    break job;
                }
                if state.stopping {
                    return;
                }
                state = shared
                    .wake
                    .wait(state)
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
            }
        };
        let _active = ActiveGuard::already_counted(job.header());
        // ponytail: isolate panics here to preserve the pool; transport payloads only if diagnostics need them.
        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| help(job)));
    }
}

struct ActiveGuard {
    header: *const JobHeader,
}

impl ActiveGuard {
    fn caller(header: &JobHeader) -> Self {
        header.active.fetch_add(1, Ordering::AcqRel);
        Self { header }
    }

    fn already_counted(header: &JobHeader) -> Self {
        Self { header }
    }
}

impl Drop for ActiveGuard {
    fn drop(&mut self) {
        // SAFETY: the job registration outlives every active guard.
        let header = unsafe { &*self.header };
        // Keep the count transition and notification under the same lock used by waiters. Once a
        // waiter observes zero, this guard has finished accessing the header.
        let _guard = lock_completion(&header.completion_lock);
        header.active.fetch_sub(1, Ordering::AcqRel);
        header.completion.notify_all();
    }
}

struct Registration {
    pool: &'static Pool,
    job: JobPtr,
    armed: bool,
}

impl Registration {
    fn new(pool: &'static Pool, job: JobPtr) -> Option<Self> {
        let mut state = lock_state(&pool.shared.state);
        if state.stopping {
            return None;
        }
        state.jobs.push(job);
        pool.shared.wake.notify_one();
        Some(Self {
            pool,
            job,
            armed: true,
        })
    }

    fn wait_for_helpers(&self) {
        let header = self.job.header();
        let mut guard = lock_completion(&header.completion_lock);
        while header.active.load(Ordering::Acquire) != 0 {
            guard = header
                .completion
                .wait(guard)
                .unwrap_or_else(std::sync::PoisonError::into_inner);
        }
    }

    fn finish(&mut self) -> bool {
        let mut state = lock_state(&self.pool.shared.state);
        state.jobs.retain(|other| !other.same(self.job));
        drop(state);
        self.pool.shared.wake.notify_all();
        self.wait_for_helpers();
        self.armed = false;
        self.job.header().finished.load(Ordering::Acquire) == self.job.header().chunks
    }
}

impl Drop for Registration {
    fn drop(&mut self) {
        if !self.armed {
            return;
        }
        {
            let mut state = lock_state(&self.pool.shared.state);
            self.job.header().cancelled.store(true, Ordering::Release);
            state.jobs.retain(|other| !other.same(self.job));
        }
        self.pool.shared.wake.notify_all();
        self.wait_for_helpers();
    }
}

struct ChunkGuard {
    job: JobPtr,
    chunk: usize,
    complete: bool,
}

impl ChunkGuard {
    fn new(job: JobPtr, chunk: usize) -> Self {
        Self {
            job,
            chunk,
            complete: false,
        }
    }

    fn finish(mut self) {
        self.job
            .header()
            .finished
            .fetch_add(1, Ordering::Release);
        self.complete = true;
    }
}

impl Drop for ChunkGuard {
    fn drop(&mut self) {
        if self.complete {
            return;
        }
        let header = self.job.header();
        record_failure(header, self.chunk, FailureKind::WorkerPanicked);
        header.finished.fetch_add(1, Ordering::Release);
    }
}

fn help(job: JobPtr) {
    let header = job.header();
    loop {
        if header.cancelled.load(Ordering::Acquire) {
            break;
        }
        let chunk = header.next.fetch_add(1, Ordering::Relaxed);
        if chunk >= header.chunks {
            break;
        }
        let chunk_guard = ChunkGuard::new(job, chunk);
        let skip = lock_failure(&header.failed)
            .as_ref()
            .is_some_and(|failure| failure.chunk < chunk);
        if !skip {
            if let Err(error) = with_context(Context::Pooled(header.width), &|| {
                job.run_chunk(chunk)
            }) {
                record_failure(header, chunk, FailureKind::Callback(error));
            }
        }
        chunk_guard.finish();
    }
}

fn record_failure(header: &JobHeader, chunk: usize, kind: FailureKind) {
    let mut failure = lock_failure(&header.failed);
    if failure
        .as_ref()
        .is_some_and(|previous| previous.chunk <= chunk)
    {
        return;
    }
    *failure = Some(Failure { chunk, kind });
}

fn take_failure(header: &JobHeader) -> Option<String> {
    lock_failure(&header.failed).take().map(|failure| match failure.kind {
        FailureKind::Callback(error) => error,
        FailureKind::WorkerPanicked => {
            format!("Parallel: worker panicked while running chunk {}", failure.chunk)
        }
    })
}

fn execute(
    width: usize,
    chunks: usize,
    callback: &(dyn Fn(usize) -> Result<(), String> + Send + Sync),
) -> Result<(), String> {
    if chunks == 0 {
        return Ok(());
    }
    if chunks == 1 {
        return callback(0);
    }

    let pool = pool()?;
    let width = width.min(pool.worker_count + 1);
    if width <= 1 {
        for chunk in 0..chunks {
            with_context(Context::Pooled(1), &|| callback(chunk))?;
        }
        return Ok(());
    }

    let job = Job::new(chunks, width, callback);
    let pointer = JobPtr(&job);
    let Some(mut registration) = Registration::new(pool, pointer) else {
        for chunk in 0..chunks {
            with_context(Context::Pooled(1), &|| callback(chunk))?;
        }
        return Ok(());
    };
    {
        let _caller = ActiveGuard::caller(&job.header);
        help(pointer);
    }
    let completed = registration.finish();
    if let Some(error) = take_failure(&job.header) {
        return Err(error);
    }
    if !completed {
        return Err("Parallel: a chunk ended without completing".to_owned());
    }
    Ok(())
}

fn chunking(width: usize, minimum: usize, length: usize) -> (usize, usize) {
    let target = width.saturating_mul(16).max(1);
    let size = minimum.max(length / target + usize::from(length % target != 0));
    let chunks = length / size + usize::from(length % size != 0);
    (size, chunks)
}

fn parallel_width(minimum: usize, length: usize) -> Result<Option<usize>, String> {
    if length < minimum || matches!(context(), Context::Sequential) {
        return Ok(None);
    }
    let available = pool()?.worker_count + 1;
    let requested = match context() {
        Context::Outside => available,
        Context::Sequential => return Ok(None),
        Context::Pooled(width) => width,
    };
    let width = requested.min(available);
    Ok((width > 1).then_some(width))
}

pub fn run_chunks(
    chunk_size: i64,
    start: i64,
    finish: i64,
    work: &(dyn Fn(i64, i64) -> Result<(), String> + Send + Sync),
) -> Result<(), String> {
    if chunk_size <= 0 {
        return Err("Parallel.run_chunks: chunk size must be positive".to_owned());
    }
    if finish < start {
        return Ok(());
    }
    let length = (finish as i128 - start as i128 + 1) as u128;
    if length > usize::MAX as u128 {
        return work(start, finish);
    }
    let length = length as usize;
    let minimum = usize::try_from(chunk_size).unwrap_or(usize::MAX);
    let Some(width) = parallel_width(minimum, length)? else {
        return work(start, finish);
    };
    let (size, chunks) = chunking(width, minimum, length);
    if chunks <= 1 {
        return work(start, finish);
    }
    execute(width, chunks, &|chunk| {
        let first = start as i128 + chunk as i128 * size as i128;
        let last = (finish as i128).min(first + size as i128 - 1);
        work(first as i64, last as i64)
    })
}

pub fn for_(
    chunk_size: i64,
    start: i64,
    finish: i64,
    work: &(dyn Fn(i64) -> Result<(), String> + Send + Sync),
) -> Result<(), String> {
    run_chunks(chunk_size, start, finish, &|first, last| {
        let mut index = first;
        loop {
            work(index)?;
            if index == last {
                break;
            }
            index += 1;
        }
        Ok(())
    })
}

pub fn both_unit(
    left: &(dyn Fn() -> Result<(), String> + Send + Sync),
    right: &(dyn Fn() -> Result<(), String> + Send + Sync),
) -> Result<(), String> {
    let Some(width) = parallel_width(1, 2)? else {
        left()?;
        return right();
    };
    execute(width, 2, &|chunk| {
        if chunk == 0 {
            left()
        } else {
            right()
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicUsize;
    use std::sync::mpsc;
    use std::time::Duration;

    fn collect(values: &[AtomicUsize]) -> Vec<usize> {
        values
            .iter()
            .map(|value| value.load(Ordering::Relaxed))
            .collect()
    }

    fn fill(values: &[AtomicUsize], domains: Option<i64>, seed: usize) -> Result<(), String> {
        if values.is_empty() {
            return Ok(());
        }
        let chunks = |first: i64, last: i64| {
            for index in first..=last {
                let index = index as usize;
                values[index].store((index ^ seed).wrapping_mul(7919), Ordering::Relaxed);
            }
            Ok(())
        };
        run_unit(domains, &|| run_chunks(3, 0, values.len() as i64 - 1, &chunks))
    }

    #[test]
    fn one_and_many_domains_produce_identical_ordered_results() {
        for length in [0_usize, 1, 2, 7, 64, 1000, 12_345] {
            let expected = (0..length)
                .map(|index| (index ^ 0x5a5a).wrapping_mul(7919))
                .collect::<Vec<_>>();
            let one = (0..length).map(|_| AtomicUsize::new(0)).collect::<Vec<_>>();
            fill(&one, Some(1), 0x5a5a).unwrap();
            assert_eq!(collect(&one), expected);

            let many = (0..length).map(|_| AtomicUsize::new(0)).collect::<Vec<_>>();
            fill(&many, None, 0x5a5a).unwrap();
            assert_eq!(collect(&many), expected);
        }
    }

    fn nested_fill(values: &[AtomicUsize], domains: Option<i64>) -> Result<(), String> {
        let outer = |first: i64, last: i64| {
            for index in first..=last {
                let inner = |nested_first: i64, nested_last: i64| {
                    for nested in nested_first..=nested_last {
                        values[index as usize * 32 + nested as usize]
                            .store((index as usize + nested as usize) * 17, Ordering::Relaxed);
                    }
                    Ok(())
                };
                run_chunks(2, 0, 31, &inner)?;
            }
            Ok(())
        };
        run_unit(domains, &|| run_chunks(1, 0, 15, &outer))
    }

    #[test]
    fn nested_jobs_match_sequential_execution() {
        let length = 16 * 32;
        let sequential = (0..length).map(|_| AtomicUsize::new(0)).collect::<Vec<_>>();
        nested_fill(&sequential, Some(1)).unwrap();
        let expected = collect(&sequential);

        let parallel = (0..length).map(|_| AtomicUsize::new(0)).collect::<Vec<_>>();
        nested_fill(&parallel, None).unwrap();
        assert_eq!(collect(&parallel), expected);
    }

    #[test]
    fn lowest_failure_reducer_handles_reverse_arrival_order() {
        let header = JobHeader::new(512, 2);
        record_failure(&header, 333, FailureKind::Callback("333".to_owned()));
        record_failure(&header, 35, FailureKind::Callback("35".to_owned()));
        assert_eq!(take_failure(&header), Some("35".to_owned()));
    }

    #[test]
    fn scheduler_propagates_the_lowest_failing_index() {
        let result = run_unit(None, &|| {
            run_chunks(1, 0, 511, &|first, last| {
                for index in first..=last {
                    if index == 35 {
                        return Err("35".to_owned());
                    }
                    if index == 333 {
                        return Err("333".to_owned());
                    }
                }
                Ok(())
            })
        });
        assert_eq!(result, Err("35".to_owned()));
    }

    #[test]
    fn inclusive_ranges_run_each_index_once() {
        let visits = (0..33).map(|_| AtomicUsize::new(0)).collect::<Vec<_>>();
        for_(2, -16, 16, &|index| {
            visits[(index + 16) as usize].fetch_add(1, Ordering::Relaxed);
            Ok(())
        })
        .unwrap();
        assert!(visits.iter().all(|count| count.load(Ordering::Relaxed) == 1));
        for_(64, -16, 16, &|index| {
            assert!((-16..=16).contains(&index));
            Ok(())
        })
        .unwrap();
        for_(64, 4, 3, &|_| Err("empty inclusive range ran".to_owned())).unwrap();
        assert_eq!(
            for_(1, 0, 15, &|index| {
                if index == 9 {
                    Err("nine".to_owned())
                } else {
                    Ok(())
                }
            }),
            Err("nine".to_owned())
        );
    }

    #[test]
    fn chunk_ranges_are_contiguous_and_at_least_the_minimum() {
        let visits = (0..103).map(|_| AtomicUsize::new(0)).collect::<Vec<_>>();
        let chunks = AtomicUsize::new(0);
        run_chunks(5, 0, 102, &|first, last| {
            assert!(last >= first);
            assert!(last - first + 1 >= 5 || last == 102);
            chunks.fetch_add(1, Ordering::Relaxed);
            for index in first..=last {
                visits[index as usize].fetch_add(1, Ordering::Relaxed);
            }
            Ok(())
        })
        .unwrap();
        assert!(chunks.load(Ordering::Relaxed) > 0);
        assert!(visits.iter().all(|count| count.load(Ordering::Relaxed) == 1));
    }

    #[test]
    fn both_unit_runs_both_callbacks() {
        let left = AtomicUsize::new(0);
        let right = AtomicUsize::new(0);
        both_unit(
            &|| {
                left.store(11, Ordering::Relaxed);
                Ok(())
            },
            &|| {
                right.store(29, Ordering::Relaxed);
                Ok(())
            },
        )
        .unwrap();
        assert_eq!(left.load(Ordering::Relaxed), 11);
        assert_eq!(right.load(Ordering::Relaxed), 29);
        assert_eq!(
            both_unit(
                &|| Err("left".to_owned()),
                &|| Err("right".to_owned())
            ),
            Err("left".to_owned())
        );
    }

    #[test]
    fn simultaneous_owners_share_the_worker_set() {
        let expected_a = (0usize..4096)
            .map(|index| (index ^ 17).wrapping_mul(7919))
            .collect::<Vec<_>>();
        let expected_b = (0usize..4096)
            .map(|index| (index ^ 29).wrapping_mul(7919))
            .collect::<Vec<_>>();
        thread::scope(|scope| {
            let owner_a = scope.spawn(|| {
                let values = (0..4096).map(|_| AtomicUsize::new(0)).collect::<Vec<_>>();
                for _ in 0..3 {
                    fill(&values, None, 17).unwrap();
                    assert_eq!(collect(&values), expected_a);
                }
            });
            let owner_b = scope.spawn(|| {
                let values = (0..4096).map(|_| AtomicUsize::new(0)).collect::<Vec<_>>();
                for _ in 0..3 {
                    fill(&values, None, 29).unwrap();
                    assert_eq!(collect(&values), expected_b);
                }
            });
            owner_a.join().unwrap();
            owner_b.join().unwrap();
        });
    }

    #[test]
    fn nested_run_keeps_sequential_context() {
        let values = (0..32).map(|_| AtomicUsize::new(0)).collect::<Vec<_>>();
        let chunks = |first: i64, last: i64| {
            for index in first..=last {
                values[index as usize].store(index as usize, Ordering::Relaxed);
            }
            Ok(())
        };
        run_unit(Some(1), &|| run_unit(None, &|| run_chunks(1, 0, 31, &chunks))).unwrap();
        assert_eq!(collect(&values), (0..32).collect::<Vec<_>>());
    }

    #[test]
    fn shutdown_stops_an_idle_worker_without_waiting() {
        let shared = Arc::new(Shared {
            state: Mutex::new(State {
                jobs: Vec::new(),
                stopping: false,
            }),
            wake: Condvar::new(),
        });
        let pool = Pool {
            shared: Arc::clone(&shared),
            worker_count: 0,
            workers: Mutex::new(Vec::new()),
        };
        shutdown_pool(&pool);
        assert!(lock_state(&shared.state).stopping);
        worker(shared);
    }

    #[test]
    fn worker_survives_a_panicking_callback() {
        let shared = Arc::new(Shared {
            state: Mutex::new(State {
                jobs: Vec::new(),
                stopping: false,
            }),
            wake: Condvar::new(),
        });
        let worker_shared = Arc::clone(&shared);
        let handle = thread::spawn(move || worker(worker_shared));

        let (panicked_tx, panicked_rx) = mpsc::channel();
        let panicking = move |_| -> Result<(), String> {
            panicked_tx.send(()).unwrap();
            panic!("intentional callback panic")
        };
        let first = Job::new(1, 2, &panicking);
        let first_ptr = JobPtr(&first);
        lock_state(&shared.state).jobs.push(first_ptr);
        shared.wake.notify_one();
        let panic_started = panicked_rx.recv_timeout(Duration::from_secs(5)).is_ok();

        let (continued_tx, continued_rx) = mpsc::channel();
        let following = move |_| {
            continued_tx.send(()).unwrap();
            Ok(())
        };
        let second = Job::new(1, 2, &following);
        let second_ptr = JobPtr(&second);
        lock_state(&shared.state).jobs.push(second_ptr);
        shared.wake.notify_one();
        let continued = continued_rx.recv_timeout(Duration::from_secs(5)).is_ok();

        stop_workers(&shared);
        handle.join().unwrap();
        lock_state(&shared.state)
            .jobs
            .retain(|job| !job.same(first_ptr) && !job.same(second_ptr));

        assert!(panic_started);
        assert!(continued, "worker exited instead of serving the next job");
        assert_eq!(
            take_failure(&first.header),
            Some("Parallel: worker panicked while running chunk 0".to_owned())
        );
    }
}
