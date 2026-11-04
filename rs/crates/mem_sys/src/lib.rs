use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicI64, AtomicU64, Ordering};
use std::sync::Mutex;

static LIVE: AtomicU64 = AtomicU64::new(0);
static PEAK: AtomicU64 = AtomicU64::new(0);
static PRESSURE: AtomicI64 = AtomicI64::new(0);
static PRESSURE_LOCK: Mutex<bool> = Mutex::new(false);

pub struct CountingAllocator;

#[global_allocator]
static GLOBAL: CountingAllocator = CountingAllocator;

fn add_live(size: usize) {
    let size = size as u64;
    let now = LIVE.try_update(Ordering::Relaxed, Ordering::Relaxed, |live| {
        Some(live.saturating_add(size))
    }).unwrap().saturating_add(size);
    let mut peak = PEAK.load(Ordering::Relaxed);
    while now > peak {
        match PEAK.compare_exchange_weak(peak, now, Ordering::Relaxed, Ordering::Relaxed) {
            Ok(_) => break,
            Err(seen) => peak = seen,
        }
    }
}

fn subtract_live(size: usize) {
    let size = size as u64;
    let _ = LIVE.try_update(Ordering::Relaxed, Ordering::Relaxed, |live| {
        Some(live.saturating_sub(size))
    });
}

// `System` owns the pointers; this wrapper only updates atomics around those operations.
unsafe impl GlobalAlloc for CountingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc(layout) };
        if !pointer.is_null() {
            add_live(layout.size());
        }
        pointer
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc_zeroed(layout) };
        if !pointer.is_null() {
            add_live(layout.size());
        }
        pointer
    }

    unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {
        unsafe { System.dealloc(pointer, layout) };
        subtract_live(layout.size());
    }

    unsafe fn realloc(&self, pointer: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        let replacement = unsafe { System.realloc(pointer, layout, new_size) };
        if !replacement.is_null() {
            if new_size >= layout.size() {
                add_live(new_size - layout.size());
            } else {
                subtract_live(layout.size() - new_size);
            }
        }
        replacement
    }
}

pub fn allocated_bytes() -> u64 {
    LIVE.load(Ordering::Relaxed)
}

pub fn peak_allocated_bytes() -> u64 {
    PEAK.load(Ordering::Relaxed)
}

unsafe extern "C" {
    fn mem_sys_pressure_start(callback: extern "C" fn(i32)) -> i32;
    fn mem_sys_pressure_poll() -> i32;
    fn mem_sys_phys_footprint(out: *mut i64) -> i32;
    fn mem_sys_peak_footprint(out: *mut i64) -> i32;
    fn mem_sys_instructions(out: *mut i64) -> i32;
    fn mem_sys_physical_ram() -> i64;
    fn mem_sys_pressure_relief();
}

extern "C" fn store_pressure(level: i32) {
    let level = i64::from(level & 3);
    let mut seen = PRESSURE.load(Ordering::Relaxed);
    loop {
        let count = (seen >> 2).wrapping_add(1);
        let next = (count << 2) | level;
        match PRESSURE.compare_exchange_weak(seen, next, Ordering::Relaxed, Ordering::Relaxed) {
            Ok(_) => return,
            Err(now) => seen = now,
        }
    }
}

pub fn pressure_inject(level: i32) {
    store_pressure(level);
}

pub fn pressure_state() -> i64 {
    let _guard = PRESSURE_LOCK.lock().unwrap_or_else(|error| error.into_inner());
    let _ = unsafe { mem_sys_pressure_poll() };
    PRESSURE.load(Ordering::Relaxed)
}

pub fn pressure_start() -> bool {
    let mut started = PRESSURE_LOCK.lock().unwrap_or_else(|error| error.into_inner());
    if !*started {
        *started = unsafe { mem_sys_pressure_start(store_pressure) != 0 };
    }
    *started
}

pub fn pressure_relief() {
    unsafe { mem_sys_pressure_relief() }
}

pub fn phys_footprint() -> Option<i64> {
    let mut value = 0;
    (unsafe { mem_sys_phys_footprint(&mut value) } != 0).then_some(value)
}

pub fn peak_footprint() -> Option<i64> {
    let mut value = 0;
    (unsafe { mem_sys_peak_footprint(&mut value) } != 0).then_some(value)
}

pub fn instructions() -> Option<i64> {
    let mut value = 0;
    (unsafe { mem_sys_instructions(&mut value) } != 0).then_some(value)
}

pub fn physical_ram() -> i64 {
    unsafe { mem_sys_physical_ram() }
}
