//! Per-frame arena for sketch-ephemeral values (strings, small boxed payloads).
//!
//! Reset at `begin_frame`. Values that escape into state (or any cross-frame store)
//! must go through [`persist`], which copies them onto the heap. In debug builds, a
//! generation counter poisons stale handles so a dangling use fails loudly.
//!
//! Design: a bump `Vec<u8>` holds frame-local bytes; a sticky text slot reuses one
//! `Arc<[u8]>` while its content is unchanged (frozen clock). Capacity is kept across
//! resets so a warm frame allocates nothing from the Rust allocator for those paths.

use flow_syntax::value::Value;
use std::cell::RefCell;
use std::sync::Arc;

#[derive(Default)]
struct Arena {
    bump: Vec<u8>,
    /// Generation incremented on every reset; stale handles disagree and panic in debug.
    generation: u32,
    /// Last text Arc produced this generation (or sticky across gens when bytes match).
    text: Option<(u32, Arc<[u8]>)>,
    /// Recycled entry/list scratch lengths (capacity kept on the bump via clear).
    #[cfg(debug_assertions)]
    poisoned: bool,
}

thread_local! {
    static ARENA: RefCell<Arena> = const { RefCell::new(Arena {
        bump: Vec::new(),
        generation: 1,
        text: None,
        #[cfg(debug_assertions)]
        poisoned: false,
    }) };
}

/// Reset the frame arena. Call from `begin_frame` once the plane advances (or matches).
pub fn reset() {
    ARENA.with(|a| {
        let mut a = a.borrow_mut();
        a.generation = a.generation.wrapping_add(1).max(1);
        a.bump.clear();
        // Keep sticky text Arc when present: frozen frames rewrite the same bytes and
        // reuse it without touching the allocator. Drop only on generation wrap.
        #[cfg(debug_assertions)]
        {
            a.poisoned = true;
        }
    });
}

/// Mark the arena live for this frame (clears the debug poison latch).
pub fn begin() {
    ARENA.with(|a| {
        let mut a = a.borrow_mut();
        #[cfg(debug_assertions)]
        {
            a.poisoned = false;
        }
        let _ = &mut a;
    });
}

#[cfg(debug_assertions)]
fn check_live(a: &Arena) {
    assert!(!a.poisoned, "frame_arena: use after reset (dangling frame value)");
}

/// Intern `bytes` as `Value::Text` for this frame. Reuses the sticky Arc when content matches.
pub fn text(bytes: &[u8]) -> Value {
    ARENA.with(|a| {
        let mut a = a.borrow_mut();
        #[cfg(debug_assertions)]
        check_live(&a);
        if let Some((generation, arc)) = &a.text {
            if *generation == a.generation && arc.as_ref() == bytes {
                return Value::Text(arc.clone());
            }
            // Sticky reuse across reset when the sketch reprints the same string (frozen clock).
            if arc.as_ref() == bytes {
                let arc = arc.clone();
                a.text = Some((a.generation, arc.clone()));
                return Value::Text(arc);
            }
        }
        let arc: Arc<[u8]> = Arc::from(bytes);
        a.text = Some((a.generation, arc.clone()));
        Value::Text(arc)
    })
}

/// Copy a frame value onto the heap so it may outlive the next [`reset`].
/// Today `Value::Text` is already an `Arc`; this is the store boundary the arena documents.
pub fn persist(value: Value) -> Value {
    match value {
        Value::Text(bytes) => Value::Text(Arc::from(bytes.as_ref())),
        Value::List(values) => Value::List(Arc::new(values.iter().cloned().map(persist).collect())),
        other => other,
    }
}

/// Append bytes into the bump (kept capacity) and return a copy as `Arc<[u8]>`.
/// Prefer [`text`] for Values; this is for mark tables that want owned bytes once.
pub fn bytes(data: &[u8]) -> Arc<[u8]> {
    ARENA.with(|a| {
        let mut a = a.borrow_mut();
        #[cfg(debug_assertions)]
        check_live(&a);
        let start = a.bump.len();
        a.bump.extend_from_slice(data);
        let slice = &a.bump[start..];
        Arc::from(slice)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sticky_text_reuses_arc_across_reset_when_bytes_match() {
        begin();
        let a = text(b"W x H frame 0");
        let Value::Text(first) = &a else { panic!() };
        reset();
        begin();
        mem_sys::local_reset();
        let start = mem_sys::local_alloc_count();
        let b = text(b"W x H frame 0");
        let allocs = mem_sys::local_alloc_count() - start;
        let Value::Text(second) = &b else { panic!() };
        assert!(Arc::ptr_eq(first, second), "sticky text shares the Arc");
        assert_eq!(allocs, 0, "matching text after reset allocates nothing");
    }

    #[test]
    #[cfg(debug_assertions)]
    fn use_after_reset_poisons() {
        begin();
        let _ = text(b"hi");
        reset();
        let crashed = std::panic::catch_unwind(|| text(b"hi"));
        assert!(crashed.is_err(), "debug poison must catch use after reset before begin");
        begin();
        let _ = text(b"ok");
    }
}
