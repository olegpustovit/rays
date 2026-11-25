//! Per-frame arena for sketch-ephemeral values (strings, small boxed payloads).
//!
//! Reset at `begin_frame`. Values that escape into state (or any cross-frame store)
//! must go through [`persist`]. In debug builds, a poison latch on reset makes a
//! dangling use fail loudly.
//!
//! Design: a reusable `scratch` `Vec<u8>` builds frame-local text; a small sticky
//! table of `Arc<[u8]>` reuses prior strings when the sketch reprints them (frozen
//! clock). Capacity is kept across resets so a warm frozen frame allocates nothing
//! from the Rust allocator for those paths.

use flow_syntax::value::Value;
use std::cell::RefCell;
use std::io::Write as _;
use std::sync::Arc;

const STICKY: usize = 8;

struct Arena {
    scratch: Vec<u8>,
    /// Incremented on every reset; sticky entries retarget to the new value on reuse.
    generation: u32,
    /// Recent text Arcs (sticky across gens when bytes match).
    sticky: [Option<(u32, Arc<[u8]>)>; STICKY],
    sticky_next: usize,
    #[cfg(debug_assertions)]
    poisoned: bool,
}

thread_local! {
    static ARENA: RefCell<Arena> = const { RefCell::new(Arena {
        scratch: Vec::new(),
        generation: 1,
        sticky: [None, None, None, None, None, None, None, None],
        sticky_next: 0,
        #[cfg(debug_assertions)]
        poisoned: false,
    }) };
}

/// Reset the frame arena. Call from `begin_frame` once the plane advances.
pub fn reset() {
    ARENA.with(|a| {
        let mut a = a.borrow_mut();
        a.generation = a.generation.wrapping_add(1).max(1);
        a.scratch.clear();
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

fn sticky_get(a: &Arena, bytes: &[u8]) -> Option<Arc<[u8]>> {
    a.sticky.iter().flatten().find_map(|(_, arc)| {
        if arc.as_ref() == bytes {
            Some(arc.clone())
        } else {
            None
        }
    })
}

fn sticky_put(a: &mut Arena, arc: Arc<[u8]>) {
    let i = a.sticky_next % STICKY;
    a.sticky[i] = Some((a.generation, arc));
    a.sticky_next = i + 1;
}

fn touch_sticky(a: &mut Arena, arc: &Arc<[u8]>) {
    let g = a.generation;
    if let Some(slot) = a.sticky.iter_mut().flatten().find(|(_, s)| Arc::ptr_eq(s, arc)) {
        slot.0 = g;
    }
}

fn intern_scratch(a: &mut Arena) -> Value {
    let bytes = a.scratch.as_slice();
    if let Some(arc) = sticky_get(a, bytes) {
        touch_sticky(a, &arc);
        return Value::Text(arc);
    }
    let arc: Arc<[u8]> = Arc::from(bytes);
    sticky_put(a, arc.clone());
    Value::Text(arc)
}

/// Intern `bytes` as `Value::Text` for this frame. Reuses a sticky Arc when content matches.
pub fn text(bytes: &[u8]) -> Value {
    ARENA.with(|a| {
        let mut a = a.borrow_mut();
        #[cfg(debug_assertions)]
        check_live(&a);
        if let Some(arc) = sticky_get(&a, bytes) {
            touch_sticky(&mut a, &arc);
            return Value::Text(arc);
        }
        let arc: Arc<[u8]> = Arc::from(bytes);
        sticky_put(&mut a, arc.clone());
        Value::Text(arc)
    })
}

/// Format a value into the reused scratch buffer and intern it as frame text.
pub fn show(value: &Value) -> Value {
    ARENA.with(|a| {
        let mut a = a.borrow_mut();
        #[cfg(debug_assertions)]
        check_live(&a);
        a.scratch.clear();
        show_into(&mut a.scratch, value);
        intern_scratch(&mut a)
    })
}

/// Build text in the reused scratch (no per-call `Vec`), then intern it.
pub fn build_text<E>(f: impl FnOnce(&mut Vec<u8>) -> Result<(), E>) -> Result<Value, E> {
    ARENA.with(|a| {
        let mut a = a.borrow_mut();
        #[cfg(debug_assertions)]
        check_live(&a);
        a.scratch.clear();
        f(&mut a.scratch)?;
        Ok(intern_scratch(&mut a))
    })
}

fn show_into(out: &mut Vec<u8>, value: &Value) {
    match value {
        Value::Int(n) => {
            let _ = write!(out, "{n}");
        }
        Value::Float(n) => out.extend_from_slice(param::float_text(*n).as_bytes()),
        Value::Bool(true) => out.extend_from_slice(b"true"),
        Value::Bool(false) => out.extend_from_slice(b"false"),
        Value::Text(text) => out.extend_from_slice(text),
        other => out.extend_from_slice(&flow_syntax::value::show_with(&|x| x.clone(), other)),
    }
}

/// Copy a frame value onto the heap so it may outlive the next [`reset`].
/// `Value::Text` is already an `Arc`; sharing it across frames is safe.
pub fn persist(value: Value) -> Value {
    match value {
        Value::List(values) => Value::List(Arc::new(values.iter().cloned().map(persist).collect())),
        other => other,
    }
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
    fn sticky_show_and_build_allocate_nothing_when_stable() {
        begin();
        let _ = show(&Value::Int(1280));
        let _ = show(&Value::Int(900));
        let _ = show(&Value::Int(0));
        let _ = build_text::<()>(|out| {
            out.extend_from_slice(b"1280 x 900  frame 0");
            Ok(())
        })
        .unwrap();
        reset();
        begin();
        mem_sys::local_reset();
        let start = mem_sys::local_alloc_count();
        let _ = show(&Value::Int(1280));
        let _ = show(&Value::Int(900));
        let _ = show(&Value::Int(0));
        let _ = build_text::<()>(|out| {
            out.extend_from_slice(b"1280 x 900  frame 0");
            Ok(())
        })
        .unwrap();
        assert_eq!(mem_sys::local_alloc_count() - start, 0, "frozen show+build is alloc-free");
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
