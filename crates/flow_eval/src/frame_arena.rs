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
    /// Most recently used first. Unshared allocations can also hold new same-length text.
    sticky: [Option<Arc<[u8]>>; STICKY],
    #[cfg(debug_assertions)]
    poisoned: bool,
}

thread_local! {
    static ARENA: RefCell<Arena> = const { RefCell::new(Arena {
        scratch: Vec::new(),
        sticky: [const { None }; STICKY],
        #[cfg(debug_assertions)]
        poisoned: false,
    }) };
}

/// Reset the frame arena. Call from `begin_frame` once the plane advances.
pub fn reset() {
    ARENA.with(|a| {
        let mut a = a.borrow_mut();
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

fn intern(sticky: &mut [Option<Arc<[u8]>>; STICKY], bytes: &[u8]) -> Value {
    let slot = if let Some(i) = sticky.iter().position(|arc| arc.as_deref() == Some(bytes)) {
        i
    } else if let Some(i) = sticky.iter_mut().rposition(|arc| {
        arc.as_mut().and_then(Arc::get_mut).filter(|text| text.len() == bytes.len()).is_some()
    }) {
        Arc::get_mut(sticky[i].as_mut().unwrap()).unwrap().copy_from_slice(bytes);
        i
    } else {
        sticky[STICKY - 1] = Some(Arc::from(bytes));
        STICKY - 1
    };
    sticky[..=slot].rotate_right(1);
    Value::Text(sticky[0].as_ref().unwrap().clone())
}

fn intern_scratch(a: &mut Arena) -> Value {
    intern(&mut a.sticky, &a.scratch)
}

/// Intern `bytes` as `Value::Text` for this frame.
pub fn text(bytes: &[u8]) -> Value {
    ARENA.with(|a| {
        let mut a = a.borrow_mut();
        #[cfg(debug_assertions)]
        check_live(&a);
        intern(&mut a.sticky, bytes)
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
    fn ephemeral_same_length_frame_text_allocates_nothing() {
        begin();
        let a = build_text::<()>(|out| {
            out.extend_from_slice(b"1280 x 900  frame 10");
            Ok(())
        })
        .unwrap();
        let len = match &a {
            Value::Text(t) => t.len(),
            _ => panic!(),
        };
        reset();
        begin();
        // Drop the only external ref so ephemeral is unique for rewrite.
        drop(a);
        mem_sys::local_reset();
        let start = mem_sys::local_alloc_count();
        let b = build_text::<()>(|out| {
            out.extend_from_slice(b"1280 x 900  frame 11");
            Ok(())
        })
        .unwrap();
        let allocs = mem_sys::local_alloc_count() - start;
        let Value::Text(second) = &b else { panic!() };
        assert_eq!(allocs, 0, "same-length frame text rewrites ephemeral Arc");
        assert_eq!(second.as_ref(), b"1280 x 900  frame 11");
        assert_eq!(len, second.len());
    }

    #[test]
    fn changing_text_reuses_storage_while_a_prior_frame_is_held() {
        begin();
        let first = text(b"frame 10");
        drop(text(b"frame 11"));
        mem_sys::local_reset();
        let third = text(b"frame 12");
        assert_eq!(mem_sys::local_alloc_count(), 0);
        assert_eq!(first, Value::Text(Arc::from(&b"frame 10"[..])));
        assert_eq!(third, Value::Text(Arc::from(&b"frame 12"[..])));
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
