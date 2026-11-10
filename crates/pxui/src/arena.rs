//! The frame arena: canvas closures and temporary strings live here for one frame. It bumps through
//! 16-byte-aligned storage that `reset` empties and nothing ever shrinks, so a warm UI allocates
//! nothing, not even for the first frame of a new canvas.
//!
//! Unsafe surface: a closure is moved into the bytes with `ptr::write` and is afterwards reached
//! only through its record's `call` (a shared `&F`) and `drop` (`drop_in_place`, once, in `reset`).
//! Growing the storage moves the bytes, which is a plain Rust move of every closure in it; no
//! pointer into the storage outlives one call.
use crate::geom::Rect;
use crate::ui_core::Paint;
use std::fmt;

#[derive(Clone, Copy)]
#[repr(C, align(16))]
struct Block([u8; 16]);
const ALIGN: usize = 16;

/// No canvas: the end of a box's list.
pub(crate) const END: u32 = u32::MAX;

struct Canvas {
    at: usize,
    call: unsafe fn(*const u8, &mut Paint, Rect),
    drop: Option<unsafe fn(*mut u8)>,
    next: u32,
}

/// A box's canvases in call order: the first and the last record.
pub(crate) type List = (u32, u32);
pub(crate) const EMPTY: List = (END, END);

#[derive(Default)]
pub(crate) struct Arena {
    blocks: Vec<Block>,
    len: usize,
    canvases: Vec<Canvas>,
}

unsafe fn call<F: Fn(&mut Paint, Rect)>(at: *const u8, paint: &mut Paint, rect: Rect) {
    unsafe { (*at.cast::<F>())(paint, rect) }
}

unsafe fn drop<F>(at: *mut u8) {
    unsafe { std::ptr::drop_in_place(at.cast::<F>()) }
}

impl Arena {
    /// `size` bytes at a multiple of `align`, as a byte offset.
    fn bump(&mut self, align: usize, size: usize) -> usize {
        // ponytail: alignment above 16 panics; over-aligned captures would need a padded base.
        assert!(align <= ALIGN, "pxui arena: a canvas capture aligned to {align} bytes");
        let at = self.len.next_multiple_of(align);
        let end = at + size;
        let blocks = end.div_ceil(ALIGN);
        if blocks > self.blocks.len() {
            self.blocks.resize(blocks.max(2 * self.blocks.len()).max(64), Block([0; ALIGN]));
        }
        self.len = end;
        at
    }

    fn base(&mut self) -> *mut u8 {
        self.blocks.as_mut_ptr().cast()
    }

    /// Append `f` to `list`; it runs once in `run` and is dropped in `reset`.
    pub(crate) fn add<F: Fn(&mut Paint, Rect) + 'static>(&mut self, list: &mut List, f: F) {
        let at = self.bump(align_of::<F>(), size_of::<F>());
        let index = self.canvases.len() as u32;
        self.canvases.push(Canvas { at, call: call::<F>, drop: std::mem::needs_drop::<F>().then_some(drop::<F>), next: END });
        // SAFETY: `at..at + size_of::<F>()` lies in the storage, is aligned for `F` (the base is
        // 16-aligned, `align_of::<F>() <= 16`) and no live value overlaps it.
        unsafe { self.base().add(at).cast::<F>().write(f) };
        if list.1 == END {
            list.0 = index;
        } else {
            self.canvases[list.1 as usize].next = index;
        }
        list.1 = index;
    }

    /// Call the canvases of a list from `first`.
    pub(crate) fn run(&self, first: u32, paint: &mut Paint, rect: Rect) {
        let mut index = first;
        while index != END {
            let canvas = &self.canvases[index as usize];
            // SAFETY: the record was written by `add` with this `call` and is not yet dropped.
            unsafe { (canvas.call)(self.blocks.as_ptr().cast::<u8>().add(canvas.at), paint, rect) };
            index = canvas.next;
        }
    }

    /// `args` written into the arena: the byte range of the text, valid until `reset`.
    pub(crate) fn fmt(&mut self, args: fmt::Arguments) -> (usize, usize) {
        struct Writer<'a>(&'a mut Arena);
        impl fmt::Write for Writer<'_> {
            fn write_str(&mut self, s: &str) -> fmt::Result {
                let at = self.0.bump(1, s.len());
                // SAFETY: `bump` reserved `s.len()` bytes at `at`.
                unsafe { std::ptr::copy_nonoverlapping(s.as_ptr(), self.0.base().add(at), s.len()) };
                Ok(())
            }
        }
        let start = self.len;
        let _ = fmt::Write::write_fmt(&mut Writer(self), args);
        (start, self.len)
    }

    pub(crate) fn str(&self, (start, end): (usize, usize)) -> &str {
        // SAFETY: `fmt` wrote whole `&str`s back to back (align 1, no padding) into these bytes.
        unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(self.blocks.as_ptr().cast::<u8>().add(start), end - start)) }
    }

    /// Drop every canvas exactly once and empty the arena; its storage stays.
    pub(crate) fn reset(&mut self) {
        while let Some(canvas) = self.canvases.pop() {
            if let Some(drop) = canvas.drop {
                // SAFETY: popped first, so a panicking drop never runs twice.
                unsafe { drop(self.base().add(canvas.at)) };
            }
        }
        self.len = 0;
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        self.reset();
    }
}
