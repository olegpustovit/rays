//! History retains Arc<Document> presents and compact snapshots; Arc::ptr_eq identifies unchanged documents.
#![allow(non_camel_case_types)]
use std::collections::VecDeque;
use std::sync::Arc;

use rays_diagnostic::Diagnostic;
use editor_document::document::{Snapshot as E, T as A};

/// How a record merges with the entry it follows.
#[derive(Clone, Debug, PartialEq)]
pub enum Merge {
    Step,
    Gesture(String),
    Burst { key: String, at: f64, window: f64 },
    Repair,
}

/// A live Arc present owns the document; history entries retain only keep() snapshots.
pub struct EditorHistory {
    /// Oldest first; the newest entry is at the back.
    past: VecDeque<(Arc<E>, String, i64)>,
    present: Arc<A>,
    label: String,
    /// The next redo is last.
    future: Vec<(Arc<E>, String, i64)>,
    /// The last present to become an entry, kept live beside the entries.
    recent: Option<(Arc<E>, Arc<A>)>,
    capacity: i64,
    byte_capacity: i64,
    keep: Arc<dyn Fn(&A) -> E>,
    size: Arc<dyn Fn(&E) -> i64>,
    depth: i64,
    byte_depth: i64,
    merge: Option<Merge>,
}

// ponytail: cheap Arc/plain-value clones preserve the prior history unchanged after a failed undo/redo.
impl Clone for EditorHistory {
    fn clone(&self) -> Self {
        EditorHistory {
            past: self.past.clone(),
            present: self.present.clone(),
            label: self.label.clone(),
            future: self.future.clone(),
            recent: self.recent.clone(),
            capacity: self.capacity,
            byte_capacity: self.byte_capacity,
            keep: self.keep.clone(),
            size: self.size.clone(),
            depth: self.depth,
            byte_depth: self.byte_depth,
            merge: self.merge.clone(),
        }
    }
}

/// [capacity] defaults to 128, [byte_capacity] to unbounded, [bytes] to 0.
pub fn create(
    capacity: Option<i64>,
    byte_capacity: Option<i64>,
    bytes: Option<Arc<dyn Fn(&E) -> i64>>,
    keep: Arc<dyn Fn(&A) -> E>,
    present: Arc<A>,
) -> Result<EditorHistory, String> {
    let capacity = capacity.unwrap_or(128);
    let byte_capacity = byte_capacity.unwrap_or(i64::MAX);
    if capacity < 1 {
        return Err("Editor_core.History.create: capacity must be positive".into());
    }
    if byte_capacity < 0 {
        return Err("Editor_core.History.create: byte_capacity must be non-negative".into());
    }
    Ok(EditorHistory {
        past: VecDeque::new(),
        present,
        label: String::new(),
        future: Vec::new(),
        recent: None,
        capacity,
        byte_capacity,
        keep,
        size: bytes.unwrap_or_else(|| Arc::new(|_: &E| 0)),
        depth: 0,
        byte_depth: 0,
        merge: None,
    })
}

pub fn present(h: &EditorHistory) -> &A {
    &h.present
}

pub fn label(h: &EditorHistory) -> &str {
    &h.label
}

/// The label of the next redo, if any.
pub fn redo_label(h: &EditorHistory) -> Option<&str> {
    h.future.last().map(|(_, label, _)| label.as_str())
}

pub fn can_undo(h: &EditorHistory) -> bool {
    !h.past.is_empty()
}

pub fn can_redo(h: &EditorHistory) -> bool {
    !h.future.is_empty()
}

pub fn depth(h: &EditorHistory) -> i64 {
    h.depth
}

/// What the byte cap counts: the entries to undo and to redo.
pub fn byte_depth(h: &EditorHistory) -> i64 {
    h.byte_depth
}

/// The entries(newest undo first, then redo), and the one live value kept beside them.
pub fn retained(h: &EditorHistory) -> (Vec<&E>, Option<&A>) {
    let entries = h
        .past
        .iter()
        .rev()
        .map(|(entry, _, _)| entry.as_ref())
        .chain(h.future.iter().rev().map(|(entry, _, _)| entry.as_ref()))
        .collect();
   (entries, h.recent.as_ref().map(|(_, live)| live.as_ref()))
}

fn entry_of(h: &EditorHistory) -> (Arc<E>, String, i64) {
    let entry = Arc::new((h.keep)(&h.present));
    let n =(h.size)(&entry).max(0);
   (entry, h.label.clone(), n)
}

/// Oldest first, down to [limit] entries and the byte cap. The newest entry stays whatever it
/// weighs: one edit of a document over the cap can still be undone.
fn trim_to(limit: i64, mut h: EditorHistory) -> EditorHistory {
    let excess = h.depth - limit;
    if excess <= 0 &&(h.byte_depth <= h.byte_capacity || h.depth <= 1) {
        return h;
    }
    let mut dropped = 0;
    let mut bytes = h.byte_depth;
    let mut depth = h.depth;
    loop {
        let n = match h.past.front() {
            Some((_, _, n)) => *n,
            None => break,
        };
        if dropped < excess ||(bytes > h.byte_capacity && depth > 1) {
            h.past.pop_front();
            dropped += 1;
            bytes -= n;
            depth -= 1;
        } else {
            break;
        }
    }
    h.byte_depth = bytes;
    h.depth = depth;
    h
}

fn trim(h: EditorHistory) -> EditorHistory {
    let limit = h.capacity;
    trim_to(limit, h)
}

fn commit(value: Arc<A>, label: &str, mut h: EditorHistory) -> EditorHistory {
    if Arc::ptr_eq(&value, &h.present) {
        return h;
    }
    let(entry, old_label, n) = entry_of(&h);
    let abandoned: i64 = h.future.iter().map(|(_, _, n)| *n).sum();
    h.past.push_back((entry.clone(), old_label, n));
    let old_present = std::mem::replace(&mut h.present, value);
    h.label = label.to_string();
    h.future.clear();
    h.recent = Some((entry, old_present));
    h.depth += 1;
    h.byte_depth = h.byte_depth + n - abandoned;
    h.merge = None;
    trim(h)
}

/// The present replaced in place: a continued gesture, or a repair that is no edit of the
/// user's(camera bookkeeping), which leaves what redo would restore.
fn amend(value: Arc<A>, mut h: EditorHistory) -> EditorHistory {
    if Arc::ptr_eq(&value, &h.present) {
        return h;
    }
    h.present = value;
    h
}

/// Step adds an entry; equal Gesture keys merge until seal; equal Burst keys merge within the time window; Repair replaces the current entry. label defaults to Edit.
pub fn record(merge: Merge, label: &str, value: Arc<A>, h: EditorHistory) -> EditorHistory {
    let continuation = match(&merge, &h.merge) {
       (Merge::Repair, _) => true,
       (Merge::Gesture(id), Some(Merge::Gesture(previous))) => id == previous,
       (
            Merge::Burst { key, at, window },
            Some(Merge::Burst { key: previous_key, at: previous_at, .. }),
        ) => key == previous_key && *at >= *previous_at && *at - *previous_at < *window,
        _ => false,
    };
    let repair = matches!(merge, Merge::Repair);
    let mut h = if continuation { amend(value, h) } else { commit(value, label, h) };
    if !repair {
        h.merge = Some(merge);
    }
    h
}

pub fn seal(gesture_only: bool, mut h: EditorHistory) -> EditorHistory {
    match &h.merge {
        None => h,
        Some(Merge::Gesture(_)) => {
            h.merge = None;
            h
        }
        Some(_) if !gesture_only => {
            h.merge = None;
            h
        }
        Some(_) => h,
    }
}

fn revive(
    restore: &dyn Fn(&E) -> Result<A, Diagnostic>,
    h: &EditorHistory,
    entry: &Arc<E>,
) -> Result<Arc<A>, Diagnostic> {
    match &h.recent {
        Some((kept, live)) if Arc::ptr_eq(kept, entry) => Ok(live.clone()),
        _ => restore(entry).map(Arc::new),
    }
}

/// `None` with nothing to undo. `Some(Err)`: nothing changed; `h` stays as it was.
pub fn undo(
    restore: &dyn Fn(&E) -> Result<A, Diagnostic>,
    h: &EditorHistory,
) -> Option<Result<EditorHistory, Diagnostic>> {
    let(previous, _, _) = h.past.back()?;
    let live = match revive(restore, h, previous) {
        Ok(live) => live,
        Err(error) => return Some(Err(error)),
    };
    let mut next = h.clone();
    let(_, label, n) = next.past.pop_back().expect("checked non-empty above");
    let(entry, current_label, m) = entry_of(&next);
    let old_present = std::mem::replace(&mut next.present, live);
    next.future.push((entry.clone(), current_label, m));
    next.label = label;
    next.recent = Some((entry, old_present));
    next.depth -= 1;
    next.byte_depth = next.byte_depth + m - n;
    next.merge = None;
    Some(Ok(next))
}

/// `None` with nothing to redo. `Some(Err)`: nothing changed; `h` stays as it was.
pub fn redo(
    restore: &dyn Fn(&E) -> Result<A, Diagnostic>,
    h: &EditorHistory,
) -> Option<Result<EditorHistory, Diagnostic>> {
    let(next_entry, _, _) = h.future.last()?;
    let live = match revive(restore, h, next_entry) {
        Ok(live) => live,
        Err(error) => return Some(Err(error)),
    };
    let mut next = h.clone();
    let(_, label, n) = next.future.pop().expect("checked non-empty above");
    let(entry, current_label, m) = entry_of(&next);
    let old_present = std::mem::replace(&mut next.present, live);
    next.past.push_back((entry.clone(), current_label, m));
    next.label = label;
    next.recent = Some((entry, old_present));
    next.depth += 1;
    next.byte_depth = next.byte_depth + m - n;
    next.merge = None;
    Some(Ok(trim(next)))
}

/// The oldest undo entries gone, down to [keep](at least one).
pub fn shed(keep: i64, h: EditorHistory) -> EditorHistory {
    trim_to(keep.max(1), h)
}

/// No entries left, and none charged: the editor is closing.
pub fn release(mut h: EditorHistory) -> EditorHistory {
    h.past.clear();
    h.future.clear();
    h.recent = None;
    h.depth = 0;
    h.byte_depth = 0;
    h.merge = None;
    h
}

/// A new document: [`release`], with this present.
pub fn restart(value: Arc<A>, h: EditorHistory) -> EditorHistory {
    let mut h = release(h);
    h.present = value;
    h.label = String::new();
    h
}

/// Return the retained present Arc.
pub fn present_arc(h: &EditorHistory) -> Arc<A> {
    h.present.clone()
}
