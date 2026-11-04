//! The graph panels' own state, by panel key, with the key of the one in use.
//!
//! Every graph panel keeps its canvas, level, selection, graph and route here; "the pane in use" is
//! only the key the map points at, so switching the focus changes a key and copies nothing. The key
//! `[]` is the pane of a document that shows no graph panel yet: the first panel adopts it.
//!
//! OCaml's `'a t` is monomorphised to its one instance, `Pane` = `Editor_model.graph_pane`.
#![allow(non_camel_case_types)]
use editor_core::panels::Path;

// TEMP-EDA: swap to `pub use crate::editor_model::graph_pane as Pane;` once editor_model exists
pub type Pane = i64;

pub type Key = Vec<String>;

#[derive(Clone)]
pub struct t {
    pub in_use: Key,
    pub at: Option<Path>,
    pub panes: Vec<(Key, Pane)>,
}

/// One pane, in use, with no panel yet.
pub fn make(pane: Pane) -> t {
    t { in_use: Vec::new(), at: None, panes: vec![(Vec::new(), pane)] }
}

/// The pane in use.
pub fn active(t: &t) -> &Pane {
    &t.panes.iter().find(|(key, _)| *key == t.in_use).expect("Panel_map: in_use is always a key of panes").1
}

/// The panel the pane in use belongs to; none before the first graph panel.
pub fn key(t: &t) -> Option<Key> {
    if t.in_use.is_empty() { None } else { Some(t.in_use.clone()) }
}

/// Where that panel's leaf was at the last frame.
pub fn leaf(t: &t) -> Option<Path> {
    t.at.clone()
}

pub fn set_leaf(at: Option<Path>, mut t: t) -> t {
    t.at = at;
    t
}

/// The panel's pane, added or replaced (in use or not).
pub fn set(k: &Key, pane: Pane, mut t: t) -> t {
    t.panes.retain(|(key, _)| key != k);
    t.panes.insert(0, (k.clone(), pane));
    t
}

/// The pane in use, changed.
pub fn update(f: &dyn Fn(&Pane) -> Pane, t: t) -> t {
    let next = f(active(&t));
    let k = t.in_use.clone();
    set(&k, next, t)
}

/// The panes not in use.
pub fn others(t: &t) -> Vec<(Key, Pane)> {
    t.panes.iter().filter(|(k, _)| *k != t.in_use).cloned().collect()
}

/// The panel's pane is the one in use; `create` makes it when the map has none.
pub fn activate(k: &Key, create: &dyn Fn() -> Pane, t: t) -> t {
    let mut t = if t.panes.iter().any(|(key, _)| key == k) { t } else { set(k, create(), t) };
    t.in_use = k.clone();
    t
}

/// The pane of no panel becomes this panel's.
pub fn adopt(k: &Key, t: t) -> t {
    let t { in_use, at, mut panes } = t;
    let at_use = panes.iter().position(|(key, _)| *key == in_use).expect("Panel_map: in_use is always a key of panes");
    let current = panes.remove(at_use).1;
    panes.retain(|(key, _)| key != &in_use);
    panes.insert(0, (k.clone(), current));
    t { in_use: k.clone(), at, panes }
}

/// These are the panes not in use now.
pub fn with_others(panes: Vec<(Key, Pane)>, t: t) -> t {
    let t { in_use, at, panes: mine } = t;
    let current = mine.into_iter().find(|(key, _)| *key == in_use).expect("Panel_map: in_use is always a key of panes").1;
    let mut all: Vec<(Key, Pane)> = panes.into_iter().filter(|(k, _)| *k != in_use).collect();
    all.insert(0, (in_use.clone(), current));
    t { in_use, at, panes: all }
}

/// The panes of the panels that pass, and the one in use.
pub fn retain(keep: &dyn Fn(&Key) -> bool, mut t: t) -> t {
    let in_use = t.in_use.clone();
    t.panes.retain(|(k, _)| *k == in_use || keep(k));
    t
}
