//! The graph panels' own state, by panel key, with the key of the one in use.
//!
//! Every graph panel keeps its canvas, level, selection, graph and route here; "the pane in use" is
//! only the key the map points at, so switching the focus changes a key and copies nothing. The key
//! `[]` is the pane of a document that shows no graph panel yet: the first panel adopts it.
//!
//! OCaml's `'a t` is monomorphised to its one instance, `Pane` = `Editor_model.graph_pane`.
#![allow(non_camel_case_types)]
use editor_core::panels::Path;
use std::rc::Rc;


pub use crate::editor_model::graph_pane as Pane;

pub type Key = Vec<String>;

#[derive(Clone)]
struct inner {
    in_use: Key,
    at: Option<Path>,
    panes: Vec<(Key, Pane)>,
}

/// Shared: the editor model holding it is cloned several times a frame; a change copies it once.
#[derive(Clone)]
pub struct t {
    s: Rc<inner>,
}

fn edit(t: t, f: impl FnOnce(&mut inner)) -> t {
    let mut i = Rc::unwrap_or_clone(t.s);
    f(&mut i);
    t { s: Rc::new(i) }
}

/// One pane, in use, with no panel yet.
pub fn make(pane: Pane) -> t {
    t { s: Rc::new(inner { in_use: Vec::new(), at: None, panes: vec![(Vec::new(), pane)] }) }
}

/// The pane in use.
pub fn active(t: &t) -> &Pane {
    &t.s.panes.iter().find(|(key, _)| *key == t.s.in_use).expect("Panel_map: in_use is always a key of panes").1
}

/// The panel the pane in use belongs to; none before the first graph panel.
pub fn key(t: &t) -> Option<Key> {
    if t.s.in_use.is_empty() { None } else { Some(t.s.in_use.clone()) }
}

/// Where that panel's leaf was at the last frame.
pub fn leaf(t: &t) -> Option<Path> {
    t.s.at.clone()
}

pub fn set_leaf(at: Option<Path>, t: t) -> t {
    if t.s.at == at { return t; }
    edit(t, |i| i.at = at)
}

fn set_in(i: &mut inner, k: &Key, pane: Pane) {
    i.panes.retain(|(key, _)| key != k);
    i.panes.insert(0, (k.clone(), pane));
}

/// The panel's pane, added or replaced (in use or not).
pub fn set(k: &Key, pane: Pane, t: t) -> t {
    edit(t, |i| set_in(i, k, pane))
}

/// The pane in use, changed.
pub fn update(f: &dyn Fn(&Pane) -> Pane, t: t) -> t {
    let next = f(active(&t));
    edit(t, |i| { let k = i.in_use.clone(); set_in(i, &k, next) })
}

/// The panes not in use.
pub fn others(t: &t) -> Vec<(Key, Pane)> {
    t.s.panes.iter().filter(|(k, _)| *k != t.s.in_use).cloned().collect()
}

/// The panel's pane is the one in use; `create` makes it when the map has none.
pub fn activate(k: &Key, create: &dyn Fn() -> Pane, t: t) -> t {
    if t.s.in_use == *k && t.s.panes.iter().any(|(key, _)| key == k) { return t; }
    edit(t, |i| {
        if !i.panes.iter().any(|(key, _)| key == k) { set_in(i, k, create()) }
        i.in_use = k.clone();
    })
}

/// The pane of no panel becomes this panel's.
pub fn adopt(k: &Key, t: t) -> t {
    edit(t, |i| {
        let at_use = i.panes.iter().position(|(key, _)| *key == i.in_use).expect("Panel_map: in_use is always a key of panes");
        let current = i.panes.remove(at_use).1;
        let in_use = i.in_use.clone();
        i.panes.retain(|(key, _)| key != &in_use);
        i.panes.insert(0, (k.clone(), current));
        i.in_use = k.clone();
    })
}

/// These are the panes not in use now.
pub fn with_others(panes: Vec<(Key, Pane)>, t: t) -> t {
    edit(t, |i| {
        let mine = std::mem::take(&mut i.panes);
        let current = mine.into_iter().find(|(key, _)| *key == i.in_use).expect("Panel_map: in_use is always a key of panes").1;
        let mut all: Vec<(Key, Pane)> = panes.into_iter().filter(|(k, _)| *k != i.in_use).collect();
        all.insert(0, (i.in_use.clone(), current));
        i.panes = all;
    })
}

/// The panes of the panels that pass, and the one in use.
pub fn retain(keep: &dyn Fn(&Key) -> bool, t: t) -> t {
    if t.s.panes.iter().all(|(k, _)| *k == t.s.in_use || keep(k)) { return t; }
    edit(t, |i| {
        let in_use = i.in_use.clone();
        i.panes.retain(|(k, _)| *k == in_use || keep(k));
    })
}
