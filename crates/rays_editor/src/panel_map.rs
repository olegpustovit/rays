//! The graph panels' own state, by panel key, with the key of the one in use.
//!
//! Every graph panel keeps its canvas, level, selection, graph and route here; "the pane in use" is
//! only the key the map points at, so switching the focus changes a key and copies nothing. The key
//! `[]` is the pane of a document that shows no graph panel yet: the first panel adopts it.
//!
//! OCaml's `'a t` is monomorphised to its one instance, `Pane` = `Editor_model.graph_pane`.
#![allow(non_camel_case_types)]
use editor_core::panels::Path;

pub use crate::editor_model::graph_pane as Pane;

pub type Key = Vec<String>;

#[derive(Clone)]
pub struct PanelMap {
    in_use: Key,
    at: Option<Path>,
    panes: Vec<(Key, Pane)>,
}

/// One pane, in use, with no panel yet.
pub fn make(pane: Pane) -> PanelMap {
    PanelMap { in_use: Vec::new(), at: None, panes: vec![(Vec::new(), pane)] }
}

/// The pane in use.
pub fn active(t: &PanelMap) -> &Pane {
    &t.panes.iter().find(|(key, _)| *key == t.in_use).expect("Panel_map: in_use is always a key of panes").1
}

pub fn find<'a>(t: &'a PanelMap, key: &Key) -> Option<&'a Pane> {
    t.panes.iter().find(|(k, _)| k == key).map(|(_, pane)| pane)
}

pub fn entry<'a>(t: &'a PanelMap, key: &Key) -> Option<(&'a Key, &'a Pane)> {
    t.panes.iter().find(|(k, _)| k == key).map(|(key, pane)| (key, pane))
}

/// The pane in use, to change in place.
pub fn active_mut(t: &mut PanelMap) -> &mut Pane {
    let in_use = &t.in_use;
    &mut t.panes.iter_mut().find(|(key, _)| key == in_use).expect("Panel_map: in_use is always a key of panes").1
}

/// The panel the pane in use belongs to; none before the first graph panel.
pub fn key_ref(t: &PanelMap) -> Option<&Key> {
    if t.in_use.is_empty() { None } else { Some(&t.in_use) }
}

pub fn key(t: &PanelMap) -> Option<Key> { key_ref(t).cloned() }

/// Where that panel's leaf was at the last frame.
pub fn leaf(t: &PanelMap) -> Option<Path> {
    t.at.clone()
}

pub fn set_leaf(t: &mut PanelMap, at: Option<Path>) {
    t.at = at;
}

fn set_in(t: &mut PanelMap, k: &Key, pane: Pane) {
    t.panes.retain(|(key, _)| key != k);
    t.panes.insert(0, (k.clone(), pane));
}

/// The panel's pane, added or replaced (in use or not).
pub fn set(t: &mut PanelMap, k: &Key, pane: Pane) {
    set_in(t, k, pane)
}

/// The panes not in use.
pub fn others(t: &PanelMap) -> Vec<(Key, Pane)> {
    t.panes.iter().filter(|(k, _)| *k != t.in_use).cloned().collect()
}

/// The panel's pane is the one in use; `create` makes it when the map has none.
pub fn activate(t: &mut PanelMap, k: &Key, create: &dyn Fn() -> Pane) {
    if t.in_use == *k && t.panes.iter().any(|(key, _)| key == k) { return; }
    if !t.panes.iter().any(|(key, _)| key == k) { set_in(t, k, create()) }
    t.in_use = k.clone();
}

/// The pane of no panel becomes this panel's.
pub fn adopt(t: &mut PanelMap, k: &Key) {
    let at_use = t.panes.iter().position(|(key, _)| *key == t.in_use).expect("Panel_map: in_use is always a key of panes");
    let current = t.panes.remove(at_use).1;
    let in_use = t.in_use.clone();
    t.panes.retain(|(key, _)| key != &in_use);
    t.panes.insert(0, (k.clone(), current));
    t.in_use = k.clone();
}

/// These are the panes not in use now.
pub fn with_others(t: &mut PanelMap, panes: Vec<(Key, Pane)>) {
    let mine = std::mem::take(&mut t.panes);
    let current = mine.into_iter().find(|(key, _)| *key == t.in_use).expect("Panel_map: in_use is always a key of panes").1;
    let mut all: Vec<(Key, Pane)> = panes.into_iter().filter(|(k, _)| *k != t.in_use).collect();
    all.insert(0, (t.in_use.clone(), current));
    t.panes = all;
}

/// The panes of the panels that pass, and the one in use.
pub fn retain(t: &mut PanelMap, keep: &dyn Fn(&Key) -> bool) {
    let in_use = t.in_use.clone();
    t.panes.retain(|(k, _)| *k == in_use || keep(k));
}
