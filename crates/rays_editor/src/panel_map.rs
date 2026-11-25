//! Panel_map owns one state per key; switching focus changes only the selected key. The canonical empty key is adopted by the first real graph panel.

use editor_core::panels::Path;

pub use crate::editor_model::GraphPane as Pane;

pub type Key = flow_check::workspace::Path;

/// Bound on retained graph panes across inactive layouts (same capacity as `LOCALS_CAPACITY`).
// ponytail: MRU Vec capped at PANES_CAPACITY rather than `lru::Cache` because `GraphPane` holds
// non-Send `Rc<ScopeKey>`; switch to an indexed map if panel counts ever exceed a few dozen.
pub const PANES_CAPACITY: usize = 64;

#[derive(Clone)]
pub struct PanelMap {
    in_use: Key,
    at: Option<Path>,
    panes: Vec<(Key, Pane)>,
}

fn cap(t: &mut PanelMap, keep_at_least: usize) {
    let limit = PANES_CAPACITY.max(keep_at_least);
    while t.panes.len() > limit {
        // Never evict `in_use`; evict the oldest (tail) entry that is not `in_use`.
        if let Some(pos) = t.panes.iter().rposition(|(k, _)| *k != t.in_use) {
            t.panes.remove(pos);
        } else {
            break;
        }
    }
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

pub fn find_mut<'a>(t: &'a mut PanelMap, key: &Key) -> Option<&'a mut Pane> {
    t.panes.iter_mut().find(|(k, _)| k == key).map(|(_, pane)| pane)
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
pub fn leaf(t: &PanelMap) -> Option<&Path> {
    t.at.as_ref()
}

pub fn set_leaf(t: &mut PanelMap, at: Option<Path>) {
    t.at = at;
}

fn set_in(t: &mut PanelMap, k: &Key, pane: Pane) {
    t.panes.retain(|(key, _)| key != k);
    t.panes.insert(0, (k.clone(), pane));
    cap(t, 1);
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
    if let Some(pos) = t.panes.iter().position(|(key, _)| key == k) {
        if pos > 0 {
            let entry = t.panes.remove(pos);
            t.panes.insert(0, entry);
        }
        t.in_use = k.clone();
        return;
    }
    t.in_use = k.clone();
    set_in(t, k, create());
}

/// The pane of no panel becomes this panel's.
pub fn adopt(t: &mut PanelMap, k: &Key) {
    let at_use = t.panes.iter().position(|(key, _)| *key == t.in_use).expect("Panel_map: in_use is always a key of panes");
    let current = t.panes.remove(at_use).1;
    let in_use = t.in_use.clone();
    t.panes.retain(|(key, _)| key != &in_use && key != k);
    t.panes.insert(0, (k.clone(), current));
    t.in_use = k.clone();
    cap(t, 1);
}

/// Rename a stored panel key (e.g. when a split rebinds `leaf` as `leaf_a`), keeping its state.
pub fn rename(t: &mut PanelMap, old: &Key, new: &Key, new_leaf: Option<Path>) {
    if old == new {
        if t.in_use == *new {
            if let Some(leaf) = new_leaf {
                t.at = Some(leaf);
            }
        }
        return;
    }
    if t.in_use == *old {
        t.in_use = new.clone();
        if let Some(leaf) = new_leaf {
            t.at = Some(leaf);
        }
    }
    if let Some(pos) = t.panes.iter().position(|(k, _)| k == old) {
        let (_, pane) = t.panes.remove(pos);
        t.panes.retain(|(k, _)| k != new);
        t.panes.insert(pos.min(t.panes.len()), (new.clone(), pane));
    }
}

/// Update the active layout's other panes while retaining inactive layouts' panes (bounded by MRU).
pub fn with_others(t: &mut PanelMap, panes: Vec<(Key, Pane)>) {
    if panes.is_empty() {
        return;
    }
    let active_count = panes.len() + 1;
    for (k, pane) in panes {
        if k == t.in_use {
            continue;
        }
        if let Some(pos) = t.panes.iter().position(|(key, _)| *key == k) {
            t.panes.remove(pos);
        }
        // Place right after index 0 (or at 0 if in_use wasn't at 0) so active-layout panes stay
        // ahead of inactive-layout panes in MRU order.
        let at = usize::from(!t.panes.is_empty() && t.panes[0].0 == t.in_use);
        t.panes.insert(at, (k, pane));
    }
    cap(t, active_count);
}

/// The panes of the panels that pass, and the one in use.
pub fn retain(t: &mut PanelMap, keep: &dyn Fn(&Key) -> bool) {
    let in_use = t.in_use.clone();
    t.panes.retain(|(k, _)| *k == in_use || keep(k));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::editor_model::level_map_empty;
    use crate::selection;
    use editor_document::document::Level;
    use rays_graph::scope_state as scope;

    fn dummy_pane(shown: &str) -> Pane {
        Pane {
            view: scope::create(None, None),
            laid: None,
            shown: Some(shown.to_string()),
            route: Vec::new(),
            views: level_map_empty(),
            at: Level::Scene,
            picked: selection::empty(),
        }
    }

    #[test]
    fn b6_mru_capacity_bounds_inactive_panes() {
        let mut map = make(dummy_pane("init"));
        let k0 = vec!["editor".into(), "p0".into()];
        adopt(&mut map, &k0);
        for i in 1..=(PANES_CAPACITY + 10) {
            let k = vec!["editor".into(), format!("p{i}").into()];
            activate(&mut map, &k, &|| dummy_pane(&format!("g{i}")));
        }
        assert_eq!(map.panes.len(), PANES_CAPACITY, "PanelMap must stay bounded at PANES_CAPACITY");
        // Oldest keys were evicted, newest keys kept.
        assert!(find(&map, &k0).is_none());
        let last = vec!["editor".into(), format!("p{}", PANES_CAPACITY + 10).into()];
        assert!(find(&map, &last).is_some());
    }
}
