//! The rows selected in the list of the open level (scene objects, World layers, the nodes of an
//! object's network): view state, never in history. The first id is the one the inspector and the
//! handles follow.
#![allow(non_camel_case_types)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct t {
    pub ids: Vec<i64>,
    pub primary: Option<i64>,
}

pub fn empty() -> t {
    t { ids: Vec::new(), primary: None }
}

pub fn selected(t: &t) -> Option<i64> {
    t.primary
}

pub fn selected_nodes(t: &t) -> &[i64] {
    &t.ids
}

pub fn select(id: i64) -> t {
    t { ids: vec![id], primary: Some(id) }
}

/// The first id is primary; the ids are kept sorted and unique.
pub fn select_nodes(ids: &[i64]) -> t {
    match ids.first() {
        None => empty(),
        Some(first) => {
            let mut sorted = ids.to_vec();
            sorted.sort();
            sorted.dedup();
            t { ids: sorted, primary: Some(*first) }
        }
    }
}

pub fn clear() -> t {
    empty()
}
