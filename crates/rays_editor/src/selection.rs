//! The rows selected in the list of the open level (scene objects, World layers, the nodes of an
//! object's network): view state, never in history. The first id is the one the inspector and the
//! handles follow.

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Selection {
    pub ids: Vec<i64>,
    pub primary: Option<i64>,
}

pub fn empty() -> Selection {
    Selection { ids: Vec::new(), primary: None }
}

pub fn selected(t: &Selection) -> Option<i64> {
    t.primary
}

pub fn selected_nodes(t: &Selection) -> &[i64] {
    &t.ids
}

pub fn select(id: i64) -> Selection {
    Selection { ids: vec![id], primary: Some(id) }
}

/// The first id is primary; the ids are kept sorted and unique.
pub fn select_nodes(ids: &[i64]) -> Selection {
    match ids.first() {
        None => empty(),
        Some(first) => {
            let mut sorted = ids.to_vec();
            sorted.sort();
            sorted.dedup();
            Selection { ids: sorted, primary: Some(*first) }
        }
    }
}

pub fn clear() -> Selection {
    empty()
}
