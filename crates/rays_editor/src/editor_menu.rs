//! the entries of the node menu at a screen point. Bundle C.

use flow_syntax::context::{self, Context};
use flow_syntax::ty;
use rays_graph::node_menu::{self as menu, Entry};
use rays_graph::scope_state as scope;

use crate::editor_add::{add_target, binding_name, value_entries};
use crate::editor_inspect::kinds;
use crate::editor_model::*;
use crate::navigator;

/// What the add menu of a scene adds beside the object kinds: the World, a merge, and the geometry
/// of each SOP graph (a second object over a graph that already exists).
pub fn scene_entries<'a>(value: &ModelView<'a>) -> Vec<Entry> {
    let entry = |key: &str, label: &str, category: &[&str]| Entry {
        key: key.to_owned(),
        label: label.to_owned(),
        category: category.iter().map(|c| (*c).to_owned()).collect(),
        arity: 0,
        context: "scene".to_owned(),
        output: ty::geometry(),
        off: None,
    };
    let mut out = vec![entry("world", "World", &["Object"]), entry("merge", "Merge", &["Object"]), entry("material", "Material", &["Material"])];
    for c in navigator::new_graph_contexts() {
        out.push(entry(&format!("newgraph:{c}"), &format!("New graph: {c}"), &["Graph"]));
    }
    for g in value.doc.workspace.ws.checked.graphs.iter() {
        if g.context == context::sop() {
            out.push(entry(&format!("of:{}", g.name), &g.name, &["Object", "Geometry of..."]));
        }
    }
    out
}

/// In a SOP graph: a sop/material of each material graph, after the selection.
pub fn material_entries<'a>(value: &ModelView<'a>) -> Vec<Entry> {
    value
        .doc
        .workspace
        .ws
        .checked
        .graphs
        .iter()
        .filter(|g| g.context == context::material())
        .map(|g| Entry {
            key: format!("of-material:{}", g.name),
            label: g.name.clone(),
            category: vec!["Material of...".to_owned()],
            arity: 1,
            context: "sop".to_owned(),
            output: ty::geometry(),
            off: None,
        })
        .collect()
}

/// The graph a menu entry is for, as the entries name it.
pub fn context_name(c: &Context) -> String {
    flow_check::workspace::context_name(c)
}

/// The kinds the node menu offers where the pane shows `graph`, at a screen point; the kinds of the
/// other graphs follow in the search, in ink-3, saying that they are not placed here. The title
/// says which node the new one goes after.
pub fn open_menu<'a>(scope_view: Option<&scope::Pane>, value: &ModelView<'a>, (x, y): (i64, i64)) -> Option<menu::Menu> {
    let scope_view = scope_view.unwrap_or_else(|| pane_view(value));
    let (_, context) = add_target(value)?;
    let extra = &value.doc.workspace.ws.checked.ops;
    let factories = |c: &Context| -> Vec<Entry> {
        kinds(value, c)
            .into_iter()
            .map(|d| Entry {
                key: d.key.clone(),
                label: d.label.clone(),
                category: d.category.clone(),
                arity: d.slots.len() as i64 - d.keyword_inputs.len() as i64,
                context: context_name(c),
                output: ty::geometry(),
                off: None,
            })
            .collect()
    };
    let not_here = |entries: Vec<Entry>| -> Vec<Entry> {
        entries.into_iter().map(|e| Entry { off: Some(format!("not in {}", context_name(&context))), ..e }).collect()
    };
    let mut elsewhere = if context == context::scene() { Vec::new() } else { not_here(scene_entries(value)) };
    for other in [context::scene(), context::world(), context::sop()] {
        if other != context {
            elsewhere.extend(not_here(factories(&other)));
        }
    }
    let selected = scope::selected(scope_view);
    let after = match selected.as_slice() {
        [path] if path.len() >= 2 => binding_name(path),
        _ => None,
    };
    let mut entries = factories(&context);
    if context == context::value() {
        entries.extend(value_entries());
    }
    entries.extend(menu::of_ops(extra, &context));
    if context == context::scene() {
        entries.extend(scene_entries(value));
    }
    if context == context::sop() {
        entries.extend(material_entries(value));
    }
    entries.extend(value_entries());
    entries.extend(menu::of_ops(extra, &context::value()));
    entries.extend(elsewhere);
    Some(menu::create(after.as_deref(), x, y, entries))
}
