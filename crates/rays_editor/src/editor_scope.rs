//! Port of lib/rays_editor/editor_scope.ml: lay the workspace pane out again when the document, the
//! probes or the graph changed (`sync_scope`), the probe previews it shows (`sync_views`), and the
//! inspector's subject for a scene object or World layer (`scene_target`). Bundle C.
#![allow(non_camel_case_types)]
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;
use std::sync::Arc;

use editor_document::document::{Home, Level};
use editor_document::layout_by_path;
use editor_document::workspace_doc;
use flow_eval::Eval;
use flow_graph::flow_edit::ArgKey;
use flow_graph::probe::{self, Options, Probe};
use flow_graph::projection as p;
use flow_native::probe_run;
use flow_sop::attribute_kernel;
use flow_sop::lower;
use flow_syntax::phase_timer;
use flow_syntax::syntax::Syntax;
use flow_syntax::ty::{self, Ty};
use flow_syntax::value::Value;
use pxui_graph::scope_state as scope;
use sop::context as sop_context;

use crate::editor_inspect::{node_owner_of, viewed_path};
use crate::editor_model::*;
use crate::editor_panes::*;
use crate::editor_shell::*;
use crate::timeline;

type S = Syntax;
type Path = Vec<String>;

/// The plan node of a drawing or image at `path`, with its type.
pub fn picture_node(records: &Rc<RefCell<Probe>>, path: &[String], probes: &[i64]) -> Option<(Ty, i64)> {
    [ty::drawing(), ty::image()].into_iter().find_map(|t_| records.borrow().plan_node(Some(&t_), path, probes).map(|id| (t_, id)))
}

/// Probe inputs use the same exact frame/region compiler as the canvas.
pub fn probe_force() -> probe::Force {
    probe_run::force()
}

fn probes_for(value: &t, chain: &[Path]) -> Vec<i64> {
    chain.iter().map(|path| value.probes.get(path).copied().unwrap_or(0)).collect()
}

/// Resolve the previews (viewport requests and the canvas's picture) of the model against its
/// lowering, once per change of the lowering, the probes or the previews.
pub fn sync_views(value: &mut t) {
    let lowered = &value.doc.workspace.lowered;
    if let Some((previous, probes, previews, canvas)) = &value.view_key {
        if Arc::ptr_eq(previous, &lowered.evaluated) && *probes == value.probes && *previews == value.previews && *canvas == value.canvas_preview {
            return;
        }
    }
    if value.previews.is_empty() && value.canvas_preview.is_none() {
        let evaluated = lowered.evaluated.clone();
        value.viewed = BTreeMap::new();
        value.canvas_viewed = None;
        value.view_key = Some((evaluated, value.probes.clone(), value.previews.clone(), value.canvas_preview.clone()));
        return;
    }
    let dynamic_lowered = lowered.clone();
    let options = Options {
        clock: Some(value.clock.clone()),
        dynamic: Some(Rc::new(move |site: &[String]| lower::zone_count(&dynamic_lowered, &site.to_vec()))),
        ..Default::default()
    };
    let records = Rc::new(RefCell::new(probe::make(&lowered.evaluated, options)));
    let mut previews = BTreeMap::new();
    let mut viewed = BTreeMap::new();
    for (object_id, pv) in &value.previews {
        let probes = probes_for(&*value, &pv.chain);
        let viewed_node = records.borrow().plan_node(None, &pv.path, &probes).and_then(|node| {
            value.doc.networks.get(object_id).and_then(|network| (lowered.preview)(node, &probes, &network.graph))
        });
        if let Some(vn) = viewed_node {
            previews.insert(*object_id, pv.clone());
            viewed.insert(*object_id, vn);
        }
    }
    let (canvas_preview, canvas_viewed) = match &value.canvas_preview {
        None => (None, None),
        Some(pv) => {
            let shown = picture_node(&records, &pv.path, &probes_for(&*value, &pv.chain));
            match shown {
                Some(_) => (value.canvas_preview.clone(), shown),
                None => (None, None),
            }
        }
    };
    let evaluated = lowered.evaluated.clone();
    value.view_key = Some((evaluated, value.probes.clone(), previews.clone(), canvas_preview.clone()));
    value.previews = previews;
    value.viewed = viewed;
    value.canvas_preview = canvas_preview;
    value.canvas_viewed = canvas_viewed;
}

/// A node asked to be viewed: the preview of the object that owns it, else the canvas's.
pub fn view_node(value: &mut t, path: &[String]) {
    let Some(key) = pane_laid(&*value).cloned() else { return };
    let chain = probe::chains(&key.scope).get(path).cloned().unwrap_or_default();
    let probes = probes_for(&*value, &chain);
    let node = key.records.as_ref().and_then(|records| records.borrow().plan_node(None, path, &probes));
    let lowered = value.doc.workspace.lowered.clone();
    let owner = node.and_then(|node| {
        let owns = |id: i64| value.doc.networks.get(&id).and_then(|network| (lowered.preview)(node, &probes, &network.graph)).is_some();
        match pane_level(&*value) {
            Level::Inside(id) if owns(id) => Some(id),
            _ => value.doc.networks.keys().copied().find(|id| owns(*id)),
        }
    });
    match owner {
        Some(owner) => {
            value.previews.insert(owner, preview { path: path.to_vec(), chain });
            sync_views(value)
        }
        None => {
            let picture = key.records.as_ref().and_then(|records| picture_node(records, path, &probes));
            if picture.is_none() {
                return;
            }
            match &value.canvas_preview {
                Some(held) if held.path == path => value.canvas_preview = None,
                _ => value.canvas_preview = Some(preview { path: path.to_vec(), chain }),
            }
            sync_views(value)
        }
    }
}

/// What the document keeps of each node, for the pane to ask (`scope::Saved`).
struct DocSaved {
    layout: layout_by_path::T,
    probes: layout_by_path::PathMap<i64>,
    imports: Vec<(String, Vec<Syntax>)>,
}

impl scope::Saved for DocSaved {
    fn at(&self, path: &[String]) -> Option<(f64, f64)> {
        self.layout.at.get(path).copied()
    }
    fn level(&self, path: &[String]) -> Option<(p::Level, bool)> {
        match (self.layout.level.get(path), self.layout.pinned.get(path)) {
            (None, None) => None,
            (level, pinned) => Some((level.copied().unwrap_or(p::Level::Card), pinned.copied().unwrap_or(false))),
        }
    }
    fn pin(&self, path: &[String], label: &str) -> Option<bool> {
        self.layout.rows.get(path).and_then(|m| m.get(label)).copied()
    }
    // a function or loop starts open, its body in view: c folds it
    fn collapsed(&self, path: &[String]) -> bool {
        self.layout.collapsed.get(path) == Some(&true)
    }
    fn probe(&self, path: &[String]) -> i64 {
        self.probes.get(path).copied().unwrap_or(0)
    }
    fn frames(&self, path: &[String]) -> Vec<scope::Frame3> {
        self.layout.frames.get(path).map(|fs| fs.iter().map(|f| (f.title.clone(), f.at, f.size)).collect()).unwrap_or_default()
    }
    fn imported(&self, path: &[String]) -> Option<String> {
        workspace_doc::imported_file(&self.imports, path)
    }
}

/// Lay the workspace pane out again when the document, the probes or the graph changed; the
/// footers are rebuilt when the recording evaluation, the cook's geometry counts or (a live
/// document) the time changed. The evaluation is retained by lowering, never repeated by a pane
/// or a frame.
pub fn sync_scope(value: &mut t) {
    sync_views(value);
    if let Some(key) = pane_laid(&*value).cloned() {
        let display = viewed_path(&*value, &key.scope);
        let view = &mut crate::panel_map::active_mut(&mut value.panes).view;
        if view.display != display {
            view.display = display;
        }
    }
    let Some(name) = graph_name(&*value) else { return };
    let Some(catalog) = flow_catalog(&*value) else { return };
    let document = value.doc.clone();
    let workspace = &document.workspace;
    let (ws, lowered) = (&workspace.ws, &workspace.lowered);
    let previous = pane_laid(&*value).cloned();
    let arguments: Option<Vec<editor_document::literal_edit::Change>> = previous.as_ref().and_then(|k| {
        if k.graph != name || k.ws.layout.collapsed != ws.layout.collapsed {
            None
        } else {
            match workspace_doc::literal_changes(&k.ws, ws) {
                Some(changes) => Some(changes),
                None if p::same_graph(&k.ws.checked, &ws.checked, &name) => Some(vec![]),
                None => None,
            }
        }
    });
    let edits: Vec<(Path, ArgKey, S)> =
        arguments.as_deref().unwrap_or(&[]).iter().map(|c| (c.path.clone(), ArgKey::Kw(c.field.clone()), c.expr.clone())).collect();
    let same = |k: &scope_key| k.graph == name && k.ws.layout == ws.layout && arguments.is_some();
    let moved = match &previous {
        Some(k) => !(same(k) && k.probe_map == value.probes),
        None => true,
    };
    let evaluated: Arc<Eval> = match &previous {
        Some(k) if k.evaluated.as_ref().map_or(false, |e| Arc::ptr_eq(e, &lowered.evaluated)) => k.evaluated.clone().unwrap(),
        _ => lowered.evaluated.clone(),
    };
    let time = if !ws.checked.live.is_empty() {
        Some(input::Frame { t: timeline::time(&value.timeline), frame: timeline::frame(&value.timeline), ..value.live_frame.clone() })
    } else {
        None
    };
    let now = (value.clock)();
    let summaries = value.cook.summaries.clone();
    let execution_count = probe_run::report_count(&lowered.profile);
    // a moving clock or pointer re-forces the probes at most five times a second: a forced frame
    // sets the quiet time to four times its own length, so forcing never takes more than a fifth
    // of the time
    // the executions are a list forcing a probe rewrites, so it is never physically the same
    // twice: comparing it by identity made every frame fresh. Their count changes when a probe
    // first runs, which is when a footer needs them.
    let quiet = |k: &scope_key| -> f64 { 2.0f64.min(0.2f64.max(4.0 * (now - k.stamp))) };
    let fresh = match &previous {
        Some(k) => !(k.evaluated.as_ref().map_or(false, |e| Arc::ptr_eq(e, &evaluated))
            && k.summaries == summaries
            && execution_count == k.executions.len()
            && (match (&k.time, &time) {
                (Some(a), Some(b)) => !sop_context::input_changed(**lowered.reads, a, b),
                (None, None) => true,
                _ => false,
            } || (now - k.stamp < k.gap && k.time.is_some() == time.is_some()))),
        None => true,
    };
    if !moved && !fresh {
        // the first call after a forced frame learns what that frame cost
        if let Some(k) = crate::panel_map::active_mut(&mut value.panes).laid.as_mut() {
            let gap = if k.fresh_last { quiet(k) } else { k.gap };
            if k.fresh_last || k.gap != gap {
                let k = Rc::make_mut(k);
                k.fresh_last = false;
                k.gap = gap;
            }
        }
        return;
    }
    let executions = probe_run::reports(&lowered.profile);
    let stamp = match &previous {
        Some(k) if !fresh => k.stamp,
        _ => now,
    };
    let gap = match &previous {
        Some(k) if k.fresh_last && fresh => quiet(k),
        Some(k) => k.gap,
        None => 0.2,
    };
    let folded_ws = ws.clone();
    let scope_proj: Rc<p::Scope> = match &previous {
        Some(k) if arguments.is_some() => {
            if edits.is_empty() {
                k.scope.clone()
            } else {
                Rc::new(p::with_arguments(&edits, &k.scope))
            }
        }
        _ => Rc::new(
            p::of_graph_with(&|path: &[String]| folded_ws.layout.collapsed.get(path) != Some(&false), &catalog, &ws.checked, &name)
                .expect("the pane's graph is checked"),
        ),
    };
    let scope_view = if !moved {
        scope::with_arguments(&edits, pane_view(&*value).clone())
    } else {
        let display = viewed_path(&*value, &scope_proj);
        let layouts: Vec<String> = match &value.doc.shell {
            Some(sh) if sh.switch.is_some() && workspace_doc::editor_graph(&value.doc.workspace.ws).map(|g| g.name) == Some(name.clone()) => {
                layouts(&*value).into_iter().map(|(n, _)| n).collect()
            }
            _ => Vec::new(),
        };
        let saved = Rc::new(DocSaved { layout: ws.layout.clone(), probes: value.probes.clone(), imports: ws.imports.clone() });
        let opts = scope::ScopeOpts { saved, display, layouts };
        let _phase = phase_timer::measure("layout");
        scope::with_scope(opts, &name, scope_proj.clone(), pane_view(&*value).clone())
    };
    // Probe snapshots need the published facts, not the previous cook's compiled graphs and their arrays.
    let owner = node_owner_of(&*value);
    let summaries = value.cook.summaries.clone();
    let sources = value.cook.sources.clone();
    let geometry: Rc<dyn Fn(i64) -> Option<probe::Geometry>> = {
        let (lowered, owner, summaries) = (lowered.clone(), owner.clone(), summaries.clone());
        Rc::new(move |id: i64| {
            let node_id = *lowered.compiled.get(&id)?;
            let object_id = owner(node_id)?;
            summaries.iter().find(|(k, _)| *k == (object_id, node_id)).map(|(_, g)| g.clone())
        })
    };
    // a pick in another graph's geometry asked for its node to be selected here
    let (scope_view, select_later) = if !value.select_later.is_empty() && value.select_later.iter().all(|pth| pth.first() == Some(&name)) {
        (scope::select(value.select_later.clone(), scope_view), Vec::new())
    } else {
        (scope_view, value.select_later.clone())
    };
    let element: Rc<dyn Fn(&[String], i64) -> Option<Vec<(String, Value)>>> = {
        let lowered = lowered.clone();
        Rc::new(move |zone: &[String], k: i64| lower::zone_element(&lowered, &zone.to_vec(), k).map(|(n, (x, y, z))| vec![(n, Value::Vec3(x, y, z))]))
    };
    let resolve: probe::ResolveFn = {
        let (lowered, owner, sources) = (lowered.clone(), owner.clone(), sources.clone());
        let lookup: flow_sop::attribute_kernel::GeometryLookup = Arc::new(move |id: i64| {
            let node_id = *lowered.compiled.get(&id)?;
            let object_id = owner(node_id)?;
            sources.iter().find(|(k, _)| *k == (object_id, node_id)).map(|(_, g)| g.clone())
        });
        Arc::new(move |v: &Value| attribute_kernel::resolve(None, &lookup, v))
    };
    let instance: Option<i64> = lowered.plan.instances.iter().position(|i| i.graph == name && i.default).map(|i| i as i64);
    let execution: Rc<dyn Fn(&[String], &[i64]) -> Option<probe::Execution>> = {
        let executions = executions.clone();
        Rc::new(move |path: &[String], probes: &[i64]| probe_run::find(&executions, instance, path, probes))
    };
    let force = match &previous {
        Some(k) if k.evaluated.as_ref().map_or(false, |e| Arc::ptr_eq(e, &evaluated)) => k.force.clone(),
        _ => probe_force(),
    };
    // a moved node or a dragged pane changes the layout, not the values: the records keep their
    // memo of footers and probes (a rebuild each frame of a drag cost about 0.8 s here)
    let reuse = match &previous {
        Some(k) if !fresh => k.records.clone(),
        _ => None,
    };
    let records: Option<Rc<RefCell<Probe>>> = if reuse.is_some() {
        reuse
    } else {
        let live_frame = time.clone().unwrap_or_else(|| input::at_time(0.0));
        let field_state = value.cook.state.clone();
        let bulk_calls: Rc<dyn Fn(&[String], &[i64]) -> Vec<probe::Bulk>> = {
            let (lowered, resolve, state, live) = (lowered.clone(), resolve.clone(), field_state.clone(), live_frame.clone());
            Rc::new(move |zone: &[String], outer: &[i64]| {
                lower::field_calls(Some(&state), &live, resolve.clone(), &lowered, &zone.to_vec(), outer)
                    .into_iter()
                    .map(|(offset, count, f)| (offset, count, Rc::new(move |i: i64| f(i)) as Rc<dyn Fn(i64) -> Result<probe::Records, rays_diagnostic::Diagnostic>>))
                    .collect()
            })
        };
        let image: Rc<dyn Fn(i64) -> Option<(i64, i64)>> = {
            let plan = lowered.plan.clone();
            Rc::new(move |node: i64| lower::image_metadata(&plan, node))
        };
        let dynamic: Rc<dyn Fn(&[String]) -> Option<i64>> = {
            let lowered = lowered.clone();
            Rc::new(move |site: &[String]| lower::zone_count(&lowered, &site.to_vec()))
        };
        let options = Options {
            clock: Some(value.clock.clone()),
            state: Some(field_state),
            live: time.clone().map(Into::into),
            resolve: Some(resolve.clone()),
            force: Some(force.clone()),
            bulk_calls: Some(bulk_calls),
            execution: Some(execution),
            geometry: Some(geometry),
            image: Some(image),
            dynamic: Some(dynamic),
            element: Some(element),
            ..Default::default()
        };
        Some(Rc::new(RefCell::new(probe::make(&evaluated, options))))
    };
    let scope_view = match &records {
        Some(records) if fresh || moved => scope::with_records(records.clone(), scope_view),
        _ => scope_view,
    };
    let targets: Vec<(Path, (i64, i64))> = match (&records, &previous) {
        (Some(_), Some(k))
            if !moved
                && (k.evaluated.as_ref().map_or(false, |e| Arc::ptr_eq(e, &evaluated))
                    || k.evaluated.as_ref().map_or(false, |ev| ev.records == lowered.evaluated.records)) =>
        {
            k.targets.clone()
        }
        (Some(records), _) => {
            let probes = value.probes.clone();
            records
                .borrow()
                .geometry_targets(&scope_proj, &move |path: &[String]| probes.get(path).copied().unwrap_or(0))
                .into_iter()
                .filter_map(|(path, id)| {
                    let node_id = *lowered.compiled.get(&id)?;
                    node_owner(&*value, node_id).map(|object_id| (path, (object_id, node_id)))
                })
                .collect()
        }
        (None, _) => Vec::new(),
    };
    value.select_later = select_later;
    let key = scope_key {
        ws: ws.clone(),
        probe_map: value.probes.clone(),
        graph: name,
        evaluated: Some(evaluated),
        summaries,
        executions,
        time,
        records,
        force,
        scope: scope_proj,
        targets,
        stamp,
        gap,
        fresh_last: fresh,
    };
    with_pane(value, &|pane| graph_pane { view: scope_view.clone(), laid: Some(Rc::new(key.clone())), ..pane.clone() });
}

fn node_owner<'a>(value: impl Into<ModelView<'a>>, node_id: i64) -> Option<i64> {
    let value = &value.into();
    crate::editor_inspect::node_owner(value, node_id)
}

/// The inspector's subject for a scene object or World layer: the graph projection of the graph
/// the text binds it in, and its path there. The pane's own projection when it shows that graph,
/// else one made for the inspector alone (ponytail: laid out again each frame the inspector shows
/// such an object; the scene graphs are small). `None` for what the text does not bind by name
/// (a host object, a loop's copy, an inline call): those keep the derived-node sheet.
pub fn scene_target<'a>(value: impl Into<ModelView<'a>>, id: i64) -> Option<(scope_key, Path)> {
    let value = &value.into();
    let homes = &value.doc.homes;
    let list = match pane_level(value) {
        Level::Scene => &homes.objects,
        Level::Inside(_) => &homes.layers,
    };
    let home = list.iter().find(|(i, _)| *i == id).map(|(_, h)| h.clone());
    let catalog = flow_catalog(value);
    match (home, catalog) {
        (Some(Home::BoundAt(path)), Some(catalog)) if path.len() >= 2 => {
            let graph = path[0].clone();
            let laid = match pane_laid(value) {
                Some(k) if k.graph == graph => k.clone(),
                _ => {
                    let ws = value.doc.workspace.ws.clone();
                    let evaluated = value.doc.workspace.lowered.evaluated.clone();
                    let options = Options { clock: Some(value.clock.clone()), state: Some(value.cook.state.clone()), ..Default::default() };
                    let collapsed = ws.layout.collapsed.clone();
                    let scope_proj = p::of_graph_with(&|p_: &[String]| collapsed.get(p_) != Some(&false), &catalog, &ws.checked, &graph).ok()?;
                    scope_key {
                        ws,
                        probe_map: value.probes.clone(),
                        graph,
                        evaluated: Some(evaluated.clone()),
                        summaries: vec![],
                        time: None,
                        executions: probe_run::no_reports(),
                        records: Some(Rc::new(RefCell::new(probe::make(&evaluated, options)))),
                        force: probe_force(),
                        targets: vec![],
                        stamp: 0.0,
                        gap: 0.0,
                        fresh_last: false,
                        scope: Rc::new(scope_proj),
                    }
                }
            };
            p::find(&laid.scope, &path).map(|_| (laid, path))
        }
        _ => None,
    }
}
