//! Authored text is the source of truth. Scene, World, settings and layout edits rewrite source before lowering. Host-only objects reconcile into authored graphs on their first explicit edit. Change flags track source/workspace replacement; unchanged derived scene/layers compare structurally and networks by pointer identity.
use crate::document::{self, Home, Level};
use crate::kind_graph as g;
use crate::kinds;
use crate::lowering::{self, Options};
use crate::objects::{self, root};
use crate::settings;
use crate::workspace_doc;
use flow_check::catalog::Catalog;
use std::sync::Arc;
use flow_check::workspace as w;
use flow_graph::flow_edit::{self as f, ArgKey, Op};
use flow_sop_catalog::port;
use flow_syntax::context;
use flow_syntax::lisp;
use flow_syntax::syntax::{self as s, Node, Syntax};
use flow_syntax::arena::{self, Draft, SyntaxBuilder, SyntaxId};
use param::{Effects, FieldView, Value};
use sop::edit_graph::Factory;

type S = Syntax;
type R<T> = Result<T, String>;
type Path = flow_check::workspace::Path;
type Edits = Vec<(String, Option<S>)>;

/// A derived edit as its caller means it: scene objects at the scene level, World layers inside
/// the World.
#[derive(Clone)]
pub enum Edit {
    /// Field values of several objects (a stroke down a flag column).
    Fields(Vec<(i64, Vec<(String, Value)>)>),
    /// An object's name; its children's `:parent` follows.
    Rename(i64, String),
    /// Objects (their children are unparented) or layers (the stack closes). A copy of a loop is
    /// deleted exactly, at any nesting depth and for any number of clauses: the iteration that
    /// made it (or, when its iteration made other objects that stay, its place in the
    /// `scene/merge` that holds it) is added to a `:skip` list (register L16), and every other
    /// copy keeps its iteration tuple.
    Delete(Vec<i64>),
    /// The World's layers, bottom first.
    Restack(Vec<i64>),
    /// A preset: the World's layers replaced and its own fields set (the level is the World).
    Layers(g::T, Vec<(String, Value)>),
    /// The render camera: the root's `:camera`, else `:active`; the document's alone when the
    /// text has neither camera.
    Camera(Option<i64>),
    /// The render settings: the root's call; the first edit writes a root over the scene's result.
    Root(root::Parameters),
    /// The settings graph, else the workspace's own settings.
    Settings(settings::T),
    /// Objects under a parent (none: the scene root): `:parent` and the transform that keeps each
    /// where it is in the world.
    Reparent(Vec<i64>, Option<i64>),
    /// Each object out to its parent's parent, likewise.
    Outdent(Vec<i64>),
}

fn stop<T>(message: impl Into<String>) -> R<T> {
    Err(message.into())
}

fn mk(node: Draft) -> S {
    let mut builder = SyntaxBuilder::new();
    let root = push(node, &mut builder);
    builder.freeze().handle(root)
}
fn push(node: Draft, builder: &mut SyntaxBuilder) -> SyntaxId {
    builder.push_draft(0, node, rays_diagnostic::Span { start: 0, finish: 0 }, None)
}
fn sym(name: &str) -> S { mk(Draft::Sym(name.to_owned())) }
fn string(text: &str) -> S { mk(Draft::Str(text.as_bytes().to_vec())) }
fn sym_id(name: &str, builder: &mut SyntaxBuilder) -> SyntaxId { push(Draft::Sym(name.to_owned()), builder) }
fn kw_id(name: &str, builder: &mut SyntaxBuilder) -> SyntaxId { push(Draft::Kw(name.to_owned()), builder) }
fn string_id(text: &str, builder: &mut SyntaxBuilder) -> SyntaxId { push(Draft::Str(text.as_bytes().to_vec()), builder) }
fn list(items: Vec<SyntaxId>, builder: &mut SyntaxBuilder) -> SyntaxId { push(Draft::List(items), builder) }
fn diag(d: &rays_diagnostic::Diagnostic) -> String {
    rays_diagnostic::to_string(d)
}

// ---- values as text ----

fn scalar_draft(value: &Value) -> Draft {
    match value {
        Value::Float(x) => Draft::Num(lisp::float(*x)),
        Value::Int(i) => Draft::Num(i.to_string()),
        Value::Bool(b) => Draft::Sym(if *b { "true" } else { "false" }.to_owned()),
        Value::Text(bytes) => Draft::Str(bytes.to_vec()),
        Value::Choice(text) => Draft::Str(text.as_bytes().to_vec()),
    }
}
fn scalar(value: &Value) -> S { mk(scalar_draft(value)) }

fn value_row(fields: &[FieldView], builder: &mut SyntaxBuilder) -> SyntaxId {
    match fields {
        [field] => push(scalar_draft(&field.current), builder),
        fields => push(Draft::Vec(fields.iter().map(|field| push(scalar_draft(&field.current), builder)).collect()), builder),
    }
}

/// The text of a parameter's current value: a number, flag or text, or a vector of numbers for the
/// three fields of a vec3.
pub fn value_syntax(fields: &[FieldView]) -> Syntax {
    let mut builder = SyntaxBuilder::new();
    let root = value_row(fields, &mut builder);
    builder.freeze().handle(root)
}

fn current(fields: &[FieldView], name: &str) -> Option<Value> {
    fields.iter().find_map(|f| if f.name == name { Some(f.current.clone()) } else { None })
}

/// The keywords a node's fields say, with `keep` deciding which fields are written.
fn ports(node: &g::Node) -> Vec<port::Parameter> {
    port::parameters(true, &g::fields(node)).unwrap_or_default()
}

fn differing(before: &g::Node, node: &g::Node) -> Edits {
    let before = g::fields(before);
    ports(node)
        .into_iter()
        .filter_map(|p| {
            if p.fields.iter().any(|f| current(&before, &f.name).as_ref() != Some(&f.current)) {
                Some((p.path.clone(), Some(value_syntax(&p.fields))))
            } else {
                None
            }
        })
        .collect()
}

/// A node's argument as text.
fn current_syntax(node: &g::Node, key: &str) -> Option<S> {
    ports(node).into_iter().find_map(|p| if p.path == key { Some(value_syntax(&p.fields)) } else { None })
}

fn non_default(node: &g::Node, builder: &mut SyntaxBuilder) -> Vec<(String, SyntaxId)> {
    ports(node).into_iter()
        .filter_map(|port| if port.fields.iter().any(|field| field.current != field.default) { Some((port.path.clone(), value_row(&port.fields, builder))) } else { None })
        .collect()
}

// ---- applying ops to the text ----

struct State {
    catalog: Arc<Catalog>,
    workspace: workspace_doc::T,
    /// Homes already made bindings.
    unfolded: Vec<(Home, Path)>,
    /// The host's objects are already written.
    adopted: bool,
    /// The workspace is no longer the document's (`st.workspace != doc.workspace.ws`).
    changed: bool,
    /// The source is no longer the document's.
    source_changed: bool,
}

fn state(factories: Option<&[Factory]>, doc: &document::T) -> R<State> {
    let catalog = kinds::shared(factories).map_err(|d| diag(&d))?;
    Ok(State { catalog, workspace: doc.workspace.ws.clone(), unfolded: Vec::new(), adopted: false, changed: false, source_changed: false })
}

fn apply(st: &mut State, op: Op) -> R<()> {
    match workspace_doc::edit(&st.catalog, &st.workspace, &op) {
        Ok(workspace) => {
            st.workspace = workspace;
            st.changed = true;
            st.source_changed = true;
            Ok(())
        }
        Err(d) => stop(d.message),
    }
}

/// The binding of a home; an inline call is unfolded into one first.
fn bind(st: &mut State, home: &Home) -> R<Path> {
    match home {
        Home::BoundAt(path) => Ok(path.clone()),
        Home::Looped => stop("Made by a macro; edit the text."),
        Home::Copy { loop_, rel, .. } => {
            let mut p = bind(st, loop_)?;
            p.extend(rel.iter().cloned());
            Ok(p)
        }
        Home::InlineIn(parent, key) => {
            let template = document::template(home);
            if let Some((_, path)) = st.unfolded.iter().find(|(h, _)| *h == template) {
                return Ok(path.clone());
            }
            let node = bind(st, parent)?;
            apply(st, Op::Unfold { node: node.clone(), key: key.clone(), sub: Vec::new() })?;
            match f::arg_text(&st.workspace.source, &node, key) {
                Some(form) if matches!(form.node, Node::Sym(_)) && !node.is_empty() => {
                    let mut path = node[..node.len() - 1].to_vec();
                    let Node::Sym(name) = form.node else { unreachable!() };
                    path.push(form.name(name).into());
                    st.unfolded.push((template, path.clone()));
                    Ok(path)
                }
                _ => stop("The call could not be named."),
            }
        }
    }
}

/// An edit of a copy is an edit of the loop's template. A literal argument takes it (every copy
/// changes); so does a component of a vector whose other components the loop computes. An argument
/// or component the loop computes from its variable has no value of its own: writing a number
/// would flatten every copy, so the edit is refused and says where the expression is. `before`
/// gives the copy's value before the edit, to find the components that changed.
fn literal(e: &S) -> bool {
    match &e.node {
        Node::Num(_) | Node::Str(_) => true,
        Node::Sym(n) => e.name(*n) == "true" || e.name(*n) == "false",
        Node::Vec(range) => e.range(*range).iter().all(|id| matches!(e.arena().row(*id).node, Node::Num(_))),
        _ => false,
    }
}

fn set(st: &mut State, home: &Home, edits: &[(String, Option<S>)], before: Option<&dyn Fn(&str) -> Option<S>>) -> R<()> {
    if edits.is_empty() {
        return Ok(());
    }
    let node = bind(st, home)?;
    let looped = document::loop_of(home).is_some();
    for (key, value) in edits {
        let write = |st: &mut State, sub: Vec<i64>, value: &S| apply(st, Op::SetArg { node: node.clone(), key: ArgKey::Kw(key.clone()), sub, value: value.clone() });
        let written = if looped { f::arg_text(&st.workspace.source, &node, &ArgKey::Kw(key.clone())) } else { None };
        let Some(value) = value else {
            apply(st, Op::Disconnect { node: node.clone(), key: ArgKey::Kw(key.clone()), fallback: None })?;
            continue;
        };
        let computed = |e: &S| -> R<()> { stop(format!("{key} is computed by the loop: type =(expression) in its row (now {}).", lisp::flat(e))) };
        let was = before.and_then(|before| before(key));
        match (&written, value.node, &was) {
            (None, _, _) => write(st, Vec::new(), value)?,
            (Some(form), _, _) if literal(form) => write(st, Vec::new(), value)?,
            (Some(template), Node::Vec(now), Some(was)) if matches!(template.node, Node::Vec(_)) && matches!(was.node, Node::Vec(_)) && template.children().len() == value.range(now).len() && value.range(now).len() == was.children().len() => {
                for (i, id) in value.range(now).iter().enumerate() {
                    let current = value.child(*id);
                    if !arena::equal_node(&current, &was.child(was.children()[i])) {
                        let authored = template.child(template.children()[i]);
                        if literal(&authored) { write(st, vec![i as i64], &current)?; }
                        else { computed(&authored)?; }
                    }
                }
            }
            (Some(form), _, _) => computed(form)?,
        }
    }
    Ok(())
}

// ---- loops: deleting copies ----

/// The iteration tuple of a loop-made home: the running index of each enclosing loop, outermost
/// first (what `Flow.Eval` calls the iteration, and what a `:skip` lists).
fn tuple(home: &Home) -> Vec<i64> {
    match home {
        Home::Copy { loop_, index, .. } => {
            let mut t = tuple(loop_);
            t.push(*index);
            t
        }
        Home::InlineIn(h, _) => tuple(h),
        Home::BoundAt(_) | Home::Looped => Vec::new(),
    }
}

/// The loops a home is a copy of, innermost first, each with the tuple of its iteration.
fn levels(home: &Home) -> Vec<(Home, Vec<i64>)> {
    match home {
        Home::Copy { loop_, .. } => {
            let mut out = vec![(document::template(loop_), tuple(home))];
            out.extend(levels(loop_));
            out
        }
        Home::InlineIn(h, _) => levels(h),
        Home::BoundAt(_) | Home::Looped => Vec::new(),
    }
}

/// The `scene/merge` that holds a copy's object, and the position of the object among its
/// arguments: a call written in it, or a binding of the loop body it names.
fn holder(source: &[S], home: &Home) -> Option<(Home, i64)> {
    match home {
        Home::InlineIn(parent, ArgKey::Pos(p)) => Some(((**parent).clone(), *p)),
        Home::Copy { loop_, rel, index } if rel.len() == 1 => {
            let name = &rel[0];
            let in_merge = |form: &S| -> Option<i64> {
                if s::head(form) != Some("scene/merge") { return None; }
                let args = &form.children()[1..];
                let (mut index, mut positional) = (0, 0);
                while index < args.len() {
                    if index + 1 < args.len() && matches!(form.arena().row(args[index]).node, Node::Kw(_)) { index += 2; }
                    else {
                        if matches!(form.arena().row(args[index]).node, Node::Sym(symbol) if form.name(symbol) == name.as_str()) { return Some(positional); }
                        index += 1;
                        positional += 1;
                    }
                }
                None
            };
            let e = document::syntax_of(source, loop_)?;
            let Node::List(range) = e.node else { return None };
            let body = e.child(*e.range(range).get(1..)?.last()?);
            let children = body.children();
            let (bindings, result) = if s::head(&body) == Some("let*") && children.len() == 3 && matches!(body.arena().row(children[1]).node, Node::Vec(_)) {
                (body.arena().children(children[1]), children[2])
            } else {
                (&[][..], body.row_id())
            };
            let found = bindings.chunks_exact(2).find_map(|pair| {
                let Node::Sym(name) = body.arena().row(pair[0]).node else { return None };
                in_merge(&body.child(pair[1])).map(|position| (body.name(name), position))
            }).or_else(|| in_merge(&body.child(result)).map(|position| ("@result", position)));
            found.map(|(name, position)| (Home::Copy { loop_: loop_.clone(), rel: vec![name.into()], index: *index }, position))
        }
        _ => None,
    }
}

/// The deleted objects of loops, `(id, home)`, are left out of the text by skipping them: the
/// iteration that made all of an iteration's objects (the outermost such one) is listed in the
/// `:skip` of its loop; an object that shares its iteration with others that stay is left out of
/// the `scene/merge` that holds it, at this iteration's tuple and its position. Nothing else
/// changes: the other copies keep their iteration tuples, so their ids, cache keys and provenance
/// stay.
fn delete_loops(st: &mut State, before: &document::T, gone: &[(i64, Home)]) -> R<()> {
    let source = before.workspace.ws.source.clone();
    let deleted = |id: i64| gone.iter().any(|(i, _)| *i == id);
    let made = |key: &Home, t: &Vec<i64>| -> Vec<i64> {
        before.homes.objects.iter().filter(|(_, h)| levels(h).iter().any(|(k, u)| k == key && u == t)).map(|(i, _)| *i).collect()
    };
    let mut skips: Vec<(Path, Vec<Vec<i64>>)> = Vec::new();
    fn add(st: &mut State, skips: &mut Vec<(Path, Vec<Vec<i64>>)>, home: &Home, entry: Vec<i64>) -> R<()> {
        let path = bind(st, home)?;
        let known: Vec<Vec<i64>> = skips.iter().find(|(p, _)| *p == path).map(|(_, k)| k.clone()).unwrap_or_default();
        if !known.contains(&entry) {
            let mut next = known;
            next.push(entry);
            skips.retain(|(p, _)| *p != path);
            skips.insert(0, (path, next));
        }
        Ok(())
    }
    for (_, home) in gone {
        let mut outward = levels(home);
        outward.reverse();
        let found = outward.iter().find(|(key, t)| made(key, t).iter().all(|i| deleted(*i)));
        match found {
            Some((key, t)) => add(st, &mut skips, key, t.clone())?,
            None => match (holder(&source, home), levels(home).first()) {
                (Some((merge, p)), Some((_, t))) => {
                    let mut entry = t.clone();
                    entry.push(p);
                    add(st, &mut skips, &document::template(&merge), entry)?
                }
                _ => return stop(format!("An object made by {} cannot be deleted alone here; edit the text.", document::describe(&source, home))),
            },
        }
    }
    for (path, entries) in skips {
        let old: Vec<Vec<i64>> = match f::arg_text(&st.workspace.source, &path, &ArgKey::Kw("skip".to_owned())) {
            Some(e) => w::skip_tuples(&e).unwrap_or_default().into_iter().map(|t| t.into_iter().map(|n| n as i64).collect()).collect(),
            None => Vec::new(),
        };
        let mut all: Vec<Vec<i64>> = old;
        all.extend(entries);
        all.sort();
        all.dedup();
        apply(st, Op::SetArg { node: path, key: ArgKey::Kw("skip".to_owned()), sub: Vec::new(), value: f::skip_value(&all) })?;
    }
    Ok(())
}

// ---- the scene ----

fn label(graph: &[g::Node], id: i64) -> Option<String> {
    g::find(graph, id).map(|n| n.label)
}

fn object_edits(before: &[g::Node], after: &[g::Node], id: i64, nb: &g::Node, na: &g::Node) -> Edits {
    let parent = |graph: &[g::Node]| objects::parent(graph, id).and_then(|p| label(graph, p));
    let mut out: Edits = Vec::new();
    if nb.label != na.label {
        out.push(("name".to_owned(), Some(string(&na.label))));
    }
    if parent(before) != parent(after) {
        out.push(("parent".to_owned(), parent(after).map(|p| string(&p))));
    }
    out.extend(differing(nb, na));
    out
}

fn homed(doc: &document::T, id: i64) -> bool {
    match doc.homes.objects.iter().find(|(i, _)| *i == id) {
        Some((_, Home::Looped)) => lowering::graph_of(&doc.workspace.ws, context::scene()).is_some(),
        Some(_) => true,
        None => false,
    }
}

fn unique(graph: &[g::Node], id: i64) -> R<()> {
    let found = label(graph, id).and_then(|l| graph.iter().find(|i| i.id != id && i.label == l && i.kind != "world"));
    match found {
        Some(_) => stop(format!("Two objects are named {}. Rename one of them first.", format!("{:?}", &label(graph, id).unwrap_or_default()))),
        None => Ok(()),
    }
}

/// The graphs a deleted object leaves behind, when no other `(ref g)` reads them.
fn refs(name: &str, e: &S) -> usize {
    if let Node::List(range) = e.node {
        let items = e.range(range);
        if items.len() >= 2 {
            match (e.arena().row(items[0]).node, e.arena().row(items[1]).node) {
                (Node::Sym(head), Node::Sym(graph)) if e.name(head) == "ref" && e.name(graph) == name => return 1,
                // a panel shows it
                (Node::Sym(head), Node::Str(graph)) if e.name(head) == "ui/graph" && e.bytes(graph) == name.as_bytes() => return 1,
                _ => {}
            }
        }
    }
    e.children().iter().map(|id| refs(name, &e.child(*id))).sum()
}

/// The graph that makes a scene object: the `(ref g)` its call holds, for a geometry's SOP graph
/// or a World's layer graph.
fn source_graph_of(doc: &document::T, id: i64) -> Option<String> {
    let home = doc.homes.objects.iter().find(|(i, _)| *i == id).map(|(_, h)| h)?;
    let e = document::syntax_of(&doc.workspace.ws.source, home)?;
    if !matches!(s::head(&e), Some("scene/geometry" | "scene/world")) { return None; }
    e.children()[1..].iter().find_map(|id| {
        let Node::List(range) = e.arena().row(*id).node else { return None };
        let [head, name] = e.range(range) else { return None };
        match (e.arena().row(*head).node, e.arena().row(*name).node) {
            (Node::Sym(head), Node::Sym(name)) if e.name(head) == "ref" => Some(e.name(name).to_owned()),
            _ => None,
        }
    })
}

/// An object a viewport over another scene instance draws is derived: its text is that reference.
fn refuse_preview(doc: &document::T, id: i64) -> R<()> {
    let key = doc.shell.as_ref().and_then(|shell| shell.views.iter().find_map(|(key, ids)| if ids.contains(&id) { Some(key.clone()) } else { None }));
    match key {
        Some(key) => stop(format!("Preview [{key}] is derived; edit its viewport scene reference or source graph.")),
        None => Ok(()),
    }
}

/// The deleted objects `(id, home)` leave the text: an inline call leaves its merge (the last one
/// first, so positions hold), then a binding goes whole.
fn delete_objects(st: &mut State, before: &document::T, gone: &[(i64, Home)]) -> R<()> {
    let (looped, rest): (Vec<(i64, Home)>, Vec<(i64, Home)>) = gone.iter().cloned().partition(|(_, h)| document::loop_of(h).is_some());
    delete_loops(st, before, &looped)?;
    let (bound, mut inline): (Vec<Home>, Vec<Home>) = rest.into_iter().map(|(_, h)| h).partition(|h| matches!(h, Home::BoundAt(_)));
    let position = |h: &Home| match h {
        Home::InlineIn(_, ArgKey::Pos(i)) => *i,
        _ => -1,
    };
    inline.sort_by(|x, y| position(y).cmp(&position(x)));
    for home in &inline {
        match home {
            Home::InlineIn(parent, key @ ArgKey::Pos(_)) => {
                let node = bind(st, parent)?;
                apply(st, Op::Disconnect { node, key: key.clone(), fallback: None })?;
            }
            home => return stop(format!("Made by {}; edit the text.", document::describe(&before.workspace.ws.source, home))),
        }
    }
    for home in &bound {
        if let Home::BoundAt(path) = home {
            apply(st, Op::DeleteNodes { nodes: vec![path.clone()] })?;
        }
    }
    // an object's graph goes with it unless another object or panel still references it
    let mut orphans: Vec<String> = gone.iter().filter_map(|(id, _)| source_graph_of(before, *id)).collect();
    orphans.sort();
    orphans.dedup();
    for name in orphans {
        if st.workspace.source.iter().all(|e| refs(&name, e) == 0) && st.workspace.checked.graphs.iter().any(|gr| gr.name == name) {
            apply(st, Op::RemoveGraph { name })?;
        }
    }
    Ok(())
}

/// The render camera in the text: the root's `:camera` when the scene has a root, else `:active`
/// on the camera. `alive` says whether the camera it replaces is still an object.
fn set_camera(st: &mut State, before: &document::T, alive: &dyn Fn(i64) -> bool, camera: Option<i64>) -> R<()> {
    let home = |id: i64| before.homes.objects.iter().find(|(i, _)| *i == id).map(|(_, h)| h.clone());
    let active = ArgKey::Kw("active".to_owned());
    match &before.homes.root {
        Some(root) => {
            // the root names the active camera; an old `:active` on the camera it replaces goes
            if let Some(id) = before.active_camera {
                if alive(id) {
                    if let Some(h) = home(id) {
                        let p = bind(st, &h)?;
                        if f::arg_text(&st.workspace.source, &p, &active).is_some() {
                            apply(st, Op::Disconnect { node: p, key: active, fallback: None })?;
                        }
                    }
                }
            }
            let target = match camera.and_then(home) {
                Some(h) => Some(bind(st, &h)?),
                None => None,
            };
            match target {
                Some(p) => set(st, root, &[("camera".to_owned(), Some(sym(&p[p.len() - 1])))], None),
                None => set(st, root, &[("camera".to_owned(), None)], None),
            }
        }
        None => {
            if let Some(h) = before.active_camera.and_then(home) {
                if alive(before.active_camera.unwrap()) {
                    set(st, &h, &[("active".to_owned(), None)], None)?;
                }
            }
            if let Some(h) = camera.and_then(home) {
                set(st, &h, &[("active".to_owned(), Some(sym("true")))], None)?;
            }
            Ok(())
        }
    }
}

/// The objects of the text as the scene `a` has them: a changed one's name, parent and fields are
/// written.
fn object_changes(st: &mut State, before: &document::T, a: &[g::Node]) -> R<()> {
    let b = document::scene_graph(before);
    for (id, home) in &before.homes.objects {
        if !homed(before, *id) {
            continue;
        }
        if let (Some(nb), Some(na)) = (g::find(&b, *id), g::find(a, *id)) {
            let edits = object_edits(&b, a, *id, &nb, &na);
            if !edits.is_empty() {
                refuse_preview(before, *id)?;
            }
            if edits.iter().any(|(k, _)| k == "parent") {
                if let Some(p) = objects::parent(a, *id) {
                    unique(a, p)?;
                }
            }
            // a name is what `:parent` reads: one two objects share would move the other's children
            if edits.iter().any(|(k, _)| k == "name") {
                unique(a, *id)?;
            }
            let cs = |key: &str| current_syntax(&nb, key);
            set(st, home, &edits, Some(&cs))?;
        }
    }
    Ok(())
}

// ---- the World ----

fn world_id(doc: &document::T) -> Option<i64> {
    objects::ids("world", &document::scene_graph(doc)).first().copied()
}

/// Layer ids from the top down.
fn order(layers: &[g::Node]) -> Vec<i64> {
    g::stack(layers).into_iter().map(|n| n.id).collect()
}

// ---- objects the text does not have ----

/// The names a workspace's graphs, functions and macros already own.
fn owned(workspace: &workspace_doc::T) -> Vec<String> {
    let c = &workspace.checked;
    let mut out: Vec<String> = c.graphs.iter().chain(c.defs.iter()).map(|gr| gr.name.clone()).collect();
    for m in c.macros.iter() {
        if let Node::List(range) = m.node {
            let items = m.range(range);
            if items.len() >= 2 {
                if let Node::Sym(name) = m.arena().row(items[1]).node {
                    if items.len() == 4 || matches!(m.arena().row(items[0]).node, Node::Sym(_)) {
                        out.push(m.name(name).to_owned());
                    }
                }
            }
        }
    }
    out
}

/// `(kind slots... :name "label" :field value ...)` for a derived node; only what differs from the
/// kind's defaults is written.
fn call_of(kind: &str, label: &str, default_label: &str, slots: Vec<SyntaxId>, extra: Vec<SyntaxId>, node: &g::Node, builder: &mut SyntaxBuilder) -> SyntaxId {
    let mut items = vec![sym_id(kind, builder)];
    items.extend(slots);
    if label != default_label {
        items.push(kw_id("name", builder));
        items.push(string_id(label, builder));
    }
    items.extend(extra);
    for (key, value) in non_default(node, builder) {
        items.push(kw_id(&key, builder));
        items.push(value);
    }
    list(items, builder)
}

fn graph_form(name: &str, context: &str, body: SyntaxId, builder: &mut SyntaxBuilder) -> SyntaxId {
    list(vec![sym_id("graph", builder), sym_id(name, builder), kw_id("context", builder), sym_id(context, builder), body], builder)
}

fn let_star(bindings: &[(String, SyntaxId)], result: SyntaxId, builder: &mut SyntaxBuilder) -> SyntaxId {
    let mut items = Vec::new();
    for (name, expression) in bindings {
        items.push(sym_id(name, builder));
        items.push(*expression);
    }
    list(vec![sym_id("let*", builder), push(Draft::Vec(items), builder), result], builder)
}

fn default_label(node: &g::Node) -> String {
    node.kind.to_ascii_lowercase()
}

fn object_binding(doc: &document::T, used: &mut Vec<String>, id: i64, info: &g::Node, builder: &mut SyntaxBuilder) -> R<(String, SyntaxId)> {
    let graph = document::scene_graph(doc);
    let parent = objects::parent(&graph, id);
    let mut default = default_label(info);
    let slots = if info.kind == "geometry" {
        let Some(name) = document::object_graph(doc, id) else { return stop(format!("{} has no graph of its own to place.", info.label)); };
        default = name.clone();
        vec![list(vec![sym_id("ref", builder), sym_id(&name, builder)], builder)]
    } else { Vec::new() };
    let mut extra = match parent.and_then(|id| label(&graph, id)) {
        Some(parent) => vec![kw_id("parent", builder), string_id(&parent, builder)],
        None => Vec::new(),
    };
    if doc.active_camera == Some(id) && info.kind == "camera" { extra.extend([kw_id("active", builder), sym_id("true", builder)]); }
    Ok((f::fresh_among(used, &info.label), call_of(&format!("scene/{}", info.kind), &info.label, &default, slots, extra, info, builder)))
}

/// The text of every object no binding holds, into the scene graph (made when there is none, even
/// with nothing to write: the graph is what says "no camera, no lights"). The scene graph is
/// authoritative, so all of the host's objects are written together.
fn adopt_objects(st: &mut State, doc: &document::T) -> R<()> {
    if st.adopted {
        return Ok(());
    }
    st.adopted = true;
    let graph = document::scene_graph(doc);
    let unhomed: Vec<g::Node> = graph.iter().filter(|i| i.kind != "world" && !homed(doc, i.id)).cloned().collect();
    if unhomed.is_empty() && lowering::graph_of(&st.workspace, context::scene()).is_none() {
        let mut builder = SyntaxBuilder::new();
        let root = graph_form("scene", "scene", list(vec![sym_id("scene/merge", &mut builder)], &mut builder), &mut builder);
        apply(st, Op::SetGraph { name: "scene".to_owned(), form: builder.freeze().handle(root) })?;
    }
    if !unhomed.is_empty() {
        let mut used = owned(&st.workspace);
        let mut builder = SyntaxBuilder::new();
        let mut bindings = Vec::new();
        for i in &unhomed {
            bindings.push(object_binding(doc, &mut used, i.id, i, &mut builder)?);
        }
        match lowering::graph_of(&st.workspace, context::scene()) {
            None => {
                let mut items = vec![sym_id("scene/merge", &mut builder)];
                items.extend(bindings.iter().map(|(name, _)| sym_id(name, &mut builder)));
                let body = let_star(&bindings, list(items, &mut builder), &mut builder);
                let root = graph_form("scene", "scene", body, &mut builder);
                apply(st, Op::SetGraph { name: "scene".to_owned(), form: builder.freeze().handle(root) })?;
            }
            Some(gr) => {
                // into a graph the text has: each name is free of what the graph binds by then
                let arena = builder.freeze();
                for ((_, expr), i) in bindings.into_iter().zip(unhomed.iter()) {
                    let name = f::fresh_name(&st.workspace.source, &gr.name, &i.label);
                    apply(st, Op::AddNode { scope: vec![gr.name.clone().into()], expr: arena.handle(expr), name })?;
                }
            }
        }
    }
    Ok(())
}


/// The layers of a World as a world graph named `name`: bound bottom first, the top layer the
/// result; `(world/none)` without layers.
fn write_stack(st: &mut State, layers_graph: &[g::Node], bottom_first: &[i64], name: &str) -> R<()> {
    let mut used = owned(&st.workspace);
    let mut bindings: Vec<(i64, String, g::Node)> = Vec::new();
    for id in bottom_first {
        let info = match layers_graph.iter().find(|i| i.id == *id) {
            Some(i) => i.clone(),
            None => return stop("That object is gone."),
        };
        let n = f::fresh_among(&mut used, &info.label);
        bindings.push((*id, n, info));
    }
    let world_kinds = kinds::world_kinds();
    let mut builder = SyntaxBuilder::new();
    let mut layers: Vec<(String, SyntaxId)> = Vec::new();
    for (i, (_, n, info)) in bindings.iter().enumerate() {
        let below = if i == 0 { Vec::new() } else { vec![sym_id(&bindings[i - 1].1, &mut builder)] };
        let qualified = format!("world/{}", info.kind);
        let dl = match world_kinds.iter().find(|(k, _)| *k == qualified) {
            Some((_, k)) => k.label.clone(),
            None => return stop(format!("Unknown World layer kind {}.", info.kind)),
        };
        layers.push((n.clone(), call_of(&qualified, &info.label, &dl, below, Vec::new(), info, &mut builder)));
    }
    let body = match layers.last() {
        None => list(vec![sym_id("world/none", &mut builder)], &mut builder),
        Some((top, _)) => let_star(&layers, sym_id(top, &mut builder), &mut builder),
    };
    let root = graph_form(name, "world", body, &mut builder);
    apply(st, Op::SetGraph { name: name.to_owned(), form: builder.freeze().handle(root) })
}

/// The layers of the World `wid` the text has, with `removed` gone and the others stacked as
/// `stack` says (top down): a deleted or moved layer is one edit of its binding, so the graph's
/// inputs, expressions, comments and names stay. `graph` has the layers as they are to be written
/// when the stack has to be written whole.
fn stack_layers(st: &mut State, before: &document::T, wid: i64, graph: &[g::Node], removed: &[i64], stack: &[i64]) -> R<()> {
    let member = before.homes.objects.iter().any(|(i, _)| *i == wid);
    let known: Vec<i64> = before.homes.layers.iter().map(|(i, _)| *i).collect();
    let current_layers = match before.layers.get(&wid) {
        Some(l) => l,
        None => return stop("That object is gone."),
    };
    let was: Vec<i64> = order(current_layers).into_iter().filter(|id| stack.contains(id)).collect();
    let reordered = was != stack;
    // a World member whose top layer is its graph's result written in place has no name to delete
    // or rewire by
    let unnamed = member
        && before.homes.layers.iter().any(|(_, home)| match home {
            Home::BoundAt(path) => path.last().map(|x| x.as_str()) == Some("@result"),
            _ => false,
        });
    if (!removed.is_empty() || reordered) && unnamed {
        match &before.homes.world_graph {
            Some(name) => {
                let mut bottom: Vec<i64> = stack.to_vec();
                bottom.reverse();
                write_stack(st, graph, &bottom, name)
            }
            None => stop("The World's graph is not named; edit the text."),
        }
    } else if !removed.is_empty() || reordered {
        // every layer is a binding of its own before the stack is rewired
        let mut paths: Vec<(i64, Path)> = Vec::new();
        for id in &known {
            let home = before.homes.layers.iter().find(|(i, _)| i == id).map(|(_, h)| h.clone()).unwrap();
            paths.push((*id, bind(st, &home)?));
        }
        let path_of = |id: i64| -> R<Path> { paths.iter().find(|(i, _)| *i == id).map(|(_, p)| p.clone()).ok_or_else(|| "That object is gone.".to_owned()) };
        for id in removed {
            apply(st, Op::DeleteNodes { nodes: vec![path_of(*id)?] })?;
        }
        if reordered {
            let mut bottom_first: Vec<i64> = stack.to_vec();
            bottom_first.reverse();
            let name = |id: i64| -> R<String> { Ok(path_of(id)?.last().map_or_else(String::new, ToString::to_string)) };
            let mut below: Option<i64> = None;
            for id in &bottom_first {
                match below {
                    Some(b) => apply(st, Op::Connect { node: path_of(*id)?, key: ArgKey::Pos(0), src: name(b)?, iter: false })?,
                    None => apply(st, Op::Disconnect { node: path_of(*id)?, key: ArgKey::Pos(0), fallback: None })?,
                }
                below = Some(*id);
            }
            let top = bottom_first.last().copied();
            match (top, &before.homes.world) {
                (Some(top), _) if member => match &before.homes.world_graph {
                    // a world graph's result is its top layer
                    Some(gname) => apply(st, Op::Connect { node: vec![gname.clone().into(), "@result".into()], key: ArgKey::Whole, src: name(top)?, iter: false })?,
                    None => return stop("The World's graph is not named; edit the text."),
                },
                (Some(top), Some(home)) => {
                    let node = bind(st, home)?;
                    apply(st, Op::Connect { node, key: ArgKey::Pos(0), src: name(top)?, iter: false })?
                }
                _ => {}
            }
        }
        Ok(())
    } else {
        Ok(())
    }
}

// ---- the root ----

/// The render settings that changed, as keywords.
fn root_edits(before: &root::Parameters, after: &root::Parameters) -> Edits {
    let was = root::fields(before);
    root::fields(after).into_iter().filter_map(|f| if current(&was, &f.name).as_ref() != Some(&f.current) { Some((f.name.clone(), Some(scalar(&f.current)))) } else { None }).collect()
}

/// A scene with no `scene/root` writes one over its result on the first edit of a render setting
/// (only what differs from the defaults), the scene graph made first when there is none.
fn adopt_root(st: &mut State, doc: &document::T, root: &root::Parameters) -> R<()> {
    if lowering::graph_of(&st.workspace, context::scene()).is_none() {
        adopt_objects(st, doc)?;
    }
    let scene = match lowering::graph_of(&st.workspace, context::scene()) {
        Some(gr) => gr.name,
        None => return stop("option is None"),
    };
    let mut builder = SyntaxBuilder::new();
    let defaults = root::fields(&root::default());
    let mut items = vec![sym_id("scene/root", &mut builder)];
    for field in root::fields(root) {
        if current(&defaults, &field.name).as_ref() != Some(&field.current) {
            items.push(kw_id(&field.name, &mut builder));
            items.push(push(scalar_draft(&field.current), &mut builder));
        }
    }
    let expression = list(items, &mut builder);
    let name = f::fresh_name(&st.workspace.source, &scene, "root");
    apply(st, Op::AddNode { scope: vec![scene.into()], name, expr: builder.freeze().handle(expression) })
}

fn root_write(st: &mut State, before: &document::T, now: &root::Parameters) -> R<()> {
    if before.root != *now {
        match &before.homes.root {
            Some(home) => set(st, home, &root_edits(&before.root, now), None),
            None => adopt_root(st, before, now),
        }
    } else {
        Ok(())
    }
}

// ---- settings ----

fn settings_write(st: &mut State, before: &document::T, now: &settings::T) -> R<()> {
    let differs = !settings::same_values(&before.settings, now);
    match &before.homes.settings {
        Some(home) if differs => {
            let was = settings::fields(&before.settings);
            let changed: Edits = settings::fields(now)
                .into_iter()
                .filter_map(|f| if current(&was, &f.name).as_ref() != Some(&f.current) { Some((f.name.clone(), Some(scalar(&f.current)))) } else { None })
                .collect();
            set(st, home, &changed, None)
        }
        None if differs => {
            st.workspace.settings = now.clone();
            st.changed = true;
            Ok(())
        }
        _ => Ok(()),
    }
}

fn unhomed_changes(before: &document::T, after: &document::T) -> bool {
    let b = document::scene_graph(before);
    let a = document::scene_graph(after);
    a.iter().any(|i| {
        i.kind != "world"
            && !homed(before, i.id)
            && match g::find(&b, i.id) {
                Some(nb) => !object_edits(&b, &a, i.id, &nb, i).is_empty() || objects::parent(&b, i.id) != objects::parent(&a, i.id),
                None => false,
            }
    }) || b.iter().any(|i| i.kind != "world" && !homed(before, i.id) && g::find(&a, i.id).is_none())
}

// ---- one object's fields, text first ----

/// Where the text of a scene object (at the scene level) or of a World layer (inside the World)
/// is, when it has one an edit can be written to; none for an object only the host made.
fn field_home(doc: &document::T, level: Level, node: i64) -> Option<Home> {
    match level {
        Level::Scene => match doc.homes.objects.iter().find(|(i, _)| *i == node) {
            Some((_, home)) if homed(doc, node) => Some(home.clone()),
            Some(_) => None,
            None => {
                if world_id(doc) == Some(node) {
                    doc.homes.world.clone()
                } else {
                    None
                }
            }
        },
        Level::Inside(id) if world_id(doc) == Some(id) => doc.homes.layers.iter().find(|(i, _)| *i == node).map(|(_, h)| h.clone()),
        Level::Inside(_) => None,
    }
}

/// The scene object (at the scene level) or World layer (inside the World) with this id has text
/// of its own that an edit can be written to.
pub fn in_text(doc: &document::T, level: Level, id: i64) -> bool {
    field_home(doc, level, id).is_some()
}

// ---- derived edits, text first ----

/// The status line for an edit written to a loop's template: every copy changes.
pub fn template_note(doc: &document::T, home: &Home) -> Option<String> {
    document::loop_of(home).map(|loop_| format!("Edited the loop template ({}); {} copies change.", document::describe(&doc.workspace.ws.source, &loop_), document::copies(doc, home)))
}

fn first(notes: Vec<Option<String>>) -> Option<String> {
    notes.into_iter().find_map(|n| n)
}

/// Every object the edit names has text of its own, so `write` takes it.
pub fn writes(doc: &document::T, level: Level, edit: &Edit) -> bool {
    match edit {
        Edit::Fields(edits) => edits.iter().all(|(node, _)| in_text(doc, level, *node)),
        Edit::Rename(node, _) => in_text(doc, level, *node),
        Edit::Delete(ids) | Edit::Restack(ids) => ids.iter().all(|id| in_text(doc, level, *id)),
        // a camera the text does not have is the document's alone
        Edit::Camera(_) => true,
        // the first edit of a render setting writes the root
        Edit::Root(_) | Edit::Settings(_) => true,
        Edit::Reparent(ids, _) | Edit::Outdent(ids) => level == Level::Scene && ids.iter().all(|id| in_text(doc, level, *id)),
        Edit::Layers(..) => match level {
            Level::Inside(wid) => world_id(doc) == Some(wid) && doc.homes.world.is_some(),
            Level::Scene => false,
        },
    }
}

fn node_at(doc: &document::T, level: Level, id: i64) -> R<g::Node> {
    match document::kinds(doc, level).and_then(|gr| g::find(&gr, id)) {
        Some(node) => Ok(node),
        None => stop("That object is gone."),
    }
}

fn write_edit(st: &mut State, doc: &document::T, level: Level, edit: &Edit) -> R<Option<String>> {
    match edit {
        Edit::Fields(edits) => {
            let mut notes = Vec::new();
            for (node, values) in edits {
                let was = node_at(doc, level, *node)?;
                let home = field_home(doc, level, *node);
                match (home, g::apply(&was, values)) {
                    (_, Err(message)) => return stop(message),
                    (None, _) => return stop("That object is not in the text."),
                    (Some(home), Ok((now, _))) => {
                        if level == Level::Scene {
                            refuse_preview(doc, *node)?;
                        }
                        let cs = |key: &str| current_syntax(&was, key);
                        set(st, &home, &differing(&was, &now), Some(&cs))?;
                        notes.push(template_note(doc, &home));
                    }
                }
            }
            Ok(first(notes))
        }
        Edit::Rename(id, name) if node_at(doc, level, *id)?.label == *name => Ok(None),
        Edit::Rename(id, name) => {
            let value = Some(string(name));
            if level == Level::Scene && doc.homes.objects.iter().any(|(i, _)| i == id) {
                // a name is what `:parent` reads: the children's follow, and one two objects
                // share would move the other's children
                let graph = document::scene_graph(doc);
                if graph.iter().any(|i| i.id != *id && i.label == *name && i.kind != "world") {
                    return stop(format!("Two objects are named {}. Rename one of them first.", format!("{:?}", name)));
                }
                for (o, home) in &doc.homes.objects {
                    let key = if o == id {
                        Some("name")
                    } else if objects::parent(&graph, *o) == Some(*id) {
                        Some("parent")
                    } else {
                        None
                    };
                    if let Some(key) = key {
                        if homed(doc, *o) {
                            refuse_preview(doc, *o)?;
                            set(st, home, &[(key.to_owned(), value.clone())], None)?;
                        }
                    }
                }
            } else if let Some(home) = field_home(doc, level, *id) {
                set(st, &home, &[("name".to_owned(), value)], None)?;
            }
            Ok(field_home(doc, level, *id).and_then(|h| template_note(doc, &h)))
        }
        Edit::Delete(ids) if level == Level::Scene => {
            let graph = document::scene_graph(doc);
            let mut gone: Vec<(i64, Home)> = doc.homes.objects.iter().filter(|(id, _)| ids.contains(id) && homed(doc, *id)).cloned().collect();
            gone.reverse();
            for (o, home) in &doc.homes.objects {
                if gone.iter().any(|(i, _)| i == o) {
                    refuse_preview(doc, *o)?;
                } else if homed(doc, *o) && objects::parent(&graph, *o).map_or(false, |p| ids.contains(&p)) {
                    refuse_preview(doc, *o)?;
                    set(st, home, &[("parent".to_owned(), None)], None)?;
                }
            }
            delete_objects(st, doc, &gone)?;
            if let (Some(root), Some(camera)) = (&doc.homes.root, doc.active_camera) {
                if ids.contains(&camera) {
                    set(st, root, &[("camera".to_owned(), None)], None)?;
                }
            }
            Ok(gone.iter().rev().find_map(|(_, home)| {
                document::loop_of(home).map(|loop_| format!("Removed a copy from the loop ({}).", document::describe(&doc.workspace.ws.source, &loop_)))
            }))
        }
        Edit::Layers(layers, values) => {
            if let Level::Inside(wid) = level {
                let was = node_at(doc, Level::Scene, wid)?;
                let now = match g::apply(&was, values) {
                    Ok((now, _)) => now,
                    Err(message) => return stop(message),
                };
                let edits = differing(&was, &now);
                if !edits.is_empty() {
                    refuse_preview(doc, wid)?;
                }
                if let Some(home) = field_home(doc, Level::Scene, wid) {
                    let cs = |key: &str| current_syntax(&was, key);
                    set(st, &home, &edits, Some(&cs))?;
                }
                match &doc.homes.world_graph {
                    Some(name) => {
                        let mut bottom = order(layers);
                        bottom.reverse();
                        write_stack(st, layers, &bottom, name)?;
                    }
                    None => return stop("The World's graph is not named; edit the text."),
                }
            }
            Ok(None)
        }
        Edit::Camera(camera) => {
            if *camera != doc.active_camera {
                set_camera(st, doc, &|_| true, *camera)?;
            }
            Ok(None)
        }
        Edit::Reparent(ids, _) | Edit::Outdent(ids) => {
            // the scene rewired aside, for the transform that keeps each object where it is: only
            // the text is written
            let graph = document::scene_graph(doc);
            let moved = match edit {
                Edit::Reparent(ids, parent) => objects::reparent(&graph, ids, *parent),
                _ => objects::outdent(&graph, ids),
            };
            object_changes(st, doc, &moved)?;
            Ok(first(ids.iter().map(|id| doc.homes.objects.iter().find(|(i, _)| i == id).and_then(|(_, h)| template_note(doc, h))).collect()))
        }
        Edit::Root(now) => {
            root_write(st, doc, now)?;
            Ok(None)
        }
        Edit::Settings(now) => {
            settings_write(st, doc, now)?;
            Ok(None)
        }
        Edit::Restack(bottom_first) => {
            if let (Level::Inside(wid), Some(w)) = (level, world_id(doc)) {
                if w == wid {
                    let layers = match doc.layers.get(&wid) {
                        Some(l) => l.clone(),
                        None => return stop("That object is gone."),
                    };
                    let mut stack = bottom_first.clone();
                    stack.reverse();
                    stack_layers(st, doc, wid, &layers, &[], &stack)?;
                }
            }
            Ok(None)
        }
        Edit::Delete(ids) => {
            if let (Level::Inside(wid), Some(w)) = (level, world_id(doc)) {
                if w == wid {
                    let layers = match doc.layers.get(&wid) {
                        Some(l) => l.clone(),
                        None => return stop("That object is gone."),
                    };
                    let removed: Vec<i64> = doc.homes.layers.iter().map(|(i, _)| *i).filter(|id| ids.contains(id)).collect();
                    let stack: Vec<i64> = order(&layers).into_iter().filter(|id| !ids.contains(id)).collect();
                    stack_layers(st, doc, wid, &layers, &removed, &stack)?;
                }
            }
            Ok(None)
        }
    }
}

fn lower(factories: Option<&[Factory]>, previous: &document::T, workspace: &workspace_doc::T) -> R<document::T> {
    let options = Options { factories: factories.map(|f| f.to_vec()), previous: Some(previous.clone()), ..Options::default() };
    lowering::of_workspace(&options, workspace).map_err(|d| diag(&d))
}

/// The edit written to the text and lowered again, the derived document never edited in between,
/// with what it did to a loop's copies for the status line; the same text as `reconcile` writes
/// for the same edit of the derived document. `None` when not `writes`: the caller edits the
/// derived document and `reconcile` adopts it.
pub fn write(factories: Option<&[Factory]>, doc: &document::T, level: Level, edit: &Edit) -> R<Option<(document::T, Option<String>)>> {
    if !writes(doc, level, edit) {
        return Ok(None);
    }
    let mut st = state(factories, doc)?;
    let note = write_edit(&mut st, doc, level, edit)?;
    // what the text does not say is the document's; what it says is lowered over this
    let doc = match edit {
        Edit::Camera(active_camera) => document::T { active_camera: *active_camera, ..doc.clone() },
        Edit::Root(root) => document::T { root: root.clone(), ..doc.clone() },
        Edit::Settings(settings) => document::T { settings: settings.clone(), ..doc.clone() },
        Edit::Delete(ids) => {
            // the lowering finds an object of the text by its home, else by kind and label: the
            // deleted ones are not there to be taken for the copies of a loop that stay
            match document::kinds(doc, level) {
                Some(graph) => document::prune(&document::with_kinds(doc, level, g::remove(ids, &graph))),
                None => doc.clone(),
            }
        }
        Edit::Fields(_) | Edit::Rename(..) | Edit::Restack(_) | Edit::Layers(..) | Edit::Reparent(..) | Edit::Outdent(_) => doc.clone(),
    };
    if !st.changed {
        Ok(Some((doc, note)))
    } else if !st.source_changed {
        let mut doc = doc;
        doc.workspace.ws = st.workspace;
        Ok(Some((doc, note)))
    } else {
        Ok(Some((lower(factories, &doc, &st.workspace)?, note)))
    }
}

/// Field values of one object (an inspector row, a viewport handle) written to its text and
/// lowered again: the node only says what the values are once its ranges have had their say, and
/// which effects they have. `None`: the object has no text (the caller edits the derived object and
/// `reconcile` adopts it).
pub fn set_fields(factories: Option<&[Factory]>, doc: &document::T, level: Level, node: i64, values: &[(String, Value)]) -> R<Option<(document::T, Effects, Home)>> {
    let home = field_home(doc, level, node);
    let was = document::kinds(doc, level).and_then(|gr| g::find(&gr, node));
    match (home, was) {
        (Some(home), Some(was)) => {
            let (_, effects) = g::apply(&was, values)?;
            let written = write(factories, doc, level, &Edit::Fields(vec![(node, values.to_vec())]))?;
            Ok(written.map(|(doc, _)| (doc, effects, home)))
        }
        _ => Ok(None),
    }
}

fn networks_same(a: &document::T, b: &document::T) -> bool {
    a.networks.len() == b.networks.len()
        && a.networks.iter().all(|(id, na)| b.networks.get(id).map_or(false, |nb| na.context == nb.context && na.displayed == nb.displayed && flow_sop_catalog::network::ptr_eq(&na.graph, &nb.graph)))
}

fn layers_same(a: &document::T, b: &document::T) -> bool {
    a.layers.len() == b.layers.len() && a.layers.iter().all(|(id, la)| b.layers.get(id).map_or(false, |lb| g::same(la, lb)))
}

/// `after` is `before` with objects the host made edited (or a camera moved that follows the
/// viewport); the result is `after` with the edit written to its text and lowered again. `adopt`:
/// an explicit edit of an object the text lacks writes it (a follow-the-viewport camera move is not
/// one, so it stays the host's).
fn run(factories: Option<&[Factory]>, adopt: bool, before: &document::T, after: &document::T) -> R<document::T> {
    if g::same(&before.scene, &after.scene) && networks_same(before, after) && layers_same(before, after) {
        return Ok(after.clone());
    }
    let mut st = state(factories, before)?;
    object_changes(&mut st, before, &document::scene_graph(after))?;
    if adopt && unhomed_changes(before, after) {
        adopt_objects(&mut st, after)?;
    }
    if !st.changed {
        Ok(after.clone())
    } else {
        lower(factories, after, &st.workspace)
    }
}

/// `reconcile ~factories before after`: `after` is `before` with objects only the host made
/// edited, added or deleted (its camera and lights), or with the fields of an object of the text
/// changed by the host (a camera following the viewport). An object only the host made is written
/// to a scene graph by its first explicit edit, and the result is `after` with the new text
/// lowered again (objects keep their ids); `adopt = Some(false)` (the camera follow) leaves the
/// host's objects to the host. A changed field, name or parent of an object of the text is written
/// as `write` writes it; an edit the text cannot take is an `Err` and changes nothing. Everything
/// else asked of an object of the text (deleting it, the render camera, the World and its layers,
/// the root, the settings) is `write`'s and is not looked for here. `adopt`: None is true.
pub fn reconcile(factories: Option<&[Factory]>, adopt: Option<bool>, before: &document::T, after: &document::T) -> R<document::T> {
    run(factories, adopt.unwrap_or(true), before, after)
}

/// The scene graph of a document that has none, written from the objects the host made (an empty
/// one when there are none).
pub fn adopt(factories: Option<&[Factory]>, doc: &document::T) -> R<document::T> {
    let mut st = state(factories, doc)?;
    adopt_objects(&mut st, doc)?;
    lower(factories, doc, &st.workspace)
}

/// The binding that holds a home, made by unfolding what is written in place (several rewrites,
/// one new document); an error when a loop or an expression made it.
pub fn bind_home(factories: Option<&[Factory]>, doc: &document::T, home: &Home) -> R<(document::T, Path)> {
    let mut st = state(factories, doc)?;
    let path = bind(&mut st, home)?;
    if !st.changed {
        Ok((doc.clone(), path))
    } else {
        Ok((lower(factories, doc, &st.workspace)?, path))
    }
}

// ---- composition: gestures on the scene's merge ----

fn scene_name(workspace: &workspace_doc::T) -> String {
    match lowering::graph_of(workspace, context::scene()) {
        Some(gr) => gr.name,
        None => "scene".to_owned(),
    }
}

/// The ops of adding geometry to the scene's merge, one gesture: a new SOP graph, a
/// `scene/geometry` binding of it and the merge input; with `existing` the object of an existing
/// SOP graph only (two objects share it).
pub fn add_geometry(doc: &document::T, existing: Option<&str>) -> Vec<Op> {
    let workspace = &doc.workspace.ws;
    let mut used = owned(workspace);
    let mut builder = SyntaxBuilder::new();
    let (new_graph, graph) = match existing {
        Some(graph) => (None, graph.to_owned()),
        None => {
            let graph = f::fresh_among(&mut used, "shape");
            let form = graph_form(&graph, "sop", list(vec![sym_id("sop/box", &mut builder)], &mut builder), &mut builder);
            (Some(form), graph)
        }
    };
    let labels: Vec<String> = document::scene_graph(doc).iter().map(|i| i.label.clone()).collect();
    let name = if !labels.contains(&graph) {
        Vec::new()
    } else {
        let mut n = 2;
        let free = loop {
            let l = format!("{graph} {n}");
            if labels.contains(&l) {
                n += 1;
            } else {
                break l;
            }
        };
        vec![kw_id("name", &mut builder), string_id(&free, &mut builder)]
    };
    let mut items = vec![sym_id("scene/geometry", &mut builder), list(vec![sym_id("ref", &mut builder), sym_id(&graph, &mut builder)], &mut builder)];
    items.extend(name);
    let expression = list(items, &mut builder);
    let arena = builder.freeze();
    let mut ops = Vec::new();
    if let Some(form) = new_graph { ops.push(Op::SetGraph { name: graph, form: arena.handle(form) }); }
    let scene = scene_name(workspace);
    ops.push(Op::AddNode { scope: vec![scene.clone().into()], name: f::fresh_name(&workspace.source, &scene, "object"), expr: arena.handle(expression) });
    ops
}

/// The ops of adding a World (a world graph and a `scene/world` member); refused when the scene
/// has one.
pub fn add_world(doc: &document::T) -> R<Vec<Op>> {
    if !objects::ids("world", &document::scene_graph(doc)).is_empty() {
        return Err("The scene already has a World.".to_owned());
    }
    let workspace = &doc.workspace.ws;
    let mut used = owned(workspace);
    let gr = f::fresh_among(&mut used, "sky");
    let mut builder = SyntaxBuilder::new();
    let body = list(vec![sym_id("world/sun", &mut builder), list(vec![sym_id("world/sky", &mut builder)], &mut builder)], &mut builder);
    let graph = graph_form(&gr, "world", body, &mut builder);
    let expression = list(vec![sym_id("scene/world", &mut builder), list(vec![sym_id("ref", &mut builder), sym_id(&gr, &mut builder)], &mut builder)], &mut builder);
    let arena = builder.freeze();
    let scene = scene_name(workspace);
    Ok(vec![
        Op::SetGraph { name: gr.clone(), form: arena.handle(graph) },
        Op::AddNode { scope: vec![scene.clone().into()], name: f::fresh_name(&workspace.source, &scene, "world"), expr: arena.handle(expression) },
    ])
}
