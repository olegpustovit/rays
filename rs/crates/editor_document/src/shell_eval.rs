//! Port of lib/editor_document/shell_eval.ml: the panel tree of a `ui/workspace` graph: its panels,
//! where each was written, the viewports and canvases it names, the start state and the layout
//! switch.
//!
//! Paths are kept root-first (OCaml keeps them reversed while walking and reverses on use).
use crate::document::{self, Home, Origin, PreviewSource, Start, Switch};
use crate::workspace_doc;
use editor_core::panels::{self, Panel};
use flow_check::workspace::{self as w, Graph, Term};
use flow_check::term_arena::{PatternNode, TermId};
use flow_eval::Plan;
use flow_graph::flow_edit::{arg_of, arg_text, ArgKey};
use flow_syntax::context;
use flow_syntax::value::Value as E;
use rays_diagnostic::Diagnostic;

type R<T> = Result<T, Diagnostic>;

fn diag(code: &str, message: &str) -> Diagnostic {
    rays_diagnostic::error(code.to_owned(), message.to_owned(), None, None, None, Vec::new(), Vec::new())
}

#[derive(Clone)]
pub struct Editor {
    pub tree: panels::t,
    pub origins: Vec<(panels::Path, Origin)>,
    pub named: Vec<(panels::Path, String)>,
    pub start: Start,
    pub repeated: Vec<String>,
    pub follows: Vec<(panels::Path, panels::Path)>,
    pub viewports: Vec<(String, E)>,
    pub canvases: Vec<(String, E)>,
    pub preview_sources: Vec<(String, PreviewSource)>,
    pub switch: Option<Switch>,
}

fn viewport_key(path: &[usize]) -> String {
    format!("v{}", join(path))
}

fn join(path: &[usize]) -> String {
    path.iter().map(|i| i.to_string()).collect::<Vec<_>>().join(".")
}

#[derive(Clone)]
struct Acc {
    named: Vec<(panels::Path, String)>,
    viewports: Vec<(String, E)>,
    canvases: Vec<(String, E)>,
    switch: Option<Switch>,
    start: Start,
}

fn flag(name: &str, args: &[(String, E)]) -> bool {
    args.iter().find(|(n, _)| n == name).map(|(_, v)| v) == Some(&E::Bool(true))
}

fn text(name: &str, args: &[(String, E)]) -> Option<String> {
    match args.iter().find(|(n, _)| n == name) {
        Some((_, E::Text(t))) => Some(String::from_utf8_lossy(t).into_owned()),
        _ => None,
    }
}

fn arg(name: &str, args: &[(String, E)]) -> R<E> {
    match args.iter().find(|(n, _)| n == name) {
        Some((_, v)) => Ok(v.clone()),
        None => Err(diag("E_LOWER", &format!("A panel is missing its {name}."))),
    }
}

fn leaf(acc: &mut Acc, path: &[usize], args: &[(String, E)], panel: Panel) -> R<panels::t> {
    if flag("focus", args) && acc.start.focus.is_none() {
        acc.start.focus = Some(path.to_vec());
    }
    Ok(panels::t::Leaf(panel))
}

fn child(path: &[usize], i: usize) -> Vec<usize> {
    let mut p = path.to_vec();
    p.push(i);
    p
}

fn go(acc: &mut Acc, path: &[usize], v: &E) -> R<panels::t> {
    match v {
        E::Struct(k, _, args) if k == "ui/canvas" => {
            let key = format!("c{}", join(path));
            let drawing = arg("drawing", args)?;
            acc.canvases.push((key.clone(), drawing));
            leaf(acc, path, args, Panel::Canvas(key))
        }
        E::Struct(k, _, args) if k == "ui/viewport" => {
            let key = viewport_key(path);
            let scene = arg("scene", args)?;
            acc.viewports.push((key.clone(), scene));
            if flag("look_through", args) {
                acc.start.looking.insert(0, key.clone());
            }
            leaf(acc, path, args, Panel::View(key))
        }
        E::Struct(k, _, args) if k == "ui/graph" => {
            let here = path.to_vec();
            if let Some(name) = text("graph", args) {
                acc.named.push((here.clone(), name));
            }
            if let Some(view) = text("view", args) {
                acc.start.graph_views.insert(0, (here, view));
            }
            leaf(acc, path, args, Panel::Graph)
        }
        E::Struct(k, _, args) if k == "ui/inspector" => leaf(acc, path, args, Panel::Inspector),
        E::Struct(k, _, args) if k == "ui/spreadsheet" => leaf(acc, path, args, Panel::Spreadsheet),
        E::Struct(k, _, args) if k == "ui/outline" => leaf(acc, path, args, Panel::Outline),
        E::Struct(k, _, args) if k == "ui/list" => leaf(acc, path, args, Panel::List),
        E::Struct(k, _, args) if k == "ui/lisp" => {
            if let Some(tab) = text("tab", args) {
                acc.start.tabs.insert(0, (path.to_vec(), tab));
            }
            leaf(acc, path, args, Panel::Lisp)
        }
        E::Struct(k, _, args) if k == "ui/timeline" => leaf(acc, path, args, Panel::Timeline),
        E::Struct(k, _, args) if k == "ui/split" || k == "ui/split-at" => {
            let first = arg("first", args)?;
            let second = arg("second", args)?;
            let a = go(acc, &child(path, 0), &first)?;
            let b = go(acc, &child(path, 1), &second)?;
            let points = |name: &str| -> Option<i64> {
                match args.iter().find(|(n, _)| n == name) {
                    Some((_, E::Int(n))) => Some(*n),
                    Some((_, E::Float(f))) => Some(*f as i64),
                    _ => None,
                }
            };
            let ratio = args.iter().find(|(n, _)| n == "ratio").map(|(_, v)| v);
            let size = match (points("first_size"), points("second_size"), ratio) {
                (Some(n), _, _) => panels::Size::First(n),
                (None, Some(n), _) => panels::Size::Second(n),
                (None, None, Some(E::Float(r))) => panels::Size::Ratio(*r),
                (None, None, Some(E::Int(n))) => panels::Size::Ratio(*n as f64),
                _ => panels::Size::Ratio(0.5),
            };
            let axis = match args.iter().find(|(n, _)| n == "axis") {
                Some((_, E::Text(t))) if t == b"vertical" => panels::Axis::V,
                _ => panels::Axis::H,
            };
            Ok(panels::t::Split { axis, size, a: Box::new(a), b: Box::new(b) })
        }
        E::Struct(k, _, args) if k == "ui/tile" => {
            let mut cells = Vec::new();
            for (i, (_, v)) in args.iter().enumerate() {
                cells.push(go(acc, &child(path, i), v)?);
            }
            Ok(panels::t::Tile(cells))
        }
        E::Struct(k, _, args) if k == "ui/floating" => {
            let p = arg("panel", args)?;
            let t = go(acc, &child(path, 0), &p)?;
            Ok(panels::t::Float(Box::new(t)))
        }
        // a switch is transparent: its active layout sits at the switch's own place in the tree, and
        // a layout that is not showing registers nothing but its shape
        E::Struct(k, _, args) if k == "ui/switch" => {
            let active = match args.iter().find(|(n, _)| n == "active") {
                Some((_, E::Int(n))) => *n,
                _ => 0,
            };
            let first = acc.switch.is_none();
            if first {
                acc.switch = Some(Switch { layouts: Vec::new(), active });
            }
            let kids: Vec<&E> = args.iter().filter(|(n, _)| n != "active").map(|(_, v)| v).collect();
            let mut trees: Vec<panels::t> = Vec::new();
            for (i, kid) in kids.iter().enumerate() {
                let saved = (acc.named.clone(), acc.viewports.clone(), acc.canvases.clone(), acc.start.clone());
                let tree = go(acc, path, kid)?;
                if i as i64 != active {
                    acc.named = saved.0;
                    acc.viewports = saved.1;
                    acc.canvases = saved.2;
                    acc.start = saved.3;
                }
                trees.push(tree);
            }
            if first {
                acc.switch = Some(Switch { layouts: trees.clone(), active });
            }
            usize::try_from(active).ok().and_then(|a| trees.get(a)).cloned().ok_or_else(|| diag("E_LOWER", "A ui/switch :active names no layout."))
        }
        _ => Err(diag("E_LOWER", "The editor graph returns a (ui/workspace ...) of panels.")),
    }
}

type Tree = (panels::t, Vec<(panels::Path, String)>, Start, Vec<(String, E)>, Vec<(String, E)>, Option<Switch>);

/// The tree of a `ui/workspace` value, with the graph a `ui/graph` names and the scene of each
/// viewport; the origins come from walking the terms beside the values.
fn panel_tree(root: &E) -> R<Tree> {
    let mut acc = Acc { named: Vec::new(), viewports: Vec::new(), canvases: Vec::new(), switch: None, start: Start::default() };
    let tree = go(&mut acc, &[], root)?;
    panels::valid(&tree).map_err(|m| diag("E_RANGE", &m))?;
    Ok((tree, acc.named, acc.start, acc.viewports, acc.canvases, acc.switch))
}

fn is_loop_term(t: &Term) -> bool {
    matches!(t.node, w::Node::Loop(_))
}

struct Found {
    found: Vec<(panels::Path, Origin)>,
    ofs: Vec<(panels::Path, Term)>,
}

impl Found {
    fn add(&mut self, path: &[usize], o: Origin) {
        // consed in OCaml: the latest is first
        self.found.insert(0, (path.to_vec(), o));
    }
    fn inline(&mut self, home: &Home, path: &[usize], key: &ArgKey, t: &Term) {
        match t.node {
            w::Node::RefBinding(id) if t.arena().name_ids(t.arena().ref_binding(id).fields).is_empty() => {}
            w::Node::Loop(_) => {}
            _ => self.add(path, Origin::Inline(home.clone(), key.clone())),
        }
    }
}

fn walk(f: &mut Found, env: &[(&str, TermId)], place: Option<(Home, ArgKey)>, path: &[usize], t: &Term, v: &E) {
    let arena = t.arena();
    let here = || match (t.path(), &place) {
        (Some(path), _) => Home::BoundAt(path.iter().map(|name| t.name(*name).to_owned()).collect()),
        (None, Some((parent, key))) => Home::InlineIn(Box::new(parent.clone()), key.clone()),
        (None, None) => Home::Looped,
    };
    let pos = |i: usize| ArgKey::Pos(i as i64);
    match (t.node, v) {
        (w::Node::RefBinding(id), _) if arena.name_ids(arena.ref_binding(id).fields).is_empty() => {
            let name = t.name(arena.ref_binding(id).name);
            f.add(path, Origin::Bound(name.to_owned()));
            if let Some((_, id)) = env.iter().find(|(key, _)| *key == name) {
                walk(f, env, None, path, &t.child(*id), v);
            }
        }
        (w::Node::Op(id), _) if matches!(t.name(arena.op(id).op), "ui/inspector" | "ui/spreadsheet" | "ui/list" | "ui/lisp") => {
            if let Some(field) = arena.fields(arena.op(id).args).iter().find(|field| t.name(field.name) == "of") {
                f.ofs.push((path.to_vec(), t.child(field.term)));
            }
        }
        (w::Node::Op(id), E::Struct(_, _, vargs)) => {
            let op = arena.op(id);
            let args = arena.fields(op.args);
            match t.name(op.op) {
                "ui/workspace" if !args.is_empty() && !vargs.is_empty() => {
                    let r = t.child(args[0].term);
                    let h = here();
                    f.inline(&h, path, &pos(0), &r);
                    walk(f, env, Some((h, pos(0))), path, &r, &vargs[0].1);
                }
                "ui/split" | "ui/split-at" => {
                    let h = here();
                    for (i, key) in ["first", "second"].iter().enumerate() {
                        if let (Some(index), Some((_, va))) = (args.iter().position(|field| t.name(field.name) == *key), vargs.iter().find(|(name, _)| name.as_str() == *key)) {
                            let a = t.child(args[index].term);
                            let k = pos(index);
                            let p = child(path, i);
                            f.inline(&h, &p, &k, &a);
                            walk(f, env, Some((h.clone(), k)), &p, &a, va);
                        }
                    }
                }
                // transparent: the active layout is found at the switch's own path
                "ui/switch" => {
                    let h = here();
                    let active = match vargs.iter().find(|(name, _)| name == "active") {
                        Some((_, E::Int(n))) => *n,
                        _ => 0,
                    };
                    if let Ok(i) = usize::try_from(active) {
                        if let (Some(field), Some((_, va))) = (args.iter().filter(|field| t.name(field.name) != "active").nth(i), vargs.iter().filter(|(name, _)| name != "active").nth(i)) {
                            let a = t.child(field.term);
                            f.inline(&h, path, &pos(i), &a);
                            walk(f, env, Some((h, pos(i))), path, &a, va);
                        }
                    }
                }
                "ui/floating" if !args.is_empty() && !vargs.is_empty() => {
                    let h = here();
                    let p = child(path, 0);
                    let a = t.child(args[0].term);
                    f.inline(&h, &p, &pos(0), &a);
                    walk(f, env, Some((h, pos(0))), &p, &a, &vargs[0].1);
                }
                "ui/tile" => {
                    let h = here();
                    if let Some(li) = args.iter().position(|field| is_loop_term(&t.child(field.term))) {
                        let key = pos(li);
                        for i in 0..vargs.len() { f.add(&child(path, i), Origin::Loop(h.clone(), key.clone())); }
                    } else {
                        for (i, field) in args.iter().enumerate() {
                            let p = child(path, i);
                            let a = t.child(field.term);
                            f.inline(&h, &p, &pos(i), &a);
                            if let Some((_, va)) = vargs.get(i) {
                                walk(f, env, Some((h.clone(), pos(i))), &p, &a, va);
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        _ => {}
    }
}

/// Which panels are named: the terms of the graph beside its value.
fn origins(graph: &Graph, value: &E) -> (Vec<(panels::Path, Origin)>, Vec<(panels::Path, Term)>) {
    let arena = graph.body.arena();
    let (bindings, result) = match graph.body.node {
        w::Node::Let(id) => {
            let node = arena.let_node(id);
            (arena.bindings(node.bindings), graph.body.child(node.body))
        }
        _ => (&[][..], graph.body.clone()),
    };
    let env: Vec<(&str, TermId)> = bindings.iter().filter_map(|binding| match arena.pattern(binding.pattern) {
        PatternNode::Name(name) => Some((arena.name(name), binding.term)),
        _ => None,
    }).collect();
    let mut f = Found { found: Vec::new(), ofs: Vec::new() };
    walk(&mut f, &env, None, &[], &result, value);
    (f.found, f.ofs)
}

fn authored(source: &[flow_syntax::syntax::Syntax], home: &Home) -> Option<flow_syntax::syntax::Syntax> {
    match home {
        Home::BoundAt(path) => arg_text(source, path, &ArgKey::Whole),
        Home::InlineIn(h, arg) => authored(source, h).and_then(|form| arg_of(&form, arg)),
        Home::Copy { .. } | Home::Looped => None,
    }
}

fn remap(key: &dyn Fn(&str) -> String, t: &panels::t) -> panels::t {
    match t {
        panels::t::Leaf(Panel::View(old)) => panels::t::Leaf(Panel::View(key(old))),
        panels::t::Leaf(Panel::Canvas(old)) => panels::t::Leaf(Panel::Canvas(key(old))),
        panels::t::Leaf(p) => panels::t::Leaf(p.clone()),
        panels::t::Split { axis, size, a, b } => panels::t::Split { axis: *axis, size: *size, a: Box::new(remap(key, a)), b: Box::new(remap(key, b)) },
        panels::t::Tile(cells) => panels::t::Tile(cells.iter().map(|c| remap(key, c)).collect()),
        panels::t::Float(p) => panels::t::Float(Box::new(remap(key, p))),
    }
}

/// The editor graph's tree, or none when the workspace has no editor graph.
pub fn editor(workspace: &workspace_doc::T, plan: &Plan) -> R<Option<Editor>> {
    let value = workspace_doc::context_value(false, workspace, plan, context::editor())?;
    match value {
        None => Ok(None),
        Some(whole @ E::Struct(..)) if matches!(&whole, E::Struct(k, _, args) if k == "ui/workspace" && args.first().map_or(false, |(n, _)| n == "root")) => {
            let E::Struct(_, _, wargs) = &whole else { unreachable!() };
            let root = &wargs[0].1;
            let (tree, named, start, viewports, canvases, switch) = panel_tree(root)?;
            let g = workspace_doc::editor_graph(workspace).expect("an editor value has an editor graph");
            let (origins, ofs) = origins(&g, &whole);
            let all = panels::leaves(&tree);
            let origin_of = |path: &panels::Path| origins.iter().find(|(p, _)| p == path).map(|(_, o)| o.clone());
            let bound = |path: &panels::Path| match origin_of(path) {
                Some(Origin::Bound(n)) => Some(n),
                _ => None,
            };
            let names: Vec<String> = all.iter().filter_map(|(path, _)| bound(path)).collect();
            let mut repeated: Vec<String> = names.iter().filter(|n| names.iter().filter(|m| m == n).count() > 1).cloned().collect();
            repeated.sort();
            repeated.dedup();
            // [:of name]: the graph leaf that binding makes in the layout shown
            let mut follows: Vec<(panels::Path, panels::Path)> = Vec::new();
            for (path, term) in &ofs {
                match &term.node {
                    w::Node::RefBinding(id) if term.arena().name_ids(term.arena().ref_binding(*id).fields).is_empty() => {
                        let n = term.name(term.arena().ref_binding(*id).name);
                        match all.iter().find(|(p, panel)| *panel == Panel::Graph && bound(p).as_deref() == Some(n)) {
                        Some((target, _)) => follows.insert(0, (path.clone(), target.clone())),
                        None => return Err(diag("E_PANEL_OF", &format!(":of {n} is not a graph panel of the layout shown."))),
                        }
                    }
                    _ => return Err(diag("E_PANEL_OF", ":of takes the binding of a graph panel.")),
                }
            }
            let leaves: Vec<(panels::Path, String, Option<Origin>)> = all
                .iter()
                .filter_map(|(path, panel)| match panel {
                    Panel::View(key) | Panel::Canvas(key) => Some((path.clone(), key.clone(), origin_of(path))),
                    _ => None,
                })
                .collect();
            // ponytail: quadratic uniqueness scan over viewport leaves; use name
            // counts if large layouts beyond the 16-renderer limit need it.
            let keys: Vec<(String, String)> = leaves
                .iter()
                .map(|(_, old, origin)| {
                    let key = match origin {
                        Some(Origin::Bound(name)) if leaves.iter().filter(|(_, _, o)| o == origin).count() == 1 => {
                            format!("{}:{}/{}", old.chars().next().unwrap_or(' '), sop::node::ocaml_quote(&g.name), sop::node::ocaml_quote(name))
                        }
                        _ => old.clone(),
                    };
                    (old.clone(), key)
                })
                .collect();
            let key = |old: &str| -> String { keys.iter().find(|(o, _)| o == old).map(|(_, k)| k.clone()).expect("every leaf key was listed") };
            let preview_sources: Vec<(String, PreviewSource)> = leaves
                .iter()
                .filter(|(_, old, _)| viewports.iter().any(|(k, _)| k == old))
                .map(|(_, old, origin)| {
                    let panel_form = match origin {
                        Some(Origin::Bound(name)) => arg_text(&workspace.source, &[g.name.clone(), name.clone()], &ArgKey::Whole),
                        Some(Origin::Inline(home, arg)) => authored(&workspace.source, home).and_then(|form| arg_of(&form, arg)),
                        Some(Origin::Loop(..)) | None => None,
                    };
                    (
                        key(old),
                        PreviewSource {
                            editor_graph: g.name.clone(),
                            panel: origin.clone(),
                            scene_ref: panel_form.and_then(|form| arg_of(&form, &ArgKey::Pos(0))),
                            instance: viewports.iter().find(|(k, _)| k == old).map(|(_, v)| v.clone()).expect("filtered above"),
                        },
                    )
                })
                .collect();
            let mut start = start;
            start.looking = start.looking.iter().map(|k| key(k)).collect();
            Ok(Some(Editor {
                tree: remap(&key, &tree),
                origins,
                named,
                repeated,
                follows,
                start,
                viewports: viewports.iter().map(|(old, scene)| (key(old), scene.clone())).collect(),
                canvases: canvases.iter().map(|(old, drawing)| (key(old), drawing.clone())).collect(),
                preview_sources,
                switch,
            }))
        }
        Some(_) => Err(diag("E_LOWER", "The editor graph returns a (ui/workspace ...).")),
    }
}

#[allow(dead_code)]
fn _uses(_: &document::T) {}
