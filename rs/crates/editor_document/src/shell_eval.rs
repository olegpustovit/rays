//! Port of lib/editor_document/shell_eval.ml: the panel tree of a `ui/workspace` graph: its panels,
//! where each was written, the viewports and canvases it names, the start state and the layout
//! switch.
//!
//! Paths are kept root-first (OCaml keeps them reversed while walking and reverses on use).
use crate::document::{self, Home, Origin, PreviewSource, Start, Switch};
use crate::workspace_doc;
use editor_core::panels::{self, Panel};
use flow_check::workspace::{self as w, Graph, Pattern, Term};
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
    matches!(t.node, w::Node::Loop { .. })
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
            w::Node::RefBinding(_, ref p) if p.is_empty() => {}
            w::Node::Loop { .. } => {}
            _ => self.add(path, Origin::Inline(home.clone(), key.clone())),
        }
    }
}

fn walk(f: &mut Found, env: &[(String, Term)], place: Option<(Home, ArgKey)>, path: &[usize], t: &Term, v: &E) {
    let here = || match (&t.path, &place) {
        (Some(p), _) => Home::BoundAt(p.clone()),
        (None, Some((parent, key))) => Home::InlineIn(Box::new(parent.clone()), key.clone()),
        (None, None) => Home::Looped,
    };
    let pos = |i: usize| ArgKey::Pos(i as i64);
    match (&t.node, v) {
        (w::Node::RefBinding(n, rest), _) if rest.is_empty() => {
            f.add(path, Origin::Bound(n.clone()));
            if let Some((_, t2)) = env.iter().find(|(k, _)| k == n) {
                walk(f, env, None, path, t2, v);
            }
        }
        (w::Node::Op { op, args, .. }, _) if matches!(op.as_str(), "ui/inspector" | "ui/spreadsheet" | "ui/list" | "ui/lisp") => {
            if let Some((_, a)) = args.iter().find(|(n, _)| n == "of") {
                f.ofs.push((path.to_vec(), a.clone()));
            }
        }
        (w::Node::Op { op, args, .. }, E::Struct(_, _, vargs)) if op == "ui/workspace" && !args.is_empty() && !vargs.is_empty() => {
            let r = &args[0].1;
            let rv = &vargs[0].1;
            let h = here();
            f.inline(&h, path, &pos(0), r);
            walk(f, env, Some((h, pos(0))), path, r, rv);
        }
        (w::Node::Op { op, args, .. }, E::Struct(_, _, vargs)) if op == "ui/split" || op == "ui/split-at" => {
            let h = here();
            for (i, key) in ["first", "second"].iter().enumerate() {
                if let (Some((_, a)), Some((_, va))) = (args.iter().find(|(n, _)| n == key), vargs.iter().find(|(n, _)| n == key)) {
                    let k = pos(args.iter().position(|(n, _)| n == key).expect("found above"));
                    let p = child(path, i);
                    f.inline(&h, &p, &k, a);
                    walk(f, env, Some((h.clone(), k)), &p, a, va);
                }
            }
        }
        // transparent: the active layout is found at the switch's own path
        (w::Node::Op { op, args, .. }, E::Struct(_, _, vargs)) if op == "ui/switch" => {
            let h = here();
            let layouts = |l: &[(String, Term)]| l.iter().filter(|(n, _)| n != "active").cloned().collect::<Vec<_>>();
            let vlayouts: Vec<&(String, E)> = vargs.iter().filter(|(n, _)| n != "active").collect();
            let active = match vargs.iter().find(|(n, _)| n == "active") {
                Some((_, E::Int(n))) => *n,
                _ => 0,
            };
            let idx = usize::try_from(active).ok();
            if let (Some(i), ls) = (idx, layouts(args)) {
                if let (Some((_, a)), Some((_, va))) = (ls.get(i), vlayouts.get(i).map(|x| *x)) {
                    f.inline(&h, path, &pos(i), a);
                    walk(f, env, Some((h, pos(i))), path, a, va);
                }
            }
        }
        (w::Node::Op { op, args, .. }, E::Struct(_, _, vargs)) if op == "ui/floating" && !args.is_empty() && !vargs.is_empty() => {
            let h = here();
            let p = child(path, 0);
            f.inline(&h, &p, &pos(0), &args[0].1);
            walk(f, env, Some((h, pos(0))), &p, &args[0].1, &vargs[0].1);
        }
        (w::Node::Op { op, args, .. }, E::Struct(_, _, vargs)) if op == "ui/tile" => {
            let h = here();
            if let Some(li) = args.iter().position(|(_, a)| is_loop_term(a)) {
                let key = pos(li);
                for i in 0..vargs.len() {
                    f.add(&child(path, i), Origin::Loop(h.clone(), key.clone()));
                }
            } else {
                for (i, (_, a)) in args.iter().enumerate() {
                    let p = child(path, i);
                    f.inline(&h, &p, &pos(i), a);
                    if let Some((_, va)) = vargs.get(i) {
                        walk(f, env, Some((h.clone(), pos(i))), &p, a, va);
                    }
                }
            }
        }
        _ => {}
    }
}

/// Which panels are named: the terms of the graph beside its value.
fn origins(graph: &Graph, value: &E) -> (Vec<(panels::Path, Origin)>, Vec<(panels::Path, Term)>) {
    let (bindings, result): (Vec<(Pattern, Term)>, Term) = match &graph.body.node {
        w::Node::Let(bs, r) => (bs.clone(), (**r).clone()),
        _ => (Vec::new(), graph.body.clone()),
    };
    let env: Vec<(String, Term)> = bindings
        .into_iter()
        .filter_map(|(p, t)| match p {
            Pattern::Name(n) => Some((n, t)),
            _ => None,
        })
        .collect();
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
                    w::Node::RefBinding(n, rest) if rest.is_empty() => match all.iter().find(|(p, panel)| *panel == Panel::Graph && bound(p).as_deref() == Some(n.as_str())) {
                        Some((target, _)) => follows.insert(0, (path.clone(), target.clone())),
                        None => return Err(diag("E_PANEL_OF", &format!(":of {n} is not a graph panel of the layout shown."))),
                    },
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
