// Port of lib/pxui_graph/scope_commands.ml(i): maps keys and menus to typed requests.
use crate::scope_state::{self as st, Change, Command, Direction, Drag, Editing, Fr, HintTarget, Hinting, Pane, PathSet};
use crate::scope_wires::{fallback, line_of_row, root_name, rows_top};
use editor_core::editor_core::command;
use editor_core::editor_core::guide_context::GuideContext as Ctx;
use editor_core::editor_core::keymap::Trigger;
use flow_check::workspace::Path;
use flow_graph::edit_tree::ArgKey;
use flow_graph::flow_edit as e;
use flow_graph::flow_edit::Op;
use flow_graph::probe::Summary;
use flow_graph::projection as p;
use flow_graph::projection::Level;
use flow_syntax::lisp;
use flow_syntax::syntax::{self as s, Node, Syntax};
use flow_syntax::ty::{self, Ty};
use flow_syntax::value::Value;
use input::Key;
use std::collections::{HashMap, HashSet};

pub type S = Syntax;

fn drop_last(q: &[String]) -> Vec<String> {
    if q.is_empty() { Vec::new() } else { q[..q.len() - 1].to_vec() }
}

pub fn parents_removed(paths: &[Path]) -> Vec<Path> {
    let set: PathSet = paths.iter().cloned().collect();
    // a path above this one is among them
    fn under(q: &[String], set: &PathSet) -> bool {
        if q.len() <= 1 {
            return false;
        }
        let up = drop_last(q);
        set.contains(&up) || under(&up, set)
    }
    paths.iter().filter(|q| !under(q, &set)).cloned().collect()
}

fn nested_chip(r: &p::Row) -> bool {
    match &r.chip {
        p::Chip::Inline { .. } => true,
        p::Chip::Name(leaf) => e::nested(leaf),
        _ => false,
    }
}

fn first_inline(n: &p::Node) -> Option<&p::Row> {
    n.rows.iter().find(|r| nested_chip(r))
}

fn inline_rail(n: &p::Node) -> Option<&p::RailRow> {
    let z = n.zone.as_ref()?;
    z.rail.iter().find(|r| matches!(r.expr.as_ref().map(|x| &x.node), Some(Node::List(_))) && r.key.is_some())
}

/// An open expression folds from its outermost call.
fn outer_expr(t: &Pane, path: &[String]) -> Path {
    let up = |q: &[String]| -> Vec<String> {
        match q.split_last() {
            Some((l, r)) => {
                let mut v = r.to_vec();
                v.push(match l.rfind('#') {
                    Some(i) => l[..i].to_owned(),
                    None => l.clone(),
                });
                v
            }
            None => Vec::new(),
        }
    };
    let mut q = path.to_vec();
    loop {
        let u = up(&q);
        if u != q && matches!(t.scope.as_ref().and_then(|sc| p::find(sc, &u)), Some(n) if n.expr) {
            q = u;
        } else {
            return q;
        }
    }
}

pub fn wired(r: &p::Row) -> bool {
    !p::sources(r).is_empty()
}

/// The value the wire carried, as the literal a disconnected row keeps.
/// ponytail: read at no probe, so a source inside a loop keeps the default
fn last_value(t: &Pane, n: &p::Node, r: &p::Row) -> Option<S> {
    let records = t.records.as_ref()?;
    let srcs = p::sources(r);
    if srcs.len() != 1 {
        return None;
    }
    let mut path = drop_last(&n.path);
    path.push(srcs[0].clone());
    let at = records.borrow_mut().at(&path, &[]);
    use flow_syntax::arena::{Draft, SyntaxBuilder};
    let mut builder = SyntaxBuilder::new();
    let mut vector = |values: &[f64]| {
        Draft::Vec(values.iter().map(|f| builder.push_draft(0, Draft::Num(lisp::float(*f)), Default::default(), None)).collect())
    };
    let draft = match at {
        Some(Summary::Value(Value::Int(i))) => Draft::Num(i.to_string()),
        Some(Summary::Value(Value::Float(f))) if f.is_finite() => Draft::Num(lisp::float(f)),
        Some(Summary::Value(Value::Bool(b))) => Draft::Sym(b.to_string()),
        Some(Summary::Value(Value::Text(b))) => Draft::Str(b),
        Some(Summary::Value(Value::Vec2(a, b))) => vector(&[a, b]),
        Some(Summary::Value(Value::Vec3(a, b, c))) => vector(&[a, b, c]),
        Some(Summary::Value(Value::Vec4(a, b, c, d))) => vector(&[a, b, c, d]),
        _ => return None,
    };
    let id = builder.push_draft(0, draft, Default::default(), None);
    Some(builder.freeze().handle(id))
}

/// Taking a wire off a row: the nested node written there stays, as a binding of its own.
pub fn unwire(t: &Pane, n: &p::Node, r: &p::Row) -> Vec<Op> {
    if !r.cells.is_empty() {
        // the calls in a vector's cells: each cell is set, the vector's other cells stay
        return r
            .cells
            .iter()
            .map(|(i, _)| {
                let (node, key, fallback) = p::cell_wire(n, r, *i);
                Op::Disconnect { node, key, fallback }
            })
            .collect();
    }
    let fb = match last_value(t, n, r) {
        Some(v) if r.kind != p::RowKind::Rest => Some(v),
        _ => fallback(r),
    };
    let mut out = Vec::new();
    if let p::Chip::Name(leaf) = &r.chip {
        if e::nested(leaf) {
            out.push(Op::Unfold { node: n.path.clone(), key: r.key.clone(), sub: Vec::new() });
        }
    }
    out.push(Op::Disconnect { node: n.path.clone(), key: r.key.clone(), fallback: fb });
    out
}

/// The edits that take the wires off these inputs.
pub fn unwire_at(t: &Pane, inputs: &[(Path, ArgKey)]) -> Vec<Op> {
    let mut out = Vec::new();
    for (path, key) in inputs {
        let found = st::node_of(t, path).and_then(|n| n.rows.iter().find(|r| r.key == *key).cloned().map(|r| (n, r)));
        match found {
            Some((n, r)) => out.extend(unwire(t, &n, &r)),
            None => {
                // the end of a wire from a vector's cell: its holder is no node
                if let ArgKey::Cell(_) = key {
                    let mut builder = flow_syntax::arena::SyntaxBuilder::new();
                    let name = builder.intern("0.0");
                    let id = builder.push(0, Node::Num(name), Default::default(), None);
                    out.push(Op::Disconnect { node: path.clone(), key: key.clone(), fallback: Some(builder.freeze().handle(id)) });
                }
            }
        }
    }
    out
}

// ---------------------------------------------------------------- frames

pub fn scope_of_path(path: &[String]) -> Vec<String> {
    drop_last(path)
}

/// A frame's size while its corner is dragged.
pub fn frame_list(t: &Pane, scope: &[String]) -> Vec<Fr> {
    let frames = t.saved.frames(scope);
    match &t.drag {
        Some(Drag::Sizing { scope: sc, index, dw, dh }) if sc.as_slice() == scope => frames
            .into_iter()
            .enumerate()
            .map(|(i, (title, at, (wd, ht)))| if i as i64 == *index { (title, at, (48.0f64.max(wd + dw), 32.0f64.max(ht + dh))) } else { (title, at, (wd, ht)) })
            .collect(),
        Some(Drag::Carrying { scope: sc, index, dx, dy, .. }) if sc.as_slice() == scope => frames
            .into_iter()
            .enumerate()
            .map(|(i, (title, (x, y), size))| if i as i64 == *index { (title, (x + dx, y + dy), size) } else { (title, (x, y), size) })
            .collect(),
        _ => frames,
    }
}

/// A frame around the selected nodes of one scope, in that scope's coordinates.
fn new_frame(t: &Pane) -> Option<(Path, Fr)> {
    let paths: Vec<Path> = st::selected(t).into_iter().filter(|q| t.geo.slot.contains_key(q)).collect();
    let first = paths.first()?;
    let scope = scope_of_path(first);
    let boxes: Vec<(f64, f64, f64, f64)> = paths.iter().filter_map(|q| if scope_of_path(q) == scope { st::box_at(t, q) } else { None }).collect();
    let (ox, oy) = t.geo.origins.iter().find(|(q, _)| *q == scope).map(|(_, o)| *o).unwrap_or((0.0, 0.0));
    let x0 = boxes.iter().fold(f64::INFINITY, |a, (x, _, _, _)| a.min(*x));
    let y0 = boxes.iter().fold(f64::INFINITY, |a, (_, y, _, _)| a.min(*y));
    let x1 = boxes.iter().fold(f64::NEG_INFINITY, |a, (x, _, wd, _)| a.max(x + wd));
    let y1 = boxes.iter().fold(f64::NEG_INFINITY, |a, (_, y, _, h)| a.max(y + h));
    Some((scope, ("frame".to_owned(), (x0 - 12.0 - ox, y0 - 24.0 - oy), (x1 - x0 + 24.0, y1 - y0 + 36.0))))
}

pub fn input_of(t: &Pane, path: &[String]) -> Option<p::Input> {
    t.scope.as_ref().and_then(|sc| sc.inputs.iter().find(|i| i.path.as_slice() == path).cloned())
}

// ------------------------------------------------------------- commands

/// `Some hidden` when pressing b hides or shows the node: it has a :visible argument and no bypass.
fn hide_row(n: &p::Node) -> Option<bool> {
    if p::bypassable(n) {
        return None;
    }
    n.rows.iter().find_map(|r| match (&r.key, r.kind, &r.ty) {
        (ArgKey::Kw(k), p::RowKind::Arg, Some(Ty::Bool)) if k == "visible" => Some(matches!(r.expr.as_ref().map(|x| &x.node), Some(Node::Sym(v)) if r.expr.as_ref().unwrap().name(*v) == "false")),
        _ => None,
    })
}

pub fn conditional_target(n: &p::Node) -> (Path, i64) {
    if let Some((leaf, outer)) = n.path.split_last() {
        if let Some((holder, keys)) = e::leaf_keys(leaf) {
            if let [ArgKey::Arm(index)] = keys.as_slice() {
                let mut v = outer.to_vec();
                v.push(holder);
                return (v, *index);
            }
        }
    }
    (n.path.clone(), -1)
}

fn edit(op: Op) -> Vec<Change> {
    vec![Change::Syntax_edit(op)]
}

fn row_of(t: &Pane, path: &[String], key: &ArgKey) -> Option<(p::Node, p::Row)> {
    st::node_of(t, path).and_then(|n| n.rows.iter().find(|r| r.key == *key).cloned().map(|r| (n, r)))
}

pub fn action_changes(t: &Pane, command: Command) -> Vec<Change> {
    let nodes = st::selected_nodes(t);
    let paths: Vec<Path> = nodes.iter().filter(|n| !n.synthetic).map(|n| n.path.clone()).collect();
    let one = |f: &dyn Fn(&p::Node) -> Vec<Change>| -> Vec<Change> {
        if nodes.len() == 1 { f(&nodes[0]) } else { vec![Change::Notice("Select one node".to_owned())] }
    };
    let wrap = |loop_: e::Loop, what: &str| -> Vec<Change> {
        if paths.is_empty() { vec![Change::Notice(format!("Select nodes to {}", what))] } else { edit(Op::Wrap { nodes: paths.clone(), loop_ }) }
    };
    match command {
        Command::Delete => {
            if let Some((path, key)) = &t.hovered_row {
                let hit = st::node_of(t, path).map(|n| n.rows.iter().any(|r| r.key == *key && (wired(r) || r.kind == p::RowKind::Rest))).unwrap_or(false);
                if hit {
                    // a wire comes off; an item of a variadic input (a list's, a merge's) goes with what it holds
                    let n = st::node_of(t, path).unwrap();
                    let r = n.rows.iter().find(|r| r.key == *key).unwrap().clone();
                    return unwire(t, &n, &r).into_iter().map(Change::Syntax_edit).collect();
                }
            }
            match &t.selected_wire {
                Some((path, key, fb)) => match row_of(t, path, key) {
                    Some((n, r)) => unwire(t, &n, &r).into_iter().map(Change::Syntax_edit).collect(),
                    None => edit(Op::Disconnect { node: path.clone(), key: key.clone(), fallback: fb.clone() }),
                },
                None => {
                    if paths.is_empty() { Vec::new() } else { edit(Op::DeleteNodes { nodes: paths.clone() }) }
                }
            }
        }
        Command::Fold_into => one(&|n| edit(Op::FoldInto { node: n.path.clone() })),
        Command::Unfold => one(&|n| {
            let (parent, arm) = conditional_target(n);
            if parent != n.path {
                return edit(Op::Unfold { node: parent, key: ArgKey::Arm(arm), sub: Vec::new() });
            }
            if let Some((pp, key)) = &t.hovered_row {
                if *pp == n.path {
                    return edit(Op::Unfold { node: n.path.clone(), key: key.clone(), sub: Vec::new() });
                }
            }
            if let Some(r) = first_inline(n) {
                return edit(Op::Unfold { node: n.path.clone(), key: r.key.clone(), sub: Vec::new() });
            }
            match inline_rail(n) {
                Some(p::RailRow { key: Some(key), .. }) => edit(Op::Unfold { node: n.path.clone(), key: key.clone(), sub: Vec::new() }),
                _ => vec![Change::Notice("Nothing to unfold here".to_owned())],
            }
        }),
        Command::Unfold_all => one(&|n| {
            let ops: Vec<Op> = n.rows.iter().filter(|r| nested_chip(r)).map(|r| Op::Unfold { node: n.path.clone(), key: r.key.clone(), sub: Vec::new() }).collect();
            if ops.is_empty() { vec![Change::Notice("Nothing to unfold here".to_owned())] } else { ops.into_iter().flat_map(edit).collect() }
        }),
        Command::Hoist => one(&|n| edit(Op::Hoist { node: n.path.clone() })),
        Command::Bypass => one(&|n| {
            // a node that cannot pass an input through but has a boolean :visible (a scene object) takes
            // itself out of the render without leaving the graph
            match hide_row(n) {
                Some(hidden) => {
                    let mut builder = flow_syntax::arena::SyntaxBuilder::new();
                    let name = builder.intern(if hidden { "true" } else { "false" });
                    let id = builder.push(0, Node::Sym(name), Default::default(), None);
                    edit(Op::SetArg {
                        node: n.path.clone(),
                        key: ArgKey::Kw("visible".to_owned()),
                        sub: Vec::new(),
                        value: builder.freeze().handle(id),
                    })
                },
                None => edit(Op::ToggleBypass { node: n.path.clone() }),
            }
        }),
        Command::Wrap_repeat => wrap(e::Loop::For, "repeat"),
        Command::Wrap_iterate => wrap(e::Loop::Fold, "iterate"),
        Command::Wrap_if => wrap(e::Loop::If, "wrap"),
        Command::Wrap_map => wrap(e::Loop::Map, "map"),
        Command::Wrap_filter => wrap(e::Loop::Filter, "filter"),
        Command::Inline_fn => one(&|n| edit(Op::InlineDefn { node: n.path.clone() })),
        Command::Add_arm => one(&|n| {
            let (node, after) = conditional_target(n);
            edit(Op::AddArm { node, after })
        }),
        Command::Delete_arm => one(&|n| {
            let (node, index) = conditional_target(n);
            let index = if node == n.path { 0 } else { index };
            edit(Op::DeleteArm { node, index })
        }),
        Command::Make_fn => {
            if paths.is_empty() { vec![Change::Notice("Select nodes to make a function".to_owned())] } else { edit(Op::MakeLocalFn { nodes: paths.clone() }) }
        }
        Command::Make_macro => {
            if paths.is_empty() { vec![Change::Notice("Select nodes to make a macro".to_owned())] } else { vec![Change::Macro_requested(paths.clone())] }
        }
        Command::Make_defn => {
            if paths.is_empty() { vec![Change::Notice("Select nodes to make a reusable function".to_owned())] } else { vec![Change::Defn_requested(paths.clone())] }
        }
        Command::Collapse => nodes
            .iter()
            .filter_map(|n| match &n.zone {
                None if n.expr => Some(Change::Zone_collapsed { zone: outer_expr(t, &n.path), collapsed: true }),
                Some(z) if z.kind == p::ZoneKind::Let => None,
                None => None,
                Some(_) => Some(Change::Zone_collapsed { zone: n.path.clone(), collapsed: !t.saved.collapsed(&n.path) }),
            })
            .collect(),
        Command::Probe_step(delta) => {
            // with nothing selected, one stepper pins the iteration of every zone of the graph
            let nodes: Vec<p::Node> = if !nodes.is_empty() {
                nodes.clone()
            } else {
                t.geo.items.iter().filter_map(|(pl, _, _)| match &pl.item { p::Item::Item(n) => Some(n.clone()), _ => None }).collect()
            };
            nodes
                .iter()
                .filter_map(|n| match &n.zone {
                    Some(z) if z.kind != p::ZoneKind::Let => {
                        let count = st::count_of(t, &n.path);
                        if count <= 0 {
                            None
                        } else {
                            let cur = t.saved.probe(&n.path);
                            let index = 0.max((count - 1).min(cur + delta));
                            if index == cur { None } else { Some(Change::Probe_set { zone: n.path.clone(), index }) }
                        }
                    }
                    _ => None,
                })
                .collect()
        }
        Command::Frame_all | Command::Tidy | Command::Toggle_tags | Command::Frame_selection | Command::Walk(_) | Command::Edit_name | Command::Make_frame | Command::Open_level
        | Command::Point_level | Command::Open_all | Command::Point_all | Command::Show_hints => Vec::new(),
        Command::Duplicate => {
            if paths.is_empty() { vec![Change::Notice("Select nodes to duplicate".to_owned())] } else { edit(Op::Duplicate { nodes: paths.clone() }) }
        }
        Command::Copy => {
            if paths.is_empty() { vec![Change::Notice("Select nodes to copy".to_owned())] } else { vec![Change::Copy_requested(paths.clone())] }
        }
        Command::Cut => {
            if paths.is_empty() {
                vec![Change::Notice("Select nodes to cut".to_owned())]
            } else {
                let mut v = vec![Change::Copy_requested(paths.clone())];
                v.extend(edit(Op::DeleteNodes { nodes: paths.clone() }));
                v
            }
        }
        Command::Paste => vec![Change::Paste_requested],
        Command::Display => one(&|n| {
            if ![ty::geometry(), ty::drawing(), ty::image()].contains(&n.ty) {
                vec![Change::Notice("Only a geometry, drawing or image node can be viewed".to_owned())]
            } else {
                vec![Change::Display_set(n.path.clone())]
            }
        }),
        Command::Item_up | Command::Item_down => {
            if let Some((path, ArgKey::Pos(i))) = &t.hovered_row {
                if matches!(st::node_of(t, path), Some(n) if p::reorderable(&n)) {
                    return edit(Op::MoveItem { node: path.clone(), pos: if command == Command::Item_up { *i } else { *i + 1 } });
                }
            }
            // off a list item: a step of a chain moves along it; a binding moves in the text (cards stay put)
            one(&|n| {
                if e::nested(n.path.last().map(|x| x.as_str()).unwrap_or("")) {
                    edit(Op::MoveStep { node: n.path.clone(), earlier: command == Command::Item_up })
                } else {
                    edit(Op::MoveBinding { node: n.path.clone(), by: if command == Command::Item_up { -1 } else { 1 } })
                }
            })
        }
    }
}

fn frame_selection(t: &mut Pane) {
    let boxes: Vec<(f64, f64, f64, f64)> = st::selected(t).iter().filter_map(|q| st::box_at(t, q)).collect();
    if boxes.is_empty() {
        return st::frame_all(t);
    }
    let x0 = boxes.iter().fold(f64::INFINITY, |a, (x, _, _, _)| a.min(*x));
    let y0 = boxes.iter().fold(f64::INFINITY, |a, (_, y, _, _)| a.min(*y));
    let x1 = boxes.iter().fold(f64::NEG_INFINITY, |a, (x, _, wd, _)| a.max(x + wd));
    let y1 = boxes.iter().fold(f64::NEG_INFINITY, |a, (_, y, _, h)| a.max(y + h));
    let wd = 1.0f64.max(x1 - x0);
    let ht = 1.0f64.max(y1 - y0);
    let zoom = 0.25f64.max(1.5f64.min(((t.width as f64 - 40.0) / wd).min((t.height as f64 - 40.0) / ht)));
    let pan_x = ((t.width as f64 - (wd * zoom)) / 2.0) - (x0 * zoom);
    let pan_y = ((t.height as f64 - (ht * zoom)) / 2.0) - (y0 * zoom);
    (t.zoom, t.pan_x, t.pan_y) = (zoom, pan_x, pan_y);
}

// ---- levels (flow.md 6.4) ----

fn level_up(l: Level) -> Level {
    match l {
        Level::Point => Level::Chip,
        Level::Chip => Level::Card,
        Level::Card | Level::Full => Level::Full,
    }
}

/// The nodes a level applies to: cards, not zones or value cards.
fn level_of(pl: &p::Placed) -> Option<Level> {
    match &pl.item {
        p::Item::Item(n) if n.zone.is_none() && !p::value_card(n) => Some(pl.level),
        _ => None,
    }
}

fn levelled(t: &Pane, path: &[String]) -> Option<Level> {
    t.item_at(path).and_then(|(pl, _, _)| level_of(pl))
}

fn level_command(t: &mut Pane, command: Command, emit: &mut Vec<Change>) {
    let chosen: Vec<(Path, Level)> = match command {
        Command::Open_all | Command::Point_all => t.geo.items.iter().filter_map(|(pl, _, _)| level_of(pl).map(|l| (pl.path.clone(), l))).collect(),
        _ => st::selected(t).into_iter().filter_map(|q| levelled(t, &q).map(|l| (q, l))).collect(),
    };
    if chosen.is_empty() {
        emit.push(Change::Notice("Select a card to change its level".to_owned()));
        return;
    }
    // by path: every node may be chosen, so no list is searched per node
    let mut before: HashMap<Path, Level> = HashMap::new();
    for (q, l) in t.back.iter().rev() {
        before.insert(q.clone(), *l);
    }
    let now: HashSet<&Path> = chosen.iter().map(|(q, _)| q).collect();
    let back_of = |q: &Path| before.get(q).copied().unwrap_or(Level::Card);
    let (changes, back): (Vec<(Path, Option<Level>, bool)>, Vec<(Path, Level)>) = match command {
        Command::Open_level => (chosen.iter().map(|(q, l)| (q.clone(), Some(level_up(*l)), true)).collect(), t.back.clone()),
        Command::Open_all => (chosen.iter().map(|(q, _)| (q.clone(), None, false)).collect(), t.back.clone()),
        _ => {
            let all_points = chosen.iter().all(|(_, l)| *l == Level::Point);
            if all_points {
                (
                    chosen
                        .iter()
                        .map(|(q, _)| {
                            let l = back_of(q);
                            (q.clone(), if l == Level::Card { None } else { Some(l) }, false)
                        })
                        .collect(),
                    t.back.clone(),
                )
            } else {
                let mut back: Vec<(Path, Level)> = chosen.iter().map(|(q, l)| (q.clone(), if *l == Level::Point { back_of(q) } else { *l })).collect();
                back.extend(t.back.iter().filter(|(q, _)| !now.contains(q)).cloned());
                (chosen.iter().map(|(q, _)| (q.clone(), Some(Level::Point), false)).collect(), back)
            }
        }
    };
    t.back = back;
    emit.push(Change::Level_set(changes));
}

// ---- letter hints (flow.md 7.5): [w] labels every input the selected output can connect to ----

const HINT_ALPHABET: &str = "asdfghjklqwertyuiopzxcvbnm";

fn hint_labels(n: usize) -> Vec<String> {
    let a: Vec<char> = HINT_ALPHABET.chars().collect();
    if n <= 26 {
        (0..n).map(|i| a[i].to_string()).collect()
    } else {
        (0..n.min(676)).map(|i| format!("{}{}", a[i / 26], a[i % 26])).collect()
    }
}

/// The nodes of the scope that `src` depends on: connecting from `src` to them would close a cycle.
fn upstream_of(t: &Pane, src: &p::Node) -> HashSet<Path> {
    fn all(sc: &p::Scope, out: &mut Vec<p::Node>) {
        for n in &sc.nodes {
            out.push(n.clone());
            if let Some(z) = &n.zone {
                all(&z.scope, out);
            }
        }
    }
    let mut nodes = Vec::new();
    if let Some(sc) = &t.scope {
        all(sc, &mut nodes);
    }
    let mut binders: HashMap<String, Vec<usize>> = HashMap::new();
    for (i, m) in nodes.iter().enumerate() {
        for b in &m.binds {
            binders.entry(b.clone()).or_default().push(i);
        }
    }
    let mut seen: HashSet<Path> = HashSet::new();
    fn visit(n: &p::Node, nodes: &[p::Node], binders: &HashMap<String, Vec<usize>>, seen: &mut HashSet<Path>) {
        if seen.contains(&n.path) {
            return;
        }
        seen.insert(n.path.clone());
        for r in &n.rows {
            for name in p::sources(r) {
                if let Some(l) = binders.get(root_name(&name)) {
                    for i in l.iter().rev() {
                        visit(&nodes[*i], nodes, binders, seen);
                    }
                }
            }
        }
    }
    visit(src, &nodes, &binders, &mut seen);
    seen
}

fn start_hints(t: &mut Pane, emit: &mut Vec<Change>) {
    let sel = st::selected_nodes(t);
    if sel.len() != 1 || sel[0].zone.is_some() || sel[0].synthetic {
        emit.push(Change::Notice("Select one node to connect from".to_owned()));
        return;
    }
    let src = &sel[0];
    let up = upstream_of(t, src);
    let scope = scope_of_path(&src.path);
    let (sx, sy) = st::box_at(t, &src.path).map(|(x, y, _, _)| (x, y)).unwrap_or((0.0, 0.0));
    let mut candidates: Vec<(p::Node, (f64, f64), Vec<p::Row>)> = Vec::new();
    for (pl, ax, ay) in t.geo.items.iter() {
        if let p::Item::Item(n) = &pl.item {
            if n.zone.is_none() && pl.path != src.path && scope_of_path(&pl.path) == scope && !up.contains(&n.path) {
                let lines = line_of_row(&pl.lines);
                let mut rows = Vec::new();
                for (i, r) in n.rows.iter().enumerate() {
                    if let Some(ty) = &r.ty {
                        if r.socket && r.kind != p::RowKind::Add && ty::fits(&src.ty, ty) && (r.head || lines.contains_key(&i)) {
                            rows.push(r.clone());
                        }
                    }
                }
                if !rows.is_empty() {
                    candidates.push((n.clone(), (*ax, *ay), rows));
                }
            }
        }
    }
    candidates.sort_by(|(_, (ax, ay), _), (_, (bx, by), _)| (ax - sx).hypot(ay - sy).total_cmp(&(bx - sx).hypot(by - sy)));
    candidates.truncate(676);
    let labels = hint_labels(candidates.len());
    let targets: Vec<HintTarget> = labels
        .into_iter()
        .zip(candidates.iter())
        .map(|(label, (n, at, rows))| {
            let key = if rows.len() == 1 { Some(rows[0].key.clone()) } else { None };
            HintTarget { label, node: n.path.clone(), key, pos: *at }
        })
        .collect();
    if targets.is_empty() {
        emit.push(Change::Notice("Nothing the selected output can connect to".to_owned()));
        return;
    }
    let hinting = Some(Hinting { src: src.name.clone(), src_ty: src.ty.clone(), typed: String::new(), targets, stage: None, armed: false });
    t.hinting = hinting;
}

/// A letter, Backspace or Escape while hints are up.
pub fn step_hints(t: &mut Pane, pressed: &[Key], clicked: bool, emit: &mut Vec<Change>) {
    let h = match t.hinting.clone() {
        None => return,
        Some(h) => h,
    };
    if !h.armed {
        // the key that began it
        t.hinting = Some(Hinting { armed: true, ..h });
        return;
    }
    let mut h_ref = h;
    let first = emit.len();
    let mut cancel = clicked;
    for k in pressed {
        match k {
            Key::Escape => cancel = true,
            Key::Backspace => {
                if !h_ref.typed.is_empty() {
                    let mut typed = h_ref.typed.clone();
                    typed.pop();
                    h_ref = Hinting { typed, ..h_ref };
                } else if h_ref.stage.is_some() {
                    h_ref = Hinting { stage: None, ..h_ref };
                } else {
                    cancel = true;
                }
            }
            Key::KeyChar(c) if HINT_ALPHABET.contains(c.to_ascii_lowercase()) && emit.len() == first => {
                let h = h_ref.clone();
                let typed = format!("{}{}", h.typed, c.to_ascii_lowercase());
                let live: Vec<&HintTarget> = h.targets.iter().filter(|tg| tg.label.starts_with(&typed)).collect();
                let found = live.iter().find(|tg| tg.label == typed).map(|tg| (*tg).clone());
                match found {
                    Some(HintTarget { node, key: Some(key), .. }) => {
                        emit.push(Change::Syntax_edit(Op::Connect { node, key, src: h.src.clone(), iter: false }));
                        cancel = true;
                    }
                    Some(HintTarget { node, key: None, .. }) if h.stage.is_none() => {
                        // a node with several inputs: its inputs get letters of their own
                        let n = match st::node_of(t, &node) {
                            Some(n) => n,
                            None => continue,
                        };
                        let rows: Vec<(usize, &p::Row)> = n
                            .rows
                            .iter()
                            .enumerate()
                            .filter(|(_, r)| matches!(&r.ty, Some(ty) if r.socket && r.kind != p::RowKind::Add && ty::fits(&h.src_ty, ty)))
                            .collect();
                        let labels = hint_labels(rows.len());
                        let (ax, ay) = st::box_at(t, &node).map(|(x, y, _, _)| (x, y)).unwrap_or((0.0, 0.0));
                        let lines = match t.item_at(&node) {
                            Some((pl, _, _)) => line_of_row(&pl.lines),
                            None => HashMap::new(),
                        };
                        let targets: Vec<HintTarget> = labels
                            .into_iter()
                            .zip(rows.iter())
                            .map(|(label, (i, r))| {
                                let y = if r.head {
                                    ay + 12.0
                                } else {
                                    match lines.get(i) {
                                        Some(k) => rows_top(&n, ay) + ((*k as f64 + 0.5) * p::ROW_HEIGHT),
                                        None => ay + 12.0,
                                    }
                                };
                                HintTarget { label, node: node.clone(), key: Some(r.key.clone()), pos: (ax, y) }
                            })
                            .collect();
                        h_ref = Hinting { typed: String::new(), targets, stage: Some(node), ..h };
                    }
                    _ => {
                        if !live.is_empty() {
                            h_ref = Hinting { typed, ..h };
                        }
                    }
                }
            }
            _ => {}
        }
    }
    t.hinting = if cancel { None } else { Some(h_ref) };
}

pub fn run_command(t: &mut Pane, command: Command, emit: &mut Vec<Change>) {
    match command {
        Command::Frame_all => st::frame_all(t),
        Command::Tidy => st::tidy(t),
        Command::Toggle_tags => {
            let msg = if t.tags { "wire tags off" } else { "wire tags on" };
            let tags = !t.tags;
            t.tags = tags;
            emit.push(Change::Notice(msg.to_owned()));
        }
        Command::Open_level | Command::Point_level | Command::Open_all | Command::Point_all => level_command(t, command, emit),
        Command::Show_hints => start_hints(t, emit),
        Command::Frame_selection => frame_selection(t),
        Command::Edit_name => {
            let sel = st::selected(t);
            if let [path] = sel.as_slice() {
                if matches!(st::node_of(t, path), Some(n) if !n.synthetic) {
                    t.editing = Some(Editing::Name(path.clone()));
                    return;
                }
                if input_of(t, path).is_some() {
                    t.editing = Some(Editing::Default(path.clone()));
                    return;
                }
            }
            emit.push(Change::Notice("Select one node or graph input to edit".to_owned()));
        }
        Command::Make_frame => match new_frame(t) {
            Some((scope, frame)) => {
                let mut frames = t.saved.frames(&scope);
                frames.push(frame);
                emit.push(Change::Frames_set { scope, frames });
            }
            None => emit.push(Change::Notice("Select nodes to frame".to_owned())),
        },
        Command::Walk(direction) => {
            let center = |pl: &p::Placed, ax: f64, ay: f64| (ax + (pl.w / 2.0), ay + (pl.h / 2.0));
            let sel = st::selected(t);
            let current = sel.first().and_then(|q| t.item_at(q)).cloned();
            match current {
                None => match t.geo.items.first() {
                    Some((pl, _, _)) => {
                        let path = pl.path.clone();
                        st::select(vec![path.clone()], t);
                        emit.push(Change::Selected(vec![path]));
                    }
                    None => {},
                },
                Some((cp, cx, cy)) => {
                    let (ox, oy) = center(&cp, cx, cy);
                    let mut best: Option<(f64, Path)> = None;
                    for (pl, ax, ay) in t.geo.items.iter() {
                        if pl.path == cp.path {
                            continue;
                        }
                        let (x, y) = center(pl, *ax, *ay);
                        let dx = x - ox;
                        let dy = y - oy;
                        let (along, across) = match direction {
                            Direction::Right => (dx, dy.abs()),
                            Direction::Left => (-dx, dy.abs()),
                            Direction::Down => (dy, dx.abs()),
                            Direction::Up => (-dy, dx.abs()),
                        };
                        if along <= 1.0 {
                            continue;
                        }
                        let cost = along + (2.0 * across);
                        match &best {
                            Some((c, _)) if *c <= cost => {}
                            _ => best = Some((cost, pl.path.clone())),
                        }
                    }
                    match best {
                        Some((_, path)) => { st::select(vec![path.clone()], t);
                        emit.push(Change::Selected(vec![path])); },
                        None => {},
                    }
                }
            }
        }
        _ => {
            let changes = action_changes(t, command);
            emit.extend(changes);
        }
    }
}

/// A binding and the scope command it runs (`command::Command.action` is the host's code, unused here).
pub type Cmd = (command::Command, Command);

fn make(guide: &[Ctx], id: &str, label: &str, action: Command, key: Key, modifiers: &[Key]) -> Cmd {
    (command::make(Some(Trigger::Chord(key, modifiers.to_vec())), None, guide.to_vec(), &format!("scope.{}", id), label, 0), action)
}

/// The scope's key bindings; the guide strip lists a key where it does something: on the empty canvas,
/// with one node selected, or with several.
pub fn bindings() -> Vec<Cmd> {
    let ch = Key::KeyChar;
    let any = [Ctx::Canvas, Ctx::Node, Ctx::Multi];
    let some = [Ctx::Node, Ctx::Multi];
    let one = [Ctx::Node];
    let shift = [Key::Shift];
    let mut v = vec![
        make(&some, "delete", "delete", Command::Delete, Key::Delete, &[]),
        make(&some, "delete", "delete", Command::Delete, Key::Backspace, &[]),
        make(&some, "delete", "delete", Command::Delete, ch('x'), &[]),
        make(&one, "fold", "fold into use", Command::Fold_into, ch('f'), &shift),
        make(&one, "unfold", "unfold a call", Command::Unfold, ch('u'), &shift),
        make(&one, "unfold-all", "unfold every call of the card", Command::Unfold_all, ch('u'), &[Key::Shift, Key::Alt]),
        make(&one, "hoist", "hoist out", Command::Hoist, ch('h'), &shift),
        make(&one, "bypass", "toggle bypass", Command::Bypass, ch('b'), &[]),
        make(&some, "macro", "make macro", Command::Make_macro, ch('m'), &[]),
        make(&some, "repeat", "repeat (loop)", Command::Wrap_repeat, ch('r'), &[]),
        make(&some, "iterate", "iterate (feed back)", Command::Wrap_iterate, ch('r'), &shift),
        make(&some, "conditional", "wrap conditional", Command::Wrap_if, ch('i'), &shift),
        make(&some, "map", "wrap in map", Command::Wrap_map, ch('m'), &shift),
        make(&some, "filter", "wrap in filter", Command::Wrap_filter, ch('l'), &shift),
        make(&one, "inline-fn", "inline a function call", Command::Inline_fn, ch('i'), &[Key::Shift, Key::Alt]),
        make(&one, "add-arm", "add conditional arm", Command::Add_arm, ch('a'), &[Key::Alt]),
        make(&one, "delete-arm", "delete first arm", Command::Delete_arm, Key::Delete, &[Key::Alt]),
        make(&some, "function", "make function", Command::Make_fn, ch('l'), &[]),
        make(&some, "defn", "make reusable function (defn)", Command::Make_defn, ch('d'), &[]),
        make(&some, "collapse", "collapse or expand zone", Command::Collapse, ch('c'), &[]),
        make(&any, "probe-prev", "previous iteration (every zone with nothing selected)", Command::Probe_step(-1), ch('['), &[]),
        make(&any, "probe-next", "next iteration (every zone with nothing selected)", Command::Probe_step(1), ch(']'), &[]),
        make(&any, "frame-all", "frame all", Command::Frame_all, Key::Home, &[]),
        make(&any, "tidy", "tidy: recompute every card position", Command::Tidy, ch('t'), &shift),
        make(&any, "tags", "toggle name tags on long wires", Command::Toggle_tags, ch('y'), &shift),
        make(&any, "frame-selection", "frame the selection", Command::Frame_selection, ch('f'), &[]),
        make(&one, "hints", "letter hints: connect the selected output", Command::Show_hints, ch('w'), &[]),
        make(&some, "open", "open one level", Command::Open_level, ch('o'), &[]),
        make(&some, "point", "point, or back", Command::Point_level, ch('p'), &[]),
        make(&any, "open-all", "every node to card", Command::Open_all, ch('o'), &shift),
        make(&any, "point-all", "every node to a point, or back", Command::Point_all, ch('k'), &shift),
        make(&one, "rename", "rename node / edit input default", Command::Edit_name, Key::F2, &[]),
        make(&some, "frame", "frame the selection (titled box)", Command::Make_frame, ch('g'), &shift),
        make(&some, "duplicate", "duplicate", Command::Duplicate, ch('d'), &[Key::Meta]),
        make(&some, "duplicate", "duplicate", Command::Duplicate, ch('d'), &[Key::Ctrl]),
        make(&some, "copy", "copy as text", Command::Copy, ch('c'), &[Key::Meta]),
        make(&some, "copy", "copy as text", Command::Copy, ch('c'), &[Key::Ctrl]),
        make(&some, "cut", "cut", Command::Cut, ch('x'), &[Key::Meta]),
        make(&some, "cut", "cut", Command::Cut, ch('x'), &[Key::Ctrl]),
        make(&any, "paste", "paste bindings", Command::Paste, ch('v'), &[Key::Meta]),
        make(&any, "paste", "paste bindings", Command::Paste, ch('v'), &[Key::Ctrl]),
        make(&one, "display", "view in the viewport", Command::Display, ch('v'), &[]),
        make(&one, "item-up", "move list item or chain step up, or the binding in the text", Command::Item_up, Key::ArrowUp, &[Key::Alt]),
        make(&one, "item-down", "move list item or chain step down, or the binding in the text", Command::Item_down, Key::ArrowDown, &[Key::Alt]),
    ];
    for (direction, arrow, name) in [(Direction::Left, Key::ArrowLeft, "left"), (Direction::Down, Key::ArrowDown, "down"), (Direction::Up, Key::ArrowUp, "up"), (Direction::Right, Key::ArrowRight, "right")] {
        v.push(make(&any, &format!("walk.{}", name), &format!("walk {}", name), Command::Walk(direction), arrow, &[]));
    }
    v
}
