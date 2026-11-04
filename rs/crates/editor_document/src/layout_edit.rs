//! Port of lib/editor_document/layout_edit.ml: the editor graph's layout edits (panels, splits,
//! tiles, floats and the switch of layouts) as pure rewrites of the workspace source, built on
//! `flow_graph::edit_tree`. OCaml's raised edit failures are `Err(Diagnostic)`.
use flow_check::catalog::Catalog;
use flow_check::op::Op as FlowOp;
use flow_check::workspace as w;
use flow_graph::edit_tree::*;
use flow_syntax::syntax::{self as s, Node, Syntax};
use rays::num::{fmax, fmin};
use rays_diagnostic::Diagnostic;

pub type S = Syntax;
pub type Path = w::Path;
type R<T> = Result<T, Diagnostic>;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Size {
    Ratio(f64),
    First(i64),
    Second(i64),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Axis {
    H,
    V,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Side {
    Left,
    Right,
    Top,
    Bottom,
}

#[derive(Clone, Debug)]
pub enum Op {
    /// The binding `node` of an editor graph is a `ui/split` or `ui/split-at`; it becomes
    /// `ui/split-at` with this ratio (clamped to 0.1-0.9, four decimals), or a `ui/split` with
    /// `:first_size` or `:second_size` in whole points (1 or more).
    SetLayoutSize { node: Path, size: Size },
    /// The panel `node` becomes the first half of a new `ui/split-at`; the second is `panel`.
    SplitPanel { node: Path, axis: Axis, ratio: f64, panel: S },
    /// The split holding panel `node` becomes the other half; a panel outside a split does not close.
    ClosePanel { node: Path },
    /// Move a panel beside another in the same scope; collapse the split/tile it leaves.
    DockPanel { node: Path, target: Path, side: Side },
    /// The binding `node` becomes `panel`.
    SetPanelKind { node: Path, panel: S },
    /// The editor graph `graph` has a `(ui/switch ...)`: its `:active` becomes `index`.
    SetLayout { graph: String, index: i64 },
    /// A copy of the active layout joins the switch and becomes active. Ten layouts at most.
    LayoutNew { graph: String },
    /// An older file with several `:context editor` graphs: the others join `graph`'s switch.
    MergeLayouts { graph: String },
    /// `MergeLayouts`, then `LayoutNew`, as one edit.
    MergeThenNew { graph: String },
    /// The active layout leaves the switch; the last stays.
    LayoutRemove { graph: String },
    /// `panel` in a new `(ui/floating ...)` window of the active layout.
    LayoutWindow { graph: String, panel: S },
    /// The docked panel at tree path `at` becomes a floating window, and a floating one docks.
    LayoutFloat { graph: String, at: Vec<i64> },
}

/// A split ratio clamped to [0.1, 0.9] with four decimals, trailing zeros trimmed.
fn ratio_text(r: f64) -> String {
    let mut t = cfmt::f(fmax(0.1, fmin(0.9, r)), 4);
    while t.ends_with('0') && !t.ends_with(".0") {
        t.pop();
    }
    t
}

const CONTAINERS: [&str; 4] = ["ui/split", "ui/split-at", "ui/tile", "ui/floating"];

fn is_container(h: &str) -> bool {
    CONTAINERS.contains(&h)
}

fn layout_binding(e: &S) -> bool {
    match head_sym(e) {
        Some(h) => is_container(h),
        None => matches!(e.node, Node::Sym(_)),
    }
}

fn binding_of(ps: &[(S, S)], n: &str) -> Option<S> {
    ps.iter().find(|(p, _)| pat_key(p) == n).map(|(_, e)| e.clone())
}

fn args_of(e: &S) -> Vec<S> {
    match &e.node {
        Node::List(items) if !items.is_empty() => items[1..].to_vec(),
        _ => Vec::new(),
    }
}

fn children_of(e: &S) -> Vec<S> {
    s::children(e).into_iter().cloned().collect()
}

/// A layout as one expression: the splits, tiles and floats it names are copied in; panels stay shared.
fn solid(ps: &[(S, S)], e: &S) -> S {
    match &e.node {
        Node::Sym(n) => match binding_of(ps, n) {
            Some(b) if layout_binding(&b) => solid(ps, &b),
            _ => e.clone(),
        },
        Node::List(items) if matches!(items.first().map(|h| &h.node), Some(Node::Sym(h)) if is_container(h)) => {
            map_children(&mut |c| solid(ps, c), e)
        }
        _ => e.clone(),
    }
}

/// The layout bindings a layout reads, through the ones it names.
fn reach(ps: &[(S, S)], e: &S) -> Vec<String> {
    let mut out = Vec::new();
    for n in dedup(&sym_list(e)) {
        if let Some(b) = binding_of(ps, &n) {
            if layout_binding(&b) {
                out.push(n);
                out.extend(reach(ps, &b));
            }
        }
    }
    out
}

fn layout_kids(e: &S) -> Vec<S> {
    match head_sym(e) {
        Some("ui/split" | "ui/split-at") => {
            let pos = positional(&args_of(e));
            if pos.len() >= 2 { vec![pos[pos.len() - 2].clone(), pos[pos.len() - 1].clone()] } else { Vec::new() }
        }
        Some("ui/tile" | "ui/floating") => positional(&args_of(e)),
        _ => Vec::new(),
    }
}

/// Where child `i` sits among the positional arguments.
fn kid_pos(e: &S, i: i64) -> i64 {
    match head_sym(e) {
        Some("ui/split" | "ui/split-at") => positional(&args_of(e)).len() as i64 - 2 + i,
        _ => i,
    }
}

fn strip(e: &S, removed: &mut Vec<String>) -> R<Option<S>> {
    match &e.node {
        Node::Sym(name) if removed.contains(name) => Ok(None),
        Node::List(items) if matches!(items.first().map(|h| &h.node), Some(Node::Sym(h)) if h == "ui/split" || h == "ui/split-at") => {
            match layout_kids(e).as_slice() {
                [a, b] => {
                    let sa = strip(a, removed)?;
                    let sb = strip(b, removed)?;
                    match (sa, sb) {
                        (None, other) => Ok(other),
                        (other, None) => Ok(other),
                        (Some(a), Some(b)) => {
                            let e1 = arg_set(e, &ArgKey::Pos(kid_pos(e, 0)), Some(a))?;
                            Ok(Some(arg_set(&e1, &ArgKey::Pos(kid_pos(e, 1)), Some(b))?))
                        }
                    }
                }
                _ => Ok(Some(e.clone())),
            }
        }
        Node::List(items) if matches!(items.first().map(|h| &h.node), Some(Node::Sym(h)) if h == "ui/tile") => {
            let mut cells = Vec::new();
            for c in &items[1..] {
                if let Some(c) = strip(c, removed)? {
                    cells.push(c);
                }
            }
            if cells.is_empty() {
                Ok(None)
            } else {
                let mut v = vec![items[0].clone()];
                v.extend(cells);
                Ok(Some(S { node: Node::List(v.into()), ..e.clone() }))
            }
        }
        Node::List(items) if items.len() == 2 && matches!(&items[0].node, Node::Sym(h) if h == "ui/floating" || h == "ui/workspace") => {
            Ok(strip(&items[1], removed)?.map(|child| S { node: Node::List(vec![items[0].clone(), child].into()), ..e.clone() }))
        }
        Node::List(items) if matches!(items.first().map(|h| &h.node), Some(Node::Sym(h)) if h == "ui/switch") => {
            let mut v = vec![items[0].clone()];
            for child in &items[1..] {
                match strip(child, removed)? {
                    Some(c) => v.push(c),
                    None => return Err(fail("Keep at least one panel in every layout.")),
                }
            }
            Ok(Some(S { node: Node::List(v.into()), ..e.clone() }))
        }
        _ => Ok(Some(e.clone())),
    }
}

/// Remove a named panel from every layout wrapper. Docking keeps its binding for reinsertion;
/// closing drops it. Empty wrapper bindings disappear too.
fn strip_panel(keep: bool, sc: &Scope, leaf: &str) -> R<(Vec<(S, S)>, S, Vec<String>)> {
    let mut removed = vec![leaf.to_owned()];
    let mut ps = sc.ps.clone();
    loop {
        let before = removed.len();
        let mut next = Vec::new();
        for (p, e) in &ps {
            if pat_key(p) == leaf {
                if keep {
                    next.push((p.clone(), e.clone()));
                }
            } else {
                match strip(e, &mut removed)? {
                    Some(e) => next.push((p.clone(), e)),
                    None => removed.insert(0, pat_key(p)),
                }
            }
        }
        ps = next;
        if before == removed.len() {
            break;
        }
    }
    let res = strip(&sc.res, &mut removed)?.ok_or_else(|| fail("Keep at least one docked panel."))?;
    Ok((ps, res, removed))
}

fn drop_kid(e: &S, i: i64) -> R<Option<S>> {
    match head_sym(e) {
        Some("ui/split" | "ui/split-at") => Ok(layout_kids(e).get((1 - i) as usize).cloned()),
        Some("ui/tile") => {
            if layout_kids(e).len() == 1 { Ok(None) } else { Ok(Some(arg_set(e, &ArgKey::Pos(i), None)?)) }
        }
        _ => Ok(None),
    }
}

/// The layout without the panel at a tree path, and that panel.
fn take(path: &[i64], e: &S) -> R<(Option<S>, S)> {
    match path.split_first() {
        None => Err(fail("A layout keeps its last panel.")),
        Some((&i, rest)) => {
            let kid = layout_kids(e).get(i as usize).cloned().ok_or_else(|| fail("That panel is not in this layout."))?;
            if rest.is_empty() {
                Ok((drop_kid(e, i)?, kid))
            } else {
                match take(rest, &kid)? {
                    (Some(k), taken) => Ok((Some(arg_set(e, &ArgKey::Pos(kid_pos(e, i)), Some(k))?), taken)),
                    (None, taken) => Ok((drop_kid(e, i)?, taken)),
                }
            }
        }
    }
}

fn node_at(path: &[i64], e: &S) -> R<S> {
    let mut e = e.clone();
    for &i in path {
        e = layout_kids(&e).get(i as usize).cloned().ok_or_else(|| fail("That panel is not in this layout."))?;
    }
    Ok(e)
}

fn has_docked(e: &S) -> bool {
    match head_sym(e) {
        Some("ui/floating") => false,
        Some("ui/split" | "ui/split-at" | "ui/tile") => layout_kids(e).iter().any(has_docked),
        _ => true,
    }
}

/// The `(ui/workspace ...)` an editor graph returns: its result, or the binding its result names.
fn workspace_call(sc: &Scope) -> R<S> {
    let err = || fail("This editor graph does not return a (ui/workspace ...).");
    match &sc.res.node {
        Node::Sym(n) => match binding_of(&sc.ps, n) {
            Some(b) if head_sym(&b) == Some("ui/workspace") => Ok(b),
            _ => Err(err()),
        },
        _ if head_sym(&sc.res) == Some("ui/workspace") => Ok(sc.res.clone()),
        _ => Err(err()),
    }
}

fn editor_scope(sx: &S) -> R<Scope> {
    let sc = ensure(sx);
    workspace_call(&sc)?;
    Ok(sc)
}

fn workspace_arg(sc: &Scope) -> R<S> {
    arg_get(&workspace_call(sc)?, &ArgKey::Pos(0)).ok_or_else(|| fail("The workspace holds no panels."))
}

/// The scope with `root` as the workspace's panel, written where the call is.
fn set_workspace(sc: &Scope, ps: &[(S, S)], root: S) -> R<Scope> {
    let call = arg_set(&workspace_call(sc)?, &ArgKey::Pos(0), Some(root))?;
    Ok(match &sc.res.node {
        Node::Sym(n) => Scope {
            ps: ps.iter().map(|(p, e)| if pat_key(p) == *n { (p.clone(), call.clone()) } else { (p.clone(), e.clone()) }).collect(),
            ..sc.clone()
        },
        _ => Scope { ps: ps.to_vec(), res: call, ..sc.clone() },
    })
}

fn rebuilt(sc: &Scope) -> S {
    rebuild(sc, &sc.ps, sc.res.clone())
}

fn is_switch(e: &S) -> bool {
    head_sym(e) == Some("ui/switch")
}

type Place = Option<(Option<String>, S)>;

/// The first switch of the layout: the root itself, or one nested in its splits, tiles and floats
/// (bound to a name, or written in place); the binding holding it, or none when written in place.
fn switch_place(sc: &Scope) -> R<Place> {
    fn find(sc: &Scope, top: bool, e: &S) -> Place {
        match &e.node {
            Node::Sym(n) => match binding_of(&sc.ps, n) {
                Some(b) if is_switch(&b) => Some((Some(n.clone()), b)),
                Some(b) if layout_binding(&b) => find(sc, false, &b),
                _ => None,
            },
            _ if is_switch(e) => {
                if top { Some((None, e.clone())) } else { None }
            }
            _ => match head_sym(e) {
                Some(h) if is_container(h) => layout_kids(e).iter().find_map(|k| find(sc, top, k)),
                _ => None,
            },
        }
    }
    Ok(find(sc, true, &workspace_arg(sc)?))
}

/// The first switch written in place below the root's splits, tiles and floats.
fn replace_inline_switch(sw: &S, root: &S) -> S {
    fn go(e: &S, found: &mut bool, sw: &S) -> S {
        if *found {
            e.clone()
        } else if is_switch(e) {
            *found = true;
            keep_notes(e, sw.clone())
        } else {
            match head_sym(e) {
                Some(h) if is_container(h) => map_children(&mut |c| go(c, found, sw), e),
                _ => e.clone(),
            }
        }
    }
    let mut found = false;
    go(root, &mut found, sw)
}

fn set_switch(sc: &Scope, ps: &[(S, S)], place: &Option<String>, sw: &S) -> R<S> {
    match place {
        None => {
            let root = replace_inline_switch(sw, &workspace_arg(sc)?);
            Ok(rebuilt(&set_workspace(sc, ps, root)?))
        }
        Some(n) => Ok(rebuild(
            sc,
            &ps.iter().map(|(p, e)| if pat_key(p) == *n { (p.clone(), keep_notes(e, sw.clone())) } else { (p.clone(), e.clone()) }).collect::<Vec<_>>(),
            sc.res.clone(),
        )),
    }
}

fn active_of(args: &[S]) -> R<i64> {
    match kw_get(args, "active") {
        None => Ok(0),
        Some(S { node: Node::Num(t), .. }) => Ok(t.parse::<i64>().unwrap_or(0)),
        Some(_) => Err(fail("The active layout is an expression: change it in the text.")),
    }
}

/// The switch's active input; an `:active` outside its layouts is refused.
fn active_layout(args: &[S]) -> R<S> {
    let a = active_of(args)?;
    let found = if a < 0 { None } else { positional(args).get(a as usize).cloned() };
    found.ok_or_else(|| fail("The active layout is not one of the switch's layouts."))
}

/// Layout bindings in `cands` that nothing reads any more go.
fn prune(body: &S, cands: &[String]) -> S {
    let Some(b) = scope_of(body) else { return body.clone() };
    let used = |ps: &[(S, S)], n: &str| count_refs(n, &b.res) > 0 || ps.iter().any(|(p, e)| pat_key(p) != n && count_refs(n, e) > 0);
    let mut ps = b.ps.clone();
    loop {
        let (dead, kept): (Vec<_>, Vec<_>) =
            ps.iter().cloned().partition(|(p, e)| cands.contains(&pat_key(p)) && layout_binding(e) && !used(&ps, &pat_key(p)));
        if dead.is_empty() {
            break;
        }
        ps = kept;
    }
    collapse(&b, &ps, b.res.clone())
}

/// `f ps layout` rewrites the active layout: the switch's active input, else the workspace's tree.
fn edit_layout(src: &[S], graph: &str, f: &dyn Fn(&[(S, S)], &S) -> R<S>) -> R<Vec<S>> {
    edit_scope(src, &[graph.to_owned()], &mut |sx| {
        let sc = editor_scope(sx)?;
        match switch_place(&sc)? {
            Some((place, sw)) => {
                let args = args_of(&sw);
                let a = active_of(&args)?;
                let slot = active_layout(&args)?;
                let new = f(&sc.ps, &slot)?;
                let put = set_switch(&sc, &sc.ps, &place, &arg_set(&sw, &ArgKey::Pos(a), Some(new))?)?;
                Ok(prune(&put, &reach(&sc.ps, &slot)))
            }
            None => {
                let slot = workspace_arg(&sc)?;
                let new = f(&sc.ps, &slot)?;
                let put = rebuilt(&set_workspace(&sc, &sc.ps, new)?);
                Ok(prune(&put, &reach(&sc.ps, &slot)))
            }
        }
    })
}

fn str_node(t: &str) -> S {
    mk(Node::Str(t.as_bytes().to_vec()))
}

fn num_node(t: &str) -> S {
    mk(Node::Num(t.to_owned()))
}

/// A workspace without a switch gets one around its tree.
fn ensure_switch(sc: &Scope, used: &mut Vec<String>) -> R<(Scope, Option<String>, S)> {
    match switch_place(sc)? {
        Some((place, sw)) => Ok((sc.clone(), place, sw)),
        None => {
            let tree = workspace_arg(sc)?;
            let (mut ps, kid) = match &tree.node {
                Node::Sym(_) => (sc.ps.clone(), tree.clone()),
                _ => {
                    let n = fresh(used, "layout");
                    let mut ps = sc.ps.clone();
                    ps.push((sym(&n), tree.clone()));
                    (ps, sym(&n))
                }
            };
            let name = fresh(used, "switch");
            let sw = call("ui/switch", vec![kid]);
            ps.push((sym(&name), sw.clone()));
            Ok((set_workspace(sc, &ps, sym(&name))?, Some(name), sw))
        }
    }
}

fn editor_graph(i: &S) -> bool {
    if let Node::List(items) = &i.node {
        if let [h, n, rest @ ..] = items.as_slice() {
            if matches!(&h.node, Node::Sym(h) if h == "graph") && matches!(n.node, Node::Sym(_)) {
                let mut k = 0;
                while k < rest.len() {
                    if let (Node::Kw(c), Some(v)) = (&rest[k].node, rest.get(k + 1)) {
                        if c == "context" && matches!(&v.node, Node::Sym(e) if e == "editor") {
                            return true;
                        }
                    }
                    k += 1;
                }
            }
        }
    }
    false
}

fn rewrite(src: &[S], op: &Op) -> R<Vec<S>> {
    match op {
        Op::MergeThenNew { graph } => {
            let merged = rewrite(src, &Op::MergeLayouts { graph: graph.clone() })?;
            rewrite(&merged, &Op::LayoutNew { graph: graph.clone() })
        }
        Op::SetLayoutSize { node, size } => {
            let (sp, leaf) = split_node(node)?;
            edit_scope(src, &sp, &mut |sx| {
                let e = get_node(sx, &leaf)?;
                let not_split = || fail("That panel is not a split.");
                let (axis, a, b) = match head_sym(&e) {
                    Some("ui/split" | "ui/split-at") => match (positional(&args_of(&e)).first(), layout_kids(&e).as_slice()) {
                        (Some(axis), [a, b]) => (axis.clone(), a.clone(), b.clone()),
                        _ => return Err(not_split()),
                    },
                    _ => return Err(not_split()),
                };
                // one side fixed in whole points, or a ratio with four decimals
                let fixed = |key: &str, n: i64| call("ui/split", vec![axis.clone(), a.clone(), b.clone(), kwf(key), num(n.max(1))]);
                let new = match size {
                    Size::First(n) => fixed("first_size", *n),
                    Size::Second(n) => fixed("second_size", *n),
                    Size::Ratio(r) => call("ui/split-at", vec![axis.clone(), num_node(&ratio_text(*r)), a.clone(), b.clone()]),
                };
                set_node(sx, &leaf, keep_notes(&e, new))
            })
        }
        Op::SplitPanel { node, axis, ratio, panel } => {
            let (sp, leaf) = split_node(node)?;
            let mut used = root_used(src, &sp[0]);
            edit_scope(src, &sp, &mut |sx| {
                let sc = scope_of(sx).ok_or_else(|| fail("This editor graph is a single expression. Edit it in Lisp."))?;
                let j = first_pair_index(&sc, &leaf)?;
                let a = fresh(&mut used, &format!("{leaf}_a"));
                let b = fresh(&mut used, &format!("{leaf}_b"));
                let (p, orig) = sc.ps[j].clone();
                let split = call(
                    "ui/split-at",
                    vec![str_node(if *axis == Axis::H { "horizontal" } else { "vertical" }), num_node(&ratio_text(*ratio)), sym(&a), sym(&b)],
                );
                let mut ps: Vec<(S, S)> = sc.ps[..j].to_vec();
                ps.push((sym(&a), orig));
                ps.push((sym(&b), panel.clone()));
                ps.push((p, split));
                ps.extend(sc.ps[j + 1..].iter().cloned());
                reorder(&rebuild(&sc, &ps, sc.res.clone()))
            })
        }
        Op::ClosePanel { node } => {
            let (sp, leaf) = split_node(node)?;
            edit_scope(src, &sp, &mut |sx| {
                let sc = scope_of(sx).ok_or_else(|| fail("This editor graph is a single expression. Edit it in Lisp."))?;
                get_node(sx, &leaf)?;
                let (ps, res, _) = strip_panel(false, &sc, &leaf)?;
                reorder(&rebuild(&sc, &ps, res))
            })
        }
        Op::DockPanel { node, target, side } => {
            let (sp, leaf) = split_node(node)?;
            let (tp, target) = split_node(target)?;
            if sp != tp || leaf == target {
                return Err(fail("Dock two different panels of the same layout."));
            }
            let mut used = root_used(src, &sp[0]);
            edit_scope(src, &sp, &mut |sx| {
                let sc = ensure(sx);
                get_node(sx, &leaf)?;
                get_node(sx, &target)?;
                let (ps, res, removed) = strip_panel(true, &sc, &leaf)?;
                if removed.contains(&target) {
                    return Err(fail("A panel cannot dock inside its own group."));
                }
                let group = fresh(&mut used, &format!("{target}_dock"));
                let first = matches!(side, Side::Left | Side::Top);
                let split = call(
                    "ui/split-at",
                    vec![
                        str_node(if matches!(side, Side::Top | Side::Bottom) { "vertical" } else { "horizontal" }),
                        num_node("0.5"),
                        sym(if first { &leaf } else { &target }),
                        sym(if first { &target } else { &leaf }),
                    ],
                );
                let mut ps: Vec<(S, S)> =
                    ps.iter().map(|(p, e)| (p.clone(), if pat_key(p) == target { e.clone() } else { rename_place(&target, &group, e) })).collect();
                ps.push((sym(&group), split));
                reorder(&rebuild(&sc, &ps, rename_place(&target, &group, &res)))
            })
        }
        Op::SetPanelKind { node, panel } => {
            let (sp, leaf) = split_node(node)?;
            edit_scope(src, &sp, &mut |sx| set_node(sx, &leaf, panel.clone()))
        }
        Op::SetLayout { graph, index } => edit_scope(src, &[graph.clone()], &mut |sx| {
            let sc = editor_scope(sx)?;
            match switch_place(&sc)? {
                None => Err(fail("This workspace has one layout and no switch.")),
                Some((place, sw)) => {
                    if *index < 0 || *index >= positional(&args_of(&sw)).len() as i64 {
                        return Err(fail(&format!("There is no layout {index}.")));
                    }
                    set_switch(&sc, &sc.ps, &place, &arg_set(&sw, &ArgKey::Kw("active".to_owned()), Some(num(*index)))?)
                }
            }
        }),
        Op::LayoutNew { graph } => {
            let mut used = root_used(src, graph);
            edit_scope(src, &[graph.clone()], &mut |sx| {
                let sc = editor_scope(sx)?;
                let (sc, place, sw) = ensure_switch(&sc, &mut used)?;
                let args = args_of(&sw);
                let n = positional(&args).len() as i64;
                if n >= 10 {
                    return Err(fail("Ten layouts is the limit of the digit keys."));
                }
                let copy = solid(&sc.ps, &active_layout(&args)?);
                let name = fresh(&mut used, "layout");
                let sw = arg_set(&arg_set(&sw, &ArgKey::Pos(n), Some(sym(&name)))?, &ArgKey::Kw("active".to_owned()), Some(num(n)))?;
                let mut ps = sc.ps.clone();
                ps.push((sym(&name), copy));
                reorder(&set_switch(&sc, &ps, &place, &sw)?)
            })
        }
        Op::MergeLayouts { graph } => {
            // an older file: the other `:context editor` graphs become layouts of a switch in
            // `graph`; each brings its bindings, renamed `graph_name` so they cannot clash, and then goes
            let (_, items) = workspace_parts(src)?;
            let others: Vec<(String, S)> = items
                .iter()
                .filter_map(|i| match root_name(i) {
                    Some(n) if n != *graph && editor_graph(i) => Some((n, i.clone())),
                    _ => None,
                })
                .collect();
            if others.is_empty() {
                return Err(fail("There is one editor graph and nothing to merge."));
            }
            let mut used = root_used(src, graph);
            let merged = edit_scope(src, &[graph.clone()], &mut |sx| {
                let sc = editor_scope(sx)?;
                let (sc, place, mut sw) = ensure_switch(&sc, &mut used)?;
                let mut ps = sc.ps.clone();
                for (name, item) in &others {
                    let theirs = editor_scope(&last_child(item)?)?;
                    let names: Vec<(String, String)> =
                        theirs.ps.iter().map(|(p, _)| (pat_key(p), fresh(&mut used, &format!("{name}_{}", pat_key(p))))).collect();
                    let rename = |e: &S| names.iter().fold(e.clone(), |e, (old, nw)| rename_ref(old, nw, &e));
                    let own: Vec<(S, S)> = theirs
                        .ps
                        .iter()
                        .map(|(p, e)| {
                            let key = pat_key(p);
                            let nw = names.iter().find(|(k, _)| *k == key).map(|(_, v)| v.clone()).unwrap_or(key);
                            (sym(&nw), rename(e))
                        })
                        .collect();
                    let tree = rename(&workspace_arg(&theirs)?);
                    let kid = match &tree.node {
                        Node::Sym(_) => {
                            ps.extend(own);
                            tree.clone()
                        }
                        _ => {
                            let n = fresh(&mut used, name);
                            ps.extend(own);
                            ps.push((sym(&n), tree.clone()));
                            sym(&n)
                        }
                    };
                    let count = positional(&args_of(&sw)).len() as i64;
                    if count >= 10 {
                        return Err(fail("Ten layouts is the limit of the digit keys."));
                    }
                    sw = arg_set(&sw, &ArgKey::Pos(count), Some(kid))?;
                }
                reorder(&set_switch(&sc, &ps, &place, &sw)?)
            })?;
            map_items(&merged, &mut |items| {
                Ok(items.into_iter().filter(|i| match root_name(i) {
                    Some(n) => !others.iter().any(|(o, _)| *o == n),
                    None => true,
                }).collect())
            })
        }
        Op::LayoutRemove { graph } => edit_scope(src, &[graph.clone()], &mut |sx| {
            let sc = editor_scope(sx)?;
            match switch_place(&sc)? {
                None => Err(fail("This workspace has one layout and no switch.")),
                Some((place, sw)) => {
                    let args = args_of(&sw);
                    let n = positional(&args).len() as i64;
                    let a = active_of(&args)?;
                    if n < 2 {
                        return Err(fail("A switch keeps its last layout."));
                    }
                    let gone = active_layout(&args)?;
                    let sw = arg_set(&arg_set(&sw, &ArgKey::Pos(a), None)?, &ArgKey::Kw("active".to_owned()), Some(num(a.min(n - 2))))?;
                    Ok(prune(&set_switch(&sc, &sc.ps, &place, &sw)?, &reach(&sc.ps, &gone)))
                }
            }
        }),
        Op::LayoutWindow { graph, panel } => edit_layout(src, graph, &|_, layout| {
            Ok(call("ui/split-at", vec![str_node("horizontal"), num_node("0.5"), layout.clone(), call("ui/floating", vec![panel.clone()])]))
        }),
        Op::LayoutFloat { graph, at } => edit_layout(src, graph, &|ps, layout| {
            let tree = solid(ps, layout);
            let up: Vec<i64> = if at.is_empty() { Vec::new() } else { at[..at.len() - 1].to_vec() };
            if !up.is_empty() && head_sym(&node_at(&up, &tree)?) == Some("ui/floating") {
                // a window docks beside the rest
                let first = |window: &S| layout_kids(window).first().cloned().ok_or_else(|| fail("A floating window holds no panel."));
                match take(&up, &tree)? {
                    (Some(rest), window) => Ok(call("ui/split-at", vec![str_node("horizontal"), num_node("0.7"), rest, first(&window)?])),
                    (None, window) => first(&window),
                }
            } else {
                match take(at, &tree)? {
                    (Some(rest), panel) if has_docked(&rest) => {
                        Ok(call("ui/split-at", vec![str_node("horizontal"), num_node("0.5"), rest, call("ui/floating", vec![panel])]))
                    }
                    _ => Err(fail("A layout keeps one docked panel.")),
                }
            }
        }),
    }
}

/// The history label: "Split panel", "New layout", ...
pub fn label(op: &Op) -> &'static str {
    match op {
        Op::SetLayoutSize { .. } => "Resize panel",
        Op::SplitPanel { .. } => "Split panel",
        Op::ClosePanel { .. } => "Close panel",
        Op::SetPanelKind { .. } => "Retype panel",
        Op::DockPanel { .. } => "Dock panel",
        Op::SetLayout { .. } => "Layout",
        Op::MergeLayouts { .. } | Op::MergeThenNew { .. } => "Merge layouts",
        Op::LayoutNew { .. } => "New layout",
        Op::LayoutRemove { .. } => "Remove layout",
        Op::LayoutWindow { .. } => "New window",
        Op::LayoutFloat { .. } => "Float panel",
    }
}

/// A history merge key for an edit that repeats (`SetLayout`), `None` for the others.
pub fn gesture(op: &Op) -> Option<&'static str> {
    match op {
        Op::SetLayout { .. } => Some("layout"),
        _ => None,
    }
}

/// The paths the edit reads, for the read-only imported-form check.
pub fn paths(op: &Op) -> Vec<Path> {
    match op {
        Op::SetLayoutSize { node, .. } | Op::SplitPanel { node, .. } | Op::ClosePanel { node } | Op::SetPanelKind { node, .. } => vec![node.clone()],
        Op::DockPanel { node, target, .. } => vec![node.clone(), target.clone()],
        Op::SetLayout { graph, .. }
        | Op::LayoutNew { graph }
        | Op::MergeLayouts { graph }
        | Op::MergeThenNew { graph }
        | Op::LayoutRemove { graph }
        | Op::LayoutWindow { graph, .. }
        | Op::LayoutFloat { graph, .. } => vec![vec![graph.clone()]],
    }
}

/// The edit folds the other editor graphs into one: their layout keys are gone.
pub fn merges(op: &Op) -> bool {
    matches!(op, Op::MergeLayouts { .. } | Op::MergeThenNew { .. })
}

/// Where a layout key goes after the edit: a closed panel's key goes (`None`), the rest stay.
pub fn remap(op: &Op, p: &Path) -> Option<Path> {
    match op {
        Op::ClosePanel { node } if has_prefix(node, p) => None,
        _ => Some(p.clone()),
    }
}

/// The rewritten source (printed and parsed again) and its check.
pub fn apply_checked(
    ops: &[FlowOp],
    library: bool,
    checker: Option<&flow_graph::flow_edit::Checker>,
    catalog: &Catalog,
    src: &[S],
    op: &Op,
) -> R<(Vec<S>, w::Workspace)> {
    apply_rewrite(ops, library, checker, catalog, &mut || rewrite(src, op))
}

/// The edit adds or removes nodes, so layout keys are pruned after it (a resize does not).
pub fn changes_nodes(op: &Op) -> bool {
    !matches!(op, Op::SetLayoutSize { .. })
}
