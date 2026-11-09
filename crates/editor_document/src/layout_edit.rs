//! Port of lib/editor_document/layout_edit.ml: the editor graph's layout edits (panels, splits,
//! tiles, floats and the switch of layouts) as pure rewrites of the workspace source, built on
//! `flow_graph::edit_tree`. OCaml's raised edit failures are `Err(Diagnostic)`.
use flow_check::catalog::Catalog;
use flow_check::op::Op as FlowOp;
use flow_check::workspace as w;
use flow_graph::edit_tree::*;
use flow_syntax::syntax::Syntax;
use rays::num::{fmax, fmin};
use rays_diagnostic::Diagnostic;

type S = SyntaxId;
pub type Path = w::Path;
type R<T> = Result<T, Diagnostic>;

pub use editor_core::panels::{Axis, Side, Size};

#[derive(Clone, Debug)]
pub enum Op {
    /// The binding `node` of an editor graph is a `ui/split` or `ui/split-at`; it becomes
    /// `ui/split-at` with this ratio (clamped to 0.1-0.9, four decimals), or a `ui/split` with
    /// `:first_size` or `:second_size` in whole points (1 or more).
    SetLayoutSize { node: Path, size: Size },
    /// The panel `node` becomes the first half of a new `ui/split-at`; the second is `panel`.
    SplitPanel { node: Path, axis: Axis, ratio: f64, panel: Syntax },
    /// The split holding panel `node` becomes the other half; a panel outside a split does not close.
    ClosePanel { node: Path },
    /// Move a panel beside another in the same scope; collapse the split/tile it leaves.
    DockPanel { node: Path, target: Path, side: Side },
    /// The binding `node` becomes `panel`.
    SetPanelKind { node: Path, panel: Syntax },
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
    LayoutWindow { graph: String, panel: Syntax },
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

fn layout_binding(e: &S, builder: &SyntaxBuilder) -> bool {
    match head_sym(e, builder) {
        Some(head) => is_container(head),
        None => matches!(node_of(e, builder), Node::Sym(_)),
    }
}

fn binding_of(ps: &[(S, S)], name: &str, builder: &SyntaxBuilder) -> Option<S> {
    ps.iter().find(|(pattern, _)| pat_key(pattern, builder) == name).map(|(_, value)| *value)
}

fn args_of<'a>(e: &S, builder: &'a SyntaxBuilder) -> &'a [S] {
    if matches!(node_of(e, builder), Node::List(_)) { children(e, builder).get(1..).unwrap_or(&[]) } else { &[] }
}

fn positions<'a>(args: &'a [S], builder: &'a SyntaxBuilder) -> impl Iterator<Item = S> + 'a {
    let mut skip = false;
    args.iter().enumerate().filter_map(move |(index, id)| {
        if skip { skip = false; return None; }
        if index + 1 < args.len() && matches!(node_of(id, builder), Node::Kw(_)) { skip = true; None }
        else { Some(*id) }
    })
}

/// A layout as one expression: the splits, tiles and floats it names are copied in; panels stay shared.
fn solid(ps: &[(S, S)], e: &S, builder: &mut SyntaxBuilder) -> S {
    match node_of(e, builder) {
        Node::Sym(name) => match binding_of(ps, builder.name(name), builder) {
            Some(binding) if layout_binding(&binding, builder) => solid(ps, &binding, builder),
            _ => *e,
        },
        Node::List(_) if head_sym(e, builder).is_some_and(is_container) => map_children(&mut |child, builder| solid(ps, child, builder), e, builder),
        _ => *e,
    }
}

/// The layout bindings a layout reads, through the ones it names.
fn reach(ps: &[(S, S)], e: &S, builder: &SyntaxBuilder) -> Vec<String> { let mut out = Vec::new();
for n in dedup(&sym_list(e, builder)) {
    if let Some(b) = binding_of(ps, &n, builder) {
        if layout_binding(&b, builder) {
            out.push(n);
            out.extend(reach(ps, &b, builder));
        }
    }
}
out }

fn layout_kids<'a>(e: &S, builder: &'a SyntaxBuilder) -> impl Iterator<Item = S> + 'a {
    let args = args_of(e, builder);
    let (start, count) = match head_sym(e, builder) {
        Some("ui/split" | "ui/split-at") => {
            let count = positions(args, builder).count();
            if count >= 2 { (count - 2, 2) } else { (0, 0) }
        }
        Some("ui/tile" | "ui/floating") => (0, usize::MAX),
        _ => (0, 0),
    };
    positions(args, builder).skip(start).take(count)
}

/// Where child `i` sits among the positional arguments.
fn kid_pos(e: &S, i: i64, builder: &SyntaxBuilder) -> i64 {
    match head_sym(e, builder) {
        Some("ui/split" | "ui/split-at") => positions(args_of(e, builder), builder).count() as i64 - 2 + i,
        _ => i,
    }
}

fn strip(e: &S, removed: &mut Vec<String>, builder: &mut SyntaxBuilder) -> R<Option<S>> {
    match node_of(e, builder) {
        Node::Sym(name) if removed.iter().any(|removed| removed == builder.name(name)) => Ok(None),
        Node::List(_) if matches!(head_sym(e, builder), Some("ui/split" | "ui/split-at")) => {
            let pair = { let mut kids = layout_kids(e, builder); (kids.next(), kids.next()) };
            match pair {
                (Some(a), Some(b)) => {
                    let a = strip(&a, removed, builder)?;
                    let b = strip(&b, removed, builder)?;
                    match (a, b) {
                        (None, other) | (other, None) => Ok(other),
                        (Some(a), Some(b)) => {
                            let first = arg_set(e, &ArgKey::Pos(kid_pos(e, 0, builder)), Some(a), builder)?;
                            Ok(Some(arg_set(&first, &ArgKey::Pos(kid_pos(e, 1, builder)), Some(b), builder)?))
                        }
                    }
                }
                _ => Ok(Some(*e)),
            }
        }
        Node::List(_) if head_sym(e, builder) == Some("ui/tile") => {
            let mut cells = Vec::new();
            let count = children(e, builder).len();
            for index in 1..count {
                let child = children(e, builder)[index];
                if let Some(child) = strip(&child, removed, builder)? { cells.push(child); }
            }
            if cells.is_empty() { Ok(None) } else {
                let mut items = vec![children(e, builder)[0]];
                items.extend(cells);
                Ok(Some(with_children(e, items, builder)))
            }
        }
        Node::List(_) if children(e, builder).len() == 2 && matches!(head_sym(e, builder), Some("ui/floating" | "ui/workspace")) => {
            let (head, child) = (children(e, builder)[0], children(e, builder)[1]);
            Ok(strip(&child, removed, builder)?.map(|child| with_children(e, vec![head, child], builder)))
        }
        Node::List(_) if head_sym(e, builder) == Some("ui/switch") => {
            let count = children(e, builder).len();
            let mut items = vec![children(e, builder)[0]];
            for index in 1..count {
                let child = children(e, builder)[index];
                match strip(&child, removed, builder)? {
                    Some(child) => items.push(child),
                    None => return Err(fail("Keep at least one panel in every layout.")),
                }
            }
            Ok(Some(with_children(e, items, builder)))
        }
        _ => Ok(Some(*e)),
    }
}

/// Remove a named panel from every layout wrapper. Docking keeps its binding for reinsertion;
/// closing drops it. Empty wrapper bindings disappear too.
fn strip_panel(keep: bool, sc: &Scope, leaf: &str, builder: &mut SyntaxBuilder) -> R<(Vec<(S, S)>, S, Vec<String>)> { let mut removed = vec![leaf.to_owned()];
let mut ps = sc.ps.clone();
loop {
    let before = removed.len();
    let mut next = Vec::new();
    for (p, e) in &ps {
        if pat_key(p, builder) == leaf {
            if keep {
                next.push((p.clone(), e.clone()));
            }
        } else {
            match strip(e, &mut removed, builder)? {
                Some(e) => next.push((p.clone(), e)),
                None => removed.insert(0, pat_key(p, builder)),
            }
        }
    }
    ps = next;
    if before == removed.len() {
        break;
    }
}
let res = strip(&sc.res, &mut removed, builder)?.ok_or_else(|| fail("Keep at least one docked panel."))?;
Ok((ps, res, removed)) }

fn drop_kid(e: &S, i: i64, builder: &mut SyntaxBuilder) -> R<Option<S>> { match head_sym(e, builder) {
    Some("ui/split" | "ui/split-at") => Ok(layout_kids(e, builder).nth((1 - i) as usize)),
    Some("ui/tile") => {
        if layout_kids(e, builder).count() == 1 { Ok(None) } else { Ok(Some(arg_set(e, &ArgKey::Pos(i), None, builder)?)) }
    }
    _ => Ok(None),
} }

/// The layout without the panel at a tree path, and that panel.
fn take(path: &[i64], e: &S, builder: &mut SyntaxBuilder) -> R<(Option<S>, S)> { match path.split_first() {
    None => Err(fail("A layout keeps its last panel.")),
    Some((&i, rest)) => {
        let kid = layout_kids(e, builder).nth(i as usize).ok_or_else(|| fail("That panel is not in this layout."))?;
        if rest.is_empty() {
            Ok((drop_kid(e, i, builder)?, kid))
        } else {
            match take(rest, &kid, builder)? {
                (Some(k), taken) => Ok((Some(arg_set(e, &ArgKey::Pos(kid_pos(e, i, builder)), Some(k), builder)?), taken)),
                (None, taken) => Ok((drop_kid(e, i, builder)?, taken)),
            }
        }
    }
} }

fn node_at(path: &[i64], e: &S, builder: &SyntaxBuilder) -> R<S> { let mut e = e.clone();
for &i in path {
    e = layout_kids(&e, builder).nth(i as usize).ok_or_else(|| fail("That panel is not in this layout."))?;
}
Ok(e) }

fn has_docked(e: &S, builder: &SyntaxBuilder) -> bool { match head_sym(e, builder) {
    Some("ui/floating") => false,
    Some("ui/split" | "ui/split-at" | "ui/tile") => layout_kids(e, builder).any(|id| has_docked(&id, builder)),
    _ => true,
} }

/// The `(ui/workspace ...)` an editor graph returns: its result, or the binding its result names.
fn workspace_call(sc: &Scope, builder: &SyntaxBuilder) -> R<S> { let err = || fail("This editor graph does not return a (ui/workspace ...).");
match node_of(&sc.res, builder) {
    Node::Sym(name) => match binding_of(&sc.ps, builder.name(name), builder) {
        Some(b) if head_sym(&b, builder) == Some("ui/workspace") => Ok(b),
        _ => Err(err()),
    },
    _ if head_sym(&sc.res, builder) == Some("ui/workspace") => Ok(sc.res.clone()),
    _ => Err(err()),
} }

fn editor_scope(sx: &S, builder: &mut SyntaxBuilder) -> R<Scope> { let sc = ensure(sx, builder);
workspace_call(&sc, builder)?;
Ok(sc) }

fn workspace_arg(sc: &Scope, builder: &mut SyntaxBuilder) -> R<S> { arg_get(&workspace_call(sc, builder)?, &ArgKey::Pos(0), builder).ok_or_else(|| fail("The workspace holds no panels.")) }

/// The scope with `root` as the workspace's panel, written where the call is.
fn set_workspace(sc: &Scope, ps: &[(S, S)], root: S, builder: &mut SyntaxBuilder) -> R<Scope> { let call = arg_set(&workspace_call(sc, builder)?, &ArgKey::Pos(0), Some(root), builder)?;
Ok(match node_of(&sc.res, builder) {
    Node::Sym(name) => Scope {
        ps: ps.iter().map(|(pattern, value)| if pat_key(pattern, builder) == builder.name(name) { (*pattern, call) } else { (*pattern, *value) }).collect(),
        ..sc.clone()
    },
    _ => Scope { ps: ps.to_vec(), res: call, ..sc.clone() },
}) }

fn rebuilt(sc: &Scope, builder: &mut SyntaxBuilder) -> S { rebuild(sc, &sc.ps, sc.res.clone(), builder) }

fn is_switch(e: &S, builder: &SyntaxBuilder) -> bool { head_sym(e, builder) == Some("ui/switch") }

type Place = Option<(Option<String>, S)>;

/// The first switch of the layout: the root itself, or one nested in its splits, tiles and floats
/// (bound to a name, or written in place); the binding holding it, or none when written in place.
fn switch_place(sc: &Scope, builder: &mut SyntaxBuilder) -> R<Place> { fn find(sc: &Scope, top: bool, e: &S, builder: &SyntaxBuilder) -> Place {
    match node_of(e, builder) {
        Node::Sym(name) => match binding_of(&sc.ps, builder.name(name), builder) {
            Some(binding) if is_switch(&binding, builder) => Some((Some(builder.name(name).to_owned()), binding)),
            Some(b) if layout_binding(&b, builder) => find(sc, false, &b, builder),
            _ => None,
        },
        _ if is_switch(e, builder) => {
            if top { Some((None, e.clone())) } else { None }
        }
        _ => match head_sym(e, builder) {
            Some(h) if is_container(h) => layout_kids(e, builder).find_map(|id| find(sc, top, &id, builder)),
            _ => None,
        },
    }
}
Ok(find(sc, true, &workspace_arg(sc, builder)?, builder)) }

/// The first switch written in place below the root's splits, tiles and floats.
fn replace_inline_switch(sw: &S, root: &S, builder: &mut SyntaxBuilder) -> S { fn go(e: &S, found: &mut bool, sw: &S, builder: &mut SyntaxBuilder) -> S {
    if *found {
        e.clone()
    } else if is_switch(e, builder) {
        *found = true;
        keep_notes(e, sw.clone(), builder)
    } else {
        match head_sym(e, builder) {
            Some(h) if is_container(h) => map_children(&mut |child, builder| go(child, found, sw, builder), e, builder),
            _ => e.clone(),
        }
    }
}
let mut found = false;
go(root, &mut found, sw, builder) }

fn set_switch(sc: &Scope, ps: &[(S, S)], place: &Option<String>, sw: &S, builder: &mut SyntaxBuilder) -> R<S> { match place {
    None => {
        let root = replace_inline_switch(sw, &workspace_arg(sc, builder)?, builder);
        Ok(rebuilt(&set_workspace(sc, ps, root, builder)?, builder))
    }
    Some(n) => Ok(rebuild(sc, &ps.iter().map(|(p, e)| if pat_key(p, builder) == *n { (p.clone(), keep_notes(e, sw.clone(), builder)) } else { (p.clone(), e.clone()) }).collect::<Vec<_>>(), sc.res.clone(), builder)),
} }

fn active_of(args: &[S], builder: &SyntaxBuilder) -> R<i64> { match kw_get(args, "active", builder) {
    None => Ok(0),
    Some(id) if matches!(node_of(&id, builder), Node::Num(_)) => {
        let Node::Num(text) = node_of(&id, builder) else { unreachable!() };
        Ok(builder.name(text).parse::<i64>().unwrap_or(0))
    }
    Some(_) => Err(fail("The active layout is an expression: change it in the text.")),
} }

/// The switch's active input; an `:active` outside its layouts is refused.
fn active_layout(args: &[S], builder: &SyntaxBuilder) -> R<S> { let a = active_of(args, builder)?;
let found = if a < 0 { None } else { positions(args, builder).nth(a as usize) };
found.ok_or_else(|| fail("The active layout is not one of the switch's layouts.")) }

/// Layout bindings in `cands` that nothing reads any more go.
fn prune(body: &S, cands: &[String], builder: &mut SyntaxBuilder) -> S { let Some(b) = scope_of(body, builder) else { return body.clone() };
let used = |ps: &[(S, S)], n: &str| count_refs(n, &b.res, builder) > 0 || ps.iter().any(|(p, e)| pat_key(p, builder) != n && count_refs(n, e, builder) > 0);
let mut ps = b.ps.clone();
loop {
    let (dead, kept): (Vec<_>, Vec<_>) =
        ps.iter().cloned().partition(|(p, e)| cands.contains(&pat_key(p, builder)) && layout_binding(e, builder) && !used(&ps, &pat_key(p, builder)));
    if dead.is_empty() {
        break;
    }
    ps = kept;
}
collapse(&b, &ps, b.res.clone(), builder) }

/// `f ps layout` rewrites the active layout: the switch's active input, else the workspace's tree.
fn edit_layout(src: &[S], graph: &str, f: &dyn Fn(&[(S, S)], &S, &mut SyntaxBuilder) -> R<S>, builder: &mut SyntaxBuilder) -> R<Vec<S>> { edit_scope(src, &[graph.to_owned()], &mut |sx, builder| {
    let sc = editor_scope(sx, builder)?;
    match switch_place(&sc, builder)? {
        Some((place, sw)) => {
            let args = args_of(&sw, builder);
            let a = active_of(&args, builder)?;
            let slot = active_layout(&args, builder)?;
            let new = f(&sc.ps, &slot, builder)?;
            let put = set_switch(&sc, &sc.ps, &place, &arg_set(&sw, &ArgKey::Pos(a), Some(new), builder)?, builder)?;
            Ok(prune(&put, &reach(&sc.ps, &slot, builder), builder))
        }
        None => {
            let slot = workspace_arg(&sc, builder)?;
            let new = f(&sc.ps, &slot, builder)?;
            let put = rebuilt(&set_workspace(&sc, &sc.ps, new, builder)?, builder);
            Ok(prune(&put, &reach(&sc.ps, &slot, builder), builder))
        }
    }
}, builder) }

fn str_node(t: &str, builder: &mut SyntaxBuilder) -> S { mk(Draft::Str(t.as_bytes().to_vec()), builder) }

fn num_node(t: &str, builder: &mut SyntaxBuilder) -> S { mk(Draft::Num(t.to_owned()), builder) }

/// A workspace without a switch gets one around its tree.
fn ensure_switch(sc: &Scope, used: &mut Vec<String>, builder: &mut SyntaxBuilder) -> R<(Scope, Option<String>, S)> { match switch_place(sc, builder)? {
    Some((place, sw)) => Ok((sc.clone(), place, sw)),
    None => {
        let tree = workspace_arg(sc, builder)?;
        let (mut ps, kid) = match node_of(&tree, builder) {
            Node::Sym(_) => (sc.ps.clone(), tree.clone()),
            _ => {
                let n = fresh(used, "layout");
                let mut ps = sc.ps.clone();
                ps.push((sym(&n, builder), tree.clone()));
                (ps, sym(&n, builder))
            }
        };
        let name = fresh(used, "switch");
        let sw = call("ui/switch", vec![kid], builder);
        ps.push((sym(&name, builder), sw.clone()));
        Ok((set_workspace(sc, &ps, sym(&name, builder), builder)?, Some(name), sw))
    }
} }

fn editor_graph(form: &S, builder: &SyntaxBuilder) -> bool {
    let items = children(form, builder);
    if head_sym(form, builder) != Some("graph") || items.len() < 2 || !matches!(node_of(&items[1], builder), Node::Sym(_)) { return false; }
    items[2..].windows(2).any(|pair| matches!((node_of(&pair[0], builder), node_of(&pair[1], builder)), (Node::Kw(key), Node::Sym(value)) if builder.name(key) == "context" && builder.name(value) == "editor"))
}

fn rewrite(src: &[S], op: &Op, builder: &mut SyntaxBuilder) -> R<Vec<S>> { match op {
    Op::MergeThenNew { graph } => {
        let merged = rewrite(src, &Op::MergeLayouts { graph: graph.clone() }, builder)?;
        rewrite(&merged, &Op::LayoutNew { graph: graph.clone() }, builder)
    }
    Op::SetLayoutSize { node, size } => {
        let (sp, leaf) = split_node(node)?;
        edit_scope(src, &sp, &mut |sx, builder| {
            let e = get_node(sx, &leaf, builder)?;
            let not_split = || fail("That panel is not a split.");
            let (axis, a, b) = match head_sym(&e, builder) {
                Some("ui/split" | "ui/split-at") => {
                    let axis = positions(args_of(&e, builder), builder).next();
                    let mut kids = layout_kids(&e, builder);
                    match (axis, kids.next(), kids.next()) {
                        (Some(axis), Some(a), Some(b)) => (axis, a, b),
                        _ => return Err(not_split()),
                    }
                }
                _ => return Err(not_split()),
            };
            // one side fixed in whole points, or a ratio with four decimals
            let fixed = |key: &str, n: i64, builder: &mut SyntaxBuilder| call("ui/split", vec![axis, a, b, kwf(key, builder), num(n.max(1), builder)], builder);
            let new = match size {
                Size::First(n) => fixed("first_size", *n, builder),
                Size::Second(n) => fixed("second_size", *n, builder),
                Size::Ratio(r) => call("ui/split-at", vec![axis, num_node(&ratio_text(*r), builder), a, b], builder),
            };
            set_node(sx, &leaf, keep_notes(&e, new, builder), builder)
        }, builder)
    }
    Op::SplitPanel { node, axis, ratio, panel } => {
        let (sp, leaf) = split_node(node)?;
        let mut used = root_used(src, &sp[0], builder);
        let panel = builder.import(panel);
        edit_scope(src, &sp, &mut |sx, builder| {
            let sc = scope_of(sx, builder).ok_or_else(|| fail("This editor graph is a single expression. Edit it in Lisp."))?;
            let j = first_pair_index(&sc, &leaf, builder)?;
            let a = fresh(&mut used, &format!("{leaf}_a"));
            let b = fresh(&mut used, &format!("{leaf}_b"));
            let (p, orig) = sc.ps[j].clone();
            let split = call("ui/split-at", vec![
                str_node(if *axis == Axis::H { "horizontal" } else { "vertical" }, builder),
                num_node(&ratio_text(*ratio), builder), sym(&a, builder), sym(&b, builder),
            ], builder);
            let mut ps: Vec<(S, S)> = sc.ps[..j].to_vec();
            ps.push((sym(&a, builder), orig));
            ps.push((sym(&b, builder), panel));
            ps.push((p, split));
            ps.extend(sc.ps[j + 1..].iter().cloned());
            reorder(&rebuild(&sc, &ps, sc.res.clone(), builder), builder)
        }, builder)
    }
    Op::ClosePanel { node } => {
        let (sp, leaf) = split_node(node)?;
        edit_scope(src, &sp, &mut |sx, builder| {
            let sc = scope_of(sx, builder).ok_or_else(|| fail("This editor graph is a single expression. Edit it in Lisp."))?;
            get_node(sx, &leaf, builder)?;
            let (ps, res, _) = strip_panel(false, &sc, &leaf, builder)?;
            reorder(&rebuild(&sc, &ps, res, builder), builder)
        }, builder)
    }
    Op::DockPanel { node, target, side } => {
        let (sp, leaf) = split_node(node)?;
        let (tp, target) = split_node(target)?;
        if sp != tp || leaf == target {
            return Err(fail("Dock two different panels of the same layout."));
        }
        let mut used = root_used(src, &sp[0], builder);
        edit_scope(src, &sp, &mut |sx, builder| {
            let sc = ensure(sx, builder);
            get_node(sx, &leaf, builder)?;
            get_node(sx, &target, builder)?;
            let (ps, res, removed) = strip_panel(true, &sc, &leaf, builder)?;
            if removed.contains(&target) {
                return Err(fail("A panel cannot dock inside its own group."));
            }
            let group = fresh(&mut used, &format!("{target}_dock"));
            let first = matches!(side, Side::Left | Side::Top);
            let split = call("ui/split-at", vec![
                str_node(if matches!(side, Side::Top | Side::Bottom) { "vertical" } else { "horizontal" }, builder),
                num_node("0.5", builder),
                sym(if first { &leaf } else { &target }, builder),
                sym(if first { &target } else { &leaf }, builder),
            ], builder);
            let mut ps: Vec<(S, S)> =
                ps.iter().map(|(p, e)| (p.clone(), if pat_key(p, builder) == target { e.clone() } else { rename_place(&target, &group, e, builder) })).collect();
            ps.push((sym(&group, builder), split));
            reorder(&rebuild(&sc, &ps, rename_place(&target, &group, &res, builder), builder), builder)
        }, builder)
    }
    Op::SetPanelKind { node, panel } => {
        let (sp, leaf) = split_node(node)?;
        let panel = builder.import(panel);
        edit_scope(src, &sp, &mut |sx, builder| set_node(sx, &leaf, panel, builder), builder)
    }
    Op::SetLayout { graph, index } => edit_scope(src, &[graph.clone()], &mut |sx, builder| {
        let sc = editor_scope(sx, builder)?;
        match switch_place(&sc, builder)? {
            None => Err(fail("This workspace has one layout and no switch.")),
            Some((place, sw)) => {
                if *index < 0 || *index >= positions(args_of(&sw, builder), builder).count() as i64 {
                    return Err(fail(&format!("There is no layout {index}.")));
                }
                set_switch(&sc, &sc.ps, &place, &arg_set(&sw, &ArgKey::Kw("active".to_owned()), Some(num(*index, builder)), builder)?, builder)
            }
        }
    }, builder),
    Op::LayoutNew { graph } => {
        let mut used = root_used(src, graph, builder);
        edit_scope(src, &[graph.clone()], &mut |sx, builder| {
            let sc = editor_scope(sx, builder)?;
            let (sc, place, sw) = ensure_switch(&sc, &mut used, builder)?;
            let args = args_of(&sw, builder);
            let n = positions(args, builder).count() as i64;
            if n >= 10 {
                return Err(fail("Ten layouts is the limit of the digit keys."));
            }
            let copy = solid(&sc.ps, &active_layout(&args, builder)?, builder);
            let name = fresh(&mut used, "layout");
            let sw = arg_set(&arg_set(&sw, &ArgKey::Pos(n), Some(sym(&name, builder)), builder)?, &ArgKey::Kw("active".to_owned()), Some(num(n, builder)), builder)?;
            let mut ps = sc.ps.clone();
            ps.push((sym(&name, builder), copy));
            reorder(&set_switch(&sc, &ps, &place, &sw, builder)?, builder)
        }, builder)
    }
    Op::MergeLayouts { graph } => {
        // an older file: the other `:context editor` graphs become layouts of a switch in
        // `graph`; each brings its bindings, renamed `graph_name` so they cannot clash, and then goes
        let (_, items) = workspace_parts(src, builder)?;
        let others: Vec<(String, S)> = items
            .iter()
            .filter_map(|i| match root_name(i, builder) {
                Some(n) if n != *graph && editor_graph(i, builder) => Some((n, i.clone())),
                _ => None,
            })
            .collect();
        if others.is_empty() {
            return Err(fail("There is one editor graph and nothing to merge."));
        }
        let mut used = root_used(src, graph, builder);
        let merged = edit_scope(src, &[graph.clone()], &mut |sx, builder| {
            let sc = editor_scope(sx, builder)?;
            let (sc, place, mut sw) = ensure_switch(&sc, &mut used, builder)?;
            let mut ps = sc.ps.clone();
            for (name, item) in &others {
                let theirs = editor_scope(&last_child(item, builder)?, builder)?;
                let names: Vec<(String, String)> =
                    theirs.ps.iter().map(|(p, _)| (pat_key(p, builder), fresh(&mut used, &format!("{name}_{}", pat_key(p, builder))))).collect();
                let rename = |expression: &S, builder: &mut SyntaxBuilder| names.iter().fold(*expression, |expression, (old, name)| rename_ref(old, name, &expression, builder));
                let own: Vec<(S, S)> = theirs
                    .ps
                    .iter()
                    .map(|(p, e)| {
                        let key = pat_key(p, builder);
                        let nw = names.iter().find(|(k, _)| *k == key).map(|(_, v)| v.clone()).unwrap_or(key);
                        (sym(&nw, builder), rename(e, builder))
                    })
                    .collect();
                let tree = rename(&workspace_arg(&theirs, builder)?, builder);
                let kid = match node_of(&tree, builder) {
                    Node::Sym(_) => {
                        ps.extend(own);
                        tree.clone()
                    }
                    _ => {
                        let n = fresh(&mut used, name);
                        ps.extend(own);
                        ps.push((sym(&n, builder), tree.clone()));
                        sym(&n, builder)
                    }
                };
                let count = positions(args_of(&sw, builder), builder).count() as i64;
                if count >= 10 {
                    return Err(fail("Ten layouts is the limit of the digit keys."));
                }
                sw = arg_set(&sw, &ArgKey::Pos(count), Some(kid), builder)?;
            }
            reorder(&set_switch(&sc, &ps, &place, &sw, builder)?, builder)
        }, builder)?;
        map_items(&merged, &mut |items, builder| {
            Ok(items.into_iter().filter(|i| match root_name(i, builder) {
                Some(n) => !others.iter().any(|(o, _)| *o == n),
                None => true,
            }).collect())
        }, builder)
    }
    Op::LayoutRemove { graph } => edit_scope(src, &[graph.clone()], &mut |sx, builder| {
        let sc = editor_scope(sx, builder)?;
        match switch_place(&sc, builder)? {
            None => Err(fail("This workspace has one layout and no switch.")),
            Some((place, sw)) => {
                let args = args_of(&sw, builder);
                let n = positions(args, builder).count() as i64;
                let a = active_of(&args, builder)?;
                if n < 2 {
                    return Err(fail("A switch keeps its last layout."));
                }
                let gone = active_layout(&args, builder)?;
                let sw = arg_set(&arg_set(&sw, &ArgKey::Pos(a), None, builder)?, &ArgKey::Kw("active".to_owned()), Some(num(a.min(n - 2), builder)), builder)?;
                Ok(prune(&set_switch(&sc, &sc.ps, &place, &sw, builder)?, &reach(&sc.ps, &gone, builder), builder))
            }
        }
    }, builder),
    Op::LayoutWindow { graph, panel } => {
        let panel = builder.import(panel);
        edit_layout(src, graph, &|_, layout, builder| {
            Ok(call("ui/split-at", vec![str_node("horizontal", builder), num_node("0.5", builder), *layout, call("ui/floating", vec![panel], builder)], builder))
        }, builder)
    }
    Op::LayoutFloat { graph, at } => edit_layout(src, graph, &|ps, layout, builder| {
        let tree = solid(ps, layout, builder);
        let up = at.get(..at.len().saturating_sub(1)).unwrap_or(&[]);
        if !up.is_empty() && head_sym(&node_at(up, &tree, builder)?, builder) == Some("ui/floating") {
            // a window docks beside the rest
            let first = |window: &S, builder: &SyntaxBuilder| layout_kids(window, builder).next().ok_or_else(|| fail("A floating window holds no panel."));
            match take(up, &tree, builder)? {
                (Some(rest), window) => Ok(call("ui/split-at", vec![str_node("horizontal", builder), num_node("0.7", builder), rest, first(&window, builder)?], builder)),
                (None, window) => first(&window, builder),
            }
        } else {
            match take(at, &tree, builder)? {
                (Some(rest), panel) if has_docked(&rest, builder) => {
                    Ok(call("ui/split-at", vec![str_node("horizontal", builder), num_node("0.5", builder), rest, call("ui/floating", vec![panel], builder)], builder))
                }
                _ => Err(fail("A layout keeps one docked panel.")),
            }
        }
    }, builder),
} }

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
    src: &[Syntax],
    op: &Op,
) -> R<(Vec<Syntax>, w::Workspace)> {
    apply_rewrite(ops, library, checker, catalog, &mut || {
        let mut builder = SyntaxBuilder::new();
        let roots = src.iter().map(|form| builder.import(form)).collect::<Vec<_>>();
        let roots = rewrite(&roots, op, &mut builder)?;
        let arena = builder.freeze();
        Ok(roots.into_iter().map(|id| arena.handle(id)).collect())
    })
}

/// The edit adds or removes nodes, so layout keys are pruned after it (a resize does not).
pub fn changes_nodes(op: &Op) -> bool {
    !matches!(op, Op::SetLayoutSize { .. })
}
