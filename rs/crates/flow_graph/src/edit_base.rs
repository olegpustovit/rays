// Port of lib/flow_graph/edit_base.ml: the syntax-tree helpers, the op type and the
// source-rewrite primitives every graph gesture is built from.
use flow_check::workspace as w;
use flow_syntax::syntax::{self as s, Node, QuoteKind, Syntax};
use flow_syntax::{lisp, symbol, ty::{self, Ty}};
use rays_diagnostic::Diagnostic;
use std::collections::HashSet;
use std::sync::OnceLock;

pub type S = Syntax;
pub type Path = w::Path;
pub type R<T> = Result<T, Diagnostic>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArgKey {
    Whole,
    Pos(i64),
    Kw(String),
    Field(String),
    Bv(i64, i64),
    Arm(i64),
    Cell(i64),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Loop {
    For,
    Fold,
    If,
    Map,
    Filter,
}

#[derive(Clone, Debug)]
pub enum Op {
    SetArg { node: Path, key: ArgKey, sub: Vec<i64>, value: S },
    Connect { node: Path, key: ArgKey, src: String, iter: bool },
    Disconnect { node: Path, key: ArgKey, fallback: Option<S> },
    SetInputDefault { form: String, input: String, value: S },
    Unfold { node: Path, key: ArgKey, sub: Vec<i64> },
    FoldInto { node: Path },
    AddArm { node: Path, after: i64 },
    DeleteArm { node: Path, index: i64 },
    Wrap { nodes: Vec<Path>, loop_: Loop },
    Hoist { node: Path },
    Rename { node: Path, to_: String },
    MakeLocalFn { nodes: Vec<Path> },
    MakeDefn { nodes: Vec<Path>, name: String, context: String, params: Vec<(String, String)> },
    MakeMacro { nodes: Vec<Path>, name: String, holes: Vec<(Vec<i64>, String)> },
    InlineMacro { node: Path },
    InlineDefn { node: Path },
    MoveBinding { node: Path, by: i64 },
    MoveStep { node: Path, earlier: bool },
    ToggleBypass { node: Path },
    SetNote { node: Path, text: String },
    AddItem { node: Path },
    MoveItem { node: Path, pos: i64 },
    AddField { node: Path, name: String, value: S },
    AddNode { scope: Path, name: String, expr: S },
    DeleteNodes { nodes: Vec<Path> },
    SetGraph { name: String, form: S },
    Duplicate { nodes: Vec<Path> },
    RemoveGraph { name: String },
    RenameGraph { name: String, to_: String },
    GroupMerge { nodes: Vec<Path>, name: String },
}

pub fn err(code: &str, message: &str) -> Diagnostic {
    rays_diagnostic::error(code.into(), message.into(), None, None, None, Vec::new(), Vec::new())
}

/// Refuse the edit with an `E_EDIT` diagnostic and this message.
pub fn fail(message: &str) -> Diagnostic {
    err("E_EDIT", message)
}

pub fn fail_code(code: &str, message: &str) -> Diagnostic {
    err(code, message)
}

// ---- syntax helpers ----

pub fn mk(node: Node) -> S {
    s::make(node)
}
pub fn sym(name: &str) -> S {
    mk(Node::Sym(name.to_owned()))
}
pub fn kwf(name: &str) -> S {
    mk(Node::Kw(name.to_owned()))
}
pub fn vec(items: Vec<S>) -> S {
    mk(Node::Vec(items.into()))
}
pub fn call(head: &str, args: Vec<S>) -> S {
    let mut items = vec![sym(head)];
    items.extend(args);
    mk(Node::List(items.into()))
}
pub fn num(n: i64) -> S {
    mk(Node::Num(n.to_string()))
}
pub fn unquote(x: S) -> S {
    mk(Node::Quote(QuoteKind::Unquote, std::sync::Arc::new(x)))
}
pub fn text(x: &str) -> S {
    mk(Node::Str(x.as_bytes().to_vec()))
}
pub fn pairs(items: &[S]) -> Vec<(S, S)> {
    items.chunks_exact(2).map(|p| (p[0].clone(), p[1].clone())).collect()
}
pub fn flat_pairs(ps: &[(S, S)]) -> Vec<S> {
    let mut out = Vec::with_capacity(ps.len() * 2);
    for (a, b) in ps {
        out.push(a.clone());
        out.push(b.clone());
    }
    out
}
pub fn head_sym(e: &S) -> Option<&str> {
    s::head(e)
}
pub fn sym_of(e: &S) -> Option<&str> {
    match &e.node {
        Node::Sym(n) => Some(n),
        _ => None,
    }
}
pub fn children(e: &S) -> Vec<S> {
    s::children(e).into_iter().cloned().collect()
}
pub fn with_node(e: &S, node: Node) -> S {
    let mut r = e.clone();
    r.node = node;
    r
}

pub fn map_children(f: &mut dyn FnMut(&S) -> S, e: &S) -> S {
    let node = match &e.node {
        Node::List(l) => Node::List(l.iter().map(&mut *f).collect()),
        Node::Vec(l) => Node::Vec(l.iter().map(&mut *f).collect()),
        Node::Map(l) => Node::Map(l.iter().map(&mut *f).collect()),
        Node::Quote(k, x) => Node::Quote(*k, std::sync::Arc::new(f(x))),
        n => n.clone(),
    };
    with_node(e, node)
}

pub fn try_map_children(f: &mut dyn FnMut(&S) -> R<S>, e: &S) -> R<S> {
    let node = match &e.node {
        Node::List(l) => Node::List(l.iter().map(&mut *f).collect::<R<Vec<S>>>()?.into()),
        Node::Vec(l) => Node::Vec(l.iter().map(&mut *f).collect::<R<Vec<S>>>()?.into()),
        Node::Map(l) => Node::Map(l.iter().map(&mut *f).collect::<R<Vec<S>>>()?.into()),
        Node::Quote(k, x) => Node::Quote(*k, std::sync::Arc::new(f(x)?)),
        n => n.clone(),
    };
    Ok(with_node(e, node))
}

pub fn root_of(name: &str) -> &str {
    match name.find('.') {
        Some(i) => &name[..i],
        None => name,
    }
}

fn syms(e: &S, acc: &mut Vec<String>) {
    match &e.node {
        Node::Sym(n) => acc.push(root_of(n).to_owned()),
        Node::List(l) if l.len() >= 2 && sym_of(&l[0]) == Some("ref") && sym_of(&l[1]).is_some() => {
            // `(ref graph ...)` names a graph, never a binding
            for c in &l[2..] {
                syms(c, acc);
            }
        }
        _ => {
            for c in s::children(e) {
                syms(c, acc);
            }
        }
    }
}

/// The names a form reads, first use first.
pub fn sym_list(e: &S) -> Vec<String> {
    let mut acc = Vec::new();
    syms(e, &mut acc);
    acc
}

pub fn dedup(l: &[String]) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for x in l {
        if !out.contains(x) {
            out.push(x.clone());
        }
    }
    out
}

pub fn count_refs(name: &str, e: &S) -> usize {
    sym_list(e).iter().filter(|x| x.as_str() == name).count()
}

pub fn replace_ref(name: &str, by: &S, e: &S) -> S {
    match &e.node {
        Node::Sym(n) if n == name => by.clone(),
        _ => map_children(&mut |c| replace_ref(name, by, c), e),
    }
}

pub fn rename_ref(old: &str, nw: &str, e: &S) -> S {
    match &e.node {
        Node::Sym(n) if root_of(n) == old => with_node(e, Node::Sym(format!("{}{}", nw, &n[old.len()..]))),
        _ => map_children(&mut |c| rename_ref(old, nw, c), e),
    }
}

/// A panel's place in the layout moves to `new`; an `:of old` still follows the panel itself.
pub fn rename_place(old: &str, nw: &str, e: &S) -> S {
    match &e.node {
        Node::List(items) => {
            let mut out = Vec::with_capacity(items.len());
            let mut i = 0;
            while i < items.len() {
                if matches!(&items[i].node, Node::Kw(k) if k == "of") && i + 1 < items.len() {
                    out.push(items[i].clone());
                    out.push(items[i + 1].clone());
                    i += 2;
                } else {
                    out.push(rename_place(old, nw, &items[i]));
                    i += 1;
                }
            }
            with_node(e, Node::List(out.into()))
        }
        _ => rename_ref(old, nw, e),
    }
}

/// `(ref old ...)` reads `new`: only the reference, never a binding of the same name.
pub fn rename_graph_ref(old: &str, nw: &str, e: &S) -> S {
    if let Node::List(l) = &e.node {
        if l.len() >= 2 && sym_of(&l[0]) == Some("ref") && sym_of(&l[1]) == Some(old) {
            let mut out = vec![l[0].clone(), with_node(&l[1], Node::Sym(nw.to_owned()))];
            out.extend(l[2..].iter().map(|c| rename_graph_ref(old, nw, c)));
            return with_node(e, Node::List(out.into()));
        }
    }
    map_children(&mut |c| rename_graph_ref(old, nw, c), e)
}

pub fn pat_names(p: &S) -> Vec<String> {
    match &p.node {
        Node::Sym(n) => vec![n.clone()],
        Node::Vec(l) => l.iter().flat_map(pat_names).collect(),
        Node::Map(l) => l
            .iter()
            .flat_map(|x| match &x.node {
                Node::Vec(v) => v.iter().flat_map(pat_names).collect::<Vec<_>>(),
                _ => Vec::new(),
            })
            .collect(),
        _ => Vec::new(),
    }
}

/// The name a binding pattern writes.
pub fn pat_key(p: &S) -> String {
    lisp::flat(p)
}

fn bound(vecf: &S, acc: &mut Vec<String>) {
    if let Node::Vec(bs) = &vecf.node {
        for (p, _) in pairs(bs) {
            acc.extend(pat_names(&p));
        }
    }
}

// every name a form declares: let and zone bindings, fn parameters, typed inputs
fn declared_into(e: &S, acc: &mut Vec<String>) {
    if let Node::List(l) = &e.node {
        let head = l.first().and_then(sym_of);
        let mut done = false;
        match head {
            Some("let*") if l.len() >= 2 => {
                bound(&l[1], acc);
                done = true;
            }
            Some("for") | Some("scan") | Some("sum") if l.len() >= 2 => {
                bound(&l[1], acc);
                done = true;
            }
            Some("fold") if l.len() >= 3 => {
                bound(&l[2], acc);
                bound(&l[1], acc);
                done = true;
            }
            Some("state") if l.len() >= 2 => {
                bound(&l[1], acc);
                done = true;
            }
            Some("fn") if l.len() >= 2 => {
                if let Node::Vec(ps) = &l[1].node {
                    for p in ps {
                        match &p.node {
                            Node::List(pl) if pl.first().and_then(sym_of).is_some() => {
                                acc.push(sym_of(&pl[0]).unwrap().to_owned())
                            }
                            _ => acc.extend(pat_names(p)),
                        }
                    }
                    done = true;
                }
            }
            _ => {}
        }
        if !done && l.len() >= 2 {
            if let (Some(n), Some(":")) = (head, sym_of(&l[1])) {
                acc.push(n.to_owned());
            }
        }
    }
    for c in s::children(e) {
        declared_into(c, acc);
    }
}

pub fn declared(e: &S) -> Vec<String> {
    let mut acc = Vec::new();
    declared_into(e, &mut acc);
    acc
}

/// Names read by a form and not declared inside it.
pub fn free(e: &S) -> Vec<String> {
    let inner: HashSet<String> = declared(e).into_iter().collect();
    sym_list(e).into_iter().filter(|x| !inner.contains(x)).collect()
}

pub fn nth_child(e: &S, i: i64) -> Option<S> {
    if i < 0 {
        return None;
    }
    s::children(e).get(i as usize).map(|c| (*c).clone())
}

pub fn set_child(e: &S, i: i64, v: S) -> R<S> {
    let node = match &e.node {
        Node::List(l) if i >= 0 && (i as usize) < l.len() => Node::List(replace_at(l, i as usize, v).into()),
        Node::Vec(l) if i >= 0 && (i as usize) < l.len() => Node::Vec(replace_at(l, i as usize, v).into()),
        Node::Map(l) if i >= 0 && (i as usize) < l.len() => Node::Map(replace_at(l, i as usize, v).into()),
        Node::Quote(k, _) if i == 0 => Node::Quote(*k, std::sync::Arc::new(v)),
        _ => return Err(fail(&format!("This form has no part {}.", i))),
    };
    Ok(with_node(e, node))
}

fn replace_at(l: &[S], i: usize, v: S) -> Vec<S> {
    let mut out = l.to_vec();
    out[i] = v;
    out
}

pub fn get_sub(e: &S, sub: &[i64]) -> Option<S> {
    match sub {
        [] => Some(e.clone()),
        [i, rest @ ..] => nth_child(e, *i).and_then(|c| get_sub(&c, rest)),
    }
}

pub fn set_sub(e: &S, sub: &[i64], v: S) -> R<S> {
    match sub {
        [] => Ok(v),
        [i, rest @ ..] => match nth_child(e, *i) {
            Some(c) => set_child(e, *i, set_sub(&c, rest, v)?),
            None => Err(fail(&format!("This form has no part {}.", i))),
        },
    }
}

/// `v`, with the notes of `old` when it has none.
pub fn keep_notes(old: &S, v: S) -> S {
    if v.notes().is_empty() {
        let mut v = v;
        *v.notes_mut() = old.notes().to_vec();
        v
    } else {
        v
    }
}

// ---- call arguments: positional ones, then :keyword pairs ----

pub fn is_kw(x: &S) -> bool {
    matches!(x.node, Node::Kw(_))
}
pub fn kw_name(k: &S) -> String {
    match &k.node {
        Node::Kw(s) => s.clone(),
        _ => String::new(),
    }
}
fn kw_pair_at(args: &[S], i: usize) -> bool {
    i + 1 < args.len() && is_kw(&args[i])
}

/// The positional arguments of a call, in written order.
pub fn positional(args: &[S]) -> Vec<S> {
    let mut out = Vec::new();
    let mut i = 0;
    while i < args.len() {
        if kw_pair_at(args, i) {
            i += 2;
        } else {
            out.push(args[i].clone());
            i += 1;
        }
    }
    out
}

/// The `:keyword value` pairs of a call's arguments, in written order.
pub fn keywords(args: &[S]) -> Vec<(String, S)> {
    let mut out = Vec::new();
    let mut i = 0;
    while i < args.len() {
        if kw_pair_at(args, i) {
            out.push((kw_name(&args[i]), args[i + 1].clone()));
            i += 2;
        } else {
            i += 1;
        }
    }
    out
}

pub fn kw_get(args: &[S], k: &str) -> Option<S> {
    let mut i = 0;
    while i + 1 < args.len() {
        if is_kw(&args[i]) {
            if kw_name(&args[i]) == k {
                return Some(args[i + 1].clone());
            }
            i += 2;
        } else {
            i += 1;
        }
    }
    None
}

pub fn with_kw(args: &[S], k: &str, v: Option<S>) -> Vec<S> {
    let mut out = Vec::new();
    let mut i = 0;
    while i < args.len() {
        if kw_pair_at(args, i) {
            if kw_name(&args[i]) == k {
                if let Some(v) = v {
                    out.push(args[i].clone());
                    out.push(keep_notes(&args[i + 1], v));
                }
                out.extend(args[i + 2..].iter().cloned());
                return out;
            }
            out.push(args[i].clone());
            out.push(args[i + 1].clone());
            i += 2;
        } else {
            out.push(args[i].clone());
            i += 1;
        }
    }
    if let Some(v) = v {
        out.push(kwf(k));
        out.push(v);
    }
    out
}

pub fn with_pos(args: &[S], i: i64, v: Option<S>) -> Vec<S> {
    let n = positional(args).len() as i64;
    if i >= n {
        match v {
            None => args.to_vec(),
            Some(v) => {
                // after the last positional argument, else first
                let mut additions: Vec<S> = (0..(i - n)).map(|_| sym("nil")).collect();
                additions.push(v);
                if n == 0 {
                    let mut out = additions;
                    out.extend(args.iter().cloned());
                    out
                } else {
                    let mut out = Vec::new();
                    let mut seen = 0;
                    let mut j = 0;
                    while j < args.len() {
                        if kw_pair_at(args, j) {
                            out.push(args[j].clone());
                            out.push(args[j + 1].clone());
                            j += 2;
                        } else if seen + 1 == n {
                            out.push(args[j].clone());
                            out.extend(additions.iter().cloned());
                            out.extend(args[j + 1..].iter().cloned());
                            return out;
                        } else {
                            out.push(args[j].clone());
                            seen += 1;
                            j += 1;
                        }
                    }
                    out
                }
            }
        }
    } else {
        let mut out = Vec::new();
        let mut k = 0;
        let mut j = 0;
        while j < args.len() {
            if kw_pair_at(args, j) {
                out.push(args[j].clone());
                out.push(args[j + 1].clone());
                j += 2;
            } else if k == i {
                if let Some(v) = v {
                    out.push(keep_notes(&args[j], v));
                }
                out.extend(args[j + 1..].iter().cloned());
                return out;
            } else {
                out.push(args[j].clone());
                k += 1;
                j += 1;
            }
        }
        out
    }
}

/// A `:skip` value (register L16) for these tuples: bare integers when each has one.
pub fn skip_value(tuples: &[Vec<i64>]) -> S {
    if tuples.iter().all(|t| t.len() == 1) {
        vec_of(tuples.iter().map(|t| num(t[0])).collect())
    } else {
        vec_of(tuples.iter().map(|t| vec_of(t.iter().map(|n| num(*n)).collect())).collect())
    }
}

fn vec_of(items: Vec<S>) -> S {
    vec(items)
}

pub fn skip_of_args(args: &[S]) -> Vec<Vec<i64>> {
    match kw_get(args, "skip") {
        Some(v) => w::skip_tuples(&v)
            .map(|ts| ts.into_iter().map(|t| t.into_iter().map(|n| n as i64).collect()).collect())
            .unwrap_or_default(),
        None => Vec::new(),
    }
}

// a `scene/merge`'s `:skip` tuples end in an argument position: when the arguments at `gone`
// (old positions) leave, the ones after them move up and the ones for them go
pub fn skip_after_removal(args: &[S], gone: &[i64]) -> Vec<S> {
    let mut moved: Vec<Vec<i64>> = Vec::new();
    for t in skip_of_args(args) {
        if let Some((p, outer)) = t.split_last() {
            if !gone.contains(p) {
                let mut r = outer.to_vec();
                r.push(p - gone.iter().filter(|g| *g < p).count() as i64);
                moved.push(r);
            }
        }
    }
    with_kw(args, "skip", if moved.is_empty() { None } else { Some(skip_value(&moved)) })
}

pub fn arm_key(e: &S, index: i64) -> ArgKey {
    if index < 0 {
        if head_sym(e) == Some("if") { ArgKey::Pos(2) } else { ArgKey::Kw("else".into()) }
    } else {
        ArgKey::Pos(match head_sym(e) {
            Some("case") => 2 * index + 2,
            Some("cond") => 2 * index + 1,
            _ => 1,
        })
    }
}

pub fn arg_get(e: &S, key: &ArgKey) -> Option<S> {
    let key = match key {
        ArgKey::Arm(i) => arm_key(e, *i),
        k => k.clone(),
    };
    match (&key, &e.node) {
        (ArgKey::Whole, _) => Some(e.clone()),
        (ArgKey::Bv(i, j), _) => nth_child(e, *i).and_then(|c| nth_child(&c, *j)),
        (ArgKey::Field(k), Node::Map(l)) => pairs(l).into_iter().find(|(a, _)| kw_name(a) == *k).map(|(_, b)| b),
        (ArgKey::Pos(i), Node::List(l)) if !l.is_empty() => {
            if *i < 0 {
                return None;
            }
            let args = s::attribute_args(head_sym(e).unwrap_or(""), &l[1..]);
            positional(&args).get(*i as usize).cloned()
        }
        (ArgKey::Kw(k), Node::List(l)) if !l.is_empty() => kw_get(&l[1..], k),
        (ArgKey::Cell(i), Node::Vec(l)) => {
            if *i < 0 {
                None
            } else {
                l.get(*i as usize).cloned()
            }
        }
        _ => None,
    }
}

pub fn set_pair(ps: &[(S, S)], k: &str, v: Option<S>) -> Vec<(S, S)> {
    if ps.iter().any(|(a, _)| kw_name(a) == k) {
        match v {
            Some(v) => ps
                .iter()
                .map(|(a, b)| if kw_name(a) == k { (a.clone(), keep_notes(b, v.clone())) } else { (a.clone(), b.clone()) })
                .collect(),
            None => ps.iter().filter(|(a, _)| kw_name(a) != k).cloned().collect(),
        }
    } else {
        let mut out = ps.to_vec();
        if let Some(v) = v {
            out.push((kwf(k), v));
        }
        out
    }
}

// a `scene/root` prints its keywords in a fixed order: the camera, the renderer, the size, the samples
fn root_order(args: &[S]) -> Vec<S> {
    let rank = |k: &str| -> usize {
        ["camera", "renderer", "width", "height", "max_spp", "bounces", "round_samples"]
            .iter()
            .position(|x| *x == k)
            .unwrap_or(99)
    };
    let mut kws: Vec<(S, S)> = Vec::new();
    let mut pos: Vec<S> = Vec::new();
    let mut i = 0;
    while i < args.len() {
        if kw_pair_at(args, i) {
            kws.push((args[i].clone(), args[i + 1].clone()));
            i += 2;
        } else {
            pos.push(args[i].clone());
            i += 1;
        }
    }
    kws.sort_by_key(|(a, _)| rank(&kw_name(a)));
    pos.extend(flat_pairs(&kws));
    pos
}

pub fn arg_set(e: &S, key: &ArgKey, v: Option<S>) -> R<S> {
    let key = match key {
        ArgKey::Arm(i) => arm_key(e, *i),
        k => k.clone(),
    };
    let e = match &e.node {
        Node::List(l) if matches!(l.first().map(|h| &h.node), Some(Node::Sym(_))) => {
            let h = sym_of(&l[0]).unwrap();
            let mut items = vec![l[0].clone()];
            items.extend(s::attribute_args(h, &l[1..]));
            with_node(e, Node::List(items.into()))
        }
        _ => e.clone(),
    };
    match (&key, &e.node, v) {
        (ArgKey::Whole, _, Some(v)) => Ok(keep_notes(&e, v)),
        (ArgKey::Whole, _, None) => Err(fail("A whole binding cannot be removed.")),
        (ArgKey::Bv(i, j), _, Some(v)) => match nth_child(&e, *i) {
            Some(c) => set_child(&e, *i, set_child(&c, *j, v)?),
            None => Err(fail(&format!("This form has no part {}.", i))),
        },
        (ArgKey::Bv(..), _, None) => Err(fail("A loop clause cannot be removed.")),
        (ArgKey::Cell(i), Node::Vec(l), Some(v)) => {
            let out = l.iter().enumerate().map(|(j, c)| if j as i64 == *i { keep_notes(c, v.clone()) } else { c.clone() }).collect();
            Ok(with_node(&e, Node::Vec(out)))
        }
        (ArgKey::Cell(_), _, None) => Err(fail("A vector cell cannot be removed.")),
        (ArgKey::Field(k), Node::Map(l), v) => Ok(with_node(&e, Node::Map(flat_pairs(&set_pair(&pairs(l), k, v)).into()))),
        (ArgKey::Pos(i), Node::List(l), None) if !l.is_empty() && sym_of(&l[0]) == Some("scene/merge") => {
            let mut items = vec![l[0].clone()];
            items.extend(skip_after_removal(&with_pos(&l[1..], *i, None), &[*i]));
            Ok(with_node(&e, Node::List(items.into())))
        }
        (ArgKey::Kw(k), Node::List(l), Some(v))
            if k == "skip" && !l.is_empty() && sym_of(&l[0]) == Some("for") && kw_get(&l[1..], "skip").is_none() =>
        {
            // the body stays last
            let args = &l[1..];
            let n = args.len();
            let mut items = vec![l[0].clone()];
            if n >= 1 {
                items.extend(args[..n - 1].iter().cloned());
                items.push(kwf("skip"));
                items.push(v);
                items.push(args[n - 1].clone());
            } else {
                return Err(fail("The edit failed: Invalid_argument(\"List.nth\")"));
            }
            Ok(with_node(&e, Node::List(items.into())))
        }
        (ArgKey::Pos(i), Node::List(l), v) if !l.is_empty() => {
            let mut items = vec![l[0].clone()];
            items.extend(with_pos(&l[1..], *i, v));
            Ok(with_node(&e, Node::List(items.into())))
        }
        (ArgKey::Kw(k), Node::List(l), v) if !l.is_empty() && sym_of(&l[0]) == Some("scene/root") => {
            let mut items = vec![l[0].clone()];
            items.extend(root_order(&with_kw(&l[1..], k, v)));
            Ok(with_node(&e, Node::List(items.into())))
        }
        (ArgKey::Kw(k), Node::List(l), v) if !l.is_empty() => {
            let mut items = vec![l[0].clone()];
            items.extend(with_kw(&l[1..], k, v));
            Ok(with_node(&e, Node::List(items.into())))
        }
        _ => Err(fail("This form has no such input.")),
    }
}

// ---- scopes: a let* (or a bare expression) whose bindings are the nodes ----

/// A `let*` (or a bare expression): its bindings in order and its result.
#[derive(Clone, Debug)]
pub struct Scope {
    pub form: S,
    pub vec: S,
    pub ps: Vec<(S, S)>,
    pub res: S,
}

pub fn scope_of(e: &S) -> Option<Scope> {
    if !e.meta().is_empty() {
        return None;
    }
    if let Node::List(l) = &e.node {
        if l.len() == 3 && sym_of(&l[0]) == Some("let*") {
            if let Node::Vec(bs) = &l[1].node {
                if bs.len() % 2 == 0 {
                    return Some(Scope { form: e.clone(), vec: l[1].clone(), ps: pairs(bs), res: l[2].clone() });
                }
            }
        }
    }
    None
}

/// The scope of an expression, a bare one wrapped in an empty `let*`.
pub fn ensure(e: &S) -> Scope {
    match scope_of(e) {
        Some(sc) => sc,
        None => {
            let v = vec(Vec::new());
            Scope { form: mk(Node::List(vec![sym("let*"), v.clone(), e.clone()].into())), vec: v, ps: Vec::new(), res: e.clone() }
        }
    }
}

pub fn rebuild(sc: &Scope, ps: &[(S, S)], res: S) -> S {
    match &sc.form.node {
        Node::List(l) if l.len() == 3 => with_node(
            &sc.form,
            Node::List(vec![l[0].clone(), with_node(&sc.vec, Node::Vec(flat_pairs(ps).into())), res].into()),
        ),
        _ => sc.form.clone(),
    }
}

/// An empty `let*` is its result.
pub fn collapse(sc: &Scope, ps: &[(S, S)], res: S) -> S {
    if ps.is_empty() { res } else { rebuild(sc, ps, res) }
}

// a call written in an argument is a node of the graph too.  It has no binding: its leaf is the
// leaf of the binding (or `@result`) that holds it, then `#` and each argument on the way down
pub fn node_call(e: &S) -> bool {
    match head_sym(e) {
        Some("fn" | "map" | "filter" | "reduce" | "sort-by" | "iterate" | "if" | "cond" | "case" | "with_error" | "error_rows" | "exact") => true,
        Some(h) => h.len() > 1 && h.contains('/'),
        None => false,
    }
}

/// The leaf names a nested node.
pub fn nested(leaf: &str) -> bool {
    leaf.contains('#')
}

// the input a chain runs through: the first input written as a node call, else the first
pub fn chain_in(e: &S) -> ArgKey {
    let mut i = 0;
    loop {
        match arg_get(e, &ArgKey::Pos(i)) {
            Some(a) if node_call(&a) => return ArgKey::Pos(i),
            Some(_) => i += 1,
            None => return ArgKey::Pos(0),
        }
    }
}

pub fn key_segment(key: &ArgKey) -> R<String> {
    match key {
        ArgKey::Pos(i) => Ok(i.to_string()),
        ArgKey::Kw(k) => Ok(format!(":{}", k)),
        ArgKey::Arm(i) => Ok(if *i < 0 {
            "else".to_owned()
        } else if *i == 0 {
            "then".to_owned()
        } else {
            format!("then~{}", i + 1)
        }),
        ArgKey::Cell(i) => Ok(format!("c{}", i)),
        ArgKey::Whole | ArgKey::Field(_) | ArgKey::Bv(..) => Err(fail("Only an input of a call holds a nested node.")),
    }
}

/// The leaf of the node written in input `key` of the node at this leaf.
pub fn nested_leaf(leaf: &str, key: &ArgKey) -> R<String> {
    Ok(format!("{}#{}", leaf, key_segment(key)?))
}

// OCaml `int_of_string_opt` on a decimal string
fn int_opt(t: &str) -> Option<i64> {
    let d = t.strip_prefix('-').unwrap_or(t);
    if d.is_empty() || !d.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    t.parse().ok()
}

pub fn split_leaf(leaf: &str) -> R<(String, Vec<ArgKey>)> {
    let mut parts = leaf.split('#');
    let base = parts.next().unwrap_or("").to_owned();
    let mut keys = Vec::new();
    for k in parts {
        let key = if k == "else" {
            ArgKey::Arm(-1)
        } else if k == "then" {
            ArgKey::Arm(0)
        } else if let Some(rest) = k.strip_prefix("then~") {
            match int_opt(rest) {
                Some(n) => ArgKey::Arm(n - 1),
                None => return Err(fail(&format!("Node {} no longer exists.", leaf))),
            }
        } else if k.len() > 1 && k.starts_with('c') && int_opt(&k[1..]).is_some() {
            ArgKey::Cell(int_opt(&k[1..]).unwrap())
        } else if k.starts_with(':') {
            ArgKey::Kw(k[1..].to_owned())
        } else {
            match int_opt(k) {
                Some(i) => ArgKey::Pos(i),
                None => return Err(fail(&format!("Node {} no longer exists.", leaf))),
            }
        };
        keys.push(key);
    }
    Ok((base, keys))
}

// the node that holds a nested one, and the input it is written in
pub fn holder(leaf: &str) -> R<(String, ArgKey)> {
    let i = leaf.rfind('#').ok_or_else(|| fail("Not a nested node."))?;
    let (_, keys) = split_leaf(&leaf[i..])?;
    let key = keys.into_iter().next().ok_or_else(|| fail("Not a nested node."))?;
    Ok((leaf[..i].to_owned(), key))
}

pub fn find_pair_in(ps: &[(S, S)], leaf: &str) -> Option<usize> {
    // ponytail: a malformed key reads as its base name here, where the OCaml raises
    let base = leaf.split('#').next().unwrap_or("");
    ps.iter().position(|(p, _)| pat_key(p) == base)
}

pub fn find_pair(sc: &Scope, leaf: &str) -> Option<usize> {
    find_pair_in(&sc.ps, leaf)
}

pub fn is_zone(e: &S) -> bool {
    e.meta().is_empty() && matches!(head_sym(e), Some("for" | "fold" | "scan" | "sum" | "state" | "fn"))
}

pub fn last_child(e: &S) -> R<S> {
    s::children(e).last().map(|c| (*c).clone()).ok_or_else(|| fail("Empty form."))
}

pub fn set_last(e: &S, v: S) -> S {
    let mut l = children(e);
    if let Some(last) = l.last_mut() {
        *last = v;
    }
    with_node(e, Node::List(l.into()))
}

// the scope inside a binding value: a let* itself, or a zone's body
pub fn enter(e: &S) -> R<S> {
    if scope_of(e).is_some() {
        Ok(e.clone())
    } else if is_zone(e) {
        last_child(e)
    } else {
        Err(fail("That node is not a scope."))
    }
}

pub fn leave(e: &S, entered: S) -> S {
    if scope_of(e).is_some() { entered } else { set_last(e, entered) }
}

fn inside(child: &S, keys: &[ArgKey], name: &str, g: &mut dyn FnMut(&S) -> R<S>) -> R<S> {
    match keys {
        [] => g(child),
        [key, rest @ ..] => {
            let argument = arg_get(child, key).ok_or_else(|| fail(&format!("Node {} no longer exists.", name)))?;
            let inner = inside(&argument, rest, name, g)?;
            arg_set(child, key, Some(inner))
        }
    }
}

pub fn descend(cur: &S, names: &[String], f: &mut dyn FnMut(&S) -> R<S>) -> R<S> {
    let Some((name, rest)) = names.split_first() else {
        return f(cur);
    };
    let (base, keys) = split_leaf(name)?;
    // the child, and how to put a changed one back
    enum Back {
        Scope(Scope, Option<usize>),
        Id,
    }
    let (child, back) = match scope_of(cur) {
        Some(sc) if base == "@result" => (sc.res.clone(), Back::Scope(sc, None)),
        Some(sc) => match find_pair(&sc, name) {
            Some(j) => (sc.ps[j].1.clone(), Back::Scope(sc, Some(j))),
            None => return Err(fail(&format!("Binding {} no longer exists.", name))),
        },
        None if base == "@result" => (cur.clone(), Back::Id),
        None => return Err(fail("Scope no longer exists.")),
    };
    let arms = keys.iter().any(|k| matches!(k, ArgKey::Arm(_)));
    let out = inside(&child, &keys, name, &mut |inner| {
        if arms {
            descend(inner, rest, &mut *f)
        } else {
            let entered = enter(inner)?;
            let r = descend(&entered, rest, &mut *f)?;
            Ok(leave(inner, r))
        }
    })?;
    Ok(match back {
        Back::Id => out,
        Back::Scope(sc, None) => rebuild(&sc, &sc.ps, out),
        Back::Scope(sc, Some(j)) => {
            let mut ps = sc.ps.clone();
            ps[j].1 = out;
            rebuild(&sc, &ps, sc.res.clone())
        }
    })
}

pub fn workspace_parts(src: &[S]) -> R<(S, Vec<S>)> {
    if let Some(ws) = src.first() {
        if let Node::List(l) = &ws.node {
            if l.len() >= 2 && sym_of(&l[0]) == Some("workspace") {
                return Ok((ws.clone(), l[2..].to_vec()));
            }
        }
    }
    Err(fail("The source is not a (workspace ...)."))
}

pub fn root_name(item: &S) -> Option<String> {
    if let Node::List(l) = &item.node {
        if l.len() >= 2 {
            if let (Some(h @ ("graph" | "defn")), Some(n)) = (sym_of(&l[0]), sym_of(&l[1])) {
                return Some(if h == "defn" { format!("def:{}", n) } else { n.to_owned() });
            }
        }
    }
    None
}

// the graphs (other than itself) whose text holds `(ref name ...)`
pub fn graph_readers(items: &[S], name: &str) -> Vec<String> {
    fn reads(e: &S, name: &str) -> bool {
        if let Node::List(l) = &e.node {
            if l.len() >= 2 && sym_of(&l[0]) == Some("ref") && sym_of(&l[1]) == Some(name) {
                return true;
            }
        }
        s::children(e).into_iter().any(|c| reads(c, name))
    }
    items
        .iter()
        .filter_map(|item| match root_name(item) {
            Some(r) if r != name && reads(item, name) => Some(r),
            _ => None,
        })
        .collect()
}

pub fn map_items(src: &[S], f: &mut dyn FnMut(Vec<S>) -> R<Vec<S>>) -> R<Vec<S>> {
    if let Some(ws) = src.first() {
        if let Node::List(l) = &ws.node {
            if l.len() >= 2 {
                let mut items = vec![l[0].clone(), l[1].clone()];
                items.extend(f(l[2..].to_vec())?);
                let mut out = vec![with_node(ws, Node::List(items.into()))];
                out.extend(src[1..].iter().cloned());
                return Ok(out);
            }
        }
    }
    Err(fail("The source is not a (workspace ...)."))
}

pub fn with_root(src: &[S], seg: &str, f: &mut dyn FnMut(&S) -> R<S>) -> R<Vec<S>> {
    let mut hit = false;
    let out = map_items(src, &mut |items| {
        items
            .iter()
            .map(|item| {
                if root_name(item).as_deref() == Some(seg) {
                    hit = true;
                    f(item)
                } else {
                    Ok(item.clone())
                }
            })
            .collect()
    })?;
    if !hit {
        return Err(fail(&format!("No graph or definition {}.", seg)));
    }
    Ok(out)
}

pub fn root_form(src: &[S], seg: &str) -> R<S> {
    let (_, items) = workspace_parts(src)?;
    items
        .into_iter()
        .find(|i| root_name(i).as_deref() == Some(seg))
        .ok_or_else(|| fail(&format!("No graph or definition {}.", seg)))
}

/// `src` with the scope at this path rewritten by `g`.
pub fn edit_scope(src: &[S], scope_path: &[String], g: &mut dyn FnMut(&S) -> R<S>) -> R<Vec<S>> {
    match scope_path.split_first() {
        None => Err(fail("No scope.")),
        Some((seg, names)) => with_root(src, seg, &mut |r| {
            let d = descend(&last_child(r)?, names, &mut *g)?;
            Ok(set_last(r, d))
        }),
    }
}

/// A node path as its scope path and its leaf.
pub fn split_node(node: &[String]) -> R<(Vec<String>, String)> {
    match node {
        [] | [_] => Err(fail("Not a node.")),
        _ => Ok((node[..node.len() - 1].to_vec(), node[node.len() - 1].clone())),
    }
}

pub fn get_node(sx: &S, leaf: &str) -> R<S> {
    if nested(leaf) {
        let (base, keys) = split_leaf(leaf)?;
        let mut e = get_node(sx, &base)?;
        for key in &keys {
            e = arg_get(&e, key).ok_or_else(|| fail(&format!("Node {} no longer exists.", leaf)))?;
        }
        Ok(e)
    } else {
        match scope_of(sx) {
            Some(sc) if leaf == "@result" => Ok(sc.res),
            Some(sc) => match find_pair(&sc, leaf) {
                Some(j) => Ok(sc.ps[j].1.clone()),
                None => Err(fail(&format!("Binding {} no longer exists.", leaf))),
            },
            None if leaf == "@result" => Ok(sx.clone()),
            None => Err(fail(&format!("Binding {} no longer exists.", leaf))),
        }
    }
}

/// Bindings in dependency order.
pub fn reorder(sx: &S) -> R<S> {
    let Some(sc) = scope_of(sx) else {
        return Ok(sx.clone());
    };
    // the first binding whose reads are all bound goes next; by name in two tables, and a scope
    // already in order (every scrub) is one pass
    let mut names: HashSet<String> = HashSet::new();
    let mut seen: HashSet<String> = HashSet::new();
    for (p, _) in &sc.ps {
        for n in pat_names(p) {
            names.insert(n);
        }
    }
    let frees: Vec<Vec<String>> = sc.ps.iter().map(|(_, e)| free(e)).collect();
    let mut left: Vec<usize> = (0..sc.ps.len()).collect();
    let mut out: Vec<usize> = Vec::with_capacity(left.len());
    let ready = |k: usize, seen: &HashSet<String>| frees[k].iter().all(|x| !names.contains(x) || seen.contains(x));
    while !left.is_empty() {
        let pick = if ready(left[0], &seen) { Some(0) } else { left.iter().position(|k| ready(*k, &seen)) };
        match pick {
            None => {
                let cyc: Vec<String> = left.iter().map(|k| pat_key(&sc.ps[*k].0)).collect();
                return Err(fail_code("E_GRAPH_CYCLE", &format!("That connection would make a cycle through {}.", cyc.join(", "))));
            }
            Some(i) => {
                let k = left.remove(i);
                for n in pat_names(&sc.ps[k].0) {
                    seen.insert(n);
                }
                out.push(k);
            }
        }
    }
    let ps: Vec<(S, S)> = out.into_iter().map(|k| sc.ps[k].clone()).collect();
    Ok(rebuild(&sc, &ps, sc.res.clone()))
}

pub fn set_node(sx: &S, leaf: &str, e: S) -> R<S> {
    if nested(leaf) {
        let (base, keys) = split_leaf(leaf)?;
        fn put(cur: &S, keys: &[ArgKey], e: &S, leaf: &str) -> R<S> {
            match keys {
                [] => Ok(keep_notes(cur, e.clone())),
                [key, rest @ ..] => match arg_get(cur, key) {
                    Some(a) => arg_set(cur, key, Some(put(&a, rest, e, leaf)?)),
                    None => Err(fail(&format!("Node {} no longer exists.", leaf))),
                },
            }
        }
        let b = get_node(sx, &base)?;
        set_node(sx, &base, put(&b, &keys, &e, leaf)?)
    } else {
        match scope_of(sx) {
            Some(sc) if leaf == "@result" => Ok(rebuild(&sc, &sc.ps, keep_notes(&sc.res, e))),
            Some(sc) => match find_pair(&sc, leaf) {
                Some(j) => {
                    let mut ps = sc.ps.clone();
                    ps[j].1 = keep_notes(&ps[j].1, e);
                    Ok(rebuild(&sc, &ps, sc.res.clone()))
                }
                None => Err(fail(&format!("Binding {} no longer exists.", leaf))),
            },
            None if leaf == "@result" => Ok(e),
            None => Err(fail(&format!("Binding {} no longer exists.", leaf))),
        }
    }
}

// ---- names ----

fn sanitize(base: &str) -> String {
    let base = match base.rfind('/') {
        Some(i) => &base[i + 1..],
        None => base,
    };
    let mut b = String::new();
    for c in base.bytes() {
        match c.to_ascii_lowercase() {
            c @ (b'a'..=b'z' | b'0'..=b'9' | b'_') => b.push(c as char),
            _ => b.push('_'),
        }
    }
    let t = b.trim_matches('_').to_owned();
    if t.is_empty() {
        "node".to_owned()
    } else if t.as_bytes()[0].is_ascii_lowercase() {
        t
    } else {
        format!("n{}", t)
    }
}

pub fn fresh(used: &mut Vec<String>, base: &str) -> String {
    let base = sanitize(base);
    let mut n = 1;
    let name = loop {
        let name = if n == 1 { base.clone() } else { format!("{}_{}", base, n) };
        if used.contains(&name) || w::name_taken(&name) {
            n += 1;
        } else {
            break name;
        }
    };
    used.push(name.clone());
    name
}

pub fn root_used(src: &[S], seg: &str) -> Vec<String> {
    match workspace_parts(src) {
        Ok((_, items)) => items.iter().filter(|i| root_name(i).as_deref() == Some(seg)).flat_map(sym_list).collect(),
        Err(_) => Vec::new(),
    }
}

/// A name unused in the graph or definition `root` and not taken by an operator or special form.
pub fn fresh_name(src: &[S], root: &str, base: &str) -> String {
    fresh(&mut root_used(src, root), base)
}

pub fn fresh_among(used: &mut Vec<String>, base: &str) -> String {
    fresh(used, base)
}

/// The name a binding of this operator starts with: `+` is `sum`, a comparison `test`.
pub fn value_base(op: &str) -> String {
    match op {
        "+" => "sum",
        "-" => "difference",
        "*" => "product",
        "/" => "quotient",
        "<" | ">" | "<=" | ">=" | "=" => "test",
        op => op,
    }
    .to_owned()
}

fn value_ops() -> &'static HashSet<String> {
    static OPS: OnceLock<HashSet<String>> = OnceLock::new();
    OPS.get_or_init(|| w::value_ops().into_iter().collect())
}

/// A plain operator call: a card of its own only while it is written inside its reader.
pub fn value_call(e: &S) -> bool {
    !node_call(e)
        && !is_zone(e)
        && match head_sym(e) {
            Some(h) => h != "list" && h != "let*" && value_ops().contains(h),
            None => false,
        }
}

/// The frame context symbol, drawn as a value card of its own while written in an input.
pub fn context_sym(e: &S) -> bool {
    matches!(&e.node, Node::Sym(n) if n == "t")
}

// still called what `fresh` would call it: the base, then maybe `_n`
pub fn auto_named(name: &str, e: &S) -> bool {
    let b = sanitize(&value_base(head_sym(e).unwrap_or("")));
    let n = b.len();
    name == b || (name.starts_with(&format!("{}_", b)) && name.len() > n + 1 && name[n + 1..].bytes().all(|c| c.is_ascii_digit()))
}

pub fn valid_name(n: &str) -> bool {
    symbol::valid_name(n)
}

fn nums(items: &[&str]) -> Vec<S> {
    items.iter().map(|n| mk(Node::Num((*n).to_owned()))).collect()
}

/// The value a disconnected input of this type falls back to; `None` for types with none.
pub fn default_for(t: &Ty, label: &str, fresh_param: Option<&dyn Fn(&str) -> String>) -> Option<S> {
    let fp = |n: &str| match fresh_param {
        Some(f) => f(n),
        None => n.to_owned(),
    };
    match t {
        Ty::Float => Some(mk(Node::Num("0.5".into()))),
        Ty::Int => Some(mk(Node::Num("1".into()))),
        Ty::Bool => Some(sym("false")),
        Ty::Vec3 => Some(vec(nums(&["0", "0", "0"]))),
        Ty::Vec2 => Some(vec(nums(&["0", "0"]))),
        Ty::Vec4 => Some(vec(nums(&["0", "0", "0", "0"]))),
        Ty::Color => Some(text("#285f77")),
        Ty::Text => Some(text(if label == "color" { "#285f77" } else { "text" })),
        Ty::Array(inner) => match inner.as_ref() {
            Ty::Float | Ty::Any => Some(call("array/float", nums(&["4"]))),
            Ty::Int => Some(call("array/int", nums(&["0"]))),
            Ty::Text => Some(call("array/text", nums(&["4"]))),
            Ty::Vec2 => {
                let name = fp("element");
                Some(call(
                    "map",
                    vec![call("fn", vec![vec_of(vec![sym(&name)]), vec_of(vec![sym(&name), sym(&name)])]), call("array/float", nums(&["4"]))],
                ))
            }
            Ty::Vec3 => Some(call("array/vec3", nums(&["4"]))),
            Ty::Vec4 => Some(call("array/vec4", nums(&["4"]))),
            _ => None,
        },
        Ty::Record(_) => branch_default(t),
        Ty::Named(_) => ty::default(t),
        Ty::Fn(Some(signature)) => branch_default(&signature.result).map(|body| {
            let params = (0..signature.params.len()).map(|i| sym(&fp(&format!("arg{}", i)))).collect();
            call("fn", vec![vec_of(params), body])
        }),
        _ => None,
    }
}

pub fn branch_default(t: &Ty) -> Option<S> {
    match t {
        Ty::List(_) => Some(call("list", Vec::new())),
        Ty::Record(fields) => {
            let mut out = Vec::new();
            for (name, ty) in fields {
                let value = branch_default(ty)?;
                out.push(kwf(name));
                out.push(value);
            }
            Some(mk(Node::Map(out.into())))
        }
        _ => default_for(t, "value", None),
    }
}

/// Number and text leaves of an expression with their child paths, the candidates for macro holes.
pub fn literals(e: &S) -> Vec<(Vec<i64>, S)> {
    fn go(e: &S, path: &mut Vec<i64>, out: &mut Vec<(Vec<i64>, S)>) {
        match &e.node {
            Node::Num(_) | Node::Str(_) => out.push((path.clone(), e.clone())),
            Node::List(_) | Node::Vec(_) | Node::Map(_) => {
                for (i, c) in s::children(e).into_iter().enumerate() {
                    if (i == 0 && matches!(e.node, Node::List(_))) || matches!(c.node, Node::Kw(_)) {
                        continue;
                    }
                    path.push(i as i64);
                    go(c, path, out);
                    path.pop();
                }
            }
            _ => {}
        }
    }
    let mut out = Vec::new();
    go(e, &mut Vec::new(), &mut out);
    out
}

// ---- selections ----

pub struct Selection {
    pub sc: Scope,
    pub sel: Vec<(S, S)>,
    pub keep: Vec<(S, S)>,
    pub first: usize,
    pub out_idx: usize,
    pub out_name: String,
}

impl Selection {
    pub fn out(&self) -> &(S, S) {
        &self.sel[self.out_idx]
    }
}

pub fn scope_path_of(nodes: &[Path]) -> R<Vec<String>> {
    match nodes.split_first() {
        None => Err(fail("Select the nodes first.")),
        Some((first, rest)) => {
            let (sp, _) = split_node(first)?;
            for n in rest {
                if split_node(n)?.0 != sp {
                    return Err(fail("Select nodes from one scope."));
                }
            }
            Ok(sp)
        }
    }
}

/// The bindings of `nodes` in scope expression `sx`, the one leaving them.
pub fn select(sx: &S, nodes: &[Path], what: &str) -> R<Selection> {
    let leaves: Vec<String> = nodes.iter().map(|n| split_node(n).map(|x| x.1)).collect::<R<_>>()?;
    if leaves.iter().any(|l| l == "@result") {
        return Err(fail(&format!("A result cannot be part of {}.", what)));
    }
    if leaves.iter().any(|l| nested(l)) {
        return Err(fail(&format!("Name the nodes first (rename them): {} needs named nodes.", what)));
    }
    let sc = ensure(sx);
    let sel: Vec<(S, S)> = sc.ps.iter().filter(|(p, _)| leaves.contains(&pat_key(p))).cloned().collect();
    if sel.len() != dedup(&leaves).len() {
        return Err(fail("A selected node no longer exists."));
    }
    let keep: Vec<(S, S)> = sc.ps.iter().filter(|(p, _)| !leaves.contains(&pat_key(p))).cloned().collect();
    let first = sc.ps.iter().position(|(p, _)| leaves.contains(&pat_key(p))).ok_or_else(|| fail("A selected node no longer exists."))?;
    let keep_free: Vec<Vec<String>> = keep.iter().map(|(_, e)| free(e)).collect();
    let res_free = free(&sc.res);
    let mentioned = |n: &String| keep_free.iter().any(|f| f.contains(n)) || res_free.contains(n);
    let outs: Vec<usize> = (0..sel.len()).filter(|k| pat_names(&sel[*k].0).iter().any(&mentioned)).collect();
    if outs.len() > 1 {
        let names: Vec<String> = outs.iter().map(|k| pat_key(&sel[*k].0)).collect();
        return Err(fail(&format!("{} needs one result leaving the selection; {} are all used outside.", what, names.join(" and "))));
    }
    let out_idx = if outs.len() == 1 { outs[0] } else { sel.len() - 1 };
    let out_name = match &sel[out_idx].0.node {
        Node::Sym(n) => n.clone(),
        _ => return Err(fail(&format!("The result of {} must be a name.", what))),
    };
    Ok(Selection { sc, sel, keep, first, out_idx, out_name })
}

pub fn body_of(x: &Selection) -> S {
    if x.sel.len() == 1 {
        x.sel[0].1.clone()
    } else {
        let mut others: Vec<(S, S)> = x.sel.iter().enumerate().filter(|(k, _)| *k != x.out_idx).map(|(_, q)| q.clone()).collect();
        others.push(x.out().clone());
        call("let*", vec![vec_of(flat_pairs(&others)), sym(&x.out_name)])
    }
}

pub fn insert_at(keep: &[(S, S)], i: i64, extra: &[(S, S)]) -> Vec<(S, S)> {
    let i = i.max(0).min(keep.len() as i64) as usize;
    let mut out = keep[..i].to_vec();
    out.extend(extra.iter().cloned());
    out.extend(keep[i..].iter().cloned());
    out
}

pub fn sel_names(x: &Selection) -> Vec<String> {
    x.sel.iter().flat_map(|(p, _)| pat_names(p)).collect()
}

pub fn sel_declared(x: &Selection) -> Vec<String> {
    let mut acc = Vec::new();
    for (_, e) in &x.sel {
        declared_into(e, &mut acc);
    }
    acc
}

/// Names the selection reads from outside, in order of appearance.
pub fn outside_names(root: &S, x: &Selection) -> Vec<String> {
    let visible = declared(root);
    let inner = sel_declared(x);
    let own = sel_names(x);
    let all: Vec<String> = x.sel.iter().flat_map(|(_, e)| sym_list(e)).collect();
    dedup(&all).into_iter().filter(|n| visible.contains(n) && !inner.contains(n) && !own.contains(n)).collect()
}

pub fn fn_names(root: &S) -> Vec<String> {
    fn go(e: &S, acc: &mut Vec<String>) {
        if let Node::List(l) = &e.node {
            if l.len() >= 2 && sym_of(&l[0]) == Some("let*") {
                if let Node::Vec(bs) = &l[1].node {
                    for (p, v) in pairs(bs) {
                        if head_sym(&v) == Some("fn") {
                            acc.extend(pat_names(&p));
                        }
                    }
                }
            }
        }
        for c in s::children(e) {
            go(c, acc);
        }
    }
    let mut acc = Vec::new();
    go(root, &mut acc);
    acc
}

// ---- the rewrites; each returns candidate sources, tried in order ----

pub fn first_pair_index(sc: &Scope, leaf: &str) -> R<usize> {
    find_pair(sc, leaf).ok_or_else(|| fail(&format!("Binding {} no longer exists.", leaf)))
}

pub fn note_lines(t: &str) -> Vec<String> {
    let t = t.trim_matches(|c| matches!(c, ' ' | '\t' | '\n' | '\r' | '\u{000c}'));
    if t.is_empty() {
        Vec::new()
    } else {
        t.split('\n').map(|l| l.trim_matches(|c| matches!(c, ' ' | '\t' | '\n' | '\r' | '\u{000c}')).to_owned()).collect()
    }
}

// ---- scene objects and World layers: they join and leave their graph's result ----

pub fn starts_with(prefix: &str, h: Option<&str>) -> bool {
    h.is_some_and(|h| h.starts_with(prefix))
}

pub enum Target {
    At(usize),
    Result,
}

// where the result of a scope is written: a named binding, or the result itself
pub fn result_target(sc: &Scope) -> (Target, S) {
    if let Node::Sym(r) = &sc.res.node {
        if let Some(j) = find_pair(sc, r) {
            return (Target::At(j), sc.ps[j].1.clone());
        }
    }
    (Target::Result, sc.res.clone())
}

fn put_target(sc: &Scope, ps: &[(S, S)], target: &Target, e: S) -> S {
    match target {
        Target::At(j) => {
            let mut ps = ps.to_vec();
            ps[*j].1 = keep_notes(&ps[*j].1, e);
            rebuild(sc, &ps, sc.res.clone())
        }
        Target::Result => rebuild(sc, ps, keep_notes(&sc.res, e)),
    }
}

fn add_into(m: &S, name: &str) -> R<S> {
    let n = positional(&children(m)[1..]).len() as i64;
    arg_set(m, &ArgKey::Pos(n), Some(sym(name)))
}

// the merge a new scene object joins: the result's, looking through the root; `ps` holds the new
// binding last
fn join_merge(sc: &Scope, ps: &[(S, S)], name: &str) -> R<S> {
    let (target, e) = result_target(sc);
    match head_sym(&e) {
        Some("scene/merge") => Ok(put_target(sc, ps, &target, add_into(&e, name)?)),
        Some("scene/root") => {
            let wrap = |inner: S| -> R<S> { Ok(put_target(sc, ps, &target, arg_set(&e, &ArgKey::Pos(0), Some(inner))?)) };
            match arg_get(&e, &ArgKey::Pos(0)) {
                Some(a) if matches!(a.node, Node::Sym(_)) => {
                    let m = sym_of(&a).unwrap().to_owned();
                    match find_pair_in(ps, &m) {
                        Some(j) if head_sym(&ps[j].1) == Some("scene/merge") => {
                            let mut ps2 = ps.to_vec();
                            let merged = add_into(&ps2[j].1, name)?;
                            ps2[j].1 = keep_notes(&ps2[j].1, merged);
                            Ok(rebuild(sc, &ps2, sc.res.clone()))
                        }
                        _ => wrap(call("scene/merge", vec![sym(&m), sym(name)])),
                    }
                }
                Some(inner) if head_sym(&inner) == Some("scene/merge") => wrap(add_into(&inner, name)?),
                Some(inner) => wrap(call("scene/merge", vec![inner, sym(name)])),
                None => wrap(sym(name)),
            }
        }
        _ => Ok(rebuild(sc, ps, call("scene/merge", vec![sc.res.clone(), sym(name)]))),
    }
}

// a new scene object is one more argument of the scene's `scene/merge`; a new root takes the
// result; a new World layer goes on top of the stack.  `ps` holds the new binding last.
pub fn attach(sc: &Scope, ps: &[(S, S)], name: &str, expr: &S) -> R<(S, S)> {
    let head = head_sym(expr);
    let (_, e) = result_target(sc);
    if head == Some("scene/root") {
        if head_sym(&e) == Some("scene/root") {
            return Err(fail("The scene already has a root."));
        }
        Ok((collapse(sc, &sc.ps, arg_set(expr, &ArgKey::Pos(0), Some(sc.res.clone()))?), expr.clone()))
    } else if starts_with("scene/", head) && head != Some("scene/merge") {
        Ok((join_merge(sc, ps, name)?, expr.clone()))
    } else if starts_with("world/", head) {
        match head_sym(&e) {
            Some("world/none") => Ok((rebuild(sc, ps, sym(name)), expr.clone())),
            Some(_) => Ok((rebuild(sc, ps, sym(name)), arg_set(expr, &ArgKey::Pos(0), Some(sc.res.clone()))?)),
            None => Ok((rebuild(sc, ps, sc.res.clone()), expr.clone())),
        }
    } else {
        Ok((rebuild(sc, ps, sc.res.clone()), expr.clone()))
    }
}

// a deleted object leaves the merge that held it; a deleted layer leaves the stack, the layer
// above it taking the one below
pub fn detach(name: &str, below: &Option<S>, e: &S) -> R<S> {
    let e = try_map_children(&mut |c| detach(name, below, c), e)?;
    match head_sym(&e) {
        Some("scene/merge") => {
            let kids = children(&e);
            let args = &kids[1..];
            let gone: Vec<i64> = positional(args)
                .iter()
                .enumerate()
                .filter(|(_, c)| c.node == Node::Sym(name.to_owned()))
                .map(|(i, _)| i as i64)
                .collect();
            let kept: Vec<S> = args.iter().filter(|c| c.node != Node::Sym(name.to_owned())).cloned().collect();
            let mut items = vec![kids[0].clone()];
            items.extend(skip_after_removal(&kept, &gone));
            Ok(with_node(&e, Node::List(items.into())))
        }
        Some(h) if h.starts_with("world/") => match arg_get(&e, &ArgKey::Pos(0)) {
            Some(a) if matches!(&a.node, Node::Sym(n) if n == name) => match below {
                Some(b) => arg_set(&e, &ArgKey::Pos(0), Some(b.clone())),
                None => Err(fail("This form has no such input.")),
            },
            _ => Ok(e),
        },
        _ => Ok(e),
    }
}

// a deleted World layer that was a world graph's result hands the result to the layer below
// it, or to `(world/none)`
pub fn detach_result(name: &str, below: &Option<S>, root: &S) -> R<S> {
    let top = match below {
        Some(b) => b.clone(),
        None => call("world/none", Vec::new()),
    };
    if matches!(head_sym(root), Some("graph")) {
        let body = last_child(root)?;
        match scope_of(&body) {
            Some(sc) if sc.res.node == Node::Sym(name.to_owned()) => Ok(set_last(root, rebuild(&sc, &sc.ps, top))),
            Some(_) => Ok(root.clone()),
            None => Ok(if body.node == Node::Sym(name.to_owned()) { set_last(root, top) } else { root.clone() }),
        }
    } else {
        Ok(root.clone())
    }
}

// the copies a duplicate makes: each selected binding with a fresh name; the copies read each
// other where the originals did
pub fn duplicate_plan(src: &[S], nodes: &[Path]) -> R<(Vec<String>, Vec<(String, String)>)> {
    let sp = scope_path_of(nodes)?;
    let mut used = root_used(src, &sp[0]);
    let leaves = dedup(&nodes.iter().map(|n| split_node(n).map(|x| x.1)).collect::<R<Vec<_>>>()?);
    if leaves.iter().any(|l| l == "@result") {
        return Err(fail("The result cannot be duplicated."));
    }
    if leaves.iter().any(|l| nested(l)) {
        return Err(fail("Name the node first (rename it), then duplicate it."));
    }
    let names = leaves.iter().map(|leaf| (leaf.clone(), fresh(&mut used, leaf))).collect();
    Ok((sp, names))
}

pub fn has_prefix(prefix: &[String], p: &[String]) -> bool {
    p.len() >= prefix.len() && p[..prefix.len()] == *prefix
}


/// Names a form reads and does not declare, first use first (field access `a.b` counts as `a`).
pub fn free_names(e: &S) -> Vec<String> {
    dedup(&free(e))
}
