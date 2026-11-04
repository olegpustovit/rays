//! Port of lib/editor_document/layout_by_path.ml: canvas layout keyed by workspace path: the
//! fields of the workspace's active layout. A path is the lexical identity of a binding, so a key
//! survives every text edit that keeps the path. Persisted as one `(layout ...)` s-expression.
use editor_core::panels::{self, State};
use flow_graph::projection::{self, Level};
use flow_syntax::lisp;
use flow_syntax::syntax::{self as s, Node, Syntax};
use std::collections::BTreeMap;

pub type Path = Vec<String>;
pub type PathMap<V> = BTreeMap<Path, V>;
pub type StringMap<V> = BTreeMap<String, V>;

/// A titled rectangle on the canvas of the scope the frame is keyed by.
#[derive(Clone, Debug, PartialEq)]
pub struct Frame {
    pub title: String,
    pub at: (f64, f64),
    pub size: (f64, f64),
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct T {
    /// The selected editor graph (a named shell layout); first by default.
    pub editor: Option<String>,
    /// Saved panel disclosure and floating window bounds, keyed by editor graph and binding.
    pub panels: PathMap<State>,
    pub at: PathMap<(f64, f64)>,
    /// Opened explicitly: the node ignores the zoom caps.
    pub pinned: PathMap<bool>,
    /// The detail level of a node (absent: a card), by path like `at`.
    pub level: PathMap<Level>,
    pub rows: PathMap<StringMap<bool>>,
    pub collapsed: PathMap<bool>,
    pub frames: PathMap<Vec<Frame>>,
}

pub fn empty() -> T {
    T::default()
}

pub fn is_empty(t: &T) -> bool {
    *t == T::default()
}

/// Rewrite every key; `None` drops it.
pub fn remap(f: &dyn Fn(&Path) -> Option<Path>, t: &T) -> T {
    fn keys<V: Clone>(f: &dyn Fn(&Path) -> Option<Path>, m: &PathMap<V>) -> PathMap<V> {
        let mut out = PathMap::new();
        for (k, v) in m {
            if let Some(k) = f(k) {
                out.insert(k, v.clone());
            }
        }
        out
    }
    T {
        editor: t.editor.as_ref().and_then(|name| match f(&vec![name.clone()]) {
            Some(p) if p.len() == 1 => Some(p[0].clone()),
            _ => None,
        }),
        panels: keys(f, &t.panels),
        at: keys(f, &t.at),
        pinned: keys(f, &t.pinned),
        level: keys(f, &t.level),
        rows: keys(f, &t.rows),
        collapsed: keys(f, &t.collapsed),
        frames: keys(f, &t.frames),
    }
}

// ---- s-expression ----

fn mk(node: Node) -> Syntax {
    s::make(node)
}
fn str(text: &str) -> Syntax {
    mk(Node::Str(text.as_bytes().to_vec()))
}
fn kw(text: &str) -> Syntax {
    mk(Node::Kw(text.to_owned()))
}
fn sym(text: &str) -> Syntax {
    mk(Node::Sym(text.to_owned()))
}
fn vec(l: Vec<Syntax>) -> Syntax {
    mk(Node::Vec(l))
}
fn bool_(b: bool) -> Syntax {
    sym(if b { "true" } else { "false" })
}

fn number(x: f64) -> Syntax {
    // positions are whole points far more often than not: keep them short
    mk(Node::Num(if x.fract() == 0. && x.is_finite() && x.abs() < 1e15 { cfmt::f(x, 0) } else { lisp::float(x) }))
}

fn pair(p: (f64, f64)) -> Syntax {
    vec(vec![number(p.0), number(p.1)])
}

fn path_form(p: &Path) -> Syntax {
    vec(p.iter().map(|x| str(x)).collect())
}

pub fn to_syntax(t: &T) -> Syntax {
    let mut nodes: PathMap<Vec<(Syntax, Syntax)>> = PathMap::new();
    for (p, v) in &t.at {
        nodes.entry(p.clone()).or_default().push((kw("at"), pair(*v)));
    }
    for (p, v) in &t.level {
        nodes.entry(p.clone()).or_default().push((kw("level"), str(projection::level_name(*v))));
    }
    for (p, v) in &t.pinned {
        nodes.entry(p.clone()).or_default().push((kw("pinned"), bool_(*v)));
    }
    for (p, v) in &t.collapsed {
        nodes.entry(p.clone()).or_default().push((kw("collapsed"), bool_(*v)));
    }
    for (p, rows) in &t.rows {
        let mut items = Vec::new();
        for (k, v) in rows {
            items.push(kw(k));
            items.push(bool_(*v));
        }
        nodes.entry(p.clone()).or_default().push((kw("rows"), mk(Node::Map(items))));
    }
    let node_forms: Vec<Syntax> = nodes
        .iter()
        .map(|(p, fields)| {
            let mut items = vec![sym("node"), path_form(p)];
            for (k, v) in fields {
                items.push(k.clone());
                items.push(v.clone());
            }
            mk(Node::List(items))
        })
        .collect();
    let mut frames = Vec::new();
    for (p, fs) in &t.frames {
        for f in fs {
            frames.push(mk(Node::List(vec![sym("frame"), path_form(p), str(&f.title), kw("at"), pair(f.at), kw("size"), pair(f.size)])));
        }
    }
    let editor: Vec<Syntax> = t.editor.iter().map(|name| mk(Node::List(vec![sym("editor"), str(name)]))).collect();
    let panel_forms: Vec<Syntax> = t
        .panels
        .iter()
        .map(|(path, state)| {
            let mut items = vec![sym("panel"), path_form(path), kw("collapsed"), bool_(state.collapsed)];
            if let Some((x, y, w, h)) = state.window {
                items.push(kw("window"));
                items.push(vec([x, y, w, h].iter().map(|n| number(*n as f64)).collect()));
            }
            mk(Node::List(items))
        })
        .collect();
    let mut items = vec![sym("layout")];
    items.extend(editor);
    items.extend(panel_forms);
    items.extend(node_forms);
    items.extend(frames);
    mk(Node::List(items))
}

fn fail<V>(message: String) -> Result<V, String> {
    Err(format!("layout: {message}"))
}

fn read_number(f: &Syntax) -> Result<f64, String> {
    match &f.node {
        Node::Num(text) => match cfmt::parse_ocaml_float(text) {
            Some(x) if x.is_finite() => Ok(x),
            _ => fail(format!("bad number {text}")),
        },
        _ => fail("expected a number".to_owned()),
    }
}

fn read_pair(f: &Syntax) -> Result<(f64, f64), String> {
    match &f.node {
        Node::Vec(items) if items.len() == 2 => {
            let x = read_number(&items[0])?;
            let y = read_number(&items[1])?;
            Ok((x, y))
        }
        _ => fail("expected [x y]".to_owned()),
    }
}

fn read_bool(f: &Syntax) -> Result<bool, String> {
    match &f.node {
        Node::Sym(t) if t == "true" => Ok(true),
        Node::Sym(t) if t == "false" => Ok(false),
        _ => fail("expected true or false".to_owned()),
    }
}

fn read_str(f: &Syntax) -> Result<String, String> {
    match &f.node {
        Node::Str(text) => Ok(String::from_utf8_lossy(text).into_owned()),
        _ => fail("expected text".to_owned()),
    }
}

fn read_path(f: &Syntax) -> Result<Path, String> {
    match &f.node {
        Node::Vec(l) => l.iter().map(read_str).collect(),
        _ => fail("expected a path vector".to_owned()),
    }
}

pub fn of_syntax(form: &Syntax) -> Result<T, String> {
    let Node::List(items) = &form.node else { return fail("expected (layout ...)".to_owned()) };
    match items.split_first() {
        Some((head, rest)) if matches!(&head.node, Node::Sym(h) if h == "layout") => {
            let mut t = empty();
            for item in rest {
                t = read_item(t, item)?;
            }
            Ok(t)
        }
        _ => fail("expected (layout ...)".to_owned()),
    }
}

fn read_item(mut t: T, item: &Syntax) -> Result<T, String> {
    let Node::List(parts) = &item.node else { return unknown(item) };
    let head = match parts.first().map(|h| &h.node) {
        Some(Node::Sym(h)) => h.as_str(),
        _ => return unknown(item),
    };
    match (head, &parts[1..]) {
        ("panel", [p, fields @ ..]) => {
            let p = read_path(p)?;
            let mut state = panels::default_state();
            let mut rest = fields;
            loop {
                match rest {
                    [] => {
                        t.panels.insert(p, state);
                        return Ok(t);
                    }
                    [k, v, tail @ ..] if matches!(&k.node, Node::Kw(k) if k == "collapsed") => {
                        state.collapsed = read_bool(v)?;
                        rest = tail;
                    }
                    [k, Syntax { node: Node::Vec(values), .. }, tail @ ..] if matches!(&k.node, Node::Kw(k) if k == "window") => {
                        let values: Vec<f64> = values.iter().map(read_number).collect::<Result<_, _>>()?;
                        match values.as_slice() {
                            [x, y, w, h] if *w >= 120. && *h >= 80. && values.iter().all(|n| n.abs() <= 1e6) => {
                                state.window = Some((*x as i64, *y as i64, *w as i64, *h as i64));
                                rest = tail;
                            }
                            _ => return fail("window needs [x y width height], width >= 120 and height >= 80".to_owned()),
                        }
                    }
                    _ => return fail("bad panel entry".to_owned()),
                }
            }
        }
        ("editor", [name]) => {
            let name = read_str(name)?;
            if t.editor.is_some() {
                fail("duplicate editor entry".to_owned())
            } else {
                t.editor = Some(name);
                Ok(t)
            }
        }
        ("node", [p, fields @ ..]) => {
            let p = read_path(p)?;
            let mut rest = fields;
            loop {
                match rest {
                    [] => return Ok(t),
                    [k, v, tail @ ..] if matches!(&k.node, Node::Kw(k) if k == "at") => {
                        t.at.insert(p.clone(), read_pair(v)?);
                        rest = tail;
                    }
                    [k, v, tail @ ..] if matches!(&k.node, Node::Kw(k) if k == "level") => {
                        let name = read_str(v)?;
                        match projection::level_of_name(&name) {
                            Some(l) => {
                                t.level.insert(p.clone(), l);
                            }
                            None => return fail(format!("unknown level {name}")),
                        }
                        rest = tail;
                    }
                    [k, v, tail @ ..] if matches!(&k.node, Node::Kw(k) if k == "pinned") => {
                        t.pinned.insert(p.clone(), read_bool(v)?);
                        rest = tail;
                    }
                    [k, v, tail @ ..] if matches!(&k.node, Node::Kw(k) if k == "collapsed") => {
                        t.collapsed.insert(p.clone(), read_bool(v)?);
                        rest = tail;
                    }
                    [k, Syntax { node: Node::Map(kvs), .. }, tail @ ..] if matches!(&k.node, Node::Kw(k) if k == "rows") => {
                        let mut m = StringMap::new();
                        let mut r = kvs.as_slice();
                        loop {
                            match r {
                                [] => break,
                                [Syntax { node: Node::Kw(key), .. }, v, tail @ ..] => {
                                    m.insert(key.clone(), read_bool(v)?);
                                    r = tail;
                                }
                                _ => return fail("bad rows".to_owned()),
                            }
                        }
                        t.rows.insert(p.clone(), m);
                        rest = tail;
                    }
                    _ => return fail("bad node entry".to_owned()),
                }
            }
        }
        // entries of features that are gone: an older file still opens, and saves without them
        ("display" | "bend" | "wireless", _) => Ok(t),
        ("frame", [p, title, a1, at, a2, size]) if matches!(&a1.node, Node::Kw(k) if k == "at") && matches!(&a2.node, Node::Kw(k) if k == "size") => {
            let p = read_path(p)?;
            let title = read_str(title)?;
            let at = read_pair(at)?;
            let size = read_pair(size)?;
            t.frames.entry(p).or_default().push(Frame { title, at, size });
            Ok(t)
        }
        _ => unknown(item),
    }
}

fn unknown(item: &Syntax) -> Result<T, String> {
    fail(format!("unknown entry {}", lisp::flat(item)))
}
