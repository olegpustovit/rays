//! canvas layout keyed by workspace path: the
//! fields of the workspace's active layout. A path is the lexical identity of a binding, so a key
//! survives every text edit that keeps the path. Persisted as one `(layout ...)` s-expression.
use editor_core::panels::{self, State};
use flow_graph::projection::{self, Level};
use flow_syntax::lisp;
use flow_syntax::syntax::{Node, Syntax};
use flow_syntax::arena::{Draft, SyntaxBuilder, SyntaxId};
use std::collections::BTreeMap;

pub type Path = flow_check::workspace::Path;
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
        editor: t.editor.as_ref().and_then(|name| match f(&vec![name.clone().into()]) {
            Some(p) if p.len() == 1 => Some(p[0].to_string()),
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

fn mk(node: Draft, builder: &mut SyntaxBuilder) -> SyntaxId {
    builder.push_draft(0, node, rays_diagnostic::Span { start: 0, finish: 0 }, None)
}
fn str(text: &str, builder: &mut SyntaxBuilder) -> SyntaxId {
    mk(Draft::Str(text.as_bytes().to_vec()), builder)
}
fn kw(text: &str, builder: &mut SyntaxBuilder) -> SyntaxId {
    mk(Draft::Kw(text.to_owned()), builder)
}
fn sym(text: &str, builder: &mut SyntaxBuilder) -> SyntaxId {
    mk(Draft::Sym(text.to_owned()), builder)
}
fn vec(items: Vec<SyntaxId>, builder: &mut SyntaxBuilder) -> SyntaxId {
    mk(Draft::Vec(items), builder)
}
fn bool_(value: bool, builder: &mut SyntaxBuilder) -> SyntaxId {
    sym(if value { "true" } else { "false" }, builder)
}

fn number(x: f64, builder: &mut SyntaxBuilder) -> SyntaxId {
    // positions are whole points far more often than not: keep them short
    mk(Draft::Num(if x.fract() == 0. && x.is_finite() && x.abs() < 1e15 { cfmt::f(x, 0) } else { lisp::float(x) }), builder)
}

fn pair(p: (f64, f64), builder: &mut SyntaxBuilder) -> SyntaxId {
    vec(vec![number(p.0, builder), number(p.1, builder)], builder)
}

fn path_form(p: &Path, builder: &mut SyntaxBuilder) -> SyntaxId {
    vec(p.iter().map(|x| str(x, builder)).collect(), builder)
}

pub fn to_syntax(t: &T) -> Syntax {
    let mut builder = SyntaxBuilder::new();
    let mut nodes: PathMap<Vec<(SyntaxId, SyntaxId)>> = PathMap::new();
    for (p, v) in &t.at {
        nodes.entry(p.clone()).or_default().push((kw("at", &mut builder), pair(*v, &mut builder)));
    }
    for (p, v) in &t.level {
        nodes.entry(p.clone()).or_default().push((kw("level", &mut builder), str(projection::level_name(*v), &mut builder)));
    }
    for (p, v) in &t.pinned {
        nodes.entry(p.clone()).or_default().push((kw("pinned", &mut builder), bool_(*v, &mut builder)));
    }
    for (p, v) in &t.collapsed {
        nodes.entry(p.clone()).or_default().push((kw("collapsed", &mut builder), bool_(*v, &mut builder)));
    }
    for (p, rows) in &t.rows {
        let mut items = Vec::new();
        for (k, v) in rows {
            items.push(kw(k, &mut builder));
            items.push(bool_(*v, &mut builder));
        }
        nodes.entry(p.clone()).or_default().push((kw("rows", &mut builder), mk(Draft::Map(items), &mut builder)));
    }
    let node_forms = nodes.iter().map(|(p, fields)| {
        let mut items = vec![sym("node", &mut builder), path_form(p, &mut builder)];
        for (k, v) in fields { items.extend([*k, *v]); }
        mk(Draft::List(items), &mut builder)
    }).collect::<Vec<_>>();
    let mut frames = Vec::new();
    for (p, fs) in &t.frames {
        for f in fs {
            frames.push(mk(Draft::List(vec![
                sym("frame", &mut builder), path_form(p, &mut builder), str(&f.title, &mut builder),
                kw("at", &mut builder), pair(f.at, &mut builder), kw("size", &mut builder), pair(f.size, &mut builder),
            ]), &mut builder));
        }
    }
    let editor = t.editor.iter().map(|name| mk(Draft::List(vec![sym("editor", &mut builder), str(name, &mut builder)]), &mut builder)).collect::<Vec<_>>();
    let panel_forms = t.panels.iter().map(|(path, state)| {
        let mut items = vec![sym("panel", &mut builder), path_form(path, &mut builder), kw("collapsed", &mut builder), bool_(state.collapsed, &mut builder)];
        if let Some((x, y, w, h)) = state.window {
            items.push(kw("window", &mut builder));
            items.push(vec([x, y, w, h].iter().map(|n| number(*n as f64, &mut builder)).collect(), &mut builder));
        }
        mk(Draft::List(items), &mut builder)
    }).collect::<Vec<_>>();
    let mut items = vec![sym("layout", &mut builder)];
    items.extend(editor);
    items.extend(panel_forms);
    items.extend(node_forms);
    items.extend(frames);
    let root = mk(Draft::List(items), &mut builder);
    builder.freeze().handle(root)
}

/// Append a parseable `(layout ...)` form. No SyntaxBuilder; grows `out` only when capacity is short.
pub fn write_into(t: &T, out: &mut String) {
    use std::fmt::Write as _;
    fn quote(out: &mut String, text: &str) {
        lisp::quote_string_into(text.as_bytes(), out);
    }
    fn num(out: &mut String, x: f64) {
        if x.fract() == 0. && x.is_finite() && x.abs() < 1e15 {
            let _ = write!(out, "{}", x as i64);
        } else {
            let _ = write!(out, "{x}");
        }
    }
    fn path(out: &mut String, p: &Path) {
        out.push('[');
        for (i, s) in p.iter().enumerate() {
            if i > 0 {
                out.push(' ');
            }
            quote(out, s.as_str());
        }
        out.push(']');
    }
    fn pair(out: &mut String, p: (f64, f64)) {
        out.push('[');
        num(out, p.0);
        out.push(' ');
        num(out, p.1);
        out.push(']');
    }
    out.push_str("(layout");
    if let Some(name) = &t.editor {
        out.push_str(" (editor ");
        quote(out, name);
        out.push(')');
    }
    for (p, state) in &t.panels {
        out.push_str(" (panel ");
        path(out, p);
        out.push_str(" :collapsed ");
        out.push_str(if state.collapsed { "true" } else { "false" });
        if let Some((x, y, w, h)) = state.window {
            out.push_str(" :window [");
            let _ = write!(out, "{x} {y} {w} {h}");
            out.push(']');
        }
        out.push(')');
    }
    // Separate node forms per field map: `of_syntax` merges by path (avoids a PathMap alloc).
    for (p, v) in &t.at {
        out.push_str(" (node ");
        path(out, p);
        out.push_str(" :at ");
        pair(out, *v);
        out.push(')');
    }
    for (p, v) in &t.level {
        out.push_str(" (node ");
        path(out, p);
        out.push_str(" :level ");
        quote(out, projection::level_name(*v));
        out.push(')');
    }
    for (p, v) in &t.pinned {
        out.push_str(" (node ");
        path(out, p);
        out.push_str(" :pinned ");
        out.push_str(if *v { "true" } else { "false" });
        out.push(')');
    }
    for (p, v) in &t.collapsed {
        out.push_str(" (node ");
        path(out, p);
        out.push_str(" :collapsed ");
        out.push_str(if *v { "true" } else { "false" });
        out.push(')');
    }
    for (p, rows) in &t.rows {
        out.push_str(" (node ");
        path(out, p);
        out.push_str(" :rows {");
        for (i, (k, v)) in rows.iter().enumerate() {
            if i > 0 {
                out.push(' ');
            }
            out.push(':');
            out.push_str(k);
            out.push(' ');
            out.push_str(if *v { "true" } else { "false" });
        }
        out.push_str("})");
    }
    for (p, fs) in &t.frames {
        for f in fs {
            out.push_str(" (frame ");
            path(out, p);
            out.push(' ');
            quote(out, &f.title);
            out.push_str(" :at ");
            pair(out, f.at);
            out.push_str(" :size ");
            pair(out, f.size);
            out.push(')');
        }
    }
    out.push(')');
}

fn fail<V>(message: String) -> Result<V, String> {
    Err(format!("layout: {message}"))
}

fn read_number(f: &Syntax) -> Result<f64, String> {
    match &f.node {
        Node::Num(text) => match (f.name(*text)).parse::<f64>().ok() {
            Some(x) if x.is_finite() => Ok(x),
            _ => fail(format!("bad number {}", f.name(*text))),
        },
        _ => fail("expected a number".to_owned()),
    }
}

fn read_pair(f: &Syntax) -> Result<(f64, f64), String> {
    match &f.node {
        Node::Vec(range) if f.range(*range).len() == 2 => {
            let items = f.range(*range);
            let x = read_number(&f.child(items[0]))?;
            let y = read_number(&f.child(items[1]))?;
            Ok((x, y))
        }
        _ => fail("expected [x y]".to_owned()),
    }
}

fn read_bool(f: &Syntax) -> Result<bool, String> {
    match &f.node {
        Node::Sym(t) if f.name(*t) == "true" => Ok(true),
        Node::Sym(t) if f.name(*t) == "false" => Ok(false),
        _ => fail("expected true or false".to_owned()),
    }
}

fn read_str(f: &Syntax) -> Result<String, String> {
    match &f.node {
        Node::Str(text) => Ok(String::from_utf8_lossy(f.bytes(*text)).into_owned()),
        _ => fail("expected text".to_owned()),
    }
}

fn read_path(f: &Syntax) -> Result<Path, String> {
    match &f.node {
        Node::Vec(range) => f.range(*range).iter().map(|id| read_str(&f.child(*id)).map(Into::into)).collect(),
        _ => fail("expected a path vector".to_owned()),
    }
}

pub fn of_syntax(form: &Syntax) -> Result<T, String> {
    let Node::List(range) = form.node else { return fail("expected (layout ...)".to_owned()) };
    match form.range(range).split_first() {
        Some((head, rest)) if matches!(form.arena().row(*head).node, Node::Sym(h) if form.name(h) == "layout") => {
            let mut t = empty();
            for id in rest {
                t = read_item(t, &form.child(*id))?;
            }
            Ok(t)
        }
        _ => fail("expected (layout ...)".to_owned()),
    }
}

fn read_item(mut t: T, item: &Syntax) -> Result<T, String> {
    let Node::List(range) = item.node else { return unknown(item) };
    let parts = item.range(range);
    let head = match parts.first().map(|id| item.arena().row(*id).node) {
        Some(Node::Sym(h)) => item.name(h),
        _ => return unknown(item),
    };
    let keyword = |id: SyntaxId, name: &str| matches!(item.arena().row(id).node, Node::Kw(key) if item.name(key) == name);
    match (head, &parts[1..]) {
        ("panel", [p, fields @ ..]) => {
            let p = read_path(&item.child(*p))?;
            let mut state = panels::default_state();
            let mut rest = fields;
            loop {
                match rest {
                    [] => {
                        t.panels.insert(p, state);
                        return Ok(t);
                    }
                    [k, v, tail @ ..] if keyword(*k, "collapsed") => {
                        state.collapsed = read_bool(&item.child(*v))?;
                        rest = tail;
                    }
                    [k, v, tail @ ..] if keyword(*k, "window") && matches!(item.arena().row(*v).node, Node::Vec(_)) => {
                        let ids = item.arena().children(*v);
                        let mut values = [0.0; 4];
                        for (i, id) in ids.iter().enumerate() {
                            let number = read_number(&item.child(*id))?;
                            if i < values.len() { values[i] = number; }
                        }
                        let [x, y, w, h] = values;
                        if ids.len() == 4 && w >= 120. && h >= 80. && values.iter().all(|n| n.abs() <= 1e6) {
                            state.window = Some((x as i64, y as i64, w as i64, h as i64));
                            rest = tail;
                        } else {
                            return fail("window needs [x y width height], width >= 120 and height >= 80".to_owned());
                        }
                    }
                    _ => return fail("bad panel entry".to_owned()),
                }
            }
        }
        ("editor", [name]) => {
            let name = read_str(&item.child(*name))?;
            if t.editor.is_some() {
                fail("duplicate editor entry".to_owned())
            } else {
                t.editor = Some(name);
                Ok(t)
            }
        }
        ("node", [p, fields @ ..]) => {
            let p = read_path(&item.child(*p))?;
            let mut rest = fields;
            loop {
                match rest {
                    [] => return Ok(t),
                    [k, v, tail @ ..] if keyword(*k, "at") => {
                        t.at.insert(p.clone(), read_pair(&item.child(*v))?);
                        rest = tail;
                    }
                    [k, v, tail @ ..] if keyword(*k, "level") => {
                        let name = read_str(&item.child(*v))?;
                        match projection::level_of_name(&name) {
                            Some(l) => {
                                t.level.insert(p.clone(), l);
                            }
                            None => return fail(format!("unknown level {name}")),
                        }
                        rest = tail;
                    }
                    [k, v, tail @ ..] if keyword(*k, "pinned") => {
                        t.pinned.insert(p.clone(), read_bool(&item.child(*v))?);
                        rest = tail;
                    }
                    [k, v, tail @ ..] if keyword(*k, "collapsed") => {
                        t.collapsed.insert(p.clone(), read_bool(&item.child(*v))?);
                        rest = tail;
                    }
                    [k, v, tail @ ..] if keyword(*k, "rows") && matches!(item.arena().row(*v).node, Node::Map(_)) => {
                        let mut m = StringMap::new();
                        let mut r = item.arena().children(*v);
                        loop {
                            match r {
                                [] => break,
                                [key, value, tail @ ..] if matches!(item.arena().row(*key).node, Node::Kw(_)) => {
                                    let Node::Kw(name) = item.arena().row(*key).node else { unreachable!() };
                                    m.insert(item.name(name).to_owned(), read_bool(&item.child(*value))?);
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
        ("frame", [p, title, a1, at, a2, size]) if keyword(*a1, "at") && keyword(*a2, "size") => {
            let p = read_path(&item.child(*p))?;
            let title = read_str(&item.child(*title))?;
            let at = read_pair(&item.child(*at))?;
            let size = read_pair(&item.child(*size))?;
            t.frames.entry(p).or_default().push(Frame { title, at, size });
            Ok(t)
        }
        _ => unknown(item),
    }
}

fn unknown(item: &Syntax) -> Result<T, String> {
    fail(format!("unknown entry {}", lisp::flat(item)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn streamed_layout_round_trips() {
        let path = vec!["network".into()];
        let mut layout = T { editor: Some("editor".to_owned()), ..T::default() };
        layout.panels.insert(path.clone(), State { collapsed: true, window: Some((5, 10, 180, 120)) });
        layout.at.insert(path.clone(), (1.25, -2.5));
        layout.pinned.insert(path.clone(), true);
        layout.level.insert(path.clone(), Level::Point);
        layout.collapsed.insert(path.clone(), false);
        layout.rows.insert(path.clone(), BTreeMap::from([("a".to_owned(), true)]));
        layout.frames.insert(path, vec![Frame {
            title: "frame \"☃\"\n\u{1}".to_owned(),
            at: (2., 3.),
            size: (200., 150.),
        }]);
        let mut text = String::new();
        write_into(&layout, &mut text);
        let forms = flow_syntax::arena_parse::parse(&text).unwrap();
        assert_eq!(of_syntax(&forms[0]).unwrap(), layout);
    }
}
