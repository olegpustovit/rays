//! Port of lib/rays_editor/text_pane.ml(i): the workspace text pane (Selection, Graph and Document
//! tabs over the Lisp text). Pure state and text plus `view`, which draws and returns intents for
//! `Editor_frame` to apply after the frame.
//!
//! Sharing: OCaml shares the source, the printed text and the span maps; here `Shown` holds them in
//! `Arc`s and `Printed` is the one lazily printed document behind `applied` / `applied_spans`.
use editor_document::literal_edit::Change;
use editor_document::settings;
use editor_document::workspace_doc::{self as wd, T as WorkspaceDoc};
use flow_graph::flow_edit::{self as fe, ArgKey, Op};
use flow_syntax::lisp::{self, Spans};
use flow_syntax::syntax::{self as s, Node, Syntax};
use rays_diagnostic::{self as diagnostic, Diagnostic, Severity, Span};
use crate::lisp_text::{self, Names, Vocab};
use pxui::theme::{self, Theme};
use pxui::ui::*;
use pxui_shell::kit::{self, ButtonOpts};
use pxui_shell::layout;
use rays_math::color;
use std::cell::{Cell, RefCell};
use std::rc::Rc;
use std::sync::{Arc, OnceLock};

type S = Syntax;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Tab {
    Selection,
    Graph,
    Document,
}
pub type Path = Vec<String>;

/// A document printed once, on demand: the text and its span map (`applied` and `applied_spans`
/// of the OCaml `shown` are two views of one of these).
#[derive(Clone)]
pub struct Printed {
    cell: Arc<PrintCell>,
}

struct PrintCell {
    value: OnceLock<(String, Spans)>,
    source: PrintSource,
}

enum PrintSource {
    Forms(Arc<Vec<S>>),
    Doc(Box<WorkspaceDoc>),
    Ready,
}

impl std::fmt::Debug for Printed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Printed")
    }
}

impl Printed {
    pub fn of_forms(source: Arc<Vec<S>>) -> Printed {
        Printed { cell: Arc::new(PrintCell { value: OnceLock::new(), source: PrintSource::Forms(source) }) }
    }
    pub fn of_workspace(workspace: &WorkspaceDoc) -> Printed {
        Printed { cell: Arc::new(PrintCell { value: OnceLock::new(), source: PrintSource::Doc(Box::new(workspace.clone())) }) }
    }
    /// Already printed (`lazy value`).
    pub fn ready(text: String, spans: Spans) -> Printed {
        let value = OnceLock::new();
        let _ = value.set((text, spans));
        Printed { cell: Arc::new(PrintCell { value, source: PrintSource::Ready }) }
    }
    fn force(&self) -> &(String, Spans) {
        self.cell.value.get_or_init(|| match &self.cell.source {
            PrintSource::Forms(forms) => lisp::print(forms),
            PrintSource::Doc(doc) => wd::print(doc),
            PrintSource::Ready => (String::new(), Vec::new()),
        })
    }
    /// `Lazy.force shown.applied`
    pub fn text(&self) -> &str {
        &self.force().0
    }
    /// `Lazy.force shown.applied_spans`
    pub fn spans(&self) -> &Spans {
        &self.force().1
    }
}

#[derive(Clone, Debug)]
pub struct Shown {
    /// the graph the Selection and Graph tabs read
    pub graph: String,
    pub text: String,
    /// byte span of the selected binding in `text`
    pub mark: Option<(i64, i64)>,
    /// what the Selection tab shows and its apply writes: the selected binding's path, else `[graph]`
    pub key: Path,
    /// the whole document's text (the draft's dirty mark)
    pub applied: Printed,
    /// the Graph tab's graph body and the spans of `text`: what the caret is looked up in
    pub body: Option<(Arc<S>, Arc<Spans>)>,
    pub source: Arc<Vec<S>>,
    pub spans: Arc<Spans>,
    pub applied_spans: Printed,
}

/// The find bar: what to find and its replacement, whether case counts, whether the replace row is
/// open, the byte the current match is the first at or after, and whether the query field takes the
/// keyboard this frame.
#[derive(Clone, Debug, PartialEq)]
pub struct Find {
    pub query: String,
    pub replace: String,
    pub case: bool,
    pub replacing: bool,
    pub at: i64,
    pub grab: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Command {
    Find,
    FindReplace,
    FindSelection,
    FindNext,
    FindPrevious,
    Definition,
    References,
    Select(i64, i64),
}

/// What the text area is asked once: replace the ranges (one undo step), then select.
#[derive(Clone, Debug, PartialEq)]
pub struct Request {
    pub select: (i64, i64),
    pub splice: Vec<(i64, i64, String)>,
}

/// The diagnostics found in the text `at`.
#[derive(Clone, Debug, PartialEq)]
pub struct Checked {
    pub at: String,
    pub found: Vec<Diagnostic>,
}

/// One graph's Graph tab draft, the document it began from and the errors of its last apply.
#[derive(Clone)]
pub struct Slot {
    pub gtext: String,
    pub gbase: Option<WorkspaceDoc>,
    pub gerrors: Checked,
}

/// The parse of a draft: `found` is `None` until `seen` has rested for the debounce.
#[derive(Clone, Debug, PartialEq)]
pub struct Live {
    pub seen: String,
    pub since: f64,
    pub found: Option<Vec<Diagnostic>>,
}

/// The cache key of `State::cache`.
#[derive(Clone)]
pub struct CacheKey {
    pub source: Arc<Vec<S>>,
    pub workspace: Option<Box<WorkspaceDoc>>,
    pub graph: String,
    pub selected: Option<Path>,
    pub tab: Tab,
}

#[derive(Clone)]
pub struct State {
    pub tab: Tab,
    /// the Document tab's unapplied text
    pub draft: Option<String>,
    /// the Selection tab's unapplied text, keyed by `shown.key`
    pub binding_draft: Option<(Path, String)>,
    /// the Graph tab's unapplied texts: one slot per graph
    pub graphs: Vec<(String, Slot)>,
    /// base document of the draft
    pub doc_base: Option<WorkspaceDoc>,
    pub binding_base: Option<WorkspaceDoc>,
    /// of the last refused apply
    pub doc_errors: Checked,
    pub binding_errors: Checked,
    pub live: Option<Live>,
    /// long lines continue on the next row
    pub wrap: bool,
    /// the closing brackets follow indentation
    pub parinfer: bool,
    /// the right-click menu while it is open
    pub menu: Option<(f64, f64)>,
    /// the colour literal being edited: byte range, edited yet
    pub picker: Option<(i64, i64, bool)>,
    /// a successfully applied token scrub; retain printed line breaks until release
    pub scrubbing: bool,
    /// a definition revealed in the Document tab
    pub target: Option<Path>,
    pub find: Option<Find>,
    /// commands `view` resolves this frame
    pub pending: Vec<Command>,
    pub request: Option<Request>,
    /// raised to scroll the same offset into view again
    pub nonce: i64,
    pub cache: Option<Box<(CacheKey, Shown)>>,
}

pub fn no_errors() -> Checked {
    Checked { at: String::new(), found: Vec::new() }
}

pub fn initial() -> State {
    State {
        tab: Tab::Selection,
        draft: None,
        binding_draft: None,
        graphs: Vec::new(),
        doc_base: None,
        binding_base: None,
        doc_errors: no_errors(),
        binding_errors: no_errors(),
        live: None,
        wrap: false,
        parinfer: true,
        menu: None,
        picker: None,
        scrubbing: false,
        target: None,
        find: None,
        pending: Vec::new(),
        request: None,
        nonce: 0,
        cache: None,
    }
}

// ---- byte helpers (OCaml strings are bytes) ----

/// `String.sub text a len`, lossily decoded where the cut falls inside a character.
pub fn sub(text: &str, a: i64, len: i64) -> String {
    let (a, len) = (a.max(0) as usize, len.max(0) as usize);
    let bytes = text.as_bytes();
    let end = (a + len).min(bytes.len());
    String::from_utf8_lossy(&bytes[a.min(end)..end]).into_owned()
}

fn index_from(text: &str, from: i64, c: u8) -> Option<i64> {
    let bytes = text.as_bytes();
    let from = from.max(0) as usize;
    if from > bytes.len() {
        return None;
    }
    bytes[from..].iter().position(|b| *b == c).map(|i| (i + from) as i64)
}

// ---- reading the source ----

fn root_forms(source: &[S]) -> Option<&[S]> {
    match source.first().map(|f| &f.node) {
        Some(Node::List(items)) if items.len() >= 2 => Some(&items[2..]),
        _ => None,
    }
}

fn root_name(item: &S) -> Option<String> {
    if let Node::List(l) = &item.node
        && l.len() >= 2
        && let (Node::Sym(h), Node::Sym(n)) = (&l[0].node, &l[1].node)
        && matches!(h.as_str(), "graph" | "defn" | "defmacro")
    {
        return Some(if h == "defn" {
            format!("def:{}", n)
        } else if h == "defmacro" {
            format!("macro:{}", n)
        } else {
            n.clone()
        });
    }
    None
}

pub fn root_form<'a>(source: &'a [S], name: &str) -> Option<&'a S> {
    root_forms(source)?.iter().find(|item| root_name(item).as_deref() == Some(name))
}

fn last(f: &S) -> Option<&S> {
    s::children(f).last().copied()
}

/// `let* [bindings] result` of an even binding vector.
fn scope(e: &S) -> Option<(Vec<(&S, &S)>, &S)> {
    if let Node::List(l) = &e.node
        && l.len() == 3
        && matches!(&l[0].node, Node::Sym(h) if h == "let*")
        && let Node::Vec(bs) = &l[1].node
        && bs.len() % 2 == 0
    {
        return Some((s::pairs(bs), &l[2]));
    }
    None
}

/// the scope inside a binding's value: itself, or a loop's body (the flag: it stepped inside)
fn enter(v: &S) -> (&S, bool) {
    if scope(v).is_some() {
        return (v, false);
    }
    if matches!(s::head(v), Some("for" | "fold" | "scan" | "sum" | "state" | "fn"))
        && let Some(l) = last(v)
    {
        return (l, true);
    }
    (v, false)
}

fn append(path: &[String], leaf: &str) -> Path {
    let mut p = path.to_vec();
    p.push(leaf.to_owned());
    p
}

type Found = Option<(Option<S>, S)>;

/// the binding a path names (pattern and expression) below a graph's body
fn find_binding(cur: &S, names: &[String]) -> Found {
    let (name, rest) = names.split_first()?;
    let (name, keys) = fe::leaf_keys(name).unwrap_or((name.clone(), Vec::new()));
    let found: Found = match scope(cur) {
        Some((_, res)) if name == "@result" => Some((None, res.clone())),
        Some((ps, _)) => ps.iter().find(|(p, _)| fe::pat_key(p) == name).map(|(p, v)| (Some((*p).clone()), (*v).clone())),
        None if name == "@result" => Some((None, cur.clone())),
        None => None,
    };
    let found = if keys.is_empty() {
        found
    } else {
        found.and_then(|(_, v)| {
            let mut e = Some(v);
            for key in &keys {
                e = e.and_then(|e| fe::arg_of(&e, key));
            }
            e.map(|e| (None, e))
        })
    };
    match found {
        Some((p, v)) if rest.is_empty() => Some((p, v)),
        Some((_, v)) => find_binding(enter(&v).0, rest),
        None => None,
    }
}

/// The binding a path names in the source: its pattern (none for a `@result`) and expression.
pub fn binding(source: &[S], path: &[String]) -> Found {
    let (graph, names) = path.split_first()?;
    let root = root_form(source, graph)?;
    let body = last(root)?;
    find_binding(body, names)
}

fn named(forms: &[S]) -> Option<Vec<(String, S)>> {
    let mut out = Vec::new();
    let mut rest = forms;
    loop {
        match rest {
            [] => return Some(out),
            [a, v, tail @ ..] if matches!(a.node, Node::Sym(_)) => {
                if let Node::Sym(n) = &a.node {
                    out.push((n.clone(), v.clone()));
                }
                rest = tail;
            }
            _ => return None,
        }
    }
}

/// Pasted text as `Add_node`s of `scope`: `name expr` pairs, or bare expressions named by their
/// head. Names pair by position and are made free of the graph's and of each other; a pasted
/// binding that reads an earlier pasted one reads its new name (all renames at once).
pub fn paste_ops(source: &[S], graph: &str, scope: &[String], text: &str) -> Result<Vec<Op>, String> {
    let head = |f: &S| -> String {
        match s::head(f) {
            Some(h) => match h.rfind('/') {
                Some(i) => h[i + 1..].to_owned(),
                None => h.to_owned(),
            },
            None => "value".to_owned(),
        }
    };
    let forms = match s::parse(text) {
        Err(_) => return Err("The clipboard holds no Lisp bindings".to_owned()),
        Ok(f) if f.is_empty() => return Err("The clipboard holds no Lisp bindings".to_owned()),
        Ok(f) => f,
    };
    let (bindings, reading) = match named(&forms) {
        Some(ps) => (ps, true),
        None => (forms.iter().map(|f| (head(f), f.clone())).collect::<Vec<_>>(), false),
    };
    let mut chosen: Vec<String> = Vec::new();
    let mut fresh = |n: &str| -> String {
        let mut k = 0;
        loop {
            let base = if k == 0 { n.to_owned() } else { format!("{}_{}", n, k) };
            let name = fe::fresh_name(source, graph, &base);
            if chosen.contains(&name) {
                k += 1;
            } else {
                chosen.push(name.clone());
                return name;
            }
        }
    };
    let mut earlier: Vec<(String, String)> = Vec::new();
    let mut ops: Vec<Op> = Vec::new();
    for (old, expr) in bindings {
        let name = fresh(&old);
        // what the earlier pasted bindings are called now, the latest of a name first
        let mut seen: Vec<String> = Vec::new();
        let mut renames: Vec<(String, String)> = Vec::new();
        for (o, n) in &earlier {
            if reading && o != n && !seen.contains(o) {
                seen.push(o.clone());
                renames.push((o.clone(), n.clone()));
            }
        }
        let held: Vec<(String, String, String)> =
            renames.iter().enumerate().map(|(i, (o, n))| (o.clone(), format!("@paste{}", i), n.clone())).collect();
        let mut expr = expr;
        for (o, hold, _) in &held {
            expr = fe::rename_ref(o, hold, &expr);
        }
        for (_, hold, n) in &held {
            expr = fe::rename_ref(hold, n, &expr);
        }
        earlier.insert(0, (old, name.clone()));
        ops.push(Op::AddNode { scope: scope.to_vec(), name, expr });
    }
    Ok(ops)
}

fn max_id(f: &S) -> i64 {
    s::children(f).iter().fold(f.id, |m, c| m.max(max_id(c)))
}

fn span_of<'a>(spans: &'a Spans, f: &S) -> Option<&'a Span> {
    spans.iter().find(|(id, _)| *id == f.id).map(|(_, sp)| sp)
}

/// the marked byte range of a binding: its name through its expression
fn mark(spans: &Spans, found: &Found) -> Option<(i64, i64)> {
    let (p, v) = found.as_ref()?;
    let b = span_of(spans, v)?;
    match p.as_ref().and_then(|p| span_of(spans, p)) {
        Some(a) => Some((a.start, b.finish)),
        None => Some((b.start, b.finish)),
    }
}

fn graph_at(shown: &Shown, byte: i64) -> Option<(String, &S)> {
    for root in root_forms(&shown.source)? {
        if let Node::List(l) = &root.node
            && l.len() >= 2
            && let (Node::Sym(h), Node::Sym(name)) = (&l[0].node, &l[1].node)
            && matches!(h.as_str(), "graph" | "defn" | "defmacro")
            && let Some(span) = span_of(&shown.spans, root)
            && span.start <= byte
            && byte < span.finish
        {
            let prefix = if h == "defn" { "def:" } else if h == "defmacro" { "macro:" } else { "" };
            return Some((format!("{}{}", prefix, name), root));
        }
    }
    None
}

fn is_list_or_map(f: &S) -> bool {
    matches!(f.node, Node::List(_) | Node::Map(_))
}

fn holds(spans: &Spans, byte: i64, f: &S) -> bool {
    match span_of(spans, f) {
        Some(sp) => sp.start <= byte && byte <= sp.finish,
        None => false,
    }
}

fn nested_at(spans: &Spans, byte: i64, prefix: &[String], leaf: &str, v: &S) -> Path {
    let (body, entered) = enter(v);
    let inner = if entered || scope(v).is_some() { within_at(spans, byte, &append(prefix, leaf), body) } else { None };
    if let Some(path) = inner {
        return path;
    }
    match fe::nested_nodes(v).into_iter().find(|(_, a)| holds(spans, byte, a)) {
        Some((key, a)) => match fe::nested_leaf(leaf, &key) {
            Ok(l) => nested_at(spans, byte, prefix, &l, &a),
            Err(_) => append(prefix, leaf),
        },
        None => append(prefix, leaf),
    }
}

fn within_at(spans: &Spans, byte: i64, prefix: &[String], cur: &S) -> Option<Path> {
    match scope(cur) {
        None => {
            if is_list_or_map(cur) && holds(spans, byte, cur) { Some(nested_at(spans, byte, prefix, "@result", cur)) } else { None }
        }
        Some((bindings, res)) => {
            for (p, v) in &bindings {
                if let (Some(a), Some(b)) = (span_of(spans, p), span_of(spans, v))
                    && a.start <= byte
                    && byte <= b.finish
                {
                    return Some(nested_at(spans, byte, prefix, &fe::pat_key(p), v));
                }
            }
            if holds(spans, byte, res) && is_list_or_map(res) { Some(nested_at(spans, byte, prefix, "@result", res)) } else { None }
        }
    }
}

/// The innermost binding or inline node at a byte in any tab.
pub fn binding_at(shown: &Shown, byte: i64) -> Option<Path> {
    let (graph, body, spans): (String, S, Arc<Spans>) = match &shown.body {
        Some((body, spans)) => (shown.graph.clone(), (**body).clone(), spans.clone()),
        None => match graph_at(shown, byte) {
            Some((graph, root)) => match last(root) {
                Some(body) => (graph, body.clone(), shown.spans.clone()),
                None => return None,
            },
            None => return None,
        },
    };
    let found = match within_at(&spans, byte, &[], &body) {
        Some(p) => Some(p),
        None => {
            if scope(&body).is_none() && holds(&spans, byte, &body) && fe::node_call(&body) {
                Some(nested_at(&spans, byte, &[], "@result", &body))
            } else {
                None
            }
        }
    };
    found.map(|names| {
        let mut p = vec![graph];
        p.extend(names);
        p
    })
}

fn parameter(p: &S) -> Vec<String> {
    match &p.node {
        Node::List(l) if !l.is_empty() => fe::pat_names(&l[0]),
        _ => fe::pat_names(p),
    }
}

fn rail_names(expr: &S) -> Vec<String> {
    let children = s::children(expr);
    match s::head(expr) {
        Some("fn") => match children.get(1).map(|c| &c.node) {
            Some(Node::Vec(ps)) => ps.iter().flat_map(parameter).collect(),
            _ => Vec::new(),
        },
        Some("for" | "sum" | "fold" | "scan" | "state") => {
            let args = &children[1.min(children.len())..];
            let take = args.len().saturating_sub(1);
            args[..take]
                .iter()
                .flat_map(|arg| match &arg.node {
                    Node::Vec(ps) => s::pairs(ps).into_iter().flat_map(|(p, _)| fe::pat_names(p)).collect::<Vec<_>>(),
                    _ => Vec::new(),
                })
                .collect()
        }
        _ => Vec::new(),
    }
}

fn input_of(source: &[S], name: &str, prefix: &[String]) -> Option<Path> {
    if let [graph] = prefix {
        let root = root_form(source, graph)?;
        let children = s::children(root);
        let params = children.iter().skip(2).find_map(|arg| match &arg.node {
            Node::Vec(ps) => Some(ps),
            _ => None,
        })?;
        if params.iter().any(|p| parameter(p).iter().any(|n| n == name)) {
            return Some(vec![graph.clone(), format!(":{}", name)]);
        }
    }
    None
}

fn local_ref(source: &[S], name: &str, prefix: &[String], leaf: &str) -> Option<Path> {
    let expr: Option<S> = match prefix {
        [graph] => root_form(source, graph).and_then(last).cloned(),
        _ => binding(source, prefix).map(|(_, v)| v),
    };
    let found = expr.and_then(|expr| {
        let body = enter(&expr).0;
        let bindings: Vec<(&S, &S)> = scope(body).map(|(ps, _)| ps).unwrap_or_default();
        let holder = fe::leaf_keys(leaf).map(|(h, _)| h).unwrap_or(leaf.to_owned());
        let bindings: Vec<(&S, &S)> = if bindings.iter().any(|(p, _)| fe::pat_key(p) == holder) {
            let mut before = Vec::new();
            for (p, v) in &bindings {
                if fe::pat_key(p) == holder {
                    if s::head(v) == Some("fn") {
                        before.push((*p, *v));
                    }
                    break;
                }
                before.push((*p, *v));
            }
            before
        } else {
            bindings
        };
        if let Some((p, _)) = bindings.iter().rev().find(|(p, _)| fe::pat_names(p).iter().any(|n| n == name)) {
            return Some(append(prefix, &fe::pat_key(p)));
        }
        if fe::nested(name) && binding(source, &append(prefix, name)).is_some() {
            return Some(append(prefix, name));
        }
        if rail_names(&expr).iter().any(|n| n == name) {
            return Some(prefix.to_vec());
        }
        input_of(source, name, prefix)
    });
    if found.is_some() {
        return found;
    }
    match prefix.split_last() {
        Some((child, parent)) if !parent.is_empty() => local_ref(source, name, parent, child),
        _ => None,
    }
}

/// Resolve a read in its lexical scope, including dotted record outputs.
pub fn reference_target(source: &[S], from: &[String], name: &str) -> Option<Path> {
    let name = match name.find('.') {
        Some(i) => &name[..i],
        None => name,
    };
    let found = match from.split_last() {
        Some((leaf, prefix)) if !prefix.is_empty() => local_ref(source, name, prefix, leaf),
        _ => None,
    };
    if found.is_some() {
        return found;
    }
    [format!("def:{}", name), format!("macro:{}", name), name.to_owned()]
        .into_iter()
        .find(|key| root_form(source, key).is_some())
        .map(|key| vec![key])
}

/// The definition of the symbol at a printed byte, including call heads.
pub fn definition_at(shown: &Shown, byte: i64) -> Option<Path> {
    fn symbol(shown: &Shown, byte: i64, form: &S) -> Option<String> {
        if let (Node::Sym(name), Some(span)) = (&form.node, span_of(&shown.spans, form))
            && span.start <= byte
            && byte < span.finish
        {
            return Some(name.clone());
        }
        s::children(form).into_iter().find_map(|c| symbol(shown, byte, c))
    }
    let name = shown.source.iter().find_map(|f| symbol(shown, byte, f))?;
    let from = binding_at(shown, byte).unwrap_or_else(|| vec![shown.graph.clone()]);
    if let Some((Some(p), _)) = binding(&shown.source, &from)
        && let Some(span) = span_of(&shown.spans, &p)
        && span.start <= byte
        && byte < span.finish
    {
        return Some(from);
    }
    reference_target(&shown.source, &from, &name)
}

fn span_range(span: &Span) -> (i64, i64) {
    (span.start, span.finish)
}

pub fn path_mark(source: &[S], spans: &Spans, path: &[String]) -> Option<(i64, i64)> {
    match path {
        [graph] => root_form(source, graph).and_then(|root| span_of(spans, root)).map(span_range),
        [graph, input] if input.starts_with(':') => {
            let name = &input[1..];
            let root = root_form(source, graph)?;
            for arg in s::children(root).into_iter().skip(2) {
                if let Node::Vec(ps) = &arg.node {
                    for p in ps {
                        let pattern = match &p.node {
                            Node::List(l) if !l.is_empty() => &l[0],
                            _ => p,
                        };
                        if fe::pat_names(pattern).iter().any(|n| n == name) {
                            return span_of(spans, p).map(span_range);
                        }
                    }
                }
            }
            None
        }
        _ => mark(spans, &binding(source, path)),
    }
}

/// The bindings of a scene graph that are `scene/camera` calls.
pub fn cameras(source: &[S], graph: &str) -> Vec<String> {
    let Some(root) = root_form(source, graph) else { return Vec::new() };
    let Some((bindings, _)) = last(root).and_then(scope) else { return Vec::new() };
    bindings
        .iter()
        .filter_map(|(p, v)| match (s::head(v), &p.node) {
            (Some("scene/camera"), Node::Sym(n)) => Some(n.clone()),
            _ => None,
        })
        .collect()
}

fn plural(n: usize, what: &str) -> String {
    format!("{} {}{}", n, what, if n == 1 { "" } else { "s" })
}

/// the top-level ancestor with what it reads, as a `let*` over the root bindings it needs; None
/// when the graph body is not a scope
fn closure(root: &S, top: &str) -> Option<S> {
    let (bindings, _) = last(root).and_then(scope)?;
    let mut declares: Vec<(String, usize)> = Vec::new();
    for (i, (p, _)) in bindings.iter().enumerate() {
        for n in fe::pat_names(p) {
            declares.push((n, i));
        }
    }
    fn need(bindings: &[(&S, &S)], declares: &[(String, usize)], acc: &mut Vec<usize>, name: &str) {
        if let Some((_, i)) = declares.iter().find(|(n, _)| n == name)
            && !acc.contains(i)
        {
            acc.push(*i);
            for n in fe::free_names(bindings[*i].1) {
                need(bindings, declares, acc, &n);
            }
        }
    }
    let mut needed = Vec::new();
    need(&bindings, &declares, &mut needed, top);
    let sub: Vec<(&S, &S)> = bindings.iter().enumerate().filter(|(i, _)| needed.contains(i)).map(|(_, b)| *b).collect();
    if sub.is_empty() {
        return None;
    }
    let inputs: Vec<String> = match s::children(root).into_iter().find_map(|c| match &c.node {
        Node::Vec(inputs) => Some(inputs),
        _ => None,
    }) {
        None => Vec::new(),
        Some(inputs) => inputs
            .iter()
            .filter_map(|i| match &i.node {
                Node::List(l) if !l.is_empty() => match &l[0].node {
                    Node::Sym(n) if sub.iter().any(|(_, v)| fe::free_names(v).contains(n)) => Some(n.clone()),
                    _ => None,
                },
                _ => None,
            })
            .collect(),
    };
    let base = max_id(root) + 1;
    let form = |id: i64, node: Node| S { id, ..s::make(node) };
    let note = format!(
        "{} \u{b7} {}{}",
        top,
        if sub.len() > 1 { format!("with {}", plural(sub.len() - 1, "upstream binding")) } else { "no upstream bindings".to_owned() },
        if inputs.is_empty() { String::new() } else { format!(" and inputs {}", inputs.join(", ")) }
    );
    let vec_items: Vec<S> = sub.iter().flat_map(|(p, v)| [(*p).clone(), (*v).clone()]).collect();
    let mut out = form(
        base,
        Node::List(vec![
            form(base + 1, Node::Sym("let*".to_owned())),
            form(base + 2, Node::Vec(vec_items)),
            form(base + 3, Node::Sym(top.to_owned())),
        ]),
    );
    out.notes = vec![
        note,
        "Selection patches named bindings; omitted bindings stay. Change the result in Graph or Document.".to_owned(),
    ];
    Some(out)
}

fn e_edit(message: String) -> Diagnostic {
    diagnostic::error("E_EDIT".to_owned(), message, None, None, None, Vec::new(), Vec::new())
}

fn printed_of(f: &S) -> String {
    lisp::print(std::slice::from_ref(f)).0
}

/// Build the complete candidate graph before checking: dependent bindings may change type
/// together, so checking individual replacements is incorrect.
fn selection_form(source: &[S], path: &[String], form: &S) -> Result<S, Diagnostic> {
    let shape = || e_edit("Expected the shown (let* [...] name) closure, or the graph form.".to_owned());
    let (graph, top) = match path {
        [graph, top, ..] => (graph, top),
        _ => return Err(shape()),
    };
    let Some((patches, result)) = scope(form) else { return Err(shape()) };
    let names: Vec<&String> = patches
        .iter()
        .filter_map(|(p, _)| match &p.node {
            Node::Sym(n) => Some(n),
            _ => None,
        })
        .collect();
    let invalid = patches.iter().find(|(p, _)| match &p.node {
        Node::Sym(n) => binding(source, &[graph.clone(), n.clone()]).is_none(),
        _ => true,
    });
    if let Some((p, _)) = invalid {
        return Err(e_edit(format!(
            "{} is not a binding of {}; add or remove bindings in the Graph tab.",
            pxui_shell::kit::ocaml_trim(&printed_of(p)),
            graph
        )));
    }
    let mut sorted = names.clone();
    sorted.sort();
    sorted.dedup();
    if names.len() != sorted.len() {
        return Err(e_edit("Duplicate binding names in Selection.".to_owned()));
    }
    if printed_of(result) != printed_of(&s::make(Node::Sym(top.clone()))) {
        return Err(e_edit("Selection's result must stay unchanged; edit the result in Graph or Document.".to_owned()));
    }
    let Some(root) = root_form(source, graph) else { return Err(e_edit("Selection graph no longer exists.".to_owned())) };
    let Some(body) = last(root) else { return Err(e_edit("Selection graph no longer exists.".to_owned())) };
    let Some((bindings, res)) = scope(body) else { return Err(e_edit("Expected the shown Selection closure.".to_owned())) };
    // a closure that changes nothing is the graph as it is, not a refusal: a scrub applies on
    // every frame of its drag, also the ones that did not move
    let bindings: Vec<(S, S)> = bindings
        .iter()
        .map(|(p, old)| match patches.iter().find(|(q, _)| q.node == p.node) {
            Some((_, v)) if printed_of(old) != printed_of(v) => {
                let mut v = (*v).clone();
                if v.notes.is_empty() {
                    v.notes = old.notes.clone();
                }
                ((*p).clone(), v)
            }
            _ => ((*p).clone(), (*old).clone()),
        })
        .collect();
    let kids = s::children(body);
    let vec_form = S { node: Node::Vec(bindings.into_iter().flat_map(|(p, v)| [p, v]).collect()), ..kids[1].clone() };
    let new_body = S { node: Node::List(vec![kids[0].clone(), vec_form, res.clone()]), ..body.clone() };
    let mut root_kids: Vec<S> = s::children(root).into_iter().cloned().collect();
    root_kids.pop();
    root_kids.push(new_body);
    Ok(S { node: Node::List(root_kids), ..root.clone() })
}

/// The `Set_graph` of the graph form a Graph tab holds, or (with `selection`, the shown binding's
/// path) of the Selection closure patched into the graph; the error of text that is neither.
pub fn graph_op(source: &[S], graph: &str, selection: Option<&[String]>, text: &str) -> Result<Op, Diagnostic> {
    let set = |form: S| Op::SetGraph { name: graph.to_owned(), form };
    let forms = s::parse(text)?;
    match forms.as_slice() {
        [form] if matches!(&form.node, Node::List(l) if matches!(l.first().map(|f| &f.node), Some(Node::Sym(h)) if h == "graph" || h == "defn")) => {
            Ok(set(form.clone()))
        }
        [form] => match selection {
            Some(path) => selection_form(source, path, form).map(set),
            None => Ok(set(form.clone())),
        },
        _ => match selection {
            Some(_) => Err(e_edit("Expected the shown (let* [...] name) closure, or the graph form.".to_owned())),
            None => Err(e_edit("Expected the one graph form".to_owned())),
        },
    }
}

fn make_shown_with(workspace: Option<&WorkspaceDoc>, source: &[S], graph: &str, selected: Option<&Path>, tab: Tab) -> Shown {
    let source_arc = Arc::new(source.to_vec());
    let printed = match workspace {
        None => Printed::of_forms(source_arc.clone()),
        Some(w) => Printed::of_workspace(w),
    };
    let key: Path = match selected {
        Some(path) => path.clone(),
        None => vec![graph.to_owned()],
    };
    match tab {
        Tab::Document => {
            let spans = printed.spans().clone();
            Shown {
                graph: graph.to_owned(),
                text: printed.text().to_owned(),
                mark: selected.and_then(|p| path_mark(source, &spans, p)),
                key,
                applied: printed.clone(),
                body: None,
                source: source_arc,
                spans: Arc::new(spans),
                applied_spans: printed,
            }
        }
        Tab::Selection | Tab::Graph => match root_form(source, graph) {
            None => Shown {
                graph: graph.to_owned(),
                text: String::new(),
                mark: None,
                key,
                applied: printed.clone(),
                body: None,
                source: source_arc,
                spans: Arc::new(Vec::new()),
                applied_spans: printed,
            },
            Some(root) => {
                let names: &[String] = match selected {
                    Some(p) if !p.is_empty() => &p[1..],
                    _ => &[],
                };
                let (form, is_root): (S, bool) = match (tab, names.first()) {
                    (Tab::Selection, Some(top)) => match closure(root, top) {
                        Some(f) => (f, false),
                        None => (root.clone(), true),
                    },
                    _ => (root.clone(), true),
                };
                let (text, spans) = lisp::print(std::slice::from_ref(&form));
                let body = if is_root { last(root).unwrap_or(root).clone() } else { form };
                Shown {
                    graph: graph.to_owned(),
                    mark: selected.and_then(|p| path_mark(source, &spans, p)),
                    text,
                    key,
                    applied: printed.clone(),
                    body: Some((Arc::new(body), Arc::new(spans.clone()))),
                    source: source_arc,
                    spans: Arc::new(spans),
                    applied_spans: printed,
                }
            }
        },
    }
}

/// `make_shown source graph selected tab`: Selection prints the selected binding's top-level
/// ancestor as a `let*` over the root bindings it reads (a note names the count and the graph
/// inputs used) with the binding marked; Graph the whole graph form; Document the workspace form.
pub fn make_shown(source: &[S], graph: &str, selected: Option<&Path>, tab: Tab) -> Shown {
    make_shown_with(None, source, graph, selected, tab)
}

fn atoms(old: &S, next: &S) -> Option<Vec<S>> {
    if s::equal(old, next) {
        return Some(Vec::new());
    }
    if old.id != next.id || old.notes != next.notes || old.meta != next.meta || old.tail != next.tail {
        return None;
    }
    match (&old.node, &next.node) {
        (Node::Num(_), Node::Num(_)) | (Node::Str(_), Node::Str(_)) | (Node::Sym(_), Node::Sym(_)) => Some(vec![next.clone()]),
        (Node::Vec(o), Node::Vec(n)) if o.len() == n.len() => {
            let mut values: Vec<S> = Vec::new();
            for (o, n) in o.iter().zip(n) {
                let mut more = atoms(o, n)?;
                more.reverse();
                more.extend(values);
                values = more;
            }
            Some(values)
        }
        _ => None,
    }
}

fn same_workspace(a: &WorkspaceDoc, b: &WorkspaceDoc) -> bool {
    Arc::ptr_eq(&a.checked, &b.checked)
        && Arc::ptr_eq(&a.inputs, &b.inputs)
        && a.layout == b.layout
        && settings::same_values(&a.settings, &b.settings)
        && a.extra == b.extra
        && a.fragment == b.fragment
        && a.imports == b.imports
}

fn patched_shown(previous: &WorkspaceDoc, workspace: &WorkspaceDoc, shown: &Shown) -> Option<Shown> {
    if previous.layout != workspace.layout || !settings::same_values(&previous.settings, &workspace.settings) || previous.extra != workspace.extra {
        return None;
    }
    let changes: Vec<Change> = wd::literal_changes(previous, workspace)?;
    let mut values: Vec<S> = Vec::new();
    for change in &changes {
        let old = fe::arg_text(&previous.source, &change.path, &ArgKey::Kw(change.field.clone()))?;
        let mut more = atoms(&old, &change.expr)?;
        more.reverse();
        more.extend(values);
        values = more;
    }
    // Another graph may have changed; only forms printed in this tab are patched.
    let present = |spans: &Spans| -> Vec<S> {
        let ids: std::collections::HashSet<i64> = spans.iter().map(|(id, _)| *id).collect();
        values.iter().filter(|f| ids.contains(&f.id)).cloned().collect()
    };
    let (text, spans) = lisp::patch_atoms(&(shown.text.clone(), (*shown.spans).clone()), &present(&shown.spans))?;
    let old_applied_spans = shown.applied_spans.spans().clone();
    let (applied, applied_spans) =
        lisp::patch_atoms(&(shown.applied.text().to_owned(), old_applied_spans.clone()), &present(&old_applied_spans))?;
    let body = root_form(&workspace.source, &shown.graph).and_then(last).cloned();
    let new_body = match (&shown.body, body) {
        (Some(_), Some(body)) => Some((Arc::new(body), Arc::new(spans.clone()))),
        _ => None,
    };
    // Selection marks are form boundaries in the repaired span map.
    let boundary = |old: i64| -> Option<i64> {
        shown.spans.iter().find_map(|(id, span)| {
            if span.start == old {
                spans.iter().find(|(i, _)| i == id).map(|(_, s)| s.start)
            } else if span.finish == old {
                spans.iter().find(|(i, _)| i == id).map(|(_, s)| s.finish)
            } else {
                None
            }
        })
    };
    let mark = shown.mark.map(|(a, b)| (boundary(a).unwrap_or(a), boundary(b).unwrap_or(b)));
    let printed = Printed::ready(applied, applied_spans);
    Some(Shown {
        source: Arc::new(workspace.source.clone()),
        text,
        applied: printed.clone(),
        applied_spans: printed,
        body: new_body,
        mark,
        spans: Arc::new(spans),
        ..shown.clone()
    })
}

/// `make_shown` for the state's tab, cached by source, optional workspace, graph, selection and
/// tab. A workspace includes saved layout and settings in the Document tab.
/// ponytail: the cache compares the source structurally (OCaml `==`), O(source) per frame; hold the
/// source in an `Arc` end to end if that shows in a profile.
pub fn shown(workspace: Option<&WorkspaceDoc>, state: &State, source: &[S], graph: &str, selected: Option<&Path>) -> (State, Shown) {
    let graph = match (&state.tab, &state.binding_draft) {
        (Tab::Selection, Some((path, _))) if !path.is_empty() => path[0].as_str(),
        _ => graph,
    };
    let selected: Option<Path> = match (&state.tab, &state.binding_draft, &state.target) {
        (Tab::Selection, Some((path, _)), _) => Some(path.clone()),
        (Tab::Document, _, Some(path)) => Some(path.clone()),
        _ => selected.cloned(),
    };
    let same = |a: &Option<Box<WorkspaceDoc>>, b: Option<&WorkspaceDoc>| match (a, b) {
        (None, None) => true,
        (Some(a), Some(b)) => same_workspace(a, b),
        _ => false,
    };
    if let Some(cache) = &state.cache {
        let (key, shown) = &**cache;
        if *key.source == *source && same(&key.workspace, workspace) && key.graph == graph && key.selected == selected && key.tab == state.tab {
            return (state.clone(), shown.clone());
        }
    }
    let patched = match (&state.cache, workspace) {
        (Some(cache), Some(workspace)) => {
            let (key, shown) = &**cache;
            match &key.workspace {
                Some(previous) if state.scrubbing && key.graph == graph && key.selected == selected && key.tab == state.tab => {
                    patched_shown(previous, workspace, shown)
                }
                _ => None,
            }
        }
        _ => None,
    };
    let shown = match patched {
        Some(shown) => shown,
        None => make_shown_with(workspace, source, graph, selected.as_ref(), state.tab),
    };
    let key = CacheKey {
        source: shown.source.clone(),
        workspace: workspace.map(|w| Box::new(w.clone())),
        graph: graph.to_owned(),
        selected,
        tab: state.tab,
    };
    let mut next = state.clone();
    next.cache = Some(Box::new((key, shown.clone())));
    (next, shown)
}

// The printer's token ID names the source form. Prefer the innermost card, then address its
// argument's child; no text is parsed or searched.
const LOOP_HEADS: [&str; 6] = ["for", "fold", "scan", "sum", "state", "fn"];

fn scrub_sub(id: i64, path: &mut Vec<i64>, form: &S) -> Option<Vec<i64>> {
    if form.id == id {
        return match form.node {
            Node::Num(_) => Some(path.clone()),
            _ => None,
        };
    }
    for (i, child) in s::children(form).into_iter().enumerate() {
        path.push(i as i64);
        let found = scrub_sub(id, path, child);
        path.pop();
        if found.is_some() {
            return found;
        }
    }
    None
}

fn scrub_within(id: i64, value: &S, path: &[String], form: &S) -> Option<Op> {
    match scope(form) {
        Some((bindings, result)) => {
            for (pattern, expr) in &bindings {
                if let found @ Some(_) = scrub_card(id, value, &append(path, &fe::pat_key(pattern)), expr) {
                    return found;
                }
            }
            scrub_card(id, value, &append(path, "@result"), result)
        }
        None => scrub_card(id, value, &append(path, "@result"), form),
    }
}

fn scrub_card(id: i64, value: &S, path: &[String], form: &S) -> Option<Op> {
    if scope(form).is_some() || LOOP_HEADS.contains(&s::head(form).unwrap_or("")) {
        let (inner, _) = enter(form);
        if let found @ Some(_) = scrub_within(id, value, path, inner) {
            return found;
        }
    }
    for (key, expr) in fe::nested_nodes(form) {
        let (last_leaf, init) = path.split_last()?;
        let Ok(leaf) = fe::nested_leaf(last_leaf, &key) else { continue };
        let next = append(init, &leaf);
        if let found @ Some(_) = scrub_card(id, value, &next, &expr) {
            return found;
        }
    }
    let args: Vec<(ArgKey, S)> = match &form.node {
        Node::List(l) if !l.is_empty() => {
            let mut out = Vec::new();
            let mut position = 0;
            let mut rest = &l[1..];
            while !rest.is_empty() {
                if let (Node::Kw(name), Some(expr)) = (&rest[0].node, rest.get(1)) {
                    out.push((ArgKey::Kw(name.clone()), expr.clone()));
                    rest = &rest[2..];
                } else {
                    out.push((ArgKey::Pos(position), rest[0].clone()));
                    position += 1;
                    rest = &rest[1..];
                }
            }
            out
        }
        Node::Map(fields) => s::pairs(fields)
            .into_iter()
            .map(|(key, expr)| (ArgKey::Field(if let Node::Kw(name) = &key.node { name.clone() } else { String::new() }), expr.clone()))
            .collect(),
        _ => vec![(ArgKey::Whole, form.clone())],
    };
    for (key, expr) in args {
        if fe::node_call(&expr) {
            continue;
        }
        if let Some(sub) = scrub_sub(id, &mut Vec::new(), &expr) {
            return Some(Op::SetArg { node: path.to_vec(), key, sub, value: value.clone() });
        }
    }
    None
}

/// A numeric token reported by PXUI, looked up through printed spans and mapped to its innermost
/// source card's `Set_arg`. No parsing or textual search.
pub fn scrub_op(shown: &Shown, range: (i64, i64), replacement: &str) -> Option<Op> {
    let (start, finish) = range;
    let id = shown.spans.iter().find(|(id, span)| *id != 0 && span.start == start && span.finish == finish).map(|(id, _)| *id)?;
    if !s::number(replacement) {
        return None;
    }
    let value = s::make(Node::Num(replacement.to_owned()));
    for root in root_forms(&shown.source)? {
        if let Node::List(l) = &root.node
            && l.len() >= 2
            && let (Node::Sym(h), Node::Sym(name)) = (&l[0].node, &l[1].node)
            && matches!(h.as_str(), "graph" | "defn")
        {
            let prefix = if h == "defn" { format!("def:{}", name) } else { name.clone() };
            if let Some(body) = last(root)
                && let found @ Some(_) = scrub_within(id, &value, &[prefix], body)
            {
                return found;
            }
        }
    }
    None
}

// ---- a draft against a document that changed under it ----

type Entries = Vec<(String, Vec<S>)>;

fn text_of(f: &S) -> String {
    printed_of(f)
}

fn same(a: &S, b: &S) -> bool {
    text_of(a) == text_of(b)
}

fn sames(a: &[S], b: &[S]) -> bool {
    a.len() == b.len() && a.iter().zip(b).all(|(x, y)| same(x, y))
}

fn entry_key(f: &S) -> String {
    if let Node::List(l) = &f.node {
        match (l.first().map(|f| &f.node), l.get(1).map(|f| &f.node)) {
            (Some(Node::Sym(h)), Some(Node::Sym(n))) => return format!("{} {}", h, n),
            (Some(Node::Sym(h)), _) => return h.clone(),
            _ => {}
        }
    }
    text_of(f)
}

fn entries(forms: &[S]) -> Entries {
    forms.iter().map(|f| (entry_key(f), vec![f.clone()])).collect()
}

fn pair_entries(forms: &[S]) -> Option<Entries> {
    if forms.len() % 2 != 0 {
        return None;
    }
    Some(forms.chunks_exact(2).map(|p| (text_of(&p[0]), vec![p[0].clone(), p[1].clone()])).collect())
}

fn merge_seq(bl: &[S], ml: &[S], tl: &[S]) -> Option<Vec<S>> {
    if bl.len() == ml.len() && ml.len() == tl.len() {
        (0..ml.len()).map(|i| merge_forms(&bl[i], &ml[i], &tl[i])).collect()
    } else {
        None
    }
}

fn merge_keyed(bl: &Entries, ml: &Entries, tl: &Entries) -> Option<Vec<S>> {
    let unique = |l: &Entries| {
        let mut keys: Vec<&String> = l.iter().map(|(k, _)| k).collect();
        let n = keys.len();
        keys.sort();
        keys.dedup();
        keys.len() == n
    };
    if !(unique(bl) && unique(ml) && unique(tl)) {
        return None;
    }
    let find = |l: &Entries, k: &str| l.iter().find(|(x, _)| x == k).map(|(_, v)| v.clone());
    let mut out: Vec<Vec<S>> = Vec::new();
    for (k, m) in ml {
        let part = match (find(bl, k), find(tl, k)) {
            (Some(b), Some(t)) => merge_seq(&b, m, &t)?,
            (Some(b), None) => {
                if sames(m, &b) {
                    Vec::new()
                } else {
                    return None;
                }
            }
            (None, Some(t)) => {
                if sames(m, &t) {
                    m.clone()
                } else {
                    return None;
                }
            }
            (None, None) => m.clone(),
        };
        out.push(part);
    }
    for (k, t) in tl {
        let part = match (ml.iter().any(|(x, _)| x == k), find(bl, k)) {
            (true, _) => Vec::new(),
            (false, None) => t.clone(),
            (false, Some(b)) => {
                if sames(t, &b) {
                    Vec::new()
                } else {
                    return None;
                }
            }
        };
        out.push(part);
    }
    Some(out.into_iter().flatten().collect())
}

// a call whose argument count differs: what stands before the first keyword pairs by position, the
// keyword arguments by name
fn merge_call(bl: &[S], ml: &[S], tl: &[S]) -> Option<Vec<S>> {
    fn split(forms: &[S]) -> Option<(Vec<S>, Entries)> {
        let at = forms.iter().position(|f| matches!(f.node, Node::Kw(_))).unwrap_or(forms.len());
        let (before, rest) = forms.split_at(at);
        let mut keywords = true;
        let mut i = 0;
        while i < rest.len() {
            if matches!(rest[i].node, Node::Kw(_)) && i + 1 < rest.len() {
                i += 2;
            } else {
                keywords = false;
                break;
            }
        }
        if rest.is_empty() {
            return Some((before.to_vec(), Vec::new()));
        }
        let pairs = if keywords { pair_entries(rest) } else { None };
        pairs.map(|p| (before.to_vec(), p))
    }
    let (bp, bk) = split(bl)?;
    let (mp, mk) = split(ml)?;
    let (tp, tk) = split(tl)?;
    let mut positional = merge_seq(&bp, &mp, &tp)?;
    positional.extend(merge_keyed(&bk, &mk, &tk)?);
    Some(positional)
}

fn merge_forms(b: &S, m: &S, t: &S) -> Option<S> {
    if same(m, b) {
        return Some(t.clone());
    }
    if same(t, b) || same(m, t) {
        return Some(m.clone());
    }
    let node = |n: Node| S { node: n, ..m.clone() };
    match (&b.node, &m.node, &t.node) {
        (Node::List(bl), Node::List(ml), Node::List(tl))
            if bl.len() >= 2
                && matches!(&bl[0].node, Node::Sym(h) if h == "let*")
                && matches!(bl[1].node, Node::Vec(_))
                && ml.len() >= 2
                && matches!(ml[1].node, Node::Vec(_))
                && tl.len() >= 2
                && matches!(tl[1].node, Node::Vec(_)) =>
        {
            let (Node::Vec(bv), Node::Vec(mv), Node::Vec(tv)) = (&bl[1].node, &ml[1].node, &tl[1].node) else { return None };
            let (bv, mv, tv) = (pair_entries(bv)?, pair_entries(mv)?, pair_entries(tv)?);
            let bindings = merge_keyed(&bv, &mv, &tv)?;
            let rest = merge_seq(&bl[2..], &ml[2..], &tl[2..])?;
            let mut kids = vec![ml[0].clone(), S { node: Node::Vec(bindings), ..ml[1].clone() }];
            kids.extend(rest);
            Some(node(Node::List(kids)))
        }
        (Node::List(bl), Node::List(ml), Node::List(tl)) if matches!(bl.first().map(|f| &f.node), Some(Node::Sym(h)) if h == "workspace") => {
            Some(node(Node::List(merge_keyed(&entries(bl), &entries(ml), &entries(tl))?)))
        }
        (Node::List(bl), Node::List(ml), Node::List(tl)) => {
            let kids = match merge_seq(bl, ml, tl) {
                Some(k) => Some(k),
                None => merge_call(bl, ml, tl),
            };
            Some(node(Node::List(kids?)))
        }
        (Node::Vec(bl), Node::Vec(ml), Node::Vec(tl)) => Some(node(Node::Vec(merge_seq(bl, ml, tl)?))),
        (Node::Map(bl), Node::Map(ml), Node::Map(tl)) => Some(node(Node::Map(merge_seq(bl, ml, tl)?))),
        _ => None,
    }
}

/// A draft (`mine`) begun from `base`, against the text the document has now (`theirs`): the
/// three merged on the syntax tree, graphs and `let*` bindings paired by name. `None` when both
/// changed the same value or the draft does not read.
/// ponytail: a binding or keyword argument `theirs` added goes last in its form; keep `theirs`'
/// place for it when the order of bindings bites (the merged text is still checked).
pub fn merge3(base: &str, mine: &str, theirs: &str) -> Option<String> {
    let (b, m, t) = (s::parse(base).ok()?, s::parse(mine).ok()?, s::parse(theirs).ok()?);
    let forms = merge_keyed(&entries(&b), &entries(&m), &entries(&t))?;
    Some(lisp::print(&forms).0)
}

// ---- find, symbols, lines ----

/// The (start, stop) of every non-overlapping match, in order; ASCII letters fold together unless
/// `case`.
pub fn find_all(case: bool, query: &str, text: &str) -> Vec<(i64, i64)> {
    let n = query.len();
    if n == 0 {
        return Vec::new();
    }
    let (hay, needle) = if case { (text.as_bytes().to_vec(), query.as_bytes().to_vec()) } else { (text.to_ascii_lowercase().into_bytes(), query.to_ascii_lowercase().into_bytes()) };
    let mut out = Vec::new();
    let mut from = 0;
    while from + n <= hay.len() {
        match (from..=hay.len() - n).find(|&i| hay[i..i + n] == needle[..]) {
            Some(i) => {
                out.push((i as i64, (i + n) as i64));
                from = i + n;
            }
            None => break,
        }
    }
    out
}

/// The match the bar is on: the first at or after `at`, else the first.
pub fn find_current(f: &Find, matches: &[(i64, i64)]) -> Option<(i64, i64)> {
    matches.iter().find(|(a, _)| *a >= f.at).or_else(|| matches.first()).copied()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Step {
    Next,
    Previous,
}

/// The bar after Next or Previous (wrapping at either end).
pub fn find_step(f: &Find, matches: &[(i64, i64)], direction: Step) -> Find {
    match find_current(f, matches) {
        None => f.clone(),
        Some((a, _)) => match direction {
            Step::Next => Find { at: a + 1, ..f.clone() },
            Step::Previous => {
                // the last match before `a`, else (wrapping) the last one
                let last = matches.iter().fold(a, |_, (b, _)| *b);
                Find { at: matches.iter().fold(last, |t, (b, _)| if *b < a { *b } else { t }), ..f.clone() }
            }
        },
    }
}

/// Replace the current match, or all, as one request, and the bar after it.
pub fn find_replace(f: &Find, matches: &[(i64, i64)], all: bool) -> Option<(Request, Find)> {
    let n = f.replace.len() as i64;
    let chosen: Vec<(i64, i64)> = if all { matches.to_vec() } else { find_current(f, matches).into_iter().collect() };
    let (a, _) = *chosen.first()?;
    let splice = chosen.iter().map(|(a, b)| (*a, *b, f.replace.clone())).collect();
    if all {
        Some((Request { select: (a, a + n), splice }, Find { at: a, ..f.clone() }))
    } else {
        Some((Request { select: (a + n, a + n), splice }, Find { at: a + n, ..f.clone() }))
    }
}

/// The bindings of a text as (name, kind, byte of the name): graphs, functions, macros and `let*`
/// names, in order; empty for text that does not read.
/// ponytail: the text is parsed per request, not cached
pub fn symbols(text: &str) -> Vec<(String, String, i64)> {
    fn walk(acc: &mut Vec<(String, String, i64)>, f: &S) {
        match &f.node {
            Node::List(l)
                if l.len() >= 2
                    && matches!(&l[0].node, Node::Sym(k) if matches!(k.as_str(), "graph" | "defn" | "defmacro"))
                    && matches!(l[1].node, Node::Sym(_)) =>
            {
                if let (Node::Sym(kind), Node::Sym(name)) = (&l[0].node, &l[1].node) {
                    acc.push((name.clone(), kind.clone(), l[1].span.start));
                }
            }
            Node::List(l) if l.len() == 3 && matches!(&l[0].node, Node::Sym(h) if h == "let*") && matches!(l[1].node, Node::Vec(_)) => {
                if let Node::Vec(bs) = &l[1].node {
                    for (p, _) in s::pairs(bs) {
                        if let Node::Sym(name) = &p.node {
                            acc.push((name.clone(), "let*".to_owned(), p.span.start));
                        }
                    }
                }
            }
            _ => {}
        }
        for c in s::children(f) {
            walk(acc, c);
        }
    }
    let Ok(forms) = s::parse(text) else { return Vec::new() };
    let mut acc = Vec::new();
    for f in &forms {
        walk(&mut acc, f);
    }
    acc
}

/// the byte where 1-based `line` of `text` starts (the last line's for a line past the end)
fn line_start(text: &str, line: i64) -> i64 {
    let (mut k, mut from) = (line, 0);
    while k > 1 {
        match index_from(text, from, b'\n') {
            Some(j) => {
                from = j + 1;
                k -= 1;
            }
            None => return from,
        }
    }
    from
}

/// where the line starting at `from` ends
fn line_end(text: &str, from: i64) -> i64 {
    index_from(text, from, b'\n').unwrap_or(text.len() as i64)
}

/// OCaml `Uchar.utf_decode_length (String.get_utf_8_uchar text i)`
fn utf8_len(text: &str, i: i64) -> i64 {
    let b = text.as_bytes();
    let i = i as usize;
    if i >= b.len() {
        return 1;
    }
    let want = match b[i] {
        0xC2..=0xDF => 2,
        0xE0..=0xEF => 3,
        0xF0..=0xF4 => 4,
        _ => return 1,
    };
    if i + want <= b.len() && std::str::from_utf8(&b[i..i + want]).is_ok() { want as i64 } else { 1 }
}

/// `line_target text "12:5"`: the byte of that 1-based line and character column (`":5"` is
/// optional; past the end is clamped); `None` for anything else.
pub fn line_target(text: &str, input: &str) -> Option<i64> {
    let number = |s: &str| match pxui_shell::kit::int_of_string_opt(pxui_shell::kit::ocaml_trim(s)) {
        Some(n) if n >= 1 => Some(n),
        _ => None,
    };
    let (line, column) = match input.find(':') {
        Some(i) => (number(&input[..i]), number(&input[i + 1..])),
        None => (number(input), Some(1)),
    };
    let (line, column) = (line?, column?);
    let from = line_start(text, line);
    let stop = line_end(text, from);
    let (mut i, mut k) = (from, column);
    while !(k <= 1 || i >= stop) {
        i += utf8_len(text, i);
        k -= 1;
    }
    Some(i)
}

/// the Graph tab's slot for a graph
pub fn graph_slot<'a>(state: &'a State, graph: &str) -> Option<&'a Slot> {
    state.graphs.iter().find(|(g, _)| g == graph).map(|(_, s)| s)
}

/// What the tab shows and edits: its draft when it has one.
pub fn current_text(state: &State, shown: &Shown) -> String {
    match state.tab {
        Tab::Document => state.draft.clone().unwrap_or_else(|| shown.applied.text().to_owned()),
        Tab::Graph => match graph_slot(state, &shown.graph) {
            Some(s) => s.gtext.clone(),
            None => shown.text.clone(),
        },
        Tab::Selection => match &state.binding_draft {
            Some((k, t)) if *k == shown.key => t.clone(),
            _ => shown.text.clone(),
        },
    }
}

/// The state after a command: `Select` is a request at once, the rest wait for `view`.
pub fn command(state: &State, c: Command) -> State {
    let mut next = state.clone();
    match c {
        Command::Select(a, b) => {
            next.request = Some(Request { select: (a, b), splice: Vec::new() });
            next.nonce += 1;
        }
        c => next.pending.push(c),
    }
    next
}

// ---- errors ----

/// The 1-based line of a diagnostic in the text it was checked from: its position, else its span.
pub fn line_of(text: &str, d: &Diagnostic) -> Option<i64> {
    match (&d.position, &d.span) {
        (Some(p), _) => Some(p.line),
        (None, Some(s)) => Some(diagnostic::position_of_offset(text, s.start).line),
        (None, None) => None,
    }
}

fn real_errors(l: &[Diagnostic]) -> Vec<Diagnostic> {
    l.iter().filter(|d| d.severity == Severity::ErrorSeverity).cloned().collect()
}

/// the byte where 1-based `line` of `text` ends
fn line_stop(text: &str, line: i64) -> i64 {
    line_end(text, line_start(text, line))
}

fn symbol_spans(text: &str) -> Vec<(String, Span)> {
    fn walk(acc: &mut Vec<(String, Span)>, f: &S) {
        if let Node::Sym(name) = &f.node {
            acc.push((name.clone(), f.span.clone()));
        }
        for c in s::children(f) {
            walk(acc, c);
        }
    }
    let Ok(forms) = s::parse(text) else { return Vec::new() };
    let mut acc = Vec::new();
    for f in &forms {
        walk(&mut acc, f);
    }
    acc
}

/// The symbol the byte `at` touches; `None` for anything else or text that does not read.
pub fn symbol_at(text: &str, byte: i64) -> Option<String> {
    symbol_spans(text).into_iter().find(|(_, span)| span.start <= byte && byte <= span.finish).map(|(n, _)| n)
}

/// `references text name`: the byte and the line's text of every use of the symbol `name`, in order.
pub fn references(text: &str, name: &str) -> Vec<(i64, String)> {
    symbol_spans(text)
        .into_iter()
        .filter(|(n, _)| n == name)
        .map(|(_, span)| {
            let line = diagnostic::position_of_offset(text, span.start).line;
            let a = line_start(text, line);
            (span.start, pxui_shell::kit::ocaml_trim(&sub(text, a, line_stop(text, line) - a)).to_owned())
        })
        .collect()
}

/// The diagnostics of `c` that still hold for a text.
pub fn visible(c: &Checked, text: &str) -> Vec<Diagnostic> {
    if c.at == text {
        return c.found.clone();
    }
    let shared = c.at.bytes().zip(text.bytes()).take_while(|(a, b)| a == b).count() as i64;
    c.found
        .iter()
        .filter(|d| match line_of(&c.at, d) {
            Some(line) => shared >= line_stop(&c.at, line).max(d.span.as_ref().map(|s| s.finish).unwrap_or(0)),
            None => false,
        })
        .cloned()
        .collect()
}

/// what the area marks for `text`: the apply errors that still hold and the rested draft's parse
fn errors_of(state: &State, c: &Checked, text: &str) -> Vec<Diagnostic> {
    let mut held = real_errors(&visible(c, text));
    if let Some(Live { seen, found: Some(found), .. }) = &state.live
        && seen == text
    {
        let extra: Vec<Diagnostic> = real_errors(found).into_iter().filter(|d| !held.contains(d)).collect();
        held.extend(extra);
    }
    held
}

/// the parse errors of a draft some tab holds, once it has rested
fn live_errors(state: &State) -> Vec<Diagnostic> {
    if let Some(Live { seen, found: Some(found), .. }) = &state.live
        && (state.draft.as_deref() == Some(seen.as_str())
            || matches!(&state.binding_draft, Some((_, t)) if t == seen)
            || state.graphs.iter().any(|(_, s)| s.gtext == *seen))
    {
        return real_errors(found);
    }
    Vec::new()
}

fn graph_errors(state: &State) -> Vec<Diagnostic> {
    state.graphs.iter().flat_map(|(_, s)| real_errors(&visible(&s.gerrors, &s.gtext))).collect()
}

/// the apply errors that still hold for the document draft and for the selection's draft
fn held_errors(state: &State) -> (Vec<Diagnostic>, Vec<Diagnostic>) {
    (
        real_errors(&visible(&state.doc_errors, state.draft.as_deref().unwrap_or(""))),
        real_errors(&visible(&state.binding_errors, state.binding_draft.as_ref().map(|(_, t)| t.as_str()).unwrap_or(""))),
    )
}

pub fn first_error(state: &State) -> Option<Diagnostic> {
    let (doc, binding) = held_errors(state);
    doc.into_iter().chain(binding).chain(graph_errors(state)).chain(live_errors(state)).next()
}

/// OCaml `%S`: `String.escaped` between double quotes (bytes outside printable ASCII in decimal).
fn ocaml_quote(text: &str) -> String {
    let mut out = String::from("\"");
    for &b in text.as_bytes() {
        match b {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            b'\n' => out.push_str("\\n"),
            b'\t' => out.push_str("\\t"),
            b'\r' => out.push_str("\\r"),
            8 => out.push_str("\\b"),
            32..=126 => out.push(b as char),
            _ => out.push_str(&format!("\\{:03}", b)),
        }
    }
    out.push('"');
    out
}

/// One line for crash reports: tab, draft, first error and its line.
pub fn summary(state: &State) -> String {
    let (doc, binding) = held_errors(state);
    let mut out = format!(
        "{} tab, draft {}",
        match state.tab {
            Tab::Selection => "selection",
            Tab::Graph => "graph",
            Tab::Document => "document",
        },
        if match state.tab {
            Tab::Document => state.draft.is_some(),
            Tab::Graph => !state.graphs.is_empty(),
            Tab::Selection => state.binding_draft.is_some(),
        } {
            "yes"
        } else {
            "no"
        }
    );
    let at_line = |l: Option<i64>| match l {
        Some(l) => format!(" at line {}", l),
        None => String::new(),
    };
    match (doc.first(), &state.draft, binding.first()) {
        (Some(d), Some(draft), _) => out.push_str(&format!(", error{}: {}", at_line(line_of(draft, d)), diagnostic::to_string(d))),
        (_, _, Some(d)) => {
            let l = state.binding_draft.as_ref().and_then(|(_, t)| line_of(t, d));
            out.push_str(&format!(", error{}: {}", at_line(l), diagnostic::to_string(d)));
        }
        (Some(d), None, _) => out.push_str(&format!(", error: {}", diagnostic::to_string(d))),
        (None, _, None) => {
            if let Some(d) = graph_errors(state).into_iter().chain(live_errors(state)).next() {
                out.push_str(&format!(", error: {}", diagnostic::to_string(&d)));
            }
        }
    }
    let mut names: Vec<&String> = state.graphs.iter().map(|(n, _)| n).collect();
    names.sort();
    if !names.is_empty() {
        out.push_str(&format!(", graph drafts: {}", names.iter().map(|s| s.as_str()).collect::<Vec<_>>().join(" ")));
    }
    if let Some(f) = &state.find {
        out.push_str(&format!(", find {} at {}{}", ocaml_quote(&f.query), f.at, if f.replacing { " replacing" } else { "" }));
    }
    out
}

// ---- the pane ----

/// What `view` asks the editor to apply after the frame.
#[derive(Clone, Debug)]
pub enum Intent {
    Tab(Tab),
    DocDraft(String),
    /// Check & apply the whole workspace text
    DocApply(String),
    DocDiscard,
    BindingDraft(Path, String),
    /// Check & apply of Selection: patch the shown closure's named bindings into one candidate
    /// graph, preserving omitted bindings and refusing changed results or duplicate names.
    BindingApply(Path, String),
    BindingDiscard,
    /// the graph's name and its draft text
    GraphDraft(String, String),
    /// Check & apply: the graph's form is replaced (`Flow_edit::Op::SetGraph`)
    GraphApply(String, String),
    /// the graph whose draft is dropped
    GraphDiscard(String),
    /// the debounced parse of the draft
    Live(Live),
    /// the right-click menu opened here, or closed
    Menu(Option<(f64, f64)>),
    ToggleWrap,
    /// the Document text while a number in it is dragged (`true` on the frame the drag ends)
    DocScrub(String, bool),
    GraphScrub(String, String, bool),
    BindingScrub(Path, String, bool),
    /// A token edit against the source shown by the pane. A stale source or unapplied draft takes
    /// the ordinary scrub/merge path.
    LiteralScrub { source: Arc<Vec<S>>, op: Op, position: diagnostic::Position, fallback: Box<Intent> },
    ToggleParinfer,
    /// the colour literal (byte range with its quotes) whose control is open, and whether an edit was made
    Picker(Option<(i64, i64, bool)>),
    /// Command-click on a `(ref name)`: show that graph (the back stack remembers this one)
    OpenGraph(String),
    /// the caret moved into this binding of the Graph tab: select its node
    SelectBinding(Path),
    /// follow and reveal a definition
    JumpDefinition(Path),
    /// a payload is held over the text at this byte of the shown text; `true` on the frame it is released
    CarryOver(i64, bool),
    /// the bar's new state, and whether the view goes to its current match
    FindSet(Option<Find>, bool),
    /// a request given to the area, or taken
    RequestSet(Option<Request>),
    /// Shift-F12: the symbol under the caret, whose uses the editor lists
    ReferencesOf(String),
    /// the pending commands were resolved
    Resolved,
}

/// Seconds a draft rests before `view` parses it.
pub const LIVE_DELAY: f64 = 0.3;
pub fn live_delay() -> f64 {
    LIVE_DELAY
}

fn dirty(state: &State, shown: &Shown) -> bool {
    match &state.draft {
        Some(d) => d != shown.applied.text(),
        None => false,
    }
}

// ---- drawing ----

type Emit = Rc<RefCell<Vec<Intent>>>;

fn push(emit: &Emit, intent: Intent) {
    emit.borrow_mut().push(intent);
}

fn first_line(message: &str) -> String {
    message.replace('\n', " ")
}

/// The kit's colour control (swatch, hex, r g b) in a popup over the literal `text[a..b)`: the
/// new literal while it is edited; `None` once the popup is dismissed.
fn colour_popup(ui: &mut Ui, at: (f64, f64), text: &str, range: (i64, i64)) -> Option<String> {
    let (a, b) = range;
    let theme = ui.theme();
    let literal = sub(text, a + 1, b - a - 2);
    let colour = color::hex(&literal).unwrap_or(color::white());
    let (r, g, bl, _) = color::to_floats(colour);
    let alpha = if literal.len() == 9 { sub(&literal, 7, 2) } else { String::new() };
    let hex = move |r: f64, g: f64, b: f64| format!("{}{}", color::to_hex_string(color::of_unit_floats(None, r, g, b)), alpha);
    let shown = hex(r, g, bl);
    let edit: Rc<RefCell<Option<String>>> = Rc::new(RefCell::new(None));
    let out = edit.clone();
    let open = ui.popup(Some(theme::edge(&theme)), None, true, false, at, 300.0, 24.0, "text-colour", &mut |ui: &mut Ui| {
        let (boxed, cx, cy, cw) = ui.inspector_row(Some(300.0), None, "text-colour-row", "colour");
        let (shown, hex) = (shown.clone(), hex.clone());
        let edit = edit.clone();
        ui.within(&boxed, &mut |ui: &mut Ui| {
            // the kit's swatch and hex field; the literal's alpha stays as written
            let hex_x = cx + 28.0;
            let typed = kit::colour(ui, "text-colour", (cx, cy), 92.0, colour, &shown);
            let from_hex = if typed == shown {
                None
            } else {
                color::hex(&typed).ok().map(|c| {
                    let (r, g, b, _) = color::to_floats(c);
                    hex(r, g, b)
                })
            };
            let sliders_x = hex_x + 72.0;
            let field_w = ((cw - 100.0).max(60.0) - 16.0) / 3.0;
            let mut from_slider: Option<String> = None;
            for (index, (axis, cur)) in [("r", r), ("g", g), ("b", bl)].into_iter().enumerate() {
                let ax_x = sliders_x + (index as f64 * (field_w + 8.0));
                let (cap_y, size, ink) = (kit::cap_y(ui, cy, 20.0), kit::cap_size(ui), theme::ink_3(&theme));
                let boxed2 = boxed.clone();
                ui.draw(
                    &boxed2,
                    Box::new(move |paint, (x, y, _, _)| {
                        let _ = cap_y;
                        paint.text_s((x + ax_x + 2.0, kit::cap_y_paint(paint, y + cy, 20.0)), size, ink, axis);
                    }),
                );
                let display = format!("{:.2}", cur);
                let o = ValueFieldOpts {
                    display: Some(display.clone()),
                    fraction: Some(cur),
                    slide: Some(Rc::new(|f| format!("{:.2}", f))),
                    edit: false,
                    left: false,
                    ..ValueFieldOpts::default()
                };
                let (changed, _) = ui.value_field((ax_x, cy), field_w, 20.0, &o, &|t| cfmt::parse_ocaml_float(t).is_some(), &format!("text-colour-{}", axis), &display);
                if changed != display && from_slider.is_none() {
                    if let Some(v) = cfmt::parse_ocaml_float(&changed) {
                        let v = v.min(1.0).max(0.0);
                        from_slider = Some(hex(if index == 0 { v } else { r }, if index == 1 { v } else { g }, if index == 2 { v } else { bl }));
                    }
                }
            }
            *edit.borrow_mut() = match from_slider {
                Some(s) => Some(s),
                None => from_hex,
            };
        });
    });
    if !open {
        return None;
    }
    let edited = out.borrow().clone();
    Some(format!("{}{}{}", sub(text, 0, a + 1), edited.unwrap_or(literal), sub(text, b - 1, text.len() as i64 - b + 1)))
}

/// the tabs' names: the workspace sheet's narrow Lisp panel (under 400 points) says "Doc"; a
/// document with a draft wears a star
fn tab_labels(width: i64, dirty: bool) -> Vec<(Tab, String)> {
    let doc = if width < 400 { "Doc" } else { "Document" };
    vec![
        (Tab::Selection, "Selection".to_owned()),
        (Tab::Graph, "Graph".to_owned()),
        (Tab::Document, if dirty { format!("{} *", doc) } else { doc.to_owned() }),
    ]
}

/// The room the Selection / Graph / Document tabs of `view` take in the header of a panel `width` wide.
pub fn tabs_width(ui: &mut Ui, width: i64, state: &State, shown: &Shown) -> f64 {
    let mut w = -12.0;
    for (_, label) in tab_labels(width, dirty(state, shown)) {
        w = w + 12.0 + ui.text_width(None, &label);
    }
    w
}

/// a find-bar button right-aligned in a row: it takes its width off `right` and leaves a gap
fn find_button(ui: &mut Ui, key: &str, right: &mut f64, active: bool, name: &str, label: &str, at_y: f64) -> bool {
    let w = kit::button_width(ui, None, false, label);
    *right -= w;
    let o = ButtonOpts { active, ..ButtonOpts::default() };
    let hit = kit::button(ui, &format!("{}{}", key, name), (*right, at_y + 4.0), w, &o, label);
    *right -= 4.0;
    hit
}

fn rule(ui: &mut Ui, theme: &Theme, key: &str, x: f64, width: f64, name: &str, at: f64) {
    let boxed = ui.box_(&format!("{}{}", key, name), BoxOpts::default().w(Size::Px(width)).h(Size::Px(1.0)).at((x, at)));
    let edge = theme::edge(theme);
    ui.draw(&boxed, Box::new(move |paint, (x, y, w, h)| paint.fill(x, y, w, h, edge)));
}

/// the find bar between the text and the footer: one row, two with the replace row
#[allow(clippy::too_many_arguments)]
fn find_bar(
    ui: &mut Ui,
    emit: &Emit,
    state: &State,
    theme: &Theme,
    find_row: f64,
    key: &str,
    width: f64,
    x: f64,
    current: Option<(i64, i64)>,
    matches: &[(i64, i64)],
    ay: f64,
) {
    let Some(f) = &state.find else { return };
    let ry = ay + 1.0;
    rule(ui, theme, key, x, width, "-find-rule", ay);
    let mut f2 = Find { grab: false, ..f.clone() };
    let (mut reveal, mut closed) = (false, false);
    let total = matches.len();
    let index = match current {
        Some(m) => 1 + matches.iter().position(|x| *x == m).unwrap_or(0),
        None => 0,
    };
    let count = if f.query.is_empty() {
        String::new()
    } else if total == 0 {
        "No matches".to_owned()
    } else {
        format!("{} / {}", index, total)
    };
    let mut right = x + width - 12.0;
    if find_button(ui, key, &mut right, false, "-find-close", "Close", ry) {
        closed = true;
    }
    if find_button(ui, key, &mut right, f.replacing, "-find-replace", "Replace", ry) {
        f2.replacing = !f.replacing;
    }
    if find_button(ui, key, &mut right, f.case, "-find-case", "Aa", ry) {
        f2.case = !f.case;
        reveal = true;
    }
    if find_button(ui, key, &mut right, false, "-find-next", "Next", ry) {
        f2 = find_step(&f2, matches, Step::Next);
        reveal = true;
    }
    if find_button(ui, key, &mut right, false, "-find-prev", "Prev", ry) {
        f2 = find_step(&f2, matches, Step::Previous);
        reveal = true;
    }
    let count_w = 76.0;
    let boxed = ui.box_(&format!("{}-find-count", key), BoxOpts::default().w(Size::Px(count_w)).h(Size::Px(find_row)).at((right - count_w, ry)));
    let (bad, ink2) = (total == 0 && !f.query.is_empty(), theme::ink_2(theme));
    ui.draw(
        &boxed,
        Box::new(move |paint, (x, y, w, h)| {
            let tw = paint.text_width(None, &count);
            paint.text_c((x + w - tw, paint.text_top(None, y, h)), if bad { theme::invalid() } else { ink2 }, &count);
        }),
    );
    let (query, step) = ui.line_field(
        (x + 12.0, ry + 4.0),
        (right - count_w - 8.0 - x - 12.0).max(60.0),
        20.0,
        f.grab,
        "Find",
        &format!("{}-find", key),
        &f.query,
    );
    if query != f.query {
        f2.query = query;
        reveal = true;
    }
    match step {
        LineStep::Next => {
            f2 = find_step(&f2, matches, Step::Next);
            reveal = true;
        }
        LineStep::Previous => {
            f2 = find_step(&f2, matches, Step::Previous);
            reveal = true;
        }
        LineStep::Close => closed = true,
        LineStep::None => {}
    }
    if f.replacing {
        let ry2 = ry + find_row;
        let mut right2 = x + width - 12.0;
        let replace_with = |f2: &mut Find, reveal: &mut bool, all: bool| {
            if let Some((r, next)) = find_replace(f2, matches, all) {
                push(emit, Intent::RequestSet(Some(r)));
                *f2 = next;
                *reveal = true;
            }
        };
        if find_button(ui, key, &mut right2, false, "-replace-all", "All", ry2) {
            replace_with(&mut f2, &mut reveal, true);
        }
        if find_button(ui, key, &mut right2, false, "-replace-one", "Replace", ry2) {
            replace_with(&mut f2, &mut reveal, false);
        }
        let (by, step) = ui.line_field(
            (x + 12.0, ry2 + 4.0),
            (right2 - 8.0 - x - 12.0).max(60.0),
            20.0,
            false,
            "Replace with",
            &format!("{}-with", key),
            &f.replace,
        );
        if by != f.replace {
            f2.replace = by;
        }
        match step {
            LineStep::Next => replace_with(&mut f2, &mut reveal, false),
            LineStep::Close => closed = true,
            LineStep::Previous | LineStep::None => {}
        }
    }
    if closed {
        push(emit, Intent::FindSet(None, false));
        push(emit, Intent::RequestSet(Some(Request { select: current.unwrap_or((f.at, f.at)), splice: Vec::new() })));
    } else if f2 != *f {
        push(emit, Intent::FindSet(Some(f2), reveal));
    }
}

/// what the byte of a text maps to in the tabs' graph, for a text that is not the shown one
fn caret_select(text: &str, state: &State, shown: &Shown) -> Rc<dyn Fn(i64) -> Option<Path>> {
    if text == shown.text {
        let shown = shown.clone();
        return Rc::new(move |byte| binding_at(&shown, byte));
    }
    let (text, tab, shown) = (text.to_owned(), state.tab, shown.clone());
    let mapped: Rc<std::cell::OnceCell<Option<Shown>>> = Rc::new(std::cell::OnceCell::new());
    Rc::new(move |byte| {
        let mapped = mapped.get_or_init(|| {
            let forms = s::parse(&text).ok()?;
            fn spans(form: &S) -> Spans {
                let mut out = vec![(form.id, form.span.clone())];
                for c in s::children(form) {
                    out.extend(spans(c));
                }
                out
            }
            let all: Spans = forms.iter().flat_map(spans).collect();
            let all_arc = Arc::new(all.clone());
            Some(match (tab, forms.as_slice()) {
                (Tab::Document, _) => Shown { source: Arc::new(forms.clone()), spans: all_arc, body: None, ..shown.clone() },
                (Tab::Graph, [form]) => Shown { spans: all_arc.clone(), body: last(form).map(|b| (Arc::new(b.clone()), all_arc)), ..shown.clone() },
                (Tab::Selection, [form]) => {
                    let body = if matches!(s::head(form), Some("graph" | "defn")) { last(form).unwrap_or(form).clone() } else { form.clone() };
                    Shown { spans: all_arc.clone(), body: Some((Arc::new(body), all_arc)), ..shown.clone() }
                }
                _ => shown.clone(),
            })
        });
        mapped.as_ref().and_then(|m| binding_at(m, byte))
    })
}

struct AreaCtx {
    x: f64,
    width: f64,
    row: f64,
    theme: Theme,
    footer: f64,
    narrow: bool,
    bar: f64,
    find_row: f64,
    language: Language,
    names: Names,
    time: f64,
}

/// the toolbar and the message row under an editable area; a right-click menu offers the same
/// buttons and the wrap toggle
#[allow(clippy::too_many_arguments)]
fn editor_area(
    ui: &mut Ui,
    emit: &Emit,
    c: &AreaCtx,
    state: &State,
    shown: &Shown,
    key: &str,
    at: (f64, f64),
    text: &str,
    errors: &[Diagnostic],
    spans: Vec<(i64, i64)>,
    reveal: Option<i64>,
    caret_sel: Rc<dyn Fn(i64) -> Option<Path>>,
    apply: &dyn Fn(String) -> Intent,
    discard: Intent,
    can_apply: bool,
    message: &str,
    draft: &dyn Fn(String) -> Intent,
    scrub: &dyn Fn(String, bool) -> Intent,
) {
    let (ey, eh) = at;
    let theme = c.theme;
    let chips = lisp_text::color_chips(text);
    // the find bar sits between the text and the footer: one row, two with the replace row
    let find_h = match &state.find {
        Some(f) => if f.replacing { 2.0 * c.find_row } else { c.find_row },
        None => 0.0,
    };
    let area_h = c.row.max(eh - c.footer - find_h);
    let matches = match &state.find {
        Some(f) => find_all(f.case, &f.query, text),
        None => Vec::new(),
    };
    let current = state.find.as_ref().and_then(|f| find_current(f, &matches));
    // every match is a band, the current one also a bar under it; the view goes to the current one
    let mut spans = spans;
    spans.extend(matches.iter().take(2000).copied());
    let mut shown_chips = chips.clone();
    if let Some((a, b)) = current {
        shown_chips.push((a, b, theme.accent));
    }
    let reveal = match current {
        Some((a, _)) => Some(a),
        None => reveal,
    };
    // a dragged number applies live (`scrub`, merged into one history entry); typing is a draft
    let phase: Rc<Cell<Option<ScrubPhase>>> = Rc::new(Cell::new(None));
    let caret: Rc<Cell<Option<i64>>> = Rc::new(Cell::new(None));
    let token: Rc<RefCell<Option<((i64, i64), String)>>> = Rc::new(RefCell::new(None));
    let selection: Rc<Cell<Option<(i64, i64)>>> = Rc::new(Cell::new(None));
    // Command-click and F12 follow a (ref name) or a definition
    let follow: Rc<dyn Fn(i64)> = {
        let (text, graphs, shown, emit) = (text.to_owned(), c.names.graphs.clone(), shown.clone(), emit.clone());
        Rc::new(move |byte| {
            let target = match lisp_text::ref_at(&text, byte.max(0) as usize) {
                Some(name) if graphs.contains(&name) => Some(vec![name]),
                _ => {
                    if text == shown.text {
                        definition_at(&shown, byte)
                    } else {
                        None
                    }
                }
            };
            if let Some(path) = target {
                if path_mark(&shown.source, &shown.spans, &path).is_none() {
                    push(&emit, Intent::Tab(Tab::Document));
                }
                push(&emit, Intent::JumpDefinition(path));
            }
        })
    };
    let mut o = TextAreaOpts::default();
    o.wrap = state.wrap;
    o.errors = errors.iter().filter_map(|d| line_of(text, d)).collect();
    o.messages = errors
        .iter()
        .filter_map(|d| line_of(text, d).map(|line| (line, d.span.as_ref().map(|s| (s.start, s.finish)), format!("{}  {}", d.code, first_line(&d.message)))))
        .collect();
    o.spans = spans;
    o.reveal = reveal;
    o.reveal_nonce = state.nonce;
    o.select = state.request.as_ref().map(|r| r.select);
    o.splice = state.request.as_ref().map(|r| r.splice.clone()).unwrap_or_default();
    o.language = Some(c.language.clone());
    o.chips = shown_chips;
    {
        let emit = emit.clone();
        o.on_context = Some(Rc::new(move |at| push(&emit, Intent::Menu(Some(at)))));
    }
    {
        let phase = phase.clone();
        o.on_scrub = Some(Rc::new(move |p| phase.set(Some(p))));
    }
    {
        let token = token.clone();
        o.on_scrub_edit = Some(Rc::new(move |range, replacement| *token.borrow_mut() = Some((range, replacement.to_owned()))));
    }
    {
        let selection = selection.clone();
        o.on_select = Some(Rc::new(move |range| selection.set(Some(range))));
    }
    {
        let emit = emit.clone();
        o.on_drop = Some(Rc::new(move |byte, drop| push(&emit, Intent::CarryOver(byte, matches!(drop, Drop::Dropped(_))))));
    }
    {
        // Command-click follows a (ref name); a click on a colour literal opens the colour control
        let (emit, follow, chips, caret_sel) = (emit.clone(), follow.clone(), chips.clone(), caret_sel.clone());
        o.on_click = Some(Rc::new(move |byte, command| {
            if command {
                follow(byte)
            } else {
                match chips.iter().find(|(a, b, _)| *a <= byte && byte < *b) {
                    Some((a, b, _)) => push(&emit, Intent::Picker(Some((*a, *b, false)))),
                    None => {
                        // the caret in a binding selects its node
                        if let Some(path) = caret_sel(byte) {
                            push(&emit, Intent::SelectBinding(path));
                        }
                    }
                }
            }
        }));
    }
    {
        let caret = caret.clone();
        o.on_caret = Some(Rc::new(move |byte| caret.set(Some(byte))));
    }
    {
        let (emit, caret_sel, shown_key) = (emit.clone(), caret_sel.clone(), shown.key.clone());
        o.on_caret_move = Some(Rc::new(move |byte| {
            if let Some(path) = caret_sel(byte)
                && path != shown_key
            {
                push(&emit, Intent::SelectBinding(path));
            }
        }));
    }
    let (text2, submitted) = ui.text_area_submit((c.x, ey), c.width, area_h, &o, key, text);
    match phase.get() {
        Some(ScrubPhase::Live) => {
            let fallback = scrub(text2.clone(), false);
            let literal = token.borrow().clone().and_then(|(range, replacement)| {
                let (start, finish) = range;
                if text != shown.text || start < 0 || finish > text.len() as i64 || text2 != format!("{}{}{}", sub(text, 0, start), replacement, sub(text, finish, text.len() as i64 - finish)) {
                    None
                } else {
                    scrub_op(shown, range, &replacement).map(|op| (op, diagnostic::position_of_offset(text, start)))
                }
            });
            push(
                emit,
                match literal {
                    Some((op, position)) => Intent::LiteralScrub { source: shown.source.clone(), op, position, fallback: Box::new(fallback) },
                    None => fallback,
                },
            );
        }
        Some(ScrubPhase::Done) => push(emit, scrub(text2.clone(), true)),
        None => {
            if text2 != text {
                push(emit, draft(text2.clone()));
            }
        }
    }
    // the colour control: each edit applies live as one merged history entry, sealed on close
    let text2 = match state.picker {
        Some((a, b, changed)) if b <= text2.len() as i64 && chips.iter().any(|(a2, b2, _)| a == *a2 && b == *b2) => {
            match colour_popup(ui, ((c.x).max(c.x + c.width - 310.0), ey + c.row), &text2, (a, b)) {
                Some(edited) if edited != text2 => {
                    push(emit, Intent::Picker(Some((a, b, true))));
                    push(emit, scrub(edited.clone(), false));
                    edited
                }
                Some(_) => text2,
                None => {
                    push(emit, Intent::Picker(None));
                    if changed {
                        push(emit, scrub(text2.clone(), true));
                    }
                    text2
                }
            }
        }
        Some(_) => {
            push(emit, Intent::Picker(None));
            text2
        }
        None => text2,
    };
    // a draft that rests is parsed once: the debounce is the time the text has been the same
    if can_apply {
        match &state.live {
            Some(l) if l.seen == text => {
                if l.found.is_none() && c.time - l.since >= LIVE_DELAY {
                    let found = match s::parse(text) {
                        Err(d) => vec![d],
                        Ok(_) => Vec::new(),
                    };
                    push(emit, Intent::Live(Live { found: Some(found), ..l.clone() }));
                }
            }
            _ => push(emit, Intent::Live(Live { seen: text.to_owned(), since: c.time, found: None })),
        }
    }
    let can_apply = can_apply || text2 != text;
    let ay = ey + area_h;
    let ty = ay + find_h;
    rule(ui, &theme, key, c.x, c.width, "-rule", ty);
    // what the keys asked this frame, now that the caret and the selection are known
    if state.request.is_some() {
        push(emit, Intent::RequestSet(None));
    }
    for cmd in &state.pending {
        match cmd {
            Command::Find | Command::FindReplace | Command::FindSelection => {
                let base = state.find.clone().unwrap_or(Find { query: String::new(), replace: String::new(), case: false, replacing: false, at: 0, grab: true });
                let picked = match selection.get() {
                    Some((a, b)) if a < b && b <= text2.len() as i64 && !sub(&text2, a, b - a).contains('\n') => Some((a, b)),
                    _ => None,
                };
                let f = Find {
                    query: match picked {
                        Some((a, b)) => sub(&text2, a, b - a),
                        None => base.query.clone(),
                    },
                    replacing: base.replacing || *cmd == Command::FindReplace,
                    at: match (picked, caret.get()) {
                        (Some((a, b)), _) => if *cmd == Command::FindSelection { b } else { a },
                        (None, Some(c)) => c,
                        (None, None) => base.at,
                    },
                    grab: *cmd != Command::FindSelection,
                    ..base
                };
                push(emit, Intent::FindSet(Some(f), true));
            }
            Command::FindNext | Command::FindPrevious => {
                if let Some(f) = &state.find {
                    let step = if *cmd == Command::FindNext { Step::Next } else { Step::Previous };
                    push(emit, Intent::FindSet(Some(find_step(f, &matches, step)), true));
                }
            }
            Command::Definition => {
                if let Some(b) = caret.get() {
                    follow(b)
                }
            }
            Command::References => {
                if let Some(name) = caret.get().and_then(|b| symbol_at(&text2, b)) {
                    push(emit, Intent::ReferencesOf(name));
                }
            }
            Command::Select(..) => {}
        }
    }
    if !state.pending.is_empty() {
        push(emit, Intent::Resolved);
    }
    find_bar(ui, emit, state, &theme, c.find_row, key, c.width, c.x, current, &matches, ay);
    // Command-Enter in the area is the button: the one outlined button of the panel
    let hint = "\u{2318}\u{21b5}";
    // the caret's line and column, 1-based, in the status row's label
    let position = caret.get().map(|byte| {
        let p = diagnostic::position_of_offset(&text2, byte.min(text2.len() as i64));
        format!("{}:{}", p.line, p.col)
    });
    let apply_w = kit::button_width(ui, Some(hint), false, "Check and apply");
    let by = ty + 1.0 + ((c.bar - 20.0) / 2.0);
    let apply_opts = ButtonOpts { primary: true, enabled: can_apply, hint: Some(hint.to_owned()), ..ButtonOpts::default() };
    if kit::button(ui, &format!("{}-apply", key), (c.x + 12.0, by), apply_w, &apply_opts, "Check and apply") || (submitted && can_apply) {
        push(emit, apply(text2.clone()));
    }
    let discard_hint = if c.narrow { None } else { Some("esc") };
    let discard_opts = ButtonOpts { enabled: can_apply, hint: discard_hint.map(|h| h.to_owned()), ..ButtonOpts::default() };
    let discard_w = kit::button_width(ui, discard_hint, false, "Discard");
    if kit::button(ui, &format!("{}-discard", key), (c.x + 12.0 + apply_w + 8.0, by), discard_w, &discard_opts, "Discard") {
        push(emit, discard.clone());
    }
    // parinfer and its switch, at the bar's end
    let switch_x = c.x + c.width - 12.0 - 28.0;
    if !c.narrow {
        if kit::switch(ui, &format!("{}-parinfer", key), (switch_x, ty + 1.0 + ((c.bar - 14.0) / 2.0)), state.parinfer) {
            push(emit, Intent::ToggleParinfer);
        }
        let boxed = ui.box_(&format!("{}-parinfer-label", key), BoxOpts::default().w(Size::Px(80.0)).h(Size::Px(c.bar)).at((switch_x - 8.0 - 80.0, ty + 1.0)));
        let ink2 = theme::ink_2(&theme);
        ui.draw(
            &boxed,
            Box::new(move |paint, (x, y, w, h)| {
                let label = "parinfer";
                let tw = paint.text_width(None, label);
                paint.text_c((x + w - tw, paint.text_top(None, y, h)), ink2, label);
            }),
        );
        rule(ui, &theme, key, c.x, c.width, "-rule2", ty + 1.0 + c.bar);
    }
    let wrong = !errors.is_empty();
    let msg = ui.box_(
        &format!("{}-message", key),
        BoxOpts::default().flags(CLIP).w(Size::Px(c.width)).h(Size::Px(if c.narrow { 0.0 } else { c.row })).at((c.x, ty + c.bar + 2.0)),
    );
    if !c.narrow {
        let (errors, text_c, message) = (errors.to_vec(), text.to_owned(), message.to_owned());
        let accent = theme.accent;
        let ink2 = theme::ink_2(&theme);
        let (state_label, cap_w) = {
            let mut parts: Vec<String> = position.into_iter().collect();
            if can_apply {
                parts.push("modified".to_owned());
            }
            let label = parts.join(" \u{b7} ");
            let w = if label.is_empty() { 0.0 } else { kit::cap_width(ui, &label) };
            (label, w)
        };
        ui.draw(
            &msg,
            Box::new(move |paint, (x, y, w, h)| {
                // the state: a dot in the error colour or the accent for a draft, then what it says; the
                // first error is "n errors" in the error colour and its line and message in ink-2
                let tx = if wrong || can_apply {
                    paint.circle((x + 15.0, y + (h / 2.0)), 3.0, Some(if wrong { theme::invalid() } else { accent }), None);
                    x + 26.0
                } else {
                    x + 12.0
                };
                // the label at the row's end: where the caret is, and whether the text is a draft
                let tail = if !state_label.is_empty() { cap_w + 20.0 } else { 12.0 };
                let ty = paint.text_top(None, y, h);
                match errors.first() {
                    Some(first) => {
                        let count = format!("{} error{}", errors.len(), if errors.len() == 1 { "" } else { "s" });
                        paint.text_c((tx, ty), theme::invalid(), &count);
                        let dx = tx + paint.text_width(None, &count) + 8.0;
                        let detail = format!(
                            "{}{}",
                            match line_of(&text_c, first) {
                                Some(l) => format!("line {} \u{b7} ", l),
                                None => String::new(),
                            },
                            first_line(&diagnostic::to_string(first))
                        );
                        let cut = ellipsis(&mut |s| paint.text_width(None, s), x + w - tail - dx, &detail);
                        paint.text_c((dx, ty), ink2, &cut);
                    }
                    None => {
                        let cut = ellipsis(&mut |s| paint.text_width(None, s), x + w - tail - tx, &message);
                        paint.text_c((tx, ty), ink2, &cut);
                    }
                }
                if !state_label.is_empty() {
                    paint.cap((x + w - 12.0 - cap_w, kit::cap_y_paint(paint, y, h)), None, &state_label);
                }
            }),
        );
    }
    if let Some(at) = state.menu {
        let items: Vec<(String, bool)> = vec![
            ("Check and apply".to_owned(), can_apply),
            ("Discard".to_owned(), can_apply),
            (String::new(), false),
            ((if state.wrap { "Unwrap long lines" } else { "Wrap long lines" }).to_owned(), true),
            (
                (if state.parinfer { "Parinfer off (keep brackets as typed)" } else { "Parinfer on (brackets follow indentation)" }).to_owned(),
                true,
            ),
        ];
        match ui.context_menu(at, &MenuOpts::default(), &format!("{}-menu", key), &items) {
            MenuResult::Open => {}
            MenuResult::Dismiss => push(emit, Intent::Menu(None)),
            MenuResult::Pick(0) => {
                push(emit, Intent::Menu(None));
                push(emit, apply(text2.clone()));
            }
            MenuResult::Pick(1) => {
                push(emit, Intent::Menu(None));
                push(emit, discard.clone());
            }
            MenuResult::Pick(3) => {
                push(emit, Intent::Menu(None));
                push(emit, Intent::ToggleWrap);
            }
            MenuResult::Pick(_) => {
                push(emit, Intent::Menu(None));
                push(emit, Intent::ToggleParinfer);
            }
        }
    }
}

/// The text pane: draws inside `Ui::frame` and returns the intents `Editor_frame` applies after it.
pub fn view(ui: &mut Ui, bounds: (i64, i64, i64, i64), tabs_right: Option<f64>, vocab: &Vocab, names: &Names, time: f64, state: &State, shown: &Shown) -> Vec<Intent> {
    let (x, y, width, height) = bounds;
    let row = ui.row_height() as f64;
    let (x, y, width, height) = (x as f64, y as f64, width as f64, height as f64);
    let theme = ui.theme();
    let emit: Emit = Rc::new(RefCell::new(Vec::new()));
    // the printed document is made here, once a frame, never inside a token scrub
    let _ = shown.applied.text();
    let dirty = dirty(state, shown);
    // the workspace sheet's narrow Lisp panel (under 400 points): "Doc", the two buttons alone in
    // the bar and no status row
    let narrow = width < 400.0;
    let tabs = tab_labels(width as i64, dirty);
    let active = tabs.iter().position(|(tab, _)| *tab == state.tab).unwrap_or(0) as i64;
    // `tabs_right` is where the header has room for them (`Chrome::header_slots`); none in a header too narrow
    if let Some(right) = tabs_right {
        let labels: Vec<String> = tabs.iter().map(|(_, l)| l.clone()).collect();
        let (hit, _) = kit::segments(ui, "text-tab", right, y - layout::HEADER_HEIGHT as f64 + 2.0, &labels, active);
        if let Some(index) = hit {
            push(&emit, Intent::Tab(tabs[index as usize].0));
        }
    }
    let bar = 32.0;
    let find_row = 28.0;
    // a hairline, the 32-point button bar, a hairline and the 24-point status row
    let footer = if narrow { bar + 1.0 } else { bar + row + 2.0 };
    let body_y = y;
    let body_h = row.max(height);
    let imports: Vec<(String, String)> = match &state.cache {
        Some(cache) => match &cache.0.workspace {
            Some(workspace) => workspace
                .imports
                .iter()
                .flat_map(|(file, forms)| {
                    forms.iter().filter_map(|form| match s::children(form).get(1).map(|c| &c.node) {
                        Some(Node::Sym(name)) => Some((name.clone(), file.clone())),
                        _ => None,
                    })
                })
                .collect(),
            None => Vec::new(),
        },
        None => Vec::new(),
    };
    let language = lisp_text::language(Some(vocab.clone()), Some(names.clone()), Some(imports), state.parinfer, theme);
    let ctx = AreaCtx { x, width, row, theme, footer, narrow, bar, find_row, language, names: names.clone(), time };
    let message_of = |errors: &[Diagnostic], dirty: bool, clean: String| -> String {
        match real_errors(errors).first() {
            Some(d) => first_line(&diagnostic::to_string(d)),
            None => {
                if dirty {
                    "Unapplied draft. Every other pane shows the last applied document.".to_owned()
                } else {
                    clean
                }
            }
        }
    };
    let text = current_text(state, shown);
    match state.tab {
        Tab::Document => {
            let errors = errors_of(state, &state.doc_errors, &text);
            let cs = caret_select(&text, state, shown);
            let message = message_of(&errors, dirty, "Source matches the applied document.".to_owned());
            editor_area(
                ui,
                &emit,
                &ctx,
                state,
                shown,
                "text-document",
                (body_y, body_h),
                &text,
                &errors,
                if dirty { Vec::new() } else { shown.mark.into_iter().collect() },
                if dirty { None } else { shown.mark.map(|m| m.0) },
                cs,
                &|t| Intent::DocApply(t),
                Intent::DocDiscard,
                dirty,
                &message,
                &|t| Intent::DocDraft(t),
                &|t, done| Intent::DocScrub(t, done),
            );
        }
        Tab::Graph => {
            let slot = graph_slot(state, &shown.graph);
            let errors = errors_of(state, &slot.map(|s| s.gerrors.clone()).unwrap_or_else(no_errors), &text);
            let dirty = text != shown.text;
            let spans: Vec<(i64, i64)> = if dirty { Vec::new() } else { shown.mark.into_iter().collect() };
            let cs = caret_select(&text, state, shown);
            let message = message_of(&errors, dirty, format!("Edit {} as text; Check and apply checks the whole workspace.", shown.graph));
            let graph = shown.graph.clone();
            let (g1, g2, g3) = (graph.clone(), graph.clone(), graph.clone());
            editor_area(
                ui,
                &emit,
                &ctx,
                state,
                shown,
                "text-graph",
                (body_y, body_h),
                &text,
                &errors,
                spans,
                if dirty { None } else { shown.mark.map(|m| m.0) },
                cs,
                &move |t| Intent::GraphApply(g1.clone(), t),
                Intent::GraphDiscard(graph),
                dirty,
                &message,
                &move |t| Intent::GraphDraft(g2.clone(), t),
                &move |t, done| Intent::GraphScrub(g3.clone(), t, done),
            );
        }
        Tab::Selection => {
            // the closure of the selection, editable: its bindings are the graph's root bindings
            let dirty = text != shown.text;
            let spans: Vec<(i64, i64)> = if dirty { Vec::new() } else { shown.mark.into_iter().collect() };
            let errors = errors_of(state, &state.binding_errors, &text);
            let cs = caret_select(&text, state, shown);
            let message = message_of(&errors, dirty, "The selection with what it reads; Check and apply writes the bindings shown.".to_owned());
            let (k1, k2, k3) = (shown.key.clone(), shown.key.clone(), shown.key.clone());
            editor_area(
                ui,
                &emit,
                &ctx,
                state,
                shown,
                "text-selection",
                (body_y, body_h),
                &text,
                &errors,
                spans,
                shown.mark.map(|m| m.0),
                cs,
                &move |t| Intent::BindingApply(k1.clone(), t),
                Intent::BindingDiscard,
                dirty,
                &message,
                &move |t| Intent::BindingDraft(k2.clone(), t),
                &move |t, done| Intent::BindingScrub(k3.clone(), t, done),
            );
        }
    }
    let out = emit.borrow().clone();
    out
}
