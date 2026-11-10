//! The text pane keeps pure tab/text state and returns intents for post-frame application. Shown shares source/text/span maps through Arcs; Printed lazily owns one document and both span views.
use flow_syntax::symbol::Symbol;
use editor_document::settings;
use editor_document::workspace_doc::{self as wd, T as WorkspaceDoc};
use flow_graph::flow_edit::{self as fe, ArgKey, Op};
use flow_syntax::lisp::{self, Spans};
use flow_syntax::syntax::{self as s, Draft, Node, Syntax, SyntaxBuilder, SyntaxId};
use rays_diagnostic::{self as diagnostic, Diagnostic, Severity, Span};
use crate::lisp_text::{self, Names, Vocab};
use pxui::theme::{self, Theme};
use pxui::ui::*;
use rays_shell::kit::{self, ButtonOpts};
use rays_shell::layout;
use pxui::geom::color;
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
pub type Path = flow_check::workspace::Path;

/// One lazily printed document owns its text and span maps.
#[derive(Clone)]
pub struct Printed {
    cell: Arc<PrintCell>,
}

struct PrintCell {
    value: OnceLock<Result<(String, Spans), Diagnostic>>,
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
    pub fn ready(text: String, spans: Spans) -> Result<Printed, Diagnostic> {
        diagnostic::check_source_len(text.len())?;
        let value = OnceLock::new();
        let _ = value.set(Ok((text, spans)));
        Ok(Printed { cell: Arc::new(PrintCell { value, source: PrintSource::Ready }) })
    }
    fn force(&self) -> Result<&(String, Spans), &Diagnostic> {
        self.cell.value.get_or_init(|| match &self.cell.source {
            PrintSource::Forms(forms) => lisp::print(forms),
            PrintSource::Doc(doc) => wd::print(doc),
            PrintSource::Ready => unreachable!("ready printer already initialized"),
        }).as_ref()
    }
    /// `Lazy.force shown.applied`
    pub fn text(&self) -> Result<&str, Diagnostic> {
        self.force().map(|(text, _)| text.as_str()).map_err(Clone::clone)
    }
    /// `Lazy.force shown.applied_spans`
    pub fn spans(&self) -> Result<&Spans, Diagnostic> {
        self.force().map(|(_, spans)| spans).map_err(Clone::clone)
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
    pub menu: Option<(f32, f32)>,
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


/// `String.sub text a len`, lossily decoded where the cut falls inside a character.
pub fn sub(text: &str, a: i64, len: i64) -> String {
    let (a, len) = (a.max(0) as usize, len.max(0) as usize);
    let bytes = text.as_bytes();
    let end = (a + len).min(bytes.len());
    String::from_utf8_lossy(&bytes[a.min(end)..end]).into_owned()
}

// ---- reading the source ----

fn root_forms(source: &[S]) -> Option<(&S, &[SyntaxId])> {
    let root = source.first()?;
    match root.node {
        Node::List(items) if root.range(items).len() >= 2 => Some((root, &root.range(items)[2..])),
        _ => None,
    }
}

fn root_name(item: &S) -> Option<String> {
    let Node::List(range) = item.node else { return None };
    let l = item.range(range);
    let (&head, &name) = (l.first()?, l.get(1)?);
    let (Node::Sym(h), Node::Sym(n)) = (item.arena().row(head).node, item.arena().row(name).node) else { return None };
    let (h, n) = (item.name(h), item.name(n));
    match h {
        "defn" => Some(format!("def:{}", n)),
        "defmacro" => Some(format!("macro:{}", n)),
        "graph" => Some(n.to_owned()),
        _ => None,
    }
}

pub fn root_form(source: &[S], name: &str) -> Option<S> {
    let (owner, roots) = root_forms(source)?;
    roots.iter().map(|&id| owner.child(id)).find(|item| root_name(item).as_deref() == Some(name))
}

fn last(f: &S) -> Option<S> {
    f.children().last().map(|&id| f.child(id))
}

/// `let* [bindings] result` of an even binding vector; IDs borrow `e`'s owner.
fn scope(e: &S) -> Option<(&[SyntaxId], SyntaxId)> {
    if let Node::List(range) = e.node {
        let l = e.range(range);
        if l.len() == 3
            && matches!(e.arena().row(l[0]).node, Node::Sym(h) if e.name(h) == "let*")
            && let Node::Vec(bs) = e.arena().row(l[1]).node
            && e.range(bs).len() % 2 == 0
        {
            return Some((e.range(bs), l[2]));
        }
    }
    None
}

/// The scope inside a binding's value: itself, or a loop's body (the flag: it stepped inside).
fn enter(v: &S) -> (S, bool) {
    if scope(v).is_some() {
        return (v.clone(), false);
    }
    if matches!(s::head(v), Some("for" | "fold" | "scan" | "sum" | "state" | "fn"))
        && let Some(l) = last(v)
    {
        return (l, true);
    }
    (v.clone(), false)
}

fn append(path: &[Symbol], leaf: &str) -> Path {
    let mut p = path.to_vec();
    p.push(leaf.into());
    p
}

type Found = Option<(Option<S>, S)>;

/// the binding a path names (pattern and expression) below a graph's body
fn find_binding(cur: &S, names: &[Symbol]) -> Found {
    let (name, rest) = names.split_first()?;
    let (name, keys) = fe::leaf_keys(name).unwrap_or((name.to_string(), Vec::new()));
    let found: Found = match scope(cur) {
        Some((_, res)) if name == "@result" => Some((None, cur.child(res))),
        Some((ps, _)) => ps.chunks_exact(2).find(|p| fe::pat_key(&cur.child(p[0])) == name).map(|p| (Some(cur.child(p[0])), cur.child(p[1]))),
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
        Some((_, v)) => find_binding(&enter(&v).0, rest),
        None => None,
    }
}

/// The binding a path names in the source: its pattern (none for a `@result`) and expression.
pub fn binding(source: &[S], path: &[Symbol]) -> Found {
    let (graph, names) = path.split_first()?;
    let root = root_form(source, graph)?;
    let body = last(&root)?;
    find_binding(&body, names)
}

fn named(forms: &[S]) -> Option<Vec<(String, S)>> {
    let mut out = Vec::new();
    let mut rest = forms;
    loop {
        match rest {
            [] => return Some(out),
            [a, v, tail @ ..] if matches!(a.node, Node::Sym(_)) => {
                if let Node::Sym(n) = &a.node {
                    out.push((a.name(*n).to_owned(), v.clone()));
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
pub fn paste_ops(source: &[S], graph: &str, scope: &[Symbol], text: &str) -> Result<Vec<Op>, String> {
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
    let mut ctor = SyntaxBuilder::new();
    for form in &forms {
        ctor.import(form);
    }
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
    let mut ops = Vec::new();
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
        let mut expr = ctor.imported(&expr).unwrap();
        for (o, hold, _) in &held {
            expr = flow_graph::edit_tree::rename_ref(o, hold, &expr, &mut ctor);
        }
        for (_, hold, n) in &held {
            expr = flow_graph::edit_tree::rename_ref(hold, n, &expr, &mut ctor);
        }
        earlier.insert(0, (old, name.clone()));
        ops.push((name, expr));
    }
    let arena = ctor.freeze();
    Ok(ops.into_iter().map(|(name, expr)| Op::AddNode { scope: scope.to_vec(), name, expr: arena.handle(expr) }).collect())
}

fn max_id(f: &S) -> i64 {
    fn visit(arena: &s::SyntaxArena, id: SyntaxId) -> i64 {
        arena.children(id).iter().fold(arena.row(id).id, |m, &child| m.max(visit(arena, child)))
    }
    visit(f.arena(), f.row_id())
}

fn span_of<'a>(spans: &'a Spans, f: &S) -> Option<&'a Span> {
    spans.iter().find(|(id, _)| *id == f.id).map(|(_, sp)| sp)
}

/// the marked byte range of a binding: its name through its expression
fn mark(spans: &Spans, found: &Found) -> Option<(i64, i64)> {
    let (p, v) = found.as_ref()?;
    let b = span_of(spans, v)?;
    match p.as_ref().and_then(|p| span_of(spans, p)) {
        Some(a) => Some((i64::from(a.start), i64::from(b.finish))),
        None => Some(span_range(b)),
    }
}

fn graph_at(shown: &Shown, byte: i64) -> Option<(String, S)> {
    let (owner, roots) = root_forms(&shown.source)?;
    for &id in roots {
        let root = owner.child(id);
        if let Some(name) = root_name(&root)
            && let Some(span) = span_of(&shown.spans, &root)
            && i64::from(span.start) <= byte
            && byte < i64::from(span.finish)
        {
            return Some((name, root));
        }
    }
    None
}

fn is_list_or_map(f: &S) -> bool {
    matches!(f.node, Node::List(_) | Node::Map(_))
}

fn holds(spans: &Spans, byte: i64, f: &S) -> bool {
    match span_of(spans, f) {
        Some(sp) => i64::from(sp.start) <= byte && byte <= i64::from(sp.finish),
        None => false,
    }
}

fn nested_at(spans: &Spans, byte: i64, prefix: &[Symbol], leaf: &str, v: &S) -> Path {
    let (body, entered) = enter(v);
    let inner = if entered || scope(v).is_some() { within_at(spans, byte, &append(prefix, leaf), &body) } else { None };
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

fn within_at(spans: &Spans, byte: i64, prefix: &[Symbol], cur: &S) -> Option<Path> {
    match scope(cur) {
        None => {
            if is_list_or_map(cur) && holds(spans, byte, cur) { Some(nested_at(spans, byte, prefix, "@result", cur)) } else { None }
        }
        Some((bindings, res)) => {
            for pair in bindings.chunks_exact(2) {
                let (p, v) = (cur.child(pair[0]), cur.child(pair[1]));
                if let (Some(a), Some(b)) = (span_of(spans, &p), span_of(spans, &v))
                    && i64::from(a.start) <= byte
                    && byte <= i64::from(b.finish)
                {
                    return Some(nested_at(spans, byte, prefix, &fe::pat_key(&p), &v));
                }
            }
            let res = cur.child(res);
            if holds(spans, byte, &res) && is_list_or_map(&res) { Some(nested_at(spans, byte, prefix, "@result", &res)) } else { None }
        }
    }
}

/// The innermost binding or inline node at a byte in any tab.
pub fn binding_at(shown: &Shown, byte: i64) -> Option<Path> {
    let (graph, body, spans): (String, S, Arc<Spans>) = match &shown.body {
        Some((body, spans)) => (shown.graph.clone(), (**body).clone(), spans.clone()),
        None => match graph_at(shown, byte) {
            Some((graph, root)) => match last(&root) {
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
        let mut p: Path = vec![graph.into()];
        p.extend(names.into_iter().map(Symbol::from));
        p
    })
}

fn parameter(p: &S) -> Vec<String> {
    match p.node {
        Node::List(range) if !p.range(range).is_empty() => fe::pat_names(&p.child(p.range(range)[0])),
        _ => fe::pat_names(p),
    }
}

fn rail_names(expr: &S) -> Vec<String> {
    let children = expr.children();
    match s::head(expr) {
        Some("fn") => match children.get(1).map(|&id| expr.arena().row(id).node) {
            Some(Node::Vec(ps)) => expr.range(ps).iter().flat_map(|&id| parameter(&expr.child(id))).collect(),
            _ => Vec::new(),
        },
        Some("for" | "sum" | "fold" | "scan" | "state") => {
            let args = &children[1.min(children.len())..];
            let take = args.len().saturating_sub(1);
            args[..take]
                .iter()
                .flat_map(|&id| match expr.arena().row(id).node {
                    Node::Vec(ps) => expr.range(ps).chunks_exact(2).flat_map(|p| fe::pat_names(&expr.child(p[0]))).collect::<Vec<_>>(),
                    _ => Vec::new(),
                })
                .collect()
        }
        _ => Vec::new(),
    }
}

fn input_of(source: &[S], name: &str, prefix: &[Symbol]) -> Option<Path> {
    if let [graph] = prefix {
        let root = root_form(source, graph)?;
        let params = root.children().iter().skip(2).find_map(|&id| match root.arena().row(id).node {
            Node::Vec(ps) => Some(root.range(ps)),
            _ => None,
        })?;
        if params.iter().any(|&id| parameter(&root.child(id)).iter().any(|n| n == name)) {
            return Some(vec![*graph, format!(":{}", name).into()]);
        }
    }
    None
}

fn local_ref(source: &[S], name: &str, prefix: &[Symbol], leaf: &str) -> Option<Path> {
    let expr: Option<S> = match prefix {
        [graph] => root_form(source, graph).and_then(|root| last(&root)),
        _ => binding(source, prefix).map(|(_, v)| v),
    };
    let found = expr.and_then(|expr| {
        let body = enter(&expr).0;
        let bindings = scope(&body).map(|(ps, _)| ps).unwrap_or_default();
        let holder = fe::leaf_keys(leaf).map(|(h, _)| h).unwrap_or(leaf.to_owned());
        let bindings = if bindings.chunks_exact(2).any(|p| fe::pat_key(&body.child(p[0])) == holder) {
            let at = bindings.chunks_exact(2).position(|p| fe::pat_key(&body.child(p[0])) == holder).unwrap();
            let end = at * 2 + if s::head(&body.child(bindings[at * 2 + 1])) == Some("fn") { 2 } else { 0 };
            &bindings[..end]
        } else {
            bindings
        };
        if let Some(p) = bindings.chunks_exact(2).rev().find(|p| fe::pat_names(&body.child(p[0])).iter().any(|n| n == name)) {
            return Some(append(prefix, &fe::pat_key(&body.child(p[0]))));
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
pub fn reference_target(source: &[S], from: &[Symbol], name: &str) -> Option<Path> {
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
        .map(|key| vec![key.into()])
}

/// The definition of the symbol at a printed byte, including call heads.
pub fn definition_at(shown: &Shown, byte: i64) -> Option<Path> {
    fn symbol(spans: &Spans, byte: i64, arena: &s::SyntaxArena, id: SyntaxId) -> Option<String> {
        let row = arena.row(id);
        if let Node::Sym(name) = row.node
            && let Some((_, span)) = spans.iter().find(|(id, _)| *id == row.id)
            && i64::from(span.start) <= byte
            && byte < i64::from(span.finish)
        {
            return Some(arena.name(name).to_owned());
        }
        arena.children(id).iter().find_map(|&child| symbol(spans, byte, arena, child))
    }
    let name = shown.source.iter().find_map(|f| symbol(&shown.spans, byte, f.arena(), f.row_id()))?;
    let from = binding_at(shown, byte).unwrap_or_else(|| vec![shown.graph.clone().into()]);
    if let Some((Some(p), _)) = binding(&shown.source, &from)
        && let Some(span) = span_of(&shown.spans, &p)
        && i64::from(span.start) <= byte
        && byte < i64::from(span.finish)
    {
        return Some(from);
    }
    reference_target(&shown.source, &from, &name)
}

fn span_range(span: &Span) -> (i64, i64) {
    (i64::from(span.start), i64::from(span.finish))
}

pub fn path_mark(source: &[S], spans: &Spans, path: &[Symbol]) -> Option<(i64, i64)> {
    match path {
        [graph] => root_form(source, graph).and_then(|root| span_of(spans, &root)).map(span_range),
        [graph, input] if input.starts_with(':') => {
            let name = &input[1..];
            let root = root_form(source, graph)?;
            for &id in root.children().iter().skip(2) {
                if let Node::Vec(ps) = root.arena().row(id).node {
                    for &id in root.range(ps) {
                        let p = root.child(id);
                        let pattern = match p.node {
                            Node::List(l) if !p.range(l).is_empty() => p.child(p.range(l)[0]),
                            _ => p.clone(),
                        };
                        if fe::pat_names(&pattern).iter().any(|n| n == name) {
                            return span_of(spans, &p).map(span_range);
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
    let Some(body) = last(&root) else { return Vec::new() };
    let Some((bindings, _)) = scope(&body) else { return Vec::new() };
    bindings
        .chunks_exact(2)
        .filter_map(|p| match (s::head(&body.child(p[1])), body.arena().row(p[0]).node) {
            (Some("scene/camera"), Node::Sym(n)) => Some(body.name(n).to_owned()),
            _ => None,
        })
        .collect()
}

/// the top-level ancestor with what it reads, as a `let*` over the root bindings it needs; None
/// when the graph body is not a scope
fn closure(root: &S, top: &str) -> Option<S> {
    let body = last(root)?;
    let (bindings, _) = scope(&body)?;
    let mut declares: Vec<(String, usize)> = Vec::new();
    for (i, pair) in bindings.chunks_exact(2).enumerate() {
        for n in fe::pat_names(&body.child(pair[0])) {
            declares.push((n, i));
        }
    }
    fn need(owner: &S, bindings: &[SyntaxId], declares: &[(String, usize)], acc: &mut Vec<usize>, name: &str) {
        if let Some((_, i)) = declares.iter().find(|(n, _)| n == name)
            && !acc.contains(i)
        {
            acc.push(*i);
            for n in fe::free_names(&owner.child(bindings[*i * 2 + 1])) {
                need(owner, bindings, declares, acc, &n);
            }
        }
    }
    let mut needed = Vec::new();
    need(&body, bindings, &declares, &mut needed, top);
    if needed.is_empty() {
        return None;
    }
    let inputs: Vec<String> = match root.children().iter().find_map(|&id| match root.arena().row(id).node {
        Node::Vec(inputs) => Some(root.range(inputs)),
        _ => None,
    }) {
        None => Vec::new(),
        Some(inputs) => inputs
            .iter()
            .filter_map(|&id| match root.arena().row(id).node {
                Node::List(l) if !root.range(l).is_empty() => match root.arena().row(root.range(l)[0]).node {
                    Node::Sym(n) if needed.iter().any(|&i| fe::free_names(&body.child(bindings[i * 2 + 1])).iter().any(|name| name == root.name(n))) => Some(root.name(n).to_owned()),
                    _ => None,
                },
                _ => None,
            })
            .collect(),
    };
    let base = max_id(root) + 1;
    let note = format!(
        "{} \u{b7} {}{}",
        top,
        if needed.len() > 1 { format!("with {}", crate::navigator::plural((needed.len() - 1) as i64, "upstream binding")) } else { "no upstream bindings".to_owned() },
        if inputs.is_empty() { String::new() } else { format!(" and inputs {}", inputs.join(", ")) }
    );
    let mut ctor = SyntaxBuilder::new();
    ctor.import(root);
    let vec_items: Vec<SyntaxId> = bindings.chunks_exact(2).enumerate()
        .filter(|(i, _)| needed.contains(i))
        .flat_map(|(_, pair)| pair.iter().map(|&id| ctor.imported(&body.child(id)).unwrap()))
        .collect();
    let head = ctor.push_draft(base + 1, Draft::Sym("let*".to_owned()), Span::default(), None);
    let bindings = ctor.push_draft(base + 2, Draft::Vec(vec_items), Span::default(), None);
    let result = ctor.push_draft(base + 3, Draft::Sym(top.to_owned()), Span::default(), None);
    let out = ctor.push_draft(base, Draft::List(vec![head, bindings, result]), Span::default(), Some(s::Trivia {
        notes: vec![note, "Selection patches named bindings; omitted bindings stay. Change the result in Graph or Document.".to_owned()],
        ..s::Trivia::default()
    }));
    Some(ctor.freeze().handle(out))
}

fn e_edit(message: String) -> Diagnostic {
    diagnostic::error("E_EDIT".to_owned(), message, None, None, None, Vec::new(), Vec::new())
}

fn printed_of(f: &S) -> String {
    lisp::text(std::slice::from_ref(f))
}

/// Build the complete candidate graph before checking: dependent bindings may change type
/// together, so checking individual replacements is incorrect.
fn selection_form(source: &[S], path: &[Symbol], form: &S) -> Result<S, Diagnostic> {
    let shape = || e_edit("Expected the shown (let* [...] name) closure, or the graph form.".to_owned());
    let (graph, top) = match path {
        [graph, top, ..] => (graph, top),
        _ => return Err(shape()),
    };
    let Some((patches, result)) = scope(form) else { return Err(shape()) };
    let names: Vec<&str> = patches
        .chunks_exact(2)
        .filter_map(|p| match form.arena().row(p[0]).node {
            Node::Sym(n) => Some(form.name(n)),
            _ => None,
        })
        .collect();
    let invalid = patches.chunks_exact(2).find(|p| match form.arena().row(p[0]).node {
        Node::Sym(n) => binding(source, &[graph.clone().into(), form.name(n).into()]).is_none(),
        _ => true,
    });
    if let Some(p) = invalid {
        return Err(e_edit(format!(
            "{} is not a binding of {}; add or remove bindings in the Graph tab.",
            printed_of(&form.child(p[0])).trim(),
            graph
        )));
    }
    let mut sorted = names.clone();
    sorted.sort();
    sorted.dedup();
    if names.len() != sorted.len() {
        return Err(e_edit("Duplicate binding names in Selection.".to_owned()));
    }
    if printed_of(&form.child(result)) != format!("{}\n", top) {
        return Err(e_edit("Selection's result must stay unchanged; edit the result in Graph or Document.".to_owned()));
    }
    let Some(root) = root_form(source, graph) else { return Err(e_edit("Selection graph no longer exists.".to_owned())) };
    let Some(body) = last(&root) else { return Err(e_edit("Selection graph no longer exists.".to_owned())) };
    let Some((bindings, res)) = scope(&body) else { return Err(e_edit("Expected the shown Selection closure.".to_owned())) };
    // A closure that changes nothing is the graph as it is, not a refusal: a scrub applies on
    // every frame of its drag, also the ones that did not move.
    let mut ctor = SyntaxBuilder::new();
    let imported_root = ctor.import(&root);
    ctor.import(form);
    let mut rewritten = Vec::with_capacity(bindings.len());
    for pair in bindings.chunks_exact(2) {
        let (p, old) = (body.child(pair[0]), body.child(pair[1]));
        let mut value = ctor.imported(&old).unwrap();
        if let Some(patch) = patches.chunks_exact(2).find(|q| s::equal_node(&form.child(q[0]), &p)) {
            let v = form.child(patch[1]);
            if printed_of(&old) != printed_of(&v) {
                value = ctor.imported(&v).unwrap();
                if v.notes().is_empty() && !old.notes().is_empty() {
                    let mut trivia = v.trivia().cloned().unwrap_or_default();
                    trivia.notes = old.notes().to_vec();
                    value = ctor.with_trivia(value, Some(trivia));
                }
            }
        }
        rewritten.extend([ctor.imported(&p).unwrap(), value]);
    }
    let kids = body.children();
    let vec_form = ctor.with_node(&body.child(kids[1]), Draft::Vec(rewritten));
    let head = ctor.imported(&body.child(kids[0])).unwrap();
    let result = ctor.imported(&body.child(res)).unwrap();
    let new_body = ctor.with_node(&body, Draft::List(vec![head, vec_form, result]));
    let mut root_kids = ctor.children(imported_root).to_vec();
    *root_kids.last_mut().unwrap() = new_body;
    let out = ctor.with_node(&root, Draft::List(root_kids));
    Ok(ctor.freeze().handle(out))
}

/// The `Set_graph` of the graph form a Graph tab holds, or (with `selection`, the shown binding's
/// path) of the Selection closure patched into the graph; the error of text that is neither.
pub fn graph_op(source: &[S], graph: &str, selection: Option<&[Symbol]>, text: &str) -> Result<Op, Diagnostic> {
    let set = |form: S| Op::SetGraph { name: graph.to_owned(), form };
    let forms = s::parse(text)?;
    match forms.as_slice() {
        [form] if matches!(form.node, Node::List(_)) && matches!(s::head(form), Some("graph" | "defn")) => {
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

fn make_shown_with(workspace: Option<&WorkspaceDoc>, source: &[S], graph: &str, selected: Option<&Path>, tab: Tab) -> Result<Shown, Diagnostic> {
    let source_arc = Arc::new(source.to_vec());
    let printed = match workspace {
        None => Printed::of_forms(source_arc.clone()),
        Some(w) => Printed::of_workspace(w),
    };
    let key: Path = match selected {
        Some(path) => path.clone(),
        None => vec![graph.into()],
    };
    Ok(match tab {
        Tab::Document => {
            let spans = printed.spans()?.clone();
            Shown {
                graph: graph.to_owned(),
                text: printed.text()?.to_owned(),
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
                let names: &[Symbol] = match selected {
                    Some(p) if !p.is_empty() => &p[1..],
                    _ => &[],
                };
                let (form, is_root): (S, bool) = match (tab, names.first()) {
                    (Tab::Selection, Some(top)) => match closure(&root, top) {
                        Some(f) => (f, false),
                        None => (root.clone(), true),
                    },
                    _ => (root.clone(), true),
                };
                let (text, spans) = lisp::print(std::slice::from_ref(&form))?;
                let body = if is_root { last(&root).unwrap_or(root) } else { form };
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
    })
}

/// `make_shown source graph selected tab`: Selection prints the selected binding's top-level
/// ancestor as a `let*` over the root bindings it reads (a note names the count and the graph
/// inputs used) with the binding marked; Graph the whole graph form; Document the workspace form.
pub fn make_shown(source: &[S], graph: &str, selected: Option<&Path>, tab: Tab) -> Result<Shown, Diagnostic> {
    make_shown_with(None, source, graph, selected, tab)
}

fn atoms(old: &S, next: &S) -> Option<Vec<S>> {
    if s::equal(old, next) {
        return Some(Vec::new());
    }
    if old.id != next.id || old.notes() != next.notes() || old.meta() != next.meta() || old.tail() != next.tail() {
        return None;
    }
    match (&old.node, &next.node) {
        (Node::Num(_), Node::Num(_)) | (Node::Str(_), Node::Str(_)) | (Node::Sym(_), Node::Sym(_)) => Some(vec![next.clone()]),
        (Node::Vec(o), Node::Vec(n)) if old.range(*o).len() == next.range(*n).len() => {
            let mut values: Vec<S> = Vec::new();
            for (&o, &n) in old.range(*o).iter().zip(next.range(*n)) {
                let mut more = atoms(&old.child(o), &next.child(n))?;
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

fn patched_shown(previous: &WorkspaceDoc, workspace: &WorkspaceDoc, shown: &Shown) -> Result<Option<Shown>, Diagnostic> {
    if previous.layout != workspace.layout || !settings::same_values(&previous.settings, &workspace.settings) || previous.extra != workspace.extra {
        return Ok(None);
    }
    let Some(changes) = wd::literal_changes(previous, workspace) else { return Ok(None) };
    let mut values: Vec<S> = Vec::new();
    for change in &changes {
        let Some(old) = fe::arg_text(&previous.source, &change.path, &ArgKey::Kw(change.field.clone())) else { return Ok(None) };
        let Some(mut more) = atoms(&old, &change.expr) else { return Ok(None) };
        more.reverse();
        more.extend(values);
        values = more;
    }
    // Another graph may have changed; only forms printed in this tab are patched.
    let present = |spans: &Spans| -> Vec<S> {
        let ids: std::collections::HashSet<i64> = spans.iter().map(|(id, _)| *id).collect();
        values.iter().filter(|f| ids.contains(&f.id)).cloned().collect()
    };
    let Some((text, spans)) = lisp::patch_atoms(&(shown.text.clone(), (*shown.spans).clone()), &present(&shown.spans)) else { return Ok(None) };
    let old_applied_spans = shown.applied_spans.spans()?.clone();
    let Some((applied, applied_spans)) =
        lisp::patch_atoms(&(shown.applied.text()?.to_owned(), old_applied_spans.clone()), &present(&old_applied_spans)) else { return Ok(None) };
    let body = root_form(&workspace.source, &shown.graph).and_then(|root| last(&root));
    let new_body = match (&shown.body, body) {
        (Some(_), Some(body)) => Some((Arc::new(body), Arc::new(spans.clone()))),
        _ => None,
    };
    // Selection marks are form boundaries in the repaired span map.
    let boundary = |old: i64| -> Option<i64> {
        shown.spans.iter().find_map(|(id, span)| {
            if i64::from(span.start) == old {
                spans.iter().find(|(i, _)| i == id).map(|(_, s)| i64::from(s.start))
            } else if i64::from(span.finish) == old {
                spans.iter().find(|(i, _)| i == id).map(|(_, s)| i64::from(s.finish))
            } else {
                None
            }
        })
    };
    let mark = shown.mark.map(|(a, b)| (boundary(a).unwrap_or(a), boundary(b).unwrap_or(b)));
    let printed = Printed::ready(applied, applied_spans)?;
    Ok(Some(Shown {
        source: Arc::new(workspace.source.clone()),
        text,
        applied: printed.clone(),
        applied_spans: printed,
        body: new_body,
        mark,
        spans: Arc::new(spans),
        ..shown.clone()
    }))
}

/// Cache shown text by source, workspace, graph, selection and tab. ponytail: source equality may walk O(source); retain an identity owner end-to-end if profiling justifies it.
pub fn shown(workspace: Option<&WorkspaceDoc>, state: &State, source: &[S], graph: &str, selected: Option<&Path>) -> Result<(State, Shown), Diagnostic> {
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
            return Ok((state.clone(), shown.clone()));
        }
    }
    let patched = match (&state.cache, workspace) {
        (Some(cache), Some(workspace)) => {
            let (key, shown) = &**cache;
            match &key.workspace {
                Some(previous) if state.scrubbing && key.graph == graph && key.selected == selected && key.tab == state.tab => {
                    patched_shown(previous, workspace, shown)?
                }
                _ => None,
            }
        }
        _ => None,
    };
    let shown = match patched {
        Some(shown) => shown,
        None => make_shown_with(workspace, source, graph, selected.as_ref(), state.tab)?,
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
    Ok((next, shown))
}

// The printer's token ID names the source form. Prefer the innermost card, then address its
// argument's child; no text is parsed or searched.
const LOOP_HEADS: [&str; 6] = ["for", "fold", "scan", "sum", "state", "fn"];

fn scrub_sub(id: i64, path: &mut Vec<i64>, form: &S) -> Option<Vec<i64>> {
    fn visit(id: i64, path: &mut Vec<i64>, arena: &s::SyntaxArena, row: SyntaxId) -> Option<Vec<i64>> {
        let form = arena.row(row);
        if form.id == id {
            return matches!(form.node, Node::Num(_)).then(|| path.clone());
        }
        for (i, &child) in arena.children(row).iter().enumerate() {
            path.push(i as i64);
            let found = visit(id, path, arena, child);
            path.pop();
            if found.is_some() {
                return found;
            }
        }
        None
    }
    visit(id, path, form.arena(), form.row_id())
}

fn scrub_within(id: i64, value: &S, path: &[Symbol], form: &S) -> Option<Op> {
    match scope(form) {
        Some((bindings, result)) => {
            for pair in bindings.chunks_exact(2) {
                if let found @ Some(_) = scrub_card(id, value, &append(path, &fe::pat_key(&form.child(pair[0]))), &form.child(pair[1])) {
                    return found;
                }
            }
            scrub_card(id, value, &append(path, "@result"), &form.child(result))
        }
        None => scrub_card(id, value, &append(path, "@result"), form),
    }
}

fn scrub_card(id: i64, value: &S, path: &[Symbol], form: &S) -> Option<Op> {
    if scope(form).is_some() || LOOP_HEADS.contains(&s::head(form).unwrap_or("")) {
        let (inner, _) = enter(form);
        if let found @ Some(_) = scrub_within(id, value, path, &inner) {
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
    let args: Vec<(ArgKey, SyntaxId)> = match form.node {
        Node::List(l) if !form.range(l).is_empty() => {
            let mut out = Vec::new();
            let mut position = 0;
            let mut rest = &form.range(l)[1..];
            while !rest.is_empty() {
                if let (Node::Kw(name), Some(&expr)) = (form.arena().row(rest[0]).node, rest.get(1)) {
                    out.push((ArgKey::Kw(form.name(name).to_owned()), expr));
                    rest = &rest[2..];
                } else {
                    out.push((ArgKey::Pos(position), rest[0]));
                    position += 1;
                    rest = &rest[1..];
                }
            }
            out
        }
        Node::Map(fields) => form.range(fields).chunks_exact(2)
            .map(|p| (ArgKey::Field(if let Node::Kw(name) = form.arena().row(p[0]).node { form.name(name).to_owned() } else { String::new() }), p[1]))
            .collect(),
        _ => vec![(ArgKey::Whole, form.row_id())],
    };
    for (key, expr) in args {
        let expr = form.child(expr);
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
    let id = shown.spans.iter().find(|(id, span)| *id != 0 && i64::from(span.start) == start && i64::from(span.finish) == finish).map(|(id, _)| *id)?;
    if !s::number(replacement) {
        return None;
    }
    let value = s::make(Draft::Num(replacement.to_owned()));
    let (owner, roots) = root_forms(&shown.source)?;
    for &root in roots {
        let root = owner.child(root);
        if matches!(s::head(&root), Some("graph" | "defn"))
            && let Some(prefix) = root_name(&root)
        {
            if let Some(body) = last(&root)
                && let found @ Some(_) = scrub_within(id, &value, &[prefix.into()], &body)
            {
                return found;
            }
        }
    }
    None
}

// ---- a draft against a document that changed under it ----

type Forms<'a> = (&'a S, &'a [SyntaxId]);

struct Entries<'a> {
    owner: &'a S,
    rows: Vec<(String, &'a [SyntaxId])>,
}

fn same(a: &S, b: &S) -> bool {
    printed_of(a) == printed_of(b)
}

fn sames((a, ai): Forms<'_>, (b, bi): Forms<'_>) -> bool {
    ai.len() == bi.len() && ai.iter().zip(bi).all(|(&x, &y)| same(&a.child(x), &b.child(y)))
}

fn entry_key(f: &S) -> String {
    if let Node::List(l) = f.node {
        let l = f.range(l);
        match (l.first().map(|&id| f.arena().row(id).node), l.get(1).map(|&id| f.arena().row(id).node)) {
            (Some(Node::Sym(h)), Some(Node::Sym(n))) => return format!("{} {}", f.name(h), f.name(n)),
            (Some(Node::Sym(h)), _) => return f.name(h).to_owned(),
            _ => {}
        }
    }
    printed_of(f)
}

fn entries((owner, forms): Forms<'_>) -> Entries<'_> {
    Entries { owner, rows: forms.iter().map(|id| (entry_key(&owner.child(*id)), std::slice::from_ref(id))).collect() }
}

fn pair_entries((owner, forms): Forms<'_>) -> Option<Entries<'_>> {
    if forms.len() % 2 != 0 {
        return None;
    }
    Some(Entries { owner, rows: forms.chunks_exact(2).map(|p| (printed_of(&owner.child(p[0])), p)).collect() })
}

fn merge_seq(ctor: &mut SyntaxBuilder, (b, bl): Forms<'_>, (m, ml): Forms<'_>, (t, tl): Forms<'_>) -> Option<Vec<SyntaxId>> {
    if bl.len() == ml.len() && ml.len() == tl.len() {
        (0..ml.len()).map(|i| merge_forms(ctor, &b.child(bl[i]), &m.child(ml[i]), &t.child(tl[i]))).collect()
    } else {
        None
    }
}

fn merge_keyed(ctor: &mut SyntaxBuilder, bl: &Entries<'_>, ml: &Entries<'_>, tl: &Entries<'_>) -> Option<Vec<SyntaxId>> {
    let unique = |l: &Entries<'_>| {
        let mut keys: Vec<&String> = l.rows.iter().map(|(k, _)| k).collect();
        let n = keys.len();
        keys.sort();
        keys.dedup();
        keys.len() == n
    };
    if !(unique(bl) && unique(ml) && unique(tl)) {
        return None;
    }
    fn find<'a>(l: &Entries<'a>, k: &str) -> Option<Forms<'a>> {
        l.rows.iter().find(|(x, _)| x == k).map(|(_, v)| (l.owner, *v))
    }
    fn copied(ctor: &SyntaxBuilder, (owner, rows): Forms<'_>) -> Vec<SyntaxId> {
        rows.iter().map(|&id| ctor.imported(&owner.child(id)).unwrap()).collect()
    }
    let mut out = Vec::new();
    for (k, m) in &ml.rows {
        let m = (ml.owner, *m);
        match (find(bl, k), find(tl, k)) {
            (Some(b), Some(t)) => out.extend(merge_seq(ctor, b, m, t)?),
            (Some(b), None) => {
                if !sames(m, b) {
                    return None;
                }
            }
            (None, Some(t)) => {
                if !sames(m, t) {
                    return None;
                }
                out.extend(copied(ctor, m));
            }
            (None, None) => out.extend(copied(ctor, m)),
        }
    }
    for (k, t) in &tl.rows {
        match (ml.rows.iter().any(|(x, _)| x == k), find(bl, k)) {
            (true, _) => {}
            (false, None) => out.extend(copied(ctor, (tl.owner, *t))),
            (false, Some(b)) => {
                if !sames((tl.owner, *t), b) {
                    return None;
                }
            }
        }
    }
    Some(out)
}

// A call whose argument count differs: what stands before the first keyword pairs by position,
// the keyword arguments by name.
fn merge_call(ctor: &mut SyntaxBuilder, b: Forms<'_>, m: Forms<'_>, t: Forms<'_>) -> Option<Vec<SyntaxId>> {
    fn split((owner, forms): Forms<'_>) -> Option<(Forms<'_>, Entries<'_>)> {
        let at = forms.iter().position(|&id| matches!(owner.arena().row(id).node, Node::Kw(_))).unwrap_or(forms.len());
        let (before, rest) = forms.split_at(at);
        if rest.len() % 2 != 0 || rest.chunks_exact(2).any(|p| !matches!(owner.arena().row(p[0]).node, Node::Kw(_))) {
            return None;
        }
        Some(((owner, before), pair_entries((owner, rest))?))
    }
    let (bp, bk) = split(b)?;
    let (mp, mk) = split(m)?;
    let (tp, tk) = split(t)?;
    let mut positional = merge_seq(ctor, bp, mp, tp)?;
    positional.extend(merge_keyed(ctor, &bk, &mk, &tk)?);
    Some(positional)
}

fn merge_forms(ctor: &mut SyntaxBuilder, b: &S, m: &S, t: &S) -> Option<SyntaxId> {
    if same(m, b) {
        return ctor.imported(t);
    }
    if same(t, b) || same(m, t) {
        return ctor.imported(m);
    }
    let (bl, ml, tl) = (b.children(), m.children(), t.children());
    let draft = match (b.node, m.node, t.node) {
        (Node::List(_), Node::List(_), Node::List(_))
            if bl.len() >= 2
                && matches!(b.arena().row(bl[0]).node, Node::Sym(h) if b.name(h) == "let*")
                && matches!(b.arena().row(bl[1]).node, Node::Vec(_))
                && ml.len() >= 2
                && matches!(m.arena().row(ml[1]).node, Node::Vec(_))
                && tl.len() >= 2
                && matches!(t.arena().row(tl[1]).node, Node::Vec(_)) =>
        {
            let (bv, mv, tv) = (b.child(bl[1]), m.child(ml[1]), t.child(tl[1]));
            let bindings = merge_keyed(ctor, &pair_entries((&bv, bv.children()))?, &pair_entries((&mv, mv.children()))?, &pair_entries((&tv, tv.children()))?)?;
            let rest = merge_seq(ctor, (b, &bl[2..]), (m, &ml[2..]), (t, &tl[2..]))?;
            let head = ctor.imported(&m.child(ml[0])).unwrap();
            let bindings = ctor.with_node(&mv, Draft::Vec(bindings));
            let mut kids = vec![head, bindings];
            kids.extend(rest);
            Draft::List(kids)
        }
        (Node::List(_), Node::List(_), Node::List(_)) if s::head(b) == Some("workspace") => {
            Draft::List(merge_keyed(ctor, &entries((b, bl)), &entries((m, ml)), &entries((t, tl)))?)
        }
        (Node::List(_), Node::List(_), Node::List(_)) => {
            Draft::List(merge_seq(ctor, (b, bl), (m, ml), (t, tl)).or_else(|| merge_call(ctor, (b, bl), (m, ml), (t, tl)))?)
        }
        (Node::Vec(_), Node::Vec(_), Node::Vec(_)) => Draft::Vec(merge_seq(ctor, (b, bl), (m, ml), (t, tl))?),
        (Node::Map(_), Node::Map(_), Node::Map(_)) => Draft::Map(merge_seq(ctor, (b, bl), (m, ml), (t, tl))?),
        _ => return None,
    };
    Some(ctor.with_node(m, draft))
}

/// A draft (`mine`) begun from `base`, against the text the document has now (`theirs`): the
/// three merged on the syntax tree, graphs and `let*` bindings paired by name. `None` when both
/// changed the same value or the draft does not read.
/// ponytail: a binding or keyword argument `theirs` added goes last in its form; keep `theirs`'
/// place for it when the order of bindings bites (the merged text is still checked).
pub fn merge3(base: &str, mine: &str, theirs: &str) -> Option<String> {
    let (b, m, t) = (s::parse(base).ok()?, s::parse(mine).ok()?, s::parse(theirs).ok()?);
    let Some(fallback) = b.first().or(m.first()).or(t.first()) else { return Some("\n".to_owned()) };
    let mut ctor = SyntaxBuilder::new();
    for form in b.iter().chain(&m).chain(&t) {
        ctor.import(form);
    }
    let (bi, mi, ti): (Vec<_>, Vec<_>, Vec<_>) = (b.iter().map(S::row_id).collect(), m.iter().map(S::row_id).collect(), t.iter().map(S::row_id).collect());
    let forms = merge_keyed(&mut ctor, &entries((b.first().unwrap_or(fallback), &bi)), &entries((m.first().unwrap_or(fallback), &mi)), &entries((t.first().unwrap_or(fallback), &ti)))?;
    let arena = ctor.freeze();
    let forms: Vec<S> = forms.into_iter().map(|id| arena.handle(id)).collect();
    Some(lisp::text(&forms))
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
    fn walk(acc: &mut Vec<(String, String, i64)>, arena: &s::SyntaxArena, id: SyntaxId) {
        if let Node::List(l) = arena.row(id).node {
            let l = arena.range(l);
            if l.len() >= 2
                && let (Node::Sym(kind), Node::Sym(name)) = (arena.row(l[0]).node, arena.row(l[1]).node)
                && matches!(arena.name(kind), "graph" | "defn" | "defmacro")
            {
                acc.push((arena.name(name).to_owned(), arena.name(kind).to_owned(), i64::from(arena.row(l[1]).span.start)));
            } else if l.len() == 3
                && matches!(arena.row(l[0]).node, Node::Sym(h) if arena.name(h) == "let*")
                && let Node::Vec(bs) = arena.row(l[1]).node
            {
                for pair in arena.range(bs).chunks_exact(2) {
                    let p = arena.row(pair[0]);
                    if let Node::Sym(name) = p.node {
                        acc.push((arena.name(name).to_owned(), "let*".to_owned(), i64::from(p.span.start)));
                    }
                }
            }
        }
        for &child in arena.children(id) {
            walk(acc, arena, child);
        }
    }
    let Ok(forms) = s::parse(text) else { return Vec::new() };
    let mut acc = Vec::new();
    for f in &forms {
        walk(&mut acc, f.arena(), f.row_id());
    }
    acc
}

/// the byte where 1-based `line` of `text` starts (the last line's for a line past the end)
fn line_offset(text: &str, line: i64) -> usize {
    text.match_indices('\n').map(|(at, _)| at + 1).take(line.saturating_sub(1).max(0) as usize).last().unwrap_or(0)
}

/// `line_target text "12:5"`: the byte of that 1-based line and character column (`":5"` is
/// optional; past the end is clamped); `None` for anything else.
pub fn line_target(text: &str, input: &str) -> Option<i64> {
    let number = |s: &str| match s.trim().parse::<i64>().ok() {
        Some(n) if n >= 1 => Some(n),
        _ => None,
    };
    let (line, column) = match input.find(':') {
        Some(i) => (number(&input[..i]), number(&input[i + 1..])),
        None => (number(input), Some(1)),
    };
    let (line, column) = (line?, column?);
    let from = line_offset(text, line);
    let stop = pxui::ui_text::line_end(text, from);
    Some(text[from..stop].char_indices().nth((column - 1) as usize).map_or(stop, |(at, _)| from + at) as i64)
}

/// the Graph tab's slot for a graph
pub fn graph_slot<'a>(state: &'a State, graph: &str) -> Option<&'a Slot> {
    state.graphs.iter().find(|(g, _)| g == graph).map(|(_, s)| s)
}

/// What the tab shows and edits: its draft when it has one.
pub fn current_text(state: &State, shown: &Shown) -> Result<String, Diagnostic> {
    Ok(match state.tab {
        Tab::Document => match &state.draft { Some(draft) => draft.clone(), None => shown.applied.text()?.to_owned() },
        Tab::Graph => match graph_slot(state, &shown.graph) {
            Some(s) => s.gtext.clone(),
            None => shown.text.clone(),
        },
        Tab::Selection => match &state.binding_draft {
            Some((k, t)) if *k == shown.key => t.clone(),
            _ => shown.text.clone(),
        },
    })
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
        (Some(p), _) => Some(i64::from(p.line)),
        (None, Some(s)) => Some(i64::from(diagnostic::position_of_offset(text, s.start).line)),
        (None, None) => None,
    }
}

fn real_errors(l: &[Diagnostic]) -> Vec<Diagnostic> {
    l.iter().filter(|d| d.severity == Severity::ErrorSeverity).cloned().collect()
}

/// the byte where 1-based `line` of `text` ends
fn line_stop(text: &str, line: i64) -> i64 {
    pxui::ui_text::line_end(text, line_offset(text, line)) as i64
}

fn symbol_spans(text: &str) -> Vec<(String, Span)> {
    fn walk(acc: &mut Vec<(String, Span)>, arena: &s::SyntaxArena, id: SyntaxId) {
        let row = arena.row(id);
        if let Node::Sym(name) = row.node {
            acc.push((arena.name(name).to_owned(), row.span.clone()));
        }
        for &child in arena.children(id) {
            walk(acc, arena, child);
        }
    }
    let Ok(forms) = s::parse(text) else { return Vec::new() };
    let mut acc = Vec::new();
    for f in &forms {
        walk(&mut acc, f.arena(), f.row_id());
    }
    acc
}

/// The symbol the byte `at` touches; `None` for anything else or text that does not read.
pub fn symbol_at(text: &str, byte: i64) -> Option<String> {
    symbol_spans(text).into_iter().find(|(_, span)| i64::from(span.start) <= byte && byte <= i64::from(span.finish)).map(|(n, _)| n)
}

/// `references text name`: the byte and the line's text of every use of the symbol `name`, in order.
pub fn references(text: &str, name: &str) -> Vec<(i64, String)> {
    symbol_spans(text)
        .into_iter()
        .filter(|(n, _)| n == name)
        .map(|(_, span)| {
            let a = pxui::ui_text::line_start(text, span.start as usize);
            let b = pxui::ui_text::line_end(text, a);
            (i64::from(span.start), text[a..b].trim().to_owned())
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
            Some(line) => shared >= line_stop(&c.at, line).max(d.span.as_ref().map(|s| i64::from(s.finish)).unwrap_or(0)),
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
        out.push_str(&format!(", find {} at {}{}", rdk_core::attribute::quote_bytes(f.query.as_bytes()), f.at, if f.replacing { " replacing" } else { "" }));
    }
    out
}

// ---- the pane ----

/// What `view` asks the editor to apply after the frame.
#[derive(Clone, Debug)]
pub enum Intent {
    /// Printing failed before any draft or source mutation.
    PrintError(Diagnostic),
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
    Menu(Option<(f32, f32)>),
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

fn dirty(state: &State, shown: &Shown) -> Result<bool, Diagnostic> {
    Ok(match &state.draft {
        Some(d) => d != shown.applied.text()?,
        None => false,
    })
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
fn colour_popup(ui: &mut Ui, at: (f32, f32), text: &str, range: (i64, i64)) -> Option<String> {
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
                let ax_x = sliders_x + (index as f32 * (field_w + 8.0));
                let (cap_y, size, ink) = (kit::cap_y(ui, cy, 20.0), kit::cap_size(ui), theme::ink_3(&theme));
                let boxed2 = boxed.clone();
                ui.draw(&boxed2, move |paint, Rect { x, y, w: _, h: _ }| {
                    let _ = cap_y;
                    paint.text_s((x + ax_x + 2.0, kit::cap_y_paint(paint, y + cy, 20.0)), size, ink, axis);
                });
                let display = format!("{:.2}", cur);
                let o = ValueFieldOpts {
                    display: Some(display.clone()),
                    fraction: Some(cur as f32),
                    slide: Some(Rc::new(|f| format!("{:.2}", f))),
                    edit: false,
                    left: false,
                    ..ValueFieldOpts::default()
                };
                let (changed, _) = ui.value_field((ax_x, cy), field_w, 20.0, &o, &|t| (t).parse::<f64>().ok().is_some(), &format!("text-colour-{}", axis), &display);
                if changed != display && from_slider.is_none() {
                    if let Some(v) = (&changed).parse::<f64>().ok() {
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
pub fn tabs_width(ui: &mut Ui, width: i64, state: &State, shown: &Shown) -> f32 {
    let dirty = match dirty(state, shown) {
        Ok(dirty) => dirty,
        Err(error) => return ui.text_width(None, &diagnostic::to_string(&error)),
    };
    let mut w = -12.0;
    for (_, label) in tab_labels(width, dirty) {
        w = w + 12.0 + ui.text_width(None, &label);
    }
    w
}

/// a find-bar button right-aligned in a row: it takes its width off `right` and leaves a gap
fn find_button(ui: &mut Ui, key: &str, right: &mut f32, active: bool, name: &str, label: &str, at_y: f32) -> bool {
    let w = kit::button_width(ui, None, false, label);
    *right -= w;
    let o = ButtonOpts { active, ..ButtonOpts::default() };
    let hit = kit::button(ui, &format!("{}{}", key, name), (*right, at_y + 4.0), w, &o, label);
    *right -= 4.0;
    hit
}

fn rule(ui: &mut Ui, theme: &Theme, key: &str, x: f32, width: f32, name: &str, at: f32) {
    let boxed = ui.box_fmt(format_args!("{}{}", key, name), BoxOpts::default().w(Size::Px(width)).h(Size::Px(1.0)).at((x, at)));
    let edge = theme::edge(theme);
    ui.draw(&boxed, move |paint, Rect { x, y, w, h }| paint.fill(x, y, w, h, edge));
}

/// the find bar between the text and the footer: one row, two with the replace row
#[allow(clippy::too_many_arguments)]
fn find_bar(
    ui: &mut Ui,
    emit: &Emit,
    state: &State,
    theme: &Theme,
    find_row: f32,
    key: &str,
    width: f32,
    x: f32,
    current: Option<(i64, i64)>,
    matches: &[(i64, i64)],
    ay: f32,
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
    let boxed = ui.box_fmt(format_args!("{}-find-count", key), BoxOpts::default().w(Size::Px(count_w)).h(Size::Px(find_row)).at((right - count_w, ry)));
    let (bad, ink2) = (total == 0 && !f.query.is_empty(), theme::ink_2(theme));
    ui.draw(&boxed, move |paint, Rect { x, y, w, h }| {
        let tw = paint.text_width(None, &count);
        paint.text_c((x + w - tw, paint.text_top(None, y, h)), if bad { theme::invalid() } else { ink2 }, &count);
    });
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
            fn spans(out: &mut Spans, arena: &s::SyntaxArena, id: SyntaxId) {
                let row = arena.row(id);
                out.push((row.id, row.span.clone()));
                for &child in arena.children(id) {
                    spans(out, arena, child);
                }
            }
            let mut all = Vec::new();
            for form in &forms {
                spans(&mut all, form.arena(), form.row_id());
            }
            let all_arc = Arc::new(all.clone());
            Some(match (tab, forms.as_slice()) {
                (Tab::Document, _) => Shown { source: Arc::new(forms.clone()), spans: all_arc, body: None, ..shown.clone() },
                (Tab::Graph, [form]) => Shown { spans: all_arc.clone(), body: last(form).map(|b| (Arc::new(b), all_arc)), ..shown.clone() },
                (Tab::Selection, [form]) => {
                    let body = if matches!(s::head(form), Some("graph" | "defn")) { last(form).unwrap_or_else(|| form.clone()) } else { form.clone() };
                    Shown { spans: all_arc.clone(), body: Some((Arc::new(body), all_arc)), ..shown.clone() }
                }
                _ => shown.clone(),
            })
        });
        mapped.as_ref().and_then(|m| binding_at(m, byte))
    })
}

struct AreaCtx {
    x: f32,
    width: f32,
    row: f32,
    theme: Theme,
    footer: f32,
    narrow: bool,
    bar: f32,
    find_row: f32,
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
    at: (f32, f32),
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
                Some(name) if graphs.contains(&name) => Some(vec![name.into()]),
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
        .filter_map(|d| line_of(text, d).map(|line| (line, d.span.as_ref().map(span_range), format!("{}  {}", d.code, first_line(&d.message)))))
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
    let apply_opts = ButtonOpts { primary: true, enabled: can_apply, hint: Some(hint.into()), ..ButtonOpts::default() };
    if kit::button(ui, &format!("{}-apply", key), (c.x + 12.0, by), apply_w, &apply_opts, "Check and apply") || (submitted && can_apply) {
        push(emit, apply(text2.clone()));
    }
    let discard_hint = if c.narrow { None } else { Some("esc") };
    let discard_opts = ButtonOpts { enabled: can_apply, hint: discard_hint.map(Into::into), ..ButtonOpts::default() };
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
        let boxed = ui.box_fmt(format_args!("{}-parinfer-label", key), BoxOpts::default().w(Size::Px(80.0)).h(Size::Px(c.bar)).at((switch_x - 8.0 - 80.0, ty + 1.0)));
        let ink2 = theme::ink_2(&theme);
        ui.draw(&boxed, move |paint, Rect { x, y, w, h }| {
            let label = "parinfer";
            let tw = paint.text_width(None, label);
            paint.text_c((x + w - tw, paint.text_top(None, y, h)), ink2, label);
        });
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
        ui.draw(&msg, move |paint, Rect { x, y, w, h }| {
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
        });
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
fn view_checked(ui: &mut Ui, bounds: (i64, i64, i64, i64), tabs_right: Option<f32>, vocab: &Vocab, names: &Names, time: f64, state: &State, shown: &Shown) -> Result<Vec<Intent>, Diagnostic> {
    let (x, y, width, height) = bounds;
    let row = ui.row_height() as f32;
    let (x, y, width, height) = (x as f32, y as f32, width as f32, height as f32);
    let theme = ui.theme();
    let emit: Emit = Rc::new(RefCell::new(Vec::new()));
    // the printed document is made here, once a frame, never inside a token scrub
    let _ = shown.applied.text()?;
    let dirty = dirty(state, shown)?;
    // the workspace sheet's narrow Lisp panel (under 400 points): "Doc", the two buttons alone in
    // the bar and no status row
    let narrow = width < 400.0;
    let tabs = tab_labels(width as i64, dirty);
    let active = tabs.iter().position(|(tab, _)| *tab == state.tab).unwrap_or(0) as i64;
    // `tabs_right` is where the header has room for them (`Chrome::header_slots`); none in a header too narrow
    if let Some(right) = tabs_right {
        let labels: Vec<String> = tabs.iter().map(|(_, l)| l.clone()).collect();
        let (hit, _) = kit::segments(ui, "text-tab", right, y - layout::HEADER_HEIGHT as f32 + 2.0, &labels, active);
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
                    forms.iter().filter_map(|form| match form.children().get(1).map(|&id| form.arena().row(id).node) {
                        Some(Node::Sym(name)) => Some((form.name(name).to_owned(), file.clone())),
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
    let text = current_text(state, shown)?;
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
    Ok(out)
}

/// A printing refusal is visible in the pane and reaches the editor without replacing its draft.
pub fn view(ui: &mut Ui, bounds: (i64, i64, i64, i64), tabs_right: Option<f32>, vocab: &Vocab, names: &Names, time: f64, state: &State, shown: &Shown) -> Vec<Intent> {
    match view_checked(ui, bounds, tabs_right, vocab, names, time, state, shown) {
        Ok(intents) => intents,
        Err(error) => {
            let (x, y, width, height) = bounds;
            let boxed = ui.box_("text-print-error", BoxOpts::default().flags(CLIP)
                .w(Size::Px(width as f32)).h(Size::Px(height as f32)).at((x as f32, y as f32)));
            let message = diagnostic::to_string(&error);
            ui.draw(&boxed, move |paint, Rect { x, y, w: _, h: _ }| {
                let top = paint.text_top(None, y, 24.0);
                paint.text_c((x + 12.0, top), theme::invalid(), &message);
            });
            vec![Intent::PrintError(error)]
        }
    }
}

#[cfg(test)]
mod printing_errors {
    use super::*;

    #[test]
    fn cached_source_size_failure_preserves_an_unapplied_draft() {
        let error = diagnostic::check_source_len(diagnostic::MAX_SOURCE_BYTES + 1).unwrap_err();
        let value = OnceLock::new();
        let _ = value.set(Err(error.clone()));
        let printed = Printed { cell: Arc::new(PrintCell { value, source: PrintSource::Ready }) };
        assert_eq!(printed.text().unwrap_err(), error);
        assert_eq!(printed.spans().unwrap_err(), error);
        let source = s::parse("(workspace w (graph g :context host 1))").unwrap();
        let mut shown = make_shown(&source, "g", None, Tab::Document).unwrap();
        shown.applied = printed;
        let state = State { tab: Tab::Document, draft: Some("unsaved source".to_owned()), ..initial() };
        assert_eq!(current_text(&state, &shown).unwrap(), "unsaved source");
        assert_eq!(state.draft.as_deref(), Some("unsaved source"));
        assert_eq!(current_text(&State { draft: None, ..state.clone() }, &shown).unwrap_err(), error);
        assert_eq!(state.draft.as_deref(), Some("unsaved source"));
    }
}
