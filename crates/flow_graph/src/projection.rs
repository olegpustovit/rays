// Port of lib/flow_graph/projection.ml: the graph pane's view of a checked workspace: nodes, zones,
// rows, chips and their layout, with no drawing.
use crate::edit_base::{self as eb, ArgKey};
use crate::exposure;
use flow_check::catalog::{self, Catalog};
use flow_check::op::{self, Op as FlowOp};
use flow_check::port_type::PortType;
use flow_check::workspace as w;
use flow_syntax::context::Context;
use flow_syntax::syntax::{self as s, Node as SNode, Syntax};
use flow_syntax::arena::{self, SyntaxBuilder, SyntaxId};
use flow_syntax::ty::{self, Ty};
use flow_syntax::{lisp, macros, value as fvalue};
use param::{KindView, Value as PValue};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;
use flow_syntax::symbol::Symbol;

type S = Syntax;
pub type Path = flow_check::workspace::Path;

#[derive(Clone, Debug, PartialEq)]
pub enum Chip {
    /// nothing written: the row shows its default
    NoValue,
    /// a number, text, flag or vector
    Const,
    /// a wire from a binding, loop variable or input, or from the nested node written in the row
    Name(String),
    /// a nested call, loop, function, record or macro call with its flat Lisp text
    Inline { glyph: String, text: String },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RowKind {
    Arg,
    Rest,
    Add,
    Hole,
    Binder,
    GroupReader,
    GroupWriter,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Control {
    Plain,
    Range(f64, f64),
    Choice,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Row {
    pub label: String,
    pub key: ArgKey,
    pub ty: Option<Ty>,
    pub expr: Option<S>,
    pub chip: Chip,
    pub default: Option<String>,
    pub socket: bool,
    pub kind: RowKind,
    pub control: Control,
    pub folder: String,
    pub head: bool,
    pub cells: Vec<(i64, String)>,
}

impl Row {
    fn with_ty(mut self, ty: Option<Ty>) -> Row {
        self.ty = ty;
        self
    }
    fn with_default(mut self, d: Option<String>) -> Row {
        self.default = d;
        self
    }
    fn with_socket(mut self, b: bool) -> Row {
        self.socket = b;
        self
    }
    fn with_kind(mut self, k: RowKind) -> Row {
        self.kind = k;
        self
    }
    fn with_control(mut self, c: Control) -> Row {
        self.control = c;
        self
    }
    fn with_folder(mut self, f: &str) -> Row {
        self.folder = f.to_owned();
        self
    }
    fn with_head(mut self, h: bool) -> Row {
        self.head = h;
        self
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ZoneKind {
    For,
    Fold,
    Scan,
    Sum,
    Let,
    Fn,
    State,
    Branch,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Role {
    Var,
    Acc,
    Param,
    Capture,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RailRow {
    pub name: String,
    pub names: Vec<String>,
    pub role: Role,
    pub ty: Option<Ty>,
    pub expr: Option<S>,
    pub key: Option<ArgKey>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Input {
    pub path: Path,
    pub name: String,
    pub ty: Ty,
    pub default: Option<S>,
}

/// A macro call and its expansion, one step at a time.
#[derive(Clone, Debug, PartialEq)]
pub struct Lens {
    pub steps: Vec<String>,
    pub error: Option<String>,
    pub template: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    pub path: Path,
    pub name: String,
    pub binds: Vec<String>,
    pub head: String,
    pub rows: Vec<Row>,
    pub outputs: Vec<(String, Ty)>,
    pub ty: Ty,
    pub note: Option<String>,
    pub bypass: bool,
    pub macro_: Option<String>,
    pub lens: Option<Lens>,
    pub live: bool,
    pub invariant: bool,
    pub synthetic: bool,
    pub zone: Option<Zone>,
    pub expr: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Zone {
    pub kind: ZoneKind,
    pub rail: Vec<RailRow>,
    pub scope: Scope,
    pub yield_label: String,
    pub order: Option<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    pub path: Path,
    pub inputs: Vec<Input>,
    pub nodes: Vec<Node>,
    pub result: ScopeResult,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ScopeResult {
    Link(String),
    Node(Path),
    Literal(S),
}

fn print_flat(f: &S) -> String {
    let t = lisp::text(std::slice::from_ref(f));
    match t.strip_suffix('\n') {
        Some(x) => x.to_owned(),
        None => t,
    }
}

// the call, then each `expand_once` of it (at most 12 steps, like the study's lens)
fn macro_lens(c: &Cx, call: &S) -> Lens {
    let mut state = macros::state();
    let mut cur = call.clone();
    let mut acc = vec![print_flat(call)];
    let mut error = None;
    let mut n = 0;
    while n < 12 {
        match macros::expand_once_with_state(&c.macros_list, &cur, &mut state) {
            Err(d) => {
                error = Some(d.message);
                break;
            }
            Ok(next) => {
                // ponytail: unchanged is judged by equality where the OCaml compares identity
                if s::equal(&next, &cur) {
                    break;
                }
                acc.push(print_flat(&next));
                cur = next;
                n += 1;
            }
        }
    }
    let template = match s::head(call) {
        Some(name) => c
            .macros_list
            .iter()
            .find(|m| m.children().get(1).is_some_and(|id| matches!(m.arena().row(*id).node, SNode::Sym(n) if m.name(n) == name)))
            .map(print_flat)
            .unwrap_or_default(),
        None => String::new(),
    };
    Lens { steps: acc, error, template }
}

// ---- what the projection reads ----

/// The part of a checked term the projection looks at.
#[derive(Clone, Debug)]
enum Slim {
    Other,
    Hof { kind: w::HofKind, inputs: Vec<(i64, Ty)> },
    CallFn { fn_name: String },
    Loop { clause_tys: Vec<Ty>, acc0: Option<Ty> },
    State { init_ty: Ty },
    Fn { params: Vec<(String, Option<Ty>)>, zone: Path },
}

#[derive(Clone, Debug)]
struct Info {
    ty: Ty,
    form_id: i64,
    /// the form, kept when it is a call of `sop/point_list` or `sop/piece_list`
    point_form: Option<S>,
    node: Slim,
}

struct GraphInfo {
    name: String,
    inputs: Vec<(String, Ty, Option<S>)>,
}

struct Cx {
    catalog: Catalog,
    ctx: Context,
    ops: Vec<FlowOp>,
    graphs: Vec<GraphInfo>,
    defs: Vec<GraphInfo>,
    macros: Vec<(String, S)>,
    macros_list: Vec<S>,
    live: w::Paths,
    invariant: w::Paths,
    terms: Vec<(Path, Info)>,
    term_index: HashMap<Path, usize>,
    forms: HashMap<i64, Info>,
}

pub fn last(e: &S) -> S {
    e.children().last().map(|id| e.child(*id)).unwrap_or_else(|| e.clone())
}
fn root_of(name: &str) -> &str {
    eb::root_of(name)
}
fn is_zone_head(h: &str) -> bool {
    matches!(h, "for" | "fold" | "scan" | "sum" | "state")
}

/// The terms below a term that the projection walks (a call of a local fn: its arguments; a fn: its body).
pub fn subterms(term: &w::Term) -> &[flow_check::term_arena::TermId] {
    match term.node {
        w::Node::CallFn(id) => term.arena().term_ids(term.arena().call_fn(id).args),
        w::Node::Fn(id) => std::slice::from_ref(&term.arena().function(id).body),
        _ => term.children(),
    }
}

fn is_point_list(f: &S) -> bool {
    matches!(s::head(f), Some("sop/point_list" | "sop/piece_list"))
}

fn info_of(t: &w::Term) -> Info {
    let arena = t.arena();
    let node = match t.node {
        w::Node::Hof(id) => {
            let hof = arena.hof(id);
            Slim::Hof { kind: hof.kind, inputs: arena.term_ids(hof.args).iter().map(|id| {
                let row = arena.row(*id);
                (arena.syntax().row(row.form).id, arena.ty(row.ty).clone())
            }).collect() }
        }
        w::Node::CallFn(id) => Slim::CallFn { fn_name: arena.name(arena.call_fn(id).name).to_owned() },
        w::Node::Loop(id) => {
            let node = arena.loop_node(id);
            Slim::Loop {
                clause_tys: arena.bindings(node.clauses).iter().map(|binding| arena.ty(arena.row(binding.term).ty).clone()).collect(),
                acc0: arena.bindings(node.accs).first().map(|binding| arena.ty(arena.row(binding.term).ty).clone()),
            }
        }
        w::Node::State(id) => Slim::State { init_ty: arena.ty(arena.row(arena.state(id).init).ty).clone() },
        w::Node::Fn(id) => {
            let function = arena.function(id);
            Slim::Fn {
                params: arena.params(function.params).iter().map(|p| (
                    match arena.pattern(p.pattern) { flow_check::term_arena::PatternNode::Name(n) => arena.name(n).to_owned(), _ => String::new() },
                    p.ty.map(|ty| arena.ty(ty).clone()),
                )).collect(),
                zone: arena.path(function.zone).iter().map(|name| arena.symbol(*name)).collect(),
            }
        }
        _ => Slim::Other,
    };
    let form = t.form();
    Info { ty: t.ty().clone(), form_id: form.id, point_form: if is_point_list(&form) { Some(form) } else { None }, node }
}

fn collect(c: &mut Cx, t: &w::Term) {
    let info = info_of(t);
    if let Some(path) = t.path() {
        let p: Path = path.iter().map(|name| t.symbol(*name)).collect();
        match c.term_index.get(&p) {
            Some(i) => c.terms[*i].1 = info.clone(),
            None => {
                c.term_index.insert(p.clone(), c.terms.len());
                c.terms.push((p.clone(), info.clone()));
            }
        }
    }
    c.forms.insert(t.form().id, info);
    for sub in subterms(t) {
        collect(c, &t.child(*sub));
    }
}

fn graph_of<'a>(workspace: &'a w::Workspace, name: &str) -> Option<&'a w::Graph> {
    workspace.graphs.iter().find(|g| g.name == name).or_else(|| workspace.defs.iter().find(|g| name == format!("def:{}", g.name) || name == g.name))
}

/// Whether the graph's authored content, checked types, live/invariant paths and macro environment
/// still describe the same projection.
pub fn same_graph(a: &w::Workspace, b: &w::Workspace, name: &str) -> bool {
    if std::ptr::eq(a, b) {
        return true;
    }
    let (Some(x), Some(y)) = (graph_of(a, name), graph_of(b, name)) else {
        return false;
    };
    // OCaml's `[]` is one immediate value, so two workspaces with no ops are physically equal too
    if !(((a.ops.is_empty() && b.ops.is_empty()) || Arc::ptr_eq(&a.ops, &b.ops))
        && x.context == y.context
        && s::equal(&x.form, &y.form)
        && a.macros.len() == b.macros.len()
        && a.macros.iter().zip(b.macros.iter()).all(|(p, q)| s::equal(p, q))
        && a.nodes.len() == b.nodes.len()
        && a.nodes.iter().zip(b.nodes.iter()).all(|(p, q)| p.kind == q.kind && p.fields == q.fields))
    {
        return false;
    }
    fn same(a: &w::Term, b: &w::Term) -> bool {
        let (sa, sb) = (subterms(a), subterms(b));
        let same_path = match (a.path(), b.path()) {
            (Some(p), Some(q)) => p.len() == q.len() && p.iter().zip(q).all(|(p, q)| a.name(*p) == b.name(*q)),
            (None, None) => true,
            _ => false,
        };
        same_path && a.ty() == b.ty() && sa.len() == sb.len() && sa.iter().zip(sb.iter()).all(|(p, q)| same(&a.child(*p), &b.child(*q)))
    }
    let root = if name.starts_with("def:") {
        name.to_owned()
    } else if a.defs.iter().any(|g| std::ptr::eq(g, x)) {
        format!("def:{}", name)
    } else {
        name.to_owned()
    };
    let under = |p: &Path| p.first().is_some_and(|first| *first == root);
    let same_paths = |p: &w::Paths, q: &w::Paths| p.iter().all(|path| !under(path) || q.contains(path)) && q.iter().all(|path| !under(path) || p.contains(path));
    same(&x.body, &y.body)
        && x.inputs.len() == y.inputs.len()
        && x.inputs.iter().zip(y.inputs.iter()).all(|((n, p, d), (m, q, e))| {
            n == m
                && p == q
                && match (d, e) {
                    (None, None) => true,
                    (Some(d), Some(e)) => same(d, e),
                    _ => false,
                }
        })
        && same_paths(&a.live, &b.live)
        && same_paths(&a.invariant, &b.invariant)
}

// ---- chips and rows ----

fn chip(c: &Cx, e: &Option<S>) -> Chip {
    let Some(e) = e else {
        return Chip::NoValue;
    };
    let inline = |glyph: &str| Chip::Inline { glyph: glyph.to_owned(), text: lisp::flat(e) };
    match &e.node {
        SNode::Sym(n) if matches!(e.name(*n), "t" | "pi" | "true" | "false" | "nil") => Chip::Const,
        SNode::Sym(n) => Chip::Name(e.name(*n).to_owned()),
        SNode::Num(_) | SNode::Str(_) | SNode::Vec(_) | SNode::Kw(_) | SNode::Quote(..) => Chip::Const,
        SNode::Map(_) => inline("{}"),
        SNode::List(_) => match s::head(e) {
            Some("for") => inline("for"),
            Some("sum") => inline("Σ"),
            Some("fold" | "scan") => inline("⟲"),
            Some("fn") => inline("λ"),
            Some("iterate") => inline("⟲"),
            Some("if" | "cond" | "case") => inline("?"),
            Some(h) if c.macros.iter().any(|(n, _)| n == h) => inline("◆"),
            _ => inline("ƒ"),
        },
    }
}

fn row(c: &Cx, label: &str, key: ArgKey, expr: Option<S>) -> Row {
    Row {
        label: label.to_owned(),
        key,
        ty: None,
        chip: chip(c, &expr),
        expr,
        default: None,
        socket: true,
        kind: RowKind::Arg,
        control: Control::Plain,
        folder: String::new(),
        head: false,
        cells: Vec::new(),
    }
}

fn add(c: &Cx, label: &str, key: ArgKey, socket: bool, ty: Option<Ty>) -> Row {
    row(c, label, key, None).with_ty(ty).with_socket(socket).with_kind(RowKind::Add)
}

fn ty_of_port(p: &PortType) -> Ty {
    match p {
        PortType::Geometry => ty::geometry(),
        PortType::Float => Ty::Float,
        PortType::Int => Ty::Int,
        PortType::Bool => Ty::Bool,
        PortType::Vec3 => Ty::Vec3,
        PortType::Image => ty::image(),
        PortType::Fn(sig) => Ty::Fn(Some(Box::new(sig.clone()))),
    }
}

fn show_value(v: &PValue) -> String {
    match v {
        PValue::Bool(b) => b.to_string(),
        PValue::Int(i) => i.to_string(),
        PValue::Float(f) => fvalue::general(*f),
        PValue::Text(s) => String::from_utf8_lossy(s).into_owned(),
        PValue::Choice(s) => s.clone(),
    }
}

// what a parameter's field is: a soft range paints a position line, a choice a chevron
fn control_of(p: &catalog::Parameter) -> Control {
    match p.fields.as_slice() {
        [(_, KindView::Floating(r), _)] => Control::Range(r.soft_min, r.soft_max),
        [(_, KindView::Integer(r), _)] => Control::Range(r.soft_min as f64, r.soft_max as f64),
        [(_, KindView::Choice(_), _)] => Control::Choice,
        _ => Control::Plain,
    }
}

fn default_text(p: &catalog::Parameter) -> Option<String> {
    match p.fields.as_slice() {
        [(_, _, v)] => Some(show_value(v)),
        fs if fs.len() == 3 => Some(format!("[{}]", fs.iter().map(|(_, _, v)| show_value(v)).collect::<Vec<_>>().join(" "))),
        _ => None,
    }
}

// a macro parameter the template uses as a loop or scope name
fn binder(p: &str, e: &S) -> bool {
    let unquoted = |x: &S| match &x.node {
        SNode::Quote(s::QuoteKind::Unquote, inner) => matches!(x.arena().row(*inner).node, SNode::Sym(n) if x.name(n) == p),
        _ => false,
    };
    let here = match e.node {
        SNode::List(range) => {
            let items = e.range(range);
            items.first().is_some_and(|head| matches!(e.arena().row(*head).node, SNode::Sym(h) if is_zone_head(e.name(h)) || e.name(h) == "let*"))
                && items[1..].iter().any(|id| match e.arena().row(*id).node {
                    SNode::Vec(range) => e.range(range).chunks_exact(2).any(|pair| unquoted(&e.child(pair[0]))),
                    _ => false,
                })
        }
        _ => false,
    };
    here || e.children().iter().any(|id| binder(p, &e.child(*id)))
}

fn positional_args<'a>(owner: &'a S, head: &str, args: &'a [SyntaxId]) -> impl Iterator<Item = (SyntaxId, Option<&'a [u8]>)> + 'a {
    let mut skip = false;
    arena::attribute_args_view(head, owner.arena(), args).enumerate().filter_map(move |(index, item)| {
        if skip { skip = false; return None; }
        if item.1.is_none() && index + 1 < args.len() && matches!(owner.arena().row(item.0).node, SNode::Kw(_)) {
            skip = true;
            None
        } else {
            Some(item)
        }
    })
}

fn keyword_args<'a>(owner: &'a S, head: &str, args: &'a [SyntaxId]) -> impl Iterator<Item = (&'a str, (SyntaxId, Option<&'a [u8]>))> + 'a {
    let mut key = None;
    arena::attribute_args_view(head, owner.arena(), args).enumerate().filter_map(move |(index, item)| {
        if let Some(key) = key.take() { return Some((key, item)); }
        if item.1.is_none() && index + 1 < args.len() {
            if let SNode::Kw(name) = owner.arena().row(item.0).node { key = Some(owner.name(name)); }
        }
        None
    })
}

fn capture_arg(owner: &S, (id, normalized): (SyntaxId, Option<&[u8]>)) -> S {
    let Some(bytes) = normalized else { return owner.child(id) };
    let mut builder = SyntaxBuilder::new();
    let source = owner.arena().row(id);
    let bytes = builder.bytes(bytes);
    let id = builder.push(source.id, SNode::Str(bytes), source.span.clone(), owner.arena().trivia(id).cloned());
    builder.freeze().handle(id)
}

fn macro_rows(c: &Cx, m: &S, owner: &S, head: &str, args: &[SyntaxId]) -> Vec<Row> {
    let npos = positional_args(owner, head, args).count() as i64;
    let Ok((req, rest)) = macros::params(m) else {
        return Vec::new();
    };
    let template = last(m);
    let mut rows: Vec<Row> = req
        .iter()
        .enumerate()
        .map(|(i, p)| row(c, p, ArgKey::Pos(i as i64), positional_args(owner, head, args).nth(i).map(|arg| capture_arg(owner, arg))).with_kind(if binder(p, &template) { RowKind::Binder } else { RowKind::Hole }))
        .collect();
    if let Some(r) = rest {
        let n = req.len();
        for (j, a) in positional_args(owner, head, args).enumerate().skip(n).map(|(i, a)| (i - n, a)) {
            let label = if j == 0 { r.clone() } else { format!("{} {}", r, j + 1) };
            rows.push(row(c, &label, ArgKey::Pos((n + j) as i64), Some(capture_arg(owner, a))).with_kind(RowKind::Rest));
        }
        rows.push(add(c, &format!("+ {}", r), ArgKey::Pos(npos), true, None));
    }
    rows
}

fn trim_ws(t: &str) -> &str {
    t.trim_matches(|c| matches!(c, ' ' | '\t' | '\n' | '\r' | '\u{000c}'))
}

fn kind_section(k: &catalog::Kind) -> String {
    let name = match k.qualified.rfind('/') {
        Some(i) => &k.qualified[i + 1..],
        None => &k.qualified,
    };
    let mut cs = name.chars();
    match cs.next() {
        Some(f) => f.to_ascii_uppercase().to_string() + cs.as_str(),
        None => String::new(),
    }
}

fn kind_rows(c: &Cx, k: &catalog::Kind, owner: &S, head: &str, args: &[SyntaxId]) -> Vec<Row> {
    let npos = positional_args(owner, head, args).count() as i64;
    let kw = |name: &str| keyword_args(owner, head, args).find(|(key, _)| *key == name).map(|(_, arg)| capture_arg(owner, arg));
    let mut rows = Vec::new();
    for (i, sl) in k.slots.iter().enumerate() {
        let i = i as i64;
        let slot_ty = w::slot_ty(Some(sl), k);
        let default = if slot_ty == ty::geometry() {
            None
        } else {
            ty::default(&slot_ty).map(|form| trim_ws(&lisp::text(&[form])).to_owned())
        };
        // the first geometry slot is the header's in-port, not a row of the body
        let head_of = i == 0 && slot_ty == ty::geometry();
        if sl.rest {
            for (j, a) in positional_args(owner, head, args).enumerate().skip(i as usize).map(|(x, a)| (x - i as usize, a)) {
                let label = if j == 0 { sl.name.clone() } else { format!("{} {}", sl.name, j + 1) };
                rows.push(row(c, &label, ArgKey::Pos(i + j as i64), Some(capture_arg(owner, a))).with_ty(Some(slot_ty.clone())).with_kind(RowKind::Rest).with_head(head_of && j == 0));
            }
            if let Some(value) = kw(&sl.name) {
                rows.push(row(c, &sl.name, ArgKey::Kw(sl.name.clone()), Some(value)).with_ty(Some(slot_ty.clone())).with_kind(RowKind::Rest));
            }
            rows.push(add(c, &format!("+ {}", sl.name), ArgKey::Pos(npos.max(i)), true, Some(slot_ty.clone())));
        } else {
            match (positional_args(owner, head, args).nth(i as usize).map(|arg| capture_arg(owner, arg)), kw(&sl.name)) {
                (Some(a), _) => rows.push(row(c, &sl.name, ArgKey::Pos(i), Some(a)).with_ty(Some(slot_ty.clone())).with_default(default).with_head(head_of)),
                (None, Some(a)) => rows.push(row(c, &sl.name, ArgKey::Kw(sl.name.clone()), Some(a)).with_ty(Some(slot_ty.clone())).with_default(default).with_head(head_of)),
                (None, None) => {
                    let key = if sl.required && i == npos { ArgKey::Pos(i) } else { ArgKey::Kw(sl.name.clone()) };
                    rows.push(row(c, &sl.name, key, None).with_ty(Some(slot_ty.clone())).with_default(default).with_head(head_of));
                }
            }
        }
    }
    for p in &k.parameters {
        let kind = if w::group_reader(p) {
            RowKind::GroupReader
        } else if w::group_writer(k, p) {
            RowKind::GroupWriter
        } else {
            RowKind::Arg
        };
        let material = k.qualified == "sop/material" && p.name == "material";
        let ty = if material {
            ty::material()
        } else {
            match &p.ty {
                Some(t) => ty_of_port(t),
                None => Ty::Text,
            }
        };
        let folder = if p.folder.is_empty() { kind_section(k) } else { p.folder.join(" / ") };
        rows.push(
            row(c, &p.name, ArgKey::Kw(p.name.clone()), kw(&p.name))
                .with_ty(Some(ty))
                .with_default(default_text(p))
                .with_control(control_of(p))
                .with_socket(true)
                .with_kind(kind)
                .with_folder(&folder),
        );
    }
    rows
}

fn input_rows(c: &Cx, inputs: &[(String, Ty, Option<S>)], owner: &S, head: &str, args: &[SyntaxId], positional: bool) -> Vec<Row> {
    inputs
        .iter()
        .enumerate()
        .map(|(i, (n, t, d))| {
            let default = d.as_ref().map(lisp::flat);
            match if positional { positional_args(owner, head, args).nth(i) } else { None } {
                Some(arg) => row(c, n, ArgKey::Pos(i as i64), Some(capture_arg(owner, arg))).with_ty(Some(t.clone())),
                None => row(c, n, ArgKey::Kw(n.clone()), keyword_args(owner, head, args).find(|(key, _)| *key == n).map(|(_, arg)| capture_arg(owner, arg))).with_ty(Some(t.clone())).with_default(default),
            }
        })
        .collect()
}

fn call_rows(c: &Cx, e: &S, h: &str, args: &[SyntaxId]) -> Vec<Row> {
    let pos = || positional_args(e, h, args);
    let kws = || keyword_args(e, h, args);
    let npos = pos().count() as i64;
    let at = |i: i64| if i < 0 { None } else { pos().nth(i as usize).map(|arg| capture_arg(e, arg)) };
    let kw_find = |name: &str| kws().find(|(key, _)| *key == name).map(|(_, arg)| capture_arg(e, arg));
    let posrow = |ty: Option<Ty>, socket: bool, label: &str, i: i64| row(c, label, ArgKey::Pos(i), at(i)).with_ty(ty).with_socket(socket);
    let kwrow = |ty: Option<Ty>, label: &str| {
        let choices = w::op_choices(&c.ops, h, label);
        let literal = !choices.is_empty() && op::find(&c.ops, h, &c.ctx).is_some_and(|o| o.out_args.is_some());
        row(c, label, ArgKey::Kw(label.to_owned()), kw_find(label))
            .with_ty(ty)
            .with_control(if choices.is_empty() { Control::Plain } else { Control::Choice })
            .with_socket(!literal)
    };
    let items = |label: &dyn Fn(i64) -> String, prefix: &str, ty: Option<Ty>| -> Vec<Row> {
        let mut rows: Vec<Row> = pos().enumerate().map(|(i, arg)| row(c, &label(i as i64), ArgKey::Pos(i as i64), Some(capture_arg(e, arg))).with_ty(ty.clone()).with_kind(RowKind::Rest)).collect();
        rows.push(add(c, prefix, ArgKey::Pos(npos), true, ty));
        rows
    };
    let list_ty = Some(Ty::List(Box::new(Ty::Any)));
    match h {
        "with_error" => vec![posrow(None, true, "protected", 0), posrow(Some(Ty::Fn(None)), true, "mapper", 1)],
        "error_rows" => vec![
            posrow(Some(Ty::Array(Box::new(Ty::Int))), true, "rows", 0),
            posrow(Some(Ty::Fn(None)), true, "mapper", 1),
            kwrow(Some(Ty::Text), "policy"),
        ],
        "if" => vec![posrow(Some(Ty::Bool), true, "if", 0), row(c, "then", ArgKey::Arm(0), at(1)), row(c, "else", ArgKey::Arm(-1), at(2))],
        "list" => items(&|i| i.to_string(), "+ item", None),
        "str" => items(&|_| "part".to_owned(), "+ part", None),
        "concat" => items(&|i| format!("list {}", i + 1), "+ list", list_ty),
        "cond" => {
            let mut rows = Vec::new();
            for i in (0..npos).filter(|i| i % 2 == 0) {
                rows.push(posrow(Some(Ty::Bool), true, "when", i));
                rows.push(row(c, "then", ArgKey::Arm(i / 2), at(i + 1)));
            }
            rows.push(row(c, "else", ArgKey::Arm(-1), kw_find("else")));
            rows
        }
        "case" => {
            let mut rows = vec![posrow(None, true, "of", 0)];
            for i in (0..npos).filter(|i| i % 2 == 1) {
                rows.push(posrow(None, true, "is", i));
                rows.push(row(c, "then", ArgKey::Arm((i - 1) / 2), at(i + 1)));
            }
            rows.push(row(c, "else", ArgKey::Arm(-1), kw_find("else")));
            rows
        }
        "map" | "filter" | "reduce" | "sort-by" => {
            let input_ty = |i: usize, fallback: Option<Ty>| -> Option<Ty> {
                match c.forms.get(&e.id) {
                    Some(Info { node: Slim::Hof { inputs, .. }, .. }) => match inputs.get(i) {
                        Some((_, t)) => Some(t.clone()),
                        None => fallback,
                    },
                    _ => fallback,
                }
            };
            let labels: Vec<&str> = match h {
                "map" => vec!["f", "list", "list 2", "list 3"],
                "filter" => vec!["keep if", "list"],
                "reduce" => vec!["f", "start", "list"],
                _ => vec!["key", "list"],
            };
            let mut rows = Vec::new();
            for (i, l) in labels.iter().enumerate() {
                if i < 2 || at(i as i64).is_some() || h == "reduce" {
                    let fb = if i == 0 {
                        Some(Ty::Fn(None))
                    } else if l.starts_with("list") {
                        list_ty.clone()
                    } else {
                        None
                    };
                    rows.push(posrow(input_ty(i, fb), true, l, i as i64));
                }
            }
            if h == "map" && npos < 4 {
                rows.push(add(c, "+ list", ArgKey::Pos(npos), true, list_ty));
            }
            rows
        }
        "iterate" => {
            let start = match c.forms.get(&e.id) {
                Some(Info { node: Slim::Hof { kind: w::HofKind::Iterate, inputs }, .. }) => inputs.get(1).map(|x| x.1.clone()),
                _ => None,
            };
            vec![
                posrow(Some(Ty::Int), true, "count", 0),
                posrow(start, true, "start", 1),
                posrow(Some(Ty::Fn(None)), true, "f", 2),
                kwrow(Some(Ty::Fn(None)), "until"),
            ]
        }
        "get" => vec![posrow(None, true, "record", 0), posrow(None, false, "field", 1)],
        "assoc" => {
            let mut rows = vec![posrow(None, true, "record", 0)];
            rows.extend(kws().map(|(key, _)| kwrow(None, key)));
            rows
        }
        "values" => {
            let mut rows: Vec<Row> = kws().map(|(key, _)| kwrow(None, key)).collect();
            rows.push(add(c, "+ output", ArgKey::Kw(String::new()), false, None));
            rows
        }
        "ref" => {
            let g = match at(0) {
                Some(form) => eb::sym_of(&form).and_then(|n| c.graphs.iter().find(|g| g.name == n)),
                _ => None,
            };
            let mut rows = vec![row(c, "graph", ArgKey::Pos(0), at(0)).with_socket(false)];
            if let Some(g) = g {
                rows.extend(input_rows(c, &g.inputs, e, h, args, false));
            }
            rows
        }
        _ => {
            if let Some(d) = c.defs.iter().find(|d| d.name == h) {
                return input_rows(c, &d.inputs, e, h, args, true);
            }
            if let Some((_, m)) = c.macros.iter().find(|(n, _)| n == h) {
                return macro_rows(c, m, e, h, args);
            }
            if let Some(o) = w::op_signature(&c.ops, &c.ctx, h) {
                let np = o.pos.len();
                let mut rows = Vec::new();
                for (i, (l, t)) in o.pos.iter().chain(o.opt.iter()).enumerate() {
                    if i < np || (i as i64) < npos {
                        rows.push(posrow(Some(t.clone()), true, l, i as i64));
                    }
                }
                if let Some((rn, t)) = &o.rest {
                    for (j, arg) in pos().enumerate().skip(np).map(|(i, arg)| (i - np, arg)) {
                        let label = if j == 0 { rn.to_string() } else { format!("{} {}", rn, j + 1) };
                        rows.push(row(c, &label, ArgKey::Pos((np + j) as i64), Some(capture_arg(e, arg))).with_ty(Some(t.clone())).with_kind(RowKind::Rest));
                    }
                    rows.push(add(c, &format!("+ {}", rn), ArgKey::Pos(npos.max(np as i64)), true, Some(t.clone())));
                }
                for (n, t) in &o.kw {
                    rows.push(kwrow(Some(t.clone()), n));
                }
                return rows;
            }
            match catalog::resolve_kind(&c.catalog, &c.ctx, h) {
                Ok(k) => kind_rows(c, &k, e, h, args),
                Err(_) => {
                    // a call of a local fn reads the fn: a wire, so a used fn never looks unused
                    let mut callee = Vec::new();
                    let mut names: Vec<String> = Vec::new();
                    if let (Some(Info { node: Slim::CallFn { fn_name }, .. }), Some(f)) = (c.forms.get(&e.id), s::children(e).first()) {
                        // its inputs are named by the fn's parameters.  ponytail: the first fn of
                        // that name in the graph, so a shadowed name in a sibling scope reads the other
                        for (p, t) in &c.terms {
                            if let Info { node: Slim::Fn { params, .. }, .. } = t {
                                if !p.is_empty() && p[p.len() - 1] == *fn_name {
                                    names = params.iter().map(|x| x.0.clone()).collect();
                                    break;
                                }
                            }
                        }
                        callee.push(row(c, "fn", ArgKey::Pos(-1), Some(e.child(*f))).with_ty(Some(Ty::Fn(None))).with_socket(false));
                    }
                    for (i, arg) in pos().enumerate() {
                        let label = match names.get(i) {
                            Some(n) if !n.is_empty() => n.clone(),
                            _ => format!("arg{}", i + 1),
                        };
                        callee.push(row(c, &label, ArgKey::Pos(i as i64), Some(capture_arg(e, arg))));
                    }
                    callee
                }
            }
        }
    }
}

fn rows_of(c: &Cx, e: &S) -> Vec<Row> {
    match (&e.node, s::head(e)) {
        (SNode::Map(range), _) => {
            let mut rows: Vec<Row> = e.range(*range).chunks_exact(2).filter_map(|pair| {
                match e.arena().row(pair[0]).node {
                    SNode::Kw(key) => {
                        let key = e.name(key);
                        Some(row(c, key, ArgKey::Field(key.to_owned()), Some(e.child(pair[1]))))
                    }
                    _ => None,
                }
            }).collect();
            rows.push(add(c, "+ field", ArgKey::Field(String::new()), false, None));
            rows
        }
        (SNode::List(range), Some(h)) => call_rows(c, e, h, &e.range(*range)[1..]),
        (SNode::Vec(_), _) => {
            let ty = c.forms.get(&e.id).map(|i| i.ty.clone());
            vec![row(c, "value", ArgKey::Whole, Some(e.clone())).with_ty(ty)]
        }
        (SNode::Sym(n), _) if !matches!(e.name(*n), "t" | "pi" | "true" | "false" | "nil") => vec![row(c, "from", ArgKey::Whole, Some(e.clone()))],
        _ => vec![row(c, "value", ArgKey::Whole, Some(e.clone()))],
    }
}

fn head_label(e: &S) -> String {
    match &e.node {
        SNode::Map(_) => "record".to_owned(),
        SNode::Num(_) => "number".to_owned(),
        SNode::Str(_) => "text".to_owned(),
        SNode::Sym(_) => "link".to_owned(),
        SNode::Vec(_) => "vector".to_owned(),
        _ => s::head(e).unwrap_or("form").to_owned(),
    }
}

// ---- nodes, zones and scopes ----

fn scope_form(e: &S) -> Option<(&[flow_syntax::arena::SyntaxId], S)> {
    if !e.meta().is_empty() {
        return None;
    }
    if let SNode::List(range) = e.node {
        let items = e.range(range);
        if items.len() == 3 && s::head(e) == Some("let*") {
            if let SNode::Vec(range) = e.arena().row(items[1]).node {
                let bindings = e.range(range);
                if bindings.len() % 2 == 0 {
                    return Some((bindings, e.child(items[2])));
                }
            }
        }
    }
    None
}

fn zone_kind(e: &S) -> Option<ZoneKind> {
    if !e.meta().is_empty() {
        return None;
    }
    match s::head(e) {
        Some("for") => Some(ZoneKind::For),
        Some("fold") => Some(ZoneKind::Fold),
        Some("scan") => Some(ZoneKind::Scan),
        Some("sum") => Some(ZoneKind::Sum),
        Some("state") => Some(ZoneKind::State),
        Some("let*") if scope_form(e).is_some() => Some(ZoneKind::Let),
        Some("fn") => Some(ZoneKind::Fn),
        _ => None,
    }
}

fn outputs(pat: &Option<S>, ty: &Ty) -> Vec<(String, Ty)> {
    let Some(pat) = pat else {
        return Vec::new();
    };
    match (&pat.node, ty) {
        (SNode::Sym(_), Ty::Record(fs)) => fs.iter().map(|(name, ty)| (name.to_string(), ty.clone())).collect(),
        (SNode::Sym(_), _) => Vec::new(),
        (SNode::Vec(_), _) => {
            let t = match ty {
                Ty::List(e) => (**e).clone(),
                Ty::Vec2 | Ty::Vec3 | Ty::Vec4 => Ty::Float,
                _ => Ty::Any,
            };
            eb::pat_names(pat).into_iter().map(|n| (n, t.clone())).collect()
        }
        _ => eb::pat_names(pat)
            .into_iter()
            .map(|n| {
                let t = match ty {
                    Ty::Record(fs) => fs.iter().find(|(k, _)| *k == n).map(|x| x.1.clone()).unwrap_or(Ty::Any),
                    _ => Ty::Any,
                };
                (n, t)
            })
            .collect(),
    }
}

fn rail_of(c: &Cx, kind: ZoneKind, e: &S, t: Option<&Info>, visible: &[String]) -> Vec<RailRow> {
    let mut bound: Vec<String> = Vec::new();
    let kids = s::children(e);
    let clause_ty = |k: usize| -> Option<Ty> {
        match t {
            Some(Info { node: Slim::Loop { clause_tys, .. }, .. }) => clause_tys.get(k).and_then(ty::elem),
            _ => None,
        }
    };
    let vars = |first: usize, bound: &mut Vec<String>| -> Vec<RailRow> {
        match kids.get(first).map(|id| e.arena().row(*id).node) {
            Some(SNode::Vec(range)) => e.range(range).chunks_exact(2)
                .enumerate()
                .map(|(k, pair)| {
                    let (p, x) = (e.child(pair[0]), e.child(pair[1]));
                    bound.extend(eb::pat_names(&p));
                    RailRow {
                        name: eb::pat_key(&p),
                        names: eb::pat_names(&p),
                        role: Role::Var,
                        ty: clause_ty(k),
                        expr: Some(x),
                        key: Some(ArgKey::Bv(first as i64, 2 * k as i64 + 1)),
                    }
                })
                .collect(),
            _ => Vec::new(),
        }
    };
    let rows: Vec<RailRow> = match kind {
        ZoneKind::For | ZoneKind::Sum => vars(1, &mut bound),
        ZoneKind::Fold | ZoneKind::Scan | ZoneKind::State => {
            let mut acc = Vec::new();
            if let Some(SNode::Vec(range)) = kids.get(1).map(|id| e.arena().row(*id).node) {
                if let [p, init] = e.range(range) {
                    let p = e.child(*p);
                    bound.extend(eb::pat_names(&p));
                    let ty = match t {
                        Some(Info { node: Slim::Loop { acc0, .. }, .. }) => acc0.clone(),
                        Some(Info { node: Slim::State { init_ty }, .. }) => Some(init_ty.clone()),
                        _ => None,
                    };
                    acc.push(RailRow { name: eb::pat_key(&p), names: eb::pat_names(&p), role: Role::Acc, ty, expr: Some(e.child(*init)), key: Some(ArgKey::Bv(1, 1)) });
                }
            }
            if kind != ZoneKind::State {
                acc.extend(vars(2, &mut bound));
            }
            acc
        }
        ZoneKind::Fn => match kids.get(1).map(|id| e.arena().row(*id).node) {
            Some(SNode::Vec(range)) => e.range(range)
                .iter()
                .enumerate()
                .map(|(k, id)| {
                    let p = e.child(*id);
                    let pat = match p.node {
                        SNode::List(range) if p.range(range).len() == 3 && eb::sym_of(&p.child(p.range(range)[1])) == Some(":") => p.child(p.range(range)[0]),
                        _ => p,
                    };
                    bound.extend(eb::pat_names(&pat));
                    let ty = match t {
                        Some(fnc @ Info { node: Slim::Fn { params, zone: path }, .. }) => match params.get(k).and_then(|x| x.1.clone()) {
                            Some(ty) => Some(ty),
                            None if matches!(fnc.ty, Ty::Fn(Some(_))) => match &fnc.ty {
                                Ty::Fn(Some(sig)) => sig.params.get(k).cloned(),
                                _ => None,
                            },
                            None => {
                                let parent: Option<Path> = match path.split_last() {
                                    Some((leaf, scope)) => match leaf.rfind('#') {
                                        Some(i) => {
                                            let mut p = scope.to_vec();
                                            p.push(leaf[..i].into());
                                            Some(p)
                                        }
                                        None => None,
                                    },
                                    None => None,
                                };
                                parent.and_then(|parent| {
                                    let info = c.term_index.get(&parent).map(|i| &c.terms[*i].1);
                                    match info {
                                        Some(Info { node: Slim::Hof { kind: w::HofKind::Reduce, inputs }, .. }) if inputs.len() >= 3 && inputs[0].0 == fnc.form_id => {
                                            if k == 0 { Some(inputs[1].1.clone()) } else { ty::elem(&inputs[2].1) }
                                        }
                                        Some(Info { node: Slim::Hof { inputs, .. }, .. }) if !inputs.is_empty() && inputs[0].0 == fnc.form_id => {
                                            inputs[1..].get(k).and_then(|x| ty::elem(&x.1))
                                        }
                                        _ => None,
                                    }
                                })
                            }
                        },
                        _ => None,
                    };
                    RailRow { name: eb::pat_key(&pat), names: eb::pat_names(&pat), role: Role::Param, ty, expr: None, key: None }
                })
                .collect(),
            _ => Vec::new(),
        },
        ZoneKind::Let | ZoneKind::Branch => Vec::new(),
    };
    let body = if kind == ZoneKind::Let { e.clone() } else { last(e) };
    let captures: Vec<String> = eb::free_names(&body).into_iter().filter(|n| visible.contains(n) && !bound.contains(n)).collect();
    let mut out = rows;
    out.extend(captures.into_iter().map(|n| RailRow { name: n.clone(), names: vec![n], role: Role::Capture, ty: None, expr: None, key: None }));
    out
}

fn yield_label(k: ZoneKind) -> &'static str {
    match k {
        ZoneKind::For | ZoneKind::Scan => "collect",
        ZoneKind::Fold | ZoneKind::State => "next",
        ZoneKind::Sum => "add",
        ZoneKind::Let => "result",
        ZoneKind::Fn => "return",
        ZoneKind::Branch => "then",
    }
}

fn is_vec(e: &S) -> bool {
    matches!(e.node, SNode::Vec(_))
}

fn scope_of(c: &Cx, folded: &dyn Fn(&[Symbol]) -> bool, card_result: bool, visible: &[String], inputs: Vec<Input>, path: &Path, body: &S) -> Scope {
    let (binds, res) = match scope_form(body) {
        Some((bs, res)) => (bs, res),
        None => (&[][..], body.clone()),
    };
    let mut vis: Vec<String> = binds.chunks_exact(2).flat_map(|pair| eb::pat_names(&body.child(pair[0]))).collect();
    vis.extend(visible.iter().cloned());
    let mut nodes: Vec<Node> = Vec::new();
    for pair in binds.chunks_exact(2) {
        let (pat, expression) = (body.child(pair[0]), body.child(pair[1]));
        nodes.extend(node_of(c, folded, &vis, false, None, path, Some(&pat), &expression));
    }
    let (result, extra) = match &res.node {
        SNode::Sym(sy) if vis.iter().any(|v| v == root_of(res.name(*sy))) => (ScopeResult::Link(res.name(*sy).to_owned()), Vec::new()),
        SNode::List(_) | SNode::Map(_) | SNode::Vec(_) if card_result || !is_vec(&res) => {
            let ns = node_of(c, folded, &vis, false, None, path, None, &res);
            let mut p = path.clone();
            p.push("@result".into());
            (ScopeResult::Node(p), ns)
        }
        _ => (ScopeResult::Literal(res.clone()), Vec::new()),
    };
    nodes.extend(extra);
    Scope { path: path.clone(), inputs, nodes, result }
}

// the node of a binding (or of the result), after the nodes of the calls nested in its inputs
#[allow(clippy::too_many_arguments)]
fn node_of(c: &Cx, folded: &dyn Fn(&[Symbol]) -> bool, visible: &[String], expr: bool, nested_leaf: Option<&str>, scope_path: &Path, pat: Option<&S>, e: &S) -> Vec<Node> {
    let name = match (nested_leaf, pat) {
        (Some(leaf), _) => leaf.to_owned(),
        (None, Some(p)) => eb::pat_key(p),
        (None, None) => "@result".to_owned(),
    };
    let mut p = scope_path.clone();
    p.push(name.clone().into());
    let term: Option<&Info> = match nested_leaf {
        Some(_) => c.forms.get(&e.id),
        None => c.term_index.get(&p).map(|i| &c.terms[*i].1),
    };
    let ty = term.map(|t| t.ty.clone()).unwrap_or(Ty::Any);
    let kind = zone_kind(e);
    let zone = kind.map(|k| {
        let rail = rail_of(c, k, e, term, visible);
        let mut inner_visible: Vec<String> = rail.iter().flat_map(|r| if r.role == Role::Capture { Vec::new() } else { r.names.clone() }).collect();
        inner_visible.extend(visible.iter().cloned());
        let body = if k == ZoneKind::Let { e.clone() } else { last(e) };
        // a loop over the points or pieces of geometry says how its elements are ordered
        let order = rail.iter().find_map(|r| {
            // the collection is the call itself, or a name bound to it
            let call: Option<S> = match (r.role, &r.expr) {
                (Role::Var, Some(x)) if is_point_list(x) && matches!(x.node, SNode::List(_)) => Some(x.clone()),
                (Role::Var, Some(form)) if matches!(form.node, SNode::Sym(_)) => {
                    let mut found = None;
                    for (path, t) in &c.terms {
                        if !path.is_empty() && eb::sym_of(form) == Some(path[path.len() - 1].as_str()) {
                            found = t.point_form.clone();
                            break;
                        }
                    }
                    found
                }
                _ => None,
            };
            call.map(|e| {
                let kids = s::children(&e);
                let mut order = "by index".to_owned();
                let mut i = 0;
                while i + 1 < kids.len() {
                    if let (SNode::Kw(k), SNode::Str(v)) = (e.arena().row(kids[i]).node, e.arena().row(kids[i + 1]).node) {
                        if e.name(k) == "key" {
                            order = format!("by {}", String::from_utf8_lossy(e.bytes(v)));
                            break;
                        }
                    }
                    i += 1;
                }
                order
            })
        });
        Zone { kind: k, rail, yield_label: yield_label(k).to_owned(), order, scope: scope_of(c, folded, k == ZoneKind::Fn, &inner_visible, Vec::new(), &p, &body) }
    });
    let macro_ = match s::head(e) {
        Some(h) if c.macros.iter().any(|(n, _)| n == h) => Some(h.to_owned()),
        _ => None,
    };
    let lens = macro_.as_ref().map(|_| macro_lens(c, e));
    // a node call written in an input is a node of its own, wired to the row by its leaf
    let mut rows: Vec<Row> = Vec::new();
    let mut inner: Vec<Node> = Vec::new();
    if kind.is_none() {
        let source_rows = if nested_leaf.is_some() && eb::context_sym(e) { Vec::new() } else { rows_of(c, e) };
        for r in source_rows {
            let keyed = matches!(r.key, ArgKey::Pos(_) | ArgKey::Kw(_) | ArgKey::Arm(_));
            let plain = r.kind == RowKind::Arg || r.kind == RowKind::Rest;
            if let Some(a) = &r.expr {
                // not a macro's argument: that is a piece of its template, which may read the macro's names
                if keyed
                    && plain
                    && macro_.is_none()
                    && (eb::node_call(a) || eb::context_sym(a) || (eb::value_call(a) && (expr || !folded(&nested_path(scope_path, &eb::nested_leaf(&name, &r.key).unwrap_or_default())))))
                {
                    let leaf = eb::nested_leaf(&name, &r.key).unwrap_or_default();
                    let mut r2 = r.clone();
                    r2.chip = Chip::Name(leaf.clone());
                    rows.push(r2);
                    inner.extend(node_of(c, folded, visible, eb::value_call(a) || eb::context_sym(a), Some(&leaf), scope_path, None, a));
                    continue;
                }
                // a call written in a cell of a vector literal is a node of its own too; the row keeps its
                // literal cells and is wired from the card
                if let SNode::Vec(cs) = &a.node {
                    if keyed && plain && macro_.is_none() {
                        let base = eb::nested_leaf(&name, &r.key).unwrap_or_default();
                        // no glyph opens a cell, so a call in one is always a card
                        let lifted: Vec<(i64, String, S)> = a.range(*cs)
                            .iter()
                            .enumerate()
                            .filter_map(|(i, id)| {
                                let child = a.child(*id);
                                let leaf = eb::nested_leaf(&base, &ArgKey::Cell(i as i64)).unwrap_or_default();
                                if eb::node_call(&child) || eb::value_call(&child) { Some((i as i64, leaf, child)) } else { None }
                            })
                            .collect();
                        if lifted.is_empty() {
                            rows.push(r);
                        } else {
                            let mut r2 = r.clone();
                            r2.cells = lifted.iter().map(|(i, leaf, _)| (*i, leaf.clone())).collect();
                            rows.push(r2);
                            for (_, leaf, a) in &lifted {
                                inner.extend(node_of(c, folded, visible, eb::value_call(a), Some(leaf), scope_path, None, a));
                            }
                        }
                        continue;
                    }
                }
            }
            rows.push(r);
        }
    }
    let head = match kind {
        Some(ZoneKind::Let) => "let*".to_owned(),
        Some(_) => s::head(e).unwrap_or("").to_owned(),
        None => {
            if nested_leaf.is_some() && eb::context_sym(e) { "t".to_owned() } else { head_label(e) }
        }
    };
    let node = Node {
        path: p.clone(),
        name: name.clone(),
        binds: match (nested_leaf, pat) {
            (None, Some(pt)) => eb::pat_names(pt),
            _ => vec![name.clone()],
        },
        head,
        rows,
        outputs: if nested_leaf.is_none() { outputs(&pat.cloned(), &ty) } else { Vec::new() },
        ty,
        note: match pat {
            Some(pt) if !pt.notes().is_empty() => Some(pt.notes().join("\n")),
            _ => None,
        },
        bypass: e.meta().iter().any(|m| m == "bypass"),
        macro_,
        lens,
        live: c.live.contains(&p),
        invariant: c.invariant.contains(&p),
        synthetic: pat.is_none() && nested_leaf.is_none(),
        zone,
        expr,
    };
    inner.push(node);
    inner
}

fn nested_path(scope_path: &Path, leaf: &str) -> Path {
    let mut p = scope_path.clone();
    p.push(leaf.into());
    p
}

/// A nested node: a call written in an input, with no binding.
pub fn anonymous(n: &Node) -> bool {
    eb::nested(&n.name)
}

/// What the card is called: the binding, `result`, or the kind of a nested node.
pub fn title(n: &Node) -> String {
    if n.synthetic {
        "result".to_owned()
    } else if anonymous(n) {
        match n.head.rfind('/') {
            Some(i) => n.head[i + 1..].to_owned(),
            None => n.head.clone(),
        }
    } else {
        n.name.clone()
    }
}

/// The names a row is wired from: the nested node written in it, else every name its expression reads.
pub fn sources(r: &Row) -> Vec<String> {
    match (&r.chip, &r.expr) {
        (Chip::Name(leaf), _) if eb::nested(leaf) => vec![leaf.clone()],
        (_, Some(e)) if matches!(e.node, SNode::Sym(n) if matches!(e.name(n), "t" | "pi" | "true" | "false" | "nil")) => Vec::new(),
        (_, Some(e)) if matches!(e.node, SNode::Vec(_)) && !r.cells.is_empty() => {
            // a call written in a cell is a card of its own, wired from its leaf
            let mut out = Vec::new();
            for (i, id) in e.children().iter().enumerate() {
                match r.cells.iter().find(|(k, _)| *k == i as i64) {
                    Some((_, leaf)) => out.push(leaf.clone()),
                    None => out.extend(eb::free_names(&e.child(*id))),
                }
            }
            out
        }
        (_, Some(e)) => eb::free_names(e),
        (_, None) => Vec::new(),
    }
}

/// The target of the wire from cell `i` of the row's vector literal.
pub fn cell_wire(n: &Node, r: &Row, i: i64) -> (Path, ArgKey, Option<S>) {
    let mut p = n.path[..n.path.len().saturating_sub(1)].to_vec();
    p.push(eb::nested_leaf(&n.name, &r.key).unwrap_or_default().into());
    let mut builder = flow_syntax::arena::SyntaxBuilder::new();
    let number = builder.intern("0.0");
    let id = builder.push(0, SNode::Num(number), rays_diagnostic::Span { start: 0, finish: 0 }, None);
    (p, ArgKey::Cell(i), Some(builder.freeze().handle(id)))
}

/// Patch existing same-type literal rows.
pub fn with_arguments(changes: &[(Path, ArgKey, S)], scope: &Scope) -> Scope {
    let mut edits: HashMap<Path, Vec<(ArgKey, S)>> = HashMap::new();
    for (path, key, expr) in changes {
        edits.entry(path.clone()).or_default().insert(0, (key.clone(), expr.clone()));
    }
    fn patch(scope: &Scope, edits: &HashMap<Path, Vec<(ArgKey, S)>>) -> Scope {
        let nodes = scope
            .nodes
            .iter()
            .map(|node| {
                let zone = node.zone.as_ref().map(|z| Zone { scope: patch(&z.scope, edits), ..z.clone() });
                let rows = match edits.get(&node.path) {
                    None => node.rows.clone(),
                    Some(changes) => node
                        .rows
                        .iter()
                        .map(|row| match changes.iter().find(|(k, _)| *k == row.key) {
                            None => row.clone(),
                            Some((_, expr)) => Row { expr: Some(expr.clone()), ..row.clone() },
                        })
                        .collect(),
                };
                Node { rows, zone, ..node.clone() }
            })
            .collect();
        Scope { nodes, ..scope.clone() }
    }
    patch(scope, &edits)
}

/// A call whose first input fits its result, or one already bypassed: it can carry the `B` flag.
pub fn bypassable(n: &Node) -> bool {
    n.zone.is_none()
        && !n.synthetic
        && n.macro_.is_none()
        && !matches!(n.head.as_str(), "record" | "number" | "text" | "link" | "vector" | "list" | "str" | "cond" | "case")
        && (n.bypass
            || n.head == "if"
            || match n.rows.iter().find(|r| r.kind == RowKind::Arg) {
                Some(Row { ty: Some(ty), .. }) => ty::fits(ty, &n.ty),
                _ => false,
            })
}

/// A node whose positional inputs are an ordered list the user can move items in.
pub fn reorderable(n: &Node) -> bool {
    n.zone.is_none() && matches!(n.head.as_str(), "list" | "str" | "scene/merge")
}

/// The graph `of_graph` would project (a `def:` name finds a definition), or `None`.
pub fn find_graph<'a>(workspace: &'a w::Workspace, name: &str) -> Option<&'a w::Graph> {
    let def = name.starts_with("def:");
    let bare = if def { &name[4..] } else { name };
    if def {
        workspace.defs.iter().find(|g| g.name == bare)
    } else {
        workspace.graphs.iter().chain(workspace.defs.iter()).find(|g| g.name == bare)
    }
}

fn graph_info(g: &w::Graph) -> GraphInfo {
    GraphInfo { name: g.name.clone(), inputs: g.inputs.iter().map(|(n, t, d)| (n.clone(), t.clone(), d.as_ref().map(|x| x.form()))).collect() }
}

/// The root scope of a graph or, for `"def:name"` or a `defn` name, of a definition. An operator call
/// written in an input is a card unless `folded` its leaf, and its whole call tree with it.
pub fn of_graph_with(folded: &dyn Fn(&[Symbol]) -> bool, catalog: &Catalog, workspace: &w::Workspace, name: &str) -> Result<Scope, String> {
    let _timer = flow_syntax::phase_timer::measure("project");
    let mut kinds: Vec<catalog::Kind> = workspace.nodes.iter().map(|n| n.kind.clone()).collect();
    kinds.extend(catalog.kinds.iter().cloned());
    let catalog = Catalog { version: catalog.version, kinds };
    let g = find_graph(workspace, name).ok_or_else(|| format!("Projection.of_graph: no graph {}", name))?;
    let root: Path = vec![if workspace.defs.iter().any(|d| std::ptr::eq(d, g)) { format!("def:{}", g.name).into() } else { g.name.clone().into() }];
    let macros_list = (*workspace.macros).clone();
    let macros: Vec<(String, S)> = macros_list
        .iter()
        .filter_map(|m| m.children().get(1).and_then(|id| match m.arena().row(*id).node {
            SNode::Sym(n) => Some((m.name(n).to_owned(), m.clone())),
            _ => None,
        }))
        .collect();
    let mut c = Cx {
        catalog,
        ctx: g.context,
        ops: (*workspace.ops).clone(),
        graphs: workspace.graphs.iter().map(graph_info).collect(),
        defs: workspace.defs.iter().map(graph_info).collect(),
        macros,
        macros_list,
        live: workspace.live.clone(),
        invariant: workspace.invariant.clone(),
        terms: Vec::new(),
        term_index: HashMap::new(),
        forms: HashMap::new(),
    };
    collect(&mut c, &g.body);
    let inputs: Vec<Input> = g
        .inputs
        .iter()
        .map(|(n, ty, d)| {
            let mut path = root.clone();
            path.push(format!(":{}", n).into());
            Input { path, name: n.clone(), ty: ty.clone(), default: d.as_ref().map(|t| t.form()) }
        })
        .collect();
    let visible: Vec<String> = inputs.iter().map(|i| i.name.clone()).collect();
    Ok(scope_of(&c, folded, false, &visible, inputs, &root, &last(&g.form)))
}

pub fn of_graph(catalog: &Catalog, workspace: &w::Workspace, name: &str) -> Result<Scope, String> {
    of_graph_with(&|_| true, catalog, workspace, name)
}

/// Every zone below the scope, outer first.
pub fn zones(sc: &Scope) -> Vec<Node> {
    let mut out = Vec::new();
    for n in &sc.nodes {
        if let Some(z) = &n.zone {
            out.push(n.clone());
            out.extend(zones(&z.scope));
        }
    }
    out
}

/// The node at a path, searching zones.
pub fn find(sc: &Scope, path: &[Symbol]) -> Option<Node> {
    for n in &sc.nodes {
        if n.path == path {
            return Some(n.clone());
        }
        if let Some(z) = &n.zone {
            if let Some(f) = find(&z.scope, path) {
                return Some(f);
            }
        }
    }
    None
}

// ---- levels and exposure ----

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Level {
    Point,
    Chip,
    Card,
    Full,
}

pub fn level_name(l: Level) -> &'static str {
    match l {
        Level::Point => "point",
        Level::Chip => "chip",
        Level::Card => "card",
        Level::Full => "full",
    }
}

pub fn level_of_name(n: &str) -> Option<Level> {
    match n {
        "point" => Some(Level::Point),
        "chip" => Some(Level::Chip),
        "card" => Some(Level::Card),
        "full" => Some(Level::Full),
        _ => None,
    }
}

/// A literal binding: a header-only card with the value in a field.
pub fn value_card(n: &Node) -> bool {
    n.zone.is_none() && n.macro_.is_none() && !n.synthetic && matches!(n.rows.as_slice(), [Row { key: ArgKey::Whole, chip: Chip::Const, .. }])
}

#[derive(Clone, Debug, PartialEq)]
pub enum Line {
    /// the n-th folder label row of a `Full` card
    Folder(i64, String),
    /// `n.rows[i]`
    Row(usize, Row),
}

fn driven(r: &Row) -> bool {
    match r.chip {
        Chip::Name(_) | Chip::Inline { .. } => true,
        _ => !sources(r).is_empty(),
    }
}

/// The exposure rule of flow.md 5.1 for one row; structural rows always show.
pub fn row_shown(pin: Option<bool>, r: &Row) -> bool {
    match r.kind {
        RowKind::Rest | RowKind::Hole | RowKind::Binder => true,
        RowKind::Add => false,
        RowKind::Arg | RowKind::GroupReader | RowKind::GroupWriter => {
            exposure::shown(&exposure::Facts { slot: false, driven: driven(r), pin, differs: r.expr.is_some(), primary: false })
        }
    }
}

/// The body of a card at a level, in order; `pin` is a row's pin by label (`layout.rows`).
pub fn lines(pin: &dyn Fn(&str) -> Option<bool>, level: Level, n: &Node) -> Vec<Line> {
    if n.zone.is_some() || value_card(n) {
        return Vec::new();
    }
    let body: Vec<(usize, &Row)> = n.rows.iter().enumerate().filter(|(_, r)| !r.head).collect();
    match level {
        Level::Point | Level::Chip => Vec::new(),
        Level::Full => {
            // every section is labelled, in the order its first row appears, and holds all its rows; the
            // slots (no folder) stay above the first label
            let mut seen: Vec<String> = Vec::new();
            for (_, r) in &body {
                if !seen.contains(&r.folder) {
                    seen.push(r.folder.clone());
                }
            }
            let rank = |r: &Row| seen.iter().position(|f| *f == r.folder).unwrap_or(0);
            let mut body = body;
            body.sort_by_key(|(_, r)| rank(r));
            let mut folders = 0;
            let mut current = String::new();
            let mut out = Vec::new();
            for (i, r) in body {
                if !r.folder.is_empty() && r.folder != current {
                    current = r.folder.clone();
                    folders += 1;
                    out.push(Line::Folder(folders, r.folder.clone()));
                }
                out.push(Line::Row(i, r.clone()));
            }
            out
        }
        Level::Card => {
            let has_head = n.rows.iter().any(|r| r.head);
            // a call with no header slot (a `list`, a record) keeps its `+` row on the card
            body.into_iter().filter(|(_, r)| (r.kind == RowKind::Add && !has_head) || row_shown(pin(&r.label), r)).map(|(i, r)| Line::Row(i, r.clone())).collect()
        }
    }
}

/// What the chip's `+N` says: the rows with something written.
pub fn set_count(n: &Node) -> usize {
    n.rows.iter().filter(|r| !r.head && r.expr.is_some() && r.kind != RowKind::Add).count()
}

// ---- layout ----

pub const ROW_HEIGHT: f64 = 24.0;
pub const HEAD_HEIGHT: f64 = 24.0;
pub const FOOT_HEIGHT: f64 = 24.0;
/// Where a card's rows start: one point above the header's bottom (kit rev 3).
pub const BODY_TOP: f64 = 23.0;
/// The 4 points of padding and the border under a card's last row.
pub const CARD_PAD: f64 = 5.0;
pub const NODE_WIDTH: f64 = 196.0;
/// The dot grid: cards and columns sit on it.
pub const LATTICE: f64 = 24.0;
pub const POINT_SIZE: f64 = 14.0;
pub const ZONE_PAD_X: f64 = 48.0;
const ZONE_PAD_TOP: f64 = 52.0;
const ZONE_PAD_BOTTOM: f64 = 8.0;
const COLUMN_GAP: f64 = 92.0;
const ROW_GAP: f64 = 36.0;
pub const LENS_WIDTH: f64 = 400.0;

pub fn row_height() -> f64 {
    ROW_HEIGHT
}
pub fn head_height() -> f64 {
    HEAD_HEIGHT
}
pub fn foot_height() -> f64 {
    FOOT_HEIGHT
}
pub fn body_top() -> f64 {
    BODY_TOP
}
pub fn card_pad() -> f64 {
    CARD_PAD
}
pub fn node_width() -> f64 {
    NODE_WIDTH
}
pub fn lattice() -> f64 {
    LATTICE
}
pub fn point_size() -> f64 {
    POINT_SIZE
}
pub fn zone_pad_x() -> f64 {
    ZONE_PAD_X
}
pub fn lens_width() -> f64 {
    LENS_WIDTH
}

pub fn snap(v: f64) -> f64 {
    (v / LATTICE).round() * LATTICE
}
fn ceil_lattice(v: f64) -> f64 {
    ((v / LATTICE) - 1e-9).ceil() * LATTICE
}
fn floor_lattice(v: f64) -> f64 {
    ((v / LATTICE) + 1e-9).floor() * LATTICE
}

#[derive(Clone, Debug, PartialEq)]
pub enum Item {
    Input(Input),
    Item(Node),
    Return,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Placed {
    pub item: Item,
    pub path: Path,
    /// relative to the scope's origin
    pub x: f64,
    pub y: f64,
    pub w: f64,
    pub h: f64,
    pub collapsed: bool,
    /// an expanded zone's own layout
    pub inner: Option<Layout>,
    /// the requested level
    pub level: Level,
    /// the card's body at that level
    pub lines: Vec<Line>,
    /// the level it is drawn at
    pub shown: Level,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Layout {
    pub placed: Vec<Placed>,
    pub w: f64,
    pub h: f64,
}

/// A zone's pad on the left: a function's parameters leave as plain wires, which need room to bend.
pub fn zone_pad_in(z: &Zone) -> f64 {
    if z.kind == ZoneKind::Fn { 96.0 } else { ZONE_PAD_X }
}

/// The rail row the zone's label row shows: its first loop variable.
pub fn label_row(z: &Zone) -> Option<RailRow> {
    z.rail.iter().find(|r| r.role == Role::Var).cloned()
}

/// The rail rows drawn under the label row (accumulators, further variables, parameters).
pub fn extra_rails(z: &Zone) -> Vec<RailRow> {
    let label = z.rail.iter().position(|r| r.role == Role::Var);
    z.rail.iter().enumerate().filter(|(i, r)| r.role != Role::Capture && Some(*i) != label).map(|(_, r)| r.clone()).collect()
}

/// Where a zone's cards start below its top.
pub fn rail_top(n: &Node) -> f64 {
    match &n.zone {
        Some(z) => ZONE_PAD_TOP + extra_rails(z).len() as f64 * ROW_HEIGHT,
        None => ZONE_PAD_TOP,
    }
}

/// The expansion panel under a macro call.
pub fn lens_height(l: &Lens, step: i64) -> f64 {
    let shown: &str = if step >= l.steps.len() as i64 { &l.template } else { &l.steps[step.max(0) as usize] };
    let lines = if l.error.is_some() && step < l.steps.len() as i64 {
        2
    } else {
        (1 + shown.bytes().filter(|c| *c == b'\n').count()).min(16)
    };
    ROW_HEIGHT + 10.0 + lines as f64 * 20.0 + ROW_HEIGHT
}

/// A card: the header alone, or the rows below it, `extra` points of footer or panel and the padding.
pub fn card_height(rows: f64, extra: f64) -> f64 {
    if rows == 0.0 && extra == 0.0 { HEAD_HEIGHT } else { BODY_TOP + rows * ROW_HEIGHT + extra + CARD_PAD }
}

/// A point's box: the disc and the name beside it.
pub fn point_title(n: &Node) -> String {
    if n.synthetic {
        "result".to_owned()
    } else if anonymous(n) {
        "node".to_owned()
    } else {
        n.name.clone()
    }
}

fn point_width(n: &Node) -> f64 {
    POINT_SIZE + 8.0 + 7.0 * point_title(n).len() as f64
}

/// The hooks a layout asks the host (all optional in the OCaml; defaults here).
#[derive(Clone)]
pub struct LayoutOptions {
    /// reserve a footer row under every card and zone
    pub foot: bool,
    /// overrides a node's position
    pub at: Rc<dyn Fn(&[Symbol]) -> Option<(f64, f64)>>,
    pub prev: Rc<dyn Fn(&[Symbol]) -> Option<(f64, f64, f64, f64)>>,
    /// folds a zone to its card
    pub collapsed: Rc<dyn Fn(&[Symbol]) -> bool>,
    /// the step of a macro call whose expansion panel is open
    pub lens: Rc<dyn Fn(&[Symbol]) -> Option<i64>>,
    pub level: Rc<dyn Fn(&[Symbol]) -> Level>,
    pub pin: Rc<dyn Fn(&[Symbol], &str) -> Option<bool>>,
    pub inner: bool,
}

impl LayoutOptions {
    pub fn new() -> LayoutOptions {
        LayoutOptions {
            foot: false,
            at: Rc::new(|_| None),
            prev: Rc::new(|_| None),
            collapsed: Rc::new(|_| false),
            lens: Rc::new(|_| None),
            level: Rc::new(|_| Level::Card),
            pin: Rc::new(|_, _| None),
            inner: false,
        }
    }
}

impl Default for LayoutOptions {
    fn default() -> Self {
        LayoutOptions::new()
    }
}

struct Sized {
    w: f64,
    h: f64,
    collapsed: bool,
    inner: Option<Layout>,
    level: Level,
    lines: Vec<Line>,
}

fn size(o: &LayoutOptions, it: &Item) -> Sized {
    match it {
        Item::Input(_) => Sized { w: NODE_WIDTH, h: HEAD_HEIGHT, collapsed: false, inner: None, level: Level::Card, lines: Vec::new() },
        Item::Return => Sized { w: NODE_WIDTH - 40.0, h: card_height(1.0, 0.0), collapsed: false, inner: None, level: Level::Card, lines: Vec::new() },
        Item::Item(n) if n.zone.is_some() => {
            let z = n.zone.as_ref().unwrap();
            let rail = z.rail.len() as f64;
            if (o.collapsed)(&n.path) {
                Sized { w: NODE_WIDTH, h: card_height(rail.max(1.0), if o.foot { FOOT_HEIGHT } else { 0.0 }), collapsed: true, inner: None, level: Level::Card, lines: Vec::new() }
            } else {
                let mut io = o.clone();
                io.inner = true;
                let l = layout(&io, &z.scope);
                Sized {
                    w: zone_pad_in(z) + l.w.max(72.0) + ZONE_PAD_X,
                    h: rail_top(n) + l.h.max(ROW_HEIGHT) + ZONE_PAD_BOTTOM,
                    collapsed: false,
                    inner: Some(l),
                    level: Level::Card,
                    lines: Vec::new(),
                }
            }
        }
        Item::Item(n) => {
            let lvl = if value_card(n) { Level::Card } else { (o.level)(&n.path) };
            let path = n.path.clone();
            let pin = o.pin.clone();
            let ln = lines(&move |label| pin(&path, label), lvl, n);
            let open_lens = match (&n.lens, (o.lens)(&n.path)) {
                (Some(l), Some(step)) => Some((l.clone(), step)),
                _ => None,
            };
            match lvl {
                Level::Point => Sized { w: point_width(n), h: POINT_SIZE, collapsed: false, inner: None, level: lvl, lines: ln },
                Level::Chip => Sized { w: NODE_WIDTH, h: HEAD_HEIGHT, collapsed: false, inner: None, level: lvl, lines: ln },
                Level::Card | Level::Full => Sized {
                    w: if open_lens.is_some() { NODE_WIDTH.max(LENS_WIDTH) } else { NODE_WIDTH },
                    h: card_height(
                        (if n.note.is_some() { 1.0 } else { 0.0 }) + ln.len() as f64 + n.outputs.len() as f64,
                        (if o.foot && lvl == Level::Full { FOOT_HEIGHT } else { 0.0 }) + open_lens.as_ref().map(|(l, step)| lens_height(l, *step)).unwrap_or(0.0),
                    ),
                    collapsed: false,
                    inner: None,
                    level: lvl,
                    lines: ln,
                },
            }
        }
    }
}

struct LItem {
    item: Item,
    path: Path,
    deps: Vec<String>,
    names: Vec<String>,
    head_src: Option<String>,
}

fn hit(a: (f64, f64, f64, f64), b: (f64, f64, f64, f64)) -> bool {
    a.0 < b.0 + b.2 - 1e-6 && b.0 < a.0 + a.2 - 1e-6 && a.1 < b.1 + b.3 - 1e-6 && b.1 < a.1 + a.3 - 1e-6
}

// ponytail: quadratic scan, fine for the hundreds of cards of a scope; use the int grid beyond that
fn clear(boxes: &mut [Placed], solid: &dyn Fn(usize) -> bool, i: usize) {
    let n = boxes.len();
    let mut j = 0;
    let mut y = boxes[i].y;
    while j < n {
        if j != i && solid(j) && hit((boxes[i].x, y, boxes[i].w, boxes[i].h), (boxes[j].x, boxes[j].y, boxes[j].w, boxes[j].h)) {
            y = ceil_lattice(boxes[j].y + boxes[j].h + ROW_GAP);
            j = 0;
        } else {
            j += 1;
        }
    }
    boxes[i].y = y;
}

/// Columns by dependency depth, inputs first and the return last; a node stacks below its column's
/// previous one. ponytail: no crossing minimisation (like `automatic_layout`).
pub fn layout(o: &LayoutOptions, sc: &Scope) -> Layout {
    let inner = o.inner;
    // the return card only when the result is a literal: otherwise the displayed node is the result
    let with_return = !sc.inputs.is_empty() && matches!(sc.result, ScopeResult::Literal(_));
    let mut arr: Vec<LItem> = Vec::new();
    for i in &sc.inputs {
        arr.push(LItem { item: Item::Input(i.clone()), path: i.path.clone(), deps: Vec::new(), names: vec![i.name.clone()], head_src: None });
    }
    for n in &sc.nodes {
        let mut deps: Vec<String> = n.rows.iter().flat_map(sources).collect();
        if let Some(z) = &n.zone {
            for r in &z.rail {
                match (r.role, &r.expr) {
                    (Role::Capture, _) => deps.push(r.name.clone()),
                    (_, Some(e)) => deps.extend(eb::free_names(e)),
                    _ => {}
                }
            }
        }
        // the node the header's in-port reads, which the card lines up with
        let head_src = n.rows.iter().find_map(|r| if r.head { sources(r).first().map(|a| root_of(a).to_owned()) } else { None });
        arr.push(LItem { item: Item::Item(n.clone()), path: n.path.clone(), deps, names: n.binds.clone(), head_src });
    }
    if with_return {
        let mut p = sc.path.clone();
        p.push("@return".into());
        arr.push(LItem { item: Item::Return, path: p, deps: Vec::new(), names: Vec::new(), head_src: None });
    }
    let count = arr.len();
    let mut by_name: HashMap<String, usize> = HashMap::new();
    for (k, it) in arr.iter().enumerate() {
        for n in &it.names {
            by_name.insert(n.clone(), k);
        }
    }
    let mut lvl: Vec<i64> = vec![-1; count];
    fn lv(k: usize, arr: &[LItem], by_name: &HashMap<String, usize>, lvl: &mut Vec<i64>) -> i64 {
        if lvl[k] >= 0 {
            return lvl[k];
        }
        lvl[k] = 0;
        let mut l = 0;
        for d in &arr[k].deps {
            if let Some(j) = by_name.get(root_of(d)) {
                if *j != k {
                    l = l.max(lv(*j, arr, by_name, lvl) + 1);
                }
            }
        }
        lvl[k] = l;
        l
    }
    for k in 0..count {
        lv(k, &arr, &by_name, &mut lvl);
    }
    // a chain is a column: the first node call a card reads in a positional input is written in it, so it
    // stacks right above it, the source on top.
    for k in (0..count).rev() {
        if let Item::Item(n) = &arr[k].item {
            let leaf = n.rows.iter().find_map(|r| match (&r.chip, &r.key) {
                (Chip::Name(leaf), ArgKey::Pos(_)) if eb::nested(leaf) && r.kind == RowKind::Arg => Some(leaf.clone()),
                _ => None,
            });
            if let Some(leaf) = leaf {
                if let Some(j) = by_name.get(&leaf) {
                    if *j < k && matches!(&arr[*j].item, Item::Item(s2) if !s2.expr) {
                        lvl[*j] = lvl[k];
                    }
                }
            }
        }
    }
    // a graph input sits one column left of its first reader, not at the far left
    for k in 0..count {
        if let Item::Input(_) = &arr[k].item {
            let mut first = i64::MAX;
            for j in 0..count {
                if j != k && arr[j].deps.iter().any(|d| arr[k].names.iter().any(|nm| nm == root_of(d))) {
                    first = first.min(lvl[j]);
                }
            }
            if first < i64::MAX {
                lvl[k] = 0.max(first - 1);
            }
        }
    }
    let cols = (lvl.iter().copied().fold(0, i64::max) + 1) as usize;
    // column 0 is ordered by the first item that reads each of its items
    let mut consumer: Vec<usize> = vec![usize::MAX; count];
    for j in 0..count {
        for d in &arr[j].deps {
            if let Some(k) = by_name.get(root_of(d)) {
                if *k != j && j < consumer[*k] {
                    consumer[*k] = j;
                }
            }
        }
    }
    // the graph's inputs stack in the order they are written, each no earlier than the one before
    let mut last_c = 0usize;
    for k in 0..count {
        if let Item::Input(_) = &arr[k].item {
            last_c = last_c.max(consumer[k]);
            consumer[k] = last_c;
        }
    }
    let order = |k: usize| (consumer[k], if matches!(arr[k].item, Item::Input(_)) { 1 } else { 0 });
    // the items of each column, in item order
    let mut by_level: Vec<Vec<usize>> = vec![Vec::new(); cols];
    for k in (0..count).rev() {
        by_level[lvl[k] as usize].insert(0, k);
    }
    let (origin_x, origin_y) = if inner { (0.0, 0.0) } else { (LATTICE, LATTICE) };
    // a zone's cards sit on the lattice row of the cards beside it
    let lead_of = |it: &Item| -> f64 {
        match it {
            Item::Item(n) if n.zone.is_some() && !inner && !(o.collapsed)(&n.path) => rail_top(n),
            _ => 0.0,
        }
    };
    let row_start = if inner { origin_y } else { floor_lattice(origin_y + arr.iter().map(|it| lead_of(&it.item)).fold(0.0, f64::max)) };
    // an edit that adds or removes a few nodes keeps every other node where it was
    let sticky = !inner && count <= 600 && {
        let mut known = 0;
        let mut fresh = 0;
        for it in &arr {
            if (o.at)(&it.path).is_some() || (o.prev)(&it.path).is_some() {
                known += 1;
            } else {
                fresh += 1;
            }
        }
        known > 0 && fresh <= 3
    };
    let mut fresh_items: Vec<usize> = Vec::new();
    let mut placed: Vec<Placed> = Vec::new();
    let mut x = origin_x;
    let mut pos_of: HashMap<String, f64> = HashMap::new();
    for l in 0..cols {
        let mut y = row_start;
        let mut cw = 0.0f64;
        let mut first = true;
        let mut members = by_level[l].clone();
        if l == 0 {
            members.sort_by_key(|k| order(*k));
        }
        for k in members {
            let it = &arr[k];
            let sz = size(o, &it.item);
            let lead = lead_of(&it.item);
            // the card it lines up with: its deepest source (the main chain), the header's on a tie
            let mut best: Option<(String, i64)> = None;
            for d in &it.deps {
                if let Some(j) = by_name.get(root_of(d)) {
                    if *j != k && (best.is_none() || lvl[*j] > best.as_ref().unwrap().1) {
                        best = Some((root_of(d).to_owned(), lvl[*j]));
                    }
                }
            }
            let head: Option<String> = match &it.head_src {
                Some(name) => match by_name.get(name) {
                    Some(j) if best.as_ref().map(|b| lvl[*j] >= b.1).unwrap_or(true) => Some(name.clone()),
                    _ => None,
                },
                None => None,
            };
            let zone_item = matches!(&it.item, Item::Item(n) if n.zone.is_some());
            let aligned: Option<f64> = if lead > 0.0 || zone_item {
                // a zone lines its first card up with the topmost of its deepest sources
                let mut rows: Vec<f64> = Vec::new();
                for d in &it.deps {
                    if let Some(j) = by_name.get(root_of(d)) {
                        if *j != k && best.as_ref().map(|b| lvl[*j] == b.1).unwrap_or(false) {
                            if let Some(v) = pos_of.get(root_of(d)) {
                                rows.push(*v);
                            }
                        }
                    }
                }
                rows.split_first().map(|(r, rest)| rest.iter().fold(*r, |a, b| a.min(*b)))
            } else {
                match (&head, &best) {
                    (Some(name), _) => pos_of.get(name).copied(),
                    (None, Some((name, _))) => pos_of.get(name).copied(),
                    (None, None) => None,
                }
            };
            let row_min = if first { y } else { floor_lattice(y + lead) };
            let row = match aligned {
                Some(sr) if sr > row_min => sr,
                _ => row_min,
            };
            let (px, py) = match (o.at)(&it.path) {
                Some((ax, ay)) => (ax, ay),
                None => match if sticky { (o.prev)(&it.path) } else { None } {
                    Some((ax, ay, _, _)) => (ax, ay),
                    None => {
                        if sticky {
                            fresh_items.insert(0, k);
                        }
                        (x, row - lead)
                    }
                },
            };
            let row = py + lead;
            first = false;
            placed.push(Placed {
                item: it.item.clone(),
                path: it.path.clone(),
                x: px,
                y: py,
                w: sz.w,
                h: sz.h,
                collapsed: sz.collapsed,
                inner: sz.inner,
                level: sz.level,
                lines: sz.lines,
                shown: sz.level,
            });
            for nm in &it.names {
                pos_of.insert(nm.clone(), row);
            }
            y = ceil_lattice(y.max(py + sz.h + ROW_GAP));
            cw = cw.max(sz.w);
        }
        if cw > 0.0 {
            x = ceil_lattice(x + cw + COLUMN_GAP);
        }
    }
    let placed = if !sticky {
        placed
    } else {
        let mut boxes = placed;
        let n = boxes.len();
        let mut index: HashMap<Path, usize> = HashMap::new();
        for (i, b) in boxes.iter().enumerate() {
            index.insert(b.path.clone(), i);
        }
        // a new node goes one column left of the first node that reads it, on that node's row
        for k in fresh_items.iter().rev() {
            let p = &arr[*k].path;
            if let Some(&i) = index.get(p) {
                let consumer_box = if consumer[*k] == usize::MAX { None } else { index.get(&arr[consumer[*k]].path).copied() };
                if let Some(o2) = consumer_box {
                    boxes[i].x = boxes[o2].x - boxes[i].w - COLUMN_GAP;
                    boxes[i].y = boxes[o2].y;
                }
                clear(&mut boxes, &|_| true, i);
            }
        }
        // a card that grew (a footer, a level) pushes the one it now covers down, and that one the next; never a dragged one
        let mut ord: Vec<usize> = (0..n).collect();
        ord.sort_by(|a, b| {
            boxes[*a].y.total_cmp(&boxes[*b].y).then(boxes[*a].x.total_cmp(&boxes[*b].x))
        });
        let mut grew: Vec<bool> = boxes.iter().map(|b| match (o.prev)(&b.path) { Some((_, _, pw, ph)) => pw != b.w || ph != b.h, None => false }).collect();
        let mut seen: Vec<bool> = vec![false; n];
        for i in ord {
            if (o.at)(&boxes[i].path).is_none() {
                let y = boxes[i].y;
                let solid: Vec<bool> = (0..n).map(|j| seen[j] && grew[j]).collect();
                clear(&mut boxes, &|j| solid[j], i);
                if boxes[i].y != y {
                    grew[i] = true;
                }
            }
            seen[i] = true;
        }
        boxes
    };
    let w = placed.iter().fold(0.0, |m: f64, b| m.max(b.x + b.w));
    let h = placed.iter().fold(0.0, |m: f64, b| m.max(b.y + b.h));
    let margin = if inner { 0.0 } else { LATTICE };
    Layout { placed, w: w + margin, h: h + margin }
}
