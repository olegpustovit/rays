//! Port of lib/editor_document/workspace_doc.ml: the workspace document: a checked workspace, its
//! layout keyed by path and the sketch settings. UI-free. The source text is the authored truth;
//! `checked` is derived from it and every edit validates atomically.
//!
//! The persisted text is the workspace form itself followed by optional `(layout ...)` and
//! `(settings :name value ...)` forms (only settings that differ from their default).
use crate::layout_by_path::{self as lbp, PathMap};
use crate::layout_edit;
use crate::literal_edit::{self, Change};
use crate::settings;
use flow_check::catalog::Catalog;
use flow_check::op::Op as FlowOp;
use flow_check::workspace::{self as w, Workspace};
use flow_eval::Plan;
use flow_graph::flow_edit::{self, ArgKey, Op};
use flow_graph::projection;
use flow_sop_catalog::catalog as fsc;
use flow_syntax::context::{self, Context};
use flow_syntax::lisp;
use flow_syntax::syntax::{self as s, Node, Syntax};
use flow_syntax::arena::{Draft, SyntaxBuilder, SyntaxId};
use flow_syntax::value::Value as E;
use param::Value as PV;
use rays_diagnostic::{Diagnostic, Span};
use std::collections::HashSet;
use std::sync::Arc;

type S = Syntax;
type R<T> = Result<T, Diagnostic>;

/// A span/term patch since `base`, bounded to 4,096 distinct catalog arguments. It is derived
/// data; it is never persisted.
#[derive(Clone)]
pub struct Literal {
    pub base: Arc<Workspace>,
    pub edits: PathMap<Change>,
}

pub type Inputs = Vec<(String, Vec<(String, E)>)>;

#[derive(Clone)]
pub struct T {
    /// The one `(workspace ...)` form.
    pub source: Vec<S>,
    pub checked: Arc<Workspace>,
    pub layout: lbp::T,
    pub settings: settings::T,
    /// The other root forms as they were written, for `to_text`.
    pub extra: Vec<S>,
    /// Host-supplied graph inputs; retained across edits and reloads, validated by lowering,
    /// never written into the source.
    pub inputs: Arc<Inputs>,
    pub literal: Option<Literal>,
    /// Read-only children spliced from each relative imported file.
    pub imports: Vec<(String, Vec<S>)>,
    /// Original imported texts, retained for reload digests and rechecking.
    pub import_sources: Vec<(String, String)>,
    /// A library of bare graph, definition or macro forms, printed without a workspace wrapper.
    pub fragment: bool,
}

/// Allocation identities for retained source/checked-term storage, excluding other editor owners.
/// The caller must keep this document alive for as long as the recorded identities are visited.
pub fn memory_allocations(value: &T) -> (Vec<(usize, usize)>, Vec<(usize, usize)>) {
    let mut memory = w::Memory::default();
    memory.syntax.forms(&value.source);
    memory.syntax.forms(&value.extra);
    memory.workspace(&value.checked);
    if let Some(literal) = &value.literal { memory.workspace(&literal.base); }
    memory.syntax.allocation(value.imports.as_ptr(), value.imports.capacity() * std::mem::size_of::<(String, Vec<S>)>());
    for (path, forms) in &value.imports {
        memory.syntax.allocation(path.as_ptr(), path.capacity());
        memory.syntax.forms(forms);
    }
    memory.syntax.allocation(value.import_sources.as_ptr(), value.import_sources.capacity() * std::mem::size_of::<(String, String)>());
    for (path, text) in &value.import_sources {
        memory.syntax.allocation(path.as_ptr(), path.capacity());
        memory.syntax.allocation(text.as_ptr(), text.capacity());
    }
    memory.into_allocations()
}

pub fn name(t: &T) -> String {
    t.checked.name.clone()
}

/// Changes on one literal-patch lineage, including several edits between frames. `None` means
/// that structural checking/lowering must run.
pub fn literal_changes(previous: &T, next: &T) -> Option<Vec<Change>> {
    if !Arc::ptr_eq(&previous.inputs, &next.inputs) {
        return None;
    }
    if Arc::ptr_eq(&previous.checked, &next.checked) {
        return Some(Vec::new());
    }
    let literal = next.literal.as_ref()?;
    let before: PathMap<Change> = if Arc::ptr_eq(&previous.checked, &literal.base) {
        PathMap::new()
    } else {
        let old = previous.literal.as_ref()?;
        if Arc::ptr_eq(&old.base, &literal.base) { old.edits.clone() } else { return None }
    };
    let mut keys: Vec<&Vec<String>> = before.keys().chain(literal.edits.keys()).collect();
    keys.sort();
    keys.dedup();
    let mut edits: Vec<Change> = Vec::new();
    for key in keys {
        match (before.get(key), literal.edits.get(key)) {
            (Some(old), Some(change)) if old.after == change.after && old.expr.node == change.expr.node => {}
            (old, Some(change)) => {
                edits.push(Change { before: old.map_or_else(|| change.before.clone(), |o| o.after.clone()), ..change.clone() });
            }
            (Some(old), None) => {
                if let Some(expr) = flow_edit::arg_text(&next.source, &old.path, &ArgKey::Kw(old.field.clone())) {
                    edits.push(Change { before: old.after.clone(), after: old.before.clone(), expr, ..old.clone() });
                }
            }
            (None, None) => {}
        }
    }
    Some(edits)
}

/// The selected named editor layout, or the first editor graph when none is selected.
pub fn editor_graph_ref<'a>(t: &'a T) -> Option<&'a w::Graph> {
    t.checked.graphs.iter().find(|g| {
        g.context == context::editor() && t.layout.editor.as_ref().map_or(true, |e| *e == g.name)
    })
}

pub fn editor_graph(t: &T) -> Option<w::Graph> {
    editor_graph_ref(t).cloned()
}

fn head(f: &S) -> Option<&str> {
    s::head(f)
}

fn diag(span: Option<Span>, code: &str, message: &str) -> Diagnostic {
    rays_diagnostic::error(code.to_owned(), message.to_owned(), span, None, None, Vec::new(), Vec::new())
}

fn is_relative(path: &[u8]) -> bool {
    let p = String::from_utf8_lossy(path);
    !(p.starts_with('/') || p.starts_with('\\') || p.as_bytes().get(1) == Some(&b':'))
}

pub fn import_paths(text: &str) -> Result<Vec<String>, Vec<Diagnostic>> {
    let forms = s::parse(text).map_err(|d| vec![d])?;
    let mut paths = Vec::new();
    for f in &forms {
        if head(f) == Some("import") {
            match f.children() {
                [_, path] => match f.arena().row(*path).node {
                    Node::Str(bytes) if is_relative(f.bytes(bytes)) => paths.push(String::from_utf8_lossy(f.bytes(bytes)).into_owned()),
                    _ => return Err(vec![diag(Some(f.span.clone()), "E_IMPORT", "Import takes one relative file path.")]),
                },
                _ => return Err(vec![diag(Some(f.span.clone()), "E_IMPORT", "Import takes one relative file path.")]),
            }
        }
    }
    Ok(paths)
}

fn second_name(f: &S) -> Option<String> {
    match f.children().get(1).map(|id| f.arena().row(*id).node) {
        Some(Node::Sym(name)) => Some(f.name(name).to_owned()),
        _ => None,
    }
}

pub fn imported_file(imports: &[(String, Vec<S>)], path: &[String]) -> Option<String> {
    let root = path.first()?;
    imports.iter().find_map(|(file, forms)| {
        if forms.iter().any(|f| match second_name(f) {
            Some(name) => *root == name || *root == format!("def:{name}"),
            None => false,
        }) {
            Some(file.clone())
        } else {
            None
        }
    })
}

fn authored_source(t: &T) -> Vec<S> {
    let names: Vec<String> = t.imports.iter().flat_map(|(_, forms)| forms.iter().filter_map(second_name)).collect();
    if names.is_empty() { return t.source.clone(); }
    let mut builder = SyntaxBuilder::new();
    for form in t.source.iter() { builder.import(form); }
    let roots = t.source.iter().map(|form| match form.node {
        Node::List(range) if form.range(range).len() >= 2 => {
            let items = form.range(range);
            let mut children = Vec::with_capacity(items.len());
            for (index, id) in items.iter().enumerate() {
                let child = form.child(*id);
                if index < 2 || second_name(&child).is_none_or(|name| !names.contains(&name)) {
                    children.push(builder.imported(&child).expect("source imported once"));
                }
            }
            builder.with_node(form, Draft::List(children))
        }
        _ => builder.imported(form).expect("source imported once"),
    }).collect::<Vec<_>>();
    let arena = builder.freeze();
    roots.into_iter().map(|id| arena.handle(id)).collect()
}

// ---- settings: (settings :name value ...), only non-default fields ----

fn settings_form(st: &settings::T) -> Option<S> {
    let fields: Vec<param::FieldView> = settings::fields(st).into_iter().filter(|f| f.current != f.default).collect();
    if fields.is_empty() {
        return None;
    }
    let mut builder = SyntaxBuilder::new();
    let span = Span { start: 0, finish: 0 };
    let mut items = vec![builder.push_draft(0, Draft::Sym("settings".to_owned()), span.clone(), None)];
    for f in &fields {
        items.push(builder.push_draft(0, Draft::Kw(f.name.clone()), span.clone(), None));
        let value = match &f.current {
            PV::Bool(b) => Draft::Sym(if *b { "true" } else { "false" }.to_owned()),
            PV::Int(i) => Draft::Num(i.to_string()),
            PV::Float(x) => Draft::Num(lisp::float(*x)),
            PV::Text(bytes) => Draft::Str(bytes.clone()),
            PV::Choice(choice) => Draft::Str(choice.as_bytes().to_vec()),
        };
        items.push(builder.push_draft(0, value, span.clone(), None));
    }
    let root = builder.push_draft(0, Draft::List(items), span, None);
    Some(builder.freeze().handle(root))
}

fn read_settings(base: &settings::T, form: &S) -> Result<settings::T, String> {
    let known = settings::fields(base);
    let mut changes: Vec<(String, PV)> = Vec::new();
    let mut rest = form.children().get(1..).unwrap_or(&[]);
    loop {
        match rest {
            [] => break,
            [key, value, tail @ ..] if matches!(form.arena().row(*key).node, Node::Kw(_)) => {
                let Node::Kw(key) = form.arena().row(*key).node else { unreachable!() };
                let key = form.name(key);
                let field = known.iter().find(|f| f.name == key).ok_or_else(|| format!("unknown setting {key}"))?;
                let wrong = || format!("setting {key} has the wrong type");
                let value = match (&field.current, form.arena().row(*value).node) {
                    (PV::Bool(_), Node::Sym(text)) if form.name(text) == "true" => PV::Bool(true),
                    (PV::Bool(_), Node::Sym(text)) if form.name(text) == "false" => PV::Bool(false),
                    (PV::Int(_), Node::Num(text)) => PV::Int(form.name(text).parse::<i64>().map_err(|_| wrong())?),
                    (PV::Float(_), Node::Num(text)) => PV::Float(cfmt::parse_ocaml_float(form.name(text)).ok_or_else(wrong)?),
                    (PV::Text(_), Node::Str(bytes)) => PV::Text(form.bytes(bytes).to_vec()),
                    (PV::Choice(_), Node::Str(bytes)) => PV::Choice(String::from_utf8_lossy(form.bytes(bytes)).into_owned()),
                    _ => return Err(wrong()),
                };
                changes.push((key.to_owned(), value));
                rest = tail;
            }
            _ => return Err("expected :name value pairs".to_owned()),
        }
    }
    settings::apply(&settings::defaults(base), &changes).map(|(st, _)| st)
}

// ---- text ----

fn check_forms(forms: &[S]) -> Result<(), Vec<Diagnostic>> {
    let mut seen: Vec<&str> = Vec::new();
    for f in forms {
        match head(f) {
            Some(name) if ["workspace", "layout", "settings", "view"].contains(&name) => {
                if seen.contains(&name) {
                    return Err(vec![diag(Some(f.span.clone()), "E_DOCUMENT_FORM", &format!("Duplicate {name} form."))]);
                }
                seen.push(name);
            }
            Some("import") => {}
            _ => {
                return Err(vec![diag(Some(f.span.clone()), "E_DOCUMENT_FORM", "Expected workspace, layout, settings or view at the document root.")]);
            }
        }
    }
    Ok(())
}

/// Layout entries live as long as what they name: a key whose node, input or scope is not in the
/// checked workspace is dropped, and so is a selected editor layout that is not a graph.
fn prune(checked: &Workspace, catalog: &Catalog, layout: &lbp::T) -> lbp::T {
    if lbp::is_empty(layout) {
        return layout.clone();
    }
    let mut known: HashSet<Vec<String>> = HashSet::new();
    fn scope(known: &mut HashSet<Vec<String>>, sc: &projection::Scope) {
        known.insert(sc.path.clone());
        for i in &sc.inputs {
            known.insert(i.path.clone());
        }
        for n in &sc.nodes {
            known.insert(n.path.clone());
            if let Some(z) = &n.zone {
                scope(known, &z.scope);
            }
        }
    }
    for g in checked.graphs.iter() {
        match projection::of_graph_with(&|_| false, catalog, checked, &g.name) {
            Ok(sc) => scope(&mut known, &sc),
            Err(_) => return layout.clone(),
        }
    }
    for g in checked.defs.iter() {
        match projection::of_graph_with(&|_| false, catalog, checked, &format!("def:{}", g.name)) {
            Ok(sc) => scope(&mut known, &sc),
            Err(_) => return layout.clone(),
        }
    }
    // a scope's own pseudo items ([@result], [@panel n]) are keyed under its path
    let keep = |path: &Vec<String>| -> bool {
        let mut before: Vec<String> = Vec::new();
        for segment in path {
            if segment.starts_with('@') {
                return known.contains(&before);
            }
            before.push(segment.clone());
        }
        known.contains(&before)
    };
    lbp::remap(&|p| if keep(p) { Some(p.clone()) } else { None }, &lbp::T { editor: None, ..layout.clone() })
}

fn editor_known(checked: &Workspace, name: &str) -> bool {
    checked.graphs.iter().any(|g| g.context == context::editor() && g.name == name)
}

fn pruned(checked: &Workspace, catalog: &Catalog, layout: &lbp::T) -> lbp::T {
    let editor = layout.editor.clone().filter(|name| editor_known(checked, name));
    lbp::T { editor, ..prune(checked, catalog, layout) }
}

/// An edit remaps the keys it moves. Validate only the remaining layout keys against the authored
/// paths, without projecting unrelated graphs.
fn prune_keys(checked: &Workspace, layout: &lbp::T) -> lbp::T {
    let graph = |root: &str| -> Option<&w::Graph> {
        checked
            .graphs
            .iter()
            .find(|g| g.name == root)
            .or_else(|| checked.defs.iter().find(|g| root == format!("def:{}", g.name) || root == g.name))
    };
    let exists = |path: &[String]| -> bool {
        match path {
            [root] => graph(root).is_some(),
            [root, input] if input.starts_with(':') => graph(root).map_or(false, |g| g.inputs.iter().any(|(name, _, _)| *input == format!(":{name}"))),
            path => flow_edit::arg_text(&checked.source, path, &ArgKey::Whole).is_some(),
        }
    };
    let keep = |path: &Vec<String>| -> bool {
        let mut prefix: Vec<String> = Vec::new();
        for segment in path {
            if segment.starts_with('@') {
                return exists(&prefix);
            }
            prefix.push(segment.clone());
        }
        exists(&prefix)
    };
    let editor = layout.editor.clone().filter(|name| editor_known(checked, name));
    lbp::T { editor, ..lbp::remap(&|p| if keep(p) { Some(p.clone()) } else { None }, &lbp::T { editor: None, ..layout.clone() }) }
}

/// What `of_text` and `check_text` take beside the catalog: OCaml's optional arguments.
#[derive(Clone, Default)]
pub struct TextOptions {
    /// Defaults to `flow_sop::operators::all`; an explicit list replaces those extensions.
    pub ops: Option<Arc<Vec<FlowOp>>>,
    pub imports: Vec<(String, String)>,
    pub inputs: Inputs,
    pub settings: Option<settings::T>,
    pub layout: Option<lbp::T>,
}

fn is_library_head(f: &S) -> bool {
    matches!(head(f), Some("graph" | "defn" | "defmacro"))
}

fn load_import(path: &str, imports: &[(String, String)]) -> Result<Vec<S>, Vec<Diagnostic>> {
    let fail = |message: &str| Err(vec![diag(None, "E_IMPORT", &format!("{path}: {message}"))]);
    let Some((_, text)) = imports.iter().find(|(p, _)| p == path) else { return fail("imported file is unavailable.") };
    let forms = s::parse(text).map_err(|d| vec![d])?;
    if forms.iter().any(|f| head(f) == Some("import")) {
        // ponytail: imports are one level; recursive resolution needs cycle detection.
        return fail("transitive imports are not supported.");
    }
    let children = forms.iter().flat_map(|form| {
        let unwrap = head(form) == Some("workspace") && form.children().len() >= 2;
        form.children().iter().skip(if unwrap { 2 } else { form.children().len() }).map(|id| form.child(*id))
            .chain((!unwrap).then(|| form.clone()))
    }).collect::<Vec<_>>();
    if children.iter().all(|f| matches!(head(f), Some("graph" | "defn" | "defnode" | "defmacro"))) {
        Ok(children)
    } else {
        fail("expected graph, defn, defnode or defmacro forms.")
    }
}

/// Parse and check. Errors are the checker's diagnostics, a parse error, or a layout or settings
/// form that does not read (`E_LAYOUT`, `E_SETTINGS`, with the form's span). Unknown or duplicate
/// document forms give `E_DOCUMENT_FORM`.
pub fn check_text(options: &TextOptions, catalog: &Catalog, text: &str) -> Result<(T, Vec<Diagnostic>), Vec<Diagnostic>> {
    let forms = s::parse(text).map_err(|d| vec![d])?;
    let fragment = !forms.is_empty() && forms.iter().all(is_library_head);
    let mut builder = SyntaxBuilder::new();
    let root = if fragment {
        let span = Span { start: 0, finish: 0 };
        let mut items = vec![
            builder.push_draft(0, Draft::Sym("workspace".to_owned()), span.clone(), None),
            builder.push_draft(0, Draft::Sym("library".to_owned()), span.clone(), None),
        ];
        items.extend(forms.iter().map(|form| builder.import(form)));
        builder.push_draft(0, Draft::List(items), span, None)
    } else {
        let Some(workspace) = forms.iter().find(|form| head(form) == Some("workspace")) else {
            return Err(vec![diag(None, "E_WORKSPACE", "Expected a (workspace ...) form.")]);
        };
        check_forms(&forms)?;
        builder.import(workspace)
    };
    let mut loaded: Vec<(String, Vec<S>)> = Vec::new();
    for path in import_paths(text)? {
        let children = load_import(&path, &options.imports)?;
        loaded.push((path, children));
    }
    let root = match builder.row(root).node {
        Node::List(range) if builder.range_ids(range).len() >= 2 && !loaded.is_empty() => {
            let mut items = builder.range_ids(range)[..2].to_vec();
            items.extend(loaded.iter().flat_map(|(_, children)| children).map(|form| builder.import(form)));
            items.extend_from_slice(&builder.range_ids(range)[2..]);
            let node = Node::List(builder.range(&items));
            let row = builder.row(root).clone();
            let trivia = builder.trivia(root).cloned();
            builder.push(row.id, node, row.span, trivia)
        }
        _ => root,
    };
    // Imported parses have independent ids; the composed workspace owns one id space.
    let (root, _) = builder.renumber(0, root);
    let ws = builder.freeze().handle(root);
    let ops: Arc<Vec<FlowOp>> = options.ops.clone().unwrap_or_else(|| Arc::new(flow_sop::operators::all().to_vec()));
    let (checked, warnings) = fsc::check_workspace(&ops, fragment, None, catalog, std::slice::from_ref(&ws));
    let Some(mut checked) = checked else { return Err(warnings) };
    // the ops stay one shared list across text edits (`same_graph` compares them by identity)
    checked.ops = ops.clone();
    let layout = match forms.iter().find(|f| head(f) == Some("layout")) {
        None => options.layout.clone().unwrap_or_default(),
        Some(f) => lbp::of_syntax(f).map_err(|m| vec![diag(Some(f.span.clone()), "E_LAYOUT", &m)])?,
    };
    let base_settings = options.settings.clone().unwrap_or_else(settings::none);
    let st = match forms.iter().find(|f| head(f) == Some("settings")) {
        None => base_settings,
        Some(f) => read_settings(&base_settings, f).map_err(|m| vec![diag(Some(f.span.clone()), "E_SETTINGS", &m)])?,
    };
    let layout = pruned(&checked, catalog, &layout);
    Ok((
        T {
            source: vec![ws],
            layout,
            settings: st,
            inputs: Arc::new(options.inputs.clone()),
            literal: None,
            imports: loaded.clone(),
            fragment,
            import_sources: options.imports.iter().filter(|(path, _)| loaded.iter().any(|(p, _)| p == path)).cloned().collect(),
            extra: if fragment { Vec::new() } else { forms.iter().filter(|f| head(f) != Some("workspace")).cloned().collect() },
            checked: Arc::new(checked),
        },
        warnings,
    ))
}

pub fn of_text(options: &TextOptions, catalog: &Catalog, text: &str) -> Result<T, Vec<Diagnostic>> {
    check_text(options, catalog, text).map(|(t, _)| t)
}

pub fn import_texts(t: &T) -> Vec<(String, String)> {
    t.import_sources.clone()
}

fn max_id(form: &S) -> i64 {
    form.children().iter().fold(form.id, |id, child| id.max(max_id(&form.child(*child))))
}

/// The workspace, then the other root forms in the order they were written (`view` verbatim,
/// `layout` and `settings` rewritten from the document), each under the comments written above the
/// form it replaces. Comments of a form that is no longer written move to the next one, or to the
/// end of the text.
pub fn print(t: &T) -> R<(String, lisp::Spans)> {
    let source = authored_source(t);
    let source: Vec<S> = if t.fragment {
        source.iter().flat_map(|form| form.children().iter().skip(2).map(|id| form.child(*id))).collect()
    } else {
        source
    };
    if t.fragment {
        return lisp::print(&source);
    }
    let fresh: Vec<(&str, Option<S>)> = vec![
        ("layout", if lbp::is_empty(&t.layout) { None } else { Some(lbp::to_syntax(&t.layout)) }),
        ("settings", settings_form(&t.settings)),
    ];
    let written: Vec<&str> = t.extra.iter().filter_map(|f| head(f)).collect();
    let mut names: Vec<&str> = written.clone();
    names.extend(["layout", "settings"].iter().filter(|n| !written.contains(n)));
    let mut carried: Vec<String> = Vec::new();
    let mut remaining: Vec<S> = t.extra.clone();
    let mut builder = SyntaxBuilder::new();
    let mut forms: Vec<SyntaxId> = Vec::new();
    for name in names {
        let old = remaining.iter().position(|f| head(f) == Some(name)).map(|i| remaining.remove(i));
        let mut notes = carried.clone();
        notes.extend(old.as_ref().map(|f| f.notes().to_vec()).unwrap_or_default());
        let tail = old.as_ref().map(|f| f.tail().to_vec()).unwrap_or_default();
        let preserve = name == "view" || name == "import";
        let chosen = if preserve { old } else { fresh.iter().find(|(n, _)| *n == name).and_then(|(_, form)| form.clone()) };
        match chosen {
            Some(form) => {
                carried.clear();
                let mut trivia = form.trivia().cloned().unwrap_or_default();
                trivia.notes = notes;
                if !preserve { trivia.tail = tail; }
                let id = builder.import(&form);
                forms.push(builder.with_trivia(id, Some(trivia)));
            }
            None => {
                carried = notes;
                carried.extend(tail);
            }
        }
    }
    // Root metadata is rebuilt, or kept from an earlier parse. Give it a disjoint printed ID range
    // so text gestures cannot alias source tokens.
    if forms.is_empty() && carried.is_empty() { return lisp::print(&source); }
    let mut next = 1 + source.iter().fold(0, |id, form| id.max(max_id(form)));
    let mut roots = source.iter().map(|form| builder.import(form)).collect::<Vec<_>>();
    for form in forms {
        let (form, n) = builder.renumber(next, form);
        next = n;
        roots.push(form);
    }
    if !carried.is_empty() {
        if let Some(last) = roots.last_mut() {
            let mut trivia = builder.trivia(*last).cloned().unwrap_or_default();
            trivia.tail.extend(carried);
            *last = builder.with_trivia(*last, Some(trivia));
        }
    }
    let arena = builder.freeze();
    lisp::print(&roots.into_iter().map(|id| arena.handle(id)).collect::<Vec<_>>())
}

pub fn to_text(t: &T) -> R<String> {
    print(t).map(|(text, _)| text)
}

/// A checked rewrite as the next document: the layout keys follow `remap`, and are pruned unless
/// the edit changes no node. The edits a scrub repeats every frame change a value, never which
/// nodes there are.
fn commit(t: &T, remap: &dyn Fn(&Vec<String>) -> Option<Vec<String>>, editor_gone: bool, prune: bool, (source, checked): (Vec<S>, Workspace)) -> T {
    let mut layout = lbp::remap(remap, &t.layout);
    if editor_gone {
        layout.editor = None; // the other editor graphs are gone
    }
    if prune {
        layout = prune_keys(&checked, &layout);
    }
    T { source, checked: Arc::new(checked), layout, literal: None, ..t.clone() }
}

fn checker_for(ops: Vec<FlowOp>, library: bool) -> Box<flow_edit::Checker> {
    Box::new(move |catalog: &Catalog, forms: &[S]| fsc::check_workspace(&ops, library, None, catalog, forms))
}

/// An edit's checked workspace carries the document's ops Arc: the edit checked against those ops, and
/// `same_graph` compares them by identity (OCaml shares the list across edits).
// ponytail: flow_check builds a fresh ops Arc from the slice it is given; swap it here instead of threading the Arc through check.
fn keep_ops(t: &T, (source, mut checked): (Vec<S>, Workspace)) -> (Vec<S>, Workspace) {
    checked.ops = t.checked.ops.clone();
    (source, checked)
}

fn edit_unchecked(catalog: &Catalog, t: &T, op: &Op) -> R<T> {
    let key = |change: &Change| {
        let mut k = change.path.clone();
        k.push(format!("#:{}", change.field));
        k
    };
    match literal_edit::patch(catalog, &t.checked, op) {
        Some(Err(d)) => return Err(d),
        Some(Ok((checked, change))) if t.literal.as_ref().map_or(0, |l| l.edits.len()) < 4096 => {
            let (base, mut edits) = match &t.literal {
                None => (t.checked.clone(), PathMap::new()),
                Some(l) => (l.base.clone(), l.edits.clone()),
            };
            let change = match edits.get(&key(&change)) {
                Some(previous) => Change { before: previous.before.clone(), ..change },
                None => change,
            };
            edits.insert(key(&change), change);
            let source = (*checked.source).clone();
            return Ok(T { checked: Arc::new(checked), source, literal: Some(Literal { base, edits }), ..t.clone() });
        }
        _ => {}
    }
    let checker = checker_for((*t.checked.ops).clone(), t.fragment);
    let result = flow_edit::apply_checked_with(&t.checked.ops, t.fragment, Some(&*checker), catalog, &t.source, op)?;
    let prune = !matches!(op, Op::SetArg { .. } | Op::SetInputDefault { .. } | Op::SetNote { .. } | Op::ToggleBypass { .. });
    Ok(commit(t, &|p| flow_edit::remap(op, p), false, prune, keep_ops(t, result)))
}

/// `run` on a document whose imported forms the edit neither names nor rewrites.
fn guarded(t: &T, paths: &[Vec<String>], run: &mut dyn FnMut() -> R<T>) -> R<T> {
    let refusal = |file: &str| Err(diag(None, "E_IMPORTED", &format!("Read-only imported form from {file}.")));
    if let Some(file) = paths.iter().find_map(|p| imported_file(&t.imports, p)) {
        return refusal(&file);
    }
    let next = run()?;
    // Renaming a local graph can rewrite references in imported forms too.
    let find = |source: &[S], name: &Option<String>| -> Option<S> {
        source.iter().find_map(|root| root.children().iter().skip(2).find_map(|id| {
            let form = root.child(*id);
            (second_name(&form) == *name).then_some(form)
        }))
    };
    let changed = t.imports.iter().find_map(|(file, forms)| {
        if forms.iter().any(|form| {
            let name = second_name(form);
            match (find(&t.source, &name), find(&next.source, &name)) {
                (Some(a), Some(b)) => !s::equal(&a, &b),
                (None, None) => false,
                _ => true,
            }
        }) {
            Some(file.clone())
        } else {
            None
        }
    });
    match changed {
        Some(file) => refusal(&file),
        None => Ok(next),
    }
}

fn op_paths(op: &Op) -> Vec<Vec<String>> {
    match op {
        Op::SetArg { node, .. }
        | Op::Connect { node, .. }
        | Op::Disconnect { node, .. }
        | Op::Unfold { node, .. }
        | Op::AddArm { node, .. }
        | Op::DeleteArm { node, .. }
        | Op::FoldInto { node }
        | Op::Hoist { node }
        | Op::Rename { node, .. }
        | Op::InlineMacro { node }
        | Op::InlineDefn { node }
        | Op::MoveBinding { node, .. }
        | Op::MoveStep { node, .. }
        | Op::ToggleBypass { node }
        | Op::SetNote { node, .. }
        | Op::AddItem { node }
        | Op::MoveItem { node, .. }
        | Op::AddField { node, .. } => vec![node.clone()],
        Op::Wrap { nodes, .. }
        | Op::MakeLocalFn { nodes }
        | Op::MakeDefn { nodes, .. }
        | Op::MakeMacro { nodes, .. }
        | Op::DeleteNodes { nodes }
        | Op::Duplicate { nodes }
        | Op::GroupMerge { nodes, .. } => nodes.clone(),
        Op::AddNode { scope, .. } => vec![scope.clone()],
        Op::SetInputDefault { form, .. } => vec![vec![form.clone()]],
        Op::SetGraph { name, .. } | Op::RemoveGraph { name } | Op::RenameGraph { name, .. } => vec![vec![name.clone()]],
    }
}

/// One gesture, atomically: the source is rewritten and re-checked, layout keys follow
/// `flow_edit::remap` and are pruned as in `check_text`, and on an error nothing changes.
pub fn edit(catalog: &Catalog, t: &T, op: &Op) -> R<T> {
    guarded(t, &op_paths(op), &mut || edit_unchecked(catalog, t, op))
}

/// One layout gesture, atomically, as `edit`: layout keys follow `layout_edit::remap`.
pub fn edit_layout(catalog: &Catalog, t: &T, op: &layout_edit::Op) -> R<T> {
    guarded(t, &layout_edit::paths(op), &mut || {
        let checker = checker_for((*t.checked.ops).clone(), t.fragment);
        let result = layout_edit::apply_checked(&t.checked.ops, t.fragment, Some(&*checker), catalog, &t.source, op)?;
        Ok(commit(t, &|p| layout_edit::remap(op, p), layout_edit::merges(op), layout_edit::changes_nodes(op), keep_ops(t, result)))
    })
}

/// The value of a context's default instance in an evaluated plan (forced at `t = 0` unless
/// `force` is false); none when the workspace has no graph of that context.
pub fn context_value(force: bool, workspace: &T, plan: &Plan, ctx: Context) -> R<Option<E>> {
    let graph = if ctx == context::editor() {
        editor_graph(workspace)
    } else {
        workspace.checked.graphs.iter().find(|g| g.context == ctx).cloned()
    };
    let Some(graph) = graph else { return Ok(None) };
    let Some(instance) = plan.instances.iter().find(|i| i.default && i.graph == graph.name) else { return Ok(None) };
    if force {
        flow_eval::force(&instance.result, &input::at_time(0.), None).map(Some)
    } else {
        Ok(Some(instance.result.clone()))
    }
}
