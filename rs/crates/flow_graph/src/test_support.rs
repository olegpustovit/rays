// The one-liners every OCaml test file under lib/flow_graph copies (and lib/test_support).
use crate::projection as p;
use flow_check::catalog::Catalog;
use flow_check::workspace as w;
use flow_eval::Value;
use flow_syntax::syntax::{self as s, Syntax};

pub fn catalog() -> Catalog {
    Catalog { version: 1, kinds: Vec::new() }
}

pub fn parse(text: &str) -> Vec<Syntax> {
    s::parse(text).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

pub fn form(text: &str) -> Syntax {
    parse(text).remove(0)
}

pub fn show(ds: &[rays_diagnostic::Diagnostic]) -> String {
    ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")
}

pub fn check(src: &[Syntax]) -> w::Workspace {
    match w::check(&[], false, false, None, &catalog(), src) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => panic!("{}", show(&ds)),
    }
}

/// `Workspace.check` accepting warnings (`Some w, _`).
pub fn check_lenient(src: &[Syntax]) -> w::Workspace {
    match w::check(&[], false, false, None, &catalog(), src) {
        (Some(ws), _) => ws,
        (_, ds) => panic!("{}", show(&ds)),
    }
}

pub fn run(ws: &w::Workspace, time: f64) -> flow_eval::Eval {
    flow_eval::run(ws, false, None, None, None, time).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

pub fn result(ws: &w::Workspace, time: f64) -> Value {
    run(ws, time).results.iter().find(|(n, _)| n == "g").unwrap().1.clone()
}

pub fn recorded(ws: &w::Workspace) -> flow_eval::Eval {
    flow_eval::r#static(ws, true, None).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

pub fn unrecorded(ws: &w::Workspace) -> flow_eval::Eval {
    flow_eval::static_eval(ws).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

pub fn pane(ws: &w::Workspace) -> p::Scope {
    p::of_graph(&catalog(), ws, "g").unwrap()
}

pub fn card(ws: &w::Workspace, path: &[&str]) -> p::Node {
    let path: Vec<String> = path.iter().map(|x| x.to_string()).collect();
    p::find(&pane(ws), &path).unwrap_or_else(|| panic!("no card {:?}", path))
}

pub fn path(parts: &[&str]) -> Vec<String> {
    parts.iter().map(|x| x.to_string()).collect()
}

pub fn text_of(forms: &[Syntax]) -> String {
    flow_syntax::lisp::print(forms).0
}

pub fn has(hay: &str, sub: &str) -> bool {
    hay.contains(sub)
}

/// The first node with this head, looking into zones.
pub fn find_head(head: &str, scope: &p::Scope) -> Option<p::Node> {
    for node in &scope.nodes {
        if node.head == head {
            return Some(node.clone());
        }
        if let Some(z) = &node.zone {
            if let Some(f) = find_head(head, &z.scope) {
                return Some(f);
            }
        }
    }
    None
}

pub fn row(card: &p::Node, key: &crate::edit_base::ArgKey) -> p::Row {
    card.rows.iter().find(|r| r.key == *key).unwrap().clone()
}
