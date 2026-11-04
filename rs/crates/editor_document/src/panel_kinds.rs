//! Port of lib/editor_document/panel_kinds.ml: the editor's panel kinds as the forms a layout
//! gesture writes. `Flow_graph.Flow_edit` rewrites the editor graph's layout and is given these
//! forms: it knows no panel kind.
use flow_syntax::syntax::{self as s, Node, Syntax};

fn sym(name: &str) -> Syntax {
    s::make(Node::Sym(name.to_owned()))
}

fn call(head: &str, args: Vec<Syntax>) -> Syntax {
    let mut items = vec![sym(head)];
    items.extend(args);
    s::make(Node::List(items))
}

/// The first graph of the workspace `source` whose `:context` is `context`.
fn graph_of(source: &[Syntax], context: &str) -> Option<String> {
    fn has(rest: &[Syntax], context: &str) -> bool {
        let mut i = 0;
        while i < rest.len() {
            if let (Node::Kw(k), Some(next)) = (&rest[i].node, rest.get(i + 1)) {
                if k == "context" {
                    return matches!(&next.node, Node::Sym(c) if c == context);
                }
            }
            i += 1;
        }
        false
    }
    let first = source.first()?;
    let Node::List(items) = &first.node else { return None };
    match items.as_slice() {
        [head, _, rest @ ..] if matches!(&head.node, Node::Sym(w) if w == "workspace") => rest.iter().find_map(|item| {
            if let Node::List(parts) = &item.node {
                if let [h, n, tail @ ..] = parts.as_slice() {
                    if let (Node::Sym(h), Node::Sym(n)) = (&h.node, &n.node) {
                        if h == "graph" && has(tail, context) {
                            return Some(n.clone());
                        }
                    }
                }
            }
            None
        }),
        _ => None,
    }
}

/// `expr source kind`: `outline`, `graph`, `list`, `lisp`, `inspector`, `spreadsheet`, `timeline`,
/// `viewport` (over the first scene graph of the workspace `source`), `canvas` (over its first
/// draw graph), or `"graph:name"` for a graph panel showing that graph.
pub fn expr(source: &[Syntax], kind: &str) -> Result<Syntax, String> {
    match kind {
        "outline" | "graph" | "list" | "lisp" | "inspector" | "spreadsheet" | "timeline" => Ok(call(&format!("ui/{kind}"), vec![])),
        "viewport" => graph_of(source, "scene")
            .map(|n| call("ui/viewport", vec![call("ref", vec![sym(&n)])]))
            .ok_or_else(|| "A viewport needs a scene graph.".to_owned()),
        "canvas" => graph_of(source, "draw")
            .map(|n| call("ui/canvas", vec![call("ref", vec![sym(&n)])]))
            .ok_or_else(|| "A canvas needs a draw graph.".to_owned()),
        _ if kind.starts_with("graph:") => Ok(call("ui/graph", vec![s::make(Node::Str(kind.as_bytes()[6..].to_vec()))])),
        _ => Err(format!("Unknown panel type {kind}.")),
    }
}

/// The kind a split puts beside a panel of this kind: a graph beside a lisp panel, else a lisp panel.
pub fn beside(kind: &str) -> &'static str {
    if kind == "lisp" { "graph" } else { "lisp" }
}
