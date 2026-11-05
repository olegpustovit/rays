//! Port of lib/editor_document/panel_kinds.ml: the editor's panel kinds as the forms a layout
//! gesture writes. `Flow_graph.Flow_edit` rewrites the editor graph's layout and is given these
//! forms: it knows no panel kind.
use flow_syntax::syntax::{Node, Syntax};
use flow_syntax::arena::{Draft, SyntaxArena, SyntaxBuilder, SyntaxId};

fn sym(name: &str, builder: &mut SyntaxBuilder) -> SyntaxId {
    builder.push_draft(0, Draft::Sym(name.to_owned()), rays_diagnostic::Span { start: 0, finish: 0 }, None)
}

fn call(head: &str, args: Vec<SyntaxId>, builder: &mut SyntaxBuilder) -> SyntaxId {
    let mut items = vec![sym(head, builder)];
    items.extend(args);
    builder.push_draft(0, Draft::List(items), rays_diagnostic::Span { start: 0, finish: 0 }, None)
}

/// The first graph of the workspace `source` whose `:context` is `context`.
fn graph_of(source: &[Syntax], context: &str) -> Option<String> {
    fn has(arena: &SyntaxArena, rest: &[SyntaxId], context: &str) -> bool {
        for pair in rest.windows(2) {
            if matches!(arena.row(pair[0]).node, Node::Kw(key) if arena.name(key) == "context") {
                return matches!(arena.row(pair[1]).node, Node::Sym(name) if arena.name(name) == context);
            }
        }
        false
    }
    let first = source.first()?;
    let arena = first.arena();
    let Node::List(range) = first.node else { return None };
    match first.range(range) {
        [head, _, rest @ ..] if matches!(arena.row(*head).node, Node::Sym(name) if arena.name(name) == "workspace") => rest.iter().find_map(|id| {
            let Node::List(range) = arena.row(*id).node else { return None };
            match arena.range(range) {
                [head, name, tail @ ..] => match (arena.row(*head).node, arena.row(*name).node) {
                    (Node::Sym(head), Node::Sym(name)) if arena.name(head) == "graph" && has(arena, tail, context) => Some(arena.name(name).to_owned()),
                    _ => None,
                },
                _ => None,
            }
        }),
        _ => None,
    }
}

/// `expr source kind`: `outline`, `graph`, `list`, `lisp`, `inspector`, `spreadsheet`, `timeline`,
/// `viewport` (over the first scene graph of the workspace `source`), `canvas` (over its first
/// draw graph), or `"graph:name"` for a graph panel showing that graph.
pub fn expr(source: &[Syntax], kind: &str) -> Result<Syntax, String> {
    let mut builder = SyntaxBuilder::new();
    let root = match kind {
        "outline" | "graph" | "list" | "lisp" | "inspector" | "spreadsheet" | "timeline" => Ok(call(&format!("ui/{kind}"), vec![], &mut builder)),
        "viewport" => graph_of(source, "scene")
            .map(|name| call("ui/viewport", vec![call("ref", vec![sym(&name, &mut builder)], &mut builder)], &mut builder))
            .ok_or_else(|| "A viewport needs a scene graph.".to_owned()),
        "canvas" => graph_of(source, "draw")
            .map(|name| call("ui/canvas", vec![call("ref", vec![sym(&name, &mut builder)], &mut builder)], &mut builder))
            .ok_or_else(|| "A canvas needs a draw graph.".to_owned()),
        _ if kind.starts_with("graph:") => {
            let name = builder.push_draft(0, Draft::Str(kind.as_bytes()[6..].to_vec()), rays_diagnostic::Span { start: 0, finish: 0 }, None);
            Ok(call("ui/graph", vec![name], &mut builder))
        }
        _ => Err(format!("Unknown panel type {kind}.")),
    }?;
    Ok(builder.freeze().handle(root))
}

/// The kind a split puts beside a panel of this kind: a graph beside a lisp panel, else a lisp panel.
pub fn beside(kind: &str) -> &'static str {
    if kind == "lisp" { "graph" } else { "lisp" }
}
