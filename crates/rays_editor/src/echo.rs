//! Gesture echo: what an edit wrote, in the words of the text (the status strip prints it, as the
//! carry's preview line does for a put). An op with no short words has none, and the history label
//! stands.
use flow_syntax::symbol::Symbol;
use flow_graph::flow_edit::{ArgKey, Op};
use flow_syntax::lisp;

fn path(node: &[Symbol]) -> String {
    flow_syntax::symbol::join_path(&node, "/")
}

fn shorten(text: &str) -> String {
    if text.len() <= 40 { text.to_string() } else { format!("{}...", String::from_utf8_lossy(&text.as_bytes()[..37])) }
}

fn key(k: &ArgKey) -> Option<String> {
    match k {
        ArgKey::Kw(k) | ArgKey::Field(k) => Some(format!(":{k}")),
        ArgKey::Pos(i) => Some(format!("input {i}")),
        ArgKey::Bv(..) | ArgKey::Whole | ArgKey::Arm(_) | ArgKey::Cell(_) => None,
    }
}

pub fn words(op: &Op) -> Option<String> {
    match op {
        Op::SetArg { node, key: k, sub, value } if sub.is_empty() => {
            key(k).map(|k| format!("{} {} on {}", k, shorten(&lisp::flat(value)), path(node)))
        }
        Op::SetArg { node, key: k, sub, value } => key(k).map(|k| {
            let parts: Vec<String> = sub.iter().map(|i| i.to_string()).collect();
            format!("{} part {} {} on {}", k, parts.join("."), shorten(&lisp::flat(value)), path(node))
        }),
        Op::Connect { node, key: ArgKey::Whole, src, .. } => Some(format!("{} as the result of {}", src, path(node))),
        Op::Connect { node, key: k, src, .. } => key(k).map(|k| format!("{} {} on {}", k, src, path(node))),
        Op::Disconnect { node, key: k, .. } => {
            Some(format!("{} unwired on {}", key(k).unwrap_or_else(|| "an input".to_string()), path(node)))
        }
        Op::AddNode { scope, name, expr } => Some(format!("{} {} in {}", name, shorten(&lisp::flat(expr)), path(scope))),
        Op::DeleteNodes { nodes } => {
            let all: Vec<String> = nodes.iter().map(|n| path(n)).collect();
            Some(format!("removed {}", shorten(&all.join(", "))))
        }
        Op::Rename { node, to_ } => Some(format!("{} renamed {}", path(node), to_)),
        Op::ToggleBypass { node } => Some(format!("{} bypass toggled", path(node))),
        _ => None,
    }
}

pub fn batch(ops: &[Op]) -> Option<String> {
    let words: Vec<String> = ops.iter().filter_map(words).collect();
    match words.len() {
        0 => None,
        1 => Some(words[0].clone()),
        n => Some(format!("{} and {} more", words[0], n - 1)),
    }
}
