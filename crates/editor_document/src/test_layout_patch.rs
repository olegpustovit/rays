//! The shell a lowering keeps: an edit that does not change the panels leaves the shell's Arc alone (the
//! shell geometry cache is keyed on it); one that does, replaces it.
use crate::{document, document_edit, layout_edit as le, preset, settings};
use flow_graph::flow_edit::{ArgKey, Op};
use flow_syntax::syntax as s;
use std::sync::Arc;

fn load(file: &str) -> document::T {
    let path = format!("{}/../../sketches/{file}", env!("CARGO_MANIFEST_DIR"));
    preset::load(None, &path, None, &settings::none()).expect("load").doc
}

fn path(names: &[&str]) -> flow_check::workspace::Path {
    names.iter().map(|n| (*n).into()).collect()
}

fn panel(text: &str) -> s::Syntax {
    s::parse(text).unwrap().remove(0)
}

#[test]
fn part_shell_arc_survives_a_value_edit() {
    let doc = load("isogrid.rays");
    let before = doc.shell.clone().expect("isogrid has an editor graph");
    let width = Op::SetArg { node: path(&["settings", "@result"]), key: ArgKey::Kw("width".into()), sub: Vec::new(), value: s::parse("1000").unwrap().remove(0) };
    let scrubbed = document_edit::syntax_edit(None, &doc, &width).expect("width edit");
    assert!(Arc::ptr_eq(&before, scrubbed.shell.as_ref().unwrap()), "a value edit replaced the shell");
    let split = le::Op::SplitPanel { node: path(&["editor", "preview"]), axis: le::Axis::H, ratio: 0.5, panel: panel("(ui/inspector)") };
    let split = document_edit::layout_op(None, &scrubbed, &split).expect("split");
    assert!(!Arc::ptr_eq(&before, split.shell.as_ref().unwrap()), "a layout edit kept the old shell");
}
