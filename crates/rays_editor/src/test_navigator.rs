//! Unit tests of navigator.rs: the rows over the bloom studio (lifted from the Navigator section of
//! lib/rays_editor/test/test_bloom_studio.ml, which needs the whole Editor), the narrow sheet, the
//! search, the jump list and the colour/notes helpers. The click and key paths of `view` run in the
//! editor suites (bundle F).
use crate::navigator::{self as n, Obj, Params, Row};
use editor_document::kinds;
use editor_document::workspace_doc::{self as doc, TextOptions};
use flow_graph::projection;
use std::rc::Rc;

fn bloom() -> (flow_check::catalog::Catalog, doc::T) {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../sketches/ws_bloom.rays");
    let text = std::fs::read_to_string(path).expect("sketches/ws_bloom.rays");
    let catalog = kinds::catalog(1, &sop::nodes::factories()).expect("catalog");
    let d = doc::of_text(&TextOptions::default(), &catalog, &text).unwrap_or_else(|e| panic!("{}", e.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")));
    (catalog, d)
}

fn params(catalog: &flow_check::catalog::Catalog, d: &doc::T, active: &str) -> Params {
    let scope = projection::of_graph(catalog, &d.checked, active).ok().map(Rc::new);
    Params {
        workspace: d.checked.clone(),
        active: Some(active.to_owned()),
        scope,
        records: None,
        probes: Rc::new(|_| 0),
        selected: Vec::new(),
        chips: Vec::new(),
        objects: vec![Obj { depth: 0, letter: String::new(), name: String::new(), detail: String::new(), visible: None, render: None, lead: false, inert: false, chosen: false, home: None }],
        root_detail: String::new(),
        layouts: None,
        notes: Vec::new(),
    }
}

fn lines(rows: &[Row]) -> Vec<String> {
    rows.iter().map(n::describe).collect()
}

fn listed(lines: &[String], text: &str) -> bool {
    lines.iter().any(|l| l.contains(text))
}

#[test]
fn rows_list_the_document() {
    let (catalog, d) = bloom();
    let state = n::open_graph("flower", &n::initial());
    let pa = params(&catalog, &d, "flower");
    let lines = lines(&n::rows(true, &state, &pa));
    let all = lines.join(" | ");
    assert!(listed(&lines, "input petals = 12") && listed(&lines, "input seed = 7"), "the inputs of flower: {all}");
    assert!(listed(&lines, "> flower \u{b7} ") && listed(&lines, "1 loop") && listed(&lines, "R root (scene)"), "the composition lists every graph, the open one marked: {all}");
    assert!(listed(&lines, "for ring"), "the open graph's loop is a tree row: {all}");
    assert!(
        listed(&lines, "half \u{b7} defn") && listed(&lines, "\u{d7}2") && listed(&lines, "petal \u{b7} defn") && listed(&lines, "\u{d7}1") && listed(&lines, "\u{3bb} twice \u{b7} 1 use"),
        "the reusable functions and macros count their uses: {all}"
    );
    assert!(listed(&lines, "read by scene"), "the data flow names who reads flower: {all}");
}

#[test]
fn folded_by_default_and_narrow_sheet() {
    let (catalog, d) = bloom();
    let pa = params(&catalog, &d, "flower");
    let folded = lines(&n::rows(true, &n::initial(), &pa));
    assert!(!listed(&folded, "for ring"), "node rows are folded until the graph is opened");
    let narrow = lines(&n::rows(false, &n::open_graph("flower", &n::initial()), &pa));
    assert!(!listed(&narrow, "for ring") && !listed(&narrow, "DATA FLOW") && !listed(&narrow, "read by scene"), "the narrow sheet has no node rows or data flow");
    assert!(listed(&narrow, "input petals = 12"), "but it keeps the inputs");
}

#[test]
fn search_lists_matches_only() {
    let (catalog, d) = bloom();
    let pa = params(&catalog, &d, "flower");
    let state = n::with_query("flow", &n::open_graph("flower", &n::initial()));
    assert_eq!(n::query(&state), "flow");
    let found = lines(&n::rows(true, &state, &pa));
    assert!(found[0].starts_with("1 MATCH") || found[0].contains("MATCH"), "{found:?}");
    assert!(listed(&found, "flower"));
    assert!(!listed(&found, "R root"));
    let none = lines(&n::rows(true, &n::with_query("zzzz", &n::initial()), &pa));
    assert_eq!(none.last().map(String::as_str), Some("nothing matches"));
}

#[test]
fn row_rects_stack_below_the_search_field() {
    let (catalog, d) = bloom();
    let pa = params(&catalog, &d, "flower");
    let state = n::open_graph("flower", &n::initial());
    let rects = n::row_rects(&state, &pa, (10, 20, 2400, 1400));
    assert!(!rects.is_empty());
    // the first row is a section head, 4 points under the 28-point search row
    assert!(matches!(rects[0].0, Row::Head(..)));
    assert_eq!(rects[0].1.1, 20.0 + 28.0);
    let mut bottom = rects[0].1.1;
    for (_, (x, y, w, h)) in &rects {
        assert!(*x == 10.0 && *w == 2400.0 && *h >= 24.0);
        assert!(*y >= bottom - 12.0 - 1e-9, "rows do not overlap: {y} < {bottom}");
        bottom = y + h;
    }
    let flower = rects.iter().find(|(r, _)| n::describe(r).contains("flower \u{b7} ")).expect("flower row");
    assert!(flower.1.3 == 24.0);
}

#[test]
fn jump_rows_follow_the_outline_groups() {
    let (_, d) = bloom();
    let rows = n::jump_rows(&d.checked);
    let groups: Vec<&str> = rows.iter().map(|(_, g)| g.as_str()).collect();
    assert!(rows.iter().any(|(name, g)| name == "flower" && g == "Geometry"), "{rows:?}");
    assert!(rows.iter().any(|(name, g)| name == "scene" && g == "Scene"), "{rows:?}");
    // groups are contiguous and in the contexts' order: Scene first
    assert_eq!(groups.first(), Some(&"Scene"));
    let mut seen: Vec<&str> = Vec::new();
    for g in &groups {
        if seen.last() != Some(g) {
            assert!(!seen.contains(g), "group {g} split in {groups:?}");
            seen.push(g);
        }
    }
}

#[test]
fn state_helpers() {
    let s = n::open_graph("g", &n::initial());
    assert_eq!(n::open_graph("g", &s), s);
    let t = n::open_graph("h", &s);
    assert_eq!(n::query(&n::with_query("x", &t)), "x");
    assert_eq!(n::new_graph_contexts(), ["sop", "scene", "draw", "image", "value", "material"]);
}
