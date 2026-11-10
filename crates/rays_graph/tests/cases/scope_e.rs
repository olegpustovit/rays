//! Sticky/readable scope interactions and large-scope layout measurement.
use crate::support::*;
use flow_graph::flow_edit as e;
use flow_graph::projection as p;
use rays_graph::scope_pane as scope;
use rays_graph::scope_state::ScopeOpts;
use std::rc::Rc;

fn scope_of(text: &str) -> p::Scope {
    let ws = checked(text);
    p::of_graph(&scope_catalog(), &ws, "g").unwrap_or_else(|e| panic!("{e}"))
}

fn table(l: &p::Layout) -> Vec<(flow_check::workspace::Path, p::Placed)> {
    l.placed.iter().map(|b| (b.path.clone(), b.clone())).collect()
}

fn find(t: &[(flow_check::workspace::Path, p::Placed)], path: &[flow_syntax::symbol::Symbol]) -> p::Placed {
    t.iter().find(|(q, _)| q.as_slice() == path).expect("placed").1.clone()
}

fn layout_prev(sc: &p::Scope, prev: Rc<dyn Fn(&[flow_syntax::symbol::Symbol]) -> Option<(f64, f64, f64, f64)>>) -> p::Layout {
    let mut o = p::LayoutOptions::new();
    o.prev = prev;
    p::layout(&o, sc)
}

fn run_sticky() {
    let before = scope_of("(workspace w (graph g :context sop (let* [a (sop/box) b (sop/transform a :translate [1 0 0]) c (sop/transform b :translate [2 0 0]) d (sop/transform c :translate [3 0 0])] d)))");
    let after = scope_of("(workspace w (graph g :context sop (let* [z (sop/box) a (sop/transform z :translate [0 0 0]) b (sop/transform a :translate [1 0 0]) c (sop/transform b :translate [2 0 0]) d (sop/transform c :translate [3 0 0])] d)))");
    let first = p::layout(&p::LayoutOptions::new(), &before);
    let first_table = table(&first);
    let ft = first_table.clone();
    let prev: Rc<dyn Fn(&[flow_syntax::symbol::Symbol]) -> Option<(f64, f64, f64, f64)>> = Rc::new(move |path: &[flow_syntax::symbol::Symbol]| ft.iter().find(|(q, _)| q.as_slice() == path).map(|(_, b)| (b.x, b.y, b.w, b.h)));
    let sticky = table(&layout_prev(&after, prev));
    let relaid = table(&p::layout(&p::LayoutOptions::new(), &after));
    for (path, b) in &first_table {
        let s = find(&sticky, path);
        check(s.x == b.x && s.y == b.y, "an edit that added a node moved a card that was there");
    }
    let pos = |name: &str, l: &[(flow_check::workspace::Path, p::Placed)]| find(l, &path(&["g", name]));
    let z = pos("z", &sticky);
    let a = pos("a", &sticky);
    check(z.x < a.x && z.y == a.y, "the new node is not on the row of its consumer, one column left");
    check(pos("a", &relaid).x != a.x, "the fixture does not shift the columns when laid out afresh");
    for (pp, b) in &sticky {
        for (q, c) in &sticky {
            check(pp == q || b.x >= c.x + c.w || c.x >= b.x + b.w || b.y >= c.y + c.h || c.y >= b.y + b.h, "two cards overlap");
        }
    }
    // moving a binding in the text moves no card
    let text = "(workspace w (graph g :context sop (let* [a (sop/box) b (sop/box) c (sop/merge a b)] c)))";
    let ws = checked(text);
    let forms = flow_syntax::syntax::parse(text).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let (source, ws2) = e::apply_checked(&scope_catalog(), &forms, &e::Op::MoveBinding { node: path(&["g", "b"]), by: -1 }).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    check(flow_syntax::lisp::print(&source).unwrap().0 != text, "Move_binding changed no text");
    let first = p::layout(&p::LayoutOptions::new(), &p::of_graph(&scope_catalog(), &ws, "g").unwrap());
    let key = |l: &p::Layout| {
        let mut v: Vec<(flow_check::workspace::Path, i64, i64)> = l.placed.iter().map(|b| (b.path.clone(), b.x.to_bits() as i64, b.y.to_bits() as i64)).collect();
        v.sort();
        v
    };
    let first_for_prev = first.clone();
    let prev: Rc<dyn Fn(&[flow_syntax::symbol::Symbol]) -> Option<(f64, f64, f64, f64)>> = Rc::new(move |path: &[flow_syntax::symbol::Symbol]| first_for_prev.placed.iter().find(|b| b.path.as_slice() == path).map(|b| (b.x, b.y, b.w, b.h)));
    let moved = layout_prev(&p::of_graph(&scope_catalog(), &ws2, "g").unwrap(), prev);
    check(key(&first) == key(&moved), "reordering bindings moved a card");
}

fn at_of(entries: Vec<(&'static str, (f64, f64))>) -> ScopeOpts {
    saved_opts(TestSaved { at: entries.into_iter().map(|(n, pt)| (path(&["g", n]), pt)).collect(), ..Default::default() })
}

/// Selecting a node lights its chain; a long wire is a tag until its chain is lit.
fn run_readable() {
    let mut ui = new_ui();
    let ws = checked("(workspace r (graph g :context sop (let* [a (sop/box) b (sop/transform a) c (sop/transform b) d (sop/transform a) e (sop/merge c d a)] e)))");
    let opts = at_of(vec![("a", (0.0, 0.0)), ("b", (288.0, 0.0)), ("c", (576.0, 0.0)), ("d", (288.0, 240.0)), ("@result", (4000.0, 0.0)), ("e", (864.0, 0.0))]);
    let (view, _) = scope_view(opts, &ws, "g");
    let (view, _) = scope_step(&mut ui, view, &frame0());
    check(scope::inspect::chain(&view).is_none(), "a chain was lit with nothing selected");
    check(scope::inspect::tagged_wires(&view) == 0, "a short graph drew a tag");
    let view = { let mut r8_view = view; scope::select(vec![path(&["g", "b"])], &mut r8_view); r8_view };
    match scope::inspect::chain(&view) {
        Some((up, down)) => {
            let has = |l: &Vec<flow_check::workspace::Path>, n: &str| l.contains(&path(&["g", n]));
            check(has(&up, "a") && has(&up, "b") && !has(&up, "c") && !has(&up, "d"), "upstream of b is not a and b");
            check(has(&down, "b") && has(&down, "c") && has(&down, "e") && !has(&down, "a") && !has(&down, "d"), "downstream of b is not b, c and e");
        }
        None => fail("selecting a node lit no chain"),
    }
    // a node 4,000 points to the right reads [a] through a tag, until a or e is selected
    let opts = at_of(vec![("e", (4000.0, 0.0)), ("a", (0.0, 0.0)), ("b", (288.0, 0.0)), ("c", (576.0, 0.0)), ("d", (288.0, 240.0))]);
    let (far, _) = scope_view(opts, &ws, "g");
    let (far, _) = scope_step(&mut ui, far, &frame0());
    check(scope::inspect::tagged_wires(&far) >= 1, "a wire four thousand points long was not a tag");
    let lit = { let mut r8_view = far.clone(); scope::select(vec![path(&["g", "a"])], &mut r8_view); r8_view };
    check(scope::inspect::tagged_wires(&lit) == 0, "selecting the source did not show the real wire");
    let (off, _) = { let mut r8_view = far; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, scope::Command::ToggleTags, &mut r8_changes); (r8_view, r8_changes) };
    check(scope::inspect::tagged_wires(&off) == 0, "the tags toggle left a tag");
    // Unfold all: one Unfold per call written in a row of the card
    let ws = checked("(workspace u (graph g :context sop (let* [a (sop/merge (sop/box) (sop/box))] a)))");
    let (view, _) = scope_view(ScopeOpts::default(), &ws, "g");
    let (view, _) = scope_step(&mut ui, view, &frame0());
    let (_, changes) = { let mut r8_view = { let mut r8_view = view; scope::select(vec![path(&["g", "a"])], &mut r8_view); r8_view }; let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, scope::Command::UnfoldAll, &mut r8_changes); (r8_view, r8_changes) };
    let unfolds = changes.iter().filter(|c| matches!(c, scope::Change::SyntaxEdit(e::Op::Unfold { .. }))).count();
    check(unfolds >= 2, "Unfold all did not unfold every call of the card");
}

/// Build, lay out and paint a 2001-node scope; RAYS_BENCH_SCOPE prints timings, otherwise check one live frame.
fn bench_scope_big() {
    let sc = Rc::new(big_scope(2001));
    let mut ui = new_ui();
    let verbose = std::env::var("RAYS_BENCH_SCOPE").is_ok();
    let time = |label: &str, runs: i32, f: &mut dyn FnMut()| {
        let started = std::time::Instant::now();
        for _ in 0..runs {
            f();
        }
        if verbose {
            println!("{:<28} {:.3} ms", label, started.elapsed().as_secs_f64() * 1000.0 / runs as f64);
        }
    };
    time("2001 nodes: with_scope", if verbose { 5 } else { 1 }, &mut || {
        let _ = { let mut r8_view = scope::create(Some(1000), Some(700)); scope::with_scope(ScopeOpts::default(), "g", sc.clone(), &mut r8_view); r8_view };
    });
    let mut view = { let mut r8_view = scope::create(Some(1000), Some(700)); scope::with_scope(ScopeOpts::default(), "g", sc.clone(), &mut r8_view); r8_view };
    for _ in 0..5 {
        view = in_frame(&mut ui, &frame0(), view).0;
    }
    let again = view.clone();
    time("2001 nodes: with_scope again", if verbose { 20 } else { 1 }, &mut || {
        let _ = { let mut r8_view = again.clone(); scope::with_scope(ScopeOpts::default(), "g", sc.clone(), &mut r8_view); r8_view };
    });
    time("2001 nodes: frame", if verbose { 100 } else { 1 }, &mut || {
        view = in_frame(&mut ui, &frame0(), view.clone()).0;
    });
    let pv = view.clone();
    time("2001 nodes: Point_all", if verbose { 5 } else { 1 }, &mut || {
        let _ = { let mut r8_view = pv.clone(); let mut r8_changes = Vec::new(); scope::run_command(&mut r8_view, scope::Command::PointAll, &mut r8_changes); (r8_view, r8_changes) };
    });
    check(scope::inspect::wire_count(&view) > 0, "the big scope has no wires");
}

fn scope_mutation_keeps_prior_changes_and_defers_syntax_edits() {
    use input::Key;
    use rays_graph::scope_commands::step_hints;
    use rays_graph::scope_state::{Change, Command};
    let projection = Rc::new(scope_of("(workspace w (graph g :context sop (let* [a (sop/box) b (sop/transform a) c (sop/transform b)] c)))"));
    let mut view = scope::create(Some(1000), Some(700));
    scope::with_scope(ScopeOpts::default(), "g", projection.clone(), &mut view);
    scope::select(vec![path(&["g", "a"])], &mut view);
    let prefix = Change::Notice("earlier intent".into());
    let mut changes = vec![prefix.clone()];
    scope::run_command(&mut view, Command::ShowHints, &mut changes);
    assert_eq!(changes, vec![prefix.clone()]);
    step_hints(&mut view, &[], false, &mut changes);
    let target = view.hinting.as_ref().unwrap().targets.iter().find(|target| target.key.is_some()).unwrap().clone();
    let keys: Vec<Key> = target.label.chars().map(Key::KeyChar).collect();
    step_hints(&mut view, &keys, false, &mut changes);
    assert_eq!(changes, vec![prefix, Change::SyntaxEdit(e::Op::Connect {
        node: target.node, key: target.key.unwrap(), src: "a".into(), iter: false,
    })]);
    assert!(view.hinting.is_none());
    assert!(Rc::ptr_eq(view.scope.as_ref().unwrap(), &projection), "a graph handler applied its syntax intent");
    assert_eq!(scope::selected(&view), vec![path(&["g", "a"])]);
}

fn change_equality() {
    use rays_graph::scope_state::Change;
    let edit = Change::SyntaxEdit(e::Op::Rename { node: path(&["g", "a"]), to_: "b".into() });
    let same = edit.clone();
    let different = Change::SyntaxEdit(e::Op::Rename { node: path(&["g", "a"]), to_: "c".into() });
    mem_sys::local_reset();
    assert_eq!(edit, same);
    assert_ne!(edit, different);
    assert_eq!(mem_sys::local_alloc_count(), 0, "change equality allocates");
}

pub const CASES: &[(&str, fn())] = &[("scope_mutation_keeps_prior_changes_and_defers_syntax_edits", scope_mutation_keeps_prior_changes_and_defers_syntax_edits), ("change_equality", change_equality), ("run_sticky", run_sticky), ("run_readable", run_readable), ("bench_scope_big", bench_scope_big)];
