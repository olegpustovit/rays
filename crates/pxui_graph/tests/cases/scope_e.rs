//! run_sticky, run_readable and the big-scope bench (the OCaml tail of test_pxui_graph.ml).
use crate::support::*;
use flow_graph::flow_edit as e;
use flow_graph::projection as p;
use pxui_graph::scope_pane as scope;
use pxui_graph::scope_state::ScopeOpts;
use std::rc::Rc;

fn scope_of(text: &str) -> p::Scope {
    let ws = checked(text);
    p::of_graph(&scope_catalog(), &ws, "g").unwrap_or_else(|e| panic!("{e}"))
}

fn table(l: &p::Layout) -> Vec<(Vec<String>, p::Placed)> {
    l.placed.iter().map(|b| (b.path.clone(), b.clone())).collect()
}

fn find(t: &[(Vec<String>, p::Placed)], path: &[String]) -> p::Placed {
    t.iter().find(|(q, _)| q.as_slice() == path).expect("placed").1.clone()
}

fn layout_prev(sc: &p::Scope, prev: Rc<dyn Fn(&[String]) -> Option<(f64, f64, f64, f64)>>) -> p::Layout {
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
    let prev: Rc<dyn Fn(&[String]) -> Option<(f64, f64, f64, f64)>> = Rc::new(move |path: &[String]| ft.iter().find(|(q, _)| q.as_slice() == path).map(|(_, b)| (b.x, b.y, b.w, b.h)));
    let sticky = table(&layout_prev(&after, prev));
    let relaid = table(&p::layout(&p::LayoutOptions::new(), &after));
    for (path, b) in &first_table {
        let s = find(&sticky, path);
        check(s.x == b.x && s.y == b.y, "an edit that added a node moved a card that was there");
    }
    let pos = |name: &str, l: &[(Vec<String>, p::Placed)]| find(l, &path(&["g", name]));
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
        let mut v: Vec<(Vec<String>, i64, i64)> = l.placed.iter().map(|b| (b.path.clone(), b.x.to_bits() as i64, b.y.to_bits() as i64)).collect();
        v.sort();
        v
    };
    let first_for_prev = first.clone();
    let prev: Rc<dyn Fn(&[String]) -> Option<(f64, f64, f64, f64)>> = Rc::new(move |path: &[String]| first_for_prev.placed.iter().find(|b| b.path.as_slice() == path).map(|b| (b.x, b.y, b.w, b.h)));
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
    let view = scope::select(vec![path(&["g", "b"])], view);
    match scope::inspect::chain(&view) {
        Some((up, down)) => {
            let has = |l: &Vec<Vec<String>>, n: &str| l.contains(&path(&["g", n]));
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
    let lit = scope::select(vec![path(&["g", "a"])], far.clone());
    check(scope::inspect::tagged_wires(&lit) == 0, "selecting the source did not show the real wire");
    let (off, _) = scope::run_command(far, scope::Command::Toggle_tags);
    check(scope::inspect::tagged_wires(&off) == 0, "the tags toggle left a tag");
    // Unfold all: one Unfold per call written in a row of the card
    let ws = checked("(workspace u (graph g :context sop (let* [a (sop/merge (sop/box) (sop/box))] a)))");
    let (view, _) = scope_view(ScopeOpts::default(), &ws, "g");
    let (view, _) = scope_step(&mut ui, view, &frame0());
    let (_, changes) = scope::run_command(scope::select(vec![path(&["g", "a"])], view), scope::Command::Unfold_all);
    let unfolds = changes.iter().filter(|c| matches!(c, scope::Change::Syntax_edit(e::Op::Unfold { .. }))).count();
    check(unfolds >= 2, "Unfold all did not unfold every call of the card");
}

/// 2,001 nodes in one scope: laying it out and painting a frame (the OCaml `bench_scope_big`). Prints
/// timings when `RAYS_BENCH_SCOPE` is set; otherwise one frame is checked so the route stays alive.
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
        let _ = scope::with_scope(ScopeOpts::default(), "g", sc.clone(), scope::create(Some(1000), Some(700)));
    });
    let mut view = scope::with_scope(ScopeOpts::default(), "g", sc.clone(), scope::create(Some(1000), Some(700)));
    for _ in 0..5 {
        view = in_frame(&mut ui, &frame0(), view).0;
    }
    let again = view.clone();
    time("2001 nodes: with_scope again", if verbose { 20 } else { 1 }, &mut || {
        let _ = scope::with_scope(ScopeOpts::default(), "g", sc.clone(), again.clone());
    });
    time("2001 nodes: frame", if verbose { 100 } else { 1 }, &mut || {
        view = in_frame(&mut ui, &frame0(), view.clone()).0;
    });
    let pv = view.clone();
    time("2001 nodes: Point_all", if verbose { 5 } else { 1 }, &mut || {
        let _ = scope::run_command(pv.clone(), scope::Command::Point_all);
    });
    check(scope::inspect::wire_count(&view) > 0, "the big scope has no wires");
}

fn change_equality() {
    use pxui_graph::scope_state::Change;
    let edit = Change::Syntax_edit(e::Op::Rename { node: path(&["g", "a"]), to_: "b".into() });
    let same = edit.clone();
    let different = Change::Syntax_edit(e::Op::Rename { node: path(&["g", "a"]), to_: "c".into() });
    mem_sys::local_reset();
    assert_eq!(edit, same);
    assert_ne!(edit, different);
    assert_eq!(mem_sys::local_alloc_count(), 0, "change equality allocates");
}

pub const CASES: &[(&str, fn())] = &[("change_equality", change_equality), ("run_sticky", run_sticky), ("run_readable", run_readable), ("bench_scope_big", bench_scope_big)];
