//! selection-scoped text, token
//! scrubs, paste, find, symbols and lines. The sections that drive the whole editor
//! (`editor_text`, `editor_binding`, `editor_w9`, `editor_active_scrub`, `editor_command_click`,
//! `editor_literal_scrub`, `editor_text_drop`, `editor_graph_drafts`, `editor_text_find`,
//! `editor_fn_frames`) need `Rays_editor::Editor`: they live in tests/gpu_editor.rs (suite t3)
//! (tests/suite_t3/textpane.rs; the unported ones are listed in port/parity_backlog.md).
use crate::text_pane::{self as t, Find, Shown, Step, Tab};
use editor_document::kinds;
use editor_document::workspace_doc::{self as doc, TextOptions};
use flow_graph::flow_edit::{self as f, ArgKey, Op};
use flow_syntax::phase_timer::SampleGuard;
use flow_syntax::syntax::{self as s, Node, Syntax};
use rays_diagnostic::{self as diagnostic, Position, Span};

type S = Syntax;

fn p(parts: &[&str]) -> flow_check::workspace::Path {
    parts.iter().map(|x| (*x).into()).collect()
}

fn root() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

/// the fixture text: a case file, else the sketch of that name
fn case(name: &str) -> String {
    let cases = root().join("specification/workspace/cases").join(format!("{name}.lisp"));
    std::fs::read_to_string(&cases).or_else(|_| std::fs::read_to_string(root().join("sketches").join(format!("ws_{name}.rays")))).unwrap_or_else(|_| panic!("no case {name}"))
}

fn source(name: &str) -> Vec<S> {
    s::parse(&case(name)).unwrap_or_else(|d| panic!("{}", diagnostic::to_string(&d)))
}

fn contains(text: &str, part: &str) -> bool {
    text.contains(part)
}

fn marked(shown: &Shown) -> String {
    match shown.mark {
        Some((a, b)) => t::sub(&shown.text, a, b - a),
        None => panic!("nothing marked"),
    }
}

fn offset_in(text: &str, part: &str) -> i64 {
    text.find(part).unwrap_or_else(|| panic!("missing token {part}")) as i64
}

fn catalog() -> flow_check::catalog::Catalog {
    kinds::catalog(1, &sop::nodes::factories()).expect("catalog")
}

fn child(form: &S, path: &[i64]) -> S {
    let mut cur = form.clone();
    for i in path {
        cur = cur.child(cur.children()[*i as usize]);
    }
    cur
}

#[test]
fn token_scrubs() {
    let source = s::parse(
        r#"(workspace tokens
    (graph g :context sop
      (let* [a (sop/box :size [11 22 33])
             repeat (for [i (range 2)] (sop/uv_sphere :radius 0.5))
             n 77
             ys (map (fn [(x : float)] (let* [shift (+ x 0.25)] (* shift 2.0))) (array/float 3 0.5))
             b (sop/transform a :uniform_scale (* 3 4))]
        (sop/transform (sop/uv_sphere :radius 0.6) :uniform_scale 0.7))))"#,
    )
    .unwrap();
    let cases: Vec<(flow_check::workspace::Path, ArgKey, Vec<i64>)> = vec![
        (p(&["g", "a"]), ArgKey::Kw("size".into()), vec![1]),
        (p(&["g", "repeat", "@result"]), ArgKey::Kw("radius".into()), vec![]),
        (p(&["g", "n"]), ArgKey::Whole, vec![]),
        (p(&["g", "ys#0", "shift"]), ArgKey::Pos(1), vec![]),
        (p(&["g", "ys#0", "@result"]), ArgKey::Pos(1), vec![]),
        (p(&["g", "b"]), ArgKey::Kw("uniform_scale".into()), vec![1]),
        (p(&["g", "@result#0"]), ArgKey::Kw("radius".into()), vec![]),
    ];
    for (path, key, sub) in &cases {
        let whole = f::arg_text(&source, path, key).unwrap();
        let atom = child(&whole, sub);
        for tab in [Tab::Selection, Tab::Graph, Tab::Document] {
            let shown = t::make_shown(&source, "g", Some(path), tab).unwrap();
            let span = shown.spans.iter().find(|(id, _)| *id == atom.id).map(|(_, sp)| sp.clone()).unwrap();
            let guard = SampleGuard::start_with_clock(Box::new(|| std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_secs_f64()));
            let op = t::scrub_op(&shown, (i64::from(span.start), i64::from(span.finish)), "1234").unwrap();
            let phases = guard.finish();
            assert!(phases.entries.is_empty(), "finding a numeric token ran the workspace pipeline");
            assert!(
                matches!(&op, Op::SetArg { node, key: k, sub: sb, value } if node == path && k == key && sb == sub && matches!(value.node, Node::Num(n) if value.name(n) == "1234")),
                "a printed numeric token targeted the wrong card or argument: {op:?}"
            );
        }
    }
    let path = p(&["g", "ys#0", "shift"]);
    let shown = t::make_shown(&source, "g", Some(&path), Tab::Graph).unwrap();
    let atom = f::arg_text(&source, &path, &ArgKey::Pos(1)).unwrap();
    let span = shown.spans.iter().find(|(id, _)| *id == atom.id).map(|(_, sp)| sp.clone()).unwrap();
    assert_eq!(t::binding_at(&shown, i64::from(span.start)), Some(path.clone()), "the caret in an inline function's body did not select its card");
    assert!(t::binding(&source, &path).is_some(), "an inline function body path did not find its source binding");
    let catalog = catalog();
    let d = doc::of_text(&TextOptions::default(), &catalog, "(workspace w (graph g :context sop (sop/box))) (view :zoom 999)").unwrap();
    let form = s::parse("(graph g :context sop (let* [a (sop/box :size [2 3 4]) b (sop/transform a)] b))").unwrap().remove(0);
    let d = doc::edit(&catalog, &d, &Op::SetGraph { name: "g".into(), form }).unwrap();
    let (printed, spans) = doc::print(&d).unwrap();
    let mut ids: Vec<i64> = spans.iter().map(|(id, _)| *id).collect();
    let n = ids.len();
    ids.sort();
    ids.dedup();
    assert_eq!(ids.len(), n, "document metadata reused source IDs after a structural edit");
    let base = t::make_shown(&d.source, "g", None, Tab::Document).unwrap();
    let shown = Shown { text: printed.clone(), spans: std::sync::Arc::new(spans.clone()), ..base };
    let last: Span = spans.iter().find(|(_, sp)| t::sub(&printed, i64::from(sp.start), i64::from(sp.finish - sp.start)) == "999").map(|(_, sp)| sp.clone()).unwrap();
    assert!(t::scrub_op(&shown, (i64::from(last.start), i64::from(last.finish)), "1000").is_none(), "metadata token aliased a source literal");
}

#[test]
fn selection_text() {
    let src = source("sunflower");
    let sel = |path: &[&str]| t::make_shown(&src, "sunflower", Some(&p(path)), Tab::Selection).unwrap();
    // a binding inside a loop: its top-level ancestor, no unrelated bindings
    let sh = sel(&["sunflower", "seeds_each", "r"]);
    assert!(contains(&sh.text, "(let* [seeds_each (for [i (range seeds)]"), "sunflower closure lost the ancestor");
    assert!(!contains(&sh.text, "head"), "sunflower closure kept a downstream binding");
    assert!(!contains(&sh.text, "(graph"), "the closure is a let*, not the graph");
    assert!(contains(&sh.text, "inputs seeds, spread"), "the closure names the inputs it reads");
    assert_eq!(marked(&sh), "r (* spread (sqrt i))");
    assert_eq!(sh.key, p(&["sunflower", "seeds_each", "r"]), "the selection's key");
    let bound = match t::binding(&src, &p(&["sunflower", "seeds_each", "r"])) {
        Some((_, v)) => flow_syntax::lisp::print(&[v]).unwrap().0.trim().to_owned(),
        None => String::new(),
    };
    assert_eq!(bound, "(* spread (sqrt i))", "binding text");
    // a binding that reads another: the upstream closure comes with it
    let sh = sel(&["sunflower", "head"]);
    assert!(contains(&sh.text, "with 1 upstream binding"), "head has one upstream binding");
    assert!(contains(&sh.text, "seeds_each (for") && contains(&sh.text, "head (sop/merge seeds_each)"), "head closure");
    assert_eq!(marked(&sh), "head (sop/merge seeds_each)", "head mark");
    // the marked text is where the printer put it: printing again finds it
    let bloom = source("bloom");
    let sel = |path: &[&str]| t::make_shown(&bloom, "flower", Some(&p(path)), Tab::Selection).unwrap();
    let sh = sel(&["flower", "ring", "leaf"]);
    assert_eq!(marked(&sh), "leaf (petal :length (+ 0.9 wobble) :width 0.22)");
    assert!(contains(&sh.text, "ring (for") && !contains(&sh.text, "bloom (sop/merge") && !contains(&sh.text, "heart"), "bloom closure of ring is the loop alone");
    let sh = sel(&["flower", "result"]);
    assert!(
        contains(&sh.text, "bloom (sop/merge ring)")
            && contains(&sh.text, "heart (sop/uv_sphere")
            && contains(&sh.text, "ring (for")
            && contains(&sh.text, "with 3 upstream bindings"),
        "bloom result closure"
    );
    assert!(marked(&sh).starts_with("result"), "bloom result mark");
    // no selection: the whole graph, nothing marked; Graph marks the selection
    let sh = t::make_shown(&bloom, "flower", None, Tab::Selection).unwrap();
    assert!(contains(&sh.text, "(graph flower") && sh.mark.is_none() && sh.key == p(&["flower"]), "unselected shows the graph");
    let g = t::make_shown(&bloom, "flower", Some(&p(&["flower", "ring", "leaf"])), Tab::Graph).unwrap();
    assert!(contains(&g.text, "(graph flower") && contains(&g.text, "heart") && g.mark.is_some(), "graph tab");
    assert!(!contains(&g.text, "(defn petal"), "the graph tab shows the active graph only");
    let d = t::make_shown(&bloom, "flower", None, Tab::Document).unwrap();
    assert!(contains(&d.text, "(defn petal") && contains(&d.text, "(graph world"), "document tab is the whole workspace");
    // the caret in a binding is that binding's path; outside every binding it is none
    let offset = |part: &str| offset_in(&g.text, part);
    assert_eq!(t::binding_at(&g, offset("leaf (") + 6), Some(p(&["flower", "ring", "leaf"])), "binding_at: a nested binding");
    assert_eq!(t::binding_at(&g, 0), None, "binding_at: the header is no binding");
    // a call written in an input is a node: the caret in it selects it, and its path finds it
    assert_eq!(t::binding_at(&g, offset("(value/hsv") + 3), Some(p(&["flower", "ring", "tint#:color"])), "binding_at: a nested node");
    assert!(
        matches!(t::binding(&bloom, &p(&["flower", "ring", "tint#:color"])), Some((None, form)) if s::head(&form) == Some("value/hsv")),
        "binding: a nested node is the call written in the input"
    );
    assert_eq!(t::binding_at(&d, 5), None, "binding_at: the Document tab has none");
    for tab in [Tab::Selection, Tab::Graph, Tab::Document] {
        let shown = t::make_shown(&bloom, "flower", Some(&p(&["flower", "ring", "leaf"])), tab).unwrap();
        assert_eq!(
            t::binding_at(&shown, offset_in(&shown.text, "leaf (") + 6),
            Some(p(&["flower", "ring", "leaf"])),
            "all Lisp tabs must select the nested node under the caret"
        );
        assert_eq!(t::definition_at(&shown, offset_in(&shown.text, "(petal :length") + 2), Some(p(&["def:petal"])), "a function call head did not resolve its definition");
    }
    let lexical = s::parse(
        r#"(workspace nav
    (defmacro panel [x] x)
    (graph g :context value
      (let* [motion {:rx 1.0}
             outer motion.rx
             inner (let* [motion {:rx 2.0} leaf motion.rx] (panel leaf))]
        inner)))"#,
    )
    .unwrap();
    assert_eq!(t::reference_target(&lexical, &p(&["g", "outer"]), "motion.rx"), Some(p(&["g", "motion"])), "a dotted output did not resolve its owning binding");
    assert_eq!(
        t::reference_target(&lexical, &p(&["g", "inner", "leaf"]), "motion.rx"),
        Some(p(&["g", "inner", "motion"])),
        "a shadowed name did not resolve in its inner scope"
    );
    let shown = t::make_shown(&lexical, "g", Some(&p(&["g", "inner"])), Tab::Graph).unwrap();
    assert_eq!(t::definition_at(&shown, offset_in(&shown.text, "(panel leaf)") + 2), Some(p(&["macro:panel"])), "a macro call head did not resolve its template");
    let rails = s::parse(
        r#"(workspace rails
    (graph g [(x : float 1.0)] :context value
      (let* [outer x
             loop (for [x (range 2)] (let* [leaf (+ x 1)] leaf))
             f (fn [(x : float)] (let* [leaf (+ x 1.0)] leaf))]
        outer)))"#,
    )
    .unwrap();
    assert_eq!(t::reference_target(&rails, &p(&["g", "outer"]), "x"), Some(p(&["g", ":x"])), "a graph input did not resolve its input card");
    for tab in [Tab::Graph, Tab::Document] {
        assert_eq!(marked(&t::make_shown(&rails, "g", Some(&p(&["g", ":x"])), tab).unwrap()), "(x : float 1.0)", "a referenced graph input was not revealed in Lisp");
    }
    for owner in ["loop", "f"] {
        assert_eq!(t::reference_target(&rails, &p(&["g", owner, "leaf"]), "x"), Some(p(&["g", owner])), "a local rail binder jumped to a shadowed graph input");
    }
    // an error's line: a position, else the span
    let d = diagnostic::error("E_X".into(), "x".into(), None, Some(Position { line: 4, col: 2 }), None, vec![], vec![]);
    assert_eq!(t::line_of("a\nb\nc\nd", &d), Some(4), "line from position");
    let d = diagnostic::error("E_X".into(), "x".into(), Some(Span { start: 4, finish: 5 }), None, None, vec![], vec![]);
    assert_eq!(t::line_of("a\nb\nc\nd", &d), Some(3), "line from span");
}

/// the pure parts of find and go to: matches, stepping, replacing, symbols, lines
#[test]
fn find_pure() {
    let find = Find { query: "ab".into(), replace: "X".into(), case: false, replacing: true, at: 0, grab: false };
    let text = "ab AB xab\nab";
    let matches = t::find_all(false, "ab", text);
    assert_eq!(matches, vec![(0, 2), (3, 5), (7, 9), (10, 12)], "matches fold ASCII case");
    assert_eq!(t::find_all(true, "ab", text), vec![(0, 2), (7, 9), (10, 12)], "matches can count case");
    assert!(t::find_all(false, "", text).is_empty() && t::find_all(false, "aa", "aaaa") == vec![(0, 2), (2, 4)], "an empty query finds nothing and matches do not overlap");
    assert_eq!(t::find_current(&find, &matches), Some((0, 2)), "the current match is the first from the byte");
    assert_eq!(t::find_current(&Find { at: 5, ..find.clone() }, &matches), Some((7, 9)), "the current match skips those before");
    assert_eq!(t::find_current(&Find { at: 11, ..find.clone() }, &matches), Some((0, 2)), "the current match wraps");
    let next = t::find_step(&find, &matches, Step::Next);
    assert_eq!(t::find_current(&next, &matches), Some((3, 5)), "Next goes to the following match");
    assert_eq!(t::find_current(&t::find_step(&Find { at: 10, ..find.clone() }, &matches, Step::Next), &matches), Some((0, 2)), "Next wraps");
    assert_eq!(t::find_current(&t::find_step(&next, &matches, Step::Previous), &matches), Some((0, 2)), "Previous goes back");
    assert_eq!(t::find_current(&t::find_step(&find, &matches, Step::Previous), &matches), Some((10, 12)), "Previous wraps");
    let (r, bar) = t::find_replace(&Find { at: 3, ..find.clone() }, &matches, false).expect("replace one found nothing");
    assert!(r.splice == vec![(3, 5, "X".to_string())] && r.select == (4, 4) && bar.at == 4, "replace one takes the current match");
    let (r, _) = t::find_replace(&find, &matches, true).expect("replace all found nothing");
    assert_eq!(r.splice.len(), 4, "replace all takes every match");
    assert!(t::find_replace(&Find { query: "zz".into(), ..find.clone() }, &[], true).is_none(), "nothing to replace");
    let symbols = t::symbols("(workspace w (graph g :context value (let* [a 1 (b : int) 2 c (+ a 1)] c)) (defn f [x] x))");
    let pairs: Vec<(String, String)> = symbols.into_iter().map(|(n, k, _)| (n, k)).collect();
    let want: Vec<(String, String)> = [("g", "graph"), ("a", "let*"), ("c", "let*"), ("f", "defn")].iter().map(|(a, b)| (a.to_string(), b.to_string())).collect();
    assert_eq!(pairs, want, "symbols are graphs, let* names and functions");
    assert!(t::symbols("(graph g").is_empty(), "text that does not read has no symbols");
    let lines = "one\n\u{e9} two\nthree";
    assert!(t::line_target(lines, "2") == Some(4) && t::line_target(lines, "3:2") == Some(12), "a line and a column");
    assert_eq!(t::line_target(lines, "2:3"), Some(7), "a column counts characters");
    assert!(t::line_target(lines, "99") == Some(11) && t::line_target(lines, "1:99") == Some(3), "past the end is clamped");
    assert!(t::line_target(lines, "x").is_none() && t::line_target(lines, "0").is_none() && t::line_target(lines, "2:y").is_none(), "anything else is refused");
}

/// E6: a paste pairs names with bindings by position and renames at once
#[test]
fn paste() {
    let catalog = catalog();
    let workspace = doc::of_text(&TextOptions::default(), &catalog, "(workspace w (graph g :context sop (let* [bed (sop/box)] bed)))").unwrap();
    let pasted = |text: &str| -> String {
        let ops = t::paste_ops(&workspace.source, "g", &p(&["g"]), text).unwrap_or_else(|m| panic!("{m}"));
        let expressions: Vec<_> = ops.iter().map(|op| match op { Op::AddNode { expr, .. } => expr, _ => panic!("expected pasted binding") }).collect();
        assert!(expressions.iter().all(|expr| std::sync::Arc::ptr_eq(expressions[0].arena(), expr.arena())), "paste froze a separate arena per binding");
        let mut after = workspace.clone();
        for op in &ops {
            after = doc::edit(&catalog, &after, op).unwrap_or_else(|d| panic!("paste of {text}: {}", diagnostic::to_string(&d)));
        }
        doc::to_text(&after).unwrap().replace('\n', " ").split(' ').filter(|w| !w.is_empty()).collect::<Vec<_>>().join(" ")
    };
    // two bare forms with one head are two bindings
    let two = pasted("(sop/box) (sop/box :size [2 2 2])");
    assert!(contains(&two, "box (sop/box)") && contains(&two, "box_1 (sop/box :size [2 2 2])"), "two forms of one head: {two}");
    // [bed] becomes [bed_2]; the pasted [bed_2] is another binding and keeps reading the pasted bed
    let chain = pasted("bed (sop/box) bed_2 (sop/transform bed)");
    assert!(contains(&chain, "bed_2 (sop/box)") && contains(&chain, "bed_2_1 (sop/transform bed_2)"), "a pasted name that is another's new name: {chain}");
}

/// Drafts that disagree with the document, three-way merged on the syntax tree.
#[test]
fn merge_three() {
    let base = "(workspace w (graph a :context sop (sop/box)) (graph b :context sop (sop/box)))";
    let mine = "(workspace w (graph a :context sop (sop/box :size [2 2 2])) (graph b :context sop (sop/box)))";
    let theirs = "(workspace w (graph a :context sop (sop/box)) (graph b :context sop (sop/uv_sphere)))";
    let merged = t::merge3(base, mine, theirs).expect("disjoint edits merge");
    assert!(merged.contains(":size [2 2 2]") && merged.contains("sop/uv_sphere"), "{merged}");
    // both changed the same value
    let both = "(workspace w (graph a :context sop (sop/uv_sphere)) (graph b :context sop (sop/box)))";
    let mine_head = "(workspace w (graph a :context sop (sop/cylinder)) (graph b :context sop (sop/box)))";
    assert!(t::merge3(base, mine_head, both).is_none(), "the same value changed on both sides");
    // a head only they changed, with a keyword only I added, both stay
    assert!(t::merge3(base, mine, both).unwrap().contains(":size [2 2 2]"));
    assert!(t::merge3(base, "(workspace w (graph", theirs).is_none(), "a draft that does not read");
}

#[test]
fn selection_patch_keeps_source_ids_and_comments() {
    let source = s::parse("(workspace w (graph g :context value (let* [a ; value note\n (+ 1 2) b (+ a 3)] b)))").unwrap();
    let root = t::root_form(&source, "g").unwrap();
    let body = root.child(*root.children().last().unwrap());
    let bindings = child(&body, &[1]);
    let old = t::binding(&source, &p(&["g", "a"])).unwrap().1;
    assert_eq!(old.notes(), &["value note".to_owned()]);
    let op = t::graph_op(&source, "g", Some(&p(&["g", "b"])), "(let* [a (+ 4 5)] b)").unwrap();
    let Op::SetGraph { form, .. } = op else { panic!("expected graph patch") };
    let next_body = form.child(*form.children().last().unwrap());
    let next_bindings = child(&next_body, &[1]);
    assert_eq!((form.id, next_body.id, next_bindings.id), (root.id, body.id, bindings.id));
    assert_eq!(child(&next_bindings, &[0]).id, child(&bindings, &[0]).id);
    assert_eq!(child(&next_bindings, &[2]).id, child(&bindings, &[2]).id);
    assert_eq!(child(&next_bindings, &[3]), child(&bindings, &[3]), "omitted binding changed");
    assert_eq!(child(&next_bindings, &[1]).notes(), old.notes());
    assert_eq!(flow_syntax::lisp::flat(&child(&next_bindings, &[1])), "(+ 4 5)");
    assert!(!std::sync::Arc::ptr_eq(root.arena(), form.arena()), "candidate retained its source arena");
    let mut pending = vec![form.row_id()];
    while let Some(id) = pending.pop() {
        let node = form.child(id);
        assert!(std::sync::Arc::ptr_eq(form.arena(), node.arena()));
        pending.extend_from_slice(node.children());
    }
}

/// The diagnostics a later text still shows, the first error and the summary line.
#[test]
fn errors_and_summary() {
    let at = "(workspace w\n  (graph g\n  oops))";
    let d = diagnostic::error("E_X".into(), "bad".into(), Some(Span { start: 25, finish: 29 }), None, None, vec![], vec![]);
    let c = t::Checked { at: at.into(), found: vec![d.clone()] };
    assert_eq!(t::visible(&c, at), vec![d.clone()]);
    // text after the error's line changes: it holds
    assert_eq!(t::visible(&c, &format!("{at}\n; more")), vec![d.clone()]);
    // the text up to the end of its line changes: it is dropped
    assert!(t::visible(&c, "(workspace w\n  (graph g\n  oopz))").is_empty());
    let mut state = t::initial();
    assert!(t::first_error(&state).is_none());
    assert_eq!(t::summary(&state), "selection tab, draft no");
    state.tab = Tab::Document;
    state.draft = Some(at.into());
    state.doc_errors = c;
    assert_eq!(t::first_error(&state), Some(d.clone()));
    assert_eq!(t::summary(&state), format!("document tab, draft yes, error at line 3: {}", diagnostic::to_string(&d)));
    state.find = Some(Find { query: "a\"b".into(), replace: String::new(), case: false, replacing: true, at: 3, grab: false });
    assert!(t::summary(&state).ends_with(", find \"a\\\"b\" at 3 replacing"), "{}", t::summary(&state));
}

/// The shown text is cached by source and tab; a command is pending or a request at once.
#[test]
fn shown_cache_and_commands() {
    let src = source("bloom");
    let state = t::initial();
    let selected = p(&["flower", "ring", "leaf"]);
    let (state, a) = t::shown(None, &state, &src, "flower", Some(&selected)).unwrap();
    assert!(state.cache.is_some());
    let (_, b) = t::shown(None, &state, &src, "flower", Some(&selected)).unwrap();
    assert!(std::sync::Arc::ptr_eq(&a.source, &b.source), "the same source and selection are not shown again");
    let (_, c) = t::shown(None, &state, &src, "flower", None).unwrap();
    assert!(c.mark.is_none() && a.mark.is_some());
    let state = t::command(&state, t::Command::FindNext);
    assert_eq!(state.pending, vec![t::Command::FindNext]);
    assert!(state.request.is_none());
    let state = t::command(&state, t::Command::Select(2, 5));
    assert_eq!(state.request, Some(t::Request { select: (2, 5), splice: vec![] }));
    assert_eq!(state.nonce, 1);
}

/// The editor's Lisp as the text area's language: indentation, bracket pairs, completions,
/// descriptions and parinfer (lisp_text.rs is bundle A's; this section lives in ).
#[test]
fn lisp_text() {
    use crate::lisp_text as l;
    use pxui::ui::CodeContext;
    let indent = |text: &str| flow_check::lisp_scan::indent(text, text.len()).chars().count();
    assert_eq!(indent("(let* [a 1"), 7, "a vector's elements line up under the first");
    assert_eq!(indent("(graph g :context sop"), 2, "a body form indents two in");
    assert_eq!(indent("  (let* [a 1]"), 4, "let* is a body form");
    assert_eq!(indent("(sop/box :size 1"), 9, "a call's arguments line up under the first");
    assert_eq!(indent("(sop/merge"), 1, "a call broken after its head indents one in");
    assert_eq!(indent("(let* [a 1] (+ 1 2)) "), 0, "a closed form indents nothing");
    assert_eq!(indent("(+ 1 ; comment (\n"), 3, "an open bracket in a comment does not count");
    assert_eq!(indent("(str \"(\" "), 5, "an open bracket in a string does not count");
    assert_eq!(indent("(str \"\u{e9}\" (+ 1"), 12, "indentation counts characters, not bytes");
    let in_ = |text: &str, marker: char| {
        let at = text.find(marker).unwrap();
        l::context_at(&format!("{}{}", &text[..at], &text[at + 1..]), at)
    };
    assert!(in_("(f |x)", '|') == CodeContext::Code, "the code between brackets is code");
    assert!(in_("(f \"a|b\")", '|') == CodeContext::String, "inside a string");
    assert!(in_("(f \"ab|\")", '|') == CodeContext::String, "before the closing quote is still inside the string");
    assert!(in_("(f \"ab\"|)", '|') == CodeContext::Code, "after the closing quote is code");
    assert!(in_("(f 1) ; a|b", '|') == CodeContext::Comment, "inside a comment");
    assert!(in_("(f 1) ; ab|", '|') == CodeContext::Comment, "the end of a comment's line is inside it");
    assert!(in_("(f 1) ;| ab\n(g)", '|') == CodeContext::Comment, "right after the semicolon is inside the comment");
    assert!(in_("(f 1) ; ab\n|(g)", '|') == CodeContext::Code, "the next line is code");
    let mut pairs = l::brackets("(a [b] c)");
    pairs.sort();
    assert_eq!(pairs, vec![(0, 8), (3, 5)], "matched pairs");
    assert!(l::brackets("(a ]").is_empty(), "a mismatched closer pairs with nothing");
    // the lexing is kept per text version: a changed text, and the same text again, read right
    let theme = pxui::theme::default();
    let lang = l::language(None, None, None, false, theme);
    assert_eq!(lang.comment, ";", "the Lisp comments with a semicolon");
    let a = (lang.colorize)("(a b)");
    assert!(
        (lang.colorize)("(a b c d)") != a && (lang.colorize)("(a b)") == a && l::brackets("(a b c d)") == vec![(0, 8)] && l::brackets("(a b)") == vec![(0, 4)],
        "a cached lexing answered for another text"
    );
    let colors = (lang.colorize)("(sop/box :size 1.5 \"s\" ; c");
    assert_eq!(colors.len(), 6, "head, keyword, number, string, comment and the paren: {} runs", colors.len());
    let malformed = "(f \"bad\\q\" :next)";
    let later = malformed.find(':').unwrap() as i64;
    assert!((lang.colorize)(malformed).iter().any(|(start, stop, _)| *start <= later && later < *stop), "the tolerant text lexer colors after a malformed escape");
}

fn indent_len(text: &str) -> usize {
    flow_check::lisp_scan::indent(text, text.len()).chars().count()
}

#[test]
fn lisp_text_completions() {
    use crate::lisp_text as l;
    let indent = indent_len;
    // completions over the catalog: ranked, for the token at the caret
    let descriptors = {
        let present: Vec<_> = sop::nodes::factories().iter().map(flow_sop::catalog::descriptor).collect();
        let mut all = flow_sop::catalog::with_library(present);
        all.extend(kinds::descriptors());
        all
    };
    let vocab = l::vocab(vec![], &descriptors);
    let names = l::no_names();
    let labels = |text: &str| -> Vec<String> { l::complete(&names, &[], &vocab, text, text.len()).into_iter().map(|c| c.label).collect() };
    let first = |text: &str| labels(text).into_iter().next().unwrap_or_default();
    let doc = "(workspace w (graph g :context sop [(depth : int 5)] (let* [ring (sop/box :size_x 2) b (";
    let d = |tail: &str| format!("{doc}{tail}");
    // a head: the context's kinds first, prefix matches before fuzzy ones
    assert_eq!(first(&d("sop/tra")), "sop/transform", "head completion");
    assert!(labels(&d("sop/tra")).iter().all(|x| x.starts_with("sop/tra") || pxui::ui::fuzzy_match("sop/tra", x)), "every head completion matches");
    assert!(labels(&d("le")).contains(&"let*".to_string()) && labels(&d("me")).contains(&"sop/merge".to_string()), "forms and kinds complete at a head");
    // every special form of the checker completes at a head, and nothing the language lacks does
    for form in flow_check::workspace::special_forms() {
        assert!(labels(&d(form)).contains(&form.to_string()), "the form {form} does not complete");
    }
    assert!(!labels(&d("whe")).contains(&"when".to_string()) && indent("(do") == 1 && indent("(let*") == 2, "a form the language does not have is offered or indented as one");
    // an operator's own name being typed has no argument place yet (E1: it raised)
    assert!(labels(&d("max")).contains(&"max".to_string()) && labels(&d("+")).contains(&"+".to_string()), "an operator's head completes");
    // the empty head lists the kinds the text uses first
    assert_eq!(first(doc), "sop/box", "the used kind ranks first");
    // a keyword: the kind's parameters, a vec3 group before its components, present ones left out
    assert_eq!(first(&d("sop/box :si")), ":size", "keyword completion");
    assert!(labels(&d("sop/box :si")).contains(&":size_x".to_string()), "the group's components follow");
    assert!(!labels(&d("sop/box :size [1 1 1] :s")).contains(&":size".to_string()), "a keyword already given is left out");
    assert_eq!(first(&d("sop/transform ring :rot")), ":rotate", "rotate group");
    // a choice after its keyword, with or without the opening quote
    assert!(
        labels(&d("sop/box :connectivity \"Qu")).contains(&"Quads".to_string()) && labels(&d("sop/box :connectivity Qu")).contains(&"Quads".to_string()),
        "choices complete after their keyword"
    );
    // the text choices of the layout forms: a keyword's, and the split axis at its place
    let ui = "(workspace w (graph e :context editor (";
    let sorted = |text: &str| {
        let mut v = labels(text);
        v.sort();
        v
    };
    assert_eq!(sorted(&format!("{ui}ui/lisp :tab \"")), ["document", "graph", "selection"], "a lisp panel's tabs complete");
    assert_eq!(labels(&format!("{ui}ui/graph :view \"l")), ["list"], "a graph panel's views complete");
    assert!(sorted(&format!("{ui}ui/split \"")) == ["horizontal", "vertical"] && labels(&format!("{ui}ui/split_at \"v")) == ["vertical"], "the split axis completes");
    assert!(labels(&format!("{ui}ui/split \"vertical\" a b :first_size \"")).is_empty(), "a size completes no text");
    let text = d("sop/box :connectivity \"Qu");
    let c = l::complete(&names, &[], &vocab, &text, doc.len() + 24);
    assert_eq!(c.first().map(|c| c.insert.as_str()), Some("\"Quads\""), "a choice inserts its quotes");
    // an argument: the bindings and inputs in scope before constants; naming a new binding offers nothing
    assert_eq!(first(&d("sop/transform ri")), "ring", "binding completion");
    assert!(labels(&d("sop/transform ring :uniform_scale de")).contains(&"depth".to_string()), "graph inputs complete");
    assert!(labels(&d("sop/transform ring :uniform_scale t")).contains(&"t".to_string()), "t completes");
    assert!(labels("(workspace w (graph g :context sop [] (let* [ri").is_empty(), "a new binding's name is not completed");
    assert!(labels(&d("sop/box :size 1")).is_empty(), "a number completes nothing");
    // the operator's keyword and the context keyword
    assert!(labels(&d("sop/curve ring :cl")).contains(&":closed".to_string()), "an operator's keyword");
    assert_eq!(first("(workspace w (graph g :context sc"), "scene", "contexts after :context");
    // descriptions
    let describe = |text: &str, at: i64| l::describe(&vocab, text, at as usize).map(|(_, _, d)| d).unwrap_or_default();
    let index_of = |part: &str| doc.find(part).unwrap() as i64;
    assert!(describe(doc, index_of("sop/box") + 2).contains("Box"), "a kind's description");
    assert!(describe(doc, index_of(":size_x") + 3).contains("Size X"), "a parameter's description");
    let with_box = format!("{doc}sop/box :size [1 1 1])");
    assert!(describe(&with_box, doc.len() as i64 + 10).contains("Size"), "a group's description");
    assert!(describe(doc, 15).contains("reactive graph"), "graph form");
    assert_eq!(describe(doc, doc.len() as i64 - 1), "", "an open paren has no description");
    let with_ring = format!("{doc}sop/transform ring)");
    assert!(describe(&with_ring, doc.len() as i64 + 15).contains("let* binding"), "a binding's description");
    assert!(describe(doc, index_of("int 5") + 4).contains("drag"), "a number's description");
}

#[test]
fn lisp_text_numbers_and_parinfer() {
    use crate::lisp_text as l;
    let theme = pxui::theme::default();
    // numbers
    assert_eq!(l::number_at("(sop/box :size_x 1.25 :x 3)", 19), Some((17, 21)), "the number at a byte");
    assert_eq!(l::number_at("(sop/box :size_x 1.25 :x 3)", 16), None, "a space is no number");
    // parinfer: closers follow indentation, the printed workspaces are already in that shape
    let pf = |text: &str| flow_syntax::parinfer::rewrite(text, 0).0;
    for name in ["bloom", "sunflower", "rosette"] {
        let printed = flow_syntax::lisp::print(&source(name)).unwrap().0;
        assert_eq!(pf(&printed), printed, "{name}: parinfer changed the printed workspace");
    }
    assert_eq!(pf("(a\n  b"), "(a\n  b)", "an open form closes at its last line");
    assert_eq!(pf("(a\nb)"), "(a)\nb", "a dedented line closes the form before it");
    assert_eq!(pf("(a (b)\n  c)"), "(a (b)\n  c)", "a closer in the middle of a line stays");
    assert_eq!(pf("(a \"(\" ; )\n  b"), "(a \"(\" ; )\n  b)", "strings and comments are not brackets");
    assert_eq!(pf("(a]"), "(a)", "a closer matching nothing is replaced");
    // E22: a string runs over line breaks: nothing inside it is a bracket or an indentation
    assert_eq!(pf("(a \"x\n(y]\" b"), "(a \"x\n(y]\" b)", "a string over two lines");
    assert_eq!(pf("(a\n  (b \"x\ny\")\n(c)"), "(a\n  (b \"x\ny\"))\n(c)", "closers after a string's last line");
    assert_eq!(l::brackets("(a \"x\n)\" b)"), vec![(0, 10)], "a bracket on a string's second line is not one");
    assert_eq!(pf("(let* [a 1\n       b 2]\n  a)"), "(let* [a 1\n       b 2]\n  a)", "a vector over two lines");
    assert_eq!(pf("(a\n  )"), "(a)\n", "a line of closers alone becomes blank");
    assert_eq!(flow_syntax::parinfer::rewrite("(a \n  )", 6), ("(a) \n  ".to_string(), 7), "Enter before a closer keeps the caret's indentation");
    assert_eq!(flow_syntax::parinfer::rewrite("(a", 2), ("(a)".to_string(), 2), "the caret stays before an inferred closer");
    assert_eq!(flow_syntax::parinfer::rewrite("(a\nb)", 4), ("(a)\nb".to_string(), 5), "the caret follows a moved closer");
    assert_eq!(flow_syntax::parinfer::rewrite("(a  )", 3), ("(a )".to_string(), 3), "a blank typed before a closer survives on the caret's line");
    assert!(
        l::language(None, None, None, true, theme).rewrite.is_some() && l::language(None, None, None, false, theme).rewrite.is_none(),
        "the language rewrites only with parinfer on"
    );
}

#[test]
fn line_targets_keep_unicode_boundaries() {
    let text = "aé中🦀e\u{301}\n終\n";
    for (column, byte) in [0, 1, 3, 6, 10, 11, 13, 13].into_iter().enumerate() {
        let target = t::line_target(text, &format!("1:{}", column + 1)).unwrap();
        assert_eq!(target, byte);
        assert!(text.is_char_boundary(target as usize));
    }
    assert_eq!(t::line_target(text, "2:99"), Some(17));
    assert_eq!(t::line_target(text, "3"), Some(18));
    assert_eq!(t::line_target(text, "9223372036854775807:9223372036854775807"), Some(18));
    assert_eq!(t::line_target("", "1:99"), Some(0));
    assert_eq!(t::line_target(text, "1:-1"), None);
    assert_eq!(t::references("(café café)\n(café)", "café"), vec![(1, "(café café)".into()), (7, "(café café)".into()), (15, "(café)".into())]);
}
