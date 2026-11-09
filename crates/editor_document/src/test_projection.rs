//! Port of lib/flow_sop/test_projection.ml (W4 part A): the graph pane's projection of every
//! fixture (nodes, zones, rows, chips), liveness and invariance flags, and the layout.
//!
//! Uses the editor's complete kinds catalog, retaining the study's graph and layout projections.
use flow_sop::lisp_sop::support::*;
use flow_sop::{catalog as fcat, port};
use flow_sop::lisp_sop;
use flow_check::catalog::Catalog;
use flow_check::workspace as w;
use flow_graph::flow_edit::{self as e, ArgKey, Op};
use flow_graph::projection as p;
use flow_syntax::syntax::{Node, Syntax};
use flow_syntax::{lisp, ty::Ty};
use param::Value;

const SOP_ONLY: [&str; 10] = ["facade", "garland", "kit", "orrery", "rosette", "sunflower", "tiles", "tree", "tunnel", "wave"];

fn catalog() -> Catalog {
    crate::kinds::catalog(1, &lisp_sop::factories()).unwrap()
}

fn workspace_of(text: &str) -> w::Workspace {
    let forms = parse(text);
    match w::check(&[], false, false, None, &catalog(), &forms) {
        (Some(ws), _) => ws,
        (None, ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    }
}

fn load(name: &str) -> w::Workspace {
    workspace_of(&case_text(name))
}

fn scope(ws: &w::Workspace, g: &str) -> p::Scope {
    p::of_graph(&catalog(), ws, g).unwrap_or_else(|m| panic!("{m}"))
}

fn node(ws: &w::Workspace, g: &str, path: &[&str]) -> p::Node {
    let mut full = vec![g.to_owned()];
    full.extend(path.iter().map(|x| (*x).to_owned()));
    p::find(&scope(ws, g), &full).unwrap_or_else(|| panic!("{} is not projected", full.join("/")))
}

fn zone(n: &p::Node) -> p::Zone {
    n.zone.clone().unwrap_or_else(|| panic!("{} is not a zone", n.name))
}

fn flat(e: &Option<Syntax>) -> String {
    e.as_ref().map(lisp::flat).unwrap_or_default()
}

fn kw(name: &str) -> ArgKey {
    ArgKey::Kw(name.to_owned())
}

fn sv(l: &[&str]) -> Vec<String> {
    l.iter().map(|x| (*x).to_owned()).collect()
}

/// every node, zone and row of a scope, as (nodes, zones, rows)
fn counts(sc: &p::Scope) -> (usize, usize, usize) {
    let mut t = (0, 0, 0);
    for n in &sc.nodes {
        let (a, b, c) = n.zone.as_ref().map_or((0, 0, 0), |z| counts(&z.scope));
        t = (t.0 + 1 + a, t.1 + usize::from(n.zone.is_some()) + b, t.2 + n.rows.len() + c);
    }
    t
}

fn graph_names(ws: &w::Workspace) -> Vec<String> {
    ws.graphs.iter().chain(ws.defs.iter()).map(|g| g.name.clone()).collect()
}

fn apply(text: &str, op: Op) -> w::Workspace {
    let forms = e::apply(&catalog(), &parse(text), &op).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    match w::check(&[], false, false, None, &catalog(), &forms) {
        (Some(ws), _) => ws,
        _ => panic!("{} failed to check", e::label(&op)),
    }
}

fn num(t: &str) -> Syntax {
    let mut b = flow_syntax::arena::SyntaxBuilder::new();
    let name = b.intern(t);
    let id = b.push(0, Node::Num(name), rays_diagnostic::Span { start: 0, finish: 0 }, None);
    b.freeze().handle(id)
}

fn text(t: &str) -> Syntax {
    parse(t).remove(0)
}

fn has_row(n: &p::Node, key: &str) -> bool {
    n.rows.iter().any(|r| r.key == kw(key))
}

#[test]
fn noise3_rows_edit_and_survive_reload() {
    let source = "(workspace noise (graph g :context value (let* [field (noise3 [t 0.7 -0.2])] field)))";
    let forms = parse(source);
    let checked = workspace_of(source);
    let card = p::find(&scope(&checked, "g"), &sv(&["g", "field"])).unwrap();
    assert!(card.ty == Ty::Float && card.rows.len() == 5, "noise3 position, seed, octaves, lacunarity and gain must be editable rows");
    let expr = text("[0.3 0.7 -0.2]");
    // Workspace_doc.edit is Flow_edit.apply_checked on the document's source
    let (edited_forms, edited) = e::apply_checked(&catalog(), &forms, &Op::SetArg { node: sv(&["g", "field"]), key: ArgKey::Pos(0), sub: vec![], value: expr }).unwrap();
    let value = flow_eval::run(&edited, false, None, None, None, 7.0).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let r = value.results.iter().find(|(n, _)| n == "g").unwrap().1.clone();
    assert!(matches!(r, flow_eval::Value::Float(x) if (0.0..=1.0).contains(&x)), "noise3 row edit did not evaluate");
    flow_sop::lower::workspace(None, Vec::new(), flow_sop::lower::Options::default(), &forms).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    // Workspace_doc.to_text / of_text: print and check again
    let reloaded = workspace_of(&lisp::print(&edited_forms).unwrap().0);
    let again = flow_eval::run(&reloaded, false, None, None, None, 7.0).unwrap();
    assert!(again.results == value.results, "noise3 was lost on document reload");
}

fn snapshot_of(name: &str) -> Vec<(&'static str, (usize, usize, usize))> {
    match name {
        "bloom" => vec![("flower", (13, 1, 86)), ("scene", (6, 0, 62)), ("world", (2, 0, 11)), ("settings", (2, 0, 7)), ("editor", (10, 0, 29)), ("half", (1, 0, 2)), ("petal", (2, 0, 53))],
        "facade" => vec![("facade", (18, 2, 97))],
        "garland" => vec![("garland", (23, 5, 98)), ("ring", (7, 1, 47))],
        // inline functions and nested maps
        "kit" => vec![("kit", (17, 2, 114)), ("window", (5, 0, 34))],
        "orrery" => vec![("orrery", (21, 1, 117))],
        // conditional arms are inputs, not scopes
        "rosette" => vec![("rosette", (4, 0, 32))],
        "sunflower" => vec![("sunflower", (9, 1, 36))],
        "tiles" => vec![("tiles", (17, 1, 84))],
        "tree" => vec![("tree", (9, 1, 184))],
        "tunnel" => vec![("rings", (5, 1, 63))],
        "variations" => vec![("garden", (6, 0, 70)), ("scene", (1, 0, 10)), ("editor", (8, 0, 22))],
        "wave" => vec![("wave", (7, 2, 46))],
        _ => panic!("no snapshot"),
    }
}

fn check_snapshot(name: &str) {
    let ws = load(name);
    let got: Vec<(String, (usize, usize, usize))> = graph_names(&ws).into_iter().map(|g| (g.clone(), counts(&scope(&ws, &g)))).collect();
    let expected: Vec<(String, (usize, usize, usize))> = snapshot_of(name).into_iter().map(|(g, c)| (g.to_owned(), c)).collect();
    assert!(got == expected, "{name}: node, zone and row counts changed: {got:?}");
}

#[test]
fn snapshots() {
    for name in SOP_ONLY {
        check_snapshot(name);
    }
}

#[test]
fn snapshots_of_the_scene_fixtures() {
    check_snapshot("bloom");
    check_snapshot("variations");
}

fn kinds(n: &p::Node) -> Vec<(String, p::RowKind)> {
    n.rows.iter().map(|r| (r.label.clone(), r.kind)).collect()
}

#[test]
fn rows_of_a_keyword_first_call() {
    // a keyword written before a positional argument: the input is still input 0, and wired
    let first = node(&workspace_of("(workspace w (graph g :context sop (let* [a (sop/box) b (sop/transform :translate [1 2 3] a)] b)))"), "g", &["b"]);
    match first.rows.iter().find(|r| r.key == ArgKey::Pos(0)) {
        Some(r) => assert!(flat(&r.expr) == "a" && p::sources(r) == vec!["a".to_owned()], "a keyword-first call lost its input"),
        None => panic!("a keyword-first call has no input row"),
    }
    assert!(first.rows.iter().any(|r| r.key == kw("translate") && flat(&r.expr) == "[1 2 3]"), "a keyword-first call lost its keyword");
    // a note travels with its binding
    let g = load("garland");
    assert!(node(&g, "garland", &["beads"]).note.is_none(), "no note");
    let w = workspace_of("(workspace w (graph g :context sop (let* [; a note\n a (sop/box)] a)))");
    assert_eq!(node(&w, "g", &["a"]).note, Some("a note".to_owned()), "note carried");
}

#[test]
fn rows_of_the_bloom_zone_calls_and_macros() {
    let w = load("bloom");
    // a zone: rail with the loop variable, captures, and the yield
    let ring = node(&w, "flower", &["ring"]);
    let z = zone(&ring);
    assert!(z.kind == p::ZoneKind::For && z.yield_label == "collect" && ring.head == "for", "ring is a for zone");
    assert!(
        z.rail.iter().map(|r| (r.name.clone(), r.role)).collect::<Vec<_>>() == vec![("i".to_owned(), p::Role::Var), ("petals".to_owned(), p::Role::Capture), ("seed".to_owned(), p::Role::Capture)],
        "ring rail: the variable, then the names it captures"
    );
    let i = &z.rail[0];
    assert!(i.key == Some(ArgKey::Bv(1, 1)) && flat(&i.expr) == "(range petals)" && i.ty == Some(Ty::Int), "the loop variable edits its collection through Bv");
    // the four bindings, the three calls nested in them, and the result last
    assert!(z.scope.nodes.len() == 8 && z.scope.nodes[7].synthetic && z.scope.result == p::ScopeResult::Node(sv(&["flower", "ring", "@result"])), "the body's result is a synthetic node");
    let names: Vec<String> = z.scope.nodes.iter().map(|n| n.name.clone()).collect();
    assert!(names.contains(&"wobble#1".to_owned()) && names.contains(&"tint#:color".to_owned()), "a call written in an input is a node");
    let hsv = node(&w, "flower", &["ring", "tint#:color"]);
    assert!(p::anonymous(&hsv) && p::title(&hsv) == "hsv" && hsv.head == "value/hsv" && !hsv.synthetic, "a nested node is titled by its kind");
    // a call: a rest slot with its add row, chips
    let bloom = node(&w, "flower", &["bloom"]);
    assert!(
        kinds(&bloom) == vec![("input".to_owned(), p::RowKind::Rest), ("+ input".to_owned(), p::RowKind::Add), ("source_attribute".to_owned(), p::RowKind::Arg), ("source_base".to_owned(), p::RowKind::Arg)],
        "merge is one rest slot, an add row, then its parameters"
    );
    let (a, add) = (&bloom.rows[0], &bloom.rows[1]);
    assert!(a.key == ArgKey::Pos(0) && a.chip == p::Chip::Name("ring".into()) && a.ty == Some(flow_syntax::ty::geometry()), "merge input wire");
    assert!(add.key == ArgKey::Pos(1) && add.chip == p::Chip::NoValue, "the add row goes one past the last input");
    let tint = node(&w, "flower", &["ring", "tint"]);
    assert!(tint.rows.iter().map(|r| r.label.as_str()).collect::<Vec<_>>() == ["in0", "group", "owner", "color", "alpha"], "set_color rows: the slot, then the parameters");
    let color = tint.rows.iter().find(|r| r.label == "color").unwrap();
    assert!(color.key == kw("color") && color.chip == p::Chip::Name("tint#:color".into()) && p::sources(color) == vec!["tint#:color".to_owned()], "the row a nested node is written in is wired from it");
    let group = tint.rows.iter().find(|r| r.label == "group").unwrap();
    assert!(group.kind == p::RowKind::GroupReader && group.chip == p::Chip::NoValue && group.default == Some(String::new()), "group reader row");
    let leaf = node(&w, "flower", &["ring", "leaf"]);
    assert!(
        leaf.head == "petal" && leaf.rows.iter().map(|r| r.key.clone()).collect::<Vec<_>>() == vec![kw("length"), kw("width")] && leaf.rows[0].default == Some("1.0".to_owned()) && leaf.rows[0].ty == Some(Ty::Float),
        "a defn call lists its inputs with their defaults"
    );
    // graph inputs, and the root scope
    let sc = scope(&w, "flower");
    assert!(
        sc.inputs.iter().map(|i| (i.name.clone(), flat(&i.default))).collect::<Vec<_>>() == vec![("petals".to_owned(), "12".to_owned()), ("seed".to_owned(), "7".to_owned())] && sc.result == p::ScopeResult::Link("result".into()),
        "root inputs and result link"
    );
}

#[test]
fn rows_of_macros_records_folds_and_lets() {
    // macros: holes and binders
    let r = load("rosette");
    let outer = node(&r, "rosette", &["outer"]);
    assert!(outer.macro_.as_deref() == Some("radial") && kinds(&outer) == vec![("i".to_owned(), p::RowKind::Binder), ("n".to_owned(), p::RowKind::Hole), ("body".to_owned(), p::RowKind::Hole)], "macro call rows");
    assert!(outer.rows[0].chip == p::Chip::Name("k".into()), "the caller supplies the binder's name");
    assert!(node(&r, "rosette", &["soft"]).bypass && !outer.bypass, "bypass flag");
    // records: output rows, patterns
    let k = load("kit");
    let window = node(&k, "kit", &["big"]);
    assert!(window.outputs.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>() == ["frame", "pane", "area"], "a values result has output rows");
    let pat = node(&k, "kit", &["[left mid right]"]);
    assert!(pat.outputs.iter().map(|(n, _)| n.as_str()).collect::<Vec<_>>() == ["left", "mid", "right"] && pat.binds == sv(&["left", "mid", "right"]), "destructuring outputs");
    let tower = node(&k, "kit", &["tower"]);
    let acc = zone(&tower).rail[0].clone();
    assert!(zone(&tower).kind == p::ZoneKind::Fold && acc.role == p::Role::Acc && acc.key == Some(ArgKey::Bv(1, 1)) && zone(&tower).yield_label == "next", "fold accumulator row");
    let result = node(&k, "kit", &["tower", "@result"]);
    assert!(
        result.head == "record" && result.rows.iter().any(|r| r.kind == p::RowKind::Add && r.key == ArgKey::Field(String::new())) && result.rows.iter().any(|r| r.key == ArgKey::Field("shape".into())),
        "record rows are fields plus an add row"
    );
    let part = node(&k, "kit", &["tower", "part"]);
    assert!(part.head == "cond" && kinds(&part).contains(&("else".to_owned(), p::RowKind::Arg)), "cond rows");
    // an arm is an input of its conditional: a name there is a wired row, not a zone
    let arms: Vec<Vec<String>> = part.rows.iter().filter(|r| matches!(r.key, ArgKey::Arm(_))).map(p::sources).collect();
    assert!(arms == vec![sv(&["big"]), sv(&["small"]), sv(&["small"])] && p::find(&scope(&k, "kit"), &sv(&["kit", "tower", "part#then"])).is_none(), "kit cond arms are rows of the cond card");
    let style = zone(&node(&k, "kit", &["style"]));
    assert!(style.kind == p::ZoneKind::Fn && style.yield_label == "return" && style.rail[0].role == p::Role::Param, "a bound fn is a zone with a param rail");
    // let* scopes are zones too
    let f = load("facade");
    let marked = node(&f, "facade", &["marked"]);
    assert!(
        zone(&marked).kind == p::ZoneKind::Let && zone(&marked).yield_label == "result" && zone(&marked).rail.iter().map(|r| (r.name.clone(), r.role)).collect::<Vec<_>>() == vec![("glass".to_owned(), p::Role::Capture)],
        "a nested let* is a zone that captures"
    );
}

fn live_names(sc: &p::Scope) -> Vec<String> {
    let mut out = Vec::new();
    for n in &sc.nodes {
        if n.live {
            out.push(n.name.clone());
        }
        if let Some(z) = &n.zone {
            out.extend(live_names(&z.scope));
        }
    }
    out
}

fn invariant_names(sc: &p::Scope) -> Vec<String> {
    let mut out = Vec::new();
    for n in &sc.nodes {
        if n.invariant {
            out.push(n.name.clone());
        }
        if let Some(z) = &n.zone {
            out.extend(invariant_names(&z.scope));
        }
    }
    out
}

#[test]
fn flags() {
    let w = load("orrery");
    let sc = scope(&w, "orrery");
    let expected = ["spin", "pulse", "sun", "glow#:color", "glow", "moons_each", "a", "bob", "moon#:center", "moon", "@result#:color", "@result", "orbit", "system"];
    assert!(live_names(&sc) == expected, "Orrery: live paths changed: {}", live_names(&sc).join(", "));
    let base = node(&w, "orrery", &["base"]);
    let moons = node(&w, "orrery", &["moons_each"]);
    assert!(!base.live && moons.live && !zone(&moons).scope.nodes.is_empty(), "a live zone");
    // bloom is a scene fixture; its graphs are checked in the ignored test below
    for name in ["sunflower", "tree", "tiles"] {
        let w = load(name);
        for g in graph_names(&w) {
            assert!(live_names(&scope(&w, &g)).is_empty(), "{name}/{g} has no live node");
        }
    }
    assert!(!live_names(&scope(&load("wave"), "wave")).is_empty(), "Wave is live");
    assert!(invariant_names(&scope(&load("sunflower"), "sunflower")) == ["turn"], "Sunflower: turn is the same each seed");
    assert!(invariant_names(&scope(&load("tree"), "tree")) == ["up"], "Tree: up is the same each level");
}

#[test]
fn bloom_has_no_live_node() {
    let w = load("bloom");
    for g in graph_names(&w) {
        assert!(live_names(&scope(&w, &g)).is_empty(), "bloom/{g} has no live node");
    }
}

// ---- layout ----

type Boxed = (f64, f64, f64, f64);

/// absolute (x, y, w, h) of every placed item, zones' children included, outer first
fn place(l: &p::Layout) -> Vec<(Vec<String>, Boxed)> {
    fn go(ox: f64, oy: f64, l: &p::Layout, out: &mut Vec<(Vec<String>, Boxed)>) {
        for q in &l.placed {
            let (ax, ay) = (ox + q.x, oy + q.y);
            out.push((q.path.clone(), (ax, ay, q.w, q.h)));
            if let (Some(inner), p::Item::Item(n)) = (&q.inner, &q.item) {
                if let Some(z) = &n.zone {
                    go(ax + p::zone_pad_in(z), ay + p::rail_top(n), inner, out);
                }
            }
        }
    }
    let mut out = Vec::new();
    go(0.0, 0.0, l, &mut out);
    out
}

fn assoc(places: &[(Vec<String>, Boxed)], path: &[String]) -> Boxed {
    places.iter().find(|(p, _)| p == path).unwrap_or_else(|| panic!("unplaced {}", path.join("/"))).1
}

fn overlaps(a: Boxed, b: Boxed) -> bool {
    a.0 < b.0 + b.2 && b.0 < a.0 + a.2 && a.1 < b.1 + b.3 && b.1 < a.1 + a.3
}

fn inside(parent: Boxed, child: Boxed) -> bool {
    child.0 >= parent.0 && child.1 >= parent.1 && child.0 + child.2 <= parent.0 + parent.2 && child.1 + child.3 <= parent.1 + parent.3
}

fn check_layout(name: &str, l: &p::Layout) {
    for (i, a) in l.placed.iter().enumerate() {
        for b in &l.placed[i + 1..] {
            assert!(!overlaps((a.x, a.y, a.w, a.h), (b.x, b.y, b.w, b.h)), "{name}: {} overlaps {}", a.path.join("/"), b.path.join("/"));
        }
    }
    for q in &l.placed {
        assert!(inside((0.0, 0.0, l.w, l.h), (q.x, q.y, q.w, q.h)), "{name}: a node is outside its scope");
        if let Some(inner) = &q.inner {
            // the zone's body has room for its whole inner layout, between its padding
            let (pad, top) = match &q.item {
                p::Item::Item(n) => (n.zone.as_ref().map_or(p::ZONE_PAD_X, p::zone_pad_in), p::rail_top(n)),
                _ => panic!("item"),
            };
            assert!(pad + inner.w + p::ZONE_PAD_X <= q.w && top + inner.h <= q.h, "{name}: a zone is smaller than its body");
            check_layout(name, inner);
        }
    }
}

fn layout_of(ws: &w::Workspace, name: &str) {
    for g in graph_names(ws) {
        let sc = scope(ws, &g);
        let opts = p::LayoutOptions::new();
        let l = p::layout(&opts, &sc);
        check_layout(&format!("{name}/{g}"), &l);
        assert!(p::layout(&opts, &sc) == l, "{name}/{g}: layout is deterministic");
        // absolute boxes: every zone child lies inside its zone
        let abs = place(&l);
        for z in p::zones(&sc) {
            let zb = assoc(&abs, &z.path);
            for c in &zone(&z).scope.nodes {
                match abs.iter().find(|(p, _)| *p == c.path) {
                    Some((_, cb)) => assert!(inside(zb, *cb), "{name}: {} leaves its zone", c.path.join("/")),
                    None => panic!("unplaced {}", c.path.join("/")),
                }
            }
        }
    }
}

#[test]
fn layout() {
    for name in SOP_ONLY {
        layout_of(&load(name), name);
    }
    let w = load("sunflower");
    let sc = scope(&w, "sunflower");
    let opts = p::LayoutOptions::new();
    let l = p::layout(&opts, &sc);
    let head = sv(&["sunflower", "head"]);
    // inputs come first, the return last, and a node sits right of what it reads
    let x = |path: &[&str]| assoc(&place(&l), &sv(path)).0;
    assert!(x(&["sunflower", ":seeds"]) < x(&["sunflower", "seeds_each"]) && x(&["sunflower", "seeds_each"]) < x(&["sunflower", "head"]), "columns follow the dependencies");
    // a graph whose result is a node has no return card: the displayed node is the result
    assert!(!l.placed.iter().any(|q| q.item == p::Item::Return), "no return card for a linked result");
    // a position override moves one node; a collapsed zone is a card
    let h2 = head.clone();
    let moved = p::layout(&p::LayoutOptions { at: std::rc::Rc::new(move |p: &[String]| if p == h2.as_slice() { Some((500.0, 300.0)) } else { None }), ..opts.clone() }, &sc);
    let (hx, hy, _, _) = assoc(&place(&moved), &head);
    assert!(hx == 500.0 && hy == 300.0, "at overrides the position");
    let z = sv(&["sunflower", "seeds_each"]);
    let open_h = assoc(&place(&l), &z).3;
    let z2 = z.clone();
    let collapsed = p::layout(&p::LayoutOptions { collapsed: std::rc::Rc::new(move |p: &[String]| p == z2.as_slice()), ..opts.clone() }, &sc);
    let (_, _, cw, ch) = assoc(&place(&collapsed), &z);
    assert!(cw == p::NODE_WIDTH && ch < open_h && place(&collapsed).len() < place(&l).len(), "a collapsed zone is a card without its children");
    // the study's card geometry, on the 24-point grid
    let q = l.placed.iter().find(|q| q.path == head).unwrap();
    assert!(q.w == p::NODE_WIDTH && q.h == p::card_height(q.lines.len() as f64, 0.0), "node size");
    // cards and columns sit on the dot lattice; the card's rows are the exposure rule's
    for q in &l.placed {
        match &q.item {
            p::Item::Item(n) if n.zone.is_none() => assert!(q.x % p::LATTICE == 0.0 && q.y % p::LATTICE == 0.0, "off the lattice: {}", q.path.join("/")),
            p::Item::Input(_) => assert!(q.x % p::LATTICE == 0.0 && q.y % p::LATTICE == 0.0, "off the lattice: {}", q.path.join("/")),
            _ => {}
        }
    }
    let head_h = |o: &p::LayoutOptions| p::layout(o, &sc).placed.into_iter().find(|q| q.path == head).unwrap().h;
    let full = |foot: bool| head_h(&p::LayoutOptions { foot, level: std::rc::Rc::new(|_| p::Level::Full), ..opts.clone() });
    assert!(
        p::card_height(0.0, 0.0) == p::HEAD_HEIGHT
            && p::card_height(3.0, 0.0) == 100.0
            && full(true) == full(false) + p::FOOT_HEIGHT
            && head_h(&p::LayoutOptions { foot: true, ..opts.clone() }) == q.h,
        "kit card heights: header only 24, three rows 100, a footer row 24 more on a full card only"
    );
}

#[test]
fn layout_of_the_scene_fixtures() {
    layout_of(&load("bloom"), "bloom");
    layout_of(&load("variations"), "variations");
}

#[test]
fn an_input_sits_just_left_of_its_reader() {
    let w = workspace_of("(workspace n (graph g :context value [(k : float 2.0)] (let* [a (+ t 1.0) b (* a 2.0) c (+ b k)] c)))");
    let l = p::layout(&p::LayoutOptions::new(), &scope(&w, "g"));
    let col = |path: &[&str]| assoc(&place(&l), &sv(path)).0;
    assert!(col(&["g", ":k"]) > col(&["g", "a"]) && col(&["g", ":k"]) < col(&["g", "c"]), "an input sits just left of its reader");
}

// a loop over the points or pieces of geometry says how its elements are ordered
#[test]
fn zone_order() {
    let order = |t: &str| -> Option<String> {
        let w = workspace_of(t);
        match scope(&w, "g").nodes.first() {
            Some(n) => match &n.zone {
                Some(z) => z.order.clone(),
                None => panic!("not a zone"),
            },
            None => panic!("no node"),
        }
    };
    assert_eq!(order("(workspace w (graph g :context sop (let* [z (for [p (sop/point_list (sop/grid))] (sop/box))] (sop/merge z))))").as_deref(), Some("by index"), "a loop over points is ordered by index");
    assert_eq!(
        order("(workspace w (graph g :context sop (let* [z (for [p (sop/piece_list (sop/grid) :key \"id\")] (sop/box))] (sop/merge z))))").as_deref(),
        Some("by id"),
        "a loop over pieces keyed by an attribute is ordered by it"
    );
    let w = workspace_of("(workspace w (graph g :context sop (let* [pts (sop/point_list (sop/grid)) z (for [p pts] (sop/box))] (sop/merge z))))");
    assert!(scope(&w, "g").nodes.iter().any(|n| n.zone.as_ref().is_some_and(|z| z.order.as_deref() == Some("by index"))), "a loop over a bound point list is ordered by index too");
    assert_eq!(order("(workspace w (graph g :context sop (let* [z (for [i (range 3)] (sop/box))] (sop/merge z))))"), None, "a loop over a range has no order to show");
}

// a row pinned onto a card shows there, and one pinned off leaves it: [lines ~pin] and
// [row_shown] read the layout's per-row flags
#[test]
fn pins() {
    let w = workspace_of("(workspace w (graph g :context sop (let* [s (sop/scatter (sop/grid) :count 50)] (sop/merge s))))");
    let n = node(&w, "g", &["s"]);
    let group = n.rows.iter().find(|r| r.label == "group").expect("scatter has no group row").clone();
    let count = n.rows.iter().find(|r| r.label == "count").expect("scatter has no count row").clone();
    let on_card = |pin: &dyn Fn(&str) -> Option<bool>| p::lines(pin, p::Level::Card, &n).iter().any(|l| matches!(l, p::Line::Row(_, r) if r.label == "group"));
    assert!(!p::row_shown(None, &group), "an unset group is off the card by default");
    assert!(p::row_shown(Some(true), &group), "a pin shows it");
    assert!(!on_card(&|_| None), "the card leaves it out");
    assert!(on_card(&|label| if label == "group" { Some(true) } else { None }), "a pinned row is on the card");
    assert!(p::row_shown(None, &count) && !p::row_shown(Some(false), &count), "a pin of false takes a written row off the card");
    // what the text says is drawn: a default written out stays on the card
    let radius = |w: &w::Workspace| node(w, "g", &["s"]).rows.into_iter().find(|r| r.label == "radius").unwrap();
    let bare = radius(&workspace_of("(workspace w (graph g :context sop (let* [s (sop/uv_sphere)] (sop/merge s))))"));
    let written = radius(&workspace_of(&format!("(workspace w (graph g :context sop (let* [s (sop/uv_sphere :radius {})] (sop/merge s))))", bare.default.clone().unwrap())));
    assert!(!p::row_shown(None, &bare) && p::row_shown(None, &written), "a written default is not on the card");
}

#[test]
fn index_choice_port_and_switch_projection() {
    let schema = param::schema("switch".to_owned(), vec![param::field("input".to_owned(), param::index_choice(vec!["a".into(), "b".into()]).unwrap(), Value::Int(0)).unwrap()]).unwrap();
    let ports = port::parameters(false, &schema.view(&[Value::Int(0)]).unwrap()).unwrap();
    let prt = port::find_parameter(&ports, "input").unwrap();
    assert!(prt.ty == Some(flow_check::port_type::PortType::Int), "index choice must expose an integer port");
    assert!(
        port::normalize(&prt, &flow_check::port_type::PortValue::Int(1)).unwrap() == (flow_check::port_type::PortValue::Int(1), vec![("input".to_owned(), Value::Int(1))]),
        "index choice must accept an integer drive"
    );
    assert!(port::normalize(&prt, &flow_check::port_type::PortValue::Int(2)).is_err(), "index choice must refuse out-of-range drives");
    let source = "(workspace w (graph g :context sop (let* [a (sop/box) b (sop/grid) s (sop/switch a b :input 1)] s)))";
    let selected = node(&workspace_of(source), "g", &["s"]);
    let row = selected.rows.iter().find(|r| r.key == kw("input")).unwrap();
    assert!(row.ty == Some(Ty::Int) && row.socket && flat(&row.expr) == "1", "Switch selection must project as an editable integer row");
    let workspace = apply(source, Op::SetArg { node: sv(&["g", "s"]), key: kw("input"), sub: vec![], value: num("0") });
    let selected = node(&workspace, "g", &["s"]);
    assert!(selected.rows.iter().any(|r| r.key == kw("input") && flat(&r.expr) == "0"), "Switch selection must edit through Flow_edit");
}

#[test]
fn table_valued_node_fields_project_and_edit() {
    let transform_source = "(workspace w (graph g :context sop (let* [a (sop/box) transformed (sop/transform a :mode \"Matrix\" :m03 2 :shear_xy 0.1 :pivot [1 2 3])] transformed)))";
    let transformed = node(&workspace_of(transform_source), "g", &["transformed"]);
    for keyword in ["mode", "m03", "shear_xy", "pivot"] {
        assert!(has_row(&transformed, keyword), "Transform projects {keyword}");
    }
    let workspace = apply(transform_source, Op::SetArg { node: sv(&["g", "transformed"]), key: kw("m03"), sub: vec![], value: num("4") });
    assert!(node(&workspace, "g", &["transformed"]).rows.iter().any(|r| r.key == kw("m03") && flat(&r.expr) == "4"), "Transform matrix fields edit through Flow_edit");

    let randomize_source = "(workspace w (graph g :context sop (let* [source (sop/box) random (sop/attribute_randomize source :distribution \"Custom discrete\" :entries \"0\\t1\\n1\\t2\" :selection_owner \"Vertex\" :minimum_vector [0 0 0])] random)))";
    let random = node(&workspace_of(randomize_source), "g", &["random"]);
    for keyword in ["entries", "selection_owner", "selection_group", "minimum_vector"] {
        assert!(has_row(&random, keyword), "Randomize projects {keyword}");
    }
    let workspace = apply(randomize_source, Op::SetArg { node: sv(&["g", "random"]), key: kw("entries"), sub: vec![], value: text("\"2\\t1\"") });
    assert!(node(&workspace, "g", &["random"]).rows.iter().any(|r| r.key == kw("entries") && r.expr.is_some()), "Randomize tables edit through Flow_edit");

    let interpolate_source = "(workspace w (graph g :context sop (let* [source (sop/box) target (sop/box) interpolated (sop/attribute_interpolate source target :attributes \"point\\tCd\\tcolor\" :driver \"Point weights\" :threshold 0.01)] interpolated)))";
    let interpolated = node(&workspace_of(interpolate_source), "g", &["interpolated"]);
    for keyword in ["attributes", "driver", "numbers_attribute", "computed_owner"] {
        assert!(has_row(&interpolated, keyword), "Interpolate projects {keyword}");
    }
    let workspace = apply(interpolate_source, Op::SetArg { node: sv(&["g", "interpolated"]), key: kw("attributes"), sub: vec![], value: text("\"vertex\\tN\\tnormal\"") });
    assert!(node(&workspace, "g", &["interpolated"]).rows.iter().any(|r| r.key == kw("attributes") && r.expr.is_some()), "Interpolate table edits through Flow_edit");

    let transfer_source = "(workspace w (graph g :context sop (let* [source (sop/box) target (sop/box) transferred (sop/attribute_transfer source target :use_names true :names \"Cd\\nN\" :distance_mode \"Auto\")] transferred)))";
    let transferred = node(&workspace_of(transfer_source), "g", &["transferred"]);
    for keyword in ["use_names", "names", "distance_mode"] {
        assert!(has_row(&transferred, keyword), "Transfer projects {keyword}");
    }
    let workspace = apply(transfer_source, Op::SetArg { node: sv(&["g", "transferred"]), key: kw("names"), sub: vec![], value: text("\"Cd\"") });
    assert!(node(&workspace, "g", &["transferred"]).rows.iter().any(|r| r.key == kw("names") && flat(&r.expr) == "\"Cd\""), "Transfer exact names edit through Flow_edit");
}
