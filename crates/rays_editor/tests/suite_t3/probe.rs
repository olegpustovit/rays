//! Port of lib/rays_editor/test/test_probe.ml (W5: recorded values, footers, iteration counts,
//! inspector rows and the viewport header text).
use super::pg::{self, check};
use super::support::*;
use flow_check::workspace::Workspace;
use flow_eval::Value;
use flow_graph::flow_edit as fe;
use flow_graph::probe::{self, Footer, Geometry, Options, Probe, Summary};
use flow_graph::projection as p;
use pxui_graph::scope_pane as scope;
use pxui_graph::scope_state::{Change, ScopeOpts};
use std::rc::Rc;

fn fail(message: &str) -> ! {
    panic!("test_probe: {message}")
}

pub const SEEDS: i64 = 240;
pub const DEPTH: i64 = 5;
pub const CELLS: i64 = 8;
pub const BEADS: i64 = 14;

pub fn fixture(name: &str) -> String {
    read_case(name)
}

pub fn load(name: &str) -> Workspace {
    checked(&fixture(name))
}

pub fn workspace_of(text: &str) -> Workspace {
    checked(text)
}

pub fn scope_of(w: &Workspace, graph: &str) -> p::Scope {
    p::of_graph(&catalog(), w, graph).unwrap_or_else(|e| panic!("{e}"))
}

pub fn node_of(w: &Workspace, graph: &str, path: &[&str]) -> p::Node {
    let mut full = vec![graph.to_string()];
    full.extend(path.iter().map(|s| s.to_string()));
    p::find(&scope_of(w, graph), &full).unwrap_or_else(|| panic!("{} is not projected", full.join("/")))
}

fn eval_recorded(w: &Workspace) -> flow_eval::Eval {
    flow_eval::r#static(w, true, None).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

fn recorded(w: &Workspace) -> Probe {
    probe::make(&eval_recorded(w), Options::default())
}

fn path(parts: &[&str]) -> Vec<String> {
    parts.iter().map(|s| s.to_string()).collect()
}

pub fn bounds() {
    // two loops call one function 4,096 times each: 8,192 records at the parameter, 4,096 kept
    let w = workspace_of("(workspace w (defn f :context value [(x : int)] (+ x 1)) (graph g :context value (let* [a (for [i (range 4096)] (f i)) b (for [j (range 4096)] (f j))] (count a))))");
    let eval = eval_recorded(&w);
    check(!eval.records.is_empty() && eval.records.iter().all(|(_, l)| l.len() <= 4096), "a path recorded more than 4,096 values");
    check(eval.records.iter().any(|(_, l)| l.len() == 4096), "the bound was never reached");
    // geometry is a reference, never geometry; the cook's counts fill it in
    let sunflower = load("sunflower");
    let eval = eval_recorded(&sunflower);
    let head = &eval.records.iter().find(|(p, _)| *p == path(&["sunflower", "head"])).expect("head record").1;
    check(matches!(head.as_slice(), [(i, Value::Deferred(flow_syntax::ty::Ty::Named(n), _))] if i.is_empty() && n == "geometry"), "geometry records a plan node id only");
}

pub fn sunflower() {
    let w = load("sunflower");
    let scope = scope_of(&w, "sunflower");
    let mut r = recorded(&w);
    let zone = path(&["sunflower", "seeds_each"]);
    let node = |name: &str| node_of(&w, "sunflower", &["seeds_each", name]);
    // the value at the probe, a sparkline across the loop, x n
    let f = r.footer(&node("r"), &[5]);
    check(f.value == "0.139" && f.runs == Some(SEEDS) && !f.invariant, &format!("r footer: {}", probe::text(&f)));
    match &f.spark {
        Some((values, at)) => check(values.len() == SEEDS as usize && *at == 5, "r sparkline spans the iterations"),
        None => fail("r has no sparkline"),
    }
    check(probe::text(&f) == format!("0.139 · ×{SEEDS}"), "r footer text");
    // the invariant one offers to leave the loop and draws no sparkline
    let f = r.footer(&node("turn"), &[5]);
    check(f.invariant && f.spark.is_none() && f.runs.is_none() && probe::text(&f).contains("↑ same each time"), &format!("turn footer: {}", probe::text(&f)));
    // outside a zone: the type when nothing was recorded there, the count for geometry
    let head = node_of(&w, "sunflower", &["head"]);
    let f = r.footer(&head, &[]);
    check(f.value == "geometry" && f.runs.is_none() && !f.invariant, "head: geometry without counts");
    let geometry = Rc::new(|_: i64| Some(Geometry { points: 1204, prims: 1440, groups: vec!["a".into(), "b".into()], extent: None, seconds: Some(0.003), attributes: Vec::new() }));
    let mut r2 = probe::make(&eval_recorded(&w), Options { geometry: Some(geometry), ..Default::default() });
    check(r2.footer(&head, &[]).value == "1440 prims · groups a, b · 1 204 pts · 0.003 s", "head: cooked point and primitive counts beside timing");
    let nowhere = p::Node { path: path(&["nowhere"]), ..head.clone() };
    check(r2.footer(&nowhere, &[]).value == "geometry", "an unrecorded node shows its type");
    // iterations: the inspector list
    let its = r.iterations(&node("r"), &[0]);
    check(its.len() == SEEDS as usize && its[0] == "0" && its[1] == "0.062" && its[SEEDS as usize - 1] != its[1], "iteration list");
    check(r.readouts(&node("r"), &[5]) == vec![("value at probe".to_string(), "0.139".to_string()), ("cook".to_string(), "cached".to_string())], "inspector rows");
    check(r.readouts(&head, &[]) == vec![("value".to_string(), "geometry".to_string()), ("cook".to_string(), "cached".to_string())], "inspector rows outside a zone");
    // counts under the probes
    check(r.counts(&scope, &|_| 0) == vec![(zone, SEEDS)], "zone count");
}

pub fn tree() {
    let w = load("tree");
    let mut r = recorded(&w);
    let up = node_of(&w, "tree", &["crown", "up"]);
    let f = r.footer(&up, &[2]);
    check(f.invariant && f.value == "[0 1 0]", &format!("up footer: {}", probe::text(&f)));
    let a = node_of(&w, "tree", &["crown", "a"]);
    check(!r.footer(&a, &[2]).invariant, "a changes with the level");
    // the fold's accumulator: one value per step
    check(r.counts(&scope_of(&w, "tree"), &|_| 0) == vec![(path(&["tree", "crown"]), DEPTH)], "fold steps");
}

pub fn branches_tiles() {
    // if: how many iterations took each arm
    let w = load("tiles");
    let mut r = recorded(&w);
    let angle = node_of(&w, "tiles", &["cells_each", "angle"]);
    let f = r.footer(&angle, &[3]);
    match &f.branch {
        Some(text) => {
            let nums: Vec<i64> = text.split(|c: char| !c.is_ascii_digit()).filter(|s| !s.is_empty()).map(|s| s.parse().unwrap()).collect();
            check(text.starts_with("then ") && nums.len() == 2 && nums[0] + nums[1] == CELLS * CELLS && nums[0] > 0 && nums[1] > 0, "then and else add up to the runs");
        }
        None => fail("an if on a name shows then/else"),
    }
    check(f.runs == Some(CELLS * CELLS), "tiles: runs");
}

pub fn branches_garland() {    // filter: kept a of b
    let w = load("garland");
    let eval = eval_recorded(&w);
    let mut r = probe::make(&eval, Options::default());
    let big = node_of(&w, "garland", &["big"]);
    let f = r.footer(&big, &[]);
    let length = |p: &[&str]| -> usize {
        let key = path(p);
        match &eval.records.iter().find(|(k, _)| *k == key).expect("record").1[..] {
            [(_, Value::List(xs))] => xs.len(),
            _ => fail("not a list"),
        }
    };
    check(
        f.kept == Some(format!("kept {} of {}", length(&["garland", "big"]), length(&["garland", "sizes"]))) && length(&["garland", "sizes"]) == BEADS as usize,
        &format!("filter footer: {}", probe::text(&f)),
    );
}

pub fn live() {
    let w = load("orrery");
    let (spin, base) = (node_of(&w, "orrery", &["spin"]), node_of(&w, "orrery", &["base"]));
    check(spin.live && !base.live, "orrery liveness");
    let ev = eval_recorded(&w);
    let at = |time: f64, n: &p::Node, probes: &[i64]| -> Footer { probe::make(&ev, Options { time: Some(time), ..Default::default() }).footer(n, probes) };
    check(at(1., &spin, &[]).value == "0.8" && at(2., &spin, &[]).value == "1.6" && at(1., &spin, &[]).live, "a live value is forced at the time shown");
    check(probe::text(&at(1., &spin, &[])).contains(" · t"), "the live tag");
    check(!at(1., &base, &[]).live, "a cached node carries no tag");
    let bob = node_of(&w, "orrery", &["moons_each", "bob"]);
    match (at(1., &bob, &[3]).spark, at(2., &bob, &[3]).spark) {
        (Some((a, _)), Some((b, _))) => check(a.len() == 24 && a != b, "a live sparkline moves with t"),
        _ => fail("bob has no sparkline"),
    }
    let mut r1 = probe::make(&ev, Options { time: Some(1.), ..Default::default() });
    check(r1.readouts(&spin, &[]) == vec![("value".to_string(), "0.8".to_string()), ("cook".to_string(), "live, recooks every frame".to_string())], "live inspector rows");
    // the viewport header
    let lower = |name: &str| {
        let forms = flow_syntax::syntax::parse(&fixture(name)).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
        flow_sop::lower::workspace(None, editor_document::kinds::descriptors(), flow_sop::lower::Options::default(), &forms).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
    };
    let lowered = lower("orrery");
    let (live_count, cached) = flow_sop::lower::counts(&lowered);
    check(live_count > 0 && cached > 0, "orrery has live and cached nodes");
    check(flow_sop::lower::status(&lowered, 0.0013) == Some(format!("t {live_count} live · {cached} cached · frame cook 1.3 ms")), "viewport header text");
    let lowered = lower("sunflower");
    check(flow_sop::lower::status(&lowered, 0.001).is_none(), "a static document has no live header");
}

pub fn nested() {
    // an inner zone counts per outer iteration, not the whole product
    let w = workspace_of("(workspace w (graph g :context sop [(n : int 3)] (let* [rows (for [i (range n)] (let* [cols (for [j (range (+ i 1))] (sop/box))] (sop/merge cols)))] (sop/merge rows))))");
    let scope = scope_of(&w, "g");
    let mut r = recorded(&w);
    let (outer, inner) = (path(&["g", "rows"]), path(&["g", "rows", "cols"]));
    let mut counts = |k: i64| r.counts(&scope, &|p| if p == outer.as_slice() { k } else { 0 });
    check(counts(0) == vec![(outer.clone(), 3), (inner.clone(), 1)] && counts(1) == vec![(outer.clone(), 3), (inner.clone(), 2)] && counts(2) == vec![(outer.clone(), 3), (inner.clone(), 3)], "nested counts follow the outer probe");
    let mut key = inner.clone();
    key.push(":j".into());
    check(r.records(&key).len() == 6, "the whole product is 6 records");
}

/// every zone shows this iteration
struct FixedProbe(i64);
impl pxui_graph::scope_state::Saved for FixedProbe {
    fn probe(&self, _: &[String]) -> i64 {
        self.0
    }
}

/// the hoist button of an invariant footer is one request
pub fn hoist() {
    let w = load("sunflower");
    let (view, _) = scope_view(ScopeOpts { saved: Rc::new(FixedProbe(5)), ..Default::default() }, &w, "sunflower");
    let mut ui = pg::new_ui();
    let (view, _) = pg::scope_step(&mut ui, view, &pg::frame0());
    let turn = path(&["sunflower", "seeds_each", "turn"]);
    let (x, y, tw, th) = scope::inspect::box_of(&view, &turn).expect("turn box");
    let z = scope::zoom(&view);
    let point = ((x + tw - 30. * z) as i64, (y + th - 10. * z) as i64);
    let (_, _changes) = pg::scope_click(&mut ui, view.clone(), point);
    // the footer's hoist button moved to the context menu; a node that varies has none
    let r = path(&["sunflower", "seeds_each", "r"]);
    let (x, y, tw, th) = scope::inspect::box_of(&view, &r).expect("r box");
    let (_, changes) = pg::scope_click(&mut ui, view, ((x + tw - 30. * z) as i64, (y + th - 10. * z) as i64));
    check(!changes.iter().any(|c| matches!(c, Change::Syntax_edit(fe::Op::Hoist { .. }))), "r offered a hoist");
    ui.destroy();
}

/// a selection that an edit removed does not survive the next layout
pub fn selection() {
    let w = load("sunflower");
    let (view, sc) = scope_view(ScopeOpts::default(), &w, "sunflower");
    let turn = path(&["sunflower", "seeds_each", "turn"]);
    let view = { let mut r8_view = view; scope::select(vec![turn.clone(), path(&["sunflower", "gone"])], &mut r8_view); r8_view };
    let view = { let mut r8_view = view; scope::with_scope(ScopeOpts::default(), "sunflower", sc, &mut r8_view); r8_view };
    check(scope::selected(&view) == vec![turn], "the selection kept a path that no longer exists");
}

/// W8: a loop over geometry has one template record; the count comes from the cook
pub fn geometry_zone() {
    let w = workspace_of("(workspace w (graph g :context sop (let* [f (sop/grid) d (for [p (sop/point_list f)] (let* [b (sop/box) m (sop/transform b :translate p)] m)) r (sop/merge f d)] r)))");
    let scope = scope_of(&w, "g");
    let zone = path(&["g", "d"]);
    let z2 = zone.clone();
    let dynamic: Rc<dyn Fn(&[String]) -> Option<i64>> = Rc::new(move |p| if p == z2.as_slice() { Some(100) } else { None });
    let eval = eval_recorded(&w);
    let before_cook = probe::make(&eval, Options::default()).counts(&scope, &|_| 0);
    check(before_cook == vec![(zone.clone(), 1)], "template count before a cook");
    let mut r = probe::make(&eval, Options { dynamic: Some(dynamic.clone()), ..Default::default() });
    check(r.counts(&scope, &|_| 0) == vec![(zone.clone(), 100)], "the cook's element count");
    let m = node_of(&w, "g", &["d", "m"]);
    check(r.footer(&m, &[41]).value != "not run here", "every element reads the template record");
    check(probe::make(&eval, Options::default()).footer(&m, &[41]).value == "not run here", "without a count only iteration 0 ran");
    // a value that reads the element is forced for the element the probe names, once the zone cooked
    let w = workspace_of("(workspace w (graph g :context sop (let* [f (sop/grid) d (for [p (sop/point_list f)] (let* [q (+ p [1 0 0]) m (sop/transform (sop/box) :translate q)] m)) r (sop/merge f d)] r)))");
    let eval = eval_recorded(&w);
    let q = node_of(&w, "g", &["d", "q"]);
    check(probe::make(&eval, Options { dynamic: Some(dynamic.clone()), ..Default::default() }).footer(&q, &[3]).value == "?", "an element-dependent value reads ? before any element is known");
    let key = eval
        .plan
        .nodes
        .iter()
        .find_map(|n| match (n.kind.as_str(), n.args.iter().find(|(k, _)| k == "element")) {
            ("zone/points", Some((_, Value::Text(k)))) => Some(String::from_utf8_lossy(k).into_owned()),
            _ => None,
        })
        .expect("a zone/points element key");
    let z3 = zone.clone();
    let element: Rc<dyn Fn(&[String], i64) -> Option<Vec<(String, Value)>>> = Rc::new(move |p, k| if p == z3.as_slice() { Some(vec![(key.clone(), Value::Vec3(k as f64, 0., 0.))]) } else { None });
    let mut r = probe::make(&eval, Options { dynamic: Some(dynamic), element: Some(element), ..Default::default() });
    check(r.footer(&q, &[3]).value == "[4 0 0]" && r.footer(&q, &[41]).value == "[42 0 0]", "an element-dependent value is not forced for the probed element");
    // and the inspector's list of iterations has every element's value
    let every = r.iterations(&q, &[3]);
    check(every.len() == 100 && every[41] == "[42 0 0]" && every[0] == "[1 0 0]", "the per-iteration list of a loop over geometry is not forced for every element");
}

/// a footer counts a node that is not upstream of the display too: the cook is asked for it, and a
/// failure there never fails the display (Async_cook.submit_some)
pub fn off_display() {
    use rays_editor::cook;
    let text = "(workspace w (graph g :context sop (let* [a (sop/box) orphan (sop/uv_sphere) r (sop/transform a)] r)))";
    let forms = flow_syntax::syntax::parse(text).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let lowered = flow_sop::lower::workspace(None, editor_document::kinds::descriptors(), flow_sop::lower::Options::default(), &forms).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let compiled = |site: &[&str]| -> Option<i64> {
        let site = path(site);
        lowered.plan.nodes.iter().find(|n| n.site == site).and_then(|n| lowered.compiled.get(&n.id).copied())
    };
    let orphan = compiled(&["g", "orphan"]).expect("orphan");
    let shown = compiled(&["g", "r"]).expect("r");
    let cook = cook::create(prepare_surface(), 1, 97, Some(1), Some(true), None, 512, 256 * 1024 * 1024).expect("cook");
    let objects = flow_sop::lower::objects(&lowered);
    let objects: Vec<(i64, flow_sop::network::Network, i64)> = objects.into_iter().map(|(id, n, d)| (id as i64, n, d)).collect();
    let object_id = objects[0].0;
    let mut frame = super::t3c::frame_at((0., 0.), vec![], 0);
    frame.input.dt = 0.;
    let updated = cook::update(None, None, false, &[(object_id, shown), (object_id, orphan)], &rays_editor::pick::Set::default(), None, &cook, &super::cook_t::none_settings(), &objects.iter().map(|(i, n, d)| (*i, n, *d)).collect::<Vec<_>>(), None, param::Effects::default(), &[], &rays_editor::timeline::create(), &frame, None);
    check(updated.edit_error.is_none(), "the probed nodes failed the display");
    let counted = |node: i64| cook::geometry(&updated.cook, object_id, node).cloned();
    check(counted(orphan).is_some_and(|g: Geometry| g.prims > 0), "a node off the display has no count");
    check(counted(shown).is_some_and(|g: Geometry| g.prims > 0), "a node on the display has no count");
    cook::close(&updated.cook);
}

/// register L16: a skipped iteration is still an iteration of the selector (the tuples do not move),
/// its body has no record, and the sparkline marks where the probe sits among the ones that ran
pub fn skips() {
    let w = workspace_of("(workspace w (graph g :context value (let* [xs (for [i (range 5)] :skip [1 3] (let* [sq (* i i)] sq))] (count xs))))");
    let scope = scope_of(&w, "g");
    let mut r = recorded(&w);
    let sq = node_of(&w, "g", &["xs", "sq"]);
    check(r.counts(&scope, &|_| 0) == vec![(path(&["g", "xs"]), 5)], "the selector counts the skipped iterations");
    check(r.records(&path(&["g", "xs", ":i"])).len() == 5 && r.records(&path(&["g", "xs", "sq"])).len() == 3, "the loop variable is recorded 5 times, the body 3");
    let f = r.footer(&sq, &[2]);
    check(f.value == "4" && f.runs == Some(3), &format!("footer at a run iteration: {}", probe::text(&f)));
    check(r.footer(&sq, &[1]).value == "not run here", "footer at a skipped iteration");
    match (&f.spark, r.footer(&sq, &[4]).spark) {
        (Some((values, at)), Some((_, last))) => check(*values == vec![0., 4., 16.] && *at == 1 && last == 2, "the sparkline places the probe among the iterations that ran"),
        _ => fail("no sparkline"),
    }
    check(r.iterations(&sq, &[0]).len() == 3, "the inspector lists the iterations that ran");
    // geometry: the plan node of a probed iteration, none for a skipped one
    let g = workspace_of("(workspace w (graph g :context sop (let* [b (for [i (range 4)] :skip [1] (sop/box :size (+ 1 i)))] (sop/merge b))))");
    let rg = recorded(&g);
    let _ = rg;
    let eval = eval_recorded(&g);
    let plan = eval.plan.clone();
    let mut rg = probe::make(&eval, Options::default());
    let result = path(&["g", "b", "@result"]);
    check(rg.plan_node(None, &result, &[1]).is_none(), "a skipped iteration has no plan node to pick");
    for k in [0i64, 2, 3] {
        match rg.plan_node(None, &result, &[k]) {
            Some(id) => {
                let n = &plan.nodes[id as usize];
                check(n.kind == flow_eval::card_kind() && n.args.iter().any(|(name, value)| name == "kind" && value == &Value::Text(b"sop/box".to_vec())) && n.iter == vec![k] && n.site == result, &format!("a card names its call at its own tuple: {} {:?} {:?}", n.kind, n.iter, n.site));
            }
            None => fail("an iteration that ran has no plan node"),
        }
    }
}

/// a probe with a clock stops past its budget: an expired clock leaves the map's count out at once
/// and the records skip the rest, where one without a clock runs the map to the end
pub fn budget() {
    let w = workspace_of(
        r#"(workspace budget_probe
    (graph g :context value
      (let* [xs (map (fn [(x : float)] (+ x (frame/dt))) (array/float 5000 1.0))
             ys (map (fn [(x : float)] (* x 2.0)) xs)]
        (array/count ys))))"#,
    );
    let evaluated = eval_recorded(&w);
    let scope = scope_of(&w, "g");
    let live = input::at_time(0.5);
    let ys = (path(&["g", "ys#0"]), 5000);
    let count = |r: &mut Probe| r.counts(&scope, &|_| 0);
    let unbudgeted = count(&mut probe::make(&evaluated, Options { live: Some(live.clone()), ..Default::default() }));
    check(unbudgeted.contains(&ys), &format!("an unbudgeted probe counts the map: {unbudgeted:?}"));
    let now = Rc::new(std::cell::Cell::new(0.0f64));
    let n2 = now.clone();
    // the clock is Send + Sync in Options: a shared atomic counter plays the OCaml ref
    let ticks = std::sync::Arc::new(std::sync::atomic::AtomicU64::new(0));
    let t2 = ticks.clone();
    let clock: std::sync::Arc<dyn Fn() -> f64 + Send + Sync> = std::sync::Arc::new(move || (t2.fetch_add(1, std::sync::atomic::Ordering::SeqCst) + 1) as f64);
    let _ = (n2, now);
    let mut records = probe::make(&evaluated, Options { clock: Some(clock), live: Some(live), ..Default::default() });
    check(!count(&mut records).iter().any(|(p, _)| *p == ys.0), "an expired probe stops");
    let before = ticks.load(std::sync::atomic::Ordering::SeqCst);
    let _ = count(&mut records);
    check(ticks.load(std::sync::atomic::Ordering::SeqCst) == before, "the records skip evaluation once one ran over");
}

pub fn compiled_inputs() {
    let w = workspace_of(
        r#"(workspace compiled_probe
    (graph g :context value
      (let* [tick (state [n 0] (+ n 1))
             xs (map (fn [(x : float)] (+ x (frame/dt)))
                     (array/float (+ 5000 (frame/index)) 1.0))
             ys (map (fn [(x : float)]
                       (let* [value (+ (+ x (frame/width)) tick)] value)) xs)]
        (array/count ys))))"#,
    );
    let evaluated = eval_recorded(&w);
    let scope = scope_of(&w, "g");
    let state = flow_eval::create_state();
    let calls = std::rc::Rc::new(std::cell::Cell::new(0));
    let c2 = calls.clone();
    let force: probe::Force = Rc::new(move |state, elems, resolve, value, live| {
        c2.set(c2.get() + 1);
        use flow_ir::flow_ir::executor;
        let program = executor::compile(value, &executor::CompileOptions::default())?;
        let o = executor::ForceOptions { state: state.cloned(), elems: elems.map(|e| e.to_vec()), resolve, ..Default::default() };
        executor::force(&program, &o, live)
    });
    for frame in [0i64, 1, 3, 0] {
        let live = input::Frame { frame, dt: 0.125, size: (321, 123), ..input::at_time(frame as f64 / 60.) };
        let stamp = flow_eval::state_stamp(&state);
        let mut old = probe::make(&evaluated, Options { state: Some(state.clone()), live: Some(live.clone()), ..Default::default() });
        let mut compiled = probe::make(&evaluated, Options { force: Some(force.clone()), state: Some(state.clone()), live: Some(live.clone()), ..Default::default() });
        let (co, cc) = (old.counts(&scope, &|_| 4999), compiled.counts(&scope, &|_| 4999));
        check(co == cc && cc.contains(&(path(&["g", "ys#0"]), 5000 + frame)), &format!("compiled probe inputs changed live selector lengths: old {co:?} compiled {cc:?}"));
        let node = node_of(&w, "g", &["ys#0", "value"]);
        for index in [0i64, 4095, 4999] {
            check(old.at(&node.path, &[index]) == compiled.at(&node.path, &[index]), "compiled inputs changed the selected reference body");
            check(old.footer(&node, &[index]) == compiled.footer(&node, &[index]), "compiled inputs changed the sampled sparkline/footer");
        }
        check(flow_eval::state_stamp(&state) == stamp, "compiled probes advanced the host fold");
    }
    check(calls.get() > 0, "live map inputs bypassed the host compiler");
}

pub fn run() {
    let images = workspace_of("(workspace w (graph g :context value (let* [picture (image/noise :width 3 :height 2)] 0)))");
    let evaluated = eval_recorded(&images);
    let node = node_of(&images, "g", &["picture"]);
    check(probe::make(&evaluated, Options::default()).footer(&node, &[]).value == "3 × 2 image", "image footer reports noise dimensions");
    let dimensions = Rc::new(std::cell::Cell::new(Some((3i64, 2i64))));
    let d2 = dimensions.clone();
    let mut borrowed = probe::make(&evaluated, Options { image: Some(Rc::new(move |_| d2.get())), ..Default::default() });
    let _ = borrowed.footer(&node, &[]);
    dimensions.set(Some((7, 5)));
    check(borrowed.footer(&node, &[]).value == "7 × 5 image", "borrowed image dimensions refresh without cooking in a probe");
    let w = workspace_of("(workspace w (graph g :context value (let* [tick (state [n 0] (+ n 1))] tick)))");
    let evaluated = eval_recorded(&w);
    let state = flow_eval::create_state();
    let mut preview = probe::make(&evaluated, Options { state: Some(state.clone()), live: Some(input::at_time(0.)), ..Default::default() });
    check(preview.records(&path(&["g", "tick"])).iter().any(|(_, v)| *v == Summary::Value(Value::Int(1))), "the probe reads a fold snapshot");
    let result = flow_eval::run(&w, false, None, Some(&state), Some(&input::Frame { frame: 1, ..input::at_time(1.) }), 1.).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    check(result.results.iter().find(|(n, _)| n == "g").map(|(_, v)| v) == Some(&Value::Int(1)), "a probe never advances the environment fold");
}

/// `Test_support.Graph_pane.scope_view` over the editor's catalog.
fn scope_view(opts: ScopeOpts, w: &Workspace, graph: &str) -> (pxui_graph::scope_state::Pane, Rc<p::Scope>) {
    let sc = Rc::new(scope_of(w, graph));
    let view = { let mut r8_view = scope::create(Some(1000), Some(700)); scope::with_scope(opts, graph, sc.clone(), &mut r8_view); r8_view };
    ({ let mut r8_view = view; scope::with_records(pg::recorded(w), &mut r8_view); r8_view }, sc)
}
