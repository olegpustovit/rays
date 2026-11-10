//! Typed drawing tests cover errors, packed particles, editable folds and host inputs; native ownership/allocation paths have separate qualification cases.
use super::support::*;
use super::t3c::*;
use flow_eval::Value;
use flow_graph::flow_edit as fe;
use flow_native::drawing;
use rays_editor::editor;
use std::rc::Rc;

fn ok<T>(r: Result<T, rays_diagnostic::Diagnostic>) -> T {
    r.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

fn doc(text: &str) -> editor_document::workspace_doc::T {
    load(text)
}

fn render_once(plan: &flow_eval::Plan, value: &Value, live: &input::Frame, size: (i64, i64)) -> Result<Vec<rays::scene::node>, rays_diagnostic::Diagnostic> {
    let prepared = drawing::prepare(&drawing::PrepareOptions::default(), plan, value)?;
    drawing::render_prepared(&prepared, live, size, &drawing::RenderOptions::default())
}

pub fn all_draws_and_errors() {
    let all_draws = doc(
        r##"(workspace drawing (graph picture :context draw
    (draw/merge (draw/background "#080a10")
      (draw/point [1 2 0]) (draw/points (array/vec3 4 [2 3 0]))
      (draw/line [0 0 0] [10 10 0] :width 2)
      (draw/rect [5 5 0] [20 10 0] :fill "#ffffff")
      (draw/circle [15 15 0] 4 :stroke [0 1 1])
      (draw/text [30 10 0] "text" :size 16)
      (draw/circles (array/vec3 4 [20 30 0]) :radius 2.0)
      (draw/rects (array/vec3 4 [40 50 0]) [5 5 0])
      (draw/lines (array/vec3 4 [20 30 0]) (array/vec3 4 [40 50 0]))
      (draw/rotate 0.2 (draw/scale 2.0 (draw/rounded_rect [1 2 0] [20 20 0] :radius 4)))
      (draw/polygon (array/vec3 3 [1 2 0]))
      (draw/polyline (array/vec3 3 [1 2 0]))
      (draw/translate [10 20 0] (draw/point [1 2 0])))))"##,
    );
    let all = ok(flow_eval::static_eval(&all_draws.checked));
    let mut kinds: Vec<String> = all.plan.nodes.iter().map(|n| n.kind.clone()).collect();
    kinds.sort();
    kinds.dedup();
    let mut expected: Vec<String> = flow_check::op::all().iter().filter(|o| o.ctx == flow_syntax::context::draw() && o.name != "draw/image").map(|o| o.name.clone()).collect();
    expected.sort();
    assert!(kinds == expected, "draw kinds differ: {kinds:?} vs {expected:?}");
    let picture = all.results.iter().find(|(n, _)| n == "picture").unwrap().1.clone();
    assert!(!ok(render_once(&all.plan, &picture, &input::at_time(0.), (800, 600))).is_empty());
    // lengths
    for text in [
        "(draw/circles (array/vec3 3 [1 2 0]) :radius (array/float 2 1.0))",
        "(draw/rects (array/vec3 3 [1 2 0]) (array/vec3 2 [3 4 0]))",
        "(draw/lines (array/vec3 3 [1 2 0]) (array/vec3 2 [3 4 0]))",
    ] {
        let ws = doc(&format!("(workspace lengths (graph picture :context draw {text}))"));
        let evaluated = ok(flow_eval::static_eval(&ws.checked));
        let value = evaluated.results.iter().find(|(n, _)| n == "picture").unwrap().1.clone();
        match render_once(&evaluated.plan, &value, &input::at_time(0.), (800, 600)) {
            Err(d) => assert!(d.code == "E_DRAW_LENGTH", "{text}: {}", d.code),
            Ok(_) => panic!("{text} rendered"),
        }
    }
    let invalid = ok(flow_eval::static_eval(&doc(r#"(workspace invalid (graph g :context draw (draw/background "invalid")))"#).checked));
    let value = invalid.results.iter().find(|(n, _)| n == "g").unwrap().1.clone();
    match render_once(&invalid.plan, &value, &input::at_time(0.), (800, 600)) {
        Err(d) => assert!(d.code == "E_DRAW_COLOR"),
        Ok(_) => panic!("an invalid color rendered"),
    }
}

fn read_particles() -> String {
    std::fs::read_to_string(format!("{}/../../sketches/particles.rays", env!("CARGO_MANIFEST_DIR"))).expect("particles.rays")
}

#[derive(Clone)]
struct Particle {
    x: f64,
    y: f64,
    vx: f64,
    vy: f64,
}

fn initial() -> Vec<Particle> {
    (0..10000)
        .map(|i| {
            let angle = i as f64 * 0.1;
            let speed = 30. + (i % 70) as f64;
            Particle { x: 400., y: 300., vx: angle.cos() * speed, vy: angle.sin() * speed }
        })
        .collect()
}

fn step(dt: f64, ps: &[Particle]) -> Vec<Particle> {
    ps.iter()
        .map(|p| {
            let (x, y) = (p.x + p.vx * dt, p.y + p.vy * dt);
            Particle { x, y, vx: if x < 0. || x >= 800. { -p.vx } else { p.vx }, vy: if y < 0. || y >= 600. { -p.vy } else { p.vy } }
        })
        .collect()
}

/// The particles sketch: a typed plan of three drawing nodes, a state zone in the projection, the
/// packed positions against a native oracle, editable folds.
pub fn particles() {
    let workspace = doc(&read_particles());
    let evaluated = ok(flow_eval::r#static(&workspace.checked, false, Some(&workspace.inputs)));
    assert!(evaluated.plan.nodes.len() == 3);
    for n in &evaluated.plan.nodes {
        assert!(n.ty == flow_syntax::ty::drawing());
    }
    let drawing_value = evaluated.results.iter().find(|(n, _)| n == "picture").unwrap().1.clone();
    assert!(matches!(&drawing_value, Value::Deferred(flow_syntax::ty::Ty::Named(n), _) if n == "drawing"));
    let catalog = catalog();
    let scope = flow_graph::projection::of_graph(&catalog, &workspace.checked, "picture").unwrap_or_else(|e| panic!("{e}"));
    let state_card = flow_graph::projection::find(&scope, &["picture".into(), "particles".into()]).expect("particles card");
    let zone = state_card.zone.expect("a zone");
    assert!(zone.kind == flow_graph::projection::ZoneKind::State);
    assert!(zone.rail.iter().any(|r| r.role == flow_graph::projection::Role::Acc && r.name == "previous"));
    let state = flow_eval::create_state();
    let points = evaluated.plan.nodes.iter().find(|n| n.kind == "draw/points").expect("draw/points");
    let positions = points.args.iter().find(|(k, _)| k == "positions").expect("positions").1.clone();
    let mut oracle = initial();
    for (frame, dt) in [1. / 60., 1. / 60., 0.125].iter().enumerate() {
        oracle = step(*dt, &oracle);
        let live = input::Frame { frame: frame as i64, dt: *dt, size: (800, 600), ..input::at_time(frame as f64 * dt) };
        let xs = ok(flow_eval::force(&positions, &live, Some(&state)));
        assert!(flow_syntax::value::array_length(&xs).unwrap() == 10000);
        for (i, p) in oracle.iter().enumerate() {
            let (x, y, z) = flow_syntax::value::comps(&flow_syntax::value::array_get(&xs, i as i64).unwrap()).unwrap();
            // Native arithmetic can fuse multiply-add; the Lisp has separately rounded IEEE operations.
            assert!((x - p.x).abs() <= 1e-9 && (y - p.y).abs() <= 1e-9 && x as i64 == p.x as i64 && y as i64 == p.y as i64 && z.to_bits() == 0f64.to_bits(), "particle {i} frame {frame}: got ({x},{y},{z}), expected ({},{},0)", p.x, p.y);
        }
        let prepared = ok(drawing::prepare(&drawing::PrepareOptions { states: evaluated.states.clone(), ..Default::default() }, &evaluated.plan, &drawing_value));
        ok(drawing::render_prepared(&prepared, &live, live.size, &drawing::RenderOptions { state: Some(state.clone()), ..Default::default() }));
    }
}

/// The same source edits used by the pane add a fold and change its seed.
pub fn editable_folds() {
    let workspace = doc(&read_particles());
    let catalog = catalog();
    let edited = |source: &[flow_syntax::syntax::Syntax], op: &fe::Op| fe::apply_checked(&catalog, source, op).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d))).0;
    let expr = flow_syntax::syntax::parse("(state [seconds 0.0] (+ seconds (frame/dt)))").unwrap().remove(0);
    let source = edited(&workspace.source, &fe::Op::AddNode { scope: vec!["picture".into()], name: "elapsed".into(), expr });
    let source = edited(
        &source,
        &fe::Op::SetArg { node: vec!["picture".into(), "elapsed".into()], key: flow_graph::edit_tree::ArgKey::Bv(1, 1), sub: Vec::new(), value: flow_syntax::syntax::make(flow_syntax::syntax::Draft::Num("2.0".into())) },
    );
    let w = match flow_sop::catalog::check_workspace(&[], false, None, &catalog, &source) {
        (Some(w), _) => w,
        _ => panic!("edited source did not check"),
    };
    let n = flow_graph::projection::find(&flow_graph::projection::of_graph(&catalog, &w, "picture").unwrap(), &["picture".into(), "elapsed".into()]).expect("elapsed");
    assert!(n.zone.expect("zone").kind == flow_graph::projection::ZoneKind::State);
}

/// Host inputs reach the lowered plan and survive an edit; a mistyped input refuses.
pub fn host_inputs() {
    let input_text = r##"(workspace inputs (graph g :context draw [(radius : float 5)] (draw/circle [10 10 0] radius :fill "#00ffff")) (graph editor :context editor (ui/workspace (ui/canvas (ref g)))))"##;
    let mut workspace = doc(input_text);
    workspace.inputs = std::sync::Arc::new(vec![("g".to_string(), vec![("radius".into(), Value::Float(9.))])]);
    let options = editor_document::lowering::Options { factories: Some(sop::nodes::factories()), ..Default::default() };
    let document = ok(editor_document::lowering::of_workspace(&options, &workspace));
    let lower = &document.workspace.lowered;
    assert!(lower.plan.nodes[0].args.iter().find(|(k, _)| k == "radius").map(|(_, v)| v.clone()) == Some(Value::Float(9.)));
    let bad = {
        let mut b = workspace.clone();
        b.inputs = std::sync::Arc::new(vec![("g".to_string(), vec![("radius".into(), Value::Text(b"wrong".as_slice().into()))])]);
        b
    };
    assert!(editor_document::lowering::of_workspace(&options, &bad).is_err());
    let _ = (Rc::new(()), editor::private::menu_forms);
}
