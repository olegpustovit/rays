//! Single-program design: a Lisp node call evaluates its
//! body in the calling program, so chained nodes are one program for the region passes.
use super::support::*;
use crate::{lisp_sop, lower, value_lane};
use sop::edit_graph as edit;

type Outcome = Result<Vec<u8>, (String, String, String)>;

fn run_chain(body: &str) -> (Vec<u8>, usize, Vec<u8>) {
    let profiles = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
    let sink = profiles.clone();
    let options = lower::Options { on_profile: Some(std::sync::Arc::new(move |p| sink.lock().unwrap().push(p.clone()))), ..lower::Options::default() };
    let text = format!("(workspace w (graph g :context sop {body}))");
    let l = lower::workspace(None, Vec::new(), options, &parse(&text)).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let graph = l.graphs[0].clone();
    let cook_node = |n: &sop::node::T| geometry_bytes(&cook_once(1, n));
    let root = edit::compile_node(&graph.network.geometry, graph.root.unwrap()).unwrap();
    let bytes = cook_node(&root);
    let mut runs = flow_ir::flow_ir::profile::executions(&l.profile).len();
    for p in profiles.lock().unwrap().iter() {
        runs += flow_ir::flow_ir::profile::executions(p).len();
    }
    let factory = lisp_sop::root(&l, &graph).unwrap();
    (bytes, runs, cook_node(&factory))
}

#[test]

fn chained_bodies_are_one_program_and_equal_the_factory_node() {
    for (name, body) in [
        ("set_orient > color_by_height", "(sop/color_by_height (sop/set_orient (sop/grid :counts \"Point counts\" :connectivity \"Points\" :columns 1025 :rows 2) :x 0.5))"),
        ("color_by_height > color_by_height", "(sop/color_by_height (sop/color_by_height (sop/grid :counts \"Point counts\" :columns 65 :rows 33)) :low_red 7)"),
    ] {
        let (bytes, _runs, factory) = run_chain(body);
        assert!(bytes == factory, "{name}: bytes differ from the factory node's");
    }
}

/// Lisp cards lower inline unless they keep a node of their own (a structure that follows the
/// frame): no plan node is a Lisp kind.
fn inlined(l: &lower::T) -> bool {
    !l.plan.nodes.iter().any(|n| l.factories.iter().any(|f| format!("sop/{}", edit::factory_key(f)) == n.kind))
}

/// Step B: an inlined card keeps the node's boundaries.
fn outcome(seed: i64, inline: &mut bool, text: &str) -> Outcome {
    match lower::workspace(None, Vec::new(), lower::Options::default(), &parse(text)) {
        Err(d) => Err(("lower".into(), d.code, d.message)),
        Ok(l) => {
            *inline = inlined(&l);
            let graph = l.graphs[0].clone();
            let live = input::at_time(0.125);
            let lane = value_lane::create(None);
            let resolved = value_lane::resolve(None, Some(&live), &lane, 0.125, &graph.network).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
            let compiled = edit::compile_node(&resolved.geometry, graph.root.unwrap()).unwrap();
            let mut session = new_session(8, 1 << 24);
            let context = sop::context::create(sop::context::Options { seed: Some(seed), frame: Some(3), time: Some(0.125), input: Some(live), domains: Some(1), ..Default::default() }).unwrap();
            let out = try_cook(&mut session, &context, &compiled);
            sop::session::close(&mut session);
            match out {
                Ok(g) => Ok(geometry_bytes(&g)),
                Err(d) => Err(("cook".into(), d.code, d.message)),
            }
        }
    }
}

fn out(text: &str) -> Outcome {
    outcome(0, &mut true, text)
}

#[test]
fn step_b_body_errors_cook_refusals_lower() {
    let normalized = r#"(defnode sop/b_norm [(geo : geometry) (rules : text "point" :kind encoded :normalize (fn [(raw : text)] (case (text/lower (text/trim raw)) "points" "point" "point" "point" :else (error "E_RULES" (str "Unknown owner: " raw)))))] (sop/with_attr geo :tag (array/text (sop/count geo :points) rules) :type text))"#;
    let rest = r#"(defnode sop/b_rest :validate (> n 0) :code "E_N" :message "n must be positive" [(geos : geometry :rest true :required false) (n : int 1)] (if (> (count geos) 0) (last geos) (sop/line :kind "Points" :points n)))"#;
    let valid = r#"(defnode sop/b_valid :validate (> count 0) :code "E_POSITIVE" :message "count must be positive" [(count : int 1 :min -10 :max 10)] (sop/line :kind "Points" :points count))"#;
    let range = r#"(defnode sop/b_range [(count : int 2 :min 1 :max 50 :hard-min 1 :hard-max 5)] (sop/line :kind "Points" :points count))"#;
    let fail = r#"(defnode sop/b_fail [(geo : geometry)] (sop/with_attr geo :w (error "E_POINTS" "selected points" :cause "point cause")))"#;
    let norm_ok = r#"(sop/b_norm (sop/line :points 3) :rules (str " POINTS" ""))"#;
    let norm_bad = r#"(sop/b_norm (sop/line :points 3) :rules (str "edges" ""))"#;
    for (expected, defnode, call) in [
        ("cook", fail, "(sop/b_fail (sop/line :points 3))"),
        ("lower", valid, "(sop/b_valid :count (- 0 3))"),
        ("ok", valid, "(sop/b_valid :count (+ 1 3))"),
        ("ok", range, "(sop/b_range :count (* 4 4))"),
        ("ok", normalized, norm_ok),
        ("lower", normalized, norm_bad),
        ("ok", rest, "(sop/b_rest)"),
        ("ok", rest, "(sop/b_rest (sop/line :points 2) (sop/line :points 3))"),
        ("ok", rest, "(sop/b_rest :n (+ 2 2))"),
        ("lower", rest, "(sop/b_rest :n (- 2 2))"),
    ] {
        let text = format!("(workspace b {defnode} (graph g :context sop {call}))");
        eprintln!("CALL {call}");
        let got = match out(&text) {
            Ok(_) => "ok".to_owned(),
            Err((stage, _, _)) => stage,
        };
        assert_eq!(got, expected, "step B outcome changed: {call}");
    }
}

/// Step C: a body that reaches its cook context lowers as its own node, specialized per context;
/// the same card with the context branch untaken inlines.
#[test]

fn step_c_context_reaching_bodies_and_live_arguments_inline() {
    for (call, inlines) in [
        ("(sop/noise_displace (sop/line :points 9) :context_seed true :amplitude 0.5)", true),
        ("(sop/noise_displace (sop/line :points 9) :seed 4 :amplitude 0.5)", true),
        ("(sop/mountain (sop/line :points 9) :seed_mode \"Auto\")", true),
        ("(sop/mountain (sop/line :points 9))", true),
        ("(sop/attribute_fade (sop/line :points 9))", true),
        ("(sop/merge (sop/line :points 2) (sop/line :points 3))", true),
        ("(sop/noise_displace (sop/line :points 9) :amplitude (* t 4.0))", true),
        ("(sop/attribute_fade (sop/line :points 9) :frame_offset (* t 8.0))", false),
        ("(sop/b_live (sop/line :points 9) :on (> t 0.1))", false),
    ] {
        let text = format!(
            "(workspace c (defnode sop/b_live [(geo : geometry) (on : bool false)] (if on (sop/with_attr geo :P (map (fn [p] (+ p [1 0 0])) (sop/attr geo :P))) geo)) (graph g :context sop {call}))"
        );
        let mut inline = true;
        assert!(outcome(2026, &mut inline, &text).is_ok(), "step C: {call} did not cook");
        assert_eq!(inline, inlines, "step C: {call} inlined wrongly");
    }
}

/// Frame-driven arguments inline: a live vec3 group is its components, and a check of a live value
/// is made as the card cooks, with the diagnostic the written-out call refuses or fails with.
#[test]

fn frame_driven_arguments_inline_and_fail_as_the_call() {
    let text = |call: &str| format!("(workspace f (graph g :context sop (sop/attribute_noise (sop/grid :rows 5 :columns 7) {call})))");
    for (live, written, inlines) in [
        (":offset [(* t 2.0) 0.5 (- 0.0 t)]", ":offset [0.25 0.5 -0.125]", true),
        (":frequency [1.0 (+ 1.0 t) 2.0] :name \"n\" :kind \"Vector\"", ":frequency [1.0 1.125 2.0] :name \"n\" :kind \"Vector\"", true),
        (":blend (* t 4.0)", ":blend 0.5", true),
        (":blend (* t 16.0)", ":blend 2.0", true),
        (":max [(+ t 1.0) 1.0 1.0] :range \"Minimum / maximum\"", ":max [1.125 1.0 1.0] :range \"Minimum / maximum\"", true),
        (":lacunarity (+ t 1.0) :roughness (+ t 0.25) :octaves 3", ":lacunarity 1.125 :roughness 0.375 :octaves 3", true),
        (":octaves (int (+ 2.0 (* t 8.0)))", ":octaves 3", true),
        (":octaves (int (- 0.0 t))", ":octaves 1", true),
    ] {
        let mut inline = false;
        let live_outcome = outcome(0, &mut inline, &text(live));
        assert_eq!(inline, inlines, "frame arguments: {live} inlined wrongly");
        let written_outcome = out(&text(written));
        match (&live_outcome, &written_outcome) {
            (Ok(a), Ok(b)) if a == b => {}
            (Err((_, _, a)), Err((_, _, b))) if a == b => {}
            _ => panic!("frame arguments: {live} differs from {written}: {:?} vs {:?}", live_outcome.as_ref().map(|_| ()), written_outcome.as_ref().map(|_| ())),
        }
    }
    match out(&text(":blend (* t 16.0)")) {
        Err((stage, code, message)) if stage == "cook" && code == "E_LOWER" && message == "sop/attribute_noise: blend must be in [0,1]" => {}
        other => panic!("frame arguments: a deferred validation does not fail the cook as refused: {:?}", other.err()),
    }
}

/// A driven transform inlines: its identity and singular tests are the write's data.
fn driven_transform_case(case: usize) {
    let text = |input: &str, call: &str| format!("(workspace x (graph g :context sop (sop/transform {input} {call})))");
    let grid = "(sop/grid :rows 4 :columns 4)".to_owned();
    let vertex = format!("(sop/normals {grid} :owner \"Vertex\")");
    // Four divisions give 25 points. Keep Group Range's 3..12 selection without its library closure.
    let range = format!("(sop/with_group {grid} (array/concat (array/int 3 0) (array/concat (array/int 10 1) (array/int 12 0))) :name \"range\")");
    let empty = format!("(sop/with_group {grid} (array/int 25 0) :name \"range\")");
    let (input, live, written, inlines) = [
        (&grid, ":shear_xy (* t 2.0) :translate [0.0 t 0.0]", ":shear_xy 0.25 :translate [0.0 0.125 0.0]", true),
        (&grid, ":translate [(- t 0.125) 0.0 0.0]", "", true),
        (&grid, ":scale [(- t 0.125) 1.0 1.0]", ":scale [0.0 1.0 1.0]", true),
        (&vertex, ":shear_xz (* t 2.0)", ":shear_xz 0.25", true),
        (&vertex, ":scale [(- t 0.125) 1.0 1.0]", ":scale [0.0 1.0 1.0]", true),
        (&range, ":group \"range\" :shear_yz (* t 2.0)", ":group \"range\" :shear_yz 0.25", false),
        (&range, ":group \"range\" :scale [1.0 (- t 0.125) 1.0]", ":group \"range\" :scale [1.0 0.0 1.0]", false),
        (&empty, ":group \"range\" :scale [1.0 (- t 0.125) 1.0]", ":group \"range\" :scale [1.0 0.0 1.0]", false),
        (&grid, ":recompute_normals true :shear_xy (* t 2.0)", ":recompute_normals true :shear_xy 0.25", true),
    ][case];
    let mut inline = false;
    let a = outcome(0, &mut inline, &text(input, live));
    assert_eq!(inline, inlines, "driven transform: {live} inlined wrongly");
    let b = out(&text(input, written));
    assert!(a.is_ok() && a == b, "driven transform: {input} {live} differs from {written}");
}

#[test] fn driven_transform_shear_translate() { driven_transform_case(0); }
#[test] fn driven_transform_translate_zero() { driven_transform_case(1); }
#[test] fn driven_transform_scale_zero() { driven_transform_case(2); }
#[test] fn driven_transform_vertex_shear() { driven_transform_case(3); }
#[test] fn driven_transform_vertex_scale_zero() { driven_transform_case(4); }
#[test] fn driven_transform_group_shear() { driven_transform_case(5); }
#[test] fn driven_transform_group_scale_zero() { driven_transform_case(6); }
#[test] fn driven_transform_empty_group() { driven_transform_case(7); }
#[test] fn driven_transform_recompute_normals() { driven_transform_case(8); }

/// Recomputed normals are sop/normals' face-area bytes, gathered per point (or vertex).
fn recomputed_normals_case(case: usize) {
    let (input, owner) = [
        ("(sop/grid :rows 4 :columns 4)", "point"),
        ("(sop/grid :rows 4 :columns 4 :connectivity \"Triangles\")", "point"),
        ("(sop/normals (sop/uv_sphere :segments 8 :rings 4) :owner \"Point\")", "point"),
        ("(sop/normals (sop/grid :rows 4 :columns 4) :owner \"Vertex\")", "vertex"),
    ][case];
    let moved = format!("(sop/transform {input} :shear_xy 0.25 :rotate [0.3 0.0 0.0])");
    let recomputed = format!("(sop/transform {input} :shear_xy 0.25 :rotate [0.3 0.0 0.0] :recompute_normals true)");
    let cap = format!("{}{}", owner[..1].to_uppercase(), &owner[1..]);
    let scattered = format!(
        "(sop/with_attr {moved} \"N\" (sop/attr (sop/normals {moved} :owner {cap:?} :weighting \"Face area\") \"N\" :owner {owner:?} :type vec3) :owner {owner:?} :type vec3)"
    );
    let text = |body: &str| format!("(workspace x (graph g :context sop {body}))");
    let (a, b) = (out(&text(&recomputed)), out(&text(&scattered)));
    assert!(a.is_ok() && a == b, "recomputed normals differ from sop/normals: {input}");
}

#[test] fn recomputed_normals_grid_quads() { recomputed_normals_case(0); }
#[test] fn recomputed_normals_grid_triangles() { recomputed_normals_case(1); }
#[test] fn recomputed_normals_sphere() { recomputed_normals_case(2); }
#[test] fn recomputed_normals_vertex() { recomputed_normals_case(3); }

/// Step E2: a cooked-data argument (a count of the cooked input) inlines; the body reads it as a
/// value resolved once per cook, in time linear in the points.
#[test]

fn cooked_data_arguments_resolve_once_per_cook() {
    let text = |amplitude: &str, points: i64| format!("(workspace e (graph g :context sop (let* [g (sop/line :points {points})] (sop/noise_displace g :amplitude {amplitude}))))");
    for points in [9i64, 4096] {
        let inline = out(&text("(/ (float (sop/count g :points)) 4096.0)", points));
        let written = out(&text(&format!("{:.17e}", points as f64 / 4096.0), points));
        assert!(inline.is_ok() && inline == written, "step E2: cooked argument differs at {points} points");
    }
    let timed = |points: i64| {
        let start = std::time::Instant::now();
        let r = out(&text("(/ (float (sop/count g :points)) 4096.0)", points));
        assert!(r.is_ok());
        start.elapsed().as_secs_f64()
    };
    let (small, large) = (timed(20_000), timed(80_000));
    assert!(large <= 12.0 * small + 0.05, "step E2: 4x points took {large:.3}s vs {small:.3}s");
}

/// Live validation/if expressions remain inline guards of the same card rather than creating new per-frame device sites and slots.
#[test]
fn a_live_validation_inlines_the_card_as_a_guard() {
    let l = lower::workspace(None, Vec::new(), lower::Options::default(), &parse("(workspace x (graph g :context sop (sop/transform (sop/grid :rows 5 :columns 7) :recompute_normals true :shear_xy (* t 2.0))))"))
        .unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let kinds: Vec<&str> = l.plan.nodes.iter().map(|n| n.kind.as_str()).collect();
    assert!(!kinds.contains(&"sop/transform"), "the driven transform stayed a catalog node: {kinds:?}");
}

/// The host bulk callback resolves a deferred grid validation mask, preserving its card and bindable cooked sources instead of relowering a catalog node every frame.
#[test]
fn a_deferred_grid_stays_a_card_and_the_driven_noise_inlines() {
    let text = "(workspace x (graph g :context sop (-> (sop/grid :counts \"Point counts\" :connectivity \"Triangles\" :columns 70 :rows 70) \
        (sop/attribute_noise :name \"wave\" :seed 7 :octaves 4 :range \"Zero centered\" :offset [(* t 0.3) 0.0 (* t 0.2)] \
        :lacunarity (+ 2.0 (* 0.3 (sin (* t 0.5))))))))";
    let l = lower::workspace(None, Vec::new(), lower::Options { defer: true, ..lower::Options::default() }, &parse(text))
        .unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let kinds: Vec<&str> = l.plan.nodes.iter().map(|n| n.kind.as_str()).collect();
    assert!(!kinds.contains(&"sop/attribute_noise"), "the driven noise stayed the catalog node: {kinds:?}");
    let grid_card = l.plan.nodes.iter().find(|n| n.kind == flow_eval::card_kind() && n.args.iter().any(|(k, v)| k == "kind" && *v == flow_eval::Value::Text(b"sop/grid".as_slice().into())));
    let value = grid_card.and_then(|n| n.args.iter().find(|(k, _)| k == "value")).map(|(_, v)| v.clone());
    assert!(matches!(value, Some(flow_eval::Value::Deferred(..))), "the grid's card is structural: {value:?}");
}
