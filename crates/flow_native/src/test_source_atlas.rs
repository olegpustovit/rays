//! The shipped Lisp is the implementation under test, including its camera and picking.
use editor_document::{kinds, workspace_doc};
use flow_sop::lower;
use flow_syntax::value::Value;

fn ok<T>(r: Result<T, rays_diagnostic::Diagnostic>) -> T { r.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d))) }

pub(crate) fn fixture(zoom: f64) -> (workspace_doc::T, lower::T) {
    let source = include_str!("../../../sketches/source_atlas.rays").replace("../_out/source_atlas/", concat!(env!("CARGO_MANIFEST_DIR"), "/../../fixtures/source_atlas/")).replace("(initial_zoom : float 1.0)", &format!("(initial_zoom : float {zoom})")).replace("(center : vec3 [0.5 0.5 0.0])", if zoom > 1.0 { "(center : vec3 [0.625 0.625 0.0])" } else { "(center : vec3 [0.5 0.5 0.0])" });
    let catalog = ok(kinds::shared(None));
    let doc = workspace_doc::of_text(&workspace_doc::TextOptions::default(), &catalog, &source).unwrap_or_else(|ds| panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")));
    let lowered = ok(lower::of_checked(None, lower::Options { inputs: Some((*doc.inputs).clone()), ..Default::default() }, &doc.checked));
    (doc, lowered)
}

pub(crate) fn result(lowered: &lower::T, name: &str) -> Value { lowered.evaluated.results.iter().find(|(n, _)| n == name).unwrap().1.clone() }

#[test]
fn source_atlas_picking_eased_camera_and_native_only_table() {
    use flow_eval as e;
    use flow_ir::flow_ir::executor as I;
    use flow_syntax::value;
    let (doc, lowered) = fixture(1.0);
    let catalog = ok(kinds::shared(None));
    let scope = flow_graph::projection::of_graph(&catalog, &doc.checked, "data").unwrap();
    let node = scope.nodes.iter().find(|n| n.head == "table/load").expect("table node projects into the graph");
    assert!(node.rows.iter().any(|row| row.ty == Some(flow_syntax::ty::Ty::Text)));
    let alternate = concat!(env!("CARGO_MANIFEST_DIR"), "/../../fixtures/source_atlas/./atlas.tsv");
    let edited = ok(workspace_doc::edit(&catalog, &doc, &flow_graph::flow_edit::Op::SetArg {
        node: node.path.clone(), key: flow_graph::flow_edit::ArgKey::Pos(0), sub: vec![],
        value: flow_syntax::syntax::make(flow_syntax::syntax::Draft::Str(alternate.as_bytes().to_vec())),
    }));
    assert!(workspace_doc::to_text(&edited).unwrap().contains("/./atlas.tsv"), "graph path gesture edits the authored resource");
    assert_eq!(web_export::web_export::lower(&doc).err().unwrap().code, "E_WEB_TABLE");

    let Value::Record(data) = result(&lowered, "data") else { panic!("table") };
    let ints = |name: &str| { let Value::IntArray(a) = data.get_str(name).unwrap() else { panic!("{name}") }; a.clone() };
    let floats = |name: &str| { let Value::Floats(1, a) = data.get_str(name).unwrap() else { panic!("{name}") }; a.clone() };
    let (kind, x, y, size) = (ints("kind"), floats("x"), floats("y"), floats("size"));
    assert_eq!(kind.len(), 302);

    // The sheet at 1440 by 1000: an art window from 56 to 1384 and from 78 down to 896, a 778-point world.
    let (side, ox, oy) = (778., 720., 487.);
    let program = ok(I::compile(&result(&lowered, "view"), &Default::default()));
    let state = e::create_state();
    let mut tick = 0;
    let mut frame = input::Frame { size: (1440, 1000), dt: 1. / 60., ..input::at_time(0.) };
    let mut force = |frame: &mut input::Frame| {
        tick += 1; frame.tick = tick; frame.frame = tick;
        let value = ok(I::force(&program, &I::ForceOptions { state: Some(state.clone()), ..Default::default() }, frame));
        let Value::Record(record) = value else { panic!("view") }; record
    };
    let n = |record: &value::Record, name: &str| ok(value::num(record.get_str(name).unwrap()));
    // Every file and symbol is picked exactly under the pointer: a leaf at its centre, a file or a
    // scope in the margin its children leave. Packing nodes are never the answer.
    for i in 0..kind.len() {
        let inset = match kind[i] { 1 => 0.01, 4 => 0.04, 2 | 5 => 0.5, _ => continue };
        frame.pointer = (ox + (x[i] + size[i] * inset - 0.5) * side, oy + (y[i] + size[i] * inset - 0.5) * side);
        assert_eq!(force(&mut frame).get_str("hover"), Some(&Value::Int(i as i64)), "pick node {i} of kind {}", kind[i]);
    }
    frame.pointer = (10., 10.);
    assert_eq!(force(&mut frame).get_str("hover"), Some(&Value::Int(-1)), "outside the art nothing is inspected");

    // The wheel sets a target; the camera eases to it and keeps the world point under the pointer.
    frame.pointer = (400., 300.);
    let before = force(&mut frame);
    frame.events = vec![input::Event::Scrolled((0., 4.))];
    let first = force(&mut frame);
    frame.events.clear();
    let target = n(&first, "tz");
    assert!((target - (0.17f64 * 4.).exp()).abs() < 1e-12);
    assert!(n(&first, "zoom") > n(&before, "zoom") && n(&first, "zoom") < target, "zoom is animated, not a jump");
    let world = |view: &value::Record, axis: &str, pointer: f64, origin: f64| n(view, axis) + (pointer - origin) / (side * n(view, "zoom"));
    let anchored = |view: &value::Record, when: &str| {
        assert!((world(&before, "cx", 400., ox) - world(view, "cx", 400., ox)).abs() < 1e-9, "wheel anchor x, {when}");
        assert!((world(&before, "cy", 300., oy) - world(view, "cy", 300., oy)).abs() < 1e-9, "wheel anchor y, {when}");
    };
    // the point under the pointer stays there on every frame, and when the wheel turns again mid-way
    anchored(&first, "first frame");
    let mut settled = force(&mut frame);
    anchored(&settled, "second frame");
    frame.events = vec![input::Event::Scrolled((0., -1.))];
    anchored(&force(&mut frame), "wheel turned back while easing");
    frame.events.clear();
    let target = (0.17f64 * 3.).exp();
    for _ in 0..240 { settled = force(&mut frame); }
    assert!((n(&settled, "zoom") - target).abs() < 1e-12, "easing snaps to its target");
    anchored(&settled, "settled");

    // A click dives to the file-tree cell under the pointer; a right click steps back; R resets.
    let tile = (0..kind.len()).find(|&i| kind[i] == 1).unwrap();
    frame.keys = vec![input::Key::KeyChar('r')];
    force(&mut frame);
    frame.keys.clear();
    for _ in 0..240 { settled = force(&mut frame); }
    assert_eq!((n(&settled, "zoom"), n(&settled, "cx"), n(&settled, "cy")), (1., 0.5, 0.5));
    // in a file's margin the file itself is the named cell under the pointer; over a symbol it is the symbol
    frame.pointer = (ox + (x[tile] + size[tile] * 0.01 - 0.5) * side, oy + (y[tile] + size[tile] * 0.01 - 0.5) * side);
    force(&mut frame);
    frame.buttons = vec![input::MouseButton::LeftButton];
    frame.events = vec![input::Event::PointerPressed(input::MouseButton::LeftButton, frame.pointer)];
    force(&mut frame);
    frame.buttons.clear();
    frame.events = vec![input::Event::PointerReleased(input::MouseButton::LeftButton, frame.pointer)];
    let dive = force(&mut frame);
    frame.events.clear();
    assert_eq!(n(&dive, "tz"), (0.7 / size[tile]).min(3.2).clamp(0.65, 180.), "click dives");
    assert!((n(&dive, "tx") - (x[tile] + size[tile] * 0.5)).abs() < 1e-12 && (n(&dive, "ty") - (y[tile] + size[tile] * 0.5)).abs() < 1e-12);
    for _ in 0..240 { settled = force(&mut frame); }
    frame.events = vec![input::Event::PointerPressed(input::MouseButton::RightButton, frame.pointer)];
    let back = force(&mut frame);
    frame.events.clear();
    assert_eq!(n(&back, "tz"), (n(&settled, "zoom") / 2.8).max(1.));

    // A drag pans by the pointer's travel and is not a click.
    for _ in 0..240 { settled = force(&mut frame); }
    frame.buttons = vec![input::MouseButton::LeftButton];
    frame.events = vec![input::Event::PointerPressed(input::MouseButton::LeftButton, frame.pointer)];
    force(&mut frame);
    frame.events.clear();
    frame.pointer.0 += 30.;
    let dragged = force(&mut frame);
    assert!((n(&dragged, "tx") - (n(&settled, "tx") - 30. / (side * n(&settled, "zoom")))).abs() < 1e-12);
    frame.buttons.clear();
    frame.events = vec![input::Event::PointerReleased(input::MouseButton::LeftButton, frame.pointer)];
    let released = force(&mut frame);
    frame.events.clear();
    assert_eq!(n(&released, "tz"), n(&dragged, "tz"), "a drag does not dive");

    // T and H toggle on the key's press, once.
    assert_eq!((released.get_str("cells"), released.get_str("ui")), (Some(&Value::Bool(false)), Some(&Value::Bool(true))));
    frame.keys = vec![input::Key::KeyChar('t'), input::Key::KeyChar('h')];
    force(&mut frame);
    let held = force(&mut frame);
    assert_eq!((held.get_str("cells"), held.get_str("ui")), (Some(&Value::Bool(true)), Some(&Value::Bool(false))));
}
