//! The shipped Lisp is the implementation under test, including its camera and picking.
use editor_document::{kinds, workspace_doc};
use flow_sop::lower;
use flow_syntax::value::Value;

fn ok<T>(r: Result<T, rays_diagnostic::Diagnostic>) -> T { r.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d))) }

pub(crate) fn fixture(zoom: f64) -> (workspace_doc::T, lower::T) {
    let source = include_str!("../../../sketches/source_atlas.rays").replace("../_out/source_atlas/atlas.tsv", concat!(env!("CARGO_MANIFEST_DIR"), "/../../fixtures/source_atlas/atlas.tsv")).replace("(initial_zoom : float 1.0)", &format!("(initial_zoom : float {zoom})")).replace("(center : vec3 [0.5 0.5 0.0])", if zoom > 1.0 { "(center : vec3 [0.625 0.625 0.0])" } else { "(center : vec3 [0.5 0.5 0.0])" });
    let catalog = ok(kinds::shared(None));
    let doc = workspace_doc::of_text(&workspace_doc::TextOptions::default(), &catalog, &source).unwrap_or_else(|ds| panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")));
    let lowered = ok(lower::of_checked(None, lower::Options { inputs: Some((*doc.inputs).clone()), ..Default::default() }, &doc.checked));
    (doc, lowered)
}

pub(crate) fn result(lowered: &lower::T, name: &str) -> Value { lowered.evaluated.results.iter().find(|(n, _)| n == name).unwrap().1.clone() }

#[test]
fn source_atlas_layout_picking_camera_and_native_only_table() {
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
    let Value::Floats(4, cells) = result(&lowered, "cells") else { panic!("cells") };
    let Value::IntArray(parents) = data.get_str("parent").unwrap() else { panic!("parents") };
    assert_eq!(cells.len(), 264 * 4);
    let cell = |i: usize| &cells[i * 4..i * 4 + 4];
    for i in 0..parents.len() {
        let c = cell(i);
        assert!(c.iter().all(|v| v.is_finite()) && c[2] > 0. && c[0] >= 0. && c[1] >= 0. && c[0] + c[2] <= 1.0000000001 && c[1] + c[2] <= 1.0000000001, "cell {i}: {c:?}");
        if parents[i] >= 0 {
            let p = cell(parents[i] as usize);
            assert!(c[0] > p[0] && c[1] > p[1] && c[0] + c[2] < p[0] + p[2] && c[1] + c[2] < p[1] + p[2], "child {i} escapes its parent");
        }
        for j in 0..i {
            if parents[j] == parents[i] {
                let b = cell(j);
                assert!(c[0] + c[2] <= b[0] + 1e-12 || b[0] + b[2] <= c[0] + 1e-12 || c[1] + c[2] <= b[1] + 1e-12 || b[1] + b[2] <= c[1] + 1e-12, "siblings {i}, {j} overlap");
            }
        }
    }
    let program = ok(I::compile(&result(&lowered, "view"), &Default::default()));
    let state = e::create_state();
    let mut tick = 0;
    let mut frame = input::Frame { size: (1280, 900), dt: 1. / 60., ..input::at_time(0.) };
    let mut force = |frame: &mut input::Frame| {
        tick += 1; frame.tick = tick; frame.frame = tick;
        let value = ok(I::force(&program, &I::ForceOptions { state: Some(state.clone()), ..Default::default() }, frame));
        let Value::Record(record) = value else { panic!("view") }; record
    };
    // Each row is selectable in its margin, including scopes whose children cover their centres.
    for i in 0..parents.len() {
        let c = cell(i);
        frame.pointer = (288. + (c[0] + c[2] * 0.01) * 704., 56. + (c[1] + c[2] * 0.01) * 704.);
        let view = force(&mut frame);
        assert_eq!(view.get_str("hover"), Some(&Value::Int(i as i64)), "pick row {i}");
    }
    let c = cell(0);
    frame.pointer = (288. + (c[0] + c[2] * 0.01) * 704., 56. + (c[1] + c[2] * 0.01) * 704.);
    let before = force(&mut frame);
    frame.events = vec![input::Event::Scrolled((0., 2.))];
    let zoomed = force(&mut frame);
    let n = |record: &value::Record, name| ok(value::num(record.get_str(name).unwrap()));
    let old_scale = 704. * n(&before, "zoom");
    let new_scale = 704. * n(&zoomed, "zoom");
    assert!(new_scale > old_scale);
    for (axis, pointer, origin) in [("x", frame.pointer.0, 288.), ("y", frame.pointer.1, 56.)] {
        assert!(((pointer - origin - n(&before, axis)) / old_scale - (pointer - origin - n(&zoomed, axis)) / new_scale).abs() < 1e-12, "wheel anchor {axis}");
    }
    frame.events.clear();
    frame.keys = vec![input::Key::KeyChar('r')];
    let reset = force(&mut frame);
    assert_eq!(n(&reset, "zoom"), 1.);
    assert_eq!(n(&reset, "x"), 0.);
    frame.keys.clear();
    force(&mut frame);
    frame.buttons = vec![input::MouseButton::LeftButton];
    force(&mut frame);
    frame.buttons.clear();
    let dive = force(&mut frame);
    assert!(n(&dive, "zoom") > 1., "click dives");
    frame.events = vec![input::Event::PointerPressed(input::MouseButton::RightButton, frame.pointer)];
    let back = force(&mut frame);
    assert!((n(&back, "zoom") - n(&dive, "zoom") * 0.5).abs() < 1e-10);
}
