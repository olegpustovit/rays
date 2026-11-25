//! What a graph-pane gesture costs (UI fleet L4): allocations on the frame of each interaction and the
//! settled frame after it, on isogrid. Ceilings are exact counts at measured + 5%, never raised.
use super::click_budget::{session, sketch, P};
use input::{Event, Key, MouseButton};

/// (action, ceiling on the key frame, ceiling on the third frame after it)
/// Measured in a fresh process plus 5%, rounded up and capped at the previous ceiling.
const CEILINGS: &[(&str, u64, u64)] = &[
    ("zoom rest", 462, 457),
    ("pan across overscan", 472, 977),
    ("node select", 467, 458),
    ("box select", 469, 677),
    ("after box select", 527, 527),
    ("escape", 612, 527),
    ("hover moves", 531, 531),
    ("enter", 1855, 458),
    ("up", 593, 458),
];

fn rest(p: &mut P, name: &str, first: (f64, u64)) {
    let mut frames = vec![first];
    for _ in 0..3 {
        frames.push(p.step(&[], vec![], (1080., 200.)));
    }
    p.record(name, frames);
}

fn tour(p: &mut P) {
    let at = (1080., 200.);
    let ch = Key::KeyChar;
    // the graph pane is the one the tour's keys address: bring it up first
    p.step(&[], vec![Event::PointerMoved(at)], at);
    let f = p.step(&[], vec![Event::Scrolled((0., 1.))], at);
    rest(p, "zoom rest", f);
    let pan = |p: &P| {
        let model = (&*p.e.as_ref().expect("editor").core).into();
        let view = rays_editor::editor_model::pane_view(&model);
        (view.pan_x, view.pan_y)
    };
    let before_pan = pan(p);
    let mut frames = Vec::new();
    frames.push(p.step(&[], vec![Event::PointerPressed(MouseButton::RightButton, at)], at));
    for i in 1..4 {
        let q = (at.0 + 100. * i as f64 - 150., at.1 + 20. * i as f64);
        frames.push(p.step(&[], vec![Event::PointerMoved(q)], q));
    }
    assert_ne!(pan(p), before_pan, "the held right button must pan the graph");
    assert!(frames[1..3].iter().all(|&(_, allocs)| allocs <= 471), "pan inside overscan rebuilt the native submission: {frames:?}");
    p.record("pan across overscan", frames);
    p.step(&[], vec![Event::PointerReleased(MouseButton::RightButton, (at.0 + 150., at.1 + 60.))], at);
    for _ in 0..3 {
        p.step(&[], vec![], at);
    }
    let f = p.step(&[], vec![Event::PointerPressed(MouseButton::LeftButton, at), Event::PointerReleased(MouseButton::LeftButton, at)], at);
    rest(p, "node select", f);
    let f = p.step(&[], vec![Event::PointerPressed(MouseButton::LeftButton, (900., 60.))], (900., 60.));
    let mut frames = vec![f];
    for i in 1..4 {
        let q = (900. + 100. * i as f64, 60. + 80. * i as f64);
        frames.push(p.step(&[], vec![Event::PointerMoved(q)], q));
    }
    assert!(frames[1..3].iter().all(|&(_, allocs)| allocs <= 471), "rubber-band drag rebuilt the native submission: {frames:?}");
    p.record("box select", frames);
    p.step(&[], vec![Event::PointerReleased(MouseButton::LeftButton, (1200., 300.))], (1200., 300.));
    let f = p.step(&[], vec![], (1200., 300.));
    rest(p, "after box select", f);
    let f = p.step(&[], vec![Event::KeyPressed(Key::Escape)], (1200., 300.));
    rest(p, "escape", f);
    let frames = (0..4).map(|i| { let q = (1000. + 35. * i as f64, 150. + 30. * i as f64); p.step(&[], vec![Event::PointerMoved(q)], q) }).collect();
    p.record("hover moves", frames);
    // into the defn `voxels` calls and up again (the pane's graph changes both times)
    let graph = |p: &P| -> Option<String> {
        let model = (&*p.e.as_ref().expect("editor").core).into();
        rays_editor::editor_panes::graph_name(&model).map(str::to_owned)
    };
    let shown = graph(p);
    {
        let core = std::rc::Rc::make_mut(&mut p.e.as_mut().expect("editor").core);
        rays_graph::scope_pane::select(vec![vec!["volume".into(), "voxels".into()]], &mut rays_editor::editor_model::pane_mut(core).view);
    }
    for _ in 0..3 {
        p.step(&[], vec![], at);
    }
    let f = p.step(&[], vec![Event::KeyPressed(ch('i'))], at);
    rest(p, "enter", f);
    assert!(graph(p).as_deref() == Some("def:build-volume"), "i did not enter the defn voxels calls ({:?})", graph(p));
    let f = p.step(&[], vec![Event::KeyPressed(ch('u'))], at);
    rest(p, "up", f);
    assert!(graph(p) == shown, "u did not come back to {shown:?} ({:?})", graph(p));
}

fn isogrid() {
    let rows = session("isogrid graph", &sketch("isogrid.rays"), &tour);
    let mut over = Vec::new();
    let mut table = String::new();
    for (name, _, key, settled) in &rows {
        table.push_str(&format!("    (\"{name}\", {key}, {settled}),\n"));
        if let Some((_, k, s)) = CEILINGS.iter().find(|c| c.0 == name) {
            if key > k || settled > s {
                over.push(format!("{name}: {key}/{settled} over {k}/{s}"));
            }
        } else if !CEILINGS.is_empty() {
            over.push(format!("{name}: no ceiling"));
        }
    }
    eprintln!("CLICK graph ceilings now:\n{table}");
    assert!(over.is_empty(), "{}", over.join("; "));
}

fn multi_graph() {
    use rays_editor::{editor_shell::graph_leaves, panel_map};
    let source = r#"(workspace two-graphs
      (graph a :context sop (let* [box (sop/box)] box))
      (graph b :context sop (let* [ball (sop/uv_sphere)] ball))
      (graph editor :context editor
        (let* [left (ui/graph "a" :focus true) right (ui/graph "b")]
          (ui/workspace (ui/split "horizontal" left right)))))"#;
    session("two graphs", source, &|p| {
        let inactive = |p: &P| {
            let core = &*p.e.as_ref().unwrap().core;
            let leaves = graph_leaves(&core.into());
            let (key, _) = leaves.iter().find(|(key, _)| Some(key) != panel_map::key_ref(&core.panes)).unwrap();
            let view = &panel_map::find(&core.panes, key).unwrap().view;
            assert_eq!(view.key, "b");
            (view.key.as_ptr(), view.zoom)
        };
        let (storage, zoom) = inactive(p);
        let at = (1040., 200.);
        p.step(&[], vec![Event::PointerMoved(at)], at);
        let first = p.step(&[], vec![Event::Scrolled((0., 1.))], at);
        let mut frames = vec![first];
        for _ in 0..3 { frames.push(p.step(&[], vec![], at)); }
        assert!(frames[0].1 <= 238 && frames[3].1 <= 233, "inactive graph exceeded its zoom allocation budget: {frames:?}");
        p.record("inactive graph zoom", frames);
        let (after, zoomed) = inactive(p);
        assert_eq!(storage, after, "drawing the inactive graph copied its pane state");
        assert!(zoomed > zoom, "the inactive graph lost its wheel gesture");
    });
}

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![("graph_budget_isogrid", isogrid), ("graph_budget_multi", multi_graph)]
}
