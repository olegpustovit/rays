//! UI frame costs of the whole editor (P5 views): nodes built, PXUI CPU, views rebuilt, instance
//! bytes written and allocations per frame, idle, hovering, playing, hovering from button to button,
//! scrubbing an inspector value, panning and zooming the graph (its content view offset and scaled,
//! nothing built until the zoom rests). Prints a table; asserts the frame gate and
//! which views each gesture rebuilds.
use super::support::*;
use input::{Event, MouseButton};

struct Cost {
    built: usize,
    views: usize,
    /// of `views`, the inspector's (its view and its sections')
    inspector: usize,
    frames: usize,
    cpu_us: f64,
    bytes: usize,
    allocs: u64,
}

/// `n` frames of `step` after the run's frames so far: per-frame averages over the frames the UI
/// actually built; `views` is the sum of the views each frame rebuilt
fn measure(r: &mut Run, n: usize, step: &dyn Fn(&mut Run, usize)) -> Cost {
    let mut c = Cost { views: 0, inspector: 0, built: 0, frames: 0, cpu_us: 0.0, bytes: 0, allocs: 0 };
    // what frames before this left to draw is theirs, not the first measured frame's
    {
        let mut ui = r.e.core.ui.borrow_mut();
        let layers = ui.output().draw.len();
        for i in 0..layers {
            ui.drawn(i);
        }
    }
    let allocs = mem_sys::alloc_count();
    for i in 0..n {
        r.e.core.ui.borrow_mut().set_timing(true);
        step(r, i);
        let mut ui = r.e.core.ui.borrow_mut();
        let times = ui.phase_times();
        let cpu: std::time::Duration = times.iter().sum();
        if cpu > std::time::Duration::ZERO {
            c.frames += 1;
            c.built += ui.built_nodes();
            c.views += ui.rebuilt_views();
            c.inspector += ui.rebuilt_named("inspector-view") + ui.rebuilt_named("flow-section-");
            c.cpu_us += cpu.as_secs_f64() * 1e6;
        }
        let out = ui.output();
        c.bytes += out.draw.iter().flat_map(|l| l.dirty.iter()).map(|(a, b)| (b - a) as usize).sum::<usize>() * std::mem::size_of::<pxui::draw::Instance>();
        let layers = out.draw.len();
        for i in 0..layers {
            ui.drawn(i);
        }
    }
    c.allocs = (mem_sys::alloc_count() - allocs) / n as u64;
    let f = c.frames.max(1);
    (c.built, c.cpu_us, c.bytes) = (c.built / f, c.cpu_us / f as f64, c.bytes / n);
    c
}

fn report(name: &str, c: &Cost, n: usize) {
    eprintln!("ui_frames {name}: {}/{n} frames built, {} nodes/frame, {} view rebuilds in all ({} the inspector's), {:.0} us UI/frame, {} instance bytes/frame, {} allocs/frame", c.frames, c.built, c.views, c.inspector, c.cpu_us, c.bytes, c.allocs);
}

/// The window points of a pane's header collapse button and of the middle of its title.
fn header_points(r: &Run, panel: editor_core::panels::Panel) -> ((f64, f64), (f64, f64)) {
    let f = frame_full(&[], &[], (0., 0.), (0., 0.), vec![], r.count);
    let core: &rays_editor::editor_model::EditorModel = &r.e.core;
    let g = rays_editor::editor_shell::geometry(&core.into(), &core.workspace, &f);
    let leaf = g.leaves.iter().find(|l| l.panel == panel).expect("pane");
    let (x, y, w, h) = leaf.header;
    (((x + w - 18) as f64, (y + h / 2) as f64), ((x + 40) as f64, (y + h / 2) as f64))
}

/// The centre of the first accessible node named `name`.
fn named(r: &Run, name: &str) -> Option<(f64, f64)> {
    let ui = r.e.core.ui.borrow();
    let tree = ui.output().a11y;
    tree.nodes.iter().find(|n| &tree.strings[n.name.start as usize..n.name.end as usize] == name).map(|n| ((n.rect.x + n.rect.w / 2.0) as f64, (n.rect.y + n.rect.h / 2.0) as f64))
}

fn workspace(name: &str, text: &str) {
    let mut r = Run::new(editor(text));
    // measured as the app runs: no verifying rebuilds
    r.e.core.ui.borrow_mut().verify_views(false);
    for _ in 0..20 {
        r.step((450., 300.), vec![]);
    }
    // a cooking workspace (orrery) takes fewer frames: the case stays well under its 5 seconds
    let n = if name == "orrery" { 12 } else { 30 };
    let idle = measure(&mut r, n, &|r, _| r.step((450., 300.), vec![]));
    report(&format!("{name} idle"), &idle, n);
    let hover = measure(&mut r, n, &|r, i| {
        let p = (450. + (i % 2) as f64, 300.);
        r.step(p, vec![Event::PointerMoved(p)]);
    });
    report(&format!("{name} pointer"), &hover, n);
    // the frame gate: an editor at rest builds no frames; one playing a clock (basic, orrery) rebuilds
    // no chrome, inspector or graph view (the status line's cook readout alone); a pointer moving
    // inside one node rebuilds no view
    check(idle.frames == 0 || name != "empty", &format!("{name}: idle built {} frames", idle.frames));
    check(idle.views == 0 || name != "basic", &format!("{name}: playing rebuilt {} views", idle.views));
    // (a cook that publishes during the run, its timing the machine's, rebuilds what shows its results)
    check(idle.views <= idle.frames + 2 || name != "orrery", &format!("{name}: playing rebuilt {} views in {} frames", idle.views, idle.frames));
    check(hover.views == 0 || name != "empty", &format!("{name}: pointer motion rebuilt {} views", hover.views));
    // with a floating window open the docked grounds stay a kept view (the window's ground is a view
    // of its own): the pointer moving over the docked graph rebuilds nothing
    check(hover.views == 0 || name != "floating", &format!("{name}: pointer motion rebuilt {} views", hover.views));
    check(hover.built <= 15 || name != "floating", &format!("{name}: pointer motion built {} nodes a frame", hover.built));
    if name == "empty" {
        use editor_core::panels::Panel;
        // from one header's button to another's: the two header views (the one left, the one entered)
        let (graph, graph_title) = header_points(&r, Panel::Graph);
        let (inspector, _) = header_points(&r, Panel::Inspector);
        let to = |r: &mut Run, p: (f64, f64)| r.step(p, vec![Event::PointerMoved(p)]);
        to(&mut r, graph);
        let across = measure(&mut r, n, &|r, i| to(r, if i % 2 == 0 { inspector } else { graph }));
        report("empty header button to header button", &across, n);
        check(across.views == 2 * n, &format!("hovering across headers rebuilt {} views in {n} frames", across.views));
        // within one header, from its button to its title: that header's view alone
        let within = measure(&mut r, n, &|r, i| to(r, if i % 2 == 0 { graph_title } else { graph }));
        report("empty header button to its title", &within, n);
        check(within.views == n, &format!("hovering inside a header rebuilt {} views in {n} frames", within.views));
        // scrubbing a number of the inspector (the camera's FOV: the body scrolled to its section,
        // the section opened): the Camera section's view and the inspector's around it, every frame
        // of the drag
        // the accessibility tree finds the rows (on from here: its walk costs the frames it runs in)
        r.e.core.ui.borrow_mut().set_a11y(true);
        to(&mut r, (451., 300.));
        let body = named(&r, "picture").expect("the inspector's first section");
        for _ in 0..12 {
            r.step(body, vec![Event::PointerMoved(body), Event::Scrolled((0.0, -3.0))]);
        }
        for _ in 0..30 {
            to(&mut r, body);
        }
        // its heading's foot: the head may lie under the clip of the body's top
        let camera = named(&r, "Camera").map(|(x, y)| (x, y + 14.0)).expect("the Camera section");
        // opening the Camera section (one content-sized view) re-lays its siblings out in that
        // frame: the Render section after it rebuilds (its gap follows an open section now), the
        // Scene root section below moves without a rebuild
        let root_before = named(&r, "Scene root").expect("the Scene root section");
        to(&mut r, camera);
        r.stepb(&[MouseButton::LeftButton], camera, vec![Event::PointerPressed(MouseButton::LeftButton, camera)]);
        let open = measure(&mut r, 1, &|r, _| r.step(camera, vec![Event::PointerReleased(MouseButton::LeftButton, camera)]));
        report("empty open a section", &open, 1);
        let root_after = named(&r, "Scene root").expect("the Scene root section");
        check(open.views == 3, &format!("opening a section rebuilt {} views (the inspector, Camera, Render)", open.views));
        check(root_after.1 > root_before.1 + 100.0, &format!("the section below did not move: {root_before:?} -> {root_after:?}"));
        for _ in 0..4 {
            to(&mut r, camera);
        }
        let fov = named(&r, "FOV").expect("the FOV row");
        let at = (fov.0 + 90.0, fov.1);
        // the same drag twice: the first warms the texts it shows (the values it passes through)
        let mut scrub = None;
        let mut fov = (0.0, 0.0);
        for pass in 0..2 {
            r.stepb(&[MouseButton::LeftButton], at, vec![Event::PointerMoved(at), Event::PointerPressed(MouseButton::LeftButton, at)]);
            // (the drag's first frames: the inspector takes the held field's camera in)
            for i in 0..2 {
                let p = (at.0 - 2.0 + i as f64, at.1);
                r.stepb(&[MouseButton::LeftButton], p, vec![Event::PointerMoved(p)]);
            }
            let before = rays::easy_camera::fov_y(&r.e.camera);
            let cost = measure(&mut r, n, &|r, i| {
                let p = (at.0 + 1.0 + i as f64 / 2.0, at.1);
                r.stepb(&[MouseButton::LeftButton], p, vec![Event::PointerMoved(p)]);
            });
            fov = (before, rays::easy_camera::fov_y(&r.e.camera));
            let end = (at.0 + 1.0 + n as f64 / 2.0, at.1);
            r.step(end, vec![Event::PointerMoved(end), Event::PointerReleased(MouseButton::LeftButton, end)]);
            for _ in 0..4 {
                to(&mut r, end);
            }
            scrub = (pass == 1).then_some(cost);
        }
        let scrub = scrub.expect("the measured drag");
        report("empty scrub inspector value", &scrub, n);
        // the field's drag rebuilds no view and builds no node past a pointer frame's (the camera
        // section, its field `kept_drag`, stays kept and so does the inspector around it): the editor
        // reads the drag after the frame and patches the field's value and line alone, allocating
        // nothing past what a pointer frame does
        eprintln!("ui_frames empty scrub: FOV {:.4} -> {:.4} rad", fov.0, fov.1);
        check(fov.1 > fov.0, &format!("the scrub did not change the FOV: {fov:?}"));
        check(scrub.views == 0, &format!("scrubbing rebuilt {} views in {n} frames", scrub.views));
        check(scrub.built <= hover.built, &format!("scrubbing built {} nodes a frame (a pointer frame {})", scrub.built, hover.built));
        check(scrub.bytes <= 8 * 64, &format!("scrubbing wrote {} instance bytes a frame (the field's value and line: at most 8 instances)", scrub.bytes));
        check(scrub.allocs <= hover.allocs, &format!("scrubbing allocated {} a frame (a pointer frame {})", scrub.allocs, hover.allocs));
        // panning the graph: a right drag over its empty canvas
        let (gx, gy) = (graph.0 - 40.0, graph.1 + 120.0);
        // (measured, so its writes are not counted in the drag's first frame)
        measure(&mut r, 1, &|r, _| r.stepb(&[MouseButton::RightButton], (gx, gy), vec![Event::PointerMoved((gx, gy)), Event::PointerPressed(MouseButton::RightButton, (gx, gy))]));
        let pan = measure(&mut r, n, &|r, i| {
            let p = (gx - 2.0 * (i + 1) as f64, gy);
            r.stepb(&[MouseButton::RightButton], p, vec![Event::PointerMoved(p)]);
        });
        report("empty graph pan", &pan, n);
        // the graph's content is a kept view the pan offsets: no view built, no instance written
        check(pan.views == 0 && pan.bytes == 0, &format!("panning rebuilt {} views and wrote {} instance bytes a frame", pan.views, pan.bytes));
        let end = (gx - 2.0 * (n + 1) as f64, gy);
        r.step(end, vec![Event::PointerReleased(MouseButton::RightButton, end)]);
        // zooming it: wheel steps scale the content view (built again once the zoom rests 0.1 s)
        for _ in 0..12 {
            to(&mut r, end);
        }
        let zoom = measure(&mut r, n, &|r, _| r.step(end, vec![Event::Scrolled((0.0, 1.0))]));
        report("empty graph zoom", &zoom, n);
        check(zoom.views == 0 && zoom.bytes == 0, &format!("zooming rebuilt {} views and wrote {} instance bytes a frame", zoom.views, zoom.bytes));
        let rest = measure(&mut r, 12, &|r, _| r.step(end, vec![]));
        report("empty graph zoom at rest", &rest, 12);
        // (the graph's anchored views: its wires view with a view for its ground, each open zone, frame
        // and wire in it, its content and each card's view in it)
        let anchored = {
            let core: &rays_editor::editor_model::EditorModel = &r.e.core;
            rays_graph::scope_pane::inspect::anchored_views(rays_editor::editor_model::pane_view(&core.into()))
        };
        check(rest.views == anchored, &format!("the rested zoom rebuilt {} views ({anchored} anchored)", rest.views));
        // a sketch's panel: with a revision it is a kept view while the revision stays (its closure not
        // called, no view built while the pointer moves), a new revision builds it once; without one it
        // builds every frame
        let built = std::rc::Rc::new(std::cell::Cell::new(0usize));
        let sketch = |r: &mut Run, revision: Option<u64>, p: (f64, f64)| {
            r.count += 1;
            let f = frame_full(&[], &[], (0., 0.), p, vec![Event::PointerMoved(p)], r.count);
            let b = built.clone();
            let panel = move |ui: &mut pxui::ui::Ui| -> std::rc::Rc<dyn std::any::Any> {
                b.set(b.get() + 1);
                ui.inspector_toggle("sketch toggle", "Sketch", true);
                std::rc::Rc::new(())
            };
            r.e = match revision {
                Some(v) => rays_editor::editor::update_with_revision(r.e.clone(), &f, v, &panel).0,
                None => rays_editor::editor::update_with(r.e.clone(), &f, &panel).0,
            };
        };
        for i in 0..4 {
            sketch(&mut r, Some(1), (450. + (i % 2) as f64, 300.));
        }
        built.set(0);
        let kept = measure(&mut r, n, &|r, i| sketch(r, Some(1), (450. + (i % 2) as f64, 300.)));
        report("empty sketch panel with a revision", &kept, n);
        check(built.get() == 0 && kept.views == 0, &format!("a sketch panel with an unchanged revision built {} times ({} views)", built.get(), kept.views));
        sketch(&mut r, Some(2), (450., 300.));
        check(built.get() == 1, &format!("a new revision built the sketch panel {} times", built.get()));
        built.set(0);
        let every = measure(&mut r, n, &|r, i| sketch(r, None, (450. + (i % 2) as f64, 300.)));
        report("empty sketch panel without a revision", &every, n);
        check(built.get() == n, &format!("a sketch panel without a revision built {} times in {n} frames", built.get()));
    }
    r.close();
}

/// Scrubbing a number of the document in the inspector (a node's `:width`): each frame of the drag
/// writes the document (the viewport and the graph follow), the inspector stays kept (its field
/// patched, `kept_drag`), and the whole scrub is one undo step back to the value it started from.
fn document_scrub() {
    use input::Key;
    let text = include_str!("../../../rays_bin/src/empty.rays")
        .replace(r##"(draw/background "#11151c")"##, r##"(let* [stroke (draw/line [0 0 0] [5 5 0] :color "#0f0" :width 3.0)] stroke)"##)
        .replace(":second_size 280", ":second_size 520");
    let mut r = Run::new(editor(&text));
    r.e.core.ui.borrow_mut().verify_views(false);
    r.e = rays_editor::reduce::select_path(r.e.clone(), &["picture".into(), "stroke".into()]);
    r.e.core.ui.borrow_mut().set_a11y(true);
    for i in 0..30 {
        r.step((450. + (i % 2) as f64, 300.), vec![]);
    }
    let n = 20;
    let width = |r: &Run| r.source().split(":width ").nth(1).and_then(|t| t.split([' ', ')']).next()).unwrap_or("").to_owned();
    // the inspector's row (the card's row has a pin of that name)
    let ix = r.panes().inspector.0 as f32;
    let row = {
        let ui = r.e.core.ui.borrow();
        let tree = ui.output().a11y;
        tree.nodes.iter().find(|n| &tree.strings[n.name.start as usize..n.name.end as usize] == "width" && n.rect.x >= ix).map(|n| ((n.rect.x + n.rect.w / 2.0) as f64, (n.rect.y + n.rect.h / 2.0) as f64))
    };
    let row = row.unwrap_or_else(|| fail("the width row"));
    let at = (row.0 + 90.0, row.1);
    let to = |r: &mut Run, p: (f64, f64)| r.step(p, vec![Event::PointerMoved(p)]);
    to(&mut r, at);
    let hover = measure(&mut r, n, &|r, i| to(r, (at.0 + (i % 2) as f64, at.1)));
    report("document pointer", &hover, n);
    let (start, history) = (width(&r), r.undo_label());
    // the scrub: a press, two frames the drag takes in, then the measured frames
    r.stepb(&[MouseButton::LeftButton], at, vec![Event::PointerMoved(at), Event::PointerPressed(MouseButton::LeftButton, at)]);
    for i in 0..2 {
        let p = (at.0 + 1.0 + i as f64, at.1);
        r.stepb(&[MouseButton::LeftButton], p, vec![Event::PointerMoved(p)]);
    }
    let scrub = measure(&mut r, n, &|r, i| {
        let p = (at.0 + 3.0 + 2.0 * i as f64, at.1);
        r.stepb(&[MouseButton::LeftButton], p, vec![Event::PointerMoved(p)]);
    });
    report("document scrub inspector value", &scrub, n);
    let scrubbed = width(&r);
    let end = (at.0 + 3.0 + 2.0 * n as f64, at.1);
    r.step(end, vec![Event::PointerMoved(end), Event::PointerReleased(MouseButton::LeftButton, end)]);
    for _ in 0..4 {
        to(&mut r, end);
    }
    let released = width(&r);
    eprintln!("ui_frames document scrub: width {start} -> {scrubbed} (released {released})");
    check(scrubbed.parse::<f64>().is_ok_and(|v| v > 3.0), &format!("the scrub did not write the document live: width {scrubbed}"));
    // the release, 2 points on from the last of 41, moves it as far as 2 points do: the drag keeps
    // the soft range it had at the press (a literal's range follows its value; it once sped up)
    let (from, last, end_value) = (3.0, scrubbed.parse::<f64>().unwrap_or(0.0), released.parse::<f64>().unwrap_or(0.0));
    check(end_value > last && end_value - last < (last - from) / 10.0, &format!("the release jumped: width {scrubbed} -> {released}"));
    // one scrub, one undo step back to where it started
    check(r.undo_label() != history, "the scrub left no history entry");
    r.step_full(&[], &[Key::Meta], end, vec![ch('z')]);
    for _ in 0..4 {
        to(&mut r, end);
    }
    check(width(&r) == start && r.undo_label() == history, &format!("one undo did not restore the scrubbed width: {} ({:?})", width(&r), r.undo_label()));
    // what the document's edit costs alone: the same edits written by the host, a pointer frame each
    let edit = measure(&mut r, n, &|r, i| {
        let op = flow_graph::flow_edit::Op::SetArg { node: vec!["picture".into(), "stroke".into()], key: flow_graph::flow_edit::ArgKey::Kw("width".into()), sub: vec![], value: flow_syntax::syntax::make(flow_syntax::syntax::Draft::Num(format!("{}.5", 3 + i))) };
        r.e = rays_editor::editor::edit(r.e.clone(), &op).unwrap_or_else(|m: String| fail(&m));
        to(r, (end.0 + (i % 2) as f64, end.1));
    });
    report("document edit by the host", &edit, n);
    // the scrub's frames rebuild no view of the inspector (kept: the field patched; the card and
    // the viewport follow the document by their own revisions), and build and allocate no more than
    // a pointer frame and the document's edit by the host (which rebuilds the inspector's view and
    // the edited section)
    check(scrub.inspector == 0, &format!("scrubbing rebuilt {} of the inspector's views in {n} frames", scrub.inspector));
    check(edit.inspector == 2 * n, &format!("the host's edits rebuilt {} of the inspector's views in {n} frames", edit.inspector));
    check(scrub.built <= edit.built, &format!("scrubbing built {} nodes a frame (the host's edits {})", scrub.built, edit.built));
    check(scrub.allocs <= edit.allocs, &format!("scrubbing allocated {} a frame (the host's edits {})", scrub.allocs, edit.allocs));
    r.close();
}

/// A drag of `at` by `n` measured frames (after a press and two frames the drag takes in), its
/// release and four frames after: the measured cost, how often the views named `name` rebuilt and
/// how far `counter` moved in the measured frames.
fn scrub_at(r: &mut Run, at: (f64, f64), n: usize, name: &'static str, counter: &dyn Fn(&Run) -> u64) -> (Cost, usize, u64) {
    let rebuilt = std::cell::Cell::new(0);
    r.stepb(&[MouseButton::LeftButton], at, vec![Event::PointerMoved(at), Event::PointerPressed(MouseButton::LeftButton, at)]);
    for i in 0..2 {
        let p = (at.0 + 1.0 + i as f64, at.1);
        r.stepb(&[MouseButton::LeftButton], p, vec![Event::PointerMoved(p)]);
    }
    let before = counter(r);
    let cost = measure(r, n, &|r, i| {
        let p = (at.0 + 3.0 + 2.0 * i as f64, at.1);
        r.stepb(&[MouseButton::LeftButton], p, vec![Event::PointerMoved(p)]);
        rebuilt.set(rebuilt.get() + r.e.core.ui.borrow().rebuilt_named(name));
    });
    let counted = counter(r) - before;
    let end = (at.0 + 3.0 + 2.0 * n as f64, at.1);
    r.step(end, vec![Event::PointerMoved(end), Event::PointerReleased(MouseButton::LeftButton, end)]);
    for _ in 0..4 {
        r.step(end, vec![Event::PointerMoved(end)]);
    }
    (cost, rebuilt.get(), counted)
}

/// One undo (Cmd-Z) and four frames after it.
fn undo_once(r: &mut Run) {
    r.step_full(&[], &[input::Key::Meta], (450., 300.), vec![ch('z')]);
    for _ in 0..4 {
        r.step((450., 300.), vec![]);
    }
}

/// Scrubbing a number on a graph card (a `sop/uv_sphere`'s `:radius`): each frame of the drag
/// writes the document (the inspector follows), the card and the graph's content stay kept (the field
/// patched, `kept_drag`), and the scrub is one undo step. The first pass verifies the kept views
/// (`verify_views`), the second is measured as the app runs.
fn card_scrub() {
    let text = r#"(workspace cards (graph g :context sop (let* [ball (sop/uv_sphere :radius 0.5 :segments 8 :rings 4)] ball)) (graph editor :context editor (ui/workspace (ui/split "horizontal" (ui/graph "g") (ui/inspector) :second_size 260))))"#;
    let mut r = Run::new(editor(text));
    for i in 0..30 {
        r.step((450. + (i % 2) as f64, 300.), vec![]);
    }
    let n = 20;
    let radius = |r: &Run| r.source().split(":radius ").nth(1).and_then(|t| t.split([' ', ')']).next()).unwrap_or("").to_owned();
    let ball: Vec<flow_syntax::symbol::Symbol> = vec!["g".into(), "ball".into()];
    let pane = |r: &Run| {
        let core: &rays_editor::editor_model::EditorModel = &r.e.core;
        rays_editor::editor_model::pane_view(&core.into()).clone()
    };
    // the field: the row's middle, half a field in from the card's right edge
    let at = {
        let t = pane(&r);
        let node = rays_graph::scope_state::node_of(&t, &ball).unwrap_or_else(|| fail("the ball card"));
        let i = node.rows.iter().position(|r| r.label == "radius").unwrap_or_else(|| fail("the radius row"));
        let (_, y) = rays_graph::scope_pane::inspect::row_center(&t, &ball, i).unwrap_or_else(|| fail("the radius row's place"));
        let (x, _, w, _) = rays_graph::scope_pane::inspect::box_of(&t, &ball).unwrap_or_else(|| fail("the ball card's box"));
        (x + w - (9.0 + 38.0) * t.zoom, y)
    };
    let (start, history) = (radius(&r), r.undo_label());
    let mut scrubbed = None;
    for pass in 0..2 {
        r.e.core.ui.borrow_mut().verify_views(pass == 0);
        r.step(at, vec![Event::PointerMoved(at)]);
        let (cost, card, content) = scrub_at(&mut r, at, n, "t:g/ball", &|r| rays_graph::scope_pane::inspect::content_builds(&pane(r)));
        let live = radius(&r);
        check(live.parse::<f64>().is_ok_and(|v| v > 0.5), &format!("the card scrub did not write the document: radius {live}"));
        undo_once(&mut r);
        check(radius(&r) == start && r.undo_label() == history, &format!("one undo did not restore the card's scrubbed radius: {} ({:?})", radius(&r), r.undo_label()));
        scrubbed = Some((cost, card, content, live));
    }
    let (scrub, card, content, live) = scrubbed.unwrap_or_else(|| fail("the measured scrub"));
    report("card scrub", &scrub, n);
    // the same edits written by the host, a pointer frame each
    let edit = measure(&mut r, n, &|r, i| {
        let op = flow_graph::flow_edit::Op::SetArg { node: vec!["g".into(), "ball".into()], key: flow_graph::flow_edit::ArgKey::Kw("radius".into()), sub: vec![], value: flow_syntax::syntax::make(flow_syntax::syntax::Draft::Num(format!("{:.2}", 0.6 + 0.1 * i as f64))) };
        r.e = rays_editor::editor::edit(r.e.clone(), &op).unwrap_or_else(|m: String| fail(&m));
        r.step((450. + (i % 2) as f64, 300.), vec![Event::PointerMoved((450. + (i % 2) as f64, 300.))]);
    });
    report("card edit by the host", &edit, n);
    eprintln!("ui_frames card scrub: radius {start} -> {live}, {card} rebuilds of the card's view, {content} content builds in {n} frames");
    check(card == 0 && content == 0, &format!("the card scrub rebuilt the card's view {card} times and the graph's content {content} times in {n} frames"));
    check(scrub.allocs <= edit.allocs, &format!("the card scrub allocated {} a frame (the host's edits {})", scrub.allocs, edit.allocs));
    r.close();
}

/// Scrubbing a graph input's slider in the Outline (`radius`): each frame of the drag sets the
/// input's default (the canvas follows), the navigator's view stays kept (its field patched,
/// `kept_drag`), and the scrub is one undo step. The first pass verifies the kept views.
fn navigator_scrub() {
    use editor_core::panels::Panel;
    let text = r##"(workspace inputs (graph g :context draw [(radius : float 5)] (draw/circle [10 10 0] radius :fill "#00ffff")) (graph editor :context editor (let* [outline (ui/outline) main (ui/graph "g") view (ui/canvas (ref g))] (ui/workspace (ui/split "horizontal" outline (ui/split "vertical" view main) :first_size 216)))))"##;
    let mut r = Run::new(editor(text));
    for i in 0..30 {
        r.step((450. + (i % 2) as f64, 300.), vec![]);
    }
    let n = 20;
    let radius = |r: &Run| r.source().split("(radius : float ").nth(1).and_then(|t| t.split([' ', ')']).next()).unwrap_or("").to_owned();
    // the slider: the middle of the radius row's field
    let at = {
        let f = frame_full(&[], &[], (0., 0.), (0., 0.), vec![], r.count);
        let core: &rays_editor::editor_model::EditorModel = &r.e.core;
        let g = rays_editor::editor_shell::geometry(&core.into(), &core.workspace, &f);
        let body = g.leaves.iter().find(|l| l.panel == Panel::Outline).unwrap_or_else(|| fail("the outline")).body;
        let pa = rays_editor::editor_panels::navigator_params(&core.into());
        let rows = rays_editor::navigator::row_rects(&core.outline, &pa, body);
        let (_, (x, y, w, h)) = rows.into_iter().find(|(row, _)| matches!(row, rays_editor::navigator::Row::InputRow { name, .. } if name == "radius")).unwrap_or_else(|| fail("the radius slider"));
        // (a narrow sheet: 56 points of label)
        ((x + 20.0 + 56.0 + (w - 32.0 - 56.0) / 2.0) as f64, (y + h / 2.0) as f64)
    };
    let (start, history) = (radius(&r), r.undo_label());
    let mut scrubbed = None;
    for pass in 0..2 {
        r.e.core.ui.borrow_mut().verify_views(pass == 0);
        r.step(at, vec![Event::PointerMoved(at)]);
        let (cost, views, _) = scrub_at(&mut r, at, n, "navigator-view", &|_| 0);
        let live = radius(&r);
        check(live.parse::<f64>().is_ok_and(|v| v > 5.0), &format!("the slider scrub did not write the document: radius {live} ({})", r.source()));
        undo_once(&mut r);
        check(radius(&r) == start && r.undo_label() == history, &format!("one undo did not restore the scrubbed radius: {} ({:?})", radius(&r), r.undo_label()));
        scrubbed = Some((cost, views, live));
    }
    let (scrub, views, live) = scrubbed.unwrap_or_else(|| fail("the measured scrub"));
    report("navigator scrub", &scrub, n);
    // the same edits written by the host (the navigator shows each: its view builds every frame)
    let hosted = std::cell::Cell::new(0);
    let edit = measure(&mut r, n, &|r, i| {
        let op = flow_graph::flow_edit::Op::SetInputDefault { form: "g".into(), input: "radius".into(), value: flow_syntax::syntax::make(flow_syntax::syntax::Draft::Num(format!("{}.5", 5 + i))) };
        r.e = rays_editor::editor::edit(r.e.clone(), &op).unwrap_or_else(|m: String| fail(&m));
        r.step((450. + (i % 2) as f64, 300.), vec![Event::PointerMoved((450. + (i % 2) as f64, 300.))]);
        hosted.set(hosted.get() + r.e.core.ui.borrow().rebuilt_named("navigator-view"));
    });
    report("navigator edit by the host", &edit, n);
    eprintln!("ui_frames navigator scrub: radius {start} -> {live}, {views} rebuilds of the navigator's view in {n} frames (the host's edits {})", hosted.get());
    check(views == 0 && hosted.get() == n, &format!("the slider scrub rebuilt the navigator's view {views} times in {n} frames (the host's edits {})", hosted.get()));
    r.close();
}

pub fn run_ui_frames() {
    document_scrub();
    card_scrub();
    navigator_scrub();
    workspace("basic", &sketch("basic"));
    workspace("empty", include_str!("../../../rays_bin/src/empty.rays"));
    workspace("floating", &with_editor("(let* [main (ui/graph) ins (ui/inspector) window (ui/floating ins) tiles (ui/tile main window)] (ui/workspace tiles))"));
    if std::env::var_os("RAYS_SLOW").is_some() {
        workspace("orrery", &case("orrery"));
    }
}
