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
    frames: usize,
    cpu_us: f64,
    bytes: usize,
    allocs: u64,
}

/// `n` frames of `step` after the run's frames so far: per-frame averages over the frames the UI
/// actually built; `views` is the sum of the views each frame rebuilt
fn measure(r: &mut Run, n: usize, step: &dyn Fn(&mut Run, usize)) -> Cost {
    let mut c = Cost { views: 0, built: 0, frames: 0, cpu_us: 0.0, bytes: 0, allocs: 0 };
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
    eprintln!("ui_frames {name}: {}/{n} frames built, {} nodes/frame, {} view rebuilds in all, {:.0} us UI/frame, {} instance bytes/frame, {} allocs/frame", c.frames, c.built, c.views, c.cpu_us, c.bytes, c.allocs);
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

pub fn run_ui_frames() {
    workspace("basic", &sketch("basic"));
    workspace("empty", include_str!("../../../rays_bin/src/empty.rays"));
    workspace("floating", &with_editor("(let* [main (ui/graph) ins (ui/inspector) window (ui/floating ins) tiles (ui/tile main window)] (ui/workspace tiles))"));
    if std::env::var_os("RAYS_SLOW").is_some() {
        workspace("orrery", &case("orrery"));
    }
}
