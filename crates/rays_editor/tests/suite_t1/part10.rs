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
        r.stepb(&[MouseButton::LeftButton], at, vec![Event::PointerMoved(at), Event::PointerPressed(MouseButton::LeftButton, at)]);
        let scrub = measure(&mut r, n, &|r, i| {
            let p = (at.0 + 1.0 + i as f64, at.1);
            r.stepb(&[MouseButton::LeftButton], p, vec![Event::PointerMoved(p)]);
        });
        report("empty scrub inspector value", &scrub, n);
        check(scrub.views == 2 * n, &format!("scrubbing rebuilt {} views in {n} frames", scrub.views));
        check(scrub.built <= 60, &format!("scrubbing built {} nodes a frame", scrub.built));
        let end = (at.0 + 1.0 + n as f64, at.1);
        r.step(end, vec![Event::PointerMoved(end), Event::PointerReleased(MouseButton::LeftButton, end)]);
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
        // (the instances it writes are the chrome's zoom readout; the graph's own are window_graph_view_pan_zoom's)
        check(zoom.views == 0, &format!("zooming rebuilt {} views", zoom.views));
        let rest = measure(&mut r, 12, &|r, _| r.step(end, vec![]));
        report("empty graph zoom at rest", &rest, 12);
        check(rest.views == 1, &format!("the rested zoom rebuilt {} views", rest.views));
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
