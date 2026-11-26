//! Warm-frame allocation ceilings (fleet Z0, docs/fleet-zero.md). Scenarios A–F from
//! `target/zero-alloc/attribution.md`. Rust counts come from the counting allocator (app + libpxui);
//! native counts from `mem_sys` `trace` when compiled in. Ceilings start at measured + 5%; lanes
//! only lower them toward 0 Rust / ≤12 native. Fresh process per case (runner isolates `zero_alloc_`).
use input::{Event, Key, MouseButton};
use rays_editor::{editor, workspace};
use std::time::Instant;

const W: i64 = 1280;
const H: i64 = 900;
const IDLE_AT: (f64, f64) = (600., 400.);

// Warm/measure: full size under RAYS_SLOW; shorter default so each case stays under the 5 s limit.
fn warm_n() -> usize {
    if std::env::var("RAYS_SLOW").as_deref() == Ok("1") { 60 } else { 24 }
}
fn measure_n() -> u64 {
    if std::env::var("RAYS_SLOW").as_deref() == Ok("1") { 30 } else { 12 }
}
fn action_rounds() -> u64 {
    if std::env::var("RAYS_SLOW").as_deref() == Ok("1") { 20 } else { 4 }
}

/// (rust allocs/unit, native mallocs/unit). Rust = counting allocator; native from trace when available.
/// Seeded at measured (2026-11-25, after Z1/Z2/Z4) + 5%, rounded up. Lanes lower these toward 0 / ≤12.
// Ceilings = measured + 5% after Z6b Image-run + Atlas direct quads (2026-11-25, RAYS_SLOW).
const A_BASIC: (u64, u64) = (63, 9); // measured 60 with shared 2D draw lists
const A_EMPTY: (u64, u64) = (0, 9);
const A_BARE: (u64, u64) = (0, 9);
const A_ISOGRID: (u64, u64) = (0, 13);
// Selected idle: Z8 may_rest path; lowered after Z5 idle cuts (meas 10/12).
const A_ISOSEL: (u64, u64) = (0, 13);
// Z10: measured 20/17 Rust allocations per hover frame.
const B_HOVER_GRAPH: (u64, u64) = (21, 13);
const B_HOVER_INSPECTOR: (u64, u64) = (18, 13);
// The cooked isogrid viewport adds the depth pass; the prior 9 budget was for the undrawn view.
const C_PAN: (u64, u64) = (0, 12);
const C_ZOOM: (u64, u64) = (0, 12);
// Z10: measured 75 Rust allocations per typed character after shared input-frame cuts.
const D_TYPING: (u64, u64) = (79, 13);
// E click/toggle (Z13): settled = idle = 0 after isogrid render 0 + settle wait for zoom-rest timer.
const E_CLICK_KEY: (u64, u64) = (75, 13);
const E_CLICK_SETTLED: (u64, u64) = (0, 13);
// Z10: measured 224 Rust allocations on the full toggle key frame.
const E_TOGGLE_KEY: (u64, u64) = (236, 13);
const E_TOGGLE_SETTLED: (u64, u64) = (0, 13);
const F_FROZEN: (u64, u64) = (0, 8);
const F_STATIC: (u64, u64) = (0, 9);

struct P {
    e: Option<editor::Editor>,
    tick: i64,
    frame: rays::frame::Frame,
    canvas: rays::canvas::Canvas,
    gpu: flow_native::workspace_gpu::WorkspaceGpu,
    freeze: bool,
}

impl P {
    fn new(source: &str) -> P {
        assert!(mem_sys::COUNTING, "zero_alloc needs counting");
        assert!(crate::common::gpu(), "native GPU required");
        let doc = workspace::load(None, Vec::new(), None, source).unwrap_or_else(|d| panic!("{d:?}"));
        let canvas = rays::canvas::create(W, H).unwrap();
        let gpu = flow_native::workspace_gpu::create(None);
        let mut e = editor::create(
            editor::Create {
                gpu_owner: Some(gpu.clone()),
                await_: Some(true),
                platform: Some(input::Platform::Apple),
                ..Default::default()
            },
            &doc,
            None,
            editor::prepare_fn(),
            editor::draw_fn(),
        )
        .unwrap();
        let core = std::rc::Rc::make_mut(&mut e.core);
        core.clock = std::sync::Arc::new(|| 0.);
        sop::async_cook::set_clock(&core.cook.worker, core.clock.clone());
        e.core.ui.borrow_mut().verify_views(false);
        let frame = rays::frame::Frame {
            input: input::Frame { size: (W, H), ..input::at_time(0.) },
            pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.),
        };
        P { e: Some(e), tick: 0, frame, canvas, gpu, freeze: false }
    }

    fn step(&mut self, keys: &[Key], events: impl IntoIterator<Item = Event>, pointer: (f64, f64)) {
        if !self.freeze {
            self.tick += 1;
        }
        let input = &mut self.frame.input;
        input.events.clear();
        input.events.extend(events);
        for event in &input.events {
            match event {
                Event::PointerPressed(button, _) if !input.buttons.contains(button) => input.buttons.push(*button),
                Event::PointerReleased(button, _) => input.buttons.retain(|held| held != button),
                _ => {}
            }
        }
        input.t = self.tick as f64 / 60.;
        input.dt = if self.freeze { 0. } else { 1. / 60. };
        input.frame = self.tick;
        input.tick = self.tick;
        input.pointer = pointer;
        input.keys.clear();
        input.keys.extend_from_slice(keys);
        let e = editor::update(self.e.take().unwrap(), &self.frame);
        assert!(e.context_error.is_none(), "{:?}", e.context_error);
        let sc = editor::scene(&e, &self.frame);
        rays::canvas::render(None, &self.canvas, &sc).unwrap();
        self.e = Some(editor::after_present(e, &self.frame));
    }

    fn idle(&mut self, n: usize, at: (f64, f64)) {
        for _ in 0..n {
            self.step(&[], [], at);
        }
    }

    fn close(self) {
        editor::close(self.e.as_ref().unwrap());
        flow_native::workspace_gpu::close(&self.gpu);
        rays::canvas::destroy(&self.canvas).unwrap();
    }

    fn graph(&self) -> &rays_graph::scope_state::Pane {
        let core = &*self.e.as_ref().unwrap().core;
        rays_editor::editor_model::pane_view(&core.into())
    }

    fn selected(&self) -> usize {
        rays_graph::scope_state::selected(self.graph()).len()
    }
}

fn sk(name: &str) -> String {
    std::fs::read_to_string(format!("{}/../../sketches/{name}", env!("CARGO_MANIFEST_DIR"))).unwrap()
}

struct Acc {
    n: u64,
    mark: u64,
}
impl Acc {
    fn new() -> Acc {
        Acc { n: 0, mark: 0 }
    }
    fn on(&mut self) {
        self.mark = mem_sys::alloc_count();
        #[cfg(any(target_os = "macos", target_os = "ios"))]
        if mem_sys::TRACE {
            mem_sys::trace::resume();
        }
    }
    fn off(&mut self) {
        #[cfg(any(target_os = "macos", target_os = "ios"))]
        if mem_sys::TRACE {
            mem_sys::trace::stop();
        }
        self.n += mem_sys::alloc_count() - self.mark;
    }
}

fn check(name: &str, rust: u64, native: u64, (max_rust, max_native): (u64, u64)) {
    eprintln!("{name}: {rust} rust/unit (ceil {max_rust}), {native} native/unit (ceil {max_native})");
    if rust > max_rust || (mem_sys::TRACE && native > max_native) {
        #[cfg(any(target_os = "macos", target_os = "ios"))]
        if mem_sys::TRACE {
            let report = mem_sys::trace::report(15);
            eprint!("top sites:\n{}", mem_sys::trace::format_sites(&report.sites, 15));
        }
        assert!(
            rust <= max_rust,
            "{name}: {rust} rust allocations/unit exceed ceiling {max_rust}"
        );
        if mem_sys::TRACE {
            assert!(
                native <= max_native,
                "{name}: {native} native mallocs/unit exceed ceiling {max_native}"
            );
        }
    }
}

/// Warm `warm_n` frames of `f`, then measure `measure_n` frames. Returns (rust/unit, native/unit).
fn scenario(p: &mut P, f: &mut dyn FnMut(&mut P, u64)) -> (u64, u64) {
    let warm = warm_n();
    let frames = measure_n();
    for i in 0..warm {
        f(p, i as u64);
    }
    let n0 = mem_sys::alloc_count();
    #[cfg(any(target_os = "macos", target_os = "ios"))]
    if mem_sys::TRACE {
        mem_sys::trace::clear();
        mem_sys::trace::start();
    }
    for i in 0..frames {
        f(p, warm as u64 + i);
    }
    let rust = (mem_sys::alloc_count() - n0).div_ceil(frames);
    let native = native_total(frames);
    (rust, native)
}

fn native_total(units: u64) -> u64 {
    #[cfg(any(target_os = "macos", target_os = "ios"))]
    if mem_sys::TRACE {
        mem_sys::trace::stop();
        return if units == 0 { 0 } else { mem_sys::trace::totals().native / units };
    }
    let _ = units;
    0
}

fn idle_case(name: &str, source: &str, ceil: (u64, u64)) {
    let started = Instant::now();
    let mut p = P::new(source);
    let (rust, native) = scenario(&mut p, &mut |p, _| p.step(&[], [], IDLE_AT));
    p.close();
    check(name, rust, native, ceil);
    eprintln!("{name} wall {:.2}s", started.elapsed().as_secs_f64());
}

fn a_basic() {
    idle_case("A_idle_basic", &sk("basic.rays"), A_BASIC);
}
fn a_empty() {
    idle_case("A_idle_empty", include_str!("../../../rays_bin/src/empty.rays"), A_EMPTY);
}
fn a_bare() {
    idle_case("A_idle_bare", "(workspace empty (graph g :context value 0))", A_BARE);
}
fn a_isogrid() {
    idle_case("A_idle_isogrid", &sk("isogrid.rays"), A_ISOGRID);
}

fn find_node(p: &mut P) -> ((f64, f64), (f64, f64)) {
    let mut node = None;
    let mut empty = None;
    let step = if std::env::var("RAYS_SLOW").as_deref() == Ok("1") { 16 } else { 24 };
    'scan: for y in (40..440).step_by(step) {
        for x in (890..1270).step_by(step) {
            let q = (x as f64, y as f64);
            p.step(&[], [Event::PointerMoved(q)], q);
            p.step(
                &[],
                [
                    Event::PointerPressed(MouseButton::LeftButton, q),
                    Event::PointerReleased(MouseButton::LeftButton, q),
                ],
                q,
            );
            p.idle(2, q);
            let s = p.selected();
            if s > 0 && node.is_none() {
                node = Some(q);
            }
            if s == 0 && empty.is_none() {
                empty = Some(q);
            }
            if node.is_some() && empty.is_some() {
                break 'scan;
            }
        }
    }
    let (node, empty) = (node.expect("no node found"), empty.expect("no empty point"));
    p.step(&[], [Event::PointerMoved(node)], node);
    p.step(
        &[],
        vec![
            Event::PointerPressed(MouseButton::LeftButton, node),
            Event::PointerReleased(MouseButton::LeftButton, node),
        ],
        node,
    );
    p.idle(10, node);
    assert!(p.selected() > 0);
    (node, empty)
}

fn a_isosel() {
    let started = Instant::now();
    let mut p = P::new(&sk("isogrid.rays"));
    p.idle(20, IDLE_AT);
    let (node, _) = find_node(&mut p);
    p.idle(10, node);
    let (rust, native) = scenario(&mut p, &mut |p, _| p.step(&[], [], node));
    p.close();
    check("A_idle_isogrid_selected", rust, native, A_ISOSEL);
    eprintln!("A_idle_isogrid_selected wall {:.2}s", started.elapsed().as_secs_f64());
}

fn b_hover() {
    let started = Instant::now();
    let mut p = P::new(&sk("isogrid.rays"));
    p.idle(20, IDLE_AT);
    let (node, _) = find_node(&mut p);
    // The second click in the scan can open the graph input's default-value editor.
    if p.graph().editing.is_some() {
        p.step(&[], [Event::KeyPressed(Key::Escape)], node);
        p.idle(2, node);
    }
    assert!(p.graph().editing.is_none() && p.selected() > 0, "hover needs a selection and no active edit");
    let mv = |p: &mut P, i: u64, x0: f64, y0: f64| {
        let q = (x0 + (i % 60) as f64 * 6., y0 + ((i / 3) % 40) as f64 * 8.);
        p.step(&[], [Event::PointerMoved(q)], q);
    };
    let (rust, native) = scenario(&mut p, &mut |p, i| mv(p, i, 900., 60.));
    check("B_hover_graph", rust, native, B_HOVER_GRAPH);
    let (rust, native) = scenario(&mut p, &mut |p, i| mv(p, i, 900., 520.));
    check("B_hover_inspector", rust, native, B_HOVER_INSPECTOR);
    p.close();
    eprintln!("B_hover wall {:.2}s", started.elapsed().as_secs_f64());
}

fn c_pan_zoom() {
    let started = Instant::now();
    let mut p = P::new(&sk("isogrid.rays"));
    let at = (1080., 200.);
    p.step(&[], [Event::PointerMoved(at)], at);
    p.idle(20, at);
    assert!(!p.e.as_ref().unwrap().drawn.is_empty(), "C must display the cooked isogrid view");
    let zoom = p.graph().zoom;
    p.step(&[], [Event::Scrolled((0., 1.))], at);
    assert!(p.graph().zoom > zoom, "wheel must zoom the graph");
    p.step(&[], [Event::Scrolled((0., -1.))], at);
    let (rust, native) = scenario(&mut p, &mut |p, i| {
        p.step(&[], [Event::Scrolled((0., if (i / 10) % 2 == 0 { 1. } else { -1. }))], at)
    });
    check("C_zoom", rust, native, C_ZOOM);
    p.idle(10, at);
    p.step(&[], [Event::PointerPressed(MouseButton::RightButton, at)], at);
    let pan = (p.graph().pan_x, p.graph().pan_y);
    let q = |i: u64| {
        let k = (i % 40) as f64;
        let d = if k < 20. { k } else { 40. - k };
        (at.0 - 40. + 4. * d, at.1 + 2. * d)
    };
    let (rust, native) = scenario(&mut p, &mut |p, i| {
        let q = q(i);
        p.step(&[], [Event::PointerMoved(q)], q)
    });
    assert_ne!((p.graph().pan_x, p.graph().pan_y), pan, "right-button drag must pan the graph");
    check("C_pan", rust, native, C_PAN);
    p.step(&[], [Event::PointerReleased(MouseButton::RightButton, at)], at);
    p.close();
    eprintln!("C_pan_zoom wall {:.2}s", started.elapsed().as_secs_f64());
}

fn c_noop_input() {
    let mut p = P::new(&sk("isogrid.rays"));
    let pointer = (1080., 200.);
    p.step(&[], [Event::PointerMoved(pointer)], pointer);
    p.idle(20, pointer);
    p.freeze = true;
    let doc = p.e.as_ref().unwrap().core.doc.clone();
    let view = (p.graph().zoom, p.graph().pan_x, p.graph().pan_y);
    let (pointer_rust, pointer_native) = scenario(&mut p, &mut |p, _| {
        p.step(&[], [Event::PointerMoved(pointer)], pointer);
    });
    let (key_rust, key_native) = scenario(&mut p, &mut |p, _| {
        p.step(&[], [Event::KeyPressed(Key::F8), Event::KeyReleased(Key::F8)], pointer);
    });
    assert!(std::sync::Arc::ptr_eq(&doc, &p.e.as_ref().unwrap().core.doc));
    assert_eq!(view, (p.graph().zoom, p.graph().pan_x, p.graph().pan_y));
    p.close();
    check("C_noop_pointer", pointer_rust, pointer_native, (0, 12));
    check("C_noop_key", key_rust, key_native, (0, 12));
}

fn d_typing() {
    let started = Instant::now();
    let mut p = P::new(&sk("isogrid.rays"));
    let ch = Key::KeyChar;
    let click = (W as f64 * 0.75, H as f64 * 0.4);
    p.idle(20, click);
    for k in [ch('/'), ch('o'), ch('v'), ch('/'), ch('l'), ch('t')] {
        p.step(&[], [Event::KeyPressed(k)], click);
    }
    p.idle(4, click);
    p.step(&[], [Event::PointerMoved(click)], click);
    p.step(
        &[],
        vec![
            Event::PointerPressed(MouseButton::LeftButton, click),
            Event::PointerReleased(MouseButton::LeftButton, click),
        ],
        click,
    );
    p.idle(20, click);
    let (rust, native) = scenario(&mut p, &mut |p, i| {
        p.step(&[], [Event::TextInput(["x", "y"][i as usize % 2].to_string())], click)
    });
    let core = &p.e.as_ref().unwrap().core;
    assert!(core.ui.borrow().text_input_focused(), "typing requires a focused editor area");
    assert!(rays_editor::text_pane::summary(&core.text).contains("draft yes"), "typing must change the draft");
    p.close();
    check("D_typing", rust, native, D_TYPING);
    eprintln!("D_typing wall {:.2}s", started.elapsed().as_secs_f64());
}

fn e_click_toggle() {
    let started = Instant::now();
    let mut p = P::new(&sk("isogrid.rays"));
    p.idle(20, IDLE_AT);
    let (node, empty) = find_node(&mut p);
    p.idle(warm_n(), node);
    let rounds = action_rounds();
    for (phase, ceil) in [(0, E_CLICK_KEY), (1, E_CLICK_SETTLED)] {
        let name = if phase == 0 { "E_click_key" } else { "E_click_settled" };
        let mut acc = Acc::new();
        #[cfg(any(target_os = "macos", target_os = "ios"))]
        if mem_sys::TRACE {
            mem_sys::trace::start();
            mem_sys::trace::stop();
        }
        for i in 0..rounds {
            let q = if i % 2 == 0 { empty } else { node };
            let before = p.selected();
            p.step(&[], [Event::PointerMoved(q)], q);
            p.idle(10, q);
            if phase == 0 {
                acc.on();
            }
            p.step(
                &[],
                [
                    Event::PointerPressed(MouseButton::LeftButton, q),
                    Event::PointerReleased(MouseButton::LeftButton, q),
                ],
                q,
            );
            if phase == 0 {
                acc.off();
            }
            // REST_FRAMES=2: still climbs 0,1,2 across three idles so the measured frame can rest.
            p.idle(3, q);
            if phase == 1 {
                acc.on();
            }
            p.idle(1, q);
            if phase == 1 {
                acc.off();
            }
            assert_ne!(before, p.selected(), "click did not change selection");
            p.idle(6, q);
        }
        check(name, acc.n / rounds, native_total(rounds), ceil);
    }
    let ch = Key::KeyChar;
    for (phase, ceil) in [(0, E_TOGGLE_KEY), (1, E_TOGGLE_SETTLED)] {
        let name = if phase == 0 { "E_toggle_key" } else { "E_toggle_settled" };
        let mut acc = Acc::new();
        #[cfg(any(target_os = "macos", target_os = "ios"))]
        if mem_sys::TRACE {
            mem_sys::trace::start();
            mem_sys::trace::stop();
        }
        for _ in 0..rounds {
            p.idle(10, IDLE_AT);
            p.step(&[], [Event::KeyPressed(ch('/'))], IDLE_AT);
            if phase == 0 {
                acc.on();
            }
            p.step(&[], [Event::KeyPressed(ch('g'))], IDLE_AT);
            if phase == 0 {
                acc.off();
            }
            // REST_FRAMES=2. Settled also waits out graph zoom-rest `repaint_after(0.1)` (~6 frames)
            // so wants_frame is clear (else ~2/20 SLOW rounds wake into a full update).
            p.idle(if phase == 1 { 8 } else { 3 }, IDLE_AT);
            if phase == 1 {
                acc.on();
            }
            p.idle(1, IDLE_AT);
            if phase == 1 {
                acc.off();
            }
        }
        check(name, acc.n / rounds, native_total(rounds), ceil);
    }
    p.close();
    eprintln!("E_click_toggle wall {:.2}s", started.elapsed().as_secs_f64());
}

fn static_basic() -> String {
    sk("basic.rays").replace("(+ previous (frame/dt))", "previous").replace("(frame/index)", "0")
}

fn f_basic_static() {
    let started = Instant::now();
    let mut p = P::new(&sk("basic.rays"));
    p.idle(warm_n(), IDLE_AT);
    p.freeze = true;
    let (rust, native) = scenario(&mut p, &mut |p, _| p.step(&[], [], IDLE_AT));
    check("F_basic_frozen_clock", rust, native, F_FROZEN);
    p.close();
    let mut p = P::new(&static_basic());
    let (rust, native) = scenario(&mut p, &mut |p, _| p.step(&[], [], IDLE_AT));
    check("F_basic_static_source", rust, native, F_STATIC);
    p.close();
    eprintln!("F_basic_static wall {:.2}s", started.elapsed().as_secs_f64());
}

fn basic_pixels_follow_the_canvas_clock() {
    fn circle(p: &P) -> usize {
        rays::canvas::pixels(&p.canvas).unwrap().iter()
            .filter(|color| (color.r, color.g, color.b) == (90, 170, 240))
            .count()
    }
    let mut p = P::new(&sk("basic.rays"));
    p.idle(4, IDLE_AT);
    let first = circle(&p);
    assert!(first > 0, "the circle is visible before animation");
    p.idle(20, IDLE_AT);
    let later = circle(&p);
    assert_ne!(first, later, "the rendered circle follows its animated radius");
    p.freeze = true;
    p.idle(6, IDLE_AT);
    assert_eq!(circle(&p), later, "freezing the clock keeps the last displayed radius");
    p.close();

    let mut p = P::new(&static_basic());
    p.idle(4, IDLE_AT);
    let fixed = circle(&p);
    assert!(fixed > 0, "the static source draws its circle");
    p.idle(20, IDLE_AT);
    assert_eq!(circle(&p), fixed, "the static source holds its displayed radius");
    p.close();
}

/// may_rest: Playing + dt>0 on basic (reads frame/index) must keep redrawing; dt==0 must not
/// advance the timeline frame (rest is then semantically identical).
fn may_rest_basic_clock() {
    let mut p = P::new(&sk("basic.rays"));
    p.idle(4, IDLE_AT);
    let before = {
        let e = p.e.as_ref().unwrap();
        rays_editor::editor_list::timeline(&e.core).frame
    };
    p.step(&[], [], IDLE_AT);
    let e = p.e.as_ref().unwrap();
    let after = rays_editor::editor_list::timeline(&e.core).frame;
    assert!(after > before, "Playing + dt>0 must advance timeline frame (basic frame N); {before} → {after}");
    let f = rays::frame::Frame {
        input: input::Frame {
            dt: 1. / 60.,
            frame: p.tick,
            tick: p.tick,
            size: (W, H),
            pointer: IDLE_AT,
            ..input::at_time(p.tick as f64 / 60.)
        },
        pixel_scale: (1., 1.),
        fps: 60.,
        mouse_delta: (0., 0.),
    };
    assert!(
        !rays_editor::editor_loop::may_rest(e, &f),
        "may_rest must stay awake while Playing with dt>0 and a time-reading canvas"
    );
    p.freeze = true;
    let frozen_at = after;
    p.step(&[], [], IDLE_AT);
    let e = p.e.as_ref().unwrap();
    assert_eq!(
        rays_editor::editor_list::timeline(&e.core).frame,
        frozen_at,
        "dt==0 must not advance frame/index (may_rest clock_idle arm)"
    );
    p.close();
}

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![
        ("zero_alloc_a_basic", a_basic),
        ("zero_alloc_basic_pixels", basic_pixels_follow_the_canvas_clock),
        ("zero_alloc_a_empty", a_empty),
        ("zero_alloc_a_bare", a_bare),
        ("zero_alloc_a_isogrid", a_isogrid),
        ("zero_alloc_a_isosel", a_isosel),
        ("zero_alloc_b_hover", b_hover),
        ("zero_alloc_c_pan_zoom", c_pan_zoom),
        ("zero_alloc_c_noop_input", c_noop_input),
        ("zero_alloc_d_typing", d_typing),
        ("zero_alloc_e_click_toggle", e_click_toggle),
        ("zero_alloc_f_basic_static", f_basic_static),
        ("zero_alloc_may_rest_basic_clock", may_rest_basic_clock),
    ]
}
