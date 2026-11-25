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
const A_BASIC: (u64, u64) = (241, 9);
const A_EMPTY: (u64, u64) = (0, 9);
const A_BARE: (u64, u64) = (0, 9);
const A_ISOGRID: (u64, u64) = (0, 13);
// Selected idle: Z8 may_rest path; lowered after Z5 idle cuts (meas 10/12).
const A_ISOSEL: (u64, u64) = (11, 13);
const B_HOVER_GRAPH: (u64, u64) = (161, 13);
const B_HOVER_INSPECTOR: (u64, u64) = (159, 13);
const C_PAN: (u64, u64) = (143, 13);
const C_ZOOM: (u64, u64) = (139, 13);
const D_TYPING: (u64, u64) = (385, 13);
const E_CLICK_KEY: (u64, u64) = (256, 13);
const E_CLICK_SETTLED: (u64, u64) = (11, 13);
// E_toggle_key: layout-only recovery print splices prior body (no SyntaxBuilder::import of source).
const E_TOGGLE_KEY: (u64, u64) = (365, 15);
const E_TOGGLE_SETTLED: (u64, u64) = (11, 15);
const F_FROZEN: (u64, u64) = (118, 8);
const F_STATIC: (u64, u64) = (123, 9);

struct P {
    e: Option<editor::Editor>,
    tick: i64,
    buttons: Vec<MouseButton>,
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
        P { e: Some(e), tick: 0, buttons: Vec::new(), canvas, gpu, freeze: false }
    }

    fn step(&mut self, keys: &[Key], events: Vec<Event>, pointer: (f64, f64)) {
        if !self.freeze {
            self.tick += 1;
        }
        let t = self.tick;
        for event in &events {
            match event {
                Event::PointerPressed(button, _) if !self.buttons.contains(button) => self.buttons.push(*button),
                Event::PointerReleased(button, _) => self.buttons.retain(|held| held != button),
                _ => {}
            }
        }
        let dt = if self.freeze { 0. } else { 1. / 60. };
        let f = rays::frame::Frame {
            input: input::Frame {
                dt,
                frame: t,
                tick: t,
                size: (W, H),
                pointer,
                keys: keys.to_vec(),
                buttons: self.buttons.clone(),
                events,
                ..input::at_time(t as f64 / 60.)
            },
            pixel_scale: (1., 1.),
            fps: 60.,
            mouse_delta: (0., 0.),
        };
        let e = editor::update(self.e.take().unwrap(), &f);
        assert!(e.context_error.is_none(), "{:?}", e.context_error);
        let sc = editor::scene(&e, &f);
        rays::canvas::render(None, &self.canvas, &sc).unwrap();
        self.e = Some(editor::after_present(e, &f));
    }

    fn idle(&mut self, n: usize, at: (f64, f64)) {
        for _ in 0..n {
            self.step(&[], vec![], at);
        }
    }

    fn close(self) {
        editor::close(self.e.as_ref().unwrap());
        flow_native::workspace_gpu::close(&self.gpu);
        rays::canvas::destroy(&self.canvas).unwrap();
    }

    fn selected(&self) -> usize {
        let core = &*self.e.as_ref().unwrap().core;
        rays_graph::scope_state::selected(rays_editor::editor_model::pane_view(&core.into())).len()
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
            eprint!("top sites:
{}", mem_sys::trace::format_sites(&report.sites, 15));
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
    let rust = (mem_sys::alloc_count() - n0) / frames;
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
    let (rust, native) = scenario(&mut p, &mut |p, _| p.step(&[], vec![], IDLE_AT));
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
            p.step(&[], vec![Event::PointerMoved(q)], q);
            p.step(
                &[],
                vec![
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
    p.step(&[], vec![Event::PointerMoved(node)], node);
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
    let (rust, native) = scenario(&mut p, &mut |p, _| p.step(&[], vec![], node));
    p.close();
    check("A_idle_isogrid_selected", rust, native, A_ISOSEL);
    eprintln!("A_idle_isogrid_selected wall {:.2}s", started.elapsed().as_secs_f64());
}

fn b_hover() {
    let started = Instant::now();
    let mut p = P::new(&sk("isogrid.rays"));
    p.idle(20, IDLE_AT);
    find_node(&mut p);
    let mv = |p: &mut P, i: u64, x0: f64, y0: f64| {
        let q = (x0 + (i % 60) as f64 * 6., y0 + ((i / 3) % 40) as f64 * 8.);
        p.step(&[], vec![Event::PointerMoved(q)], q);
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
    p.step(&[], vec![Event::PointerMoved(at)], at);
    p.idle(20, at);
    let (rust, native) = scenario(&mut p, &mut |p, i| {
        p.step(&[], vec![Event::Scrolled((0., if (i / 10) % 2 == 0 { 1. } else { -1. }))], at)
    });
    check("C_zoom", rust, native, C_ZOOM);
    p.idle(10, at);
    p.step(&[], vec![Event::PointerPressed(MouseButton::RightButton, at)], at);
    let q = |i: u64| {
        let k = (i % 40) as f64;
        let d = if k < 20. { k } else { 40. - k };
        (at.0 - 40. + 4. * d, at.1 + 2. * d)
    };
    let (rust, native) = scenario(&mut p, &mut |p, i| {
        let q = q(i);
        p.step(&[], vec![Event::PointerMoved(q)], q)
    });
    check("C_pan", rust, native, C_PAN);
    p.step(&[], vec![Event::PointerReleased(MouseButton::RightButton, at)], at);
    p.close();
    eprintln!("C_pan_zoom wall {:.2}s", started.elapsed().as_secs_f64());
}

fn d_typing() {
    let started = Instant::now();
    let mut p = P::new(&sk("isogrid.rays"));
    let ch = Key::KeyChar;
    let click = (W as f64 * 0.75, H as f64 * 0.4);
    p.idle(20, click);
    for k in [ch('/'), ch('o'), ch('v'), ch('/'), ch('l'), ch('t')] {
        p.step(&[], vec![Event::KeyPressed(k)], click);
    }
    p.idle(4, click);
    p.step(&[], vec![Event::PointerMoved(click)], click);
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
        p.step(&[], vec![Event::TextInput(["x", "y"][i as usize % 2].to_string())], click)
    });
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
            p.step(&[], vec![Event::PointerMoved(q)], q);
            p.idle(10, q);
            if phase == 0 {
                acc.on();
            }
            p.step(
                &[],
                vec![
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
            p.step(&[], vec![Event::KeyPressed(ch('/'))], IDLE_AT);
            if phase == 0 {
                acc.on();
            }
            p.step(&[], vec![Event::KeyPressed(ch('g'))], IDLE_AT);
            if phase == 0 {
                acc.off();
            }
            // REST_FRAMES=2: three idles so still>=2 before the measured rest frame.
            p.idle(3, IDLE_AT);
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

fn f_basic_static() {
    let started = Instant::now();
    let mut p = P::new(&sk("basic.rays"));
    p.idle(warm_n(), IDLE_AT);
    p.freeze = true;
    let (rust, native) = scenario(&mut p, &mut |p, _| p.step(&[], vec![], IDLE_AT));
    check("F_basic_frozen_clock", rust, native, F_FROZEN);
    p.close();
    let src = sk("basic.rays")
        .replace("(+ previous (frame/dt))", "previous")
        .replace("(frame/index)", "0");
    let mut p = P::new(&src);
    let (rust, native) = scenario(&mut p, &mut |p, _| p.step(&[], vec![], IDLE_AT));
    check("F_basic_static_source", rust, native, F_STATIC);
    p.close();
    eprintln!("F_basic_static wall {:.2}s", started.elapsed().as_secs_f64());
}

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![
        ("zero_alloc_a_basic", a_basic),
        ("zero_alloc_a_empty", a_empty),
        ("zero_alloc_a_bare", a_bare),
        ("zero_alloc_a_isogrid", a_isogrid),
        ("zero_alloc_a_isosel", a_isosel),
        ("zero_alloc_b_hover", b_hover),
        ("zero_alloc_c_pan_zoom", c_pan_zoom),
        ("zero_alloc_d_typing", d_typing),
        ("zero_alloc_e_click_toggle", e_click_toggle),
        ("zero_alloc_f_basic_static", f_basic_static),
    ]
}
