//! The Rays host glue PXUI's own headless tests leave out: UI layers as scene nodes drawn by the Rays
//! renderer (the graph zone and port tokens' pixel goldens, one draw call per layer, native content
//! between layers), the system clipboard round trip, param's scrub rule and the memory ledger.
use crate::common;
use input::{Event, Key, MouseButton, Platform};
use pxui::ui::*;
use rays::{canvas, clipboard, frame, image, scene};
use rays::scene::layer;
use rays_math::color;
use std::cell::RefCell;
use std::rc::Rc;

const WIDTH: i64 = 320;

fn golden_ui() -> Ui {
    let path = format!("{}/../../pxui/assets/fonts/DepartureMono-Regular.otf", env!("CARGO_MANIFEST_DIR"));
    rays_shell::host::create(Platform::Apple, None, Some(&path))
}

fn frame_at(count: i64, height: i64, density: i64, events: Vec<Event>) -> frame::Frame {
    let mut input = input::at_time(count as f64 / 60.0);
    input.dt = 1.0 / 60.0;
    input.frame = count;
    input.tick = count;
    input.size = (WIDTH, height);
    input.pointer = (-100.0, -100.0);
    input.events = events;
    frame::Frame { input, pixel_scale: (density as f64, density as f64), fps: 60.0, mouse_delta: (0.0, 0.0) }
}

/// Two frames of a fresh UI rendered offscreen through the Rays renderer, and its draw calls.
fn render(density: i64, height: i64, build: &mut dyn FnMut(&mut Ui)) -> (canvas::Canvas, u64) {
    let ui = Rc::new(RefCell::new(golden_ui()));
    for count in 0..2 {
        rays_shell::host::build(&mut ui.borrow_mut(), &frame_at(count, height, density, vec![Event::PointerMoved((-100.0, -100.0))]), build);
    }
    let canvas_ = canvas::create(WIDTH * density, height * density).expect("canvas");
    let mut nodes = vec![scene::clear(color::rgb(228, 139, 161))];
    nodes.extend(rays_shell::host::scene(&ui, None));
    let before = rays_shell::host::draw_calls();
    canvas::render(Some(density), &canvas_, &nodes).expect("Canvas.render");
    let calls = rays_shell::host::draw_calls() - before;
    rays_shell::host::destroy(&mut ui.borrow_mut());
    (canvas_, calls)
}

/// The graph pane zone tokens: for, fold, sum, a hollow dashed fn zone, a let scope and the
/// branches, with the socket colours of text, functions and records.
fn zones(ui: &mut Ui) {
    let t = ui.tokens();
    let b = ui.node("zones", pxui::Node::DEFAULT.w(Size::Px(320.0)).h(Size::Px(390.0)).at(0.0, 0.0));
    ui.canvas(&b, move |paint, _| {
        let list = [
            rays_graph::scope_paint::zone_for(&t),
            rays_graph::scope_paint::zone_fold(&t),
            rays_graph::scope_paint::zone_sum(&t),
            rays_graph::scope_paint::zone_fn(&t),
            rays_graph::scope_paint::zone_let(&t),
            rays_graph::scope_paint::zone_branch(&t, false),
            rays_graph::scope_paint::zone_branch(&t, true),
        ];
        for (i, zone) in list.iter().enumerate() {
            let x = 12.0 + (i % 2) as f32 * 154.0;
            let y = 12.0 + (i / 2) as f32 * 92.0;
            paint.fill(Rect::new(x, y, 142.0, 80.0), Fill::new(zone.fill));
            if zone.dashed {
                paint.dashed_rect(Rect::new(x, y, 142.0, 80.0), Stroke::new(zone.edge));
            } else {
                paint.stroke(pxui::Rect::new(x + 0.5, y + 0.5, 141.0, 79.0), pxui::Stroke::new(zone.edge));
            }
        }
        let ports = rays_graph::scope_paint::ports(&t);
        for (i, c) in [ports.text, ports.fn_, ports.record].iter().enumerate() {
            paint.fill(Rect::new(170.0 + i as f32 * 20.0, 200.0, 10.0, 10.0), Fill::new(*c));
        }
    });
}

/// The zone tokens through the Rays renderer at 1x and 2x match their goldens (exactly on Metal)
/// (`RAYS_UPDATE_FIXTURES=1` rewrites them), in one draw call.
fn window_host_zones_parity() {
    if !common::gpu() {
        return;
    }
    // Metal rasterises the goldens: exact. Vulkan blends a channel off by 1-2 (NVIDIA and lavapipe round
    // straight-alpha blends differently; gpu_graph_pixels allows the same), so there a pixel differs past 2.
    let tolerance = if ogpu_core::impl_::backend().expect("backend") == "vulkan" { 2 } else { 0 };
    for density in [1, 2] {
        let (canvas_, calls) = render(density, 390, &mut zones);
        assert_eq!(calls, 1, "the zones layer drew in {calls} draw calls");
        let golden = format!("{}/fixtures/kit_zones_{density}x.png", env!("CARGO_MANIFEST_DIR"));
        if std::env::var("RAYS_UPDATE_FIXTURES").as_deref() == Ok("1") {
            canvas::save_png(&canvas_, &golden).expect("refresh golden");
        }
        let actual = image::pixels(&canvas::to_image(&canvas_).expect("to_image")).expect("pixels");
        let expected = image::pixels(&image::load(&golden).expect("golden")).expect("pixels");
        let different = actual.chunks(4).zip(expected.chunks(4)).filter(|(a, b)| a.iter().zip(*b).any(|(x, y)| x.abs_diff(*y) > tolerance)).count();
        assert!(actual.len() == expected.len() && different == 0, "zones at {density}x: {different} pixels differ from {golden}");
        let _ = canvas::destroy(&canvas_);
    }
}

/// A raised control's layer goes above native content placed under it, the base layer below.
fn window_host_layers_around_native_content() {
    let mut ui = golden_ui();
    let mut front_key = 0;
    rays_shell::host::build(&mut ui, &frame_at(0, 240, 1, vec![]), &mut |ui| {
        let front = ui.node("front", pxui::Node::DEFAULT.flags(pxui::Flags::CLICKABLE).at(0.0, 0.0).w(Size::Px(100.0)).h(Size::Px(60.0)));
        front_key = front.box_key;
        ui.to_front(0, &front);
        let back = ui.node("back", pxui::Node::DEFAULT.flags(pxui::Flags::CLICKABLE).at(0.0, 0.0).w(Size::Px(100.0)).h(Size::Px(60.0)));
        for b in [&front, &back] {
            ui.canvas(b, |paint, Rect { x, y, w, h }| paint.fill(pxui::Rect::new(x, y, w, h), pxui::Fill::new(pxui::Rgba::WHITE)));
        }
    });
    let under = move |k: i64| -> Vec<scene::Node> {
        if k == front_key { vec![scene::rect((0, 0), 100, 60, Some(color::black()), None).unwrap()] } else { Vec::new() }
    };
    let ui = Rc::new(RefCell::new(ui));
    let nodes = rays_shell::host::scene(&ui, Some(&under));
    let staged = layer::stage_native(320, 240, &nodes).unwrap_or_else(|_| panic!("staging failed"));
    assert!(
        matches!(&staged.layers[..], [layer::NativeLayer::UiLayer(..), layer::NativeLayer::Scene2Layer(..), layer::NativeLayer::UiLayer(..)]),
        "native content was not between the background and floating controls"
    );
    rays_shell::host::destroy(&mut ui.borrow_mut());
}

/// Copy goes to the system clipboard after the frame; paste arrives from it with the next frame.
fn window_host_clipboard() {
    let previous = clipboard::get_text();
    let value = RefCell::new("Functional UI".to_owned());
    let mut ui = golden_ui();
    let step = |ui: &mut Ui, count: i64, keys: Vec<Key>, events: Vec<Event>| {
        let mut f = frame_at(count, 240, 1, events);
        f.input.keys = keys;
        rays_shell::host::frame(ui, &f, &mut |ui| {
            let v = value.borrow().clone();
            *value.borrow_mut() = ui.text_field("caption", &v, &pxui::TextField::DEFAULT.name("Caption")).changed.unwrap_or(&v).to_owned();
        });
    };
    step(&mut ui, 0, vec![], vec![]);
    let field = (200.0, 12.0);
    step(&mut ui, 1, vec![], vec![Event::PointerPressed(MouseButton::LeftButton, field), Event::PointerReleased(MouseButton::LeftButton, field)]);
    step(&mut ui, 2, vec![Key::Meta], vec![Event::KeyPressed(Key::KeyChar('a'))]);
    step(&mut ui, 3, vec![Key::Meta], vec![Event::KeyPressed(Key::KeyChar('c'))]);
    let copied = clipboard::get_text();
    clipboard::set_text("pasted").expect("clipboard");
    step(&mut ui, 4, vec![Key::Meta], vec![Event::KeyPressed(Key::KeyChar('v'))]);
    step(&mut ui, 5, vec![], vec![]);
    if let Ok(text) = previous {
        let _ = clipboard::set_text(&text);
    }
    assert_eq!(copied.as_deref(), Ok("Functional UI"), "copy did not reach the clipboard");
    assert_eq!(*value.borrow(), "pasted", "paste did not arrive from the clipboard");
    rays_shell::host::destroy(&mut ui);
}

/// The text area's integer scrub is param's rule (one per six points). Floats differ on purpose:
/// the text area steps a tenth of the literal's last place, the inspector its own ranges.
fn window_host_scrub_rule() {
    for literal in ["10", "-3"] {
        for dx in [-20.0, -6.0, -2.0, 0.0, 2.0, 6.0, 20.0] {
            assert_eq!(scrubbed(literal, dx, false), param::number::scrub(param::number::Kind::Int, None, literal, f64::from(dx), false), "{literal} by {dx}");
        }
    }
}

/// The ledger follows the bytes the UI reports, and destroying it takes them off.
fn window_host_memory_ledger() {
    let owner = mem::document("pxui.text_undo");
    let at_start = mem::bytes(&owner);
    let mut ui = golden_ui();
    let mut edit = ui.load_text_edit("undo-ledger", "small");
    edit.text = "x".repeat(10_000);
    ui.remember("undo-ledger", &edit, false);
    rays_shell::host::charge(&mut ui);
    assert_eq!(mem::bytes(&owner) - at_start, ui.text_undo_bytes() as i64, "text undo's charge is not what it holds");
    rays_shell::host::destroy(&mut ui);
    assert_eq!(mem::bytes(&owner), at_start, "destroying the UI releases its charge");
}

/// Destroying a UI destroys its renderer now, though the scene's cached layers still hold the paint closure (and so the
/// renderer's cell): a renderer left to the closure keeps its lease on the device past the device's end.
fn window_host_destroy_ends_renderer_under_cached_paint() {
    if !common::gpu() {
        return;
    }
    let ui = Rc::new(RefCell::new(golden_ui()));
    rays_shell::host::build(&mut ui.borrow_mut(), &frame_at(0, 40, 1, vec![]), &mut |ui| {
        let b = ui.node("b", pxui::Node::DEFAULT.w(Size::Px(20.0)).h(Size::Px(20.0)).at(0.0, 0.0));
        ui.canvas(&b, |paint, Rect { x, y, w, h }| paint.fill(pxui::Rect::new(x, y, w, h), pxui::Fill::new(pxui::Rgba::WHITE)));
    });
    let canvas_ = canvas::create(WIDTH, 40).expect("canvas");
    let kept = rays_shell::host::scene(&ui, None); // what the native-stage cache keeps
    canvas::render(Some(1), &canvas_, &kept).expect("Canvas.render");
    let cell = rays_shell::host::renderer_probe(&ui.borrow()).expect("the UI has a scene entry");
    assert!(cell.borrow().is_some(), "the frame made a renderer");
    rays_shell::host::destroy(&mut ui.borrow_mut());
    assert!(cell.borrow().is_none(), "destroy left the renderer to the cached paint closure");
    drop(kept);
    let _ = canvas::destroy(&canvas_);
}

fn window_host_replayed_ui_defers_image_drop() {
    use rays::sketch;
    use rays_shell::host::{self, HostImage};

    if !common::gpu() {
        return;
    }
    common::release_keepalive();
    thread_local! {
        static SERIALS: RefCell<Vec<(u64, u64)>> = const { RefCell::new(Vec::new()) };
    }
    scene::on_frame_begin(|frame| SERIALS.with(|serials| {
        serials.borrow_mut().push((frame.serial, frame.completed));
        Ok(())
    }));
    struct State {
        ui: Rc<RefCell<Ui>>,
        image: Option<HostImage>,
        dropped_at: Option<u64>,
    }
    let live = || ogpu_core::impl_::live_handles().expect("live handles");
    let before = live();
    let options = sketch::RunOptions {
        config: Some(sketch::Config { width: 96, height: 64, fps: None, ..sketch::default_config() }),
        max_frames: Some(5),
        ..Default::default()
    };
    let program = sketch::Program {
        init: Box::new(|_| Box::new(State { ui: Rc::new(RefCell::new(golden_ui())), image: None, dropped_at: None })),
        update: Box::new(|state, frame| {
            let state = state.downcast_mut::<State>().unwrap();
            if frame.input.frame == 2 {
                let source = image::create(4, 4, Some(color::red())).expect("image");
                state.image = Some(host::image(&source).expect("host image after first frame"));
                image::destroy(&source).expect("destroy source image");
            }
            host::build(&mut state.ui.borrow_mut(), frame, &mut |ui| {
                let box_ = ui.node("image area", Node::DEFAULT.w_px(20.).h_px(20.));
                let image = state.image.as_ref().map(|image| **image);
                let offset = frame.input.frame as f32;
                ui.canvas(&box_, move |paint, _| {
                    let rect = Rect::new(offset, 0., 20., 20.);
                    paint.fill(rect, Fill::new(pxui::Rgba::WHITE));
                    if let Some(image) = image {
                        paint.image(rect, &image, &pxui::ImageStyle::DEFAULT).expect("paint image");
                    }
                });
            });
            None
        }),
        view: Box::new(|state, _| host::scene(&state.downcast_ref::<State>().unwrap().ui, None)),
        after_present: Some(Box::new(move |state, frame| {
            let state = state.downcast_mut::<State>().unwrap();
            if frame.input.frame == 3 {
                let (serial, completed) = SERIALS.with(|serials| *serials.borrow().last().unwrap());
                assert!(completed < serial, "the presented image's frame must be in flight");
                let before_drop = live();
                drop(state.image.take().expect("the frame sampled the image"));
                assert_eq!(live(), before_drop, "dropping the host image freed an in-flight texture");
                state.dropped_at = Some(serial);
            }
            None
        })),
        crash_dump: None,
        idle: None,
        on_stop: Some(Box::new(|state| host::destroy(&mut state.downcast_ref::<State>().unwrap().ui.borrow_mut()))),
    };
    let state = sketch::run_state(options, program).unwrap_or_else(|error| panic!("{}", sketch::error_to_string(&error)));
    let dropped_at = state.downcast_ref::<State>().unwrap().dropped_at.expect("image drop was exercised");
    let frames = SERIALS.with(|serials| std::mem::take(&mut *serials.borrow_mut()));
    assert_eq!(frames.len(), 5, "each replay prepared the UI");
    assert!(frames.windows(2).all(|pair| pair[0].0 < pair[1].0), "each replay has its own frame serial");
    assert!(frames.iter().any(|&(serial, completed)| serial > dropped_at && completed >= dropped_at),
        "the renderer did not report completion of the image's last frame");
    assert_eq!(live(), before, "a retired HostImage leaked a native texture");
}

pub const CASES: &[(&str, fn())] = &[
    ("window_host_zones_parity", window_host_zones_parity),
    ("window_host_layers_around_native_content", window_host_layers_around_native_content),
    ("window_host_clipboard", window_host_clipboard),
    ("window_host_scrub_rule", window_host_scrub_rule),
    ("window_host_memory_ledger", window_host_memory_ledger),
    ("window_host_destroy_ends_renderer_under_cached_paint", window_host_destroy_ends_renderer_under_cached_paint),
    ("window_host_replayed_ui_defers_image_drop", window_host_replayed_ui_defers_image_drop),
];
