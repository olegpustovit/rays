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
    let path = format!("{}/../../assets/fonts/DepartureMono-Regular.otf", env!("CARGO_MANIFEST_DIR"));
    Ui::create(Platform::Apple, None, rays_shell::host::text(Some(&path)))
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
        ui.borrow_mut().frame(&frame_at(count, height, density, vec![Event::PointerMoved((-100.0, -100.0))]), build);
    }
    let canvas_ = canvas::create(WIDTH * density, height * density).expect("canvas");
    let mut nodes = vec![scene::clear(color::rgb(228, 139, 161))];
    nodes.extend(rays_shell::host::scene(&ui, None));
    let before = rays_shell::host::draw_calls();
    canvas::render(Some(density), &canvas_, &nodes).expect("Canvas.render");
    let calls = rays_shell::host::draw_calls() - before;
    ui.borrow_mut().destroy();
    (canvas_, calls)
}

/// The graph pane zone tokens: for, fold, sum, a hollow dashed fn zone, a let scope and the
/// branches, with the socket colours of text, functions and records.
fn zones(ui: &mut Ui) {
    let t = ui.theme();
    let b = ui.box_("zones", BoxOpts::default().w(Size::Px(320.0)).h(Size::Px(390.0)).at((0.0, 0.0)));
    ui.draw(&b, move |paint, _| {
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
            paint.rect(x, y, 142.0, 80.0, Some(zone.fill), None);
            if zone.dashed {
                paint.dashed_rect(x, y, 142.0, 80.0, zone.edge);
            } else {
                paint.stroke(x + 0.5, y + 0.5, 141.0, 79.0, zone.edge);
            }
        }
        let ports = rays_graph::scope_paint::ports(&t);
        for (i, c) in [ports.text, ports.fn_, ports.record].iter().enumerate() {
            paint.rect(170.0 + i as f32 * 20.0, 200.0, 10.0, 10.0, Some(*c), None);
        }
    });
}

/// The zone tokens through the Rays renderer at 1x and 2x match their goldens exactly
/// (`RAYS_UPDATE_FIXTURES=1` rewrites them), in one draw call.
fn window_host_zones_parity() {
    if !common::gpu() {
        return;
    }
    for density in [1, 2] {
        let (canvas_, calls) = render(density, 390, &mut zones);
        assert_eq!(calls, 1, "the zones layer drew in {calls} draw calls");
        let golden = format!("{}/fixtures/kit_zones_{density}x.png", env!("CARGO_MANIFEST_DIR"));
        if std::env::var("RAYS_UPDATE_FIXTURES").as_deref() == Ok("1") {
            canvas::save_png(&canvas_, &golden).expect("refresh golden");
        }
        let actual = image::pixels(&canvas::to_image(&canvas_).expect("to_image")).expect("pixels");
        let expected = image::pixels(&image::load(&golden).expect("golden")).expect("pixels");
        let different = actual.chunks(4).zip(expected.chunks(4)).filter(|(a, b)| a != b).count();
        assert!(actual.len() == expected.len() && different == 0, "zones at {density}x: {different} pixels differ from {golden}");
        let _ = canvas::destroy(&canvas_);
    }
}

/// A raised control's layer goes above native content placed under it, the base layer below.
fn window_host_layers_around_native_content() {
    let mut ui = golden_ui();
    let mut front_key = 0;
    ui.frame(&frame_at(0, 240, 1, vec![]), &mut |ui| {
        let front = ui.box_("front", BoxOpts::default().flags(CLICKABLE).at((0.0, 0.0)).w(Size::Px(100.0)).h(Size::Px(60.0)));
        front_key = key(&front);
        ui.to_front(0, &front);
        let back = ui.box_("back", BoxOpts::default().flags(CLICKABLE).at((0.0, 0.0)).w(Size::Px(100.0)).h(Size::Px(60.0)));
        for b in [&front, &back] {
            ui.draw(b, |paint, Rect { x, y, w, h }| paint.fill(x, y, w, h, pxui::geom::color::white()));
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
    ui.borrow_mut().destroy();
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
            *value.borrow_mut() = ui.text_field("Caption", &v).unwrap_or(v);
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
    ui.destroy();
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
    let mut edit = ui.load_text_edit(1, "small");
    edit.text = "x".repeat(10_000);
    ui.remember(&edit, false);
    rays_shell::host::charge(&mut ui);
    assert_eq!(mem::bytes(&owner) - at_start, ui.text_undo_bytes() as i64, "text undo's charge is not what it holds");
    rays_shell::host::destroy(&mut ui);
    assert_eq!(mem::bytes(&owner), at_start, "destroying the UI releases its charge");
}

pub const CASES: &[(&str, fn())] = &[
    ("window_host_zones_parity", window_host_zones_parity),
    ("window_host_layers_around_native_content", window_host_layers_around_native_content),
    ("window_host_clipboard", window_host_clipboard),
    ("window_host_scrub_rule", window_host_scrub_rule),
    ("window_host_memory_ledger", window_host_memory_ledger),
];
