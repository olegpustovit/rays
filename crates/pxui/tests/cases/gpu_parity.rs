//! Pixel goldens pin kit widgets and overlays at 1x and 2x, rendered headless: the UI's layers drawn
//! by pxui_render's painter into an offscreen texture, whatever the display. Every channel is
//! checked; `RAYS_UPDATE_FIXTURES=1` rewrites the goldens.
use crate::gpu;
use crate::png;
use input::{frame, Event, Platform};
use pxui::ui::*;
use std::rc::Rc;

const WIDTH: i64 = 320;
const DENSITIES: [i64; 2] = [1, 2];

fn golden_path(name: &str) -> String {
    format!("{}/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name)
}

fn frame_at(count: i64, height: i64, density: i64) -> frame::Frame {
    let mut input = input::at_time(count as f64 / 60.0);
    input.dt = 1.0 / 60.0;
    input.frame = count;
    input.tick = count;
    input.size = (WIDTH, height);
    input.pointer = (-100.0, -100.0);
    input.events = vec![Event::PointerMoved((-100.0, -100.0))];
    frame::Frame { input, pixel_scale: (density as f64, density as f64), fps: 60.0, mouse_delta: (0.0, 0.0) }
}

/// The second frame of a fresh UI with the pointer parked away from every widget, rendered at
/// `density`, and the painter's draw calls for it.
fn render(density: i64, height: i64, build: &mut dyn FnMut(&mut Ui)) -> (Vec<u8>, u64) {
    let mut ui = Ui::create(Platform::Apple, None, crate::support::golden_text());
    for count in 0..2 {
        ui.frame(&frame_at(count, height, density), build);
    }
    let rendered = gpu::render(&mut ui, (WIDTH as u32, height as u32), density as u32, [228, 139, 161, 255]);
    ui.destroy();
    rendered
}

/// Compare `pixels` with the golden at `density`; a drift names the pixels that differ.
fn check(name: &str, golden: &str, density: i64, height: i64, pixels: &[u8]) {
    let path = golden_path(&golden.replace("_2x.png", &format!("_{density}x.png")));
    let (w, h) = ((WIDTH * density) as u32, (height * density) as u32);
    if std::env::var("RAYS_UPDATE_FIXTURES").as_deref() == Ok("1") {
        std::fs::write(&path, png::encode(w, h, pixels)).expect("refresh parity golden");
    }
    let (gw, gh, g) = png::decode(&std::fs::read(&path).expect("golden"));
    assert!((gw, gh) == (w, h), "PXUI {name} parity: golden is {gw}x{gh}, render is {w}x{h}");
    // drivers round blends differently by a level (lavapipe): 2 per channel, as pxui_render's golden
    const TOLERANCE: i32 = 2;
    let mut different = 0usize;
    let mut worst = 0i32;
    let mut first = None;
    for (index, (p, q)) in g.chunks(4).zip(pixels.chunks(4)).enumerate() {
        let most = (0..4).map(|k| (p[k] as i32 - q[k] as i32).abs()).max().unwrap_or(0);
        worst = worst.max(most);
        if most > TOLERANCE {
            different += 1;
            first.get_or_insert((index as u32 % w, index as u32 / w));
        }
    }
    assert!(
        different == 0,
        "PXUI {name} drifted from its golden: {different} of {} pixels differ by more than {TOLERANCE} (largest channel difference {worst}), first at {:?}",
        g.len() / 4,
        first
    );
    println!("PXUI {name} parity at {density}x: max channel difference {worst}");
}

/// The second theme, from its file alone.
fn dusk() -> Theme {
    Theme::parse(include_str!("../../themes/dusk.theme"), &Theme::default()).expect("dusk.theme")
}

/// The kit panel under the dusk theme.
fn dusk_panel(ui: &mut Ui) {
    let theme = dusk();
    ui.themed(&theme, &mut |ui| new_panel(ui));
}

fn new_panel(ui: &mut Ui) {
    ui.panel(PanelOpts::default(), "panel", &mut |ui| {
        ui.label("PXUI");
        ui.toggle("Animate", true);
        ui.toggle("Wire", false);
        ui.text_field("Caption", "Functional UI");
        ui.button("Quit");
    });
}

/// The floating widgets: a modal with a picker, and a context menu.
fn overlays(ui: &mut Ui) {
    ui.modal(Some(220.0), "modal", &mut |ui| {
        ui.label("Presets");
        let rows = |_: &str| {
            vec![
                ("wall".to_owned(), "09-24".to_owned()),
                ("tower".to_owned(), "09-23".to_owned()),
                ("arch".to_owned(), "09-20".to_owned()),
                ("dome".to_owned(), "09-19".to_owned()),
            ]
        };
        ui.picker(&PickerOpts { limit: 3, ..PickerOpts::default() }, "Search presets", "", &rows);
    });
    ui.context_menu(
        (12.0, 200.0),
        &MenuOpts::default(),
        "context",
        &[("View".to_owned(), true), ("Set active camera".to_owned(), false), ("Delete".to_owned(), true)],
    );
}

fn table(ui: &mut Ui) {
    let headers: Vec<String> = ["index", "P.x", "weight", "group"].iter().map(|s| s.to_string()).collect();
    let cell = Rc::new(|row: i64, column: i64| -> String {
        if column == 0 {
            row.to_string()
        } else if column == 3 {
            if row % 2 == 0 { "1".to_owned() } else { "0".to_owned() }
        } else {
            cfmt::g((row + column) as f64 * 0.125, 6)
        }
    });
    ui.table((8.0, 8.0), 304.0, 280.0, &headers, 1000000, cell, "table");
}

fn parity(name: &str, golden: &str, height: i64, build: fn(&mut Ui)) {
    if !gpu::available() {
        return;
    }
    for density in DENSITIES {
        let (pixels, _) = render(density, height, &mut |ui| build(ui));
        check(name, golden, density, height, &pixels);
    }
}

fn gpu_parity_overlays() {
    parity("overlays", "kit_overlays_2x.png", 300, overlays);
}

fn gpu_parity_table() {
    parity("table", "kit_table_2x.png", 300, table);
}

fn gpu_parity_panel() {
    parity("kit panel", "kit_panel_2x.png", 300, new_panel);
}

/// Restyling from data alone: the kit panel under `themes/dusk.theme` has its golden, rounded corners
/// that show the ground, a lifted shadow and a gradient button.
fn gpu_parity_dusk() {
    if !gpu::available() {
        return;
    }
    parity("dusk panel", "kit_dusk_2x.png", 300, dusk_panel);
    let (pixels, calls) = render(2, 300, &mut |ui| dusk_panel(ui));
    assert_eq!(calls, 1, "the dusk panel drew in {calls} draw calls");
    let px = |x: f32, y: f32| {
        let i = (((y * 2.0) as usize) * (WIDTH as usize * 2) + (x * 2.0) as usize) * 4;
        [pixels[i], pixels[i + 1], pixels[i + 2]]
    };
    let ground = [228, 139, 161];
    let panel = [0x1d, 0x22, 0x30];
    assert_eq!(px(120.0, 76.0), panel, "the panel's inside is not its fill");
    // the panel spans (12, 12) to (292, 136); its corner is cut by the 10-point radius
    assert!(px(12.5, 12.5) != panel, "the panel's corner is not rounded");
    assert_eq!(px(300.0, 290.0), ground, "far from the panel is not the ground");
    let shade = px(150.0, 140.0);
    assert!(shade.iter().zip(ground).all(|(s, g)| *s < g), "no shadow under the panel: {shade:?}");
    // the button's ground runs 114 to 134: a gradient from #3b82f6 down to #1d4ed8
    let (top, bottom) = (px(24.0, 115.0), px(24.0, 133.0));
    assert!(top[1] > bottom[1] + 30 && top[0] > bottom[0], "the button is not a top-to-bottom gradient: {top:?} {bottom:?}");
    println!("PXUI dusk: rounded corner, shadow and gradient endpoints {top:?} {bottom:?}");
}

/// Every fixture paints nonempty layers at 1x and 2x, without a device.
fn gpu_parity_commands() {
    let fixtures: [(&str, i64, fn(&mut Ui)); 3] = [("overlays", 300, overlays), ("table", 300, table), ("panel", 300, new_panel)];
    for density in DENSITIES {
        for (name, height, build) in fixtures.iter() {
            let mut ui = Ui::create(Platform::Apple, None, crate::support::golden_text());
            for count in 0..2 {
                ui.frame(&frame_at(count, *height, density), &mut |ui| build(ui));
            }
            let draw = ui.output().draw;
            assert!(!draw.is_empty() && draw.iter().any(|l| !l.instances.is_empty()), "{name}: no UI instances at {density}x");
            ui.destroy();
        }
    }
    println!("PXUI layers: three parity fixtures at 1x/2x");
}

/// One draw call per UI layer: the kit panel, and the table, a clipped scroll list whose clips
/// ride in the instances.
fn gpu_parity_draw_calls() {
    if !gpu::available() {
        return;
    }
    for (name, build) in [("kit panel", new_panel as fn(&mut Ui)), ("table", table)] {
        let (_, calls) = render(2, 300, &mut |ui| build(ui));
        assert_eq!(calls, 1, "{name}: one layer drew in {calls} draw calls");
    }
}

/// A node patched between builds renders as a fresh build with the patched values would.
fn gpu_patch() {
    if !gpu::available() {
        return;
    }
    let chip = |text: &'static str, fill: (i64, i64, i64)| {
        move |ui: &mut Ui| -> Handle {
            let look = Look { fill: pxui::geom::color::rgb(fill.0, fill.1, fill.2), text: pxui::geom::color::black(), ..Look::NONE };
            let h = ui.node("clock", Node { w: Size::Px(120.0), h: Size::Px(24.0), at: Some((10.0, 10.0)), look, inset: Edges { l: 6.0, ..Edges::default() }, ..Node::DEFAULT });
            ui.text(&h, text);
            h
        }
    };
    let shot = |ui: &mut Ui| gpu::render(ui, (WIDTH as u32, 60), 2, [228, 139, 161, 255]).0;
    let built = |text: &'static str, fill| {
        let mut ui = Ui::create(Platform::Apple, None, crate::support::golden_text());
        let mut handle = None;
        for count in 0..2 {
            ui.frame(&frame_at(count, 60, 2), &mut |ui| handle = Some(chip(text, fill)(ui)));
        }
        (ui, handle.unwrap())
    };
    let (mut ui, clock) = built("12:00", (200, 40, 40));
    let before = shot(&mut ui);
    assert!(ui.patch_fill(&clock, pxui::geom::color::rgb(40, 40, 200)) && ui.patch_text(&clock, "12:00:01"), "patch refused");
    let patched = shot(&mut ui);
    ui.destroy();
    let (mut fresh, _) = built("12:00:01", (40, 40, 200));
    let expected = shot(&mut fresh);
    fresh.destroy();
    assert!(patched != before && patched == expected, "a patched node renders unlike its fresh build");
    println!("PXUI patch: renders as its fresh build");
}

/// A kit panel in a view, kept: the same pixels as rebuilding it every frame.
fn gpu_view_kept_pixels() {
    if !gpu::available() {
        return;
    }
    let shoot = |rebuild: bool| {
        let mut ui = Ui::create(Platform::Apple, None, crate::support::golden_text());
        for count in 0..3 {
            let revision = if rebuild { count as u64 } else { 1 };
            ui.frame(&frame_at(count, 240, 2), &mut |ui| {
                ui.view("kit", revision, Size::Px(WIDTH as f32), Size::Px(240.0), &mut |ui| new_panel(ui));
            });
        }
        let pixels = gpu::render(&mut ui, (WIDTH as u32, 240), 2, [228, 139, 161, 255]).0;
        ui.destroy();
        pixels
    };
    assert!(shoot(false) == shoot(true), "a kept view renders unlike its rebuild");
    println!("PXUI view: kept renders as rebuilt");
}

/// `view_offset` moves a kept view with its table entry: the pixels of the view rebuilt at that
/// offset.
fn gpu_view_offset_pixels() {
    if !gpu::available() {
        return;
    }
    let shoot = |offset: bool| {
        let mut ui = Ui::create(Platform::Apple, None, crate::support::golden_text());
        let mut view = None;
        for count in 0..2 {
            ui.frame(&frame_at(count, 240, 2), &mut |ui| {
                let at = if offset { (0.0, 0.0) } else { (24.0, 16.0) };
                let holder = ui.node("holder", Node { w: Size::Px(300.0), h: Size::Px(200.0), at: Some(at), ..Node::DEFAULT });
                let saved = ui.enter(&holder);
                view = Some(ui.view("kit", 1, Size::Px(300.0), Size::Px(200.0), &mut |ui| new_panel(ui)));
                ui.leave(saved);
            });
        }
        if offset {
            ui.view_offset(&view.unwrap(), 24.0, 16.0);
        }
        let pixels = gpu::render(&mut ui, (WIDTH as u32, 240), 2, [228, 139, 161, 255]).0;
        ui.destroy();
        pixels
    };
    assert!(shoot(true) == shoot(false), "an offset view renders unlike the view rebuilt at that offset");
    println!("PXUI view: offset renders as rebuilt there");
}

/// The painter writes what changed: nothing for an all-kept frame, one node's instances for a
/// patch (in each of its three frame copies).
fn gpu_retained_bytes() {
    if !gpu::available() {
        return;
    }
    let mut ui = Ui::create(Platform::Apple, None, crate::support::golden_text());
    let mut chip = None;
    let mut build = |ui: &mut Ui| {
        ui.view("kit", 1, Size::Px(WIDTH as f32), Size::Px(200.0), &mut |ui| new_panel(ui));
        ui.view("chip", 1, Size::Px(100.0), Size::Px(30.0), &mut |ui| {
            let look = Look { fill: pxui::geom::color::rgb(200, 40, 40), ..Look::NONE };
            chip = Some(ui.node("chip", Node { w: Size::Px(80.0), h: Size::Px(20.0), look, ..Node::DEFAULT }));
        });
    };
    let mut painter = gpu::painter(&mut ui);
    let size = (WIDTH as u32, 240);
    for count in 0..4 {
        ui.frame(&frame_at(count, 240, 2), &mut build);
        gpu::render_with(&mut painter, &mut ui, size, 2, [228, 139, 161, 255]);
    }
    ui.frame(&frame_at(4, 240, 2), &mut build);
    gpu::render_with(&mut painter, &mut ui, size, 2, [228, 139, 161, 255]);
    assert_eq!(painter.retained_bytes(), 0, "an all-kept frame wrote instances");
    assert!(ui.patch_fill(&chip.unwrap(), pxui::geom::color::rgb(40, 40, 200)), "patch refused");
    for frame in 0..4 {
        gpu::render_with(&mut painter, &mut ui, size, 2, [228, 139, 161, 255]);
        let expected = if frame < 3 { 64 } else { 0 };
        assert_eq!(painter.retained_bytes(), expected, "frame {frame} after a one-instance patch");
    }
    ui.destroy();
    println!("PXUI retained: 0 bytes kept, 64 bytes per copy for a one-instance patch");
}

pub const CASES: &[(&str, fn())] = &[
    ("gpu_patch", gpu_patch),
    ("gpu_view_kept_pixels", gpu_view_kept_pixels),
    ("gpu_view_offset_pixels", gpu_view_offset_pixels),
    ("gpu_retained_bytes", gpu_retained_bytes),
    ("gpu_parity_commands", gpu_parity_commands),
    ("gpu_parity_draw_calls", gpu_parity_draw_calls),
    ("gpu_parity_overlays", gpu_parity_overlays),
    ("gpu_parity_table", gpu_parity_table),
    ("gpu_parity_panel", gpu_parity_panel),
    ("gpu_parity_dusk", gpu_parity_dusk),
];
