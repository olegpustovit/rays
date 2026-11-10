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
    let mut different = 0usize;
    let mut worst = 0i32;
    let mut first = None;
    for (index, (p, q)) in g.chunks(4).zip(pixels.chunks(4)).enumerate() {
        if p != q {
            different += 1;
            for k in 0..4 {
                worst = worst.max((p[k] as i32 - q[k] as i32).abs());
            }
            first.get_or_insert((index as u32 % w, index as u32 / w));
        }
    }
    assert!(
        different == 0,
        "PXUI {name} drifted from its golden: {different} of {} pixels differ (largest channel difference {worst}), first at {:?}",
        g.len() / 4,
        first
    );
    println!("PXUI {name} parity at {density}x: exact");
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

pub const CASES: &[(&str, fn())] = &[
    ("gpu_parity_commands", gpu_parity_commands),
    ("gpu_parity_draw_calls", gpu_parity_draw_calls),
    ("gpu_parity_overlays", gpu_parity_overlays),
    ("gpu_parity_table", gpu_parity_table),
    ("gpu_parity_panel", gpu_parity_panel),
];
