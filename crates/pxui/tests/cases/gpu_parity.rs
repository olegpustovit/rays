//! Native pixel goldens pin kit widgets, overlays and graph zone tokens at the selected display density. Token geometry and every channel are checked.
use crate::common;
use input::{Event, Platform};
use pxui::theme;
use pxui::ui::*;
use rays::{canvas, frame, image, scene};
use rays_math::color;
use std::rc::Rc;

const WIDTH: i64 = 320;
fn scale() -> i64 {
    static SCALE: std::sync::OnceLock<i64> = std::sync::OnceLock::new();
    *SCALE.get_or_init(|| {
        let window = sdl3::Window::create("PXUI density", 1, 1, &[sdl3::WindowFlag::Hidden, sdl3::WindowFlag::HighPixelDensity]).expect("density probe");
        let density = f64::from(window.pixel_density().expect("display pixel density"));
        println!("PXUI native display density: {density}x");
        assert!(density >= 1.0 && density.fract() == 0.0, "native Canvas requires whole pixel density: {density}");
        density as i64
    })
}

fn font_path() -> String {
    format!("{}/../../assets/fonts/DepartureMono-Regular.otf", env!("CARGO_MANIFEST_DIR"))
}

fn golden_path(name: &str) -> String {
    format!("{}/../../crates/pxui/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name)
}

fn frame_at(count: i64, height: i64) -> frame::Frame {
    let mut input = input::at_time(count as f64 / 60.0);
    input.dt = 1.0 / 60.0;
    input.frame = count;
    input.tick = count;
    input.size = (WIDTH, height);
    input.pointer = (-100.0, -100.0);
    input.events = vec![Event::PointerMoved((-100.0, -100.0))];
    frame::Frame { input, pixel_scale: (scale() as f64, scale() as f64), fps: 60.0, mouse_delta: (0.0, 0.0) }
}

/// The second frame of a fresh UI with the pointer parked away from every widget, rendered
/// at the selected display density.
fn render(height: i64, build: &mut dyn FnMut(&mut Ui)) -> canvas::Canvas {
    let mut ui = Ui::create(Platform::Apple, None, Some(font_path()));
    for count in 0..2 {
        ui.frame(&frame_at(count, height), build);
    }
    let canvas_ = canvas::create(WIDTH * scale(), height * scale()).expect("canvas");
    let mut nodes = vec![scene::clear(color::rgb(228, 139, 161))];
    nodes.extend(ui.scene(None));
    canvas::render(Some(scale()), &canvas_, &nodes).expect("Canvas.render");
    ui.destroy();
    canvas_
}

/// Compare `canvas_` with the golden; a drift names the pixels that differ.
fn check(name: &str, golden: &str, canvas_: canvas::Canvas) {
    let golden = golden.replace("_2x.png", &format!("_{}x.png", scale()));
    if std::env::var("RAYS_UPDATE_FIXTURES").as_deref() == Ok("1") {
        canvas::save_png(&canvas_, &golden_path(&golden)).expect("refresh parity golden");
    }
    let actual = canvas::to_image(&canvas_).expect("to_image");
    let golden_image = image::load(&golden_path(&golden)).expect("golden");
    let (gd, ad) = (image::dimensions(&golden_image), image::dimensions(&actual));
    assert!(gd == ad, "PXUI {name} parity: golden is {}x{}, render is {}x{}", gd.0, gd.1, ad.0, ad.1);
    let g = image::pixels(&golden_image).expect("pixels");
    let a = image::pixels(&actual).expect("pixels");
    let mut different = 0usize;
    let mut worst = 0i32;
    let mut first = None;
    for (index, (p, q)) in g.chunks(4).zip(a.chunks(4)).enumerate() {
        if p != q {
            different += 1;
            for k in 0..4 {
                worst = worst.max((p[k] as i32 - q[k] as i32).abs());
            }
            if first.is_none() {
                first = Some(((index as i64) % gd.0, (index as i64) / gd.0));
            }
        }
    }
    assert!(
        different == 0,
        "PXUI {name} drifted from its golden: {different} of {} pixels differ (largest channel difference {worst}), first at {:?}",
        g.len() / 4,
        first
    );
    println!("PXUI {name} parity: exact");
    let _ = canvas::destroy(&canvas_);
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

/// The graph pane zone tokens: for, fold, sum, a hollow dashed fn zone and a let scope, with the
/// socket colours of lists and functions.
fn zones(ui: &mut Ui) {
    let t = ui.theme();
    let b = ui.box_("zones", BoxOpts::default().w(Size::Px(320.0)).h(Size::Px(390.0)).at((0.0, 0.0)));
    ui.draw(&b, move |paint, _| {
        let list = [
            theme::zone_for(&t),
            theme::zone_fold(&t),
            theme::zone_sum(&t),
            theme::zone_fn(&t),
            theme::zone_let(&t),
            theme::zone_branch(&t, false),
            theme::zone_branch(&t, true),
        ];
        for (i, zone) in list.iter().enumerate() {
            let x = 12.0 + (i % 2) as f64 * 154.0;
            let y = 12.0 + (i / 2) as f64 * 92.0;
            paint.rect(x, y, 142.0, 80.0, Some(zone.fill), None);
            if zone.dashed {
                paint.dashed_rect(x, y, 142.0, 80.0, zone.edge);
            } else {
                paint.stroke(x + 0.5, y + 0.5, 141.0, 79.0, zone.edge);
            }
        }
        let ports = theme::ports(&t);
        for (i, c) in [ports.text, ports.fn_, ports.record].iter().enumerate() {
            paint.rect(170.0 + i as f64 * 20.0, 200.0, 10.0, 10.0, Some(*c), None);
        }
    });
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

fn gpu_parity_overlays() {
    if !common::gpu() {
        return;
    }
    check("overlays", "kit_overlays_2x.png", render(300, &mut overlays));
}

fn gpu_parity_zones() {
    if !common::gpu() {
        return;
    }
    check("zones", "kit_zones_2x.png", render(390, &mut zones));
}

fn gpu_parity_table() {
    if !common::gpu() {
        return;
    }
    check("table", "kit_table_2x.png", render(300, &mut table));
}

fn gpu_parity_panel() {
    if !common::gpu() {
        return;
    }
    check("kit panel", "kit_panel_2x.png", render(300, &mut new_panel));
}

/// Inspect actual staged painter packets at 1x/2x without a device; every fixture must produce nonempty, valid UI layers.
fn gpu_parity_commands() {
    let fixtures: [(&str, i64, fn(&mut Ui)); 4] = [("overlays", 300, overlays), ("zones", 390, zones), ("table", 300, table), ("panel", 300, new_panel)];
    for density in [1.0, 2.0] {
        for (name, height, build) in fixtures.iter() {
            let mut ui = Ui::create(Platform::Apple, None, Some(font_path()));
            for count in 0..2 {
                let mut f = frame_at(count, *height);
                f.pixel_scale = (density, density);
                ui.frame(&f, &mut |ui| build(ui));
            }
            let staged = rays::scene::layer::stage_native(WIDTH, *height, &ui.scene(None)).unwrap_or_else(|e| panic!("{name}: {e}"));
            let mut packets = 0;
            for layer in &staged.layers {
                if let rays::scene::layer::NativeLayer::UiLayer(batch, _) = layer {
                    assert!(batch.count() > 0, "{name}: empty UI batch");
                    for b in batch.batches() {
                        assert!(b.first >= 0 && b.count > 0, "{name}: malformed batch");
                    }
                    packets += 1;
                }
            }
            assert!(packets > 0, "{name}: no UI packets at {density}x");
            ui.destroy();
        }
    }
    println!("PXUI packets: four parity fixtures in logical screen points at 1x/2x");
}

pub const CASES: &[(&str, fn())] = &[
    ("gpu_parity_commands", gpu_parity_commands),
    ("gpu_parity_overlays", gpu_parity_overlays),
    ("gpu_parity_zones", gpu_parity_zones),
    ("gpu_parity_table", gpu_parity_table),
    ("gpu_parity_panel", gpu_parity_panel),
];
