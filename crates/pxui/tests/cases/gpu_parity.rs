//! Pixel goldens pin kit widgets, overlays and graph zone tokens at 1x and 2x, rendered offscreen
//! through the PXUI painter whatever the display's density. Token geometry and every channel are checked.
use crate::common;
use input::{Event, Platform};
use pxui::ui::*;
use rays::{canvas, frame, image, scene};
use rays_math::color;
use std::cell::RefCell;
use std::rc::Rc;

const WIDTH: i64 = 320;
const DENSITIES: [i64; 2] = [1, 2];

fn font_path() -> String {
    format!("{}/../../assets/fonts/DepartureMono-Regular.otf", env!("CARGO_MANIFEST_DIR"))
}

fn golden_path(name: &str) -> String {
    format!("{}/../../crates/pxui/fixtures/{}", env!("CARGO_MANIFEST_DIR"), name)
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

/// The second frame of a fresh UI with the pointer parked away from every widget, rendered
/// offscreen at `density`, and the painter's draw calls for it.
fn render(density: i64, height: i64, build: &mut dyn FnMut(&mut Ui)) -> (canvas::Canvas, u64) {
    let ui = Rc::new(RefCell::new(Ui::create(Platform::Apple, None, rays_shell::host::text(Some(&font_path())))));
    for count in 0..2 {
        ui.borrow_mut().frame(&frame_at(count, height, density), build);
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

/// Compare `canvas_` with the golden at `density`; a drift names the pixels that differ.
fn check(name: &str, golden: &str, density: i64, canvas_: canvas::Canvas) {
    let golden = golden.replace("_2x.png", &format!("_{density}x.png"));
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
    println!("PXUI {name} parity at {density}x: exact");
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
    if !common::gpu() {
        return;
    }
    for density in DENSITIES {
        check(name, golden, density, render(density, height, &mut |ui| build(ui)).0);
    }
}

fn gpu_parity_overlays() {
    parity("overlays", "kit_overlays_2x.png", 300, overlays);
}

fn gpu_parity_zones() {
    parity("zones", "kit_zones_2x.png", 390, zones);
}

fn gpu_parity_table() {
    parity("table", "kit_table_2x.png", 300, table);
}

fn gpu_parity_panel() {
    parity("kit panel", "kit_panel_2x.png", 300, new_panel);
}

/// Every fixture paints nonempty layers at 1x and 2x, without a device.
fn gpu_parity_commands() {
    let fixtures: [(&str, i64, fn(&mut Ui)); 4] = [("overlays", 300, overlays), ("zones", 390, zones), ("table", 300, table), ("panel", 300, new_panel)];
    for density in DENSITIES {
        for (name, height, build) in fixtures.iter() {
            let mut ui = Ui::create(Platform::Apple, None, rays_shell::host::text(Some(&font_path())));
            for count in 0..2 {
                ui.frame(&frame_at(count, *height, density), &mut |ui| build(ui));
            }
            let draw = ui.output().draw;
            assert!(!draw.is_empty() && draw.iter().any(|l| !l.instances.is_empty()), "{name}: no UI instances at {density}x");
            ui.destroy();
        }
    }
    println!("PXUI layers: four parity fixtures at 1x/2x");
}

/// One draw call per UI layer: the kit panel, and the table, a clipped scroll list whose clips
/// ride in the instances.
fn gpu_parity_draw_calls() {
    if !common::gpu() {
        return;
    }
    for (name, build) in [("kit panel", new_panel as fn(&mut Ui)), ("table", table)] {
        let (canvas_, calls) = render(2, 300, &mut |ui| build(ui));
        let _ = canvas::destroy(&canvas_);
        assert_eq!(calls, 1, "{name}: one layer drew in {calls} draw calls");
    }
}

pub const CASES: &[(&str, fn())] = &[
    ("gpu_parity_commands", gpu_parity_commands),
    ("gpu_parity_draw_calls", gpu_parity_draw_calls),
    ("gpu_parity_overlays", gpu_parity_overlays),
    ("gpu_parity_zones", gpu_parity_zones),
    ("gpu_parity_table", gpu_parity_table),
    ("gpu_parity_panel", gpu_parity_panel),
];
