//! The gallery's goldens and its idle budget. Each page renders headless through the same page
//! function the app uses, in a view, at 1x and 2x under the Rays theme and `dusk.theme` (from the
//! file); the kit page under the default theme is pixel-equal to pxui's own `kit_panel` golden; and
//! the app, run hidden with no input, builds no frame after it settles.
#[path = "../../pxui_render/tests/png.rs"]
mod png;
#[path = "../../pxui/tests/cases/run.rs"]
mod run;

use input::{frame, Event, Platform};
use pxui::ui::*;
use pxui_gallery::gpu::{self, Gpu};
use pxui_gallery::*;

const DEPARTURE_MONO: &[u8] = include_bytes!("../../../assets/fonts/DepartureMono-Regular.otf");
const DUSK: &str = include_str!("../../pxui/themes/dusk.theme");

fn frame_at(count: i64, size: (i64, i64), density: i64) -> frame::Frame {
    let mut input = input::at_time(count as f64 / 60.0);
    (input.dt, input.frame, input.tick, input.size) = (1.0 / 60.0, count, count, size);
    input.pointer = (-100.0, -100.0);
    input.events = vec![Event::PointerMoved((-100.0, -100.0))];
    frame::Frame { input, pixel_scale: (density as f64, density as f64), fps: 60.0, mouse_delta: (0.0, 0.0) }
}

/// The second frame of a fresh UI over the golden face, in `theme`, drawn over `ground`.
fn shoot(gpu: &mut Gpu, theme: &Theme, size: (i64, i64), density: i64, ground: [u8; 4], build: &mut dyn FnMut(&mut Ui)) -> Vec<u8> {
    let mut ui = Ui::create(Platform::Apple, None, pxui::text::Text::new(&[DEPARTURE_MONO]));
    ui.set_theme(*theme);
    for count in 0..2 {
        ui.frame(&frame_at(count, size, density), build);
    }
    let mut painter = gpu.painter(&mut ui);
    let pixels = gpu.render(&mut painter, &mut ui, (size.0 as u32, size.1 as u32), density as u32, ground);
    drop(painter);
    ui.destroy();
    pixels
}

/// The largest channel difference from the golden at `path`, refreshed first with
/// `RAYS_UPDATE_FIXTURES=1` when `refresh` allows it.
fn compare(path: &str, size: (i64, i64), density: i64, pixels: &[u8], refresh: bool) -> i32 {
    let (w, h) = ((size.0 * density) as u32, (size.1 * density) as u32);
    if refresh && std::env::var("RAYS_UPDATE_FIXTURES").as_deref() == Ok("1") {
        std::fs::write(path, png::encode(w, h, pixels)).expect("refresh golden");
    }
    let (gw, gh, g) = png::decode(&std::fs::read(path).unwrap_or_else(|e| panic!("{path}: {e}")));
    assert!((gw, gh) == (w, h), "{path}: golden is {gw}x{gh}, render is {w}x{h}");
    g.chunks(4).zip(pixels.chunks(4)).map(|(p, q)| (0..4).map(|k| (p[k] as i32 - q[k] as i32).abs()).max().unwrap_or(0)).max().unwrap_or(0)
}

fn dusk() -> Theme {
    Theme::parse(DUSK, &Theme::default()).expect("dusk.theme")
}

fn ground(theme: &Theme) -> [u8; 4] {
    let c = theme.tokens.panel;
    [c.r as u8, c.g as u8, c.b as u8, 255]
}

/// Every page at 1x and 2x under both themes against `fixtures/gallery_<page>_<theme>_<d>x.png`.
fn gpu_gallery_pages() {
    let mut gpu = Gpu::open().expect("a GPU backend");
    let image = gpu.image(96, 64, &gpu::test_card(96, 64));
    for (name, theme) in [("rays", Theme::default()), ("dusk", dusk())] {
        for (i, page_name) in PAGES.iter().enumerate() {
            for density in [1, 2] {
                let mut s = Gallery::new();
                s.image = Some(image.clone());
                // the overlays page with its dialog and context menu open
                (s.dialog, s.menu_at) = (i == 3, (i == 3).then_some((360.0, 40.0)));
                let pixels = shoot(&mut gpu, &theme, (PAGE_W, PAGE_H), density, ground(&theme), &mut |ui| {
                    ui.view("page", 1, Size::Grow, Size::Grow, &mut |ui| page(ui, &mut s, i));
                });
                let path = format!("{}/fixtures/gallery_{}_{name}_{density}x.png", env!("CARGO_MANIFEST_DIR"), page_name.to_lowercase());
                let worst = compare(&path, (PAGE_W, PAGE_H), density, &pixels, true);
                // drivers round blends differently by a level (lavapipe): 2 per channel, as pxui's goldens
                assert!(worst <= 2, "gallery {page_name} under {name} at {density}x drifted: largest channel difference {worst}");
            }
        }
    }
    gpu.close();
    println!("PXUI gallery: 4 pages x 2 themes x 2 densities match their goldens");
}

/// The exit criterion: the kit page under the default theme, in the kit golden's window, is pxui's
/// `kit_panel` golden pixel for pixel (never refreshed from here).
fn gpu_gallery_kit_is_kit_panel() {
    let mut gpu = Gpu::open().expect("a GPU backend");
    for density in [1, 2] {
        let mut s = Gallery::new();
        let pixels = shoot(&mut gpu, &Theme::default(), (320, 300), density, [228, 139, 161, 255], &mut |ui| {
            ui.view("page", 1, Size::Grow, Size::Grow, &mut |ui| kit_page(ui, &mut s));
        });
        let path = format!("{}/../pxui/fixtures/kit_panel_{density}x.png", env!("CARGO_MANIFEST_DIR"));
        let worst = compare(&path, (320, 300), density, &pixels, false);
        let limit = if cfg!(target_os = "macos") { 0 } else { 2 };
        assert!(worst <= limit, "the kit page at {density}x differs from kit_panel_{density}x.png by {worst}");
        println!("PXUI gallery kit page = kit_panel_{density}x.png: max channel difference {worst}");
    }
    gpu.close();
}

/// The app run hidden with no input for 2.5 seconds builds no frame once it has settled (the
/// first half second): the frame gate holds while the theme poll and the readout tick along.
fn gpu_gallery_idle() {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_pxui_gallery"))
        .env("PXUI_GALLERY_HIDDEN", "1")
        .env("PXUI_GALLERY_EXIT_AFTER", "2.5")
        .env("PXUI_GALLERY_COUNT", "1")
        .output()
        .expect("run the gallery");
    let text = String::from_utf8_lossy(&out.stdout);
    let field = |name: &str| -> u64 {
        let at = text.find(&format!("{name} ")).unwrap_or_else(|| panic!("no {name} in {text:?} ({})", String::from_utf8_lossy(&out.stderr)));
        text[at + name.len() + 1..].split_whitespace().next().and_then(|v| v.parse().ok()).expect("a count")
    };
    assert!(out.status.success(), "the gallery failed: {}", String::from_utf8_lossy(&out.stderr));
    let (frames, settled) = (field("frames"), field("settled"));
    assert!(frames >= 1 && settled == 0, "idle gallery: {frames} frames, {settled} after it settled");
    println!("PXUI gallery idle: {frames} frames in the first half second, {settled} in the next two");
}

fn main() {
    run::run(&[
        ("gpu_gallery_kit_is_kit_panel", gpu_gallery_kit_is_kit_panel),
        ("gpu_gallery_pages", gpu_gallery_pages),
        ("gpu_gallery_idle", gpu_gallery_idle),
    ]);
}
