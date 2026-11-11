//! Backdrop blur through the Rays renderer: a PXUI view's blur record is fulfilled by splitting the scene
//! pass inside the frame's one command buffer, so the blur sees the scene content drawn before the UI
//! layer, and content drawn after it is untouched.
use crate::common;
use input::{Event, Platform};
use pxui::ui::*;
use rays::{canvas, frame, image, scene};
use rays_math::color;
use std::cell::{Cell, RefCell};
use std::rc::Rc;

const W: i64 = 320;
const H: i64 = 200;

fn frame_at(count: i64) -> frame::Frame {
    let mut input = input::at_time(count as f64 / 60.0);
    input.dt = 1.0 / 60.0;
    input.frame = count;
    input.tick = count;
    input.size = (W, H);
    input.pointer = (-100.0, -100.0);
    input.events = vec![Event::PointerMoved((-100.0, -100.0))];
    frame::Frame { input, pixel_scale: (1.0, 1.0), fps: 60.0, mouse_delta: (0.0, 0.0) }
}

/// RGBA pixels of a frame with a hard black/white edge at x = 160 behind a view at (80, 40, 160, 80)
/// asking for `blur`, and a red square drawn after the UI layer; the UI's draw calls and the pass
/// splits the render made.
fn render(canvas_: &canvas::Canvas, blur: f32) -> (Vec<u8>, u64, u64) {
    render_at(canvas_, blur, 80.0, 0.0)
}

/// `render` with the view at x `left` and its box's look rounded by `radius`. The renders share one
/// canvas: each canvas builds its own pipelines, which a CPU driver compiles for a second or more.
fn render_at(canvas_: &canvas::Canvas, blur: f32, left: f32, radius: f32) -> (Vec<u8>, u64, u64) {
    let path = format!("{}/../../pxui/assets/fonts/DepartureMono-Regular.otf", env!("CARGO_MANIFEST_DIR"));
    let ui = Rc::new(RefCell::new(Ui::create(Platform::Apple, None, rays_shell::host::text(Some(&path)))));
    let view = Cell::new(None);
    let mut build = |ui: &mut Ui| {
        ui.node("under", Node { w: Size::Px(4.0), h: Size::Px(1.0), look: Look { fill: pxui::geom::color::rgba(0, 0, 0, 1), ..Look::NONE }, ..Node::DEFAULT });
        let glass = ui.view("glass", 1, Size::Px(160.0), Size::Px(80.0), &mut |ui| {
            ui.node("pane", Node { w: Size::Px(160.0), h: Size::Px(80.0), look: Look { fill: pxui::geom::color::rgba(0, 0, 0, 1), ..Look::NONE }, ..Node::DEFAULT });
        });
        ui.node_mut(&glass).look.radius = [radius; 4];
        view.set(Some(glass));
    };
    for count in 0..2 {
        ui.borrow_mut().frame(&frame_at(count), &mut build);
        if count == 0 {
            let mut ui = ui.borrow_mut();
            ui.view_offset(&view.get().unwrap(), left, 40.0);
            ui.view_backdrop_blur(&view.get().unwrap(), blur);
        }
    }
    let mut nodes = vec![scene::clear(color::rgb(255, 255, 255)), scene::rect((0, 0), 160, H, Some(color::black()), None).unwrap()];
    nodes.extend(rays_shell::host::scene(&ui, None));
    nodes.push(scene::rect((10, 150), 40, 40, Some(color::rgb(255, 0, 0)), None).unwrap());
    let (calls, splits) = (rays_shell::host::draw_calls(), scene::ui_splits());
    canvas::render(Some(1), canvas_, &nodes).expect("Canvas.render");
    let result = (image::pixels(&canvas::to_image(canvas_).expect("to_image")).expect("pixels"), rays_shell::host::draw_calls() - calls, scene::ui_splits() - splits);
    ui.borrow_mut().destroy();
    result
}

fn window_host_backdrop_blur() {
    if !common::gpu() {
        return;
    }
    let canvas_ = canvas::create(W, H).expect("canvas");
    let (plain, plain_calls, plain_splits) = render(&canvas_, 0.0);
    assert_eq!((plain_calls, plain_splits), (1, 0), "a frame with no blur draws its layer once and splits no pass");
    let (blurred, calls, splits) = render(&canvas_, 8.0);
    assert_eq!(splits, 1, "one blur splits the pass once");
    assert_eq!(calls, 3, "the layer before the blur, the blurred image, the layer after it");
    let red = |pixels: &[u8], x: usize, y: usize| pixels[(y * W as usize + x) * 4] as i32;
    for (x, y, want) in [(40, 80, 0), (280, 80, 255), (30, 20, 0), (30, 130, 0), (300, 20, 255)] {
        assert_eq!(red(&blurred, x, y), want, "outside the view ({x}, {y}) is the untouched scene");
    }
    assert_eq!(&blurred[(170 * W as usize + 30) * 4..][..3], &[255, 0, 0], "content after the UI layer is intact");
    assert!(red(&blurred, 90, 80) <= 4 && red(&blurred, 230, 80) >= 251, "far from the edge the blur is the backdrop");
    let ramp: Vec<i32> = [148, 154, 160, 166, 172].iter().map(|x| red(&blurred, *x, 80)).collect();
    assert!(ramp.windows(2).all(|w| w[0] < w[1]) && ramp[0] > 0 && ramp[4] < 255 && (40..215).contains(&ramp[2]), "a soft ramp across the hard edge: {ramp:?}");
    assert!(red(&plain, 150, 80) == 0 && red(&plain, 170, 80) >= 253, "the same frame unblurred keeps its hard edge");
    // the view's right corners on the edge: a rounded record leaves its corners unblurred
    let (square, _, _) = render_at(&canvas_, 8.0, 5.0, 0.0);
    let (round, _, _) = render_at(&canvas_, 8.0, 5.0, 16.0);
    assert!(red(&square, 163, 41) < 250, "a square blur reaches its corner: {}", red(&square, 163, 41));
    assert!(red(&round, 163, 41) >= 253, "a rounded blur leaves its corner unblurred: {}", red(&round, 163, 41));
    assert_eq!(red(&round, 163, 80), red(&square, 163, 80), "inside the corners the blur is the same");
    let _ = canvas::destroy(&canvas_);
}

pub const CASES: &[(&str, fn())] = &[("window_host_backdrop_blur", window_host_backdrop_blur)];
