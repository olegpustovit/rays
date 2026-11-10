//! Window-free orbit bench: isogrid.rays, Alt+drag in the viewport, per-frame update/render time.
//! cargo run --release -p rays_editor --example isogrid_orbit [FRAMES]
use rays_editor::{editor, workspace};
use std::time::Instant;

fn main() {
    #[cfg(windows)]
    ogpu_core::impl_::select(ogpu_vulkan::impl_::implementation());
    #[cfg(target_os = "macos")]
    ogpu_core::impl_::select(ogpu_metal::impl_::implementation());
    let frames: i64 = std::env::args().nth(1).and_then(|a| a.parse().ok()).unwrap_or(60);
    let source = std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../../sketches/isogrid.rays")).unwrap();
    let doc = workspace::load(None, Vec::new(), None, &source).unwrap_or_else(|d| panic!("{d:?}"));
    let (w, h) = (1280, 900);
    let canvas = rays::canvas::create(w, h).unwrap();
    let gpu = flow_native::workspace_gpu::create(None);
    let mut editor = editor::create(editor::Create { gpu_owner: Some(gpu.clone()), await_: Some(true), ..Default::default() }, &doc, None, editor::prepare_fn(), editor::draw_fn()).unwrap();
    let at = (300., 400.);
    let frame = |tick: i64, pointer: (f64, f64), events: Vec<input::Event>, drag: bool, delta: (f64, f64)| rays::frame::Frame {
        input: input::Frame { dt: 1. / 60., frame: tick, tick, size: (w, h), pointer,
            buttons: if drag { vec![input::MouseButton::LeftButton] } else { vec![] },
            keys: if drag { vec![input::Key::Alt] } else { vec![] }, events, ..input::at_time(tick as f64 / 60.) },
        pixel_scale: (1., 1.), fps: 60., mouse_delta: delta };
    let step = |editor: editor::Editor, f: &rays::frame::Frame| -> (editor::Editor, f64, f64) {
        let t0 = Instant::now();
        let e = editor::update(editor, f);
        let t1 = Instant::now();
        rays::canvas::render(None, &canvas, &editor::scene(&e, f)).unwrap();
        let e = editor::after_present(e, f);
        let t2 = Instant::now();
        (e, (t1 - t0).as_secs_f64() * 1e3, (t2 - t1).as_secs_f64() * 1e3)
    };
    let mut tick = 0;
    for _ in 0..30 { tick += 1; let f = frame(tick, at, vec![], false, (0., 0.)); editor = step(editor, &f).0; }
    let idle0 = mem_sys::alloc_bytes_total();
    for _ in 0..30 { tick += 1; let f = frame(tick, at, vec![], false, (0., 0.)); editor = step(editor, &f).0; }
    println!("idle alloc {} KB/frame", (mem_sys::alloc_bytes_total() - idle0) / 30 / 1024);
    // press with Alt held
    tick += 1;
    let f = frame(tick, at, vec![input::Event::KeyPressed(input::Key::Alt), input::Event::PointerPressed(input::MouseButton::LeftButton, at)], true, (0., 0.));
    editor = step(editor, &f).0;
    let cam0 = format!("{:?}", rays::camera::position(&editor::view_camera(&editor)));
    let (mut su, mut sr, mut worst) = (0., 0., 0f64);
    let mut p = at;
    let alloc0 = mem_sys::alloc_bytes_total();
    for _ in 0..frames {
        tick += 1;
        p.0 += 4.;
        let f = frame(tick, p, vec![input::Event::PointerMoved(p)], true, (4., 0.));
        let (e, u, r) = step(editor, &f);
        editor = e; su += u; sr += r; worst = worst.max(u + r);
        if u + r > 30. { eprintln!("slow frame {tick}: update {u:.1} ms render {r:.1} ms"); }
    }
    let alloc = (mem_sys::alloc_bytes_total() - alloc0) / frames as u64;
    let cam1 = format!("{:?}", rays::camera::position(&editor::view_camera(&editor)));
    let n = frames as f64;
    println!("camera {cam0} -> {cam1}");
    println!("orbit frames {frames}: update {:.2} ms  render {:.2} ms  total {:.2} ms ({:.1} fps)  worst {:.2} ms  alloc {} KB/frame",
        su / n, sr / n, (su + sr) / n, 1000. / ((su + sr) / n), worst, alloc / 1024);
    editor::close(&editor);
    flow_native::workspace_gpu::close(&gpu);
    rays::canvas::destroy(&canvas).unwrap();
}
