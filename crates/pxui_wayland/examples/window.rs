//! The PXUI host loop on Wayland: pump, draw a few rects, present, sleep until the next event.
//! `cargo run -p pxui_wayland --example window` under a Wayland session; a click flips the colours.
#[cfg(target_os = "linux")]
#[path = "support/gpu.rs"]
mod gpu;

#[cfg(target_os = "linux")]
fn main() {
    use pxui_render::Instance;
    let mut window = pxui_wayland::Window::open("pxui_wayland", 480, 320).unwrap_or_else(|e| panic!("{e}"));
    let mut gpu = gpu::Gpu::open(&mut window, ogpu_vulkan::backend::create);
    let mut events = Vec::with_capacity(256);
    let mut flipped = false;
    while window.pump(&mut events) {
        flipped ^= events.iter().any(|e| matches!(e, input::Event::PointerPressed(..)));
        events.clear();
        let (w, h) = window.size();
        let (a, b) = if flipped { ([40, 80, 220, 255], [220, 40, 40, 255]) } else { ([220, 40, 40, 255], [40, 80, 220, 255]) };
        let rects = [
            Instance::rect([16.0, 16.0, w as f32 / 2.0 - 8.0, h as f32 - 16.0], a).radii([8.0; 4]),
            Instance::rect([w as f32 / 2.0 + 8.0, 16.0, w as f32 - 16.0, h as f32 - 16.0], b).radii([8.0; 4]).border(2.0, [240, 240, 240, 255]),
        ];
        gpu.frame(&mut window, &rects);
        window.wait(None);
    }
    gpu.close();
}

#[cfg(not(target_os = "linux"))]
fn main() {}
