//! Against a running compositor (WSLg, weston, ...): a window, its token, a fatal-validation Vulkan swapchain,
//! three painted frames with one programmatic resize, cursor, IME rectangle and a clipboard round trip.
//! Skips with a note when WAYLAND_DISPLAY is unset.
mod common;

#[cfg(target_os = "linux")]
#[path = "../examples/support/gpu.rs"]
mod gpu;

#[cfg(target_os = "linux")]
fn window_wayland_present() {
    use input::Event;
    use pxui_render::Instance;
    use pxui_wayland::{CursorShape, Protocols, Window};
    if std::env::var_os("WAYLAND_DISPLAY").is_none() {
        println!("RAYS_TEST_SKIP: pxui_wayland: WAYLAND_DISPLAY is unset, no compositor to test against");
        return;
    }
    let mut window = Window::open("pxui_wayland test", 320, 200).unwrap_or_else(|e| panic!("{e}"));
    let protocols = window.protocols().unwrap();
    let names: [(i32, &str); 10] = [
        (Protocols::SEAT, "wl_seat"), (Protocols::POINTER, "pointer"), (Protocols::KEYBOARD, "keyboard"),
        (Protocols::DATA_DEVICE, "wl_data_device"), (Protocols::VIEWPORTER, "wp_viewporter"),
        (Protocols::FRACTIONAL_SCALE, "wp_fractional_scale_v1"), (Protocols::CURSOR_SHAPE, "wp_cursor_shape_v1"),
        (Protocols::DECORATION, "zxdg_decoration_v1"), (Protocols::SERVER_DECORATED, "server-side decorated"),
        (Protocols::TEXT_INPUT, "zwp_text_input_v3"),
    ];
    let pick = |offered: bool| names.iter().filter(|(bit, _)| protocols.has(*bit) == offered).map(|(_, name)| *name).collect::<Vec<_>>();
    println!("pxui_wayland: offered {:?}; missing {:?}; scale {}", pick(true), pick(false), window.scale());
    assert_eq!(window.size(), (320, 200));
    assert!(window.scale() >= 1.0);

    let mut gpu = gpu::Gpu::open(&mut window, || ogpu_vulkan::backend::create_with_validation((true, true)));
    let mut events = Vec::new();
    let mut presented = 0;
    let mut resized = false;
    for frame in 0..60 {
        assert!(window.pump(&mut events), "window closed during the test");
        resized |= events.iter().any(|e| *e == Event::Resized((400, 260)));
        events.clear();
        if presented == 1 && window.size() != (400, 260) {
            window.resize(400, 260).unwrap();
            assert_eq!(window.size(), (400, 260), "a resize takes effect at once");
        }
        let (w, h) = window.size();
        let rects = [
            Instance::rect([8.0, 8.0, w as f32 / 2.0, h as f32 - 8.0], [220, 40, 40, 255]),
            Instance::rect([w as f32 / 2.0 + 4.0, 8.0, w as f32 - 8.0, 40.0 + frame as f32], [40, 80, 220, 255]).radii([6.0; 4]),
        ];
        presented += usize::from(gpu.frame(&mut window, &rects));
        if presented == 3 {
            break;
        }
        window.wait(Some(window.time() + 0.05));
    }
    assert_eq!(presented, 3, "three frames presented");
    assert!(resized, "the next pump announced the programmatic resize");

    window.set_cursor(CursorShape::Text).unwrap();
    window.set_cursor(CursorShape::HorizontalResize).unwrap();
    window.set_text_input(Some(((10, 20, 120, 24), 30))).unwrap();
    window.set_text_input(None).unwrap();

    let text = "pxui_wayland clipboard \u{2713}";
    if protocols.has(Protocols::DATA_DEVICE) {
        window.clipboard_set_text(text).unwrap();
        // let the compositor take the selection and echo an offer before reading it back
        for _ in 0..3 {
            window.wait(Some(window.time() + 0.05));
            window.pump(&mut events);
        }
        events.clear();
        let source = window.request_paste().unwrap();
        let deadline = window.time() + 3.0;
        let pasted = loop {
            window.pump(&mut events);
            if let Some(Event::Paste(pasted)) = events.iter().find(|e| matches!(e, Event::Paste(_))) {
                break pasted.clone();
            }
            assert!(window.time() < deadline, "no Paste event within 3 s ({source:?})");
            window.wait(Some(window.time() + 0.05));
        };
        println!("pxui_wayland: paste via {source:?}");
        assert_eq!(pasted, text);
        assert_eq!(pxui_wayland::clipboard_get_text().unwrap(), text, "the synchronous read of the newest window");
    } else {
        assert!(window.clipboard_set_text(text).is_err());
    }

    // Relative mode: motion accumulates only while it is on (no pointer moves here, so none arrives).
    window.set_relative_mouse(true).unwrap();
    window.pump(&mut events);
    window.set_relative_mouse(false).unwrap();
    assert_eq!(window.take_motion(), (0.0, 0.0));
    assert_eq!(window.show_dialog(&[], None, pxui_wayland::DialogKind::OpenFile), Err(pxui_wayland::NO_FILE_DIALOG.to_owned()));

    gpu.close();
    window.destroy().unwrap();
    pxui_wayland::clipboard_set_text("local").unwrap();
    assert_eq!(pxui_wayland::clipboard_get_text().unwrap(), "local", "without a window the clipboard is process-local");
    assert!(window.set_cursor(CursorShape::Default).is_err(), "a destroyed window refuses calls");
    let mut events = Vec::new();
    assert!(!window.pump(&mut events));
}

fn main() {
    #[cfg(target_os = "linux")]
    common::run(&[("window_wayland_present", window_wayland_present)]);
    #[cfg(not(target_os = "linux"))]
    common::run(&[]);
}
