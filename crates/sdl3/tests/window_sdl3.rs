//! Real native layer ownership runs on the process initial thread, outside libtest workers.
#[path = "../../renderer/tests/common/window.rs"]
mod window;

fn release_keepalive() {}

fn main() {
    sdl3::init::configure_native_process().expect("SDL window tests require the process initial thread");
    window::run(&[
        #[cfg(target_os = "macos")]
        ("cocoa_layer_ownership_and_trackpad_phases", cocoa_layer_ownership_and_trackpad_phases),
        #[cfg(target_os = "windows")]
        ("windows_sys_layer_token_dies_before_parent_hwnd", windows_sys_layer_token_dies_before_parent_hwnd),
    ]);
}

#[cfg(target_os = "macos")]
fn cocoa_layer_ownership_and_trackpad_phases() {
    // Trackpad phases need a real Cocoa window; the dummy driver cannot cover this case.
    if std::env::var("SDL_VIDEODRIVER").ok().as_deref() == Some("dummy") {
        eprintln!("RAYS_TEST_SKIP: trackpad phases require a real Cocoa window");
        assert!(std::env::var("RAYS_ALLOW_SKIP").as_deref() == Ok("1"), "RAYS_ALLOW_SKIP=1 explicitly permits skipping Cocoa coverage");
        return;
    }
    // Native startup reports whether this process can reach a window server.
    if let Err(e) = sdl3::init::init(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]) {
        assert!(e.message().contains("not available") || e.message().contains("No available video device"), "{e:?}");
        eprintln!("RAYS_TEST_SKIP: skipped: no window server ({})", e.message());
        assert!(std::env::var("RAYS_ALLOW_SKIP").as_deref() == Ok("1"), "missing required GPU/display/audio support; RAYS_ALLOW_SKIP=1 explicitly permits skipping");
        return;
    }
    let mut window = sdl3::Window::create(
        "SDL3 Rust Metal test", 64, 48,
        &[sdl3::WindowFlag::Hidden, sdl3::WindowFlag::HighPixelDensity, sdl3::WindowFlag::Metal],
    ).unwrap();
    let (logical_width, logical_height) = window.size().unwrap();
    let (pixel_width, pixel_height) = window.size_in_pixels().unwrap();
    let density = window.pixel_density().unwrap();
    assert!(density > 0.0 && density.is_finite());
    assert!((pixel_width as f32 / logical_width as f32 - density).abs() < 0.01);
    assert!((pixel_height as f32 / logical_height as f32 - density).abs() < 0.01);
    window.set_size(80, 60).unwrap();
    window.sync().unwrap();
    assert_eq!(window.size().unwrap(), (80, 60));
    let (resized_width, resized_height) = window.size_in_pixels().unwrap();
    assert!((resized_width as f32 / 80.0 - density).abs() < 0.01);
    assert!((resized_height as f32 / 60.0 - density).abs() < 0.01);
    window.set_resizable(false).unwrap();
    window.set_resizable(true).unwrap();
    window.center().unwrap();
    window.set_background(0.95, 0.95, 0.93).unwrap();

    let _ = sdl3::event::poll_coalesced().unwrap();
    for (points, phase) in [(0, 1), (-7, 2), (0, 4)] {
        sdl3_sys::test_util::post_scroll(points, phase);
    }
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    let mut phases = Vec::new();
    while phases.len() < 3 && std::time::Instant::now() < deadline {
        phases.extend(sdl3::event::poll_coalesced().unwrap().into_iter().filter_map(|event| {
            if let sdl3::Event::Scroll { phase, y, .. } = event { Some((phase, y)) } else { None }
        }));
        if phases.len() < 3 { std::thread::sleep(std::time::Duration::from_millis(10)); }
    }
    assert!(matches!(phases.as_slice(), [
        (sdl3::ScrollPhase::Began, _),
        (sdl3::ScrollPhase::Changed, y),
        (sdl3::ScrollPhase::Ended, _),
    ] if y.abs() == 7.0));

    let mut view = sdl3::MetalView::create(&window).unwrap();
    let token = view.layer().unwrap();
    let second = view.layer().unwrap();
    assert!(token.alive());
    assert_eq!(token.owner_id(), second.owner_id());
    assert_eq!(token.generation(), second.generation());
    assert_eq!(window.destroy().unwrap_err().kind(), sdl3::ErrorKind::ParentHasDependents);
    view.destroy().unwrap();
    view.destroy().unwrap();
    assert!(!token.alive());
    match view.layer() {
        Err(error) => assert_eq!(error.kind(), sdl3::ErrorKind::Destroyed),
        Ok(_) => panic!("destroyed Metal view remained usable"),
    }
    window.destroy().unwrap();
    let mut native_window = sdl3_sys::Window::create(
        "SDL3 sys parent-retention test", 32, 24,
        &[sdl3_sys::WindowFlag::Hidden, sdl3_sys::WindowFlag::Metal],
    ).unwrap_or_else(|error| panic!("{}", error.message));
    let mut native_view = sdl3_sys::MetalView::create(&native_window)
        .unwrap_or_else(|error| panic!("{}", error.message));
    let token = native_view.layer_token(99).unwrap_or_else(|error| panic!("{}", error.message));
    let (owner, generation) = (token.owner_id(), token.generation());
    assert!(token.borrow(owner, generation).is_some());
    assert_eq!(
        native_window.destroy().unwrap_err().kind,
        sdl3_sys::ErrorKind::ParentHasDependents,
    );
    drop(native_window);
    native_view.destroy();
    assert!(token.borrow(owner, generation).is_none());
    sdl3::init::quit_subsystems(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]).unwrap();
}


#[cfg(target_os = "windows")]
fn windows_sys_layer_token_dies_before_parent_hwnd() {
    sdl3::hint::set(sdl3::hint::VIDEO_DRIVER, "windows").unwrap();
    sdl3::init::init(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]).unwrap();
    let mut window = sdl3_sys::Window::create(
        "SDL3 HWND token lifetime test", 32, 24,
        &[sdl3_sys::WindowFlag::Hidden, sdl3_sys::WindowFlag::Metal],
    ).unwrap_or_else(|error| panic!("{}", error.message));
    let mut view = sdl3_sys::MetalView::create(&window)
        .unwrap_or_else(|error| panic!("{}", error.message));
    let token = view.layer_token(101).unwrap_or_else(|error| panic!("{}", error.message));
    let second_token = view.layer_token(102).unwrap_or_else(|error| panic!("{}", error.message));
    let (owner, generation) = (token.owner_id(), token.generation());
    let second_generation = second_token.generation();
    assert!(token.borrow(owner, generation).is_some());
    assert!(second_token.borrow(owner, second_generation).is_some());
    assert_eq!(window.destroy().unwrap_err().kind, sdl3_sys::ErrorKind::ParentHasDependents);
    drop(window);
    assert!(token.borrow(owner, generation).is_some());
    view.destroy();
    assert!(token.borrow(owner, generation).is_none());
    assert!(second_token.borrow(owner, second_generation).is_none());
    drop(view);
    sdl3::init::quit_subsystems(&[sdl3::init::Subsystem::Video, sdl3::init::Subsystem::Events]).unwrap();
}
