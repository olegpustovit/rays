use crate::{ErrorKind, Version};

fn get<T>(result: crate::Result<T>) -> T {
    result.unwrap_or_else(|error| panic!("{error}"))
}

thread_local! {
    static MAIN: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// The calling test thread becomes the thread the library treats as the SDL main thread.
fn claim_main() {
    MAIN.with(|main| main.set(true));
}

pub(crate) fn is_main() -> bool {
    MAIN.with(std::cell::Cell::get)
}

/// Claims the main thread for a test that needs the dummy video driver, and fails if the gate did not select it.
fn claim_dummy_main() {
    claim_main();
    assert_eq!(
        std::env::var("SDL_VIDEODRIVER").ok().as_deref(),
        Some("dummy"),
        "window tests need SDL_VIDEODRIVER=dummy",
    );
}

#[test]
fn version_compatibility_rejects_older_and_prerelease_versions() {
    let stable = Version { major: 3, minor: 4, patch: 18 };
    assert!(crate::validate_version_for("SDL3", stable, true, stable).is_ok());
    assert!(crate::validate_version_for(
        "SDL3", stable, true, Version { major: 3, minor: 4, patch: 20 },
    ).is_ok());
    assert_eq!(
        crate::validate_version_for(
            "SDL3",
            stable,
            false,
            Version { major: 3, minor: 4, patch: 17 },
        )
        .unwrap_err()
        .kind,
        ErrorKind::IncompatibleVersion,
    );
    let prerelease = Version { major: 3, minor: 3, patch: 1 };
    assert_eq!(
        crate::validate_version_for("SDL3", stable, false, Version { major: 3, minor: 6, patch: 18 })
            .unwrap_err().kind,
        ErrorKind::IncompatibleVersion,
    );
    assert_eq!(
        crate::validate_version_for("SDL3", stable, false, Version { major: 4, minor: 4, patch: 18 })
            .unwrap_err().kind,
        ErrorKind::IncompatibleVersion,
    );
    assert_eq!(
        crate::validate_version_for("SDL3_ttf", prerelease, true, prerelease)
            .unwrap_err()
            .kind,
        ErrorKind::IncompatibleVersion,
    );
}

#[test]
fn invalid_arguments_are_rejected_before_native_thread_checks() {
    let title = match crate::Window::create("bad\0title", 8, 8, &[]) {
        Err(error) => error,
        Ok(_) => panic!("NUL window title was accepted"),
    };
    assert_eq!(title.kind, ErrorKind::InvalidArgument);
    let dimensions = match crate::Window::create("invalid size", 0, 8, &[]) {
        Err(error) => error,
        Ok(_) => panic!("zero-width window was accepted"),
    };
    assert_eq!(dimensions.kind, ErrorKind::InvalidArgument);
    assert_eq!(crate::clipboard::set_text("bad\0text").unwrap_err().kind, ErrorKind::InvalidArgument);
}

#[test]
fn sdl_calls_are_refused_from_worker_threads() {
    let result = std::thread::spawn(|| crate::event::wait(0))
        .join()
        .expect("SDL worker check panicked");
    assert_eq!(result.unwrap_err().kind, ErrorKind::WrongDomain);
}

#[test]
fn dummy_video_window_lifecycle_and_typed_errors() {
    claim_dummy_main();
    get(crate::init::init(&[crate::init::Subsystem::Video, crate::init::Subsystem::Events]));
    let mut window = get(crate::Window::create(
        "SDL3 Rust ownership test", 96, 64, &[crate::WindowFlag::Hidden],
    ));
    assert_eq!(get(window.size()), (96, 64));
    assert_eq!(get(window.title()), "SDL3 Rust ownership test");
    assert_eq!(get(window.size_in_pixels()), (96, 64));
    assert_eq!(get(window.pixel_density()), 1.0);
    get(window.set_size(112, 72));
    get(window.sync());
    assert_eq!(get(window.size()), (112, 72));
    assert_eq!(get(window.size_in_pixels()), (112, 72));
    assert!(get(window.state()).hidden);
    let resizable = window.set_resizable(false);
    assert!(resizable.is_ok() || matches!(resizable, Err(error) if error.kind == ErrorKind::SdlError));
    let resizable = window.set_resizable(true);
    assert!(resizable.is_ok() || matches!(resizable, Err(error) if error.kind == ErrorKind::SdlError));
    get(window.start_text_input());
    get(window.set_text_input_area(crate::Rect { x: 3, y: 4, width: 40, height: 16 }, 7));
    get(window.stop_text_input());
    get(crate::clipboard::set_text("Rays ž clipboard"));
    assert_eq!(get(crate::clipboard::get_text()), "Rays ž clipboard");
    get(crate::hint::control_click_is_right_click(true));
    get(crate::hint::control_click_is_right_click(false));
    get(window.set_background(0.1, 0.2, 0.3));
    match window.set_relative_mouse(true) {
        Ok(()) => get(window.set_relative_mouse(false)),
        Err(error) if matches!(error.kind, ErrorKind::SdlError | ErrorKind::Unsupported)
            && !error.message.is_empty() => {}
        Err(error) => panic!("untyped relative-mouse error: {error}"),
    }
    for shape in [
        crate::CursorShape::Default, crate::CursorShape::Text,
        crate::CursorShape::EwResize, crate::CursorShape::NsResize,
    ] {
        match crate::Cursor::create(shape) {
            Err(error) => assert_eq!(error.kind, ErrorKind::Unsupported),
            Ok(mut cursor) => {
                get(cursor.set());
                get(cursor.destroy());
                get(cursor.destroy());
                assert_eq!(cursor.set().unwrap_err().kind, ErrorKind::Destroyed);
            }
        }
    }
    get(window.show());
    get(window.sync());
    assert!(!get(window.state()).hidden);
    get(window.hide());
    get(window.sync());
    assert!(get(window.state()).hidden);
    assert_eq!(window.set_size(0, 72).unwrap_err().kind, ErrorKind::InvalidArgument);
    assert_eq!(
        window.set_text_input_area(crate::Rect { x: 0, y: 0, width: 4, height: 4 }, -1)
            .unwrap_err().kind,
        ErrorKind::InvalidArgument,
    );
    assert_eq!(
        window.dialog_show(crate::DialogKind::OpenFile,
            &[crate::DialogFilter { name: "a\0b".into(), pattern: "*".into() }], None)
            .unwrap_err().kind,
        ErrorKind::InvalidArgument,
    );
    assert_eq!(
        window.dialog_show(crate::DialogKind::OpenFile,
            &[crate::DialogFilter { name: "all".into(), pattern: "".into() }], None)
            .unwrap_err().kind,
        ErrorKind::InvalidArgument,
    );
    assert_eq!(
        window.dialog_show(crate::DialogKind::SaveFile, &[], Some("/tmp\0location"))
            .unwrap_err().kind,
        ErrorKind::InvalidArgument,
    );
    let filters: Vec<_> = (0..33).map(|_| crate::DialogFilter { name: "x".into(), pattern: "*".into() }).collect();
    assert_eq!(
        window.dialog_show(crate::DialogKind::OpenFiles, &filters, None).unwrap_err().kind,
        ErrorKind::InvalidArgument,
    );
    assert!(matches!(crate::MetalView::create(&window), Err(error) if error.kind == ErrorKind::SdlError));
    get(window.destroy());
    get(window.destroy());
    assert_eq!(window.size().unwrap_err().kind, ErrorKind::Destroyed);
    get(crate::init::quit_subsystems(&[crate::init::Subsystem::Video, crate::init::Subsystem::Events]));
}

#[test]
fn typed_event_trace_and_async_dialog_outcomes_match_native_bridge() {
    claim_dummy_main();
    get(crate::init::init(&[crate::init::Subsystem::Events]));
    let _ = get(crate::event::poll_coalesced());
    assert!(sdl3_sys::test_util::push_event_trace());
    let events = get(crate::event::poll_coalesced());
    use crate::Key as K;
    use crate::KeyModifier as M;
    use crate::WindowChange as W;
    let expected = vec![
        crate::Event::Key { key: K::Char('a'), modifiers: vec![M::Shift, M::CapsLock], down: true, repeat: false },
        crate::Event::Key { key: K::Char('a'), modifiers: vec![], down: false, repeat: false },
        crate::Event::Key { key: K::ArrowUp, modifiers: vec![], down: true, repeat: true },
        crate::Event::Key { key: K::Escape, modifiers: vec![], down: true, repeat: false },
        crate::Event::Key { key: K::F5, modifiers: vec![], down: true, repeat: false },
        crate::Event::Key { key: K::Shift, modifiers: vec![M::Shift], down: true, repeat: false },
        crate::Event::Key { key: K::Meta, modifiers: vec![M::Meta], down: true, repeat: false },
        crate::Event::Key { key: K::Delete, modifiers: vec![], down: true, repeat: false },
        crate::Event::Key { key: K::Enter, modifiers: vec![], down: true, repeat: false },
        crate::Event::Key { key: K::Space, modifiers: vec![], down: true, repeat: false },
        crate::Event::Key { key: K::Char('z'), modifiers: vec![], down: true, repeat: false },
        crate::Event::Key { key: K::Char('a'), modifiers: vec![M::Meta], down: true, repeat: false },
        crate::Event::Key { key: K::Char('7'), modifiers: vec![], down: true, repeat: false },
        crate::Event::Key { key: K::Unknown(0x444), modifiers: vec![], down: true, repeat: false },
        crate::Event::TextInput("žirafa".into()),
        crate::Event::TextEditing { text: "č".into(), start: 1, length: 2 },
        crate::Event::MouseMotion { x: 11.5, y: 22.5, dx: 7.0, dy: 3.0 },
        crate::Event::MouseButton { button: crate::MouseButton::Left, down: true, x: 30.0, y: 40.0 },
        crate::Event::MouseButton { button: crate::MouseButton::Middle, down: false, x: 31.0, y: 41.0 },
        crate::Event::MouseButton { button: crate::MouseButton::Right, down: true, x: 32.0, y: 42.0 },
        crate::Event::MouseButton { button: crate::MouseButton::X1, down: true, x: 33.0, y: 43.0 },
        crate::Event::MouseButton { button: crate::MouseButton::X2, down: false, x: 34.0, y: 44.0 },
        crate::Event::MouseWheel {
            x: 1.0, y: -2.0, direction: crate::WheelDirection::Flipped,
            mouse_x: 50.0, mouse_y: 60.0, integer_x: 1, integer_y: -2,
        },
        crate::Event::Window(W::Shown), crate::Event::Window(W::Hidden),
        crate::Event::Window(W::Minimized), crate::Event::Window(W::Restored),
        crate::Event::Window(W::Occluded), crate::Event::Window(W::FocusGained),
        crate::Event::Window(W::FocusLost), crate::Event::Window(W::Resized(640, 480)),
        crate::Event::Window(W::PixelSizeChanged(1280, 960)),
        crate::Event::Window(W::CloseRequested),
        crate::Event::Pinch { phase: crate::PinchPhase::Began, scale: 1.0 },
        crate::Event::Pinch { phase: crate::PinchPhase::Updated, scale: 1.25 },
        crate::Event::Pinch { phase: crate::PinchPhase::Ended, scale: 1.0 },
        crate::Event::Drop { change: crate::DropChange::Begin, x: 0.0, y: 0.0 },
        crate::Event::Drop { change: crate::DropChange::Position, x: 5.5, y: 6.5 },
        crate::Event::Drop { change: crate::DropChange::File("/tmp/žaba.png".into()), x: 30.0, y: 40.0 },
        crate::Event::Drop { change: crate::DropChange::Complete, x: 0.0, y: 0.0 },
        crate::Event::Quit,
    ];
    assert_eq!(events, expected);
    sdl3_sys::test_util::mutate_event_sources();
    assert!(events.iter().any(|event| matches!(event, crate::Event::TextInput(text) if text == "žirafa")));
    assert!(events.iter().any(|event| matches!(event, crate::Event::TextEditing { text, .. } if text == "č")));
    assert!(events.iter().any(|event| matches!(event,
        crate::Event::Drop { change: crate::DropChange::File(path), .. } if path == "/tmp/žaba.png")));

    let outcomes = || -> Vec<(i32, crate::DialogOutcome)> {
        get(crate::event::poll_coalesced()).into_iter().filter_map(|event| match event {
            crate::Event::Dialog { id, outcome } => Some((id, outcome)),
            _ => None,
        }).collect()
    };
    let first = sdl3_sys::test_util::dialog_reserve();
    assert_ne!(first, 0);
    assert!(sdl3_sys::test_util::dialog_finish(first, Some(&["/tmp/žaba.png", "/tmp/b c.png"])));
    assert_eq!(outcomes(), vec![(first, crate::DialogOutcome::Chosen(vec!["/tmp/žaba.png".into(), "/tmp/b c.png".into()]))]);
    assert!(outcomes().is_empty());
    let cancelled = sdl3_sys::test_util::dialog_reserve();
    assert!(sdl3_sys::test_util::dialog_finish(cancelled, Some(&[])));
    assert_eq!(outcomes(), vec![(cancelled, crate::DialogOutcome::Cancelled)]);
    let failed = sdl3_sys::test_util::dialog_reserve();
    assert!(sdl3_sys::test_util::dialog_fail(failed));
    assert_eq!(outcomes(), vec![(failed, crate::DialogOutcome::Failed("boom".into()))]);
    let huge = sdl3_sys::test_util::dialog_reserve();
    assert!(sdl3_sys::test_util::dialog_finish_huge(huge));
    assert!(matches!(outcomes().as_slice(), [(id, crate::DialogOutcome::Failed(message))] if *id == huge && !message.is_empty()));
    let (older, newer) = (sdl3_sys::test_util::dialog_reserve(), sdl3_sys::test_util::dialog_reserve());
    assert!(sdl3_sys::test_util::dialog_finish(newer, Some(&["/tmp/newer"])));
    assert!(sdl3_sys::test_util::dialog_finish(older, Some(&["/tmp/older"])));
    assert_eq!(outcomes(), vec![
        (older, crate::DialogOutcome::Chosen(vec!["/tmp/older".into()])),
        (newer, crate::DialogOutcome::Chosen(vec!["/tmp/newer".into()])),
    ]);
    let held: Vec<_> = (0..8).map(|_| sdl3_sys::test_util::dialog_reserve()).collect();
    assert!(held.iter().all(|id| *id != 0));
    assert_eq!(sdl3_sys::test_util::dialog_reserve(), 0);
    held.into_iter().for_each(sdl3_sys::test_util::dialog_abandon);
    let recovered = sdl3_sys::test_util::dialog_reserve();
    assert_ne!(recovered, 0);
    sdl3_sys::test_util::dialog_abandon(recovered);
    assert!(sdl3_sys::test_util::push_resize_burst());
    assert_eq!(get(crate::event::poll_coalesced()), vec![
        crate::Event::Window(W::Resized(800, 600)),
        crate::Event::Window(W::PixelSizeChanged(1280, 960)),
    ]);
    let redraws = std::rc::Rc::new(std::cell::Cell::new(0));
    let inside_pump = std::rc::Rc::new(std::cell::Cell::new(false));
    let observed_redraws = std::rc::Rc::clone(&redraws);
    let observed_pump = std::rc::Rc::clone(&inside_pump);
    get(crate::event::set_live_resize_callback(Some(Box::new(move || {
        observed_pump.set(sdl3_sys::test_util::polling_event_queue());
        observed_redraws.set(observed_redraws.get() + 1);
    }))));
    sdl3_sys::test_util::queue_resize_during_poll();
    assert_eq!(get(crate::event::poll_coalesced()), vec![
        crate::Event::Window(W::PixelSizeChanged(640, 480)),
    ]);
    assert!(inside_pump.get(), "resize callback ran after the native pump returned");
    assert_eq!(redraws.get(), 1);
    get(crate::event::set_live_resize_callback(None));
    get(crate::init::quit_subsystems(&[crate::init::Subsystem::Events]));

}
#[test]
fn live_resize_callback_runs_during_the_window_event_pump() {
    claim_dummy_main();
    get(crate::init::init(&[crate::init::Subsystem::Video, crate::init::Subsystem::Events]));
    let window = std::rc::Rc::new(get(crate::Window::create(
        "SDL3 live resize callback test", 32, 24, &[crate::WindowFlag::Hidden],
    )));
    let callback_window = std::rc::Rc::clone(&window);
    let callback_ran = std::rc::Rc::new(std::cell::Cell::new(false));
    let observed = std::rc::Rc::clone(&callback_ran);
    get(crate::event::set_live_resize_callback(Some(Box::new(move || {
        assert!(sdl3_sys::test_util::polling_event_queue());
        let _ = get(callback_window.size());
        observed.set(true);
    }))));
    sdl3_sys::test_util::queue_resize_during_poll();
    assert_eq!(get(crate::event::poll_coalesced()), vec![
        crate::Event::Window(crate::WindowChange::PixelSizeChanged(640, 480)),
    ]);
    assert!(callback_ran.get());
    get(crate::event::set_live_resize_callback(None));
    drop(window);
    get(crate::init::quit_subsystems(&[crate::init::Subsystem::Video, crate::init::Subsystem::Events]));
}
#[test]
fn injected_video_startup_failure_is_captured_immediately() {
    // The body needs a missing video driver, which the parent gate cannot set for this process, so it runs in a child.
    let status = std::process::Command::new(std::env::current_exe().unwrap())
        .args(["--exact", "tests::injected_video_startup_failure_child", "--test-threads=1"])
        .env("RAYS_SDL3_VIDEO_FAILURE_CHILD", "1")
        .env("SDL_VIDEODRIVER", "rays_missing_video_driver")
        .status()
        .unwrap();
    assert!(status.success(), "video-failure child failed");
}

#[test]
fn injected_video_startup_failure_child() {
    if std::env::var("RAYS_SDL3_VIDEO_FAILURE_CHILD").is_err() {
        return;
    }
    claim_main();
    get(crate::init::init(&[crate::init::Subsystem::Events]));
    let failure = match crate::Window::create("no video subsystem", 16, 16, &[]) {
        Err(error) if error.kind == ErrorKind::SdlError && !error.message.is_empty() => error,
        Err(_) => panic!("window creation returned the wrong error"),
        Ok(_) => panic!("window creation succeeded without a video subsystem"),
    };
    let message = failure.message.clone();
    let _ = crate::linked_version();
    assert_eq!(failure.message, message);
    get(crate::init::quit_subsystems(&[crate::init::Subsystem::Events]));
    assert_eq!(
        crate::init::init(&[crate::init::Subsystem::Video]).unwrap_err().kind,
        ErrorKind::SdlError,
    );
}

#[cfg(target_os = "macos")]
#[test]
fn cocoa_layer_ownership_and_trackpad_phases() {
    // Trackpad phases need a real Cocoa window, so the dummy driver gate skips this (macOS only).
    if std::env::var("SDL_VIDEODRIVER").ok().as_deref() == Some("dummy") {
        return;
    }
    claim_main();
    get(crate::init::init(&[crate::init::Subsystem::Video, crate::init::Subsystem::Events]));
    let mut window = get(crate::Window::create(
        "SDL3 Rust Metal test", 64, 48,
        &[crate::WindowFlag::Hidden, crate::WindowFlag::HighPixelDensity, crate::WindowFlag::Metal],
    ));
    let (logical_width, logical_height) = get(window.size());
    let (pixel_width, pixel_height) = get(window.size_in_pixels());
    let density = get(window.pixel_density());
    assert!(density > 0.0 && density.is_finite());
    assert!((pixel_width as f32 / logical_width as f32 - density).abs() < 0.01);
    assert!((pixel_height as f32 / logical_height as f32 - density).abs() < 0.01);
    get(window.set_size(80, 60));
    get(window.sync());
    assert_eq!(get(window.size()), (80, 60));
    get(window.set_background(0.95, 0.95, 0.93));

    let _ = get(crate::event::poll_coalesced());
    for (points, phase) in [(0, 1), (-7, 2), (0, 4)] {
        sdl3_sys::test_util::post_scroll(points, phase);
    }
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    let mut phases = Vec::new();
    while phases.len() < 3 && std::time::Instant::now() < deadline {
        phases.extend(get(crate::event::poll_coalesced()).into_iter().filter_map(|event| {
            if let crate::Event::Scroll { phase, y, .. } = event { Some((phase, y)) } else { None }
        }));
        if phases.len() < 3 { std::thread::sleep(std::time::Duration::from_millis(10)); }
    }
    assert!(matches!(phases.as_slice(), [
        (crate::ScrollPhase::Began, _),
        (crate::ScrollPhase::Changed, y),
        (crate::ScrollPhase::Ended, _),
    ] if y.abs() == 7.0));

    let mut view = get(crate::MetalView::create(&window));
    let token = get(view.layer());
    let second = get(view.layer());
    assert!(token.alive());
    assert_eq!(token.owner_id(), second.owner_id());
    assert_eq!(token.generation(), second.generation());
    assert_eq!(window.destroy().unwrap_err().kind, ErrorKind::ParentHasDependents);
    get(view.destroy());
    get(view.destroy());
    assert!(!token.alive());
    match view.layer() {
        Err(error) => assert_eq!(error.kind, ErrorKind::Destroyed),
        Ok(_) => panic!("destroyed Metal view remained usable"),
    }
    get(window.destroy());
    let mut native_window = sdl3_sys::Window::create(
        "SDL3 sys parent-retention test", 32, 24,
        [sdl3_sys::WindowFlag::Hidden, sdl3_sys::WindowFlag::Metal],
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
    get(crate::init::quit_subsystems(&[crate::init::Subsystem::Video, crate::init::Subsystem::Events]));
}

#[cfg(target_os = "windows")]
#[test]
fn windows_sys_layer_token_dies_before_parent_hwnd() {
    // The dummy driver creates no HWND, so the body runs in a child on the real Windows driver.
    let status = std::process::Command::new(std::env::current_exe().unwrap())
        .args(["--exact", "tests::windows_sys_layer_token_child", "--test-threads=1"])
        .env("RAYS_SDL3_WINDOWS_LAYER_CHILD", "1")
        .env("SDL_VIDEODRIVER", "windows")
        .status()
        .unwrap();
    assert!(status.success(), "Windows layer-token child failed");
}

#[cfg(target_os = "windows")]
#[test]
fn windows_sys_layer_token_child() {
    if std::env::var("RAYS_SDL3_WINDOWS_LAYER_CHILD").is_err() {
        return;
    }
    claim_main();
    get(crate::init::init(&[crate::init::Subsystem::Video, crate::init::Subsystem::Events]));
    let mut window = sdl3_sys::Window::create(
        "SDL3 HWND token lifetime test", 32, 24,
        [sdl3_sys::WindowFlag::Hidden, sdl3_sys::WindowFlag::Metal],
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
    get(crate::init::quit_subsystems(&[crate::init::Subsystem::Video, crate::init::Subsystem::Events]));
}

#[test]
fn repeated_window_and_cursor_drop_releases_native_handles() {
    claim_dummy_main();
    get(crate::init::init(&[crate::init::Subsystem::Video, crate::init::Subsystem::Events]));
    for _ in 0..5_000 {
        let window = get(crate::Window::create("SDL3 lifecycle stress", 16, 16, &[crate::WindowFlag::Hidden]));
        assert_eq!(get(window.size()), (16, 16));
        drop(window);
    }
    for _ in 0..500 {
        drop(get(crate::Window::create("SDL3 release stress", 8, 8, &[crate::WindowFlag::Hidden])));
        match crate::Cursor::create(crate::CursorShape::Text) {
            Ok(cursor) => drop(cursor),
            Err(error) if error.kind == ErrorKind::Unsupported => {}
            Err(error) => panic!("{error}"),
        }
    }
    let _ = get(crate::event::poll_coalesced());
    get(crate::init::quit_subsystems(&[crate::init::Subsystem::Video, crate::init::Subsystem::Events]));
}
