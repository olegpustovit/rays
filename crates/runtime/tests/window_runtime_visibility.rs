mod common;
use runtime::*;

// The render target over a real window: hidden/show, surface configuration and resize tracking.
fn window_runtime_visibility() {
    let mut runtime = match create(None, None, None, None, 32, 24) {
        Ok(runtime) => runtime,
        Err(e) => {
            println!("RAYS_TEST_SKIP: runtime visibility: skipped ({})", ogpu_core::error::to_string(&e));
            assert!(std::env::var("RAYS_ALLOW_SKIP").as_deref() == Ok("1"), "missing required GPU/display/audio support; RAYS_ALLOW_SKIP=1 explicitly permits skipping");
            return;
        }
    };
    set_text_input(&mut runtime, Some(((1, 2, 20, 12), 7))).unwrap();
    set_text_input(&mut runtime, None).unwrap();
    assert!(!visible(&mut runtime).unwrap(), "native window was not initially hidden");
    show(&mut runtime).unwrap();
    assert!(visible(&mut runtime).unwrap(), "show did not set visible flag");
    let configuration = surface_configuration(&mut runtime).unwrap();
    assert!(configuration.layer.is_some() && configuration.logical_width > 0);
    resize(&mut runtime, 40, 30).unwrap();
    assert_eq!(frame_facts(&runtime).logical_width, 40);
    set_cursor(&mut runtime, CursorShape::Text).unwrap();
    destroy(&mut runtime).unwrap();
    assert!(show(&mut runtime).is_err());
    println!("runtime visibility: hidden/show flags passed");
}

fn main() {
    common::run(&[("window_runtime_visibility", window_runtime_visibility)]);
}
