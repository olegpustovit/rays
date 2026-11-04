mod common;
use runtime::*;

// Needs SDL_VIDEODRIVER=dummy (or a display) and the initial thread; a missing window is a skip,
// as in the OCaml test.
fn window_runtime_visibility() {
    let mut runtime = match create(None, None, None, None, 32, 24) {
        Ok(runtime) => runtime,
        Err(e) => {
            println!("runtime visibility: skipped ({})", ogpu::error::to_string(&e));
            return;
        }
    };
    set_text_input(&mut runtime, Some(((1, 2, 20, 12), 7))).unwrap();
    set_text_input(&mut runtime, None).unwrap();
    set_text_input(&mut runtime, None).unwrap();
    assert!(!visible(&mut runtime).unwrap(), "native window was not initially hidden");
    show(&mut runtime).unwrap();
    assert!(visible(&mut runtime).unwrap(), "show did not set visible flag");
    // a window target hands out a surface configuration with a layer and tracks resize
    let configuration = surface_configuration(&mut runtime).unwrap();
    assert!(configuration.layer.is_some() && configuration.logical_width > 0);
    resize(&mut runtime, 40, 30).unwrap();
    assert_eq!(frame_facts(&runtime).logical_width, 40);
    set_cursor(&mut runtime, cursor_shape::Text).unwrap();
    set_cursor(&mut runtime, cursor_shape::Text).unwrap();
    destroy(&mut runtime).unwrap();
    assert!(show(&mut runtime).is_err());
    println!("runtime visibility: hidden/show flags passed");
}

fn main() {
    common::run(&[("window_runtime_visibility", window_runtime_visibility)]);
}
