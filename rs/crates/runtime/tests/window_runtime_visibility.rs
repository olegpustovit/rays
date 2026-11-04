mod common;
use runtime::*;

fn get<A>(result: Result<A, ogpu::error::Error>) -> A {
    result.unwrap_or_else(|e| panic!("{}", ogpu::error::to_string(&e)))
}

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
    get(set_text_input(&mut runtime, Some(((1, 2, 20, 12), 7))));
    get(set_text_input(&mut runtime, None));
    get(set_text_input(&mut runtime, None));
    assert!(!get(visible(&mut runtime)), "native window was not initially hidden");
    get(show(&mut runtime));
    assert!(get(visible(&mut runtime)), "show did not set visible flag");
    // a window target hands out a surface configuration with a layer and tracks resize
    let configuration = get(surface_configuration(&mut runtime));
    assert!(configuration.layer.is_some() && configuration.logical_width > 0);
    get(resize(&mut runtime, 40, 30));
    assert_eq!(frame_facts(&runtime).logical_width, 40);
    get(set_cursor(&mut runtime, cursor_shape::Text));
    get(set_cursor(&mut runtime, cursor_shape::Text));
    get(destroy(&mut runtime));
    assert!(show(&mut runtime).is_err());
    println!("runtime visibility: hidden/show flags passed");
}

fn main() {
    common::run(&[("window_runtime_visibility", window_runtime_visibility)]);
}
