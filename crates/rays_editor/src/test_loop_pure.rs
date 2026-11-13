//! Pure pieces of bundle F: the World map geometry, the camera control's defaults and the free
//! functions of the viewport that need no editor.
use crate::viewport as V;
use crate::world_map::{map_rect, map_uv, wrap_degrees};
use rays_math::vec3;

#[test]
fn map_fits_two_to_one_and_centres() {
    assert_eq!(map_rect((10, 20, 400, 100)), (110, 20, 200, 100));
    assert_eq!(map_rect((0, 0, 100, 400)), (0, 175, 100, 50));
}

#[test]
fn map_uv_is_none_off_the_map() {
    let area = (0, 0, 200, 100);
    assert_eq!(map_uv(area, (100., 50.)), Some((0.5, 0.5)));
    assert_eq!(map_uv(area, (201., 50.)), None);
    assert_eq!(map_uv((0, 0, 400, 100), (10., 50.)), None);
}

#[test]
fn degrees_wrap_into_minus_180_180() {
    assert_eq!(wrap_degrees(190.), -170.);
    assert_eq!(wrap_degrees(-190.), 170.);
    assert_eq!(wrap_degrees(0.), 0.);
}

#[test]
fn control_toggles_and_camera_defaults() {
    let control = V::create_control();
    assert!(V::ui_visible(&control));
    assert!(!V::ui_visible(&V::toggle_ui(&control)));
    // opening the camera section while hidden shows the UI and opens it explicitly
    let hidden = V::toggle_ui(&control);
    let opened = V::open_camera(&hidden);
    assert!(V::ui_visible(&opened) && opened.open_camera == Some(true));
    let camera = V::default_camera();
    assert_eq!(rays::easy_camera::distance(&camera), 7.);
}

#[test]
fn same_view_tolerates_rounding_only() {
    let a = rays::camera::perspective(Some(1.), None, None, vec3::create(0., 0., 5.), vec3::zero()).unwrap();
    let b = rays::camera::perspective(Some(1.), None, None, vec3::create(0., 0., 5. + 1e-9), vec3::zero()).unwrap();
    let c = rays::camera::perspective(Some(1.), None, None, vec3::create(0., 0., 6.), vec3::zero()).unwrap();
    assert!(V::same_view(&a, &b));
    assert!(!V::same_view(&a, &c));
}

/// A sketch in a canvas pane picks what is under the cursor: the pane's header and position are
/// taken off the pointer and off every pointer event.
#[test]
fn canvas_pane_input_is_pane_local() {
    use input::{Event, MouseButton};
    let window = input::Frame {
        size: (1440, 1000),
        pointer: (300., 228.),
        events: vec![Event::PointerPressed(MouseButton::LeftButton, (300., 228.)), Event::Scrolled((0., 1.)), Event::PointerReleased(MouseButton::LeftButton, (301., 230.))],
        ..input::at_time(0.)
    };
    let pane = crate::editor_loop::pane_input(&window, (100, 28, 800, 600));
    assert_eq!((pane.size, pane.pointer), ((800, 600), (200., 200.)));
    assert_eq!(pane.events, vec![Event::PointerPressed(MouseButton::LeftButton, (200., 200.)), Event::Scrolled((0., 1.)), Event::PointerReleased(MouseButton::LeftButton, (201., 202.))]);
}
