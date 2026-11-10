use crate::camera;
use crate::easy_camera as ec;
use crate::frame;
use input::{Event, Key, MouseButton};
use rays_math::vec3;

fn frame_(keys: Vec<Key>, time: f64, dt: f64, events: Vec<Event>) -> frame::Frame {
    let mut input = input::at_time(time);
    input.dt = dt;
    input.size = (320, 240);
    input.frame = 0;
    input.tick = 0;
    input.pointer = (20., 20.);
    input.keys = keys;
    input.events = events;
    frame::Frame { input, pixel_scale: (1., 1.), fps: 60., mouse_delta: (0., 0.) }
}

fn frame(events: Vec<Event>) -> frame::Frame {
    frame_(Vec::new(), 1., 0., events)
}

type Snap = (vec3::T, vec3::T, vec3::T, camera::Projection, f64);

fn snapshot(value: &ec::EasyCamera) -> Snap {
    let c = ec::camera(value).expect("camera");
    (camera::position(&c), camera::target(&c), camera::up(&c), camera::projection(&c), ec::distance(value))
}

fn drag(button: MouseButton) -> Vec<Event> {
    vec![Event::PointerPressed(button, (20., 20.)), Event::PointerMoved((35., 27.)), Event::PointerReleased(button, (35., 27.))]
}

fn update(value: &ec::EasyCamera, f: &frame::Frame) -> ec::EasyCamera {
    ec::update(value, f).expect("update")
}

#[test]
fn easy_camera_buttons_modifiers_bounds_reset_cancellation_fly() {
    let initial = ec::create(None, None, None, None, None, Some(false)).expect("create");
    for button in [MouseButton::LeftButton, MouseButton::MiddleButton, MouseButton::RightButton] {
        let plain = update(&initial, &frame(drag(button)));
        for key in [Key::Shift, Key::Ctrl, Key::Alt, Key::Meta] {
            let mixed = vec![
                Event::KeyPressed(key),
                Event::PointerPressed(button, (20., 20.)),
                Event::KeyReleased(key),
                Event::PointerMoved((35., 27.)),
                Event::PointerReleased(button, (35., 27.)),
            ];
            assert!(snapshot(&update(&initial, &frame_(vec![key], 1., 0., mixed))) == snapshot(&plain));
        }
        if button == MouseButton::LeftButton {
            assert!(ec::target(&plain) == ec::target(&initial));
            assert!(
                camera::position(&ec::camera(&plain).unwrap()) != camera::position(&ec::camera(&initial).unwrap())
            );
        } else {
            assert!(ec::target(&plain) != ec::target(&initial));
            assert!(ec::distance(&plain) == ec::distance(&initial));
        }
    }
    assert!(snapshot(&update(&initial, &frame(drag(MouseButton::MouseX1)))) == snapshot(&initial));
    let bounded = ec::with_control_area(Some((10, 10, 30, 30)), &initial).expect("area");
    assert!(snapshot(&update(&bounded, &frame(drag(MouseButton::LeftButton)))) != snapshot(&bounded));
    assert!(
        snapshot(&update(
            &bounded,
            &frame(vec![
                Event::PointerPressed(MouseButton::LeftButton, (40., 20.)),
                Event::PointerMoved((35., 27.)),
                Event::PointerReleased(MouseButton::LeftButton, (35., 27.)),
            ])
        )) == snapshot(&bounded)
    );
    let moved = update(&initial, &frame(drag(MouseButton::LeftButton)));
    let mut scaled = frame(drag(MouseButton::LeftButton));
    scaled.pixel_scale = (2., 2.);
    assert!(snapshot(&update(&initial, &scaled)) == snapshot(&moved));
    let far = vec![
        Event::PointerPressed(MouseButton::LeftButton, (20., 20.)),
        Event::PointerMoved((90., 70.)),
        Event::PointerReleased(MouseButton::LeftButton, (90., 70.)),
    ];
    assert!(snapshot(&update(&bounded, &frame(far.clone()))) == snapshot(&update(&initial, &frame(far))));
    let reset = update(&moved, &frame_(Vec::new(), 1.2, 0., vec![Event::PointerPressed(MouseButton::LeftButton, (20., 20.))]));
    assert!(snapshot(&reset) == snapshot(&initial));
    for stop in [Event::PointerCancelled(MouseButton::LeftButton), Event::FocusLost] {
        let active = update(
            &initial,
            &frame(vec![Event::PointerPressed(MouseButton::LeftButton, (20., 20.)), Event::PointerMoved((35., 27.)), stop]),
        );
        assert!(snapshot(&update(&active, &frame(vec![Event::PointerMoved((70., 80.))]))) == snapshot(&active));
    }
    let wheel = update(&initial, &frame(vec![Event::Scrolled((0., 1.))]));
    assert!(ec::distance(&wheel) < ec::distance(&initial));
    let (walk, _) = ec::fly(2., &initial, &frame_(vec![Key::KeyChar('w')], 1., 1., Vec::new())).expect("fly");
    let (fast, speed) = ec::fly(2., &initial, &frame_(vec![Key::KeyChar('w'), Key::Shift], 1., 1., vec![Event::Scrolled((0., 1.))])).expect("fly");
    let eye = camera::position(&ec::camera(&initial).unwrap());
    let delta = |value: &ec::EasyCamera| vec3::sub(camera::position(&ec::camera(value).unwrap()), eye);
    assert!(vec3::nearly_equal(delta(&fast), vec3::scale(delta(&walk), 4.), 1e-12));
    assert!((speed - 2.4).abs() < 1e-12);
}
