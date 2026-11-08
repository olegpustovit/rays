//! Runtime contracts from test_rays.ml; test_text.ml is helper-only (zero cases).
use rays::{camera, easy_camera, font, image, light, path, scene};
use rays_math::{color, mat4, noise, rand, vec3};
use std::sync::{Mutex, OnceLock};

// Rust exposes the scheduler, not OCaml's init_array/map_array convenience wrappers.
// Keep the original sizes, grains and nested scheduling through the public scheduler.
fn init<T: Send + Sync>(grain: i64, length: usize, f: impl Fn(i64) -> T + Send + Sync) -> Vec<T> {
    let values: Vec<OnceLock<T>> = (0..length).map(|_| OnceLock::new()).collect();
    parallel::for_(grain, 0, length as i64 - 1, &|index| {
        assert!(values[index as usize].set(f(index)).is_ok());
        Ok(())
    }).expect("parallel initialization");
    values.into_iter().map(|value| value.into_inner().expect("initialized slot")).collect()
}

fn run_core() {
    let initialized = init(1, 10_000, |index| index * index);
    assert_eq!((initialized[0], initialized[9_999]), (0, 99_980_001));
    let mapped = init(1, initialized.len(), |index| initialized[index as usize] + 1);
    assert_eq!(mapped[5_000], 25_000_001);
    let nested = init(1, 2, |index| {
        let value = [10, 20][index as usize];
        init(1, 3, |index| [1, 2, 3][index as usize] + value)
    });
    assert_eq!(nested, vec![vec![11, 12, 13], vec![21, 22, 23]]);
    let left = OnceLock::new();
    let right = OnceLock::new();
    parallel::run_unit(Some(2), &|| parallel::both_unit(
        &|| { assert!(left.set(20 + 1).is_ok()); Ok(()) },
        &|| { assert!(right.set(6 * 7).is_ok()); Ok(()) },
    )).expect("parallel both");
    assert_eq!((left.into_inner(), right.into_inner()), (Some(21), Some(42)));
    let calling_domain = std::thread::current().id();
    parallel::run_unit(Some(1), &|| {
        let domains = init(1, 10_000, |_| std::thread::current().id());
        assert!(domains.iter().all(|id| *id == calling_domain));
        Ok(())
    }).expect("sequential domain");
    parallel::run_unit(Some(2), &|| {
        let domains = init(64, 8, |_| std::thread::current().id());
        let observed = Mutex::new(std::thread::current().id());
        parallel::for_(64, 0, 7, &|_| {
            *observed.lock().unwrap() = std::thread::current().id();
            Ok(())
        })?;
        assert!(domains.iter().all(|id| *id == calling_domain));
        assert_eq!(*observed.lock().unwrap(), calling_domain);
        Ok(())
    }).expect("sequential grain cutoff");

    assert_eq!(color::to_tuple(color::hex("#369c").expect("short RGBA")), (0x33, 0x66, 0x99, 0xcc));
    let indexed = rand::seed64(0x7eed_1234_5678_9abc);
    let indexed_values: Vec<_> = (0..10_000).map(|index| rand::float_at(indexed, index)).collect();
    assert_eq!(indexed_values, (0..10_000).map(|index| rand::float_at(indexed, index)).collect::<Vec<_>>());
    assert!(indexed_values.iter().all(|value| *value >= 0. && *value < 1.));
    assert_ne!(indexed_values[17], indexed_values[18]);
    let sample_a = noise::fbm2(None, &noise::create(17), 1.25, -3.5).expect("noise");
    let sample_b = noise::fbm2(None, &noise::create(17), 1.25, -3.5).expect("noise");
    assert_eq!(sample_a, sample_b);
    assert!((0. ..=1.).contains(&sample_a));
    let transform = mat4::mul(&mat4::translation(vec3::create(3., -2., 5.)),
        &mat4::mul(&mat4::rotation_y(0.7), &mat4::scaling(vec3::create(2., 3., 4.))));
    let inverse = mat4::inverse(&transform).expect("invertible transform");
    let point = vec3::create(1.25, -0.5, 3.);
    assert!(vec3::nearly_equal(point, mat4::transform_point(&inverse, mat4::transform_point(&transform, point)), 1e-9));
    let camera = camera::perspective(None, None, None, vec3::create(0., 0., 5.), vec3::zero()).expect("camera");
    let screen = camera::world_to_screen((0, 0, 640, 360), &camera, vec3::zero()).expect("projection").expect("visible origin");
    assert!((screen.x - 320.).abs() <= 1e-6 && (screen.y - 180.).abs() <= 1e-6);
    let easy = easy_camera::create(None, Some(5.), None, None, None, None).expect("easy camera");
    // Cameras are by-value in Rust: public equality/invalidation is observable, physical cache identity is not.
    assert_eq!(easy_camera::camera(&easy).unwrap(), easy_camera::camera(&easy).unwrap());
    assert_ne!(easy_camera::camera(&easy_camera::with_distance(6., &easy).unwrap()).unwrap(), easy_camera::camera(&easy).unwrap());
    let viewed = easy_camera::of_view(vec3::create(3., 4., -5.), vec3::create(1., 1., 1.),
        &easy_camera::create(None, None, None, None, None, None).unwrap());
    assert!(vec3::nearly_equal(camera::position(&easy_camera::camera(&viewed).unwrap()), vec3::create(3., 4., -5.), 1e-9));
    assert_eq!(easy_camera::target(&viewed), vec3::create(1., 1., 1.));
    let framed = easy_camera::frame_bounds(vec3::create(-1., -1., -1.), vec3::create(3., 1., 1.),
        &easy_camera::create(None, None, None, None, Some(std::f64::consts::PI / 2.), None).unwrap());
    assert!(vec3::nearly_equal(easy_camera::target(&framed), vec3::create(1., 0., 0.), 1e-12));
    assert!((easy_camera::distance(&framed) - 6_f64.sqrt() * 1.2).abs() <= 1e-9);
    let sampled_area = light::area(None, None, None, Some(16), vec3::create(0., 3., 0.), vec3::create(0., -1., 0.), 4., 2.).unwrap();
    assert!(matches!(sampled_area.kind, light::kind::Area { samples: 16, .. }));
}

fn run_keys() {
    use input::Key as K;
    use sdl3::Key as S;
    for (raw, expected) in [
        (S::ArrowRight, K::ArrowRight), (S::ArrowLeft, K::ArrowLeft),
        (S::ArrowDown, K::ArrowDown), (S::ArrowUp, K::ArrowUp),
        (S::Enter, K::Enter), (S::Escape, K::Escape), (S::Control, K::Ctrl),
        (S::Meta, K::Meta), (S::PageUp, K::PageUp), (S::Char('a'), K::KeyChar('a')),
        (S::Space, K::Space), (S::F12, K::F12), (S::Unknown(4242), K::Unknown(4242)),
    ] {
        assert_eq!(runtime::runtime_input_sdl3::key(raw), expected);
    }
}

fn run_scene2() {
    let commands = [
        path::Command::MoveTo(path::Point { x: 0., y: 0. }),
        path::Command::LineTo(path::Point { x: 10.5, y: 5.25 }),
        path::Command::LineTo(path::Point { x: 20., y: 0. }),
    ];
    let open = path::of_commands(&commands);
    let mut closed = commands.to_vec();
    closed.push(path::Command::Close);
    for (name, node) in [
        ("point", scene::point((3, 4), None).unwrap()),
        ("open path", scene::path(None, &open).unwrap()),
        ("closed path", scene::path(Some(color::white()), &path::of_commands(&closed)).unwrap()),
    ] {
        scene::Layer::to_ir(&[node]).unwrap_or_else(|message| panic!("{name} did not lower: {message}"));
    }
}

fn run_font() {
    assert!(crate::common::gpu(), "font runtime requires the native qualification device");
    let font = font::system(Some(20)).expect("system font");
    let ascent = font::get_ascent(&font).expect("font metrics");
    let pixels = |align| {
        let image = font::cached_text(Some(400), Some(align), None, &font,
            "short\na much longer second line", font::render_mode::Blended(color::white())).expect("cached text");
        image::pixels(&image).expect("text pixels")
    };
    let left = pixels(font::alignment::Left);
    let center = pixels(font::alignment::Center);
    font::destroy(&font);
    assert!(ascent > 0, "font metrics are placeholders");
    assert_ne!(left, center, "font alignment is ignored");
}

pub fn cases() -> Vec<(&'static str, fn())> {
    vec![("rays.core", run_core), ("rays.keys", run_keys),
        ("rays.scene2", run_scene2), ("rays.font", run_font)]
}
