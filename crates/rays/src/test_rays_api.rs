//! The internal layer's value contracts that need no native resources: math, camera lens,
//! materials and lights, frame facts, 2D staging order. (Resource ownership and the automatic
//! text staging are in `tests/window_resources.rs`: they need the SDL main thread.)
use crate::camera;
use crate::easy_camera;
use crate::frame;
use crate::light;
use crate::material;
use crate::path;
use crate::scene::{self, Layer};
use crate::sketch;
use crate::texture;
use input::{Event, Key, MouseButton};
use ogpu_core::error::ErrorKind;
use rays_math::{color, mat4, noise, rand, vec3};
use scene_command::path::{Command, Point};
use scene_command::render_ir;
use std::cell::Cell;
use std::rc::Rc;

fn check(ok: bool, message: &str) {
    assert!(ok, "{message}");
}

#[test]
fn sketch_startup_result() {
    let called = Rc::new(Cell::new(false));
    let seen = called.clone();
    let config = sketch::config { width: 0, ..sketch::default_config() };
    let program = sketch::program {
        init: Box::new(|_| Box::new(()) as sketch::model),
        update: Box::new(|_, _| None),
        view: Box::new(move |_, _| {
            seen.set(true);
            Vec::new()
        }),
        after_present: None,
        crash_dump: None,
        idle: None,
        on_stop: None,
    };
    match sketch::run_state(Some(config), None, None, None, program) {
        Err(error) if error.kind == ErrorKind::InvalidArgument => check(!called.get(), "invalid sketch configuration reached user callbacks"),
        Err(error) => panic!("{}", ogpu_core::error::to_string(&error)),
        Ok(_) => panic!("invalid sketch configuration succeeded"),
    }
}

#[test]
fn math_vec_color_mat4() {
    let normal = vec3::cross(vec3::unit_x(), vec3::unit_y());
    check(vec3::nearly_equal(normal, vec3::unit_z(), 1e-12), "Vec3 parity");
    check(color::hex("#f08c") == Ok(color::rgba(255, 0, 136, 204)), "Color hex");
    check(color::gradient(&[color::red(), color::rgb(0, 0, 255)], 0.5) == Ok(color::rgba(128, 0, 128, 255)), "Color gradient");
    let m4 = mat4::mul(&mat4::translation(vec3::create(2., 3., 4.)), &mat4::scaling(vec3::create(2., 2., 2.)));
    check(vec3::nearly_equal(mat4::transform_point(&m4, vec3::create(1., 1., 1.)), vec3::create(4., 5., 6.), 1e-12), "Mat4 affine");
    check(
        mat4::inverse(&m4).is_some_and(|inverse| mat4::nearly_equal(&mat4::mul(&inverse, &m4), &mat4::identity(), 1e-12)),
        "Mat4 inverse",
    );
}

#[test]
fn random_and_noise_are_deterministic() {
    let a = rand::seed64(0x123456789abcdef);
    let b = rand::seed64(0x123456789abcdef);
    let left: Vec<f64> = (0..100_000).map(|i| rand::float_at(a, i)).collect();
    let right: Vec<f64> = (0..100_000).map(|i| rand::float_at(b, i)).collect();
    check(left == right && left.iter().all(|x| *x >= 0. && *x < 1.), "Rand deterministic 100k");
    let n = noise::create(42);
    check(noise::sample3(&n, 0.25, 0.5, 0.75) == noise::sample3(&noise::create(42), 0.25, 0.5, 0.75), "Noise seed");
}

#[test]
fn camera_lens_material_light_projection() {
    // A camera's lens: pinhole by default, focus on the target unless set.
    let cam = camera::perspective(None, None, None, vec3::create(0., 0., 4.), vec3::zero()).expect("camera");
    check(camera::lens(&cam) == camera::pinhole() && camera::focus_distance(&cam) == 4., "Camera default lens");
    let lens = camera::lens { aperture: 0.25, focus_distance: Some(2.5) };
    let focused = camera::with_lens(lens, &cam).expect("lens");
    check(
        camera::lens(&focused) == lens
            && camera::focus_distance(&focused) == 2.5
            && camera::focus_distance(&camera::with_lens(camera::lens { focus_distance: None, ..lens }, &focused).expect("lens")) == 4.,
        "Camera.with_lens",
    );
    check(camera::with_lens(camera::lens { aperture: -1., ..lens }, &cam).is_err(), "Camera.with_lens accepted a negative aperture");
    let m = material::matte(color::rgb(10, 20, 30));
    check(m.diffuse == color::rgb(10, 20, 30), "Material matte");
    let l = light::spot(None, None, None, vec3::create(1., 2., 3.), vec3::create(0., 0., -1.), 0.5, 4.).expect("spot");
    check(l.intensity == 1., "Light intensity");
    let cam = camera::perspective(None, None, None, vec3::create(0., 0., 5.), vec3::zero()).expect("camera");
    check(camera::world_to_screen((0, 0, 64, 64), &cam, vec3::zero()).expect("projection").is_some(), "Camera projection");
    let easy = easy_camera::create(Some(vec3::zero()), Some(5.), None, None, None, None).expect("easy");
    check(easy_camera::distance(&easy) == 5., "Easy_camera distance");
}

#[test]
fn frame_facts_scene_lowering_path_texture() {
    check(sketch::set_cursor(sketch::cursor::Horizontal_resize).is_err(), "cursor without an active sketch");
    let events = vec![
        Event::Key_pressed(Key::Space),
        Event::Pointer_pressed(MouseButton::LeftButton, (2., 3.)),
        Event::Pointer_moved((4., 8.)),
        Event::Scrolled((1., -2.)),
        Event::Text_editing { text: "ime".to_string(), start: 0, length: 3 },
        Event::Resized((8, 9)),
        Event::Closed,
    ];
    let mut input = input::at_time(1.);
    input.size = (4, 3);
    input.dt = 0.5;
    input.frame = 7;
    input.tick = 7;
    input.pointer = (4., 8.);
    input.keys = vec![Key::Space];
    input.buttons = vec![MouseButton::LeftButton, MouseButton::MouseX2];
    input.events = events;
    let f = frame::t { input, pixel_scale: (2., 2.), fps: 2., mouse_delta: (3., 6.) };
    check(
        frame::mouse_down(MouseButton::LeftButton, &f)
            && frame::mouse_down(MouseButton::MouseX2, &f)
            && !frame::mouse_down(MouseButton::RightButton, &f)
            && frame::has_event(&|e| matches!(e, Event::Pointer_moved(_)), &f)
            && frame::has_event(&|e| matches!(e, Event::Closed), &f)
            && !frame::has_event(&|e| matches!(e, Event::Focus_lost), &f),
        "frame facts",
    );
    let s = vec![
        scene::clear(color::black()),
        scene::rect((0, 0), 4, 3, Some(color::red()), None).expect("rect"),
        scene::text_input_region((1, 1), 2, 1, Some(true), Some(1)).expect("region"),
    ];
    let ir = Layer::to_ir(&s).expect("to_ir");
    check(render_ir::commands_readonly(&ir).len() == 2 && Layer::text_regions(&s) == vec![(1, 1, 2, 1, true, 1)], "scene lowering");
    check(scene::text_input_region((0, 0), 1, 1, None, Some(-1)).is_err(), "negative IME cursor accepted");
    let p = |x: f64, y: f64| Point { x, y };
    let open_path = path::of_commands(&[Command::MoveTo(p(0.25, 0.5)), Command::LineTo(p(4.5, 0.5)), Command::LineTo(p(4.5, 4.75)), Command::LineTo(p(0.25, 4.75))]);
    let closed = path::of_commands(&[
        Command::MoveTo(p(0.25, 0.5)),
        Command::LineTo(p(4.5, 0.5)),
        Command::LineTo(p(4.5, 4.75)),
        Command::LineTo(p(0.25, 4.75)),
        Command::Close,
    ]);
    check(path::tessellate(0.25, path::FillRule::NonZero, &closed).is_ok(), "Path tessellates closed commands");
    match path::stroke(0.25, 1., path::Cap::Butt, path::Join::Miter, 4., &open_path) {
        Ok(mesh) => check(mesh.vertices[0].x == 0.25, "Path retains subpixel coordinates"),
        Err(_) => panic!("Path stroke"),
    }
    let (red, blue) = (color::red(), color::rgb(0, 0, 255));
    let tex = texture::generate_mipmaps(
        &texture::init(2, 2, &|x, y| match (x, y) {
            (0, 0) => red,
            (1, 0) => color::rgb(0, 255, 0),
            (0, 1) => blue,
            _ => color::white(),
        })
        .expect("texture"),
    )
    .expect("mipmaps");
    check(texture::Private::levels(&tex).expect("levels").len() == 2, "Texture mipmaps");
    check(texture::size(&tex) == Ok((2, 2)) && texture::pixels(&tex).expect("pixels")[0] == red, "Texture upper-left texel");
}

/// Layered 2D staging: order, scope and layer breaks (the text/View3d layering is in
/// `tests/window_resources.rs`, which needs a font).
#[test]
fn scene_staging_order_and_layer_breaks() {
    let context = Layer::create_cache_context();
    Layer::with_cache_context(&context, &mut || {
        let scene2_only = vec![
            scene::clear(color::black()),
            scene::rect((1, 2), 8, 6, Some(color::red()), None).expect("rect"),
            scene::rect((2, 3), 1, 1, Some(color::white()), None).expect("rect"),
        ];
        let staged = Layer::stage_native(16, 12, &scene2_only).expect("stage");
        match staged.layers.as_slice() {
            [Layer::native_layer::Scene2_layer(ir, _)] => check(Rc::ptr_eq(ir, &staged.scene2), "Scene2-only native staging duplicated its render IR"),
            _ => panic!("Scene2-only native staging split one ordered layer"),
        }
        let split = vec![
            scene::rect((1, 2), 8, 6, Some(color::red()), None).expect("rect"),
            Layer::layer_break(),
            scene::rect((2, 3), 1, 1, Some(color::white()), None).expect("rect"),
        ];
        let staged_split = Layer::stage_native(16, 12, &split).expect("stage");
        match staged_split.layers.as_slice() {
            [Layer::native_layer::Scene2_layer(first, _), Layer::native_layer::Scene2_layer(second, _)] => {
                let (a, b) = (render_ir::commands_readonly(first), render_ir::commands_readonly(second));
                check(a.len() == 1 && b.len() == 1 && matches!(a[0], render_ir::Command::Geometry(_)) && matches!(b[0], render_ir::Command::Geometry(_)), "Scene2 layer break changed command ordering");
            }
            _ => panic!("Scene2 layer break did not isolate dynamic commands"),
        }
        check(render_ir::commands_readonly(&staged_split.scene2).len() == 2, "Scene2 layer break leaked into compatibility IR");
        let styled = vec![scene::polygon(&[(0, 0), (8, 0), (8, 8)], Some(color::red()), Some(color::white())).expect("polygon")];
        let (styled_ir, _) = Layer::stage(16, 16, &styled).expect("stage");
        let c = render_ir::commands_readonly(&styled_ir);
        check(c.len() == 2 && c.iter().all(|x| matches!(x, render_ir::Command::Geometry(_))), "polygon fill/stroke did not stage two meshes");
    });
}
