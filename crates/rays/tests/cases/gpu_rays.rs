//! Native offscreen geometry, World, depth and upload cases release the helper device lease before lifetime assertions.
use crate::common;
use rays::camera;
use rays::canvas;
use rays::material;
use rays::mesh;
use rays::scene::{self, layer};
use rays::scene3 as d3;
use rays::world;
use rays_math::{color, mat4, vec3};
use renderer::renderer as rr;
use renderer::scene_execution as se;
use scene_command::ui_batch;
use std::sync::Arc;

fn live_handles() -> i64 {
    ogpu_core::impl_::live_handles().expect("live handles")
}

fn require(condition: bool, message: &str) {
    assert!(condition, "{message}");
}

fn render(canvas_: &canvas::Canvas, density: Option<i64>, scene_: &[scene::node]) {
    canvas::render(density, canvas_, scene_).unwrap_or_else(|e| fail(&format!("Canvas.render: {e}")));
}

fn stats(canvas_: &canvas::Canvas) -> rr::Stats {
    canvas::private::native_stats(canvas_).expect("native stats")
}

fn rgba(canvas_: &canvas::Canvas, x: i64, y: i64) -> (i64, i64, i64, i64) {
    color::to_tuple(canvas::pixel(canvas_, x, y).expect("pixel").expect("in bounds"))
}

fn snapshot(canvas_: &canvas::Canvas) -> Vec<u8> {
    let image = canvas::to_image(canvas_).expect("to_image");
    let pixels = rays::image::pixels(&image).expect("pixels");
    let _ = rays::image::destroy(&image);
    pixels
}

fn mesh_node(material_: material::Material, cull: d3::cull, m: &mesh::Mesh) -> d3::node {
    d3::mesh(Some(material_), None, None, Some(cull), None, m.clone())
}

fn scene_of(nodes: Vec<d3::node>) -> d3::Scene3 {
    d3::create(None, None, None, None, nodes).expect("scene3")
}

fn gpu_rays_camera_depth() {
    if !common::gpu() {
        return;
    }
    common::release_keepalive();
    let baseline = live_handles();
    let canvas_ = canvas::create(32, 32).expect("canvas");
    let plane = mesh::plane(1., 1.).expect("plane");
    let cameras = [
        camera::orthographic(2., vec3::create(0., 0., 2.), vec3::zero()).expect("camera"),
        camera::perspective(None, None, None, vec3::create(0., 0., 0.15), vec3::zero()).expect("camera"),
    ];
    for cam in &cameras {
        let far = d3::translate(vec3::create(0., 0., -0.02), vec![mesh_node(material::unlit(color::red()), d3::cull::Cull_none, &plane)]);
        let near = mesh_node(material::unlit(color::rgb(0, 255, 0)), d3::cull::Cull_none, &plane);
        let s = vec![scene::clear(color::black()), scene::view3d(None, cam, scene_of(vec![near, far]))];
        render(&canvas_, None, &s);
        require(rgba(&canvas_, 16, 16) == (0, 255, 0, 255), "camera native near/far clipping or depth order");
        let first = snapshot(&canvas_);
        render(&canvas_, None, &s);
        require(snapshot(&canvas_) == first, "camera depth retained replay pixels");
        let transforms = [mat4::identity(), mat4::translation(vec3::create(0., 0., -0.02))];
        let instances = d3::instances_array(Some(material::unlit(color::rgb(0, 0, 255))), None, Some(d3::cull::Cull_none), None, plane.clone(), &transforms);
        let s = vec![scene::clear(color::black()), scene::view3d(None, cam, scene_of(vec![instances]))];
        render(&canvas_, None, &s);
        require(rgba(&canvas_, 16, 16) == (0, 0, 255, 255), "camera native instance clipping");
        let first = snapshot(&canvas_);
        render(&canvas_, None, &s);
        require(snapshot(&canvas_) == first, "camera instance retained replay pixels");
    }
    let _ = canvas::destroy(&canvas_);
    assert_eq!(live_handles(), baseline, "camera native live-handle delta");
}

fn gpu_rays_canvas_resize_density() {
    if !common::gpu() {
        return;
    }
    let canvas_ = canvas::create(8, 8).expect("canvas");
    render(&canvas_, None, &[scene::clear(color::rgb(255, 0, 0))]);
    require(rgba(&canvas_, 3, 3) == (255, 0, 0, 255), "canvas clear");
    require(canvas::pixel(&canvas_, 8, 0).expect("pixel").is_none(), "out of bounds pixel");
    let before = snapshot(&canvas_);
    render(&canvas_, None, &[scene::clear(color::rgb(255, 0, 0))]);
    require(snapshot(&canvas_) == before, "repeat render pixels");
    require(canvas::render(Some(0), &canvas_, &[]).is_err(), "density 0 accepted");
    let _ = canvas::destroy(&canvas_);
    require(canvas::render(None, &canvas_, &[]).is_err(), "destroyed canvas rendered");
}

fn tone(linear: f64, exposure: f64) -> i64 {
    let c = color::aces(linear * exposure);
    ((rays::num::fmin(1., rays::num::fmax(0., c)).powf(1. / 2.2) * 255.) + 0.5) as i64
}

fn near(tolerance: i64, name: &str, expected: i64, c: color::T) {
    for (channel, value) in [("r", c.r), ("g", c.g), ("b", c.b)] {
        assert!((value - expected).abs() <= tolerance, "{name}: {channel} channel {value}, expected {expected} +- {tolerance}");
    }
}

fn uniform(l: f64) -> world::World {
    let c = world::rgb(l, l, l);
    world::World {
        layers: vec![world::entry {
            name: "furnace".to_string(),
            visible: true,
            layer: world::layer::Gradient { zenith: c, horizon: c, nadir: c, sharpness: 1. },
        }],
        ..world::default()
    }
}

fn sphere() -> d3::node {
    d3::mesh(Some(material::matte(color::white())), None, None, None, None, sphere_mesh())
}

fn sphere_mesh() -> mesh::Mesh {
    let (segments, rings) = (24_i64, 16_i64);
    let row = segments + 1;
    let normals: Vec<vec3::T> = (0..row * (rings + 1))
        .map(|index| {
            let theta = (index % row) as f64 / segments as f64 * 2. * std::f64::consts::PI;
            let phi = (index / row) as f64 / rings as f64 * std::f64::consts::PI;
            vec3::create(phi.sin() * theta.cos(), phi.cos(), phi.sin() * theta.sin())
        })
        .collect();
    let mut indices = vec![0i64; (segments * rings * 6) as usize];
    for cell in 0..segments * rings {
        let a = (cell / segments * row) + (cell % segments);
        let (b, d) = (a + 1, a + row);
        indices[(cell * 6) as usize..(cell * 6 + 6) as usize].copy_from_slice(&[a, b, d + 1, a, d + 1, d]);
    }
    let planes = |v: &[vec3::T]| mesh::Vec3_buffer::of_planes(v.iter().map(|p| p.x).collect(), v.iter().map(|p| p.y).collect(), v.iter().map(|p| p.z).collect());
    mesh::view::create_packed_owned(None, Some(indices), Some(planes(&normals)), None, None, planes(&normals)).expect("sphere")
}

fn gpu_rays_world_furnace() {
    if !common::gpu() {
        return;
    }
    common::release_keepalive();
    let baseline = live_handles();
    let radiance = 0.5;
    let baked = world::bake(Some(1), None, 64, 32, &uniform(radiance)).expect("bake");
    let cam = camera::perspective(None, None, None, vec3::create(0., 0., 4.), vec3::zero()).expect("camera");
    let sphere_node = sphere();
    let scene_ = |baked: &world::baked| {
        vec![scene::clear(color::black()), scene::view3d(None, &cam, d3::with_world(baked.clone(), &scene_of(vec![sphere_node.clone()])))]
    };
    let canvas_ = canvas::create(64, 64).expect("canvas");
    let uploaded = || stats(&canvas_).uploaded_bytes;
    let pixel = |x: i64, y: i64| canvas::pixel(&canvas_, x, y).expect("pixel").expect("in bounds");
    // Furnace: a white Lambertian under constant L reflects L; the camera map behind it is L too,
    // both through the same tone map.
    let expected = tone(radiance, 1.);
    render(&canvas_, None, &scene_(&baked));
    let first = uploaded();
    near(3, "furnace sphere centre", expected, pixel(32, 32));
    near(1, "camera-map background", expected, pixel(1, 1));
    // One upload per bake: an identical frame re-sends no texture.
    render(&canvas_, None, &scene_(&baked));
    let delta = uploaded() - first;
    require(delta < 16384, &format!("World maps re-uploaded: {delta} bytes"));
    // Color background paints that radiance; only background changed, so the shared maps stay
    // uploaded. Exposure is stops through the tone map.
    let before = uploaded();
    let tinted = world::with_background(&baked, world::background::Color(world::rgb(0.1, 0.1, 0.1)), Some(1.));
    render(&canvas_, None, &scene_(&tinted));
    let delta = uploaded() - before;
    require(delta < 16384, &format!("background change re-uploaded maps: {delta} bytes"));
    near(1, "color background", tone(0.1, 2.), pixel(1, 1));
    near(3, "exposed sphere", tone(radiance, 2.), pixel(32, 32));
    let clear = world::with_background(&baked, world::background::Transparent, None);
    render(&canvas_, None, &scene_(&clear));
    require(pixel(1, 1) == color::black(), "transparent background keeps the clear");
    let _ = canvas::destroy(&canvas_);
    // Degenerate drawings draw nothing instead of failing the frame: an empty mesh (a cook with
    // no triangles), a zero-length line, a zero circle.
    let canvas_ = canvas::create(32, 32).expect("canvas");
    let empty = mesh::view::create_owned(Some(mesh::mode::Triangles), Some(Vec::new()), &[]).expect("empty mesh");
    render(
        &canvas_,
        None,
        &[scene::view3d(None, &cam, scene_of(vec![d3::mesh(None, None, None, None, None, empty), sphere()]))],
    );
    render(
        &canvas_,
        None,
        &[scene::line((4, 4), (4, 4), Some(color::white()), Some(1)).expect("line"), scene::circle((8, 8), 0, Some(color::white()), None).expect("circle")],
    );
    let _ = canvas::destroy(&canvas_);
    assert_eq!(live_handles(), baseline, "World raster native handle delta");
}

/// A World sun straight down (+Z here) through the GPU sun map: the plane under a floating box is
/// darker than beside it, the same scene without a sun shows no patch, and an identical re-render
/// reuses the map.
fn gpu_rays_world_sun_shadow() {
    if !common::gpu() {
        return;
    }
    common::release_keepalive();
    let baseline = live_handles();
    let baked = world::bake(Some(1), None, 64, 32, &uniform(0.2)).expect("bake");
    let sun = world::sun { direction: vec3::unit_z(), radiance: world::rgb(2000., 2000., 2000.), angular_radius: 0.02 };
    let cam = camera::perspective(None, None, None, vec3::create(0., -4., 5.), vec3::zero()).expect("camera");
    let matte = material::matte(color::white());
    let plane = d3::mesh(Some(matte.clone()), None, None, None, None, mesh::plane(8., 8.).expect("plane"));
    let box_ = d3::mesh(Some(matte), None, None, None, None, box_mesh());
    // Rebuilt around the same meshes, a scene keeps its sun map.
    let scene_ = |baked: &world::baked| {
        vec![
            scene::clear(color::black()),
            scene::view3d(
                None,
                &cam,
                d3::with_world(baked.clone(), &scene_of(vec![plane.clone(), d3::translate(vec3::create(0., 0., 1.5), vec![box_.clone()])])),
            ),
        ]
    };
    let canvas_ = canvas::create(128, 128).expect("canvas");
    let luminance = |p: vec3::T| {
        let s = camera::world_to_screen((0, 0, 128, 128), &cam, p).expect("projection").expect("off screen");
        let c = canvas::pixel(&canvas_, s.x as i64, s.y as i64).expect("pixel").expect("in bounds");
        c.r + c.g + c.b
    };
    let under = vec3::create(0., 0.2, 0.);
    let beside = vec3::create(2.5, 0., 0.);
    let passes = || stats(&canvas_).sun_shadow_passes;
    let with_sun = |s: &world::sun| world::with_sun(&baked, Some(s.clone()));
    let lit = scene_(&with_sun(&sun));
    render(&canvas_, None, &lit);
    let (shadowed, open) = (luminance(under), luminance(beside));
    require(shadowed + 150 <= open, &format!("no sun shadow under the box: {shadowed} vs {open} beside"));
    let first = passes();
    require(first == 1, &format!("{first} sun map passes, expected 1"));
    render(&canvas_, None, &lit);
    render(&canvas_, None, &scene_(&with_sun(&sun)));
    require(passes() == first, "identical scene re-rendered the sun map");
    let tilted = world::sun { direction: vec3::normalize(vec3::create(0.3, 0., 1.)), ..sun.clone() };
    render(&canvas_, None, &scene_(&with_sun(&tilted)));
    require(passes() == first + 1, "a moved sun kept the old sun map");
    render(&canvas_, None, &scene_(&baked));
    let (a, b) = (luminance(under), luminance(beside));
    require((a - b).abs() <= 3, &format!("sunless World shows a patch: {a} vs {b}"));
    let _ = canvas::destroy(&canvas_);
    assert_eq!(live_handles(), baseline, "World sun shadow native handle delta");
}

fn box_mesh() -> mesh::Mesh {
    let half = 0.5;
    let faces = [
        (vec3::unit_x(), vec3::unit_y(), vec3::unit_z()),
        (vec3::neg(vec3::unit_x()), vec3::unit_z(), vec3::unit_y()),
        (vec3::unit_y(), vec3::unit_z(), vec3::unit_x()),
        (vec3::neg(vec3::unit_y()), vec3::unit_x(), vec3::unit_z()),
        (vec3::unit_z(), vec3::unit_x(), vec3::unit_y()),
        (vec3::neg(vec3::unit_z()), vec3::unit_y(), vec3::unit_x()),
    ];
    let (mut positions, mut normals, mut indices) = (Vec::new(), Vec::new(), Vec::new());
    for (face, (n, u, v)) in faces.iter().enumerate() {
        for (su, sv) in [(-1., -1.), (1., -1.), (1., 1.), (-1., 1.)] {
            let p = vec3::add(*n, vec3::add(vec3::scale(*u, su), vec3::scale(*v, sv)));
            positions.push(vec3::create(p.x * half, p.y * half, p.z * half));
            normals.push(*n);
        }
        let o = face as i64 * 4;
        indices.extend_from_slice(&[o, o + 1, o + 2, o, o + 2, o + 3]);
    }
    let planes = |v: &[vec3::T]| mesh::Vec3_buffer::of_planes(v.iter().map(|p| p.x).collect(), v.iter().map(|p| p.y).collect(), v.iter().map(|p| p.z).collect());
    mesh::view::create_packed_owned(None, Some(indices), Some(planes(&normals)), None, None, planes(&positions)).expect("box")
}

/// Camera orbit preserves the uploaded Scene3 geometry.
fn gpu_rays_lowering_orbit_uploads() {
    if !common::gpu() {
        return;
    }
    let canvas_ = canvas::create(64, 64).expect("canvas");
    let count = 4096_i64;
    let points: Vec<vec3::T> = (0..count).map(|i| vec3::create(((i % 64) as f64 / 32.) - 1., ((i / 64) as f64 / 32.) - 1., 0.)).collect();
    let indices: Vec<i64> = (0..(count - 2) * 3).map(|i| if i % 3 == 0 { 0 } else { (i / 3) + (i % 3) }).collect();
    let planes = mesh::Vec3_buffer::of_planes(points.iter().map(|p| p.x).collect(), points.iter().map(|p| p.y).collect(), points.iter().map(|p| p.z).collect());
    let normals = mesh::Vec3_buffer::of_planes(vec![0.; count as usize], vec![0.; count as usize], vec![1.; count as usize]);
    let heavy = mesh::view::create_packed_owned(None, Some(indices), Some(normals), None, None, planes).expect("heavy");
    let world_ = scene_of(vec![d3::mesh(Some(material::unlit(color::red())), None, None, None, None, heavy)]);
    let cam_at = |p: vec3::T| camera::orthographic(2., p, vec3::zero()).expect("camera");
    render(&canvas_, None, &[scene::clear(color::black()), scene::view3d(Some((0, 0, 64, 64)), &cam_at(vec3::create(0., 0., 2.)), world_.clone())]);
    let after_first = stats(&canvas_).uploaded_bytes;
    render(&canvas_, None, &[scene::clear(color::black()), scene::view3d(Some((0, 0, 64, 64)), &cam_at(vec3::create(0.2, 0., 2.)), world_)]);
    let delta = stats(&canvas_).uploaded_bytes - after_first;
    require(delta <= 32_768, &format!("camera orbit reuploaded Scene3 vertices ({delta} bytes)"));
    let _ = canvas::destroy(&canvas_);
}

/// A recoloured triangle uploads only its 36-byte attribute stream and coalesces identically.
fn gpu_rays_float32_attribute_stream() {
    if !common::gpu() {
        return;
    }
    let (cam, scene_context) = (camera::orthographic(2., vec3::create(0., 0., 2.), vec3::zero()).expect("camera"), layer::create_cache_context());
    let mut done = false;
    layer::with_cache_context(&scene_context, &mut || {
        let colors = vec![color::rgb(255, 0, 0), color::rgb(0, 255, 0), color::rgb(0, 0, 255)];
        let vertices = mesh::Vec3_buffer::of_planes(vec![-0.70000001, 0.60000001, 0.10000001], vec![-0.60000001, -0.50000001, 0.80000001], vec![0., 0., 0.]);
        let normals = mesh::Vec3_buffer::of_planes(vec![0., 0., 0.], vec![0., 0., 0.], vec![1., 1., 1.]);
        let indices = Arc::new(vec![0u32, 1, 2]);
        let make = |colors: Vec<color::T>| {
            mesh::view::create_packed_shared(None, Some(indices.clone()), Some(normals.clone()), Some(Arc::new(colors)), None, vertices.clone()).expect("mesh")
        };
        let stage = |m: &mesh::Mesh| {
            let s = scene_of(vec![d3::mesh(Some(material::unlit(color::white())), None, None, None, None, m.clone())]);
            layer::stage_native(128, 128, &[scene::view3d(None, &cam, s)]).expect("stage").scene3[0].entries[0].clone()
        };
        let (first, second) = (stage(&make(colors)), stage(&make(vec![color::rgb(255, 255, 0); 3])));
        let configuration = rr::Configuration {
            logical_width: 128,
            logical_height: 128,
            drawable_width: 128,
            drawable_height: 128,
            ..rr::default_configuration()
        };
        let runtime = rr::create_offscreen(&configuration).unwrap();
        let draw = |entry: &se::SampledDraw| {
            rr::prepared_draw(entry.family, Some(entry.blend), entry.texture.clone(), entry.auxiliary.clone(), entry.vertex_attributes.clone(), Some(entry.samples), entry.draw.clone())
        };
        let render_entry = |entry: &se::SampledDraw| {
            rr::draw(Some((0., 0., 0., 1.)), &runtime, &[draw(entry)]).unwrap();
            rr::read_pixels(&runtime, 512).unwrap()
        };
        let current = render_entry(&first);
        let before = rr::stats(&runtime).unwrap().uploaded_bytes;
        let changed = render_entry(&second);
        let uploaded = rr::stats(&runtime).unwrap().uploaded_bytes - before;
        assert!(changed != current, "recolour did not change visible triangle pixels");
        assert!(uploaded == 36, "color edit reuploaded geometry ({uploaded} bytes)");
        let many: Vec<_> = (0..65).map(|_| draw(&second)).collect();
        rr::draw(Some((0., 0., 0., 1.)), &runtime, &many).unwrap();
        let combined = rr::read_pixels(&runtime, 512).unwrap();
        assert!(combined == changed, "coalescing lost the float32 attribute stream");
        let _ = rr::destroy(&runtime);
        done = true;
    });
    assert!(done);
}

/// Native circle rim, packed/singular alpha parity and the 100000-instance single draw.
fn gpu_rays_primitives() {
    if !common::gpu() {
        return;
    }
    let canvas_ = canvas::create(64, 64).expect("canvas");
    let cyan = color::rgb(0, 255, 255);
    let rgba = |c: color::T| ((c.r << 24) | (c.g << 16) | (c.b << 8) | c.a) as u32 as i32;
    let px = |canvas_: &canvas::Canvas| canvas::pixels(canvas_).expect("pixels");
    let mut b = ui_batch::Builder::create();
    b.circle(32., 32., 12., Some(rgba(cyan)), None, 0.).expect("circle");
    let packed = scene::shapes(b.publish().expect("publish"));
    render(&canvas_, None, &[scene::clear(color::black()), scene::circle((32, 32), 12, Some(cyan), None).expect("circle")]);
    let polygon = px(&canvas_);
    render(&canvas_, None, &[scene::clear(color::black()), packed]);
    let sdf = px(&canvas_);
    let boundary: Vec<(f64, f64)> = (0..32)
        .map(|i| {
            let angle = 2. * std::f64::consts::PI * i as f64 / 32.;
            ((32 + (12. * angle.cos()) as i64) as f64, (32 + (12. * angle.sin()) as i64) as f64)
        })
        .collect();
    let distance = |(x, y): (f64, f64), (ax, ay): (f64, f64), (bx, by): (f64, f64)| {
        let (dx, dy) = (bx - ax, by - ay);
        let length2 = dx * dx + dy * dy;
        let t = if length2 == 0. { 0. } else { rays::num::fmax(0., rays::num::fmin(1., ((x - ax) * dx + (y - ay) * dy) / length2)) };
        (x - ax - t * dx).hypot(y - ay - t * dy)
    };
    let mut worst: f64 = 0.;
    for (i, expected) in polygon.iter().enumerate() {
        if sdf[i] != *expected {
            let sample = ((i % 64) as f64 + 0.5, (i / 64) as f64 + 0.5);
            let d = (0..32).map(|k| distance(sample, boundary[k], boundary[(k + 1) % 32])).fold(f64::INFINITY, f64::min);
            worst = worst.max(d);
        }
    }
    require(worst <= 2f64.sqrt(), "circle differs more than one pixel per axis from the reference 32-gon rim");
    let batch = |count: i64| {
        let mut b = ui_batch::Builder::with_capacity(count);
        for i in 0..count {
            b.circle(8. + ((i % 19) as f64 * 2.), 8. + ((i % 13) as f64 * 2.), 3., Some(rgba(color::rgba(40, 120, 180, 70))), None, 0.).expect("circle");
        }
        b.publish().expect("publish")
    };
    let wrap = |nodes: Vec<scene::node>| {
        vec![scene::clip((7, 9), 43, 37, vec![scene::translate(27, 23, vec![scene::rotate(0.37, vec![scene::scale(1.7, 0.8, nodes)])])])]
    };
    for count in [63, 64, 65] {
        let mut frame = vec![scene::clear(color::black())];
        frame.extend(wrap(vec![scene::shapes(batch(count))]));
        render(&canvas_, None, &frame);
        let expected = px(&canvas_);
        require(expected.iter().any(|p| *p != color::black()), "packed circles drew nothing");
        let single: Vec<scene::node> = (0..count)
            .map(|i| {
                let mut one = ui_batch::Builder::with_capacity(1);
                one.circle(8. + ((i % 19) as f64 * 2.), 8. + ((i % 13) as f64 * 2.), 3., Some(rgba(color::rgba(40, 120, 180, 70))), None, 0.).expect("circle");
                scene::shapes(one.publish().expect("publish"))
            })
            .collect();
        let mut frame = vec![scene::clear(color::black())];
        frame.extend(wrap(single));
        render(&canvas_, None, &frame);
        require(px(&canvas_) == expected, "packed/singular transform clip alpha parity");
    }
    for count in [63, 64, 65, 100_000] {
        let mut builder = ui_batch::Builder::with_capacity(count);
        let mut reference = vec![scene::clear(color::black())];
        for i in 0..count {
            let (x, y) = (8 + (2 * (i % 19)), 8 + (2 * (i % 13)));
            builder.shape_rect(x as f64, y as f64, 2., 2., rgba(cyan), 0).expect("rect");
            reference.push(scene::rect((x, y), 2, 2, Some(cyan), None).expect("rect"));
        }
        render(&canvas_, None, &reference);
        let expected = px(&canvas_);
        render(&canvas_, None, &[scene::clear(color::black()), scene::shapes(builder.publish().expect("publish"))]);
        require(px(&canvas_) == expected, &format!("legacy/packed rectangle pixels at {count} instances"));
    }
    let dense = batch(100_000);
    render(&canvas_, None, &[scene::clear(color::black()), scene::shapes(dense.clone())]);
    let before = stats(&canvas_);
    render(&canvas_, None, &[scene::clear(color::black()), scene::shapes(dense)]);
    let after = stats(&canvas_);
    require(after.logical_draws - before.logical_draws == 1, "100000 instances one draw");
    require(after.uploaded_bytes == before.uploaded_bytes, "static shapes reuploaded");
    let _ = canvas::destroy(&canvas_);
}

pub const CASES: &[(&str, fn())] = &[
    ("gpu_rays_primitives", gpu_rays_primitives),
    ("gpu_rays_camera_depth", gpu_rays_camera_depth),
    ("gpu_rays_canvas_resize_density", gpu_rays_canvas_resize_density),
    ("gpu_rays_world_furnace", gpu_rays_world_furnace),
    ("gpu_rays_world_sun_shadow", gpu_rays_world_sun_shadow),
    ("gpu_rays_lowering_orbit_uploads", gpu_rays_lowering_orbit_uploads),
    ("gpu_rays_float32_attribute_stream", gpu_rays_float32_attribute_stream),
];

/// A failed check ends the process with its message (these targets run on the main thread).
fn fail(message: &str) -> ! {
    eprintln!("{message}");
    std::process::exit(101)
}
