//! Port of lib/rays_pathtracer/test_pathtracer.ml. The OCaml `run` is one long function; it is split here
//! into named cases (each its own process under `cargo t`, each under 5 s): `lit` (determinism, preview,
//! camera moves), `instanced` (instancing against the transformed mesh, queue_mesh, handles), `lens`,
//! `residency`, `cap`, `furnace`, `materials_motion`, `strands`. Differences from the OCaml:
//! - geometry comes from tests/common/meshes.rs (no rdk_test_support port yet);
//! - resolutions and sample counts are shrunk to stay under the 5 s budget (noted per case);
//! - `exact_m1` (byte-for-byte digests of two frames recorded on an Apple M1) is not ported: the digests
//!   are of the OCaml/RDK geometry, which these generators do not reproduce.
mod common;
use common::meshes;
use common::pt::{self, frame, frames, get, pixels, rgb};
use rays_math::{mat4, vec3};
use rays_pathtracer::rays_pathtracer as p;
use std::collections::HashSet;

fn metal() -> p::material {
    p::material(Some(0.2), Some(1.), None, None, rgb(0.9, 0.7, 0.4))
}

fn matte() -> p::material {
    p::material(Some(0.8), None, None, None, rgb(0.6, 0.6, 0.6))
}

struct Lit {
    sphere: rays::mesh::t,
    floor: rays::mesh::t,
    scene: p::scene,
}

fn lit_scene() -> Lit {
    let sphere = meshes::uv_sphere(vec3::create(0., 1., 0.), 24, 12, 1.);
    let floor = meshes::box_mesh(vec3::create(0., -0.05, 0.), vec3::create(20., 0.1, 20.));
    let scene = p::scene {
        objects: vec![(sphere.clone(), metal()), (floor.clone(), matte())],
        spheres: vec![],
        strands: vec![],
        environment: p::environment { sky: rgb(0.5, 0.6, 0.8), ground: rgb(0.2, 0.2, 0.2), panels: vec![p::panel(None, 6., 0.5, 0.4, vec3::create(1., 2., 1.))] },
        lights: vec![p::rect_light(20., (2., 2.), vec3::zero(), vec3::create(-2., 5., 2.))],
    };
    Lit { sphere, floor, scene }
}

fn lit_camera() -> rays::camera::t {
    rays::camera::perspective(Some(0.9), None, None, vec3::create(0., 2., 6.), vec3::create(0., 0.8, 0.)).unwrap()
}

fn prepared(mesh: &rays::mesh::t, material: p::material) -> p::mesh {
    get(p::mesh(None, None, &[(mesh.clone(), material)]))
}

/// `capture`: settle a camera change before resetting, so its preview cannot change the counted sample
/// total in a deterministic comparison.
fn capture(tracer: &mut p::t, camera: &rays::camera::t, count: i64) -> Vec<u8> {
    frames(tracer, camera, 2);
    p::reset(tracer);
    frames(tracer, camera, count);
    pixels(tracer)
}

/// A new tracer's first two frames from reset: no preview, four samples.
fn lit() {
    if !common::gpu() {
        return;
    }
    assert!(p::unavailable(&ogpu::caps::minimum_m1()).is_some());
    let mut caps = ogpu::caps::minimum_m1();
    caps.ray_tracing = true;
    assert!(p::unavailable(&caps).is_none());
    let initial_handles = pt::live_handles();
    let l = lit_scene();
    let mut tracer = get(p::create(&pt::options(2), 48, 32, &l.scene));
    let camera = lit_camera();
    let unsupported = rays::camera::orthographic(4., rays::camera::position(&camera), rays::camera::target(&camera)).unwrap();
    assert!(p::render(&mut tracer, &unsupported).is_err(), "orthographic path-tracer camera was accepted");
    assert_eq!(p::samples(&tracer), 0);
    p::reset(&mut tracer);
    frames(&mut tracer, &camera, 2);
    let cold = pixels(&tracer);
    assert_eq!(p::samples(&tracer), 4);
    let run = |tracer: &mut p::t| capture(tracer, &camera, 2);
    let first = run(&mut tracer);
    let second = run(&mut tracer);
    assert!(first == second && first.len() == 48 * 32 * 4);
    // A settled camera accumulates the frame the new tracer rendered.
    assert!(first == cold);
    assert_eq!(p::samples(&tracer), 4);
    let mut distinct = HashSet::new();
    for pixel in 0..48 * 32 {
        assert_eq!(first[pixel * 4 + 3], 255);
        distinct.insert(u32::from_le_bytes(first[pixel * 4..pixel * 4 + 4].try_into().unwrap()));
    }
    assert!(distinct.len() > 8);
    let moved = rays::camera::with_position(vec3::create(0.4, 2., 6.), &camera).unwrap();
    frame(&mut tracer, &moved);
    assert_eq!(p::samples(&tracer), 0);
    let preview = pixels(&tracer);
    let at = |dx: usize, dy: usize, x: usize, y: usize| u32::from_le_bytes(preview[(((2 * y + dy) * 48) + 2 * x + dx) * 4..][..4].try_into().unwrap());
    let mut varying_block = false;
    for y in 0..16 {
        for x in 0..24 {
            if at(0, 0, x, y) != at(1, 0, x, y) || at(0, 0, x, y) != at(0, 1, x, y) || at(0, 0, x, y) != at(1, 1, x, y) {
                varying_block = true;
            }
        }
    }
    assert!(varying_block);
    p::reset(&mut tracer);
    frame(&mut tracer, &camera);
    p::reset(&mut tracer);
    frame(&mut tracer, &moved);
    assert!(pixels(&tracer) != preview);
    // A camera move in the middle of an accumulation is deterministic too.
    let motion_run = |tracer: &mut p::t| {
        p::reset(tracer);
        frame(tracer, &camera);
        frame(tracer, &camera);
        frame(tracer, &moved);
        pixels(tracer)
    };
    let a = motion_run(&mut tracer);
    assert!(a == motion_run(&mut tracer));
    frame(&mut tracer, &moved);
    assert_eq!(p::samples(&tracer), 2);
    p::destroy(&mut tracer);
    drop(tracer);
    assert_eq!(pt::live_handles(), initial_handles);
    println!("pathtracer: deterministic lit render ok");
}

/// Depth of field: a lens blurs the image deterministically; a pinhole lens restores the exact pinhole
/// frame.
fn lens() {
    if !common::gpu() {
        return;
    }
    let l = lit_scene();
    let mut tracer = get(p::create(&pt::options(2), 48, 32, &l.scene));
    let camera = lit_camera();
    let pinhole = capture(&mut tracer, &camera, 2);
    let lensed = rays::camera::with_lens(rays::camera::lens { aperture: 0.2, focus_distance: Some(2.) }, &camera).unwrap();
    let blurred = capture(&mut tracer, &lensed, 16);
    assert!(blurred != pinhole && capture(&mut tracer, &lensed, 16) == blurred);
    assert!(capture(&mut tracer, &camera, 2) == pinhole);
    p::destroy(&mut tracer);
    println!("pathtracer: depth of field ok");
}

/// Instancing against the same mesh transformed, queue_mesh superseding, and handle stability.
fn instanced() {
    if !common::gpu() {
        return;
    }
    let initial_handles = pt::live_handles();
    let mut l = lit_scene();
    // one material class throughout (the OCaml scene mixes a metal sphere with the matte instances): every
    // distinct shape costs a kernel front end, and this case needs only the instanced and the flat one
    l.scene.objects = vec![(l.sphere.clone(), pt::material(rgb(0.7, 0.5, 0.3))), (l.floor.clone(), matte())];
    let mut tracer = get(p::create(&pt::options(2), 48, 32, &l.scene));
    let camera = lit_camera();
    let matrix = mat4::mul(&mat4::translation(vec3::create(0.3, 0., 0.)), &mat4::mul(&mat4::rotation_y(0.23), &mat4::scaling(vec3::create(1.3, 0.8, 1.1))));
    let instance_material = pt::material(rgb(0.7, 0.5, 0.3));
    let instance_prototype = prepared(&l.sphere, instance_material.clone());
    let instanced = get(p::mesh_instanced(&instance_prototype, None, None, &[matrix.clone()]));
    get(p::replace_mesh(&mut tracer, &instanced));
    frames(&mut tracer, &camera, 2);
    let instanced_pixels = pixels(&tracer);
    let transformed = meshes::transform(&matrix, &l.sphere);
    let transformed_mesh = || prepared(&transformed, instance_material.clone());
    get(p::replace_mesh(&mut tracer, &transformed_mesh()));
    frames(&mut tracer, &camera, 2);
    let transformed_pixels = pixels(&tracer);
    assert_eq!(instanced_pixels.len(), transformed_pixels.len());
    let largest = instanced_pixels.iter().zip(&transformed_pixels).map(|(a, b)| (*a as i64 - *b as i64).abs()).max().unwrap();
    assert!(largest <= 2, "instanced vs transformed differ by {largest}");
    let expected = transformed_pixels;
    get(p::queue_mesh(&mut tracer, &instanced));
    get(p::queue_mesh(&mut tracer, &transformed_mesh()));
    get(p::flush(&mut tracer));
    assert_eq!(p::samples(&tracer), 0);
    frame(&mut tracer, &camera);
    assert_eq!(p::samples(&tracer), 0);
    // an edit's first frame is a preview
    frame(&mut tracer, &camera);
    // The last queued mesh wins and renders as when it was replaced directly.
    assert!(pixels(&tracer) == expected);
    let mut stable = None;
    for index in 1..=10 {
        let matrix = mat4::mul(&mat4::translation(vec3::create(index as f64 * 0.02, 0., 0.)), &mat4::scaling(vec3::create(1.3, 0.8, 1.1)));
        get(p::queue_mesh(&mut tracer, &get(p::mesh_instanced(&instance_prototype, None, None, &[matrix]))));
        get(p::flush(&mut tracer));
        if index == 1 {
            stable = Some(pt::live_handles());
        }
    }
    assert!(pt::live_handles() <= stable.unwrap());
    p::destroy(&mut tracer);
    drop(tracer);
    assert_eq!(pt::live_handles(), initial_handles);
    println!("pathtracer: instancing, queue_mesh, stable handles ok");
}

/// An instance structure only references its BLAS: without declaring it read each dispatch, the device
/// evicts it and later frames get darker. (Shrunk: 160x160 and 20 frames; OCaml 320x320 and 60.)
fn residency() {
    if !common::gpu() {
        return;
    }
    let floor = meshes::box_mesh(vec3::create(0., -0.05, 0.), vec3::create(20., 0.1, 20.));
    let big = meshes::uv_sphere(vec3::zero(), 96, 48, 0.4);
    let grid: Vec<mat4::T> = (0..64).map(|i| mat4::translation(vec3::create((i % 8) as f64 - 3.5, (i / 8) as f64 - 3.5, 0.))).collect();
    let lattice_material = p::material(Some(0.6), None, None, None, rgb(0.8, 0.8, 0.8));
    let lattice = get(p::mesh_instanced(&prepared(&big, lattice_material), None, None, &grid));
    let scene = p::scene {
        objects: vec![(floor, pt::material(rgb(0.5, 0.5, 0.5)))],
        spheres: vec![],
        strands: vec![],
        environment: p::environment { sky: rgb(0.02, 0.02, 0.02), ground: rgb(0.02, 0.02, 0.02), panels: vec![] },
        lights: vec![p::rect_light(12., (8., 8.), vec3::zero(), vec3::create(-6., 8., 8.))],
    };
    let mut tracer = get(p::create(&p::create_options { spp: Some(1), bounces: Some(2), ..Default::default() }, 160, 160, &scene));
    get(p::replace_mesh(&mut tracer, &get(p::scene_mesh(&[(mat4::identity(), lattice)]))));
    let camera = rays::camera::perspective(Some(0.7), None, None, vec3::create(0., 0., 12.), vec3::zero()).unwrap();
    let fresh = |tracer: &mut p::t| {
        p::reset(tracer);
        frame(tracer, &camera);
        let sum: i64 = pixels(tracer).iter().enumerate().filter(|(i, _)| i % 4 != 3).map(|(_, v)| *v as i64).sum();
        assert!(sum > 0);
        sum
    };
    let first = fresh(&mut tracer);
    frames(&mut tracer, &camera, 20);
    let later = fresh(&mut tracer);
    p::destroy(&mut tracer);
    assert!((later - first).abs() * 50 <= first, "instanced scene faded: fresh frame sum {first} then {later}");
    println!("pathtracer: instance structure stays resident");
}

/// A sample cap reached at a larger size: every frame completes with no device loss (validation is fatal),
/// the film changed from its first frame and settled toward its mean. (Shrunk to 320x180; OCaml 960x540.)
fn cap() {
    if !common::gpu() {
        return;
    }
    let l = lit_scene();
    let mut tracer = get(p::create(&pt::options(16), 320, 180, &l.scene));
    let camera = lit_camera();
    let distance = |a: &[u8], b: &[u8]| a.iter().zip(b).map(|(x, y)| (*x as i64 - *y as i64).abs()).sum::<i64>();
    frame(&mut tracer, &camera);
    let first = pixels(&tracer);
    while p::samples(&tracer) < 48 {
        frame(&mut tracer, &camera);
    }
    let near = pixels(&tracer);
    while p::samples(&tracer) < 64 {
        frame(&mut tracer, &camera);
    }
    assert_eq!(p::samples(&tracer), 64);
    let last = pixels(&tracer);
    assert!(distance(&first, &last) > 0);
    assert!(distance(&near, &last) * 2 < distance(&first, &last));
    p::destroy(&mut tracer);
    println!("pathtracer: 64 samples accumulate at 320x180");
}

/// Furnace: an albedo-0.8 floor under a uniform white dome reflects exactly 0.8 along every path, which the
/// ACES/2.2 resolve maps to 224. A zero-intensity panel exercises the MIS path (panel sampling plus the
/// weighted BSDF escape) and must not change the mean. The analytic sphere in the furnace conserves energy
/// like the floor, which checks sphere hits, normals, and the table binding.
fn furnace_mean(scene: &p::scene, camera: &rays::camera::t) -> f64 {
    let mut tracer = get(p::create(&pt::options(4), 32, 32, scene));
    frames(&mut tracer, camera, 16);
    get(p::flush(&mut tracer));
    let sum: i64 = pixels(&tracer).iter().step_by(4).map(|v| *v as i64).sum();
    p::destroy(&mut tracer);
    sum as f64 / 1024.
}

fn furnace() {
    if !common::gpu() {
        return;
    }
    let floor = meshes::box_mesh(vec3::create(0., -0.05, 0.), vec3::create(20., 0.1, 20.));
    let white = rgb(1., 1., 1.);
    let furnace_scene = |panels: Vec<p::panel>| p::scene {
        objects: vec![(floor.clone(), p::material(Some(0.8), None, None, None, rgb(0.8, 0.8, 0.8)))],
        spheres: vec![],
        strands: vec![],
        environment: p::environment { sky: white, ground: white, panels },
        lights: vec![],
    };
    let camera = rays::camera::perspective(Some(0.5), None, None, vec3::create(0., 4., 0.001), vec3::zero()).unwrap();
    let plain = furnace_mean(&furnace_scene(vec![]), &camera);
    let mis = furnace_mean(&furnace_scene(vec![p::panel(Some(0.2), 0., 0.9, 0.6, vec3::create(0.3, 1., 0.2))]), &camera);
    println!("pathtracer: furnace mean {plain:.1} / {mis:.1} (expected 224)");
    assert!((plain - 224.).abs() < 2.);
    assert!((mis - 224.).abs() < 2.);
}

fn sphere_furnace() {
    if !common::gpu() {
        return;
    }
    let white = rgb(1., 1., 1.);
    let scene = p::scene {
        objects: vec![],
        strands: vec![],
        spheres: vec![p::sphere { center: vec3::zero(), radius: 1., sphere_material: p::material(Some(0.8), None, None, None, rgb(0.8, 0.8, 0.8)) }],
        environment: p::environment { sky: white, ground: white, panels: vec![] },
        lights: vec![],
    };
    // Every corner ray must hit the sphere, rather than the white sky.
    let camera = rays::camera::perspective(Some(0.2), None, None, vec3::create(0., 0., 6.), vec3::zero()).unwrap();
    let mean = furnace_mean(&scene, &camera);
    println!("pathtracer: sphere furnace mean {mean:.1} (expected 224)");
    assert!((mean - 224.).abs() < 2.);
}

/// Per-instance materials and motion blur. Two emissive instances of one prototype: the left one red, the
/// right one green, selected through the instance user id. With motion the right instance sweeps to the far
/// right, so its rest position is only partly covered and the far position is hit at all, which a static
/// build never does.
fn materials_motion() {
    if !common::gpu() {
        return;
    }
    let sphere = meshes::uv_sphere(vec3::create(0., 1., 0.), 24, 12, 1.);
    let black = p::environment { sky: rgb(0., 0., 0.), ground: rgb(0., 0., 0.), panels: vec![] };
    let scene = p::scene { objects: vec![(sphere.clone(), pt::material(rgb(0.5, 0.5, 0.5)))], spheres: vec![], strands: vec![], environment: black, lights: vec![] };
    let mut tracer = get(p::create(&pt::options(4), 64, 32, &scene));
    let place = |x: f64| mat4::translation(vec3::create(x, 0., 0.));
    let red = p::material(None, None, Some(rgb(4., 0., 0.)), None, rgb(0., 0., 0.));
    let green = p::material(None, None, Some(rgb(0., 4., 0.)), None, rgb(0., 0., 0.));
    let instance_prototype = prepared(&sphere, pt::material(rgb(0.5, 0.5, 0.5)));
    let static_mesh = get(p::mesh_instanced(&instance_prototype, Some(&[red.clone(), green.clone()]), None, &[place(-2.), place(2.)]));
    let camera = rays::camera::perspective(Some(0.9), None, None, vec3::create(0., 1., 10.), vec3::create(0., 1., 0.)).unwrap();
    let render_mesh = |tracer: &mut p::t, mesh: &p::mesh| {
        get(p::replace_mesh(tracer, mesh));
        p::reset(tracer);
        frames(tracer, &camera, 16);
        pixels(tracer)
    };
    let static_pixels = render_mesh(&mut tracer, &static_mesh);
    let channel_at = |bytes: &[u8], point: vec3::T, channel: usize| {
        let point = rays::camera::world_to_screen((0, 0, 64, 32), &camera, point).unwrap().unwrap();
        let (x, y) = (point.x as usize, point.y as usize);
        bytes[((y * 64) + x) * 4 + channel] as i64
    };
    let left = vec3::create(-2., 1., 0.);
    let right = vec3::create(2., 1., 0.);
    let end_point = vec3::create(4.5, 1., 0.);
    assert!(channel_at(&static_pixels, left, 0) > 200 && channel_at(&static_pixels, left, 1) == 0);
    assert!(channel_at(&static_pixels, right, 1) > 200 && channel_at(&static_pixels, right, 0) == 0);
    let moving = get(p::mesh_instanced(&instance_prototype, Some(&[red.clone(), green.clone()]), Some(&[place(-2.), place(4.5)]), &[place(-2.), place(2.)]));
    let moving_pixels = render_mesh(&mut tracer, &moving);
    assert!(render_mesh(&mut tracer, &moving) == moving_pixels);
    assert_eq!(channel_at(&static_pixels, end_point, 1), 0);
    if p::Private::motion_blurs(&tracer) {
        assert!(channel_at(&moving_pixels, end_point, 1) > 0);
        assert!(channel_at(&moving_pixels, right, 1) > 0 && channel_at(&moving_pixels, right, 1) < channel_at(&static_pixels, right, 1));
    } else {
        // no Ray_tracing_motion: the instance stands sharp at the shutter midpoint, x = 3.25
        assert!(channel_at(&moving_pixels, vec3::create(3.25, 1., 0.), 1) > 200);
        assert!(moving_pixels != static_pixels);
    }
    assert!(p::mesh_instanced(&prepared(&sphere, red.clone()), Some(&[red.clone()]), None, &[place(0.), place(1.)]).is_err(), "mismatched instance material count was accepted");
    assert!(p::mesh_instanced(&prepared(&sphere, red), None, Some(&[place(0.)]), &[place(0.), place(1.)]).is_err(), "mismatched motion transform count was accepted");
    p::destroy(&mut tracer);
    println!("pathtracer: spheres, instance materials, motion ok");
}

/// Strands: native curves where the device has them, else one capsule test per segment.
fn strands() {
    if !common::gpu() {
        return;
    }
    let black = p::environment { sky: rgb(0., 0., 0.), ground: rgb(0., 0., 0.), panels: vec![] };
    let strand_scene = p::scene {
        objects: vec![],
        spheres: vec![],
        strands: vec![p::strand {
            points: vec![vec3::create(-0.6, 0., 0.), vec3::zero(), vec3::create(0.6, 0., 0.)],
            thickness: 0.2,
            strand_material: p::material(None, None, Some(rgb(4., 0., 0.)), None, rgb(0., 0., 0.)),
        }],
        environment: black,
        lights: vec![],
    };
    let mut tracer = get(p::create(&pt::options(1), 32, 32, &strand_scene));
    let camera = rays::camera::perspective(Some(0.5), None, None, vec3::create(0., 0., 4.), vec3::zero()).unwrap();
    frame(&mut tracer, &camera);
    let red = pixels(&tracer).iter().enumerate().filter(|(i, v)| i % 4 == 0 && **v > 200).count();
    assert!(red > 0);
    p::destroy(&mut tracer);
    println!("pathtracer: strands ok");
}

fn main() {
    common::window::run(&[
        ("lit", lit),
        ("lens", lens),
        ("instanced", instanced),
        ("residency", residency),
        ("cap", cap),
        ("furnace", furnace),
        ("sphere_furnace", sphere_furnace),
        ("materials_motion", materials_motion),
        ("strands", strands),
    ]);
}
