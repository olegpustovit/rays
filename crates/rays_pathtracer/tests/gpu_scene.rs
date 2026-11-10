//! Scene tracing covers live updates, instanced structures, mixed analytic spheres and light conversion; each case owns a fresh device and stays within the warm case budget.
use crate::common;
use common::meshes;
use common::pt::{self, frames, get, pixels, rgb};
use rays_math::{mat4, vec3};
use rays_pathtracer::rays_pathtracer as p;
use std::rc::Rc;

fn black() -> p::Environment {
    p::Environment { sky: rgb(0., 0., 0.), ground: rgb(0., 0., 0.), panels: vec![] }
}

fn v3(x: f64, y: f64, z: f64) -> vec3::T {
    vec3::create(x, y, z)
}

fn prepared(geometry: &rays::mesh::Mesh, material: &p::Material) -> p::Mesh {
    get(p::mesh(None, None, &[(geometry.clone(), material.clone())]))
}

/// The kernel's resolve (ACES fit, gamma 2.2, exposure 1) and its inverse.
fn resolve(x: f64) -> f64 {
    rays_math::color::aces(x).max(0.).min(1.).powf(1. / 2.2) * 255.
}

fn unresolve(value: f64) -> f64 {
    let (mut lo, mut hi) = (0., 16.);
    for _ in 0..60 {
        let mid = (lo + hi) / 2.;
        if resolve(mid) < value { lo = mid } else { hi = mid }
    }
    (lo + hi) / 2.
}

struct Objects {
    floor: rays::mesh::Mesh,
    boxm: rays::mesh::Mesh,
    ball: rays::mesh::Mesh,
}

fn objects() -> Objects {
    Objects {
        floor: meshes::box_mesh(v3(0., -0.05, 0.), v3(20., 0.1, 20.)),
        boxm: meshes::box_mesh(v3(0., 0.5, 0.), v3(1., 1., 1.)),
        ball: meshes::uv_sphere(vec3::zero(), 24, 12, 0.4),
    }
}

fn base_scene(o: &Objects) -> p::Scene {
    p::Scene {
        objects: vec![(o.floor.clone(), pt::material(rgb(0.5, 0.5, 0.5)))],
        spheres: vec![],
        strands: vec![],
        environment: p::Environment { sky: rgb(0.5, 0.6, 0.8), ground: rgb(0.2, 0.2, 0.2), panels: vec![] },
        lights: vec![p::rect_light(20., (2., 2.), vec3::zero(), v3(-2., 5., 2.))],
    }
}

fn camera() -> rays::camera::Camera {
    rays::camera::perspective(Some(0.9), None, None, v3(0., 2.5, 6.), v3(0., 0.6, 0.)).unwrap()
}

/// A mesh changing every frame installs while frames keep tracing, with no flush to help it.
fn live_updates() {
    if !common::gpu() {
        return;
    }
    let o = objects();
    let material = pt::material(rgb(0.8, 0.2, 0.2));
    let scene = p::Scene { objects: vec![(o.ball.clone(), material.clone())], spheres: vec![], strands: vec![], environment: black(), lights: vec![] };
    let mut tracer = get(p::create(&Default::default(), 16, 16, &scene));
    let camera = rays::camera::perspective(None, None, None, v3(0., 0., 5.), vec3::zero()).unwrap();
    let meshes_ = [prepared(&o.boxm, &material), prepared(&o.ball, &material)];
    let start = p::private::edits(&tracer);
    let mut frame = 0usize;
    while p::private::edits(&tracer) < start + 2 && frame < 200_000 {
        get(p::queue_mesh(&mut tracer, &meshes_[frame & 1]));
        get(p::render(&mut tracer, &camera));
        frame += 1;
    }
    assert!(p::private::edits(&tracer) >= start + 2, "a live mesh never installed");
    p::destroy(&mut tracer);
}

/// A device mesh whose triangle indices live on the device (no host corners) refits in place while the
/// producer's topology stamp holds, over a fresh index buffer and registration each frame.
fn device_topology_refits() {
    if !common::gpu() {
        return;
    }
    use ogpu_core::backend;
    use ogpu_core::types::{BufferDescriptor, BufferUsage};
    let material = pt::material(rgb(0.8, 0.2, 0.2));
    let scene = p::Scene { objects: vec![(objects().ball, material.clone())], spheres: vec![], strands: vec![], environment: black(), lights: vec![] };
    let mut tracer = get(p::create(&Default::default(), 16, 16, &scene));
    let camera = rays::camera::perspective(None, None, None, v3(0., 0., 5.), vec3::zero()).unwrap();
    let device = renderer::renderer::gpu_device(&tracer.lease);
    let make = |label: &str, bytes: &[u8]| {
        let usage = vec![BufferUsage::Vertex, BufferUsage::Index, BufferUsage::Storage, BufferUsage::CopyDst];
        let buffer = backend::create_buffer(&device, BufferDescriptor { label: Some(label.to_owned()), size: bytes.len() as i64, usage }, None).unwrap();
        backend::write_buffer(&buffer, 0, bytes).unwrap();
        buffer
    };
    let floats = |values: &[f32]| values.iter().flat_map(|v| v.to_le_bytes()).collect::<Vec<u8>>();
    let normals = make("topology-normals", &floats(&[0., 0., 1., 0., 0., 1., 0., 0., 1., 0., 0., 1.]));
    let mut held = vec![normals.clone()];
    let mut unregisters = vec![];
    for frame in 0..3 {
        let z = frame as f32 * 0.1;
        let positions = make("topology-positions", &floats(&[-1., -1., z, 1., -1., z, 1., 1., z, -1., 1., z]));
        let indices = make("topology-indices", &[0u32, 1, 2, 0, 2, 3].iter().flat_map(|v| v.to_le_bytes()).collect::<Vec<u8>>());
        let (key, unregister) = renderer::renderer::register_gpu_positions(&tracer.lease, &positions, Some(normals.clone()), Some((indices.clone(), 6)), None, 4, 1, Rc::new(|| true)).unwrap();
        held.extend([positions, indices]);
        unregisters.push(unregister);
        let mesh = rays::mesh::view::create_device(
            rays::mesh::view::device_args { device_index_count: Some(6), key, normals: true, ..Default::default() },
            4,
        )
        .unwrap();
        let start = p::private::edits(&tracer);
        get(p::queue_mesh(&mut tracer, &prepared(&mesh, &material)));
        let mut spins = 0;
        while p::private::edits(&tracer) <= start && spins < 200_000 {
            get(p::render(&mut tracer, &camera));
            spins += 1;
        }
        assert!(p::private::edits(&tracer) > start, "device mesh frame {frame} never installed");
    }
    assert_eq!(p::private::refits(&tracer), 2, "same-stamp GPU-topology frames must refit, not rebuild");
    p::destroy(&mut tracer);
    unregisters.iter().for_each(|u| u());
    held.iter().for_each(|b| backend::destroy_buffer(b).unwrap());
}

fn mean_diff(a: &[u8], b: &[u8]) -> (f64, usize) {
    let (mut total, mut outliers) = (0i64, 0usize);
    for (i, (x, y)) in a.iter().zip(b).enumerate() {
        if i % 4 != 3 {
            let d = (*x as i64 - *y as i64).abs();
            total += d;
            if d > 8 {
                outliers += 1;
            }
        }
    }
    (total as f64 / (64 * 48 * 3) as f64, outliers)
}

fn scene_mesh() {
    if !common::gpu() {
        return;
    }
    let initial = pt::live_handles();
    let o = objects();
    let mut tracer = get(p::create(&pt::options(2), 64, 48, &base_scene(&o)));
    let camera = camera();
    // 1. Two objects at world transforms, one flat and one instanced with per-instance materials, against the
    // same meshes composed by hand.
    let (red, green) = (pt::material(rgb(0.8, 0.2, 0.2)), pt::material(rgb(0.2, 0.8, 0.2)));
    let blue = p::material(Some(0.2), None, None, None, rgb(0.2, 0.2, 0.8));
    let gray = pt::material(rgb(0.5, 0.5, 0.5));
    let t1 = mat4::mul(&mat4::translation(v3(-1.2, 0., 0.)), &mat4::rotation_y(0.4));
    let t2 = mat4::mul(&mat4::translation(v3(1., 0., 0.)), &mat4::rotation_y(-0.3));
    let a = mat4::translation(v3(0., 0.4, 0.));
    let b = mat4::mul(&mat4::translation(v3(0.3, 1.3, 0.)), &mat4::scaling(v3(0.8, 0.8, 0.8)));
    let scene_of = |t1: &mat4::T| {
        get(p::scene_mesh(&[
            (t1.clone(), prepared(&o.boxm, &red)),
            (t2.clone(), get(p::mesh_instanced(&prepared(&o.ball, &gray), Some(&[green.clone(), blue.clone()]), None, &[a.clone(), b.clone()]))),
            (mat4::identity(), prepared(&o.floor, &gray)),
        ]))
    };
    let composed = get(p::mesh(
        None,
        None,
        &[
            (meshes::transform(&t1, &o.boxm), red.clone()),
            (meshes::transform(&mat4::mul(&t2, &a), &o.ball), green.clone()),
            (meshes::transform(&mat4::mul(&t2, &b), &o.ball), blue.clone()),
            (o.floor.clone(), gray.clone()),
        ],
    ));
    let placed = scene_of(&t1);
    let render = |tracer: &mut p::PathTracer, frames_: i64| {
        p::reset(tracer);
        frames(tracer, &camera, frames_);
        pixels(tracer)
    };
    get(p::replace_mesh(&mut tracer, &placed));
    let (unpacked, packed) = p::private::scene_compaction(&tracer);
    println!("scene BLASes: {unpacked} -> {packed} bytes compacted");
    assert!(packed > 0 && packed < unpacked);
    let scene_pixels = render(&mut tracer, 4);
    assert!(scene_pixels == render(&mut tracer, 4));
    let capture = |tracer: &mut p::PathTracer, mesh: &p::Mesh| {
        get(p::replace_mesh(tracer, mesh));
        render(tracer, 2)
    };
    let placed_pixels = capture(&mut tracer, &placed);
    let composed_pixels = capture(&mut tracer, &composed);
    assert!(placed_pixels.len() == 64 * 48 * 4 && composed_pixels.len() == placed_pixels.len());
    let (mean, outliers) = mean_diff(&placed_pixels, &composed_pixels);
    println!("pathtracer scene: vs composed mean |diff| {mean:.3}, {outliers} outliers");
    assert!(mean < 1.0 && outliers < 64 * 48 * 3 / 100);
    // A translated object moves in the image: the box's red excess centroid shifts right.
    let shifted_pixels = capture(&mut tracer, &scene_of(&mat4::mul(&mat4::translation(v3(0.8, 0., 0.)), &t1)));
    let red_center = |bytes: &[u8]| {
        let (mut sum, mut weighted) = (0i64, 0i64);
        for pixel in 0..64 * 48 {
            let channel = |i: usize| bytes[pixel * 4 + i] as i64;
            let weight = (channel(0) - channel(1).max(channel(2))).max(0);
            sum += weight;
            weighted += weight * (pixel as i64 % 64);
        }
        assert!(sum > 0);
        weighted as f64 / sum as f64
    };
    let (before, after) = (red_center(&placed_pixels), red_center(&shifted_pixels));
    println!("pathtracer scene: red centroid {before:.1} -> {after:.1}");
    assert!(after > before + 3.);
    assert!(p::scene_mesh(&[]).is_err(), "empty scene accepted");
    let with_sphere = get(p::mesh(Some(&[p::Sphere { center: vec3::zero(), radius: 1., sphere_material: red.clone() }]), None, &[]));
    assert!(p::scene_mesh(&[(mat4::identity(), with_sphere)]).is_err(), "scene object with spheres accepted");
    // Nested: a scene is itself an object.
    get(p::replace_mesh(&mut tracer, &get(p::scene_mesh(&[(mat4::identity(), placed.clone())]))));
    assert!(scene_pixels == render(&mut tracer, 4));
    // move: re-placing objects over the built BLASes matches a fresh scene_mesh at the new transforms,
    // directly, superseding a pending move, and queued behind a full build.
    let t1b = mat4::mul(&mat4::translation(v3(0.8, 0., 0.)), &t1);
    let same = |name: &str, pixels: &[u8]| {
        let total: i64 = pixels.iter().zip(&shifted_pixels).enumerate().filter(|(i, _)| i % 4 != 3).map(|(_, (x, y))| (*x as i64 - *y as i64).abs()).sum();
        let mean = total as f64 / (64 * 48 * 3) as f64;
        println!("pathtracer scene: move ({name}) vs scene_mesh mean |diff| {mean:.3}");
        assert!(mean < 0.5);
    };
    get(p::replace_mesh(&mut tracer, &placed));
    get(p::r#move(&mut tracer, &[t1b.clone(), t2.clone(), mat4::identity()]));
    get(p::flush(&mut tracer));
    same("direct", &render(&mut tracer, 2));
    get(p::r#move(&mut tracer, &[t1.clone(), t2.clone(), mat4::identity()]));
    get(p::r#move(&mut tracer, &[t1b.clone(), t2.clone(), mat4::identity()]));
    get(p::flush(&mut tracer));
    same("superseded", &render(&mut tracer, 2));
    get(p::queue_mesh(&mut tracer, &placed));
    get(p::r#move(&mut tracer, &[t1b.clone(), t2.clone(), mat4::identity()]));
    get(p::flush(&mut tracer));
    same("queued", &render(&mut tracer, 2));
    assert!(p::r#move(&mut tracer, &[t1.clone()]).is_err(), "move with a wrong count accepted");
    // An edit renders like a camera move: once a queued swap installs, the next frame is an uncounted
    // preview, and the one after accumulates.
    pt::frame(&mut tracer, &camera);
    get(p::queue_mesh(&mut tracer, &placed));
    get(p::flush(&mut tracer));
    pt::frame(&mut tracer, &camera);
    assert_eq!(p::samples(&tracer), 0);
    pt::frame(&mut tracer, &camera);
    assert_eq!(p::samples(&tracer), 2);
    p::destroy(&mut tracer);
    drop(tracer);
    assert_eq!(pt::live_handles(), initial);
}

/// ~2000 instances of a 12-triangle box plus a 20k-triangle mesh, a full scene_mesh + queue_mesh + flush
/// against move + flush.
fn timing() {
    if !common::gpu() {
        return;
    }
    let o = objects();
    let mut tracer = get(p::create(&pt::options(2), 64, 48, &base_scene(&o)));
    let red = pt::material(rgb(0.8, 0.2, 0.2));
    let blue = p::material(Some(0.2), None, None, None, rgb(0.2, 0.2, 0.8));
    let boxes = get(p::mesh_instanced(&prepared(&o.boxm, &red), None, None, &(0..2000).map(|i| mat4::translation(v3((i % 50) as f64 * 0.3 - 7.5, 0., (i / 50) as f64 * -0.3))).collect::<Vec<_>>()));
    let dense = get(p::mesh(None, None, &[(meshes::uv_sphere(vec3::zero(), 100, 101, 0.5), blue)]));
    let at = |x: f64| mat4::translation(v3(x, 1., 0.));
    let time = |f: &mut dyn FnMut()| {
        let start = std::time::Instant::now();
        f();
        start.elapsed().as_secs_f64() * 1000.
    };
    let runs = 3;
    get(p::queue_mesh(&mut tracer, &get(p::scene_mesh(&[(mat4::identity(), boxes.clone()), (at(0.), dense.clone())]))));
    get(p::flush(&mut tracer));
    let full: Vec<f64> = (0..runs)
        .map(|i| {
            time(&mut || {
                get(p::queue_mesh(&mut tracer, &get(p::scene_mesh(&[(mat4::identity(), boxes.clone()), (at(i as f64 * 0.1), dense.clone())]))));
                get(p::flush(&mut tracer));
            })
        })
        .collect();
    let moves: Vec<f64> = (0..runs)
        .map(|i| {
            time(&mut || {
                get(p::r#move(&mut tracer, &[mat4::identity(), at(i as f64 * -0.1)]));
                get(p::flush(&mut tracer));
            })
        })
        .collect();
    let median = |mut l: Vec<f64>| {
        l.sort_by(|a, b| a.partial_cmp(b).unwrap());
        l[l.len() / 2]
    };
    let (full, moves) = (median(full), median(moves));
    println!("pathtracer scene: 2001-instance scene: scene_mesh+queue_mesh+flush {full:.2} ms, move+flush {moves:.2} ms (median of {runs})");
    // what a move saves depends on the machine and the GPU's state (both runs end in one flush), so the
    // default run prints the numbers and a qualification run (RAYS_QUALIFY=1) checks them
    if std::env::var_os("RAYS_QUALIFY").is_some() {
        assert!(moves < full);
    }
    p::destroy(&mut tracer);
}

/// One mesh with triangles and analytic spheres builds (one BLAS per kind under a TLAS) and shows both: an
/// emissive red sphere over the lit floor.
fn mixed_spheres() {
    if !common::gpu() {
        return;
    }
    let o = objects();
    let mut tracer = get(p::create(&pt::options(2), 64, 48, &base_scene(&o)));
    let camera = camera();
    let gray = pt::material(rgb(0.5, 0.5, 0.5));
    let spheres = [p::Sphere { center: v3(0., 1., 0.), radius: 0.5, sphere_material: p::material(None, None, Some(rgb(4., 0., 0.)), None, rgb(0., 0., 0.)) }];
    let capture = |tracer: &mut p::PathTracer, mesh: &p::Mesh| {
        get(p::replace_mesh(tracer, mesh));
        p::reset(tracer);
        frames(tracer, &camera, 2);
        pixels(tracer)
    };
    let mixed = get(p::mesh(Some(&spheres), None, &[(o.floor.clone(), gray)]));
    let mixed_pixels = capture(&mut tracer, &mixed);
    // The red pixels are the sphere's footprint: none outside its projected disc and most of the disc. Green
    // and blue stay below 100, not near zero, because the black dielectric also reflects the sky.
    let screen = |at: vec3::T| rays::camera::world_to_screen((0, 0, 64, 48), &camera, at).unwrap().unwrap();
    let center = screen(v3(0., 1., 0.));
    let radius = (screen(v3(0.5, 1., 0.)).x - center.x).abs();
    let mut red_pixels = 0;
    for pixel in 0..64 * 48 {
        let channel = |i: usize| mixed_pixels[pixel * 4 + i];
        if channel(0) > 200 && channel(1) < 100 && channel(2) < 100 {
            red_pixels += 1;
            assert!(((pixel % 64) as f64 + 0.5 - center.x).hypot((pixel / 64) as f64 + 0.5 - center.y) <= radius + 1.);
        }
    }
    println!("pathtracer scene: {red_pixels} red pixels in a disc of radius {radius:.2} px");
    assert!(red_pixels as f64 >= 0.7 * std::f64::consts::PI * radius * radius);
    let without_floor = capture(&mut tracer, &get(p::mesh(Some(&spheres), None, &[])));
    let point = rays::camera::world_to_screen((0, 0, 64, 48), &camera, v3(3., 0., 0.)).unwrap().unwrap();
    let pixel = point.y as usize * 64 + point.x as usize;
    let difference: i64 = (0..3).map(|c| (mixed_pixels[pixel * 4 + c] as i64 - without_floor[pixel * 4 + c] as i64).abs()).sum();
    assert!(difference > 10);
    get(p::flush(&mut tracer));
    assert!(p::r#move(&mut tracer, &[mat4::identity()]).is_err(), "move of a non-scene accepted");
    p::destroy(&mut tracer);
    println!("pathtracer scene: mixed triangles and spheres ok");
}

/// light_of: a white matte plane under each converted light converges to the raster irradiance pi I / d^2,
/// i.e. outgoing radiance albedo * I / d^2 (x0.97 for the BSDF's Fresnel at normal incidence).
fn light_of() {
    if !common::gpu() {
        return;
    }
    let o = objects();
    let plane = p::Scene { objects: vec![(o.floor.clone(), p::material(Some(1.), None, None, None, rgb(0.8, 0.8, 0.8)))], spheres: vec![], strands: vec![], environment: black(), lights: vec![] };
    let mut tracer = get(p::create(&pt::options(4), 32, 32, &plane));
    let camera = rays::camera::perspective(Some(0.1), None, None, v3(0., 4., 0.001), vec3::zero()).unwrap();
    let mut check = |name: &str, light: rays::light::Light, expected: f64| {
        get(p::set_lights(&mut tracer, &[p::light_of(&light)]));
        p::reset(&mut tracer);
        frames(&mut tracer, &camera, 32);
        let image = pixels(&tracer);
        let mut sum = 0.;
        for y in 12..20 {
            for x in 12..20 {
                sum += image[(y * 32 + x) * 4] as f64;
            }
        }
        let measured = unresolve(sum / 64.);
        println!("pathtracer light_of {name}: {measured:.4} expected {expected:.4}");
        assert!((measured - expected).abs() < 0.06 * expected, "{name}: {measured} vs {expected}");
    };
    let at = v3(0., 2., 0.);
    let down = v3(0., -1., 0.);
    let physical = rays::light::attenuation(None, Some(1.)).unwrap();
    let matte = |intensity: f64, d2: f64| 0.8 * 0.97 * intensity / d2;
    check("area", rays::light::area(None, Some(2.), Some(physical), None, at, down, 0.2, 0.2).unwrap(), matte(2., 4.));
    check("point", rays::light::point(None, Some(2.), Some(physical), at).unwrap(), matte(2., 4.));
    check("spot", rays::light::spot(None, Some(2.), Some(physical), at, down, 0.5, 1.).unwrap(), matte(2., 4.));
    check("directional", rays::light::directional(None, Some(0.5), down).unwrap(), matte(0.5, 1.));
    p::destroy(&mut tracer);
}

pub const CASES: &[(&str, fn())] = &[("live_updates", live_updates), ("scene_mesh", scene_mesh), ("timing", timing), ("mixed_spheres", mixed_spheres), ("light_of", light_of), ("device_topology_refits", device_topology_refits)];
