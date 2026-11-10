//! World tracing checks unbiased CDF/MIS sampling, lower variance, miss backgrounds and hittable rectangle lights. Sixteen samples per render amortize GPU round trips without changing sample count.
use crate::common;
use common::pt::{self, get, mean_red, pixels, rgb, rmse};
use rays::world as w;
use rays_math::vec3;
use rays_pathtracer::rays_pathtracer as p;

const SPP: i64 = 8;

fn v3(x: f64, y: f64, z: f64) -> vec3::T {
    vec3::create(x, y, z)
}

fn white() -> w::Rgb {
    w::rgb(1., 1., 1.)
}

fn gradient(c: w::Rgb) -> w::Layer {
    w::Layer::Gradient { zenith: c, horizon: c, nadir: c, sharpness: 1. }
}

fn world(background: w::Background, sun: (f64, f64), layers: Vec<w::Layer>) -> w::Baked {
    let mut definition = w::default();
    definition.layers = layers.into_iter().map(|layer| w::Entry { name: "layer".to_string(), layer, visible: true }).collect();
    definition.background = background;
    definition.exposure = 0.;
    definition.sun = w::SunMode::Manual(sun.0, sun.1);
    w::bake(Some(1), None, 64, 32, &definition).expect("bake")
}

fn environment_world(layers: Vec<w::Layer>) -> w::Baked {
    world(w::Background::Environment, (0., 1.), layers)
}

fn black_dome() -> p::Environment {
    p::Environment { sky: rgb(0., 0., 0.), ground: rgb(0., 0., 0.), panels: vec![] }
}

/// A lone analytic sphere: convex, so every path is direct light and the indirect firefly clamp never
/// applies.
fn scene(material: p::Material, lights: Vec<p::Light>) -> p::Scene {
    p::Scene {
        objects: vec![],
        strands: vec![],
        spheres: vec![p::Sphere { center: v3(0., 0., 0.), radius: 1., sphere_material: material }],
        environment: black_dome(),
        lights,
    }
}

fn matte() -> p::Scene {
    scene(p::material(Some(1.), None, None, None, rgb(0.8, 0.8, 0.8)), vec![])
}

fn create(bsdf_only: bool, scene: &p::Scene) -> p::PathTracer {
    get(p::create(&p::CreateOptions { spp: Some(SPP), bsdf_only: Some(bsdf_only), ..Default::default() }, 32, 32, scene))
}

/// Keep all corner rays inside the sphere's projected disc for the furnace.
fn camera(fov: f64) -> rays::camera::Camera {
    rays::camera::perspective(Some(fov), None, None, v3(0., 0., 6.), v3(0., 0., 0.)).unwrap()
}

/// `samples` samples per pixel of `baked` (after two settling frames).
fn capture(tracer: &mut p::PathTracer, camera: &rays::camera::Camera, baked: &w::Baked, samples: i64) -> Vec<u8> {
    let started = std::time::Instant::now();
    get(p::set_world(tracer, Some(baked)));
    pt::frames(tracer, camera, 2);
    p::reset(tracer);
    let frames = (samples + SPP - 1) / SPP;
    pt::frames(tracer, camera, frames);
    assert_eq!(p::samples(tracer), frames * SPP);
    let out = pixels(tracer);
    assert_eq!(out.len(), 32 * 32 * 4);
    if std::env::var_os("RAYS_PT_TIME").is_some() {
        eprintln!("capture {samples} samples: {:.2}s", started.elapsed().as_secs_f64());
    }
    out
}

/// The resolve's ACES fit and 2.2 gamma, for an exact background check.
fn resolved(x: f64) -> u8 {
    let c = rays_math::color::aces(x);
    ((c.max(0.).min(1.)).powf(1. / 2.2) * 255. + 0.5) as u8
}

fn furnace_mis() {
    if !common::gpu() {
        return;
    }
    let baseline = pt::live_handles();
    let matte = matte();
    let mut tracer = create(false, &matte);
    let mut bsdf = create(true, &matte);
    let cam = camera(0.2);
    let furnace = environment_world(vec![gradient(white())]);
    let uniform = capture(&mut tracer, &cam, &furnace, 256);
    let furnace_mean = mean_red(&uniform);
    assert!((furnace_mean - 224.).abs() < 2., "furnace mean {furnace_mean}");
    for (i, v) in uniform.iter().enumerate() {
        if i % 4 == 3 {
            assert_eq!(*v, 255);
        }
    }
    let peaked = world(
        w::Background::Environment,
        (0., 0.7),
        vec![
            gradient(w::rgb(0.02, 0.02, 0.02)),
            w::Layer::Shape { shape: w::Shape::Circle, azimuth: 0.5, elevation: 0.5, width: 0.4, height: 0.4, softness: 0.05, color: white(), intensity: 8., emit: w::Emit::Dome },
            w::Layer::Sun { angular_radius: 0.12, intensity: 50., tint: white() },
        ],
    );
    let reference = capture(&mut tracer, &cam, &peaked, 1024);
    let bsdf_reference = capture(&mut bsdf, &cam, &peaked, 1024);
    let difference = (mean_red(&reference) - mean_red(&bsdf_reference)).abs();
    assert!(mean_red(&reference) > 0. && difference < 1.5, "converged mean difference {difference}");
    assert!(rmse(&reference, &bsdf_reference) < 4.);
    let mis_short = capture(&mut tracer, &cam, &peaked, 16);
    let bsdf_short = capture(&mut bsdf, &cam, &peaked, 16);
    let (mis_error, bsdf_error) = (rmse(&mis_short, &reference), rmse(&bsdf_short, &reference));
    println!("furnace {furnace_mean:.2} diff {difference:.3} mis {mis_error:.3} bsdf {bsdf_error:.3}");
    assert!(mis_error < bsdf_error);
    println!("World: furnace {furnace_mean:.2}, converged mean difference {difference:.3}, 16-spp RMSE MIS {mis_error:.3} BSDF {bsdf_error:.3}");
    p::destroy(&mut bsdf);
    p::destroy(&mut tracer);
    drop(bsdf);
    drop(tracer);
    assert_eq!(pt::live_handles(), baseline);
}

fn backgrounds() {
    if !common::gpu() {
        return;
    }
    let baseline = pt::live_handles();
    let mut tracer = create(false, &matte());
    let miss_camera = rays::camera::perspective(Some(0.3), None, None, v3(0., 0., 6.), v3(0., 0., 7.)).unwrap();
    let background = w::rgb(0.2, 0.4, 0.6);
    let colored = capture(&mut tracer, &miss_camera, &world(w::Background::Color(background), (0., 1.), vec![gradient(white())]), 32);
    let expected = [resolved(background.r), resolved(background.g), resolved(background.b), 255];
    for (i, v) in colored.iter().enumerate() {
        assert_eq!(*v, expected[i % 4], "colored background byte {i}");
    }
    let transparent = capture(&mut tracer, &miss_camera, &world(w::Background::Transparent, (0., 1.), vec![gradient(white())]), 32);
    assert!(transparent.iter().all(|v| *v == 0));
    // With the sphere in view only the miss pixels are background: a corner is the colour or alpha 0, the
    // centre an opaque hit.
    let wide = camera(0.6);
    let center = (16 * 32 + 16) * 4;
    let over = capture(&mut tracer, &wide, &world(w::Background::Color(background), (0., 1.), vec![gradient(white())]), 32);
    for channel in 0..4 {
        assert_eq!(over[channel], expected[channel]);
    }
    assert!(over[center + 3] == 255 && over[center..center + 3] != over[0..3]);
    let cut = capture(&mut tracer, &wide, &world(w::Background::Transparent, (0., 1.), vec![gradient(white())]), 32);
    assert!(cut[3] == 0 && cut[center + 3] == 255);
    p::destroy(&mut tracer);
    drop(tracer);
    assert_eq!(pt::live_handles(), baseline);
}

fn mirror() {
    if !common::gpu() {
        return;
    }
    let baseline = pt::live_handles();
    let matte = matte();
    let mut tracer = create(false, &matte);
    let mut bsdf = create(true, &matte);
    let mirror = scene(p::material(Some(0.005), Some(1.), None, None, rgb(0.8, 0.8, 0.8)), vec![p::rect_light(4., (6., 6.), v3(0., 0., 0.), v3(0., 0., 8.))]);
    let geometry = get(p::mesh(Some(&mirror.spheres), None, &[]));
    get(p::replace_mesh(&mut tracer, &geometry));
    get(p::replace_mesh(&mut bsdf, &geometry));
    get(p::set_lights(&mut tracer, &mirror.lights));
    get(p::set_lights(&mut bsdf, &mirror.lights));
    let dark = environment_world(vec![gradient(w::rgb(0., 0., 0.))]);
    let cam = camera(0.2);
    let reflected = capture(&mut bsdf, &cam, &dark, 1024);
    let sampled = capture(&mut tracer, &cam, &dark, 1024);
    let center = (16 * 32 + 16) * 4;
    assert!(reflected[center] > 200);
    assert!((mean_red(&reflected) - mean_red(&sampled)).abs() < 2.);
    p::destroy(&mut bsdf);
    p::destroy(&mut tracer);
    drop(bsdf);
    drop(tracer);
    assert_eq!(pt::live_handles(), baseline);
    println!("pathtracer World: reflected rect and teardown checked");
}

pub const CASES: &[(&str, fn())] = &[("furnace_mis", furnace_mis), ("backgrounds", backgrounds), ("mirror", mirror)];
