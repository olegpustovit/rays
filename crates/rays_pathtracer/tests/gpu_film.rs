//! the traced film is published as a GPU texture, byte
//! for byte the standalone render, sampled by Canvas, released and resized, with zero live-handle delta.
use crate::common;
use common::meshes;
use common::pt::{self, get, rgb};
use rays_math::vec3;
use rays_pathtracer::rays_pathtracer as p;

fn film() {
    if !common::gpu() {
        return;
    }
    let baseline = pt::live_handles();
    let cube = meshes::box_mesh(vec3::zero(), vec3::create(1., 1., 1.));
    let scene = p::Scene {
        objects: vec![(cube, pt::material(rgb(0.8, 0.6, 0.4)))],
        spheres: vec![],
        strands: vec![],
        environment: p::Environment { sky: rgb(0.6, 0.7, 0.8), ground: rgb(0.1, 0.1, 0.1), panels: vec![] },
        lights: vec![],
    };
    let camera = rays::camera::perspective(Some(0.9), None, None, vec3::create(0., 0., 4.), vec3::zero()).unwrap();
    let mut standalone = get(p::create(&Default::default(), 48, 32, &scene));
    get(p::render(&mut standalone, &camera));
    get(p::flush(&mut standalone));
    assert_eq!(p::samples(&standalone), 1);
    let expected = pt::pixels(&standalone);
    p::destroy(&mut standalone);
    drop(standalone);
    assert_eq!(expected.len(), 48 * 32 * 4);
    assert_eq!(pt::live_handles(), baseline);
    let config = renderer::renderer::Configuration { logical_width: 48, logical_height: 32, drawable_width: 48, drawable_height: 32, title: "GPU film test".to_string(), ..renderer::renderer::default_configuration() };
    let target = runtime::open_window(&config.title, config.logical_width, config.logical_height, config.vsync, config.high_density).unwrap_or_else(|e| panic!("{}", ogpu_core::error::to_string(&e)));
    let coordinator = renderer::renderer::create(&config, &target).unwrap_or_else(|e| panic!("{}", ogpu_core::error::to_string(&e)));
    // A live shared lease keeps the window's device alive.
    let lease = renderer::renderer::acquire_gpu().unwrap();
    assert!(renderer::renderer::gpu_shared(&lease));
    assert!(renderer::renderer::destroy(&coordinator).is_err());
    renderer::renderer::release_gpu(&lease);
    let mut tracer = get(p::create(&Default::default(), 48, 32, &scene));
    get(p::render(&mut tracer, &camera));
    get(p::flush(&mut tracer));
    let image = rays::image::resource(&p::image(&tracer));
    let snapshot = |image: &runtime_resources::image::Image| match runtime_resources::image::gpu_snapshot(image) {
        Ok(Some((_, _, _, texture))) => texture,
        _ => panic!("traced film was published through CPU image storage"),
    };
    let texture = snapshot(&image);
    let direct = ogpu_core::backend::read_texture(&texture, 48 * 4).unwrap();
    assert_eq!(p::samples(&tracer), 1);
    assert!(direct == expected);
    assert!(pt::pixels(&tracer) == direct);
    let canvas = rays::canvas::create(48, 32).unwrap();
    get(rays::canvas::render(None, &canvas, &[get(rays::scene::image(&p::image(&tracer), (0, 0), None, None))]));
    let copied = get(rays::canvas::to_image(&canvas));
    let sampled = get(rays::image::pixels(&copied));
    let _ = rays::image::destroy(&copied);
    let _ = rays::canvas::destroy(&canvas);
    assert!(sampled == direct);
    let colors: std::collections::HashSet<u8> = direct.iter().step_by(4).cloned().collect();
    assert!(colors.len() > 1);
    let mut textures = std::collections::HashSet::new();
    textures.insert(ogpu_core::backend::texture_id(&texture));
    for _ in 0..30 {
        pt::frame(&mut tracer, &camera);
        textures.insert(ogpu_core::backend::texture_id(&snapshot(&image)));
    }
    assert_eq!(textures.len(), 3);
    // a tracer at rest: the film goes, the picture stays, and the next frame is a first one
    let shown = pt::pixels(&tracer);
    let before = p::private::device_bytes(&tracer);
    get(p::release_film(&mut tracer));
    let released = before - p::private::device_bytes(&tracer);
    // Metal frees a buffer when its last Rust clone drops (ogpu_metal), not at destroy: the bytes are checked
    // where destroy frees them, and only printed on Metal
    println!("released {released} bytes (expected at least {})", 116 * 48 * 32);
    assert!(released >= 116 * 48 * 32 || ogpu_core::impl_::backend().map(|b| b == "metal").unwrap_or(false));
    assert!(pt::pixels(&tracer) == shown);
    assert_eq!(p::samples(&tracer), 31);
    get(p::release_film(&mut tracer));
    get(p::render(&mut tracer, &camera));
    assert_eq!(p::samples(&tracer), 0);
    get(p::flush(&mut tracer));
    assert_eq!(p::samples(&tracer), 1);
    assert!(pt::pixels(&tracer) == expected);
    // released and resized, then drawn at the new size
    get(p::release_film(&mut tracer));
    get(p::resize(&mut tracer, 32, 24));
    pt::frame(&mut tracer, &camera);
    assert_eq!(pt::pixels(&tracer).len(), 32 * 24 * 4);
    // what a rest gives back at a real size: 116 of the film's 120 bytes a pixel (shrunk from 960x540)
    get(p::resize(&mut tracer, 320, 180));
    pt::frame(&mut tracer, &camera);
    let before = p::private::device_bytes(&tracer);
    get(p::release_film(&mut tracer));
    // Release drops the owners; settle deferred native destruction before measuring residency.
    pt::live_handles();
    let after = p::private::device_bytes(&tracer);
    println!("GPU film at rest: device {before} -> {after} bytes at 320x180");
    assert!(before - after >= 116 * 320 * 180);
    p::destroy(&mut tracer);
    drop(tracer);
    renderer::renderer::destroy(&coordinator).unwrap_or_else(|e| panic!("{}", ogpu_core::error::to_string(&e)));
    runtime::destroy(&mut target.borrow_mut()).unwrap();
    drop(coordinator);
    drop(texture);
    drop(image);
    drop(canvas);
    drop(copied);
    assert_eq!(pt::live_handles(), baseline);
    println!("GPU film: exact standalone/shared/Canvas bytes, direct texture, readback, zero live delta");
}

pub const CASES: &[(&str, fn())] = &[("film", film)];
