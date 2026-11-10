//! Raster/path-tracer parity compares probe luminance/highlight location over converged World presets, using 256 samples at 128x64 to stay within the case budget.
use crate::common;
use common::meshes;
use common::pt::{self, get, rgb};
use rays::{camera, canvas, material, scene, scene3, world};
use rays_math::vec3;
use rays_pathtracer::rays_pathtracer as p;

const WIDTH: usize = 128;
const HEIGHT: usize = 64;
const GRAY: i64 = 200;

fn probes() -> [vec3::T; 2] {
    [vec3::create(-1.3, 0., 0.), vec3::create(1.3, 0., 0.)]
}

/// Both renderers see the same surfaces: raster's Material mapping gives albedo = diffuse (sRGB decoded),
/// roughness = sqrt (2 / (shininess + 2)).
fn roughness(shininess: f64) -> f64 {
    (2. / (shininess + 2.)).sqrt().min(1.)
}

fn view_camera() -> camera::Camera {
    camera::perspective(Some(0.5), None, None, vec3::create(0., 0., 7.), vec3::zero()).unwrap()
}

/// Pixels inside each probe's disc on screen.
fn disc(index: usize) -> Vec<(usize, usize)> {
    let camera = view_camera();
    let viewport = (0, 0, WIDTH as i64, HEIGHT as i64);
    let center = camera::world_to_screen(viewport, &camera, probes()[index]).unwrap().unwrap();
    let edge = camera::world_to_screen(viewport, &camera, vec3::add(probes()[index], vec3::create(0.9, 0., 0.))).unwrap().unwrap();
    let radius = (edge.x - center.x).abs();
    let mut out = Vec::new();
    for y in 0..HEIGHT {
        for x in 0..WIDTH {
            if (x as f64 + 0.5 - center.x).hypot(y as f64 + 0.5 - center.y) < radius {
                out.push((x, y));
            }
        }
    }
    out
}

fn luminance(bytes: &[u8], (x, y): (usize, usize)) -> f64 {
    let at = |channel: usize| bytes[4 * (y * WIDTH + x) + channel] as f64;
    0.2126 * at(0) + 0.7152 * at(1) + 0.0722 * at(2)
}

fn mean(bytes: &[u8], pixels: &[(usize, usize)]) -> f64 {
    pixels.iter().map(|p| luminance(bytes, *p)).sum::<f64>() / pixels.len() as f64
}

/// The highlight is the probe's half-maximum region: the pixels at least halfway from the mean to the
/// brightest. A narrower band is a handful of pixels on a mirror-like probe, and noise then picks one of
/// several equally bright reflections.
fn highlight(bytes: &[u8], pixels: &[(usize, usize)]) -> Vec<(usize, usize)> {
    let peak = pixels.iter().map(|p| luminance(bytes, *p)).fold(0., f64::max);
    let low = mean(bytes, pixels);
    pixels.iter().cloned().filter(|p| luminance(bytes, *p) >= peak - 0.5 * (peak - low)).collect()
}

fn centroid(bright: &[(usize, usize)]) -> (f64, f64) {
    let n = bright.len() as f64;
    bright.iter().fold((0., 0.), |(x, y), (px, py)| (x + *px as f64 / n, y + *py as f64 / n))
}

/// Mean distance from each highlight pixel to the other image's nearest one, the larger direction: it sees
/// one displaced reflection among several, which a centroid of symmetric reflections does not.
fn spread(a: &[(usize, usize)], b: &[(usize, usize)]) -> f64 {
    let one = |a: &[(usize, usize)], b: &[(usize, usize)]| {
        a.iter().map(|(x, y)| b.iter().map(|(u, v)| (*x as f64 - *u as f64).hypot(*y as f64 - *v as f64)).fold(f64::INFINITY, f64::min)).sum::<f64>() / a.len() as f64
    };
    one(a, b).max(one(b, a))
}

fn parity() {
    if !common::gpu() {
        return;
    }
    if std::env::var_os("RAYS_SLOW").is_none() {
        println!("skipped: the preset sweep is a RAYS_SLOW=1 case (about 15 s warm)");
        return;
    }
    let baseline = pt::live_handles();
    let albedo = (GRAY as f64 / 255.).powf(2.2);
    let shininess = [1., 2000.];
    let geometry: Vec<_> = probes().iter().map(|center| meshes::uv_sphere(*center, 64, 32, 0.9)).collect();
    let objects: Vec<_> = geometry.iter().enumerate().map(|(i, g)| (g.clone(), p::material(Some(roughness(shininess[i])), None, None, None, rgb(albedo, albedo, albedo)))).collect();
    let tracing = p::Scene {
        objects,
        spheres: vec![],
        strands: vec![],
        environment: p::Environment { sky: rgb(0., 0., 0.), ground: rgb(0., 0., 0.), panels: vec![] },
        lights: vec![],
    };
    let mut tracer = get(p::create(&p::CreateOptions { spp: Some(4), ..Default::default() }, WIDTH as i64, HEIGHT as i64, &tracing));
    let surface = get(canvas::create(WIDTH as i64, HEIGHT as i64));
    let nodes: Vec<_> = geometry
        .iter()
        .enumerate()
        .map(|(i, g)| scene3::mesh(Some(get(material::create(Some(rays_math::color::gray(GRAY)), None, None, None, Some(shininess[i]), None))), None, None, None, None, g.clone()))
        .collect();
    let discs = [disc(0), disc(1)];
    for pixels in &discs {
        assert!(!pixels.is_empty());
    }
    let presets = world::presets();
    assert_eq!(presets.len(), 4);
    let camera = view_camera();
    let mut failures = Vec::new();
    for (name, definition) in presets {
        let baked = get(world::bake(Some(1), None, 128, 64, &definition));
        get(p::set_world(&mut tracer, Some(&baked)));
        p::reset(&mut tracer);
        pt::frames(&mut tracer, &camera, 64);
        assert_eq!(p::samples(&tracer), 256);
        let traced = pt::pixels(&tracer);
        get(canvas::render(None, &surface, &[scene::view3d(None, &camera, scene3::with_world(baked.clone(), &get(scene3::create(None, None, None, None, nodes.clone()))))]));
        let image = get(canvas::to_image(&surface));
        let raster = get(rays::image::pixels(&image));
        let _ = rays::image::destroy(&image);
        assert!(traced.len() == WIDTH * HEIGHT * 4 && raster.len() == traced.len());
        for (i, pixels) in discs.iter().enumerate() {
            let (traced_mean, raster_mean) = (mean(&traced, pixels), mean(&raster, pixels));
            assert!(traced_mean > 0. && raster_mean > 0.);
            let relative = (traced_mean - raster_mean).abs() / traced_mean;
            let (traced_light, raster_light) = (highlight(&traced, pixels), highlight(&raster, pixels));
            let ((tx, ty), (rx, ry)) = (centroid(&traced_light), centroid(&raster_light));
            let distance = (tx - rx).hypot(ty - ry);
            let spread = spread(&traced_light, &raster_light);
            println!("parity {name} probe {i}: mean path {traced_mean:.2} raster {raster_mean:.2} ({:.1}%), highlight {distance:.2} px, spread {spread:.2} px", relative * 100.);
            // Raster's SH/prefiltered approximation has a separate error allowance. Measured natively on M1:
            // mean error 0.0-2.9%, highlight centroid distance 0.2-4.8 px, spread 0.03-2.4 px (the probe
            // disc radius is about 16 px).
            if relative > 0.05 || distance > 8. || spread > 4. {
                failures.push(format!("{name} probe {i}"));
            }
        }
    }
    p::destroy(&mut tracer);
    drop(tracer);
    let _ = canvas::destroy(&surface);
    assert_eq!(pt::live_handles(), baseline);
    assert!(failures.is_empty(), "parity outside bounds: {}", failures.join(", "));
    println!("raster/path tracer parity: all rendered presets within bounds");
}

pub const CASES: &[(&str, fn())] = &[("parity", parity)];
