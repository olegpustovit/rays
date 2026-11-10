//! GPU Lisp kernels compare against CPU reference films on small deterministic dome/World scenes. PT_W/PT_H/PT_FRAMES control the budgeted dimensions/frame count; native kernels retain register-pressure qualification.
use crate::common;
use common::pt::{self, get, rgb};
use flow_check::ray_host::{self, Hit};
use rays::world as w;
use rays_math::{mat4, vec3};
use rays_pathtracer::rays_pathtracer as p;
use std::sync::Arc;

fn env(name: &str, default: i64) -> i64 {
    std::env::var(name).ok().and_then(|v| v.parse().ok()).unwrap_or(default)
}

/// pathtrace.metal, the handwritten kernel this one replaced, was 908 lines
const HANDWRITTEN_LINES: usize = 908;

fn mesh() -> rays::mesh::Mesh {
    rays::mesh::recalculate_normals(&rays::mesh::plane(4., 4.).unwrap())
}

fn material() -> p::Material {
    p::material(Some(0.5), Some(0.2), None, Some(0.3), rgb(0.7, 0.5, 0.3))
}

fn light() -> p::Light {
    p::Light { at: vec3::create(0.5, 0.8, 2.), target: vec3::create(0.5, 0.8, 0.), size: (1.5, 1.), color: rgb(1., 1., 1.), intensity: 6. }
}

fn environment() -> p::Environment {
    p::Environment {
        sky: rgb(0.5, 0.6, 0.8),
        ground: rgb(0.2, 0.2, 0.2),
        panels: vec![p::Panel { direction: vec3::create(0.3, 0.6, 0.74), width: 0.5, height: 0.35, softness: 0.2, color: rgb(1., 0.9, 0.8), intensity: 3. }],
    }
}

fn dome() -> p::Scene {
    p::Scene { objects: vec![(mesh(), material())], spheres: vec![], strands: vec![], environment: environment(), lights: vec![light()] }
}

fn camera() -> rays::camera::Camera {
    rays::camera::perspective(Some(0.9), None, None, vec3::create(0.3, -3., 2.5), vec3::zero()).unwrap()
}

/// a small World: a dim gradient, a bright disc baked into both maps, a rect-light strip and a sun
fn baked() -> w::Baked {
    let layers = vec![
        w::Layer::Gradient { zenith: w::rgb(0.3, 0.35, 0.5), horizon: w::rgb(0.2, 0.2, 0.2), nadir: w::rgb(0.05, 0.05, 0.05), sharpness: 1. },
        w::Layer::Shape { shape: w::Shape::Circle, azimuth: 2.5, elevation: 0.6, width: 0.6, height: 0.6, softness: 0.1, color: w::rgb(1., 0.8, 0.6), intensity: 4., emit: w::Emit::Dome },
        w::Layer::Shape { shape: w::Shape::Rect, azimuth: -1., elevation: 0.4, width: 0.5, height: 0.3, softness: 0., color: w::rgb(1., 1., 1.), intensity: 3., emit: w::Emit::Light },
        w::Layer::Sun { angular_radius: 0.1, intensity: 20., tint: w::rgb(1., 0.95, 0.9) },
    ];
    let mut definition = w::default();
    definition.exposure = 0.5;
    definition.sun = w::SunMode::Manual(0.8, 0.9);
    definition.layers = layers.into_iter().map(|layer| w::Entry { name: "layer".to_string(), layer, visible: true }).collect();
    w::bake(Some(1), None, 16, 8, &definition).expect("bake")
}

/// the CPU walker's scene: brute-force Moller-Trumbore over the plane's triangles, as scene/trace reports a
/// hit
fn register_walker() {
    let view = rays::mesh::view::packed_view(&mesh()).unwrap();
    let triangles: Vec<(f64, f64, f64)> = view.indices.iter().map(|k| (view.vertices.x[*k as usize], view.vertices.y[*k as usize], view.vertices.z[*k as usize])).collect();
    let sub = |a: (f64, f64, f64), b: (f64, f64, f64)| (a.0 - b.0, a.1 - b.1, a.2 - b.2);
    let dot = |a: (f64, f64, f64), b: (f64, f64, f64)| a.0 * b.0 + a.1 * b.1 + a.2 * b.2;
    let cross = |a: (f64, f64, f64), b: (f64, f64, f64)| (a.1 * b.2 - a.2 * b.1, a.2 * b.0 - a.0 * b.2, a.0 * b.1 - a.1 * b.0);
    let normalize = move |v: (f64, f64, f64)| {
        let k = dot(v, v).sqrt();
        (v.0 / k, v.1 / k, v.2 / k)
    };
    ray_host::register(
        1,
        Arc::new(move |q: &[f64]| {
            let o = (q[0], q[1], q[2]);
            let d = normalize((q[3], q[4], q[5]));
            let mut best = ray_host::MISS;
            for k in 0..triangles.len() / 3 {
                let (p0, p1, p2) = (triangles[3 * k], triangles[3 * k + 1], triangles[3 * k + 2]);
                let (e1, e2) = (sub(p1, p0), sub(p2, p0));
                let pv = cross(d, e2);
                let det = dot(e1, pv);
                if det.abs() > 1e-12 {
                    let inv = 1. / det;
                    let tv = sub(o, p0);
                    let u = dot(tv, pv) * inv;
                    let qv = cross(tv, e1);
                    let v = dot(d, qv) * inv;
                    let t = dot(e2, qv) * inv;
                    if u >= 0. && v >= 0. && u + v <= 1. && t >= q[6] && t <= q[7] && (!best.hit || t < best.t) {
                        let (nx, ny, nz) = normalize(cross(e1, e2));
                        best = Hit { t, prim: k as i64, u, v, nx, ny, nz, hit: true, instance: 0 };
                    }
                }
            }
            best
        }),
    );
}

fn has(fragment: &str, line: &str) -> bool {
    line.contains(fragment)
}

fn scenario(label: &str, instanced: bool, world: Option<&w::Baked>) {
    let (width, height, frames) = (env("PT_W", 16), env("PT_H", 12), env("PT_FRAMES", 2));
    let mut t = get(p::create(&p::CreateOptions { spp: Some(2), bounces: Some(4), ..Default::default() }, width, height, &dome()));
    get(p::set_world(&mut t, world));
    if instanced {
        get(p::replace_mesh(&mut t, &get(p::scene_mesh(&[(mat4::identity(), get(p::mesh(None, None, &[(mesh(), material())])))]))));
    }
    p::reset(&mut t);
    let camera = camera();
    // the CPU walker accumulates the same frames, each before the device renders it
    let mut accum = vec![0.; (4 * width * height) as usize];
    for _ in 0..frames {
        accum = get(p::private::cpu_trace(&mut t, &camera, &accum));
        get(p::render(&mut t, &camera));
        get(p::flush(&mut t));
    }
    let film = get(rays::image::pixels(&p::image(&t)));
    let exposure = world.map(|b| 2f64.powf(b.exposure)).unwrap_or(1.);
    let aces = |c: f64| {
        let c = c * exposure;
        (c * (2.51 * c + 0.03) / (c * (2.43 * c + 0.59) + 0.14)).max(0.).min(1.).powf(1. / 2.2)
    };
    let (mut squares, mut different) = (0., 0);
    let mut distinct = std::collections::HashSet::new();
    for pixel in 0..(width * height) as usize {
        let weight = accum[4 * pixel + 3].max(1.);
        for c in 0..3 {
            let x = (aces(accum[4 * pixel + c] / weight) * 255.).round();
            let y = film[4 * pixel + c] as f64;
            if x != y {
                different += 1;
            }
            squares += ((x - y) / 255.).powi(2);
        }
        distinct.insert((film[4 * pixel], film[4 * pixel + 1]));
    }
    let rmse = (squares / (width * height * 3) as f64).sqrt();
    println!("pathtrace.rays {label}, GPU vs the CPU walker: {width}x{height}, {frames} frames x 2 spp, {} distinct colours, {different} of {} channels differ, RMSE {rmse:.3e}", distinct.len(), width * height * 3);
    assert!(distinct.len() > 4 && rmse <= 1e-4);
    // static shape gates (specification/gpu-codegen.md): one atomic, no 64-bit arithmetic in the loops but
    // their bound checks, within twice the handwritten kernel's lines; register pressure: Metal lowers a
    // pipeline's thread limit when a kernel needs more registers
    let (threads, source) = get(p::private::lisp_kernel(&mut t, "trace"));
    let lines: Vec<&str> = source.split('\n').collect();
    assert_eq!(lines.iter().filter(|l| has("atomic_fetch_or_explicit(", l)).count(), 1);
    let from = lines.iter().position(|l| has("for(r", l)).map(|i| &lines[i..]).unwrap_or(&[]);
    assert!(from.iter().all(|l| !has("ulong(", l) || has("bool over", l)));
    assert!(lines.len() <= 2 * HANDWRITTEN_LINES);
    println!("pathtrace.rays {label}: trace kernel {} lines, maxTotalThreads {threads} (pathtrace.metal 384)", lines.len());
    // the recorded limits (bound materials and bands): 448 flat, 384 instanced; a drop is a regression.
    // Metal only: another pipeline limit says nothing about this kernel's registers.
    if ogpu_core::impl_::backend().map(|b| b == "metal").unwrap_or(false) {
        assert!(threads >= if instanced { 384 } else { 448 });
    }
    p::destroy(&mut t);
}

/// the tracer types its declarations only at their call sites: here they are checked whole
fn library() {
    get(p::private::check_library());
    get(p::private::check_sketch());
}

/// the kernel cache: a first tracer stores the dome's three kernels, the dome scenario then renders from
/// them, and torn entries are recompiled to the same kernel
fn cache() {
    if !common::gpu() {
        return;
    }
    let directory = std::env::temp_dir().join(format!("rays-kernels-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&directory);
    *p::private::kernel_cache.lock().unwrap() = Some(directory.to_string_lossy().to_string());
    let trace = || {
        p::private::forget_memory_cache();
        let mut t = get(p::create(&p::CreateOptions { spp: Some(2), bounces: Some(4), ..Default::default() }, 16, 12, &dome()));
        pt::frame(&mut t, &camera());
        let (_, source) = get(p::private::lisp_kernel(&mut t, "trace"));
        p::destroy(&mut t);
        source
    };
    let entries = || -> Vec<std::path::PathBuf> { std::fs::read_dir(&directory).unwrap().map(|e| e.unwrap().path()).collect() };
    let stored = trace();
    assert_eq!(entries().len(), 3);
    for file in entries() {
        std::fs::write(file, "torn").unwrap();
    }
    assert!(trace() == stored && entries().iter().all(|f| std::fs::metadata(f).unwrap().len() > 16));
    // and a clean read returns the same kernel again
    assert!(trace() == stored);
    let _ = std::fs::remove_dir_all(&directory);
}

fn dome_case() {
    if !common::gpu() {
        return;
    }
    register_walker();
    scenario("dome", false, None);
}

fn dome_instanced() {
    if !common::gpu() {
        return;
    }
    register_walker();
    scenario("dome, instanced", true, None);
}

fn world_case() {
    if !common::gpu() {
        return;
    }
    register_walker();
    scenario("world", false, Some(&baked()));
}

pub const CASES: &[(&str, fn())] = &[("library", library), ("cache", cache), ("dome", dome_case), ("dome_instanced", dome_instanced), ("world", world_case)];
