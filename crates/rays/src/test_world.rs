use crate::world::{self as w, Background, Emit, Entry, Layer, Shape, SunMode};
use rays_math::vec3;
use std::sync::Arc;

fn check(name: &str, ok: bool) {
    assert!(ok, "test_world: {name}");
}

fn same_bits(a: &[f64], b: &[f64]) -> bool {
    a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.to_bits() == y.to_bits())
}

fn same_baked(a: &w::Baked, b: &w::Baked) -> bool {
    same_bits(&a.camera.pixels, &b.camera.pixels)
        && same_bits(&a.lighting.pixels, &b.lighting.pixels)
        && same_bits(&a.cdf.marginal, &b.cdf.marginal)
        && same_bits(&a.cdf.conditional, &b.cdf.conditional)
        && a.specular().len() == b.specular().len()
        && a.specular().iter().zip(b.specular().iter()).all(|(x, y)| same_bits(&x.pixels, &y.pixels))
        && a.sh9() == b.sh9()
        && a.lights == b.lights
        && a.sun == b.sun
}

fn entry_(l: Layer) -> Entry {
    Entry { name: "layer".to_string(), layer: l, visible: true }
}

fn with_layers(layers: Vec<Layer>) -> w::World {
    w::World { layers: layers.into_iter().map(entry_).collect(), ..w::default() }
}

fn dark(v: f64) -> Layer {
    Layer::Gradient { zenith: w::rgb(v, v, v), horizon: w::rgb(v, v, v), nadir: w::rgb(v, v, v), sharpness: 1. }
}

fn scatter(mirror: bool, emit_: Emit, seed: i64) -> Layer {
    Layer::Scatter {
        seed,
        count: 12,
        elevation_min: 0.1,
        elevation_max: 1.,
        size_min: 0.1,
        size_max: 0.3,
        palette: vec![w::rgb(1., 0.5, 0.2)],
        intensity: 3.,
        mirror,
        emit: emit_,
    }
}

fn bake(domains: Option<i64>, width: i64, height: i64, world: &w::World) -> w::Baked {
    w::bake(domains, None, width, height, world).expect("bake")
}

#[test]
fn domains() {
    for (name, world) in w::presets() {
        let a = bake(Some(1), 96, 48, &world);
        let b = bake(Some(4), 96, 48, &world);
        check(&format!("domains {name}"), same_baked(&a, &b));
    }
}

#[test]
fn seeds() {
    let make = |seed: i64| bake(Some(1), 64, 32, &with_layers(vec![dark(0.), scatter(false, Emit::Dome, seed)]));
    check("same seed", same_baked(&make(3), &make(3)));
    check("different seed", !same_bits(&make(3).camera.pixels, &make(4).camera.pixels));
}

/// Red irradiance at unit normal `n` from the baked coefficients, on the real basis `sh9`
/// documents.
fn irradiance(baked: &w::Baked, n: vec3::T) -> f64 {
    let (x, y, z) = (n.x, n.y, n.z);
    let basis = [
        0.282095,
        0.488603 * y,
        0.488603 * z,
        0.488603 * x,
        1.092548 * x * y,
        1.092548 * y * z,
        0.315392 * ((3. * z * z) - 1.),
        1.092548 * x * z,
        0.546274 * ((x * x) - (y * y)),
    ];
    baked.sh9().iter().enumerate().map(|(k, c)| c.r * basis[k]).sum()
}

#[test]
fn sh9() {
    let world = with_layers(vec![
        Layer::Gradient { zenith: w::rgb(1., 0.9, 0.8), horizon: w::rgb(0.4, 0.4, 0.5), nadir: w::rgb(0.1, 0.1, 0.1), sharpness: 1. },
        Layer::Shape {
            shape: Shape::Circle,
            azimuth: 1.,
            elevation: 0.4,
            width: 1.2,
            height: 0.,
            softness: 0.6,
            color: w::rgb(2., 1., 0.5),
            intensity: 1.,
            emit: Emit::Dome,
        },
    ]);
    let baked = bake(None, 128, 64, &world);
    let m = &baked.lighting;
    let pi = std::f64::consts::PI;
    let brute = |n: vec3::T| {
        let mut sum = 0.;
        for j in 0..m.height {
            let v = (j as f64 + 0.5) / m.height as f64;
            let dw = 2. * pi / m.width as f64 * (pi / m.height as f64) * (v * pi).sin();
            for i in 0..m.width {
                let d = w::direction_of_uv((i as f64 + 0.5) / m.width as f64, v);
                let c = crate::num::fmax(0., vec3::dot(d, n));
                sum += m.pixels[(((j * m.width) + i) * 3) as usize] * c * dw;
            }
        }
        sum
    };
    let dirs = [
        vec3::unit_y(),
        vec3::neg(vec3::unit_y()),
        vec3::unit_x(),
        vec3::unit_z(),
        vec3::normalize(vec3::create(1., 0.5, -1.)),
    ];
    let reference: Vec<f64> = dirs.iter().map(|d| brute(*d)).collect();
    let scale = reference.iter().fold(0., |a, b| crate::num::fmax(a, *b));
    for (n, e) in dirs.iter().zip(reference.iter()) {
        let got = irradiance(&baked, *n);
        check(&format!("sh9 {got:.3} vs {e:.3}"), (got - e).abs() <= 0.08 * scale);
    }
}

#[test]
fn promoted() {
    let manual = SunMode::Manual(1., 0.5);
    let sun = Layer::Sun { angular_radius: 0.05, intensity: 50., tint: w::rgb(1., 1., 0.9) };
    let rect = Layer::Shape {
        shape: Shape::Rect,
        azimuth: -0.8,
        elevation: 0.2,
        width: 0.6,
        height: 0.3,
        softness: 0.1,
        color: w::rgb(0.2, 1., 0.4),
        intensity: 5.,
        emit: Emit::Light,
    };
    let circle = Layer::Shape {
        shape: Shape::Circle,
        azimuth: 2.,
        elevation: 0.,
        width: 0.5,
        height: 0.,
        softness: 0.1,
        color: w::rgb(1., 1., 1.),
        intensity: 2.,
        emit: Emit::Dome,
    };
    let world = |layers: Vec<Layer>| w::World { sun: manual, rotation: 0.3, ..with_layers(layers) };
    let full = bake(None, 128, 64, &world(vec![dark(0.2), rect.clone(), scatter(true, Emit::Light, 5), circle, sun.clone()]));
    let only = bake(None, 128, 64, &world(vec![rect, scatter(false, Emit::Light, 5), sun]));
    let p = full.camera.pixels.len();
    for k in 0..p {
        let cam = full.camera.pixels[k];
        let diff = cam - full.lighting.pixels[k];
        check("lighting of promoted-only is black", only.lighting.pixels[k] == 0.);
        check("camera - lighting = promoted + disc", (diff - only.camera.pixels[k]).abs() <= 1e-9 * (1. + cam));
    }
    check("some promoted radiance", only.camera.pixels.iter().sum::<f64>() > 0.);
    check("light count", full.lights.len() == 13);
    check("sun extracted", full.sun.is_some());
    let room = |emit_: Emit| {
        bake(
            None,
            64,
            32,
            &with_layers(vec![Layer::Room {
                width: 6.,
                depth: 8.,
                height: 3.,
                rows: 2,
                columns: 3,
                panel: 0.5,
                intensity: 4.,
                wall: w::rgb(0.8, 0.8, 0.8),
                cove: 1.,
                emit: emit_,
            }]),
        )
    };
    let lit = room(Emit::Light);
    let dome = room(Emit::Dome);
    check("room camera independent of emit", same_bits(&lit.camera.pixels, &dome.camera.pixels));
    check("room dome lighting = camera", same_bits(&dome.lighting.pixels, &dome.camera.pixels));
    check("room panels leave lighting", lit.lighting.pixels.iter().sum::<f64>() < lit.camera.pixels.iter().sum::<f64>());
    check("room panel lights", lit.lights.len() == 6 && dome.lights.is_empty());
}

#[test]
fn cdf() {
    let world = with_layers(vec![
        dark(1e-4),
        Layer::Shape {
            shape: Shape::Circle,
            azimuth: 1.,
            elevation: 0.3,
            width: 0.2,
            height: 0.,
            softness: 0.,
            color: w::rgb(1., 1., 1.),
            intensity: 100.,
            emit: Emit::Dome,
        },
    ]);
    let baked = bake(None, 128, 64, &world);
    let c = &baked.cdf;
    let (width, height) = (128usize, 64usize);
    check("marginal ends at 1", c.marginal[height] == 1.);
    for j in 0..height {
        check("conditional row ends at 1", c.conditional[j * (width + 1) + width] == 1.);
    }
    // First index k in [o, o + n) with a.(k + 1) > x.
    let invert = |a: &[f64], o: usize, n: usize, x: f64| {
        let (mut lo, mut hi) = (0usize, n - 1);
        while lo < hi {
            let mid = (lo + hi) / 2;
            if a[o + mid + 1] > x {
                hi = mid;
            } else {
                lo = mid + 1;
            }
        }
        lo
    };
    let centre = vec3::create(0.3_f64.cos() * 1.0_f64.sin(), 0.3_f64.sin(), -0.3_f64.cos() * 1.0_f64.cos());
    let n = 1000;
    let mut hits = 0;
    for s in 0..n {
        let x1 = (s as f64 + 0.5) / n as f64;
        let x2 = (s as f64 * 0.618034) % 1.;
        let j = invert(&c.marginal, 0, height, x1);
        let i = invert(&c.conditional, j * (width + 1), width, x2);
        let d = w::direction_of_uv((i as f64 + 0.5) / width as f64, (j as f64 + 0.5) / height as f64);
        if vec3::dot(d, centre) > 0.2_f64.cos() {
            hits += 1;
        }
    }
    check("cdf concentrates", hits > 9 * n / 10);
}

/// The linked sun as the bake extracts it: the direction toward the sun (east = +X, up = +Y,
/// north = -Z), or `None` below the horizon.
fn linked_sun(latitude: f64, day_of_year: i64, hours: f64) -> Option<vec3::T> {
    let world = with_layers(vec![dark(0.), Layer::Sun { angular_radius: 0.05, intensity: 50., tint: w::rgb(1., 1., 0.9) }]);
    bake(None, 16, 8, &w::World { sun: SunMode::Linked, rotation: 0., latitude, day_of_year, time_of_day: hours, ..world })
        .sun
        .map(|s| s.direction)
}

#[test]
fn solar() {
    match linked_sun(0., 80, 12.) {
        Some(d) => check("equinox noon at zenith", d.y > 0.99),
        None => check("equinox noon has a sun", false),
    }
    check("midnight below horizon", linked_sun(0.7, 172, 0.).is_none());
    match linked_sun(0.7, 172, 8.) {
        Some(d) => check("morning sun in the east", d.x > 0. && d.y > 0.),
        None => check("a summer morning has a sun", false),
    }
}

/// The equirectangular coordinates of a unit direction, as World documents them.
fn uv_of_direction(d: vec3::T) -> (f64, f64) {
    let u = (d.x.atan2(-d.z) / (2. * std::f64::consts::PI)) + 0.5;
    (if u >= 1. { u - 1. } else { u }, crate::num::fmax(-1., crate::num::fmin(1., d.y)).acos() / std::f64::consts::PI)
}

#[test]
fn round_trip() {
    for k in 0..100 {
        let u = (k as f64 + 0.5) / 100.;
        let v = 0.02 + (0.96 * ((k as f64 * 0.618) % 1.));
        let d = w::direction_of_uv(u, v);
        check("unit direction", (vec3::length(d) - 1.).abs() < 1e-12);
        let (u2, v2) = uv_of_direction(d);
        check("uv round trip", (u - u2).abs() < 1e-12 && (v - v2).abs() < 1e-12);
    }
    let front = w::direction_of_uv(0.5, 0.5);
    check("front is u = 0.5", front.x.abs() < 1e-12 && front.y.abs() < 1e-12 && (front.z + 1.).abs() < 1e-12);
    check("row 0 is the zenith", w::direction_of_uv(0.25, 0.).y == 1.);
    check("azimuth grows toward +X", w::direction_of_uv(0.75, 0.5).x > 0.999999);
}

#[test]
fn cached() {
    let context = w::private::create_cache_context();
    w::private::with_cache_context(&context, &mut || {
        let a = w::bake_cached(32, 16, &w::default()).expect("bake");
        let b = w::bake_cached(32, 16, &w::World { exposure: 2., ..w::default() }).expect("bake");
        check("exposure-only change hits the cache", Arc::ptr_eq(&a.camera.pixels, &b.camera.pixels) && b.exposure == 2.);
        let _ = Background::Environment;
    });
}
