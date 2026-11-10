#![allow(dead_code)]
use rays_math::color::linear;
use rays_pathtracer::rays_pathtracer as p;

pub fn rgb(r: f64, g: f64, b: f64) -> linear::T {
    linear::rgb(r, g, b)
}

pub fn get<T>(result: Result<T, String>) -> T {
    result.unwrap_or_else(|e| panic!("{e}"))
}

pub fn pixels(tracer: &p::PathTracer) -> Vec<u8> {
    get(rays::image::pixels(&p::image(tracer)))
}

pub fn frame(tracer: &mut p::PathTracer, camera: &rays::camera::Camera) {
    get(p::render(tracer, camera));
    get(p::flush(tracer));
}

pub fn frames(tracer: &mut p::PathTracer, camera: &rays::camera::Camera, n: i64) {
    for _ in 0..n {
        frame(tracer, camera);
    }
}

pub fn options(spp: i64) -> p::CreateOptions {
    p::CreateOptions { spp: Some(spp), ..Default::default() }
}

pub fn material(albedo: linear::T) -> p::Material {
    p::material(None, None, None, None, albedo)
}

pub fn live_handles() -> i64 {
    ogpu_core::impl_::live_handles().expect("live handles")
}

pub fn mean_red(bytes: &[u8]) -> f64 {
    let sum: i64 = bytes.iter().step_by(4).map(|b| *b as i64).sum();
    sum as f64 / (bytes.len() / 4) as f64
}

pub fn rmse(a: &[u8], b: &[u8]) -> f64 {
    let sum: f64 = a.iter().zip(b).step_by(4).map(|(x, y)| (*x as f64 - *y as f64).powi(2)).sum();
    (sum / (a.len() / 4) as f64).sqrt()
}
