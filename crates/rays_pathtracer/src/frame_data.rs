//! Port of lib/rays_pathtracer/frame_data.ml: the film's buffers and the camera's axes.
use ogpu::backend::{self as b, Buffer, Texture};
use ogpu::types::{BufferDescriptor, BufferUsage, Memory, TextureDescriptor, TextureFormat, TextureUsage};
use rays_math::vec3;

pub fn gpu_err(e: ogpu::error::Error) -> String {
    ogpu::error::to_string(&e)
}

pub fn put_f32(bytes: &mut [u8], offset: usize, value: f64) {
    bytes[offset..offset + 4].copy_from_slice(&(value as f32).to_le_bytes());
}

pub fn put_u32(bytes: &mut [u8], offset: usize, value: i64) {
    bytes[offset..offset + 4].copy_from_slice(&(value as i32).to_le_bytes());
}

pub fn color_tuple(color: &rays_math::color::Linear::T) -> (f64, f64, f64) {
    (color.r, color.g, color.b)
}

pub fn buffer(memory: Memory, usage: Vec<BufferUsage>, device: &b::Device, label: &str, size: i64) -> Result<Buffer, String> {
    b::create_buffer(device, BufferDescriptor { label: Some(label.to_string()), size: size.max(4), usage }, Some(memory)).map_err(gpu_err)
}

/// A shared storage buffer.
pub fn storage(device: &b::Device, label: &str, size: i64) -> Result<Buffer, String> {
    buffer(Memory::Shared, vec![BufferUsage::Storage], device, label, size)
}

/// The Lisp kernels map over `pixels`, the image's pixel indices in 16 x 16 tiles (the tracing kernels,
/// whose films hold pixel p at `order[p]`) or over `linear` ones (the resolves, writing `resolved`, their
/// RGBA in [0, 1]), drawn into an output texture by Flow's image sink pass.
#[derive(Clone)]
pub struct film {
    pub accum: Buffer,
    pub history_color: Vec<Buffer>,
    pub history_geometry: Vec<Buffer>,
    pub outputs: Vec<Texture>,
    pub pixels: Buffer,
    pub linear: Buffer,
    pub order: Buffer,
    pub resolved: Buffer,
}

fn destroy_buffers(list: &[Buffer]) {
    for value in list {
        let _ = b::destroy_buffer(value);
    }
}

/// ponytail: a failure midway leaks the earlier allocations to the device's leak report, like
/// `create`; allocation failures are out-of-memory only.
pub fn allocate_film(device: &b::Device, width: i64, height: i64) -> Result<film, String> {
    let history = |label: &str| buffer(Memory::DeviceLocal, vec![BufferUsage::Storage], device, label, width * height * 16);
    let texture = |index: i64| {
        b::create_texture(
            device,
            TextureDescriptor {
                label: Some(format!("pathtracer-film-{index}")),
                width,
                height,
                depth: 1,
                mip_levels: 1,
                sample_count: 1,
                format: TextureFormat::Rgba8Unorm,
                usage: vec![
                    TextureUsage::TextureBinding,
                    TextureUsage::StorageBinding,
                    TextureUsage::RenderAttachment,
                    TextureUsage::TextureCopySrc,
                    TextureUsage::TextureCopyDst,
                ],
            },
        )
        .map_err(gpu_err)
    };
    let accum = history("pathtracer-accum")?;
    let color0 = history("pathtracer-history-color-0")?;
    let color1 = history("pathtracer-history-color-1")?;
    let geometry0 = history("pathtracer-history-geometry-0")?;
    let geometry1 = history("pathtracer-history-geometry-1")?;
    let output0 = texture(0)?;
    let output1 = texture(1)?;
    let output2 = texture(2)?;
    // a threadgroup of 256 traces a 16 x 16 block, each SIMD group of 32 an 8 x 4 tile of it; one staging
    // buffer is filled and uploaded three times: the tiled indices, their inverse, the identity
    let n = (width * height) as usize;
    let mut bytes = vec![0u8; 4 * n];
    // (tiled index, pixel) in tile order
    let tiled = |f: &mut dyn FnMut(usize, usize)| {
        let mut t = 0usize;
        let tiles = |w: i64, h: i64, x0: i64, y0: i64, x1: i64, y1: i64, f: &mut dyn FnMut(i64, i64, i64, i64)| {
            for ty in 0..=(y1 - y0 - 1) / h {
                for tx in 0..=(x1 - x0 - 1) / w {
                    f(x0 + w * tx, y0 + h * ty, x1.min(x0 + w * tx + w), y1.min(y0 + h * ty + h));
                }
            }
        };
        tiles(16, 16, 0, 0, width, height, &mut |x0, y0, x1, y1| {
            tiles(8, 4, x0, y0, x1, y1, &mut |x0, y0, x1, y1| {
                for y in y0..y1 {
                    for x in x0..x1 {
                        f(t, (y * width + x) as usize);
                        t += 1;
                    }
                }
            });
        });
    };
    let upload = |label: &str, bytes: &[u8]| -> Result<Buffer, String> {
        let buf = storage(device, label, bytes.len() as i64)?;
        b::write_buffer(&buf, 0, bytes).map_err(gpu_err)?;
        Ok(buf)
    };
    tiled(&mut |t, p| put_f32(&mut bytes, 4 * t, p as f64));
    let pixels = upload("pathtracer-pixels", &bytes)?;
    tiled(&mut |t, p| put_f32(&mut bytes, 4 * p, t as f64));
    let order = upload("pathtracer-order", &bytes)?;
    for i in 0..n {
        put_f32(&mut bytes, 4 * i, i as f64);
    }
    let linear = upload("pathtracer-linear", &bytes)?;
    let resolved = history("pathtracer-resolved")?;
    Ok(film { accum, history_color: vec![color0, color1], history_geometry: vec![geometry0, geometry1], outputs: vec![output0, output1, output2], pixels, linear, order, resolved })
}

/// `keep`: an output the image still samples, left alive
pub fn destroy_film(keep: Option<usize>, film: &film) {
    let mut all = vec![film.accum.clone(), film.pixels.clone(), film.linear.clone(), film.order.clone(), film.resolved.clone()];
    all.extend(film.history_color.iter().cloned());
    all.extend(film.history_geometry.iter().cloned());
    destroy_buffers(&all);
    for (slot, texture) in film.outputs.iter().enumerate() {
        if keep != Some(slot) {
            let _ = b::destroy_texture(texture);
        }
    }
}

pub fn camera_fov(camera: &rays::camera::t) -> Result<f64, String> {
    match rays::camera::projection(camera) {
        rays::camera::projection::Perspective { fov_y, .. } => Ok(fov_y),
        rays::camera::projection::Orthographic { .. } => Err("path tracer requires a perspective camera".to_string()),
    }
}

pub fn normalize(v: vec3::T) -> vec3::T {
    let l = (v.x * v.x + v.y * v.y + v.z * v.z).sqrt();
    if l == 0. { vec3::create(0., 0., 1.) } else { vec3::create(v.x / l, v.y / l, v.z / l) }
}

fn cross(a: vec3::T, b: vec3::T) -> vec3::T {
    vec3::create(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x)
}

/// The camera's forward, right and up axes.
pub fn axes(camera: &rays::camera::t) -> (vec3::T, vec3::T, vec3::T) {
    let p = rays::camera::position(camera);
    let target = rays::camera::target(camera);
    let forward = normalize(vec3::create(target.x - p.x, target.y - p.y, target.z - p.z));
    let camera_up = rays::camera::up(camera);
    let world_up = if (forward.x * camera_up.x + forward.y * camera_up.y + forward.z * camera_up.z).abs() > 0.999 {
        if forward.y.abs() > 0.999 { vec3::create(0., 0., 1.) } else { vec3::create(0., 1., 0.) }
    } else {
        camera_up
    };
    let right = normalize(cross(forward, world_up));
    (forward, right, cross(right, forward))
}
