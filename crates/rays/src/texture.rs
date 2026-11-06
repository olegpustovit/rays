//! Immutable CPU-readable textures for 3D mesh sampling.
use crate::num::{fmax, fmin, int_of_float};
use rays_math::color;
use runtime_resources::Image as Resource;
use std::rc::Rc;
use std::sync::atomic::{AtomicI64, Ordering};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum filter {
    Nearest,
    Bilinear,
    Trilinear,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum wrap {
    Clamp,
    Repeat,
    Mirror,
}

pub struct level {
    pub width: i64,
    pub height: i64,
    pub pixels: Vec<color::T>,
}

pub struct cpu {
    pub width: i64,
    pub height: i64,
    pub pixels: Vec<color::T>,
    pub mipmaps: Vec<level>,
}

pub enum backing {
    Cpu(cpu),
    Borrowed(Resource::t),
}

pub struct texture_data {
    backing: backing,
    id: i64,
}

/// A texture value: clones share one immutable texture.
#[derive(Clone)]
pub struct t(Rc<texture_data>);

static NEXT_ID: AtomicI64 = AtomicI64::new(1);

fn fresh_id() -> i64 {
    NEXT_ID.fetch_add(1, Ordering::SeqCst)
}

fn cpu_ref(texture: &t) -> Option<&cpu> {
    match &texture.0.backing {
        backing::Cpu(data) => Some(data),
        backing::Borrowed(_) => None,
    }
}

fn borrowed_message(operation: &str) -> String {
    format!("{operation}: borrowed GPU texture needs an explicit CPU snapshot")
}

fn owned(data: cpu) -> t {
    t(Rc::new(texture_data { backing: backing::Cpu(data), id: fresh_id() }))
}

/// Zero-copy construction for fresh immutable pixel arrays. The supplied array becomes backing
/// storage.
pub fn create_owned(width: i64, height: i64, pixels: Vec<color::T>) -> Result<t, String> {
    if width <= 0 || height <= 0 {
        return Err("Texture.create: dimensions must be positive".to_string());
    }
    let expected = width * height;
    if pixels.len() as i64 != expected {
        return Err(format!(
            "Texture.create: expected {expected} pixels for {width}x{height}, received {}",
            pixels.len()
        ));
    }
    Ok(owned(cpu { width, height, pixels, mipmaps: Vec::new() }))
}

pub fn init(width: i64, height: i64, make: &dyn Fn(i64, i64) -> color::T) -> Result<t, String> {
    if width <= 0 || height <= 0 {
        return Err("Texture.init: dimensions must be positive".to_string());
    }
    let pixels = (0..width * height).map(|index| make(index % width, index / width)).collect();
    Ok(owned(cpu { width, height, pixels, mipmaps: Vec::new() }))
}

pub fn size(texture: &t) -> Result<(i64, i64), String> {
    match &texture.0.backing {
        backing::Cpu(data) => Ok((data.width, data.height)),
        backing::Borrowed(image) => {
            Resource::size(image).map_err(|e| format!("Texture.size: {}", ogpu::error::to_string(&e)))
        }
    }
}

pub fn pixels(texture: &t) -> Result<Vec<color::T>, String> {
    Ok(cpu_ref(texture).ok_or_else(|| borrowed_message("Texture.pixels"))?.pixels.clone())
}

fn next_level(source_width: i64, source_height: i64, source: &[color::T]) -> level {
    let width = std::cmp::max(1, (source_width + 1) / 2);
    let height = std::cmp::max(1, (source_height + 1) / 2);
    let pixels = (0..width * height)
        .map(|index| {
            let x = index % width;
            let y = index / width;
            let (mut red, mut green, mut blue, mut alpha, mut count) = (0, 0, 0, 0, 0);
            for offset_y in 0..2 {
                for offset_x in 0..2 {
                    let source_x = x * 2 + offset_x;
                    let source_y = y * 2 + offset_y;
                    if source_x < source_width && source_y < source_height {
                        let c = source[(source_y * source_width + source_x) as usize];
                        red += c.r;
                        green += c.g;
                        blue += c.b;
                        alpha += c.a;
                        count += 1;
                    }
                }
            }
            let rounded = |value: i64| (value + count / 2) / count;
            color::rgba(rounded(red), rounded(green), rounded(blue), rounded(alpha))
        })
        .collect();
    level { width, height, pixels }
}

pub fn generate_mipmaps(texture: &t) -> Result<t, String> {
    let data = cpu_ref(texture).ok_or_else(|| borrowed_message("Texture.generate_mipmaps"))?;
    let mut mipmaps: Vec<level> = Vec::new();
    loop {
        let (width, height, pixels) = match mipmaps.last() {
            Some(l) => (l.width, l.height, &l.pixels[..]),
            None => (data.width, data.height, &data.pixels[..]),
        };
        if width == 1 && height == 1 {
            break;
        }
        let next = next_level(width, height, pixels);
        mipmaps.push(next);
    }
    Ok(owned(cpu { width: data.width, height: data.height, pixels: data.pixels.clone(), mipmaps }))
}

pub fn subsection(x: i64, y: i64, width: i64, height: i64, texture: &t) -> Result<t, String> {
    let source = match &texture.0.backing {
        backing::Borrowed(_) => {
            return Err("Texture.subsection: borrowed GPU texture needs an explicit CPU snapshot".to_string());
        }
        backing::Cpu(data) => data,
    };
    if width <= 0 || height <= 0 {
        return Err("Texture.subsection: dimensions must be positive".to_string());
    }
    if x < 0 || y < 0 || x + width > source.width || y + height > source.height {
        return Err("Texture.subsection: rectangle is outside the source texture".to_string());
    }
    let pixels = (0..width * height)
        .map(|index| {
            let target_x = index % width;
            let target_y = index / width;
            source.pixels[((y + target_y) * source.width + x + target_x) as usize]
        })
        .collect();
    Ok(owned(cpu { width, height, pixels, mipmaps: Vec::new() }))
}

fn wrap_coordinate(mode: wrap, value: f64) -> f64 {
    match mode {
        wrap::Clamp => fmax(0., fmin(1., value)),
        wrap::Repeat => {
            let value = value - value.floor();
            if value < 0. { value + 1. } else { value }
        }
        wrap::Mirror => {
            let value = value - (2. * (value / 2.).floor());
            let value = if value < 0. { value + 2. } else { value };
            if value <= 1. { value } else { 2. - value }
        }
    }
}

fn pack(c: color::T) -> i64 {
    (c.r << 24) | (c.g << 16) | (c.b << 8) | c.a
}

fn unpack(value: i64) -> color::T {
    color::rgba((value >> 24) & 0xff, (value >> 16) & 0xff, (value >> 8) & 0xff, value & 0xff)
}

fn packed_channel(value: i64, shift: i64) -> i64 {
    (value >> shift) & 0xff
}

fn blend_packed(left: i64, right: i64, amount: f64) -> i64 {
    let inverse = 1. - amount;
    let channel = |shift: i64| {
        int_of_float(packed_channel(left, shift) as f64 * inverse + packed_channel(right, shift) as f64 * amount + 0.5)
    };
    let (red, green, blue, alpha) = (channel(24), channel(16), channel(8), channel(0));
    (red << 24) | (green << 16) | (blue << 8) | alpha
}

fn packed_texel(pixels: &[color::T], width: i64, x: i64, y: i64) -> i64 {
    pack(pixels[(y * width + x) as usize])
}

fn sample_pixels_packed(
    filter_: filter,
    wrap_u: wrap,
    wrap_v: wrap,
    width: i64,
    height: i64,
    pixels: &[color::T],
    u: f64,
    v: f64,
) -> i64 {
    let u = wrap_coordinate(wrap_u, u);
    let v = wrap_coordinate(wrap_v, v);
    let x = u * (width - 1) as f64;
    let y = v * (height - 1) as f64;
    match filter_ {
        filter::Nearest => packed_texel(pixels, width, int_of_float(x.round()), int_of_float(y.round())),
        filter::Bilinear | filter::Trilinear => {
            let x0 = int_of_float(x.floor());
            let y0 = int_of_float(y.floor());
            let x1 = std::cmp::min(width - 1, x0 + 1);
            let y1 = std::cmp::min(height - 1, y0 + 1);
            let horizontal = x - x0 as f64;
            let vertical = y - y0 as f64;
            let top = blend_packed(packed_texel(pixels, width, x0, y0), packed_texel(pixels, width, x1, y0), horizontal);
            let bottom = blend_packed(packed_texel(pixels, width, x0, y1), packed_texel(pixels, width, x1, y1), horizontal);
            blend_packed(top, bottom, vertical)
        }
    }
}

fn sample_index_packed(filter_: filter, wrap_u: wrap, wrap_v: wrap, texture: &cpu, index: usize, u: f64, v: f64) -> i64 {
    if index == 0 {
        sample_pixels_packed(filter_, wrap_u, wrap_v, texture.width, texture.height, &texture.pixels, u, v)
    } else {
        let level = &texture.mipmaps[index - 1];
        sample_pixels_packed(filter_, wrap_u, wrap_v, level.width, level.height, &level.pixels, u, v)
    }
}

pub fn sample_lod_packed(
    filter_: Option<filter>,
    wrap_u: Option<wrap>,
    wrap_v: Option<wrap>,
    texture: &t,
    lod: f64,
    u: f64,
    v: f64,
) -> Result<i64, String> {
    let filter_ = filter_.unwrap_or(filter::Bilinear);
    let wrap_u = wrap_u.unwrap_or(wrap::Clamp);
    let wrap_v = wrap_v.unwrap_or(wrap::Clamp);
    if !lod.is_finite() {
        return Err("Texture.sample_lod: lod must be finite".to_string());
    }
    let texture = cpu_ref(texture).ok_or_else(|| borrowed_message("Texture.sample_lod"))?;
    let maximum = texture.mipmaps.len();
    let lod = fmax(0., fmin(maximum as f64, lod));
    Ok(match filter_ {
        filter::Nearest | filter::Bilinear => {
            let index = int_of_float(lod.round()) as usize;
            sample_index_packed(filter_, wrap_u, wrap_v, texture, index, u, v)
        }
        filter::Trilinear => {
            let lower = int_of_float(lod.floor()) as usize;
            let upper = std::cmp::min(maximum, lower + 1);
            let amount = lod - lower as f64;
            blend_packed(
                sample_index_packed(filter::Bilinear, wrap_u, wrap_v, texture, lower, u, v),
                sample_index_packed(filter::Bilinear, wrap_u, wrap_v, texture, upper, u, v),
                amount,
            )
        }
    })
}

pub fn sample_lod(
    filter_: Option<filter>,
    wrap_u: Option<wrap>,
    wrap_v: Option<wrap>,
    texture: &t,
    lod: f64,
    u: f64,
    v: f64,
) -> Result<color::T, String> {
    sample_lod_packed(filter_, wrap_u, wrap_v, texture, lod, u, v).map(unpack)
}

/// Sample normalized texture coordinates. (0, 0) is the upper-left texel.
pub fn sample(texture: &t, u: f64, v: f64) -> Result<color::T, String> {
    sample_lod(None, None, None, texture, 0., u, v)
}

pub fn of_image(image: &Resource::t) -> Result<t, String> {
    match Resource::gpu_snapshot(image) {
        Err(error) => Err(format!("Texture.Private.of_image: {}", ogpu::error::to_string(&error))),
        Ok(None) => Err("Texture.Private.of_image: resident GPU image required".to_string()),
        Ok(Some(_)) => Ok(t(Rc::new(texture_data { backing: backing::Borrowed(image.clone()), id: fresh_id() }))),
    }
}

/// Process-local identity: equal only for the same immutable texture value, so renderers can key
/// uploads without hashing pixels.
pub mod Private {
    use super::*;

    pub fn identity(texture: &t) -> i64 {
        texture.0.id
    }

    pub fn image(texture: &t) -> Option<Resource::t> {
        match &texture.0.backing {
            backing::Cpu(_) => None,
            backing::Borrowed(image) => Some(image.clone()),
        }
    }

    /// Packed RGBA levels, base level first: (width, height, pixels) copies.
    pub fn levels(texture: &t) -> Result<Vec<(i64, i64, Vec<color::T>)>, String> {
        let data = cpu_ref(texture).ok_or_else(|| borrowed_message("Texture.Private.levels"))?;
        let mut out = vec![(data.width, data.height, data.pixels.clone())];
        for l in &data.mipmaps {
            out.push((l.width, l.height, l.pixels.clone()));
        }
        Ok(out)
    }
}
