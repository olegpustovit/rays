//! A World: an ordered stack of procedural environment layers, and the deterministic CPU bake that
//! turns it into equirectangular HDR maps plus extracted lights. Pure data: no GPU values. The
//! raster renderer and the path tracer read the same `baked` products.
//!
//! Conventions. +Y is up. Colors are linear-light floats, HDR, never clamped. A unit direction
//! `d = (x, y, z)` maps to the equirectangular coordinates `u = atan2 x (-z) / 2pi + 0.5` in
//! [0, 1) and `v = acos y / pi` in [0, 1]: -Z (the front, azimuth 0) is `u = 0.5`, azimuth grows
//! toward +X, row 0 is the zenith. Texel `(i, j)` of a `w x h` map holds the direction at
//! `((i + 0.5) / w, (j + 0.5) / h)`. `rotation` (radians, about +Y) turns the whole World, lights
//! and sun included: layers are evaluated at azimuth `phi - rotation`.
use crate::num::{fmax, fmin, int_of_float};
use renderer::side_lru::{self, Table, TableKey};
use rays_math::color::linear;
use rays_math::rand;
use rays_math::vec3;
use std::cell::RefCell;
use std::f64::consts::PI as pi;
use std::rc::Rc;
use std::sync::{Arc, LazyLock, Mutex};

pub type rgb = linear::T;

pub fn rgb(r: f64, g: f64, b: f64) -> rgb {
    linear::rgb(r, g, b)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum emit {
    /// Baked into both maps.
    Dome,
    /// Also extracted as rect area lights; it stays in the camera map and is left out of the
    /// lighting map.
    Light,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum shape {
    /// `width` x `height` angular box around its centre.
    Rect,
    /// A band along the parallel at `elevation`: `width` radians of azimuth (>= 2pi is a full
    /// ring), `height` radians thick.
    Strip,
    /// Disc of angular diameter `width`.
    Circle,
    /// Annulus of angular diameter `width`, `height` thick.
    Ring,
}

#[derive(Clone, Debug, PartialEq)]
pub enum layer {
    /// Base. Weight `1 - (1 - |y|) ** sharpness` from the horizon color toward zenith (y > 0) or
    /// nadir.
    Gradient { zenith: rgb, horizon: rgb, nadir: rgb, sharpness: f64 },
    /// Base. Nishita single scattering lit by the World sun; below the horizon it fades to a
    /// ground of 0.3 x the horizon color. Baked at 128x64 and bilinearly upsampled.
    Sky { turbidity: f64, intensity: f64 },
    /// A disc of radiance `intensity * tint` in the camera map only; lighting gets it through
    /// `baked.sun`. Dropped when the sun is below the horizon.
    Sun { angular_radius: f64, intensity: f64, tint: rgb },
    /// Adds `color * intensity`, falling off with a smoothstep over `softness` radians outside
    /// the edge.
    Shape {
        shape: shape,
        azimuth: f64,
        elevation: f64,
        width: f64,
        height: f64,
        softness: f64,
        color: rgb,
        intensity: f64,
        emit: emit,
    },
    /// `count` soft round blobs, all from `seed` only. `mirror` adds each blob reflected below
    /// the horizon at 0.25 x radiance; mirrors are always `Dome`.
    Scatter {
        seed: i64,
        count: i64,
        elevation_min: f64,
        elevation_max: f64,
        size_min: f64,
        size_max: f64,
        palette: Vec<rgb>,
        intensity: f64,
        mirror: bool,
        emit: emit,
    },
    /// Base. The interior of a box seen from the origin with a ceiling grid of panels.
    Room {
        width: f64,
        depth: f64,
        height: f64,
        rows: i64,
        columns: i64,
        panel: f64,
        intensity: f64,
        wall: rgb,
        cove: f64,
        emit: emit,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct entry {
    pub name: String,
    pub layer: layer,
    pub visible: bool,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum background {
    /// Show the camera map.
    Environment,
    Color(rgb),
    Transparent,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum sun_mode {
    /// Uses the linked sun direction.
    Linked,
    Manual(f64, f64),
}

#[derive(Clone, Debug, PartialEq)]
pub struct World {
    /// Bottom to top. A base layer (Gradient, Sky, Room) replaces everything below it; the others
    /// add radiance. Invisible entries are skipped, and anything below the topmost visible base
    /// layer is dropped (no maps, no lights).
    pub layers: Vec<entry>,
    /// Not baked: renderers read it at bounce 0.
    pub background: background,
    pub rotation: f64,
    /// Stops, not baked: carried to the renderers.
    pub exposure: f64,
    /// Hours, 0..24, local solar time.
    pub time_of_day: f64,
    /// Radians, north positive.
    pub latitude: f64,
    pub day_of_year: i64,
    pub sun: sun_mode,
}

/// Row-major RGB, 3 floats per texel, equirectangular as above.
#[derive(Clone, Debug)]
pub struct map {
    pub width: i64,
    pub height: i64,
    pub pixels: Arc<Vec<f64>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct light {
    pub position: vec3::T,
    /// Emitting side; faces the origin.
    pub normal: vec3::T,
    /// Half-extent axes: the rect is `position +- u +- v`.
    pub u: vec3::T,
    pub v: vec3::T,
    pub radiance: rgb,
}

/// `direction` points toward the sun; `radiance` is the disc's.
#[derive(Clone, Debug, PartialEq)]
pub struct sun {
    pub direction: vec3::T,
    pub radiance: rgb,
    pub angular_radius: f64,
}

/// Importance sampling of the lighting map by luminance x sin theta. A row or map with zero
/// weight is uniform.
#[derive(Clone, Debug)]
pub struct cdf {
    /// `height + 1` entries, 0 .. 1.
    pub marginal: Arc<Vec<f64>>,
    /// `height` rows of `width + 1`, 0 .. 1.
    pub conditional: Arc<Vec<f64>>,
}

type lazy_sh9 = LazyLock<Vec<rgb>, Box<dyn FnOnce() -> Vec<rgb> + Send + Sync>>;
type lazy_specular = LazyLock<Vec<map>, Box<dyn FnOnce() -> Vec<map> + Send + Sync>>;

#[derive(Clone)]
pub struct baked {
    /// Everything, sun disc included: what the eye sees.
    pub camera: map,
    /// `camera` minus the `Light` emitters and the sun disc.
    pub lighting: map,
    pub cdf: cdf,
    sh9: Arc<lazy_sh9>,
    specular: Arc<lazy_specular>,
    pub lights: Vec<light>,
    pub sun: Option<sun>,
    pub background: background,
    pub exposure: f64,
}

impl baked {
    /// Irradiance-ready SH: the lighting map's coefficients on the real basis, already multiplied
    /// by the cosine-lobe factors pi, 2pi/3, pi/4 (Ramamoorthi-Hanrahan). Computed when first
    /// forced.
    pub(crate) fn sh9(&self) -> &Vec<rgb> {
        &self.sh9
    }

    /// 6 GGX-prefiltered lighting mips for roughness 0, 0.2, .., 1: 128x64, 64x32, .., 4x2. Mip 0
    /// is a box downsample. Computed when first forced.
    pub(crate) fn specular(&self) -> &Vec<map> {
        &self.specular
    }
}

impl std::fmt::Debug for baked {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "baked {{ camera: {}x{}, lights: {} }}", self.camera.width, self.camera.height, self.lights.len())
    }
}

const TWO_PI: f64 = 2. * pi;

fn clamp1(x: f64) -> f64 {
    fmin(1., fmax(-1., x))
}

fn white() -> rgb {
    rgb(1., 1., 1.)
}

fn scale_rgb(k: f64, c: rgb) -> rgb {
    linear::T { r: k * c.r, g: k * c.g, b: k * c.b }
}

// ---- parallel rows ----

/// `init_array`: row `j` of `count` computed on the shared pool; results are kept in order, so the
/// output is identical for every domain count.
fn par_rows(count: i64, chunk_size: i64, f: &(dyn Fn(i64) -> Vec<f64> + Send + Sync)) -> Vec<Vec<f64>> {
    if count <= 0 {
        return Vec::new();
    }
    let slots: Mutex<Vec<Vec<f64>>> = Mutex::new(vec![Vec::new(); count as usize]);
    let _ = parallel::for_(chunk_size, 0, count - 1, &|j| {
        let row = f(j);
        slots.lock().expect("row slots")[j as usize] = row;
        Ok(())
    });
    slots.into_inner().expect("row slots")
}

// ---- mapping ----

pub fn direction_of_uv(u: f64, v: f64) -> vec3::T {
    let phi = (u - 0.5) * TWO_PI;
    let theta = v * pi;
    let s = theta.sin();
    vec3::create(s * phi.sin(), theta.cos(), -s * phi.cos())
}

fn direction(azimuth: f64, elevation: f64) -> vec3::T {
    let c = elevation.cos();
    vec3::create(c * azimuth.sin(), elevation.sin(), -c * azimuth.cos())
}

/// Turns azimuth by `a` (toward +X).
fn rotate(a: f64, d: vec3::T) -> vec3::T {
    let c = a.cos();
    let s = a.sin();
    vec3::create((d.x * c) - (d.z * s), d.y, (d.z * c) + (d.x * s))
}

fn sun_direction(latitude: f64, day_of_year: i64, hours: f64) -> vec3::T {
    let dec = -0.40910518 * (TWO_PI * (day_of_year + 10) as f64 / 365.).cos();
    let h = (hours - 12.) * pi / 12.;
    let east = -dec.cos() * h.sin();
    let north = (dec.sin() * latitude.cos()) - (dec.cos() * latitude.sin() * h.cos());
    let up = (dec.sin() * latitude.sin()) + (dec.cos() * latitude.cos() * h.cos());
    vec3::create(east, up, -north)
}

fn smooth_edge(soft: f64, dist: f64) -> f64 {
    if dist <= 0. {
        1.
    } else if dist >= soft {
        0.
    } else {
        let t = dist / soft;
        1. - (t * t * (3. - (2. * t)))
    }
}

// ---- bilinear sampling ----

fn sample_into(m: &map, u: f64, v: f64) -> [f64; 3] {
    let fx = (u * m.width as f64) - 0.5;
    let fy = (v * m.height as f64) - 0.5;
    let x0 = fx.floor();
    let y0 = fy.floor();
    let tx = fx - x0;
    let ty = fy - y0;
    let w = m.width;
    let x0 = int_of_float(x0);
    let y0 = int_of_float(y0);
    let xa = ((x0 % w) + w) % w;
    let xb = (xa + 1) % w;
    let ya = std::cmp::max(0, std::cmp::min(m.height - 1, y0)) * w;
    let yb = std::cmp::max(0, std::cmp::min(m.height - 1, y0 + 1)) * w;
    let p = &m.pixels;
    let aa = ((ya + xa) * 3) as usize;
    let ba = ((ya + xb) * 3) as usize;
    let ab = ((yb + xa) * 3) as usize;
    let bb = ((yb + xb) * 3) as usize;
    let mut out = [0.; 3];
    for c in 0..3 {
        let top = p[aa + c] + (tx * (p[ba + c] - p[aa + c]));
        let bottom = p[ab + c] + (tx * (p[bb + c] - p[ab + c]));
        out[c] = top + (ty * (bottom - top));
    }
    out
}

// ---- Nishita sky ----

const EARTH: f64 = 6360e3;
const ATMOSPHERE: f64 = 6420e3;
const BETA_R: [f64; 3] = [5.8e-6, 13.5e-6, 33.1e-6];

/// Distance from a point inside a sphere to where the ray leaves it.
fn exit_distance(ox: f64, oy: f64, oz: f64, dx: f64, dy: f64, dz: f64, radius: f64) -> f64 {
    let b = (ox * dx) + (oy * dy) + (oz * dz);
    let c = (ox * ox) + (oy * oy) + (oz * oz) - (radius * radius);
    -b + ((b * b) - c).sqrt()
}

fn nishita(turbidity: f64, intensity: f64, s: vec3::T, dx: f64, dy: f64, dz: f64) -> [f64; 3] {
    let views = 12_i64;
    let steps = 4_i64;
    let g = 0.76_f64;
    let oy = EARTH + 1.;
    let seg = exit_distance(0., oy, 0., dx, dy, dz, ATMOSPHERE) / views as f64;
    let mu = (dx * s.x) + (dy * s.y) + (dz * s.z);
    let phase_r = 3. / (16. * pi) * (1. + (mu * mu));
    let phase_m = 3. / (8. * pi) * ((1. - (g * g)) * (1. + (mu * mu))) / ((2. + (g * g)) * (1. + (g * g) - (2. * g * mu)).powf(1.5));
    let beta_m = 21e-6 * turbidity;
    let mut sum_r = [0.; 3];
    let mut sum_m = [0.; 3];
    let mut od_r = 0.;
    let mut od_m = 0.;
    for i in 0..views {
        let t = seg * (i as f64 + 0.5);
        let px = dx * t;
        let py = oy + (dy * t);
        let pz = dz * t;
        let h = ((px * px) + (py * py) + (pz * pz)).sqrt() - EARTH;
        let hr = (-h / 7994.).exp() * seg;
        let hm = (-h / 1200.).exp() * seg;
        od_r += hr;
        od_m += hm;
        let seg_l = exit_distance(px, py, pz, s.x, s.y, s.z, ATMOSPHERE) / steps as f64;
        let mut lr = 0.;
        let mut lm = 0.;
        let mut lit = true;
        let mut j = 0;
        while lit && j < steps {
            let t = seg_l * (j as f64 + 0.5);
            let qx = px + (s.x * t);
            let qy = py + (s.y * t);
            let qz = pz + (s.z * t);
            let hl = ((qx * qx) + (qy * qy) + (qz * qz)).sqrt() - EARTH;
            if hl < 0. {
                lit = false;
            } else {
                lr += (-hl / 7994.).exp() * seg_l;
                lm += (-hl / 1200.).exp() * seg_l;
            }
            j += 1;
        }
        if lit {
            for c in 0..3 {
                let tau = (BETA_R[c] * (od_r + lr)) + (beta_m * 1.1 * (od_m + lm));
                let a = (-tau).exp();
                sum_r[c] += a * hr;
                sum_m[c] += a * hm;
            }
        }
    }
    let mut out = [0.; 3];
    for c in 0..3 {
        out[c] = ((sum_r[c] * BETA_R[c] * phase_r) + (sum_m[c] * beta_m * phase_m)) * 20. * intensity;
    }
    out
}

// ponytail: the sky is baked at a fixed 128x64 and upsampled; a sharper Mie halo or horizon needs
// a larger sky map.
fn sky_map(turbidity: f64, intensity: f64, sun: vec3::T) -> map {
    let w = 128_i64;
    let h = 64_i64;
    let column = move |i: i64| (((i as f64 + 0.5) / w as f64) - 0.5) * TWO_PI;
    let horizon = par_rows(w, 8, &move |i| {
        let phi = column(i);
        nishita(turbidity, intensity, sun, phi.sin(), 0., -phi.cos()).to_vec()
    });
    let rows = par_rows(h, 2, &|j| {
        let theta = (j as f64 + 0.5) / h as f64 * pi;
        let y = theta.cos();
        let s = theta.sin();
        let mut row = vec![0.; (w * 3) as usize];
        for i in 0..w {
            let o = (i * 3) as usize;
            if y >= 0. {
                let phi = column(i);
                let px = nishita(turbidity, intensity, sun, s * phi.sin(), y, -s * phi.cos());
                row[o..o + 3].copy_from_slice(&px);
            } else {
                let k = 1. - (0.7 * (1. - smooth_edge(0.1, -y)));
                for c in 0..3 {
                    row[o + c] = k * horizon[i as usize][c];
                }
            }
        }
        row
    });
    map { width: w, height: h, pixels: Arc::new(rows.concat()) }
}

// ---- layer preparation ----

/// A soft round emitter: disc (ring = false) or annulus, in local space.
#[derive(Clone)]
struct round {
    cx: f64,
    cy: f64,
    cz: f64,
    theta: f64,
    bound: f64,
    cos_bound: f64,
    radius: f64,
    half: f64,
    ring: bool,
    soft: f64,
    color: rgb,
    promote: bool,
}

enum prepared {
    /// one rgb per row
    Fill(Vec<f64>),
    Sky_map(map),
    Disc { x: f64, y: f64, z: f64, cos_r: f64, color: rgb },
    Rounds(Vec<round>),
    Box {
        c: vec3::T,
        t: vec3::T,
        b: vec3::T,
        hw: f64,
        hh: f64,
        soft: f64,
        cos_bound: f64,
        color: rgb,
        promote: bool,
    },
    Band {
        azimuth: f64,
        hw: f64,
        elevation: f64,
        hh: f64,
        soft: f64,
        y_lo: f64,
        y_hi: f64,
        color: rgb,
        promote: bool,
    },
    Box_room {
        hw: f64,
        hh: f64,
        hd: f64,
        rows: i64,
        columns: i64,
        panel: f64,
        intensity: f64,
        ambient: rgb,
        cove: f64,
        promote: bool,
    },
}

fn is_base(l: &layer) -> bool {
    matches!(l, layer::Gradient { .. } | layer::Sky { .. } | layer::Room { .. })
}

/// Visible entries from the topmost visible base layer up.
fn effective(world: &World) -> Vec<layer> {
    let mut kept: Vec<layer> = Vec::new();
    for e in &world.layers {
        if !e.visible {
            continue;
        }
        if is_base(&e.layer) {
            kept = vec![e.layer.clone()];
        } else {
            kept.insert(0, e.layer.clone());
        }
    }
    kept.reverse();
    kept
}

fn local_sun(world: &World) -> vec3::T {
    match world.sun {
        sun_mode::Linked => sun_direction(world.latitude, world.day_of_year, world.time_of_day),
        sun_mode::Manual(azimuth, elevation) => direction(azimuth, elevation),
    }
}

fn tangent(azimuth: f64) -> vec3::T {
    vec3::create(azimuth.cos(), 0., azimuth.sin())
}

fn bitangent(azimuth: f64, elevation: f64) -> vec3::T {
    vec3::create(-elevation.sin() * azimuth.sin(), elevation.cos(), elevation.sin() * azimuth.cos())
}

fn make_round(azimuth: f64, elevation: f64, radius: f64, half: f64, ring: bool, soft: f64, color: rgb, promote: bool) -> round {
    let c = direction(azimuth, elevation);
    let bound = fmin(pi, radius + half + soft);
    round {
        cx: c.x,
        cy: c.y,
        cz: c.z,
        theta: clamp1(c.y).acos(),
        bound,
        cos_bound: bound.cos(),
        radius,
        half,
        ring,
        soft,
        color,
        promote,
    }
}

/// Blob `k` of a scatter, keyed only by seed and index: (azimuth, elevation, size, color).
fn blobs(seed: i64, count: i64, elevation_min: f64, elevation_max: f64, size_min: f64, size_max: f64, palette: &[rgb]) -> Vec<(f64, f64, f64, rgb)> {
    let rng = rand::seed(seed);
    (0..count.max(0))
        .map(|k| {
            let at = |n: i64| rand::float_at(rng, (4 * k) + n);
            let color = if palette.is_empty() {
                white()
            } else {
                palette[std::cmp::min(palette.len() as i64 - 1, int_of_float(at(3) * palette.len() as f64)) as usize]
            };
            (
                TWO_PI * at(0),
                elevation_min + ((elevation_max - elevation_min) * at(1)),
                size_min + ((size_max - size_min) * at(2)),
                color,
            )
        })
        .collect()
}

fn room_ambient(intensity: f64, panel: f64) -> f64 {
    intensity * panel * panel * 0.5
}

fn prepare(height: i64, sun: vec3::T, l: &layer) -> Option<prepared> {
    match l {
        layer::Gradient { zenith, horizon, nadir, sharpness } => {
            let mut rows = vec![0.; (height * 3) as usize];
            for j in 0..height {
                let y = ((j as f64 + 0.5) / height as f64 * pi).cos();
                let w = 1. - (1. - y.abs()).powf(*sharpness);
                let far = if y >= 0. { zenith } else { nadir };
                let o = (j * 3) as usize;
                rows[o] = horizon.r + (w * (far.r - horizon.r));
                rows[o + 1] = horizon.g + (w * (far.g - horizon.g));
                rows[o + 2] = horizon.b + (w * (far.b - horizon.b));
            }
            Some(prepared::Fill(rows))
        }
        layer::Sky { turbidity, intensity } => Some(prepared::Sky_map(sky_map(*turbidity, *intensity, sun))),
        layer::Sun { angular_radius, intensity, tint } => {
            if sun.y <= 0. {
                None
            } else {
                // The camera disc is at least 1.5 texels wide so it never vanishes at preview
                // resolution.
                let r = fmax(*angular_radius, 1.5 * pi / height as f64);
                Some(prepared::Disc { x: sun.x, y: sun.y, z: sun.z, cos_r: r.cos(), color: scale_rgb(*intensity, *tint) })
            }
        }
        layer::Shape { shape: shape_, azimuth, elevation, width, height: sh, softness, color, intensity, emit: emit_ } => {
            let color = scale_rgb(*intensity, *color);
            let promote = *emit_ == emit::Light;
            let soft = fmax(0., *softness);
            match shape_ {
                shape::Circle => Some(prepared::Rounds(vec![make_round(*azimuth, *elevation, width / 2., 0., false, soft, color, promote)])),
                shape::Ring => Some(prepared::Rounds(vec![make_round(*azimuth, *elevation, width / 2., sh / 2., true, soft, color, promote)])),
                shape::Rect => {
                    let a = (width / 2.) + soft;
                    let b = (sh / 2.) + soft;
                    let cos_bound = if a >= pi / 2. || b >= pi / 2. { 0. } else { (a.tan().hypot(b.tan())).atan().cos() };
                    Some(prepared::Box {
                        c: direction(*azimuth, *elevation),
                        t: tangent(*azimuth),
                        b: bitangent(*azimuth, *elevation),
                        hw: width / 2.,
                        hh: sh / 2.,
                        soft,
                        cos_bound,
                        color,
                        promote,
                    })
                }
                shape::Strip => {
                    let lo = elevation - (sh / 2.) - soft;
                    let hi = elevation + (sh / 2.) + soft;
                    Some(prepared::Band {
                        azimuth: *azimuth,
                        hw: width / 2.,
                        elevation: *elevation,
                        hh: sh / 2.,
                        soft,
                        y_lo: fmax(-pi / 2., lo).sin(),
                        y_hi: fmin(pi / 2., hi).sin(),
                        color,
                        promote,
                    })
                }
            }
        }
        layer::Scatter { seed, count, elevation_min, elevation_max, size_min, size_max, palette, intensity, mirror, emit: emit_ } => {
            let promote = *emit_ == emit::Light;
            let items = blobs(*seed, *count, *elevation_min, *elevation_max, *size_min, *size_max, palette);
            let mut rounds = Vec::new();
            for (azimuth, elevation, size, c) in items {
                let blob = |elevation: f64, k: f64, promote: bool| make_round(azimuth, elevation, 0., 0., false, size, scale_rgb(k * intensity, c), promote);
                if *mirror {
                    rounds.push(blob(elevation, 1., promote));
                    rounds.push(blob(-elevation, 0.25, false));
                } else {
                    rounds.push(blob(elevation, 1., promote));
                }
            }
            Some(prepared::Rounds(rounds))
        }
        layer::Room { width, depth, height: rh, rows, columns, panel, intensity, wall, cove, emit: emit_ } => Some(prepared::Box_room {
            hw: width / 2.,
            hh: rh / 2.,
            hd: depth / 2.,
            rows: std::cmp::max(1, *rows),
            columns: std::cmp::max(1, *columns),
            panel: *panel,
            intensity: *intensity,
            cove: *cove,
            promote: *emit_ == emit::Light,
            ambient: scale_rgb(room_ambient(*intensity, *panel), *wall),
        }),
    }
}

// ---- per-texel evaluation ----

/// acc: 0..2 lighting, 3..5 promoted (camera-only) radiance.
fn add(acc: &mut [f64; 12], promote: bool, k: f64, c: rgb) {
    let o = if promote { 3 } else { 0 };
    acc[o] += k * c.r;
    acc[o + 1] += k * c.g;
    acc[o + 2] += k * c.b;
}

fn base(acc: &mut [f64; 12], r: f64, g: f64, b: f64) {
    acc[0] = r;
    acc[1] = g;
    acc[2] = b;
    acc[3] = 0.;
    acc[4] = 0.;
    acc[5] = 0.;
}

/// The texel arrives in acc 6..11 (u, v, phi, x, y, z). `rounds` is the row's culled round items.
fn eval(acc: &mut [f64; 12], j: i64, prep: &prepared, rounds: Option<&[usize]>) {
    let u = acc[6];
    let v = acc[7];
    let phi = acc[8];
    let x = acc[9];
    let y = acc[10];
    let z = acc[11];
    match prep {
        prepared::Fill(rows) => {
            let o = (j * 3) as usize;
            base(acc, rows[o], rows[o + 1], rows[o + 2]);
        }
        prepared::Sky_map(m) => {
            let p = sample_into(m, u, v);
            base(acc, p[0], p[1], p[2]);
        }
        prepared::Disc { x: dx, y: dy, z: dz, cos_r, color } => {
            if (x * dx) + (y * dy) + (z * dz) >= *cos_r {
                add(acc, true, 1., *color);
            }
        }
        prepared::Rounds(items) => {
            let indices = rounds.unwrap_or(&[]);
            for &k in indices {
                let it = &items[k];
                let dot = (x * it.cx) + (y * it.cy) + (z * it.cz);
                if dot >= it.cos_bound {
                    let a = clamp1(dot).acos();
                    let dist = if it.ring { (a - it.radius).abs() - it.half } else { a - it.radius };
                    let m = smooth_edge(it.soft, dist);
                    if m > 0. {
                        add(acc, it.promote, m, it.color);
                    }
                }
            }
        }
        prepared::Box { c, t, b, hw, hh, soft, cos_bound, color, promote } => {
            let dc = (x * c.x) + (y * c.y) + (z * c.z);
            if dc > 0. && dc >= *cos_bound {
                let a = (((x * t.x) + (y * t.y) + (z * t.z)) / dc).atan();
                let bb = (((x * b.x) + (y * b.y) + (z * b.z)) / dc).atan();
                let m = smooth_edge(*soft, fmax(a.abs() - hw, bb.abs() - hh));
                if m > 0. {
                    add(acc, *promote, m, *color);
                }
            }
        }
        prepared::Band { azimuth, hw, elevation, hh, soft, y_lo, y_hi, color, promote } => {
            if y >= *y_lo && y <= *y_hi {
                let el = clamp1(y).asin();
                let da = ((phi - azimuth + (3. * pi)) % TWO_PI) - pi;
                let along = if *hw >= pi { f64::NEG_INFINITY } else { (da.abs() - hw) * el.cos() };
                let m = smooth_edge(*soft, fmax(along, (el - elevation).abs() - hh));
                if m > 0. {
                    add(acc, *promote, m, *color);
                }
            }
        }
        prepared::Box_room { hw, hh, hd, rows, columns, panel, intensity, ambient, cove, promote } => {
            let ax = x.abs();
            let ay = y.abs();
            let az = z.abs();
            let tx = if ax > 0. { hw / ax } else { f64::INFINITY };
            let ty = if ay > 0. { hh / ay } else { f64::INFINITY };
            let tz = if az > 0. { hd / az } else { f64::INFINITY };
            let t = fmin(tx, fmin(ty, tz));
            let a = ambient;
            if ty <= t && y > 0. {
                base(acc, 0.5 * a.r, 0.5 * a.g, 0.5 * a.b);
                let cell = |v: f64, half: f64, n: i64| {
                    let f = (v + half) / (2. * half) * n as f64;
                    let f = f - f.floor();
                    (f - 0.5).abs() < panel / 2.
                };
                if cell(x * t, *hw, *columns) && cell(z * t, *hd, *rows) {
                    add(acc, *promote, *intensity, white());
                }
            } else {
                base(acc, a.r, a.g, a.b);
                if ty > t && y * t > hh * 0.9 {
                    add(acc, false, *cove, white());
                }
            }
        }
    }
}

// ---- extracted lights ----

fn rect_light(rotation: f64, radius: f64, azimuth: f64, elevation: f64, half_w: f64, half_h: f64, radiance: rgb) -> light {
    let c = direction(azimuth, elevation);
    let cap = |a: f64| fmin(a, 1.4);
    let rot = |d: vec3::T| rotate(rotation, d);
    light {
        position: rot(vec3::scale(c, radius)),
        normal: rot(vec3::neg(c)),
        u: rot(vec3::scale(tangent(azimuth), radius * cap(half_w).tan())),
        v: rot(vec3::scale(bitangent(azimuth, elevation), radius * cap(half_h).tan())),
        radiance,
    }
}

// ponytail: every emitter becomes rect lights of its angular extent; round shapes scale radiance
// by area ratio (small-angle), softness is ignored.
fn extract(rotation: f64, radius: f64, l: &layer) -> Vec<light> {
    let rect = |azimuth, elevation, half_w, half_h, c| rect_light(rotation, radius, azimuth, elevation, half_w, half_h, c);
    match l {
        layer::Shape { emit: emit::Light, shape: shape_, azimuth, elevation, width, height, color, intensity, .. } => {
            let c = scale_rgb(*intensity, *color);
            match shape_ {
                shape::Rect => vec![rect(*azimuth, *elevation, width / 2., height / 2., c)],
                shape::Circle => vec![rect(*azimuth, *elevation, width / 2., width / 2., scale_rgb(pi / 4., c))],
                shape::Ring => {
                    let outer = (width / 2.) + (height / 2.);
                    let k = TWO_PI * (width / 2.) * height / (4. * outer * outer);
                    vec![rect(*azimuth, *elevation, outer, outer, scale_rgb(k, c))]
                }
                shape::Strip => {
                    let span = fmin(*width, TWO_PI);
                    let n = std::cmp::max(1, int_of_float((span / (pi / 3.)).ceil()));
                    (0..n)
                        .map(|k| {
                            let step = span / n as f64;
                            rect(azimuth - (span / 2.) + ((k as f64 + 0.5) * step), *elevation, step / 2., height / 2., c)
                        })
                        .collect()
                }
            }
        }
        layer::Scatter { emit: emit::Light, seed, count, elevation_min, elevation_max, size_min, size_max, palette, intensity, .. } => {
            blobs(*seed, *count, *elevation_min, *elevation_max, *size_min, *size_max, palette)
                .into_iter()
                .map(|(azimuth, elevation, size, c)| rect(azimuth, elevation, size / 2., size / 2., scale_rgb(0.3 * pi * intensity, c)))
                .collect()
        }
        layer::Room { emit: emit::Light, width, depth, height, rows, columns, panel, intensity, .. } => {
            let rows = std::cmp::max(1, *rows);
            let columns = std::cmp::max(1, *columns);
            let cw = width / columns as f64;
            let cd = depth / rows as f64;
            let rot = |d: vec3::T| rotate(rotation, d);
            (0..rows * columns)
                .map(|k| {
                    let i = k % columns;
                    let j = k / columns;
                    light {
                        position: rot(vec3::create((-width / 2.) + ((i as f64 + 0.5) * cw), height / 2., (-depth / 2.) + ((j as f64 + 0.5) * cd))),
                        normal: rot(vec3::create(0., -1., 0.)),
                        u: rot(vec3::create(cw * panel / 2., 0., 0.)),
                        v: rot(vec3::create(0., 0., cd * panel / 2.)),
                        radiance: rgb(*intensity, *intensity, *intensity),
                    }
                })
                .collect()
        }
        _ => Vec::new(),
    }
}

// ---- derived products ----

fn texel_solid_angle(width: i64, height: i64, j: i64) -> f64 {
    TWO_PI / width as f64 * (pi / height as f64) * ((j as f64 + 0.5) / height as f64 * pi).sin()
}

fn sh_basis(d: vec3::T, out: &mut [f64; 9]) {
    let (x, y, z) = (d.x, d.y, d.z);
    out[0] = 0.282095;
    out[1] = 0.488603 * y;
    out[2] = 0.488603 * z;
    out[3] = 0.488603 * x;
    out[4] = 1.092548 * x * y;
    out[5] = 1.092548 * y * z;
    out[6] = 0.315392 * ((3. * z * z) - 1.);
    out[7] = 1.092548 * x * z;
    out[8] = 0.546274 * ((x * x) - (y * y));
}

fn lobe() -> [f64; 9] {
    [pi, 2. * pi / 3., 2. * pi / 3., 2. * pi / 3., pi / 4., pi / 4., pi / 4., pi / 4., pi / 4.]
}

/// Per-row partial sums, then a fixed-order total: same bits on any domain count.
fn project_sh(m: &map) -> Vec<rgb> {
    let rows = par_rows(m.height, 4, &|j| {
        let mut sums = vec![0.; 27];
        let mut y = [0.; 9];
        let dw = texel_solid_angle(m.width, m.height, j);
        let v = (j as f64 + 0.5) / m.height as f64;
        for i in 0..m.width {
            sh_basis(direction_of_uv((i as f64 + 0.5) / m.width as f64, v), &mut y);
            let o = (((j * m.width) + i) * 3) as usize;
            for c in 0..3 {
                let l = m.pixels[o + c] * dw;
                for k in 0..9 {
                    sums[(k * 3) + c] += l * y[k];
                }
            }
        }
        sums
    });
    let mut total = [0.; 27];
    for sums in &rows {
        for (k, s) in sums.iter().enumerate() {
            total[k] += s;
        }
    }
    let lobe = lobe();
    (0..9).map(|k| rgb(lobe[k] * total[k * 3], lobe[k] * total[(k * 3) + 1], lobe[k] * total[(k * 3) + 2])).collect()
}

fn luminance(p: &[f64], o: usize) -> f64 {
    fmax(0., (0.2126 * p[o]) + (0.7152 * p[o + 1]) + (0.0722 * p[o + 2]))
}

/// Running sum normalised in place; uniform when the total is zero.
fn normalise(a: &mut [f64], o: usize, n: usize, total: f64) {
    for k in 0..=n {
        a[o + k] = if total > 0. { a[o + k] / total } else { k as f64 / n as f64 };
    }
    a[o + n] = 1.;
}

fn build_cdf(m: &map) -> cdf {
    let w = m.width as usize;
    let h = m.height as usize;
    let rows = par_rows(m.height, 8, &|j| {
        // one row of `w + 1` cumulative entries, normalised, and its raw sum in the last slot
        let s = ((j as f64 + 0.5) / h as f64 * pi).sin();
        let mut row = vec![0.; w + 2];
        let mut sum = 0.;
        for i in 0..w {
            sum += luminance(&m.pixels, ((j as usize * w) + i) * 3) * s;
            row[i + 1] = sum;
        }
        normalise(&mut row[..w + 1], 0, w, sum);
        row[w + 1] = sum;
        row
    });
    let mut conditional = Vec::with_capacity(h * (w + 1));
    let mut marginal = vec![0.; h + 1];
    let mut sum = 0.;
    for (j, row) in rows.iter().enumerate() {
        conditional.extend_from_slice(&row[..w + 1]);
        sum += row[w + 1];
        marginal[j + 1] = sum;
    }
    normalise(&mut marginal, 0, h, sum);
    cdf { marginal: Arc::new(marginal), conditional: Arc::new(conditional) }
}

/// Box average when shrinking, bilinear when growing.
fn resample(src: &map, w: i64, h: i64) -> map {
    let rows = par_rows(h, 8, &|j| {
        let mut row = vec![0.; (w * 3) as usize];
        for i in 0..w {
            let o = (i * 3) as usize;
            if src.width >= w && src.height >= h {
                let x0 = i * src.width / w;
                let x1 = std::cmp::max((i * src.width / w) + 1, (i + 1) * src.width / w);
                let y0 = j * src.height / h;
                let y1 = std::cmp::max((j * src.height / h) + 1, (j + 1) * src.height / h);
                let n = ((x1 - x0) * (y1 - y0)) as f64;
                for c in 0..3 {
                    let mut s = 0.;
                    for y in y0..y1 {
                        for x in x0..x1 {
                            s += src.pixels[((((y * src.width) + x) * 3) + c as i64) as usize];
                        }
                    }
                    row[o + c] = s / n;
                }
            } else {
                let px = sample_into(src, (i as f64 + 0.5) / w as f64, (j as f64 + 0.5) / h as f64);
                row[o..o + 3].copy_from_slice(&px);
            }
        }
        row
    });
    map { width: w, height: h, pixels: Arc::new(rows.concat()) }
}

fn radical_inverse(i: i64) -> f64 {
    let mut i = i;
    let mut bit = 0.5;
    let mut acc = 0.;
    while i != 0 {
        if i & 1 == 1 {
            acc += bit;
        }
        i >>= 1;
        bit *= 0.5;
    }
    acc
}

// ponytail: 32 fixed Hammersley GGX samples per texel from the previous box level (Karis
// split-sum, N = V = R); fine-grained lobes alias above that.
fn prefilter(src: &map, roughness: f64, w: i64, h: i64) -> map {
    let samples = 32_usize;
    let a = roughness * roughness;
    // Half vectors in the (t, b, n) frame.
    let mut hx = vec![0.; samples];
    let mut hy = vec![0.; samples];
    let mut hz = vec![0.; samples];
    for s in 0..samples {
        let e1 = (s as f64 + 0.5) / samples as f64;
        let e2 = radical_inverse(s as i64);
        let cos_t = ((1. - e2) / (1. + (((a * a) - 1.) * e2))).sqrt();
        let sin_t = (1. - (cos_t * cos_t)).sqrt();
        let phi = TWO_PI * e1;
        hx[s] = sin_t * phi.cos();
        hy[s] = sin_t * phi.sin();
        hz[s] = cos_t;
    }
    let rows = par_rows(h, 1, &|j| {
        let mut row = vec![0.; (w * 3) as usize];
        let theta = (j as f64 + 0.5) / h as f64 * pi;
        let ny = theta.cos();
        let st = theta.sin();
        for i in 0..w {
            let phi = (((i as f64 + 0.5) / w as f64) - 0.5) * TWO_PI;
            let nx = st * phi.sin();
            let nz = -st * phi.cos();
            // t = normalize (Y x n), b = n x t; texel centres never hit a pole.
            let tx = -phi.cos();
            let tz = -phi.sin();
            let bx = ny * tz;
            let by = (nz * tx) - (nx * tz);
            let bz = -ny * tx;
            let (mut sr, mut sg, mut sb, mut sw) = (0., 0., 0., 0.);
            for s in 0..samples {
                let a = hx[s];
                let c = hy[s];
                let d = hz[s];
                let k = 2. * d;
                let lx = (k * ((a * tx) + (c * bx) + (d * nx))) - nx;
                let ly = (k * ((c * by) + (d * ny))) - ny;
                let lz = (k * ((a * tz) + (c * bz) + (d * nz))) - nz;
                let nl = (2. * d * d) - 1.;
                if nl > 0. {
                    let u = (lx.atan2(-lz) / TWO_PI) + 0.5;
                    let v = clamp1(ly).acos() / pi;
                    let px = sample_into(src, u, v);
                    sr += nl * px[0];
                    sg += nl * px[1];
                    sb += nl * px[2];
                    sw += nl;
                }
            }
            let o = (i * 3) as usize;
            let k = if sw > 0. { 1. / sw } else { 0. };
            row[o] = sr * k;
            row[o + 1] = sg * k;
            row[o + 2] = sb * k;
        }
        row
    });
    map { width: w, height: h, pixels: Arc::new(rows.concat()) }
}

fn specular_chain(box_: &map) -> Vec<map> {
    let mut boxes: Vec<map> = vec![box_.clone()];
    for k in 1..6 {
        let next = resample(&boxes[k - 1], 128 >> k, 64 >> k);
        boxes.push(next);
    }
    (0..6)
        .map(|k| if k == 0 { boxes[0].clone() } else { prefilter(&boxes[k - 1], 0.2 * k as f64, 128 >> k, 64 >> k) })
        .collect()
}

// ---- bake ----

/// Byte-identical for every `domains` (default: recommended). `Light` shapes and blobs become
/// rect lights at distance `light_radius` (default 10.) of angular size preserved
/// (`2 R tan half-angle`); strips split into segments of at most 60 degrees; room panels stay at
/// their true position in the box.
pub fn bake(domains: Option<i64>, light_radius: Option<f64>, width: i64, height: i64, world: &World) -> Result<baked, String> {
    let light_radius = light_radius.unwrap_or(10.);
    if width < 1 || height < 1 {
        return Err("World.bake: empty map".to_string());
    }
    let sun_ = local_sun(world);
    let layers = effective(world);
    let rotation = world.rotation;
    let lights: Vec<light> = layers.iter().flat_map(|l| extract(rotation, light_radius, l)).collect();
    let mut sun_out = None;
    for l in &layers {
        if let layer::Sun { angular_radius, intensity, tint } = l
            && sun_.y > 0. {
                sun_out = Some(sun { direction: rotate(rotation, sun_), angular_radius: *angular_radius, radiance: scale_rgb(*intensity, *tint) });
            }
    }
    // Local azimuth per column: layers are evaluated at phi - rotation.
    let phis: Vec<f64> = (0..width)
        .map(|i| {
            let phi = ((((i as f64 + 0.5) / width as f64) - 0.5) * TWO_PI) - rotation;
            phi - (TWO_PI * ((phi + pi) / TWO_PI).floor())
        })
        .collect();
    let sin_phis: Vec<f64> = phis.iter().map(|p| p.sin()).collect();
    let cos_phis: Vec<f64> = phis.iter().map(|p| p.cos()).collect();
    let produced: RefCell<Option<(Vec<f64>, Vec<f64>)>> = RefCell::new(None);
    parallel::run_unit(domains, &|| {
        let prepared_layers: Vec<prepared> = layers.iter().filter_map(|l| prepare(height, sun_, l)).collect();
        let rows = par_rows(height, 4, &|j| {
            let v = (j as f64 + 0.5) / height as f64;
            let theta = v * pi;
            let y = theta.cos();
            let s = theta.sin();
            // Cull round items whose polar band misses this row.
            let row: Vec<Option<Vec<usize>>> = prepared_layers
                .iter()
                .map(|p| match p {
                    prepared::Rounds(items) => {
                        Some((0..items.len()).filter(|&k| (theta - items[k].theta).abs() <= items[k].bound).collect())
                    }
                    _ => None,
                })
                .collect();
            let mut out = vec![0.; (width * 6) as usize];
            let mut acc = [0.; 12];
            acc[7] = v;
            acc[10] = y;
            for i in 0..width as usize {
                let phi = phis[i];
                acc[..6].fill(0.);
                acc[6] = (phi / TWO_PI) + 0.5;
                acc[8] = phi;
                acc[9] = s * sin_phis[i];
                acc[11] = -s * cos_phis[i];
                for (k, p) in prepared_layers.iter().enumerate() {
                    eval(&mut acc, j, p, row[k].as_deref());
                }
                for c in 0..3 {
                    let l = acc[c];
                    out[i * 6 + c] = l;
                    out[i * 6 + 3 + c] = l + acc[c + 3];
                }
            }
            out
        });
        let mut lighting = Vec::with_capacity((width * height * 3) as usize);
        let mut camera = Vec::with_capacity((width * height * 3) as usize);
        for row in &rows {
            for i in 0..width as usize {
                lighting.extend_from_slice(&row[i * 6..i * 6 + 3]);
                camera.extend_from_slice(&row[i * 6 + 3..i * 6 + 6]);
            }
        }
        *produced.borrow_mut() = Some((lighting, camera));
        Ok(())
    })?;
    let (lighting, camera) = produced.into_inner().ok_or_else(|| "World.bake: no result".to_string())?;
    let lighting = map { width, height, pixels: Arc::new(lighting) };
    // ponytail: SH9 is projected from the 128x64 box level, not the full map; exact enough for 9
    // coefficients.
    let cdf_ = parallel_cdf(domains, &lighting)?;
    // ponytail: the raster products are forced on one domain at a time (the raster lowering); the
    // path tracer never reads them
    let box_source = lighting.clone();
    let box_: Arc<LazyLock<map, Box<dyn FnOnce() -> map + Send + Sync>>> = Arc::new(LazyLock::new(Box::new(move || resample(&box_source, 128, 64))));
    let box_sh = box_.clone();
    let box_spec = box_;
    let sh9: Arc<lazy_sh9> = Arc::new(LazyLock::new(Box::new(move || {
        let out: RefCell<Vec<rgb>> = RefCell::new(Vec::new());
        let _ = parallel::run_unit(domains, &|| {
            *out.borrow_mut() = project_sh(&box_sh);
            Ok(())
        });
        out.into_inner()
    })));
    let specular: Arc<lazy_specular> = Arc::new(LazyLock::new(Box::new(move || {
        let out: RefCell<Vec<map>> = RefCell::new(Vec::new());
        let _ = parallel::run_unit(domains, &|| {
            *out.borrow_mut() = specular_chain(&box_spec);
            Ok(())
        });
        out.into_inner()
    })));
    Ok(baked {
        camera: map { width, height, pixels: Arc::new(camera) },
        lighting,
        cdf: cdf_,
        sh9,
        specular,
        lights,
        sun: sun_out,
        background: world.background,
        exposure: world.exposure,
    })
}

fn parallel_cdf(domains: Option<i64>, lighting: &map) -> Result<cdf, String> {
    let out: RefCell<Option<cdf>> = RefCell::new(None);
    parallel::run_unit(domains, &|| {
        *out.borrow_mut() = Some(build_cdf(lighting));
        Ok(())
    })?;
    out.into_inner().ok_or_else(|| "World.bake: no cdf".to_string())
}

struct context_data {
    cache: RefCell<Table>,
}

/// A run-owned LRU of capacity 4 for `bake_cached`.
#[derive(Clone)]
pub struct cache_context(Rc<context_data>);

thread_local! {
    static CURRENT_CACHE: RefCell<Option<cache_context>> = const { RefCell::new(None) };
}

pub fn create_cache_context() -> cache_context {
    cache_context(Rc::new(context_data {
        cache: RefCell::new(side_lru::create(lru::cache("rays.world"), true, side_lru::UNBOUNDED_BYTES, Box::new(|_, _| {}), 4)),
    }))
}

struct Restore(Option<cache_context>);

impl Drop for Restore {
    fn drop(&mut self) {
        let previous = self.0.take();
        CURRENT_CACHE.with(|c| *c.borrow_mut() = previous);
    }
}

pub mod private {
    pub use super::cache_context;
    pub use super::create_cache_context;

    pub(crate) fn with_cache_context(context: &cache_context, f: &mut dyn FnMut()) {
        let previous = super::CURRENT_CACHE.with(|c| c.borrow_mut().replace(context.clone()));
        let _restore = super::Restore(previous);
        f()
    }
}

fn baked_bytes(b: &baked) -> i64 {
    let map_bytes = |m: &map| (m.pixels.len() * 8) as i64;
    map_bytes(&b.camera) + map_bytes(&b.lighting) + ((b.cdf.marginal.len() + b.cdf.conditional.len()) * 8) as i64
}

/// `bake` behind a run-owned LRU of capacity 4 keyed structurally; changing only `background` or
/// `exposure` is a hit.
pub fn bake_cached(width: i64, height: i64, world: &World) -> Result<baked, String> {
    let light_radius = 10.;
    let plain = World { background: background::Environment, exposure: 0., ..world.clone() };
    let key = TableKey::Str(format!("{plain:?}|{width}|{height}|{light_radius:?}"));
    let context = CURRENT_CACHE.with(|c| c.borrow().clone());
    let baked_ = match context {
        None => bake(None, Some(light_radius), width, height, &plain)?,
        Some(context) => {
            let found = side_lru::find(&mut context.0.cache.borrow_mut(), &key).and_then(|item| item.downcast::<baked>().ok());
            match found {
                Some(b) => (*b).clone(),
                None => {
                    let b = bake(None, Some(light_radius), width, height, &plain)?;
                    side_lru::add(&mut context.0.cache.borrow_mut(), baked_bytes(&b), key, Rc::new(b.clone()));
                    b
                }
            }
        }
    };
    Ok(baked { background: world.background, exposure: world.exposure, ..baked_ })
}

// ---- presets ----

fn entry_(name: &str, layer: layer) -> entry {
    entry { name: name.to_string(), layer, visible: true }
}

fn base_world(layers: Vec<entry>) -> World {
    World {
        layers,
        background: background::Environment,
        rotation: 0.,
        exposure: 0.,
        time_of_day: 14.,
        latitude: 0.7,
        day_of_year: 172,
        sun: sun_mode::Linked,
    }
}

/// A neutral grey studio gradient.
pub fn default() -> World {
    base_world(vec![entry_(
        "studio",
        layer::Gradient { zenith: rgb(0.8, 0.8, 0.8), horizon: rgb(0.5, 0.5, 0.5), nadir: rgb(0.2, 0.2, 0.2), sharpness: 1. },
    )])
}

fn strip(name: &str, elevation: f64, color: rgb, emit_: emit) -> entry {
    entry_(
        name,
        layer::Shape {
            shape: shape::Strip,
            azimuth: 0.,
            elevation,
            width: TWO_PI,
            height: 0.03,
            softness: 0.02,
            color,
            intensity: 6.,
            emit: emit_,
        },
    )
}

/// "neon corridor", "soft blobs", "white room", "daylight".
pub fn presets() -> Vec<(String, World)> {
    vec![
        (
            "neon corridor".to_string(),
            base_world(vec![
                entry_(
                    "dark",
                    layer::Gradient { zenith: rgb(0.01, 0.005, 0.02), horizon: rgb(0.02, 0.01, 0.03), nadir: rgb(0.005, 0.005, 0.01), sharpness: 2. },
                ),
                strip("magenta high", 0.45, rgb(1., 0.1, 0.6), emit::Light),
                strip("cyan high", 0.25, rgb(0.1, 0.8, 1.), emit::Light),
                strip("cyan low", -0.25, rgb(0.1, 0.8, 1.), emit::Dome),
                strip("magenta low", -0.45, rgb(1., 0.1, 0.6), emit::Dome),
            ]),
        ),
        (
            "soft blobs".to_string(),
            base_world(vec![
                entry_(
                    "dusk",
                    layer::Gradient { zenith: rgb(0.05, 0.04, 0.08), horizon: rgb(0.12, 0.08, 0.1), nadir: rgb(0.03, 0.03, 0.04), sharpness: 1.5 },
                ),
                entry_(
                    "blobs",
                    layer::Scatter {
                        seed: 7,
                        count: 24,
                        elevation_min: 0.05,
                        elevation_max: 1.2,
                        size_min: 0.08,
                        size_max: 0.3,
                        palette: vec![rgb(1., 0.6, 0.7), rgb(0.6, 0.8, 1.), rgb(1., 0.9, 0.6), rgb(0.7, 1., 0.8)],
                        intensity: 4.,
                        mirror: true,
                        emit: emit::Dome,
                    },
                ),
            ]),
        ),
        (
            "white room".to_string(),
            base_world(vec![entry_(
                "room",
                layer::Room {
                    width: 8.,
                    depth: 12.,
                    height: 4.,
                    rows: 3,
                    columns: 2,
                    panel: 0.6,
                    intensity: 8.,
                    wall: rgb(0.8, 0.8, 0.8),
                    cove: 2.,
                    emit: emit::Light,
                },
            )]),
        ),
        (
            "daylight".to_string(),
            base_world(vec![
                entry_("sky", layer::Sky { turbidity: 1., intensity: 1. }),
                entry_("sun", layer::Sun { angular_radius: 0.02, intensity: 2000., tint: rgb(1., 0.95, 0.9) }),
            ]),
        ),
    ]
}

/// `{ baked with background; exposure }` (the OCaml record update; the products are shared).
pub fn with_background(b: &baked, background: background, exposure: Option<f64>) -> baked {
    baked { background, exposure: exposure.unwrap_or(b.exposure), ..b.clone() }
}

/// `{ baked with sun }`.
pub fn with_sun(b: &baked, sun: Option<sun>) -> baked {
    baked { sun, ..b.clone() }
}
