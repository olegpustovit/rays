//! Immutable indexed 3D geometry.
//!
//! Packed planes and index arrays are `Arc`s so `same` is identity in constant time; functions
//! OCaml raised `Invalid_argument` from return `Err(message)`.
use crate::num::{fmax, fmin};
use rays_math::color;
use rays_math::vec2;
use rays_math::vec3;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::{Arc, Mutex};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum mode {
    Points,
    Lines,
    Line_strip,
    Line_loop,
    Triangles,
    Triangle_strip,
    Triangle_fan,
}

/// Packed coordinate planes.
#[derive(Clone, Debug, Default)]
pub struct Vec3_buffer {
    pub x: Arc<Vec<f64>>,
    pub y: Arc<Vec<f64>>,
    pub z: Arc<Vec<f64>>,
}

pub type vec3_view = Vec3_buffer;

impl Vec3_buffer {
    pub fn length(&self) -> i64 {
        self.x.len() as i64
    }

    pub fn get(&self, index: usize) -> vec3::T {
        vec3::create(self.x[index], self.y[index], self.z[index])
    }

    pub fn of_array(values: &[vec3::T]) -> Vec3_buffer {
        Vec3_buffer {
            x: Arc::new(values.iter().map(|v| v.x).collect()),
            y: Arc::new(values.iter().map(|v| v.y).collect()),
            z: Arc::new(values.iter().map(|v| v.z).collect()),
        }
    }

    pub fn of_planes(x: Vec<f64>, y: Vec<f64>, z: Vec<f64>) -> Vec3_buffer {
        Vec3_buffer { x: Arc::new(x), y: Arc::new(y), z: Arc::new(z) }
    }

    pub fn to_array(&self) -> Vec<vec3::T> {
        (0..self.x.len()).map(|i| self.get(i)).collect()
    }

    fn same(&self, other: &Vec3_buffer) -> bool {
        Arc::ptr_eq(&self.x, &other.x) && Arc::ptr_eq(&self.y, &other.y) && Arc::ptr_eq(&self.z, &other.z)
    }
}

/// Positions (and normals) a device holds.
pub struct device {
    pub key: String,
    pub count: i64,
    pub device_normals: bool,
    pub device_index_count: Option<i64>,
    pub device_color_width: i64,
    /// Its normals made from its positions for display (it has none of its own): a tracer, which
    /// cusps a mesh without normals, reads the host copy.
    pub derived: bool,
    /// Held by the mesh: its producer watches it to release its buffers.
    pub keep: Rc<dyn Fn()>,
    host: Option<Rc<dyn Fn() -> Result<t, String>>>,
    host_made: RefCell<Option<t>>,
    device_bounds: Option<Rc<dyn Fn() -> Option<(vec3::T, vec3::T)>>>,
}

#[derive(Clone)]
pub struct t {
    mode: mode,
    vertices: Vec3_buffer,
    indices: Arc<Vec<i64>>,
    normals: Option<Vec3_buffer>,
    colors: Option<Arc<Vec<color::T>>>,
    tex_coords: Option<Arc<Vec<vec2::T>>>,
    device: Option<Rc<device>>,
}

fn same_colors(a: &Option<Arc<Vec<color::T>>>, b: &Option<Arc<Vec<color::T>>>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(x), Some(y)) => Arc::ptr_eq(x, y),
        _ => false,
    }
}

fn same_uvs(a: &Option<Arc<Vec<vec2::T>>>, b: &Option<Arc<Vec<vec2::T>>>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(x), Some(y)) => Arc::ptr_eq(x, y),
        _ => false,
    }
}

/// The same mesh by identity: its packed arrays physically, a device mesh by its key. O(1): a
/// structural compare walks every index of a mesh a live cook replaced.
pub fn same(a: &t, b: &t) -> bool {
    a.mode == b.mode
        && a.vertices.same(&b.vertices)
        && Arc::ptr_eq(&a.indices, &b.indices)
        && match (&a.normals, &b.normals) {
            (None, None) => true,
            (Some(x), Some(y)) => x.same(y),
            _ => false,
        }
        && same_colors(&a.colors, &b.colors)
        && same_uvs(&a.tex_coords, &b.tex_coords)
        && match (&a.device, &b.device) {
            (None, None) => true,
            (Some(x), Some(y)) => {
                x.key == y.key
                    && x.count == y.count
                    && x.device_normals == y.device_normals
                    && x.device_index_count == y.device_index_count
                    && x.device_color_width == y.device_color_width
                    && x.derived == y.derived
            }
            _ => false,
        }
}

/// A device mesh's host copy where a reader needs vertices; any other mesh itself.
fn readable(mesh: &t) -> Result<t, String> {
    if let Some(d) = &mesh.device {
        if let Some(host) = &d.host {
            if let Some(made) = d.host_made.borrow().as_ref() {
                return Ok(made.clone());
            }
            let made = host()?;
            *d.host_made.borrow_mut() = Some(made.clone());
            return Ok(made);
        }
    }
    Ok(mesh.clone())
}

fn count_error(name: &str, vertex_count: i64, len: usize) -> String {
    format!("Mesh.create: {name} count ({len}) must match vertex count ({vertex_count})")
}

fn colors_attribute(vertex_count: i64, values: Option<Vec<color::T>>) -> Result<Option<Arc<Vec<color::T>>>, String> {
    match values {
        None => Ok(None),
        Some(values) if values.len() as i64 == vertex_count => Ok(Some(Arc::new(values))),
        Some(values) => Err(count_error("color", vertex_count, values.len())),
    }
}

fn uv_attribute(vertex_count: i64, values: Option<Vec<vec2::T>>) -> Result<Option<Arc<Vec<vec2::T>>>, String> {
    match values {
        None => Ok(None),
        Some(values) if values.len() as i64 == vertex_count => Ok(Some(Arc::new(values))),
        Some(values) => Err(count_error("texture coordinate", vertex_count, values.len())),
    }
}

fn first_bad_index(indices: &[i64], vertex_count: i64) -> Option<i64> {
    indices.iter().copied().find(|&index| index < 0 || index >= vertex_count)
}

pub fn create_owned_full(
    mode_: Option<mode>,
    indices: Option<Vec<i64>>,
    normals: Option<Vec<vec3::T>>,
    colors: Option<Vec<color::T>>,
    tex_coords: Option<Vec<vec2::T>>,
    vertices: &[vec3::T],
) -> Result<t, String> {
    let vertex_count = vertices.len() as i64;
    let indices = indices.unwrap_or_else(|| (0..vertex_count).collect());
    if let Some(index) = first_bad_index(&indices, vertex_count) {
        return Err(format!("Mesh.create: index {index} is outside the vertex array"));
    }
    let normals = match normals {
        None => Ok(None),
        Some(values) if values.len() as i64 == vertex_count => Ok(Some(values)),
        Some(values) => Err(format!("Mesh.create: normal count ({}) must match vertex count ({vertex_count})", values.len())),
    };
    let colors = colors_attribute(vertex_count, colors);
    let tex_coords = uv_attribute(vertex_count, tex_coords);
    // OCaml reports the first failing of (normals, colors, tex_coords).
    let normals = normals?;
    let colors = colors?;
    let tex_coords = tex_coords?;
    Ok(t {
        mode: mode_.unwrap_or(mode::Triangles),
        vertices: Vec3_buffer::of_array(vertices),
        indices: Arc::new(indices),
        normals: normals.map(|n| Vec3_buffer::of_array(&n)),
        colors,
        tex_coords,
        device: None,
    })
}

pub fn create_packed_owned(
    mode_: Option<mode>,
    indices: Option<Vec<i64>>,
    normals: Option<Vec3_buffer>,
    colors: Option<Vec<color::T>>,
    tex_coords: Option<Vec<vec2::T>>,
    vertices: Vec3_buffer,
) -> Result<t, String> {
    let colors = match colors {
        None => None,
        Some(c) => Some(Arc::new(c)),
    };
    let tex_coords = match tex_coords {
        None => None,
        Some(c) => Some(Arc::new(c)),
    };
    create_packed_arcs(mode_, indices.map(Arc::new), normals, colors, tex_coords, vertices)
}

/// Zero-copy construction from already-published immutable packed storage: the arrays are
/// retained and may be shared with another immutable value.
pub fn create_packed_arcs(
    mode_: Option<mode>,
    indices: Option<Arc<Vec<i64>>>,
    normals: Option<Vec3_buffer>,
    colors: Option<Arc<Vec<color::T>>>,
    tex_coords: Option<Arc<Vec<vec2::T>>>,
    vertices: Vec3_buffer,
) -> Result<t, String> {
    let vertex_count = vertices.length();
    let indices: Arc<Vec<i64>> = indices.unwrap_or_else(|| Arc::new((0..vertex_count).collect()));
    if let Some(index) = first_bad_index(&indices, vertex_count) {
        return Err(format!("Mesh.create: index {index} is outside the vertex array"));
    }
    let normals = match normals {
        None => Ok(None),
        Some(values) if values.length() == vertex_count => Ok(Some(values)),
        Some(values) => Err(format!("Mesh.create: normal count ({}) must match vertex count ({vertex_count})", values.length())),
    };
    let colors_checked = match &colors {
        Some(c) if c.len() as i64 != vertex_count => Err(count_error("color", vertex_count, c.len())),
        _ => Ok(()),
    };
    let uv_checked = match &tex_coords {
        Some(c) if c.len() as i64 != vertex_count => Err(count_error("texture coordinate", vertex_count, c.len())),
        _ => Ok(()),
    };
    let normals = normals?;
    colors_checked?;
    uv_checked?;
    Ok(t {
        mode: mode_.unwrap_or(mode::Triangles),
        vertices,
        indices,
        normals,
        colors,
        tex_coords,
        device: None,
    })
}

pub fn mode(mesh: &t) -> mode {
    mesh.mode
}

pub fn vertices(mesh: &t) -> Result<Vec<vec3::T>, String> {
    Ok(readable(mesh)?.vertices.to_array())
}

pub fn vertex_count(mesh: &t) -> i64 {
    match &mesh.device {
        Some(d) => d.count,
        None => mesh.vertices.length(),
    }
}

pub fn index_count(mesh: &t) -> i64 {
    match &mesh.device {
        Some(d) if d.device_index_count.is_some() => d.device_index_count.unwrap_or(0),
        _ => mesh.indices.len() as i64,
    }
}

pub fn centroid(mesh: &t) -> Result<Option<vec3::T>, String> {
    let mesh = readable(mesh)?;
    let count = mesh.vertices.length();
    if count == 0 {
        return Ok(None);
    }
    let (mut x, mut y, mut z) = (0., 0., 0.);
    for index in 0..count as usize {
        x += mesh.vertices.x[index];
        y += mesh.vertices.y[index];
        z += mesh.vertices.z[index];
    }
    let inverse = 1. / count as f64;
    Ok(Some(vec3::create(x * inverse, y * inverse, z * inverse)))
}

/// The vertices' box: a device mesh's `bounds` where it has them (its host copy is not made),
/// otherwise a scan of the vertices.
pub fn bounds(mesh: &t) -> Result<Option<(vec3::T, vec3::T)>, String> {
    if let Some(d) = &mesh.device {
        if let Some(bounds) = &d.device_bounds {
            return Ok(bounds());
        }
    }
    let readable = readable(mesh)?;
    let v = &readable.vertices;
    if v.x.is_empty() {
        return Ok(None);
    }
    let mut lo = [f64::INFINITY; 3];
    let mut hi = [f64::NEG_INFINITY; 3];
    for i in 0..v.x.len() {
        lo[0] = fmin(lo[0], v.x[i]);
        lo[1] = fmin(lo[1], v.y[i]);
        lo[2] = fmin(lo[2], v.z[i]);
        hi[0] = fmax(hi[0], v.x[i]);
        hi[1] = fmax(hi[1], v.y[i]);
        hi[2] = fmax(hi[2], v.z[i]);
    }
    Ok(Some((vec3::create(lo[0], lo[1], lo[2]), vec3::create(hi[0], hi[1], hi[2]))))
}

pub fn vertex(index: i64, mesh: &t) -> Result<Option<vec3::T>, String> {
    let mesh = readable(mesh)?;
    Ok(if index < 0 || index >= mesh.vertices.length() { None } else { Some(mesh.vertices.get(index as usize)) })
}

pub fn with_mode(mode_: mode, mesh: &t) -> t {
    t { mode: mode_, ..mesh.clone() }
}

pub fn recalculate_normals(mesh: &t) -> t {
    if mesh.device.is_some() { mesh.clone() } else { recalculate_host_normals(mesh) }
}

fn recalculate_host_normals(mesh: &t) -> t {
    let vertex_count = mesh.vertices.length() as usize;
    let (mut nx, mut ny, mut nz) = (vec![0.; vertex_count], vec![0.; vertex_count], vec![0.; vertex_count]);
    let v = &mesh.vertices;
    let mut add_triangle = |a: usize, b: usize, c: usize| {
        let abx = v.x[b] - v.x[a];
        let aby = v.y[b] - v.y[a];
        let abz = v.z[b] - v.z[a];
        let acx = v.x[c] - v.x[a];
        let acy = v.y[c] - v.y[a];
        let acz = v.z[c] - v.z[a];
        let x = (aby * acz) - (abz * acy);
        let y = (abz * acx) - (abx * acz);
        let z = (abx * acy) - (aby * acx);
        for i in [a, b, c] {
            nx[i] += x;
            ny[i] += y;
            nz[i] += z;
        }
    };
    let values = &mesh.indices;
    let count = values.len() as i64;
    let at = |i: i64| values[i as usize] as usize;
    match mesh.mode {
        mode::Triangles => {
            for face in 0..count / 3 {
                let offset = face * 3;
                add_triangle(at(offset), at(offset + 1), at(offset + 2));
            }
        }
        mode::Triangle_strip => {
            for face in 0..(count - 2).max(0) {
                if face & 1 == 0 {
                    add_triangle(at(face), at(face + 1), at(face + 2));
                } else {
                    add_triangle(at(face + 1), at(face), at(face + 2));
                }
            }
        }
        mode::Triangle_fan => {
            for face in 0..(count - 2).max(0) {
                add_triangle(at(0), at(face + 1), at(face + 2));
            }
        }
        mode::Points | mode::Lines | mode::Line_strip | mode::Line_loop => {}
    }
    // ponytail: sequential; the OCaml loop was parallel with identical results, add a chunked
    // parallel fill when the parallel crate gets a safe disjoint-write helper.
    let (mut ox, mut oy, mut oz) = (Vec::with_capacity(vertex_count), Vec::with_capacity(vertex_count), Vec::with_capacity(vertex_count));
    for index in 0..vertex_count {
        let (x, y, z) = (nx[index], ny[index], nz[index]);
        let length = ((x * x) + (y * y) + (z * z)).sqrt();
        let n = if length <= 1e-18 { vec3::zero() } else { vec3::create(x / length, y / length, z / length) };
        ox.push(n.x);
        oy.push(n.y);
        oz.push(n.z);
    }
    t { normals: Some(Vec3_buffer::of_planes(ox, oy, oz)), ..mesh.clone() }
}

fn positive(name: &str, value: f64) -> Result<(), String> {
    if !value.is_finite() || value <= 0. {
        return Err(format!("Mesh.{name}: dimensions must be finite and positive"));
    }
    Ok(())
}

pub fn plane(width: f64, height: f64) -> Result<t, String> {
    positive("plane", width)?;
    positive("plane", height)?;
    let x = width * 0.5;
    let y = height * 0.5;
    let vertices = [
        vec3::create(-x, -y, 0.),
        vec3::create(x, -y, 0.),
        vec3::create(-x, y, 0.),
        vec3::create(x, y, 0.),
    ];
    let normals = vec![vec3::unit_z(); 4];
    let tex_coords = vec![vec2::create(0., 1.), vec2::create(1., 1.), vec2::create(0., 0.), vec2::create(1., 0.)];
    create_owned_full(None, Some(vec![0, 1, 3, 0, 3, 2]), Some(normals), None, Some(tex_coords), &vertices)
}

static DEVICE_IDENTITY: Mutex<Option<Arc<Vec<i64>>>> = Mutex::new(None);
static DEVICE_CHECKED: Mutex<Option<(Arc<Vec<i64>>, i64)>> = Mutex::new(None);

pub mod View {
    use super::*;

    pub struct view {
        pub mode: mode,
        pub vertices: Vec<vec3::T>,
        pub normals: Option<Vec<vec3::T>>,
        pub colors: Option<Arc<Vec<color::T>>>,
        pub tex_coords: Option<Arc<Vec<vec2::T>>>,
    }

    pub fn view(mesh: &t) -> Result<view, String> {
        let mesh = readable(mesh)?;
        Ok(view {
            mode: mesh.mode,
            vertices: mesh.vertices.to_array(),
            normals: mesh.normals.as_ref().map(|n| n.to_array()),
            colors: mesh.colors.clone(),
            tex_coords: mesh.tex_coords.clone(),
        })
    }

    #[derive(Clone)]
    pub struct packed_view {
        pub mode: mode,
        pub vertices: vec3_view,
        pub indices: Arc<Vec<i64>>,
        pub normals: Option<vec3_view>,
        pub colors: Option<Arc<Vec<color::T>>>,
        pub tex_coords: Option<Arc<Vec<vec2::T>>>,
    }

    /// The mesh's own arrays: a device mesh's indices, colours and texture coordinates with no
    /// vertices, its host copy never made.
    pub fn unread(mesh: &t) -> packed_view {
        packed_view {
            mode: mesh.mode,
            vertices: mesh.vertices.clone(),
            indices: mesh.indices.clone(),
            normals: mesh.normals.clone(),
            colors: mesh.colors.clone(),
            tex_coords: mesh.tex_coords.clone(),
        }
    }

    pub fn packed_view(mesh: &t) -> Result<packed_view, String> {
        Ok(unread(&readable(mesh)?))
    }

    /// Ownership-transfer construction for sibling-library generators.
    pub fn create_owned(mode_: Option<mode>, indices: Option<Vec<i64>>, vertices: &[vec3::T]) -> Result<t, String> {
        create_owned_full(mode_, indices, None, None, None, vertices)
    }

    pub fn create_packed_owned(
        mode_: Option<mode>,
        indices: Option<Vec<i64>>,
        normals: Option<vec3_view>,
        colors: Option<Vec<color::T>>,
        tex_coords: Option<Vec<vec2::T>>,
        vertices: vec3_view,
    ) -> Result<t, String> {
        super::create_packed_owned(mode_, indices, normals, colors, tex_coords, vertices)
    }

    /// Zero-copy construction from already-published immutable packed storage. The returned mesh
    /// retains the supplied arrays and may share them with another immutable value.
    pub fn create_packed_shared(
        mode_: Option<mode>,
        indices: Option<Arc<Vec<i64>>>,
        normals: Option<vec3_view>,
        colors: Option<Arc<Vec<color::T>>>,
        tex_coords: Option<Arc<Vec<vec2::T>>>,
        vertices: vec3_view,
    ) -> Result<t, String> {
        super::create_packed_arcs(mode_, indices, normals, colors, tex_coords, vertices)
    }

    /// Arguments of `create_device`; unnamed ones default as in OCaml.
    #[derive(Default)]
    pub struct device_args {
        pub mode: Option<mode>,
        pub indices: Option<Arc<Vec<i64>>>,
        pub colors: Option<Vec<color::T>>,
        pub tex_coords: Option<Vec<vec2::T>>,
        pub host: Option<Rc<dyn Fn() -> Result<t, String>>>,
        pub bounds: Option<Rc<dyn Fn() -> Option<(vec3::T, vec3::T)>>>,
        pub derived: bool,
        pub device_index_count: Option<i64>,
        pub device_color_width: i64,
        pub key: String,
        pub normals: bool,
        pub keep: Option<Rc<dyn Fn()>>,
    }

    /// A mesh of `count` vertices whose packed float3 positions (and, with `normals`, normals) a
    /// device holds, registered under `key`; colours, texture coordinates and indices stay host
    /// arrays unless their device metadata is supplied.
    pub fn create_device(args: device_args, count: i64) -> Result<t, String> {
        let device_args {
            mode: mode_,
            indices,
            colors,
            tex_coords,
            host,
            bounds,
            derived,
            device_index_count,
            device_color_width,
            key,
            normals,
            keep,
        } = args;
        let mode_ = mode_.unwrap_or(mode::Triangles);
        // a frame's device mesh has the last one's indices: the same cloud identity, the same
        // topology's corners, so neither is made or checked again
        let indices: Arc<Vec<i64>> = match indices {
            Some(indices) => indices,
            None if device_index_count.is_some() => Arc::new(Vec::new()),
            None => {
                let mut identity = DEVICE_IDENTITY.lock().expect("device identity");
                match identity.as_ref() {
                    Some(known) if known.len() as i64 == count => known.clone(),
                    _ => {
                        let made = Arc::new((0..count.max(0)).collect::<Vec<i64>>());
                        *identity = Some(made.clone());
                        made
                    }
                }
            }
        };
        let checked = {
            let mut known = DEVICE_CHECKED.lock().expect("device checked");
            match known.as_ref() {
                Some((arr, n)) if Arc::ptr_eq(arr, &indices) && *n == count => None,
                _ => {
                    let bad = first_bad_index(&indices, count);
                    if bad.is_none() {
                        *known = Some((indices.clone(), count));
                    }
                    bad
                }
            }
        };
        let max_string_length: i64 = 144115188075855863;
        if key.is_empty() || count <= 0 {
            return Err("Mesh.create_device: a device mesh has vertices".to_string());
        }
        let bad_indices = match device_index_count {
            Some(n) => n <= 0 || n > max_string_length / 4 || n % 3 != 0 || mode_ != mode::Triangles || !indices.is_empty(),
            None => false,
        };
        if bad_indices || ![0, 3, 4].contains(&device_color_width) || (device_color_width != 0 && (colors.is_some() || tex_coords.is_some())) {
            return Err("Mesh.create_device: incompatible device indices or colours".to_string());
        }
        if let Some(index) = checked {
            return Err(format!("Mesh.create_device: index {index} is outside the vertex array"));
        }
        let colors = colors_attribute(count, colors);
        let tex_coords = uv_attribute(count, tex_coords);
        let colors = colors?;
        let tex_coords = tex_coords?;
        Ok(t {
            mode: mode_,
            vertices: Vec3_buffer::default(),
            indices,
            normals: None,
            colors,
            tex_coords,
            device: Some(Rc::new(device {
                key,
                count,
                device_normals: normals,
                device_index_count,
                device_color_width,
                derived,
                keep: keep.unwrap_or_else(|| Rc::new(|| {})),
                host,
                host_made: RefCell::new(None),
                device_bounds: bounds,
            })),
        })
    }

    /// A device mesh's key and whether it carries normals.
    pub fn device(mesh: &t) -> Option<(String, bool)> {
        mesh.device.as_ref().map(|d| (d.key.clone(), d.device_normals))
    }

    /// The borrowed triangle index count, without reading any buffer.
    pub fn device_index_count(mesh: &t) -> Option<i64> {
        mesh.device.as_ref().and_then(|d| d.device_index_count)
    }

    /// 3 or 4 for colours the device holds under the mesh's key, otherwise 0.
    pub fn device_color_width(mesh: &t) -> i64 {
        mesh.device.as_ref().map_or(0, |d| d.device_color_width)
    }

    /// Whether a device mesh's normals were made from its positions for display.
    pub fn derived_normals(mesh: &t) -> bool {
        mesh.device.as_ref().is_some_and(|d| d.derived)
    }

    pub fn triangle_count(mesh: &t) -> i64 {
        match mesh.mode {
            mode::Triangles => index_count(mesh) / 3,
            mode::Triangle_strip | mode::Triangle_fan => (mesh.indices.len() as i64 - 2).max(0),
            mode::Points | mode::Lines | mode::Line_strip | mode::Line_loop => 0,
        }
    }
}
