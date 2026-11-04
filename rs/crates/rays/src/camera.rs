//! Immutable perspective and orthographic cameras.
//!
//! Functions OCaml documents as raising `Invalid_argument` return `Err(message)`.
use rays_math::mat4;
use rays_math::vec3;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum projection {
    Perspective { fov_y: f64, near: f64, far: f64 },
    Orthographic { height: f64, near: f64, far: f64 },
}

/// Thin-lens depth of field for renderers that model it (the path tracer): `aperture` is the lens
/// radius in scene units (0, the default, is a pinhole); `focus_distance` is the sharp distance
/// along the view direction, or `None` to focus on `target`. Raster views ignore the lens.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct lens {
    pub aperture: f64,
    pub focus_distance: Option<f64>,
}

pub fn pinhole() -> lens {
    lens { aperture: 0., focus_distance: None }
}

#[derive(Clone, Debug, PartialEq)]
pub struct t {
    position: vec3::T,
    target: vec3::T,
    up: vec3::T,
    projection: projection,
    lens: lens,
}

fn validate_view(position: vec3::T, target: vec3::T, up: vec3::T) -> Result<(), String> {
    if vec3::length_sq(vec3::sub(target, position)) == 0. {
        return Err("Camera: position and target must differ".to_string());
    }
    if vec3::length_sq(up) == 0. {
        return Err("Camera: up must be non-zero".to_string());
    }
    Ok(())
}

fn create(position: vec3::T, target: vec3::T, up: vec3::T, projection: projection) -> Result<t, String> {
    validate_view(position, target, up)?;
    Ok(t { position, target, up: vec3::normalize(up), projection, lens: pinhole() })
}

/// `fov_y` is a vertical angle in radians.
pub fn perspective(fov_y: Option<f64>, near: Option<f64>, far: Option<f64>, at: vec3::T, target: vec3::T) -> Result<t, String> {
    let fov_y = fov_y.unwrap_or(std::f64::consts::PI / 3.);
    let near = near.unwrap_or(0.1);
    let far = far.unwrap_or(1000.);
    mat4::perspective(fov_y, 1., near, far)?;
    create(at, target, vec3::unit_y(), projection::Perspective { fov_y, near, far })
}

pub fn orthographic(height: f64, at: vec3::T, target: vec3::T) -> Result<t, String> {
    let near = 0.1;
    let far = 1000.;
    if !height.is_finite() || height <= 0. {
        return Err("Camera.orthographic: height must be finite and positive".to_string());
    }
    create(at, target, vec3::unit_y(), projection::Orthographic { height, near, far })
}

pub fn position(camera: &t) -> vec3::T {
    camera.position
}

pub fn target(camera: &t) -> vec3::T {
    camera.target
}

pub fn up(camera: &t) -> vec3::T {
    camera.up
}

pub fn projection(camera: &t) -> projection {
    camera.projection
}

pub fn lens(camera: &t) -> lens {
    camera.lens
}

/// The lens's focus distance, or the distance to `target`.
pub fn focus_distance(camera: &t) -> f64 {
    match camera.lens.focus_distance {
        Some(distance) => distance,
        None => vec3::length(vec3::sub(camera.target, camera.position)),
    }
}

/// Errors for a negative or non-finite aperture or a non-positive focus distance.
pub fn with_lens(lens: lens, camera: &t) -> Result<t, String> {
    if !lens.aperture.is_finite() || lens.aperture < 0. {
        return Err("Camera.with_lens: aperture must be finite and non-negative".to_string());
    }
    if let Some(distance) = lens.focus_distance {
        if !distance.is_finite() || distance <= 0. {
            return Err("Camera.with_lens: focus distance must be finite and positive".to_string());
        }
    }
    Ok(t { lens, ..camera.clone() })
}

pub fn with_position(position: vec3::T, camera: &t) -> Result<t, String> {
    validate_view(position, camera.target, camera.up)?;
    Ok(t { position, ..camera.clone() })
}

pub fn with_up(up: vec3::T, camera: &t) -> Result<t, String> {
    validate_view(camera.position, camera.target, up)?;
    Ok(t { up: vec3::normalize(up), ..camera.clone() })
}

fn viewport_size(viewport: (i64, i64, i64, i64)) -> Result<(f64, f64), String> {
    let (_, _, width, height) = viewport;
    if width <= 0 || height <= 0 {
        return Err("Camera: viewport dimensions must be positive".to_string());
    }
    Ok((width as f64, height as f64))
}

fn view_matrix(camera: &t) -> Result<mat4::T, String> {
    mat4::look_at(camera.position, camera.target, camera.up)
}

fn projection_matrix(viewport: (i64, i64, i64, i64), camera: &t) -> Result<mat4::T, String> {
    let (width, height) = viewport_size(viewport)?;
    let aspect = width / height;
    match camera.projection {
        projection::Perspective { fov_y, near, far } => mat4::perspective(fov_y, aspect, near, far),
        projection::Orthographic { height, near, far } => {
            let half_height = height / 2.;
            let half_width = half_height * aspect;
            mat4::orthographic(-half_width, half_width, -half_height, half_height, near, far)
        }
    }
}

pub fn view_projection_matrix(viewport: (i64, i64, i64, i64), camera: &t) -> Result<mat4::T, String> {
    // OCaml evaluates the right operand of an application first: the view matrix, then the projection.
    let view = view_matrix(camera)?;
    let projection = projection_matrix(viewport, camera)?;
    Ok(mat4::mul(&projection, &view))
}

/// Logical screen x/y and normalized depth in [0, 1].
pub fn world_to_screen(viewport: (i64, i64, i64, i64), camera: &t, point: vec3::T) -> Result<Option<vec3::T>, String> {
    let (vx, vy, _, _) = viewport;
    let (width, height) = viewport_size(viewport)?;
    let matrix = view_projection_matrix(viewport, camera)?;
    let (x, y, z, w) = mat4::transform(&matrix, (point.x, point.y, point.z, 1.));
    if w <= 1e-12 {
        return Ok(None);
    }
    let ndc_x = x / w;
    let ndc_y = y / w;
    let ndc_z = z / w;
    Ok(Some(vec3::create(
        vx as f64 + ((ndc_x + 1.) * 0.5 * width),
        vy as f64 + ((1. - ndc_y) * 0.5 * height),
        (ndc_z + 1.) * 0.5,
    )))
}

fn screen_to_world(viewport: (i64, i64, i64, i64), camera: &t, point: vec3::T) -> Result<Option<vec3::T>, String> {
    let (vx, vy, _, _) = viewport;
    let (width, height) = viewport_size(viewport)?;
    match mat4::inverse(&view_projection_matrix(viewport, camera)?) {
        None => Ok(None),
        Some(inverse) => {
            let ndc_x = (2. * (point.x - vx as f64) / width) - 1.;
            let ndc_y = 1. - (2. * (point.y - vy as f64) / height);
            let ndc_z = (2. * point.z) - 1.;
            let (x, y, z, w) = mat4::transform(&inverse, (ndc_x, ndc_y, ndc_z, 1.));
            if w.abs() <= 1e-12 { Ok(None) } else { Ok(Some(vec3::create(x / w, y / w, z / w))) }
        }
    }
}

pub fn world_to_camera(camera: &t, point: vec3::T) -> Result<vec3::T, String> {
    Ok(mat4::transform_point(&view_matrix(camera)?, point))
}

/// A world-space ray origin and normalized direction.
pub fn screen_ray(viewport: (i64, i64, i64, i64), camera: &t, at: (f64, f64)) -> Result<Option<(vec3::T, vec3::T)>, String> {
    let (x, y) = at;
    // Tuple components evaluate right to left in OCaml: the far point first.
    let far = screen_to_world(viewport, camera, vec3::create(x, y, 1.))?;
    let near = screen_to_world(viewport, camera, vec3::create(x, y, 0.))?;
    Ok(match (near, far) {
        (Some(near), Some(far)) => Some((near, vec3::normalize(vec3::sub(far, near)))),
        _ => None,
    })
}

