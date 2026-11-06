//! Immutable light sources for `Scene3`.
use rays_math::color;
use rays_math::vec3;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct attenuation {
    pub constant: f64,
    pub quadratic: f64,
}

#[derive(Clone, Debug, PartialEq)]
pub enum kind {
    Directional { direction: vec3::T },
    Point { position: vec3::T, attenuation: attenuation },
    Spot { position: vec3::T, direction: vec3::T, cutoff: f64, concentration: f64, attenuation: attenuation },
    Area { position: vec3::T, direction: vec3::T, width: f64, height: f64, samples: i64, attenuation: attenuation },
}

/// `diffuse` tints both the diffuse and the specular response.
#[derive(Clone, Debug, PartialEq)]
pub struct t {
    pub kind: kind,
    pub diffuse: color::T,
    pub intensity: f64,
}

const NO_ATTENUATION: attenuation = attenuation { constant: 1., quadratic: 0. };

/// Falloff divisor `constant + quadratic * distance^2`. Both coefficients must be finite and
/// non-negative, with at least one non-zero.
pub fn attenuation(constant: Option<f64>, quadratic: Option<f64>) -> Result<attenuation, String> {
    let constant = constant.unwrap_or(1.);
    let quadratic = quadratic.unwrap_or(0.);
    if constant < 0. || quadratic < 0. || !constant.is_finite() || !quadratic.is_finite() {
        return Err("Light.attenuation: coefficients must be finite and non-negative".to_string());
    }
    if constant == 0. && quadratic == 0. {
        return Err("Light.attenuation: at least one coefficient must be non-zero".to_string());
    }
    Ok(attenuation { constant, quadratic })
}

fn make(diffuse: Option<color::T>, intensity: Option<f64>, kind: kind) -> Result<t, String> {
    let intensity = intensity.unwrap_or(1.);
    if !intensity.is_finite() || intensity < 0. {
        return Err("Light: intensity must be finite and non-negative".to_string());
    }
    Ok(t { kind, diffuse: diffuse.unwrap_or_else(color::white), intensity })
}

fn direction(value: vec3::T) -> Result<vec3::T, String> {
    if vec3::length_sq(value) == 0. {
        return Err("Light: direction must be non-zero".to_string());
    }
    Ok(vec3::normalize(value))
}

pub fn directional(diffuse: Option<color::T>, intensity: Option<f64>, value: vec3::T) -> Result<t, String> {
    make(diffuse, intensity, kind::Directional { direction: direction(value)? })
}

pub fn point(diffuse: Option<color::T>, intensity: Option<f64>, attenuation: Option<attenuation>, at: vec3::T) -> Result<t, String> {
    make(diffuse, intensity, kind::Point { position: at, attenuation: attenuation.unwrap_or(NO_ATTENUATION) })
}

/// `cutoff` is the cone half-angle in radians.
pub fn spot(
    diffuse: Option<color::T>,
    intensity: Option<f64>,
    attenuation: Option<attenuation>,
    at: vec3::T,
    value: vec3::T,
    cutoff: f64,
    concentration: f64,
) -> Result<t, String> {
    if !cutoff.is_finite() || cutoff <= 0. || cutoff > std::f64::consts::PI / 2. {
        return Err("Light.spot: cutoff must be in (0, pi/2]".to_string());
    }
    if !concentration.is_finite() || concentration < 0. {
        return Err("Light.spot: concentration must be finite and non-negative".to_string());
    }
    make(
        diffuse,
        intensity,
        kind::Spot {
            position: at,
            direction: direction(value)?,
            cutoff,
            concentration,
            attenuation: attenuation.unwrap_or(NO_ATTENUATION),
        },
    )
}

/// `samples` is 1, 4, 9, or 16 deterministic surface samples (default 9).
pub fn area(
    diffuse: Option<color::T>,
    intensity: Option<f64>,
    attenuation: Option<attenuation>,
    samples: Option<i64>,
    at: vec3::T,
    value: vec3::T,
    width: f64,
    height: f64,
) -> Result<t, String> {
    let samples = samples.unwrap_or(9);
    if !(width.is_finite() && height.is_finite()) || width <= 0. || height <= 0. {
        return Err("Light.area: dimensions must be finite and positive".to_string());
    }
    if ![1, 4, 9, 16].contains(&samples) {
        return Err("Light.area: samples must be 1, 4, 9, or 16".to_string());
    }
    make(
        diffuse,
        intensity,
        kind::Area {
            position: at,
            direction: direction(value)?,
            width,
            height,
            samples,
            attenuation: attenuation.unwrap_or(NO_ATTENUATION),
        },
    )
}
