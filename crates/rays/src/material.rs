//! Surface properties for 3D lighting.
use rays_math::color;

#[derive(Clone, Debug, PartialEq)]
pub struct t {
    pub diffuse: color::T,
    pub ambient: color::T,
    pub specular: color::T,
    pub emissive: color::T,
    pub shininess: f64,
    /// Round-corners radius in scene units for the path tracer (0 = off): edges shade as a bevel
    /// of this radius without changing geometry. Raster ignores it.
    pub round: f64,
}

pub fn create(
    diffuse: Option<color::T>,
    ambient: Option<color::T>,
    specular: Option<color::T>,
    emissive: Option<color::T>,
    shininess: Option<f64>,
    round: Option<f64>,
) -> Result<t, String> {
    let shininess = shininess.unwrap_or(32.);
    let round = round.unwrap_or(0.);
    if !shininess.is_finite() || shininess < 0. {
        return Err("Material.create: shininess must be finite and non-negative".to_string());
    }
    if !round.is_finite() || round < 0. {
        return Err("Material.create: round must be finite and non-negative".to_string());
    }
    Ok(t {
        diffuse: diffuse.unwrap_or_else(color::white),
        ambient: ambient.unwrap_or_else(color::white),
        specular: specular.unwrap_or_else(color::white),
        emissive: emissive.unwrap_or_else(color::black),
        shininess,
        round,
    })
}

pub fn default() -> t {
    create(None, None, None, None, None, None).expect("default material")
}

pub fn matte(diffuse: color::T) -> t {
    create(Some(diffuse), Some(diffuse), Some(color::black()), None, Some(0.), None).expect("matte material")
}

pub fn unlit(emissive: color::T) -> t {
    let diffuse = color::with_alpha(color::black(), emissive.a);
    create(Some(diffuse), Some(color::black()), Some(color::black()), Some(emissive), Some(0.), None).expect("unlit material")
}
