use rays_diagnostic::{self, Diagnostic};
use std::collections::BTreeMap;
use std::sync::{Arc, LazyLock, RwLock};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Hit {
    pub t: f64,
    pub prim: i64,
    pub u: f64,
    pub v: f64,
    pub nx: f64,
    pub ny: f64,
    pub nz: f64,
    pub hit: bool,
    pub instance: i64,
}

pub const MISS: Hit = Hit { t: 0.0, prim: -1, u: 0.0, v: 0.0, nx: 0.0, ny: 0.0, nz: 0.0, hit: false, instance: -1 };

pub type Scene = Arc<dyn Fn(&[f64]) -> Hit + Send + Sync + 'static>;
static SCENES: LazyLock<RwLock<Arc<BTreeMap<i64, Scene>>>> = LazyLock::new(|| RwLock::new(Arc::new(BTreeMap::new())));

pub fn register(id: i64, scene: Scene) {
    let mut guard = SCENES.write().unwrap_or_else(std::sync::PoisonError::into_inner);
    let mut next = (**guard).clone();
    next.insert(id, scene);
    *guard = Arc::new(next);
}

pub fn unregister(id: i64) {
    let mut guard = SCENES.write().unwrap_or_else(std::sync::PoisonError::into_inner);
    let mut next = (**guard).clone();
    next.remove(&id);
    *guard = Arc::new(next);
}

pub fn query(id: i64, args: &[f64]) -> Result<Hit, Diagnostic> {
    let scene = {
        let guard = SCENES.read().unwrap_or_else(std::sync::PoisonError::into_inner);
        guard.get(&id).cloned()
    };
    match scene {
        Some(scene) => Ok(scene(args)),
        None => Err(rays_diagnostic::error(
            "E_TRACE".into(),
            format!("scene/trace: no scene is registered with id {}.", id),
            None, None, None, Vec::new(), Vec::new(),
        )),
    }
}

/// Inverts a row-major 3x4 affine transform. Input indexing follows the source contract.
pub fn world_to_object(m: &[f64]) -> Vec<f64> {
    let (a,b,c,d,e,f,g,h,i) = (m[0],m[1],m[2],m[4],m[5],m[6],m[8],m[9],m[10]);
    let det = a*(e*i-f*h) - b*(d*i-f*g) + c*(d*h-e*g);
    let r = [
        e*i-f*h, c*h-b*i, b*f-c*e,
        f*g-d*i, a*i-c*g, c*d-a*f,
        d*h-e*g, b*g-a*h, a*e-b*d,
    ].map(|x| x / det);
    let t = |k: usize| -(r[3*k]*m[3] + r[3*k+1]*m[7] + r[3*k+2]*m[11]);
    vec![r[0],r[1],r[2],t(0),r[3],r[4],r[5],t(1),r[6],r[7],r[8],t(2)]
}

pub fn shutter(time: f64) -> f64 { time.max(0.0).min(1.0) }

/// Componentwise keyframe interpolation; mismatched shapes return `E_SHAPE`.
pub fn lerp(a: &[f64], b: &[f64], time: f64) -> Result<Vec<f64>, Diagnostic> {
    if a.len() != b.len() {
        return Err(rays_diagnostic::error(
            "E_SHAPE".into(), "Cannot interpolate arrays with different lengths.".into(),
            None, None, None, Vec::new(), Vec::new(),
        ));
    }
    Ok(a.iter().zip(b).map(|(x,y)| x + (y-x)*time).collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn registry_reports_typed_miss_and_runs_registered_scene() {
        let id = i64::MIN + 49123;
        unregister(id);
        assert_eq!(query(id, &[]).unwrap_err().code, "E_TRACE");
        register(id, Arc::new(|_| Hit { hit: true, ..MISS }));
        assert!(query(id, &[0.0]).unwrap().hit);
        unregister(id);
        assert_eq!(query(id, &[]).unwrap_err().code, "E_TRACE");
    }

    #[test]
    fn inverse_transform_and_shutter_interpolation() {
        assert_eq!(world_to_object(&[2.0,0.0,0.0,4.0, 0.0,2.0,0.0,6.0, 0.0,0.0,2.0,8.0]), vec![0.5,0.0,0.0,-2.0,0.0,0.5,0.0,-3.0,0.0,0.0,0.5,-4.0]);
        assert_eq!(shutter(-1.0), 0.0);
        assert_eq!(shutter(2.0), 1.0);
        assert_eq!(lerp(&[0.0,2.0], &[2.0,4.0], 0.5), Ok(vec![1.0,3.0]));
    }
    #[test]
    fn lerp_rejects_unequal_lengths() {
        let error = lerp(&[1.0], &[1.0, 2.0], 0.5).unwrap_err();
        assert_eq!(error.code, "E_SHAPE");
        assert_eq!(error.message, "Cannot interpolate arrays with different lengths.");
    }

}
