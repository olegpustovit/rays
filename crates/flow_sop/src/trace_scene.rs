//! Port of lib/flow_sop/trace_scene.ml: `scene/trace` on the CPU. Registers a geometry (with
//! optional procedural spheres, strands, vertex motion) or instances of prototypes with the
//! `Ray_host` registry that the Flow evaluator and Packed kernels query.
use flow_check::ray_host::{self as host, Hit};
use rdk_core::packed::Float3;
use rdk_geom::surface_index::{self as surface, RayDirectionMode, RaySurfaceHit, View as SurfaceView};
use rdk_core::{error as rdk_error, geometry};
use std::sync::Arc;

/// A strand segment (ax, ay, az, bx, by, bz, radius): a linear round curve with sphere caps. A hit
/// reports `prim` = its index, `u` = the curve parameter in [0, 1], `v` = 0, `instance` = -2, and
/// pathtrace.metal's normal: the offset from the axis point at `u`, less its component along the
/// segment.
pub type Strand = (f64, f64, f64, f64, f64, f64, f64);

/// A procedural sphere (centre x, y, z, radius).
pub type Sphere = (f64, f64, f64, f64);

type Trace = Arc<dyn Fn(&[f64]) -> Hit + Send + Sync>;

fn sphere_prim(k: usize) -> i64 {
    -2 - k as i64
}

fn miss() -> Hit {
    host::MISS
}

fn planes(positions: &Float3::T) -> Result<(Arc<[f64]>, Arc<[f64]>, Arc<[f64]>), String> {
    let view = Float3::View::view(positions).map_err(|e| rdk_error::to_string(&e))?;
    Ok((view.x, view.y, view.z))
}

/// The nearest triangle of one prototype, in its own space.
fn static_prototype(geometry_value: &geometry::T) -> Result<Trace, String> {
    let index = surface::create(None, None, None, None, None, geometry_value).map_err(|e| rdk_error::to_string(&e))?;
    let (px, py, pz) = planes(geometry::positions(geometry_value))?;
    Ok(Arc::new(move |q: &[f64]| {
        let directions = SurfaceView::RayDirections::ConstantDirection { x: q[3], y: q[4], z: q[5] };
        let origin = Float3::View::of_owned_exn(vec![q[0]], vec![q[1]], vec![q[2]]).expect("one origin");
        let mut primitives = [-1_i64];
        let mut triangles = [-1_i64];
        let (mut a, mut b, mut c) = ([0.0_f64], [0.0_f64], [0.0_f64]);
        let mut distances = [f64::INFINITY];
        SurfaceView::raycast_many_into(
            None,
            None,
            1,
            &index,
            &origin,
            &directions,
            q[6],
            q[7],
            0.0,
            RayDirectionMode::RayForward,
            RaySurfaceHit::RayFirstSurface,
            &mut primitives,
            &mut triangles,
            &mut a,
            &mut b,
            &mut c,
            &mut distances,
        )
        .expect("a built index casts rays");
        if primitives[0] < 0 {
            return miss();
        }
        let point = |corner: i64| {
            let k = SurfaceView::triangle_point(&index, triangles[0], corner) as usize;
            (px[k], py[k], pz[k])
        };
        let (x0, y0, z0) = point(0);
        let (x1, y1, z1) = point(1);
        let (x2, y2, z2) = point(2);
        let (ux, uy, uz) = (x1 - x0, y1 - y0, z1 - z0);
        let (vx, vy, vz) = (x2 - x0, y2 - y0, z2 - z0);
        let nx = (uy * vz) - (uz * vy);
        let ny = (uz * vx) - (ux * vz);
        let nz = (ux * vy) - (uy * vx);
        let n = ((nx * nx) + (ny * ny) + (nz * nz)).sqrt();
        Hit {
            t: distances[0],
            prim: primitives[0],
            u: b[0],
            v: c[0],
            nx: nx / n,
            ny: ny / n,
            nz: nz / n,
            hit: true,
            instance: 0,
        }
    }))
}

/// Vertex motion: the triangles of `geometry_value` with each point moved toward `ending`'s at the
/// query's time, intersected directly (Moller-Trumbore) since the interpolated surface changes per
/// ray.
fn moving_prototype(geometry_value: &geometry::T, ending: &geometry::T) -> Result<Trace, String> {
    let start = geometry::positions(geometry_value);
    let end = geometry::positions(ending);
    if Float3::length(start) != Float3::length(end) {
        return Err("scene/trace: a moving geometry's end keyframe must have its start's points".to_owned());
    }
    let index = surface::create(None, None, None, None, None, geometry_value).map_err(|e| rdk_error::to_string(&e))?;
    let (sx, sy, sz) = planes(start)?;
    let (ex, ey, ez) = planes(end)?;
    Ok(Arc::new(move |q: &[f64]| {
        let time = host::shutter(q[8]);
        let norm = ((q[3] * q[3]) + (q[4] * q[4]) + (q[5] * q[5])).sqrt();
        let (dx, dy, dz) = (q[3] / norm, q[4] / norm, q[5] / norm);
        let mut nearest = miss();
        for triangle in 0..surface::triangle_count(&index) {
            let point = |corner: i64| {
                let k = SurfaceView::triangle_point(&index, triangle, corner) as usize;
                (
                    sx[k] + ((ex[k] - sx[k]) * time),
                    sy[k] + ((ey[k] - sy[k]) * time),
                    sz[k] + ((ez[k] - sz[k]) * time),
                )
            };
            let (x0, y0, z0) = point(0);
            let (x1, y1, z1) = point(1);
            let (x2, y2, z2) = point(2);
            let (ux, uy, uz) = (x1 - x0, y1 - y0, z1 - z0);
            let (vx, vy, vz) = (x2 - x0, y2 - y0, z2 - z0);
            let px = (dy * vz) - (dz * vy);
            let py = (dz * vx) - (dx * vz);
            let pz = (dx * vy) - (dy * vx);
            let det = (ux * px) + (uy * py) + (uz * pz);
            if det != 0.0 {
                let (sx0, sy0, sz0) = (q[0] - x0, q[1] - y0, q[2] - z0);
                let b = ((sx0 * px) + (sy0 * py) + (sz0 * pz)) / det;
                let qx = (sy0 * uz) - (sz0 * uy);
                let qy = (sz0 * ux) - (sx0 * uz);
                let qz = (sx0 * uy) - (sy0 * ux);
                let c = ((dx * qx) + (dy * qy) + (dz * qz)) / det;
                let t = ((vx * qx) + (vy * qy) + (vz * qz)) / det;
                if b >= 0.0 && c >= 0.0 && b + c <= 1.0 && t >= q[6] && t <= q[7] && (!nearest.hit || t < nearest.t) {
                    let nx = (uy * vz) - (uz * vy);
                    let ny = (uz * vx) - (ux * vz);
                    let nz = (ux * vy) - (uy * vx);
                    let n = ((nx * nx) + (ny * ny) + (nz * nz)).sqrt();
                    nearest = Hit {
                        t,
                        prim: SurfaceView::triangle_primitive(&index, triangle),
                        u: b,
                        v: c,
                        nx: nx / n,
                        ny: ny / n,
                        nz: nz / n,
                        hit: true,
                        instance: 0,
                    };
                }
            }
        }
        nearest
    }))
}

fn prototype(ending: Option<&geometry::T>, geometry_value: &geometry::T) -> Result<Trace, String> {
    match ending {
        None => static_prototype(geometry_value),
        Some(ending) => moving_prototype(geometry_value, ending),
    }
}

/// The ray in an instance's space; the hit back in world space. `w` is world_to_object.
fn instance_hit(trace: &Trace, w: &[f64], k: i64, q: &[f64]) -> Hit {
    let (dx, dy, dz) = (q[3], q[4], q[5]);
    let norm = ((dx * dx) + (dy * dy) + (dz * dz)).sqrt();
    let transform = |x: f64, y: f64, z: f64, translate: bool| -> [f64; 3] {
        [
            (w[0] * x) + (w[1] * y) + (w[2] * z) + if translate { w[3] } else { 0.0 },
            (w[4] * x) + (w[5] * y) + (w[6] * z) + if translate { w[7] } else { 0.0 },
            (w[8] * x) + (w[9] * y) + (w[10] * z) + if translate { w[11] } else { 0.0 },
        ]
    };
    let o = transform(q[0], q[1], q[2], true);
    let d = transform(dx / norm, dy / norm, dz / norm, false);
    let scale = ((d[0] * d[0]) + (d[1] * d[1]) + (d[2] * d[2])).sqrt();
    let h = trace(&[o[0], o[1], o[2], d[0], d[1], d[2], q[6] * scale, q[7] * scale]);
    if !h.hit {
        return h;
    }
    let n = [
        (w[0] * h.nx) + (w[4] * h.ny) + (w[8] * h.nz),
        (w[1] * h.nx) + (w[5] * h.ny) + (w[9] * h.nz),
        (w[2] * h.nx) + (w[6] * h.ny) + (w[10] * h.nz),
    ];
    let length = ((n[0] * n[0]) + (n[1] * n[1]) + (n[2] * n[2])).sqrt();
    Hit { t: h.t / scale, nx: n[0] / length, ny: n[1] / length, nz: n[2] / length, instance: k, ..h }
}

fn with_spheres(spheres: &[Sphere], q: &[f64], nearest: Hit) -> Hit {
    let mut nearest = nearest;
    let norm = ((q[3] * q[3]) + (q[4] * q[4]) + (q[5] * q[5])).sqrt();
    let (dx, dy, dz) = (q[3] / norm, q[4] / norm, q[5] / norm);
    for (k, (cx, cy, cz, r)) in spheres.iter().enumerate() {
        let (ox, oy, oz) = (q[0] - cx, q[1] - cy, q[2] - cz);
        let b = (ox * dx) + (oy * dy) + (oz * dz);
        let disc = (b * b) - ((ox * ox) + (oy * oy) + (oz * oz) - (r * r));
        if disc >= 0.0 {
            let root = disc.sqrt();
            let t = if -b - root >= q[6] { -b - root } else { -b + root };
            if t >= q[6] && t <= q[7] && (!nearest.hit || t < nearest.t) {
                nearest = Hit {
                    t,
                    prim: sphere_prim(k),
                    u: 0.0,
                    v: 0.0,
                    nx: (ox + (t * dx)) / r,
                    ny: (oy + (t * dy)) / r,
                    nz: (oz + (t * dz)) / r,
                    hit: true,
                    instance: -1,
                };
            }
        }
    }
    nearest
}

fn dot(a: &[f64; 3], b: &[f64; 3]) -> f64 {
    (a[0] * b[0]) + (a[1] * b[1]) + (a[2] * b[2])
}

fn sub(a: &[f64; 3], b: &[f64; 3]) -> [f64; 3] {
    [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
}

/// Strand segments (a, b, radius): linear round curves, each a capsule (the cylinder within the
/// segment, the end spheres beyond it); a hit reports prim = segment, u = the curve parameter,
/// instance = -2, and pathtrace.metal's normal.
fn with_strands(strands: &[Strand], q: &[f64], nearest: Hit) -> Hit {
    if strands.is_empty() {
        return nearest;
    }
    let mut nearest = nearest;
    let norm = ((q[3] * q[3]) + (q[4] * q[4]) + (q[5] * q[5])).sqrt();
    let d = [q[3] / norm, q[4] / norm, q[5] / norm];
    let o = [q[0], q[1], q[2]];
    for (k, (ax, ay, az, bx, by, bz, r)) in strands.iter().enumerate() {
        let (a, b) = ([*ax, *ay, *az], [*bx, *by, *bz]);
        let ba = sub(&b, &a);
        let oa = sub(&o, &a);
        let (baba, bard, baoa) = (dot(&ba, &ba), dot(&ba, &d), dot(&ba, &oa));
        let limit = if nearest.hit { nearest.t } else { q[7] };
        let mut best = f64::INFINITY;
        let mut consider = |valid: &dyn Fn(f64) -> bool, t: f64| {
            let y = baoa + (t * bard);
            if valid(y) && t >= q[6] && t <= limit && t < best {
                best = t;
            }
        };
        let mut roots = |disc: f64, minus_b: f64, scale: f64, valid: &dyn Fn(f64) -> bool| {
            if disc >= 0.0 {
                for s in [-1.0_f64, 1.0] {
                    consider(valid, (minus_b + (s * disc.sqrt())) / scale);
                }
            }
        };
        let big_a = baba - (bard * bard);
        let big_b = (baba * dot(&d, &oa)) - (baoa * bard);
        let big_c = (baba * dot(&oa, &oa)) - (baoa * baoa) - (r * r * baba);
        if big_a > 0.0 {
            roots((big_b * big_b) - (big_a * big_c), -big_b, big_a, &|y| y > 0.0 && y < baba);
        }
        let caps: [([f64; 3], bool); 2] = [(a, false), (b, true)];
        for (centre, far) in caps {
            let oc = sub(&o, &centre);
            let half = dot(&oc, &d);
            let valid = |y: f64| if far { y >= baba } else { y <= 0.0 };
            roots((half * half) - (dot(&oc, &oc) - (r * r)), -half, 1.0, &valid);
        }
        let t = best;
        if t <= limit {
            let u = 1.0_f64.min(0.0_f64.max((baoa + (t * bard)) / baba));
            let mut off = [0.0; 3];
            for i in 0..3 {
                off[i] = o[i] + (t * d[i]) - (a[i] + (u * ba[i]));
            }
            let along = dot(&off, &ba) / baba.max(1e-8);
            for i in 0..3 {
                off[i] -= ba[i] * along;
            }
            let n = dot(&off, &off).sqrt();
            nearest = Hit {
                t,
                prim: k as i64,
                u,
                v: 0.0,
                nx: off[0] / n,
                ny: off[1] / n,
                nz: off[2] / n,
                hit: true,
                instance: -2,
            };
        }
    }
    nearest
}

/// `spheres` are procedural (centre, radius) primitives tested analytically beside the triangles;
/// the nearer hit wins. `normal` of a sphere hit is the outward unit normal; `u` and `v` are 0.
/// `ending` is vertex motion: the same points at the shutter's end, each interpolated at the
/// query's time (clamped to [0, 1]).
pub fn register(
    spheres: &[Sphere],
    strands: &[Strand],
    ending: Option<&geometry::T>,
    id: i64,
    geometry_value: &geometry::T,
) -> Result<(), String> {
    let trace = prototype(ending, geometry_value)?;
    let (spheres, strands) = (spheres.to_vec(), strands.to_vec());
    host::register(id, Arc::new(move |q: &[f64]| with_spheres(&spheres, q, with_strands(&strands, q, trace(q)))));
    Ok(())
}

/// Instances of prototypes with row-major 3x4 object-to-world transforms. `instance` is the array
/// index, `prim` the primitive within its prototype, `normal` the world-space unit normal, `t` the
/// world distance. `motion` is instance motion: each instance's shutter-end transform,
/// interpolated with its start at the query's time.
///
/// Unlike OCaml, a transform that is not 12 numbers is refused here, at registration, rather than
/// failing on a query.
pub fn register_instances(
    spheres: &[Sphere],
    strands: &[Strand],
    motion: Option<&[Vec<f64>]>,
    id: i64,
    instances: &[(geometry::T, Vec<f64>)],
) -> Result<(), String> {
    let mut traces: Vec<(Trace, Vec<f64>, Vec<f64>)> = Vec::with_capacity(instances.len());
    for (geometry_value, transform) in instances {
        if transform.len() != 12 {
            return Err("scene/trace: an instance transform needs 12 numbers".to_owned());
        }
        let trace = prototype(None, geometry_value)?;
        traces.push((trace, transform.clone(), host::world_to_object(transform)));
    }
    if let Some(ends) = motion {
        if ends.len() != traces.len() {
            return Err("scene/trace: motion needs one end transform per instance".to_owned());
        }
        if ends.iter().any(|end| end.len() != 12) {
            return Err("scene/trace: an instance transform needs 12 numbers".to_owned());
        }
    }
    let motion: Option<Vec<Vec<f64>>> = motion.map(|ends| ends.to_vec());
    let (spheres, strands) = (spheres.to_vec(), strands.to_vec());
    host::register(
        id,
        Arc::new(move |q: &[f64]| {
            let mut nearest = miss();
            for (k, (trace, transform, w)) in traces.iter().enumerate() {
                // instance motion: the object-to-world transform interpolated at the query's time
                let moved;
                let w: &[f64] = match &motion {
                    None => w,
                    Some(ends) => {
                        let lerped = host::lerp(transform, &ends[k], host::shutter(q[8])).expect("checked lengths");
                        moved = host::world_to_object(&lerped);
                        &moved
                    }
                };
                let h = instance_hit(trace, w, k as i64, q);
                if h.hit && (!nearest.hit || h.t < nearest.t) {
                    nearest = h;
                }
            }
            with_spheres(&spheres, q, with_strands(&strands, q, nearest))
        }),
    );
    Ok(())
}

pub fn unregister(id: i64) {
    host::unregister(id);
}

#[cfg(test)]
mod tests {
    //! lib/flow_sop/test_trace.ml, host side: the registered scene answers a lattice of rays at a
    //! quad like a direct `Surface_index.raycast` (t and prim), vertex motion with an unmoved end
    //! keyframe agrees with the static path, and spheres, strands and instances report their
    //! documented prim, instance and normal. The kernel-level checks of that file (a `.rays` program
    //! through `Flow_ir.Packed` at 1 and 8 domains, one host query per ray, the flow_web refusal)
    //! need `Test_program` and the flow_web crate.
    use super::*;
    use rdk_core::topology;
    use rays_math::vec3;

    const ID: i64 = 7;
    const COUNT: usize = 256;

    fn quad() -> geometry::T {
        let positions = Float3::of_owned(vec![-1.0, 1.0, 1.0, -1.0], vec![-1.0, -1.0, 1.0, 1.0], vec![0.0; 4]).expect("positions");
        let topology = topology::polygons_owned(4, vec![0, 1, 2, 3], vec![0, 4]).expect("topology");
        geometry::create(positions, topology, None, None, None).expect("quad")
    }

    fn x(i: usize) -> f64 {
        (0.15 * (i % 16) as f64) - 1.2
    }

    fn y(i: usize) -> f64 {
        (0.15 * (i / 16) as f64) - 1.2
    }

    fn query(id: i64, args: &[f64]) -> Hit {
        host::query(id, args).expect("registered")
    }

    #[test]
    fn registered_quad_matches_a_direct_raycast() {
        let quad = quad();
        register(&[], &[], None, ID, &quad).expect("registers");
        let index = surface::create(None, None, None, None, None, &quad).expect("index");
        let mut hits = 0;
        for i in 0..COUNT {
            let q = [x(i), y(i), 5.0, 0.0, 0.0, -1.0, 0.0, 100.0, 1.0];
            let a = query(ID, &q);
            let direct = surface::raycast(&index, vec3::create(x(i), y(i), 5.0), vec3::create(0.0, 0.0, -1.0)).expect("cast");
            match direct {
                Some(hit) => {
                    hits += 1;
                    assert!(a.hit && a.t == hit.distance && a.prim == hit.primitive && a.nz * a.nz > 0.0, "ray {i}");
                }
                None => assert!(!a.hit && a.prim == -1, "ray {i}"),
            }
        }
        assert!(hits > 0 && hits < COUNT);
        // vertex motion with an unmoved end keyframe intersects directly: the same prim, t and uv
        register(&[], &[], Some(&quad), 9, &quad).expect("moving registers");
        for i in 0..COUNT {
            let q = [x(i), y(i), 5.0, 0.0, 0.0, -1.0, 0.0, 100.0, 0.3];
            let (a, b) = (query(ID, &q), query(9, &q));
            assert!(a.hit == b.hit && a.prim == b.prim && (a.t - b.t).abs() < 1e-12 && (a.u - b.u).abs() < 1e-12);
            assert!((a.v - b.v).abs() < 1e-12 && a.nz == b.nz, "ray {i}");
        }
        unregister(9);
        unregister(ID);
        assert!(host::query(ID, &[0.0; 9]).is_err());
    }

    #[test]
    fn a_moving_geometry_needs_matching_points() {
        let two = geometry::create(
            Float3::of_owned(vec![0.0, 1.0], vec![0.0, 0.0], vec![0.0, 0.0]).expect("positions"),
            topology::polygons_owned(2, Vec::new(), vec![0]).expect("empty topology"),
            None,
            None,
            None,
        )
        .expect("points");
        assert!(register(&[], &[], Some(&two), 41, &quad()).is_err());
    }

    #[test]
    fn spheres_and_strands_report_their_hits() {
        let quad = quad();
        register(&[(0.0, 0.0, 2.0, 0.5)], &[(2.0, -1.0, 0.0, 2.0, 1.0, 0.0, 0.25)], None, 42, &quad).expect("registers");
        // down the z axis the sphere (z in [1.5, 2.5]) is nearer than the quad
        let sphere = query(42, &[0.0, 0.0, 5.0, 0.0, 0.0, -1.0, 0.0, 100.0, 0.0]);
        assert!(sphere.hit && sphere.prim == -2 && sphere.instance == -1);
        assert!((sphere.t - 2.5).abs() < 1e-12 && sphere.nz == 1.0 && sphere.u == 0.0);
        // a ray along -x through the strand's axis
        let strand = query(42, &[5.0, 0.0, 0.0, -1.0, 0.0, 0.0, 0.0, 100.0, 0.0]);
        assert!(strand.hit && strand.prim == 0 && strand.instance == -2);
        assert!((strand.t - 2.75).abs() < 1e-12 && (strand.u - 0.5).abs() < 1e-12 && strand.nx == 1.0);
        let none = query(42, &[5.0, 5.0, 5.0, 1.0, 0.0, 0.0, 0.0, 100.0, 0.0]);
        assert!(!none.hit);
        unregister(42);
    }

    #[test]
    fn instances_report_index_prototype_prim_and_world_distance() {
        let quad = quad();
        // object-to-world: translate by x + 10, and a second copy lifted to z = -3
        let at = |tx: f64, tz: f64| vec![1.0, 0.0, 0.0, tx, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, tz];
        register_instances(&[], &[], None, 43, &[(quad.clone(), at(10.0, 0.0)), (quad.clone(), at(0.0, -3.0))]).expect("registers");
        let first = query(43, &[10.0, 0.0, 5.0, 0.0, 0.0, -1.0, 0.0, 100.0, 0.0]);
        assert!(first.hit && first.instance == 0 && (first.t - 5.0).abs() < 1e-12 && first.nz.abs() == 1.0);
        let second = query(43, &[0.0, 0.0, 5.0, 0.0, 0.0, -1.0, 0.0, 100.0, 0.0]);
        assert!(second.hit && second.instance == 1 && (second.t - 8.0).abs() < 1e-12);
        let miss = query(43, &[50.0, 0.0, 5.0, 0.0, 0.0, -1.0, 0.0, 100.0, 0.0]);
        assert!(!miss.hit);
        // instance motion: the second copy slides to x = 10 at the shutter's end
        register_instances(&[], &[], Some(&[at(10.0, 0.0), at(10.0, -3.0)]), 44, &[(quad.clone(), at(10.0, 0.0)), (quad.clone(), at(0.0, -3.0))])
            .expect("moving registers");
        let early = query(44, &[0.0, 0.0, 5.0, 0.0, 0.0, -1.0, 0.0, 100.0, 0.0]);
        let late = query(44, &[0.0, 0.0, 5.0, 0.0, 0.0, -1.0, 0.0, 100.0, 1.0]);
        assert!(early.hit && early.instance == 1 && !late.hit);
        assert!(register_instances(&[], &[], Some(&[at(1.0, 0.0)]), 45, &[(quad.clone(), at(0.0, 0.0)), (quad, at(0.0, 1.0))]).is_err());
        unregister(43);
        unregister(44);
    }
}
