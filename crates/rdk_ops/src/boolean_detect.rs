//! Detect source/collision polygon intersections without changing source topology or payload.
//!
//! Both inputs are deterministically triangulated only inside packed surface indexes. Broad phase,
//! narrow testing, row sorting, and packed group output use deterministic disjoint parallel ranges.
use std::sync::Arc;

use rdk_core::{attribute, cancel, error, geometry, group, packed, support};
use rdk_core::error::Code;
use rdk_geom::surface_index;
use rdk_geom::triangle_intersection::{self, EVENT_STRIDE, MAX_EVENTS};

const OPERATION: &str = "boolean_detect";


pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: i64,
    pub source_primitives: Option<group::T>,
    pub collision_primitives: Option<group::T>,
    pub tolerance: f64,
    pub include_coplanar: bool,
    pub intersecting_group: Option<String>,
    pub intersections_attribute: Option<String>,
    pub count_attribute: Option<String>,
    pub self_intersecting_group: Option<String>,
    pub self_intersections_attribute: Option<String>,
    pub self_count_attribute: Option<String>,
}

impl Default for Params {
    fn default() -> Params {
        Params {
            cancel: None,
            grain: 16_384,
            source_primitives: None,
            collision_primitives: None,
            tolerance: 0.0,
            include_coplanar: true,
            intersecting_group: Some("boolean_intersections".to_owned()),
            intersections_attribute: None,
            count_attribute: None,
            self_intersecting_group: None,
            self_intersections_attribute: None,
            self_count_attribute: None,
        }
    }
}

fn make(code: Code, message: String) -> error::T {
    error::new(OPERATION, code, message)
}

fn cancelled() -> error::T {
    error::cancelled(OPERATION)
}

struct Outputs {
    unique_counts: Vec<i64>,
    offsets: Vec<i64>,
    values: Vec<i64>,
}

/// Sorted unique rows (ranges of primitives run in parallel, joined in range order).
fn unique_rows(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    primitive_count: i64,
    raw_counts: &[i64],
    raw_values: &[i64],
) -> Result<Outputs, error::T> {
    let mut raw_offsets = vec![0_i64; primitive_count as usize + 1];
    for primitive in 0..primitive_count as usize {
        cancel::poll(cancel_value, primitive as i64, 4095)?;
        raw_offsets[primitive + 1] = raw_offsets[primitive] + raw_counts[primitive];
    }
    // Rows are sorted/deduped in place inside a copy of the raw values (range windows follow the raw
    // offsets, fixed by `grain`); a sequential memmove then closes the gaps dedup left between ranges.
    let step = grain.max(1);
    let ranges = support::range_count(primitive_count, step);
    let bound = |range: i64| raw_offsets[range.saturating_mul(step).min(primitive_count) as usize] as usize;
    let mut values = raw_values.to_vec();
    let mut unique_counts = vec![0_i64; primitive_count as usize];
    let (w_values, w_counts) =
        (support::Windows::new(&mut values, ranges, bound), support::Windows::even(&mut unique_counts, step));
    support::run_indexed_ranges(grain, primitive_count, &|range, first, last| {
        let (window, counts) = (w_values.take(range), w_counts.take(range));
        let base = raw_offsets[first as usize];
        let mut used = 0;
        for primitive in first..last {
            cancel::poll(cancel_value, primitive, 4095)?;
            let start = (raw_offsets[primitive as usize] - base) as usize;
            let end = (raw_offsets[primitive as usize + 1] - base) as usize;
            window[start..end].sort_unstable();
            let row_start = used;
            for index in start..end {
                if used == row_start || window[index] != window[used - 1] {
                    window[used] = window[index];
                    used += 1;
                }
            }
            counts[(primitive - first) as usize] = (used - row_start) as i64;
        }
        Ok(())
    })?;
    let mut end = 0;
    for range in 0..ranges {
        let (first, last) = ((range * step) as usize, ((range + 1) * step).min(primitive_count) as usize);
        let used = unique_counts[first..last].iter().sum::<i64>() as usize;
        let start = bound(range);
        values.copy_within(start..start + used, end);
        end += used;
    }
    values.truncate(end);
    let mut offsets = vec![0_i64; primitive_count as usize + 1];
    for primitive in 0..primitive_count as usize {
        cancel::poll(cancel_value, primitive as i64, 4095)?;
        offsets[primitive + 1] = offsets[primitive] + unique_counts[primitive];
    }
    Ok(Outputs { unique_counts, offsets, values })
}

fn install_outputs(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    intersecting_group: Option<&str>,
    intersections_attribute: Option<&str>,
    count_attribute: Option<&str>,
    raw_counts: &[i64],
    raw_values: &[i64],
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let primitive_count = geometry::primitive_count(geometry_value);
    let outputs = unique_rows(cancel_value, grain, primitive_count, raw_counts, raw_values)?;
    let mut output = geometry_value.clone();
    // Failures building these outputs are `invalid_output`; cancellation is not.
    let wrap = |value: error::T| {
        if error::is_cancelled(&value) { value } else { make(Code::InvalidOutput, error::message(&value).to_owned()) }
    };
    if let Some(name) = intersecting_group {
        let counts = &outputs.unique_counts;
        let item = group::init(Some(grain), group::Owner::Primitive, name, primitive_count, &|primitive| {
            counts[primitive as usize] > 0
        })
        .map_err(wrap)?;
        output = geometry::with_group(item, &output).map_err(wrap)?;
    }
    if let Some(name) = intersections_attribute {
        let values = packed::int_array::create_owned(outputs.offsets.clone(), outputs.values.clone()).map_err(wrap)?;
        let item = attribute::create_owned(name, attribute::Owner::Primitive, attribute::Storage::IntArray(values))
            .map_err(wrap)?;
        output = geometry::with_attribute(item, &output).map_err(wrap)?;
    }
    if let Some(name) = count_attribute {
        let item = attribute::create_owned(
            name,
            attribute::Owner::Primitive,
            attribute::Storage::Int(Arc::new(outputs.unique_counts.clone())),
        )
        .map_err(wrap)?;
        output = geometry::with_attribute(item, &output).map_err(wrap)?;
    }
    Ok(output)
}

#[allow(clippy::too_many_arguments)]
fn detect_pairs(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    tolerance: f64,
    include_coplanar: bool,
    is_self: bool,
    source_surface: &Arc<surface_index::T>,
    source_geometry: &geometry::T,
    collision_surface: &Arc<surface_index::T>,
    collision_geometry: &geometry::T,
) -> Result<(Vec<i64>, Vec<i64>), error::T> {
    let (source_triangles, collision_triangles) = if is_self {
        surface_index::view::overlapping_self_triangle_pairs(cancel_value, grain, tolerance, source_surface)?
    } else {
        surface_index::view::overlapping_triangle_pairs(cancel_value, grain, tolerance, source_surface, collision_surface)?
    };
    let candidates = source_triangles.len() as i64;
    let source_narrow = triangle_intersection::surface(Arc::clone(source_surface), source_geometry)?;
    let collision_narrow = triangle_intersection::surface(Arc::clone(collision_surface), collision_geometry)?;
    let byte_count = (candidates + 7) / 8;
    let byte_grain = (grain / 8).max(1);
    let mut keep = vec![0_u8; byte_count as usize];
    let windows = support::Windows::even(&mut keep, byte_grain);
    support::run_indexed_ranges(byte_grain, byte_count, &|range, first_byte, last_byte| {
        let mut scratch = [0.0_f64; 26];
        let mut point_info = [0_i64; 10];
        let mut events = [0.0_f64; MAX_EVENTS * EVENT_STRIDE];
        for (byte, slot) in (first_byte..last_byte).zip(windows.take(range)) {
            let first = byte * 8;
            let last = candidates.min(first + 8);
            let mut bits = 0_u8;
            for candidate in first..last {
                cancel::poll(cancel_value, candidate, 4095)?;
                let count = triangle_intersection::events_into(
                    &mut scratch,
                    &mut point_info,
                    &mut events,
                    is_self,
                    tolerance,
                    include_coplanar,
                    &source_narrow,
                    source_triangles[candidate as usize],
                    &collision_narrow,
                    collision_triangles[candidate as usize],
                )?;
                if count > 0 {
                    bits |= 1 << (candidate - first);
                }
            }
            *slot = bits;
        }
        Ok(())
    })?;
    let primitive_count = geometry::primitive_count(source_geometry) as usize;
    let mut raw_counts = vec![0_i64; primitive_count];
    let mut exact = 0_usize;
    for candidate in 0..candidates {
        cancel::poll(cancel_value, candidate, 4095)?;
        if support::bits::mem(&keep, candidate) {
            let source_primitive =
                surface_index::view::triangle_primitive(source_surface, source_triangles[candidate as usize]);
            raw_counts[source_primitive as usize] += 1;
            if is_self {
                let collision_primitive = surface_index::view::triangle_primitive(
                    collision_surface,
                    collision_triangles[candidate as usize],
                );
                raw_counts[collision_primitive as usize] += 1;
            }
            exact += 1;
        }
    }
    let mut cursors = vec![0_i64; primitive_count + 1];
    for primitive in 0..primitive_count {
        cancel::poll(cancel_value, primitive as i64, 4095)?;
        cursors[primitive + 1] = cursors[primitive] + raw_counts[primitive];
    }
    let multiplicity = if is_self { 2 } else { 1 };
    let mut raw_values = vec![0_i64; exact * multiplicity];
    for candidate in 0..candidates {
        cancel::poll(cancel_value, candidate, 4095)?;
        if support::bits::mem(&keep, candidate) {
            let source_primitive =
                surface_index::view::triangle_primitive(source_surface, source_triangles[candidate as usize]) as usize;
            let collision_primitive = surface_index::view::triangle_primitive(
                collision_surface,
                collision_triangles[candidate as usize],
            ) as usize;
            raw_values[cursors[source_primitive] as usize] = collision_primitive as i64;
            cursors[source_primitive] += 1;
            if is_self {
                raw_values[cursors[collision_primitive] as usize] = source_primitive as i64;
                cursors[collision_primitive] += 1;
            }
        }
    }
    Ok((raw_counts, raw_values))
}

fn duplicate_name(names: &[&str]) -> bool {
    let mut names = names.to_vec();
    names.sort();
    names.windows(2).any(|pair| pair[0] == pair[1])
}

fn invalid(message: &str) -> error::T {
    make(Code::InvalidParameter, message.to_owned())
}

fn run(params: &Params, collision: &geometry::T, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_value = params.cancel.as_deref();
    let grain = params.grain;
    let tolerance = params.tolerance;
    support::check_positive(grain).map_err(|_| invalid("grain must be positive"))?;
    if !tolerance.is_finite() || tolerance < 0.0 {
        return Err(invalid("tolerance must be finite and non-negative"));
    }
    let intersecting_group = params.intersecting_group.as_deref();
    let intersections_attribute = params.intersections_attribute.as_deref();
    let count_attribute = params.count_attribute.as_deref();
    let self_group = params.self_intersecting_group.as_deref();
    let self_intersections = params.self_intersections_attribute.as_deref();
    let self_count = params.self_count_attribute.as_deref();
    if intersecting_group.is_none()
        && intersections_attribute.is_none()
        && count_attribute.is_none()
        && self_group.is_none()
        && self_intersections.is_none()
        && self_count.is_none()
    {
        return Err(invalid("at least one output must be requested"));
    }
    let names: Vec<&str> =
        [intersections_attribute, count_attribute, self_intersections, self_count].into_iter().flatten().collect();
    let groups: Vec<&str> = [intersecting_group, self_group].into_iter().flatten().collect();
    if names.iter().any(|name| support::ocaml_trim_is_empty(name))
        || groups.iter().any(|name| support::ocaml_trim_is_empty(name))
    {
        return Err(invalid("output names must not be empty"));
    }
    if duplicate_name(&names) {
        return Err(invalid("Boolean Detect attribute outputs must have distinct names"));
    }
    if duplicate_name(&groups) {
        return Err(invalid("Boolean Detect group outputs must have distinct names"));
    }
    let source_surface = Arc::new(surface_index::create(
        cancel_value,
        Some(grain),
        params.source_primitives.as_ref(),
        None,
        None,
        geometry_value,
    )?);
    let cross_requested = intersecting_group.is_some() || intersections_attribute.is_some() || count_attribute.is_some();
    let self_requested = self_group.is_some() || self_intersections.is_some() || self_count.is_some();
    let mut current = geometry_value.clone();
    if cross_requested {
        let collision_surface = Arc::new(surface_index::create(
            cancel_value,
            Some(grain),
            params.collision_primitives.as_ref(),
            None,
            None,
            collision,
        )?);
        let (raw_counts, raw_values) = detect_pairs(
            cancel_value,
            grain,
            tolerance,
            params.include_coplanar,
            false,
            &source_surface,
            geometry_value,
            &collision_surface,
            collision,
        )?;
        current = install_outputs(
            cancel_value,
            grain,
            intersecting_group,
            intersections_attribute,
            count_attribute,
            &raw_counts,
            &raw_values,
            geometry_value,
        )?;
    }
    if self_requested {
        let (raw_counts, raw_values) = detect_pairs(
            cancel_value,
            grain,
            tolerance,
            params.include_coplanar,
            true,
            &source_surface,
            geometry_value,
            &source_surface,
            geometry_value,
        )?;
        // The OCaml installs the self outputs onto the geometry returned by the cross stage.
        current = install_outputs(
            cancel_value,
            grain,
            self_group,
            self_intersections,
            self_count,
            &raw_counts,
            &raw_values,
            &current,
        )?;
    }
    Ok(current)
}

/// Detect source/collision polygon intersections without changing source topology or payload.
/// `params.intersecting_group` defaults to `boolean_intersections`; `None` omits it.
pub fn run_checked(params: &Params, collision: &geometry::T, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    run(params, collision, geometry_value).map_err(|value| {
        if error::is_cancelled(&value) { cancelled() } else { value }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rdk_core::topology;

    fn mesh(points: &[(f64, f64, f64)], triangles: &[i64]) -> geometry::T {
        let x = points.iter().map(|p| p.0).collect();
        let y = points.iter().map(|p| p.1).collect();
        let z = points.iter().map(|p| p.2).collect();
        let offsets = (0..=triangles.len() as i64 / 3).map(|p| p * 3).collect();
        let topology_value = topology::polygons_owned(points.len() as i64, triangles.to_vec(), offsets).unwrap();
        geometry::create(packed::float3::view::of_owned_exn(x, y, z).unwrap(), topology_value, None, None, None).unwrap()
    }

    fn source() -> geometry::T {
        mesh(
            &[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.), (10., 0., 0.), (12., 0., 0.), (10., 2., 0.)],
            &[0, 1, 2, 3, 4, 5],
        )
    }

    fn collision() -> geometry::T {
        mesh(
            &[(0.5, -0.5, -1.), (0.5, 1.5, 1.), (0.5, 1.5, -1.), (0.25, 0.25, 0.), (0.5, 0.25, 0.), (0.25, 0.5, 0.)],
            &[0, 1, 2, 3, 4, 5],
        )
    }

    fn int_array(name: &str, value: &geometry::T) -> (Vec<i64>, Vec<i64>) {
        let item = geometry::find_attribute(attribute::Owner::Primitive, name, value)
            .unwrap_or_else(|| panic!("missing {name}"));
        match attribute::view::storage(&item).unwrap() {
            attribute::Storage::IntArray(values) => {
                let view = packed::int_array::view::view(values);
                (view.offsets.to_vec(), view.values.to_vec())
            }
            _ => panic!("{name} has wrong storage"),
        }
    }

    fn ints(name: &str, value: &geometry::T) -> Vec<i64> {
        let item = geometry::find_attribute(attribute::Owner::Primitive, name, value)
            .unwrap_or_else(|| panic!("missing {name}"));
        match attribute::view::storage(&item).unwrap() {
            attribute::Storage::Int(values) => values.to_vec(),
            _ => panic!("{name} has wrong storage"),
        }
    }

    fn named_group(name: &str, value: &geometry::T) -> group::T {
        geometry::find_group(group::Owner::Primitive, name, value).unwrap()
    }

    fn topology_equal(left: &geometry::T, right: &geometry::T) -> bool {
        let l = topology::view::view(geometry::topology(left)).unwrap();
        let r = topology::view::view(geometry::topology(right)).unwrap();
        l.point_count == r.point_count
            && l.vertex_points == r.vertex_points
            && l.primitive_offsets == r.primitive_offsets
            && l.primitive_kinds == r.primitive_kinds
    }

    type Signature = (Vec<i64>, Vec<i64>, Vec<i64>, Vec<bool>);

    fn output_signature(value: &geometry::T) -> Signature {
        let (offsets, values) = int_array("hits", value);
        let selected = named_group("intersections", value);
        (
            offsets,
            values,
            ints("hit_count", value),
            (0..geometry::primitive_count(value)).map(|p| group::mem(p, &selected)).collect(),
        )
    }

    fn expect(code: Code, value: Result<geometry::T, error::T>) {
        match value {
            Err(failure) => assert_eq!(error::code(&failure), code, "{}", error::to_string(&failure)),
            Ok(_) => panic!("expected {code:?}"),
        }
    }

    fn no_collision() -> geometry::T {
        mesh(&[], &[])
    }

    #[test]
    fn cross_and_coplanar() {
        let temperature = attribute::create_owned(
            "temperature",
            attribute::Owner::Point,
            attribute::Storage::Float(Arc::new(vec![0., 1., 2., 3., 4., 5.])),
        ).unwrap();
        let preserved = group::init(None, group::Owner::Primitive, "preserved", 2, &|p| p == 1).unwrap();
        let stale_hits = attribute::create_owned(
            "hits",
            attribute::Owner::Primitive,
            attribute::Storage::Int(Arc::new(vec![9, 9])),
        ).unwrap();
        let stale_intersections = group::init(None, group::Owner::Primitive, "intersections", 2, &|_| true).unwrap();
        let mut source = source();
        source = geometry::with_attribute(temperature.clone(), &source).unwrap();
        source = geometry::with_attribute(stale_hits, &source).unwrap();
        source = geometry::with_group(preserved.clone(), &source).unwrap();
        source = geometry::with_group(stale_intersections, &source).unwrap();
        let collision = collision();
        let params = Params {
            grain: 1,
            intersecting_group: Some("intersections".to_owned()),
            intersections_attribute: Some("hits".to_owned()),
            count_attribute: Some("hit_count".to_owned()),
            ..Params::default()
        };
        let output = run_checked(&params, &collision, &source).unwrap();
        assert!(
            topology::data_id(geometry::topology(&output)) == topology::data_id(geometry::topology(&source))
                && packed::float3::data_id(geometry::positions(&output))
                    == packed::float3::data_id(geometry::positions(&source)),
            "copied unchanged source geometry"
        );
        let retained_temperature = geometry::find_attribute(attribute::Owner::Point, "temperature", &output).unwrap();
        let retained_group = geometry::find_group(group::Owner::Primitive, "preserved", &output).unwrap();
        assert!(
            attribute::data_id(&retained_temperature) == attribute::data_id(&temperature)
                && group::data_id(&retained_group) == group::data_id(&preserved),
            "did not preserve unrelated source metadata"
        );
        let (offsets, values) = int_array("hits", &output);
        let selected = named_group("intersections", &output);
        assert!(
            offsets == [0, 2, 2]
                && values == [0, 1]
                && ints("hit_count", &output) == [2, 0]
                && group::cardinality(&selected) == 1
                && group::mem(0, &selected),
            "cross/coplanar primitive aggregation"
        );
        let crossing_only = run_checked(
            &Params {
                include_coplanar: false,
                intersecting_group: None,
                intersections_attribute: Some("hits".to_owned()),
                ..Params::default()
            },
            &collision,
            &source,
        ).unwrap();
        let (crossing_offsets, crossing_values) = int_array("hits", &crossing_only);
        assert!(crossing_offsets == [0, 1, 1] && crossing_values == [0], "include_coplanar=false");
        assert!(topology_equal(&output, &source), "changed topology values");
    }

    fn translated(value: &geometry::T, dx: f64, dy: f64, dz: f64) -> geometry::T {
        let p = packed::float3::view::view(geometry::positions(value)).unwrap();
        let moved = packed::float3::view::of_owned_exn(
            p.x.iter().map(|v| v + dx).collect(),
            p.y.iter().map(|v| v + dy).collect(),
            p.z.iter().map(|v| v + dz).collect(),
        )
        .unwrap();
        geometry::create(moved, geometry::topology(value).clone(), None, None, None).unwrap()
    }

    #[test]
    fn tolerance_and_translation() {
        let single_source = mesh(&[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.)], &[0, 1, 2]);
        let separated = mesh(&[(2.01, 0., 0.), (2.51, 0., 0.), (2.01, 0.5, 0.)], &[0, 1, 2]);
        let hits = |tolerance: f64| {
            int_array(
                "hits",
                &run_checked(
                    &Params { tolerance, intersections_attribute: Some("hits".to_owned()), ..Params::default() },
                    &separated,
                    &single_source,
                ).unwrap(),
            )
            .1
        };
        assert!(hits(0.0).is_empty() && hits(0.02) == [0], "world-space tolerance");
        let params = Params {
            intersecting_group: Some("intersections".to_owned()),
            intersections_attribute: Some("hits".to_owned()),
            count_attribute: Some("hit_count".to_owned()),
            ..Params::default()
        };
        let origin_output = run_checked(&params, &collision(), &source()).unwrap();
        let moved_output = run_checked(
            &params,
            &translated(&collision(), 1e12, -1e12, 1e12),
            &translated(&source(), 1e12, -1e12, 1e12),
        ).unwrap();
        assert!(
            output_signature(&origin_output) == output_signature(&moved_output),
            "changed under a large common translation"
        );
    }

    fn self_detect(value: &geometry::T) -> geometry::T {
        run_checked(
            &Params {
                intersecting_group: None,
                self_intersecting_group: Some("self_intersections".to_owned()),
                self_intersections_attribute: Some("self_hits".to_owned()),
                self_count_attribute: Some("self_hit_count".to_owned()),
                ..Params::default()
            },
            &no_collision(),
            value,
        ).unwrap()
    }

    #[test]
    fn self_intersections() {
        let crossing_source = mesh(
            &[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.), (0.5, -0.5, -1.), (0.5, 1.5, 1.), (0.5, 1.5, -1.)],
            &[0, 1, 2, 3, 4, 5],
        );
        let crossing = self_detect(&crossing_source);
        let (offsets, values) = int_array("self_hits", &crossing);
        assert!(
            offsets == [0, 1, 2]
                && values == [1, 0]
                && ints("self_hit_count", &crossing) == [1, 1]
                && group::cardinality(&named_group("self_intersections", &crossing)) == 2,
            "AxA symmetric crossing output"
        );
        let both = run_checked(
            &Params {
                intersecting_group: Some("cross".to_owned()),
                intersections_attribute: Some("cross_hits".to_owned()),
                self_intersecting_group: Some("self".to_owned()),
                self_intersections_attribute: Some("self_hits".to_owned()),
                ..Params::default()
            },
            &collision(),
            &crossing_source,
        ).unwrap();
        assert!(
            geometry::find_group(group::Owner::Primitive, "cross", &both).is_some()
                && geometry::find_group(group::Owner::Primitive, "self", &both).is_some()
                && !int_array("cross_hits", &both).1.is_empty()
                && int_array("self_hits", &both).1 == [1, 0],
            "combined AxA/AxB atomic outputs"
        );
        let square = [(0., 0., 0.), (1., 0., 0.), (1., 1., 0.), (0., 1., 0.)];
        let adjacent = self_detect(&mesh(&square, &[0, 1, 2, 0, 2, 3]));
        assert!(int_array("self_hits", &adjacent).1.is_empty(), "ordinary shared edge");
        let vertex_touch = self_detect(&mesh(
            &[(0., 0., 0.), (1., 0., 0.), (0., 1., 0.), (0., 0., 1.), (0., 1., 1.)],
            &[0, 1, 2, 0, 3, 4],
        ));
        assert!(int_array("self_hits", &vertex_touch).1.is_empty(), "ordinary shared vertex");
        let folded = self_detect(&mesh(
            &[(0., 0., 0.), (2., 0., 0.), (0., 2., 0.), (0.5, 0.5, 0.)],
            &[0, 1, 2, 0, 1, 3],
        ));
        assert!(int_array("self_hits", &folded).1 == [1, 0], "coplanar overlap beyond a shared edge");
        let tri = [(0., 0., 0.), (2., 0., 0.), (0., 2., 0.)];
        let duplicate = self_detect(&mesh(&tri, &[0, 1, 2, 2, 1, 0]));
        assert!(int_array("self_hits", &duplicate).1 == [1, 0], "duplicate overlapping primitives");
        let duplicate_without_coplanar = run_checked(
            &Params {
                intersecting_group: None,
                include_coplanar: false,
                self_intersections_attribute: Some("self_hits".to_owned()),
                ..Params::default()
            },
            &no_collision(),
            &mesh(&tri, &[0, 1, 2, 2, 1, 0]),
        ).unwrap();
        assert!(int_array("self_hits", &duplicate_without_coplanar).1.is_empty(), "AxA include_coplanar=false");
        let quad_topology = topology::create_owned(
            4,
            vec![0, 1, 2, 3],
            vec![0, 4],
            vec![topology::PrimitiveKind::Polygon],
        ).unwrap();
        let quad_source = mesh(&square, &[0, 1, 2, 0, 2, 3]);
        let quad = geometry::create(geometry::positions(&quad_source).clone(), quad_topology, None, None, None).unwrap();
        assert!(
            int_array("self_hits", &self_detect(&quad)).1.is_empty(),
            "exposed one polygon's triangulation diagonal as AxA"
        );
        expect(Code::InvalidParameter, run_checked(&Params { grain: 0, ..Params::default() }, &collision(), &source()));
    }

    fn grid(columns: i64, rows: i64, size: f64) -> (Vec<(f64, f64, f64)>, Vec<i64>) {
        let mut points = Vec::new();
        for row in 0..=rows {
            for column in 0..=columns {
                points.push((
                    (column as f64 / columns as f64 - 0.5) * size,
                    (row as f64 / rows as f64 - 0.5) * size,
                    0.0,
                ));
            }
        }
        let stride = columns + 1;
        let mut triangles = Vec::new();
        for row in 0..rows {
            for column in 0..columns {
                let a = row * stride + column;
                let (b, c, d) = (a + 1, a + stride, a + stride + 1);
                if (row + column) % 2 == 0 {
                    triangles.extend_from_slice(&[a, b, d, a, d, c]);
                } else {
                    triangles.extend_from_slice(&[a, b, c, b, d, c]);
                }
            }
        }
        (points, triangles)
    }

    use parallel::with_domains as at_domains;

    fn parallel_exact_fixture(source_fixture: bool) {
        let (mut points, mut triangles) = if source_fixture { grid(79, 59, 20.0) } else { grid(80, 60, 20.0) };
        if source_fixture {
            for point in &mut points {
                *point = (point.0, 0., point.1);
            }
            for triangle in triangles.chunks_exact_mut(3) {
                triangle.swap(1, 2);
            }
        }
        let source = mesh(&points, &triangles);
        let rotated: Vec<(f64, f64, f64)> = points
            .iter()
            .map(|&(x, y, z)| {
                let angle = std::f64::consts::PI / 2.0;
                (x, y * angle.cos() - z * angle.sin(), y * angle.sin() + z * angle.cos())
            })
            .collect();
        let collision = mesh(&rotated, &triangles);
        let params = Params {
            grain: 31,
            intersecting_group: Some("intersections".to_owned()),
            intersections_attribute: Some("hits".to_owned()),
            count_attribute: Some("hit_count".to_owned()),
            ..Params::default()
        };
        let run_cross = |domains: i64| at_domains(domains, &|| run_checked(&params, &collision, &source).unwrap());
        let one = run_cross(1);
        let four = run_cross(4);
        assert!(output_signature(&one) == output_signature(&four), "one/four-domain output drift");
        assert!(group::cardinality(&named_group("intersections", &one)) > 0, "scale fixture found no intersections");
        let count = points.len() as i64;
        let mut combined_points = points.clone();
        combined_points.extend_from_slice(&rotated);
        let mut combined_triangles = triangles.clone();
        combined_triangles.extend(triangles.iter().map(|v| v + count));
        let combined = mesh(&combined_points, &combined_triangles);
        let self_params = Params {
            grain: if source_fixture { 16_384 } else { 31 },
            intersecting_group: None,
            self_intersecting_group: Some("self_intersections".to_owned()),
            self_intersections_attribute: Some("self_hits".to_owned()),
            self_count_attribute: Some("self_hit_count".to_owned()),
            ..Params::default()
        };
        let run_self = |domains: i64| {
            at_domains(domains, &|| run_checked(&self_params, &no_collision(), &combined).unwrap())
        };
        let signature = |value: &geometry::T| {
            let (offsets, values) = int_array("self_hits", value);
            let selected = named_group("self_intersections", value);
            (
                offsets,
                values,
                ints("self_hit_count", value),
                (0..geometry::primitive_count(value)).map(|p| group::mem(p, &selected)).collect::<Vec<_>>(),
            )
        };
        let one = run_self(1);
        let four = run_self(4);
        assert!(signature(&one) == signature(&four), "AxA one/four-domain output drift");
        assert!(!int_array("self_hits", &one).1.is_empty(), "AxA scale fixture found no intersections");
    }

    #[test]
    fn parallel_exact() {
        parallel_exact_fixture(false);
        parallel_exact_fixture(true);
    }
}
