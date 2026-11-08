//! Exact arrangement of projected planar constraint segments.
use std::cmp::Ordering;
use std::sync::Mutex;
use std::sync::Arc;

use rdk_core::{cancel, error, support};
use rdk_core::error::Code;

use crate::bounds2_index;
use crate::implicit_point::{self, Projection};
use crate::predicates::{self, Sign};

const MAX_INT: i64 = (1 << 62) - 1;

pub struct T {
    source_count: i64,
    source: implicit_point::Source,
    explicit_cache: Mutex<Vec<Option<implicit_point::T>>>,
    split_points: Vec<implicit_point::T>,
    x: Arc<Vec<f64>>,
    y: Arc<Vec<f64>>,
    inserted: Arc<[i64]>,
    constraints: Arc<[i64]>,
    constraint_winding: Arc<[i64]>,
    split_first: Vec<i64>,
    split_second: Vec<i64>,
    split_parameter: Vec<f64>,
}

fn invalid(message: &str) -> error::T {
    error::new("Planar_constraints", Code::InvalidArgument, message.to_owned())
}

fn invalid_string(message: String) -> error::T {
    error::new("Planar_constraints", Code::InvalidArgument, message)
}

/// Maps an internal error to the OCaml `Error` message.
fn message_of(value: &error::T) -> String {
    if error::is_cancelled(value) {
        "Planar constraint arrangement was cancelled".to_owned()
    } else {
        error::message(value).to_owned()
    }
}

pub fn point_count(value: &T) -> i64 {
    value.x.len() as i64
}

pub fn split_point_count(value: &T) -> i64 {
    value.split_points.len() as i64
}

fn check_split(value: &T, split: i64) -> Result<(), error::T> {
    if split < 0 || split >= split_point_count(value) {
        return Err(invalid("Planar_constraints: split point is out of range"));
    }
    Ok(())
}

pub fn split_source_first(value: &T, split: i64) -> Result<i64, error::T> {
    check_split(value, split)?;
    Ok(value.split_first[split as usize])
}

pub fn split_source_second(value: &T, split: i64) -> Result<i64, error::T> {
    check_split(value, split)?;
    Ok(value.split_second[split as usize])
}

pub fn split_source_parameter(value: &T, split: i64) -> Result<f64, error::T> {
    check_split(value, split)?;
    Ok(value.split_parameter[split as usize])
}

fn exact_point(value: &T, point: i64) -> Result<implicit_point::T, error::T> {
    if point < 0 || point >= point_count(value) {
        return Err(invalid("Planar_constraints: point is out of range"));
    }
    if point >= value.source_count {
        return Ok(value.split_points[(point - value.source_count) as usize].clone());
    }
    let mut cache = match value.explicit_cache.lock() {
        Ok(guard) => guard,
        Err(poisoned) => poisoned.into_inner(),
    };
    if let Some(result) = &cache[point as usize] {
        return Ok(result.clone());
    }
    let result = match implicit_point::explicit(&value.source, point) {
        Ok(result) => result,
        Err(_) => return Err(invalid("Planar constraints lost an explicit source point")),
    };
    cache[point as usize] = Some(result.clone());
    Ok(result)
}

#[allow(non_snake_case)]
pub mod View {
    use super::{T, exact_point};
    use crate::implicit_point;
    use crate::predicates::Sign;
    use rdk_core::error;
    use std::sync::Arc;

    #[derive(Clone, Debug)]
    pub struct View {
        pub x: Arc<Vec<f64>>,
        pub y: Arc<Vec<f64>>,
        pub insert_points: Arc<[i64]>,
        pub constraint_points: Arc<[i64]>,
        pub constraint_winding: Arc<[i64]>,
    }

    pub fn view(value: &T) -> View {
        View {
            x: Arc::clone(&value.x),
            y: Arc::clone(&value.y),
            insert_points: Arc::clone(&value.inserted),
            constraint_points: Arc::clone(&value.constraints),
            constraint_winding: Arc::clone(&value.constraint_winding),
        }
    }

    pub fn point(value: &T, point: i64) -> Result<implicit_point::T, error::T> {
        exact_point(value, point)
    }

    /// Panics (OCaml `Invalid_argument`) when a point index is out of range.
    pub fn orient2d(value: &T, a: i64, b: i64, c: i64) -> Sign {
        let pa = exact_point(value, a).expect("Planar_constraints: point is out of range");
        let pb = exact_point(value, b).expect("Planar_constraints: point is out of range");
        let pc = exact_point(value, c).expect("Planar_constraints: point is out of range");
        implicit_point::orient2d_xy(&pa, &pb, &pc)
    }

    /// Panics (OCaml `Invalid_argument`) when a point index is out of range.
    pub fn incircle(value: &T, a: i64, b: i64, c: i64, d: i64) -> Sign {
        let pa = exact_point(value, a).expect("Planar_constraints: point is out of range");
        let pb = exact_point(value, b).expect("Planar_constraints: point is out of range");
        let pc = exact_point(value, c).expect("Planar_constraints: point is out of range");
        let pd = exact_point(value, d).expect("Planar_constraints: point is out of range");
        implicit_point::incircle_xy(&pa, &pb, &pc, &pd)
    }
}

fn opposite(left: Sign, right: Sign) -> bool {
    matches!(
        (left, right),
        (Sign::Negative, Sign::Positive) | (Sign::Positive, Sign::Negative)
    )
}

fn omin(a: f64, b: f64) -> f64 {
    if a <= b { a } else { b }
}

fn omax(a: f64, b: f64) -> f64 {
    if a >= b { a } else { b }
}

/// OCaml `Float.compare` as -1/0/1.
fn float_compare(a: f64, b: f64) -> i64 {
    if a < b {
        -1
    } else if a > b {
        1
    } else if a == b {
        0
    } else if a.is_nan() {
        if b.is_nan() { 0 } else { -1 }
    } else {
        1
    }
}

fn to_ordering(value: i64) -> Ordering {
    value.cmp(&0)
}

fn stable_parameter(first: f64, second: f64, point: f64) -> f64 {
    let denominator = second - first;
    let numerator = point - first;
    if denominator.is_finite() && numerator.is_finite() {
        numerator / denominator
    } else {
        let scale = omax(first.abs(), omax(second.abs(), point.abs()));
        if scale == 0. {
            0.
        } else {
            ((point / scale) - (first / scale)) / ((second / scale) - (first / scale))
        }
    }
}

fn explicit_of(
    source: &implicit_point::Source,
    cache: &mut [Option<implicit_point::T>],
    point: i64,
) -> Result<implicit_point::T, error::T> {
    if let Some(value) = &cache[point as usize] {
        return Ok(value.clone());
    }
    let value = match implicit_point::explicit(source, point) {
        Ok(value) => value,
        Err(_) => return Err(invalid("Planar explicit point failed")),
    };
    cache[point as usize] = Some(value.clone());
    Ok(value)
}

fn run(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    split_crossings: bool,
    x: &[f64],
    y: &[f64],
    segment_points: &[i64],
    embedded_points: &[i64],
    segment_winding: Option<&[i64]>,
) -> Result<T, error::T> {
    cancel::check_opt(cancel_value)?;
    if grain <= 0 {
        return Err(invalid("Planar constraint grain must be positive"));
    }
    let source_count = x.len();
    if source_count == 0 || y.len() != source_count {
        return Err(invalid(
            "Planar constraints require equal non-empty coordinate planes",
        ));
    }
    for (point, value) in x.iter().enumerate() {
        if !(value.is_finite() && y[point].is_finite()) {
            return Err(invalid_string(format!(
                "Planar constraint point {} is non-finite",
                point
            )));
        }
    }
    if segment_points.len() % 2 != 0 {
        return Err(invalid(
            "Planar constraint endpoint array must contain pairs",
        ));
    }
    let segment_count = segment_points.len() / 2;
    let segment_winding: Vec<i64> = match segment_winding {
        None => vec![0; segment_count],
        Some(values) if values.len() == segment_count => values.to_vec(),
        Some(_) => {
            return Err(invalid(
                "Planar constraint winding cardinality must match segment count",
            ));
        }
    };
    let sc = source_count as i64;
    let mut first = vec![0i64; segment_count];
    let mut second = vec![0i64; segment_count];
    let mut min_x = vec![0.; segment_count];
    let mut min_y = vec![0.; segment_count];
    let mut max_x = vec![0.; segment_count];
    let mut max_y = vec![0.; segment_count];
    for segment in 0..segment_count {
        let a = segment_points[segment * 2];
        let b = segment_points[segment * 2 + 1];
        if a < 0 || a >= sc || b < 0 || b >= sc {
            return Err(invalid("Planar constraint endpoint is out of range"));
        }
        let (au, bu) = (a as usize, b as usize);
        if a == b || (x[au] == x[bu] && y[au] == y[bu]) {
            return Err(invalid("Planar constraint segment is degenerate"));
        }
        first[segment] = a;
        second[segment] = b;
        min_x[segment] = omin(x[au], x[bu]);
        min_y[segment] = omin(y[au], y[bu]);
        max_x[segment] = omax(x[au], x[bu]);
        max_y[segment] = omax(y[au], y[bu]);
    }
    // Rust panics replace Invalid_argument in predicates: finiteness is validated above, so none can fire.
    let source_z = vec![0.; source_count];
    let source = match implicit_point::source(x.to_vec(), y.to_vec(), source_z) {
        Ok(source) => source,
        Err(_) => return Err(invalid("Planar exact source failed")),
    };
    let mut explicit_cache: Vec<Option<implicit_point::T>> = vec![None; source_count];
    let mut split_segments: Vec<i64> = Vec::new();
    let mut split_points: Vec<i64> = Vec::new();
    for segment in 0..segment_count {
        split_segments.push(segment as i64);
        split_points.push(first[segment]);
        split_segments.push(segment as i64);
        split_points.push(second[segment]);
    }
    // Keeps OCaml's `explicit 0` side effect on the cache.
    explicit_of(&source, &mut explicit_cache, 0)?;
    let mut crossing_points: Vec<implicit_point::T> = Vec::new();
    let mut crossing_first: Vec<i64> = Vec::new();
    let mut crossing_second: Vec<i64> = Vec::new();
    let on_segment = |segment: usize, point: usize| -> bool {
        x[point] >= min_x[segment]
            && x[point] <= max_x[segment]
            && y[point] >= min_y[segment]
            && y[point] <= max_y[segment]
    };
    let segment_tree =
        bounds2_index::create(cancel_value, &min_x, &min_y, &max_x, &max_y)?;
    let mut classified_pairs: i64 = 0;
    bounds2_index::candidate_pairs(cancel_value, &segment_tree, &mut |left, right| {
        classified_pairs += 1;
        cancel::poll(cancel_value, classified_pairs, 4095)?;
        let (lu, ru) = (left as usize, right as usize);
        let (a, b, c, d) = (first[lu], second[lu], first[ru], second[ru]);
        let ab_c = predicates::orient2d_packed(x, y, a, b, c);
        let ab_d = predicates::orient2d_packed(x, y, a, b, d);
        let cd_a = predicates::orient2d_packed(x, y, c, d, a);
        let cd_b = predicates::orient2d_packed(x, y, c, d, b);
        if opposite(ab_c, ab_d) && opposite(cd_a, cd_b) {
            if !split_crossings {
                return Err(invalid(
                    "Planar constraints cross and crossing splitting is disabled",
                ));
            }
            let point = match implicit_point::line_line(&source, Projection::XY, a, b, c, d) {
                Ok(point) => point,
                Err(_) => return Err(invalid("Planar proper crossing construction failed")),
            };
            crossing_points.push(point);
            crossing_first.push(left);
            crossing_second.push(right);
        } else {
            if ab_c == Sign::Zero && on_segment(lu, c as usize) {
                split_segments.push(left);
                split_points.push(c);
            }
            if ab_d == Sign::Zero && on_segment(lu, d as usize) {
                split_segments.push(left);
                split_points.push(d);
            }
            if cd_a == Sign::Zero && on_segment(ru, a as usize) {
                split_segments.push(right);
                split_points.push(a);
            }
            if cd_b == Sign::Zero && on_segment(ru, b as usize) {
                split_segments.push(right);
                split_points.push(b);
            }
        }
        Ok(())
    })?;
    let embedded_count = embedded_points.len();
    if embedded_count > 0 {
        let mut embedded_seen = vec![0u8; source_count];
        for &point in embedded_points {
            if point < 0 || point >= sc {
                return Err(invalid("Planar embedded point is out of range"));
            }
            if embedded_seen[point as usize] != 0 {
                return Err(invalid("Planar embedded point is duplicated"));
            }
            embedded_seen[point as usize] = 1;
        }
        let query = |point: i64,
                     visit: &mut dyn FnMut(i64) -> Result<(), error::T>|
         -> Result<(), error::T> {
            let p = point as usize;
            bounds2_index::query(
                cancel_value,
                &segment_tree,
                x[p],
                y[p],
                x[p],
                y[p],
                &mut |segment| {
                    let s = segment as usize;
                    if point != first[s]
                        && point != second[s]
                        && on_segment(s, p)
                        && predicates::orient2d_packed(x, y, first[s], second[s], point)
                            == Sign::Zero
                    {
                        visit(segment)?;
                    }
                    Ok(())
                },
            )
        };
        let mut counts = vec![0_i64; embedded_count];
        support::par_fill(cancel_value, grain, &mut counts, &|index| {
            let mut value: i64 = 0;
            query(embedded_points[index as usize], &mut |_| {
                if value == MAX_INT {
                    return Err(invalid(
                        "Planar embedded incidence cardinality exceeds integer range",
                    ));
                }
                value += 1;
                Ok(())
            })?;
            Ok(value)
        })?;
        let mut offsets = vec![0i64; embedded_count + 1];
        for index in 0..embedded_count {
            let Some(next) = offsets[index].checked_add(counts[index]) else {
                return Err(invalid(
                    "Planar embedded incidence cardinality exceeds array limits",
                ));
            };
            offsets[index + 1] = next;
        }
        let mut incidences = vec![0_i64; offsets[embedded_count] as usize];
        let embedded = embedded_count as i64;
        let windows = support::Windows::new(&mut incidences, support::range_count(embedded, grain), |range| {
            offsets[range.saturating_mul(grain).min(embedded) as usize] as usize
        });
        support::run_indexed_ranges(grain, embedded, &|range, first, last| {
            let window = windows.take(range);
            let base = offsets[first as usize];
            for index in first..last {
                cancel::poll(cancel_value, index, 4095)?;
                let mut at = offsets[index as usize];
                query(embedded_points[index as usize], &mut |segment| {
                    window[(at - base) as usize] = segment;
                    at += 1;
                    Ok(())
                })?;
                if at != offsets[index as usize + 1] {
                    return Err(invalid(
                        "Planar embedded incidence count changed between passes",
                    ));
                }
            }
            Ok(())
        })?;
        for index in 0..embedded_count {
            let point = embedded_points[index];
            for incidence in offsets[index]..offsets[index + 1] {
                split_segments.push(incidences[incidence as usize]);
                split_points.push(point);
            }
        }
    }
    let crossing_count = crossing_points.len();
    let mut crossing_point_id = vec![-1i64; crossing_count];
    let mut unique_points: Vec<implicit_point::T> = Vec::new();
    let mut unique_first: Vec<i64> = Vec::new();
    let mut unique_second: Vec<i64> = Vec::new();
    if crossing_count > 0 {
        let mut original_seen = vec![0u8; source_count];
        let mut original_count: i64 = 0;
        let mut include_original = |point: i64| {
            if original_seen[point as usize] == 0 {
                original_seen[point as usize] = 1;
                original_count += 1;
            }
        };
        for segment in 0..segment_count {
            include_original(first[segment]);
            include_original(second[segment]);
        }
        for &point in embedded_points {
            include_original(point);
        }
        let Some(_) = (crossing_count as i64).checked_add(original_count) else {
            return Err(invalid(
                "Planar constraint intersection cardinality exceeds array limits",
            ));
        };
        let entries = original_count as usize + crossing_count;
        let mut entry_points: Vec<implicit_point::T> = Vec::with_capacity(entries);
        let mut entry_original = vec![-1i64; entries];
        let mut entry_crossing = vec![-1i64; entries];
        let mut at = 0usize;
        for point in 0..source_count {
            if original_seen[point] != 0 {
                entry_points.push(explicit_of(&source, &mut explicit_cache, point as i64)?);
                entry_original[at] = point as i64;
                at += 1;
            }
        }
        for crossing in 0..crossing_count {
            entry_points.push(crossing_points[crossing].clone());
            entry_crossing[at] = crossing as i64;
            at += 1;
        }
        let mut order: Vec<usize> = (0..entries).collect();
        order.sort_by(|&left, &right| {
            let compared = implicit_point::compare_x(&entry_points[left], &entry_points[right]);
            if compared != 0 {
                return to_ordering(compared);
            }
            let compared = implicit_point::compare_y(&entry_points[left], &entry_points[right]);
            if compared != 0 {
                return to_ordering(compared);
            }
            left.cmp(&right)
        });
        let mut position = 0usize;
        while position < entries {
            let mut last = position + 1;
            let mut representative: i64 = -1;
            while last < entries
                && implicit_point::equal(&entry_points[order[position]], &entry_points[order[last]])
            {
                last += 1;
            }
            for slot in position..last {
                let original = entry_original[order[slot]];
                if original >= 0 && (representative < 0 || original < representative) {
                    representative = original;
                }
            }
            let point_id = if representative >= 0 {
                representative
            } else {
                let crossing = entry_crossing[order[position]];
                if crossing < 0 {
                    return Err(invalid("Planar crossing group has no construction"));
                }
                unique_points.push(entry_points[order[position]].clone());
                unique_first.push(crossing_first[crossing as usize]);
                unique_second.push(crossing_second[crossing as usize]);
                sc + unique_points.len() as i64 - 1
            };
            for slot in position..last {
                let crossing = entry_crossing[order[slot]];
                if crossing >= 0 {
                    crossing_point_id[crossing as usize] = point_id;
                }
            }
            position = last;
        }
    }
    for crossing in 0..crossing_count {
        let point = crossing_point_id[crossing];
        split_segments.push(crossing_first[crossing]);
        split_points.push(point);
        split_segments.push(crossing_second[crossing]);
        split_points.push(point);
    }
    let unique_count = unique_points.len();
    let total_points = source_count + unique_count;
    let split_objects = unique_points;
    let mut output_x = vec![0.; total_points];
    let mut output_y = vec![0.; total_points];
    output_x[..source_count].copy_from_slice(x);
    output_y[..source_count].copy_from_slice(y);
    let mut split_first_source = vec![0i64; unique_count];
    let mut split_second_source = vec![0i64; unique_count];
    let mut split_parameter = vec![0.; unique_count];
    for split in 0..unique_count {
        let (px, py, _) = implicit_point::approximate(&split_objects[split]);
        output_x[source_count + split] = px;
        output_y[source_count + split] = py;
        let segment = unique_first[split] as usize;
        let a = first[segment] as usize;
        let b = second[segment] as usize;
        split_first_source[split] = a as i64;
        split_second_source[split] = b as i64;
        let dx = x[b] - x[a];
        let dy = y[b] - y[a];
        let parameter = if dx.abs() >= dy.abs() {
            stable_parameter(x[a], x[b], px)
        } else {
            stable_parameter(y[a], y[b], py)
        };
        split_parameter[split] = omax(0., omin(1., parameter));
    }
    let event_count = split_segments.len();
    let mut atomic: Vec<i64> = Vec::new();
    let mut atomic_winding: Vec<i64> = Vec::new();
    if event_count == segment_points.len() {
        for segment in 0..segment_count {
            atomic.push(first[segment]);
            atomic.push(second[segment]);
            atomic_winding.push(segment_winding[segment]);
        }
    } else {
        let object_of_point = |cache: &mut [Option<implicit_point::T>],
                               point: i64|
         -> Result<implicit_point::T, error::T> {
            if point < sc {
                explicit_of(&source, cache, point)
            } else {
                Ok(split_objects[(point - sc) as usize].clone())
            }
        };
        // Resolve every event point to its exact object once (the comparator then only reads).
        let mut event_objects: Vec<Option<implicit_point::T>> = vec![None; event_count];
        for event in 0..event_count {
            let point = split_points[event];
            if point >= sc {
                event_objects[event] = Some(object_of_point(&mut explicit_cache, point)?);
            }
        }
        // Source-point objects are needed only for mixed comparisons; build lazily via the cache.
        let mut source_objects: Vec<Option<implicit_point::T>> = vec![None; source_count];
        for event in 0..event_count {
            let point = split_points[event];
            if point < sc && source_objects[point as usize].is_none() {
                source_objects[point as usize] = Some(explicit_of(
                    &source,
                    &mut explicit_cache,
                    point,
                )?);
            }
        }
        let obj = |point: i64| -> &implicit_point::T {
            if point < sc {
                source_objects[point as usize].as_ref().expect("resolved above")
            } else {
                &split_objects[(point - sc) as usize]
            }
        };
        let same_point = |left: i64, right: i64| -> bool {
            if left < sc && right < sc {
                x[left as usize] == x[right as usize] && y[left as usize] == y[right as usize]
            } else {
                implicit_point::equal(obj(left), obj(right))
            }
        };
        let mut event_order: Vec<usize> = (0..event_count).collect();
        event_order.sort_by(|&left, &right| {
            let ls = split_segments[left];
            let rs = split_segments[right];
            if ls != rs {
                return ls.cmp(&rs);
            }
            let segment = ls as usize;
            let lp = split_points[left];
            let rp = split_points[right];
            let a = first[segment] as usize;
            let b = second[segment] as usize;
            let by_x = (x[b] - x[a]).abs() >= (y[b] - y[a]).abs();
            let direction = if by_x {
                float_compare(x[a], x[b])
            } else {
                float_compare(y[a], y[b])
            };
            let compared = if lp < sc && rp < sc {
                if by_x {
                    float_compare(x[lp as usize], x[rp as usize])
                } else {
                    float_compare(y[lp as usize], y[rp as usize])
                }
            } else if by_x {
                implicit_point::compare_x(obj(lp), obj(rp))
            } else {
                implicit_point::compare_y(obj(lp), obj(rp))
            };
            let compared = if direction <= 0 { compared } else { -compared };
            if compared != 0 {
                to_ordering(compared)
            } else {
                lp.cmp(&rp)
            }
        });
        let mut event = 0usize;
        while event < event_count {
            let segment = split_segments[event_order[event]];
            let mut points: Vec<i64> = Vec::new();
            while event < event_count && split_segments[event_order[event]] == segment {
                let point = split_points[event_order[event]];
                if points.is_empty() || !same_point(points[points.len() - 1], point) {
                    points.push(point);
                }
                event += 1;
            }
            for local in 0..points.len().saturating_sub(1) {
                if points[local] != points[local + 1] {
                    atomic.push(points[local]);
                    atomic.push(points[local + 1]);
                    atomic_winding.push(segment_winding[segment as usize]);
                }
            }
        }
    }
    let atomic_count = atomic.len() / 2;
    let atomic_first = |segment: usize| omin_i(atomic[segment * 2], atomic[segment * 2 + 1]);
    let atomic_second = |segment: usize| omax_i(atomic[segment * 2], atomic[segment * 2 + 1]);
    let mut atomic_order: Vec<usize> = (0..atomic_count).collect();
    // ponytail: stable sort where OCaml's heap sort is unstable; equal pairs are merged by summing
    // windings, which differs only in which overflow is detected first.
    atomic_order.sort_by(|&left, &right| {
        atomic_first(left)
            .cmp(&atomic_first(right))
            .then(atomic_second(left).cmp(&atomic_second(right)))
    });
    let mut unique_constraints: Vec<i64> = Vec::new();
    let mut unique_winding: Vec<i64> = Vec::new();
    let mut previous_a: i64 = -1;
    let mut previous_b: i64 = -1;
    for &segment in &atomic_order {
        let a = atomic_first(segment);
        let b = atomic_second(segment);
        let winding = atomic_winding[segment];
        let winding = if atomic[segment * 2] == a {
            winding
        } else {
            winding.checked_neg().ok_or_else(|| invalid("Planar constraint winding exceeds integer range"))?
        };
        if a != previous_a || b != previous_b {
            unique_constraints.push(a);
            unique_constraints.push(b);
            unique_winding.push(winding);
            previous_a = a;
            previous_b = b;
        } else {
            let last = unique_winding.len() - 1;
            let previous = unique_winding[last];
            unique_winding[last] =
                previous.checked_add(winding).ok_or_else(|| invalid("Planar constraint winding exceeds integer range"))?;
        }
    }
    let inserted: Vec<i64> = (0..unique_count as i64).map(|split| sc + split).collect();
    Ok(T {
        source_count: sc,
        source,
        explicit_cache: Mutex::new(explicit_cache),
        split_points: split_objects,
        x: Arc::from(output_x),
        y: Arc::from(output_y),
        inserted: Arc::from(inserted),
        constraints: Arc::from(unique_constraints),
        constraint_winding: Arc::from(unique_winding),
        split_first: split_first_source,
        split_second: split_second_source,
        split_parameter,
    })
}

fn omin_i(a: i64, b: i64) -> i64 {
    if a <= b { a } else { b }
}

fn omax_i(a: i64, b: i64) -> i64 {
    if a >= b { a } else { b }
}

/// Split proper crossings, constraint endpoints, and `embedded_points` lying on constraint
/// segments into a non-crossing PSLG. `grain` defaults to 16384 when `None`.
pub fn build(
    cancel_value: Option<&cancel::T>,
    grain: Option<i64>,
    split_crossings: bool,
    x: &[f64],
    y: &[f64],
    segment_points: &[i64],
    embedded_points: Option<&[i64]>,
    segment_winding: Option<&[i64]>,
) -> Result<T, String> {
    run(
        cancel_value,
        grain.unwrap_or(16_384),
        split_crossings,
        x,
        y,
        segment_points,
        embedded_points.unwrap_or(&[]),
        segment_winding,
    )
    .map_err(|e| message_of(&e))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get(value: Result<T, String>) -> T {
        value.unwrap_or_else(|message| panic!("{}", message))
    }

    fn cp(value: &T) -> Vec<i64> {
        View::view(value).constraint_points.to_vec()
    }

    #[test]
    fn square_crossing() {
        let x = [0., 2., 2., 0.];
        let y = [0., 0., 2., 2.];
        let value = get(build(None, None, true, &x, &y, &[0, 2, 1, 3], None, None));
        assert_eq!(point_count(&value) - split_point_count(&value), 4);
        assert_eq!(point_count(&value), 5);
        assert_eq!(split_point_count(&value), 1);
        assert_eq!(&View::view(&value).insert_points[..], &[4]);
        let view = View::view(&value);
        assert!(view.x[4] == 1. && view.y[4] == 1.);
        assert_eq!(cp(&value), vec![0, 4, 1, 4, 2, 4, 3, 4]);
        let weighted = get(build(
            None,
            None,
            true,
            &x,
            &y,
            &[0, 2, 1, 3],
            None,
            Some(&[2, 3]),
        ));
        assert_eq!(&View::view(&weighted).constraint_winding[..], &[2, 3, -2, -3]);
        assert!(split_source_first(&value, 0).unwrap() == 0 && split_source_second(&value, 0).unwrap() == 2);
        assert!(split_source_parameter(&value, 0).unwrap() == 0.5);
    }

    #[test]
    fn crossing_policy() {
        let x = [0., 2., 2., 0.];
        let y = [0., 0., 2., 2.];
        assert!(build(None, None, false, &x, &y, &[0, 2, 1, 3], None, None).is_err());
    }

    #[test]
    fn t_junction_and_overlap() {
        let x = [0., 2., 1., 1.];
        let y = [0., 0., 0., 1.];
        let junction = get(build(None, None, true, &x, &y, &[0, 1, 2, 3], None, None));
        assert_eq!(split_point_count(&junction), 0);
        assert_eq!(cp(&junction), vec![0, 2, 1, 2, 2, 3]);
        let x = [0., 3., 1., 2.];
        let y = [0., 0., 0., 0.];
        let overlap = get(build(None, None, true, &x, &y, &[0, 1, 2, 3], None, None));
        assert_eq!(cp(&overlap), vec![0, 2, 1, 3, 2, 3]);
    }

    fn with_domains(domains: i64, work: &dyn Fn() -> T) -> T {
        let slot: std::cell::RefCell<Option<T>> = std::cell::RefCell::new(None);
        parallel::run_unit(Some(domains), &|| {
            *slot.borrow_mut() = Some(work());
            Ok(())
        })
        .unwrap();
        slot.into_inner().unwrap()
    }

    #[test]
    fn embedded_source_points() {
        let x = [0., 1., 2., 3., 4., 2.];
        let y = [0., 0., 0., 0., 0., 1.];
        let run = |domains| {
            with_domains(domains, &|| {
                get(build(
                    None,
                    Some(1),
                    true,
                    &x,
                    &y,
                    &[0, 4],
                    Some(&[0, 1, 2, 3, 4, 5]),
                    None,
                ))
            })
        };
        let one = run(1);
        let four = run(4);
        assert_eq!(split_point_count(&one), 0);
        assert_eq!(cp(&one), vec![0, 1, 1, 2, 2, 3, 3, 4]);
        assert_eq!(cp(&four), cp(&one));
        let maximum = f64::MAX;
        let extreme = get(build(
            None,
            None,
            true,
            &[-maximum, 0., maximum],
            &[0., 0., 0.],
            &[0, 2],
            Some(&[0, 1, 2]),
            None,
        ));
        assert_eq!(cp(&extreme), vec![0, 1, 1, 2]);
        let tiny = f64::from_bits(1);
        let subnormal = get(build(
            None,
            None,
            true,
            &[0., tiny, tiny + tiny],
            &[0., 0., 0.],
            &[0, 2],
            Some(&[0, 1, 2]),
            None,
        ));
        assert_eq!(cp(&subnormal), vec![0, 1, 1, 2]);
        let crossing = get(build(
            None,
            Some(1),
            true,
            &[0., 2., 2., 0., 1.],
            &[0., 0., 2., 2., 1.],
            &[0, 2, 1, 3],
            Some(&[0, 1, 2, 3, 4]),
            None,
        ));
        assert!(split_point_count(&crossing) == 0 && cp(&crossing) == vec![0, 4, 1, 4, 2, 4, 3, 4]);
        let duplicate = get(build(
            None,
            None,
            true,
            &[0., 1., 1., 2.],
            &[0., 0., 0., 0.],
            &[0, 3],
            Some(&[0, 1, 2, 3]),
            None,
        ));
        assert_eq!(cp(&duplicate), vec![0, 1, 1, 3]);
    }

    #[test]
    fn many_way_dedup_and_domains() {
        let x = [0., 2., 0., 2., 1., 1.];
        let y = [0., 2., 2., 0., 0., 2.];
        let run = |domains| {
            with_domains(domains, &|| {
                get(build(None, None, true, &x, &y, &[0, 1, 2, 3, 4, 5], None, None))
            })
        };
        let one = run(1);
        let four = run(4);
        assert_eq!(split_point_count(&one), 1);
        let (a, b) = (View::view(&one), View::view(&four));
        assert!(a.x == b.x && a.y == b.y && a.constraint_points == b.constraint_points);
    }

    #[test]
    fn extreme_scales() {
        let maximum = f64::MAX;
        let large = get(build(
            None,
            None,
            true,
            &[-maximum, maximum, 0., 0.],
            &[0., 0., -maximum, maximum],
            &[0, 1, 2, 3],
            None,
            None,
        ));
        let v = View::view(&large);
        assert!(split_point_count(&large) == 1 && v.x[4] == 0. && v.y[4] == 0.);
        assert!(split_source_parameter(&large, 0).unwrap() == 0.5);
        let tiny = f64::from_bits(1);
        let twice = tiny + tiny;
        let small = get(build(
            None,
            None,
            true,
            &[0., twice, twice, 0.],
            &[0., 0., twice, twice],
            &[0, 2, 1, 3],
            None,
            None,
        ));
        let v = View::view(&small);
        assert!(split_point_count(&small) == 1 && v.x[4] == tiny && v.y[4] == tiny);
    }

    #[test]
    fn scale_and_ownership() {
        let segments = 20_000usize;
        let x: Vec<f64> = (0..segments * 2).map(|p| if p & 1 == 0 { 0. } else { 0.25 }).collect();
        let y: Vec<f64> = (0..segments * 2).map(|p| (p / 2) as f64).collect();
        let edges: Vec<i64> = (0..segments as i64 * 2).collect();
        let value = get(build(None, None, true, &x, &y, &edges, None, None));
        assert!(split_point_count(&value) == 0 && cp(&value).len() == segments * 2);
        let point_count = 20_000usize;
        let xs: Vec<f64> = (0..point_count).map(|p| p as f64).collect();
        let ys = vec![0.; point_count];
        let all: Vec<i64> = (0..point_count as i64).collect();
        let embedded = get(build(
            None,
            Some(257),
            true,
            &xs,
            &ys,
            &[0, point_count as i64 - 1],
            Some(&all),
            None,
        ));
        assert_eq!(cp(&embedded).len(), (point_count - 1) * 2);
    }

    #[test]
    fn validation_and_cancellation() {
        let b = |x: &[f64], y: &[f64], s: &[i64], g: Option<i64>, e: Option<&[i64]>, w: Option<&[i64]>| {
            build(None, g, true, x, y, s, e, w).is_err()
        };
        assert!(b(&[0.], &[], &[], None, None, None));
        assert!(b(&[0., 1.], &[0., 0.], &[0, 0], None, None, None));
        assert!(b(&[0., 1.], &[0., 0.], &[0], None, None, None));
        assert!(b(&[0., f64::INFINITY], &[0., 0.], &[0, 1], None, None, None));
        assert!(b(&[0., 1.], &[0., 0.], &[0, 2], None, None, None));
        assert!(b(&[0., 1.], &[0., 0.], &[0, 1], None, None, Some(&[])));
        assert!(b(&[0., 1.], &[0., 0.], &[0, 1], Some(0), None, None));
        assert!(b(&[0., 1.], &[0., 0.], &[0, 1], None, Some(&[2]), None));
        assert!(b(&[0., 1.], &[0., 0.], &[0, 1], None, Some(&[0, 0]), None));
        let token = cancel::create();
        cancel::cancel(&token);
        assert!(
            build(Some(&token), None, true, &[0., 1.], &[0., 0.], &[0, 1], None, None).is_err()
        );
    }
}
