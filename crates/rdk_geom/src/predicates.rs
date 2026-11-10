//! Filtered exact predicates require finite coordinates and valid packed ranges; programmer-invalid input panics before indexing.
use crate::exact_dyadic::scratch;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Sign {
    Negative,
    Zero,
    Positive,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SegmentSegment {
    Disjoint,
    Point,
    Overlap,
    Degenerate,
}

fn sign_of_comparison(comparison: i64) -> Sign {
    if comparison < 0 {
        Sign::Negative
    } else if comparison > 0 {
        Sign::Positive
    } else {
        Sign::Zero
    }
}

fn exact_orient2d(ax: f64, ay: f64, bx: f64, by: f64, cx: f64, cy: f64) -> Sign {
    sign_of_comparison(scratch::orient2d(ax, ay, bx, by, cx, cy))
}

/// Exact sign of the oriented area determinant `(a-c) x (b-c)`. Panics when any coordinate is
/// non-finite.
pub fn orient2d(ax: f64, ay: f64, bx: f64, by: f64, cx: f64, cy: f64) -> Sign {
    if !(ax.is_finite()
        && ay.is_finite()
        && bx.is_finite()
        && by.is_finite()
        && cx.is_finite()
        && cy.is_finite())
    {
        unreachable!("rdk_geom::predicates::orient2d: coordinates must be finite");
    }
    orient2d_filtered(ax, ay, bx, by, cx, cy)
}

fn orient2d_filtered(ax: f64, ay: f64, bx: f64, by: f64, cx: f64, cy: f64) -> Sign {
    let acx = ax - cx;
    let bcx = bx - cx;
    let acy = ay - cy;
    let bcy = by - cy;
    let left = acx * bcy;
    let right = acy * bcx;
    let determinant = left - right;
    let permanent = left.abs() + right.abs();
    let error_bound = (3. + (16. * f64::EPSILON)) * f64::EPSILON;
    if determinant.is_finite() && permanent.is_finite() && determinant.abs() > error_bound * permanent {
        if determinant < 0. { Sign::Negative } else { Sign::Positive }
    } else {
        exact_orient2d(ax, ay, bx, by, cx, cy)
    }
}

pub fn orient2d_packed(x: &[f64], y: &[f64], a: i64, b: i64, c: i64) -> Sign {
    let (a, b, c) = (a as usize, b as usize, c as usize);
    let (ax, ay, bx, by, cx, cy) = (x[a], y[a], x[b], y[b], x[c], y[c]);
    if !(ax.is_finite()
        && ay.is_finite()
        && bx.is_finite()
        && by.is_finite()
        && cx.is_finite()
        && cy.is_finite())
    {
        unreachable!("rdk_geom::predicates::orient2d_packed: coordinates must be finite");
    }
    orient2d_filtered(ax, ay, bx, by, cx, cy)
}

#[allow(clippy::too_many_arguments)]
fn exact_orient3d(
    ax: f64,
    ay: f64,
    az: f64,
    bx: f64,
    by: f64,
    bz: f64,
    cx: f64,
    cy: f64,
    cz: f64,
    dx: f64,
    dy: f64,
    dz: f64,
) -> Sign {
    sign_of_comparison(scratch::orient3d(ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz))
}

#[allow(clippy::too_many_arguments)]
fn orient3d_filtered(
    ax: f64,
    ay: f64,
    az: f64,
    bx: f64,
    by: f64,
    bz: f64,
    cx: f64,
    cy: f64,
    cz: f64,
    dx: f64,
    dy: f64,
    dz: f64,
) -> Sign {
    let adx = ax - dx;
    let ady = ay - dy;
    let adz = az - dz;
    let bdx = bx - dx;
    let bdy = by - dy;
    let bdz = bz - dz;
    let cdx = cx - dx;
    let cdy = cy - dy;
    let cdz = cz - dz;
    let bdxcdy = bdx * cdy;
    let cdxbdy = cdx * bdy;
    let cdxady = cdx * ady;
    let adxcdy = adx * cdy;
    let adxbdy = adx * bdy;
    let bdxady = bdx * ady;
    let determinant =
        (adz * (bdxcdy - cdxbdy)) + (bdz * (cdxady - adxcdy)) + (cdz * (adxbdy - bdxady));
    let permanent = ((bdxcdy.abs() + cdxbdy.abs()) * adz.abs())
        + ((cdxady.abs() + adxcdy.abs()) * bdz.abs())
        + ((adxbdy.abs() + bdxady.abs()) * cdz.abs());
    let error_bound = (7. + (56. * f64::EPSILON)) * f64::EPSILON;
    if determinant.is_finite() && permanent.is_finite() && determinant.abs() > error_bound * permanent {
        if determinant < 0. { Sign::Negative } else { Sign::Positive }
    } else {
        exact_orient3d(ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz)
    }
}

fn all_finite(values: &[f64]) -> bool {
    values.iter().all(|value| value.is_finite())
}

/// Exact sign of the determinant of `a-d`, `b-d`, `c-d`. Panics for a non-finite coordinate.
#[allow(clippy::too_many_arguments)]
pub fn orient3d(
    ax: f64,
    ay: f64,
    az: f64,
    bx: f64,
    by: f64,
    bz: f64,
    cx: f64,
    cy: f64,
    cz: f64,
    dx: f64,
    dy: f64,
    dz: f64,
) -> Sign {
    if !all_finite(&[ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz]) {
        unreachable!("rdk_geom::predicates::orient3d: coordinates must be finite");
    }
    orient3d_filtered(ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz)
}

pub fn orient3d_packed(x: &[f64], y: &[f64], z: &[f64], a: i64, b: i64, c: i64, d: i64) -> Sign {
    let (a, b, c, d) = (a as usize, b as usize, c as usize, d as usize);
    let (ax, ay, az) = (x[a], y[a], z[a]);
    let (bx, by, bz) = (x[b], y[b], z[b]);
    let (cx, cy, cz) = (x[c], y[c], z[c]);
    let (dx, dy, dz) = (x[d], y[d], z[d]);
    if !all_finite(&[ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz]) {
        unreachable!("rdk_geom::predicates::orient3d_packed: coordinates must be finite");
    }
    orient3d_filtered(ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz)
}

fn exact_incircle(ax: f64, ay: f64, bx: f64, by: f64, cx: f64, cy: f64, dx: f64, dy: f64) -> Sign {
    sign_of_comparison(scratch::incircle(ax, ay, bx, by, cx, cy, dx, dy))
}

#[allow(clippy::too_many_arguments)]
fn incircle_values(ax: f64, ay: f64, bx: f64, by: f64, cx: f64, cy: f64, dx: f64, dy: f64) -> Sign {
    if !all_finite(&[ax, ay, bx, by, cx, cy, dx, dy]) {
        unreachable!("rdk_geom::predicates::incircle: coordinates must be finite");
    }
    let adx = ax - dx;
    let ady = ay - dy;
    let bdx = bx - dx;
    let bdy = by - dy;
    let cdx = cx - dx;
    let cdy = cy - dy;
    let bdxcdy = bdx * cdy;
    let cdxbdy = cdx * bdy;
    let cdxady = cdx * ady;
    let adxcdy = adx * cdy;
    let adxbdy = adx * bdy;
    let bdxady = bdx * ady;
    let alift = (adx * adx) + (ady * ady);
    let blift = (bdx * bdx) + (bdy * bdy);
    let clift = (cdx * cdx) + (cdy * cdy);
    let determinant =
        (alift * (bdxcdy - cdxbdy)) + (blift * (cdxady - adxcdy)) + (clift * (adxbdy - bdxady));
    let permanent = ((bdxcdy.abs() + cdxbdy.abs()) * alift)
        + ((cdxady.abs() + adxcdy.abs()) * blift)
        + ((adxbdy.abs() + bdxady.abs()) * clift);
    let error_bound = (10. + (96. * f64::EPSILON)) * f64::EPSILON;
    if determinant.is_finite() && permanent.is_finite() && determinant.abs() > error_bound * permanent {
        if determinant < 0. { Sign::Negative } else { Sign::Positive }
    } else {
        exact_incircle(ax, ay, bx, by, cx, cy, dx, dy)
    }
}

pub fn incircle_packed(x: &[f64], y: &[f64], a: i64, b: i64, c: i64, d: i64) -> Sign {
    let (a, b, c, d) = (a as usize, b as usize, c as usize, d as usize);
    incircle_values(x[a], y[a], x[b], y[b], x[c], y[c], x[d], y[d])
}

/// Exact sign of twice the signed shoelace area of one packed polygon range.
pub fn polygon_area_sign_packed(x: &[f64], y: &[f64], points: &[i64], first: i64, count: i64) -> Sign {
    if count < 3 || first < 0 || first > points.len() as i64 - count {
        unreachable!("rdk_geom::predicates::polygon_area_sign_packed: invalid polygon range");
    }
    let mut sum = 0.;
    let mut correction = 0.;
    let mut permanent = 0.;
    for local in 0..count {
        let next = if local + 1 == count { 0 } else { local + 1 };
        let a = points[(first + local) as usize];
        let b = points[(first + next) as usize];
        if a < 0 || a >= x.len() as i64 || b < 0 || b >= x.len() as i64 || y.len() != x.len() {
            unreachable!("rdk_geom::predicates::polygon_area_sign_packed: invalid packed planes");
        }
        let (a, b) = (a as usize, b as usize);
        let (ax, ay, bx, by) = (x[a], y[a], x[b], y[b]);
        if !all_finite(&[ax, ay, bx, by]) {
            unreachable!("rdk_geom::predicates::polygon_area_sign_packed: coordinates must be finite");
        }
        let left = ax * by;
        let right = bx * ay;
        let term = left - right;
        let adjusted = term - correction;
        let next_sum = sum + adjusted;
        correction = next_sum - sum - adjusted;
        sum = next_sum;
        permanent = permanent + left.abs() + right.abs();
    }
    let operations = (6. * count as f64) + 2.;
    let product = operations * f64::EPSILON;
    let error_bound = if product < 0.5 { product / (1. - product) } else { f64::INFINITY };
    let sum: f64 = sum;
    if sum.is_finite() && permanent.is_finite() && sum.abs() > error_bound * permanent {
        if sum < 0. { Sign::Negative } else { Sign::Positive }
    } else {
        sign_of_comparison(scratch::polygon_area_packed(x, y, points, first, count))
    }
}

fn opposite_signs(left: Sign, right: Sign) -> bool {
    matches!((left, right), (Sign::Negative, Sign::Positive) | (Sign::Positive, Sign::Negative))
}

#[allow(clippy::too_many_arguments)]
fn projected_segment_orientation(
    projection: i64,
    ax: f64,
    ay: f64,
    az: f64,
    bx: f64,
    by: f64,
    bz: f64,
    cx: f64,
    cy: f64,
    cz: f64,
) -> Sign {
    if projection == 0 {
        orient2d(ax, ay, bx, by, cx, cy)
    } else if projection == 1 {
        orient2d(ay, az, by, bz, cy, cz)
    } else {
        orient2d(az, ax, bz, bx, cz, cx)
    }
}

#[allow(clippy::too_many_arguments)]
fn projected_on_segment_coords(
    projection: i64,
    ax: f64,
    ay: f64,
    az: f64,
    bx: f64,
    by: f64,
    bz: f64,
    px: f64,
    py: f64,
    pz: f64,
) -> bool {
    if projection == 0 {
        px >= ax.min(bx) && px <= ax.max(bx) && py >= ay.min(by) && py <= ay.max(by)
    } else if projection == 1 {
        py >= ay.min(by) && py <= ay.max(by) && pz >= az.min(bz) && pz <= az.max(bz)
    } else {
        pz >= az.min(bz) && pz <= az.max(bz) && px >= ax.min(bx) && px <= ax.max(bx)
    }
}

#[allow(clippy::too_many_arguments)]
fn projected_segment_contact(
    projection: i64,
    signs: [Sign; 4],
    left_a: [f64; 3],
    left_b: [f64; 3],
    right_a: [f64; 3],
    right_b: [f64; 3],
) -> SegmentSegment {
    let [first, second, third, fourth] = signs;
    let on = |a: [f64; 3], b: [f64; 3], p: [f64; 3]| {
        projected_on_segment_coords(projection, a[0], a[1], a[2], b[0], b[1], b[2], p[0], p[1], p[2])
    };
    if (opposite_signs(first, second) && opposite_signs(third, fourth))
        || (first == Sign::Zero && on(left_a, left_b, right_a))
        || (second == Sign::Zero && on(left_a, left_b, right_b))
        || (third == Sign::Zero && on(right_a, right_b, left_a))
        || (fourth == Sign::Zero && on(right_a, right_b, left_b))
    {
        SegmentSegment::Point
    } else {
        SegmentSegment::Disjoint
    }
}

fn collinear_segment_contact(
    left_a: [f64; 3],
    left_b: [f64; 3],
    right_a: [f64; 3],
    right_b: [f64; 3],
) -> SegmentSegment {
    let axis = if left_a[0] != left_b[0] || right_a[0] != right_b[0] {
        0
    } else if left_a[1] != left_b[1] || right_a[1] != right_b[1] {
        1
    } else {
        2
    };
    let (first, second, third, fourth) =
        (left_a[axis], left_b[axis], right_a[axis], right_b[axis]);
    let lower = first.min(second).max(third.min(fourth));
    let upper = first.max(second).min(third.max(fourth));
    if lower > upper {
        SegmentSegment::Disjoint
    } else if lower == upper {
        SegmentSegment::Point
    } else {
        SegmentSegment::Overlap
    }
}

/// Exact closed-segment contact in 3D over finite binary64 coordinates.
pub fn segment_segment_packed(
    x: &[f64],
    y: &[f64],
    z: &[f64],
    left_a: i64,
    left_b: i64,
    right_a: i64,
    right_b: i64,
) -> SegmentSegment {
    let point = |i: i64| [x[i as usize], y[i as usize], z[i as usize]];
    let (la, lb, ra, rb) = (point(left_a), point(left_b), point(right_a), point(right_b));
    if !la.iter().chain(&lb).chain(&ra).chain(&rb).all(|v| v.is_finite()) {
        unreachable!("rdk_geom::predicates::segment_segment_packed: coordinates must be finite");
    }
    if la == lb || ra == rb {
        return SegmentSegment::Degenerate;
    }
    if orient3d(
        la[0], la[1], la[2], lb[0], lb[1], lb[2], ra[0], ra[1], ra[2], rb[0], rb[1], rb[2],
    ) != Sign::Zero
    {
        return SegmentSegment::Disjoint;
    }
    for projection in 0..3 {
        let orient = |a: [f64; 3], b: [f64; 3], c: [f64; 3]| {
            projected_segment_orientation(
                projection, a[0], a[1], a[2], b[0], b[1], b[2], c[0], c[1], c[2],
            )
        };
        let first = orient(la, lb, ra);
        let second = orient(la, lb, rb);
        let third = orient(ra, rb, la);
        let fourth = orient(ra, rb, lb);
        if first != Sign::Zero || second != Sign::Zero || third != Sign::Zero || fourth != Sign::Zero {
            return projected_segment_contact(projection, [first, second, third, fourth], la, lb, ra, rb);
        }
    }
    collinear_segment_contact(la, lb, ra, rb)
}

fn compatible_sides(first: Sign, second: Sign, third: Sign) -> bool {
    let has_negative = first == Sign::Negative || second == Sign::Negative || third == Sign::Negative;
    let has_positive = first == Sign::Positive || second == Sign::Positive || third == Sign::Positive;
    !(has_negative && has_positive)
}

fn same_nonzero_side(left: Sign, right: Sign) -> bool {
    matches!((left, right), (Sign::Negative, Sign::Negative) | (Sign::Positive, Sign::Positive))
}

fn triangle_location_code(first: Sign, second: Sign, third: Sign) -> i64 {
    use Sign::Zero;
    if !compatible_sides(first, second, third) {
        return -1;
    }
    match (first, second, third) {
        (Zero, Zero, Zero) => -1,
        (Zero, Zero, _) => 5,
        (_, Zero, Zero) => 6,
        (Zero, _, Zero) => 4,
        (Zero, _, _) => 1,
        (_, Zero, _) => 2,
        (_, _, Zero) => 3,
        _ => 0,
    }
}

fn flip(sign: Sign) -> Sign {
    match sign {
        Sign::Negative => Sign::Positive,
        Sign::Positive => Sign::Negative,
        Sign::Zero => Sign::Zero,
    }
}

fn projected_triangle_location(x: &[f64], y: &[f64], z: &[f64], a: i64, b: i64, c: i64, point: i64) -> i64 {
    let classify = |first: Sign, second: Sign, third: Sign, orientation: Sign| {
        if orientation == Sign::Negative {
            triangle_location_code(flip(first), flip(second), flip(third))
        } else {
            triangle_location_code(first, second, third)
        }
    };
    let xy = orient2d_packed(x, y, a, b, c);
    if xy != Sign::Zero {
        return classify(
            orient2d_packed(x, y, a, b, point),
            orient2d_packed(x, y, b, c, point),
            orient2d_packed(x, y, c, a, point),
            xy,
        );
    }
    let yz = orient2d_packed(y, z, a, b, c);
    if yz != Sign::Zero {
        return classify(
            orient2d_packed(y, z, a, b, point),
            orient2d_packed(y, z, b, c, point),
            orient2d_packed(y, z, c, a, point),
            yz,
        );
    }
    let zx = orient2d_packed(z, x, a, b, c);
    if zx != Sign::Zero {
        return classify(
            orient2d_packed(z, x, a, b, point),
            orient2d_packed(z, x, b, c, point),
            orient2d_packed(z, x, c, a, point),
            zx,
        );
    }
    -1
}

#[allow(clippy::too_many_arguments)]
fn segment_triangle_code_packed(
    x: &[f64],
    y: &[f64],
    z: &[f64],
    segment_start: i64,
    segment_end: i64,
    triangle_a: i64,
    triangle_b: i64,
    triangle_c: i64,
) -> i64 {
    let (a, b, c, p, q) = (triangle_a, triangle_b, triangle_c, segment_start, segment_end);
    let triangle_degenerate = orient2d_packed(x, y, a, b, c) == Sign::Zero
        && orient2d_packed(y, z, a, b, c) == Sign::Zero
        && orient2d_packed(z, x, a, b, c) == Sign::Zero;
    if triangle_degenerate {
        return 2;
    }
    let start_side = orient3d_packed(x, y, z, a, b, c, p);
    let end_side = orient3d_packed(x, y, z, a, b, c, q);
    if start_side == Sign::Zero && end_side == Sign::Zero {
        1
    } else if same_nonzero_side(start_side, end_side) {
        0
    } else if start_side == Sign::Zero {
        let location = projected_triangle_location(x, y, z, a, b, c, p);
        if location < 0 { 0 } else { 3 + location }
    } else if end_side == Sign::Zero {
        let location = projected_triangle_location(x, y, z, a, b, c, q);
        if location < 0 { 0 } else { 10 + location }
    } else {
        let ab = orient3d_packed(x, y, z, p, q, a, b);
        let bc = orient3d_packed(x, y, z, p, q, b, c);
        let ca = orient3d_packed(x, y, z, p, q, c, a);
        let location = triangle_location_code(ab, bc, ca);
        if location < 0 { 0 } else { 17 + location }
    }
}

fn triangle_degenerate_packed(x: &[f64], y: &[f64], z: &[f64], a: i64, b: i64, c: i64) -> bool {
    orient2d_packed(x, y, a, b, c) == Sign::Zero
        && orient2d_packed(y, z, a, b, c) == Sign::Zero
        && orient2d_packed(z, x, a, b, c) == Sign::Zero
}

fn encoded_segment_location(code: i64) -> i64 {
    if code < 10 {
        0
    } else if code < 17 {
        1
    } else {
        2
    }
}

fn encoded_triangle_location(code: i64) -> i64 {
    if code < 10 {
        code - 3
    } else if code < 17 {
        code - 10
    } else {
        code - 17
    }
}

fn feature_of_segment_location(edge: i64, location: i64) -> i64 {
    match location {
        0 => edge,
        1 => (edge + 1) % 3,
        _ => 4 + edge,
    }
}

fn feature_of_triangle_location(location: i64) -> i64 {
    match location {
        0 => 8,
        1 => 4,
        2 => 5,
        3 => 6,
        4 => 0,
        5 => 1,
        6 => 2,
        _ => unreachable!("rdk_geom::predicates: invalid encoded triangle location"),
    }
}

fn add_feature_pair(output: &mut [i64], count: i64, left: i64, right: i64) -> i64 {
    let mut duplicate = false;
    let mut event = 0;
    while !duplicate && event < count {
        duplicate = output[(event * 2) as usize] == left && output[(event * 2 + 1) as usize] == right;
        event += 1;
    }
    if duplicate {
        count
    } else if count == 2 {
        unreachable!("rdk_geom::predicates: non-coplanar triangles produced more than two intersection points")
    } else {
        output[(count * 2) as usize] = left;
        output[(count * 2 + 1) as usize] = right;
        count + 1
    }
}

fn triangle_vertex(a: i64, b: i64, c: i64, index: i64) -> i64 {
    match index {
        0 => a,
        1 => b,
        _ => c,
    }
}

#[allow(clippy::too_many_arguments)]
fn triangle_triangle_features_into(
    x: &[f64],
    y: &[f64],
    z: &[f64],
    left_a: i64,
    left_b: i64,
    left_c: i64,
    right_a: i64,
    right_b: i64,
    right_c: i64,
    output: &mut [i64],
) -> i64 {
    if output.len() < 4 {
        unreachable!("rdk_geom::predicates::triangle_triangle_features_into: output is too small");
    }
    if triangle_degenerate_packed(x, y, z, left_a, left_b, left_c)
        || triangle_degenerate_packed(x, y, z, right_a, right_b, right_c)
    {
        return -2;
    }
    if orient3d_packed(x, y, z, left_a, left_b, left_c, right_a) == Sign::Zero
        && orient3d_packed(x, y, z, left_a, left_b, left_c, right_b) == Sign::Zero
        && orient3d_packed(x, y, z, left_a, left_b, left_c, right_c) == Sign::Zero
    {
        return -1;
    }
    let mut count = 0;
    for edge in 0..3 {
        if count < 2 {
            let code = segment_triangle_code_packed(
                x,
                y,
                z,
                triangle_vertex(left_a, left_b, left_c, edge),
                triangle_vertex(left_a, left_b, left_c, (edge + 1) % 3),
                right_a,
                right_b,
                right_c,
            );
            if code >= 3 {
                let left_feature = feature_of_segment_location(edge, encoded_segment_location(code));
                let right_feature = feature_of_triangle_location(encoded_triangle_location(code));
                count = add_feature_pair(output, count, left_feature, right_feature);
            }
        }
    }
    for edge in 0..3 {
        if count < 2 {
            let code = segment_triangle_code_packed(
                x,
                y,
                z,
                triangle_vertex(right_a, right_b, right_c, edge),
                triangle_vertex(right_a, right_b, right_c, (edge + 1) % 3),
                left_a,
                left_b,
                left_c,
            );
            if code >= 3 {
                let left_feature = feature_of_triangle_location(encoded_triangle_location(code));
                let right_feature = feature_of_segment_location(edge, encoded_segment_location(code));
                count = add_feature_pair(output, count, left_feature, right_feature);
            }
        }
    }
    if count == 2 {
        let (left0, right0, left1, right1) = (output[0], output[1], output[2], output[3]);
        if left1 < left0 || (left1 == left0 && right1 < right0) {
            output[0] = left1;
            output[1] = right1;
            output[2] = left0;
            output[3] = right0;
        }
    }
    count
}

fn projected_orient(projection: i64, x: &[f64], y: &[f64], z: &[f64], a: i64, b: i64, c: i64) -> Sign {
    if projection == 0 {
        orient2d_packed(x, y, a, b, c)
    } else if projection == 1 {
        orient2d_packed(y, z, a, b, c)
    } else {
        orient2d_packed(z, x, a, b, c)
    }
}

fn projected_u(projection: i64, x: &[f64], y: &[f64], z: &[f64], point: i64) -> f64 {
    let point = point as usize;
    if projection == 0 {
        x[point]
    } else if projection == 1 {
        y[point]
    } else {
        z[point]
    }
}

fn projected_v(projection: i64, x: &[f64], y: &[f64], z: &[f64], point: i64) -> f64 {
    let point = point as usize;
    if projection == 0 {
        y[point]
    } else if projection == 1 {
        z[point]
    } else {
        x[point]
    }
}

fn opposite_sign(first: Sign, second: Sign) -> bool {
    opposite_signs(first, second)
}

fn projected_on_segment(projection: i64, x: &[f64], y: &[f64], z: &[f64], a: i64, b: i64, point: i64) -> bool {
    let au = projected_u(projection, x, y, z, a);
    let av = projected_v(projection, x, y, z, a);
    let bu = projected_u(projection, x, y, z, b);
    let bv = projected_v(projection, x, y, z, b);
    let pu = projected_u(projection, x, y, z, point);
    let pv = projected_v(projection, x, y, z, point);
    pu >= au.min(bu) && pu <= au.max(bu) && pv >= av.min(bv) && pv <= av.max(bv)
}

#[allow(clippy::too_many_arguments)]
fn projected_segments_contact(
    projection: i64,
    x: &[f64],
    y: &[f64],
    z: &[f64],
    a: i64,
    b: i64,
    c: i64,
    d: i64,
) -> bool {
    let abc = projected_orient(projection, x, y, z, a, b, c);
    let abd = projected_orient(projection, x, y, z, a, b, d);
    let cda = projected_orient(projection, x, y, z, c, d, a);
    let cdb = projected_orient(projection, x, y, z, c, d, b);
    (opposite_sign(abc, abd) && opposite_sign(cda, cdb))
        || (abc == Sign::Zero && projected_on_segment(projection, x, y, z, a, b, c))
        || (abd == Sign::Zero && projected_on_segment(projection, x, y, z, a, b, d))
        || (cda == Sign::Zero && projected_on_segment(projection, x, y, z, c, d, a))
        || (cdb == Sign::Zero && projected_on_segment(projection, x, y, z, c, d, b))
}

#[allow(clippy::too_many_arguments)]
fn projected_point_in_triangle(
    projection: i64,
    x: &[f64],
    y: &[f64],
    z: &[f64],
    a: i64,
    b: i64,
    c: i64,
    point: i64,
) -> bool {
    let ab = projected_orient(projection, x, y, z, a, b, point);
    let bc = projected_orient(projection, x, y, z, b, c, point);
    let ca = projected_orient(projection, x, y, z, c, a, point);
    (ab != Sign::Negative && bc != Sign::Negative && ca != Sign::Negative)
        || (ab != Sign::Positive && bc != Sign::Positive && ca != Sign::Positive)
}

#[allow(clippy::too_many_arguments)]
fn coplanar_triangles_contact_packed(
    x: &[f64],
    y: &[f64],
    z: &[f64],
    left_a: i64,
    left_b: i64,
    left_c: i64,
    right_a: i64,
    right_b: i64,
    right_c: i64,
) -> bool {
    let xy = orient2d_packed(x, y, left_a, left_b, left_c);
    let projection = if xy != Sign::Zero {
        0
    } else if orient2d_packed(y, z, left_a, left_b, left_c) != Sign::Zero {
        1
    } else if orient2d_packed(z, x, left_a, left_b, left_c) != Sign::Zero {
        2
    } else {
        -1
    };
    if projection < 0 {
        return false;
    }
    let mut contact = false;
    let mut first_edge = 0;
    while !contact && first_edge < 3 {
        let mut second_edge = 0;
        while !contact && second_edge < 3 {
            contact = projected_segments_contact(
                projection,
                x,
                y,
                z,
                triangle_vertex(left_a, left_b, left_c, first_edge),
                triangle_vertex(left_a, left_b, left_c, (first_edge + 1) % 3),
                triangle_vertex(right_a, right_b, right_c, second_edge),
                triangle_vertex(right_a, right_b, right_c, (second_edge + 1) % 3),
            );
            second_edge += 1;
        }
        first_edge += 1;
    }
    contact
        || projected_point_in_triangle(projection, x, y, z, left_a, left_b, left_c, right_a)
        || projected_point_in_triangle(projection, x, y, z, right_a, right_b, right_c, left_a)
}

pub mod view {
    //! The triangle-feature transport used by `triangle_triangle_features_into`.

    /// Return -1 when the feature is not a vertex.
    pub fn triangle_feature_vertex_local(feature: i64) -> i64 {
        if (0..=2).contains(&feature) { feature } else { -1 }
    }

    /// Return -1 when the feature is not an edge.
    pub fn triangle_feature_edge_local(feature: i64) -> i64 {
        if (4..=6).contains(&feature) { feature - 4 } else { -1 }
    }

    /// Internal transport code; do not persist or interpret it outside RDK.
    #[allow(clippy::too_many_arguments)]
    pub fn segment_triangle_code_packed(
        x: &[f64],
        y: &[f64],
        z: &[f64],
        segment_start: i64,
        segment_end: i64,
        triangle_a: i64,
        triangle_b: i64,
        triangle_c: i64,
    ) -> i64 {
        super::segment_triangle_code_packed(
            x,
            y,
            z,
            segment_start,
            segment_end,
            triangle_a,
            triangle_b,
            triangle_c,
        )
    }

    /// Fill up to two canonical feature pairs (`output` needs length four). Returns -2 for a
    /// degenerate triangle, -1 for coplanar triangles, or the event count 0..2.
    #[allow(clippy::too_many_arguments)]
    pub fn triangle_triangle_features_into(
        x: &[f64],
        y: &[f64],
        z: &[f64],
        left_a: i64,
        left_b: i64,
        left_c: i64,
        right_a: i64,
        right_b: i64,
        right_c: i64,
        output: &mut [i64],
    ) -> i64 {
        super::triangle_triangle_features_into(
            x, y, z, left_a, left_b, left_c, right_a, right_b, right_c, output,
        )
    }

    /// Exact closed contact between two certified nondegenerate coplanar triangles.
    #[allow(clippy::too_many_arguments)]
    pub fn coplanar_triangles_contact_packed(
        x: &[f64],
        y: &[f64],
        z: &[f64],
        left_a: i64,
        left_b: i64,
        left_c: i64,
        right_a: i64,
        right_b: i64,
        right_c: i64,
    ) -> bool {
        super::coplanar_triangles_contact_packed(
            x, y, z, left_a, left_b, left_c, right_a, right_b, right_c,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::exact_dyadic as dyadic;

    fn sign_of(c: i64) -> Sign {
        sign_of_comparison(c)
    }

    fn exact_orient2d_reference(ax: f64, ay: f64, bx: f64, by: f64, cx: f64, cy: f64) -> Sign {
        use dyadic::*;
        let (ax, ay, bx, by, cx, cy) =
            (of_float(ax), of_float(ay), of_float(bx), of_float(by), of_float(cx), of_float(cy));
        sign_of(compare_zero(&subtract(
            &multiply(&subtract(&ax, &cx), &subtract(&by, &cy)),
            &multiply(&subtract(&ay, &cy), &subtract(&bx, &cx)),
        )))
    }

    #[allow(clippy::too_many_arguments)]
    fn exact_orient3d_reference(
        ax: f64,
        ay: f64,
        az: f64,
        bx: f64,
        by: f64,
        bz: f64,
        cx: f64,
        cy: f64,
        cz: f64,
        dx: f64,
        dy: f64,
        dz: f64,
    ) -> Sign {
        use dyadic::*;
        let f = of_float;
        let (ax, ay, az, bx, by, bz) = (f(ax), f(ay), f(az), f(bx), f(by), f(bz));
        let (cx, cy, cz, dx, dy, dz) = (f(cx), f(cy), f(cz), f(dx), f(dy), f(dz));
        let adx = subtract(&ax, &dx);
        let ady = subtract(&ay, &dy);
        let adz = subtract(&az, &dz);
        let bdx = subtract(&bx, &dx);
        let bdy = subtract(&by, &dy);
        let bdz = subtract(&bz, &dz);
        let cdx = subtract(&cx, &dx);
        let cdy = subtract(&cy, &dy);
        let cdz = subtract(&cz, &dz);
        let first = multiply(&adx, &subtract(&multiply(&bdy, &cdz), &multiply(&bdz, &cdy)));
        let second = multiply(&ady, &subtract(&multiply(&bdz, &cdx), &multiply(&bdx, &cdz)));
        let third = multiply(&adz, &subtract(&multiply(&bdx, &cdy), &multiply(&bdy, &cdx)));
        sign_of(compare_zero(&add(&add(&first, &second), &third)))
    }

    #[allow(clippy::too_many_arguments)]
    fn exact_incircle_reference(
        ax: f64,
        ay: f64,
        bx: f64,
        by: f64,
        cx: f64,
        cy: f64,
        dx: f64,
        dy: f64,
    ) -> Sign {
        use dyadic::*;
        let f = of_float;
        let (ax, ay, bx, by, cx, cy, dx, dy) = (f(ax), f(ay), f(bx), f(by), f(cx), f(cy), f(dx), f(dy));
        let adx = subtract(&ax, &dx);
        let ady = subtract(&ay, &dy);
        let bdx = subtract(&bx, &dx);
        let bdy = subtract(&by, &dy);
        let cdx = subtract(&cx, &dx);
        let cdy = subtract(&cy, &dy);
        let alift = add(&multiply(&adx, &adx), &multiply(&ady, &ady));
        let blift = add(&multiply(&bdx, &bdx), &multiply(&bdy, &bdy));
        let clift = add(&multiply(&cdx, &cdx), &multiply(&cdy, &cdy));
        let bcdet = subtract(&multiply(&bdx, &cdy), &multiply(&cdx, &bdy));
        let cadet = subtract(&multiply(&cdx, &ady), &multiply(&adx, &cdy));
        let abdet = subtract(&multiply(&adx, &bdy), &multiply(&bdx, &ady));
        sign_of(compare_zero(&add(
            &add(&multiply(&alift, &bcdet), &multiply(&blift, &cadet)),
            &multiply(&clift, &abdet),
        )))
    }

    fn opposite(sign: Sign) -> Sign {
        flip(sign)
    }

    fn check_orient2d(expected: Sign, a: (f64, f64), b: (f64, f64), c: (f64, f64), message: &str) {
        let actual = orient2d(a.0, a.1, b.0, b.1, c.0, c.1);
        assert!(actual == expected, "{message}");
        let reversed = orient2d(b.0, b.1, a.0, a.1, c.0, c.1);
        assert!(reversed == opposite(expected), "{message} (antisymmetry)");
    }

    fn check_orient3d(
        expected: Sign,
        a: (f64, f64, f64),
        b: (f64, f64, f64),
        c: (f64, f64, f64),
        d: (f64, f64, f64),
        message: &str,
    ) {
        let actual = orient3d(a.0, a.1, a.2, b.0, b.1, b.2, c.0, c.1, c.2, d.0, d.1, d.2);
        assert!(actual == expected, "{message}");
        let reversed = orient3d(b.0, b.1, b.2, a.0, a.1, a.2, c.0, c.1, c.2, d.0, d.1, d.2);
        assert!(reversed == opposite(expected), "{message} (antisymmetry)");
    }

    #[allow(clippy::too_many_arguments)]
    fn check_segment_segment(
        expected: SegmentSegment,
        x: &[f64],
        y: &[f64],
        z: &[f64],
        left_a: i64,
        left_b: i64,
        right_a: i64,
        right_b: i64,
        message: &str,
    ) {
        let actual = segment_segment_packed(x, y, z, left_a, left_b, right_a, right_b);
        assert!(actual == expected, "{message}");
        let reversed = segment_segment_packed(x, y, z, right_b, right_a, left_b, left_a);
        assert!(reversed == expected, "{message} (operand/endpoint reversal)");
    }

    #[allow(clippy::too_many_arguments)]
    fn triangle_event_count(
        x: &[f64],
        y: &[f64],
        z: &[f64],
        la: i64,
        lb: i64,
        lc: i64,
        ra: i64,
        rb: i64,
        rc: i64,
    ) -> (i64, [i64; 4]) {
        let mut output = [-1i64; 4];
        let count = view::triangle_triangle_features_into(x, y, z, la, lb, lc, ra, rb, rc, &mut output);
        (count, output)
    }

    fn sign_of_int64(value: i64) -> Sign {
        if value < 0 {
            Sign::Negative
        } else if value > 0 {
            Sign::Positive
        } else {
            Sign::Zero
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn planar_segment_oracle(
        ax: i64,
        ay: i64,
        bx: i64,
        by: i64,
        cx: i64,
        cy: i64,
        dx: i64,
        dy: i64,
    ) -> SegmentSegment {
        let orient = |ax: i64, ay: i64, bx: i64, by: i64, cx: i64, cy: i64| -> i64 {
            (ax - cx) * (by - cy) - (ay - cy) * (bx - cx)
        };
        if (ax == bx && ay == by) || (cx == dx && cy == dy) {
            return SegmentSegment::Degenerate;
        }
        let first = orient(ax, ay, bx, by, cx, cy);
        let second = orient(ax, ay, bx, by, dx, dy);
        let third = orient(cx, cy, dx, dy, ax, ay);
        let fourth = orient(cx, cy, dx, dy, bx, by);
        if first == 0 && second == 0 && third == 0 && fourth == 0 {
            let (a, b, c, d) = if ax != bx || cx != dx { (ax, bx, cx, dx) } else { (ay, by, cy, dy) };
            let lower = a.min(b).max(c.min(d));
            let upper = a.max(b).min(c.max(d));
            if lower > upper {
                SegmentSegment::Disjoint
            } else if lower == upper {
                SegmentSegment::Point
            } else {
                SegmentSegment::Overlap
            }
        } else {
            let opposite = |left: i64, right: i64| (left < 0 && right > 0) || (left > 0 && right < 0);
            let on_segment = |ax: i64, ay: i64, bx: i64, by: i64, px: i64, py: i64| {
                px >= ax.min(bx) && px <= ax.max(bx) && py >= ay.min(by) && py <= ay.max(by)
            };
            if (opposite(first, second) && opposite(third, fourth))
                || (first == 0 && on_segment(ax, ay, bx, by, cx, cy))
                || (second == 0 && on_segment(ax, ay, bx, by, dx, dy))
                || (third == 0 && on_segment(cx, cy, dx, dy, ax, ay))
                || (fourth == 0 && on_segment(cx, cy, dx, dy, bx, by))
            {
                SegmentSegment::Point
            } else {
                SegmentSegment::Disjoint
            }
        }
    }

    fn deterministic_coordinate(index: i64, salt: i64, extent: i64) -> i64 {
        let mixed = (index * 1_103_515_245) + (salt * 97_531) + 12_345;
        (mixed % ((2 * extent) + 1)) - extent
    }

    fn scaled_coordinate(index: i64, salt: i64) -> f64 {
        let exponents: [i64; 11] = [-1074, -1022, -700, -300, -53, -1, 0, 17, 300, 700, 1023];
        let mixed = (index * 1_664_525) + (salt * 1_013_904_223) + 97;
        let exponent = exponents[((mixed & i64::MAX) % exponents.len() as i64) as usize];
        let signed = ((mixed >> 5) % 2047) - 1023;
        let mantissa = if signed == 0 { 1. } else { signed as f64 / 1024. };
        dyadic::ldexp(mantissa, exponent)
    }

    #[test]
    fn ordinary_and_cancelled_cases() {
        check_orient2d(Sign::Positive, (0., 0.), (1., 0.), (0., 1.), "ordinary orient2d");
        check_orient2d(Sign::Zero, (0., 0.), (1., 1.), (2., 2.), "collinear orient2d");

        // Every input is exactly representable, but the two rounded products are
        // indistinguishable at the filter's certified error scale. The exact
        // determinant is -1.
        let n = dyadic::ldexp(1., 52);
        check_orient2d(Sign::Negative, (0., 0.), (n, n - 1.), (n - 1., n - 2.), "cancelled orient2d exact fallback");

        let smallest = f64::from_bits(1);
        let minimum_normal = dyadic::ldexp(1., -1022);
        assert!(minimum_normal * smallest == 0., "underflow fixture did not underflow");
        check_orient2d(
            Sign::Positive,
            (0., 0.),
            (minimum_normal, 0.),
            (0., smallest),
            "underflow orient2d exact fallback",
        );
        check_orient2d(
            Sign::Positive,
            (0., 0.),
            (f64::MAX, 0.),
            (0., f64::MAX),
            "overflow orient2d exact fallback",
        );
        let packed_x = [0., n, n - 1.];
        let packed_y = [0., n - 1., n - 2.];
        assert!(orient2d_packed(&packed_x, &packed_y, 0, 1, 2) == Sign::Negative, "packed orient2d exact fallback");

        check_orient3d(
            Sign::Positive,
            (1., 0., 0.),
            (0., 1., 0.),
            (0., 0., 1.),
            (0., 0., 0.),
            "ordinary orient3d",
        );
        check_orient3d(
            Sign::Zero,
            (0., 0., 0.),
            (1., 0., 0.),
            (0., 1., 0.),
            (1., 1., 0.),
            "coplanar orient3d",
        );
        check_orient3d(
            Sign::Negative,
            (n, n - 1., 0.),
            (n - 1., n - 2., 0.),
            (0., 0., 1.),
            (0., 0., 0.),
            "cancelled orient3d exact fallback",
        );
        check_orient3d(
            Sign::Positive,
            (minimum_normal, 0., 0.),
            (0., smallest, 0.),
            (0., 0., smallest),
            (0., 0., 0.),
            "underflow orient3d exact fallback",
        );
        let packed_z = [0., 0., 1., 0.];
        assert!(
            orient3d_packed(&[n, n - 1., 0., 0.], &[n - 1., n - 2., 0., 0.], &packed_z, 0, 1, 2, 3)
                == Sign::Negative,
            "packed orient3d exact fallback"
        );

        let incircle_x = [0., 2., 0., 0.5];
        let incircle_y = [0., 0., 2., 0.5];
        assert!(incircle_packed(&incircle_x, &incircle_y, 0, 1, 2, 3) == Sign::Positive, "packed incircle");
    }

    #[test]
    fn segment_and_triangle_cases() {
        let sx = [0., 2., 1., 1., 1., 1., 0., 2., 3., 4., 2., 3., 0.];
        let sy = [0., 0., -1., 1., -1., 1., 0., 0., 0., 0., 0., 0., 0.];
        let sz = [0., 0., 0., 0., 1., 1., 0., 0., 0., 0., 0., 0., 0.];
        use SegmentSegment as S;
        check_segment_segment(S::Point, &sx, &sy, &sz, 0, 1, 2, 3, "coplanar transverse segment contact");
        check_segment_segment(S::Disjoint, &sx, &sy, &sz, 0, 1, 4, 5, "skew segment rejection");
        check_segment_segment(S::Overlap, &sx, &sy, &sz, 0, 1, 6, 7, "collinear segment overlap");
        check_segment_segment(S::Disjoint, &sx, &sy, &sz, 0, 1, 8, 9, "collinear segment separation");
        check_segment_segment(S::Point, &sx, &sy, &sz, 0, 1, 10, 11, "collinear endpoint contact");
        check_segment_segment(S::Degenerate, &sx, &sy, &sz, 0, 12, 2, 3, "zero-length segment classification");
        let tiny = f64::from_bits(1);
        check_segment_segment(
            S::Point,
            &[0., tiny, 0., tiny],
            &[0., tiny, tiny, 0.],
            &[0., 0., 0., 0.],
            0,
            1,
            2,
            3,
            "subnormal exact segment contact",
        );
        check_segment_segment(
            S::Point,
            &[0., 0., 0., 0.],
            &[0., 2., 1., 1.],
            &[0., 0., -1., 1.],
            0,
            1,
            2,
            3,
            "YZ-projected segment contact",
        );
        check_segment_segment(
            S::Point,
            &[0., 0., -1., 1.],
            &[0., 0., 0., 0.],
            &[0., 2., 1., 1.],
            0,
            1,
            2,
            3,
            "ZX-projected segment contact",
        );
        let largest = f64::MAX;
        check_segment_segment(
            S::Overlap,
            &[-largest, largest, 0., 1.],
            &[0., 0., 0., 0.],
            &[0., 0., 0., 0.],
            0,
            1,
            2,
            3,
            "overflow-range collinear overlap",
        );

        let tx = [0., 2., 0., 0.5, 0.5, 0.5, 4., 5., 4., 0., 1., 2.];
        let ty = [0., 0., 2., -0.5, 1.5, 1.5, 4., 4., 5., 0., 0., 0.];
        let tz = [0., 0., 0., -1., 1., -1., 1., 2., 1., 0., 0., 0.];
        let (count, features) = triangle_event_count(&tx, &ty, &tz, 0, 1, 2, 3, 4, 5);
        assert!(count == 2 && features[0] <= features[2], "triangle/triangle segment features");
        let (count, _) = triangle_event_count(&tx, &ty, &tz, 0, 1, 2, 6, 7, 8);
        assert!(count == 0, "triangle/triangle disjoint result");
        let (count, _) = triangle_event_count(&tx, &ty, &tz, 0, 1, 2, 0, 1, 2);
        assert!(count == -1, "triangle/triangle coplanar result");
        let coplanar_contact = |la, lb, lc, ra, rb, rc| {
            view::coplanar_triangles_contact_packed(&tx, &ty, &tz, la, lb, lc, ra, rb, rc)
        };
        assert!(coplanar_contact(0, 1, 2, 0, 1, 2), "identical coplanar triangles do not contact");
        assert!(!coplanar_contact(0, 1, 2, 6, 7, 8), "disjoint coplanar triangles contact");
        let cx = [0., 4., 0., 1., 2., 1., 4., 5., 4., 4., 5., 4.];
        let cy = [0., 0., 4., 1., 1., 2., 0., 0., 1., 0., 0., -1.];
        let cz = [0.; 12];
        let contact = |la, lb, lc, ra, rb, rc| {
            view::coplanar_triangles_contact_packed(&cx, &cy, &cz, la, lb, lc, ra, rb, rc)
        };
        assert!(contact(0, 1, 2, 3, 4, 5), "contained coplanar triangle does not contact");
        assert!(contact(0, 1, 2, 6, 7, 8), "edge-touching coplanar triangle does not contact");
        assert!(contact(0, 1, 2, 9, 10, 11), "vertex-touching coplanar triangle does not contact");
        let (count, _) = triangle_event_count(&tx, &ty, &tz, 0, 9, 10, 3, 4, 5);
        assert!(count == -2, "triangle/triangle degenerate result");
    }

    #[test]
    #[should_panic(expected = "orient2d")]
    fn orient2d_rejects_nan() {
        orient2d(f64::NAN, 0., 0., 0., 0., 0.);
    }

    #[test]
    #[should_panic(expected = "orient3d")]
    fn orient3d_rejects_infinity() {
        orient3d(f64::INFINITY, 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.);
    }

    #[test]
    #[should_panic(expected = "segment_segment_packed")]
    fn segment_segment_rejects_nan() {
        segment_segment_packed(&[f64::NAN, 0., 0., 1.], &[0., 0., 1., 0.], &[0., 0., 0., 0.], 0, 1, 2, 3);
    }

    #[test]
    #[should_panic(expected = "invalid polygon range")]
    fn polygon_area_rejects_short_range() {
        polygon_area_sign_packed(&[0., 1.], &[0., 1.], &[0, 1], 0, 2);
    }

    #[test]
    fn orient2d_matches_int64_oracle() {
        for index in 0..50_000i64 {
            let coordinate = |salt| deterministic_coordinate(index, salt, 1_000_000);
            let (ax, ay, bx, by, cx, cy) =
                (coordinate(0), coordinate(1), coordinate(2), coordinate(3), coordinate(4), coordinate(5));
            let determinant = (ax - cx) * (by - cy) - (ay - cy) * (bx - cx);
            let actual = orient2d(ax as f64, ay as f64, bx as f64, by as f64, cx as f64, cy as f64);
            assert!(actual == sign_of_int64(determinant), "orient2d disagrees with the int64 oracle");
        }
    }

    #[test]
    fn segment_segment_matches_planar_oracle() {
        for index in 0..20_000i64 {
            let coordinate = |salt| deterministic_coordinate(index, salt, 10_000);
            let v: Vec<i64> = (0..8).map(coordinate).collect();
            let expected = planar_segment_oracle(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7]);
            let actual = segment_segment_packed(
                &[v[0] as f64, v[2] as f64, v[4] as f64, v[6] as f64],
                &[v[1] as f64, v[3] as f64, v[5] as f64, v[7] as f64],
                &[0., 0., 0., 0.],
                0,
                1,
                2,
                3,
            );
            assert!(actual == expected, "segment/segment disagrees with the planar int64 oracle");
        }
    }

    #[test]
    fn orient3d_matches_int64_oracle() {
        for index in 0..20_000i64 {
            let c = |salt| deterministic_coordinate(index, salt, 10_000);
            let v: Vec<i64> = (0..12).map(c).collect();
            let adx = v[0] - v[9];
            let ady = v[1] - v[10];
            let adz = v[2] - v[11];
            let bdx = v[3] - v[9];
            let bdy = v[4] - v[10];
            let bdz = v[5] - v[11];
            let cdx = v[6] - v[9];
            let cdy = v[7] - v[10];
            let cdz = v[8] - v[11];
            let determinant = adx * (bdy * cdz - bdz * cdy)
                + ady * (bdz * cdx - bdx * cdz)
                + adz * (bdx * cdy - bdy * cdx);
            let f: Vec<f64> = v.iter().map(|value| *value as f64).collect();
            let actual = orient3d(f[0], f[1], f[2], f[3], f[4], f[5], f[6], f[7], f[8], f[9], f[10], f[11]);
            assert!(actual == sign_of_int64(determinant), "orient3d disagrees with the int64 oracle");
        }
    }

    // Force both exact engines over the complete binary64 exponent range. The packed arena
    // resets between calls and must remain identical to the immutable reference.
    #[test]
    fn exact_engines_agree_over_full_exponent_range() {
        for index in 0..5_000i64 {
            let c = |salt| scaled_coordinate(index, salt);
            let (ax, ay, bx, by, cx, cy) = (c(0), c(1), c(2), c(3), c(4), c(5));
            let arena = orient2d(ax, ay, bx, by, cx, cy);
            let reference = exact_orient2d_reference(ax, ay, bx, by, cx, cy);
            assert!(arena == reference, "packed exact orient2d differs from reference");
        }
        for index in 0..2_000i64 {
            let c = |salt| scaled_coordinate(index, salt);
            let v: Vec<f64> = (0..12).map(c).collect();
            let arena = orient3d(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11]);
            let reference = exact_orient3d_reference(
                v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11],
            );
            assert!(arena == reference, "packed exact orient3d differs from reference");
        }
        for index in 0..2_000i64 {
            let c = |salt| scaled_coordinate(index, salt);
            let v: Vec<f64> = (0..8).map(c).collect();
            let arena = incircle_packed(
                &[v[0], v[2], v[4], v[6]],
                &[v[1], v[3], v[5], v[7]],
                0,
                1,
                2,
                3,
            );
            let reference = exact_incircle_reference(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7]);
            assert!(arena == reference, "packed exact incircle differs from reference");
        }
    }

    fn polygon_sign(x: &[f64], y: &[f64]) -> Sign {
        let points: Vec<i64> = (0..x.len() as i64).collect();
        polygon_area_sign_packed(x, y, &points, 0, x.len() as i64)
    }

    #[test]
    fn polygon_area_cases() {
        assert!(
            polygon_sign(&[0., 1., 1., 0.], &[0., 0., 1., 1.]) == Sign::Positive,
            "polygon area ordinary sign"
        );
        assert!(
            polygon_sign(&[0., 0., 1., 1.], &[0., 1., 1., 0.]) == Sign::Negative,
            "polygon area reversed sign"
        );
        let maximum = f64::MAX;
        assert!(
            polygon_sign(&[-maximum, maximum, maximum, -maximum], &[-maximum, -maximum, maximum, maximum])
                == Sign::Positive,
            "polygon area overflow fallback"
        );
        let tiny = f64::from_bits(1);
        assert!(
            polygon_sign(&[0., tiny, tiny, 0.], &[0., 0., tiny, tiny]) == Sign::Positive,
            "polygon area underflow fallback"
        );
        assert!(
            polygon_sign(&[0., 1., 0., 1.], &[0., 1., 1., 0.]) == Sign::Zero,
            "polygon area exact cancellation"
        );
        for index in 0..5_000i64 {
            let count = 7usize;
            let xi: Vec<i64> =
                (0..count).map(|local| deterministic_coordinate(index, local as i64 * 2, 1_000)).collect();
            let yi: Vec<i64> = (0..count)
                .map(|local| deterministic_coordinate(index, local as i64 * 2 + 1, 1_000))
                .collect();
            let mut area = 0i64;
            for local in 0..count {
                let next = if local + 1 == count { 0 } else { local + 1 };
                area += xi[local] * yi[next] - xi[next] * yi[local];
            }
            let xf: Vec<f64> = xi.iter().map(|v| *v as f64).collect();
            let yf: Vec<f64> = yi.iter().map(|v| *v as f64).collect();
            assert!(polygon_sign(&xf, &yf) == sign_of_int64(area), "polygon area disagrees with int64 oracle");
        }
    }

    // The filtered path and exact fallback are pure and domain-independent.
    #[test]
    fn signs_are_domain_independent() {
        use std::sync::Mutex;
        let sample = |domains: i64| -> Vec<Sign> {
            let out = Mutex::new(vec![Sign::Zero; 20_000]);
            parallel::run_unit(Some(domains), &|| {
                parallel::for_(256, 0, 19_999, &|index| {
                    let offset = index as f64 * 0.125;
                    let sign = orient2d(offset, 0., offset + 1., 0., offset, 1.);
                    out.lock().unwrap()[index as usize] = sign;
                    Ok(())
                })
            })
            .unwrap();
            out.into_inner().unwrap()
        };
        assert!(sample(1) == sample(4), "predicate signs differ across domains");
    }
}
