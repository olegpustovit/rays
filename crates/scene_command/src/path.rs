use rays_math::float::{max as float_max, compare as compare_float};
use std::cell::RefCell;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Point {
    pub x: f64,
    pub y: f64,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Command {
    MoveTo(Point),
    LineTo(Point),
    QuadraticTo(Point, Point),
    CubicTo(Point, Point, Point),
    Close,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Path(Vec<Command>);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FillRule {
    EvenOdd,
    NonZero,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Mesh {
    pub vertices: Vec<Point>,
    pub indices: Vec<i64>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cap {
    Butt,
    Square,
    Round,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Join {
    Miter,
    Bevel,
    Round,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    EmptyPath,
    InvalidTolerance(f64),
    InvalidWidth(f64),
    InvalidMiterLimit(f64),
    NonFinitePoint(Point),
    MissingCurrentPoint,
    OpenContour,
    ComplexityLimit,
    InvalidTopology,
}

const MAX_POINTS: usize = 1_048_576;

pub fn of_commands(commands: &[Command]) -> Path {
    Path(commands.to_vec())
}

impl Path {
    /// Path commands remain in authored order.
    pub fn commands(&self) -> &[Command] {
        &self.0
    }
}

fn finite(p: Point) -> bool {
    p.x.is_finite() && p.y.is_finite()
}

fn midpoint(a: Point, b: Point) -> Point {
    Point { x: (a.x + b.x) * 0.5, y: (a.y + b.y) * 0.5 }
}

fn distance_to_line_squared(p: Point, a: Point, b: Point) -> f64 {
    let dx = b.x - a.x;
    let dy = b.y - a.y;
    let denominator = dx * dx + dy * dy;
    if denominator == 0.0 {
        let x = p.x - a.x;
        let y = p.y - a.y;
        x * x + y * y
    } else {
        let cross = (p.x - a.x) * dy - (p.y - a.y) * dx;
        cross * cross / denominator
    }
}

fn add_point(points: &mut Vec<Point>, point: Point) -> Result<(), Error> {
    if points.len() >= MAX_POINTS {
        return Err(Error::ComplexityLimit);
    }
    points.push(point);
    Ok(())
}

fn quadratic(
    output: &mut Vec<Point>,
    depth: u32,
    a: Point,
    control: Point,
    b: Point,
    tolerance_squared: f64,
) -> Result<(), Error> {
    if depth == 24 || distance_to_line_squared(control, a, b) <= tolerance_squared {
        return add_point(output, b);
    }
    let ab = midpoint(a, control);
    let bc = midpoint(control, b);
    let center = midpoint(ab, bc);
    quadratic(output, depth + 1, a, ab, center, tolerance_squared)?;
    quadratic(output, depth + 1, center, bc, b, tolerance_squared)
}

fn cubic(
    output: &mut Vec<Point>,
    depth: u32,
    a: Point,
    c1: Point,
    c2: Point,
    b: Point,
    tolerance_squared: f64,
) -> Result<(), Error> {
    if depth == 24
        || float_max(distance_to_line_squared(c1, a, b), distance_to_line_squared(c2, a, b))
            <= tolerance_squared
    {
        return add_point(output, b);
    }
    let a1 = midpoint(a, c1);
    let c12 = midpoint(c1, c2);
    let c2b = midpoint(c2, b);
    let left = midpoint(a1, c12);
    let right = midpoint(c12, c2b);
    let center = midpoint(left, right);
    cubic(output, depth + 1, a, a1, left, center, tolerance_squared)?;
    cubic(output, depth + 1, center, right, c2b, b, tolerance_squared)
}

/// Persistent working sets for flatten/tessellate/stroke. `clear()` keeps capacity.
#[derive(Default)]
pub struct Scratch {
    contour_pool: Vec<Vec<Point>>,
    contours: Vec<Vec<Point>>,
    points: Vec<Point>,
    ys: Vec<f64>,
    edges: Vec<Crossing>,
    vertices: Vec<Point>,
    pub indices: Vec<i64>,
    pub flat: Vec<f64>,
}

impl Scratch {
    fn recycle_contours(&mut self) {
        for mut c in self.contours.drain(..) {
            c.clear();
            self.contour_pool.push(c);
        }
    }

    fn take_contour_buf(&mut self) -> Vec<Point> {
        let mut buf = self.contour_pool.pop().unwrap_or_default();
        buf.clear();
        buf
    }

    fn finish_contour(&mut self) {
        if self.points.is_empty() {
            return;
        }
        let mut done = self.take_contour_buf();
        std::mem::swap(&mut done, &mut self.points);
        self.contours.push(done);
    }
}

thread_local! {
    static SCRATCH: RefCell<Scratch> = RefCell::new(Scratch::default());
}

/// Borrow the process scratch (clear + reuse). Nested tessellation is unsupported.
pub fn with_scratch<R>(f: impl FnOnce(&mut Scratch) -> R) -> R {
    SCRATCH.with(|s| f(&mut s.borrow_mut()))
}

fn flatten_into(scratch: &mut Scratch, tolerance: f64, path: &Path) -> Result<(), Error> {
    if !tolerance.is_finite() || tolerance <= 0.0 {
        return Err(Error::InvalidTolerance(tolerance));
    }
    if path.0.is_empty() {
        return Err(Error::EmptyPath);
    }
    let tolerance_squared = tolerance * tolerance;
    scratch.recycle_contours();
    scratch.points.clear();
    let mut current: Option<Point> = None;
    let mut start: Option<Point> = None;
    for &command in &path.0 {
        match command {
            Command::MoveTo(p) => {
                if !finite(p) { return Err(Error::NonFinitePoint(p)); }
                if current.is_some() {
                    scratch.finish_contour();
                }
                add_point(&mut scratch.points, p)?;
                current = Some(p);
                start = Some(p);
            }
            Command::LineTo(p) => {
                if !finite(p) { return Err(Error::NonFinitePoint(p)); }
                if current.is_none() { return Err(Error::MissingCurrentPoint); }
                add_point(&mut scratch.points, p)?;
                current = Some(p);
            }
            Command::QuadraticTo(control, p) => {
                if !finite(control) { return Err(Error::NonFinitePoint(control)); }
                if !finite(p) { return Err(Error::NonFinitePoint(p)); }
                let Some(a) = current else { return Err(Error::MissingCurrentPoint) };
                quadratic(&mut scratch.points, 0, a, control, p, tolerance_squared)?;
                current = Some(p);
            }
            Command::CubicTo(c1, c2, p) => {
                if !finite(c1) { return Err(Error::NonFinitePoint(c1)); }
                if !finite(c2) { return Err(Error::NonFinitePoint(c2)); }
                if !finite(p) { return Err(Error::NonFinitePoint(p)); }
                let Some(a) = current else { return Err(Error::MissingCurrentPoint) };
                cubic(&mut scratch.points, 0, a, c1, c2, p, tolerance_squared)?;
                current = Some(p);
            }
            Command::Close => {
                match (current, start) {
                    (Some(last), Some(first)) if last != first => add_point(&mut scratch.points, first)?,
                    (Some(_), Some(_)) => (),
                    _ => return Err(Error::MissingCurrentPoint),
                }
                scratch.finish_contour();
                current = None;
                start = None;
            }
        }
    }
    if current.is_some() {
        scratch.finish_contour();
    }
    Ok(())
}

#[derive(Clone, Copy)]
struct Crossing {
    x0: f64,
    x1: f64,
    winding: i32,
    source: u32,
}

fn add_quad(vertices: &mut Vec<Point>, indices: &mut Vec<i64>, left: Crossing, right: Crossing, y0: f64, y1: f64) -> Result<(), Error> {
    if vertices.len().checked_add(4).filter(|n| *n <= MAX_POINTS).is_none() {
        return Err(Error::ComplexityLimit);
    }
    let base = vertices.len() as i64;
    vertices.extend_from_slice(&[
        Point { x: left.x0, y: y0 }, Point { x: right.x0, y: y0 },
        Point { x: right.x1, y: y1 }, Point { x: left.x1, y: y1 },
    ]);
    indices.extend_from_slice(&[base + 3, base + 2, base, base + 2, base + 1, base]);
    Ok(())
}

fn tessellate_into(scratch: &mut Scratch, tolerance: f64, fill_rule: FillRule, path: &Path) -> Result<(), Error> {
    let mut open_contour = false;
    let mut active = false;
    for command in &path.0 {
        match command {
            Command::MoveTo(_) => {
                if active { open_contour = true; }
                active = true;
            }
            Command::Close => active = false,
            _ => (),
        }
    }
    if active || open_contour { return Err(Error::OpenContour); }
    flatten_into(scratch, tolerance, path)?;
    scratch.ys.clear();
    for contour in &scratch.contours {
        for p in contour {
            scratch.ys.push(p.y);
        }
    }
    scratch.ys.sort_by(|a, b| compare_float(*a, *b));
    scratch.ys.dedup_by(|a, b| *a == *b);
    scratch.vertices.clear();
    scratch.indices.clear();
    for band in 0..scratch.ys.len().saturating_sub(1) {
        let y0 = scratch.ys[band];
        let y1 = scratch.ys[band + 1];
        if y1 <= y0 { continue; }
        let middle = (y0 + y1) * 0.5;
        scratch.edges.clear();
        for contour in &scratch.contours {
            for pair in contour.windows(2) {
                let a = pair[0];
                let b = pair[1];
                let low = a.y.min(b.y);
                let high = a.y.max(b.y);
                if a.y != b.y && middle >= low && middle < high {
                    let at = |y| a.x + ((y - a.y) * (b.x - a.x) / (b.y - a.y));
                    let source = u32::try_from(scratch.edges.len()).map_err(|_| Error::ComplexityLimit)?;
                    scratch.edges.push(Crossing { x0: at(y0), x1: at(y1), winding: if b.y > a.y { 1 } else { -1 }, source });
                }
            }
        }
        scratch.edges.sort_by(|a, b| compare_float((a.x0 + a.x1) * 0.5, (b.x0 + b.x1) * 0.5).then(a.source.cmp(&b.source)));
        let mut inside = false;
        let mut winding = 0;
        let mut left = None;
        for &edge in &scratch.edges {
            let was_inside = inside;
            match fill_rule {
                FillRule::EvenOdd => inside = !inside,
                FillRule::NonZero => { winding += edge.winding; inside = winding != 0; }
            }
            if !was_inside && inside { left = Some(edge); }
            else if was_inside && !inside {
                let Some(left_edge) = left else { return Err(Error::InvalidTopology); };
                add_quad(&mut scratch.vertices, &mut scratch.indices, left_edge, edge, y0, y1)?;
            }
        }
    }
    Ok(())
}

pub fn tessellate(tolerance: f64, fill_rule: FillRule, path: &Path) -> Result<Mesh, Error> {
    with_scratch(|scratch| {
        tessellate_into(scratch, tolerance, fill_rule, path)?;
        Ok(Mesh { vertices: scratch.vertices.clone(), indices: scratch.indices.clone() })
    })
}

/// Fill into scratch flat/index buffers (no Mesh alloc). Caller reads `scratch.flat` / `scratch.indices`.
pub fn tessellate_scratch(scratch: &mut Scratch, tolerance: f64, fill_rule: FillRule, path: &Path) -> Result<(), Error> {
    tessellate_into(scratch, tolerance, fill_rule, path)?;
    scratch.flat.clear();
    scratch.flat.reserve(scratch.vertices.len() * 2);
    for p in &scratch.vertices {
        scratch.flat.push(p.x);
        scratch.flat.push(p.y);
    }
    Ok(())
}

fn stroke_into(
    scratch: &mut Scratch,
    tolerance: f64,
    width: f64,
    cap: Cap,
    join: Join,
    miter_limit: f64,
    path: &Path,
) -> Result<(), Error> {
    if !width.is_finite() || width <= 0.0 { return Err(Error::InvalidWidth(width)); }
    if !miter_limit.is_finite() || miter_limit < 1.0 { return Err(Error::InvalidMiterLimit(miter_limit)); }
    flatten_into(scratch, tolerance, path)?;
    scratch.vertices.clear();
    scratch.indices.clear();
    let half = width * 0.5;
    let unit = |a: Point, b: Point| {
        let dx = b.x - a.x;
        let dy = b.y - a.y;
        let length = (dx * dx + dy * dy).sqrt();
        if length == 0.0 { None } else { Some((dx / length, dy / length)) }
    };
    let offset = |p: Point, nx: f64, ny: f64, scale: f64| Point { x: p.x + nx * scale, y: p.y + ny * scale };
    let add_triangle = |vertices: &mut Vec<Point>, indices: &mut Vec<i64>, a: Point, b: Point, c: Point| -> Result<(), Error> {
        if vertices.len().checked_add(3).filter(|n| *n <= MAX_POINTS).is_none() { return Err(Error::ComplexityLimit); }
        let base = vertices.len() as i64;
        vertices.extend_from_slice(&[a, b, c]);
        indices.extend_from_slice(&[base + 2, base + 1, base]);
        Ok(())
    };
    let fan = |vertices: &mut Vec<Point>, indices: &mut Vec<i64>, center: Point, radius: f64, start_angle: f64, delta: f64| -> Result<(), Error> {
        let denominator = if tolerance >= radius { std::f64::consts::PI } else { 2.0 * rays_math::libm::acos((1.0 - (tolerance / radius)).max(-1.0)) };
        let steps = ((delta.abs() / denominator).ceil() as usize).clamp(1, 64);
        let mut previous = offset(center, rays_math::libm::cos(start_angle), rays_math::libm::sin(start_angle), radius);
        for i in 1..=steps {
            let angle = start_angle + delta * i as f64 / steps as f64;
            let next = offset(center, rays_math::libm::cos(angle), rays_math::libm::sin(angle), radius);
            add_triangle(vertices, indices, center, previous, next)?;
            previous = next;
        }
        Ok(())
    };
    // Contours are in scratch.contours; stroke reads them without borrowing scratch mutably for helpers.
    let contour_count = scratch.contours.len();
    for ci in 0..contour_count {
        let count = scratch.contours[ci].len();
        let closed = count > 2 && scratch.contours[ci][0] == scratch.contours[ci][count - 1];
        for pair in scratch.contours[ci].windows(2) {
            if let Some((dx, dy)) = unit(pair[0], pair[1]) {
                let (nx, ny) = (-dy, dx);
                let (a, b) = (pair[0], pair[1]);
                let a0 = offset(a, nx, ny, half);
                let a1 = offset(a, nx, ny, -half);
                let b0 = offset(b, nx, ny, half);
                let b1 = offset(b, nx, ny, -half);
                if scratch.vertices.len().checked_add(4).filter(|n| *n <= MAX_POINTS).is_none() { return Err(Error::ComplexityLimit); }
                let base = scratch.vertices.len() as i64;
                scratch.vertices.extend_from_slice(&[a0, a1, b1, b0]);
                scratch.indices.extend_from_slice(&[base + 2, base + 1, base, base + 3, base + 2, base]);
            }
        }
        let add_join = |vertices: &mut Vec<Point>, indices: &mut Vec<i64>, a: Point, b: Point, c: Point| -> Result<(), Error> {
            let (Some((dx0, dy0)), Some((dx1, dy1))) = (unit(a, b), unit(b, c)) else { return Ok(()) };
            let cross = dx0 * dy1 - dy0 * dx1;
            if !(cross.abs() > 1e-15) { return Ok(()); }
            let side = if cross > 0.0 { 1.0 } else { -1.0 };
            let n0x = -dy0 * side;
            let n0y = dx0 * side;
            let n1x = -dy1 * side;
            let n1y = dx1 * side;
            let p0 = offset(b, n0x, n0y, half);
            let p1 = offset(b, n1x, n1y, half);
            match join {
                Join::Bevel => add_triangle(vertices, indices, b, p0, p1),
                Join::Round => {
                    let a0 = rays_math::libm::atan2(n0y, n0x);
                    let a1 = rays_math::libm::atan2(n1y, n1x);
                    let raw = a1 - a0;
                    let delta = if side > 0.0 && raw < 0.0 { raw + 2.0 * std::f64::consts::PI } else if side < 0.0 && raw > 0.0 { raw - 2.0 * std::f64::consts::PI } else { raw };
                    fan(vertices, indices, b, half, a0, delta)
                }
                Join::Miter => {
                    let sx = n0x + n1x;
                    let sy = n0y + n1y;
                    let length = (sx * sx + sy * sy).sqrt();
                    let mx = sx / length;
                    let my = sy / length;
                    let scale = half / (mx * n1x + my * n1y);
                    if scale.abs() > half * miter_limit { add_triangle(vertices, indices, b, p0, p1) }
                    else { add_triangle(vertices, indices, p0, offset(b, mx, my, scale), p1) }
                }
            }
        };
        let add_cap = |vertices: &mut Vec<Point>, indices: &mut Vec<i64>, point: Point, direction: (f64, f64), at_start: bool| -> Result<(), Error> {
            let (dx, dy) = direction;
            let (nx, ny) = (-dy, dx);
            match cap {
                Cap::Butt => Ok(()),
                Cap::Square => {
                    let sign = if at_start { -1.0 } else { 1.0 };
                    let inner0 = offset(point, nx, ny, half);
                    let inner1 = offset(point, nx, ny, -half);
                    let end_point = offset(point, dx, dy, sign * half);
                    let outer0 = offset(end_point, nx, ny, half);
                    let outer1 = offset(end_point, nx, ny, -half);
                    add_triangle(vertices, indices, inner0, inner1, outer1)?;
                    add_triangle(vertices, indices, inner0, outer1, outer0)
                }
                Cap::Round => {
                    let angle = rays_math::libm::atan2(ny, nx);
                    fan(vertices, indices, point, half, angle, if at_start { std::f64::consts::PI } else { -std::f64::consts::PI })
                }
            }
        };
        if closed {
            for i in 1..count.saturating_sub(1) {
                let (a, b, c) = (scratch.contours[ci][i - 1], scratch.contours[ci][i], scratch.contours[ci][i + 1]);
                add_join(&mut scratch.vertices, &mut scratch.indices, a, b, c)?;
            }
            let (a, b, c) = (scratch.contours[ci][count - 2], scratch.contours[ci][0], scratch.contours[ci][1]);
            add_join(&mut scratch.vertices, &mut scratch.indices, a, b, c)?;
        } else if count >= 2 {
            for i in 1..count.saturating_sub(1) {
                let (a, b, c) = (scratch.contours[ci][i - 1], scratch.contours[ci][i], scratch.contours[ci][i + 1]);
                add_join(&mut scratch.vertices, &mut scratch.indices, a, b, c)?;
            }
            if let Some(direction) = unit(scratch.contours[ci][0], scratch.contours[ci][1]) {
                add_cap(&mut scratch.vertices, &mut scratch.indices, scratch.contours[ci][0], direction, true)?;
            }
            if let Some(direction) = unit(scratch.contours[ci][count - 2], scratch.contours[ci][count - 1]) {
                add_cap(&mut scratch.vertices, &mut scratch.indices, scratch.contours[ci][count - 1], direction, false)?;
            }
        }
    }
    Ok(())
}

pub fn stroke(
    tolerance: f64,
    width: f64,
    cap: Cap,
    join: Join,
    miter_limit: f64,
    path: &Path,
) -> Result<Mesh, Error> {
    with_scratch(|scratch| {
        stroke_into(scratch, tolerance, width, cap, join, miter_limit, path)?;
        Ok(Mesh { vertices: scratch.vertices.clone(), indices: scratch.indices.clone() })
    })
}

/// Stroke into scratch flat/index buffers.
pub fn stroke_scratch(
    scratch: &mut Scratch,
    tolerance: f64,
    width: f64,
    cap: Cap,
    join: Join,
    miter_limit: f64,
    path: &Path,
) -> Result<(), Error> {
    stroke_into(scratch, tolerance, width, cap, join, miter_limit, path)?;
    scratch.flat.clear();
    scratch.flat.reserve(scratch.vertices.len() * 2);
    for p in &scratch.vertices {
        scratch.flat.push(p.x);
        scratch.flat.push(p.y);
    }
    Ok(())
}

#[test]
fn crossing_tie_key_uses_existing_padding() {
    assert_eq!(std::mem::size_of::<Crossing>(), 24);
}

#[test]
fn scratch_reuses_capacity_across_tessellations() {
    let path = of_commands(&[
        Command::MoveTo(Point { x: 0.0, y: 0.0 }),
        Command::LineTo(Point { x: 4.0, y: 0.0 }),
        Command::LineTo(Point { x: 4.0, y: 3.0 }),
        Command::LineTo(Point { x: 0.0, y: 3.0 }),
        Command::Close,
    ]);
    with_scratch(|s| {
        tessellate_scratch(s, 0.25, FillRule::NonZero, &path).unwrap();
        let v_cap = s.vertices.capacity();
        let i_cap = s.indices.capacity();
        let e_cap = s.edges.capacity();
        let y_cap = s.ys.capacity();
        tessellate_scratch(s, 0.25, FillRule::NonZero, &path).unwrap();
        assert!(s.vertices.capacity() >= v_cap, "vertices reallocated");
        assert!(s.indices.capacity() >= i_cap, "indices reallocated");
        assert!(s.edges.capacity() >= e_cap, "edges reallocated");
        assert!(s.ys.capacity() >= y_cap, "ys reallocated");
    });
}
