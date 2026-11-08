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
    /// The commands in order (OCaml `(path :> command array)`).
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

/// OCaml Stdlib `max` (NaN-propagating as written in the source, unlike `f64::max`).
fn ocaml_max(x: f64, y: f64) -> f64 {
    if x >= y { x } else { y }
}

/// OCaml `Array.sort` (heap sort, not stable), reproduced step for step so that
/// equal keys end up in the same order as in the OCaml tessellator.
fn ocaml_sort<T: Copy, F: Fn(T, T) -> std::cmp::Ordering>(a: &mut [T], cmp: F) {
    use std::cmp::Ordering::{Greater, Less};
    // Ok(child) of the larger son, or Err(i) where OCaml raises `Bottom i`.
    fn maxson<T: Copy, F: Fn(T, T) -> std::cmp::Ordering>(a: &[T], cmp: &F, l: usize, i: usize) -> Result<usize, usize> {
        let i31 = i + i + i + 1;
        if i31 + 2 < l {
            let mut x = i31;
            if cmp(a[i31], a[i31 + 1]) == Less { x = i31 + 1; }
            if cmp(a[x], a[i31 + 2]) == Less { x = i31 + 2; }
            Ok(x)
        } else if i31 + 1 < l && cmp(a[i31], a[i31 + 1]) == Less {
            Ok(i31 + 1)
        } else if i31 < l {
            Ok(i31)
        } else {
            Err(i)
        }
    }
    let l = a.len();
    let trickle = |a: &mut [T], mut i: usize, e: T| loop {
        match maxson(a, &cmp, l, i) {
            Err(b) => { a[b] = e; return; }
            Ok(j) => {
                if cmp(a[j], e) == Greater { a[i] = a[j]; i = j; } else { a[i] = e; return; }
            }
        }
    };
    let bubble = |a: &mut [T], l: usize, mut i: usize| -> usize {
        loop {
            match maxson(a, &cmp, l, i) {
                Err(b) => return b,
                Ok(j) => { a[i] = a[j]; i = j; }
            }
        }
    };
    let trickleup = |a: &mut [T], mut i: usize, e: T| loop {
        let father = (i - 1) / 3;
        if cmp(a[father], e) == Less {
            a[i] = a[father];
            if father > 0 { i = father; } else { a[0] = e; return; }
        } else {
            a[i] = e;
            return;
        }
    };
    for i in (0..(l + 1) / 3).rev() {
        let e = a[i];
        trickle(a, i, e);
    }
    for i in (2..l).rev() {
        let e = a[i];
        a[i] = a[0];
        let b = bubble(a, i, 0);
        trickleup(a, b, e);
    }
    if l > 1 {
        let e = a[1];
        a[1] = a[0];
        a[0] = e;
    }
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
        || ocaml_max(distance_to_line_squared(c1, a, b), distance_to_line_squared(c2, a, b))
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

fn flatten(tolerance: f64, path: &Path) -> Result<Vec<Vec<Point>>, Error> {
    if !tolerance.is_finite() || tolerance <= 0.0 {
        return Err(Error::InvalidTolerance(tolerance));
    }
    if path.0.is_empty() {
        return Err(Error::EmptyPath);
    }
    let tolerance_squared = tolerance * tolerance;
    let mut contours = Vec::new();
    let mut points = Vec::new();
    let mut current: Option<Point> = None;
    let mut start: Option<Point> = None;
    for &command in &path.0 {
        match command {
            Command::MoveTo(p) => {
                if !finite(p) { return Err(Error::NonFinitePoint(p)); }
                if current.is_some() {
                    if !points.is_empty() { contours.push(std::mem::take(&mut points)); }
                }
                add_point(&mut points, p)?;
                current = Some(p);
                start = Some(p);
            }
            Command::LineTo(p) => {
                if !finite(p) { return Err(Error::NonFinitePoint(p)); }
                if current.is_none() { return Err(Error::MissingCurrentPoint); }
                add_point(&mut points, p)?;
                current = Some(p);
            }
            Command::QuadraticTo(control, p) => {
                if !finite(control) { return Err(Error::NonFinitePoint(control)); }
                if !finite(p) { return Err(Error::NonFinitePoint(p)); }
                let Some(a) = current else { return Err(Error::MissingCurrentPoint) };
                quadratic(&mut points, 0, a, control, p, tolerance_squared)?;
                current = Some(p);
            }
            Command::CubicTo(c1, c2, p) => {
                if !finite(c1) { return Err(Error::NonFinitePoint(c1)); }
                if !finite(c2) { return Err(Error::NonFinitePoint(c2)); }
                if !finite(p) { return Err(Error::NonFinitePoint(p)); }
                let Some(a) = current else { return Err(Error::MissingCurrentPoint) };
                cubic(&mut points, 0, a, c1, c2, p, tolerance_squared)?;
                current = Some(p);
            }
            Command::Close => {
                match (current, start) {
                    (Some(last), Some(first)) if last != first => add_point(&mut points, first)?,
                    (Some(_), Some(_)) => (),
                    _ => return Err(Error::MissingCurrentPoint),
                }
                if !points.is_empty() { contours.push(std::mem::take(&mut points)); }
                current = None;
                start = None;
            }
        }
    }
    if current.is_some() && !points.is_empty() {
        contours.push(points);
    }
    Ok(contours)
}

#[derive(Clone, Copy)]
struct Crossing {
    x0: f64,
    x1: f64,
    winding: i32,
}
fn compare_float(a: f64, b: f64) -> std::cmp::Ordering {
    match (a.is_nan(), b.is_nan()) {
        (true, true) => std::cmp::Ordering::Equal,
        (true, false) => std::cmp::Ordering::Less,
        (false, true) => std::cmp::Ordering::Greater,
        (false, false) => if a < b { std::cmp::Ordering::Less } else if a > b { std::cmp::Ordering::Greater } else { std::cmp::Ordering::Equal },
    }
}


pub fn tessellate(tolerance: f64, fill_rule: FillRule, path: &Path) -> Result<Mesh, Error> {
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
    let contours = flatten(tolerance, path)?;
    let mut ys: Vec<f64> = contours.iter().flatten().map(|p| p.y).collect();
    ocaml_sort(&mut ys, compare_float);
    ys.dedup_by(|a, b| *a == *b);
    let mut vertices = Vec::new();
    let mut triangles = Vec::new();
    // OCaml prepends each quad's six indices onto a list that is reversed at the end,
    // so every quad is emitted as one block in reverse order.
    let add_quad = |vertices: &mut Vec<Point>, triangles: &mut Vec<usize>, left: Crossing, right: Crossing, y0: f64, y1: f64| -> Result<(), Error> {
        if vertices.len().checked_add(4).filter(|n| *n <= MAX_POINTS).is_none() {
            return Err(Error::ComplexityLimit);
        }
        let base = vertices.len();
        vertices.extend_from_slice(&[
            Point { x: left.x0, y: y0 }, Point { x: right.x0, y: y0 },
            Point { x: right.x1, y: y1 }, Point { x: left.x1, y: y1 },
        ]);
        triangles.extend_from_slice(&[base + 3, base + 2, base, base + 2, base + 1, base]);
        Ok(())
    };
    for band in 0..ys.len().saturating_sub(1) {
        let y0 = ys[band];
        let y1 = ys[band + 1];
        if y1 <= y0 { continue; }
        let middle = (y0 + y1) * 0.5;
        let mut edges = Vec::new();
        for contour in &contours {
            for pair in contour.windows(2) {
                let a = pair[0];
                let b = pair[1];
                let low = a.y.min(b.y);
                let high = a.y.max(b.y);
                if a.y != b.y && middle >= low && middle < high {
                    let at = |y| a.x + ((y - a.y) * (b.x - a.x) / (b.y - a.y));
                    edges.push(Crossing { x0: at(y0), x1: at(y1), winding: if b.y > a.y { 1 } else { -1 } });
                }
            }
        }
        // OCaml collects crossings by prepending, so the array it sorts is in reverse order.
        edges.reverse();
        ocaml_sort(&mut edges, |a: Crossing, b: Crossing| compare_float((a.x0 + a.x1) * 0.5, (b.x0 + b.x1) * 0.5));
        let mut inside = false;
        let mut winding = 0;
        let mut left = None;
        for edge in edges {
            let was_inside = inside;
            match fill_rule {
                FillRule::EvenOdd => inside = !inside,
                FillRule::NonZero => { winding += edge.winding; inside = winding != 0; }
            }
            if !was_inside && inside { left = Some(edge); }
            else if was_inside && !inside {
                let Some(left_edge) = left else { return Err(Error::InvalidTopology); };
                add_quad(&mut vertices, &mut triangles, left_edge, edge, y0, y1)?;
            }
        }
    }
    Ok(Mesh { vertices, indices: triangles.into_iter().map(|i| i as i64).collect() })
}

pub fn stroke(
    tolerance: f64,
    width: f64,
    cap: Cap,
    join: Join,
    miter_limit: f64,
    path: &Path,
) -> Result<Mesh, Error> {
    if !width.is_finite() || width <= 0.0 { return Err(Error::InvalidWidth(width)); }
    if !miter_limit.is_finite() || miter_limit < 1.0 { return Err(Error::InvalidMiterLimit(miter_limit)); }
    let contours = flatten(tolerance, path)?;
    let mut vertices = Vec::new();
    let mut indices = Vec::new();
    let half = width * 0.5;
    let unit = |a: Point, b: Point| {
        let dx = b.x - a.x;
        let dy = b.y - a.y;
        let length = (dx * dx + dy * dy).sqrt();
        if length == 0.0 { None } else { Some((dx / length, dy / length)) }
    };
    let offset = |p: Point, nx: f64, ny: f64, scale: f64| Point { x: p.x + nx * scale, y: p.y + ny * scale };
    // OCaml's `triangle (vertex a) (vertex b) (vertex c)` evaluates its arguments right to
    // left, so the vertices are stored c, b, a and the triangle reads back in that order.
    let add_triangle = |vertices: &mut Vec<Point>, indices: &mut Vec<usize>, a: Point, b: Point, c: Point| -> Result<(), Error> {
        if vertices.len().checked_add(3).filter(|n| *n <= MAX_POINTS).is_none() { return Err(Error::ComplexityLimit); }
        let base = vertices.len();
        vertices.extend_from_slice(&[c, b, a]);
        indices.extend_from_slice(&[base, base + 1, base + 2]);
        Ok(())
    };
    let fan = |vertices: &mut Vec<Point>, indices: &mut Vec<usize>, center: Point, radius: f64, start_angle: f64, delta: f64| -> Result<(), Error> {
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
    for contour in contours {
        let count = contour.len();
        let closed = count > 2 && contour[0] == contour[count - 1];
        for pair in contour.windows(2) {
            if let Some((dx, dy)) = unit(pair[0], pair[1]) {
                let (nx, ny) = (-dy, dx);
                let (a, b) = (pair[0], pair[1]);
                let a0 = offset(a, nx, ny, half);
                let a1 = offset(a, nx, ny, -half);
                let b0 = offset(b, nx, ny, half);
                let b1 = offset(b, nx, ny, -half);
                if vertices.len().checked_add(4).filter(|n| *n <= MAX_POINTS).is_none() { return Err(Error::ComplexityLimit); }
                let base = vertices.len();
                vertices.extend_from_slice(&[a0, a1, b1, b0]);
                indices.extend_from_slice(&[base + 2, base + 1, base, base + 3, base + 2, base]);
            }
        }
        let add_join = |vertices: &mut Vec<Point>, indices: &mut Vec<usize>, a: Point, b: Point, c: Point| -> Result<(), Error> {
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
        let add_cap = |vertices: &mut Vec<Point>, indices: &mut Vec<usize>, point: Point, direction: (f64, f64), at_start: bool| -> Result<(), Error> {
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
            for i in 1..count.saturating_sub(1) { add_join(&mut vertices, &mut indices, contour[i - 1], contour[i], contour[i + 1])?; }
            add_join(&mut vertices, &mut indices, contour[count - 2], contour[0], contour[1])?;
        } else if count >= 2 {
            for i in 1..count.saturating_sub(1) { add_join(&mut vertices, &mut indices, contour[i - 1], contour[i], contour[i + 1])?; }
            if let Some(direction) = unit(contour[0], contour[1]) { add_cap(&mut vertices, &mut indices, contour[0], direction, true)?; }
            if let Some(direction) = unit(contour[count - 2], contour[count - 1]) { add_cap(&mut vertices, &mut indices, contour[count - 1], direction, false)?; }
        }
    }
    Ok(Mesh { vertices, indices: indices.into_iter().map(|i| i as i64).collect() })
}
