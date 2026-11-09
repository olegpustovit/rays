use rays_math::float::{min as float_min, max as float_max};
use crate::{path, render_ir};
use path::{Command, Point};
use render_ir::Geometry;

fn point((x, y): (f64, f64)) -> Point { Point { x, y } }
fn geometry_of_mesh(color: i32, mesh: path::Mesh) -> Geometry {
    let vertices = mesh.vertices.into_iter().flat_map(|p| [p.x, p.y]).collect();
    Geometry { vertices, indices: mesh.indices, color }
}
fn path_error(operation: &str, result: Result<path::Mesh, path::Error>) -> Result<path::Mesh, String> {
    match result {
        Ok(mesh) => Ok(mesh),
        Err(path::Error::EmptyPath) => Ok(path::Mesh { vertices: Vec::new(), indices: Vec::new() }),
        Err(_) => Err(operation.to_owned()),
    }
}
pub fn fill_path(color: i32, path: &path::Path) -> Result<Geometry, String> {
    let mesh = path_error("Shape2 fill", path::tessellate(0.25, path::FillRule::NonZero, path))?;
    Ok(geometry_of_mesh(color, mesh))
}
pub fn stroke_path(color: i32, path: &path::Path, width: f64) -> Result<Geometry, String> {
    let mesh = path_error("Shape2 stroke", path::stroke(0.25, width, path::Cap::Butt, path::Join::Miter, 4.0, path))?;
    Ok(geometry_of_mesh(color, mesh))
}
fn styled(path: &path::Path, fill: Option<i32>, stroke: Option<i32>) -> Result<Vec<Geometry>, String> {
    let mut output = Vec::new();
    if let Some(color) = fill { output.push(fill_path(color, path)?); }
    if let Some(color) = stroke { output.push(stroke_path(color, path, 1.0)?); }
    Ok(output)
}
fn path_of_points(closed: bool, points: &[(f64, f64)]) -> path::Path {
    let mut commands = Vec::with_capacity(points.len() + if closed { 1 } else { 0 });
    if let Some((&first, rest)) = points.split_first() {
        commands.push(Command::MoveTo(point(first)));
        commands.extend(rest.iter().copied().map(|p| Command::LineTo(point(p))));
        if closed { commands.push(Command::Close); }
    }
    path::of_commands(&commands)
}

pub fn line(from: (f64, f64), to: (f64, f64), width: f64, color: i32) -> Result<Geometry, String> {
    stroke_path(color, &path_of_points(false, &[from, to]), float_max(1.0, width))
}
pub fn polygon(points: &[(f64, f64)], fill: Option<i32>, stroke: Option<i32>) -> Result<Vec<Geometry>, String> {
    let fill = if fill.is_none() && stroke.is_none() { Some(0xffff_ffffu32 as i32) } else { fill };
    styled(&path_of_points(true, points), fill, stroke)
}
pub fn polyline(points: &[(f64, f64)], color: i32) -> Result<Geometry, String> {
    stroke_path(color, &path_of_points(false, points), 1.0)
}
pub fn path(fill: Option<i32>, stroke: Option<i32>, value: &path::Path) -> Result<Vec<Geometry>, String> {
    styled(value, fill, stroke)
}
pub fn rounded_rect(width: f64, height: f64, radius: f64, fill: Option<i32>, stroke: Option<i32>) -> Result<Vec<Geometry>, String> {
    let fill = if fill.is_none() && stroke.is_none() { Some(0xffff_ffffu32 as i32) } else { fill };
    let cap = float_min(width.abs(), height.abs()) / 2.0;
    let radius = float_max(0.0, float_min(radius, cap));
    let k = 0.5522847498307936 * radius;
    let p = |x, y| Point { x, y };
    let commands = [
        Command::MoveTo(p(radius, 0.0)), Command::LineTo(p(width - radius, 0.0)),
        Command::CubicTo(p(width - radius + k, 0.0), p(width, radius - k), p(width, radius)),
        Command::LineTo(p(width, height - radius)),
        Command::CubicTo(p(width, height - radius + k), p(width - radius + k, height), p(width - radius, height)),
        Command::LineTo(p(radius, height)),
        Command::CubicTo(p(radius - k, height), p(0.0, height - radius + k), p(0.0, height - radius)),
        Command::LineTo(p(0.0, radius)),
        Command::CubicTo(p(0.0, radius - k), p(radius - k, 0.0), p(radius, 0.0)),
        Command::Close,
    ];
    styled(&path::of_commands(&commands), fill, stroke)
}
fn ellipse_points((cx, cy): (f64, f64), rx: f64, ry: f64) -> Vec<(f64, f64)> {
    (0..32).map(|index| {
        let angle = 2.0 * std::f64::consts::PI * index as f64 / 32.0;
        (cx + rx * rays_math::libm::cos(angle), cy + ry * rays_math::libm::sin(angle))
    }).collect()
}
pub fn ellipse(center: (f64, f64), rx: f64, ry: f64, fill: Option<i32>, stroke: Option<i32>) -> Result<Vec<Geometry>, String> {
    polygon(&ellipse_points(center, rx, ry), fill, stroke)
}
