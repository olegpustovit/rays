//! What an editor reads of a cooked geometry: summaries, bounds, the sheet's columns, a wire mesh,
//! picking and the probed-iteration tint. Pure over the geometry; no UI.
//!
//! Gaps against OCaml: `points_of_nothing` is a function (Rust has no such value); `tags` copies
//! the tag array where OCaml borrowed it.
use rays::mesh;
use rays_math::vec3;
use rdk_core::attribute::{self, Owner, Storage};
use rdk_core::{error, geometry, group, packed, topology, topology_index};
use rdk_geom::surface_index;

pub type T = geometry::T;
pub type Surface = surface_index::T;

pub fn error_string(value: &error::T) -> String {
    error::to_string(value)
}

/// A geometry of no points.
pub fn points_of_nothing() -> Result<T, error::T> {
    rdk_ops::line_geometry::points(&[])
}

pub fn point_count(value: &T) -> i64 {
    geometry::point_count(value)
}

pub fn bounds(value: &T) -> Option<(vec3::T, vec3::T)> {
    let b = packed::Float3::bounds(geometry::positions(value)).expect("Geometry_view.bounds")?;
    Some((vec3::create(b[0], b[1], b[2]), vec3::create(b[3], b[4], b[5])))
}

pub struct Summary {
    pub points: i64,
    pub prims: i64,
    pub groups: Vec<String>,
    pub attributes: Vec<(String, String, String, i64)>,
}

/// Point and primitive counts, group names and (owner, name, kind, length) of each attribute.
pub fn summary(g: &T) -> Summary {
    let mut groups: Vec<String> = geometry::groups(g).iter().map(|x| group::name(x).to_owned()).collect();
    groups.sort();
    groups.dedup();
    Summary {
        points: packed::Float3::length(geometry::positions(g)),
        prims: geometry::primitive_count(g),
        groups,
        attributes: geometry::attributes(g)
            .iter()
            .map(|a| {
                let owner = match attribute::owner(a) {
                    Owner::Point => "point",
                    Owner::Vertex => "vertex",
                    Owner::Primitive => "primitive",
                    Owner::Detail => "detail",
                };
                (owner.to_owned(), attribute::name(a).to_owned(), attribute::kind_name(a).to_owned(), attribute::length(a))
            })
            .collect(),
    }
}

/// The triangles a scene draws: a polygon of n corners fans into n - 2 (a count, not a walk).
pub fn triangles(g: &T) -> i64 {
    (geometry::vertex_count(g) - 2 * geometry::primitive_count(g)).max(0)
}

/// Every polygon edge once, as a line mesh.
pub fn wire_mesh(value: &T) -> Result<mesh::t, String> {
    let topo = geometry::topology(value);
    let edges = topology_index::create(None, topo).map_err(|e| error::to_string(&e))?;
    let points = geometry::positions(value);
    let view = packed::Float3::View::view(points).map_err(|e| error::to_string(&e))?;
    let vertices: Vec<vec3::T> = (0..view.x.len()).map(|i| vec3::create(view.x[i], view.y[i], view.z[i])).collect();
    let n = topology_index::edge_count(&edges);
    let mut indices = vec![0i64; (n * 2) as usize];
    for edge in 0..n {
        let (a, b) = topology_index::edge_points(&edges, edge).map_err(|e| error::to_string(&e))?;
        indices[(edge * 2) as usize] = a;
        indices[(edge * 2 + 1) as usize] = b;
    }
    mesh::View::create_owned(Some(mesh::mode::Lines), Some(indices), &vertices)
}

// ---- the sheet ----

fn owner_of(owner: i64) -> Owner {
    match owner {
        0 => Owner::Point,
        1 => Owner::Vertex,
        2 => Owner::Primitive,
        _ => Owner::Detail,
    }
}

/// Rows of the sheet for an owner: 0 point, 1 vertex, 2 primitive, 3 detail.
pub fn count(owner: i64, value: &T) -> i64 {
    match owner_of(owner) {
        Owner::Point => geometry::point_count(value),
        Owner::Vertex => geometry::vertex_count(value),
        Owner::Primitive => geometry::primitive_count(value),
        Owner::Detail => 1,
    }
}

fn number(value: f64) -> String {
    param::float_text_with_decimal(value, true)
}

fn ints(values: &[i64]) -> String {
    values.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" ")
}

fn floats(values: &[f64]) -> String {
    values.iter().map(|v| number(*v)).collect::<Vec<_>>().join(" ")
}

/// One cell of an attribute: `component` picks the axis of a tuple storage.
fn cell(a: &attribute::T, row: i64, component: usize) -> String {
    let Ok(storage) = attribute::View::storage(a) else { return String::new() };
    let at = row as usize;
    match storage {
        Storage::Float(values) => number(values[at]),
        Storage::Int(values) => values[at].to_string(),
        Storage::Text(values) => values[at].clone(),
        Storage::IntArray(values) => packed::IntArray::get(values, row).map(|v| ints(&v)).unwrap_or_default(),
        Storage::FloatArray(values) => packed::FloatArray::get(values, row).map(|v| floats(&v)).unwrap_or_default(),
        Storage::Float2(values) => {
            let (x, y) = packed::Float2::get(values, row).unwrap_or_default();
            number(if component == 0 { x } else { y })
        }
        Storage::Float3(values) => {
            let (x, y, z) = packed::Float3::get(values, row).unwrap_or_default();
            number(match component {
                0 => x,
                1 => y,
                _ => z,
            })
        }
        Storage::Float4(values) => {
            let (x, y, z, w) = packed::Float4::get(values, row).unwrap_or_default();
            number(match component {
                0 => x,
                1 => y,
                2 => z,
                _ => w,
            })
        }
    }
}

pub type Column = (String, Box<dyn Fn(i64) -> String>);

/// The sheet's columns for an owner (see [`count`]): a header and the cell of a row.
pub fn columns(owner: i64, value: &T) -> Vec<Column> {
    let owner = owner_of(owner);
    let mut result: Vec<Column> = vec![("index".to_owned(), Box::new(|row| row.to_string()))];
    if owner == Owner::Point {
        let positions = geometry::positions(value).clone();
        for (i, axis) in ["x", "y", "z"].iter().enumerate() {
            let positions = positions.clone();
            result.push((
                format!("P.{axis}"),
                Box::new(move |row| {
                    let (x, y, z) = packed::Float3::get(&positions, row).unwrap_or_default();
                    number(match i {
                        0 => x,
                        1 => y,
                        _ => z,
                    })
                }),
            ));
        }
    }
    for a in geometry::attributes(value) {
        if attribute::owner(&a) != owner {
            continue;
        }
        let name = attribute::name(&a).to_owned();
        let length = attribute::length(&a);
        // the storage kind, read once for the header axes (the cells re-borrow it)
        let axes: &[&str] = match attribute::View::storage(&a) {
            Ok(Storage::Float2(_)) => &["x", "y"],
            Ok(Storage::Float3(_)) => &["x", "y", "z"],
            Ok(Storage::Float4(_)) => &["x", "y", "z", "w"],
            _ => &[],
        };
        if axes.is_empty() {
            let a = a.clone();
            result.push((name, Box::new(move |row| if row < length { cell(&a, row, 0) } else { String::new() })));
        } else {
            for (i, axis) in axes.iter().enumerate() {
                let a = a.clone();
                result.push((
                    format!("{name}.{axis}"),
                    Box::new(move |row| if row < length { cell(&a, row, i) } else { String::new() }),
                ));
            }
        }
    }
    for g in geometry::groups(value) {
        let matches = matches!(
            (group::owner(&g), owner),
            (group::Owner::Point, Owner::Point) | (group::Owner::Vertex, Owner::Vertex) | (group::Owner::Primitive, Owner::Primitive)
        );
        if matches {
            result.push((
                format!("group:{}", group::name(&g)),
                Box::new(move |row| if row < group::length(&g) && group::mem(row, &g) { "1".to_owned() } else { "0".to_owned() }),
            ));
        }
    }
    result
}

// ---- picking ----

/// The primitive-owned integer provenance tags named `attribute_name`.
pub fn tags(attribute_name: &str, value: &T) -> Option<Vec<i64>> {
    let a = geometry::find_attribute(Owner::Primitive, attribute_name, value)?;
    match attribute::View::storage(&a) {
        Ok(Storage::Int(values)) => Some(values.clone()),
        _ => None,
    }
}

/// A ray index of the geometry; `None` when it cannot be picked (degenerate polygons).
pub fn surface(value: &T) -> Option<Surface> {
    surface_index::create(None, None, None, None, None, value).ok()
}

/// The nearest hit as (distance, tag); a geometry without tags never picks.
pub fn cast(attribute_name: &str, surface: Option<&Surface>, value: &T, origin: vec3::T, direction: vec3::T) -> Option<(f64, i64)> {
    let (surface, tags) = (surface?, tags(attribute_name, value)?);
    match surface_index::raycast(surface, origin, direction) {
        Ok(Some(hit)) => Some((hit.distance, tags[hit.primitive as usize])),
        _ => None,
    }
}

/// The nearest hit as (distance, `shop_materialpath` of that primitive).
pub fn cast_material(surface: Option<&Surface>, value: &T, origin: vec3::T, direction: vec3::T) -> Option<(f64, Option<String>)> {
    let surface = surface?;
    match surface_index::raycast(surface, origin, direction) {
        Ok(Some(hit)) => {
            let path = geometry::find_attribute(Owner::Primitive, "shop_materialpath", value).and_then(|a| match attribute::storage(&a) {
                Ok(Storage::Text(paths)) if !paths[hit.primitive as usize].is_empty() => Some(paths[hit.primitive as usize].clone()),
                _ => None,
            });
            Some((hit.distance, path))
        }
        _ => None,
    }
}

const DIM: f64 = 0.3;
const MIX: f64 = 0.6;

enum Base {
    Vertex(packed::Float4::View::View),
    Point(packed::Float4::View::View),
}

/// The primitives whose tag satisfies `lit` take the accent over their colour, the rest dim: a
/// vertex colour per corner, so a prepare that reads `Cd` draws it without knowing about picking.
pub fn tint(attribute_name: &str, accent: (f64, f64, f64), lit: &dyn Fn(i64) -> bool, value: &T) -> T {
    let Some(tags) = tags(attribute_name, value) else { return value.clone() };
    let (ar, ag, ab) = accent;
    let topo: &topology::T = geometry::topology(value);
    let corners = topology::vertex_count(topo) as usize;
    let float4 = |a: &attribute::T| match attribute::View::storage(a) {
        Ok(Storage::Float4(c)) => Some(packed::Float4::View::view(c)),
        _ => None,
    };
    let base = match (geometry::find_attribute(Owner::Vertex, "Cd", value), geometry::find_attribute(Owner::Point, "Cd", value)) {
        (Some(a), _) => float4(&a).map(Base::Vertex),
        (None, Some(a)) => float4(&a).map(Base::Point),
        (None, None) => None,
    };
    let (mut r, mut g, mut b, mut w) = (vec![1.0; corners], vec![1.0; corners], vec![1.0; corners], vec![1.0; corners]);
    for (primitive, tag) in tags.iter().enumerate() {
        let Ok((first, last)) = topology::primitive_vertex_range(topo, primitive as i64) else { return value.clone() };
        let lit = lit(*tag);
        for corner in first..last {
            let c = corner as usize;
            let (cr, cg, cb, cw) = match &base {
                None => (1.0, 1.0, 1.0, 1.0),
                Some(Base::Vertex(v)) => (v.x[c], v.y[c], v.z[c], v.w[c]),
                Some(Base::Point(v)) => {
                    let Ok(p) = topology::point_of_vertex(topo, corner) else { return value.clone() };
                    let p = p as usize;
                    (v.x[p], v.y[p], v.z[p], v.w[p])
                }
            };
            let shade = |own: f64, accent: f64| if lit { own * (1.0 - MIX) + accent * MIX } else { own * DIM };
            r[c] = shade(cr, ar);
            g[c] = shade(cg, ag);
            b[c] = shade(cb, ab);
            w[c] = cw;
        }
    }
    let Ok(colors) = packed::Float4::of_owned(r, g, b, w) else { return value.clone() };
    attribute::create_key_owned(&attribute::color(Owner::Vertex), Storage::Float4(colors))
        .and_then(|a| geometry::with_attribute(a, value))
        .unwrap_or_else(|_| value.clone())
}
