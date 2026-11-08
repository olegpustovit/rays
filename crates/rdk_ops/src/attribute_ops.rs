//! Attribute ownership and reduction operations (OCaml `Attribute_ops`). The thin wrappers over the
//! lifecycle/blur/transfer/interpolate modules re-code their failures as OCaml does.
use std::sync::Mutex;

use rdk_core::{cancel, error, support};

/// `Error.make` for an operation, code and message (neither operation nor code is blank here).
pub fn make_error(operation: &str, code: &str, message: &str) -> error::T {
    error::make(operation.to_owned(), code.to_owned(), message.to_owned(), None).unwrap_or_else(|bad| bad)
}

/// `Error.guard`/`Result.map_error (Error.make ~operation ~code)`: a cancellation becomes this
/// operation's cancelled error, any other failure keeps only its message under the given code.
pub fn recode(failure: &error::T, operation: &str, code: &str, cancelled: &str) -> error::T {
    if error::code(failure) == "cancelled" {
        make_error(operation, "cancelled", cancelled)
    } else {
        make_error(operation, code, error::message(failure))
    }
}

/// OCaml `String.trim s = ""`.
pub fn is_blank(text: &str) -> bool {
    support::ocaml_trim_is_empty(text)
}

/// OCaml `Printf "%S"`: a double-quoted string with `String.escaped` escapes.
pub fn ocaml_quote(text: impl AsRef<[u8]>) -> String {
    let mut out = String::from("\"");
    for &byte in text.as_ref() {
        match byte {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            b'\n' => out.push_str("\\n"),
            b'\t' => out.push_str("\\t"),
            b'\r' => out.push_str("\\r"),
            8 => out.push_str("\\b"),
            32..=126 => out.push(byte as char),
            _ => out.push_str(&format!("\\{byte:03}")),
        }
    }
    out.push('"');
    out
}

enum Chunk {
    Planes(Vec<Vec<f64>>),
    Ints(Vec<i64>),
    Texts(Vec<Vec<u8>>),
}

type Slots = Vec<Mutex<Option<Chunk>>>;

fn new_slots(count: i64, grain: i64) -> Slots {
    let ranges = if count <= 0 { 0 } else { ((count - 1) / grain + 1) as usize };
    (0..ranges).map(|_| Mutex::new(None)).collect()
}

fn put(slots: &Slots, range: i64, value: Chunk) {
    *slots[range as usize].lock().unwrap_or_else(std::sync::PoisonError::into_inner) = Some(value);
}

fn take(slots: Slots) -> Vec<Chunk> {
    slots
        .into_iter()
        .filter_map(|slot| slot.into_inner().unwrap_or_else(std::sync::PoisonError::into_inner))
        .collect()
}

/// Runs `work(element, planes)` for every element of `0..count`, in disjoint ranges of `grain`
/// elements on the pool. `planes` holds the element's component values, loaded from `init` (one
/// slice per plane, each `count` long) and stored back; per-range buffers are joined in range order,
/// so the result is identical for any domain count. The first failing element (lowest range) wins.
pub fn map_planes(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    init: &[&[f64]],
    work: &(dyn Fn(i64, &mut [f64]) -> Result<(), error::T> + Sync),
) -> Result<Vec<Vec<f64>>, error::T> {
    let width = init.len();
    let count = init.first().map_or(0, |plane| plane.len()) as i64;
    let slots: Slots = new_slots(count, grain);
    support::run_indexed_ranges(grain, count, &|range, first, last| {
        let mut local: Vec<Vec<f64>> = init.iter().map(|plane| plane[first as usize..last as usize].to_vec()).collect();
        let mut slot = [0.0_f64; 8];
        for element in first..last {
            cancel::poll(cancel_token, element, 4095)?;
            let at = (element - first) as usize;
            for plane in 0..width {
                slot[plane] = local[plane][at];
            }
            work(element, &mut slot[..width])?;
            for plane in 0..width {
                local[plane][at] = slot[plane];
            }
        }
        put(&slots, range, Chunk::Planes(local));
        Ok(())
    })?;
    let mut output: Vec<Vec<f64>> = (0..width).map(|_| Vec::with_capacity(count as usize)).collect();
    for chunk in take(slots) {
        if let Chunk::Planes(local) = chunk {
            for (plane, values) in local.into_iter().enumerate() {
                output[plane].extend(values);
            }
        }
    }
    Ok(output)
}

/// `map_planes` for one integer value per element.
pub fn map_ints(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    init: &[i64],
    work: &(dyn Fn(i64, &mut i64) -> Result<(), error::T> + Sync),
) -> Result<Vec<i64>, error::T> {
    let count = init.len() as i64;
    let slots: Slots = new_slots(count, grain);
    support::run_indexed_ranges(grain, count, &|range, first, last| {
        let mut local = init[first as usize..last as usize].to_vec();
        for element in first..last {
            cancel::poll(cancel_token, element, 4095)?;
            work(element, &mut local[(element - first) as usize])?;
        }
        put(&slots, range, Chunk::Ints(local));
        Ok(())
    })?;
    let mut output = Vec::new();
    for chunk in take(slots) {
        if let Chunk::Ints(values) = chunk {
            output.extend(values);
        }
    }
    Ok(output)
}

/// `map_planes` for one text value per element.
pub fn map_texts(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    init: &[Vec<u8>],
    work: &(dyn Fn(i64, &mut Vec<u8>) -> Result<(), error::T> + Sync),
) -> Result<Vec<Vec<u8>>, error::T> {
    let count = init.len() as i64;
    let slots: Slots = new_slots(count, grain);
    support::run_indexed_ranges(grain, count, &|range, first, last| {
        let mut local = init[first as usize..last as usize].to_vec();
        for element in first..last {
            cancel::poll(cancel_token, element, 4095)?;
            work(element, &mut local[(element - first) as usize])?;
        }
        put(&slots, range, Chunk::Texts(local));
        Ok(())
    })?;
    let mut output = Vec::new();
    for chunk in take(slots) {
        if let Chunk::Texts(values) = chunk {
            output.extend(values);
        }
    }
    Ok(output)
}

// ---- public operations ----

use std::sync::Arc;

use rdk_core::{attribute, attribute_pattern, geometry, group};

use crate::{attribute_blur, attribute_interpolate, attribute_lifecycle, attribute_transfer};

pub fn delete(
    params: &attribute_lifecycle::DeleteParams,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    attribute_lifecycle::delete(params, geometry_value).map_err(|failure| {
        recode(&failure, "attribute_delete", "invalid_attribute", "attribute deletion was cancelled")
    })
}

pub fn rename(
    cancel_token: Option<&cancel::T>,
    rules: &[attribute_lifecycle::RenameRule],
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    attribute_lifecycle::rename(cancel_token, rules, geometry_value).map_err(|failure| {
        recode(&failure, "attribute_rename", "invalid_attribute", "attribute renaming was cancelled")
    })
}

pub fn swap(
    cancel_token: Option<&cancel::T>,
    rules: &[attribute_lifecycle::SwapRule],
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    attribute_lifecycle::swap(cancel_token, rules, geometry_value).map_err(|failure| {
        recode(&failure, "attribute_swap", "invalid_attribute", "attribute swap was cancelled")
    })
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InterpolateAttribute {
    pub interpolate_owner: attribute::Owner,
    pub interpolate_source: String,
    pub interpolate_target: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InterpolateDriver {
    PrimitiveUvw { primitive_attribute: String, uvw_attribute: String },
    PointWeights { numbers_attribute: String, weights_attribute: String },
    VertexWeights { numbers_attribute: String, weights_attribute: String },
    PrimitiveWeights { numbers_attribute: String, weights_attribute: String },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InterpolateComputed {
    pub computed_owner: attribute::Owner,
    pub computed_numbers_attribute: String,
    pub computed_weights_attribute: String,
}

pub fn blur_points(
    params: &attribute_blur::Params,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    attribute_blur::run(params, geometry_value)
}

pub fn interpolate_attribute(
    into: Option<&str>,
    owner: attribute::Owner,
    name: &str,
) -> Result<InterpolateAttribute, error::T> {
    let bad = |message: &str| make_error("attribute_interpolate", "invalid_argument", message);
    if is_blank(name) {
        return Err(bad("Rdk.Attribute_ops.interpolate_attribute: empty source name"));
    }
    if name == "P" && owner != attribute::Owner::Point {
        return Err(bad("Rdk.Attribute_ops.interpolate_attribute: canonical P must be point owned"));
    }
    let target = into.unwrap_or(name);
    if is_blank(target) {
        return Err(bad("Rdk.Attribute_ops.interpolate_attribute: empty target name"));
    }
    Ok(InterpolateAttribute {
        interpolate_owner: owner,
        interpolate_source: name.to_owned(),
        interpolate_target: target.to_owned(),
    })
}

fn expand_interpolate_patterns(
    match_groups: bool,
    point_pattern: &Option<String>,
    vertex_pattern: &Option<String>,
    primitive_pattern: &Option<String>,
    detail_pattern: &Option<String>,
    source: &geometry::T,
) -> Result<(Vec<InterpolateAttribute>, Vec<attribute_interpolate::GroupSpec>), error::T> {
    let compile = |source_pattern: &Option<String>| -> Result<Option<attribute_pattern::T>, error::T> {
        match source_pattern {
            None => Ok(None),
            Some(text) => attribute_pattern::compile(text).map(Some),
        }
    };
    let patterns = [
        compile(point_pattern)?,
        compile(vertex_pattern)?,
        compile(primitive_pattern)?,
        compile(detail_pattern)?,
    ];
    let pattern = |owner: attribute::Owner| -> Option<&attribute_pattern::T> {
        let index = match owner {
            attribute::Owner::Point => 0,
            attribute::Owner::Vertex => 1,
            attribute::Owner::Primitive => 2,
            attribute::Owner::Detail => 3,
        };
        patterns[index].as_ref()
    };
    let mut output = Vec::new();
    let mut groups = Vec::new();
    if let Some(found) = pattern(attribute::Owner::Point) {
        if attribute_pattern::matches(found, "P") {
            output.push(interpolate_attribute(None, attribute::Owner::Point, "P")?);
        }
    }
    for item in geometry::attributes(source) {
        if let Some(found) = pattern(attribute::owner(&item)) {
            if attribute_pattern::matches(found, attribute::name(&item)) {
                output.push(interpolate_attribute(None, attribute::owner(&item), attribute::name(&item))?);
            }
        }
    }
    if match_groups {
        for item in geometry::groups(source) {
            let owner = match group::owner(&item) {
                group::Owner::Point => attribute::Owner::Point,
                group::Owner::Vertex => attribute::Owner::Vertex,
                group::Owner::Primitive => attribute::Owner::Primitive,
            };
            if let Some(found) = pattern(owner) {
                if attribute_pattern::matches(found, group::name(&item)) {
                    groups.push(attribute_interpolate::GroupSpec { group_source_owner: owner, group_source: item });
                }
            }
        }
    }
    Ok((output, groups))
}

#[derive(Clone)]
pub struct Params {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub selection: Option<group::T>,
    pub driver: Option<InterpolateDriver>,
    pub compute_weights: Option<InterpolateComputed>,
    pub point_pattern: Option<String>,
    pub vertex_pattern: Option<String>,
    pub primitive_pattern: Option<String>,
    pub detail_pattern: Option<String>,
    pub match_groups: bool,
    pub pre_scale: Option<f64>,
    pub normalize_weights: Option<bool>,
    pub threshold: Option<f64>,
    pub blend: Option<f64>,
    pub unmatched: attribute_transfer::Unmatched,
    pub target_owner: attribute::Owner,
    pub attributes: Vec<InterpolateAttribute>,
    pub source: geometry::T,
    pub target: geometry::T,
}

pub fn interpolate(params: &Params) -> Result<geometry::T, error::T> {
    const OP: &str = "attribute_interpolate";
    let invalid_interpolate = |failure: &error::T| {
        let code = error::code(failure);
        if code == "cancelled" {
            make_error(OP, "cancelled", "attribute interpolation was cancelled")
        } else if code == "invalid_parameter" {
            make_error(OP, "invalid_parameter", error::message(failure))
        } else {
            make_error(OP, "invalid_interpolate", error::message(failure))
        }
    };
    let unmatched = match params.unmatched {
        attribute_transfer::Unmatched::KeepTarget => attribute_interpolate::Unmatched::KeepTarget,
        attribute_transfer::Unmatched::DefaultValue => attribute_interpolate::Unmatched::DefaultValue,
    };
    let driver = params.driver.clone().unwrap_or(InterpolateDriver::PrimitiveUvw {
        primitive_attribute: "source_primitive".to_owned(),
        uvw_attribute: "source_uvw".to_owned(),
    });
    let (expanded, groups) = expand_interpolate_patterns(
        params.match_groups,
        &params.point_pattern,
        &params.vertex_pattern,
        &params.primitive_pattern,
        &params.detail_pattern,
        &params.source,
    )
    .map_err(|failure| make_error(OP, "invalid_interpolate", error::message(&failure)))?;
    let mut all_attributes = params.attributes.clone();
    all_attributes.extend(expanded);
    let specs: Vec<attribute_interpolate::Spec> = all_attributes
        .into_iter()
        .map(|item| attribute_interpolate::Spec {
            source_owner: item.interpolate_owner,
            source_name: item.interpolate_source,
            target_name: item.interpolate_target,
        })
        .collect();
    let compute = match &params.compute_weights {
        None => None,
        Some(value) => {
            let owner = match value.computed_owner {
                attribute::Owner::Point => attribute_interpolate::WeightedSource::Points,
                attribute::Owner::Vertex => attribute_interpolate::WeightedSource::Vertices,
                attribute::Owner::Primitive | attribute::Owner::Detail => {
                    return Err(make_error(
                        OP,
                        "invalid_interpolate",
                        "Attribute Interpolate: computed weights require point or vertex ownership",
                    ))
                }
            };
            Some(attribute_interpolate::Computed {
                computed_owner: owner,
                computed_numbers_attribute: value.computed_numbers_attribute.clone(),
                computed_weights_attribute: value.computed_weights_attribute.clone(),
            })
        }
    };
    let weighted = |weighted_owner: attribute_interpolate::WeightedSource,
                    numbers_attribute: &str,
                    weights_attribute: &str|
     -> Result<geometry::T, error::T> {
        if compute.is_some() {
            return Err(make_error(
                OP,
                "invalid_interpolate",
                "Attribute Interpolate: computed weights require primitive/UVW mode",
            ));
        }
        attribute_interpolate::interpolate_weighted(&attribute_interpolate::WeightedParams {
            cancel: params.cancel.clone(),
            grain: params.grain,
            selection: params.selection.clone(),
            numbers_attribute: Some(numbers_attribute.to_owned()),
            weights_attribute: Some(weights_attribute.to_owned()),
            pre_scale: params.pre_scale,
            normalize_weights: params.normalize_weights,
            threshold: params.threshold,
            blend: params.blend,
            unmatched: Some(unmatched),
            weighted_owner,
            target_owner: params.target_owner,
            attributes: specs.clone(),
            groups: groups.clone(),
            source: params.source.clone(),
            target: params.target.clone(),
        })
        .map_err(|failure| invalid_interpolate(&failure))
    };
    match &driver {
        InterpolateDriver::PrimitiveUvw { primitive_attribute, uvw_attribute } => {
            attribute_interpolate::interpolate_primitive(&attribute_interpolate::PrimitiveParams {
                cancel: params.cancel.clone(),
                grain: params.grain,
                selection: params.selection.clone(),
                compute,
                primitive_attribute: Some(primitive_attribute.clone()),
                uvw_attribute: Some(uvw_attribute.clone()),
                pre_scale: params.pre_scale,
                blend: params.blend,
                unmatched: Some(unmatched),
                target_owner: params.target_owner,
                attributes: specs,
                groups,
                source: params.source.clone(),
                target: params.target.clone(),
            })
            .map_err(|failure| invalid_interpolate(&failure))
        }
        InterpolateDriver::PointWeights { numbers_attribute, weights_attribute } => {
            weighted(attribute_interpolate::WeightedSource::Points, numbers_attribute, weights_attribute)
        }
        InterpolateDriver::VertexWeights { numbers_attribute, weights_attribute } => {
            weighted(attribute_interpolate::WeightedSource::Vertices, numbers_attribute, weights_attribute)
        }
        InterpolateDriver::PrimitiveWeights { numbers_attribute, weights_attribute } => {
            weighted(attribute_interpolate::WeightedSource::Primitives, numbers_attribute, weights_attribute)
        }
    }
}

pub fn transfer_points(params: &attribute_transfer::SpatialParams) -> Result<geometry::T, error::T> {
    attribute_transfer::transfer_points(params)
}

pub fn transfer_primitives(params: &attribute_transfer::SpatialParams) -> Result<geometry::T, error::T> {
    attribute_transfer::transfer_primitives(params)
}

pub fn transfer_detail(params: &attribute_transfer::DetailParams) -> Result<geometry::T, error::T> {
    attribute_transfer::transfer_detail(params)
}

pub fn transfer_surface(params: &attribute_transfer::SurfaceParams) -> Result<geometry::T, error::T> {
    attribute_transfer::transfer_surface(params)
}

pub fn transfer_vertices(params: &attribute_transfer::VerticesParams) -> Result<geometry::T, error::T> {
    attribute_transfer::transfer_vertices(params)
}

pub fn transfer_all(params: &attribute_transfer::AllParams) -> Result<geometry::T, error::T> {
    attribute_transfer::transfer_all(params)
}

/// Test fixtures shared by the attribute and group tests of this crate (OCaml `Rdk_test_support`
/// helpers, reduced to what those tests use).
#[cfg(test)]
pub(crate) mod fixtures {
    use std::sync::{Arc, Mutex};

    use rdk_core::{attribute, error, geometry, group, packed, topology};

    pub fn points(values: &[(f64, f64, f64)]) -> geometry::T {
        crate::line_geometry::points(values).unwrap()
    }

    /// Geometry over `positions` whose primitives are `(kind, points)`.
    pub fn mesh(
        positions: &[(f64, f64, f64)],
        primitives: &[(topology::PrimitiveKind, Vec<i64>)],
    ) -> geometry::T {
        let mut vertex_points: Vec<i64> = Vec::new();
        let mut offsets = vec![0_i64];
        let mut kinds = Vec::new();
        for (kind, indices) in primitives {
            vertex_points.extend(indices);
            offsets.push(vertex_points.len() as i64);
            kinds.push(*kind);
        }
        let topology_value =
            topology::create_owned(positions.len() as i64, vertex_points, offsets, kinds).unwrap();
        let x: Vec<f64> = positions.iter().map(|p| p.0).collect();
        let y: Vec<f64> = positions.iter().map(|p| p.1).collect();
        let z: Vec<f64> = positions.iter().map(|p| p.2).collect();
        geometry::create(
            packed::Float3::View::of_owned_exn(x, y, z).unwrap(),
            topology_value,
            None,
            None,
            None,
        )
        .unwrap()
    }

    pub fn polygons(positions: &[(f64, f64, f64)], faces: &[Vec<i64>]) -> geometry::T {
        let primitives: Vec<(topology::PrimitiveKind, Vec<i64>)> =
            faces.iter().map(|face| (topology::PrimitiveKind::Polygon, face.clone())).collect();
        mesh(positions, &primitives)
    }

    /// A `columns` x `rows` grid of unit-size triangle pairs in the XZ plane.
    pub fn grid(columns: i64, rows: i64) -> geometry::T {
        let mut positions = Vec::new();
        for row in 0..=rows {
            for column in 0..=columns {
                positions.push((column as f64 / columns as f64 - 0.5, 0.0, row as f64 / rows as f64 - 0.5));
            }
        }
        let stride = columns + 1;
        let mut faces = Vec::new();
        for row in 0..rows {
            for column in 0..columns {
                let a = row * stride + column;
                faces.push(vec![a, a + 1, a + stride + 1]);
                faces.push(vec![a, a + stride + 1, a + stride]);
            }
        }
        polygons(&positions, &faces)
    }

    pub fn owner_count(geometry_value: &geometry::T, owner: attribute::Owner) -> i64 {
        match owner {
            attribute::Owner::Point => geometry::point_count(geometry_value),
            attribute::Owner::Vertex => geometry::vertex_count(geometry_value),
            attribute::Owner::Primitive => geometry::primitive_count(geometry_value),
            attribute::Owner::Detail => 1,
        }
    }

    pub fn add_storage(
        owner: attribute::Owner,
        name: &str,
        storage: attribute::Storage,
        geometry_value: &geometry::T,
    ) -> geometry::T {
        let item = attribute::create_owned(name, owner, storage).unwrap();
        geometry::with_attribute(item, geometry_value).unwrap()
    }

    pub fn add_float(owner: attribute::Owner, name: &str, seed: f64, geometry_value: &geometry::T) -> geometry::T {
        let count = owner_count(geometry_value, owner);
        let values: Vec<f64> = (0..count).map(|index| seed + index as f64).collect();
        add_storage(owner, name, attribute::Storage::Float(Arc::new(values)), geometry_value)
    }

    pub fn find(owner: attribute::Owner, name: &str, geometry_value: &geometry::T) -> attribute::T {
        geometry::find_attribute(owner, name, geometry_value)
            .unwrap_or_else(|| panic!("missing attribute {name}"))
    }

    pub fn has(owner: attribute::Owner, name: &str, geometry_value: &geometry::T) -> bool {
        geometry::find_attribute(owner, name, geometry_value).is_some()
    }

    pub fn floats(owner: attribute::Owner, name: &str, geometry_value: &geometry::T) -> Vec<f64> {
        match attribute::storage(&find(owner, name, geometry_value)).unwrap() {
            attribute::Storage::Float(values) => values.to_vec(),
            _ => panic!("wrong float storage for {name}"),
        }
    }

    pub fn ints(owner: attribute::Owner, name: &str, geometry_value: &geometry::T) -> Vec<i64> {
        match attribute::storage(&find(owner, name, geometry_value)).unwrap() {
            attribute::Storage::Int(values) => values.to_vec(),
            _ => panic!("wrong int storage for {name}"),
        }
    }

    pub fn texts(owner: attribute::Owner, name: &str, geometry_value: &geometry::T) -> Vec<Vec<u8>> {
        match attribute::storage(&find(owner, name, geometry_value)).unwrap() {
            attribute::Storage::Text(values) => values,
            _ => panic!("wrong text storage for {name}"),
        }
    }

    /// The three position planes (or any float3 attribute) of a geometry.
    pub fn float3(owner: attribute::Owner, name: &str, geometry_value: &geometry::T) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
        match attribute::storage(&find(owner, name, geometry_value)).unwrap() {
            attribute::Storage::Float3(values) => {
                let view = packed::Float3::View::view(&values).unwrap();
                (view.x.to_vec(), view.y.to_vec(), view.z.to_vec())
            }
            _ => panic!("wrong float3 storage for {name}"),
        }
    }

    pub fn positions(geometry_value: &geometry::T) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
        let view = packed::Float3::View::view(geometry::positions(geometry_value)).unwrap();
        (view.x.to_vec(), view.y.to_vec(), view.z.to_vec())
    }

    pub fn names(geometry_value: &geometry::T) -> Vec<(attribute::Owner, String)> {
        geometry::attributes(geometry_value)
            .iter()
            .map(|item| (attribute::owner(item), attribute::name(item).to_owned()))
            .collect()
    }

    pub fn group_of(owner: group::Owner, name: &str, members: &[i64], count: i64) -> group::T {
        group::init(Some(1), owner, name, count, &|index| members.contains(&index)).unwrap()
    }

    pub fn with_group(group_value: group::T, geometry_value: &geometry::T) -> geometry::T {
        geometry::with_group(group_value, geometry_value).unwrap()
    }

    pub fn err_code(result: Result<geometry::T, error::T>) -> String {
        match result {
            Ok(_) => "ok".to_owned(),
            Err(failure) => error::code(&failure).to_owned(),
        }
    }

    pub fn code(failure: &error::T) -> String {
        error::code(failure).to_owned()
    }

    /// Runs `work` on a pool of `domains` domains and returns its geometry.
    pub fn on_domains(domains: i64, work: &(dyn Fn() -> geometry::T + Sync)) -> geometry::T {
        let slot: Mutex<Option<geometry::T>> = Mutex::new(None);
        parallel::run_unit(Some(domains), &|| {
            *slot.lock().unwrap() = Some(work());
            Ok(())
        })
        .unwrap();
        slot.into_inner().unwrap().unwrap()
    }
}

#[cfg(test)]
pub(crate) mod fixtures2 {
    use std::sync::Arc;

    use rdk_core::{attribute, geometry, packed};

    pub fn add_values(owner: attribute::Owner, name: &str, values: Vec<f64>, g: &geometry::T) -> geometry::T {
        super::fixtures::add_storage(owner, name, attribute::Storage::Float(Arc::new(values)), g)
    }

    pub fn add_ints(owner: attribute::Owner, name: &str, values: Vec<i64>, g: &geometry::T) -> geometry::T {
        super::fixtures::add_storage(owner, name, attribute::Storage::Int(Arc::new(values)), g)
    }

    pub fn add_float3s(
        owner: attribute::Owner,
        name: &str,
        x: Vec<f64>,
        y: Vec<f64>,
        z: Vec<f64>,
        g: &geometry::T,
    ) -> geometry::T {
        super::fixtures::add_storage(
            owner,
            name,
            attribute::Storage::Float3(packed::Float3::of_owned(x, y, z).unwrap()),
            g,
        )
    }

    /// A triangle (0,0) (2,0) (0,2) at height `z`.
    pub fn triangle(z: f64) -> geometry::T {
        super::fixtures::polygons(&[(0.0, 0.0, z), (2.0, 0.0, z), (0.0, 2.0, z)], &[vec![0, 1, 2]])
    }

    pub fn near(left: f64, right: f64) -> bool {
        (left - right).abs() <= 1e-12
    }

    pub fn near_all(expected: &[f64], actual: &[f64]) -> bool {
        expected.len() == actual.len() && expected.iter().zip(actual).all(|(a, b)| near(*a, *b))
    }

    /// The shared byte-exact geometry oracle, including topology and ordered/edge groups.
    pub fn signature(g: &geometry::T) -> String {
        crate::compact_points::fixtures::geometry_signature(g)
    }
}
