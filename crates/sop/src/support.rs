//! Shared SOP declaration helpers retain user-visible choice row order.
use param::{Kind, Value};
use rays_diagnostic::Diagnostic;
use rdk_core::{attribute, geometry, group};
use rdk_ops::group_ops;

use crate::node::GeometryCooked;

// ---------------------------------------------------------------- choice helpers

/// A choice parameter: the labels the user sees, each with the value it selects, in declaration order.
pub struct Choice<T: 'static>(pub &'static [(&'static str, T)]);

impl<T: Clone> Choice<T> {
    pub fn kind(&self) -> Kind {
        param::choice(self.0.iter().map(|(label, _)| (*label).to_owned()).collect()).expect("choice table labels are distinct")
    }

    /// The selected row's value. The schema validated the choice already.
    pub fn of(&self, value: &Value) -> T {
        match value {
            Value::Choice(label) => self.0.iter().find(|(item, _)| item == label).expect("choice label in table").1.clone(),
            _ => unreachable!("choice parameter received a non-choice value"),
        }
    }

    /// The choice value at table position `index` (the declared defaults).
    pub fn at(&self, index: usize) -> Value {
        Value::Choice(self.0[index].0.to_owned())
    }

    pub fn value(&self, item: &T) -> Value
    where
        T: PartialEq,
    {
        Value::Choice(self.0.iter().find(|(_, value)| value == item).expect("value in choice table").0.to_owned())
    }
}

// ---------------------------------------------------------------- diagnostics and results

pub fn diagnostic(code: &str, message: &str) -> Diagnostic {
    rays_diagnostic::error(code.to_owned(), message.to_owned(), None, None, None, Vec::new(), Vec::new())
}

/// `Diagnostic.error ~code ~cause ~hints message`.
pub fn diagnostic_with(code: &str, message: &str, cause: Option<String>, hints: Vec<String>) -> Diagnostic {
    rays_diagnostic::error(code.to_owned(), message.to_owned(), None, None, cause.map(String::into_bytes), hints, Vec::new())
}

pub fn cooked(geometry: geometry::T) -> Result<GeometryCooked, Diagnostic> {
    Ok(GeometryCooked { geometry, instances: None })
}

pub fn rdk_error(hints: Vec<String>, operation: &str, message: &str) -> Diagnostic {
    diagnostic_with(
        &format!("{operation}_failed"),
        &format!("{operation} could not produce valid geometry"),
        Some(message.to_owned()),
        hints,
    )
}

pub fn structured_rdk_error(error: &rdk_core::error::T) -> Diagnostic {
    diagnostic_with(
        rdk_core::error::code(error).as_str(),
        &format!("{} could not produce valid geometry", rdk_core::error::operation(error)),
        Some(rdk_core::error::to_string(error)),
        Vec::new(),
    )
}

fn missing_group(message: String) -> Diagnostic {
    diagnostic("missing_group", &message)
}

// ---------------------------------------------------------------- small helpers

pub fn optional_text(value: &str) -> Option<String> {
    let value = value.trim();
    if value.is_empty() { None } else { Some(value.to_owned()) }
}

pub fn exact_or_pattern(exact: &str, pattern: &str) -> (Option<String>, Option<String>) {
    match optional_text(pattern) {
        Some(pattern) => (None, Some(pattern)),
        None => (optional_text(exact), None),
    }
}

pub fn vec3_copy(value: &rays_math::vec3::T) -> rays_math::vec3::T {
    *value
}

pub fn encode_table(rows: &[Vec<Vec<u8>>]) -> Vec<u8> {
    param::string_table::encode(rows)
}

pub fn decode_table(text: &[u8]) -> Result<Vec<Vec<String>>, String> {
    Ok(param::string_table::decode(text)?
        .into_iter()
        .map(|row| row.into_iter().map(|cell| String::from_utf8_lossy(&cell).into_owned()).collect())
        .collect())
}

pub fn parameter_table_rows(name: &str, text: &[u8]) -> Result<Vec<Vec<String>>, String> {
    decode_table(text).map_err(|message| format!("{name}: {message}"))
}

// ---------------------------------------------------------------- plain enums

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CleanOverlap {
    KeepFirst,
    DeletePairs,
    Auto,
}
/// A named group of one element kind, as picked by a node's `group_owner` / `group` parameters.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ElementGroup {
    pub owner: GroupOpsOwner,
    pub name: String,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MirrorMethod {
    Plane,
    Mapping,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MirrorTransform {
    Copy,
    Uv,
    Vector,
    Point,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PolyCutDetection {
    All,
    Crossing,
    Change,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PolyReduceTarget {
    Ratio,
    PrimitiveCount,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SoftTransformMetric {
    Radius,
    Edge,
    Attribute,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RayDirection {
    Vector,
    Normal,
    Attribute,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PolyBevelShape {
    Chamfer,
    Round,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FuseTargeting {
    NearPoints,
    SpecifiedPoints,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SubdivisionCracks {
    DoNotClose,
    PullNoDivision,
    PullDivide,
    PullTriangulate,
    StitchNoDivision,
    StitchDivide,
    StitchTriangulate,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BooleanClosedPolicy {
    Default,
    Required,
    NotRequired,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Triangulate2dProjection {
    BestFit,
    Xy,
    Yz,
    Zx,
    Plane,
    Attribute,
}

// ---------------------------------------------------------------- choice tables

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KernelMode {
    Explicit,
    Auto,
}
pub const KERNEL_MODE: Choice<KernelMode> = Choice(&[("Explicit", KernelMode::Explicit), ("Auto", KernelMode::Auto)]);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DistanceRadiusMode {
    Fixed,
    Maximum,
}
pub const DISTANCE_RADIUS: Choice<DistanceRadiusMode> =
    Choice(&[("Fixed", DistanceRadiusMode::Fixed), ("Maximum distance", DistanceRadiusMode::Maximum)]);

use group_ops::{CopyConflict, Owner as GroupOpsOwner};

/// Element selections name their owner singular (`ELEMENT_OWNER`), group-transfer rules plural.
pub const ELEMENT_OWNER: Choice<GroupOpsOwner> = Choice(&[
    ("Point", GroupOpsOwner::GroupPoints),
    ("Vertex", GroupOpsOwner::GroupVertices),
    ("Primitive", GroupOpsOwner::GroupPrimitives),
    ("Edge", GroupOpsOwner::GroupEdges),
]);
pub const GROUP_OWNER: Choice<GroupOpsOwner> = Choice(&[
    ("Points", GroupOpsOwner::GroupPoints),
    ("Vertices", GroupOpsOwner::GroupVertices),
    ("Primitives", GroupOpsOwner::GroupPrimitives),
    ("Edges", GroupOpsOwner::GroupEdges),
]);

pub const GROUP_COPY_CONFLICT: Choice<CopyConflict> = Choice(&[
    ("Skip", CopyConflict::CopySkip),
    ("Overwrite", CopyConflict::CopyOverwrite),
    ("Add suffix", CopyConflict::CopyAddSuffix),
]);

pub const ATTRIBUTE_OWNER: Choice<attribute::Owner> = Choice(&[
    ("Point", attribute::Owner::Point),
    ("Vertex", attribute::Owner::Vertex),
    ("Primitive", attribute::Owner::Primitive),
    ("Detail", attribute::Owner::Detail),
]);
/// Point, Vertex, Primitive only (the first three of `ATTRIBUTE_OWNER`).
pub const ELEMENT_ATTRIBUTE_OWNER: Choice<attribute::Owner> = Choice(&[
    ("Point", attribute::Owner::Point),
    ("Vertex", attribute::Owner::Vertex),
    ("Primitive", attribute::Owner::Primitive),
]);
pub const SOFT_FALLOFF: Choice<rdk_ops::transform_ops::SoftTransformFalloff> = Choice(&[
    ("Linear", rdk_ops::transform_ops::SoftTransformFalloff::SoftLinear),
    ("Quadratic", rdk_ops::transform_ops::SoftTransformFalloff::SoftQuadratic),
    ("Cubic", rdk_ops::transform_ops::SoftTransformFalloff::SoftCubic),
]);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FacetConsolidation {
    None,
    Points,
    Normals,
}
pub const FACET_CONSOLIDATION: Choice<FacetConsolidation> = Choice(&[
    ("None", FacetConsolidation::None),
    ("Points", FacetConsolidation::Points),
    ("Normals", FacetConsolidation::Normals),
]);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TransferMode {
    Nearest,
    Inverse,
    Links,
    Renderman,
    Hart,
}
pub const TRANSFER_MODE: Choice<TransferMode> = Choice(&[
    ("Nearest", TransferMode::Nearest),
    ("Inverse distance", TransferMode::Inverse),
    ("Links kernel", TransferMode::Links),
    ("RenderMan kernel", TransferMode::Renderman),
    ("Hart kernel", TransferMode::Hart),
]);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TransferFalloff {
    Linear,
    Smoothstep,
    Uniform,
}
pub const TRANSFER_FALLOFF: Choice<TransferFalloff> = Choice(&[
    ("Linear", TransferFalloff::Linear),
    ("Smoothstep", TransferFalloff::Smoothstep),
    ("Uniform", TransferFalloff::Uniform),
]);

pub const TRANSFER_UNMATCHED: Choice<rdk_ops::attribute_transfer::Unmatched> = Choice(&[
    ("Keep target", rdk_ops::attribute_transfer::Unmatched::KeepTarget),
    ("Default value", rdk_ops::attribute_transfer::Unmatched::DefaultValue),
]);

/// `Support.transfer_mode mode neighbors power radius` (the function, not the type).
pub fn transfer_mode_of(mode: TransferMode, neighbors: i64, power: f64, radius: f64) -> rdk_ops::attribute_transfer::Mode {
    use rdk_ops::attribute_transfer::{Kernel, Mode};
    match mode {
        TransferMode::Nearest => Mode::Nearest,
        TransferMode::Inverse => Mode::InverseDistance { neighbors, power },
        TransferMode::Links => Mode::Kernel { neighbors, radius, kernel: Kernel::Links },
        TransferMode::Renderman => Mode::Kernel { neighbors, radius, kernel: Kernel::RenderMan },
        TransferMode::Hart => Mode::Kernel { neighbors, radius, kernel: Kernel::Hart },
    }
}

pub fn transfer_falloff_of(falloff: TransferFalloff, bias: f64) -> rdk_ops::attribute_transfer::Falloff {
    use rdk_ops::attribute_transfer::Falloff;
    match falloff {
        TransferFalloff::Linear => Falloff::Linear,
        TransferFalloff::Smoothstep => Falloff::Smoothstep,
        TransferFalloff::Uniform => Falloff::Uniform(bias),
    }
}

// ---------------------------------------------------------------- tokens

/// `(singular, plural, owner)`: the spellings an encoded table accepts; the singular is what it writes.
pub(crate) const GROUP_OWNER_TOKENS: [(&str, &str, GroupOpsOwner); 4] = [
    ("point", "points", GroupOpsOwner::GroupPoints),
    ("vertex", "vertices", GroupOpsOwner::GroupVertices),
    ("primitive", "primitives", GroupOpsOwner::GroupPrimitives),
    ("edge", "edges", GroupOpsOwner::GroupEdges),
];
const ATTRIBUTE_OWNER_TOKENS: [(&str, &str, attribute::Owner); 4] = [
    ("point", "points", attribute::Owner::Point),
    ("vertex", "vertices", attribute::Owner::Vertex),
    ("primitive", "primitives", attribute::Owner::Primitive),
    ("detail", "detail", attribute::Owner::Detail),
];

fn owner_of_token<T: Copy>(table: &[(&str, &str, T)], what: &str, token: &str) -> Result<T, String> {
    table
        .iter()
        .find(|(one, many, _)| token == *one || token == *many)
        .map(|row| row.2)
        .ok_or_else(|| format!("unknown {what} owner {}", rdk_core::attribute::quote(token)))
}

fn attribute_owner_of_token(token: &str) -> Result<attribute::Owner, String> {
    owner_of_token(&ATTRIBUTE_OWNER_TOKENS, "attribute", token)
}

pub fn group_owner_token(owner: GroupOpsOwner) -> &'static str {
    GROUP_OWNER_TOKENS.iter().find(|row| row.2 == owner).expect("every group owner has a token").0
}

pub fn group_owner_of_token(token: &str) -> Result<GroupOpsOwner, String> {
    owner_of_token(&GROUP_OWNER_TOKENS, "group", token)
}

// ---------------------------------------------------------------- encoded table parameters

pub const FUSE_ATTRIBUTE_METHODS: [(&str, rdk_ops::fuse_rules::AttributeMethod); 16] = {
    use rdk_ops::fuse_rules::AttributeMethod::*;
    [
        ("average", Average),
        ("least", LeastPoint),
        ("greatest", GreatestPoint),
        ("maximum", Maximum),
        ("minimum", Minimum),
        ("mode", Mode),
        ("median", Median),
        ("sum", Sum),
        ("sum_squares", SumSquares),
        ("root_mean_square", RootMeanSquare),
        ("concatenate", Concatenate),
        ("weighted_average", WeightedAverage),
        ("weighted_sum", WeightedSum),
        ("minimum_weight", MinimumWeight),
        ("maximum_weight", MaximumWeight),
        ("concatenate_weight_order", ConcatenateWeightOrder),
    ]
};

pub const FUSE_GROUP_METHODS: [(&str, rdk_ops::fuse_rules::GroupMethod); 5] = {
    use rdk_ops::fuse_rules::GroupMethod::*;
    [("least", LeastPoint), ("greatest", GreatestPoint), ("union", Union), ("intersection", Intersection), ("most_common", MostCommon)]
};

pub fn validate_parameter_pattern(name: &str, pattern: &str) -> Result<(), String> {
    rdk_core::attribute_pattern::compile(pattern)
        .map(|_| ())
        .map_err(|e| format!("{name}: {}", rdk_core::error::message(&e)))
}

pub fn decode_fuse_attribute_rules(name: &str, text: &[u8]) -> Result<Vec<rdk_ops::fuse_rules::AttributeRule>, String> {
    use rdk_ops::fuse_rules::AttributeMethod::*;
    parameter_table_rows(name, text)?
        .into_iter()
        .map(|row| match row.as_slice() {
            [pattern, method, weight] => {
                validate_parameter_pattern(name, pattern)?;
                let method = FUSE_ATTRIBUTE_METHODS
                    .iter()
                    .find(|(token, _)| token == method)
                    .map(|(_, m)| *m)
                    .ok_or_else(|| format!("{name}: unknown reduction method {method}"))?;
                let weight_attribute = optional_text(weight);
                let needs_weight = matches!(
                    method,
                    WeightedAverage | WeightedSum | MinimumWeight | MaximumWeight | ConcatenateWeightOrder
                );
                if needs_weight && weight_attribute.is_none() {
                    Err(format!("{name}: weighted reduction requires a weight attribute"))
                } else {
                    Ok(rdk_ops::fuse_rules::AttributeRule { pattern: pattern.clone(), method, weight_attribute })
                }
            }
            _ => Err(format!("{name}: attribute rule needs pattern, method and weight columns")),
        })
        .collect()
}

pub fn decode_fuse_group_rules(name: &str, text: &[u8]) -> Result<Vec<rdk_ops::fuse_rules::GroupRule>, String> {
    parameter_table_rows(name, text)?
        .into_iter()
        .map(|row| match row.as_slice() {
            [group_pattern, method] => {
                validate_parameter_pattern(name, group_pattern)?;
                let group_method = FUSE_GROUP_METHODS
                    .iter()
                    .find(|(token, _)| token == method)
                    .map(|(_, m)| *m)
                    .ok_or_else(|| format!("{name}: unknown reduction method {method}"))?;
                Ok(rdk_ops::fuse_rules::GroupRule { group_pattern: group_pattern.clone(), group_method })
            }
            _ => Err(format!("{name}: group rule needs pattern and method columns")),
        })
        .collect()
}

pub fn decode_curve_join_picks(text: &str) -> Result<Option<Vec<rdk_ops::curve_join::CurveJoinPick>>, String> {
    use rdk_ops::curve_join::{CurveJoinEnd, CurveJoinPick};
    if text.trim().is_empty() {
        return Ok(None);
    }
    if text.trim() == "[]" {
        return Ok(Some(Vec::new()));
    }
    let mut seen = std::collections::HashSet::new();
    let mut picks = Vec::new();
    for token in text.split(',') {
        let parts: Vec<&str> = token.trim().split(':').collect();
        let [primitive, end] = parts.as_slice() else {
            return Err("sop/join_curves: picks must be comma-separated primitive:start/end pairs".to_owned());
        };
        let primitive = match primitive.trim().parse::<i64>() {
            Ok(value) if value >= 0 => value,
            _ => return Err("sop/join_curves: picked primitive must be nonnegative".to_owned()),
        };
        if !seen.insert(primitive) {
            return Err("sop/join_curves: duplicate picked primitive".to_owned());
        }
        let end_ = match end.trim() {
            "start" => CurveJoinEnd::JoinCurveStart,
            "end" => CurveJoinEnd::JoinCurveEnd,
            _ => return Err("sop/join_curves: picked end must be start or end".to_owned()),
        };
        picks.push(CurveJoinPick { primitive, end_ });
    }
    Ok(Some(picks))
}

pub fn decode_surface_attributes(text: &[u8]) -> Result<Vec<rdk_ops::attribute_transfer::SurfaceAttribute>, String> {
    parameter_table_rows("sop/attribute_transfer_surface", text)?
        .into_iter()
        .map(|row| match row.as_slice() {
            [owner, source_name, target_name] => {
                let source_owner = attribute_owner_of_token(&owner.trim().to_ascii_lowercase())
                    .map_err(|m| format!("sop/attribute_transfer_surface: {m}"))?;
                if source_owner == attribute::Owner::Detail {
                    Err("sop/attribute_transfer_surface: detail source is not spatial".to_owned())
                } else if source_name.trim().is_empty() || target_name.trim().is_empty() {
                    Err("sop/attribute_transfer_surface: attribute names must be nonblank".to_owned())
                } else if source_owner == attribute::Owner::Point && source_name == "P" {
                    Err("sop/attribute_transfer_surface: source P is not an ordinary attribute".to_owned())
                } else {
                    Ok(rdk_ops::attribute_transfer::SurfaceAttribute {
                        source_owner,
                        source_name: source_name.clone(),
                        target_name: target_name.clone(),
                    })
                }
            }
            _ => Err("sop/attribute_transfer_surface: attributes need owner, source and target columns".to_owned()),
        })
        .collect()
}

// ---------------------------------------------------------------- groups and attributes

pub fn optional_element_group(owner: GroupOpsOwner, name: &str) -> Option<ElementGroup> {
    optional_text(name).map(|name| ElementGroup { owner, name })
}

fn group_missing(operation: &str, what: &str, name: &str) -> Diagnostic {
    missing_group(format!("{operation} could not find {what} group {}", rdk_core::attribute::quote(name)))
}

pub fn resolve_element_group(
    operation: &str,
    selection: &Option<ElementGroup>,
    geometry_value: &geometry::T,
) -> Result<Option<rdk_core::element_selection::T>, Diagnostic> {
    use rdk_core::element_selection::T as Selected;
    let Some(ElementGroup { owner, name }) = selection else { return Ok(None) };
    let (what, found) = match owner {
        GroupOpsOwner::GroupPoints => ("point", geometry::find_group(group::Owner::Point, name, geometry_value).map(Selected::SelectedPoints)),
        GroupOpsOwner::GroupVertices => ("vertex", geometry::find_group(group::Owner::Vertex, name, geometry_value).map(Selected::SelectedVertices)),
        GroupOpsOwner::GroupPrimitives => ("primitive", geometry::find_group(group::Owner::Primitive, name, geometry_value).map(Selected::SelectedPrimitives)),
        GroupOpsOwner::GroupEdges => ("edge", geometry::find_edge_group(name, geometry_value).map(Selected::SelectedEdges)),
    };
    found.map(Some).ok_or_else(|| group_missing(operation, what, name))
}

pub fn resolve_optional_primitive_group(
    operation: &str,
    name: &Option<String>,
    geometry_value: &geometry::T,
) -> Result<Option<group::T>, Diagnostic> {
    match name {
        None => Ok(None),
        Some(name) => geometry::find_group(group::Owner::Primitive, name, geometry_value)
            .map(Some)
            .ok_or_else(|| group_missing(operation, "primitive", name)),
    }
}

pub fn resolve_optional_point_group(
    operation: &str,
    name: &Option<String>,
    geometry_value: &geometry::T,
) -> Result<Option<group::T>, Diagnostic> {
    match name {
        None => Ok(None),
        Some(name) => geometry::find_group(group::Owner::Point, name, geometry_value)
            .map(Some)
            .ok_or_else(|| group_missing(operation, "point", name)),
    }
}

pub fn resolve_optional_edge_group(
    operation: &str,
    name: &Option<String>,
    geometry_value: &geometry::T,
) -> Result<Option<rdk_core::edge_group::T>, Diagnostic> {
    match name {
        None => Ok(None),
        Some(name) => geometry::find_edge_group(name, geometry_value)
            .map(Some)
            .ok_or_else(|| group_missing(operation, "edge", name)),
    }
}

/// `(edge group, vertex group)` for a UV seam name: an edge group first, then a vertex group.
pub fn uv_parameterize_seams(
    operation: &str,
    input: &geometry::T,
    group_name: &Option<String>,
) -> Result<(Option<rdk_core::edge_group::T>, Option<group::T>), Diagnostic> {
    let Some(group_name) = group_name else { return Ok((None, None)) };
    if let Some(value) = geometry::find_edge_group(group_name, input) {
        return Ok((Some(value), None));
    }
    if let Some(value) = geometry::find_group(group::Owner::Vertex, group_name, input) {
        return Ok((None, Some(value)));
    }
    Err(diagnostic_with(
        "missing_group",
        &format!("{operation} could not find edge or vertex seam group {}", rdk_core::attribute::quote(group_name)),
        None,
        vec![format!("Create a native edge group or compatibility vertex-edge group before {operation}")],
    ))
}

fn attribute_group_owner(owner: attribute::Owner) -> Option<group::Owner> {
    match owner {
        attribute::Owner::Point => Some(group::Owner::Point),
        attribute::Owner::Vertex => Some(group::Owner::Vertex),
        attribute::Owner::Primitive => Some(group::Owner::Primitive),
        attribute::Owner::Detail => None,
    }
}

pub fn attribute_mirror_group_owner(owner: attribute::Owner) -> group::Owner {
    attribute_group_owner(owner).expect("Attribute Mirror: detail attributes cannot be mirrored")
}

fn group_length(owner: group::Owner, geometry_value: &geometry::T) -> i64 {
    match owner {
        group::Owner::Point => geometry::point_count(geometry_value),
        group::Owner::Vertex => geometry::vertex_count(geometry_value),
        group::Owner::Primitive => geometry::primitive_count(geometry_value),
    }
}

pub fn compile_transfer_group_pattern(
    operation: &str,
    label: &str,
    pattern: &Option<String>,
) -> Result<Option<rdk_core::attribute_pattern::T>, String> {
    match pattern {
        None => Ok(None),
        Some(pattern) => rdk_core::attribute_pattern::compile(pattern)
            .map(Some)
            .map_err(|e| format!("sop/{operation}: invalid {label} group pattern: {}", rdk_core::error::message(&e))),
    }
}

/// `resolve_transfer_group`: exactly one of `exact`/`pattern` may be set (both is a programming error).
#[allow(clippy::too_many_arguments)]
pub fn resolve_transfer_group(
    operation: &str,
    owner: group::Owner,
    owner_name: &str,
    exact: &Option<String>,
    pattern: &Option<rdk_core::attribute_pattern::T>,
    cancel_token: &rdk_core::cancel::T,
    grain: i64,
    geometry_value: &geometry::T,
) -> Result<Option<group::T>, Diagnostic> {
    match (exact, pattern) {
        (Some(_), Some(_)) => unreachable!("sop/{operation}: exact and patterned {owner_name} groups are mutually exclusive"),
        (Some(name), None) => geometry::find_group(owner, name, geometry_value)
            .map(Some)
            .ok_or_else(|| group_missing(operation, owner_name, name)),
        (None, None) => Ok(None),
        (None, Some(pattern)) => {
            // Rdk.Cancel.check raises; here a cancellation is a diagnostic.
            rdk_core::cancel::check(cancel_token).map_err(|e| structured_rdk_error(&e))?;
            let matches: Vec<group::T> = geometry::groups(geometry_value)
                .into_iter()
                .filter(|g| group::owner(g) == owner && rdk_core::attribute_pattern::matches(pattern, group::name(g)))
                .collect();
            let selection = match matches.len() {
                0 => group::init(
                    Some(grain),
                    owner,
                    "__attribute_transfer_empty",
                    group_length(owner, geometry_value),
                    &|_| Ok(false),
                ),
                1 => Ok(matches[0].clone()),
                _ => group::union_many(Some(cancel_token), Some(grain), "__attribute_transfer_union", &matches),
            };
            selection
                .map(Some)
                .map_err(|e| diagnostic("invalid_group", &format!("{operation}: {}", rdk_core::error::to_string(&e))))
        }
    }
}
