//! Port of lib/sop/support.ml: helpers shared by the node declarations and the hand-written SOPs.
//!
//! Choice parameters: every OCaml `Param.choice ~equal [ (label, v); ... ]` becomes a label table
//! `FOO_LABELS`, a value table `FOO` in the same order, `foo_kind()` and the pair `foo_of_value` /
//! `foo_value`. Enums with a table are `Copy` with discriminants equal to table positions.
use param::{Kind, Value};
use rays_diagnostic::Diagnostic;
use rdk::{attribute, geometry, group, group_ops};

use crate::node::GeometryCooked;

// ---------------------------------------------------------------- choice helpers

/// OCaml's `String.trim`: space, tab, newline, form feed and carriage return.
pub fn ocaml_trim(text: &str) -> &str {
    text.trim_matches(|c| matches!(c, ' ' | '\t' | '\n' | '\r' | '\x0c'))
}

pub fn labels(table: &[&str]) -> Vec<String> {
    table.iter().map(|label| (*label).to_owned()).collect()
}

pub fn choice_kind(table: &[&str]) -> Kind {
    param::choice(labels(table)).expect("choice table labels are distinct")
}

/// Position of a `Value::Choice` label in its table. The schema validated the value already.
pub fn choice_index(table: &[&str], value: &Value) -> usize {
    match value {
        Value::Choice(label) => table.iter().position(|item| item == label).expect("choice label in table"),
        _ => unreachable!("choice parameter received a non-choice value"),
    }
}

pub fn choice_value(table: &[&str], index: usize) -> Value {
    Value::Choice(table[index].to_owned())
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

pub fn structured_rdk_error(error: &rdk::error::T) -> Diagnostic {
    diagnostic_with(
        rdk::error::code(error),
        &format!("{} could not produce valid geometry", rdk::error::operation(error)),
        Some(rdk::error::to_string(error)),
        rdk::error::hints(error).to_vec(),
    )
}

pub fn missing_group(message: String) -> Diagnostic {
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

/// OCaml `int_of_string_opt`: optional sign, `_` separators, `0x`/`0o`/`0b` prefixes.
pub fn int_of_string_opt(text: &str) -> Option<i64> {
    let (negative, rest) = match text.strip_prefix('-') {
        Some(rest) => (true, rest),
        None => (false, text.strip_prefix('+').unwrap_or(text)),
    };
    let lower = rest.to_ascii_lowercase();
    let (radix, digits) = if let Some(d) = lower.strip_prefix("0x") {
        (16, d.to_owned())
    } else if let Some(d) = lower.strip_prefix("0o") {
        (8, d.to_owned())
    } else if let Some(d) = lower.strip_prefix("0b") {
        (2, d.to_owned())
    } else {
        (10, lower)
    };
    if digits.is_empty() || digits.starts_with('_') {
        return None;
    }
    let digits: String = digits.chars().filter(|c| *c != '_').collect();
    let magnitude = i128::from_str_radix(&digits, radix).ok()?;
    let value = if negative { -magnitude } else { magnitude };
    // OCaml int is 63-bit; hex/octal/binary literals may wrap into the negative half
    let (min, max) = (-(1_i128 << 62), (1_i128 << 62) - 1);
    if (min..=max).contains(&value) {
        Some(value as i64)
    } else if radix != 10 && !negative && value < (1_i128 << 63) {
        Some(((value as i64) << 1) >> 1)
    } else {
        None
    }
}

// ---------------------------------------------------------------- plain enums

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GroupInvertOwner {
    Any,
    Owner(group_ops::Owner),
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReverseOperation {
    Reverse,
    Shift,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SortKey {
    X,
    Y,
    Z,
    Distance,
    Vector,
    Attribute,
    VertexOrder,
    PrimitiveIndex,
    Spatial,
    Random,
    IndexAttribute,
    Reverse,
    Shift,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GroupBoundsShape {
    Box,
    Sphere,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UvProjection {
    Planar,
    Cylindrical,
    Spherical,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AxisOrientation {
    X,
    Y,
    Z,
    Custom,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CleanOverlap {
    KeepFirst,
    DeletePairs,
    Auto,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RemapRange {
    Automatic,
    Explicit,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConnectivityOutput {
    Integer,
    Text,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EnumerateStorage {
    Integer,
    Text,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SmoothingMode {
    Laplacian,
    Custom,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PolyframeStyle {
    FirstEdge,
    TwoEdges,
    Centroid,
    TextureUv,
    TextureUvGradient,
    AttributeGradient,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GroupRangeMode {
    StartEnd,
    FromEnds,
    StartLength,
    Partition,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GroupRangeConnectivity {
    NoConnectivity,
    Disconnected,
    Connected,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CentroidRunMode {
    Detail,
    Primitives,
    PointPieces,
    PrimitivePieces,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ElementGroup {
    Point(String),
    Vertex(String),
    Primitive(String),
    Edge(String),
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TargetJustifyMode {
    Explicit,
    Auto,
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
pub enum VelocityInitialization {
    Compute,
    Keep,
    Set,
    FromAttribute,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SpiralExtentMode {
    TurnsHeight,
    HeightPitch,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SpiralRadiusMode {
    ArchimedeanChange,
    ArchimedeanEnd,
    LogarithmicChange,
    LogarithmicEnd,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SpiralDivisionsMode {
    PerCurve,
    PerTurn,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CircleArc {
    Closed,
    Open,
    Chord,
    Sliced,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PlaneOrientation {
    Xy,
    Xz,
    Yz,
    Axes,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BlastAttributeMode {
    Below,
    Range,
    Width,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BlastAttributeOutput {
    Delete,
    Group,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExtractPointCut {
    Constant,
    PrimitiveAttribute,
    CurrentTime,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PointGenerationMode {
    Total,
    PerPoint,
    Probability,
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
pub enum CopyMatch {
    Cyclic,
    ByValues,
    ToElement,
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
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TransformMode {
    Trs,
    Matrix,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NoiseLocation {
    Position,
    ElementNumber,
    Attribute,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NoiseRange {
    Positive,
    ZeroCentered,
    MinMax,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RandomDistribution {
    Constant,
    TwoValues,
    Uniform,
    UniformDiscrete,
    Normal,
    Exponential,
    LogNormal,
    Cauchy,
    Direction,
    InsideSphere,
    InsideSphereCone,
    CustomRamp,
    CustomDiscrete,
    CustomDiscreteText,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InterpolateDriver {
    PrimitiveUvw,
    PointWeights,
    VertexWeights,
    PrimitiveWeights,
}

// ---------------------------------------------------------------- choice tables

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KernelMode {
    Explicit,
    Auto,
}
pub const KERNEL_MODE_LABELS: [&str; 2] = ["Explicit", "Auto"];
pub const KERNEL_MODE: [KernelMode; 2] = [KernelMode::Explicit, KernelMode::Auto];
pub fn kernel_mode_kind() -> Kind {
    choice_kind(&KERNEL_MODE_LABELS)
}
pub fn kernel_mode_of_value(value: &Value) -> KernelMode {
    KERNEL_MODE[choice_index(&KERNEL_MODE_LABELS, value)]
}
pub fn kernel_mode_value(mode: KernelMode) -> Value {
    choice_value(&KERNEL_MODE_LABELS, mode as usize)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumericKind {
    Scalar,
    Vec2,
    Vec3,
    Vec4,
}
pub const NUMERIC_KIND_LABELS: [&str; 4] = ["Scalar", "Vector 2", "Vector 3", "Vector 4"];
pub const NUMERIC_KIND: [NumericKind; 4] = [NumericKind::Scalar, NumericKind::Vec2, NumericKind::Vec3, NumericKind::Vec4];
pub fn numeric_kind_of_value(value: &Value) -> NumericKind {
    NUMERIC_KIND[choice_index(&NUMERIC_KIND_LABELS, value)]
}
pub fn numeric_kind_value(kind: NumericKind) -> Value {
    choice_value(&NUMERIC_KIND_LABELS, kind as usize)
}

pub fn numeric_value(kind: NumericKind, x: f64, y: f64, z: f64, w: f64) -> rdk::attribute_generate::NumericValue {
    use rdk::attribute_generate::NumericValue;
    match kind {
        NumericKind::Scalar => NumericValue::Scalar(x),
        NumericKind::Vec2 => NumericValue::Vec2(rays_math::vec2::create(x, y)),
        NumericKind::Vec3 => NumericValue::Vec3(rays_math::vec3::create(x, y, z)),
        NumericKind::Vec4 => NumericValue::Vec4(x, y, z, w),
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DistanceRadiusMode {
    Fixed,
    Maximum,
}
pub const DISTANCE_RADIUS_LABELS: [&str; 2] = ["Fixed", "Maximum distance"];
pub const DISTANCE_RADIUS: [DistanceRadiusMode; 2] = [DistanceRadiusMode::Fixed, DistanceRadiusMode::Maximum];
pub fn distance_radius_kind() -> Kind {
    choice_kind(&DISTANCE_RADIUS_LABELS)
}
pub fn distance_radius_of_value(value: &Value) -> DistanceRadiusMode {
    DISTANCE_RADIUS[choice_index(&DISTANCE_RADIUS_LABELS, value)]
}
pub fn distance_radius_value(mode: DistanceRadiusMode) -> Value {
    choice_value(&DISTANCE_RADIUS_LABELS, mode as usize)
}

use group_ops::{CopyConflict, Owner as GroupOpsOwner};

pub const ELEMENT_OWNER_LABELS: [&str; 4] = ["Point", "Vertex", "Primitive", "Edge"];
pub const GROUP_OWNER_LABELS: [&str; 4] = ["Points", "Vertices", "Primitives", "Edges"];
pub const GROUP_OWNERS: [GroupOpsOwner; 4] =
    [GroupOpsOwner::GroupPoints, GroupOpsOwner::GroupVertices, GroupOpsOwner::GroupPrimitives, GroupOpsOwner::GroupEdges];
pub fn element_owner_kind() -> Kind {
    choice_kind(&ELEMENT_OWNER_LABELS)
}
pub fn element_owner_of_value(value: &Value) -> GroupOpsOwner {
    GROUP_OWNERS[choice_index(&ELEMENT_OWNER_LABELS, value)]
}
pub fn element_owner_value(owner: GroupOpsOwner) -> Value {
    choice_value(&ELEMENT_OWNER_LABELS, group_owner_position(owner))
}
pub fn group_owner_of_value(value: &Value) -> GroupOpsOwner {
    GROUP_OWNERS[choice_index(&GROUP_OWNER_LABELS, value)]
}
pub fn group_owner_value(owner: GroupOpsOwner) -> Value {
    choice_value(&GROUP_OWNER_LABELS, group_owner_position(owner))
}
fn group_owner_position(owner: GroupOpsOwner) -> usize {
    GROUP_OWNERS.iter().position(|item| *item == owner).unwrap()
}

pub const GROUP_COPY_CONFLICT_LABELS: [&str; 3] = ["Skip", "Overwrite", "Add suffix"];
pub const GROUP_COPY_CONFLICT: [CopyConflict; 3] =
    [CopyConflict::CopySkip, CopyConflict::CopyOverwrite, CopyConflict::CopyAddSuffix];
pub fn group_copy_conflict_kind() -> Kind {
    choice_kind(&GROUP_COPY_CONFLICT_LABELS)
}
pub fn group_copy_conflict_of_value(value: &Value) -> CopyConflict {
    GROUP_COPY_CONFLICT[choice_index(&GROUP_COPY_CONFLICT_LABELS, value)]
}
pub fn group_copy_conflict_value(conflict: CopyConflict) -> Value {
    choice_value(&GROUP_COPY_CONFLICT_LABELS, GROUP_COPY_CONFLICT.iter().position(|item| *item == conflict).unwrap())
}

pub const ATTRIBUTE_OWNER_LABELS: [&str; 4] = ["Point", "Vertex", "Primitive", "Detail"];
pub const ATTRIBUTE_OWNERS: [attribute::Owner; 4] =
    [attribute::Owner::Point, attribute::Owner::Vertex, attribute::Owner::Primitive, attribute::Owner::Detail];
pub fn attribute_owner_kind() -> Kind {
    choice_kind(&ATTRIBUTE_OWNER_LABELS)
}
pub fn attribute_owner_of_value(value: &Value) -> attribute::Owner {
    ATTRIBUTE_OWNERS[choice_index(&ATTRIBUTE_OWNER_LABELS, value)]
}
pub fn attribute_owner_value(owner: attribute::Owner) -> Value {
    choice_value(&ATTRIBUTE_OWNER_LABELS, owner as usize)
}
/// Point, Vertex, Primitive only (the first three of `ATTRIBUTE_OWNER_LABELS`).
pub const ELEMENT_ATTRIBUTE_OWNER_LABELS: [&str; 3] = ["Point", "Vertex", "Primitive"];
pub fn element_attribute_owner_kind() -> Kind {
    choice_kind(&ELEMENT_ATTRIBUTE_OWNER_LABELS)
}
pub fn element_attribute_owner_of_value(value: &Value) -> attribute::Owner {
    ATTRIBUTE_OWNERS[choice_index(&ELEMENT_ATTRIBUTE_OWNER_LABELS, value)]
}
pub const SOFT_FALLOFF_LABELS: [&str; 3] = ["Linear", "Quadratic", "Cubic"];
pub fn soft_falloff_kind() -> Kind {
    choice_kind(&SOFT_FALLOFF_LABELS)
}
pub fn soft_falloff_of_value(value: &Value) -> rdk::transform_ops::SoftTransformFalloff {
    use rdk::transform_ops::SoftTransformFalloff::*;
    [SoftLinear, SoftQuadratic, SoftCubic][choice_index(&SOFT_FALLOFF_LABELS, value)]
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FacetConsolidation {
    None,
    Points,
    Normals,
}
pub const FACET_CONSOLIDATION_LABELS: [&str; 3] = ["None", "Points", "Normals"];
pub const FACET_CONSOLIDATION: [FacetConsolidation; 3] =
    [FacetConsolidation::None, FacetConsolidation::Points, FacetConsolidation::Normals];
pub fn facet_consolidation_kind() -> Kind {
    choice_kind(&FACET_CONSOLIDATION_LABELS)
}
pub fn facet_consolidation_of_value(value: &Value) -> FacetConsolidation {
    FACET_CONSOLIDATION[choice_index(&FACET_CONSOLIDATION_LABELS, value)]
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TransferMode {
    Nearest,
    Inverse,
    Links,
    Renderman,
    Hart,
}
pub const TRANSFER_MODE_LABELS: [&str; 5] =
    ["Nearest", "Inverse distance", "Links kernel", "RenderMan kernel", "Hart kernel"];
pub const TRANSFER_MODE: [TransferMode; 5] =
    [TransferMode::Nearest, TransferMode::Inverse, TransferMode::Links, TransferMode::Renderman, TransferMode::Hart];
pub fn transfer_mode_kind() -> Kind {
    choice_kind(&TRANSFER_MODE_LABELS)
}
pub fn transfer_mode_of_value(value: &Value) -> TransferMode {
    TRANSFER_MODE[choice_index(&TRANSFER_MODE_LABELS, value)]
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TransferFalloff {
    Linear,
    Smoothstep,
    Uniform,
}
pub const TRANSFER_FALLOFF_LABELS: [&str; 3] = ["Linear", "Smoothstep", "Uniform"];
pub const TRANSFER_FALLOFF: [TransferFalloff; 3] =
    [TransferFalloff::Linear, TransferFalloff::Smoothstep, TransferFalloff::Uniform];
pub fn transfer_falloff_kind() -> Kind {
    choice_kind(&TRANSFER_FALLOFF_LABELS)
}
pub fn transfer_falloff_of_value(value: &Value) -> TransferFalloff {
    TRANSFER_FALLOFF[choice_index(&TRANSFER_FALLOFF_LABELS, value)]
}

pub const TRANSFER_UNMATCHED_LABELS: [&str; 2] = ["Keep target", "Default value"];
pub fn transfer_unmatched_kind() -> Kind {
    choice_kind(&TRANSFER_UNMATCHED_LABELS)
}
pub fn transfer_unmatched_of_value(value: &Value) -> rdk::attribute_transfer::Unmatched {
    match choice_index(&TRANSFER_UNMATCHED_LABELS, value) {
        0 => rdk::attribute_transfer::Unmatched::KeepTarget,
        _ => rdk::attribute_transfer::Unmatched::DefaultValue,
    }
}

/// `Support.transfer_mode mode neighbors power radius` (the function, not the type).
pub fn transfer_mode_of(mode: TransferMode, neighbors: i64, power: f64, radius: f64) -> rdk::attribute_transfer::Mode {
    use rdk::attribute_transfer::{Kernel, Mode};
    match mode {
        TransferMode::Nearest => Mode::Nearest,
        TransferMode::Inverse => Mode::InverseDistance { neighbors, power },
        TransferMode::Links => Mode::Kernel { neighbors, radius, kernel: Kernel::Links },
        TransferMode::Renderman => Mode::Kernel { neighbors, radius, kernel: Kernel::RenderMan },
        TransferMode::Hart => Mode::Kernel { neighbors, radius, kernel: Kernel::Hart },
    }
}

pub fn transfer_falloff_of(falloff: TransferFalloff, bias: f64) -> rdk::attribute_transfer::Falloff {
    use rdk::attribute_transfer::Falloff;
    match falloff {
        TransferFalloff::Linear => Falloff::Linear,
        TransferFalloff::Smoothstep => Falloff::Smoothstep,
        TransferFalloff::Uniform => Falloff::Uniform(bias),
    }
}

// ---------------------------------------------------------------- tokens

fn attribute_owner_of_token(token: &str) -> Result<attribute::Owner, String> {
    match token {
        "point" | "points" => Ok(attribute::Owner::Point),
        "vertex" | "vertices" => Ok(attribute::Owner::Vertex),
        "primitive" | "primitives" => Ok(attribute::Owner::Primitive),
        "detail" => Ok(attribute::Owner::Detail),
        token => Err(format!("unknown attribute owner {}", crate::node::ocaml_quote(token))),
    }
}

pub fn group_owner_token(owner: GroupOpsOwner) -> &'static str {
    match owner {
        GroupOpsOwner::GroupPoints => "point",
        GroupOpsOwner::GroupVertices => "vertex",
        GroupOpsOwner::GroupPrimitives => "primitive",
        GroupOpsOwner::GroupEdges => "edge",
    }
}

pub fn group_owner_of_token(token: &str) -> Result<GroupOpsOwner, String> {
    match token {
        "point" | "points" => Ok(GroupOpsOwner::GroupPoints),
        "vertex" | "vertices" => Ok(GroupOpsOwner::GroupVertices),
        "primitive" | "primitives" => Ok(GroupOpsOwner::GroupPrimitives),
        "edge" | "edges" => Ok(GroupOpsOwner::GroupEdges),
        token => Err(format!("unknown group owner {}", crate::node::ocaml_quote(token))),
    }
}

// ---------------------------------------------------------------- encoded table parameters

pub const FUSE_ATTRIBUTE_METHODS: [(&str, rdk::fuse_rules::AttributeMethod); 16] = {
    use rdk::fuse_rules::AttributeMethod::*;
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

pub const FUSE_GROUP_METHODS: [(&str, rdk::fuse_rules::GroupMethod); 5] = {
    use rdk::fuse_rules::GroupMethod::*;
    [("least", LeastPoint), ("greatest", GreatestPoint), ("union", Union), ("intersection", Intersection), ("most_common", MostCommon)]
};

pub fn validate_parameter_pattern(name: &str, pattern: &str) -> Result<(), String> {
    rdk::attribute_pattern::compile(pattern)
        .map(|_| ())
        .map_err(|e| format!("{name}: {}", rdk::error::message(&e)))
}

pub fn decode_fuse_attribute_rules(name: &str, text: &[u8]) -> Result<Vec<rdk::fuse_rules::AttributeRule>, String> {
    use rdk::fuse_rules::AttributeMethod::*;
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
                    Ok(rdk::fuse_rules::AttributeRule { pattern: pattern.clone(), method, weight_attribute })
                }
            }
            _ => Err(format!("{name}: attribute rule needs pattern, method and weight columns")),
        })
        .collect()
}

pub fn decode_fuse_group_rules(name: &str, text: &[u8]) -> Result<Vec<rdk::fuse_rules::GroupRule>, String> {
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
                Ok(rdk::fuse_rules::GroupRule { group_pattern: group_pattern.clone(), group_method })
            }
            _ => Err(format!("{name}: group rule needs pattern and method columns")),
        })
        .collect()
}

pub fn decode_curve_join_picks(text: &str) -> Result<Option<Vec<rdk::curve_join::CurveJoinPick>>, String> {
    use rdk::curve_join::{CurveJoinEnd, CurveJoinPick};
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
        let primitive = match int_of_string_opt(primitive.trim()) {
            Some(value) if value >= 0 => value,
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

pub fn decode_surface_attributes(text: &[u8]) -> Result<Vec<rdk::attribute_transfer::SurfaceAttribute>, String> {
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
                    Ok(rdk::attribute_transfer::SurfaceAttribute {
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
    optional_text(name).map(|name| match owner {
        GroupOpsOwner::GroupPoints => ElementGroup::Point(name),
        GroupOpsOwner::GroupVertices => ElementGroup::Vertex(name),
        GroupOpsOwner::GroupPrimitives => ElementGroup::Primitive(name),
        GroupOpsOwner::GroupEdges => ElementGroup::Edge(name),
    })
}

fn group_missing(operation: &str, what: &str, name: &str) -> Diagnostic {
    missing_group(format!("{operation} could not find {what} group {}", crate::node::ocaml_quote(name)))
}

pub fn resolve_element_group(
    operation: &str,
    selection: &Option<ElementGroup>,
    geometry_value: &geometry::T,
) -> Result<Option<rdk::element_selection::T>, Diagnostic> {
    use rdk::element_selection::T as Selected;
    let Some(selection) = selection else { return Ok(None) };
    let found = match selection {
        ElementGroup::Point(name) => geometry::find_group(group::Owner::Point, name, geometry_value)
            .map(Selected::SelectedPoints)
            .ok_or_else(|| group_missing(operation, "point", name)),
        ElementGroup::Vertex(name) => geometry::find_group(group::Owner::Vertex, name, geometry_value)
            .map(Selected::SelectedVertices)
            .ok_or_else(|| group_missing(operation, "vertex", name)),
        ElementGroup::Primitive(name) => geometry::find_group(group::Owner::Primitive, name, geometry_value)
            .map(Selected::SelectedPrimitives)
            .ok_or_else(|| group_missing(operation, "primitive", name)),
        ElementGroup::Edge(name) => geometry::find_edge_group(name, geometry_value)
            .map(Selected::SelectedEdges)
            .ok_or_else(|| group_missing(operation, "edge", name)),
    };
    found.map(Some)
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
) -> Result<Option<rdk::edge_group::T>, Diagnostic> {
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
) -> Result<(Option<rdk::edge_group::T>, Option<group::T>), Diagnostic> {
    let Some(group_name) = group_name else { return Ok((None, None)) };
    if let Some(value) = geometry::find_edge_group(group_name, input) {
        return Ok((Some(value), None));
    }
    if let Some(value) = geometry::find_group(group::Owner::Vertex, group_name, input) {
        return Ok((None, Some(value)));
    }
    Err(diagnostic_with(
        "missing_group",
        &format!("{operation} could not find edge or vertex seam group {}", crate::node::ocaml_quote(group_name)),
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
) -> Result<Option<rdk::attribute_pattern::T>, String> {
    match pattern {
        None => Ok(None),
        Some(pattern) => rdk::attribute_pattern::compile(pattern)
            .map(Some)
            .map_err(|e| format!("sop/{operation}: invalid {label} group pattern: {}", rdk::error::message(&e))),
    }
}

/// `resolve_transfer_group`: exactly one of `exact`/`pattern` may be set (both is a programming error).
#[allow(clippy::too_many_arguments)]
pub fn resolve_transfer_group(
    operation: &str,
    owner: group::Owner,
    owner_name: &str,
    exact: &Option<String>,
    pattern: &Option<rdk::attribute_pattern::T>,
    cancel_token: &rdk::cancel::T,
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
            rdk::cancel::check(cancel_token).map_err(|e| structured_rdk_error(&e))?;
            let matches: Vec<group::T> = geometry::groups(geometry_value)
                .into_iter()
                .filter(|g| group::owner(g) == owner && rdk::attribute_pattern::matches(pattern, group::name(g)))
                .collect();
            let selection = match matches.len() {
                0 => group::init(
                    Some(grain),
                    owner,
                    "__attribute_transfer_empty",
                    group_length(owner, geometry_value),
                    &|_| false,
                ),
                1 => Ok(matches[0].clone()),
                _ => group::union_many(Some(cancel_token), Some(grain), "__attribute_transfer_union", &matches),
            };
            selection
                .map(Some)
                .map_err(|e| diagnostic("invalid_group", &format!("{operation}: {}", rdk::error::to_string(&e))))
        }
    }
}
