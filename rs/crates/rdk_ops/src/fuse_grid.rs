use std::sync::Arc;

use rdk_core::{attribute, cancel, error, geometry, group, packed};
use rdk_geom::point_clusters::{self, Clusters};
use rdk_geom::point_snap;

use crate::fuse_cleanup;
use crate::fuse_reduce::{self, Position};
use crate::fuse_rules::{self, AttributeRule, DefaultAttributes, GroupRule};
use crate::fuse_target_rules;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FuseMetric {
    Euclidean,
    Componentwise,
}

enum FuseAttributeView {
    Float(Vec<f64>),
    Int(Vec<i64>),
    Float2(Arc<[f64]>, Arc<[f64]>),
    Float3(Arc<[f64]>, Arc<[f64]>, Arc<[f64]>),
    Float4(Arc<[f64]>, Arc<[f64]>, Arc<[f64]>, Arc<[f64]>),
    IntArray(Arc<[i64]>, Arc<[i64]>),
    FloatArray(Arc<[i64]>, Arc<[f64]>),
    Text(Vec<String>),
}

fn make(operation: &str, code: &str, message: String) -> error::T {
    error::make(operation.to_owned(), code.to_owned(), message, None).unwrap_or_else(|e| e)
}

fn fail(message: &str) -> error::T {
    make("Rdk.Fuse_grid", "invalid_parameter", message.to_owned())
}

struct ClusterOptions {
    selection: Option<group::T>,
    tolerance: f64,
    position: Position,
    weight_attribute: Option<String>,
    attributes: DefaultAttributes,
    attribute_rules: Vec<AttributeRule>,
    group_rules: Vec<GroupRule>,
    metric: FuseMetric,
    inclusive: bool,
    match_attributes: bool,
    keep_fused_points: bool,
}

fn same_row_int(offsets: &[i64], values: &[i64], left: usize, right: usize) -> bool {
    let (lf, ll, rf, rl) = (offsets[left], offsets[left + 1], offsets[right], offsets[right + 1]);
    ll - lf == rl - rf && (0..ll - lf).all(|local| values[(lf + local) as usize] == values[(rf + local) as usize])
}

fn fuse_clusters(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    options: &ClusterOptions,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    if grain <= 0 {
        return Err(make(
            "Rdk.Fuse_grid",
            "invalid_argument",
            "Rdk.Fuse_grid.fuse: grain must be positive".to_owned(),
        ));
    }
    let count = geometry::point_count(geometry_value);
    if let Some(item) = options.selection.as_ref() {
        if group::owner(item) != group::Owner::Point || group::length(item) != count {
            return Err(fail("Rdk.Fuse_grid.fuse: selection must be a matching point group"));
        }
    }
    let tolerance = options.tolerance;
    if !tolerance.is_finite() || tolerance < 0.0 {
        return Err(fail("Rdk.Fuse_grid.fuse: tolerance must be finite and non-negative"));
    }
    let exact = tolerance == 0.0;
    let mut point_attributes: Vec<FuseAttributeView> = Vec::new();
    if options.match_attributes {
        for item in geometry::attributes(geometry_value) {
            if attribute::owner(&item) != attribute::Owner::Point {
                continue;
            }
            point_attributes.push(match attribute::View::storage(&item)? {
                attribute::Storage::Float(values) => FuseAttributeView::Float(values.clone()),
                attribute::Storage::Int(values) => FuseAttributeView::Int(values.clone()),
                attribute::Storage::Text(values) => FuseAttributeView::Text(values.clone()),
                attribute::Storage::Float2(values) => {
                    let v = packed::Float2::View::view(values);
                    FuseAttributeView::Float2(v.x, v.y)
                }
                attribute::Storage::Float3(values) => {
                    let v = packed::Float3::View::view(values)?;
                    FuseAttributeView::Float3(v.x, v.y, v.z)
                }
                attribute::Storage::Float4(values) => {
                    let v = packed::Float4::View::view(values);
                    FuseAttributeView::Float4(v.x, v.y, v.z, v.w)
                }
                attribute::Storage::IntArray(values) => {
                    let v = packed::IntArray::View::view(values);
                    FuseAttributeView::IntArray(v.offsets, v.values)
                }
                attribute::Storage::FloatArray(values) => {
                    let v = packed::FloatArray::View::view(values);
                    FuseAttributeView::FloatArray(v.offsets, v.values)
                }
            });
        }
    }
    let inclusive = options.inclusive;
    let within = |value: f64| {
        if exact {
            value == 0.0
        } else if inclusive {
            value <= tolerance
        } else {
            value < tolerance
        }
    };
    let near = |left: f64, right: f64| within((left - right).abs());
    let compatible = |left: i64, right: i64| -> bool {
        let (l, r) = (left as usize, right as usize);
        point_attributes.iter().all(|view| match view {
            FuseAttributeView::Float(values) => near(values[l], values[r]),
            FuseAttributeView::Int(values) => values[l] == values[r],
            FuseAttributeView::Text(values) => values[l] == values[r],
            FuseAttributeView::Float2(x, y) => near(x[l], x[r]) && near(y[l], y[r]),
            FuseAttributeView::Float3(x, y, z) => near(x[l], x[r]) && near(y[l], y[r]) && near(z[l], z[r]),
            FuseAttributeView::Float4(x, y, z, w) => {
                near(x[l], x[r]) && near(y[l], y[r]) && near(z[l], z[r]) && near(w[l], w[r])
            }
            FuseAttributeView::IntArray(offsets, values) => same_row_int(offsets, values, l, r),
            FuseAttributeView::FloatArray(offsets, values) => {
                let (lf, ll, rf, rl) = (offsets[l], offsets[l + 1], offsets[r], offsets[r + 1]);
                ll - lf == rl - rf
                    && (0..ll - lf).all(|local| near(values[(lf + local) as usize], values[(rf + local) as usize]))
            }
        })
    };
    let cluster_metric = match options.metric {
        FuseMetric::Euclidean => point_clusters::Metric::Euclidean,
        FuseMetric::Componentwise => point_clusters::Metric::Componentwise,
    };
    match point_clusters::create(
        cancel_value,
        options.selection.as_ref(),
        cluster_metric,
        options.inclusive,
        "Rdk.Fuse_grid.fuse",
        tolerance,
        &compatible,
        geometry_value,
    )? {
        point_clusters::T::Identity => Ok(geometry_value.clone()),
        point_clusters::T::Clusters(clusters) => fuse_reduce::apply(
            cancel_value,
            &fuse_reduce::Params {
                grain,
                position: options.position,
                weight_attribute: options.weight_attribute.clone(),
                attributes: options.attributes,
                attribute_rules: options.attribute_rules.clone(),
                group_rules: options.group_rules.clone(),
                compact: !options.keep_fused_points,
                rewire: true,
                remap_edge_groups: true,
            },
            &clusters,
            geometry_value,
        ),
    }
}

pub struct Params {
    pub cancel: Option<std::sync::Arc<cancel::T>>,
    pub grain: i64,
    pub selection: Option<group::T>,
    pub target_selection: Option<group::T>,
    pub targeting: point_snap::Targeting,
    pub using: point_snap::Using,
    pub tolerance: f64,
    pub position: Position,
    pub weight_attribute: Option<String>,
    pub attributes: DefaultAttributes,
    pub attribute_rules: Vec<AttributeRule>,
    pub group_rules: Vec<GroupRule>,
    pub metric: FuseMetric,
    pub inclusive: bool,
    pub match_attributes: bool,
    pub radius_attribute: Option<String>,
    pub match_attribute: Option<String>,
    pub match_condition: point_snap::MatchCondition,
    pub match_tolerance: f64,
    pub modify_target: bool,
    pub fuse_points: bool,
    pub keep_fused_points: bool,
    pub snapped_group: Option<String>,
    pub snapped_destination_attribute: Option<String>,
    pub remove_degenerate_primitives: bool,
    pub remove_unused_points_from_degenerate_primitives: bool,
    pub remove_all_unused_points: bool,
    pub target: Option<geometry::T>,
}

fn invalid_name(label: &str, name: &Option<String>) -> Option<error::T> {
    match name {
        Some(name) if name.trim().is_empty() => {
            Some(fail(&format!("Rdk.Fuse_grid.fuse: {label} name must not be empty")))
        }
        _ => None,
    }
}

fn install_outputs(
    params: &Params,
    destinations: &[i64],
    destination_bits: &Option<Vec<bool>>,
    count: i64,
    output: geometry::T,
) -> Result<geometry::T, error::T> {
    let mut output = output;
    if let Some(name) = &params.snapped_destination_attribute {
        let mut values = destinations.to_vec();
        if let Some(bits) = destination_bits {
            for point in 0..count as usize {
                if values[point] < 0 && bits[point] {
                    values[point] = point as i64;
                }
            }
        }
        let item = attribute::create_owned(name, attribute::Owner::Point, attribute::Storage::Int(values))?;
        output = geometry::with_attribute(item, &output)?;
    }
    if let Some(name) = &params.snapped_group {
        let item = group::init(Some(params.grain), group::Owner::Point, name, count, &|point| {
            destinations[point as usize] >= 0
        })?;
        output = geometry::with_group(item, &output)?;
    }
    Ok(output)
}

fn install_reduced_outputs(
    params: &Params,
    destinations: &[i64],
    clusters: &Clusters,
    compact: bool,
    output: geometry::T,
) -> Result<geometry::T, error::T> {
    let output_count = geometry::point_count(&output);
    let source_point = |output_point: i64| -> i64 {
        if compact {
            let first = clusters.offsets[output_point as usize];
            let last = clusters.offsets[output_point as usize + 1];
            for slot in first..last {
                let point = clusters.members[slot as usize];
                if destinations[point as usize] >= 0 {
                    return point;
                }
            }
            -1
        } else {
            output_point
        }
    };
    let mut output = output;
    if let Some(name) = &params.snapped_destination_attribute {
        let values: Vec<i64> = (0..output_count)
            .map(|output_point| {
                let point = source_point(output_point);
                if point < 0 { -1 } else { destinations[point as usize] }
            })
            .collect();
        let item = attribute::create_owned(name, attribute::Owner::Point, attribute::Storage::Int(values))?;
        output = geometry::with_attribute(item, &output)?;
    }
    if let Some(name) = &params.snapped_group {
        let item = group::init(Some(params.grain), group::Owner::Point, name, output_count, &|output_point| {
            source_point(output_point) >= 0
        })?;
        output = geometry::with_group(item, &output)?;
    }
    Ok(output)
}

fn fuse_inner(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_value = params.cancel.as_deref();
    let grain = params.grain;
    fuse_rules::validate(&params.attribute_rules, &params.group_rules, geometry_value)?;
    let advanced = params.target.is_some()
        || params.target_selection.is_some()
        || params.targeting != point_snap::Targeting::NearPoints
        || params.using != point_snap::Using::LeastTargetPoint
        || params.radius_attribute.is_some()
        || params.match_attribute.is_some()
        || params.match_condition != point_snap::MatchCondition::EqualAttributeValues
        || params.match_tolerance != 0.0
        || params.modify_target
        || !params.fuse_points
        || params.snapped_group.is_some()
        || params.snapped_destination_attribute.is_some();
    let cleanup = |result: Result<geometry::T, error::T>| -> Result<geometry::T, error::T> {
        fuse_cleanup::apply(
            cancel_value,
            grain,
            params.remove_degenerate_primitives,
            params.remove_unused_points_from_degenerate_primitives,
            params.remove_all_unused_points,
            &result?,
        )
    };
    if !advanced {
        return cleanup(fuse_clusters(
            cancel_value,
            grain,
            &ClusterOptions {
                selection: params.selection.clone(),
                tolerance: params.tolerance,
                position: params.position,
                weight_attribute: params.weight_attribute.clone(),
                attributes: params.attributes,
                attribute_rules: params.attribute_rules.clone(),
                group_rules: params.group_rules.clone(),
                metric: params.metric,
                inclusive: params.inclusive,
                match_attributes: params.match_attributes,
                keep_fused_points: params.keep_fused_points,
            },
            geometry_value,
        ));
    }
    cleanup(fuse_advanced(params, geometry_value))
}

fn fuse_advanced(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_value = params.cancel.as_deref();
    let grain = params.grain;
    if let Some(failure) = invalid_name("snapped group", &params.snapped_group) {
        return Err(failure);
    }
    if let Some(failure) = invalid_name("snapped destination attribute", &params.snapped_destination_attribute) {
        return Err(failure);
    }
    let target_geometry = params.target.as_ref().unwrap_or(geometry_value);
    let same = geometry::data_id(geometry_value) == geometry::data_id(target_geometry);
    if params.keep_fused_points && !params.fuse_points {
        return Err(fail("Rdk.Fuse_grid.fuse: Keep Fused Points requires Fuse Snapped Points"));
    }
    if params.modify_target && !same {
        return Err(fail("Rdk.Fuse_grid.fuse: Modify Target is unavailable with a second input"));
    }
    let effective_targets: Option<&group::T> = match (&params.target_selection, same) {
        (Some(item), _) => Some(item),
        (None, true) => params.selection.as_ref(),
        (None, false) => None,
    };
    let effective_modify_target = params.modify_target || (same && params.target_selection.is_none());
    let cluster_metric = match params.metric {
        FuseMetric::Euclidean => point_clusters::Metric::Euclidean,
        FuseMetric::Componentwise => point_clusters::Metric::Componentwise,
    };
    let destinations = point_snap::plan(
        cancel_value,
        grain,
        params.selection.as_ref(),
        effective_targets,
        &params.targeting,
        params.using,
        params.tolerance,
        cluster_metric,
        params.inclusive,
        params.radius_attribute.as_deref(),
        params.match_attribute.as_deref(),
        params.match_condition,
        params.match_tolerance,
        geometry_value,
        target_geometry,
    )?;
    let count = geometry::point_count(geometry_value);
    let source = packed::Float3::View::view(geometry::positions(geometry_value))?;
    let target_positions = packed::Float3::View::view(geometry::positions(target_geometry))?;
    let mut mapped = 0i64;
    let mut moved = 0i64;
    for point in 0..count as usize {
        let destination = destinations[point];
        if destination >= 0 {
            mapped += 1;
            let d = destination as usize;
            if source.x[point] != target_positions.x[d]
                || source.y[point] != target_positions.y[d]
                || source.z[point] != target_positions.z[d]
            {
                moved += 1;
            }
        }
    }
    let destination_bits: Option<Vec<bool>> = if same && params.fuse_points && mapped > 0 {
        let mut bits = vec![false; count as usize];
        for point in 0..count as usize {
            let destination = destinations[point];
            if destination >= 0 {
                bits[destination as usize] = true;
            }
        }
        Some(bits)
    } else {
        None
    };
    if same && effective_modify_target && mapped > 0 {
        return match rdk_geom::point_clusters::of_links(cancel_value, "Rdk.Fuse_grid.fuse", &destinations)? {
            point_clusters::T::Identity => install_outputs(params, &destinations, &destination_bits, count, geometry_value.clone()),
            point_clusters::T::Clusters(clusters) => {
                let compact = params.fuse_points && !params.keep_fused_points;
                let reduced = fuse_reduce::apply(
                    cancel_value,
                    &fuse_reduce::Params {
                        grain,
                        position: params.position,
                        weight_attribute: params.weight_attribute.clone(),
                        attributes: params.attributes,
                        attribute_rules: params.attribute_rules.clone(),
                        group_rules: params.group_rules.clone(),
                        compact,
                        rewire: params.fuse_points,
                        remap_edge_groups: true,
                    },
                    &clusters,
                    geometry_value,
                )?;
                install_reduced_outputs(params, &destinations, &clusters, compact, reduced)
            }
        };
    }
    let output = if moved == 0 {
        geometry_value.clone()
    } else {
        let mut x = source.x.to_vec();
        let mut y = source.y.to_vec();
        let mut z = source.z.to_vec();
        for point in 0..count as usize {
            cancel::poll(cancel_value, point as i64, 4095)?;
            let destination = destinations[point];
            if destination >= 0 {
                x[point] = target_positions.x[destination as usize];
                y[point] = target_positions.y[destination as usize];
                z[point] = target_positions.z[destination as usize];
            }
        }
        let moved_geometry =
            geometry::with_positions(packed::Float3::View::of_owned_exn(x, y, z)?, geometry_value)?;
        let moved_geometry = geometry::without_attribute(attribute::Owner::Point, "N", &moved_geometry);
        geometry::without_attribute(attribute::Owner::Vertex, "N", &moved_geometry)
    };
    let output = fuse_target_rules::apply(
        cancel_value,
        grain,
        &params.attribute_rules,
        &params.group_rules,
        &destinations,
        &output,
        target_geometry,
    )?;
    let output = install_outputs(params, &destinations, &destination_bits, count, output)?;
    if !params.fuse_points || mapped == 0 {
        return Ok(output);
    }
    let selected = group::init(Some(grain), group::Owner::Point, "__rdk_fuse_snapped", count, &|point| {
        destinations[point as usize] >= 0
            || destination_bits.as_ref().is_some_and(|bits| bits[point as usize])
    })?;
    fuse_clusters(
        cancel_value,
        grain,
        &ClusterOptions {
            selection: Some(selected),
            tolerance: 0.0,
            position: Position::First,
            weight_attribute: None,
            attributes: params.attributes,
            attribute_rules: Vec::new(),
            group_rules: Vec::new(),
            metric: FuseMetric::Euclidean,
            inclusive: true,
            match_attributes: params.match_attributes,
            keep_fused_points: params.keep_fused_points,
        },
        &output,
    )
}

pub fn fuse(params: &Params, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    fuse_inner(params, geometry_value).map_err(|failure| {
        if error::code(&failure) == "cancelled" {
            make("fuse", "cancelled", "geometry operation was cancelled".to_owned())
        } else {
            make("fuse", "invalid_geometry", error::message(&failure).to_owned())
        }
    })
}

pub struct WeldParams {
    pub cancel: Option<std::sync::Arc<cancel::T>>,
    pub grain: i64,
    pub selection: Option<group::T>,
    pub tolerance: f64,
}

/// Fuse of the points within the tolerance, averaging positions and keeping the first attributes.
pub fn weld(params: WeldParams, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    fuse(
        &Params {
            cancel: params.cancel,
            grain: params.grain,
            selection: params.selection,
            target_selection: None,
            targeting: point_snap::Targeting::NearPoints,
            using: point_snap::Using::LeastTargetPoint,
            tolerance: params.tolerance,
            position: Position::Average,
            weight_attribute: None,
            attributes: DefaultAttributes::KeepFirst,
            attribute_rules: Vec::new(),
            group_rules: Vec::new(),
            metric: FuseMetric::Euclidean,
            inclusive: true,
            match_attributes: false,
            radius_attribute: None,
            match_attribute: None,
            match_condition: point_snap::MatchCondition::EqualAttributeValues,
            match_tolerance: 0.0,
            modify_target: false,
            fuse_points: true,
            keep_fused_points: false,
            snapped_group: None,
            snapped_destination_attribute: None,
            remove_degenerate_primitives: false,
            remove_unused_points_from_degenerate_primitives: false,
            remove_all_unused_points: false,
            target: None,
        },
        geometry_value,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compact_points::fixtures::*;
    use rdk_core::attribute::{Owner, Storage};
    use rdk_core::topology::PrimitiveKind;
    use rdk_core::{edge_group, topology, topology_index};

    fn points(list: &[(f64, f64, f64)]) -> geometry::T {
        let x: Vec<f64> = list.iter().map(|p| p.0).collect();
        let y: Vec<f64> = list.iter().map(|p| p.1).collect();
        let z: Vec<f64> = list.iter().map(|p| p.2).collect();
        geom(&x, &y, &z, &[], &[0], None)
    }

    fn base() -> Params {
        Params {
            cancel: None,
            grain: 16_384,
            selection: None,
            target_selection: None,
            targeting: point_snap::Targeting::NearPoints,
            using: point_snap::Using::LeastTargetPoint,
            tolerance: 1e-6,
            position: Position::Average,
            weight_attribute: None,
            attributes: DefaultAttributes::KeepFirst,
            attribute_rules: Vec::new(),
            group_rules: Vec::new(),
            metric: FuseMetric::Euclidean,
            inclusive: true,
            match_attributes: false,
            radius_attribute: None,
            match_attribute: None,
            match_condition: point_snap::MatchCondition::EqualAttributeValues,
            match_tolerance: 0.0,
            modify_target: false,
            fuse_points: true,
            keep_fused_points: false,
            snapped_group: None,
            snapped_destination_attribute: None,
            remove_degenerate_primitives: false,
            remove_unused_points_from_degenerate_primitives: false,
            remove_all_unused_points: false,
            target: None,
        }
    }

    fn point_group(name: &str, count: i64, predicate: &(dyn Fn(i64) -> bool + Send + Sync)) -> group::T {
        group::init(None, group::Owner::Point, name, count, predicate).unwrap()
    }

    fn xs(g: &geometry::T) -> Vec<f64> {
        positions(g).0
    }

    fn near(a: f64, b: f64) -> bool {
        (a - b).abs() <= 1e-12
    }

    fn code(result: Result<geometry::T, error::T>) -> String {
        error::code(&err(result)).to_owned()
    }

    fn group_members(g: &geometry::T, name: &str) -> Vec<i64> {
        let found = geometry::find_group(group::Owner::Point, name, g).unwrap();
        (0..geometry::point_count(g)).filter(|&p| group::mem(p, &found)).collect()
    }

    #[test]
    fn restricted_fuse_and_cluster_contract() {
        let source = points(&[(0., 0., 0.), (0.0005, 0., 0.), (0.0005, 0., 0.)]);
        let selection = point_group("query", 3, &|p| p < 2);
        let output = fuse(&Params { selection: Some(selection), tolerance: 0.001, ..base() }, &source).unwrap();
        assert_eq!(geometry::point_count(&output), 2);
        let p = xs(&output);
        assert!(near(p[0], 0.00025) && near(p[1], 0.0005));
        let only_unselected = point_group("query", 3, &|p| p == 2);
        let unchanged =
            fuse(&Params { selection: Some(only_unselected), tolerance: 1.0, ..base() }, &source).unwrap();
        assert_eq!(geometry::data_id(&unchanged), geometry::data_id(&source));

        let chain = points(&[(0., 0., 0.), (0.009, 0., 0.), (0.018, 0., 0.)]);
        let chain = fuse(&Params { tolerance: 0.01, ..base() }, &chain).unwrap();
        let p = xs(&chain);
        assert!(p.len() == 2 && near(p[0], 0.0045) && near(p[1], 0.018), "representative-bounded clustering");
        let signed_zero = points(&[(0., 0., 0.), (-0.0, 0., 0.)]);
        assert_eq!(geometry::point_count(&fuse(&Params { tolerance: 0.0, ..base() }, &signed_zero).unwrap()), 1);
        let token = std::sync::Arc::new(cancel::create());
        cancel::cancel(&token);
        let pair = points(&[(0., 0., 0.), (0., 0., 0.)]);
        assert_eq!(code(fuse(&Params { cancel: Some(token), tolerance: 0.0, ..base() }, &pair)), "cancelled");
    }

    #[test]
    fn target_policies() {
        let source = points(&[(0.1, 0., 0.), (0.9, 0., 0.), (2., 0., 0.), (5., 0., 0.)]);
        let target = points(&[(0., 0., 0.), (1., 0., 0.), (3., 0., 0.)]);
        let run = |using: point_snap::Using, fuse_points: bool| {
            fuse(
                &Params {
                    selection: Some(point_group("queries", 4, &|p| p < 3)),
                    target_selection: Some(point_group("targets", 3, &|p| p < 2)),
                    target: Some(target.clone()),
                    using,
                    tolerance: 1.0,
                    fuse_points,
                    snapped_group: Some("snapped".to_owned()),
                    snapped_destination_attribute: Some("destination".to_owned()),
                    ..base()
                },
                &source,
            )
            .unwrap()
        };
        let least = run(point_snap::Using::LeastTargetPoint, false);
        let closest = run(point_snap::Using::ClosestTargetPoint, false);
        assert_eq!(xs(&least), vec![0., 0., 1., 5.]);
        assert_eq!(ints(Owner::Point, "destination", &least), vec![0, 0, 1, -1]);
        assert_eq!(group_members(&least, "snapped"), vec![0, 1, 2]);
        assert_eq!(xs(&closest), vec![0., 1., 1., 5.]);
        assert_eq!(ints(Owner::Point, "destination", &closest), vec![0, 1, 1, -1]);
        let fused = run(point_snap::Using::ClosestTargetPoint, true);
        assert_eq!(geometry::point_count(&fused), 3);
        assert_eq!(xs(&fused), vec![0., 1., 5.]);
        assert_eq!(ints(Owner::Point, "destination", &fused), vec![0, 1, -1]);
        assert_eq!(group_members(&fused, "snapped"), vec![0, 1]);
    }

    #[test]
    fn specified_targets_and_validation() {
        let source = with_attr(
            &points(&[(9., 0., 0.), (9., 0., 0.), (2., 0., 0.), (5., 0., 0.)]),
            Owner::Point,
            "target_point",
            Storage::Int(vec![2, 0, 99, -1]),
        );
        let target = points(&[(0., 0., 0.), (1., 0., 0.), (3., 0., 0.)]);
        let output = fuse(
            &Params {
                target: Some(target.clone()),
                target_selection: Some(point_group("targets", 3, &|p| p != 1)),
                targeting: point_snap::Targeting::SpecifiedPoints("target_point".to_owned()),
                fuse_points: false,
                snapped_destination_attribute: Some("destination".to_owned()),
                ..base()
            },
            &source,
        )
        .unwrap();
        assert_eq!(xs(&output), vec![3., 0., 2., 5.]);
        assert_eq!(ints(Owner::Point, "destination", &output), vec![2, 0, -1, -1]);
        let missing = Params {
            target: Some(target.clone()),
            targeting: point_snap::Targeting::SpecifiedPoints("missing".to_owned()),
            ..base()
        };
        assert_eq!(code(fuse(&missing, &source)), "invalid_geometry");

        let center = 1e150;
        let scale = 1e140;
        let far = points(&[(center + scale, center, center)]);
        let near_target = points(&[(center, center, center)]);
        let output = fuse(
            &Params {
                target: Some(near_target),
                tolerance: 2.0 * scale,
                using: point_snap::Using::ClosestTargetPoint,
                fuse_points: false,
                ..base()
            },
            &far,
        )
        .unwrap();
        assert_eq!(xs(&output), vec![center]);
        let invalid_radius = with_attr(&points(&[(0., 0., 0.)]), Owner::Point, "radius", Storage::Float(vec![-1.0]));
        let valid_radius = with_attr(&points(&[(0., 0., 0.)]), Owner::Point, "radius", Storage::Float(vec![0.0]));
        assert_eq!(
            code(fuse(
                &Params { target: Some(valid_radius), radius_attribute: Some("radius".to_owned()), ..base() },
                &invalid_radius
            )),
            "invalid_geometry"
        );
        let wrong = point_group("wrong", 2, &|_| true);
        let one = points(&[(0., 0., 0.)]);
        assert_eq!(
            code(fuse(&Params { target: Some(one.clone()), target_selection: Some(wrong), ..base() }, &one)),
            "invalid_geometry"
        );
        let nonfinite = points(&[(f64::INFINITY, 0., 0.)]);
        assert_eq!(
            code(fuse(
                &Params {
                    target: Some(nonfinite),
                    using: point_snap::Using::ClosestTargetPoint,
                    ..base()
                },
                &one
            )),
            "invalid_geometry"
        );
    }

    #[test]
    fn radius_and_match_attributes() {
        let source = with_attr(
            &with_attr(
                &points(&[(1.9, 0., 0.), (0.9, 0., 0.), (0.1, 0., 0.)]),
                Owner::Point,
                "radius",
                Storage::Float(vec![0.6, 0., 0.]),
            ),
            Owner::Point,
            "piece",
            Storage::Int(vec![3, 2, 1]),
        );
        let target = with_attr(
            &with_attr(
                &points(&[(0., 0., 0.), (1., 0., 0.), (3., 0., 0.)]),
                Owner::Point,
                "radius",
                Storage::Float(vec![0., 0., 0.5]),
            ),
            Owner::Point,
            "piece",
            Storage::Int(vec![1, 2, 3]),
        );
        let run = |tolerance: f64, radius: bool, matching: bool, unequal: bool| {
            fuse(
                &Params {
                    target: Some(target.clone()),
                    tolerance,
                    radius_attribute: radius.then(|| "radius".to_owned()),
                    match_attribute: matching.then(|| "piece".to_owned()),
                    match_condition: if unequal {
                        point_snap::MatchCondition::UnequalAttributeValues
                    } else {
                        point_snap::MatchCondition::EqualAttributeValues
                    },
                    using: point_snap::Using::ClosestTargetPoint,
                    fuse_points: false,
                    ..base()
                },
                &source,
            )
            .unwrap()
        };
        assert_eq!(xs(&run(0.0, true, false, false)), vec![3., 0.9, 0.1]);
        assert_eq!(xs(&run(2.0, false, true, false)), vec![3., 1., 0.]);
        assert_eq!(xs(&run(2.0, false, true, true)), vec![1., 0., 1.]);
        let float_source = with_attr(&points(&[(0., 0., 0.)]), Owner::Point, "key", Storage::Float(vec![1.001]));
        let float_target = with_attr(&points(&[(1., 0., 0.)]), Owner::Point, "key", Storage::Float(vec![1.0]));
        let tolerant = fuse(
            &Params {
                target: Some(float_target),
                tolerance: 2.0,
                match_attribute: Some("key".to_owned()),
                match_tolerance: 0.01,
                fuse_points: false,
                ..base()
            },
            &float_source,
        )
        .unwrap();
        assert_eq!(xs(&tolerant), vec![1.0]);
    }

    #[test]
    fn same_geometry_groups_modify_and_keep() {
        let source = points(&[(0., 0., 0.), (0.1, 0., 0.), (10., 0., 0.)]);
        let output = fuse(
            &Params {
                selection: Some(point_group("queries", 3, &|p| p == 1)),
                target_selection: Some(point_group("targets", 3, &|p| p == 0)),
                tolerance: 0.2,
                snapped_destination_attribute: Some("destination".to_owned()),
                ..base()
            },
            &source,
        )
        .unwrap();
        assert_eq!(xs(&output), vec![0., 10.]);
        assert_eq!(ints(Owner::Point, "destination", &output), vec![0, -1]);

        let source = points(&[(0., 0., 0.), (2., 0., 0.), (10., 0., 0.)]);
        let modify = |fuse_points: bool| {
            fuse(
                &Params {
                    selection: Some(point_group("queries", 3, &|p| p == 0)),
                    target_selection: Some(point_group("targets", 3, &|p| p == 1)),
                    modify_target: true,
                    fuse_points,
                    tolerance: 3.0,
                    ..base()
                },
                &source,
            )
            .unwrap()
        };
        assert_eq!(xs(&modify(true)), vec![1., 10.]);
        assert_eq!(xs(&modify(false)), vec![1., 1., 10.]);
        assert_eq!(
            code(fuse(
                &Params { target: Some(points(&[(2., 0., 0.)])), modify_target: true, ..base() },
                &source
            )),
            "invalid_geometry"
        );
        let line = geom(
            &[0., 0.1, 1.],
            &[0.; 3],
            &[0.; 3],
            &[0, 1, 2],
            &[0, 3],
            Some(vec![PrimitiveKind::OpenPolyline]),
        );
        let kept = fuse(&Params { tolerance: 0.2, keep_fused_points: true, ..base() }, &line).unwrap();
        assert_eq!(geometry::point_count(&kept), 3);
        assert_eq!(xs(&kept), vec![0.05, 0.05, 1.0]);
        assert_eq!(topo(&kept).0, vec![0, 0, 2]);
        assert_eq!(
            code(fuse(&Params { tolerance: 0.2, fuse_points: false, keep_fused_points: true, ..base() }, &line)),
            "invalid_geometry"
        );
    }

    #[test]
    fn position_reductions() {
        let source = with_attr(
            &points(&[(0., 3., 6.), (2., 1., 4.), (4., 2., 5.)]),
            Owner::Point,
            "weight",
            Storage::Float(vec![1., 2., 3.]),
        );
        let position = |mode: Position| {
            let out = fuse(
                &Params {
                    tolerance: 10.0,
                    position: mode,
                    weight_attribute: Some("weight".to_owned()),
                    ..base()
                },
                &source,
            )
            .unwrap();
            let p = positions(&out);
            (p.0[0], p.1[0], p.2[0])
        };
        let same = |got: (f64, f64, f64), want: (f64, f64, f64)| {
            near(got.0, want.0) && near(got.1, want.1) && near(got.2, want.2)
        };
        assert!(same(position(Position::First), (0., 3., 6.)));
        assert!(same(position(Position::LeastPoint), (0., 3., 6.)));
        assert!(same(position(Position::GreatestPoint), (4., 2., 5.)));
        assert!(same(position(Position::Average), (2., 2., 5.)));
        assert!(same(position(Position::Minimum), (0., 1., 4.)));
        assert!(same(position(Position::Maximum), (4., 3., 6.)));
        assert!(same(position(Position::Median), (2., 2., 5.)));
        assert!(same(position(Position::Sum), (6., 6., 15.)));
        assert!(same(position(Position::SumSquares), (20., 14., 77.)));
        assert!(same(position(Position::RootMeanSquare), ((20.0f64 / 3.0).sqrt(), (14.0f64 / 3.0).sqrt(), (77.0f64 / 3.0).sqrt())));
        assert!(same(position(Position::WeightedAverage), (16. / 6., 11. / 6., 29. / 6.)));
        assert!(same(position(Position::WeightedSum), (16., 11., 29.)));
        assert!(same(position(Position::MinimumWeight), (0., 3., 6.)));
        assert!(same(position(Position::MaximumWeight), (4., 2., 5.)));
        let mode_source = points(&[(0., 3., 6.), (2., 1., 4.), (2., 1., 5.), (4., 2., 5.)]);
        let mode = fuse(&Params { tolerance: 10.0, position: Position::Mode, ..base() }, &mode_source).unwrap();
        let p = positions(&mode);
        assert_eq!((p.0, p.1, p.2), (vec![2.0], vec![1.0], vec![5.0]));
        let huge = fuse(
            &Params { tolerance: 0.0, ..base() },
            &points(&[(f64::MAX, 0., 0.), (f64::MAX, 0., 0.)]),
        )
        .unwrap();
        assert_eq!(xs(&huge), vec![f64::MAX]);
        assert_eq!(
            code(fuse(
                &Params { tolerance: 0.0, position: Position::SumSquares, ..base() },
                &points(&[(1e200, 0., 0.), (1e200, 0., 0.)])
            )),
            "invalid_geometry"
        );
        let zero_weight = with_attr(
            &points(&[(0., 0., 0.), (0., 0., 0.)]),
            Owner::Point,
            "weight",
            Storage::Float(vec![1., -1.]),
        );
        assert_eq!(
            code(fuse(
                &Params {
                    tolerance: 0.0,
                    position: Position::WeightedAverage,
                    weight_attribute: Some("weight".to_owned()),
                    ..base()
                },
                &zero_weight
            )),
            "invalid_geometry"
        );
    }

    fn cleanup_fixture() -> geometry::T {
        let x = [0., 0.1, 1., 2., 5., 5.1, 9.];
        let g = geom(
            &x,
            &[0.; 7],
            &[0.; 7],
            &[0, 1, 2, 3, 0, 1, 4, 5],
            &[0, 4, 6, 8],
            Some(vec![PrimitiveKind::Polygon, PrimitiveKind::OpenPolyline, PrimitiveKind::OpenPolyline]),
        );
        let g = with_attr(&g, Owner::Point, "point_id", Storage::Int(vec![100, 101, 102, 103, 104, 105, 106]));
        let g = with_attr(&g, Owner::Vertex, "vertex_id", Storage::Int(vec![10, 11, 12, 13, 14, 15, 16, 17]));
        let g = with_attr(&g, Owner::Primitive, "primitive_id", Storage::Int(vec![20, 21, 22]));
        let unused = group::ordered(group::Owner::Point, "unused", 7, &[6]).unwrap();
        let g = geometry::with_group(unused, &g).unwrap();
        let topology_value = geometry::topology(&g).clone();
        let index = topology_index::create(None, &topology_value).unwrap();
        let edge = topology_index::find_edge(&index, 1, 2).unwrap();
        let mut builder = edge_group::Builder::create(&topology_value, &index, "edge").unwrap();
        edge_group::Builder::set(&mut builder, edge, true).unwrap();
        geometry::with_edge_group(edge_group::Builder::freeze(&mut builder).unwrap(), &g).unwrap()
    }

    #[test]
    fn cleanup_after_fuse() {
        let source = cleanup_fixture();
        let unclean = fuse(&Params { tolerance: 0.2, ..base() }, &source).unwrap();
        assert_eq!(geometry::primitive_count(&unclean), 3);
        assert_eq!(topo(&unclean).0, vec![0, 0, 1, 2, 0, 0, 3, 3]);
        let cleaned = fuse(
            &Params { grain: 1, tolerance: 0.2, remove_degenerate_primitives: true, ..base() },
            &source,
        )
        .unwrap();
        let (vp, offsets, _) = topo(&cleaned);
        assert_eq!(geometry::point_count(&cleaned), 5);
        assert_eq!(geometry::vertex_count(&cleaned), 3);
        assert_eq!(geometry::primitive_count(&cleaned), 1);
        assert_eq!((vp, offsets), (vec![0, 1, 2], vec![0, 3]));
        assert_eq!(ints(Owner::Vertex, "vertex_id", &cleaned), vec![10, 12, 13]);
        assert_eq!(ints(Owner::Primitive, "primitive_id", &cleaned), vec![20]);
        let edges = geometry::find_edge_group("edge", &cleaned).unwrap();
        assert_eq!(edge_group::cardinality(&edges), 1);
        let selective = fuse(
            &Params {
                grain: 1,
                tolerance: 0.2,
                remove_degenerate_primitives: true,
                remove_unused_points_from_degenerate_primitives: true,
                ..base()
            },
            &source,
        )
        .unwrap();
        assert_eq!(geometry::point_count(&selective), 4);
        assert_eq!(xs(&selective), vec![0.05, 1., 2., 9.]);
        assert_eq!(ints(Owner::Point, "point_id", &selective), vec![100, 102, 103, 106]);
        assert_eq!(group_members(&selective, "unused"), vec![3]);
        let compact = fuse(
            &Params {
                grain: 1,
                tolerance: 0.2,
                remove_degenerate_primitives: true,
                remove_all_unused_points: true,
                ..base()
            },
            &source,
        )
        .unwrap();
        assert_eq!(xs(&compact), vec![0.05, 1., 2.]);
    }

    #[test]
    fn rules_apply_to_snapped_pairs() {
        // Two stacked lattices are linked through a closest-target snap that modifies the target.
        let lattice = grid(4, 3, 3.0, false);
        let count = geometry::point_count(&lattice);
        let shifted = {
            let (x, y, z) = positions(&lattice);
            let (vp, offsets, _) = topo(&lattice);
            let x2: Vec<f64> = x.iter().map(|v| v + 0.013).collect();
            let y2: Vec<f64> = y.iter().map(|v| v - 0.017).collect();
            (x2, y2, z, vp, offsets)
        };
        let (px, py, pz) = positions(&lattice);
        let (vp, offsets, _) = topo(&lattice);
        let mut x = px.clone();
        x.extend(shifted.0.iter());
        let mut y = py.clone();
        y.extend(shifted.1.iter());
        let mut z = pz.clone();
        z.extend(shifted.2.iter());
        let mut all_vp = vp.clone();
        all_vp.extend(vp.iter().map(|v| v + count));
        let mut all_offsets = offsets.clone();
        let base_len = *offsets.last().unwrap();
        all_offsets.extend(offsets.iter().skip(1).map(|o| o + base_len));
        let linked = geom(&x, &y, &z, &all_vp, &all_offsets, None);
        let total = geometry::point_count(&linked);
        let linked = with_attr(
            &linked,
            Owner::Point,
            "rule_weight",
            Storage::Float((0..total).map(|p| if p < count { 1.0 } else { 3.0 }).collect()),
        );
        let linked = with_attr(
            &linked,
            Owner::Point,
            "rule_value",
            Storage::Float((0..total).map(|p| ((p % count) as f64 * 0.013).sin()).collect()),
        );
        let linked = with_attr(&linked, Owner::Point, "rule_id", Storage::Int((0..total).map(|p| p % 17).collect()));
        let output = fuse(
            &Params {
                selection: Some(point_group("queries", total, &|p| p < count)),
                target_selection: Some(point_group("targets", total, &|p| p >= count)),
                modify_target: true,
                using: point_snap::Using::ClosestTargetPoint,
                tolerance: 0.05,
                attribute_rules: vec![
                    AttributeRule {
                        pattern: "rule_value".to_owned(),
                        method: fuse_rules::AttributeMethod::WeightedAverage,
                        weight_attribute: Some("rule_weight".to_owned()),
                    },
                    AttributeRule {
                        pattern: "rule_id".to_owned(),
                        method: fuse_rules::AttributeMethod::Mode,
                        weight_attribute: None,
                    },
                ],
                ..base()
            },
            &linked,
        )
        .unwrap();
        assert_eq!(geometry::point_count(&output), count);
        let value = floats(Owner::Point, "rule_value", &output);
        // Each cluster pairs a weight-1 and a weight-3 point: (1 * a + 3 * b) / 4.
        let first = ((0.0f64 * 0.013).sin() * 1.0 + (0.0f64 * 0.013).sin() * 3.0) / 4.0;
        assert!(near(value[0], first));
    }

    #[test]
    fn weld_averages_close_points() {
        let source = points(&[(0., 0., 0.), (0.001, 0., 0.), (5., 0., 0.)]);
        let out = weld(WeldParams { cancel: None, grain: 16_384, selection: None, tolerance: 0.01 }, &source).unwrap();
        assert_eq!(geometry::point_count(&out), 2);
        assert!(near(xs(&out)[0], 0.0005));
        let _ = topology::point_count;
    }
}
