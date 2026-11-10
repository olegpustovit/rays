use rdk_core::{attribute, cancel, edge_group, error, geometry, packed, support, topology, topology_index};
use rdk_core::error::Code;
use crate::point_clusters::Clusters;
use crate::point_grid::{float_max, float_min};

use crate::fuse_rules::{
    self, average_float, expand_f64, select_f64, sort_float_entries,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Position {
    First,
    LeastPoint,
    GreatestPoint,
    Average,
    Minimum,
    Maximum,
    Mode,
    Median,
    Sum,
    SumSquares,
    RootMeanSquare,
    WeightedAverage,
    WeightedSum,
    MinimumWeight,
    MaximumWeight,
}

pub struct Params {
    pub grain: i64,
    pub position: Position,
    pub weight_attribute: Option<String>,
    pub attributes: fuse_rules::DefaultAttributes,
    pub attribute_rules: Vec<fuse_rules::AttributeRule>,
    pub group_rules: Vec<fuse_rules::GroupRule>,
    pub compact: bool,
    pub rewire: bool,
    pub remap_edge_groups: bool,
}

fn fail(message: String) -> error::T {
    error::new("Rdk.Fuse_reduce", Code::InvalidParameter, message)
}

fn average_plane(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    clusters: &Clusters,
    source: &[f64],
) -> Result<Vec<f64>, error::T> {
    let mut output = vec![0.0; clusters.count as usize];
    support::par_fill(cancel_value, (grain / 8).max(1), &mut output, &|cluster| {
        Ok(average_float(clusters, source, cluster as usize))
    })?;
    Ok(output)
}

fn sorted_reduction(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    clusters: &Clusters,
    source: &[f64],
    median: bool,
) -> Result<Vec<f64>, error::T> {
    let mut output = vec![0.0; clusters.count as usize];
    support::par_fill(cancel_value, (grain / 8).max(1), &mut output, &|cluster| {
        let cluster = cluster as usize;
        let first = clusters.offsets[cluster] as usize;
        let last = clusters.offsets[cluster + 1] as usize;
        let mut values: Vec<(f64, i64)> = clusters.members[first..last].iter().map(|&point| (source[point as usize], point)).collect();
        let (first, last) = (0usize, last - first);
        sort_float_entries(&mut values);
        if median {
            Ok(values[first + (last - first) / 2].0)
        } else {
            let mut best_count = 0usize;
            let mut best_first_point = i64::MAX;
            let mut best_value = values[first].0;
            let mut run_first = first;
            for slot in first + 1..=last {
                if slot == last || values[slot].0 != values[run_first].0 {
                    let count = slot - run_first;
                    // Numeric modes merge signed zeros and retain the least source point.
                    let representative = (run_first..slot).min_by_key(|&i| values[i].1).unwrap();
                    let point = values[representative].1;
                    if count > best_count || (count == best_count && point < best_first_point) {
                        best_count = count;
                        best_first_point = point;
                        best_value = values[representative].0;
                    }
                    run_first = slot;
                }
            }
            Ok(best_value)
        }
    })?;
    Ok(output)
}

fn numeric_reduction(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    clusters: &Clusters,
    source: &[f64],
    position: Position,
    weights: Option<&[f64]>,
) -> Result<Vec<f64>, error::T> {
    match position {
        Position::First | Position::LeastPoint => Ok(select_f64(source, &clusters.representatives)),
        Position::GreatestPoint => Ok((0..clusters.count as usize)
            .map(|cluster| source[clusters.members[clusters.offsets[cluster + 1] as usize - 1] as usize])
            .collect()),
        Position::Average => average_plane(cancel_value, grain, clusters, source),
        Position::Median => sorted_reduction(cancel_value, grain, clusters, source, true),
        Position::Mode => sorted_reduction(cancel_value, grain, clusters, source, false),
        Position::MinimumWeight | Position::MaximumWeight => {
            Err(fail("Rdk.Fuse_reduce.fuse: weight positions select a point before reduction".to_owned()))
        }
        _ => {
            let mut output = vec![0.0; clusters.count as usize];
            support::par_fill(cancel_value, (grain / 8).max(1), &mut output, &|cluster| {
                let cluster = cluster as usize;
                let first = clusters.offsets[cluster] as usize;
                let last = clusters.offsets[cluster + 1] as usize;
                let item = |slot: usize| source[clusters.members[slot] as usize];
                let mut value = item(first);
                match position {
                    Position::Minimum | Position::Maximum => {
                        for slot in first + 1..last {
                            value = if position == Position::Minimum {
                                float_min(value, item(slot))
                            } else {
                                float_max(value, item(slot))
                            };
                        }
                    }
                    Position::Sum => {
                        value = 0.0;
                        for slot in first..last {
                            value += item(slot);
                        }
                    }
                    Position::SumSquares => {
                        value = 0.0;
                        for slot in first..last {
                            let x = item(slot);
                            value += x * x;
                        }
                    }
                    Position::RootMeanSquare => {
                        let mut scale = 0.0;
                        for slot in first..last {
                            scale = float_max(scale, item(slot).abs());
                        }
                        let mut squares = 0.0;
                        if scale != 0.0 {
                            for slot in first..last {
                                let x = item(slot) / scale;
                                squares += x * x;
                            }
                        }
                        value = scale * (squares / (last - first) as f64).sqrt();
                    }
                    _ => {
                        let weights = weights.expect("weights");
                        let mut weighted = 0.0;
                        let mut total = 0.0;
                        for slot in first..last {
                            let point = clusters.members[slot] as usize;
                            weighted += source[point] * weights[point];
                            total += weights[point];
                        }
                        if position == Position::WeightedAverage {
                            if total == 0.0 {
                                return Err(fail(format!(
                                    "Rdk.Fuse_reduce.fuse: position weight sum is zero for cluster {cluster}"
                                )));
                            }
                            value = weighted / total;
                        } else {
                            value = weighted;
                        }
                    }
                }
                Ok(value)
            })?;
            Ok(output)
        }
    }
}

fn weight_values(name: &str, geometry_value: &geometry::T) -> Result<Vec<f64>, error::T> {
    let Some(found) = geometry::find_attribute(attribute::Owner::Point, name, geometry_value) else {
        return Err(fail(format!(
            "Rdk.Fuse_reduce.fuse: position weight point attribute {name:?} is missing"
        )));
    };
    let values: Vec<f64> = match attribute::view::storage(&found)? {
        attribute::Storage::Float(values) => values.to_vec(),
        attribute::Storage::Int(values) => values.iter().map(|&v| v as f64).collect(),
        _ => {
            return Err(fail(format!(
                "Rdk.Fuse_reduce.fuse: position weight attribute {name:?} must be scalar float or integer"
            )));
        }
    };
    for (point, value) in values.iter().enumerate() {
        if !value.is_finite() {
            return Err(fail(format!(
                "Rdk.Fuse_reduce.fuse: position weight attribute {name:?} is non-finite at point {point}"
            )));
        }
    }
    Ok(values)
}

fn selected_by_weight(clusters: &Clusters, weights: &[f64], minimum: bool) -> Vec<i64> {
    (0..clusters.count as usize)
        .map(|cluster| {
            let first = clusters.offsets[cluster] as usize;
            let last = clusters.offsets[cluster + 1] as usize;
            let mut selected = clusters.members[first];
            for slot in first + 1..last {
                let point = clusters.members[slot];
                let better = if minimum {
                    weights[point as usize] < weights[selected as usize]
                } else {
                    weights[point as usize] > weights[selected as usize]
                };
                if better {
                    selected = point;
                }
            }
            selected
        })
        .collect()
}

fn validate_positions(x: &[f64], y: &[f64], z: &[f64]) -> Result<(), error::T> {
    for point in 0..x.len() {
        if !(x[point].is_finite() && y[point].is_finite() && z[point].is_finite()) {
            return Err(fail(format!(
                "Rdk.Fuse_reduce.fuse: position reduction is non-finite at output point {point}"
            )));
        }
    }
    Ok(())
}

fn reduce(
    cancel_value: Option<&cancel::T>,
    params: &Params,
    clusters: &Clusters,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let compact = params.compact;
    let rewire = params.rewire;
    let source_count = geometry::point_count(geometry_value) as usize;
    let weighted = matches!(
        params.position,
        Position::WeightedAverage | Position::WeightedSum | Position::MinimumWeight | Position::MaximumWeight
    );
    let weights = if weighted {
        match &params.weight_attribute {
            None => {
                return Err(fail(
                    "Rdk.Fuse_reduce.fuse: weighted position reduction requires a weight attribute".to_owned(),
                ));
            }
            Some(name) if (name).trim().is_empty() => {
                return Err(fail(
                    "Rdk.Fuse_reduce.fuse: position weight attribute name must not be empty".to_owned(),
                ));
            }
            Some(name) => Some(weight_values(name, geometry_value)?),
        }
    } else {
        None
    };
    let positions = packed::float3::view::view(geometry::positions(geometry_value))?;
    let selected = match params.position {
        Position::MinimumWeight => Some(selected_by_weight(clusters, weights.as_deref().expect("w"), true)),
        Position::MaximumWeight => Some(selected_by_weight(clusters, weights.as_deref().expect("w"), false)),
        _ => None,
    };
    let cluster_plane = |source: &[f64]| -> Result<Vec<f64>, error::T> {
        match &selected {
            Some(mapping) => Ok(select_f64(source, mapping)),
            None => numeric_reduction(cancel_value, params.grain, clusters, source, params.position, weights.as_deref()),
        }
    };
    let px = expand_f64(clusters, compact, cluster_plane(&positions.x)?);
    let py = expand_f64(clusters, compact, cluster_plane(&positions.y)?);
    let pz = expand_f64(clusters, compact, cluster_plane(&positions.z)?);
    validate_positions(&px, &py, &pz)?;
    let output_positions = packed::float3::view::of_owned_exn(px, py, pz)?;
    let point_map: Vec<i64> = if compact {
        clusters.of_point.clone()
    } else if rewire {
        (0..source_count)
            .map(|point| clusters.representatives[clusters.of_point[point] as usize])
            .collect()
    } else {
        (0..source_count as i64).collect()
    };
    let source_topology_value = geometry::topology(geometry_value);
    let source_topology = topology::view::view(source_topology_value)?;
    let output_topology = if !compact && !rewire {
        source_topology_value.clone()
    } else {
        let mut vertex_points = vec![0i64; geometry::vertex_count(geometry_value) as usize];
        support::par_fill(cancel_value, params.grain, &mut vertex_points, &|vertex| {
            Ok(point_map[source_topology.vertex_points[vertex as usize] as usize])
        })?;
        topology::view::create_validated_shared(
            if compact { clusters.count } else { source_count as i64 },
            vertex_points,
            source_topology.primitive_offsets.clone(),
            source_topology.primitive_kinds,
        )
    };
    let (output_attributes, output_groups) = fuse_rules::apply(
        cancel_value,
        params.grain,
        params.attributes,
        &params.attribute_rules,
        &params.group_rules,
        compact,
        rewire,
        clusters,
        geometry_value,
    )?;
    let edge_groups = if !params.remap_edge_groups {
        Vec::new()
    } else if !compact && !rewire {
        geometry::edge_groups(geometry_value)
    } else {
        let source_groups = geometry::edge_groups(geometry_value);
        if source_groups.is_empty() {
            Vec::new()
        } else {
            let source_index = topology_index::create(cancel_value, source_topology_value)?;
            let target_index = topology_index::create(cancel_value, &output_topology)?;
            let mut output = Vec::with_capacity(source_groups.len());
            for item in &source_groups {
                output.push(edge_group::remap(
                    cancel_value, &source_index, &output_topology, &target_index, &point_map, item,
                )?);
            }
            output
        }
    };
    geometry::create(
        output_positions,
        output_topology,
        Some(output_attributes),
        Some(output_groups),
        Some(edge_groups),
    )
}

#[doc(hidden)]
pub fn apply(
    cancel_value: Option<&cancel::T>,
    params: &Params,
    clusters: &Clusters,
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    reduce(cancel_value, params, clusters, geometry_value).map_err(|failure| {
        if error::is_cancelled(&failure) || error::code(&failure) == Code::InvalidParameter {
            failure
        } else {
            fail(error::message(&failure).to_owned())
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use crate::test_support::*;
    use rdk_core::attribute::{Owner, Storage};
    use rdk_core::topology::PrimitiveKind;

    fn clusters() -> Clusters {
        Clusters {
            count: 2,
            of_point: vec![0, 0, 1, 0],
            offsets: vec![0, 3, 4],
            members: vec![0, 1, 3, 2],
            representatives: vec![0, 2],
        }
    }

    fn params(position: Position, compact: bool, rewire: bool) -> Params {
        Params {
            grain: 1,
            position,
            weight_attribute: None,
            attributes: fuse_rules::DefaultAttributes::KeepFirst,
            attribute_rules: Vec::new(),
            group_rules: Vec::new(),
            compact,
            rewire,
            remap_edge_groups: true,
        }
    }

    fn line() -> geometry::T {
        let g = geom(
            &[0., 3., 6., 9.],
            &[0., 1., 2., 4.],
            &[0.; 4],
            &[0, 1, 2, 3],
            &[0, 4],
            Some(vec![PrimitiveKind::OpenPolyline]),
        );
        with_attr(&g, Owner::Point, "w", Storage::Float(Arc::new(vec![1., 2., 3., 4.])))
    }

    #[test]
    fn compact_rewires_topology_and_positions() {
        let source = line();
        let out = apply(None, &params(Position::Average, true, true), &clusters(), &source).unwrap();
        assert_eq!(geometry::point_count(&out), 2);
        // Cluster 0 holds points 0, 1, 3; cluster 1 holds point 2.
        assert_eq!(positions(&out).0, vec![4.0, 6.0]);
        assert_eq!(topo(&out).0, vec![0, 0, 1, 0]);
        // Without compaction every source point survives, moved to its cluster value.
        let loose = apply(None, &params(Position::Average, false, true), &clusters(), &source).unwrap();
        assert_eq!(positions(&loose).0, vec![4.0, 4.0, 6.0, 4.0]);
        assert_eq!(topo(&loose).0, vec![0, 0, 2, 0]);
        // Neither compact nor rewired leaves the topology value shared.
        let flat = apply(None, &params(Position::Maximum, false, false), &clusters(), &source).unwrap();
        assert_eq!(topology::data_id(geometry::topology(&flat)), topology::data_id(geometry::topology(&source)));
        let zero_source = geom(&[0., -0., 10., 1.], &[0.; 4], &[0.; 4], &[], &[0], None);
        let mode = apply(None, &params(Position::Mode, true, true), &clusters(), &zero_source).unwrap();
        assert_eq!(positions(&mode).0[0].to_bits(), 0f64.to_bits());
    }

    #[test]
    fn weight_positions_and_errors() {
        let source = line();
        let mut by_weight = params(Position::MaximumWeight, true, true);
        by_weight.weight_attribute = Some("w".to_owned());
        let out = apply(None, &by_weight, &clusters(), &source).unwrap();
        assert_eq!(positions(&out).0, vec![9.0, 6.0]);
        let failure = apply(None, &params(Position::WeightedSum, true, true), &clusters(), &source).err().unwrap();
        assert_eq!(
            error::message(&failure),
            "Rdk.Fuse_reduce.fuse: weighted position reduction requires a weight attribute"
        );
        let mut blank = params(Position::WeightedSum, true, true);
        blank.weight_attribute = Some(" ".to_owned());
        assert!(apply(None, &blank, &clusters(), &source).is_err());
        let mut missing = params(Position::WeightedSum, true, true);
        missing.weight_attribute = Some("nope".to_owned());
        let failure = apply(None, &missing, &clusters(), &source).err().unwrap();
        assert_eq!(
            error::message(&failure),
            "Rdk.Fuse_reduce.fuse: position weight point attribute \"nope\" is missing"
        );
        let mut zero = params(Position::WeightedAverage, true, true);
        zero.weight_attribute = Some("w".to_owned());
        let zeroed = with_attr(&source, Owner::Point, "w", Storage::Float(Arc::new(vec![1., -1., 1., 0.])));
        let failure = apply(None, &zero, &clusters(), &zeroed).err().unwrap();
        assert!(error::message(&failure).contains("position weight sum is zero for cluster 0"));
    }

    #[test]
    fn edge_groups_follow_the_rewire() {
        let source = line();
        let t = geometry::topology(&source).clone();
        let source = geometry::with_edge_group(edge_group_pairs(&t, "e", &[(2, 3)]), &source).unwrap();
        let out = apply(None, &params(Position::First, true, true), &clusters(), &source).unwrap();
        let found = geometry::find_edge_group("e", &out).unwrap();
        assert_eq!(edge_group::cardinality(&found), 1);
        let mut dropped = params(Position::First, true, true);
        dropped.remap_edge_groups = false;
        let out = apply(None, &dropped, &clusters(), &source).unwrap();
        assert!(geometry::find_edge_group("e", &out).is_none());
    }

    #[test]
    fn non_finite_results_and_cancellation_are_reported() {
        let huge = geom(&[1e308, 1e308, 0., 0.], &[0.; 4], &[0.; 4], &[], &[0], None);
        let failure = apply(None, &params(Position::Sum, true, true), &clusters(), &huge).err().unwrap();
        assert_eq!(
            error::message(&failure),
            "Rdk.Fuse_reduce.fuse: position reduction is non-finite at output point 0"
        );
        let token = cancel::create();
        cancel::cancel(&token);
        let failure = apply(Some(&token), &params(Position::First, true, true), &clusters(), &line()).err().unwrap();
        assert_eq!(error::code(&failure), Code::Cancelled);
    }

    #[test]
    fn parallel_exact_at_scale() {
        let (n, k) = (30_000i64, 10_000i64);
        let mut members = Vec::new();
        let mut offsets = vec![0i64];
        for cluster in 0..k {
            for j in 0..n / k {
                members.push(cluster + j * k);
            }
            offsets.push(members.len() as i64);
        }
        let big = Clusters {
            count: k,
            of_point: (0..n).map(|p| p % k).collect(),
            offsets,
            members,
            representatives: (0..k).collect(),
        };
        let x: Vec<f64> = (0..n).map(|p| ((p * 7919) % 101) as f64 * 0.37).collect();
        let y: Vec<f64> = (0..n).map(|p| ((p * 31) % 17) as f64).collect();
        let z: Vec<f64> = (0..n).map(|p| (p % 5) as f64 * 0.25).collect();
        let g = geom(&x, &y, &z, &[0, 1, 2], &[0, 3], None);
        let g = with_attr(&g, Owner::Point, "w", Storage::Float(Arc::new((0..n).map(|p| 1.0 + (p % 5) as f64).collect())));
        for position in [Position::Average, Position::Median, Position::Mode, Position::Sum, Position::RootMeanSquare, Position::WeightedAverage] {
            let mut p = params(position, true, true);
            p.grain = 256;
            p.weight_attribute = Some("w".to_owned());
            let out = exact_at_one_and_four(&|| apply(None, &p, &big, &g).unwrap());
            assert_eq!(geometry::point_count(&out), k, "{position:?}");
        }
    }
}
