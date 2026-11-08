use std::collections::HashMap;

use crate::subdivide_refine::{AFloats, AInts};
use rdk_core::{attribute, attribute_pattern, cancel, error, geometry, group, packed, ragged_ops, support};
use std::sync::Mutex;
use rdk_geom::point_clusters::Clusters;
use rdk_geom::point_grid::{float_max, float_min};

const OCAML_MAX_INT: i64 = (1i64 << 62) - 1;
const OCAML_MIN_INT: i64 = -(1i64 << 62);
const MAX_STRING_LENGTH: i64 = (1i64 << 57) - 9;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DefaultAttributes {
    KeepFirst,
    AverageNumeric,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AttributeMethod {
    Average,
    LeastPoint,
    GreatestPoint,
    Maximum,
    Minimum,
    Mode,
    Median,
    Sum,
    SumSquares,
    RootMeanSquare,
    Concatenate,
    WeightedAverage,
    WeightedSum,
    MinimumWeight,
    MaximumWeight,
    ConcatenateWeightOrder,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AttributeRule {
    pub pattern: String,
    pub method: AttributeMethod,
    pub weight_attribute: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GroupMethod {
    LeastPoint,
    GreatestPoint,
    Union,
    Intersection,
    MostCommon,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GroupRule {
    pub group_pattern: String,
    pub group_method: GroupMethod,
}

struct CompiledAttributeRule {
    pattern: attribute_pattern::T,
    method: AttributeMethod,
    weight: Option<String>,
}

struct CompiledGroupRule {
    pattern: attribute_pattern::T,
    method: GroupMethod,
}

fn fail(message: String) -> error::T {
    error::make(
        "Rdk_mesh.Fuse_rules".to_owned(),
        "invalid_argument".to_owned(),
        format!("Rdk_mesh.Fuse_rules.fuse: {message}"),
        None,
    )
    .unwrap_or_else(|e| e)
}

fn core_fail(failure: error::T) -> error::T {
    if error::code(&failure) == "cancelled" {
        failure
    } else {
        fail(error::message(&failure).to_owned())
    }
}

fn run(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    count: i64,
    operation: &(dyn Fn(i64) -> Result<(), error::T> + Sync),
) -> Result<(), error::T> {
    support::parallel_for(cancel_value, grain, count, operation)
}

fn needs_weight(method: AttributeMethod) -> bool {
    matches!(
        method,
        AttributeMethod::WeightedAverage
            | AttributeMethod::WeightedSum
            | AttributeMethod::MinimumWeight
            | AttributeMethod::MaximumWeight
            | AttributeMethod::ConcatenateWeightOrder
    )
}

fn compile_attribute_rules(rules: &[AttributeRule]) -> Result<Vec<CompiledAttributeRule>, error::T> {
    let mut output = Vec::with_capacity(rules.len());
    for rule in rules {
        let weighted = needs_weight(rule.method);
        if weighted && rule.weight_attribute.is_none() {
            return Err(fail(format!("attribute rule {:?} requires a weight attribute", rule.pattern)));
        }
        if weighted {
            if let Some(name) = &rule.weight_attribute {
                if support::ocaml_trim_is_empty(name) {
                    return Err(fail(format!(
                        "attribute rule {:?} has an empty weight attribute",
                        rule.pattern
                    )));
                }
            }
        }
        output.push(CompiledAttributeRule {
            pattern: attribute_pattern::compile(&rule.pattern).map_err(core_fail)?,
            method: rule.method,
            weight: if weighted { rule.weight_attribute.clone() } else { None },
        });
    }
    Ok(output)
}

fn compile_group_rules(rules: &[GroupRule]) -> Result<Vec<CompiledGroupRule>, error::T> {
    let mut output = Vec::with_capacity(rules.len());
    for rule in rules {
        output.push(CompiledGroupRule {
            pattern: attribute_pattern::compile(&rule.group_pattern).map_err(core_fail)?,
            method: rule.group_method,
        });
    }
    Ok(output)
}

fn find_attribute_rule(rules: &[CompiledAttributeRule], name: &str) -> Option<usize> {
    let mut selected = None;
    for (index, rule) in rules.iter().enumerate() {
        if attribute_pattern::matches(&rule.pattern, name) {
            selected = Some(index);
        }
    }
    selected
}

fn find_group_rule(rules: &[CompiledGroupRule], name: &str) -> Option<usize> {
    let mut selected = None;
    for (index, rule) in rules.iter().enumerate() {
        if attribute_pattern::matches(&rule.pattern, name) {
            selected = Some(index);
        }
    }
    selected
}

fn representative_mapping(clusters: &Clusters, compact: bool) -> Vec<i64> {
    if compact {
        clusters.representatives.clone()
    } else {
        clusters
            .of_point
            .iter()
            .map(|&cluster| clusters.representatives[cluster as usize])
            .collect()
    }
}

pub fn expand_f64(clusters: &Clusters, compact: bool, values: Vec<f64>) -> Vec<f64> {
    if compact {
        values
    } else {
        clusters.of_point.iter().map(|&cluster| values[cluster as usize]).collect()
    }
}

pub fn expand_i64(clusters: &Clusters, compact: bool, values: Vec<i64>) -> Vec<i64> {
    if compact {
        values
    } else {
        clusters.of_point.iter().map(|&cluster| values[cluster as usize]).collect()
    }
}

pub fn expand_string(clusters: &Clusters, compact: bool, values: Vec<Vec<u8>>) -> Vec<Vec<u8>> {
    if compact {
        values
    } else {
        clusters.of_point.iter().map(|&cluster| values[cluster as usize].clone()).collect()
    }
}

fn greatest_mapping(clusters: &Clusters) -> Vec<i64> {
    (0..clusters.count as usize)
        .map(|cluster| clusters.members[clusters.offsets[cluster + 1] as usize - 1])
        .collect()
}

pub fn select_f64(source: &[f64], mapping: &[i64]) -> Vec<f64> {
    mapping.iter().map(|&index| source[index as usize]).collect()
}

pub fn select_i64(source: &[i64], mapping: &[i64]) -> Vec<i64> {
    mapping.iter().map(|&index| source[index as usize]).collect()
}

pub fn select_string(source: &[Vec<u8>], mapping: &[i64]) -> Vec<Vec<u8>> {
    mapping.iter().map(|&index| source[index as usize].clone()).collect()
}

/// OCaml Float.compare: nan equals itself and sorts before every other float.
pub fn float_compare(a: f64, b: f64) -> i64 {
    if a < b {
        -1
    } else if a > b {
        1
    } else if a == b {
        0
    } else if a.is_nan() {
        if b.is_nan() { 0 } else { -1 }
    } else {
        1
    }
}

fn int_compare(a: i64, b: i64) -> i64 {
    if a < b { -1 } else if a > b { 1 } else { 0 }
}

/// Order two slots by value, then by point index.
pub fn compare_float_entry(values: &[f64], points: &[i64], left: usize, right: usize) -> i64 {
    let compared = float_compare(values[left], values[right]);
    if compared != 0 { compared } else { int_compare(points[left], points[right]) }
}

fn compare_int_entry(values: &[i64], points: &[i64], left: usize, right: usize) -> i64 {
    let compared = int_compare(values[left], values[right]);
    if compared != 0 { compared } else { int_compare(points[left], points[right]) }
}

fn compare_text_entry(values: &[Vec<u8>], points: &[i64], left: usize, right: usize) -> i64 {
    let compared = match values[left].cmp(&values[right]) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    };
    if compared != 0 { compared } else { int_compare(points[left], points[right]) }
}

// Heap sort of slots first..last of both planes; the three element types each get one copy.
pub fn sort_range_f64(values: &mut [f64], points: &mut [i64], first: usize, last: usize) {
    let count = last - first;
    if count < 2 {
        return;
    }
    let sift = |values: &mut [f64], points: &mut [i64], root: usize, count: usize| {
        let mut root = root;
        loop {
            let child = root * 2 + 1;
            if child >= count {
                break;
            }
            let selected =
                if child + 1 < count && compare_float_entry(values, points, first + child, first + child + 1) < 0 {
                    child + 1
                } else {
                    child
                };
            if compare_float_entry(values, points, first + root, first + selected) < 0 {
                values.swap(first + root, first + selected);
                points.swap(first + root, first + selected);
                root = selected;
            } else {
                break;
            }
        }
    };
    for root in (0..count / 2).rev() {
        sift(values, points, root, count);
    }
    for remaining in (1..count).rev() {
        values.swap(first, first + remaining);
        points.swap(first, first + remaining);
        sift(values, points, 0, remaining);
    }
}

fn sort_range_i64(values: &mut [i64], points: &mut [i64], first: usize, last: usize) {
    let count = last - first;
    if count < 2 {
        return;
    }
    let sift = |values: &mut [i64], points: &mut [i64], root: usize, count: usize| {
        let mut root = root;
        loop {
            let child = root * 2 + 1;
            if child >= count {
                break;
            }
            let selected =
                if child + 1 < count && compare_int_entry(values, points, first + child, first + child + 1) < 0 {
                    child + 1
                } else {
                    child
                };
            if compare_int_entry(values, points, first + root, first + selected) < 0 {
                values.swap(first + root, first + selected);
                points.swap(first + root, first + selected);
                root = selected;
            } else {
                break;
            }
        }
    };
    for root in (0..count / 2).rev() {
        sift(values, points, root, count);
    }
    for remaining in (1..count).rev() {
        values.swap(first, first + remaining);
        points.swap(first, first + remaining);
        sift(values, points, 0, remaining);
    }
}

fn sort_range_string(values: &mut [Vec<u8>], points: &mut [i64], first: usize, last: usize) {
    let count = last - first;
    if count < 2 {
        return;
    }
    let sift = |values: &mut [Vec<u8>], points: &mut [i64], root: usize, count: usize| {
        let mut root = root;
        loop {
            let child = root * 2 + 1;
            if child >= count {
                break;
            }
            let selected =
                if child + 1 < count && compare_text_entry(values, points, first + child, first + child + 1) < 0 {
                    child + 1
                } else {
                    child
                };
            if compare_text_entry(values, points, first + root, first + selected) < 0 {
                values.swap(first + root, first + selected);
                points.swap(first + root, first + selected);
                root = selected;
            } else {
                break;
            }
        }
    };
    for root in (0..count / 2).rev() {
        sift(values, points, root, count);
    }
    for remaining in (1..count).rev() {
        values.swap(first, first + remaining);
        points.swap(first, first + remaining);
        sift(values, points, 0, remaining);
    }
}

fn sorted_float(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    clusters: &Clusters,
    source: &[f64],
    method: AttributeMethod,
) -> Result<Vec<f64>, error::T> {
    let output = AFloats::new(clusters.count, 0.0);
    run(cancel_value, grain, clusters.count, &|cluster| {
        let first = clusters.offsets[cluster as usize] as usize;
        let last = clusters.offsets[cluster as usize + 1] as usize;
        let mut values: Vec<f64> = clusters.members[first..last].iter().map(|&point| source[point as usize]).collect();
        let mut points = clusters.members[first..last].to_vec();
        let (first, last) = (0usize, last - first);
        sort_range_f64(&mut values, &mut points, first, last);
        if method == AttributeMethod::Median {
            output.set(cluster, values[first + (last - first) / 2]);
        } else {
            let mut best_count = 0usize;
            let mut best_value = values[first];
            let mut run_first = first;
            for slot in first + 1..=last {
                if slot == last || float_compare(values[slot], values[run_first]) != 0 {
                    let count = slot - run_first;
                    if count > best_count {
                        best_count = count;
                        best_value = values[run_first];
                    }
                    run_first = slot;
                }
            }
            output.set(cluster, best_value);
        }
        Ok(())
    })?;
    Ok(output.into_vec())
}

fn sorted_int(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    clusters: &Clusters,
    source: &[i64],
    method: AttributeMethod,
) -> Result<Vec<i64>, error::T> {
    let output = AInts::new(clusters.count, 0);
    run(cancel_value, grain, clusters.count, &|cluster| {
        let first = clusters.offsets[cluster as usize] as usize;
        let last = clusters.offsets[cluster as usize + 1] as usize;
        let mut values: Vec<i64> = clusters.members[first..last].iter().map(|&point| source[point as usize]).collect();
        let mut points = clusters.members[first..last].to_vec();
        let (first, last) = (0usize, last - first);
        sort_range_i64(&mut values, &mut points, first, last);
        if method == AttributeMethod::Median {
            output.set(cluster, values[first + (last - first) / 2]);
        } else {
            let mut best_count = 0usize;
            let mut best_value = values[first];
            let mut run_first = first;
            for slot in first + 1..=last {
                if slot == last || values[slot] != values[run_first] {
                    let count = slot - run_first;
                    if count > best_count {
                        best_count = count;
                        best_value = values[run_first];
                    }
                    run_first = slot;
                }
            }
            output.set(cluster, best_value);
        }
        Ok(())
    })?;
    Ok(output.into_vec())
}

fn sorted_text(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    clusters: &Clusters,
    source: &[Vec<u8>],
    method: AttributeMethod,
) -> Result<Vec<Vec<u8>>, error::T> {
    let output: Vec<Mutex<Vec<u8>>> = (0..clusters.count).map(|_| Mutex::new(Vec::new())).collect();
    run(cancel_value, grain, clusters.count, &|cluster| {
        let first = clusters.offsets[cluster as usize] as usize;
        let last = clusters.offsets[cluster as usize + 1] as usize;
        let mut values: Vec<Vec<u8>> = clusters.members[first..last].iter().map(|&point| source[point as usize].clone()).collect();
        let mut points = clusters.members[first..last].to_vec();
        let (first, last) = (0usize, last - first);
        sort_range_string(&mut values, &mut points, first, last);
        if method == AttributeMethod::Median {
            *output[cluster as usize].lock().unwrap() = values[first + (last - first) / 2].clone();
        } else {
            let mut best_count = 0usize;
            let mut best_value = values[first].clone();
            let mut run_first = first;
            for slot in first + 1..=last {
                if slot == last || values[slot] != values[run_first] {
                    let count = slot - run_first;
                    if count > best_count {
                        best_count = count;
                        best_value = values[run_first].clone();
                    }
                    run_first = slot;
                }
            }
            *output[cluster as usize].lock().unwrap() = best_value;
        }
        Ok(())
    })?;
    Ok(output.into_iter().map(|slot| slot.into_inner().unwrap()).collect())
}

fn weight_values(geometry_value: &geometry::T, name: &str) -> Result<Vec<f64>, error::T> {
    let Some(found) = geometry::find_attribute(attribute::Owner::Point, name, geometry_value) else {
        return Err(fail(format!("attribute-rule weight point attribute {name:?} is missing")));
    };
    let values: Vec<f64> = match attribute::View::storage(&found).map_err(core_fail)? {
        attribute::Storage::Float(values) => values.clone(),
        attribute::Storage::Int(values) => values.iter().map(|&v| v as f64).collect(),
        _ => return Err(fail(format!("attribute-rule weight {name:?} must be scalar float or integer"))),
    };
    for (point, value) in values.iter().enumerate() {
        if !value.is_finite() {
            return Err(fail(format!("attribute-rule weight {name:?} is non-finite at point {point}")));
        }
    }
    Ok(values)
}

pub fn validate(
    attribute_rules: &[AttributeRule],
    group_rules: &[GroupRule],
    _geometry: &geometry::T,
) -> Result<(), error::T> {
    compile_attribute_rules(attribute_rules)?;
    compile_group_rules(group_rules)?;
    Ok(())
}

fn weight_mapping(clusters: &Clusters, weights: &[f64], minimum: bool) -> Vec<i64> {
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

fn source_mapping(clusters: &Clusters, method: AttributeMethod, weights: Option<&[f64]>) -> Option<Vec<i64>> {
    match method {
        AttributeMethod::LeastPoint => Some(clusters.representatives.clone()),
        AttributeMethod::GreatestPoint => Some(greatest_mapping(clusters)),
        AttributeMethod::MinimumWeight => Some(weight_mapping(clusters, weights.expect("weights"), true)),
        AttributeMethod::MaximumWeight => Some(weight_mapping(clusters, weights.expect("weights"), false)),
        _ => None,
    }
}

fn validate_float(name: &str, source: &[f64]) -> Result<(), error::T> {
    for (point, value) in source.iter().enumerate() {
        if !value.is_finite() {
            return Err(fail(format!("attribute {name:?} is non-finite at point {point}")));
        }
    }
    Ok(())
}

pub fn average_float(clusters: &Clusters, source: &[f64], cluster: usize) -> f64 {
    let first = clusters.offsets[cluster] as usize;
    let last = clusters.offsets[cluster + 1] as usize;
    let mut sum = 0.0;
    for slot in first..last {
        sum += source[clusters.members[slot] as usize];
    }
    if sum.is_finite() {
        sum / (last - first) as f64
    } else {
        let mut scale = 0.0;
        for slot in first..last {
            scale = float_max(scale, source[clusters.members[slot] as usize].abs());
        }
        let mut normalized = 0.0;
        if scale != 0.0 {
            for slot in first..last {
                normalized += source[clusters.members[slot] as usize] / scale;
            }
        }
        normalized / (last - first) as f64 * scale
    }
}

fn reduce_float(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    name: &str,
    clusters: &Clusters,
    source: &[f64],
    method: AttributeMethod,
    weights: Option<&[f64]>,
) -> Result<Vec<f64>, error::T> {
    validate_float(name, source)?;
    if let Some(mapping) = source_mapping(clusters, method, weights) {
        return Ok(select_f64(source, &mapping));
    }
    if method == AttributeMethod::Mode || method == AttributeMethod::Median {
        return sorted_float(cancel_value, grain, clusters, source, method);
    }
    let output = AFloats::new(clusters.count, 0.0);
    run(cancel_value, grain, clusters.count, &|cluster| {
        let cluster = cluster as usize;
        let first = clusters.offsets[cluster] as usize;
        let last = clusters.offsets[cluster + 1] as usize;
        let item = |slot: usize| source[clusters.members[slot] as usize];
        let value = match method {
            AttributeMethod::Average => average_float(clusters, source, cluster),
            AttributeMethod::Minimum | AttributeMethod::Maximum => {
                let mut value = item(first);
                for slot in first + 1..last {
                    value = if method == AttributeMethod::Minimum {
                        float_min(value, item(slot))
                    } else {
                        float_max(value, item(slot))
                    };
                }
                value
            }
            AttributeMethod::Sum | AttributeMethod::SumSquares => {
                let mut value = 0.0;
                for slot in first..last {
                    let x = item(slot);
                    value += if method == AttributeMethod::SumSquares { x * x } else { x };
                }
                value
            }
            AttributeMethod::RootMeanSquare => {
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
                scale * (squares / (last - first) as f64).sqrt()
            }
            AttributeMethod::WeightedAverage | AttributeMethod::WeightedSum => {
                let weights = weights.expect("weights");
                let mut value = 0.0;
                let mut total = 0.0;
                for slot in first..last {
                    let point = clusters.members[slot] as usize;
                    value += source[point] * weights[point];
                    total += weights[point];
                }
                if method == AttributeMethod::WeightedAverage {
                    if total == 0.0 {
                        return Err(fail(format!(
                            "attribute {name:?} weighted-average denominator is zero in cluster {cluster}"
                        )));
                    }
                    value / total
                } else {
                    value
                }
            }
            _ => return Err(fail("reduction method is handled before the cluster loop".to_owned())),
        };
        if !value.is_finite() {
            return Err(fail(format!("attribute {name:?} reduction is non-finite in cluster {cluster}")));
        }
        output.set(cluster as i64, value);
        Ok(())
    })?;
    Ok(output.into_vec())
}

fn checked_add(name: &str, cluster: usize, left: i64, right: i64) -> Result<i64, error::T> {
    if (right > 0 && left > OCAML_MAX_INT - right) || (right < 0 && left < OCAML_MIN_INT - right) {
        return Err(fail(format!("integer attribute {name:?} overflows in cluster {cluster}")));
    }
    Ok(left + right)
}

fn checked_square(name: &str, cluster: usize, value: i64) -> Result<i64, error::T> {
    let overflow = || fail(format!("integer attribute {name:?} square overflows in cluster {cluster}"));
    if value == OCAML_MIN_INT {
        return Err(overflow());
    }
    let absolute = value.abs();
    if absolute != 0 && absolute > OCAML_MAX_INT / absolute {
        return Err(overflow());
    }
    Ok(absolute * absolute)
}

fn int_of_float_checked(name: &str, cluster: usize, value: f64) -> Result<i64, error::T> {
    let lower = OCAML_MIN_INT as f64;
    let upper = -lower;
    if !value.is_finite() || value < lower || value >= upper {
        return Err(fail(format!("integer attribute {name:?} reduction overflows in cluster {cluster}")));
    }
    Ok(value as i64)
}

fn reduce_int(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    name: &str,
    clusters: &Clusters,
    source: &[i64],
    method: AttributeMethod,
    weights: Option<&[f64]>,
) -> Result<Vec<i64>, error::T> {
    if let Some(mapping) = source_mapping(clusters, method, weights) {
        return Ok(select_i64(source, &mapping));
    }
    if method == AttributeMethod::Mode || method == AttributeMethod::Median {
        return sorted_int(cancel_value, grain, clusters, source, method);
    }
    let output = AInts::new(clusters.count, 0);
    run(cancel_value, grain, clusters.count, &|cluster| {
        let cluster = cluster as usize;
        let first = clusters.offsets[cluster] as usize;
        let last = clusters.offsets[cluster + 1] as usize;
        let item = |slot: usize| source[clusters.members[slot] as usize];
        let value = match method {
            AttributeMethod::Minimum | AttributeMethod::Maximum => {
                let mut value = item(first);
                for slot in first + 1..last {
                    value = if method == AttributeMethod::Minimum { value.min(item(slot)) } else { value.max(item(slot)) };
                }
                value
            }
            AttributeMethod::Average => {
                let count = (last - first) as i64;
                let mut quotients = 0i64;
                let mut remainders = 0i64;
                for slot in first..last {
                    let value = item(slot);
                    quotients = checked_add(name, cluster, quotients, value / count)?;
                    remainders = checked_add(name, cluster, remainders, value % count)?;
                }
                checked_add(name, cluster, quotients, remainders / count)?
            }
            AttributeMethod::Sum => {
                let mut value = 0i64;
                for slot in first..last {
                    value = checked_add(name, cluster, value, item(slot))?;
                }
                value
            }
            AttributeMethod::SumSquares => {
                let mut value = 0i64;
                for slot in first..last {
                    let square = checked_square(name, cluster, item(slot))?;
                    value = checked_add(name, cluster, value, square)?;
                }
                value
            }
            AttributeMethod::RootMeanSquare => {
                let mut scale = 0.0;
                for slot in first..last {
                    scale = float_max(scale, (item(slot) as f64).abs());
                }
                let mut squares = 0.0;
                if scale != 0.0 {
                    for slot in first..last {
                        let x = item(slot) as f64 / scale;
                        squares += x * x;
                    }
                }
                int_of_float_checked(name, cluster, scale * (squares / (last - first) as f64).sqrt())?
            }
            AttributeMethod::WeightedAverage | AttributeMethod::WeightedSum => {
                let weights = weights.expect("weights");
                let mut value = 0.0;
                let mut total = 0.0;
                for slot in first..last {
                    let point = clusters.members[slot] as usize;
                    value += source[point] as f64 * weights[point];
                    total += weights[point];
                }
                if method == AttributeMethod::WeightedAverage {
                    if total == 0.0 {
                        return Err(fail(format!(
                            "attribute {name:?} weighted-average denominator is zero in cluster {cluster}"
                        )));
                    }
                    int_of_float_checked(name, cluster, value / total)?
                } else {
                    int_of_float_checked(name, cluster, value)?
                }
            }
            _ => return Err(fail("reduction method is handled before the cluster loop".to_owned())),
        };
        output.set(cluster as i64, value);
        Ok(())
    })?;
    Ok(output.into_vec())
}

fn concatenate_text(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    clusters: &Clusters,
    source: &[Vec<u8>],
    ordered_members: &[i64],
) -> Result<Vec<Vec<u8>>, error::T> {
    let output: Vec<Mutex<Vec<u8>>> = (0..clusters.count).map(|_| Mutex::new(Vec::new())).collect();
    run(cancel_value, grain, clusters.count, &|cluster| {
        let first = clusters.offsets[cluster as usize] as usize;
        let last = clusters.offsets[cluster as usize + 1] as usize;
        let mut length = 0i64;
        for slot in first..last {
            let item = source[ordered_members[slot] as usize].len() as i64;
            if item > MAX_STRING_LENGTH - length {
                return Err(fail("concatenated text exceeds string limits".to_owned()));
            }
            length += item;
        }
        let mut bytes = Vec::with_capacity(length as usize);
        for slot in first..last {
            bytes.extend_from_slice(&source[ordered_members[slot] as usize]);
        }
        *output[cluster as usize].lock().unwrap() = bytes;
        Ok(())
    })?;
    Ok(output.into_iter().map(|slot| slot.into_inner().unwrap()).collect())
}

fn reduce_text(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    name: &str,
    clusters: &Clusters,
    source: &[Vec<u8>],
    method: AttributeMethod,
    weights: Option<&[f64]>,
    ordered_members: &[i64],
) -> Result<Vec<Vec<u8>>, error::T> {
    if let Some(mapping) = source_mapping(clusters, method, weights) {
        return Ok(select_string(source, &mapping));
    }
    match method {
        AttributeMethod::Mode | AttributeMethod::Median => sorted_text(cancel_value, grain, clusters, source, method),
        AttributeMethod::Minimum | AttributeMethod::Maximum => Ok((0..clusters.count as usize)
            .map(|cluster| {
                let first = clusters.offsets[cluster] as usize;
                let last = clusters.offsets[cluster + 1] as usize;
                let mut value = &source[clusters.members[first] as usize];
                for slot in first + 1..last {
                    let candidate = &source[clusters.members[slot] as usize];
                    let replace = if method == AttributeMethod::Minimum {
                        candidate < value
                    } else {
                        candidate > value
                    };
                    if replace {
                        value = candidate;
                    }
                }
                value.clone()
            })
            .collect()),
        AttributeMethod::Concatenate | AttributeMethod::ConcatenateWeightOrder => {
            concatenate_text(cancel_value, grain, clusters, source, ordered_members)
        }
        _ => Err(fail(format!("attribute {name:?} method is unsupported for text storage"))),
    }
}

fn weighted_order(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    clusters: &Clusters,
    weights: &[f64],
) -> Result<Vec<i64>, error::T> {
    let points = AInts::new(clusters.members.len() as i64, 0);
    run(cancel_value, grain, clusters.count, &|cluster| {
        let first = clusters.offsets[cluster as usize] as usize;
        let last = clusters.offsets[cluster as usize + 1] as usize;
        let mut values: Vec<f64> = clusters.members[first..last].iter().map(|&point| weights[point as usize]).collect();
        let mut order = clusters.members[first..last].to_vec();
        sort_range_f64(&mut values, &mut order, 0, last - first);
        for (offset, &point) in order.iter().enumerate() {
            points.set((first + offset) as i64, point);
        }
        Ok(())
    })?;
    Ok(points.into_vec())
}

fn output_offsets(clusters: &Clusters, compact: bool, cluster_lengths: &[i64]) -> Result<Vec<i64>, error::T> {
    let count = if compact { clusters.count as usize } else { clusters.of_point.len() };
    let mut offsets = vec![0i64; count + 1];
    for output in 0..count {
        let cluster = if compact { output } else { clusters.of_point[output] as usize };
        let length = cluster_lengths[cluster];
        let Some(next) = offsets[output].checked_add(length) else {
            return Err(fail("concatenated attribute exceeds array limits".to_owned()));
        };
        offsets[output + 1] = next;
    }
    Ok(offsets)
}

fn output_count(clusters: &Clusters, compact: bool) -> usize {
    if compact { clusters.count as usize } else { clusters.of_point.len() }
}

fn cluster_of_output(clusters: &Clusters, compact: bool, output: usize) -> usize {
    if compact { output } else { clusters.of_point[output] as usize }
}

fn concatenate_int_scalar(
    clusters: &Clusters,
    compact: bool,
    source: &[i64],
    ordered: &[i64],
) -> Result<packed::IntArray::T, error::T> {
    let lengths: Vec<i64> = (0..clusters.count as usize)
        .map(|c| clusters.offsets[c + 1] - clusters.offsets[c])
        .collect();
    let offsets = output_offsets(clusters, compact, &lengths)?;
    let count = output_count(clusters, compact);
    let mut values = vec![0i64; offsets[count] as usize];
    for output in 0..count {
        let cluster = cluster_of_output(clusters, compact, output);
        let mut at = offsets[output] as usize;
        for slot in clusters.offsets[cluster]..clusters.offsets[cluster + 1] {
            values[at] = source[ordered[slot as usize] as usize];
            at += 1;
        }
    }
    Ok(packed::IntArray::View::create_validated_owned(offsets, values))
}

fn concatenate_float_scalar(
    clusters: &Clusters,
    compact: bool,
    source: &[f64],
    ordered: &[i64],
) -> Result<packed::FloatArray::T, error::T> {
    let lengths: Vec<i64> = (0..clusters.count as usize)
        .map(|c| clusters.offsets[c + 1] - clusters.offsets[c])
        .collect();
    let offsets = output_offsets(clusters, compact, &lengths)?;
    let count = output_count(clusters, compact);
    let mut values = vec![0.0f64; offsets[count] as usize];
    for output in 0..count {
        let cluster = cluster_of_output(clusters, compact, output);
        let mut at = offsets[output] as usize;
        for slot in clusters.offsets[cluster]..clusters.offsets[cluster + 1] {
            values[at] = source[ordered[slot as usize] as usize];
            at += 1;
        }
    }
    Ok(packed::FloatArray::View::create_validated_owned(offsets, values))
}

fn concatenate_int_rows(
    clusters: &Clusters,
    compact: bool,
    source: &packed::IntArray::T,
    ordered: &[i64],
) -> Result<packed::IntArray::T, error::T> {
    let view = packed::IntArray::View::view(source);
    let mut lengths = vec![0i64; clusters.count as usize];
    for cluster in 0..clusters.count as usize {
        for slot in clusters.offsets[cluster]..clusters.offsets[cluster + 1] {
            let point = ordered[slot as usize] as usize;
            let length = view.offsets[point + 1] - view.offsets[point];
            let Some(next) = lengths[cluster].checked_add(length) else {
                return Err(fail("concatenated integer array exceeds array limits".to_owned()));
            };
            lengths[cluster] = next;
        }
    }
    let offsets = output_offsets(clusters, compact, &lengths)?;
    let count = output_count(clusters, compact);
    let mut values = vec![0i64; offsets[count] as usize];
    for output in 0..count {
        let cluster = cluster_of_output(clusters, compact, output);
        let mut at = offsets[output] as usize;
        for slot in clusters.offsets[cluster]..clusters.offsets[cluster + 1] {
            let point = ordered[slot as usize] as usize;
            let first = view.offsets[point] as usize;
            let length = (view.offsets[point + 1] - view.offsets[point]) as usize;
            values[at..at + length].copy_from_slice(&view.values[first..first + length]);
            at += length;
        }
    }
    Ok(packed::IntArray::View::create_validated_owned(offsets, values))
}

fn concatenate_float_rows(
    clusters: &Clusters,
    compact: bool,
    source: &packed::FloatArray::T,
    ordered: &[i64],
) -> Result<packed::FloatArray::T, error::T> {
    let view = packed::FloatArray::View::view(source);
    let mut lengths = vec![0i64; clusters.count as usize];
    for cluster in 0..clusters.count as usize {
        for slot in clusters.offsets[cluster]..clusters.offsets[cluster + 1] {
            let point = ordered[slot as usize] as usize;
            let length = view.offsets[point + 1] - view.offsets[point];
            let Some(next) = lengths[cluster].checked_add(length) else {
                return Err(fail("concatenated float array exceeds array limits".to_owned()));
            };
            lengths[cluster] = next;
        }
    }
    let offsets = output_offsets(clusters, compact, &lengths)?;
    let count = output_count(clusters, compact);
    let mut values = vec![0.0f64; offsets[count] as usize];
    for output in 0..count {
        let cluster = cluster_of_output(clusters, compact, output);
        let mut at = offsets[output] as usize;
        for slot in clusters.offsets[cluster]..clusters.offsets[cluster + 1] {
            let point = ordered[slot as usize] as usize;
            let first = view.offsets[point] as usize;
            let length = (view.offsets[point + 1] - view.offsets[point]) as usize;
            values[at..at + length].copy_from_slice(&view.values[first..first + length]);
            at += length;
        }
    }
    Ok(packed::FloatArray::View::create_validated_owned(offsets, values))
}

fn ordered_members(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    clusters: &Clusters,
    method: AttributeMethod,
    weights: Option<&[f64]>,
) -> Result<Vec<i64>, error::T> {
    if method == AttributeMethod::ConcatenateWeightOrder {
        weighted_order(cancel_value, grain, clusters, weights.expect("weights"))
    } else {
        Ok(clusters.members.clone())
    }
}

fn is_concatenate(method: AttributeMethod) -> bool {
    method == AttributeMethod::Concatenate || method == AttributeMethod::ConcatenateWeightOrder
}

fn reduced_attribute(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    compact: bool,
    clusters: &Clusters,
    geometry_value: &geometry::T,
    weights_cache: &mut HashMap<String, Vec<f64>>,
    rule: &CompiledAttributeRule,
    item: &attribute::T,
) -> Result<attribute::T, error::T> {
    let name = attribute::name(item).to_owned();
    let method = rule.method;
    if let Some(weight_name) = &rule.weight {
        if !weights_cache.contains_key(weight_name) {
            let values = weight_values(geometry_value, weight_name)?;
            weights_cache.insert(weight_name.clone(), values);
        }
    }
    let weights: Option<&[f64]> = rule.weight.as_ref().map(|w| weights_cache[w].as_slice());
    let ordered = ordered_members(cancel_value, grain, clusters, method, weights)?;
    let plane_float = |source: &[f64]| -> Result<Vec<f64>, error::T> {
        Ok(expand_f64(
            clusters,
            compact,
            reduce_float(cancel_value, grain, &name, clusters, source, method, weights)?,
        ))
    };
    let plane_int = |source: &[i64]| -> Result<Vec<i64>, error::T> {
        Ok(expand_i64(clusters, compact, reduce_int(cancel_value, grain, &name, clusters, source, method, weights)?))
    };
    let concat_scalar_message = || fail(format!("attribute {name:?} concatenate requires scalar or array storage"));
    let storage = match attribute::View::storage(item).map_err(core_fail)? {
        attribute::Storage::Float(values) if is_concatenate(method) => attribute::Storage::FloatArray(
            concatenate_float_scalar(clusters, compact, values, &ordered)?,
        ),
        attribute::Storage::Int(values) if is_concatenate(method) => {
            attribute::Storage::IntArray(concatenate_int_scalar(clusters, compact, values, &ordered)?)
        }
        attribute::Storage::Float(values) => attribute::Storage::Float(plane_float(values)?),
        attribute::Storage::Int(values) => attribute::Storage::Int(plane_int(values)?),
        attribute::Storage::Text(values) => attribute::Storage::Text(expand_string(
            clusters,
            compact,
            reduce_text(cancel_value, grain, &name, clusters, values, method, weights, &ordered)?,
        )),
        attribute::Storage::Float2(values) => {
            if is_concatenate(method) {
                return Err(concat_scalar_message());
            }
            let view = packed::Float2::View::view(values);
            attribute::Storage::Float2(
                packed::Float2::of_owned(plane_float(&view.x)?, plane_float(&view.y)?).map_err(core_fail)?,
            )
        }
        attribute::Storage::Float3(values) => {
            if is_concatenate(method) {
                return Err(concat_scalar_message());
            }
            let view = packed::Float3::View::view(values).map_err(core_fail)?;
            attribute::Storage::Float3(
                packed::Float3::View::of_owned_exn(
                    plane_float(&view.x)?,
                    plane_float(&view.y)?,
                    plane_float(&view.z)?,
                )
                .map_err(core_fail)?,
            )
        }
        attribute::Storage::Float4(values) => {
            if is_concatenate(method) {
                return Err(concat_scalar_message());
            }
            let view = packed::Float4::View::view(values);
            attribute::Storage::Float4(
                packed::Float4::of_owned(
                    plane_float(&view.x)?,
                    plane_float(&view.y)?,
                    plane_float(&view.z)?,
                    plane_float(&view.w)?,
                )
                .map_err(core_fail)?,
            )
        }
        attribute::Storage::IntArray(values) => match method {
            AttributeMethod::Concatenate | AttributeMethod::ConcatenateWeightOrder => {
                attribute::Storage::IntArray(concatenate_int_rows(clusters, compact, values, &ordered)?)
            }
            AttributeMethod::LeastPoint
            | AttributeMethod::GreatestPoint
            | AttributeMethod::MinimumWeight
            | AttributeMethod::MaximumWeight => {
                let mapping = source_mapping(clusters, method, weights).expect("mapping");
                let mapping = if compact { mapping } else { expand_i64(clusters, false, mapping) };
                attribute::Storage::IntArray(
                    ragged_ops::remap_int(cancel_value, Some(grain), &mapping, values).map_err(core_fail)?,
                )
            }
            _ => return Err(fail(format!("attribute {name:?} method is unsupported for integer arrays"))),
        },
        attribute::Storage::FloatArray(values) => match method {
            AttributeMethod::Concatenate | AttributeMethod::ConcatenateWeightOrder => {
                attribute::Storage::FloatArray(concatenate_float_rows(clusters, compact, values, &ordered)?)
            }
            AttributeMethod::LeastPoint
            | AttributeMethod::GreatestPoint
            | AttributeMethod::MinimumWeight
            | AttributeMethod::MaximumWeight => {
                let mapping = source_mapping(clusters, method, weights).expect("mapping");
                let mapping = if compact { mapping } else { expand_i64(clusters, false, mapping) };
                attribute::Storage::FloatArray(
                    ragged_ops::remap_float(cancel_value, Some(grain), &mapping, values).map_err(core_fail)?,
                )
            }
            _ => return Err(fail(format!("attribute {name:?} method is unsupported for float arrays"))),
        },
    };
    attribute::create_owned(&name, attribute::Owner::Point, storage).map_err(core_fail)
}

fn default_attribute(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    compact: bool,
    clusters: &Clusters,
    default: DefaultAttributes,
    item: &attribute::T,
) -> Result<attribute::T, error::T> {
    let representatives = representative_mapping(clusters, compact);
    let average_numeric = default == DefaultAttributes::AverageNumeric;
    let average = |source: &[f64]| -> Vec<f64> {
        expand_f64(
            clusters,
            compact,
            (0..clusters.count as usize).map(|c| average_float(clusters, source, c)).collect(),
        )
    };
    let first = |source: &[f64]| select_f64(source, &representatives);
    let plane = |source: &[f64]| if average_numeric { average(source) } else { first(source) };
    let storage = match attribute::View::storage(item).map_err(core_fail)? {
        attribute::Storage::Float(values) => attribute::Storage::Float(plane(values)),
        attribute::Storage::Int(values) => attribute::Storage::Int(select_i64(values, &representatives)),
        attribute::Storage::Text(values) => {
            attribute::Storage::Text(select_string(values, &representatives))
        }
        attribute::Storage::Float2(values) => {
            let view = packed::Float2::View::view(values);
            attribute::Storage::Float2(
                packed::Float2::of_owned(plane(&view.x), plane(&view.y)).map_err(core_fail)?,
            )
        }
        attribute::Storage::Float3(values) => {
            let view = packed::Float3::View::view(values).map_err(core_fail)?;
            let mut x = plane(&view.x);
            let mut y = plane(&view.y);
            let mut z = plane(&view.z);
            if average_numeric && attribute::name(item) == "N" {
                for point in 0..x.len() as i64 {
                    cancel::poll(cancel_value, point, 4095)?;
                    let p = point as usize;
                    let length = ((x[p] * x[p]) + (y[p] * y[p]) + (z[p] * z[p])).sqrt();
                    if length > 1e-20 {
                        x[p] /= length;
                        y[p] /= length;
                        z[p] /= length;
                    }
                }
            }
            let _ = grain;
            attribute::Storage::Float3(packed::Float3::View::of_owned_exn(x, y, z).map_err(core_fail)?)
        }
        attribute::Storage::Float4(values) => {
            let view = packed::Float4::View::view(values);
            attribute::Storage::Float4(
                packed::Float4::of_owned(plane(&view.x), plane(&view.y), plane(&view.z), plane(&view.w))
                    .map_err(core_fail)?,
            )
        }
        attribute::Storage::IntArray(values) => attribute::Storage::IntArray(
            ragged_ops::remap_int(cancel_value, Some(grain), &representatives, values).map_err(core_fail)?,
        ),
        attribute::Storage::FloatArray(values) => attribute::Storage::FloatArray(
            ragged_ops::remap_float(cancel_value, Some(grain), &representatives, values).map_err(core_fail)?,
        ),
    };
    attribute::create_owned(attribute::name(item), attribute::Owner::Point, storage).map_err(core_fail)
}

fn cluster_membership(clusters: &Clusters, item: &group::T, method: GroupMethod, cluster: i64) -> bool {
    let first = clusters.offsets[cluster as usize] as usize;
    let last = clusters.offsets[cluster as usize + 1] as usize;
    match method {
        GroupMethod::LeastPoint => group::mem(clusters.members[first], item),
        GroupMethod::GreatestPoint => group::mem(clusters.members[last - 1], item),
        _ => {
            let members = (first..last).filter(|&slot| group::mem(clusters.members[slot], item)).count();
            match method {
                GroupMethod::Union => members > 0,
                GroupMethod::Intersection => members == last - first,
                _ => members * 2 > last - first,
            }
        }
    }
}

fn with_cluster_order(
    clusters: &Clusters,
    compact: bool,
    source: &group::T,
    target: group::T,
) -> Result<group::T, error::T> {
    let Some(source_order) = group::View::order_view(source) else {
        return Ok(target);
    };
    let cluster_count = clusters.count as usize;
    let mut seen = vec![false; cluster_count];
    let mut order = Vec::with_capacity(group::cardinality(&target) as usize);
    let mut visit = |cluster: usize, order: &mut Vec<i64>| {
        if seen[cluster] {
            return;
        }
        seen[cluster] = true;
        if compact {
            if group::mem(cluster as i64, &target) {
                order.push(cluster as i64);
            }
        } else if group::mem(clusters.members[clusters.offsets[cluster] as usize], &target) {
            for slot in clusters.offsets[cluster]..clusters.offsets[cluster + 1] {
                order.push(clusters.members[slot as usize]);
            }
        }
    };
    for &point in source_order {
        visit(clusters.of_point[point as usize] as usize, &mut order);
    }
    for cluster in 0..cluster_count {
        visit(cluster, &mut order);
    }
    group::View::with_owned_order(order, &target).map_err(core_fail)
}

fn reduced_group(
    grain: i64,
    compact: bool,
    clusters: &Clusters,
    method: GroupMethod,
    item: &group::T,
) -> Result<group::T, error::T> {
    let cluster_group = group::init(
        Some(grain),
        group::Owner::Point,
        group::name(item),
        clusters.count,
        &|cluster| cluster_membership(clusters, item, method, cluster),
    )
    .map_err(core_fail)?;
    let target = if compact {
        cluster_group
    } else {
        group::init(
            Some(grain),
            group::Owner::Point,
            group::name(item),
            clusters.of_point.len() as i64,
            &|point| group::mem(clusters.of_point[point as usize], &cluster_group),
        )
        .map_err(core_fail)?
    };
    with_cluster_order(clusters, compact, item, target)
}

/// Apply point-attribute and point-group cluster policies. Failures carry the Invalid_argument
/// message of the original.
pub fn apply(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    default_attributes: DefaultAttributes,
    attribute_rules: &[AttributeRule],
    group_rules: &[GroupRule],
    compact: bool,
    rewire: bool,
    clusters: &Clusters,
    geometry_value: &geometry::T,
) -> Result<(Vec<attribute::T>, Vec<group::T>), error::T> {
    let attribute_rules = compile_attribute_rules(attribute_rules)?;
    let group_rules = compile_group_rules(group_rules)?;
    let mut weights_cache: HashMap<String, Vec<f64>> = HashMap::new();
    let mut attributes = Vec::new();
    for item in geometry::attributes(geometry_value) {
        if attribute::owner(&item) != attribute::Owner::Point {
            attributes.push(item);
            continue;
        }
        match find_attribute_rule(&attribute_rules, attribute::name(&item)) {
            Some(rule) => attributes.push(reduced_attribute(
                cancel_value, grain, compact, clusters, geometry_value, &mut weights_cache, &attribute_rules[rule], &item,
            )?),
            None if !attribute_rules.is_empty() && !compact && !rewire => attributes.push(item),
            None => attributes.push(default_attribute(
                cancel_value, grain, compact, clusters, default_attributes, &item,
            )?),
        }
    }
    let mut groups = Vec::new();
    for item in geometry::groups(geometry_value) {
        if group::owner(&item) != group::Owner::Point {
            groups.push(item);
            continue;
        }
        match find_group_rule(&group_rules, group::name(&item)) {
            Some(rule) => groups.push(reduced_group(grain, compact, clusters, group_rules[rule].method, &item)?),
            None if !group_rules.is_empty() && !compact && !rewire => groups.push(item),
            None => groups.push(reduced_group(grain, compact, clusters, GroupMethod::Union, &item)?),
        }
    }
    Ok((attributes, groups))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compact_points::fixtures::*;
    use rdk_core::attribute::{Owner, Storage};

    fn clusters() -> Clusters {
        Clusters {
            count: 2,
            of_point: vec![0, 0, 1, 0],
            offsets: vec![0, 3, 4],
            members: vec![0, 1, 3, 2],
            representatives: vec![0, 2],
        }
    }

    fn cloud() -> geometry::T {
        let g = geom(&[0., 1., 2., 3.], &[0.; 4], &[0.; 4], &[], &[0], None);
        let g = with_attr(&g, Owner::Point, "f", Storage::Float(vec![1., 5., 10., 2.]));
        let g = with_attr(&g, Owner::Point, "i", Storage::Int(vec![3, 3, 7, 9]));
        let text = ["b", "a", "z", "c"].iter().map(|s| s.as_bytes().to_vec()).collect();
        let g = with_attr(&g, Owner::Point, "t", Storage::Text(text));
        let g = with_attr(&g, Owner::Point, "w", Storage::Float(vec![1., 1., 1., 4.]));
        geometry::with_group(group::init(None, group::Owner::Point, "g1", 4, &|p| p == 0 || p == 2).unwrap(), &g).unwrap()
    }

    fn rule(pattern: &str, method: AttributeMethod) -> AttributeRule {
        AttributeRule { pattern: pattern.to_owned(), method, weight_attribute: Some("w".to_owned()) }
    }

    fn reduce(
        pattern: &str,
        method: AttributeMethod,
        compact: bool,
    ) -> Result<Vec<attribute::T>, error::T> {
        apply(
            None,
            1,
            DefaultAttributes::KeepFirst,
            &[rule(pattern, method)],
            &[],
            compact,
            true,
            &clusters(),
            &cloud(),
        )
        .map(|(attributes, _)| attributes)
    }

    fn find(list: &[attribute::T], name: &str) -> attribute::T {
        list.iter().find(|a| attribute::name(a) == name).unwrap().clone()
    }

    fn floats_of(list: &[attribute::T], name: &str) -> Vec<f64> {
        match attribute::View::storage(&find(list, name)).unwrap() {
            Storage::Float(values) => values.clone(),
            _ => panic!("not float"),
        }
    }

    fn ints_of(list: &[attribute::T], name: &str) -> Vec<i64> {
        match attribute::View::storage(&find(list, name)).unwrap() {
            Storage::Int(values) => values.clone(),
            _ => panic!("not int"),
        }
    }

    fn near(a: f64, b: f64) -> bool {
        (a - b).abs() < 1e-12
    }

    #[test]
    fn float_methods_per_cluster() {
        use AttributeMethod::*;
        let expect = |method: AttributeMethod, want: f64| {
            let got = floats_of(&reduce("f", method, true).unwrap(), "f");
            assert!(near(got[0], want), "{method:?}: {} vs {want}", got[0]);
            let single = if method == SumSquares { 100.0 } else { 10.0 };
            assert!(near(got[1], single), "{method:?} single member");
        };
        expect(Average, 8.0 / 3.0);
        expect(LeastPoint, 1.0);
        expect(GreatestPoint, 2.0);
        expect(Maximum, 5.0);
        expect(Minimum, 1.0);
        expect(Mode, 1.0);
        expect(Median, 2.0);
        expect(Sum, 8.0);
        expect(SumSquares, 30.0);
        expect(RootMeanSquare, 10.0f64.sqrt());
        expect(WeightedAverage, 14.0 / 6.0);
        expect(WeightedSum, 14.0);
        expect(MinimumWeight, 1.0);
        expect(MaximumWeight, 2.0);
    }

    #[test]
    fn int_and_text_methods() {
        use AttributeMethod::*;
        let ints = |method: AttributeMethod| ints_of(&reduce("i", method, true).unwrap(), "i");
        assert_eq!(ints(Mode), vec![3, 7]);
        assert_eq!(ints(Median), vec![3, 7]);
        assert_eq!(ints(Average), vec![5, 7]);
        assert_eq!(ints(Sum), vec![15, 7]);
        assert_eq!(ints(SumSquares), vec![99, 49]);
        let texts = |method: AttributeMethod| match attribute::View::storage(&find(&reduce("t", method, true).unwrap(), "t"))
            .unwrap()
        {
            Storage::Text(values) => values.clone(),
            _ => panic!("not text"),
        };
        assert_eq!(texts(Minimum), vec![b"a".to_vec(), b"z".to_vec()]);
        assert_eq!(texts(Maximum), vec![b"c".to_vec(), b"z".to_vec()]);
        assert_eq!(texts(Concatenate), vec![b"bac".to_vec(), b"z".to_vec()]);
        assert_eq!(texts(ConcatenateWeightOrder), vec![b"bac".to_vec(), b"z".to_vec()]);
        let failure = reduce("t", Sum, true).err().unwrap();
        assert_eq!(
            error::message(&failure),
            "Rdk_mesh.Fuse_rules.fuse: attribute \"t\" method is unsupported for text storage"
        );
    }

    #[test]
    fn concatenate_builds_arrays_and_expands_per_point() {
        let out = reduce("i", AttributeMethod::Concatenate, true).unwrap();
        let found = find(&out, "i");
        let Storage::IntArray(rows) = attribute::View::storage(&found).unwrap() else { panic!("rows") };
        let view = packed::IntArray::View::view(rows);
        assert_eq!(view.offsets.to_vec(), vec![0, 3, 4]);
        assert_eq!(view.values.to_vec(), vec![3, 3, 9, 7]);
        // Not compact: one value per source point, repeated through the cluster.
        let expanded = floats_of(&reduce("f", AttributeMethod::Sum, false).unwrap(), "f");
        assert_eq!(expanded, vec![8., 8., 10., 8.]);
        // Rules present, not compact, no rewire: attributes without a rule stay as they were.
        let (kept, _) = apply(
            None, 1, DefaultAttributes::AverageNumeric, &[rule("f", AttributeMethod::Sum)], &[], false, false,
            &clusters(), &cloud(),
        )
        .unwrap();
        assert_eq!(ints_of(&kept, "i"), vec![3, 3, 7, 9]);
    }

    #[test]
    fn defaults_and_groups() {
        let (first, _) = apply(
            None, 1, DefaultAttributes::KeepFirst, &[], &[], true, true, &clusters(), &cloud(),
        )
        .unwrap();
        assert_eq!(floats_of(&first, "f"), vec![1., 10.]);
        let (averaged, _) = apply(
            None, 1, DefaultAttributes::AverageNumeric, &[], &[], true, true, &clusters(), &cloud(),
        )
        .unwrap();
        assert!(near(floats_of(&averaged, "f")[0], 8.0 / 3.0));
        assert_eq!(ints_of(&averaged, "i"), vec![3, 7], "integers keep the first member");
        let members = |method: GroupMethod| {
            let rules = [GroupRule { group_pattern: "g1".to_owned(), group_method: method }];
            let (_, groups) = apply(
                None, 1, DefaultAttributes::KeepFirst, &[], &rules, true, true, &clusters(), &cloud(),
            )
            .unwrap();
            (0..2).map(|c| group::mem(c, &groups[0])).collect::<Vec<_>>()
        };
        assert_eq!(members(GroupMethod::Union), vec![true, true]);
        assert_eq!(members(GroupMethod::Intersection), vec![false, true]);
        assert_eq!(members(GroupMethod::MostCommon), vec![false, true]);
        assert_eq!(members(GroupMethod::LeastPoint), vec![true, true]);
        assert_eq!(members(GroupMethod::GreatestPoint), vec![false, true]);
    }

    #[test]
    fn rule_validation() {
        let missing = AttributeRule {
            pattern: "f".to_owned(),
            method: AttributeMethod::WeightedSum,
            weight_attribute: None,
        };
        let failure = validate(&[missing], &[], &cloud()).unwrap_err();
        assert_eq!(
            error::message(&failure),
            "Rdk_mesh.Fuse_rules.fuse: attribute rule \"f\" requires a weight attribute"
        );
        let blank = AttributeRule {
            pattern: "f".to_owned(),
            method: AttributeMethod::WeightedSum,
            weight_attribute: Some(" ".to_owned()),
        };
        assert!(validate(&[blank], &[], &cloud()).is_err());
        assert!(validate(&[rule("f", AttributeMethod::Sum)], &[], &cloud()).is_ok());
        let bad_pattern = AttributeRule { pattern: "[".to_owned(), method: AttributeMethod::Sum, weight_attribute: None };
        assert!(validate(&[bad_pattern], &[], &cloud()).is_err());
        // A missing or non-finite weight attribute is reported when the rule runs.
        let absent = AttributeRule {
            pattern: "f".to_owned(),
            method: AttributeMethod::WeightedSum,
            weight_attribute: Some("nope".to_owned()),
        };
        let failure = apply(None, 1, DefaultAttributes::KeepFirst, &[absent], &[], true, true, &clusters(), &cloud())
            .err()
            .unwrap();
        assert!(error::message(&failure).contains("weight point attribute \"nope\" is missing"));
        // Non-finite source values are rejected for float reductions.
        let poisoned = with_attr(&cloud(), Owner::Point, "f", Storage::Float(vec![1., f64::NAN, 10., 2.]));
        let failure = apply(
            None, 1, DefaultAttributes::KeepFirst, &[rule("f", AttributeMethod::Sum)], &[], true, true,
            &clusters(), &poisoned,
        )
        .err()
        .unwrap();
        assert!(error::message(&failure).contains("is non-finite at point 1"));
    }

    #[test]
    fn float_ordering_matches_ocaml_compare() {
        assert_eq!(float_compare(f64::NAN, f64::NAN), 0);
        assert_eq!(float_compare(f64::NAN, 1.0), -1);
        assert_eq!(float_compare(1.0, f64::NAN), 1);
        assert_eq!(float_compare(-0.0, 0.0), 0);
        let mut values = vec![3.0, 1.0, 2.0, 1.0];
        let mut points = vec![3, 2, 1, 0];
        sort_range_f64(&mut values, &mut points, 0, 4);
        assert_eq!(values, vec![1.0, 1.0, 2.0, 3.0]);
        assert_eq!(points, vec![0, 2, 1, 3], "ties order by point index");
    }

    #[test]
    fn parallel_exact() {
        use AttributeMethod::*;
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
        let z = vec![0.0; n as usize];
        let g = geom(&(0..n).map(|p| p as f64).collect::<Vec<_>>(), &z, &z, &[], &[0], None);
        let g = with_attr(&g, Owner::Point, "f", Storage::Float((0..n).map(|p| ((p * 7919) % 101) as f64 * 0.37).collect()));
        let g = with_attr(&g, Owner::Point, "i", Storage::Int((0..n).map(|p| (p * 31) % 17).collect()));
        let g = with_attr(&g, Owner::Point, "t", Storage::Text((0..n).map(|p| format!("t{}", (p * 13) % 29).into_bytes()).collect()));
        let g = with_attr(&g, Owner::Point, "w", Storage::Float((0..n).map(|p| 1.0 + (p % 5) as f64).collect()));
        for method in [Average, Median, Mode, Sum, SumSquares, RootMeanSquare, WeightedAverage, ConcatenateWeightOrder] {
            let patterns: &[&str] = if matches!(method, Mode | Median | ConcatenateWeightOrder) { &["f", "i", "t"] } else { &["f", "i"] };
            let rules: Vec<AttributeRule> = patterns
                .iter()
                .map(|pattern| AttributeRule { pattern: (*pattern).to_owned(), method, weight_attribute: Some("w".to_owned()) })
                .collect();
            let signature = same_at_one_and_four_strings(&|| {
                match apply(None, 256, DefaultAttributes::KeepFirst, &rules, &[], true, true, &big, &g) {
                    Ok((attributes, _)) => attributes.iter().map(attribute_signature).collect::<Vec<_>>(),
                    Err(failure) => panic!("{method:?}: {}", error::message(&failure)),
                }
            });
            assert!(!signature.is_empty(), "{method:?}");
        }
    }
}
