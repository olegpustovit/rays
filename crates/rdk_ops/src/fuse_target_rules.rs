use std::sync::Arc;
use rays_math::text_array::TextArray;
use std::collections::HashSet;

use rdk_core::support::Windows;
use rdk_core::{attribute, attribute_pattern, cancel, error, geometry, group, packed, support};
use rdk_core::error::Code;

use crate::fuse_rules::{AttributeMethod, AttributeRule, GroupRule};


struct CompiledAttributeRule {
    pattern: attribute_pattern::T,
    method: AttributeMethod,
    weight_attribute: Option<String>,
}

struct CompiledGroupRule {
    pattern: attribute_pattern::T,
}

fn fail(message: String) -> error::T {
    error::new(
        "Rdk_mesh.Fuse_target_rules",
        Code::InvalidArgument,
        format!("Rdk_mesh.Fuse_target_rules.fuse: {message}"),
    )
}

fn core_fail(failure: error::T) -> error::T {
    if error::is_cancelled(&failure) {
        failure
    } else {
        fail(error::message(&failure).to_owned())
    }
}

fn compile_attribute_rules(rules: &[AttributeRule]) -> Result<Vec<CompiledAttributeRule>, error::T> {
    let mut output = Vec::new();
    for rule in rules {
        output.push(CompiledAttributeRule {
            pattern: attribute_pattern::compile(&rule.pattern).map_err(core_fail)?,
            method: rule.method,
            weight_attribute: rule.weight_attribute.clone(),
        });
    }
    Ok(output)
}

fn compile_group_rules(rules: &[GroupRule]) -> Result<Vec<CompiledGroupRule>, error::T> {
    let mut output = Vec::new();
    for rule in rules {
        output.push(CompiledGroupRule {
            pattern: attribute_pattern::compile(&rule.group_pattern).map_err(core_fail)?,
        });
    }
    Ok(output)
}

fn find_attribute_rule(rules: &[CompiledAttributeRule], name: &str) -> Option<usize> {
    let mut found = None;
    for (index, rule) in rules.iter().enumerate() {
        if attribute_pattern::matches(&rule.pattern, name) {
            found = Some(index);
        }
    }
    found
}

fn find_group_rule(rules: &[CompiledGroupRule], name: &str) -> Option<usize> {
    let mut found = None;
    for (index, rule) in rules.iter().enumerate() {
        if attribute_pattern::matches(&rule.pattern, name) {
            found = Some(index);
        }
    }
    found
}

fn weighted(method: AttributeMethod) -> bool {
    matches!(
        method,
        AttributeMethod::WeightedAverage
            | AttributeMethod::WeightedSum
            | AttributeMethod::MinimumWeight
            | AttributeMethod::MaximumWeight
            | AttributeMethod::ConcatenateWeightOrder
    )
}

fn selection(method: AttributeMethod) -> bool {
    matches!(
        method,
        AttributeMethod::LeastPoint
            | AttributeMethod::GreatestPoint
            | AttributeMethod::MinimumWeight
            | AttributeMethod::MaximumWeight
    )
}

fn concatenate(method: AttributeMethod) -> bool {
    matches!(method, AttributeMethod::Concatenate | AttributeMethod::ConcatenateWeightOrder)
}

fn validate_weight(target: &geometry::T, rule: &CompiledAttributeRule) -> Result<(), error::T> {
    if !weighted(rule.method) {
        return Ok(());
    }
    let Some(name) = &rule.weight_attribute else {
        return Err(fail("fixed-target attribute rule requires a weight attribute".to_owned()));
    };
    let Some(found) = geometry::find_attribute(attribute::Owner::Point, name, target) else {
        return Err(fail(format!("target weight point attribute {name:?} is missing")));
    };
    match attribute::view::storage(&found).map_err(core_fail)? {
        attribute::Storage::Float(values) => {
            for (point, value) in values.iter().enumerate() {
                if !value.is_finite() {
                    return Err(fail(format!(
                        "target weight attribute {name:?} is non-finite at point {point}"
                    )));
                }
            }
            Ok(())
        }
        attribute::Storage::Int(_) => Ok(()),
        _ => Err(fail(format!("target weight attribute {name:?} must be scalar float or integer"))),
    }
}

fn mapped_f64(cancel_value: Option<&cancel::T>, grain: i64, destinations: &[i64], source: &[f64], target: &[f64]) -> Result<Vec<f64>, error::T> {
    let mut output = source.to_vec();
    support::par_fill(cancel_value, grain, &mut output, &|point| {
        let destination = destinations[point as usize];
        Ok(if destination >= 0 { target[destination as usize] } else { source[point as usize] })
    })?;
    Ok(output)
}

fn mapped_i64(cancel_value: Option<&cancel::T>, grain: i64, destinations: &[i64], source: &[i64], target: &[i64]) -> Result<Vec<i64>, error::T> {
    let mut output = source.to_vec();
    support::par_fill(cancel_value, grain, &mut output, &|point| {
        let destination = destinations[point as usize];
        Ok(if destination >= 0 { target[destination as usize] } else { source[point as usize] })
    })?;
    Ok(output)
}

fn mapped_string(cancel_value: Option<&cancel::T>, grain: i64, destinations: &[i64], source: &TextArray, target: &TextArray) -> Result<TextArray, error::T> {
    let mut output: Vec<&[u8]> = vec![&[]; source.len()];
    support::par_fill(cancel_value, grain, &mut output, &|point| {
        let destination = destinations[point as usize];
        Ok(if destination >= 0 { &target[destination as usize] } else { &source[point as usize] })
    })?;
    Ok(output.into_iter().collect())
}

fn scalar_int_array(cancel_value: Option<&cancel::T>, grain: i64, destinations: &[i64], source: &[i64], target: &[i64]) -> Result<packed::int_array::T, error::T> {
    let count = destinations.len();
    let offsets: Vec<i64> = (0..=count as i64).collect();
    let values = mapped_i64(cancel_value, grain, destinations, source, target)?;
    Ok(packed::int_array::view::create_validated_owned(offsets, values))
}

fn scalar_float_array(cancel_value: Option<&cancel::T>, grain: i64, destinations: &[i64], source: &[f64], target: &[f64]) -> Result<packed::float_array::T, error::T> {
    let count = destinations.len();
    let offsets: Vec<i64> = (0..=count as i64).collect();
    let values = mapped_f64(cancel_value, grain, destinations, source, target)?;
    Ok(packed::float_array::view::create_validated_owned(offsets, values))
}

fn mapped_int_rows(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    destinations: &[i64],
    source: &packed::int_array::T,
    target: &packed::int_array::T,
) -> Result<packed::int_array::T, error::T> {
    let source = packed::int_array::view::view(source);
    let target = packed::int_array::view::view(target);
    let count = destinations.len();
    let mut offsets = vec![0i64; count + 1];
    for point in 0..count {
        let destination = destinations[point];
        let length = if destination < 0 {
            source.offsets[point + 1] - source.offsets[point]
        } else {
            target.offsets[destination as usize + 1] - target.offsets[destination as usize]
        };
        if offsets[point].checked_add(length).is_none() {
            return Err(fail("fixed-target integer array exceeds array limits".to_owned()));
        }
        offsets[point + 1] = offsets[point] + length;
    }
    let mut values = vec![0i64; offsets[count] as usize];
    let grain = grain.max(1);
    {
        let windows = Windows::new(&mut values, support::range_count(count as i64, grain), |range| {
            offsets[(range.saturating_mul(grain) as usize).min(count)] as usize
        });
        support::run_indexed_ranges(grain, count as i64, &|range, chunk_first, chunk_last| {
            let window = windows.take(range);
            let base = offsets[chunk_first as usize];
            for point in chunk_first..chunk_last {
                support::poll(cancel_value, point)?;
                let point = point as usize;
                let destination = destinations[point];
                let (input, first, last) = if destination < 0 {
                    (&source.values, source.offsets[point], source.offsets[point + 1])
                } else {
                    (&target.values, target.offsets[destination as usize], target.offsets[destination as usize + 1])
                };
                let at = (offsets[point] - base) as usize;
                window[at..at + (last - first) as usize].copy_from_slice(&input[first as usize..last as usize]);
            }
            Ok(())
        })?;
    }
    Ok(packed::int_array::view::create_validated_owned(offsets, values))
}

fn mapped_float_rows(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    destinations: &[i64],
    source: &packed::float_array::T,
    target: &packed::float_array::T,
) -> Result<packed::float_array::T, error::T> {
    let source = packed::float_array::view::view(source);
    let target = packed::float_array::view::view(target);
    let count = destinations.len();
    let mut offsets = vec![0i64; count + 1];
    for point in 0..count {
        let destination = destinations[point];
        let length = if destination < 0 {
            source.offsets[point + 1] - source.offsets[point]
        } else {
            target.offsets[destination as usize + 1] - target.offsets[destination as usize]
        };
        if offsets[point].checked_add(length).is_none() {
            return Err(fail("fixed-target float array exceeds array limits".to_owned()));
        }
        offsets[point + 1] = offsets[point] + length;
    }
    let mut values = vec![0.0; offsets[count] as usize];
    let grain = grain.max(1);
    {
        let windows = Windows::new(&mut values, support::range_count(count as i64, grain), |range| {
            offsets[(range.saturating_mul(grain) as usize).min(count)] as usize
        });
        support::run_indexed_ranges(grain, count as i64, &|range, chunk_first, chunk_last| {
            let window = windows.take(range);
            let base = offsets[chunk_first as usize];
            for point in chunk_first..chunk_last {
                support::poll(cancel_value, point)?;
                let point = point as usize;
                let destination = destinations[point];
                let (input, first, last) = if destination < 0 {
                    (&source.values, source.offsets[point], source.offsets[point + 1])
                } else {
                    (&target.values, target.offsets[destination as usize], target.offsets[destination as usize + 1])
                };
                let at = (offsets[point] - base) as usize;
                window[at..at + (last - first) as usize].copy_from_slice(&input[first as usize..last as usize]);
            }
            Ok(())
        })?;
    }
    Ok(packed::float_array::view::create_validated_owned(offsets, values))
}

fn apply_attribute(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    destinations: &[i64],
    target: &geometry::T,
    rule: &CompiledAttributeRule,
    source_attribute: &attribute::T,
) -> Result<attribute::T, error::T> {
    validate_weight(target, rule)?;
    let name = attribute::name(source_attribute).to_owned();
    let Some(target_attribute) = geometry::find_attribute(attribute::Owner::Point, &name, target) else {
        return Err(fail(format!("target point attribute {name:?} is missing")));
    };
    let incompatible = || {
        fail(format!(
            "source and target point attribute {name:?} storage differs or does not support this rule"
        ))
    };
    let method = rule.method;
    let source_storage = attribute::view::storage(source_attribute).map_err(core_fail)?;
    let target_storage = attribute::view::storage(&target_attribute).map_err(core_fail)?;
    let storage = match (source_storage, target_storage) {
        (attribute::Storage::Float(s), attribute::Storage::Float(t)) if concatenate(method) => {
            attribute::Storage::FloatArray(scalar_float_array(cancel_value, grain, destinations, s, t)?)
        }
        (attribute::Storage::Int(s), attribute::Storage::Int(t)) if concatenate(method) => {
            attribute::Storage::IntArray(scalar_int_array(cancel_value, grain, destinations, s, t)?)
        }
        (attribute::Storage::Int(s), attribute::Storage::Int(t)) => {
            attribute::Storage::Int(Arc::new(mapped_i64(cancel_value, grain, destinations, s, t)?))
        }
        (attribute::Storage::Text(s), attribute::Storage::Text(t)) => {
            let supported = selection(method)
                || concatenate(method)
                || matches!(
                    method,
                    AttributeMethod::Minimum | AttributeMethod::Maximum | AttributeMethod::Mode | AttributeMethod::Median
                );
            if !supported {
                return Err(incompatible());
            }
            attribute::Storage::Text(Arc::new(mapped_string(cancel_value, grain, destinations, s, t)?))
        }
        (attribute::Storage::Float(_), attribute::Storage::Float(_))
        | (attribute::Storage::Float2(_), attribute::Storage::Float2(_))
        | (attribute::Storage::Float3(_), attribute::Storage::Float3(_))
        | (attribute::Storage::Float4(_), attribute::Storage::Float4(_)) => {
            if concatenate(method) { return Err(incompatible()); }
            let source_planes = attribute::float_planes(source_storage).map_err(core_fail)?.expect("float source planes");
            let target_planes = attribute::float_planes(target_storage).map_err(core_fail)?.expect("float target planes");
            let output = source_planes.iter().zip(target_planes).map(|(s, t)| mapped_f64(cancel_value, grain, destinations, s, t))
                .collect::<Result<Vec<_>, _>>()?;
            attribute::of_float_planes(source_storage, output).map_err(core_fail)?
        }
        (attribute::Storage::IntArray(s), attribute::Storage::IntArray(t))
            if concatenate(method) || selection(method) =>
        {
            attribute::Storage::IntArray(mapped_int_rows(cancel_value, grain, destinations, s, t)?)
        }
        (attribute::Storage::FloatArray(s), attribute::Storage::FloatArray(t))
            if concatenate(method) || selection(method) =>
        {
            attribute::Storage::FloatArray(mapped_float_rows(cancel_value, grain, destinations, s, t)?)
        }
        _ => return Err(incompatible()),
    };
    attribute::create_owned(&name, attribute::Owner::Point, storage).map_err(core_fail)
}

fn apply_groups(
    grain: i64,
    rules: &[CompiledGroupRule],
    destinations: &[i64],
    source: &geometry::T,
    target: &geometry::T,
) -> Result<Vec<group::T>, error::T> {
    let source_groups = geometry::groups(source);
    let mut source_names: HashSet<String> = HashSet::new();
    for item in &source_groups {
        if group::owner(item) == group::Owner::Point {
            source_names.insert(group::name(item).to_owned());
        }
    }
    let mut groups = source_groups;
    for item in geometry::groups(target) {
        if group::owner(&item) == group::Owner::Point
            && !source_names.contains(group::name(&item))
            && find_group_rule(rules, group::name(&item)).is_some()
        {
            groups.push(item);
        }
    }
    let mut output = Vec::with_capacity(groups.len());
    for item in groups {
        if group::owner(&item) != group::Owner::Point || find_group_rule(rules, group::name(&item)).is_none() {
            output.push(item);
            continue;
        }
        let source_group = geometry::find_group(group::Owner::Point, group::name(&item), source);
        let target_group = geometry::find_group(group::Owner::Point, group::name(&item), target);
        output.push(
            group::init(
                Some(grain),
                group::Owner::Point,
                group::name(&item),
                geometry::point_count(source),
                &|point| Ok({
                    let destination = destinations[point as usize];
                    if destination >= 0 {
                        target_group.as_ref().is_some_and(|g| group::mem(destination, g))
                    } else {
                        source_group.as_ref().is_some_and(|g| group::mem(point, g))
                    }
                }),
            )
            .map_err(core_fail)?,
        );
    }
    Ok(output)
}

pub(crate) fn apply(
    cancel_value: Option<&cancel::T>,
    grain: i64,
    attribute_rules: &[AttributeRule],
    group_rules: &[GroupRule],
    destinations: &[i64],
    source: &geometry::T,
    target: &geometry::T,
) -> Result<geometry::T, error::T> {
    if destinations.len() as i64 != geometry::point_count(source) {
        return Err(fail("fixed-target destination count differs from source point count".to_owned()));
    }
    let attribute_rules = compile_attribute_rules(attribute_rules)?;
    let group_rules = compile_group_rules(group_rules)?;
    if attribute_rules.is_empty() && group_rules.is_empty() {
        return Ok(source.clone());
    }
    let mut attributes = Vec::new();
    for item in geometry::attributes(source) {
        if attribute::owner(&item) != attribute::Owner::Point {
            attributes.push(item);
            continue;
        }
        match find_attribute_rule(&attribute_rules, attribute::name(&item)) {
            None => attributes.push(item),
            Some(rule) => attributes.push(apply_attribute(cancel_value, grain, destinations, target, &attribute_rules[rule], &item)?),
        }
    }
    let groups = apply_groups(grain, &group_rules, destinations, source, target)?;
    geometry::create(
        geometry::positions(source).clone(),
        geometry::topology(source).clone(),
        Some(attributes),
        Some(groups),
        Some(geometry::edge_groups(source)),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compact_points::fixtures::*;
    use rdk_core::attribute::{Owner, Storage};

    fn cloud(f: Vec<f64>) -> geometry::T {
        let g = geom(&[0., 1., 2.], &[0.; 3], &[0.; 3], &[], &[0], None);
        with_attr(&g, Owner::Point, "f", Storage::Float(Arc::new(f)))
    }

    fn rule(method: AttributeMethod) -> AttributeRule {
        AttributeRule { pattern: "f".to_owned(), method, weight_attribute: None }
    }

    #[test]
    fn selection_rules_copy_the_target_value() {
        let source = cloud(vec![0., 0., 0.]);
        let target = cloud(vec![10., 20., 30.]);
        let out = apply(None, 1, &[rule(AttributeMethod::LeastPoint)], &[], &[1, -1, 2], &source, &target).unwrap();
        assert_eq!(floats(Owner::Point, "f", &out), vec![20., 0., 30.]);
        // Averages reduce to the single stable target.
        let out = apply(None, 1, &[rule(AttributeMethod::Average)], &[], &[0, 0, -1], &source, &target).unwrap();
        assert_eq!(floats(Owner::Point, "f", &out), vec![10., 10., 0.]);
        // Concatenation keeps the scalar-to-array shape.
        let out = apply(None, 1, &[rule(AttributeMethod::Concatenate)], &[], &[2, -1, -1], &source, &target).unwrap();
        let found = geometry::find_attribute(Owner::Point, "f", &out).unwrap();
        let Storage::FloatArray(rows) = attribute::view::storage(&found).unwrap() else { panic!("rows") };
        let view = packed::float_array::view::view(rows);
        assert_eq!(view.offsets.to_vec(), vec![0, 1, 2, 3]);
        assert_eq!(view.values.to_vec(), vec![30., 0., 0.]);
    }

    #[test]
    fn target_groups_are_appended_and_membership_follows_destinations() {
        let source = cloud(vec![0., 0., 0.]);
        let target = geometry::with_group(
            group::init(None, group::Owner::Point, "only_target", 3, &|p| Ok(p == 1)).unwrap(),
            &cloud(vec![1., 2., 3.]),
        )
        .unwrap();
        let rules = [GroupRule { group_pattern: "only_*".to_owned(), group_method: crate::fuse_rules::GroupMethod::Union }];
        let out = apply(None, 1, &[], &rules, &[1, -1, 2], &source, &target).unwrap();
        let found = geometry::find_group(group::Owner::Point, "only_target", &out).unwrap();
        let members: Vec<bool> = (0..3).map(|p| group::mem(p, &found)).collect();
        assert_eq!(members, vec![true, false, false]);
    }

    #[test]
    fn mismatches_are_diagnosed() {
        let source = cloud(vec![0., 0., 0.]);
        let target = cloud(vec![1., 2., 3.]);
        let none = apply(None, 1, &[], &[], &[0, 0, 0], &source, &target).unwrap();
        assert_eq!(geometry::data_id(&none), geometry::data_id(&source), "no rules is an identity");
        let failure = apply(None, 1, &[rule(AttributeMethod::Sum)], &[], &[0], &source, &target).err().unwrap();
        assert_eq!(
            error::message(&failure),
            "Rdk_mesh.Fuse_target_rules.fuse: fixed-target destination count differs from source point count"
        );
        let bare = geom(&[0., 1., 2.], &[0.; 3], &[0.; 3], &[], &[0], None);
        let failure = apply(None, 1, &[rule(AttributeMethod::Sum)], &[], &[0, 0, 0], &source, &bare).err().unwrap();
        assert!(error::message(&failure).contains("target point attribute \"f\" is missing"));
        let ints = with_attr(&bare, Owner::Point, "f", Storage::Int(Arc::new(vec![1, 2, 3])));
        let failure = apply(None, 1, &[rule(AttributeMethod::Sum)], &[], &[0, 0, 0], &source, &ints).err().unwrap();
        assert!(error::message(&failure).contains("storage differs"));
        let weighted = AttributeRule {
            pattern: "f".to_owned(),
            method: AttributeMethod::WeightedSum,
            weight_attribute: None,
        };
        let failure = apply(None, 1, &[weighted], &[], &[0, 0, 0], &source, &target).err().unwrap();
        assert!(error::message(&failure).contains("requires a weight attribute"));
    }

    #[test]
    fn parallel_exact() {
        let n = 40_000i64;
        let build = |salt: i64| {
            let z = vec![0.0; n as usize];
            let g = geom(&(0..n).map(|p| p as f64).collect::<Vec<_>>(), &z, &z, &[], &[0], None);
            let g = with_attr(&g, Owner::Point, "f", Storage::Float(Arc::new((0..n).map(|p| (p + salt) as f64 * 0.5).collect())));
            let g = with_attr(&g, Owner::Point, "i", Storage::Int(Arc::new((0..n).map(|p| p * 3 + salt).collect())));
            let g = with_attr(&g, Owner::Point, "t", Storage::Text(Arc::new((0..n).map(|p| format!("s{}", p + salt).into_bytes()).collect())));
            let offsets: Vec<i64> = std::iter::once(0).chain((1..=n).scan(0, |at, p| { *at += p % 4; Some(*at) })).collect();
            let values: Vec<f64> = (0..offsets[n as usize]).map(|v| v as f64 + salt as f64).collect();
            let rows = packed::float_array::view::create_validated_owned(offsets, values);
            with_attr(&g, Owner::Point, "r", Storage::FloatArray(rows))
        };
        let (source, target) = (build(0), build(1_000_000));
        let destinations: Vec<i64> = (0..n).map(|p| if p % 3 == 0 { -1 } else { (p * 7919) % n }).collect();
        let rules: Vec<AttributeRule> = ["f", "i", "t", "r"]
            .iter()
            .map(|pattern| AttributeRule { pattern: (*pattern).to_owned(), method: AttributeMethod::LeastPoint, weight_attribute: None })
            .collect();
        let signature = same_at_one_and_four_strings(&|| {
            let out = apply(None, 512, &rules, &[], &destinations, &source, &target).unwrap();
            geometry::attributes(&out).iter().map(attribute_signature).collect::<Vec<_>>()
        });
        assert_eq!(signature.len(), 4);
    }
}
