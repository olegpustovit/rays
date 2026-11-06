//! Named group metadata (rename, delete, transfer) over one local group store.
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use rdk_core::{attribute, attribute_pattern, cancel, edge_group, error, geometry, group, topology_index};
use rdk_geom::{proximity_index, spatial_index};

use crate::attribute_ops::{is_blank, make_error, ocaml_quote, recode};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Owner {
    GroupPoints,
    GroupVertices,
    GroupPrimitives,
    GroupEdges,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PromoteMode {
    IncludeAny,
    IncludeAll,
    IncludeSharedEdge,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BoundaryAttribute {
    pub boundary_attribute_owner: attribute::Owner,
    pub boundary_attribute_pattern: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PromoteBoundaryOptions {
    pub promote_boundary_attributes: Vec<BoundaryAttribute>,
    pub promote_boundary_tolerance: f64,
    pub promote_include_unshared_edges: bool,
    pub promote_all_unshared_edges: bool,
    pub promote_all_boundary_prims: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PromoteOperation {
    PromoteElements(PromoteMode),
    PromoteBoundary(PromoteBoundaryOptions),
}

#[derive(Clone, Debug, PartialEq)]
pub struct PromotionRule {
    pub promotion_source: Owner,
    pub promotion_destination: Owner,
    pub promotion_pattern: String,
    pub promotion_new_name: Option<String>,
    pub promotion_keep_original: bool,
    pub promotion_output_as_attribute: bool,
    pub promotion_operation: PromoteOperation,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BooleanOperation {
    GroupReplace,
    GroupUnion,
    GroupIntersection,
    GroupSubtract,
    GroupXor,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RangeCollision {
    pub collision_owner: Owner,
    pub collision_pattern: String,
    pub keep_boundary: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum RangeConnectivity {
    RangeDisconnected {
        region: Option<i64>,
    },
    RangeConnected {
        connectivity_attributes: Option<String>,
        connectivity_tolerance: f64,
        collision: Option<RangeCollision>,
        region: Option<i64>,
        remove_other_regions: bool,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RenameConflict {
    RenameSkip,
    RenameError,
    RenameOverwrite,
    RenameUnion,
}

#[derive(Clone, Debug)]
pub struct RenameRule {
    pub rename_owner: Option<Owner>,
    pub rename_pattern: String,
    pub rename_replacement: String,
    pub rename_conflict: RenameConflict,
}

#[derive(Clone, Debug)]
pub struct DeleteRule {
    pub delete_owner: Option<Owner>,
    pub delete_pattern: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CopyConflict {
    CopySkip,
    CopyOverwrite,
    CopyAddSuffix,
}

#[derive(Clone, Debug)]
pub struct TransferRule {
    pub transfer_owner: Owner,
    pub transfer_pattern: String,
    pub transfer_prefix: String,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Bounds {
    BoundsBox { minimum: rays_math::vec3::T, maximum: rays_math::vec3::T },
    BoundsSphere { center: rays_math::vec3::T, radius: f64 },
}

#[derive(Clone)]
pub enum Selection {
    Ordinary(group::T),
    NativeEdges(edge_group::T),
}

#[derive(Clone)]
pub struct NamedEntry {
    pub owner: Owner,
    pub entry_name: String,
    pub value: Selection,
    pub alive: bool,
}

pub struct GroupStore {
    table: HashMap<(Owner, String), usize>,
    entries: Vec<NamedEntry>,
    dirty: bool,
}

#[derive(Clone)]
pub struct DeleteParams {
    pub rules: Vec<DeleteRule>,
    pub delete_unused: bool,
}

#[derive(Clone)]
pub enum ElementMap {
    Identity(i64),
    Explicit(Vec<i64>),
    Proximity { indices: Vec<i64>, distances_squared: Vec<f64> },
}

#[derive(Clone)]
pub struct TransferParams {
    pub cancel: Option<Arc<cancel::T>>,
    pub grain: Option<i64>,
    pub rules: Vec<TransferRule>,
    pub conflict: CopyConflict,
    pub create_empty: bool,
    pub distance: f64,
    pub source: geometry::T,
    pub target: geometry::T,
}

const DEFAULT_CANCELLED: &str = "geometry operation was cancelled";

fn owner_name(owner: Owner) -> &'static str {
    match owner {
        Owner::GroupPoints => "point",
        Owner::GroupVertices => "vertex",
        Owner::GroupPrimitives => "primitive",
        Owner::GroupEdges => "edge",
    }
}

fn ordinary_owner(owner: Owner) -> Option<group::Owner> {
    match owner {
        Owner::GroupPoints => Some(group::Owner::Point),
        Owner::GroupVertices => Some(group::Owner::Vertex),
        Owner::GroupPrimitives => Some(group::Owner::Primitive),
        Owner::GroupEdges => None,
    }
}

fn selection_mem(selection: &Selection, index: i64) -> bool {
    match selection {
        Selection::Ordinary(value) => group::mem(index, value),
        Selection::NativeEdges(value) => edge_group::mem(index, value),
    }
}

fn selection_cardinality(selection: &Selection) -> i64 {
    match selection {
        Selection::Ordinary(value) => group::cardinality(value),
        Selection::NativeEdges(value) => edge_group::cardinality(value),
    }
}

fn owner_of_group(owner: group::Owner) -> Owner {
    match owner {
        group::Owner::Point => Owner::GroupPoints,
        group::Owner::Vertex => Owner::GroupVertices,
        group::Owner::Primitive => Owner::GroupPrimitives,
    }
}

fn selection_with_name(name: &str, value: &Selection) -> Result<Selection, error::T> {
    Ok(match value {
        Selection::Ordinary(value) => Selection::Ordinary(group::with_name(name, value)?),
        Selection::NativeEdges(value) => Selection::NativeEdges(edge_group::with_name(name, value)?),
    })
}

fn store_of_geometry(geometry_value: &geometry::T) -> GroupStore {
    let mut entries: Vec<NamedEntry> = Vec::new();
    for item in geometry::groups(geometry_value) {
        entries.push(NamedEntry {
            owner: owner_of_group(group::owner(&item)),
            entry_name: group::name(&item).to_owned(),
            value: Selection::Ordinary(item),
            alive: true,
        });
    }
    for item in geometry::edge_groups(geometry_value) {
        entries.push(NamedEntry {
            owner: Owner::GroupEdges,
            entry_name: edge_group::name(&item).to_owned(),
            value: Selection::NativeEdges(item),
            alive: true,
        });
    }
    let mut table = HashMap::with_capacity(entries.len() * 2);
    for (index, entry) in entries.iter().enumerate() {
        table.insert((entry.owner, entry.entry_name.clone()), index);
    }
    GroupStore { table, entries, dirty: false }
}

fn store_find(store: &GroupStore, owner: Owner, name: &str) -> Option<usize> {
    match store.table.get(&(owner, name.to_owned())) {
        Some(&index) if store.entries[index].alive => Some(index),
        _ => None,
    }
}

fn store_remove(store: &mut GroupStore, index: usize) {
    if store.entries[index].alive {
        let key = (store.entries[index].owner, store.entries[index].entry_name.clone());
        store.table.remove(&key);
        store.entries[index].alive = false;
        store.dirty = true;
    }
}

fn store_replace(store: &mut GroupStore, index: usize, value: &Selection) -> Result<(), error::T> {
    store.entries[index].value = selection_with_name(&store.entries[index].entry_name, value)?;
    store.dirty = true;
    Ok(())
}

fn store_add(store: &mut GroupStore, owner: Owner, name: &str, value: &Selection) -> Result<usize, error::T> {
    if store.table.contains_key(&(owner, name.to_owned())) {
        return Err(make_error(
            "group_transfer",
            "invalid_group",
            "Group operation: duplicate local group-store key",
        ));
    }
    let entry = NamedEntry {
        owner,
        entry_name: name.to_owned(),
        value: selection_with_name(name, value)?,
        alive: true,
    };
    store.entries.push(entry);
    let index = store.entries.len() - 1;
    store.table.insert((owner, name.to_owned()), index);
    store.dirty = true;
    Ok(index)
}

fn store_rename(store: &mut GroupStore, index: usize, name: &str, value: &Selection) -> Result<(), error::T> {
    let key = (store.entries[index].owner, store.entries[index].entry_name.clone());
    store.table.remove(&key);
    store.entries[index].entry_name = name.to_owned();
    store.entries[index].value = selection_with_name(name, value)?;
    store.table.insert((store.entries[index].owner, name.to_owned()), index);
    store.dirty = true;
    Ok(())
}

fn store_commit(store: &GroupStore, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    if !store.dirty {
        return Ok(geometry_value.clone());
    }
    let mut groups: Vec<group::T> = Vec::new();
    let mut edges: Vec<edge_group::T> = Vec::new();
    for entry in &store.entries {
        if entry.alive {
            match &entry.value {
                Selection::Ordinary(value) => groups.push(value.clone()),
                Selection::NativeEdges(value) => edges.push(value.clone()),
            }
        }
    }
    geometry::create(
        geometry::positions(geometry_value).clone(),
        geometry::topology(geometry_value).clone(),
        Some(geometry::attributes(geometry_value)),
        Some(groups),
        Some(edges),
    )
}

pub fn valid_group_name(name: &str) -> bool {
    let bytes = name.as_bytes();
    !bytes.is_empty()
        && !bytes[0].is_ascii_digit()
        && bytes.iter().all(|byte| byte.is_ascii_alphanumeric() || *byte == b'_')
}

pub fn force_valid_group_name(name: &str) -> String {
    let bytes = name.as_bytes();
    let prefix_digit = !bytes.is_empty() && bytes[0].is_ascii_digit();
    let offset = usize::from(prefix_digit);
    let mut output = vec![b'_'; bytes.len() + offset];
    for (index, byte) in bytes.iter().enumerate() {
        if byte.is_ascii_alphanumeric() || *byte == b'_' {
            output[offset + index] = *byte;
        }
    }
    // Only ASCII bytes are ever written, so this cannot fail.
    String::from_utf8(output).unwrap_or_default()
}

fn selection_boolean(
    operation: BooleanOperation,
    left: &Selection,
    right: &Selection,
) -> Result<Selection, error::T> {
    match (left, right) {
        (Selection::Ordinary(left), Selection::Ordinary(right)) => Ok(Selection::Ordinary(match operation {
            BooleanOperation::GroupReplace => right.clone(),
            BooleanOperation::GroupUnion => group::union(left, right)?,
            BooleanOperation::GroupIntersection => group::intersection(left, right)?,
            BooleanOperation::GroupSubtract => group::difference(left, right)?,
            BooleanOperation::GroupXor => group::symmetric_difference(left, right)?,
        })),
        (Selection::NativeEdges(left), Selection::NativeEdges(right)) => {
            Ok(Selection::NativeEdges(match operation {
                BooleanOperation::GroupReplace => right.clone(),
                BooleanOperation::GroupUnion => edge_group::union(left, right)?,
                BooleanOperation::GroupIntersection => edge_group::intersection(left, right)?,
                BooleanOperation::GroupSubtract => edge_group::difference(left, right)?,
                BooleanOperation::GroupXor => edge_group::symmetric_difference(left, right)?,
            }))
        }
        _ => Err(make_error(
            "group_ops",
            "invalid_group",
            "Group operation: cannot combine ordinary and native-edge groups",
        )),
    }
}

fn owner_selected(selected: Option<Owner>, owner: Owner) -> bool {
    match selected {
        None => true,
        Some(expected) => expected == owner,
    }
}

fn rename_entry(
    store: &mut GroupStore,
    conflict: RenameConflict,
    index: usize,
    new_name: &str,
    value: &Selection,
) -> Result<(), error::T> {
    if store.entries[index].entry_name == new_name {
        return store_replace(store, index, value);
    }
    let owner = store.entries[index].owner;
    match store_find(store, owner, new_name) {
        None => store_rename(store, index, new_name, value),
        Some(destination) if destination == index => store_replace(store, index, value),
        Some(destination) => match conflict {
            RenameConflict::RenameSkip => Ok(()),
            RenameConflict::RenameError => Err(make_error(
                "group_rename",
                "invalid_group",
                &format!(
                    "Group Rename: destination {} group {} already exists",
                    owner_name(owner),
                    ocaml_quote(new_name)
                ),
            )),
            RenameConflict::RenameOverwrite => {
                store_remove(store, destination);
                store_rename(store, index, new_name, value)
            }
            RenameConflict::RenameUnion => {
                let combined =
                    selection_boolean(BooleanOperation::GroupUnion, &store.entries[destination].value, value)?;
                store_replace(store, destination, &combined)?;
                store_remove(store, index);
                Ok(())
            }
        },
    }
}

pub fn rename(rules: &[RenameRule], geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    rename_inner(rules, geometry_value)
        .map_err(|failure| recode(&failure, "group_rename", "invalid_group", DEFAULT_CANCELLED))
}

fn rename_inner(rules: &[RenameRule], geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let mut store = store_of_geometry(geometry_value);
    for rule in rules {
        let rewrite = attribute_pattern::compile_rewrite(&rule.rename_pattern, &rule.rename_replacement)?;
        let count = store.entries.len();
        for index in 0..count {
            if !store.entries[index].alive || !owner_selected(rule.rename_owner, store.entries[index].owner) {
                continue;
            }
            let Some(new_name) = attribute_pattern::rewrite(&rewrite, &store.entries[index].entry_name) else {
                continue;
            };
            let new_name = String::from_utf8_lossy(&new_name).into_owned();
            if is_blank(&new_name) {
                return Err(make_error(
                    "group_rename",
                    "invalid_group",
                    "Group Rename: rewrite produced an empty group name",
                ));
            }
            let value = store.entries[index].value.clone();
            rename_entry(&mut store, rule.rename_conflict, index, &new_name, &value)?;
        }
    }
    store_commit(&store, geometry_value)
}

pub fn delete(params: &DeleteParams, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    delete_inner(params, geometry_value)
        .map_err(|failure| recode(&failure, "group_delete", "invalid_group", DEFAULT_CANCELLED))
}

fn delete_inner(params: &DeleteParams, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let mut store = store_of_geometry(geometry_value);
    let mut compiled: Vec<(Option<Owner>, attribute_pattern::T)> = Vec::new();
    for rule in &params.rules {
        if !is_blank(&rule.delete_pattern) {
            compiled.push((rule.delete_owner, attribute_pattern::compile(&rule.delete_pattern)?));
        }
    }
    for index in 0..store.entries.len() {
        if !store.entries[index].alive {
            continue;
        }
        let matched = compiled.iter().any(|(owner, pattern)| {
            owner_selected(*owner, store.entries[index].owner)
                && attribute_pattern::matches(pattern, &store.entries[index].entry_name)
        });
        let empty = params.delete_unused && selection_cardinality(&store.entries[index].value) == 0;
        if matched || empty {
            store_remove(&mut store, index);
        }
    }
    store_commit(&store, geometry_value)
}

fn mapped_source(mapping: &ElementMap, target: i64) -> i64 {
    match mapping {
        ElementMap::Identity(source_count) => {
            if target < *source_count {
                target
            } else {
                -1
            }
        }
        ElementMap::Explicit(values) => values[target as usize],
        ElementMap::Proximity { indices, .. } => indices[target as usize],
    }
}

fn target_owner_count(
    cancel_token: Option<&cancel::T>,
    owner: Owner,
    geometry_value: &geometry::T,
) -> Result<i64, error::T> {
    Ok(match owner {
        Owner::GroupPoints => geometry::point_count(geometry_value),
        Owner::GroupVertices => geometry::vertex_count(geometry_value),
        Owner::GroupPrimitives => geometry::primitive_count(geometry_value),
        Owner::GroupEdges => topology_index::edge_count(&topology_index::create(
            cancel_token,
            geometry::topology(geometry_value),
        )?),
    })
}

fn float_compare(left: f64, right: f64) -> std::cmp::Ordering {
    left.partial_cmp(&right).unwrap_or_else(|| left.is_nan().cmp(&right.is_nan()).reverse())
}

fn copied_selection(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    owner: Owner,
    mapping: &ElementMap,
    source_group: &Selection,
    target: &geometry::T,
) -> Result<Selection, error::T> {
    let count = target_owner_count(cancel_token, owner, target)?;
    let polled = AtomicBool::new(false);
    let member = |element: i64| -> bool {
        if cancel::poll(cancel_token, element, 16_383).is_err() {
            polled.store(true, Ordering::Relaxed);
            return false;
        }
        let source = mapped_source(mapping, element);
        source >= 0 && selection_mem(source_group, source)
    };
    let cancelled = || {
        make_error("group_transfer", "cancelled", DEFAULT_CANCELLED)
    };
    match ordinary_owner(owner) {
        Some(group_owner) => {
            let target_group = group::init(Some(grain), group_owner, "__copied", count, &member)?;
            if polled.load(Ordering::Relaxed) {
                return Err(cancelled());
            }
            let target_group = match source_group {
                Selection::NativeEdges(_) => target_group,
                Selection::Ordinary(source_group) if !group::is_ordered(source_group) => target_group,
                Selection::Ordinary(source_group) => match mapping {
                    ElementMap::Explicit(source_of_target) => {
                        group::View::remap_order(source_group, source_of_target, &target_group)?
                    }
                    ElementMap::Identity(source_count) => {
                        let source_of_target: Vec<i64> =
                            (0..count).map(|target| if target < *source_count { target } else { -1 }).collect();
                        group::View::remap_order(source_group, &source_of_target, &target_group)?
                    }
                    ElementMap::Proximity { indices, distances_squared } => {
                        let source_order = group::View::order_view(source_group).unwrap_or(&[]);
                        let mut rank = vec![-1_i64; group::length(source_group) as usize];
                        for (position, &element) in source_order.iter().enumerate() {
                            rank[element as usize] = position as i64;
                        }
                        let mut order: Vec<i64> = Vec::with_capacity(group::cardinality(&target_group) as usize);
                        for element in 0..group::length(&target_group) {
                            if group::mem(element, &target_group) {
                                order.push(element);
                            }
                        }
                        order.sort_by(|&left, &right| {
                            let left_source = indices[left as usize] as usize;
                            let right_source = indices[right as usize] as usize;
                            rank[left_source]
                                .cmp(&rank[right_source])
                                .then_with(|| {
                                    float_compare(distances_squared[left as usize], distances_squared[right as usize])
                                })
                                .then_with(|| left.cmp(&right))
                        });
                        group::View::with_owned_order(order, &target_group)?
                    }
                },
            };
            Ok(Selection::Ordinary(target_group))
        }
        None => {
            let topology = geometry::topology(target);
            let index = topology_index::create(cancel_token, topology)?;
            let edges = edge_group::init(Some(grain), topology, &index, "__copied", &member)?;
            if polled.load(Ordering::Relaxed) {
                return Err(cancelled());
            }
            Ok(Selection::NativeEdges(edges))
        }
    }
}

fn unique_suffix(store: &GroupStore, owner: Owner, base: &str) -> Result<String, error::T> {
    const MAX_INT: i64 = (1_i64 << 62) - 1;
    let mut suffix: i64 = 2;
    let mut candidate = format!("{base}2");
    while store_find(store, owner, &candidate).is_some() {
        if suffix == MAX_INT {
            return Err(make_error("group_transfer", "invalid_group", "Group Copy: suffix space exhausted"));
        }
        suffix += 1;
        candidate = format!("{base}{suffix}");
    }
    Ok(candidate)
}

fn feature_proximity_map(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    distance: f64,
    source_features: proximity_index::Features,
    target_features: proximity_index::Features,
) -> Result<ElementMap, error::T> {
    let source_index = proximity_index::create(cancel_token, Some(grain), source_features)?;
    let (indices, distances_squared) = proximity_index::nearest_entities_with_distances(
        cancel_token,
        Some(grain),
        distance,
        &source_index,
        &target_features,
    )?;
    Ok(ElementMap::Proximity { indices, distances_squared })
}

fn transfer_mapping(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    distance: f64,
    owner: Owner,
    source: &geometry::T,
    target: &geometry::T,
) -> Result<ElementMap, error::T> {
    match owner {
        Owner::GroupPoints => point_proximity_map(cancel_token, grain, distance * distance, source, target),
        Owner::GroupPrimitives => feature_proximity_map(
            cancel_token,
            grain,
            distance,
            proximity_index::primitive_features(cancel_token, Some(grain), source)?,
            proximity_index::primitive_features(cancel_token, Some(grain), target)?,
        ),
        Owner::GroupEdges => feature_proximity_map(
            cancel_token,
            grain,
            distance,
            proximity_index::edge_features(cancel_token, Some(grain), source)?,
            proximity_index::edge_features(cancel_token, Some(grain), target)?,
        ),
        Owner::GroupVertices => Err(make_error(
            "group_transfer",
            "invalid_group",
            "Group Transfer: vertex groups are not supported",
        )),
    }
}

/// Nearest source point within `maximum_squared` for every target point (ties: lowest index).
fn point_proximity_map(
    cancel_token: Option<&cancel::T>,
    grain: i64,
    maximum_squared: f64,
    source: &geometry::T,
    target: &geometry::T,
) -> Result<ElementMap, error::T> {
    let index = spatial_index::create(cancel_token, Some(grain), None, geometry::positions(source))
        .map_err(|failure| make_error("group_transfer", "invalid_group", &error::to_string(&failure)))?;
    let count = geometry::point_count(target) as usize;
    let mut indices = vec![-1_i64; count];
    let mut distances = vec![f64::INFINITY; count];
    let mut counts = vec![0_i64; count];
    spatial_index::View::nearest_k_many_into(
        cancel_token,
        None,
        grain,
        &index,
        geometry::positions(target),
        maximum_squared,
        1,
        &mut indices,
        &mut distances,
        &mut counts,
    )?;
    Ok(ElementMap::Proximity { indices, distances_squared: distances })
}

pub fn transfer(params: &TransferParams) -> Result<geometry::T, error::T> {
    transfer_inner(params)
        .map_err(|failure| recode(&failure, "group_transfer", "invalid_group", DEFAULT_CANCELLED))
}

fn transfer_inner(params: &TransferParams) -> Result<geometry::T, error::T> {
    let cancel_token = params.cancel.as_deref();
    let grain = params.grain.unwrap_or(16_384);
    let fail = |message: &str| make_error("group_transfer", "invalid_group", message);
    if grain <= 0 {
        return Err(fail("Group Transfer: grain must be positive"));
    }
    let distance = params.distance;
    if !distance.is_finite() || distance < 0.0 || distance > f64::MAX.sqrt() {
        return Err(fail(
            "Group Transfer: distance threshold must be finite, non-negative, and safely squarable",
        ));
    }
    let source_store = store_of_geometry(&params.source);
    let mut target_store = store_of_geometry(&params.target);
    let mut point_mapping: Option<ElementMap> = None;
    let mut primitive_mapping: Option<ElementMap> = None;
    let mut edge_mapping: Option<ElementMap> = None;
    for rule in &params.rules {
        cancel::check_opt(cancel_token)?;
        if rule.transfer_owner == Owner::GroupVertices {
            return Err(fail("Group Transfer: vertex groups are not supported"));
        }
        let pattern_source = if is_blank(&rule.transfer_pattern) { "*" } else { rule.transfer_pattern.as_str() };
        let pattern = attribute_pattern::compile(pattern_source)?;
        let sources: Vec<&NamedEntry> = source_store
            .entries
            .iter()
            .filter(|entry| {
                entry.alive
                    && entry.owner == rule.transfer_owner
                    && attribute_pattern::matches(&pattern, &entry.entry_name)
            })
            .collect();
        if sources.is_empty() {
            continue;
        }
        let slot = match rule.transfer_owner {
            Owner::GroupPoints => &mut point_mapping,
            Owner::GroupPrimitives => &mut primitive_mapping,
            _ => &mut edge_mapping,
        };
        if slot.is_none() {
            *slot = Some(transfer_mapping(
                cancel_token,
                grain,
                distance,
                rule.transfer_owner,
                &params.source,
                &params.target,
            )?);
        }
        let mapping = slot.as_ref().ok_or_else(|| fail("Group Transfer: missing mapping"))?;
        for source_entry in sources {
            let proposed = format!("{}{}", rule.transfer_prefix, source_entry.entry_name);
            if is_blank(&proposed) {
                return Err(fail("Group Transfer: prefix produced an empty group name"));
            }
            let existing = store_find(&target_store, rule.transfer_owner, &proposed);
            if params.conflict == CopyConflict::CopySkip && existing.is_some() {
                continue;
            }
            let output_name = if params.conflict == CopyConflict::CopyAddSuffix && existing.is_some() {
                unique_suffix(&target_store, rule.transfer_owner, &proposed)?
            } else {
                proposed
            };
            let copied = copied_selection(
                cancel_token,
                grain,
                rule.transfer_owner,
                mapping,
                &source_entry.value,
                &params.target,
            )?;
            if !params.create_empty && selection_cardinality(&copied) == 0 {
                continue;
            }
            match store_find(&target_store, rule.transfer_owner, &output_name) {
                Some(destination) => store_replace(&mut target_store, destination, &copied)?,
                None => {
                    store_add(&mut target_store, rule.transfer_owner, &output_name, &copied)?;
                }
            }
        }
    }
    store_commit(&target_store, &params.target)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::attribute_ops::fixtures::*;
    use rdk_core::{packed, topology::PrimitiveKind};

    fn code_of(result: Result<geometry::T, error::T>) -> String {
        err_code(result)
    }

    fn cloud(count: i64) -> geometry::T {
        points(&(0..count).map(|point| (point as f64, 0.0, 0.0)).collect::<Vec<_>>())
    }

    fn mesh_of(faces: &[Vec<i64>], point_count: i64) -> geometry::T {
        polygons(&(0..point_count).map(|point| (point as f64, 0.0, 0.0)).collect::<Vec<_>>(), faces)
    }

    fn owner_len(g: &geometry::T, owner: group::Owner) -> i64 {
        match owner {
            group::Owner::Point => geometry::point_count(g),
            group::Owner::Vertex => geometry::vertex_count(g),
            group::Owner::Primitive => geometry::primitive_count(g),
        }
    }

    fn with_group_where(owner: group::Owner, name: &str, predicate: &(dyn Fn(i64) -> bool + Send + Sync), g: &geometry::T) -> geometry::T {
        let value = group::init(Some(1), owner, name, owner_len(g, owner), predicate).unwrap();
        geometry::with_group(value, g).unwrap()
    }

    fn with_ordered(owner: group::Owner, name: &str, elements: &[i64], g: &geometry::T) -> geometry::T {
        let value = group::ordered(owner, name, owner_len(g, owner), elements).unwrap();
        geometry::with_group(value, g).unwrap()
    }

    fn with_edges(name: &str, predicate: &(dyn Fn(i64) -> bool + Send + Sync), g: &geometry::T) -> geometry::T {
        let topology_value = geometry::topology(g);
        let index = topology_index::create(None, topology_value).unwrap();
        let value = edge_group::init(Some(1), topology_value, &index, name, predicate).unwrap();
        geometry::with_edge_group(value, g).unwrap()
    }

    fn ordinary(owner: group::Owner, name: &str, g: &geometry::T) -> group::T {
        geometry::find_group(owner, name, g).unwrap_or_else(|| panic!("missing group {name}"))
    }

    fn members(value: &group::T) -> Vec<i64> {
        (0..group::length(value)).filter(|&index| group::mem(index, value)).collect()
    }

    fn edge_members(name: &str, g: &geometry::T) -> Vec<i64> {
        let value = geometry::find_edge_group(name, g).unwrap_or_else(|| panic!("missing edge group {name}"));
        (0..edge_group::length(&value)).filter(|&edge| edge_group::mem(edge, &value)).collect()
    }

    fn rule(owner: Owner, pattern: &str, prefix: &str) -> TransferRule {
        TransferRule { transfer_owner: owner, transfer_pattern: pattern.to_owned(), transfer_prefix: prefix.to_owned() }
    }

    fn transfer_with(
        rules: Vec<TransferRule>,
        conflict: CopyConflict,
        create_empty: bool,
        distance: f64,
        source: &geometry::T,
        target: &geometry::T,
    ) -> Result<geometry::T, error::T> {
        transfer(&TransferParams {
            cancel: None,
            grain: Some(1),
            rules,
            conflict,
            create_empty,
            distance,
            source: source.clone(),
            target: target.clone(),
        })
    }

    fn curve_mesh(positions: &[(f64, f64, f64)], curves: &[Vec<i64>]) -> geometry::T {
        let primitives: Vec<(PrimitiveKind, Vec<i64>)> =
            curves.iter().map(|curve| (PrimitiveKind::OpenPolyline, curve.clone())).collect();
        mesh(positions, &primitives)
    }

    #[test]
    fn group_names_are_validated_and_forced() {
        assert!(valid_group_name("abc_9"));
        assert!(!valid_group_name("9abc") && !valid_group_name("") && !valid_group_name("a-b"));
        assert_eq!(force_valid_group_name("9a-b"), "_9a_b");
        assert_eq!(force_valid_group_name("ok_1"), "ok_1");
    }

    #[test]
    fn rename_and_delete_follow_conflict_policies() {
        let source = with_group_where(
            group::Owner::Point,
            "b",
            &|p| p == 1,
            &with_group_where(group::Owner::Point, "a", &|p| p == 0, &cloud(3)),
        );
        let renamed = rename(
            &[RenameRule {
                rename_owner: Some(Owner::GroupPoints),
                rename_pattern: "a".to_owned(),
                rename_replacement: "c".to_owned(),
                rename_conflict: RenameConflict::RenameError,
            }],
            &source,
        )
        .unwrap();
        assert_eq!(members(&ordinary(group::Owner::Point, "c", &renamed)), vec![0]);
        assert!(geometry::find_group(group::Owner::Point, "a", &renamed).is_none());
        let rule_to_b = |conflict| RenameRule {
            rename_owner: None,
            rename_pattern: "a".to_owned(),
            rename_replacement: "b".to_owned(),
            rename_conflict: conflict,
        };
        assert_eq!(code_of(rename(&[rule_to_b(RenameConflict::RenameError)], &source)), "invalid_group");
        let skipped = rename(&[rule_to_b(RenameConflict::RenameSkip)], &source).unwrap();
        assert_eq!(members(&ordinary(group::Owner::Point, "a", &skipped)), vec![0]);
        let overwritten = rename(&[rule_to_b(RenameConflict::RenameOverwrite)], &source).unwrap();
        assert_eq!(members(&ordinary(group::Owner::Point, "b", &overwritten)), vec![0]);
        assert!(geometry::find_group(group::Owner::Point, "a", &overwritten).is_none());
        let united = rename(&[rule_to_b(RenameConflict::RenameUnion)], &source).unwrap();
        assert_eq!(members(&ordinary(group::Owner::Point, "b", &united)), vec![0, 1]);
        let unchanged = rename(&[], &source).unwrap();
        assert_eq!(geometry::data_id(&unchanged), geometry::data_id(&source));

        let deleted = delete(
            &DeleteParams {
                rules: vec![DeleteRule { delete_owner: Some(Owner::GroupPoints), delete_pattern: "a".to_owned() }],
                delete_unused: false,
            },
            &source,
        )
        .unwrap();
        assert!(geometry::find_group(group::Owner::Point, "a", &deleted).is_none());
        assert!(geometry::find_group(group::Owner::Point, "b", &deleted).is_some());
        let with_empty = with_group_where(group::Owner::Point, "empty", &|_| false, &source);
        let unused = delete(&DeleteParams { rules: vec![], delete_unused: true }, &with_empty).unwrap();
        assert!(geometry::find_group(group::Owner::Point, "empty", &unused).is_none());
        assert!(geometry::find_group(group::Owner::Point, "a", &unused).is_some());
        let malformed = delete(
            &DeleteParams {
                rules: vec![DeleteRule { delete_owner: None, delete_pattern: "[bad".to_owned() }],
                delete_unused: false,
            },
            &source,
        );
        assert_eq!(code_of(malformed), "invalid_group");
    }

    #[test]
    fn transfer_point_proximity_conflicts_and_policies() {
        let source = with_group_where(
            group::Owner::Point,
            "unused",
            &|p| p == 1,
            &with_group_where(group::Owner::Point, "picked", &|p| p == 0, &cloud(2)),
        );
        let scaled_positions = [(0.0, 0.0, 0.0), (10.0, 0.0, 0.0)];
        let source = polygons_with_groups(&scaled_positions, &source);
        let target = points(&[(0.1, 0.0, 0.0), (9.9, 0.0, 0.0), (5.0, 0.0, 0.0)]);
        let rules = vec![rule(Owner::GroupPoints, "picked", "near_")];
        let go = |distance: f64, conflict, create_empty, rules: Vec<TransferRule>, target: &geometry::T| {
            transfer_with(rules, conflict, create_empty, distance, &source, target).unwrap()
        };
        let transferred = go(0.2, CopyConflict::CopySkip, false, rules.clone(), &target);
        assert_eq!(members(&ordinary(group::Owner::Point, "near_picked", &transferred)), vec![0]);
        let tied = go(5.0, CopyConflict::CopySkip, false, rules.clone(), &target);
        assert_eq!(members(&ordinary(group::Owner::Point, "near_picked", &tied)), vec![0, 2]);

        let ordered_source = with_ordered(group::Owner::Point, "path", &[1, 0], &points(&[(0.0, 0.0, 0.0), (10.0, 0.0, 0.0)]));
        let ordered_target = points(&[(0.9, 0.0, 0.0), (0.1, 0.0, 0.0), (9.8, 0.0, 0.0), (10.5, 0.0, 0.0)]);
        let ordered = transfer_with(
            vec![rule(Owner::GroupPoints, "path", "mapped_")],
            CopyConflict::CopySkip,
            false,
            1.0,
            &ordered_source,
            &ordered_target,
        )
        .unwrap();
        assert_eq!(
            group::ordered_elements(&ordinary(group::Owner::Point, "mapped_path", &ordered)),
            Some(vec![2, 3, 1, 0])
        );

        let conflict_target = with_group_where(group::Owner::Point, "near_picked", &|p| p == 2, &target);
        let skipped = go(0.2, CopyConflict::CopySkip, false, rules.clone(), &conflict_target);
        assert_eq!(members(&ordinary(group::Owner::Point, "near_picked", &skipped)), vec![2]);
        let overwritten = go(0.2, CopyConflict::CopyOverwrite, false, rules.clone(), &conflict_target);
        assert_eq!(members(&ordinary(group::Owner::Point, "near_picked", &overwritten)), vec![0]);
        let suffixed = go(0.2, CopyConflict::CopyAddSuffix, false, rules.clone(), &conflict_target);
        assert_eq!(members(&ordinary(group::Owner::Point, "near_picked", &suffixed)), vec![2]);
        assert_eq!(members(&ordinary(group::Owner::Point, "near_picked2", &suffixed)), vec![0]);

        let empty_rule = vec![rule(Owner::GroupPoints, "unused", "near_")];
        let omitted = go(0.05, CopyConflict::CopySkip, false, empty_rule.clone(), &target);
        assert!(geometry::find_group(group::Owner::Point, "near_unused", &omitted).is_none());
        let retained = go(0.05, CopyConflict::CopySkip, true, empty_rule, &target);
        assert_eq!(group::cardinality(&ordinary(group::Owner::Point, "near_unused", &retained)), 0);
        let no_op = go(0.001, CopyConflict::CopySkip, false, vec![rule(Owner::GroupPoints, "missing*", "mapped_")], &target);
        assert_eq!(geometry::data_id(&no_op), geometry::data_id(&target));
    }

    /// `source` with its positions replaced by `positions` (same topology, attributes and groups).
    fn polygons_with_groups(positions: &[(f64, f64, f64)], source: &geometry::T) -> geometry::T {
        let x: Vec<f64> = positions.iter().map(|p| p.0).collect();
        let y: Vec<f64> = positions.iter().map(|p| p.1).collect();
        let z: Vec<f64> = positions.iter().map(|p| p.2).collect();
        geometry::with_positions(packed::Float3::of_owned(x, y, z).unwrap(), source).unwrap()
    }

    #[test]
    fn transfer_curves_triangles_and_diagnostics() {
        let target = points(&[(0.1, 0.0, 0.0), (9.9, 0.0, 0.0), (5.0, 0.0, 0.0)]);
        let curve_source = curve_mesh(
            &[(-1.0, 0.0, 0.0), (1.0, 0.0, 0.0), (5.0, 0.0, 0.0), (6.0, 0.0, 0.0)],
            &[vec![0, 1], vec![2, 3]],
        );
        let curve_source = with_edges("crossing_edge", &|edge| edge == 0, &with_group_where(group::Owner::Primitive, "crossing_curve", &|p| p == 0, &curve_source));
        let curve_target = curve_mesh(&[(0.0, -1.0, 0.0), (0.0, 1.0, 0.0)], &[vec![0, 1]]);
        let transferred = transfer_with(
            vec![
                rule(Owner::GroupPrimitives, "crossing_curve", "mapped_"),
                rule(Owner::GroupEdges, "crossing_edge", "mapped_"),
            ],
            CopyConflict::CopySkip,
            false,
            0.0,
            &curve_source,
            &curve_target,
        )
        .unwrap();
        assert_eq!(members(&ordinary(group::Owner::Primitive, "mapped_crossing_curve", &transferred)), vec![0]);
        assert_eq!(edge_members("mapped_crossing_edge", &transferred), vec![0]);

        let triangle_source = polygons(
            &[(0.0, -1.0, -1.0), (0.0, 1.0, -1.0), (0.0, 0.0, 1.0), (-0.2, -0.2, 0.05), (0.2, -0.2, 0.05), (0.0, 0.2, 0.05)],
            &[vec![0, 1, 2], vec![3, 4, 5]],
        );
        let triangle_source = with_group_where(group::Owner::Primitive, "intersecting", &|p| p == 0, &triangle_source);
        let triangle_target = polygons(&[(-0.2, -0.2, 0.0), (0.2, -0.2, 0.0), (0.0, 0.2, 0.0)], &[vec![0, 1, 2]]);
        let triangle_transferred = transfer_with(
            vec![rule(Owner::GroupPrimitives, "intersecting", "mapped_")],
            CopyConflict::CopySkip,
            false,
            0.0,
            &triangle_source,
            &triangle_target,
        )
        .unwrap();
        assert_eq!(members(&ordinary(group::Owner::Primitive, "mapped_intersecting", &triangle_transferred)), vec![0]);

        let source = with_group_where(group::Owner::Point, "picked", &|p| p == 0, &cloud(2));
        assert_eq!(
            code_of(transfer_with(vec![], CopyConflict::CopySkip, false, -1.0, &source, &target)),
            "invalid_group"
        );
        assert_eq!(
            code_of(transfer_with(vec![rule(Owner::GroupVertices, "*", "")], CopyConflict::CopySkip, false, 0.001, &source, &target)),
            "invalid_group"
        );
        let degenerate = with_group_where(group::Owner::Primitive, "bad", &|_| true, &mesh_of(&[vec![0, 1, 2]], 3));
        assert_eq!(
            code_of(transfer_with(
                vec![rule(Owner::GroupPrimitives, "bad", "")],
                CopyConflict::CopySkip,
                false,
                1.0,
                &degenerate,
                &triangle_target
            )),
            "invalid_group"
        );
        let nonfinite = with_group_where(group::Owner::Point, "bad", &|_| true, &points(&[(f64::NAN, 0.0, 0.0)]));
        assert_eq!(
            code_of(transfer_with(vec![rule(Owner::GroupPoints, "bad", "")], CopyConflict::CopySkip, false, 1.0, &nonfinite, &target)),
            "invalid_group"
        );
        let huge = with_group_where(
            group::Owner::Primitive,
            "huge",
            &|_| true,
            &curve_mesh(&[(-f64::MAX, 0.0, 0.0), (f64::MAX, 0.0, 0.0)], &[vec![0, 1]]),
        );
        assert_eq!(
            code_of(transfer_with(vec![rule(Owner::GroupPrimitives, "huge", "")], CopyConflict::CopySkip, false, 1.0, &huge, &huge)),
            "invalid_group"
        );
    }

    #[test]
    fn transfer_is_exact_across_domains_and_cancels() {
        let base = grid(100, 80);
        let source = with_edges(
            "edge_band",
            &|edge| edge % 23 < 2,
            &with_group_where(
                group::Owner::Primitive,
                "face_band",
                &|p| p % 19 < 3,
                &with_ordered(
                    group::Owner::Point,
                    "ordered_seed",
                    &[404, 5, 8_000, 2],
                    &with_group_where(group::Owner::Point, "point_band", &|p| p % 101 < 7, &base),
                ),
            ),
        );
        let (px, py, pz) = positions(&source);
        let shifted: Vec<(f64, f64, f64)> = (0..px.len()).map(|i| (px[i] + 0.0001, py[i], pz[i] + 0.0001)).collect();
        let target = polygons_with_groups(&shifted, &source);
        let rules = vec![
            rule(Owner::GroupPoints, "point_band", "mapped_"),
            rule(Owner::GroupPoints, "ordered_seed", "mapped_"),
            rule(Owner::GroupPrimitives, "face_band", "mapped_"),
            rule(Owner::GroupEdges, "edge_band", "mapped_"),
        ];
        let run = |domains: i64| {
            on_domains(domains, &|| {
                transfer(&TransferParams {
                    cancel: None,
                    grain: Some(257),
                    rules: rules.clone(),
                    conflict: CopyConflict::CopyOverwrite,
                    create_empty: false,
                    distance: 0.01,
                    source: source.clone(),
                    target: target.clone(),
                })
                .unwrap()
            })
        };
        let (one, four) = (run(1), run(4));
        for (owner, name) in [
            (group::Owner::Point, "mapped_point_band"),
            (group::Owner::Primitive, "mapped_face_band"),
            (group::Owner::Point, "mapped_ordered_seed"),
        ] {
            let (left, right) = (ordinary(owner, name, &one), ordinary(owner, name, &four));
            assert_eq!(members(&left), members(&right), "{name}");
            assert_eq!(group::ordered_elements(&left), group::ordered_elements(&right), "{name}");
        }
        assert_eq!(edge_members("mapped_edge_band", &one), edge_members("mapped_edge_band", &four));
        assert!(!members(&ordinary(group::Owner::Point, "mapped_point_band", &one)).is_empty());
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        let cancelled = transfer(&TransferParams {
            cancel: Some(token),
            grain: Some(1),
            rules,
            conflict: CopyConflict::CopySkip,
            create_empty: false,
            distance: 0.01,
            source,
            target,
        });
        assert_eq!(code_of(cancelled), "cancelled");
    }
}
