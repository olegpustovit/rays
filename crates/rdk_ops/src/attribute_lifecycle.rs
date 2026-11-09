//! Attribute delete, rename and swap/move/copy over the attribute table.
use std::collections::HashMap;
use std::sync::Arc;

use rdk_core::{attribute, attribute_pattern, cancel, error, geometry, packed};
use rdk_core::error::Code;

use crate::attribute_ops::{is_blank, make_error, ocaml_quote};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RenameConflict {
    AttributeRenameSkip,
    AttributeRenameError,
    AttributeRenameOverwrite,
}

#[derive(Clone, Debug)]
pub struct RenameRule {
    pub rename_attribute_owner: Option<attribute::Owner>,
    pub rename_attribute_pattern: String,
    pub rename_attribute_replacement: String,
    pub rename_attribute_conflict: RenameConflict,
}

#[derive(Clone)]
pub struct DeleteParams {
    pub cancel: Option<Arc<cancel::T>>,
    pub reference: Option<geometry::T>,
    pub delete_non_selected: bool,
    pub point_pattern: Option<String>,
    pub vertex_pattern: Option<String>,
    pub primitive_pattern: Option<String>,
    pub detail_pattern: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SwapMethod {
    AttributeSwap,
    AttributeMove,
    AttributeCopy,
}

#[derive(Clone, Debug)]
pub struct SwapRule {
    pub swap_attribute_owner: attribute::Owner,
    pub swap_attribute_source: String,
    pub swap_attribute_destination: String,
    pub swap_attribute_method: SwapMethod,
}

fn fail(message: String) -> error::T {
    make_error("attribute_lifecycle", Code::InvalidAttribute, &message)
}

fn owner_index(owner: attribute::Owner) -> usize {
    match owner {
        attribute::Owner::Point => 0,
        attribute::Owner::Vertex => 1,
        attribute::Owner::Primitive => 2,
        attribute::Owner::Detail => 3,
    }
}

use rdk_core::attribute::owner_name;

fn check_cancel(cancel_token: Option<&cancel::T>, index: i64) -> Result<(), error::T> {
    cancel::poll(cancel_token, index, 1023)
}

fn compile_optional(label: &str, source: &Option<String>) -> Result<Option<attribute_pattern::T>, error::T> {
    match source {
        None => Ok(None),
        Some(source) if is_blank(source) => Ok(None),
        Some(source) => attribute_pattern::compile(source)
            .map(Some)
            .map_err(|failure| fail(format!("{label}: {}", error::message(&failure)))),
    }
}

fn reference_names(reference: &Option<geometry::T>) -> Vec<HashMap<String, ()>> {
    let mut tables: Vec<HashMap<String, ()>> = (0..4).map(|_| HashMap::new()).collect();
    if let Some(reference) = reference {
        for item in geometry::view::attributes(reference) {
            tables[owner_index(attribute::owner(item))].insert(attribute::name(item).to_owned(), ());
        }
    }
    tables
}

pub(crate) fn delete(params: &DeleteParams, geometry_value: &geometry::T) -> Result<geometry::T, error::T> {
    let cancel_token = params.cancel.as_deref();
    let point = compile_optional("Attribute Delete point pattern", &params.point_pattern)?;
    let vertex = compile_optional("Attribute Delete vertex pattern", &params.vertex_pattern)?;
    let primitive = compile_optional("Attribute Delete primitive pattern", &params.primitive_pattern)?;
    let detail = compile_optional("Attribute Delete detail pattern", &params.detail_pattern)?;
    let patterns = [point, vertex, primitive, detail];
    let names = reference_names(&params.reference);
    let nonempty: Vec<bool> = names.iter().map(|table| !table.is_empty()).collect();
    let selected = |item: &attribute::T| -> bool {
        let owner = owner_index(attribute::owner(item));
        let name = attribute::name(item);
        let from_reference = names[owner].contains_key(name);
        match &patterns[owner] {
            None => from_reference,
            Some(pattern) if nonempty[owner] => attribute_pattern::apply(from_reference, pattern, name),
            Some(pattern) => attribute_pattern::matches(pattern, name),
        }
    };
    let attributes = geometry::view::attributes(geometry_value);
    if attributes.is_empty() {
        return Ok(geometry_value.clone());
    }
    let mut output: Vec<attribute::T> = Vec::with_capacity(attributes.len());
    for (index, item) in attributes.iter().enumerate() {
        check_cancel(cancel_token, index as i64)?;
        let remove = if params.delete_non_selected { !selected(item) } else { selected(item) };
        if !remove {
            output.push(item.clone());
        }
    }
    if output.len() == attributes.len() {
        return Ok(geometry_value.clone());
    }
    geometry::view::with_attributes_owned(output, geometry_value)
}

struct Entry {
    owner: attribute::Owner,
    attribute: attribute::T,
    alive: bool,
}

struct Store {
    entries: Vec<Entry>,
    tables: Vec<HashMap<String, usize>>,
    changed: bool,
    positions: packed::float3::T,
    attributes_changed: bool,
    positions_changed: bool,
}

fn store_of_geometry(geometry_value: &geometry::T) -> Store {
    let attributes = geometry::view::attributes(geometry_value);
    let entries: Vec<Entry> = attributes
        .iter()
        .map(|item| Entry { owner: attribute::owner(item), attribute: item.clone(), alive: true })
        .collect();
    let mut tables: Vec<HashMap<String, usize>> = (0..4).map(|_| HashMap::new()).collect();
    for (index, entry) in entries.iter().enumerate() {
        tables[owner_index(entry.owner)].insert(attribute::name(&entry.attribute).to_owned(), index);
    }
    Store {
        entries,
        tables,
        changed: false,
        positions: geometry::positions(geometry_value).clone(),
        attributes_changed: false,
        positions_changed: false,
    }
}

fn store_remove(store: &mut Store, index: usize) {
    if store.entries[index].alive {
        let owner = owner_index(store.entries[index].owner);
        let name = attribute::name(&store.entries[index].attribute).to_owned();
        store.tables[owner].remove(&name);
        store.entries[index].alive = false;
        store.changed = true;
    }
}

fn store_rename(store: &mut Store, index: usize, name: &str) -> Result<(), error::T> {
    let owner = owner_index(store.entries[index].owner);
    let old = attribute::name(&store.entries[index].attribute).to_owned();
    store.tables[owner].remove(&old);
    store.entries[index].attribute = attribute::with_name(name, &store.entries[index].attribute)?;
    store.tables[owner].insert(name.to_owned(), index);
    store.changed = true;
    Ok(())
}

fn rename_entry(
    store: &mut Store,
    conflict: RenameConflict,
    index: usize,
    name: &str,
) -> Result<(), error::T> {
    let old_name = attribute::name(&store.entries[index].attribute).to_owned();
    let owner = store.entries[index].owner;
    if old_name == name {
        return Ok(());
    }
    if is_blank(name) {
        return Err(fail("Attribute Rename: rewrite produced an empty attribute name".to_owned()));
    }
    if owner == attribute::Owner::Point && name == "P" {
        return Err(fail("Attribute Rename: canonical point position P is reserved".to_owned()));
    }
    match store.tables[owner_index(owner)].get(name).copied() {
        None => store_rename(store, index, name),
        Some(destination) if destination == index => Ok(()),
        Some(destination) => match conflict {
            RenameConflict::AttributeRenameSkip => Ok(()),
            RenameConflict::AttributeRenameError => Err(fail(format!(
                "Attribute Rename: destination {} attribute {} already exists",
                owner_name(owner),
                ocaml_quote(name)
            ))),
            RenameConflict::AttributeRenameOverwrite => {
                store_remove(store, destination);
                store_rename(store, index, name)
            }
        },
    }
}

fn name_of(bytes: Vec<u8>) -> String {
    String::from_utf8(bytes).unwrap_or_else(|bad| String::from_utf8_lossy(bad.as_bytes()).into_owned())
}

pub(crate) fn rename(
    cancel_value: Option<&cancel::T>,
    rules: &[RenameRule],
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let mut compiled: Vec<(&RenameRule, attribute_pattern::Rewrite)> = Vec::new();
    for rule in rules {
        let rewrite = attribute_pattern::compile_rewrite(
            &rule.rename_attribute_pattern,
            &rule.rename_attribute_replacement,
        )
        .map_err(|failure| fail(format!("Attribute Rename: {}", error::message(&failure))))?;
        compiled.push((rule, rewrite));
    }
    let mut store = store_of_geometry(geometry_value);
    for (rule, rewrite) in &compiled {
        for index in 0..store.entries.len() {
            check_cancel(cancel_value, index as i64)?;
            let entry = &store.entries[index];
            let applies = entry.alive
                && match rule.rename_attribute_owner {
                    None => true,
                    Some(owner) => owner == entry.owner,
                };
            if !applies {
                continue;
            }
            let current = attribute::name(&entry.attribute).to_owned();
            if let Some(name) = attribute_pattern::rewrite(rewrite, &current) {
                rename_entry(&mut store, rule.rename_attribute_conflict, index, &name_of(name))?;
            }
        }
    }
    if !store.changed {
        return Ok(geometry_value.clone());
    }
    let output: Vec<attribute::T> = store
        .entries
        .iter()
        .filter(|entry| entry.alive)
        .map(|entry| entry.attribute.clone())
        .collect();
    geometry::view::with_attributes_owned(output, geometry_value)
}

#[derive(Clone, Copy)]
enum Slot {
    Position,
    Attribute(usize),
}

fn swap_find(store: &Store, owner: attribute::Owner, name: &str) -> Option<Slot> {
    if owner == attribute::Owner::Point && name == "P" {
        return Some(Slot::Position);
    }
    match store.tables[owner_index(owner)].get(name) {
        Some(&index) if store.entries[index].alive => Some(Slot::Attribute(index)),
        _ => None,
    }
}

fn swap_remove(store: &mut Store, index: usize) {
    if store.entries[index].alive {
        let owner = owner_index(store.entries[index].owner);
        let name = attribute::name(&store.entries[index].attribute).to_owned();
        store.tables[owner].remove(&name);
        store.entries[index].alive = false;
        store.attributes_changed = true;
    }
}

fn swap_set_position(store: &mut Store, item: &attribute::T) -> Result<(), error::T> {
    match attribute::view::storage(item)? {
        attribute::Storage::Float3(positions) => {
            if packed::float3::data_id(positions) != packed::float3::data_id(&store.positions) {
                store.positions = positions.clone();
                store.positions_changed = true;
            }
            Ok(())
        }
        _ => Err(fail(format!(
            "Attribute Swap: point position P requires float3 storage, but {} is {}",
            ocaml_quote(attribute::name(item)),
            attribute::kind_name(item)
        ))),
    }
}

fn position_attribute(store: &Store, name: &str) -> Result<attribute::T, error::T> {
    attribute::create_owned(
        name,
        attribute::Owner::Point,
        attribute::Storage::Float3(store.positions.clone()),
    )
}

fn swap_replace_or_append(
    store: &mut Store,
    owner: attribute::Owner,
    name: &str,
    item: &attribute::T,
) -> Result<(), error::T> {
    let replacement = attribute::with_name(name, item)?;
    match swap_find(store, owner, name) {
        Some(Slot::Position) => swap_set_position(store, &replacement),
        Some(Slot::Attribute(destination)) => {
            store.entries[destination].attribute = replacement;
            store.attributes_changed = true;
            Ok(())
        }
        None => {
            store.entries.push(Entry { owner, attribute: replacement, alive: true });
            let index = store.entries.len() - 1;
            store.tables[owner_index(owner)].insert(name.to_owned(), index);
            store.attributes_changed = true;
            Ok(())
        }
    }
}

fn swap_copy_slot(
    store: &mut Store,
    owner: attribute::Owner,
    source: &str,
    destination: &str,
) -> Result<(), error::T> {
    if source == destination {
        return Ok(());
    }
    match swap_find(store, owner, source) {
        None => Ok(()),
        Some(Slot::Position) => {
            let item = position_attribute(store, destination)?;
            swap_replace_or_append(store, owner, destination, &item)
        }
        Some(Slot::Attribute(index)) => {
            let item = store.entries[index].attribute.clone();
            swap_replace_or_append(store, owner, destination, &item)
        }
    }
}

fn swap_move_slot(
    store: &mut Store,
    owner: attribute::Owner,
    source: &str,
    destination: &str,
) -> Result<(), error::T> {
    if source == destination {
        return Ok(());
    }
    match swap_find(store, owner, source) {
        None => Ok(()),
        // Canonical P must always exist, so Houdini defines moving it as a copy.
        Some(Slot::Position) => swap_copy_slot(store, owner, source, destination),
        Some(Slot::Attribute(source_entry)) => {
            let table = owner_index(owner);
            match swap_find(store, owner, destination) {
                Some(Slot::Position) => {
                    let item = store.entries[source_entry].attribute.clone();
                    swap_set_position(store, &item)?;
                    swap_remove(store, source_entry);
                    Ok(())
                }
                Some(Slot::Attribute(destination_entry)) => {
                    swap_remove(store, destination_entry);
                    let renamed = attribute::with_name(destination, &store.entries[source_entry].attribute)?;
                    store.tables[table].remove(source);
                    store.entries[source_entry].attribute = renamed;
                    store.tables[table].insert(destination.to_owned(), source_entry);
                    store.attributes_changed = true;
                    Ok(())
                }
                None => {
                    let renamed = attribute::with_name(destination, &store.entries[source_entry].attribute)?;
                    store.tables[table].remove(source);
                    store.entries[source_entry].attribute = renamed;
                    store.tables[table].insert(destination.to_owned(), source_entry);
                    store.attributes_changed = true;
                    Ok(())
                }
            }
        }
    }
}

fn swap_two_slots(
    store: &mut Store,
    owner: attribute::Owner,
    source: &str,
    destination: &str,
) -> Result<(), error::T> {
    if source == destination {
        return Ok(());
    }
    match (swap_find(store, owner, source), swap_find(store, owner, destination)) {
        (None, None) => Ok(()),
        (Some(_), None) => swap_copy_slot(store, owner, source, destination),
        (None, Some(_)) => swap_copy_slot(store, owner, destination, source),
        (Some(Slot::Position), Some(Slot::Position)) => Ok(()),
        (Some(Slot::Attribute(source_entry)), Some(Slot::Attribute(destination_entry))) => {
            let source_attribute = attribute::with_name(source, &store.entries[destination_entry].attribute)?;
            let destination_attribute = attribute::with_name(destination, &store.entries[source_entry].attribute)?;
            store.entries[source_entry].attribute = source_attribute;
            store.entries[destination_entry].attribute = destination_attribute;
            store.attributes_changed = true;
            Ok(())
        }
        (Some(Slot::Position), Some(Slot::Attribute(destination_entry))) => {
            let old_positions = store.positions.clone();
            let item = store.entries[destination_entry].attribute.clone();
            swap_set_position(store, &item)?;
            store.entries[destination_entry].attribute = attribute::create_owned(
                destination,
                attribute::Owner::Point,
                attribute::Storage::Float3(old_positions),
            )?;
            store.attributes_changed = true;
            Ok(())
        }
        (Some(Slot::Attribute(source_entry)), Some(Slot::Position)) => {
            let old_positions = store.positions.clone();
            let item = store.entries[source_entry].attribute.clone();
            swap_set_position(store, &item)?;
            store.entries[source_entry].attribute = attribute::create_owned(
                source,
                attribute::Owner::Point,
                attribute::Storage::Float3(old_positions),
            )?;
            store.attributes_changed = true;
            Ok(())
        }
    }
}

pub(crate) fn swap(
    cancel_value: Option<&cancel::T>,
    rules: &[SwapRule],
    geometry_value: &geometry::T,
) -> Result<geometry::T, error::T> {
    let mut compiled: Vec<(&SwapRule, attribute_pattern::Rewrite, attribute_pattern::Rewrite)> = Vec::new();
    for rule in rules {
        let both = attribute_pattern::compile_rewrite(&rule.swap_attribute_source, &rule.swap_attribute_destination)
            .and_then(|forward| {
                attribute_pattern::compile_rewrite(&rule.swap_attribute_destination, &rule.swap_attribute_source)
                    .map(|backward| (forward, backward))
            })
            .map_err(|failure| fail(format!("Attribute Swap: {}", error::message(&failure))))?;
        compiled.push((rule, both.0, both.1));
    }
    let mut store = store_of_geometry(geometry_value);
    for (rule, forward, backward) in &compiled {
        let owner = rule.swap_attribute_owner;
        let mut matches: Vec<(String, String)> = Vec::new();
        if owner == attribute::Owner::Point {
            if let Some(destination) = attribute_pattern::rewrite(forward, "P") {
                matches.push(("P".to_owned(), name_of(destination)));
            }
        }
        let limit = store.entries.len();
        for index in 0..limit {
            check_cancel(cancel_value, index as i64)?;
            let entry = &store.entries[index];
            if entry.alive && entry.owner == owner {
                let source = attribute::name(&entry.attribute).to_owned();
                if let Some(destination) = attribute_pattern::rewrite(forward, &source) {
                    matches.push((source, name_of(destination)));
                }
            }
        }
        if !matches.is_empty() {
            for (source, destination) in &matches {
                cancel::check_opt(cancel_value)?;
                match rule.swap_attribute_method {
                    SwapMethod::AttributeSwap => swap_two_slots(&mut store, owner, source, destination)?,
                    SwapMethod::AttributeMove => swap_move_slot(&mut store, owner, source, destination)?,
                    SwapMethod::AttributeCopy => swap_copy_slot(&mut store, owner, source, destination)?,
                }
            }
        } else if rule.swap_attribute_method == SwapMethod::AttributeSwap {
            // A swap whose source is missing copies the other way; scan the destination pattern
            // only when the forward side matched nothing, avoiding double-processing pairs.
            let mut inverse: Vec<(String, String)> = Vec::new();
            if owner == attribute::Owner::Point {
                if let Some(source) = attribute_pattern::rewrite(backward, "P") {
                    inverse.push(("P".to_owned(), name_of(source)));
                }
            }
            let limit = store.entries.len();
            for index in 0..limit {
                check_cancel(cancel_value, index as i64)?;
                let entry = &store.entries[index];
                if entry.alive && entry.owner == owner {
                    let destination = attribute::name(&entry.attribute).to_owned();
                    if let Some(source) = attribute_pattern::rewrite(backward, &destination) {
                        inverse.push((destination, name_of(source)));
                    }
                }
            }
            for (destination, source) in &inverse {
                cancel::check_opt(cancel_value)?;
                if swap_find(&store, owner, source).is_none() {
                    swap_copy_slot(&mut store, owner, destination, source)?;
                }
            }
        }
    }
    if !store.attributes_changed && !store.positions_changed {
        return Ok(geometry_value.clone());
    }
    let attributes: Vec<attribute::T> = store
        .entries
        .iter()
        .filter(|entry| entry.alive)
        .map(|entry| entry.attribute.clone())
        .collect();
    if store.positions_changed {
        geometry::view::with_positions_and_attributes_owned(store.positions.clone(), attributes, geometry_value)
    } else {
        geometry::view::with_attributes_owned(attributes, geometry_value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::attribute_ops::{self, fixtures::*};
    use attribute::Owner::{Detail, Point, Primitive, Vertex};
    use rdk_core::packed;

    fn fixture() -> geometry::T {
        let g = grid(1, 1);
        let g = add_float(Point, "keep_point", 1.0, &g);
        let g = add_float(Point, "temporary_point", 2.0, &g);
        let g = add_float(Point, "temporary_keep", 3.0, &g);
        let g = add_float(Point, "target", 4.0, &g);
        let g = add_float(Vertex, "keep_vertex", 5.0, &g);
        let g = add_float(Vertex, "temporary_vertex", 6.0, &g);
        let g = add_float(Primitive, "keep_primitive", 7.0, &g);
        let g = add_float(Primitive, "temporary_primitive", 8.0, &g);
        let g = add_float(Detail, "keep_detail", 9.0, &g);
        add_float(Detail, "temporary_detail", 10.0, &g)
    }

    fn delete_params(
        reference: Option<geometry::T>,
        non_selected: bool,
        point: Option<&str>,
        vertex: Option<&str>,
        primitive: Option<&str>,
        detail: Option<&str>,
    ) -> DeleteParams {
        DeleteParams {
            cancel: None,
            reference,
            delete_non_selected: non_selected,
            point_pattern: point.map(str::to_owned),
            vertex_pattern: vertex.map(str::to_owned),
            primitive_pattern: primitive.map(str::to_owned),
            detail_pattern: detail.map(str::to_owned),
        }
    }

    fn rename_rule(owner: Option<attribute::Owner>, pattern: &str, replacement: &str, conflict: RenameConflict) -> RenameRule {
        RenameRule {
            rename_attribute_owner: owner,
            rename_attribute_pattern: pattern.to_owned(),
            rename_attribute_replacement: replacement.to_owned(),
            rename_attribute_conflict: conflict,
        }
    }

    fn swap_rule(owner: attribute::Owner, source: &str, destination: &str, method: SwapMethod) -> SwapRule {
        SwapRule {
            swap_attribute_owner: owner,
            swap_attribute_source: source.to_owned(),
            swap_attribute_destination: destination.to_owned(),
            swap_attribute_method: method,
        }
    }

    #[test]
    fn pattern_apply_subtracts_and_adds_to_an_implicit_selection() {
        let applied = attribute_pattern::compile("^bar score").unwrap();
        assert!(!attribute_pattern::apply(true, &applied, "bar"));
        assert!(attribute_pattern::apply(false, &applied, "score"));
    }

    #[test]
    fn delete_selection_identity_sharing_and_diagnostics() {
        let source = fixture();
        let source_id = geometry::data_id(&source);
        let none = attribute_ops::delete(&delete_params(None, false, None, None, None, None), &source).unwrap();
        assert_eq!(geometry::data_id(&none), source_id, "empty Attribute Delete keeps identity");
        let blank = attribute_ops::delete(&delete_params(None, false, Some("  "), None, None, None), &source).unwrap();
        assert_eq!(geometry::data_id(&blank), source_id);
        let keep_nothing = attribute_ops::delete(&delete_params(None, true, None, None, None, None), &source).unwrap();
        assert!(geometry::attributes(&keep_nothing).is_empty());

        let before = find(Point, "keep_point", &source);
        let deleted = attribute_ops::delete(
            &delete_params(None, false, Some("temporary_* ^temporary_keep"), Some("temporary_*"), Some("temporary_*"), Some("temporary_*")),
            &source,
        )
        .unwrap();
        assert!(!has(Point, "temporary_point", &deleted));
        assert!(has(Point, "temporary_keep", &deleted));
        assert!(!has(Vertex, "temporary_vertex", &deleted));
        assert!(!has(Primitive, "temporary_primitive", &deleted));
        assert!(!has(Detail, "temporary_detail", &deleted));
        let after = find(Point, "keep_point", &deleted);
        assert_eq!(attribute::data_id(&after), attribute::data_id(&before));
        assert_eq!(attribute::storage_id(&after), attribute::storage_id(&before));
        assert_eq!(
            packed::float3::data_id(geometry::positions(&source)),
            packed::float3::data_id(geometry::positions(&deleted))
        );

        let kept = attribute_ops::delete(
            &delete_params(None, true, Some("keep_*"), Some("keep_*"), Some("keep_*"), Some("keep_*")),
            &source,
        )
        .unwrap();
        assert_eq!(geometry::attributes(&kept).len(), 4);
        assert!(has(Point, "keep_point", &kept) && has(Vertex, "keep_vertex", &kept));
        assert!(has(Primitive, "keep_primitive", &kept) && has(Detail, "keep_detail", &kept));

        let reference = add_float(Point, "foo", 2.0, &add_float(Point, "bar", 1.0, &points(&[(0.0, 0.0, 0.0)])));
        let reference_source = add_float(
            Point,
            "weight",
            4.0,
            &add_float(Point, "score", 3.0, &add_float(Point, "foo", 2.0, &add_float(Point, "bar", 1.0, &points(&[(0.0, 0.0, 0.0)])))),
        );
        let reference_deleted = attribute_ops::delete(
            &delete_params(Some(reference.clone()), false, Some("^bar score"), None, None, None),
            &reference_source,
        )
        .unwrap();
        assert!(has(Point, "bar", &reference_deleted) && !has(Point, "foo", &reference_deleted));
        assert!(!has(Point, "score", &reference_deleted) && has(Point, "weight", &reference_deleted));
        let reference_kept = attribute_ops::delete(
            &delete_params(Some(reference), true, Some("^bar"), None, None, None),
            &reference_source,
        )
        .unwrap();
        assert!(!has(Point, "bar", &reference_kept) && has(Point, "foo", &reference_kept));
        assert!(!has(Point, "score", &reference_kept));

        let exclusion = attribute_ops::delete(&delete_params(None, false, Some("^keep_point"), None, None, None), &source).unwrap();
        assert!(has(Point, "keep_point", &exclusion) && !has(Point, "target", &exclusion) && has(Vertex, "keep_vertex", &exclusion));

        let malformed = attribute_ops::delete(&delete_params(None, false, Some("[bad"), None, None, None), &source);
        assert_eq!(err_code(malformed), "invalid_attribute");
        let token = Arc::new(cancel::create());
        cancel::cancel(&token);
        let mut params = delete_params(None, false, Some("*"), None, None, None);
        params.cancel = Some(token);
        assert_eq!(err_code(attribute_ops::delete(&params, &source)), "cancelled");
    }

    #[test]
    fn rename_rules_conflicts_and_diagnostics() {
        let source = fixture();
        let source_id = geometry::data_id(&source);
        let before = find(Point, "temporary_point", &source);
        let untouched_before = find(Point, "keep_point", &source);
        let renamed = attribute_ops::rename(
            None,
            &[
                rename_rule(Some(Point), "temporary_*", "stage_*", RenameConflict::AttributeRenameError),
                rename_rule(None, "stage_*", "final_*", RenameConflict::AttributeRenameError),
            ],
            &source,
        )
        .unwrap();
        assert!(has(Point, "final_point", &renamed) && has(Point, "final_keep", &renamed));
        assert!(has(Vertex, "temporary_vertex", &renamed) && !has(Point, "temporary_point", &renamed));
        let after = find(Point, "final_point", &renamed);
        assert_ne!(attribute::data_id(&after), attribute::data_id(&before));
        assert_eq!(attribute::storage_id(&after), attribute::storage_id(&before));
        assert_eq!(
            attribute::data_id(&find(Point, "keep_point", &renamed)),
            attribute::data_id(&untouched_before)
        );

        let none = attribute_ops::rename(None, &[], &source).unwrap();
        let missing = attribute_ops::rename(
            None,
            &[rename_rule(None, "missing_*", "still_missing_*", RenameConflict::AttributeRenameError)],
            &source,
        )
        .unwrap();
        let same = attribute_ops::rename(
            None,
            &[rename_rule(Some(Point), "keep_point", "keep_point", RenameConflict::AttributeRenameError)],
            &source,
        )
        .unwrap();
        assert!([&none, &missing, &same].iter().all(|g| geometry::data_id(g) == source_id));

        let skipped = attribute_ops::rename(
            None,
            &[rename_rule(Some(Point), "temporary_point", "target", RenameConflict::AttributeRenameSkip)],
            &source,
        )
        .unwrap();
        assert_eq!(geometry::data_id(&skipped), source_id);
        let conflict = attribute_ops::rename(
            None,
            &[rename_rule(Some(Point), "temporary_point", "target", RenameConflict::AttributeRenameError)],
            &source,
        );
        assert_eq!(err_code(conflict), "invalid_attribute");
        let overwritten = attribute_ops::rename(
            None,
            &[rename_rule(Some(Point), "temporary_point", "target", RenameConflict::AttributeRenameOverwrite)],
            &source,
        )
        .unwrap();
        assert!(!has(Point, "temporary_point", &overwritten));
        assert_eq!(attribute::storage_id(&find(Point, "target", &overwritten)), attribute::storage_id(&before));
        assert_eq!(geometry::attributes(&overwritten).len(), geometry::attributes(&source).len() - 1);
        let to_p = attribute_ops::rename(
            None,
            &[rename_rule(Some(Point), "temporary_point", "P", RenameConflict::AttributeRenameOverwrite)],
            &source,
        );
        assert_eq!(err_code(to_p), "invalid_attribute");
        let malformed = attribute_ops::rename(
            None,
            &[rename_rule(None, "temporary_*", "fixed", RenameConflict::AttributeRenameError)],
            &source,
        );
        assert_eq!(err_code(malformed), "invalid_attribute");
        let token = cancel::create();
        cancel::cancel(&token);
        let cancelled = attribute_ops::rename(
            Some(&token),
            &[rename_rule(None, "temporary_*", "cancelled_*", RenameConflict::AttributeRenameError)],
            &source,
        );
        assert_eq!(err_code(cancelled), "cancelled");
    }

    fn swap_source() -> geometry::T {
        let base = points(&[(1.0, 2.0, 3.0), (4.0, 5.0, 6.0)]);
        let g = add_float(Point, "left_weight", 10.0, &base);
        let g = add_float(Point, "right_weight", 20.0, &g);
        let g = add_float(Vertex, "left_corner", 30.0, &g);
        let g = add_float(Primitive, "left_face", 40.0, &g);
        let g = add_storage(Detail, "left_float", attribute::Storage::Float(Arc::new(vec![1.25])), &g);
        let g = add_storage(Detail, "left_int", attribute::Storage::Int(Arc::new(vec![7])), &g);
        let g = add_storage(
            Detail,
            "left_int_array",
            attribute::Storage::IntArray(packed::int_array::create_owned(vec![0, 2], vec![2, 5]).unwrap()),
            &g,
        );
        let g = add_storage(
            Detail,
            "left_float_array",
            attribute::Storage::FloatArray(packed::float_array::create_owned(vec![0, 2], vec![2.5, 5.5]).unwrap()),
            &g,
        );
        let g = add_storage(
            Detail,
            "left_float2",
            attribute::Storage::Float2(packed::float2::of_owned(vec![2.0], vec![3.0]).unwrap()),
            &g,
        );
        let g = add_storage(
            Detail,
            "left_float3",
            attribute::Storage::Float3(packed::float3::of_owned(vec![2.0], vec![3.0], vec![4.0]).unwrap()),
            &g,
        );
        let g = add_storage(
            Detail,
            "left_float4",
            attribute::Storage::Float4(
                packed::float4::of_owned(vec![2.0], vec![3.0], vec![4.0], vec![5.0]).unwrap(),
            ),
            &g,
        );
        add_storage(Detail, "left_text", attribute::Storage::Text(std::sync::Arc::new(vec![b"payload".to_vec()].into())), &g)
    }

    #[test]
    fn swap_copy_move_and_position_rules() {
        let source = swap_source();
        let left = find(Point, "left_weight", &source);
        let right = find(Point, "right_weight", &source);
        let swapped = attribute_ops::swap(
            None,
            &[swap_rule(Point, "left_*", "right_*", SwapMethod::AttributeSwap)],
            &source,
        )
        .unwrap();
        assert_eq!(attribute::storage_id(&find(Point, "left_weight", &swapped)), attribute::storage_id(&right));
        assert_eq!(attribute::storage_id(&find(Point, "right_weight", &swapped)), attribute::storage_id(&left));
        assert_eq!(geometry::point_count(&swapped), geometry::point_count(&source));

        let copied = attribute_ops::swap(
            None,
            &[
                swap_rule(Vertex, "left_*", "copied_*", SwapMethod::AttributeCopy),
                swap_rule(Primitive, "left_*", "copied_*", SwapMethod::AttributeCopy),
                swap_rule(Detail, "left_*", "copied_*", SwapMethod::AttributeCopy),
            ],
            &source,
        )
        .unwrap();
        for suffix in ["float", "int", "int_array", "float_array", "float2", "float3", "float4", "text"] {
            let from = find(Detail, &format!("left_{suffix}"), &copied);
            let to = find(Detail, &format!("copied_{suffix}"), &copied);
            assert_eq!(attribute::storage_id(&from), attribute::storage_id(&to), "{suffix}");
        }
        assert!(has(Vertex, "left_corner", &copied) && has(Vertex, "copied_corner", &copied));
        assert!(has(Primitive, "left_face", &copied) && has(Primitive, "copied_face", &copied));

        let moved = attribute_ops::swap(
            None,
            &[swap_rule(Point, "left_weight", "right_weight", SwapMethod::AttributeMove)],
            &source,
        )
        .unwrap();
        assert!(!has(Point, "left_weight", &moved));
        assert_eq!(attribute::storage_id(&find(Point, "right_weight", &moved)), attribute::storage_id(&left));
        assert_eq!(geometry::attributes(&moved).len(), geometry::attributes(&source).len() - 1);

        let missing_destination = attribute_ops::swap(
            None,
            &[swap_rule(Point, "left_weight", "new_weight", SwapMethod::AttributeSwap)],
            &source,
        )
        .unwrap();
        assert!(has(Point, "left_weight", &missing_destination));
        assert_eq!(attribute::storage_id(&find(Point, "new_weight", &missing_destination)), attribute::storage_id(&left));
        let missing_source = attribute_ops::swap(
            None,
            &[swap_rule(Point, "new_weight", "left_weight", SwapMethod::AttributeSwap)],
            &source,
        )
        .unwrap();
        assert!(has(Point, "left_weight", &missing_source));
        assert_eq!(attribute::storage_id(&find(Point, "new_weight", &missing_source)), attribute::storage_id(&left));

        let position_copy = attribute_ops::swap(
            None,
            &[swap_rule(Point, "P", "rest", SwapMethod::AttributeCopy)],
            &source,
        )
        .unwrap();
        match attribute::storage(&find(Point, "rest", &position_copy)).unwrap() {
            attribute::Storage::Float3(values) => {
                assert_eq!(packed::float3::data_id(&values), packed::float3::data_id(geometry::positions(&source)))
            }
            _ => panic!("Attribute Copy P emitted non-float3 storage"),
        }
        let alternate = packed::float3::of_owned(vec![9.0, 8.0], vec![7.0, 6.0], vec![5.0, 4.0]).unwrap();
        let with_rest = add_storage(Point, "rest", attribute::Storage::Float3(alternate), &source);
        let position_swapped = attribute_ops::swap(
            None,
            &[swap_rule(Point, "P", "rest", SwapMethod::AttributeSwap)],
            &with_rest,
        )
        .unwrap();
        assert_eq!(packed::float3::get(geometry::positions(&position_swapped), 0).unwrap(), (9.0, 7.0, 5.0));
        let (rx, ry, rz) = float3(Point, "rest", &position_swapped);
        assert_eq!((rx[0], ry[0], rz[0]), (1.0, 2.0, 3.0));
        let position_moved = attribute_ops::swap(
            None,
            &[swap_rule(Point, "rest", "P", SwapMethod::AttributeMove)],
            &with_rest,
        )
        .unwrap();
        assert!(!has(Point, "rest", &position_moved));
        assert_eq!(packed::float3::get(geometry::positions(&position_moved), 1).unwrap(), (8.0, 6.0, 4.0));
        let p_move = attribute_ops::swap(
            None,
            &[swap_rule(Point, "P", "rest", SwapMethod::AttributeMove)],
            &source,
        )
        .unwrap();
        assert!(has(Point, "rest", &p_move) && geometry::point_count(&p_move) == 2);

        let ordered = attribute_ops::swap(
            None,
            &[
                swap_rule(Point, "left_weight", "stage_weight", SwapMethod::AttributeMove),
                swap_rule(Point, "stage_*", "final_*", SwapMethod::AttributeCopy),
            ],
            &source,
        )
        .unwrap();
        assert!(!has(Point, "left_weight", &ordered));
        assert!(has(Point, "stage_weight", &ordered) && has(Point, "final_weight", &ordered));

        let none = attribute_ops::swap(None, &[], &source).unwrap();
        let absent = attribute_ops::swap(
            None,
            &[swap_rule(Detail, "absent", "also_absent", SwapMethod::AttributeCopy)],
            &source,
        )
        .unwrap();
        assert_eq!(geometry::data_id(&none), geometry::data_id(&source));
        assert_eq!(geometry::data_id(&absent), geometry::data_id(&source));
        let malformed = attribute_ops::swap(
            None,
            &[swap_rule(Point, "left_*", "fixed", SwapMethod::AttributeCopy)],
            &source,
        );
        assert_eq!(err_code(malformed), "invalid_attribute");
        let to_scalar = attribute_ops::swap(
            None,
            &[
                swap_rule(Point, "left_weight", "temporary_copy", SwapMethod::AttributeCopy),
                swap_rule(Point, "left_weight", "P", SwapMethod::AttributeCopy),
            ],
            &source,
        );
        assert_eq!(err_code(to_scalar), "invalid_attribute");
        assert!(!has(Point, "temporary_copy", &source));
        let token = cancel::create();
        cancel::cancel(&token);
        let cancelled = attribute_ops::swap(
            Some(&token),
            &[swap_rule(Detail, "left_*", "cancelled_*", SwapMethod::AttributeCopy)],
            &source,
        );
        assert_eq!(err_code(cancelled), "cancelled");
    }

    #[test]
    fn dense_rename_delete_and_swap_are_exact_across_domains() {
        let base = points(&[(0.0, 0.0, 0.0)]);
        let attributes: Vec<attribute::T> = (0..10_000_i64)
            .map(|index| {
                let prefix = if index & 1 == 0 { "temporary" } else { "keep" };
                attribute::create_owned(
                    &format!("{prefix}_{index:05}"),
                    Detail,
                    attribute::Storage::Float(Arc::new(vec![index as f64])),
                )
                .unwrap()
            })
            .collect();
        let dense = geometry::create(
            geometry::positions(&base).clone(),
            geometry::topology(&base).clone(),
            Some(attributes),
            None,
            None,
        )
        .unwrap();
        let run = |domains: i64| {
            on_domains(domains, &|| {
                let renamed = attribute_ops::rename(
                    None,
                    &[rename_rule(Some(Detail), "temporary_*", "final_*", RenameConflict::AttributeRenameError)],
                    &dense,
                )
                .unwrap();
                attribute_ops::delete(&delete_params(None, false, None, None, None, Some("keep_*")), &renamed).unwrap()
            })
        };
        let (one, many) = (run(1), run(4));
        assert_eq!(names(&one), names(&many));
        assert_eq!(geometry::attributes(&one).len(), 5_000);
        for value in geometry::attributes(&one) {
            let original = find(Detail, &format!("temporary_{}", &attribute::name(&value)[6..11]), &dense);
            assert_eq!(attribute::storage_id(&value), attribute::storage_id(&original));
        }
        let run_swap = |domains: i64| {
            on_domains(domains, &|| {
                attribute_ops::swap(
                    None,
                    &[swap_rule(Detail, "temporary_*", "copy_*", SwapMethod::AttributeCopy)],
                    &dense,
                )
                .unwrap()
            })
        };
        let (swap_one, swap_many) = (run_swap(1), run_swap(4));
        assert_eq!(names(&swap_one), names(&swap_many));
        assert_eq!(geometry::attributes(&swap_one).len(), 15_000);
        for value in geometry::attributes(&swap_one) {
            if attribute::name(&value).starts_with("copy_") {
                let source = find(Detail, &format!("temporary_{}", &attribute::name(&value)[5..10]), &dense);
                assert_eq!(attribute::storage_id(&value), attribute::storage_id(&source));
            }
        }
    }
}
