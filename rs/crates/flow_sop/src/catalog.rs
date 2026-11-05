//! Port of lib/flow_sop/catalog.ml: adapt native factories and embedded Lisp declarations to the
//! context-generic Flow checker/printer descriptor, and check workspaces with the embedded node
//! library in scope. The library is `lib/flow_sop/nodes/*.rays`, read in place with `include_str!`
//! and sorted by file name (as `gen/embed_forms.ml`); each form's id range is fixed by its place in
//! the whole library.
use flow_check::catalog::{self as flow_catalog, Catalog, Kind, Parameter, Slot};
use flow_check::defnode::{self, Defnode};
use flow_check::op::Op;
use flow_check::port_type::PortType;
use flow_check::workspace::{self, Prelude, Workspace};
use flow_syntax::context;
use flow_syntax::syntax::{self, Node, Syntax};
use flow_syntax::ty::{self, Ty};
use param::FieldView;
use rays_diagnostic::Diagnostic;
use sop::edit_graph::{self, Factory, InputRequirement};
use sop::node::Elementwise;
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;
use std::sync::OnceLock;

use crate::encoded_parameters;
use crate::port;

/// One catalog kind: a SOP factory, or a scene, world or settings kind that the editor document
/// generates from its own schemas. The namespace of `qualified` gives the context.
#[derive(Clone, Debug)]
pub struct Descriptor {
    pub qualified: String,
    pub key: String,
    pub operation: String,
    pub label: String,
    pub category: Vec<String>,
    pub slots: Vec<(String, InputRequirement)>,
    /// Optional serialized types in physical input order. Empty keeps the context defaults.
    pub slot_types: Vec<String>,
    /// Required fn/image inputs exposed as keyword parameters.
    pub keyword_inputs: Vec<String>,
    pub fields: Vec<FieldView>,
}

fn catalog_error(message: String) -> Diagnostic {
    port::error("E_CATALOG", &message)
}

/// The `sop/` kind of a factory.
pub fn descriptor(factory: &Factory) -> Descriptor {
    let slots = edit_graph::factory_slot_names(factory)
        .iter()
        .cloned()
        .zip(edit_graph::factory_inputs(factory))
        .collect();
    Descriptor {
        qualified: format!("sop/{}", edit_graph::factory_key(factory)),
        key: edit_graph::factory_key(factory).to_owned(),
        operation: edit_graph::factory_operation(factory).to_owned(),
        label: edit_graph::factory_label(factory).to_owned(),
        category: edit_graph::factory_category(factory).to_vec(),
        slots,
        slot_types: edit_graph::factory_input_types(factory).to_vec(),
        keyword_inputs: edit_graph::factory_keyword_inputs(factory).to_vec(),
        fields: edit_graph::factory_fields(factory).to_vec(),
    }
}

fn of_defnode(node: &Defnode) -> Descriptor {
    let key = match node.kind.qualified.find('/') {
        Some(i) => node.kind.qualified[i + 1..].to_owned(),
        None => node.kind.qualified.clone(),
    };
    Descriptor {
        qualified: node.kind.qualified.clone(),
        key,
        operation: node.operation.clone(),
        label: node.label.clone(),
        category: node.category.clone(),
        slots: node
            .slots
            .iter()
            .map(|slot| {
                let requirement = match (slot.required, slot.rest) {
                    (true, true) => InputRequirement::Rest,
                    (false, true) => InputRequirement::OptionalRest,
                    (true, false) => InputRequirement::Required,
                    (false, false) => InputRequirement::Optional,
                };
                (slot.name.clone(), requirement)
            })
            .collect(),
        slot_types: node
            .slots
            .iter()
            .map(|slot| slot.ty.as_ref().map_or("geometry".to_owned(), ty::to_string))
            .collect(),
        keyword_inputs: node.keyword_inputs.clone(),
        fields: node.fields.clone(),
    }
}

/// The library as a process holds it: each form with its fixed id base, the names it mentions, and
/// a defnode's declaration without its Lisp.
struct Entry {
    file: usize,
    form: usize,
    name: String,
    base: i64,
    references: Vec<String>,
    header: Option<Defnode>,
}

fn empty_form() -> Syntax {
    syntax::parse("()").expect("empty list parses").remove(0)
}

fn definition_name(form: &Syntax) -> Option<String> {
    let items = form.children();
    if !matches!(form.node, Node::List(_)) || items.len() < 2 {
        return None;
    }
    let head = form.child(items[0]);
    let name = form.child(items[1]);
    match (head.node, name.node) {
        (Node::Sym(h), Node::Sym(n)) if matches!(head.name(h), "defnode" | "defn" | "defmacro") => Some(name.name(n).to_owned()),
        _ => None,
    }
}

/// Names a form mentions, as a set.
fn mentions(into: &mut HashSet<String>, form: &Syntax) {
    if let Node::Sym(name) = &form.node {
        into.insert(form.name(*name).to_owned());
    }
    for &child in form.children() {
        mentions(into, &form.child(child));
    }
}

fn size(form: &Syntax) -> i64 {
    1 + form.children().iter().map(|&id| size(&form.child(id))).sum::<i64>()
}


const NODE_FILES: [(&str, &str); 129] = [
    ("attr_from_image.rays", include_str!("../../../../lib/flow_sop/nodes/attr_from_image.rays")),
    ("attribute_blur.rays", include_str!("../../../../lib/flow_sop/nodes/attribute_blur.rays")),
    ("attribute_composite.rays", include_str!("../../../../lib/flow_sop/nodes/attribute_composite.rays")),
    ("attribute_copy.rays", include_str!("../../../../lib/flow_sop/nodes/attribute_copy.rays")),
    ("attribute_fade.rays", include_str!("../../../../lib/flow_sop/nodes/attribute_fade.rays")),
    ("attribute_interpolate.rays", include_str!("../../../../lib/flow_sop/nodes/attribute_interpolate.rays")),
    ("attribute_laplacian.rays", include_str!("../../../../lib/flow_sop/nodes/attribute_laplacian.rays")),
    ("attribute_noise.rays", include_str!("../../../../lib/flow_sop/nodes/attribute_noise.rays")),
    ("attribute_noise_quaternion.rays", include_str!("../../../../lib/flow_sop/nodes/attribute_noise_quaternion.rays")),
    ("attribute_randomize.rays", include_str!("../../../../lib/flow_sop/nodes/attribute_randomize.rays")),
    ("attribute_remap.rays", include_str!("../../../../lib/flow_sop/nodes/attribute_remap.rays")),
    ("bend.rays", include_str!("../../../../lib/flow_sop/nodes/bend.rays")),
    ("blast.rays", include_str!("../../../../lib/flow_sop/nodes/blast.rays")),
    ("blast_by_attribute.rays", include_str!("../../../../lib/flow_sop/nodes/blast_by_attribute.rays")),
    ("blend_shapes.rays", include_str!("../../../../lib/flow_sop/nodes/blend_shapes.rays")),
    ("bound.rays", include_str!("../../../../lib/flow_sop/nodes/bound.rays")),
    ("box.rays", include_str!("../../../../lib/flow_sop/nodes/box.rays")),
    ("carve.rays", include_str!("../../../../lib/flow_sop/nodes/carve.rays")),
    ("circle.rays", include_str!("../../../../lib/flow_sop/nodes/circle.rays")),
    ("circle_from_edges.rays", include_str!("../../../../lib/flow_sop/nodes/circle_from_edges.rays")),
    ("color_by_height.rays", include_str!("../../../../lib/flow_sop/nodes/color_by_height.rays")),
    ("compact_points.rays", include_str!("../../../../lib/flow_sop/nodes/compact_points.rays")),
    ("connectivity.rays", include_str!("../../../../lib/flow_sop/nodes/connectivity.rays")),
    ("convert_line.rays", include_str!("../../../../lib/flow_sop/nodes/convert_line.rays")),
    ("copy_to_points.rays", include_str!("../../../../lib/flow_sop/nodes/copy_to_points.rays")),
    ("crease.rays", include_str!("../../../../lib/flow_sop/nodes/crease.rays")),
    ("delete_attributes.rays", include_str!("../../../../lib/flow_sop/nodes/delete_attributes.rays")),
    ("delete_edge_group.rays", include_str!("../../../../lib/flow_sop/nodes/delete_edge_group.rays")),
    ("distance_along_geometry.rays", include_str!("../../../../lib/flow_sop/nodes/distance_along_geometry.rays")),
    ("distance_from_target.rays", include_str!("../../../../lib/flow_sop/nodes/distance_from_target.rays")),
    ("duplicate.rays", include_str!("../../../../lib/flow_sop/nodes/duplicate.rays")),
    ("edge_cusp.rays", include_str!("../../../../lib/flow_sop/nodes/edge_cusp.rays")),
    ("edge_divide.rays", include_str!("../../../../lib/flow_sop/nodes/edge_divide.rays")),
    ("edge_equalize.rays", include_str!("../../../../lib/flow_sop/nodes/edge_equalize.rays")),
    ("edge_flip.rays", include_str!("../../../../lib/flow_sop/nodes/edge_flip.rays")),
    ("edge_relax.rays", include_str!("../../../../lib/flow_sop/nodes/edge_relax.rays")),
    ("edge_straighten.rays", include_str!("../../../../lib/flow_sop/nodes/edge_straighten.rays")),
    ("edge_transport.rays", include_str!("../../../../lib/flow_sop/nodes/edge_transport.rays")),
    ("edge_transport_curves.rays", include_str!("../../../../lib/flow_sop/nodes/edge_transport_curves.rays")),
    ("edge_transport_parent.rays", include_str!("../../../../lib/flow_sop/nodes/edge_transport_parent.rays")),
    ("ends.rays", include_str!("../../../../lib/flow_sop/nodes/ends.rays")),
    ("enumerate.rays", include_str!("../../../../lib/flow_sop/nodes/enumerate.rays")),
    ("extract_centroid.rays", include_str!("../../../../lib/flow_sop/nodes/extract_centroid.rays")),
    ("extract_point_from_curve.rays", include_str!("../../../../lib/flow_sop/nodes/extract_point_from_curve.rays")),
    ("graph_color.rays", include_str!("../../../../lib/flow_sop/nodes/graph_color.rays")),
    ("grid.rays", include_str!("../../../../lib/flow_sop/nodes/grid.rays")),
    ("group_backface.rays", include_str!("../../../../lib/flow_sop/nodes/group_backface.rays")),
    ("group_boundary_components.rays", include_str!("../../../../lib/flow_sop/nodes/group_boundary_components.rays")),
    ("group_bounds.rays", include_str!("../../../../lib/flow_sop/nodes/group_bounds.rays")),
    ("group_combine.rays", include_str!("../../../../lib/flow_sop/nodes/group_combine.rays")),
    ("group_copy.rays", include_str!("../../../../lib/flow_sop/nodes/group_copy.rays")),
    ("group_delete.rays", include_str!("../../../../lib/flow_sop/nodes/group_delete.rays")),
    ("group_edge_depth.rays", include_str!("../../../../lib/flow_sop/nodes/group_edge_depth.rays")),
    ("group_edges.rays", include_str!("../../../../lib/flow_sop/nodes/group_edges.rays")),
    ("group_expand.rays", include_str!("../../../../lib/flow_sop/nodes/group_expand.rays")),
    ("group_find_path.rays", include_str!("../../../../lib/flow_sop/nodes/group_find_path.rays")),
    ("group_from_attribute_boundary.rays", include_str!("../../../../lib/flow_sop/nodes/group_from_attribute_boundary.rays")),
    ("group_invert.rays", include_str!("../../../../lib/flow_sop/nodes/group_invert.rays")),
    ("group_non_planar.rays", include_str!("../../../../lib/flow_sop/nodes/group_non_planar.rays")),
    ("group_normal.rays", include_str!("../../../../lib/flow_sop/nodes/group_normal.rays")),
    ("group_promote_boundary.rays", include_str!("../../../../lib/flow_sop/nodes/group_promote_boundary.rays")),
    ("group_promotions.rays", include_str!("../../../../lib/flow_sop/nodes/group_promotions.rays")),
    ("group_random.rays", include_str!("../../../../lib/flow_sop/nodes/group_random.rays")),
    ("group_range.rays", include_str!("../../../../lib/flow_sop/nodes/group_range.rays")),
    ("group_ranges.rays", include_str!("../../../../lib/flow_sop/nodes/group_ranges.rays")),
    ("group_rename.rays", include_str!("../../../../lib/flow_sop/nodes/group_rename.rays")),
    ("group_unshared.rays", include_str!("../../../../lib/flow_sop/nodes/group_unshared.rays")),
    ("groups_from_name.rays", include_str!("../../../../lib/flow_sop/nodes/groups_from_name.rays")),
    ("iso_surface.rays", include_str!("../../../../lib/flow_sop/nodes/iso_surface.rays")),
    ("line.rays", include_str!("../../../../lib/flow_sop/nodes/line.rays")),
    ("match_axis.rays", include_str!("../../../../lib/flow_sop/nodes/match_axis.rays")),
    ("match_size.rays", include_str!("../../../../lib/flow_sop/nodes/match_size.rays")),
    ("material.rays", include_str!("../../../../lib/flow_sop/nodes/material.rays")),
    ("measure.rays", include_str!("../../../../lib/flow_sop/nodes/measure.rays")),
    ("measure_curvature.rays", include_str!("../../../../lib/flow_sop/nodes/measure_curvature.rays")),
    ("merge.rays", include_str!("../../../../lib/flow_sop/nodes/merge.rays")),
    ("mirror.rays", include_str!("../../../../lib/flow_sop/nodes/mirror.rays")),
    ("mountain.rays", include_str!("../../../../lib/flow_sop/nodes/mountain.rays")),
    ("name_from_groups.rays", include_str!("../../../../lib/flow_sop/nodes/name_from_groups.rays")),
    ("noise_displace.rays", include_str!("../../../../lib/flow_sop/nodes/noise_displace.rays")),
    ("normals.rays", include_str!("../../../../lib/flow_sop/nodes/normals.rays")),
    ("ordered_group.rays", include_str!("../../../../lib/flow_sop/nodes/ordered_group.rays")),
    ("peak.rays", include_str!("../../../../lib/flow_sop/nodes/peak.rays")),
    ("platonic.rays", include_str!("../../../../lib/flow_sop/nodes/platonic.rays")),
    ("point_generate.rays", include_str!("../../../../lib/flow_sop/nodes/point_generate.rays")),
    ("point_jitter.rays", include_str!("../../../../lib/flow_sop/nodes/point_jitter.rays")),
    ("point_replicate.rays", include_str!("../../../../lib/flow_sop/nodes/point_replicate.rays")),
    ("point_split.rays", include_str!("../../../../lib/flow_sop/nodes/point_split.rays")),
    ("point_velocity.rays", include_str!("../../../../lib/flow_sop/nodes/point_velocity.rays")),
    ("points.rays", include_str!("../../../../lib/flow_sop/nodes/points.rays")),
    ("poly_bridge.rays", include_str!("../../../../lib/flow_sop/nodes/poly_bridge.rays")),
    ("poly_loft.rays", include_str!("../../../../lib/flow_sop/nodes/poly_loft.rays")),
    ("poly_path.rays", include_str!("../../../../lib/flow_sop/nodes/poly_path.rays")),
    ("polyframe.rays", include_str!("../../../../lib/flow_sop/nodes/polyframe.rays")),
    ("polywire.rays", include_str!("../../../../lib/flow_sop/nodes/polywire.rays")),
    ("prelude.rays", include_str!("../../../../lib/flow_sop/nodes/prelude.rays")),
    ("promote_attributes.rays", include_str!("../../../../lib/flow_sop/nodes/promote_attributes.rays")),
    ("rename_attributes.rays", include_str!("../../../../lib/flow_sop/nodes/rename_attributes.rays")),
    ("rename_edge_group.rays", include_str!("../../../../lib/flow_sop/nodes/rename_edge_group.rays")),
    ("resample.rays", include_str!("../../../../lib/flow_sop/nodes/resample.rays")),
    ("rest_position.rays", include_str!("../../../../lib/flow_sop/nodes/rest_position.rays")),
    ("reverse.rays", include_str!("../../../../lib/flow_sop/nodes/reverse.rays")),
    ("revolve.rays", include_str!("../../../../lib/flow_sop/nodes/revolve.rays")),
    ("rewire_vertices.rays", include_str!("../../../../lib/flow_sop/nodes/rewire_vertices.rays")),
    ("scatter.rays", include_str!("../../../../lib/flow_sop/nodes/scatter.rays")),
    ("separate_pieces.rays", include_str!("../../../../lib/flow_sop/nodes/separate_pieces.rays")),
    ("set_color.rays", include_str!("../../../../lib/flow_sop/nodes/set_color.rays")),
    ("set_float.rays", include_str!("../../../../lib/flow_sop/nodes/set_float.rays")),
    ("set_int.rays", include_str!("../../../../lib/flow_sop/nodes/set_int.rays")),
    ("set_orient.rays", include_str!("../../../../lib/flow_sop/nodes/set_orient.rays")),
    ("set_transform.rays", include_str!("../../../../lib/flow_sop/nodes/set_transform.rays")),
    ("set_vector.rays", include_str!("../../../../lib/flow_sop/nodes/set_vector.rays")),
    ("skin.rays", include_str!("../../../../lib/flow_sop/nodes/skin.rays")),
    ("smooth.rays", include_str!("../../../../lib/flow_sop/nodes/smooth.rays")),
    ("snap_to_grid.rays", include_str!("../../../../lib/flow_sop/nodes/snap_to_grid.rays")),
    ("sort.rays", include_str!("../../../../lib/flow_sop/nodes/sort.rays")),
    ("spiral.rays", include_str!("../../../../lib/flow_sop/nodes/spiral.rays")),
    ("subdivide.rays", include_str!("../../../../lib/flow_sop/nodes/subdivide.rays")),
    ("swap_attributes.rays", include_str!("../../../../lib/flow_sop/nodes/swap_attributes.rays")),
    ("sweep.rays", include_str!("../../../../lib/flow_sop/nodes/sweep.rays")),
    ("sweep_circle.rays", include_str!("../../../../lib/flow_sop/nodes/sweep_circle.rays")),
    ("torus.rays", include_str!("../../../../lib/flow_sop/nodes/torus.rays")),
    ("transform.rays", include_str!("../../../../lib/flow_sop/nodes/transform.rays")),
    ("tube.rays", include_str!("../../../../lib/flow_sop/nodes/tube.rays")),
    ("uv_auto_seam.rays", include_str!("../../../../lib/flow_sop/nodes/uv_auto_seam.rays")),
    ("uv_project.rays", include_str!("../../../../lib/flow_sop/nodes/uv_project.rays")),
    ("uv_sphere.rays", include_str!("../../../../lib/flow_sop/nodes/uv_sphere.rays")),
    ("uv_transform.rays", include_str!("../../../../lib/flow_sop/nodes/uv_transform.rays")),
    ("uv_unitize.rays", include_str!("../../../../lib/flow_sop/nodes/uv_unitize.rays")),
];

struct Library {
    entries: Vec<Entry>,
    forms: OnceLock<Vec<Syntax>>,
    nodes: OnceLock<Result<Vec<Defnode>, Diagnostic>>,
}

fn parse_file(index: usize) -> Result<Vec<Syntax>, Diagnostic> {
    let (file, text) = NODE_FILES[index];
    let mut forms: Vec<Syntax> = Vec::new();
    let parsed = syntax::parse(text).map_err(|mut d| {
        d.message = format!("{file}: {}", d.message);
        d
    })?;
    for form in parsed {
        let children = if syntax::head(&form) == Some("workspace") && form.children().len() >= 2 {
            Some(form.children()[2..].iter().map(|&id| form.child(id)).collect::<Vec<_>>())
        } else {
            None
        };
        match children {
            Some(children) => forms.extend(children),
            None => forms.push(form),
        }
    }
    Ok(forms)
}

fn load_library() -> Result<Library, Diagnostic> {
    // Validate every source and retain its header/index, not every unused implementation body.
    let empty = empty_form();
    let mut entries = Vec::new();
    let mut next = LIBRARY_BASE;
    for file in 0..NODE_FILES.len() {
        for (form_index, form) in parse_file(file)?.iter().enumerate() {
            let Some(name) = definition_name(form) else {
                return Err(catalog_error("node sources contain only defnode, defn and defmacro forms".into()));
            };
            let header = if syntax::head(form) != Some("defnode") {
                None
            } else {
                let mut node = defnode::parse(form)?;
                node.inputs = Vec::new();
                node.body = empty.clone();
                node.validations = Vec::new();
                node.normalizers = Vec::new();
                node.form = empty.clone();
                Some(node)
            };
            let mut names = HashSet::new();
            mentions(&mut names, form);
            let mut references: Vec<String> = names.into_iter().collect();
            references.sort();
            let base = next;
            next += size(form);
            entries.push(Entry { file, form: form_index, name, base, references, header });
        }
    }
    Ok(Library { entries, forms: OnceLock::new(), nodes: OnceLock::new() })
}

fn library() -> Result<&'static Library, Diagnostic> {
    static LIBRARY: OnceLock<Result<Library, Diagnostic>> = OnceLock::new();
    static OWNER: OnceLock<mem::Owner> = OnceLock::new();
    let library = LIBRARY.get_or_init(load_library).as_ref().map_err(|d| d.clone())?;
    OWNER.get_or_init(|| {
        let owner = mem::cache("flow.library.syntax");
        mem::root(&owner, std::sync::Arc::new(move |visitor| {
            let mut memory = syntax::Memory::default();
            if let Some(forms) = library.forms.get() { memory.forms(forms); }
            for (address, bytes) in memory.into_allocations() { visitor.allocation(address, bytes); }
        }));
        owner
    });
    Ok(library)
}

/// The whole library, for what reads all of it (the manifest, tests). A workspace check does not.
/// Embedded node-library definitions, sorted by source filename.
pub fn sources() -> Result<&'static [Syntax], Diagnostic> {
    Ok(library()?.forms.get_or_init(|| (0..NODE_FILES.len())
        .flat_map(|file| parse_file(file).expect("embedded sources were validated when indexing the library"))
        .collect()))
}

/// `Catalog.definitions_of_sources`: the defnode forms among `forms`, parsed.
pub fn definitions_of_sources(forms: &[Syntax]) -> Result<Vec<Defnode>, Diagnostic> {
    let mut nodes = Vec::new();
    for form in forms {
        if syntax::head(form) == Some("defnode") {
            nodes.push(defnode::parse(form)?);
        }
    }
    Ok(nodes)
}

/// The embedded library never changes; custom source lists remain caller-owned and uncached.
pub fn node_definitions() -> Result<&'static [Defnode], Diagnostic> {
    let library = library()?;
    library
        .nodes
        .get_or_init(|| definitions_of_sources(sources()?))
        .as_ref()
        .map(|nodes| nodes.as_slice())
        .map_err(|d| d.clone())
}

/// What a catalog needs of a defnode: its declaration, with no Lisp behind it.
fn node_headers() -> Vec<Defnode> {
    match library() {
        Ok(library) => library.entries.iter().filter_map(|e| e.header.clone()).collect(),
        Err(_) => Vec::new(),
    }
}

fn library_descriptors() -> &'static Vec<Descriptor> {
    static DESCRIPTORS: OnceLock<Vec<Descriptor>> = OnceLock::new();
    DESCRIPTORS.get_or_init(|| node_headers().iter().map(of_defnode).collect())
}

/// `present` then every embedded library node it lacks, whether or not a workspace uses it: what
/// the editor's node menu and completion offer (a workspace lowers only the library definitions
/// it reaches).
pub fn with_library(present: Vec<Descriptor>) -> Vec<Descriptor> {
    let missing: Vec<Descriptor> = library_descriptors()
        .iter()
        .filter(|d| !present.iter().any(|p| p.qualified == d.qualified))
        .cloned()
        .collect();
    let mut all = present;
    all.extend(missing);
    all
}

/// Grow `keep` to the sources reachable from `queue` through `visit`ed names; `by_name` maps a
/// definition name to the indices that bind it, `references` gives what an index mentions.
fn close_over(
    by_name: &HashMap<String, Vec<usize>>,
    keep: &mut [bool],
    seen: &mut HashSet<String>,
    queue: &mut VecDeque<String>,
    references: &mut dyn FnMut(usize) -> Vec<String>,
) {
    while let Some(name) = queue.pop_front() {
        if let Some(indices) = by_name.get(&name) {
            for &index in indices {
                if !keep[index] {
                    keep[index] = true;
                    for reference in references(index) {
                        if seen.insert(reference.clone()) {
                            queue.push_back(reference);
                        }
                    }
                }
            }
        }
    }
}

fn seed(forms: &[Syntax], seen: &mut HashSet<String>, queue: &mut VecDeque<String>) {
    for form in forms {
        let mut names = HashSet::new();
        mentions(&mut names, form);
        for name in names {
            if seen.insert(name.clone()) {
                queue.push_back(name);
            }
        }
    }
}

/// The entries a workspace reaches through the names it mentions, in library order.
fn reachable_entries(forms: &[Syntax], allowed: &dyn Fn(&Entry) -> bool) -> Vec<usize> {
    let Ok(library) = library() else { return Vec::new() };
    let mut by_name: HashMap<String, Vec<usize>> = HashMap::new();
    for (i, entry) in library.entries.iter().enumerate() {
        if allowed(entry) {
            by_name.entry(entry.name.clone()).or_default().push(i);
        }
    }
    let (mut seen, mut queue) = (HashSet::new(), VecDeque::new());
    seed(forms, &mut seen, &mut queue);
    let mut keep = vec![false; library.entries.len()];
    close_over(&by_name, &mut keep, &mut seen, &mut queue, &mut |i| library.entries[i].references.clone());
    (0..keep.len()).filter(|i| keep[*i]).collect()
}

/// The library definitions reachable from `forms` through the names they mention, in library
/// order, together with every non-definition source. Syntactic and conservative: a name anywhere
/// in a form counts, so shadowing and duplicate-name errors are still reported. Returns indices.
fn reachable_indices(sources: &[Syntax], forms: &[Syntax]) -> Vec<usize> {
    let mut by_name: HashMap<String, Vec<usize>> = HashMap::new();
    for (i, form) in sources.iter().enumerate() {
        if let Some(name) = definition_name(form) {
            by_name.entry(name).or_default().push(i);
        }
    }
    let (mut seen, mut queue) = (HashSet::new(), VecDeque::new());
    seed(forms, &mut seen, &mut queue);
    let mut keep = vec![false; sources.len()];
    close_over(&by_name, &mut keep, &mut seen, &mut queue, &mut |i| {
        let mut names = HashSet::new();
        mentions(&mut names, &sources[i]);
        names.into_iter().collect()
    });
    (0..sources.len()).filter(|i| definition_name(&sources[*i]).is_none() || keep[*i]).collect()
}

fn reachable(sources: &[Syntax], forms: &[Syntax]) -> Vec<Syntax> {
    reachable_indices(sources, forms).into_iter().map(|i| sources[i].clone()).collect()
}

/// Each library form has a fixed id, from its position in the whole library above any authored
/// id: a library definition checks to the same form ids whatever the authored text or the
/// reachable subset, so a kept evaluation of its body finds its terms again.
fn library_bases(sources: &[Syntax]) -> Vec<(Syntax, i64)> {
    let mut next = LIBRARY_BASE;
    sources
        .iter()
        .map(|source| {
            let base = next;
            next += size(source);
            (source.clone(), base)
        })
        .collect()
}


/// The first form id of the library forms a check splices in: each library form has a fixed id
/// from its position in the whole library, above any authored id.
pub const LIBRARY_BASE: i64 = syntax::LIBRARY_BASE;

pub fn library_base() -> i64 {
    LIBRARY_BASE
}

fn is_embedded(supplied: &[Syntax]) -> bool {
    library().ok().and_then(|library| library.forms.get()).is_some_and(|embedded|
        std::ptr::eq(supplied.as_ptr(), embedded.as_ptr()) && supplied.len() == embedded.len())
}

type PreludeKey = (Vec<String>, bool, Catalog, Vec<i64>);

thread_local! {
    // ponytail: per-thread cache of eight preludes, most recent first (OCaml kept one global list).
    static PRELUDES: RefCell<Vec<(PreludeKey, Rc<(Catalog, Prelude)>)>> = const { RefCell::new(Vec::new()) };
    static PRELUDE_CHECKS: Cell<usize> = const { Cell::new(0) };
}

fn op_names(ops: &[Op]) -> Vec<String> {
    ops.iter().map(|op| op.name.clone()).collect()
}

/// The embedded library is fixed, so its checked definitions are kept between workspace checks,
/// keyed on the reachable subset (by fixed position), the catalog (by value: hosts rebuild it),
/// the operators and the library flag. Eight subsets, most recent first.
fn prelude_cached(
    ops: &[Op],
    library: bool,
    catalog: &Catalog,
    positions: &[i64],
    make: &dyn Fn() -> (Catalog, Prelude),
) -> Rc<(Catalog, Prelude)> {
    static OWNER: OnceLock<mem::Owner> = OnceLock::new();
    OWNER.get_or_init(|| {
        let owner = mem::cache("flow.library.preludes");
        mem::root(&owner, std::sync::Arc::new(|visitor| {
            // Workspace checking and memory reports run on the initial domain.
            let mut memory = workspace::Memory::default();
            PRELUDES.with(|cache| {
                for (_, prelude) in cache.borrow().iter() { memory.prelude(&prelude.1); }
            });
            let (syntax, checked) = memory.into_allocations();
            for (address, bytes) in syntax.into_iter().chain(checked) { visitor.allocation(address, bytes); }
        }));
        owner
    });
    let key: PreludeKey = (op_names(ops), library, catalog.clone(), positions.to_vec());
    let hit = PRELUDES.with(|cache| {
        let mut cache = cache.borrow_mut();
        let found = cache.iter().position(|(k, _)| *k == key);
        found.map(|i| {
            let entry = cache.remove(i);
            let value = entry.1.clone();
            cache.insert(0, entry);
            value
        })
    });
    if let Some(value) = hit {
        return value;
    }
    PRELUDE_CHECKS.with(|n| n.set(n.get() + 1));
    let value = Rc::new(make());
    PRELUDES.with(|cache| {
        let mut cache = cache.borrow_mut();
        cache.insert(0, (key, value.clone()));
        cache.truncate(8);
    });
    value
}

/// Introspection for tests (the OCaml `Catalog.Private`).
pub mod private {
    use super::*;

    pub fn reachable_sources(sources: &[Syntax], forms: &[Syntax]) -> Vec<Syntax> {
        reachable(sources, forms)
    }

    pub fn definitions_of_sources(forms: &[Syntax]) -> Result<Vec<Defnode>, Diagnostic> {
        super::definitions_of_sources(forms)
    }

    /// Each source's fixed first form id, in the whole library given.
    pub fn library_bases(sources: &[Syntax]) -> Vec<(Syntax, i64)> {
        super::library_bases(sources)
    }

    /// How many times the embedded library's reachable definitions were checked on this thread
    /// (kept checks excluded).
    pub fn library_checks() -> usize {
        PRELUDE_CHECKS.with(|n| n.get())
    }
}

fn without_kinds(catalog: &Catalog, drop: &dyn Fn(&Kind) -> bool) -> Catalog {
    Catalog { version: catalog.version, kinds: catalog.kinds.iter().filter(|k| !drop(k)).cloned().collect() }
}

fn check_embedded(ops: &[Op], library_flag: bool, catalog: &Catalog, forms: &[Syntax]) -> (Option<Workspace>, Vec<Diagnostic>) {
    let Ok(library) = library() else {
        return workspace::check(ops, library_flag, false, None, catalog, forms);
    };
    let in_catalog = |name: &str| catalog.kinds.iter().any(|kind| kind.qualified == name);
    let any_node = library.entries.iter().any(|e| e.header.is_some() && in_catalog(&e.name));
    let reached = if any_node {
        reachable_entries(forms, &|e| e.header.is_none() || in_catalog(&e.name))
    } else {
        Vec::new()
    };
    if reached.is_empty() {
        return workspace::check(ops, library_flag, false, None, catalog, forms);
    }
    let positions: Vec<i64> = reached.iter().map(|i| library.entries[*i].base).collect();
    let make = || {
        let catalog = without_kinds(catalog, &|kind| {
            reached.iter().any(|i| {
                let e = &library.entries[*i];
                e.header.is_some() && e.name == kind.qualified
            })
        });
        let mut files = HashMap::new();
        let sources: Vec<Syntax> = reached.iter().map(|i| {
            let entry = &library.entries[*i];
            let forms = files.entry(entry.file).or_insert_with(|| parse_file(entry.file)
                .expect("embedded sources were validated when indexing the library"));
            syntax::renumber(entry.base, &forms[entry.form]).0
        }).collect();
        let prelude = workspace::prelude(ops, library_flag, false, &catalog, &sources);
        (catalog, prelude)
    };
    let cached = prelude_cached(ops, library_flag, catalog, &positions, &make);
    workspace::check(ops, library_flag, false, Some(&cached.1), &cached.0, forms)
}

fn check_workspace_raw(
    ops: &[Op],
    library_flag: bool,
    override_sources: Option<&[Syntax]>,
    catalog: &Catalog,
    forms: &[Syntax],
) -> (Option<Workspace>, Vec<Diagnostic>) {
    let Some(supplied) = override_sources else {
        return check_embedded(ops, library_flag, catalog, forms);
    };
    let nodes: Result<Vec<Defnode>, Diagnostic> = if is_embedded(supplied) {
        node_definitions().map(|nodes| nodes.to_vec())
    } else {
        definitions_of_sources(supplied)
    };
    let nodes = match nodes {
        Ok(nodes) => nodes,
        Err(diagnostic) => return (None, vec![diagnostic]),
    };
    let bases = library_bases(supplied);
    let (kept, nodes): (Vec<usize>, Vec<Defnode>) = if library_flag {
        ((0..supplied.len()).collect(), nodes)
    } else {
        // An explicit library is pruned the same way: checking all of it per workspace made every
        // test case pay for every Lisp node.
        let kept = reachable_indices(supplied, forms);
        let nodes = nodes
            .into_iter()
            .filter(|node| kept.iter().any(|i| definition_name(&supplied[*i]).as_deref() == Some(node.kind.qualified.as_str())))
            .collect();
        (kept, nodes)
    };
    if kept.is_empty() {
        return workspace::check(ops, library_flag, false, None, catalog, forms);
    }
    let positions: Vec<i64> = kept.iter().map(|i| bases[*i].1).collect();
    let make = || {
        let catalog = without_kinds(catalog, &|kind| nodes.iter().any(|node| node.kind.qualified == kind.qualified));
        let sources: Vec<Syntax> =
            kept.iter().zip(&positions).map(|(i, base)| syntax::renumber(*base, &supplied[*i]).0).collect();
        let prelude = workspace::prelude(ops, library_flag, false, &catalog, &sources);
        (catalog, prelude)
    };
    if is_embedded(supplied) {
        let cached = prelude_cached(ops, library_flag, catalog, &positions, &make);
        workspace::check(ops, library_flag, false, Some(&cached.1), &cached.0, forms)
    } else {
        let (catalog, prelude) = make();
        workspace::check(ops, library_flag, false, Some(&prelude), &catalog, forms)
    }
}

/// Check with the embedded definitions registered in `catalog` in scope. Authored form IDs stay
/// intact; library forms receive a disjoint ID space and stay out of `Workspace.source`.
/// `sources` overrides the embedded definitions for callers providing their own node library,
/// regardless of catalog membership.
pub fn check_workspace(
    ops: &[Op],
    library: bool,
    sources: Option<&[Syntax]>,
    catalog: &Catalog,
    forms: &[Syntax],
) -> (Option<Workspace>, Vec<Diagnostic>) {
    match check_workspace_raw(ops, library, sources, catalog, forms) {
        (None, diagnostics) => (None, diagnostics),
        (Some(checked), mut diagnostics) => match encoded_parameters::checked(checked) {
            Ok(checked) => (Some(checked), diagnostics),
            Err(diagnostic) => {
                diagnostics.push(diagnostic);
                (None, diagnostics)
            }
        },
    }
}

/// The catalog describes a kind at its default parameters: the `:facts` names that read a
/// parameter (`$param`, `$param|fallback`, `$param~prefix`) resolve against the defaults, as the
/// manifest prints them.
fn described_kind(node: &Defnode) -> Kind {
    let lookup = |name: &str| -> Option<String> {
        node.fields.iter().find_map(|field| match &field.default {
            param::Value::Text(text) if field.name == name => Some(String::from_utf8_lossy(text).into_owned()),
            _ => None,
        })
    };
    let mut kind = node.kind.clone();
    kind.facts = node.kind.facts.as_ref().map(|f| flow_catalog::KernelFacts {
        reads: flow_catalog::resolve_component_names(&lookup, &f.reads),
        writes: flow_catalog::resolve_component_names(&lookup, &f.writes),
        ..f.clone()
    });
    kind
}

fn required(requirement: InputRequirement) -> bool {
    matches!(requirement, InputRequirement::Required | InputRequirement::Rest)
}

fn repeats(requirement: InputRequirement) -> bool {
    matches!(requirement, InputRequirement::Rest | InputRequirement::OptionalRest)
}

fn keyword_error(entry: &Descriptor) -> Option<Diagnostic> {
    let mut sorted = entry.keyword_inputs.clone();
    sorted.sort();
    sorted.dedup();
    let bad = sorted.len() != entry.keyword_inputs.len()
        || entry.keyword_inputs.iter().any(|name| match entry.slots.iter().position(|(slot, _)| slot == name) {
            None => true,
            Some(index) => {
                entry.slots[index].1 != InputRequirement::Required
                    || match entry.slot_types.get(index).and_then(|t| ty::of_string(t)) {
                        Some(Ty::Fn(Some(_))) => false,
                        Some(t) if t == ty::image() => false,
                        _ => true,
                    }
            }
        });
    bad.then(|| catalog_error(format!("Keyword inputs require distinct required fn/image slots in {}", entry.qualified)))
}

fn kind_of_descriptor(entry: &Descriptor, facts: Option<flow_catalog::KernelFacts>) -> Result<Kind, Diagnostic> {
    let types: Vec<Option<Ty>> = if entry.slot_types.is_empty() {
        entry.slots.iter().map(|_| None).collect()
    } else {
        entry.slot_types.iter().map(|name| if name == "geometry" { None } else { ty::of_string(name) }).collect()
    };
    let inputs: Vec<Slot> = entry
        .slots
        .iter()
        .zip(types)
        .map(|((name, requirement), ty)| Slot {
            name: name.clone(),
            required: required(*requirement),
            rest: repeats(*requirement),
            ty,
        })
        .collect();
    let (keyword_slots, slots): (Vec<Slot>, Vec<Slot>) =
        inputs.into_iter().partition(|s| entry.keyword_inputs.contains(&s.name));
    let mut keywords = Vec::new();
    for s in keyword_slots {
        let ty = match &s.ty {
            Some(Ty::Fn(Some(signature))) => PortType::Fn((**signature).clone()),
            Some(t) if *t == ty::image() => PortType::Image,
            _ => return Err(catalog_error(format!("Typed keyword input must be fn or image: {}", s.name))),
        };
        keywords.push(Parameter {
            name: s.name.clone(),
            label: s.name.clone(),
            ty: Some(ty),
            fields: Vec::new(),
            folder: Vec::new(),
            primary: true,
            unit: None,
        });
    }
    let ports = port::parameters(false, &entry.fields)?;
    if ports.iter().any(|p| entry.keyword_inputs.contains(&p.path)) {
        return Err(catalog_error(format!("Keyword input clashes with a scalar parameter in {}", entry.qualified)));
    }
    let mut parameters: Vec<Parameter> = ports
        .iter()
        .map(|port| {
            let first = port.fields.first();
            Parameter {
                name: port.path.clone(),
                label: first.map_or(port.path.clone(), |f| f.label.clone()),
                ty: port.ty.clone(),
                fields: port.fields.iter().map(|f| (f.name.clone(), f.kind.clone(), f.default.clone())).collect(),
                folder: first.map_or(Vec::new(), |f| f.folder.clone()),
                primary: first.is_some_and(|f| f.primary),
                unit: match port.fields.as_slice() {
                    [f] => f.unit.clone(),
                    _ => None,
                },
            }
        })
        .collect();
    parameters.extend(keywords);
    let context = context::of_qualified(&entry.qualified).expect("checked by the caller");
    let outputs = if context == context::sop() { vec![("geo".to_owned(), PortType::Geometry)] } else { Vec::new() };
    Ok(Kind { qualified: entry.qualified.clone(), context, slots, parameters, outputs, facts })
}

pub fn of_factories(
    version: i64,
    extra: &[Descriptor],
    nodes: Option<&[Defnode]>,
    factories: &[Factory],
) -> Result<Catalog, Diagnostic> {
    let mut declarations: HashMap<String, flow_catalog::KernelFacts> = HashMap::new();
    for factory in factories {
        let facts = edit_graph::factory_facts(factory);
        declarations.insert(
            format!("sop/{}", edit_graph::factory_key(factory)),
            flow_catalog::KernelFacts {
                elementwise: match facts.elementwise {
                    Elementwise::Points => flow_catalog::KernelElements::Points,
                    Elementwise::Primitives => flow_catalog::KernelElements::Primitives,
                    Elementwise::None => flow_catalog::KernelElements::Irregular,
                },
                reads: facts.reads.clone(),
                writes: facts.writes.clone(),
                preserves_topology: facts.topology == sop::node::Topology::Preserved,
                exact: facts.exact,
            },
        );
    }
    let entries: Vec<Descriptor> = factories.iter().map(descriptor).chain(extra.iter().cloned()).collect();
    let mut seen: HashSet<String> = HashSet::new();
    let mut kinds: Vec<Kind> = Vec::new();
    for entry in &entries {
        if !entry.slot_types.is_empty() && entry.slot_types.len() != entry.slots.len() {
            return Err(catalog_error(format!("Input type count differs in {}", entry.qualified)));
        }
        if !entry
            .slot_types
            .iter()
            .all(|name| matches!(ty::of_string(name), Some(Ty::Named(_)) | Some(Ty::Fn(Some(_)))))
        {
            return Err(catalog_error(format!("Unknown input type in {}", entry.qualified)));
        }
        if let Some(diagnostic) = keyword_error(entry) {
            return Err(diagnostic);
        }
        if context::of_qualified(&entry.qualified).is_none() {
            return Err(catalog_error(format!("Unknown context prefix in {}", entry.qualified)));
        }
        if seen.contains(&entry.qualified) {
            return Err(catalog_error(format!("Duplicate kind {}", entry.qualified)));
        }
        seen.insert(entry.qualified.clone());
        kinds.push(kind_of_descriptor(entry, declarations.get(&entry.qualified).cloned())?);
    }
    let catalog = Catalog { version, kinds };
    let nodes: Vec<Defnode> = match nodes {
        Some(nodes) => nodes.to_vec(),
        None => node_headers(),
    };
    let mut checked = catalog.kinds.clone();
    for node in &nodes {
        if checked.iter().any(|kind| kind.qualified == node.kind.qualified) {
            return Err(catalog_error(format!("Duplicate kind {}", node.kind.qualified)));
        }
        checked.push(described_kind(node));
    }
    let (native, extra_kinds): (Vec<Kind>, Vec<Kind>) =
        catalog.kinds.into_iter().partition(|kind| declarations.contains_key(&kind.qualified));
    let mut kinds = native;
    kinds.extend(nodes.iter().map(described_kind));
    kinds.extend(extra_kinds);
    Ok(Catalog { version, kinds })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn nodes_dir() -> String {
        option_env!("RAYS_NODES_DIR")
            .unwrap_or(concat!(env!("CARGO_MANIFEST_DIR"), "/../../../lib/flow_sop/nodes"))
            .to_owned()
    }

    #[test]
    fn retained_prelude_arenas_are_charged_once() {
        let catalog = Catalog { version: 0, kinds: Vec::new() };
        let forms = syntax::parse("(defn identity :context value [(x : int)] x)").unwrap();
        let cached = prelude_cached(&[], false, &catalog, &[1], &|| {
            (catalog.clone(), workspace::prelude(&[], false, false, &catalog, &forms))
        });
        let mut memory = workspace::Memory::default();
        memory.prelude(&cached.1);
        let expected = memory.checked_bytes() + memory.syntax.bytes();
        assert!(expected > 0);
        memory.prelude(&cached.1);
        assert_eq!(memory.checked_bytes() + memory.syntax.bytes(), expected);
        for _ in 0..2 {
            let report = mem::report();
            let owner = report.owners.iter().find(|owner| owner.name == "flow.library.preludes").unwrap();
            assert_eq!(owner.bytes, expected as i64);
        }
    }

    #[test]
    fn retained_library_syntax_is_charged_once_across_roots() {
        let library = library().expect("library loads");
        sources().expect("materialize the full library explicitly");
        let forms = library.forms.get().unwrap();
        let mut memory = syntax::Memory::default();
        memory.forms(forms);
        let expected = memory.bytes() as i64;
        assert!(expected > 0);
        let alias = mem::cache("test.library.alias");
        mem::root(&alias, Arc::new(move |visitor| {
            let mut memory = syntax::Memory::default();
            memory.forms(forms);
            for (address, bytes) in memory.into_allocations() { visitor.allocation(address, bytes); }
        }));
        for _ in 0..2 {
            let report = mem::report();
            let owner = report.owners.iter().find(|owner| owner.name == "flow.library.syntax").expect("library owner");
            assert_eq!(owner.bytes, expected);
            assert_eq!(mem::bytes(&alias), 0, "another root cannot charge the same source arenas twice");
        }
    }

    #[test]
    fn embedded_files_are_the_whole_node_directory_sorted() {
        let mut on_disk: Vec<String> = std::fs::read_dir(nodes_dir())
            .expect("nodes directory")
            .map(|entry| entry.expect("entry").file_name().to_string_lossy().into_owned())
            .collect();
        on_disk.sort();
        let listed: Vec<String> = NODE_FILES.iter().map(|(name, _)| (*name).to_owned()).collect();
        assert_eq!(listed, on_disk);
    }

    #[test]
    fn the_library_parses_and_its_ids_are_fixed_by_position() {
        let library = library().expect("library loads");
        assert!(library.entries.len() > 200, "{} forms", library.entries.len());
        assert_eq!(library.entries[0].base, LIBRARY_BASE);
        for pair in library.entries.windows(2) {
            assert!(pair[1].base > pair[0].base);
        }
        let nodes = library.entries.iter().filter(|e| e.header.is_some()).count();
        assert_eq!(nodes, NODE_FILES.iter().map(|(_, text)| text.matches("(defnode ").count()).sum::<usize>());
        let again = node_definitions().expect("definitions parse");
        assert_eq!(again.len(), nodes);
        assert!(std::ptr::eq(sources().expect("sources").as_ptr(), library.forms.get().unwrap().as_ptr()));
    }

    #[test]
    fn normal_checks_do_not_retain_the_complete_library_syntax() {
        let catalog = of_factories(1, &[], None, &sop::nodes::factories()).expect("catalog");
        let library = library().expect("library");
        assert!(library.forms.get().is_none());
        let forms = syntax::parse("(workspace w (graph g :context sop (sop/box)))").expect("parses");
        let (checked, diagnostics) = check_workspace(crate::operators::all(), false, None, &catalog, &forms);
        assert!(checked.is_some(), "{diagnostics:?}");
        assert!(!checked.unwrap().nodes.is_empty(), "the reached implementation is present");
        assert!(library.forms.get().is_none(), "unused implementations remain unmaterialized");
    }

    /// test_catalog_load.ml: a catalog without native kinds still lists every library node, and a
    /// small draw workspace checks against it.
    #[test]
    fn small_workspace_checks_against_the_library_catalog() {
        let catalog = of_factories(1, &[], None, &[]).expect("catalog");
        assert!(catalog.kinds.len() > 100);
        let forms = syntax::parse("(workspace w (graph g :context draw (draw/background \"#000000\")))").expect("parses");
        let (checked, diagnostics) = check_workspace(&[], false, None, &catalog, &forms);
        assert!(checked.is_some(), "{diagnostics:?}");
    }

    #[test]
    fn with_library_appends_only_missing_nodes() {
        let all = with_library(Vec::new());
        assert!(!all.is_empty());
        let again = with_library(all.clone());
        assert_eq!(again.len(), all.len());
    }

    fn field_factory() -> Factory {
        let mut args = edit_graph::factory_args("test_field", "Test field", &["Test"]);
        args.slots = Some(vec!["field".to_owned(), "picture".to_owned()]);
        args.keyword_inputs = vec!["field".to_owned(), "picture".to_owned()];
        args.input_types = Some(vec!["fn(vec3,float)->float".to_owned(), "image".to_owned()]);
        edit_graph::factory(
            args,
            2,
            Arc::new(|inputs| {
                Ok(sop::node::build::make(
                    sop::node::build::Args { inputs: inputs.to_vec(), ..sop::node::build::args("test_field") },
                    None,
                    Arc::new(|_, _, _| Err(crate::port::error("E_TEST", "never cooked"))),
                ))
            }),
        )
    }

    /// test_keyword_ports.ml, catalog side: typed keyword inputs become Fn/Image parameters, not
    /// slots, and calls check against them. (The manifest round trip, the graph projection and edit
    /// gestures and the lowered cooks of that file are in the slow golden and the lower tests.)
    #[test]
    fn keyword_inputs_become_typed_parameters() {
        let field = field_factory();
        assert_eq!(edit_graph::factory_keyword_inputs(&field), ["field", "picture"]);
        assert_eq!(edit_graph::factory_input_types(&field), ["fn(vec3,float)->float", "image"]);
        let mut factories = vec![field];
        factories.extend(sop::nodes::factories());
        let catalog = of_factories(crate::manifest::VERSION, &[], None, &factories).expect("catalog");
        let kind = &catalog.kinds[0];
        assert!(kind.slots.is_empty());
        assert_eq!(kind.parameters.iter().map(|p| p.name.as_str()).collect::<Vec<_>>(), ["field", "picture"]);
        assert!(matches!(kind.parameters[0].ty, Some(PortType::Fn(_))));
        assert_eq!(kind.parameters[1].ty, Some(PortType::Image));
        let checked = |args: &str| {
            let text = format!("(workspace w (graph g :context sop (sop/test_field {args})))");
            check_workspace(crate::operators::all(), false, None, &catalog, &syntax::parse(&text).expect("parses")).0.is_some()
        };
        assert!(checked(":field (fn [p amount] (+ p.x amount)) :picture (image/noise :width 2 :height 2)"));
        assert!(!checked(":field (fn [p] p.x) :picture (image/noise)"));
        assert!(!checked(":picture (image/noise)"));
        assert!(!checked(":field (fn [p amount] (+ p.x amount))"));
    }

    #[test]
    fn catalog_rejects_bad_descriptors() {
        let ok = Descriptor {
            qualified: "sop/test_ok".into(),
            key: "test_ok".into(),
            operation: "test_ok".into(),
            label: "Ok".into(),
            category: vec!["Test".into()],
            slots: vec![("field".into(), InputRequirement::Required)],
            slot_types: Vec::new(),
            keyword_inputs: Vec::new(),
            fields: Vec::new(),
        };
        assert!(of_factories(1, std::slice::from_ref(&ok), Some(&[]), &[]).is_ok());
        let duplicate = [ok.clone(), ok.clone()];
        assert_eq!(of_factories(1, &duplicate, Some(&[]), &[]).unwrap_err().code, "E_CATALOG");
        let keyword = Descriptor { keyword_inputs: vec!["field".into()], slot_types: vec!["fn(vec3,)->float".into()], ..ok.clone() };
        assert!(of_factories(1, &[keyword], Some(&[]), &[]).is_err());
        let typed = Descriptor { keyword_inputs: vec!["field".into()], slot_types: vec!["fn(vec3,float)->float".into()], ..ok.clone() };
        let catalog = of_factories(1, &[typed], Some(&[]), &[]).expect("typed keyword input");
        assert!(catalog.kinds[0].slots.is_empty());
        assert_eq!(catalog.kinds[0].parameters[0].name, "field");
        let unknown = Descriptor { qualified: "nowhere/x".into(), ..ok };
        assert!(of_factories(1, &[unknown], Some(&[]), &[]).is_err());
    }
}

