//! Port of lib/editor_document/kinds.ml: one Lisp kind per object schema (`scene/light`), per World
//! layer (`world/sky`) and the workspace settings record (`settings/config`), generated from their
//! schemas, with the window the settings graph asks of the host.
//!
//! These plug into the catalog as `flow_sop_catalog::catalog::Descriptor`s passed as `extra` to
//! `flow_sop_catalog::catalog::of_factories` (see `catalog` below); a manifest generator that wants them in
//! `flow_manifest.sexp` appends `descriptors()` to the factory descriptors.
use crate::kind_graph::{self as g, chc, int, kind_fields, schema_of};
use crate::layers;
use crate::objects;
use flow_check::catalog::Catalog;
use flow_sop_catalog::port::Parameter;
use flow_sop_catalog::catalog::{self as fcat, Descriptor};
use flow_sop_catalog::port;
use param::{FieldView, Impact, KindView, Schema, Value};
use rays_diagnostic::Diagnostic;
use sop::decl;
use sop::edit_graph::{Factory, InputRequirement};
use std::sync::{Arc, OnceLock};

fn name_field() -> FieldView {
    FieldView {
        name: "name".to_owned(),
        label: "Name".to_owned(),
        description: None,
        folder: Vec::new(),
        impact: Impact::Cook,
        primary: false,
        unit: None,
        vec3: None,
        kind: KindView::Text,
        default: Value::Text(Vec::new()),
        current: Value::Text(Vec::new()),
    }
}

/// `:parent "label"` places an object under another by name (a camera has none); `:active true`
/// makes a camera the render camera. Neither is a field of the node: the lowering reads them.
fn parent_field() -> FieldView {
    FieldView { name: "parent".to_owned(), label: "Parent".to_owned(), ..name_field() }
}

fn active_field() -> FieldView {
    FieldView {
        name: "active".to_owned(),
        label: "Active camera".to_owned(),
        kind: KindView::Toggle,
        default: Value::Bool(false),
        current: Value::Bool(false),
        ..name_field()
    }
}

/// The World and the root are scene kinds too: `scene/world` is a merge member that references a
/// world graph, `scene/root` ends the scene graph.
pub fn scene_kinds() -> Vec<(String, g::Kind)> {
    let mut kinds = objects::kinds();
    kinds.push(layers::settings::kind());
    kinds.push(objects::root::kind());
    kinds.into_iter().map(|k| (format!("scene/{}", k.key), k)).collect()
}

pub fn world_kinds() -> Vec<(String, g::Kind)> {
    layers::kinds().into_iter().map(|k| (format!("world/{}", k.key), k)).collect()
}

/// Workspace settings: what the host needs to open the window.
#[derive(Clone, Debug, PartialEq)]
pub struct Window {
    pub title: String,
    pub width: i64,
    pub height: i64,
    pub fps: i64,
    pub seed: i64,
    pub fast: bool,
}

pub fn window_schema() -> Arc<Schema> {
    static S: OnceLock<Arc<Schema>> = OnceLock::new();
    S.get_or_init(|| {
        let title = decl::Spec {
            label: "Title (on restart)",
            ..decl::spec("title", decl::text_kind(), Value::Text(b"Rays".to_vec()))
        };
        schema_of(vec![
            title,
            int("width", "Width (on restart)", "", 1280, 320, 3840, Some(64), None),
            int("height", "Height (on restart)", "", 800, 240, 2160, Some(64), None),
            int("fps", "Frames per second (on restart)", "", 60, 1, 120, Some(1), Some(240)),
            int("seed", "Seed (on restart)", "", 1, 0, 9999, Some(0), None),
            // fast: what only reaches the screen may run on the GPU in float32 and int32
            chc("precision", "Precision (live)", &["fast", "exact"], "fast"),
        ])
    })
    .clone()
}

pub fn window_of_values(values: &[Value]) -> Window {
    let r = g::Rec::of(&window_schema(), values.to_vec());
    let title = match &values[0] {
        Value::Text(t) => String::from_utf8_lossy(t).into_owned(),
        _ => String::new(),
    };
    Window { title, width: r.i("width"), height: r.i("height"), fps: r.i("fps"), seed: r.i("seed"), fast: r.c("precision") == "fast" }
}

pub fn window_default() -> Window {
    window_of_values(window_schema().default())
}

fn window_fields() -> Vec<FieldView> {
    let schema = window_schema();
    schema.view(schema.default()).expect("valid window record")
}

/// Slot of each kind: a geometry object takes its geometry, a World layer the layer below.
fn kind_slots(qualified: &str) -> Vec<(String, InputRequirement)> {
    let s = |n: &str, r: InputRequirement| (n.to_owned(), r);
    match qualified {
        "scene/geometry" => vec![s("geometry", InputRequirement::Required), s("texture", InputRequirement::Optional)],
        "scene/world" => vec![s("world", InputRequirement::Required)],
        "scene/root" => vec![s("scene", InputRequirement::Required), s("camera", InputRequirement::Optional)],
        q if q.starts_with("world/") => vec![s("below", InputRequirement::Optional)],
        _ => Vec::new(),
    }
}

fn extra_fields(qualified: &str) -> Vec<FieldView> {
    match qualified {
        "scene/camera" => vec![active_field()],
        "scene/geometry" | "scene/light" => vec![parent_field()],
        _ => Vec::new(),
    }
}

/// Every generated kind; the manifest generator writes them.
pub fn descriptors() -> Vec<Descriptor> {
    static D: OnceLock<Vec<Descriptor>> = OnceLock::new();
    D.get_or_init(|| {
        let mut out: Vec<Descriptor> = Vec::new();
        for (qualified, kind) in scene_kinds().into_iter().chain(world_kinds()) {
            let groups = param::group_vec3(&kind_fields(&kind), true).expect("valid vec3 groups");
            let mut fields = vec![name_field()];
            fields.extend(extra_fields(&qualified));
            fields.extend(groups.into_iter().flatten());
            out.push(Descriptor {
                slots: kind_slots(&qualified),
                slot_types: if qualified == "scene/geometry" { vec!["geometry".to_owned(), "image".to_owned()] } else { Vec::new() },
                keyword_inputs: Vec::new(),
                fields,
                key: kind.key.clone(),
                operation: kind.key.clone(),
                label: kind.label.clone(),
                category: kind.category.clone(),
                qualified,
            });
        }
        out.push(Descriptor {
            qualified: "settings/config".to_owned(),
            key: "config".to_owned(),
            operation: "config".to_owned(),
            label: "Settings".to_owned(),
            category: vec!["Workspace".to_owned()],
            slots: Vec::new(),
            slot_types: Vec::new(),
            keyword_inputs: Vec::new(),
            fields: window_fields(),
        });
        out
    })
    .clone()
}

/// `Flow_sop.Catalog.of_factories` with `descriptors`: what a workspace text is checked against.
/// Not cached: see `shared`.
pub fn catalog(version: i64, factories: &[Factory]) -> Result<Catalog, Diagnostic> {
    fcat::of_factories(version, &descriptors(), None, factories)
}

/// The catalog of `factories` (default `sop::nodes::factories`) at the manifest version, shared: built
/// again only when the factories differ from the few last asked (a lowering per edit must not
/// rebuild every kind).
pub fn shared(factories: Option<&[Factory]>) -> Result<Arc<Catalog>, Diagnostic> {
    type Entry = (Vec<Factory>, Result<Arc<Catalog>, Diagnostic>);
    thread_local! { static CACHE: std::cell::RefCell<Vec<Entry>> = const { std::cell::RefCell::new(Vec::new()) }; }
    let all = factories.map(<[Factory]>::to_vec).unwrap_or_else(sop::nodes::factories);
    CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        if let Some((_, found)) = cache.iter().find(|(f, _)| f.len() == all.len() && f.iter().zip(&all).all(|(a, b)| sop::edit_graph::same_factory(a, b))) {
            return found.clone();
        }
        let built = catalog(flow_sop_catalog::manifest::VERSION, &all).map(Arc::new);
        cache.insert(0, (all, built.clone()));
        cache.truncate(4);
        built
    })
}

/// The parameters of a qualified kind; none for an unknown one.
pub fn ports(qualified: &str) -> Vec<Parameter> {
    match descriptors().iter().find(|d| d.qualified == qualified) {
        Some(d) => port::parameters(false, &d.fields).expect("valid by construction"),
        None => Vec::new(),
    }
}
