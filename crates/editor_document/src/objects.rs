//! Port of lib/editor_document/objects.ml: the scene's objects as plain kinds (`scene/geometry`,
//! `scene/camera`, `scene/light`, `scene/root`): typed schemas, and the reads and edits of a
//! `kind_graph::T` of them. The input of an object is its parent.
use crate::kind_graph::{self as g, chc, fl, int, kind_make, schema_of, tog, Rec};
use param::{Schema, Value};
use rays::{camera as rcamera, light as rlight};
use rays_math::{color, mat4, vec3};
use std::sync::{Arc, OnceLock};

fn radians(degrees: f64) -> f64 {
    degrees * std::f64::consts::PI / 180.
}

fn float(name: &str, value: f64) -> (String, Value) {
    (name.to_owned(), Value::Float(value))
}

pub mod geometry {
    use super::*;

    #[derive(Clone, Debug, PartialEq)]
    pub struct Parameters {
        pub translate_x: f64,
        pub translate_y: f64,
        pub translate_z: f64,
        pub rotate_x: f64,
        pub rotate_y: f64,
        pub rotate_z: f64,
        pub scale_x: f64,
        pub scale_y: f64,
        pub scale_z: f64,
        pub visible: bool,
        pub render: bool,
    }

    pub fn parameters_schema() -> Arc<Schema> {
        static S: OnceLock<Arc<Schema>> = OnceLock::new();
        S.get_or_init(|| {
            let t = |n: &'static str, l: &'static str| fl(n, l, "Transform", 0., -20., 20., None, None);
            let r = |n: &'static str, l: &'static str| fl(n, l, "Transform", 0., -180., 180., None, None);
            let s = |n: &'static str, l: &'static str| fl(n, l, "Transform", 1., 0., 5., None, None);
            schema_of(vec![
                t("translate_x", "Translate X"),
                t("translate_y", "Translate Y"),
                t("translate_z", "Translate Z"),
                r("rotate_x", "Rotate X (degrees)"),
                r("rotate_y", "Rotate Y (degrees)"),
                r("rotate_z", "Rotate Z (degrees)"),
                s("scale_x", "Scale X"),
                s("scale_y", "Scale Y"),
                s("scale_z", "Scale Z"),
                tog("visible", "Visible", "", true),
                tog("render", "Renderable", "", true),
                chc("cull", "Cull faces", &CULL_LABELS, "Back"),
            ])
        })
        .clone()
    }

    /// Which faces the raster view skips: back faces by default, so a closed surface draws about
    /// half its triangles; "None" for open or single-sided geometry seen from both sides.
    pub const CULL_LABELS: [&str; 3] = ["Back", "None", "Front"];

    pub fn from_values(values: Vec<Value>) -> Parameters {
        let r = Rec::of(&parameters_schema(), values);
        Parameters {
            translate_x: r.f("translate_x"),
            translate_y: r.f("translate_y"),
            translate_z: r.f("translate_z"),
            rotate_x: r.f("rotate_x"),
            rotate_y: r.f("rotate_y"),
            rotate_z: r.f("rotate_z"),
            scale_x: r.f("scale_x"),
            scale_y: r.f("scale_y"),
            scale_z: r.f("scale_z"),
            visible: r.b("visible"),
            render: r.b("render"),
        }
    }

    pub fn parameters_default() -> Parameters {
        from_values(parameters_schema().default().to_vec())
    }

    pub fn kind() -> g::Kind {
        kind_make("geometry", "Geometry", &["Object"], &parameters_schema())
    }
}

/// A camera looks from its eye at its target; it has no parent.
pub mod camera {
    use super::*;

    #[derive(Clone, Debug, PartialEq)]
    pub struct Parameters {
        pub fov: f64,
        pub near: f64,
        pub far: f64,
        pub aperture: f64,
        pub focus_distance: f64,
        pub eye_x: f64,
        pub eye_y: f64,
        pub eye_z: f64,
        pub target_x: f64,
        pub target_y: f64,
        pub target_z: f64,
        pub up_x: f64,
        pub up_y: f64,
        pub up_z: f64,
        pub follow_viewport: bool,
    }

    pub fn parameters_schema() -> Arc<Schema> {
        static S: OnceLock<Arc<Schema>> = OnceLock::new();
        S.get_or_init(|| {
            let mut fov = fl("fov", "FOV (degrees)", "Lens", 60., 5., 150., Some(1.), Some(179.));
            fov.unit = Some("deg");
            let tr = |n: &'static str, l: &'static str, d: f64| fl(n, l, "Transform", d, -100., 100., None, None);
            let up = |n: &'static str, l: &'static str, d: f64| fl(n, l, "Transform", d, -1., 1., None, None);
            schema_of(vec![
                fov,
                fl("near", "Near clip", "Lens", 0.1, 0.01, 10., Some(0.0001), None),
                fl("far", "Far clip", "Lens", 1000., 10., 10000., Some(0.001), None),
                fl("aperture", "Aperture (lens radius)", "Lens", 0., 0., 2., Some(0.), None),
                fl("focus_distance", "Focus distance (0: target)", "Lens", 0., 0., 100., Some(0.), None),
                tr("eye_x", "Eye X", 0.),
                tr("eye_y", "Eye Y", 0.),
                tr("eye_z", "Eye Z", 7.),
                tr("target_x", "Target X", 0.),
                tr("target_y", "Target Y", 0.),
                tr("target_z", "Target Z", 0.),
                up("up_x", "Up X", 0.),
                up("up_y", "Up Y", 1.),
                up("up_z", "Up Z", 0.),
                tog("follow_viewport", "Follow viewport", "Transform", false),
            ])
        })
        .clone()
    }

    pub fn from_values(values: Vec<Value>) -> Parameters {
        let r = Rec::of(&parameters_schema(), values);
        Parameters {
            fov: r.f("fov"),
            near: r.f("near"),
            far: r.f("far"),
            aperture: r.f("aperture"),
            focus_distance: r.f("focus_distance"),
            eye_x: r.f("eye_x"),
            eye_y: r.f("eye_y"),
            eye_z: r.f("eye_z"),
            target_x: r.f("target_x"),
            target_y: r.f("target_y"),
            target_z: r.f("target_z"),
            up_x: r.f("up_x"),
            up_y: r.f("up_y"),
            up_z: r.f("up_z"),
            follow_viewport: r.b("follow_viewport"),
        }
    }

    pub fn parameters_default() -> Parameters {
        from_values(parameters_schema().default().to_vec())
    }

    pub fn kind() -> g::Kind {
        kind_make("camera", "Camera", &["Object"], &parameters_schema())
    }

    pub fn of_node(node: &g::Node) -> Option<(rcamera::t, bool)> {
        if node.kind != "camera" {
            return None;
        }
        let p = from_values(super::read(&parameters_schema(), node)?);
        let at = vec3::create(p.eye_x, p.eye_y, p.eye_z);
        let target = vec3::create(p.target_x, p.target_y, p.target_z);
        let up = vec3::create(p.up_x, p.up_y, p.up_z);
        let lens = rcamera::lens {
            aperture: p.aperture,
            focus_distance: if p.focus_distance > 0. { Some(p.focus_distance) } else { None },
        };
        let camera = rcamera::perspective(Some(p.fov * std::f64::consts::PI / 180.), Some(p.near), Some(p.far), at, target).ok()?;
        let camera = rcamera::with_up(up, &camera).ok()?;
        let camera = rcamera::with_lens(lens, &camera).ok()?;
        Some((camera, p.follow_viewport))
    }

    pub fn lens_values(lens: &rcamera::lens) -> Vec<(String, Value)> {
        vec![float("aperture", lens.aperture), float("focus_distance", lens.focus_distance.unwrap_or(0.))]
    }

    pub fn to_values(eye: vec3::T, target: vec3::T, fov_y: f64) -> Vec<(String, Value)> {
        vec![
            float("eye_x", eye.x),
            float("eye_y", eye.y),
            float("eye_z", eye.z),
            float("target_x", target.x),
            float("target_y", target.y),
            float("target_z", target.z),
            float("fov", fov_y * 180. / std::f64::consts::PI),
        ]
    }
}

/// How the scene is rendered: the one `scene/root` of a scene graph (a scene graph without one is a
/// part and gets the defaults). The active camera is the root's `:camera` slot, not a field.
pub mod root {
    use super::*;

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Renderer {
        Raster,
        Wireframe,
        PathTraced,
    }

    pub const RENDERER_LABELS: [&str; 3] = ["Raster", "Wireframe", "Path traced"];

    pub fn renderer_label(r: Renderer) -> &'static str {
        match r {
            Renderer::Raster => "Raster",
            Renderer::Wireframe => "Wireframe",
            Renderer::PathTraced => "Path traced",
        }
    }

    pub fn renderer_of_label(label: &str) -> Renderer {
        match label {
            "Wireframe" => Renderer::Wireframe,
            "Path traced" => Renderer::PathTraced,
            _ => Renderer::Raster,
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Parameters {
        pub renderer: Renderer,
        pub width: i64,
        pub height: i64,
        pub max_spp: i64,
        pub bounces: i64,
        pub round_samples: i64,
    }

    pub fn parameters_schema() -> Arc<Schema> {
        static S: OnceLock<Arc<Schema>> = OnceLock::new();
        S.get_or_init(|| {
            schema_of(vec![
                chc("renderer", "Renderer", &RENDERER_LABELS, "Raster"),
                int("width", "Width (pixels)", "Size", 1920, 64, 7680, Some(1), Some(16384)),
                int("height", "Height (pixels)", "Size", 1080, 64, 4320, Some(1), Some(16384)),
                int("max_spp", "Max samples per pixel", "Samples", 256, 1, 4096, Some(1), None),
                int("bounces", "Bounces", "Samples", 4, 1, 32, Some(1), Some(64)),
                int("round_samples", "Round-corner samples", "Samples", 4, 0, 16, Some(0), Some(64)),
            ])
        })
        .clone()
    }

    pub fn from_values(values: Vec<Value>) -> Parameters {
        let r = Rec::of(&parameters_schema(), values);
        Parameters {
            renderer: renderer_of_label(r.c("renderer")),
            width: r.i("width"),
            height: r.i("height"),
            max_spp: r.i("max_spp"),
            bounces: r.i("bounces"),
            round_samples: r.i("round_samples"),
        }
    }

    pub fn to_values(p: &Parameters) -> Vec<Value> {
        vec![
            Value::Choice(renderer_label(p.renderer).to_owned()),
            Value::Int(p.width),
            Value::Int(p.height),
            Value::Int(p.max_spp),
            Value::Int(p.bounces),
            Value::Int(p.round_samples),
        ]
    }

    pub fn parameters_default() -> Parameters {
        from_values(parameters_schema().default().to_vec())
    }

    pub fn kind() -> g::Kind {
        kind_make("root", "Root", &["Render"], &parameters_schema())
    }

    pub fn default() -> Parameters {
        parameters_default()
    }

    /// The render resolution and samples a sketch exports at.
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct Render {
        pub width: i64,
        pub height: i64,
        pub max_spp: i64,
    }

    pub fn render(p: &Parameters) -> Render {
        Render { width: p.width.max(1), height: p.height.max(1), max_spp: p.max_spp.max(1) }
    }

    pub fn fields(p: &Parameters) -> Vec<param::FieldView> {
        parameters_schema().view(&to_values(p)).expect("valid root record")
    }
}

pub mod light {
    use super::*;

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Shape {
        Point,
        Spot,
        Area,
        Directional,
    }

    pub const SHAPE_LABELS: [&str; 4] = ["Area", "Point", "Spot", "Directional"];

    pub fn shape_of_label(label: &str) -> Shape {
        match label {
            "Point" => Shape::Point,
            "Spot" => Shape::Spot,
            "Directional" => Shape::Directional,
            _ => Shape::Area,
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Parameters {
        pub shape: Shape,
        pub translate_x: f64,
        pub translate_y: f64,
        pub translate_z: f64,
        pub target_x: f64,
        pub target_y: f64,
        pub target_z: f64,
        pub color_r: f64,
        pub color_g: f64,
        pub color_b: f64,
        pub intensity: f64,
        pub width: f64,
        pub height: f64,
        pub cone: f64,
        pub visible: bool,
        pub render: bool,
    }

    pub fn parameters_schema() -> Arc<Schema> {
        static S: OnceLock<Arc<Schema>> = OnceLock::new();
        S.get_or_init(|| {
            let tr = |n: &'static str, l: &'static str, d: f64| fl(n, l, "Transform", d, -20., 20., None, None);
            let aim = |n: &'static str, l: &'static str| fl(n, l, "Aim", 0., -20., 20., None, None);
            let col = |n: &'static str, l: &'static str| fl(n, l, "Color", 1., 0., 1., Some(0.), None);
            schema_of(vec![
                chc("shape", "Type", &SHAPE_LABELS, "Area"),
                tr("translate_x", "Translate X", 3.),
                tr("translate_y", "Translate Y", 5.),
                tr("translate_z", "Translate Z", 4.),
                aim("target_x", "Target X"),
                aim("target_y", "Target Y"),
                aim("target_z", "Target Z"),
                col("color_r", "Red"),
                col("color_g", "Green"),
                col("color_b", "Blue"),
                fl("intensity", "Intensity", "", 40., 0., 2000., Some(0.), None),
                fl("width", "Width", "Area", 2., 0.01, 20., Some(0.001), None),
                fl("height", "Height", "Area", 2., 0.01, 20., Some(0.001), None),
                fl("cone", "Cone (degrees)", "Spot", 30., 1., 89., Some(0.1), Some(89.9)),
                tog("visible", "Visible", "", true),
                tog("render", "Renderable", "", true),
            ])
        })
        .clone()
    }

    pub fn from_values(values: Vec<Value>) -> Parameters {
        let r = Rec::of(&parameters_schema(), values);
        Parameters {
            shape: shape_of_label(r.c("shape")),
            translate_x: r.f("translate_x"),
            translate_y: r.f("translate_y"),
            translate_z: r.f("translate_z"),
            target_x: r.f("target_x"),
            target_y: r.f("target_y"),
            target_z: r.f("target_z"),
            color_r: r.f("color_r"),
            color_g: r.f("color_g"),
            color_b: r.f("color_b"),
            intensity: r.f("intensity"),
            width: r.f("width"),
            height: r.f("height"),
            cone: r.f("cone"),
            visible: r.b("visible"),
            render: r.b("render"),
        }
    }

    pub fn parameters_default() -> Parameters {
        from_values(parameters_schema().default().to_vec())
    }

    pub fn kind() -> g::Kind {
        kind_make("light", "Light", &["Object"], &parameters_schema())
    }
}

/// The node's values read through `schema` (OCaml `read`); `None` when a value is refused.
pub fn read(schema: &Schema, node: &g::Node) -> Option<Vec<Value>> {
    // values made over this very schema are already its record
    if let Some(values) = crate::settings::record_of(&node.values, schema) {
        return Some(values.to_vec());
    }
    let changes: Vec<(String, Value)> = g::fields(node).into_iter().map(|f| (f.name, f.current)).collect();
    schema.apply_all(schema.default(), &changes).ok().map(|(values, _)| values)
}

pub fn geometry(node: &g::Node) -> Option<geometry::Parameters> {
    if node.kind == "geometry" {
        read(&geometry::parameters_schema(), node).map(geometry::from_values)
    } else {
        None
    }
}

pub fn light(node: &g::Node) -> Option<light::Parameters> {
    if node.kind == "light" {
        read(&light::parameters_schema(), node).map(light::from_values)
    } else {
        None
    }
}

/// Translate, then rotate Z.Y.X (degrees), then scale; lights and cameras carry positions instead
/// of a rotation, so only a translation.
fn local(node: &g::Node) -> mat4::T {
    match geometry(node) {
        Some(p) => mat4::mul(
            &mat4::translation(vec3::create(p.translate_x, p.translate_y, p.translate_z)),
            &mat4::mul(
                &mat4::rotation_z(radians(p.rotate_z)),
                &mat4::mul(
                    &mat4::rotation_y(radians(p.rotate_y)),
                    &mat4::mul(&mat4::rotation_x(radians(p.rotate_x)), &mat4::scaling(vec3::create(p.scale_x, p.scale_y, p.scale_z))),
                ),
            ),
        ),
        None => mat4::identity(),
    }
}

pub fn parent(scene: &[g::Node], id: i64) -> Option<i64> {
    g::find(scene, id).and_then(|node| node.input)
}

/// `above` is `id` or one of its descendants: parenting `id` under it would make a cycle.
pub fn within(scene: &[g::Node], id: i64, above: i64) -> bool {
    let mut above = above;
    loop {
        if above == id {
            return true;
        }
        match parent(scene, above) {
            Some(p) => above = p,
            None => return false,
        }
    }
}

/// The parent chain's transforms, excluding the object's own.
pub fn parent_world(scene: &[g::Node], id: i64) -> mat4::T {
    match parent(scene, id) {
        None => mat4::identity(),
        Some(parent_id) => match g::find(scene, parent_id) {
            Some(node) => mat4::mul(&parent_world(scene, parent_id), &local(&node)),
            None => mat4::identity(),
        },
    }
}

pub fn world(scene: &[g::Node], id: i64) -> mat4::T {
    match g::find(scene, id) {
        Some(node) => mat4::mul(&parent_world(scene, id), &local(&node)),
        None => mat4::identity(),
    }
}

pub fn visible(node: &g::Node) -> bool {
    match (geometry(node), light(node)) {
        (Some(p), _) => p.visible,
        (None, Some(p)) => p.visible,
        (None, None) => true,
    }
}

/// A geometry object's face culling (back faces when it says nothing).
pub fn cull(node: &g::Node) -> rays::scene3::cull {
    match g::field(node, "cull") {
        Some(Value::Choice(c)) if c == "None" => rays::scene3::cull::Cull_none,
        Some(Value::Choice(c)) if c == "Front" => rays::scene3::cull::Cull_front,
        _ => rays::scene3::cull::Cull_back,
    }
}

pub fn flag(name: &str, node: &g::Node) -> bool {
    g::field(node, name) == Some(Value::Bool(true))
}

pub fn has_flag(name: &str, node: &g::Node) -> bool {
    g::field(node, name).is_some()
}

/// A light object as a renderer light, its positions in world space.
fn to_light(scene: &[g::Node], id: i64) -> Option<rlight::t> {
    let node = g::find(scene, id)?;
    let p = light(&node)?;
    let space = parent_world(scene, id);
    let at = mat4::transform_point(&space, vec3::create(p.translate_x, p.translate_y, p.translate_z));
    let target = mat4::transform_point(&space, vec3::create(p.target_x, p.target_y, p.target_z));
    let direction = {
        let d = vec3::sub(target, at);
        if vec3::length(d) < 1e-6 { vec3::create(0., -1., 0.) } else { vec3::normalize(d) }
    };
    let channel = |value: f64| (255. * rays::num::fmin(1., value)).round() as i64;
    let diffuse = color::rgb(channel(p.color_r), channel(p.color_g), channel(p.color_b));
    let intensity = p.intensity;
    // Physical 1/d^2 falloff, so raster and the path tracer agree.
    let attenuation = rlight::attenuation(Some(0.), Some(1.)).ok()?;
    let result = match p.shape {
        light::Shape::Point => rlight::point(Some(diffuse), Some(intensity), Some(attenuation), at),
        light::Shape::Spot => rlight::spot(Some(diffuse), Some(intensity), Some(attenuation), at, direction, radians(p.cone), 8.),
        // ponytail: one raster sample per area light keeps the viewport fast; the path tracer
        // integrates the true rectangle.
        light::Shape::Area => rlight::area(Some(diffuse), Some(intensity), Some(attenuation), Some(1), at, direction, p.width, p.height),
        light::Shape::Directional => rlight::directional(Some(diffuse), Some(intensity), direction),
    };
    result.ok()
}

pub fn ids(kind: &str, scene: &[g::Node]) -> Vec<i64> {
    g::ids(kind, scene)
}

pub fn lights(render: bool, scene: &[g::Node]) -> Vec<rlight::t> {
    ids("light", scene)
        .into_iter()
        .filter_map(|id| match g::find(scene, id) {
            Some(node) if visible(&node) && (!render || flag("render", &node)) => to_light(scene, id),
            _ => None,
        })
        .collect()
}

/// Parameters a light object gets from a renderer light.
pub fn light_values(l: &rlight::t) -> Vec<(String, Value)> {
    let (r, gr, b, _) = color::to_tuple(l.diffuse);
    let (at, target, shape, extra): (vec3::T, vec3::T, &str, Vec<(String, Value)>) = match &l.kind {
        rlight::kind::Directional { direction } => (vec3::scale(*direction, -8.), vec3::zero(), "Directional", vec![]),
        rlight::kind::Point { position, .. } => (*position, vec3::zero(), "Point", vec![]),
        rlight::kind::Spot { position, direction, cutoff, .. } => {
            (*position, vec3::add(*position, *direction), "Spot", vec![float("cone", cutoff * 180. / std::f64::consts::PI)])
        }
        rlight::kind::Area { position, direction, width, height, .. } => {
            (*position, vec3::add(*position, *direction), "Area", vec![float("width", *width), float("height", *height)])
        }
    };
    let mut out = vec![
        ("shape".to_owned(), Value::Choice(shape.to_owned())),
        float("translate_x", at.x),
        float("translate_y", at.y),
        float("translate_z", at.z),
        float("target_x", target.x),
        float("target_y", target.y),
        float("target_z", target.z),
        float("color_r", r as f64 / 255.),
        float("color_g", gr as f64 / 255.),
        float("color_b", b as f64 / 255.),
        float("intensity", l.intensity),
    ];
    out.extend(extra);
    out
}

/// The object kinds a scene adds: geometry, light, camera.
pub fn kinds() -> Vec<g::Kind> {
    vec![geometry::kind(), light::kind(), camera::kind()]
}

/// Object parameters for a local matrix T.Rz.Ry.Rx.S (shear is dropped).
fn decompose(matrix: &mat4::T) -> Vec<(String, Value)> {
    let get = |row: i64, column: i64| mat4::get(matrix, row, column).expect("matrix index");
    let scale = |column: i64| {
        let length = vec3::length(vec3::create(get(0, column), get(1, column), get(2, column)));
        if length < 1e-9 { 1. } else { length }
    };
    let (sx, sy, sz) = (scale(0), scale(1), scale(2));
    let r = |row: i64, column: i64| get(row, column) / [sx, sy, sz][column as usize];
    let degrees = |angle: f64| angle * 180. / std::f64::consts::PI;
    use rays::num::{fmax, fmin};
    vec![
        float("translate_x", get(0, 3)),
        float("translate_y", get(1, 3)),
        float("translate_z", get(2, 3)),
        float("rotate_x", degrees(r(2, 1).atan2(r(2, 2)))),
        float("rotate_y", degrees(fmax(-1., fmin(1., -r(2, 0))).asin())),
        float("rotate_z", degrees(r(1, 0).atan2(r(0, 0)))),
        float("scale_x", sx),
        float("scale_y", sy),
        float("scale_z", sz),
    ]
}

/// Parameters that keep `id` where it was in the world after its parent changed; `before` is the
/// old parent chain's transform.
fn keep_world(scene: &[g::Node], id: i64, before: &mat4::T) -> Vec<(String, Value)> {
    let (Some(node), Some(inverse)) = (g::find(scene, id), mat4::inverse(&parent_world(scene, id))) else {
        return Vec::new();
    };
    let change = mat4::mul(&inverse, before);
    if geometry(&node).is_some() {
        return decompose(&mat4::mul(&change, &local(&node)));
    }
    match light(&node) {
        Some(p) => {
            let point = |x: f64, y: f64, z: f64| mat4::transform_point(&change, vec3::create(x, y, z));
            let at = point(p.translate_x, p.translate_y, p.translate_z);
            let target = point(p.target_x, p.target_y, p.target_z);
            vec![
                float("translate_x", at.x),
                float("translate_y", at.y),
                float("translate_z", at.z),
                float("target_x", target.x),
                float("target_y", target.y),
                float("target_z", target.z),
            ]
        }
        None => Vec::new(),
    }
}

/// `scene` with `ids` rewired under `under` (None: the scene root), each keeping its world placement.
pub fn reparent(scene: &[g::Node], ids: &[i64], under: Option<i64>) -> g::T {
    let mut scene: g::T = scene.to_vec();
    for &id in ids {
        let before = parent_world(&scene, id);
        scene = match under {
            Some(above) if !within(&scene, id, above) => g::update(&scene, id, &|n| g::Node { input: Some(above), ..n }),
            Some(_) => scene,
            None => g::update(&scene, id, &|n| g::Node { input: None, ..n }),
        };
        let values = keep_world(&scene, id, &before);
        if !values.is_empty() {
            if let Ok((next, _)) = g::set_fields(&scene, id, &values) {
                scene = next;
            }
        }
    }
    scene
}

/// Each of `ids` moved out to its parent's parent.
pub fn outdent(scene: &[g::Node], ids: &[i64]) -> g::T {
    let mut scene: g::T = scene.to_vec();
    for &id in ids {
        if let Some(above) = parent(&scene, id) {
            let grand = parent(&scene, above);
            scene = reparent(&scene, &[id], grand);
        }
    }
    scene
}
