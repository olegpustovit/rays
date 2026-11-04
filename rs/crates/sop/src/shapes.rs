//! Port of lib/sop/shapes.ml: shape nodes, declared once. This wave ports `null` and `switch`, the
//! worked examples of PORTING.md; clip, exploded_view, pack_instances, soft_transform and ray follow.
use std::sync::Arc;

use param::Value;
use rays_diagnostic::Diagnostic;

use crate::context::{self, dependencies};
use crate::decl::{self, Decl, Meta, Spec};
use crate::edit_graph::Factory;
use crate::node::{self, build, CookMode, InputPolicy};
use crate::support;

pub mod null {
    use super::*;

    // `type parameters = unit`: no fields, so no Parameters struct and no conversion.
    pub const META: Meta = Meta {
        key: "null",
        label: "Null",
        category: "Utility",
        inputs: 1,
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        Vec::new()
    }

    pub fn operator(label: &str, _values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        Ok(build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                cook_mode: CookMode::Passthrough(0),
                dependencies: dependencies::STATIC,
                inputs: vec![inputs[0].clone().unwrap()],
                ..build::args("null")
            },
            None,
            Arc::new(|_, _, inputs| support::cooked(inputs[0].clone())),
        ))
    }

    pub fn decl() -> Decl {
        Decl { meta: META, schema: decl::schema(&META, specs()), operator, dynamic_schema: None }
    }

    pub fn factory() -> Factory {
        decl::factory(&decl())
    }
}

pub mod switch {
    use super::*;

    #[derive(Clone, Debug, PartialEq)]
    pub struct Parameters {
        pub input: i64,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        match &values[0] {
            Value::Int(input) => Parameters { input: *input },
            _ => panic!("switch: parameter record has the wrong shape"),
        }
    }

    pub fn to_values(p: &Parameters) -> Vec<Value> {
        vec![Value::Int(p.input)]
    }

    pub const META: Meta = Meta {
        key: "switch",
        label: "Switch",
        category: "Utility",
        inputs: 3,
        slots: Some("a, b, inputs"),
        optional: &[2],
        rest: Some(2),
        ..decl::META
    };

    /// `choices`: the connected branches by index and label, once the node exists (the dynamic
    /// schema); `None` for the static schema.
    pub fn specs(choices: Option<Vec<String>>) -> Vec<Spec> {
        let kind = match choices {
            Some(labels) => param::index_choice(labels).expect("switch choices"),
            None => decl::int_kind(0, 9999, Some(0), None),
        };
        vec![Spec {
            label: "Source",
            description: Some("Input branch displayed and cooked by this Switch SOP"),
            ..decl::spec("input", kind, Value::Int(0))
        }]
    }

    /// The choice lists the connected branches by index and label.
    pub fn schema(_values: &[Value], made: &node::T) -> Arc<param::Schema> {
        let labels = node::inputs(made)
            .iter()
            .enumerate()
            .map(|(index, input)| format!("{index} \u{b7} {}", node::label(input)))
            .collect();
        decl::schema(&META, specs(Some(labels)))
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let nodes = decl::connected(inputs);
        let index = from_values(values).input;
        if index < 0 || index as usize >= nodes.len() {
            return Err("sop/switch: input is outside the connected branches".to_owned());
        }
        Ok(build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                cook_mode: CookMode::Passthrough(index),
                dependencies: dependencies::STATIC,
                input_policy: Some(InputPolicy::Only(index)),
                inputs: nodes,
                ..build::args("switch")
            },
            None,
            Arc::new(|_, _, selected| support::cooked(selected[0].clone())),
        ))
    }

    pub fn decl() -> Decl {
        Decl { meta: META, schema: decl::schema(&META, specs(None)), operator, dynamic_schema: Some(schema) }
    }

    pub fn factory() -> Factory {
        decl::factory(&decl())
    }
}

// ---------------------------------------------------------------- shared helpers for the nodes below

/// Positional reader over a schema-normalised value slice (`from_values` of the big records).
struct Reader<'a> {
    values: &'a [Value],
    at: usize,
}

impl<'a> Reader<'a> {
    fn new(values: &'a [Value]) -> Reader<'a> {
        Reader { values, at: 0 }
    }
    fn next(&mut self) -> &'a Value {
        self.at += 1;
        &self.values[self.at - 1]
    }
    fn float(&mut self) -> f64 {
        match self.next() {
            Value::Float(f) => *f,
            _ => panic!("parameter record has the wrong shape"),
        }
    }
    fn int(&mut self) -> i64 {
        match self.next() {
            Value::Int(i) => *i,
            _ => panic!("parameter record has the wrong shape"),
        }
    }
    fn bool(&mut self) -> bool {
        match self.next() {
            Value::Bool(b) => *b,
            _ => panic!("parameter record has the wrong shape"),
        }
    }
    fn text(&mut self) -> String {
        match self.next() {
            Value::Text(t) => String::from_utf8_lossy(t).into_owned(),
            _ => panic!("parameter record has the wrong shape"),
        }
    }
    fn choice(&mut self, table: &[&str]) -> usize {
        support::choice_index(table, self.next())
    }
}

fn fl(name: &'static str, label: &'static str, folder: &'static str, min: f64, max: f64, default: f64) -> Spec {
    Spec { label, folder, ..decl::spec(name, decl::float_kind(min, max, None, None), Value::Float(default)) }
}
fn fl_hard(
    name: &'static str,
    label: &'static str,
    folder: &'static str,
    min: f64,
    max: f64,
    hard_min: f64,
    default: f64,
) -> Spec {
    Spec { label, folder, ..decl::spec(name, decl::float_kind(min, max, Some(hard_min), None), Value::Float(default)) }
}
fn v3(spec: Spec, group: &'static str, component: i64) -> Spec {
    Spec { vec3: Some((group, component)), ..spec }
}
fn txt(name: &'static str, label: &'static str, folder: &'static str, default: &str) -> Spec {
    Spec { label, folder, ..decl::spec(name, decl::text_kind(), Value::Text(default.as_bytes().to_vec())) }
}
fn tog(name: &'static str, label: &'static str, folder: &'static str, default: bool) -> Spec {
    Spec { label, folder, ..decl::spec(name, decl::toggle_kind(), Value::Bool(default)) }
}
fn pick(name: &'static str, label: &'static str, folder: &'static str, kind: param::Kind, default: Value) -> Spec {
    Spec { label, folder, ..decl::spec(name, kind, default) }
}
fn owner_spec(folder: &'static str) -> Spec {
    pick(
        "group_owner",
        "Group type",
        folder,
        support::element_owner_kind(),
        support::element_owner_value(rdk::group_ops::Owner::GroupPoints),
    )
}
fn all_finite(values: &[f64]) -> bool {
    values.iter().all(|v| v.is_finite())
}
/// OCaml `String.trim` (space, \t, \n, \r, \012).
fn trim(text: &str) -> &str {
    text.trim_matches([' ', '\t', '\n', '\r', '\x0c'])
}
fn vec3(x: f64, y: f64, z: f64) -> rays_math::vec3::T {
    rays_math::vec3::create(x, y, z)
}
fn xyz_specs(
    names: [&'static str; 3],
    labels: [&'static str; 3],
    folder: &'static str,
    limit: f64,
    group: &'static str,
    defaults: [f64; 3],
) -> Vec<Spec> {
    (0..3).map(|i| v3(fl(names[i], labels[i], folder, -limit, limit, defaults[i]), group, i as i64)).collect()
}

pub mod clip {
    use super::*;
    use rdk::plane_clip::{self, Keep};

    pub const KEEP_LABELS: [&str; 3] = ["Above", "Below", "All"];
    pub const KEEP: [Keep; 3] = [Keep::Above, Keep::Below, Keep::All];

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub group_owner: rdk::group_ops::Owner,
        pub group: String,
        pub keep: Keep,
        pub snapping_tolerance: f64,
        pub fill: bool,
        pub split_connectivity: bool,
        pub distance: f64,
        pub origin: [f64; 3],
        pub normal: [f64; 3],
        pub clip_attribute: String,
        pub clipped_edge_group: String,
        pub cap_group: String,
        pub clipped_group: String,
        pub above_group: String,
        pub below_group: String,
        pub replace_existing_groups: bool,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        let mut r = Reader::new(values);
        Parameters {
            group_owner: support::element_owner_of_value(r.next()),
            group: r.text(),
            keep: KEEP[r.choice(&KEEP_LABELS)],
            snapping_tolerance: r.float(),
            fill: r.bool(),
            split_connectivity: r.bool(),
            distance: r.float(),
            origin: [r.float(), r.float(), r.float()],
            normal: [r.float(), r.float(), r.float()],
            clip_attribute: r.text(),
            clipped_edge_group: r.text(),
            cap_group: r.text(),
            clipped_group: r.text(),
            above_group: r.text(),
            below_group: r.text(),
            replace_existing_groups: r.bool(),
        }
    }

    pub const META: Meta =
        Meta { key: "clip", label: "Clip", category: "Modify", inputs: 1, validate: Some(validate), ..decl::META };

    pub fn specs() -> Vec<Spec> {
        let mut specs = vec![
            owner_spec("Selection"),
            txt("group", "Group", "Selection", ""),
            pick("keep", "Keep", "", support::choice_kind(&KEEP_LABELS), support::choice_value(&KEEP_LABELS, 0)),
            fl_hard("snapping_tolerance", "Snapping tolerance", "Robustness", 0.0, 0.001, 0.0, 1e-9),
            tog("fill", "Fill cut", "Topology", false),
            tog("split_connectivity", "Split connectivity", "Topology", false),
            fl("distance", "Distance", "Plane", -10.0, 10.0, 0.0),
        ];
        specs.extend(xyz_specs(
            ["origin_x", "origin_y", "origin_z"],
            ["Origin X", "Origin Y", "Origin Z"],
            "Plane/Origin",
            10.0,
            "origin",
            [0.0; 3],
        ));
        specs.extend(xyz_specs(
            ["normal_x", "normal_y", "normal_z"],
            ["Normal X", "Normal Y", "Normal Z"],
            "Plane/Normal",
            1.0,
            "normal",
            [0.0, 1.0, 0.0],
        ));
        specs.extend([
            txt("clip_attribute", "Clip attribute", "Attributes", "P"),
            txt("clipped_edge_group", "Clipped edges", "Output groups", ""),
            txt("cap_group", "Caps", "Output groups", ""),
            txt("clipped_group", "Clipped", "Output groups", ""),
            txt("above_group", "Above", "Output groups", ""),
            txt("below_group", "Below", "Output groups", ""),
            tog("replace_existing_groups", "Replace existing groups", "Output groups", false),
        ]);
        specs
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        let [ox, oy, oz] = p.origin;
        let [nx, ny, nz] = p.normal;
        if !all_finite(&[ox, oy, oz, nx, ny, nz, p.distance, p.snapping_tolerance]) {
            return Err("sop/clip: plane and tolerance must be finite".to_owned());
        }
        if p.snapping_tolerance < 0.0 {
            return Err("sop/clip: snapping tolerance must be non-negative".to_owned());
        }
        if p.split_connectivity && p.keep != Keep::All {
            return Err("sop/clip: split connectivity requires keep=All".to_owned());
        }
        if nx == 0.0 && ny == 0.0 && nz == 0.0 {
            return Err("sop/clip: normal must be non-zero".to_owned());
        }
        let names: Vec<String> = [&p.cap_group, &p.clipped_group, &p.above_group, &p.below_group]
            .into_iter()
            .filter_map(|n| support::optional_text(n))
            .collect();
        let mut unique = names.clone();
        unique.sort();
        unique.dedup();
        if unique.len() != names.len() {
            return Err("sop/clip: output primitive group names must be distinct".to_owned());
        }
        Ok(())
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let selection = support::optional_element_group(p.group_owner, &p.group);
        let clip_attribute = support::optional_text(&p.clip_attribute).unwrap_or_else(|| "P".to_owned());
        let origin = support::vec3_copy(&vec3(p.origin[0], p.origin[1], p.origin[2]));
        let normal = support::vec3_copy(&vec3(p.normal[0], p.normal[1], p.normal[2]));
        let clipped_edge_group = support::optional_text(&p.clipped_edge_group);
        let cap_group = support::optional_text(&p.cap_group);
        let clipped_group = support::optional_text(&p.clipped_group);
        let above_group = support::optional_text(&p.above_group);
        let below_group = support::optional_text(&p.below_group);
        Ok(build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                version: 5,
                cook_mode: CookMode::DuplicateInput(0),
                dependencies: dependencies::STATIC,
                inputs: vec![inputs[0].clone().unwrap()],
                ..build::args("clip")
            },
            None,
            Arc::new(move |_, context, inputs| {
                let selection = support::resolve_element_group("clip", &selection, &inputs[0])?;
                let params = plane_clip::Params {
                    cancel: Some(Arc::clone(context::cancel_token(context))),
                    grain: context::grain(context),
                    keep: p.keep,
                    snapping_tolerance: p.snapping_tolerance,
                    fill: p.fill,
                    split_connectivity: p.split_connectivity,
                    clip_attribute: clip_attribute.clone(),
                    distance: p.distance,
                    selection,
                    replace_existing_groups: p.replace_existing_groups,
                    clipped_edge_group: clipped_edge_group.clone(),
                    cap_group: cap_group.clone(),
                    clipped_group: clipped_group.clone(),
                    above_group: above_group.clone(),
                    below_group: below_group.clone(),
                    origin,
                    normal,
                };
                match plane_clip::clip(&params, &inputs[0]) {
                    Ok(geometry) => support::cooked(geometry),
                    Err(error) => Err(support::structured_rdk_error(&error)),
                }
            }),
        ))
    }

    pub fn decl() -> Decl {
        Decl { meta: META, schema: decl::schema(&META, specs()), operator, dynamic_schema: None }
    }

    pub fn factory() -> Factory {
        decl::factory(&decl())
    }
}

pub mod exploded_view {
    use super::*;
    use param::Impact;

    pub const META: Meta = Meta {
        key: "exploded_view",
        label: "Exploded View",
        category: "Visualize",
        inputs: 1,
        validate: Some(validate),
        ..decl::META
    };

    fn view(spec: Spec) -> Spec {
        Spec { impact: Impact::View, ..spec }
    }

    pub fn specs() -> Vec<Spec> {
        vec![
            view(fl("amount", "Uniform scale", "Explosion", -0.95, 8.0, 0.32)),
            view(v3(fl("scale_x", "Scale X", "Explosion/Scale", -2.0, 2.0, 1.0), "scale", 0)),
            view(v3(fl("scale_y", "Scale Y", "Explosion/Scale", -2.0, 2.0, 1.0), "scale", 1)),
            view(v3(fl("scale_z", "Scale Z", "Explosion/Scale", -2.0, 2.0, 1.0), "scale", 2)),
            txt("piece_attribute", "Piece attribute", "Pieces", "piece"),
            view(fl("noise_amount", "Noise amount", "Noise", 0.0, 1.0, 0.0)),
            view(fl_hard("noise_frequency", "Noise frequency", "Noise", 0.02, 4.0, 0.0, 0.8)),
            view(Spec {
                label: "Noise seed",
                folder: "Noise",
                ..decl::spec("noise_seed", decl::int_kind(0, 9999, None, None), Value::Int(0))
            }),
        ]
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let mut r = Reader::new(values);
        let (amount, sx, sy, sz) = (r.float(), r.float(), r.float(), r.float());
        let piece = r.text();
        let (noise_amount, noise_frequency) = (r.float(), r.float());
        if !all_finite(&[amount, sx, sy, sz, noise_amount, noise_frequency]) || noise_frequency < 0.0 {
            return Err("sop/exploded_view: controls must be finite and noise frequency nonnegative".to_owned());
        }
        if trim(&piece).is_empty() {
            return Err("sop/exploded_view: piece attribute must be nonblank".to_owned());
        }
        Ok(())
    }

    pub fn operator(label: &str, _values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        Ok(build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                cook_mode: CookMode::Passthrough(0),
                dependencies: dependencies::STATIC,
                inputs: vec![inputs[0].clone().unwrap()],
                ..build::args("exploded_view")
            },
            None,
            Arc::new(|_, _, inputs| support::cooked(inputs[0].clone())),
        ))
    }

    pub fn decl() -> Decl {
        Decl { meta: META, schema: decl::schema(&META, specs()), operator, dynamic_schema: None }
    }

    pub fn factory() -> Factory {
        decl::factory(&decl())
    }
}

/// The packed result a Lisp body cannot say on its own: the source unchanged, drawn at the affine rows
/// r0, r1, r2 (Point Float4 attributes of the second input, one point per instance).
pub mod pack_instances {
    use super::*;
    use rdk::attribute::{self, Storage};
    use rdk::{geometry, packed};

    pub const META: Meta = Meta {
        key: "pack_instances",
        label: "Pack Instances",
        category: "Copy",
        inputs: 2,
        slots: Some("source, instances"),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        Vec::new()
    }

    fn row(name: &str, instances: &geometry::T) -> Result<packed::Float4::View::View, Diagnostic> {
        let invalid = |message: String| support::diagnostic("invalid_attribute", &message);
        match geometry::find_attribute(attribute::Owner::Point, name, instances) {
            Some(found) => match attribute::storage(&found).map_err(|e| support::structured_rdk_error(&e))? {
                Storage::Float4(values) => Ok(packed::Float4::View::view(&values)),
                _ => Err(invalid(format!("pack_instances: {name} must be point float4"))),
            },
            None => Err(invalid(format!("pack_instances: missing point attribute {name}"))),
        }
    }

    pub fn operator(label: &str, _values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        Ok(build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                cook_mode: CookMode::Generic,
                dependencies: dependencies::STATIC,
                inputs: vec![inputs[0].clone().unwrap(), inputs[1].clone().unwrap()],
                ..build::args("pack_instances")
            },
            None,
            Arc::new(|_, _, inputs| {
                let (r0, r1, r2) = (row("r0", &inputs[1])?, row("r1", &inputs[1])?, row("r2", &inputs[1])?);
                let transforms: Vec<_> = (0..r0.x.len())
                    .map(|i| {
                        rays_math::mat4::of_rows(
                            (r0.x[i], r0.y[i], r0.z[i], r0.w[i]),
                            (r1.x[i], r1.y[i], r1.z[i], r1.w[i]),
                            (r2.x[i], r2.y[i], r2.z[i], r2.w[i]),
                            (0.0, 0.0, 0.0, 1.0),
                        )
                    })
                    .collect();
                Ok(node::GeometryCooked { geometry: inputs[0].clone(), instances: Some(Arc::new(transforms)) })
            }),
        ))
    }

    pub fn decl() -> Decl {
        Decl { meta: META, schema: decl::schema(&META, specs()), operator, dynamic_schema: None }
    }

    pub fn factory() -> Factory {
        decl::factory(&decl())
    }
}

pub mod soft_transform {
    use super::*;
    use crate::node::{Elementwise, Topology};
    use rdk::transform_ops::{
        self, SoftTransformFalloff, SoftTransformMetric as Metric, TransformOrder as Order, TransformRotationOrder as Rot,
    };
    use support::SoftTransformMetric;

    pub const ORDER_LABELS: [&str; 6] = ["SRT", "STR", "RST", "RTS", "TSR", "TRS"];
    pub const ORDER: [Order; 6] = [
        Order::TransformSrt,
        Order::TransformStr,
        Order::TransformRst,
        Order::TransformRts,
        Order::TransformTsr,
        Order::TransformTrs,
    ];
    pub const ROTATION_ORDER_LABELS: [&str; 6] = ["XYZ", "XZY", "YXZ", "YZX", "ZXY", "ZYX"];
    pub const ROTATION_ORDER: [Rot; 6] = [
        Rot::TransformXyz,
        Rot::TransformXzy,
        Rot::TransformYxz,
        Rot::TransformYzx,
        Rot::TransformZxy,
        Rot::TransformZyx,
    ];
    pub const METRIC_LABELS: [&str; 3] = ["Radius", "Edge distance", "Attribute"];
    pub const METRIC: [SoftTransformMetric; 3] =
        [SoftTransformMetric::Radius, SoftTransformMetric::Edge, SoftTransformMetric::Attribute];

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub group_owner: rdk::group_ops::Owner,
        pub group: String,
        pub order: Order,
        pub rotation_order: Rot,
        pub translate: [f64; 3],
        pub rotate: [f64; 3],
        pub scale: [f64; 3],
        pub shear: [f64; 3],
        pub uniform_scale: f64,
        pub pivot: [f64; 3],
        pub pivot_rotation: [f64; 3],
        pub invert: bool,
        pub metric: SoftTransformMetric,
        pub metric_attribute: String,
        pub apply_rolloff: bool,
        pub falloff: SoftTransformFalloff,
        pub radius: f64,
        pub falloff_attribute: String,
        pub recompute_normals: bool,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        let mut r = Reader::new(values);
        let triple = |r: &mut Reader| [r.float(), r.float(), r.float()];
        Parameters {
            group_owner: support::element_owner_of_value(r.next()),
            group: r.text(),
            order: ORDER[r.choice(&ORDER_LABELS)],
            rotation_order: ROTATION_ORDER[r.choice(&ROTATION_ORDER_LABELS)],
            translate: triple(&mut r),
            rotate: triple(&mut r),
            scale: triple(&mut r),
            shear: triple(&mut r),
            uniform_scale: r.float(),
            pivot: triple(&mut r),
            pivot_rotation: triple(&mut r),
            invert: r.bool(),
            metric: METRIC[r.choice(&METRIC_LABELS)],
            metric_attribute: r.text(),
            apply_rolloff: r.bool(),
            falloff: support::soft_falloff_of_value(r.next()),
            radius: r.float(),
            falloff_attribute: r.text(),
            recompute_normals: r.bool(),
        }
    }

    pub const META: Meta = Meta {
        key: "soft_transform",
        label: "Soft Transform",
        category: "Deform",
        inputs: 1,
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        let pi = std::f64::consts::PI;
        let mut specs = vec![
            owner_spec("Selection"),
            txt("group", "Group", "Selection", ""),
            pick("order", "Transform order", "Transform", support::choice_kind(&ORDER_LABELS), support::choice_value(&ORDER_LABELS, 0)),
            pick(
                "rotation_order",
                "Rotation order",
                "Transform/Rotate",
                support::choice_kind(&ROTATION_ORDER_LABELS),
                support::choice_value(&ROTATION_ORDER_LABELS, 0),
            ),
        ];
        specs.extend(xyz_specs(
            ["translate_x", "translate_y", "translate_z"],
            ["Translate X", "Translate Y", "Translate Z"],
            "Transform/Translate",
            10.0,
            "translate",
            [0.0; 3],
        ));
        specs.extend(xyz_specs(
            ["rotate_x", "rotate_y", "rotate_z"],
            ["Rotate X", "Rotate Y", "Rotate Z"],
            "Transform/Rotate",
            pi,
            "rotate",
            [0.0; 3],
        ));
        specs.extend(xyz_specs(
            ["scale_x", "scale_y", "scale_z"],
            ["Scale X", "Scale Y", "Scale Z"],
            "Transform/Scale",
            10.0,
            "scale",
            [1.0; 3],
        ));
        specs.extend([
            fl("shear_xy", "Shear XY", "Transform/Shear", -4.0, 4.0, 0.0),
            fl("shear_xz", "Shear XZ", "Transform/Shear", -4.0, 4.0, 0.0),
            fl("shear_yz", "Shear YZ", "Transform/Shear", -4.0, 4.0, 0.0),
            fl("uniform_scale", "Uniform scale", "Transform", -10.0, 10.0, 1.0),
        ]);
        specs.extend(xyz_specs(
            ["pivot_x", "pivot_y", "pivot_z"],
            ["Pivot X", "Pivot Y", "Pivot Z"],
            "Transform/Pivot",
            10.0,
            "pivot",
            [0.0; 3],
        ));
        specs.extend(xyz_specs(
            ["pivot_rotation_x", "pivot_rotation_y", "pivot_rotation_z"],
            ["Pivot rotate X", "Pivot rotate Y", "Pivot rotate Z"],
            "Transform/Pivot rotation",
            pi,
            "pivot_rotation",
            [0.0; 3],
        ));
        specs.extend([
            tog("invert", "Invert transform", "Transform", false),
            pick(
                "metric",
                "Distance metric",
                "Soft selection",
                support::choice_kind(&METRIC_LABELS),
                support::choice_value(&METRIC_LABELS, 0),
            ),
            txt("metric_attribute", "Metric attribute", "Soft selection", "mask"),
            tog("apply_rolloff", "Apply rolloff", "Soft selection", true),
            pick(
                "falloff",
                "Falloff",
                "Soft selection",
                support::soft_falloff_kind(),
                support::choice_value(&support::SOFT_FALLOFF_LABELS, 2),
            ),
            fl_hard("radius", "Radius", "Soft selection", 0.0, 100.0, 0.0, 1.0),
            txt("falloff_attribute", "Falloff output attribute", "Output", ""),
            tog("recompute_normals", "Recompute normals", "Output", true),
        ]);
        specs
    }

    fn compose_params(p: &Parameters) -> transform_ops::ComposeTransformParams {
        let v = |a: [f64; 3]| vec3(a[0], a[1], a[2]);
        transform_ops::ComposeTransformParams {
            order: p.order,
            rotation_order: p.rotation_order,
            translate: v(p.translate),
            rotate: v(p.rotate),
            scale: v(p.scale),
            shear: v(p.shear),
            uniform_scale: p.uniform_scale,
            pivot: v(p.pivot),
            pivot_rotation: v(p.pivot_rotation),
            invert: p.invert,
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        let refuse = |message: &str| Err(format!("sop/soft_transform_trs: {message}"));
        if !p.radius.is_finite() || p.radius < 0.0 {
            return refuse("radius must be finite and nonnegative");
        }
        if p.radius == 0.0 && (p.metric != SoftTransformMetric::Attribute || p.apply_rolloff) {
            return refuse("rolloff radius must be positive");
        }
        if p.metric == SoftTransformMetric::Attribute && trim(&p.metric_attribute).is_empty() {
            return refuse("metric attribute must be nonblank");
        }
        if p.metric == SoftTransformMetric::Attribute && p.metric_attribute == "P" {
            return refuse("metric attribute requires scalar storage");
        }
        if trim(&p.falloff_attribute) == "P" {
            return refuse("falloff output must not replace P");
        }
        match transform_ops::compose_transform(&compose_params(&p)) {
            Err(error) => refuse(&rdk::error::to_string(&error)),
            Ok(matrix) => {
                let finite = (0..16).all(|i| rays_math::mat4::get(&matrix, i / 4, i % 4).is_ok_and(f64::is_finite));
                if finite { Ok(()) } else { refuse("composed transform must be finite") }
            }
        }
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let selection = support::optional_element_group(p.group_owner, &p.group);
        let falloff_attribute = support::optional_text(&p.falloff_attribute);
        let metric = match p.metric {
            SoftTransformMetric::Radius => Metric::SoftRadius,
            SoftTransformMetric::Edge => Metric::SoftEdge,
            SoftTransformMetric::Attribute => {
                Metric::SoftAttribute { attribute: p.metric_attribute.clone(), apply_rolloff: p.apply_rolloff }
            }
        };
        let matrix = transform_ops::compose_transform(&compose_params(&p)).expect("soft_transform: validated transform");
        let (falloff, radius, recompute_normals) = (p.falloff, p.radius, p.recompute_normals);
        let made = build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                cook_mode: CookMode::DuplicateInput(0),
                dependencies: dependencies::STATIC,
                inputs: vec![inputs[0].clone().unwrap()],
                ..build::args("soft_transform")
            },
            None,
            Arc::new(move |_, context, inputs| {
                let selection = support::resolve_element_group("soft_transform", &selection, &inputs[0])?;
                let params = transform_ops::SoftTransformParams {
                    cancel: Some(Arc::clone(context::cancel_token(context))),
                    grain: Some(context::grain(context)),
                    selection,
                    metric: metric.clone(),
                    falloff,
                    radius,
                    falloff_attribute: falloff_attribute.clone(),
                    recompute_normals,
                };
                match transform_ops::soft_transform(&params, &matrix, &inputs[0]) {
                    Ok(geometry) => support::cooked(geometry),
                    Err(error) => Err(support::structured_rdk_error(&error)),
                }
            }),
        );
        // [@@sop.node_facts]: the attribute names come from the parameters.
        let named = |extra: &str| -> Vec<String> {
            ["P", "N", extra].into_iter().filter(|n| !trim(n).is_empty()).map(str::to_owned).collect()
        };
        let facts = node::Facts {
            elementwise: Elementwise::Points,
            reads: named(&p.metric_attribute),
            writes: named(&p.falloff_attribute),
            topology: Topology::Preserved,
            exact: false,
            ..node::facts(&made).clone()
        };
        Ok(build::with_facts(facts, &made))
    }

    pub fn decl() -> Decl {
        Decl { meta: META, schema: decl::schema(&META, specs()), operator, dynamic_schema: None }
    }

    pub fn factory() -> Factory {
        decl::factory(&decl())
    }
}

pub mod ray {
    use super::*;
    use rdk::ray::{Combine, Direction, Method};
    use rdk::surface_index::{RayDirectionMode, RaySurfaceHit};
    use support::RayDirection;

    pub const METHOD_LABELS: [&str; 2] = ["Minimum distance", "Project rays"];
    pub const METHOD: [Method; 2] = [Method::RayMinimumDistance, Method::RayProject];
    pub const DIRECTION_LABELS: [&str; 3] = ["Vector", "Normal", "Attribute"];
    pub const DIRECTION: [RayDirection; 3] = [RayDirection::Vector, RayDirection::Normal, RayDirection::Attribute];
    pub const DIRECTION_MODE_LABELS: [&str; 4] =
        ["Forward", "Reverse", "Bidirectional closest", "Bidirectional farthest"];
    pub const DIRECTION_MODE: [RayDirectionMode; 4] = [
        RayDirectionMode::RayForward,
        RayDirectionMode::RayReverse,
        RayDirectionMode::RayBidirectionalClosest,
        RayDirectionMode::RayBidirectionalFarthest,
    ];
    pub const SURFACE_LABELS: [&str; 2] = ["First surface", "Last surface"];
    pub const SURFACE: [RaySurfaceHit; 2] = [RaySurfaceHit::RayFirstSurface, RaySurfaceHit::RayLastSurface];
    pub const COMBINE_LABELS: [&str; 4] = ["Average", "Median", "Shortest", "Longest"];
    pub const COMBINE: [Combine; 4] =
        [Combine::RayAverage, Combine::RayMedian, Combine::RayShortest, Combine::RayLongest];

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub group_owner: rdk::group_ops::Owner,
        pub group: String,
        pub collision_group: String,
        pub method: Method,
        pub direction: RayDirection,
        pub direction_vector: [f64; 3],
        pub direction_attribute: String,
        pub direction_mode: RayDirectionMode,
        pub surface_hit: RaySurfaceHit,
        pub samples: i64,
        pub jitter_scale: f64,
        pub seed: i64,
        pub combine: Combine,
        pub min_distance: f64,
        pub limit_max_distance: bool,
        pub max_distance: f64,
        pub tolerance: f64,
        pub scale: f64,
        pub lift: f64,
        pub distance_attribute: String,
        pub primitive_attribute: String,
        pub source_vertex_numbers_attribute: String,
        pub source_vertex_weights_attribute: String,
        pub hit_group: String,
        pub normal_attribute: String,
        pub point_pattern: String,
        pub vertex_pattern: String,
        pub primitive_pattern: String,
        pub detail_pattern: String,
        pub match_groups: bool,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        let mut r = Reader::new(values);
        Parameters {
            group_owner: support::element_owner_of_value(r.next()),
            group: r.text(),
            collision_group: r.text(),
            method: METHOD[r.choice(&METHOD_LABELS)],
            direction: DIRECTION[r.choice(&DIRECTION_LABELS)],
            direction_vector: [r.float(), r.float(), r.float()],
            direction_attribute: r.text(),
            direction_mode: DIRECTION_MODE[r.choice(&DIRECTION_MODE_LABELS)],
            surface_hit: SURFACE[r.choice(&SURFACE_LABELS)],
            samples: r.int(),
            jitter_scale: r.float(),
            seed: r.int(),
            combine: COMBINE[r.choice(&COMBINE_LABELS)],
            min_distance: r.float(),
            limit_max_distance: r.bool(),
            max_distance: r.float(),
            tolerance: r.float(),
            scale: r.float(),
            lift: r.float(),
            distance_attribute: r.text(),
            primitive_attribute: r.text(),
            source_vertex_numbers_attribute: r.text(),
            source_vertex_weights_attribute: r.text(),
            hit_group: r.text(),
            normal_attribute: r.text(),
            point_pattern: r.text(),
            vertex_pattern: r.text(),
            primitive_pattern: r.text(),
            detail_pattern: r.text(),
            match_groups: r.bool(),
        }
    }

    pub const META: Meta = Meta {
        key: "ray",
        label: "Ray",
        category: "Modify/Project",
        inputs: 2,
        slots: Some("source, collision"),
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        let int = |name, label, folder, min, max, hard_min, default| Spec {
            label,
            folder,
            ..decl::spec(name, decl::int_kind(min, max, hard_min, None), Value::Int(default))
        };
        let mut specs = vec![
            owner_spec(""),
            txt("group", "Group", "", ""),
            txt("collision_group", "Collision primitive group", "", ""),
            pick("method_", "Method", "", support::choice_kind(&METHOD_LABELS), support::choice_value(&METHOD_LABELS, 1)),
            pick("direction", "Direction", "Ray", support::choice_kind(&DIRECTION_LABELS), support::choice_value(&DIRECTION_LABELS, 1)),
        ];
        specs.extend(xyz_specs(
            ["direction_x", "direction_y", "direction_z"],
            ["Direction X", "Direction Y", "Direction Z"],
            "Ray/Vector",
            1.0,
            "direction_vector",
            [0.0, 1.0, 0.0],
        ));
        specs.extend([
            txt("direction_attribute", "Direction attribute", "Ray", "N"),
            pick(
                "direction_mode",
                "Direction mode",
                "Ray",
                support::choice_kind(&DIRECTION_MODE_LABELS),
                support::choice_value(&DIRECTION_MODE_LABELS, 0),
            ),
            pick("surface_hit", "Surface hit", "Ray", support::choice_kind(&SURFACE_LABELS), support::choice_value(&SURFACE_LABELS, 0)),
            int("samples", "Samples", "Jitter", 1, 1024, Some(1), 1),
            fl_hard("jitter_scale", "Jitter scale", "Jitter", 0.0, 10.0, 0.0, 1.0),
            int("seed", "Seed", "Jitter", 0, 999999, None, 0),
            pick("combine", "Combine", "Jitter", support::choice_kind(&COMBINE_LABELS), support::choice_value(&COMBINE_LABELS, 0)),
            fl_hard("min_distance", "Minimum distance", "Distance", 0.0, 1000.0, 0.0, 0.0),
            tog("limit_max_distance", "Limit maximum distance", "Distance", false),
            fl_hard("max_distance", "Maximum distance", "Distance", 0.0, 1000.0, 0.0, 10.0),
            fl_hard("tolerance", "Tolerance", "Distance", 0.0, 1.0, 0.0, 0.0),
            fl("scale", "Scale", "Transform", -10.0, 10.0, 1.0),
            fl("lift", "Lift", "Transform", -10.0, 10.0, 0.0),
            txt("distance_attribute", "Distance", "Output", ""),
            txt("primitive_attribute", "Primitive", "Output", ""),
            txt("source_vertex_numbers_attribute", "Source vertices", "Output", ""),
            txt("source_vertex_weights_attribute", "Source weights", "Output", ""),
            txt("hit_group", "Hit group", "Output", ""),
            txt("normal_attribute", "Hit normal", "Output", ""),
            txt("point_pattern", "Point attributes", "Transfer", ""),
            txt("vertex_pattern", "Vertex attributes", "Transfer", ""),
            txt("primitive_pattern", "Primitive attributes", "Transfer", ""),
            txt("detail_pattern", "Detail attributes", "Transfer", ""),
            tog("match_groups", "Match groups", "Transfer", false),
        ]);
        specs
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        let refuse = |message: &str| Err(format!("sop/ray: {message}"));
        let [dx, dy, dz] = p.direction_vector;
        if !all_finite(&[dx, dy, dz, p.jitter_scale, p.min_distance, p.max_distance, p.tolerance, p.scale, p.lift]) {
            return refuse("numeric controls must be finite");
        }
        if p.samples < 1 || p.samples > 1024 {
            return refuse("samples must be in [1, 1024]");
        }
        if p.method == Method::RayMinimumDistance && p.samples != 1 {
            return refuse("multiple samples require projection");
        }
        if p.jitter_scale < 0.0
            || p.min_distance < 0.0
            || p.max_distance < 0.0
            || p.tolerance < 0.0
            || p.tolerance > f64::MAX.sqrt()
        {
            return refuse("distance and jitter controls are out of range");
        }
        if p.limit_max_distance && p.max_distance < p.min_distance {
            return refuse("maximum must be at least minimum distance");
        }
        if p.limit_max_distance && p.method == Method::RayMinimumDistance && p.max_distance > f64::MAX.sqrt() {
            return refuse("closest distance must be safely squarable");
        }
        if p.method == Method::RayProject {
            match p.direction {
                RayDirection::Vector if dx == 0.0 && dy == 0.0 && dz == 0.0 => {
                    return refuse("direction must be nonzero");
                }
                RayDirection::Attribute if trim(&p.direction_attribute).is_empty() => {
                    return refuse("direction attribute must be nonblank");
                }
                _ => {}
            }
        }
        let numbers = support::optional_text(&p.source_vertex_numbers_attribute);
        let weights = support::optional_text(&p.source_vertex_weights_attribute);
        if numbers.is_some() != weights.is_some() {
            return refuse("vertex numbers and weights must be requested together");
        }
        let names: Vec<String> = [
            &p.distance_attribute,
            &p.primitive_attribute,
            &p.source_vertex_numbers_attribute,
            &p.source_vertex_weights_attribute,
            &p.normal_attribute,
        ]
        .into_iter()
        .filter_map(|n| support::optional_text(n))
        .collect();
        if names.iter().any(|n| n == "P") {
            return refuse("output must not replace P");
        }
        let mut unique = names.clone();
        unique.sort();
        unique.dedup();
        if unique.len() != names.len() {
            return refuse("output names must be distinct");
        }
        let patterns = [&p.point_pattern, &p.vertex_pattern, &p.primitive_pattern, &p.detail_pattern];
        for pattern in patterns {
            if !trim(pattern).is_empty() {
                support::validate_parameter_pattern("sop/ray", pattern)?;
            }
        }
        if p.match_groups && [&p.point_pattern, &p.vertex_pattern, &p.primitive_pattern].iter().all(|n| trim(n).is_empty()) {
            return refuse("group interpolation requires a pattern");
        }
        Ok(())
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let selection = support::optional_element_group(p.group_owner, &p.group);
        let collision_group = support::optional_text(&p.collision_group);
        let direction = match p.direction {
            RayDirection::Vector => {
                Direction::RayVector(vec3(p.direction_vector[0], p.direction_vector[1], p.direction_vector[2]))
            }
            RayDirection::Normal => Direction::RayNormal,
            RayDirection::Attribute => Direction::RayAttribute(p.direction_attribute.clone()),
        };
        let max_distance = if p.limit_max_distance { Some(p.max_distance) } else { None };
        let text = |s: &String| support::optional_text(s);
        Ok(build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                version: 2,
                cook_mode: CookMode::DuplicateInput(0),
                dependencies: dependencies::STATIC,
                inputs: vec![inputs[0].clone().unwrap(), inputs[1].clone().unwrap()],
                ..build::args("ray")
            },
            None,
            Arc::new(move |_, context, inputs| {
                let selection = support::resolve_element_group("ray", &selection, &inputs[0])?;
                let collision_primitives = match &collision_group {
                    None => None,
                    Some(name) => Some(
                        rdk::geometry::find_group(rdk::group::Owner::Primitive, name, &inputs[1]).ok_or_else(|| {
                            support::diagnostic(
                                "missing_collision_group",
                                &format!("ray could not find collision primitive group {}", node::ocaml_quote(name)),
                            )
                        })?,
                    ),
                };
                let params = rdk::ray::Params {
                    grain: context::grain(context),
                    selection,
                    collision_primitives,
                    method: p.method,
                    direction: direction.clone(),
                    direction_mode: p.direction_mode,
                    surface_hit: p.surface_hit,
                    samples: p.samples,
                    jitter_scale: p.jitter_scale,
                    seed: p.seed,
                    combine: p.combine,
                    min_distance: p.min_distance,
                    max_distance,
                    tolerance: p.tolerance,
                    scale: p.scale,
                    lift: p.lift,
                    distance_attribute: text(&p.distance_attribute),
                    primitive_attribute: text(&p.primitive_attribute),
                    source_vertex_numbers_attribute: text(&p.source_vertex_numbers_attribute),
                    source_vertex_weights_attribute: text(&p.source_vertex_weights_attribute),
                    hit_group: text(&p.hit_group),
                    normal_attribute: text(&p.normal_attribute),
                    point_pattern: text(&p.point_pattern),
                    vertex_pattern: text(&p.vertex_pattern),
                    primitive_pattern: text(&p.primitive_pattern),
                    detail_pattern: text(&p.detail_pattern),
                    match_groups: p.match_groups,
                };
                match rdk::ray::run(Some(context::cancel_token(context).as_ref()), &params, &inputs[0], &inputs[1]) {
                    Ok(geometry) => support::cooked(geometry),
                    Err(error) => Err(support::structured_rdk_error(&error)),
                }
            }),
        ))
    }

    pub fn decl() -> Decl {
        Decl { meta: META, schema: decl::schema(&META, specs()), operator, dynamic_schema: None }
    }

    pub fn factory() -> Factory {
        decl::factory(&decl())
    }
}
