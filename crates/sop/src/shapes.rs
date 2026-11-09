//! Port of lib/sop/shapes.ml: shape nodes, declared once. This wave ports `null` and `switch`, the
//! worked examples of PORTING.md; clip, exploded_view, pack_instances, soft_transform and ray follow.
use std::sync::Arc;

use param::Value;
use rays_diagnostic::Diagnostic;

use crate::context::{self, dependencies};
use crate::decl::{self, Decl, Meta, Spec};
use crate::edit_graph::Factory;
use crate::node::{self, build, CookMode, InputPolicy};
use crate::support::{self, Choice};

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

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

pub mod switch {
    use super::*;

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
        let index = match &values[0] { Value::Int(input) => *input, _ => unreachable!("switch: parameter record has the wrong shape") };
        let nodes = decl::connected(inputs);
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

/// Borrowed positional reader over a schema-normalised value slice.
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
            _ => unreachable!("parameter record has the wrong shape"),
        }
    }
    fn int(&mut self) -> i64 {
        match self.next() {
            Value::Int(i) => *i,
            _ => unreachable!("parameter record has the wrong shape"),
        }
    }
    fn bool(&mut self) -> bool {
        match self.next() {
            Value::Bool(b) => *b,
            _ => unreachable!("parameter record has the wrong shape"),
        }
    }
    fn text(&mut self) -> String {
        match self.next() {
            Value::Text(t) => String::from_utf8_lossy(t).into_owned(),
            _ => unreachable!("parameter record has the wrong shape"),
        }
    }
    fn choice<T: Clone>(&mut self, table: &support::Choice<T>) -> T {
        table.of(self.next())
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
        support::ELEMENT_OWNER.kind(),
        support::ELEMENT_OWNER.value(&rdk_ops::group_ops::Owner::GroupPoints),
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
    use rdk_ops::plane_clip::{self, Keep};

    pub const KEEP: Choice<Keep> = Choice(&[
        ("Above", Keep::Above),
        ("Below", Keep::Below),
        ("All", Keep::All),
    ]);

    pub const META: Meta =
        Meta { key: "clip", label: "Clip", category: "Modify", inputs: 1, validate: Some(validate), ..decl::META };

    pub fn specs() -> Vec<Spec> {
        let mut specs = vec![
            owner_spec("Selection"),
            txt("group", "Group", "Selection", ""),
            pick("keep", "Keep", "", KEEP.kind(), KEEP.at(0)),
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
        let mut r = Reader::new(values);
        r.at += 2;
        let p_keep = r.choice(&KEEP);
        let p_snapping_tolerance = r.float();
        r.at += 1;
        let p_split_connectivity = r.bool();
        let p_distance = r.float();
        let p_origin = [r.float(), r.float(), r.float()];
        let p_normal = [r.float(), r.float(), r.float()];
        r.at += 2;
        let p_cap_group = r.text();
        let p_clipped_group = r.text();
        let p_above_group = r.text();
        let p_below_group = r.text();
        let [ox, oy, oz] = p_origin;
        let [nx, ny, nz] = p_normal;
        if !all_finite(&[ox, oy, oz, nx, ny, nz, p_distance, p_snapping_tolerance]) {
            return Err("sop/clip: plane and tolerance must be finite".to_owned());
        }
        if p_snapping_tolerance < 0.0 {
            return Err("sop/clip: snapping tolerance must be non-negative".to_owned());
        }
        if p_split_connectivity && p_keep != Keep::All {
            return Err("sop/clip: split connectivity requires keep=All".to_owned());
        }
        if nx == 0.0 && ny == 0.0 && nz == 0.0 {
            return Err("sop/clip: normal must be non-zero".to_owned());
        }
        let names: Vec<String> = [&p_cap_group, &p_clipped_group, &p_above_group, &p_below_group]
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
        let mut r = Reader::new(values);
        let p_group_owner = support::ELEMENT_OWNER.of(r.next());
        let p_group = r.text();
        let p_keep = r.choice(&KEEP);
        let p_snapping_tolerance = r.float();
        let p_fill = r.bool();
        let p_split_connectivity = r.bool();
        let p_distance = r.float();
        let p_origin = [r.float(), r.float(), r.float()];
        let p_normal = [r.float(), r.float(), r.float()];
        let p_clip_attribute = r.text();
        let p_clipped_edge_group = r.text();
        let p_cap_group = r.text();
        let p_clipped_group = r.text();
        let p_above_group = r.text();
        let p_below_group = r.text();
        let p_replace_existing_groups = r.bool();
        let selection = support::optional_element_group(p_group_owner, &p_group);
        let clip_attribute = support::optional_text(&p_clip_attribute).unwrap_or_else(|| "P".to_owned());
        let origin = support::vec3_copy(&vec3(p_origin[0], p_origin[1], p_origin[2]));
        let normal = support::vec3_copy(&vec3(p_normal[0], p_normal[1], p_normal[2]));
        let clipped_edge_group = support::optional_text(&p_clipped_edge_group);
        let cap_group = support::optional_text(&p_cap_group);
        let clipped_group = support::optional_text(&p_clipped_group);
        let above_group = support::optional_text(&p_above_group);
        let below_group = support::optional_text(&p_below_group);
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
                    keep: p_keep,
                    snapping_tolerance: p_snapping_tolerance,
                    fill: p_fill,
                    split_connectivity: p_split_connectivity,
                    clip_attribute: clip_attribute.clone(),
                    distance: p_distance,
                    selection,
                    replace_existing_groups: p_replace_existing_groups,
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

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
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

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

/// The packed result a Lisp body cannot say on its own: the source unchanged, drawn at the affine rows
/// r0, r1, r2 (Point Float4 attributes of the second input, one point per instance).
pub mod pack_instances {
    use super::*;
    use rdk_core::attribute::{self, Storage};
    use rdk_core::{geometry, packed};

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

    fn row(name: &str, instances: &geometry::T) -> Result<packed::float4::view::View, Diagnostic> {
        let invalid = |message: String| support::diagnostic("invalid_attribute", &message);
        match geometry::find_attribute(attribute::Owner::Point, name, instances) {
            Some(found) => match attribute::storage(&found).map_err(|e| support::structured_rdk_error(&e))? {
                Storage::Float4(values) => Ok(packed::float4::view::view(&values)),
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

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

pub mod soft_transform {
    use super::*;
    use crate::node::{Elementwise, Topology};
    use rdk_ops::transform_ops::{
        self, SoftTransformMetric as Metric, TransformOrder as Order, TransformRotationOrder as Rot,
    };
    use support::SoftTransformMetric;

    pub const ORDER: Choice<Order> = Choice(&[
        ("SRT", Order::TransformSrt),
        ("STR", Order::TransformStr),
        ("RST", Order::TransformRst),
        ("RTS", Order::TransformRts),
        ("TSR", Order::TransformTsr),
        ("TRS", Order::TransformTrs),
    ]);
    pub const ROTATION_ORDER: Choice<Rot> = Choice(&[
        ("XYZ", Rot::TransformXyz),
        ("XZY", Rot::TransformXzy),
        ("YXZ", Rot::TransformYxz),
        ("YZX", Rot::TransformYzx),
        ("ZXY", Rot::TransformZxy),
        ("ZYX", Rot::TransformZyx),
    ]);
    pub const METRIC: Choice<SoftTransformMetric> = Choice(&[
        ("Radius", SoftTransformMetric::Radius),
        ("Edge distance", SoftTransformMetric::Edge),
        ("Attribute", SoftTransformMetric::Attribute),
    ]);

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
            pick("order", "Transform order", "Transform", ORDER.kind(), ORDER.at(0)),
            pick(
                "rotation_order",
                "Rotation order",
                "Transform/Rotate",
                ROTATION_ORDER.kind(),
                ROTATION_ORDER.at(0),
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
                METRIC.kind(),
                METRIC.at(0),
            ),
            txt("metric_attribute", "Metric attribute", "Soft selection", "mask"),
            tog("apply_rolloff", "Apply rolloff", "Soft selection", true),
            pick(
                "falloff",
                "Falloff",
                "Soft selection",
                support::SOFT_FALLOFF.kind(),
                support::SOFT_FALLOFF.at(2),
            ),
            fl_hard("radius", "Radius", "Soft selection", 0.0, 100.0, 0.0, 1.0),
            txt("falloff_attribute", "Falloff output attribute", "Output", ""),
            tog("recompute_normals", "Recompute normals", "Output", true),
        ]);
        specs
    }

    fn compose_params(values: &[Value]) -> transform_ops::ComposeTransformParams {
        let mut r = Reader::new(values);
        r.at += 2;
        let p_order = r.choice(&ORDER);
        let p_rotation_order = r.choice(&ROTATION_ORDER);
        let p_translate = [r.float(), r.float(), r.float()];
        let p_rotate = [r.float(), r.float(), r.float()];
        let p_scale = [r.float(), r.float(), r.float()];
        let p_shear = [r.float(), r.float(), r.float()];
        let p_uniform_scale = r.float();
        let p_pivot = [r.float(), r.float(), r.float()];
        let p_pivot_rotation = [r.float(), r.float(), r.float()];
        let p_invert = r.bool();
        let v = |a: [f64; 3]| vec3(a[0], a[1], a[2]);
        transform_ops::ComposeTransformParams {
            order: p_order,
            rotation_order: p_rotation_order,
            translate: v(p_translate),
            rotate: v(p_rotate),
            scale: v(p_scale),
            shear: v(p_shear),
            uniform_scale: p_uniform_scale,
            pivot: v(p_pivot),
            pivot_rotation: v(p_pivot_rotation),
            invert: p_invert,
        }
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let mut r = Reader::new(values);
        r.at += 24;
        let p_metric = r.choice(&METRIC);
        let p_metric_attribute = r.text();
        let p_apply_rolloff = r.bool();
        r.at += 1;
        let p_radius = r.float();
        let p_falloff_attribute = r.text();
        let refuse = |message: &str| Err(format!("sop/soft_transform_trs: {message}"));
        if !p_radius.is_finite() || p_radius < 0.0 {
            return refuse("radius must be finite and nonnegative");
        }
        if p_radius == 0.0 && (p_metric != SoftTransformMetric::Attribute || p_apply_rolloff) {
            return refuse("rolloff radius must be positive");
        }
        if p_metric == SoftTransformMetric::Attribute && trim(&p_metric_attribute).is_empty() {
            return refuse("metric attribute must be nonblank");
        }
        if p_metric == SoftTransformMetric::Attribute && p_metric_attribute == "P" {
            return refuse("metric attribute requires scalar storage");
        }
        if trim(&p_falloff_attribute) == "P" {
            return refuse("falloff output must not replace P");
        }
        match transform_ops::compose_transform(&compose_params(values)) {
            Err(error) => refuse(&rdk_core::error::to_string(&error)),
            Ok(matrix) => {
                let finite = (0..16).all(|i| rays_math::mat4::get(&matrix, i / 4, i % 4).is_ok_and(f64::is_finite));
                if finite { Ok(()) } else { refuse("composed transform must be finite") }
            }
        }
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let mut r = Reader::new(values);
        let p_group_owner = support::ELEMENT_OWNER.of(r.next());
        let p_group = r.text();
        r.at += 22;
        let p_metric = r.choice(&METRIC);
        let p_metric_attribute = r.text();
        let p_apply_rolloff = r.bool();
        let p_falloff = support::SOFT_FALLOFF.of(r.next());
        let p_radius = r.float();
        let p_falloff_attribute = r.text();
        let p_recompute_normals = r.bool();
        let selection = support::optional_element_group(p_group_owner, &p_group);
        let falloff_attribute = support::optional_text(&p_falloff_attribute);
        let metric = match p_metric {
            SoftTransformMetric::Radius => Metric::SoftRadius,
            SoftTransformMetric::Edge => Metric::SoftEdge,
            SoftTransformMetric::Attribute => {
                Metric::SoftAttribute { attribute: p_metric_attribute.clone(), apply_rolloff: p_apply_rolloff }
            }
        };
        let matrix = transform_ops::compose_transform(&compose_params(values)).expect("soft_transform: validated transform");
        let (falloff, radius, recompute_normals) = (p_falloff, p_radius, p_recompute_normals);
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
            reads: named(&p_metric_attribute),
            writes: named(&p_falloff_attribute),
            topology: Topology::Preserved,
            exact: false,
            ..node::facts(&made).clone()
        };
        Ok(build::with_facts(facts, &made))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

pub mod ray {
    use super::*;
    use rdk_ops::ray::{Combine, Direction, Method};
    use rdk_geom::surface_index::{RayDirectionMode, RaySurfaceHit};
    use support::RayDirection;

    pub const METHOD: Choice<Method> = Choice(&[
        ("Minimum distance", Method::RayMinimumDistance),
        ("Project rays", Method::RayProject),
    ]);
    pub const DIRECTION: Choice<RayDirection> = Choice(&[
        ("Vector", RayDirection::Vector),
        ("Normal", RayDirection::Normal),
        ("Attribute", RayDirection::Attribute),
    ]);
    pub const DIRECTION_MODE: Choice<RayDirectionMode> = Choice(&[
        ("Forward", RayDirectionMode::RayForward),
        ("Reverse", RayDirectionMode::RayReverse),
        ("Bidirectional closest", RayDirectionMode::RayBidirectionalClosest),
        ("Bidirectional farthest", RayDirectionMode::RayBidirectionalFarthest),
    ]);
    pub const SURFACE: Choice<RaySurfaceHit> = Choice(&[
        ("First surface", RaySurfaceHit::RayFirstSurface),
        ("Last surface", RaySurfaceHit::RayLastSurface),
    ]);
    pub const COMBINE: Choice<Combine> = Choice(&[
        ("Average", Combine::RayAverage),
        ("Median", Combine::RayMedian),
        ("Shortest", Combine::RayShortest),
        ("Longest", Combine::RayLongest),
    ]);

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
            pick("method_", "Method", "", METHOD.kind(), METHOD.at(1)),
            pick("direction", "Direction", "Ray", DIRECTION.kind(), DIRECTION.at(1)),
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
                DIRECTION_MODE.kind(),
                DIRECTION_MODE.at(0),
            ),
            pick("surface_hit", "Surface hit", "Ray", SURFACE.kind(), SURFACE.at(0)),
            int("samples", "Samples", "Jitter", 1, 1024, Some(1), 1),
            fl_hard("jitter_scale", "Jitter scale", "Jitter", 0.0, 10.0, 0.0, 1.0),
            int("seed", "Seed", "Jitter", 0, 999999, None, 0),
            pick("combine", "Combine", "Jitter", COMBINE.kind(), COMBINE.at(0)),
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
        let mut r = Reader::new(values);
        r.at += 3;
        let p_method = r.choice(&METHOD);
        let p_direction = r.choice(&DIRECTION);
        let p_direction_vector = [r.float(), r.float(), r.float()];
        let p_direction_attribute = r.text();
        r.at += 2;
        let p_samples = r.int();
        let p_jitter_scale = r.float();
        r.at += 2;
        let p_min_distance = r.float();
        let p_limit_max_distance = r.bool();
        let p_max_distance = r.float();
        let p_tolerance = r.float();
        let p_scale = r.float();
        let p_lift = r.float();
        let p_distance_attribute = r.text();
        let p_primitive_attribute = r.text();
        let p_source_vertex_numbers_attribute = r.text();
        let p_source_vertex_weights_attribute = r.text();
        r.at += 1;
        let p_normal_attribute = r.text();
        let p_point_pattern = r.text();
        let p_vertex_pattern = r.text();
        let p_primitive_pattern = r.text();
        let p_detail_pattern = r.text();
        let p_match_groups = r.bool();
        let refuse = |message: &str| Err(format!("sop/ray: {message}"));
        let [dx, dy, dz] = p_direction_vector;
        if !all_finite(&[dx, dy, dz, p_jitter_scale, p_min_distance, p_max_distance, p_tolerance, p_scale, p_lift]) {
            return refuse("numeric controls must be finite");
        }
        if p_samples < 1 || p_samples > 1024 {
            return refuse("samples must be in [1, 1024]");
        }
        if p_method == Method::RayMinimumDistance && p_samples != 1 {
            return refuse("multiple samples require projection");
        }
        if p_jitter_scale < 0.0
            || p_min_distance < 0.0
            || p_max_distance < 0.0
            || p_tolerance < 0.0
            || p_tolerance > f64::MAX.sqrt()
        {
            return refuse("distance and jitter controls are out of range");
        }
        if p_limit_max_distance && p_max_distance < p_min_distance {
            return refuse("maximum must be at least minimum distance");
        }
        if p_limit_max_distance && p_method == Method::RayMinimumDistance && p_max_distance > f64::MAX.sqrt() {
            return refuse("closest distance must be safely squarable");
        }
        if p_method == Method::RayProject {
            match p_direction {
                RayDirection::Vector if dx == 0.0 && dy == 0.0 && dz == 0.0 => {
                    return refuse("direction must be nonzero");
                }
                RayDirection::Attribute if trim(&p_direction_attribute).is_empty() => {
                    return refuse("direction attribute must be nonblank");
                }
                _ => {}
            }
        }
        let numbers = support::optional_text(&p_source_vertex_numbers_attribute);
        let weights = support::optional_text(&p_source_vertex_weights_attribute);
        if numbers.is_some() != weights.is_some() {
            return refuse("vertex numbers and weights must be requested together");
        }
        let names: Vec<String> = [
            &p_distance_attribute,
            &p_primitive_attribute,
            &p_source_vertex_numbers_attribute,
            &p_source_vertex_weights_attribute,
            &p_normal_attribute,
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
        let patterns = [&p_point_pattern, &p_vertex_pattern, &p_primitive_pattern, &p_detail_pattern];
        for pattern in patterns {
            if !trim(pattern).is_empty() {
                support::validate_parameter_pattern("sop/ray", pattern)?;
            }
        }
        if p_match_groups && [&p_point_pattern, &p_vertex_pattern, &p_primitive_pattern].iter().all(|n| trim(n).is_empty()) {
            return refuse("group interpolation requires a pattern");
        }
        Ok(())
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let mut r = Reader::new(values);
        let p_group_owner = support::ELEMENT_OWNER.of(r.next());
        let p_group = r.text();
        let p_collision_group = r.text();
        let p_method = r.choice(&METHOD);
        let p_direction = r.choice(&DIRECTION);
        let p_direction_vector = [r.float(), r.float(), r.float()];
        let p_direction_attribute = r.text();
        let p_direction_mode = r.choice(&DIRECTION_MODE);
        let p_surface_hit = r.choice(&SURFACE);
        let p_samples = r.int();
        let p_jitter_scale = r.float();
        let p_seed = r.int();
        let p_combine = r.choice(&COMBINE);
        let p_min_distance = r.float();
        let p_limit_max_distance = r.bool();
        let p_max_distance = r.float();
        let p_tolerance = r.float();
        let p_scale = r.float();
        let p_lift = r.float();
        let p_distance_attribute = r.text();
        let p_primitive_attribute = r.text();
        let p_source_vertex_numbers_attribute = r.text();
        let p_source_vertex_weights_attribute = r.text();
        let p_hit_group = r.text();
        let p_normal_attribute = r.text();
        let p_point_pattern = r.text();
        let p_vertex_pattern = r.text();
        let p_primitive_pattern = r.text();
        let p_detail_pattern = r.text();
        let p_match_groups = r.bool();
        let selection = support::optional_element_group(p_group_owner, &p_group);
        let collision_group = support::optional_text(&p_collision_group);
        let direction = match p_direction {
            RayDirection::Vector => {
                Direction::RayVector(vec3(p_direction_vector[0], p_direction_vector[1], p_direction_vector[2]))
            }
            RayDirection::Normal => Direction::RayNormal,
            RayDirection::Attribute => Direction::RayAttribute(p_direction_attribute.clone()),
        };
        let max_distance = if p_limit_max_distance { Some(p_max_distance) } else { None };
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
                        rdk_core::geometry::find_group(rdk_core::group::Owner::Primitive, name, &inputs[1]).ok_or_else(|| {
                            support::diagnostic(
                                "missing_collision_group",
                                &format!("ray could not find collision primitive group {}", node::ocaml_quote(name)),
                            )
                        })?,
                    ),
                };
                let params = rdk_ops::ray::Params {
                    grain: context::grain(context),
                    selection,
                    collision_primitives,
                    method: p_method,
                    direction: direction.clone(),
                    direction_mode: p_direction_mode,
                    surface_hit: p_surface_hit,
                    samples: p_samples,
                    jitter_scale: p_jitter_scale,
                    seed: p_seed,
                    combine: p_combine,
                    min_distance: p_min_distance,
                    max_distance,
                    tolerance: p_tolerance,
                    scale: p_scale,
                    lift: p_lift,
                    distance_attribute: text(&p_distance_attribute),
                    primitive_attribute: text(&p_primitive_attribute),
                    source_vertex_numbers_attribute: text(&p_source_vertex_numbers_attribute),
                    source_vertex_weights_attribute: text(&p_source_vertex_weights_attribute),
                    hit_group: text(&p_hit_group),
                    normal_attribute: text(&p_normal_attribute),
                    point_pattern: text(&p_point_pattern),
                    vertex_pattern: text(&p_vertex_pattern),
                    primitive_pattern: text(&p_primitive_pattern),
                    detail_pattern: text(&p_detail_pattern),
                    match_groups: p_match_groups,
                };
                match rdk_ops::ray::run(Some(context::cancel_token(context).as_ref()), &params, &inputs[0], &inputs[1]) {
                    Ok(geometry) => support::cooked(geometry),
                    Err(error) => Err(support::structured_rdk_error(&error)),
                }
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}
