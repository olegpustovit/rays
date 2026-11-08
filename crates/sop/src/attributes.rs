//! Port of lib/sop/attributes.ml: the attribute, UV and transfer nodes, declared once each (Image
//! Texels is the carrier that lets a Lisp body sample pixels). See PORTING.md.
use std::sync::Arc;

use param::Value;
use rdk::{attribute, attribute_ops, geometry, group, packed, surface_index, transform_ops, uv_ops};

use crate::context::{self, dependencies};
use crate::decl::{self, Decl, Meta, Spec};
use crate::edit_graph::Factory;
use crate::node::{self, build, Cooked, CookMode};
use crate::payload;
use crate::support::{self, Choice, DistanceRadiusMode, KernelMode, MirrorMethod, MirrorTransform, TransferFalloff, TransferMode};

// ---------------------------------------------------------------- shared helpers

fn bytes(value: &Value) -> &[u8] {
    match value {
        Value::Text(text) => text,
        _ => unreachable!("attribute node: parameter record has the wrong shape"),
    }
}

fn text(value: &Value) -> String {
    String::from_utf8_lossy(bytes(value)).into_owned()
}

fn text_value(value: &str) -> Value {
    Value::Text(value.as_bytes().to_vec())
}

fn int(value: &Value) -> i64 {
    match value {
        Value::Int(int) => *int,
        _ => unreachable!("attribute node: parameter record has the wrong shape"),
    }
}

fn float(value: &Value) -> f64 {
    match value {
        Value::Float(float) => *float,
        _ => unreachable!("attribute node: parameter record has the wrong shape"),
    }
}

fn boolean(value: &Value) -> bool {
    match value {
        Value::Bool(flag) => *flag,
        _ => unreachable!("attribute node: parameter record has the wrong shape"),
    }
}

/// `sqrt Float.max_float`: the largest distance whose square stays finite.
fn safe_square_limit() -> f64 {
    f64::MAX.sqrt()
}

// Choice tables that have no support.rs twin: the row order is the OCaml `Param.choice` order.
const REFERENCE: Choice<transform_ops::DistanceFromGeometryReference> = Choice(&[
    ("Points", transform_ops::DistanceFromGeometryReference::DistanceReferencePoints),
    ("Primitives", transform_ops::DistanceFromGeometryReference::DistanceReferencePrimitives),
]);
const GROUP_USE: Choice<rdk::attribute_mirror::GroupUse> = Choice(&[
    ("Group is source", rdk::attribute_mirror::GroupUse::MirrorGroupAsSource),
    ("Group is destination", rdk::attribute_mirror::GroupUse::MirrorGroupAsDestination),
]);
const MIRROR_METHOD: Choice<MirrorMethod> = Choice(&[("Plane", MirrorMethod::Plane), ("Mapping attribute", MirrorMethod::Mapping)]);
const MIRROR_TRANSFORM: Choice<MirrorTransform> = Choice(&[
    ("Copy", MirrorTransform::Copy),
    ("UV", MirrorTransform::Uv),
    ("Vector", MirrorTransform::Vector),
    ("Point", MirrorTransform::Point),
]);
const VERTEX_SELECTION: Choice<surface_index::VertexSelection> = Choice(&[
    ("All triangle vertices", surface_index::VertexSelection::AllTriangleVertices),
    ("Any triangle vertex", surface_index::VertexSelection::AnyTriangleVertex),
]);

// ---------------------------------------------------------------- image texels

pub mod image_texels {
    use super::*;

    // `type parameters = unit`: no fields, so no Parameters struct and no conversion.
    pub const META: Meta = Meta {
        key: "image_texels",
        label: "Image Texels",
        category: "Attribute/Image",
        inputs: 1,
        slots: Some("image"),
        types: Some(&["image"]),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        Vec::new()
    }

    /// One point per texel in row-major order, P = (x, y, width), and the Point Float4 attribute
    /// "rgba". An Image input is cooked by its own node; this one is a Generator over it.
    pub fn operator(label: &str, _values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        Ok(build::make(
            build::Args {
                label: Some(label.to_owned()),
                cook_mode: CookMode::Generator,
                dependencies: dependencies::STATIC,
                inputs: vec![inputs[0].clone().unwrap()],
                ..build::args("image_texels")
            },
            None,
            Arc::new(|_, _, inputs| {
                let image = payload::image(&inputs[0])?;
                let width = crate::image::width(&image);
                let rgba = crate::image::storage(&image);
                let count = width * crate::image::height(&image);
                let fail = |message: String| support::diagnostic("invalid_image_texels", &message);
                let mut geometry_value = geometry::generate_point_ranges(count, &mut |first, last, x, y, z| {
                    for i in first..last {
                        let index = i as usize;
                        x[index] = (i % width) as f64;
                        y[index] = (i / width) as f64;
                        z[index] = width as f64;
                    }
                })
                .map_err(|e| fail(rdk::error::to_string(&e)))?;
                let plane = |k: usize| (0..count as usize).map(|i| rgba[4 * i + k]).collect::<Vec<f64>>();
                let values = packed::Float4::of_owned(plane(0), plane(1), plane(2), plane(3))
                    .map_err(|e| fail(rdk::error::to_string(&e)))?;
                let attribute_value = attribute::create_owned("rgba", attribute::Owner::Point, attribute::Storage::Float4(values))
                    .map_err(|e| fail(rdk::error::to_string(&e)))?;
                geometry_value = geometry::with_attribute(attribute_value, &geometry_value)
                    .map_err(|e| fail(rdk::error::to_string(&e)))?;
                Ok(Cooked { payload: payload::T::Geometry(geometry_value), instances: None })
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

// ---------------------------------------------------------------- UV flatten and relax

pub mod uv_flatten {
    use super::*;

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub name: String,
        pub seams: String,
        pub iterations: i64,
        pub tolerance: f64,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            name: text(&values[0]),
            seams: text(&values[1]),
            iterations: int(&values[2]),
            tolerance: float(&values[3]),
        }
    }

    pub fn to_values(p: &Parameters) -> Vec<Value> {
        vec![text_value(&p.name), text_value(&p.seams), Value::Int(p.iterations), Value::Float(p.tolerance)]
    }

    pub const META: Meta = Meta {
        key: "uv_flatten",
        label: "UV Flatten",
        category: "UV/Layout",
        inputs: 1,
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            Spec { label: "UV attribute", ..decl::spec("name", decl::text_kind(), text_value("uv")) },
            Spec { label: "Seam group", ..decl::spec("seams", decl::text_kind(), text_value("")) },
            Spec {
                label: "Iterations",
                ..decl::spec("iterations", decl::int_kind(1, 2000, Some(1), None), Value::Int(500))
            },
            Spec {
                label: "Tolerance",
                ..decl::spec("tolerance", decl::float_kind(0.0, 0.01, Some(0.0), None), Value::Float(1e-7))
            },
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let input = inputs[0].clone().unwrap();
        let (name, seams_name, iterations, tolerance) =
            (p.name, support::optional_text(&p.seams), p.iterations, p.tolerance);
        Ok(build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                cook_mode: CookMode::DuplicateInput(0),
                dependencies: dependencies::STATIC,
                inputs: vec![input],
                ..build::args("uv_flatten")
            },
            None,
            Arc::new(move |_, context, inputs| {
                let (edge_seams, seams) = support::uv_parameterize_seams("uv_flatten", &inputs[0], &seams_name)?;
                let params = uv_ops::Params {
                    cancel: Some(Arc::clone(context::cancel_token(context))),
                    grain: Some(context::grain(context)),
                    name: name.clone(),
                    seams,
                    edge_seams,
                    uv_tolerance: 1e-9,
                    iterations,
                    tolerance,
                };
                match uv_ops::flatten(&params, &inputs[0]) {
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

pub mod uv_relax {
    use super::*;

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub name: String,
        pub seams: String,
        pub uv_tolerance: f64,
        pub iterations: i64,
        pub tolerance: f64,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            name: text(&values[0]),
            seams: text(&values[1]),
            uv_tolerance: float(&values[2]),
            iterations: int(&values[3]),
            tolerance: float(&values[4]),
        }
    }

    pub fn to_values(p: &Parameters) -> Vec<Value> {
        vec![
            text_value(&p.name),
            text_value(&p.seams),
            Value::Float(p.uv_tolerance),
            Value::Int(p.iterations),
            Value::Float(p.tolerance),
        ]
    }

    pub const META: Meta = Meta {
        key: "uv_relax",
        label: "UV Relax",
        category: "UV/Layout",
        inputs: 1,
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            Spec { label: "UV attribute", ..decl::spec("name", decl::text_kind(), text_value("uv")) },
            Spec { label: "Seam group", ..decl::spec("seams", decl::text_kind(), text_value("")) },
            Spec {
                label: "UV tolerance",
                ..decl::spec("uv_tolerance", decl::float_kind(0.0, 0.01, Some(0.0), None), Value::Float(1e-9))
            },
            Spec {
                label: "Iterations",
                ..decl::spec("iterations", decl::int_kind(1, 2000, Some(1), None), Value::Int(500))
            },
            Spec {
                label: "Tolerance",
                ..decl::spec("tolerance", decl::float_kind(0.0, 0.01, Some(0.0), None), Value::Float(1e-7))
            },
        ]
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let input = inputs[0].clone().unwrap();
        let (name, seams_name, uv_tolerance, iterations, tolerance) =
            (p.name, support::optional_text(&p.seams), p.uv_tolerance, p.iterations, p.tolerance);
        Ok(build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                cook_mode: CookMode::DuplicateInput(0),
                dependencies: dependencies::STATIC,
                inputs: vec![input],
                ..build::args("uv_relax")
            },
            None,
            Arc::new(move |_, context, inputs| {
                let (edge_seams, seams) = support::uv_parameterize_seams("uv_relax", &inputs[0], &seams_name)?;
                let params = uv_ops::Params {
                    cancel: Some(Arc::clone(context::cancel_token(context))),
                    grain: Some(context::grain(context)),
                    name: name.clone(),
                    seams,
                    edge_seams,
                    uv_tolerance,
                    iterations,
                    tolerance,
                };
                match uv_ops::relax(&params, &inputs[0]) {
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

// ---------------------------------------------------------------- distance from geometry

pub mod distance_from_geometry {
    use super::*;

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub affected_owner: rdk::group_ops::Owner,
        pub affected_group: String,
        pub reference_owner: rdk::group_ops::Owner,
        pub reference_group: String,
        pub reference_kind: transform_ops::DistanceFromGeometryReference,
        pub falloff: transform_ops::SoftTransformFalloff,
        pub radius_mode: DistanceRadiusMode,
        pub radius: f64,
        pub distance_attribute: String,
        pub mask_attribute: String,
    }

    pub fn from_values(values: &[Value]) -> Parameters {
        Parameters {
            affected_owner: support::ELEMENT_OWNER.of(&values[0]),
            affected_group: text(&values[1]),
            reference_owner: support::ELEMENT_OWNER.of(&values[2]),
            reference_group: text(&values[3]),
            reference_kind: REFERENCE.of(&values[4]),
            falloff: support::SOFT_FALLOFF.of(&values[5]),
            radius_mode: support::DISTANCE_RADIUS.of(&values[6]),
            radius: float(&values[7]),
            distance_attribute: text(&values[8]),
            mask_attribute: text(&values[9]),
        }
    }

    pub fn to_values(p: &Parameters) -> Vec<Value> {
        vec![
            support::ELEMENT_OWNER.value(&p.affected_owner),
            text_value(&p.affected_group),
            support::ELEMENT_OWNER.value(&p.reference_owner),
            text_value(&p.reference_group),
            REFERENCE.value(&p.reference_kind),
            support::SOFT_FALLOFF.value(&p.falloff),
            support::DISTANCE_RADIUS.value(&p.radius_mode),
            Value::Float(p.radius),
            text_value(&p.distance_attribute),
            text_value(&p.mask_attribute),
        ]
    }

    pub const META: Meta = Meta {
        key: "distance_from_geometry",
        label: "Distance from Geometry",
        category: "Attribute/Distance",
        inputs: 2,
        slots: Some("source, reference"),
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            Spec {
                label: "Affected group type",
                folder: "Source",
                ..decl::spec(
                    "affected_owner",
                    support::ELEMENT_OWNER.kind(),
                    support::ELEMENT_OWNER.value(&rdk::group_ops::Owner::GroupPoints),
                )
            },
            Spec {
                label: "Affected group",
                folder: "Source",
                ..decl::spec("affected_group", decl::text_kind(), text_value(""))
            },
            Spec {
                label: "Reference group type",
                folder: "Reference",
                ..decl::spec(
                    "reference_owner",
                    support::ELEMENT_OWNER.kind(),
                    support::ELEMENT_OWNER.value(&rdk::group_ops::Owner::GroupPrimitives),
                )
            },
            Spec {
                label: "Reference group",
                folder: "Reference",
                ..decl::spec("reference_group", decl::text_kind(), text_value(""))
            },
            Spec {
                label: "Reference type",
                folder: "Reference",
                ..decl::spec("reference_kind", REFERENCE.kind(), REFERENCE.at(1))
            },
            Spec {
                label: "Falloff",
                ..decl::spec(
                    "falloff",
                    support::SOFT_FALLOFF.kind(),
                    support::SOFT_FALLOFF.at(0),
                )
            },
            Spec {
                label: "Radius",
                ..decl::spec(
                    "radius_mode",
                    support::DISTANCE_RADIUS.kind(),
                    support::DISTANCE_RADIUS.value(&DistanceRadiusMode::Maximum),
                )
            },
            Spec {
                label: "Fixed radius",
                ..decl::spec("radius", decl::float_kind(0.0001, 1000.0, Some(0.0), None), Value::Float(1.0))
            },
            Spec {
                label: "Distance attribute",
                folder: "Output",
                ..decl::spec("distance_attribute", decl::text_kind(), text_value("distance"))
            },
            Spec {
                label: "Mask attribute",
                folder: "Output",
                ..decl::spec("mask_attribute", decl::text_kind(), text_value(""))
            },
        ]
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        if !p.radius.is_finite()
            || p.radius < 0.0
            || (matches!(p.radius_mode, DistanceRadiusMode::Fixed) && p.radius == 0.0)
        {
            return Err("sop/distance_from_geometry: fixed radius must be positive and finite".to_owned());
        }
        let mut names: Vec<String> = [&p.distance_attribute, &p.mask_attribute]
            .into_iter()
            .filter_map(|name| support::optional_text(name))
            .collect();
        let count = names.len();
        names.sort();
        names.dedup();
        if count == 0 || names.iter().any(|name| name == "P") || names.len() != count {
            return Err("sop/distance_from_geometry: output names must be present, distinct and cannot be P".to_owned());
        }
        Ok(())
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let (source, reference) = (inputs[0].clone().unwrap(), inputs[1].clone().unwrap());
        let affected = support::optional_element_group(p.affected_owner, &p.affected_group);
        let reference_selection = support::optional_element_group(p.reference_owner, &p.reference_group);
        let radius = match p.radius_mode {
            DistanceRadiusMode::Fixed => transform_ops::DistanceAlongRadius::DistanceFixed(p.radius),
            DistanceRadiusMode::Maximum => transform_ops::DistanceAlongRadius::DistanceMaximum,
        };
        let (reference_kind, falloff) = (p.reference_kind, p.falloff);
        let distance_attribute = support::optional_text(&p.distance_attribute);
        let mask_attribute = support::optional_text(&p.mask_attribute);
        Ok(build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                cook_mode: CookMode::DuplicateInput(0),
                dependencies: dependencies::STATIC,
                inputs: vec![source, reference],
                ..build::args("distance_from_geometry")
            },
            None,
            Arc::new(move |_, context, inputs| {
                let affected = support::resolve_element_group("distance_from_geometry affected", &affected, &inputs[0])?;
                let reference_selection = support::resolve_element_group(
                    "distance_from_geometry reference",
                    &reference_selection,
                    &inputs[1],
                )?;
                let params = transform_ops::DistanceFromGeometryParams {
                    cancel: Some(Arc::clone(context::cancel_token(context))),
                    grain: Some(context::grain(context)),
                    affected,
                    reference_selection,
                    reference_kind,
                    falloff,
                    radius: radius.clone(),
                    distance_attribute: distance_attribute.clone(),
                    mask_attribute: mask_attribute.clone(),
                    reference: inputs[1].clone(),
                };
                match transform_ops::distance_from_geometry(&params, &inputs[0]) {
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

// ---------------------------------------------------------------- attribute mirror

pub mod attribute_mirror {
    use super::*;

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub owner: attribute::Owner,
        pub attributes: String,
        pub group: String,
        pub group_use: rdk::attribute_mirror::GroupUse,
        pub method_: MirrorMethod,
        pub origin_x: f64,
        pub origin_y: f64,
        pub origin_z: f64,
        pub normal_x: f64,
        pub normal_y: f64,
        pub normal_z: f64,
        pub distance: f64,
        pub tolerance: f64,
        pub mapping_attribute: String,
        pub mapping_destination_group: String,
        pub transform: MirrorTransform,
        pub uv_origin_u: f64,
        pub uv_origin_v: f64,
        pub uv_direction_u: f64,
        pub uv_direction_v: f64,
        pub replace_strings: bool,
        pub string_search: Vec<u8>,
        pub string_replacement: Vec<u8>,
        pub output_mapping: String,
        pub source_group: String,
        pub destination_group: String,
    }

    pub fn from_values(v: &[Value]) -> Parameters {
        Parameters {
            owner: support::ELEMENT_ATTRIBUTE_OWNER.of(&v[0]),
            attributes: text(&v[1]),
            group: text(&v[2]),
            group_use: GROUP_USE.of(&v[3]),
            method_: MIRROR_METHOD.of(&v[4]),
            origin_x: float(&v[5]),
            origin_y: float(&v[6]),
            origin_z: float(&v[7]),
            normal_x: float(&v[8]),
            normal_y: float(&v[9]),
            normal_z: float(&v[10]),
            distance: float(&v[11]),
            tolerance: float(&v[12]),
            mapping_attribute: text(&v[13]),
            mapping_destination_group: text(&v[14]),
            transform: MIRROR_TRANSFORM.of(&v[15]),
            uv_origin_u: float(&v[16]),
            uv_origin_v: float(&v[17]),
            uv_direction_u: float(&v[18]),
            uv_direction_v: float(&v[19]),
            replace_strings: boolean(&v[20]),
            string_search: bytes(&v[21]).to_vec(),
            string_replacement: bytes(&v[22]).to_vec(),
            output_mapping: text(&v[23]),
            source_group: text(&v[24]),
            destination_group: text(&v[25]),
        }
    }

    pub fn to_values(p: &Parameters) -> Vec<Value> {
        vec![
            support::ATTRIBUTE_OWNER.value(&p.owner),
            text_value(&p.attributes),
            text_value(&p.group),
            GROUP_USE.value(&p.group_use),
            MIRROR_METHOD.value(&p.method_),
            Value::Float(p.origin_x),
            Value::Float(p.origin_y),
            Value::Float(p.origin_z),
            Value::Float(p.normal_x),
            Value::Float(p.normal_y),
            Value::Float(p.normal_z),
            Value::Float(p.distance),
            Value::Float(p.tolerance),
            text_value(&p.mapping_attribute),
            text_value(&p.mapping_destination_group),
            MIRROR_TRANSFORM.value(&p.transform),
            Value::Float(p.uv_origin_u),
            Value::Float(p.uv_origin_v),
            Value::Float(p.uv_direction_u),
            Value::Float(p.uv_direction_v),
            Value::Bool(p.replace_strings),
            Value::Text(p.string_search.clone()),
            Value::Text(p.string_replacement.clone()),
            text_value(&p.output_mapping),
            text_value(&p.source_group),
            text_value(&p.destination_group),
        ]
    }

    pub const META: Meta = Meta {
        key: "attribute_mirror",
        label: "Attribute Mirror",
        category: "Attribute/Transform",
        inputs: 1,
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            Spec {
                label: "Attribute owner",
                ..decl::spec("owner", support::ELEMENT_ATTRIBUTE_OWNER.kind(), support::ELEMENT_ATTRIBUTE_OWNER.at(0))
            },
            Spec { label: "Attributes", ..decl::spec("attributes", decl::text_kind(), text_value("Cd")) },
            Spec { label: "Selection group", ..decl::spec("group", decl::text_kind(), text_value("")) },
            Spec {
                label: "Group use",
                ..decl::spec("group_use", GROUP_USE.kind(), GROUP_USE.at(0))
            },
            Spec {
                label: "Mirror method",
                ..decl::spec("method_", MIRROR_METHOD.kind(), MIRROR_METHOD.at(0))
            },
            Spec {
                label: "Origin X",
                folder: "Plane/Origin",
                vec3: Some(("origin", 0)),
                ..decl::spec("origin_x", decl::float_kind(-10.0, 10.0, None, None), Value::Float(0.0))
            },
            Spec {
                label: "Origin Y",
                folder: "Plane/Origin",
                vec3: Some(("origin", 1)),
                ..decl::spec("origin_y", decl::float_kind(-10.0, 10.0, None, None), Value::Float(0.0))
            },
            Spec {
                label: "Origin Z",
                folder: "Plane/Origin",
                vec3: Some(("origin", 2)),
                ..decl::spec("origin_z", decl::float_kind(-10.0, 10.0, None, None), Value::Float(0.0))
            },
            Spec {
                label: "Normal X",
                folder: "Plane/Normal",
                vec3: Some(("normal", 0)),
                ..decl::spec("normal_x", decl::float_kind(-1.0, 1.0, None, None), Value::Float(1.0))
            },
            Spec {
                label: "Normal Y",
                folder: "Plane/Normal",
                vec3: Some(("normal", 1)),
                ..decl::spec("normal_y", decl::float_kind(-1.0, 1.0, None, None), Value::Float(0.0))
            },
            Spec {
                label: "Normal Z",
                folder: "Plane/Normal",
                vec3: Some(("normal", 2)),
                ..decl::spec("normal_z", decl::float_kind(-1.0, 1.0, None, None), Value::Float(0.0))
            },
            Spec {
                label: "Maximum distance",
                folder: "Plane",
                ..decl::spec("distance", decl::float_kind(0.0, 1000000.0, Some(0.0), None), Value::Float(1000000.0))
            },
            Spec {
                label: "Tolerance",
                folder: "Plane",
                ..decl::spec("tolerance", decl::float_kind(0.0, 1.0, Some(0.0), None), Value::Float(0.00001))
            },
            Spec {
                label: "Mapping attribute",
                folder: "Mapping",
                ..decl::spec("mapping_attribute", decl::text_kind(), text_value("mirror"))
            },
            Spec {
                label: "Mapping destination group",
                folder: "Mapping",
                ..decl::spec("mapping_destination_group", decl::text_kind(), text_value("mirror_destination"))
            },
            Spec {
                label: "Value transform",
                ..decl::spec("transform", MIRROR_TRANSFORM.kind(), MIRROR_TRANSFORM.at(0))
            },
            Spec {
                label: "UV origin U",
                folder: "Value transform/UV",
                ..decl::spec("uv_origin_u", decl::float_kind(-10.0, 10.0, None, None), Value::Float(0.0))
            },
            Spec {
                label: "UV origin V",
                folder: "Value transform/UV",
                ..decl::spec("uv_origin_v", decl::float_kind(-10.0, 10.0, None, None), Value::Float(0.0))
            },
            Spec {
                label: "UV direction U",
                folder: "Value transform/UV",
                ..decl::spec("uv_direction_u", decl::float_kind(-10.0, 10.0, None, None), Value::Float(1.0))
            },
            Spec {
                label: "UV direction V",
                folder: "Value transform/UV",
                ..decl::spec("uv_direction_v", decl::float_kind(-10.0, 10.0, None, None), Value::Float(0.0))
            },
            Spec {
                label: "Replace strings",
                folder: "Strings",
                ..decl::spec("replace_strings", decl::toggle_kind(), Value::Bool(false))
            },
            Spec { label: "Search", folder: "Strings", ..decl::spec("string_search", decl::text_kind(), text_value("L")) },
            Spec {
                label: "Replacement",
                folder: "Strings",
                ..decl::spec("string_replacement", decl::text_kind(), text_value("R"))
            },
            Spec {
                label: "Output mapping",
                folder: "Output",
                ..decl::spec("output_mapping", decl::text_kind(), text_value(""))
            },
            Spec { label: "Source group", folder: "Output", ..decl::spec("source_group", decl::text_kind(), text_value("")) },
            Spec {
                label: "Destination group",
                folder: "Output",
                ..decl::spec("destination_group", decl::text_kind(), text_value(""))
            },
        ]
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        let finite = |value: f64| -> Result<(), String> {
            if value.is_finite() { Ok(()) } else { Err("sop/attribute_mirror: controls must be finite".to_owned()) }
        };
        for value in [
            p.origin_x,
            p.origin_y,
            p.origin_z,
            p.normal_x,
            p.normal_y,
            p.normal_z,
            p.distance,
            p.tolerance,
            p.uv_origin_u,
            p.uv_origin_v,
            p.uv_direction_u,
            p.uv_direction_v,
        ] {
            finite(value)?;
        }
        if p.distance < 0.0 || p.tolerance < 0.0 || p.tolerance > safe_square_limit() {
            return Err("sop/attribute_mirror: invalid distance or tolerance".to_owned());
        }
        if p.replace_strings && p.string_search.is_empty() {
            return Err("sop/attribute_mirror: empty string search".to_owned());
        }
        if let (Some(source), Some(destination)) =
            (support::optional_text(&p.source_group), support::optional_text(&p.destination_group))
        {
            if source == destination {
                return Err("sop/attribute_mirror: source and destination groups must differ".to_owned());
            }
        }
        if p.transform == MirrorTransform::Uv && p.uv_direction_u == 0.0 && p.uv_direction_v == 0.0 {
            return Err("sop/attribute_mirror: UV direction must be non-zero".to_owned());
        }
        support::validate_parameter_pattern("sop/attribute_mirror: attributes", &p.attributes)?;
        match p.method_ {
            MirrorMethod::Mapping => {
                if support::ocaml_trim(&p.mapping_attribute).is_empty() {
                    return Err("sop/attribute_mirror: empty mapping attribute".to_owned());
                }
                if support::ocaml_trim(&p.mapping_destination_group).is_empty() {
                    return Err("sop/attribute_mirror: empty mapping destination group".to_owned());
                }
                if matches!(p.transform, MirrorTransform::Vector | MirrorTransform::Point) {
                    return Err("sop/attribute_mirror: vector/point transforms require a plane".to_owned());
                }
                Ok(())
            }
            MirrorMethod::Plane => {
                if p.owner == attribute::Owner::Vertex {
                    return Err("sop/attribute_mirror: vertex owner requires mapping".to_owned());
                }
                let scale = p.normal_x.abs().max(p.normal_y.abs()).max(p.normal_z.abs());
                if scale == 0.0 {
                    return Err("sop/attribute_mirror: normal must be non-zero".to_owned());
                }
                let (x, y, z) = (p.normal_x / scale, p.normal_y / scale, p.normal_z / scale);
                let length = (x * x + y * y + z * z).sqrt();
                for value in [
                    p.origin_x + p.distance * (x / length),
                    p.origin_y + p.distance * (y / length),
                    p.origin_z + p.distance * (z / length),
                ] {
                    finite(value)?;
                }
                Ok(())
            }
        }
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let input = inputs[0].clone().unwrap();
        let origin = rays_math::vec3::create(p.origin_x, p.origin_y, p.origin_z);
        let normal = rays_math::vec3::create(p.normal_x, p.normal_y, p.normal_z);
        let transform = match p.transform {
            MirrorTransform::Copy => rdk::attribute_mirror::Transform::MirrorCopy,
            MirrorTransform::Uv => rdk::attribute_mirror::Transform::MirrorUv {
                origin_u: p.uv_origin_u,
                origin_v: p.uv_origin_v,
                direction_u: p.uv_direction_u,
                direction_v: p.uv_direction_v,
            },
            MirrorTransform::Vector => rdk::attribute_mirror::Transform::MirrorVector,
            MirrorTransform::Point => rdk::attribute_mirror::Transform::MirrorPoint,
        };
        let group_name = support::optional_text(&p.group);
        let output_mapping = support::optional_text(&p.output_mapping);
        let source_group = support::optional_text(&p.source_group);
        let destination_group = support::optional_text(&p.destination_group);
        let string_replace = p.replace_strings.then(|| (p.string_search.clone(), p.string_replacement.clone()));
        let owner = p.owner;
        let (group_use, method_kind) = (p.group_use, p.method_);
        let (distance, tolerance) = (p.distance, p.tolerance);
        let (mapping_attribute, mapping_destination) = (p.mapping_attribute.clone(), p.mapping_destination_group.clone());
        let attributes = p.attributes.clone();
        Ok(build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                cook_mode: CookMode::DuplicateInput(0),
                dependencies: dependencies::STATIC,
                inputs: vec![input],
                ..build::args("attribute_mirror")
            },
            None,
            Arc::new(move |_, context, inputs| {
                let geometry = &inputs[0];
                let group_owner = support::attribute_mirror_group_owner(owner);
                let resolved_group = match &group_name {
                    None => None,
                    Some(name) => Some(geometry::find_group(group_owner, name, geometry).ok_or_else(|| {
                        support::diagnostic(
                            "missing_group",
                            &format!("attribute_mirror could not find selection group {}", node::ocaml_quote(name)),
                        )
                    })?),
                };
                let method = match method_kind {
                    MirrorMethod::Plane => rdk::attribute_mirror::Method::MirrorByPlane { origin, normal, distance, tolerance },
                    MirrorMethod::Mapping => {
                        let destination_group = geometry::find_group(group_owner, &mapping_destination, geometry)
                            .ok_or_else(|| {
                                support::diagnostic(
                                    "missing_group",
                                    &format!(
                                        "attribute_mirror could not find mapping destination group {}",
                                        node::ocaml_quote(&mapping_destination)
                                    ),
                                )
                            })?;
                        rdk::attribute_mirror::Method::MirrorByMapping {
                            mapping_attribute: mapping_attribute.clone(),
                            destination_group,
                        }
                    }
                };
                let params = rdk::attribute_mirror::Params {
                    cancel: Some(Arc::clone(context::cancel_token(context))),
                    grain: Some(context::grain(context)),
                    group: resolved_group,
                    group_use,
                    attributes: attributes.clone(),
                    transform,
                    string_replace: string_replace.clone(),
                    output_mapping: output_mapping.clone(),
                    source_group: source_group.clone(),
                    destination_group: destination_group.clone(),
                    owner,
                    method_: method,
                };
                match rdk::attribute_mirror::run(&params, geometry) {
                    Ok(result) => support::cooked(result),
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

// ---------------------------------------------------------------- attribute transfer all

pub mod attribute_transfer_all {
    use super::*;

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub point_pattern: String,
        pub vertex_pattern: String,
        pub primitive_pattern: String,
        pub detail_pattern: String,
        pub mode: TransferMode,
        pub neighbors: i64,
        pub power: f64,
        pub kernel_radius: f64,
        pub distance_mode: KernelMode,
        pub max_distance: f64,
        pub blend_width: f64,
        pub falloff: TransferFalloff,
        pub uniform_bias: f64,
        pub unmatched: rdk::attribute_transfer::Unmatched,
    }

    pub fn from_values(v: &[Value]) -> Parameters {
        Parameters {
            point_pattern: text(&v[0]),
            vertex_pattern: text(&v[1]),
            primitive_pattern: text(&v[2]),
            detail_pattern: text(&v[3]),
            mode: support::TRANSFER_MODE.of(&v[4]),
            neighbors: int(&v[5]),
            power: float(&v[6]),
            kernel_radius: float(&v[7]),
            distance_mode: support::KERNEL_MODE.of(&v[8]),
            max_distance: float(&v[9]),
            blend_width: float(&v[10]),
            falloff: support::TRANSFER_FALLOFF.of(&v[11]),
            uniform_bias: float(&v[12]),
            unmatched: support::TRANSFER_UNMATCHED.of(&v[13]),
        }
    }

    pub fn to_values(p: &Parameters) -> Vec<Value> {
        vec![
            text_value(&p.point_pattern),
            text_value(&p.vertex_pattern),
            text_value(&p.primitive_pattern),
            text_value(&p.detail_pattern),
            support::TRANSFER_MODE.value(&p.mode),
            Value::Int(p.neighbors),
            Value::Float(p.power),
            Value::Float(p.kernel_radius),
            support::KERNEL_MODE.value(&p.distance_mode),
            Value::Float(p.max_distance),
            Value::Float(p.blend_width),
            support::TRANSFER_FALLOFF.value(&p.falloff),
            Value::Float(p.uniform_bias),
            support::TRANSFER_UNMATCHED.value(&p.unmatched),
        ]
    }

    pub const META: Meta = Meta {
        key: "attribute_transfer_all",
        label: "Attribute Transfer All",
        category: "Attribute/Transfer",
        inputs: 2,
        slots: Some("source, target"),
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            Spec { label: "Point attributes", ..decl::spec("point_pattern", decl::text_kind(), text_value("*")) },
            Spec { label: "Vertex attributes", ..decl::spec("vertex_pattern", decl::text_kind(), text_value("")) },
            Spec { label: "Primitive attributes", ..decl::spec("primitive_pattern", decl::text_kind(), text_value("")) },
            Spec { label: "Detail attributes", ..decl::spec("detail_pattern", decl::text_kind(), text_value("")) },
            Spec {
                label: "Transfer mode",
                ..decl::spec("mode", support::TRANSFER_MODE.kind(), support::TRANSFER_MODE.at(0))
            },
            Spec {
                label: "Neighbors",
                folder: "Sampling",
                ..decl::spec("neighbors", decl::int_kind(1, 128, Some(1), None), Value::Int(4))
            },
            Spec {
                label: "Inverse power",
                folder: "Sampling",
                ..decl::spec("power", decl::float_kind(0.0, 10.0, Some(0.0), None), Value::Float(2.0))
            },
            Spec {
                label: "Kernel radius",
                folder: "Sampling",
                ..decl::spec("kernel_radius", decl::float_kind(0.0, 100.0, Some(0.0), None), Value::Float(1.0))
            },
            Spec {
                label: "Distance mode",
                folder: "Distance",
                ..decl::spec("distance_mode", support::KERNEL_MODE.kind(), support::KERNEL_MODE.value(&KernelMode::Explicit))
            },
            Spec {
                label: "Maximum distance",
                folder: "Distance",
                ..decl::spec("max_distance", decl::float_kind(0.0, 100.0, Some(0.0), None), Value::Float(1.0))
            },
            Spec {
                label: "Blend width",
                folder: "Distance",
                ..decl::spec("blend_width", decl::float_kind(0.0, 100.0, Some(0.0), None), Value::Float(0.0))
            },
            Spec {
                label: "Falloff",
                folder: "Distance",
                ..decl::spec("falloff", support::TRANSFER_FALLOFF.kind(), support::TRANSFER_FALLOFF.at(0))
            },
            Spec {
                label: "Uniform bias",
                folder: "Distance",
                ..decl::spec("uniform_bias", decl::float_kind(0.0, 1.0, Some(0.0), Some(1.0)), Value::Float(0.5))
            },
            Spec {
                label: "Unmatched",
                ..decl::spec("unmatched", support::TRANSFER_UNMATCHED.kind(), support::TRANSFER_UNMATCHED.at(0))
            },
        ]
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        let refuse = |message: &str| Err(format!("sop/attribute_transfer_all: {message}"));
        let patterns = [&p.point_pattern, &p.vertex_pattern, &p.primitive_pattern, &p.detail_pattern];
        if patterns.iter().all(|text| support::ocaml_trim(text).is_empty()) {
            return refuse("at least one owner pattern is required");
        }
        for text in patterns {
            if !support::ocaml_trim(text).is_empty() {
                support::validate_parameter_pattern("sop/attribute_transfer_all", text)?;
            }
        }
        if p.neighbors < 1 {
            return refuse("neighbors must be positive");
        }
        if ![p.power, p.kernel_radius, p.max_distance, p.blend_width, p.uniform_bias].iter().all(|v| v.is_finite()) {
            return refuse("sampling controls must be finite");
        }
        if p.power < 0.0 || (p.mode == TransferMode::Inverse && p.power == 0.0) {
            return refuse("invalid inverse power");
        }
        let limit = safe_square_limit();
        if p.kernel_radius < 0.0
            || p.kernel_radius > limit
            || p.max_distance < 0.0
            || p.max_distance > limit
            || p.blend_width < 0.0
        {
            return refuse("distances must be nonnegative and safely squarable");
        }
        if p.uniform_bias < 0.0 || p.uniform_bias > 1.0 {
            return refuse("uniform bias must be within [0, 1]");
        }
        if p.distance_mode == KernelMode::Auto && p.blend_width > 0.0 {
            return refuse("blend width requires an explicit maximum distance");
        }
        if p.distance_mode == KernelMode::Explicit && p.max_distance + p.blend_width > limit {
            return refuse("distance plus blend width must be safely squarable");
        }
        Ok(())
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let (source, target) = (inputs[0].clone().unwrap(), inputs[1].clone().unwrap());
        let mode = support::transfer_mode_of(p.mode, p.neighbors, p.power, p.kernel_radius);
        let falloff = support::transfer_falloff_of(p.falloff, p.uniform_bias);
        let max_distance = match p.distance_mode {
            KernelMode::Explicit => Some(p.max_distance),
            KernelMode::Auto => None,
        };
        let (point_pattern, vertex_pattern) = (support::optional_text(&p.point_pattern), support::optional_text(&p.vertex_pattern));
        let (primitive_pattern, detail_pattern) =
            (support::optional_text(&p.primitive_pattern), support::optional_text(&p.detail_pattern));
        let (blend_width, unmatched) = (p.blend_width, p.unmatched);
        Ok(build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                cook_mode: CookMode::Generic,
                dependencies: dependencies::STATIC,
                inputs: vec![source, target],
                ..build::args("attribute_transfer_all")
            },
            None,
            Arc::new(move |_, context, inputs| {
                let params = rdk::attribute_transfer::AllParams {
                    cancel: Some(Arc::clone(context::cancel_token(context))),
                    grain: Some(context::grain(context)),
                    point_pattern: point_pattern.clone(),
                    vertex_pattern: vertex_pattern.clone(),
                    primitive_pattern: primitive_pattern.clone(),
                    detail_pattern: detail_pattern.clone(),
                    mode,
                    max_distance,
                    blend_width,
                    falloff,
                    unmatched,
                    source: inputs[0].clone(),
                    target: inputs[1].clone(),
                };
                match attribute_ops::transfer_all(&params) {
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

// ---------------------------------------------------------------- attribute transfer surface

pub mod attribute_transfer_surface {
    use super::*;

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub target_owner: attribute::Owner,
        /// The encoded (owner, source, target) table, kept as the schema stores it.
        pub attributes: Vec<u8>,
        pub distance_mode: KernelMode,
        pub max_distance: f64,
        pub blend_width: f64,
        pub falloff: TransferFalloff,
        pub uniform_bias: f64,
        pub unmatched: rdk::attribute_transfer::Unmatched,
        pub distance_attribute: String,
        pub source_group: String,
        pub source_group_pattern: String,
        pub source_vertex_group: String,
        pub source_vertex_group_pattern: String,
        pub source_vertex_selection: surface_index::VertexSelection,
        pub target_group: String,
        pub target_group_pattern: String,
    }

    pub fn from_values(v: &[Value]) -> Parameters {
        Parameters {
            target_owner: support::ATTRIBUTE_OWNER.of(&v[0]),
            attributes: bytes(&v[1]).to_vec(),
            distance_mode: support::KERNEL_MODE.of(&v[2]),
            max_distance: float(&v[3]),
            blend_width: float(&v[4]),
            falloff: support::TRANSFER_FALLOFF.of(&v[5]),
            uniform_bias: float(&v[6]),
            unmatched: support::TRANSFER_UNMATCHED.of(&v[7]),
            distance_attribute: text(&v[8]),
            source_group: text(&v[9]),
            source_group_pattern: text(&v[10]),
            source_vertex_group: text(&v[11]),
            source_vertex_group_pattern: text(&v[12]),
            source_vertex_selection: VERTEX_SELECTION.of(&v[13]),
            target_group: text(&v[14]),
            target_group_pattern: text(&v[15]),
        }
    }

    pub fn to_values(p: &Parameters) -> Vec<Value> {
        vec![
            support::ATTRIBUTE_OWNER.value(&p.target_owner),
            Value::Text(p.attributes.clone()),
            support::KERNEL_MODE.value(&p.distance_mode),
            Value::Float(p.max_distance),
            Value::Float(p.blend_width),
            support::TRANSFER_FALLOFF.value(&p.falloff),
            Value::Float(p.uniform_bias),
            support::TRANSFER_UNMATCHED.value(&p.unmatched),
            text_value(&p.distance_attribute),
            text_value(&p.source_group),
            text_value(&p.source_group_pattern),
            text_value(&p.source_vertex_group),
            text_value(&p.source_vertex_group_pattern),
            VERTEX_SELECTION.value(&p.source_vertex_selection),
            text_value(&p.target_group),
            text_value(&p.target_group_pattern),
        ]
    }

    pub const META: Meta = Meta {
        key: "attribute_transfer_surface",
        label: "Attribute Transfer Surface",
        category: "Attribute/Transfer",
        inputs: 2,
        slots: Some("source, target"),
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            Spec {
                label: "Target owner",
                ..decl::spec(
                    "target_owner",
                    support::ELEMENT_ATTRIBUTE_OWNER.kind(),
                    support::ELEMENT_ATTRIBUTE_OWNER.at(0),
                )
            },
            Spec {
                label: "Attributes (owner, source, target)",
                ..decl::spec("attributes", decl::text_kind(), text_value("point\tCd\tCd"))
            },
            Spec {
                label: "Distance mode",
                folder: "Distance",
                ..decl::spec("distance_mode", support::KERNEL_MODE.kind(), support::KERNEL_MODE.value(&KernelMode::Explicit))
            },
            Spec {
                label: "Maximum distance",
                folder: "Distance",
                ..decl::spec("max_distance", decl::float_kind(0.0, 100.0, Some(0.0), None), Value::Float(1.0))
            },
            Spec {
                label: "Blend width",
                folder: "Distance",
                ..decl::spec("blend_width", decl::float_kind(0.0, 100.0, Some(0.0), None), Value::Float(0.0))
            },
            Spec {
                label: "Falloff",
                folder: "Distance",
                ..decl::spec("falloff", support::TRANSFER_FALLOFF.kind(), support::TRANSFER_FALLOFF.at(0))
            },
            Spec {
                label: "Uniform bias",
                folder: "Distance",
                ..decl::spec("uniform_bias", decl::float_kind(0.0, 1.0, Some(0.0), Some(1.0)), Value::Float(0.5))
            },
            Spec {
                label: "Unmatched",
                ..decl::spec("unmatched", support::TRANSFER_UNMATCHED.kind(), support::TRANSFER_UNMATCHED.at(0))
            },
            Spec {
                label: "Distance attribute",
                folder: "Output",
                ..decl::spec("distance_attribute", decl::text_kind(), text_value(""))
            },
            Spec { label: "Source group", folder: "Groups/Source", ..decl::spec("source_group", decl::text_kind(), text_value("")) },
            Spec {
                label: "Source group pattern",
                folder: "Groups/Source",
                ..decl::spec("source_group_pattern", decl::text_kind(), text_value(""))
            },
            Spec {
                label: "Source vertex group",
                folder: "Groups/Source",
                ..decl::spec("source_vertex_group", decl::text_kind(), text_value(""))
            },
            Spec {
                label: "Source vertex group pattern",
                folder: "Groups/Source",
                ..decl::spec("source_vertex_group_pattern", decl::text_kind(), text_value(""))
            },
            Spec {
                label: "Vertex selection",
                folder: "Groups/Source",
                ..decl::spec(
                    "source_vertex_selection",
                    VERTEX_SELECTION.kind(),
                    VERTEX_SELECTION.at(0),
                )
            },
            Spec { label: "Target group", folder: "Groups/Target", ..decl::spec("target_group", decl::text_kind(), text_value("")) },
            Spec {
                label: "Target group pattern",
                folder: "Groups/Target",
                ..decl::spec("target_group_pattern", decl::text_kind(), text_value(""))
            },
        ]
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        let refuse = |message: &str| Err(format!("sop/attribute_transfer_surface: {message}"));
        if p.target_owner == attribute::Owner::Detail {
            return refuse("detail target is not spatial");
        }
        let attributes = support::decode_surface_attributes(&p.attributes)?;
        let names: Vec<&str> = attributes.iter().map(|attribute| attribute.target_name.as_str()).collect();
        let mut sorted = names.clone();
        sorted.sort();
        sorted.dedup();
        if sorted.len() != names.len() {
            return refuse("target attribute names must be distinct");
        }
        if p.target_owner == attribute::Owner::Point && names.contains(&"P") {
            return refuse("target P is not an ordinary attribute");
        }
        let distance = support::optional_text(&p.distance_attribute);
        let conflicts = distance.as_deref().is_some_and(|name| names.contains(&name));
        if (p.target_owner == attribute::Owner::Point && distance.as_deref() == Some("P")) || conflicts {
            return refuse("distance output conflicts with attributes");
        }
        if !(p.max_distance.is_finite() && p.blend_width.is_finite() && p.uniform_bias.is_finite()) {
            return refuse("distance controls must be finite");
        }
        if p.max_distance < 0.0
            || p.max_distance > safe_square_limit()
            || p.blend_width < 0.0
            || p.uniform_bias < 0.0
            || p.uniform_bias > 1.0
        {
            return refuse("distance controls are out of range");
        }
        if p.distance_mode == KernelMode::Auto && p.blend_width > 0.0 {
            return refuse("blend width requires an explicit maximum distance");
        }
        if p.distance_mode == KernelMode::Explicit && p.max_distance + p.blend_width > safe_square_limit() {
            return refuse("distance window exceeds the squared-distance range");
        }
        for pattern in [&p.source_group_pattern, &p.source_vertex_group_pattern, &p.target_group_pattern] {
            if !support::ocaml_trim(pattern).is_empty() {
                support::validate_parameter_pattern("attribute_transfer_surface", pattern)?;
            }
        }
        Ok(())
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let (source, target) = (inputs[0].clone().unwrap(), inputs[1].clone().unwrap());
        let attributes = support::decode_surface_attributes(&p.attributes).expect("validated");
        let target_owner = p.target_owner;
        let max_distance = match p.distance_mode {
            KernelMode::Auto => None,
            KernelMode::Explicit => Some(p.max_distance),
        };
        let falloff = support::transfer_falloff_of(p.falloff, p.uniform_bias);
        let distance_attribute = support::optional_text(&p.distance_attribute);
        let (source_group, source_group_pattern) = support::exact_or_pattern(&p.source_group, &p.source_group_pattern);
        let (source_vertex_group, source_vertex_group_pattern) =
            support::exact_or_pattern(&p.source_vertex_group, &p.source_vertex_group_pattern);
        let (target_group, target_group_pattern) = support::exact_or_pattern(&p.target_group, &p.target_group_pattern);
        let source_pattern =
            support::compile_transfer_group_pattern("attribute_transfer_surface", "source", &source_group_pattern)
                .expect("validated");
        let source_vertex_pattern = support::compile_transfer_group_pattern(
            "attribute_transfer_surface",
            "source vertex",
            &source_vertex_group_pattern,
        )
        .expect("validated");
        let target_pattern =
            support::compile_transfer_group_pattern("attribute_transfer_surface", "target", &target_group_pattern)
                .expect("validated");
        let target_group_owner = match target_owner {
            attribute::Owner::Point => group::Owner::Point,
            attribute::Owner::Vertex => group::Owner::Vertex,
            attribute::Owner::Primitive => group::Owner::Primitive,
            attribute::Owner::Detail => unreachable!("detail targets are refused by validate"),
        };
        let target_owner_name = match target_group_owner {
            group::Owner::Primitive => "primitive",
            group::Owner::Vertex => "vertex",
            group::Owner::Point => "point",
        };
        let (blend_width, unmatched, source_vertex_selection) = (p.blend_width, p.unmatched, p.source_vertex_selection);
        
        Ok(build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                version: 5,
                cook_mode: CookMode::Generic,
                dependencies: dependencies::STATIC,
                inputs: vec![source, target],
                ..build::args("attribute_transfer_surface")
            },
            None,
            Arc::new(move |_, context, inputs| {
                let cancel = context::cancel_token(context);
                let grain = context::grain(context);
                let source_primitives = support::resolve_transfer_group(
                    "attribute_transfer_surface",
                    group::Owner::Primitive,
                    "primitive",
                    &source_group,
                    &source_pattern,
                    cancel,
                    grain,
                    &inputs[0],
                )?;
                let source_vertices = support::resolve_transfer_group(
                    "attribute_transfer_surface",
                    group::Owner::Vertex,
                    "vertex",
                    &source_vertex_group,
                    &source_vertex_pattern,
                    cancel,
                    grain,
                    &inputs[0],
                )?;
                let target_elements = support::resolve_transfer_group(
                    "attribute_transfer_surface",
                    target_group_owner,
                    target_owner_name,
                    &target_group,
                    &target_pattern,
                    cancel,
                    grain,
                    &inputs[1],
                )?;
                let params = rdk::attribute_transfer::SurfaceParams {
                    cancel: Some(Arc::clone(cancel)),
                    grain: Some(grain),
                    max_distance,
                    blend_width,
                    falloff,
                    unmatched,
                    target_owner,
                    distance_attribute: distance_attribute.clone(),
                    source_primitives,
                    source_vertices,
                    source_vertex_selection,
                    target_elements,
                    attributes: attributes.clone(),
                    source: inputs[0].clone(),
                    target: inputs[1].clone(),
                };
                match attribute_ops::transfer_surface(&params) {
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

// ---------------------------------------------------------------- attribute transfer

pub mod attribute_transfer {
    use super::*;

    /// The exact names table: one column per row.
    pub fn decode_names(text: &[u8]) -> Result<Vec<String>, String> {
        support::parameter_table_rows("sop/attribute_transfer", text)?
            .into_iter()
            .map(|row| match row.as_slice() {
                [name] => Ok(name.clone()),
                _ => Err("sop/attribute_transfer: exact names need one column per row".to_owned()),
            })
            .collect()
    }

    #[derive(Clone, Debug)]
    pub struct Parameters {
        pub owner: attribute::Owner,
        pub use_names: bool,
        /// The encoded exact-names table, kept as the schema stores it.
        pub names: Vec<u8>,
        pub pattern: String,
        pub mode: TransferMode,
        pub neighbors: i64,
        pub power: f64,
        pub kernel_radius: f64,
        pub distance_mode: KernelMode,
        pub max_distance: f64,
        pub blend_width: f64,
        pub falloff: TransferFalloff,
        pub uniform_bias: f64,
        pub unmatched: rdk::attribute_transfer::Unmatched,
        pub source_group: String,
        pub source_group_pattern: String,
        pub source_vertex_group: String,
        pub source_vertex_group_pattern: String,
        pub source_vertex_selection: surface_index::VertexSelection,
        pub target_group: String,
        pub target_group_pattern: String,
    }

    pub fn from_values(v: &[Value]) -> Parameters {
        Parameters {
            owner: support::ATTRIBUTE_OWNER.of(&v[0]),
            use_names: boolean(&v[1]),
            names: bytes(&v[2]).to_vec(),
            pattern: text(&v[3]),
            mode: support::TRANSFER_MODE.of(&v[4]),
            neighbors: int(&v[5]),
            power: float(&v[6]),
            kernel_radius: float(&v[7]),
            distance_mode: support::KERNEL_MODE.of(&v[8]),
            max_distance: float(&v[9]),
            blend_width: float(&v[10]),
            falloff: support::TRANSFER_FALLOFF.of(&v[11]),
            uniform_bias: float(&v[12]),
            unmatched: support::TRANSFER_UNMATCHED.of(&v[13]),
            source_group: text(&v[14]),
            source_group_pattern: text(&v[15]),
            source_vertex_group: text(&v[16]),
            source_vertex_group_pattern: text(&v[17]),
            source_vertex_selection: VERTEX_SELECTION.of(&v[18]),
            target_group: text(&v[19]),
            target_group_pattern: text(&v[20]),
        }
    }

    pub fn to_values(p: &Parameters) -> Vec<Value> {
        vec![
            support::ATTRIBUTE_OWNER.value(&p.owner),
            Value::Bool(p.use_names),
            Value::Text(p.names.clone()),
            text_value(&p.pattern),
            support::TRANSFER_MODE.value(&p.mode),
            Value::Int(p.neighbors),
            Value::Float(p.power),
            Value::Float(p.kernel_radius),
            support::KERNEL_MODE.value(&p.distance_mode),
            Value::Float(p.max_distance),
            Value::Float(p.blend_width),
            support::TRANSFER_FALLOFF.value(&p.falloff),
            Value::Float(p.uniform_bias),
            support::TRANSFER_UNMATCHED.value(&p.unmatched),
            text_value(&p.source_group),
            text_value(&p.source_group_pattern),
            text_value(&p.source_vertex_group),
            text_value(&p.source_vertex_group_pattern),
            VERTEX_SELECTION.value(&p.source_vertex_selection),
            text_value(&p.target_group),
            text_value(&p.target_group_pattern),
        ]
    }

    pub const META: Meta = Meta {
        key: "attribute_transfer",
        label: "Attribute Transfer",
        category: "Attribute/Transfer",
        inputs: 2,
        slots: Some("source, target"),
        validate: Some(validate),
        ..decl::META
    };

    pub fn specs() -> Vec<Spec> {
        vec![
            Spec {
                label: "Owner",
                ..decl::spec(
                    "owner",
                    support::ATTRIBUTE_OWNER.kind(),
                    support::ATTRIBUTE_OWNER.at(0),
                )
            },
            Spec { label: "Use exact names", ..decl::spec("use_names", decl::toggle_kind(), Value::Bool(false)) },
            Spec { label: "Exact attribute names", ..decl::spec("names", decl::text_kind(), text_value("")) },
            Spec { label: "Attributes", ..decl::spec("pattern", decl::text_kind(), text_value("*")) },
            Spec {
                label: "Transfer mode",
                ..decl::spec("mode", support::TRANSFER_MODE.kind(), support::TRANSFER_MODE.at(0))
            },
            Spec {
                label: "Neighbors",
                folder: "Sampling",
                ..decl::spec("neighbors", decl::int_kind(1, 128, Some(1), None), Value::Int(4))
            },
            Spec {
                label: "Inverse power",
                folder: "Sampling",
                ..decl::spec("power", decl::float_kind(0.0, 10.0, Some(0.0), None), Value::Float(2.0))
            },
            Spec {
                label: "Kernel radius",
                folder: "Sampling",
                ..decl::spec("kernel_radius", decl::float_kind(0.0, 100.0, Some(0.0), None), Value::Float(1.0))
            },
            Spec {
                label: "Distance mode",
                folder: "Distance",
                ..decl::spec("distance_mode", support::KERNEL_MODE.kind(), support::KERNEL_MODE.value(&KernelMode::Explicit))
            },
            Spec {
                label: "Maximum distance",
                folder: "Distance",
                ..decl::spec("max_distance", decl::float_kind(0.0, 100.0, Some(0.0), None), Value::Float(1.0))
            },
            Spec {
                label: "Blend width",
                folder: "Distance",
                ..decl::spec("blend_width", decl::float_kind(0.0, 100.0, Some(0.0), None), Value::Float(0.0))
            },
            Spec {
                label: "Falloff",
                folder: "Distance",
                ..decl::spec("falloff", support::TRANSFER_FALLOFF.kind(), support::TRANSFER_FALLOFF.at(0))
            },
            Spec {
                label: "Uniform bias",
                folder: "Distance",
                ..decl::spec("uniform_bias", decl::float_kind(0.0, 1.0, Some(0.0), Some(1.0)), Value::Float(0.5))
            },
            Spec {
                label: "Unmatched",
                ..decl::spec("unmatched", support::TRANSFER_UNMATCHED.kind(), support::TRANSFER_UNMATCHED.at(0))
            },
            Spec { label: "Source group", folder: "Groups/Source", ..decl::spec("source_group", decl::text_kind(), text_value("")) },
            Spec {
                label: "Source group pattern",
                folder: "Groups/Source",
                ..decl::spec("source_group_pattern", decl::text_kind(), text_value(""))
            },
            Spec {
                label: "Source vertex group",
                folder: "Groups/Source",
                ..decl::spec("source_vertex_group", decl::text_kind(), text_value(""))
            },
            Spec {
                label: "Source vertex pattern",
                folder: "Groups/Source",
                ..decl::spec("source_vertex_group_pattern", decl::text_kind(), text_value(""))
            },
            Spec {
                label: "Vertex selection",
                folder: "Groups/Source",
                ..decl::spec(
                    "source_vertex_selection",
                    VERTEX_SELECTION.kind(),
                    VERTEX_SELECTION.at(0),
                )
            },
            Spec { label: "Target group", folder: "Groups/Target", ..decl::spec("target_group", decl::text_kind(), text_value("")) },
            Spec {
                label: "Target group pattern",
                folder: "Groups/Target",
                ..decl::spec("target_group_pattern", decl::text_kind(), text_value(""))
            },
        ]
    }

    pub fn validate(values: &[Value]) -> Result<(), String> {
        let p = from_values(values);
        let refuse = |message: &str| Err(format!("sop/attribute_transfer: {message}"));
        if p.use_names {
            let names = decode_names(&p.names)?;
            for name in &names {
                if support::ocaml_trim(name).is_empty() {
                    return refuse("empty attribute name");
                }
                if p.owner == attribute::Owner::Point && name == "P" {
                    return refuse("canonical P cannot be transferred as an ordinary attribute");
                }
            }
            let mut sorted = names.clone();
            sorted.sort();
            sorted.dedup();
            if sorted.len() != names.len() {
                return refuse("duplicate attribute name");
            }
        } else {
            support::validate_parameter_pattern("sop/attribute_transfer", &p.pattern)?;
        }
        if p.neighbors < 1 {
            return refuse("neighbors must be positive");
        }
        if p.power < 0.0 || (p.mode == TransferMode::Inverse && p.power == 0.0) {
            return refuse("invalid inverse power");
        }
        let limit = safe_square_limit();
        if p.kernel_radius < 0.0 || p.kernel_radius > limit || p.max_distance < 0.0 || p.max_distance > limit || p.blend_width < 0.0 {
            return refuse("distances must be nonnegative and safely squarable");
        }
        if p.uniform_bias < 0.0 || p.uniform_bias > 1.0 {
            return refuse("uniform bias must be within [0,1]");
        }
        if p.distance_mode == KernelMode::Auto && p.blend_width > 0.0 {
            return refuse("blend width requires an explicit maximum distance");
        }
        if p.distance_mode == KernelMode::Explicit && p.max_distance + p.blend_width > limit {
            return refuse("distance plus blend width must be safely squarable");
        }
        let (source_group, source_pattern) = support::exact_or_pattern(&p.source_group, &p.source_group_pattern);
        let (vertex_group, vertex_pattern) =
            support::exact_or_pattern(&p.source_vertex_group, &p.source_vertex_group_pattern);
        let (target_group, target_pattern) = support::exact_or_pattern(&p.target_group, &p.target_group_pattern);
        for (name, pattern) in [("source", &source_pattern), ("source vertex", &vertex_pattern), ("target", &target_pattern)] {
            support::compile_transfer_group_pattern("attribute_transfer", name, pattern)?;
        }
        if p.owner != attribute::Owner::Vertex && (vertex_group.is_some() || vertex_pattern.is_some()) {
            return refuse("source vertex groups require vertex ownership");
        }
        if p.owner == attribute::Owner::Detail
            && (source_group.is_some()
                || source_pattern.is_some()
                || target_group.is_some()
                || target_pattern.is_some()
                || p.distance_mode != KernelMode::Auto
                || p.blend_width != 0.0)
        {
            return refuse("detail transfer does not accept spatial options");
        }
        if matches!(p.owner, attribute::Owner::Detail | attribute::Owner::Vertex) && p.mode != TransferMode::Nearest {
            return refuse("this owner supports nearest mode only");
        }
        Ok(())
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p = from_values(values);
        let (source, target) = (inputs[0].clone().unwrap(), inputs[1].clone().unwrap());
        let owner = p.owner;
        let source_vertex_selection = p.source_vertex_selection;
        let (names, pattern) = if p.use_names {
            (Some(decode_names(&p.names).expect("validated")), None)
        } else {
            (None, Some(p.pattern.clone()))
        };
        let mode = support::transfer_mode_of(p.mode, p.neighbors, p.power, p.kernel_radius);
        let falloff = support::transfer_falloff_of(p.falloff, p.uniform_bias);
        let (unmatched, blend_width) = (p.unmatched, p.blend_width);
        let max_distance = match p.distance_mode {
            KernelMode::Auto => None,
            KernelMode::Explicit => Some(p.max_distance),
        };
        let (source_group, source_group_pattern) = support::exact_or_pattern(&p.source_group, &p.source_group_pattern);
        let (source_vertex_group, source_vertex_group_pattern) =
            support::exact_or_pattern(&p.source_vertex_group, &p.source_vertex_group_pattern);
        let (target_group, target_group_pattern) = support::exact_or_pattern(&p.target_group, &p.target_group_pattern);
        let source_pattern = support::compile_transfer_group_pattern("attribute_transfer", "source", &source_group_pattern)
            .expect("validated");
        let source_vertex_pattern =
            support::compile_transfer_group_pattern("attribute_transfer", "source vertex", &source_vertex_group_pattern)
                .expect("validated");
        let target_pattern = support::compile_transfer_group_pattern("attribute_transfer", "target", &target_group_pattern)
            .expect("validated");
        Ok(build::make_geometry(
            build::Args {
                label: Some(label.to_owned()),
                version: 6,
                cook_mode: CookMode::Generic,
                dependencies: dependencies::STATIC,
                inputs: vec![source, target],
                ..build::args("attribute_transfer")
            },
            None,
            Arc::new(move |_, context, inputs| {
                let (source_group_owner, target_group_owner, source_owner_name, target_owner_name) = match owner {
                    attribute::Owner::Point => (group::Owner::Point, group::Owner::Point, "point", "point"),
                    attribute::Owner::Vertex => (group::Owner::Primitive, group::Owner::Vertex, "primitive", "vertex"),
                    attribute::Owner::Primitive => {
                        (group::Owner::Primitive, group::Owner::Primitive, "primitive", "primitive")
                    }
                    attribute::Owner::Detail => (group::Owner::Point, group::Owner::Point, "detail", "detail"),
                };
                let cancel = context::cancel_token(context);
                let grain = context::grain(context);
                let source_elements = support::resolve_transfer_group(
                    "attribute_transfer",
                    source_group_owner,
                    source_owner_name,
                    &source_group,
                    &source_pattern,
                    cancel,
                    grain,
                    &inputs[0],
                )?;
                let source_vertices = support::resolve_transfer_group(
                    "attribute_transfer",
                    group::Owner::Vertex,
                    "vertex",
                    &source_vertex_group,
                    &source_vertex_pattern,
                    cancel,
                    grain,
                    &inputs[0],
                )?;
                let target_elements = support::resolve_transfer_group(
                    "attribute_transfer",
                    target_group_owner,
                    target_owner_name,
                    &target_group,
                    &target_pattern,
                    cancel,
                    grain,
                    &inputs[1],
                )?;
                let (source, target) = (inputs[0].clone(), inputs[1].clone());
                let result = match owner {
                    attribute::Owner::Point => attribute_ops::transfer_points(&rdk::attribute_transfer::SpatialParams {
                        cancel: Some(Arc::clone(cancel)),
                        grain: Some(grain),
                        names: names.clone(),
                        pattern: pattern.clone(),
                        mode,
                        max_distance,
                        blend_width,
                        falloff,
                        unmatched,
                        source_elements,
                        target_elements,
                        source,
                        target,
                    }),
                    attribute::Owner::Vertex => attribute_ops::transfer_vertices(&rdk::attribute_transfer::VerticesParams {
                        cancel: Some(Arc::clone(cancel)),
                        grain: Some(grain),
                        names: names.clone(),
                        pattern: pattern.clone(),
                        max_distance,
                        blend_width,
                        falloff,
                        unmatched,
                        source_primitives: source_elements,
                        source_vertices,
                        source_vertex_selection,
                        target_vertices: target_elements,
                        source,
                        target,
                    }),
                    attribute::Owner::Primitive => {
                        attribute_ops::transfer_primitives(&rdk::attribute_transfer::SpatialParams {
                            cancel: Some(Arc::clone(cancel)),
                            grain: Some(grain),
                            names: names.clone(),
                            pattern: pattern.clone(),
                            mode,
                            max_distance,
                            blend_width,
                            falloff,
                            unmatched,
                            source_elements,
                            target_elements,
                            source,
                            target,
                        })
                    }
                    attribute::Owner::Detail => attribute_ops::transfer_detail(&rdk::attribute_transfer::DetailParams {
                        names: names.clone(),
                        pattern: pattern.clone(),
                        source,
                        target,
                    }),
                };
                match result {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{edit_graph, image_nodes, node};
    use crate::test_support::*;

    fn text_arg(name: &str, text: &str) -> (String, Value) {
        (name.to_owned(), text_value(text))
    }

    #[test]
    fn every_node_declares_its_fields_in_ocaml_order() {
        let names = |factory: &Factory| -> Vec<String> {
            edit_graph::factory_fields(factory).iter().map(|f| f.name.clone()).collect()
        };
        assert_eq!(names(&image_texels::factory()), Vec::<String>::new());
        assert_eq!(names(&uv_flatten::factory()), ["name", "seams", "iterations", "tolerance"]);
        assert_eq!(names(&uv_relax::factory()), ["name", "seams", "uv_tolerance", "iterations", "tolerance"]);
        assert_eq!(names(&distance_from_geometry::factory()).len(), 10);
        assert_eq!(names(&attribute_mirror::factory()).len(), 26);
        assert_eq!(names(&attribute_transfer_all::factory()).len(), 14);
        assert_eq!(names(&attribute_transfer_surface::factory()).len(), 16);
        assert_eq!(names(&attribute_transfer::factory()).len(), 21);
    }

    #[test]
    fn image_texels_cooks_one_point_per_texel_from_an_image() {
        let noise = edit_graph::instantiate(&image_nodes::noise_factory(), &[]).unwrap();
        let (small, _) = node::apply_parameters(
            &noise,
            &[("width".to_owned(), Value::Int(4)), ("height".to_owned(), Value::Int(3))],
        )
        .unwrap();
        let texels = edit_graph::instantiate(&image_texels::factory(), &[small]).unwrap();
        let mut s = session(8);
        let g = geometry_of(&cook(&mut s, 1, &texels));
        assert_eq!(rdk::geometry::point_count(&g), 12);
        let rgba = rdk::geometry::find_attribute(rdk::attribute::Owner::Point, "rgba", &g).expect("rgba");
        assert!(matches!(rdk::attribute::View::storage(&rgba), Ok(rdk::attribute::Storage::Float4(_))));
        crate::session::close(&mut s);
    }

    #[test]
    fn distance_from_geometry_refuses_a_zero_fixed_radius_and_clashing_outputs() {
        let n = edit_graph::instantiate(
            &distance_from_geometry::factory(),
            &[snapshot(&fixture()), snapshot(&fixture())],
        )
        .unwrap();
        let fixed = node::apply_parameters(&n, &[("radius_mode".to_owned(), support::DISTANCE_RADIUS.value(&DistanceRadiusMode::Fixed))]).unwrap().0;
        let zero = node::apply_parameters(&fixed, &[("radius".to_owned(), Value::Float(0.0))]);
        let message = zero.err().expect("a zero fixed radius is refused");
        assert!(message.contains("fixed radius must be positive and finite"), "{message}");
        let clash = node::apply_parameters(&n, &[text_arg("mask_attribute", "distance")]);
        let message = clash.err().expect("duplicate output names are refused");
        assert!(message.contains("output names must be present, distinct and cannot be P"), "{message}");
    }

    #[test]
    fn attribute_mirror_refuses_a_mapping_without_a_mapping_attribute() {
        let n = edit_graph::instantiate(&attribute_mirror::factory(), &[snapshot(&fixture())]).unwrap();
        let message = node::apply_parameters(
            &n,
            &[
                ("method_".to_owned(), MIRROR_METHOD.at(1)),
                text_arg("mapping_attribute", "  "),
            ],
        )
        .err()
        .expect("empty mapping attribute is refused");
        assert!(message.contains("empty mapping attribute"), "{message}");
    }

    #[test]
    fn attribute_transfer_refuses_duplicate_exact_names() {
        let n = edit_graph::instantiate(&attribute_transfer::factory(), &[snapshot(&fixture()), snapshot(&fixture())]).unwrap();
        let table = support::encode_table(&[vec![b"Cd".to_vec()], vec![b"Cd".to_vec()]]);
        let message = node::apply_parameters(
            &n,
            &[
                ("use_names".to_owned(), Value::Bool(true)),
                ("names".to_owned(), Value::Text(table)),
            ],
        )
        .err()
        .expect("duplicate names are refused");
        assert!(message.contains("duplicate attribute name"), "{message}");
    }

    #[test]
    fn attribute_transfer_all_cooks_its_defaults_between_two_grids() {
        let n = edit_graph::instantiate(
            &attribute_transfer_all::factory(),
            &[snapshot(&fixture()), snapshot(&with_color(2.0, &fixture()))],
        )
        .unwrap();
        let mut s = session(8);
        let g = geometry_of(&cook(&mut s, 1, &n));
        assert_eq!(rdk::geometry::point_count(&g), 9);
        crate::session::close(&mut s);
    }
}
