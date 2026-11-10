//! the attribute, UV and transfer nodes, declared once each (Image
//! Texels is the carrier that lets a Lisp body sample pixels). See PORTING.md.
use std::sync::Arc;

use param::Value;
use rdk_core::{attribute, geometry, group, packed};
use rdk_geom::surface_index;
use rdk_ops::{attribute_ops, transform_ops, uv_ops};

use crate::context::{self, dependencies};
use crate::decl::{self, Meta, Spec};
use crate::edit_graph::Factory;
use crate::node::{self, build, Cooked, CookMode};
use crate::payload;
use crate::support::{self, Choice, DistanceRadiusMode, KernelMode, MirrorMethod, MirrorTransform, TransferMode};

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
    Value::Text(value.as_bytes().into())
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

// Choice row order is user-visible and remains authored order.
const REFERENCE: Choice<transform_ops::DistanceFromGeometryReference> = Choice(&[
    ("Points", transform_ops::DistanceFromGeometryReference::DistanceReferencePoints),
    ("Primitives", transform_ops::DistanceFromGeometryReference::DistanceReferencePrimitives),
]);
const GROUP_USE: Choice<rdk_ops::attribute_mirror::GroupUse> = Choice(&[
    ("Group is source", rdk_ops::attribute_mirror::GroupUse::MirrorGroupAsSource),
    ("Group is destination", rdk_ops::attribute_mirror::GroupUse::MirrorGroupAsDestination),
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
                let rgba = crate::image::storage(&image)?;
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
                .map_err(|e| if rdk_core::error::code(&e) == rdk_core::error::Code::ArrayMemory { support::structured_rdk_error(&e) } else { fail(rdk_core::error::to_string(&e)) })?;
                let plane = |k: usize| -> Result<Vec<f64>, rays_diagnostic::Diagnostic> {
                    let mut values = Vec::new();
                    values.try_reserve_exact(count as usize).map_err(|_| support::diagnostic("E_ARRAY_MEMORY", "Image texel allocation failed."))?;
                    values.extend((0..count as usize).map(|i| rgba[4 * i + k]));
                    Ok(values)
                };
                let values = packed::float4::of_owned(plane(0)?, plane(1)?, plane(2)?, plane(3)?)
                    .map_err(|e| fail(rdk_core::error::to_string(&e)))?;
                let attribute_value = attribute::create_owned("rgba", attribute::Owner::Point, attribute::Storage::Float4(values))
                    .map_err(|e| fail(rdk_core::error::to_string(&e)))?;
                geometry_value = geometry::with_attribute(attribute_value, &geometry_value)
                    .map_err(|e| fail(rdk_core::error::to_string(&e)))?;
                Ok(Cooked { payload: payload::T::Geometry(geometry_value), instances: None })
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- UV flatten and relax

pub mod uv_flatten {
    use super::*;

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
        let p_name = text(&values[0]);
        let p_seams = text(&values[1]);
        let p_iterations = int(&values[2]);
        let p_tolerance = float(&values[3]);
        let input = inputs[0].clone().unwrap();
        let (name, seams_name, iterations, tolerance) =
            (p_name, support::optional_text(&p_seams), p_iterations, p_tolerance);
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

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

pub mod uv_relax {
    use super::*;

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
        let p_name = text(&values[0]);
        let p_seams = text(&values[1]);
        let p_uv_tolerance = float(&values[2]);
        let p_iterations = int(&values[3]);
        let p_tolerance = float(&values[4]);
        let input = inputs[0].clone().unwrap();
        let (name, seams_name, uv_tolerance, iterations, tolerance) =
            (p_name, support::optional_text(&p_seams), p_uv_tolerance, p_iterations, p_tolerance);
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

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- distance from geometry

pub mod distance_from_geometry {
    use super::*;

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
                    support::ELEMENT_OWNER.value(&rdk_ops::group_ops::Owner::GroupPoints),
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
                    support::ELEMENT_OWNER.value(&rdk_ops::group_ops::Owner::GroupPrimitives),
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
        let p_radius_mode = support::DISTANCE_RADIUS.of(&values[6]);
        let p_radius = float(&values[7]);
        let p_distance_attribute = text(&values[8]);
        let p_mask_attribute = text(&values[9]);
        if !p_radius.is_finite()
            || p_radius < 0.0
            || (matches!(p_radius_mode, DistanceRadiusMode::Fixed) && p_radius == 0.0)
        {
            return Err("sop/distance_from_geometry: fixed radius must be positive and finite".to_owned());
        }
        let mut names: Vec<String> = [&p_distance_attribute, &p_mask_attribute]
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
        let p_affected_owner = support::ELEMENT_OWNER.of(&values[0]);
        let p_affected_group = text(&values[1]);
        let p_reference_owner = support::ELEMENT_OWNER.of(&values[2]);
        let p_reference_group = text(&values[3]);
        let p_reference_kind = REFERENCE.of(&values[4]);
        let p_falloff = support::SOFT_FALLOFF.of(&values[5]);
        let p_radius_mode = support::DISTANCE_RADIUS.of(&values[6]);
        let p_radius = float(&values[7]);
        let p_distance_attribute = text(&values[8]);
        let p_mask_attribute = text(&values[9]);
        let (source, reference) = (inputs[0].clone().unwrap(), inputs[1].clone().unwrap());
        let affected = support::optional_element_group(p_affected_owner, &p_affected_group);
        let reference_selection = support::optional_element_group(p_reference_owner, &p_reference_group);
        let radius = match p_radius_mode {
            DistanceRadiusMode::Fixed => transform_ops::DistanceAlongRadius::DistanceFixed(p_radius),
            DistanceRadiusMode::Maximum => transform_ops::DistanceAlongRadius::DistanceMaximum,
        };
        let (reference_kind, falloff) = (p_reference_kind, p_falloff);
        let distance_attribute = support::optional_text(&p_distance_attribute);
        let mask_attribute = support::optional_text(&p_mask_attribute);
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

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- attribute mirror

pub mod attribute_mirror {
    use super::*;

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
        let p_owner = support::ELEMENT_ATTRIBUTE_OWNER.of(&values[0]);
        let p_attributes = text(&values[1]);
        let p_method_ = MIRROR_METHOD.of(&values[4]);
        let p_origin_x = float(&values[5]);
        let p_origin_y = float(&values[6]);
        let p_origin_z = float(&values[7]);
        let p_normal_x = float(&values[8]);
        let p_normal_y = float(&values[9]);
        let p_normal_z = float(&values[10]);
        let p_distance = float(&values[11]);
        let p_tolerance = float(&values[12]);
        let p_mapping_attribute = text(&values[13]);
        let p_mapping_destination_group = text(&values[14]);
        let p_transform = MIRROR_TRANSFORM.of(&values[15]);
        let p_uv_origin_u = float(&values[16]);
        let p_uv_origin_v = float(&values[17]);
        let p_uv_direction_u = float(&values[18]);
        let p_uv_direction_v = float(&values[19]);
        let p_replace_strings = boolean(&values[20]);
        let p_string_search = bytes(&values[21]).to_vec();
        let p_source_group = text(&values[24]);
        let p_destination_group = text(&values[25]);
        let finite = |value: f64| -> Result<(), String> {
            if value.is_finite() { Ok(()) } else { Err("sop/attribute_mirror: controls must be finite".to_owned()) }
        };
        for value in [
            p_origin_x,
            p_origin_y,
            p_origin_z,
            p_normal_x,
            p_normal_y,
            p_normal_z,
            p_distance,
            p_tolerance,
            p_uv_origin_u,
            p_uv_origin_v,
            p_uv_direction_u,
            p_uv_direction_v,
        ] {
            finite(value)?;
        }
        if p_distance < 0.0 || p_tolerance < 0.0 || p_tolerance > safe_square_limit() {
            return Err("sop/attribute_mirror: invalid distance or tolerance".to_owned());
        }
        if p_replace_strings && p_string_search.is_empty() {
            return Err("sop/attribute_mirror: empty string search".to_owned());
        }
        if let (Some(source), Some(destination)) =
            (support::optional_text(&p_source_group), support::optional_text(&p_destination_group))
        {
            if source == destination {
                return Err("sop/attribute_mirror: source and destination groups must differ".to_owned());
            }
        }
        if p_transform == MirrorTransform::Uv && p_uv_direction_u == 0.0 && p_uv_direction_v == 0.0 {
            return Err("sop/attribute_mirror: UV direction must be non-zero".to_owned());
        }
        support::validate_parameter_pattern("sop/attribute_mirror: attributes", &p_attributes)?;
        match p_method_ {
            MirrorMethod::Mapping => {
                if p_mapping_attribute.trim().is_empty() {
                    return Err("sop/attribute_mirror: empty mapping attribute".to_owned());
                }
                if p_mapping_destination_group.trim().is_empty() {
                    return Err("sop/attribute_mirror: empty mapping destination group".to_owned());
                }
                if matches!(p_transform, MirrorTransform::Vector | MirrorTransform::Point) {
                    return Err("sop/attribute_mirror: vector/point transforms require a plane".to_owned());
                }
                Ok(())
            }
            MirrorMethod::Plane => {
                if p_owner == attribute::Owner::Vertex {
                    return Err("sop/attribute_mirror: vertex owner requires mapping".to_owned());
                }
                let scale = p_normal_x.abs().max(p_normal_y.abs()).max(p_normal_z.abs());
                if scale == 0.0 {
                    return Err("sop/attribute_mirror: normal must be non-zero".to_owned());
                }
                let (x, y, z) = (p_normal_x / scale, p_normal_y / scale, p_normal_z / scale);
                let length = (x * x + y * y + z * z).sqrt();
                for value in [
                    p_origin_x + p_distance * (x / length),
                    p_origin_y + p_distance * (y / length),
                    p_origin_z + p_distance * (z / length),
                ] {
                    finite(value)?;
                }
                Ok(())
            }
        }
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_owner = support::ELEMENT_ATTRIBUTE_OWNER.of(&values[0]);
        let p_attributes = text(&values[1]);
        let p_group = text(&values[2]);
        let p_group_use = GROUP_USE.of(&values[3]);
        let p_method_ = MIRROR_METHOD.of(&values[4]);
        let p_origin_x = float(&values[5]);
        let p_origin_y = float(&values[6]);
        let p_origin_z = float(&values[7]);
        let p_normal_x = float(&values[8]);
        let p_normal_y = float(&values[9]);
        let p_normal_z = float(&values[10]);
        let p_distance = float(&values[11]);
        let p_tolerance = float(&values[12]);
        let p_mapping_attribute = text(&values[13]);
        let p_mapping_destination_group = text(&values[14]);
        let p_transform = MIRROR_TRANSFORM.of(&values[15]);
        let p_uv_origin_u = float(&values[16]);
        let p_uv_origin_v = float(&values[17]);
        let p_uv_direction_u = float(&values[18]);
        let p_uv_direction_v = float(&values[19]);
        let p_replace_strings = boolean(&values[20]);
        let p_string_search = bytes(&values[21]).to_vec();
        let p_string_replacement = bytes(&values[22]).to_vec();
        let p_output_mapping = text(&values[23]);
        let p_source_group = text(&values[24]);
        let p_destination_group = text(&values[25]);
        let input = inputs[0].clone().unwrap();
        let origin = rays_math::vec3::create(p_origin_x, p_origin_y, p_origin_z);
        let normal = rays_math::vec3::create(p_normal_x, p_normal_y, p_normal_z);
        let transform = match p_transform {
            MirrorTransform::Copy => rdk_ops::attribute_mirror::Transform::MirrorCopy,
            MirrorTransform::Uv => rdk_ops::attribute_mirror::Transform::MirrorUv {
                origin_u: p_uv_origin_u,
                origin_v: p_uv_origin_v,
                direction_u: p_uv_direction_u,
                direction_v: p_uv_direction_v,
            },
            MirrorTransform::Vector => rdk_ops::attribute_mirror::Transform::MirrorVector,
            MirrorTransform::Point => rdk_ops::attribute_mirror::Transform::MirrorPoint,
        };
        let group_name = support::optional_text(&p_group);
        let output_mapping = support::optional_text(&p_output_mapping);
        let source_group = support::optional_text(&p_source_group);
        let destination_group = support::optional_text(&p_destination_group);
        let string_replace = p_replace_strings.then(|| (p_string_search.clone(), p_string_replacement.clone()));
        let owner = p_owner;
        let (group_use, method_kind) = (p_group_use, p_method_);
        let (distance, tolerance) = (p_distance, p_tolerance);
        let (mapping_attribute, mapping_destination) = (p_mapping_attribute.clone(), p_mapping_destination_group.clone());
        let attributes = p_attributes.clone();
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
                            &format!("attribute_mirror could not find selection group {}", rdk_core::attribute::quote(name)),
                        )
                    })?),
                };
                let method = match method_kind {
                    MirrorMethod::Plane => rdk_ops::attribute_mirror::Method::MirrorByPlane { origin, normal, distance, tolerance },
                    MirrorMethod::Mapping => {
                        let destination_group = geometry::find_group(group_owner, &mapping_destination, geometry)
                            .ok_or_else(|| {
                                support::diagnostic(
                                    "missing_group",
                                    &format!(
                                        "attribute_mirror could not find mapping destination group {}",
                                        rdk_core::attribute::quote(&mapping_destination)
                                    ),
                                )
                            })?;
                        rdk_ops::attribute_mirror::Method::MirrorByMapping {
                            mapping_attribute: mapping_attribute.clone(),
                            destination_group,
                        }
                    }
                };
                let params = rdk_ops::attribute_mirror::Params {
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
                match rdk_ops::attribute_mirror::run(&params, geometry) {
                    Ok(result) => support::cooked(result),
                    Err(error) => Err(support::structured_rdk_error(&error)),
                }
            }),
        ))
    }

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- attribute transfer all

pub mod attribute_transfer_all {
    use super::*;

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
        let p_point_pattern = text(&values[0]);
        let p_vertex_pattern = text(&values[1]);
        let p_primitive_pattern = text(&values[2]);
        let p_detail_pattern = text(&values[3]);
        let p_mode = support::TRANSFER_MODE.of(&values[4]);
        let p_neighbors = int(&values[5]);
        let p_power = float(&values[6]);
        let p_kernel_radius = float(&values[7]);
        let p_distance_mode = support::KERNEL_MODE.of(&values[8]);
        let p_max_distance = float(&values[9]);
        let p_blend_width = float(&values[10]);
        let p_uniform_bias = float(&values[12]);
        let refuse = |message: &str| Err(format!("sop/attribute_transfer_all: {message}"));
        let patterns = [&p_point_pattern, &p_vertex_pattern, &p_primitive_pattern, &p_detail_pattern];
        if patterns.iter().all(|text| text.trim().is_empty()) {
            return refuse("at least one owner pattern is required");
        }
        for text in patterns {
            if !text.trim().is_empty() {
                support::validate_parameter_pattern("sop/attribute_transfer_all", text)?;
            }
        }
        if p_neighbors < 1 {
            return refuse("neighbors must be positive");
        }
        if ![p_power, p_kernel_radius, p_max_distance, p_blend_width, p_uniform_bias].iter().all(|v| v.is_finite()) {
            return refuse("sampling controls must be finite");
        }
        if p_power < 0.0 || (p_mode == TransferMode::Inverse && p_power == 0.0) {
            return refuse("invalid inverse power");
        }
        let limit = safe_square_limit();
        if p_kernel_radius < 0.0
            || p_kernel_radius > limit
            || p_max_distance < 0.0
            || p_max_distance > limit
            || p_blend_width < 0.0
        {
            return refuse("distances must be nonnegative and safely squarable");
        }
        if p_uniform_bias < 0.0 || p_uniform_bias > 1.0 {
            return refuse("uniform bias must be within [0, 1]");
        }
        if p_distance_mode == KernelMode::Auto && p_blend_width > 0.0 {
            return refuse("blend width requires an explicit maximum distance");
        }
        if p_distance_mode == KernelMode::Explicit && p_max_distance + p_blend_width > limit {
            return refuse("distance plus blend width must be safely squarable");
        }
        Ok(())
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_point_pattern = text(&values[0]);
        let p_vertex_pattern = text(&values[1]);
        let p_primitive_pattern = text(&values[2]);
        let p_detail_pattern = text(&values[3]);
        let p_mode = support::TRANSFER_MODE.of(&values[4]);
        let p_neighbors = int(&values[5]);
        let p_power = float(&values[6]);
        let p_kernel_radius = float(&values[7]);
        let p_distance_mode = support::KERNEL_MODE.of(&values[8]);
        let p_max_distance = float(&values[9]);
        let p_blend_width = float(&values[10]);
        let p_falloff = support::TRANSFER_FALLOFF.of(&values[11]);
        let p_uniform_bias = float(&values[12]);
        let p_unmatched = support::TRANSFER_UNMATCHED.of(&values[13]);
        let (source, target) = (inputs[0].clone().unwrap(), inputs[1].clone().unwrap());
        let mode = support::transfer_mode_of(p_mode, p_neighbors, p_power, p_kernel_radius);
        let falloff = support::transfer_falloff_of(p_falloff, p_uniform_bias);
        let max_distance = match p_distance_mode {
            KernelMode::Explicit => Some(p_max_distance),
            KernelMode::Auto => None,
        };
        let (point_pattern, vertex_pattern) = (support::optional_text(&p_point_pattern), support::optional_text(&p_vertex_pattern));
        let (primitive_pattern, detail_pattern) =
            (support::optional_text(&p_primitive_pattern), support::optional_text(&p_detail_pattern));
        let (blend_width, unmatched) = (p_blend_width, p_unmatched);
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
                let params = rdk_ops::attribute_transfer::AllParams {
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

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
    }
}

// ---------------------------------------------------------------- attribute transfer surface

pub mod attribute_transfer_surface {
    use super::*;

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
        let p_target_owner = support::ATTRIBUTE_OWNER.of(&values[0]);
        let p_attributes = bytes(&values[1]).to_vec();
        let p_distance_mode = support::KERNEL_MODE.of(&values[2]);
        let p_max_distance = float(&values[3]);
        let p_blend_width = float(&values[4]);
        let p_uniform_bias = float(&values[6]);
        let p_distance_attribute = text(&values[8]);
        let p_source_group_pattern = text(&values[10]);
        let p_source_vertex_group_pattern = text(&values[12]);
        let p_target_group_pattern = text(&values[15]);
        let refuse = |message: &str| Err(format!("sop/attribute_transfer_surface: {message}"));
        if p_target_owner == attribute::Owner::Detail {
            return refuse("detail target is not spatial");
        }
        let attributes = support::decode_surface_attributes(&p_attributes)?;
        let names: Vec<&str> = attributes.iter().map(|attribute| attribute.target_name.as_str()).collect();
        let mut sorted = names.clone();
        sorted.sort();
        sorted.dedup();
        if sorted.len() != names.len() {
            return refuse("target attribute names must be distinct");
        }
        if p_target_owner == attribute::Owner::Point && names.contains(&"P") {
            return refuse("target P is not an ordinary attribute");
        }
        let distance = support::optional_text(&p_distance_attribute);
        let conflicts = distance.as_deref().is_some_and(|name| names.contains(&name));
        if (p_target_owner == attribute::Owner::Point && distance.as_deref() == Some("P")) || conflicts {
            return refuse("distance output conflicts with attributes");
        }
        if !(p_max_distance.is_finite() && p_blend_width.is_finite() && p_uniform_bias.is_finite()) {
            return refuse("distance controls must be finite");
        }
        if p_max_distance < 0.0
            || p_max_distance > safe_square_limit()
            || p_blend_width < 0.0
            || p_uniform_bias < 0.0
            || p_uniform_bias > 1.0
        {
            return refuse("distance controls are out of range");
        }
        if p_distance_mode == KernelMode::Auto && p_blend_width > 0.0 {
            return refuse("blend width requires an explicit maximum distance");
        }
        if p_distance_mode == KernelMode::Explicit && p_max_distance + p_blend_width > safe_square_limit() {
            return refuse("distance window exceeds the squared-distance range");
        }
        for pattern in [&p_source_group_pattern, &p_source_vertex_group_pattern, &p_target_group_pattern] {
            if !pattern.trim().is_empty() {
                support::validate_parameter_pattern("attribute_transfer_surface", pattern)?;
            }
        }
        Ok(())
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_target_owner = support::ATTRIBUTE_OWNER.of(&values[0]);
        let p_attributes = bytes(&values[1]).to_vec();
        let p_distance_mode = support::KERNEL_MODE.of(&values[2]);
        let p_max_distance = float(&values[3]);
        let p_blend_width = float(&values[4]);
        let p_falloff = support::TRANSFER_FALLOFF.of(&values[5]);
        let p_uniform_bias = float(&values[6]);
        let p_unmatched = support::TRANSFER_UNMATCHED.of(&values[7]);
        let p_distance_attribute = text(&values[8]);
        let p_source_group = text(&values[9]);
        let p_source_group_pattern = text(&values[10]);
        let p_source_vertex_group = text(&values[11]);
        let p_source_vertex_group_pattern = text(&values[12]);
        let p_source_vertex_selection = VERTEX_SELECTION.of(&values[13]);
        let p_target_group = text(&values[14]);
        let p_target_group_pattern = text(&values[15]);
        let (source, target) = (inputs[0].clone().unwrap(), inputs[1].clone().unwrap());
        let attributes = support::decode_surface_attributes(&p_attributes).expect("validated");
        let target_owner = p_target_owner;
        let max_distance = match p_distance_mode {
            KernelMode::Auto => None,
            KernelMode::Explicit => Some(p_max_distance),
        };
        let falloff = support::transfer_falloff_of(p_falloff, p_uniform_bias);
        let distance_attribute = support::optional_text(&p_distance_attribute);
        let (source_group, source_group_pattern) = support::exact_or_pattern(&p_source_group, &p_source_group_pattern);
        let (source_vertex_group, source_vertex_group_pattern) =
            support::exact_or_pattern(&p_source_vertex_group, &p_source_vertex_group_pattern);
        let (target_group, target_group_pattern) = support::exact_or_pattern(&p_target_group, &p_target_group_pattern);
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
        let (blend_width, unmatched, source_vertex_selection) = (p_blend_width, p_unmatched, p_source_vertex_selection);
        
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
                let params = rdk_ops::attribute_transfer::SurfaceParams {
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

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
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
        let p_owner = support::ATTRIBUTE_OWNER.of(&values[0]);
        let p_use_names = boolean(&values[1]);
        let p_names = bytes(&values[2]).to_vec();
        let p_pattern = text(&values[3]);
        let p_mode = support::TRANSFER_MODE.of(&values[4]);
        let p_neighbors = int(&values[5]);
        let p_power = float(&values[6]);
        let p_kernel_radius = float(&values[7]);
        let p_distance_mode = support::KERNEL_MODE.of(&values[8]);
        let p_max_distance = float(&values[9]);
        let p_blend_width = float(&values[10]);
        let p_uniform_bias = float(&values[12]);
        let p_source_group = text(&values[14]);
        let p_source_group_pattern = text(&values[15]);
        let p_source_vertex_group = text(&values[16]);
        let p_source_vertex_group_pattern = text(&values[17]);
        let p_target_group = text(&values[19]);
        let p_target_group_pattern = text(&values[20]);
        let refuse = |message: &str| Err(format!("sop/attribute_transfer: {message}"));
        if p_use_names {
            let names = decode_names(&p_names)?;
            for name in &names {
                if name.trim().is_empty() {
                    return refuse("empty attribute name");
                }
                if p_owner == attribute::Owner::Point && name == "P" {
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
            support::validate_parameter_pattern("sop/attribute_transfer", &p_pattern)?;
        }
        if p_neighbors < 1 {
            return refuse("neighbors must be positive");
        }
        if p_power < 0.0 || (p_mode == TransferMode::Inverse && p_power == 0.0) {
            return refuse("invalid inverse power");
        }
        let limit = safe_square_limit();
        if p_kernel_radius < 0.0 || p_kernel_radius > limit || p_max_distance < 0.0 || p_max_distance > limit || p_blend_width < 0.0 {
            return refuse("distances must be nonnegative and safely squarable");
        }
        if p_uniform_bias < 0.0 || p_uniform_bias > 1.0 {
            return refuse("uniform bias must be within [0,1]");
        }
        if p_distance_mode == KernelMode::Auto && p_blend_width > 0.0 {
            return refuse("blend width requires an explicit maximum distance");
        }
        if p_distance_mode == KernelMode::Explicit && p_max_distance + p_blend_width > limit {
            return refuse("distance plus blend width must be safely squarable");
        }
        let (source_group, source_pattern) = support::exact_or_pattern(&p_source_group, &p_source_group_pattern);
        let (vertex_group, vertex_pattern) =
            support::exact_or_pattern(&p_source_vertex_group, &p_source_vertex_group_pattern);
        let (target_group, target_pattern) = support::exact_or_pattern(&p_target_group, &p_target_group_pattern);
        for (name, pattern) in [("source", &source_pattern), ("source vertex", &vertex_pattern), ("target", &target_pattern)] {
            support::compile_transfer_group_pattern("attribute_transfer", name, pattern)?;
        }
        if p_owner != attribute::Owner::Vertex && (vertex_group.is_some() || vertex_pattern.is_some()) {
            return refuse("source vertex groups require vertex ownership");
        }
        if p_owner == attribute::Owner::Detail
            && (source_group.is_some()
                || source_pattern.is_some()
                || target_group.is_some()
                || target_pattern.is_some()
                || p_distance_mode != KernelMode::Auto
                || p_blend_width != 0.0)
        {
            return refuse("detail transfer does not accept spatial options");
        }
        if matches!(p_owner, attribute::Owner::Detail | attribute::Owner::Vertex) && p_mode != TransferMode::Nearest {
            return refuse("this owner supports nearest mode only");
        }
        Ok(())
    }

    pub fn operator(label: &str, values: &[Value], inputs: &[Option<node::T>]) -> Result<node::T, String> {
        let p_owner = support::ATTRIBUTE_OWNER.of(&values[0]);
        let p_use_names = boolean(&values[1]);
        let p_names = bytes(&values[2]).to_vec();
        let p_pattern = text(&values[3]);
        let p_mode = support::TRANSFER_MODE.of(&values[4]);
        let p_neighbors = int(&values[5]);
        let p_power = float(&values[6]);
        let p_kernel_radius = float(&values[7]);
        let p_distance_mode = support::KERNEL_MODE.of(&values[8]);
        let p_max_distance = float(&values[9]);
        let p_blend_width = float(&values[10]);
        let p_falloff = support::TRANSFER_FALLOFF.of(&values[11]);
        let p_uniform_bias = float(&values[12]);
        let p_unmatched = support::TRANSFER_UNMATCHED.of(&values[13]);
        let p_source_group = text(&values[14]);
        let p_source_group_pattern = text(&values[15]);
        let p_source_vertex_group = text(&values[16]);
        let p_source_vertex_group_pattern = text(&values[17]);
        let p_source_vertex_selection = VERTEX_SELECTION.of(&values[18]);
        let p_target_group = text(&values[19]);
        let p_target_group_pattern = text(&values[20]);
        let (source, target) = (inputs[0].clone().unwrap(), inputs[1].clone().unwrap());
        let owner = p_owner;
        let source_vertex_selection = p_source_vertex_selection;
        let (names, pattern) = if p_use_names {
            (Some(decode_names(&p_names).expect("validated")), None)
        } else {
            (None, Some(p_pattern.clone()))
        };
        let mode = support::transfer_mode_of(p_mode, p_neighbors, p_power, p_kernel_radius);
        let falloff = support::transfer_falloff_of(p_falloff, p_uniform_bias);
        let (unmatched, blend_width) = (p_unmatched, p_blend_width);
        let max_distance = match p_distance_mode {
            KernelMode::Auto => None,
            KernelMode::Explicit => Some(p_max_distance),
        };
        let (source_group, source_group_pattern) = support::exact_or_pattern(&p_source_group, &p_source_group_pattern);
        let (source_vertex_group, source_vertex_group_pattern) =
            support::exact_or_pattern(&p_source_vertex_group, &p_source_vertex_group_pattern);
        let (target_group, target_group_pattern) = support::exact_or_pattern(&p_target_group, &p_target_group_pattern);
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
                    attribute::Owner::Point => attribute_ops::transfer_points(&rdk_ops::attribute_transfer::SpatialParams {
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
                    attribute::Owner::Vertex => attribute_ops::transfer_vertices(&rdk_ops::attribute_transfer::VerticesParams {
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
                        attribute_ops::transfer_primitives(&rdk_ops::attribute_transfer::SpatialParams {
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
                    attribute::Owner::Detail => attribute_ops::transfer_detail(&rdk_ops::attribute_transfer::DetailParams {
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

    pub fn factory() -> Factory {
        decl::native(&META, specs(), operator)
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
    fn every_node_declares_its_authored_schema_fields() {
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
        assert_eq!(rdk_core::geometry::point_count(&g), 12);
        let rgba = rdk_core::geometry::find_attribute(rdk_core::attribute::Owner::Point, "rgba", &g).expect("rgba");
        assert!(matches!(rdk_core::attribute::view::storage(&rgba), Ok(rdk_core::attribute::Storage::Float4(_))));
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
                ("names".to_owned(), Value::Text(table.into())),
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
        assert_eq!(rdk_core::geometry::point_count(&g), 9);
        crate::session::close(&mut s);
    }
}
