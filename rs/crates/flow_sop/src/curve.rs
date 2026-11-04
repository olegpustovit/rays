//! Port of lib/flow_sop/curve.ml: the lowered `sop/curve`. Authored `points` use a lossless text
//! encoding for parameter views; live drives pass the vec3 array through `replace_points`.
//! `closed` joins the last point to the first.
//!
//! ponytail: the OCaml schema value is the typed array (encoded only for views); parameter values
//! here are text, so a live `replace_points` re-encodes its points (O(n) hex floats per rebuild).
use flow_ir::core_ir::hex_float;
use param::{Kind, Value};
use rays_diagnostic::Diagnostic;
use rdk::line_geometry;
use rdk::{error as rdk_error, geometry, topology};
use sop::context::dependencies;
use sop::edit_graph::{self, Factory};
use sop::node::{self, build, CookMode, GeometryCooked};
use std::sync::{Arc, OnceLock};

/// The factory key of the lowered curve.
pub const KEY: &str = "flow.curve";

/// `"points"`.
pub const PARAMETER: &str = "points";
/// `"closed"`.
pub const CLOSED_PARAMETER: &str = "closed";
/// `"filled"`: the closed curve is one polygon surface, not a polyline.
pub const FILLED_PARAMETER: &str = "filled";

/// Lossless (hexadecimal floats), deterministic.
pub fn encode(points: &[(f64, f64, f64)]) -> String {
    let mut out = String::with_capacity(points.len() * 40);
    for (i, (x, y, z)) in points.iter().enumerate() {
        if i > 0 {
            out.push(';');
        }
        out.push_str(&format!("{},{},{}", hex_float(*x), hex_float(*y), hex_float(*z)));
    }
    out
}

fn decode(text: &str) -> Result<Vec<(f64, f64, f64)>, String> {
    let failure = || "curve points: expected x,y,z;...".to_owned();
    if text.is_empty() {
        return Ok(Vec::new());
    }
    let number = |part: &str| cfmt::parse_ocaml_float(part).ok_or_else(failure);
    let mut points = Vec::new();
    for point in text.split(';') {
        let parts: Vec<&str> = point.split(',').collect();
        match parts.as_slice() {
            [x, y, z] => points.push((number(x)?, number(y)?, number(z)?)),
            _ => return Err(failure()),
        }
    }
    Ok(points)
}

#[derive(Clone)]
struct Curve {
    points: Vec<(f64, f64, f64)>,
    closed: bool,
    filled: bool,
}

fn schema() -> Arc<param::Schema> {
    static SCHEMA: OnceLock<Arc<param::Schema>> = OnceLock::new();
    SCHEMA
        .get_or_init(|| {
            let points_kind = Kind::Encoded {
                equal: Box::new(|a, b| a == b),
                normalize: Box::new(|raw| {
                    let text = std::str::from_utf8(raw).map_err(|_| "curve points: expected x,y,z;...".to_owned())?;
                    Ok(encode(&decode(text)?).into_bytes())
                }),
            };
            let fields = vec![
                param::field(PARAMETER.into(), points_kind, Value::Text(Vec::new()))
                    .and_then(|f| f.label("Points".into()))
                    .expect("curve points field"),
                param::field(CLOSED_PARAMETER.into(), Kind::Toggle, Value::Bool(false))
                    .and_then(|f| f.label("Closed".into()))
                    .expect("curve closed field"),
                param::field(FILLED_PARAMETER.into(), Kind::Toggle, Value::Bool(false))
                    .and_then(|f| f.label("Filled".into()))
                    .expect("curve filled field"),
            ];
            Arc::new(param::schema("flow_curve".into(), fields).expect("curve schema"))
        })
        .clone()
}

fn values_of(curve: &Curve) -> Vec<Value> {
    vec![Value::Text(encode(&curve.points).into_bytes()), Value::Bool(curve.closed), Value::Bool(curve.filled)]
}

fn of_values(values: &[Value]) -> Result<Curve, String> {
    match values {
        [Value::Text(points), Value::Bool(closed), Value::Bool(filled)] => Ok(Curve {
            points: decode(std::str::from_utf8(points).map_err(|_| "curve points: expected x,y,z;...".to_owned())?)?,
            closed: *closed,
            filled: *filled,
        }),
        _ => Err("curve parameters have the wrong shape".to_owned()),
    }
}

/// A filled curve is one polygon primitive (a surface), not a closed polyline.
fn polyline(closed: bool, filled: bool, points: &[(f64, f64, f64)]) -> Result<geometry::T, rdk_error::T> {
    let line = line_geometry::polyline(&line_geometry::Params { closed: closed || filled }, points)?;
    if !filled {
        return Ok(line);
    }
    let count = points.len() as i64;
    let failure = |message: String| {
        rdk_error::make("polyline".into(), "invalid_parameter".into(), message, None)
            .unwrap_or_else(|e| e)
    };
    let topology = topology::polygons_owned(count, (0..count).collect(), vec![0, count]).map_err(|e| failure(rdk_error::message(&e).to_owned()))?;
    geometry::create(geometry::positions(&line).clone(), topology, None, None, None)
        .map_err(|e| failure(rdk_error::message(&e).to_owned()))
}

fn parameter_key(curve: &Curve) -> Vec<u8> {
    let mut bytes: Vec<u8> = Vec::with_capacity(KEY.len() + 3 + curve.points.len() * 24);
    bytes.extend_from_slice(KEY.as_bytes());
    bytes.push(0);
    bytes.push(u8::from(curve.closed));
    bytes.push(u8::from(curve.filled));
    for (x, y, z) in &curve.points {
        for value in [x, y, z] {
            bytes.extend_from_slice(&value.to_bits().to_le_bytes());
        }
    }
    bytes
}

fn build_node(label: &str, inputs: &[node::T], curve: &Curve) -> Result<node::T, String> {
    let parameters = format!("count={};closed={};filled={}", curve.points.len(), curve.closed, curve.filled);
    let shared = curve.clone();
    let made = build::make_geometry(
        build::Args {
            label: Some(label.to_owned()),
            version: 1,
            parameters: parameters.into_bytes(),
            cook_mode: CookMode::Generator,
            dependencies: dependencies::STATIC,
            inputs: inputs.to_vec(),
            ..build::args("polyline")
        },
        None,
        Arc::new(move |_node_id, _context, _inputs| match polyline(shared.closed, shared.filled, &shared.points) {
            Ok(geometry) => Ok(GeometryCooked { geometry, instances: None }),
            Err(failure) => Err(Diagnostic {
                cause: Some(rdk_error::to_string(&failure)),
                hints: rdk_error::hints(&failure).to_vec(),
                ..crate::port::error(rdk_error::code(&failure), &format!("{} could not produce valid geometry", rdk_error::operation(&failure)))
            }),
        }),
    );
    let rebuild: node::RebuildFn = Arc::new(|label, inputs, values| build_node(label, inputs, &of_values(values)?));
    node::parameterize_with_key(Some(parameter_key(curve)), schema(), values_of(curve), rebuild, &made)
}

fn node_of(curve: &Curve) -> node::T {
    build_node("Curve", &[], curve).expect("a default or typed curve always builds")
}

/// Rebuild from typed points while retaining the source node's identity and label.
pub fn replace_points(source: &node::T, closed: bool, filled: bool, points: &[(f64, f64, f64)]) -> node::T {
    build::adopt_identity(source, &node_of(&Curve { points: points.to_vec(), closed, filled }))
}

pub fn factory() -> Factory {
    let default = Curve { points: Vec::new(), closed: false, filled: false };
    let fields = node::parameter_fields(&node_of(&default));
    let mut args = edit_graph::factory_args(KEY, "Curve", &["Flow"]);
    args.fields = fields;
    edit_graph::factory(args, 0, Arc::new(|_| Ok(node_of(&Curve { points: Vec::new(), closed: false, filled: false }))))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encoding_is_lossless_and_round_trips() {
        let points = vec![(0.1, -2.5, 1e300), (0.0, -0.0, 3.0)];
        let text = encode(&points);
        assert_eq!(text, "0x1.999999999999ap-4,-0x1.4p+1,0x1.7e43c8800759cp+996;0x0p+0,-0x0p+0,0x1.8p+1");
        let back = decode(&text).expect("decodes");
        assert_eq!(back.len(), 2);
        assert_eq!(back[0].0.to_bits(), 0.1f64.to_bits());
        assert_eq!(back[1].1.to_bits(), (-0.0f64).to_bits());
        assert!(decode("1,2").is_err());
        assert_eq!(decode("").expect("empty"), Vec::new());
    }

    #[test]
    fn curve_node_cooks_a_polyline_and_keeps_identity() {
        let curve = Curve { points: vec![(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0)], closed: true, filled: false };
        let made = node_of(&curve);
        let again = replace_points(&made, true, false, &curve.points);
        assert_eq!(node::id(&again), node::id(&made));
        assert_eq!(node::parameter_key(&again), node::parameter_key(&made));
        let (edited, effects) =
            node::apply_parameters(&made, &[(CLOSED_PARAMETER.to_owned(), Value::Bool(false))]).expect("edit");
        assert!(effects.cook);
        assert_eq!(node::id(&edited), node::id(&made));
        assert!(node::apply_parameters(&made, &[(PARAMETER.to_owned(), Value::Text(b"bad".to_vec()))]).is_err());
    }
}
