//! Port of lib/sop/zone.ml: the zone cook step, a loop over the elements of cooked geometry.
//!
//! A zone node has the collection as input 0 (and any outer geometry its body reads as further
//! inputs). When the session cooks it, `Node.Build.expand` derives the elements of the collection,
//! asks `body` for the sub-graph of each one, cooks those through the same session (so an element
//! whose sub-graph is unchanged is a cache hit) and merges the results, writing
//! `source_base + element index` into `source_attribute`.
use std::collections::{BTreeSet, HashMap};
use std::sync::Arc;

use rays_diagnostic::Diagnostic;
use rdk_core::{attribute, geometry};

use crate::context::{self, dependencies};
use crate::node::{self, build, CookMode, GeometryCooked};
use crate::support;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Points,
    /// A point is its position; a piece is one primitive, or the primitives that share a value of
    /// the key attribute.
    Pieces,
}

#[derive(Clone)]
pub struct Element {
    /// position in the ordered list
    pub index: i64,
    /// the `key` attribute's value, or the source index
    pub key: i64,
    /// a point's position; a piece's first point
    pub position: (f64, f64, f64),
    /// the geometry of a piece
    pub piece: Option<geometry::T>,
    /// identity of a piece's content, lowercase hex (empty for a point)
    pub digest: String,
}

/// 4,096, as for every loop.
pub const MAX_ELEMENTS: i64 = 4096;

fn error(code: &str, message: String) -> Diagnostic {
    support::diagnostic(code, &message)
}

fn too_many(what: &str, count: i64) -> Diagnostic {
    error(
        "E_ITER_BOUND",
        format!("A loop over {what} runs {count} iterations; the limit is {MAX_ELEMENTS}."),
    )
}

fn keys(owner: attribute::Owner, count: i64, name: Option<&str>, geometry_value: &geometry::T) -> Vec<i64> {
    let by_index: Vec<i64> = (0..count).collect();
    let Some(name) = name else { return by_index };
    match geometry::find_attribute(owner, name, geometry_value).and_then(|a| attribute::storage(&a).ok()) {
        Some(attribute::Storage::Int(values)) if values.len() as i64 == count => values.to_vec(),
        Some(attribute::Storage::Float(values)) if values.len() as i64 == count => {
            values.iter().map(|v| v.floor() as i64).collect()
        }
        _ => by_index,
    }
}

fn order(keys: &[i64]) -> Vec<usize> {
    let mut order: Vec<usize> = (0..keys.len()).collect();
    order.sort_by_key(|i| keys[*i]); // stable
    order
}

/// Two independent FNV-1a lanes: 128 bits of content identity (OCaml used MD5; only equality of
/// digests is observable).
struct Digest(u64, u64);

impl Digest {
    fn new() -> Digest {
        Digest(0xcbf29ce484222325, 0x84222325cbf29ce4)
    }
    fn byte(&mut self, b: u8) {
        self.0 = (self.0 ^ u64::from(b)).wrapping_mul(0x100000001b3);
        self.1 = (self.1 ^ u64::from(b)).wrapping_mul(0x1000193).rotate_left(5);
    }
    fn int(&mut self, v: i64) {
        v.to_le_bytes().iter().for_each(|b| self.byte(*b));
    }
    fn text(&mut self, s: &str) {
        self.int(s.len() as i64);
        s.bytes().for_each(|b| self.byte(b));
    }
    fn hex(&self) -> String {
        format!("{:016x}{:016x}", self.0, self.1)
    }
}

fn digest(geometry_value: &geometry::T) -> String {
    let mut d = Digest::new();
    if let Ok(view) = rdk_core::topology::View::view(geometry::topology(geometry_value)) {
        view.vertex_points.iter().for_each(|v| d.int(*v));
        view.primitive_offsets.iter().for_each(|v| d.int(*v));
        view.primitive_kinds.iter().for_each(|v| d.byte(*v));
    }
    if let Ok(view) = rdk_core::packed::Float3::View::view(geometry::positions(geometry_value)) {
        for plane in [&view.x, &view.y, &view.z] {
            plane.iter().for_each(|v| d.int(v.to_bits() as i64));
        }
    }
    for a in geometry::attributes(geometry_value) {
        d.text(attribute::name(&a));
        d.int(attribute::owner(&a) as i64);
        d.text(attribute::kind_name(&a));
        d.int(attribute::length(&a));
    }
    d.hex()
}

/// Ordered by `key` (an int or float attribute of the points, or of the primitives for pieces)
/// when the geometry carries it, else by index (ties keep index order).
pub fn elements(kind: Kind, key: Option<&str>, geometry_value: &geometry::T) -> Result<Vec<Element>, Diagnostic> {
    match kind {
        Kind::Points => {
            let count = geometry::point_count(geometry_value);
            if count > MAX_ELEMENTS {
                return Err(too_many("points", count));
            }
            let positions = geometry::positions(geometry_value);
            let keys = keys(attribute::Owner::Point, count, key, geometry_value);
            let mut out = Vec::with_capacity(count as usize);
            for (index, source) in order(&keys).into_iter().enumerate() {
                let position = rdk_core::packed::Float3::get(positions, source as i64)
                    .map_err(|e| support::structured_rdk_error(&e))?;
                out.push(Element { index: index as i64, key: keys[source], position, piece: None, digest: String::new() });
            }
            Ok(out)
        }
        Kind::Pieces => {
            let count = geometry::primitive_count(geometry_value);
            let keys = keys(attribute::Owner::Primitive, count, key, geometry_value);
            let distinct: Vec<i64> = keys.iter().copied().collect::<BTreeSet<_>>().into_iter().collect();
            let pieces = distinct.len() as i64;
            if pieces > MAX_ELEMENTS {
                return Err(too_many("pieces", pieces));
            }
            let rank: HashMap<i64, i64> = distinct.iter().enumerate().map(|(i, k)| (*k, i as i64)).collect();
            let assignment: Vec<i64> = keys.iter().map(|k| rank[k]).collect();
            let parts = rdk_ops::deletion::primitive_partitions(pieces, &assignment, geometry_value)
                .map_err(|e| support::structured_rdk_error(&e))?;
            let mut out = Vec::with_capacity(parts.len());
            for (index, part) in parts.into_iter().enumerate() {
                let position = if geometry::point_count(&part) == 0 {
                    (0.0, 0.0, 0.0)
                } else {
                    rdk_core::packed::Float3::get(geometry::positions(&part), 0).map_err(|e| support::structured_rdk_error(&e))?
                };
                let digest = digest(&part);
                out.push(Element { index: index as i64, key: distinct[index], position, piece: Some(part), digest });
            }
            Ok(out)
        }
    }
}

/// The geometry of a piece as a node, keyed by its content digest so the nodes fed by it hit the
/// session cache while it is unchanged.
pub fn element_node(element: &Element) -> node::T {
    let piece = element.piece.clone().expect("a piece element");
    build::make_geometry(
        build::Args {
            parameters: format!("content={}", element.digest).into_bytes(),
            cook_mode: CookMode::Generator,
            ..build::args("zone_element")
        },
        None,
        Arc::new(move |_, _, _| Ok(GeometryCooked { geometry: piece.clone(), instances: None })),
    )
}

/// Built once per cook with the node's current inputs and the cook's context: what does not read
/// the element is shared. Must be pure, and must not capture the inputs the node was made with.
pub type ElementBody = Arc<dyn Fn(&Element) -> node::T + Send + Sync>;
pub type Body = Arc<dyn Fn(&[node::T], &context::T) -> ElementBody + Send + Sync>;

pub struct Args {
    /// receives the elements of each cook that runs (not of a cache hit)
    pub report: Option<Arc<dyn Fn(&[Element]) + Send + Sync>>,
    /// a live zone (its body reads `t`) depends on the context's time
    pub live: bool,
    pub reads: dependencies::T,
    /// identifies an immutable frame-fold snapshot captured by `body`
    pub stamp: String,
    pub kind: Kind,
    pub key: Option<String>,
    /// expands only that element for a viewport preview, keeping its original source tag
    pub select: Option<i64>,
    pub source_attribute: String,
    pub source_base: i64,
}

pub fn args(kind: Kind, source_attribute: &str, source_base: i64) -> Args {
    Args {
        report: None,
        live: false,
        reads: dependencies::one(dependencies::Fact::Input),
        stamp: String::new(),
        kind,
        key: None,
        select: None,
        source_attribute: source_attribute.to_owned(),
        source_base,
    }
}

pub fn node(args: Args, body: Body, inputs: Vec<node::T>) -> node::T {
    let mut parameters = format!(
        "kind={};key={};source={};base={};inputs={};live={}",
        match args.kind {
            Kind::Points => "points",
            Kind::Pieces => "pieces",
        },
        node::ocaml_quote(args.key.as_deref().unwrap_or("")),
        node::ocaml_quote(&args.source_attribute),
        args.source_base,
        inputs.len(),
        args.live
    );
    if let Some(i) = args.select {
        parameters.push_str(&format!(";select={i}"));
    }
    if !args.stamp.is_empty() {
        parameters.push_str(&format!(";frame-state={}", args.stamp));
    }
    let (kind, key, select) = (args.kind, args.key.clone(), args.select);
    let report = args.report.clone();
    let expand: node::GeometryExpandFn = Arc::new(move |context, nodes, geometries| {
        let found = elements(kind, key.as_deref(), &geometries[0])?;
        if let Some(report) = &report {
            report(&found);
        }
        let chosen: Vec<Element> = match select {
            None => found,
            Some(index) if index >= 0 && (index as usize) < found.len() => vec![found[index as usize].clone()],
            Some(_) => Vec::new(),
        };
        // the body is built once per cook: what does not read the element is shared
        let element = body(nodes, context);
        Ok(chosen.iter().map(|e| element(e)).collect())
    });
    let (source_attribute, source_base) = (args.source_attribute.clone(), args.source_base);
    build::make_geometry(
        build::Args {
            parameters: parameters.into_bytes(),
            cook_mode: CookMode::Generic,
            dependencies: if args.live { args.reads } else { dependencies::STATIC },
            inputs,
            ..build::args("zone")
        },
        Some(expand),
        Arc::new(move |_, context, parts| {
            let params = rdk_ops::mesh_merge::RunParams {
                cancel: Some(Arc::clone(context::cancel_token(context))),
                grain: Some(context::grain(context)),
                source_attribute: Some(source_attribute.clone()),
                source_base: Some(source_base + select.unwrap_or(0)),
            };
            match rdk_ops::mesh_merge::run(&params, parts) {
                Ok(geometry) => Ok(GeometryCooked { geometry, instances: None }),
                Err(e) => Err(support::diagnostic_with(
                    rdk_core::error::code(&e),
                    "zone could not merge its elements",
                    Some(rdk_core::error::to_string(&e)),
                    Vec::new(),
                )),
            }
        }),
    )
}
