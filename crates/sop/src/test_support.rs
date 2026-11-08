//! Shared fixtures of the ported lib/sop tests (the slice of lisp_sop and rdk_test_support they
//! use): snapshot nodes, a small grid, counted nodes with declared facts.
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

use rdk_core::{attribute, geometry, packed, topology};

use crate::context::{self, dependencies};
use crate::node::{self, build, CookMode, Elementwise, Facts, Topology};
use crate::session;

pub fn context(domains: i64) -> context::T {
    context::create(context::Options { domains: Some(domains), grain: Some(37), ..Default::default() }).unwrap()
}

pub fn session(entries: i64) -> session::T {
    session::create(entries, 16_000_000).unwrap()
}

pub fn cook(session: &mut session::T, domains: i64, n: &node::T) -> session::Output {
    match session::cook(session, None, &context(domains), n) {
        Ok(output) => output,
        Err(error) => panic!("{}", rays_diagnostic::error_to_string(&error)),
    }
}

pub fn geometry_of(output: &session::Output) -> geometry::T {
    crate::payload::geometry(&output.payload).unwrap()
}

/// `Lisp_sop.snapshot`: a generator that returns the geometry, keyed by its data id.
pub fn snapshot(g: &geometry::T) -> node::T {
    let held = g.clone();
    build::make_geometry(
        build::Args {
            parameters: format!("data_id={}", geometry::data_id(g)).into_bytes(),
            cook_mode: CookMode::Generator,
            ..build::args("snapshot")
        },
        None,
        Arc::new(move |_, _, _| Ok(node::GeometryCooked { geometry: held.clone(), instances: None })),
    )
}

/// A 3x3 grid of four quads with a point normal attribute.
pub fn fixture() -> geometry::T {
    let mut x = Vec::new();
    let mut y = Vec::new();
    for j in 0..3 {
        for i in 0..3 {
            x.push(f64::from(i));
            y.push(f64::from(j));
        }
    }
    let positions = packed::Float3::of_owned(x, y, vec![0.0; 9]).unwrap();
    let vertex_points = vec![0, 1, 4, 3, 1, 2, 5, 4, 3, 4, 7, 6, 4, 5, 8, 7];
    let topology = topology::polygons_owned(9, vertex_points, vec![0, 4, 8, 12, 16]).unwrap();
    let normals = packed::Float3::of_owned(vec![0.0; 9], vec![0.0; 9], vec![1.0; 9]).unwrap();
    let n = attribute::create_key_owned(&attribute::normal(attribute::Owner::Point), attribute::Storage::Float3(normals)).unwrap();
    geometry::create(positions, topology, Some(vec![n]), None, None).unwrap()
}

pub fn scalar(owner: attribute::Owner, name: &str, count: i64, value: f64) -> attribute::T {
    attribute::create_owned(name, owner, attribute::Storage::Float(vec![value; count as usize])).unwrap()
}

pub fn with_color(value: f64, g: &geometry::T) -> geometry::T {
    geometry::with_attribute(scalar(attribute::Owner::Point, "Cd", geometry::point_count(g), value), g).unwrap()
}

/// Declared per-point facts (`facts node reads writes`).
pub fn facts(n: &node::T, reads: &[&str], writes: &[&str]) -> node::T {
    build::with_facts(
        Facts {
            elementwise: Elementwise::Points,
            topology: Topology::Preserved,
            exact: false,
            reads: reads.iter().map(|s| (*s).to_owned()).collect(),
            writes: writes.iter().map(|s| (*s).to_owned()).collect(),
            ..node::facts(n).clone()
        },
        n,
    )
}

pub type Transform = Arc<dyn Fn(&geometry::T) -> geometry::T + Send + Sync>;

/// A duplicate-input node that counts its cooks and applies `transform`.
pub fn counted(source: &node::T, calls: &Arc<AtomicI64>, reads: &[&str], writes: &[&str], transform: Transform) -> node::T {
    let calls = Arc::clone(calls);
    let made = build::make_geometry(
        build::Args {
            label: Some("counted".to_owned()),
            cook_mode: CookMode::DuplicateInput(0),
            dependencies: dependencies::STATIC,
            inputs: vec![source.clone()],
            ..build::args("facts_test")
        },
        None,
        Arc::new(move |_, _, inputs| {
            calls.fetch_add(1, Ordering::SeqCst);
            Ok(node::GeometryCooked { geometry: transform(&inputs[0]), instances: None })
        }),
    );
    facts(&made, reads, writes)
}

pub fn identity() -> Transform {
    Arc::new(|g| g.clone())
}

pub fn rewire(n: &node::T, g: &geometry::T) -> node::T {
    build::rebuild_with_inputs(n, &[snapshot(g)])
}

pub fn calls() -> Arc<AtomicI64> {
    Arc::new(AtomicI64::new(0))
}

pub fn count(calls: &Arc<AtomicI64>) -> i64 {
    calls.load(Ordering::SeqCst)
}

/// Positions and attribute payload identity: two geometries with equal planes and attributes.
pub fn same_geometry(a: &geometry::T, b: &geometry::T) -> bool {
    let pa = packed::Float3::View::view(geometry::positions(a)).unwrap();
    let pb = packed::Float3::View::view(geometry::positions(b)).unwrap();
    let attrs = |g: &geometry::T| -> Vec<(String, i64, i64)> {
        geometry::attributes(g)
            .iter()
            .map(|x| (attribute::name(x).to_owned(), attribute::owner(x) as i64, attribute::data_id(x)))
            .collect()
    };
    pa.x == pb.x && pa.y == pb.y && pa.z == pb.z && attrs(a) == attrs(b)
}
