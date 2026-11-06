//! Shared helpers of the ported lib/flow_sop tests (Test_support, Rdk_test_support).
use crate::lower;
use flow_syntax::syntax;
use rdk::geometry as geo;
use sop::context::{self as sctx, Options as ContextOptions};
use sop::edit_graph as edit;
use sop::node;
use sop::payload;
use sop::session;

pub fn parse(text: &str) -> Vec<syntax::Syntax> {
    syntax::parse(text).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

/// The first default graph of a workspace, lowered, with its compiled root node.
pub fn lower_text(options: lower::Options, text: &str) -> (lower::T, node::T) {
    let lowered = lower::workspace(None, Vec::new(), options, &parse(text)).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let graph = lowered.graphs.iter().find(|g| g.default).expect("a default graph").clone();
    let root = edit::compile_node(&graph.network.geometry, graph.root.expect("a graph result")).expect("compile");
    (lowered, root)
}

pub fn new_session(entries: i64, bytes: i64) -> session::T {
    session::create(entries, bytes).expect("session")
}

pub fn context(domains: i64, grain: i64) -> sctx::T {
    sctx::create(ContextOptions { domains: Some(domains), grain: Some(grain), ..ContextOptions::default() }).expect("context")
}

pub fn try_cook(session: &mut session::T, context: &sctx::T, n: &node::T) -> Result<geo::T, rays_diagnostic::Diagnostic> {
    let out = session::cook(session, None, context, n)?;
    payload::geometry(&out.payload)
}

pub fn cook(session: &mut session::T, context: &sctx::T, n: &node::T) -> geo::T {
    try_cook(session, context, n).unwrap_or_else(|d| panic!("{}", rays_diagnostic::error_to_string(&d)))
}

/// One cook in a fresh session.
pub fn cook_once(domains: i64, n: &node::T) -> geo::T {
    let mut session = new_session(64, 64 * 1024 * 1024);
    let g = cook(&mut session, &context(domains, 257), n);
    session::close(&mut session);
    g
}

/// A byte image of a geometry: positions, topology and every attribute, to compare exactly.
pub fn geometry_bytes(g: &geo::T) -> Vec<u8> {
    let mut out = String::new();
    let positions = geo::positions(g);
    for i in 0..rdk::packed::Float3::length(positions) {
        let (x, y, z) = rdk::packed::Float3::get(positions, i).expect("position");
        out.push_str(&format!("{:016x}{:016x}{:016x};", x.to_bits(), y.to_bits(), z.to_bits()));
    }
    if let Ok(view) = rdk::topology::View::view(geo::topology(g)) {
        out.push_str(&format!("T{}|{:?}|{:?}|{:?};", view.point_count, view.vertex_points, view.primitive_offsets, view.primitive_kinds));
    }
    for a in geo::attributes(g) {
        out.push_str(&format!("A{:?}:{}:{};", rdk::attribute::owner(&a), rdk::attribute::name(&a), rdk::attribute::kind_name(&a)));
        match rdk::attribute::storage(&a) {
            Ok(rdk::attribute::Storage::Float(v)) => out.push_str(&v.iter().map(|x| format!("{:016x}", x.to_bits())).collect::<String>()),
            Ok(rdk::attribute::Storage::Int(v)) => out.push_str(&format!("{v:?}")),
            Ok(rdk::attribute::Storage::Text(v)) => out.push_str(&format!("{v:?}")),
            Ok(rdk::attribute::Storage::Float2(v)) => {
                for i in 0..rdk::packed::Float2::length(&v) {
                    out.push_str(&format!("{:?}", rdk::packed::Float2::get(&v, i).map(|(x, y)| (x.to_bits(), y.to_bits()))));
                }
            }
            Ok(rdk::attribute::Storage::Float3(v)) => {
                for i in 0..rdk::packed::Float3::length(&v) {
                    out.push_str(&format!("{:?}", rdk::packed::Float3::get(&v, i).map(|(x, y, z)| (x.to_bits(), y.to_bits(), z.to_bits()))));
                }
            }
            Ok(rdk::attribute::Storage::Float4(v)) => {
                for i in 0..rdk::packed::Float4::length(&v) {
                    out.push_str(&format!("{:?}", rdk::packed::Float4::get(&v, i).map(|(a, b, c, d)| (a.to_bits(), b.to_bits(), c.to_bits(), d.to_bits()))));
                }
            }
            Ok(rdk::attribute::Storage::IntArray(v)) => {
                let v = rdk::packed::IntArray::View::view(&v);
                out.push_str(&format!("{:?}:{:?}", v.offsets, v.values));
            }
            Ok(rdk::attribute::Storage::FloatArray(v)) => {
                let v = rdk::packed::FloatArray::View::view(&v);
                out.push_str(&format!("{:?}:", v.offsets));
                for x in v.values.iter() { out.push_str(&format!("{:016x}", x.to_bits())); }
            }
            Err(e) => out.push_str(&format!("{e:?}")),
        }
        out.push(';');
    }
    for group in geo::groups(g) {
        out.push_str(&format!("G{:?}:{}:", rdk::group::owner(&group), rdk::group::name(&group)));
        for i in 0..rdk::group::length(&group) {
            out.push(if rdk::group::mem(i, &group) { '1' } else { '0' });
        }
        out.push_str(&format!("{:?}", rdk::group::ordered_elements(&group)));
        out.push(';');
    }
    for group in geo::edge_groups(g) {
        out.push_str(&format!("E{}:", rdk::edge_group::name(&group)));
        for i in 0..rdk::edge_group::length(&group) {
            out.push(if rdk::edge_group::mem(i, &group) { '1' } else { '0' });
        }
        out.push(';');
    }
    out.into_bytes()
}

/// The text of a specification/workspace/cases fixture: `bloom` and `variations` are fixtures of
/// their own, the other ten are the sketches `sketches/ws_NAME.rays` (the OCaml dune rule copies them).
pub fn case_text(name: &str) -> String {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let path = if name == "bloom" || name == "variations" {
        root.join("specification/workspace/cases").join(format!("{name}.lisp"))
    } else {
        root.join("sketches").join(format!("ws_{name}.rays"))
    };
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}

/// `Rdk_test_support.get_ok` / `get_string_ok`.
pub fn fixture_ok<A, E: ToString>(r: Result<A, E>) -> A {
    r.unwrap_or_else(|e| panic!("{}", e.to_string()))
}

/// `Plane_generators.grid ~connectivity:Grid_quads ~columns ~rows ~size ()`: a quad grid in the XY plane.
pub fn fixture_grid(columns: i64, rows: i64, size: f64) -> geo::T {
    let stride = columns + 1;
    let (mut x, mut y) = (Vec::new(), Vec::new());
    for j in 0..=rows {
        for i in 0..=columns {
            x.push((i as f64 / columns as f64 - 0.5) * size);
            y.push((j as f64 / rows as f64 - 0.5) * size);
        }
    }
    let z = vec![0.0; x.len()];
    let (mut vp, mut offsets) = (Vec::new(), vec![0i64]);
    for j in 0..rows {
        for i in 0..columns {
            let a = j * stride + i;
            vp.extend_from_slice(&[a, a + 1, a + 1 + stride, a + stride]);
            offsets.push(vp.len() as i64);
        }
    }
    let positions = fixture_ok(rdk::packed::Float3::of_owned(x, y, z).map_err(|e| rdk::error::to_string(&e)));
    let topology = fixture_ok(rdk::topology::polygons_owned(stride * (rows + 1), vp, offsets).map_err(|e| rdk::error::to_string(&e)));
    fixture_ok(geo::create(positions, topology, None, None, None).map_err(|e| rdk::error::to_string(&e)))
}

