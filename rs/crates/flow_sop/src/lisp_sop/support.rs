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
            Ok(_) => out.push_str("array"),
            Err(e) => out.push_str(&format!("{e:?}")),
        }
        out.push(';');
    }
    for group in geo::groups(g) {
        out.push_str(&format!("G{:?}:{}:", rdk::group::owner(&group), rdk::group::name(&group)));
        for i in 0..rdk::group::length(&group) {
            out.push(if rdk::group::mem(i, &group) { '1' } else { '0' });
        }
        out.push(';');
    }
    out.into_bytes()
}
