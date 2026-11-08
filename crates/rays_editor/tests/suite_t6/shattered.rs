//! test_shattered_studio.ml: the studio fracture, exact across 1 and 4 domains, closed manifold shards.
#![allow(dead_code, unused_imports)]
use std::collections::HashMap;

use rdk_core::packed::Float3;
use sop::{context, node, session};

use super::super::support::*;

fn with_cook<R>(domains: i64, f: &dyn Fn(&dyn Fn(&node::T) -> session::Output) -> R) -> R {
    let mut s = session::create(32, 256 * 1024 * 1024).unwrap();
    let c = context::create(context::Options { domains: Some(domains), grain: Some(2), ..Default::default() }).unwrap();
    let cell = std::cell::RefCell::new(&mut s);
    let r = f(&|n| session::cook(&mut cell.borrow_mut(), None, &c, n).unwrap_or_else(|e| panic!("{}", rays_diagnostic::error_to_string(&e))));
    session::close(&mut cell.borrow_mut());
    r
}

fn edge_counts(g: &rdk_core::geometry::T) -> (i64, i64) {
    let index = rdk_core::topology_index::create(None, rdk_core::geometry::topology(g)).unwrap();
    let (mut boundary, mut non_manifold) = (0, 0);
    for edge in 0..rdk_core::topology_index::edge_count(&index) {
        let n = rdk_core::topology_index::edge_incidence_count(&index, edge).unwrap();
        if n == 1 {
            boundary += 1;
        }
        if n > 2 {
            non_manifold += 1;
        }
    }
    (boundary, non_manifold)
}

pub fn shattered_studio() {
    let text = read_repo("sketches/shattered_studio.rays");
    let workspace = load(&text);
    let graphs = rays_editor::Workspace::sop_graphs(None, &workspace).unwrap();
    let graph = graphs.iter().find(|(n, _)| n == "shards").expect("shards").1.clone();
    let fracture_id = sop::node::inspect(&graph).into_iter().find(|i| i.operation == "boolean_fracture").expect("fracture").id;
    let fracture = sop::node::find(&graph, fracture_id).unwrap();
    let one = with_cook(1, &|cook| cook(&fracture));
    let four = with_cook(4, &|cook| cook(&fracture));
    let g = sop::payload::geometry(&one.payload).unwrap();
    let h = sop::payload::geometry(&four.payload).unwrap();
    assert!(rdk_core::geometry::point_count(&g) == rdk_core::geometry::point_count(&h));
    assert!(rdk_core::geometry::primitive_count(&g) == rdk_core::geometry::primitive_count(&h));
    for i in 0..rdk_core::geometry::point_count(&g) {
        assert!(Float3::get(rdk_core::geometry::positions(&g), i) == Float3::get(rdk_core::geometry::positions(&h), i));
    }
    let tg = rdk_core::topology::View::view(rdk_core::geometry::topology(&g)).unwrap();
    let th = rdk_core::topology::View::view(rdk_core::geometry::topology(&h)).unwrap();
    assert!(tg.vertex_points == th.vertex_points && tg.primitive_offsets == th.primitive_offsets);
    let pieces = |geometry: &rdk_core::geometry::T| match rdk_core::attribute::storage(&rdk_core::geometry::find_attribute(rdk_core::attribute::Owner::Primitive, "piece", geometry).expect("piece")).unwrap() {
        rdk_core::attribute::Storage::Int(a) => a,
        _ => panic!("fracture lost its piece IDs"),
    };
    let piece_ids = pieces(&g);
    assert!(piece_ids == pieces(&h));
    let mut dense: HashMap<i64, i64> = HashMap::new();
    let membership: Vec<i64> = piece_ids
        .iter()
        .map(|id| {
            let next = dense.len() as i64;
            *dense.entry(*id).or_insert(next)
        })
        .collect();
    let count = dense.len() as i64;
    assert!(count >= 1000, "{count} pieces");
    let parts = rdk_ops::deletion::primitive_partitions(count, &membership, &g).unwrap();
    for part in &parts {
        assert!(edge_counts(part) == (0, 0));
    }
    let source = sop::node::inputs(&fracture).remove(0);
    let source = with_cook(1, &|cook| cook(&source));
    let original_volume = rdk_core::analysis::signed_volume(&sop::payload::geometry(&source.payload).unwrap()).unwrap();
    let fractured_volume = rdk_core::analysis::signed_volume(&g).unwrap();
    assert!((original_volume - fractured_volume).abs() < 1e-7 * original_volume.abs());
    let colored = with_cook(1, &|cook| cook(&graph));
    let geometry = sop::payload::geometry(&colored.payload).unwrap();
    let prepared = rdk_rays::surface::of_geometry(colored.instances.as_ref().map(|i| (**i).clone()), &geometry).unwrap();
    let explosion = rdk_rays::packed_pieces::explosion(sop::node::operation(&graph), &sop::node::parameter_fields(&graph));
    let drawings = rays::scene3::walk::drawings(&rdk_rays::surface::scene3(explosion.as_ref(), None, &prepared).unwrap());
    assert!(drawings.len() == 4, "{} material batches", drawings.len());
}
