//! Port of lib/rays_editor/test/test_packed_pieces.ml. The `test_timeline_and_schedule` section is the
//! assertions of `test_timeline::timeline_advances_pauses_stops_resets_and_seeks` and
//! `test_schedule::*` in src/ (bundle A ported them from the same OCaml), so it is not repeated.
use super::support::*;
use rays_math::vec3;
use sop::context::{self as sctx, Options as ContextOptions};
use sop::session;

fn fail(message: &str) -> ! {
    panic!("test_packed_pieces: {message}")
}

fn ok<T, E: std::fmt::Debug>(r: Result<T, E>) -> T {
    r.unwrap_or_else(|e| fail(&format!("{e:?}")))
}

/// Two triangles side by side, authored vertex normals and a `class` primitive attribute.
fn geometry() -> rdk_core::geometry::T {
    let positions = ok(rdk_core::packed::Float3::of_owned(vec![-2.5, -1.5, -2., 1.5, 2.5, 2.], vec![-0.5, -0.5, 0.5, -0.5, -0.5, 0.5], vec![0.; 6]));
    let topology = ok(rdk_core::topology::polygons_owned(6, vec![0, 1, 2, 3, 4, 5], vec![0, 3, 6]));
    let geometry = ok(rdk_core::geometry::create(positions, topology, None, None, None));
    let normals = ok(rdk_core::attribute::create_owned("N", rdk_core::attribute::Owner::Vertex, rdk_core::attribute::Storage::Float3(ok(rdk_core::packed::Float3::of_owned(vec![0.; 6], vec![1.; 6], vec![0.; 6])))));
    let geometry = ok(rdk_core::geometry::with_attribute(normals, &geometry));
    let pieces = ok(rdk_core::attribute::create_owned("class", rdk_core::attribute::Owner::Primitive, rdk_core::attribute::Storage::Int(std::sync::Arc::new(vec![0, 1]))));
    ok(rdk_core::geometry::with_attribute(pieces, &geometry))
}

fn positions(mesh: &rays::mesh::t) -> (Vec<f64>, Vec<f64>, Vec<f64>) {
    let v = ok(rays::mesh::View::packed_view(mesh)).vertices;
    (v.x.to_vec(), v.y.to_vec(), v.z.to_vec())
}

/// The piece offsets of the two-triangle fixture through an Exploded View node with the given
/// parameters, as the editor's terminal mesh builds them.
fn exploded(pieces: &rdk_rays::packed_pieces::T, parameters: &str) -> rays::mesh::t {
    let node = node(&[("src".to_string(), snapshot(&geometry()))], &format!(r#"(sop/exploded_view (sop/ext_src) :piece_attribute "class" {parameters})"#));
    let explosion = rdk_rays::packed_pieces::explosion(sop::node::operation(&node), &sop::node::parameter_fields(&node));
    if explosion.is_none() {
        fail("an Exploded View node carried no explosion");
    }
    ok(rdk_rays::packed_pieces::mesh_for_explosion(explosion.as_ref(), pieces))
}

pub fn exploded_view() {
    let pieces = ok(rdk_rays::packed_pieces::of_geometry("class", &geometry()));
    if rdk_rays::packed_pieces::piece_count(&pieces) != 2 {
        fail("Packed_pieces did not preserve two connected pieces");
    }
    let plain = node(&[("src".to_string(), snapshot(&geometry()))], "(sop/ext_src)");
    if rdk_rays::packed_pieces::explosion(sop::node::operation(&plain), &sop::node::parameter_fields(&plain)).is_some() {
        fail("a node that is not an Exploded View carried an explosion");
    }
    let base = positions(&exploded(&pieces, ":amount 0.0"));
    let moved = positions(&exploded(&pieces, ":amount 1.0"));
    if positions(&ok(rdk_rays::packed_pieces::mesh_for_explosion(None, &pieces))) != base {
        fail("a zero explosion moved the pieces");
    }
    let n = ok(rays::mesh::View::packed_view(&exploded(&pieces, ":amount 0.0"))).normals.expect("normals");
    if !(n.x.iter().all(|v| *v == 0.) && n.y.iter().all(|v| *v == 1.) && n.z.iter().all(|v| *v == 0.)) {
        fail("Packed_pieces replaced authored vertex normals with flat faces");
    }
    if base.0.len() != 6 {
        fail("Packed_pieces changed the vertex count");
    }
    for vertex in 0..base.0.len() {
        let expected = if base.0[vertex] < 0. { -2. } else { 2. };
        if (moved.0[vertex] - base.0[vertex] - expected).abs() > 1e-12 {
            fail("Packed_pieces explosion was not a rigid center translation");
        }
        if moved.1[vertex] != base.1[vertex] || moved.2[vertex] != base.2[vertex] {
            fail("Packed_pieces explosion changed an orthogonal coordinate");
        }
    }
    // The per-axis scale multiplies the offset: half as far along X.
    let half = positions(&exploded(&pieces, ":amount 1.0 :scale [0.5 1.0 1.0]"));
    for (vertex, x) in base.0.iter().enumerate() {
        let expected = if *x < 0. { -1. } else { 1. };
        if (half.0[vertex] - x - expected).abs() > 1e-12 {
            fail("Packed_pieces ignored the explosion's axis scale");
        }
    }
    let noisy = |seed: i64| positions(&exploded(&pieces, &format!(":amount 0.7 :noise_amount 0.4 :noise_frequency 1.3 :noise_seed {seed}")));
    let (a, b) = (noisy(19), noisy(19));
    if a != b {
        fail("Packed_pieces noise is not deterministic");
    }
    let quiet = positions(&exploded(&pieces, ":amount 0.7"));
    if a == quiet {
        fail("Packed_pieces noise moved nothing");
    }
}

pub fn boolean_fracture() {
    let source = node(&[], r#"(sop/box :connectivity "Quads" :normals "None" :size [2.0 2.0 2.0] :consolidate_points true)"#);
    let cutter = node(&[], r#"(sop/grid :width_mode "Auto" :height_mode "Auto" :columns 1 :rows 1 :size 3.0)"#);
    let fractured = node(&[("source".to_string(), source), ("cutter".to_string(), cutter)], "(sop/boolean_fracture (sop/ext_source) (sop/ext_cutter))");
    let mut s = ok(session::create(16, 16 * 1024 * 1024));
    let context = ok(sctx::create(ContextOptions { seed: Some(0), ..Default::default() }));
    let output = match session::cook(&mut s, None, &context, &fractured) {
        Ok(o) => o,
        Err(e) => fail(&rays_diagnostic::error_to_string(&e)),
    };
    session::close(&mut s);
    let g = ok(sop::payload::geometry(&output.payload));
    let pieces = rdk_core::geometry::find_attribute(rdk_core::attribute::Owner::Primitive, "piece", &g).expect("a piece attribute");
    match ok(rdk_core::attribute::storage(&pieces)) {
        rdk_core::attribute::Storage::Int(values) => {
            let maximum = values.iter().copied().fold(-1, i64::max);
            if maximum != 1 {
                fail("one cutting plane did not produce exactly two Boolean cells");
            }
            let mut counts = [0usize; 2];
            for p in values.iter() {
                counts[*p as usize] += 1;
            }
            if counts[0] <= 1 || counts[1] <= 1 {
                fail("Boolean fracture assigned individual polygons as pieces");
            }
            let packed = ok(rdk_rays::packed_pieces::of_geometry("piece", &g));
            if rdk_rays::packed_pieces::piece_count(&packed) != 2 {
                fail("Boolean fracture piece attribute did not pack two rigid shards");
            }
        }
        _ => fail("Boolean fracture piece attribute is not integer-valued"),
    }
    let _ = vec3::zero();
}
