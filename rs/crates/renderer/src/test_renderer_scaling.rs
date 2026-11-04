//! lib/renderer/test_renderer_scaling.ml. The OCaml test also counted minor-heap words per cached
//! call; Rust has no such counter, so the cached path is checked for identity instead (the same
//! draws come back without being rebuilt).
use crate::renderer::Native;
use crate::renderer_lowering::default_state;
use crate::scene_execution::{empty_bytes, geometry_draw, mesh, mesh_key, pipeline_family, sampled_draw};
use ogpu::pipeline::Blend;
use ogpu::render_pass::Primitive;
use runtime::{Change_flag, frame_facts};
use std::rc::Rc;

fn facts(drawable_width: i64, drawable_height: i64, scale_x: f64, scale_y: f64) -> frame_facts {
    frame_facts { logical_width: 4, logical_height: 4, drawable_width, drawable_height, pixel_scale_x: scale_x, pixel_scale_y: scale_y }
}

#[test]
fn renderer_scaling() {
    let the_mesh = mesh {
        key: mesh_key::Host("scale".to_owned()),
        vertices: empty_bytes(),
        vertex_count: 1,
        indices: empty_bytes(),
        index_count: 1,
        primitive: Primitive::TriangleList,
    };
    let draw = geometry_draw { mesh: the_mesh, state: default_state((1, 2, 3, 4), (0, 0, 4, 4)) };
    let one = facts(4, 4, 1.0, 1.0);
    let retina = facts(8, 12, 2.0, 3.0);
    let draws = vec![draw.clone()];
    let same = Native::scale_draws(&one, &draws);
    assert!(same.len() == 1 && same[0].state == draw.state, "1x draw graph was changed");
    let sampled: Vec<Rc<sampled_draw>> = vec![Rc::new(sampled_draw {
        family: pipeline_family::Scene2,
        blend: Blend::Replace,
        texture: None,
        auxiliary: None,
        vertex_attributes: None,
        samples: 1,
        draw: draw.clone(),
    })];
    let same_sampled = Native::scale_sampled_resources(&one, &sampled);
    assert!(Rc::ptr_eq(&same_sampled[0], &sampled[0]), "1x sampled graph was copied");
    let scaled = Native::scale_draws(&retina, &draws).remove(0);
    assert!(scaled.state.viewport == (2, 6, 6, 12) && scaled.state.scissor == (0, 0, 8, 12), "Retina draw edges were not scaled exactly");
    let unchanged = geometry_draw { state: default_state((0, 0, 0, 0), (0, 0, 0, 0)), ..draw.clone() };
    let unchanged_sampled: Vec<Rc<sampled_draw>> = vec![Rc::new(sampled_draw { draw: unchanged, ..(*sampled[0]).clone() })];
    assert!(
        Rc::ptr_eq(&Native::scale_sampled_resources(&retina, &unchanged_sampled)[0], &unchanged_sampled[0]),
        "unchanged Retina draw was copied"
    );
    let resized = facts(12, 8, 2.0, 3.0);
    let changed = Native::scale_draws(&resized, &draws).remove(0);
    assert!(changed.state.viewport == (3, 4, 9, 8) && changed.state.scissor == (0, 0, 12, 8), "resized nonuniform edges were not recomputed");
    // The per-frame path memoizes on the draw list's identity: the same list and facts return the
    // same scaled draws; a new list rescales.
    let mut cache = Native::new_scaled_cache();
    let many: Vec<Rc<sampled_draw>> = (0..1000).map(|_| sampled[0].clone()).collect();
    let first = Native::scale_sampled_cached(&mut cache, &retina, &many);
    assert!(!Rc::ptr_eq(&first[0], &many[0]) && first.len() == 1000, "cached Retina list was not scaled");
    let again = Native::scale_sampled_cached(&mut cache, &retina, &many);
    assert!(again.iter().zip(first.iter()).all(|(a, b)| Rc::ptr_eq(a, b)), "unchanged draw list was rescaled");
    let shorter = Native::scale_sampled_cached(&mut cache, &retina, &many[1..]);
    assert_eq!(shorter.len(), 999, "a different draw list reused the cached scaling");
    let bypass = Native::scale_sampled_cached(&mut cache, &one, &many);
    assert!(bypass.iter().zip(many.iter()).all(|(a, b)| Rc::ptr_eq(a, b)), "1x facts did not bypass the cache");
    // A window is not asked for its size every frame: SDL announces a change, and one announcement
    // costs exactly one query however many arrive.
    let mut flag = Change_flag::create();
    let mut queries = 0;
    for _ in 0..1000 {
        assert!(!Change_flag::refresh(&mut flag, &mut || queries += 1), "a frame with no size event queried the window");
    }
    assert_eq!(queries, 0, "quiet frames queried the window");
    Change_flag::announce(&mut flag);
    Change_flag::announce(&mut flag);
    assert!(Change_flag::refresh(&mut flag, &mut || queries += 1) && queries == 1, "an announced change did not query the window once");
    assert!(!Change_flag::refresh(&mut flag, &mut || queries += 1), "one announcement queried the window twice");
    Change_flag::announce(&mut flag);
    assert!(Change_flag::refresh(&mut flag, &mut || queries += 1) && queries == 2, "a later announcement was lost");
}
