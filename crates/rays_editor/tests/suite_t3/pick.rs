//! Port of lib/rays_editor/test/test_viewport_pick.ml (W6: viewport provenance, CPU ray picks,
//! the highlight, and the editor's click-to-select), using the original sketches.
use super::probe_t::fixture as case_text;

use super::t3c::*;
use flow_sop::lower;
use input::{Event, Key, MouseButton};
use rays::camera;
use rays_editor::cook;
use rays_editor::pick;
use rays_editor::editor;
use rays_math::{mat4, vec3};
use std::cell::Cell;
use std::rc::Rc;
use std::sync::Arc;

fn fail(message: &str) -> ! {
    panic!("test_viewport_pick: {message}")
}

fn check(condition: bool, message: &str) {
    if !condition {
        fail(message);
    }
}

fn lower_case(name: &str) -> lower::T {
    let forms = flow_syntax::syntax::parse(&case_text(name)).unwrap_or_else(|d| fail(&rays_diagnostic::to_string(&d)));
    lower::workspace(Some(sop::nodes::factories()), editor_document::kinds::descriptors(), lower::Options::default(), &forms).unwrap_or_else(|d| fail(&rays_diagnostic::to_string(&d)))
}

/// The cooked piece of a lowered fixture; `prepared` counts the prepares.
fn cooked(lit: &pick::Set, existing: Option<cook::Cook>, lowered: &lower::T, prepared: &Rc<Cell<i64>>) -> cook::Cook {
    let cook = match existing {
        Some(c) => c,
        None => {
            let counter = prepared.clone();
            let prepare: cook::Prepare_fn = Rc::new(move |s, o| {
                counter.set(counter.get() + 1);
                cook::prepare(s, o)
            });
            let c = cook::create(prepare, 42, 97, Some(1), Some(true), None, 512, 256 * 1024 * 1024).expect("cook");
            cook::set_volatile(&c, lower::is_volatile(lowered));
            c
        }
    };
    let objects: Vec<_> = lower::objects(lowered).into_iter().map(|(id, n, d)| (id as i64, n, d)).collect();
    let mut f = frame_at((0., 0.), vec![], 0);
    f.input.dt = 0.;
    let update = cook::update(None, None, false, &[], lit, None, &cook, &super::cook_t::none_settings(), &objects.iter().map(|(i, n, d)| (*i, n, *d)).collect::<Vec<_>>(), None, param::Effects::default(), &[], &rays_editor::Timeline::create(), &f, None);
    check(update.edit_error.is_none(), "cook error");
    update.cook
}

fn primitives_of(tags: &[i64], tag: i64) -> Vec<usize> {
    (0..tags.len()).filter(|p| tags[*p] == tag).collect()
}

/// A point just above the highest primitive of `tag`.
fn above(geometry: &rdk_core::geometry::T, tag: i64) -> vec3::T {
    let tags = pick::tags(geometry).expect("tags");
    let topology = rdk_core::geometry::topology(geometry);
    let points = rdk_core::geometry::positions(geometry);
    let centre = |primitive: usize| -> (f64, f64, f64) {
        let (first, last) = rdk_core::topology::primitive_vertex_range(topology, primitive as i64).expect("range");
        let mut sum = (0., 0., 0.);
        for v in first..last {
            let (x, y, z) = rdk_core::packed::float3::get(points, rdk_core::topology::point_of_vertex(topology, v).expect("point")).expect("position");
            sum = (sum.0 + x, sum.1 + y, sum.2 + z);
        }
        let n = (last - first) as f64;
        (sum.0 / n, sum.1 / n, sum.2 / n)
    };
    let mut best: Option<(usize, (f64, f64, f64))> = None;
    for p in primitives_of(&tags, tag) {
        let c = centre(p);
        match best {
            Some((_, b)) if b.1 >= c.1 => {}
            _ => best = Some((p, c)),
        }
    }
    let (x, y, z) = best.expect("a primitive").1;
    vec3::create(x, y + 1e-3, z)
}

fn present(geometry: &rdk_core::geometry::T, tag: i64) -> bool {
    pick::tags(geometry).expect("tags").contains(&tag)
}

fn geometry_of(piece: &cook::piece) -> rdk_core::geometry::T {
    sop::payload::geometry(&piece.output.payload).expect("geometry")
}

pub fn pick_sunflower() {
    let sunflower = lower_case("sunflower");
    let prepared = Rc::new(Cell::new(0i64));
    let cook = cooked(&pick::Set::new(), None, &sunflower, &prepared);
    let piece = cook::pieces(&cook)[0].clone();
    let geometry = geometry_of(&piece);
    let tags = pick::tags(&geometry).expect("tags");
    let of_iteration = |i: i64| -> Vec<i64> { sunflower.provenance.iter().filter(|(_, o)| o.iter == vec![i]).map(|(t, _)| *t).collect() };
    let iteration = super::probe_t::SEEDS / 2;
    let tag = of_iteration(iteration)[0];
    let origin = &sunflower.provenance[&tag];
    check(origin.iter == vec![iteration] && origin.input == iteration, "the provenance table names the iteration");
    check(tags.len() as i64 == rdk_core::geometry::primitive_count(&geometry) && sunflower.provenance.len() as i64 == super::probe_t::SEEDS, "a tag per primitive, one origin per seed");
    match cook::pick(&piece, above(&geometry, tag), vec3::create(0., -1., 0.)) {
        Some((_, hit)) => check(hit == tag, "the ray finds the seed"),
        None => fail("the ray missed the seed"),
    }
    check(cook::pick(&piece, vec3::create(0., 9., 9.), vec3::create(0., -1., 0.)).is_none(), "a miss");
    // instanced pieces: the prototype is picked at every instance, hits compare in world units
    let over = above(&geometry, tag);
    let down = vec3::create(0., -1., 0.);
    let instanced = |transforms: Vec<mat4::T>| {
        let mut p = piece.clone();
        p.output.instances = Some(Arc::new(transforms));
        p
    };
    let hit = |p: &cook::piece, origin: vec3::T| cook::pick(p, origin, down);
    let d1 = hit(&piece, over).map(|(d, _)| d).unwrap_or_else(|| fail("no plain hit"));
    let shifted = vec3::create(over.x + 100., over.y, over.z);
    match hit(&instanced(vec![mat4::identity(), mat4::translation(vec3::create(100., 0., 0.))]), shifted) {
        Some((d, t)) => check(t == tag && (d - d1).abs() < 1e-9, "the second instance was not picked"),
        None => fail("an instanced piece was not picked"),
    }
    check(hit(&instanced(vec![mat4::translation(vec3::create(100., 0., 0.))]), over).is_none(), "the prototype at the origin was picked without an instance there");
    let big = vec3::create(2. * over.x, 2. * over.y, 2. * over.z);
    match hit(&instanced(vec![mat4::scaling(vec3::create(2., 2., 2.))]), big) {
        Some((d, t)) => check(t == tag && (d - 2. * d1).abs() < 1e-7, "a scaled instance's distance is not in world units"),
        None => fail("a scaled instance was not picked"),
    }
    match hit(&instanced(vec![mat4::translation(vec3::create(0., -5., 0.)), mat4::identity()]), over) {
        Some((d, _)) => check((d - d1).abs() < 1e-9, "the nearest instance wins"),
        None => fail("the nearest instance was not picked"),
    }
    // the highlight: prepared again from the kept output, never recooked
    let misses = cook::stats(&cook).misses;
    let before = prepared.get();
    let lit: pick::Set = [tag].into_iter().collect();
    let cook = cooked(&lit, Some(cook), &sunflower, &prepared);
    check(prepared.get() == before + 1, "a new highlight prepares the piece once");
    check(cook::stats(&cook).misses == misses, "the highlight does not recook");
    let lit_piece = cook::pieces(&cook)[0].clone();
    let colours = rdk_core::geometry::find_attribute(rdk_core::attribute::Owner::Vertex, "Cd", &lit_piece.prepared.geometry);
    match colours.map(|a| rdk_core::attribute::storage(&a)) {
        Some(Ok(rdk_core::attribute::Storage::Float4(c))) => {
            let topology = rdk_core::geometry::topology(&lit_piece.prepared.geometry);
            let corner = |p: usize| rdk_core::topology::primitive_vertex_range(topology, p as i64).expect("range").0;
            let seed = primitives_of(&tags, tag)[0];
            let other = primitives_of(&tags, of_iteration(0)[0])[0];
            let red = |i: i64| rdk_core::packed::float4::get(&c, i).expect("colour").0;
            check((red(corner(other)) - 0.3).abs() < 1e-9, "the rest is dimmed");
            check(red(corner(seed)) > red(corner(other)), "the probed iteration is tinted");
        }
        _ => fail("no vertex colour"),
    }
    let cook = cooked(&lit, Some(cook), &sunflower, &prepared);
    check(prepared.get() == before + 1, "an unchanged highlight prepares nothing");
    let cook = cooked(&pick::Set::new(), Some(cook), &sunflower, &prepared);
    check(prepared.get() == before + 1, "deselecting takes the first preparation back from the cache");
    check(cook::stats(&cook).misses == misses, "deselecting does not recook");
    check(rdk_core::geometry::data_id(&cook::pieces(&cook)[0].prepared.geometry) == rdk_core::geometry::data_id(&geometry), "deselect restores the geometry");
    cook::close(&cook);
}

pub fn pick_bloom() {
    let bloom = lower_case("bloom");
    let prepared = Rc::new(Cell::new(0i64));
    let cook = cooked(&pick::Set::new(), None, &bloom, &prepared);
    let piece = cook::pieces(&cook)[0].clone();
    let geometry = geometry_of(&piece);
    let petal: Vec<_> = bloom.provenance.iter().filter(|(_, o)| o.iter == vec![4]).collect();
    check(petal.len() == 1 || petal.len() == 2, "petal 4 of the first flower");
    let (tag, o) = bloom.provenance.iter().find(|(t, o)| present(&geometry, **t) && o.iter == vec![4]).map(|(t, o)| (*t, o.clone())).expect("a petal");
    match cook::pick(&piece, above(&geometry, tag), vec3::create(0., -1., 0.)) {
        Some((_, hit)) => {
            let hit = &bloom.provenance[&hit];
            check(hit.iter == vec![4] && hit.site == o.site, "a petal picks its iteration through two merges");
        }
        None => fail("the ray missed petal 4"),
    }
    let heart = bloom.provenance.iter().find(|(t, o)| o.iter.is_empty() && o.input == 1 && present(&geometry, **t)).expect("the heart");
    check(heart.1.site != o.site, "the heart is another site");
    cook::close(&cook);
}

/// The editor picks the same way: a click in the viewport is a ray through its camera, and the
/// node that made the clicked box is selected in the graph pane.
fn flat(text: &str, clicks: &[((f64, f64), &str, &str)]) {
    let workspace = load(text);
    let camera = rays::easy_camera::create(None, None, None, None, None, Some(false)).expect("camera");
    let mut d = Driver::new(create(editor::Create { domains: Some(1), camera: Some(camera), ..options() }, &workspace));
    let selected = |d: &Driver| dump_line(d.e(), "scope selected");
    for _ in 0..6 {
        d.step(vec![]);
    }
    let panes = editor::panes(d.e(), &frame_at((0., 0.), vec![], 0));
    // the scene opens as a list: a click on the object's row selects it and [i] enters its graph
    let (gx, gy, _, _) = panes.graph;
    let row = ((gx + 60) as f64, (gy + 24 + 12) as f64);
    d.step_at(row, vec![]);
    d.step_at(row, vec![Event::Pointer_pressed(MouseButton::LeftButton, row), Event::Pointer_released(MouseButton::LeftButton, row)]);
    d.step_at(row, vec![ch('i')]);
    d.count += 30; // past the double-click interval
    for _ in 0..4 {
        d.step(vec![]);
    }
    let at = |d: &Driver, (x, y): (f64, f64)| -> (f64, f64) {
        match camera::world_to_screen(panes.view, &editor::view_camera(d.e()), vec3::create(x, y, 0.)).expect("projection") {
            Some(p) => (p.x, p.y),
            None => panic!("the point is behind the camera"),
        }
    };
    for (world, expected, message) in clicks {
        let p = at(&d, *world);
        d.step_at(p, vec![Event::Pointer_moved(p)]);
        d.step_full(&[MouseButton::LeftButton], &[], p, vec![Event::Pointer_pressed(MouseButton::LeftButton, p)], 1);
        d.step_at(p, vec![Event::Pointer_released(MouseButton::LeftButton, p)]);
        d.step_at(p, vec![]);
        let got = selected(&d);
        if got != *expected {
            panic!("{message}: {got}");
        }
    }
    d.close();
    let _ = Key::Shift;
}

pub fn flat_boxes() {
    flat(
        r#"(workspace flat
    (graph g :context sop
      (let* [left (sop/transform (sop/box) :translate [-2 0 0])
             right (sop/transform (sop/box) :translate [2 0 0])]
        (sop/merge left right))))"#,
        &[((2., 0.), "g/right", "a click on the right box selected"), ((-2., 0.), "g/left", "a click on the left box selected"), ((0., 0.), "-", "a click on nothing kept the selection")],
    );
}

pub fn flat_loops() {
    let loop_text = |extra: &str| {
        format!(
            r#"(workspace looped
    (graph g :context sop
      (let* [boxes (for [i (range 2)] (sop/transform (sop/box) :translate [(- (* i 4) 2) 0 0]))
             all (sop/merge boxes)]
        all)))
{extra}"#
        )
    };
    // a pick inside a collapsed loop selects the loop, which shows what is inside
    flat(&loop_text(""), &[((-2., 0.), "g/boxes/@result", "a click inside an open loop selects the body's node")]);
    flat(&loop_text("(layout (node [\"g\" \"boxes\"] :collapsed true))"), &[((-2., 0.), "g/boxes", "a click inside a collapsed loop did not select the loop")]);
}
