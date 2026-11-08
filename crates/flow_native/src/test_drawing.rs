//! Tests of the native drawing path (`drawing`, `marks_scene`) on the CPU: the mark table a draw
//! graph produces becomes the Scene the OCaml adapter builds. (The OCaml suite reaches these only
//! through the editor tests; these are the Rust port's own checks of the ported modules.)
use crate::drawing;
use crate::marks_scene;
use flow_ir::core_exec::Buffers;
use flow_sop::lower;
use flow_syntax::syntax;
use rays::scene;
use scene_command::marks::{ColorAttr, Entry, Kind, PairAttr, ScalarAttr};

fn ok<A>(r: Result<A, rays_diagnostic::Diagnostic>) -> A {
    r.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

fn draw(body: &str) -> (lower::T, flow_syntax::value::Value) {
    let forms = ok(syntax::parse(&format!("(workspace w (graph g :context draw {body}))")));
    let lowered = ok(lower::workspace(None, Vec::new(), lower::Options::default(), &forms));
    let value = lowered.evaluated.results.iter().find(|(n, _)| n == "g").map(|(_, v)| v.clone()).expect("g");
    (lowered, value)
}

fn render(body: &str, size: (i64, i64)) -> scene::Scene {
    let (lowered, value) = draw(body);
    let prepared = ok(drawing::prepare(&drawing::PrepareOptions::default(), &lowered.evaluated.plan, &value));
    ok(drawing::render_prepared(&prepared, &input::at_time(0.), size, &drawing::RenderOptions::default()))
}

#[test]
fn a_draw_graph_becomes_a_scene() {
    let s = render(r##"(draw/merge (draw/rect [1 2 0] [30 40 0] :fill "#f00") (draw/circle [10 20 0] 4) (draw/line [0 0 0] [5 5 0]))"##, (100, 100));
    assert_eq!(s.len(), 3);
    assert!(s.iter().all(|n| matches!(n, scene::node::Shapes(_))));
}

#[test]
fn transforms_and_clips_nest() {
    let s = render("(draw/translate [1 2 0] (draw/merge (draw/point [0 0 0]) (draw/point [1 1 0])))", (10, 10));
    assert_eq!(s.len(), 1);
    assert!(matches!(&s[0], scene::node::Affine(_, children) if children.len() == 2));
}

#[test]
fn per_instance_colours_go_through_drawing_color() {
    let s = render(r##"(let* [p (map (fn [i] [i 0 0]) (array/range 3)) c (map (fn [i] [(/ i 4) 0 0]) (array/range 3))] (draw/rects p [10 10 0] :fill c))"##, (10, 10));
    assert_eq!(s.len(), 1);
}

#[test]
fn a_static_drawing_is_kept_per_size() {
    let (lowered, value) = draw(r##"(draw/circle [10 20 0] 4)"##);
    let prepared = ok(drawing::prepare(&drawing::PrepareOptions::default(), &lowered.evaluated.plan, &value));
    let live = input::at_time(0.);
    let options = drawing::RenderOptions::default();
    let a = ok(drawing::render_prepared(&prepared, &live, (50, 50), &options));
    let b = ok(drawing::render_prepared(&prepared, &live, (50, 50), &options));
    assert!(scene::same(&a, &b));
    assert!(!drawing::borrows_gpu(&prepared));
}

fn rect_set(count: i64) -> Entry {
    Entry::MarkSet {
        count,
        kind: Kind::Rect { position: PairAttr::Constant((1., 2.)), size: PairAttr::Constant((3., 4.)), fill: ColorAttr::Constant(-1), stroke: ColorAttr::Constant(0), stroke_width: ScalarAttr::Constant(1.) },
    }
}

#[test]
fn marks_scene_adapts_a_table_and_caches_constant_sets() {
    let buffers = Buffers::create();
    let cache = marks_scene::cache_new();
    let table = vec![rect_set(2), Entry::PushClip { x: 0., y: 0., width: 5., height: 5. }, rect_set(1), Entry::PopClip, Entry::Text { x: 1, y: 2, size: 12, color: -1, text: b"hi".to_vec() }];
    let a = ok(marks_scene::scene(&buffers, None, None, Some(&cache), None, &table));
    assert_eq!(a.len(), 3);
    assert!(matches!(&a[1], scene::node::Clip(0, 0, 5, 5, children) if children.len() == 1));
    assert_eq!(marks_scene::cache_size(&cache), 2);
    let b = ok(marks_scene::scene(&buffers, None, None, Some(&cache), None, &table));
    assert!(scene::same(&a, &b));
}

#[test]
fn marks_scene_refuses_what_it_cannot_draw() {
    let buffers = Buffers::create();
    let too_big = vec![Entry::PushClip { x: 1e30, y: 0., width: 5., height: 5. }, Entry::PopClip];
    assert_eq!(marks_scene::scene(&buffers, None, None, None, None, &too_big).err().map(|d| d.code), Some("E_DRAW_RANGE".to_owned()));
    let image = vec![Entry::Image { resource: 1, x: 0, y: 0, scale: 1., angle: 0. }];
    assert_eq!(marks_scene::scene(&buffers, None, None, None, None, &image).err().map(|d| d.code), Some("E_IMAGE".to_owned()));
    let unbalanced = vec![Entry::PopClip];
    assert_eq!(marks_scene::scene(&buffers, None, None, None, None, &unbalanced).err().map(|d| d.code), Some("E_DRAW_RANGE".to_owned()));
}
