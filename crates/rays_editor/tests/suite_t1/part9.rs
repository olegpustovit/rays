//! , part 9: run_roots (the window-free half of the two-roots section).
//! The native sections (run_native, run_view_native, run_renderers_native, run_roots_native,
//! run_budget_native) open a window through Sketch.run_state and are not ported here.
use super::support::*;
use input::{Event, Key};
use rays_editor::editor_renderer as b;
use rays_editor::editor;

fn roots_text(renderer: &str) -> String {
    format!(
        r#"(workspace roots
  (graph g :context sop (sop/box))
  (graph noon :context world (world/sun (world/sky :name "noon") :name "sun"))
  (graph dusk :context world (world/sun (world/sky :name "dusk") :name "sun"))
  (graph set :context scene (scene/merge (scene/geometry (ref g) :name "body")))
  (graph day :context scene
    (let* [cam (scene/camera :name "noon-cam" :eye [0 1 8])
           all (scene/merge (ref set) cam (scene/world (ref noon) :name "Noon"))
           root (scene/root all :camera cam :renderer "{renderer}" :width 800 :height 450 :max_spp 4096)]
      root))
  (graph night :context scene
    (let* [cam (scene/camera :name "dusk-cam" :eye [5 2 6])
           all (scene/merge (ref set) cam (scene/world (ref dusk) :name "Dusk" :exposure -2))
           root (scene/root all :camera cam :renderer "{renderer}" :width 640 :height 640 :max_spp 16 :bounces 12)]
      root))
  (graph editor :context editor
    (ui/workspace (ui/split_at "horizontal" 0.5 (ui/viewport (ref day)) (ui/viewport (ref night))))))"#
    )
}

fn eye_of(which: &str) -> rays_math::vec3::T {
    if which == "day" { rays_math::vec3::create(0., 1., 8.) } else { rays_math::vec3::create(5., 2., 6.) }
}

pub fn run_roots() {
    if !gpu() {
        return;
    }
    let f = |res, gate| b::film(true, res, gate);
    check(f((1600, 900), (1700, 1000)) == ((1600, 900), 1), "a gate over the resolution is step 1");
    check(f((1600, 900), (1600, 900)) == ((1600, 900), 1), "a gate at the resolution is step 1");
    check(f((1600, 900), (1599, 899)) == ((800, 450), 2), "one pixel under is a half");
    check(f((1600, 900), (800, 450)) == ((800, 450), 2), "the half fits its gate exactly");
    check(f((1600, 900), (799, 449)) == ((400, 225), 4), "then a quarter");
    check(f((1600, 900), (300, 169)) == ((200, 112), 8), "then an eighth");
    check(f((1600, 900), (10, 10)) == ((200, 112), 8), "never smaller than an eighth");
    // a resize inside a step leaves the film alone
    check((0..200).all(|i| {
        let w = 400 + i;
        f((1600, 900), (w, w * 9 / 16)) == ((400, 225), 4)
    }), "every gate from 400 to 599 points wide gets the same film");
    // a free view is traced in place: its film is its pane, whatever the root's resolution
    check([(1396, 1960), (640, 360), (3000, 2000)].iter().all(|gate| b::film(false, (1920, 1080), *gate) == (*gate, 1)), "a traced free view does not fill its pane pixel for pixel");
    let order: Vec<String> = ["a", "b", "c", "d"].iter().map(|s| s.to_string()).collect();
    let w = |xs: &[&str]| xs.iter().map(|s| s.to_string()).collect::<Vec<_>>();
    check(b::next_turn(&order, None, &w(&["b", "c", "d"])) == Some("b".into()), "the first wanting viewport takes the first turn");
    check(b::next_turn(&order, Some("b"), &w(&["b", "c", "d"])) == Some("c".into()), "the next takes the next");
    check(b::next_turn(&order, Some("d"), &w(&["b", "c"])) == Some("b".into()), "the rotate wraps");
    check(b::next_turn(&order, Some("c"), &w(&["b"])) == Some("b".into()), "a lone one repeats");
    check(b::next_turn(&order, Some("a"), &[]).is_none(), "no one wants, no turn");
    let mut r = Run::new(editor(&roots_text("Raster")));
    let keys: Vec<String> = shell_of(&build_ok(&editor::workspace(&r.e))).preview_sources.iter().map(|(k, _)| k.clone()).collect();
    let (day, night) = match keys.as_slice() {
        [d, n] => (d.clone(), n.clone()),
        _ => fail("two viewports, two roots"),
    };
    let p = (100., 100.);
    r.step(p, vec![]);
    r.step(p, vec![]);
    // the free view is drawn in its whole pane; only looking through the camera gates it
    let film = |r: &Run| editor::film(&r.e, &frame(&[], p, vec![], r.count));
    let (fx, fy, pane_w, pane_h) = film(&r);
    check((fx, fy) == (0, 0) && pane_w > 0 && pane_h > 0, "a free view does not fill its pane");
    r.step(p, vec![Event::KeyPressed(Key::KeyChar('/')), Event::KeyPressed(Key::KeyChar('v'))]);
    r.step(p, vec![]);
    r.step(p, vec![]);
    check(editor::look_through(&r.e), "/ v did not look through the camera");
    let (gx, gy, gate_w, gate_h) = film(&r);
    check(gx > 0 && gy > 0 && gate_w < pane_w && gate_h < pane_h && (gate_w * 450 - gate_h * 800).abs() <= 800, "looking through the camera does not show the root's 800 x 450 gate inside the pane");
    let near = |a, b| rays_math::vec3::nearly_equal(a, b, 1e-4);
    check(near(rays::camera::position(&editor::viewport_camera(&r.e, &day)), eye_of("day")), "the first viewport does not look through its own root's camera");
    // look-through is the focused viewport's own: the other keeps its orbit until its panel says so
    check(!near(rays::camera::position(&editor::viewport_camera(&r.e, &night)), eye_of("night")), "/ v looked through a viewport that does not have the focus");
    let through = editor(&replace(&roots_text("Raster"), "(ui/viewport (ref night))", "(ui/viewport (ref night) :look_through true)"));
    let through = editor::update(editor::update(through, &frame(&[], p, vec![], 1)), &frame(&[], p, vec![], 2));
    check(near(rays::camera::position(&editor::viewport_camera(&through, &night)), eye_of("night")) && !editor::look_through(&through), "the second viewport does not look through its own root's camera as its :look_through says");
    editor::close(&through);
    check(editor::slot_of(&r.e, &day).is_none() && editor::slot_of(&r.e, &night).is_none(), "a raster viewport has no tracer slot");
    r.close();
}
