//! test_editor_input.ml: the host shields popups/World edits and retains viewport/graph gesture owners.
use super::t4::*;
use editor_document::workspace_doc;
use input::{Event, Key, MouseButton::*};
use rays_editor::editor;

const M0: (f64, f64) = (100., 300.);

fn palette() -> Vec<Event> {
    vec![key(Key::KeyChar('/')), chr('/')]
}
fn wheel(p: (f64, f64)) -> Vec<Event> {
    vec![Event::Pointer_moved(p), Event::Scrolled((0., 1.))]
}

pub const WORLD: &str = r#"(workspace world
    (graph g :context sop (sop/box))
    (graph sky :context world (world/none))
    (graph scene :context scene
      (scene/merge (scene/geometry (ref g)) (scene/world (ref sky)))))"#;

type Cam = (rays_math::vec3::T, rays_math::vec3::T, f64);

fn camera(d: &Driver) -> Cam {
    let c = editor::camera(d.e());
    let cam = rays::easy_camera::camera(&c).unwrap();
    (rays::camera::position(&cam), rays::camera::target(&cam), rays::easy_camera::distance(&c))
}

fn snapshot(d: &Driver, dir: &str) -> String {
    editor::crash_dump(d.e(), dir).unwrap();
    std::fs::read_to_string(format!("{dir}/document.txt")).unwrap()
}

fn rotation(d: &Driver, dir: &str) -> f64 {
    let text = snapshot(d, dir);
    let line = text.lines().find(|l| !l.trim().is_empty() && l.split(' ').any(|w| w == "world") && l.starts_with("  node")).expect("a world node line");
    line.split(' ').find_map(|w| w.strip_prefix("rotation=").map(|v| float_of_string(v))).unwrap()
}

pub fn run() {
    if !crate::common::gpu() {
        return;
    }
    let name = "Editor";
    let workspace: workspace_doc::T = of_text(None, WORLD);
    let dir = temp_dir("input");
    let camera_opt = rays::easy_camera::create(None, None, None, None, None, Some(false)).unwrap();
    let ed = editor::create(editor::Create { ui_text_size: Some(11), platform: Some(input::Platform::Apple), camera: Some(camera_opt), ..Default::default() }, &workspace, None, editor::prepare_fn(), editor::draw_fn()).expect("opens");
    let mut d = Driver::new(ed);
    let graph_rect = |d: &Driver| editor::panes(d.e(), &frame0()).graph;
    let view_rect = |d: &Driver| editor::panes(d.e(), &frame0()).view;

    d.step(vec![]);
    let before_wheel = camera(&d);
    d.step_at((800., 300.), vec![Event::Pointer_moved(M0), Event::Scrolled((0., 1.)), Event::Pointer_moved((800., 300.))]);
    assert!(camera(&d) != before_wheel, "{name}: a later pane crossing discarded an owned wheel event");
    let after_wheel = camera(&d);
    d.step_at(M0, vec![Event::Pointer_moved((800., 300.)), Event::Scrolled((0., 1.)), Event::Pointer_moved(M0)]);
    assert!(camera(&d) == after_wheel, "{name}: an inspector-owned wheel reached the viewport after crossing");
    let original = snapshot(&d, &dir);
    d.step(palette());
    d.step(vec![]);
    // modal gestures
    d.step_at((330., 300.), wheel((330., 300.)));
    for button in [LeftButton, RightButton, MiddleButton] {
        d.bk(&[button], &[], M0, vec![Event::Pointer_pressed(button, M0)]);
        d.full(&[button], &[Key::Shift], (70., 20.), (170., 320.), vec![Event::Pointer_moved((170., 320.))]);
        d.step_at((170., 320.), vec![Event::Pointer_released(button, (170., 320.))]);
        d.step(palette());
        d.step(vec![]);
    }
    assert!(snapshot(&d, &dir) == original, "{name}: modal wheel/pointer/Shift-World gestures changed document or camera");
    d.step(vec![key(Key::Escape)]);
    d.step(vec![]);
    let original = snapshot(&d, &dir);
    // Newly opened and dismissed popups own all remaining pointer events even in one frame.
    let mut ev = palette();
    ev.extend(wheel((330., 300.)));
    d.step_at((330., 300.), ev);
    assert!(snapshot(&d, &dir) == original, "{name}: same-frame popup opening leaked a wheel");
    let mut ev = vec![key(Key::Escape), Event::Pointer_pressed(RightButton, M0), Event::Pointer_moved((170., 320.)), Event::Pointer_released(RightButton, (170., 320.))];
    ev.extend(wheel(M0));
    d.step_at(M0, ev);
    assert!(snapshot(&d, &dir) == original, "{name}: same-frame popup dismissal leaked a press/move/release/wheel");
    d.step(vec![]);

    let original = snapshot(&d, &dir);
    let before = camera(&d);
    let s = |d: &Driver| snapshot(d, &dir);
    d.step_at((140., 300.), vec![key(Key::Shift), Event::Pointer_pressed(LeftButton, M0), Event::Pointer_moved((130., 300.)), Event::Key_released(Key::Shift), Event::Pointer_released(LeftButton, (140., 300.))]);
    assert!(rotation(&d, &dir) == 20. && camera(&d) == before, "{name}: same-frame World drag lost ordered movement or also navigated");
    d.bk(&[], &[Key::Meta], M0, vec![chr('z')]);
    assert!(s(&d) == original, "one same-frame World drag did not undo as one operation");
    d.bk(&[LeftButton], &[Key::Shift], (120., 300.), vec![Event::Pointer_pressed(LeftButton, M0), Event::Pointer_moved((120., 300.))]);
    assert!(rotation(&d, &dir) == 10., "{name}: first-frame held World movement was lost");
    d.bk(&[LeftButton], &[], (150., 300.), vec![Event::Key_released(Key::Shift), Event::Pointer_moved((150., 300.))]);
    assert!(rotation(&d, &dir) == 25. && camera(&d) == before, "{name}: releasing Shift changed the operation during a World drag");
    d.step_at((160., 300.), vec![Event::Pointer_released(LeftButton, (160., 300.))]);
    assert!(rotation(&d, &dir) == 30., "{name}: World drag dropped its final release position");
    let first_drag = s(&d);
    d.step_at((120., 300.), vec![key(Key::Shift), Event::Pointer_pressed(LeftButton, M0), Event::Pointer_released(LeftButton, (120., 300.)), Event::Key_released(Key::Shift)]);
    assert!(rotation(&d, &dir) == 40., "{name}: second World drag did not apply");
    d.bk(&[], &[Key::Meta], M0, vec![chr('z')]);
    assert!(s(&d) == first_drag, "two separate World drags merged in history");
    d.bk(&[], &[Key::Meta], M0, vec![chr('z')]);
    assert!(s(&d) == original, "release-frame movement split one World drag into multiple undo entries");
    d.bk(&[LeftButton], &[Key::Shift], M0, vec![Event::Pointer_pressed(LeftButton, M0)]);
    d.bk(&[LeftButton], &[], M0, palette());
    d.step(vec![key(Key::Escape)]);
    d.step(vec![]);
    d.step_at((200., 300.), vec![Event::Pointer_moved((200., 300.))]);
    assert!(s(&d) == original, "a popup-cancelled World drag resumed on movement");
    for cancellation in [Event::Focus_lost, Event::Pointer_cancelled(LeftButton)] {
        d.bk(&[LeftButton], &[Key::Shift], M0, vec![Event::Pointer_pressed(LeftButton, M0)]);
        d.step(vec![cancellation]);
        d.step_at((220., 300.), vec![Event::Pointer_moved((220., 300.))]);
        assert!(s(&d) == original, "a cancelled World drag resumed on movement");
    }
    d.bk(&[LeftButton], &[Key::Shift], (360., 300.), vec![Event::Pointer_pressed(LeftButton, (360., 300.))]);
    d.bk(&[LeftButton], &[Key::Shift], (430., 300.), vec![Event::Pointer_moved((430., 300.))]);
    assert!(rotation(&d, &dir) == 35. && camera(&d) == before, "{name}: a World drag lost ownership at a pane crossing");
    d.step_at((430., 300.), vec![Event::Pointer_released(LeftButton, (430., 300.))]);
    d.bk(&[], &[Key::Meta], M0, vec![chr('z')]);
    assert!(s(&d) == original, "cross-pane World drag did not undo as one operation");
    d.step_at((-2300., 300.), vec![key(Key::Shift), Event::Pointer_pressed(LeftButton, M0), Event::Pointer_released(LeftButton, (-2300., 300.)), Event::Key_released(Key::Shift)]);
    assert!(rotation(&d, &dir) == -120., "{name}: a large negative World drag did not wrap rotation");
    d.bk(&[], &[Key::Meta], M0, vec![chr('z')]);
    assert!(s(&d) == original, "large negative World drag did not undo in one entry");
    // a press and release is one gesture per frame (the capture signal holds one); two frames
    d.step_at((120., 300.), vec![key(Key::Shift), Event::Pointer_pressed(LeftButton, M0), Event::Pointer_released(LeftButton, (120., 300.))]);
    d.bk(&[], &[Key::Shift], (140., 300.), vec![key(Key::Shift), Event::Pointer_pressed(LeftButton, M0), Event::Pointer_released(LeftButton, (140., 300.)), Event::Key_released(Key::Shift)]);
    assert!(rotation(&d, &dir) == 30., "{name}: consecutive World gestures lost an operation");
    d.bk(&[], &[Key::Meta], M0, vec![chr('z')]);
    assert!(rotation(&d, &dir) == 10., "{name}: consecutive World gestures merged in history");
    d.bk(&[], &[Key::Meta], M0, vec![chr('z')]);
    assert!(s(&d) == original, "batched World gesture undo did not restore the original");
    d.bk(&[], &[Key::Shift], M0, vec![]);
    d.step_at((140., 300.), vec![Event::Pointer_pressed(LeftButton, M0), Event::Pointer_moved((140., 300.)), Event::Focus_lost]);
    assert!(rotation(&d, &dir) == 20., "{name}: final focus loss erased Shift from earlier World events");
    d.bk(&[], &[Key::Meta], M0, vec![chr('z')]);
    assert!(s(&d) == original, "focus-cancelled World drag did not undo in one entry");

    // Start in the view, cross both other panes, and finish outside it.
    let before = camera(&d);
    d.bk(&[RightButton], &[], M0, vec![Event::Pointer_pressed(RightButton, M0)]);
    d.bk(&[RightButton], &[], (500., 320.), vec![Event::Pointer_moved((500., 320.))]);
    let across_graph = camera(&d);
    assert!(across_graph != before, "{name}: viewport drag stopped at the graph pane");
    d.bk(&[RightButton], &[], (800., 350.), vec![Event::Pointer_moved((800., 350.))]);
    assert!(camera(&d) != across_graph, "{name}: viewport drag stopped at the inspector pane");
    d.step_at((800., 350.), vec![Event::Pointer_released(RightButton, (800., 350.))]);
    let released = camera(&d);
    d.step(vec![Event::Pointer_moved((110., 300.))]);
    assert!(camera(&d) == released, "{name}: released drag resumed on re-entry");
    d.bk(&[RightButton], &[], M0, vec![Event::Pointer_pressed(RightButton, M0)]);
    d.step(vec![Event::Focus_lost]);
    let cancelled = camera(&d);
    d.step(vec![Event::Pointer_moved((140., 330.))]);
    assert!(camera(&d) == cancelled, "{name}: focus loss did not end the viewport drag");
    d.bk(&[RightButton], &[], M0, vec![Event::Pointer_pressed(RightButton, M0)]);
    d.bk(&[RightButton], &[], M0, palette());
    d.step(vec![key(Key::Escape)]);
    d.step(vec![]);
    let cancelled = camera(&d);
    d.step(vec![Event::Pointer_moved((190., 350.))]);
    assert!(camera(&d) == cancelled, "{name}: popup did not cancel an existing drag");

    // the scene opens as a list: / l g in the graph pane shows the scene graph
    let (gx, gy, _, _) = graph_rect(&d);
    let point = ((gx + 25) as f64, (gy + 300) as f64);
    d.step_at(point, vec![Event::Pointer_pressed(LeftButton, point), Event::Pointer_released(LeftButton, point)]);
    d.step(vec![key(Key::KeyChar('/')), chr('l'), chr('g')]);
    d.step(vec![]);
    let (gx, gy, _, _) = graph_rect(&d);
    let point = ((gx + 25) as f64, (gy + 300) as f64);
    let before = camera(&d);
    d.bk(&[MiddleButton], &[], point, vec![Event::Pointer_pressed(MiddleButton, point)]);
    d.bk(&[MiddleButton], &[], M0, vec![Event::Pointer_moved(M0)]);
    d.step(vec![Event::Pointer_released(MiddleButton, M0)]);
    assert!(camera(&d) == before, "{name}: graph-owned pan began viewport navigation");
    // a press in the graph pane that drags out over the view is the pane's, never navigation
    let (gx, gy, _, gh) = graph_rect(&d);
    let point = ((gx + 12) as f64, (gy + gh - 12) as f64);
    d.step_at(point, vec![key(Key::Home)]);
    d.step(vec![]);
    d.bk(&[LeftButton], &[], point, vec![Event::Pointer_pressed(LeftButton, point)]);
    d.bk(&[LeftButton], &[], M0, vec![Event::Pointer_moved(M0)]);
    d.step(vec![Event::Pointer_released(LeftButton, M0)]);
    assert!(camera(&d) == before, "{name}: a graph pane drag reached viewport navigation");
    d.count += 30; // past the double-click interval
    // Tab adds in the graph; shared chrome traversal still works from View.
    d.step_at(M0, vec![Event::Pointer_pressed(LeftButton, M0), Event::Pointer_released(LeftButton, M0)]);
    let (_, _, width, _) = view_rect(&d);
    d.step(vec![key(Key::Tab), key(Key::Space)]);
    let (_, _, collapsed, _) = view_rect(&d);
    assert!(collapsed < width, "{name}: same-frame Tab/Space did not activate the pane header");
    d.close();
    let _ = std::fs::remove_dir_all(&dir);
}
