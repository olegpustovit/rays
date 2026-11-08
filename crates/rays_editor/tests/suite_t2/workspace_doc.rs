//! test_workspace_doc.ml, the Editor-driven parts: part_editor, part_conditional_history,
//! part_live, part_preset, part_pane_layout, part_editor_contexts, part_projection_reuse
//! (the pure Workspace_doc parts live in editor_document's own tests).
use crate::common;
use super::support::*;
use editor_document::preset;
use editor_document::settings;
use editor_document::workspace_doc as doc;
use flow_graph::flow_edit::label;
use flow_graph::flow_edit::{ArgKey, Loop, Op};
use flow_graph::projection;
use flow_syntax::syntax::{self as syn, Draft as Node};
use input::{Event, Key};
use rays_editor::Editor;

pub const CASES: &[(&str, fn())] = &[
    ("workspace_doc_part_editor", part_editor),
    ("workspace_doc_part_conditional_history", part_conditional_history),
    ("workspace_doc_part_live", part_live),
    ("workspace_doc_part_preset", part_preset),
    ("workspace_doc_part_pane_layout", part_pane_layout),
    ("workspace_doc_part_editor_contexts", part_editor_contexts),
    ("workspace_doc_part_projection_reuse", part_projection_reuse),
];

pub const SKIPPED: &[(&str, &str)] = &[("workspace_doc_part_files", "lowers every checked-in .rays (6-30 s each in the OCaml build, the sweep opens sketches whose lowering hangs): slow lowering: perf3")];

const STILL: &str = "; a small study
(workspace study
  (graph g :context sop [(n : int 3)]
    (let* [a (sop/uv_sphere :radius 0.5 :segments 8 :rings 4)
           ; moved right
           b (sop/transform a :translate [1 0 0])]
      b)))
";

const MOVING: &str = "(workspace moving
  (graph g :context sop
    (let* [a (sop/uv_sphere :radius 0.5 :segments 8 :rings 4)
           b (sop/transform a :translate [(sin t) 0 0])]
      b)))
";

fn fr(keys: &[Key], events: Vec<Event>, count: i64) -> rays::frame::t {
    frame_full(&[], keys, (0., 0.), (450., 320.), events, count)
}

fn ch(c: char) -> Event {
    Event::Key_pressed(Key::KeyChar(c))
}

fn editor(text: &str, presets: Option<&str>) -> Ed {
    create(&workspace(text), presets, None, false)
}

/// Update frames until `ok` holds: the editor awaits each frame's cook.
fn settle(mut e: Ed, from: i64, ok: &dyn Fn(&Ed) -> bool) -> (Ed, i64) {
    let mut count = from;
    loop {
        e = Editor::update(e, &fr(&[], vec![], count));
        if ok(&e) {
            return (e, count);
        }
        assert!(count <= from + 200, "the editor did not settle");
        count += 1;
    }
}

fn undo(e: Ed, c: i64) -> Ed {
    Editor::update(e, &fr(&[Key::Meta], vec![ch('z')], c))
}

fn first_x(g: &rdk::geometry::T) -> f64 {
    rdk::packed::Float3::View::view(rdk::geometry::positions(g)).unwrap().x[0]
}

fn objects(e: &Ed) -> Vec<(String, String)> {
    Editor::document(e).iter().map(|i| (i.label.clone(), i.kind.clone())).collect()
}

fn part_editor() {
    if !common::gpu() {
        return;
    }
    let e = editor(STILL, None);
    assert!(objects(&e).contains(&("g".to_owned(), "geometry".to_owned())), "one geometry object per sop graph");
    let (mut e, mut count) = settle(e, 0, &|e| Editor::prepared(e).is_some());
    let before = Editor::prepared(&e).unwrap();
    let (e2, c2) = settle(e, count, &|_| true);
    e = e2;
    count = c2;
    for _ in 0..20 {
        e = Editor::update(e, &fr(&[], vec![], count));
        count += 1;
    }
    assert!(rdk::geometry::data_id(&Editor::prepared(&e).unwrap()) == rdk::geometry::data_id(&before), "static nodes stay cached");
    assert!(Editor::undo_label(&e).is_none(), "no edit yet");
    let scrub = |v: &str| Op::SetArg { node: s(&["g", "a"]), key: ArgKey::Kw("radius".into()), sub: vec![], value: syn::make(Node::Num(v.into())) };
    e = Editor::edit(e, &scrub("0.6")).unwrap_or_else(|m| panic!("{m}"));
    e = Editor::edit(e, &scrub("0.7")).unwrap();
    assert_eq!(Editor::undo_label(&e).as_deref(), Some("Edit value"), "scrub label");
    assert!(doc::to_text(&Editor::workspace(&e)).unwrap().contains(":radius 0.7"), "scrub rewrote the source");
    e = Editor::edit(e, &Op::Rename { node: s(&["g", "b"]), to_: "moved".into() }).unwrap();
    assert_eq!(Editor::undo_label(&e).as_deref(), Some("Rename"), "rename label");
    e = Editor::edit(e, &Op::Wrap { nodes: vec![s(&["g", "a"])], loop_: Loop::For }).unwrap();
    assert_eq!(Editor::undo_label(&e).as_deref(), Some("Repeat"), "repeat label");
    assert!(Editor::edit(e.clone(), &Op::Rename { node: s(&["g", "a"]), to_: "moved".into() }).is_err(), "a refused edit is an error");
    assert_eq!(Editor::undo_label(&e).as_deref(), Some("Repeat"), "a refused edit records nothing");
    let (mut e, count) = settle(e, count + 1, &|e| Editor::prepared(e).map(|g| rdk::geometry::point_count(&g) > rdk::geometry::point_count(&before)).unwrap_or(false));
    e = undo(e, count);
    assert!(Editor::undo_label(&e).as_deref() == Some("Rename") && Editor::redo_label(&e).as_deref() == Some("Repeat"), "undo Repeat: undo={:?} redo={:?}", Editor::undo_label(&e), Editor::redo_label(&e));
    e = undo(e, count + 1);
    assert!(Editor::undo_label(&e).as_deref() == Some("Edit value") && Editor::redo_label(&e).as_deref() == Some("Rename"), "undo Rename");
    e = undo(e, count + 2);
    assert!(Editor::undo_label(&e).is_none() && Editor::redo_label(&e).as_deref() == Some("Edit value"), "one undo reverts the merged scrub");
    assert_eq!(doc::to_text(&Editor::workspace(&e)).unwrap(), doc::to_text(&workspace(STILL)).unwrap(), "undo restored the source");
    e = Editor::update(e, &fr(&[Key::Meta, Key::Shift], vec![ch('z')], count + 3));
    assert_eq!(Editor::undo_label(&e).as_deref(), Some("Edit value"), "redo");
    Editor::close(&e);
}

fn part_conditional_history() {
    if !common::gpu() {
        return;
    }
    let e = editor(
        "(workspace history
    (graph g :context value
      (let* [a (+ 1 2)
             b (if true (let* [u (+ a 1)] u) (+ a 2))
             c (cond true (+ a 3) false (+ a 4) :else (+ a 5))]
        (+ b c))))",
        None,
    );
    let before = doc::to_text(&Editor::workspace(&e)).unwrap();
    let ops = vec![
        Op::Wrap { nodes: vec![s(&["g", "a"])], loop_: Loop::If },
        Op::SetArg { node: s(&["g", "c"]), key: ArgKey::Pos(0), sub: vec![], value: syn::make(Node::Sym("false".into())) },
        Op::AddArm { node: s(&["g", "c"]), after: 0 },
        Op::DeleteArm { node: s(&["g", "c"]), index: 0 },
        Op::Unfold { node: s(&["g", "b"]), key: ArgKey::Arm(-1), sub: vec![] },
        Op::FoldInto { node: s(&["g", "b#then", "u"]) },
        Op::ToggleBypass { node: s(&["g", "b"]) },
    ];
    let mut e = e;
    for (i, op) in ops.iter().enumerate() {
        let changed = Editor::edit(e.clone(), op).unwrap_or_else(|m| panic!("{}: {m}", label(op)));
        assert_eq!(Editor::undo_label(&changed).as_deref(), Some(label(op)), "conditional gesture history label");
        let undone = undo(changed, i as i64 + 1);
        assert!(
            doc::to_text(&Editor::workspace(&undone)).unwrap() == before && Editor::undo_label(&undone).is_none() && Editor::redo_label(&undone).as_deref() == Some(label(op)),
            "conditional gesture was not one undo step: {}",
            label(op)
        );
        e = undone;
    }
    Editor::close(&e);
}

fn part_live() {
    if !common::gpu() {
        return;
    }
    let e = editor(MOVING, None);
    let (mut e, mut count) = settle(e, 0, &|e| Editor::prepared(e).is_some());
    let mut xs: Vec<f64> = Vec::new();
    let unique = |xs: &[f64]| {
        let mut v: Vec<u64> = xs.iter().map(|x| x.to_bits()).collect();
        v.sort();
        v.dedup();
        v.len()
    };
    while unique(&xs) < 4 && count < 400 {
        e = Editor::update(e, &fr(&[], vec![], count));
        count += 1;
        if let Some(g) = Editor::prepared(&e) {
            xs.push(first_x(&g));
        }
    }
    assert!(unique(&xs) >= 4, "a time-driven workspace animates in the editor");
    Editor::close(&e);
}

fn part_preset() {
    if !common::gpu() {
        return;
    }
    with_dir(|directory| {
        let e = editor(STILL, Some(directory));
        let (e, count) = settle(e, 0, &|e| Editor::prepared(e).is_some());
        let mut e = Editor::edit(e, &Op::SetNote { node: s(&["g", "a"]), text: "kept".into() }).unwrap();
        e = Editor::update(e, &fr(&[], vec![ch('/'), ch('s')], count + 1));
        e = Editor::update(e, &fr(&[], vec![Event::Key_pressed(Key::Enter)], count + 2));
        let listed = preset::list(directory);
        assert!(listed.len() == 1, "/ s did not save one preset");
        let name = listed[0].0.clone();
        let path = preset::path(directory, &name);
        assert!(path.ends_with(".rays") && std::path::Path::new(&path).exists(), "/ s saved a .rays");
        let text = std::fs::read_to_string(&path).unwrap();
        assert!(text.contains("(workspace study") && text.contains("; kept") && text.contains("(view"), "the preset is s-expressions with its comments");
        assert!(!text.contains("{\"") && !text.contains("\"version\""), "no JSON");
        let loaded = preset::load(None, &path, None, &settings::none()).unwrap();
        assert_eq!(doc::to_text(&loaded.doc.workspace.ws).unwrap(), doc::to_text(&Editor::workspace(&e)).unwrap(), "save then load round trip");
        assert_eq!(preset::list(directory).into_iter().map(|(n, _)| n).collect::<Vec<_>>(), vec![name.clone()], "listed");
        // load through / b: one undo entry
        e = Editor::edit(e, &Op::Rename { node: s(&["g", "a"]), to_: "ball".into() }).unwrap();
        e = Editor::update(e, &fr(&[], vec![ch('/'), ch('b')], count + 3));
        e = Editor::update(e, &fr(&[], vec![Event::Text_input(name.clone()), Event::Key_pressed(Key::Enter)], count + 4));
        e = Editor::update(e, &fr(&[], vec![], count + 5));
        assert!(doc::to_text(&Editor::workspace(&e)).unwrap().contains("(sop/transform a "), "/ b restored the saved source");
        assert_eq!(Editor::undo_label(&e).as_deref(), Some("Load preset"), "load is one undo entry");
        // errors never touch the document
        let corrupt = preset::path(directory, "corrupt");
        std::fs::write(&corrupt, "(workspace").unwrap();
        assert!(preset::load(None, &corrupt, None, &settings::none()).is_err(), "corrupt text is an error");
        let mut builder = syn::SyntaxBuilder::new();
        let fields = [Node::Kw("zoom".into()), Node::Num("inf".into())].into_iter()
            .map(|draft| builder.push_draft(0, draft, Default::default(), None)).collect();
        let id = builder.push_draft(0, Node::Map(fields), Default::default(), None);
        let nonfinite = builder.freeze().handle(id);
        assert!(preset::save(directory, "x", &loaded.doc, &nonfinite).is_err(), "a nonfinite view is refused");
        assert!(preset::save(directory, "", &loaded.doc, &syn::make(Node::Map(vec![].into()))).is_err(), "an empty name is refused");
        Editor::close(&e);
    });
}

fn part_pane_layout() {
    let d = workspace(STILL);
    let opened = build(&d);
    let mut ws = opened.workspace.ws.clone();
    ws.layout.at.insert(s(&["g", "a"]), (40., 60.));
    ws.layout.collapsed.insert(s(&["g", "b"]), true);
    ws.layout.level.insert(s(&["g", "gone"]), projection::Level::Full);
    ws.layout.pinned.insert(s(&["nograph", "a"]), true);
    let again = workspace(&doc::to_text(&ws).unwrap());
    assert!(again.layout.at[&s(&["g", "a"])] == (40., 60.) && again.layout.collapsed.contains_key(&s(&["g", "b"])), "moved items and collapsed zones round-trip through the s-expression");
    assert!(again.layout.level.is_empty() && again.layout.pinned.is_empty(), "stale layout keys were kept");
    let two = workspace("(workspace w (graph a :context sop (sop/box)) (graph b :context sop (let* [x (sop/box)] x)))\n(layout (node [\"b\" \"x\"] :at [10 20]))");
    assert!(two.layout.at.contains_key(&s(&["b", "x"])), "a live key was dropped");
    let removed = doc::edit(&catalog(), &two, &Op::RemoveGraph { name: "b".into() }).unwrap();
    assert!(editor_document::layout_by_path::is_empty(&removed.layout), "the layout of a removed graph was kept");
    assert!(doc::of_text(&Default::default(), &catalog(), &doc::to_text(&removed).unwrap()).is_ok(), "the text after removing a graph does not load");
    let noted = workspace(
        "(workspace w (graph a :context sop (let* [x (sop/box)] x)))\n; about the view\n(view {:eye [1 2 3]})\n; where things are\n(layout (node [\"a\" \"x\"] :at [10 20]))\n; the end\n",
    );
    let out = doc::to_text(&noted).unwrap();
    assert!(out.contains("; about the view") && out.contains("(view {:eye [1 2 3]})") && out.contains("; where things are") && out.contains("; the end"), "root forms and their comments: {out}");
    assert_eq!(doc::to_text(&workspace(&out)).unwrap(), out, "to_text with root comments is a fixed point");
    match doc::of_text(&Default::default(), &catalog(), "(workspace w (graph a :context sop (sop/box)))\n(layout (nonsense))") {
        Err(ds) if ds.len() == 1 && ds[0].code == "E_LAYOUT" && ds[0].span.as_ref().map(|s| s.start > 0).unwrap_or(false) => {}
        other => panic!("E_LAYOUT has no span: {:?}", other.err().map(|d| diag_text(&d))),
    }
}

fn bloom_fixture() -> String {
    let path = format!("{}/../../specification/workspace/cases/bloom.lisp", env!("CARGO_MANIFEST_DIR"));
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path}: {e}"))
}

fn part_editor_contexts() {
    if !common::gpu() {
        return;
    }
    let mut e = editor(&bloom_fixture(), None);
    let mut count = 0;
    for _ in 0..3 {
        count += 1;
        e = Editor::update(e, &fr(&[], vec![], count));
    }
    let dump = |e: &Ed| {
        with_dir(|dir| {
            Editor::crash_dump(e, dir).unwrap();
            std::fs::read_to_string(format!("{dir}/editor.txt")).unwrap()
        })
    };
    let objs = objects(&e);
    assert!(objs.contains(&("accent".into(), "geometry".into())) && objs.contains(&("Bloom study".into(), "world".into())), "the editor opens the workspace's objects and World");
    assert!(objs.iter().filter(|(_, op)| op == "camera").count() == 1 && objs.iter().filter(|(_, op)| op == "light").count() == 1, "a scene that declares a camera and a light gets no seeded ones");
    assert!(dump(&e).contains("pane graph: scene\n"), "the pane shows the scene graph at the scene level until an outline row or a named panel picks another");
    let mut builder = syn::SyntaxBuilder::new();
    let fields = ["1", "1", "1"].into_iter().map(|n| builder.push_draft(0, Node::Num(n.into()), Default::default(), None)).collect();
    let id = builder.push_draft(0, Node::Vec(fields), Default::default(), None);
    let vec3 = builder.freeze().handle(id);
    let e2 = Editor::edit(e.clone(), &Op::SetArg { node: s(&["scene", "accent"]), key: ArgKey::Kw("scale".into()), sub: vec![], value: vec3 }).unwrap();
    assert_eq!(Editor::undo_label(&e2).as_deref(), Some("Edit value"), "one history entry");
    assert!(
        Editor::document(&e2).iter().any(|i| i.label == "accent" && editor_document::objects::geometry(i).map(|g| g.scale_x == 1.).unwrap_or(false)),
        "the scene edit reached the object"
    );
    Editor::close(&e);
}

fn part_projection_reuse() {
    let text = r#"(workspace w
    (graph g :context sop [(count : int 3)] (let* [a (sop/box)] a))
    (graph other :context sop (let* [b (sop/points :points 1)] b)))
    (layout (node ["g" "a"] :at [120 48])
            (node ["g" ":count"] :at [0 0])
            (node ["other" "b"] :at [240 96]))"#;
    let ws = workspace(text);
    let sample = |f: &mut dyn FnMut()| {
        let t0 = std::time::Instant::now();
        let guard = flow_syntax::phase_timer::SampleGuard::start_with_clock(Box::new(move || t0.elapsed().as_secs_f64()));
        f();
        guard.finish()
    };
    let mut renamed = None;
    let phases = sample(&mut || renamed = Some(doc::edit(&catalog(), &ws, &Op::Rename { node: s(&["other", "b"]), to_: "moved".into() }).unwrap()));
    let renamed = renamed.unwrap();
    assert_eq!(flow_syntax::phase_timer::calls(&phases, "project"), 0, "a rename projected every graph to prune layout");
    assert!(projection::same_graph(&ws.checked, &renamed.checked, "g"), "a rename elsewhere invalidated an untouched graph");
    assert!(!projection::same_graph(&ws.checked, &renamed.checked, "other"), "projection invalidation ignored lexical graph content");
    assert!(
        renamed.layout.at[&s(&["g", "a"])] == (120., 48.) && renamed.layout.at.contains_key(&s(&["g", ":count"])) && renamed.layout.at[&s(&["other", "moved"])] == (240., 96.) && !renamed.layout.at.contains_key(&s(&["other", "b"])),
        "path pruning lost a surviving key or kept the renamed one"
    );
    let mut removed = None;
    let phases = sample(&mut || removed = Some(doc::edit(&catalog(), &renamed, &Op::RemoveGraph { name: "other".into() }).unwrap()));
    let removed = removed.unwrap();
    assert!(flow_syntax::phase_timer::calls(&phases, "project") == 0 && !removed.layout.at.contains_key(&s(&["other", "moved"])), "removing a graph projected or retained its layout");
    let typed = workspace("(workspace w (defn a :context value [] 1) (graph b :context value (let* [result (a)] 1)))");
    let changed = workspace("(workspace w (defn a :context value [] [1 2 3]) (graph b :context value (let* [result (a)] 1)))");
    assert!(!projection::same_graph(&typed.checked, &changed.checked, "b"), "projection reuse hid a changed dependency type");
    if !common::gpu() {
        return;
    }
    let e = editor(text, None);
    let e = Editor::update(e, &fr(&[], vec![], 0));
    let mut after = None;
    let phases = sample(&mut || {
        let e2 = Editor::edit(e.clone(), &Op::Rename { node: s(&["other", "b"]), to_: "moved".into() }).unwrap();
        after = Some(Editor::update(e2, &fr(&[], vec![], 1)));
    });
    assert_eq!(flow_syntax::phase_timer::calls(&phases, "project"), 0, "editing another graph re-projected the pane");
    Editor::close(&e);
    if let Some(a) = after {
        Editor::close(&a);
    }
}
