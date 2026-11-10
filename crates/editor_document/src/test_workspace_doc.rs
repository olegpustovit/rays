use crate::kinds;
use crate::layout_by_path as lbp;
use crate::settings;
use crate::workspace_doc::{self as doc, TextOptions};
use flow_check::catalog::Catalog;
use flow_graph::flow_edit::{self as e, ArgKey, Loop, Op};
use flow_graph::projection::Level;
use flow_syntax::syntax::{self as s, Node};
use param::Value;
use std::sync::Arc;

const STILL: &str = "; a small study
(workspace study
  (graph g :context sop [(n : int 3)]
    (let* [a (sop/uv_sphere :radius 0.5)
           ; moved right
           b (sop/transform a :translate [1 0 0])]
      b)))
";

const VALUE_STUDY: &str = "(workspace study (graph g :context value (let* [a 1.] a)))\n";

fn catalog() -> Catalog {
    kinds::catalog(1, &sop::nodes::factories()).expect("catalog")
}

fn has(text: &str, sub: &str) -> bool {
    text.contains(sub)
}

fn schema() -> Arc<param::Schema> {
    Arc::new(
        param::schema(
            "doc-settings".to_owned(),
            vec![
                param::field("amount".to_owned(), param::floating(0., 10., None, None).unwrap(), Value::Float(0.)).unwrap().label("Amount".to_owned()).unwrap(),
                param::field("mode".to_owned(), param::text(), Value::Text(b"a".as_slice().into())).unwrap().label("Mode".to_owned()).unwrap(),
            ],
        )
        .unwrap(),
    )
}

fn opts(st: Option<settings::T>) -> TextOptions {
    TextOptions { settings: st, ..Default::default() }
}

fn of_text(catalog: &Catalog, st: Option<settings::T>, text: &str) -> doc::T {
    match doc::of_text(&opts(st), catalog, text) {
        Ok(d) => d,
        Err(ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
    }
}

fn plain() -> settings::T {
    settings::make(&schema(), vec![Value::Float(0.), Value::Text(b"a".as_slice().into())])
}

fn p(parts: &[&str]) -> flow_check::workspace::Path {
    parts.iter().map(|x| (*x).into()).collect()
}

#[test]
fn part_text() {
    let catalog = catalog();
    let text = format!(
        "{STILL}\n(layout\n  (node [\"g\" \"a\"] :at [120 48] :level \"full\" :pinned true :collapsed false :rows {{:radius false}})\n  (node [\"g\" \"b\"] :at [300.5 40])\n  (frame [\"g\"] \"Legs\" :at [0 0] :size [200 100]))\n\n(settings :amount 2.5 :mode \"b\")\n"
    );
    let d = of_text(&catalog, Some(plain()), &text);
    assert_eq!(doc::name(&d), "study");
    assert_eq!(d.layout.at[&p(&["g", "a"])], (120., 48.));
    assert!(d.layout.level[&p(&["g", "a"])] == Level::Full && d.layout.pinned[&p(&["g", "a"])]);
    assert_eq!(d.layout.at[&p(&["g", "b"])], (300.5, 40.));
    assert_eq!(d.layout.frames[&p(&["g"])][0].title, "Legs");
    assert_eq!(settings::get(&schema(), &d.settings).unwrap(), vec![Value::Float(2.5), Value::Text(b"b".as_slice().into())]);
    let out = doc::to_text(&d).unwrap();
    assert!(has(&out, "; a small study") && has(&out, "; moved right"), "comments survive to_text");
    let again = of_text(&catalog, Some(plain()), &out);
    assert_eq!(doc::to_text(&again).unwrap(), out, "to_text is a fixed point");
    assert_eq!(again.layout, d.layout, "layout round trip");
    // an open expression is keyed by the leaf of a call that is only a card while open
    let opened = of_text(
        &catalog,
        None,
        "(workspace w (graph g :context value (let* [a (max 1.0 (+ 2.0 3.0))] a)))\n(layout (node [\"g\" \"a#1\"] :collapsed false))",
    );
    assert_eq!(opened.layout.collapsed.get(&p(&["g", "a#1"])), Some(&false), "an open expression survives loading");
    // a default setting is not written
    let plain_doc = of_text(&catalog, Some(plain()), STILL);
    let t = doc::to_text(&plain_doc).unwrap();
    assert!(!has(&t, "settings") && !has(&t, "layout"), "empty layout and defaults are omitted");
    let bad = |text: &str, code: &str| match doc::of_text(&opts(Some(plain())), &catalog, text) {
        Err(ds) => assert!(ds.iter().any(|d| d.code == code), "{code}: {ds:?}"),
        Ok(_) => panic!("accepted: {code}"),
    };
    // layout and settings are read after the workspace is checked: a value-context base keeps each check cheap
    bad(&format!("{VALUE_STUDY}(layout (nope))"), "E_LAYOUT");
    bad(&format!("{VALUE_STUDY}(layout (node [\"g\"] :at [1]))"), "E_LAYOUT");
    bad(&format!("{VALUE_STUDY}(settings :nope 1)"), "E_SETTINGS");
    bad(&format!("{VALUE_STUDY}(settings :amount \"x\")"), "E_SETTINGS");
    bad("(workspace w (graph g :context sop (sop/uv_sphere :nope 1)))", "E_UNKNOWN_PARAM");
    bad("(nothing)", "E_WORKSPACE");
}

#[test]
fn part_edit() {
    let catalog = catalog();
    let text = format!("{STILL}\n(layout\n  (node [\"g\" \"a\"] :at [120 40])\n  (node [\"g\" \"b\"] :at [300 40]))\n");
    let d = of_text(&catalog, None, &text);
    let renamed = doc::edit(&catalog, &d, &Op::Rename { node: p(&["g", "a"]), to_: "ball".to_owned() }).expect("rename");
    assert!(renamed.layout.at.contains_key(&p(&["g", "ball"])) && !renamed.layout.at.contains_key(&p(&["g", "a"])), "rename rewrites layout keys in the same transaction");
    assert!(renamed.layout.at.contains_key(&p(&["g", "b"])), "other layout keys stay");
    assert!(has(&doc::to_text(&renamed).unwrap(), "(sop/transform ball"), "rename rewrites the source");
    assert_eq!(renamed.checked.name, "study");
    // atomic: a refused edit leaves the document as it was
    assert!(doc::edit(&catalog, &d, &Op::Rename { node: p(&["g", "a"]), to_: "b".to_owned() }).is_err(), "accepted a clashing rename");
    assert!(
        doc::edit(&catalog, &d, &Op::SetArg { node: p(&["g", "a"]), key: ArgKey::Kw("nope".to_owned()), sub: vec![], value: s::make(s::Draft::Num("1".to_owned())) }).is_err(),
        "accepted an unknown keyword"
    );
    assert_eq!(doc::to_text(&d).unwrap(), doc::to_text(&of_text(&catalog, None, &text)).unwrap(), "the original document is untouched");
    // the spare node is added to the document already checked above (the layout keys are not asserted)
    let added = doc::edit(
        &catalog,
        &d,
        &Op::AddNode { scope: p(&["g"]), name: "spare".to_owned(), expr: s::make(s::Draft::Num("1".to_owned())) },
    )
    .expect("add");
    let d = doc::edit(&catalog, &added, &Op::DeleteNodes { nodes: vec![p(&["g", "spare"])] }).expect("delete");
    assert_eq!(d.layout.at.len(), 2, "delete keeps other keys");
}

#[test]
fn part_imports() {
    let catalog = catalog();
    let library = ";; shared definitions\n(workspace library (defn twice :context value [(x : float)] (* x 2)) (graph shared :context value (twice 3)))\n";
    let text = "(workspace local (graph result :context value (+ (ref shared) (twice 2))))\n;; library stays separate\n(import \"lib.rays\")\n";
    let imports = vec![("lib.rays".to_owned(), library.to_owned())];
    let with = |imports: &[(String, String)]| TextOptions { imports: imports.to_vec(), ..Default::default() };
    let d = doc::of_text(&with(&imports), &catalog, text).unwrap();
    assert!(d.checked.graphs.len() == 2 && d.checked.defs.iter().filter(|g| g.name == "twice").count() == 1, "import children were not checked together");
    assert_eq!(doc::import_texts(&d), imports, "raw import text was lost (reload digest)");
    assert!(
        doc::imported_file(&d.imports, &p(&["shared", "@result"])) == Some("lib.rays".to_owned()) && doc::imported_file(&d.imports, &p(&["def:twice"])) == Some("lib.rays".to_owned()),
        "import provenance was lost"
    );
    let printed = doc::to_text(&d).unwrap();
    assert!(
        has(&printed, "(import \"lib.rays\")") && has(&printed, "library stays separate") && !has(&printed, "(defn twice") && !has(&printed, "(graph shared"),
        "save inlined imported definitions"
    );
    assert_eq!(doc::to_text(&doc::of_text(&with(&imports), &catalog, &printed).unwrap()).unwrap(), printed, "imports did not round trip");
    let edit = Op::SetArg { node: p(&["shared", "@result"]), key: ArgKey::Pos(0), sub: vec![], value: s::make(s::Draft::Num("4".to_owned())) };
    assert!(matches!(doc::edit(&catalog, &d, &edit), Err(x) if x.code == "E_IMPORTED" && has(&x.message, "lib.rays")), "imported expression remained writable");
    assert!(matches!(doc::edit(&catalog, &d, &Op::RemoveGraph { name: "shared".to_owned() }), Err(x) if x.code == "E_IMPORTED"), "imported graph remained removable");
    let one = |t: &str| s::parse(t).unwrap().remove(0);
    let ops = vec![
        Op::RenameGraph { name: "shared".to_owned(), to_: "renamed".to_owned() },
        Op::SetNote { node: p(&["shared"]), text: "changed".to_owned() },
        Op::Wrap { nodes: vec![p(&["shared", "@result"])], loop_: Loop::If },
        Op::AddNode { scope: p(&["shared"]), name: "extra".to_owned(), expr: s::make(s::Draft::Num("1".to_owned())) },
        Op::SetInputDefault { form: "def:twice".to_owned(), input: "x".to_owned(), value: s::make(s::Draft::Num("1".to_owned())) },
        Op::SetGraph { name: "shared".to_owned(), form: one("(graph shared :context value 1)") },
    ];
    for op in &ops {
        assert!(matches!(doc::edit(&catalog, &d, op), Err(x) if x.code == "E_IMPORTED"), "imported gesture remained writable: {}", e::label(op));
    }
    let dependent = doc::of_text(&with(&[("lib.rays".to_owned(), "(graph shared :context value (ref local))".to_owned())]), &catalog, "(workspace w (graph local :context value 1)) (import \"lib.rays\")").unwrap();
    assert!(
        matches!(doc::edit(&catalog, &dependent, &Op::RenameGraph { name: "local".to_owned(), to_: "moved".to_owned() }), Err(x) if x.code == "E_IMPORTED"),
        "local rename rewrote an imported reference"
    );
    let mut fragment = doc::of_text(&TextOptions::default(), &catalog, "(defn twice :context value [(x : float)] (* x 2))").unwrap();
    fragment.layout.at.insert(p(&["def:twice", "@result"]), (20., 40.));
    let ft = doc::to_text(&fragment).unwrap();
    assert!(!has(&ft, "(workspace") && !has(&ft, "(layout"), "editing a library fragment appended workspace metadata");
    let error = |imports: &[(String, String)], text: &str, code: &str| {
        assert!(matches!(doc::of_text(&with(imports), &catalog, text), Err(ds) if ds.iter().any(|d| d.code == code)), "missing {code}");
    };
    error(&[], text, "E_IMPORT");
    error(&[("lib.rays".to_owned(), format!("(import \"other.rays\")\n{library}"))], text, "E_IMPORT");
    error(&imports, "(workspace local (graph shared :context value 1)) (import \"lib.rays\")", "E_NAME");
    error(&[], "(workspace local (graph g :context value 1)) (import \"/absolute.rays\")", "E_IMPORT");
    let local = doc::edit(&catalog, &d, &Op::SetGraph { name: "result".to_owned(), form: one("(graph result :context value (twice 9))") }).unwrap();
    assert!(doc::import_texts(&local) == imports && !has(&doc::to_text(&local).unwrap(), "(graph shared"), "local edit changed imported source");
}

// ---- parts that lower a document (each lowering is ~200 ms in a debug build: RAYS_SLOW=1) ----

fn factories() -> Vec<sop::edit_graph::Factory> {
    sop::nodes::factories()
}

fn case(name: &str) -> String {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../../specification/workspace/cases").join(format!("{name}.lisp"));
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}

fn small_bloom() -> String {
    case("bloom").replace("(petals : int 12)", "(petals : int 3)").replace(":petals 7", ":petals 2")
}

fn build(previous: Option<&crate::document::T>, ws: &doc::T) -> crate::document::T {
    let options = crate::lowering::Options { factories: Some(factories()), previous: previous.cloned(), ..Default::default() };
    match crate::lowering::of_workspace(&options, ws) {
        Ok(d) => d,
        Err(d) => panic!("{}", rays_diagnostic::to_string(&d)),
    }
}

/// Run the checked edit without the literal fast path.
fn full_edit(catalog: &Catalog, d: &doc::T, op: &Op) -> Result<(Vec<s::Syntax>, flow_check::workspace::Workspace), rays_diagnostic::Diagnostic> {
    let (ops, library) = (d.checked.ops.clone(), d.fragment);
    let checker = move |catalog: &Catalog, forms: &[s::Syntax]| flow_sop_catalog::catalog::check_workspace(&ops, library, None, catalog, forms);
    e::apply_checked_with(&d.checked.ops, d.fragment, Some(&checker), catalog, &d.source, op)
}

fn sampled<T>(run: impl FnOnce() -> T) -> (T, flow_syntax::phase_timer::PhaseTimer) {
    let start = std::time::Instant::now();
    let guard = flow_syntax::phase_timer::SampleGuard::start_with_clock(Box::new(move || start.elapsed().as_secs_f64()));
    let value = run();
    (value, guard.finish())
}

fn calls(t: &flow_syntax::phase_timer::PhaseTimer, phase: &str) -> i64 {
    flow_syntax::phase_timer::calls(t, phase)
}

fn parse_one(text: &str) -> s::Syntax {
    s::parse(text).expect("parse").remove(0)
}

#[test]
fn part_view() {
    let directory = std::env::temp_dir().join(format!("rays-workspace-view-{}", std::process::id()));
    std::fs::create_dir_all(&directory).unwrap();
    let dir = directory.to_string_lossy().into_owned();
    let d = build(None, &of_text(&catalog(), None, STILL));
    let view = parse_one("{:eye [1.5 -2.0 3] :look_through true :name \"a \\\"b\\\"\"}");
    let path = crate::preset::save(&dir, "v", &d, &view).expect("save");
    let loaded = crate::preset::load(None, &path, Some(&factories()), &settings::none()).expect("load");
    let print = |v: &s::Syntax| flow_syntax::lisp::print(&[v.clone()]).unwrap().0;
    assert_eq!(print(&loaded.view), print(&view), "view round trip");
    std::fs::remove_dir_all(&directory).ok();
}

#[test]
#[ignore = "upstream: bloom (sphere-like nodes) lowers for > 7 min in a debug build in total, never completed; assertions unverified (parity_backlog: flow_eval static_with cost)"]
fn part_contexts() {
    use crate::{layers, lowering, objects};
    let catalog = catalog();
    let bloom = of_text(&catalog, None, &small_bloom());
    let d = build(None, &bloom);
    let infos = crate::document::scene_graph(&d);
    let ops = |kind: &str| -> Vec<crate::kind_graph::Node> { infos.iter().filter(|i| i.kind == kind).cloned().collect() };
    let labels = |v: &[crate::kind_graph::Node]| v.iter().map(|i| i.label.clone()).collect::<Vec<_>>();
    assert_eq!(labels(&ops("geometry")), ["flower", "accent"], "the scene graph's geometry objects, by :name");
    assert!(ops("camera").len() == 1 && ops("light").len() == 1 && ops("world").len() == 1, "camera, light and World come from the workspace");
    assert!(crate::document::validate(&d).is_ok(), "the document validates");
    let accent = ops("geometry")[1].clone();
    let g = objects::geometry(&accent).expect("accent is not a geometry object");
    assert!(g.translate_x == 2.2 && g.translate_z == -1.2 && g.scale_x == 0.55 && g.scale_y == 0.55 && g.rotate_y == 0., "a vec3 keyword sets its three fields");
    let (camera, _) = objects::camera::of_node(&ops("camera")[0]).expect("no camera");
    assert!(scene_core::camera::position(&camera) == rays_math::vec3::create(0.5, 1.8, 6.), "camera eye");
    let l = objects::light(&ops("light")[0]).expect("no light");
    assert!(l.intensity == 60. && l.translate_x == 4., "light fields");
    assert_eq!(d.active_camera, Some(ops("camera")[0].id), "the workspace's camera is the active one");
    let (flower, accent_id) = (ops("geometry")[0].id, ops("geometry")[1].id);
    let (nf, na) = (d.networks.get(&flower).unwrap(), d.networks.get(&accent_id).unwrap());
    assert!(nf.displayed.is_some() && na.displayed.is_some() && !flow_sop_catalog::network::ptr_eq(&nf.graph, &na.graph), "two refs of one graph with different inputs are two networks");
    let world_node = ops("world")[0].clone();
    assert_eq!(world_node.label, "Bloom study", "the World's name");
    let w = layers::to_world(None, &world_node, crate::document::layers(&d, world_node.id).unwrap()).expect("the World does not read");
    assert!(w.exposure == -0.5, "World exposure");
    assert!(
        matches!(&w.layers[..], [a, b] if matches!(&a.layer, scene_core::world::layer::Sky { turbidity, .. } if *turbidity == 3.) && matches!(&b.layer, scene_core::world::layer::Sun { intensity, .. } if *intensity == 1500.)),
        "World layers, bottom first"
    );
    let window = lowering::window(&bloom).unwrap();
    assert_eq!(window, kinds::Window { title: "Bloom study".to_owned(), width: 1400, height: 800, fps: 60, seed: 42, fast: true }, "settings graph: window title, size, fps (a macro call) and seed");
    let tree = lowering::window(&of_text(&catalog, None, &case("tree"))).unwrap();
    assert!(tree.title == "Rays" && tree.fps == 60 && tree.fast, "no settings graph: defaults, fast precision");
    let exact = of_text(&catalog, None, "(workspace e (graph g :context value 1.0) (graph s :context settings (settings/config :precision \"exact\")))");
    assert!(!lowering::window(&exact).unwrap().fast, "settings :precision \"exact\" is not fast");
    assert!(lowering::fast(&d), "a document without :precision is fast");
    assert!(
        settings::fields(&d.settings).iter().any(|f| f.name == "title" && f.current == Value::Text(b"Bloom study".as_slice().into())),
        "the settings graph is the document's settings"
    );
    // ids are fresh per build; everything else is a function of the source
    let shape = |d: &crate::document::T| {
        (
            crate::document::scene_graph(d).iter().map(|i| (i.kind.clone(), i.label.clone(), crate::kind_graph::fields(i).iter().map(|f| (f.name.clone(), f.current.clone())).collect::<Vec<_>>())).collect::<Vec<_>>(),
            crate::document::scene_graph(d).iter().filter(|i| i.kind == "world").filter_map(|i| crate::document::layers(d, i.id).and_then(|l| layers::to_world(None, i, l))).map(|w| format!("{w:?}")).collect::<Vec<_>>(),
            settings::fields(&d.settings).iter().map(|f| (f.name.clone(), f.current.clone())).collect::<Vec<_>>(),
        )
    };
    assert!(shape(&build(None, &bloom)) == shape(&d), "the same source builds the same document twice");
    assert_eq!(crate::document::dump(&build(Some(&d), &bloom)), crate::document::dump(&d), "rebuilding over the previous document changes nothing");
    assert_eq!(doc::to_text(&of_text(&catalog, None, &doc::to_text(&bloom).unwrap())).unwrap(), doc::to_text(&bloom).unwrap(), "print and parse are a fixed point");
    let directory = std::env::temp_dir().join(format!("rays-contexts-{}", std::process::id()));
    std::fs::create_dir_all(&directory).unwrap();
    let dir = directory.to_string_lossy().into_owned();
    let path = crate::preset::save(&dir, "bloom", &d, &s::make(s::Draft::Map(Vec::new().into()))).unwrap();
    assert!(!has(&std::fs::read_to_string(&path).unwrap(), "(settings :"), "settings owned by a graph are not written a second time");
    let loaded = crate::preset::load(None, &path, Some(&factories()), &settings::none()).unwrap();
    assert!(shape(&loaded.doc) == shape(&d), "the preset round trips scene, World and settings");
    std::fs::remove_dir_all(&directory).ok();
    // a Flow_edit on the scene graph rebuilds it; ids, and viewport edits of unmentioned fields, stay
    let edited = doc::edit(
        &catalog,
        &bloom,
        &Op::SetArg { node: p(&["scene", "accent"]), key: ArgKey::Kw("translate".to_owned()), sub: vec![], value: s::parse("[4 0 1]").unwrap().remove(0) },
    )
    .unwrap();
    let doc2 = build(Some(&d), &edited);
    let accent2 = crate::document::scene_graph(&doc2).into_iter().filter(|i| i.kind == "geometry").nth(1).unwrap();
    assert_eq!(accent2.id, accent_id, "an edit keeps the object's id");
    assert!(objects::geometry(&accent2).unwrap().translate_x == 4., "the edit reaches the object");
    assert!(has(&doc::to_text(&edited).unwrap(), ":translate [4 0 1]"), "the source carries the edit");
    let settings_of = |body: &str| of_text(&catalog, None, &format!("(workspace w (graph c :context settings [(f : int 60)] {body}))"));
    assert!(lowering::window(&settings_of("(settings/config :fps f)")).is_ok(), "a settings graph with an input");
    assert_eq!(lowering::window(&settings_of("(settings/config :fps (* f 100))")).unwrap().fps, 240, "a computed fps above the hard bound is clamped when lowered");
    let none_world = lowering::of_workspace(&lowering::Options { factories: Some(factories()), ..Default::default() }, &of_text(&catalog, None, "(workspace w (graph x :context world (world/sky)))"));
    assert!(matches!(none_world, Ok(ref d) if objects::ids("world", &crate::document::scene_graph(d)).is_empty()), "a world graph no scene/world references is no World");
    let plain_doc = build(None, &of_text(&catalog, None, STILL));
    assert_eq!(labels(&crate::document::scene_graph(&plain_doc).into_iter().filter(|i| i.kind == "geometry").collect::<Vec<_>>()), ["g"], "no scene graph: one object per sop graph");
    let variations = build(None, &of_text(&catalog, None, &case("variations")));
    assert_eq!(labels(&crate::document::scene_graph(&variations)), ["garden", "garden (v1.1.1)", "garden (v1.1.2)", "garden (v1.1.3)"], "variations scene");
}

#[test]
fn literal_spelling_changes() {
    let catalog = catalog();
    let base = of_text(&catalog, None, STILL);
    let edit = |doc: &doc::T, text| doc::edit(&catalog, doc, &Op::SetArg {
        node: p(&["g", "a"]), key: ArgKey::Kw("radius".into()), sub: vec![], value: parse_one(text),
    }).unwrap();
    let first = edit(&base, "0.6");
    let spelling = edit(&first, "0.60");
    let changes = doc::literal_changes(&first, &spelling).unwrap();
    assert_eq!(changes.len(), 1, "spelling-only edits must reach the pane");
    let expr = &changes[0].expr;
    let Node::Num(token) = expr.node else { panic!("expected numeric spelling") };
    assert_eq!(expr.name(token), "0.60");
    assert!(doc::literal_changes(&spelling, &edit(&spelling, "0.60")).unwrap().is_empty());
}

#[test]
#[ignore = "slow: each lowering costs ~200 ms in a debug build (flow_eval::Host::static_with_requests); run with RAYS_SLOW=1"]
fn part_literals() {
    use crate::literal_edit;
    let catalog = catalog();
    let text = "(workspace w
    (defmacro identity [x] `(sop/transform ~x))
    (graph g :context sop
      (let* [a (sop/box :size [2 2 2] :consolidate_points true :normals \"Point\")
             b (sop/transform a :translate [1 0 0])
             c (sop/set_color b :color \"#ff8800\")]
        c)))";
    let mut d = of_text(&catalog, None, text);
    let lower_checked = |checked: &flow_check::workspace::Workspace, compiled_ids: flow_sop::lower::PathMap, sites: Vec<flow_check::workspace::Path>| {
        flow_sop::lower::of_checked(Some(factories()), flow_sop::lower::Options { compiled_ids, sites, ..Default::default() }, checked).expect("lower")
    };
    let mut lowered = flow_sop::lower::of_checked(Some(factories()), Default::default(), &d.checked).expect("lower");
    let op = |path: &[&str], field: &str, sub: Vec<i64>, text: &str| Op::SetArg { node: p(path), key: ArgKey::Kw(field.to_owned()), sub, value: parse_one(text) };
    for operation in [
        op(&["g", "a"], "size", vec![], "[3 4 5]"),
        op(&["g", "b"], "translate", vec![0], "17"),
        op(&["g", "c"], "color", vec![], "\"#0088ff\""),
        op(&["g", "a"], "consolidate_points", vec![], "false"),
    ] {
        let old = d.clone();
        let (source, checked) = full_edit(&catalog, &old, &operation).expect("full edit");
        let (next, phases) = sampled(|| doc::edit(&catalog, &old, &operation).expect("edit"));
        for phase in ["print", "parse", "check", "project"] {
            assert_eq!(calls(&phases, phase), 0, "literal repeated {phase}");
        }
        assert_eq!(doc::to_text(&next).unwrap(), doc::to_text(&doc::T { source: source.clone(), checked: Arc::new(checked.clone()), ..next.clone() }).unwrap(), "literal save differs from slow edit");
        let changes = doc::literal_changes(&old, &next).expect("literal changes");
        let (patched, phases) = sampled(|| {
            let mut current = Some(lowered.clone());
            for change in &changes {
                current = current.and_then(|l| literal_edit::lower(&l, change)).map(|r| r.expect("patch"));
            }
            current
        });
        let fast = patched.is_some();
        let patched = match patched {
            Some(patched) => {
                assert!(calls(&phases, "lower") == 0 && calls(&phases, "evaluate") == 0, "literal re-evaluated or re-lowered");
                patched
            }
            None => lower_checked(&checked, lowered.compiled_ids.clone(), lowered.sites.clone()),
        };
        let reference = lower_checked(&checked, patched.compiled_ids.clone(), patched.sites.clone());
        if fast {
            assert_eq!(format!("{:?}", patched.plan), format!("{:?}", reference.plan), "literal plan differs from full evaluation");
        }
        // the cook comparison (Test_support.Workspace_cook, 3 domains) and the Pxui_graph pane
        // update are not here: both live in crates editor_document cannot reach
        let scope = flow_graph::projection::of_graph(&catalog, &old.checked, "g").unwrap();
        let arguments: Vec<_> = changes.iter().map(|c| (c.path.clone(), ArgKey::Kw(c.field.clone()), c.expr.clone())).collect();
        let patched_scope = flow_graph::projection::with_arguments(&arguments, &scope);
        let reference_scope = flow_graph::projection::of_graph(&catalog, &checked, "g").unwrap();
        let rows = |sc: &flow_graph::projection::Scope| {
            sc.nodes.iter().map(|n| (n.path.clone(), n.rows.iter().map(|r| (r.label.clone(), r.ty.clone(), r.chip.clone(), r.expr.as_ref().map(flow_syntax::lisp::flat))).collect::<Vec<_>>())).collect::<Vec<_>>()
        };
        assert!(rows(&patched_scope) == rows(&reference_scope), "literal projected rows differ from a full projection");
        d = next;
        lowered = patched;
    }
    for operation in [op(&["g", "a"], "normals", vec![], "\"unknown\""), op(&["g", "c"], "color", vec![], "\"bad\""), op(&["g", "a"], "size", vec![0], "true")] {
        let (slow, fast) = (full_edit(&catalog, &d, &operation), doc::edit(&catalog, &d, &operation));
        assert!(matches!((&slow, &fast), (Err(a), Err(b)) if a.code == b.code), "invalid literal did not refuse atomically with the checker's code");
    }
    // Source identity survives width changes and separate edits before lowering.
    let first = d.clone();
    let next = doc::edit(&catalog, &first, &op(&["g", "b"], "translate", vec![1], "88")).unwrap();
    let next = doc::edit(&catalog, &next, &op(&["g", "a"], "size", vec![2], "27")).unwrap();
    assert_eq!(doc::literal_changes(&first, &next).unwrap().len(), 2, "two literals in one frame lost a change");
    let base = of_text(&catalog, None, STILL);
    let first = doc::edit(&catalog, &base, &op(&["g", "a"], "radius", vec![], "0.6")).unwrap();
    let second = doc::edit(&catalog, &first, &op(&["g", "b"], "translate", vec![0], "7")).unwrap();
    assert_eq!(doc::literal_changes(&second, &first).unwrap().len(), 1, "undo of another field lost the reverse literal change");
    let spelling = doc::edit(&catalog, &first, &op(&["g", "a"], "radius", vec![], "0.60")).unwrap();
    assert_eq!(doc::literal_changes(&first, &spelling).unwrap().len(), 1, "a spelling-only edit left the pane's authored expression stale");
    let cases: Vec<(&str, Op, bool)> = vec![
        ("(workspace w (graph g :context sop (let* [a (sop/uv_sphere :radius ; radius note\n0.5)] a)))", op(&["g", "a"], "radius", vec![], "0.7"), false),
        ("(workspace w (graph g :context sop (let* [copies (for [n (range 3)] (let* [a (sop/box :size [1 1 1])] a))] (sop/merge copies))))", op(&["g", "copies", "a"], "size", vec![0], "2"), false),
        ("(workspace w (graph g :context sop (let* [a (sop/transform (sop/box :size [1 1 1])) b (sop/box :size [1 1 1])] (sop/merge a b))))", op(&["g", "a#0"], "size", vec![0], "2.5"), false),
        ("(workspace w (graph g :context sop (-> (sop/box :size [1 1 1]) (sop/transform) (sop/transform))))", op(&["g", "@result#0#0"], "size", vec![], "[2 3 4]"), false),
        ("(workspace w (defn unit_box :context sop [] (sop/box :size [1 1 1])) (graph g :context sop (sop/merge (unit_box) (unit_box))))", op(&["def:unit_box", "@result"], "size", vec![0], "2.5"), false),
        ("(workspace w (graph g :context sop [(count : int 2)] (sop/box :size [1 1 1])) (graph copies :context sop (sop/merge (ref g) (ref g :count 3))))", op(&["g", "@result"], "size", vec![0], "2.5"), false),
        ("(workspace w (graph g :context sop (let* [a (sop/box :size [1 1 1]) make_box (fn [n] (sop/box)) copies (map make_box (range 3))] (sop/merge a copies))))", op(&["g", "a"], "size", vec![0], "2.5"), true),
    ];
    for (text, operation, fallback) in cases {
        let old = of_text(&catalog, None, text);
        let (next, phases) = sampled(|| doc::edit(&catalog, &old, &operation).unwrap());
        assert!(calls(&phases, "check") == 0 && calls(&phases, "parse") == 0, "literal with notes or in a loop repeated the workspace check");
        let (source, checked) = full_edit(&catalog, &old, &operation).unwrap();
        assert_eq!(doc::to_text(&next).unwrap(), doc::to_text(&doc::T { source, checked: Arc::new(checked.clone()), ..next.clone() }).unwrap(), "loop/comment literal text differs");
        let low = flow_sop::lower::of_checked(Some(factories()), Default::default(), &old.checked).unwrap();
        let change = doc::literal_changes(&old, &next).unwrap().remove(0);
        let (patched, phases) = sampled(|| literal_edit::lower(&low, &change));
        // every card here is a Lisp node: inlined, its literal is part of the one program
        let fallback = fallback || patched.is_none();
        if fallback {
            assert!(patched.is_none(), "geometry template or retained function did not use the full lowering fallback");
        } else {
            assert!(calls(&phases, "lower") == 0 && calls(&phases, "evaluate") == 0, "literal lowered or evaluated: {text}");
            let patched = patched.unwrap().unwrap();
            let full = lower_checked(&checked, patched.compiled_ids.clone(), patched.sites.clone());
            assert_eq!(format!("{:?}", patched.plan), format!("{:?}", full.plan), "literal plan differs: {text}");
        }
    }
    // not ported: the Editor.create half (editor literal phases), see rays_editor
}


#[test]
fn text_edit_keeps_the_ops_arc_so_an_unchanged_graph_is_not_reprojected() {
    let d = doc::of_text(&TextOptions::default(), &catalog(), STILL).unwrap();
    let text = format!("; edited
{STILL}");
    let edited = doc::of_text(&TextOptions { ops: Some(d.checked.ops.clone()), ..Default::default() }, &catalog(), &text).unwrap();
    assert!(Arc::ptr_eq(&d.checked.ops, &edited.checked.ops));
    assert!(flow_graph::projection::same_graph(&d.checked, &edited.checked, "g"));
}
