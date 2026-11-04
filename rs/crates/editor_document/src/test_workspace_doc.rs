//! Port of the Workspace_doc parts (text, edit, imports) of lib/rays_editor/test/test_workspace_doc.ml.
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
                param::field("mode".to_owned(), param::text(), Value::Text(b"a".to_vec())).unwrap().label("Mode".to_owned()).unwrap(),
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
    settings::make(&schema(), vec![Value::Float(0.), Value::Text(b"a".to_vec())])
}

fn p(parts: &[&str]) -> Vec<String> {
    parts.iter().map(|x| (*x).to_owned()).collect()
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
    assert_eq!(settings::get(&schema(), &d.settings).unwrap(), vec![Value::Float(2.5), Value::Text(b"b".to_vec())]);
    let out = doc::to_text(&d);
    assert!(has(&out, "; a small study") && has(&out, "; moved right"), "comments survive to_text");
    let again = of_text(&catalog, Some(plain()), &out);
    assert_eq!(doc::to_text(&again), out, "to_text is a fixed point");
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
    let t = doc::to_text(&plain_doc);
    assert!(!has(&t, "settings") && !has(&t, "layout"), "empty layout and defaults are omitted");
    let bad = |text: &str, code: &str| match doc::of_text(&opts(Some(plain())), &catalog, text) {
        Err(ds) => assert!(ds.iter().any(|d| d.code == code), "{code}: {ds:?}"),
        Ok(_) => panic!("accepted: {code}"),
    };
    bad(&format!("{STILL}(layout (nope))"), "E_LAYOUT");
    bad(&format!("{STILL}(layout (node [\"g\"] :at [1]))"), "E_LAYOUT");
    bad(&format!("{STILL}(settings :nope 1)"), "E_SETTINGS");
    bad(&format!("{STILL}(settings :amount \"x\")"), "E_SETTINGS");
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
    assert!(has(&doc::to_text(&renamed), "(sop/transform ball"), "rename rewrites the source");
    assert_eq!(renamed.checked.name, "study");
    // atomic: a refused edit leaves the document as it was
    assert!(doc::edit(&catalog, &d, &Op::Rename { node: p(&["g", "a"]), to_: "b".to_owned() }).is_err(), "accepted a clashing rename");
    assert!(
        doc::edit(&catalog, &d, &Op::SetArg { node: p(&["g", "a"]), key: ArgKey::Kw("nope".to_owned()), sub: vec![], value: s::make(Node::Num("1".to_owned())) }).is_err(),
        "accepted an unknown keyword"
    );
    assert_eq!(doc::to_text(&d), doc::to_text(&of_text(&catalog, None, &text)), "the original document is untouched");
    let added = doc::edit(
        &catalog,
        &of_text(&catalog, None, &format!("{STILL}(layout (node [\"g\" \"a\"] :at [1 2]) (node [\"g\" \"b\"] :at [3 4]))")),
        &Op::AddNode { scope: p(&["g"]), name: "spare".to_owned(), expr: s::make(Node::Num("1".to_owned())) },
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
        doc::imported_file(&d, &p(&["shared", "@result"])) == Some("lib.rays".to_owned()) && doc::imported_file(&d, &p(&["def:twice"])) == Some("lib.rays".to_owned()),
        "import provenance was lost"
    );
    let printed = doc::to_text(&d);
    assert!(
        has(&printed, "(import \"lib.rays\")") && has(&printed, "library stays separate") && !has(&printed, "(defn twice") && !has(&printed, "(graph shared"),
        "save inlined imported definitions"
    );
    assert_eq!(doc::to_text(&doc::of_text(&with(&imports), &catalog, &printed).unwrap()), printed, "imports did not round trip");
    let edit = Op::SetArg { node: p(&["shared", "@result"]), key: ArgKey::Pos(0), sub: vec![], value: s::make(Node::Num("4".to_owned())) };
    assert!(matches!(doc::edit(&catalog, &d, &edit), Err(x) if x.code == "E_IMPORTED" && has(&x.message, "lib.rays")), "imported expression remained writable");
    assert!(matches!(doc::edit(&catalog, &d, &Op::RemoveGraph { name: "shared".to_owned() }), Err(x) if x.code == "E_IMPORTED"), "imported graph remained removable");
    let one = |t: &str| s::parse(t).unwrap().remove(0);
    let ops = vec![
        Op::RenameGraph { name: "shared".to_owned(), to_: "renamed".to_owned() },
        Op::SetNote { node: p(&["shared"]), text: "changed".to_owned() },
        Op::Wrap { nodes: vec![p(&["shared", "@result"])], loop_: Loop::If },
        Op::AddNode { scope: p(&["shared"]), name: "extra".to_owned(), expr: s::make(Node::Num("1".to_owned())) },
        Op::SetInputDefault { form: "def:twice".to_owned(), input: "x".to_owned(), value: s::make(Node::Num("1".to_owned())) },
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
    let ft = doc::to_text(&fragment);
    assert!(!has(&ft, "(workspace") && !has(&ft, "(layout"), "editing a library fragment appended workspace metadata");
    let error = |imports: &[(String, String)], text: &str, code: &str| {
        assert!(matches!(doc::of_text(&with(imports), &catalog, text), Err(ds) if ds.iter().any(|d| d.code == code)), "missing {code}");
    };
    error(&[], text, "E_IMPORT");
    error(&[("lib.rays".to_owned(), format!("(import \"other.rays\")\n{library}"))], text, "E_IMPORT");
    error(&imports, "(workspace local (graph shared :context value 1)) (import \"lib.rays\")", "E_NAME");
    error(&[], "(workspace local (graph g :context value 1)) (import \"/absolute.rays\")", "E_IMPORT");
    let local = doc::edit(&catalog, &d, &Op::SetGraph { name: "result".to_owned(), form: one("(graph result :context value (twice 9))") }).unwrap();
    assert!(doc::import_texts(&local) == imports && !has(&doc::to_text(&local), "(graph shared"), "local edit changed imported source");
}
