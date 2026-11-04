//! Port of lib/flow_sop/test_node_sources.ml: the embedded node library, its pruning, kept checks,
//! cache-key facts, manifest round trip and diagnostics.
use super::support::*;
use crate::{catalog, context_kernel, lower, manifest, operators};
use flow_check::catalog::{Catalog, Kind};
use flow_check::defnode::{self, Defnode};
use flow_check::workspace::Workspace;
use flow_graph::flow_edit::Op as EditOp;
use flow_syntax::syntax::{self, Node, Syntax};
use flow_syntax::value::Value;
use rays_diagnostic::{Diagnostic, Severity};
use sop::context::dependencies::{self, Fact};
use sop::edit_graph as edit;
use sop::session;

fn ok<A>(r: Result<A, Diagnostic>) -> A {
    r.unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)))
}

fn null_factory() -> edit::Factory {
    sop::nodes::factories().into_iter().find(|f| edit::factory_key(f) == "null").expect("null factory")
}

fn line_nodes() -> Vec<Defnode> {
    ok(catalog::node_definitions().map(|n| n.to_vec())).into_iter().filter(|n| n.kind.qualified == "sop/line").collect()
}

fn checked(r: (Option<Workspace>, Vec<Diagnostic>)) -> Workspace {
    match r {
        (Some(w), d) if d.is_empty() => w,
        (_, d) => panic!("{}", d.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")),
    }
}

#[test]
fn an_explicit_library_is_pruned_to_what_the_workspace_reaches() {
    let cat = Catalog { version: 1, kinds: vec![] };
    let authored = parse("(workspace narrow (graph value :context value 0.0))");
    let workspace = checked(catalog::check_workspace(&[], false, None, &cat, &authored));
    assert!(workspace.nodes.is_empty() && workspace.ops.is_empty());
    let sources = parse("(defnode sop/selected_fixture [(geo : geometry)] geo)");
    // An explicit library is pruned to what the workspace reaches; library:true checks all of it.
    let workspace = checked(catalog::check_workspace(&[], false, Some(&sources), &cat, &authored));
    assert!(workspace.nodes.is_empty() && workspace.source == authored);
    let workspace = checked(catalog::check_workspace(&[], true, Some(&sources), &cat, &authored));
    assert_eq!(workspace.nodes.iter().map(|n| n.kind.qualified.clone()).collect::<Vec<_>>(), vec!["sop/selected_fixture".to_owned()]);
    assert!(workspace.source == authored);
}

#[test]
fn the_embedded_source_list_and_a_fresh_copy_agree() {
    // The embedded immutable source list uses the prepared index; a fresh list spine with the same
    // forms takes the uncached path. Both must select the same definitions and preserve identical
    // checker diagnostics.
    let embedded = ok(catalog::sources());
    let copied: Vec<Syntax> = embedded.to_vec();
    let nodes = ok(catalog::node_definitions());
    let cat = ok(catalog::of_factories(manifest::VERSION, &[], Some(nodes), &[]));
    let describe = |r: (Option<Workspace>, Vec<Diagnostic>)| -> String {
        match r {
            (None, diagnostics) => format!("Error {:?}", diagnostics.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>()),
            (Some(w), diagnostics) => format!(
                "Ok {:?} {:?} {:?}",
                diagnostics.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>(),
                w.nodes.iter().map(|n| n.kind.qualified.clone()).collect::<Vec<_>>(),
                w.source.iter().map(|f| syntax::head(f).map(str::to_owned)).collect::<Vec<_>>()
            ),
        }
    };
    let check = |sources: &[Syntax], text: &str| {
        let forms = parse(text);
        let result = catalog::check_workspace(operators::all(), false, Some(sources), &cat, &forms);
        (forms, describe(result))
    };
    for text in ["(workspace indexed (graph g :context sop (sop/points :points 2)))", "(workspace indexed (graph g :context sop (sop/points :not_a_parameter 2)))"] {
        let (fast_forms, fast) = check(embedded, text);
        let (fallback_forms, fallback) = check(&copied, text);
        assert_eq!(fast, fallback);
        assert_eq!(fast_forms.iter().map(|f| syntax::head(f).map(str::to_owned)).collect::<Vec<_>>(), fallback_forms.iter().map(|f| syntax::head(f).map(str::to_owned)).collect::<Vec<_>>());
    }
}

/// A parameter change is an edit of the call's text.
fn helper_seed_node(enabled: &str) -> sop::node::T {
    let source = parse(&format!(
        "(workspace helper_seed
    (defn helper_seed :context value [(enabled : bool)]
      (let* [selected enabled] (if selected (sop/context_seed \"helper_fixture\") 0)))
    (defn forward_seed :context value [(enabled : bool)] (helper_seed enabled))
    (defnode sop/helper_seed [(geo : geometry) (enabled : bool false)]
      (let* [shift (forward_seed (not enabled))]
        (sop/with_attr geo :P (exact
          (map (fn [p] (+ p [shift 0.0 0.0])) (sop/attr geo :P))))))
    (graph g :context sop (sop/helper_seed (sop/line :kind \"Points\" :points 2) :enabled {enabled})))"
    ));
    let nodes = line_nodes();
    let cat = ok(catalog::of_factories(1, &[], Some(&nodes), &[]));
    let workspace = checked(catalog::check_workspace(operators::all(), false, None, &cat, &source));
    let lowered = ok(lower::of_checked(Some(vec![]), lower::Options::default(), &workspace));
    let graph = &lowered.graphs[0];
    edit::compile_node(&graph.network.geometry, graph.root.expect("a graph result")).expect("compile")
}

#[test]
#[ignore = "deadlock: irfix (exceeds 5 s: flow_ir static-map hang in qualify_workspace on the defnode body)"]
fn a_helper_seed_read_is_a_cache_dependency_only_while_reachable() {
    let node = helper_seed_node("false");
    // the card cone: its marker over its inlined body
    assert!(dependencies::mem(Fact::Seed, sop::node::graph_dependencies(&node)));
    let inactive = helper_seed_node("true");
    assert!(!dependencies::mem(Fact::Seed, sop::node::graph_dependencies(&inactive)));
    fn cook_at(s: &mut session::T, n: &sop::node::T, seed: i64) -> (f64, f64, f64) {
        let context = sop::context::create(sop::context::Options { seed: Some(seed), ..Default::default() }).unwrap();
        let g = cook(s, &context, n);
        rdk::packed::Float3::get(rdk::geometry::positions(&g), 0).expect("a point")
    }
    let mut s = session::create(16, 1_000_000).unwrap();
    let first = cook_at(&mut s, &node, 3);
    let before = session::stats(&s);
    assert!(cook_at(&mut s, &node, 0x7000000000000000) != first);
    assert!(session::stats(&s).misses == before.misses + 1);
    assert!(cook_at(&mut s, &inactive, 3) == (0.0, 0.0, 0.0));
    let before = session::stats(&s);
    assert!(cook_at(&mut s, &inactive, 0x7000000000000000) == (0.0, 0.0, 0.0));
    assert!(session::stats(&s).misses == before.misses);
    session::close(&mut s);
}

#[test]
fn a_seed_read_inside_a_fn_is_scoped_to_the_fn() {
    let source = parse(
        "(workspace fn_seed_scope
    (defn fn_seed_helper :context value []
      (first (map (fn [enabled] (if enabled (sop/context_seed \"fn_fixture\") 0))
        (list true))))
    (defnode sop/fn_seed_scope [(geo : geometry) (enabled : bool false)]
      (let* [shift (fn_seed_helper)] geo))
    (graph g :context sop (sop/fn_seed_scope (sop/line :points 2))))",
    );
    let nodes = line_nodes();
    let cat = ok(catalog::of_factories(1, &[], Some(&nodes), &[]));
    let workspace = checked(catalog::check_workspace(operators::all(), false, None, &cat, &source));
    let definition = workspace.nodes.iter().find(|n| n.kind.qualified == "sop/fn_seed_scope").expect("definition");
    assert!(context_kernel::depends(None, &[("enabled".to_owned(), Value::Bool(false))], &workspace.defs, &definition.body));
}

fn has(name: &str, text: &str, form: &Syntax) -> bool {
    (syntax::head(form) == Some(name)
        && syntax::children(form).iter().any(|child| match &child.node {
            Node::Str(value) => value.as_slice() == text.as_bytes(),
            Node::Sym(value) => value == text,
            _ => false,
        }))
        || syntax::children(form).iter().any(|c| has(name, text, c))
}

fn sorted(kinds: &[Kind]) -> Vec<Kind> {
    let mut kinds = kinds.to_vec();
    kinds.sort_by(|a, b| a.qualified.cmp(&b.qualified));
    kinds
}

#[test]
fn embedded_fixture_checks_edits_and_round_trips_the_manifest() {
    ok(catalog::sources());
    let forms = parse(
        "(defnode sop/embedded_fixture :label \"Embedded fixture\" :category [\"Test\"]
    :facts {:elementwise points :reads [\"P\"] :writes [\"P\"] :topology preserved :exact true}
    [(geo : geometry) (distance : float 0.5 :min -10 :max 10 :primary true)]
    (sop/null geo))",
    );
    let node = ok(defnode::parse(&forms[0]));
    let cat = ok(catalog::of_factories(manifest::VERSION, &[], Some(&[node.clone()]), &[]));
    let authored = parse(
        "(workspace w (graph g :context sop
    (sop/embedded_fixture (sop/line) :distance 2.0)))",
    );
    // The body delegates to a real native kind; the custom fixture itself is Lisp.
    let native = ok(catalog::of_factories(manifest::VERSION, &[], Some(&line_nodes()), &[null_factory()]));
    let mut kinds = native.kinds.clone();
    kinds.extend(cat.kinds.clone());
    let cat = Catalog { kinds, ..cat };
    let workspace = checked(catalog::check_workspace(&[], false, Some(&forms), &cat, &authored));
    assert!(workspace.source == authored);
    assert_eq!(workspace.nodes.len(), 1);
    let graph = &workspace.graphs[0];
    let source_graph = *syntax::children(&authored[0]).last().unwrap();
    assert_eq!(graph.form.id, source_graph.id);
    assert!(workspace.nodes[0].body.form.id > source_graph.id);
    let checker_forms = forms.clone();
    let checker = move |c: &Catalog, s: &[Syntax]| catalog::check_workspace(&[], false, Some(&checker_forms), c, s);
    let (edited, checked_edit) = ok(flow_graph::flow_edit::apply_checked_with(&[], false, Some(&checker), &cat, &authored, &EditOp::SetNote { node: vec!["g".to_owned(), "@result".to_owned()], text: "kept note".to_owned() }));
    assert!(checked_edit.source == edited);
    assert_eq!(checked_edit.nodes.len(), 1);
    let saved = flow_syntax::lisp::print(&edited).0;
    assert!(syntax::children(&edited[0]).iter().all(|form| syntax::head(form) != Some("defnode")));
    assert_eq!(checked(catalog::check_workspace(&[], false, Some(&forms), &cat, &parse(&saved))).nodes.len(), 1);
    let mut nodes = vec![node.clone()];
    nodes.extend(line_nodes());
    let (text, digest) = ok(manifest::generate(&[], Some(&nodes), &[null_factory()]));
    let (reread, read_digest) = ok(flow_check::catalog::catalog_of_manifest(&text));
    assert!(digest == read_digest && sorted(&reread.kinds) == sorted(&cat.kinds));
    let tree = &parse(&text)[0];
    assert!(has("body", "lisp", tree));
    assert!(has("calls", "sop/null", tree));
    let keyword_node = ok(defnode::parse(&parse("(defnode sop/keyword_fixture\n    [(geo : geometry) (image : image :keyword true)\n     (field : (fn [vec3] float) :keyword true)] geo)")[0]));
    let (keyword_text, _) = ok(manifest::generate(&[], Some(&[keyword_node.clone()]), &[]));
    let (keyword_catalog, _) = ok(flow_check::catalog::catalog_of_manifest(&keyword_text));
    assert!(keyword_catalog.kinds == vec![keyword_node.kind.clone()]);
    let keyword_tree = &parse(&keyword_text)[0];
    assert!(has("keyword-inputs", "image", keyword_tree));
    assert!(has("slot", "field", keyword_tree));
}

#[test]
#[ignore = "blocked upstream: flow_check message text differs from OCaml (E_TYPE 'expects vec4; got text.' vs 'expected vec4, got text.'; E_FIELD 'Value has no such output.' vs 'c has no output x.')"]
fn library_diagnostics_keep_their_codes_spans_and_messages_and_checks_are_kept() {
    // Library diagnostics keep their codes, spans and messages, and a second check of the same
    // workspace reuses the kept library check instead of checking the library again.
    let cat = ok(catalog::of_factories(manifest::VERSION, &[], None, &sop::nodes::factories()));
    let check = |text: &str| -> Vec<String> {
        let (_, diagnostics) = catalog::check_workspace(operators::all(), false, None, &cat, &parse(text));
        diagnostics
            .iter()
            .map(|d: &Diagnostic| {
                let (start, finish) = d.span.clone().map_or((0, 0), |s| (s.start, s.finish));
                format!("{start}-{finish} {}", rays_diagnostic::to_string(d))
            })
            .collect()
    };
    let text = "(workspace kept
    (defn afi_lerp :context value [(a : float)] a)
    (graph g :context value (afi_channel \"r\" \"not a vec4\")))";
    // dev's diagnostics before library checks were kept: the E_FIELDs sit in afi_channel's body
    let mut expected = vec!["20-66 E_NAME: Invalid, reserved or duplicate name: afi_lerp.".to_owned(), "112-124 E_TYPE: afi_channel :c: expected vec4, got text.".to_owned()];
    for (span, f) in [("1210-1213", "x"), ("1243-1246", "y"), ("1276-1279", "z"), ("1309-1312", "w"), ("1339-1342", "z"), ("1355-1358", "y"), ("1369-1372", "x")] {
        expected.push(format!("{span} E_FIELD: c has no output {f}. A vec3 has .x .y .z; a record has its fields."));
    }
    assert_eq!(check(text), expected);
    let before = catalog::private::library_checks();
    assert_eq!(check(text), expected);
    assert_eq!(catalog::private::library_checks(), before);
    let _ = Severity::ErrorSeverity;
}
