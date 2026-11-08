//! Encoded-parameter workspace checks (need flow_sop::operators).
use crate::operators;
use flow_check::catalog::Catalog;
use flow_check::workspace::{self, Node, Workspace};
use flow_sop_catalog::catalog::{self, Descriptor};
use flow_sop_catalog::encoded_parameters::for_node;
use param::Value as ParamValue;
use rays_diagnostic::Diagnostic;
use flow_syntax::{lisp, syntax};

const NORMALIZER: &str = r#"(fn [(raw : text)]
  (case (text/lower (text/trim raw)) "points" "point" "point" "point" "detail" "detail"
    :else (error "E_RULES" (str "Unknown owner: " raw))))"#;

fn declaration(hook: &str, default: &str, operation: &str) -> String {
    format!(
        "(defnode sop/test_encoded :label \"Encoded card\" :operation \"{operation}\" [(geo : geometry) (rules : text \"{default}\" :kind encoded :normalize {hook})]\n(sop/with_attr geo :tag (array/text (sop/count geo :points) rules) :type text))"
    )
}

fn source(hook: &str, default: &str, operation: &str, argument: &str) -> String {
    format!(
        "(workspace encoded {} (graph g :context sop (let* [card (sop/test_encoded (sop/line :points 3) {argument})] card)))",
        declaration(hook, default, operation)
    )
}

fn test_catalog() -> Catalog {
    catalog::of_factories(1, &[] as &[Descriptor], None, &sop::nodes::factories()).expect("catalog")
}

fn check(catalog: &Catalog, text: &str) -> (Option<Workspace>, Vec<Diagnostic>) {
    catalog::check_workspace(operators::all(), false, None, catalog, &syntax::parse(text).expect("parses"))
}

fn refused(catalog: &Catalog, code: &str, text: &str) {
    let (workspace, diagnostics) = check(catalog, text);
    assert!(
        workspace.is_none() && diagnostics.iter().any(|d| d.code == code),
        "expected {code}: {:?}",
        diagnostics.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>()
    );
}

#[test]
fn defaults_are_canonical_and_the_authored_source_is_kept() {
    let catalog = test_catalog();
    let text = source(NORMALIZER, " POINTS ", "test_encoded", "");
    let forms = syntax::parse(&text).expect("parses");
    let (checked, diagnostics) = catalog::check_workspace(operators::all(), false, None, &catalog, &forms);
    let checked = checked.unwrap_or_else(|| panic!("{diagnostics:?}"));
    let definition = checked.nodes.iter().find(|n| n.kind.qualified == "sop/test_encoded").expect("node");
    assert_eq!(definition.fields[0].default, ParamValue::Text(b"point".to_vec()));
    assert_eq!(lisp::flat(&checked.source[0]), lisp::flat(&forms[0]));
    assert_eq!(definition.kind.parameters[0].fields[0].2, ParamValue::Text(b"point".to_vec()));
}

#[test]
fn literal_arguments_are_canonicalized_and_bad_ones_refused() {
    let catalog = test_catalog();
    let (checked, diagnostics) = check(&catalog, &source(NORMALIZER, "point", "test_encoded", ":rules \" DETAIL \""));
    let checked = checked.unwrap_or_else(|| panic!("{diagnostics:?}"));
    let literal = checked.graphs.iter().find_map(|graph| {
        let mut stack = vec![graph.body.clone()];
        while let Some(term) = stack.pop() {
            if let Node::Call(id) = term.node {
                let call = term.arena().call(id);
                if term.name(call.kind) == "sop/test_encoded" {
                    return term.arena().fields(call.args).iter().find(|field| term.name(field.name) == "rules").and_then(|field| {
                        match term.child(field.term).node {
                            Node::Text(id) => Some(term.arena().bytes(id).to_vec()),
                            _ => None,
                        }
                    });
                }
            }
            stack.extend(term.children().iter().map(|&id| term.child(id)));
        }
        None
    });
    assert_eq!(literal, Some(b"detail".to_vec()));
    refused(&catalog, "E_RULES", &source(NORMALIZER, "bad", "test_encoded", ""));
    refused(&catalog, "E_RULES", &source(NORMALIZER, "point", "test_encoded", ":rules \"bad\""));
    refused(&catalog, "E_PARAM_NORMALIZE", &source("(fn [(raw : text)] (str raw \"x\"))", "point", "test_encoded", ""));
    refused(&catalog, "E_SHAPE", &source(NORMALIZER, "point", "", ""));
}

#[test]
fn hooks_must_be_typed_pure_text_functions() {
    let catalog = test_catalog();
    for (hook, code) in [
        ("(fn [(raw : int)] raw)", "E_TYPE"),
        ("(fn [(raw : text)] 1)", "E_TYPE"),
        ("(fn [(raw : text)] (str raw t))", "E_PARAM_NORMALIZE"),
        ("(fn [(raw : text)] (let* [img (image/noise)] raw))", "E_PARAM_NORMALIZE"),
    ] {
        refused(&catalog, code, &source(hook, "point", "test_encoded", ""));
    }
    refused(&catalog, "E_SHAPE", "(workspace bad (defnode sop/bad [(geo : geometry) (rules : text \"x\" :kind encoded)] geo))");
    refused(
        &catalog,
        "E_SHAPE",
        "(workspace bad (defnode sop/bad [(geo : geometry) (rules : text \"x\" :normalize (fn [(raw : text)] raw))] geo))",
    );
    refused(
        &catalog,
        "E_PARAM_NORMALIZE",
        &format!(
            "(workspace bad (defn impure :context value [(raw : text)] (str raw t)) {} (graph g :context sop (sop/test_encoded nil)))",
            declaration("impure", "point", "test_encoded")
        ),
    );
    let pure = format!(
        "(workspace pure (defn canonical :context value [(raw : text)] (text/lower (text/trim raw))) {} (graph g :context sop (sop/test_encoded nil)))",
        declaration("canonical", "point", "test_encoded")
    );
    let (checked, diagnostics) = check(&catalog, &pure);
    assert!(checked.is_some(), "{diagnostics:?}");
    let (errors, diagnostics) = check(&catalog, "(workspace errors (graph g :context sop (if true (sop/line) (error \"E_TEST\" \"bad\"))))");
    assert!(errors.is_some(), "{diagnostics:?}");
}

#[test]
fn a_normalizer_runs_to_its_fixed_point() {
    let catalog = test_catalog();
    let (checked, diagnostics) = check(&catalog, &source(NORMALIZER, " POINTS ", "test_encoded", ""));
    let checked = checked.unwrap_or_else(|| panic!("{diagnostics:?}"));
    let definition = checked.nodes.iter().find(|n| n.kind.qualified == "sop/test_encoded").expect("node");
    let normalizers = for_node(&checked, definition);
    let (_, normalize) = normalizers.iter().find(|(name, _)| name == "rules").expect("rules hook");
    assert_eq!(normalize(b" DETAIL ").expect("canonical"), b"detail".to_vec());
    assert_eq!(normalize(b"detail").expect("idempotent"), b"detail".to_vec());
    assert_eq!(normalize(b"bad").unwrap_err().code, "E_RULES");
}
