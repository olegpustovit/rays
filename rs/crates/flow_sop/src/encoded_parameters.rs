//! Port of lib/flow_sop/encoded_parameters.ml: pure checked Lisp normalizers used by parameter
//! defaults and native schemas.
use flow_check::catalog::{self as flow_catalog, Parameter};
use flow_check::workspace::{Graph, Node, NodeDefinition, Pattern, Term, Workspace};
use flow_check::term_arena::{Draft, TermBuilder};
use flow_eval::{Host, Residual, ResidualHandlers, Value};
use flow_syntax::context;
use flow_syntax::phase_timer;
use flow_syntax::syntax::Syntax;
use flow_syntax::ty::Ty;
use param::{FieldView, Value as ParamValue};
use rays_diagnostic::Diagnostic;
use std::sync::Arc;

/// A node's text normalizer: canonical text from raw text, or the checker's diagnostic.
pub type Normalizer = Arc<dyn Fn(&[u8]) -> Result<Vec<u8>, Diagnostic> + Send + Sync>;

fn normalize_error(span: &Syntax, message: &str) -> Diagnostic {
    rays_diagnostic::error(
        "E_PARAM_NORMALIZE".into(),
        message.into(),
        Some(span.span.clone()),
        None,
        None,
        Vec::new(),
        Vec::new(),
    )
}

fn evaluate(workspace: &Workspace, graph: &str, form: &Syntax, raw: &[u8]) -> Result<Vec<u8>, Diagnostic> {
    let inputs = vec![(graph.to_owned(), vec![("raw_value".to_owned(), Value::Text(raw.to_vec()))])];
    let result = Residual::handle(ResidualHandlers::default(), &mut || {
        Host::static_with_requests(None, false, Some(&inputs), workspace)
    })?;
    match result.results.iter().find(|(name, _)| name == graph) {
        Some((_, Value::Text(canonical))) if result.plan.nodes.is_empty() => Ok(canonical.clone()),
        _ => Err(normalize_error(form, "An encoded normalizer must return concrete text without cooked effects.")),
    }
}

/// The normalizers a node definition declares, each evaluated through a one-graph workspace.
pub fn for_node(checked: &Workspace, definition: &NodeDefinition) -> Vec<(String, Normalizer)> {
    definition
        .normalizers
        .iter()
        .map(|(name, normalizer)| {
            let mut builder = TermBuilder::new();
            let form_id = builder.import_form(&normalizer.form());
            let normalizer_id = builder.import(normalizer);
            let argument = builder.push(None, &Ty::Text, form_id, Draft::RefBinding("raw_value".into(), Vec::new()));
            let call = builder.push(None, &Ty::Text, form_id, Draft::CallFn { fn_name: "normalizer".into(), args: vec![argument], body: None });
            let body = builder.push(None, &Ty::Text, form_id, Draft::Let(vec![(Pattern::Name("normalizer".into()), normalizer_id)], call));
            let arena = builder.freeze();
            let body = arena.handle(body);
            let form = arena.syntax().handle(form_id);
            let graph = Graph {
                name: format!("lisp_normalize_{name}"),
                context: context::value(),
                inputs: vec![("raw_value".into(), Ty::Text, None)],
                body,
                form: form.clone(),
            };
            let graph_name = graph.name.clone();
            let mut one = checked.clone();
            one.graphs = vec![graph].into();
            let one = Arc::new(one);
            let normalize: Normalizer = Arc::new(move |raw: &[u8]| {
                let _untimed = phase_timer::without();
                let canonical = evaluate(&one, &graph_name, &form, raw)?;
                let again = evaluate(&one, &graph_name, &form, &canonical)?;
                if again == canonical {
                    Ok(canonical)
                } else {
                    Err(normalize_error(&form, "An encoded normalizer must preserve its canonical output."))
                }
            });
            (name.clone(), normalize)
        })
        .collect()
}

fn normalize_node(workspace: &Workspace, node: &NodeDefinition) -> Result<NodeDefinition, Diagnostic> {
    let normalizers = for_node(workspace, node);
    let mut fields: Vec<FieldView> = Vec::with_capacity(node.fields.len());
    for field in &node.fields {
        let hook = normalizers.iter().find(|(name, _)| *name == field.name);
        match (hook, &field.default) {
            (Some((_, normalize)), ParamValue::Text(raw)) => {
                let canonical = normalize(raw)?;
                let value = ParamValue::Text(canonical);
                fields.push(FieldView { default: value.clone(), current: value, ..field.clone() });
            }
            _ => fields.push(field.clone()),
        }
    }
    let mut parameters = flow_catalog::parameters_of_fields(&fields)
        .map_err(|message| rays_diagnostic::error("E_VEC3".into(), message, None, None, None, Vec::new(), Vec::new()))?;
    let keywords: Vec<Parameter> = node
        .kind
        .parameters
        .iter()
        .filter(|p| node.keyword_inputs.contains(&p.name))
        .cloned()
        .collect();
    parameters.extend(keywords);
    let mut kind = node.kind.clone();
    kind.parameters = parameters;
    Ok(NodeDefinition { fields, kind, ..node.clone() })
}

/// Normalize defaults of every node with an encoded parameter and the literal text arguments of
/// calls to such nodes.
fn normalize_defaults(workspace: &Workspace) -> Result<Workspace, Diagnostic> {
    let mut nodes = Vec::with_capacity(workspace.nodes.len());
    for node in workspace.nodes.iter() {
        // a node with no normalizer keeps its fields and parameters as checked
        if node.normalizers.is_empty() {
            nodes.push(node.clone());
            continue;
        }
        nodes.push(Arc::new(normalize_node(workspace, node)?));
    }
    Ok(Workspace { nodes: nodes.into(), ..workspace.clone() })
}

struct Hooks {
    nodes: Vec<(String, Vec<(String, Normalizer)>)>,
}

fn walk_graph(hooks: &Hooks, graph: &Graph) -> Result<Graph, Diagnostic> {
    let mut builder = TermBuilder::new();
    let roots: Vec<_> = std::iter::once(graph.body.clone()).chain(graph.inputs.iter().filter_map(|(_, _, default)| default.clone())).collect();
    let mut error = None;
    let ids = builder.rewrite_roots(&roots, &mut |builder, _, draft| {
        if error.is_some() { return draft; }
        let Draft::Call { kind, ctx, args } = draft else { return draft };
        let table = hooks.nodes.iter().find(|(name, _)| name == &kind).map(|(_, h)| h.as_slice()).unwrap_or(&[]);
        let mut canonical = Vec::with_capacity(args.len());
        for (name, arg) in args {
            let hook = table.iter().find(|(hook, _)| *hook == name);
            let arg = match (hook, builder.node(arg)) {
                (Some((_, normalize)), Node::Text(raw)) => {
                    match normalize(builder.raw_bytes(raw)) {
                        Ok(text) => {
                            let row = *builder.row(arg);
                            let ty = builder.type_by_id(row.ty).clone();
                            let path: Option<Vec<_>> = row.path.map(|id| builder.path_names(id).iter().map(|&id| builder.name(id).to_owned()).collect());
                            builder.push(path.as_deref(), &ty, row.form, Draft::Text(text))
                        }
                        Err(mut d) => {
                            let form = builder.row(arg).form;
                            d.span = Some(builder.syntax_builder().row(form).span.clone());
                            error = Some(d);
                            arg
                        }
                    }
                }
                _ => arg,
            };
            canonical.push((name, arg));
        }
        Draft::Call { kind, ctx, args: canonical }
    });
    if let Some(error) = error { return Err(error); }
    let form = builder.import_form(&graph.form);
    let arena = builder.freeze();
    let mut defaults = ids[1..].iter();
    Ok(Graph {
        body: arena.handle(ids[0]),
        inputs: graph.inputs.iter().map(|(name, ty, default)| (name.clone(), ty.clone(), default.as_ref().map(|_| arena.handle(*defaults.next().unwrap())))).collect(),
        form: arena.syntax().handle(form),
        ..graph.clone()
    })
}

/// Node-owned schemas and cook identity for checked Lisp geometry bodies: the workspace with
/// every encoded default and literal argument canonicalized.
pub fn checked(workspace: Workspace) -> Result<Workspace, Diagnostic> {
    if workspace.nodes.iter().all(|node| node.normalizers.is_empty()) {
        return Ok(workspace);
    }
    let workspace = normalize_defaults(&workspace)?;
    let hooks = Hooks {
        nodes: workspace.nodes.iter().map(|node| (node.kind.qualified.clone(), for_node(&workspace, node))).collect(),
    };
    // Only a graph that calls a node with a hook can change; a collection with none keeps its Arc.
    let hooked: Vec<&str> = hooks.nodes.iter().filter(|(_, h)| !h.is_empty()).map(|(k, _)| k.as_str()).collect();
    let rebuild = |all: &Arc<Vec<Graph>>| -> Result<Arc<Vec<Graph>>, Diagnostic> {
        if !all.iter().any(|g| graph_calls(g, &hooked)) {
            return Ok(all.clone());
        }
        let walked = all.iter().map(|g| if graph_calls(g, &hooked) { walk_graph(&hooks, g) } else { Ok(g.clone()) }).collect::<Result<Vec<_>, _>>()?;
        Ok(Arc::new(walked))
    };
    let graphs = rebuild(&workspace.graphs)?;
    let defs = rebuild(&workspace.defs)?;
    let nodes = if workspace.nodes.iter().any(|n| graph_calls(&n.body, &hooked)) {
        let mut nodes = Vec::with_capacity(workspace.nodes.len());
        for node in workspace.nodes.iter() {
            nodes.push(if graph_calls(&node.body, &hooked) {
                Arc::new(NodeDefinition {
                    kind: node.kind.clone(), slots: node.slots.clone(), keyword_inputs: node.keyword_inputs.clone(), fields: node.fields.clone(),
                    label: node.label.clone(), category: node.category.clone(), description: node.description.clone(), operation: node.operation.clone(), version: node.version,
                    body: walk_graph(&hooks, &node.body)?, validations: node.validations.clone(), normalizers: node.normalizers.clone(),
                })
            } else {
                node.clone()
            });
        }
        Arc::new(nodes)
    } else {
        workspace.nodes.clone()
    };
    Ok(Workspace { graphs, defs, nodes, ..workspace })
}

/// Whether any term of the graph calls one of these node kinds (the only terms `walk` changes).
fn graph_calls(graph: &Graph, kinds: &[&str]) -> bool {
    !kinds.is_empty() && (calls(&graph.body, kinds) || graph.inputs.iter().any(|(_, _, d)| d.as_ref().is_some_and(|t| calls(t, kinds))))
}

fn calls(term: &Term, kinds: &[&str]) -> bool {
    matches!(term.node, Node::Call(id) if kinds.contains(&term.name(term.arena().call(id).kind)))
        || term.children().iter().any(|&id| calls(&term.child(id), kinds))
}

#[cfg(test)]
mod tests {
    //! The parts of test_encoded_parameters.ml that check a workspace (no lowering, cooking,
    //! projection or editing): defaults canonicalized through the hook, literal arguments
    //! canonicalized, and the refusal codes.
    use super::*;
    use crate::catalog::{self, Descriptor};
    use crate::operators;
    use flow_check::catalog::Catalog;
    use flow_check::workspace;
    use flow_syntax::lisp;
    use flow_syntax::syntax;

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

    fn catalog() -> Catalog {
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
        let catalog = catalog();
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
        let catalog = catalog();
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
        let catalog = catalog();
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
        let (errors, diagnostics) =
            check(&catalog, "(workspace errors (graph g :context sop (if true (sop/line) (error \"E_TEST\" \"bad\"))))");
        assert!(errors.is_some(), "{diagnostics:?}");
    }

    #[test]
    fn a_normalizer_runs_to_its_fixed_point() {
        let catalog = catalog();
        let (checked, diagnostics) = check(&catalog, &source(NORMALIZER, " POINTS ", "test_encoded", ""));
        let checked = checked.unwrap_or_else(|| panic!("{diagnostics:?}"));
        let definition = checked.nodes.iter().find(|n| n.kind.qualified == "sop/test_encoded").expect("node");
        let normalizers = for_node(&checked, definition);
        let (_, normalize) = normalizers.iter().find(|(name, _)| name == "rules").expect("rules hook");
        assert_eq!(normalize(b" DETAIL ").expect("canonical"), b"detail".to_vec());
        assert_eq!(normalize(b"detail").expect("idempotent"), b"detail".to_vec());
        assert_eq!(normalize(b"bad").unwrap_err().code, "E_RULES");
    }
}
