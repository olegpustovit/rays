//! Port of lib/flow_sop/encoded_parameters.ml: pure checked Lisp normalizers used by parameter
//! defaults and native schemas.
use flow_check::catalog::{self as flow_catalog, Parameter};
use flow_check::workspace::{Graph, Node, NodeDefinition, Pattern, Term, Workspace};
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
            let form = normalizer.form.clone();
            let term = |ty: Ty, node: Node| Term { path: None, ty, node, form: form.clone() };
            let argument = term(Ty::Text, Node::RefBinding("raw_value".into(), Vec::new()));
            let body = term(
                Ty::Text,
                Node::Let(
                    vec![(Pattern::Name("normalizer".into()), normalizer.clone())],
                    Box::new(term(
                        Ty::Text,
                        Node::CallFn { fn_name: "normalizer".into(), args: vec![argument], body: None },
                    )),
                ),
            );
            let graph = Graph {
                name: format!("lisp_normalize_{name}"),
                context: context::value(),
                inputs: vec![("raw_value".into(), Ty::Text, None)],
                body,
                form: (*form).clone(),
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
        nodes.push(normalize_node(workspace, node)?);
    }
    Ok(Workspace { nodes: nodes.into(), ..workspace.clone() })
}

struct Hooks {
    nodes: Vec<(String, Vec<(String, Normalizer)>)>,
}

fn walk_terms(hooks: &Hooks, terms: &[Term]) -> Result<Vec<Term>, Diagnostic> {
    terms.iter().map(|t| walk(hooks, t)).collect()
}

fn walk_fields(hooks: &Hooks, fields: &[(String, Term)]) -> Result<Vec<(String, Term)>, Diagnostic> {
    fields.iter().map(|(name, t)| Ok((name.clone(), walk(hooks, t)?))).collect()
}

fn walk_patterns(hooks: &Hooks, bindings: &[(Pattern, Term)]) -> Result<Vec<(Pattern, Term)>, Diagnostic> {
    bindings.iter().map(|(p, t)| Ok((p.clone(), walk(hooks, t)?))).collect()
}

fn walk_box(hooks: &Hooks, term: &Term) -> Result<Box<Term>, Diagnostic> {
    Ok(Box::new(walk(hooks, term)?))
}

fn walk_optional(hooks: &Hooks, term: &Option<Box<Term>>) -> Result<Option<Box<Term>>, Diagnostic> {
    match term {
        None => Ok(None),
        Some(term) => Ok(Some(walk_box(hooks, term)?)),
    }
}

fn walk(hooks: &Hooks, term: &Term) -> Result<Term, Diagnostic> {
    let node = match &term.node {
        Node::Call { kind, ctx, args } => {
            let args = walk_fields(hooks, args)?;
            let table = hooks.nodes.iter().find(|(name, _)| name == kind).map(|(_, h)| h.as_slice()).unwrap_or(&[]);
            let mut canonical = Vec::with_capacity(args.len());
            for (name, arg) in args {
                let hook = table.iter().find(|(hook, _)| *hook == name);
                match (hook, &arg.node) {
                    (Some((_, normalize)), Node::Text(raw)) => {
                        let text = normalize(raw).map_err(|mut d| {
                            d.span = Some(arg.form.span.clone());
                            d
                        })?;
                        canonical.push((name, Term { node: Node::Text(text), ..arg }));
                    }
                    _ => canonical.push((name, arg)),
                }
            }
            Node::Call { kind: kind.clone(), ctx: *ctx, args: canonical }
        }
        Node::Op { op, args, skip } => Node::Op { op: op.clone(), args: walk_fields(hooks, args)?, skip: skip.clone() },
        Node::GraphRef { graph, inputs } => {
            Node::GraphRef { graph: graph.clone(), inputs: walk_fields(hooks, inputs)? }
        }
        Node::CallFn { fn_name, args, body } => {
            let args = walk_terms(hooks, args)?;
            Node::CallFn { fn_name: fn_name.clone(), args, body: walk_optional(hooks, body)? }
        }
        Node::Vec(values) => Node::Vec(walk_terms(hooks, values)?),
        Node::ListLit(values) => Node::ListLit(walk_terms(hooks, values)?),
        Node::Str(values) => Node::Str(walk_terms(hooks, values)?),
        Node::Hof(kind, values) => Node::Hof(*kind, walk_terms(hooks, values)?),
        Node::ListOp(kind, values) => Node::ListOp(kind.clone(), walk_terms(hooks, values)?),
        Node::Record(values) => Node::Record(walk_fields(hooks, values)?),
        Node::Let(bindings, body) => {
            let bindings = walk_patterns(hooks, bindings)?;
            Node::Let(bindings, walk_box(hooks, body)?)
        }
        Node::Loop { kind, accs, clauses, skip, body, zone } => {
            let accs = walk_patterns(hooks, accs)?;
            let clauses = walk_patterns(hooks, clauses)?;
            Node::Loop {
                kind: *kind,
                accs,
                clauses,
                skip: skip.clone(),
                body: walk_box(hooks, body)?,
                zone: zone.clone(),
            }
        }
        Node::State { binder, init, step, zone } => {
            let init = walk_box(hooks, init)?;
            Node::State { binder: binder.clone(), init, step: walk_box(hooks, step)?, zone: zone.clone() }
        }
        Node::WithError(a, b) => {
            let a = walk_box(hooks, a)?;
            Node::WithError(a, walk_box(hooks, b)?)
        }
        Node::ErrorRows { rows, policy, mapper } => {
            let rows = walk_box(hooks, rows)?;
            Node::ErrorRows { rows, policy: *policy, mapper: walk_box(hooks, mapper)? }
        }
        Node::If(condition, yes, no) => {
            let condition = walk_box(hooks, condition)?;
            let yes = walk_box(hooks, yes)?;
            Node::If(condition, yes, walk_box(hooks, no)?)
        }
        Node::Cond(arms, default) => {
            let mut walked = Vec::with_capacity(arms.len());
            for (a, b) in arms {
                let a = walk(hooks, a)?;
                walked.push((a, walk(hooks, b)?));
            }
            Node::Cond(walked, walk_box(hooks, default)?)
        }
        Node::Case(value, arms, default) => {
            let value = walk_box(hooks, value)?;
            let mut walked = Vec::with_capacity(arms.len());
            for (pattern, arm) in arms {
                walked.push((pattern.clone(), walk(hooks, arm)?));
            }
            Node::Case(value, walked, walk_box(hooks, default)?)
        }
        Node::Fn { params, body, zone, capture } => {
            let capture = walk_optional(hooks, capture)?;
            Node::Fn { params: params.clone(), body: walk_box(hooks, body)?, zone: zone.clone(), capture }
        }
        Node::Get(value, field) => Node::Get(walk_box(hooks, value)?, field.clone()),
        Node::Bypass(value) => Node::Bypass(walk_box(hooks, value)?),
        Node::Expanded { macro_name, body } => {
            Node::Expanded { macro_name: macro_name.clone(), body: walk_box(hooks, body)? }
        }
        Node::Assoc(value, updates) => {
            let value = walk_box(hooks, value)?;
            Node::Assoc(value, walk_fields(hooks, updates)?)
        }
        Node::Lit(_) | Node::Text(_) | Node::Nil | Node::Time | Node::RefBinding(..) | Node::FnRef(_) => {
            term.node.clone()
        }
    };
    Ok(Term { node, ..term.clone() })
}

fn walk_graph(hooks: &Hooks, graph: &Graph) -> Result<Graph, Diagnostic> {
    let body = walk(hooks, &graph.body)?;
    let mut inputs = Vec::with_capacity(graph.inputs.len());
    for (name, ty, default) in &graph.inputs {
        let default = match default {
            None => None,
            Some(term) => Some(walk(hooks, term)?),
        };
        inputs.push((name.clone(), ty.clone(), default));
    }
    Ok(Graph { body, inputs, ..graph.clone() })
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
    let graphs = workspace.graphs.iter().map(|g| walk_graph(&hooks, g)).collect::<Result<Vec<_>, _>>()?;
    let defs = workspace.defs.iter().map(|g| walk_graph(&hooks, g)).collect::<Result<Vec<_>, _>>()?;
    let mut nodes = Vec::with_capacity(workspace.nodes.len());
    for node in workspace.nodes.iter() {
        nodes.push(NodeDefinition { body: walk_graph(&hooks, &node.body)?, ..node.clone() });
    }
    Ok(Workspace { graphs: graphs.into(), defs: defs.into(), nodes: nodes.into(), ..workspace })
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
                if let Node::Call { kind, args, .. } = &term.node {
                    if kind == "sop/test_encoded" {
                        return args.iter().find(|(name, _)| name == "rules").map(|(_, arg)| arg.node.clone());
                    }
                }
                stack.extend(workspace::children(&term));
            }
            None
        });
        assert_eq!(literal, Some(Node::Text(b"detail".to_vec())));
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
