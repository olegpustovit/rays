//! Catalog checks that need `operators::all` (live in flow_sop, not flow_sop_catalog).
use crate::operators;
use flow_check::port_type::PortType;
use flow_sop_catalog::catalog;
use flow_sop_catalog::manifest::VERSION;
use flow_sop_catalog::port;
use flow_syntax::syntax;
use sop::edit_graph;
use std::sync::Arc;

#[test]
fn normal_checks_do_not_retain_the_complete_library_syntax() {
    let catalog = catalog::of_factories(1, &[], None, &sop::nodes::factories()).expect("catalog");
    let library = catalog::private::library().expect("library");
    assert!(!catalog::private::library_forms_materialized(library));
    let forms = syntax::parse("(workspace w (graph g :context sop (sop/box)))").expect("parses");
    let (checked, diagnostics) = catalog::check_workspace(operators::all(), false, None, &catalog, &forms);
    assert!(checked.is_some(), "{diagnostics:?}");
    assert!(!checked.unwrap().nodes.is_empty(), "the reached implementation is present");
    assert!(library.forms.get().is_none(), "unused implementations remain unmaterialized");
}

fn field_factory() -> edit_graph::Factory {
    let mut args = edit_graph::factory_args("test_field", "Test field", &["Test"]);
    args.slots = Some(vec!["field".to_owned(), "picture".to_owned()]);
    args.keyword_inputs = vec!["field".to_owned(), "picture".to_owned()];
    args.input_types = Some(vec!["fn(vec3,float)->float".to_owned(), "image".to_owned()]);
    edit_graph::factory(
        args,
        2,
        Arc::new(|inputs| {
            Ok(sop::node::build::make(
                sop::node::build::Args { inputs: inputs.to_vec(), ..sop::node::build::args("test_field") },
                None,
                Arc::new(|_, _, _| Err(port::error("E_TEST", "never cooked"))),
            ))
        }),
    )
}

#[test]
fn keyword_inputs_become_typed_parameters() {
    let field = field_factory();
    assert_eq!(edit_graph::factory_keyword_inputs(&field), ["field", "picture"]);
    assert_eq!(edit_graph::factory_input_types(&field), ["fn(vec3,float)->float", "image"]);
    let mut factories = vec![field];
    factories.extend(sop::nodes::factories());
    let catalog = catalog::of_factories(VERSION, &[], None, &factories).expect("catalog");
    let kind = &catalog.kinds[0];
    assert!(kind.slots.is_empty());
    assert_eq!(kind.parameters.iter().map(|p| p.name.as_str()).collect::<Vec<_>>(), ["field", "picture"]);
    assert!(matches!(kind.parameters[0].ty, Some(PortType::Fn(_))));
    assert_eq!(kind.parameters[1].ty, Some(PortType::Image));
    let checked = |args: &str| {
        let text = format!("(workspace w (graph g :context sop (sop/test_field {args})))");
        catalog::check_workspace(operators::all(), false, None, &catalog, &syntax::parse(&text).expect("parses")).0.is_some()
    };
    assert!(checked(":field (fn [p amount] (+ p.x amount)) :picture (image/noise :width 2 :height 2)"));
    assert!(!checked(":field (fn [p] p.x) :picture (image/noise)"));
    assert!(!checked(":picture (image/noise)"));
    assert!(!checked(":field (fn [p amount] (+ p.x amount))"));
}
