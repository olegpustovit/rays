//! Port of lib/sop/test_edit_graph.ml. Lisp-built nodes are replaced by `snapshot` sources, the
//! ported `null` factory and a small N-input pass-through. Not ported: the "factory metadata
//! sharing" block (it asserts OCaml physical sharing of immutable default values).
use std::sync::Arc;

use crate::edit_graph::{self, InputRequirement::*};
use crate::node::{self, build, CookMode};
use crate::shapes;
use crate::test_support::*;

fn check(ok: bool, message: &str) {
    assert!(ok, "{message}");
}

fn get<T>(r: Result<T, String>) -> T {
    r.unwrap_or_else(|e| panic!("{e}"))
}

/// `merge_of`: a node over any number of inputs; its output is its first input's geometry.
fn merge_of(inputs: Vec<node::T>) -> node::T {
    build::make_geometry(
        build::Args { inputs, cook_mode: CookMode::Generic, ..build::args("merge") },
        None,
        Arc::new(|_, _, inputs| Ok(node::GeometryCooked { geometry: inputs[0].clone(), instances: None })),
    )
}

fn nodes(list: &[Option<node::T>]) -> Vec<node::T> {
    list.iter().flatten().cloned().collect()
}

#[test]
fn editable_graph() {
    let source = snapshot(&fixture());
    let null = shapes::null::factory();
    let middle = get(edit_graph::instantiate(&null, &[source.clone()]));
    let output = get(edit_graph::instantiate(&null, &[middle.clone()]));
    let document = edit_graph::of_graph(&output);
    check(
        edit_graph::node_slot_names(&document, node::id(&middle)) == Some(vec!["in0".to_owned()])
            && edit_graph::node_slot_names(&document, -1).is_none(),
        "editable entry did not expose canonical slots or reject a missing node",
    );
    check(
        edit_graph::ptr_eq(&get(edit_graph::replace_node(&middle, &document)), &document)
            && edit_graph::ptr_eq(&get(edit_graph::apply_parameters(&document, node::id(&middle), &[])).0, &document),
        "unchanged parameter writes rebuilt the editable graph",
    );
    check(
        edit_graph::root(&document) == Some(node::id(&output)) && edit_graph::inspect(&document).len() == 3,
        "editable graph did not import the compiled DAG",
    );
    let disconnected = get(edit_graph::disconnect(node::id(&middle), 0, &document));
    check(
        edit_graph::compile(&disconnected).is_err() && edit_graph::compile_node(&disconnected, node::id(&source)).is_ok(),
        "editable graph did not retain a cookable branch around a disconnected slot",
    );
    let reconnected = get(edit_graph::connect(node::id(&source), node::id(&middle), 0, &disconnected));
    check(edit_graph::compile(&reconnected).is_ok(), "editable graph did not compile after reconnecting its slot");
    check(
        edit_graph::connect(node::id(&middle), node::id(&middle), 0, &reconnected).is_err(),
        "editable graph accepted a self cycle",
    );
    let compiled = edit_graph::compile_all(None, &reconnected);
    let node_of = |c: &edit_graph::Compiled, id: i64| get(edit_graph::compiled_node(c, id));
    let again = edit_graph::compile_all(Some(&compiled), &reconnected);
    check(
        node::ptr_eq(&node_of(&again, node::id(&output)), &node_of(&compiled, node::id(&output)))
            && node::id(&node_of(&again, node::id(&output))) == node::id(&get(edit_graph::compile(&reconnected))),
        "incremental compile rebuilt an unchanged document",
    );
    let rewired = get(edit_graph::disconnect(node::id(&middle), 0, &reconnected));
    check(
        edit_graph::compiled_node(&edit_graph::compile_all(Some(&compiled), &rewired), node::id(&output)).is_err(),
        "incremental compile reused a node across a disconnected input",
    );
    let rewired = get(edit_graph::connect(node::id(&source), node::id(&middle), 0, &rewired));
    let recompiled = edit_graph::compile_all(Some(&compiled), &rewired);
    check(
        node::ptr_eq(&node_of(&recompiled, node::id(&source)), &node_of(&compiled, node::id(&source)))
            && node::id(&node_of(&recompiled, node::id(&output))) == node::id(&output),
        "incremental compile did not reuse the unchanged upstream node",
    );
    let removed = edit_graph::remove_nodes(&[node::id(&source)], &reconnected);
    check(
        edit_graph::find(&removed, node::id(&source)).is_none()
            && edit_graph::inputs(&removed, node::id(&middle)) == Some(vec![None])
            && edit_graph::compile(&removed).is_err(),
        "editable graph node deletion did not leave an explicit disconnected slot",
    );
    let box_source = source.clone();
    let box_factory = edit_graph::factory(
        edit_graph::factory_args("box", "Box", &["Create"]),
        0,
        Arc::new(move |_| Ok(box_source.clone())),
    );
    check(
        edit_graph::factory_arity(&box_factory) == 0 && edit_graph::instantiate(&box_factory, &[]).is_ok(),
        "editable graph factory did not instantiate its declared arity",
    );
    check(
        edit_graph::factory_slot_names(&null) == ["in0"],
        "unnamed factory inputs did not use canonical port names",
    );
    let loose_null = get(edit_graph::instantiate_optional(&null, &[None]));
    let loose_document = get(edit_graph::add_node(Some(&[None]), None, &loose_null, &document));
    check(
        edit_graph::inputs(&loose_document, node::id(&loose_null)) == Some(vec![None]),
        "factory could not create a node with a disconnected input",
    );
    let match_size_factory = edit_graph::factory_slots(
        edit_graph::FactoryArgs {
            slots: Some(vec!["input".to_owned(), "target".to_owned()]),
            ..edit_graph::factory_args("match_size", "Match Size", &["Modify"])
        },
        vec![Required, Optional],
        Arc::new(|inputs| match inputs {
            [Some(input), None] => Ok(merge_of(vec![input.clone()])),
            [Some(input), Some(target)] => Ok(merge_of(vec![input.clone(), target.clone()])),
            _ => Err("Match Size requires its geometry input".to_owned()),
        }),
    );
    check(
        edit_graph::factory_slot_names(&match_size_factory) == ["input", "target"],
        "optional factory did not retain named input slots",
    );
    let loose_match = get(edit_graph::instantiate_optional(&match_size_factory, &[None, None]));
    let optional_document = get(edit_graph::add_node(Some(&[None, None]), Some(&match_size_factory), &loose_match, &document));
    let optional_document = get(edit_graph::connect(node::id(&source), node::id(&loose_match), 0, &optional_document));
    let without_target = get(edit_graph::compile_node(&optional_document, node::id(&loose_match)));
    check(
        edit_graph::node_slot_names(&optional_document, node::id(&loose_match)) == Some(vec!["input".to_owned(), "target".to_owned()]),
        "editable entry lost its factory's named slots",
    );
    check(
        node::inputs(&without_target).len() == 1 && edit_graph::factory_inputs(&match_size_factory) == [Required, Optional],
        "optional factory slot was treated as a disconnected required input",
    );
    let optional_document = get(edit_graph::connect(node::id(&middle), node::id(&loose_match), 1, &optional_document));
    let with_target = get(edit_graph::compile_node(&optional_document, node::id(&loose_match)));
    check(
        node::id(&with_target) == node::id(&loose_match) && node::inputs(&with_target).len() == 2,
        "connecting an optional slot did not rebuild the physical SOP inputs",
    );
    let optional_document = get(edit_graph::disconnect(node::id(&loose_match), 1, &optional_document));
    let without_again = get(edit_graph::compile_node(&optional_document, node::id(&loose_match)));
    check(
        node::id(&without_again) == node::id(&loose_match) && node::inputs(&without_again).len() == 1,
        "disconnecting an optional slot did not restore the unary SOP",
    );
    // a rest slot grows by connecting one past its last input
    let rest_factory = edit_graph::factory_slots(
        edit_graph::FactoryArgs {
            slots: Some(vec!["input".to_owned()]),
            ..edit_graph::factory_args("merge", "Merge", &["Test"])
        },
        vec![Rest],
        Arc::new(|inputs| Ok(merge_of(nodes(inputs)))),
    );
    check(
        edit_graph::instantiate_optional(&rest_factory, &[]).is_err()
            && edit_graph::instantiate_optional(&rest_factory, &[Some(source.clone()), None, Some(middle.clone())]).is_ok()
,
        "rest slot arity or position was not validated",
    );
    let rest_node = get(edit_graph::instantiate_optional(&rest_factory, &[Some(source.clone())]));
    let rest_document = get(edit_graph::add_node(Some(&[None]), Some(&rest_factory), &rest_node, &document));
    let grow = |index: i64, source_node: &node::T, d: &edit_graph::T| {
        edit_graph::connect(node::id(source_node), node::id(&rest_node), index, d)
    };
    let rest_document = get(grow(0, &source, &rest_document));
    let rest_document = get(grow(1, &middle, &rest_document));
    let rest_document = get(grow(2, &source, &rest_document));
    check(
        edit_graph::node_slot_names(&rest_document, node::id(&rest_node))
            == Some(vec!["input".to_owned(), "input_2".to_owned(), "input_3".to_owned()])
            && grow(4, &source, &rest_document).is_err()
            && node::inputs(&get(edit_graph::compile_node(&rest_document, node::id(&rest_node)))).len() == 3,
        "rest slot did not grow, name its extras or rebuild through the factory",
    );
    let stored_rest = edit_graph::find(&rest_document, node::id(&rest_node)).unwrap();
    check(
        node::ptr_eq(&get(edit_graph::compile_node(&rest_document, node::id(&rest_node))), &stored_rest),
        "compiling unchanged connected rest inputs rebuilt the stored node",
    );
    let rest_document = get(edit_graph::disconnect(node::id(&rest_node), 1, &rest_document));
    check(
        node::inputs(&get(edit_graph::compile_node(&rest_document, node::id(&rest_node)))).len() == 2,
        "a disconnected rest input was not skipped",
    );
    let optional_rest_factory = edit_graph::factory_slots(
        edit_graph::FactoryArgs {
            slots: Some(vec!["base".to_owned(), "fixed".to_owned(), "extras".to_owned()]),
            ..edit_graph::factory_args("optional_rest", "Optional rest", &["Test"])
        },
        vec![Required, Optional, OptionalRest],
        Arc::new(|inputs| Ok(merge_of(nodes(inputs)))),
    );
    check(
        edit_graph::instantiate_optional(&optional_rest_factory, &[Some(source.clone())]).is_err()
            && edit_graph::instantiate_optional(&optional_rest_factory, &[Some(source.clone()), None]).is_ok()
            && edit_graph::instantiate_optional(&optional_rest_factory, &[Some(source.clone()), None, None]).is_ok(),
        "optional rest validates the fixed prefix and permits zero extras",
    );
    let optional_rest_node = get(edit_graph::instantiate_optional(&optional_rest_factory, &[Some(source.clone()), None]));
    let doc = get(edit_graph::add_node(
        Some(&[Some(node::id(&source)), None]),
        Some(&optional_rest_factory),
        &optional_rest_node,
        &document,
    ));
    let doc = get(edit_graph::connect(node::id(&middle), node::id(&optional_rest_node), 2, &doc));
    let doc = get(edit_graph::connect(node::id(&source), node::id(&optional_rest_node), 3, &doc));
    check(
        edit_graph::node_slot_names(&doc, node::id(&optional_rest_node))
            == Some(["base", "fixed", "extras", "extras_2"].iter().map(|s| (*s).to_owned()).collect())
            && node::inputs(&get(edit_graph::compile_node(&doc, node::id(&optional_rest_node)))).len() == 3,
        "optional rest grows after a disconnected fixed slot",
    );
    let doc = get(edit_graph::disconnect(node::id(&optional_rest_node), 2, &doc));
    let doc = get(edit_graph::disconnect(node::id(&optional_rest_node), 3, &doc));
    let compiled_rest = get(edit_graph::compile_node(&doc, node::id(&optional_rest_node)));
    check(
        node::id(&compiled_rest) == node::id(&optional_rest_node) && node::inputs(&compiled_rest).len() == 1,
        "optional rest disconnect preserves identity and the fixed prefix",
    );
}

#[test]
fn declared_factories_follow_their_metadata() {
    let switch = crate::shapes::switch::factory();
    assert_eq!(edit_graph::factory_inputs(&switch), [Required, Required, OptionalRest]);
    assert_eq!(edit_graph::factory_slot_names(&switch), ["a", "b", "inputs"]);
    let (a, b) = (snapshot(&fixture()), snapshot(&fixture()));
    let built = edit_graph::instantiate_optional(&switch, &[Some(a.clone()), Some(b.clone()), None]).unwrap();
    assert_eq!(node::inputs(&built).len(), 2);
    assert_eq!(node::operation(&built), "switch");
    // the choice lists the connected branches
    let fields = node::parameter_fields(&built);
    assert_eq!(fields.len(), 1);
    match &fields[0].kind {
        param::KindView::Choice(labels) => assert_eq!(labels.len(), 2),
        _ => panic!("switch input must be a choice"),
    }
}

fn blank_slots(slots: Vec<String>) {
    let probe = snapshot(&fixture());
    edit_graph::factory(
        edit_graph::FactoryArgs { slots: Some(slots), ..edit_graph::factory_args("invalid", "Invalid", &["Test"]) },
        1,
        Arc::new(move |_| Ok(probe.clone())),
    );
}

#[test]
#[should_panic(expected = "one distinct name per input")]
fn factory_refuses_no_slot_names() {
    blank_slots(vec![]);
}

#[test]
#[should_panic(expected = "one distinct name per input")]
fn factory_refuses_blank_slot_name() {
    blank_slots(vec![String::new()]);
}

#[test]
#[should_panic(expected = "one distinct name per input")]
fn factory_refuses_duplicate_slot_names() {
    blank_slots(vec!["a".to_owned(), "a".to_owned()]);
}

#[test]
#[should_panic(expected = "only the last input may be Rest")]
fn rest_slot_must_be_last() {
    let probe = snapshot(&fixture());
    edit_graph::factory_slots(
        edit_graph::factory_args("bad", "Bad", &["T"]),
        vec![Rest, Required],
        Arc::new(move |_| Ok(probe.clone())),
    );
}
