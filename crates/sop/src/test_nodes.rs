//! Tests of the declared nodes (the worked examples of PORTING.md): construction through the
//! factory, parameter edits that rebuild the operator, and cooking through a session.
use param::Value;

use crate::edit_graph;
use crate::groups;
use crate::node;
use crate::payload;
use crate::shapes;
use crate::test_support::*;

#[test]
fn null_passes_its_input_through() {
    let g = fixture();
    let n = edit_graph::instantiate(&shapes::null::factory(), &[snapshot(&g)]).unwrap();
    assert_eq!(node::operation(&n), "null");
    assert_eq!(node::label(&n), "null");
    assert!(node::has_parameters(&n) && node::parameter_fields(&n).is_empty());
    let mut s = session(8);
    let out = geometry_of(&cook(&mut s, 1, &n));
    assert!(same_geometry(&out, &g));
    assert!(edit_graph::instantiate(&shapes::null::factory(), &[]).is_err());
    crate::session::close(&mut s);
}

#[test]
fn switch_selects_a_branch_and_rebuilds_on_edit() {
    let (a, b) = (with_color(1.0, &fixture()), with_color(2.0, &fixture()));
    let n = edit_graph::instantiate_optional(&shapes::switch::factory(), &[Some(snapshot(&a)), Some(snapshot(&b)), None]).unwrap();
    let mut s = session(8);
    assert!(same_geometry(&geometry_of(&cook(&mut s, 1, &n)), &a));
    let (edited, effects) = node::apply_parameters(&n, &[("input".to_owned(), Value::Int(1))]).unwrap();
    assert!(effects.cook);
    assert_eq!(node::id(&edited), node::id(&n));
    assert!(same_geometry(&geometry_of(&cook(&mut s, 1, &edited)), &b));
    // the choice only lists the connected branches
    assert!(node::apply_parameters(&n, &[("input".to_owned(), Value::Int(5))]).is_err());
    // the edited node's parameter text reaches the cache key: the two cooks did not collide
    assert_ne!(node::parameter_key(&n), node::parameter_key(&edited));
    crate::session::close(&mut s);
}

#[test]
fn switch_input_policy_skips_unselected_branches() {
    let tally = calls();
    let skipped = counted(&snapshot(&fixture()), &tally, &["P"], &[], identity());
    let kept = snapshot(&fixture());
    let n = edit_graph::instantiate_optional(&shapes::switch::factory(), &[Some(skipped), Some(kept), None]).unwrap();
    let (n, _) = node::apply_parameters(&n, &[("input".to_owned(), Value::Int(1))]).unwrap();
    let mut s = session(8);
    cook(&mut s, 1, &n);
    assert_eq!(count(&tally), 0, "an unselected switch input was cooked");
    crate::session::close(&mut s);
}

#[test]
fn group_transfer_declares_its_schema_and_cooks() {
    let factory = groups::group_transfer::factory();
    let fields = edit_graph::factory_fields(&factory);
    assert_eq!(fields.iter().map(|f| f.name.as_str()).collect::<Vec<_>>(), ["use_rules", "rules", "conflict", "create_empty", "distance"]);
    let (source, target) = (fixture(), fixture());
    let n = edit_graph::instantiate(&factory, &[snapshot(&source), snapshot(&target)]).unwrap();
    let mut s = session(8);
    let out = cook(&mut s, 1, &n);
    assert!(payload::geometry(&out.payload).is_ok());
    let (edited, effects) = node::apply_parameters(&n, &[("distance".to_owned(), Value::Float(2.0))]).unwrap();
    assert!(effects.cook && node::id(&edited) == node::id(&n));
    let values = groups::group_transfer::to_values(&groups::group_transfer::from_values(&[
        Value::Bool(true),
        Value::Text(groups::group_transfer::encode(&groups::default_transfer_rules())),
        crate::support::GROUP_COPY_CONFLICT.value(&rdk_ops::group_ops::CopyConflict::CopySkip),
        Value::Bool(false),
        Value::Float(0.5),
    ]));
    assert_eq!(values[4], Value::Float(0.5));
    assert!(matches!(&values[1], Value::Text(t) if !t.is_empty()));
    crate::session::close(&mut s);
}

#[test]
fn registry_lists_the_ported_factories_in_order() {
    let keys: Vec<String> = crate::nodes::factories().iter().map(|f| edit_graph::factory_key(f).to_owned()).collect();
    // The registry is shared by every wave, so other waves' keys may interleave: the attribute
    // wave's keys must appear in OCaml order.
    let mut rest = keys.iter();
    for key in [
        "image_texels",
        "distance_from_geometry",
        "uv_flatten",
        "uv_relax",
        "attribute_mirror",
        "attribute_transfer",
        "attribute_transfer_surface",
        "attribute_transfer_all",
    ] {
        assert!(rest.any(|k| k == key), "{key} is missing or out of OCaml order in {keys:?}");
    }
}

#[test]
fn choice_tables_round_trip_their_values() {
    use crate::support::*;
    for (_, mode) in KERNEL_MODE.0 {
        assert_eq!(KERNEL_MODE.of(&KERNEL_MODE.value(mode)), *mode);
    }
    for (_, owner) in GROUP_OWNER.0 {
        assert_eq!(GROUP_OWNER.of(&GROUP_OWNER.value(owner)), *owner);
        assert_eq!(group_owner_of_token(group_owner_token(*owner)), Ok(*owner));
    }
    for (_, owner) in ATTRIBUTE_OWNER.0 {
        assert_eq!(ATTRIBUTE_OWNER.of(&ATTRIBUTE_OWNER.value(owner)), *owner);
    }
}

#[test]
fn curve_join_picks_use_decimal_i64_indices() {
    use crate::support::decode_curve_join_picks;
    let picks = decode_curve_join_picks(" 9223372036854775807:start ").unwrap().unwrap();
    assert_eq!(picks[0].primitive, i64::MAX);
    for input in ["0x1F:start", "1_000:start", "-1:start", "9223372036854775808:start"] {
        assert!(decode_curve_join_picks(input).is_err(), "accepted invalid primitive {input}");
    }
}

#[test]
fn async_cook_publishes_the_newest_request_and_refuses_after_close() {
    use crate::async_cook::{self, Prepared};
    let worker = async_cook::create(8, 4_000_000).unwrap();
    let n = snapshot(&fixture());
    let id = async_cook::submit(
        &worker,
        context(1),
        n.clone(),
        Box::new(|output| Ok(Box::new(payload::element_count(&output.payload)) as Prepared)),
    )
    .unwrap();
    let done = async_cook::await_completion(&worker);
    assert_eq!(done.request_id, id);
    let points = done.result.ok().expect("cook succeeded").downcast::<i64>().expect("prepared an i64");
    assert_eq!(*points, 9);
    assert_eq!(async_cook::status(&worker), async_cook::Status::Idle);
    assert_eq!(async_cook::stats(&worker).cooks, 1);
    // a failing prepare is a Prepare_error, not a lost request
    async_cook::submit(&worker, context(1), n.clone(), Box::new(|_| Err("no".to_owned()))).unwrap();
    match async_cook::await_completion(&worker).result {
        Err(error) => assert_eq!(async_cook::error_to_string(&error), "no"),
        Ok(_) => panic!("prepare failure was lost"),
    }
    async_cook::close(&worker);
    assert!(async_cook::submit(&worker, context(1), n, Box::new(|_| Err("late".to_owned()))).is_err());
}
