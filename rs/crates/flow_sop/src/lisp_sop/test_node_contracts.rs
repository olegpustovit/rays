//! Ports of lib/flow_sop/test_noise_displace_contract.ml and test_merge_source.ml.
use super::support::*;
use crate::{catalog, lisp_sop, lower, manifest, operators};
use rdk::geometry as geo;
use sop::context::{self as sctx, Options as ContextOptions};
use sop::edit_graph as edit;
use sop::node;
use sop::session;
use std::sync::Arc;

fn line_points(points: &[(f64, f64, f64)]) -> geo::T {
    rdk::line_geometry::points(points).unwrap_or_else(|e| panic!("{}", rdk::error::to_string(&e)))
}

fn contract_geometry() -> geo::T {
    let points: Vec<(f64, f64, f64)> = (0..129).map(|i| ((i as f64) * 0.13 - 0.4, ((i as f64) * 0.27).sin(), ((i / 65) as f64) * 0.19 - 0.3)).collect();
    line_points(&points)
}

/// The node's parameter contract, checked on its factory node: noise_displace over an input
/// factory, the library node lowered as that node.
fn noise_node(input: node::T, keywords: &str) -> node::T {
    let factory = edit::factory(edit::factory_args("contract_input", "Contract input", &["Test"]), 0, Arc::new(move |_| Ok(input.clone())));
    let mut factories = vec![factory];
    factories.extend(sop::nodes::factories().into_iter().filter(|f| edit::factory_key(f) != "noise_displace"));
    let cat = catalog::of_factories(manifest::VERSION, &[], Some(&[]), &factories).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let forms = parse(&format!("(workspace noise-contract (graph g :context sop (sop/noise_displace (sop/contract_input) {keywords})))"));
    let sources = catalog::sources().unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let checked = match catalog::check_workspace(operators::all(), false, Some(sources), &cat, &forms) {
        (Some(w), diagnostics) if !diagnostics.iter().any(|d| d.severity == rays_diagnostic::Severity::ErrorSeverity) => w,
        (_, diagnostics) => panic!("{}", diagnostics.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")),
    };
    let lowered = lower::of_checked(Some(factories), lower::Options::default(), &checked).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    lisp_sop::root(&lowered, &lowered.graphs[0]).unwrap()
}

fn context(domains: i64, seed: i64) -> sctx::T {
    sctx::create(ContextOptions { seed: Some(seed), domains: Some(domains), grain: Some(97), ..Default::default() }).unwrap()
}

fn capture_once(max_entries: i64, context: &sctx::T, n: &node::T) -> geo::T {
    let mut s = session::create(max_entries, 1_000_000).unwrap();
    let g = cook(&mut s, context, n);
    session::close(&mut s);
    g
}

#[test]
fn noise_displace_sop_cache_context_seed_dependency_and_relabel_identity() {
    let input = lisp_sop::snapshot(&contract_geometry());
    let plain = noise_node(input, "");
    let (seeded, _) = node::apply_parameters(&plain, &[("context_seed".to_owned(), param::Value::Bool(true))]).unwrap();
    let mut s = session::create(16, 1_000_000).unwrap();
    let (c42, c999) = (context(1, 42), context(1, 999));
    let first = cook(&mut s, &c42, &plain);
    assert!(same(&cook(&mut s, &c42, &plain), &first));
    assert!(geometry_bytes(&capture_once(0, &c42, &plain)) == geometry_bytes(&first));
    let c42_four = context(4, 42);
    assert!(geometry_bytes(&capture_once(0, &c42_four, &seeded)) == geometry_bytes(&capture_once(16, &c42_four, &seeded)));
    let misses = session::stats(&s).misses;
    assert!(same(&cook(&mut s, &c999, &plain), &first));
    assert_eq!(session::stats(&s).misses, misses);
    let seeded42 = cook(&mut s, &c42, &seeded);
    let misses = session::stats(&s).misses;
    let seeded999 = cook(&mut s, &c999, &seeded);
    assert_eq!(session::stats(&s).misses, misses + 1);
    assert!(geometry_bytes(&seeded42) != geometry_bytes(&seeded999));
    let relabeled = node::relabel("déplacement 雪", &seeded);
    let restored = node::relabel(node::label(&seeded), &relabeled);
    assert_eq!(node::id(&relabeled), node::id(&seeded));
    for (context, expected) in [(&c42, &seeded42), (&c999, &seeded999)] {
        let misses = session::stats(&s).misses;
        assert!(same(&cook(&mut s, context, &relabeled), expected));
        assert!(same(&cook(&mut s, context, &restored), expected));
        assert_eq!(session::stats(&s).misses, misses);
    }
    session::close(&mut s);
}

/// The original two-merge implementation remains the independent oracle for fused provenance.
#[test]
fn lisp_source_merge_domains_inputs_and_cancellation() {
    use rdk::{attribute::{self,Owner,Storage},mesh_merge};
    let geometry=cook_once(1,&lisp_sop::node(&[],"(sop/box :size [1.0 2.0 3.0] :x_divisions 2 :y_divisions 2 :z_divisions 2 :face_groups \"face\")"));
    let count=geo::primitive_count(&geometry)as usize;let tag=lower::source_attribute();
    let add=|name:&str,storage:Storage|geo::with_attribute(attribute::create_owned(name,Owner::Primitive,storage).unwrap(),&geometry).unwrap();
    let named=add("src",Storage::Int(vec![73;count]));let tagged=add(tag,Storage::Int(vec![42;count]));let float_named=add("src",Storage::Float(vec![0.25;count]));
    let cases=vec![
        ("src",vec![geometry.clone()]),("src",vec![geometry.clone(),geometry.clone()]),
        ("src",vec![tagged.clone(),tagged.clone()]),("src",vec![tagged.clone(),geometry.clone()]),
        ("src",vec![geometry.clone(),tagged]),("src",vec![named.clone(),named.clone()]),
        ("src",vec![named.clone(),float_named.clone()]),("src",vec![float_named.clone(),float_named]),
        ("src",vec![named,geometry.clone()]),(tag,vec![geometry.clone(),geometry.clone()]),
        ("src",vec![rdk::line_geometry::points(&[]).unwrap()]),
    ];
    for(attribute,geometries)in cases{
        let originals=geometries.iter().map(geometry_bytes).collect::<Vec<_>>();
        let run=|name:&str,base|mesh_merge::run(&mesh_merge::RunParams{cancel:None,grain:None,source_attribute:Some(name.into()),source_base:Some(base)},&geometries);
        let expected=run(tag,0).and_then(|tagged|run(attribute,10).and_then(|named|{
            match geo::find_attribute(Owner::Primitive,attribute,&named){
                Some(values)if geo::find_attribute(Owner::Primitive,attribute,&tagged).is_none()=>geo::with_attribute(values,&tagged),
                _=>Ok(tagged)
            }
        }));
        let with_=geometries.iter().enumerate().map(|(i,g)|(format!("i{i}"),lisp_sop::snapshot(g))).collect::<Vec<_>>();
        let merged=lisp_sop::node(&with_,&format!("(sop/merge {} :source_attribute {attribute:?} :source_base 10)",(0..geometries.len()).map(|i|format!("(sop/ext_i{i})")).collect::<Vec<_>>().join(" ")));
        for domains in [1,8]{let mut s=new_session(32,268_435_456);let actual=try_cook(&mut s,&context(domains,97),&merged);
            match(&expected,actual){(Ok(expected),Ok(actual))=>assert_eq!(geometry_bytes(expected),geometry_bytes(&actual),"source merge changed geometry/tags/order"),(Err(_),Err(d))=>assert_eq!(d.code,"schema_mismatch"),_=>panic!("source merge changed success/refusal")}
            session::close(&mut s);
        }
        assert_eq!(geometries.iter().map(geometry_bytes).collect::<Vec<_>>(),originals);
    }
    let cancel=Arc::new(rdk::cancel::create());rdk::cancel::cancel(&cancel);
    let c=sctx::create(ContextOptions{domains:Some(1),cancel:Some(cancel),..Default::default()}).unwrap();
    let with_=vec![("i0".into(),lisp_sop::snapshot(&geometry)),("i1".into(),lisp_sop::snapshot(&geometry))];
    let merged=lisp_sop::node(&with_,"(sop/merge (sop/ext_i0) (sop/ext_i1) :source_attribute \"src\" :source_base 10)");
    let mut s=new_session(32,268_435_456);assert!(try_cook(&mut s,&c,&merged).is_err());session::close(&mut s);
}

/// Physical identity of a cooked geometry (OCaml `==`): the same payload object.
fn same(a: &geo::T, b: &geo::T) -> bool {
    geo::data_id(a) == geo::data_id(b)
}
