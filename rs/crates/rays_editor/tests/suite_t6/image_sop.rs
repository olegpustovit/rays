//! test_image_sop.ml: the mixed image SOP `attr_from_image`.
#![allow(dead_code, unused_imports)]
use std::sync::Arc;

use editor_document::kinds;
use editor_document::workspace_doc as wd;
use flow_graph::flow_edit::ArgKey;
use flow_graph::flow_edit::Op;
use flow_graph::projection as P;
use flow_syntax::syntax::{self as S, Node};
use flow_syntax::ty::Ty;
use sop::edit_graph;
use sop::{context, session};

const SOURCE: &str = r#"(workspace sample
  (graph img :context image (image/noise :width 2 :height 2 :seed 9))
  (graph g :context sop
    (let* [picture (ref img)
           mesh (sop/grid :columns 1 :rows 1 :uv_attribute "uv")
           result (sop/attr_from_image mesh picture :attribute "sample" :channel "a")]
      result)))"#;

fn load(catalog: &flow_check::catalog::Catalog, text: &str) -> wd::T {
    wd::of_text(&wd::TextOptions::default(), catalog, text).unwrap_or_else(|ds| panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")))
}

fn diagnostics(catalog: &flow_check::catalog::Catalog, text: &str) -> Vec<rays_diagnostic::Diagnostic> {
    flow_check::workspace::check(&[], false, false, None, catalog, &S::parse(text).unwrap()).1
}

pub fn image_sop() {
    let factories = sop::nodes::factories();
    let catalog = kinds::catalog(1, &factories).unwrap();
    let all = flow_sop::catalog::with_library(Vec::new());
    let kind = all.iter().find(|d| d.key == "attr_from_image").expect("attr_from_image");
    assert!(kind.slot_types == ["geometry", "image"]);
    assert!(kind.slots.iter().map(|s| s.0.as_str()).collect::<Vec<_>>() == ["geometry", "image"]);
    for input_types in [vec![], vec!["image".to_owned(), "image".to_owned()], vec!["bad type".to_owned()]] {
        let r = std::panic::catch_unwind(|| {
            let mut args = edit_graph::factory_args("typed", "Typed", &["Test"]);
            args.input_types = Some(input_types.clone());
            edit_graph::factory(args, 1, Arc::new(|inputs| Ok(inputs[0].clone())))
        });
        assert!(r.is_err());
    }
    let mut args = edit_graph::factory_args("bad_typed", "Bad Typed", &["Test"]);
    args.input_types = Some(vec!["missing_type".to_owned()]);
    let bad = edit_graph::factory(args, 1, Arc::new(|inputs| Ok(inputs[0].clone())));
    assert!(flow_sop::catalog::of_factories(1, &[], None, &[bad]).is_err());
    let manifest = if std::env::var("RAYS_SLOW").as_deref() == Ok("1") { flow_sop::manifest::generate(&kinds::descriptors(), None, &factories).unwrap().0 } else { crate::support::read_repo("lib/flow_sop/flow_manifest.sexp") }; // regenerating the whole library takes minutes at -O0: the checked-in manifest is the same text
    let (parsed, _) = flow_check::catalog::catalog_of_manifest(&manifest).unwrap();
    let kind_of = |c: &flow_check::catalog::Catalog| c.kinds.iter().find(|k| k.qualified == "sop/attr_from_image").unwrap().clone();
    assert!(kind_of(&parsed).slots == kind_of(&catalog).slots);
    assert!(kind_of(&parsed).slots[1].ty == Some(flow_syntax::ty::image()));
    assert!(!parsed.kinds.iter().find(|k| k.qualified == "sop/copy_to_points").unwrap().slots.iter().any(|s| s.ty.is_some()));
    let document = load(&catalog, SOURCE);
    let scope = P::of_graph(&catalog, &document.checked, "g").unwrap();
    let node = P::find(&scope, &["g".to_owned(), "result".to_owned()]).unwrap();
    assert!(node.rows[0].ty == Some(flow_syntax::ty::geometry()) && node.rows[0].head);
    assert!(node.rows[1].ty == Some(flow_syntax::ty::image()) && !node.rows[1].head);
    let wrong = r#"(workspace wrong
    (graph img :context image (image/noise :width 1 :height 1))
    (graph g :context sop (sop/attr_from_image (ref img) (sop/grid))))"#;
    assert!(diagnostics(&catalog, wrong).iter().any(|d| d.code == "E_TYPE"));
    assert!(diagnostics(&catalog, "(workspace missing (graph g :context sop (sop/attr_from_image (sop/grid))))").iter().any(|d| d.code == "E_MISSING_INPUT"));
    let result = vec!["g".to_owned(), "result".to_owned()];
    for gesture in [
        Op::Connect { node: result.clone(), key: ArgKey::Pos(1), src: "picture".into(), iter: false },
        Op::SetArg { node: result.clone(), key: ArgKey::Kw("channel".into()), sub: Vec::new(), value: S::make(Node::Str(b"b".to_vec())) },
        Op::Disconnect { node: result.clone(), key: ArgKey::Pos(1), fallback: Some(S::make(Node::List(vec![S::make(Node::Sym("ref".into())), S::make(Node::Sym("img".into()))]))) },
    ] {
        wd::edit(&catalog, &document, &gesture).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    }
    let fallback = S::parse(node.rows[1].default.as_ref().unwrap()).unwrap().remove(0);
    let disconnected = wd::edit(&catalog, &document, &Op::Disconnect { node: result.clone(), key: ArgKey::Pos(1), fallback: Some(fallback) }).unwrap();
    assert!(P::find(&P::of_graph(&catalog, &disconnected.checked, "g").unwrap(), &["g".to_owned(), "result#1".to_owned()]).is_some());
    let lowered = flow_sop::lower::workspace(Some(factories.clone()), kinds::descriptors(), flow_sop::lower::Options::default(), &document.source).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let graph = lowered.graphs.iter().find(|g| g.name == "g" && g.default).unwrap();
    let node = edit_graph::compile_node(&graph.network.geometry, graph.root.unwrap()).unwrap();
    let mut s = session::create(16, 65536).unwrap();
    let out = session::cook(&mut s, None, &context::create(Default::default()).unwrap(), &node).unwrap_or_else(|d| panic!("{}", rays_diagnostic::error_to_string(&d)));
    let geometry = sop::payload::geometry(&out.payload).unwrap();
    let attr = rdk::geometry::find_attribute(rdk::attribute::Owner::Point, "sample", &geometry).expect("sample attribute");
    assert!(rdk::geometry::point_count(&geometry) == 4 && rdk::attribute::length(&attr) == 4);
    let values = rdk::attribute::get(&rdk::attribute::key("sample", rdk::attribute::Owner::Point, rdk::attribute::float()), &attr).unwrap();
    match values { Some(rdk::attribute::Storage::Float(v)) => assert!(v.iter().all(|x| *x == 1.)), _ => panic!("sample is not a float attribute") }
    session::close(&mut s);
}
