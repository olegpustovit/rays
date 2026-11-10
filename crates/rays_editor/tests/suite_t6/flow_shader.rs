//! , adapted to the Rust image-kernel API (`image_kernel::prepare` takes the map's function id).
#![allow(dead_code, unused_imports)]
use flow_syntax::symbol::Symbol;
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;
use std::sync::Arc;

use flow_graph::probe;
use flow_graph::projection as P;
use flow_syntax::value::Value;
use rays_editor::navigator as N;
use sop::{context, session};

use super::super::support::*;

pub fn flow_shader() {
    let text = read_repo("sketches/flow_shader.rays");
    let doc = load(&text);
    let checked = doc.checked.clone();
    let params = N::Params {
        workspace: checked.clone(),
        active: Some("picture".into()),
        scope: None,
        records: None,
        probes: Rc::new(|_| 0),
        selected: Vec::new(),
        chips: Vec::new(),
        objects: Vec::new(),
        root_detail: String::new(),
        layouts: None,
        notes: Vec::new(),
    };
    let outline: Vec<String> = N::rows(false, &N::initial(), &params).iter().map(N::describe).collect();
    assert!(outline.iter().any(|r| r == "DEFINITIONS \u{b7} used"), "{outline:?}");
    assert!(!outline.iter().any(|r| r.starts_with("GEOMETRY")));
    assert!(outline.iter().any(|r| r.starts_with("\u{3bb} grad \u{b7} 1 use")), "{outline:?}");
    let catalog = rays_editor::workspace_catalog(None).unwrap();
    let scope = P::of_graph(&catalog, &checked, "picture").unwrap();
    assert!(scope.nodes.len() == 9);
    let panels = ["amber", "comet", "lime", "velvet", "ember", "echoes"];
    for name in panels {
        assert!(P::find(&scope, &["picture".into(), name.into()]).is_some(), "{name}");
    }
    let amber = P::find(&scope, &["picture".into(), "amber".into()]).unwrap();
    assert!(amber.macro_.as_deref() == Some("panel") && amber.rows.iter().any(|r| r.kind == P::RowKind::Hole && r.label == "bands"));
    let level = {
        let l = doc.layout.level.clone();
        Rc::new(move |p: &[Symbol]| l.get(p).copied().unwrap_or(P::Level::Card))
    };
    let collapsed = {
        let c = doc.layout.collapsed.clone();
        Rc::new(move |p: &[Symbol]| c.get(p).copied().unwrap_or(false))
    };
    let layout = P::layout(&P::LayoutOptions { level, collapsed, ..Default::default() }, &scope);
    assert!(layout.h < 800. && layout.w < 1000., "{}x{}", layout.w, layout.h);
    let evaluated = flow_eval::static_with(&checked, true, None, None).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
    let pr = probe::make(&evaluated, probe::Options::default());
    for name in panels {
        assert!(pr.plan_node(Some(&flow_syntax::ty::drawing()), &["picture".into(), name.into()], &[]).is_some(), "{name}");
    }
    let images: Vec<&flow_eval::Node> = evaluated.plan.nodes.iter().filter(|n| n.kind == "image/map").collect();
    let sites: HashSet<_> = images.iter().map(|n| (n.inst, n.site.clone(), n.iter.clone())).collect();
    assert!(images.len() == 25 && sites.len() == 25);
    let mut s = session::create(0, 0).unwrap();
    let samples: Vec<(Vec<u8>, Vec<u8>)> = images
        .iter()
        .map(|image| {
            let f = match image.args.iter().find(|(k, _)| k == "function").map(|(_, v)| v) {
                Some(Value::Fn(f)) => *f,
                other => panic!("{other:?}"),
            };
            let kernel = flow_sop::image_kernel::prepare(None, None, None, image.id, 5, 5, f, &[], &[]).unwrap_or_else(|d| panic!("{}", rays_diagnostic::to_string(&d)));
            let mut sample = |time: f64| {
                let c = context::create(context::Options { time: Some(time), domains: Some(1), ..Default::default() }).unwrap();
                let out = session::cook(&mut s, None, &c, &flow_sop::image_kernel::node(None, &kernel)).unwrap_or_else(|e| panic!("{}", rays_diagnostic::error_to_string(&e)));
                let pixels = sop::payload::image(&out.payload).unwrap();
                sop::image::rgba8(&pixels).unwrap().to_vec()
            };
            (sample(0.), sample(1.))
        })
        .collect();
    session::close(&mut s);
    let mut backgrounds: Vec<&Vec<u8>> = [0, 4, 8, 12, 16, 21].iter().map(|i| &samples[*i].0).collect();
    backgrounds.sort();
    backgrounds.dedup();
    assert!(backgrounds.len() == 4, "{} distinct backgrounds", backgrounds.len());
    assert!(samples.iter().filter(|(a, b)| a != b).count() >= 12);
}
