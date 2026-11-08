//! test_workspace_ir.ml with the static part of workspace_parity.ml: for each sketch, the static evaluation, the
//! compiled lowering and the reference lowering (`Options.reference`) agree on every node, instance, result, state
//! and record (keys with functions replaced by their bodies), and every authored value compiles for the IR executor.
//! Not ported: the per-frame IR/reference value forcing at four times and two domain counts, the SOP cooks at each
//! frame, the pixel comparisons (RAYS_WORKSPACE_PIXELS) and `qualify_workspace`'s fused/unfused emitter agreement:
//! they need the `Executor.force` option plumbing and the evaluator under redesign.
#![allow(dead_code, unused_imports)]
use flow_eval as E;
use flow_syntax::value::Value;
use flow_syntax::lisp;

use super::super::support::*;

fn functions(v: &Value) -> Value {
    match v {
        Value::Fn(f) => Value::Record(vec![
            ("body".into(), Value::Text(match E::residual::function_body(*f) {
                Some((params, body)) => format!("{params:?}{}", lisp::flat(&body.form())).into_bytes(),
                None => b"named".to_vec(),
            })),
            ("captures".into(), functions(&Value::Record(E::residual::function_bindings(*f)))),
        ]),
        Value::Residual(r) => match E::residual::residual_view(*r) {
            Ok(view) => Value::Record(vec![("residual".into(), Value::Text(lisp::flat(&view.term.form()).into_bytes())), ("bindings".into(), functions(&Value::Record(view.bindings.clone())))]),
            Err(_) => v.clone(),
        },
        Value::List(xs) => Value::List(std::sync::Arc::new(xs.iter().map(functions).collect())),
        Value::Record(fs) => Value::Record(fs.iter().map(|(n, v)| (n.clone(), functions(v))).collect()),
        Value::Struct(g) => flow_syntax::value::struct_value(g.name.clone(), g.ty.clone(), g.fields.iter().map(|(n, v)| (n.clone(), functions(v))).collect()),
        other => other.clone(),
    }
}

fn key(v: &Value) -> Vec<u8> {
    flow_syntax::value::key_of(&|r| E::residual::residual_id(r), &functions(v))
}

type Sig = (Vec<String>, Vec<String>, Vec<(String, Vec<u8>)>, Vec<Vec<u8>>, Vec<(Vec<String>, Vec<(Vec<i64>, Vec<u8>)>)>);

fn signature(e: &E::Eval) -> Sig {
    let nodes = e.plan.nodes.iter().map(|n| format!("{}|{}|{:?}|{:?}|{:?}|{}|{:?}", n.id, n.kind, n.ty, n.site, n.iter, n.inst, n.args.iter().map(|(k, v)| (k.clone(), String::from_utf8_lossy(&key(v)).into_owned())).collect::<Vec<_>>())).collect();
    let instances = e.plan.instances.iter().map(|i| format!("{}|{}|{:?}|{:?}", i.graph, i.default, i.inputs.iter().map(|(k, v)| (k.clone(), key(v))).collect::<Vec<_>>(), key(&i.result))).collect();
    let results = e.results.iter().map(|(n, v)| (n.clone(), key(v))).collect();
    let states = e.states.iter().map(key).collect();
    let records = e.records.iter().map(|(path, values)| (path.clone(), values.iter().map(|(iter, value)| (iter.clone(), key(value))).collect())).collect();
    (nodes, instances, results, states, records)
}

pub fn check(root_relative: &str) {
    // OCaml installs these module hooks before any static evaluation, not at the first lowering.
    flow_sop::lower::install_hooks();
    let path = repo_root().join(root_relative);
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{root_relative}: {e}"));
    let imports = rays_editor::Source::read_imports(path.to_str().unwrap(), &text).unwrap_or_else(|ds| panic!("{root_relative}: {}", diag_text(&ds)));
    let workspace = rays_editor::workspace::load(None, imports, None, &text).unwrap_or_else(|ds| panic!("{root_relative}: {}", diag_text(&ds)));
    let inputs = (*workspace.inputs).clone();
    let evaluated = E::static_with(&workspace.checked, true, Some(&inputs), None).unwrap_or_else(|d| panic!("{root_relative}: {}", rays_diagnostic::to_string(&d)));
    let lower = |reference: bool| {
        flow_sop::lower::of_checked(None, flow_sop::lower::Options { reference, inputs: Some(inputs.clone()), ..Default::default() }, &workspace.checked).unwrap_or_else(|d| panic!("{root_relative}: {}", rays_diagnostic::to_string(&d)))
    };
    let (compiled, reference) = (lower(false), lower(true));
    let expected = signature(&evaluated);
    for (label, got) in [("compiled", signature(&compiled.evaluated)), ("reference", signature(&reference.evaluated))] {
        let parts = [("nodes", expected.0 == got.0), ("instances", expected.1 == got.1), ("results", expected.2 == got.2), ("states", expected.3 == got.3), ("records", expected.4 == got.4)];
        let bad: Vec<_> = parts.iter().filter(|p| !p.1).map(|p| p.0).collect();
        if !bad.is_empty() {
            if let Some(i) = (0..expected.0.len().min(got.0.len())).find(|i| expected.0[*i] != got.0[*i]) {
                eprintln!("first node difference:
  static  {}
  {label} {}", expected.0[i], got.0[i]);
            }
        }
        assert!(bad.is_empty(), "{root_relative}: static vs {label} lowering differ in {bad:?}");
    }
    // every authored value compiles for the executor
    let mut values: Vec<Value> = evaluated.results.iter().map(|(_, v)| v.clone()).chain(evaluated.states.iter().cloned()).collect();
    for n in &evaluated.plan.nodes {
        if !n.site.iter().any(|s| s.starts_with("node:")) && n.kind != E::card_kind() {
            values.extend(n.args.iter().map(|(_, v)| v.clone()));
        }
    }
    for instance in &evaluated.plan.instances {
        values.push(instance.result.clone());
        values.extend(instance.inputs.iter().map(|(_, value)| value.clone()));
    }
    values.extend(evaluated.records.iter().filter(|(path, _)| !path.iter().any(|s| s.starts_with("node:"))).flat_map(|(_, records)| records.iter().map(|(_, value)| value.clone())));
    for v in &values {
        flow_ir::flow_ir::executor::compile(v, &Default::default()).unwrap_or_else(|d| panic!("{root_relative}: {}", rays_diagnostic::to_string(&d)));
    }
}

macro_rules! ir_cases {
    ($($name:ident => $path:expr),* $(,)?) => {
        $(pub fn $name() { check($path); })*
    };
}

ir_cases! {
    ws_kit => "sketches/ws_kit.rays",
    voxel_wall => "sketches/voxel_wall.rays",
    flow_kernel => "sketches/flow_kernel.rays",
    audio => "sketches/audio.rays",
    basic => "sketches/basic.rays",
    file_dialog => "sketches/file_dialog.rays",
    generative => "sketches/generative.rays",
    noise => "sketches/noise.rays",
    pxui => "sketches/pxui.rays",
    recursive_rectangles => "sketches/recursive_rectangles.rays",
    pathtracer => "sketches/pathtracer.rays",
    kit => "specification/pxui-kit/kit.rays",
    import => "specification/workspace/cases/import.rays",
}
