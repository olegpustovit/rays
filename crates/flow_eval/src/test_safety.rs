use crate::*;
use crate::test_workspace_eval::{check, ok};
use flow_syntax::{ty::Ty, value};
use std::sync::Arc;

use flow_check::term_arena::{Draft, TermBuilder};

fn safety_cases() {
    let mut workspace = check("(workspace w (graph g :context value 7))");
    for compiled in [false, true] {
        let mut builder = TermBuilder::new();
        let form = builder.import_form(&flow_syntax::syntax::make(flow_syntax::syntax::Draft::Sym("safety".into())));
        let leaf = builder.push(None, &Ty::Int, form, Draft::Lit(param::Value::Int(7)));
        let test = builder.push(None, &Ty::Bool, form, Draft::Lit(param::Value::Bool(true)));
        let mut term = leaf;
        for _ in 0..4096 {
            let node = if compiled { Draft::If(test, term, leaf) } else { Draft::Get(term, "x".into()) };
            term = builder.push(None, &Ty::Int, form, node);
        }
        let owner = builder.freeze();
        Arc::make_mut(&mut workspace.graphs)[0].body = owner.handle(term);
        let diagnostic = static_eval(&workspace).err().expect("deep authored terms must be refused before evaluation");
        assert_eq!(diagnostic.code, "E_DEPTH");
        Arc::make_mut(&mut workspace.graphs)[0].body = owner.handle(leaf);
        assert_eq!(ok(static_eval(&workspace)).results[0].1, Value::Int(7));
    }

    let mut value = Value::Float(1.0);
    for _ in 0..4096 { value = Value::List(Arc::new(vec![value])); }
    assert_eq!(force(&value, &input::at_time(0.0), None).unwrap_err().code, "E_DEPTH");
    while let Value::List(values) = value { value = Arc::try_unwrap(values).unwrap().pop().unwrap(); }

    fn reenter(source: &Value) -> Result<Value, rays_diagnostic::Diagnostic> {
        force_with(source, &input::at_time(0.0), None, None, Some(Arc::new(reenter)))
    }
    let source = value::source_value("recursive-host", Ty::Float, Vec::new());
    assert_eq!(reenter(&source).unwrap_err().code, "E_DEPTH");
    assert_eq!(ok(static_eval(&workspace)).results[0].1, Value::Int(7));

    for constructor in ["float", "int", "vec4", "text"] {
        let source = format!("(workspace w (graph g :context host (array/{constructor} 4503599627370495)))");
        let workspace = check(&source);
        let diagnostic = static_eval(&workspace).err().expect("impossible allocation must be refused");
        assert_eq!(diagnostic.code, "E_ARRAY_MEMORY", "{constructor}: {diagnostic:?}");
    }
    for body in ["(array/range 4503599627370495)", "(array/range 4503599627370495 :type int)", "(array/scatter_add (array/int 0) (array/float 0) 4503599627370495)"] {
        let workspace = check(&format!("(workspace w (graph g :context host {body}))"));
        assert_eq!(static_eval(&workspace).err().unwrap().code, "E_ARRAY_MEMORY");
    }
    assert_eq!(ok(static_eval(&workspace)).results[0].1, Value::Int(7));

    // Graph entry consumes one of the existing 64 call levels. These 63 named calls
    // reach, but do not exceed, that contract in compiled and recorded evaluation.
    let template = workspace.graphs[0].clone();
    let mut definitions = Vec::new();
    for index in 0..63 {
        let mut graph = template.clone();
        graph.name = format!("f{index}");
        let mut builder = TermBuilder::new();
        let form = builder.import_form(&flow_syntax::syntax::make(flow_syntax::syntax::Draft::Sym("safety".into())));
        let node = if index == 62 { Draft::Lit(param::Value::Int(7)) } else { Draft::CallFn { fn_name: format!("f{}", index + 1), args: Vec::new(), body: None } };
        let row = builder.push(None, &Ty::Int, form, node);
        graph.body = builder.freeze().handle(row);
        definitions.push(graph);
    }
    workspace.defs = Arc::new(definitions);
    let mut builder = TermBuilder::new();
    let form = builder.import_form(&flow_syntax::syntax::make(flow_syntax::syntax::Draft::Sym("safety".into())));
    let row = builder.push(None, &Ty::Int, form, Draft::CallFn { fn_name: "f0".into(), args: Vec::new(), body: None });
    Arc::make_mut(&mut workspace.graphs)[0].body = builder.freeze().handle(row);
    for record in [false, true] {
        assert_eq!(ok(r#static(&workspace, record, None)).results[0].1, Value::Int(7));
    }
}

#[test]
fn evaluator_safety_on_normal_stack_subprocess() {
    const CHILD: &str = "RAYS_EVAL_SAFETY_CHILD";
    if std::env::var_os(CHILD).is_some() {
        std::thread::spawn(safety_cases).join().expect("normal-stack evaluation panicked");
        return;
    }
    let output = std::process::Command::new(std::env::current_exe().unwrap())
        .args(["--exact", "test_safety::evaluator_safety_on_normal_stack_subprocess", "--test-threads=1", "--nocapture"])
        .env(CHILD, "1").output().unwrap();
    assert!(output.status.success(), "safety child failed: {}\n{}", String::from_utf8_lossy(&output.stdout), String::from_utf8_lossy(&output.stderr));
}

#[test]
fn interrupts_only_abandon_work_when_a_stop_was_observed() {
    use std::sync::atomic::{AtomicUsize, Ordering};
    let probes = Arc::new(AtomicUsize::new(0));
    let counted = probes.clone();
    assert!(with_interrupt(Arc::new(move || { counted.fetch_add(1, Ordering::Relaxed); true }), &mut || {}));
    assert_eq!(probes.load(Ordering::Relaxed), 0, "completed work does not sample the deadline afterwards");
    let mut ran = false;
    assert!(!with_interrupt(Arc::new(|| true), &mut || {
        assert_eq!(interruptible(&mut || ran = true).unwrap_err().code, "cancelled");
    }));
    assert!(!ran);
    assert!(current_interrupt().is_none(), "the outer scope is restored");
    assert!(!with_interrupt(Arc::new(|| true), &mut || {
        assert!(!with_interrupt(Arc::new(|| false), &mut || {
            assert!(interruptible(&mut || panic!("nested probe masked cancellation")).is_err());
        }));
    }), "a nested timing probe must preserve its caller's cancellation");
    assert!(with_interrupt(Arc::new(|| false), &mut || {
        assert!(!with_interrupt(Arc::new(|| true), &mut || {
            assert!(interruptible(&mut || panic!("expired probe ran")).is_err());
        }));
        assert!(interruptible(&mut || {}).is_ok());
    }), "a local deadline must leave its caller usable for a retry");
    assert!(!with_interrupt(Arc::new(|| true), &mut || {
        let inherited = current_interrupt().unwrap();
        std::thread::spawn(move || {
            assert!(!with_interrupt(inherited, &mut || {
                assert!(interruptible(&mut || panic!("cancelled work ran")).is_err());
            }));
        }).join().unwrap();
    }), "a worker's observed stop reaches its caller's latch");
}

#[test]
fn retained_evaluation_uses_the_callers_interrupt() {
    let workspace = check("(workspace w (graph g :context host (array/count (map (fn [(x : float)] (+ x (frame/dt))) (array/float 32 1.0)))))");
    let evaluated = ok(static_eval(&workspace));
    let value = &evaluated.results[0].1;
    let live = input::Frame { dt: 0.125, ..input::at_time(0.0) };
    assert!(!with_interrupt(Arc::new(|| true), &mut || {
        assert_eq!(force(value, &live, None).unwrap_err().code, "cancelled");
    }));
    let result = force(value, &live, None).unwrap();
    assert_eq!(result, Value::Int(32));
}
