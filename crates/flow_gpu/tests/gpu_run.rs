//! Port of lib/flow_gpu/test_run.ml, the runner half: emitted kernels match the CPU tier within
//! the recorded native noise tolerance, uniform/frame/input reuse, fast precision (carried loops,
//! int32 arithmetic, gathers), selected and untaken integer errors, empty dispatches, fused
//! outputs, refusals, and a drawing's region read back through `Executor::region_readback`.
//! Resident-fold, resident-value and counted compaction scenarios live in `gpu_resident.rs`.
mod common;
use common::test_program as tp;
use flow_gpu::{host, pipelines, run};
use flow_ir::flow_ir::{Executor, Gpu};
use flow_ir::packed::{self, host as phost};
use flow_shader::emit;
use flow_syntax::value::{self, Value};
use std::sync::Arc;

fn array(v: &Value) -> Vec<f64> {
    match v {
        Value::Float_array(a) | Value::Vec2_array(a) | Value::Vec3_array(a) | Value::Vec4_array(a) => (**a).clone(),
        _ => panic!("float array expected"),
    }
}

fn live(t: f64) -> input::Frame {
    input::at_time(t)
}

fn prepare(program: &packed::T, fast: bool, t: f64) -> phost::Inputs {
    tp::ok(phost::prepare(program, &phost::PrepareOptions { fast, ..Default::default() }, &live(t)))
}

fn force(program: &packed::T, t: f64, strict: bool) -> Value {
    tp::ok(packed::force(program, &live(t), &packed::ForceOptions { strict, ..Default::default() }))
}

fn start() -> Option<(renderer::renderer::Gpu, pipelines::T)> {
    if !common::gpu() {
        return None;
    }
    let lease = common::lease();
    let caps = ogpu::backend::capabilities(&renderer::renderer::gpu_device(&lease));
    if !caps.compute_pipeline {
        println!("SKIP: backend lacks Compute_pipeline");
        return None;
    }
    let pipes = pipelines::create(&|| 0.0, &renderer::renderer::gpu_device(&lease));
    Some((lease, pipes))
}

fn runner(lease: &renderer::renderer::Gpu, pipes: &pipelines::T, program: &packed::T, fast: bool) -> run::T {
    let msl = tp::ok(emit::kernel(&emit::Options { fast, ..Default::default() }, program));
    run::create(None, lease, pipes, &msl)
}

fn dispatch(r: &run::T, inputs: &phost::Inputs) -> Result<run::Output, rays_diagnostic::Diagnostic> {
    run::dispatch(r, &run::Options::default(), None, None, inputs)
}

fn dispatch_nowait(r: &run::T, inputs: &phost::Inputs) -> Result<run::Output, rays_diagnostic::Diagnostic> {
    run::dispatch(r, &run::Options { wait: Some(false), ..Default::default() }, None, None, inputs)
}

fn same_kind(a: &Value, b: &Value) -> bool {
    value::ty_of(a) == value::ty_of(b)
}

fn fixtures() {
    let Some((lease, pipes)) = start() else { return };
    for count in [1024i64, 16384] {
        let mut cases = tp::fixtures(None, count);
        // angles far past 2 pi, the hash idiom of sketches: exact floats, so the CPU agrees
        cases.push(("large-angle".into(), tp::compile(&format!("(map (fn [x] (sin (+ (* x 4096.0) (* t 0.0)))) (array/range {count}))"))));
        cases.extend(tp::vector_fixtures(None, count));
        for (name, program) in cases {
            let msl = tp::ok(emit::kernel(&emit::Options::default(), &program));
            let compiled = tp::ok(pipelines::get(&pipes, &msl));
            let compiled_before = pipelines::Private::compilations(&pipes);
            tp::ok(pipelines::get(&pipes, &msl));
            assert!(pipelines::Private::compilations(&pipes) == compiled_before && compiled_before >= 1);
            let _ = &compiled;
            let r = run::create(None, &lease, &pipes, &msl);
            let values = prepare(&program, false, 1.0);
            let output = tp::ok(dispatch(&r, &values));
            let actual_value = tp::ok(run::readback(&output, None));
            let expected_value = force(&program, 1.0, false);
            assert_eq!(run::Private::status_reads(&r), 1, "{name}");
            assert_eq!(run::Private::readback_bytes(&r), count * run::width(&output) * 4, "{name}");
            assert!(same_kind(&actual_value, &expected_value), "{name}");
            let (actual, expected) = (array(&actual_value), array(&expected_value));
            let maximum = actual.iter().zip(&expected).fold(0.0f64, |m, (a, e)| m.max((a - e).abs()));
            println!("{name},{count},max_abs_error={maximum:.9}");
            if name == "large-angle" {
                assert!(maximum <= 1e-5, "{name}");
            } else if name != "noise" {
                assert!(maximum == 0.0, "{name}: {maximum}");
            } else {
                assert!(maximum <= 2e-4, "{name}: {maximum}");
            }
            let created = run::Private::buffer_creations(&r);
            tp::ok(dispatch(&r, &values));
            assert_eq!(run::Private::buffer_creations(&r), created, "{name}");
            assert_eq!(run::Private::input_uploads(&r), values.arrays.len() as i64, "{name}");
            if name == "vec2" {
                let mut replacement = values.clone();
                replacement.arrays = values.arrays.iter().map(|a| Arc::new((**a).clone())).collect();
                Arc::make_mut(&mut replacement.arrays[0])[0] = 17.0;
                let changed = array(&tp::ok(run::readback(&tp::ok(dispatch(&r, &replacement)), None)));
                assert!(changed[0] == 17.0 && changed[1] == 18.0);
                let restored = array(&tp::ok(run::readback(&tp::ok(dispatch(&r, &values)), None)));
                assert_eq!(restored, expected);
                let later = prepare(&program, false, 2.0);
                let mut moved = values.clone();
                moved.frame = later.frame.clone();
                moved.uniforms = later.uniforms.clone();
                let animated = array(&tp::ok(run::readback(&tp::ok(dispatch(&r, &moved)), None)));
                assert!(animated[0] == 0.0 && animated[1] == 2.0);
                assert_eq!(run::Private::input_uploads(&r), 3);
            }
            run::close(&r, false);
        }
    }
    pipelines::close(&pipes);
}

/// fast precision: carried loops match the CPU kernel; 63-bit Int checks hold on the CPU and their
/// 32-bit form on the device, whose fast Int registers are 32 bits
fn carried() {
    let Some((lease, pipes)) = start() else { return };
    let int32 = tp::compile(&format!("(map (fn [x] (iterate 1 [t 0. 0. 0.] (fn [unused] {}))) (array/range 512))", tp::INT32_CHECKS));
    let r = runner(&lease, &pipes, &int32, true);
    let actual = array(&tp::ok(run::readback(&tp::ok(dispatch(&r, &prepare(&int32, true, 1.0))), None)));
    assert!(actual.len() == 2048 && actual.iter().all(|v| *v == 1.0), "32-bit Int checks on the device");
    run::close(&r, false);
    for (name, program) in tp::carried_fixtures(512) {
        if name == "iterate-int63" {
            assert!(array(&force(&program, 1.0, true)).iter().all(|v| *v == 1.0), "63-bit Int checks on the CPU");
            continue;
        }
        let r = runner(&lease, &pipes, &program, true);
        let expected = array(&tp::ok(packed::host::reference(&program, &live(1.0), None, None, None)));
        let cpu = array(&force(&program, 1.0, true));
        assert_eq!(cpu, expected, "{name}");
        let values = prepare(&program, true, 1.0);
        let actual = array(&tp::ok(run::readback(&tp::ok(dispatch(&r, &values)), None)));
        assert!(actual == expected, "{name}: carried GPU loop differs from reference");
        println!("{name}: 512 native GPU rows match strict CPU and reference");
        run::close(&r, false);
    }
    pipelines::close(&pipes);
}


const STATUS: [(&str, &str, bool, &str); 8] = [
    ("shift-selected", "(map (fn [(x : int)] (if (> (+ (float x) t) 0.) (float (int/shr x (int/add 63 (int/mod (frame/index) 1)))) (float x))) (array/range 8 :type int))", true, "Integer shift count must be within [0, 62]."),
    ("shift-untaken", "(map (fn [(x : int)] (if (< (+ (float x) t) 0.) (float (int/shr x (int/add 63 (int/mod (frame/index) 1)))) (float x))) (array/range 8 :type int))", false, ""),
    ("negative-shift-selected", "(map (fn [(x : int)] (if (> (+ (float x) t) 0.) (float (int/shr x (int/sub (frame/index) (int/add (frame/index) 1)))) (float x))) (array/range 8 :type int))", true, "Integer shift count must be within [0, 62]."),
    ("negative-shift-untaken", "(map (fn [(x : int)] (if (< (+ (float x) t) 0.) (float (int/shr x (int/sub (frame/index) (int/add (frame/index) 1)))) (float x))) (array/range 8 :type int))", false, ""),
    ("divide-selected", "(map (fn [(x : int)] (if (> (+ (float x) t) 0.) (float (int/div x (int/sub (frame/index) (frame/index)))) (float x))) (array/range 8 :type int))", true, "Integer division by zero."),
    ("divide-untaken", "(map (fn [(x : int)] (if (< (+ (float x) t) 0.) (float (int/div x (int/sub (frame/index) (frame/index)))) (float x))) (array/range 8 :type int))", false, ""),
    ("modulo-selected", "(map (fn [(x : int)] (if (> (+ (float x) t) 0.) (float (int/mod x (int/sub (frame/index) (frame/index)))) (float x))) (array/range 8 :type int))", true, "Integer division by zero."),
    ("modulo-untaken", "(map (fn [(x : int)] (if (< (+ (float x) t) 0.) (float (int/mod x (int/sub (frame/index) (frame/index)))) (float x))) (array/range 8 :type int))", false, ""),
];

fn statuses() {
    let Some((lease, pipes)) = start() else { return };
    for (name, source, selected, message) in STATUS {
        let program = tp::compile(source);
        let r = runner(&lease, &pipes, &program, true);
        let inputs = prepare(&program, true, 1.0);
        let output = dispatch_nowait(&r, &inputs).unwrap_or_else(|_| panic!("{name}: dispatch"));
        match run::readback(&output, None) {
            Err(d) => {
                assert!(selected && d.code == "E_RANGE" && d.message == message, "{name}: {}", d.message);
                assert_eq!(run::Private::status_reads(&r), 1, "{name}");
                let empty = tp::ok(dispatch_nowait(&r, &phost::Inputs { count: 0, ..inputs.clone() }));
                assert!(array(&tp::ok(run::readback(&empty, None))).is_empty() && run::Private::status_reads(&r) == 1);
                if name == "divide-selected" {
                    let queue = renderer::renderer::gpu_queue(&lease);
                    let commands = ogpu::backend::begin_commands(&queue).expect("commands");
                    tp::ok(run::dispatch(&r, &run::Options { commands: Some(commands.clone()), ..Default::default() }, None, None, &inputs));
                    let empty = tp::ok(dispatch_nowait(&r, &phost::Inputs { count: 0, ..inputs.clone() }));
                    assert!(array(&tp::ok(run::readback(&empty, None))).is_empty());
                    let receipt = ogpu::backend::commit(&commands).expect("commit");
                    match run::Private::check_status(&r, receipt.epoch) {
                        Err(d) => assert!(d.code == "E_RANGE" && d.message == message),
                        Ok(()) => panic!("empty dispatch cleared an earlier resident GPU error"),
                    }
                    let h = host::create(&|| 0.0, &lease);
                    let kernel = tp::ok((host::backend(&h).prepare)(true, &program));
                    let value = tp::ok((kernel.run)(&inputs));
                    match host::output(&h, &value) {
                        Err(d) => assert!(d.code == "E_RANGE" && d.message == message),
                        Ok(_) => panic!("Host.output exposed a GPU buffer with a selected integer error"),
                    }
                    assert_eq!(host::stats(&h).status_reads, 1);
                    host::close(&h);
                }
            }
            Ok(v) => assert!(!selected && array(&v) == (0..8).map(|i| i as f64).collect::<Vec<_>>(), "{name}"),
        }
        run::close(&r, false);
    }
    pipelines::close(&pipes);
}

fn empty_and_outputs() {
    let Some((lease, pipes)) = start() else { return };
    let fresh = tp::compile("(map (fn [x] (+ x t)) (array/range 8))");
    let r = runner(&lease, &pipes, &fresh, true);
    let values = prepare(&fresh, true, 1.0);
    let empty = tp::ok(dispatch_nowait(&r, &phost::Inputs { count: 0, ..values }));
    assert!(array(&tp::ok(run::readback(&empty, None))).is_empty() && run::Private::status_reads(&r) == 0);
    run::close(&r, false);
    // a two-output kernel on a host: an empty run borrows nothing and reads no status
    let program = tp::two_outputs();
    let h = host::create(&|| 0.0, &lease);
    let kernel = tp::ok((host::backend(&h).prepare)(false, &program));
    let values = prepare(&program, false, 1.0);
    let empty = tp::ok((kernel.run)(&phost::Inputs { count: 0, ..values }));
    let borrowed = tp::ok(host::output(&h, &empty)).expect("borrowed");
    let read = tp::ok((kernel.readback)(&Gpu::Value { width: run::width(&borrowed), ..empty.clone() }));
    assert!(array(&read).is_empty() && host::stats(&h).status_reads == 0);
    host::close(&h);
    pipelines::close(&pipes);
}

fn refusals() {
    let Some((lease, pipes)) = start() else { return };
    for (body, refuses) in [("(fold [a x] [i (range (+ 4097 (int t)))] (+ a 1.))", true), ("(if (> t 0.) x (fold [a x] [i (range (+ 4097 (int t)))] (+ a 1.)))", false)] {
        let program = tp::compile(&format!("(map (fn [x] {body}) (array/range 2))"));
        let r = runner(&lease, &pipes, &program, true);
        let values = prepare(&program, true, 1.0);
        match dispatch(&r, &values) {
            Err(d) => assert!(refuses && d.code == "E_KERNEL", "{body}"),
            Ok(output) => {
                assert!(!refuses, "{body}");
                assert_eq!(array(&tp::ok(run::readback(&output, None))), vec![0.0, 1.0]);
            }
        }
        run::close(&r, false);
    }
    // a hidden overflow: a taken branch refuses, an untaken one does not
    for (body, refuses) in [("(if (> (+ x t) 0) 0 (* (* (+ x t) 1e20) (* (+ x t) 1e20)))", false), ("(min (* (* (+ x t) 1e20) (* (+ x t) 1e20)) 0)", true)] {
        let hidden = tp::compile(&format!("(map (fn [x] {body}) (array/range 1024))"));
        let r = runner(&lease, &pipes, &hidden, false);
        assert!(array(&force(&hidden, 1.0, false)).iter().all(|v| *v == 0.0));
        let inputs = prepare(&hidden, false, 1.0);
        match dispatch(&r, &inputs) {
            Err(d) => assert!(refuses && d.code == "E_KERNEL", "{body}"),
            Ok(output) => {
                assert!(!refuses, "{body}");
                assert!(array(&tp::ok(run::readback(&output, None))).iter().all(|v| *v == 0.0));
            }
        }
        run::close(&r, false);
    }
    pipelines::close(&pipes);
}

fn gather_and_region() {
    let Some((lease, pipes)) = start() else { return };
    let program = tp::compile("(let* [tab (map (fn [x] [x (+ x t) 0]) (array/range 64))] (map (fn [i] (let* [p (array/nth tab (int/mod (int/mul (int i) 7) 64))] (+ p.y (int/and (int/add (int i) 1) 3)))) (array/range 4096)))");
    let r = runner(&lease, &pipes, &program, true);
    let output = tp::ok(dispatch(&r, &prepare(&program, true, 1.0)));
    let actual = array(&tp::ok(run::readback(&output, None)));
    let expected = array(&force(&program, 1.0, false));
    assert!(actual == expected, "fast int32 and gather kernel differs from the CPU");
    println!("fast int32 and gather kernel matches the CPU at 4,096 elements");
    // a drawing's region read back from the device in fast precision, and declined in exact
    let h = host::create(&|| 0.0, &lease);
    let config = Gpu::Config { backend: Some(Arc::new(host::backend(&h))), ..Gpu::default() };
    let exact = Executor::region_readback(Some(config.clone()));
    assert!(tp::ok(exact.call(&program, &live(1.0))).is_none(), "exact precision sent a region to the device");
    let fast = Executor::region_readback(Some(Gpu::Config { fast: true, ..config }));
    match tp::ok(fast.call(&program, &live(1.0))) {
        Some(v) => assert!(array(&v) == expected, "the device region differs from the CPU"),
        None => panic!("fast precision kept a 4,096-element region on the CPU"),
    }
    host::close(&h);
    run::close(&r, false);
    pipelines::close(&pipes);
}

fn selected_outputs() {
    let Some((lease, pipes)) = start() else { return };
    let program = tp::two_outputs();
    let r = runner(&lease, &pipes, &program, false);
    let output = tp::ok(dispatch(&r, &prepare(&program, false, 1.0)));
    let expected = tp::ok(packed::force_all(&program, &live(1.0), &Default::default()));
    for (index, e) in expected.iter().enumerate() {
        let selected = run::select_output(index as i64, &output).expect("selected");
        assert_eq!(run::width(&selected), phost::view(&program).outputs[index].len() as i64);
        assert_eq!(run::buffer(&selected, None).map(|b| ogpu::backend::buffer_id(&b)), run::buffer(&output, Some(index as i64)).map(|b| ogpu::backend::buffer_id(&b)));
        assert_eq!(array(&tp::ok(run::readback(&selected, None))), array(e));
    }
    assert!(run::select_output(-1, &output).is_none());
    assert!(run::select_output(expected.len() as i64, &output).is_none());
    let h = host::create(&|| 0.0, &lease);
    let kernel = tp::ok((host::backend(&h).prepare)(false, &program));
    let value = tp::ok((kernel.run)(&prepare(&program, false, 1.0)));
    for (out, e) in expected.iter().enumerate() {
        let width = phost::view(&program).outputs[out].len() as i64;
        assert_eq!(array(&tp::ok((kernel.readback)(&Gpu::Value { output: out as i64, width, ..value.clone() }))), array(e));
    }
    assert!(tp::ok(host::output(&h, &Gpu::Value { output: expected.len() as i64, ..value.clone() })).is_none());
    tp::ok((kernel.run)(&prepare(&program, false, 1.0)));
    assert!(tp::ok(host::output(&h, &value)).is_none());
    host::close(&h);
    run::close(&r, false);
    pipelines::close(&pipes);
}

/// a captured value that holds still is folded into a background-compiled kernel; the result is
/// the generic kernel's, and a changed value is right at once, through the generic kernel
fn specialized_captures() {
    let Some((lease, pipes)) = start() else { return };
    let program = tp::compile("(let* [side 3] (map (fn [(i : int)] (+ (* t 0.0) (float (int/div (int/mul i 7) side)))) (array/range 4096 :type int)))");
    let r = runner(&lease, &pipes, &program, true);
    let values = prepare(&program, true, 1.0);
    assert!(!values.uniforms.is_empty(), "the capture is a uniform");
    let expected = array(&force(&program, 1.0, false));
    assert_eq!(array(&tp::ok(run::readback(&tp::ok(dispatch(&r, &values)), None))), expected);
    let start = std::time::Instant::now();
    let generic = pipelines::Private::count(&pipes);
    while pipelines::Private::count(&pipes) == generic && start.elapsed().as_secs_f64() < 3.0 {
        assert_eq!(array(&tp::ok(run::readback(&tp::ok(dispatch(&r, &values)), None))), expected);
        std::thread::sleep(std::time::Duration::from_millis(5));
    }
    assert!(pipelines::Private::count(&pipes) > generic, "a specialized kernel compiled");
    assert_eq!(array(&tp::ok(run::readback(&tp::ok(dispatch(&r, &values)), None))), expected);
    let mut changed = values.clone();
    changed.uniforms[0] = vec![5.0];
    let five = array(&tp::ok(run::readback(&tp::ok(dispatch(&r, &changed)), None)));
    assert!(five.iter().enumerate().all(|(i, x)| *x == ((i * 7) / 5) as f64));
    run::close(&r, false);
    pipelines::close(&pipes);
}

fn guarded_gather() {
    let Some((lease,pipes))=start() else {return};
    let program=tp::compile("(let* [table (array/int 8 1)]
      (map (fn [(i : int)]
        (if (and (> (array/nth table i) 0)
                 (= (if (< (float i) (+ 7.0 (* t 0.0))) (array/nth table (int/add i 1)) 0) 0)) 1.0 0.0))
        (array/range 8 :type int)))");
    let shader=tp::ok(emit::kernel(&emit::Options {fast:true,uint32_tables:vec![0],..Default::default()},&program));
    assert_eq!(shader.gather_uint32,vec![true,true],"every guarded read of the table must retain its integer type");
    let device=renderer::renderer::gpu_device(&lease);
    let table=ogpu::backend::create_buffer(&device,ogpu::types::BufferDescriptor {label:None,size:32,usage:vec![ogpu::types::BufferUsage::Storage,ogpu::types::BufferUsage::CopyDst]},None).unwrap();
    ogpu::backend::write_buffer(&table,0,&[1u32.to_le_bytes();8].concat()).unwrap();
    let r=run::create(None,&lease,&pipes,&shader);
    let output=tp::ok(run::dispatch(&r,&Default::default(),None,Some(&|_|Some((table.clone(),8))),&prepare(&program,true,0.)));
    let actual=array(&tp::ok(run::readback(&output,None)));
    assert_eq!(actual,array(&force(&program,0.,false)));
    run::close(&r,false);ogpu::backend::destroy_buffer(&table).unwrap();pipelines::close(&pipes);
}

fn main() {
    common::window::run(&[("guarded_gather",guarded_gather),("specialized_captures", specialized_captures), ("fixtures", fixtures), ("carried", carried), ("statuses", statuses), ("empty_and_outputs", empty_and_outputs), ("refusals", refusals), ("gather_and_region", gather_and_region), ("selected_outputs", selected_outputs)]);
}
