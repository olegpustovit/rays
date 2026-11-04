//! Port of lib/flow_gpu/test_uint32.ml: exact uint32 outputs, inputs, tables and holders above 2^24
//! and through 2^32-1, the resident fold's uint32 scope, compaction block tails, topology
//! validation on the device and GPU-origin streams.
//!
//! Deviation, noted in the crate report: the OCaml test materialises holders from 4 worker
//! domains; OGPU views are `Rc` here, so a worker thread can only read planes the creating thread
//! already read. These cases materialise on the creating thread.
mod common;
use common::test_program as tp;
use flow_gpu::{host, pipelines, run};
use flow_ir::flow_ir::{Executor, Gpu};
use flow_ir::packed::{self, host as phost};
use flow_shader::emit;
use flow_syntax::ty::Ty;
use flow_syntax::value::Value;
use ogpu::backend as b;
use ogpu::types::{BufferDescriptor, BufferUsage};
use std::sync::Arc;

fn program(expression: &str) -> packed::T {
    tp::compile(&format!("(map (fn [(i : int)] {expression}) (array/range 1025 :type int))"))
}

fn live(t: f64) -> input::Frame {
    input::at_time(t)
}

fn prepare(p: &packed::T, t: f64) -> phost::Inputs {
    tp::ok(phost::prepare(p, &phost::PrepareOptions { fast: true, ..Default::default() }, &live(t)))
}

fn u32_kernel(p: &packed::T) -> emit::Kernel {
    tp::ok(emit::kernel(&emit::Options { fast: true, uint32_outputs: vec![0], ..Default::default() }, p))
}

fn force_strict(p: &packed::T, t: f64) -> Value {
    tp::ok(packed::force(p, &live(t), &packed::ForceOptions { strict: true, ..Default::default() }))
}

fn ints(v: &Value) -> Vec<i64> {
    match v {
        Value::Int_array(a) => (**a).clone(),
        _ => panic!("int array expected"),
    }
}

fn setup() -> Option<(renderer::renderer::gpu, pipelines::T)> {
    if !common::gpu() {
        return None;
    }
    let lease = common::lease();
    let pipes = pipelines::create(&|| 0.0, &renderer::renderer::gpu_device(&lease));
    Some((lease, pipes))
}

fn compaction(lease: &renderer::renderer::gpu, pipes: &pipelines::T, n: i64, limited: bool) {
    let queue = renderer::renderer::gpu_queue(lease);
    let device = renderer::renderer::gpu_device(lease);
    let mut buffers: Vec<b::Buffer> = vec![];
    let mut buffer = |length: i64| {
        let x = b::create_buffer(&device, BufferDescriptor { label: Some("uint32 compaction test".into()), size: length.max(4), usage: vec![BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst] }, None).expect("buffer");
        buffers.push(x.clone());
        x
    };
    let large = n > 1 << 24;
    let indices: Vec<i64> = if large { vec![n - 3, n - 2, n - 1] } else { (0..n).filter(|i| i % 3 == 1).collect() };
    let mut bytes = vec![0u8; (n * 4).max(4) as usize];
    for i in &indices {
        bytes[*i as usize * 4..*i as usize * 4 + 4].copy_from_slice(&1i32.to_le_bytes());
    }
    let mask = buffer(bytes.len() as i64);
    let kept = buffer(4 * indices.len() as i64);
    let total = buffer(4);
    let cell = buffer(4);
    let blocks = buffer(run::compact_blocks(n));
    let groups = buffer(12);
    match run::fill_indices(None, &queue, pipes, &kept, b::buffer_size(&kept) / 4 + 1) {
        Err(d) => assert_eq!(d.code, "E_GPU"),
        Ok(()) => panic!("integer fill exceeded buffer capacity"),
    }
    b::write_buffer(&mask, 0, &bytes).unwrap();
    let count = if limited { n / 2 } else { n };
    b::write_buffer(&cell, 0, &(count as i32).to_le_bytes()).unwrap();
    let commands = b::begin_commands(&queue).unwrap();
    tp::ok(run::compact(true, None, &commands, pipes, &mask, n, if limited { Some(&cell) } else { None }, &kept, &total, &blocks, &groups));
    let receipt = b::commit(&commands).unwrap();
    b::complete_through(&queue, receipt.epoch).unwrap();
    let expected: Vec<i64> = indices.iter().copied().filter(|i| *i < count).collect();
    assert_eq!(i32::from_le_bytes(b::read_buffer(&total, 0, 4).unwrap()[0..4].try_into().unwrap()) as i64, expected.len() as i64);
    let result = b::read_buffer(&kept, 0, (4 * expected.len() as i64).max(4)).unwrap();
    for (k, i) in expected.iter().enumerate() {
        assert_eq!(u32::from_le_bytes(result[k * 4..k * 4 + 4].try_into().unwrap()) as i64, *i);
    }
    for x in buffers {
        let _ = b::destroy_buffer(&x);
    }
}

fn compaction_cases() {
    let Some((lease, pipes)) = setup() else { return };
    for n in [0, 1, 1023, 1024, 1025] {
        for limited in [false, true] {
            compaction(&lease, &pipes, n, limited);
        }
    }
    compaction(&lease, &pipes, (1 << 24) + 3, false);
    pipelines::close(&pipes);
}

fn resident_values() {
    if !common::gpu() {
        return;
    }
    let lease = common::lease();
    let h = host::create(&|| 0.0, &lease);
    let int_array = Ty::Array(Box::new(Ty::Int));
    for length in [0i64, 1, 1025, 2049] {
        let before = host::stats(&h).payload_readback_bytes;
        let scope = host::resident_scope(true, true, &h);
        let resident = scope.executor.clone();
        let range = (resident.range)(&int_array, &Value::Int(length), 1, length).unwrap().expect("range");
        let kept = (resident.compact)(&range).unwrap().expect("compact");
        let filled = (resident.fill)(&int_array, length, &Value::Int(4294967295)).unwrap().expect("fill");
        assert!((resident.fill)(&int_array, length, &Value::Int(-1)).unwrap().is_none());
        (resident.settle)().unwrap();
        assert_eq!(host::stats(&h).payload_readback_bytes, before);
        assert_eq!((resident.count)(&kept).unwrap(), Some((length - 1).max(0)));
        let kept = (scope.retain_uint32)(&kept).expect("kept");
        let range = (scope.retain_uint32)(&range).expect("range");
        let filled = (scope.retain_uint32)(&filled).expect("filled");
        assert!(!Arc::ptr_eq(&kept.materialise, &range.materialise) && !Arc::ptr_eq(&range.materialise, &filled.materialise));
        (resident.close)();
        assert_eq!(host::stats(&h).payload_readback_bytes, before);
        assert_eq!((kept.materialise)(), (1..=(length - 1).max(0)).collect::<Vec<_>>());
        assert_eq!((filled.materialise)(), vec![4294967295; length as usize]);
        assert_eq!((range.materialise)(), (0..length).collect::<Vec<_>>());
    }
    for small_first in [false, true] {
        let before = host::stats(&h).payload_readback_bytes;
        let scope = host::resident_scope(true, true, &h);
        let resident = scope.executor.clone();
        let range = |count: i64| (resident.range)(&int_array, &Value::Int(count), 1, count).unwrap().expect("range");
        let large_value = range(1025);
        let small_value = range(17);
        let first = (scope.retain_uint32)(if small_first { &small_value } else { &large_value }).unwrap();
        let second = (scope.retain_uint32)(if small_first { &large_value } else { &small_value }).unwrap();
        let (small, large) = if small_first { (first, second) } else { (second, first) };
        assert!(small.count == 17 && large.count == 1025);
        assert!(Arc::ptr_eq(&(scope.retain_uint32)(&small_value).unwrap().materialise, &small.materialise));
        assert_eq!((resident.count)(&small_value).unwrap(), Some(17));
        assert_eq!((resident.count)(&large_value).unwrap(), Some(1025));
        let (sb, sn, _, su) = (small.pin)().expect("small pin");
        let (lb, ln, _, lu) = (large.pin)().expect("large pin");
        assert!(b::buffer_id(&sb) == b::buffer_id(&lb) && sn == 17 && ln == 1025);
        su();
        lu();
        let prefix = (small.materialise)();
        assert_eq!(prefix, (0..17).collect::<Vec<_>>());
        (small.release)(false);
        let copied = (resident.compact)(&large_value).unwrap().expect("compact");
        assert_eq!((resident.count)(&copied).unwrap(), Some(1024));
        let copied = (scope.retain_uint32)(&copied).unwrap();
        (resident.close)();
        assert_eq!((large.materialise)(), (0..1025).collect::<Vec<_>>());
        assert_eq!(host::stats(&h).payload_readback_bytes - before, 1025 * 4);
        assert_eq!((copied.materialise)(), (1..=1024).collect::<Vec<_>>());
    }
    host::close(&h);
}

fn kinds_of_values() {
    if !common::gpu() {
        return;
    }
    let lease = common::lease();
    let programs: [(&str, bool); 3] = [
        (
            "(let* [xs (map (fn [(i : int)] (int/add (int/add i 16777217) (int/mod (frame/index) 1))) (array/range 1025 :type int))
                copied (map (fn [(i : int)] (int/add i (int/mod (frame/index) 1))) xs)
                gathered (map (fn [(i : int)] (array/nth xs i)) (array/range (array/count xs) :type int))]
          {:values xs :alias xs :copied copied :gathered gathered
           :positions (map (fn [(i : int)] [(+ (float i) t) 2. 3.]) (array/range 1025 :type int))})",
            true,
        ),
        ("(let* [xs (map (fn [(i : int)] (int/add i (int/mod (frame/index) 1))) (array/range 1025 :type int))] (map (fn [(x : int)] (+ (float x) t)) xs))", false),
        (
            "{:vertex_points (map (fn [(i : int)] (int/add i (int/mod (frame/index) 1))) (array/range 1026 :type int))
           :primitive_offsets (map (fn [(i : int)] (int/add (int/mul i 3) (int/mod (frame/index) 1))) (array/range 343 :type int))
           :kinds (map (fn [(i : int)] (int/mod (frame/index) 1)) (array/range 342 :type int))
           :positions (map (fn [(i : int)] [(+ (float i) t) 0. 0.]) (array/range 1026 :type int))}",
            true,
        ),
    ];
    for (body, no_read) in programs {
        let source = format!("(workspace w (graph g :context value (let* [tested (if (>= t 0.) {body} {body})] 0.)))");
        let ws = tp::check_workspace(&source);
        let evaluated = tp::ok(flow_eval::r#static(&ws, true, None));
        let value = evaluated.records.iter().find(|(p, _)| p == &vec!["g".to_string(), "tested".to_string()]).expect("record").1[0].1.clone();
        let compiled = tp::ok(Executor::compile(&value, &Executor::CompileOptions { strict: true, ..Default::default() }));
        let h = host::create(&|| 0.0, &lease);
        for time in [0.0, 1.0] {
            let before_run = host::stats(&h);
            let scope = ((host::backend(&h)).residency)(true).expect("residency");
            let resident = scope.executor.clone();
            let result = tp::ok(Executor::force(&compiled, &Executor::ForceOptions { strict: true, resident: Some(resident.clone()), ..Default::default() }, &live(time)));
            if no_read {
                assert_eq!(host::stats(&h).payload_readback_bytes, before_run.payload_readback_bytes);
            }
            if let Value::Record(fields) = &result {
                let get = |n: &str| fields.iter().find(|(k, _)| k == n).map(|(_, v)| v.clone());
                if let (Some(v), Some(o), Some(k)) = (get("vertex_points"), get("primitive_offsets"), get("kinds")) {
                    tp::ok((scope.validate_topology)(1026, &v, &o, &k, 3).expect("streams"));
                    assert_eq!(host::stats(&h).validation_readback_bytes, before_run.validation_readback_bytes + 36);
                }
            }
            fn capture(scope: &Gpu::Residency, v: &Value) -> Box<dyn Fn() -> Value> {
                match v {
                    Value::Record(fields) => {
                        let fields: Vec<(String, Box<dyn Fn() -> Value>)> = fields.iter().map(|(k, v)| (k.clone(), capture(scope, v))).collect();
                        Box::new(move || Value::Record(fields.iter().map(|(k, make)| (k.clone(), make())).collect()))
                    }
                    Value::Int_array(_) => {
                        let holder = (scope.retain_uint32)(v).expect("uint32 holder");
                        Box::new(move || Value::Int_array(Arc::new((holder.materialise)())))
                    }
                    Value::Float_array(_) | Value::Vec2_array(_) | Value::Vec3_array(_) | Value::Vec4_array(_) => {
                        let holder = (scope.retain_float)(v).expect("float holder");
                        let v = v.clone();
                        Box::new(move || {
                            let planes = (holder.materialise)();
                            let (c, w) = (holder.count as usize, holder.width as usize);
                            let values = Arc::new((0..c * w).map(|i| planes[i % w][i / w]).collect::<Vec<_>>());
                            match v {
                                Value::Float_array(_) => Value::Float_array(values),
                                Value::Vec2_array(_) => Value::Vec2_array(values),
                                Value::Vec3_array(_) => Value::Vec3_array(values),
                                _ => Value::Vec4_array(values),
                            }
                        })
                    }
                    other => {
                        let other = other.clone();
                        Box::new(move || other.clone())
                    }
                }
            }
            let materialise = capture(&scope, &result);
            (resident.close)();
            if no_read {
                assert_eq!(host::stats(&h).payload_readback_bytes, before_run.payload_readback_bytes);
            }
            let expected = tp::ok(flow_eval::Host::force_reference(&value, &live(time), None, None, None));
            assert_eq!(format!("{:?}", materialise()), format!("{expected:?}"));
            let before = host::stats(&h).payload_readback_bytes;
            assert_eq!(format!("{:?}", materialise()), format!("{expected:?}"));
            assert_eq!(host::stats(&h).payload_readback_bytes, before);
        }
        host::close(&h);
    }
}

fn holders() {
    let Some((lease, pipes)) = setup() else { return };
    let packed = program("(int/add (if (int/eq (int/mod i 2) 0) 4294967295 16777217) (int/mod (frame/index) 1))");
    let expected: Vec<i64> = (0..1025).map(|i| if i % 2 == 0 { 4294967295 } else { 16777217 }).collect();
    let r = run::create(None, &lease, &pipes, &u32_kernel(&packed));
    let dispatch = || {
        let o = tp::ok(run::dispatch(&r, &run::Options { wait: Some(false), ..Default::default() }, None, None, &prepare(&packed, 0.0)));
        tp::ok(run::keep_uint32(&o, None))
    };
    let first = dispatch();
    let neutral = run::integers(&first);
    assert!(neutral.count == first.count);
    assert!(run::pin_integers(&neutral).is_none());
    tp::ok((first.settle)());
    assert_eq!(run::Private::readback_bytes(&r), 0);
    let (_, count, valid, unpin) = run::pin_integers(&neutral).expect("pin");
    assert!(count == 1025 && valid());
    let foreign = std::thread::spawn(move || run::pin_integers(&neutral).is_none());
    assert!(foreign.join().unwrap());
    let second = dispatch();
    tp::ok((second.settle)());
    assert_eq!(run::Private::readback_bytes(&r), 0);
    for _ in 0..2 {
        assert_eq!((first.materialise)(), expected);
    }
    assert_eq!(run::Private::readback_bytes(&r), 1025 * 4);
    assert_eq!(run::Private::materialisations(&r), 1);
    assert!(valid());
    unpin();
    unpin();
    assert!(!valid());
    assert!((first.pin)().is_none());
    let freed = dispatch();
    tp::ok((freed.settle)());
    (freed.release)(false);
    assert!((freed.pin)().is_none());
    let read = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| (freed.materialise)()));
    assert!(read.is_err(), "freed uint32 holder read");
    (second.release)(true);
    run::close(&r, false);
    assert_eq!((second.materialise)(), expected);
    assert_eq!(run::Private::readback_bytes(&r), 2 * 1025 * 4);
    // an invalid word is never published
    let bad = program("(int/add -1 (int/mod (frame/index) 1))");
    let r = run::create(None, &lease, &pipes, &u32_kernel(&bad));
    let o = tp::ok(run::dispatch(&r, &run::Options { wait: Some(false), ..Default::default() }, None, None, &prepare(&bad, 0.0)));
    let holder = tp::ok(run::keep_uint32(&o, None));
    let read = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| (holder.materialise)()));
    assert!(read.is_err(), "invalid uint32 holder published");
    assert_eq!(run::Private::readback_bytes(&r), 0);
    run::close(&r, true);
    pipelines::close(&pipes);
}

fn topology() {
    let Some((lease, pipes)) = setup() else { return };
    let device = renderer::renderer::gpu_device(&lease);
    let queue = renderer::renderer::gpu_queue(&lease);
    let h = host::create(&|| 0.0, &lease);
    let buffers: std::cell::RefCell<Vec<b::Buffer>> = Default::default();
    let buffer = |words: &[i64]| {
        let x = b::create_buffer(&device, BufferDescriptor { label: Some("topology validator test".into()), size: (words.len() as i64 * 4).max(4), usage: vec![BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst] }, None).unwrap();
        buffers.borrow_mut().push(x.clone());
        if !words.is_empty() {
            b::write_buffer(&x, 0, &words.iter().flat_map(|n| (*n as i32).to_le_bytes()).collect::<Vec<_>>()).unwrap();
        }
        x
    };
    let status = buffer(&[-1; 9]);
    let check = |points: i64, limit: i64, vertices: &[i64], offsets: &[i64], kinds: &[i64], expected: Option<(&str, String)>| {
        let (vb, ob, kb) = (buffer(vertices), buffer(offsets), buffer(kinds));
        let (v, o, k) = ((&vb, vertices.len() as i64), (&ob, offsets.len() as i64), (&kb, kinds.len() as i64));
        let result = run::validate_topology(&|_| (), &queue, &pipes, v, o, k, &status, points, limit);
        let before = host::stats(&h);
        let hosted = host::validate_topology(&h, v, o, k, points, limit);
        assert_eq!(hosted.as_ref().map_err(|d| (d.code.clone(), d.message.clone())), result.as_ref().map_err(|d| (d.code.clone(), d.message.clone())));
        let after = host::stats(&h);
        assert_eq!(after.validation_readback_bytes, before.validation_readback_bytes + 36);
        assert_eq!(after.payload_readback_bytes, 0);
        assert_eq!(after.readback_bytes, 0);
        assert_eq!(after.buffer_creations, 1);
        match (result, expected) {
            (Ok(()), None) => {}
            (Err(d), Some((code, message))) => {
                assert_eq!(d.code, code);
                assert_eq!(d.message, message);
            }
            (Err(d), None) => panic!("{}", rays_diagnostic::to_string(&d)),
            (Ok(()), Some(_)) => panic!("invalid topology accepted"),
        }
    };
    let err = |m: &str| Some(("E_TOPOLOGY", format!("Topology: {m}")));
    let quad = [0, 1, 2, 3];
    let max = 4294967295;
    check(4, max, &quad, &[0, 4], &[0], None);
    check(4, max, &[], &[0], &[], None);
    check(4, max, &[], &[], &[], err("primitive offsets must contain the initial zero"));
    check(4, max, &quad, &[1, 4], &[0], err("first primitive offset must be zero"));
    check(4, max, &quad, &[0, 3], &[0], err("final primitive offset must equal vertex count"));
    check(4, max, &quad, &[0, 4], &[], err("primitive kind count must match primitive count"));
    for bad in [-1, 4, 16777217] {
        check(4, max, &[0, bad, 2, 3], &[0, 4], &[0], err("vertex references an invalid point"));
    }
    check(4, max, &quad, &[0, 3, 2, 4], &[0, 0, 0], err("primitive offsets are not ordered"));
    for bad in [-1, 3] {
        check(4, max, &quad, &[0, 4], &[bad], Some(("E_TOPOLOGY_KIND", "Primitive kinds are 0 (polygon), 1 (open polyline) or 2 (closed polyline).".to_owned())));
    }
    check(4, max, &[0, 1], &[0, 2], &[0], err("primitive has too few vertices for its kind"));
    check(4, max, &[0], &[0, 1], &[1], err("primitive has too few vertices for its kind"));
    check(4, max, &[0, 1], &[0, 2], &[2], err("primitive has too few vertices for its kind"));
    check(4, max, &[0, 1], &[0, 2], &[1], None);
    check(4, 3, &quad, &[0, 4], &[0], err("polygon exceeds the supported vertex count"));
    check(16777218, max, &[16777217, 1, 2], &[0, 3], &[0], None);
    check(4294967295, max, &[4294967294, 1, 2], &[0, 3], &[0], None);
    let n = 1025;
    check(n, max, &(0..3 * n).map(|i| i % n).collect::<Vec<_>>(), &(0..=n).map(|i| i * 3).collect::<Vec<_>>(), &vec![0; n as usize], None);
    check(4, max, &quad, &[0, 4], &[0], None);
    let small = buffer(&[0]);
    for (expression, valid) in [("(int/add i (int/mod (frame/index) 1))", true), ("(int/add (if (int/eq i 1024) 1025 i) (int/mod (frame/index) 1))", false)] {
        let packed = program(expression);
        let r = run::create(None, &lease, &pipes, &u32_kernel(&packed));
        let output = tp::ok(run::dispatch(&r, &run::Options::default(), None, None, &prepare(&packed, 0.0)));
        let offsets = buffer(&[0, 1025]);
        let vb = run::buffer(&output, None).expect("buffer");
        let result = host::validate_topology(&h, (&vb, run::count(&output)), (&offsets, 2), (&small, 1), 1025, max);
        assert_eq!(run::Private::readback_bytes(&r), 0);
        match (result, valid) {
            (Ok(()), true) => {}
            (Err(d), false) => {
                assert_eq!(d.code, "E_TOPOLOGY");
                assert_eq!(d.message, "Topology: vertex references an invalid point");
            }
            (Err(d), true) => panic!("{}", rays_diagnostic::to_string(&d)),
            (Ok(()), false) => panic!("GPU-origin invalid point accepted"),
        }
        run::close(&r, false);
    }
    let before = host::stats(&h);
    match host::validate_topology(&h, (&small, 2), (&small, 1), (&small, 0), 1, 4) {
        Err(d) => assert_eq!(d.code, "E_TOPOLOGY"),
        Ok(()) => panic!("topology count exceeds storage"),
    }
    assert_eq!(host::stats(&h).validation_readback_bytes, before.validation_readback_bytes);
    host::close(&h);
    for x in buffers.borrow().iter() {
        let _ = b::destroy_buffer(x);
    }
    pipelines::close(&pipes);
}

/// uint32 outputs, inputs bound from a device buffer, gathers and scalars
fn outputs() {
    let Some((lease, pipes)) = setup() else { return };
    let lv = live(0.0);
    for expression in ["(+ (+ i 16777217) (int t))", "(+ 4294967295 (int/mod (int t) 1))", "(int/mod (+ i (int t)) 2)"] {
        let packed = program(expression);
        let msl = u32_kernel(&packed);
        assert_eq!(msl.output_uint32, vec![true]);
        let r = run::create(None, &lease, &pipes, &msl);
        let inputs = prepare(&packed, 0.0);
        let output = tp::ok(run::dispatch(&r, &run::Options::default(), None, None, &inputs));
        let before = run::Private::readback_bytes(&r);
        match run::keep(&output, None) {
            Err(d) => assert_eq!(d.code, "E_GPU_UINT32_KEEP"),
            Ok(_) => panic!("uint32 buffer escaped as float32 storage"),
        }
        assert_eq!(run::Private::readback_bytes(&r), before);
        let expected = force_strict(&packed, 0.0);
        let source = run::buffer(&output, None).expect("source");
        let consume = |packed: &packed::T, msl: &emit::Kernel, bound: &dyn Fn(i64) -> Option<b::Buffer>, bound_table: &dyn Fn(i64) -> Option<(b::Buffer, i64)>| {
            let consumer = run::create(None, &lease, &pipes, msl);
            let values = prepare(packed, 0.0);
            match run::dispatch(&consumer, &run::Options::default(), None, None, &values) {
                Err(d) => assert_eq!(d.code, "E_GPU_UINT32_BINDING"),
                Ok(_) => panic!("uint32 input silently uploaded float storage"),
            }
            let result = tp::ok(run::dispatch(&consumer, &run::Options::default(), Some(bound), Some(bound_table), &values));
            assert_eq!(format!("{:?}", tp::ok(run::readback(&result, None))), format!("{expected:?}"));
            let host_result = tp::ok(run::dispatch(&consumer, &run::Options { uint32_cpu: true, ..Default::default() }, None, None, &values));
            assert_eq!(format!("{:?}", tp::ok(run::readback(&host_result, None))), format!("{:?}", force_strict(packed, 0.0)));
            if msl.input_uint32.iter().any(|x| *x) {
                let exact = phost::Inputs { arrays: vec![Arc::new(vec![16777217.0, 4294967295.0])], count: 2, ..values.clone() };
                let output = tp::ok(run::dispatch(&consumer, &run::Options { uint32_cpu: true, ..Default::default() }, None, None, &exact));
                assert_eq!(ints(&tp::ok(run::readback(&output, None))), vec![16777217, 4294967295]);
                for invalid in [-1.0, 4294967296.0, 1.5, f64::NAN] {
                    let bad = phost::Inputs { arrays: vec![Arc::new(vec![invalid])], count: 1, ..values.clone() };
                    match run::dispatch(&consumer, &run::Options { uint32_cpu: true, ..Default::default() }, None, None, &bad) {
                        Err(d) => assert_eq!(d.code, "E_GPU_UINT32_RANGE"),
                        Ok(_) => panic!("invalid uint32 host input was uploaded"),
                    }
                }
                let retry = tp::ok(run::dispatch(&consumer, &run::Options { uint32_cpu: true, ..Default::default() }, None, None, &values));
                assert_eq!(format!("{:?}", tp::ok(run::readback(&retry, None))), format!("{:?}", force_strict(packed, 0.0)));
            }
            run::close(&consumer, false);
        };
        let copy = program("(+ i (int/mod (int t) 1))");
        let copy_kernel = tp::ok(emit::kernel(&emit::Options { fast: true, uint32_inputs: vec![0], uint32_outputs: vec![0], ..Default::default() }, &copy));
        let s2 = source.clone();
        consume(&copy, &copy_kernel, &move |_| Some(s2.clone()), &|_| None);
        let gather = tp::compile("(let* [table (array/range 1025 :type int)] (map (fn [(i : int)] (+ (array/nth table i) (int/mod (int t) 1))) (array/range 1025 :type int)))");
        let layout = tp::ok(emit::kernel(&emit::Options { fast: true, ..Default::default() }, &gather));
        let u = layout.gathers[0].0;
        let gk = tp::ok(emit::kernel(&emit::Options { fast: true, uint32_tables: vec![u], uint32_outputs: vec![0], ..Default::default() }, &gather));
        let (s3, n) = (source.clone(), inputs.count);
        consume(&gather, &gk, &|_| None, &move |index| if index == u { Some((s3.clone(), n)) } else { None });
        assert_eq!(run::Private::readback_bytes(&r), before);
        assert_eq!(format!("{:?}", tp::ok(run::readback(&output, None))), format!("{expected:?}"));
        assert_eq!(run::Private::readback_bytes(&r), before + inputs.count * 4);
        let planes = tp::ok(run::planes(&output, None));
        assert_eq!(planes, vec![ints(&expected).iter().map(|v| *v as f64).collect::<Vec<_>>()]);
        assert_eq!(run::Private::status_reads(&r), 1);
        let empty = tp::ok(run::dispatch(&r, &run::Options::default(), None, None, &phost::Inputs { count: 0, ..inputs.clone() }));
        assert_eq!(ints(&tp::ok(run::readback(&empty, None))), Vec::<i64>::new());
        assert_eq!(tp::ok(run::planes(&empty, None)), vec![Vec::<f64>::new()]);
        run::close(&r, false);
    }
    pipelines::close(&pipes);
}

/// uint32 scalars (frame words past 2^24), fused mixed outputs and out-of-range refusals
fn scalars_and_ranges() {
    let Some((lease, pipes)) = setup() else { return };
    let lv = { let mut f = live(16777217.0); f.frame = 16777217; f.tick = 16777217; f };
    for packed in [
        program("(int/add i (frame/index))"),
        tp::compile("(let* [n (int t)] (map (fn [(i : int)] (int/add i n)) (array/range 1025 :type int)))"),
    ] {
        let msl = tp::ok(emit::kernel(&emit::Options { fast: true, uint32_scalars: true, uint32_outputs: vec![0], ..Default::default() }, &packed));
        assert!(!msl.uint32_uniforms.is_empty());
        let r = run::create(None, &lease, &pipes, &msl);
        let values = tp::ok(phost::prepare(&packed, &phost::PrepareOptions { fast: true, ..Default::default() }, &lv));
        let output = tp::ok(run::dispatch(&r, &run::Options::default(), None, None, &values));
        let expected = tp::ok(packed::force(&packed, &lv, &packed::ForceOptions { strict: true, ..Default::default() }));
        assert_eq!(format!("{:?}", tp::ok(run::readback(&output, None))), format!("{expected:?}"));
        run::close(&r, false);
    }
    let integer = program("(+ (+ i 16777217) (int t))");
    let floats = program("(+ (float i) t)");
    let merged = flow_ir::core_fuse::merge(&[integer, floats]).expect("merge");
    let msl = tp::ok(emit::kernel(&emit::Options { fast: true, uint32_outputs: vec![0], ..Default::default() }, &merged));
    assert_eq!(msl.output_uint32, vec![true, false]);
    let r = run::create(None, &lease, &pipes, &msl);
    let lv0 = live(0.0);
    let inputs = tp::ok(phost::prepare(&merged, &phost::PrepareOptions { fast: true, ..Default::default() }, &lv0));
    let output = tp::ok(run::dispatch(&r, &run::Options::default(), None, None, &inputs));
    let expected = tp::ok(packed::force_all(&merged, &lv0, &packed::ForceOptions { strict: true, ..Default::default() }));
    for (index, e) in expected.iter().enumerate() {
        assert_eq!(format!("{:?}", tp::ok(run::readback(&output, Some(index as i64)))), format!("{e:?}"));
    }
    run::close(&r, false);
    for expression in ["(+ -1 (int/mod (int t) 1))", "(+ 4294967296 (int/mod (int t) 1))"] {
        let packed = program(expression);
        let r = run::create(None, &lease, &pipes, &u32_kernel(&packed));
        match run::dispatch(&r, &run::Options::default(), None, None, &prepare(&packed, 0.0)) {
            Err(d) => assert_eq!(d.code, "E_GPU_UINT32_RANGE"),
            Ok(_) => panic!("out-of-range uint32 output was published"),
        }
        run::close(&r, false);
    }
    pipelines::close(&pipes);
}

fn main() {
    common::window::run(&[
        ("compaction_cases", compaction_cases),
        ("resident_values", resident_values),
        ("kinds_of_values", kinds_of_values),
        ("holders", holders),
        ("topology", topology),
        ("outputs", outputs),
        ("scalars_and_ranges", scalars_and_ranges),
    ]);
}
