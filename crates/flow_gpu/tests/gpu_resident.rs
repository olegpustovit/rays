//! Native source map for  (the runner half is gpu_run.rs):
//! 474–587 -> resident_fold (all six calls at 888–901, unchanged capacities/levels);
//! 589–668 -> resident_value; 670–725 -> resident_history;
//! 727–801 -> counted; 803–874 -> compaction; 876–966 -> host_counters.
//! The owner-only wrong-domain Host.stats call is excluded by Host's checked !Send type;
//! no owner handles are migrated. All remaining source runtime assertions run on native OGPU.
use crate::common;
use common::{test_program as tp, Observe, Seen};
use flow_gpu::{host, pipelines, run};
use flow_ir::{core_draw, core_exec as c, core_ir, core_lower, packed::host as phost};
use flow_shader::emit;
use flow_syntax::value::{ResidualId, Value};
use ogpu_core::{backend as b, types::{BufferDescriptor, BufferUsage}};
use scene_command::marks::{Entry, Kind, PairAttr};
use std::sync::{Arc, atomic::{AtomicUsize, Ordering}};

// A future accidental Send implementation makes inference ambiguous (the native owner may not move).
const _: fn() = || {
    trait AmbiguousIfSend<A> { fn check() {} }
    impl<T: ?Sized> AmbiguousIfSend<()> for T {}
    struct Invalid;
    impl<T: ?Sized + Send> AmbiguousIfSend<Invalid> for T {}
    let _ = <host::T as AmbiguousIfSend<_>>::check;
};

fn start() -> Option<renderer::renderer::Gpu> {
    if !common::gpu() { return None; }
    let gpu = common::lease();
    if !b::capabilities(&renderer::renderer::gpu_device(&gpu)).compute_pipeline {
        println!("SKIP: backend lacks Compute_pipeline");
        return None;
    }
    Some(gpu)
}
fn draw_program(body: &str) -> (core_ir::Program, Option<ResidualId>) {
    let ws = tp::check_workspace(&format!("(workspace w (graph g :context draw {body}))"));
    let evaluation = tp::ok(flow_eval::r#static(&ws, false, None));
    let value = &evaluation.results.iter().find(|(name, _)| name == "g").expect("graph g").1;
    core_draw::lower(None, &evaluation.plan, value).expect("draw lowering")
}
fn prepare(p: &core_ir::Program) -> c::Prepared {
    c::prepare(p, false, true, &Default::default(), false)
}
fn points(prepared: &c::Prepared, root: Option<ResidualId>, resident: Option<Arc<dyn c::Resident>>, t: f64) -> (i64, Vec<f64>) {
    let buffers = c::Buffers::create();
    let mut live = input::at_time(t);
    live.size = (200, 100);
    let table = tp::ok(c::draw(prepared, &live, (200, 100), &buffers, &c::DrawOptions { root, resident, ..Default::default() }));
    match table.as_slice() {
        [Entry::MarkSet { kind: Kind::Point { position: PairAttr::Buffer(position), .. }, count }] => {
            let values = buffers.find(position.id).expect("point buffer");
            (*count, values[..(*count * 3) as usize].to_vec())
        }
        _ => panic!("resident fold: one point set expected: {table:?}"),
    }
}
fn array(v: &Value) -> &[f64] {
    match v {
        Value::Floats(1..=4, a) => a,
        _ => panic!("float array expected"),
    }
}

fn fold_case(gpu: &renderer::renderer::Gpu, n: i64, levels: i64, keep: &str, slot: &str, range: &str) {
    let body = format!("(let* [p (fold [b (map (fn [i] [(+ i (floor t)) (* i 2) 0]) (array/range {n}))] [d (range {levels})] (let* [kept (filter (fn [i] {keep}) (array/range (array/count b)))] (map (fn [i] (let* [q (array/nth b (int (array/nth kept {slot}))) ] [(+ q.x (+ 1 (floor t))) q.y 0])) (array/range {range}))))] (draw/points p))");
    let (program, root) = draw_program(&body);
    let expected = points(&prepare(&program), root, None, 0.0);
    let h = host::create(&|| 0.0, gpu);
    let base = host::resident(false, true, &h);
    let capacity = base.clone();
    let ran = Arc::new(AtomicUsize::new(0));
    let calls = ran.clone();
    let resident: Arc<dyn c::Resident> = Arc::new(Observe { base, one_by_one: true, seen: Box::new(move |seen| {
        let Seen::Invoke(Some(v)) = seen else { return };
        calls.fetch_add(1, Ordering::Relaxed);
        let block = match v {
            Value::IntArray(a) => a.len(),
            Value::Floats(1..=4, a) => a.len(),
            _ => usize::MAX,
        };
        assert!(block <= 4, "resident fold: a placeholder's block has its capacity");
        assert!(capacity.capacity(v).is_some(), "resident fold: a placeholder has no capacity");
    }) });
    let actual = points(&prepare(&program), root, Some(resident), 0.0);
    assert!(ran.load(Ordering::Relaxed) > 0, "resident fold: no region ran on the device");
    assert!(mem::bytes(&mem::plan_in(mem::Pool::Device, "flow_gpu.resident")) > 0, "resident fold: its kept buffers have no owner");
    assert_eq!(actual, expected, "resident fold: device points differ from CPU");
    let (bad_program, bad_root) = draw_program("(let* [p (fold [b (map (fn [i] [i (* i 2) 0]) (array/range 9000))] [d (range 2)] (map (fn [i] [(float (int/div (int i) (int/sub (frame/index) (frame/index)))) 0 0]) (array/range (array/count b))))] (draw/points p))");
    let mut live = input::at_time(0.0);
    live.size = (200, 100);
    let bad = c::draw(&prepare(&bad_program), &live, (200, 100), &c::Buffers::create(), &c::DrawOptions {
        root: bad_root, resident: Some(host::resident(false, true, &h)), ..Default::default()
    }).expect_err("resident fold must report integer division status");
    assert!(bad.code == "E_RANGE" && bad.message == "Integer division by zero.");
    let display = tp::fixtures(None, 4096).remove(0).1;
    let inputs = tp::ok(phost::prepare(&display, &phost::PrepareOptions { fast: true, ..Default::default() }, &input::at_time(1.0)));
    let backend = host::backend(&h);
    let kernels: Vec<_> = (0..2).map(|_| tp::ok(backend.prepare(true, &display))).collect();
    for _ in 0..4 {
        for kernel in &kernels { tp::ok(kernel.run(&inputs)); }
        assert_eq!(points(&prepare(&program), root, Some(host::resident(false, true, &h)), 0.0), expected,
            "resident fold: a later draw's device points differ from CPU");
    }
    println!("resident fold over {range}: {} device regions draw the CPU's {} points", ran.load(Ordering::Relaxed), expected.0);
    host::close(&h);
}
fn resident_fold() {
    let Some(gpu) = start() else { return };
    let keep = "(>= (int/mod (int i) 3) 1)";
    for (n, levels, predicate, slot, range) in [
        (9000, 3, keep, "(int i)", "(array/count kept)"),
        (9000, 3, keep, "(int i)", "(+ (* (array/count kept) 1) 0)"),
        (400_000, 3, keep, "(int i)", "(array/count kept)"),
        (9000, 6, "(= (int/mod (int i) 3) 1)", "(int/div (int i) 3)", "(* (array/count kept) 3)"),
        (9000, 3, keep, "(int i)", "(int/div (array/count kept) 2)"),
        (9000, 3, "(< (int i) 0)", "(int i)", "(array/count kept)"),
    ] { fold_case(&gpu, n, levels, predicate, slot, range); }
}

/// Sibling maps of one count run as one multi-output dispatch (`invoke_all`); a host reading int
/// arrays as uint32 needs every int output written as uint32, not only the first (here the second,
/// after a vec3). Before the fix the int output read back as f32 bit patterns.
fn resident_sibling_ints() {
    use flow_syntax::ty::Ty;
    let Some(gpu) = start() else { return };
    let positions = tp::compile("(map (fn [x] [(+ x 1.0) t 0]) (array/range 300))");
    let tripled = tp::compile("(map (fn [x] (int/mul (int (+ x t)) 3)) (array/range 300))");
    let merged = flow_ir::core_fuse::merge(&[positions, tripled.clone()]).expect("sibling regions merge");
    let live = input::at_time(1.0);
    let expected = tp::ok(phost::reference(&tripled, &live, None, None, None));
    let h = host::create(&|| 0.0, &gpu);
    for uint32 in [false, true] {
        let r = host::resident(uint32, true, &h);
        let tys = [Ty::Array(Box::new(Ty::Vec3)), Ty::Array(Box::new(Ty::Int))];
        let Some(Value::List(made)) = tp::ok(r.invoke_all(&merged, &tys, &[], &[], &live)) else { panic!("sibling dispatch declined") };
        assert_eq!(tp::ok(r.finish(&made[1])), expected, "sibling int output (uint32 {uint32})");
        r.close();
    }
    host::close(&h);
}
fn resident_history() {
    let Some(gpu) = start() else { return };
    let (program, root) = draw_program("(let* [p (fold [b (map (fn [i] [i (* i 2) 0]) (array/range 2000))] [d (range 3)] (let* [kept (filter (fn [i] (< (int/mod (int i) 8) (int t))) (array/range (array/count b)))] (map (fn [i] (let* [q (array/nth b (int (array/nth kept (int/div (int i) 2))))] [(+ q.x d) (+ q.y (mod i 2)) 0])) (array/range (* (array/count kept) 2)))))] (draw/points p))");
    let h = host::create(&|| 0.0, &gpu);
    let prepared = prepare(&program);
    let cuts = Arc::new(AtomicUsize::new(0));
    for t in [1.0, 6.0, 6.0, 2.0, 0.0, 1.0, 7.0, 8.0, 3.0, 8.0] {
        let expected = points(&prepare(&program), root, None, t);
        let cuts = cuts.clone();
        let resident: Arc<dyn c::Resident> = Arc::new(Observe { base: host::resident(false, true, &h), one_by_one: false, seen: Box::new(move |seen| {
            if let Seen::Finish(_, result) = seen {
                if result.as_ref().is_err_and(c::is_resident_overflow) { cuts.fetch_add(1, Ordering::Relaxed); }
            }
        }) });
        assert_eq!(points(&prepared, root, Some(resident), t), expected, "resident history at t={t}");
    }
    assert!(cuts.load(Ordering::Relaxed) > 0, "resident history: no draw outgrew its slots");
    host::close(&h);
}

// A device-mapped head has an exact length even though it still owns a count cell.
fn resident_unfold() {
    let Some(gpu) = start() else { return };
    let h = host::create(&|| 0.0, &gpu);
    let value = tp::composition_value(r#"(let* [
      starts (map (fn [(i : int)] [(float i) t 0.0]) (array/range 1025 :type int))
      trail (iterate 4 {:head starts :points starts}
        (fn [(walk : {:head (array vec3) :points (array vec3)})]
          (let* [previous walk.head
                 head (map (fn [(p : vec3)] (+ p [0.0 1.0 0.5])) previous)]
            {:head head :points (array/concat walk.points head)})))
      history trail.points] history)"#);
    let (_, program) = tp::composition_core(&value);
    let prepared = tp::prepare(&program);
    for t in [0.0, 1.0, 2.0] {
        let live = input::at_time(t);
        let expected = tp::ok(c::execute(&prepared, &live, &c::Options { strict: true, ..Default::default() }));
        let ran = Arc::new(AtomicUsize::new(0));
        let calls = ran.clone();
        let resident: Arc<dyn c::Resident> = Arc::new(Observe { base: host::resident(false, true, &h), one_by_one: false, seen: Box::new(move |seen| {
            if let Seen::Unfold(Some(_)) = seen { calls.fetch_add(1, Ordering::Relaxed); }
        }) });
        let actual = tp::ok(c::execute(&prepared, &live, &c::Options { strict: true, resident: Some(resident.clone()), ..Default::default() }));
        assert_eq!(tp::ok(c::host_value(&*resident, &actual)), expected);
        resident.close();
        assert_eq!(ran.load(Ordering::Relaxed), 1,
            "an exact device-mapped head must unfold");
    }
    host::close(&h);
}

fn resident_state() {
    let Some(gpu) = start() else { return };
    let h = host::create(&|| 0.0, &gpu);
    let value = tp::composition_value(r#"(state [previous (exact (array/vec3 262144 [0.0 0.0 0.0]))]
        (map (fn [(p : vec3)] (+ p [1.0 0.5 0.25])) previous))"#);
    let (root, program) = tp::composition_core(&value);
    let prepared = tp::prepare(&program);
    let state = flow_eval::create_state();
    let owner = mem::plan_in(mem::Pool::Device, "flow_gpu.held");
    let before = owner.bytes();
    let mut last = Value::NoGeo;
    for frame in 0..32 {
        let live = input::Frame { frame, tick: frame, ..input::at_time(frame as f64 / 60.) };
        tp::ok(flow_eval::residual::bind_live(root, &live, Some(&state)));
        let resident = host::resident(false, true, &h);
        last = tp::ok(c::execute(&prepared, &live, &c::Options { root: Some(root), strict: true, resident: Some(resident.clone()), ..Default::default() }));
        assert!(flow_eval::device::held_by_device(&last));
        resident.close();
        assert_eq!(host::stats(&h).payload_readback_bytes, 0, "state read back at frame {frame}");
        assert!(owner.bytes() - before <= 6 * run::size_class(262144 * 3 * 4), "old state buffers retained at frame {frame}: {} bytes", owner.bytes() - before);
    }
    let materialised = flow_eval::device::thaw(&last);
    assert_eq!(array(&materialised).len(), 262144 * 3);
    assert!(array(&materialised).chunks_exact(3).all(|p| p == [32.0, 16.0, 8.0]));
    // CPU placement can peek this frame's device result or advance the next frame.
    for frame in [31, 32] {
        let live = input::Frame { frame, tick: frame, ..input::at_time(frame as f64 / 60.) };
        tp::ok(flow_eval::residual::bind_live(root, &live, Some(&state)));
        let cpu = tp::ok(c::execute(&prepared, &live, &c::Options { root: Some(root), strict: true, ..Default::default() }));
        assert_eq!(array(&cpu).len(), 262144 * 3);
        let step = (frame + 1) as f64;
        assert!(array(&cpu).chunks_exact(3).all(|p| p == [step, step * 0.5, step * 0.25]));
    }
    host::close(&h);
}

fn same_identity(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::IntArray(a), Value::IntArray(b)) => Arc::ptr_eq(a, b),
        (Value::Floats(aw @ 1..=4, a), Value::Floats(bw, b)) if aw == bw => Arc::ptr_eq(a, b),
        _ => a == b,
    }
}
fn resident_value() {
    let Some(gpu) = start() else { return };
    let ws = tp::check_workspace(r#"(workspace w (graph g :context value
      (let* [tested (if (>= t 0.)
        (let* [mask (map (fn [(i : int)] (if (< (int/mod i 8) 5) 1 0))
                       (array/range (int t) :type int))
               kept (array/compact mask)
               points (map (fn [(i : int)] [(float i) 2.0 0.0]) kept)]
          {:points points :indices kept :count (array/count kept)})
        {:points (array/vec3 0) :indices (array/int 0) :count 0})] 0.)))"#);
    let evaluation = tp::ok(flow_eval::r#static(&ws, true, None));
    let records = &evaluation.records.iter().find(|(path, _)| path == &["g".to_owned(), "tested".to_owned()]).expect("tested record").1;
    let value = &records[0].1;
    let program = core_lower::lower(None, None, true, None, None, tp::residual(value)).expect("record lowering");
    let prepared = tp::prepare(&program);
    let h = host::create(&|| 0.0, &gpu);
    for count in [9000, 5001, 0, 16385, 1025] {
        for uint32 in [false, true] {
            let r = host::resident(uint32, true, &h);
            let before = host::stats(&h);
            let invoked = Arc::new(AtomicUsize::new(0));
            let reads = Arc::new(AtomicUsize::new(0));
            let calls = invoked.clone();
            let read_calls = reads.clone();
            let resident: Arc<dyn c::Resident> = Arc::new(Observe { base: r, one_by_one: true, seen: Box::new(move |seen| match seen {
                Seen::Invoke(Some(_)) => { calls.fetch_add(1, Ordering::Relaxed); }
                Seen::Finish(value, Ok(actual)) if !same_identity(actual, value) => { read_calls.fetch_add(1, Ordering::Relaxed); }
                _ => {}
            }) });
            let live = input::at_time(count as f64);
            let result = tp::ok(c::execute(&prepared, &live, &c::Options { strict: true, resident: Some(resident.clone()), ..Default::default() }));
            if count >= 256 {
                assert!(invoked.load(Ordering::Relaxed) >= 2);
                assert_eq!(reads.load(Ordering::Relaxed), 0);
            }
            let published = host::stats(&h);
            assert_eq!(published.payload_readback_bytes, before.payload_readback_bytes);
            if count >= 256 { assert!(published.count_readback_bytes > before.count_readback_bytes); }
            let materialised = tp::ok(c::host_value(&*resident, &result));
            assert_eq!(materialised, tp::ok(flow_eval::host::force_reference(value, &live, None, None, None)));
            let read = host::stats(&h);
            if count >= 256 { assert!(read.payload_readback_bytes > published.payload_readback_bytes); }
            assert_eq!(read.readback_bytes, read.payload_readback_bytes + read.bounds_readback_bytes);
            assert_eq!(read.status_readback_bytes, 4 * read.status_reads);
            resident.close();
        }
    }
    host::close(&h);
}

fn buffer(device: &b::Device, bytes: i64) -> b::Buffer {
    b::create_buffer(device, BufferDescriptor {
        label: Some("resident oracle".into()), size: bytes.max(4),
        usage: vec![BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst],
    }, None).expect("native buffer")
}
fn counted() {
    let Some(gpu) = start() else { return };
    let device = renderer::renderer::gpu_device(&gpu);
    let queue = renderer::renderer::gpu_queue(&gpu);
    let pipes = pipelines::create(&|| 0.0, &device);
    let program = tp::fixtures(None, 4096).remove(0).1;
    let values = tp::ok(phost::prepare(&program, &phost::PrepareOptions { fast: true, ..Default::default() }, &input::at_time(1.0)));
    let kernel = tp::ok(emit::kernel(&emit::Options { fast: true, ..Default::default() }, &program));
    let runner = run::create(None, &gpu, &pipes, &kernel);
    let full = tp::ok(run::dispatch(&runner, &Default::default(), None, None, &values));
    let width = run::width(&full);
    let expected = tp::ok(run::readback(&full, None));
    for (count, mul, div) in [(0,1,1), (1,1,1), (255,1,1), (256,1,1), (257,1,1), (4096,1,1), (9999,1,1), (100,3,1), (171,3,2), (513,1,2)] {
        let n = values.count.min(count * mul / div);
        let out = buffer(&device, values.count * width * 4);
        let record = buffer(&device, 24);
        let cell = buffer(&device, 4);
        b::write_buffer(&out, 0, &vec![0xff; (values.count * width * 4) as usize]).expect("garbage output");
        b::write_buffer(&record, 0, &[7; 24]).expect("garbage count record");
        b::write_buffer(&cell, 0, &(count as u32).to_le_bytes()).expect("count cell");
        let commands = b::begin_commands(&queue).expect("commands");
        tp::ok(run::dispatch(&runner, &run::Options { commands: Some(commands.clone()), count: Some((cell.clone(), mul, div)), into: Some((out.clone(), record.clone())), ..Default::default() }, None, None, &values));
        let receipt = b::commit(&commands).expect("commit");
        b::complete_through(&queue, receipt.epoch).expect("complete");
        let words = b::read_buffer(&record, 0, 24).expect("count record");
        let words: Vec<i64> = words.chunks_exact(4).map(|w| u32::from_le_bytes(w.try_into().unwrap()) as i64).collect();
        assert_eq!(words, [n, 6*n, 1, 0, 0, 0], "counted {count} * {mul} / {div}");
        let actual = tp::ok(run::read_floats(&queue, &out, 0, values.count * width));
        assert_eq!(&actual[..(n * width) as usize], &array(&expected)[..(n * width) as usize]);
        for b in [&out, &record, &cell] { b::destroy_buffer(b).expect("destroy"); }
    }
    run::close(&runner, false);
    pipelines::close(&pipes);
}
fn compaction() {
    let Some(gpu) = start() else { return };
    let device = renderer::renderer::gpu_device(&gpu);
    let queue = renderer::renderer::gpu_queue(&gpu);
    let pipes = pipelines::create(&|| 0.0, &device);
    for n in [0i64, 1, 1023, 1024, 1025, 300_000] {
        for mode in 0..4 {
            let count_in = match mode { 1 => Some(n * 5 / 6), 3 => Some(n / 2), _ => None };
            let mask: Vec<f32> = (0..n).map(|i| {
                let keep = match mode { 2 => true, 3 => false, _ => (((i as u64 * 2654435761) >> 13) & 7) < 3 };
                if keep { 1.0 } else { 0.0 }
            }).collect();
            let m = buffer(&device, 4*n);
            let kept = buffer(&device, 4*n);
            let total = buffer(&device, 4);
            let cell = buffer(&device, 4);
            let blocks = buffer(&device, run::compact_blocks(n));
            let groups = buffer(&device, 12);
            let mut bytes = vec![0; (4*n).max(4) as usize];
            for (word, value) in bytes.chunks_exact_mut(4).zip(&mask) { word.copy_from_slice(&value.to_le_bytes()); }
            b::write_buffer(&m, 0, &bytes).expect("mask");
            let count = count_in.unwrap_or(n);
            b::write_buffer(&cell, 0, &(count as u32).to_le_bytes()).expect("count");
            let commands = b::begin_commands(&queue).expect("commands");
            tp::ok(run::compact(false, None, &commands, &pipes, &m, n, count_in.map(|_| &cell), &kept, &total, &blocks, &groups));
            let receipt = b::commit(&commands).expect("commit");
            b::complete_through(&queue, receipt.epoch).expect("complete");
            let expected: Vec<f64> = (0..count).filter(|i| mask[*i as usize] != 0.0).map(|i| i as f64).collect();
            let word = b::read_buffer(&total, 0, 4).expect("total");
            assert_eq!(u32::from_le_bytes(word.try_into().unwrap()) as usize, expected.len(), "compaction n={n} mode={mode}");
            let kept_bytes = b::read_buffer(&kept, 0, (4*expected.len()).max(4) as i64).expect("kept");
            let actual: Vec<f64> = kept_bytes.chunks_exact(4).take(expected.len()).map(|word| f32::from_le_bytes(word.try_into().unwrap()) as f64).collect();
            assert_eq!(actual, expected, "compaction n={n} mode={mode}");
            for b in [&m, &kept, &total, &cell, &blocks, &groups] { b::destroy_buffer(b).expect("destroy"); }
        }
    }
    pipelines::close(&pipes);
}

fn host_counters() {
    let Some(gpu) = start() else { return };
    drop(gpu);
    common::release_keepalive();
    let baseline = ogpu_core::impl_::live_handles().expect("native handles");
    {
        let Some(gpu) = start() else { return };
        let h = host::create(&|| 0.0, &gpu);
        let program = tp::fixtures(None, 1024).remove(0).1;
        let inputs = tp::ok(phost::prepare(&program, &Default::default(), &input::at_time(1.0)));
        let backend = host::backend(&h);
        let first = tp::ok(backend.prepare(false, &program));
        let value = tp::ok(first.run(&inputs));
        tp::ok(first.readback(&value));
        assert!(tp::ok(host::output(&h, &value)).is_some());
        let initial = host::stats(&h);
        assert_eq!((initial.runners_created, initial.runners_released, initial.status_reads), (1, 0, 1));
        // a dropped kernel releases its runner at once (a region prepared again every frame would
        // otherwise leave 64 dead producers with their buffers)
        drop(tp::ok(backend.prepare(false, &program)));
        let dropped = host::stats(&h);
        assert_eq!((dropped.runners_created, dropped.runners_released), (2, 1));
        // 64 live kernels evict the first one's runner
        let mut live = Vec::new();
        for _ in 0..64 {
            let kernel = tp::ok(backend.prepare(false, &program));
            tp::ok(kernel.run(&inputs));
            live.push(kernel);
        }
        let evicted = host::stats(&h);
        assert!(evicted.runners_created >= 65);
        assert_eq!(evicted.runners_released, evicted.runners_created - 64);
        // The program and its shared device-generated float range each compile once.
        assert_eq!((evicted.pipeline_compilations, evicted.status_reads), (2, 65));
        assert_eq!(evicted.buffer_creations, 65 * initial.buffer_creations);
        assert_eq!(evicted.input_uploads, 65 * initial.input_uploads);
        assert_eq!(evicted.input_uploaded_bytes, 65 * initial.input_uploaded_bytes);
        assert_eq!(evicted.readback_bytes, initial.readback_bytes);
        assert_eq!(initial.readback_bytes, 4096);
        assert!(tp::ok(host::output(&h, &value)).is_none());
        tp::ok(first.run(&inputs));
        let renewed = host::stats(&h);
        assert_eq!(renewed.runners_created, evicted.runners_created + 1);
        assert_eq!(renewed.runners_released, evicted.runners_released + 1);
        assert_eq!(renewed.status_reads, 66);
        host::close(&h);
        host::close(&h);
        let closed = host::stats(&h);
        assert_eq!(closed.runners_created, renewed.runners_created);
        assert_eq!(closed.runners_released, closed.runners_created);
        assert_eq!((closed.pipeline_compilations, closed.pipeline_releases), (2, 2));
        assert_eq!(closed.status_reads, renewed.status_reads);
        assert_eq!(closed.readback_bytes, initial.readback_bytes);
        assert_eq!(closed.buffer_creations, renewed.buffer_creations);
        assert_eq!(closed.input_uploads, renewed.input_uploads);
        assert_eq!(closed.input_uploaded_bytes, renewed.input_uploaded_bytes);
    }
    common::release_keepalive();
    assert_eq!(ogpu_core::impl_::live_handles().expect("native handles"), baseline);
}
fn resident_scenarios() {
    let Some(gpu) = start() else { return };
    drop(gpu);
    common::release_keepalive();
    let baseline = ogpu_core::impl_::live_handles().expect("native handles");
    resident_fold();
    resident_sibling_ints();
    resident_history();
    resident_value();
    counted();
    compaction();
    host_counters();
    common::release_keepalive();
    assert_eq!(ogpu_core::impl_::live_handles().expect("native handles"), baseline);
}
pub const CASES: &[(&str, fn())] = &[("resident_state", resident_state), ("resident_unfold", resident_unfold), ("resident_scenarios", resident_scenarios)];
