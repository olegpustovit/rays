//! GPU-only construction of deterministic incidence/fan tables and packed coordinate planes.
use crate::common;
use flow_gpu::{pipelines, run};
use flow_ir::flow_ir::executor;
use flow_ir::flow_ir::gpu::Residency as _;
use flow_syntax::{ty::Ty, value::Value};
use ogpu_core::{backend as b, types::{BufferDescriptor, BufferUsage}};

fn buffer(device: &b::Device, words: i64) -> b::Buffer {
    b::create_buffer(device, BufferDescriptor {
        label: Some("incidence regression".into()), size: 4 * words.max(1),
        usage: vec![BufferUsage::Storage, BufferUsage::CopySrc, BufferUsage::CopyDst],
    }, None).unwrap()
}
fn write(buffer: &b::Buffer, values: &[u32]) {
    if !values.is_empty() { b::write_buffer(buffer, 0, &values.iter().flat_map(|x| x.to_le_bytes()).collect::<Vec<_>>()).unwrap(); }
}
fn read(buffer: &b::Buffer, words: usize) -> Vec<u32> {
    if words == 0 { return Vec::new(); }
    b::read_buffer(buffer, 0, words as i64 * 4).unwrap().chunks_exact(4).map(|w| u32::from_le_bytes(w.try_into().unwrap())).collect()
}
fn incidence() {
    assert!(common::gpu());
    let gpu = common::lease();
    let device = renderer::renderer::gpu_device(&gpu);
    let queue = renderer::renderer::gpu_queue(&gpu);
    let pipes = pipelines::create(&|| 0., &device);
    let mut cases = vec![(0, 0), (7, 0), (1, 1), (37, 341), (37, 342), (37, 343), (1025, 2003), (131071, 21845)];
    if std::env::var_os("RAYS_SLOW").is_some() { cases.push((1_000_000, 1_996_002)); }
    for (points, primitives) in cases {
        let nv = primitives * 3;
        let vertices: Vec<u32> = (0..nv).map(|v| ((v as u64 * 2654435761 >> 13) % points as u64) as u32).collect();
        let offsets: Vec<u32> = (0..=primitives).map(|p| p as u32 * 3).collect();
        let kinds: Vec<u32> = (0..primitives).map(|p| p as u32 % 3).collect();
        let v = buffer(&device, nv); let o = buffer(&device, primitives + 1); let k = buffer(&device, primitives);
        write(&v, &vertices); write(&o, &offsets); write(&k, &kinds);
        let point_offsets = buffer(&device, points + 1);
        let point_vertices = buffer(&device, nv);
        let primitive_of_vertex = buffer(&device, nv);
        let first = buffer(&device, nv); let corners = buffer(&device, nv);
        let scratch = buffer(&device, nv); let blocks = buffer(&device, run::compact_blocks(nv) / 4 + 1);
        let out = run::Incidence { point_offsets: &point_offsets, point_vertices: &point_vertices,
            primitive_of_vertex: &primitive_of_vertex, first: &first, corners: &corners };
        let began = std::time::Instant::now();
        run::point_incidence(&queue, &pipes, points, (&v, nv), (&o, primitives + 1), (&k, primitives), &out, &scratch, &blocks).unwrap();
        println!("incidence {points} points, {nv} vertices: {:?}", began.elapsed());
        let mut expected: Vec<u32> = (0..nv as u32).collect();
        expected.sort_by_key(|v| (vertices[*v as usize], *v));
        let mut expected_offsets = vec![0u32; points as usize + 1];
        for p in &vertices { expected_offsets[*p as usize + 1] += 1; }
        for p in 0..points as usize { expected_offsets[p + 1] += expected_offsets[p]; }
        let expected_first: Vec<u32> = expected.iter().map(|v| v / 3 * 3).collect();
        let expected_corners: Vec<u32> = expected.iter().map(|v| if kinds[*v as usize / 3] == 0 { 3 } else { 0 }).collect();
        assert_eq!(read(&point_vertices, nv as usize), expected);
        assert_eq!(read(&point_offsets, points as usize + 1), expected_offsets);
        assert_eq!(read(&primitive_of_vertex, nv as usize), (0..nv as u32).map(|v| v / 3).collect::<Vec<_>>());
        assert_eq!(read(&first, nv as usize), expected_first);
        assert_eq!(read(&corners, nv as usize), expected_corners);
        // Reuse dirty scratch and outputs: no result depends on zero-initialized storage.
        run::point_incidence(&queue, &pipes, points, (&v, nv), (&o, primitives + 1), (&k, primitives), &out, &scratch, &blocks).unwrap();
        assert_eq!(read(&point_vertices, nv as usize), expected);
        for b in [&v, &o, &k, &point_offsets, &point_vertices, &primitive_of_vertex, &first, &corners, &scratch, &blocks] { b::destroy_buffer(b).unwrap(); }
    }
    pipelines::close(&pipes);
}
fn planes() {
    assert!(common::gpu());
    let gpu = common::lease();
    let device = renderer::renderer::gpu_device(&gpu);
    let queue = renderer::renderer::gpu_queue(&gpu);
    let pipes = pipelines::create(&|| 0., &device);
    for count in [0, 1, 255, 256, 257, 65537] {
        let x = buffer(&device, count); let y = buffer(&device, count); let z = buffer(&device, count);
        let out = buffer(&device, count * 3);
        let values: Vec<Vec<u32>> = [1., -0., -0.25].into_iter().map(|step| (0..count).map(|i| (i as f32 * step).to_bits()).collect()).collect();
        for (b, data) in [&x, &y, &z].into_iter().zip(&values) { write(b, data); }
        run::pack_planes(&queue, &pipes, [&x, &y, &z], &out, count).unwrap();
        assert_eq!(read(&out, count as usize * 3), (0..count as usize).flat_map(|i| values.iter().map(move |v| v[i])).collect::<Vec<_>>());
        assert!(run::pack_planes(&queue, &pipes, [&x, &y, &z], &out, count + 1).is_err());
        for b in [&x, &y, &z, &out] { b::destroy_buffer(b).unwrap(); }
    }
    pipelines::close(&pipes);
}
fn constant_arrays() {
    assert!(common::gpu());
    let gpu = common::lease();
    let host = flow_gpu::host::create(&|| 0., &gpu);
    for count in [0, 1, 1025] {
        for (kind, literal, row) in [("float", "-0.", vec![-0.]), ("vec3", "[1.25 -0. -3.]", vec![1.25, -0., -3.]),
            ("vec4", "[1. -2. 3. -4.]", vec![1., -2., 3., -4.]), ("range", ":type \"float\"", vec![0.])]
        {
            let body = format!("(array/{kind} (int/add {count} (frame/index)) {literal})");
            let ws = common::test_program::check_workspace(&format!("(workspace w (graph g :context value (let* [tested {body}] 0.)))"));
            let evaluated = flow_eval::r#static(&ws, true, None).unwrap();
            let value = &evaluated.records.iter().find(|(p, _)| p == &["g", "tested"]).unwrap().1[0].1;
            let compiled = executor::compile(value, &executor::CompileOptions { strict: true, ..Default::default() }).unwrap();
            let scope = flow_gpu::host::backend(&host).residency(true).unwrap();
            let before = flow_gpu::host::stats(&host);
            let made = executor::force(&compiled, &executor::ForceOptions { strict: true, resident: Some(scope.executor().clone()), ..Default::default() }, &input::at_time(1.)).unwrap();
            assert_eq!(scope.executor().count(&made).unwrap(), Some(count));
            let kept = scope.retain_float(&made).expect("constant array stayed on the CPU");
            scope.executor().close();
            let after = flow_gpu::host::stats(&host);
            assert_eq!(after.input_uploaded_bytes, before.input_uploaded_bytes);
            assert_eq!(after.payload_readback_bytes, before.payload_readback_bytes);
            assert_eq!((kept.count, kept.width), (count, row.len() as i64));
            let planes = kept.materialise().unwrap();
            for (column, expected) in planes.iter().zip(row) {
                let expected = if kind == "range" { (0..count).map(|i| (i as f64).to_bits()).collect() } else { vec![f64::to_bits(expected); count as usize] };
                assert_eq!(column.iter().map(|x| x.to_bits()).collect::<Vec<_>>(), expected);
            }
            kept.release(false);
        }
    }
    let scope = flow_gpu::host::resident_scope(true, true, &host);
    assert!(scope.executor().fill(&Ty::Array(Box::new(Ty::Float)), 1, &Value::Float(f64::MAX)).unwrap().is_none());
    assert!(scope.executor().fill(&Ty::Array(Box::new(Ty::Text)), 1, &Value::Text(Vec::new())).unwrap().is_none());
    scope.executor().close();
    flow_gpu::host::close(&host);
}
pub const CASES: &[(&str, fn())] = &[("incidence", incidence), ("planes", planes), ("constant_arrays", constant_arrays)];
