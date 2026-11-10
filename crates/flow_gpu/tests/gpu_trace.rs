//! GPU scene tracing compares primitive/instance/hit exactly and t/uv/normal within 1e-5. Static midpoint scenes qualify devices without motion tracing. Fixtures cover triangles, instances, procedural spheres, motion and strands.
use crate::common;
use common::test_program as tp;
use flow_gpu::{pipelines, run};
use flow_ir::packed::{self, host as phost};
use flow_shader::emit::{self, Curves};
use flow_syntax::value::Value;
use rdk_core::packed::float3;
use rdk_core::{geometry, topology};

const COUNT: usize = 256;
const SPHERES: [(f64, f64, f64, f64); 2] = [(0.3, 0.2, 1.0, 0.4), (-0.5, -0.4, 0.5, 0.3)];

type Points = Vec<[f64; 3]>;

/// The two triangles of a prototype (columns 2, rows 2, in the xy plane at z 0).
fn grid(size: f64) -> Points {
    let h = size / 2.0;
    vec![[-h, -h, 0.0], [h, -h, 0.0], [h, h, 0.0], [-h, h, 0.0]]
}

const TRIANGLES: [[usize; 3]; 2] = [[0, 1, 2], [0, 2, 3]];

fn mesh(points: &Points) -> geometry::T {
    let positions = float3::of_owned(points.iter().map(|p| p[0]).collect(), points.iter().map(|p| p[1]).collect(), points.iter().map(|p| p[2]).collect()).expect("positions");
    let top = topology::polygons_owned(points.len() as i64, TRIANGLES.iter().flatten().map(|i| *i as u32).collect(), vec![0, 3, 6]).expect("topology");
    geometry::create(positions, top, None, None, None).expect("mesh")
}

/// the two triangles of a prototype in primitive order, as packed float3s
fn vertex_bytes(points: &Points) -> Vec<u8> {
    TRIANGLES.iter().flatten().flat_map(|i| points[*i].iter().flat_map(|v| (*v as f32).to_le_bytes())).collect()
}

fn floats(values: &[f64]) -> Vec<u8> {
    values.iter().flat_map(|v| (*v as f32).to_le_bytes()).collect()
}

fn sphere_data() -> Vec<u8> {
    floats(&SPHERES.iter().flat_map(|(x, y, z, r)| [*x, *y, *z, *r]).collect::<Vec<_>>())
}

fn program(id: i64) -> packed::T {
    tp::compile(&format!(
        "(map (fn [(i : int)] (let* [x (- (* 0.15 (float (int/mod i 16))) 1.2) y (- (* 0.15 (float (int/div i 16))) 1.17)\n      h (scene/trace {id} [x y 5.] [0. 0. -1.] 0. 100. t)]\n    [h.t (float h.prim) (+ (float h.instance) (+ h.u h.v))\n     (if h.hit (+ 1. (+ (* h.normal.x 0.3) (+ (* h.normal.y 0.6) (* h.normal.z h.normal.z)))) 0.)])) (array/range {COUNT} :type int))"
    ))
}

fn values(program: &packed::T, time: f64) -> phost::Inputs {
    tp::ok(phost::prepare(program, &phost::PrepareOptions { fast: true, ..Default::default() }, &input::at_time(time)))
}

fn cpu(program: &packed::T, time: f64) -> Vec<f64> {
    match tp::ok(packed::force(program, &input::at_time(time), &Default::default())) {
        Value::Floats(4, v) => (*v).clone(),
        _ => panic!("vec4 array expected"),
    }
}

struct Run {
    caps: Option<ogpu_core::caps::Caps>,
    cpu_time: Option<f64>,
    tolerance: f64,
}

/// the GPU run at each `time` against the CPU host at `cpu_time` (or `time`)
fn compare_runs(r: Run, name: &str, id: i64, description: &run::Description, times: &[f64]) -> run::Built {
    let lease = common::lease();
    let built = tp::ok(run::build_scene(r.caps.as_ref(), &lease, description));
    let program = program(id);
    let pipes = pipelines::create(&|| 0.0, &renderer::renderer::gpu_device(&lease));
    let msl = tp::ok(emit::kernel(&emit::Options { fast: true, instanced: built.instanced, motion: built.motion, curves: built.curves, ..Default::default() }, &program));
    let runner = run::create(None, &lease, &pipes, &msl);
    for &time in times {
        let expected = cpu(&program, r.cpu_time.unwrap_or(time));
        let out = tp::ok(run::dispatch(
            &runner,
            &run::Options { scene: Some(built.scene.clone()), extras: Some(built.extras.clone()), ..Default::default() },
            None,
            None,
            &values(&program, time),
        ));
        let actual = match tp::ok(run::readback(&out, None)) {
            Value::Floats(4, v) => (*v).clone(),
            _ => panic!("vec4 array expected"),
        };
        let (mut worst, mut hits) = (0.0f64, 0);
        for i in 0..COUNT {
            if expected[4 * i + 3] != 0.0 {
                hits += 1;
            }
            // prim and hit/miss agree exactly; the instance (z's integer part) within the 1e-5 below
            let same = actual[4 * i + 1] == expected[4 * i + 1] && (actual[4 * i + 3] != 0.0) == (expected[4 * i + 3] != 0.0);
            if !same {
                println!("ray {i}: gpu {:?} cpu {:?}", &actual[4 * i..4 * i + 4], &expected[4 * i..4 * i + 4]);
            }
            assert!(same, "{name}: ray {i}");
            for k in 0..4 {
                let d = (actual[4 * i + k] - expected[4 * i + k]).abs();
                if d > r.tolerance {
                    println!("ray {i} field {k}: gpu {:.7} cpu {:.7}", actual[4 * i + k], expected[4 * i + k]);
                }
                worst = worst.max(d);
            }
        }
        println!("scene/trace ({name}, time {time}): {hits} of {COUNT} rays hit, prim exact, max abs difference {worst:.3e}");
        assert!(hits > 0 && worst <= r.tolerance, "{name}: hits {hits} worst {worst}");
    }
    run::close(&runner, false);
    pipelines::close(&pipes);
    (built.release)();
    built
}

fn default_run() -> Run {
    Run { caps: None, cpu_time: None, tolerance: 1e-5 }
}

fn tilted(points: &Points, angle: f64, shift: f64) -> Points {
    let (s, c) = angle.sin_cos();
    points.iter().map(|p| [p[0] + shift, c * p[1] - s * p[2] + 0.1, s * p[1] + c * p[2] + 0.5]).collect()
}

fn skip() -> bool {
    if !common::gpu() {
        return true;
    }
    let caps = ogpu_core::backend::capabilities(&renderer::renderer::gpu_device(&common::lease()));
    if !caps.ray_tracing || !caps.ray_tracing_motion {
        println!("SKIP: backend lacks ray tracing / motion");
        return true;
    }
    false
}

fn flat() {
    if skip() {
        return;
    }
    let lease = common::lease();
    let a = grid(2.0);
    flow_sop::trace_scene::register(&SPHERES, &[], None, 9, &mesh(&a)).expect("register");
    compare_runs(
        default_run(),
        "flat, triangles + spheres",
        9,
        &run::Description { triangles: vertex_bytes(&a), triangles_end: None, instances: vec![], sphere_data: sphere_data(), strand_data: vec![] },
        &[1.0],
    );
    let ending = tilted(&a, 0.5, 0.3);
    flow_sop::trace_scene::register(&SPHERES, &[], Some(&mesh(&ending)), 9, &mesh(&a)).expect("register moving");
    let moving = run::Description { triangles: vertex_bytes(&a), triangles_end: Some(vertex_bytes(&ending)), instances: vec![], sphere_data: sphere_data(), strand_data: vec![] };
    assert!(compare_runs(default_run(), "flat, vertex motion", 9, &moving, &[0.0, 0.35, 1.0, 1.5]).motion);
    let mut caps = ogpu_core::backend::capabilities(&renderer::renderer::gpu_device(&lease));
    caps.ray_tracing_motion = false;
    assert!(!compare_runs(Run { caps: Some(caps), cpu_time: Some(0.5), tolerance: 1e-5 }, "flat, shutter midpoint without motion", 9, &moving, &[0.0, 1.0]).motion);
    flow_sop::trace_scene::unregister(9);
}

/// strands over the flat grid: a tilted span, a long thin one, and an upright one whose top cap the rays meet
const STRANDS: [(f64, f64, f64, f64, f64, f64, f64); 3] = [(-1.3, 0.1, 1.0, 1.3, 0.35, 1.2, 0.08), (0.6, -1.0, 0.8, 0.7, 0.9, 0.5, 0.05), (-0.45, -0.6, 0.2, -0.45, -0.6, 1.5, 0.1)];

fn stranded() {
    if skip() {
        return;
    }
    let lease = common::lease();
    let a = grid(2.0);
    flow_sop::trace_scene::register(&SPHERES, &STRANDS, None, 13, &mesh(&a)).expect("register");
    let strand_data = floats(&STRANDS.iter().flat_map(|(ax, ay, az, bx, by, bz, r)| [*ax, *ay, *az, *r, *bx, *by, *bz, *r]).collect::<Vec<_>>());
    let description = run::Description { triangles: vertex_bytes(&a), triangles_end: None, instances: vec![], sphere_data: sphere_data(), strand_data };
    let caps = ogpu_core::backend::capabilities(&renderer::renderer::gpu_device(&lease));
    if caps.ray_tracing_curves {
        assert_eq!(compare_runs(Run { tolerance: 1e-4, ..default_run() }, "flat + native curve strands", 13, &description, &[1.0]).curves, Some(Curves::Native));
    } else {
        // below the curves feature their kernel still compiles to a pipeline
        let pipes = pipelines::create(&|| 0.0, &renderer::renderer::gpu_device(&lease));
        let msl = tp::ok(emit::kernel(&emit::Options { fast: true, curves: Some(Curves::Native), ..Default::default() }, &program(13)));
        tp::ok(pipelines::get(&pipes, &msl));
        pipelines::close(&pipes);
        println!("scene/trace (flat + native curve strands): compiled; the device lacks ray_tracing_curves");
    }
    let mut noncurves = caps;
    noncurves.ray_tracing_curves = false;
    assert_eq!(compare_runs(Run { caps: Some(noncurves), ..default_run() }, "flat + capsule strands", 13, &description, &[1.0]).curves, Some(Curves::Loop));
    flow_sop::trace_scene::unregister(13);
}

fn matmul3(x: &[f64; 9], y: &[f64; 9]) -> [f64; 9] {
    let mut m = [0.0; 9];
    for k in 0..9 {
        let (r, q) = (k / 3, k % 3);
        m[k] = x[3 * r] * y[q] + x[3 * r + 1] * y[3 + q] + x[3 * r + 2] * y[6 + q];
    }
    m
}

fn instanced() {
    if skip() {
        return;
    }
    let lease = common::lease();
    let (a, b) = (grid(2.0), grid(1.0));
    let (rotation, tilt): (f64, f64) = (0.5, 0.4);
    // Rx(tilt) * diag(1, 2, 1), then Rz(rotation), then a translation: non-uniform, so the normal
    // needs the inverse transpose
    let (c, s, cr, sr) = (tilt.cos(), tilt.sin(), rotation.cos(), rotation.sin());
    let m = matmul3(&[cr, -sr, 0.0, sr, cr, 0.0, 0.0, 0.0, 1.0], &matmul3(&[1.0, 0.0, 0.0, 0.0, c, -s, 0.0, s, c], &[1.0, 0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 0.0, 1.0]));
    let transforms: Vec<Vec<f64>> = vec![
        vec![1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0],
        vec![m[0], m[1], m[2], 0.3, m[3], m[4], m[5], 0.2, m[6], m[7], m[8], 1.0],
        vec![0.4, 0.0, 0.0, -1.0, 0.0, 0.4, 0.0, -0.8, 0.0, 0.0, 0.4, -0.5],
    ];
    // the shutter's end: the first instance tilts and slides, the second squashes, the third
    // stays; x=0.42 keeps the sample grid off the prototype's exact outer edge.
    let ends: Vec<Vec<f64>> = vec![
        vec![1.0, 0.0, 0.0, 0.42, 0.0, 0.8, -0.6, 0.0, 0.0, 0.6, 0.8, 0.3],
        transforms[1].iter().enumerate().map(|(k, v)| if k == 4 || k == 5 || k == 6 { v * 0.5 } else { *v }).collect(),
        transforms[2].clone(),
    ];
    let prototypes = [&a, &b, &a];
    let firsts = [0, 2, 0];
    let description = |transform_end: &dyn Fn(usize) -> Option<Vec<f64>>| run::Description {
        triangles: [vertex_bytes(&a), vertex_bytes(&b)].concat(),
        triangles_end: None,
        instances: transforms.iter().enumerate().map(|(k, t)| run::Instance { first: firsts[k], triangles: 2, transform: t.clone(), transform_end: transform_end(k) }).collect(),
        sphere_data: sphere_data(),
        strand_data: vec![],
    };
    let register = |motion: Option<&[Vec<f64>]>| {
        let instances: Vec<(geometry::T, Vec<f64>)> = transforms.iter().enumerate().map(|(k, t)| (mesh(prototypes[k]), t.clone())).collect();
        flow_sop::trace_scene::register_instances(&SPHERES, &[], motion, 11, &instances).expect("register instances");
    };
    register(None);
    compare_runs(default_run(), "instanced, three instances of two prototypes + spheres", 11, &description(&|_| None), &[1.0]);
    register(Some(&ends));
    let moving = description(&|k| Some(ends[k].clone()));
    assert!(compare_runs(default_run(), "instanced, instance motion", 11, &moving, &[0.0, 0.25, 0.7, 1.0]).motion);
    let mut caps = ogpu_core::backend::capabilities(&renderer::renderer::gpu_device(&lease));
    caps.ray_tracing_motion = false;
    assert!(!compare_runs(Run { caps: Some(caps), cpu_time: Some(0.5), tolerance: 1e-5 }, "instanced, shutter midpoint without motion", 11, &moving, &[0.3]).motion);
    flow_sop::trace_scene::unregister(11);
}

pub const CASES: &[(&str, fn())] = &[("flat", flat), ("stranded", stranded), ("instanced", instanced)];
