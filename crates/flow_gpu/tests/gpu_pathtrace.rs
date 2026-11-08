//! Ports of lib/flow_gpu/test_film.ml, test_resolve.ml and test_helpers.ml.
//! film: in-place read-modify-write accumulation through `bound`/`into` plus an RGBA8 resolve by
//! the image sink, ties to even. resolve: `resolve_preview.rays` on the GPU matches the same
//! arithmetic written out on the host. helpers: `pathtrace_helpers.rays` kernels on the GPU match
//! the CPU within float32 tolerance; the CPU also matches independent host arithmetic.
mod common;
#[path = "common/pathtrace_reference.rs"]
mod reference_math;
#[path = "common/helpers_reference.rs"]
mod helpers_reference;
use common::test_program as tp;
use flow_gpu::{image_sink, pipelines, run};
use flow_ir::packed::{self, host as phost};
use flow_shader::emit;
use flow_syntax::value::Value;
use ogpu_core::backend as b;
use ogpu_core::types::{BufferDescriptor, BufferUsage};

const RESOLVE_PREVIEW: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../../crates/rays_pathtracer/resolve_preview.rays"));
const HELPERS: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../../crates/rays_pathtracer/pathtrace_helpers.rays"));

fn vec4(v: &Value) -> Vec<f64> {
    match v {
        Value::Vec4_array(a) => (**a).clone(),
        _ => panic!("vec4 array expected"),
    }
}

fn prepare(p: &packed::T, t: f64) -> phost::Inputs {
    tp::ok(phost::prepare(p, &phost::PrepareOptions { fast: true, ..Default::default() }, &input::at_time(t)))
}

fn film() {
    if !common::gpu() {
        return;
    }
    let lease = common::lease();
    let (width, height) = (64i64, 48i64);
    let n = width * height;
    let source = format!("(map (fn [x] [x 1 2 3]) (array/range {n}))");
    let accumulate = tp::compile_with(None, false, "", &format!("(map (fn [(p : vec4)] (+ p [(* t 0.25) 0.125 0.5 1])) {source})"));
    let resolve = tp::compile_with(None, false, "", &format!("(map (fn [(p : vec4)] [(+ (/ p.x p.w) (* 0. t)) (/ p.y p.w) (/ p.z p.w) 1.]) {source})"));
    let device = renderer::renderer::gpu_device(&lease);
    let buffer = |size: i64| b::create_buffer(&device, BufferDescriptor { label: Some("film".into()), size, usage: vec![BufferUsage::Storage] }, None).expect("buffer");
    let (film, cell) = (buffer(n * 16), buffer(24));
    b::write_buffer(&film, 0, &vec![0u8; (n * 16) as usize]).unwrap();
    let pipes = pipelines::create(&|| 0.0, &device);
    let sink = tp::ok(image_sink::create(&lease));
    let runner = |p: &packed::T| run::create(None, &lease, &pipes, &tp::ok(emit::kernel(&emit::Options { fast: true, ..Default::default() }, p)));
    let (accumulator, resolver) = (runner(&accumulate), runner(&resolve));
    let f2 = film.clone();
    let bound = move |i: i64| if i == 0 { Some(f2.clone()) } else { None };
    for frame in 1..=4 {
        tp::ok(run::dispatch(&accumulator, &run::Options { into: Some((film.clone(), cell.clone())), ..Default::default() }, Some(&bound), None, &prepare(&accumulate, frame as f64)));
    }
    let floats = b::read_buffer(&film, 0, n * 16).unwrap();
    let word = |k: usize| f32::from_le_bytes(floats[4 * k..4 * k + 4].try_into().unwrap());
    for pixel in 0..n as usize {
        assert!(word(4 * pixel) == 2.5 && word(4 * pixel + 1) == 0.5 && word(4 * pixel + 2) == 2.0 && word(4 * pixel + 3) == 4.0, "pixel {pixel}");
    }
    // resolve: 2.5/4 -> 159, 0.5/4 -> 32, 2/4 -> 128 (127.5 rounds to even), alpha 255
    let resolved = tp::ok(run::dispatch(&resolver, &run::Options::default(), Some(&bound), None, &prepare(&resolve, 1.0)));
    let image = tp::ok(image_sink::convert(&sink, width, height, &resolved));
    let texture = image_sink::texture(&image).expect("texture");
    let pixels = b::read_texture(&texture, width * 4).unwrap();
    for pixel in 0..n as usize {
        assert!(pixels[4 * pixel] == 159 && pixels[4 * pixel + 1] == 32 && pixels[4 * pixel + 2] == 128 && pixels[4 * pixel + 3] == 255, "pixel {pixel}");
    }
    println!("film: 4 in-place accumulation dispatches over {n} pixels and an RGBA8 resolve match exactly");
    run::close(&accumulator, false);
    run::close(&resolver, false);
    image_sink::close(&sink);
    pipelines::close(&pipes);
    let _ = b::destroy_buffer(&film);
    let _ = b::destroy_buffer(&cell);
}

/// pathtrace.metal's resolve_preview, written out on the host (the A side of the A/B)
fn reference(color: &[f64], geom: &[f64], w: usize, h: usize) -> Vec<f64> {
    let n = w * h;
    let aces = |c: f64| {
        let c = c * 1.3;
        (c * (2.51 * c + 0.03) / (c * (2.43 * c + 0.59) + 0.14)).max(0.0).min(1.0).powf(1.0 / 2.2)
    };
    (0..n * 4)
        .map(|k| {
            let (p, ch) = (k / 4, k % 4);
            let cw = geom[p * 4 + 3];
            let mut sum = [color[p * 4], color[p * 4 + 1], color[p * 4 + 2]];
            let mut weight = 1.0;
            if cw > 0.0 {
                for dy in -1i64..=1 {
                    for dx in -1i64..=1 {
                        let (x, y) = ((p % w) as i64 + dx, (p / w) as i64 + dy);
                        if (dx != 0 || dy != 0) && x >= 0 && y >= 0 && x < w as i64 && y < h as i64 {
                            let q = (y as usize) * w + x as usize;
                            let sw = geom[q * 4 + 3];
                            if sw > 0.0
                                && (sw - cw).abs() < 0.03f64.max(cw * 0.01)
                                && geom[q * 4] * geom[p * 4] + geom[q * 4 + 1] * geom[p * 4 + 1] + geom[q * 4 + 2] * geom[p * 4 + 2] > 0.95
                            {
                                let wt = if dx == 0 || dy == 0 { 0.5 } else { 0.25 };
                                for c in 0..3 {
                                    sum[c] += color[q * 4 + c] * wt;
                                }
                                weight += wt;
                            }
                        }
                    }
                }
            }
            if ch == 3 {
                if cw <= 0.0 { 0.0 } else { 1.0 }
            } else {
                aces(sum[ch] / weight)
            }
        })
        .collect()
}

fn rmse(a: &[f64], b: &[f64]) -> f64 {
    (a.iter().zip(b).map(|(x, y)| (x - y).powi(2)).sum::<f64>() / a.len() as f64).sqrt()
}

fn resolve() {
    if !common::gpu() {
        return;
    }
    let lease = common::lease();
    let (w, h) = (64usize, 48usize);
    let n = w * h;
    let color_src = format!("(map (fn [(i : int)] [(+ 0.5 (noise3 [(* (float i) 0.07) 1. (* 0. t)] :seed 1)) (+ 0.4 (noise3 [(* (float i) 0.05) 2. 0.] :seed 2)) (+ 0.3 (noise3 [(* (float i) 0.03) 3. 0.] :seed 3)) 1.]) (array/range {n} :type int))");
    let geom_src = format!("(map (fn [(i : int)] [0. 0. 1. (+ (* 0. t) (if (int/lt (int/mod i 11) 2) 0. (+ 1. (* 0.004 (float (int/mod i 7))))))]) (array/range {n} :type int))");
    let film = format!("(let* [color {color_src} geom {geom_src} order (map (fn [(i : int)] (int/add i (int (* 0. t)))) (array/range {n} :type int))]\n     (map (fn [(i : int)] (resolve-pixel color geom order i {w} {h} (+ 1.3 (* 0. t)) true)) (array/range {n} :type int)))");
    let program = tp::compile_with(None, true, RESOLVE_PREVIEW, &film);
    let live = input::at_time(1.0);
    let table = |src: &str| vec4(&tp::ok(packed::force(&tp::compile(src), &live, &Default::default())));
    let cpu = vec4(&tp::ok(packed::force(&program, &live, &Default::default())));
    let a = reference(&table(&color_src), &table(&geom_src), w, h);
    println!("resolve_preview.rays: CPU vs MSL-semantics RMSE {:.3e}", rmse(&cpu, &a));
    assert!(rmse(&cpu, &a) <= 1e-4);
    let pipes = pipelines::create(&|| 0.0, &renderer::renderer::gpu_device(&lease));
    let msl = tp::ok(emit::kernel(&emit::Options { fast: true, ..Default::default() }, &program));
    let runner = run::create(None, &lease, &pipes, &msl);
    let out = tp::ok(run::dispatch(&runner, &run::Options::default(), None, None, &prepare(&program, 1.0)));
    let gpu_out = vec4(&tp::ok(run::readback(&out, None)));
    assert_eq!(gpu_out.len(), cpu.len());
    let r = rmse(&gpu_out, &a);
    println!("resolve_preview.rays: {} px, GPU vs MSL-semantics RMSE {r:.3e}", w * h);
    assert!(r <= 1e-4);
    run::close(&runner, false);
    pipelines::close(&pipes);
}

const N: usize = 4096;

const PRELUDE: &str = "(let* [s0 (int/and (int/mul (int/add i 1) 2654435761) 4294967295)\n  a (pcg-next s0) b (pcg-next a.next) c (pcg-next b.next) d (pcg-next c.next)\n  r1 (unit-float a.value) r2 (unit-float b.value) r3 (unit-float c.value) r4 (unit-float d.value)\n  ns (normalize [(- (* r1 2.) 1.) (- (* r2 2.) 1.) (+ r3 0.1)])\n  v (normalize [r2 r3 (+ r1 0.2)]) l (normalize [r3 r1 (+ r2 0.2)])\n  a2 (+ 0.01 (* r4 0.5)) fr (fresnel [r1 r2 r3] r4)] ";
const MORE: &str = "  df [r1 r2 r3] f0 [(* r4 0.5) (* r3 0.5) (* r2 0.5)] bs (basis ns)\n  lo [(- (* r1 2.) 1.) (- (* r2 2.) 1.) 3.] lu [1. r3 0.] lv [0. 1. r4] ray (normalize [(- r1 0.5) (- r2 0.5) 1.])\n  rh (rect-hit lo lu lv (if (> r4 0.5) 1. 0.) [0. 0. 0.] ray 10.) wuv (world-uv ns) wd (world-dir r1 r2) br (eval-brdf ns v l df f0 a2)] ";

fn kernel(body: &str) -> String {
    format!("(map (fn [(i : int)] {PRELUDE} {body}) (array/range {N} :type int))")
}

fn kernel3(body: &str) -> String {
    format!("(map (fn [(i : int)] {}{MORE} {body})) (array/range {N} :type int))", &PRELUDE[..PRELUDE.len() - 2])
}

fn helpers() {
    let live = input::at_time(1.0);
    let sources = vec![
        kernel("[(float (int/and a.value 16777215)) (d-ggx r1 a2) (v-smith r2 r3 a2) (+ r4 (* 0. t))])"),
        kernel("[fr.x fr.y fr.z (+ (bsdf-pdf ns v l a2 r4) (* 0. t))])"),
        kernel3("[br.x br.y br.z (+ (power-weight r1 r2) (* 0. t))]"),
        kernel3("[bs.tan.x bs.tan.y bs.tan.z (+ bs.b.x (* 0. t))]"),
        kernel3("[bs.b.y bs.b.z (+ rh.t (* 0. t)) rh.cos]"),
        kernel3("[wuv.u wuv.v wd.x (+ wd.z (* 0. t))]"),
    ];
    let programs: Vec<_> = sources.iter().map(|src| tp::compile_with(None, true, HELPERS, src)).collect();
    let cpu: Vec<_> = programs.iter().map(|p| vec4(&tp::ok(packed::force(p, &live, &Default::default())))).collect();
    helpers_reference::check(&cpu);
    if !common::gpu() { return; }
    let lease = common::lease();
    let pipes = pipelines::create(&|| 0.0, &renderer::renderer::gpu_device(&lease));
    for (k, program) in programs.iter().enumerate() {
        let expected = &cpu[k];
        let runner = run::create(None, &lease, &pipes, &tp::ok(emit::kernel(&emit::Options { fast: true, ..Default::default() }, &program)));
        let gpu = vec4(&tp::ok(run::readback(&tp::ok(run::dispatch(&runner, &run::Options::default(), None, None, &prepare(&program, 1.0))), None)));
        let worst = gpu.iter().zip(expected).fold(0.0f64, |m, (v, e)| m.max((v - e).abs() / e.abs().max(1.0)));
        println!("pathtrace_helpers.rays kernel {k}: {N} rows, GPU vs CPU max relative error {worst:.3e}");
        assert!(worst <= 1e-3, "kernel {k}: {worst}");
        run::close(&runner, false);
    }
    pipelines::close(&pipes);
}

fn main() {
    common::window::run(&[("film", film), ("resolve", resolve), ("helpers", helpers)]);
}
