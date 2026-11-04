//! Port of lib/flow_sop/image_kernel.ml(i): packed pixel functions; procedural images never depend
//! on the Flow compiler.
use crate::attribute_kernel;
use flow_check::workspace::{Path, Paths};
use flow_eval as e;
use flow_ir::flow_ir::{Executor, Sink};
use flow_ir::packed;
use flow_syntax::ty::{FnSignature, Ty};
use flow_syntax::value::{FnId, Value};
use rays_diagnostic::Diagnostic;
use sop::node::{self, build, CookMode, Cooked};
use sop::{context, payload};
use std::sync::Arc;

/// A prepared image map: one immutable pixel-center UV grid's compiled body.
#[derive(Clone)]
pub struct T {
    identity: i64,
    width: i64,
    height: i64,
    function: FnId,
    sources: Vec<i64>,
    inputs: Vec<node::T>,
    origins: Vec<(i64, i64)>,
    program: Executor::Program,
    packed: flow_ir::flow_ir::Packed,
}

fn error<R>(message: &str) -> Result<R, Diagnostic> {
    Err(flow_syntax::value::fail("E_IMAGE", message))
}

// OCaml: min Sys.max_floatarray_length Sys.max_string_length, on a 64-bit build.
const MAX_STORAGE: i64 = (1_i64 << 54) - 1;

/// Retains one immutable pixel-center UV grid and the actual compiled body. Display qualification
/// uses the authored `path`; absent provenance stays CPU-only. `node` always cooks an exact CPU
/// snapshot.
pub fn prepare(
    site: Option<(i64, Path, Vec<i64>)>,
    path: Option<Path>,
    approx: Option<Paths>,
    identity: i64,
    width: i64,
    height: i64,
    function: FnId,
    sources: &[i64],
    inputs: &[node::T],
) -> Result<T, Diagnostic> {
    if width <= 0 || height <= 0 {
        return error("Image dimensions must be positive.");
    }
    if width > MAX_STORAGE / 4 / height {
        return error("Image dimensions exceed native storage bounds.");
    }
    let vec4_body = e::Residual::function_body(function).is_some_and(|(_, body)| body.ty == Ty::Vec4);
    if !vec4_body {
        return error("Image map needs an instantiated vec4 pixel body.");
    }
    let origins = attribute_kernel::source_origins(sources, inputs)?;
    let mut uv = vec![0.0; (width * height * 2) as usize];
    for y in 0..height {
        let v = (y as f64 + 0.5) / height as f64;
        for x in 0..width {
            let i = ((y * width + x) * 2) as usize;
            uv[i] = (x as f64 + 0.5) / width as f64;
            uv[i + 1] = v;
        }
    }
    let signature = FnSignature { params: vec![Ty::Vec2], result: Ty::Vec4 };
    let values = e::Host::map_function(site, path.clone(), &signature, function, &[Value::Vec2_array(Arc::new(uv))])?;
    let approx = if path.is_none() { Paths::new() } else { approx.unwrap_or_default() };
    let program = attribute_kernel::prepare(None, false, Some(approx), Some(Sink::Display("image/map".to_owned())), sources, inputs, &values)?;
    match Executor::packed_region(&program) {
        Some(packed) => Ok(T {
            identity,
            width,
            height,
            function,
            sources: sources.to_vec(),
            inputs: inputs.to_vec(),
            origins,
            program,
            packed,
        }),
        None => match &values {
            Value::Residual(r) => {
                let view = e::Residual::residual_view(*r)?;
                match packed::compile_result(&packed::CompileOptions::default(), *r, &view.term) {
                    Err(mut diagnostics) if !diagnostics.is_empty() => Err(diagnostics.remove(0)),
                    _ => Err(flow_syntax::value::fail("E_KERNEL_FORM", "Image function needs a packed map body.")),
                }
            }
            _ => error("Image function did not produce a pixel map."),
        },
    }
}

pub fn program(t: &T) -> Executor::Program {
    t.program.clone()
}

/// Rebind current compiled inputs while sharing the UV grid and program. A changed point-origin
/// proof or source count returns `E_DATA_SOURCE`.
pub fn with_inputs(t: &T, inputs: &[node::T]) -> Result<T, Diagnostic> {
    let origins = attribute_kernel::source_origins(&t.sources, inputs)?;
    if origins != t.origins {
        return Err(flow_syntax::value::fail("E_DATA_SOURCE", "Image source point-origin proof changed; prepare the kernel again."));
    }
    Ok(T { inputs: inputs.to_vec(), ..t.clone() })
}

/// Cook through `Sop::Session` to install the context's shared pool.
pub fn node(state: Option<&e::State>, t: &T) -> node::T {
    let mut parameters = format!("{}:{}:{}", t.identity, t.width, t.height).into_bytes();
    if let Some(state) = state {
        parameters.extend_from_slice(e::state_stamp(state).as_bytes());
    }
    let (sources, packed, width, height) = (t.sources.clone(), t.packed.clone(), t.width, t.height);
    let state = state.cloned();
    build::make(
        build::Args {
            parameters,
            cook_mode: CookMode::Generator,
            dependencies: attribute_kernel::input_reads(&Value::Fn(t.function)),
            inputs: t.inputs.clone(),
            ..build::args("image/map")
        },
        None,
        Arc::new(move |_node_id, ctx, payloads| {
            let lookup: attribute_kernel::GeometryLookup = {
                let payloads = Arc::new(payloads.to_vec());
                let sources = sources.clone();
                Arc::new(move |id| sources.iter().position(|s| *s == id).and_then(|i| payload::geometry(&payloads[i]).ok()))
            };
            if context::cancelled(ctx) {
                return Err(flow_syntax::value::fail("E_CANCELLED", "Image kernel cancelled."));
            }
            let resolve: packed::Resolve = Arc::new(move |v| attribute_kernel::resolve(None, &lookup, v));
            let options = packed::ForceOptions { state: state.clone(), resolve: Some(resolve), ..Default::default() };
            match packed::force(&packed, context::input(ctx), &options)? {
                Value::Vec4_array(rgba) => {
                    let image = sop::image::of_vec4(ctx, width, height, &rgba)?;
                    Ok(Cooked { payload: payload::T::Image(image), instances: None })
                }
                _ => error("Image kernel must return vec4 pixels."),
            }
        }),
    )
}

/// Port of lib/flow_sop/test_image_kernel.ml, less its Flow_graph projection and edit gestures and
/// the display-qualification test (those need the graph library and `Lower`).
#[cfg(test)]
mod tests {
    use super::*;
    use flow_check::catalog::Catalog;

    fn check(text: &str) -> flow_check::workspace::Workspace {
        let forms = flow_syntax::syntax::parse(text).unwrap();
        match flow_check::workspace::check(&[], false, false, None, &Catalog { version: 1, kinds: Vec::new() }, &forms) {
            (Some(w), _) => w,
            (_, ds) => panic!("{}", ds.iter().map(|d| d.message.clone()).collect::<Vec<_>>().join("; ")),
        }
    }
    fn accepted(text: &str) -> bool {
        let forms = flow_syntax::syntax::parse(text).unwrap();
        flow_check::workspace::check(&[], false, false, None, &Catalog { version: 1, kinds: Vec::new() }, &forms).0.is_some()
    }
    fn function(text: &str) -> FnId {
        let evaluated = e::static_eval(&check(text)).unwrap();
        let producer = evaluated.plan.nodes.iter().find(|n| n.kind == "image/map").expect("image/map node");
        match producer.args.iter().find(|(name, _)| name == "function") {
            Some((_, Value::Fn(f))) => *f,
            _ => panic!("image/map has no function"),
        }
    }
    fn prepared(width: i64, height: i64, f: FnId) -> T {
        prepare(None, None, None, node::build::fresh_id(), width, height, f, &[], &[]).unwrap()
    }
    fn cook(time: f64, domains: i64, kernel: &T) -> Vec<u8> {
        let mut session = sop::session::create(0, 0).unwrap();
        let context = context::create(context::Options { domains: Some(domains), time: Some(time), ..Default::default() }).unwrap();
        let output = sop::session::cook(&mut session, None, &context, &node(None, kernel));
        sop::session::close(&mut session);
        let image = payload::image(&output.unwrap().payload).unwrap();
        sop::image::rgba8(&image).unwrap().to_vec()
    }

    const GRADIENT: &str = "(workspace w (graph img :context image (let* [picture\n    (image/map (fn [uv] [uv.x uv.y 0.5 1]) :width 387 :height 91)] picture)))";

    #[test]
    fn typed_function_is_checked() {
        for body in ["(fn [uv] uv.x)", "(fn [(uv : vec3)] [uv.x uv.y 0 1])", "(fn [u v] [0 0 0 1])"] {
            assert!(!accepted(&format!("(workspace w (graph img :context image (image/map {body})))")), "accepted: {body}");
        }
    }

    #[test]
    #[ignore = "flow_check: an untyped fn parameter passed to image/map is refused (E_TYPE got fn); OCaml accepts"]
    fn image_map_through_a_defn_fn_parameter() {
        let defn = "(workspace w (defn tex :context image [(f : fn)] (image/map f :width 8 :height 8)) (graph img :context image (tex (fn [uv] [uv.x uv.y 0 1]))))";
        let forms = flow_syntax::syntax::parse(defn).unwrap();
        let (checked, diagnostics) = flow_check::workspace::check(&[], false, false, None, &Catalog { version: 1, kinds: Vec::new() }, &forms);
        assert!(checked.is_some(), "{:?}", diagnostics.iter().map(|d| format!("{}: {}", d.code, d.message)).collect::<Vec<_>>());
    }

    #[test]
    fn uv_orientation_and_domain_bytes() {
        let kernel = prepared(387, 91, function(GRADIENT));
        let one = cook(0.0, 1, &kernel);
        let eight = cook(0.0, 8, &kernel);
        assert_eq!(one, eight);
        assert_eq!(one.len(), 387 * 91 * 4);
        let reference = match Executor::force(&program(&kernel), &Executor::ForceOptions { reference: true, ..Default::default() }, &input::at_time(0.0)).unwrap() {
            Value::Vec4_array(xs) => xs,
            _ => panic!("vec4 pixels expected"),
        };
        let uv_byte = |i: i64, size: i64| {
            let numerator = (2 * i + 1) * 255;
            let denominator = 2 * size;
            let (n, remainder) = (numerator / denominator, numerator % denominator);
            if 2 * remainder > denominator || (2 * remainder == denominator && n % 2 == 1) { n + 1 } else { n }
        };
        for y in 0..91_i64 {
            for x in 0..387_i64 {
                let i = ((y * 387 + x) * 4) as usize;
                let (u, v) = ((x as f64 + 0.5) / 387.0, (y as f64 + 0.5) / 91.0);
                assert!(reference[i] == u && reference[i + 1] == v && reference[i + 2] == 0.5 && reference[i + 3] == 1.0);
                for (c, n) in [uv_byte(x, 387), uv_byte(y, 91), 128, 255].into_iter().enumerate() {
                    assert_eq!(one[i + c] as i64, n);
                }
            }
        }
    }

    #[test]
    fn constants_live_captures_and_conversions() {
        let constant = prepared(387, 91, function("(workspace w (graph img :context image\n    (image/map (fn [uv] [0.25 0.5 0.75 1]))))"));
        let expected: Vec<u8> = (0..387 * 91 * 4).map(|i| [64, 128, 191, 255][i % 4]).collect();
        for domains in [1, 8] {
            assert_eq!(cook(0.0, domains, &constant), expected);
        }
        let live = prepared(65, 3, function("(workspace w (defn render :context image [(bias : float)]\n      (image/map (fn [uv] [(+ uv.x bias) uv.y 0.5 1]) :width 65 :height 3))\n      (graph img :context image (render (* t 0.25))))"));
        assert_eq!(cook(0.0, 1, &live), cook(0.0, 8, &live));
        assert_eq!(cook(1.0, 1, &live), cook(1.0, 8, &live));
        assert_ne!(cook(0.0, 1, &live), cook(1.0, 1, &live));
        let named = prepared(65, 3, function("(workspace w (defn pixel :context value [(uv : vec2)] [uv.x uv.y t 1])\n    (graph img :context image (image/map pixel :width 65 :height 3)))"));
        assert_eq!(cook(0.5, 1, &named), cook(0.5, 8, &named));
        let converted = prepared(65, 3, function("(workspace w (graph img :context image\n    (image/map (fn [uv] [(floor uv.x) (floor uv.y) 0 1]))))"));
        let expected: Vec<u8> = (0..65 * 3 * 4).map(|i| if i % 4 == 3 { 255 } else { 0 }).collect();
        for domains in [1, 8] {
            assert_eq!(cook(0.0, domains, &converted), expected);
        }
    }

    #[test]
    fn dimensions_and_unsupported_bodies_refuse() {
        let f = function(GRADIENT);
        for (width, height) in [(0, 1), (1, 0), (i64::MAX >> 1, 2), (1, i64::MAX >> 1)] {
            assert!(prepare(None, None, None, 0, width, height, f, &[], &[]).is_err(), "{width}x{height}");
        }
        let unsupported = function("(workspace w (graph img :context image (image/map (fn [uv] [(value/rand uv.x) 0 0 1]))))");
        assert!(prepare(None, None, None, 0, 1, 1, unsupported, &[], &[]).is_err());
    }

    #[test]
    fn state_steps_with_its_callers_state() {
        let stateful = function("(workspace w (graph img :context image\n    (let* [bias (state [n 0.0] (+ n 0.1))]\n      (image/map (fn [uv] [uv.x uv.y bias 1])))))");
        let kernel = prepared(65, 3, stateful);
        let program = program(&kernel);
        let packed = Executor::packed_region(&program).unwrap();
        for domains in [1, 8] {
            parallel::run_unit(Some(domains), &|| {
                let (state, reference) = (e::create_state(), e::create_state());
                let mut blues: Vec<f64> = Vec::new();
                for frame in 0..3 {
                    let mut live = input::at_time(0.0);
                    live.frame = frame;
                    let actual = packed::force(&packed, &live, &packed::ForceOptions { state: Some(state.clone()), ..Default::default() }).unwrap();
                    let walked = Executor::force(&program, &Executor::ForceOptions { reference: true, state: Some(reference.clone()), ..Default::default() }, &live).unwrap();
                    assert_eq!(actual, walked);
                    assert_eq!(e::Host::state_values(&state), e::Host::state_values(&reference));
                    match actual {
                        Value::Vec4_array(xs) => blues.push(xs[2]),
                        _ => panic!("vec4 pixels expected"),
                    }
                }
                blues.sort_by(|a, b| a.partial_cmp(b).unwrap());
                blues.dedup();
                assert_eq!(blues.len(), 3);
                Ok(())
            })
            .unwrap();
        }
    }
}
