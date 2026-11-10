//! Each path-tracer scene shape compiles Lisp kernels once. Static facts fold; dispatch uniforms and retained device buffers supply dynamic values. Placeholders have distinct scalar/Arc identities and CPU reference bindings own a separate declaration copy. The disk cache serializes fields under executable/text/scene content identity.
use flow_syntax::symbol::Symbol;
use flow_check::catalog::Catalog;
use flow_check::op::{self, Op, Shape, Signature};
use flow_check::workspace as w;
use flow_eval as e;
use flow_gpu::{pipelines, run};
use flow_ir::packed::{self, host as phost};
use flow_shader::emit::{self, Curves};
use flow_syntax::ty::{FnSignature, Ty};
use flow_syntax::value::{FnId, Value, source_value};
use flow_syntax::{context, syntax};
use ogpu_core::backend::{Accel, Buffer, Commands};
use rays_diagnostic::Diagnostic;
use std::cell::RefCell;
use std::rc::Rc;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

pub const SOURCE: &str = concat!(
    include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../../crates/rays_pathtracer/pathtrace_helpers.rays")),
    include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../../crates/rays_pathtracer/pathtrace.rays")),
    include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/../../crates/rays_pathtracer/resolve_preview.rays")),
);

pub use crate::lisp_kernel_cache::CACHE_DIR;

pub fn uniforms() -> Vec<(&'static str, Ty)> {
    vec![
        ("eye", Ty::Vec3),
        ("fwd", Ty::Vec3),
        ("rgt", Ty::Vec3),
        ("up", Ty::Vec3),
        ("tanhalf", Ty::Float),
        ("aspect", Ty::Float),
        ("lens", Ty::Float),
        ("focus", Ty::Float),
        ("frame", Ty::Int),
        ("lo", Ty::Int),
        ("hi", Ty::Int),
        ("peye", Ty::Vec3),
        ("pfwd", Ty::Vec3),
        ("prgt", Ty::Vec3),
        ("pup", Ty::Vec3),
        ("ptan", Ty::Float),
        ("paspect", Ty::Float),
        ("pvalid", Ty::Int),
        ("pacc", Ty::Int),
        ("sky", Ty::Vec3),
        ("ground", Ty::Vec3),
        ("bg", Ty::Vec3),
        ("sundir", Ty::Vec3),
        ("suncos", Ty::Float),
        ("sunrad", Ty::Vec3),
        ("sunpdf", Ty::Float),
        ("exposure", Ty::Float),
        ("count", Ty::Float),
        ("spherebase", Ty::Int),
        ("strandbase", Ty::Int),
        ("width", Ty::Int),
        ("height", Ty::Int),
    ]
}

fn array_of(ty: Ty) -> Ty {
    Ty::Array(Box::new(ty))
}

pub fn tables() -> Vec<(&'static str, Ty)> {
    vec![
        ("lights", array_of(Ty::Vec4)),
        ("pan", array_of(Ty::Vec4)),
        ("wmap", array_of(Ty::Vec3)),
        ("cdf", array_of(Ty::Float)),
        ("mats", array_of(Ty::Vec4)),
        ("ids", array_of(Ty::Int)),
        ("inst", array_of(Ty::Vec3)),
        ("nrm", array_of(Ty::Vec3)),
        ("cols", array_of(Ty::Vec3)),
        ("accum", array_of(Ty::Vec4)),
        ("geo", array_of(Ty::Vec4)),
        ("pgeo", array_of(Ty::Vec4)),
        ("pcol", array_of(Ty::Vec4)),
        ("order", array_of(Ty::Int)),
    ]
}

fn placeholder_op(name: &str, ty: Ty) -> Op {
    let mut o = op::find(&[], "noise3", &context::value()).expect("noise3").clone();
    let full = format!("pt/{name}");
    o.name = full.clone();
    o.packed_extension = None;
    o.signature = Signature { pos: vec![], opt: vec![], rest: None, kw: vec![] };
    let out = ty.clone();
    o.out = Arc::new(move |_| Ok(out.clone()));
    o.shape = Shape::Struct { splice: false };
    o.check = Arc::new(|_| Ok(()));
    o.body = Arc::new(move |_, _, _| Ok(source_value(full.clone(), ty.clone(), vec![].into())));
    o
}

pub fn ops() -> Vec<Op> {
    uniforms().into_iter().chain(tables()).map(|(name, ty)| placeholder_op(name, ty)).collect()
}

static SERIAL: AtomicI64 = AtomicI64::new(0);

/// Fresh blocks: a placeholder is found again in a template's uniforms by `same_placeholder`.
pub fn placeholder(ty: &Ty) -> Value {
    let k = SERIAL.fetch_add(1, Ordering::Relaxed) + 1;
    let s = 1_000_000.0 * k as f64 + 0.5;
    match ty {
        Ty::Vec3 => Value::Vec3(s, s + 0.25, s + 0.75),
        Ty::Float => Value::Float(s),
        Ty::Int => Value::Int(1_000_000 * k + 7),
        Ty::Array(inner) if **inner == Ty::Vec4 => Value::Floats(4, Arc::new(vec![0.; 4])),
        Ty::Array(inner) if **inner == Ty::Vec3 => Value::Floats(3, Arc::new(vec![0.; 3])),
        Ty::Array(inner) if **inner == Ty::Int => Value::IntArray(Arc::new(vec![0])),
        _ => Value::Floats(1, Arc::new(vec![0.])),
    }
}

fn same_placeholder(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Floats(1, x), Value::Floats(1, y)) | (Value::Floats(3, x), Value::Floats(3, y)) | (Value::Floats(4, x), Value::Floats(4, y)) => Arc::ptr_eq(x, y),
        (Value::IntArray(x), Value::IntArray(y)) => Arc::ptr_eq(x, y),
        _ => a == b,
    }
}

pub fn width(ty: &Ty) -> i64 {
    match ty {
        Ty::Array(inner) if **inner == Ty::Vec4 => 4,
        Ty::Array(inner) if **inner == Ty::Vec3 => 3,
        _ => 1,
    }
}

/// The scene facts a kernel set is specialized on; equal facts share one compiled set.
#[derive(Clone, Debug, PartialEq)]
pub struct Facts {
    pub spp: i64,
    pub bounces: i64,
    pub round_samples: i64,
    pub panels: i64,
    pub lights: i64,
    pub world: bool,
    pub background: i64,
    pub map_w: i64,
    pub map_h: i64,
    pub sun: bool,
    pub bsdf_only: bool,
    pub instanced: bool,
    pub colored: bool,
    /// a device mesh: one material, whatever its triangles
    pub lone: bool,
    pub classes: (bool, bool, bool),
    pub motion: bool,
    pub curves: Option<Curves>,
    pub spheres: bool,
}

/// The kernels, in two sets checked apart: an accumulation frame's, and a preview's (when the camera moves).
pub const PREVIEWS: [&str; 3] = ["preview", "preview-geometry", "preview-resolve"];

pub fn text(f: &Facts, preview_set: bool) -> String {
    let (metallic, emissive, round) = f.classes;
    let u = |preview: bool| {
        format!(
            "{{:eye eye :lens {} :fwd fwd :rgt rgt :up up :tan-half tanhalf :aspect aspect :focus focus :width width :height height :spp {} :bounces {} :frame frame :sky sky :ground ground :panels {} :lights {} :world {} :background {} :map-w {} :map-h {} :bg bg :sun-dir sundir :sun-cos suncos :sun-rad sunrad :sun-pdf {} :round-samples {} :preview {} :bsdf-only {} :instanced {} :colored {} :lone {} :sphere-base spherebase :metallic {} :emissive {} :round {} :motion {} :inst-rows {} :strand-base strandbase}}",
            if preview { "0." } else { "lens" },
            if preview { 1 } else { f.spp },
            if preview { 1 } else { f.bounces },
            f.panels,
            f.lights,
            f.world as i64,
            f.background,
            f.map_w,
            f.map_h,
            if f.sun { "sunpdf" } else { "0." },
            if preview { 1 } else { f.round_samples },
            preview,
            f.bsdf_only,
            f.instanced,
            f.colored,
            f.lone,
            metallic,
            emissive,
            round,
            f.motion,
            if f.motion { 7 } else { 4 },
        )
    };
    let args = "lights mats ids inst nrm cols pan wmap cdf";
    let clear = f.world && f.background == 2;
    // the records are built inside each kernel, whose captures are then the placeholders themselves
    let kernel = |params: &str, name: &str, body: String| {
        format!(
            "{name} (fn [{params}] (let* [u {} v {} pc {{:eye peye :fwd pfwd :rgt prgt :up pup :tan-half ptan :aspect paspect :valid pvalid :from-accum pacc}}] {body}))",
            u(false),
            u(true)
        )
    };
    let default = "(pixel : int)";
    let kernels: Vec<String> = if preview_set {
        vec![
            kernel(default, "preview", format!("(preview-pixel v pc {clear} {args} geo pgeo pcol order pixel)")),
            kernel(default, "preview-geometry", "(primary-geo v pixel)".to_string()),
            kernel(default, "preview-resolve", format!("(resolve-pixel pcol geo order pixel width height exposure {clear})")),
        ]
    } else {
        vec![
            kernel("(pixel : int) (slot : int)", "trace", format!("(accumulate-pixel u lo hi {clear} {args} accum slot pixel)")),
            kernel(default, "geometry", "(primary-geo u pixel)".to_string()),
            kernel(default, "resolve", format!("(resolve-accum (array/nth accum (array/nth order pixel)) exposure {clear} count)")),
        ]
    };
    let declarations: Vec<String> = uniforms().into_iter().chain(tables()).map(|(name, _)| format!("{name} (pt/{name})")).collect();
    format!("(workspace pt {SOURCE} (graph g :context value (let* [{} {}] 0.0)))", declarations.join(" "), kernels.join(" "))
}

pub struct Kernel {
    pub runner: run::T,
    pub msl: emit::Kernel,
    pub base: phost::Inputs,
    /// the placeholder each uniform reads
    pub slots: Vec<Option<(String, Ty)>>,
}

pub struct T {
    pub facts: Facts,
    fns: RefCell<Vec<(String, FnId)>>,
    /// the evaluations whose functions `fns` holds
    evals: RefCell<Vec<e::Eval>>,
    values: Vec<(String, (Ty, Value))>,
    pixels: Value,
    kernels: RefCell<Vec<(String, Rc<Kernel>)>>,
    /// the checked workspace of the CPU oracle's set
    oracle: RefCell<Option<w::Workspace>>,
}

fn diagnostics(ds: &[Diagnostic]) -> String {
    ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")
}

fn catalog() -> Catalog {
    Catalog { version: 1, kinds: vec![] }
}

/// One set's kernel functions. The declarations are checked whole once, by a test (`check_library`); a set
/// types them only at its kernels' call sites, which is all it reaches.
/// `checked` keeps the set's checked workspace for another evaluation of the same facts (the CPU oracle
/// evaluates a fresh function per frame).
fn functions(f: &Facts, preview_set: bool, checked: &mut Option<w::Workspace>) -> Result<(Vec<(String, FnId)>, e::Eval), String> {
    let started = std::time::Instant::now();
    let workspace = match checked.clone() {
        Some(ws) => ws,
        None => {
            let forms = syntax::parse(&text(f, preview_set)).map_err(|d| rays_diagnostic::to_string(&d))?;
            match w::check(&ops(), false, true, None, &catalog(), &forms) {
                (None, ds) => return Err(diagnostics(&ds)),
                (Some(_), ds) if !ds.is_empty() => return Err(diagnostics(&ds)),
                (Some(ws), _) => {
                    *checked = Some(ws.clone());
                    ws
                }
            }
        }
    };
    let parsed = started.elapsed();
    let check_time = started.elapsed();
    let evaluated = e::r#static(&workspace, true, None).map_err(|d| format!("path tracer kernels: {}", rays_diagnostic::to_string(&d)))?;
    if std::env::var_os("RAYS_PT_TIME").is_some() {
        eprintln!("pt functions: parse {:.2}s check {:.2}s static {:.2}s", parsed.as_secs_f64(), (check_time - parsed).as_secs_f64(), (started.elapsed() - check_time).as_secs_f64());
    }
    let names: &[&str] = if preview_set { &PREVIEWS } else { &["trace", "geometry", "resolve"] };
    let mut found = Vec::new();
    for name in names {
        let path = vec!["g".to_string(), name.to_string()];
        let id = match evaluated.records.iter().find(|(p, _)| *p == path) {
            Some((_, list)) => match list.first() {
                Some((_, Value::Fn(id))) => *id,
                _ => return Err(format!("path tracer kernel {name} is not a function")),
            },
            None => return Err(format!("path tracer kernel {name} is not a function")),
        };
        found.push((name.to_string(), id));
    }
    Ok((found, evaluated))
}

/// A set's functions on the first use of one of its kernels.
pub fn compile(f: &Facts) -> Result<Rc<T>, String> {
    Ok(Rc::new(T {
        facts: f.clone(),
        fns: RefCell::new(vec![]),
        evals: RefCell::new(vec![]),
        values: uniforms().into_iter().chain(tables()).map(|(name, ty)| (name.to_string(), (ty.clone(), placeholder(&ty)))).collect(),
        pixels: Value::IntArray(Arc::new(vec![0])),
        kernels: RefCell::new(vec![]),
        oracle: RefCell::new(None),
    }))
}

/// What one front-end run owns: the front end recurses deeply (check, evaluation, template compile), so
/// it runs on its own large-stack thread with copies of the set's state and hands the functions it
/// evaluated back.
struct FrontJob {
    facts: Facts,
    values: Vec<(String, (Ty, Value))>,
    pixels: Value,
    fns: Vec<(String, FnId)>,
    evals: Vec<e::Eval>,
    /// the selected OGPU implementation is the main thread's
    vulkan: bool,
}

/// The front end's stack (the main thread keeps the SDL and GPU calls).
const BIG_STACK: usize = 256 << 20;

fn fn_in(job: &mut FrontJob, name: &str) -> Result<FnId, String> {
    if !job.fns.iter().any(|(n, _)| n == name) {
        let (fns, evaluated) = functions(&job.facts, PREVIEWS.contains(&name), &mut None)?;
        job.fns.extend(fns);
        job.evals.push(evaluated);
    }
    job.fns.iter().find(|(n, _)| n == name).map(|(_, f)| *f).ok_or_else(|| format!("no path tracer kernel {name}"))
}

/// Checks pathtrace.rays and its helpers whole; the tracer types them only where its kernels call them.
pub fn check_library() -> Result<(), String> {
    match std::thread::Builder::new().stack_size(BIG_STACK).spawn(check_library_here).expect("front-end thread").join() {
        Ok(r) => r,
        Err(panic) => std::panic::resume_unwind(panic),
    }
}

fn check_library_here() -> Result<(), String> {
    let forms = syntax::parse(&format!("(workspace pt {SOURCE})")).map_err(|d| rays_diagnostic::to_string(&d))?;
    match w::check(&[], true, false, None, &catalog(), &forms) {
        (Some(_), ds) if ds.is_empty() => Ok(()),
        (_, ds) => Err(diagnostics(&ds)),
    }
}

/// The check of pathtracer.rays's accumulation set, alone: tools/bench_check times it.
pub fn check_sketch() -> Result<(), String> {
    match std::thread::Builder::new().stack_size(BIG_STACK).spawn(check_sketch_here).expect("front-end thread").join() {
        Ok(r) => r,
        Err(panic) => std::panic::resume_unwind(panic),
    }
}

fn check_sketch_here() -> Result<(), String> {
    let f = Facts {
        spp: 1,
        bounces: 5,
        round_samples: 4,
        panels: 0,
        lights: 2,
        world: true,
        background: 0,
        map_w: 512,
        map_h: 256,
        sun: false,
        bsdf_only: false,
        instanced: true,
        colored: true,
        lone: false,
        classes: (false, false, true),
        motion: false,
        curves: None,
        spheres: false,
    };
    let forms = syntax::parse(&text(&f, false)).map_err(|d| rays_diagnostic::to_string(&d))?;
    match w::check(&ops(), false, true, None, &catalog(), &forms) {
        (Some(_), ds) if ds.is_empty() => Ok(()),
        (_, ds) => Err(diagnostics(&ds)),
    }
}

/// The front end's result for one kernel: its MSL, its inputs, and the placeholder each uniform reads.
pub type FrontResult = (emit::Kernel, phost::Inputs, Vec<Option<(String, Ty)>>);

fn replace_sources(values: Vec<(String, (Ty, Value))>) -> Box<dyn Fn(&Value) -> Value> {
    Box::new(move |value| match value {
        Value::Source(p) if p.name.starts_with("pt/") && p.fields.is_empty() => values.iter().find(|(k, _)| *k == p.name[3..]).map(|(_, (_, p))| p.clone()).expect("pt placeholder"),
        value => value.clone(),
    })
}

fn deferred(name: &str, bound: FnId, inputs: &[Value], result: Ty) -> Result<flow_syntax::value::ResidualId, String> {
    let signature = FnSignature { params: inputs.iter().map(|_| Ty::Int).collect(), result };
    match e::host::map_function(None, None, &signature, bound, inputs) {
        Ok(Value::Residual(r)) => Ok(r),
        Ok(_) => Err("path tracer kernel did not defer".to_string()),
        Err(d) => Err(format!("{name}: {}", rays_diagnostic::to_string(&d))),
    }
}

fn front(job: &mut FrontJob, name: &str) -> Result<FrontResult, String> {
    let started = std::time::Instant::now();
    let f = fn_in(job, name)?;
    let check_done = started.elapsed();
    let bound = e::bind_captures(f, &*replace_sources(job.values.clone()));
    let inputs = if name == "trace" { vec![job.pixels.clone(), job.pixels.clone()] } else { vec![job.pixels.clone()] };
    let residual = deferred(name, bound, &inputs, Ty::Vec4)?;
    let term = e::residual::residual_view(residual).map_err(|d| rays_diagnostic::to_string(&d))?.term;
    let template = packed::compile_template(&packed::CompileOptions::default(), residual, &term).ok_or_else(|| format!("path tracer kernel {name} has no device template"))?;
    let options = emit::Options {
        spirv: job.vulkan,
        fast: true,
        instanced: job.facts.instanced,
        motion: job.facts.motion,
        curves: job.facts.curves,
        spheres: job.facts.spheres,
        ..Default::default()
    };
    let msl = emit::kernel(&options, &template).map_err(|d| format!("{name}: {}", rays_diagnostic::to_string(&d)))?;
    let base = phost::prepare(&template, &phost::PrepareOptions { fast: true, ..Default::default() }, &input::at_time(0.)).map_err(|d| format!("{name}: {}", rays_diagnostic::to_string(&d)))?;
    let slots = phost::uniform_shapes(&template)
        .iter()
        .map(|shape| match shape {
            phost::Shape::Value(v) => job.values.iter().find(|(_, (_, p))| same_placeholder(p, v)).map(|(n, (ty, _))| (n.clone(), ty.clone())),
            _ => None,
        })
        .collect();
    if std::env::var_os("RAYS_PT_TIME").is_some() {
        eprintln!("pt front {name}: check+static {:.2}s total {:.2}s", check_done.as_secs_f64(), started.elapsed().as_secs_f64());
    }
    Ok((msl, base, slots))
}

thread_local! {
    /// A newest-first front-end cache skips checking, evaluation and emission for later tracers with the same scene shape.
    static FRONTS: RefCell<Vec<(String, FrontResult)>> = const { RefCell::new(Vec::new()) };
}

const FRONT_ENTRIES: usize = 24;

fn front_big(t: &T, name: &str) -> Result<FrontResult, String> {
    let mut job = FrontJob { facts: t.facts.clone(), values: t.values.clone(), pixels: t.pixels.clone(), fns: t.fns.borrow().clone(), evals: std::mem::take(&mut *t.evals.borrow_mut()), vulkan: ogpu_core::impl_::backend().map(|b| b == "vulkan").unwrap_or(false) };
    let name = name.to_string();
    let handle = std::thread::Builder::new()
        .stack_size(BIG_STACK)
        .spawn(move || {
            let result = front(&mut job, &name);
            (job, result)
        })
        .expect("front-end thread");
    let (job, result) = match handle.join() {
        Ok(done) => done,
        Err(panic) => std::panic::resume_unwind(panic),
    };
    *t.fns.borrow_mut() = job.fns;
    *t.evals.borrow_mut() = job.evals;
    result
}

fn cached_front(t: &T, name: &str) -> Result<FrontResult, String> {
    let key = format!("{:?}|{name}|{}", t.facts, ogpu_core::impl_::backend().unwrap_or_default());
    if let Some(found) = FRONTS.with(|f| f.borrow().iter().find(|(k, _)| *k == key).map(|(_, v)| v.clone())) {
        return Ok(found);
    }
    let made = crate::lisp_kernel_cache::disk_cached(t, name, &|| front_big(t, name))?;
    FRONTS.with(|f| {
        let mut f = f.borrow_mut();
        f.insert(0, (key, made.clone()));
        f.truncate(FRONT_ENTRIES);
    });
    Ok(made)
}

fn kernel_of(t: &T, lease: &renderer::renderer::Gpu, pipes: &pipelines::T, name: &str) -> Result<Rc<Kernel>, String> {
    if let Some((_, k)) = t.kernels.borrow().iter().find(|(n, _)| n == name) {
        return Ok(k.clone());
    }
    let (msl, base, slots) = cached_front(t, name)?;
    let k = Rc::new(Kernel { runner: run::create(None, lease, pipes, &msl), msl, base, slots });
    t.kernels.borrow_mut().push((name.to_string(), k.clone()));
    Ok(k)
}

/// Encodes kernel `name` over the film's `count` pixels into `commands`, writing `into`: `words(n)` is
/// uniform `n`'s current value and `table(n)` the buffer and element count bound for table `n`.
pub fn dispatch(
    t: &T,
    lease: &renderer::renderer::Gpu,
    pipes: &pipelines::T,
    commands: &Commands,
    count: i64,
    pixels: &Buffer,
    slots: Option<&Buffer>,
    cell: &Buffer,
    into: &Buffer,
    scene: Option<run::Scene>,
    extras: Option<run::Extras>,
    resident: Vec<Accel>,
    words: &dyn Fn(&str) -> Vec<f64>,
    table: &dyn Fn(&str) -> (Buffer, i64),
    name: &str,
) -> Result<(), String> {
    let k = kernel_of(t, lease, pipes, name)?;
    let uniforms: Vec<Vec<f64>> = k
        .base
        .uniforms
        .iter()
        .enumerate()
        .map(|(u, old)| match &k.slots[u] {
            Some((n, ty)) if !matches!(ty, Ty::Array(_)) => words(n),
            _ => old.clone(),
        })
        .collect();
    let inputs = phost::Inputs { arrays: k.base.arrays.iter().map(|_| Arc::new(vec![])).collect(), uniforms, tables: k.base.tables.clone(), frame: k.base.frame.clone(), count };
    let options = run::Options {
        uint32_cpu: true,
        commands: Some(commands.clone()),
        reset_status: Some(false),
        into: Some((into.clone(), cell.clone())),
        scene,
        extras,
        resident,
        ..Default::default()
    };
    let slots = slots.cloned();
    let pixels = pixels.clone();
    let bound = move |i: i64| if i == 0 { Some(pixels.clone()) } else { slots.clone() };
    let bound_table = |u: i64| match &k.slots[u as usize] {
        Some((n, ty @ Ty::Array(_))) => {
            let (buffer, count) = table(n);
            Some((buffer, count * width(ty)))
        }
        _ => None,
    };
    run::dispatch(&k.runner, &options, Some(&bound), Some(&bound_table), &inputs).map(|_| ()).map_err(|d| rays_diagnostic::to_string(&d))
}

pub fn max_total_threads(t: &T, lease: &renderer::renderer::Gpu, pipes: &pipelines::T, name: &str) -> Result<i64, String> {
    let k = kernel_of(t, lease, pipes, name)?;
    run::max_total_threads(&k.runner).map_err(|d| rays_diagnostic::to_string(&d))
}

pub fn source(t: &T, lease: &renderer::renderer::Gpu, pipes: &pipelines::T, name: &str) -> Result<String, String> {
    let k = kernel_of(t, lease, pipes, name)?;
    Ok(k.msl.source.clone())
}

pub fn close(t: &T) {
    for (_, k) in t.kernels.borrow().iter() {
        run::close(&k.runner, false);
    }
    t.kernels.borrow_mut().clear();
}

/// A device mesh's triangle corners: each corner's vec3 from a device's packed float3 `src` (positions or
/// normals) at its vertex index, the input (a float per corner, uploaded once per topology), written packed
/// into the tracer's per-corner buffer that the structure and the `nrm` table read.
pub struct Gather {
    pub runner: run::T,
    pub base: phost::Inputs,
    pub src_slot: usize,
}

pub fn gather_kernel(lease: &renderer::renderer::Gpu, pipes: &pipelines::T) -> Result<Gather, String> {
    let src = Value::Floats(3, Arc::new(vec![0.; 3]));
    let text = "(workspace pt (graph g :context value (let* [src (pt/src) gather (fn [(v : int)] (array/nth src v))] 0.0)))";
    let src_op = {
        let mut o = placeholder_op("src", array_of(Ty::Vec3));
        o.name = "pt/src".to_string();
        o
    };
    let forms = syntax::parse(text).map_err(|d| rays_diagnostic::to_string(&d))?;
    let ws = match w::check(&[src_op], false, true, None, &catalog(), &forms) {
        (Some(ws), ds) if ds.is_empty() => ws,
        (_, ds) => return Err(diagnostics(&ds)),
    };
    let evaluated = e::r#static(&ws, true, None).map_err(|d| rays_diagnostic::to_string(&d))?;
    let path = vec!["g".to_string(), "gather".to_string()];
    let f = match evaluated.records.iter().find(|(p, _)| *p == path).and_then(|(_, l)| l.first()) {
        Some((_, Value::Fn(id))) => *id,
        _ => return Err("path tracer gather is not a function".to_string()),
    };
    let replacement = src.clone();
    let bound = e::bind_captures(f, &move |v| match v {
        Value::Source(p) if p.name == "pt/src" && p.fields.is_empty() => replacement.clone(),
        v => v.clone(),
    });
    let residual = deferred("gather", bound, &[Value::IntArray(Arc::new(vec![0]))], Ty::Vec3)?;
    let term = e::residual::residual_view(residual).map_err(|d| rays_diagnostic::to_string(&d))?.term;
    let template = packed::compile_template(&packed::CompileOptions::default(), residual, &term).ok_or("path tracer gather has no device template")?;
    let options = emit::Options {
        spirv: ogpu_core::impl_::backend().map(|b| b == "vulkan").unwrap_or(false),
        fast: true,
        uint32_scalars: true,
        uint32_inputs: vec![0],
        ..Default::default()
    };
    let msl = emit::kernel(&options, &template).map_err(|d| rays_diagnostic::to_string(&d))?;
    let base = phost::prepare(&template, &phost::PrepareOptions { fast: true, ..Default::default() }, &input::at_time(0.)).map_err(|d| rays_diagnostic::to_string(&d))?;
    let src_slot = phost::uniform_shapes(&template)
        .iter()
        .position(|s| matches!(s, phost::Shape::Value(v) if same_placeholder(v, &src)))
        .ok_or("path tracer gather reads no source table")?;
    let mut gather_base = base;
    gather_base.arrays = gather_base.arrays.iter().map(|_| Arc::new(vec![])).collect();
    Ok(Gather { runner: run::create(None, lease, pipes, &msl), base: gather_base, src_slot })
}

/// Encodes the gather of `count` corners whose vertex indices `indices` holds, from `src` (`vertices`
/// packed float3) into `into`.
pub fn gather_dispatch(g: &Gather, commands: &Commands, cell: &Buffer, indices: &Buffer, count: i64, src: &Buffer, vertices: i64, into: &Buffer) -> Result<(), String> {
    let options = run::Options { commands: Some(commands.clone()), reset_status: Some(false), into: Some((into.clone(), cell.clone())), ..Default::default() };
    let indices = indices.clone();
    let bound = move |i: i64| if i == 0 { Some(indices.clone()) } else { None };
    let src = src.clone();
    let slot = g.src_slot as i64;
    let bound_table = move |u: i64| if u == slot { Some((src.clone(), 3 * vertices)) } else { None };
    let inputs = phost::Inputs { count, arrays: g.base.arrays.clone(), uniforms: g.base.uniforms.clone(), tables: g.base.tables.clone(), frame: g.base.frame.clone() };
    run::dispatch(&g.runner, &options, Some(&bound), Some(&bound_table), &inputs).map(|_| ()).map_err(|d| rays_diagnostic::to_string(&d))
}

pub fn close_gather(g: &Gather) {
    run::close(&g.runner, false);
}

/// The CPU walker's run of kernel `name` over `pixels` (the oracle a device film is checked against):
/// `words(n)` gives uniform `n` and `value(n)` table `n` as host values. It evaluates its own copy of the
/// declarations (see the module notes).
pub fn cpu(t: &T, words: Vec<(String, Vec<f64>)>, values: Vec<(Symbol, Value)>, pixels: Value, name: &str) -> Result<Value, String> {
    let facts = t.facts.clone();
    let name = name.to_string();
    let checked = t.oracle.borrow().clone();
    let handle = std::thread::Builder::new().stack_size(BIG_STACK).spawn(move || cpu_here(facts, checked, words, values, pixels, name)).expect("front-end thread");
    let (result, checked) = match handle.join() {
        Ok(done) => done,
        Err(panic) => std::panic::resume_unwind(panic),
    };
    *t.oracle.borrow_mut() = checked;
    result
}

fn cpu_here(facts: Facts, mut checked: Option<w::Workspace>, words: Vec<(String, Vec<f64>)>, values: Vec<(Symbol, Value)>, pixels: Value, name: String) -> (Result<Value, String>, Option<w::Workspace>) {
    let result = cpu_run(&facts, &mut checked, words, values, pixels, &name);
    (result, checked)
}

fn cpu_run(facts: &Facts, checked: &mut Option<w::Workspace>, words: Vec<(String, Vec<f64>)>, values: Vec<(Symbol, Value)>, pixels: Value, name: &str) -> Result<Value, String> {
    let words = |n: &str| words.iter().find(|(k, _)| k == n).expect("uniform").1.clone();
    let value = |n: &str| values.iter().find(|(k, _)| k == n).expect("table").1.clone();
    let (fns, _keep) = functions(facts, PREVIEWS.contains(&name), checked)?;
    let f = fns.iter().find(|(n, _)| n == name).map(|(_, f)| *f).ok_or_else(|| format!("no path tracer kernel {name}"))?;
    let types: Vec<(String, Ty)> = uniforms().into_iter().chain(tables()).map(|(n, ty)| (n.to_string(), ty)).collect();
    let bound = e::bind_captures(f, &|v| match v {
        Value::Source(p) if p.name.starts_with("pt/") && p.fields.is_empty() => {
            let n = &p.name[3..];
            let ty = &types.iter().find(|(k, _)| k == n).expect("pt placeholder").1;
            match ty {
                Ty::Array(_) => value(n),
                Ty::Vec3 => {
                    let w = words(n);
                    Value::Vec3(w[0], w[1], w[2])
                }
                Ty::Int => Value::Int(words(n)[0] as i64),
                _ => Value::Float(words(n)[0]),
            }
        }
        v => v.clone(),
    });
    let inputs = if name == "trace" { vec![pixels.clone(), pixels] } else { vec![pixels] };
    let residual = deferred(name, bound, &inputs, Ty::Vec4)?;
    let term = e::residual::residual_view(residual).map_err(|d| rays_diagnostic::to_string(&d))?.term;
    let program = packed::compile_result(&packed::CompileOptions::default(), residual, &term).map_err(|ds| diagnostics(&ds))?;
    packed::force(&program, &input::at_time(0.), &Default::default()).map_err(|d| rays_diagnostic::to_string(&d))
}

/// Forgets this thread's in-memory front-end results (the disk cache stays): a test reading the disk
/// cache back needs the memory layer out of the way.
pub fn forget_memory_cache() {
    FRONTS.with(|f| f.borrow_mut().clear());
}
