//! Specialized numeric packed maps, loops and reductions with scalar registers in 1,024-element
//! blocks (port of lib/flow_ir/packed.ml). Accumulators retain reference evaluation order.
//!
//! Rust shape notes: OCaml's optional arguments become option structs ([`CompileOptions`],
//! [`ForceOptions`], [`PrepareOptions`]); a kernel's exceptions become [`KErr`] values; the chunks
//! of a launch write disjoint `&mut` slices cut from the output arrays (`split_parts`), so the
//! parallel path needs no `unsafe`.
#![allow(non_snake_case, non_camel_case_types, clippy::too_many_arguments)]

use crate::core_fuse;
use crate::core_ir::region::{self, Instruction, LiveFloat, LiveInt, Loop, Projection, RegionResult};
use crate::core_region;
use flow_check::workspace::{Node, Path, Term};
use flow_check::{op, ray_host};
use flow_eval as E;
use flow_eval::{Live, State};
use flow_syntax::context;
use flow_syntax::packed_ops::{self, Binary, Conversion, Integer, Unary};
use flow_syntax::ty::Ty;
use flow_syntax::value::{self, ResidualId, Value, struct_value};
use rays_diagnostic::Diagnostic;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, LazyLock, Mutex, OnceLock};

pub use crate::core_ir::region::{Iteration, Stage};
pub use crate::core_ir::region::Origin;
pub use host::Inputs;
pub use region::CountSource;

pub type T = region::T;
pub type Resolve = Arc<dyn Fn(&Value) -> Result<Value, Diagnostic> + Send + Sync>;
pub type Dispatch = Arc<dyn Fn(ResidualId, &Term) -> Result<Value, Diagnostic> + Send + Sync>;
pub type Clock = Arc<dyn Fn() -> f64 + Send + Sync>;
/// `(region, seconds, reference)`.
pub type Report = Arc<dyn Fn(&T, f64, bool) + Send + Sync>;

/// OCaml `max_int` (2^62 - 1): the iteration-count overflow bound of a product.
/// OCaml `Sys.max_array_length` = `Sys.max_floatarray_length` on 64 bits.
const MAX_ARRAY: usize = op::MAX_WOSIZE;

#[derive(Clone)]
pub struct Measure {
    pub clock: Clock,
    pub report: Report,
}

/// A borrowed source column: elements at `offset + i * stride` of `data`.
#[derive(Clone, Debug)]
pub struct Column {
    pub data: Arc<Vec<f64>>,
    pub offset: i64,
    pub stride: i64,
}

pub type ColumnSource = Arc<dyn Fn(ResidualId, &Term) -> Result<Option<Vec<Column>>, Diagnostic> + Send + Sync>;

/// Why a kernel stopped: a form it cannot run (the caller falls back to the reference
/// interpreter) or a diagnostic.
#[derive(Clone, Debug)]
pub enum KErr {
    Unsupported,
    Diag(Diagnostic),
}
impl From<Diagnostic> for KErr {
    fn from(d: Diagnostic) -> KErr {
        KErr::Diag(d)
    }
}
type K<X> = Result<X, KErr>;
fn interrupted(expired: &Option<Arc<dyn Fn() -> bool + Send + Sync>>, run: &mut dyn FnMut() -> K<()>) -> K<()> {
    let mut result = Ok(());
    let mut work = || {
        if let Err(d) = E::interruptible(&mut || result = run()) {
            result = Err(KErr::Diag(d));
        }
    };
    match expired {
        Some(expired) => {
            if !E::with_interrupt(expired.clone(), &mut work) {
                return Err(fail("cancelled", "Evaluation interrupted."));
            }
        }
        None => work(),
    }
    result
}

fn fail(code: &str, message: &str) -> KErr {
    KErr::Diag(value::fail(code, message))
}

fn stage_refused() -> KErr {
    fail("E_GPU_STAGE", "A raster-stage intrinsic runs only in a fragment stage.")
}

#[cold]
#[inline(never)]
fn nonfinite<X>() -> K<X> {
    Err(fail("E_NONFINITE", "packed produced a nonfinite value."))
}

/// Instructions that are typed (integer, text, table, loop): the general runner takes them.
fn typed_instruction(i: &Instruction) -> bool {
    use Instruction::*;
    match i {
        Int_const(_) => false, // compile-time configuration literals may be unused
        Int_input(_) | Int_uniform(_) | Int_frame(_) | Int_accumulator | Int_binary(..) | Int_compare(..)
        | Int_abs(_) | Convert(..) | Int_select(..) | Raise(..) | Rand_at(..) | Trace(..)
        | Array_prepare(..) | Gather(..) | Int_gather(..) | Text_const(_) | Text_input(_)
        | Text_uniform(_) | Text_gather(..) | Text_select(..) => true,
        Loop_begin(_) | Loop_carry(..) | Loop_end(..) | Loop_result(..) => true,
        Guarded(_, body) => typed_instruction(body),
        _ => false,
    }
}

fn is_array_of_int(term: &Term) -> bool {
    *term.ty() == Ty::Array(Box::new(Ty::Int))
}

pub fn integer_program(t: &T) -> bool {
    t.code.iter().any(typed_instruction)
        || t.outputs.iter().any(|o| o.iter().any(|r| region::integer_register(&t.code[*r as usize])))
        || t.sources.iter().any(|(_, term)| is_array_of_int(term))
}

// a noise's lacunarity or gain at element `j`; a live one is checked as noise3 checks it
#[inline(always)]
fn fractal_at(value: &dyn Fn(i64, usize) -> f64, fractal: LiveFloat, j: usize) -> f64 {
    match fractal {
        LiveFloat::Fixed(v) => v,
        LiveFloat::Live(r) => value(r, j),
    }
}

fn live_fractal(lacunarity: LiveFloat, gain: LiveFloat) -> bool {
    !matches!((lacunarity, gain), (LiveFloat::Fixed(_), LiveFloat::Fixed(_)))
}

#[inline(always)]
fn checked_projection(projection: Projection, value: f64) -> f64 {
    let valid = value.is_finite();
    match projection {
        Projection::Value => if valid { value } else { 0.0 },
        Projection::Valid => if valid { 1.0 } else { 0.0 },
    }
}

fn convert_int(op: &Conversion, x: f64) -> K<i64> {
    Ok(match op {
        Conversion::Truncate => value::to_int("int", x.trunc())?,
        Conversion::Floor => value::to_int("floor", x.floor())?,
        Conversion::Ceil => value::to_int("ceil", x.ceil())?,
        Conversion::Round => value::to_int("round", x.round())?,
        Conversion::CoerceInt => value::to_int("int", (x + 0.5).floor())?,
        Conversion::IntegerResult(name) => value::to_int(name, (x + 0.5).floor())?,
        Conversion::ToFloat => unreachable!("To_float is not an integer conversion"),
    })
}



fn zipped(t: &T) -> bool {
    t.iteration == Iteration::Zip || t.sources.len() == 1
}

/// Fresh instruction mask used by GPU qualification and code generation.
pub fn output_reachable(t: &T) -> Vec<bool> {
    let mut used = vec![false; t.code.len()];
    fn mark(t: &T, used: &mut Vec<bool>, slot: usize) {
        if used[slot] {
            return;
        }
        used[slot] = true;
        for r in region::operands(&t.code[slot]) {
            mark(t, used, r as usize);
        }
        if let Instruction::Loop_begin(loop_) = Instruction::unguarded(&t.code[slot]) {
            for body in slot + 1..=loop_.finish as usize {
                mark(t, used, body);
            }
        }
    }
    for output in t.outputs.iter() {
        for r in output {
            mark(t, &mut used, *r as usize);
        }
    }
    used
}

/// After `gpu_refusals` succeeds, instructions whose result needs a finite check.
pub fn finite_checks(t: &T) -> Vec<bool> {
    t.code
        .iter()
        .map(|instruction| match Instruction::unguarded(instruction) {
            Instruction::Const(_) | Instruction::Select(..) | Instruction::Array_prepare(..)
            | Instruction::Unary(Unary::Abs | Unary::Sqrt | Unary::Not, _)
            | Instruction::Loop_begin(_) | Instruction::Loop_carry(..) | Instruction::Loop_end(..)
            | Instruction::Loop_result(..) | Instruction::Checked_mul(..) | Instruction::Checked_fma(..)
            | Instruction::Binary(
                Binary::Min | Binary::Max | Binary::Lt | Binary::Le | Binary::Gt | Binary::Ge | Binary::Eq | Binary::And | Binary::Or,
                _,
                _,
            ) => false,
            body if region::text_register(body) => false,
            _ => true,
        })
        .collect()
}

fn gpu_error(message: &str) -> Diagnostic {
    value::fail("E_GPU_FORM", message)
}

/// Pure form restrictions shared by placement, display execution and emission. `fused` admits
/// fused and checked multiply-add; the default keeps the binary64 forms (the web export).
pub fn gpu_refusals(fused: bool, fast: bool, t: &T) -> Vec<Diagnostic> {
    let mut reasons: Vec<Diagnostic> = Vec::new();
    let any = |f: &dyn Fn(&Instruction) -> bool| t.code.iter().any(|i| f(Instruction::unguarded(i)));
    if t.code.iter().any(region::text_register) {
        reasons.push(gpu_error("Text kernels stay on the CPU."));
    }
    if !fast && any(&|i| matches!(i, Instruction::Trace(..))) {
        reasons.push(gpu_error("scene/trace needs fast precision and a bound acceleration structure."));
    }
    // fast precision (the workspace default) takes gathers and int32 on the device
    if !fast && any(&|i| matches!(i, Instruction::Array_prepare(..) | Instruction::Gather(..) | Instruction::Int_gather(..))) {
        reasons.push(gpu_error("Indexed array reads stay on the CPU."));
    }
    if !fast && integer_program(t) && !t.code.iter().any(region::text_register) {
        reasons.push(gpu_error("Integer sources, uniforms and instructions stay on the CPU."));
    }
    if fast && any(&|i| matches!(i, Instruction::Rand_at(..) | Instruction::Int_accumulator)) {
        reasons.push(gpu_error("Random streams and integer accumulators stay on the CPU."));
    }
    if any(&|i| matches!(i, Instruction::Binary(Binary::Hypot, _, _))) {
        reasons.push(gpu_error("Hypot requires the CPU binary64 implementation."));
    }
    if !fused {
        if any(&|i| matches!(i, Instruction::Fma(..))) {
            reasons.push(gpu_error("FMA requires exact single-rounding binary64 arithmetic."));
        }
        if any(&|i| matches!(i, Instruction::Checked_mul(..) | Instruction::Checked_fma(..))) {
            reasons.push(gpu_error("Checked arithmetic requires binary64 overflow status."));
        }
    }
    if t.result != RegionResult::Collect || !zipped(t) || !t.skip.is_empty() {
        reasons.push(gpu_error("GPU kernels require Collect with Zip iteration and no skipped elements."));
    }
    if any(&|i| matches!(i, Instruction::Accumulator(_) | Instruction::Int_accumulator)) {
        reasons.push(gpu_error("Ordered accumulators stay on the CPU."));
    }
    let used = output_reachable(t);
    let (mut bad_noise, mut bad_constant) = (false, false);
    for (i, instruction) in t.code.iter().enumerate() {
        if used[i] {
            match Instruction::unguarded(instruction) {
                Instruction::Noise3(_, _, _, _, LiveInt::Fixed(octaves), _, _) => {
                    if !packed_ops::supported_gpu_noise_octaves(*octaves) {
                        bad_noise = true;
                    }
                }
                Instruction::Const(value) => {
                    if !packed_ops::finite_float32(*value) {
                        bad_constant = true;
                    }
                }
                // a captured constant is a uniform; its current value takes the constant's check
                Instruction::Uniform(u, c) => {
                    if let Ok(values) = components(&t.uniforms[*u as usize]) {
                        if values.get(*c as usize).is_some_and(|x| !packed_ops::finite_float32(*x)) {
                            bad_constant = true;
                        }
                    }
                }
                _ => {}
            }
        }
    }
    if bad_noise {
        reasons.push(gpu_error("GPU noise supports 1 to 32 octaves."));
    }
    if bad_constant {
        reasons.push(gpu_error("Nonfinite float32 shader constants require reference evaluation."));
    }
    reasons
}

// ---------------------------------------------------------------------------------------------
// Compilation

/// Optional arguments of [`compile_result`] and friends.
#[derive(Clone)]
pub struct CompileOptions {
    /// Sees every compilation result; tests record snapshots through it.
    pub trace: Option<Arc<dyn Fn(&Result<T, Vec<Diagnostic>>) + Send + Sync>>,
    pub through_exact: bool,
    /// Fusion defaults to true; false retains intermediate arrays for parity checks and profiling.
    pub fusion: bool,
    pub count_source: Option<CountSource>,
}
impl Default for CompileOptions {
    fn default() -> CompileOptions {
        CompileOptions { trace: None, through_exact: false, fusion: true, count_source: None }
    }
}

pub(crate) fn compile_impl(
    through_exact: bool,
    fusion: bool,
    dynamic: bool,
    count_source: &CountSource,
    residual: ResidualId,
    term: &Term,
) -> Result<T, Vec<Diagnostic>> {
    let _phase = flow_syntax::phase_timer::measure("packed.compile");
    core_region::lower(fusion, dynamic, count_source, residual, term)
        .map(|region| if fusion { core_fuse::fuse(through_exact, region) } else { region })
}

fn none_source() -> CountSource {
    CountSource::none()
}

pub fn static_count(t: &T) -> Option<i64> {
    t.count
}
pub fn count_origin(t: &T) -> Option<Origin> {
    t.origin.clone()
}

fn traced(
    trace: &Option<Arc<dyn Fn(&Result<T, Vec<Diagnostic>>) + Send + Sync>>,
    result: Result<T, Vec<Diagnostic>>,
) -> Result<T, Vec<Diagnostic>> {
    if let Some(f) = trace {
        f(&result);
    }
    result
}

/// The first actual compilation refusal, with its source location. Declined fusion retains a
/// valid unfused program and is not a refusal.
pub fn compile_result(options: &CompileOptions, residual: ResidualId, term: &Term) -> Result<T, Vec<Diagnostic>> {
    let source = options.count_source.clone().unwrap_or_else(none_source);
    // Captures lower as uniforms so a changed parameter reuses the compiled shader (a 36 KB MSL
    // kernel costs 150-450 ms to compile); a body that needs a capture as a constant falls back.
    let fixed = || compile_impl(options.through_exact, options.fusion, false, &source, residual, term);
    // A producer that needs a capture as a constant (a noise seed) is not fused into the uniform
    // program: it stays a separate source, run on the host. When constants fuse more stages, they win
    // (live captures stay uniforms either way).
    let (dynamic, read) = core_region::tracking_dynamic(|| compile_impl(options.through_exact, options.fusion, true, &source, residual, term));
    let compiled = match dynamic {
        // a compile no capture mode steered is also the fixed one: equal stages, so dynamic wins
        Ok(dynamic) if !read => Ok(dynamic),
        Ok(dynamic) if options.fusion => match fixed() {
            Ok(fixed) if fixed.stages.len() > dynamic.stages.len() => Ok(fixed),
            _ => Ok(dynamic),
        },
        Ok(dynamic) => Ok(dynamic),
        Err(_) => fixed(),
    };
    traced(&options.trace, compiled)
}

/// Unsupported bodies remain on the reference interpreter.
pub fn compile(options: &CompileOptions, residual: ResidualId, term: &Term) -> Option<T> {
    compile_result(options, residual, term).ok()
}

pub fn compile_template(options: &CompileOptions, residual: ResidualId, term: &Term) -> Option<T> {
    let source = options.count_source.clone().unwrap_or_else(none_source);
    traced(&options.trace, compile_impl(false, true, true, &source, residual, term)).ok()
}

/// A frame-fold template keeps captures as uniforms and reuses its register code with the next
/// step's immutable bindings. A changed function body or a fused lexical scope requires a fresh
/// specialization.
pub fn rebind(t: &T, residual: ResidualId) -> Option<T> {
    core_region::rebind(t, residual)
}

/// False for sums and accumulator steps; they must not fuse as independent elements.
pub fn elementwise(t: &T) -> bool {
    matches!(t.result, RegionResult::Collect)
}

/// Graph instance, authored site and iteration tuple for every fused stage.
pub fn provenance(t: &T) -> Vec<(i64, Path, Vec<i64>)> {
    let mut sites = (*t.sites).clone();
    sites.sort();
    sites.dedup();
    sites
}

/// The consuming stage, where the group's single timing belongs.
pub fn site(t: &T) -> (i64, Path, Vec<i64>) {
    match E::residual::residual_site(t.residual) {
        Some((instance, site, iter)) => (instance, t.term.path().map(|ids| ids.iter().map(|&id| t.term.name(id).to_owned()).collect()).unwrap_or(site), iter),
        None => (0, t.term.path().map(|ids| ids.iter().map(|&id| t.term.name(id).to_owned()).collect()).unwrap_or_default(), Vec::new()),
    }
}

// ---------------------------------------------------------------------------------------------
// Chunk layout, shared helpers

/// `(block_size, blocks, blocks_per_chunk, chunks)`: sixteen blocks a chunk, or fewer so that a
/// smaller map still makes four or more chunks: a 13,824-voxel map was one chunk, run by one
/// domain. Each chunk allocates its lane scratch, so chunks stay several blocks. Chunks only
/// schedule; sums run them in order.
pub fn block_layout(count: usize, limit: usize) -> (usize, usize, usize, usize) {
    let block_size = 1.max(count.min(limit));
    let blocks = if count == 0 { 0 } else { (count - 1) / block_size + 1 };
    let blocks_per_chunk = 1.max(16.min(blocks / 4));
    let chunks = if blocks == 0 { 0 } else { (blocks - 1) / blocks_per_chunk + 1 };
    (block_size, blocks, blocks_per_chunk, chunks)
}

fn borrowed_count(width: usize, columns: &[Column]) -> K<usize> {
    if columns.len() != width {
        return Err(fail("E_ARRAY_TYPE", "Borrowed columns changed the packed element width."));
    }
    let length = |c: &Column| -> K<usize> {
        let len = c.data.len() as i64;
        if c.stride <= 0 || c.offset < 0 || c.offset > len {
            Err(fail("E_ARRAY_RANGE", "Borrowed column offset and stride must describe an in-bounds span."))
        } else if c.offset == len {
            Ok(0)
        } else {
            Ok((1 + (len - 1 - c.offset) / c.stride) as usize)
        }
    };
    let count = length(&columns[0])?;
    for c in columns {
        if length(c)? != count {
            return Err(fail("E_ARRAY_COUNT", "Borrowed components need the same element count."));
        }
    }
    Ok(count)
}

fn array_len_k(value: &Value) -> K<usize> {
    Ok(value::array_length(value)?)
}

/// Splits `data` (elements of `width` floats) at the element boundaries `bounds` into disjoint
/// parts, one per chunk; a chunk takes its part out of its cell.
fn split_parts<'a>(data: &'a mut [f64], bounds: &[usize], width: usize) -> Vec<Mutex<Option<&'a mut [f64]>>> {
    let mut parts = Vec::with_capacity(bounds.len().saturating_sub(1));
    let mut rest = data;
    for c in 0..bounds.len().saturating_sub(1) {
        let (head, tail) = std::mem::take(&mut rest).split_at_mut((bounds[c + 1] - bounds[c]) * width);
        parts.push(Mutex::new(Some(head)));
        rest = tail;
    }
    parts
}

type NodeArgs = [(String, Value)];

fn unsupported_node(_: &str, _: &NodeArgs) -> Result<Value, Diagnostic> {
    Err(value::fail("$unsupported", "node"))
}

fn node_error(d: Diagnostic) -> KErr {
    if d.code == "$unsupported" { KErr::Unsupported } else { KErr::Diag(d) }
}

/// A frame fact's value (`t` or a value-context op over the live frame).
fn frame_fact(name: &str, live: &Live) -> K<Value> {
    if name == "t" {
        return Ok(Value::Float(live.t));
    }
    match op::find(&[], name, &context::value()) {
        Some(op) => (op.body)(live, &unsupported_node, &[]).map_err(node_error),
        None => Err(KErr::Unsupported),
    }
}

fn num_k(v: &Value) -> K<f64> {
    Ok(value::num(v)?)
}

fn components(v: &Value) -> K<Vec<f64>> {
    components_k(v)
}

fn noise_error(message: String) -> KErr {
    KErr::Diag(value::fail("E_KERNEL", &message))
}

// ---------------------------------------------------------------------------------------------
// Whole-kernel fast paths
//
// A launch whose region has one of these exact shapes skips the register machine. They perform the
// same binary64 operations in the same order, so the results equal the general path's bit for bit.

struct Streaming {
    noise: usize,
    amplitude: usize,
    x: (i64, i64, i64),
    y: (i64, i64, i64),
    z: (i64, i64, i64),
    frequency: Option<usize>,
    products: Vec<Vec<((i64, i64, i64), (i64, i64, i64))>>,
}

struct ScalarStreaming {
    noise: usize,
    x: (i64, i64, i64),
    y: (i64, i64, i64),
    z: Option<(i64, i64, i64)>,
    frequency: Option<usize>,
    gain: usize,
    bias: usize,
    amplitude: usize,
    outputs: Vec<Vec<((i64, i64, i64), Option<usize>)>>,
}

enum Plan {
    Noise(Streaming),
    Scalar(ScalarStreaming),
}

/// A matched plan with what its loops read: float columns, the noise table and the scalar register
/// values (`Const`, `Uniform` and `Frame` results by register).
struct Fast {
    plan: Plan,
    columns: Vec<Vec<Column>>,
    inputs: Vec<Arc<Vec<f64>>>,
    scalars: Vec<f64>,
    table: Arc<rays_math::noise::T>,
    block_size: usize,
    planes: bool,
}

struct Blk {
    start: usize,
    length: usize,
}

// A summed or accumulated chunk whose additions stay in reference order.
fn chunk_ordered(sh: &TS, chunk: usize) -> K<()> {
    let source = match &sh.inputs[0] {
        Value::Floats(1..=4, d) => d,
        _ => unreachable!("ordered add over a float array"),
    };
    let first = chunk * sh.blocks_per_chunk * sh.block_size;
    let last = sh.count.min((chunk + 1) * sh.blocks_per_chunk * sh.block_size);
    let mut acc = sh.accumulator.lock().unwrap();
    let width = sh.t.outputs[0].len();
    let mut total = components_k(&acc)?;
    if width == 3 {
        let (mut x, mut y, mut z) = (total[0], total[1], total[2]);
        for i in first..last {
            let offset = i * 3;
            let next = x + source[offset];
            if !next.is_finite() {
                return nonfinite();
            }
            x = next;
            let next = y + source[offset + 1];
            if !next.is_finite() {
                return nonfinite();
            }
            y = next;
            let next = z + source[offset + 2];
            if !next.is_finite() {
                return nonfinite();
            }
            z = next;
        }
        total.copy_from_slice(&[x, y, z]);
    } else {
        for i in first..last {
            for component in 0..width {
                let x = total[component] + source[i * width + component];
                if !x.is_finite() {
                    return nonfinite();
                }
                total[component] = x;
            }
        }
    }
    *acc = scalar_value_of(width, &total);
    Ok(())
}

fn column_at(c: &Column, i: usize) -> f64 {
    c.data[(c.offset + i as i64 * c.stride) as usize]
}

// Noise3 scaled by one amplitude and added to a base: sampled straight into the outputs.
fn block_streaming(
    r: &Fast,
    scratch: &mut [f64],
    b: &Blk,
    plan: &Streaming,
    base: usize,
    outs: &mut [Option<&mut [f64]>],
    planes: &mut [Vec<Option<&mut [f64]>>],
) -> K<()> {
    let bs = r.block_size;
    let (x, y, z) = (
        &r.columns[plan.x.0 as usize][plan.x.2 as usize],
        &r.columns[plan.y.0 as usize][plan.y.2 as usize],
        &r.columns[plan.z.0 as usize][plan.z.2 as usize],
    );
    let noise = &r.table;
    let length = b.length;
    let frequency = plan.frequency.map(|f| r.scalars[f]).unwrap_or(1.0);
    rays_math::noise::sample3_into(
        noise,
        0,
        length as i64,
        Some(x.offset + b.start as i64 * x.stride),
        Some(y.offset + b.start as i64 * y.stride),
        Some(z.offset + b.start as i64 * z.stride),
        None,
        Some(x.stride),
        Some(y.stride),
        Some(z.stride),
        Some(frequency),
        &x.data,
        &y.data,
        &z.data,
        scratch,
    )
    .map_err(noise_error)?;
    let amplitude = r.scalars[plan.amplitude];
    for j in 0..length {
        let noise = scratch[j];
        scratch[j] = noise * amplitude;
        // Multiplication cannot make NaN/infinity finite, so this check also rejects every
        // nonfinite noise sample.
        if !scratch[j].is_finite() {
            return nonfinite();
        }
    }
    let output_start = b.start - base;
    for (k, components) in plan.products.iter().enumerate() {
        let width = components.len();
        if r.planes {
            for (component, (bse, dir)) in components.iter().enumerate() {
                let bcol = &r.columns[bse.0 as usize][bse.2 as usize];
                let dcol = &r.columns[dir.0 as usize][dir.2 as usize];
                for j in 0..length {
                    scratch[bs + j] = column_at(dcol, b.start + j) * scratch[j];
                    if !scratch[bs + j].is_finite() {
                        return nonfinite();
                    }
                    let v = column_at(bcol, b.start + j) + scratch[bs + j];
                    if !v.is_finite() {
                        return nonfinite();
                    }
                    if let Some(data) = &mut planes[k][component] {
                        data[output_start + j] = v;
                    }
                }
            }
        } else {
            let output = outs[k].as_deref_mut().expect("collected output");
            let vector = if width == 3 {
                let ((bi, bw, bx), (di, dw, dx)) = components[0];
                let ((bj, bv, by), (dj, dv, dy)) = components[1];
                let ((bk, bu, bz), (dk, du, dz)) = components[2];
                if bi == bj && bj == bk && bw == bv && bv == bu && di == dj && dj == dk && dw == dv && dv == du {
                    Some((bi as usize, bw as usize, bx as usize, by as usize, bz as usize, di as usize, dw as usize, dx as usize, dy as usize, dz as usize))
                } else {
                    None
                }
            } else {
                None
            };
            match vector {
                Some((bi, bw, bx, by, bz, di, dw, dx, dy, dz)) => {
                    let (base_in, dir_in) = (&r.inputs[bi], &r.inputs[di]);
                    for j in 0..length {
                        let at = (output_start + j) * 3;
                        let (bj, dj, scale) = ((b.start + j) * bw, (b.start + j) * dw, scratch[j]);
                        output[at] = dir_in[dj + dx] * scale;
                        output[at + 1] = dir_in[dj + dy] * scale;
                        output[at + 2] = dir_in[dj + dz] * scale;
                        output[at] = base_in[bj + bx] + output[at];
                        output[at + 1] = base_in[bj + by] + output[at + 1];
                        output[at + 2] = base_in[bj + bz] + output[at + 2];
                        // A nonfinite product remains nonfinite after addition.
                        if !(output[at].is_finite() && output[at + 1].is_finite() && output[at + 2].is_finite()) {
                            return nonfinite();
                        }
                    }
                }
                None => {
                    for (component, ((bi, bw, bc), (di, dw, dc))) in components.iter().enumerate() {
                        let (base_in, dir_in) = (&r.inputs[*bi as usize], &r.inputs[*di as usize]);
                        let (bw, bc, dw, dc) = (*bw as usize, *bc as usize, *dw as usize, *dc as usize);
                        for j in 0..length {
                            let at = (output_start + j) * width + component;
                            output[at] = dir_in[(b.start + j) * dw + dc] * scratch[j];
                            if !output[at].is_finite() {
                                return nonfinite();
                            }
                            output[at] = base_in[(b.start + j) * bw + bc] + output[at];
                            if !output[at].is_finite() {
                                return nonfinite();
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

// Noise3 under gain, bias and amplitude, in the same direct form.
fn block_scalar_streaming(
    r: &Fast,
    scratch: &mut [f64],
    b: &Blk,
    plan: &ScalarStreaming,
    base: usize,
    outs: &mut [Option<&mut [f64]>],
    planes: &mut [Vec<Option<&mut [f64]>>],
) -> K<()> {
    let bs = r.block_size;
    let (x, y) = (&r.columns[plan.x.0 as usize][plan.x.2 as usize], &r.columns[plan.y.0 as usize][plan.y.2 as usize]);
    let noise = &r.table;
    let length = b.length;
    let frequency = plan.frequency.map(|f| r.scalars[f]).unwrap_or(1.0);
    match &plan.z {
        Some((i, _, c)) => {
            let z = &r.columns[*i as usize][*c as usize];
            rays_math::noise::sample3_into(
                noise,
                0,
                length as i64,
                Some(x.offset + b.start as i64 * x.stride),
                Some(y.offset + b.start as i64 * y.stride),
                Some(z.offset + b.start as i64 * z.stride),
                None,
                Some(x.stride),
                Some(y.stride),
                Some(z.stride),
                Some(frequency),
                &x.data,
                &y.data,
                &z.data,
                scratch,
            )
            .map_err(noise_error)?;
        }
        None => {
            // z is the constant zero: a zero buffer stands for it.
            let zeros = vec![0.0f64; length.max(1)];
            rays_math::noise::sample3_into(
                noise,
                0,
                length as i64,
                Some(x.offset + b.start as i64 * x.stride),
                Some(y.offset + b.start as i64 * y.stride),
                Some(0),
                None,
                Some(x.stride),
                Some(y.stride),
                Some(1),
                Some(frequency),
                &x.data,
                &y.data,
                &zeros,
                scratch,
            )
            .map_err(noise_error)?;
        }
    }
    let _ = bs;
    let (gain, bias, amplitude) = (r.scalars[plan.gain], r.scalars[plan.bias], r.scalars[plan.amplitude]);
    for j in 0..length {
        scratch[j] *= gain;
        scratch[j] -= bias;
        scratch[j] *= amplitude;
        // Mul/Sub/Mul propagate every invalid intermediate to the final scale.
        if !scratch[j].is_finite() {
            return nonfinite();
        }
    }
    let output_start = b.start - base;
    for (k, components) in plan.outputs.iter().enumerate() {
        let width = components.len();
        for (component, ((bi, bw, bc), expression)) in components.iter().enumerate() {
            if r.planes {
                let col = &r.columns[*bi as usize][*bc as usize];
                for j in 0..length {
                    let v0 = column_at(col, b.start + j);
                    let v = if expression.is_some() { v0 + scratch[j] } else { v0 };
                    if !v.is_finite() {
                        return nonfinite();
                    }
                    if let Some(data) = &mut planes[k][component] {
                        data[output_start + j] = v;
                    }
                }
            } else {
                let base_in = &r.inputs[*bi as usize];
                let (bw, bc) = (*bw as usize, *bc as usize);
                let output = outs[k].as_deref_mut().expect("collected output");
                for j in 0..length {
                    let v0 = base_in[(b.start + j) * bw + bc];
                    let v = if expression.is_some() { v0 + scratch[j] } else { v0 };
                    if !v.is_finite() {
                        return nonfinite();
                    }
                    output[(output_start + j) * width + component] = v;
                }
            }
        }
    }
    Ok(())
}

// Select the narrower streaming loops only when every register belongs to the supported
// noise/scale/output shape. Other programs stay on the general block executor.
fn streaming_plan(t: &T) -> Option<Streaming> {
    if t.result != RegionResult::Collect || t.iteration != Iteration::Zip || !t.skip.is_empty() {
        return None;
    }
    let code = &t.code;
    let mut allowed: Vec<bool> =
        code.iter().map(|i| matches!(i, Instruction::Input(..) | Instruction::Const(_) | Instruction::Uniform(..) | Instruction::Frame(_))).collect();
    let input = |id: usize| -> Option<(i64, i64, i64)> {
        match &code[id] {
            Instruction::Input(s, w, c) => Some((*s, *w, *c)),
            _ => None,
        }
    };
    let scalar = |id: usize| -> Option<usize> {
        matches!(&code[id], Instruction::Const(_) | Instruction::Uniform(..) | Instruction::Frame(_)).then_some(id)
    };
    // an output `base + direction * scale`; the product is the later operand when both are products
    let mut shared: Option<usize> = None;
    let mut products = Vec::new();
    for output in t.outputs.iter() {
        let mut row = Vec::new();
        for register in output {
            let register = *register as usize;
            let Instruction::Binary(Binary::Add, a, b) = &code[register] else { return None };
            let (a, b) = (*a as usize, *b as usize);
            let (base, product) = if matches!(code[b], Instruction::Binary(Binary::Mul, ..)) { (a, b) } else { (b, a) };
            let Instruction::Binary(Binary::Mul, x, y) = &code[product] else { return None };
            let (x, y) = (*x as usize, *y as usize);
            let (direction, scale) = match (&code[x], &code[y]) {
                (Instruction::Input(..), _) => (input(x)?, y),
                (_, Instruction::Input(..)) => (input(y)?, x),
                _ => return None,
            };
            match shared {
                None => shared = Some(scale),
                Some(old) if old == scale => {}
                _ => return None,
            }
            allowed[register] = true;
            allowed[product] = true;
            row.push((input(base)?, direction));
        }
        products.push(row);
    }
    let scale = shared?;
    let (noise, amplitude) = match &code[scale] {
        Instruction::Binary(Binary::Mul, a, b) => {
            let (a, b) = (*a as usize, *b as usize);
            match (&code[a], &code[b]) {
                (Instruction::Noise3(..), _) => (a, scalar(b)?),
                (_, Instruction::Noise3(..)) => (b, scalar(a)?),
                _ => return None,
            }
        }
        _ => return None,
    };
    allowed[scale] = true;
    allowed[noise] = true;
    // the three coordinates: inputs, or inputs under one shared scalar factor
    let (x, y, z, frequency) = match &code[noise] {
        Instruction::Noise3(x, y, z, _, LiveInt::Fixed(1), LiveFloat::Fixed(_), LiveFloat::Fixed(_)) => {
            let scaled = |id: i64| -> Option<((i64, i64, i64), usize)> {
                match &code[id as usize] {
                    Instruction::Binary(Binary::Mul, a, b) => match (&code[*a as usize], &code[*b as usize]) {
                        (Instruction::Input(..), _) => Some((input(*a as usize)?, scalar(*b as usize)?)),
                        (_, Instruction::Input(..)) => Some((input(*b as usize)?, scalar(*a as usize)?)),
                        _ => None,
                    },
                    _ => None,
                }
            };
            match (scaled(*x), scaled(*y), scaled(*z)) {
                (Some((a, f)), Some((b, g)), Some((c, h))) if f == g && g == h => {
                    for id in [*x, *y, *z] {
                        allowed[id as usize] = true;
                    }
                    (a, b, c, Some(f))
                }
                _ => (input(*x as usize)?, input(*y as usize)?, input(*z as usize)?, None),
            }
        }
        _ => return None,
    };
    if !allowed.iter().all(|a| *a) {
        return None;
    }
    Some(Streaming { noise, amplitude, x, y, z, frequency, products })
}

fn scalar_streaming_plan(t: &T) -> Option<ScalarStreaming> {
    if t.result != RegionResult::Collect || t.iteration != Iteration::Zip || !t.skip.is_empty() {
        return None;
    }
    let code = &t.code;
    let mut allowed: Vec<bool> =
        code.iter().map(|i| matches!(i, Instruction::Input(..) | Instruction::Const(_) | Instruction::Uniform(..) | Instruction::Frame(_))).collect();
    let input = |id: usize| -> Option<(i64, i64, i64)> {
        match &code[id] {
            Instruction::Input(i, w, c) => Some((*i, *w, *c)),
            _ => None,
        }
    };
    let scalar = |id: usize| -> Option<usize> {
        matches!(&code[id], Instruction::Const(_) | Instruction::Uniform(..) | Instruction::Frame(_)).then_some(id)
    };
    // (input, scalar factor) of a possibly scaled input
    let scaled = |allowed: &mut Vec<bool>, id: usize| -> Option<((i64, i64, i64), Option<usize>)> {
        match &code[id] {
            Instruction::Binary(Binary::Mul, a, b) => {
                let (a, b) = (*a as usize, *b as usize);
                match (&code[a], &code[b]) {
                    (Instruction::Input(..), _) => {
                        allowed[id] = true;
                        Some((input(a)?, Some(scalar(b)?)))
                    }
                    (_, Instruction::Input(..)) => {
                        allowed[id] = true;
                        Some((input(b)?, Some(scalar(a)?)))
                    }
                    _ => None,
                }
            }
            Instruction::Input(..) => Some((input(id)?, None)),
            _ => None,
        }
    };
    let mut shared: Option<usize> = None;
    let mut outputs = Vec::new();
    for output in t.outputs.iter() {
        let mut row = Vec::new();
        for id in output {
            let id = *id as usize;
            match &code[id] {
                Instruction::Input(..) => row.push((input(id)?, None)),
                Instruction::Binary(Binary::Add, a, b) => {
                    let (a, b) = (*a as usize, *b as usize);
                    let (base, expression) = match (&code[a], &code[b]) {
                        (Instruction::Input(..), _) => (input(a)?, b),
                        (_, Instruction::Input(..)) => (input(b)?, a),
                        _ => return None,
                    };
                    match shared {
                        None => shared = Some(expression),
                        Some(old) if old == expression => {}
                        _ => return None,
                    }
                    allowed[id] = true;
                    row.push((base, Some(expression)));
                }
                _ => return None,
            }
        }
        outputs.push(row);
    }
    let root = shared?;
    let (transform, amplitude) = match &code[root] {
        Instruction::Binary(Binary::Mul, a, b) => {
            let (a, b) = (*a as usize, *b as usize);
            match (&code[a], &code[b]) {
                (Instruction::Binary(Binary::Sub, _, _), _) => (a, scalar(b)?),
                (_, Instruction::Binary(Binary::Sub, _, _)) => (b, scalar(a)?),
                _ => return None,
            }
        }
        _ => return None,
    };
    let (gain_reg, bias) = match &code[transform] {
        Instruction::Binary(Binary::Sub, a, b) => (*a as usize, scalar(*b as usize)?),
        _ => return None,
    };
    let (noise, gain) = match &code[gain_reg] {
        Instruction::Binary(Binary::Mul, a, b) => {
            let (a, b) = (*a as usize, *b as usize);
            match (&code[a], &code[b]) {
                (Instruction::Noise3(..), _) => (a, scalar(b)?),
                (_, Instruction::Noise3(..)) => (b, scalar(a)?),
                _ => return None,
            }
        }
        _ => return None,
    };
    allowed[root] = true;
    allowed[transform] = true;
    allowed[gain_reg] = true;
    allowed[noise] = true;
    let (x, y, z, frequency) = match &code[noise] {
        Instruction::Noise3(x, y, z, _, LiveInt::Fixed(1), LiveFloat::Fixed(_), LiveFloat::Fixed(_)) => {
            let (x, f) = scaled(&mut allowed, *x as usize)?;
            let (y, h) = scaled(&mut allowed, *y as usize)?;
            if f != h {
                return None;
            }
            let z = match &code[*z as usize] {
                Instruction::Const(z) if *z == 0.0 => None,
                _ => {
                    let (z, h) = scaled(&mut allowed, *z as usize)?;
                    if h != f {
                        return None;
                    }
                    Some(z)
                }
            };
            (x, y, z, f)
        }
        _ => return None,
    };
    if !allowed.iter().all(|a| *a) {
        return None;
    }
    Some(ScalarStreaming { noise, x, y, z, frequency, gain, bias, amplitude, outputs })
}

/// `reduce +` over one array of floats or vectors: the additions run in reference order, straight
/// from the source, with no registers.
pub fn ordered_add(t: &T) -> bool {
    let output = &t.outputs[0];
    let width = output.len();
    match &t.result {
        RegionResult::Accumulate(_, false)
            if (1..=4).contains(&width)
                && t.iteration == Iteration::Zip
                && t.sources.len() == 1
                && t.widths.len() == 1
                && t.widths[0] == width as i64
                && t.skip.is_empty()
                && t.code.len() == 3 * width
                && region::dependent_flags(t).len() == 3 * width =>
        {
            let dependent = region::dependent_flags(t);
            let w = width as i64;
            (0..width).all(|k| {
                t.code[k] == Instruction::Input(0, w, k as i64)
                    && !dependent[k]
                    && t.code[width + k] == Instruction::Accumulator(k as i64)
                    && dependent[width + k]
                    && t.code[2 * width + k] == Instruction::Binary(Binary::Add, w + k as i64, k as i64)
                    && dependent[2 * width + k]
                    && output[k] == (2 * width + k) as i64
            })
        }
        _ => false,
    }
}

/// The streaming plan of a Collect launch, with the columns and scalars its loops read; `None` when
/// the region or its inputs do not have a plan's exact shape.
fn fast_path(sh: &TS, fo: &Forcer<'_>) -> K<Option<Fast>> {
    let t = &sh.t;
    if sh.count == 0 {
        return Ok(None);
    }
    let plan = match streaming_plan(t) {
        Some(plan) => Plan::Noise(plan),
        None => match scalar_streaming_plan(t) {
            Some(plan) => Plan::Scalar(plan),
            None => return Ok(None),
        },
    };
    let mut columns = Vec::with_capacity(sh.inputs.len());
    let mut inputs = Vec::with_capacity(sh.inputs.len());
    for (i, value) in sh.inputs.iter().enumerate() {
        let width = t.widths[i] as usize;
        match (&sh.borrowed[i], value, width) {
            (Some(borrowed), _, _) => {
                columns.push(borrowed.clone());
                inputs.push(Arc::new(Vec::new()));
            }
            (None, Value::Floats(actual @ 1..=4, d), width) if *actual == width => {
                columns.push((0..width).map(|c| Column { data: d.clone(), offset: c as i64, stride: width as i64 }).collect());
                inputs.push(d.clone());
            }
            _ => return Ok(None),
        }
    }
    let mut scalars = vec![0.0; t.code.len()];
    for (slot, instruction) in t.code.iter().enumerate() {
        scalars[slot] = match instruction {
            Instruction::Const(x) => *x,
            Instruction::Uniform(i, c) => *sh.ucomps(*i as usize, Some(fo))?.get(*c as usize).ok_or(KErr::Unsupported)?,
            Instruction::Frame(_) => num_k(&sh.frame(slot)?)?,
            _ => 0.0,
        };
    }
    let noise = match &plan {
        Plan::Noise(plan) => plan.noise,
        Plan::Scalar(plan) => plan.noise,
    };
    let Instruction::Noise3(_, _, _, seed, ..) = &t.code[noise] else { unreachable!("a plan's noise register") };
    Ok(Some(Fast { plan, columns, inputs, scalars, table: op::noise_table(*seed), block_size: sh.block_size, planes: sh.planes }))
}

fn fast_chunk(sh: &TS, f: &Fast, chunk: usize, base: usize, outs: &mut [Option<&mut [f64]>], planes: &mut [Vec<Option<&mut [f64]>>]) -> K<()> {
    let bs = sh.block_size;
    let mut scratch = vec![0.0f64; if f.planes { 2 * bs } else { bs }];
    for block in chunk * sh.blocks_per_chunk..sh.blocks.min((chunk + 1) * sh.blocks_per_chunk) {
        E::interruptible(&mut || {})?;
        let start = block * bs;
        let b = Blk { start, length: sh.count.min(start + bs) - start };
        match &f.plan {
            Plan::Noise(plan) => block_streaming(f, &mut scratch, &b, plan, base, outs, planes)?,
            Plan::Scalar(plan) => block_scalar_streaming(f, &mut scratch, &b, plan, base, outs, planes)?,
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------------------------
// Running a region

/// Everything one `force` carries through its nested evaluations.
#[derive(Clone)]
struct Run {
    t: T,
    live: Live,
    state: State,
    elems: Option<Vec<(String, Value)>>,
    resolve: Option<Resolve>,
    measure: Option<Measure>,
    on_walk: Option<Arc<dyn Fn() + Send + Sync>>,
    strict: bool,
    column_source: Option<ColumnSource>,
    expected_count: Option<usize>,
    in_scope: bool,
}

fn run_state(run: &Run) -> Option<&State> {
    Some(&run.state)
}

fn seconds_since(measure: &Measure, started: f64) -> f64 {
    ((measure.clock)() - started).max(0.0)
}

fn float_value_of(width: usize, data: Vec<f64>) -> Value {
    Value::Floats(width, Arc::new(data))
}

fn scalar_value_of(width: usize, a: &[f64]) -> Value {
    match width {
        2 => Value::Vec2(a[0], a[1]),
        3 => Value::Vec3(a[0], a[1], a[2]),
        4 => Value::Vec4(a[0], a[1], a[2], a[3]),
        _ => Value::Float(a[0]),
    }
}

type PlaneOutputs<'a, 'b> = Option<&'a mut [Vec<Option<&'b mut [f64]>>]>;

// ---------------------------------------------------------------------------------------------
// General (typed) path: integers, text, tables, loops, random streams, ray queries.

fn components_k(v: &Value) -> K<Vec<f64>> {
    Ok(match v {
        Value::Vec2(x, y) => vec![*x, *y],
        Value::Vec3(x, y, z) => vec![*x, *y, *z],
        Value::Vec4(x, y, z, w) => vec![*x, *y, *z, *w],
        other => vec![value::num(other)?],
    })
}

fn concrete(v: &Value) -> bool {
    matches!(
        v,
        Value::Int(_) | Value::Text(_) | Value::Float(_) | Value::Bool(_) | Value::Vec2(..) | Value::Vec3(..) | Value::Vec4(..)
            | Value::Int_array(_) | Value::Text_array(_) | Value::Floats(1..=4, _)
    )
}

fn scalar_value(v: &Value) -> bool {
    matches!(v, Value::Int(_) | Value::Float(_) | Value::Bool(_) | Value::Vec2(..) | Value::Vec3(..) | Value::Vec4(..))
}

/// Characters of a lazily forced capture: forced once (on the main thread), then read by every
/// chunk.
struct UCell {
    value: OnceLock<K<Value>>,
    comps: OnceLock<K<Vec<f64>>>,
}

type Forcer<'a> = dyn Fn(&Value) -> Result<Value, Diagnostic> + 'a;

fn trace_field(field: i64, h: &ray_host::Hit) -> f64 {
    match field {
        0 => h.t,
        1 => h.prim as f64,
        2 => h.u,
        3 => h.v,
        4 => h.nx,
        5 => h.ny,
        6 => h.nz,
        7 => {
            if h.hit { 1.0 } else { 0.0 }
        }
        _ => h.instance as f64,
    }
}

struct TS {
    t: T,
    live: Live,
    inputs: Vec<Value>,
    borrowed: Vec<Option<Vec<Column>>>,
    ranges: Vec<Option<usize>>,
    lengths: Vec<usize>,
    strides: Vec<usize>,
    skip: Vec<usize>,
    count: usize,
    ucells: Vec<UCell>,
    prepared: Vec<OnceLock<Value>>,
    frames: Vec<OnceLock<K<Value>>>,
    accumulator: Mutex<Value>,
    initialized: AtomicBool,
    integer_outputs: Vec<bool>,
    text_outputs: Vec<bool>,
    carried: usize,
    gather: bool,
    cache_guards: bool,
    total_prefix: usize,
    collecting: bool,
    planes: bool,
    block_size: usize,
    blocks: usize,
    blocks_per_chunk: usize,
    dependent_flags: Vec<bool>,
    text_count: usize,
    contiguous: bool,
    ordered: bool,
    fast: Option<Fast>,
}

impl TS {
    fn skipped_before(&self, n: usize) -> usize {
        self.skip.partition_point(|s| *s < n)
    }
    fn uvalue(&self, i: usize, fo: Option<&Forcer<'_>>) -> K<Value> {
        if let Some(r) = self.ucells[i].value.get() {
            return r.clone();
        }
        match fo {
            Some(f) => {
                let made = f(&self.t.uniforms[i]).map_err(KErr::Diag);
                self.ucells[i].value.get_or_init(|| made).clone()
            }
            None => Err(KErr::Unsupported),
        }
    }
    fn ucomps(&self, i: usize, fo: Option<&Forcer<'_>>) -> K<Vec<f64>> {
        if let Some(r) = self.ucells[i].comps.get() {
            return r.clone();
        }
        let made = self.uvalue(i, fo).and_then(|v| components_k(&v));
        self.ucells[i].comps.get_or_init(|| made).clone()
    }
    fn frame(&self, slot: usize) -> K<Value> {
        self.frames[slot]
            .get_or_init(|| match Instruction::unguarded(&self.t.code[slot]) {
                Instruction::Int_frame(name) | Instruction::Frame(name) => frame_fact(name, &self.live),
                _ => Ok(Value::No_geo),
            })
            .clone()
    }
    fn prepare(&self, i: usize, ty: &Ty, fo: Option<&Forcer<'_>>) -> K<&Value> {
        if let Some(v) = self.prepared[i].get() {
            return Ok(v);
        }
        let value = self.uvalue(i, fo)?;
        if value::ty_of(&value) != *ty {
            return Err(fail("E_ARRAY_TYPE", "Kernel lookup table changed its packed element type."));
        }
        value::validate(&value)?;
        let _ = self.prepared[i].set(value);
        Ok(self.prepared[i].get().unwrap())
    }
    fn table(&self, register: i64) -> K<&Value> {
        match Instruction::unguarded(&self.t.code[register as usize]) {
            Instruction::Array_prepare(i, _) => self.prepared[*i as usize].get().ok_or_else(|| fail("E_PACKED_LAYOUT", "Array lookup needs its prepared table.")),
            _ => Err(fail("E_PACKED_LAYOUT", "Array lookup needs a table handle.")),
        }
    }
}

fn row(array: &Value, index: i64) -> K<usize> {
    if index < 0 || index as usize > MAX_ARRAY {
        return Err(fail("E_ARRAY_RANGE", "array/nth needs a non-negative whole length."));
    }
    if index as usize >= array_len_k(array)? {
        return Err(fail("E_ARRAY_RANGE", "Array index is outside its length."));
    }
    Ok(index as usize)
}

/// `finite(x)?` without building a `Result` inside a hot loop.
macro_rules! finite {
    ($x:expr) => {{
        let x = $x;
        if !x.is_finite() {
            return nonfinite();
        }
        x
    }};
}

/// `f[at + j] = op(f[a + j], f[b + j])` over `first..last`. `at`, `a` and `b` are register blocks,
/// equal or disjoint.
#[inline(always)]
fn binary_lanes(f: &mut [f64], at: usize, a: usize, b: usize, first: usize, last: usize, op: impl Fn(f64, f64) -> f64) -> K<()> {
    if last - first < 16 {
        // a fold's lane at a time: the split costs more than it saves
        for j in first..last {
            f[at + j] = finite!(op(f[a + j], f[b + j]));
        }
        return Ok(());
    }
    wide_lanes(f, at, a, b, first, last, op)
}

/// `binary_lanes` over a block, then one finite check: a loop with no early exit or bounds check
/// vectorises. Not inlined: in `texec` its copies slow the integer instructions' loops.
#[inline(never)]
fn wide_lanes(f: &mut [f64], at: usize, a: usize, b: usize, first: usize, last: usize, op: impl Fn(f64, f64) -> f64) -> K<()> {
    let n = last - first;
    let (low, high) = f.split_at_mut(at + first);
    let (out, rest) = high.split_at_mut(n);
    // a source block below the target is in `low`, one above it in `rest`
    let source = |r: usize| -> Option<&[f64]> {
        if r < at { Some(&low[r + first..r + last]) } else if r > at { Some(&rest[r - at - n..][..n]) } else { None }
    };
    match (source(a), source(b)) {
        (Some(x), Some(y)) => out.iter_mut().zip(x.iter().zip(y)).for_each(|(o, (x, y))| *o = op(*x, *y)),
        (None, Some(y)) => out.iter_mut().zip(y).for_each(|(o, y)| *o = op(*o, *y)),
        (Some(x), None) => out.iter_mut().zip(x).for_each(|(o, x)| *o = op(*x, *o)),
        (None, None) => out.iter_mut().for_each(|o| *o = op(*o, *o)),
    }
    if out.iter().all(|x| x.is_finite()) { Ok(()) } else { nonfinite() }
}

#[inline(always)]
fn finite(x: f64) -> K<f64> {
    if x.is_finite() { Ok(x) } else { nonfinite() }
}

// whether a leading instruction is total: it cannot fail or read lazily
fn typed_total(sh: &TS, instruction: &Instruction) -> bool {
    use Instruction::*;
    match instruction {
        Const(_) | Int_const(_) | Text_const(_) | Int_abs(_) | Int_compare(..) | Int_select(..) | Text_select(..) => true,
        Int_input(i) => sh.ranges[*i as usize].is_some() || matches!(sh.inputs[*i as usize], Value::Int_array(_)),
        Text_input(i) => matches!(sh.inputs[*i as usize], Value::Text_array(_)),
        Int_binary(Integer::Iadd | Integer::Isub | Integer::Imul | Integer::Iand | Integer::Ior | Integer::Ixor, _, _) => true,
        Guarded(_, body) => typed_total(sh, body),
        _ => false,
    }
}

/// One guard list's selected spans over `first..last`, valid in the block stamped `stamp`.
struct GuardSpans {
    guards: Vec<(i64, bool)>,
    first: usize,
    last: usize,
    stamp: usize,
    spans: Vec<usize>,
}

struct TC {
    integer: Vec<i64>,
    floats: Vec<f64>,
    texts: Vec<Vec<u8>>,
    traced: Vec<Option<(i64, Vec<f64>, ray_host::Hit)>>,
    next_float: Vec<f64>,
    next_int: Vec<i64>,
    next_text: Vec<Vec<u8>>,
    tables: HashMap<i64, Arc<rays_math::noise::T>>,
    scratch: rays_math::noise::Fbm3Scratch,
    indices: Vec<usize>,
    start: usize,
    guard_spans: Option<Vec<GuardSpans>>,
    /// the current block's stamp in `guard_spans` (0: none)
    stamp: usize,
    /// A float fold's running value, chunk-local: the chunk's last lane hands it back to the launch.
    acc: [f64; 4],
}

impl TC {
    // inlined: the guarded span scan calls these per lane (select kernel -5% instructions)
    #[inline(always)]
    fn numeric(&self, regs: &region::Registers, bs: usize, r: usize, j: usize) -> f64 {
        let at = r * bs + j;
        if regs.integer_slots[r] { self.integer[at] as f64 } else { self.floats[at] }
    }
    #[inline(always)]
    fn selected(&self, regs: &region::Registers, bs: usize, j: usize, guards: &[(i64, bool)]) -> bool {
        guards.iter().all(|(r, yes)| (self.numeric(regs, bs, *r as usize, j) != 0.0) == *yes)
    }
    fn copy_loop(&mut self, regs: &region::Registers, bs: usize, source: usize, target: usize, lane: usize) {
        if regs.text_slots[target] >= 0 {
            self.texts[regs.text_slots[target] as usize * bs + lane] = self.texts[regs.text_slots[source] as usize * bs + lane].clone();
        } else if regs.integer_slots[target] {
            self.integer[target * bs + lane] = self.integer[source * bs + lane];
        } else {
            self.floats[target * bs + lane] = self.floats[source * bs + lane];
        }
    }
}

fn texec_range(
    sh: &TS,
    regs: &region::Registers,
    tc: &mut TC,
    fo: Option<&Forcer<'_>>,
    dependent: Option<bool>,
    first_slot: usize,
    last_slot: usize,
    first: usize,
    last: usize,
) -> K<()> {
    let wanted = |slot: usize| dependent.map_or(true, |d| sh.dependent_flags[slot] == d);
    if sh.carried == 0 {
        for slot in first_slot..last_slot {
            if wanted(slot) {
                texec(sh, regs, tc, fo, slot, &regs.run_code[slot], first, last)?;
            }
        }
    } else {
        let mut slot = first_slot;
        while slot < last_slot {
            let instruction = &regs.run_code[slot];
            if wanted(slot) {
                texec(sh, regs, tc, fo, slot, instruction, first, last)?;
            }
            slot = match Instruction::unguarded(instruction) {
                Instruction::Loop_begin(l) => l.finish as usize + 1,
                _ => slot + 1,
            };
        }
    }
    Ok(())
}

fn execute_loop(
    sh: &TS,
    regs: &region::Registers,
    tc: &mut TC,
    fo: Option<&Forcer<'_>>,
    header: usize,
    lp: &Loop,
    first: usize,
    last: usize,
) -> K<()> {
    let bs = sh.block_size;
    let (next, stop) = match Instruction::unguarded(&regs.run_code[lp.finish as usize]) {
        Instruction::Loop_end(_, next, stop) => (next.clone(), *stop),
        _ => unreachable!("loop without its end"),
    };
    let count = lp.initial.len();
    // Lanes step together: each iteration runs the body over the spans of lanes still looping, each
    // lane's values in its own order, so they equal a lane-at-a-time run's; a block that raises is
    // redone lane-major by its caller (the first error in lane order).
    let lanes = last - first;
    let mut lows = vec![0i64; lanes];
    let mut lengths = vec![0usize; lanes];
    for lane in first..last {
        let lo = tc.integer[lp.first as usize * bs + lane];
        let hi = tc.integer[lp.last as usize * bs + lane];
        lows[lane - first] = lo;
        let limit = lp.limit.map(|l| l as usize).unwrap_or(usize::MAX >> 1);
        lengths[lane - first] = match op::range_length(Some(limit), lo, hi) {
            Ok(n) => n,
            Err(d) => return Err(KErr::Diag(Diagnostic { span: Some(lp.span.clone()), ..d })),
        };
        for component in 0..count {
            tc.copy_loop(regs, bs, lp.initial[component] as usize, header + 1 + component, lane);
        }
    }
    // until every lane has run its count or stopped
    let mut iteration = 0usize;
    while lengths.iter().any(|length| iteration < *length) {
        let active = |lengths: &Vec<usize>, lane: usize| iteration < lengths[lane - first];
        let mut lane = first;
        while lane < last {
            while lane < last && !active(&lengths, lane) {
                lane += 1;
            }
            let span = lane;
            while lane < last && active(&lengths, lane) {
                tc.integer[header * bs + lane] = lows[lane - first] + iteration as i64;
                lane += 1;
            }
            if span < lane {
                texec_range(sh, regs, tc, fo, None, header + 1 + count, lp.finish as usize, span, lane)?;
            }
        }
        for lane in first..last {
            if active(&lengths, lane) {
                // a lane that stops commits these next values and leaves the loop
                if let Some(slot) = stop {
                    if tc.floats[slot as usize * bs + lane] != 0.0 {
                        lengths[lane - first] = 0;
                    }
                }
                for component in 0..count {
                    let target = header + 1 + component;
                    let value = next[component] as usize;
                    if regs.text_slots[target] >= 0 {
                        tc.next_text[component] = tc.texts[regs.text_slots[value] as usize * bs + lane].clone();
                    } else if regs.integer_slots[target] {
                        tc.next_int[component] = tc.integer[value * bs + lane];
                    } else {
                        tc.next_float[component] = tc.floats[value * bs + lane];
                    }
                }
                for component in 0..count {
                    let target = header + 1 + component;
                    if regs.text_slots[target] >= 0 {
                        tc.texts[regs.text_slots[target] as usize * bs + lane] = tc.next_text[component].clone();
                    } else if regs.integer_slots[target] {
                        tc.integer[target * bs + lane] = tc.next_int[component];
                    } else {
                        tc.floats[target * bs + lane] = tc.next_float[component];
                    }
                }
            }
        }
        iteration += 1;
    }
    Ok(())
}

fn texec(
    sh: &TS,
    regs: &region::Registers,
    tc: &mut TC,
    fo: Option<&Forcer<'_>>,
    slot: usize,
    instruction: &Instruction,
    first: usize,
    last: usize,
) -> K<()> {
    let bs = sh.block_size;
    let t = &sh.t;
    let place = regs.defs[slot] as usize;
    let at = place * bs;
    let banks = &regs.integer_slots;
    let toff = |p: usize| regs.text_slots[p] as usize * bs;
    let source_index = |tc: &TC, j: usize, i: usize| -> usize {
        let index = if sh.contiguous { tc.start + j } else { tc.indices[j] };
        if t.iteration == Iteration::Zip { index } else { index / sh.strides[i] % sh.lengths[i] }
    };
    match instruction {
        Instruction::Loop_begin(lp) => execute_loop(sh, regs, tc, fo, slot, lp, first, last)?,
        Instruction::Loop_result(lp, component, _) => {
            for lane in first..last {
                tc.copy_loop(regs, bs, (*lp + 1 + *component) as usize, slot, lane);
            }
        }
        Instruction::Loop_carry(..) | Instruction::Loop_end(..) => unreachable!("loop bookkeeping runs inside its loop"),
        Instruction::Text_const(v) => {
            let o = toff(place);
            for j in first..last {
                tc.texts[o + j] = v.clone();
            }
        }
        Instruction::Text_uniform(i) => {
            if first < last {
                let v = match sh.uvalue(*i as usize, fo)? {
                    Value::Text(text) => text,
                    _ => return Err(fail("E_ARRAY_TYPE", "Kernel input changed its packed element type.")),
                };
                let o = toff(place);
                for j in first..last {
                    tc.texts[o + j] = v.clone();
                }
            }
        }
        Instruction::Text_input(i) => {
            let i = *i as usize;
            let data = match &sh.inputs[i] {
                Value::Text_array(data) => data,
                _ => return Err(fail("E_ARRAY_TYPE", "Kernel input changed its packed element type.")),
            };
            let o = toff(place);
            for j in first..last {
                tc.texts[o + j] = data[source_index(tc, j, i)].to_vec();
            }
        }
        Instruction::Text_gather(array, index) => {
            let array = sh.table(*array)?;
            let data = match array {
                Value::Text_array(data) => data,
                _ => unreachable!("text table"),
            };
            let o = toff(place);
            for j in first..last {
                let k = row(array, tc.integer[*index as usize * bs + j])?;
                tc.texts[o + j] = data[k].to_vec();
            }
        }
        Instruction::Text_select(condition, yes, no) => {
            let o = toff(place);
            for j in first..last {
                let register = if tc.numeric(regs, bs, *condition as usize, j) != 0.0 { *yes } else { *no };
                tc.texts[o + j] = tc.texts[toff(register as usize) + j].clone();
            }
        }
        Instruction::Guarded(guards, body) if last == first + 1 => {
            // Ordered gather execution visits one lane; bypass the span scanner without inspecting
            // any later lane.
            if tc.selected(regs, bs, first, guards) {
                texec(sh, regs, tc, fo, slot, body, first, last)?;
            }
        }
        Instruction::Guarded(guards, body) => {
            // Publish spans after their first execution: scanning ahead of a failing body would
            // change lazy predicate evaluation. Entries and span buffers live for the chunk (a
            // stamp marks the current block): per-block keys and copies contended in the
            // allocator at 8 domains.
            let stamp = tc.stamp;
            let found = tc.guard_spans.as_ref().and_then(|c| c.iter().position(|e| e.guards == *guards));
            let hit = found.filter(|&k| matches!(&tc.guard_spans, Some(c) if c[k].stamp == stamp && c[k].first == first && c[k].last == last));
            if let Some(k) = hit {
                let entry = &mut tc.guard_spans.as_mut().expect("guard cache")[k];
                // taken (and unstamped) while its body runs, so a nested equal list cannot see it empty
                entry.stamp = 0;
                let spans = std::mem::take(&mut entry.spans);
                for i in 0..spans.len() / 2 {
                    texec(sh, regs, tc, fo, slot, body, spans[2 * i], spans[2 * i + 1])?;
                }
                if let Some(c) = &mut tc.guard_spans {
                    // a nested equal list may have reused the entry meanwhile
                    (c[k].first, c[k].last, c[k].stamp, c[k].spans) = (first, last, stamp, spans);
                }
            } else {
                let remember = match &tc.guard_spans {
                    Some(c) => found.is_some() || c.len() < t.code.len(),
                    None => false,
                };
                let mut saved = match (found, &mut tc.guard_spans) {
                    (Some(k), Some(c)) => {
                        c[k].stamp = 0;
                        std::mem::take(&mut c[k].spans)
                    }
                    _ => Vec::new(),
                };
                saved.clear();
                let mut next = first;
                while next < last {
                    while next < last && !tc.selected(regs, bs, next, guards) {
                        next += 1;
                    }
                    let begin = next;
                    while next < last && tc.selected(regs, bs, next, guards) {
                        next += 1;
                    }
                    if begin < next {
                        texec(sh, regs, tc, fo, slot, body, begin, next)?;
                        if remember {
                            saved.push(begin);
                            saved.push(next);
                        }
                    }
                }
                if let (true, Some(c)) = (remember, &mut tc.guard_spans) {
                    let entry = GuardSpans { guards: Vec::new(), first, last, stamp, spans: saved };
                    match found {
                        Some(k) => c[k] = GuardSpans { guards: std::mem::take(&mut c[k].guards), ..entry },
                        None => c.push(GuardSpans { guards: guards.clone(), ..entry }),
                    }
                }
            }
        }
        Instruction::Raise(_, code, message, cause) => {
            if first < last {
                return Err(KErr::Diag(rays_diagnostic::error(code.clone(), message.clone(), None, None, cause.clone(), Vec::new(), Vec::new())));
            }
        }
        Instruction::Array_prepare(i, ty) => {
            if first < last {
                sh.prepare(*i as usize, ty, fo)?;
            }
        }
        Instruction::Int_gather(array, index) => {
            let array = sh.table(*array)?;
            match array {
                Value::Int_array(data) => {
                    for j in first..last {
                        let k = row(array, tc.integer[*index as usize * bs + j])?;
                        tc.integer[at + j] = data[k];
                    }
                }
                _ => return Err(fail("E_ARRAY_TYPE", "Integer lookup needs integer storage.")),
            }
        }
        Instruction::Gather(array, index, width, component) => {
            let array = sh.table(*array)?;
            let data = match (array, *width) {
                (Value::Floats(actual @ 1..=4, d), width) if *actual as i64 == width => d,
                _ => return Err(fail("E_ARRAY_TYPE", "Float lookup changed its packed element width.")),
            };
            for j in first..last {
                let k = row(array, tc.integer[*index as usize * bs + j])?;
                tc.floats[at + j] = finite!(data[k * *width as usize + *component as usize]);
            }
        }
        Instruction::Int_const(n) => tc.integer[at + first..at + last].fill(*n),
        // A constant produces no new value: like every other target (finite_checks), its consumers check it.
        Instruction::Const(x) => tc.floats[at + first..at + last].fill(*x),
        Instruction::Int_input(i) => {
            let i = *i as usize;
            if sh.contiguous {
                match (&sh.ranges[i], &sh.inputs[i]) {
                    (Some(_), _) => {
                        for j in first..last {
                            tc.integer[at + j] = (tc.start + j) as i64;
                        }
                    }
                    (None, Value::Int_array(data)) => {
                        tc.integer[at + first..at + last].copy_from_slice(&data[tc.start + first..tc.start + last]);
                    }
                    (None, value) => {
                        for j in first..last {
                            tc.integer[at + j] = match value::array_get(value, (tc.start + j) as i64)? {
                                Value::Int(n) => n,
                                _ => return Err(fail("E_ARRAY_TYPE", "Kernel input changed its packed element type.")),
                            };
                        }
                    }
                }
            } else {
                for j in first..last {
                    let s = source_index(tc, j, i);
                    tc.integer[at + j] = match (&sh.ranges[i], &sh.inputs[i]) {
                        (Some(_), _) => s as i64,
                        (None, Value::Int_array(data)) => data[s],
                        (None, value) => match value::array_get(value, s as i64)? {
                            Value::Int(n) => n,
                            _ => return Err(fail("E_ARRAY_TYPE", "Kernel input changed its packed element type.")),
                        },
                    };
                }
            }
        }
        Instruction::Input(i, width, component) => {
            let (i, width, component) = (*i as usize, *width as usize, *component as usize);
            if let Some(columns) = &sh.borrowed[i] {
                let column = &columns[component];
                for j in first..last {
                    let x = column_at(column, source_index(tc, j, i));
                    if !x.is_finite() {
                        return nonfinite();
                    }
                    tc.floats[at + j] = x;
                }
            } else {
                let direct = match (&sh.inputs[i], width) {
                    (Value::Floats(actual @ 1..=4, d), width) if *actual == width => Some(d),
                    _ => None,
                };
                match direct {
                    Some(data) if sh.contiguous && width == 1 => {
                        let lanes = &mut tc.floats[at + first..at + last];
                        lanes.copy_from_slice(&data[tc.start + first..tc.start + last]);
                        if !lanes.iter().all(|x| x.is_finite()) {
                            return nonfinite();
                        }
                    }
                    Some(data) if sh.contiguous => {
                        for j in first..last {
                            tc.floats[at + j] = finite!(data[(tc.start + j) * width + component]);
                        }
                    }
                    Some(data) => {
                        for j in first..last {
                            let x = data[source_index(tc, j, i) * width + component];
                            if !x.is_finite() {
                                return nonfinite();
                            }
                            tc.floats[at + j] = x;
                        }
                    }
                    None => {
                        for j in first..last {
                            let element = value::array_get(&sh.inputs[i], source_index(tc, j, i) as i64)?;
                            tc.floats[at + j] = finite!(components_k(&element)?[component]);
                        }
                    }
                }
            }
        }
        Instruction::Int_uniform(i) => {
            if first < last {
                match sh.uvalue(*i as usize, fo)? {
                    Value::Int(n) => tc.integer[at + first..at + last].fill(n),
                    _ => return Err(fail("E_ARRAY_TYPE", "Kernel uniform changed its integer type.")),
                }
            }
        }
        Instruction::Int_frame(_) => {
            if first < last {
                match sh.frame(slot)? {
                    Value::Int(n) => tc.integer[at + first..at + last].fill(n),
                    _ => return Err(KErr::Unsupported),
                }
            }
        }
        Instruction::Uniform(i, c) => {
            if first < last {
                let comps = sh.ucomps(*i as usize, fo)?;
                let x = finite!(*comps.get(*c as usize).ok_or(KErr::Unsupported)?);
                tc.floats[at + first..at + last].fill(x);
            }
        }
        Instruction::Frame(_) => {
            if first < last {
                let x = finite!(num_k(&sh.frame(slot)?)?);
                tc.floats[at + first..at + last].fill(x);
            }
        }
        Instruction::Int_accumulator => match &*sh.accumulator.lock().unwrap() {
            Value::Int(n) => tc.integer[at + first..at + last].fill(*n),
            _ => unreachable!("integer accumulator"),
        },
        Instruction::Accumulator(c) => {
            let x = finite!(tc.acc[*c as usize]);
            tc.floats[at + first..at + last].fill(x);
        }
        Instruction::Int_binary(operation, a, b) => {
            let (a, b) = (*a as usize * bs, *b as usize * bs);
            for j in first..last {
                tc.integer[at + j] = packed_ops::integer_apply(*operation, tc.integer[a + j], tc.integer[b + j])?;
            }
        }
        Instruction::Int_compare(operation, a, b) => {
            let (a, b) = (*a as usize * bs, *b as usize * bs);
            for j in first..last {
                tc.floats[at + j] = if packed_ops::comparison_apply(*operation, tc.integer[a + j], tc.integer[b + j]) { 1.0 } else { 0.0 };
            }
        }
        Instruction::Int_abs(a) => {
            let a = *a as usize * bs;
            for j in first..last {
                tc.integer[at + j] = tc.integer[a + j].wrapping_abs();
            }
        }
        Instruction::Convert(Conversion::ToFloat, a) => {
            for j in first..last {
                tc.floats[at + j] = finite!(tc.numeric(regs, bs, *a as usize, j));
            }
        }
        Instruction::Convert(operation, a) => {
            for j in first..last {
                tc.integer[at + j] = convert_int(operation, tc.numeric(regs, bs, *a as usize, j))?;
            }
        }
        Instruction::Int_select(c, a, b) => {
            let (a, b) = (*a as usize * bs, *b as usize * bs);
            for j in first..last {
                let chosen = if tc.numeric(regs, bs, *c as usize, j) != 0.0 { a } else { b };
                tc.integer[at + j] = tc.integer[chosen + j];
            }
        }
        // Registers hold finite values: a selected one needs no second check.
        Instruction::Select(c, a, b) if !banks[*c as usize] && !banks[*a as usize] && !banks[*b as usize] => {
            let (c, a, b) = (*c as usize * bs, *a as usize * bs, *b as usize * bs);
            for j in first..last {
                tc.floats[at + j] = tc.floats[(if tc.floats[c + j] != 0.0 { a } else { b }) + j];
            }
        }
        Instruction::Select(c, a, b) => {
            for j in first..last {
                let chosen = if tc.numeric(regs, bs, *c as usize, j) != 0.0 { *a } else { *b };
                tc.floats[at + j] = finite!(tc.numeric(regs, bs, chosen as usize, j));
            }
        }
        Instruction::Binary(operation, a, b) if !banks[*a as usize] && !banks[*b as usize] => {
            let (a, b) = (*a as usize * bs, *b as usize * bs);
            let compare = matches!(
                operation,
                Binary::Lt | Binary::Le | Binary::Gt | Binary::Ge | Binary::Eq | Binary::And | Binary::Or
            );
            let f = &mut tc.floats;
            match operation {
                Binary::Add => binary_lanes(f, at, a, b, first, last, |x, y| x + y)?,
                Binary::Sub => binary_lanes(f, at, a, b, first, last, |x, y| x - y)?,
                Binary::Mul => binary_lanes(f, at, a, b, first, last, |x, y| x * y)?,
                Binary::Div => binary_lanes(f, at, a, b, first, last, |x, y| if y == 0.0 { 0.0 } else { x / y })?,
                _ if compare => {
                    for j in first..last {
                        f[at + j] = packed_ops::binary_apply(*operation, f[a + j], f[b + j]);
                    }
                }
                _ => binary_lanes(f, at, a, b, first, last, |x, y| packed_ops::binary_apply(*operation, x, y))?,
            }
        }
        Instruction::Fma(a, b, c) if !banks[*a as usize] && !banks[*b as usize] && !banks[*c as usize] => {
            let (a, b, c) = (*a as usize * bs, *b as usize * bs, *c as usize * bs);
            for j in first..last {
                tc.floats[at + j] = finite!(rays_math::fma::fma(tc.floats[a + j], tc.floats[b + j], tc.floats[c + j]));
            }
        }
        Instruction::Checked_mul(p, a, b) if !banks[*a as usize] && !banks[*b as usize] => {
            let (a, b) = (*a as usize * bs, *b as usize * bs);
            for j in first..last {
                let (x, y) = (finite(tc.floats[a + j])?, finite(tc.floats[b + j])?);
                tc.floats[at + j] = checked_projection(*p, std::hint::black_box(x * y));
            }
        }
        Instruction::Checked_fma(p, a, b, c) if !banks[*a as usize] && !banks[*b as usize] && !banks[*c as usize] => {
            let (a, b, c) = (*a as usize * bs, *b as usize * bs, *c as usize * bs);
            for j in first..last {
                let (x, y, z) = (finite(tc.floats[a + j])?, finite(tc.floats[b + j])?, finite(tc.floats[c + j])?);
                tc.floats[at + j] = checked_projection(*p, rays_math::fma::fma(x, y, z));
            }
        }
        Instruction::Fma(a, b, c) => {
            for j in first..last {
                let v = rays_math::fma::fma(
                    tc.numeric(regs, bs, *a as usize, j),
                    tc.numeric(regs, bs, *b as usize, j),
                    tc.numeric(regs, bs, *c as usize, j),
                );
                tc.floats[at + j] = finite!(v);
            }
        }
        Instruction::Checked_mul(p, a, b) => {
            for j in first..last {
                let x = finite!(tc.numeric(regs, bs, *a as usize, j));
                let y = finite!(tc.numeric(regs, bs, *b as usize, j));
                tc.floats[at + j] = checked_projection(*p, std::hint::black_box(x * y));
            }
        }
        Instruction::Checked_fma(p, a, b, c) => {
            for j in first..last {
                let x = finite!(tc.numeric(regs, bs, *a as usize, j));
                let y = finite!(tc.numeric(regs, bs, *b as usize, j));
                let z = finite!(tc.numeric(regs, bs, *c as usize, j));
                tc.floats[at + j] = checked_projection(*p, rays_math::fma::fma(x, y, z));
            }
        }
        Instruction::Binary(operation, a, b) => {
            for j in first..last {
                let v = packed_ops::binary_apply(*operation, tc.numeric(regs, bs, *a as usize, j), tc.numeric(regs, bs, *b as usize, j));
                tc.floats[at + j] = finite!(v);
            }
        }
        Instruction::Trace(field, r) => {
            for j in first..last {
                let id = tc.numeric(regs, bs, r[0] as usize, j) as i64;
                let q: Vec<f64> = (0..9).map(|k| tc.numeric(regs, bs, r[k + 1] as usize, j)).collect();
                let hit = match &tc.traced[j] {
                    Some((i, k, h)) if *i == id && *k == q => *h,
                    _ => {
                        let h = ray_host::query(id, &q)?;
                        tc.traced[j] = Some((id, q, h));
                        h
                    }
                };
                tc.floats[at + j] = finite!(trace_field(*field, &hit));
            }
        }
        Instruction::Stage(..) => return Err(stage_refused()),
        Instruction::Rand_at(seed, index) => {
            let (seed, index) = (*seed as usize * bs, *index as usize * bs);
            for j in first..last {
                tc.floats[at + j] = packed_ops::rand_at(tc.integer[seed + j], tc.integer[index + j]);
            }
        }
        Instruction::Unary(operation, a) if !banks[*a as usize] => {
            let a = *a as usize * bs;
            let f = &mut tc.floats;
            match operation {
                // total over finite values
                Unary::Sqrt | Unary::Abs | Unary::Not => {
                    for j in first..last {
                        f[at + j] = packed_ops::unary_apply(*operation, f[a + j]);
                    }
                }
                _ => {
                    for j in first..last {
                        f[at + j] = finite!(packed_ops::unary_apply(*operation, f[a + j]));
                    }
                }
            }
        }
        Instruction::Unary(operation, a) => {
            for j in first..last {
                let v = packed_ops::unary_apply(*operation, tc.numeric(regs, bs, *a as usize, j));
                tc.floats[at + j] = finite!(v);
            }
        }
        Instruction::Noise3(x, y, z, seed, octaves, lacunarity, gain) => {
            let noise = tc.tables.entry(*seed).or_insert_with(|| op::noise_table(*seed)).clone();
            let live = live_fractal(*lacunarity, *gain);
            if matches!(octaves, LiveInt::Fixed(1)) && !banks[*x as usize] && !banks[*y as usize] && !banks[*z as usize] {
                if live {
                    let value = |reg: i64, j: usize| tc.floats[reg as usize * bs + j];
                    for j in first..last {
                        op::check_noise_fractal(fractal_at(&value, *lacunarity, j), fractal_at(&value, *gain, j))?;
                    }
                }
                let offset = |register: &i64| Some((*register as usize * bs) as i64);
                rays_math::noise::sample3_in_place(&noise, first as i64, last as i64, offset(x), offset(y), offset(z), Some(at as i64), None, None, None, None, &mut tc.floats)
                    .map_err(noise_error)?;
                return Ok(());
            }
            for j in first..last {
                let (lac, gn) = {
                    let value = |reg: i64, j: usize| tc.numeric(regs, bs, reg as usize, j);
                    (fractal_at(&value, *lacunarity, j), fractal_at(&value, *gain, j))
                };
                if live {
                    op::check_noise_fractal(lac, gn)?;
                }
                let octaves = match octaves {
                    LiveInt::Fixed(n) => *n,
                    LiveInt::Live(r) => {
                        let n = tc.integer[*r as usize * bs + j];
                        op::check_noise_octaves(n)?;
                        n
                    }
                };
                let (px, py, pz) =
                    (tc.numeric(regs, bs, *x as usize, j), tc.numeric(regs, bs, *y as usize, j), tc.numeric(regs, bs, *z as usize, j));
                let v = rays_math::noise::fbm3_with_scratch(&mut tc.scratch, &noise, octaves, lac, gn, px, py, pz);
                tc.floats[at + j] = finite!(v);
            }
        }
    }
    Ok(())
}

fn integer_range(residual: ResidualId, term: &Term) -> Option<(ResidualId, Term)> {
    let arena = term.arena();
    match term.node {
        Node::RefBinding(id) if arena.name_ids(arena.ref_binding(id).fields).is_empty() => {
            match E::residual::residual_binding(residual, term.name(arena.ref_binding(id).name)) {
                Some(Value::Residual(nested)) => {
                    let term = E::residual::residual_term(nested)?;
                    integer_range(nested, &term)
                }
                _ => None,
            }
        }
        Node::Op(id) => {
            let op = arena.op(id);
            if term.name(op.op) != "array/range" || !arena.skip(op.skip).is_empty() || !is_array_of_int(term) { return None; }
            let args = arena.fields(op.args);
            let kind = args.iter().find(|f| term.name(f.name) == "type")?;
            let count = args.iter().find(|f| term.name(f.name) == "count")?;
            let bytes = match arena.row(kind.term).node {
                Node::Text(id) => arena.bytes(id),
                Node::Lit(id) => match arena.literal(id) {
                    flow_check::term_arena::Literal::Text(id) => arena.bytes(id),
                    _ => return None,
                },
                _ => return None,
            };
            (bytes == b"int").then(|| (residual, term.child(count.term)))
        }
        _ => None,
    }
}

fn cached_ty(sh: &TS, i: i64, ty: &Ty) -> bool {
    matches!(sh.prepared[i as usize].get(), Some(v) if value::ty_of(v) == *ty)
}

fn cached_table(sh: &TS, register: i64) -> Option<&Value> {
    match Instruction::unguarded(&sh.t.code[register as usize]) {
        Instruction::Array_prepare(i, ty) if cached_ty(sh, *i, ty) => sh.prepared[*i as usize].get(),
        _ => None,
    }
}

fn valid_indices(sh: &TS, regs: &region::Registers, tc: &TC, length: usize, guards: &[(i64, bool)], index: i64, count: usize) -> bool {
    let bs = sh.block_size;
    let mut j = 0;
    while j < length
        && (!tc.selected(regs, bs, j, guards) || {
            let index = tc.integer[index as usize * bs + j];
            index >= 0 && index as usize <= MAX_ARRAY && (index as usize) < count
        })
    {
        j += 1;
    }
    j == length
}

// whether an instruction may run before the lane-major redo: its table is already published and
// every selected index is in bounds
fn safe(sh: &TS, regs: &region::Registers, tc: &TC, length: usize, guards: Vec<(i64, bool)>, instruction: &Instruction) -> bool {
    match instruction {
        Instruction::Guarded(more, body) => {
            let mut all = guards;
            all.extend(more.iter().copied());
            safe(sh, regs, tc, length, all, body)
        }
        Instruction::Array_prepare(i, ty) => cached_ty(sh, *i, ty),
        Instruction::Text_gather(array, index) => match cached_table(sh, *array) {
            Some(Value::Text_array(data)) => valid_indices(sh, regs, tc, length, &guards, *index, data.len()),
            _ => false,
        },
        Instruction::Int_gather(array, index) => match cached_table(sh, *array) {
            Some(Value::Int_array(data)) => valid_indices(sh, regs, tc, length, &guards, *index, data.len()),
            _ => false,
        },
        Instruction::Gather(array, index, width, _) => match (cached_table(sh, *array), *width) {
            (Some(Value::Floats(actual @ 1..=4, data)), want) if *actual as i64 == want => {
                data.len() % *width as usize == 0 && valid_indices(sh, regs, tc, length, &guards, *index, data.len() / *width as usize)
            }
            _ => false,
        },
        body => typed_total(sh, body),
    }
}

/// One chunk of the general path: its blocks' instructions, then their results stored.
fn typed_chunk(
    sh: &TS,
    regs: &region::Registers,
    chunk: usize,
    fo: Option<&Forcer<'_>>,
    base: usize,
    fouts: &mut [Option<&mut [f64]>],
    iouts: &mut [Option<&mut [i64]>],
    touts: &mut [Option<&mut [Vec<u8>]>],
    planes: &mut [Vec<Option<&mut [f64]>>],
) -> K<()> {
    if sh.ordered {
        return chunk_ordered(sh, chunk);
    }
    if let Some(fast) = &sh.fast {
        return fast_chunk(sh, fast, chunk, base, fouts, planes);
    }
    let t = &sh.t;
    let bs = sh.block_size;
    let banks = &regs.integer_slots;
    let slots = regs.slots as usize;
    // a bank nothing writes stays empty
    let integers = banks.contains(&true);
    let traces = t.code.iter().any(|i| matches!(Instruction::unguarded(i), Instruction::Trace(..)));
    let mut tc = TC {
        integer: vec![0; if integers { slots * bs } else { 0 }],
        floats: vec![0.0; slots * bs],
        texts: vec![Vec::new(); sh.text_count * bs],
        traced: vec![None; if traces { bs } else { 0 }],
        next_float: vec![0.0; sh.carried],
        next_int: vec![0; sh.carried],
        next_text: vec![Vec::new(); sh.carried],
        tables: HashMap::new(),
        scratch: rays_math::noise::create_fbm3_scratch(),
        indices: if sh.contiguous { Vec::new() } else { vec![0; bs] },
        start: 0,
        guard_spans: None,
        stamp: 0,
        acc: [0.0; 4],
    };
    if region::clobbered(t) {
        for (slot, dedicated) in regs.dedicated.iter().enumerate() {
            if !dedicated {
                if integers {
                    tc.integer[slot * bs..(slot + 1) * bs].fill(i64::MIN);
                }
                tc.floats[slot * bs..(slot + 1) * bs].fill(f64::NAN);
            }
        }
    }
    // A float Sum or Accumulate runs on a chunk-local accumulator: chunks run in order, each
    // taking the running value from the launch and handing it back.
    let fold = (t.result != RegionResult::Collect && !sh.integer_outputs[0]).then(|| t.outputs[0].len());
    let mut initialized = sh.initialized.load(Ordering::Relaxed);
    if fold.is_some() {
        let running = components_k(&sh.accumulator.lock().unwrap())?;
        tc.acc[..running.len()].copy_from_slice(&running);
    }
    let dependent = sh.dependent_flags.contains(&true);
    let has_planes = sh.planes;
    let ncode = t.code.len();
    let last_block = sh.blocks.min((chunk + 1) * sh.blocks_per_chunk);
    for block in chunk * sh.blocks_per_chunk..last_block {
        E::interruptible(&mut || {})?;
        let start = block * bs;
        let limit = sh.count.min((block + 1) * bs);
        let before = sh.skipped_before(start);
        let length = if sh.skip.is_empty() {
            if !sh.contiguous {
                for j in 0..limit - start {
                    tc.indices[j] = start + j;
                }
            }
            limit - start
        } else {
            let mut skipped = before;
            let mut length = 0;
            for i in start..limit {
                if skipped < sh.skip.len() && sh.skip[skipped] == i {
                    skipped += 1;
                } else {
                    tc.indices[length] = i;
                    length += 1;
                }
            }
            length
        };
        tc.start = start;
        tc.stamp += 1;
        if sh.cache_guards && tc.guard_spans.is_none() {
            tc.guard_spans = Some(Vec::new());
        }
        if sh.gather && t.result == RegionResult::Collect {
            // Speculate: run the block instruction-major. Without an error the values equal the
            // lane-major ones, because lanes are independent and instructions pure; on any error
            // the block is redone lane-major below, so the first error in lane order is the one
            // reported. Spans cached from a failed speculation may hold garbage guards, so the
            // cache is dropped for the redo.
            let speculated = match texec_range(sh, regs, &mut tc, fo, None, 0, ncode, 0, length) {
                Ok(()) => true,
                Err(KErr::Diag(_)) => false,
                Err(e) => return Err(e),
            };
            if !speculated {
                tc.guard_spans = None;
                for slot in 0..sh.total_prefix {
                    texec(sh, regs, &mut tc, fo, slot, &regs.run_code[slot], 0, length)?;
                }
                // Only already published, validated tables can extend the prefix. A failed bounds
                // proof leaves the whole remaining suffix lane-major.
                let mut prefix = sh.total_prefix;
                while prefix < regs.run_code.len() && safe(sh, regs, &tc, length, Vec::new(), &regs.run_code[prefix]) {
                    if !matches!(Instruction::unguarded(&t.code[prefix]), Instruction::Array_prepare(..)) {
                        texec(sh, regs, &mut tc, fo, prefix, &regs.run_code[prefix], 0, length)?;
                    }
                    prefix += 1;
                }
                for j in 0..length {
                    texec_range(sh, regs, &mut tc, fo, None, prefix, ncode, j, j + 1)?;
                }
            }
        } else if !sh.gather {
            texec_range(sh, regs, &mut tc, fo, Some(false), 0, ncode, 0, length)?;
        }
        // Instructions validate selected float results before collection.
        if t.result == RegionResult::Collect {
            let output_index = start - before - base;
            for (k, registers) in regs.run_outputs.iter().enumerate() {
                if has_planes {
                    for (c, r) in registers.iter().enumerate() {
                        let r = *r as usize;
                        if let Some(data) = &mut planes[k][c] {
                            if banks[r] {
                                for j in 0..length {
                                    data[output_index + j] = tc.integer[r * bs + j] as f64;
                                }
                            } else {
                                data[output_index..output_index + length].copy_from_slice(&tc.floats[r * bs..r * bs + length]);
                            }
                        }
                    }
                } else if sh.text_outputs[k] {
                    let o = regs.text_slots[registers[0] as usize] as usize * bs;
                    if let Some(data) = &mut touts[k] {
                        data[output_index..output_index + length].clone_from_slice(&tc.texts[o..o + length]);
                    }
                } else if sh.integer_outputs[k] {
                    let r = registers[0] as usize;
                    if let Some(data) = &mut iouts[k] {
                        data[output_index..output_index + length].copy_from_slice(&tc.integer[r * bs..r * bs + length]);
                    }
                } else if let Some(data) = &mut fouts[k] {
                    let width = registers.len();
                    for (c, r) in registers.iter().enumerate() {
                        let r = *r as usize;
                        if banks[r] {
                            for j in 0..length {
                                data[(output_index + j) * width + c] = tc.integer[r * bs + j] as f64;
                            }
                        } else if width == 1 {
                            data[output_index..output_index + length].copy_from_slice(&tc.floats[r * bs..r * bs + length]);
                        } else {
                            for j in 0..length {
                                data[(output_index + j) * width + c] = tc.floats[r * bs + j];
                            }
                        }
                    }
                }
            }
        } else {
            for j in 0..length {
                if sh.gather {
                    texec_range(sh, regs, &mut tc, fo, None, 0, ncode, j, j + 1)?;
                } else if dependent {
                    texec_range(sh, regs, &mut tc, fo, Some(true), 0, ncode, j, j + 1)?;
                }
                let output_index = start - before + j - base;
                let registers = &t.outputs[0];
                match fold {
                    Some(width) => {
                        for (c, r) in registers.iter().enumerate() {
                            let x = finite(tc.numeric(regs, bs, *r as usize, j))?;
                            tc.acc[c] = match &t.result {
                                RegionResult::Sum(_) if initialized => finite(tc.acc[c] + x)?,
                                _ => x,
                            };
                            if let (true, Some(data)) = (sh.collecting, &mut fouts[0]) {
                                data[output_index * width + c] = x;
                            }
                        }
                    }
                    None => {
                        let value = Value::Int(tc.integer[registers[0] as usize * bs + j]);
                        let mut acc = sh.accumulator.lock().unwrap();
                        let next = match &t.result {
                            RegionResult::Collect => unreachable!("collect stores whole blocks"),
                            RegionResult::Accumulate(..) => value.clone(),
                            RegionResult::Sum(_) if !initialized => value.clone(),
                            RegionResult::Sum(zero) => match (&*acc, &value) {
                                (Value::Int(a), Value::Int(b)) if *zero => Value::Int(packed_ops::integer_apply(Integer::Iadd, *a, *b)?),
                                (a, b) => value::arith("array/sum", &|x, y| x + y, a, b)?,
                            },
                        };
                        *acc = next;
                        if let (true, Some(data)) = (sh.collecting, &mut iouts[0]) {
                            data[output_index] = tc.integer[registers[0] as usize * bs + j];
                        }
                    }
                }
                initialized = true;
            }
        }
    }
    if t.result != RegionResult::Collect {
        if let Some(width) = fold {
            *sh.accumulator.lock().unwrap() = scalar_value_of(width, &tc.acc[..width]);
        }
        sh.initialized.store(initialized, Ordering::Relaxed);
    }
    Ok(())
}

fn split_ints<'a>(data: &'a mut [i64], bounds: &[usize]) -> Vec<Mutex<Option<&'a mut [i64]>>> {
    let mut parts = Vec::with_capacity(bounds.len().saturating_sub(1));
    let mut rest = data;
    for c in 0..bounds.len().saturating_sub(1) {
        let (head, tail) = std::mem::take(&mut rest).split_at_mut(bounds[c + 1] - bounds[c]);
        parts.push(Mutex::new(Some(head)));
        rest = tail;
    }
    parts
}

fn split_texts<'a>(data: &'a mut [Vec<u8>], bounds: &[usize]) -> Vec<Mutex<Option<&'a mut [Vec<u8>]>>> {
    let mut parts = Vec::with_capacity(bounds.len().saturating_sub(1));
    let mut rest = data;
    for c in 0..bounds.len().saturating_sub(1) {
        let (head, tail) = std::mem::take(&mut rest).split_at_mut(bounds[c + 1] - bounds[c]);
        parts.push(Mutex::new(Some(head)));
        rest = tail;
    }
    parts
}

fn run_typed(
    run: &Run,
    seed: &Option<Value>,
    evaluate: &dyn Fn(ResidualId, &Arc<Term>) -> Result<Value, Diagnostic>,
    uniform: &dyn Fn(&Value) -> Result<Value, Diagnostic>,
    plane_outputs: PlaneOutputs<'_, '_>,
) -> K<Vec<Value>> {
    let t = &run.t;
    let carried = t
        .code
        .iter()
        .fold(0usize, |count, i| match Instruction::unguarded(i) {
            Instruction::Loop_begin(l) => count.max(l.initial.len()),
            _ => count,
        });
    let gather = t.code.iter().any(|i| match Instruction::unguarded(i) {
        Instruction::Array_prepare(..) | Instruction::Gather(..) | Instruction::Int_gather(..) | Instruction::Loop_begin(_) => true,
        body => region::text_register(body),
    });
    let cache_guards = carried == 0 && t.result == RegionResult::Collect && t.code.iter().any(|i| matches!(i, Instruction::Guarded(..)));
    // typed inputs
    let nsrc = t.sources.len();
    let mut borrowed: Vec<Option<Vec<Column>>> = vec![None; nsrc];
    let mut ranges: Vec<Option<usize>> = vec![None; nsrc];
    let mut empty_product = false;
    let mut lengths = vec![0usize; nsrc];
    let mut inputs: Vec<Value> = Vec::with_capacity(nsrc);
    for (i, (r, term)) in t.sources.iter().enumerate() {
        if empty_product {
            inputs.push(Value::No_geo);
            continue;
        }
        borrowed[i] = match (&run.column_source, term.ty()) {
            (Some(source), Ty::Array(inner)) if matches!(**inner, Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4) => source(*r, term)?,
            _ => None,
        };
        let value = if borrowed[i].is_some() {
            Value::No_geo
        } else {
            match integer_range(*r, term) {
                Some((rr, count)) => {
                    let count_value = evaluate(rr, &Arc::new(count))?;
                    ranges[i] = Some(op::array_count(Some(MAX_ARRAY), "array/range", &count_value)?);
                    Value::No_geo
                }
                None => evaluate(*r, term)?,
            }
        };
        lengths[i] = match (&ranges[i], &borrowed[i]) {
            (Some(count), _) => *count,
            (None, None) => array_len_k(&value)?,
            (None, Some(columns)) => borrowed_count(t.widths[i] as usize, columns)?,
        };
        if t.iteration == Iteration::Product && lengths[i] == 0 {
            empty_product = true;
        }
        inputs.push(value);
    }
    let started = run.measure.as_ref().map(|m| (m.clock)());
    let excluded = std::cell::Cell::new(0.0f64);
    let measured_uniform = |v: &Value| -> Result<Value, Diagnostic> {
        match &run.measure {
            None => uniform(v),
            Some(m) => {
                let before = (m.clock)();
                let result = uniform(v);
                excluded.set(excluded.get() + ((m.clock)() - before).max(0.0));
                result
            }
        }
    };
    let regs = region::registers(t);
    let mut sh_ranges = ranges.clone();
    let _ = &mut sh_ranges;
    let ucells: Vec<UCell> = t
        .uniforms
        .iter()
        .map(|v| {
            let cell = UCell { value: OnceLock::new(), comps: OnceLock::new() };
            if concrete(v) {
                let _ = cell.value.set(Ok(v.clone()));
            }
            if scalar_value(v) {
                let _ = cell.comps.set(components_k(v));
            }
            cell
        })
        .collect();
    let mut sh = TS {
        t: t.clone(),
        live: run.live.clone(),
        inputs,
        borrowed,
        ranges,
        lengths: lengths.clone(),
        strides: vec![1; nsrc],
        skip: Vec::new(),
        count: 0,
        ucells,
        prepared: t.uniforms.iter().map(|_| OnceLock::new()).collect(),
        frames: t.code.iter().map(|_| OnceLock::new()).collect(),
        accumulator: Mutex::new(Value::Float(0.0)),
        initialized: AtomicBool::new(true),
        integer_outputs: Vec::new(),
        text_outputs: Vec::new(),
        carried,
        gather,
        cache_guards,
        total_prefix: 0,
        collecting: false,
        planes: plane_outputs.is_some(),
        block_size: 1,
        blocks: 0,
        blocks_per_chunk: 1,
        dependent_flags: Vec::new(),
        text_count: regs.text_slots.iter().filter(|o| **o >= 0).count(),
        contiguous: false,
        ordered: false,
        fast: None,
    };
    // A leading total prefix only writes private banks. Keep every potentially failing instruction
    // and lazy capture in the existing reference lane order.
    let total_prefix = if gather && t.result == RegionResult::Collect {
        let mut length = 0;
        while length < t.code.len() && typed_total(&sh, &t.code[length]) {
            length += 1;
        }
        length
    } else {
        0
    };
    let count: i64 = if nsrc == 0 {
        0
    } else if t.iteration == Iteration::Product && lengths.contains(&0) {
        0
    } else {
        let mut n: i64 = if t.iteration == Iteration::Zip { i64::MAX } else { 1 };
        for length in &lengths {
            let length = *length as i64;
            n = match t.iteration {
                Iteration::Zip => n.min(length),
                Iteration::Product => {
                    if length != 0 && n > i64::MAX / length {
                        return Err(fail("E_ARRAY_RANGE", "Kernel iteration count exceeds native bounds."));
                    }
                    n * length
                }
            };
        }
        n
    };
    let count = count as usize;
    for (_, term) in t.stages.iter() {
        if let Ty::Array(ty) = term.ty() {
            if packed_ops::scalar_width(ty).is_some_and(|w| count > MAX_ARRAY / w) {
                return Err(fail("E_ARRAY_RANGE", "Kernel intermediate exceeds native storage bounds."));
            }
        }
    }
    let mut strides = vec![1usize; nsrc];
    for i in (0..nsrc.saturating_sub(1)).rev() {
        strides[i] = strides[i + 1] * lengths[i + 1];
    }
    let mut skip: Vec<usize> = t.skip.iter().filter(|i| **i >= 0 && (**i as usize) < count).map(|i| *i as usize).collect();
    skip.sort_unstable();
    skip.dedup();
    let kept = count - skip.len();
    if let Some(n) = run.expected_count {
        if n != kept {
            return Err(fail("E_ARRAY_COUNT", "Column sink count differs from the kernel count."));
        }
    }
    let collecting = matches!(t.result, RegionResult::Collect | RegionResult::Accumulate(_, true));
    let integer_outputs: Vec<bool> = t.outputs.iter().map(|regs| regs.len() == 1 && region::integer_register(&t.code[regs[0] as usize])).collect();
    let text_outputs: Vec<bool> = t.outputs.iter().map(|regs| regs.len() == 1 && region::text_register(&t.code[regs[0] as usize])).collect();
    if let Some(planes) = &plane_outputs {
        if t.result != RegionResult::Collect || planes.len() != t.outputs.len() {
            return Err(fail("E_ARRAY_TYPE", "Column sinks require matching Collect outputs."));
        }
        for (k, lanes) in planes.iter().enumerate() {
            if text_outputs[k] {
                return Err(fail("E_ARRAY_TYPE", "Float column sinks cannot store Text outputs."));
            }
            if integer_outputs[k] || lanes.len() != t.outputs[k].len() {
                return Err(fail("E_ARRAY_TYPE", "Float column sinks cannot store integer outputs."));
            }
            for a in lanes.iter().flatten() {
                if a.len() != kept {
                    return Err(fail("E_ARRAY_COUNT", "Column sink count differs from the kernel count."));
                }
            }
        }
    }
    let has_planes = plane_outputs.is_some();
    let n_out = t.outputs.len();
    let mut fvec: Vec<Vec<f64>> = vec![Vec::new(); n_out];
    let mut ivec: Vec<Vec<i64>> = vec![Vec::new(); n_out];
    let mut tvec: Vec<Vec<Vec<u8>>> = vec![Vec::new(); n_out];
    if collecting && !has_planes {
        for k in 0..n_out {
            let width = t.outputs[k].len();
            if text_outputs[k] {
                if kept > MAX_ARRAY {
                    return Err(fail("E_ARRAY_RANGE", "Array length is outside native storage bounds."));
                }
                tvec[k] = vec![Vec::new(); kept];
            } else if integer_outputs[k] {
                if kept > MAX_ARRAY {
                    return Err(fail("E_ARRAY_RANGE", "Array length is outside native storage bounds."));
                }
                ivec[k] = region::ints_for(t, kept);
            } else {
                if kept > MAX_ARRAY / width {
                    return Err(fail("E_ARRAY_RANGE", "Array length is outside native storage bounds."));
                }
                fvec[k] = region::floats_for(t, kept * width);
            }
        }
    }
    // a float accumulator: a vector seed's components, a scalar seed repeated, zeros without one
    let accumulator = if integer_outputs[0] {
        seed.clone().unwrap_or(Value::Int(0))
    } else {
        let width = t.outputs[0].len();
        let mut running = [0.0; 4];
        match seed {
            Some(v @ (Value::Vec2(..) | Value::Vec3(..) | Value::Vec4(..))) => {
                let components = components(v)?;
                running[..components.len()].copy_from_slice(&components);
            }
            Some(v) => running[..width].fill(num_k(v)?),
            None => {}
        }
        scalar_value_of(width, &running)
    };
    let initialized = match t.result {
        RegionResult::Sum(zero) => zero,
        _ => true,
    };
    // a block's scratch holds every register for each of its lanes: a large kernel takes fewer
    // lanes, so scratch stays near 64K slots (512 KiB a bank)
    let (block_size, blocks, blocks_per_chunk, chunks) = block_layout(count, 64.max(1024.min(65536 / 1.max(regs.slots as usize))));
    let dependent_flags = if count == 0 { Vec::new() } else { region::dependent_flags(t).clone() };
    sh.strides = strides;
    sh.skip = skip;
    sh.count = count;
    sh.accumulator = Mutex::new(accumulator);
    sh.initialized = AtomicBool::new(initialized);
    sh.integer_outputs = integer_outputs;
    sh.text_outputs = text_outputs;
    sh.total_prefix = total_prefix;
    sh.collecting = collecting;
    sh.block_size = block_size;
    sh.blocks = blocks;
    sh.blocks_per_chunk = blocks_per_chunk;
    sh.dependent_flags = dependent_flags;
    sh.contiguous = t.iteration == Iteration::Zip && sh.skip.is_empty();
    sh.ordered = ordered_add(t)
        && sh.borrowed[0].is_none()
        && matches!((&sh.inputs[0], t.widths[0]), (Value::Floats(actual @ 1..=4, _), want) if *actual as i64 == want);
    sh.fast = if sh.ordered { None } else { fast_path(&sh, &measured_uniform)? };
    let sh = sh;
    // A Cartesian projection repeats one checked row. Its unused outer input is concrete integers
    // or a checked range; removing those loads cannot hide a type or finite-value error.
    let outer_ok = |i: usize| sh.ranges[i].is_some() || matches!(sh.inputs[i], Value::Int_array(_));
    let repeated_row: Option<(usize, Arc<Vec<f64>>)> = match (t.iteration, &t.result, sh.skip.is_empty(), sh.inputs.as_slice(), t.code.as_slice(), t.outputs.as_slice()) {
        (Iteration::Product, RegionResult::Collect, true, [_, Value::Floats(1, row)], [Instruction::Int_input(0), Instruction::Input(1, 1, 0)], [o])
            if o.as_slice() == [1] && outer_ok(0) =>
        {
            Some((sh.lengths[0], row.clone()))
        }
        (Iteration::Product, RegionResult::Collect, true, [_, Value::Floats(1, row)], [Instruction::Input(1, 1, 0), Instruction::Int_input(0)], [o])
            if o.as_slice() == [0] && outer_ok(0) =>
        {
            Some((sh.lengths[0], row.clone()))
        }
        _ => None,
    };
    // Exact integer scaling needs one pass and no temporary register banks.
    let scaled_column: Option<i64> = match (t.iteration, &t.result, sh.skip.is_empty(), sh.inputs.as_slice(), t.code.as_slice(), t.outputs.as_slice()) {
        (Iteration::Zip, RegionResult::Collect, true, [_], [Instruction::Int_input(0), Instruction::Int_const(factor), Instruction::Int_binary(Integer::Imul, 0, 1)], [o])
            if o.as_slice() == [2] && outer_ok(0) && !has_planes && ivec[0].len() == kept =>
        {
            Some(*factor)
        }
        (Iteration::Zip, RegionResult::Collect, true, [_], [Instruction::Int_const(factor), Instruction::Int_input(0), Instruction::Int_binary(Integer::Imul, 0, 1)], [o])
            if o.as_slice() == [2] && outer_ok(0) && !has_planes && ivec[0].len() == kept =>
        {
            Some(*factor)
        }
        _ => None,
    };
    if let Some((outer_count, row)) = repeated_row {
        if count > 0 {
            for x in row.iter() {
                if !x.is_finite() {
                    return nonfinite();
                }
            }
            let len = row.len();
            let per = 4096usize;
            let nchunks = outer_count.div_ceil(per);
            let bounds: Vec<usize> = (0..=nchunks).map(|c| (c * per).min(outer_count)).collect();
            let mut planes_opt = plane_outputs;
            let mut targets: Vec<Vec<Mutex<Option<&mut [f64]>>>> = Vec::new();
            match planes_opt.as_mut() {
                Some(planes) => {
                    if let Some(d) = planes[0][0].as_mut() {
                        targets.push(split_parts(&mut **d, &bounds, len));
                    }
                }
                None => targets.push(split_parts(fvec[0].as_mut_slice(), &bounds, len)),
            }
            if let Some(parts) = targets.first() {
                let work = |c: i64| -> Result<(), String> {
                    let part = parts[c as usize].lock().unwrap().take().unwrap();
                    for chunk in part.chunks_mut(len) {
                        chunk.copy_from_slice(&row);
                    }
                    Ok(())
                };
                parallel::for_(1, 0, nchunks as i64 - 1, &work).map_err(noise_error)?;
            }
        }
    } else if let Some(factor) = scaled_column {
        let per = 4096usize;
        let nchunks = count.div_ceil(per);
        let bounds: Vec<usize> = (0..=nchunks).map(|c| (c * per).min(count)).collect();
        let parts = split_ints(ivec[0].as_mut_slice(), &bounds);
        let input: Option<Arc<Vec<i64>>> = match &sh.inputs[0] {
            Value::Int_array(d) => Some(d.clone()),
            _ => None,
        };
        let failure: Mutex<Option<Diagnostic>> = Mutex::new(None);
        let work = |c: i64| -> Result<(), String> {
            let part = parts[c as usize].lock().unwrap().take().unwrap();
            let first = bounds[c as usize];
            for (i, slot) in part.iter_mut().enumerate() {
                let x = match &input {
                    Some(d) => d[first + i],
                    None => (first + i) as i64,
                };
                match packed_ops::integer_apply(Integer::Imul, x, factor) {
                    Ok(v) => *slot = v,
                    Err(d) => {
                        *failure.lock().unwrap() = Some(d);
                        return Ok(());
                    }
                }
            }
            Ok(())
        };
        parallel::for_(1, 0, nchunks as i64 - 1, &work).map_err(noise_error)?;
        if let Some(d) = failure.into_inner().unwrap() {
            return Err(KErr::Diag(d));
        }
    } else {
        // Lazy captures cannot be forced from a worker thread. The first chunk runs here, in
        // reference order, and forces whichever captures its lanes select; when every capture is
        // then a value the remaining chunks run in parallel.
        let bounds: Vec<usize> = (0..=chunks)
            .map(|c| {
                let s = count.min(c * blocks_per_chunk * block_size);
                s - sh.skipped_before(s)
            })
            .collect();
        let mut planes_opt = plane_outputs;
        let f_parts: Vec<Option<Vec<Mutex<Option<&mut [f64]>>>>> = fvec
            .iter_mut()
            .enumerate()
            .map(|(k, d)| if d.is_empty() { None } else { Some(split_parts(d.as_mut_slice(), &bounds, t.outputs[k].len())) })
            .collect();
        let i_parts: Vec<Option<Vec<Mutex<Option<&mut [i64]>>>>> =
            ivec.iter_mut().map(|d| if d.is_empty() { None } else { Some(split_ints(d.as_mut_slice(), &bounds)) }).collect();
        let t_parts: Vec<Option<Vec<Mutex<Option<&mut [Vec<u8>]>>>>> =
            tvec.iter_mut().map(|d| if d.is_empty() { None } else { Some(split_texts(d.as_mut_slice(), &bounds)) }).collect();
        let p_parts: Vec<Vec<Option<Vec<Mutex<Option<&mut [f64]>>>>>> = match planes_opt.as_mut() {
            None => Vec::new(),
            Some(planes) => planes
                .iter_mut()
                .map(|row| row.iter_mut().map(|p| p.as_mut().map(|d| split_parts(&mut **d, &bounds, 1))).collect())
                .collect(),
        };
        let interrupt = E::current_interrupt();
        let run_one = |c: usize, fo: Option<&Forcer<'_>>| -> K<()> {
            let mut fouts: Vec<Option<&mut [f64]>> = f_parts.iter().map(|p| p.as_ref().map(|v| v[c].lock().unwrap().take().unwrap())).collect();
            let mut iouts: Vec<Option<&mut [i64]>> = i_parts.iter().map(|p| p.as_ref().map(|v| v[c].lock().unwrap().take().unwrap())).collect();
            let mut touts: Vec<Option<&mut [Vec<u8>]>> = t_parts.iter().map(|p| p.as_ref().map(|v| v[c].lock().unwrap().take().unwrap())).collect();
            let mut planes: Vec<Vec<Option<&mut [f64]>>> = p_parts
                .iter()
                .map(|row| row.iter().map(|p| p.as_ref().map(|v| v[c].lock().unwrap().take().unwrap())).collect())
                .collect();
            interrupted(&interrupt, &mut || typed_chunk(&sh, regs, c, fo, bounds[c], &mut fouts, &mut iouts, &mut touts, &mut planes))
        };
        let table = |i: usize| -> K<bool> { Ok(matches!(value::ty_of(&sh.uvalue(i, Some(&measured_uniform))?), Ty::Array(_))) };
        let settled = || -> bool {
            if !sh.ucells.iter().all(|c| c.value.get().is_some()) {
                return false;
            }
            for i in 0..sh.ucells.len() {
                match table(i) {
                    Ok(false) => {
                        if sh.ucomps(i, Some(&measured_uniform)).is_err() {
                            return false;
                        }
                    }
                    Ok(true) => {}
                    Err(_) => return false,
                }
            }
            (0..sh.ucells.len()).all(|i| matches!(table(i), Ok(true)) || sh.ucells[i].comps.get().is_some())
        };
        let first_chunk = if chunks == 0 || t.uniforms.iter().all(concrete) {
            0
        } else {
            run_one(0, Some(&measured_uniform))?;
            1
        };
        if t.result == RegionResult::Collect && (count >= 4096 || !integer_program(t)) && (first_chunk == 0 || settled()) {
            let errors: Vec<Mutex<Option<KErr>>> = (0..chunks).map(|_| Mutex::new(None)).collect();
            let work = |i: i64| -> Result<(), String> {
                if let Err(e) = run_one(i as usize, None) {
                    *errors[i as usize].lock().unwrap() = Some(e);
                }
                Ok(())
            };
            parallel::for_(1, first_chunk as i64, chunks as i64 - 1, &work).map_err(noise_error)?;
            for e in errors.iter() {
                if let Some(e) = e.lock().unwrap().take() {
                    return Err(e);
                }
            }
        } else {
            for i in first_chunk..chunks {
                run_one(i, Some(&measured_uniform))?;
            }
        }
    }
    E::interruptible(&mut || {})?;
    let result: Vec<Value> = if has_planes {
        Vec::new()
    } else if collecting {
        (0..n_out)
            .map(|k| {
                if sh.text_outputs[k] {
                    Value::Text_array(Arc::new(rays_math::text_array::TextArray::from(std::mem::take(&mut tvec[k]))))
                } else if sh.integer_outputs[k] {
                    Value::Int_array(Arc::new(std::mem::take(&mut ivec[k])))
                } else {
                    float_value_of(t.outputs[k].len(), std::mem::take(&mut fvec[k]))
                }
            })
            .collect()
    } else {
        let acc = sh.accumulator.lock().unwrap().clone();
        vec![if kept == 0 {
            match (&t.result, seed) {
                (RegionResult::Accumulate(..), Some(seed)) => seed.clone(),
                (RegionResult::Sum(false), _) => Value::Int(0),
                _ => acc,
            }
        } else {
            acc
        }]
    };
    if let (Some(m), Some(started)) = (&run.measure, started) {
        (m.report)(t, ((m.clock)() - started - excluded.get()).max(0.0), false);
    }
    if result.len() == 1 {
        region::finished(t, result[0].clone());
    }
    Ok(result)
}

// ---------------------------------------------------------------------------------------------
// One outermost run shares lazily forced captures
//
// Within one outermost run, a lazily forced capture is evaluated again by every region and capture
// that reads it (a map feeding a minimum, a maximum and the map that scales by both ran three
// times). A residual term is a function of its captured values, the frame, the host handles
// behind `resolve` and the frame state, so the results of one outermost run are shared between its
// nested evaluations when all four are the same and the term reads no state, and dropped when
// that run ends.

type RView = E::residual::View;

struct ScopeData {
    resolve: Option<Resolve>,
    live: Live,
    state_stamp: E::StateStamp,
    strict: bool,
    depth: usize,
    entries: Vec<(ResidualId, Arc<Term>, Value)>,
}

const SCOPE_CAPACITY: usize = 32;

thread_local! {
    static CURRENT: RefCell<Option<ScopeData>> = const { RefCell::new(None) };
    static ACTIVE: RefCell<Vec<(Arc<Term>, RView)>> = const { RefCell::new(Vec::new()) };
    static REQUESTS: RefCell<Vec<(Arc<Term>, RView)>> = const { RefCell::new(Vec::new()) };
    static PENDING: RefCell<Option<Diagnostic>> = const { RefCell::new(None) };
}

fn same_term(a: &Arc<Term>, b: &Arc<Term>) -> bool {
    a.identity() == b.identity() || (a.form().span == b.form().span && **a == **b)
}

fn same_value(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Floats(aw @ 1..=4, x), Value::Floats(bw, y)) if aw == bw => Arc::ptr_eq(x, y),
        (Value::Int_array(x), Value::Int_array(y)) => Arc::ptr_eq(x, y),
        (Value::Text_array(x), Value::Text_array(y)) => Arc::ptr_eq(x, y),
        _ => a == b,
    }
}

fn same_bindings(a: &[(String, Value)], b: &[(String, Value)]) -> bool {
    a.len() == b.len() && a.iter().zip(b.iter()).all(|((n, x), (m, y))| n == m && same_value(x, y))
}

fn same_captures(a: &[(Arc<str>, Value)], b: &[(Arc<str>, Value)]) -> bool {
    a.len() == b.len() && a.iter().zip(b).all(|((n, x), (m, y))| n == m && same_value(x, y))
}

fn same_context(view: &RView, other: &RView) -> bool {
    view.instance == other.instance
        && view.site == other.site
        && view.prefix == other.prefix
        && view.owner == other.owner
        && same_captures(&view.bindings, &other.bindings)
}

fn same_resolve(a: &Option<Resolve>, b: &Option<Resolve>) -> bool {
    match (a, b) {
        (None, None) => true,
        (Some(a), Some(b)) => Arc::ptr_eq(a, b),
        _ => false,
    }
}

// The same residual can be met as distinct records, under distinct ids (a walker's kernel request
// numbers each one afresh), so identity is the same term, bindings and site.
fn same_residual(a: ResidualId, b: ResidualId) -> bool {
    if a == b {
        return true;
    }
    let (Ok(x), Ok(y)) = (E::residual::residual_view(a), E::residual::residual_view(b)) else { return false };
    x.term == y.term
        && x.previous == y.previous
        && x.instance == y.instance
        && x.iter == y.iter
        && x.prefix == y.prefix
        && x.owner == y.owner
        && x.host == y.host
        && x.geometry == y.geometry
        && same_captures(&x.bindings, &y.bindings)
}

fn scope_enter(run: &Run) -> bool {
    CURRENT.with(|cell| {
        let mut current = cell.borrow_mut();
        match current.as_mut() {
            Some(scope)
                if same_resolve(&scope.resolve, &run.resolve)
                    && scope.live == run.live
                    && scope.state_stamp == E::state_stamp(&run.state)
                    && scope.strict == run.strict =>
            {
                scope.depth += 1;
                true
            }
            Some(_) => false,
            None => {
                *current = Some(ScopeData {
                    resolve: run.resolve.clone(),
                    live: run.live.clone(),
                    state_stamp: E::state_stamp(&run.state),
                    strict: run.strict,
                    depth: 1,
                    entries: Vec::new(),
                });
                true
            }
        }
    })
}

struct ScopeGuard(bool);
impl Drop for ScopeGuard {
    fn drop(&mut self) {
        if self.0 {
            CURRENT.with(|cell| {
                let mut current = cell.borrow_mut();
                let done = match current.as_mut() {
                    Some(scope) => {
                        scope.depth -= 1;
                        scope.depth == 0
                    }
                    None => false,
                };
                if done {
                    *current = None;
                }
            });
        }
    }
}

struct PopGuard(&'static std::thread::LocalKey<RefCell<Vec<(Arc<Term>, RView)>>>);
impl Drop for PopGuard {
    fn drop(&mut self) {
        self.0.with(|stack| {
            stack.borrow_mut().pop();
        });
    }
}

fn scope_find(residual: ResidualId, term: &Arc<Term>) -> Option<Value> {
    CURRENT.with(|cell| {
        let current = cell.borrow();
        current.as_ref()?.entries.iter().find(|(r, t, _)| same_term(t, term) && same_residual(*r, residual)).map(|(_, _, v)| v.clone())
    })
}

fn scope_add(residual: ResidualId, term: &Arc<Term>, value: &Value) {
    CURRENT.with(|cell| {
        if let Some(scope) = cell.borrow_mut().as_mut() {
            if scope.entries.len() < SCOPE_CAPACITY {
                scope.entries.push((residual, term.clone(), value.clone()));
            }
        }
    });
}

// ---------------------------------------------------------------------------------------------
// Evaluation of a region's captures

fn kernel_form_error() -> Diagnostic {
    value::fail("E_KERNEL_FORM", "Numeric node bodies must compile to packed kernels.")
}

fn leaf_term(term: &Term) -> bool {
    matches!(term.node, Node::Lit(_) | Node::Text(_) | Node::Nil | Node::Time | Node::FnRef(_))
}

fn numeric_loop(term: &Term) -> bool {
    match term.node {
        Node::Hof(id) => matches!(term.arena().hof(id).kind, flow_check::workspace::HofKind::Map | flow_check::workspace::HofKind::Reduce),
        Node::Loop(_) => true,
        Node::Op(id) => term.name(term.arena().op(id).op) == "array/sum",
        _ => false,
    }
}

fn take_pending() -> Option<Diagnostic> {
    PENDING.with(|p| p.borrow_mut().take())
}

/// The reference interpreter over one residual term (`E.Residual.eval_term` with the kernel handler
/// active); a strict kernel request's error surfaces here.
fn eval_term(run: &Run, residual: ResidualId, term: &Term) -> Result<Value, Diagnostic> {
    if let Some(on_walk) = &run.on_walk {
        on_walk();
    }
    let result = E::residual::eval_term(residual, term, &run.live, run_state(run), run.elems.as_deref(), run.resolve.clone());
    match take_pending() {
        Some(d) => Err(d),
        None => result,
    }
}

fn evaluate(run: &Run, dispatch: &Option<Dispatch>, residual: ResidualId, term: &Arc<Term>) -> Result<Value, Diagnostic> {
    let view = E::residual::residual_view(residual)?;
    let looped = !leaf_term(term)
        && ACTIVE.with(|stack| stack.borrow().iter().any(|(t, other)| same_term(t, term) && same_context(&view, other) && view.iter == other.iter));
    if looped {
        if run.strict && numeric_loop(term) {
            return Err(kernel_form_error());
        }
        return match dispatch {
            Some(d) => d(residual, term),
            None => eval_term(run, residual, term),
        };
    }
    ACTIVE.with(|stack| stack.borrow_mut().push((term.clone(), view)));
    let _pop = PopGuard(&ACTIVE);
    if !run.in_scope {
        return evaluate_once(run, dispatch, residual, term);
    }
    if let Some(value) = scope_find(residual, term) {
        return Ok(value);
    }
    let result = evaluate_once(run, dispatch, residual, term);
    if let Ok(value) = &result {
        if !E::state_dependent(&Value::Residual(residual)) {
            scope_add(residual, term, value);
        }
    }
    result
}

fn evaluate_once(run: &Run, dispatch: &Option<Dispatch>, residual: ResidualId, term: &Arc<Term>) -> Result<Value, Diagnostic> {
    let nested = match &term.node {
        Node::RefBinding(id) if term.arena().name_ids(term.arena().ref_binding(*id).fields).is_empty() => {
            let name = term.name(term.arena().ref_binding(*id).name);
            let view = E::residual::residual_view(residual)?;
            match view.bindings.iter().find(|(n, _)| n.as_ref() == name) {
                Some((_, Value::Residual(r))) => {
                    let inner = E::residual::residual_view(*r)?;
                    Some((*r, inner.term))
                }
                // a settled value (a captured array per iterate step) is read, not compiled
                Some((_, v)) if !E::is_live(v) => return match dispatch {
                    Some(d) => d(residual, term),
                    None if value::ty_of(v) == *term.ty() => Ok(v.clone()),
                    None => eval_term(run, residual, term),
                },
                _ => None,
            }
        }
        _ => None,
    };
    if let Some((r, inner)) = nested {
        return evaluate(run, dispatch, r, &inner);
    }
    if leaf_term(term) {
        return eval_term(run, residual, term);
    }
    if let Some(d) = dispatch {
        return d(residual, term);
    }
    let t = &run.t;
    match compile_impl(false, t.fusion, t.dynamic, &t.count_source, residual, term) {
        Ok(program) => {
            let inner = Run {
                t: program,
                live: run.live.clone(),
                state: run.state.clone(),
                elems: run.elems.clone(),
                resolve: run.resolve.clone(),
                measure: run.measure.clone(),
                on_walk: None,
                strict: run.strict,
                column_source: None,
                expected_count: None,
                in_scope: false,
            };
            force_one(&inner, None)
        }
        Err(_) => match (run.strict, &term.node) {
            // a fold over a list (a matrix inverse's four pivots) is value work, not a body
            (true, Node::Loop(id)) if term.arena().bindings(term.arena().loop_node(*id).clauses).iter().all(|c| matches!(term.arena().ty(term.arena().row(c.term).ty), Ty::List(_))) => eval_term(run, residual, term),
            (true, _) if numeric_loop(term) => Err(kernel_form_error()),
            (false, _) if numeric_loop(term) => eval_term(run, residual, term),
            _ => uniform(run, dispatch, &Value::Residual(E::residual::with_bindings(residual, term, &[])?)),
        },
    }
}

fn force_values(run: &Run, value: &Value, residual: &dyn Fn(ResidualId) -> Result<Value, Diagnostic>) -> Result<Value, Diagnostic> {
    if !E::is_live(value) {
        return Ok(value.clone());
    }
    let reference = |v: &Value| E::host::force_reference(v, &run.live, run_state(run), run.elems.as_deref(), run.resolve.clone());
    match value {
        Value::Residual(r) => {
            let forced = residual(*r)?;
            force_values(run, &forced, residual)
        }
        value if value.is_data_source() => {
            let forced = reference(value)?;
            force_values(run, &forced, residual)
        }
        Value::List(values) => {
            let mut out = Vec::with_capacity(values.len());
            for v in values.iter() {
                out.push(force_values(run, v, residual)?);
            }
            Ok(Value::List(Arc::new(out)))
        }
        Value::Record(fields) => {
            let mut out = Vec::with_capacity(fields.len());
            for (name, v) in fields {
                out.push((name.clone(), force_values(run, v, residual)?));
            }
            Ok(Value::Record(out))
        }
        Value::Struct(g) => {
            let mut out = Vec::with_capacity(g.fields.len());
            for (field, v) in &g.fields {
                out.push((field.clone(), force_values(run, v, residual)?));
            }
            Ok(struct_value(g.name.clone(), g.ty.clone(), out))
        }
        other => Ok(other.clone()),
    }
}

fn uniform(run: &Run, dispatch: &Option<Dispatch>, value: &Value) -> Result<Value, Diagnostic> {
    force_values(run, value, &|residual| {
        let view = E::residual::residual_view(residual)?;
        evaluate(run, dispatch, residual, &view.term)
    })
}

fn execute_request(run: &Run, dispatch: &Option<Dispatch>, residual: ResidualId) -> Option<Value> {
    let view = E::residual::residual_view(residual).ok()?;
    let term = view.term.clone();
    let active = REQUESTS.with(|stack| stack.borrow().iter().any(|(t, other)| same_term(t, &term) && same_context(&view, other)));
    if active {
        if run.strict {
            PENDING.with(|p| *p.borrow_mut() = Some(value::fail("E_KERNEL_FORM", "A nested numeric kernel request cannot be compiled.")));
        }
        return None;
    }
    REQUESTS.with(|stack| stack.borrow_mut().push((term.clone(), view)));
    let _pop = PopGuard(&REQUESTS);
    match evaluate(run, dispatch, residual, &term) {
        Ok(value) => Some(value),
        Err(d) if run.strict => {
            PENDING.with(|p| *p.borrow_mut() = Some(d));
            None
        }
        Err(_) => None,
    }
}

fn force_one(run: &Run, dispatch: Option<Dispatch>) -> Result<Value, Diagnostic> {
    match run_all(run, dispatch, None)? {
        values if values.len() == 1 => Ok(values.into_iter().next().unwrap()),
        _ => Err(value::fail("E_KERNEL", "Program has several outputs; use force_all.")),
    }
}

fn run_all(run: &Run, dispatch: Option<Dispatch>, plane_outputs: PlaneOutputs) -> Result<Vec<Value>, Diagnostic> {
    let t = &run.t;
    let mut run = run.clone();
    run.in_scope = if run.elems.is_some() { false } else { scope_enter(&run) };
    let _scope = ScopeGuard(run.in_scope);
    let has_planes = plane_outputs.is_some();
    let mut plane_outputs = plane_outputs;
    let handlers = {
        let (r1, d1) = (run.clone(), dispatch.clone());
        let (r2, d2) = (run.clone(), dispatch.clone());
        E::ResidualHandlers {
            kernel: Some(Arc::new(move |id| execute_request(&r1, &d1, id))),
            bulk: Some(Arc::new(move |id| execute_request(&r2, &d2, id))),
            ..Default::default()
        }
    };
    let result: RefCell<Option<Result<Vec<Value>, Diagnostic>>> = RefCell::new(None);
    {
        let mut work = || -> Result<(), Diagnostic> {
            let outcome = (|| -> K<Vec<Value>> {
                let eval = |r: ResidualId, term: &Arc<Term>| evaluate(&run, &dispatch, r, term);
                let uni = |v: &Value| uniform(&run, &dispatch, v);
                let seed = match &t.result {
                    RegionResult::Accumulate(seed_term, _) => Some(eval(t.residual, seed_term)?),
                    _ => None,
                };
                let planes = plane_outputs.take();
                run_typed(&run, &seed, &eval, &uni, planes)
            })();
            let flat = match &outcome {
                Ok(_) => Ok(()),
                Err(KErr::Unsupported) => Err(value::fail("E_KERNEL", "Kernel needs reference evaluation.")),
                Err(KErr::Diag(d)) => Err(d.clone()),
            };
            *result.borrow_mut() = Some(match outcome {
                Ok(v) => Ok(v),
                Err(KErr::Unsupported) => Err(value::fail("E_KERNEL", "Kernel needs reference evaluation.")),
                Err(KErr::Diag(d)) => Err(d),
            });
            flat
        };
        let mut handled = || -> Result<E::Eval, Diagnostic> {
            let _ = work();
            Err(value::fail("$handled", ""))
        };
        let _ = E::transaction(&run.state, &mut || {
            let _ = E::residual::handle(handlers.clone(), &mut handled);
            match &*result.borrow() {
                Some(Err(d)) => Err(d.clone()),
                _ => Ok(()),
            }
        });
    }
    let result = result.into_inner().unwrap_or_else(|| Err(value::fail("E_KERNEL", "Kernel needs reference evaluation.")));
    let result = match (take_pending(), result) {
        (Some(d), _) => Err(d),
        (None, r) => r,
    };
    match &result {
        Ok(_) => result,
        Err(d) if d.code == "cancelled" => result,
        Err(_) if run.strict || has_planes || t.outputs.len() > 1 => result,
        Err(_) => {
            let started = run.measure.as_ref().map(|m| (m.clock)());
            let reference = reference_observed(&run, None);
            if let (Some(m), Some(started)) = (&run.measure, started) {
                (m.report)(t, seconds_since(m, started), true);
            }
            reference.map(|v| vec![v])
        }
    }
}

fn reference_observed(run: &Run, on_walk: Option<&Arc<dyn Fn() + Send + Sync>>) -> Result<Value, Diagnostic> {
    if let Some(f) = on_walk.or(run.on_walk.as_ref()) {
        f();
    }
    E::residual::eval_term(run.t.residual, &run.t.term, &run.live, run_state(run), run.elems.as_deref(), run.resolve.clone())
}

// ---------------------------------------------------------------------------------------------
// Public execution API

/// Optional arguments of [`force`], [`force_all`] and [`force_into`].
#[derive(Clone, Default)]
pub struct ForceOptions {
    pub dispatch: Option<Dispatch>,
    pub state: Option<State>,
    pub elems: Option<Vec<(String, Value)>>,
    pub resolve: Option<Resolve>,
    pub measure: Option<Measure>,
    pub on_walk: Option<Arc<dyn Fn() + Send + Sync>>,
    /// Returns kernel errors directly instead of rerunning the reference interpreter.
    pub strict: bool,
}

fn make_run(t: &T, live: &Live, o: &ForceOptions, column_source: Option<ColumnSource>, expected_count: Option<usize>) -> Run {
    Run {
        t: t.clone(),
        live: live.clone(),
        state: o.state.clone().unwrap_or_else(E::create_state),
        elems: o.elems.clone(),
        resolve: o.resolve.clone(),
        measure: o.measure.clone(),
        on_walk: o.on_walk.clone(),
        strict: o.strict,
        column_source,
        expected_count,
        in_scope: false,
    }
}

/// The single-output accessor: refuses a program with several outputs.
pub fn force(t: &T, live: &Live, o: &ForceOptions) -> Result<Value, Diagnostic> {
    let run = make_run(t, live, o, None, None);
    match run_all(&run, o.dispatch.clone(), None)? {
        values if values.len() == 1 => Ok(values.into_iter().next().unwrap()),
        _ => Err(value::fail("E_KERNEL", "Program has several outputs; use force_all.")),
    }
}

/// One packed array per output, in output order. A program with several outputs only collects and
/// has no reference fallback.
pub fn force_all(t: &T, live: &Live, o: &ForceOptions) -> Result<Vec<Value>, Diagnostic> {
    let run = make_run(t, live, o, None, None);
    run_all(&run, o.dispatch.clone(), None)
}

/// Execute a Collect region into caller-owned contiguous output planes. Borrowed source columns
/// retain their offset/stride and are never written. `None` sources use ordinary packed
/// materialization; `None` output lanes skip writes while retaining arithmetic and finite checks.
/// Every plane must have `count` elements.
pub fn force_into(
    t: &T,
    live: &Live,
    o: &ForceOptions,
    column_source: &ColumnSource,
    count: i64,
    outputs: &mut Vec<Vec<Option<Vec<f64>>>>,
) -> Result<(), Diagnostic> {
    let run = make_run(t, live, o, Some(column_source.clone()), Some(count.max(0) as usize));
    let mut planes: Vec<Vec<Option<&mut [f64]>>> =
        outputs.iter_mut().map(|row| row.iter_mut().map(|p| p.as_mut().map(|v| v.as_mut_slice())).collect()).collect();
    run_all(&run, o.dispatch.clone(), Some(planes.as_mut_slice())).map(|_| ())
}

/// The reference interpreter over the region's own term.
pub fn reference(t: &T, live: &Live, state: Option<&State>, elems: Option<&[(String, Value)]>, resolve: Option<Resolve>) -> Result<Value, Diagnostic> {
    E::residual::eval_term(t.residual, &t.term, live, state, elems, resolve)
}

// ---------------------------------------------------------------------------------------------
// Host: what a device or another host needs from a region

pub mod host {
    use super::*;
    use std::collections::HashSet;

    pub use super::{ordered_add, finite_checks, output_reachable, reference};

    /// No outermost run is sharing results by binding identity on this thread, so an array that dies
    /// between two regions cannot be found again through a scope entry.
    pub fn scope_idle() -> bool {
        CURRENT.with(|c| c.borrow().is_none())
    }

    fn argument_index(v: &Value) -> Result<Option<(Ty, i64)>, Diagnostic> {
        match v {
            Value::Source(g) if g.name == "$core/argument" => match g.fields.as_slice() {
                [(f, Value::Int(index))] if f == "index" => Ok(Some((g.ty.clone(), *index))),
                _ => Err(value::fail("E_CORE_ARGUMENT", "Invalid compiled argument declaration.")),
            },
            _ => Ok(None),
        }
    }

    /// Declared types of the structured Core arguments captured by this region.
    pub fn argument_types(t: &T) -> Result<Vec<Ty>, Diagnostic> {
        let mut types: HashMap<i64, Ty> = HashMap::new();
        let mut visited: HashSet<(i64, i64)> = HashSet::new();
        fn value(v: &Value, types: &mut HashMap<i64, Ty>, visited: &mut HashSet<(i64, i64)>) -> Result<(), Diagnostic> {
            if let Some((ty, index)) = argument_index(v)? {
                if index < 0 {
                    return Err(value::fail("E_CORE_ARGUMENT", "Invalid compiled argument index."));
                }
                match types.get(&index) {
                    Some(old) if *old != ty => return Err(value::fail("E_CORE_ARGUMENT", "Compiled argument types disagree.")),
                    _ => {
                        types.insert(index, ty);
                    }
                }
                return Ok(());
            }
            match v {
                Value::Residual(r) => residual(*r, types, visited)?,
                Value::Fn(f) => {
                    for (_, v) in E::residual::function_bindings(*f) {
                        value(&v, types, visited)?;
                    }
                }
                Value::List(values) => {
                    for v in values.iter() {
                        value(v, types, visited)?;
                    }
                }
                v @ (Value::Record(_) | Value::Struct(_) | Value::Source(_)) => {
                    for (_, v) in v.geo_fields().unwrap() {
                        value(v, types, visited)?;
                    }
                }
                _ => {}
            }
            Ok(())
        }
        fn residual(r: ResidualId, types: &mut HashMap<i64, Ty>, visited: &mut HashSet<(i64, i64)>) -> Result<(), Diagnostic> {
            let id = (r.1, r.0);
            if visited.insert(id) {
                for (_, v) in E::residual::residual_view(r)?.bindings.iter() {
                    value(v, types, visited)?;
                }
            }
            Ok(())
        }
        residual(t.residual, &mut types, &mut visited)?;
        for v in &t.uniforms {
            value(v, &mut types, &mut visited)?;
        }
        for (r, _) in &t.sources {
            residual(*r, &mut types, &mut visited)?;
        }
        for (r, _) in &t.stages {
            residual(*r, &mut types, &mut visited)?;
        }
        (0..types.len() as i64)
            .map(|index| types.get(&index).cloned().ok_or_else(|| value::fail("E_CORE_ARGUMENT", "Compiled argument indices must be contiguous.")))
            .collect()
    }

    /// Borrow checked immutable Core arguments in place of typed source markers. Refuses wrong
    /// arity/storage; rewrites closure and residual captures without re-evaluating their checked
    /// bodies. Shared residuals are copied once per bind, preserving their DAG sharing; no argument
    /// or rewritten capture is retained across binds. `validate: false` skips the finite-value scan,
    /// for a device that checks what it uploads (a resident fold's placeholders are zeros).
    pub fn bind_arguments(t: &T, arguments: &[Value], validate: Option<bool>) -> Result<T, Diagnostic> {
        let validate = validate.unwrap_or(true);
        let types = argument_types(t)?;
        if types.len() != arguments.len() {
            return Err(value::fail("E_CORE_ARGUMENT", "Compiled region argument count differs from its declaration."));
        }
        for (index, v) in arguments.iter().enumerate() {
            if value::ty_of(v) != types[index] {
                return Err(value::fail("E_CORE_ARGUMENT", "Compiled region argument storage differs from its declared type."));
            }
            if validate {
                value::validate(v)?;
            }
        }
        if types.is_empty() {
            return Ok(t.clone());
        }
        // One rewrite of every capture together keeps the sharing between them.
        let mut all = vec![Value::Residual(t.residual)];
        all.extend(t.uniforms.iter().cloned());
        all.extend(t.sources.iter().map(|(r, _)| Value::Residual(*r)));
        all.extend(t.stages.iter().map(|(r, _)| Value::Residual(*r)));
        let replace = |v: &Value| -> Option<Value> {
            match argument_index(v) {
                Ok(Some((_, index))) => Some(arguments[index as usize].clone()),
                _ => None,
            }
        };
        let (rewritten, _roots) = E::host::rewrite_owned(&Value::List(Arc::new(all)), &replace);
        let Value::List(rewritten) = rewritten else {
            return Err(value::fail("E_CORE_ARGUMENT", "Compiled region rewrite changed shape."));
        };
        let residual_of = |v: &Value| -> Result<ResidualId, Diagnostic> {
            match v {
                Value::Residual(r) => Ok(*r),
                _ => Err(value::fail("E_CORE_ARGUMENT", "Compiled region rewrite lost a residual.")),
            }
        };
        let residual = residual_of(&rewritten[0])?;
        let uniforms: Vec<Value> = rewritten[1..1 + t.uniforms.len()].to_vec();
        let mut at = 1 + t.uniforms.len();
        let mut sources = Vec::with_capacity(t.sources.len());
        for (_, term) in &t.sources {
            sources.push((residual_of(&rewritten[at])?, term.clone()));
            at += 1;
        }
        let mut stages = Vec::with_capacity(t.stages.len());
        for (_, term) in &t.stages {
            stages.push((residual_of(&rewritten[at])?, term.clone()));
            at += 1;
        }
        region::private::with_captures(t, residual, uniforms, sources, stages, t.count)
    }

    #[derive(Clone, Debug)]
    pub struct View {
        pub code: Vec<Instruction>,
        pub widths: Vec<i64>,
        pub outputs: Vec<Vec<i64>>,
        pub uniform_widths: Vec<usize>,
        pub collecting: bool,
    }

    pub fn view(t: &T) -> View {
        let mut uniform_widths = vec![1usize; t.uniforms.len()];
        fn instruction(i: &Instruction, widths: &mut [usize]) {
            match i {
                Instruction::Uniform(index, component) => {
                    let w = &mut widths[*index as usize];
                    *w = (*w).max(*component as usize + 1);
                }
                Instruction::Guarded(_, body) => instruction(body, widths),
                _ => {}
            }
        }
        for i in t.code.iter() {
            instruction(i, &mut uniform_widths);
        }
        View {
            code: (*t.code).clone(),
            widths: (*t.widths).clone(),
            outputs: (*t.outputs).clone(),
            uniform_widths,
            collecting: matches!(t.result, RegionResult::Collect),
        }
    }

    #[derive(Clone, Debug)]
    pub enum Shape {
        Value(Value),
        Range(Value, i64, i64),
        Opaque(Option<Vec<Value>>),
    }

    fn binding_of(r: ResidualId, name: &str) -> Option<Value> {
        E::residual::residual_binding(r, name)
    }

    // What a source or uniform reads: a value itself; `array/range` of an array's count times `mul`
    // over `div`; or a term over the listed values (`None`: a form not traversed here).
    fn shape(residual: ResidualId, term: &Term) -> Shape {
        // `acc` holds the OCaml list reversed: a cons is a push.
        fn refs(r: ResidualId, term: &Term, acc: Option<Vec<Value>>) -> Option<Vec<Value>> {
            let mut acc = acc?;
            match &term.node {
                Node::Lit(_) | Node::Text(_) | Node::Nil | Node::Time | Node::FnRef(_) => Some(acc),
                Node::RefBinding(id) => match binding_of(r, term.name(term.arena().ref_binding(*id).name)) {
                    Some(Value::Residual(inner)) => {
                        let term = E::residual::residual_term(inner)?;
                        refs(inner, &term, Some(acc))
                    }
                    Some(v) => {
                        acc.push(v);
                        Some(acc)
                    }
                    None => None,
                },
                Node::Op(_) | Node::Call(_) | Node::Vec(_) => {
                    let mut acc = Some(acc);
                    for id in term.children() {
                        acc = refs(r, &term.child(*id), acc);
                    }
                    acc
                }
                Node::If(id) => {
                    let node = term.arena().if_node(*id);
                    refs(r, &term.child(node.test), refs(r, &term.child(node.yes), refs(r, &term.child(node.no), Some(acc))))
                }
                _ => None,
            }
        }
        let value_of = |r: ResidualId, term: &Term| -> Option<Value> {
            match &term.node {
                Node::RefBinding(id) if term.arena().name_ids(term.arena().ref_binding(*id).fields).is_empty() => match binding_of(r, term.name(term.arena().ref_binding(*id).name)) {
                    Some(Value::Residual(_)) | None => None,
                    Some(v) => Some(v),
                },
                _ => None,
            }
        };
        let int = |term: &Term| -> Option<i64> {
            match &term.node {
                Node::Lit(id) => match term.arena().literal(*id) { flow_check::term_arena::Literal::Int(k) if k > 0 => Some(k), _ => None },
                _ => None,
            }
        };
        let count = |term: &Term| -> Option<Value> {
            match &term.node {
                Node::Op(id) if term.name(term.arena().op(*id).op) == "array/count" && term.arena().fields(term.arena().op(*id).args).len() == 1 => value_of(residual, &term.child(term.arena().fields(term.arena().op(*id).args)[0].term)),
                _ => None,
            }
        };
        let scaled = |term: &Term| -> Option<(Value, i64, i64)> {
            let Node::Op(id) = term.node else { return None };
            let node = term.arena().op(id);
            let op = term.name(node.op);
            let args = term.arena().fields(node.args);
            match op {
                "array/count" => count(term).map(|v| (v, 1, 1)),
                "*" | "int/mul" if args.len() == 2 => {
                    let (a, b) = (term.child(args[0].term), term.child(args[1].term));
                    match (count(&a), int(&b), count(&b), int(&a)) {
                        (Some(v), Some(k), _, _) => Some((v, k, 1)),
                        (_, _, Some(v), Some(k)) => Some((v, k, 1)),
                        _ => None,
                    }
                }
                "int/div" if args.len() == 2 => match (count(&term.child(args[0].term)), int(&term.child(args[1].term))) {
                    (Some(v), Some(k)) => Some((v, 1, k)),
                    _ => None,
                },
                _ => None,
            }
        };
        if let Some(v) = value_of(residual, term) {
            return Shape::Value(v);
        }
        if let Node::Op(id) = term.node {
            let node = term.arena().op(id);
            let args = term.arena().fields(node.args);
            if term.name(node.op) == "array/range" && matches!(args.first(), Some(f) if term.name(f.name) == "count") && (*term.ty() == Ty::Array(Box::new(Ty::Int)) || *term.ty() == Ty::Array(Box::new(Ty::Float))) {
                if let Some((v, mul, div)) = scaled(&term.child(args[0].term)) {
                    return Shape::Range(v, mul, div);
                }
            }
        }
        Shape::Opaque(refs(residual, term, Some(Vec::new())).map(|mut v| {
            v.reverse();
            v
        }))
    }

    pub fn source_shapes(t: &T) -> Vec<Shape> {
        t.sources.iter().map(|(r, term)| shape(*r, term)).collect()
    }

    // `array/range n` (through bindings): 0, 1, ... a device makes from the element index
    fn index_range(residual: ResidualId, term: &Term) -> Option<(ResidualId, Term)> {
        match &term.node {
            Node::RefBinding(id) if term.arena().name_ids(term.arena().ref_binding(*id).fields).is_empty() => match binding_of(residual, term.name(term.arena().ref_binding(*id).name)) {
                Some(Value::Residual(r)) => {
                    let term = E::residual::residual_term(r)?;
                    index_range(r, &term)
                }
                _ => None,
            },
            Node::Op(id) => {
                let op = term.arena().op(*id);
                let args = term.arena().fields(op.args);
                if term.name(op.op) != "array/range"
                    || !matches!(args.first(), Some(f) if term.name(f.name) == "count")
                    || !matches!(term.ty(), Ty::Array(inner) if matches!(**inner, Ty::Float | Ty::Int)) { return None; }
                Some((residual, term.child(args[0].term)))
            }
            _ => None,
        }
    }

    /// Sources that are `array/range n` (`[0, 1, ...]`, float or int): a device binds its index buffer.
    pub fn index_sources(t: &T) -> Vec<bool> {
        t.sources.iter().map(|(r, term)| index_range(*r, term).is_some()).collect()
    }

    pub fn uniform_shapes(t: &T) -> Vec<Shape> {
        t.uniforms
            .iter()
            .map(|v| match v {
                Value::Residual(r) => match E::residual::residual_term(*r) {
                    Some(term) => shape(*r, &term),
                    None => Shape::Opaque(None),
                },
                v => Shape::Value(v.clone()),
            })
            .collect()
    }

    /// Borrowed immutable arrays for downstream compilation. `tables[u]` is array-valued uniform `u`
    /// flattened (empty for a scalar uniform), the table a gather reads; integer arrays and scalars
    /// are floats.
    #[derive(Clone, Debug)]
    pub struct Inputs {
        pub arrays: Vec<Arc<Vec<f64>>>,
        pub uniforms: Vec<Vec<f64>>,
        pub tables: Vec<Arc<Vec<f64>>>,
        pub frame: Vec<f64>,
        pub count: i64,
    }

    #[derive(Clone, Debug)]
    pub struct ValueInputs {
        pub arrays: Vec<Value>,
        pub uniforms: Vec<Value>,
        pub frame: Vec<Value>,
        pub count: i64,
    }

    fn forwards(p: &T) -> bool {
        p.sources.len() == 1
            && p.count.is_none()
            && p.iteration == Iteration::Zip
            && p.result == RegionResult::Collect
            && p.skip.is_empty()
            && p.outputs.len() == 1
            && p.sources[0].1.ty() == p.term.ty()
            && p.outputs[0].len() as i64 == p.widths[0]
            && p.outputs[0].iter().enumerate().all(|(c, r)| matches!(Instruction::unguarded(&p.code[*r as usize]), Instruction::Input(0, w, c2) if *w == p.widths[0] && *c2 == c as i64))
    }

    pub type SourceFn = Arc<dyn Fn(ResidualId, &Term) -> Result<Option<Value>, Diagnostic> + Send + Sync>;

    /// Optional arguments of [`prepare_values`] and [`prepare`].
    #[derive(Clone, Default)]
    pub struct PrepareOptions {
        /// the approximate tier: integer arrays and scalars travel as exact-to-2^24 floats
        pub fast: bool,
        pub state: Option<State>,
        pub resolve: Option<Resolve>,
        pub measure: Option<Measure>,
        /// source `i`'s length instead of building it (an Int or Float range the caller binds on the
        /// device); its array is then empty
        pub lengths: Option<Arc<dyn Fn(usize) -> Option<usize> + Send + Sync>>,
        pub capacity: Option<Arc<dyn Fn(&Value) -> Option<usize> + Send + Sync>>,
        /// an `index_sources` source is not built either, its length is its count
        pub index: bool,
        /// for a host source `v` the resolve would answer, data the caller holds on the device: its
        /// placeholder array and element count
        pub resident: Option<Arc<dyn Fn(&Value) -> Option<(Value, i64)> + Send + Sync>>,
        /// an immutable cached source; its storage type is checked as usual
        pub source: Option<SourceFn>,
    }

    #[derive(Clone)]
    struct Pv {
        t: T,
        live: Live,
        state: State,
        measure: Option<Measure>,
    }

    fn pv_run(pv: &Pv, resolve: &Option<Resolve>) -> Run {
        Run {
            t: pv.t.clone(),
            live: pv.live.clone(),
            state: pv.state.clone(),
            elems: None,
            resolve: resolve.clone(),
            measure: pv.measure.clone(),
            on_walk: None,
            strict: false,
            column_source: None,
            expected_count: None,
            in_scope: false,
        }
    }

    fn with_kernel(kernel: Arc<dyn Fn(ResidualId) -> Option<Value> + Send + Sync>, f: &mut dyn FnMut() -> Result<Value, Diagnostic>) -> Result<Value, Diagnostic> {
        let mut result: Option<Result<Value, Diagnostic>> = None;
        let handlers = E::ResidualHandlers { kernel: Some(kernel), ..Default::default() };
        let _ = E::residual::handle(handlers, &mut || {
            result = Some(f());
            Err(value::fail("$handled", ""))
        });
        result.unwrap_or_else(|| Err(value::fail("E_KERNEL", "Kernel inputs need reference evaluation.")))
    }

    /// A source or request compiled as the consumer was: captures as uniforms, then (as `compile_result`
    /// does) as constants for a body that needs one (a noise seed), live ones staying uniforms. Without
    /// the retry such a source is interpreted on the host, element by element, every frame.
    fn compile_source(t: &T, residual: ResidualId, term: &Term) -> Result<T, Vec<Diagnostic>> {
        compile_impl(false, t.fusion, t.dynamic, &t.count_source, residual, term)
            .or_else(|e| if t.dynamic { compile_impl(false, t.fusion, false, &t.count_source, residual, term) } else { Err(e) })
    }

    fn pv_eval_term(pv: &Pv, resolve: &Option<Resolve>, residual: ResidualId, term: &Term) -> Result<Value, Diagnostic> {
        let (p, r) = (pv.clone(), resolve.clone());
        with_kernel(Arc::new(move |id| pv_execute_request(&p, &r, id)), &mut || {
            E::residual::eval_term(residual, term, &pv.live, Some(&pv.state), None, resolve.clone())
        })
    }

    fn pv_execute_request(pv: &Pv, resolve: &Option<Resolve>, residual: ResidualId) -> Option<Value> {
        if E::state_dependent(&Value::Residual(residual)) {
            return None;
        }
        let term = E::residual::residual_term(residual)?;
        match compile_source(&pv.t, residual, &term) {
            Ok(program) => pv_force(pv, resolve, &program).ok(),
            Err(_) => None,
        }
    }

    fn pv_force(pv: &Pv, resolve: &Option<Resolve>, program: &T) -> Result<Value, Diagnostic> {
        let mut run = pv_run(pv, resolve);
        run.t = program.clone();
        force_one(&run, None)
    }

    fn pv_evaluate_with(pv: &Pv, resolve: &Option<Resolve>, residual: ResidualId, term: &Term) -> Result<Value, KErrOrDiag> {
        if let Node::RefBinding(id) = term.node {
            let binding = term.arena().ref_binding(id);
            if term.arena().name_ids(binding.fields).is_empty() {
                if let Some(Value::Residual(inner)) = binding_of(residual, term.name(binding.name)) {
                    let view = E::residual::residual_view(inner)?;
                    return pv_evaluate_with(pv, resolve, inner, &view.term);
                }
            }
        }
        pv_materialize(pv, resolve, residual, term)
    }

    fn pv_materialize(pv: &Pv, resolve: &Option<Resolve>, residual: ResidualId, term: &Term) -> Result<Value, KErrOrDiag> {
        match compile_source(&pv.t, residual, term) {
            Ok(program) if forwards(&program) => {
                // a region that only copies its one source (a branch chosen by constants) is that
                // source: the same array, which a device holding it does not upload again
                let (r, source_term) = program.sources[0].clone();
                pv_evaluate_with(pv, resolve, r, &source_term)
            }
            Ok(program) => Ok(pv_force(pv, resolve, &program)?),
            Err(_) => Ok(pv_eval_term(pv, resolve, residual, term)?),
        }
    }

    type KErrOrDiag = KErr;

    // An integer array's float copy, kept by the array's identity (8 arrays, charged to the ledger):
    // a source or table made once per topology (a CSR, a selection) is converted once, and stays the
    // array a device's identity check finds uploaded, instead of a fresh copy uploaded with every
    // prepare.
    static INT_FLOATS: LazyLock<lru::IdentityList> = LazyLock::new(|| {
        let same = |stored: &lru::Key, probe: &(dyn std::any::Any + Send + Sync)| {
            matches!((stored.as_any().downcast_ref::<Arc<Vec<i64>>>(), probe.downcast_ref::<Arc<Vec<i64>>>()), (Some(a), Some(b)) if Arc::ptr_eq(a, b))
        };
        lru::IdentityList::new(lru::cache("flow_ir.int_floats"), 8, Arc::new(same))
    });

    // Backend-owned integer arrays travel through the numeric input carrier by identity. Both
    // sides are weak: this table neither owns device storage nor retains an expired placeholder.
    static INTEGER_INPUTS: Mutex<Vec<(std::sync::Weak<Vec<i64>>, std::sync::Weak<Vec<f64>>)>> = Mutex::new(Vec::new());

    pub fn register_integer_input(values: &Arc<Vec<i64>>, input: &Arc<Vec<f64>>) {
        let mut all = INTEGER_INPUTS.lock().unwrap();
        all.retain(|(v, p)| v.strong_count() > 0 && p.strong_count() > 0);
        all.push((Arc::downgrade(values), Arc::downgrade(input)));
    }

    fn int_floats(values: &Arc<Vec<i64>>) -> Result<Arc<Vec<f64>>, Diagnostic> {
        if let Some(input) = INTEGER_INPUTS.lock().unwrap().iter().find_map(|(v, p)| {
            v.upgrade().filter(|v| Arc::ptr_eq(v, values)).and_then(|_| p.upgrade())
        }) { return Ok(input); }
        if let Some(floats) = INT_FLOATS.find_by(values).and_then(|found| found.downcast::<Vec<f64>>().ok()) {
            return Ok(floats);
        }
        let bytes = values.len() * 8;
        if bytes >= 1 << 20 {
            mem::admit_bytes(mem::Pool::Host, bytes as i64).map_err(|r| value::fail("E_ARRAY_MEMORY", &r.to_string()))?;
        }
        let floats = Arc::new(values.iter().map(|n| *n as f64).collect::<Vec<f64>>());
        INT_FLOATS.add(lru::Key::from_value(Arc::new(values.clone())), floats.clone(), bytes as i64);
        Ok(floats)
    }

    /// A numeric array as a device input's floats (an integer array's copy kept by its identity).
    pub fn floats(v: &Value) -> Result<Option<Arc<Vec<f64>>>, Diagnostic> {
        Ok(match v {
            Value::Floats(1..=4, values) => Some(values.clone()),
            Value::Int_array(values) if values.is_empty() => Some(Arc::new(Vec::new())),
            Value::Int_array(values) => Some(int_floats(values)?),
            _ => None,
        })
    }

    pub fn prepare_values(t: &T, o: &PrepareOptions, live: &Live) -> Result<ValueInputs, Diagnostic> {
        let state = o.state.clone().unwrap_or_else(E::create_state);
        let pv = Pv { t: t.clone(), live: live.clone(), state: state.clone(), measure: o.measure.clone() };
        let outcome: RefCell<Option<Result<ValueInputs, KErr>>> = RefCell::new(None);
        let _ = E::transaction(&state, &mut || {
            let r = prepare_values_inner(&pv, o);
            let failure = match &r {
                Ok(_) => None,
                Err(KErr::Unsupported) => Some(value::fail("E_KERNEL", "Kernel inputs need reference evaluation.")),
                Err(KErr::Diag(d)) => Some(d.clone()),
            };
            *outcome.borrow_mut() = Some(r);
            match failure {
                Some(d) => Err(d),
                None => Ok(()),
            }
        });
        match outcome.into_inner() {
            Some(Ok(v)) => Ok(v),
            Some(Err(KErr::Unsupported)) | None => Err(value::fail("E_KERNEL", "Kernel inputs need reference evaluation.")),
            Some(Err(KErr::Diag(d))) => Err(d),
        }
    }

    fn prepare_values_inner(pv: &Pv, o: &PrepareOptions) -> K<ValueInputs> {
        let t = &pv.t;
        let live = &pv.live;
        let resolve = &o.resolve;
        let evaluate = |residual: ResidualId, term: &Term| pv_evaluate_with(pv, resolve, residual, term);
        // A source that is one read of data the device holds (`resident`) is that data's placeholder,
        // bound on the device, with its count: tried with a resolve answering placeholders, kept only
        // when the source is exactly the one placeholder it was handed (anything computed over a
        // placeholder is evaluated again with the caller's resolve).
        let mut placed: Vec<Option<usize>> = vec![None; t.sources.len()];
        let mut evaluate_source = |i: usize, residual: ResidualId, term: &Term| -> K<Value> {
            match (&o.resident, resolve) {
                (Some(resident), Some(resolve)) => {
                    let handed: Arc<Mutex<Vec<(Value, i64)>>> = Arc::new(Mutex::new(Vec::new()));
                    let speculative: Resolve = {
                        let (resident, resolve, handed) = (resident.clone(), resolve.clone(), handed.clone());
                        Arc::new(move |v: &Value| match resident(v) {
                            Some((p, n)) => {
                                handed.lock().unwrap().push((p.clone(), n));
                                Ok(p)
                            }
                            None => resolve(v),
                        })
                    };
                    let some_resolve = Some(speculative);
                    let attempt = pv_evaluate_with(pv, &some_resolve, residual, term);
                    let handed = handed.lock().unwrap().clone();
                    match attempt {
                        Ok(value) if handed.is_empty() => Ok(value),
                        Ok(value) => {
                            if handed.len() == 1 && same_value(&value, &handed[0].0) {
                                placed[i] = Some(handed[0].1 as usize);
                                Ok(value)
                            } else {
                                evaluate(residual, term)
                            }
                        }
                        Err(_) if !handed.is_empty() => evaluate(residual, term),
                        Err(e) => Err(e),
                    }
                }
                _ => evaluate(residual, term),
            }
        };
        let mut empty_product = false;
        // a source whose length the caller knows (a range it binds on the device) is not built
        let mut known: Vec<Option<usize>> = Vec::with_capacity(t.sources.len());
        for (i, (residual, term)) in t.sources.iter().enumerate() {
            let n = match (o.lengths.as_ref().and_then(|f| f(i)), o.index) {
                (Some(n), _) => Some(n),
                (None, false) => None,
                (None, true) => match index_range(*residual, term) {
                    Some((r, n)) => match evaluate(r, &n)? {
                        value @ Value::Int(_) => {
                            let limit = MAX_ARRAY;
                            match op::array_count(Some(limit), "array/range", &value) {
                                Ok(n) => Some(n),
                                Err(_) => {
                                    pv_eval_term(pv, resolve, *residual, term)?;
                                    return Err(KErr::Unsupported);
                                }
                            }
                        }
                        _ => return Err(KErr::Unsupported),
                    },
                    None => None,
                },
            };
            known.push(n);
        }
        let mut arrays: Vec<Value> = Vec::with_capacity(t.sources.len());
        for (i, (residual, term)) in t.sources.iter().enumerate() {
            let value = if known[i].is_some() {
                match term.ty() {
                    Ty::Array(inner) if **inner == Ty::Int => Value::Int_array(Arc::new(Vec::new())),
                    Ty::Array(inner) if **inner == Ty::Float => Value::Floats(1, Arc::new(Vec::new())),
                    _ => return Err(KErr::Unsupported),
                }
            } else if empty_product {
                match term.ty() {
                    Ty::Array(inner) => match **inner {
                        Ty::Int => Value::Int_array(Arc::new(Vec::new())),
                        Ty::Text => Value::Text_array(Arc::new(rays_math::text_array::TextArray::new())),
                        Ty::Float => Value::Floats(1, Arc::new(Vec::new())),
                        Ty::Vec2 => Value::Floats(2, Arc::new(Vec::new())),
                        Ty::Vec3 => Value::Floats(3, Arc::new(Vec::new())),
                        Ty::Vec4 => Value::Floats(4, Arc::new(Vec::new())),
                        _ => return Err(KErr::Unsupported),
                    },
                    _ => return Err(KErr::Unsupported),
                }
            } else {
                match o.source.as_ref().map(|f| f(*residual, term)).transpose()?.flatten() {
                    Some(v) => v,
                    None => evaluate_source(i, *residual, term)?,
                }
            };
            let measured = match o.capacity.as_ref().and_then(|f| f(&value)) {
                Some(n) => n,
                None => array_len_k(&value)?,
            };
            if t.iteration == Iteration::Product && measured == 0 && known[i].is_none() {
                empty_product = true;
            }
            arrays.push(value);
        }
        let length = |i: usize, value: &Value| -> K<usize> {
            match (known[i], placed[i]) {
                (Some(n), _) | (None, Some(n)) => Ok(n),
                (None, None) => match o.capacity.as_ref().and_then(|f| f(value)) {
                    Some(n) => Ok(n),
                    None => array_len_k(value),
                },
            }
        };
        for (index, value) in arrays.iter().enumerate() {
            if value::ty_of(value) != *t.sources[index].1.ty() {
                return Err(fail("E_ARRAY_TYPE", "Kernel input changed its packed element type."));
            }
        }
        let count = if arrays.is_empty() || empty_product {
            0
        } else {
            let mut count: i64 = if t.iteration == Iteration::Zip { i64::MAX } else { 1 };
            for (i, value) in arrays.iter().enumerate() {
                let length = length(i, value)? as i64;
                count = match t.iteration {
                    Iteration::Zip => count.min(length),
                    Iteration::Product => {
                        if length != 0 && count > i64::MAX / length {
                            return Err(fail("E_ARRAY_RANGE", "Kernel iteration count exceeds native bounds."));
                        }
                        count * length
                    }
                };
            }
            count as usize
        };
        // a residual several uniforms read (a matrix, its inverse) is evaluated once: each uniform's
        // live bindings are their values, made once per call; a binding that fails made eagerly stays
        // as it was, made only where the term reads it. So do the residuals inside a record or list
        // binding: a live matrix's rows, which every read of the matrix made again.
        let made: RefCell<HashMap<(i64, i64), Value>> = RefCell::new(HashMap::new());
        fn uniform_of(pv: &Pv, resolve: &Option<Resolve>, made: &RefCell<HashMap<(i64, i64), Value>>, residual: ResidualId) -> K<Value> {
            let id = (residual.1, residual.0);
            if let Some(v) = made.borrow().get(&id) {
                return Ok(v.clone());
            }
            let view = E::residual::residual_view(residual)?;
            let bindings: Vec<(String, Value)> = view.bindings.iter().map(|(n, v)| (n.to_string(), binding_of_value(pv, resolve, made, v))).collect();
            let residual = if view.bindings.len() == bindings.len() && view.bindings.iter().zip(&bindings).all(|((n, x), (m, y))| n.as_ref() == m && same_value(x, y)) { residual } else { E::residual::with_bindings(residual, &view.term, &bindings)? };
            let value = pv_evaluate_with(pv, resolve, residual, &view.term)?;
            made.borrow_mut().insert(id, value.clone());
            Ok(value)
        }
        fn binding_of_value(pv: &Pv, resolve: &Option<Resolve>, made: &RefCell<HashMap<(i64, i64), Value>>, value: &Value) -> Value {
            match value {
                Value::Residual(r) if !E::state_dependent(value) => uniform_of(pv, resolve, made, *r).unwrap_or_else(|_| value.clone()),
                Value::Record(fields) => {
                    let mapped: Vec<(String, Value)> = fields.iter().map(|(n, v)| (n.clone(), binding_of_value(pv, resolve, made, v))).collect();
                    if same_bindings(fields, &mapped) { value.clone() } else { Value::Record(mapped) }
                }
                Value::List(items) => {
                    let mapped: Vec<Value> = items.iter().map(|v| binding_of_value(pv, resolve, made, v)).collect();
                    if items.iter().zip(mapped.iter()).all(|(a, b)| same_value(a, b)) { value.clone() } else { Value::List(Arc::new(mapped)) }
                }
                _ => value.clone(),
            }
        }
        // a table that is one read of data the device holds (`sop/attr g :P` gathered by index) is its
        // placeholder, bound there as the table, never read back
        let held = |value: &Value| -> Option<Value> {
            match (&o.resident, value) {
                (Some(resident), value) if value.is_data_source() => resident(value).map(|(p, _)| p),
                _ => None,
            }
        };
        let run = pv_run(pv, resolve);
        let mut uniforms = Vec::with_capacity(t.uniforms.len());
        for value in t.uniforms.iter() {
            uniforms.push(match held(value) {
                Some(placeholder) => placeholder,
                None => force_values(&run, value, &|residual| uniform_of(pv, resolve, &made, residual).map_err(|e| match e {
                    KErr::Diag(d) => d,
                    KErr::Unsupported => value::fail("E_KERNEL", "Kernel inputs need reference evaluation."),
                }))?,
            });
        }
        let mut frame = Vec::with_capacity(t.code.len());
        for instruction in t.code.iter() {
            frame.push(match Instruction::unguarded(instruction) {
                Instruction::Frame(name) | Instruction::Int_frame(name) => frame_fact(name, live)?,
                _ => Value::Float(0.0),
            });
        }
        Ok(ValueInputs { arrays, uniforms, frame, count: count as i64 })
    }

    #[derive(Clone, Debug, PartialEq)]
    pub enum Argument {
        Argument(i64),
        Argument_range(i64, i64, i64),
    }

    #[derive(Clone)]
    pub struct Plan {
        /// each source: a Core argument, or `array/range` of one's count times `mul` over `div`
        pub plan_sources: Vec<Argument>,
        /// each uniform: a Core argument
        pub plan_uniforms: Vec<i64>,
        /// frame instructions' values
        pub plan_frames: Vec<(usize, Arc<dyn Fn(&Live) -> f64 + Send + Sync>)>,
    }

    /// On an unbound region (its sources and uniforms still argument markers): its inputs as its Core
    /// arguments decide them, worked out once, so a caller holding the arguments makes
    /// `device_inputs`' result without binding or evaluating; `None` when a source or uniform is any
    /// other form (or the region has no sources, a static count or a product).
    pub fn plan(t: &T) -> Option<Plan> {
        let argument = |v: &Value| -> Option<i64> {
            match argument_index(v) {
                Ok(Some((_, i))) => Some(i),
                _ => None,
            }
        };
        if t.iteration != Iteration::Zip || t.count.is_some() || t.sources.is_empty() {
            return None;
        }
        let mut plan_sources = Vec::new();
        for (r, term) in t.sources.iter() {
            plan_sources.push(match shape(*r, term) {
                Shape::Value(v) => Argument::Argument(argument(&v)?),
                Shape::Range(v, mul, div) => Argument::Argument_range(argument(&v)?, mul, div),
                Shape::Opaque(_) => return None,
            });
        }
        let mut plan_uniforms = Vec::new();
        for v in t.uniforms.iter() {
            let v = match v {
                Value::Residual(r) => match shape(*r, E::residual::residual_term(*r)?.as_ref()) {
                    Shape::Value(v) => v,
                    _ => return None,
                },
                v => v.clone(),
            };
            plan_uniforms.push(argument(&v)?);
        }
        let mut plan_frames: Vec<(usize, Arc<dyn Fn(&Live) -> f64 + Send + Sync>)> = Vec::new();
        for (i, instruction) in t.code.iter().enumerate() {
            match Instruction::unguarded(instruction) {
                Instruction::Frame(name) | Instruction::Int_frame(name) if name == "t" && matches!(Instruction::unguarded(instruction), Instruction::Frame(_)) => {
                    plan_frames.push((i, Arc::new(|live: &Live| live.t)));
                }
                Instruction::Frame(name) | Instruction::Int_frame(name) => {
                    op::find(&[], name, &context::value())?;
                    let name = name.clone();
                    plan_frames.push((
                        i,
                        Arc::new(move |live: &Live| match frame_fact(&name, live).and_then(|v| num_k(&v)) {
                            Ok(x) => x,
                            Err(_) => f64::NAN,
                        }),
                    ));
                }
                _ => {}
            }
        }
        Some(Plan { plan_sources, plan_uniforms, plan_frames })
    }

    pub fn device_inputs(values: &ValueInputs) -> Result<Inputs, Diagnostic> {
        let mut arrays = Vec::with_capacity(values.arrays.len());
        for v in &values.arrays {
            arrays.push(floats(v)?.ok_or_else(|| value::fail("E_ARRAY_TYPE", "Kernel input is not a numeric array."))?);
        }
        let mut uniforms = Vec::with_capacity(values.uniforms.len());
        let mut tables = Vec::with_capacity(values.uniforms.len());
        for v in &values.uniforms {
            let as_floats = floats(v)?;
            uniforms.push(if as_floats.is_none() { components(v).map_err(|e| match e { KErr::Diag(d) => d, KErr::Unsupported => value::fail("E_KERNEL", "unsupported") })? } else { Vec::new() });
            tables.push(as_floats.unwrap_or_else(|| Arc::new(Vec::new())));
        }
        let mut frame = Vec::with_capacity(values.frame.len());
        for v in &values.frame {
            frame.push(value::num(v)?);
        }
        Ok(Inputs { arrays, uniforms, tables, frame, count: values.count })
    }

    /// `prepare_values`' result as a device kernel's inputs: 32-bit-exact floats (integer arrays and
    /// scalars travel as floats, exact to 2^24, the fast precision's contract), an array-valued
    /// uniform as the table a gather reads.
    pub fn prepare(t: &T, o: &PrepareOptions, live: &Live) -> Result<Inputs, Diagnostic> {
        let fast = o.fast;
        if t.code.iter().any(region::text_register) || (integer_program(t) && !fast) {
            return Err(value::fail(
                "E_GPU_FORM",
                if t.code.iter().any(region::text_register) { "Text kernels stay on the CPU." } else { "Integer sources, uniforms and instructions stay on the CPU." },
            ));
        }
        device_inputs(&prepare_values(t, o, live)?)
    }
}

// ---------------------------------------------------------------------------------------------
// Tests (ported from lib/flow_ir/test_packed.ml, test_noise.ml and test_uniforms.ml; the cases that
// run through the executor, the profile or the device belong to flow_ir.rs and core_exec.rs)

#[cfg(test)]
mod tests {
    use super::*;
    use flow_check::catalog::Catalog;
    use flow_check::workspace as W;
    use flow_syntax::syntax;

    fn checked(body: &str) -> W::Workspace {
        let text = format!("(workspace w (graph g :context value (let* [tested {body}] 0.0)))");
        let forms = syntax::parse(&text).expect("parse");
        match W::check(&[], false, false, None, &Catalog { version: 1, kinds: vec![] }, &forms) {
            (Some(ws), ds) if ds.is_empty() => ws,
            (_, ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("; ")),
        }
    }

    fn recorded(body: &str) -> Value {
        let evaluation = E::static_with(&checked(body), true, None, None).expect("static");
        let path: Path = vec!["g".to_owned(), "tested".to_owned()];
        evaluation.records.iter().find(|(p, _)| *p == path).expect("tested").1[0].1.clone()
    }

    fn compile_body(body: &str, fusion: bool) -> (Value, T) {
        let value = recorded(body);
        let Value::Residual(residual) = value else { panic!("expected a live packed program: {body}") };
        let options = CompileOptions { fusion, ..Default::default() };
        let view = E::residual::residual_view(residual).unwrap();
        match compile_result(&options, residual, &view.term) {
            Ok(program) => (Value::Residual(residual), program),
            Err(ds) => panic!("{}", ds.iter().map(rays_diagnostic::to_string).collect::<Vec<_>>().join("\n")),
        }
    }

    fn floats_same(a: &[f64], b: &[f64]) -> bool {
        a.len() == b.len() && a.iter().zip(b).all(|(x, y)| x.to_bits() == y.to_bits())
    }

    // Marshal-equality of the OCaml tests: exact float bits, exact integers.
    fn value_same(a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Float(x), Value::Float(y)) => x.to_bits() == y.to_bits(),
            (Value::Floats(aw @ 1..=4, x), Value::Floats(bw, y)) if aw == bw => floats_same(x, y),
            (a, b) => a == b,
        }
    }

    fn same(a: &Result<Value, Diagnostic>, b: &Result<Value, Diagnostic>) -> bool {
        match (a, b) {
            (Ok(a), Ok(b)) => value_same(a, b),
            (Err(a), Err(b)) => rays_diagnostic::to_string(a) == rays_diagnostic::to_string(b),
            _ => false,
        }
    }

    fn strict() -> ForceOptions {
        ForceOptions { strict: true, ..Default::default() }
    }

    use parallel::with_domains as at_domains;

    fn reference_of(t: &T, live: &Live) -> Result<Value, Diagnostic> {
        super::reference(t, live, None, None, None)
    }

    #[test]
    fn block_layout_cuts_small_maps_into_four_chunks() {
        // fourteen 1,024-lane blocks: five chunks of three blocks
        assert_eq!(block_layout(13824, 1024), (1024, 14, 3, 5));
        // a map of one block stays one chunk; sixty-four blocks use the sixteen-block chunks
        assert_eq!(block_layout(100, 1024), (100, 1, 1, 1));
        assert_eq!(block_layout(64 * 1024, 1024), (1024, 64, 16, 4));
        assert_eq!(block_layout(0, 1024), (1, 0, 1, 0));
    }

    #[test]
    fn completed_cpu_probe_is_kept_without_a_second_kernel_run() {
        use std::sync::atomic::AtomicUsize;
        let (_, program) = compile_body("(map (fn [x] (+ x t)) (array/float 5 1.0))", true);
        let runs = Arc::new(AtomicUsize::new(0));
        let expired = Arc::new(AtomicBool::new(false));
        let measure = Measure {
            clock: Arc::new(|| 0.0),
            report: Arc::new({
                let runs = runs.clone();
                let expired = expired.clone();
                move |_, _, reference| {
                    assert!(!reference);
                    runs.fetch_add(1, Ordering::Relaxed);
                    // The CPU cook has completed as its deadline expires, before force returns.
                    expired.store(true, Ordering::Relaxed);
                }
            }),
        };
        let stop = expired.clone();
        let mut output = None;
        let completed = E::with_interrupt(Arc::new(move || stop.load(Ordering::Relaxed)), &mut || {
            output = Some(force(&program, &input::at_time(0.5), &ForceOptions { measure: Some(measure.clone()), ..Default::default() }));
        });
        assert!(completed, "A completed CPU result must not be abandoned just because its budget is now expired.");
        assert_eq!(runs.load(Ordering::Relaxed), 1);
        assert_eq!(output.unwrap().unwrap(), Value::Floats(1, Arc::new(vec![1.5; 5])));
    }

    #[test]
    fn cancelled_packed_chunks_never_restart_the_reference_cook() {
        use std::sync::atomic::AtomicUsize;
        for body in [
            "(map (fn [x] (+ x t)) (array/range 16384))",
            "(map (fn [x] (int/add x (frame/index))) (array/range 16384 :type int))",
        ] {
            let (_, program) = compile_body(body, true);
            for domains in [1, 8] {
                at_domains(domains, &|| {
                    let checks = Arc::new(AtomicUsize::new(0));
                    let stop = checks.clone();
                    let fallbacks = Arc::new(AtomicUsize::new(0));
                    let measure = Measure {
                        clock: Arc::new(|| 0.0),
                        report: Arc::new({
                            let fallbacks = fallbacks.clone();
                            move |_, _, reference| { if reference { fallbacks.fetch_add(1, Ordering::Relaxed); } }
                        }),
                    };
                    let mut result = None;
                    let finished = E::with_interrupt(Arc::new(move || stop.fetch_add(1, Ordering::Relaxed) >= 2), &mut || {
                        result = Some(force(&program, &input::at_time(0.5), &ForceOptions { measure: Some(measure.clone()), ..Default::default() }));
                    });
                    assert!(!finished);
                    assert_eq!(result.unwrap().unwrap_err().code, "cancelled");
                    assert_eq!(fallbacks.load(Ordering::Relaxed), 0, "An interrupted packed cook must not cook the same data in the walker.");
                });
            }
        }
    }

    #[test]
    fn capture_results_are_shared_only_within_the_outer_run() {
        use std::sync::atomic::AtomicUsize;
        let (_, program) = compile_body("(map (fn [x] (+ x t)) (array/float 5 1.0))", true);
        let runs = Arc::new(AtomicUsize::new(0));
        let measure = Measure {
            clock: Arc::new(|| 0.0),
            report: Arc::new({
                let runs = runs.clone();
                move |_, _, _| { runs.fetch_add(1, Ordering::Relaxed); }
            }),
        };
        let mut run = make_run(&program, &input::at_time(0.5), &ForceOptions { measure: Some(measure), strict: true, ..Default::default() }, None, None);
        run.in_scope = scope_enter(&run);
        let first = {
            let _scope = ScopeGuard(run.in_scope);
            let first = evaluate(&run, &None, program.residual, &program.term).unwrap();
            let second = evaluate(&run, &None, program.residual, &program.term).unwrap();
            match (&first, &second) {
                (Value::Floats(1, a), Value::Floats(1, b)) => assert!(Arc::ptr_eq(a, b)),
                _ => panic!("Expected shared packed array captures."),
            }
            assert_eq!(runs.load(Ordering::Relaxed), 1);
            first
        };
        assert!(CURRENT.with(|scope| scope.borrow().is_none()));
        run.in_scope = scope_enter(&run);
        let _scope = ScopeGuard(run.in_scope);
        let next = evaluate(&run, &None, program.residual, &program.term).unwrap();
        assert_eq!(first, next);
        assert_eq!(runs.load(Ordering::Relaxed), 2);
    }

    // A map of fourteen 1,024-lane blocks splits into five chunks, which the domains share: values,
    // the first error and an ordered sum are those of one domain.
    #[test]
    fn small_maps_share_chunks_exactly() {
        let source = "(array/range 13824 :type int)";
        for body in [
            format!("(map (fn [x] [(- (float x) t) (float (int/mod x 6))]) {source})"),
            format!("(map (fn [x] (if (int/ge x 9000) (int/div x 0) (int/add x (frame/index)))) {source})"),
            format!("(array/sum (map (fn [x] (* (float x) (+ t 0.1))) {source}))"),
        ] {
            let (value, packed) = compile_body(&body, true);
            let mut live = input::at_time(0.25);
            live.frame = 3;
            let expected = E::host::force_reference(&value, &live, None, None, None);
            for domains in [1, 4, 8] {
                at_domains(domains, &|| {
                    assert!(same(&expected, &force(&packed, &live, &strict())), "Small chunk parity: {body}");
                });
            }
        }
    }

    #[test]
    fn typed_blocks_agree_with_the_reference() {
        let source = "(array/range 2100 :type int)";
        let cases = [
            format!("(map (fn [x] (int/add (int/xor x (frame/index)) 9007199254740993)) {source})"),
            format!("(map (fn [x] (if (int/lt x 0) (int/div x 0) (int/add x (frame/index)))) {source})"),
            format!("(map (fn [x] [(- (float x) t) (float (int/shr x 10))]) {source})"),
            format!("(for [x {source} y (array/float 2 t)] y)"),
            format!("(for [x {source} y (array/vec3 2 [t -0.0 2])] y)"),
            format!("(for [x {source} y (array/vec4 2 [t -0.0 2 3])] y)"),
            format!("(for [x {source} y (array/range 3 :type int)] :skip [0 1024 16384] (int/add (int/mul x 17) (int/add y (frame/index))))"),
            format!("(array/sum (map (fn [x] (int/add x (frame/index))) {source}))"),
            format!("(scan [a (frame/index)] [x {source}] (int/xor (int/mul a 2177342782468422677) x))"),
            format!("(fold [a (frame/index)] [x {source}] (int/xor (int/mul a 2177342782468422677) x))"),
            format!("(map (fn [x] (if (int/eq x 1500) (int/shr x 63) (int/add x (frame/index)))) {source})"),
            format!("(map (fn [x] (if (int/eq x 1500) (int/div x 0) (int/add x (frame/index)))) {source})"),
        ];
        for body in cases {
            let (value, packed) = compile_body(&body, true);
            for frame in [0, 42] {
                let mut live = input::at_time(0.25);
                live.frame = frame;
                let expected = E::host::force_reference(&value, &live, None, None, None);
                for domains in [1, 4, 8] {
                    at_domains(domains, &|| {
                        assert!(same(&expected, &force(&packed, &live, &strict())), "Typed block parity: {body}");
                    });
                }
            }
        }
    }

    #[test]
    fn integer_range_views_match_the_reference() {
        let max_array = MAX_ARRAY + 1;
        for body in [
            "(map (fn [x] (int/add x (int t))) (array/range (int/add 2100 (int t)) :type int))".to_owned(),
            "(let* [n (int/add 2100 (int t)) xs (array/range n :type int)] (map (fn [x] (int/sub x (int t))) xs))".to_owned(),
            "(for [x (array/range (int/add 1025 (int t)) :type int) y (array/range 3 :type int)] (int/add (int/mul x 3) y))".to_owned(),
            "(map (fn [x] (if (< x 2) (int/sub 0 x) (int/add x 7))) (array/range (int t) :type int))".to_owned(),
            "(map (fn [x] (int/add x 1)) (array/range (int/add -1 (int t)) :type int))".to_owned(),
            format!("(map (fn [x] (int/add x 1)) (array/range (int/add {max_array} (int t)) :type int))"),
        ] {
            let (_, packed) = compile_body(&body, false);
            for time in [0.0, 1.0, 2.0] {
                let live = input::at_time(time);
                for domains in [1, 4, 8] {
                    at_domains(domains, &|| {
                        assert!(same(&force(&packed, &live, &strict()), &reference_of(&packed, &live)), "{body} at {time}");
                    });
                }
            }
        }
    }

    #[test]
    fn integer_scaling_is_exact_and_wrapping() {
        for count in [0usize, 1, 5000] {
            for factor in [0i64, -3, (1 << 62) - 1] {
                for reverse in [false, true] {
                    let operation = if reverse { format!("(int/mul {factor} x)") } else { format!("(int/mul x {factor})") };
                    let (_, packed) = compile_body(&format!("(map (fn [x] {operation}) (array/range (int/add {count} (int t)) :type int))"), true);
                    let live = input::at_time(0.25);
                    let expected = Value::Int_array(Arc::new((0..count as i64).map(|i| packed_ops::integer_apply(Integer::Imul, i, factor).unwrap()).collect()));
                    for domains in [1, 4, 8] {
                        at_domains(domains, &|| {
                            assert!(same(&force(&packed, &live, &strict()), &Ok(expected.clone())));
                            assert!(same(&force(&packed, &live, &strict()), &reference_of(&packed, &live)));
                        });
                    }
                }
            }
        }
    }

    // Guard reuse: changing blocks, nested/truncated spans, checked projections and untaken failure
    // match the reference at one/four/eight domains.
    // Binary lanes over whole blocks and over guarded spans that start inside a block (sources
    // above and below the target register) match the reference; a lane that overflows fails.
    #[test]
    fn binary_lanes_match_the_reference() {
        for body in [
            "(map (fn [x] (if (< (mod x 64.0) 32.0) (+ (* x 3.0) (- t x)) (if (< x 0.0) (error \"E_UNTAKEN\" \"Inactive guarded instruction ran.\") (/ (- x t) (+ x 1.0))))) (array/range 5000))",
            "(map (fn [x] (let* [a (* x x) b (+ a t)] (- (* b 0.5) (/ a (+ b 1.0))))) (array/range 3000))",
        ] {
            let (_, packed) = compile_body(body, true);
            for time in [0.0, 0.25] {
                let live = input::at_time(time);
                let expected = reference_of(&packed, &live);
                for domains in [1, 4] {
                    at_domains(domains, &|| assert!(same(&force(&packed, &live, &strict()), &expected), "{body} at {time}"));
                }
            }
        }
        let (_, packed) = compile_body("(map (fn [x] (if (< x 4000.0) (* x t) (* (* (+ x t) 1e300) 1e300))) (array/range 5000))", true);
        assert_eq!(force(&packed, &input::at_time(0.0), &strict()).unwrap_err().code, "E_NONFINITE");
        // a span inside 64-lane blocks, the target between its sources, and in place
        let bs = 64;
        let block = |r: usize| (0..bs).map(|j| (r * 100 + j) as f64).collect::<Vec<f64>>();
        let mut f: Vec<f64> = (0..3).flat_map(block).collect();
        binary_lanes(&mut f, bs, 2 * bs, 0, 20, 60, |x, y| x - y).unwrap();
        assert!((0..bs).all(|j| f[bs + j] == if (20..60).contains(&j) { 200.0 } else { (100 + j) as f64 }));
        binary_lanes(&mut f, bs, bs, 2 * bs, 20, 60, |x, y| x + y).unwrap();
        assert!((20..60).all(|j| f[bs + j] == (400 + j) as f64));
        assert!(binary_lanes(&mut f, 0, 0, bs, 0, bs, |x, y| x * y * 1e308).is_err());
    }

    #[test]
    fn guard_reuse_matches_the_reference() {
        let (_, packed) = compile_body(
            "(map (fn [index]\n      (let* [x (+ (float index) t)]\n        (if (< (mod x 2053.0) 1025.0)\n          (let* [a (float/mul_checked x 0.5)]\n            (if (< (mod x 7.0) 3.0)\n              [a.value (float a.valid) (+ x 1.0) (- x 1.0)]\n              [a.value (float a.valid) (+ x 2.0) (- x 2.0)]))\n          (if (int/lt index 0)\n            (error \"E_UNTAKEN\" \"Inactive guarded instruction ran.\")\n            [-0.0 0.0 x (- 0.0 x)])))) (array/range 5000 :type int))",
            true,
        );
        let nested = region::rebuild(
            &packed,
            region::Rebuild {
                code: Some(
                    packed
                        .code
                        .iter()
                        .map(|i| match i {
                            Instruction::Guarded(g, body) if g.len() >= 2 => {
                                Instruction::Guarded(vec![g[0]], Box::new(Instruction::Guarded(g[1..].to_vec(), body.clone())))
                            }
                            other => other.clone(),
                        })
                        .collect(),
                ),
                ..Default::default()
            },
        )
        .unwrap();
        for time in [0.0, 0.25, 1024.5] {
            let live = input::at_time(time);
            let expected = reference_of(&packed, &live);
            for domains in [1, 4, 8] {
                at_domains(domains, &|| {
                    for program in [&packed, &nested] {
                        assert!(same(&force(program, &live, &strict()), &expected));
                    }
                });
            }
        }
    }

    // Guarded float blocks: selected spans, nested branches, noise, borrowed columns and skipped
    // sinks match the reference.
    #[test]
    fn guarded_float_blocks_with_borrowed_columns() {
        let count = 2100usize;
        for body in [
            "(if (< (mod x 2.0) 1.0) (if (< (mod x 4.0) 1.0) (+ (sin x) t) (- (cos x) t)) (+ x t))",
            "(if (< (mod x 2.0) 1.0) (+ x t) (exp (* -1000.0 x)))",
            "(if (< (mod x 2.0) 1.0) (noise3 (* [(* x 0.01) t 0.25] 0.5) :seed 31 :octaves 3) t)",
        ] {
            let (_, packed) = compile_body(&format!("(map (fn [x] {body}) (array/range {count}))"), true);
            let data: Arc<Vec<f64>> = Arc::new((0..count * 2).map(|i| if i % 2 == 0 { -99.0 } else { (i / 2) as f64 }).collect());
            let source: ColumnSource = {
                let data = data.clone();
                Arc::new(move |_, _| Ok(Some(vec![Column { data: data.clone(), offset: 1, stride: 2 }])))
            };
            for time in [0.0, 0.25, 2.0] {
                let live = input::at_time(time);
                let reference = reference_of(&packed, &live);
                for domains in [1, 8] {
                    at_domains(domains, &|| {
                        assert!(same(&force(&packed, &live, &strict()), &reference));
                        let mut out = vec![vec![Some(vec![0.0; count])]];
                        force_into(&packed, &live, &strict(), &source, count as i64, &mut out).unwrap();
                        let produced = Value::Floats(1, Arc::new(out[0][0].take().unwrap()));
                        assert!(same(&Ok(produced), &reference));
                        let mut skipped = vec![vec![None]];
                        force_into(&packed, &live, &strict(), &source, count as i64, &mut skipped).unwrap();
                    });
                }
            }
        }
        for body in [
            "(if (< x 1.0) t (exp (* x 1000.0)))",
            "(let* [q (* [x x x] 1.7976931348623157e308)] (if (< x -1.0) (noise3 q :seed 31) t))",
            "(if (> x 1.0) (let* [q (* [x x x] 1.7976931348623157e308)] (if (< x -1.0) (noise3 q :seed 31) t)) t)",
        ] {
            let (_, packed) = compile_body(&format!("(map (fn [x] {body}) (array/range {count}))"), true);
            let live = input::at_time(0.25);
            let no_columns: ColumnSource = Arc::new(|_, _| Ok(None));
            assert_eq!(reference_of(&packed, &live).unwrap_err().code, "E_NONFINITE");
            for domains in [1, 8] {
                at_domains(domains, &|| {
                    assert_eq!(force(&packed, &live, &strict()).unwrap_err().code, "E_NONFINITE");
                    let mut none = vec![vec![None]];
                    let error = force_into(&packed, &live, &strict(), &no_columns, count as i64, &mut none).unwrap_err();
                    assert_eq!(error.code, "E_NONFINITE");
                });
            }
        }
    }

    // test_noise.ml: scaled noise skips an untaken overflow and refuses a selected one.
    #[test]
    fn scaled_noise_skips_untaken_overflow_and_refuses_selected_overflow() {
        let (_, packed) = compile_body("(map (fn [p] (if (> t 1e308) (noise3 (* p t)) 0.0)) (array/vec3 2051 [2.0 3.0 4.0]))", true);
        match force(&packed, &input::at_time(1e308), &strict()) {
            Ok(Value::Floats(1, values)) => assert!(values.iter().all(|x| *x == 0.0)),
            other => panic!("Untaken scaled noise branch was evaluated: {other:?}"),
        }
        assert!(force(&packed, &input::at_time(f64::MAX), &strict()).is_err());
    }

    #[test]
    fn one_and_eight_domains_agree_on_a_map() {
        let (_, packed) = compile_body("(map (fn [x] (+ (* x 2) t)) (array/range 5000))", true);
        let live = input::at_time(0.5);
        let one = Mutex::new(None);
        at_domains(1, &|| *one.lock().unwrap() = Some(force_all(&packed, &live, &strict())));
        let eight = Mutex::new(None);
        at_domains(8, &|| *eight.lock().unwrap() = Some(force_all(&packed, &live, &strict())));
        let (one, eight) = (one.into_inner().unwrap().unwrap().unwrap(), eight.into_inner().unwrap().unwrap().unwrap());
        assert!(one.iter().zip(eight.iter()).all(|(a, b)| value_same(a, b)));
    }

    // The streaming plans are chosen for exactly these shapes (noise under one amplitude, and under
    // gain and bias) and equal the reference at one and eight domains over several blocks.
    #[test]
    fn streaming_plans_match_the_reference() {
        let points = "(map (fn [i] [(* i 0.37) (* i 0.11) 0.5]) (array/range 2051))";
        let cases = [
            (format!("(map (fn [p n] (+ p (* n (* (noise3 (* p 0.5) :seed 7) t)))) {points} {points})"), true),
            (
                format!("(map (fn [h p] (+ h (* (- (* (noise3 [(* p.x 0.1) (* p.y 0.1) 0.0] :seed 7) 2.0) 0.5) t))) (map (fn [i] (* i 0.25)) (array/range 2051)) {points})"),
                false,
            ),
        ];
        for (body, noise_plan) in cases {
            let (_, program) = compile_body(&body, false);
            assert_eq!(streaming_plan(&program).is_some(), noise_plan, "{body}");
            assert_eq!(scalar_streaming_plan(&program).is_some(), !noise_plan, "{body}");
            for domains in [1, 8] {
                at_domains(domains, &|| {
                    let live = input::at_time(0.37);
                    assert!(same(&force(&program, &live, &strict()), &reference_of(&program, &live)), "{body} at {domains} domains");
                });
            }
        }
    }

    // An integer array's float copy is charged to a ledger owner while it is kept, and found again
    // by the array's identity.
    #[test]
    fn integer_float_copies_are_charged_and_found_by_identity() {
        let values = Arc::new((0..3001i64).collect::<Vec<_>>());
        let first = host::floats(&Value::Int_array(values.clone())).unwrap().unwrap();
        assert_eq!(first.len(), 3001);
        assert!(mem::peak(&lru::cache("flow_ir.int_floats")) >= 3001 * 8);
        assert!(Arc::ptr_eq(&first, &host::floats(&Value::Int_array(values)).unwrap().unwrap()));
    }

    #[test]
    fn kernel_layout_plans_a_compiled_region() {
        let (_, packed) = compile_body("(map (fn [x] (+ (* x 2.0) t)) (array/range 100))", true);
        let plan = crate::kernel_layout::plan(Default::default(), &packed);
        assert_eq!(plan.output_count, 1);
        assert!(plan.used.iter().any(|u| *u));
        assert!(crate::kernel_layout::uniform_words(&plan) >= 1);
    }
}
