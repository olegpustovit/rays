//! Port of lib/flow_ir/core_exec.ml(i): initial-domain execution of the structured frame core,
//! using native primitive bodies.
//!
//! OCaml exceptions become `Result<_, Diagnostic>` throughout. `Resident_overflow` is a
//! diagnostic with the reserved code [`RESIDENT_OVERFLOW`]: [`resident_overflow`] makes one and
//! [`is_resident_overflow`] tests for it. A region is run through `Options::force`
//! (default `packed::force`). Optional OCaml arguments are the fields of [`Options`].
#![allow(non_camel_case_types, non_snake_case)]

use crate::core_draw;
use crate::core_ir::{self as c, Draw, Expr, FrameField, Plane, Primitive, Program, Statement, StatementNode};
use crate::core_marks;
use crate::packed;
use flow_check::op::{self, Op};
use flow_check::workspace::{DiagnosticPolicy, Paths};
use flow_eval as e;
use flow_syntax::context;
use flow_syntax::ty::Ty;
use flow_syntax::value::{self as v, ResidualId, Value};
use input::Frame;
use rays_diagnostic::Diagnostic;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

pub type Packed = c::RegionT;
pub type Buffers = core_marks::Buffers;

pub type ForceFn = Arc<dyn Fn(&Packed) -> Result<Value, Diagnostic>>;
pub type ResolveFn = Arc<dyn Fn(&Value) -> Result<Value, Diagnostic> + Send + Sync>;
pub type HostSourceFn = Arc<dyn Fn(&Value) -> Result<Option<Value>, Diagnostic>>;
pub type DeviceFn = Arc<dyn Fn(&Packed) -> Result<Option<Vec<(i64, i64, i64)>>, Diagnostic>>;
pub type ImageFn = Arc<dyn Fn(&Value) -> Result<i64, Diagnostic> + Send + Sync>;

/// `[finish]` found a placeholder sized from an earlier draw too small: the loop runs again.
pub const RESIDENT_OVERFLOW: &str = "E_RESIDENT_OVERFLOW";
pub fn resident_overflow() -> Diagnostic {
    v::fail(RESIDENT_OVERFLOW, "A resident placeholder was sized too small.")
}
pub fn is_resident_overflow(d: &Diagnostic) -> bool { d.code == RESIDENT_OVERFLOW }

fn ix(n: i64) -> usize { n as usize }

/// A resident fold's device: inside a qualifying top-level loop of a drawing, a region's array may
/// stay on the GPU, standing in the locals as a placeholder the device knows.
pub struct Resident {
    pub fill: Box<dyn Fn(&Ty, i64, &Value) -> Result<Option<Value>, Diagnostic> + Send + Sync>,
    /// run the region with these arguments on the device (`locals`: the draw's); None leaves it to the CPU
    pub invoke: Box<dyn Fn(&Packed, &[Value], &[Value], &Frame) -> Result<Option<Value>, Diagnostic> + Send + Sync>,
    /// a region with several outputs (fused siblings) run once: a list of a value for each of the
    /// types; None runs them one by one
    pub invoke_all: Box<dyn Fn(&Packed, &[Ty], &[Value], &[Value], &Frame) -> Result<Option<Value>, Diagnostic> + Send + Sync>,
    /// the region run `steps` passes in each lane over its one carried input, every pass kept
    pub unfold: Box<dyn Fn(&Packed, &[Value], i64, &Value, &[Value], &Frame) -> Result<Option<Value>, Diagnostic> + Send + Sync>,
    pub compact: Box<dyn Fn(&Value) -> Result<Option<Value>, Diagnostic> + Send + Sync>,
    pub coerce: Box<dyn Fn(&Ty, &Value) -> Result<Option<Value>, Diagnostic> + Send + Sync>,
    pub concat: Box<dyn Fn(&Value, &Value) -> Result<Option<Value>, Diagnostic> + Send + Sync>,
    /// the host array a placeholder stands for; any other value as it is
    pub finish: Box<dyn Fn(&Value) -> Result<Value, Diagnostic> + Send + Sync>,
    /// a placeholder kept on the device past the scope; None: finish it
    pub keep: Box<dyn Fn(&Value) -> Result<Option<Value>, Diagnostic> + Send + Sync>,
    pub count: Box<dyn Fn(&Value) -> Result<Option<i64>, Diagnostic> + Send + Sync>,
    pub capacity: Box<dyn Fn(&Value) -> Option<i64> + Send + Sync>,
    pub close: Box<dyn Fn() + Send + Sync>,
    pub settle: Box<dyn Fn() -> Result<(), Diagnostic> + Send + Sync>,
    pub range: Box<dyn Fn(&Ty, &Value, i64, i64) -> Result<Option<Value>, Diagnostic> + Send + Sync>,
}

/// Materialise placeholders recursively through records, lists, structs and source arguments.
pub fn host_value(resident: &Resident, value: &Value) -> Result<Value, Diagnostic> {
    fn fields(r: &Resident, fs: &[(String, Value)]) -> Result<Vec<(String, Value)>, Diagnostic> {
        fs.iter().map(|(n, x)| Ok((n.clone(), host_value(r, x)?))).collect()
    }
    Ok(match value {
        Value::Record(fs) => Value::Record(fields(resident, fs)?),
        Value::List(xs) => Value::List(Arc::new(xs.iter().map(|x| host_value(resident, x)).collect::<Result<_, _>>()?)),
        Value::Source(n, t, fs) => Value::Source(n.clone(), t.clone(), fields(resident, fs)?),
        Value::Struct(n, t, fs) => Value::Struct(n.clone(), t.clone(), fields(resident, fs)?),
        other => (resident.finish)(other)?,
    })
}

/// A validated constant-array descriptor can be filled by the resident backend directly.
pub fn fill_constant(resident: &Resident, value: &Value) -> Result<Option<Value>, Diagnostic> {
    let Value::Source(name, ty, fields) = value else { return Ok(None) };
    let Some(operation) = name.strip_prefix("$core/constant-array/") else { return Ok(None) };
    let Value::Source(_, actual, fields) = op::constant_array_descriptor(operation, fields)? else { unreachable!() };
    if *ty != actual { return Err(v::fail("E_ARRAY_TYPE", "Constant array descriptor has the wrong declared type.")); }
    match fields.as_slice() {
        [(a, Value::Int(length)), (b, initial)] if a == "count" && b == "value" => (resident.fill)(ty, *length, initial),
        _ => unreachable!("constant_array_descriptor validates the fields"),
    }
}

/// what a state stores from a resident run: device arrays stay there, the rest is read back
fn kept_value(resident: &Resident, value: &Value) -> Result<Value, Diagnostic> {
    fn fields(r: &Resident, fs: &[(String, Value)]) -> Result<Vec<(String, Value)>, Diagnostic> {
        fs.iter().map(|(n, x)| Ok((n.clone(), kept_value(r, x)?))).collect()
    }
    Ok(match value {
        Value::Record(fs) => Value::Record(fields(resident, fs)?),
        Value::List(xs) => Value::List(Arc::new(xs.iter().map(|x| kept_value(resident, x)).collect::<Result<_, _>>()?)),
        Value::Struct(n, t, fs) => Value::Struct(n.clone(), t.clone(), fields(resident, fs)?),
        other => match (resident.keep)(other)? {
            Some(kept) => kept,
            None => (resident.finish)(other)?,
        },
    })
}

// ---------------------------------------------------------------------------------------------
// The array arena (memory plan S5).

/// `size[n] >= 0` is a size class of local `n`, live over statements `first[n]..=last[n]`: the
/// buffer each takes, -1 for a local alone in its buffer. Largest first; an array's first fitting
/// buffer.
pub fn arena_buffers(first: &[i64], last: &[i64], size: &[i64]) -> Vec<i64> {
    let mut order: Vec<usize> = (0..size.len()).filter(|n| size[*n] >= 0).collect();
    order.sort_by(|a, b| size[*b].cmp(&size[*a]));
    // (class, members), in creation order
    let mut buffers: Vec<(i64, Vec<usize>)> = Vec::new();
    let mut assignment = vec![-1i64; size.len()];
    for n in order {
        let fits = |class: i64, members: &Vec<usize>| {
            class == size[n] && members.iter().all(|m| last[*m] < first[n] || last[n] < first[*m])
        };
        // the first fitting buffer in creation order (the OCaml list is reversed once, then searched)
        match buffers.iter().position(|(class, members)| fits(*class, members)) {
            Some(i) => {
                assignment[n] = i as i64;
                buffers[i].1.push(n);
            }
            None => {
                assignment[n] = buffers.len() as i64;
                buffers.push((size[n], vec![n]));
            }
        }
    }
    for (_, members) in &buffers {
        if let [alone] = members.as_slice() { assignment[*alone] = -1; }
    }
    assignment
}

#[derive(Clone, Debug)]
struct ArenaPlan {
    first: Vec<i64>,
    last: Vec<i64>,
    dying: Vec<Vec<i64>>,
}

struct Arena {
    plan: Arc<ArenaPlan>,
    sizes: Vec<i64>,
    dirty: bool,
    assignment: Vec<i64>,
    buffers: Vec<Value>,
    verified: Vec<bool>,
    kills: bool,
}

fn arena_new(plan: &Arc<ArenaPlan>, kills: bool) -> Arena {
    let n = plan.first.len();
    Arena {
        plan: plan.clone(),
        sizes: vec![-1; n],
        dirty: false,
        assignment: vec![-1; n],
        buffers: vec![Value::No_geo; n],
        verified: vec![false; n],
        kills,
    }
}

fn arena_take(a: &mut Arena, result: i64) -> Value {
    match a.assignment[ix(result)] {
        -1 => Value::No_geo,
        buffer => std::mem::replace(&mut a.buffers[ix(buffer)], Value::No_geo),
    }
}

fn arena_kill(value: &mut Value) {
    // an array shared elsewhere is not ours to fill (the OCaml arena owns its arrays outright)
    match value {
        Value::Int_array(data) => {
            if let Some(d) = Arc::get_mut(data) { d.iter_mut().for_each(|x| *x = OCAML_MIN_INT); }
        }
        Value::Float_array(data) | Value::Vec2_array(data) | Value::Vec3_array(data) | Value::Vec4_array(data) => {
            if let Some(d) = Arc::get_mut(data) { d.iter_mut().for_each(|x| *x = f64::NAN); }
        }
        _ => {}
    }
}
/// OCaml's `min_int` (63-bit).
const OCAML_MIN_INT: i64 = -(1i64 << 62);

/// The carrier `Region::floats_for` recognises: only the data is reused, each result boxes its own
/// width.
fn arena_die(a: &mut Arena, values: &mut [Value], n: i64) {
    let mut value = std::mem::replace(&mut values[ix(n)], Value::No_geo);
    if a.verified[ix(n)] {
        if a.kills { arena_kill(&mut value); }
        match a.assignment[ix(n)] {
            -1 => {}
            buffer => {
                a.buffers[ix(buffer)] = match value {
                    Value::Vec2_array(d) | Value::Vec3_array(d) | Value::Vec4_array(d) => Value::Float_array(d),
                    other => other,
                };
            }
        }
    }
}

/// Physical identity of two values (OCaml `==`): the same array storage, or the same constant.
pub fn phys_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Int_array(x), Value::Int_array(y)) => Arc::ptr_eq(x, y),
        (Value::Text_array(x), Value::Text_array(y)) => Arc::ptr_eq(x, y),
        (Value::Float_array(x), Value::Float_array(y))
        | (Value::Vec2_array(x), Value::Vec2_array(y))
        | (Value::Vec3_array(x), Value::Vec3_array(y))
        | (Value::Vec4_array(x), Value::Vec4_array(y)) => Arc::ptr_eq(x, y),
        (Value::No_geo, Value::No_geo) => true,
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => x.to_bits() == y.to_bits(),
        (Value::Vec2(a, b), Value::Vec2(c, d)) => a.to_bits() == c.to_bits() && b.to_bits() == d.to_bits(),
        (Value::Vec3(a, b, c), Value::Vec3(d, e, f)) => a.to_bits() == d.to_bits() && b.to_bits() == e.to_bits() && c.to_bits() == f.to_bits(),
        (Value::Vec4(a, b, c, d), Value::Vec4(e, f, g, h)) => a.to_bits() == e.to_bits() && b.to_bits() == f.to_bits() && c.to_bits() == g.to_bits() && d.to_bits() == h.to_bits(),
        (Value::Text(x), Value::Text(y)) => x == y,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Residual(x), Value::Residual(y)) => x == y,
        (Value::Fn(x), Value::Fn(y)) => x == y,
        _ => false,
    }
}

/// A region ran: its planned result is ours if its block loop made it; a reader that did not run
/// its block loop may have kept the array, which is then the collector's alone.
fn arena_ran(a: &mut Arena, result: i64, args: &[i64], d: &c::Region::Destination, value: &Value) {
    let ran = phys_eq(value, &d.produced());
    for n in args {
        if a.plan.first[ix(*n)] >= 0 { a.verified[ix(*n)] = a.verified[ix(*n)] && ran; }
    }
    if a.plan.first[ix(result)] >= 0 {
        let size = match value {
            Value::Int_array(data) if ran => (2 * data.len() as i64) + 1,
            Value::Float_array(data) | Value::Vec2_array(data) | Value::Vec3_array(data) | Value::Vec4_array(data) if ran => {
                2 * data.len() as i64
            }
            _ => -1,
        };
        a.verified[ix(result)] = size >= 0;
        if a.sizes[ix(result)] != size {
            a.sizes[ix(result)] = size;
            a.dirty = true;
        }
    }
}

fn arena_finish(a: &mut Arena) {
    if a.dirty {
        a.dirty = false;
        a.assignment = arena_buffers(&a.plan.first, &a.plan.last, &a.sizes);
        let mut wanted = vec![false; a.buffers.len()];
        for buffer in &a.assignment {
            if *buffer >= 0 { wanted[ix(*buffer)] = true; }
        }
        for (i, keep) in wanted.iter().enumerate() {
            if !keep { a.buffers[i] = Value::No_geo; }
        }
    }
}

// ---------------------------------------------------------------------------------------------
// Execution state.

static STATE_IDS: AtomicU64 = AtomicU64::new(1);

struct State {
    id: u64,
    arena: Option<Arena>,
    values: Vec<Value>,
    /// allocated by the first builder, so a call that builds nothing pays nothing
    builders: Vec<Option<Arc<AtomicBool>>>,
    /// by region: a fused sibling's result, made by its group's first member in this pass
    fused_done: Vec<bool>,
    calls: Vec<i64>,
    live: Frame,
    root: Option<ResidualId>,
    force: Option<ForceFn>,
    resolve: Option<ResolveFn>,
    host_source: Option<HostSourceFn>,
    /// the table the draw statements of a draw program fill
    marks: Option<core_marks::Sink>,
    device: Option<DeviceFn>,
    resident: Option<Arc<Resident>>,
    resident_on: bool,
    /// after a resident loop, the locals that may hold its device arrays (`resident_tail`)
    kept: Vec<i64>,
}

type Run = Arc<dyn Fn(&mut State) -> Result<(), Diagnostic> + Send + Sync>;
type Ex = Arc<dyn Fn(&mut State) -> Result<Value, Diagnostic> + Send + Sync>;

/// an array's length; a placeholder's is its capacity, which its block does not say
fn held_length(st: &State, value: &Value) -> Result<i64, Diagnostic> {
    if let Some(r) = &st.resident {
        if let Some(n) = (r.capacity)(value) { return Ok(n); }
    }
    Ok(v::array_length(value)? as i64)
}

struct Scalar {
    code: Vec<i64>,
    a: Vec<usize>,
    b: Vec<usize>,
    k: Vec<f64>,
    result: usize,
    /// None when taken
    regs: Mutex<Option<Vec<f64>>>,
}

pub struct Prepared {
    program: Arc<Program>,
    body: Run,
    /// the locals of a large or all-scalar program, reused across calls
    pool: Mutex<Option<Vec<Value>>>,
    pooled: bool,
    /// no state slot and no region: a failure leaves the evaluation state untouched
    stateless: bool,
    scalar: Option<Scalar>,
    arena_plan: Option<Arc<ArenaPlan>>,
    /// None when taken, as `pool`
    arena_pool: Mutex<Option<Arena>>,
    arena_kills: bool,
}

/// A float-only scalar program (arithmetic, sin, cos over the frame clock and literals) runs on an
/// unboxed float register file inside this interpreter. A non-finite register leaves the program to
/// the general path, which owns the diagnostics.
fn scalar_program(program: &Program) -> Option<Scalar> {
    let n = program.locals.len();
    if n == 0
        || !program.regions.is_empty()
        || !program.state_slots.is_empty()
        || program.call_slots > 0
        || program.results.len() != 1
        || program.locals[ix(program.results[0])] != Ty::Float
    {
        return None;
    }
    let mut code = vec![-1i64; n];
    let mut a = vec![0usize; n];
    let mut b = vec![0usize; n];
    let mut k = vec![0.0f64; n];
    let mut ok = true;
    fn assign(code: &mut [i64], a: &mut [usize], b: &mut [usize], locals: &[Ty], ok: &mut bool, local: i64, op: i64, x: usize, y: usize) {
        let l = ix(local);
        if code[l] >= 0 || locals[l] != Ty::Float { *ok = false; } else { code[l] = op; a[l] = x; b[l] = y; }
    }
    let operand = |code: &[i64], ok: &mut bool, local: i64| -> usize {
        if local < 0 || ix(local) >= n || code[ix(local)] < 0 { *ok = false; 0 } else { ix(local) }
    };
    for st in &program.body {
        match &st.node {
            StatementNode::Assign(l, Expr::Constant(cn)) => match &program.constants[ix(*cn)] {
                c::Literal::Float(f) if f.is_finite() => {
                    assign(&mut code, &mut a, &mut b, &program.locals, &mut ok, *l, 0, 0, 0);
                    k[ix(*l)] = *f;
                }
                _ => ok = false,
            },
            StatementNode::Assign(l, Expr::Frame(FrameField::Time)) => assign(&mut code, &mut a, &mut b, &program.locals, &mut ok, *l, 1, 0, 0),
            StatementNode::Assign(l, Expr::Frame(FrameField::Dt)) => assign(&mut code, &mut a, &mut b, &program.locals, &mut ok, *l, 2, 0, 0),
            StatementNode::Assign(l, Expr::Frame(FrameField::Pointer_x)) => assign(&mut code, &mut a, &mut b, &program.locals, &mut ok, *l, 3, 0, 0),
            StatementNode::Assign(l, Expr::Frame(FrameField::Pointer_y)) => assign(&mut code, &mut a, &mut b, &program.locals, &mut ok, *l, 4, 0, 0),
            StatementNode::Assign(l, Expr::Local(m)) => {
                let x = operand(&code, &mut ok, *m);
                assign(&mut code, &mut a, &mut b, &program.locals, &mut ok, *l, 5, x, 0);
            }
            StatementNode::Assign(l, Expr::Prim(Primitive::Numeric(op @ (flow_syntax::packed_ops::Binary::Add | flow_syntax::packed_ops::Binary::Sub | flow_syntax::packed_ops::Binary::Mul)), args))
                if args.len() == 2 && args[0].0 == "a" && args[1].0 == "b" =>
            {
                let x = operand(&code, &mut ok, args[0].1);
                let y = operand(&code, &mut ok, args[1].1);
                let o = match op { flow_syntax::packed_ops::Binary::Add => 6, flow_syntax::packed_ops::Binary::Sub => 7, _ => 8 };
                assign(&mut code, &mut a, &mut b, &program.locals, &mut ok, *l, o, x, y);
            }
            StatementNode::Assign(l, Expr::Prim(Primitive::Math(op @ (flow_syntax::packed_ops::Unary::Sin | flow_syntax::packed_ops::Unary::Cos)), args))
                if args.len() == 1 =>
            {
                let x = operand(&code, &mut ok, args[0].1);
                let o = if *op == flow_syntax::packed_ops::Unary::Sin { 9 } else { 10 };
                assign(&mut code, &mut a, &mut b, &program.locals, &mut ok, *l, o, x, 0);
            }
            _ => ok = false,
        }
    }
    if ok && code.iter().all(|c| *c >= 0) {
        Some(Scalar { code, a, b, k, result: ix(program.results[0]), regs: Mutex::new(None) })
    } else {
        None
    }
}

/// Runs the register file; `None` when a register went non-finite (the general path owns that).
fn run_scalar(sc: &Scalar, live: &Frame) -> Option<f64> {
    let n = sc.code.len();
    let mut r = sc.regs.lock().unwrap().take().unwrap_or_else(|| vec![0.0; n]);
    let mut finite = true;
    for i in 0..n {
        let value = match sc.code[i] {
            0 => sc.k[i],
            1 => live.t,
            2 => live.dt,
            3 => live.pointer.0,
            4 => live.pointer.1,
            5 => r[sc.a[i]],
            6 => r[sc.a[i]] + r[sc.b[i]],
            7 => r[sc.a[i]] - r[sc.b[i]],
            8 => r[sc.a[i]] * r[sc.b[i]],
            9 => r[sc.a[i]].sin(),
            _ => r[sc.a[i]].cos(),
        };
        if !value.is_finite() { finite = false; break; }
        r[i] = value;
    }
    let x = r[sc.result];
    *sc.regs.lock().unwrap() = Some(r);
    if finite { Some(x) } else { None }
}

fn seal(st: &State, n: i64) {
    if !st.builders.is_empty() {
        if let Some(flag) = &st.builders[ix(n)] { flag.store(false, Ordering::Relaxed); }
    }
}

/// Resolved once per prepared program, not per executed statement.
fn operator(name: &str) -> Result<Op, Diagnostic> {
    op::find(&[], name, &context::value()).ok_or_else(|| v::fail("E_CORE", &format!("Unsupported core primitive: {name}")))
}

fn apply_op(operation: &Op, st: &State, args: &[(String, Value)]) -> Result<Value, Diagnostic> {
    let node = |_: &str, _: &op::Args| -> Result<Value, Diagnostic> {
        Err(v::fail("E_CORE", "Core primitives cannot create catalog nodes."))
    };
    (operation.body)(&st.live, &node, args)
}

/// A lazily resolved operator, shared by the closures built for one call site.
fn lazy_operator(name: String) -> Arc<(String, OnceLock<Result<Op, Diagnostic>>)> {
    Arc::new((name, OnceLock::new()))
}
fn resolved(cell: &(String, OnceLock<Result<Op, Diagnostic>>)) -> Result<&Op, Diagnostic> {
    match cell.1.get_or_init(|| operator(&cell.0)) {
        Ok(operation) => Ok(operation),
        Err(d) => Err(d.clone()),
    }
}

fn generic_named(name: &str, args: &[(String, i64)]) -> Ex {
    let cell = lazy_operator(name.to_owned());
    let args = args.to_vec();
    Arc::new(move |st: &mut State| {
        let operation = resolved(&cell)?;
        let given: Vec<(String, Value)> = args.iter().map(|(n, i)| (n.clone(), st.values[ix(*i)].clone())).collect();
        apply_op(operation, st, &given)
    })
}

/// A local of a plain scalar type never holds a list builder, so it needs no ownership bookkeeping.
fn plain(ty: &Ty) -> bool { matches!(ty, Ty::Float | Ty::Int | Ty::Bool | Ty::Vec2 | Ty::Vec3 | Ty::Vec4 | Ty::Text) }

fn arg(args: &[(String, i64)], key: &str) -> Option<i64> { args.iter().find(|(n, _)| n == key).map(|(_, i)| *i) }
fn arg_names(args: &[(String, i64)], names: &[&str]) -> bool {
    args.len() == names.len() && args.iter().zip(names).all(|((n, _), m)| n == m)
}
fn text_value(s: &str) -> Value { Value::Text(s.as_bytes().to_vec()) }

fn rows_parallel() -> Box<dyn Fn(i64, i64, &(dyn Fn(i64) + Send + Sync)) + Send + Sync> {
    Box::new(|start, finish, work| {
        let _ = parallel::for_(64, start, finish, &|i| {
            work(i);
            Ok(())
        });
    })
}

fn primitive(track: bool, locals: &[Ty], operation: &Primitive, args: &[(String, i64)]) -> Ex {
    use flow_syntax::packed_ops::{Binary, Unary};
    let first = args.first().map(|(_, n)| *n).unwrap_or(-1);
    let seal_args = !matches!(operation, Primitive::Sequence_get | Primitive::Sequence_length | Primitive::Sequence_build);
    let sealed: Vec<i64> = args.iter().filter(|(_, n)| !plain(&locals[ix(*n)])).map(|(_, n)| *n).collect();
    let name = c::primitive_name(operation);
    let args_v: Vec<(String, i64)> = args.to_vec();
    let generic = || -> Ex {
        let cell = lazy_operator(name.to_owned());
        let args = args_v.clone();
        Arc::new(move |st: &mut State| {
            let operation = resolved(&cell)?;
            let given: Vec<(String, Value)> = args.iter().map(|(n, i)| (n.clone(), st.values[ix(*i)].clone())).collect();
            apply_op(operation, st, &given)
        })
    };
    let body: Ex = match operation {
        Primitive::Coerce(ty) => {
            let ty = ty.clone();
            Arc::new(move |st| v::coerce_to(&ty, &st.values[ix(first)]))
        }
        Primitive::Truthy => Arc::new(move |st| Ok(Value::Bool(v::truthy(&st.values[ix(first)])?))),
        Primitive::Show => Arc::new(move |st| Ok(Value::Text(v::show_with(&|x| x.clone(), &st.values[ix(first)])))),
        Primitive::Text_concat => {
            let args = args_v.clone();
            Arc::new(move |st| {
                let mut out: Vec<u8> = Vec::new();
                for (_, i) in &args {
                    match &st.values[ix(*i)] {
                        Value::Text(s) => out.extend_from_slice(s),
                        _ => return Err(v::fail("E_TYPE", "Expected text.")),
                    }
                }
                Ok(Value::Text(out))
            })
        }
        Primitive::Fail(code, message) => {
            let (code, message) = (code.clone(), message.clone());
            Arc::new(move |_| Err(v::fail(&code, &message)))
        }
        Primitive::Fail_diagnostic(d) => {
            let d = d.clone();
            Arc::new(move |_| Err(d.clone()))
        }
        Primitive::Error => generic(),
        Primitive::Rand_at => {
            let seed = arg(args, "seed").expect("seed");
            let index = arg(args, "index").expect("index");
            Arc::new(move |st| match (&st.values[ix(seed)], &st.values[ix(index)]) {
                (Value::Int(seed), Value::Int(index)) => Ok(Value::Float(flow_syntax::packed_ops::rand_at(*seed, *index))),
                _ => Err(v::fail("E_TYPE", "rand/at needs integer seed and index.")),
            })
        }
        Primitive::Array_reduce(operation) => {
            let values = arg(args, "array").expect("array");
            let operation = *operation;
            Arc::new(move |st| op::reduce_array(None, operation, None, &st.values[ix(values)]))
        }
        Primitive::Array_segment(operation) => {
            let values = arg(args, "values").expect("values");
            let offsets = arg(args, "offsets").expect("offsets");
            let operation = *operation;
            Arc::new(move |st| {
                let rows = rows_parallel();
                op::reduce_array(Some(&*rows), operation, Some(&st.values[ix(offsets)]), &st.values[ix(values)])
            })
        }
        Primitive::Array_segment_argmax => {
            let values = arg(args, "values").expect("values");
            let offsets = arg(args, "offsets").expect("offsets");
            Arc::new(move |st| {
                let rows = rows_parallel();
                op::segment_argmax(Some(&*rows), &st.values[ix(offsets)], &st.values[ix(values)])
            })
        }
        Primitive::Remainder => {
            let a = arg(args, "a").expect("a");
            let b = arg(args, "b").expect("b");
            Arc::new(move |st| {
                let x = v::num(&st.values[ix(a)])?;
                let y = v::num(&st.values[ix(b)])?;
                Ok(Value::Float(v::fin("remainder", x % y)?))
            })
        }
        Primitive::Array_count if arg_names(args, &["array"]) => Arc::new(move |st| Ok(Value::Int(held_length(st, &st.values[ix(first)])?))),
        Primitive::Array_nth if arg_names(args, &["array", "index"]) => {
            // A whole index inside native storage, as the operator checks; anything else is its own body.
            let array = arg(args, "array").unwrap();
            let index = arg(args, "index").unwrap();
            let slow = generic();
            Arc::new(move |st| match &st.values[ix(index)] {
                Value::Int(i) if *i >= 0 && (*i as u64) <= op::MAX_WOSIZE as u64 => v::array_get(&st.values[ix(array)], *i),
                _ => slow(st),
            })
        }
        Primitive::Sequence_get => generic_named("nth", args),
        Primitive::Sequence_length => generic_named("count", args),
        Primitive::Sequence_build => {
            let count = arg(args, "count").expect("count");
            let width = arg(args, "width");
            let integer = arg(args, "type");
            let cooked = arg(args, "cooked");
            // With a width the builder is packed storage, which has no list cap.
            Arc::new(move |st| {
                let count = v::int_of(&st.values[ix(count)])?;
                let element = match integer {
                    None => Ty::Float,
                    Some(n) => match &st.values[ix(n)] {
                        Value::Text(t) if t == b"int" => Ty::Int,
                        Value::Text(t) if t == b"float" => Ty::Float,
                        Value::Text(t) if t == b"text" => Ty::Text,
                        _ => return Err(v::fail("E_ARRAY_TYPE", "Core array type is int, float or text.")),
                    },
                };
                match width {
                    None => {
                        if element != Ty::Float { return Err(v::fail("E_ARRAY_TYPE", "Typed builders require width one.")); }
                        let limit = match cooked {
                            None => 4096,
                            Some(n) => match &st.values[ix(n)] {
                                Value::Bool(true) => op::MAX_WOSIZE as i64,
                                Value::Bool(false) => 4096,
                                _ => return Err(v::fail("E_TYPE", "Core cooked collection flag must be boolean.")),
                            },
                        };
                        if count < 0 || count > limit { return Err(v::fail("E_ITER_BOUND", "Core list length exceeds its bound.")); }
                        Ok(Value::List(Arc::new(vec![Value::No_geo; count as usize])))
                    }
                    Some(width) => {
                        let width = v::int_of(&st.values[ix(width)])?;
                        if width < 1 || width > 4 || (element != Ty::Float && width != 1) {
                            return Err(v::fail("E_ARRAY_TYPE", "Core packed width is one to four; integers require width one."));
                        }
                        if element == Ty::Int {
                            v::array_init(&Ty::Int, count, &|_| Value::Int(0))
                        } else if element == Ty::Text {
                            v::array_init(&Ty::Text, count, &|_| Value::Text(Vec::new()))
                        } else {
                            if count < 0 || count > op::MAX_WOSIZE as i64 / width {
                                return Err(v::fail("E_ARRAY_RANGE", "Array length is outside native storage bounds."));
                            }
                            let data = Arc::new(vec![0.0f64; (count * width) as usize]);
                            Ok(match width {
                                2 => Value::Vec2_array(data),
                                3 => Value::Vec3_array(data),
                                4 => Value::Vec4_array(data),
                                _ => Value::Float_array(data),
                            })
                        }
                    }
                }
            })
        }
        Primitive::Sequence_slice => {
            let list = arg(args, "list").expect("list");
            let firstk = arg(args, "first").expect("first");
            let count = arg(args, "count").expect("count");
            Arc::new(move |st| {
                let xs = v::list_arg(&st.values[ix(list)])?;
                let first = v::int_of(&st.values[ix(firstk)])?;
                let count = v::int_of(&st.values[ix(count)])?;
                let len = xs.len() as i64;
                if first < 0 || count < 0 || first > len || count > len - first {
                    return Err(v::fail("E_LIST_RANGE", "A list slice must be within its source."));
                }
                Ok(Value::List(Arc::new(xs[first as usize..(first + count) as usize].to_vec())))
            })
        }
        Primitive::Sequence_concat => {
            let args = args_v.clone();
            Arc::new(move |st| {
                let mut parts: Vec<&[Value]> = Vec::new();
                for (_, n) in &args { parts.push(v::list_arg(&st.values[ix(*n)])?); }
                let mut count = 0i64;
                for xs in &parts {
                    if xs.len() as i64 > 4096 - count {
                        return Err(v::fail("E_ITER_BOUND", "Core concatenation exceeds 4,096 values."));
                    }
                    count += xs.len() as i64;
                }
                let mut out: Vec<Value> = Vec::with_capacity(count as usize);
                for part in parts { out.extend_from_slice(part); }
                let ty = v::elem_ty(&out);
                Ok(Value::List(Arc::new(out.iter().map(|x| v::coerce_to(&ty, x)).collect::<Result<_, _>>()?)))
            })
        }
        Primitive::Color_parse => Arc::new(move |st| {
            let color = op::drawing_color(&st.values[ix(first)])?;
            Ok(Value::Record(vec![
                ("r".into(), Value::Int(color.r)),
                ("g".into(), Value::Int(color.g)),
                ("b".into(), Value::Int(color.b)),
                ("a".into(), Value::Int(color.a)),
            ]))
        }),
        Primitive::Color_byte(mode) => {
            let mode = *mode;
            Arc::new(move |st| {
                let f = v::fin("color", v::num(&st.values[ix(first)])?)?;
                let f = match mode { c::ByteMode::Clamped => f.min(1.0).max(0.0), c::ByteMode::Unclamped => f };
                Ok(Value::Int((((f * 255.0) + 0.5) as i64).max(0).min(255)))
            })
        }
        Primitive::Numeric(compare @ (Binary::Lt | Binary::Le | Binary::Gt | Binary::Ge | Binary::Eq | Binary::And | Binary::Or))
            if arg_names(args, &["a", "b"]) =>
        {
            // The operators' own bodies, without building an argument list per call.
            let a = arg(args, "a").unwrap();
            let b = arg(args, "b").unwrap();
            let compare = *compare;
            Arc::new(move |st| {
                let (x, y) = (&st.values[ix(a)], &st.values[ix(b)]);
                let t = match compare {
                    Binary::Lt => v::num(x)? < v::num(y)?,
                    Binary::Le => v::num(x)? <= v::num(y)?,
                    Binary::Gt => v::num(x)? > v::num(y)?,
                    Binary::Ge => v::num(x)? >= v::num(y)?,
                    Binary::Eq => v::num(x)? == v::num(y)?,
                    Binary::And => v::truthy(x)? && v::truthy(y)?,
                    _ => v::truthy(x)? || v::truthy(y)?,
                };
                Ok(Value::Bool(t))
            })
        }
        Primitive::Equal if arg_names(args, &["a", "b"]) => {
            let a = arg(args, "a").unwrap();
            let b = arg(args, "b").unwrap();
            Arc::new(move |st| Ok(Value::Bool(st.values[ix(a)] == st.values[ix(b)])))
        }
        Primitive::Numeric(operation @ (Binary::Add | Binary::Sub | Binary::Mul)) if arg_names(args, &["a", "b"]) => {
            // Two Floats whose result is finite, without the generic dispatch; anything else is the operator's own body.
            match op::arith(name) {
                Some(arithmetic) => {
                    let a = arg(args, "a").unwrap();
                    let b = arg(args, "b").unwrap();
                    let operation = *operation;
                    Arc::new(move |st| {
                        if let (Value::Float(x), Value::Float(y)) = (&st.values[ix(a)], &st.values[ix(b)]) {
                            let r = match operation { Binary::Add => x + y, Binary::Sub => x - y, _ => x * y };
                            if r.is_finite() { return Ok(Value::Float(r)); }
                        }
                        (arithmetic.apply)(&st.values[ix(a)], &st.values[ix(b)])
                    })
                }
                None => generic(),
            }
        }
        Primitive::Math(operation @ (Unary::Sin | Unary::Cos)) => {
            let x = first;
            let slow = generic();
            let sine = *operation == Unary::Sin;
            Arc::new(move |st| {
                if let Value::Float(f) = &st.values[ix(x)] {
                    let r = if sine { f.sin() } else { f.cos() };
                    if r.is_finite() { return Ok(Value::Float(r)); }
                }
                slow(st)
            })
        }
        Primitive::Numeric(_) if arg_names(args, &["a", "b"]) => match op::arith(name) {
            Some(arithmetic) => {
                let a = arg(args, "a").unwrap();
                let b = arg(args, "b").unwrap();
                Arc::new(move |st| (arithmetic.apply)(&st.values[ix(a)], &st.values[ix(b)]))
            }
            None => generic(),
        },
        _ => generic(),
    };
    if track && seal_args && !sealed.is_empty() {
        Arc::new(move |st| {
            for n in &sealed { seal(st, *n); }
            body(st)
        })
    } else {
        body
    }
}

fn builds(statements: &[Statement]) -> bool {
    let mut found = false;
    c::iter(
        &mut |s: &Statement| {
            if let StatementNode::Assign(_, Expr::Prim(Primitive::Sequence_build, _)) = &s.node { found = true; }
        },
        statements,
    );
    found
}

// ---------------------------------------------------------------------------------------------
// Loop memory, resident loops.

/// what a region reads besides its Core arguments: a host source, or a residual over the frame or
/// state, is an input the loop's key does not see
fn hidden(frame: bool, value: &Value) -> bool {
    match value {
        Value::Source(name, _, _) => name != "$core/argument",
        Value::Residual(r) => {
            (!frame && e::frame_dependent(value))
                || e::state_dependent(value)
                || e::Residual::residual_view(*r).map(|view| view.bindings.iter().any(|(_, x)| hidden(frame, x))).unwrap_or(false)
        }
        Value::Deferred(..) => true,
        Value::List(xs) => xs.iter().any(|x| hidden(frame, x)),
        Value::Record(fs) | Value::Struct(_, _, fs) => fs.iter().any(|(_, x)| hidden(frame, x)),
        _ => false,
    }
}

fn frame_free(frame: bool, instruction: &c::Region::Instruction) -> bool {
    use c::Region::Instruction as I;
    match instruction {
        I::Frame(_) | I::Int_frame(_) => frame,
        I::Rand_at(..) => false,
        I::Guarded(_, inner) => frame_free(frame, inner),
        _ => true,
    }
}

/// A top-level loop of a live program keeps its last results keyed by what it reads from outside
/// itself, so a drawing whose loop inputs did not change skips it. The reads and every local the
/// loop assigns, or `None` for a loop that reads the frame, state, a host or a random stream, or
/// draws.
fn loop_footprint(frame: bool, program: &Program, s: &Statement) -> Option<(Vec<i64>, Vec<i64>)> {
    let mut reads: Vec<i64> = Vec::new();
    let mut writes: Vec<i64> = Vec::new();
    let mut exit = false;
    {
        let mut collect = |statement: &Statement| {
            match &statement.node {
                StatementNode::Assign(_, Expr::Prim(Primitive::Rand | Primitive::Rand_at, _)) => exit = true,
                StatementNode::Assign(_, Expr::Frame(_)) if !frame => exit = true,
                StatementNode::Assign(_, Expr::Source(_) | Expr::Host_source { .. } | Expr::Source_descriptor { .. }) => exit = true,
                StatementNode::Invoke { region, .. } => {
                    let r = &program.regions[ix(*region)];
                    if !r.code.iter().all(|i| frame_free(frame, i))
                        || r.uniforms.iter().any(|u| hidden(frame, u))
                        || r.sources.iter().any(|(residual, _)| hidden(frame, &Value::Residual(*residual)))
                    {
                        exit = true;
                    }
                }
                StatementNode::Draw(_) | StatementNode::State_read { .. } | StatementNode::State_peek { .. } | StatementNode::State_write { .. } => exit = true,
                _ => {}
            }
            reads.extend(c::reads(statement));
            writes.extend(c::writes(statement));
        };
        c::iter(&mut collect, std::slice::from_ref(s));
    }
    if exit { return None; }
    let writes: BTreeSet<i64> = writes.into_iter().collect();
    let reads: BTreeSet<i64> = reads.into_iter().collect();
    Some((reads.into_iter().filter(|n| !writes.contains(n)).collect(), writes.into_iter().collect()))
}

/// A drawing's loop memory: the locals a statement reads from outside itself and the locals it
/// writes, or `None` when it reads the frame, state, a host or a random stream, draws, or calls a
/// region with hidden inputs.
pub fn footprint(program: &Program, s: &Statement) -> Option<(Vec<i64>, Vec<i64>)> { loop_footprint(false, program, s) }

/// Every local a statement mentions, its nested statements' included.
fn mentions(acc: Vec<i64>, s: &Statement) -> Vec<i64> {
    let mut all = acc;
    c::iter(
        &mut |statement: &Statement| {
            all.extend(c::reads(statement));
            all.extend(c::writes(statement));
            if let StatementNode::Reset_call(n) = &statement.node { all.push(*n); }
        },
        std::slice::from_ref(s),
    );
    all
}

/// A loop whose arrays can stay on the device between its statements: its body holds regions,
/// scalar statements, ifs, array/compact, retyping and array/count, and no statement reads an
/// array's contents on the host.
fn resident_loop(program: &Program, s: &Statement) -> bool {
    let array = |n: i64| matches!(program.locals[ix(n)], Ty::Array(_));
    let scalars = |ns: &[i64]| ns.iter().all(|n| !array(*n));
    let expr = |e: &Expr| -> bool {
        match e {
            Expr::Constant(_) | Expr::Local(_) | Expr::Field(..) | Expr::Frame(_) | Expr::Call_index(_) => true,
            Expr::Prim(Primitive::Array_compact | Primitive::Coerce(_) | Primitive::Array_count, _) => true,
            Expr::Prim(_, args) => scalars(&args.iter().map(|(_, n)| *n).collect::<Vec<_>>()),
            Expr::Vec(ns) | Expr::Items(ns) => scalars(ns),
            Expr::Fields(xs) => scalars(&xs.iter().map(|(_, n)| *n).collect::<Vec<_>>()),
            Expr::Source(_) | Expr::Host_source { .. } | Expr::Source_descriptor { .. } => false,
        }
    };
    fn ok(s: &Statement, expr: &dyn Fn(&Expr) -> bool) -> bool {
        match &s.node {
            StatementNode::Assign(_, e) => expr(e),
            StatementNode::If { .. } => c::children(s).into_iter().all(|x| ok(x, expr)),
            StatementNode::Invoke { .. } => true,
            _ => false,
        }
    }
    match &s.node {
        StatementNode::Iterate { body, .. } => {
            body.iter().all(|x| ok(x, &expr)) && body.iter().any(|x| matches!(x.node, StatementNode::Invoke { .. }))
        }
        _ => false,
    }
}

/// A top-level loop a resident fold may keep on the device (its body holds only regions, scalars,
/// ifs, compaction, retyping, counts and immutable frame facts): the writes later statements read,
/// which its end reads back; `None` for any other statement.
pub fn resident_live(program: &Program, s: &Statement) -> Option<Vec<i64>> {
    // Frame facts are fixed during execution: they forbid loop memoization, not GPU residency.
    match loop_footprint(true, program, s) {
        Some((_, writes)) if resident_loop(program, s) => {
            let mut outside: Vec<i64> = Vec::new();
            for t in &program.body {
                if t.id != s.id { outside = mentions(outside, t); }
            }
            Some(writes.into_iter().filter(|n| outside.contains(n)).collect())
        }
        _ => None,
    }
}

/// After a [`resident_live`] loop, whether its arrays may stay on the device for the rest of the
/// draw: the later statements only count them, divide a count by a constant, make an `array/range`
/// of it, and pass these to regions, each a mark attribute (`mark region`) or one whose array
/// result stays under the same rule. The locals that may then hold device arrays, and each range
/// local's source array and divisor.
pub fn resident_tail(program: &Program, s: &Statement, mark: &dyn Fn(i64) -> bool) -> Option<(Vec<i64>, Vec<(i64, (i64, i64))>)> {
    let live = match resident_live(program, s) {
        None => return None,
        Some(l) if l.is_empty() => return None,
        Some(l) => l,
    };
    let device_array = |n: i64| matches!(&program.locals[ix(n)], Ty::Array(inner) if matches!(**inner, Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4));
    let after: &[Statement] = {
        let at = program.body.iter().position(|t| t.id == s.id);
        match at { Some(i) => &program.body[i + 1..], None => &[] }
    };
    let mut held: BTreeSet<i64> = BTreeSet::new();
    let mut counts: BTreeMap<i64, (i64, i64)> = BTreeMap::new();
    let mut ranges: BTreeMap<i64, (i64, i64)> = BTreeMap::new();
    let mut constants: BTreeMap<i64, i64> = BTreeMap::new();
    // its scalars need no device; an integer array (which the display path cannot bind) keeps the read back
    let live: Vec<i64> = live.into_iter().filter(|n| matches!(program.locals[ix(*n)], Ty::Array(_))).collect();
    if live.is_empty() || !live.iter().all(|n| device_array(*n)) { return None; }
    for n in &live { held.insert(*n); }
    for t in after {
        let tracked = |n: i64, held: &BTreeSet<i64>, counts: &BTreeMap<i64, (i64, i64)>, ranges: &BTreeMap<i64, (i64, i64)>| {
            held.contains(&n) || counts.contains_key(&n) || ranges.contains_key(&n)
        };
        let reads = mentions(Vec::new(), t);
        let step = match &t.node {
            StatementNode::Assign(n, Expr::Constant(cn)) => {
                if let c::Literal::Int(k) = &program.constants[ix(*cn)] {
                    if *k > 0 { constants.insert(*n, *k); }
                }
                !tracked(*n, &held, &counts, &ranges)
            }
            StatementNode::Assign(n, Expr::Prim(Primitive::Array_count, args)) if args.len() == 1 && held.contains(&args[0].1) => {
                counts.insert(*n, (args[0].1, 1));
                true
            }
            StatementNode::Assign(n, Expr::Prim(Primitive::Integer(flow_syntax::packed_ops::Integer::Idiv), args))
                if args.len() == 2 && counts.contains_key(&args[0].1) && constants.contains_key(&args[1].1) =>
            {
                let (p, d) = counts[&args[0].1];
                counts.insert(*n, (p, d * constants[&args[1].1]));
                true
            }
            StatementNode::Assign(n, Expr::Prim(Primitive::Array_range, args)) if args.len() == 1 && counts.contains_key(&args[0].1) => {
                ranges.insert(*n, counts[&args[0].1]);
                true
            }
            StatementNode::Invoke { region, args, result } if args.iter().any(|a| tracked(*a, &held, &counts, &ranges)) => {
                if args.iter().any(|a| counts.contains_key(a)) {
                    false
                } else if mark(*region) {
                    true
                } else if device_array(*result) {
                    held.insert(*result);
                    true
                } else {
                    false
                }
            }
            _ => !reads.iter().any(|n| tracked(*n, &held, &counts, &ranges)),
        };
        if !step { return None; }
    }
    let mut kept: Vec<i64> = held.iter().copied().collect();
    kept.extend(ranges.keys().copied());
    Some((kept, ranges.into_iter().collect()))
}

/// A kept tail goes back to the host when a region over it cannot run on the device, or another
/// resident loop needs the slots.
fn release_kept(r: &Resident, st: &mut State) -> Result<(), Diagnostic> {
    let kept = std::mem::take(&mut st.kept);
    for n in kept {
        let value = (r.finish)(&st.values[ix(n)])?;
        st.values[ix(n)] = value;
    }
    Ok(())
}

/// A resident loop's run: the arrays later statements read come back once, as it ends, or with a
/// `tail` stay for the rest of the draw. An overflow discards its work, and it runs again from its
/// start, sized by its bounds from the cut region on.
fn resident_run(r: &Resident, st: &mut State, run: &Run, live: &[i64], tail: &Option<Vec<i64>>) -> Result<(), Diagnostic> {
    if !st.kept.is_empty() {
        release_kept(r, st)?;
        (r.close)();
    }
    let start = st.values.clone();
    loop {
        st.resident_on = true;
        let attempt = (|| -> Result<(), Diagnostic> {
            run(st)?;
            match tail {
                Some(kept) => {
                    (r.settle)()?;
                    st.kept = kept.clone();
                }
                None => {
                    for n in live {
                        let value = (r.finish)(&st.values[ix(*n)])?;
                        st.values[ix(*n)] = value;
                    }
                }
            }
            Ok(())
        })();
        match attempt {
            Ok(()) => {
                st.resident_on = false;
                // kept arrays stay until the draw ends (`draw` closes)
                if tail.is_none() { (r.close)(); }
                return Ok(());
            }
            Err(d) if is_resident_overflow(&d) => {
                st.resident_on = false;
                (r.close)();
                for (i, value) in start.iter().enumerate() { st.values[i] = value.clone(); }
            }
            Err(d) => {
                st.resident_on = false;
                (r.close)();
                return Err(d);
            }
        }
    }
}

/// the same input: a scalar by its bits (NaN and -0 stay distinct), a packed array up to 1M
/// numbers by its bits, a list up to 4,096 items item by item, a record field by field, anything
/// else by identity
fn same_floats(x: &Arc<Vec<f64>>, y: &Arc<Vec<f64>>) -> bool {
    Arc::ptr_eq(x, y) || (x.len() == y.len() && x.len() <= 1_048_576 && x.iter().zip(y.iter()).all(|(a, b)| a.to_bits() == b.to_bits()))
}

fn same_input(a: &Value, b: &Value) -> bool {
    if phys_eq(a, b) { return true; }
    match (a, b) {
        (Value::List(x), Value::List(y)) => x.len() == y.len() && x.len() <= 4096 && x.iter().zip(y.iter()).all(|(p, q)| same_input(p, q)),
        (Value::Record(x), Value::Record(y)) => x.len() == y.len() && x.iter().zip(y.iter()).all(|((n, p), (m, q))| n == m && same_input(p, q)),
        (Value::Float_array(x), Value::Float_array(y))
        | (Value::Vec2_array(x), Value::Vec2_array(y))
        | (Value::Vec3_array(x), Value::Vec3_array(y))
        | (Value::Vec4_array(x), Value::Vec4_array(y)) => same_floats(x, y),
        (Value::Float(x), Value::Float(y)) => x.to_bits() == y.to_bits(),
        (Value::Vec2(a, b), Value::Vec2(c, d)) => a.to_bits() == c.to_bits() && b.to_bits() == d.to_bits(),
        (Value::Vec3(a, b, c), Value::Vec3(d, e, f)) => a.to_bits() == d.to_bits() && b.to_bits() == e.to_bits() && c.to_bits() == f.to_bits(),
        (Value::Text(x), Value::Text(y)) => x == y,
        _ => false,
    }
}

/// ponytail: 4 entries per loop and no byte cap: the results are references to arrays the loop
/// made anyway, at most 4 of them kept alive; add a byte cap if a loop's results grow past tens of
/// MB.
fn remember(reads: Vec<i64>, writes: Vec<i64>, run: Run) -> Run {
    let table: Mutex<Vec<(Vec<Value>, Vec<Value>)>> = Mutex::new(Vec::new());
    Arc::new(move |st: &mut State| {
        // a kept device array is a placeholder the next draw reuses: never a key
        if !st.kept.is_empty() || st.resident_on {
            return run(st);
        }
        let key: Vec<Value> = reads.iter().map(|n| st.values[ix(*n)].clone()).collect();
        let hit = {
            let t = table.lock().unwrap();
            t.iter().find(|(k, _)| k.len() == key.len() && k.iter().zip(key.iter()).all(|(p, q)| same_input(p, q))).map(|(_, vals)| vals.clone())
        };
        match hit {
            Some(values) => {
                for (i, n) in writes.iter().enumerate() { st.values[ix(*n)] = values[i].clone(); }
                Ok(())
            }
            None => {
                run(st)?;
                let values: Vec<Value> = writes.iter().map(|n| st.values[ix(*n)].clone()).collect();
                // a region left on the GPU names a buffer of this draw's mark table, cleared by the
                // next; a kept device array lives as long as the draw
                if st.kept.is_empty() && !values.iter().any(core_marks::is_device) {
                    let mut t = table.lock().unwrap();
                    t.insert(0, (key, values));
                    t.truncate(4);
                }
                Ok(())
            }
        }
    })
}

fn bind_arguments(st: &State, region: &Packed, args: &[i64]) -> Result<Packed, Diagnostic> {
    let vals: Vec<Value> = args.iter().map(|n| st.values[ix(*n)].clone()).collect();
    packed::host::bind_arguments(region, &vals, None)
}

/// The arrays a program can drop at their last read: a result of a top-level region, its local
/// written once, read only by regions and `array/count` (the rule of `resident_tail`), so nothing
/// outside the program ever holds it. A memoized statement's arrays stay; so does a fused or kept
/// region's.
fn arena_plan(remember_loops: bool, excluded: &dyn Fn(i64, i64) -> bool, program: &Program) -> Option<Arc<ArenaPlan>> {
    let n = program.locals.len();
    let mut first = vec![-1i64; n];
    let mut last = vec![-1i64; n];
    let mut written = vec![0i64; n];
    let remembered = |s: &Statement| remember_loops && footprint(program, s).is_some();
    c::iter(
        &mut |s: &Statement| {
            for w in c::writes(s) { written[ix(w)] += 1; }
        },
        &program.body,
    );
    for (i, s) in program.body.iter().enumerate() {
        if let StatementNode::Invoke { region, result, .. } = &s.node {
            if written[ix(*result)] == 1
                && !excluded(*region, *result)
                && !remembered(s)
                && matches!(&program.locals[ix(*result)], Ty::Array(inner) if matches!(**inner, Ty::Int | Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4))
            {
                first[ix(*result)] = i as i64;
            }
        }
    }
    for n in &program.results { first[ix(*n)] = -1; }
    c::iter(
        &mut |s: &Statement| match &s.node {
            StatementNode::Invoke { .. } | StatementNode::Assign(_, Expr::Prim(Primitive::Array_count, _)) => {}
            _ => {
                for n in c::reads(s) { first[ix(n)] = -1; }
            }
        },
        &program.body,
    );
    for (i, s) in program.body.iter().enumerate() {
        for n in mentions(Vec::new(), s) {
            if remembered(s) { first[ix(n)] = -1; }
            last[ix(n)] = last[ix(n)].max(i as i64);
        }
    }
    if first.iter().all(|at| *at < 0) { return None; }
    let mut dying: Vec<Vec<i64>> = vec![Vec::new(); program.body.len()];
    for (n, at) in first.iter().enumerate() {
        if *at >= 0 { dying[ix(last[n])].insert(0, n as i64); }
    }
    Some(Arc::new(ArenaPlan { first, last, dying }))
}

fn prepare_invoke(packed_region: Packed, args: Vec<i64>, result: i64, on_device: bool, keep: bool, clobber: bool) -> Run {
    // with no arguments the region binds once
    let bound = if args.is_empty() { packed::host::bind_arguments(&packed_region, &[], None) } else { Ok(packed_region.clone()) };
    let kept: Mutex<Option<Value>> = Mutex::new(None);
    Arc::new(move |st: &mut State| {
        if !st.builders.is_empty() { st.builders[ix(result)] = None; }
        if st.marks.is_some() {
            if let Some(v) = kept.lock().unwrap().clone() {
                st.values[ix(result)] = v;
                return Ok(());
            }
        }
        let region = bound.clone()?;
        let bind = |st: &State, validate: Option<bool>, region: &Packed| -> Result<Packed, Diagnostic> {
            if args.is_empty() {
                Ok(region.clone())
            } else {
                let vals: Vec<Value> = args.iter().map(|n| st.values[ix(*n)].clone()).collect();
                packed::host::bind_arguments(region, &vals, validate)
            }
        };
        // after a resident loop, a region over the arrays it kept runs on the device too: a mark
        // attribute through `device`, any other region there, its array kept
        let held = !st.kept.is_empty() && args.iter().any(|n| st.kept.contains(n));
        let argv = |st: &State| -> Vec<Value> { args.iter().map(|n| st.values[ix(*n)].clone()).collect() };
        let resident = match st.resident.clone() {
            Some(r) if st.resident_on => match (r.invoke)(&region, &argv(st), &st.values, &st.live)? {
                Some(value) => {
                    st.values[ix(result)] = value;
                    true
                }
                None => {
                    for n in &args {
                        let value = host_value(&r, &st.values[ix(*n)])?;
                        st.values[ix(*n)] = value;
                    }
                    false
                }
            },
            Some(r) if held && !on_device => match (r.invoke)(&region, &argv(st), &st.values, &st.live)? {
                Some(value) => {
                    st.values[ix(result)] = value;
                    true
                }
                None => {
                    release_kept(&r, st)?;
                    false
                }
            },
            _ => false,
        };
        if resident { return Ok(()); }
        let unbound = region.clone();
        let mut bound_region = bind(st, if held { Some(false) } else { None }, &region)?;
        let mut device_done = false;
        if let (Some(device), Some(sink)) = (st.device.clone(), st.marks.as_ref()) {
            if on_device {
                match device(&bound_region)? {
                    Some(outputs) => {
                        if let [(token, count, width)] = outputs.as_slice() {
                            st.values[ix(result)] = core_marks::device_value(sink, *token, *count, *width);
                            device_done = true;
                        } else {
                            return Err(v::fail("E_CORE_DEVICE", "A single region needs one device output."));
                        }
                    }
                    None => {}
                }
            }
        }
        if device_done { return Ok(()); }
        if let Some(r) = st.resident.clone() {
            if held && !st.kept.is_empty() {
                release_kept(&r, st)?;
                bound_region = bind(st, None, &unbound)?;
            }
        }
        let destination: Option<Arc<c::Region::Destination>> = match st.arena.as_mut() {
            Some(a) => {
                let buffer = arena_take(a, result);
                Some(Arc::new(c::Region::Destination::new(buffer, Value::No_geo, a.kills)))
            }
            None => {
                if clobber { Some(Arc::new(c::Region::Destination::new(Value::No_geo, Value::No_geo, clobber))) } else { None }
            }
        };
        if let Some(d) = &destination { bound_region = c::Region::with_destination(&bound_region, d.clone()); }
        let forced = match st.force.clone() {
            Some(force) => force(&bound_region),
            None => packed::force(&bound_region, &st.live, &packed::ForceOptions::default()),
        };
        let value = forced?;
        st.values[ix(result)] = value.clone();
        if let (Some(a), Some(d)) = (st.arena.as_mut(), &destination) { arena_ran(a, result, &args, d, &value); }
        if keep && st.marks.is_some() { *kept.lock().unwrap() = Some(value); }
        Ok(())
    })
}

fn grouped(st: &State) -> bool { (st.device.is_some() && st.marks.is_some() && !st.resident_on) || (st.resident_on && st.resident.is_some()) }

/// A resident fold's sibling group, run where its first member is: one dispatch for all of them
/// (each later member then finds its result made), else the first member alone, the others
/// each where it stands.
fn prepare_first(group: Arc<core_draw::Group>, tys: Vec<Ty>, first: Run) -> Run {
    Arc::new(move |st: &mut State| {
        if let (true, Some(r)) = (st.resident_on, st.resident.clone()) {
            let args: Vec<Value> = group.args.iter().map(|a| st.values[ix(*a)].clone()).collect();
            if let Some(Value::List(made)) = (r.invoke_all)(&group.region, &tys, &args, &st.values, &st.live)? {
                if made.len() == group.results.len() {
                    let regions = group.members.iter().map(|m| ix(*m) + 1).max().unwrap_or(0);
                    if st.fused_done.len() < regions { st.fused_done.resize(regions, false); }
                    for ((member, result), value) in group.members.iter().zip(&group.results).zip(made.iter()) {
                        if !st.builders.is_empty() { st.builders[ix(*result)] = None; }
                        st.values[ix(*result)] = value.clone();
                        st.fused_done[ix(*member)] = *member != group.members[0];
                    }
                    return Ok(());
                }
            }
        }
        for member in &group.members {
            if let Some(done) = st.fused_done.get_mut(ix(*member)) { *done = false; }
        }
        first(st)
    })
}

/// `tys`: the members' result types
fn prepare_group(group: Arc<core_draw::Group>, tys: Vec<Ty>, fallback: Vec<(Option<c::Span>, Run)>) -> Run {
    let results: Vec<i64> = group.results.clone();
    Arc::new(move |st: &mut State| {
        if !grouped(st) {
            return (fallback.last().expect("a group has a member").1)(st);
        }
        let original = |st: &mut State| -> Result<(), Diagnostic> {
            for (span, run) in &fallback {
                if let Err(d) = run(st) {
                    return Err(if d.span.is_none() { Diagnostic { span: span.clone(), ..d } } else { d });
                }
            }
            Ok(())
        };
        // a resident fold runs the siblings as one dispatch, each result its own placeholder
        if let (true, Some(r)) = (st.resident_on, st.resident.clone()) {
            let args: Vec<Value> = group.args.iter().map(|a| st.values[ix(*a)].clone()).collect();
            return match (r.invoke_all)(&group.region, &tys, &args, &st.values, &st.live)? {
                Some(Value::List(made)) if made.len() == results.len() => {
                    for (result, value) in results.iter().zip(made.iter()) {
                        if !st.builders.is_empty() { st.builders[ix(*result)] = None; }
                        st.values[ix(*result)] = value.clone();
                    }
                    Ok(())
                }
                _ => original(st),
            };
        }
        let device = st.device.clone().expect("device");
        let offered = bind_arguments(st, &group.region, &group.args).and_then(|bound| device(&bound));
        match offered {
            Ok(Some(outputs))
                if outputs.len() == results.len()
                    && outputs.iter().enumerate().all(|(index, (_, count, width))| {
                        *count >= 0 && *width == group.region.outputs[index].len() as i64
                    }) =>
            {
                let sink = st.marks.as_ref().expect("marks");
                let made: Vec<(i64, Value)> = outputs
                    .iter()
                    .enumerate()
                    .map(|(index, (token, count, width))| (results[index], core_marks::device_value(sink, *token, *count, *width)))
                    .collect();
                for (result, value) in made {
                    if !st.builders.is_empty() { st.builders[ix(result)] = None; }
                    st.values[ix(result)] = value;
                }
                Ok(())
            }
            Ok(_) | Err(_) => original(st),
        }
    })
}

/// A counted loop that steps one region over a carried array and appends each pass to another:
/// `pre` its loop-invariant scalars, the region and its arguments, the carried history (`points`).
/// The carried head must be the region's one source and dead after the loop, the history unread by
/// the region; anything else declines, and the loop runs pass by pass.
struct Unfold {
    pre: Vec<Statement>,
    region: i64,
    args: Vec<i64>,
    head: i64,
    points: i64,
}

fn unfold_shape(program: &Program, loop_: &Statement, body: &[Statement], index: i64, item: i64) -> Option<Unfold> {
    let invokes: Vec<(i64, Vec<i64>, i64)> = body
        .iter()
        .filter_map(|s| match &s.node {
            StatementNode::Invoke { region, args, result } => Some((*region, args.clone(), *result)),
            _ => None,
        })
        .collect();
    if invokes.len() != 1 { return None; }
    let (region, args, h) = invokes[0].clone();
    let mut copies: BTreeMap<i64, i64> = BTreeMap::new();
    for s in body {
        if let StatementNode::Assign(x, Expr::Local(y)) = &s.node { copies.insert(*x, *y); }
    }
    // the local a copy chain starts from
    fn origin(copies: &BTreeMap<i64, i64>, x: i64) -> i64 {
        let mut x = x;
        let mut guard = 0;
        while let Some(y) = copies.get(&x) {
            if *y == x || guard > copies.len() { break; }
            x = *y;
            guard += 1;
        }
        x
    }
    let concat = body.iter().find_map(|s| match &s.node {
        StatementNode::Assign(pc, Expr::Prim(Primitive::Array_concat, a)) if a.len() == 2 && origin(&copies, a[1].1) == h => Some((*pc, a[0].1)),
        _ => None,
    });
    let (pc, pin) = concat?;
    let carried_to = |src: i64| -> Vec<i64> { copies.keys().filter(|x| origin(&copies, **x) == src && **x != src).copied().collect() };
    let head: Vec<i64> = carried_to(h).into_iter().filter(|x| args.contains(x)).collect();
    let points_ok = carried_to(pc).contains(&pin);
    let mut moved = vec![h, pc];
    moved.extend(carried_to(h));
    moved.extend(carried_to(pc));
    let pre: Vec<Statement> = body
        .iter()
        .filter(|s| match &s.node {
            StatementNode::Invoke { .. } => false,
            StatementNode::Assign(x, Expr::Local(_)) if moved.contains(x) => false,
            StatementNode::Assign(x, _) if *x == pc => false,
            _ => true,
        })
        .cloned()
        .collect();
    let pure = |s: &Statement| -> bool {
        matches!(s.node, StatementNode::Assign(..) | StatementNode::Reset_call(_))
            && !c::reads(s).iter().any(|n| *n == index || *n == item || moved.contains(n))
    };
    match head.as_slice() {
        [head] if points_ok && pre.iter().all(pure) && program.regions[ix(region)].sources.len() == 1 => {
            // the head is dead past the loop: nothing outside it reads it
            let mut inside: Vec<i64> = Vec::new();
            c::iter(&mut |s: &Statement| inside.push(s.id), body);
            let mut read_outside = false;
            c::iter(
                &mut |s: &Statement| {
                    if s.id != loop_.id && !inside.contains(&s.id) && c::reads(s).contains(head) {
                        read_outside = true;
                    }
                },
                &program.body,
            );
            if read_outside { None } else { Some(Unfold { pre, region, args, head: *head, points: pin }) }
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------------------------
// prepare.

type SlotKey = Arc<dyn Fn(&State) -> Result<(i64, Vec<String>, Vec<i64>, bool), Diagnostic> + Send + Sync>;

struct Ctx {
    program: Arc<Program>,
    track: bool,
    device_ok: Vec<bool>,
    fused: Vec<Option<Arc<core_draw::Group>>>,
    individual: Mutex<Vec<Option<(Option<c::Span>, Run)>>>,
    static_ok: Vec<bool>,
    tails: Vec<(usize, Vec<i64>)>,
    tail_ranges: Vec<(i64, (i64, i64))>,
    constants: Vec<Value>,
    slot_keys: Vec<SlotKey>,
    hoisted_writes: Vec<i64>,
    planned: Option<Arc<ArenaPlan>>,
    remember_loops: bool,
    clobber: bool,
    in_loop: std::cell::Cell<bool>,
    aliases: Vec<Vec<i64>>,
}

fn addr(s: &Statement) -> usize { s as *const Statement as usize }

impl Ctx {
    fn hoisted(&self, s: &Statement) -> bool {
        match &s.node {
            StatementNode::Assign(n, Expr::Constant(_) | Expr::Frame(FrameField::Time | FrameField::Dt | FrameField::Index | FrameField::Tick | FrameField::Width | FrameField::Height | FrameField::Pointer_x | FrameField::Pointer_y)) => {
                self.hoisted_writes[ix(*n)] == 1
            }
            _ => false,
        }
    }

    fn expression(&self, expr: &Expr) -> Ex {
        let program = &self.program;
        match expr {
            Expr::Constant(n) => {
                let value = self.constants[ix(*n)].clone();
                Arc::new(move |_| Ok(value.clone()))
            }
            Expr::Source(value) => {
                // a static source read in a loop resolves once per execution, not per element
                let last: Mutex<Option<(u64, Value)>> = Mutex::new(None);
                let value = value.clone();
                Arc::new(move |st| {
                    if let Some((owner, resolved)) = &*last.lock().unwrap() {
                        if *owner == st.id { return Ok(resolved.clone()); }
                    }
                    let resolved = e::Host::force_reference(&value, &st.live, None, None, st.resolve.clone())?;
                    *last.lock().unwrap() = Some((st.id, resolved.clone()));
                    Ok(resolved)
                })
            }
            Expr::Host_source { name, ty, args } => {
                let (name, ty, args) = (name.clone(), ty.clone(), args.clone());
                Arc::new(move |st| {
                    let offered = match st.host_source.clone() {
                        None => Ok(None),
                        Some(offer) => {
                            let fields = args.iter().map(|(n, i)| (n.clone(), st.values[ix(*i)].clone())).collect();
                            offer(&Value::Source(name.clone(), ty.clone(), fields))
                        }
                    };
                    match offered? {
                        Some(value) => {
                            if matches!(&value, Value::Source(..) | Value::Struct(_, Ty::Array(_), _)) {
                                return Err(v::fail("E_DATA_SOURCE", "The host returned an unresolved data source."));
                            }
                            if ty != Ty::Any && v::ty_of(&value) != ty {
                                return Err(v::fail("E_DATA_SOURCE", "The host returned a data source of the wrong type."));
                            }
                            v::validate(&value)?;
                            Ok(value)
                        }
                        None => {
                            let mut given: Vec<(String, Value)> = Vec::new();
                            for (n, i) in &args {
                                let value = st.values[ix(*i)].clone();
                                given.push((n.clone(), match &st.resident { Some(r) => host_value(r, &value)?, None => value }));
                            }
                            if name == "$core/complete" && ty == flow_syntax::ty::geometry() && given.len() == 1 && given[0].0 == "value" && given[0].1 == Value::No_geo {
                                Ok(Value::No_geo)
                            } else {
                                let value = Value::Source(name.clone(), ty.clone(), given);
                                e::Host::force_reference(&value, &st.live, None, None, st.resolve.clone())
                            }
                        }
                    }
                })
            }
            Expr::Source_descriptor { name, ty, args } => {
                let (name, ty, args) = (name.clone(), ty.clone(), args.clone());
                Arc::new(move |st| {
                    let given: Vec<(String, Value)> = args.iter().map(|(n, i)| (n.clone(), st.values[ix(*i)].clone())).collect();
                    let prefix = "$core/constant-array/";
                    if let Some(operation) = name.strip_prefix(prefix) {
                        let value = op::constant_array_descriptor(operation, &given)?;
                        if v::ty_of(&value) != ty { return Err(v::fail("E_ARRAY_TYPE", "Constant array descriptor has the wrong declared type.")); }
                        if let Some(resident) = &st.resident {
                            if st.resident_on {
                                if let Some(made) = fill_constant(resident, &value)? { return Ok(made); }
                            }
                        }
                        Ok(value)
                    } else {
                        Ok(Value::Source(name.clone(), ty.clone(), given))
                    }
                })
            }
            Expr::Local(n) => {
                let n = ix(*n);
                Arc::new(move |st| Ok(st.values[n].clone()))
            }
            Expr::Call_index(slot) => {
                let slot = ix(*slot);
                Arc::new(move |st| {
                    let n = st.calls[slot];
                    st.calls[slot] = n + 1;
                    Ok(Value::Int(n))
                })
            }
            Expr::Frame(field) => match field {
                FrameField::Time => Arc::new(|st| Ok(Value::Float(st.live.t))),
                FrameField::Dt => Arc::new(|st| Ok(Value::Float(st.live.dt))),
                FrameField::Index => Arc::new(|st| Ok(Value::Int(st.live.frame))),
                FrameField::Width => Arc::new(|st| Ok(Value::Int(st.live.size.0))),
                FrameField::Height => Arc::new(|st| Ok(Value::Int(st.live.size.1))),
                FrameField::Pointer_x => Arc::new(|st| Ok(Value::Float(st.live.pointer.0))),
                FrameField::Pointer_y => Arc::new(|st| Ok(Value::Float(st.live.pointer.1))),
                FrameField::Tick => Arc::new(|st| Ok(Value::Int(st.live.tick))),
                FrameField::Input => {
                    let cell = lazy_operator("frame/input".to_owned());
                    Arc::new(move |st| apply_op(resolved(&cell)?, st, &[]))
                }
            },
            Expr::Prim(operation, args) => {
                let run = primitive(self.track, &program.locals, operation, args);
                let constant = matches!(operation, Primitive::Array_int | Primitive::Array_float | Primitive::Array_vec3 | Primitive::Array_vec4).then(|| c::primitive_name(operation));
                let named_args = if constant.is_some() { args.clone() } else { Vec::new() };
                let args: Vec<i64> = args.iter().map(|(_, n)| *n).collect();
                Arc::new(move |st| {
                    if let Some(r) = st.resident.clone() {
                        if st.resident_on {
                            if let Some(name) = constant {
                                let given: Vec<(String, Value)> = named_args.iter().map(|(name, n)| (name.clone(), st.values[ix(*n)].clone())).collect();
                                let descriptor = op::constant_array_descriptor(name, &given)?;
                                if let Some(made) = fill_constant(&r, &descriptor)? { return Ok(made); }
                            }
                            for n in &args {
                                let value = host_value(&r, &st.values[ix(*n)])?;
                                st.values[ix(*n)] = value;
                            }
                        }
                    }
                    run(st)
                })
            }
            Expr::Vec(ns) => {
                let ns = ns.clone();
                Arc::new(move |st| {
                    let mut xs = Vec::with_capacity(ns.len());
                    for n in &ns { xs.push(v::num(&st.values[ix(*n)])?); }
                    match xs.as_slice() {
                        [x, y] => Ok(Value::Vec2(*x, *y)),
                        [x, y, z] => Ok(Value::Vec3(*x, *y, *z)),
                        [x, y, z, w] => Ok(Value::Vec4(*x, *y, *z, *w)),
                        _ => Err(v::fail("E_CORE", "Invalid vector width.")),
                    }
                })
            }
            Expr::Fields(xs) => {
                let xs = xs.clone();
                Arc::new(move |st| {
                    Ok(Value::Record(
                        xs.iter()
                            .map(|(name, n)| {
                                seal(st, *n);
                                (name.clone(), st.values[ix(*n)].clone())
                            })
                            .collect(),
                    ))
                })
            }
            Expr::Items(ns) => {
                let ns = ns.clone();
                let all_geometry = ns.iter().all(|n| program.locals[ix(*n)] == flow_syntax::ty::geometry());
                Arc::new(move |st| {
                    let xs: Vec<Value> = ns
                        .iter()
                        .map(|n| {
                            seal(st, *n);
                            st.values[ix(*n)].clone()
                        })
                        .collect();
                    if xs.len() > 4096 && !all_geometry { return Err(v::fail("E_ITER_BOUND", "Core list exceeds 4,096 values.")); }
                    let ty = v::elem_ty(&xs);
                    Ok(Value::List(Arc::new(xs.iter().map(|x| v::coerce_to(&ty, x)).collect::<Result<_, _>>()?)))
                })
            }
            Expr::Field(n, name) => {
                let n = ix(*n);
                let name = name.clone();
                Arc::new(move |st| {
                    let fast = match (&st.values[n], name.as_str()) {
                        (Value::Vec2(x, _) | Value::Vec3(x, _, _) | Value::Vec4(x, _, _, _), "x") => Some(*x),
                        (Value::Vec2(_, y) | Value::Vec3(_, y, _) | Value::Vec4(_, y, _, _), "y") => Some(*y),
                        (Value::Vec3(_, _, z) | Value::Vec4(_, _, z, _), "z") => Some(*z),
                        (Value::Vec4(_, _, _, w), "w") => Some(*w),
                        _ => None,
                    };
                    if let Some(x) = fast { return Ok(Value::Float(x)); }
                    match &st.values[n] {
                        Value::Record(xs) => match xs.iter().find(|(k, _)| *k == name) {
                            Some((_, value)) => Ok(value.clone()),
                            None => Err(v::fail("E_FIELD", "Missing core field.")),
                        },
                        _ => Err(v::fail("E_FIELD", "Invalid core field.")),
                    }
                })
            }
        }
    }

    /// Constants and frame facts a loop body sets once are set when the loop starts, not per iteration.
    fn invariant(&self, statements: &[Statement]) -> Vec<(i64, Ex)> {
        let mut out = Vec::new();
        for s in statements {
            match &s.node {
                StatementNode::Assign(n, e) if self.hoisted(s) => out.push((*n, self.expression(e))),
                StatementNode::If { yes, no, .. } => {
                    out.extend(self.invariant(yes));
                    out.extend(self.invariant(no));
                }
                _ => {}
            }
        }
        out
    }

    fn block(&self, statements: &[Statement], top_body: bool) -> Run {
        let in_loop = self.in_loop.get();
        let kept: Vec<&Statement> = if in_loop { statements.iter().filter(|s| !self.hoisted(s)).collect() } else { statements.iter().collect() };
        let top = !in_loop;
        let arena_dying: Option<&Vec<Vec<i64>>> = if top_body { self.planned.as_ref().map(|p| &p.dying) } else { None };
        let mut runs: Vec<Run> = Vec::with_capacity(kept.len());
        for s in kept.iter() {
            let remembered_kind = matches!(s.node, StatementNode::Iterate { .. } | StatementNode::Counted { .. } | StatementNode::Invoke { .. });
            let fused_invoke = match &s.node {
                StatementNode::Invoke { region, .. } => self.fused[ix(*region)].is_some(),
                _ => false,
            };
            let run = if remembered_kind && top && self.remember_loops && !fused_invoke {
                let run = self.statement(s);
                let run: Run = match resident_live(&self.program, s) {
                    Some(live) => {
                        let tail = self.tails.iter().find(|(a, _)| *a == addr(s)).map(|(_, k)| k.clone());
                        let plain_run = run.clone();
                        Arc::new(move |st: &mut State| match st.resident.clone() {
                            Some(r) if st.marks.is_some() => resident_run(&r, st, &plain_run, &live, &tail),
                            _ => plain_run(st),
                        })
                    }
                    None => run,
                };
                match footprint(&self.program, s) {
                    Some((reads, writes)) => remember(reads, writes, run),
                    None => run,
                }
            } else {
                self.statement(s)
            };
            runs.push(run);
        }
        // the dying arrays of statement `i` (the statements are program.body's own when arena_dying is set)
        let runs: Vec<Run> = runs
            .into_iter()
            .enumerate()
            .map(|(i, run)| match arena_dying {
                Some(dying) if !dying[i].is_empty() => {
                    let dying = dying[i].clone();
                    let wrapped: Run = Arc::new(move |st: &mut State| {
                        run(st)?;
                        if let Some(a) = st.arena.as_mut() {
                            for n in &dying { arena_die(a, &mut st.values, *n); }
                        }
                        Ok(())
                    });
                    wrapped
                }
                _ => run,
            })
            .collect();
        let spans: Vec<Option<c::Span>> = kept.iter().map(|s| s.source.span.clone()).collect();
        // The innermost block that sees an unlocated failure gives it its statement's span.
        Arc::new(move |st: &mut State| {
            for (i, run) in runs.iter().enumerate() {
                if let Err(d) = run(st) {
                    return Err(if d.span.is_none() { Diagnostic { span: spans[i].clone(), ..d } } else { d });
                }
            }
            Ok(())
        })
    }

    fn loop_body(&self, body: &[Statement]) -> (Run, Run) {
        let inner = self.in_loop.get();
        self.in_loop.set(true);
        let run = self.block(body, false);
        self.in_loop.set(inner);
        let setup = self.invariant(body);
        let setup: Run = Arc::new(move |st: &mut State| {
            for (n, value) in &setup {
                let x = value(st)?;
                st.values[ix(*n)] = x;
            }
            Ok(())
        });
        (setup, run)
    }

    fn plain_block(&self, statements: &[Statement]) -> Run {
        let inner = self.in_loop.get();
        self.in_loop.set(false);
        let run = self.block(statements, false);
        self.in_loop.set(inner);
        run
    }

    fn statement(&self, s: &Statement) -> Run {
        let program = self.program.clone();
        match &s.node {
            StatementNode::Assign(n, e @ Expr::Prim(op @ (Primitive::Array_compact | Primitive::Coerce(Ty::Array(_))), args)) if args.len() == 1 => {
                // in a resident fold the device compacts and retypes its own arrays; the host otherwise
                let value = self.expression(e);
                let arg = args[0].1;
                let n = ix(*n);
                let op = op.clone();
                Arc::new(move |st| {
                    let x = match st.resident.clone() {
                        Some(r) if st.resident_on => {
                            let made = match &op {
                                Primitive::Array_compact => (r.compact)(&st.values[ix(arg)])?,
                                Primitive::Coerce(ty) => (r.coerce)(ty, &st.values[ix(arg)])?,
                                _ => None,
                            };
                            match made {
                                Some(x) => x,
                                None => {
                                    let done = (r.finish)(&st.values[ix(arg)])?;
                                    st.values[ix(arg)] = done;
                                    value(st)?
                                }
                            }
                        }
                        _ => value(st)?,
                    };
                    st.values[n] = x;
                    Ok(())
                })
            }
            StatementNode::Assign(n, e @ Expr::Prim(Primitive::Array_concat, args)) if args.len() == 2 => {
                // in a resident fold the device joins its own arrays (appending a history in place)
                let value = self.expression(e);
                let (a, b) = (args[0].1, args[1].1);
                let n = ix(*n);
                Arc::new(move |st| {
                    let x = match st.resident.clone() {
                        Some(r) if st.resident_on => match (r.concat)(&st.values[ix(a)], &st.values[ix(b)])? {
                            Some(x) => x,
                            None => value(st)?,
                        },
                        _ => value(st)?,
                    };
                    st.values[n] = x;
                    Ok(())
                })
            }
            StatementNode::Assign(n, e @ Expr::Prim(Primitive::Array_range, args)) if args.len() == 1 && self.tail_ranges.iter().any(|(k, _)| k == n) => {
                // a range of a kept array's count: the device's index buffer while the array stays there,
                // else made from the array's count (a kept count is its capacity)
                let value = self.expression(e);
                let count = args[0].1;
                let (source, div) = self.tail_ranges.iter().find(|(k, _)| k == n).unwrap().1;
                let n = *n;
                Arc::new(move |st| match st.resident.clone() {
                    None => {
                        let x = value(st)?;
                        st.values[ix(n)] = x;
                        Ok(())
                    }
                    Some(r) => {
                        let made = if !st.kept.contains(&n) {
                            None
                        } else {
                            match &st.values[ix(count)] {
                                Value::Int(length) => (r.range)(&program.locals[ix(n)], &st.values[ix(source)], div, *length)?,
                                _ => None,
                            }
                        };
                        match made {
                            Some(x) => st.values[ix(n)] = x,
                            None => {
                                if !st.kept.is_empty() { release_kept(&r, st)?; }
                                st.values[ix(count)] = Value::Int(held_length(st, &st.values[ix(source)])? / div);
                                let x = value(st)?;
                                st.values[ix(n)] = x;
                            }
                        }
                        Ok(())
                    }
                })
            }
            StatementNode::Assign(n, e @ Expr::Prim(Primitive::Array_range, args)) => {
                let value = self.expression(e);
                let count = arg(args, "count").expect("count");
                let n = *n;
                Arc::new(move |st| {
                    let mut result = None;
                    if let Some(r) = st.resident.clone() {
                        if st.resident_on && program.locals[ix(n)] == Ty::Array(Box::new(Ty::Int)) {
                            let length = op::array_count(Some(op::MAX_WOSIZE), "array/range", &st.values[ix(count)])? as i64;
                            result = (r.range)(&program.locals[ix(n)], &Value::Int(length), 1, length)?;
                        }
                    }
                    let x = match result { Some(x) => x, None => value(st)? };
                    st.values[ix(n)] = x;
                    Ok(())
                })
            }
            StatementNode::Assign(n, e @ Expr::Prim(Primitive::Array_int, args)) => {
                let value = self.expression(e);
                let count = arg(args, "count").expect("count");
                let initial = arg(args, "value");
                let n = *n;
                Arc::new(move |st| {
                    let mut made = None;
                    if let Some(r) = st.resident.clone() {
                        if st.resident_on {
                            let length = op::array_count(Some(op::MAX_WOSIZE), "array/int", &st.values[ix(count)])? as i64;
                            let initial = match initial { Some(i) => st.values[ix(i)].clone(), None => Value::Int(0) };
                            made = (r.fill)(&program.locals[ix(n)], length, &initial)?;
                        }
                    }
                    let x = match made { Some(x) => x, None => value(st)? };
                    st.values[ix(n)] = x;
                    Ok(())
                })
            }
            StatementNode::Assign(n, e @ Expr::Prim(Primitive::Array_count, args)) if args.len() == 1 => {
                // a resident array's length is a capacity: its count is the host array's
                let value = self.expression(e);
                let arg = args[0].1;
                let n = ix(*n);
                Arc::new(move |st| {
                    let mut made = None;
                    if let Some(r) = st.resident.clone() {
                        if st.resident_on { made = (r.count)(&st.values[ix(arg)])?; }
                    }
                    let x = match made { Some(count) => Value::Int(count), None => value(st)? };
                    st.values[n] = x;
                    Ok(())
                })
            }
            StatementNode::Assign(n, e) => {
                let value = self.expression(e);
                let n = ix(*n);
                enum Ownership { Fresh, Alias(i64), No }
                let ownership = match e {
                    Expr::Prim(Primitive::Sequence_build, _) => Ownership::Fresh,
                    Expr::Local(old) => Ownership::Alias(*old),
                    _ => Ownership::No,
                };
                if !self.track || plain(&program.locals[n]) {
                    Arc::new(move |st| {
                        let x = value(st)?;
                        st.values[n] = x;
                        Ok(())
                    })
                } else {
                    Arc::new(move |st| {
                        let x = value(st)?;
                        match &ownership {
                            Ownership::Fresh => {
                                if st.builders.is_empty() { st.builders = vec![None; program.locals.len()]; }
                                st.builders[n] = Some(Arc::new(AtomicBool::new(true)));
                            }
                            Ownership::Alias(old) => {
                                if !st.builders.is_empty() { st.builders[n] = st.builders[ix(*old)].clone(); }
                            }
                            Ownership::No => {
                                if !st.builders.is_empty() { st.builders[n] = None; }
                            }
                        }
                        st.values[n] = x;
                        Ok(())
                    })
                }
            }
            StatementNode::Reset_call(slot) => {
                let slot = ix(*slot);
                Arc::new(move |st| {
                    st.calls[slot] = 0;
                    Ok(())
                })
            }
            StatementNode::Draw(d) => {
                let run = core_marks::compile(d);
                Arc::new(move |st| {
                    let State { values, marks, .. } = st;
                    match marks {
                        Some(sink) => run(values, sink),
                        None => Err(v::fail("E_CORE", "Drawing statements need a mark table.")),
                    }
                })
            }
            StatementNode::Diagnose_rows { rows, policy, index, handler, diagnostic, result } => {
                let handler = self.plain_block(handler);
                let (rows, policy, index, diagnostic, result) = (*rows, *policy, *index, *diagnostic, *result);
                Arc::new(move |st| {
                    let rows_value = st.values[ix(rows)].clone();
                    let failure: std::cell::RefCell<Option<Diagnostic>> = std::cell::RefCell::new(None);
                    let resolve = st.resolve.clone();
                    let select = |value: &Value, first: i64| -> i64 {
                        match (&resolve, policy) {
                            (None, DiagnosticPolicy::First) => first,
                            (None, DiagnosticPolicy::RangeLast) => {
                                *failure.borrow_mut() = Some(v::fail("E_DIAGNOSTIC_CONTEXT", "Range-last diagnosis needs its native host."));
                                -1
                            }
                            (Some(resolve), policy) => {
                                let policy = match policy {
                                    DiagnosticPolicy::First => "first",
                                    DiagnosticPolicy::RangeLast => "range-last",
                                };
                                let source = Value::Source(
                                    "$core/diagnostic_index".into(),
                                    Ty::Int,
                                    vec![("rows".into(), value.clone()), ("policy".into(), text_value(policy))],
                                );
                                match resolve(&source) {
                                    Ok(Value::Int(i)) => i,
                                    Ok(_) => {
                                        *failure.borrow_mut() = Some(v::fail("E_DIAGNOSTIC_CONTEXT", "Host diagnosis returns one integer row."));
                                        -1
                                    }
                                    Err(d) => {
                                        *failure.borrow_mut() = Some(d);
                                        -1
                                    }
                                }
                            }
                        }
                    };
                    let cell = std::cell::RefCell::new(&mut *st);
                    let map = |value: Value| -> Result<Value, Diagnostic> {
                        let mut s = cell.borrow_mut();
                        s.values[ix(index)] = value;
                        handler(&mut **s)?;
                        seal(&**s, diagnostic);
                        Ok(s.values[ix(diagnostic)].clone())
                    };
                    let outcome = v::diagnose_rows(&select, &map, &rows_value);
                    drop(cell);
                    let outcome = match (outcome, failure.into_inner()) {
                        (_, Some(d)) => Err(d),
                        (o, None) => o,
                    }?;
                    st.values[ix(result)] = outcome;
                    Ok(())
                })
            }
            StatementNode::Protect { body, value, failure, handler, mapped, result } => {
                let body = self.plain_block(body);
                let handler = self.plain_block(handler);
                let (value, failure, mapped, result) = (*value, *failure, *mapped, *result);
                Arc::new(move |st| {
                    let cell = std::cell::RefCell::new(&mut *st);
                    let protected = || -> Result<Value, Diagnostic> {
                        let mut s = cell.borrow_mut();
                        body(&mut **s)?;
                        seal(&**s, value);
                        Ok(s.values[ix(value)].clone())
                    };
                    let map = |diagnostic: Value| -> Result<Value, Diagnostic> {
                        let mut s = cell.borrow_mut();
                        s.values[ix(failure)] = diagnostic;
                        handler(&mut **s)?;
                        seal(&**s, mapped);
                        Ok(s.values[ix(mapped)].clone())
                    };
                    let outcome = v::with_error(&protected, &map);
                    drop(cell);
                    st.values[ix(result)] = outcome?;
                    Ok(())
                })
            }
            StatementNode::If { condition, yes, no } => {
                let yes = self.block(yes, false);
                let no = self.block(no, false);
                let condition = ix(*condition);
                Arc::new(move |st| if v::truthy(&st.values[condition])? { yes(st) } else { no(st) })
            }
            StatementNode::Iterate { sequence, index, item, limit, body } => {
                let (setup, body) = self.loop_body(body);
                let (sequence, index, item, limit) = (ix(*sequence), ix(*index), ix(*item), *limit);
                Arc::new(move |st| {
                    let source = match st.resident.clone() {
                        Some(r) if st.resident_on => host_value(&r, &st.values[sequence])?,
                        _ => st.values[sequence].clone(),
                    };
                    let count = match &source {
                        Value::List(xs) => xs.len() as i64,
                        other => v::array_length(other)? as i64,
                    };
                    if count > limit { return Err(v::fail("E_ITER_BOUND", "Core iteration exceeds its bound.")); }
                    setup(st)?;
                    for i in 0..count {
                        if i & 1023 == 0 { e::interruptible(&mut || {})?; }
                        st.values[index] = Value::Int(i);
                        st.values[item] = match &source {
                            Value::List(xs) => xs[i as usize].clone(),
                            other => v::array_get(other, i)?,
                        };
                        body(st)?;
                    }
                    Ok(())
                })
            }
            StatementNode::Counted { first, last, count, index, item, limit, stop, init, body } => {
                let shape = if stop.is_none() { unfold_shape(&program, s, body, *index, *item) } else { None };
                let init = self.plain_block(init);
                let (setup, body) = self.loop_body(body);
                let (first, last, count, index, item, limit, stop) = (ix(*first), ix(*last), ix(*count), ix(*index), ix(*item), *limit, stop.map(ix));
                let normal: Run = {
                    let (init, setup, body) = (init.clone(), setup.clone(), body.clone());
                    Arc::new(move |st| {
                        let first_v = v::int_of(&st.values[first])?;
                        let last_v = v::int_of(&st.values[last])?;
                        let length = (last_v as i128) - (first_v as i128);
                        if length > limit as i128 { return Err(v::fail("E_ITER_BOUND", "Core iteration exceeds its bound.")); }
                        let length = if length <= 0 { 0 } else { length as i64 };
                        st.values[count] = Value::Int(length);
                        init(st)?;
                        setup(st)?;
                        match stop {
                            None => {
                                for i in 0..length {
                                    if i & 1023 == 0 { e::interruptible(&mut || {})?; }
                                    st.values[index] = Value::Int(i);
                                    st.values[item] = Value::Int(first_v + i);
                                    body(st)?;
                                }
                            }
                            Some(stop) => {
                                let mut i = 0;
                                while i < length && !v::truthy(&st.values[stop])? {
                                    if i & 1023 == 0 { e::interruptible(&mut || {})?; }
                                    st.values[index] = Value::Int(i);
                                    st.values[item] = Value::Int(first_v + i);
                                    body(st)?;
                                    i += 1;
                                }
                            }
                        }
                        Ok(())
                    })
                };
                match shape {
                    None => normal,
                    Some(u) => {
                        // an iterate appending one region's pass to a history, on the device: its passes in
                        // one dispatch (each lane runs them all), the history joined once
                        let pre = self.plain_block(&u.pre);
                        Arc::new(move |st| {
                            let first_v = v::int_of(&st.values[first])?;
                            let last_v = v::int_of(&st.values[last])?;
                            let length = last_v - first_v;
                            let r = match st.resident.clone() {
                                Some(r) if st.resident_on && length > 1 && length <= limit => r,
                                _ => return normal(st),
                            };
                            st.values[count] = Value::Int(length);
                            st.values[index] = Value::Int(0);
                            st.values[item] = Value::Int(first_v);
                            pre(st)?;
                            let argv: Vec<Value> = u.args.iter().map(|n| st.values[ix(*n)].clone()).collect();
                            let region = &program.regions[ix(u.region)];
                            // the region's one source is the carried head itself, and no uniform the history
                            let fits = match packed::host::bind_arguments(region, &argv, Some(false)) {
                                Err(_) => false,
                                Ok(bound) => {
                                    let is = |x: &Value, shape: &packed::host::Shape| match shape {
                                        packed::host::Shape::Value(y) => phys_eq(y, x),
                                        packed::host::Shape::Range(y, _, _) => phys_eq(y, x),
                                        packed::host::Shape::Opaque(Some(ys)) => ys.iter().any(|y| phys_eq(y, x)),
                                        packed::host::Shape::Opaque(None) => true,
                                    };
                                    // its source is checked where it is bound: the device knows its input
                                    let uniforms = packed::host::uniform_shapes(&bound);
                                    packed::host::source_shapes(&bound).len() == 1
                                        && !uniforms.iter().any(|s| is(&st.values[ix(u.points)], s))
                                        && !uniforms.iter().any(|s| is(&st.values[ix(u.head)], s))
                                }
                            };
                            let un = if fits {
                                (r.unfold)(region, &argv, length, &st.values[ix(u.head)], &st.values, &st.live)?
                            } else {
                                None
                            };
                            let joined = match un {
                                Some(history) => (r.concat)(&st.values[ix(u.points)], &history)?,
                                None => None,
                            };
                            match joined {
                                Some(j) => {
                                    st.values[ix(u.points)] = j;
                                    Ok(())
                                }
                                None => normal(st),
                            }
                        })
                    }
                }
            }
            StatementNode::Store { sequence, index, value } => {
                let (sequence, index, value) = (*sequence, *index, *value);
                let aliases = self.aliases[ix(sequence)].clone();
                Arc::new(move |st| {
                    let writable = if st.builders.is_empty() { false } else { st.builders[ix(sequence)].as_ref().map(|w| w.load(Ordering::Relaxed)).unwrap_or(false) };
                    if !writable { return Err(v::fail("E_CORE_OWNERSHIP", "Core stores require an unpublished local builder.")); }
                    let i = v::int_of(&st.values[ix(index)])?;
                    // the builder is unpublished: take it, write in place, put it back
                    let x = st.values[ix(value)].clone();
                    let mut target = std::mem::replace(&mut st.values[ix(sequence)], Value::No_geo);
                    if matches!(target, Value::List(_)) { seal(st, value); }
                    let stored = store_value(&mut target, i, &x);
                    st.values[ix(sequence)] = target;
                    stored?;
                    // an alias of the builder (a copy of the local) stands for the same array
                    for m in &aliases {
                        let same = match (&st.builders[ix(*m)], &st.builders[ix(sequence)]) {
                            (Some(a), Some(b)) => Arc::ptr_eq(a, b),
                            _ => false,
                        };
                        if same { st.values[ix(*m)] = st.values[ix(sequence)].clone(); }
                    }
                    Ok(())
                })
            }
            StatementNode::State_read { slot, result, init, initial } => {
                let init = self.plain_block(init);
                let key = self.slot_keys[ix(*slot)].clone();
                let (result, initial) = (ix(*result), ix(*initial));
                Arc::new(move |st| {
                    let (instance, path, iter, host) = key(st)?;
                    if !st.builders.is_empty() { st.builders[result] = None; }
                    let root = state_root(st)?;
                    let failed: std::cell::RefCell<Option<Diagnostic>> = std::cell::RefCell::new(None);
                    let cell = std::cell::RefCell::new(&mut *st);
                    let seed = || -> Value {
                        let mut s = cell.borrow_mut();
                        if let Err(d) = init(&mut **s) {
                            *failed.borrow_mut() = Some(d);
                            return Value::No_geo;
                        }
                        // a seed the device computed stays there (Flow.Eval.Device)
                        let r = s.resident.clone();
                        match r {
                            Some(r) if s.resident_on => match kept_value(&r, &s.values[initial]) {
                                Ok(x) => x,
                                Err(d) => {
                                    *failed.borrow_mut() = Some(d);
                                    Value::No_geo
                                }
                            },
                            _ => s.values[initial].clone(),
                        }
                    };
                    let value = e::Residual::state_read(root, instance, path, iter, host, &seed);
                    drop(cell);
                    if let Some(d) = failed.into_inner() { return Err(d); }
                    st.values[result] = value?;
                    Ok(())
                })
            }
            StatementNode::State_peek { slot, found, value } => {
                let key = self.slot_keys[ix(*slot)].clone();
                let (found, value) = (ix(*found), ix(*value));
                Arc::new(move |st| {
                    let (instance, path, iter, host) = key(st)?;
                    let root = state_root(st)?;
                    let pending = e::Residual::state_peek(root, instance, path, iter, host);
                    st.values[found] = Value::Bool(pending.is_some());
                    st.values[value] = pending.unwrap_or(Value::No_geo);
                    Ok(())
                })
            }
            StatementNode::State_write { slot, value } => {
                let key = self.slot_keys[ix(*slot)].clone();
                let value = ix(*value);
                Arc::new(move |st| {
                    // a step the device computed stays there, its host copy read only when one is asked for
                    if let Some(r) = st.resident.clone() {
                        if st.resident_on {
                            let kept = kept_value(&r, &st.values[value])?;
                            st.values[value] = kept;
                        }
                    }
                    let (instance, path, iter, host) = key(st)?;
                    seal(st, value as i64);
                    let root = state_root(st)?;
                    st.values[value] = e::Residual::state_write(root, instance, path, iter, host, st.values[value].clone())?;
                    Ok(())
                })
            }
            StatementNode::Invoke { region, args, result } => {
                let run = prepare_invoke(
                    (*program.regions[ix(*region)]).clone(),
                    args.clone(),
                    *result,
                    self.device_ok[ix(*region)],
                    self.static_ok[ix(*result)],
                    self.clobber,
                );
                self.individual.lock().unwrap()[ix(*region)] = Some((s.source.span.clone(), run.clone()));
                match &self.fused[ix(*region)] {
                    None => run,
                    Some(group) if group.at_first && group.members[0] == *region => {
                        let tys = group.members.iter().map(|r| program.regions[ix(*r)].term.ty().clone()).collect();
                        prepare_first(group.clone(), tys, run)
                    }
                    Some(_) if self.fused[ix(*region)].as_ref().is_some_and(|g| g.at_first) => {
                        let region = ix(*region);
                        Arc::new(move |st| if st.fused_done.get(region).copied().unwrap_or(false) { st.fused_done[region] = false; Ok(()) } else { run(st) })
                    }
                    Some(group) if *group.members.last().expect("members") != *region => {
                        Arc::new(move |st| if !grouped(st) { run(st) } else { Ok(()) })
                    }
                    Some(group) => {
                        let individual = self.individual.lock().unwrap();
                        let members: Vec<(Option<c::Span>, Run)> = group
                            .members
                            .iter()
                            .map(|r| individual[ix(*r)].clone().expect("member prepared"))
                            .collect();
                        let tys = group.members.iter().map(|r| program.regions[ix(*r)].term.ty().clone()).collect();
                        prepare_group(group.clone(), tys, members)
                    }
                }
            }
        }
    }
}

fn state_root(st: &State) -> Result<ResidualId, Diagnostic> {
    st.root.ok_or_else(|| v::fail("E_CORE_STATE", "Core state needs its residual's state."))
}

/// `prepare ~remember_loops` (a drawing) keeps each top-level loop's last 4 results keyed by the
/// values it reads from outside itself, and skips it when they repeat; a loop that reads the
/// frame, state, a host or a random stream, or draws, always runs. `clobber` (tests): an array the
/// program's arena drops is filled with NaN or `min_int` as it dies, so a read past its last use
/// fails loudly.
pub fn prepare(program: &Program, clobber: bool, remember_loops: bool, approx: &Paths, fast: bool) -> Prepared {
    let program = Arc::new(program.clone());
    let track = builds(&program.body);
    let (device_ok, groups) = core_draw::device_groups(
        approx,
        fast,
        &program,
        &|region| match program.regions[ix(region)].outputs.as_slice() {
            [output] => Some(output.len() as i64),
            _ => None,
        },
        &|region| packed::gpu_refusals(true, fast, region).is_empty(),
    );
    let mut fused: Vec<Option<Arc<core_draw::Group>>> = vec![None; program.regions.len()];
    for group in groups {
        if group.members.len() > 1 {
            let group = Arc::new(group);
            for region in &group.members { fused[ix(*region)] = Some(group.clone()); }
        }
    }
    let static_ok = core_draw::static_invokes(&program);
    // the resident loops whose arrays may stay on the device after them, and their ranges
    let mut tails: Vec<(usize, Vec<i64>)> = Vec::new();
    let mut tail_ranges: Vec<(i64, (i64, i64))> = Vec::new();
    if remember_loops {
        for s in &program.body {
            if let Some((kept, ranges)) = resident_tail(&program, s, &|r| device_ok[ix(r)]) {
                tail_ranges.extend(ranges);
                tails.push((addr(s), kept));
            }
        }
    }
    let constants: Vec<Value> = program.constants.iter().map(c::value_of_literal).collect();
    let slot_keys: Vec<SlotKey> = program
        .state_slots
        .iter()
        .map(|slot| -> SlotKey {
            let mut path = slot.prefix.clone();
            path.extend(slot.zone.iter().cloned());
            let host = slot.plane == Plane::Host;
            let instance = slot.source.instance;
            let iter = slot.source.iter.clone();
            if iter.iter().all(|i| matches!(i, c::Iteration::Static_index(_))) {
                let fixed: Vec<i64> = iter.iter().map(|i| if let c::Iteration::Static_index(i) = i { *i } else { 0 }).collect();
                Arc::new(move |_| Ok((instance, path.clone(), fixed.clone(), host)))
            } else {
                Arc::new(move |st| {
                    let mut out = Vec::with_capacity(iter.len());
                    for i in &iter {
                        out.push(match i {
                            c::Iteration::Static_index(i) => *i,
                            c::Iteration::Local_index(l) => v::int_of(&st.values[ix(*l)])?,
                        });
                    }
                    Ok((instance, path.clone(), out, host))
                })
            }
        })
        .collect();
    let mut writes = vec![0i64; program.locals.len()];
    c::iter(
        &mut |s: &Statement| match &s.node {
            StatementNode::Reset_call(_) | StatementNode::Store { .. } | StatementNode::Draw(_) => {}
            StatementNode::State_write { value, .. } => writes[ix(*value)] += 1,
            _ => {
                for w in c::writes(s) { writes[ix(w)] += 1; }
            }
        },
        &program.body,
    );
    let planned = arena_plan(remember_loops, &|region, result| fused[ix(region)].is_some() || static_ok[ix(result)], &program);
    // locals copied from one another (Local aliases), for a store through a builder alias
    let mut aliases: Vec<Vec<i64>> = vec![Vec::new(); program.locals.len()];
    {
        let mut parent: Vec<usize> = (0..program.locals.len()).collect();
        fn find(parent: &mut [usize], x: usize) -> usize {
            let mut r = x;
            while parent[r] != r { r = parent[r]; }
            let mut y = x;
            while parent[y] != r { let next = parent[y]; parent[y] = r; y = next; }
            r
        }
        c::iter(
            &mut |s: &Statement| {
                if let StatementNode::Assign(a, Expr::Local(b)) = &s.node {
                    let (ra, rb) = (find(&mut parent, ix(*a)), find(&mut parent, ix(*b)));
                    if ra != rb { parent[ra] = rb; }
                }
            },
            &program.body,
        );
        let n = program.locals.len();
        let roots: Vec<usize> = (0..n).map(|x| find(&mut parent, x)).collect();
        // members per root (ascending), so the pass is linear in the locals
        let mut members: Vec<Vec<i64>> = vec![Vec::new(); n];
        for (y, root) in roots.iter().enumerate() { members[*root].push(y as i64); }
        for x in 0..n {
            let group = &members[roots[x]];
            if group.len() > 1 { aliases[x] = group.iter().copied().filter(|y| *y != x as i64).collect(); }
        }
    }
    let ctx = Ctx {
        program: program.clone(),
        track,
        device_ok,
        fused,
        individual: Mutex::new(vec![None; program.regions.len()]),
        static_ok,
        tails,
        tail_ranges,
        constants,
        slot_keys,
        hoisted_writes: writes,
        planned: planned.clone(),
        remember_loops,
        clobber,
        in_loop: std::cell::Cell::new(false),
        aliases,
    };
    let body = ctx.block(&program.body, true);
    let pooled = program.locals.len() > 256 || program.locals.iter().all(plain);
    let stateless = program.state_slots.is_empty() && program.regions.is_empty();
    let scalar = scalar_program(&program);
    Prepared {
        program,
        body,
        pool: Mutex::new(None),
        pooled,
        stateless,
        scalar,
        arena_plan: planned,
        arena_pool: Mutex::new(None),
        arena_kills: clobber,
    }
}

impl Prepared {
    /// The program invokes a region, so `execute` needs a `force`.
    pub fn has_regions(&self) -> bool { !self.program.regions.is_empty() }

    /// Stateless and free of host sources: `execute` without a root needs no checkpoint or resolver.
    pub fn direct(&self) -> bool {
        let mut needs_resolver = false;
        c::iter(
            &mut |s: &Statement| {
                if let StatementNode::Assign(_, Expr::Source(_) | Expr::Host_source { .. } | Expr::Source_descriptor { .. }) = &s.node { needs_resolver = true; }
            },
            &self.program.body,
        );
        self.stateless && !needs_resolver
    }
}
pub fn has_regions(prepared: &Prepared) -> bool { prepared.has_regions() }
pub fn direct(prepared: &Prepared) -> bool { prepared.direct() }

fn call_counters(program: &Program) -> Vec<i64> { vec![0; program.call_slots as usize] }

/// Only a pure CPU run drops arrays: a device keeps uploads by an array's identity, and an
/// enclosing run's `Scope` finds results by it.
fn arena_for(prepared: &Prepared, device: bool) -> Option<Arena> {
    match &prepared.arena_plan {
        Some(plan) if !device && packed::host::scope_idle() => match prepared.arena_pool.lock().unwrap().take() {
            Some(mut a) => {
                a.verified.iter_mut().for_each(|x| *x = false);
                Some(a)
            }
            None => Some(arena_new(plan, prepared.arena_kills)),
        },
        _ => None,
    }
}

/// The optional arguments of [`execute`].
#[derive(Clone)]
pub struct Options {
    /// the residual whose state a state statement uses
    pub root: Option<ResidualId>,
    pub strict: bool,
    pub rerun_on_error: bool,
    pub on_walk: Option<Arc<dyn Fn()>>,
    pub force: Option<ForceFn>,
    pub resolve: Option<ResolveFn>,
    pub resident: Option<Arc<Resident>>,
    pub host_source: Option<HostSourceFn>,
}
impl Default for Options {
    fn default() -> Options {
        Options { root: None, strict: false, rerun_on_error: true, on_walk: None, force: None, resolve: None, resident: None, host_source: None }
    }
}

fn new_state(prepared: &Prepared, values: Vec<Value>, arena: Option<Arena>, live: &Frame, o: &Options) -> State {
    State {
        id: STATE_IDS.fetch_add(1, Ordering::Relaxed),
        arena,
        values,
        builders: Vec::new(),
        fused_done: Vec::new(),
        calls: call_counters(&prepared.program),
        live: live.clone(),
        root: o.root,
        force: o.force.clone(),
        resolve: o.resolve.clone(),
        host_source: o.host_source.clone(),
        marks: None,
        device: None,
        resident: o.resident.clone(),
        resident_on: o.resident.is_some(),
        kept: Vec::new(),
    }
}

fn general(prepared: &Prepared, live: &Frame, o: &Options) -> Result<Value, Diagnostic> {
    e::interruptible(&mut || {})?;
    if let Err(message) = input::validate(live) {
        return Err(v::fail("E_FRAME", &message));
    }
    if let (Some(sc), None) = (&prepared.scalar, &o.resident) {
        if let Some(x) = run_scalar(sc, live) { return Ok(Value::Float(x)); }
    }
    general_values(prepared, live, o)
}

fn general_values(prepared: &Prepared, live: &Frame, o: &Options) -> Result<Value, Diagnostic> {
    let program = &prepared.program;
    let n = program.locals.len();
    let values = if !prepared.pooled {
        vec![Value::No_geo; n]
    } else {
        match prepared.pool.lock().unwrap().take() {
            Some(values) if !values.is_empty() => values,
            _ => vec![Value::No_geo; n],
        }
    };
    let arena = arena_for(prepared, o.resident.is_some());
    let mut st = new_state(prepared, values, arena, live, o);
    let result = (|| -> Result<Value, Diagnostic> {
        (prepared.body)(&mut st)?;
        if let Some(r) = &o.resident { (r.settle)()?; }
        if program.results.len() != 1 { return Err(v::fail("E_CORE", "Expected one core result.")); }
        Ok(st.values[ix(program.results[0])].clone())
    })();
    if prepared.pooled { *prepared.pool.lock().unwrap() = Some(std::mem::take(&mut st.values)); }
    if let Some(mut a) = st.arena.take() {
        arena_finish(&mut a);
        *prepared.arena_pool.lock().unwrap() = Some(a);
    }
    result
}

/// State needs its residual even in strict mode; strict failures restore it without entering the
/// tree walker. Ordinary failures retain the evaluator's fallback.
pub fn execute(prepared: &Prepared, live: &Frame, o: &Options) -> Result<Value, Diagnostic> {
    let restore: Box<dyn Fn()> = match o.root {
        Some(r) if !prepared.stateless => e::Residual::checkpoint(r)?,
        _ => Box::new(|| {}),
    };
    let close = || {
        if let Some(r) = &o.resident { (r.close)(); }
    };
    let result = loop {
        match general(prepared, live, o) {
            Err(d) if is_resident_overflow(&d) && o.resident.is_some() => {
                close();
                restore();
            }
            other => break other,
        }
    };
    // state runs on the device too: what it stores is read back (State_read's seed, State_write)
    if result.is_err() { close(); }
    match o.root {
        None => result,
        Some(r) => match result {
            Ok(_) => result,
            Err(d) if d.code == "E_CORE_RESIDENT" => Err(d),
            Err(d) if d.code == "cancelled" || d.code == "E_CANCELLED" || o.strict || o.resolve.is_some() || !o.rerun_on_error => {
                restore();
                Err(d)
            }
            Err(_) => {
                restore();
                e::Residual::rerun(r, o.on_walk.as_deref())
            }
        },
    }
}

/// Run a draw program (`Core_draw::lower`) into one mark table in painter order. CPU attribute
/// arrays are registered in `buffers`, which the caller owns, clears per frame and reads while it
/// adapts the table. `root` is the residual whose state the program reads. `image` resolves an
/// image node to a host resource id. A failure is the core's own diagnostic.
///
/// `device` offers each region whose results are read only as the positions, sizes, radius, from,
/// to, fill, stroke or color of a mark set (the others are the frame program's): `Some(outputs)`
/// supplies `(token, count, width)` for every region output in order; `None` runs the region on
/// the CPU as usual.
pub struct DrawOptions {
    pub root: Option<ResidualId>,
    pub force: Option<ForceFn>,
    pub resolve: Option<ResolveFn>,
    pub device: Option<DeviceFn>,
    pub resident: Option<Arc<Resident>>,
    pub image: Option<ImageFn>,
}
impl Default for DrawOptions {
    fn default() -> DrawOptions { DrawOptions { root: None, force: None, resolve: None, device: None, resident: None, image: None } }
}

pub fn draw(prepared: &Prepared, live: &Frame, size: (i64, i64), buffers: &Buffers, o: &DrawOptions) -> Result<scene_command::marks::Table, Diagnostic> {
    if let Err(message) = input::validate(live) {
        return Err(v::fail("E_FRAME", &message));
    }
    let program = &prepared.program;
    let image = o.image.clone();
    let sink = core_marks::sink(image, size, buffers);
    let arena = arena_for(prepared, o.device.is_some() || o.resident.is_some());
    let options = Options { root: o.root, force: o.force.clone(), resolve: o.resolve.clone(), resident: o.resident.clone(), ..Options::default() };
    let mut st = new_state(prepared, vec![Value::No_geo; program.locals.len()], arena, live, &options);
    st.host_source = None;
    st.marks = Some(sink);
    st.device = o.device.clone();
    st.resident_on = false;
    // a resident loop's kept arrays live until the draw ends (closing an idle one is a no-op)
    let ran = (prepared.body)(&mut st);
    if let Some(r) = &o.resident { (r.close)(); }
    ran?;
    if let Some(mut a) = st.arena.take() {
        arena_finish(&mut a);
        *prepared.arena_pool.lock().unwrap() = Some(a);
    }
    core_marks::finish(st.marks.as_ref().expect("marks"))
}

#[allow(dead_code)]
fn _unused(_: &Draw) {}

/// Write element `i` of an unpublished builder in place (a shared array is copied first; the
/// caller refreshes the builder's aliases).
fn store_value(target: &mut Value, i: i64, x: &Value) -> Result<(), Diagnostic> {
    let range_error = || v::fail("E_LIST_RANGE", "Core store is outside its sequence.");
    let width = match target { Value::Vec2_array(_) => 2, Value::Vec3_array(_) => 3, Value::Vec4_array(_) => 4, _ => 1 };
    match target {
        Value::Text_array(data) => {
            if i < 0 || i as usize >= data.len() { return Err(range_error()); }
            match x {
                Value::Text(text) => Arc::make_mut(data)[i as usize] = text.clone(),
                _ => return Err(v::fail("E_ARRAY_TYPE", "Text arrays contain text values.")),
            }
            Ok(())
        }
        Value::Int_array(data) => {
            if i < 0 || i as usize >= data.len() { return Err(range_error()); }
            v::int_array_put(Arc::make_mut(data).as_mut_slice(), i as usize, x)
        }
        Value::Float_array(data) | Value::Vec2_array(data) | Value::Vec3_array(data) | Value::Vec4_array(data) => {
            if i < 0 || i as usize >= data.len() / width { return Err(range_error()); }
            v::array_put(Arc::make_mut(data).as_mut_slice(), width, i as usize, x)
        }
        Value::List(xs) => {
            if i < 0 || i as usize >= xs.len() { return Err(range_error()); }
            Arc::make_mut(xs)[i as usize] = x.clone();
            Ok(())
        }
        other => {
            v::list_arg(other)?;
            Err(range_error())
        }
    }
}
