//! Port of lib/flow_ir/core.ml(i): structured frame programs. Locals and constants are numbered in
//! authored traversal order; values are immutable after publication. OCaml `Core` is `core_ir` here.
//!
//! Naming: variants keep their OCaml spelling (`Array_max_length`), types are CamelCase, OCaml's
//! `Core.Region` is the module [`Region`] with the record `Region::T` (alias [`RegionT`]). All ints
//! are i64 (registers, locals, constants are indices; cast with `as usize` where indexing).
//! `'a live` is concretized as [`Region::LiveInt`] / [`Region::LiveFloat`].
//! `Region::T` fields are all `pub` for reading; build one only through `make`/`rebuild`.
#![allow(non_camel_case_types, non_snake_case)]

use flow_check::workspace::{DiagnosticPolicy, Path, Term};
use flow_syntax::packed_ops;
pub use flow_syntax::packed_ops::{ByteMode, Comparison, Integer, Primitive, primitive_name};
use flow_syntax::ty::{self, Ty};
use flow_syntax::value::{self, ResidualId, Value};
pub use flow_syntax::Diagnostic;
pub use rays_diagnostic::Span;
use std::sync::Arc;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Text(Vec<u8>),
    Vector(Vec<f64>),
    List(Vec<Literal>),
    Record(Vec<(String, Literal)>),
    Array { width: i64, data: Arc<Vec<f64>> },
    Int_array(Arc<Vec<i64>>),
    Text_array(Arc<rays_math::text_array::TextArray>),
    Empty_geometry,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FrameField { Time, Dt, Index, Tick, Width, Height, Pointer_x, Pointer_y, Input }

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Constant(i64),
    Local(i64),
    Frame(FrameField),
    Call_index(i64),
    /// Immutable closure-free host source, materialized only when this expression executes.
    Source(Value),
    /// Construct a typed host source from computed arguments.
    Host_source { name: String, ty: Ty, args: Vec<(String, i64)> },
    /// Immutable host-array query retained for a cooked storage consumer.
    Source_descriptor { name: String, ty: Ty, args: Vec<(String, i64)> },
    Prim(Primitive, Vec<(String, i64)>),
    Vec(Vec<i64>),
    Fields(Vec<(String, i64)>),
    Items(Vec<i64>),
    Field(i64, String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Iteration { Static_index(i64), Local_index(i64) }

#[derive(Clone, Debug, PartialEq)]
pub struct Source {
    pub site: Path,
    pub owner: Path,
    pub instance: i64,
    pub prefix: Path,
    pub iter: Vec<Iteration>,
    pub span: Option<Span>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mark { Rect, Circle, Line }
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Transform { Translate, Rotate, Scale }
/// OCaml `Core.path`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PathKind { Rounded_rect, Polygon, Polyline }

#[derive(Clone, Debug, PartialEq)]
pub enum Draw {
    Background(i64),
    Point { at: i64, ink: i64 },
    /// One mark; the arguments are the operator's own input names.
    Mark { mark: Mark, args: Vec<(String, i64)> },
    /// A mark set; the count is that of `positions` (of `from` for lines).
    Batch { mark: Mark, args: Vec<(String, i64)> },
    Points { positions: i64, ink: i64 },
    Path { path: PathKind, args: Vec<(String, i64)> },
    Text_run { at: i64, text: i64, ink: i64, size: i64 },
    Image { source: (Ty, i64), at: i64, scale: i64, angle: i64 },
    Push(Transform, i64),
    Pop,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Statement {
    pub id: i64,
    pub source: Source,
    pub node: StatementNode,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StatementNode {
    Draw(Draw),
    Assign(i64, Expr),
    Reset_call(i64),
    Protect { body: Vec<Statement>, value: i64, failure: i64, handler: Vec<Statement>, mapped: i64, result: i64 },
    Diagnose_rows { rows: i64, policy: DiagnosticPolicy, index: i64, handler: Vec<Statement>, diagnostic: i64, result: i64 },
    If { condition: i64, yes: Vec<Statement>, no: Vec<Statement> },
    Iterate { sequence: i64, index: i64, item: i64, limit: i64, body: Vec<Statement> },
    /// Exact integer induction after the source's endpoint conversion.
    Counted { first: i64, last: i64, count: i64, index: i64, item: i64, limit: i64, stop: Option<i64>, init: Vec<Statement>, body: Vec<Statement> },
    Store { sequence: i64, index: i64, value: i64 },
    State_read { slot: i64, result: i64, init: Vec<Statement>, initial: i64 },
    /// `found` := whether this frame already holds a pending write; `value` := it.
    State_peek { slot: i64, found: i64, value: i64 },
    State_write { slot: i64, value: i64 },
    Invoke { region: i64, args: Vec<i64>, result: i64 },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Plane { Frame, Host }

#[derive(Clone, Debug, PartialEq)]
pub struct StateSlot {
    pub source: Source,
    pub prefix: Path,
    pub zone: Path,
    pub plane: Plane,
    pub ty: Ty,
}

/// A region: a pure numeric loop body over packed arrays, as a register program (instruction `n`
/// defines register `n` from earlier registers).
pub mod region {
    use super::*;
    use std::sync::{Mutex, OnceLock};

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Projection { Value, Valid }

    /// A noise's octave count: a constant, or a register (an Int register, checked where the noise runs).
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum LiveInt { Fixed(i64), Live(i64) }
    /// A noise's lacunarity or gain: a constant, or a register.
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub enum LiveFloat { Fixed(f64), Live(i64) }

    pub use flow_syntax::packed_ops::Conversion;

    #[derive(Clone, Debug, PartialEq)]
    pub struct Loop {
        pub first: i64,
        pub last: i64,
        pub finish: i64,
        pub initial: Vec<i64>,
        pub limit: Option<i64>,
        pub span: Span,
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Stage {
        Fwidth,
        Float_bits,
        Sample(i64, i64),
        Sample_level(i64, i64),
        Read(i64, i64),
        Texture_size(i64, i64),
        Trace_triangles(i64),
        Trace_any,
        Trace_curves(i64),
        Trace_curves_any,
        Scene_vertex(bool, i64),
        Scene_instance,
        Scene_sphere(i64),
        Scene_strand(i64),
        Group_scan(i64),
        Store,
        Noise_table(i64),
    }

    #[derive(Clone, Debug, PartialEq)]
    pub enum Instruction {
        Const(f64),
        /// source, width, component
        Input(i64, i64, i64),
        Uniform(i64, i64),
        Frame(String),
        Accumulator(i64),
        Binary(packed_ops::Binary, i64, i64),
        Unary(packed_ops::Unary, i64),
        Noise3(i64, i64, i64, i64, LiveInt, LiveFloat, LiveFloat),
        Fma(i64, i64, i64),
        Rand_at(i64, i64),
        Select(i64, i64, i64),
        Checked_mul(Projection, i64, i64),
        Checked_fma(Projection, i64, i64, i64),
        /// One field of a host ray query: registers scene id, origin, direction, tmin, tmax, time.
        Trace(i64, Vec<i64>),
        /// A raster-stage intrinsic over its operand registers.
        Stage(Stage, Vec<i64>),
        /// Execution-local immutable table capture, resolved before its index.
        Array_prepare(i64, Ty),
        /// Prepared table register, exact integer index, width and component.
        Gather(i64, i64, i64, i64),
        Int_gather(i64, i64),
        Text_const(Vec<u8>),
        Text_input(i64),
        Text_uniform(i64),
        Text_gather(i64, i64),
        Text_select(i64, i64, i64),
        Int_const(i64),
        Int_input(i64),
        Int_uniform(i64),
        Int_frame(String),
        Int_accumulator,
        Int_binary(Integer, i64, i64),
        Int_compare(Comparison, i64, i64),
        Int_abs(i64),
        Convert(Conversion, i64),
        Int_select(i64, i64, i64),
        Guarded(Vec<(i64, bool)>, Box<Instruction>),
        Raise(Ty, String, String, Option<Vec<u8>>),
        Loop_begin(Loop),
        Loop_carry(i64, i64, Ty),
        Loop_end(i64, Vec<i64>, Option<i64>),
        Loop_result(i64, i64, Ty),
    }

    impl Instruction {
        pub fn unguarded(&self) -> &Self {
            let mut current = self;
            while let Self::Guarded(_, body) = current { current = body; }
            current
        }

        /// Conditions in execution order, from outermost guard to innermost.
        pub fn guarded(&self) -> (Vec<(i64, bool)>, &Self) {
            let mut guards = Vec::new();
            let mut current = self;
            while let Self::Guarded(more, body) = current {
                guards.extend_from_slice(more);
                current = body;
            }
            (guards, current)
        }
    }

    pub fn integer_register(instruction: &Instruction) -> bool {
        use Instruction::*;
        match instruction {
            Int_const(_) | Int_input(_) | Int_uniform(_) | Int_frame(_) | Int_accumulator | Int_binary(..)
            | Int_abs(_) | Int_select(..) | Int_gather(..) => true,
            Raise(Ty::Int, ..) => true,
            Convert(Conversion::ToFloat, _) => false,
            Convert(..) => true,
            Guarded(_, body) => integer_register(body),
            Loop_begin(_) => true,
            Loop_carry(_, _, Ty::Int) | Loop_result(_, _, Ty::Int) => true,
            Stage(self::Stage::Trace_triangles(1 | 5) | self::Stage::Trace_curves(1) | self::Stage::Group_scan(_) | self::Stage::Store | self::Stage::Noise_table(_), _) => true,
            _ => false,
        }
    }

    pub fn text_register(instruction: &Instruction) -> bool {
        use Instruction::*;
        match instruction {
            Text_const(_) | Text_input(_) | Text_uniform(_) | Text_gather(..) | Text_select(..) => true,
            Raise(Ty::Text, ..) => true,
            Guarded(_, body) => text_register(body),
            Loop_carry(_, _, Ty::Text) | Loop_result(_, _, Ty::Text) => true,
            _ => false,
        }
    }

    pub fn operands(instruction: &Instruction) -> Vec<i64> {
        use Instruction::*;
        match instruction {
            Binary(_, a, b) | Int_binary(_, a, b) | Int_compare(_, a, b) | Rand_at(a, b) | Checked_mul(_, a, b)
            | Gather(a, b, _, _) | Int_gather(a, b) | Text_gather(a, b) => vec![*a, *b],
            Trace(_, registers) | Stage(_, registers) => registers.clone(),
            Unary(_, a) | Int_abs(a) | Convert(_, a) => vec![*a],
            Noise3(a, b, c, _, octaves, lacunarity, gain) => {
                let mut result = vec![*a, *b, *c];
                if let LiveInt::Live(r) = octaves { result.push(*r); }
                if let LiveFloat::Live(r) = lacunarity { result.push(*r); }
                if let LiveFloat::Live(r) = gain { result.push(*r); }
                result
            }
            Fma(a, b, c) | Select(a, b, c) | Int_select(a, b, c) | Text_select(a, b, c) | Checked_fma(_, a, b, c) => vec![*a, *b, *c],
            Guarded(guards, body) => {
                let mut result: Vec<i64> = guards.iter().map(|(c, _)| *c).collect();
                result.extend(operands(body));
                result
            }
            Loop_begin(Loop { first, last, initial, .. }) => {
                let mut result = vec![*first, *last];
                result.extend(initial.iter().copied());
                result
            }
            Loop_carry(loop_, _, _) => vec![*loop_],
            Loop_end(loop_, values, stop) => {
                let mut result = vec![*loop_];
                result.extend(values.iter().copied());
                result.extend(stop.iter().copied());
                result
            }
            Loop_result(loop_, component, _) => vec![*loop_, loop_.wrapping_add(1).wrapping_add(*component)],
            _ => Vec::new(),
        }
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum Iteration { Zip, Product }
    #[derive(Clone, Debug, PartialEq)]
    pub enum RegionResult { Collect, Sum(bool), Accumulate(Arc<Term>, bool) }
    pub type Origin = (Path, Vec<i64>);

    /// `count_source`: given a residual and its term, the origin of its element count.
    #[derive(Clone)]
    pub struct CountSource(pub Arc<dyn Fn(ResidualId, &Term) -> Option<Origin> + Send + Sync>);
    impl std::fmt::Debug for CountSource {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { f.write_str("<count_source>") }
    }
    impl CountSource {
        pub fn new(f: Arc<dyn Fn(ResidualId, &Term) -> Option<Origin> + Send + Sync>) -> CountSource { CountSource(f) }
        pub fn none() -> CountSource { CountSource(Arc::new(|_, _| None)) }
        pub fn call(&self, residual: ResidualId, term: &Term) -> Option<Origin> { (self.0)(residual, term) }
    }

    /// One CPU run of a region from a program's arena: `buffer` is a dead array its one output may
    /// overwrite (or `No_geo`); the run sets `produced` to its result as it returns.
    #[derive(Debug)]
    pub struct Destination {
        pub buffer: Value,
        pub produced: Mutex<Value>,
        pub clobber: bool,
    }
    impl Destination {
        pub fn new(buffer: Value, produced: Value, clobber: bool) -> Destination {
            Destination { buffer, produced: Mutex::new(produced), clobber }
        }
        pub fn produced(&self) -> Value { self.produced.lock().unwrap().clone() }
    }

    #[derive(Clone, Debug)]
    pub struct Registers {
        /// an instruction's slot
        pub defs: Vec<i64>,
        /// the instructions over slots; array handles keep their register
        pub run_code: Vec<Instruction>,
        pub run_outputs: Vec<Vec<i64>>,
        pub slots: i64,
        pub integer_slots: Vec<bool>,
        /// a text slot's place in the text bank, -1 for the others
        pub text_slots: Vec<i64>,
        /// a slot that is one register's alone
        pub dedicated: Vec<bool>,
    }

    pub type Sources = Vec<(ResidualId, Arc<Term>)>;

    /// Heavy fields are `Arc`s: `rebuild`/`with_captures` copy a region without copying its code.
    #[derive(Clone, Debug)]
    pub struct T {
        pub capture_roots: flow_eval::Roots,
        pub destination: Option<Arc<Destination>>,
        pub slot_plan: Arc<OnceLock<Registers>>,
        pub residual: ResidualId,
        pub term: Arc<Term>,
        pub sources: Sources,
        pub widths: Arc<Vec<i64>>,
        pub uniforms: Vec<Value>,
        pub uniform_names: Arc<Vec<(String, Vec<String>)>>,
        pub code: Arc<Vec<Instruction>>,
        pub dependent: Arc<OnceLock<Vec<bool>>>,
        /// component registers of each output; `Collect` regions only may have several
        pub outputs: Arc<Vec<Vec<i64>>>,
        pub count: Option<i64>,
        pub origin: Option<Origin>,
        pub iteration: Iteration,
        pub result: RegionResult,
        pub skip: Arc<Vec<i64>>,
        pub stages: Sources,
        pub fusion: bool,
        pub dynamic: bool,
        pub count_source: CountSource,
        pub body: Arc<Term>,
        pub binding_fn: Option<String>,
        pub sites: Arc<Vec<(i64, Path, Vec<i64>)>>,
    }

    /// The arguments of [`make`] (OCaml's labelled arguments), owned.
    #[derive(Clone, Debug)]
    pub struct Parts {
        pub residual: ResidualId,
        pub term: Arc<Term>,
        pub sources: Sources,
        pub widths: Vec<i64>,
        pub uniforms: Vec<Value>,
        pub uniform_names: Vec<(String, Vec<String>)>,
        pub code: Vec<Instruction>,
        pub outputs: Vec<Vec<i64>>,
        pub count: Option<i64>,
        pub origin: Option<Origin>,
        pub iteration: Iteration,
        pub result: RegionResult,
        pub skip: Vec<i64>,
        pub stages: Sources,
        pub fusion: bool,
        pub dynamic: bool,
        pub count_source: CountSource,
        pub body: Arc<Term>,
        pub binding_fn: Option<String>,
        pub sites: Vec<(i64, Path, Vec<i64>)>,
    }

    /// The optional arguments of [`rebuild`]; `None` keeps the region's own.
    #[derive(Clone, Debug, Default)]
    pub struct Rebuild {
        pub residual: Option<ResidualId>,
        pub term: Option<Arc<Term>>,
        pub sources: Option<Sources>,
        pub widths: Option<Vec<i64>>,
        pub uniforms: Option<Vec<Value>>,
        pub uniform_names: Option<Vec<(String, Vec<String>)>>,
        pub code: Option<Vec<Instruction>>,
        pub outputs: Option<Vec<Vec<i64>>>,
        pub count: Option<Option<i64>>,
        pub origin: Option<Option<Origin>>,
        pub iteration: Option<Iteration>,
        pub result: Option<RegionResult>,
        pub skip: Option<Vec<i64>>,
        pub stages: Option<Sources>,
        pub fusion: Option<bool>,
        pub dynamic: Option<bool>,
        pub count_source: Option<CountSource>,
        pub body: Option<Arc<Term>>,
        pub binding_fn: Option<Option<String>>,
        pub sites: Option<Vec<(i64, Path, Vec<i64>)>>,
    }

    pub fn with_destination(t: &T, destination: Arc<Destination>) -> T {
        T { destination: Some(destination), ..t.clone() }
    }

    pub fn floats_for(t: &T, length: usize) -> Vec<f64> {
        if let Some(d) = &t.destination {
            if let Value::Floats(1, data) = &d.buffer {
                if data.len() == length && t.outputs.len() == 1 { return (**data).clone(); }
            }
        }
        vec![0.0; length]
    }

    pub fn ints_for(t: &T, length: usize) -> Vec<i64> {
        if let Some(d) = &t.destination {
            if let Value::Int_array(data) = &d.buffer {
                if data.len() == length && t.outputs.len() == 1 { return (**data).clone(); }
            }
        }
        vec![0; length]
    }

    pub fn finished(t: &T, value: Value) {
        if let Some(d) = &t.destination { *d.produced.lock().unwrap() = value; }
    }

    pub fn clobbered(t: &T) -> bool { t.destination.as_ref().map(|d| d.clobber).unwrap_or(false) }

    pub fn map_registers(at: &dyn Fn(i64) -> i64, instruction: &Instruction) -> Instruction {
        use Instruction::*;
        match instruction {
            Gather(a, i, w, c) => Gather(*a, at(*i), *w, *c),
            Int_gather(a, i) => Int_gather(*a, at(*i)),
            Text_gather(a, i) => Text_gather(*a, at(*i)),
            Text_select(c, a, b) => Text_select(at(*c), at(*a), at(*b)),
            Binary(op, a, b) => Binary(*op, at(*a), at(*b)),
            Int_binary(op, a, b) => Int_binary(*op, at(*a), at(*b)),
            Int_compare(op, a, b) => Int_compare(*op, at(*a), at(*b)),
            Unary(op, a) => Unary(*op, at(*a)),
            Int_abs(a) => Int_abs(at(*a)),
            Convert(op, a) => Convert(op.clone(), at(*a)),
            Noise3(a, b, c, seed, octaves, lacunarity, gain) => {
                let li = |x: &LiveInt| match x { LiveInt::Fixed(v) => LiveInt::Fixed(*v), LiveInt::Live(r) => LiveInt::Live(at(*r)) };
                let lf = |x: &LiveFloat| match x { LiveFloat::Fixed(v) => LiveFloat::Fixed(*v), LiveFloat::Live(r) => LiveFloat::Live(at(*r)) };
                Noise3(at(*a), at(*b), at(*c), *seed, li(octaves), lf(lacunarity), lf(gain))
            }
            Rand_at(seed, index) => Rand_at(at(*seed), at(*index)),
            Fma(a, b, c) => Fma(at(*a), at(*b), at(*c)),
            Checked_mul(p, a, b) => Checked_mul(*p, at(*a), at(*b)),
            Trace(field, r) => Trace(*field, r.iter().map(|x| at(*x)).collect()),
            Stage(stage, r) => Stage(*stage, r.iter().map(|x| at(*x)).collect()),
            Checked_fma(p, a, b, c) => Checked_fma(*p, at(*a), at(*b), at(*c)),
            Select(a, b, c) => Select(at(*a), at(*b), at(*c)),
            Int_select(a, b, c) => Int_select(at(*a), at(*b), at(*c)),
            Guarded(guards, body) => Guarded(guards.iter().map(|(c, yes)| (at(*c), *yes)).collect(), Box::new(map_registers(at, body))),
            other => other.clone(),
        }
    }

    pub fn has_loop(instruction: &Instruction) -> bool {
        match instruction {
            Instruction::Loop_begin(_) | Instruction::Loop_carry(..) | Instruction::Loop_end(..) | Instruction::Loop_result(..) => true,
            Instruction::Guarded(_, body) => has_loop(body),
            _ => false,
        }
    }

    pub fn array_handle(instruction: &Instruction) -> bool {
        match instruction {
            Instruction::Array_prepare(..) => true,
            Instruction::Guarded(_, body) => array_handle(body),
            _ => false,
        }
    }

    fn classified(code: &[Instruction], defs: &[i64], slots: usize) -> (Vec<bool>, Vec<i64>) {
        let mut integer_slots = vec![false; slots];
        let mut text_slots = vec![-1_i64; slots];
        let mut next_text = 0_i64;
        for (i, instruction) in code.iter().enumerate() {
            if integer_register(instruction) { integer_slots[defs[i] as usize] = true; }
            if text_register(instruction) && text_slots[defs[i] as usize] < 0 {
                text_slots[defs[i] as usize] = next_text;
                next_text += 1;
            }
        }
        (integer_slots, text_slots)
    }

    /// Linear scan over a `Collect` region's registers: a register's slot goes back to its bank after
    /// its last read, and never to an operand of the instruction it defines. A guarded result keeps its
    /// own slot, and a guard condition too. Array handles have no slot. Regions with loops keep one
    /// slot per register.
    fn plan_registers(t: &T) -> Registers {
        let n = t.code.len();
        let identity = || {
            let defs: Vec<i64> = (0..n as i64).collect();
            let (integer_slots, text_slots) = classified(&t.code, &defs, n);
            Registers {
                defs,
                run_code: (*t.code).clone(),
                run_outputs: (*t.outputs).clone(),
                slots: n as i64,
                integer_slots,
                text_slots,
                dedicated: vec![true; n],
            }
        };
        if t.result != RegionResult::Collect || t.code.iter().any(has_loop) { return identity(); }
        let mut last: Vec<i64> = (0..n as i64).collect();
        let mut dedicated = vec![false; n];
        for (i, instruction) in t.code.iter().enumerate() {
            for r in operands(instruction) { last[r as usize] = last[r as usize].max(i as i64); }
            if let Instruction::Guarded(guards, _) = instruction {
                dedicated[i] = true;
                for (c, _) in guards { dedicated[*c as usize] = true; }
            }
        }
        for output in t.outputs.iter() { for r in output { last[*r as usize] = n as i64; } }
        let mut defs = vec![0_i64; n];
        let mut free: [Vec<i64>; 3] = [Vec::new(), Vec::new(), Vec::new()];
        let mut next = 0_i64;
        let mut own: Vec<i64> = Vec::new();
        let bank = |i: usize| if text_register(&t.code[i]) { 2 } else if integer_register(&t.code[i]) { 1 } else { 0 };
        for i in 0..n {
            if !array_handle(&t.code[i]) {
                let b = bank(i);
                if !dedicated[i] && !free[b].is_empty() {
                    defs[i] = free[b].pop().unwrap();
                } else {
                    let slot = next;
                    next += 1;
                    if dedicated[i] { own.push(slot); }
                    defs[i] = slot;
                }
            }
            let mut release = |r: usize, last: &mut Vec<i64>| {
                if last[r] == i as i64 && !dedicated[r] && !array_handle(&t.code[r]) {
                    last[r] = -1;
                    free[bank(r)].push(defs[r]);
                }
            };
            for r in operands(&t.code[i]) { release(r as usize, &mut last); }
            release(i, &mut last);
        }
        let slots = next.max(1) as usize;
        let (integer_slots, text_slots) = classified(&t.code, &defs, slots);
        let mut dedicated_slots = vec![false; slots];
        for slot in own { dedicated_slots[slot as usize] = true; }
        let at = |r: i64| defs[r as usize];
        Registers {
            run_code: t.code.iter().map(|i| map_registers(&at, i)).collect(),
            run_outputs: t.outputs.iter().map(|o| o.iter().map(|r| defs[*r as usize]).collect()).collect(),
            defs,
            slots: slots as i64,
            integer_slots,
            text_slots,
            dedicated: dedicated_slots,
        }
    }

    /// A CPU run's linear-scan register slots, cached per region.
    pub fn registers(t: &T) -> &Registers {
        t.slot_plan.get_or_init(|| plan_registers(t))
    }

    fn dependent_of(code: &[Instruction]) -> Vec<bool> {
        fn accumulator(i: &Instruction) -> bool {
            match i {
                Instruction::Accumulator(_) | Instruction::Int_accumulator => true,
                Instruction::Guarded(_, body) => accumulator(body),
                _ => false,
            }
        }
        fn finish(i: &Instruction, id: usize, dependent: &mut [bool]) {
            match i {
                Instruction::Loop_end(loop_, _, _) => {
                    let loop_ = *loop_ as usize;
                    let mut carried = dependent[loop_];
                    for slot in loop_ + 1..=id { carried = carried || dependent[slot]; }
                    for d in &mut dependent[loop_..=id] { *d = carried; }
                }
                Instruction::Guarded(_, body) => finish(body, id, dependent),
                _ => {}
            }
        }
        let mut dependent = vec![false; code.len()];
        for (id, instruction) in code.iter().enumerate() {
            dependent[id] = accumulator(instruction) || operands(instruction).iter().any(|r| dependent[*r as usize]);
            finish(instruction, id, &mut dependent);
        }
        dependent
    }

    pub fn dependent_flags(r: &T) -> &Vec<bool> { r.dependent.get_or_init(|| dependent_of(&r.code)) }



    fn controls_valid(code: &[Instruction], outputs: &[Vec<i64>]) -> bool {
        let n = code.len();
        let mut scopes: Vec<Vec<i64>> = vec![Vec::new(); n];
        let mut closed = vec![false; n];
        let mut active: Vec<i64> = Vec::new();
        let mut valid = true;
        let inside = |outer: &[i64], inner: &[i64]| outer.iter().all(|s| inner.contains(s));
        let header = |id: i64| -> Option<&Loop> {
            if id < 0 || id >= n as i64 { None } else if let Instruction::Loop_begin(l) = Instruction::unguarded(&code[id as usize]) { Some(l) } else { None }
        };
        let scalar = |id: i64, ty: &Ty| -> bool {
            if id < 0 || id >= n as i64 { return false; }
            let i = &code[id as usize];
            match ty {
                Ty::Int => integer_register(i),
                Ty::Text => text_register(i),
                Ty::Float | Ty::Bool => !(integer_register(i) || text_register(i)),
                _ => false,
            }
        };
        for slot in 0..n {
            let instruction = &code[slot];
            scopes[slot] = active.clone();
            let reads = operands(instruction);
            if reads.iter().any(|id| *id < 0 || *id >= slot as i64) {
                valid = false;
            } else if !matches!(Instruction::unguarded(instruction), Instruction::Loop_result(..))
                && reads.iter().any(|id| !inside(&scopes[*id as usize], &active))
            {
                valid = false;
            }
            let slot_i = slot as i64;
            match Instruction::unguarded(instruction) {
                Instruction::Loop_begin(loop_) => {
                    if loop_.finish <= slot_i + loop_.initial.len() as i64
                        || loop_.finish >= n as i64
                        || loop_.initial.is_empty()
                        || loop_.first < 0 || loop_.first >= slot_i
                        || loop_.last < 0 || loop_.last >= slot_i
                        || !(integer_register(&code[loop_.first as usize]) && integer_register(&code[loop_.last as usize]))
                        || loop_.limit.map(|l| 0 > l).unwrap_or(false)
                    {
                        valid = false;
                    } else {
                        match Instruction::unguarded(&code[loop_.finish as usize]) {
                            Instruction::Loop_end(owner, values, _) if *owner == slot_i && values.len() == loop_.initial.len() => {}
                            _ => valid = false,
                        }
                        for (component, initial) in loop_.initial.iter().enumerate() {
                            match Instruction::unguarded(&code[slot + 1 + component]) {
                                Instruction::Loop_carry(owner, index, ty) if *owner == slot_i && *index == component as i64 && scalar(*initial, ty) => {}
                                _ => valid = false,
                            }
                        }
                    }
                    active.push(slot_i);
                    scopes[slot] = active.clone();
                }
                Instruction::Loop_carry(loop_, component, ty) => {
                    let ok = match (active.last(), header(*loop_)) {
                        (Some(owner), Some(control)) => {
                            owner == loop_ && *component >= 0
                                && *component < control.initial.len() as i64
                                && slot_i == loop_ + 1 + component
                                && scalar(control.initial[*component as usize], ty)
                                && Instruction::guarded(instruction).0 == Instruction::guarded(&code[*loop_ as usize]).0
                        }
                        _ => false,
                    };
                    if !ok { valid = false; }
                }
                Instruction::Loop_end(loop_, values, stop) => {
                    let ok = match (active.last(), header(*loop_)) {
                        (Some(owner), Some(control)) => {
                            owner == loop_ && control.finish == slot_i
                                && control.finish > loop_ + control.initial.len() as i64
                                && control.finish < n as i64
                                && values.len() == control.initial.len()
                                && Instruction::guarded(instruction).0 == Instruction::guarded(&code[*loop_ as usize]).0
                        }
                        _ => false,
                    };
                    if ok {
                        for (component, value) in values.iter().enumerate() {
                            match Instruction::unguarded(&code[(*loop_ as usize) + 1 + component]) {
                                Instruction::Loop_carry(_, _, ty) if scalar(*value, ty) => {}
                                _ => valid = false,
                            }
                        }
                        if stop.map(|v| !scalar(v, &Ty::Bool)).unwrap_or(false) { valid = false; }
                        closed[*loop_ as usize] = true;
                        active.pop();
                    } else {
                        valid = false;
                    }
                }
                Instruction::Loop_result(loop_, component, ty) => {
                    let ok = match header(*loop_) {
                        Some(control) => {
                            *loop_ < slot_i && closed[*loop_ as usize] && *component >= 0
                                && loop_ + 1 + component < n as i64
                                && *component < control.initial.len() as i64
                                && inside(&scopes[*loop_ as usize][..scopes[*loop_ as usize].len().saturating_sub(1)], &active)
                                && Instruction::guarded(instruction).0 == Instruction::guarded(&code[*loop_ as usize]).0
                                && matches!(Instruction::unguarded(&code[(loop_ + 1 + component) as usize]), Instruction::Loop_carry(_, _, stored) if stored == ty)
                        }
                        None => false,
                    };
                    if !ok { valid = false; }
                }
                _ => {}
            }
        }
        valid && active.is_empty()
            && outputs.iter().all(|o| o.iter().all(|id| {
                *id >= 0 && *id < n as i64 && scopes[*id as usize].is_empty()
                    && !matches!(Instruction::unguarded(&code[*id as usize]), Instruction::Loop_begin(_) | Instruction::Loop_carry(..) | Instruction::Loop_end(..))
            }))
    }

    /// The type of an uniform value; a residual's own term type (expired residual: `Any`).
    fn uniform_ty(value: &Value) -> Ty {
        match value {
            Value::Residual(id) => flow_eval::residual::residual_ty(*id).unwrap_or(Ty::Any),
            other => value::ty_of(other),
        }
    }

    fn invalid(term: &Term, message: &str) -> Vec<Diagnostic> {
        vec![rays_diagnostic::error("E_PACKED_LAYOUT".to_owned(), message.to_owned(), Some(term.form().span.clone()), None, None, Vec::new(), Vec::new())]
    }

    fn validate(r: T) -> Result<T, Vec<Diagnostic>> {
        let ok = |slot: i64, id: i64| id >= 0 && id < slot;
        let mut operands_ok = true;
        let mut controls = false;
        let ncode = r.code.len() as i64;
        let array_handle_id = |id: i64| id >= 0 && id < ncode && matches!(Instruction::unguarded(&r.code[id as usize]), Instruction::Array_prepare(..));
        let table_type = |id: i64| match Instruction::unguarded(&r.code[id as usize]) { Instruction::Array_prepare(_, ty) => ty.clone(), _ => Ty::Any };
        let text = |id: i64| id >= 0 && id < ncode && text_register(&r.code[id as usize]);
        let prepared_for = |selected: &[(i64, bool)], array: i64| Instruction::guarded(&r.code[array as usize]).0.iter().all(|g| selected.contains(g));
        fn check(
            r: &T, slot: usize, instruction: &Instruction, selected: &[(i64, bool)], operands_ok: &mut bool, controls: &mut bool,
            array_handle_id: &dyn Fn(i64) -> bool, table_type: &dyn Fn(i64) -> Ty, text: &dyn Fn(i64) -> bool,
            prepared_for: &dyn Fn(&[(i64, bool)], i64) -> bool,
        ) {
            let slot_i = slot as i64;
            let ok = |id: i64| id >= 0 && id < slot_i;
            let ops = operands(instruction);
            if !ops.iter().all(|id| ok(*id)) { *operands_ok = false; }
            match instruction {
                Instruction::Gather(..) | Instruction::Int_gather(..) | Instruction::Text_gather(..) | Instruction::Guarded(..) => {}
                // a store names its buffer by its table
                Instruction::Stage(Stage::Store, registers) => {
                    if registers[1..].iter().any(|x| array_handle_id(*x))
                        || !(array_handle_id(registers[0]) && prepared_for(selected, registers[0]))
                    {
                        *operands_ok = false;
                    }
                }
                _ => { if ops.iter().any(|x| array_handle_id(*x)) { *operands_ok = false; } }
            }
            match instruction {
                Instruction::Text_select(..) | Instruction::Guarded(..) | Instruction::Loop_begin(_) | Instruction::Loop_end(..) | Instruction::Loop_result(..) => {}
                _ => { if ops.iter().any(|x| text(*x)) { *operands_ok = false; } }
            }
            let nuniforms = r.uniforms.len() as i64;
            match instruction {
                Instruction::Array_prepare(input, ty) => {
                    if *input < 0 || *input >= nuniforms
                        || !matches!(ty, Ty::Array(e) if matches!(**e, Ty::Int | Ty::Text | Ty::Float | Ty::Vec2 | Ty::Vec3 | Ty::Vec4))
                    {
                        *operands_ok = false;
                    } else if uniform_ty(&r.uniforms[*input as usize]) != *ty {
                        *operands_ok = false;
                    }
                }
                Instruction::Gather(array, index, width, component) => {
                    if !(ok(*array) && ok(*index) && array_handle_id(*array))
                        || !integer_register(&r.code[*index as usize])
                        || *component < 0 || *component >= *width
                    {
                        *operands_ok = false;
                    } else if !prepared_for(selected, *array)
                        || table_type(*array) == Ty::Array(Box::new(Ty::Int))
                        || packed_ops::array_width(&table_type(*array)) != Some(*width as usize)
                    {
                        *operands_ok = false;
                    }
                }
                Instruction::Int_gather(array, index) => {
                    if !(ok(*array) && ok(*index) && array_handle_id(*array)) || !integer_register(&r.code[*index as usize]) {
                        *operands_ok = false;
                    } else if !prepared_for(selected, *array) || table_type(*array) != Ty::Array(Box::new(Ty::Int)) {
                        *operands_ok = false;
                    }
                }
                Instruction::Text_gather(array, index) => {
                    if !(ok(*array) && ok(*index) && array_handle_id(*array)) || !integer_register(&r.code[*index as usize]) {
                        *operands_ok = false;
                    } else if !prepared_for(selected, *array) || table_type(*array) != Ty::Array(Box::new(Ty::Text)) {
                        *operands_ok = false;
                    }
                }
                Instruction::Text_select(condition, yes, no) => {
                    if array_handle_id(*condition) || text(*condition) || !(text(*yes) && text(*no)) { *operands_ok = false; }
                }
                Instruction::Text_input(input) => {
                    if *input < 0 || *input >= r.sources.len() as i64 || *input >= r.widths.len() as i64
                        || r.widths[*input as usize] != 1
                        || *r.sources[*input as usize].1.ty() != Ty::Array(Box::new(Ty::Text))
                    {
                        *operands_ok = false;
                    }
                }
                Instruction::Text_uniform(input) => {
                    if *input < 0 || *input >= nuniforms { *operands_ok = false; }
                    else if uniform_ty(&r.uniforms[*input as usize]) != Ty::Text { *operands_ok = false; }
                }
                Instruction::Input(input, width, component) => {
                    if *input < 0 || *input >= r.widths.len() as i64
                        || *width != r.widths[*input as usize]
                        || *component < 0 || *component >= *width
                        || (*input < r.sources.len() as i64 && *r.sources[*input as usize].1.ty() == Ty::Array(Box::new(Ty::Text)))
                    {
                        *operands_ok = false;
                    }
                }
                Instruction::Int_input(input) => {
                    if *input < 0 || *input >= r.sources.len() as i64 || *input >= r.widths.len() as i64
                        || r.widths[*input as usize] != 1
                        || *r.sources[*input as usize].1.ty() != Ty::Array(Box::new(Ty::Int))
                    {
                        *operands_ok = false;
                    }
                }
                Instruction::Uniform(input, component) => {
                    if *input < 0 || *input >= nuniforms || *component < 0 || *component >= 4 { *operands_ok = false; }
                    else if uniform_ty(&r.uniforms[*input as usize]) == Ty::Text { *operands_ok = false; }
                }
                Instruction::Int_uniform(input) => {
                    if *input < 0 || *input >= nuniforms { *operands_ok = false; }
                    else if uniform_ty(&r.uniforms[*input as usize]) != Ty::Int { *operands_ok = false; }
                }
                Instruction::Accumulator(component) => { if *component < 0 || *component >= 4 { *operands_ok = false; } }
                Instruction::Loop_begin(_) | Instruction::Loop_carry(..) | Instruction::Loop_end(..) | Instruction::Loop_result(..) => *controls = true,
                Instruction::Guarded(guards, body) => {
                    if guards.iter().any(|(id, _)| array_handle_id(*id) || text(*id)) { *operands_ok = false; }
                    let mut selected2 = selected.to_vec();
                    selected2.extend(guards.iter().copied());
                    check(r, slot, body, &selected2, operands_ok, controls, array_handle_id, table_type, text, prepared_for);
                }
                _ => {}
            }
        }
        for (slot, instruction) in r.code.iter().enumerate() {
            check(&r, slot, instruction, &[], &mut operands_ok, &mut controls, &array_handle_id, &table_type, &text, &prepared_for);
        }
        let all_ok = |output: &Vec<i64>| {
            !output.is_empty() && output.len() <= 4
                && output.iter().all(|id| ok(ncode, *id) && !array_handle_id(*id))
                && (!output.iter().any(|id| text(*id)) || output.len() == 1)
        };
        if r.widths.len() != r.sources.len() || !r.widths.iter().all(|w| *w >= 1 && *w <= 4) {
            Err(invalid(&r.term, "Region inputs need matching sources and widths from 1 to 4."))
        } else if !operands_ok || (controls && !controls_valid(&r.code, &r.outputs)) {
            Err(invalid(&r.term, "Region instruction reads an invalid input or a register defined later."))
        } else if r.code.len() > packed_ops::REGISTER_LIMIT {
            Err(invalid(&r.term, &format!("Region exceeds {} registers.", packed_ops::REGISTER_LIMIT)))
        } else if r.outputs.is_empty() || (r.outputs.len() > 1 && r.result != RegionResult::Collect) {
            Err(invalid(&r.term, "A region has one output, or several only when it collects."))
        } else if !r.outputs.iter().all(all_ok) {
            Err(invalid(&r.term, "Region output needs 1 to 4 existing scalar registers."))
        } else if r.result != RegionResult::Collect && r.code.iter().any(text_register) {
            Err(invalid(&r.term, "Text leaves require collecting kernels."))
        } else {
            Ok(r)
        }
    }

    fn capture_roots(t: &T) -> flow_eval::Roots {
        flow_eval::roots(std::iter::once(Value::Residual(t.residual)).chain(t.uniforms.iter().cloned())
            .chain(t.sources.iter().chain(&t.stages).map(|(r, _)| Value::Residual(*r))).collect::<Vec<_>>().iter())
    }

    pub fn make(p: Parts) -> Result<T, Vec<Diagnostic>> {
        let mut t = validate(T {
            capture_roots: Default::default(),
            destination: None,
            slot_plan: Arc::new(OnceLock::new()),
            residual: p.residual,
            term: p.term,
            sources: p.sources,
            widths: Arc::new(p.widths),
            uniforms: p.uniforms,
            uniform_names: Arc::new(p.uniform_names),
            code: Arc::new(p.code),
            dependent: Arc::new(OnceLock::new()),
            outputs: Arc::new(p.outputs),
            count: p.count,
            origin: p.origin,
            iteration: p.iteration,
            result: p.result,
            skip: Arc::new(p.skip),
            stages: p.stages,
            fusion: p.fusion,
            dynamic: p.dynamic,
            count_source: p.count_source,
            body: p.body,
            binding_fn: p.binding_fn,
            sites: Arc::new(p.sites),
        })?;
        t.capture_roots = capture_roots(&t);
        Ok(t)
    }

    /// `rebuild t ?... ()`: re-validates, with fresh caches and no destination (as `make`).
    pub fn rebuild(t: &T, o: Rebuild) -> Result<T, Vec<Diagnostic>> {
        let mut t = validate(T {
            capture_roots: t.capture_roots.clone(),
            destination: None,
            slot_plan: Arc::new(OnceLock::new()),
            residual: o.residual.unwrap_or(t.residual),
            term: o.term.unwrap_or_else(|| t.term.clone()),
            sources: o.sources.unwrap_or_else(|| t.sources.clone()),
            widths: o.widths.map(Arc::new).unwrap_or_else(|| t.widths.clone()),
            uniforms: o.uniforms.unwrap_or_else(|| t.uniforms.clone()),
            uniform_names: o.uniform_names.map(Arc::new).unwrap_or_else(|| t.uniform_names.clone()),
            code: o.code.map(Arc::new).unwrap_or_else(|| t.code.clone()),
            dependent: Arc::new(OnceLock::new()),
            outputs: o.outputs.map(Arc::new).unwrap_or_else(|| t.outputs.clone()),
            count: o.count.unwrap_or(t.count),
            origin: o.origin.unwrap_or_else(|| t.origin.clone()),
            iteration: o.iteration.unwrap_or(t.iteration),
            result: o.result.unwrap_or_else(|| t.result.clone()),
            skip: o.skip.map(Arc::new).unwrap_or_else(|| t.skip.clone()),
            stages: o.stages.unwrap_or_else(|| t.stages.clone()),
            fusion: o.fusion.unwrap_or(t.fusion),
            dynamic: o.dynamic.unwrap_or(t.dynamic),
            count_source: o.count_source.unwrap_or_else(|| t.count_source.clone()),
            body: o.body.unwrap_or_else(|| t.body.clone()),
            binding_fn: o.binding_fn.unwrap_or_else(|| t.binding_fn.clone()),
            sites: o.sites.map(Arc::new).unwrap_or_else(|| t.sites.clone()),
        })?;
        t.capture_roots = capture_roots(&t);
        Ok(t)
    }

    pub mod private {
        use super::*;
        /// Rebinds a region's captures; the capture layout (types, counts) must not change.
        pub fn with_captures(
            t: &T, residual: ResidualId, uniforms: Vec<Value>, sources: Sources, stages: Sources, count: Option<i64>,
        ) -> Result<T, Diagnostic> {
            let diagnostic = rays_diagnostic::error(
                "E_CORE_ARGUMENT".to_owned(),
                "A rebound region changed its validated capture layout.".to_owned(),
                Some(t.term.form().span.clone()), None, None, Vec::new(), Vec::new(),
            );
            if uniforms.len() != t.uniforms.len()
                || !t.uniforms.iter().zip(uniforms.iter()).all(|(b, a)| uniform_ty(b) == uniform_ty(a))
                || sources.len() != t.sources.len()
                || !t.sources.iter().zip(sources.iter()).all(|(b, a)| b.1.ty() == a.1.ty())
                || stages.len() != t.stages.len()
            {
                return Err(diagnostic);
            }
            let mut bound = T { residual, uniforms, sources, stages, count, ..t.clone() };
            bound.capture_roots = capture_roots(&bound);
            Ok(bound)
        }
    }
}

/// OCaml `type region = Region.t`.
pub type RegionT = region::T;

#[derive(Clone, Debug)]
pub struct Program {
    pub constants: Vec<Literal>,
    pub locals: Vec<Ty>,
    pub body: Vec<Statement>,
    pub results: Vec<i64>,
    pub state_slots: Vec<StateSlot>,
    pub regions: Vec<Arc<RegionT>>,
    pub call_slots: i64,
}

/// Local operands read directly by one statement, in operand order.
pub fn reads(s: &Statement) -> Vec<i64> {
    fn pairs(args: &[(String, i64)]) -> Vec<i64> { args.iter().map(|(_, n)| *n).collect() }
    fn expr(e: &Expr) -> Vec<i64> {
        match e {
            Expr::Constant(_) | Expr::Call_index(_) | Expr::Frame(_) | Expr::Source(_) => Vec::new(),
            Expr::Local(n) | Expr::Field(n, _) => vec![*n],
            Expr::Prim(_, args) | Expr::Host_source { args, .. } | Expr::Source_descriptor { args, .. } => pairs(args),
            Expr::Vec(ns) | Expr::Items(ns) => ns.clone(),
            Expr::Fields(xs) => pairs(xs),
        }
    }
    fn draw(d: &Draw) -> Vec<i64> {
        match d {
            Draw::Background(n) | Draw::Push(_, n) => vec![*n],
            Draw::Point { at, ink } => vec![*at, *ink],
            Draw::Mark { args, .. } | Draw::Batch { args, .. } | Draw::Path { args, .. } => pairs(args),
            Draw::Points { positions, ink } => vec![*positions, *ink],
            Draw::Text_run { at, text, ink, size } => vec![*at, *text, *ink, *size],
            Draw::Image { at, scale, angle, .. } => vec![*at, *scale, *angle],
            Draw::Pop => Vec::new(),
        }
    }
    match &s.node {
        StatementNode::Draw(d) => draw(d),
        StatementNode::Assign(_, e) => expr(e),
        StatementNode::Reset_call(_) | StatementNode::State_peek { .. } => Vec::new(),
        StatementNode::Protect { value, mapped, .. } => vec![*value, *mapped],
        StatementNode::Diagnose_rows { rows, diagnostic, .. } => vec![*rows, *diagnostic],
        StatementNode::If { condition, .. } => vec![*condition],
        StatementNode::Iterate { sequence, .. } => vec![*sequence],
        StatementNode::Counted { first, last, stop, .. } => {
            let mut r = vec![*first, *last];
            r.extend(stop.iter().copied());
            r
        }
        StatementNode::Store { index, value, .. } => vec![*index, *value],
        StatementNode::State_read { initial, .. } => vec![*initial],
        StatementNode::State_write { value, .. } => vec![*value],
        StatementNode::Invoke { args, .. } => args.clone(),
    }
}

/// Local slots written directly by one statement. Nested statements are not included.
pub fn writes(s: &Statement) -> Vec<i64> {
    match &s.node {
        StatementNode::Assign(n, _) => vec![*n],
        StatementNode::Reset_call(_) | StatementNode::Draw(_) | StatementNode::State_write { .. } => Vec::new(),
        StatementNode::Protect { failure, result, .. } => vec![*failure, *result],
        StatementNode::Diagnose_rows { index, result, .. } => vec![*index, *result],
        StatementNode::If { .. } => Vec::new(),
        StatementNode::Iterate { index, item, .. } => vec![*index, *item],
        StatementNode::Counted { count, index, item, .. } => vec![*count, *index, *item],
        StatementNode::Store { sequence, .. } => vec![*sequence],
        StatementNode::State_read { result, .. } => vec![*result],
        StatementNode::State_peek { found, value, .. } => vec![*found, *value],
        StatementNode::Invoke { result, .. } => vec![*result],
    }
}

/// Immediate nested statements in execution/source order: body before handler, yes before no, and
/// loop initialization before its body.
pub fn children(s: &Statement) -> Vec<&Statement> {
    match &s.node {
        StatementNode::Protect { body, handler, .. } => body.iter().chain(handler.iter()).collect(),
        StatementNode::Diagnose_rows { handler, .. } => handler.iter().collect(),
        StatementNode::If { yes, no, .. } => yes.iter().chain(no.iter()).collect(),
        StatementNode::Iterate { body, .. } => body.iter().collect(),
        StatementNode::Counted { init, body, .. } => init.iter().chain(body.iter()).collect(),
        StatementNode::State_read { init, .. } => init.iter().collect(),
        _ => Vec::new(),
    }
}

/// Visit statements in preorder, recursively following [`children`].
pub fn iter(f: &mut dyn FnMut(&Statement), statements: &[Statement]) {
    for s in statements {
        f(s);
        for child in children(s) { iter(f, std::slice::from_ref(child)); }
    }
}


fn frame_name(f: FrameField) -> &'static str {
    match f {
        FrameField::Time => "t", FrameField::Dt => "dt", FrameField::Index => "frame", FrameField::Tick => "tick",
        FrameField::Width => "width", FrameField::Height => "height", FrameField::Pointer_x => "pointer/x",
        FrameField::Pointer_y => "pointer/y", FrameField::Input => "input",
    }
}

/// `Some(literal)` when the value is plain data (no residuals, sources, closures; lists up to 4096).
pub fn literal_of_value(value: &Value) -> Option<Literal> {
    fn convert(value: &Value) -> Option<Literal> {
        Some(match value {
            Value::Int(n) => Literal::Int(*n),
            Value::Float(x) => Literal::Float(*x),
            Value::Bool(b) => Literal::Bool(*b),
            Value::Text(s) => Literal::Text(s.clone()),
            Value::Vec2(x, y) => Literal::Vector(vec![*x, *y]),
            Value::Vec3(x, y, z) => Literal::Vector(vec![*x, *y, *z]),
            Value::Vec4(x, y, z, w) => Literal::Vector(vec![*x, *y, *z, *w]),
            Value::List(xs) if xs.len() <= 4096 => Literal::List(xs.iter().map(convert).collect::<Option<Vec<_>>>()?),
            Value::Record(xs) => Literal::Record(xs.iter().map(|(n, v)| convert(v).map(|l| (n.clone(), l))).collect::<Option<Vec<_>>>()?),
            Value::Int_array(data) => Literal::Int_array(data.clone()),
            Value::Text_array(data) => Literal::Text_array(data.clone()),
            Value::Floats(width @ 1..=4, data) => Literal::Array { width: *width as i64, data: data.clone() },
            Value::No_geo => Literal::Empty_geometry,
            _ => return None,
        })
    }
    convert(value)
}

/// Panics (OCaml `invalid_arg`) on a vector or array of an unsupported width.
pub fn value_of_literal(literal: &Literal) -> Value {
    match literal {
        Literal::Int(n) => Value::Int(*n),
        Literal::Float(x) => Value::Float(*x),
        Literal::Bool(b) => Value::Bool(*b),
        Literal::Text(s) => Value::Text(s.clone()),
        Literal::Vector(v) => match v.as_slice() {
            [x, y] => Value::Vec2(*x, *y),
            [x, y, z] => Value::Vec3(*x, *y, *z),
            [x, y, z, w] => Value::Vec4(*x, *y, *z, *w),
            _ => panic!("Core.value_of_literal: width"),
        },
        Literal::List(xs) => Value::List(Arc::new(xs.iter().map(value_of_literal).collect())),
        Literal::Record(xs) => Value::Record(xs.iter().map(|(n, v)| (n.clone(), value_of_literal(v))).collect()),
        Literal::Int_array(data) => Value::Int_array(data.clone()),
        Literal::Text_array(data) => Value::Text_array(data.clone()),
        Literal::Array { width: width @ 1..=4, data } => Value::Floats(*width as usize, data.clone()),
        Literal::Empty_geometry => Value::No_geo,
        Literal::Array { .. } => panic!("Core.value_of_literal: width"),
    }
}

/// OCaml `%S`: a double-quoted `String.escaped`.
pub fn quoted(bytes: &[u8]) -> String {
    let mut out = String::from("\"");
    for &b in bytes {
        match b {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            b'\n' => out.push_str("\\n"),
            b'\t' => out.push_str("\\t"),
            b'\r' => out.push_str("\\r"),
            8 => out.push_str("\\b"),
            32..=126 => out.push(b as char),
            _ => out.push_str(&format!("\\{:03}", b)),
        }
    }
    out.push('"');
    out
}

/// OCaml `%h`.
pub fn hex_float(x: f64) -> String {
    if x.is_nan() { return "nan".to_owned(); }
    if x.is_infinite() { return if x > 0.0 { "infinity" } else { "-infinity" }.to_owned(); }
    let bits = x.to_bits();
    let sign = if bits >> 63 == 1 { "-" } else { "" };
    let exponent = ((bits >> 52) & 0x7ff) as i64;
    let mantissa = bits & ((1u64 << 52) - 1);
    let (lead, exp) = if exponent == 0 { (0, if mantissa == 0 { 0 } else { -1022 }) } else { (1, exponent - 1023) };
    let mut digits = format!("{:013x}", mantissa);
    while digits.ends_with('0') { digits.pop(); }
    let frac = if digits.is_empty() { String::new() } else { format!(".{}", digits) };
    format!("{}0x{}{}p{}{}", sign, lead, frac, if exp < 0 { "-" } else { "+" }, exp.abs())
}

fn lisp_float(x: f64) -> String { flow_syntax::lisp::float(x) }

/// Deterministic diagnostic printer, including every statement's source identity.
pub fn pp(p: &Program) -> String {
    let mut o = String::new();
    fn local(o: &mut String, n: i64) { o.push_str(&format!("%{}", n)); }
    fn separated(o: &mut String, n: usize, render: &dyn Fn(&mut String, usize)) {
        for i in 0..n {
            if i > 0 { o.push(' '); }
            render(o, i);
        }
    }
    fn named_locals(o: &mut String, xs: &[(String, i64)]) {
        separated(o, xs.len(), &|o, i| { o.push_str(&quoted(xs[i].0.as_bytes())); o.push(':'); local(o, xs[i].1); });
    }
    fn literal(o: &mut String, l: &Literal) {
        match l {
            Literal::Int(n) => o.push_str(&n.to_string()),
            Literal::Float(f) => o.push_str(&lisp_float(*f)),
            Literal::Bool(b) => o.push_str(if *b { "true" } else { "false" }),
            Literal::Text(s) => o.push_str(&quoted(s)),
            Literal::Vector(xs) => { o.push_str("vec["); separated(o, xs.len(), &|o, i| o.push_str(&lisp_float(xs[i]))); o.push(']'); }
            Literal::List(xs) => { o.push_str("list["); separated(o, xs.len(), &|o, i| literal(o, &xs[i])); o.push(']'); }
            Literal::Record(xs) => {
                o.push_str("record{");
                separated(o, xs.len(), &|o, i| { o.push_str(&quoted(xs[i].0.as_bytes())); o.push(':'); literal(o, &xs[i].1); });
                o.push('}');
            }
            Literal::Int_array(data) => { o.push_str("array:int["); separated(o, data.len(), &|o, i| o.push_str(&data[i].to_string())); o.push(']'); }
            Literal::Text_array(data) => { o.push_str("array:text["); separated(o, data.len(), &|o, i| o.push_str(&quoted(&data[i]))); o.push(']'); }
            Literal::Array { width, data } => {
                o.push_str(&format!("array{}[", width));
                separated(o, data.len(), &|o, i| o.push_str(&lisp_float(data[i])));
                o.push(']');
            }
            Literal::Empty_geometry => o.push_str("nil"),
        }
    }
    fn expression(o: &mut String, e: &Expr) {
        match e {
            Expr::Constant(n) => o.push_str(&format!("${}", n)),
            Expr::Local(n) => local(o, *n),
            Expr::Frame(f) => o.push_str(&format!("frame.{}", frame_name(*f))),
            Expr::Call_index(n) => o.push_str(&format!("call[{}]++", n)),
            Expr::Source(value) => o.push_str(&format!("source<{}>", ty::to_string(&value::ty_of(value)))),
            Expr::Host_source { name, ty, args } => {
                o.push_str(&format!("host_source<{} {}>(", ty::to_string(ty), quoted(name.as_bytes())));
                named_locals(o, args);
                o.push(')');
            }
            Expr::Source_descriptor { name, ty, args } => {
                o.push_str(&format!("source_descriptor<{} {}>(", ty::to_string(ty), quoted(name.as_bytes())));
                named_locals(o, args);
                o.push(')');
            }
            Expr::Prim(op, args) => {
                o.push_str(primitive_name(op));
                match op {
                    Primitive::Coerce(ty) => o.push_str(&format!("<{}>", ty::to_string(ty))),
                    Primitive::Color_byte(ByteMode::Clamped) => o.push_str("<clamped>"),
                    Primitive::Color_byte(ByteMode::Unclamped) => o.push_str("<unclamped>"),
                    Primitive::Fail(code, message) => o.push_str(&format!("<{} {}>", quoted(code.as_bytes()), quoted(message.as_bytes()))),
                    Primitive::Fail_diagnostic(d) => o.push_str(&format!(
                        "<{} {} cause={}>", quoted(d.code.as_bytes()), quoted(d.message.as_bytes()),
                        quoted(d.cause.as_deref().unwrap_or_default()))),
                    _ => {}
                }
                o.push('(');
                named_locals(o, args);
                o.push(')');
            }
            Expr::Vec(ns) => { o.push_str("vec["); separated(o, ns.len(), &|o, i| local(o, ns[i])); o.push(']'); }
            Expr::Fields(xs) => { o.push_str("record{"); named_locals(o, xs); o.push('}'); }
            Expr::Items(ns) => { o.push_str("list["); separated(o, ns.len(), &|o, i| local(o, ns[i])); o.push(']'); }
            Expr::Field(n, name) => { local(o, *n); o.push_str(&format!(".{}", quoted(name.as_bytes()))); }
        }
    }
    fn args_line(o: &mut String, args: &[(String, i64)]) {
        for (name, n) in args { o.push_str(&format!(" {}=", name)); local(o, *n); }
    }
    fn draw(o: &mut String, d: &Draw) {
        match d {
            Draw::Background(c) => { o.push_str("draw/background "); local(o, *c); }
            Draw::Point { at, ink } => { o.push_str("draw/point at="); local(o, *at); o.push_str(" ink="); local(o, *ink); }
            Draw::Mark { mark, args } | Draw::Batch { mark, args } => {
                let plural = if matches!(d, Draw::Batch { .. }) { "s" } else { "" };
                o.push_str(&format!("draw/{}{}", match mark { Mark::Rect => "rect", Mark::Circle => "circle", Mark::Line => "line" }, plural));
                args_line(o, args);
            }
            Draw::Points { positions, ink } => { o.push_str("draw/points positions="); local(o, *positions); o.push_str(" ink="); local(o, *ink); }
            Draw::Path { path, args } => {
                o.push_str(&format!("draw/{}", match path { PathKind::Rounded_rect => "rounded_rect", PathKind::Polygon => "polygon", PathKind::Polyline => "polyline" }));
                args_line(o, args);
            }
            Draw::Text_run { at, text, ink, size } => {
                o.push_str("draw/text at="); local(o, *at); o.push_str(" text="); local(o, *text);
                o.push_str(" ink="); local(o, *ink); o.push_str(" size="); local(o, *size);
            }
            Draw::Image { source: (ty, id), at, scale, angle } => {
                o.push_str(&format!("draw/image {}#{} at=", ty::to_string(ty), id));
                local(o, *at); o.push_str(" scale="); local(o, *scale); o.push_str(" angle="); local(o, *angle);
            }
            Draw::Push(t, n) => {
                o.push_str(&format!("push {} ", match t { Transform::Translate => "translate", Transform::Rotate => "rotate", Transform::Scale => "scale" }));
                local(o, *n);
            }
            Draw::Pop => o.push_str("pop"),
        }
    }
    fn path(o: &mut String, xs: &[String]) {
        o.push('[');
        separated(o, xs.len(), &|o, i| o.push_str(&quoted(xs[i].as_bytes())));
        o.push(']');
    }
    fn source(o: &mut String, s: &Source) {
        o.push_str(" @");
        path(o, &s.site);
        o.push_str(" owner=");
        path(o, &s.owner);
        o.push_str(&format!(" instance={} iter=[", s.instance));
        separated(o, s.iter.len(), &|o, i| match &s.iter[i] { Iteration::Static_index(n) => o.push_str(&n.to_string()), Iteration::Local_index(n) => local(o, *n) });
        o.push(']');
        if !s.prefix.is_empty() { o.push_str(" prefix="); path(o, &s.prefix); }
        if let Some(span) = &s.span { o.push_str(&format!(" span={}..{}", span.start, span.finish)); }
    }
    fn statements(o: &mut String, depth: usize, xs: &[Statement]) {
        for s in xs {
            o.push_str(&format!("{}#{} ", " ".repeat(depth * 2), s.id));
            let nested = |o: &mut String, label: &str, xs: &[Statement]| {
                o.push_str(&format!("{} {{\n", label));
                statements(o, depth + 1, xs);
                o.push_str(&format!("{}}}", " ".repeat(depth * 2)));
            };
            match &s.node {
                StatementNode::Assign(n, e) => { local(o, *n); o.push_str(" = "); expression(o, e); }
                StatementNode::Reset_call(n) => o.push_str(&format!("call[{}] <- 0", n)),
                StatementNode::Protect { body, value, failure, handler, mapped, result } => {
                    local(o, *result); o.push_str(" = protect "); nested(o, "body", body);
                    o.push_str(" value="); local(o, *value); o.push_str(" error="); local(o, *failure);
                    o.push(' '); nested(o, "map", handler); o.push_str(" value="); local(o, *mapped);
                }
                StatementNode::Diagnose_rows { rows, policy, index, handler, diagnostic, result } => {
                    local(o, *result); o.push_str(" = diagnose_rows "); local(o, *rows);
                    o.push_str(&format!(" policy={} index=", match policy { DiagnosticPolicy::First => "first", DiagnosticPolicy::RangeLast => "range-last" }));
                    local(o, *index); o.push(' '); nested(o, "map", handler); o.push_str(" diagnostic="); local(o, *diagnostic);
                }
                StatementNode::If { condition, yes, no } => {
                    o.push_str("if "); local(o, *condition); o.push(' '); nested(o, "then", yes); o.push(' '); nested(o, "else", no);
                }
                StatementNode::Iterate { sequence, index, item, limit, body } => {
                    o.push_str("iterate "); local(o, *sequence); o.push_str(" index="); local(o, *index);
                    o.push_str(" item="); local(o, *item); o.push_str(&format!(" limit={} ", limit)); nested(o, "do", body);
                }
                StatementNode::Counted { first, last, count, index, item, limit, stop, init, body } => {
                    o.push_str("counted "); local(o, *first); o.push_str(".."); local(o, *last);
                    o.push_str(" count="); local(o, *count); o.push_str(" index="); local(o, *index);
                    o.push_str(" item="); local(o, *item); o.push_str(&format!(" limit={} ", limit));
                    if let Some(n) = stop { o.push_str("stop="); local(o, *n); o.push(' '); }
                    nested(o, "init", init); o.push(' '); nested(o, "do", body);
                }
                StatementNode::Store { sequence, index, value } => {
                    local(o, *sequence); o.push('['); local(o, *index); o.push_str("] <- "); local(o, *value);
                }
                StatementNode::State_read { slot, result, init, initial } => {
                    local(o, *result); o.push_str(&format!(" = state[{}] ", slot)); nested(o, "init", init);
                    o.push_str(" value="); local(o, *initial);
                }
                StatementNode::State_peek { slot, found, value } => {
                    local(o, *found); o.push_str(&format!(" = state[{}] pending ", slot)); local(o, *value);
                }
                StatementNode::State_write { slot, value } => { o.push_str(&format!("state[{}] <- ", slot)); local(o, *value); }
                StatementNode::Invoke { region, args, result } => {
                    local(o, *result); o.push_str(&format!(" = region[{}](", region));
                    separated(o, args.len(), &|o, i| local(o, args[i])); o.push(')');
                }
                StatementNode::Draw(d) => draw(o, d),
            }
            source(o, &s.source);
            o.push('\n');
        }
    }
    fn instruction(o: &mut String, x: &region::Instruction) {
        use region::Instruction as I;
        let regs = |r: &[i64]| r.iter().map(|r| format!("r{}", r)).collect::<Vec<_>>().join(" ");
        let proj = |p: &region::Projection| match p { region::Projection::Value => "value", region::Projection::Valid => "valid" };
        match x {
            I::Const(f) => o.push_str(&lisp_float(*f)),
            I::Input(s, w, c) => o.push_str(&format!("input[{}].{}/{}", s, c, w)),
            I::Uniform(u, c) => o.push_str(&format!("uniform[{}].{}", u, c)),
            I::Frame(name) => o.push_str(&format!("frame.{}", name)),
            I::Accumulator(c) => o.push_str(&format!("accumulator.{}", c)),
            I::Binary(op, a, b) => o.push_str(&format!("{}(r{} r{})", primitive_name(&Primitive::Numeric(*op)), a, b)),
            I::Unary(op, a) => o.push_str(&format!("{}(r{})", primitive_name(&Primitive::Math(*op)), a)),
            I::Noise3(x, y, z, seed, octaves, lacunarity, gain) => {
                let fractal = |v: &region::LiveFloat| match v { region::LiveFloat::Fixed(v) => hex_float(*v), region::LiveFloat::Live(r) => format!("r{}", r) };
                let oct = match octaves { region::LiveInt::Fixed(n) => n.to_string(), region::LiveInt::Live(r) => format!("r{}", r) };
                let extra = if *lacunarity == region::LiveFloat::Fixed(2.0) && *gain == region::LiveFloat::Fixed(0.5) { String::new() }
                    else { format!(" lacunarity={} gain={}", fractal(lacunarity), fractal(gain)) };
                o.push_str(&format!("noise3(r{} r{} r{} seed={} octaves={}{})", x, y, z, seed, oct, extra));
            }
            I::Rand_at(seed, index) => o.push_str(&format!("rand_at(r{} r{})", seed, index)),
            I::Fma(a, b, c) => o.push_str(&format!("fma(r{} r{} r{})", a, b, c)),
            I::Checked_mul(p, a, b) => o.push_str(&format!("mul_checked.{}(r{} r{})", proj(p), a, b)),
            I::Array_prepare(i, ty) => o.push_str(&format!("array_prepare u{}:{}", i, ty::to_string(ty))),
            I::Gather(a, i, w, c) => o.push_str(&format!("gather(r{} r{} width={} component={})", a, i, w, c)),
            I::Int_gather(a, i) => o.push_str(&format!("int_gather(r{} r{})", a, i)),
            I::Text_const(s) => o.push_str(&format!("text_const {}", quoted(s))),
            I::Text_input(i) => o.push_str(&format!("text_input in{}", i)),
            I::Text_uniform(i) => o.push_str(&format!("text_uniform u{}", i)),
            I::Text_gather(a, i) => o.push_str(&format!("text_gather(r{} r{})", a, i)),
            I::Text_select(c, a, b) => o.push_str(&format!("text_select(r{} r{} r{})", c, a, b)),
            I::Trace(field, r) => o.push_str(&format!("trace.{}({})", field, regs(r))),
            I::Stage(_, r) => o.push_str(&format!("stage({})", regs(r))),
            I::Checked_fma(p, a, b, c) => o.push_str(&format!("fma_checked.{}(r{} r{} r{})", proj(p), a, b, c)),
            I::Select(c, a, b) | I::Int_select(c, a, b) => o.push_str(&format!("select(r{} r{} r{})", c, a, b)),
            I::Int_const(n) => o.push_str(&format!("int:{}", n)),
            I::Int_input(i) => o.push_str(&format!("input:int[{}]", i)),
            I::Int_uniform(i) => o.push_str(&format!("uniform:int[{}]", i)),
            I::Int_accumulator => o.push_str("accumulator:int"),
            I::Int_frame(name) => o.push_str(&format!("frame:int[{}]", name)),
            I::Int_binary(op, a, b) => o.push_str(&format!("{}(r{} r{})", packed_ops::integer_name(*op), a, b)),
            I::Int_compare(op, a, b) => o.push_str(&format!("{}(r{} r{})", packed_ops::comparison_name(*op), a, b)),
            I::Int_abs(a) => o.push_str(&format!("int:abs(r{})", a)),
            I::Convert(op, a) => {
                let name = match op {
                    region::Conversion::ToFloat => "float".to_owned(),
                    region::Conversion::Truncate => "int".to_owned(),
                    region::Conversion::Floor => "floor".to_owned(),
                    region::Conversion::Ceil => "ceil".to_owned(),
                    region::Conversion::Round => "round".to_owned(),
                    region::Conversion::CoerceInt => "coerce:int".to_owned(),
                    region::Conversion::IntegerResult(name) => format!("int-result:{}", name),
                };
                o.push_str(&format!("{}(r{})", name, a));
            }
            I::Guarded(guards, body) => {
                o.push_str("guard[");
                separated(o, guards.len(), &|o, i| o.push_str(&format!("r{}={}", guards[i].0, guards[i].1)));
                o.push_str("] ");
                instruction(o, body);
            }
            I::Raise(ty, code, message, cause) => {
                o.push_str(&format!("error<{} {} {}", ty::to_string(ty), quoted(code.as_bytes()), quoted(message.as_bytes())));
                if let Some(cause) = cause { o.push_str(&format!(" cause={}", quoted(cause))); }
                o.push('>');
            }
            I::Loop_begin(l) => o.push_str(&format!("loop r{}..r{} end={}", l.first, l.last, l.finish)),
            I::Loop_carry(loop_, component, ty) => o.push_str(&format!("carry r{}.{}:{}", loop_, component, ty::to_string(ty))),
            I::Loop_end(loop_, _, stop) => {
                o.push_str(&format!("next r{}{}", loop_, stop.map(|s| format!(" until r{}", s)).unwrap_or_default()));
            }
            I::Loop_result(loop_, component, ty) => o.push_str(&format!("result r{}.{}:{}", loop_, component, ty::to_string(ty))),
        }
    }
    o.push_str("core\n");
    if p.call_slots > 0 { o.push_str(&format!("  calls {}\n", p.call_slots)); }
    for (n, v) in p.constants.iter().enumerate() {
        o.push_str(&format!("  ${} = ", n));
        literal(&mut o, v);
        o.push('\n');
    }
    for (n, ty) in p.locals.iter().enumerate() { o.push_str(&format!("  %{} : {}\n", n, ty::to_string(ty))); }
    for (n, s) in p.state_slots.iter().enumerate() {
        o.push_str(&format!("  state[{}] : {} plane={} prefix=", n, ty::to_string(&s.ty), match s.plane { Plane::Frame => "frame", Plane::Host => "host" }));
        path(&mut o, &s.prefix);
        o.push_str(" zone=");
        path(&mut o, &s.zone);
        source(&mut o, &s.source);
        o.push('\n');
    }
    for (n, r) in p.regions.iter().enumerate() {
        let names = |a: &[i64]| a.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(" ");
        o.push_str(&format!(
            "  region[{}] {} {} count={} widths=[{}] output=[{}] uniforms={} skip=[{}] stages={}\n",
            n,
            match r.iteration { region::Iteration::Zip => "zip", region::Iteration::Product => "product" },
            match &r.result {
                region::RegionResult::Collect => "collect",
                region::RegionResult::Sum(zero) => if *zero { "sum0" } else { "sum" },
                region::RegionResult::Accumulate(_, scan) => if *scan { "scan" } else { "fold" },
            },
            r.count.map(|c| c.to_string()).unwrap_or_else(|| "?".to_owned()),
            names(&r.widths),
            r.outputs.iter().map(|a| names(a)).collect::<Vec<_>>().join(" | "),
            r.uniforms.len(),
            names(&r.skip),
            r.stages.len(),
        ));
        for (i, x) in r.code.iter().enumerate() {
            o.push_str(&format!("    r{} = ", i));
            instruction(&mut o, x);
            o.push('\n');
        }
    }
    statements(&mut o, 1, &p.body);
    o.push_str("  return ");
    separated(&mut o, p.results.len(), &|o, i| local(o, p.results[i]));
    o.push('\n');
    o
}

#[cfg(test)]
mod tests {
    use super::*;

    fn source() -> Source {
        Source {
            site: vec!["g".into(), "value".into()],
            owner: vec!["g".into()],
            instance: 2,
            prefix: Vec::new(),
            iter: vec![Iteration::Static_index(3)],
            span: Some(Span { start: 10, finish: 20 }),
        }
    }
    fn s(id: i64, node: StatementNode) -> Statement { Statement { id, source: source(), node } }
    fn program(constants: Vec<Literal>, locals: Vec<Ty>, body: Vec<Statement>, results: Vec<i64>) -> Program {
        Program { constants, locals, body, results, state_slots: Vec::new(), regions: Vec::new(), call_slots: 0 }
    }
    fn check(p: &Program, expected: &str) {
        let actual = pp(p);
        assert_eq!(actual, expected, "Unexpected core print");
        assert_eq!(actual, pp(p));
    }
    const TAIL: &str = "@[\"g\" \"value\"] owner=[\"g\"] instance=2 iter=[3] span=10..20";

    #[test]
    fn prints_arithmetic() {
        let p = program(
            vec![Literal::Int(2)],
            vec![Ty::Int, Ty::Float, Ty::Float],
            vec![
                s(0, StatementNode::Assign(0, Expr::Constant(0))),
                s(1, StatementNode::Assign(1, Expr::Frame(FrameField::Time))),
                s(2, StatementNode::Assign(2, Expr::Prim(Primitive::Numeric(packed_ops::Binary::Mul), vec![("a".into(), 0), ("b".into(), 1)]))),
            ],
            vec![2],
        );
        let expected = format!("core\n  $0 = 2\n  %0 : int\n  %1 : float\n  %2 : float\n  #0 %0 = $0 {t}\n  #1 %1 = frame.t {t}\n  #2 %2 = *(\"a\":%0 \"b\":%1) {t}\n  return %2\n", t = TAIL);
        check(&p, &expected);
    }

    #[test]
    fn prints_if() {
        let p = program(
            vec![Literal::Text(b"yes".to_vec()), Literal::Text(b"no".to_vec()), Literal::Bool(true)],
            vec![Ty::Bool, Ty::Text],
            vec![
                s(0, StatementNode::Assign(0, Expr::Constant(2))),
                s(1, StatementNode::If {
                    condition: 0,
                    yes: vec![s(2, StatementNode::Assign(1, Expr::Constant(0)))],
                    no: vec![s(3, StatementNode::Assign(1, Expr::Constant(1)))],
                }),
            ],
            vec![1],
        );
        let expected = format!("core\n  $0 = \"yes\"\n  $1 = \"no\"\n  $2 = true\n  %0 : bool\n  %1 : text\n  #0 %0 = $2 {t}\n  #1 if %0 then {{\n    #2 %1 = $0 {t}\n  }} else {{\n    #3 %1 = $1 {t}\n  }} {t}\n  return %1\n", t = TAIL);
        check(&p, &expected);
    }

    #[test]
    fn prints_state_and_iterate() {
        let mut p = program(
            vec![Literal::List(vec![Literal::Float(1.0), Literal::Float(2.0)]), Literal::Float(0.0)],
            vec![Ty::List(Box::new(Ty::Float)), Ty::Int, Ty::Float, Ty::Float],
            vec![
                s(0, StatementNode::Assign(0, Expr::Constant(0))),
                s(1, StatementNode::State_read { slot: 0, result: 3, init: vec![s(2, StatementNode::Assign(3, Expr::Constant(1)))], initial: 3 }),
                s(3, StatementNode::Iterate {
                    sequence: 0, index: 1, item: 2, limit: 4096,
                    body: vec![s(4, StatementNode::Assign(3, Expr::Prim(Primitive::Numeric(packed_ops::Binary::Add), vec![("a".into(), 3), ("b".into(), 2)])))],
                }),
                s(5, StatementNode::State_write { slot: 0, value: 3 }),
            ],
            vec![3],
        );
        p.state_slots = vec![StateSlot { source: source(), prefix: vec!["call".into()], zone: vec!["g".into(), "s".into()], plane: Plane::Frame, ty: Ty::Float }];
        let t = TAIL;
        let expected = format!(
            "core\n  $0 = list[1.0 2.0]\n  $1 = 0.0\n  %0 : list:float\n  %1 : int\n  %2 : float\n  %3 : float\n  state[0] : float plane=frame prefix=[\"call\"] zone=[\"g\" \"s\"] {t}\n  #0 %0 = $0 {t}\n  #1 %3 = state[0] init {{\n    #2 %3 = $1 {t}\n  }} value=%3 {t}\n  #3 iterate %0 index=%1 item=%2 limit=4096 do {{\n    #4 %3 = +(\"a\":%3 \"b\":%2) {t}\n  }} {t}\n  #5 state[0] <- %3 {t}\n  return %3\n");
        check(&p, &expected);
    }

    #[test]
    fn hex_float_and_quoting() {
        assert_eq!(hex_float(2.0), "0x1p+1");
        assert_eq!(hex_float(0.5), "0x1p-1");
        assert_eq!(hex_float(1.5), "0x1.8p+0");
        assert_eq!(hex_float(0.0), "0x0p+0");
        assert_eq!(quoted(&[b'a', b'"', b'b', 10, 1]), "\"a\\\"b\\n\\001\"");
    }

    #[test]
    fn literal_round_trip() {
        let v = Value::Record(vec![("a".into(), Value::Vec3(1.0, 2.0, 3.0)), ("b".into(), Value::List(Arc::new(vec![Value::Int(1)])))]);
        assert_eq!(value_of_literal(&literal_of_value(&v).unwrap()), v);
        assert!(literal_of_value(&Value::Fn(value::FnId(0, 0))).is_none());
    }
}
