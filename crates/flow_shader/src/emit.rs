//! The Metal (MSL) kernel printer and the raster-stage instruction printer (port of
//! lib/flow_shader/emit.ml). The kernel is printed twice: the first pass counts each register's
//! references and where they are, the second prints a register read once inline.
//!
//! Rust shape notes: OCaml's exception-based `fail` becomes a recorded failure (`Gen::failure`)
//! after which the printer stops; the layout visitor is the `Visit` impl of the generator.
#![allow(non_snake_case)]
use rays_math::hash::md5_hex;
use crate::noise;
use crate::trace::Back;
pub use crate::trace::Curves;
use crate::trace;
use flow_ir::core_ir::Region::{self, Instruction, Loop, Projection, Stage};
use flow_ir::kernel_layout::{self, Scope, Visit};
use flow_ir::packed;
use flow_syntax::packed_ops::{self, Binary, Comparison, Conversion, Integer, Unary};
use flow_syntax::ty::Ty;
use flow_syntax::value;
use ogpu_core::shader::{Binding, BindingKind, Stage as ShaderStage};
use rays_diagnostic::Diagnostic;
use std::collections::HashMap;
use std::sync::atomic::AtomicI64;
use std::sync::{Arc, Mutex};

pub type SpirvResult = Result<(Vec<u8>, String, Vec<Binding>), Diagnostic>;

enum SpirvState {
    Pending(Box<dyn FnOnce() -> SpirvResult + Send>),
    Done(SpirvResult),
}

/// OCaml's `Lazy.t`: the SPIR-V form of a kernel, printed when first asked.
#[derive(Clone)]
pub struct SpirvLazy(Arc<Mutex<SpirvState>>);

impl SpirvLazy {
    pub fn from_val(result: SpirvResult) -> SpirvLazy {
        SpirvLazy(Arc::new(Mutex::new(SpirvState::Done(result))))
    }
    pub fn force(&self) -> SpirvResult {
        let mut state = self.0.lock().unwrap();
        if let SpirvState::Pending(_) = &*state {
            let SpirvState::Pending(thunk) = std::mem::replace(&mut *state, SpirvState::Done(Err(value::fail("E_GPU_FORM", "SPIR-V thunk re-entered.")))) else { unreachable!() };
            *state = SpirvState::Done(thunk());
        }
        match &*state {
            SpirvState::Done(r) => r.clone(),
            SpirvState::Pending(_) => unreachable!(),
        }
    }
}

#[derive(Clone)]
pub struct Kernel {
    pub source: String,
    pub entry: String,
    /// The same kernel as a Vulkan SPIR-V module, its entry and its bindings, printed when first asked.
    pub spirv: SpirvLazy,
    pub interface: Vec<Binding>,
    pub uniform_layout: Vec<(String, i64, i64)>,
    pub uniform_bytes: i64,
    pub uint32_uniforms: Vec<i64>,
    pub table_seeds: Vec<i64>,
    pub output_widths: Vec<i64>,
    pub output_uint32: Vec<bool>,
    pub input_widths: Vec<i64>,
    pub input_uint32: Vec<bool>,
    pub gathers: Vec<(i64, i64)>,
    pub gather_uint32: Vec<bool>,
    pub counts: Option<i64>,
    pub scene: Option<i64>,
}

/// The optional arguments of [`kernel`] / [`generate`].
#[derive(Clone, Debug)]
pub struct Options {
    pub spirv: bool,
    pub fast: bool,
    pub instanced: bool,
    pub motion: bool,
    pub curves: Option<Curves>,
    pub spheres: bool,
    pub uint32_scalars: bool,
    pub uint32_outputs: Vec<i64>,
    pub uint32_inputs: Vec<i64>,
    pub uint32_tables: Vec<i64>,
    pub unfold: Option<i64>,
}

impl Default for Options {
    fn default() -> Options {
        Options {
            spirv: true,
            fast: false,
            instanced: false,
            motion: false,
            curves: None,
            spheres: true,
            uint32_scalars: false,
            uint32_outputs: vec![],
            uint32_inputs: vec![],
            uint32_tables: vec![],
            unfold: None,
        }
    }
}

pub fn names() -> Vec<String> {
    packed_ops::names()
}

/// The trace helpers, specialised by two static scene facts: `instanced` (an instance structure and
/// its table) and `motion` (the query's time).
fn structure_type(instanced: bool, motion: bool) -> &'static str {
    match (instanced, motion) {
        (true, false) => "instance_acceleration_structure",
        (false, false) => "primitive_acceleration_structure",
        (true, true) => "acceleration_structure<instancing, instance_motion>",
        (false, true) => "acceleration_structure<primitive_motion>",
    }
}

fn xyzw(c: i64) -> char {
    ['x', 'y', 'z', 'w'][c as usize]
}

/// A raster-stage intrinsic; texture t is sampled with sampler t + 1 and read (whole texels, no
/// sampler) at integer coordinates.
fn stage_intrinsic(stage: &Stage, operands: &[String]) -> String {
    match stage {
        Stage::Fwidth => format!("fwidth({})", operands[0]),
        Stage::Sample(t, c) => format!("texture{t}.sample(sampler{},float2({},{})).{}", t + 1, operands[0], operands[1], xyzw(*c)),
        Stage::Sample_level(t, c) => {
            format!("texture{t}.sample(sampler{},float2({},{}),level({})).{}", t + 1, operands[0], operands[1], operands[2], xyzw(*c))
        }
        Stage::Read(t, c) => format!("texture{t}.read(uint2({},{})).{}", operands[0], operands[1], xyzw(*c)),
        Stage::Texture_size(t, c) => format!("float(texture{t}.get_{}())", if *c == 0 { "width" } else { "height" }),
        _ => panic!("Emit.stage_intrinsic"),
    }
}

fn fail_d(message: &str) -> Diagnostic {
    value::fail("E_GPU_FORM", message)
}

fn register(index: i64) -> String {
    format!("r{index}")
}

fn guarded(instruction: &Instruction) -> (Vec<(i64, bool)>, &Instruction) {
    match instruction {
        Instruction::Guarded(more, body) => {
            let (mut inner, i) = guarded(body);
            let mut guards = more.clone();
            guards.append(&mut inner);
            (guards, i)
        }
        i => (vec![], i),
    }
}

fn unguarded(instruction: &Instruction) -> &Instruction {
    guarded(instruction).1
}

fn binary(operation: Binary, a: &str, b: &str) -> Result<String, Diagnostic> {
    let infix = |operator: &str| format!("({a}{operator}{b})");
    Ok(match operation {
        Binary::Add => infix("+"),
        Binary::Sub => infix("-"),
        Binary::Mul => infix("*"),
        Binary::Div => format!("({b}==0.0f?0.0f:{a}/{b})"),
        Binary::Mod => format!("({b}==0.0f?0.0f:fmod(fmod({a},{b})+{b},{b}))"),
        Binary::Pow => format!("pow(fabs({a}),{b})"),
        Binary::Min => format!("min({a},{b})"),
        Binary::Max => format!("max({a},{b})"),
        Binary::Lt => infix("<"),
        Binary::Le => infix("<="),
        Binary::Gt => infix(">"),
        Binary::Ge => infix(">="),
        Binary::Eq => infix("=="),
        Binary::And => format!("({a}!=0.0f&&{b}!=0.0f)"),
        Binary::Or => format!("({a}!=0.0f||{b}!=0.0f)"),
        Binary::Atan2 => format!("atan2({a},{b})"),
        Binary::Hypot => return Err(fail_d("Native-safe hypot is not supported by the Metal kernel target.")),
        Binary::Ldexp => format!("ldexp({a},int({b}))"),
    })
}

fn unary(operation: Unary, a: &str) -> String {
    match operation {
        Unary::Sin => format!("sin({a})"),
        Unary::Cos => format!("cos({a})"),
        Unary::Sqrt => format!("sqrt(fabs({a}))"),
        Unary::Abs => format!("fabs({a})"),
        Unary::Not => format!("({a}==0.0f)"),
        Unary::Tan => format!("tan({a})"),
        Unary::Asin | Unary::Acos => format!(
            "(fabs({a})>1.0f?as_type<float>(0x7fc00000u):{}({a}))",
            if operation == Unary::Asin { "asin" } else { "acos" }
        ),
        Unary::Exp => format!("exp({a})"),
        Unary::Log => format!("({a}<=0.0f?as_type<float>(0x7fc00000u):log({a}))"),
        Unary::Rsqrt => format!("rsqrt(fabs({a}))"),
        Unary::Sign => format!("({a}>0.0f?1.0f:({a}<0.0f?-1.0f:{a}))"),
    }
}

/// The 32-bit lowering of checked arithmetic: the value or zero, or the finite flag.
fn checked(projection: Projection, expression: &str) -> String {
    match projection {
        Projection::Value => format!("(isfinite({expression})?{expression}:0.0f)"),
        Projection::Valid => format!("(isfinite({expression})?1.0f:0.0f)"),
    }
}

fn wrap_integer(value: &str) -> String {
    format!("(as_type<long>(ulong({value})<<1)>>1)")
}

/// Integer registers occupy 64 bits; Int operations wrap in the signed 63-bit domain.
fn integer_status(unsigned: bool, indentation: &str, operation: Integer, b: &str) -> String {
    match operation {
        Integer::Idiv | Integer::Imod => format!("{indentation}sb|=({b}==0)?2u:0u;\n"),
        Integer::Ishl | Integer::Ishr if unsigned => format!("{indentation}sb|=({b}>62u)?4u:0u;\n"),
        Integer::Ishl | Integer::Ishr => format!("{indentation}sb|=({b}<0 || {b}>62)?4u:0u;\n"),
        _ => String::new(),
    }
}

/// A fast compute kernel's float `mod`, equal bit for bit to `binary`'s two-`fmod` form: integral operands
/// under 2^23 (so the quotient's floor and `a - b*q` are exact) skip the precise `fmod`, a slow library
/// call; the zero result keeps the sign of `b`, as `fmod(fmod(a,b)+b,b)` does.
const RAYS_MOD_MSL: &str = "static inline float rays_mod(float a,float b){if(b==0.0f) return 0.0f;if(a==trunc(a)&&b==trunc(b)&&fabs(a)<8388608.0f&&fabs(b)<8388608.0f) return copysign(a-b*floor(a/b),b);return fmod(fmod(a,b)+b,b);}\n";

/// A float as a fast compute kernel's Int: truncated and saturated to the 32-bit range (NaN to its
/// minimum); MSL leaves a conversion past it undefined, and the compiler drops what reads one.
const RAYS_INT_MSL: &str = "static inline int rays_int(float x){return int(fmin(fmax(x,-2147483648.0f),2147483520.0f));}\n";

/// The status bits gather in the thread's `sb` and reach the shared word with the kernel's one atomic.
const FLUSH_STATUS: &str = "if(sb!=0u||i==0) atomic_fetch_or_explicit(status,sb,memory_order_relaxed);";

/// A scope's declarations, consecutive ones of one type on one line (at most twelve), as written by hand.
fn declarations(decls: &str) -> String {
    let mut out = String::with_capacity(decls.len());
    fn flush(out: &mut String, group: &[(String, String)]) {
        if let Some((ty, _)) = group.first() {
            out.push_str(&format!("  {} {};\n", ty, group.iter().map(|(_, i)| i.as_str()).collect::<Vec<_>>().join(",")));
        }
    }
    let split = |line: &str| -> Option<(String, String)> {
        if !(line.starts_with("  ") && line.ends_with(';')) || line.len() < 3 {
            return None;
        }
        let rest = &line[2..line.len() - 1];
        match rest.find(' ') {
            Some(j) if j > 0 => Some((rest[..j].to_owned(), rest[j + 1..].to_owned())),
            _ => None,
        }
    };
    let mut group: Vec<(String, String)> = Vec::new();
    for line in decls.split('\n') {
        match split(line) {
            Some((ty, item)) if group.first().is_some_and(|(t, _)| *t == ty) && group.len() < 12 => group.push((ty, item)),
            Some((ty, item)) => {
                flush(&mut out, &group);
                group = vec![(ty, item)];
            }
            None => {
                flush(&mut out, &group);
                if !line.is_empty() {
                    out.push_str(line);
                    out.push('\n');
                }
                group = vec![];
            }
        }
    }
    flush(&mut out, &group);
    out
}

fn opening(condition: i64, selected: bool) -> String {
    format!("if(r{condition}{}0.0f) {{", if selected { "!=" } else { "==" })
}

/// A fast compute kernel's Int: 32-bit two's complement that wraps; shifts are logical over 32 bits and a
/// count over 31 gives 0 (the narrow register's rule). Division and remainder truncate.
fn integer32(operation: Integer, a: &str, b: &str) -> String {
    let wrap = |operator: &str| format!("as_type<int>(uint({a}){operator}uint({b}))");
    let shift = |operator: &str| format!("(({b}&63)>31?0:as_type<int>(uint({a}){operator}uint({b}&63)))");
    match operation {
        Integer::Iadd => wrap("+"),
        Integer::Isub => wrap("-"),
        Integer::Imul => wrap("*"),
        // -1 apart: the minimum Int over -1 wraps, as the 63-bit form's does
        Integer::Idiv => format!("({b}==0?0:{b}==-1?as_type<int>(0u-uint({a})):{a}/{b})"),
        Integer::Imod => format!("({b}==0||{b}==-1?0:{a}%{b})"),
        Integer::Iand => format!("({a}&{b})"),
        Integer::Ior => format!("({a}|{b})"),
        Integer::Ixor => format!("({a}^{b})"),
        Integer::Ishl => shift("<<"),
        Integer::Ishr => shift(">>"),
    }
}

/// An Int constant as a 32-bit literal: its low word.
fn int_literal(n: i64) -> String {
    let w = n as i32;
    if w < 0 { format!("as_type<int>({}u)", w as u32) } else { w.to_string() }
}

fn integer(operation: Integer, a: &str, b: &str) -> String {
    wrap_integer(&match operation {
        Integer::Iadd => format!("(ulong({a})+ulong({b}))"),
        Integer::Isub => format!("(ulong({a})-ulong({b}))"),
        Integer::Imul => format!("(ulong({a})*ulong({b}))"),
        Integer::Idiv => format!("({b}==0?0:{a}/{b})"),
        Integer::Imod => format!("({b}==0?0:{a}%{b})"),
        Integer::Iand => format!("({a}&{b})"),
        Integer::Ior => format!("({a}|{b})"),
        Integer::Ixor => format!("({a}^{b})"),
        Integer::Ishl => format!("(ulong({a})<<({b}&63))"),
        Integer::Ishr => format!("long((ulong({a})&0x7fffffffffffffffUL)>>({b}&63))"),
    })
}

/// A narrow (32-bit unsigned) Int register's operation: the low 32 bits of the signed-63 result.
fn narrow_integer(operation: Integer, a: &str, b: &str) -> String {
    let shift = |operator: &str| format!("(({b}&63)>31?0u:({a}{operator}uint({b}&63)))");
    match operation {
        Integer::Iadd => format!("({a}+{b})"),
        Integer::Isub => format!("({a}-{b})"),
        Integer::Imul => format!("({a}*{b})"),
        Integer::Iand => format!("({a}&{b})"),
        Integer::Ior => format!("({a}|{b})"),
        Integer::Ixor => format!("({a}^{b})"),
        Integer::Ishl => shift("<<"),
        Integer::Ishr => shift(">>"),
        Integer::Idiv => format!("({b}==0u?0u:{a}/{b})"),
        Integer::Imod => format!("({b}==0u?0u:{a}%{b})"),
    }
}

fn comparison(operation: Comparison, a: &str, b: &str) -> String {
    let op = match operation {
        Comparison::Ilt => "<",
        Comparison::Ile => "<=",
        Comparison::Igt => ">",
        Comparison::Ige => ">=",
        Comparison::Ieq => "==",
    };
    format!("({a}{op}{b}?1.0f:0.0f)")
}

pub const CURVE_CAPSULE_MSL: &str = concat!(
    "inline float rays_curve_nearer(float best,float value,bool valid,float tmin,float tmax) {\n",
    "  return valid && value>=tmin && value<=tmax && value<best ? value : best;\n",
    "}\n",
    "inline bool rays_curve_intersect(float3 origin,float3 direction,float tmin,float tmax,float4 sa,float4 sb,thread float &distance,thread float &parameter) {\n",
    "  float shift=dot((sa.xyz+sb.xyz)*0.5f-origin,direction);\n",
    "  float3 o=origin+direction*shift,ba=sb.xyz-sa.xyz,oa=o-sa.xyz,ob=o-sb.xyz;\n",
    "  tmin-=shift;tmax-=shift;\n",
    "  float radius=sa.w,baba=dot(ba,ba),bard=dot(ba,direction),baoa=dot(ba,oa);\n",
    "  float qa=baba-bard*bard,qb=baba*dot(direction,oa)-baoa*bard;\n",
    "  float qc=baba*dot(oa,oa)-baoa*baoa-radius*radius*baba;\n",
    "  float disc=qb*qb-qa*qc,root=sqrt(max(disc,0.0f)),safe_qa=qa!=0.0f?qa:1.0f;\n",
    "  float t0=(-qb-root)/safe_qa,t1=(-qb+root)/safe_qa;\n",
    "  float y0=baoa+t0*bard,y1=baoa+t1*bard;\n",
    "  float best=INFINITY;\n",
    "  best=rays_curve_nearer(best,t0,qa>0.0f && disc>=0.0f && y0>0.0f && y0<baba,tmin,tmax);\n",
    "  best=rays_curve_nearer(best,t1,qa>0.0f && disc>=0.0f && y1>0.0f && y1<baba,tmin,tmax);\n",
    "  float ha=dot(oa,direction),cap_a=ha*ha-(dot(oa,oa)-radius*radius),root_a=sqrt(max(cap_a,0.0f));\n",
    "  float ta0=-ha-root_a,ta1=-ha+root_a;\n",
    "  best=rays_curve_nearer(best,ta0,cap_a>=0.0f && baoa+ta0*bard<=0.0f,tmin,tmax);\n",
    "  best=rays_curve_nearer(best,ta1,cap_a>=0.0f && baoa+ta1*bard<=0.0f,tmin,tmax);\n",
    "  float hb=dot(ob,direction),cap_b=hb*hb-(dot(ob,ob)-radius*radius),root_b=sqrt(max(cap_b,0.0f));\n",
    "  float tb0=-hb-root_b,tb1=-hb+root_b;\n",
    "  best=rays_curve_nearer(best,tb0,cap_b>=0.0f && baoa+tb0*bard>=baba,tmin,tmax);\n",
    "  best=rays_curve_nearer(best,tb1,cap_b>=0.0f && baoa+tb1*bard>=baba,tmin,tmax);\n",
    "  if(!isfinite(best)) return false;\n",
    "  distance=best+shift;parameter=clamp((baoa+best*bard)/max(baba,1e-8f),0.0f,1.0f);\n",
    "  return true;\n",
    "}"
);

/// A raster placement (Raster): elements are vertex ids or interpolated fragment inputs, gathers read
/// the renderer's bound buffers, and outputs are stage results; the stage function around the body is
/// Raster's. `input source component` and `int_input source` print an element's inputs, `buffer u`
/// names the bound buffer behind array uniform `u`, `output k component value` is the statement
/// storing an output, and `builtin name` reads a built-in fact.
pub struct Raster {
    pub input: Box<dyn Fn(i64, i64) -> String>,
    pub int_input: Box<dyn Fn(i64) -> String>,
    pub buffer: Box<dyn Fn(i64) -> String>,
    pub output: Box<dyn Fn(i64, i64, &str) -> String>,
    pub builtin: Box<dyn Fn(&str) -> String>,
}

fn no_spirv() -> SpirvLazy {
    SpirvLazy::from_val(Err(fail_d("This body has no SPIR-V form.")))
}

/// The kernel as plain data, for a disk cache: the SPIR-V printed if `vulkan`, dropped otherwise.
pub fn settled(vulkan: bool, msl: &Kernel) -> Kernel {
    Kernel { spirv: SpirvLazy::from_val(if vulkan { msl.spirv.force() } else { no_spirv().force() }), ..msl.clone() }
}

/// `options.spirv` (default true) also keeps the means to print the SPIR-V form, which holds the
/// program until it is printed: a host that will never ask, Metal's, passes false.
pub fn kernel(options: &Options, program: &packed::T) -> Result<Kernel, Diagnostic> {
    let spirv = if !options.spirv {
        no_spirv()
    } else {
        let (o, p) = (options.clone(), program.clone());
        SpirvLazy(Arc::new(Mutex::new(SpirvState::Pending(Box::new(move || {
            crate::spirv::kernel(
                &crate::spirv::Options {
                    fast: o.fast,
                    instanced: o.instanced,
                    motion: o.motion,
                    curves: o.curves.is_some(),
                    spheres: o.spheres,
                    uint32_scalars: o.uint32_scalars,
                    uint32_outputs: o.uint32_outputs.clone(),
                    uint32_inputs: o.uint32_inputs.clone(),
                    uint32_tables: o.uint32_tables.clone(),
                    unfold: o.unfold,
                },
                &p,
            )
            .map(|k| (k.bytes, k.entry, k.interface))
        })))))
    };
    generate(options, None, program).map(|k| Kernel { spirv, ..k })
}

pub fn stage_body(raster: Raster, program: &packed::T) -> Result<String, Diagnostic> {
    generate(&Options { fast: true, ..Options::default() }, Some(raster), program).map(|k| k.source)
}

fn u(x: i64) -> usize {
    x as usize
}

type Guards = Vec<(i64, bool)>;

struct Gen {
    layout: Arc<kernel_layout::T>,
    fast: bool,
    /// Int registers are 32-bit (`integer32`): a fast compute kernel, never a raster stage.
    int32: bool,
    raster: Option<Raster>,
    unfold: Option<i64>,
    instancing: bool,
    moving: bool,
    native: bool,
    input_uint32: Vec<bool>,
    gather_uint32: Vec<bool>,
    output_flags: Vec<bool>,
    uint32_uniforms: Vec<i64>,
    references: Vec<i64>,
    referenced: Vec<(i64, Guards)>,
    defined: Vec<(i64, Guards)>,
    inlined: Vec<Option<String>>,
    counting: bool,
    context: (i64, Guards),
    blocks: Vec<i64>,
    defined_in: Vec<Vec<i64>>,
    read_in: Vec<Vec<i64>>,
    local: Vec<bool>,
    named: Vec<bool>,
    hits: HashMap<(i64, Guards, bool, bool, Vec<i64>), String>,
    scans: Vec<i64>,
    scopes: Vec<Scope::T>,
    failure: Option<Diagnostic>,
}

fn prefix(a: &[i64], b: &[i64]) -> bool {
    a.len() <= b.len() && a.iter().zip(b.iter()).all(|(x, y)| x == y)
}

impl Gen {
    fn fail(&mut self, message: &str) -> String {
        if self.failure.is_none() {
            self.failure = Some(fail_d(message));
        }
        String::new()
    }

    fn ok(&mut self, r: Result<String, Diagnostic>) -> String {
        match r {
            Ok(s) => s,
            Err(d) => {
                if self.failure.is_none() {
                    self.failure = Some(d);
                }
                String::new()
            }
        }
    }

    fn code(&self, slot: i64) -> &Instruction {
        &self.layout.view.code[u(slot)]
    }

    /// An Int expression widened to 64 bits where registers are 32-bit.
    fn wide(&self, x: String) -> String {
        if self.int32 { format!("long({x})") } else { x }
    }

    /// An expression as an Int register: a float through `rays_int` in 32-bit registers.
    fn int_cast(&self, x: String, float: bool) -> String {
        if self.int32 && float { format!("rays_int({x})") } else { format!("{}({x})", self.lt()) }
    }

    /// The wide Int register type.
    fn lt(&self) -> &'static str {
        if self.int32 { "int" } else { "long" }
    }

    fn integer_register(&self, slot: i64) -> bool {
        Region::integer_register(self.code(slot))
    }

    fn narrow(&self, slot: i64) -> bool {
        self.layout.narrow[u(slot)]
    }

    fn reg(&mut self, r: i64) -> String {
        if let Some(text) = &self.inlined[u(r)] {
            return format!("({text})");
        }
        if self.counting {
            self.references[u(r)] += 1;
            self.referenced[u(r)] = self.context.clone();
            self.read_in[u(r)] = self.blocks.clone();
            if !prefix(&self.defined_in[u(r)], &self.blocks) {
                self.local[u(r)] = false;
            }
        }
        register(r)
    }

    /// A raster stage reads an interpolated input where it is used.
    fn inline(&self, slot: usize, instruction: &Instruction) -> bool {
        (!self.counting && self.raster.is_some() && !self.named[slot] && matches!(instruction, Instruction::Input(..)))
            || (!self.counting
                && self.references[slot] == 1
                && !self.named[slot]
                && !self.layout.finite_checks[slot]
                && (match instruction {
                    Instruction::Trace(..) | Instruction::Raise(..) => false,
                    Instruction::Int_binary(Integer::Idiv | Integer::Imod | Integer::Ishl | Integer::Ishr, _, _) => false,
                    Instruction::Stage(
                        Stage::Trace_triangles(_) | Stage::Trace_any | Stage::Trace_curves(_) | Stage::Trace_curves_any | Stage::Group_scan(_) | Stage::Store,
                        _,
                    ) => false,
                    _ => true,
                })
                && self.defined[slot].0 == self.referenced[slot].0
                && prefix(&self.defined_in[slot], &self.read_in[slot]))
    }

    fn int_of(&mut self, r: i64) -> String {
        if self.integer_register(r) && !self.narrow(r) {
            self.reg(r)
        } else {
            let float = !self.integer_register(r);
            let x = self.reg(r);
            self.int_cast(x, float)
        }
    }

    /// A register's low 32 bits as a uint.
    fn low(&mut self, r: i64) -> String {
        if self.narrow(r) {
            self.reg(r)
        } else {
            format!("uint({})", self.int_of(r))
        }
    }

    /// An integer-valued register as an Int-register expression (32-bit in a fast compute kernel): an
    /// integer register, its conversion to float, an integral constant, or +, -, *, mod, min, max of two such.
    fn whole(&mut self, r: i64) -> Option<String> {
        if self.integer_register(r) {
            return Some(self.int_of(r));
        }
        let instruction = self.layout.view.code[u(r)].clone();
        match unguarded(&instruction) {
            Instruction::Convert(Conversion::ToFloat, x) if self.integer_register(*x) => Some(self.int_of(*x)),
            Instruction::Const(c) if c.is_finite() && c.fract() == 0.0 && c.abs() < if self.int32 { 2147483648.0 } else { 9007199254740992.0 } => {
                Some(if self.int32 { int_literal(*c as i64) } else { format!("{:.0}L", c) })
            }
            Instruction::Binary(op @ (Binary::Add | Binary::Sub | Binary::Mul | Binary::Mod | Binary::Min | Binary::Max), a, b) if self.fast => {
                let wb = self.whole(*b);
                let wa = self.whole(*a);
                match (wa, wb) {
                    (Some(a), Some(b)) => Some(format!(
                        "({})",
                        match op {
                            Binary::Add | Binary::Sub | Binary::Mul if self.int32 => integer32(
                                match op {
                                    Binary::Add => Integer::Iadd,
                                    Binary::Sub => Integer::Isub,
                                    _ => Integer::Imul,
                                },
                                &a,
                                &b,
                            ),
                            Binary::Add => format!("{a}+{b}"),
                            Binary::Sub => format!("{a}-{b}"),
                            Binary::Mul => format!("{a}*{b}"),
                            Binary::Mod if self.int32 => format!("{b}==0?0:({a}%{b})+((({a}%{b})!=0&&(({a}%{b})<0)!=({b}<0))?{b}:0)"),
                            Binary::Mod => format!("{b}==0?0:({a}%{b}+{b})%{b}"),
                            Binary::Min => format!("min({a},{b})"),
                            _ => format!("max({a},{b})"),
                        }
                    )),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn float_of(&mut self, r: i64) -> String {
        if self.integer_register(r) {
            format!("float({})", self.reg(r))
        } else {
            self.reg(r)
        }
    }

    fn table(&self, seed: i64) -> i64 {
        match self.layout.table_seeds.iter().position(|s| *s == seed) {
            Some(index) => index as i64 * 512,
            None => panic!("noise table seed without its table"),
        }
    }

    fn gather_index(&mut self, slot: i64) -> i64 {
        match self.layout.gathers.iter().position(|(s, _)| *s == slot) {
            Some(k) => k as i64,
            None => {
                self.fail("Gather without its table.");
                0
            }
        }
    }

    fn uniform_word(&self, offset: i64) -> String {
        if self.uint32_uniforms.contains(&(offset * 4)) {
            format!("uniforms[{offset}]")
        } else {
            format!("as_type<float>(uniforms[{offset}])")
        }
    }

    fn element(&mut self, integer: bool, prepared: i64, index: i64, width: i64, component: i64) -> String {
        let k = self.gather_index(prepared);
        if self.failure.is_some() {
            return String::new();
        }
        let index = if self.narrow(index) { self.reg(index) } else { self.int_of(index) };
        let value = match &self.raster {
            Some(r) => format!("{}[{}*{}+{}]", (r.buffer)(self.layout.gathers[u(k)].1 .0), index, width, component),
            None => {
                let raw = format!(
                    "gather{k}[clamp(int({index}),0,max(int(uniforms[{}])-1,0))*{width}+{component}]",
                    self.layout.gather_lengths[u(k)]
                );
                if self.gather_uint32[u(k)] {
                    raw
                } else {
                    format!("as_type<float>({raw})")
                }
            }
        };
        if integer {
            self.int_cast(value, !self.gather_uint32[u(k)])
        } else if self.gather_uint32[u(k)] {
            format!("float({value})")
        } else {
            value
        }
    }

    fn scene_count(&self, name: &str) -> String {
        let word = if name == "gpu/scene-spheres" { self.layout.scene_spheres } else { self.layout.scene_strand_count };
        format!("uniforms[{}]", word.expect("scene count word"))
    }

    fn current(&mut self) -> &mut Scope::T {
        self.scopes.last_mut().expect("a scope")
    }

    /// A ray query (Trace): its result is held in locals declared in the scope and stored once a
    /// scope, guard set and ray (registers `r`), where its first field is read.
    fn trace_query(&mut self, guards: &[(i64, bool)], slot: i64, stage: &Stage, r: &[i64]) -> String {
        let curve = matches!(stage, Stage::Trace_curves(_) | Stage::Trace_curves_any);
        let any = matches!(stage, Stage::Trace_any | Stage::Trace_curves_any);
        let (instancing, moving, native) = (self.instancing, self.moving, self.native);
        let key = (self.current().id, guards.to_vec(), curve, any, r.to_vec());
        let name = match self.hits.get(&key) {
            Some(name) => name.clone(),
            None => {
                let name = format!("hit{slot}");
                fn declare(scope: &mut Scope::T, name: &str, ty: &str, field: &str, zero: &str) {
                    scope.decls.push_str(&format!("  {ty} {name}{field}={zero};\n"));
                }
                if curve && !native {
                    // a0..a7 and a6, a7 again: every operand print counts a read
                    let mut a: Vec<String> = Vec::new();
                    for k in 0..8 {
                        a.push(self.float_of(r[k]));
                    }
                    let a6 = self.float_of(r[6]);
                    let a7 = self.float_of(r[7]);
                    let committed = "q.get_committed_intersection_type()==intersection_type::bounding_box";
                    let tail = if any {
                        format!("{name}={committed};")
                    } else {
                        format!("{name}_h={committed};{name}_t=q.get_committed_distance();{name}_p=q.get_committed_primitive_id();{name}_u=best_parameter;")
                    };
                    let body = format!(
                        "{{ray r(float3({},{},{}),float3({},{},{}),{},{});intersection_params params;intersection_query<> q(r,scene_curves,params);float best=INFINITY;uint best_primitive=0u;float best_parameter=0.0f;while(q.next()){{if(q.get_candidate_intersection_type()==intersection_type::bounding_box){{uint k=q.get_candidate_primitive_id();float t,u;if(rays_curve_intersect(r.origin,r.direction,{},{},scene_strands[2u*k],scene_strands[2u*k+1u],t,u)){{q.commit_bounding_box_intersection(t);if(t<best){{best=t;best_primitive=k;best_parameter=u;}}}}}}}}{}}}",
                        a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a6, a7, tail
                    );
                    let scope = self.current();
                    if any {
                        declare(scope, &name, "bool", "", "false");
                    } else {
                        declare(scope, &name, "bool", "_h", "false");
                        declare(scope, &name, "float", "_t", "0.0f");
                        declare(scope, &name, "uint", "_p", "0u");
                        declare(scope, &name, "float", "_u", "0.0f");
                    }
                    let indentation = Scope::indent(scope);
                    scope.code.push_str(&format!("{indentation}{body}\n"));
                } else {
                    let mut a: Vec<String> = Vec::new();
                    for k in 0..8 {
                        a.push(self.float_of(r[k]));
                    }
                    let tags: Vec<&str> = if !moving { vec![] } else { vec![if instancing { "instance_motion" } else { "primitive_motion" }] };
                    let mut data: Vec<&str> = Vec::new();
                    if curve {
                        data.push("curve_data");
                    } else if !any {
                        data.push("triangle_data");
                    }
                    if instancing && !curve {
                        data.push("instancing");
                    }
                    if !curve {
                        data.extend(tags);
                    }
                    let data = data.join(", ");
                    let target = if curve {
                        "scene_curves".to_owned()
                    } else if moving {
                        let a8 = self.float_of(r[8]);
                        format!("scene,{a8}")
                    } else {
                        "scene".to_owned()
                    };
                    let call = format!("q.intersect(ray(float3({},{},{}),float3({},{},{}),{},{}),{})", a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], target);
                    let kind = if curve { "curve" } else { "triangle" };
                    let scope = self.current();
                    let store = if any {
                        declare(scope, &name, "bool", "", "false");
                        format!("{name}={call}.type==intersection_type::{kind};")
                    } else {
                        declare(scope, &name, "bool", "_h", "false");
                        declare(scope, &name, "float", "_t", "0.0f");
                        declare(scope, &name, "uint", "_p", "0u");
                        if curve {
                            declare(scope, &name, "float", "_u", "0.0f");
                        } else {
                            declare(scope, &name, "float2", "_uv", "0.0f");
                        }
                        if instancing && !curve {
                            declare(scope, &name, "uint", "_i", "0u");
                        }
                        format!(
                            "auto h={call};{name}_h=h.type==intersection_type::{kind};{name}_t=h.distance;{name}_p=h.primitive_id;{}",
                            if curve {
                                format!("{name}_u=h.curve_parameter;")
                            } else {
                                format!("{name}_uv=h.triangle_barycentric_coord;{}", if instancing { format!("{name}_i=h.instance_id;") } else { String::new() })
                            }
                        )
                    };
                    let indentation = Scope::indent(scope);
                    scope.code.push_str(&format!(
                        "{indentation}{{intersector<{data}> q;q.assume_geometry_type(geometry_type::{kind});q.force_opacity(forced_opacity::opaque);{}{}{}}}\n",
                        if curve {
                            "q.assume_curve_type(curve_type::round);q.assume_curve_basis(curve_basis::linear);q.assume_curve_control_point_count(2);"
                        } else {
                            ""
                        },
                        if any { "q.accept_any_intersection(true);" } else { "" },
                        store
                    ));
                }
                self.hits.insert(key, name.clone());
                name
            }
        };
        match stage {
            Stage::Trace_any | Stage::Trace_curves_any => format!("({name}?1.0f:0.0f)"),
            Stage::Trace_triangles(0) | Stage::Trace_curves(0) => format!("{name}_t"),
            Stage::Trace_triangles(1) | Stage::Trace_curves(1) => format!("{name}_p"),
            Stage::Trace_triangles(2) => format!("{name}_uv.x"),
            Stage::Trace_triangles(3) => format!("{name}_uv.y"),
            Stage::Trace_curves(2) => format!("{name}_u"),
            Stage::Trace_triangles(4) | Stage::Trace_curves(3) => format!("({name}_h?1.0f:0.0f)"),
            _ => {
                if instancing {
                    format!("{name}_i")
                } else {
                    "0u".to_owned()
                }
            }
        }
    }

    /// A scene table read at uint indices: a triangle corner (through the instance's vertex base),
    /// an instance-table word, a sphere or a strand end.
    fn scene_read(&mut self, stage: &Stage, r: &[i64]) -> String {
        let (instancing, stride) = (self.instancing, if self.moving { 32 } else { 16 });
        match stage {
            Stage::Scene_vertex(ending, c) => {
                let base = if instancing {
                    let i0 = self.low(r[0]);
                    let i1 = self.low(r[1]);
                    format!("(uint(scene_instances[{i0}*{stride}u])+{i1})")
                } else {
                    self.low(r[1])
                };
                let i2 = self.low(r[2]);
                format!("scene_vertices{}[{base}*3u+{i2}].{}", if *ending { "_end" } else { "" }, xyzw(*c))
            }
            Stage::Scene_instance => {
                let i0 = self.low(r[0]);
                let i1 = self.low(r[1]);
                format!("scene_instances[{i0}*{stride}u+{i1}]")
            }
            Stage::Scene_sphere(c) => {
                let i0 = self.low(r[0]);
                format!("scene_spheres[{i0}].{}", xyzw(*c))
            }
            Stage::Scene_strand(c) => {
                let i0 = self.low(r[0]);
                let i1 = self.low(r[1]);
                format!("scene_strands[2u*{i0}+{i1}].{}", xyzw(*c))
            }
            _ => self.fail("A raster-stage intrinsic runs only in a fragment stage."),
        }
    }

    /// A compute stage's workgroup intrinsics (Raster.compute): a group scan of x is a Hillis-Steele
    /// scan in threadgroup memory between barriers, printed once a scanned register where its first
    /// field is read; a store writes a bound buffer's element and is 1.
    fn group_intrinsic(&mut self, guards: &[(i64, bool)], stage: &Stage, r: &[i64]) -> String {
        match stage {
            Stage::Group_scan(field) => {
                if self.current().id != 0 || !guards.is_empty() {
                    return self.fail("A group scan runs in uniform control flow, outside loops and branches.");
                }
                let name = format!("scan{}", r[0]);
                let size = (self.raster.as_ref().unwrap().builtin)("gpu/group-size");
                let x = self.low(r[0]);
                if !self.scans.contains(&r[0]) {
                    self.scans.push(r[0]);
                    let scope = self.current();
                    scope.decls.push_str(&format!("  uint {name}_before=0u;\n  uint {name}_total=0u;\n"));
                    scope.code.push_str(&format!(
                        "  threadgroup uint {name}[{size}];\n  {{uint v={x};{name}[lid]=v;for(uint d=1u;d<{size}u;d<<=1u){{threadgroup_barrier(mem_flags::mem_threadgroup);uint w=lid>=d?{name}[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;{name}[lid]=v;}}threadgroup_barrier(mem_flags::mem_threadgroup);{name}_before=v-{x};{name}_total={name}[{size}-1];}}\n"
                    ));
                }
                format!("{name}{}", if *field == 0 { "_before" } else { "_total" })
            }
            Stage::Store => {
                let table = self.code(r[0]).clone();
                let (u_, ty) = match unguarded(&table) {
                    Instruction::Array_prepare(u_, ty) => (*u_, ty.clone()),
                    _ => {
                        self.fail("A store names a bound buffer.");
                        return String::new();
                    }
                };
                let buffer = (self.raster.as_ref().unwrap().buffer)(u_);
                let at = self.low(r[1]);
                let value = if ty == Ty::Array(Box::new(Ty::Int)) { self.low(r[2]) } else { self.float_of(r[2]) };
                format!("({buffer}[{at}]={value},1u)")
            }
            _ => self.fail("Not a workgroup intrinsic."),
        }
    }

    fn enter(&mut self, scope_index: usize, guards: &[(i64, bool)]) {
        Scope::enter(&mut self.scopes[scope_index], &opening, guards);
    }

    /// The instruction's value expression.
    fn expression(&mut self, slot: i64, guards: &[(i64, bool)], instruction: &Instruction) -> String {
        if self.narrow(slot) {
            return match instruction {
                Instruction::Int_const(n) => format!("{}u", n & 0xffff_ffff),
                Instruction::Raise(..) if self.raster.is_some() => self.fail("Explicit errors need a compute status record."),
                Instruction::Raise(..) => "0u".to_owned(),
                Instruction::Int_binary(op, a, b) => {
                    let lb = if matches!(op, Integer::Ishl | Integer::Ishr) && !self.narrow(*b) { self.int_of(*b) } else { self.low(*b) };
                    let la = self.low(*a);
                    narrow_integer(*op, &la, &lb)
                }
                Instruction::Int_select(_, yes, no) if yes == no => self.low(*yes),
                Instruction::Int_select(condition, yes, no) => {
                    let n = self.low(*no);
                    let y = self.low(*yes);
                    let c = self.float_of(*condition);
                    format!("({c}!=0.0f?{y}:{n})")
                }
                Instruction::Loop_result(loop_, component, _) => self.reg(loop_ + 1 + component),
                Instruction::Stage(Stage::Noise_table(seed), r) => {
                    let l = self.low(r[0]);
                    format!("uint(table[{}+{l}])", self.table(*seed))
                }
                Instruction::Stage(stage, r) => {
                    if self.raster.is_some() {
                        self.group_intrinsic(guards, stage, r)
                    } else {
                        self.trace_query(guards, slot, stage, r)
                    }
                }
                Instruction::Int_frame(name) => match &self.raster {
                    Some(x) => (x.builtin)(name),
                    None => self.scene_count(name),
                },
                _ => panic!("narrow register without an integer form"),
            };
        }
        let raster = self.raster.is_some();
        match instruction {
            Instruction::Const(value) => format!("as_type<float>(0x{:x}u)", (*value as f32).to_bits()),
            Instruction::Raise(..) if raster => self.fail("Explicit errors need a compute status record."),
            Instruction::Raise(..) => (if self.integer_register(slot) { if self.int32 { "0" } else { "0L" } } else { "0.0f" }).to_owned(),
            Instruction::Input(input, _, component) if raster => (self.raster.as_ref().unwrap().input)(*input, *component),
            Instruction::Input(0, _, component) if self.unfold.is_some() => format!("uf{component}"),
            Instruction::Input(input, width, component) => {
                let value = format!("input{input}[i*{width}+{component}]");
                if self.input_uint32[u(*input)] {
                    format!("float({value})")
                } else {
                    value
                }
            }
            Instruction::Uniform(index, component) => {
                let offset = self.layout.uniforms[u(*index)] + component;
                let value = self.uniform_word(offset);
                if self.uint32_uniforms.contains(&(offset * 4)) {
                    format!("float({value})")
                } else {
                    value
                }
            }
            Instruction::Frame(name) if raster => (self.raster.as_ref().unwrap().builtin)(name),
            Instruction::Frame(_) => format!("as_type<float>(uniforms[{}])", self.layout.frames[u(slot)]),
            Instruction::Accumulator(_) => panic!("accumulator in a GPU kernel"),
            Instruction::Binary(Binary::Add | Binary::Sub | Binary::Mul | Binary::Mod | Binary::Min | Binary::Max, _, _)
                if self.fast && self.whole(slot).is_some() =>
            {
                format!("float({})", self.whole(slot).unwrap())
            }
            Instruction::Convert(Conversion::Truncate | Conversion::CoerceInt | Conversion::IntegerResult(_) | Conversion::Floor | Conversion::Ceil | Conversion::Round, a)
                if self.fast && self.whole(*a).is_some() =>
            {
                self.whole(*a).unwrap()
            }
            Instruction::Binary(op, a, b) => {
                let fb = self.float_of(*b);
                let fa = self.float_of(*a);
                let r = if self.int32 && *op == Binary::Mod { Ok(format!("rays_mod({fa},{fb})")) } else { binary(*op, &fa, &fb) };
                self.ok(r)
            }
            Instruction::Unary(op, a) => {
                let fa = self.float_of(*a);
                unary(*op, &fa)
            }
            Instruction::Fma(a, b, c) => {
                let (rc, rb, ra) = (self.reg(*c), self.reg(*b), self.reg(*a));
                format!("fma({ra},{rb},{rc})")
            }
            Instruction::Checked_mul(projection, a, b) => {
                let (rb, ra) = (self.reg(*b), self.reg(*a));
                checked(*projection, &format!("({ra}*{rb})"))
            }
            Instruction::Checked_fma(projection, a, b, c) => {
                let (rc, rb, ra) = (self.reg(*c), self.reg(*b), self.reg(*a));
                checked(*projection, &format!("fma({ra},{rb},{rc})"))
            }
            Instruction::Gather(prepared, index, width, component) => self.element(false, *prepared, *index, *width, *component),
            Instruction::Int_gather(prepared, index) => self.element(true, *prepared, *index, 1, 0),
            Instruction::Array_prepare(..) => panic!("array_prepare is not printed"),
            Instruction::Int_const(n) if self.int32 => int_literal(*n),
            Instruction::Int_const(n) => format!("long({n}L)"),
            Instruction::Int_input(input) if raster => (self.raster.as_ref().unwrap().int_input)(*input),
            Instruction::Int_input(input) => self.int_cast(format!("input{input}[i]"), !self.input_uint32[u(*input)]),
            Instruction::Int_uniform(index) => {
                let offset = self.layout.uniforms[u(*index)];
                self.int_cast(self.uniform_word(offset), !self.uint32_uniforms.contains(&(offset * 4)))
            }
            Instruction::Int_frame(name) if raster => format!("long({})", (self.raster.as_ref().unwrap().builtin)(name)),
            Instruction::Int_frame(name) if kernel_layout::scene_fact(name) => format!("{}({})", self.lt(), self.scene_count(name)),
            Instruction::Int_frame(_) => {
                let offset = self.layout.frames[u(slot)];
                self.int_cast(self.uniform_word(offset), !self.uint32_uniforms.contains(&(offset * 4)))
            }
            Instruction::Int_binary(op, a, b) => {
                let ib = self.int_of(*b);
                let ia = self.int_of(*a);
                if self.int32 { integer32(*op, &ia, &ib) } else { integer(*op, &ia, &ib) }
            }
            Instruction::Int_compare(op, a, b) => {
                let ib = self.int_of(*b);
                let ia = self.int_of(*a);
                comparison(*op, &ia, &ib)
            }
            Instruction::Int_abs(a) => {
                let ia = self.int_of(*a);
                if self.int32 { format!("abs({ia})") } else { wrap_integer(&format!("abs({ia})")) }
            }
            Instruction::Int_select(_, yes, no) if yes == no => self.int_of(*yes),
            Instruction::Int_select(condition, yes, no) => {
                let n = self.int_of(*no);
                let y = self.int_of(*yes);
                let c = self.float_of(*condition);
                format!("({c}!=0.0f?{y}:{n})")
            }
            Instruction::Convert(Conversion::ToFloat, a) if self.rounded_float(*a) => {
                let inner = self.code(*a).clone();
                match unguarded(&inner) {
                    Instruction::Convert(op, x) => {
                        let f = match op {
                            Conversion::Floor => "floor(",
                            Conversion::Ceil => "ceil(",
                            Conversion::Round => "round(",
                            _ => "trunc(",
                        };
                        format!("{f}{})", self.float_of(*x))
                    }
                    _ => unreachable!("checked by rounded_float"),
                }
            }
            Instruction::Convert(Conversion::ToFloat, a) => self.float_of(*a),
            Instruction::Convert(Conversion::Truncate | Conversion::CoerceInt | Conversion::IntegerResult(_), a) => self.int_of(*a),
            Instruction::Convert(Conversion::Floor, a) => {
                let x = format!("floor({})", self.float_of(*a));
                self.int_cast(x, true)
            }
            Instruction::Convert(Conversion::Ceil, a) => {
                let x = format!("ceil({})", self.float_of(*a));
                self.int_cast(x, true)
            }
            Instruction::Convert(Conversion::Round, a) => {
                let x = format!("round({})", self.float_of(*a));
                self.int_cast(x, true)
            }
            Instruction::Text_const(_) | Instruction::Text_input(_) | Instruction::Text_uniform(_) | Instruction::Text_gather(..) | Instruction::Text_select(..) => {
                self.fail("Text kernels stay on the CPU.")
            }
            Instruction::Select(_, yes, no) if yes == no => self.reg(*yes),
            Instruction::Select(condition, yes, no) => {
                let n = self.reg(*no);
                let y = self.reg(*yes);
                let c = self.reg(*condition);
                format!("({c}!=0.0f?{y}:{n})")
            }
            Instruction::Stage(stage @ (Stage::Trace_triangles(_) | Stage::Trace_any | Stage::Trace_curves(_) | Stage::Trace_curves_any), r) if !raster => {
                let value = self.trace_query(guards, slot, stage, r);
                if self.integer_register(slot) {
                    format!("{}({value})", self.lt())
                } else {
                    value
                }
            }
            Instruction::Stage(Stage::Noise_table(seed), r) => {
                let l = self.low(r[0]);
                format!("{}(table[{}+{l}])", self.lt(), self.table(*seed))
            }
            Instruction::Stage(stage, r) if !raster => self.scene_read(stage, r),
            Instruction::Stage(stage @ (Stage::Group_scan(_) | Stage::Store), r) => format!("long({})", self.group_intrinsic(guards, stage, r)),
            Instruction::Stage(Stage::Float_bits, r) => format!("as_type<float>({})", self.low(r[0])),
            Instruction::Stage(stage, r) => {
                let operands: Vec<String> = r.iter().map(|x| self.float_of(*x)).collect();
                stage_intrinsic(stage, &operands)
            }
            Instruction::Loop_result(loop_, component, _) => self.reg(loop_ + 1 + component),
            _ => self.fail("Integer instructions and conversions are not supported by the Metal kernel target."),
        }
    }

    /// `Convert (To_float, a)` where `a` rounds a non-integer float register.
    fn rounded_float(&self, a: i64) -> bool {
        match unguarded(self.code(a)) {
            Instruction::Convert(Conversion::Floor | Conversion::Ceil | Conversion::Round | Conversion::Truncate, x) => !self.integer_register(*x),
            _ => false,
        }
    }

    fn print(&mut self) -> Result<String, Diagnostic> {
        self.hits.clear();
        self.scans.clear();
        self.scopes = vec![Scope::create(None, Some(Arc::new(AtomicI64::new(0))), 0)];
        let layout = self.layout.clone();
        kernel_layout::iter(&layout, self);
        if let Some(d) = self.failure.take() {
            return Err(d);
        }
        self.enter(0, &[]);
        self.context = (0, vec![]);
        self.blocks = vec![];
        let mut body = String::with_capacity(4096);
        body.push_str(&declarations(&self.scopes[0].decls));
        body.push_str(&self.scopes[0].code.clone());
        let view = layout.view.clone();
        match self.raster.is_some() {
            true => {
                for (k, registers) in view.outputs.iter().enumerate() {
                    for (component, slot) in registers.iter().enumerate() {
                        let value = self.float_of(*slot);
                        let line = (self.raster.as_ref().unwrap().output)(k as i64, component as i64, &value);
                        body.push_str(&format!("  {line}\n"));
                    }
                }
            }
            false => {
                if layout.output_checks {
                    for registers in &view.outputs {
                        for slot in registers {
                            if !self.integer_register(*slot) {
                                body.push_str(&format!("  sb|=!isfinite(r{slot});\n"));
                            }
                        }
                    }
                }
                self.output_tail(&mut body);
            }
        }
        match self.failure.take() {
            Some(d) => Err(d),
            None => Ok(body),
        }
    }

    fn output_uint32(&self, k: usize) -> bool {
        self.output_flags[k]
    }

    fn output_tail(&mut self, body: &mut String) {
        let layout = self.layout.clone();
        let view = &layout.view;
        for (k, registers) in view.outputs.iter().enumerate() {
            // a 32-bit Int is out of uint32 range only when negative; a narrow register never is
            if self.output_uint32(k) && self.int32 {
                if !self.narrow(registers[0]) {
                    let value = self.int_of(registers[0]);
                    body.push_str(&format!("  sb|=({value}<0)?64u:0u;\n"));
                }
            } else if self.output_uint32(k) {
                let value = self.int_of(registers[0]);
                body.push_str(&format!("  sb|=(({value})<0L||ulong({value})>0xffffffffUL)?64u:0u;\n"));
            }
        }
        body.push_str(&format!("  {FLUSH_STATUS}\n"));
        let outputs = view.outputs.len();
        let output_name = |k: usize| if outputs == 1 { "output".to_owned() } else { format!("output{k}") };
        match self.unfold {
            None => {
                for (k, registers) in view.outputs.iter().enumerate() {
                    for (component, slot) in registers.iter().enumerate() {
                        let v = if self.output_uint32(k) { format!("uint({})", self.int_of(*slot)) } else { self.float_of(*slot) };
                        body.push_str(&format!("  {}[i*{}+{}]={};\n", output_name(k), registers.len(), component, v));
                    }
                }
            }
            Some(_) => {
                let registers = &view.outputs[0];
                for (component, slot) in registers.iter().enumerate() {
                    let v = self.float_of(*slot);
                    body.push_str(&format!("  output[(k*uf_n+i)*{}+{}]={};\n", registers.len(), component, v));
                }
                for (component, slot) in registers.iter().enumerate() {
                    let v = self.float_of(*slot);
                    body.push_str(&format!("  uf{component}={v};\n"));
                }
            }
        }
    }
}

impl Visit for Gen {
    fn instruction(&mut self, slot: usize, instruction: &Instruction) {
        if self.failure.is_some() || !self.layout.used[slot] {
            return;
        }
        let (guards, ins) = guarded(instruction);
        if matches!(ins, Instruction::Array_prepare(..)) {
            return;
        }
        let ci = self.scopes.len() - 1;
        self.context = (self.scopes[ci].id, guards.clone());
        if self.counting {
            self.defined[slot] = self.context.clone();
        }
        let inline = self.inline(slot, ins);
        let s = slot as i64;
        let (ty, zero) = if self.narrow(s) {
            ("uint", "0u")
        } else if self.integer_register(s) {
            (self.lt(), "0")
        } else {
            ("float", "0.0f")
        };
        let constant = match ins {
            Instruction::Const(_) | Instruction::Int_const(_) => !self.layout.finite_checks[slot],
            _ => false,
        };
        // a guarded constant is declared with its value: consumers fold it, and an unselected value is never read
        if !(constant || inline) {
            self.enter(ci, &guards);
        }
        self.blocks = if constant { self.scopes[ci].outer.clone() } else { Scope::stack(&self.scopes[ci]) };
        if self.counting {
            self.defined_in[slot] = self.blocks.clone();
        }
        let indentation = Scope::indent(&self.scopes[ci]);
        let expression = self.expression(s, &guards, ins);
        if self.failure.is_some() {
            return;
        }
        if inline {
            self.inlined[slot] = Some(expression);
        } else if constant {
            self.scopes[ci].decls.push_str(&format!("  {ty} r{slot}={expression};\n"));
        } else if self.scopes[ci].opened.is_empty() || (self.local[slot] && !self.named[slot] && !self.counting) {
            self.scopes[ci].code.push_str(&format!("{indentation}{ty} r{slot}={expression};\n"));
        } else {
            self.scopes[ci].decls.push_str(&format!("  {ty} r{slot}={zero};\n"));
            self.scopes[ci].code.push_str(&format!("{indentation}r{slot}={expression};\n"));
        }
        if self.layout.finite_checks[slot] && !self.integer_register(s) && self.raster.is_none() {
            self.scopes[ci].code.push_str(&format!("{indentation}sb|=!isfinite(r{slot});\n"));
        }
        match ins {
            Instruction::Raise(_, code, message, _) => {
                self.scopes[ci].code.push_str(&format!("{indentation}sb|={}u;\n", packed_ops::error_status(code, message)));
            }
            Instruction::Int_binary(operation, _, b) => {
                let text = if self.narrow(*b) {
                    let l = self.low(*b);
                    integer_status(true, &indentation, *operation, &l)
                } else {
                    let i = self.int_of(*b);
                    integer_status(false, &indentation, *operation, &i)
                };
                self.scopes[ci].code.push_str(&text);
            }
            _ => {}
        }
    }

    fn loop_(&mut self, header: usize, control: &Loop, next: &[i64], stop: Option<i64>, body: &mut dyn FnMut(&mut dyn Visit)) {
        if self.failure.is_some() || !self.layout.used[header] {
            return;
        }
        let h = header as i64;
        let oi = self.scopes.len() - 1;
        let guards = guarded(&self.layout.view.code[header]).0;
        self.context = (self.scopes[oi].id, guards.clone());
        fn ty(g: &Gen, slot: i64) -> &'static str {
            if g.narrow(slot) {
                "uint"
            } else if g.integer_register(slot) {
                g.lt()
            } else {
                "float"
            }
        }
        // a raster stage counts a capped pass from a small constant in 32 bits
        let counter = match (self.raster.is_some(), control.limit, unguarded(self.code(control.first))) {
            (true, Some(limit), Instruction::Int_const(v)) if !self.narrow(h) && v.abs() + limit < (1 << 30) => "int",
            _ => ty(self, h),
        };
        let line = format!("  {counter} r{header}=0;\n");
        self.scopes[oi].decls.push_str(&line);
        for component in 0..control.initial.len() {
            let slot = h + 1 + component as i64;
            let line = format!("  {} r{slot}=0;\n", ty(self, slot));
            self.scopes[oi].decls.push_str(&line);
        }
        self.enter(oi, &guards);
        self.blocks = Scope::stack(&self.scopes[oi]);
        let indentation = Scope::indent(&self.scopes[oi]);
        if let Some(limit) = control.limit {
            let il = self.loop_bound(control.last);
            let if_ = self.loop_bound(control.first);
            let il2 = { let x = self.loop_bound(control.last); self.wide(x) };
            let if2 = { let x = self.loop_bound(control.first); self.wide(x) };
            self.scopes[oi].code.push_str(&format!(
                "{indentation}bool over{header}={il}>{if_} && ulong({il2}-{if2})>{limit}uL;\n{indentation}sb|=over{header}?1u:0u;\n"
            ));
        }
        for (component, initial) in control.initial.iter().enumerate() {
            let slot = h + 1 + component as i64;
            let value = self.carried(slot, *initial);
            self.scopes[oi].code.push_str(&format!("{indentation}r{slot}={value};\n"));
        }
        fn bound(g: &mut Gen, h: i64, r: i64) -> String {
            if g.narrow(h) { g.low(r) } else { g.loop_bound(r) }
        }
        let last = if control.limit.is_none() {
            bound(self, h, control.last)
        } else {
            let f = bound(self, h, control.first);
            let l = bound(self, h, control.last);
            format!("(over{header}?{f}:{l})")
        };
        let first = bound(self, h, control.first);
        self.scopes[oi].code.push_str(&format!("{indentation}for(r{header}={first};r{header}<{last};r{header}++){{\n"));
        let inner = Scope::create(Some(Scope::stack(&self.scopes[oi])), Some(self.scopes[oi].next.clone()), h);
        self.scopes.push(inner);
        body(self);
        let ii = self.scopes.len() - 1;
        self.enter(ii, &[]);
        self.context = (self.scopes[ii].id, vec![]);
        self.blocks = Scope::stack(&self.scopes[ii]);
        let inner = self.scopes.pop().expect("inner scope");
        let decls = declarations(&inner.decls);
        self.scopes[oi].code.push_str(&decls);
        self.scopes[oi].code.push_str(&inner.code);
        if let Some(source) = stop {
            let f = self.float_of(source);
            self.scopes[oi].code.push_str(&format!("  bool stop{header}={f}!=0.0f;\n"));
        }
        for (component, source) in next.iter().enumerate() {
            let slot = h + 1 + component as i64;
            let value = self.carried(slot, *source);
            let t = ty(self, slot);
            self.scopes[oi].code.push_str(&format!("  {t} next{header}_{component}={value};\n"));
        }
        for component in 0..next.len() {
            self.scopes[oi].code.push_str(&format!("  r{}=next{header}_{component};\n", h + 1 + component as i64));
        }
        if stop.is_some() {
            self.scopes[oi].code.push_str(&format!("  if(stop{header})break;\n"));
        }
        self.scopes[oi].code.push_str(&format!("{indentation}}}\n"));
    }
}

impl Gen {
    /// A loop bound: in 32-bit registers an Int constant past them saturates, so `(iterate
    /// 4611686018427387903 ... :until ...)` still runs until it stops.
    fn loop_bound(&mut self, r: i64) -> String {
        match unguarded(self.code(r)) {
            Instruction::Int_const(n) if self.int32 && *n != *n as i32 as i64 => int_literal((*n).clamp(i32::MIN as i64, i32::MAX as i64)),
            _ => self.int_of(r),
        }
    }

    fn carried(&mut self, slot: i64, source: i64) -> String {
        if self.narrow(slot) {
            self.low(source)
        } else if self.integer_register(slot) {
            self.int_of(source)
        } else {
            self.reg(source)
        }
    }
}

fn generate(o: &Options, raster: Option<Raster>, program: &packed::T) -> Result<Kernel, Diagnostic> {
    if let Some(reason) = packed::gpu_refusals(true, o.fast, program).into_iter().next() {
        return Err(reason);
    }
    let fast = o.fast;
    let (program, back) = if raster.is_some() { (program.clone(), Back::identity()) } else { noise::helpers(o.instanced, o.motion, o.curves, o.spheres, program)? };
    let layout = noise::checked(
        &back,
        kernel_layout::plan(
            kernel_layout::PlanOptions { fast, image: false, instanced: o.instanced, motion: o.motion, curves: o.curves.is_some() },
            &program,
        ),
    );
    let native = o.curves == Some(Curves::Native) && layout.scene_strands.is_some();
    let view = layout.view.clone();
    if let Some(steps) = o.unfold {
        if steps <= 0
            || raster.is_some()
            || view.widths.len() != 1
            || view.outputs.len() != 1
            || view.outputs[0].len() as i64 != view.widths[0]
            || !o.uint32_inputs.is_empty()
            || !o.uint32_outputs.is_empty()
        {
            return Err(value::fail("E_GPU_UNFOLD", "An unfolded kernel reads one input and writes one output of its width."));
        }
    }
    let mut input_uint32 = vec![false; view.widths.len()];
    for i in &o.uint32_inputs {
        if !fast || *i < 0 || *i >= view.widths.len() as i64 || view.widths[u(*i)] != 1 {
            return Err(fail_d("Uint32 input requires a scalar buffer in fast precision."));
        }
        input_uint32[u(*i)] = true;
    }
    let mut gather_uint32 = vec![false; layout.gathers.len()];
    for t in &o.uint32_tables {
        let mut found = false;
        for (k, (_, (index, width))) in layout.gathers.iter().enumerate() {
            if index == t && *width == 1 { gather_uint32[k] = true; found = true; }
        }
        if !fast || !found {
            return Err(fail_d("Uint32 table requires a scalar gathered buffer in fast precision."));
        }
    }
    let mut uint32_uniforms: Vec<i64> = Vec::new();
    if o.uint32_scalars {
        if !fast {
            return Err(fail_d("Uint32 scalars require fast precision."));
        }
        for (slot, code) in view.code.iter().enumerate() {
            match unguarded(code) {
                Instruction::Int_uniform(un) => uint32_uniforms.push(layout.uniforms[u(*un)] * 4),
                Instruction::Int_frame(_) if layout.frames[slot] >= 0 => uint32_uniforms.push(layout.frames[slot] * 4),
                _ => {}
            }
        }
    }
    uint32_uniforms.sort_unstable();
    uint32_uniforms.dedup();
    let size = view.code.len();
    let mut named = vec![false; size];
    for registers in &view.outputs {
        for slot in registers {
            named[u(*slot)] = true;
        }
    }
    for (slot, instruction) in view.code.iter().enumerate() {
        if layout.used[slot] {
            for (c, _) in guarded(instruction).0 {
                named[u(c)] = true;
            }
        }
    }
    let outputs = view.outputs.len();
    let mut output_flags = vec![false; outputs];
    for index in &o.uint32_outputs {
        if *index < 0 || *index >= outputs as i64 {
            return Err(fail_d("Uint32 output index is outside its producer."));
        }
        match view.outputs[u(*index)].as_slice() {
            [slot] if fast && Region::integer_register(&view.code[u(*slot)]) => output_flags[u(*index)] = true,
            _ => return Err(fail_d("Uint32 output requires a scalar integer result in fast precision.")),
        }
    }
    let is_raster = raster.is_some();
    let mut g = Gen {
        fast,
        int32: fast && raster.is_none(),
        raster,
        unfold: o.unfold,
        instancing: layout.scene_instances.is_some(),
        moving: layout.scene_motion,
        native,
        input_uint32: input_uint32.clone(),
        gather_uint32: gather_uint32.clone(),
        output_flags: output_flags.clone(),
        uint32_uniforms: uint32_uniforms.clone(),
        references: vec![0; size],
        referenced: vec![(-1, vec![]); size],
        defined: vec![(-1, vec![]); size],
        inlined: vec![None; size],
        counting: true,
        context: (0, vec![]),
        blocks: vec![],
        defined_in: vec![vec![]; size],
        read_in: vec![vec![]; size],
        local: vec![true; size],
        named,
        hits: HashMap::new(),
        scans: vec![],
        scopes: vec![],
        failure: None,
        layout: Arc::new(layout),
    };
    g.print()?;
    g.counting = false;
    let body = g.print()?;
    let layout = g.layout.clone();
    if is_raster {
        return Ok(Kernel {
            source: format!("  uint sb=0u;\n{body}"),
            entry: String::new(),
            spirv: no_spirv(),
            interface: vec![],
            uniform_layout: vec![],
            uniform_bytes: 0,
            uint32_uniforms: vec![],
            table_seeds: vec![],
            output_widths: vec![],
            output_uint32: vec![],
            input_widths: vec![],
            input_uint32: vec![],
            gathers: vec![],
            gather_uint32: vec![],
            counts: None,
            scene: None,
        });
    }
    let count = view.widths.len() as i64;
    let scene_tables: Vec<(bool, String, i64)> = match layout.scene_binding {
        None => vec![],
        Some(b) => {
            let mut all: Vec<(bool, String, i64, &str)> = vec![
                (true, format!("{} scene", structure_type(layout.scene_instances.is_some(), layout.scene_motion)), b, ""),
                (false, "device const packed_float3* scene_vertices".to_owned(), b + 1, "scene_vertices["),
                (false, "device const float4* scene_spheres".to_owned(), b + 2, "scene_spheres["),
            ];
            if layout.scene_instances.is_some() {
                all.push((false, "device const float* scene_instances".to_owned(), b + 3, "scene_instances["));
            }
            if layout.scene_vertices_end.is_some() {
                all.push((false, "device const packed_float3* scene_vertices_end".to_owned(), b + 3, "scene_vertices_end["));
            }
            if let Some(s) = layout.scene_strands {
                all.push((false, "device const float4* scene_strands".to_owned(), s, "scene_strands["));
                all.push((true, "primitive_acceleration_structure scene_curves".to_owned(), s + 1, "scene_curves"));
            }
            all.into_iter().filter(|(_, _, _, name)| body.contains(name)).map(|(accel, declaration, binding, _)| (accel, declaration, binding)).collect()
        }
    };
    let output_name = |k: usize| if outputs == 1 { "output".to_owned() } else { format!("output{k}") };
    let mut arguments: Vec<String> = Vec::new();
    for index in 0..view.widths.len() {
        arguments.push(format!("device const {}* input{index} [[buffer({index})]]", if input_uint32[index] { "uint" } else { "float" }));
    }
    for k in 0..outputs {
        arguments.push(format!("device {}* {} [[buffer({})]]", if output_flags[k] { "uint" } else { "float" }, output_name(k), count + k as i64));
    }
    arguments.push(format!("constant uint* uniforms [[buffer({})]]", layout.uniform_binding));
    arguments.push(format!("device atomic_uint* status [[buffer({})]]", layout.status_binding));
    if let Some(binding) = layout.table_binding {
        arguments.push(format!("device const int* table [[buffer({binding})]]"));
    }
    for k in 0..layout.gathers.len() {
        arguments.push(format!("device const uint* gather{k} [[buffer({})]]", layout.gather_base + k as i64));
    }
    if layout.counts.is_some() {
        arguments.push(format!("device const uint* count_in [[buffer({})]]", layout.count_base));
        arguments.push(format!("device uint* count_out [[buffer({})]]", layout.count_base + 1));
    }
    for (_, declaration, binding) in &scene_tables {
        arguments.push(format!("{declaration} [[buffer({binding})]]"));
    }
    arguments.push("uint i [[thread_position_in_grid]]".to_owned());
    let prefix = format!(
        "#include <metal_stdlib>\nusing namespace metal;\n{}{}{}{}",
        if layout.scene_binding.is_none() { "" } else { "#include <metal_raytracing>\nusing namespace metal::raytracing;\n" },
        if o.curves == Some(Curves::Loop) && layout.scene_strands.is_some() { format!("{CURVE_CAPSULE_MSL}\n") } else { String::new() },
        if body.contains("rays_mod(") { RAYS_MOD_MSL } else { "" },
        if body.contains("rays_int(") { RAYS_INT_MSL } else { "" }
    );
    let signature = arguments.join(",\n  ");
    let guard = match layout.counts {
        None => "  if(i>=uniforms[0]) return;\n".to_owned(),
        Some(off) => {
            let w = off / 4;
            format!(
                "  uint n=uniforms[0];\n  if(uniforms[{}]!=0u) n=min(n,count_in[0]*uniforms[{w}]/uniforms[{}]);\n  if(i==0){{count_out[0]=n;count_out[1]=6u*n;count_out[2]=1u;count_out[3]=0u;count_out[4]=0u;count_out[5]=0u;}}\n  if(i>=n) return;\n",
                w + 2,
                w + 1
            )
        }
    };
    let body = match o.unfold {
        None => format!("{guard}  uint sb=0u;\n{body}"),
        Some(steps) => {
            let width = view.widths[0];
            let mut text = guard;
            text.push_str(&format!("  uint uf_n={};\n", if layout.counts.is_none() { "uniforms[0]" } else { "n" }));
            text.push_str("  uint sb=0u;\n");
            for c in 0..width {
                text.push_str(&format!("  float uf{c}=input0[i*{width}+{c}];\n"));
            }
            text.push_str(&format!("  for(uint k=0u;k<{steps}u;k++){{\n"));
            text.push_str(&body);
            text.push_str("  }\n");
            text
        }
    };
    let digest = md5_hex(format!("{prefix}{signature}{body}").as_bytes());
    let entry = format!("kernel_{digest}");
    let source = format!("{prefix}kernel void {entry}(\n  {signature}) {{\n{body}}}\n");
    let mut interface: Vec<Binding> = (0..(layout.count_base + if layout.counts.is_none() { 0 } else { 2 }))
        .filter(|binding| *binding >= count || layout.input_used[u(*binding)])
        .map(|binding| Binding {
            group: 0,
            binding,
            kind: if binding == layout.uniform_binding { BindingKind::UniformBuffer } else { BindingKind::StorageBuffer },
            visibility: vec![ShaderStage::Compute],
        })
        .collect();
    for (accel, _, binding) in &scene_tables {
        interface.push(Binding {
            group: 0,
            binding: *binding,
            kind: if *accel { BindingKind::AccelerationStructure } else { BindingKind::StorageBuffer },
            visibility: vec![ShaderStage::Compute],
        });
    }
    let uniform_layout = trace::frames(&back, &layout.uniform_layout);
    Ok(Kernel {
        source,
        entry,
        spirv: no_spirv(),
        interface,
        uniform_layout,
        uniform_bytes: layout.uniform_bytes,
        uint32_uniforms,
        scene: layout.scene_binding,
        table_seeds: layout.table_seeds.clone(),
        output_widths: view.outputs.iter().map(|o| o.len() as i64).collect(),
        output_uint32: output_flags,
        input_widths: view.widths.clone(),
        input_uint32,
        gathers: layout.gathers.iter().map(|(_, g)| *g).collect(),
        gather_uint32,
        counts: layout.counts,
    })
}
