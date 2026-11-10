//! WGSL printing preserves shared refusals, binding numbers and uniform layout. Fast-kernel visitors record their first failure; real WebGPU compilation is qualified by the browser path.
use flow_ir::core_ir::region::{self, Instruction, LiveFloat, LiveInt, Stage as RStage};
use flow_ir::kernel_layout::{self, PlanOptions, scope, Visit};
use flow_ir::packed;
use flow_syntax::packed_ops::{Binary, Comparison, Conversion, Integer, Unary};
use flow_syntax::ty::Ty;
use flow_syntax::value;
use flow_syntax::Diagnostic;
use rays_math::hash::digest;
use std::collections::HashSet;
use std::fmt::Write;
use std::sync::Arc;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind { UniformBuffer, StorageRead, StorageReadWrite }

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Binding { pub group: i64, pub binding: i64, pub kind: Kind }

/// The printed kernel. `entry` is the compute entry point, `kernel_<digest>`, workgroup size 64.
#[derive(Clone, Debug)]
pub struct Wgsl {
    pub source: String,
    pub entry: String,
    /// Bindings in group 0, ascending.
    pub interface: Vec<Binding>,
    /// `(name, byte offset, width)` exactly as the Metal emitter lays them out.
    pub uniform_layout: Vec<(String, i64, i64)>,
    /// Metal's size rounded up to a multiple of 16.
    pub uniform_bytes: i64,
    /// Noise seeds, ascending; seed k's 512-entry permutation starts at table element 512*k.
    pub table_seeds: Vec<i64>,
    pub output_widths: Vec<i64>,
    pub input_widths: Vec<i64>,
    /// The uniform whose array gather table k holds, and its element width.
    pub gathers: Vec<(i64, i64)>,
    /// The byte offset of the three count words in the uniform block.
    pub counts: Option<i64>,
}

const FINITE: &str = "fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }\n";

/// Refine the hardware division to match native's double UV grid rounded to f32. A one-ULP UV error
/// can flip a hard glyph edge. WGSL permits 2.5 ULP for division (section 15.7.4).
const IMAGE_UV: &str = "fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
";

const POWER: &str = "fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
";

const PERLIN: &str = "fn wfade(t: f32) -> f32 { return t * t * t * (t * (t * 6.0 - 15.0) + 10.0); }
fn wblend(a: f32, b: f32, t: f32) -> f32 { return a + t * (b - a); }
fn wgrad(hash: i32, x: f32, y: f32, z: f32) -> f32 {
  let h = hash & 15;
  let u = select(y, x, h < 8);
  let v = select(select(z, x, h == 12 || h == 14), y, h < 4);
  return select(-u, u, (h & 1) == 0) + select(-v, v, (h & 2) == 0);
}
fn wnoise(p: vec3<f32>, o: i32) -> f32 {
  if (!(wfinite(p.x) && wfinite(p.y) && wfinite(p.z))) {
    let nan = 0x7fc00000u;
    return bitcast<f32>(nan);
  }
  let base = floor(p);
  let q = p - base;
  let f = vec3<f32>(wfade(q.x), wfade(q.y), wfade(q.z));
  let x = i32(base.x % 256.0) & 255;
  let y = i32(base.y % 256.0) & 255;
  let z = i32(base.z % 256.0) & 255;
  let a = table[o + x] + y;
  let b = table[o + x + 1] + y;
  let aa = table[o + a] + z;
  let ab = table[o + a + 1] + z;
  let ba = table[o + b] + z;
  let bb = table[o + b + 1] + z;
  let x00 = wblend(wgrad(table[o + aa], q.x, q.y, q.z), wgrad(table[o + ba], q.x - 1.0, q.y, q.z), f.x);
  let x10 = wblend(wgrad(table[o + ab], q.x, q.y - 1.0, q.z), wgrad(table[o + bb], q.x - 1.0, q.y - 1.0, q.z), f.x);
  let x01 = wblend(wgrad(table[o + aa + 1], q.x, q.y, q.z - 1.0), wgrad(table[o + ba + 1], q.x - 1.0, q.y, q.z - 1.0), f.x);
  let x11 = wblend(wgrad(table[o + ab + 1], q.x, q.y - 1.0, q.z - 1.0), wgrad(table[o + bb + 1], q.x - 1.0, q.y - 1.0, q.z - 1.0), f.x);
  return clamp((wblend(wblend(x00, x10, f.y), wblend(x01, x11, f.y), f.z) + 1.0) * 0.5, 0.0, 1.0);
}
fn wfbm(p: vec3<f32>, octaves: i32, o: i32) -> f32 {
  var frequency: f32 = 1.0;
  var amplitude: f32 = 1.0;
  var sum: f32 = 0.0;
  var weights: f32 = 0.0;
  for (var octave: i32 = 0; octave < octaves; octave++) {
    sum += wnoise(p * frequency, o) * amplitude;
    weights += amplitude;
    frequency *= 2.0;
    amplitude *= 0.5;
  }
  return select(sum / weights, 0.0, weights == 0.0);
}
";

/// a lacunarity or gain other than 2 and 0.5 (or live): NaN where noise3 would refuse it, so the
/// status sends the region to the CPU, which reports it
const PERLIN_FRACTAL: &str = "fn wfbm_fractal(p: vec3<f32>, octaves: i32, o: i32, lacunarity: f32, gain: f32) -> f32 {
  if (!(wfinite(lacunarity) && lacunarity > 0.0 && gain >= 0.0 && gain <= 1.0)) {
    let nan = 0x7fc00000u;
    return bitcast<f32>(nan);
  }
  var frequency: f32 = 1.0;
  var amplitude: f32 = 1.0;
  var sum: f32 = 0.0;
  var weights: f32 = 0.0;
  for (var octave: i32 = 0; octave < octaves; octave++) {
    sum += wnoise(p * frequency, o) * amplitude;
    weights += amplitude;
    frequency *= lacunarity;
    amplitude *= gain;
  }
  return select(sum / weights, 0.0, weights == 0.0);
}
";

/// a live octave count, an integer register pair: NaN outside the device's 1 to 32
const PERLIN_LIVE: &str = "fn wfbm_live(p: vec3<f32>, octaves: vec2<u32>, o: i32, lacunarity: f32, gain: f32) -> f32 {
  if (octaves.y != 0u || octaves.x < 1u || octaves.x > 32u) {
    let nan = 0x7fc00000u;
    return bitcast<f32>(nan);
  }
  return wfbm_fractal(p, i32(octaves.x), o, lacunarity, gain);
}
";

fn live_noise(i: &Instruction) -> bool { matches!(i, Instruction::Noise3(_, _, _, _, LiveInt::Live(_), _, _)) }

fn fractal_noise(i: &Instruction) -> bool {
    match i {
        Instruction::Noise3(_, _, _, _, LiveInt::Live(_), _, _) => true,
        Instruction::Noise3(_, _, _, _, _, lacunarity, gain) => *lacunarity != LiveFloat::Fixed(2.0) || *gain != LiveFloat::Fixed(0.5),
        _ => false,
    }
}

fn is_pow(i: &Instruction) -> bool { matches!(i, Instruction::Binary(Binary::Pow, _, _)) }

type NoiseArgs = (i64, i64, i64, i64, LiveInt, LiveFloat, LiveFloat);

/// The call of a noise3: `register` names a register, `count` an integer register as a pair,
/// `number` prints a constant.
fn noise_call(
    register: &dyn Fn(i64) -> String,
    count: &dyn Fn(i64) -> Result<String, Diagnostic>,
    number: &dyn Fn(f64) -> String,
    table: &dyn Fn(i64) -> i64,
    (x, y, z, seed, octaves, lacunarity, gain): NoiseArgs,
) -> Result<String, Diagnostic> {
    let position = format!("vec3<f32>({}, {}, {})", register(x), register(y), register(z));
    let fractal = |f: LiveFloat| match f {
        LiveFloat::Fixed(v) => number(v),
        LiveFloat::Live(r) => register(r),
    };
    Ok(match octaves {
        LiveInt::Live(r) => format!("wfbm_live({}, {}, {}, {}, {})", position, count(r)?, table(seed), fractal(lacunarity), fractal(gain)),
        LiveInt::Fixed(octaves) => {
            if lacunarity == LiveFloat::Fixed(2.0) && gain == LiveFloat::Fixed(0.5) {
                format!("wfbm({}, {}, {})", position, octaves, table(seed))
            } else {
                format!("wfbm_fractal({}, {}, {}, {}, {})", position, octaves, table(seed), fractal(lacunarity), fractal(gain))
            }
        }
    })
}

fn fail(message: &str) -> Diagnostic { value::fail("E_GPU_FORM", message) }
fn register(index: i64) -> String { format!("r{index}") }

fn integer_status(operation: Integer, b: &str) -> String {
    match operation {
        Integer::Idiv | Integer::Imod => format!("if (all({b} == vec2<u32>(0u))) {{ sb |= 2u; }}\n"),
        Integer::Ishl | Integer::Ishr => format!("if ({b}.y != 0u || {b}.x > 63u) {{ sb |= 4u; }}\n"),
        _ => String::new(),
    }
}

/// As the Metal printer: status bits gather in `sb` and reach the shared word once; guarded
/// instructions share blocks per scope (`kernel_layout::scope`).
const FLUSH_STATUS: &str = "if (sb != 0u) { atomicOr(&status, sb); }";



fn truth(condition: &str) -> String { format!("select(0.0, 1.0, {condition})") }

fn binary(operation: Binary, a: &str, b: &str) -> Result<String, Diagnostic> {
    let template = flow_syntax::packed_ops::math_prim(&flow_syntax::packed_ops::Primitive::Numeric(operation)).wgsl.ok_or_else(|| fail("Native-safe hypot is not supported by the WGSL kernel target."))?;
    Ok(flow_syntax::packed_ops::math_source(template, a, b))
}

fn unary(operation: Unary, a: &str) -> String {
    flow_syntax::packed_ops::math_source(flow_syntax::packed_ops::math_prim(&flow_syntax::packed_ops::Primitive::Math(operation)).wgsl.expect("unary target spelling"), a, "")
}

fn float_bits(value: f64) -> String { format!("bitcast<f32>(0x{:x}u)", (value as f32).to_bits()) }

fn word(index: i64) -> String { format!("uniforms[{}].{}", index / 4, xyzw(index % 4)) }

fn xyzw(component: i64) -> &'static str { ["x", "y", "z", "w"][component as usize] }

fn kind_text(kind: Kind) -> &'static str {
    match kind {
        Kind::UniformBuffer => "<uniform>",
        Kind::StorageRead => "<storage, read>",
        Kind::StorageReadWrite => "<storage, read_write>",
    }
}

fn lo32(n: i64) -> u64 { (n as u64) & 0xffff_ffff }
fn hi32(n: i64) -> u64 { ((n as u64) >> 32) & 0xffff_ffff }

fn enter_with(condition: &dyn Fn(i64) -> String, scope: &mut scope::T, guards: &[(i64, bool)]) {
    scope::enter(scope, &|slot, selected| format!("if ({} {} 0.0) {{", condition(slot), if selected { "!=" } else { "==" }), guards);
}

/// Plain f32 kernel (the non-fast precision), or with `image` a pixel-center UV kernel.
/// The precise (f32-only) kernel: integer instructions, indexed array reads and text stay on the CPU.
/// `image` prints the one-UV-input, one-vec4-output image form.
pub fn kernel(image: bool, program: &packed::T) -> Result<Wgsl, Diagnostic> { print(false, image, None, program) }

/// Fast precision's 64-bit integer registers as `vec2<u32>` (low, high), as the Metal kernel's
/// `long`s: wrapping add, sub and mul, truncating division (`0` for a zero divisor), logical right
/// shift, shifts by `n & 63`.
pub const INT64: &str = "fn i64_add(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> { let lo = a.x + b.x; return vec2<u32>(lo, a.y + b.y + select(0u, 1u, lo < a.x)); }
fn i64_sub(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> { return vec2<u32>(a.x - b.x, a.y - b.y - select(0u, 1u, a.x < b.x)); }
fn mulhi32(a: u32, b: u32) -> u32 {
  let a0 = a & 0xffffu; let a1 = a >> 16u; let b0 = b & 0xffffu; let b1 = b >> 16u;
  let m1 = a1 * b0 + ((a0 * b0) >> 16u);
  let m2 = a0 * b1 + (m1 & 0xffffu);
  return a1 * b1 + (m1 >> 16u) + (m2 >> 16u);
}
fn i64_mul(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> { return vec2<u32>(a.x * b.x, mulhi32(a.x, b.x) + a.x * b.y + a.y * b.x); }
fn i64_neg(a: vec2<u32>) -> bool { return bitcast<i32>(a.y) < 0; }
fn i64_lt(a: vec2<u32>, b: vec2<u32>) -> bool { let x = bitcast<i32>(a.y); let y = bitcast<i32>(b.y); return x < y || (x == y && a.x < b.x); }
fn i64_ult(a: vec2<u32>, b: vec2<u32>) -> bool { return a.y < b.y || (a.y == b.y && a.x < b.x); }
fn i64_abs(a: vec2<u32>) -> vec2<u32> { return select(a, i64_sub(vec2<u32>(0u, 0u), a), i64_neg(a)); }
fn i64_to_f(a: vec2<u32>) -> f32 {
  if ((a.y == 0u && a.x < 0x80000000u) || (a.y == 0xffffffffu && a.x >= 0x80000000u)) { return f32(bitcast<i32>(a.x)); }
  let m = select(a, vec2<u32>(0u - a.x, 0u - a.y - select(0u, 1u, a.x != 0u)), bitcast<i32>(a.y) < 0);
  return select(1.0, -1.0, bitcast<i32>(a.y) < 0) * (f32(m.y) * 4294967296.0 + f32(m.x));
}
fn i64_from_f(x: f32) -> vec2<u32> {
  if (abs(x) < 2147483648.0) { let n = i32(x); return vec2<u32>(bitcast<u32>(n), select(0u, 0xffffffffu, n < 0)); }
  let h = floor(x / 4294967296.0);
  return vec2<u32>(u32(x - h * 4294967296.0), bitcast<u32>(i32(h)));
}
fn i64_shl(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> {
  let n = b.x & 63u;
  if (n == 0u) { return a; }
  if (n >= 32u) { return vec2<u32>(0u, a.x << (n - 32u)); }
  return vec2<u32>(a.x << n, (a.y << n) | (a.x >> (32u - n)));
}
fn i64_shr(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> {
  let n = b.x & 63u;
  if (n == 0u) { return a; }
  if (n >= 32u) { return vec2<u32>(a.y >> (n - 32u), 0u); }
  return vec2<u32>((a.x >> n) | (a.y << (32u - n)), a.y >> n);
}
fn i64_fmod(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> {
  let r = i64_divmod(a, b, true);
  if ((r.x != 0u || r.y != 0u) && (i64_neg(r) != i64_neg(b))) { return i64_add(r, b); }
  return r;
}
fn i64_small(a: vec2<u32>) -> bool { return (a.y == 0u && a.x < 0x80000000u) || (a.y == 0xffffffffu && a.x >= 0x80000000u); }
fn i64_divmod(a: vec2<u32>, b: vec2<u32>, rem: bool) -> vec2<u32> {
  if (b.x == 0u && b.y == 0u) { return vec2<u32>(0u, 0u); }
  if (i64_small(a) && i64_small(b) && !(a.x == 0x80000000u && b.x == 0xffffffffu)) {
    let x = bitcast<i32>(a.x); let y = bitcast<i32>(b.x);
    let q = select(x / y, x % y, rem);
    return vec2<u32>(bitcast<u32>(q), select(0u, 0xffffffffu, q < 0));
  }
  let ua = i64_abs(a); let ub = i64_abs(b);
  var q = vec2<u32>(0u, 0u); var r = vec2<u32>(0u, 0u);
  for (var k = 63; k >= 0; k--) {
    r = vec2<u32>(r.x << 1u, (r.y << 1u) | (r.x >> 31u));
    let bit = select((ua.x >> u32(k)) & 1u, (ua.y >> u32(k - 32)) & 1u, k >= 32);
    r.x = r.x | bit;
    if (!i64_ult(r, ub)) {
      r = i64_sub(r, ub);
      if (k >= 32) { q.y = q.y | (1u << u32(k - 32)); } else { q.x = q.x | (1u << u32(k)); }
    }
  }
  if (rem) { return select(r, i64_sub(vec2<u32>(0u, 0u), r), i64_neg(a)); }
  return select(q, i64_sub(vec2<u32>(0u, 0u), q), i64_neg(a) != i64_neg(b));
}
";

/// A compute stage (Flow_shader.Raster's): workgroups of `size` threads, the global index the
/// entry's element and `lid` the scan's; its buffers `(array uniform, binding, u32 words, stored)`,
/// its result discarded.
#[derive(Clone, Debug)]
pub struct Compute { pub size: i64, pub name: String, pub buffers: Vec<(i64, i64, bool, bool)> }

/// A raster stage (Flow_shader.Raster.stages): its buffers (array uniform, binding, u32 words),
/// element parameters and result leaves (name, type, flat).
#[derive(Clone, Debug)]
pub struct Stage {
    pub entry: String,
    pub program: packed::T,
    pub buffers: Vec<(i64, i64, bool)>,
    pub elements: Vec<String>,
    pub outputs: Vec<(String, Ty, bool)>,
}

/// a vertex stage (with `quads`, of a six-vertex instanced draw) or a fragment stage of the struct named
#[derive(Clone, Debug)]
enum Placement { Compute(Compute), Vertex(bool, Stage), Fragment(String, Stage) }

fn identifier(name: &str) -> String { name.replace('-', "_") }

/// The web counterpart of `Flow_shader.Emit.kernel ~fast`.
pub fn kernel_fast(program: &packed::T) -> Result<Wgsl, Diagnostic> { print(true, false, None, program) }

/// A compute stage (Flow_shader.Raster's) as the module of entry `name`.
pub fn compute(size: i64, name: &str, buffers: &[(i64, i64, bool, bool)], program: &packed::T) -> Result<String, Diagnostic> {
    print(true, false, Some(Placement::Compute(Compute { size, name: name.to_owned(), buffers: buffers.to_vec() })), program).map(|w| w.source)
}

/// The kernel instruction printer, one per kernel; `Visit` walks its layout. `fast` selects fast
/// precision (64-bit integer registers, gathers, loops, placed stages); without it the kernel is the
/// precise f32-only form, which refuses what `fast` prints.
struct Printer {
    fast: bool,
    image: bool,
    layout: Arc<kernel_layout::T>,
    placement: Option<Placement>,
    buffers: Vec<(i64, i64, bool, bool)>,
    raster: bool,
    placed: bool,
    fwidths: String,
    hoisted: HashSet<String>,
    scans: HashSet<i64>,
    workgroup: String,
    scopes: Vec<scope::T>,
    failure: Option<Diagnostic>,
}

impl Printer {
    fn integer(&self, slot: i64) -> bool { region::integer_register(&self.layout.view.code[slot as usize]) }

    fn int_of(&self, r: i64) -> String {
        if self.layout.narrow[r as usize] {
            format!("vec2<u32>({}, 0u)", register(r))
        } else if self.integer(r) {
            register(r)
        } else {
            format!("i64_from_f({})", register(r))
        }
    }

    fn float_of(&self, r: i64) -> String {
        if self.integer(r) { format!("i64_to_f({})", self.int_of(r)) } else { register(r) }
    }

    /// a register's low 32 bits as a u32 (as `Flow_shader.Emit`'s narrow registers)
    fn low(&self, r: i64) -> String {
        if self.layout.narrow[r as usize] { register(r) } else { format!("({}).x", self.int_of(r)) }
    }

    fn narrow_integer(&self, operation: Integer, a: &str, b: i64) -> String {
        let shift = |operator: &str| {
            let k = format!("({})", self.int_of(b));
            format!("select({a} {operator} ({k}.x & 31u), 0u, ({k}.x & 63u) > 31u)")
        };
        match operation {
            Integer::Iadd => format!("({a} + {})", self.low(b)),
            Integer::Isub => format!("({a} - {})", self.low(b)),
            Integer::Imul => format!("({a} * {})", self.low(b)),
            Integer::Iand => format!("({a} & {})", self.low(b)),
            Integer::Ior => format!("({a} | {})", self.low(b)),
            Integer::Ixor => format!("({a} ^ {})", self.low(b)),
            Integer::Ishl => shift("<<"),
            Integer::Ishr => shift(">>"),
            Integer::Idiv => format!("select({a} / {}, 0u, {} == 0u)", self.low(b), self.low(b)),
            Integer::Imod => format!("select({a} % {}, 0u, {} == 0u)", self.low(b), self.low(b)),
        }
    }

    /// an integer-valued register as a 64-bit expression (as `Flow_shader.Emit`'s)
    fn whole(&self, r: i64) -> Option<String> {
        if !self.fast {
            return None;
        }
        if self.integer(r) {
            return Some(self.int_of(r));
        }
        match Instruction::unguarded(&self.layout.view.code[r as usize]) {
            Instruction::Convert(Conversion::ToFloat, x) if self.integer(*x) => Some(self.int_of(*x)),
            Instruction::Const(c) if c.is_finite() && c.fract() == 0.0 && c.abs() < 9007199254740992.0 => {
                let n = *c as i64;
                Some(format!("vec2<u32>({}u, {}u)", lo32(n), hi32(n)))
            }
            Instruction::Binary(op @ (Binary::Add | Binary::Sub | Binary::Mul | Binary::Mod | Binary::Min | Binary::Max), a, b) => match (self.whole(*a), self.whole(*b)) {
                (Some(a), Some(b)) => Some(match op {
                    Binary::Add => format!("i64_add({a}, {b})"),
                    Binary::Sub => format!("i64_sub({a}, {b})"),
                    Binary::Mul => format!("i64_mul({a}, {b})"),
                    Binary::Mod => format!("i64_fmod({a}, {b})"),
                    Binary::Min => format!("select({b}, {a}, i64_lt({a}, {b}))"),
                    _ => format!("select({a}, {b}, i64_lt({a}, {b}))"),
                }),
                _ => None,
            },
            _ => None,
        }
    }

    fn gather_index(&self, slot: i64) -> Result<usize, Diagnostic> {
        self.layout.gathers.iter().position(|(s, _)| *s == slot).ok_or_else(|| fail("Gather without its table."))
    }

    fn element(&self, prepared: i64, index: i64, width: i64, component: i64) -> Result<String, Diagnostic> {
        let k = self.gather_index(prepared)?;
        Ok(format!(
            "gather{k}[u32(clamp(i32({}), 0, max(i32({}) - 1, 0))) * {width}u + {component}u]",
            if self.layout.narrow[index as usize] { format!("bitcast<i32>({})", register(index)) } else { self.float_of(index) },
            word(self.layout.gather_lengths[k]),
        ))
    }

    /// a placed stage reads its bound buffers unclamped, as the Metal raster placement; a float
    /// from a binding of words is their bits
    fn read(&self, prepared: i64, index: i64, width: i64, component: i64) -> Result<String, Diagnostic> {
        match Instruction::unguarded(&self.layout.view.code[prepared as usize]) {
            Instruction::ArrayPrepare(u, ty) => {
                let binding = match self.buffers.iter().find(|(v, _, _, _)| v == u) {
                    Some((_, b, _, _)) => *b,
                    None => return Err(fail("A placed stage reads an unbound table.")),
                };
                let value = format!("buffer{binding}[{} * {width}u + {component}u]", self.low(index));
                if *ty != Ty::Array(Box::new(Ty::Int)) && self.buffers.iter().any(|(_, b, w, _)| *b == binding && *w) {
                    Ok(format!("bitcast<f32>({value})"))
                } else {
                    Ok(value)
                }
            }
            _ => Err(fail("Gather without its table.")),
        }
    }

    /// a fragment's varying, and its fwidth: read once at the top, before any branch (WGSL's
    /// derivative uniformity)
    fn varying(&self, input: i64, width: i64, component: i64) -> Result<String, Diagnostic> {
        match &self.placement {
            Some(Placement::Fragment(_, s)) => Ok(format!(
                "v.{}{}",
                identifier(&s.elements[input as usize]),
                if width == 1 { String::new() } else { format!(".{}", xyzw(component)) }
            )),
            _ => Err(fail("Only a fragment stage reads varyings.")),
        }
    }

    fn intrinsic(&mut self, stage: &RStage, r: &[i64]) -> Result<String, Diagnostic> {
        let source = Instruction::unguarded(&self.layout.view.code[r[0] as usize]).clone();
        match (stage, &source) {
            (RStage::Fwidth, Instruction::Input(input, width, component)) => {
                let name = format!("fw{input}_{component}");
                if !self.hoisted.contains(&name) {
                    self.hoisted.insert(name.clone());
                    let v = self.varying(*input, *width, *component)?;
                    let _ = writeln!(self.fwidths, "  let {name} = fwidth({v});");
                }
                Ok(name)
            }
            (RStage::Fwidth, _) => Err(fail("On the web fwidth reads a varying (WGSL's derivative uniformity).")),
            (RStage::FloatBits, _) => Ok(format!("bitcast<f32>({})", self.low(r[0]))),
            (RStage::Sample(t, c), _) => Ok(format!(
                "textureSample(texture{t}, sampler{}, vec2<f32>({}, {})).{}",
                t + 1,
                self.float_of(r[0]),
                self.float_of(r[1]),
                xyzw(*c)
            )),
            _ => Err(fail("This raster-stage intrinsic has no WGSL form.")),
        }
    }

    /// A compute stage's group scan (Hillis-Steele in workgroup memory between barriers, once a
    /// scanned register, in uniform control flow) and store (a statement before its register's 1),
    /// as the Metal printer's.
    fn group(&mut self, current: &mut scope::T, guards_empty: bool, indentation: &str, stage: &RStage, r: &[i64]) -> Result<String, Diagnostic> {
        let compute = match &self.placement {
            Some(Placement::Compute(c)) => c.clone(),
            _ => return Err(fail("A workgroup intrinsic runs only in a compute stage.")),
        };
        match stage {
            RStage::GroupScan(field) => {
                if current.id != 0 || !guards_empty {
                    return Err(fail("A group scan runs in uniform control flow, outside loops and branches."));
                }
                let name = format!("scan{}", r[0]);
                let x = self.low(r[0]);
                let size = compute.size;
                if !self.scans.contains(&r[0]) {
                    self.scans.insert(r[0]);
                    let _ = writeln!(self.workgroup, "var<workgroup> {name}: array<u32, {size}>;");
                    let _ = write!(current.decls, "  var {name}_before: u32 = 0u;\n  var {name}_total: u32 = 0u;\n");
                    let _ = writeln!(
                        current.code,
                        "  {{ var v: u32 = {x}; {name}[lid] = v; for (var d = 1u; d < {size}u; d = d << 1u) {{ workgroupBarrier(); var w = 0u; if (lid >= d) {{ w = {name}[lid - d]; }} workgroupBarrier(); v = v + w; {name}[lid] = v; }} workgroupBarrier(); {name}_before = v - {x}; {name}_total = {name}[{}u]; }}",
                        size - 1
                    );
                }
                Ok(format!("{name}{}", if *field == 0 { "_before" } else { "_total" }))
            }
            RStage::Store => {
                let (u, ty) = match Instruction::unguarded(&self.layout.view.code[r[0] as usize]) {
                    Instruction::ArrayPrepare(u, ty) => (*u, ty.clone()),
                    _ => return Err(fail("A store names a bound buffer.")),
                };
                let buffer = match compute.buffers.iter().find(|(v, _, _, _)| *v == u) {
                    Some((_, binding, _, _)) => format!("buffer{binding}"),
                    None => return Err(fail("A compute stage reads an unbound table.")),
                };
                let stored = if ty == Ty::Array(Box::new(Ty::Int)) { self.low(r[2]) } else { self.float_of(r[2]) };
                let _ = writeln!(current.code, "{indentation}{buffer}[{}] = {stored};", self.low(r[1]));
                Ok("1u".to_owned())
            }
            _ => Err(fail("A raster-stage intrinsic runs only in a fragment stage.")),
        }
    }

    fn enter(&self, scope: &mut scope::T, guards: &[(i64, bool)]) {
        enter_with(&|r| self.float_of(r), scope, guards);
    }

    fn carried(&self, slot: i64, source: i64) -> String {
        if self.layout.narrow[slot as usize] {
            self.low(source)
        } else if self.integer(slot) {
            self.int_of(source)
        } else {
            register(source)
        }
    }

    fn type_of(&self, slot: i64) -> &'static str {
        if self.layout.narrow[slot as usize] { "u32" } else if self.integer(slot) { "vec2<u32>" } else { "f32" }
    }

    fn zero_of(&self, slot: i64) -> &'static str {
        if self.layout.narrow[slot as usize] { "0u" } else if self.integer(slot) { "vec2<u32>(0u, 0u)" } else { "0.0" }
    }

    /// What the precise kernel refuses at the instruction (the fast printer prints all of it).
    fn precise_refusal(&self, instruction: &Instruction) -> Result<(), Diagnostic> {
        match instruction {
            Instruction::Const(_) | Instruction::Input(..) | Instruction::Uniform(..) | Instruction::Frame(_) | Instruction::Accumulator(_)
            | Instruction::Binary(..) | Instruction::Unary(..) | Instruction::Select(..) => Ok(()),
            Instruction::Noise3(_, _, _, _, LiveInt::Live(_), _, _) => Err(fail("Integer instructions are not supported by the WGSL kernel.")),
            Instruction::Noise3(..) => Ok(()),
            Instruction::CheckedMul(..) | Instruction::CheckedFma(..) => Err(fail("Checked arithmetic requires binary64 overflow status.")),
            Instruction::Trace(..) => Err(fail("scene/trace needs a native ray-query host.")),
            Instruction::Stage(..) => Err(fail("A raster-stage intrinsic runs only in a fragment stage.")),
            Instruction::ArrayPrepare(..) | Instruction::Gather(..) | Instruction::IntGather(..) => Err(fail("Indexed array reads stay on the CPU.")),
            Instruction::TextConst(_) | Instruction::TextInput(_) | Instruction::TextUniform(_) | Instruction::TextGather(..) | Instruction::TextSelect(..) => {
                Err(fail("Text kernels stay on the CPU."))
            }
            _ => Err(fail("Integer instructions and conversions are not supported by the WGSL kernel target.")),
        }
    }

    fn print_instruction(&mut self, current: &mut scope::T, slot: usize, instruction: &Instruction) -> Result<(), Diagnostic> {
        let layout = self.layout.clone();
        let s = slot as i64;
        if !layout.used[slot] || (self.fast && matches!(Instruction::unguarded(instruction), Instruction::ArrayPrepare(..))) {
            return Ok(());
        }
        let (guards, instruction) = Instruction::guarded(instruction);
        if !self.fast {
            self.precise_refusal(instruction)?;
        }
        let constant = match instruction {
            Instruction::Const(_) | Instruction::IntConst(_) => self.fast && !layout.finite_checks[slot],
            _ => false,
        };
        if !constant {
            self.enter(current, &guards);
        }
        let indentation = scope::indent(current);
        let compute = matches!(self.placement, Some(Placement::Compute(_)));
        let (raster, placed) = (self.raster, self.placed);
        let narrow = layout.narrow[slot];
        let expression = if narrow {
            match instruction {
                Instruction::IntConst(n) => format!("{}u", n & 0xffff_ffff),
                Instruction::Raise(..) if raster => return Err(fail("Explicit errors need a compute status record.")),
                Instruction::Raise(..) => "0u".to_owned(),
                Instruction::IntBinary(op, a, b) => self.narrow_integer(*op, &self.low(*a), *b),
                Instruction::IntSelect(condition, yes, no) => format!("select({}, {}, {} != 0.0)", self.low(*no), self.low(*yes), self.float_of(*condition)),
                Instruction::LoopResult(loop_, component, _) => register(loop_ + 1 + component),
                Instruction::Stage(stage, r) if compute => self.group(current, guards.is_empty(), &indentation, stage, r)?,
                _ => unreachable!("narrow register of an unexpected instruction"),
            }
        } else {
            match instruction {
                Instruction::Const(value) => float_bits(*value),
                Instruction::Raise(..) if raster => return Err(fail("Explicit errors need a compute status record.")),
                Instruction::Raise(..) => (if self.integer(s) { "vec2<u32>(0u, 0u)" } else { "0.0" }).to_owned(),
                Instruction::IntInput(input) => match &self.placement {
                    Some(Placement::Compute(_)) => "vec2<u32>(gid.x, 0u)".to_owned(),
                    Some(Placement::Vertex(true, _)) => "vec2<u32>(iid * 4u + quad_corner[vid], 0u)".to_owned(),
                    Some(Placement::Vertex(false, _)) => "vec2<u32>(vid, 0u)".to_owned(),
                    Some(Placement::Fragment(..)) => "vec2<u32>(0u, 0u)".to_owned(),
                    None => format!("i64_from_f(input{input}[i])"),
                },
                Instruction::Stage(stage, r) if compute => format!("vec2<u32>({}, 0u)", self.group(current, guards.is_empty(), &indentation, stage, r)?),
                Instruction::Stage(stage, r) if raster => self.intrinsic(stage, r)?,
                Instruction::Input(input, width, component) if raster => self.varying(*input, *width, *component)?,
                Instruction::Input(_, _, component) if self.image => format!(
                    "pixel_uv({}, bitcast<f32>({}))",
                    if *component == 0 { "i % image_width" } else { "i / image_width" },
                    word(if *component == 0 { layout.image_width.unwrap_or(0) } else { layout.image_height.unwrap_or(0) })
                ),
                Instruction::Gather(prepared, index, width, component) if placed => self.read(*prepared, *index, *width, *component)?,
                Instruction::IntGather(prepared, index) if placed => format!("vec2<u32>({}, 0u)", self.read(*prepared, *index, 1, 0)?),
                Instruction::Input(input, width, component) => format!("input{input}[i * {width}u + {component}u]"),
                Instruction::Uniform(index, component) => format!("bitcast<f32>({})", word(layout.uniforms[*index as usize] + component)),
                Instruction::Frame(_) => format!("bitcast<f32>({})", word(layout.frames[slot])),
                Instruction::Accumulator(_) => unreachable!("accumulator in a collecting kernel"),
                // arithmetic on two integers stays in 64 bits: a float register would round them first
                Instruction::Binary(Binary::Add | Binary::Sub | Binary::Mul | Binary::Mod | Binary::Min | Binary::Max, _, _) if self.whole(s).is_some() => {
                    format!("i64_to_f({})", self.whole(s).unwrap())
                }
                Instruction::Convert(Conversion::Truncate | Conversion::CoerceInt | Conversion::IntegerResult(_) | Conversion::Floor | Conversion::Ceil | Conversion::Round, a)
                    if self.whole(*a).is_some() =>
                {
                    self.whole(*a).unwrap()
                }
                Instruction::Binary(op, a, b) => binary(*op, &self.float_of(*a), &self.float_of(*b))?,
                Instruction::Unary(op, a) => unary(*op, &self.float_of(*a)),
                Instruction::Fma(a, b, c) => format!("fma({}, {}, {})", register(*a), register(*b), register(*c)),
                Instruction::CheckedMul(..) | Instruction::CheckedFma(..) => return Err(fail("Checked arithmetic requires binary64 overflow status.")),
                Instruction::Trace(..) => return Err(fail("scene/trace needs a native ray-query host.")),
                Instruction::Stage(..) => return Err(fail("A raster-stage intrinsic runs only in a fragment stage.")),
                Instruction::Gather(prepared, index, width, component) => self.element(*prepared, *index, *width, *component)?,
                Instruction::IntGather(prepared, index) => format!("i64_from_f({})", self.element(*prepared, *index, 1, 0)?),
                Instruction::ArrayPrepare(..) => unreachable!("array_prepare is not printed"),
                Instruction::IntConst(n) => format!("vec2<u32>({}u, {}u)", lo32(*n), hi32(*n)),
                Instruction::IntUniform(index) => format!("i64_from_f(bitcast<f32>({}))", word(layout.uniforms[*index as usize])),
                Instruction::IntFrame(_) => format!("i64_from_f(bitcast<f32>({}))", word(layout.frames[slot])),
                Instruction::IntBinary(op, a, b) => {
                    let (a, b) = (self.int_of(*a), self.int_of(*b));
                    match op {
                        Integer::Iadd => format!("i64_add({a}, {b})"),
                        Integer::Isub => format!("i64_sub({a}, {b})"),
                        Integer::Imul => format!("i64_mul({a}, {b})"),
                        Integer::Idiv => format!("i64_divmod({a}, {b}, false)"),
                        Integer::Imod => format!("i64_divmod({a}, {b}, true)"),
                        Integer::Iand => format!("({a} & {b})"),
                        Integer::Ior => format!("({a} | {b})"),
                        Integer::Ixor => format!("({a} ^ {b})"),
                        Integer::Ishl => format!("i64_shl({a}, {b})"),
                        Integer::Ishr => format!("i64_shr({a}, {b})"),
                    }
                }
                Instruction::IntCompare(op, a, b) => {
                    let (a, b) = (self.int_of(*a), self.int_of(*b));
                    truth(&match op {
                        Comparison::Ilt => format!("i64_lt({a}, {b})"),
                        Comparison::Igt => format!("i64_lt({b}, {a})"),
                        Comparison::Ile => format!("!i64_lt({b}, {a})"),
                        Comparison::Ige => format!("!i64_lt({a}, {b})"),
                        Comparison::Ieq => format!("all({a} == {b})"),
                    })
                }
                Instruction::IntAbs(a) => format!("i64_abs({})", self.int_of(*a)),
                Instruction::IntSelect(condition, yes, no) => format!("select({}, {}, {} != 0.0)", self.int_of(*no), self.int_of(*yes), self.float_of(*condition)),
                Instruction::Convert(Conversion::ToFloat, a) => self.float_of(*a),
                Instruction::Convert(Conversion::Truncate | Conversion::CoerceInt | Conversion::IntegerResult(_), a) => self.int_of(*a),
                Instruction::Convert(Conversion::Floor, a) => format!("i64_from_f(floor({}))", self.float_of(*a)),
                Instruction::Convert(Conversion::Ceil, a) => format!("i64_from_f(ceil({}))", self.float_of(*a)),
                Instruction::Convert(Conversion::Round, a) => {
                    let f = self.float_of(*a);
                    format!("i64_from_f(sign({f}) * floor(abs({f}) + 0.5))")
                }
                Instruction::TextConst(_) | Instruction::TextInput(_) | Instruction::TextUniform(_) | Instruction::TextGather(..) | Instruction::TextSelect(..) => {
                    return Err(fail("Text kernels stay on the CPU."))
                }
                Instruction::Select(condition, yes, no) => format!("select({}, {}, {} != 0.0)", register(*no), register(*yes), register(*condition)),
                Instruction::Noise3(x, y, z, seed, octaves, lacunarity, gain) => {
                    let table = |seed: i64| match layout.table_seeds.iter().position(|v| *v == seed) {
                        Some(index) => index as i64 * 512,
                        None => unreachable!("noise seed without its table"),
                    };
                    noise_call(&|r| register(r), &|r| Ok(self.int_of(r)), &float_bits, &table, (*x, *y, *z, *seed, *octaves, *lacunarity, *gain))?
                }
                Instruction::LoopResult(loop_, component, _) => register(loop_ + 1 + component),
                _ => return Err(fail("This instruction is not supported by the WGSL fast kernel target.")),
            }
        };
        let (ty, zero) = if narrow { ("u32", "0u") } else if self.integer(s) { ("vec2<u32>", "vec2<u32>(0u, 0u)") } else { ("f32", "0.0") };
        if constant {
            let _ = writeln!(current.decls, "  let r{slot}: {ty} = {expression};");
        } else if current.opened.is_empty() {
            let _ = writeln!(current.code, "{indentation}let r{slot}: {ty} = {expression};");
        } else {
            let _ = writeln!(current.decls, "  var r{slot}: {ty} = {zero};");
            let _ = writeln!(current.code, "{indentation}r{slot} = {expression};");
        }
        if layout.finite_checks[slot] && !self.integer(s) && !raster {
            let _ = writeln!(current.code, "{indentation}if (!wfinite(r{slot})) {{ sb |= 1u; }}");
        }
        match instruction {
            Instruction::Raise(_, code, message, _) => {
                let _ = writeln!(current.code, "{indentation}sb |= {}u;", flow_syntax::packed_ops::error_status(code, message));
            }
            Instruction::IntBinary(operation, _, b) => {
                current.code.push_str(&indentation);
                current.code.push_str(&integer_status(*operation, &self.int_of(*b)));
            }
            _ => {}
        }
        Ok(())
    }

    fn print_loop(&mut self, header: usize, control: &region::Loop, next: &[i64], stop: Option<i64>, body: &mut dyn FnMut(&mut dyn Visit)) -> Result<(), Diagnostic> {
        if !self.layout.used[header] {
            return Ok(());
        }
        let (guards, _) = Instruction::guarded(&self.layout.view.code[header].clone());
        let mut outer = self.scopes.pop().expect("a scope");
        let result = self.print_loop_in(&mut outer, header, control, next, stop, body, &guards);
        self.scopes.push(outer);
        result
    }

    #[allow(clippy::too_many_arguments)]
    fn print_loop_in(
        &mut self,
        outer: &mut scope::T,
        header: usize,
        control: &region::Loop,
        next: &[i64],
        stop: Option<i64>,
        body: &mut dyn FnMut(&mut dyn Visit),
        guards: &[(i64, bool)],
    ) -> Result<(), Diagnostic> {
        let h = header as i64;
        let _ = writeln!(outer.decls, "  var r{h}: {} = {};", self.type_of(h), self.zero_of(h));
        for component in 0..control.initial.len() as i64 {
            let slot = h + 1 + component;
            let _ = writeln!(outer.decls, "  var r{slot}: {} = {};", self.type_of(slot), self.zero_of(slot));
        }
        self.enter(outer, guards);
        let indentation = scope::indent(outer);
        // a pass count over the limit flags the status and runs no pass (as the Metal printer)
        if let Some(limit) = control.limit {
            let _ = write!(
                outer.code,
                "{indentation}let over{h} = i64_lt({}, {}) && i64_lt(vec2<u32>({}u, {}u), i64_sub({}, {}));\n{indentation}if (over{h}) {{ sb |= 1u; }}\n",
                self.int_of(control.first),
                self.int_of(control.last),
                lo32(limit),
                (limit as u64) >> 32,
                self.int_of(control.last),
                self.int_of(control.first)
            );
        }
        for (component, initial) in control.initial.iter().enumerate() {
            let slot = h + 1 + component as i64;
            let _ = writeln!(outer.code, "{indentation}r{slot} = {};", self.carried(slot, *initial));
        }
        let last = |bound: &dyn Fn(i64) -> String| {
            if control.limit.is_none() {
                bound(control.last)
            } else {
                format!("select({}, {}, over{h})", bound(control.last), bound(control.first))
            }
        };
        if self.layout.narrow[header] {
            let _ = writeln!(
                outer.code,
                "{indentation}for (r{h} = {}; r{h} < {}; r{h} = r{h} + 1u) {{",
                self.low(control.first),
                last(&|r| self.low(r))
            );
        } else {
            let _ = writeln!(
                outer.code,
                "{indentation}for (r{h} = {}; i64_lt(r{h}, {}); r{h} = i64_add(r{h}, vec2<u32>(1u, 0u))) {{",
                self.int_of(control.first),
                last(&|r| self.int_of(r))
            );
        }
        self.scopes.push(scope::create(None, None, h));
        body(self);
        let mut inner = self.scopes.pop().expect("loop scope");
        self.enter(&mut inner, &[]);
        outer.code.push_str(&inner.decls);
        outer.code.push_str(&inner.code);
        if let Some(source) = stop {
            let _ = writeln!(outer.code, "  let stop{h} = {} != 0.0;", self.float_of(source));
        }
        for (component, source) in next.iter().enumerate() {
            let slot = h + 1 + component as i64;
            let _ = writeln!(outer.code, "  let next{h}_{component}: {} = {};", self.type_of(slot), self.carried(slot, *source));
        }
        for component in 0..next.len() {
            let _ = writeln!(outer.code, "  r{} = next{h}_{component};", h + 1 + component as i64);
        }
        if stop.is_some() {
            let _ = writeln!(outer.code, "  if (stop{h}) {{ break; }}");
        }
        let _ = writeln!(outer.code, "{indentation}}}");
        Ok(())
    }
}

impl Visit for Printer {
    fn instruction(&mut self, slot: usize, instruction: &Instruction) {
        if self.failure.is_some() {
            return;
        }
        let mut current = self.scopes.pop().expect("a scope");
        let result = self.print_instruction(&mut current, slot, instruction);
        self.scopes.push(current);
        if let Err(d) = result {
            self.failure = Some(d);
        }
    }

    fn loop_(&mut self, header: usize, control: &region::Loop, next: &[i64], stop: Option<i64>, body: &mut dyn FnMut(&mut dyn Visit)) {
        if self.failure.is_some() {
            return;
        }
        if let Err(d) = self.print_loop(header, control, next, stop, body) {
            self.failure = Some(d);
        }
    }
}

/// One kernel in the precision `fast` selects; `image` (precise only) is the UV-to-vec4 image form.
fn print(fast: bool, image: bool, placement: Option<Placement>, program: &packed::T) -> Result<Wgsl, Diagnostic> {
    if let Some(reason) = packed::gpu_refusals(fast, fast, program).into_iter().next() {
        return Err(reason);
    }
    let layout = kernel_layout::plan(PlanOptions { fast, image, ..Default::default() }, program);
    if image && (layout.view.widths != [2] || layout.view.outputs.iter().map(|o| o.len()).collect::<Vec<_>>() != [4]) {
        return Err(fail("An image kernel needs one UV input and one vec4 output."));
    }
    let buffers = match &placement {
        Some(Placement::Compute(c)) => c.buffers.clone(),
        Some(Placement::Vertex(_, s) | Placement::Fragment(_, s)) => s.buffers.iter().map(|(u, b, w)| (*u, *b, *w, false)).collect(),
        None => Vec::new(),
    };
    let compute = matches!(placement, Some(Placement::Compute(_)));
    let placed = placement.is_some();
    let raster = placed && !compute;
    let layout = Arc::new(layout);
    let mut printer = Printer {
        fast,
        image,
        layout: layout.clone(),
        placement: placement.clone(),
        buffers,
        raster,
        placed,
        fwidths: String::with_capacity(64),
        hoisted: HashSet::new(),
        scans: HashSet::new(),
        workgroup: String::with_capacity(64),
        scopes: vec![scope::create(None, None, 0)],
        failure: None,
    };
    kernel_layout::iter(&layout, &mut printer);
    if let Some(d) = printer.failure.take() {
        return Err(d);
    }
    let mut root = printer.scopes.pop().expect("root scope");
    printer.enter(&mut root, &[]);
    let view = &layout.view;
    let outputs = layout.output_count;
    let mut body = String::with_capacity(4096);
    body.push_str(&root.decls);
    body.push_str(&root.code);
    let has = |operation: &dyn Fn(&Instruction) -> bool| view.code.iter().enumerate().any(|(slot, i)| layout.used[slot] && operation(Instruction::unguarded(i)));
    let empty = |source: String, entry: String| Wgsl {
        source,
        entry,
        interface: Vec::new(),
        uniform_layout: Vec::new(),
        uniform_bytes: 0,
        table_seeds: Vec::new(),
        output_widths: Vec::new(),
        input_widths: Vec::new(),
        gathers: Vec::new(),
        counts: None,
    };
    match &placement {
        Some(Placement::Vertex(_, s) | Placement::Fragment(_, s)) => {
            let mut stores = String::with_capacity(256);
            for (k, registers) in view.outputs.iter().enumerate() {
                let (name, _, _) = &s.outputs[k];
                for (component, slot) in registers.iter().enumerate() {
                    let c = xyzw(component as i64);
                    if name == "discard" {
                        let _ = writeln!(stores, "  if ({} != 0.0) {{ discard; }}", printer.float_of(*slot));
                    } else if matches!(placement, Some(Placement::Fragment(..))) {
                        let _ = writeln!(stores, "  colour.{c} = {};", printer.float_of(*slot));
                    } else {
                        let _ = writeln!(
                            stores,
                            "  o.{}{} = {};",
                            identifier(name),
                            if registers.len() == 1 { String::new() } else { format!(".{c}") },
                            printer.float_of(*slot)
                        );
                    }
                }
            }
            let entry = identifier(&s.entry);
            let source = match &placement {
                Some(Placement::Fragment(out, _)) => format!(
                    "@fragment\nfn {entry}(v: {out}) -> @location(0) vec4<f32> {{\n{}  var sb: u32 = 0u;\n  var colour: vec4<f32>;\n{body}{stores}  return colour;\n}}\n",
                    printer.fwidths
                ),
                _ => format!(
                    "@vertex\nfn {entry}(@builtin(vertex_index) vid: u32, @builtin(instance_index) iid: u32) -> {entry}_out {{\n  var sb: u32 = 0u;\n  var o: {entry}_out;\n{body}{stores}  return o;\n}}\n"
                ),
            };
            Ok(empty(source, entry))
        }
        Some(Placement::Compute(Compute { size, name, buffers })) => {
            let mut declarations = String::new();
            for (_, binding, words, stored) in buffers {
                let _ = writeln!(
                    declarations,
                    "@group(0) @binding({binding}) var<storage, {}> buffer{binding}: array<{}>;",
                    if *stored { "read_write" } else { "read" },
                    if *words { "u32" } else { "f32" }
                );
            }
            let source = format!(
                "{declarations}{}{INT64}@compute @workgroup_size({size})\nfn {name}(@builtin(local_invocation_index) lid: u32, @builtin(global_invocation_id) gid: vec3<u32>) {{\n  var sb: u32 = 0u;\n{body}}}\n",
                printer.workgroup
            );
            Ok(empty(source, name.clone()))
        }
        None => {
            if fast && layout.output_checks {
                for registers in &view.outputs {
                    for slot in registers {
                        if !printer.integer(*slot) {
                            let _ = writeln!(body, "  if (!wfinite(r{slot})) {{ sb |= 1u; }}");
                        }
                    }
                }
            }
            let _ = writeln!(body, "  {FLUSH_STATUS}");
            let output_name = |k: i64| if outputs == 1 { "outbuf".to_owned() } else { format!("outbuf{k}") };
            for (k, registers) in view.outputs.iter().enumerate() {
                for (component, slot) in registers.iter().enumerate() {
                    let _ = writeln!(body, "  {}[i * {}u + {}u] = {};", output_name(k as i64), registers.len(), component, printer.float_of(*slot));
                }
            }
            let uniform_words = kernel_layout::uniform_words(&layout);
            let mut declarations = String::with_capacity(512);
            let mut interface: Vec<Binding> = Vec::new();
            let mut bind = |binding: i64, kind: Kind, decl: String| {
                interface.push(Binding { group: 0, binding, kind });
                let _ = writeln!(declarations, "@group(0) @binding({binding}) var{} {decl};", kind_text(kind));
            };
            for (index, used) in layout.input_used.iter().enumerate() {
                if *used {
                    bind(index as i64, Kind::StorageRead, format!("input{index}: array<f32>"));
                }
            }
            for k in 0..outputs {
                bind(layout.input_count + k, Kind::StorageReadWrite, format!("{}: array<f32>", output_name(k)));
            }
            bind(layout.uniform_binding, Kind::UniformBuffer, format!("uniforms: array<vec4<u32>, {uniform_words}>"));
            bind(layout.status_binding, Kind::StorageReadWrite, "status: atomic<u32>".to_owned());
            if let Some(binding) = layout.table_binding {
                bind(binding, Kind::StorageRead, "table: array<i32>".to_owned());
            }
            if fast {
                for k in 0..layout.gathers.len() as i64 {
                    bind(layout.gather_base + k, Kind::StorageRead, format!("gather{k}: array<f32>"));
                }
                bind(layout.count_base, Kind::StorageRead, "count_in: array<u32>".to_owned());
                bind(layout.count_base + 1, Kind::StorageReadWrite, "count_out: array<u32>".to_owned());
            }
            let mut prefix = declarations;
            prefix.push_str(FINITE);
            if image {
                prefix.push_str(IMAGE_UV);
            }
            if fast {
                prefix.push_str(INT64);
            }
            if has(&is_pow) {
                prefix.push_str(POWER);
            }
            if !layout.table_seeds.is_empty() {
                prefix.push_str(PERLIN);
            }
            if has(&fractal_noise) {
                prefix.push_str(PERLIN_FRACTAL);
            }
            if has(&live_noise) {
                prefix.push_str(PERLIN_LIVE);
            }
            let signature = "(@builtin(global_invocation_id) gid: vec3<u32>)";
            let counts = layout.counts;
            let mut head = match counts {
                Some(counts) => {
                    let w = counts / 4;
                    format!(
                        "  let i = gid.x;\n  var n = uniforms[0].x;\n  if ({} != 0u) {{ n = min(n, count_in[0] * {} / {}); }}\n  if (i == 0u) {{ count_out[0] = n; count_out[1] = 6u * n; count_out[2] = 1u; count_out[3] = 0u; count_out[4] = 0u; count_out[5] = 0u; count_out[6] = 6u; count_out[7] = n; count_out[8] = 0u; count_out[9] = 0u; }}\n  if (i >= n) {{ return; }}\n  var sb: u32 = 0u;\n",
                        word(w + 2),
                        word(w),
                        word(w + 1)
                    )
                }
                None => String::from("  let i = gid.x;\n  if (i >= uniforms[0].x) { return; }\n  var sb: u32 = 0u;\n"),
            };
            if image {
                let _ = writeln!(head, "  let image_width = u32(bitcast<f32>({}));", word(layout.image_width.unwrap_or(0)));
            }
            let body = format!("{head}{body}");
            let entry = format!("kernel_{:032x}", digest(format!("{prefix}{signature}{body}").as_bytes()));
            let source = format!("{prefix}@compute @workgroup_size(64)\nfn {entry}{signature} {{\n{body}}}\n");
            Ok(Wgsl {
                source,
                entry,
                interface,
                uniform_layout: layout.uniform_layout.clone(),
                uniform_bytes: uniform_words * 16,
                table_seeds: layout.table_seeds.clone(),
                output_widths: view.outputs.iter().map(|o| o.len() as i64).collect(),
                input_widths: if image { Vec::new() } else { view.widths.clone() },
                gathers: layout.gathers.iter().map(|(_, g)| *g).collect(),
                counts,
            })
        }
    }
}

fn wgsl_type(ty: &Ty) -> String {
    match ty {
        Ty::Float | Ty::Bool => "f32".to_owned(),
        Ty::Vec2 => "vec2<f32>".to_owned(),
        Ty::Vec3 => "vec3<f32>".to_owned(),
        Ty::Vec4 => "vec4<f32>".to_owned(),
        other => format!("<{}>", flow_syntax::ty::to_string(other)),
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
enum Resource { Buffer(i64), Texture(i64) }

/// Raster pipelines `(quads, vertex, fragment)` as one module: every resource in group 0 at its
/// binding (a binding of words declared `u32`, a float binding `f32`), texture t with its sampler
/// at t + 1, the varyings at locations in the vertex result's leaf order (`@interpolate(flat)`
/// under `:flat`).
pub fn raster(pipelines: &[(bool, Stage, Stage)]) -> Result<String, Diagnostic> {
    let mut resources: Vec<(Resource, bool)> = Vec::new();
    let mut words: HashSet<i64> = HashSet::new();
    // a binding any stage reads as words is words in every stage: its floats are their bits
    for (_, v, f) in pipelines {
        for (_, b, w) in v.buffers.iter().chain(f.buffers.iter()) {
            if *w {
                words.insert(*b);
            }
        }
    }
    let shared = |s: &Stage| -> Stage {
        let mut s = s.clone();
        s.buffers = s.buffers.iter().map(|(u, b, w)| (*u, *b, *w || words.contains(b))).collect();
        s
    };
    let mut text: Vec<String> = Vec::new();
    for (quads, v, f) in pipelines {
        let (v, f) = (shared(v), shared(f));
        let out = format!("{}_out", identifier(&v.entry));
        let mut location: i64 = -1;
        let mut fields = String::new();
        for (name, t, flat) in &v.outputs {
            if name == "position" {
                fields.push_str("  @builtin(position) position: vec4<f32>,\n");
            } else {
                location += 1;
                let _ = writeln!(fields, "  @location({location}) {}{}: {},", if *flat { "@interpolate(flat) " } else { "" }, identifier(name), wgsl_type(t));
            }
        }
        for s in [&v, &f] {
            for (_, b, w) in &s.buffers {
                replace(&mut resources, Resource::Buffer(*b), *w);
            }
            for i in s.program.code.iter() {
                if let Instruction::Stage(RStage::Sample(t, _), _) = Instruction::unguarded(i) {
                    replace(&mut resources, Resource::Texture(*t), true);
                }
            }
        }
        let vertex = print(true, false, Some(Placement::Vertex(*quads, v.clone())), &v.program)?;
        let fragment = print(true, false, Some(Placement::Fragment(out.clone(), f.clone())), &f.program)?;
        text.push(format!("struct {out} {{\n{fields}}}\n{}{}", vertex.source, fragment.source));
    }
    resources.sort_by_key(|(k, _)| *k);
    let mut declarations = String::new();
    for (key, words) in resources {
        match key {
            Resource::Buffer(b) => {
                let _ = writeln!(declarations, "@group(0) @binding({b}) var<storage, read> buffer{b}: array<{}>;", if words { "u32" } else { "f32" });
            }
            Resource::Texture(t) => {
                let _ = write!(
                    declarations,
                    "@group(0) @binding({t}) var texture{t}: texture_2d<f32>;\n@group(0) @binding({}) var sampler{}: sampler;\n",
                    t + 1,
                    t + 1
                );
            }
        }
    }
    let mut out = declarations;
    if pipelines.iter().any(|(quads, _, _)| *quads) {
        out.push_str("const quad_corner = array<u32, 6>(0u, 1u, 2u, 0u, 2u, 3u);\n");
    }
    out.push_str(INT64);
    out.push_str(POWER);
    out.push_str(&text.concat());
    Ok(out)
}

/// `Hashtbl.replace` on a short association list.
fn replace(resources: &mut Vec<(Resource, bool)>, key: Resource, words: bool) {
    match resources.iter_mut().find(|(k, _)| *k == key) {
        Some(entry) => entry.1 = words,
        None => resources.push((key, words)),
    }
}
