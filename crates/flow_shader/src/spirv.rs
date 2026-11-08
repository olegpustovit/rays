//! Binary SPIR-V compute emission from the shared kernel layout, and Vulkan raster/compute stage
//! modules (port of lib/flow_shader/spirv.ml). Opcodes and operands follow the Khronos unified
//! SPIR-V grammar: <https://registry.khronos.org/SPIR-V/>.
//!
//! Id allocation order is part of the output: nested calls are bound to temporaries left to right
//! whenever more than one operand allocates an id or a constant.
#![allow(non_snake_case)]
use crate::noise;
use crate::trace;
use flow_ir::core_ir::region::{self, Instruction, Loop, Projection, Stage};
use flow_ir::kernel_layout::{self, Visit};
use flow_ir::packed;
use flow_syntax::packed_ops::{self, Binary, Comparison, Conversion, Integer, Unary};
use flow_syntax::value;
use ogpu_core::shader::{Binding, BindingKind, Stage as ShaderStage};
use rays_diagnostic::Diagnostic;
use std::collections::HashMap;
use std::sync::Arc;

pub struct T {
    pub bytes: Vec<u8>,
    pub entry: String,
    pub interface: Vec<Binding>,
    pub layout: kernel_layout::T,
    pub uniform_bytes: i64,
}

#[derive(Default)]
pub struct Words {
    next: i64,
    annotations: Vec<u8>,
    declarations: Vec<u8>,
    body: Vec<u8>,
    constants: HashMap<(i64, i32), i64>,
    wide_constants: HashMap<i64, i64>,
}

fn add_word(into: &mut Vec<u8>, word: i64) {
    into.extend_from_slice(&(word as u32).to_le_bytes());
}

fn instruction(into: &mut Vec<u8>, opcode: i64, operands: &[i64]) {
    add_word(into, ((operands.len() as i64 + 1) << 16) | opcode);
    for w in operands {
        add_word(into, *w);
    }
}

impl Words {
    pub fn new() -> Words {
        Words { next: 1, ..Words::default() }
    }

    fn id(&mut self) -> i64 {
        let result = self.next;
        self.next = result + 1;
        result
    }

    fn declare(&mut self, opcode: i64, operands: &[i64]) -> i64 {
        let result = self.id();
        let mut all = vec![result];
        all.extend_from_slice(operands);
        instruction(&mut self.declarations, opcode, &all);
        result
    }

    fn value(&mut self, ty: i64, opcode: i64, operands: &[i64]) -> i64 {
        let result = self.id();
        let mut all = vec![ty, result];
        all.extend_from_slice(operands);
        instruction(&mut self.body, opcode, &all);
        result
    }

    fn constant(&mut self, ty: i64, bits: i32) -> i64 {
        if let Some(id) = self.constants.get(&(ty, bits)) {
            return *id;
        }
        let result = self.id();
        instruction(&mut self.declarations, 43, &[ty, result, bits as i64]);
        self.constants.insert((ty, bits), result);
        result
    }

    fn string_words(text: &str) -> Vec<i64> {
        let mut bytes = vec![0u8; (text.len() + 4) / 4 * 4];
        bytes[..text.len()].copy_from_slice(text.as_bytes());
        bytes.chunks(4).map(|c| i32::from_le_bytes([c[0], c[1], c[2], c[3]]) as i64).collect()
    }

    fn wide_constant(&mut self, ty: i64, bits: i64) -> i64 {
        if let Some(id) = self.wide_constants.get(&bits) {
            return *id;
        }
        let result = self.id();
        instruction(&mut self.declarations, 43, &[ty, result, (bits as i32) as i64, ((bits as u64 >> 32) as i32) as i64]);
        self.wide_constants.insert(bits, result);
        result
    }
}

#[derive(Clone, Debug, Default)]
pub struct Scene {
    accel: i64,
    accel_type: i64,
    vertices: i64,
    spheres: i64,
    instances: Option<i64>,
    vertices_end: Option<i64>,
    motion: bool,
    query: i64,
    nested_query: Option<i64>,
    curve_accel: Option<i64>,
    curve_query: Option<i64>,
    prototype_accel: Option<i64>,
    motion_best: Option<(i64, i64, i64, i64, i64)>,
    vec3: i64,
    vec2: i64,
}

#[derive(Default)]
pub struct Emitter {
    words: Words,
    boolean: i64,
    uint: i64,
    sint: i64,
    float: i64,
    long: Option<i64>,
    ulong: Option<i64>,
    /// A fast compute kernel: `long`/`ulong` are the 32-bit `sint`/`uint` (Emit's `integer32`).
    int32: bool,
    glsl: i64,
    uniform: i64,
    uniform_pointer: i64,
    storage_pointer: i64,
    uint_storage_pointer: i64,
    #[allow(dead_code)]
    status: i64,
    uint_function_pointer: i64,
    /// the thread's Function-storage status word
    status_bits: i64,
    table: Option<i64>,
    scene: Option<Scene>,
    block: i64,
    trace_cache: Vec<(Vec<i64>, i64, Vec<i64>)>,
    /// the strand segments' storage variable
    strands: Option<i64>,
    loop_depth: i64,
    failure: Option<Diagnostic>,
}

fn fail_d(message: &str) -> Diagnostic {
    value::fail("E_GPU_FORM", message)
}

impl Emitter {
    fn fail(&mut self, message: &str) -> i64 {
        if self.failure.is_none() {
            self.failure = Some(fail_d(message));
        }
        0
    }
    fn u(&mut self, value: i64) -> i64 {
        let ty = self.uint;
        self.words.constant(ty, value as i32)
    }
    fn f(&mut self, value: f64) -> i64 {
        let ty = self.float;
        self.words.constant(ty, (value as f32).to_bits() as i32)
    }
    fn result(&mut self, ty: i64, opcode: i64, operands: &[i64]) -> i64 {
        self.words.value(ty, opcode, operands)
    }
    fn long(&self) -> i64 {
        self.long.expect("a 64-bit integer type")
    }
    fn ulong(&self) -> i64 {
        self.ulong.expect("a 64-bit unsigned type")
    }
    fn l(&mut self, value: i64) -> i64 {
        let ty = self.long();
        if self.int32 {
            return self.words.constant(ty, value as i32);
        }
        self.words.wide_constant(ty, value)
    }
    fn from_long(&mut self, value: i64) -> i64 {
        let ty = self.float;
        self.result(ty, 111, &[value])
    }
    fn body(&mut self, opcode: i64, operands: &[i64]) {
        instruction(&mut self.words.body, opcode, operands);
    }
    fn annotate(&mut self, opcode: i64, operands: &[i64]) {
        instruction(&mut self.words.annotations, opcode, operands);
    }
    fn floating(&mut self, opcode: i64, operands: &[i64]) -> i64 {
        let ty = self.float;
        let value = self.result(ty, opcode, operands);
        if [129, 131, 133, 136, 140, 141].contains(&opcode) {
            self.annotate(71, &[value, 42]);
        }
        value
    }
    fn compare(&mut self, opcode: i64, a: i64, b: i64) -> i64 {
        let ty = self.boolean;
        self.result(ty, opcode, &[a, b])
    }
    fn select(&mut self, ty: i64, condition: i64, yes: i64, no: i64) -> i64 {
        self.result(ty, 169, &[condition, yes, no])
    }
    fn select_f(&mut self, condition: i64, yes: i64, no: i64) -> i64 {
        let ty = self.float;
        self.select(ty, condition, yes, no)
    }
    fn select_u(&mut self, condition: i64, yes: i64, no: i64) -> i64 {
        let ty = self.uint;
        self.select(ty, condition, yes, no)
    }
    fn select_l(&mut self, condition: i64, yes: i64, no: i64) -> i64 {
        let ty = self.long();
        self.select(ty, condition, yes, no)
    }
    fn truth(&mut self, value: i64) -> i64 {
        let zero = self.f(0.0);
        self.compare(183, value, zero)
    }
    fn numeric_truth(&mut self, value: i64) -> i64 {
        let one = self.f(1.0);
        let zero = self.f(0.0);
        self.select_f(value, one, zero)
    }
    fn ext(&mut self, opcode: i64, operands: &[i64]) -> i64 {
        let mut all = vec![self.glsl, opcode];
        all.extend_from_slice(operands);
        let ty = self.float;
        self.result(ty, 12, &all)
    }
    fn uniform(&mut self, word: i64) -> i64 {
        let a = self.u(0);
        let b = self.u(word / 4);
        let c = self.u(word % 4);
        let ty = self.uniform_pointer;
        let pointer = self.result(ty, 65, &[self.uniform, a, b, c]);
        let uint = self.uint;
        self.result(uint, 61, &[pointer])
    }
    fn uniform_float(&mut self, word: i64) -> i64 {
        let w = self.uniform(word);
        let ty = self.float;
        self.result(ty, 124, &[w])
    }
    fn load(&mut self, variable: i64, index: i64) -> i64 {
        let zero = self.u(0);
        let ty = self.storage_pointer;
        let pointer = self.result(ty, 65, &[variable, zero, index]);
        let float = self.float;
        self.result(float, 61, &[pointer])
    }
    fn store(&mut self, variable: i64, index: i64, value: i64) {
        let zero = self.u(0);
        let ty = self.storage_pointer;
        let pointer = self.result(ty, 65, &[variable, zero, index]);
        self.body(62, &[pointer, value]);
    }
    fn load_uint(&mut self, variable: i64, index: i64) -> i64 {
        let zero = self.u(0);
        let ty = self.uint_storage_pointer;
        let pointer = self.result(ty, 65, &[variable, zero, index]);
        let uint = self.uint;
        self.result(uint, 61, &[pointer])
    }
    fn store_uint(&mut self, variable: i64, index: i64, value: i64) {
        let zero = self.u(0);
        let ty = self.uint_storage_pointer;
        let pointer = self.result(ty, 65, &[variable, zero, index]);
        self.body(62, &[pointer, value]);
    }
    fn offset(&mut self, index: i64, width: i64, component: i64) -> i64 {
        let w = self.u(width);
        let uint = self.uint;
        let m = self.result(uint, 132, &[index, w]);
        let c = self.u(component);
        self.result(uint, 128, &[m, c])
    }
    fn non_finite(&mut self, value: i64) -> i64 {
        let ty = self.boolean;
        let a = self.result(ty, 156, &[value]);
        let b = self.result(ty, 157, &[value]);
        self.result(ty, 166, &[a, b])
    }
    fn label(&mut self, id: i64) {
        self.body(248, &[id]);
        self.block = id;
    }
    fn jump(&mut self, id: i64) {
        self.body(249, &[id]);
    }
    /// Status bits gather in the thread's word and reach the shared status with the kernel's one atomic.
    fn record_status(&mut self, condition: i64, bits: i64) {
        let uint = self.uint;
        let old = self.result(uint, 61, &[self.status_bits]);
        let b = self.u(bits);
        let zero = self.u(0);
        let bit = self.result(uint, 169, &[condition, b, zero]);
        let or = self.result(uint, 197, &[old, bit]);
        self.body(62, &[self.status_bits, or]);
    }
    fn mark_invalid(&mut self, invalid: &mut i64, condition: i64) {
        if self.loop_depth > 0 {
            self.record_status(condition, 1);
        } else {
            let ty = self.boolean;
            *invalid = self.result(ty, 166, &[*invalid, condition]);
        }
    }
    fn conditional(&mut self, ty: i64, condition: i64, yes: &mut dyn FnMut(&mut Emitter) -> i64, no: &mut dyn FnMut(&mut Emitter) -> i64) -> i64 {
        let yes_block = self.words.id();
        let no_block = self.words.id();
        let after = self.words.id();
        self.body(247, &[after, 0]);
        self.body(250, &[condition, yes_block, no_block]);
        self.label(yes_block);
        let yes_value = yes(self);
        let yes_end = self.block;
        self.jump(after);
        self.label(no_block);
        let no_value = no(self);
        let no_end = self.block;
        self.jump(after);
        self.label(after);
        self.result(ty, 245, &[yes_value, yes_end, no_value, no_end])
    }
    fn bool_false(&mut self) -> i64 {
        let z1 = self.u(0);
        let z2 = self.u(0);
        self.compare(171, z1, z2)
    }
}

fn binary(e: &mut Emitter, operation: Binary, a: i64, b: i64) -> i64 {
    let float = e.float;
    let boolean = e.boolean;
    match operation {
        Binary::Add => e.floating(129, &[a, b]),
        Binary::Sub => e.floating(131, &[a, b]),
        Binary::Mul => e.floating(133, &[a, b]),
        Binary::Div => {
            let z = e.f(0.0);
            let c = e.compare(180, b, z);
            let q = e.floating(136, &[a, b]);
            e.select_f(c, z, q)
        }
        Binary::Mod => {
            let r1 = e.floating(140, &[a, b]);
            let r2 = e.floating(129, &[r1, b]);
            let remainder = e.floating(140, &[r2, b]);
            let z = e.f(0.0);
            let c = e.compare(180, b, z);
            e.select_f(c, z, remainder)
        }
        Binary::Pow => {
            // zero = select (b == 0) 1 (select (b > 0) 0 infinity)
            let z = e.f(0.0);
            let c180 = e.compare(180, b, z);
            let one = e.f(1.0);
            let c186 = e.compare(186, b, z);
            let inf = e.f(f64::INFINITY);
            let inner = e.select_f(c186, z, inf);
            let zero = e.select_f(c180, one, inner);
            let z0 = e.f(0.0);
            let c = e.compare(180, a, z0);
            let abs = e.ext(4, &[a]);
            let pow = e.ext(26, &[abs, b]);
            e.select_f(c, zero, pow)
        }
        Binary::Min => e.ext(37, &[a, b]),
        Binary::Max => e.ext(40, &[a, b]),
        Binary::Lt | Binary::Le | Binary::Gt | Binary::Ge | Binary::Eq => {
            let opcode = match operation {
                Binary::Lt => 184,
                Binary::Le => 188,
                Binary::Gt => 186,
                Binary::Ge => 190,
                _ => 180,
            };
            let c = e.compare(opcode, a, b);
            e.numeric_truth(c)
        }
        Binary::And => {
            let ta = e.truth(a);
            let tb = e.truth(b);
            let c = e.result(boolean, 167, &[ta, tb]);
            e.numeric_truth(c)
        }
        Binary::Or => {
            let ta = e.truth(a);
            let tb = e.truth(b);
            let c = e.result(boolean, 166, &[ta, tb]);
            e.numeric_truth(c)
        }
        Binary::Atan2 => e.ext(25, &[a, b]),
        Binary::Hypot => {
            e.fail("Native-safe hypot is not supported by the SPIR-V kernel target.");
            let _ = float;
            0
        }
        Binary::Ldexp => {
            let sint = e.sint;
            let i = e.result(sint, 110, &[b]);
            e.ext(53, &[a, i])
        }
    }
}

/// GLSL.std.450 promises Sin, Cos and Tan only near [-pi, pi], and a driver may lose every bit past
/// it: the hash idiom `sin(i * 12.9898) * 43758.5` collapses to a few values. So the angle is reduced
/// first, Cody-Waite in two parts of 2 pi with exact Fma, which leaves an angle under pi unchanged.
fn reduced_angle(e: &mut Emitter, value: i64) -> i64 {
    let scale = e.f(1.0 / (2.0 * std::f64::consts::PI));
    let float = e.float;
    let m = e.result(float, 133, &[value, scale]);
    let turns = e.ext(1, &[m]);
    let minus = e.result(float, 127, &[turns]);
    let high = f32::from_bits(0x40C90FDB) as f64;
    let fh = e.f(high);
    let first = e.ext(50, &[minus, fh, value]);
    let fl = e.f((2.0 * std::f64::consts::PI) - high);
    e.ext(50, &[minus, fl, first])
}

fn unary(e: &mut Emitter, operation: Unary, value: i64) -> i64 {
    match operation {
        Unary::Sin => {
            let r = reduced_angle(e, value);
            e.ext(13, &[r])
        }
        Unary::Cos => {
            let r = reduced_angle(e, value);
            e.ext(14, &[r])
        }
        Unary::Tan => {
            let r = reduced_angle(e, value);
            e.ext(15, &[r])
        }
        Unary::Abs => e.ext(4, &[value]),
        Unary::Sqrt => {
            let a = e.ext(4, &[value]);
            e.ext(31, &[a])
        }
        Unary::Rsqrt => {
            let a = e.ext(4, &[value]);
            e.ext(32, &[a])
        }
        Unary::Not => {
            let z = e.f(0.0);
            let c = e.compare(180, value, z);
            e.numeric_truth(c)
        }
        Unary::Asin | Unary::Acos => {
            let minus = e.f(-1.0);
            let one = e.f(1.0);
            let clamped = e.ext(43, &[value, minus, one]);
            let abs = e.ext(4, &[value]);
            let c = e.compare(186, abs, one);
            let nan = e.f(f64::NAN);
            let r = e.ext(if operation == Unary::Asin { 16 } else { 17 }, &[clamped]);
            e.select_f(c, nan, r)
        }
        Unary::Exp => e.ext(27, &[value]),
        Unary::Log => {
            let z = e.f(0.0);
            let positive = e.compare(186, value, z);
            let one = e.f(1.0);
            let s = e.select_f(positive, value, one);
            let l = e.ext(28, &[s]);
            let nan = e.f(f64::NAN);
            e.select_f(positive, l, nan)
        }
        Unary::Sign => {
            let z = e.f(0.0);
            let c186 = e.compare(186, value, z);
            let one = e.f(1.0);
            let c184 = e.compare(184, value, z);
            let minus = e.f(-1.0);
            let inner = e.select_f(c184, minus, value);
            e.select_f(c186, one, inner)
        }
    }
}

/// A runtime-array storage type: (the struct's, the element's) pointer types.
fn storage_type(words: &mut Words, scalar: i64) -> (i64, i64) {
    let array = words.declare(29, &[scalar]);
    let structure = words.declare(30, &[array]);
    instruction(&mut words.annotations, 71, &[array, 6, 4]);
    instruction(&mut words.annotations, 71, &[structure, 2]);
    instruction(&mut words.annotations, 72, &[structure, 0, 35, 0]);
    let a = words.declare(32, &[12, structure]);
    let b = words.declare(32, &[12, scalar]);
    (a, b)
}

fn integer_binary(e: &mut Emitter, operation: Integer, a: i64, b: i64) -> i64 {
    let long = e.long();
    match operation {
        Integer::Ishl | Integer::Ishr if e.int32 => {
            // logical over 32 bits; a count over 31 gives 0
            let c63 = e.l(63);
            let k = e.result(long, 199, &[b, c63]);
            let c31 = e.l(31);
            let kk = e.result(long, 199, &[k, c31]);
            let shifted = e.result(long, if operation == Integer::Ishl { 196 } else { 194 }, &[a, kk]);
            let c = e.compare(173, k, c31);
            let z = e.l(0);
            e.select_l(c, z, shifted)
        }
        Integer::Iadd => e.result(long, 128, &[a, b]),
        Integer::Isub => e.result(long, 130, &[a, b]),
        Integer::Imul => e.result(long, 132, &[a, b]),
        Integer::Iand => e.result(long, 199, &[a, b]),
        Integer::Ior => e.result(long, 197, &[a, b]),
        Integer::Ixor => e.result(long, 198, &[a, b]),
        Integer::Ishl | Integer::Ishr => {
            let sixty_three = e.l(63);
            let masked = e.result(long, 199, &[b, sixty_three]);
            e.result(long, if operation == Integer::Ishl { 196 } else { 194 }, &[a, masked])
        }
        Integer::Idiv | Integer::Imod => {
            // Truncating, as Emit's `/` and `%`, from the magnitudes with unsigned operations: the NVIDIA
            // driver's OpSRem and OpSDiv by a constant returned the unsigned result for a negative
            // dividend.
            let ulong = e.ulong();
            let zero_l = e.l(0);
            let zero = e.compare(170, b, zero_l);
            fn negative(e: &mut Emitter, x: i64) -> i64 {
                let z = e.l(0);
                e.compare(177, x, z)
            }
            fn negate(e: &mut Emitter, x: i64) -> i64 {
                let long = e.long();
                let z = e.l(0);
                e.result(long, 130, &[z, x])
            }
            fn magnitude(e: &mut Emitter, x: i64) -> i64 {
                let c = negative(e, x);
                let n = negate(e, x);
                e.select_l(c, n, x)
            }
            let ma = magnitude(e, a);
            let ua = e.result(ulong, 124, &[ma]);
            let one = e.l(1);
            let mb = magnitude(e, b);
            let divisor = e.select_l(zero, one, mb);
            let ud = e.result(ulong, 124, &[divisor]);
            let division = e.result(ulong, if operation == Integer::Idiv { 134 } else { 137 }, &[ua, ud]);
            let value = e.result(long, 124, &[division]);
            let flip = if operation == Integer::Idiv {
                let na = negative(e, a);
                let nb = negative(e, b);
                let boolean = e.boolean;
                e.result(boolean, 165, &[na, nb])
            } else {
                negative(e, a)
            };
            let negated = negate(e, value);
            let signed = e.select_l(flip, negated, value);
            let z = e.l(0);
            e.select_l(zero, z, signed)
        }
    }
}

fn integer_float_binary(e: &mut Emitter, operation: Binary, a: i64, b: i64) -> i64 {
    match operation {
        Binary::Add => integer_binary(e, Integer::Iadd, a, b),
        Binary::Sub => integer_binary(e, Integer::Isub, a, b),
        Binary::Mul => integer_binary(e, Integer::Imul, a, b),
        Binary::Min => {
            let c = e.compare(177, a, b);
            e.select_l(c, a, b)
        }
        Binary::Max => {
            let c = e.compare(173, a, b);
            e.select_l(c, a, b)
        }
        Binary::Mod => {
            let r = integer_binary(e, Integer::Imod, a, b);
            let boolean = e.boolean;
            let zb = e.l(0);
            let c171 = e.compare(171, r, zb);
            let cr = e.compare(177, r, zb);
            let cb = e.compare(177, b, zb);
            let sign = e.result(boolean, 165, &[cr, cb]);
            let adjust = e.result(boolean, 167, &[c171, sign]);
            let sum = integer_binary(e, Integer::Iadd, r, b);
            e.select_l(adjust, sum, r)
        }
        _ => unreachable!("not an integer-float operation"),
    }
}

fn unguarded(i: &Instruction) -> &Instruction {
    match i {
        Instruction::Guarded(_, body) => unguarded(body),
        body => body,
    }
}

fn needs_integer(layout: &kernel_layout::T) -> bool {
    let mut whole = vec![false; layout.view.code.len()];
    let mut needed = false;
    for (slot, instruction) in layout.view.code.iter().enumerate() {
        let instruction = unguarded(instruction);
        let integer = region::integer_register(instruction);
        whole[slot] = integer
            || match instruction {
                Instruction::Const(value) => value.is_finite() && value.fract() == 0.0 && value.abs() < 9007199254740992.0,
                Instruction::Convert(Conversion::ToFloat, a) => whole[*a as usize],
                Instruction::Binary(Binary::Add | Binary::Sub | Binary::Mul | Binary::Mod | Binary::Min | Binary::Max, a, b) => whole[*a as usize] && whole[*b as usize],
                _ => false,
            };
        if layout.used[slot]
            && (integer || matches!(instruction, Instruction::Binary(Binary::Add | Binary::Sub | Binary::Mul | Binary::Mod | Binary::Min | Binary::Max, _, _)) && whole[slot])
        {
            needed = true;
        }
    }
    needed
}

fn round_float(e: &mut Emitter, op: &Conversion, value: i64) -> i64 {
    match op {
        Conversion::Floor => e.ext(8, &[value]),
        Conversion::Ceil => e.ext(9, &[value]),
        _ => {
            let sign = unary(e, Unary::Sign, value);
            let abs = e.ext(4, &[value]);
            let half = e.f(0.5);
            let sum = e.floating(129, &[abs, half]);
            let floor = e.ext(8, &[sum]);
            e.floating(133, &[sign, floor])
        }
    }
}

fn int_compare_opcode(op: Comparison) -> i64 {
    match op {
        Comparison::Ilt => 177,
        Comparison::Ile => 179,
        Comparison::Igt => 173,
        Comparison::Ige => 175,
        Comparison::Ieq => 170,
    }
}

/// A ray query: initialise `query` over origin, direction, tmin and tmax.
fn ray_query_init(e: &mut Emitter, scene: &Scene, query: i64, accel: i64, flags: i64, origin: [i64; 3], tmin: i64, direction: [i64; 3], tmax: i64) {
    let v3 = scene.vec3;
    let loaded = e.result(scene.accel_type, 61, &[accel]);
    let cflags = e.u(flags);
    let c255 = e.u(255);
    let vo = e.result(v3, 80, &[origin[0], origin[1], origin[2]]);
    let vd = e.result(v3, 80, &[direction[0], direction[1], direction[2]]);
    e.body(4473, &[query, loaded, cflags, c255, vo, tmin, vd, tmax]);
}

fn ray_query_loop(e: &mut Emitter, query: i64, candidate: &mut dyn FnMut(&mut Emitter)) {
    let header = e.words.id();
    let body = e.words.id();
    let continuing = e.words.id();
    let merge = e.words.id();
    e.jump(header);
    e.label(header);
    let boolean = e.boolean;
    let proceed = e.result(boolean, 4477, &[query]);
    e.body(246, &[merge, continuing, 0]);
    e.body(250, &[proceed, body, merge]);
    e.label(body);
    candidate(e);
    e.jump(continuing);
    e.label(continuing);
    e.jump(header);
    e.label(merge);
}

fn sub3(ray: &[i64], at: usize) -> [i64; 3] {
    [ray[at], ray[at + 1], ray[at + 2]]
}

fn triangle_test(e: &mut Emitter, scene: &Scene, ray: &[i64], primitive: i64, time: i64) -> (i64, i64, i64, i64) {
    let uint = e.uint;
    let float = e.float;
    let boolean = e.boolean;
    fn add(e: &mut Emitter, a: i64, b: i64) -> i64 {
        let uint = e.uint;
        e.result(uint, 128, &[a, b])
    }
    fn times(e: &mut Emitter, a: i64, k: i64) -> i64 {
        let uint = e.uint;
        let c = e.u(k);
        e.result(uint, 132, &[a, c])
    }
    let zero_f = e.f(0.0);
    let one_f = e.f(1.0);
    let time = e.ext(43, &[time, zero_f, one_f]);
    let vertices = scene.vertices_end.expect("flat motion vertices");
    let corner = |e: &mut Emitter, corner: i64| -> [i64; 3] {
        let mut out = [0i64; 3];
        for axis in 0..3 {
            let p3 = times(e, primitive, 3);
            let cc = e.u(corner);
            let s = add(e, p3, cc);
            let t = times(e, s, 3);
            let ca = e.u(axis);
            let at = add(e, t, ca);
            let a = e.load(scene.vertices, at);
            let b = e.load(vertices, at);
            let d = e.floating(131, &[b, a]);
            let m = e.floating(133, &[d, time]);
            out[axis as usize] = e.floating(129, &[a, m]);
        }
        out
    };
    let a = corner(e, 0);
    let b = corner(e, 1);
    let c = corner(e, 2);
    let direction = sub3(ray, 3);
    let origin = sub3(ray, 0);
    let (dx, dy, dz) = (direction[0], direction[1], direction[2]);
    let ax = e.ext(4, &[dx]);
    let ay = e.ext(4, &[dy]);
    let az = e.ext(4, &[dz]);
    let c_ay = e.compare(186, ax, ay);
    let c_az = e.compare(186, ax, az);
    let x_major = e.result(boolean, 167, &[c_ay, c_az]);
    let y_major = e.compare(186, ay, az);
    let u0 = e.u(0);
    let u1 = e.u(1);
    let u2 = e.u(2);
    let inner = e.select_u(y_major, u1, u2);
    let kz = e.select_u(x_major, u0, inner);
    let t = e.result(uint, 128, &[kz, u1]);
    let u3 = e.u(3);
    let kx = e.result(uint, 137, &[t, u3]);
    let t = e.result(uint, 128, &[kx, u1]);
    let ky = e.result(uint, 137, &[t, u3]);
    let direction_vector = e.result(scene.vec3, 80, &direction);
    let kz_value = e.result(float, 77, &[direction_vector, kz]);
    let swap = e.compare(184, kz_value, zero_f);
    let kx_new = e.select_u(swap, ky, kx);
    let ky_new = e.select_u(swap, kx, ky);
    let (kx, ky) = (kx_new, ky_new);
    fn component(e: &mut Emitter, vec3: i64, vector: &[i64; 3], axis: i64) -> i64 {
        let float = e.float;
        let v = e.result(vec3, 80, vector);
        e.result(float, 77, &[v, axis])
    }
    let c180 = e.compare(180, kz_value, zero_f);
    let safe_direction = e.select_f(c180, one_f, kz_value);
    let vec3 = scene.vec3;
    let cx = component(e, vec3, &direction, kx);
    let sx = e.floating(136, &[cx, safe_direction]);
    let cy = component(e, vec3, &direction, ky);
    let sy = e.floating(136, &[cy, safe_direction]);
    let sz = e.floating(136, &[one_f, safe_direction]);
    let projected = |e: &mut Emitter, point: &[i64; 3]| -> (i64, i64, i64) {
        let mut translated = [0i64; 3];
        for k in 0..3 {
            translated[k] = e.floating(131, &[point[k], origin[k]]);
        }
        let z = component(e, vec3, &translated, kz);
        let cx = component(e, vec3, &translated, kx);
        let m = e.floating(133, &[sx, z]);
        let x = e.floating(131, &[cx, m]);
        let cy = component(e, vec3, &translated, ky);
        let m = e.floating(133, &[sy, z]);
        let y = e.floating(131, &[cy, m]);
        (x, y, e.floating(133, &[sz, z]))
    };
    let (ax, ay, az) = projected(e, &a);
    let (bx, by, bz) = projected(e, &b);
    let (cx, cy, cz) = projected(e, &c);
    let edge = |e: &mut Emitter, x0: i64, y0: i64, x1: i64, y1: i64| -> i64 {
        let l = e.floating(133, &[x0, y1]);
        let r = e.floating(133, &[y0, x1]);
        e.floating(131, &[l, r])
    };
    let e0 = edge(e, bx, by, cx, cy);
    let e1 = edge(e, cx, cy, ax, ay);
    let e2 = edge(e, ax, ay, bx, by);
    let n0 = e.compare(184, e0, zero_f);
    let n1 = e.compare(184, e1, zero_f);
    let inner = e.result(boolean, 166, &[n0, n1]);
    let n2 = e.compare(184, e2, zero_f);
    let has_negative = e.result(boolean, 166, &[inner, n2]);
    let p0 = e.compare(186, e0, zero_f);
    let p1 = e.compare(186, e1, zero_f);
    let inner = e.result(boolean, 166, &[p0, p1]);
    let p2 = e.compare(186, e2, zero_f);
    let has_positive = e.result(boolean, 166, &[inner, p2]);
    let sum = e.floating(129, &[e0, e1]);
    let determinant = e.floating(129, &[sum, e2]);
    let both = e.result(boolean, 167, &[has_negative, has_positive]);
    let valid_edges = e.result(boolean, 168, &[both]);
    let c180 = e.compare(180, determinant, zero_f);
    let safe_determinant = e.select_f(c180, one_f, determinant);
    let t0 = e.floating(133, &[e0, az]);
    let t1 = e.floating(133, &[e1, bz]);
    let s = e.floating(129, &[t0, t1]);
    let t2 = e.floating(133, &[e2, cz]);
    let scaled_t = e.floating(129, &[s, t2]);
    let distance = e.floating(136, &[scaled_t, safe_determinant]);
    let u_value = e.floating(136, &[e1, safe_determinant]);
    let v_value = e.floating(136, &[e2, safe_determinant]);
    let ge = e.compare(190, distance, ray[6]);
    let le = e.compare(188, distance, ray[7]);
    let range = e.result(boolean, 167, &[ge, le]);
    let c180 = e.compare(180, determinant, zero_f);
    let nonzero = e.result(boolean, 168, &[c180]);
    let edges = e.result(boolean, 167, &[valid_edges, nonzero]);
    let hit = e.result(boolean, 167, &[edges, range]);
    (hit, distance, u_value, v_value)
}

fn moving_triangles_query(e: &mut Emitter, scene: &Scene, ray: &[i64], any: bool) -> Vec<i64> {
    let query = scene.query;
    ray_query_init(e, scene, query, scene.accel, 0, sub3(ray, 0), ray[6], sub3(ray, 3), ray[7]);
    ray_query_loop(e, query, &mut |e: &mut Emitter| {
        let uint = e.uint;
        let z = e.u(0);
        let kind = e.result(uint, 4479, &[query, z]);
        let one = e.u(1);
        let c = e.compare(170, kind, one);
        let scene2 = scene.clone();
        e.conditional(
            uint,
            c,
            &mut |e: &mut Emitter| {
                let uint = e.uint;
                let z = e.u(0);
                let primitive = e.result(uint, 6023, &[query, z]);
                let (hit, distance, _, _) = triangle_test(e, &scene2, ray, primitive, ray[8]);
                e.conditional(
                    uint,
                    hit,
                    &mut |e: &mut Emitter| {
                        e.body(4475, &[query, distance]);
                        e.u(1)
                    },
                    &mut |e: &mut Emitter| e.u(0),
                )
            },
            &mut |e: &mut Emitter| e.u(0),
        );
    });
    let uint = e.uint;
    let float = e.float;
    let one = e.u(1);
    let committed = e.result(uint, 4479, &[query, one]);
    let two = e.u(2);
    let hit = e.compare(170, committed, two);
    if any {
        return vec![hit];
    }
    let raw_primitive = e.result(uint, 6023, &[query, one]);
    let z = e.u(0);
    let primitive = e.select_u(hit, raw_primitive, z);
    let (_test, _, bary_u, bary_v) = triangle_test(e, scene, ray, primitive, ray[8]);
    let t = e.result(float, 6018, &[query, one]);
    let zero = e.u(0);
    vec![t, primitive, bary_u, bary_v, hit, zero]
}

/// (valid, origin, direction) of the ray in an instance's moving frame.
fn motion_instance_ray(e: &mut Emitter, scene: &Scene, ray: &[i64], instance: i64) -> (i64, [i64; 3], [i64; 3]) {
    let table = scene.instances.expect("an instance table");
    let uint = e.uint;
    let float = e.float;
    let c32 = e.u(32);
    let base = e.result(uint, 132, &[instance, c32]);
    let word = |e: &mut Emitter, index: i64| -> i64 {
        let c = e.u(index);
        e.result(uint, 128, &[base, c])
    };
    let zero = e.f(0.0);
    let one = e.f(1.0);
    let time = e.ext(43, &[ray[8], zero, one]);
    let mut matrix = [0i64; 12];
    for k in 0..12 {
        let w = word(e, k as i64 + 1);
        let start = e.load(table, w);
        let w = word(e, k as i64 + 13);
        let ending = e.load(table, w);
        let d = e.floating(131, &[ending, start]);
        let m = e.floating(133, &[d, time]);
        matrix[k] = e.floating(129, &[start, m]);
    }
    let add = |e: &mut Emitter, a: i64, b: i64| e.floating(129, &[a, b]);
    let sub = |e: &mut Emitter, a: i64, b: i64| e.floating(131, &[a, b]);
    let mul = |e: &mut Emitter, a: i64, b: i64| e.floating(133, &[a, b]);
    let (a, b, c, tx) = (matrix[0], matrix[1], matrix[2], matrix[3]);
    let (d, e_, f_, ty) = (matrix[4], matrix[5], matrix[6], matrix[7]);
    let (g, h, i, tz) = (matrix[8], matrix[9], matrix[10], matrix[11]);
    // the nine cofactors
    let mut cofactors = [0i64; 9];
    let pairs: [((i64, i64), (i64, i64)); 9] = [
        ((e_, i), (f_, h)),
        ((c, h), (b, i)),
        ((b, f_), (c, e_)),
        ((f_, g), (d, i)),
        ((a, i), (c, g)),
        ((c, d), (a, f_)),
        ((d, h), (e_, g)),
        ((b, g), (a, h)),
        ((a, e_), (b, d)),
    ];
    for k in 0..9 {
        let ((p, q), (r, s)) = pairs[k];
        let m1 = mul(e, p, q);
        let m2 = mul(e, r, s);
        cofactors[k] = sub(e, m1, m2);
    }
    let m0 = mul(e, a, cofactors[0]);
    let m3 = mul(e, b, cofactors[3]);
    let s1 = add(e, m0, m3);
    let m6 = mul(e, c, cofactors[6]);
    let determinant = add(e, s1, m6);
    let boolean = e.boolean;
    let cz = e.compare(180, determinant, zero);
    let valid = e.result(boolean, 168, &[cz]);
    let c180 = e.compare(180, determinant, zero);
    let safe_determinant = e.select_f(c180, one, determinant);
    let invert = |e: &mut Emitter, vector: [i64; 3], translation: [i64; 3]| -> [i64; 3] {
        // let x = ... and y = ... and z = ...: in order
        let x = sub(e, vector[0], translation[0]);
        let y = sub(e, vector[1], translation[1]);
        let z = sub(e, vector[2], translation[2]);
        let mut out = [0i64; 3];
        for row in 0..3 {
            let c0 = cofactors[row * 3];
            let c1 = cofactors[row * 3 + 1];
            let c2 = cofactors[row * 3 + 2];
            let mx = mul(e, c0, x);
            let my = mul(e, c1, y);
            let s = add(e, mx, my);
            let mz = mul(e, c2, z);
            let t = add(e, s, mz);
            out[row] = e.floating(136, &[t, safe_determinant]);
        }
        out
    };
    let origin = invert(e, sub3(ray, 0), [tx, ty, tz]);
    let direction = invert(e, sub3(ray, 3), [zero, zero, zero]);
    let _ = float;
    (valid, origin, direction)
}

/// Trace the nested prototype instance's triangles in the instance's frame: (hit, distance, primitive,
/// barycentric u, barycentric v).
fn nested_instance_trace(e: &mut Emitter, scene: &Scene, query: i64, origin: [i64; 3], direction: [i64; 3], ray: &[i64], prototype: i64) -> (i64, i64, i64, i64, i64) {
    ray_query_init(e, scene, query, scene.prototype_accel.expect("a prototype structure"), 2, origin, ray[6], direction, ray[7]);
    ray_query_loop(e, query, &mut |e: &mut Emitter| {
        let uint = e.uint;
        let boolean = e.boolean;
        let z = e.u(0);
        let kind = e.result(uint, 4479, &[query, z]);
        let custom = e.result(uint, 6019, &[query, z]);
        let triangle = e.compare(170, kind, z);
        let target = e.compare(170, custom, prototype);
        let accept = e.result(boolean, 167, &[triangle, target]);
        e.conditional(
            uint,
            accept,
            &mut |e: &mut Emitter| {
                e.body(4476, &[query]);
                e.u(1)
            },
            &mut |e: &mut Emitter| e.u(0),
        );
    });
    let uint = e.uint;
    let float = e.float;
    let one = e.u(1);
    let committed = e.result(uint, 4479, &[query, one]);
    let hit = e.compare(170, committed, one);
    let zero = e.u(0);
    let raw = e.result(uint, 6023, &[query, one]);
    let primitive = e.select_u(hit, raw, zero);
    let zero_f = e.f(0.0);
    let raw_t = e.result(float, 6018, &[query, one]);
    let distance = e.select_f(hit, raw_t, zero_f);
    let barycentrics = e.result(scene.vec2, 6024, &[query, one]);
    let u_ = e.result(float, 81, &[barycentrics, 0]);
    let v = e.result(float, 81, &[barycentrics, 1]);
    (hit, distance, primitive, u_, v)
}

fn moving_instances_query(e: &mut Emitter, scene: &Scene, ray: &[i64], any: bool) -> Vec<i64> {
    let query = scene.query;
    let nested = scene.nested_query.expect("a nested query");
    let (best_t, best_primitive, best_instance, best_bary_u, best_bary_v) = scene.motion_best.expect("motion best variables");
    let inf = e.f(f64::INFINITY);
    e.body(62, &[best_t, inf]);
    let z = e.u(0);
    e.body(62, &[best_primitive, z]);
    e.body(62, &[best_instance, z]);
    let zf = e.f(0.0);
    e.body(62, &[best_bary_u, zf]);
    e.body(62, &[best_bary_v, zf]);
    ray_query_init(e, scene, query, scene.accel, 2, sub3(ray, 0), ray[6], sub3(ray, 3), ray[7]);
    let scene_c = scene.clone();
    ray_query_loop(e, query, &mut |e: &mut Emitter| {
        let uint = e.uint;
        let z = e.u(0);
        let kind = e.result(uint, 4479, &[query, z]);
        let one = e.u(1);
        let candidate = e.compare(170, kind, one);
        let scene = scene_c.clone();
        e.conditional(
            uint,
            candidate,
            &mut |e: &mut Emitter| {
                let uint = e.uint;
                let z = e.u(0);
                let instance = e.result(uint, 6020, &[query, z]);
                let prototype = e.result(uint, 6021, &[query, z]);
                let (valid, origin, direction) = motion_instance_ray(e, &scene, ray, instance);
                e.conditional(
                    uint,
                    valid,
                    &mut |e: &mut Emitter| {
                        let uint = e.uint;
                        let boolean = e.boolean;
                        let float = e.float;
                        let (hit, distance, primitive, bary_u, bary_v) = nested_instance_trace(e, &scene, nested, origin, direction, ray, prototype);
                        let best = e.result(float, 61, &[best_t]);
                        let closer = e.compare(184, distance, best);
                        let accept = e.result(boolean, 167, &[hit, closer]);
                        e.conditional(
                            uint,
                            accept,
                            &mut |e: &mut Emitter| {
                                e.body(4475, &[query, distance]);
                                e.body(62, &[best_t, distance]);
                                e.body(62, &[best_primitive, primitive]);
                                e.body(62, &[best_instance, instance]);
                                e.body(62, &[best_bary_u, bary_u]);
                                e.body(62, &[best_bary_v, bary_v]);
                                e.u(1)
                            },
                            &mut |e: &mut Emitter| e.u(0),
                        )
                    },
                    &mut |e: &mut Emitter| e.u(0),
                )
            },
            &mut |e: &mut Emitter| e.u(0),
        );
    });
    let uint = e.uint;
    let one = e.u(1);
    let committed = e.result(uint, 4479, &[query, one]);
    let two = e.u(2);
    let hit = e.compare(170, committed, two);
    if any {
        return vec![hit];
    }
    let float = e.float;
    fn output(e: &mut Emitter, hit: i64, value: i64, ty: i64, zero: i64) -> i64 {
        let loaded = e.result(ty, 61, &[value]);
        e.select(ty, hit, loaded, zero)
    }
    let z = e.u(0);
    let zf = e.f(0.0);
    let t = output(e, hit, best_t, float, zf);
    let p = output(e, hit, best_primitive, uint, z);
    let bu = output(e, hit, best_bary_u, float, zf);
    let bv = output(e, hit, best_bary_v, float, zf);
    let instance = output(e, hit, best_instance, uint, z);
    vec![t, p, bu, bv, hit, instance]
}

fn capsule_intersection(e: &mut Emitter, ray: &[i64], primitive: i64) -> (i64, i64, i64) {
    let float = e.float;
    let boolean = e.boolean;
    let uint = e.uint;
    let add = |e: &mut Emitter, a: i64, b: i64| e.floating(129, &[a, b]);
    let sub = |e: &mut Emitter, a: i64, b: i64| e.floating(131, &[a, b]);
    let mul = |e: &mut Emitter, a: i64, b: i64| e.floating(133, &[a, b]);
    let div = |e: &mut Emitter, a: i64, b: i64| e.floating(136, &[a, b]);
    let logical_and = |e: &mut Emitter, a: i64, b: i64| {
        let boolean = e.boolean;
        e.result(boolean, 167, &[a, b])
    };
    let c8 = e.u(8);
    let base = e.result(uint, 132, &[primitive, c8]);
    let strands = e.strands.expect("strand segments");
    let strand = |e: &mut Emitter, component: i64| -> i64 {
        let c = e.u(component);
        let w = e.result(uint, 128, &[base, c]);
        e.load(strands, w)
    };
    let a = [strand(e, 0), strand(e, 1), strand(e, 2)];
    let b = [strand(e, 4), strand(e, 5), strand(e, 6)];
    let radius = strand(e, 3);
    let origin = sub3(ray, 0);
    let direction = sub3(ray, 3);
    let dot = |e: &mut Emitter, a: &[i64; 3], b: &[i64; 3]| -> i64 {
        // ext 50 [a2; b2; ext 50 [a1; b1; mul a0 b0]]
        let m = mul(e, a[0], b[0]);
        let inner = e.ext(50, &[a[1], b[1], m]);
        e.ext(50, &[a[2], b[2], inner])
    };
    let sqrt = |e: &mut Emitter, value: i64| {
        let a = e.ext(4, &[value]);
        e.ext(31, &[a])
    };
    let half = e.f(0.5);
    let mut center = [0i64; 3];
    for k in 0..3 {
        let s = add(e, a[k], b[k]);
        center[k] = mul(e, s, half);
    }
    let mut to_center = [0i64; 3];
    for k in 0..3 {
        to_center[k] = sub(e, center[k], origin[k]);
    }
    let shift = dot(e, &to_center, &direction);
    let mut moved = [0i64; 3];
    for k in 0..3 {
        let m = mul(e, direction[k], shift);
        moved[k] = add(e, origin[k], m);
    }
    let origin = moved;
    let tmin = sub(e, ray[6], shift);
    let tmax = sub(e, ray[7], shift);
    let mut ba = [0i64; 3];
    for k in 0..3 {
        ba[k] = sub(e, b[k], a[k]);
    }
    let mut oa = [0i64; 3];
    for k in 0..3 {
        oa[k] = sub(e, origin[k], a[k]);
    }
    let mut ob = [0i64; 3];
    for k in 0..3 {
        ob[k] = sub(e, origin[k], b[k]);
    }
    let baba = dot(e, &ba, &ba);
    let bard = dot(e, &ba, &direction);
    let baoa = dot(e, &ba, &oa);
    let bb = mul(e, bard, bard);
    let qa = sub(e, baba, bb);
    let d_oa = dot(e, &direction, &oa);
    let m1 = mul(e, baba, d_oa);
    let m2 = mul(e, baoa, bard);
    let qb = sub(e, m1, m2);
    // sub (sub (mul baba (dot oa oa)) (mul baoa baoa)) (mul (mul radius radius) baba)
    let ooa = dot(e, &oa, &oa);
    let m_ba = mul(e, baba, ooa);
    let m_bb = mul(e, baoa, baoa);
    let s = sub(e, m_ba, m_bb);
    let rr = mul(e, radius, radius);
    let rrb = mul(e, rr, baba);
    let qc = sub(e, s, rrb);
    let qbqb = mul(e, qb, qb);
    let qaqc = mul(e, qa, qc);
    let discriminant = sub(e, qbqb, qaqc);
    let root = sqrt(e, discriminant);
    let zero = e.f(0.0);
    let c = e.compare(186, qa, zero);
    let one = e.f(1.0);
    let safe_qa = e.select_f(c, qa, one);
    let nqb = e.floating(127, &[qb]);
    let n0 = sub(e, nqb, root);
    let t0 = div(e, n0, safe_qa);
    let nqb2 = e.floating(127, &[qb]);
    let p0 = add(e, nqb2, root);
    let t1 = div(e, p0, safe_qa);
    let m = mul(e, t0, bard);
    let y0 = add(e, baoa, m);
    let m = mul(e, t1, bard);
    let y1 = add(e, baoa, m);
    let within = |e: &mut Emitter, t: i64| {
        let ge = e.compare(190, t, tmin);
        let le = e.compare(188, t, tmax);
        logical_and(e, ge, le)
    };
    let nearer = |e: &mut Emitter, best: i64, t: i64, valid: i64| -> i64 {
        let w = within(e, t);
        let valid = logical_and(e, valid, w);
        let lt = e.compare(184, t, best);
        let valid = logical_and(e, valid, lt);
        e.select_f(valid, t, best)
    };
    let c186 = e.compare(186, qa, zero);
    let c190 = e.compare(190, discriminant, zero);
    let side = logical_and(e, c186, c190);
    let c = e.compare(186, y0, zero);
    let lt = e.compare(184, y0, baba);
    let inner = logical_and(e, c, lt);
    let side0 = logical_and(e, side, inner);
    let c = e.compare(186, y1, zero);
    let lt = e.compare(184, y1, baba);
    let inner = logical_and(e, c, lt);
    let side1 = logical_and(e, side, inner);
    let inf = e.f(f64::INFINITY);
    let n1 = nearer(e, inf, t0, side0);
    let side_best = nearer(e, n1, t1, side1);
    let ha = dot(e, &oa, &direction);
    let r2 = mul(e, radius, radius);
    let ooa = dot(e, &oa, &oa);
    let d = sub(e, ooa, r2);
    let hh = mul(e, ha, ha);
    let cap_a = sub(e, hh, d);
    let root_a = sqrt(e, cap_a);
    let nha = e.floating(127, &[ha]);
    let ta0 = sub(e, nha, root_a);
    let nha2 = e.floating(127, &[ha]);
    let ta1 = add(e, nha2, root_a);
    let cap_a_ok = e.compare(190, cap_a, zero);
    let m = mul(e, ta0, bard);
    let s0 = add(e, baoa, m);
    let c0 = e.compare(188, s0, zero);
    let a0 = logical_and(e, cap_a_ok, c0);
    let m = mul(e, ta1, bard);
    let s1 = add(e, baoa, m);
    let c1 = e.compare(188, s1, zero);
    let a1 = logical_and(e, cap_a_ok, c1);
    let n = nearer(e, side_best, ta0, a0);
    let a_best = nearer(e, n, ta1, a1);
    let hb = dot(e, &ob, &direction);
    let r2 = mul(e, radius, radius);
    let oob = dot(e, &ob, &ob);
    let d = sub(e, oob, r2);
    let hbh = mul(e, hb, hb);
    let cap_b = sub(e, hbh, d);
    let root_b = sqrt(e, cap_b);
    let nhb = e.floating(127, &[hb]);
    let tb0 = sub(e, nhb, root_b);
    let nhb2 = e.floating(127, &[hb]);
    let tb1 = add(e, nhb2, root_b);
    let cap_b_ok = e.compare(190, cap_b, zero);
    let m = mul(e, tb0, bard);
    let s0 = add(e, baoa, m);
    let c0 = e.compare(190, s0, baba);
    let b0 = logical_and(e, cap_b_ok, c0);
    let m = mul(e, tb1, bard);
    let s1 = add(e, baoa, m);
    let c1 = e.compare(190, s1, baba);
    let b1 = logical_and(e, cap_b_ok, c1);
    let n = nearer(e, a_best, tb0, b0);
    let best = nearer(e, n, tb1, b1);
    let inf2 = e.f(f64::INFINITY);
    let hit = e.compare(184, best, inf2);
    let safe_best = e.select_f(hit, best, zero);
    let m = mul(e, safe_best, bard);
    let sum = add(e, baoa, m);
    let c = e.compare(186, baba, zero);
    let safe_baba = e.select_f(c, baba, one);
    let q = div(e, sum, safe_baba);
    let parameter = e.ext(43, &[q, zero, one]);
    let shifted = add(e, best, shift);
    let distance = e.select_f(hit, shifted, zero);
    let _ = (float, boolean);
    (hit, distance, parameter)
}

fn curves_query(e: &mut Emitter, ray: &[i64], any: bool) -> Vec<i64> {
    let mut key = vec![if any { -4 } else { -3 }];
    key.extend_from_slice(ray);
    let block = e.block;
    if let Some((_, _, values)) = e.trace_cache.iter().find(|(k, b, _)| *k == key && *b == block) {
        return values.clone();
    }
    let scene = e.scene.clone().expect("a scene");
    let query = scene.curve_query.expect("a curve query");
    ray_query_init(e, &scene, query, scene.curve_accel.expect("a curve structure"), 0, sub3(ray, 0), ray[6], sub3(ray, 3), ray[7]);
    ray_query_loop(e, query, &mut |e: &mut Emitter| {
        let uint = e.uint;
        let z = e.u(0);
        let kind = e.result(uint, 4479, &[query, z]);
        let one = e.u(1);
        let c = e.compare(170, kind, one);
        e.conditional(
            uint,
            c,
            &mut |e: &mut Emitter| {
                let uint = e.uint;
                let z = e.u(0);
                let primitive = e.result(uint, 6023, &[query, z]);
                let (hit, distance, _) = capsule_intersection(e, ray, primitive);
                e.conditional(
                    uint,
                    hit,
                    &mut |e: &mut Emitter| {
                        e.body(4475, &[query, distance]);
                        e.u(1)
                    },
                    &mut |e: &mut Emitter| e.u(0),
                )
            },
            &mut |e: &mut Emitter| e.u(0),
        );
    });
    let uint = e.uint;
    let float = e.float;
    let one = e.u(1);
    let committed = e.result(uint, 4479, &[query, one]);
    let two = e.u(2);
    let hit = e.compare(170, committed, two);
    let raw_primitive = e.result(uint, 6023, &[query, one]);
    let z = e.u(0);
    let primitive = e.select_u(hit, raw_primitive, z);
    let (_test, _, parameter) = capsule_intersection(e, ray, primitive);
    let values = if any {
        vec![hit]
    } else {
        let t = e.result(float, 6018, &[query, one]);
        let zf = e.f(0.0);
        let first = e.select_f(hit, t, zf);
        vec![first, primitive, parameter, hit]
    };
    e.trace_cache.push((key, e.block, values.clone()));
    values
}

fn query(e: &mut Emitter, r: &[i64], any: bool) -> Vec<i64> {
    let mut key = vec![if any { -1 } else { -2 }];
    key.extend_from_slice(r);
    let block = e.block;
    if let Some((_, _, values)) = e.trace_cache.iter().find(|(k, b, _)| *k == key && *b == block) {
        return values.clone();
    }
    let scene = e.scene.clone().expect("a scene");
    let values = if scene.motion {
        if scene.instances.is_none() {
            moving_triangles_query(e, &scene, r, any)
        } else {
            moving_instances_query(e, &scene, r, any)
        }
    } else {
        let query = scene.query;
        ray_query_init(e, &scene, query, scene.accel, if any { 5 } else { 1 }, sub3(r, 0), r[6], sub3(r, 3), r[7]);
        let boolean = e.boolean;
        let uint = e.uint;
        let float = e.float;
        e.result(boolean, 4477, &[query]);
        let one = e.u(1);
        let committed = e.result(uint, 4479, &[query, one]);
        let hit = e.compare(170, committed, one);
        if any {
            vec![hit]
        } else {
            let barycentrics = e.result(scene.vec2, 6024, &[query, one]);
            let t = e.result(float, 6018, &[query, one]);
            let p = e.result(uint, 6023, &[query, one]);
            let u_ = e.result(float, 81, &[barycentrics, 0]);
            let v = e.result(float, 81, &[barycentrics, 1]);
            let instance = e.result(uint, 6020, &[query, one]);
            vec![t, p, u_, v, hit, instance]
        }
    };
    e.trace_cache.push((key, e.block, values.clone()));
    values
}

/// A narrow (32-bit unsigned) register's operation on the operands' low words.
fn narrow_binary(e: &mut Emitter, operation: Integer, a: i64, b: i64) -> i64 {
    let uint = e.uint;
    match operation {
        Integer::Iadd => e.result(uint, 128, &[a, b]),
        Integer::Isub => e.result(uint, 130, &[a, b]),
        Integer::Imul => e.result(uint, 132, &[a, b]),
        Integer::Iand => e.result(uint, 199, &[a, b]),
        Integer::Ior => e.result(uint, 197, &[a, b]),
        Integer::Ixor => e.result(uint, 198, &[a, b]),
        Integer::Ishl | Integer::Ishr => {
            let c63 = e.u(63);
            let k = e.result(uint, 199, &[b, c63]);
            let c31 = e.u(31);
            let kk = e.result(uint, 199, &[k, c31]);
            let shifted = e.result(uint, if operation == Integer::Ishl { 196 } else { 194 }, &[a, kk]);
            let c = e.compare(172, k, c31);
            let z = e.u(0);
            e.select_u(c, z, shifted)
        }
        Integer::Idiv | Integer::Imod => {
            let z = e.u(0);
            let zero = e.compare(170, b, z);
            let one = e.u(1);
            let safe = e.select_u(zero, one, b);
            let value = e.result(uint, if operation == Integer::Idiv { 134 } else { 137 }, &[a, safe]);
            e.select_u(zero, z, value)
        }
    }
}

/// A raster-stage intrinsic's operands: floats, but a store's table (its array uniform), a store's
/// index and Int value and a group scan's operand as uint words.
pub struct StageState {
    spec: StageSpec,
    inputs: Vec<i64>,
    vertex_index: Option<i64>,
    outputs: Vec<i64>,
    buffers: Vec<(i64, (i64, bool))>,
    image: i64,
    sampler: i64,
    sampled: i64,
    vec2: i64,
    vec4: i64,
    textures: HashMap<i64, i64>,
    sizes: bool,
    builtins: HashMap<i64, i64>,
    discard: Option<i64>,
    scans: HashMap<i64, [i64; 2]>,
    types: HashMap<Vec<i64>, i64>,
    interface: Vec<i64>,
    output_widths: Vec<usize>,
}

impl StageState {
    /// A type is declared once (SPIR-V refuses duplicate non-aggregate types).
    fn typ(&mut self, e: &mut Emitter, opcode: i64, operands: &[i64]) -> i64 {
        let mut key = vec![opcode];
        key.extend_from_slice(operands);
        if let Some(id) = self.types.get(&key) {
            return *id;
        }
        let id = e.words.declare(opcode, operands);
        self.types.insert(key, id);
        id
    }
    fn vector(&mut self, e: &mut Emitter, n: i64) -> i64 {
        if n == 1 {
            e.float
        } else {
            let f = e.float;
            self.typ(e, 23, &[f, n])
        }
    }
    fn pointer(&mut self, e: &mut Emitter, storage: i64, ty: i64) -> i64 {
        self.typ(e, 32, &[storage, ty])
    }
    fn variable(&mut self, e: &mut Emitter, ty: i64, storage: i64) -> i64 {
        let id = e.words.id();
        instruction(&mut e.words.declarations, 59, &[ty, id, storage]);
        id
    }
    fn io(&mut self, e: &mut Emitter, builtin: i64, storage: i64, width: i64, location: Option<i64>, flat: bool) -> i64 {
        let v = self.vector(e, width);
        let p = self.pointer(e, storage, v);
        let id = self.variable(e, p, storage);
        match location {
            Some(l) => e.annotate(71, &[id, 30, l]),
            None => e.annotate(71, &[id, 11, builtin]),
        }
        if flat {
            e.annotate(71, &[id, 14]);
        }
        self.interface.push(id);
        id
    }
    fn texture(&mut self, e: &mut Emitter, t: i64) -> i64 {
        if let Some(id) = self.textures.get(&t) {
            return *id;
        }
        let p = self.pointer(e, 0, self.image);
        let id = self.variable(e, p, 0);
        e.annotate(71, &[id, 34, 0]);
        e.annotate(71, &[id, 33, t]);
        self.textures.insert(t, id);
        id
    }
    fn sampler_of(&mut self, e: &mut Emitter, t: i64) -> i64 {
        if let Some(id) = self.textures.get(&(-1 - t)) {
            return *id;
        }
        let p = self.pointer(e, 0, self.sampler);
        let id = self.variable(e, p, 0);
        e.annotate(71, &[id, 34, 0]);
        e.annotate(71, &[id, 33, t + 1]);
        self.textures.insert(-1 - t, id);
        id
    }
    fn to_long(&mut self, e: &mut Emitter, value: i64) -> i64 {
        let ulong = e.ulong();
        let long = e.long();
        let w = e.result(ulong, 113, &[value]);
        e.result(long, 124, &[w])
    }
    fn builtin_variable(&mut self, e: &mut Emitter, ty: i64, decoration: i64) -> i64 {
        if let Some(id) = self.builtins.get(&decoration) {
            return *id;
        }
        let p = self.pointer(e, 1, ty);
        let id = self.variable(e, p, 1);
        e.annotate(71, &[id, 11, decoration]);
        self.interface.push(id);
        self.builtins.insert(decoration, id);
        id
    }
    /// A group scan of x over the workgroup: Hillis-Steele in Workgroup memory between barriers, once a
    /// scanned value; the exclusive prefix and the total.
    fn scan(&mut self, e: &mut Emitter, x: i64) -> [i64; 2] {
        if let Some(values) = self.scans.get(&x) {
            return *values;
        }
        let n = self.spec.workgroup.expect("a workgroup size");
        let uint = e.uint;
        let boolean = e.boolean;
        let un = e.u(n);
        let array = self.typ(e, 28, &[uint, un]);
        let p = self.pointer(e, 4, array);
        let shared = self.variable(e, p, 4);
        let b29 = self.builtin_variable(e, uint, 29);
        let lid = e.result(uint, 61, &[b29]);
        fn at(st: &mut StageState, e: &mut Emitter, shared: i64, index: i64) -> i64 {
            let uint = e.uint;
            let p = st.pointer(e, 4, uint);
            e.result(p, 65, &[shared, index])
        }
        fn barrier(e: &mut Emitter) {
            let c2 = e.u(2);
            let c108 = e.u(0x108);
            e.body(224, &[c2, c2, c108]);
        }
        let a = at(self, e, shared, lid);
        e.body(62, &[a, x]);
        let mut d = 1;
        let mut v = x;
        while d < n {
            barrier(e);
            let ud = e.u(d);
            let reach = e.result(boolean, 174, &[lid, ud]);
            let sub = e.result(uint, 130, &[lid, ud]);
            let z = e.u(0);
            let s = e.select_u(reach, sub, z);
            let a = at(self, e, shared, s);
            let w = e.result(uint, 61, &[a]);
            barrier(e);
            let z = e.u(0);
            let s = e.select_u(reach, w, z);
            v = e.result(uint, 128, &[v, s]);
            let a = at(self, e, shared, lid);
            e.body(62, &[a, v]);
            d *= 2;
        }
        barrier(e);
        let last = e.u(n - 1);
        let a = at(self, e, shared, last);
        let total = e.result(uint, 61, &[a]);
        let before = e.result(uint, 130, &[v, x]);
        let values = [before, total];
        self.scans.insert(x, values);
        values
    }

    fn input(&mut self, e: &mut Emitter, k: i64, c: i64) -> i64 {
        let (_, width, _) = self.spec.inputs[k as usize];
        let vt = self.vector(e, width);
        let value = e.result(vt, 61, &[self.inputs[k as usize]]);
        if width == 1 {
            value
        } else {
            let float = e.float;
            e.result(float, 81, &[value, c])
        }
    }
    fn int_input(&mut self, e: &mut Emitter) -> i64 {
        let uint = e.uint;
        let value = match self.vertex_index {
            Some(id) => e.result(uint, 61, &[id]),
            None if self.spec.grid => {
                let v3 = self.typ(e, 23, &[uint, 3]);
                let b = self.builtin_variable(e, v3, 28);
                let loaded = e.result(v3, 61, &[b]);
                e.result(uint, 81, &[loaded, 0])
            }
            None => {
                let b = self.builtin_variable(e, uint, 29);
                e.result(uint, 61, &[b])
            }
        };
        self.to_long(e, value)
    }
    fn gather(&mut self, e: &mut Emitter, u: i64, at: i64, width: i64, component: i64) -> i64 {
        let (var, w) = self.buffers.iter().find(|(k, _)| *k == u).expect("a bound buffer").1;
        let offset = e.offset(at, width, component);
        if w {
            let l = e.load_uint(var, offset);
            let float = e.float;
            e.result(float, 124, &[l])
        } else {
            e.load(var, offset)
        }
    }
    fn int_gather(&mut self, e: &mut Emitter, u: i64, at: i64) -> i64 {
        let var = self.buffers.iter().find(|(k, _)| *k == u).expect("a bound buffer").1 .0;
        let l = e.load_uint(var, at);
        self.to_long(e, l)
    }
    fn intrinsic(&mut self, e: &mut Emitter, stage: &Stage, operands: &[i64]) -> i64 {
        let float = e.float;
        match stage {
            Stage::Fwidth => e.result(float, 209, &[operands[0]]),
            Stage::Float_bits => e.result(float, 124, &[operands[0]]),
            Stage::Sample(t, c) | Stage::Sample_level(t, c) => {
                let tx = self.texture(e, *t);
                let li = e.result(self.image, 61, &[tx]);
                let s = self.sampler_of(e, *t);
                let ls = e.result(self.sampler, 61, &[s]);
                let combined = e.result(self.sampled, 86, &[li, ls]);
                let uv = e.result(self.vec2, 80, &[operands[0], operands[1]]);
                let texel = match stage {
                    Stage::Sample_level(..) => e.result(self.vec4, 88, &[combined, uv, 2, operands[2]]),
                    _ => e.result(self.vec4, 87, &[combined, uv]),
                };
                e.result(float, 81, &[texel, *c])
            }
            Stage::Read(t, c) => {
                let sint = e.sint;
                let ivec2 = self.typ(e, 23, &[sint, 2]);
                let x = e.result(sint, 110, &[operands[0]]);
                let y = e.result(sint, 110, &[operands[1]]);
                let at = e.result(ivec2, 80, &[x, y]);
                let tx = self.texture(e, *t);
                let li = e.result(self.image, 61, &[tx]);
                let texel = e.result(self.vec4, 95, &[li, at]);
                e.result(float, 81, &[texel, *c])
            }
            Stage::Texture_size(t, c) => {
                self.sizes = true;
                let sint = e.sint;
                let i = self.texture(e, *t);
                let li = e.result(self.image, 61, &[i]);
                let zero = e.words.constant(sint, 0);
                let ivec2 = self.typ(e, 23, &[sint, 2]);
                let size = e.result(ivec2, 103, &[li, zero]);
                let extracted = e.result(sint, 81, &[size, *c]);
                e.result(float, 111, &[extracted])
            }
            Stage::Group_scan(field) => self.scan(e, operands[0])[*field as usize],
            Stage::Store => {
                let (var, w) = self.buffers.iter().find(|(k, _)| *k == operands[0]).expect("a bound buffer").1;
                if w {
                    e.store_uint(var, operands[1], operands[2]);
                } else {
                    e.store(var, operands[1], operands[2]);
                }
                e.u(1)
            }
            _ => e.fail("A ray query or scene table read runs only in a compute kernel."),
        }
    }
    fn output(&mut self, e: &mut Emitter, k: i64, c: i64, value: i64) {
        if self.spec.workgroup.is_none() {
            match self.spec.outputs[k as usize] {
                Output::Discard => self.discard = Some(value),
                _ => {
                    let width = self.output_widths[k as usize];
                    let target = if width == 1 {
                        self.outputs[k as usize]
                    } else {
                        let uc = e.u(c);
                        let float = e.float;
                        let p = self.pointer(e, 3, float);
                        e.result(p, 65, &[self.outputs[k as usize], uc])
                    };
                    e.body(62, &[target, value]);
                }
            }
        }
    }
    /// instance id (a long, as an Int frame fact), facing (1.0 or 0.0) and FragCoord
    fn builtin(&mut self, e: &mut Emitter, name: &str) -> i64 {
        match name {
            "gpu/instance-id" => {
                let uint = e.uint;
                let b = self.builtin_variable(e, uint, 43);
                let loaded = e.result(uint, 61, &[b]);
                self.to_long(e, loaded)
            }
            "gpu/front-facing" => {
                let boolean = e.boolean;
                let b = self.builtin_variable(e, boolean, 17);
                let loaded = e.result(boolean, 61, &[b]);
                e.numeric_truth(loaded)
            }
            "gpu/fragment-x" | "gpu/fragment-y" | "gpu/depth" => {
                let c = match name {
                    "gpu/fragment-x" => 0,
                    "gpu/fragment-y" => 1,
                    _ => 2,
                };
                let v4 = self.vector(e, 4);
                let b = self.builtin_variable(e, v4, 15);
                let loaded = e.result(v4, 61, &[b]);
                let float = e.float;
                e.result(float, 81, &[loaded, c])
            }
            name => e.fail(&format!("no raster built-in {name}")),
        }
    }
}


struct EP {
    e: Emitter,
    layout: Arc<kernel_layout::T>,
    raster: Option<StageState>,
    carried: Option<Vec<i64>>,
    input_uint32: Vec<bool>,
    gather_uint32: Vec<bool>,
    uint32_words: Vec<i64>,
    output_uint32: Vec<bool>,
    inputs: Vec<i64>,
    outputs: Vec<i64>,
    gathers: Vec<i64>,
    index: i64,
    registers: Vec<i64>,
    whole: Vec<Option<i64>>,
    invalid: i64,
}

impl EP {
    fn integer(&self, slot: i64) -> bool {
        region::integer_register(&self.layout.view.code[slot as usize])
    }
    fn narrow(&self, slot: i64) -> bool {
        self.layout.narrow[slot as usize]
    }
    fn known(&self, slot: i64) -> bool {
        self.narrow(slot) || self.whole[slot as usize].is_some()
    }
    fn is_uint32(&self, words: i64) -> bool {
        self.uint32_words.contains(&words)
    }
    fn widen_word(&mut self, word: i64) -> i64 {
        let ulong = self.e.ulong();
        let long = self.e.long();
        if self.e.int32 {
            return self.e.result(long, 124, &[word]);
        }
        let w = self.e.result(ulong, 113, &[word]);
        self.e.result(long, 124, &[w])
    }
    fn widen(&mut self, slot: i64) -> i64 {
        let r = self.registers[slot as usize];
        self.widen_word(r)
    }
    fn whole_of(&mut self, slot: i64) -> i64 {
        if self.narrow(slot) {
            self.widen(slot)
        } else {
            self.whole[slot as usize].expect("a known whole value")
        }
    }
    fn float_of(&mut self, slot: i64) -> i64 {
        if self.narrow(slot) {
            let w = self.widen(slot);
            self.e.from_long(w)
        } else if self.integer(slot) {
            let r = self.registers[slot as usize];
            self.e.from_long(r)
        } else {
            self.registers[slot as usize]
        }
    }
    fn to_int(&mut self, value: i64) -> i64 {
        let boolean = self.e.boolean;
        let lo = self.e.f(-9223372036854775808.0);
        let c190 = self.e.compare(190, value, lo);
        let hi = self.e.f(9223372036854775808.0);
        let c184 = self.e.compare(184, value, hi);
        let valid = self.e.result(boolean, 167, &[c190, c184]);
        let not_valid = self.e.result(boolean, 168, &[valid]);
        self.mark_invalid(not_valid);
        let zero = self.e.f(0.0);
        let float = self.e.float;
        let s = self.e.select(float, valid, value, zero);
        let long = self.e.long();
        if self.e.int32 {
            // Emit's `rays_int`: saturated to the 32-bit range
            let lo = self.e.f(-2147483648.0);
            let hi = self.e.f(2147483520.0);
            let clamped = self.e.ext(43, &[s, lo, hi]);
            return self.e.result(long, 110, &[clamped]);
        }
        self.e.result(long, 110, &[s])
    }
    fn mark_invalid(&mut self, condition: i64) {
        let mut invalid = self.invalid;
        self.e.mark_invalid(&mut invalid, condition);
        self.invalid = invalid;
    }
    fn table_of(&self, prepared: i64) -> i64 {
        match unguarded(&self.layout.view.code[prepared as usize]) {
            Instruction::Array_prepare(u, _) => *u,
            _ => panic!("a table register"),
        }
    }
    fn int_of(&mut self, slot: i64) -> i64 {
        if self.narrow(slot) {
            self.widen(slot)
        } else if self.integer(slot) {
            self.registers[slot as usize]
        } else {
            match self.whole[slot as usize] {
                Some(value) => value,
                None => {
                    let f = self.float_of(slot);
                    self.to_int(f)
                }
            }
        }
    }
    fn low(&mut self, slot: i64) -> i64 {
        if self.narrow(slot) {
            self.registers[slot as usize]
        } else {
            let i = self.int_of(slot);
            let uint = self.e.uint;
            self.e.result(uint, if self.e.int32 { 124 } else { 113 }, &[i])
        }
    }
    fn guard_condition(&mut self, guards: &[(i64, bool)]) -> Option<i64> {
        let mut combined: Option<i64> = None;
        let mut values = Vec::new();
        for (slot, selected) in guards {
            let f = self.float_of(*slot);
            let value = self.e.truth(f);
            values.push(if *selected {
                value
            } else {
                let boolean = self.e.boolean;
                self.e.result(boolean, 168, &[value])
            });
        }
        for value in values {
            combined = Some(match combined {
                None => value,
                Some(left) => {
                    let boolean = self.e.boolean;
                    self.e.result(boolean, 167, &[left, value])
                }
            });
        }
        combined
    }
    fn gather_k(&self, prepared: i64) -> usize {
        self.layout.gathers.iter().position(|(slot, _)| *slot == prepared).expect("a gather table")
    }
    /// One component of a prepared table's row `at` (a long), clamped into the table.
    fn element_word(&mut self, prepared: i64, at: i64, width: i64, component: i64) -> i64 {
        // the index is evaluated before the table row is read
        let at = if self.narrow(at) {
            let r = self.registers[at as usize];
            let sint = self.e.sint;
            let long = self.e.long();
            let s = self.e.result(sint, 124, &[r]);
            if self.e.int32 { s } else { self.e.result(long, 114, &[s]) }
        } else {
            self.int_of(at)
        };
        let k = self.gather_k(prepared);
        let word = self.e.uniform(self.layout.gather_lengths[k]);
        let size = self.widen_word(word);
        let one = self.e.l(1);
        let last = integer_binary(&mut self.e, Integer::Isub, size, one);
        let zero = self.e.l(0);
        let c = self.e.compare(177, last, zero);
        let last = self.e.select_l(c, zero, last);
        let c = self.e.compare(177, at, zero);
        let at = self.e.select_l(c, zero, at);
        let c = self.e.compare(173, at, last);
        let at = self.e.select_l(c, last, at);
        let uint = self.e.uint;
        let at = self.e.result(uint, if self.e.int32 { 124 } else { 113 }, &[at]);
        let offset = self.e.offset(at, width, component);
        self.e.load_uint(self.gathers[k], offset)
    }
    fn element(&mut self, prepared: i64, at: i64, width: i64, component: i64) -> i64 {
        let word = self.element_word(prepared, at, width, component);
        let k = self.gather_k(prepared);
        let float = self.e.float;
        self.e.result(float, if self.gather_uint32[k] { 112 } else { 124 }, &[word])
    }
    fn noise_table(&mut self, seed: i64, index: i64) -> i64 {
        let base = self.layout.table_seeds.iter().position(|s| *s == seed).expect("a noise table") as i64 * 512;
        let ub = self.e.u(base);
        let uint = self.e.uint;
        let at = self.e.result(uint, 128, &[ub, index]);
        let table = self.e.table.expect("a table binding");
        self.e.load_uint(table, at)
    }
    fn scene_count(&mut self, name: &str, wide: bool) -> i64 {
        let word = if name == "gpu/scene-spheres" { self.layout.scene_spheres } else { self.layout.scene_strand_count };
        let w = self.e.uniform(word.expect("a scene count word"));
        if wide {
            self.widen_word(w)
        } else {
            w
        }
    }
    fn frame_word(&mut self, name: &str) -> i64 {
        if self.raster.is_some() {
            self.raster_builtin(name)
        } else {
            self.scene_count(name, false)
        }
    }
    fn raster_builtin(&mut self, name: &str) -> i64 {
        let st = self.raster.as_mut().expect("a raster placement");
        st.builtin(&mut self.e, name)
    }
    fn trace_stage(&mut self, stage: &Stage, r: &[i64], wide: bool) -> i64 {
        match stage {
            Stage::Trace_triangles(k) => {
                let floats: Vec<i64> = r.iter().map(|x| self.float_of(*x)).collect();
                let value = query(&mut self.e, &floats, false)[*k as usize];
                if *k == 4 {
                    self.e.numeric_truth(value)
                } else if wide && (*k == 1 || *k == 5) {
                    self.widen_word(value)
                } else {
                    value
                }
            }
            Stage::Trace_any => {
                let floats: Vec<i64> = r.iter().map(|x| self.float_of(*x)).collect();
                let v = query(&mut self.e, &floats, true)[0];
                self.e.numeric_truth(v)
            }
            Stage::Trace_curves(k) => {
                let floats: Vec<i64> = r.iter().map(|x| self.float_of(*x)).collect();
                let value = curves_query(&mut self.e, &floats, false)[*k as usize];
                if *k == 3 {
                    self.e.numeric_truth(value)
                } else if wide && *k == 1 {
                    self.widen_word(value)
                } else {
                    value
                }
            }
            Stage::Trace_curves_any => {
                let floats: Vec<i64> = r.iter().map(|x| self.float_of(*x)).collect();
                let v = curves_query(&mut self.e, &floats, true)[0];
                self.e.numeric_truth(v)
            }
            stage => {
                let words: Vec<i64> = r.iter().map(|x| self.low(*x)).collect();
                self.scene_read(stage, &words)
            }
        }
    }
    /// A scene table read at uint indices: a triangle corner through the instance's vertex base, an
    /// instance-table word, a sphere or a strand end component.
    fn scene_read(&mut self, stage: &Stage, index: &[i64]) -> i64 {
        let e = &mut self.e;
        let scene = e.scene.clone().expect("a scene");
        let uint = e.uint;
        fn add(e: &mut Emitter, a: i64, b: i64) -> i64 {
            let uint = e.uint;
            e.result(uint, 128, &[a, b])
        }
        fn times(e: &mut Emitter, a: i64, k: i64) -> i64 {
            let uint = e.uint;
            let c = e.u(k);
            e.result(uint, 132, &[a, c])
        }
        let stride = if scene.motion { 32 } else { 16 };
        match stage {
            Stage::Scene_vertex(ending, c) => {
                let base = match scene.instances {
                    None => index[1],
                    Some(table) => {
                        let t = times(e, index[0], stride);
                        let l = e.load(table, t);
                        let converted = e.result(uint, 109, &[l]);
                        add(e, converted, index[1])
                    }
                };
                let vertices = if *ending { scene.vertices_end.expect("end vertices") } else { scene.vertices };
                let t1 = times(e, base, 3);
                let a = add(e, t1, index[2]);
                let t2 = times(e, a, 3);
                let uc = e.u(*c);
                let at = add(e, t2, uc);
                e.load(vertices, at)
            }
            Stage::Scene_instance => {
                let t = times(e, index[0], stride);
                let at = add(e, t, index[1]);
                e.load(scene.instances.expect("an instance table"), at)
            }
            Stage::Scene_sphere(c) => {
                let t = times(e, index[0], 4);
                let uc = e.u(*c);
                let at = add(e, t, uc);
                e.load(scene.spheres, at)
            }
            Stage::Scene_strand(c) => {
                let t1 = times(e, index[0], 2);
                let a = add(e, t1, index[1]);
                let t2 = times(e, a, 4);
                let uc = e.u(*c);
                let at = add(e, t2, uc);
                e.load(e.strands.expect("strand segments"), at)
            }
            _ => e.fail("scene/trace native curves have no SPIR-V ray-query form."),
        }
    }
    fn integer_status(&mut self, operation: Integer, b: i64) {
        match operation {
            Integer::Idiv | Integer::Imod => {
                let z = self.e.l(0);
                let c = self.e.compare(170, b, z);
                self.e.record_status(c, 2);
            }
            Integer::Ishl | Integer::Ishr => {
                let boolean = self.e.boolean;
                let z = self.e.l(0);
                let lo = self.e.compare(177, b, z);
                let c63 = self.e.l(63);
                let hi = self.e.compare(173, b, c63);
                let c = self.e.result(boolean, 166, &[lo, hi]);
                self.e.record_status(c, 4);
            }
            _ => {}
        }
    }
    /// A raster-stage intrinsic's operands.
    fn stage_operands(&mut self, stage: &Stage, a: &[i64]) -> Vec<i64> {
        let mut out = Vec::new();
        for (k, x) in a.iter().enumerate() {
            out.push(match stage {
                Stage::Store if k == 0 => self.table_of(*x),
                Stage::Store | Stage::Group_scan(_) if self.integer(*x) => self.low(*x),
                Stage::Float_bits => self.low(*x),
                _ => self.float_of(*x),
            });
        }
        out
    }
    fn stage_value(&mut self, stage: &Stage, r: &[i64], wide: bool) -> i64 {
        if self.raster.is_some() {
            let operands = self.stage_operands(stage, r);
            let st = self.raster.as_mut().expect("a raster placement");
            st.intrinsic(&mut self.e, stage, &operands)
        } else {
            self.trace_stage(stage, r, wide)
        }
    }

    fn expression(&mut self, slot: i64, instruction: &Instruction) -> i64 {
        if self.narrow(slot) {
            match instruction {
                Instruction::Int_const(n) => self.e.u(n & 0xffff_ffff),
                Instruction::Raise(_, code, message, _) => {
                    if self.raster.is_some() {
                        return self.e.fail("Explicit errors need a compute status record.");
                    }
                    let one = self.e.f(1.0);
                    let t = self.e.truth(one);
                    self.e.record_status(t, packed_ops::error_status(code, message) as i64);
                    self.e.u(0)
                }
                Instruction::Int_binary(op, a, b) => {
                    let ib = self.int_of(*b);
                    self.integer_status(*op, ib);
                    let la = self.low(*a);
                    let lb = self.low(*b);
                    narrow_binary(&mut self.e, *op, la, lb)
                }
                Instruction::Int_select(condition, yes, no) => {
                    if yes == no {
                        self.low(*yes)
                    } else {
                        let f = self.float_of(*condition);
                        let t = self.e.truth(f);
                        let y = self.low(*yes);
                        let n = self.low(*no);
                        self.e.select_u(t, y, n)
                    }
                }
                Instruction::Loop_result(loop_, component, _) => self.registers[(loop_ + 1 + component) as usize],
                Instruction::Stage(Stage::Noise_table(seed), r) => {
                    let l = self.low(r[0]);
                    self.noise_table(*seed, l)
                }
                Instruction::Stage(stage, r) => self.stage_value(stage, r, false),
                Instruction::Int_frame(name) => self.frame_word(name),
                _ => panic!("narrow register without an integer form"),
            }
        } else {
            self.wide_expression(slot, instruction)
        }
    }

    fn wide_expression(&mut self, slot: i64, instruction: &Instruction) -> i64 {
        let raster = self.raster.is_some();
        let integer = self.integer(slot);
        match instruction {
            Instruction::Raise(_, code, message, _) => {
                if raster {
                    return self.e.fail("Explicit errors need a compute status record.");
                }
                let one = self.e.f(1.0);
                let t = self.e.truth(one);
                self.e.record_status(t, packed_ops::error_status(code, message) as i64);
                if integer {
                    self.e.l(0)
                } else {
                    self.e.f(0.0)
                }
            }
            Instruction::Stage(Stage::Noise_table(seed), r) => {
                let l = self.low(r[0]);
                let word = self.noise_table(*seed, l);
                self.widen_word(word)
            }
            Instruction::Stage(stage, r) if !raster => self.stage_value(stage, r, integer),
            Instruction::Int_frame(name) if kernel_layout::scene_fact(name) => self.scene_count(name, true),
            Instruction::Const(value) => {
                if self.e.long.is_some() && value.is_finite() && value.fract() == 0.0 && value.abs() < if self.e.int32 { 2147483648.0 } else { 9007199254740992.0 } {
                    self.whole[slot as usize] = Some(self.e.l(*value as i64));
                }
                self.e.f(*value)
            }
            Instruction::Input(..) | Instruction::Int_input(_) | Instruction::Gather(..) | Instruction::Int_gather(..) | Instruction::Stage(..) if raster => match instruction {
                Instruction::Input(k, _, c) => {
                    let st = self.raster.as_mut().unwrap();
                    st.input(&mut self.e, *k, *c)
                }
                Instruction::Int_input(_) => {
                    let st = self.raster.as_mut().unwrap();
                    st.int_input(&mut self.e)
                }
                Instruction::Stage(s, a) => {
                    let operands = self.stage_operands(s, a);
                    let st = self.raster.as_mut().unwrap();
                    st.intrinsic(&mut self.e, s, &operands)
                }
                Instruction::Gather(p, at, w, c) => {
                    let l = self.low(*at);
                    let t = self.table_of(*p);
                    let st = self.raster.as_mut().unwrap();
                    st.gather(&mut self.e, t, l, *w, *c)
                }
                Instruction::Int_gather(p, at) => {
                    let l = self.low(*at);
                    let t = self.table_of(*p);
                    let st = self.raster.as_mut().unwrap();
                    st.int_gather(&mut self.e, t, l)
                }
                _ => unreachable!(),
            },
            Instruction::Input(0, _, component) if self.carried.is_some() => {
                // an unfolded kernel's input: the carried pass value
                let float = self.e.float;
                let var = self.carried.as_ref().unwrap()[*component as usize];
                self.e.result(float, 61, &[var])
            }
            Instruction::Input(input, width, component) if self.input_uint32[*input as usize] => {
                let offset = self.e.offset(self.index, *width, *component);
                let l = self.e.load_uint(self.inputs[*input as usize], offset);
                let float = self.e.float;
                self.e.result(float, 112, &[l])
            }
            Instruction::Input(input, width, component) => {
                let offset = self.e.offset(self.index, *width, *component);
                self.e.load(self.inputs[*input as usize], offset)
            }
            Instruction::Uniform(input, component) if self.is_uint32(self.layout.uniforms[*input as usize] + component) => {
                let w = self.e.uniform(self.layout.uniforms[*input as usize] + component);
                let float = self.e.float;
                self.e.result(float, 112, &[w])
            }
            Instruction::Uniform(input, component) => self.e.uniform_float(self.layout.uniforms[*input as usize] + component),
            Instruction::Frame(_) | Instruction::Guarded(..) => panic!("frames and guards are resolved before printing"),
            Instruction::Binary(op @ (Binary::Add | Binary::Sub | Binary::Mul | Binary::Mod | Binary::Min | Binary::Max), a, b) if self.known(*a) && self.known(*b) => {
                let wa = self.whole_of(*a);
                let wb = self.whole_of(*b);
                let value = integer_float_binary(&mut self.e, *op, wa, wb);
                self.whole[slot as usize] = Some(value);
                self.e.from_long(value)
            }
            Instruction::Binary(op, a, b) => {
                let fa = self.float_of(*a);
                let fb = self.float_of(*b);
                binary(&mut self.e, *op, fa, fb)
            }
            Instruction::Unary(op, a) => {
                let fa = self.float_of(*a);
                unary(&mut self.e, *op, fa)
            }
            Instruction::Fma(a, b, c) => {
                let fa = self.float_of(*a);
                let fb = self.float_of(*b);
                let fc = self.float_of(*c);
                self.e.ext(50, &[fa, fb, fc])
            }
            Instruction::Checked_mul(projection, a, b) | Instruction::Checked_fma(projection, a, b, _) => {
                let value = match instruction {
                    Instruction::Checked_fma(_, _, _, c) => {
                        let fa = self.float_of(*a);
                        let fb = self.float_of(*b);
                        let fc = self.float_of(*c);
                        self.e.ext(50, &[fa, fb, fc])
                    }
                    _ => {
                        let fa = self.float_of(*a);
                        let fb = self.float_of(*b);
                        self.e.floating(133, &[fa, fb])
                    }
                };
                let boolean = self.e.boolean;
                let nf = self.e.non_finite(value);
                let valid = self.e.result(boolean, 168, &[nf]);
                match projection {
                    Projection::Value => {
                        let z = self.e.f(0.0);
                        self.e.select_f(valid, value, z)
                    }
                    Projection::Valid => self.e.numeric_truth(valid),
                }
            }
            Instruction::Select(condition, yes, no) => {
                if yes == no {
                    self.float_of(*yes)
                } else {
                    let f = self.float_of(*condition);
                    let t = self.e.truth(f);
                    let y = self.float_of(*yes);
                    let n = self.float_of(*no);
                    self.e.select_f(t, y, n)
                }
            }
            Instruction::Loop_result(loop_, component, _) => {
                let at = (loop_ + 1 + component) as usize;
                self.whole[slot as usize] = self.whole[at];
                self.registers[at]
            }
            Instruction::Int_const(value) => self.e.l(*value),
            Instruction::Int_input(input) if self.input_uint32[*input as usize] => {
                let l = self.e.load_uint(self.inputs[*input as usize], self.index);
                self.widen_word(l)
            }
            Instruction::Int_input(input) => {
                let l = self.e.load(self.inputs[*input as usize], self.index);
                self.to_int(l)
            }
            Instruction::Int_uniform(input) if self.is_uint32(self.layout.uniforms[*input as usize]) => {
                let w = self.e.uniform(self.layout.uniforms[*input as usize]);
                self.widen_word(w)
            }
            Instruction::Int_uniform(input) => {
                let w = self.e.uniform_float(self.layout.uniforms[*input as usize]);
                self.to_int(w)
            }
            Instruction::Int_frame(name) if raster => self.raster_builtin(name),
            Instruction::Int_frame(_) if self.is_uint32(self.layout.frames[slot as usize]) => {
                let w = self.e.uniform(self.layout.frames[slot as usize]);
                self.widen_word(w)
            }
            Instruction::Int_frame(_) => {
                let w = self.e.uniform_float(self.layout.frames[slot as usize]);
                self.to_int(w)
            }
            Instruction::Int_binary(op, a, b) => {
                let a = self.int_of(*a);
                let b = self.int_of(*b);
                self.integer_status(*op, b);
                integer_binary(&mut self.e, *op, a, b)
            }
            Instruction::Int_compare(op, a, b) => {
                let ia = self.int_of(*a);
                let ib = self.int_of(*b);
                let c = self.e.compare(int_compare_opcode(*op), ia, ib);
                self.e.numeric_truth(c)
            }
            Instruction::Int_abs(a) => {
                let value = self.int_of(*a);
                let z = self.e.l(0);
                let c = self.e.compare(177, value, z);
                let long = self.e.long();
                let abs = self.e.result(long, 126, &[value]);
                self.e.select_l(c, abs, value)
            }
            Instruction::Int_select(condition, yes, no) => {
                if yes == no {
                    self.int_of(*yes)
                } else {
                    let f = self.float_of(*condition);
                    let t = self.e.truth(f);
                    let y = self.int_of(*yes);
                    let n = self.int_of(*no);
                    self.e.select_l(t, y, n)
                }
            }
            Instruction::Gather(prepared, at, width, component) => self.element(*prepared, *at, *width, *component),
            Instruction::Int_gather(prepared, at) if self.gather_uint32[self.gather_k(*prepared)] => {
                let w = self.element_word(*prepared, *at, 1, 0);
                self.widen_word(w)
            }
            Instruction::Int_gather(prepared, at) => {
                let f = self.element(*prepared, *at, 1, 0);
                self.to_int(f)
            }
            Instruction::Convert(Conversion::ToFloat, a) => {
                self.whole[slot as usize] = if self.known(*a) { Some(self.whole_of(*a)) } else { None };
                self.float_of(*a)
            }
            Instruction::Convert(_, a) if self.known(*a) => self.whole_of(*a),
            Instruction::Convert(Conversion::Truncate | Conversion::CoerceInt | Conversion::IntegerResult(_), a) => {
                let f = self.float_of(*a);
                self.to_int(f)
            }
            Instruction::Convert(op @ (Conversion::Floor | Conversion::Ceil | Conversion::Round), a) => {
                let value = self.float_of(*a);
                let r = round_float(&mut self.e, op, value);
                self.to_int(r)
            }
            _ => self.e.fail("This instruction is not supported by the SPIR-V kernel target."),
        }
    }

    fn emit(&mut self, slot: i64, guards: Vec<(i64, bool)>, instruction: &Instruction) {
        match instruction {
            Instruction::Guarded(more, i) => {
                let mut g = guards;
                g.extend(more.iter().copied());
                self.emit(slot, g, i);
            }
            instruction => {
                let compute = |ep: &mut EP| -> i64 {
                    let value = match instruction {
                        Instruction::Frame(name) if ep.raster.is_some() => ep.raster_builtin(name),
                        Instruction::Frame(_) => ep.e.uniform_float(ep.layout.frames[slot as usize]),
                        _ => ep.expression(slot, instruction),
                    };
                    if ep.layout.finite_checks[slot as usize] && !ep.integer(slot) {
                        let nf = ep.e.non_finite(value);
                        ep.mark_invalid(nf);
                    }
                    value
                };
                let s = slot as usize;
                if guards.is_empty() {
                    self.registers[s] = compute(self);
                    if self.integer(slot) && !self.narrow(slot) {
                        self.whole[s] = Some(self.registers[s]);
                    }
                } else {
                    let before = self.e.block;
                    let before_invalid = self.invalid;
                    let condition = self.guard_condition(&guards).expect("a guard condition");
                    let yes = self.e.words.id();
                    let after = self.e.words.id();
                    self.e.body(247, &[after, 0]);
                    self.e.body(250, &[condition, yes, after]);
                    self.e.label(yes);
                    let value = compute(self);
                    let yes_invalid = self.invalid;
                    let last = self.e.block;
                    self.e.jump(after);
                    self.e.label(after);
                    let (ty, zero) = if self.narrow(slot) {
                        (self.e.uint, self.e.u(0))
                    } else if self.integer(slot) {
                        (self.e.long(), self.e.l(0))
                    } else {
                        (self.e.float, self.e.f(0.0))
                    };
                    self.registers[s] = self.e.result(ty, 245, &[value, last, zero, before]);
                    if self.e.loop_depth == 0 {
                        let boolean = self.e.boolean;
                        self.invalid = self.e.result(boolean, 245, &[yes_invalid, last, before_invalid, before]);
                    }
                    if self.narrow(slot) {
                    } else if self.integer(slot) {
                        self.whole[s] = Some(self.registers[s]);
                    } else if let Some(value) = self.whole[s] {
                        let long = self.e.long();
                        let z = self.e.l(0);
                        self.whole[s] = Some(self.e.result(long, 245, &[value, last, z, before]));
                    }
                }
            }
        }
    }

    /// A loop bound, as Emit's: in 32-bit registers an Int constant past them saturates.
    fn loop_bound(&mut self, slot: i64) -> i64 {
        match unguarded(&self.layout.view.code[slot as usize]) {
            Instruction::Int_const(n) if self.e.int32 && *n != *n as i32 as i64 => self.e.l((*n).clamp(i32::MIN as i64, i32::MAX as i64)),
            _ => self.int_of(slot),
        }
    }

    fn carried_value(&mut self, slot: i64, source: i64) -> i64 {
        if self.narrow(slot) {
            self.low(source)
        } else if self.integer(slot) {
            self.int_of(source)
        } else {
            self.registers[source as usize]
        }
    }

    fn emit_loop(&mut self, header: usize, control: &Loop, next: &[i64], stop: Option<i64>, body: &mut dyn FnMut(&mut dyn Visit)) {
        if !self.layout.used[header] {
            return;
        }
        let h = header as i64;
        let boolean = self.e.boolean;
        let uint = self.e.uint;
        let guards = match &self.layout.view.code[header] {
            Instruction::Guarded(guards, _) => guards.clone(),
            _ => vec![],
        };
        let guarded = !guards.is_empty();
        let preheader = self.e.words.id();
        let skipped = self.e.words.id();
        let loop_header = self.e.words.id();
        let loop_body = self.e.words.id();
        let continuing = self.e.words.id();
        let finished = self.e.words.id();
        let after_guard = self.e.words.id();
        let found = self.guard_condition(&guards);
        let default = self.e.bool_false();
        let guard_condition = found.unwrap_or(default);
        if guarded {
            self.e.body(247, &[after_guard, 0]);
            self.e.body(250, &[guard_condition, preheader, skipped]);
            self.e.label(skipped);
            self.e.jump(after_guard);
            self.e.label(preheader);
        } else {
            self.e.jump(preheader);
            self.e.label(preheader);
        }
        let first = self.loop_bound(control.first);
        let last = self.loop_bound(control.last);
        let initial: Vec<i64> = control.initial.iter().enumerate().map(|(c, source)| self.carried_value(h + 1 + c as i64, *source)).collect();
        let long = self.e.long();
        let index_type = if self.narrow(h) { uint } else { long };
        let (index_first, index_last) = if self.narrow(h) {
            let f = self.low(control.first);
            let l = self.low(control.last);
            (f, l)
        } else {
            (first, last)
        };
        let too_long = match control.limit {
            None => self.e.bool_false(),
            Some(limit) => {
                let positive = self.e.compare(173, last, first);
                let ulong = self.e.ulong();
                let unsigned_last = self.e.result(ulong, 124, &[last]);
                let unsigned_first = self.e.result(ulong, 124, &[first]);
                let distance = self.e.result(ulong, 130, &[unsigned_last, unsigned_first]);
                let bound = if self.e.int32 { self.e.u(limit.min(u32::MAX as i64)) } else { self.e.words.wide_constant(ulong, limit) };
                let over = self.e.result(boolean, 172, &[distance, bound]);
                let too_long = self.e.result(boolean, 167, &[positive, over]);
                self.e.record_status(too_long, 1);
                too_long
            }
        };
        let preheader_pred = self.e.block;
        let index_phi = self.e.words.id();
        let carry_phis: Vec<i64> = control.initial.iter().map(|_| self.e.words.id()).collect();
        let stop_phi = stop.map(|_| self.e.words.id());
        self.registers[header] = index_phi;
        self.whole[header] = if self.narrow(h) { None } else { Some(index_phi) };
        for (component, phi) in carry_phis.iter().enumerate() {
            let slot = h + 1 + component as i64;
            self.registers[slot as usize] = *phi;
            self.whole[slot as usize] = if self.integer(slot) && !self.narrow(slot) { Some(*phi) } else { None };
        }
        let outer_body = std::mem::take(&mut self.e.words.body);
        self.e.label(loop_body);
        self.e.loop_depth += 1;
        body(self);
        let next_values: Vec<i64> = next
            .iter()
            .enumerate()
            .map(|(component, source)| {
                let slot = h + 1 + component as i64;
                if self.integer(slot) {
                    self.carried_value(slot, *source)
                } else {
                    self.float_of(*source)
                }
            })
            .collect();
        let stop_value = stop.map(|source| {
            let f = self.float_of(source);
            self.e.truth(f)
        });
        self.e.loop_depth -= 1;
        self.e.jump(continuing);
        let body_words = std::mem::take(&mut self.e.words.body);
        self.e.label(continuing);
        let next_index = if self.narrow(h) {
            let one = self.e.u(1);
            self.e.result(uint, 128, &[index_phi, one])
        } else {
            let one = self.e.l(1);
            integer_binary(&mut self.e, Integer::Iadd, index_phi, one)
        };
        self.e.jump(loop_header);
        let continuing_words = std::mem::replace(&mut self.e.words.body, outer_body);
        let initial_stop = self.e.bool_false();
        self.e.block = preheader_pred;
        self.e.jump(loop_header);
        self.e.label(loop_header);
        self.e.body(245, &[index_type, index_phi, index_first, preheader_pred, next_index, continuing]);
        for (component, phi) in carry_phis.iter().enumerate() {
            let slot = h + 1 + component as i64;
            let ty = if self.narrow(slot) {
                uint
            } else if self.integer(slot) {
                long
            } else {
                self.e.float
            };
            self.e.body(245, &[ty, *phi, initial[component], preheader_pred, next_values[component], continuing]);
        }
        if let Some(phi) = stop_phi {
            self.e.body(245, &[boolean, phi, initial_stop, preheader_pred, stop_value.expect("a stop value"), continuing]);
        }
        let in_range = self.e.compare(if self.narrow(h) { 176 } else { 177 }, index_phi, index_last);
        let z = self.e.u(0);
        let none = self.e.compare(170, z, z);
        let not_stopped = match stop_phi {
            None => none,
            Some(phi) => self.e.result(boolean, 168, &[phi]),
        };
        let both = self.e.result(boolean, 167, &[in_range, not_stopped]);
        let not_too_long = self.e.result(boolean, 168, &[too_long]);
        let condition = self.e.result(boolean, 167, &[both, not_too_long]);
        self.e.body(246, &[finished, continuing, 0]);
        self.e.body(250, &[condition, loop_body, finished]);
        self.e.words.body.extend_from_slice(&body_words);
        self.e.words.body.extend_from_slice(&continuing_words);
        self.e.label(finished);
        if guarded {
            self.e.jump(after_guard);
            self.e.label(after_guard);
            let zero = if self.narrow(h) { self.e.u(0) } else { self.e.l(0) };
            self.registers[header] = self.e.result(index_type, 245, &[index_phi, finished, zero, skipped]);
            for (component, phi) in carry_phis.iter().enumerate() {
                let slot = h + 1 + component as i64;
                let (ty, zero) = if self.narrow(slot) {
                    (uint, self.e.u(0))
                } else if self.integer(slot) {
                    (long, self.e.l(0))
                } else {
                    (self.e.float, self.e.f(0.0))
                };
                self.registers[slot as usize] = self.e.result(ty, 245, &[*phi, finished, zero, skipped]);
                self.whole[slot as usize] = if self.integer(slot) && !self.narrow(slot) { Some(self.registers[slot as usize]) } else { None };
            }
        }
    }

    /// The outputs: stored to the output buffers, or handed to the raster placement.
    fn finish(&mut self) {
        let layout = self.layout.clone();
        for (output, values) in layout.view.outputs.iter().enumerate() {
            for (component, register) in values.iter().enumerate() {
                let register = *register;
                if layout.output_checks && !self.integer(register) {
                    let f = self.float_of(register);
                    let nf = self.e.non_finite(f);
                    self.mark_invalid(nf);
                }
                if self.raster.is_some() {
                    let f = self.float_of(register);
                    let st = self.raster.as_mut().unwrap();
                    st.output(&mut self.e, output as i64, component as i64, f);
                } else if self.output_uint32[output] {
                    let value = self.int_of(register);
                    let boolean = self.e.boolean;
                    // a 32-bit Int is out of uint32 range only when negative; a narrow register never is
                    let c = if self.e.int32 && self.narrow(register) {
                        self.e.bool_false()
                    } else if self.e.int32 {
                        let zero = self.e.l(0);
                        self.e.compare(177, value, zero)
                    } else {
                        let zero = self.e.l(0);
                        let c177 = self.e.compare(177, value, zero);
                        let top = self.e.l(0xffff_ffff);
                        let c172 = self.e.compare(172, value, top);
                        self.e.result(boolean, 166, &[c177, c172])
                    };
                    self.e.record_status(c, 64);
                    let uint = self.e.uint;
                    let narrowed = self.e.result(uint, if self.e.int32 { 124 } else { 113 }, &[value]);
                    let offset = self.e.offset(self.index, values.len() as i64, component as i64);
                    self.e.store_uint(self.outputs[output], offset, narrowed);
                } else {
                    let f = self.float_of(register);
                    let offset = self.e.offset(self.index, values.len() as i64, component as i64);
                    self.e.store(self.outputs[output], offset, f);
                    // an unfolded kernel feeds its pass's value to the next
                    if let Some(vars) = &self.carried {
                        let var = vars[component];
                        let f = self.float_of(register);
                        self.e.body(62, &[var, f]);
                    }
                }
            }
        }
    }
}

impl Visit for EP {
    fn instruction(&mut self, slot: usize, instruction: &Instruction) {
        if self.e.failure.is_some() || !self.layout.used[slot] {
            return;
        }
        if !matches!(unguarded(instruction), Instruction::Array_prepare(..)) {
            self.emit(slot as i64, vec![], instruction);
        }
    }
    fn loop_(&mut self, header: usize, control: &Loop, next: &[i64], stop: Option<i64>, body: &mut dyn FnMut(&mut dyn Visit)) {
        if self.e.failure.is_some() {
            return;
        }
        self.emit_loop(header, control, next, stop, body);
    }
}

pub struct ProgramOptions {
    carried: Option<Vec<i64>>,
    input_uint32: Vec<bool>,
    gather_uint32: Vec<bool>,
    uint32_words: Vec<i64>,
    output_uint32: Vec<bool>,
}

/// The register program of a kernel or a raster stage; returns the invalid flag.
fn emit_program(
    e: &mut Emitter,
    layout: &Arc<kernel_layout::T>,
    raster: &mut Option<StageState>,
    options: &ProgramOptions,
    inputs: &[i64],
    outputs: &[i64],
    gathers: &[i64],
    index: i64,
) -> i64 {
    let size = layout.view.code.len();
    let taken = std::mem::take(e);
    let mut ep = EP {
        e: taken,
        layout: layout.clone(),
        raster: raster.take(),
        carried: options.carried.clone(),
        input_uint32: options.input_uint32.clone(),
        gather_uint32: options.gather_uint32.clone(),
        uint32_words: options.uint32_words.clone(),
        output_uint32: options.output_uint32.clone(),
        inputs: inputs.to_vec(),
        outputs: outputs.to_vec(),
        gathers: gathers.to_vec(),
        index,
        registers: vec![0; size],
        whole: vec![None; size],
        invalid: 0,
    };
    ep.invalid = ep.e.bool_false();
    kernel_layout::iter(layout, &mut ep);
    if ep.e.failure.is_none() {
        ep.finish();
    }
    let invalid = ep.invalid;
    *raster = ep.raster.take();
    *e = ep.e;
    invalid
}

fn resident_count(e: &mut Emitter, layout: &kernel_layout::T, index: i64, buffers: Option<(i64, i64)>) -> i64 {
    let n = e.uniform(0);
    match (layout.counts, buffers) {
        (Some(bytes), Some((input, output))) => {
            let word = bytes / 4;
            let uint = e.uint;
            let w2 = e.uniform(word + 2);
            let z = e.u(0);
            let cond = e.compare(171, w2, z);
            let n = e.conditional(
                uint,
                cond,
                &mut |e: &mut Emitter| {
                    let uint = e.uint;
                    let z = e.u(0);
                    let l = e.load_uint(input, z);
                    let uw = e.uniform(word);
                    let product = e.result(uint, 132, &[l, uw]);
                    let divisor = e.uniform(word + 1);
                    let z = e.u(0);
                    let c = e.compare(170, divisor, z);
                    let one = e.u(1);
                    let divisor = e.select_u(c, one, divisor);
                    let bound = e.result(uint, 134, &[product, divisor]);
                    let c = e.compare(176, n, bound);
                    e.select_u(c, n, bound)
                },
                &mut |_e: &mut Emitter| n,
            );
            let z = e.u(0);
            let c = e.compare(170, index, z);
            e.conditional(
                uint,
                c,
                &mut |e: &mut Emitter| {
                    let uint = e.uint;
                    let six = e.u(6);
                    let sixn = e.result(uint, 132, &[n, six]);
                    let one = e.u(1);
                    let z = e.u(0);
                    let values = [n, sixn, one, z, z, z];
                    for (slot, value) in values.iter().enumerate() {
                        let us = e.u(slot as i64);
                        e.store_uint(output, us, *value);
                    }
                    n
                },
                &mut |_e: &mut Emitter| n,
            );
            n
        }
        (None, None) => n,
        _ => panic!("count record without its buffers"),
    }
}

/// The optional arguments of [`kernel`]; `curves` selects the in-loop capsule test of linear curves.
#[derive(Clone, Debug)]
pub struct Options {
    pub fast: bool,
    pub instanced: bool,
    pub motion: bool,
    pub curves: bool,
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
            fast: false,
            instanced: false,
            motion: false,
            curves: false,
            spheres: true,
            uint32_scalars: false,
            uint32_outputs: vec![],
            uint32_inputs: vec![],
            uint32_tables: vec![],
            unfold: None,
        }
    }
}

struct Module {
    words: Words,
    globals: Vec<i64>,
    interface: Vec<Binding>,
}

impl Module {
    fn variable(&mut self, pointer: i64, storage: i64) -> i64 {
        let id = self.words.id();
        instruction(&mut self.words.declarations, 59, &[pointer, id, storage]);
        self.globals.push(id);
        id
    }
    fn bind(&mut self, pointer: i64, storage: i64, binding: i64, kind: BindingKind) -> i64 {
        let id = self.variable(pointer, storage);
        instruction(&mut self.words.annotations, 71, &[id, 34, 0]);
        instruction(&mut self.words.annotations, 71, &[id, 33, binding]);
        self.interface.push(Binding { group: 0, binding, kind, visibility: vec![ShaderStage::Compute] });
        id
    }
}

pub fn kernel(options: &Options, program: &packed::T) -> Result<T, Diagnostic> {
    let fast = options.fast;
    if let Some(d) = packed::gpu_refusals(true, fast, program).into_iter().next() {
        return Err(d);
    }
    let (program, back) = noise::helpers(options.instanced, options.motion, if options.curves { Some(trace::Curves::Loop) } else { None }, options.spheres, program)?;
    let layout = noise::checked(
        &back,
        kernel_layout::plan(
            kernel_layout::PlanOptions { fast, image: false, instanced: options.instanced, motion: options.motion, curves: options.curves },
            &program,
        ),
    );
    // the exact uint32 buffers and scalars, validated as Emit does
    let mut input_uint32 = vec![false; layout.view.widths.len()];
    for i in &options.uint32_inputs {
        if !fast || *i < 0 || *i >= layout.view.widths.len() as i64 || layout.view.widths[*i as usize] != 1 {
            return Err(fail_d("Uint32 input requires a scalar buffer in fast precision."));
        }
        input_uint32[*i as usize] = true;
    }
    let mut gather_uint32 = vec![false; layout.gathers.len()];
    for t in &options.uint32_tables {
        let mut found = false;
        for (k, (_, (index, width))) in layout.gathers.iter().enumerate() {
            if index == t && *width == 1 { gather_uint32[k] = true; found = true; }
        }
        if !fast || !found {
            return Err(fail_d("Uint32 table requires a scalar gathered buffer in fast precision."));
        }
    }
    let mut output_uint32 = vec![false; layout.output_count as usize];
    for index in &options.uint32_outputs {
        if *index < 0 || *index >= layout.output_count {
            return Err(fail_d("Uint32 output index is outside its producer."));
        }
        match layout.view.outputs[*index as usize].as_slice() {
            [slot] if fast && region::integer_register(&layout.view.code[*slot as usize]) => output_uint32[*index as usize] = true,
            _ => return Err(fail_d("Uint32 output requires a scalar integer result in fast precision.")),
        }
    }
    let mut uint32_words: Vec<i64> = Vec::new();
    if options.uint32_scalars {
        if !fast {
            return Err(fail_d("Uint32 scalars require fast precision."));
        }
        for (slot, code) in layout.view.code.iter().enumerate() {
            match unguarded(code) {
                Instruction::Int_uniform(u) => uint32_words.push(layout.uniforms[*u as usize]),
                Instruction::Int_frame(_) if layout.frames[slot] >= 0 => uint32_words.push(layout.frames[slot]),
                _ => {}
            }
        }
        uint32_words.sort_unstable();
        uint32_words.dedup();
    }
    let mut m = Module { words: Words::new(), globals: vec![], interface: vec![] };
    let void = m.words.declare(19, &[]);
    let boolean = m.words.declare(20, &[]);
    let uint = m.words.declare(21, &[32, 0]);
    let sint = m.words.declare(21, &[32, 1]);
    let float = m.words.declare(22, &[32]);
    let (long, ulong) = if options.fast && needs_integer(&layout) {
        (Some(sint), Some(uint))
    } else if needs_integer(&layout) {
        let long = m.words.declare(21, &[64, 1]);
        let ulong = m.words.declare(21, &[64, 0]);
        (Some(long), Some(ulong))
    } else {
        (None, None)
    };
    let vector3 = m.words.declare(23, &[uint, 3]);
    let _vector4 = m.words.declare(23, &[uint, 4]);
    let vector4 = _vector4;
    let function_type = m.words.declare(33, &[void]);
    let (storage, storage_pointer) = storage_type(&mut m.words, float);
    let (uint_storage, uint_storage_pointer) = storage_type(&mut m.words, uint);
    let uint_function_pointer = m.words.declare(32, &[7, uint]);
    let uniform_length = m.words.constant(uint, kernel_layout::uniform_words(&layout) as i32);
    let uniform_array = m.words.declare(28, &[vector4, uniform_length]);
    let uniform_struct = m.words.declare(30, &[uniform_array]);
    instruction(&mut m.words.annotations, 71, &[uniform_array, 6, 16]);
    instruction(&mut m.words.annotations, 71, &[uniform_struct, 2]);
    instruction(&mut m.words.annotations, 72, &[uniform_struct, 0, 35, 0]);
    let uniform_type = m.words.declare(32, &[2, uniform_struct]);
    let uniform_pointer = m.words.declare(32, &[2, uint]);
    let invocation_pointer = m.words.declare(32, &[1, vector3]);
    let mut inputs = Vec::new();
    for (index, used) in layout.input_used.iter().enumerate() {
        inputs.push(if *used { m.bind(if input_uint32[index] { uint_storage } else { storage }, 12, index as i64, BindingKind::StorageBuffer) } else { 0 });
    }
    let mut outputs = Vec::new();
    for index in 0..layout.output_count {
        outputs.push(m.bind(if output_uint32[index as usize] { uint_storage } else { storage }, 12, layout.input_count + index, BindingKind::StorageBuffer));
    }
    let uniform_buffer = m.bind(uniform_type, 2, layout.uniform_binding, BindingKind::UniformBuffer);
    let status = m.bind(uint_storage, 12, layout.status_binding, BindingKind::StorageBuffer);
    let table = layout.table_binding.map(|binding| m.bind(uint_storage, 12, binding, BindingKind::StorageBuffer));
    let mut gathers = Vec::new();
    for k in 0..layout.gathers.len() {
        gathers.push(m.bind(uint_storage, 12, layout.gather_base + k as i64, BindingKind::StorageBuffer));
    }
    let counts = layout.counts.map(|_| {
        let input = m.bind(uint_storage, 12, layout.count_base, BindingKind::StorageBuffer);
        let output = m.bind(uint_storage, 12, layout.count_base + 1, BindingKind::StorageBuffer);
        (input, output)
    });
    let scene = match layout.scene_binding {
        None => None,
        Some(binding) => {
            let accel_type = m.words.declare(5341, &[]);
            let p = m.words.declare(32, &[0, accel_type]);
            let accel = m.bind(p, 0, binding, BindingKind::AccelerationStructure);
            let vertices = m.bind(storage, 12, binding + 1, BindingKind::StorageBuffer);
            let spheres = m.bind(storage, 12, binding + 2, BindingKind::StorageBuffer);
            let instances = layout.scene_instances.map(|slot| m.bind(storage, 12, slot, BindingKind::StorageBuffer));
            let vertices_end = layout.scene_vertices_end.map(|slot| m.bind(storage, 12, slot, BindingKind::StorageBuffer));
            let query_type = m.words.declare(4472, &[]);
            let p = m.words.declare(32, &[6, query_type]);
            let query = m.variable(p, 6);
            let nested_query = if layout.scene_motion && instances.is_some() {
                let p = m.words.declare(32, &[6, query_type]);
                Some(m.variable(p, 6))
            } else {
                None
            };
            let curve_accel = layout.scene_strands.map(|strand_binding| {
                let p = m.words.declare(32, &[0, accel_type]);
                m.bind(p, 0, strand_binding + 1, BindingKind::AccelerationStructure)
            });
            let motion_best = if layout.scene_motion && instances.is_some() {
                let p = m.words.declare(32, &[6, float]);
                let v1 = m.variable(p, 6);
                let p = m.words.declare(32, &[6, uint]);
                let v2 = m.variable(p, 6);
                let p = m.words.declare(32, &[6, uint]);
                let v3 = m.variable(p, 6);
                let p = m.words.declare(32, &[6, float]);
                let v4 = m.variable(p, 6);
                let p = m.words.declare(32, &[6, float]);
                let v5 = m.variable(p, 6);
                Some((v1, v2, v3, v4, v5))
            } else {
                None
            };
            let curve_query = curve_accel.map(|_| {
                let p = m.words.declare(32, &[6, query_type]);
                m.variable(p, 6)
            });
            let prototype_binding = match (layout.scene_motion, layout.scene_instances) {
                (true, Some(instances)) => Some(match layout.scene_strands {
                    None => instances + 1,
                    Some(strands) => strands + 2,
                }),
                _ => None,
            };
            let prototype_accel = prototype_binding.map(|slot| {
                let p = m.words.declare(32, &[0, accel_type]);
                m.bind(p, 0, slot, BindingKind::AccelerationStructure)
            });
            let vec3 = m.words.declare(23, &[float, 3]);
            let vec2 = m.words.declare(23, &[float, 2]);
            Some(Scene {
                accel,
                accel_type,
                vertices,
                spheres,
                instances,
                vertices_end,
                motion: layout.scene_motion,
                query,
                nested_query,
                curve_accel,
                curve_query,
                prototype_accel,
                motion_best,
                vec3,
                vec2,
            })
        }
    };
    let strands = layout.scene_strands.map(|binding| m.bind(storage, 12, binding, BindingKind::StorageBuffer));
    let invocation = m.variable(invocation_pointer, 1);
    instruction(&mut m.words.annotations, 71, &[invocation, 11, 28]);
    let glsl = m.words.id();
    let main = m.words.id();
    let layout = Arc::new(layout);
    let mut e = Emitter {
        words: std::mem::take(&mut m.words),
        boolean,
        uint,
        sint,
        float,
        long,
        ulong,
        int32: options.fast,
        glsl,
        uniform: uniform_buffer,
        uniform_pointer,
        storage_pointer,
        uint_storage_pointer,
        status,
        uint_function_pointer,
        status_bits: 0,
        table,
        scene: scene.clone(),
        block: 0,
        trace_cache: vec![],
        strands,
        loop_depth: 0,
        failure: None,
    };
    e.body(54, &[void, main, 0, function_type]);
    let entry_block = e.words.id();
    e.label(entry_block);
    let z = e.u(0);
    e.status_bits = e.result(uint_function_pointer, 59, &[7, z]);
    // an unfolded kernel's carried value and pass, function variables (the entry block's first)
    let carried = options.unfold.map(|_| {
        let float_function_pointer = e.words.declare(32, &[7, float]);
        let vars: Vec<i64> = (0..layout.view.widths[0]).map(|_| e.result(float_function_pointer, 59, &[7])).collect();
        let z = e.u(0);
        let pass = e.result(uint_function_pointer, 59, &[7, z]);
        (vars, pass)
    });
    let gid = e.result(vector3, 61, &[invocation]);
    let index = e.result(uint, 81, &[gid, 0]);
    let body = e.words.id();
    let finish = e.words.id();
    let count = resident_count(&mut e, &layout, index, counts);
    let within = e.compare(176, index, count);
    e.body(247, &[finish, 0]);
    e.body(250, &[within, body, finish]);
    e.label(body);
    let mut none = None;
    let program_options = |carried: Option<Vec<i64>>| ProgramOptions {
        carried,
        input_uint32: input_uint32.clone(),
        gather_uint32: gather_uint32.clone(),
        uint32_words: uint32_words.clone(),
        output_uint32: output_uint32.clone(),
    };
    let invalid = match (options.unfold, &carried) {
        (Some(steps), Some((vars, pass))) => {
            // the region run `steps` passes in this lane: pass k's value stored at (k n + i), then fed
            // back (Emit.kernel ~unfold)
            let width = layout.view.widths[0];
            for (c, var) in vars.iter().enumerate() {
                let off = e.offset(index, width, c as i64);
                let l = e.load(inputs[0], off);
                e.body(62, &[*var, l]);
            }
            let header = e.words.id();
            let test = e.words.id();
            let step = e.words.id();
            let continuing = e.words.id();
            let merge = e.words.id();
            e.jump(header);
            e.label(header);
            e.body(246, &[merge, continuing, 0]);
            e.jump(test);
            e.label(test);
            let k = e.result(uint, 61, &[*pass]);
            let us = e.u(steps);
            let c = e.compare(176, k, us);
            e.body(250, &[c, step, merge]);
            e.label(step);
            e.loop_depth += 1;
            let kc = e.result(uint, 132, &[k, count]);
            let at = e.result(uint, 128, &[kc, index]);
            let invalid = emit_program(&mut e, &layout, &mut none, &program_options(Some(vars.clone())), &inputs, &outputs, &gathers, at);
            e.record_status(invalid, 1);
            e.loop_depth -= 1;
            e.jump(continuing);
            e.label(continuing);
            let one = e.u(1);
            let next = e.result(uint, 128, &[k, one]);
            e.body(62, &[*pass, next]);
            e.jump(header);
            e.label(merge);
            e.bool_false()
        }
        _ => emit_program(&mut e, &layout, &mut none, &program_options(None), &inputs, &outputs, &gathers, index),
    };
    if let Some(d) = e.failure.take() {
        return Err(d);
    }
    e.record_status(invalid, 1);
    let bits = e.result(uint, 61, &[e.status_bits]);
    let z = e.u(0);
    let flagged = e.compare(171, bits, z);
    let bad = e.words.id();
    let after = e.words.id();
    e.body(247, &[after, 0]);
    e.body(250, &[flagged, bad, after]);
    e.label(bad);
    let z = e.u(0);
    let status_pointer = e.result(uint_storage_pointer, 65, &[status, z, z]);
    let one = e.u(1);
    e.result(uint, 241, &[status_pointer, one, z, bits]);
    e.jump(after);
    e.label(after);
    e.jump(finish);
    e.label(finish);
    e.body(253, &[]);
    e.body(56, &[]);
    if let Some(d) = e.failure.take() {
        return Err(d);
    }
    let words = e.words;
    let mut module_bytes: Vec<u8> = Vec::with_capacity(words.body.len() + words.declarations.len() + 512);
    for w in [0x07230203i64, if scene.is_none() { 0x00010300 } else { 0x00010400 }, 0, words.next, 0] {
        add_word(&mut module_bytes, w);
    }
    instruction(&mut module_bytes, 17, &[1]);
    if long.is_some() && !e.int32 {
        instruction(&mut module_bytes, 17, &[11]);
    }
    if scene.is_some() {
        instruction(&mut module_bytes, 17, &[4472]);
        instruction(&mut module_bytes, 10, &Words::string_words("SPV_KHR_ray_query"));
    }
    let mut g = vec![glsl];
    g.extend(Words::string_words("GLSL.std.450"));
    instruction(&mut module_bytes, 11, &g);
    instruction(&mut module_bytes, 14, &[0, 1]);
    let entry = "rays_kernel".to_owned();
    let mut ep = vec![5, main];
    ep.extend(Words::string_words(&entry));
    if scene.is_none() {
        ep.push(invocation);
    } else {
        ep.extend(m.globals.iter().copied());
    }
    instruction(&mut module_bytes, 15, &ep);
    instruction(&mut module_bytes, 16, &[main, 17, 256, 1, 1]);
    module_bytes.extend_from_slice(&words.annotations);
    module_bytes.extend_from_slice(&words.declarations);
    module_bytes.extend_from_slice(&words.body);
    let mut interface = m.interface.clone();
    interface.sort_by(|l, r| (l.group, l.binding).cmp(&(r.group, r.binding)));
    let uniform_layout = trace::frames(&back, &layout.uniform_layout);
    let uniform_bytes = kernel_layout::uniform_words(&layout) * 16;
    let layout = kernel_layout::T { uniform_layout, ..(*layout).clone() };
    Ok(T { bytes: module_bytes, entry, interface, layout, uniform_bytes })
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Output {
    Position,
    #[allow(non_camel_case_types)]
    Point_size,
    Varying(i64, bool),
    Colour,
    Discard,
}

#[derive(Clone, Debug)]
pub struct StageSpec {
    pub vertex: bool,
    pub inputs: Vec<(i64, i64, bool)>,
    pub outputs: Vec<Output>,
    pub buffers: Vec<(i64, i64, bool)>,
    pub workgroup: Option<i64>,
    pub grid: bool,
}

/// A raster stage as a Vulkan vertex or fragment module, entry `main` (Raster's placement of
/// `emit_program`): element `k` of a fragment is the interpolated input at location `k`'s
/// (location, width, flat); a vertex's one element is VertexIndex. Buffers are runtime arrays at their
/// bindings (uint words, or floats bit-cast from them), texture `t` and its sampler `t + 1` are
/// separate descriptors, and a discard output ends the invocation with OpKill after the stores.
pub fn stage(s: &StageSpec, program: &packed::T) -> Result<Vec<u8>, Diagnostic> {
    let layout = Arc::new(kernel_layout::plan(kernel_layout::PlanOptions { fast: true, ..kernel_layout::PlanOptions::default() }, program));
    let mut words = Words::new();
    let void = words.declare(19, &[]);
    let boolean = words.declare(20, &[]);
    let uint = words.declare(21, &[32, 0]);
    let sint = words.declare(21, &[32, 1]);
    let float = words.declare(22, &[32]);
    let (long, ulong) = if needs_integer(&layout) || layout.view.code.iter().any(|i| region::integer_register(unguarded(i))) {
        let long = words.declare(21, &[64, 1]);
        let ulong = words.declare(21, &[64, 0]);
        (Some(long), Some(ulong))
    } else {
        (None, None)
    };
    let mut e = Emitter { words, boolean, uint, sint, float, long, ulong, ..Emitter::default() };
    let mut st = StageState {
        spec: s.clone(),
        inputs: vec![],
        vertex_index: None,
        outputs: vec![],
        buffers: vec![],
        image: 0,
        sampler: 0,
        sampled: 0,
        vec2: 0,
        vec4: 0,
        textures: HashMap::new(),
        sizes: false,
        builtins: HashMap::new(),
        discard: None,
        scans: HashMap::new(),
        types: HashMap::new(),
        interface: vec![],
        output_widths: layout.view.outputs.iter().map(|o| o.len()).collect(),
    };
    st.vec2 = st.vector(&mut e, 2);
    st.vec4 = st.vector(&mut e, 4);
    let (storage, storage_pointer) = storage_type(&mut e.words, float);
    let (uint_storage, uint_storage_pointer) = storage_type(&mut e.words, uint);
    for (location, width, flat) in &s.inputs {
        let id = st.io(&mut e, 0, 1, *width, Some(*location), *flat);
        st.inputs.push(id);
    }
    if s.vertex {
        let p = st.pointer(&mut e, 1, uint);
        let id = st.variable(&mut e, p, 1);
        e.annotate(71, &[id, 11, 42]);
        st.interface.push(id);
        st.vertex_index = Some(id);
    }
    for (k, o) in s.outputs.iter().enumerate() {
        let id = match o {
            Output::Position => st.io(&mut e, 0, 3, 4, None, false),
            Output::Point_size => st.io(&mut e, 1, 3, 1, None, false),
            Output::Varying(location, flat) => st.io(&mut e, 0, 3, layout.view.outputs[k].len() as i64, Some(*location), *flat),
            Output::Colour => st.io(&mut e, 0, 3, 4, Some(0), false),
            Output::Discard => 0,
        };
        st.outputs.push(id);
    }
    for (u, binding, w) in &s.buffers {
        let id = st.variable(&mut e, if *w { uint_storage } else { storage }, 12);
        e.annotate(71, &[id, 34, 0]);
        e.annotate(71, &[id, 33, *binding]);
        st.buffers.push((*u, (id, *w)));
    }
    st.image = e.words.declare(25, &[float, 1, 0, 0, 0, 1, 0]);
    st.sampler = e.words.declare(26, &[]);
    st.sampled = e.words.declare(27, &[st.image]);
    e.glsl = e.words.id();
    let main = e.words.id();
    e.storage_pointer = storage_pointer;
    e.uint_storage_pointer = uint_storage_pointer;
    e.uint_function_pointer = st.pointer(&mut e, 7, uint);
    let fn_type = e.words.declare(33, &[void]);
    e.body(54, &[void, main, 0, fn_type]);
    let entry_block = e.words.id();
    e.label(entry_block);
    let z = e.u(0);
    e.status_bits = e.result(e.uint_function_pointer, 59, &[7, z]);
    let index = e.u(0);
    let mut raster = Some(st);
    let options = ProgramOptions { carried: None, input_uint32: vec![], gather_uint32: vec![], uint32_words: vec![], output_uint32: vec![] };
    emit_program(&mut e, &layout, &mut raster, &options, &[], &[], &[], index);
    if let Some(d) = e.failure.take() {
        return Err(d);
    }
    let st = raster.take().expect("the stage state");
    if let Some(flag) = st.discard {
        let kill = e.words.id();
        let after = e.words.id();
        let flag = e.truth(flag);
        e.body(247, &[after, 0]);
        e.body(250, &[flag, kill, after]);
        e.label(kill);
        e.body(252, &[]);
        e.label(after);
    }
    e.body(253, &[]);
    e.body(56, &[]);
    let words = &e.words;
    let mut bytes: Vec<u8> = Vec::with_capacity(words.body.len() + words.declarations.len() + 512);
    for w in [0x07230203i64, 0x00010300, 0, words.next, 0] {
        add_word(&mut bytes, w);
    }
    instruction(&mut bytes, 17, &[1]);
    if long.is_some() {
        instruction(&mut bytes, 17, &[11]);
    }
    if st.sizes {
        instruction(&mut bytes, 17, &[50]);
    }
    let mut g = vec![e.glsl];
    g.extend(Words::string_words("GLSL.std.450"));
    instruction(&mut bytes, 11, &g);
    instruction(&mut bytes, 14, &[0, 1]);
    let model = if s.workgroup.is_some() {
        5
    } else if s.vertex {
        0
    } else {
        4
    };
    let mut ep = vec![model, main];
    ep.extend(Words::string_words("main"));
    ep.extend(st.interface.iter().copied());
    instruction(&mut bytes, 15, &ep);
    match s.workgroup {
        Some(n) => instruction(&mut bytes, 16, &[main, 17, n, 1, 1]),
        None => {
            if !s.vertex {
                instruction(&mut bytes, 16, &[main, 7]);
            }
        }
    }
    bytes.extend_from_slice(&words.annotations);
    bytes.extend_from_slice(&words.declarations);
    bytes.extend_from_slice(&words.body);
    Ok(bytes)
}
