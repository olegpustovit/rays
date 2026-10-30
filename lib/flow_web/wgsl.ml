module P = Flow_ir.Packed

type kind = Uniform_buffer | Storage_read | Storage_read_write
type binding = { group : int; binding : int; kind : kind }

type wgsl = {
  source : string;
  entry : string;
  interface : binding list;
  uniform_layout : (string * int * int) list;
  uniform_bytes : int;
  table_seeds : int array;
  output_widths : int array;
  input_widths : int array;
  gathers : (int * int) array;
  counts : int option;
}

let finite =
  {|fn wfinite(x: f32) -> bool { return (bitcast<u32>(x) & 0x7f800000u) != 0x7f800000u; }
|}

(* Refine the hardware division to match native's double UV grid rounded to f32. A one-ULP
   UV error can flip a hard glyph edge. WGSL permits 2.5 ULP for division (section 15.7.4). *)
let image_uv =
  {|fn pixel_uv(index: u32, size: f32) -> f32 {
  let x = f32(index) + 0.5;
  let q = x / size;
  return q + fma(-q, size, x) / size;
}
|}

let power =
  {|fn wpow(a: f32, b: f32) -> f32 {
  let x = abs(a);
  if (x == 0.0) {
    if (b == 0.0) { return 1.0; }
    if (b > 0.0) { return 0.0; }
    let infinity = 0x7f800000u;
    return bitcast<f32>(infinity);
  }
  return pow(x, b);
}
|}

let perlin =
  {|fn wfade(t: f32) -> f32 { return t * t * t * (t * (t * 6.0 - 15.0) + 10.0); }
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
|}

(* a lacunarity or gain other than 2 and 0.5 (or live): NaN where noise3 would refuse it, so the
   status sends the region to the CPU, which reports it *)
let perlin_fractal =
  {|fn wfbm_fractal(p: vec3<f32>, octaves: i32, o: i32, lacunarity: f32, gain: f32) -> f32 {
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
|}

(* a live octave count, an integer register pair: NaN outside the device's 1 to 32 *)
let perlin_live =
  {|fn wfbm_live(p: vec3<f32>, octaves: vec2<u32>, o: i32, lacunarity: f32, gain: f32) -> f32 {
  if (octaves.y != 0u || octaves.x < 1u || octaves.x > 32u) {
    let nan = 0x7fc00000u;
    return bitcast<f32>(nan);
  }
  return wfbm_fractal(p, i32(octaves.x), o, lacunarity, gain);
}
|}

let live_noise = function P.Noise3 (_, _, _, _, Live _, _, _) -> true | _ -> false

let fractal_noise = function
  | P.Noise3 (_, _, _, _, Live _, _, _) -> true
  | P.Noise3 (_, _, _, _, _, lacunarity, gain) -> lacunarity <> Fixed 2. || gain <> Fixed 0.5
  | _ -> false

(* the call of a noise3: [register] names a register, [count] an integer register as a pair,
   [number] prints a constant *)
let noise_call ~register ~count ~number ~table (x, y, z, seed, octaves, lacunarity, gain) =
  let position = Printf.sprintf "vec3<f32>(%s, %s, %s)" (register x) (register y) (register z) in
  let fractal = function P.Fixed v -> number v | Live r -> register r in
  match octaves with
  | P.Live r ->
      Printf.sprintf "wfbm_live(%s, %s, %d, %s, %s)" position (count r) (table seed)
        (fractal lacunarity) (fractal gain)
  | Fixed octaves ->
      if lacunarity = P.Fixed 2. && gain = P.Fixed 0.5 then
        Printf.sprintf "wfbm(%s, %d, %d)" position octaves (table seed)
      else
        Printf.sprintf "wfbm_fractal(%s, %d, %d, %s, %s)" position octaves (table seed)
          (fractal lacunarity) (fractal gain)

let fail message = raise (Diagnostic.Error (Diagnostic.error ~code:"E_GPU_FORM" message))
let register index = "r" ^ string_of_int index

let integer_status operation b =
  match operation with
  | Flow.Packed_ops.Idiv | Imod -> Printf.sprintf "if (all(%s == vec2<u32>(0u))) { sb |= 2u; }\n" b
  | Ishl | Ishr -> Printf.sprintf "if (%s.y != 0u || %s.x > 62u) { sb |= 4u; }\n" b b
  | _ -> ""

(* As the Metal printer: status bits gather in [sb] and reach the shared word once; guarded
   instructions share blocks per scope (Flow_ir.Kernel_layout.Scope). *)
let flush_status = "if (sb != 0u) { atomicOr(&status, sb); }"

module Scope = Flow_ir.Kernel_layout.Scope

let enter condition =
  Scope.enter ~opening:(fun slot selected ->
      Printf.sprintf "if (%s %s 0.0) {" (condition slot) (if selected then "!=" else "=="))

let rec guarded guards = function
  | P.Guarded (more, body) -> guarded (guards @ more) body
  | instruction -> (guards, instruction)

let truth condition = "select(0.0, 1.0, " ^ condition ^ ")"

let binary operation a b =
  let infix operator = truth ("(" ^ a ^ operator ^ b ^ ")") in
  match operation with
  | P.Add -> "(" ^ a ^ " + " ^ b ^ ")"
  | Sub -> "(" ^ a ^ " - " ^ b ^ ")"
  | Mul -> "(" ^ a ^ " * " ^ b ^ ")"
  | Div -> "select(" ^ a ^ " / " ^ b ^ ", 0.0, " ^ b ^ " == 0.0)"
  | Mod -> "select(((" ^ a ^ " % " ^ b ^ ") + " ^ b ^ ") % " ^ b ^ ", 0.0, " ^ b ^ " == 0.0)"
  | Pow -> "wpow(" ^ a ^ ", " ^ b ^ ")"
  | Min -> "min(" ^ a ^ ", " ^ b ^ ")"
  | Max -> "max(" ^ a ^ ", " ^ b ^ ")"
  | Lt -> infix " < "
  | Le -> infix " <= "
  | Gt -> infix " > "
  | Ge -> infix " >= "
  | Eq -> infix " == "
  | And -> truth ("(" ^ a ^ " != 0.0 && " ^ b ^ " != 0.0)")
  | Or -> truth ("(" ^ a ^ " != 0.0 || " ^ b ^ " != 0.0)")
  | Atan2 -> "atan2(" ^ a ^ ", " ^ b ^ ")"
  | Hypot -> fail "Native-safe hypot is not supported by the WGSL kernel target."
  | Ldexp -> "ldexp(" ^ a ^ ", i32(" ^ b ^ "))"

let unary operation a =
  match operation with
  | P.Sin -> "sin(" ^ a ^ ")"
  | Cos -> "cos(" ^ a ^ ")"
  | Sqrt -> "sqrt(abs(" ^ a ^ "))"
  | Abs -> "abs(" ^ a ^ ")"
  | Not -> truth ("(" ^ a ^ " == 0.0)")
  | Tan -> "tan(" ^ a ^ ")"
  | Asin | Acos ->
      "select("
      ^ (if operation = P.Asin then "asin" else "acos")
      ^ "(clamp(" ^ a ^ ", -1.0, 1.0)), bitcast<f32>(0x7fc00000u), abs(" ^ a ^ ") > 1.0)"
  | Exp -> "exp(" ^ a ^ ")"
  | Log ->
      "select(log(select(1.0, " ^ a ^ ", " ^ a ^ " > 0.0)), bitcast<f32>(0x7fc00000u), " ^ a
      ^ " <= 0.0)"
  | Rsqrt -> "inverseSqrt(abs(" ^ a ^ "))"
  | Sign -> "select(" ^ a ^ ", select(-1.0, 1.0, " ^ a ^ " > 0.0), " ^ a ^ " != 0.0)"

let kernel ?(image = false) program =
  try
    (match P.gpu_refusals program with [] -> () | reason :: _ -> raise (Diagnostic.Error reason));
    let layout = Flow_ir.Kernel_layout.plan ~image program in
    let {
      Flow_ir.Kernel_layout.view;
      used;
      finite_checks;
      uniforms;
      frames;
      image_width;
      image_height;
      table_seeds;
      input_used;
      input_count = count;
      output_count = outputs;
      uniform_layout;
      uniform_binding;
      status_binding;
      table_binding;
      _;
    } =
      layout
    in
    if image && (view.widths <> [| 2 |] || Array.map Array.length view.outputs <> [| 4 |]) then
      fail "An image kernel needs one UV input and one vec4 output.";
    let image_width = Option.value image_width ~default:0 in
    let image_height = Option.value image_height ~default:0 in
    let word index = Printf.sprintf "uniforms[%d].%c" (index / 4) "xyzw".[index mod 4] in
    let has operation =
      Array.exists Fun.id
        (Array.mapi
           (fun slot instruction -> used.(slot) && operation (snd (guarded [] instruction)))
           view.code)
    in
    let table seed =
      match Array.find_index (( = ) seed) table_seeds with
      | Some index -> index * 512
      | None -> assert false
    in
    let root = Scope.create 0 in
    Array.iteri
      (fun slot instruction ->
        if used.(slot) then begin
          let guards, instruction = guarded [] instruction in
          enter register root guards;
          let body = root.code and indentation = Scope.indent root in
          let expression =
            match instruction with
            | P.Const value -> Printf.sprintf "bitcast<f32>(0x%lxu)" (Int32.bits_of_float value)
            | Input (_, _, component) when image ->
                Printf.sprintf "pixel_uv(%s, bitcast<f32>(%s))"
                  (if component = 0 then "i % image_width" else "i / image_width")
                  (word (if component = 0 then image_width else image_height))
            | Input (input, width, component) ->
                Printf.sprintf "input%d[i * %du + %du]" input width component
            | Uniform (index, component) ->
                Printf.sprintf "bitcast<f32>(%s)" (word (uniforms.(index) + component))
            | Frame _ -> Printf.sprintf "bitcast<f32>(%s)" (word frames.(slot))
            | Accumulator _ -> assert false
            | Binary (op, a, b) -> binary op (register a) (register b)
            | Unary (op, a) -> unary op (register a)
            | Checked_mul _ | Checked_fma _ ->
                fail "Checked arithmetic requires binary64 overflow status."
            | Trace _ -> fail "scene/trace needs a native ray-query host."
            | Stage _ -> fail "A raster-stage intrinsic runs only in a fragment stage."
            | Array_prepare _ | Gather _ | Int_gather _ ->
                fail "Indexed array reads stay on the CPU."
            | Text_const _ | Text_input _ | Text_uniform _ | Text_gather _ | Text_select _ ->
                fail "Text kernels stay on the CPU."
            | Select (condition, yes, no) ->
                Printf.sprintf "select(%s, %s, %s != 0.0)" (register no) (register yes)
                  (register condition)
            | Noise3 (x, y, z, seed, octaves, lacunarity, gain) ->
                noise_call ~register
                  ~count:(fun _ ->
                    fail "Integer instructions are not supported by the WGSL kernel.")
                  ~number:(fun v -> Printf.sprintf "bitcast<f32>(0x%lxu)" (Int32.bits_of_float v))
                  ~table
                  (x, y, z, seed, octaves, lacunarity, gain)
            | _ ->
                fail
                  "Integer instructions and conversions are not supported by the WGSL kernel \
                   target."
          in
          if guards = [] then
            Printf.bprintf body "%slet r%d: f32 = %s;\n" indentation slot expression
          else begin
            Printf.bprintf root.decls "  var r%d: f32 = 0.0;\n" slot;
            Printf.bprintf body "%sr%d = %s;\n" indentation slot expression
          end;
          if finite_checks.(slot) then
            Printf.bprintf body "%sif (!wfinite(r%d)) { sb |= 1u; }\n" indentation slot
        end)
      view.code;
    enter register root [];
    let body = Buffer.create 4096 in
    Buffer.add_buffer body root.decls;
    Buffer.add_buffer body root.code;
    Printf.bprintf body "  %s\n" flush_status;
    let output_name k = if outputs = 1 then "outbuf" else "outbuf" ^ string_of_int k in
    Array.iteri
      (fun k registers ->
        Array.iteri
          (fun component slot ->
            Printf.bprintf body "  %s[i * %du + %du] = r%d;\n" (output_name k)
              (Array.length registers) component slot)
          registers)
      view.outputs;
    let uniform_words = Flow_ir.Kernel_layout.uniform_words layout in
    let declarations = Buffer.create 512 in
    let declare binding kind decl =
      Printf.bprintf declarations "@group(0) @binding(%d) var%s %s;\n" binding
        (match kind with
        | Uniform_buffer -> "<uniform>"
        | Storage_read -> "<storage, read>"
        | Storage_read_write -> "<storage, read_write>")
        decl
    in
    let interface = ref [] in
    let bind binding kind decl =
      interface := { group = 0; binding; kind } :: !interface;
      declare binding kind decl
    in
    Array.iteri
      (fun index used ->
        if used then bind index Storage_read (Printf.sprintf "input%d: array<f32>" index))
      input_used;
    for k = 0 to outputs - 1 do
      bind (count + k) Storage_read_write (output_name k ^ ": array<f32>")
    done;
    bind uniform_binding Uniform_buffer
      (Printf.sprintf "uniforms: array<vec4<u32>, %d>" uniform_words);
    bind status_binding Storage_read_write "status: atomic<u32>";
    Option.iter (fun binding -> bind binding Storage_read "table: array<i32>") table_binding;
    let prefix =
      Buffer.contents declarations ^ finite
      ^ (if image then image_uv else "")
      ^ (if has (function P.Binary (P.Pow, _, _) -> true | _ -> false) then power else "")
      ^ (if table_seeds = [||] then "" else perlin)
      ^ if has fractal_noise then perlin_fractal else ""
    in
    let signature = "(@builtin(global_invocation_id) gid: vec3<u32>)" in
    let body =
      "  let i = gid.x;\n  if (i >= uniforms[0].x) { return; }\n  var sb: u32 = 0u;\n"
      ^ (if image then
           Printf.sprintf "  let image_width = u32(bitcast<f32>(%s));\n" (word image_width)
         else "")
      ^ Buffer.contents body
    in
    let entry = "kernel_" ^ Digest.to_hex (Digest.string (prefix ^ signature ^ body)) in
    let source =
      prefix ^ "@compute @workgroup_size(64)\nfn " ^ entry ^ signature ^ " {\n" ^ body ^ "}\n"
    in
    Ok
      {
        source;
        entry;
        interface = List.rev !interface;
        uniform_layout;
        uniform_bytes = uniform_words * 16;
        table_seeds;
        output_widths = Array.map Array.length view.outputs;
        input_widths = (if image then [||] else view.widths);
        gathers = [||];
        counts = None;
      }
  with Diagnostic.Error diagnostic -> Error diagnostic

(* Fast precision's 64-bit integer registers as [vec2<u32>] (low, high), as the Metal kernel's
   [long]s: wrapping add, sub and mul, truncating division ([0] for a zero divisor), logical right
   shift, shifts by [n & 63]. *)
let int64 =
  {|fn i64_add(a: vec2<u32>, b: vec2<u32>) -> vec2<u32> { let lo = a.x + b.x; return vec2<u32>(lo, a.y + b.y + select(0u, 1u, lo < a.x)); }
fn i63(a: vec2<u32>) -> vec2<u32> { return vec2<u32>(a.x, bitcast<u32>(bitcast<i32>(a.y << 1u) >> 1u)); }
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
    if (!i64_lt(r, ub) || bitcast<i32>(r.y) < 0) {
      r = i64_sub(r, ub);
      if (k >= 32) { q.y = q.y | (1u << u32(k - 32)); } else { q.x = q.x | (1u << u32(k)); }
    }
  }
  if (rem) { return select(r, i64_sub(vec2<u32>(0u, 0u), r), i64_neg(a)); }
  return select(q, i64_sub(vec2<u32>(0u, 0u), q), i64_neg(a) != i64_neg(b));
}
|}

(* The web counterpart of [Flow_shader.Emit.kernel ~fast]: the same refusals, bindings and uniform
   layout, gathers from bound tables (f32, clamped, their length a uniform word), 64-bit integer
   registers ([int64]), and the count words: the element count is the dispatch count, or its minimum
   with [count_in[0] * mul / div] when use is set, and is written to the 40-byte count record
   [count_out] {n, 6n, 1, 0, 0, 0, 6, n, 0, 0}: native's six words, then from byte 24 WebGPU's
   [drawIndirect] of n six-vertex instances, which a mark set reads. *)
(* A compute stage (Flow_shader.Raster's): workgroups of [size] threads, the global index the entry's
   element and [lid] the scan's; its buffers [(array uniform, binding, u32 words, stored)], its
   result discarded. *)
type compute = { size : int; name : string; buffers : (int * int * bool * bool) list }

(* A raster stage (Flow_shader.Raster.stages): its buffers (array uniform, binding, u32 words), element
   parameters and result leaves (name, type, flat). *)
type stage = {
  entry : string;
  program : P.t;
  buffers : (int * int * bool) list;
  elements : string list;
  outputs : (string * Flow.Ty.t * bool) list;
}

(* a vertex stage (with [quads], of a six-vertex instanced draw) or a fragment stage of the struct named *)
type placement = Compute of compute | Vertex of bool * stage | Fragment of string * stage

let identifier name = String.map (fun c -> if c = '-' then '_' else c) name

let compute_buffer (c : compute) u =
  match List.find_opt (fun (v, _, _, _) -> v = u) c.buffers with
  | Some (_, binding, _, _) -> "buffer" ^ string_of_int binding
  | None -> fail "A compute stage reads an unbound table."

(* A compute stage's group scan (Hillis-Steele in workgroup memory between barriers, once a scanned
   register, in uniform control flow) and store (a statement before its register's 1), as the Metal
   printer's. *)
let workgroup_intrinsic ~scans ~workgroup ~(compute : compute) ~(current : Scope.t) ~guards ~body
    ~indentation ~table ~low ~float_of stage (r : int array) =
  match stage with
  | Flow_ir.Core.Region.Group_scan field ->
      if current.Scope.id <> 0 || guards <> [] then
        fail "A group scan runs in uniform control flow, outside loops and branches.";
      let name = Printf.sprintf "scan%d" r.(0) and x = low r.(0) and size = compute.size in
      if not (Hashtbl.mem scans r.(0)) then begin
        Hashtbl.add scans r.(0) ();
        Printf.bprintf workgroup "var<workgroup> %s: array<u32, %d>;\n" name size;
        Printf.bprintf current.Scope.decls "  var %s_before: u32 = 0u;\n  var %s_total: u32 = 0u;\n"
          name name;
        Printf.bprintf body
          "  { var v: u32 = %s; %s[lid] = v; for (var d = 1u; d < %du; d = d << 1u) { \
           workgroupBarrier(); var w = 0u; if (lid >= d) { w = %s[lid - d]; } workgroupBarrier(); \
           v = v + w; %s[lid] = v; } workgroupBarrier(); %s_before = v - %s; %s_total = %s[%du]; }\n"
          x name size name name name x name name (size - 1)
      end;
      name ^ if field = 0 then "_before" else "_total"
  | Store ->
      let u, ty = table r.(0) in
      Printf.bprintf body "%s%s[%s] = %s;\n" indentation (compute_buffer compute u)
        (low r.(1))
        (if ty = Flow.Ty.Array Flow.Ty.Int then low r.(2) else float_of r.(2));
      "1u"
  | _ -> fail "A raster-stage intrinsic runs only in a fragment stage."

let rec kernel_fast program = fast_kernel program

and compute ~size ~name ~buffers program =
  Result.map (fun w -> w.source) (fast_kernel ~placement:(Compute { size; name; buffers }) program)

and fast_kernel ?placement program =
  try
    (match P.gpu_refusals ~fused:true ~fast:true program with
    | [] -> ()
    | reason :: _ -> raise (Diagnostic.Error reason));
    let layout = Flow_ir.Kernel_layout.plan ~fast:true program in
    let {
      Flow_ir.Kernel_layout.view;
      used;
      finite_checks;
      uniforms;
      frames;
      table_seeds;
      gathers;
      gather_lengths;
      input_used;
      input_count = count;
      output_count = outputs;
      uniform_layout;
      uniform_binding;
      status_binding;
      table_binding;
      gather_base;
      count_base;
      counts;
    } =
      layout
    in
    let word index = Printf.sprintf "uniforms[%d].%c" (index / 4) "xyzw".[index mod 4] in
    let has operation =
      Array.exists Fun.id
        (Array.mapi
           (fun slot instruction -> used.(slot) && operation (snd (guarded [] instruction)))
           view.code)
    in
    let table seed =
      match Array.find_index (( = ) seed) table_seeds with
      | Some index -> index * 512
      | None -> assert false
    in
    let gather_index slot =
      match List.find_index (fun (s, _) -> s = slot) gathers with
      | Some k -> k
      | None -> fail "Gather without its table."
    in
    let integer slot = Flow_ir.Core.Region.integer_register view.code.(slot) in
    let narrow = layout.narrow in
    let int_of r =
      if narrow.(r) then "vec2<u32>(" ^ register r ^ ", 0u)"
      else if integer r then register r
      else "i64_from_f(" ^ register r ^ ")"
    in
    let float_of r = if integer r then "i64_to_f(" ^ int_of r ^ ")" else register r in
    (* a register's low 32 bits as a u32 (as [Flow_shader.Emit]'s narrow registers) *)
    let low r = if narrow.(r) then register r else "(" ^ int_of r ^ ").x" in
    let narrow_integer operation a b =
      let shift operator =
        let k = "(" ^ int_of b ^ ")" in
        Printf.sprintf "select(%s %s (%s.x & 31u), 0u, (%s.x & 63u) > 31u)" a operator k k
      in
      match operation with
      | Flow.Packed_ops.Iadd -> "(" ^ a ^ " + " ^ low b ^ ")"
      | Isub -> "(" ^ a ^ " - " ^ low b ^ ")"
      | Imul -> "(" ^ a ^ " * " ^ low b ^ ")"
      | Iand -> "(" ^ a ^ " & " ^ low b ^ ")"
      | Ior -> "(" ^ a ^ " | " ^ low b ^ ")"
      | Ixor -> "(" ^ a ^ " ^ " ^ low b ^ ")"
      | Ishl -> shift "<<"
      | Ishr -> shift ">>"
      | Idiv -> Printf.sprintf "select(%s / %s, 0u, %s == 0u)" a (low b) (low b)
      | Imod -> Printf.sprintf "select(%s %% %s, 0u, %s == 0u)" a (low b) (low b)
    in
    (* an integer-valued register as a 64-bit expression (as [Flow_shader.Emit]'s) *)
    let rec whole r =
      if integer r then Some (int_of r)
      else
        match snd (guarded [] view.code.(r)) with
        | P.Convert (To_float, x) when integer x -> Some (int_of x)
        | P.Const c when Float.is_integer c && Float.abs c < 9007199254740992. ->
            let n = Int64.of_float c in
            Some
              (Printf.sprintf "vec2<u32>(%Luu, %Luu)" (Int64.logand n 0xffffffffL)
                 (Int64.logand (Int64.shift_right_logical n 32) 0xffffffffL))
        | P.Binary (((Add | Sub | Mul | Mod | Min | Max) as op), a, b) -> (
            match (whole a, whole b) with
            | Some a, Some b ->
                Some
                  (match op with
                  | P.Add -> "i64_add(" ^ a ^ ", " ^ b ^ ")"
                  | Sub -> "i64_sub(" ^ a ^ ", " ^ b ^ ")"
                  | Mul -> "i64_mul(" ^ a ^ ", " ^ b ^ ")"
                  | Mod -> "i64_fmod(" ^ a ^ ", " ^ b ^ ")"
                  | Min -> "select(" ^ b ^ ", " ^ a ^ ", i64_lt(" ^ a ^ ", " ^ b ^ "))"
                  | _ -> "select(" ^ a ^ ", " ^ b ^ ", i64_lt(" ^ a ^ ", " ^ b ^ "))")
            | _ -> None)
        | _ -> None
    in
    let element prepared index width component =
      let k = gather_index prepared in
      Printf.sprintf "gather%d[u32(clamp(i32(%s), 0, max(i32(%s) - 1, 0))) * %du + %du]" k
        (if narrow.(index) then "bitcast<i32>(" ^ register index ^ ")" else float_of index)
        (word gather_lengths.(k))
        width component
    in
    let compute = match placement with Some (Compute c) -> Some c | _ -> None in
    let placed = placement <> None and raster = placement <> None && compute = None in
    let buffers =
      match placement with
      | Some (Compute c) -> c.buffers
      | Some (Vertex (_, s) | Fragment (_, s)) -> List.map (fun (u, b, w) -> (u, b, w, false)) s.buffers
      | None -> []
    in
    (* a placed stage reads its bound buffers unclamped, as the Metal raster placement; a float from a
       binding of words is their bits *)
    let read prepared index width component =
      match snd (guarded [] view.code.(prepared)) with
      | P.Array_prepare (u, ty) ->
          let binding =
            match List.find_opt (fun (v, _, _, _) -> v = u) buffers with
            | Some (_, b, _, _) -> b
            | None -> fail "A placed stage reads an unbound table."
          in
          let value = Printf.sprintf "buffer%d[%s * %du + %du]" binding (low index) width component in
          if ty <> Flow.Ty.Array Int && List.exists (fun (_, b, w, _) -> b = binding && w) buffers
          then "bitcast<f32>(" ^ value ^ ")"
          else value
      | _ -> fail "Gather without its table."
    in
    (* a fragment's varying, and its fwidth: read once at the top, before any branch (WGSL's derivative
       uniformity) *)
    let varying input width component =
      match placement with
      | Some (Fragment (_, s)) ->
          "v." ^ identifier (List.nth s.elements input)
          ^ if width = 1 then "" else "." ^ String.make 1 "xyzw".[component]
      | _ -> fail "Only a fragment stage reads varyings."
    in
    let fwidths = Buffer.create 64 and hoisted = Hashtbl.create 4 in
    let intrinsic stage (r : int array) =
      match (stage, snd (guarded [] view.code.(r.(0)))) with
      | Flow_ir.Core.Region.Fwidth, P.Input (input, width, component) ->
          let name = Printf.sprintf "fw%d_%d" input component in
          if not (Hashtbl.mem hoisted name) then begin
            Hashtbl.add hoisted name ();
            Printf.bprintf fwidths "  let %s = fwidth(%s);\n" name (varying input width component)
          end;
          name
      | Fwidth, _ -> fail "On the web fwidth reads a varying (WGSL's derivative uniformity)."
      | Float_bits, _ -> "bitcast<f32>(" ^ low r.(0) ^ ")"
      | Sample (t, c), _ ->
          Printf.sprintf "textureSample(texture%d, sampler%d, vec2<f32>(%s, %s)).%c" t (t + 1)
            (float_of r.(0)) (float_of r.(1)) "xyzw".[c]
      | _ -> fail "This raster-stage intrinsic has no WGSL form."
    in
    let scans = Hashtbl.create 1 and workgroup = Buffer.create 64 in
    let root = Scope.create 0 in
    let scopes = ref [ root ] in
    Flow_ir.Kernel_layout.iter layout
      ~instruction:(fun slot instruction ->
        if
          used.(slot)
          && match snd (guarded [] instruction) with P.Array_prepare _ -> false | _ -> true
        then begin
          let current = List.hd !scopes in
          let guards, instruction = guarded [] instruction in
          let constant =
            match instruction with
            | P.Const _ | Int_const _ -> not finite_checks.(slot)
            | _ -> false
          in
          if not constant then enter float_of current guards;
          let body = current.code and indentation = Scope.indent current in
          let group stage r =
            workgroup_intrinsic ~scans ~workgroup ~compute:(match compute with Some c -> c | None -> fail "A workgroup intrinsic runs only in a compute stage.") ~current ~guards
              ~body ~indentation
              ~table:(fun x ->
                match snd (guarded [] view.code.(x)) with
                | P.Array_prepare (u, ty) -> (u, ty)
                | _ -> fail "A store names a bound buffer.")
              ~low ~float_of stage r
          in
          let expression =
            if narrow.(slot) then
              match instruction with
              | P.Int_const n -> Printf.sprintf "%du" (n land 0xffffffff)
              | Int_binary (op, a, b) -> narrow_integer op (low a) b
              | Int_select (condition, yes, no) ->
                  Printf.sprintf "select(%s, %s, %s != 0.0)" (low no) (low yes) (float_of condition)
              | Loop_result (loop, component, _) -> register (loop + 1 + component)
              | Stage (stage, r) when compute <> None -> group stage r
              | _ -> assert false
            else
              match instruction with
              | P.Const value -> Printf.sprintf "bitcast<f32>(0x%lxu)" (Int32.bits_of_float value)
              | Int_input input -> (
                  match placement with
                  | Some (Compute _) -> "vec2<u32>(gid.x, 0u)"
                  | Some (Vertex (true, _)) -> "vec2<u32>(iid * 4u + quad_corner[vid], 0u)"
                  | Some (Vertex (false, _)) -> "vec2<u32>(vid, 0u)"
                  | Some (Fragment _) -> "vec2<u32>(0u, 0u)"
                  | None -> Printf.sprintf "i64_from_f(input%d[i])" input)
              | Stage (stage, r) when compute <> None -> "vec2<u32>(" ^ group stage r ^ ", 0u)"
              | Stage (stage, r) when raster -> intrinsic stage r
              | Input (input, width, component) when raster -> varying input width component
              | Gather (prepared, index, width, component) when placed ->
                  read prepared index width component
              | Int_gather (prepared, index) when placed ->
                  "vec2<u32>(" ^ read prepared index 1 0 ^ ", 0u)"
              | Input (input, width, component) ->
                  Printf.sprintf "input%d[i * %du + %du]" input width component
              | Uniform (index, component) ->
                  Printf.sprintf "bitcast<f32>(%s)" (word (uniforms.(index) + component))
              | Frame _ -> Printf.sprintf "bitcast<f32>(%s)" (word frames.(slot))
              | Accumulator _ -> assert false
              (* arithmetic on two integers stays in 64 bits: a float register would round them first *)
              | Binary ((Add | Sub | Mul | Mod | Min | Max), _, _) when whole slot <> None ->
                  "i64_to_f(" ^ Option.get (whole slot) ^ ")"
              | Convert ((Truncate | Coerce_int | Integer_result _ | Floor | Ceil | Round), a)
                when whole a <> None ->
                  Option.get (whole a)
              | Binary (op, a, b) -> binary op (float_of a) (float_of b)
              | Unary (op, a) -> unary op (float_of a)
              | Fma (a, b, c) ->
                  Printf.sprintf "fma(%s, %s, %s)" (register a) (register b) (register c)
              | Checked_mul _ | Checked_fma _ ->
                  fail "Checked arithmetic requires binary64 overflow status."
              | Trace _ -> fail "scene/trace needs a native ray-query host."
              | Stage _ -> fail "A raster-stage intrinsic runs only in a fragment stage."
              | Gather (prepared, index, width, component) -> element prepared index width component
              | Int_gather (prepared, index) -> "i64_from_f(" ^ element prepared index 1 0 ^ ")"
              | Array_prepare _ -> assert false
              | Int_const n ->
                  let n = Int64.of_int n in
                  Printf.sprintf "vec2<u32>(%Luu, %Luu)" (Int64.logand n 0xffffffffL)
                    (Int64.logand (Int64.shift_right_logical n 32) 0xffffffffL)
              | Int_uniform index ->
                  Printf.sprintf "i64_from_f(bitcast<f32>(%s))" (word uniforms.(index))
              | Int_frame _ -> Printf.sprintf "i64_from_f(bitcast<f32>(%s))" (word frames.(slot))
              | Int_binary (op, a, b) ->
                  let a = int_of a and b = int_of b in
                  "i63("
                  ^ (match op with
                    | Flow.Packed_ops.Iadd -> "i64_add(" ^ a ^ ", " ^ b ^ ")"
                    | Isub -> "i64_sub(" ^ a ^ ", " ^ b ^ ")"
                    | Imul -> "i64_mul(" ^ a ^ ", " ^ b ^ ")"
                    | Idiv -> "i64_divmod(" ^ a ^ ", " ^ b ^ ", false)"
                    | Imod -> "i64_divmod(" ^ a ^ ", " ^ b ^ ", true)"
                    | Iand -> "(" ^ a ^ " & " ^ b ^ ")"
                    | Ior -> "(" ^ a ^ " | " ^ b ^ ")"
                    | Ixor -> "(" ^ a ^ " ^ " ^ b ^ ")"
                    | Ishl -> "i64_shl(" ^ a ^ ", " ^ b ^ ")"
                    | Ishr ->
                        "i64_shr((" ^ a ^ " & vec2<u32>(0xffffffffu, 0x7fffffffu)), " ^ b ^ ")")
                  ^ ")"
              | Int_compare (op, a, b) ->
                  let a = int_of a and b = int_of b in
                  truth
                    (match op with
                    | Flow.Packed_ops.Ilt -> "i64_lt(" ^ a ^ ", " ^ b ^ ")"
                    | Igt -> "i64_lt(" ^ b ^ ", " ^ a ^ ")"
                    | Ile -> "!i64_lt(" ^ b ^ ", " ^ a ^ ")"
                    | Ige -> "!i64_lt(" ^ a ^ ", " ^ b ^ ")"
                    | Ieq -> "all(" ^ a ^ " == " ^ b ^ ")")
              | Int_abs a -> "i63(i64_abs(" ^ int_of a ^ "))"
              | Int_select (condition, yes, no) ->
                  Printf.sprintf "select(%s, %s, %s != 0.0)" (int_of no) (int_of yes)
                    (float_of condition)
              | Convert (To_float, a) -> float_of a
              | Convert ((Truncate | Coerce_int | Integer_result _), a) -> int_of a
              | Convert (Floor, a) -> "i64_from_f(floor(" ^ float_of a ^ "))"
              | Convert (Ceil, a) -> "i64_from_f(ceil(" ^ float_of a ^ "))"
              | Convert (Round, a) ->
                  "i64_from_f(sign(" ^ float_of a ^ ") * floor(abs(" ^ float_of a ^ ") + 0.5))"
              | Text_const _ | Text_input _ | Text_uniform _ | Text_gather _ | Text_select _ ->
                  fail "Text kernels stay on the CPU."
              | Select (condition, yes, no) ->
                  Printf.sprintf "select(%s, %s, %s != 0.0)" (register no) (register yes)
                    (register condition)
              | Noise3 (x, y, z, seed, octaves, lacunarity, gain) ->
                  noise_call ~register ~count:int_of
                    ~number:(fun v -> Printf.sprintf "bitcast<f32>(0x%lxu)" (Int32.bits_of_float v))
                    ~table
                    (x, y, z, seed, octaves, lacunarity, gain)
              | Loop_result (loop, component, _) -> register (loop + 1 + component)
              | _ -> fail "This instruction is not supported by the WGSL fast kernel target."
          in
          let ty, zero =
            if narrow.(slot) then ("u32", "0u")
            else if integer slot then ("vec2<u32>", "vec2<u32>(0u, 0u)")
            else ("f32", "0.0")
          in
          if constant then Printf.bprintf current.decls "  let r%d: %s = %s;\n" slot ty expression
          else if current.opened = [] then
            Printf.bprintf body "%slet r%d: %s = %s;\n" indentation slot ty expression
          else begin
            Printf.bprintf current.decls "  var r%d: %s = %s;\n" slot ty zero;
            Printf.bprintf body "%sr%d = %s;\n" indentation slot expression
          end;
          if finite_checks.(slot) && (not (integer slot)) && not raster then
            Printf.bprintf body "%sif (!wfinite(r%d)) { sb |= 1u; }\n" indentation slot;
          match instruction with
          | Int_binary (operation, _, b) ->
              Buffer.add_string body (indentation ^ integer_status operation (int_of b))
          | _ -> ()
        end)
      ~loop:(fun header control ~next ~stop ~body:emit_body ->
        if used.(header) then begin
          let outer = List.hd !scopes in
          let guards, _ = guarded [] view.code.(header) in
          let ty slot =
            if narrow.(slot) then "u32" else if integer slot then "vec2<u32>" else "f32"
          in
          let zero slot =
            if narrow.(slot) then "0u" else if integer slot then "vec2<u32>(0u, 0u)" else "0.0"
          in
          let carried slot source =
            if narrow.(slot) then low source
            else if integer slot then int_of source
            else register source
          in
          Printf.bprintf outer.decls "  var r%d: %s = %s;\n" header (ty header) (zero header);
          Array.iteri
            (fun component _ ->
              let slot = header + 1 + component in
              Printf.bprintf outer.decls "  var r%d: %s = %s;\n" slot (ty slot) (zero slot))
            control.initial;
          enter float_of outer guards;
          let body = outer.code and indentation = Scope.indent outer in
          (* a pass count over the limit flags the status and runs no pass (as the Metal printer) *)
          Option.iter
            (fun limit ->
              let bound = Int64.of_int limit in
              Printf.bprintf body
                "%slet over%d = i64_lt(%s, %s) && i64_lt(vec2<u32>(%Luu, %Luu), i64_sub(%s, %s));\n\
                 %sif (over%d) { sb |= 1u; }\n"
                indentation header (int_of control.first) (int_of control.last)
                (Int64.logand bound 0xffffffffL)
                (Int64.shift_right_logical bound 32)
                (int_of control.last) (int_of control.first) indentation header)
            control.limit;
          Array.iteri
            (fun component initial ->
              let slot = header + 1 + component in
              Printf.bprintf body "%sr%d = %s;\n" indentation slot (carried slot initial))
            control.initial;
          let last bound =
            if control.limit = None then bound control.last
            else
              Printf.sprintf "select(%s, %s, over%d)" (bound control.last) (bound control.first)
                header
          in
          if narrow.(header) then
            Printf.bprintf body "%sfor (r%d = %s; r%d < %s; r%d = r%d + 1u) {\n" indentation header
              (low control.first) header (last low) header header
          else
            Printf.bprintf body
              "%sfor (r%d = %s; i64_lt(r%d, %s); r%d = i64_add(r%d, vec2<u32>(1u, 0u))) {\n"
              indentation header (int_of control.first) header (last int_of) header header;
          let inner = Scope.create header in
          scopes := inner :: !scopes;
          emit_body ();
          enter float_of inner [];
          scopes := List.tl !scopes;
          Buffer.add_buffer body inner.decls;
          Buffer.add_buffer body inner.code;
          Option.iter
            (fun source ->
              Printf.bprintf body "  let stop%d = %s != 0.0;\n" header (float_of source))
            stop;
          Array.iteri
            (fun component source ->
              let slot = header + 1 + component in
              Printf.bprintf body "  let next%d_%d: %s = %s;\n" header component (ty slot)
                (carried slot source))
            next;
          Array.iteri
            (fun component _ ->
              Printf.bprintf body "  r%d = next%d_%d;\n" (header + 1 + component) header component)
            next;
          Option.iter (fun _ -> Printf.bprintf body "  if (stop%d) { break; }\n" header) stop;
          Printf.bprintf body "%s}\n" indentation
        end);
    enter float_of root [];
    let body = Buffer.create 4096 in
    Buffer.add_buffer body root.decls;
    Buffer.add_buffer body root.code;
    match placement with
    | Some (Vertex (_, s) | Fragment (_, s)) ->
        let stores = Buffer.create 256 in
        Array.iteri
          (fun k registers ->
            let name, _, _ = List.nth s.outputs k in
            Array.iteri
              (fun component slot ->
                let c = String.make 1 "xyzw".[component] in
                match placement with
                | _ when name = "discard" ->
                    Printf.bprintf stores "  if (%s != 0.0) { discard; }\n" (float_of slot)
                | Some (Fragment _) -> Printf.bprintf stores "  colour.%s = %s;\n" c (float_of slot)
                | _ ->
                    Printf.bprintf stores "  o.%s%s = %s;\n" (identifier name)
                      (if Array.length registers = 1 then "" else "." ^ c)
                      (float_of slot))
              registers)
          view.outputs;
        let entry = identifier s.entry in
        let source =
          match placement with
          | Some (Fragment (out, _)) ->
              Printf.sprintf
                "@fragment\nfn %s(v: %s) -> @location(0) vec4<f32> {\n\
                 %s  var sb: u32 = 0u;\n  var colour: vec4<f32>;\n%s%s  return colour;\n}\n"
                entry out (Buffer.contents fwidths) (Buffer.contents body) (Buffer.contents stores)
          | _ ->
              Printf.sprintf
                "@vertex\nfn %s(@builtin(vertex_index) vid: u32, @builtin(instance_index) iid: u32) \
                 -> %s_out {\n  var sb: u32 = 0u;\n  var o: %s_out;\n%s%s  return o;\n}\n"
                entry entry entry (Buffer.contents body) (Buffer.contents stores)
        in
        Ok
          {
            source;
            entry;
            interface = [];
            uniform_layout = [];
            uniform_bytes = 0;
            table_seeds = [||];
            output_widths = [||];
            input_widths = [||];
            gathers = [||];
            counts = None;
          }
    | Some (Compute { size; name; buffers }) ->
        let declarations =
          String.concat ""
            (List.map
               (fun (_, binding, words, stored) ->
                 Printf.sprintf "@group(0) @binding(%d) var<storage, %s> buffer%d: array<%s>;\n"
                   binding
                   (if stored then "read_write" else "read")
                   binding
                   (if words then "u32" else "f32"))
               buffers)
        in
        let source =
          declarations ^ Buffer.contents workgroup ^ int64
          ^ Printf.sprintf
              "@compute @workgroup_size(%d)\n\
               fn %s(@builtin(local_invocation_index) lid: u32, \
               @builtin(global_invocation_id) gid: vec3<u32>) {\n\
              \  var sb: u32 = 0u;\n\
               %s}\n"
              size name (Buffer.contents body)
        in
        Ok
          {
            source;
            entry = name;
            interface = [];
            uniform_layout = [];
            uniform_bytes = 0;
            table_seeds = [||];
            output_widths = [||];
            input_widths = [||];
            gathers = [||];
            counts = None;
          }
    | None ->
        if layout.output_checks then
          Array.iter
            (Array.iter (fun slot ->
                 if not (integer slot) then
                   Printf.bprintf body "  if (!wfinite(r%d)) { sb |= 1u; }\n" slot))
            view.outputs;
        Printf.bprintf body "  %s\n" flush_status;
        let output_name k = if outputs = 1 then "outbuf" else "outbuf" ^ string_of_int k in
        Array.iteri
          (fun k registers ->
            Array.iteri
              (fun component slot ->
                Printf.bprintf body "  %s[i * %du + %du] = %s;\n" (output_name k)
                  (Array.length registers) component (float_of slot))
              registers)
          view.outputs;
        let counts = Option.get counts in
        let uniform_words = Flow_ir.Kernel_layout.uniform_words layout in
        let declarations = Buffer.create 512 in
        let interface = ref [] in
        let bind binding kind decl =
          interface := { group = 0; binding; kind } :: !interface;
          Printf.bprintf declarations "@group(0) @binding(%d) var%s %s;\n" binding
            (match kind with
            | Uniform_buffer -> "<uniform>"
            | Storage_read -> "<storage, read>"
            | Storage_read_write -> "<storage, read_write>")
            decl
        in
        Array.iteri
          (fun index used ->
            if used then bind index Storage_read (Printf.sprintf "input%d: array<f32>" index))
          input_used;
        for k = 0 to outputs - 1 do
          bind (count + k) Storage_read_write (output_name k ^ ": array<f32>")
        done;
        bind uniform_binding Uniform_buffer
          (Printf.sprintf "uniforms: array<vec4<u32>, %d>" uniform_words);
        bind status_binding Storage_read_write "status: atomic<u32>";
        Option.iter (fun binding -> bind binding Storage_read "table: array<i32>") table_binding;
        List.iteri
          (fun k _ -> bind (gather_base + k) Storage_read (Printf.sprintf "gather%d: array<f32>" k))
          gathers;
        bind count_base Storage_read "count_in: array<u32>";
        bind (count_base + 1) Storage_read_write "count_out: array<u32>";
        let prefix =
          Buffer.contents declarations ^ finite ^ int64
          ^ (if has (function P.Binary (P.Pow, _, _) -> true | _ -> false) then power else "")
          ^ (if table_seeds = [||] then "" else perlin)
          ^ (if has fractal_noise then perlin_fractal else "")
          ^ if has live_noise then perlin_live else ""
        in
        let signature = "(@builtin(global_invocation_id) gid: vec3<u32>)" in
        let w = counts / 4 in
        let body =
          Printf.sprintf
            "  let i = gid.x;\n\
            \  var n = uniforms[0].x;\n\
            \  if (%s != 0u) { n = min(n, count_in[0] * %s / %s); }\n\
            \  if (i == 0u) { count_out[0] = n; count_out[1] = 6u * n; count_out[2] = 1u; \
             count_out[3] = 0u; count_out[4] = 0u; count_out[5] = 0u; count_out[6] = 6u; \
             count_out[7] = n; count_out[8] = 0u; count_out[9] = 0u; }\n\
            \  if (i >= n) { return; }\n\
            \  var sb: u32 = 0u;\n"
            (word (w + 2))
            (word w)
            (word (w + 1))
          ^ Buffer.contents body
        in
        let entry = "kernel_" ^ Digest.to_hex (Digest.string (prefix ^ signature ^ body)) in
        let source =
          prefix ^ "@compute @workgroup_size(64)\nfn " ^ entry ^ signature ^ " {\n" ^ body ^ "}\n"
        in
        Ok
          {
            source;
            entry;
            interface = List.rev !interface;
            uniform_layout;
            uniform_bytes = uniform_words * 16;
            table_seeds;
            output_widths = Array.map Array.length view.outputs;
            input_widths = view.widths;
            gathers = Array.of_list (List.map snd gathers);
            counts = Some counts;
          }
  with Diagnostic.Error diagnostic -> Error diagnostic

(* Raster pipelines (quads, vertex, fragment) as one module: every resource in group 0 at its binding
   (a binding of words declared [u32], a float binding [f32]), texture t with its sampler at t + 1,
   the varyings at locations in the vertex result's leaf order ([@interpolate(flat)] under [:flat]). *)
let raster pipelines =
  let ( let* ) = Result.bind in
  let ty = function
    | Flow.Ty.Float | Bool -> "f32"
    | Vec2 -> "vec2<f32>"
    | Vec3 -> "vec3<f32>"
    | Vec4 -> "vec4<f32>"
    | ty -> "<" ^ Flow.Ty.to_string ty ^ ">"
  in
  let resources = Hashtbl.create 16 and words = Hashtbl.create 16 in
  (* a binding any stage reads as words is words in every stage: its floats are their bits *)
  List.iter
    (fun (_, (v : stage), (f : stage)) ->
      List.iter (fun (_, b, w) -> if w then Hashtbl.replace words b ()) (v.buffers @ f.buffers))
    pipelines;
  let shared (s : stage) =
    { s with buffers = List.map (fun (u, b, w) -> (u, b, w || Hashtbl.mem words b)) s.buffers }
  in
  let rec functions = function
    | [] -> Ok []
    | (quads, v, f) :: more ->
        let v = shared v and f = shared f in
        let out = identifier v.entry ^ "_out" in
        let location = ref (-1) in
        let fields =
          List.map
            (fun (name, t, flat) ->
              if name = "position" then "  @builtin(position) position: vec4<f32>,\n"
              else begin
                incr location;
                Printf.sprintf "  @location(%d) %s%s: %s,\n" !location
                  (if flat then "@interpolate(flat) " else "")
                  (identifier name) (ty t)
              end)
            v.outputs
        in
        List.iter
          (fun (s : stage) ->
            List.iter
              (fun (_, b, w) -> Hashtbl.replace resources (`Buffer b) w)
              s.buffers;
            Array.iter
              (fun i ->
                match snd (guarded [] i) with
                | P.Stage (Sample (t, _), _) -> Hashtbl.replace resources (`Texture t) true
                | _ -> ())
              s.program.Flow_ir.Core.Region.code)
          [ v; f ];
        let* vertex = fast_kernel ~placement:(Vertex (quads, v)) v.program in
        let* fragment = fast_kernel ~placement:(Fragment (out, f)) f.program in
        let* rest = functions more in
        Ok
          ((Printf.sprintf "struct %s {\n%s}\n" out (String.concat "" fields)
           ^ vertex.source ^ fragment.source)
          :: rest)
  in
  let* text = functions pipelines in
  let declarations =
    List.sort compare (Hashtbl.fold (fun key words acc -> (key, words) :: acc) resources [])
    |> List.map (function
      | `Buffer b, words ->
          Printf.sprintf "@group(0) @binding(%d) var<storage, read> buffer%d: array<%s>;\n" b b
            (if words then "u32" else "f32")
      | `Texture t, _ ->
          Printf.sprintf
            "@group(0) @binding(%d) var texture%d: texture_2d<f32>;\n\
             @group(0) @binding(%d) var sampler%d: sampler;\n"
            t t (t + 1) (t + 1))
  in
  Ok
    (String.concat "" declarations
    ^ (if List.exists (fun (quads, _, _) -> quads) pipelines then
         "const quad_corner = array<u32, 6>(0u, 1u, 2u, 0u, 2u, 3u);\n"
       else "")
    ^ int64 ^ power ^ String.concat "" text)
