module P = Flow_ir.Packed
module R = Flow_ir.Core.Region

let number x = Param.float_text x
let register index = "r" ^ string_of_int index
let flag condition = "(" ^ condition ^ " ? 1 : 0)"

let binary operation a b =
  match operation with
  | P.Add -> a ^ " + " ^ b
  | Sub -> a ^ " - " ^ b
  | Mul -> a ^ " * " ^ b
  | Div -> b ^ " === 0 ? 0 : " ^ a ^ " / " ^ b
  | Mod -> b ^ " === 0 ? 0 : ((" ^ a ^ " % " ^ b ^ ") + " ^ b ^ ") % " ^ b
  | Pow -> "Math.pow(Math.abs(" ^ a ^ "), " ^ b ^ ")"
  | Min -> "Math.min(" ^ a ^ ", " ^ b ^ ")"
  | Max -> "Math.max(" ^ a ^ ", " ^ b ^ ")"
  | Lt -> flag (a ^ " < " ^ b)
  | Le -> flag (a ^ " <= " ^ b)
  | Gt -> flag (a ^ " > " ^ b)
  | Ge -> flag (a ^ " >= " ^ b)
  | Eq -> flag (a ^ " === " ^ b)
  | And -> flag (a ^ " !== 0 && " ^ b ^ " !== 0")
  | Or -> flag (a ^ " !== 0 || " ^ b ^ " !== 0")
  | Atan2 -> "Math.atan2(" ^ a ^ ", " ^ b ^ ")"
  | Hypot -> "Math.hypot(" ^ a ^ ", " ^ b ^ ")"
  | Ldexp -> "(" ^ a ^ " * 2 ** Math.trunc(" ^ b ^ "))"

let unary operation a =
  match operation with
  | P.Sin -> "Math.sin(" ^ a ^ ")"
  | Cos -> "Math.cos(" ^ a ^ ")"
  | Sqrt -> "Math.sqrt(Math.abs(" ^ a ^ "))"
  | Abs -> "Math.abs(" ^ a ^ ")"
  | Not -> flag (a ^ " === 0")
  | Tan -> "Math.tan(" ^ a ^ ")"
  | Asin -> "Math.asin(" ^ a ^ ")"
  | Acos -> "Math.acos(" ^ a ^ ")"
  | Exp -> "Math.exp(" ^ a ^ ")"
  | Log -> "Math.log(" ^ a ^ ")"
  | Sign -> "Math.sign(" ^ a ^ ")"
  | Rsqrt -> "(1 / Math.sqrt(Math.abs(" ^ a ^ ")))"

let fused =
  {|  const fmaBytes = new DataView(new ArrayBuffer(8));
  const fmaDecode = x => {
    fmaBytes.setFloat64(0, x, false);
    const b = fmaBytes.getBigUint64(0, false), exponent = Number((b >> 52n) & 2047n);
    const fraction = b & ((1n << 52n) - 1n);
    return {negative: !!(b >> 63n), n: exponent ? fraction | (1n << 52n) : fraction,
      e: exponent ? exponent - 1075 : -1074};
  };
  const fused = (a, b, c) => {
    if (!Number.isFinite(a) || !Number.isFinite(b) || !Number.isFinite(c)) return NaN;
    const aa = fmaDecode(a), bb = fmaDecode(b), cc = fmaDecode(c);
    const negative = aa.negative !== bb.negative, pe = aa.e + bb.e, e = Math.min(pe, cc.e);
    let n = aa.n * bb.n << BigInt(pe - e);
    if (negative) n = -n;
    const cn = cc.n << BigInt(cc.e - e);
    n += cc.negative ? -cn : cn;
    if (n === 0n) return aa.n * bb.n === 0n && cc.n === 0n && negative && cc.negative ? -0 : 0;
    const sign = n < 0n;
    if (sign) n = -n;
    const quantum = Math.max(-1074, e + n.toString(2).length - 53), shift = quantum - e;
    let rounded;
    if (shift > 0) {
      const s = BigInt(shift), q = n >> s, remainder = n - (q << s), half = 1n << (s - 1n);
      rounded = q + (remainder > half || (remainder === half && (q & 1n)) ? 1n : 0n);
    } else rounded = n << BigInt(-shift);
    const value = Number(rounded) * 2 ** quantum;
    return sign ? -value : value;
  };
|}

(* Fast precision's 64-bit integer registers, as the Metal kernel's [long]s: a register is a signed
   high and an unsigned low 32-bit half, both plain numbers; an operation returns the low half and
   leaves the high one in [RH], so nothing is allocated per element. Division and [mod] of values
   past 2^53 fall back to BigInt. *)
let fast64 =
  {|  let RH = 0;
  const P32 = 4294967296;
  const split = x => { const h = Math.floor(x / P32); RH = h | 0; return x - h * P32; };
  const unbig = v => { v = BigInt.asIntN(64, v); RH = Number(v >> 32n); return Number(BigInt.asUintN(32, v)); };
  const big = (h, l) => (BigInt(h) << 32n) + BigInt(l);
  const add64 = (ah, al, bh, bl) => { const s = al + bl; RH = (ah + bh + (s >= P32 ? 1 : 0)) | 0; return s >= P32 ? s - P32 : s; };
  const sub64 = (ah, al, bh, bl) => { const d = al - bl; RH = (ah - bh - (d < 0 ? 1 : 0)) | 0; return d < 0 ? d + P32 : d; };
  const mulhi = (a, b) => { const a0 = a & 0xffff, a1 = a >>> 16, b0 = b & 0xffff, b1 = b >>> 16;
    const m1 = a1 * b0 + Math.floor(a0 * b0 / 65536), m2 = a0 * b1 + (m1 & 0xffff);
    return (a1 * b1 + Math.floor(m1 / 65536) + Math.floor(m2 / 65536)) >>> 0; };
  const mul64 = (ah, al, bh, bl) => { RH = (mulhi(al, bl) + Math.imul(ah, bl) + Math.imul(al, bh)) | 0; return Math.imul(al, bl) >>> 0; };
  const div64 = (ah, al, bh, bl, mod) => {
    if (bh === 0 && bl === 0) fail('E_RANGE', 'Integer division by zero.');
    const a = ah * P32 + al, b = bh * P32 + bl;
    if (Math.abs(a) <= 9007199254740991 && Math.abs(b) <= 9007199254740991) return split(mod ? a % b : Math.trunc(a / b));
    return unbig(mod ? big(ah, al) % big(bh, bl) : big(ah, al) / big(bh, bl)); };
  const shift64 = (ah, al, b, right) => {
    if (b < 0 || b > 62) fail('E_RANGE', 'Integer shift count must be within [0, 62].');
    if (b === 0) { RH = ah; return al; }
    if (right) { ah &= 0x7fffffff; if (b >= 32) { RH = 0; return ah >>> (b - 32); } RH = (ah >>> b) | 0; return ((al >>> b) | (ah << (32 - b))) >>> 0; }
    if (b >= 32) { RH = (al << (b - 32)) | 0; return 0; }
    RH = ((ah << b) | (al >>> (32 - b))) | 0; return (al << b) >>> 0; };
|}

(* [fast] (the workspace's fast precision, a collecting region): integer registers are exact 64-bit
   pairs ([fast64]), as native fast precision's kernels hold them. *)
let fast_less a b = Printf.sprintf "(r%dh < r%dh || (r%dh === r%dh && r%dl < r%dl))" a b a b a b

let region ?(fast = false) program =
  let view = P.Host.view program and region = program in
  let fast =
    fast && region.result = R.Collect
    && not
         (Array.exists
            (fun i ->
              match i with P.Rand_at _ -> true | P.Guarded (_, P.Rand_at _) -> true | _ -> false)
            view.code)
  in
  let line = Buffer.create 4096 in
  let add format = Printf.bprintf line format in
  let rec unguarded = function
    | P.Guarded (_, body) -> unguarded body
    | instruction -> instruction
  in
  let checks = P.Host.finite_checks program in
  let gathered =
    Array.exists
      (fun instruction -> match unguarded instruction with P.Array_prepare _ -> true | _ -> false)
      view.code
  in
  let nonfinite =
    if gathered then "fail('E_NONFINITE', 'packed produced a nonfinite value.');" else "return 1;"
  in
  let collecting =
    match region.result with R.Collect | R.Accumulate (_, true) -> true | _ -> false
  in
  let accumulating = region.result <> R.Collect in
  let outputs = Array.length view.outputs and width = Array.length view.outputs.(0) in
  let textual = Array.exists R.text_register view.code in
  let state = if collecting then outputs else 0 in
  let sources = Array.length view.widths in
  let product = region.iteration = R.Product && sources > 1 in
  let skip =
    Array.to_list region.skip |> List.filter (fun index -> index >= 0) |> List.sort_uniq Int.compare
  in
  add "%s"
    (if textual then "function region(inputs, uniforms, outputs, start, stop, outputBase = 0) {\n"
     else "function region(inputs, uniforms, outputs, start, stop) {\n");
  if
    Array.exists
      (fun instruction ->
        match unguarded instruction with P.Raise (_, _, _, Some _) -> true | _ -> false)
      view.code
  then
    add
      "  const fail = (code, message, cause) => { const e = new Error(message); e.code = code; if \
       (cause !== undefined) e.cause = cause; throw e; };\n"
  else
    add
      "  const fail = (code, message) => { const e = new Error(message); e.code = code; throw e; };\n";
  if fast then add "%s" fast64 else add "  const wrap = x => BigInt.asIntN(63, x);\n";
  if
    Array.exists
      (fun instruction -> match unguarded instruction with P.Array_prepare _ -> true | _ -> false)
      view.code
  then begin
    add "  const tables = new Map();\n";
    add
      "  const table = (slot, integer, width) => { if (tables.has(slot)) return tables.get(slot); \
       let data = uniforms[slot];\n";
    add "    const text = integer === 'text';\n";
    add
      "    if (text ? !Array.isArray(data) : !(data instanceof (integer ? BigInt64Array : \
       Float64Array))) fail('E_ARRAY_TYPE', 'Kernel lookup table changed its packed element \
       type.');\n";
    add
      "    if (text && !data.every(x => typeof x === 'string')) fail('E_ARRAY_TYPE', 'Text arrays \
       contain text values.');\n";
    add
      "    if (data.length %% width !== 0) fail('E_ARRAY_TYPE', `Packed vec${width} storage has \
       ${['','','two','three','four'][width]} coordinates per element.`);\n";
    (* fast: a table is read as the device would read it, unchecked *)
    if not fast then
      add
        "    if (!integer) for (const x of data) if (!Number.isFinite(x)) fail('E_NONFINITE', \
         'array input produced a nonfinite value.');\n";
    if fast then begin
      (* an integer table is read as numbers: converted once a call, not per gather *)
      add "    tables.set(slot, data); return data; };\n";
      add
        "  const row = (array, index, width) => { if (index < 0 || index >= array.length / width) \
         fail('E_ARRAY_RANGE', 'Array index is outside its length.'); return index; };\n"
    end
    else begin
      add "    tables.set(slot, data); return data; };\n";
      add
        "  const row = (array, index, width) => { if (index < 0n || index > %sn) \
         fail('E_ARRAY_RANGE', 'array/nth needs a non-negative whole length.');\n"
        (string_of_int Sys.max_array_length);
      add
        "    if (index >= BigInt(array.length / width)) fail('E_ARRAY_RANGE', 'Array index is \
         outside its length.'); return Number(index); };\n"
    end
  end;
  if textual then
    add
      "  const text = value => typeof value === 'string' ? value : fail('E_ARRAY_TYPE', 'Kernel \
       input changed its packed element type.');\n";
  if
    Array.exists
      (fun instruction ->
        match unguarded instruction with P.Fma _ | Checked_fma _ -> true | _ -> false)
      view.code
  then add "%s" fused;
  if
    Array.exists
      (fun instruction ->
        match unguarded instruction with P.Checked_mul _ | Checked_fma _ -> true | _ -> false)
      view.code
  then add "  const checked = (valid, value) => Number.isFinite(value) ? (valid ? 1 : value) : 0;\n";
  if
    Array.exists
      (fun instruction -> match unguarded instruction with P.Noise3 _ -> true | _ -> false)
      view.code
  then
    add
      "  const noise = (...args) => { try { return noise3(...args); } catch (e) { if (e?.code === \
       'E_NONFINITE') return NaN; throw e; } };\n";
  if
    Array.exists
      (fun instruction -> match unguarded instruction with P.Rand_at _ -> true | _ -> false)
      view.code
  then
    add
      "  const randAt = (seed, index) => { let x = wrap(seed ^ wrap(wrap(index + \
       0x11b54a32d192ed03n) * 0x1e3779b97f4a7c15n)); x = wrap(x ^ (BigInt.asUintN(63, x) >> 30n)); \
       x = wrap(x * 0x3f58476d1ce4e5b9n); x = wrap(x ^ (BigInt.asUintN(63, x) >> 27n)); x = wrap(x \
       * 0x14d049bb133111ebn); x = wrap(x ^ (BigInt.asUintN(63, x) >> 31n)); return \
       Number(BigInt.asUintN(63, x) >> 10n) / 9007199254740992; };\n";
  if fast then
    add
      "  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? Math.trunc(x) : \
       fail('E_NONFINITE', name + ' produced a nonfinite value.');\n"
  else begin
    add
      "  const convert = (name, x) => Number.isFinite(x) && Math.abs(x) < 4e18 ? \
       BigInt(Math.trunc(x)) : fail('E_NONFINITE', name + ' produced a nonfinite value.');\n";
    add
      "  const divide = (a, b, mod) => b === 0n ? fail('E_RANGE', 'Integer division by zero.') : \
       wrap(mod ? a %% b : a / b);\n";
    add
      "  const shift = (a, b, right) => b < 0n || b > 62n ? fail('E_RANGE', 'Integer shift count \
       must be within [0, 62].') : wrap(right ? BigInt.asUintN(63, a) >> b : a << b);\n"
  end;
  Array.iteri (fun k _ -> add "  const in%d = inputs[%d];\n" k k) view.widths;
  if product then begin
    Array.iteri (fun k w -> add "  const n%d = in%d.length / %d;\n" k k w) view.widths;
    add "  const s%d = 1;\n" (sources - 1);
    for k = sources - 2 downto 0 do
      add "  const s%d = s%d * n%d;\n" k (k + 1) (k + 1)
    done
  end;
  for k = 0 to outputs - 1 do
    add "  const out%d = outputs[%d];\n" k k
  done;
  if accumulating then add "  const st = outputs[%d];\n" state;
  if skip <> [] then begin
    add "  const skip = [%s];\n  let s = 0;\n" (String.concat ", " (List.map string_of_int skip));
    add "  for (let hi = skip.length; s < hi;) {\n    const mid = (s + hi) >> 1;\n";
    add "    if (skip[mid] < start) s = mid + 1; else hi = mid;\n  }\n"
  end;
  add "  for (let i = start; i < stop; i++) {\n";
  if skip <> [] then
    add "    if (s < skip.length && skip[s] === i) { s++; continue; }\n    const o = i - s;\n"
  else add "    const o = i;\n";
  if product then
    Array.iteri (fun k _ -> add "    const j%d = Math.floor(i / s%d) %% n%d;\n" k k k) view.widths
  else Array.iteri (fun k _ -> add "    const j%d = i;\n" k) view.widths;
  let numeric r =
    if R.integer_register view.code.(r) then
      if fast then Printf.sprintf "(r%dh * 4294967296 + r%dl)" r r else "Number(" ^ register r ^ ")"
    else register r
  in
  let index r = if fast && R.integer_register view.code.(r) then numeric r else register r in
  let rec expression slot instruction =
    match instruction with
    | P.Const value -> number value
    | Loop_result (loop, component, _) -> register (loop + 1 + component)
    | Loop_begin _ | Loop_carry _ | Loop_end _ -> assert false
    | Input (input, w, component) ->
        Printf.sprintf "Number(in%d[j%d * %d + %d])" input input w component
    | Uniform (index, component) -> Printf.sprintf "Number(uniforms[%d][%d])" index component
    | Frame _ -> Printf.sprintf "uniforms[%d][%d]" (Array.length view.uniform_widths) slot
    | Accumulator component -> Printf.sprintf "st[%d]" component
    | Binary (op, a, b) -> binary op (numeric a) (numeric b)
    | Unary (op, a) -> unary op (numeric a)
    | Select (c, yes, no) ->
        Printf.sprintf "%s !== 0 ? %s : %s" (numeric c) (numeric yes) (numeric no)
    | Noise3 (x, y, z, seed, octaves, lacunarity, gain) ->
        let fractal = function P.Fixed v -> number v | Live r -> numeric r in
        Printf.sprintf "noise(Float64Array.of(%s, %s, %s), %dn, %dn, %s, %s)" (numeric x)
          (numeric y) (numeric z) seed octaves (fractal lacunarity) (fractal gain)
    | Fma (a, b, c) -> Printf.sprintf "fused(%s, %s, %s)" (numeric a) (numeric b) (numeric c)
    | Checked_mul (p, a, b) ->
        Printf.sprintf "checked(%b, %s * %s)" (p = P.Valid) (numeric a) (numeric b)
    | Checked_fma (p, a, b, c) ->
        Printf.sprintf "checked(%b, fused(%s, %s, %s))" (p = P.Valid) (numeric a) (numeric b)
          (numeric c)
    | Array_prepare (i, ty) ->
        if ty = Flow.Ty.Array Flow.Ty.Text then Printf.sprintf "table(%d, 'text', 1)" i
        else
          Printf.sprintf "table(%d, %b, %d)" i (ty = Flow.Ty.Array Flow.Ty.Int)
            (Option.get (Flow.Packed_ops.array_width ty))
    | Gather (array, i, width, component) ->
        Printf.sprintf "%s[row(%s, %s, %d) * %d + %d]" (register array) (register array) (index i)
          width width component
    | Int_gather (array, i) ->
        Printf.sprintf "%s[row(%s, %s, 1)]" (register array) (register array) (index i)
    | Trace _ ->
        raise
          (Diagnostic.Error
             (Diagnostic.error ~code:"E_WEB_SOURCE"
                "scene/trace needs a native ray-query host; it is outside the 2D web target."))
    | Stage _ ->
        raise
          (Diagnostic.Error
             (Diagnostic.error ~code:"E_WEB_SOURCE"
                "A raster-stage intrinsic runs only in a native fragment stage."))
    | Text_const value -> Text_literal.quote value
    | Text_input input -> Printf.sprintf "text(in%d[j%d])" input input
    | Text_uniform input -> Printf.sprintf "text(uniforms[%d])" input
    | Text_gather (array, i) ->
        Printf.sprintf "%s[row(%s, %s, 1)]" (register array) (register array) (index i)
    | Text_select (condition, yes, no) ->
        Printf.sprintf "(%s !== 0 ? %s : %s)" (numeric condition) (register yes) (register no)
    | Rand_at (seed, index) -> Printf.sprintf "randAt(%s, %s)" (register seed) (register index)
    | Int_compare (op, a, b) when fast ->
        let h r = Printf.sprintf "r%dh" r and l r = Printf.sprintf "r%dl" r in
        let lt = fast_less in
        let eq = Printf.sprintf "(%s === %s && %s === %s)" (h a) (h b) (l a) (l b) in
        flag
          (match op with
          | Ilt -> lt a b
          | Igt -> lt b a
          | Ile -> "!" ^ lt b a
          | Ige -> "!" ^ lt a b
          | Ieq -> eq)
    | Int_const n -> string_of_int n ^ "n"
    | Int_input i -> Printf.sprintf "in%d[j%d]" i i
    | Int_uniform i -> Printf.sprintf "uniforms[%d][0]" i
    | Int_accumulator -> "st[0]"
    | Int_frame _ -> Printf.sprintf "uniforms[%d][%d]" (Array.length view.uniform_widths) slot
    | Int_binary (op, a, b) -> (
        let a = register a and b = register b in
        match op with
        | Iadd -> "wrap(" ^ a ^ " + " ^ b ^ ")"
        | Isub -> "wrap(" ^ a ^ " - " ^ b ^ ")"
        | Imul -> "wrap(" ^ a ^ " * " ^ b ^ ")"
        | Idiv -> "divide(" ^ a ^ ", " ^ b ^ ", false)"
        | Imod -> "divide(" ^ a ^ ", " ^ b ^ ", true)"
        | Iand -> "wrap(" ^ a ^ " & " ^ b ^ ")"
        | Ior -> "wrap(" ^ a ^ " | " ^ b ^ ")"
        | Ixor -> "wrap(" ^ a ^ " ^ " ^ b ^ ")"
        | Ishl -> "shift(" ^ a ^ ", " ^ b ^ ", false)"
        | Ishr -> "shift(" ^ a ^ ", " ^ b ^ ", true)")
    | Int_compare (op, a, b) ->
        flag
          (register a
          ^ (match op with
            | Ilt -> " < "
            | Ile -> " <= "
            | Igt -> " > "
            | Ige -> " >= "
            | Ieq -> " === ")
          ^ register b)
    | Int_abs a -> Printf.sprintf "%s < 0n ? wrap(-%s) : %s" (register a) (register a) (register a)
    | Int_select (c, a, b) ->
        Printf.sprintf "%s !== 0 ? %s : %s" (numeric c) (register a) (register b)
    | Convert (To_float, a) -> numeric a
    | Convert (op, a) ->
        let name, x =
          match op with
          | Truncate -> ("int", "Math.trunc(" ^ numeric a ^ ")")
          | Floor -> ("floor", "Math.floor(" ^ numeric a ^ ")")
          | Ceil -> ("ceil", "Math.ceil(" ^ numeric a ^ ")")
          | Round ->
              ( "round",
                "Math.trunc(" ^ numeric a ^ ") + (Math.abs(" ^ numeric a ^ " - Math.trunc("
                ^ numeric a ^ ")) >= 0.5 ? Math.sign(" ^ numeric a ^ ") : 0)" )
          | Coerce_int -> ("int", "Math.floor(" ^ numeric a ^ " + 0.5)")
          | Integer_result n -> (n, "Math.floor(" ^ numeric a ^ " + 0.5)")
          | To_float -> assert false
        in
        Printf.sprintf "convert(%S, %s)" name x
    | Raise (_, code, message, cause) ->
        Printf.sprintf "fail(%s, %s%s)" (Text_literal.quote code) (Text_literal.quote message)
          (Option.fold ~none:"" ~some:(fun cause -> ", " ^ Text_literal.quote cause) cause)
    | Guarded (_, body) -> expression slot body
  in
  (* a fast integer register's statement: both halves of [r<slot>] *)
  let pair slot body =
    let h r = Printf.sprintf "r%dh" r and l r = Printf.sprintf "r%dl" r in
    let lo e = Printf.sprintf "r%dl = %s; r%dh = RH;" slot e slot in
    let read v = lo (Printf.sprintf "(typeof (%s) === 'bigint' ? unbig(%s) : split(%s))" v v v) in
    match body with
    | P.Int_const n ->
        let n = Int64.of_int n in
        Printf.sprintf "r%dh = %ld; r%dl = %Ld;" slot
          (Int64.to_int32 (Int64.shift_right n 32))
          slot (Int64.logand n 0xffffffffL)
    | Int_input i -> read (Printf.sprintf "in%d[j%d]" i i)
    | Int_uniform i -> read (Printf.sprintf "uniforms[%d][0]" i)
    | Int_frame _ ->
        read (Printf.sprintf "uniforms[%d][%d]" (Array.length view.uniform_widths) slot)
    | Int_gather _ -> read (expression slot body)
    | Int_binary (op, a, b) ->
        let call f = lo (Printf.sprintf "%s(%s, %s, %s, %s)" f (h a) (l a) (h b) (l b)) in
        let bits o =
          Printf.sprintf "r%dh = %s %s %s; r%dl = (%s %s %s) >>> 0;" slot (h a) o (h b) slot (l a) o
            (l b)
        in
        (match op with
          | Iadd -> call "add64"
          | Isub -> call "sub64"
          | Imul -> call "mul64"
          | Idiv -> lo (Printf.sprintf "div64(%s, %s, %s, %s, false)" (h a) (l a) (h b) (l b))
          | Imod -> lo (Printf.sprintf "div64(%s, %s, %s, %s, true)" (h a) (l a) (h b) (l b))
          | Iand -> bits "&"
          | Ior -> bits "|"
          | Ixor -> bits "^"
          | Ishl -> lo (Printf.sprintf "shift64(%s, %s, %s, false)" (h a) (l a) (numeric b))
          | Ishr -> lo (Printf.sprintf "shift64(%s, %s, %s, true)" (h a) (l a) (numeric b)))
        ^ Printf.sprintf " r%dh = (r%dh << 1) >> 1;" slot slot
    | Int_abs a ->
        Printf.sprintf
          "if (%s < 0) { r%dl = sub64(0, 0, %s, %s); r%dh = RH; } else { r%dh = %s; r%dl = %s; } \
           r%dh = (r%dh << 1) >> 1;"
          (h a) slot (h a) (l a) slot slot (h a) slot (l a) slot slot
    | Int_select (c, a, b) ->
        Printf.sprintf "if (%s !== 0) { r%dh = %s; r%dl = %s; } else { r%dh = %s; r%dl = %s; }"
          (numeric c) slot (h a) slot (l a) slot (h b) slot (l b)
    | Loop_result (loop, component, _) ->
        let source = loop + 1 + component in
        Printf.sprintf "r%dh = %s; r%dl = %s;" slot (h source) slot (l source)
    | Convert (_, _) -> lo (Printf.sprintf "split(%s)" (expression slot body))
    | _ -> invalid_arg "fast integer register"
  in
  let emit slot instruction =
    let guards, body = match instruction with P.Guarded (g, b) -> (g, b) | b -> ([], b) in
    let guard =
      String.concat " && "
        (List.map (fun (r, yes) -> numeric r ^ if yes then " !== 0" else " === 0") guards)
    in
    if fast && R.integer_register body then begin
      add "    let r%dh = 0, r%dl = 0;\n" slot slot;
      if guards <> [] then add "    if (%s) {\n" guard;
      add "    %s\n" (pair slot body);
      if guards <> [] then add "    }\n"
    end
    else begin
      add "    let r%d;\n" slot;
      if guards <> [] then add "    if (%s) {\n" guard;
      (match body with
      | Checked_mul (_, a, b) ->
          add "    if (!(Number.isFinite(%s) && Number.isFinite(%s))) return 1;\n" (numeric a)
            (numeric b)
      | Checked_fma (_, a, b, c) ->
          add
            "    if (!(Number.isFinite(%s) && Number.isFinite(%s) && Number.isFinite(%s))) return 1;\n"
            (numeric a) (numeric b) (numeric c)
      | _ -> ());
      add "    r%d = %s;\n" slot (expression slot body);
      if (not (R.integer_register body)) && checks.(slot) then
        add "    if (!Number.isFinite(r%d)) %s\n" slot nonfinite;
      if guards <> [] then add "    }\n"
    end
  in
  let declare slot =
    if fast && R.integer_register view.code.(slot) then
      add "    let r%dh = 0, r%dl = 0;\n" slot slot
    else add "    let r%d;\n" slot
  in
  let assign target source =
    if fast && R.integer_register view.code.(target) then
      add "    r%dh = r%dh; r%dl = r%dl;\n" target source target source
    else add "    r%d = r%d;\n" target source
  in
  let exact slot = if fast then Printf.sprintf "big(r%dh, r%dl)" slot slot else register slot in
  let rec emit_range first last =
    let slot = ref first in
    while !slot < last do
      let instruction = view.code.(!slot) in
      match unguarded instruction with
      | P.Loop_begin loop ->
          let header = !slot and count = Array.length loop.initial in
          declare header;
          Array.iteri (fun component _ -> declare (header + 1 + component)) loop.initial;
          let guards = match instruction with P.Guarded (guards, _) -> guards | _ -> [] in
          if guards <> [] then
            add "    if (%s) {\n"
              (String.concat " && "
                 (List.map (fun (r, yes) -> numeric r ^ if yes then " !== 0" else " === 0") guards));
          Option.iter
            (fun limit ->
              add "    const length%d = %s - %s;\n" header (exact loop.last) (exact loop.first);
              add
                "    if (length%d > %dn) fail('E_ITER_BOUND', `range ${%s}‥${%s} exceeds %s \
                 iterations.`);\n"
                header limit (exact loop.first) (exact loop.last)
                (if limit = 4096 then "4,096" else string_of_int limit))
            loop.limit;
          assign header loop.first;
          Array.iteri
            (fun component initial -> assign (header + 1 + component) initial)
            loop.initial;
          add "    for (; %s;) {\n"
            (if fast then fast_less header loop.last
             else Printf.sprintf "r%d < r%d" header loop.last);
          emit_range (header + 1 + count) loop.finish;
          let values, stop =
            match unguarded view.code.(loop.finish) with
            | P.Loop_end (_, values, stop) -> (values, stop)
            | _ -> assert false
          in
          Option.iter
            (fun source -> add "    const stop%d = %s !== 0;\n" header (numeric source))
            stop;
          Array.iteri
            (fun component source ->
              if fast && R.integer_register view.code.(source) then
                add "    const next%dh%d = r%dh, next%dl%d = r%dl;\n" header component source header
                  component source
              else add "    const next%d_%d = r%d;\n" header component source)
            values;
          Array.iteri
            (fun component _ ->
              let target = header + 1 + component in
              if fast && R.integer_register view.code.(target) then
                add "    r%dh = next%dh%d; r%dl = next%dl%d;\n" target header component target
                  header component
              else add "    r%d = next%d_%d;\n" target header component)
            values;
          Option.iter (fun _ -> add "    if (stop%d) break;\n" header) stop;
          if fast then
            add "    r%dl = add64(r%dh, r%dl, 0, 1); r%dh = RH;\n" header header header header
          else add "    r%d++;\n" header;
          add "    }\n";
          if guards <> [] then add "    }\n";
          slot := loop.finish + 1
      | _ ->
          emit !slot instruction;
          incr slot
    done
  in
  emit_range 0 (Array.length view.code);
  Array.iteri
    (fun k registers ->
      Array.iteri
        (fun component slot ->
          if not (R.integer_register view.code.(slot) || R.text_register view.code.(slot)) then
            add "    if (!Number.isFinite(r%d)) %s\n" slot nonfinite;
          (match region.result with
          | R.Collect | R.Accumulate _ -> ()
          | R.Sum true when R.integer_register view.code.(slot) ->
              add "    st[%d] = st[%d] !== 0 ? wrap(st[%d] + r%d) : r%d;\n" component width
                component slot slot
          | R.Sum false when R.integer_register view.code.(slot) ->
              add
                "    st[%d] = st[%d] !== 0 ? convert('+', Math.floor(Number(st[%d]) + Number(r%d) \
                 + 0.5)) : r%d;\n"
                component width component slot slot
          | R.Sum _ ->
              add
                "    st[%d] = st[%d] !== 0 ? st[%d] + r%d : r%d;\n\
                \    if (!Number.isFinite(st[%d])) return 1;\n"
                component width component slot slot component);
          if collecting then
            if R.text_register view.code.(slot) then add "    out%d[o - outputBase] = r%d;\n" k slot
            else
              add "    out%d[o * %d + %d] = %s;\n" k (Array.length registers) component
                (if Array.length registers = 1 && R.integer_register view.code.(slot) then
                   if fast then Printf.sprintf "big(r%dh, r%dl)" slot slot else register slot
                 else numeric slot))
        registers)
    view.outputs;
  (match region.result with
  | R.Accumulate _ ->
      Array.iteri (fun component slot -> add "    st[%d] = r%d;\n" component slot) view.outputs.(0)
  | R.Sum _ -> add "    st[%d] = 1;\n" width
  | Collect -> ());
  add "  }\n  return 0;\n}\n";
  Buffer.contents line

(* ---- the frame level ---- *)
module C = Flow_ir.Core

let escape = Text_literal.quote

let rec literal ?store : C.literal -> string = function
  | Int n -> string_of_int n ^ "n"
  | Float x -> number x
  | Bool b -> string_of_bool b
  | Text s -> escape s
  | Vector xs -> "Float64Array.of(" ^ String.concat ", " (List.map number (Array.to_list xs)) ^ ")"
  | List xs -> "[" ^ String.concat ", " (List.map (literal ?store) (Array.to_list xs)) ^ "]"
  | Record fields ->
      "new Map(["
      ^ String.concat ", "
          (List.map (fun (n, v) -> "[" ^ escape n ^ ", " ^ literal ?store v ^ "]") fields)
      ^ "])"
  | Array { width; data } -> (
      match store with
      | Some store -> Printf.sprintf "new rt.PArray(%d, %s)" width (store data)
      | None ->
          Printf.sprintf "new rt.PArray(%d, Float64Array.of(%s))" width
            (String.concat ", " (List.map number (Array.to_list data))))
  | Int_array data ->
      "new rt.PArray(1, BigInt64Array.of("
      ^ String.concat ", " (Array.to_list (Array.map (fun n -> string_of_int n ^ "n") data))
      ^ "))"
  | Text_array data ->
      "new rt.TextArray([" ^ String.concat ", " (Array.to_list (Array.map escape data)) ^ "])"
  | Empty_geometry -> "null"

let rec ty_json : Flow.Ty.t -> string = function
  | Float -> {|"float"|}
  | Int -> {|"int"|}
  | Bool -> {|"bool"|}
  | Text -> {|"text"|}
  | Vec2 -> {|"vec2"|}
  | Vec3 -> {|"vec3"|}
  | Vec4 -> {|"vec4"|}
  | Any -> {|"any"|}
  | Array t -> "{array: " ^ ty_json t ^ "}"
  | List t -> "{list: " ^ ty_json t ^ "}"
  | Record fields ->
      "{record: ["
      ^ String.concat ", " (List.map (fun (n, t) -> "[" ^ escape n ^ ", " ^ ty_json t ^ "]") fields)
      ^ "]}"
  | _ -> {|"other"|}

(* The name prims.js exports a primitive under. *)
let export_name primitive =
  match C.primitive_name primitive with
  | "+" -> "add"
  | "-" -> "sub"
  | "*" -> "mul"
  | "/" -> "div"
  | "<" -> "lt"
  | "<=" -> "le"
  | ">" -> "gt"
  | ">=" -> "ge"
  | "=" -> "eq"
  | "equal?" -> "equal"
  | "text/blank?" -> "text_blank"
  | name -> String.map (fun c -> if c = '/' then '_' else c) name

(* Fixed-arity primitives: the argument names, in prims.js's parameter order. *)
let signature : C.primitive -> string list option = function
  | Numeric _ | Remainder | Integer _ | Integer_compare _ | Equal -> Some [ "a"; "b" ]
  | Math Not -> Some [ "a" ]
  | Math _ | Exp | Floor | Ceil | Round | Int_convert | Float_convert | Color_byte _ -> Some [ "x" ]
  | Show | Truthy | Coerce _ -> Some [ "value" ]
  | Text_blank | Text_trim | Text_lower | Text_repr -> Some [ "text" ]
  | Text_table_decode -> Some [ "text" ]
  | Text_table_encode -> Some [ "rows" ]
  | Text_tokens | Text_to_float | Text_to_int -> Some [ "text" ]
  | Text_split -> Some [ "text"; "separator" ]
  | Array_grade -> Some [ "keys"; "descending" ]
  | Array_index_of -> Some [ "keys"; "queries" ]
  | Array_compact -> Some [ "mask" ]
  | Array_scan_sum -> Some [ "array" ]
  | Array_scatter_add -> Some [ "indices"; "values"; "size" ]
  | Array_reduce _ -> Some [ "array" ]
  | Array_segment _ | Array_segment_argmax -> Some [ "values"; "offsets" ]
  | Next_after -> Some [ "x"; "y" ]
  | Sequence_length -> Some [ "list" ]
  | Sequence_get -> Some [ "list"; "index" ]
  | Sequence_build -> Some [ "count"; "width"; "type"; "cooked" ]
  | Sequence_slice -> Some [ "list"; "first"; "count" ]
  | Array_max_length -> Some [ "type" ]
  | Array_range -> Some [ "count"; "type" ]
  | Array_int | Array_text | Array_float | Array_vec3 | Array_vec4 -> Some [ "count"; "value" ]
  | Array_count -> Some [ "array" ]
  | Array_nth -> Some [ "array"; "index" ]
  | Array_slice -> Some [ "array"; "first"; "count" ]
  | Array_concat -> Some [ "first"; "second" ]
  | Noise3 -> Some [ "position"; "seed"; "octaves"; "lacunarity"; "gain" ]
  | Rand_at -> Some [ "seed"; "index" ]
  | Error -> Some [ "code"; "message"; "cause" ]
  | Fma | Checked_fma -> Some [ "a"; "b"; "c" ]
  | Checked_mul -> Some [ "a"; "b" ]
  | Fail _ | Fail_diagnostic _ -> Some []
  | Text_concat | Sequence_concat | Rand | Color_parse -> None

(* Helpers the module carries, emitted only when the body calls them (name, text). *)
let helpers =
  [
    ( "floatPoint",
      {|const floatPoint = (rt, v) => { const c = comps(rt, v);
  if (![c[0], c[1]].every(x => Number.isFinite(x) && Math.abs(x) <= 3.4028234663852886e38))
    rt.fail("E_DRAW_RANGE", "A drawing coordinate must be finite and fit a 32-bit float.");
  return [c[0], c[1]];
};
|}
    );
    ( "field",
      {|const field = (rt, v, name) => v instanceof Map
  ? (v.has(name) ? v.get(name) : rt.fail("E_FIELD", "Missing core field."))
  : v instanceof Float64Array && "xyzw".indexOf(name) >= 0 && "xyzw".indexOf(name) < v.length
    ? v["xyzw".indexOf(name)] : rt.fail("E_FIELD", "Invalid core field.");
|}
    );
    ( "store",
      {|const store = (rt, s, index, value) => {
  const i = Number(rt.coerce("int", index));
  if (s instanceof rt.TextArray) {
    if (i < 0 || i >= s.data.length) rt.fail("E_LIST_RANGE", "Core store is outside its sequence.");
    if (typeof value !== 'string') rt.fail("E_ARRAY_TYPE", "Text arrays contain text values.");
    s.data[i] = value;
  } else if (s instanceof rt.PArray) {
    const n = s.data.length / s.width;
    if (i < 0 || i >= n) rt.fail("E_LIST_RANGE", "Core store is outside its sequence.");
    if (s.data instanceof BigInt64Array) {
      if (typeof value !== 'number' && typeof value !== 'bigint' && typeof value !== 'boolean')
        rt.fail("E_ARRAY_TYPE", "Packed integer arrays contain integer values.");
      s.data[i] = rt.coerce("int", value);
      return;
    }
    const comps = value instanceof Float64Array ? value : Array(s.width).fill(rt.float(value));
    if (comps.length !== s.width) rt.fail("E_ARRAY_TYPE", "Packed array element has the wrong vector width.");
    for (let j = 0; j < s.width; j++)
      s.data[i * s.width + j] = Number.isFinite(comps[j]) ? comps[j] : rt.fail("E_NONFINITE", "array produced a nonfinite value.");
  } else {
    if (i < 0 || i >= s.length) rt.fail("E_LIST_RANGE", "Core store is outside its sequence.");
    s[i] = value;
  }
};
|}
    );
    ( "cell",
      {|const cell = (state, slot, key, stamp) => {
  const cells = state[slot] ??= new Map();
  let c = cells.get(key);
  if (c === undefined) cells.set(key, c = { value: undefined, pending: undefined, stamp });
  else if (c.stamp !== stamp) {
    if (c.pending !== undefined) c.value = c.pending;
    c.pending = undefined; c.stamp = stamp;
  }
  return c;
};
|}
    );
    ( "num",
      {|const num = (rt, v) => typeof v === "number" ? v : typeof v === "bigint" ? Number(v)
  : typeof v === "boolean" ? +v : rt.fail("E_TYPE", "Expected a number.");
const comps = (rt, v) => v instanceof Float64Array && v.length === 3 ? v : (n => [n, n, n])(num(rt, v));
const toInt = (rt, x) => Number.isFinite(x) && x < 2 ** 62 && x >= -(2 ** 62) ? Math.trunc(x)
  : rt.fail("E_DRAW_RANGE", "A drawing coordinate must fit an integer.");
const point = (rt, v) => { const c = comps(rt, v); return [toInt(rt, c[0]), toInt(rt, c[1])]; };
const pair = (rt, v) => { const c = comps(rt, v); return [c[0], c[1]]; };
const rgba = (rt, v) => {
  const c = rt.color_parse(v);
  return (Number(c.get("r")) << 24 | Number(c.get("g")) << 16 | Number(c.get("b")) << 8 | Number(c.get("a"))) | 0;
};
const packed3 = (rt, v) => v instanceof rt.PArray && v.width === 3;
const ink = (rt, v) => packed3(rt, v) ? -1 : rgba(rt, v);
const emit = (rt, entry) => {
  if (rt.marks.length >= 1048576) rt.fail("E_DRAW_RANGE", "A drawing has too many entries.");
  rt.marks.push(entry);
};
const emitAll = (rt, entries) => { for (const e of entries) emit(rt, e); };
const buffer = (data, count, width) => ({ buffer: data, count, width });
const items = (rt, v) => v instanceof rt.PArray ? Array.from({ length: Number(rt.array_count(v)) }, (_, i) => rt.array_nth(v, i))
  : Array.isArray(v) ? v : rt.fail("E_TYPE", "Expected an array.");
const arg = (rt, a, k) => a[k] ?? rt.fail("E_DRAW_INPUT", "draw needs " + k + ".");
const colourAttr = (rt, count, v) => {
  if (!packed3(rt, v)) return { constant: rgba(rt, v) };
  if (v.data.length !== 3 * count) rt.fail("E_DRAW_LENGTH", "Colors and positions have different lengths.");
  return buffer(v.data, count, 3);
};
const checkColours = (rt, n, a) => {
  for (const k of ["fill", "stroke", "color"])
    if (packed3(rt, a[k]) && a[k].data.length !== 3 * n) rt.fail("E_DRAW_LENGTH", "Colors and positions have different lengths.");
};
const pointsAttr = (rt, v) => {
  if (v instanceof rt.PArray && (v.width === 2 || v.width === 3)) {
    const n = v.data.length / v.width;
    return [n, buffer(v.data, n, v.width)];
  }
  const xs = items(rt, v), data = new Float64Array(2 * xs.length);
  xs.forEach((x, i) => { const p = pair(rt, x); data[2 * i] = p[0]; data[2 * i + 1] = p[1]; });
  return [xs.length, buffer(data, xs.length, 2)];
};
const markSet = (rt, batch, kind, a) => {
  const style = n => [a.fill === undefined ? { constant: 0 } : colourAttr(rt, n, a.fill),
    a.stroke === undefined ? { constant: 0 } : colourAttr(rt, n, a.stroke)];
  const where = key => batch ? pointsAttr(rt, arg(rt, a, key)) : [1, { constant: pair(rt, arg(rt, a, key)) }];
  const one = v => ({ constant: pair(rt, v) });
  const width = { constant: 1 };
  if (kind === "circle") {
    const [n, position] = where(batch ? "positions" : "at");
    const r = arg(rt, a, "radius");
    let radius;
    if (!batch) radius = { constant: num(rt, r) };
    else if (r instanceof rt.PArray && r.width === 1) {
      if (r.data.length !== n) rt.fail("E_DRAW_LENGTH", "Radii and positions have different lengths.");
      radius = buffer(r.data, n, 1);
    } else if (typeof r === "bigint" || typeof r === "number") radius = { constant: num(rt, r) };
    else rt.fail("E_TYPE", "Radius is a number or packed float array.");
    checkColours(rt, n, a);
    const [fill, stroke] = style(n);
    emit(rt, { type: "mark_set", kind, count: n, position, radius, fill, stroke, stroke_width: width });
  } else if (kind === "rect") {
    const [n, position] = where(batch ? "positions" : "at");
    const s = arg(rt, a, batch ? "sizes" : "size");
    let size;
    if (!batch) size = one(s);
    else if (packed3(rt, s)) {
      if (s.data.length !== 3 * n) rt.fail("E_DRAW_LENGTH", "Sizes and positions have different lengths.");
      size = buffer(s.data, n, 3);
    } else if (s instanceof Float64Array && s.length === 3) size = one(s);
    else rt.fail("E_TYPE", "Sizes are a vec3 or packed vec3 array.");
    checkColours(rt, n, a);
    const [fill, stroke] = style(n);
    emit(rt, { type: "mark_set", kind, count: n, position, size, fill, stroke, stroke_width: width });
  } else {
    const [n, position] = where("from");
    let end;
    if (batch) {
      const [m, e] = pointsAttr(rt, arg(rt, a, "to"));
      if (m !== n) rt.fail("E_DRAW_LENGTH", "Line endpoints have different lengths.");
      end = e;
    } else end = one(arg(rt, a, "to"));
    checkColours(rt, n, a);
    emit(rt, { type: "mark_set", kind, count: n, position, end, stroke: colourAttr(rt, n, arg(rt, a, "color")),
      stroke_width: { constant: num(rt, arg(rt, a, "width")) } });
  }
};
const optColour = (rt, v) => v === undefined ? null : rgba(rt, v);
const pathOf = (rt, kind, a) => {
  if (kind === "polygon") emitAll(rt, rt.polygon(items(rt, arg(rt, a, "points")).map(p => floatPoint(rt, p)), optColour(rt, a.fill), optColour(rt, a.stroke)));
  else if (kind === "polyline") emitAll(rt, rt.polyline(items(rt, arg(rt, a, "points")).map(p => floatPoint(rt, p)), ink(rt, arg(rt, a, "color"))));
  else {
    const [w, h] = floatPoint(rt, arg(rt, a, "size")), [x, y] = floatPoint(rt, arg(rt, a, "at"));
    const radius = Math.max(0, Math.min(num(rt, arg(rt, a, "radius")), Math.min(Math.abs(w), Math.abs(h)) / 2));
    emit(rt, { type: "push_transform", xx: 1, xy: 0, yx: 0, yy: 1, tx: x, ty: y });
    emitAll(rt, rt.rounded_rect(w, h, radius, optColour(rt, a.fill), optColour(rt, a.stroke)));
    emit(rt, { type: "pop_transform" });
  }
};
const transform = (rt, kind, v) => {
  if (kind === "translate") {
    const [x, y] = point(rt, v);
    emit(rt, { type: "push_transform", xx: 1, xy: 0, yx: 0, yy: 1, tx: x, ty: y });
  } else if (kind === "rotate") {
    const a = num(rt, v);
    if (!Number.isFinite(a)) rt.fail("E_NONFINITE", "angle produced a nonfinite value.");
    emit(rt, { type: "push_transform", xx: Math.cos(a), xy: Math.sin(a), yx: -Math.sin(a), yy: Math.cos(a), tx: 0, ty: 0 });
  } else {
    const [x, y] = v instanceof Float64Array && v.length === 3 ? [v[0], v[1]] : (n => [n, n])(num(rt, v));
    if (!Number.isFinite(x) || !Number.isFinite(y)) rt.fail("E_NONFINITE", "scale produced a nonfinite value.");
    emit(rt, { type: "push_transform", xx: x, xy: 0, yx: 0, yy: y, tx: 0, ty: 0 });
  }
};
|}
    );
  ]

type context = {
  program : C.program;
  body : Buffer.t;
  used : (string, unit) Hashtbl.t;
  mutable map : (int * Diagnostic.span) list;
  mutable async : bool;
  gpu : int -> bool;
  state_slot : int -> int;
  bindings : bool;
  templated : bool;
  plans : (int, string) Hashtbl.t;
  fast : bool;
  mutable remembered : bool;
  mutable resident : bool;
  kernel : int -> string option;
}

let local n = "l" ^ string_of_int n
let use ctx name = Hashtbl.replace ctx.used name ()

let call ctx primitive args =
  let refuse message =
    raise (Diagnostic.Error (Diagnostic.error ~code:"E_WEB_PRIMITIVE" message))
  in
  let values =
    match signature primitive with
    | None -> List.map (fun (_, n) -> local n) args
    | Some names ->
        List.iter
          (fun (name, _) ->
            if not (List.mem name names) then
              refuse
                (Printf.sprintf "Primitive %s has no argument %s." (C.primitive_name primitive) name))
          args;
        let rec last = function
          | [] -> []
          | n :: rest ->
              (match List.assoc_opt n args with Some v -> local v | None -> "undefined")
              :: last rest
        in
        let values = last names in
        let rec trim = function "undefined" :: rest -> trim rest | l -> l in
        List.rev (trim (List.rev values))
  in
  ignore ctx;
  let statics =
    match primitive with
    | C.Coerce ty -> [ ty_json ty ]
    | Color_byte C.Clamped -> [ {|"clamped"|} ]
    | Color_byte Unclamped -> [ {|"unclamped"|} ]
    | Fail (code, message) -> [ escape code; escape message ]
    | Fail_diagnostic d ->
        [ escape d.code; escape d.message; Option.fold ~none:"undefined" ~some:escape d.cause ]
        @
        if d.hints = [] && d.trace = [] then []
        else
          [
            "[" ^ String.concat "," (List.map escape d.hints) ^ "]";
            "["
            ^ String.concat ","
                (List.map
                   (fun (t : Diagnostic.trace) ->
                     Printf.sprintf "{node_id:%dn,label:%s,operation:%s}" t.node_id (escape t.label)
                       (escape t.operation))
                   d.trace)
            ^ "]";
          ]
    | _ -> []
  in
  Printf.sprintf "rt.%s(%s)" (export_name primitive) (String.concat ", " (statics @ values))

let expression ctx : C.expr -> string = function
  | Constant n -> Printf.sprintf "K[%d]" n
  | Local n -> local n
  | Call_index slot -> Printf.sprintf "BigInt(calls[%d]++)" slot
  | Source (Flow.Eval.Source ("$core/argument", _, [ ("index", Flow.Eval.Int index) ]))
    when ctx.bindings && index >= 0 ->
      Printf.sprintf "rt.coreArgument(%d)" index
  | Source _ | Host_source _ | Source_descriptor _ ->
      raise
        (Diagnostic.Error
           (Diagnostic.error ~code:"E_WEB_SOURCE"
              "This host-owned source is outside the 2D web target."))
  | Frame field -> (
      match field with
      | Time -> "input.t"
      | Dt -> "input.dt"
      | Index -> "BigInt(input.frame)"
      | Tick -> "BigInt(input.tick)"
      | Width -> "BigInt(input.size[0])"
      | Height -> "BigInt(input.size[1])"
      | Pointer_x -> "input.pointer[0]"
      | Pointer_y -> "input.pointer[1]"
      | Input -> "rt.frameValue(input)")
  | Prim (primitive, args) -> call ctx primitive args
  | Vec ns ->
      "Float64Array.of("
      ^ String.concat ", " (List.map (fun n -> "rt.float(" ^ local n ^ ")") ns)
      ^ ")"
  | Fields fields ->
      "new Map(["
      ^ String.concat ", "
          (List.map (fun (name, n) -> "[" ^ escape name ^ ", " ^ local n ^ "]") fields)
      ^ "])"
  | Items ns ->
      "rt.sequence_concat(" ^ String.concat ", " (List.map (fun n -> "[" ^ local n ^ "]") ns) ^ ")"
  | Field (n, name) ->
      use ctx "field";
      Printf.sprintf "field(rt, %s, %s)" (local n) (escape name)

let object_of args =
  "{" ^ String.concat ", " (List.map (fun (k, n) -> k ^ ": " ^ local n) args) ^ "}"

let kind_name = function C.Rect -> "rect" | C.Circle -> "circle" | C.Line -> "line"

let draw ctx : C.draw -> string =
 fun d ->
  use ctx "num";
  match d with
  | Background c ->
      Printf.sprintf
        "{ const size = rt.drawSize ?? input.size; emitAll(rt, rt.polygon([[0, 0], [size[0], 0], \
         [size[0], size[1]], [0, size[1]]], rgba(rt, %s), null)); }"
        (local c)
  | Point { at; ink } ->
      use ctx "floatPoint";
      Printf.sprintf
        "{ const [x, y] = floatPoint(rt, %s); emitAll(rt, rt.polygon([[x, y], [x + 1, y], [x + 1, \
         y + 1], [x, y + 1]], ink(rt, %s), null)); }"
        (local at) (local ink)
  | Mark { mark; args } ->
      Printf.sprintf "markSet(rt, false, %s, %s);" (escape (kind_name mark)) (object_of args)
  | Batch { mark; args } ->
      Printf.sprintf "markSet(rt, true, %s, %s);" (escape (kind_name mark)) (object_of args)
  | Points { positions; ink } ->
      Printf.sprintf
        "{ const [count, position] = pointsAttr(rt, %s); emit(rt, {type: \"mark_set\", kind: \
         \"point\", count, position, fill: ink(rt, %s)}); }"
        (local positions) (local ink)
  | Path { path; args } ->
      use ctx "floatPoint";
      Printf.sprintf "pathOf(rt, %s, %s);"
        (escape
           (match path with
           | C.Rounded_rect -> "rounded_rect"
           | Polygon -> "polygon"
           | Polyline -> "polyline"))
        (object_of args)
  | Text_run { at; text; ink; size } ->
      Printf.sprintf
        "if (typeof %s !== \"string\") rt.fail(\"E_TYPE\", \"Drawing text is text.\"); else if (%s \
         !== \"\") { const [x, y] = point(rt, %s); emit(rt, {type: \"text\", x, y, size: \
         Number(rt.coerce(\"int\", %s)), color: ink(rt, %s), text: %s}); }"
        (local text) (local text) (local at) (local size) (local ink) (local text)
  | Image { source = _, id; at; scale; angle } ->
      ctx.async <- true;
      Printf.sprintf
        "{ const resource = await rt.image(%d); const [x, y] = point(rt, %s), scale = num(rt, %s), \
         angle = num(rt, %s); if (!Number.isFinite(scale) || !Number.isFinite(angle) || scale <= \
         0) rt.fail(\"E_DRAW_RANGE\", \"Scene.image: invalid transform\"); emit(rt, {type: \
         \"image\", resource, x, y, scale, angle}); }"
        id (local at) (local scale) (local angle)
  | Push (t, n) ->
      Printf.sprintf "transform(rt, %s, %s);"
        (escape (match t with C.Translate -> "translate" | Rotate -> "rotate" | Scale -> "scale"))
        (local n)
  | Pop -> "emit(rt, {type: \"pop_transform\"});"

(* What each source and uniform of a region reads, by argument index ([Packed.Host.shape] of its
   template, whose arguments are [$core/argument] markers): a resident fold takes a count on the
   device from ["arg", i] and ["range", i, mul, div], and declines a placeholder read any other way. *)
let shapes_json packed =
  let arg = function
    | Flow.Eval.Source ("$core/argument", _, [ ("index", Flow.Eval.Int i) ]) -> Some i
    | _ -> None
  in
  let shape = function
    | P.Host.Value v -> (
        match arg v with Some i -> Printf.sprintf "['arg',%d]" i | None -> "['host']")
    | Range (v, mul, div) -> (
        match arg v with
        | Some i -> Printf.sprintf "['range',%d,%d,%d]" i mul div
        | None -> "['host']")
    | Opaque None -> "['opaque',null]"
    | Opaque (Some vs) ->
        Printf.sprintf "['opaque',[%s]]"
          (String.concat "," (List.filter_map (fun v -> Option.map string_of_int (arg v)) vs))
  in
  let list a = "[" ^ String.concat "," (Array.to_list (Array.map shape a)) ^ "]" in
  Printf.sprintf "{sources:%s,uniforms:%s}"
    (list (P.Host.source_shapes packed))
    (list (P.Host.uniform_shapes packed))

let rec statements ctx depth (list : C.statement list) = List.iter (statement ctx depth) list

and statement ctx depth (s : C.statement) =
  let pad = String.make (2 * depth) ' ' in
  let line fmt = Printf.ksprintf (fun text -> Buffer.add_string ctx.body (pad ^ text ^ "\n")) fmt in
  let block body = statements ctx (depth + 1) body in
  Option.iter (fun span -> ctx.map <- (s.id, span) :: ctx.map) s.source.span;
  let at = Printf.sprintf "at = %d; " s.id in
  let slot_key slot =
    let iter = ctx.program.state_slots.(slot).source.iter in
    let stamp =
      if ctx.program.state_slots.(slot).plane = `Host then "input.tick" else "input.frame"
    in
    let key =
      if iter = [] then {|""|}
      else
        "["
        ^ String.concat ", "
            (List.map
               (function
                 | C.Static_index i -> string_of_int i | Local_index l -> local l ^ ".toString()")
               iter)
        ^ "].join(\",\")"
    in
    use ctx "cell";
    Printf.sprintf "cell(state, %d, %s, %s)" (ctx.state_slot slot) key stamp
  in
  match s.node with
  | Assign (n, Items ns) when ctx.program.locals.(n) = Flow.Ty.List Flow.Ty.geometry ->
      line "%s%s = rt.sequence_cooked(%s);" at (local n)
        ("[" ^ String.concat "," (List.map local ns) ^ "]")
  (* a resident loop's array is on the device: its count is the one read back *)
  | Assign (n, Prim (Array_compact, [ (_, a) ])) when ctx.resident ->
      ctx.async <- true;
      line "%s%s = await rt.resident.prim('compact', null, %s);" at (local n) (local a)
  | Assign (n, Prim (Coerce (Flow.Ty.Array _ as ty), [ (_, a) ])) when ctx.resident ->
      ctx.async <- true;
      line "%s%s = await rt.resident.prim('coerce', %s, %s);" at (local n) (ty_json ty) (local a)
  | Assign (n, Prim (Array_count, [ (_, a) ])) when ctx.resident ->
      ctx.async <- true;
      line "%s%s = await rt.resident.settle(%s); %s = rt.array_count(%s);" at (local a) (local a)
        (local n) (local a)
  | Assign (n, e) -> line "%s%s = %s;" at (local n) (expression ctx e)
  | Reset_call slot -> line "%scalls[%d] = 0;" at slot
  | Draw d -> line "%s%s" at (draw ctx d)
  | Diagnose_rows { rows; policy; index; handler; diagnostic; result } ->
      if policy = Flow.Workspace.Range_last then
        raise
          (Diagnostic.Error
             (Diagnostic.error ?span:s.source.span ~code:"E_WEB_SOURCE"
                "Range-last diagnosis needs its native host."));
      line "%s{" at;
      line "  const row%d = rt.diagnosticFirst(%s);" s.id (local rows);
      line "  if (row%d >= 0) {" s.id;
      line "    %s = BigInt(row%d);" (local index) s.id;
      line "    try {";
      statements ctx (depth + 3) handler;
      line
        "    } catch (rowError%d) { rt.error_mapper_failure(new rt.Fail(\"E_DIAGNOSTIC_ROW\", \"A \
         diagnostic row failed.\"), rowError%d); }"
        s.id s.id;
      line "    rt.diagnosticRowsThrow(%s);" (local diagnostic);
      line "  }";
      line "  %s = null;" (local result);
      line "}"
  | Protect { body; value; failure; handler; mapped; result } ->
      line "%stry {" at;
      block body;
      line "  %s = %s;" (local result) (local value);
      line "} catch (error%d) {" s.id;
      line
        "  if (error%d instanceof rt.Fail && error%d.statement === undefined) error%d.statement = \
         at;"
        s.id s.id s.id;
      line "  %s = rt.error_payload(error%d);" (local failure) s.id;
      line "  try {";
      statements ctx (depth + 2) handler;
      line "  } catch (mapper%d) {" s.id;
      line
        "    if (mapper%d instanceof rt.Fail && mapper%d.statement === undefined) \
         mapper%d.statement = at;"
        s.id s.id s.id;
      line "    rt.error_mapper_failure(error%d, mapper%d);" s.id s.id;
      line "  }";
      line "  rt.error_rethrow(error%d, %s);" s.id (local mapped);
      line "}"
  | If { condition; yes; no } ->
      line "%sif (rt.truthy(%s)) {" at (local condition);
      block yes;
      if no = [] then line "}"
      else (
        line "} else {";
        block no;
        line "}")
  | Iterate { sequence; index; item; limit; body } ->
      line "%s{" at;
      let id = s.id in
      line "  const s%d = %s, c%d = Array.isArray(s%d) ? s%d.length : Number(rt.array_count(s%d));"
        id (local sequence) id id id id;
      line "  if (c%d > %d) rt.fail(\"E_ITER_BOUND\", \"Core iteration exceeds its bound.\");" id
        limit;
      line "  for (let i%d = 0; i%d < c%d; i%d++) {" id id id id;
      line "    %s = BigInt(i%d); %s = Array.isArray(s%d) ? s%d[i%d] : rt.array_nth(s%d, i%d);"
        (local index) id (local item) id id id id id;
      statements ctx (depth + 2) body;
      line "  }";
      line "}"
  | Counted { first; last; count; index; item; limit; stop; init; body } ->
      let id = s.id in
      line "%s{" at;
      line
        "  const f%d = rt.coerce(\"int\", %s), e%d = rt.coerce(\"int\", %s), n%d = e%d > f%d ? e%d \
         - f%d : 0n;"
        id (local first) id (local last) id id id id id;
      line "  if (n%d > %dn) rt.fail(\"E_ITER_BOUND\", \"Core iteration exceeds its bound.\");" id
        limit;
      line "  %s = n%d;" (local count) id;
      statements ctx (depth + 1) init;
      let exact_count = Int64.of_int limit > 9007199254740991L in
      line "  for (let i%d = %s, c%d = %s; i%d < c%d%s; i%d++) {" id
        (if exact_count then "0n" else "0")
        id
        (Printf.sprintf (if exact_count then "n%d" else "Number(n%d)") id)
        id id
        (Option.fold ~none:"" ~some:(fun n -> " && !rt.truthy(" ^ local n ^ ")") stop)
        id;
      line "    const j%d = %s; %s = j%d; %s = f%d + j%d;" id
        (Printf.sprintf (if exact_count then "i%d" else "BigInt(i%d)") id)
        (local index) id (local item) id id;
      statements ctx (depth + 2) body;
      line "  }";
      line "}"
  | Store { sequence; index; value } ->
      use ctx "store";
      line "%sstore(rt, %s, %s, %s);" at (local sequence) (local index) (local value)
  | State_read { slot; result; init; initial } ->
      let id = s.id in
      line "%s{ const c%d = %s;" at id (slot_key slot);
      line "  if (c%d.value !== undefined) %s = c%d.value; else {" id (local result) id;
      statements ctx (depth + 2) init;
      line "    %s = %s;" (local result) (local initial);
      line "  } }"
  | State_peek { slot; found; value } ->
      let id = s.id in
      line "%s{ const c%d = %s; %s = c%d.pending !== undefined; %s = %s ? c%d.pending : null; }" at
        id (slot_key slot) (local found) id (local value) (local found) id
  | State_write { slot; value } -> line "%s%s.pending = %s;" at (slot_key slot) (local value)
  | Invoke { region; args; result } ->
      ctx.async <- true;
      if ctx.gpu region && ctx.kernel region = None then
        line "%s%s = await rt.gpuArray(%d);" at (local result) region
      else begin
        let p = ctx.program.regions.(region) in
        let r = p in
        let dynamic =
          ctx.templated
          || Array.exists
               (fun (residual, term) ->
                 Flow.Eval.frame_dependent
                   (Flow.Eval.Residual (Flow.Eval.Residual.with_bindings residual ~term [])))
               r.sources
          || Array.exists Flow.Eval.frame_dependent r.uniforms
        in
        let plan = if dynamic then ", B" ^ string_of_int region else "" in
        if dynamic && not (Hashtbl.mem ctx.plans region) then
          Hashtbl.add ctx.plans region (binding_plan ~fast:ctx.fast ?kernel:(ctx.kernel region) p);
        (* a display region bound per call runs on the device (fast precision) *)
        if ctx.gpu region && dynamic then
          line "%s%s = await rt.gpuInvoke(%d, [%s]%s);" at (local result) region
            (String.concat ", " (List.map local args))
            plan
        else
          line "%s%s = await rt.invoke(%d, [%s]%s);" at (local result) region
            (String.concat ", " (List.map local args))
            plan
      end

and binding_plan ?(fast = false) ?kernel packed =
  let r = packed in
  let refuse message = raise (Diagnostic.Error (Diagnostic.error ~code:"E_WEB_SOURCE" message)) in
  if r.result <> R.Collect || Array.length r.outputs <> 1 then
    refuse "Dynamic region arguments require a single Collect output.";
  let rec supported = function
    | Flow.Ty.Int | Float | Bool | Text | Vec2 | Vec3 | Vec4 -> true
    | Array (Int | Float | Text | Vec2 | Vec3 | Vec4) | List Any -> true
    | List ty -> supported ty
    | Record fields -> List.for_all (fun (_, ty) -> supported ty) fields
    | _ -> false
  in
  if not (Array.for_all supported (P.Host.argument_types packed)) then
    refuse "This compiled region argument type is outside the 2D web target.";
  let needs_frame_value = ref false in
  let prepare ~uniform program =
    if Array.length program.C.state_slots <> 0 || program.call_slots <> 0 then
      refuse "Region preparation cannot own state or call-index slots.";
    (* A capture is read by synchronous selected instructions, including workers. *)
    let has predicate statements =
      let found = ref false in
      C.iter (fun statement -> if predicate statement then found := true) statements;
      !found
    in
    let asynchronous =
      has (fun (s : C.statement) -> match s.node with Invoke _ -> true | _ -> false)
    in
    let input =
      has (fun (s : C.statement) ->
          match s.node with Assign (_, Frame Input) -> true | _ -> false)
    in
    if uniform && input program.body then needs_frame_value := true;
    if uniform && asynchronous program.body then
      refuse "Asynchronous region captures require a compiled synchronous preparation path.";
    fst (frame_result ~fast ~bindings:true ~internal:true program)
  in
  let source (residual, term) =
    let view = Flow.Eval.Residual.residual_view residual in
    let residual = Flow.Eval.Residual.with_bindings residual ~term view.bindings in
    let program =
      match Flow_ir.Core_lower.lower ~strict:true residual with
      | Ok p -> p
      | Error form -> refuse ("Region input preparation: " ^ form)
    in
    "{type:" ^ ty_json term.Flow.Workspace.ty ^ ",source:"
    ^ escape (prepare ~uniform:false program)
    ^ "}"
  in
  let uniform value =
    let view = Flow.Eval.Residual.residual_view r.residual in
    let source : C.source =
      {
        site = view.site;
        owner = view.owner;
        instance = view.instance;
        prefix = view.prefix;
        iter = List.map (fun i -> C.Static_index i) view.iter;
        span = Some view.term.form.span;
      }
    in
    let result = ref (-1) in
    let program =
      match
        Flow_ir.Core_lower.lower_draw
          ~root:(lazy r.residual)
          (fun ctx -> result := ctx.value source value)
      with
      | Ok p -> { p with results = [| !result |] }
      | Error form -> refuse ("Region capture preparation: " ^ form)
    in
    let ty =
      match value with
      | Flow.Eval.Residual residual -> (Flow.Eval.Residual.residual_view residual).term.ty
      | v -> Flow.Value.ty_of v
    in
    "{type:" ^ ty_json ty ^ ",source:" ^ escape (prepare ~uniform:true program) ^ "}"
  in
  let rec facts slot = function
    | R.Frame name -> [ Printf.sprintf "[%d,%s,false]" slot (escape name) ]
    | Int_frame name -> [ Printf.sprintf "[%d,%s,true]" slot (escape name) ]
    | Guarded (_, body) -> facts slot body
    | _ -> []
  in
  let sources = Array.map source r.sources and captures = Array.map uniform r.uniforms in
  let terminal = r.outputs.(0) in
  let integer = Array.length terminal = 1 && R.integer_register r.code.(terminal.(0))
  and text = Array.length terminal = 1 && R.text_register r.code.(terminal.(0)) in
  Printf.sprintf
    "{source:%s,argumentTypes:[%s],sources:[%s],captures:[%s],product:%b,skip:[%s],frame:[%s],length:%d,width:%d,integer:%b,text:%b,frameValue:%b,arrayLimit:%sn,countLimit:%sn,kernel:%s,shapes:%s}"
    (escape (region ~fast packed))
    (String.concat "," (Array.to_list (Array.map ty_json (P.Host.argument_types packed))))
    (String.concat "," (Array.to_list sources))
    (String.concat "," (Array.to_list captures))
    (r.iteration = R.Product)
    (String.concat "," (Array.to_list (Array.map string_of_int r.skip)))
    (String.concat "," (List.concat (Array.to_list (Array.mapi facts r.code))))
    (Array.length r.code)
    (Array.length r.outputs.(0))
    integer text !needs_frame_value
    (string_of_int (if integer || text then Sys.max_array_length else Sys.max_floatarray_length))
    (string_of_int max_int)
    (match kernel with Some id -> escape id | None -> "null")
    (shapes_json packed)

and frame_result ?store ?(fast = false) ?(kernel = fun _ -> None) ?(gpu = fun _ -> false)
    ?(state_slot = Fun.id) ?(bindings = false) ?(internal = false) (program : C.program) =
  let body = Buffer.create 4096 in
  (* Once one region consumes computed locals, its producer Invokes need real
     source preparation too. Argument-free fixed exports keep data.bin calls. *)
  let templated =
    bindings || Array.exists (fun region -> P.Host.argument_types region <> [||]) program.regions
  in
  let ctx =
    {
      program;
      body;
      used = Hashtbl.create 8;
      map = [];
      async = false;
      gpu;
      state_slot;
      bindings;
      templated;
      plans = Hashtbl.create 4;
      fast;
      remembered = false;
      resident = false;
      kernel;
    }
  in
  (* A drawing's loop memory, as native [Core_exec.prepare ~remember_loops]: a top-level loop or
     region call whose reads repeat one of its last 4 keys hands back that run's writes. *)
  List.iter
    (fun (s : C.statement) ->
      match (s.node, Flow_ir.Core_exec.footprint program s) with
      | (Iterate _ | Counted _ | Invoke _), Some (reads, writes)
        when (not bindings) && (not internal) && writes <> [||] ->
          ctx.remembered <- true;
          let names a = String.concat ", " (List.map local (Array.to_list a)) in
          Printf.bprintf body "    { const k%d = [%s], h%d = rt.recall(M, %d, k%d);\n" s.id
            (names reads) s.id s.id s.id;
          Printf.bprintf body "    if (h%d) [%s] = h%d; else {\n" s.id (names writes) s.id;
          (* a resident fold (native Core_exec.resident_live): in fast precision its regions stay on
           the device and its live writes come back once, as it ends *)
          (match Flow_ir.Core_exec.resident_live program s with
          | Some live when fast ->
              ctx.async <- true;
              Printf.bprintf body "    rt.resident.begin();\n    try {\n";
              ctx.resident <- true;
              statement ctx 3 s;
              ctx.resident <- false;
              Printf.bprintf body
                "      [%s] = await rt.resident.finish([%s]);\n\
                \    } finally { rt.resident.end(); }\n"
                (names live) (names live)
          | _ -> statement ctx 3 s);
          Printf.bprintf body "    rt.keep(M, %d, k%d, [%s]); } }\n" s.id s.id (names writes)
      | _ -> statement ctx 2 s)
    program.body;
  let code = Buffer.contents body in
  let mentions name = Hashtbl.mem ctx.used name in
  let out = Buffer.create 8192 in
  Buffer.add_string out
    "// Rays web export: frame program. frame(input, state, rt); see lib/flow_web/js.mli.\n";
  if Array.length program.constants > 0 then Buffer.add_string out "let K;\n";
  if ctx.remembered then Buffer.add_string out "const M = new Map();\n";
  List.iter (fun (name, text) -> if mentions name then Buffer.add_string out text) helpers;
  Hashtbl.to_seq ctx.plans |> List.of_seq
  |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
  |> List.iter (fun (id, plan) -> Printf.bprintf out "const B%d = %s;\n" id plan);
  Printf.bprintf out "%s%sfunction frame(input, state, rt) {\n"
    (if internal then "" else "export ")
    (if ctx.async then "async " else "");
  if Array.length program.constants > 0 then
    Printf.bprintf out "  K ??= [%s];\n"
      (String.concat ", " (List.map (literal ?store) (Array.to_list program.constants)));
  let n = Array.length program.locals in
  if n > 0 then begin
    let names = List.init n local in
    let rec rows = function
      | [] -> []
      | l ->
          let row = List.take 16 l and rest = List.drop 16 l in
          ("  let " ^ String.concat ", " row ^ ";\n") :: rows rest
    in
    List.iter (Buffer.add_string out) (rows names)
  end;
  Buffer.add_string out "  let at = 0;\n";
  if program.call_slots > 0 then
    Printf.bprintf out "  const calls = new Int32Array(%d);\n" program.call_slots;
  Buffer.add_string out "  try {\n";
  Buffer.add_string out code;
  Buffer.add_string out
    "  } catch (e) {\n\
    \    if (e !== null && typeof e === \"object\" && e.statement === undefined) e.statement = at;\n\
    \    throw e;\n\
    \  }\n";
  (match Array.to_list program.results with
  | [ r ] -> Printf.bprintf out "  return %s;\n" (local r)
  | rs -> Printf.bprintf out "  return [%s];\n" (String.concat ", " (List.map local rs)));
  Buffer.add_string out "}\n";
  (Buffer.contents out, List.rev ctx.map)

let frame ?store ?fast ?kernel ?gpu ?state_slot program =
  match frame_result ?store ?fast ?kernel ?gpu ?state_slot program with
  | result -> Ok (fst result)
  | exception Diagnostic.Error diagnostic -> Error diagnostic

(* A region with arguments or frame-dependent inputs runs from a binding plan; this is whether one
   can be made for it, so a draw lowering may keep it a region on the web. *)
let bindable packed =
  match binding_plan ~fast:true packed with _ -> true | exception Diagnostic.Error _ -> false

let frame_map program =
  match frame_result program with
  | _, map ->
      String.concat ""
        (List.map
           (fun (id, (s : Diagnostic.span)) -> Printf.sprintf "%d %d %d\n" id s.start s.finish)
           map)
  | exception Diagnostic.Error _ -> ""
