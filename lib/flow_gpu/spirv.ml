module P = Flow_ir.Packed

type t = {
  bytes : bytes;
  entry : string;
  interface : Ogpu.Shader.binding list;
  layout : Flow_ir.Kernel_layout.t;
  uniform_bytes : int;
}

(* Opcodes and operands follow the Khronos unified SPIR-V grammar:
   https://registry.khronos.org/SPIR-V/ *)
module Words = struct
  type t = {
    mutable next : int;
    annotations : Buffer.t;
    declarations : Buffer.t;
    mutable body : Buffer.t;
    constants : (int * int32, int) Hashtbl.t;
    wide_constants : (int64, int) Hashtbl.t;
  }

  let create () =
    {
      next = 1;
      annotations = Buffer.create 256;
      declarations = Buffer.create 512;
      body = Buffer.create 2048;
      constants = Hashtbl.create 32;
      wide_constants = Hashtbl.create 16;
    }

  let id w =
    let result = w.next in
    w.next <- result + 1;
    result

  let instruction into opcode operands =
    Buffer.add_int32_le into (Int32.of_int (((List.length operands + 1) lsl 16) lor opcode));
    List.iter (fun word -> Buffer.add_int32_le into (Int32.of_int word)) operands

  let declare w opcode operands =
    let result = id w in
    instruction w.declarations opcode (result :: operands);
    result

  let value w ty opcode operands =
    let result = id w in
    instruction w.body opcode (ty :: result :: operands);
    result

  let constant w ty bits =
    match Hashtbl.find_opt w.constants (ty, bits) with
    | Some id -> id
    | None ->
        let result = id w in
        instruction w.declarations 43 [ ty; result; Int32.to_int bits ];
        Hashtbl.add w.constants (ty, bits) result;
        result

  let string_words text =
    let bytes = Bytes.make ((String.length text + 4) / 4 * 4) '\000' in
    Bytes.blit_string text 0 bytes 0 (String.length text);
    List.init
      (Bytes.length bytes / 4)
      (fun index -> Int32.to_int (Bytes.get_int32_le bytes (index * 4)))

  let wide_constant w ty bits =
    match Hashtbl.find_opt w.wide_constants bits with
    | Some id -> id
    | None ->
        let result = id w in
        instruction w.declarations 43
          [
            ty;
            result;
            Int32.to_int (Int64.to_int32 bits);
            Int32.to_int (Int64.to_int32 (Int64.shift_right_logical bits 32));
          ];
        Hashtbl.add w.wide_constants bits result;
        result
end

type emitter = {
  words : Words.t;
  boolean : int;
  uint : int;
  sint : int;
  float : int;
  long : int option;
  ulong : int option;
  glsl : int;
  uniform : int;
  uniform_pointer : int;
  storage_pointer : int;
  uint_storage_pointer : int;
  status : int;
  uint_function_pointer : int;
  mutable status_bits : int;  (* the thread's Function-storage status word *)
  table : int option;
  noise : (int * int) option;
  scene : (int * int * int * int * int option * int * int * int) option;  (* acceleration structure variable and type, vertices, spheres, ray query, vec3, vec2 *)
  mutable block : int;
  mutable trace_cache : (int list * int * int array) list;
  strands : int option;  (* the strand segments' storage variable *)
  mutable loop_depth : int;
}

let fail message = raise (Diagnostic.Error (Diagnostic.error ~code:"E_GPU_FORM" message))
let u e value = Words.constant e.words e.uint (Int32.of_int value)
let f e value = Words.constant e.words e.float (Int32.bits_of_float value)
let result e ty opcode operands = Words.value e.words ty opcode operands
let long e = Option.get e.long
let l e value = Words.wide_constant e.words (long e) value
let from_long e value = result e e.float 111 [ value ]

let floating e opcode operands =
  let value = result e e.float opcode operands in
  if List.mem opcode [ 129; 131; 133; 136; 140; 141 ] then
    Words.instruction e.words.annotations 71 [ value; 42 ];
  value

let compare e opcode a b = result e e.boolean opcode [ a; b ]
let select e ty condition yes no = result e ty 169 [ condition; yes; no ]
let truth e value = compare e 183 value (f e 0.)
let numeric_truth e value = select e e.float value (f e 1.) (f e 0.)
let ext e opcode operands = result e e.float 12 (e.glsl :: opcode :: operands)

let uniform e word =
  let pointer =
    result e e.uniform_pointer 65 [ e.uniform; u e 0; u e (word / 4); u e (word mod 4) ]
  in
  result e e.uint 61 [ pointer ]

let uniform_float e word = result e e.float 124 [ uniform e word ]

let load e variable index =
  let pointer = result e e.storage_pointer 65 [ variable; u e 0; index ] in
  result e e.float 61 [ pointer ]

let store e variable index value =
  let pointer = result e e.storage_pointer 65 [ variable; u e 0; index ] in
  Words.instruction e.words.body 62 [ pointer; value ]

let load_uint e variable index =
  let pointer = result e e.uint_storage_pointer 65 [ variable; u e 0; index ] in
  result e e.uint 61 [ pointer ]

let store_uint e variable index value =
  let pointer = result e e.uint_storage_pointer 65 [ variable; u e 0; index ] in
  Words.instruction e.words.body 62 [ pointer; value ]

let offset e index width component =
  result e e.uint 128 [ result e e.uint 132 [ index; u e width ]; u e component ]

let non_finite e value =
  result e e.boolean 166 [ result e e.boolean 156 [ value ]; result e e.boolean 157 [ value ] ]

let label e id =
  Words.instruction e.words.body 248 [ id ];
  e.block <- id

let jump e id = Words.instruction e.words.body 249 [ id ]

(* Status bits gather in the thread's word and reach the shared status with the kernel's one atomic,
   at its end (as the Metal and WGSL printers). *)
let record_status e condition bits =
  let old = result e e.uint 61 [ e.status_bits ] in
  let bit = result e e.uint 169 [ condition; u e bits; u e 0 ] in
  Words.instruction e.words.body 62 [ e.status_bits; result e e.uint 197 [ old; bit ] ]

let mark_invalid e invalid condition =
  if e.loop_depth > 0 then record_status e condition 1
  else invalid := result e e.boolean 166 [ !invalid; condition ]

let conditional e ty condition yes no =
  let yes_block = Words.id e.words and no_block = Words.id e.words and after = Words.id e.words in
  Words.instruction e.words.body 247 [ after; 0 ];
  Words.instruction e.words.body 250 [ condition; yes_block; no_block ];
  label e yes_block;
  let yes_value = yes () in
  let yes_end = e.block in
  jump e after;
  label e no_block;
  let no_value = no () in
  let no_end = e.block in
  jump e after;
  label e after;
  result e ty 245 [ yes_value; yes_end; no_value; no_end ]

let noise_sample e offset x y z =
  let add a b = floating e 129 [ a; b ]
  and sub a b = floating e 131 [ a; b ]
  and mul a b = floating e 133 [ a; b ] in
  let either a b = result e e.boolean 166 [ a; b ] in
  let invalid = either (non_finite e x) (either (non_finite e y) (non_finite e z)) in
  conditional e e.float invalid
    (fun () -> f e nan)
    (fun () ->
      let coordinate value =
        let base = ext e 8 [ value ] in
        let q = sub value base in
        let index = result e e.sint 110 [ floating e 140 [ base; f e 256. ] ] in
        let index = result e e.uint 199 [ result e e.uint 124 [ index ]; u e 255 ] in
        let fade = mul (mul (mul q q) q) (add (mul q (sub (mul q (f e 6.)) (f e 15.))) (f e 10.)) in
        (index, q, fade)
      in
      let ix, x, fx = coordinate x and iy, y, fy = coordinate y and iz, z, fz = coordinate z in
      let table index =
        let index = result e e.uint 128 [ offset; index ] in
        let pointer = result e e.uint_storage_pointer 65 [ Option.get e.table; u e 0; index ] in
        result e e.uint 61 [ pointer ]
      in
      let plus a b = result e e.uint 128 [ a; b ] in
      let next a = plus a (u e 1) in
      let a = plus (table ix) iy and b = plus (table (next ix)) iy in
      let aa = plus (table a) iz
      and ab = plus (table (next a)) iz
      and ba = plus (table b) iz
      and bb = plus (table (next b)) iz in
      let gradient hash x y z =
        let h = result e e.uint 199 [ hash; u e 15 ] in
        let first = select e e.float (compare e 176 h (u e 8)) x y in
        let middle = either (compare e 170 h (u e 12)) (compare e 170 h (u e 14)) in
        let second = select e e.float (compare e 176 h (u e 4)) y (select e e.float middle x z) in
        let sign mask value =
          select e e.float
            (compare e 170 (result e e.uint 199 [ h; u e mask ]) (u e 0))
            value (floating e 127 [ value ])
        in
        add (sign 1 first) (sign 2 second)
      in
      let blend a b weight = add a (mul weight (sub b a)) in
      let xm = sub x (f e 1.) and ym = sub y (f e 1.) and zm = sub z (f e 1.) in
      let row aa ba y z = blend (gradient (table aa) x y z) (gradient (table ba) xm y z) fx in
      let x00 = row aa ba y z
      and x10 = row ab bb ym z
      and x01 = row (next aa) (next ba) y zm
      and x11 = row (next ab) (next bb) ym zm in
      let value = mul (add (blend (blend x00 x10 fy) (blend x01 x11 fy) fz) (f e 1.)) (f e 0.5) in
      ext e 43 [ value; f e 0.; f e 1. ])

let noise_functions e =
  match e.noise with
  | None -> ()
  | Some (sample, fbm) ->
      let function_ id types body =
        let signature = Words.declare e.words 33 (e.float :: types) in
        Words.instruction e.words.body 54 [ e.float; id; 0; signature ];
        let parameters =
          List.map
            (fun ty ->
              let id = Words.id e.words in
              Words.instruction e.words.body 55 [ ty; id ];
              id)
            types
        in
        label e (Words.id e.words);
        let value = body parameters in
        Words.instruction e.words.body 254 [ value ];
        Words.instruction e.words.body 56 []
      in
      function_ sample [ e.float; e.float; e.float; e.uint ] (function
        | [ x; y; z; offset ] -> noise_sample e offset x y z
        | _ -> assert false);
      function_ fbm [ e.float; e.float; e.float; e.uint; e.uint ] (function
        | [ x; y; z; offset; octaves ] ->
            let before = e.block in
            let header = Words.id e.words
            and body = Words.id e.words
            and continuing = Words.id e.words
            and finish = Words.id e.words in
            let next = Array.init 5 (fun _ -> Words.id e.words) in
            jump e header;
            label e header;
            let phi ty initial next = result e ty 245 [ initial; before; next; continuing ] in
            let frequency = phi e.float (f e 1.) next.(0)
            and amplitude = phi e.float (f e 1.) next.(1)
            and sum = phi e.float (f e 0.) next.(2)
            and weights = phi e.float (f e 0.) next.(3)
            and iteration = phi e.uint (u e 0) next.(4) in
            let within = compare e 176 iteration octaves in
            Words.instruction e.words.body 246 [ finish; continuing; 0 ];
            Words.instruction e.words.body 250 [ within; body; finish ];
            label e body;
            let scaled value = floating e 133 [ value; frequency ] in
            let value = result e e.float 57 [ sample; scaled x; scaled y; scaled z; offset ] in
            let weighted = floating e 133 [ value; amplitude ] in
            jump e continuing;
            label e continuing;
            let define slot ty opcode operands =
              Words.instruction e.words.body opcode (ty :: next.(slot) :: operands);
              if ty = e.float then Words.instruction e.words.annotations 71 [ next.(slot); 42 ]
            in
            define 0 e.float 133 [ frequency; f e 2. ];
            define 1 e.float 133 [ amplitude; f e 0.5 ];
            define 2 e.float 129 [ sum; weighted ];
            define 3 e.float 129 [ weights; amplitude ];
            define 4 e.uint 128 [ iteration; u e 1 ];
            jump e header;
            label e finish;
            select e e.float
              (compare e 180 weights (f e 0.))
              (f e 0.)
              (floating e 136 [ sum; weights ])
        | _ -> assert false)

let binary e operation a b =
  let op opcode = floating e opcode [ a; b ] in
  let relation opcode = numeric_truth e (compare e opcode a b) in
  match operation with
  | P.Add -> op 129
  | Sub -> op 131
  | Mul -> op 133
  | Div -> select e e.float (compare e 180 b (f e 0.)) (f e 0.) (op 136)
  | Mod ->
      let remainder = floating e 140 [ floating e 129 [ op 140; b ]; b ] in
      select e e.float (compare e 180 b (f e 0.)) (f e 0.) remainder
  | Pow ->
      let zero =
        select e e.float
          (compare e 180 b (f e 0.))
          (f e 1.)
          (select e e.float (compare e 186 b (f e 0.)) (f e 0.) (f e infinity))
      in
      select e e.float (compare e 180 a (f e 0.)) zero (ext e 26 [ ext e 4 [ a ]; b ])
  | Min -> ext e 37 [ a; b ]
  | Max -> ext e 40 [ a; b ]
  | Lt -> relation 184
  | Le -> relation 188
  | Gt -> relation 186
  | Ge -> relation 190
  | Eq -> relation 180
  | And -> numeric_truth e (result e e.boolean 167 [ truth e a; truth e b ])
  | Or -> numeric_truth e (result e e.boolean 166 [ truth e a; truth e b ])
  | Atan2 -> ext e 25 [ a; b ]
  | Hypot -> fail "Native-safe hypot is not supported by the SPIR-V kernel target."

let unary e operation value =
  match operation with
  | P.Sin -> ext e 13 [ value ]
  | Cos -> ext e 14 [ value ]
  | Tan -> ext e 15 [ value ]
  | Abs -> ext e 4 [ value ]
  | Sqrt -> ext e 31 [ ext e 4 [ value ] ]
  | Not -> numeric_truth e (compare e 180 value (f e 0.))
  | Asin | Acos ->
      let clamped = ext e 43 [ value; f e (-1.); f e 1. ] in
      select e e.float
        (compare e 186 (ext e 4 [ value ]) (f e 1.))
        (f e nan)
        (ext e (if operation = P.Asin then 16 else 17) [ clamped ])
  | Exp -> ext e 27 [ value ]
  | Log ->
      let positive = compare e 186 value (f e 0.) in
      select e e.float positive (ext e 28 [ select e e.float positive value (f e 1.) ]) (f e nan)
  | Sign ->
      select e e.float
        (compare e 186 value (f e 0.))
        (f e 1.)
        (select e e.float (compare e 184 value (f e 0.)) (f e (-1.)) value)

let storage_type words scalar =
  let array = Words.declare words 29 [ scalar ] in
  let structure = Words.declare words 30 [ array ] in
  Words.instruction words.annotations 71 [ array; 6; 4 ];
  Words.instruction words.annotations 71 [ structure; 2 ];
  Words.instruction words.annotations 72 [ structure; 0; 35; 0 ];
  (Words.declare words 32 [ 12; structure ], Words.declare words 32 [ 12; scalar ])

let integer_binary e operation a b =
  let op code operands = result e (long e) code operands in
  match operation with
  | Flow.Packed_ops.Iadd -> op 128 [ a; b ]
  | Isub -> op 130 [ a; b ]
  | Imul -> op 132 [ a; b ]
  | Iand -> op 199 [ a; b ]
  | Ior -> op 197 [ a; b ]
  | Ixor -> op 198 [ a; b ]
  | Ishl | Ishr ->
      let a = if operation = Ishr then op 199 [a; l e Int64.max_int] else a in
      op (if operation = Ishl then 196 else 194) [ a; op 199 [ b; l e 63L ] ]
  | Idiv | Imod ->
      let zero = compare e 170 b (l e 0L) in
      let overflow =
        result e e.boolean 167 [ compare e 170 a (l e Int64.min_int); compare e 170 b (l e (-1L)) ]
      in
      let unsafe = result e e.boolean 166 [ zero; overflow ] in
      let divisor = select e (long e) unsafe (l e 1L) b in
      let value = op (if operation = Idiv then 135 else 138) [ a; divisor ] in
      let value = select e (long e) overflow (if operation = Idiv then a else l e 0L) value in
      select e (long e) zero (l e 0L) value

let wrap_integer e value =
  let shifted=result e (long e) 196 [value;l e 1L]in
  result e (long e) 195 [shifted;l e 1L]

let integer_float_binary e operation a b =
  match operation with
  | P.Add -> integer_binary e Iadd a b
  | Sub -> integer_binary e Isub a b
  | Mul -> integer_binary e Imul a b
  | Min -> select e (long e) (compare e 177 a b) a b
  | Max -> select e (long e) (compare e 173 a b) a b
  | Mod ->
      let r = integer_binary e Imod a b in
      let sign = result e e.boolean 165 [ compare e 177 r (l e 0L); compare e 177 b (l e 0L) ] in
      let adjust = result e e.boolean 167 [ compare e 171 r (l e 0L); sign ] in
      select e (long e) adjust (integer_binary e Iadd r b) r
  | _ -> assert false

let rec unguarded = function P.Guarded (_, body) -> unguarded body | body -> body

let needs_integer (layout : Flow_ir.Kernel_layout.t) =
  let whole = Array.make (Array.length layout.view.code) false in
  let needed = ref false in
  Array.iteri
    (fun slot instruction ->
      let instruction = unguarded instruction in
      let integer = Flow_ir.Core.Region.integer_register instruction in
      whole.(slot) <-
        (integer
        ||
        match instruction with
        | P.Const value -> Float.is_integer value && Float.abs value < 9007199254740992.
        | Convert (To_float, a) -> whole.(a)
        | Binary ((Add | Sub | Mul | Mod | Min | Max), a, b) -> whole.(a) && whole.(b)
        | _ -> false);
      if
        layout.used.(slot)
        && (integer
           ||
           match instruction with
           | P.Binary ((Add | Sub | Mul | Mod | Min | Max), _, _) -> whole.(slot)
           | _ -> false)
      then needed := true)
    layout.view.code;
  !needed

let round_float e op value =
  match op with
  | Flow.Packed_ops.Floor -> ext e 8 [ value ]
  | Ceil -> ext e 9 [ value ]
  | _ ->
      floating e 133 [ unary e Sign value; ext e 8 [ floating e 129 [ ext e 4 [ value ]; f e 0.5 ] ] ]

let int_compare_opcode = function
  | Flow.Packed_ops.Ilt -> 177
  | Ile -> 179
  | Igt -> 173
  | Ige -> 175
  | Ieq -> 170


(* An in-loop analytic test after the ray query, over [count] records: [values] are the nine fields so far;
   the loop carries them through phis and [body index phis] returns whether record [index] is nearer and its
   fields. No inner branches. *)
let analytic_loop e count values body =
  let words = e.words and float = e.float in
  let pre = e.block and header = Words.id words and body_label = Words.id words and continuing = Words.id words
  and finished = Words.id words and index_phi = Words.id words in
  let phis = Array.map (fun _ -> Words.id words) values in
  let main = words.body and loop_words = Buffer.create 512 and phi_words = Buffer.create 256 in
  jump e header;
  words.body <- loop_words;
  e.block <- header;
  let more = compare e 176 index_phi count in
  Words.instruction words.body 246 [ finished; continuing; 0 ];
  Words.instruction words.body 250 [ more; body_label; finished ];
  label e body_label;
  let take, next = body index_phi phis in
  let chosen = Array.mapi (fun k value -> select e float take value phis.(k)) next in
  jump e continuing;
  label e continuing;
  let following = result e e.uint 128 [ index_phi; u e 1 ] in
  jump e header;
  words.body <- phi_words;
  label e header;
  Words.instruction words.body 245 [ e.uint; index_phi; u e 0; pre; following; continuing ];
  Array.iteri (fun k phi -> Words.instruction words.body 245 [ float; phi; values.(k); pre; chosen.(k); continuing ]) phis;
  words.body <- main;
  Buffer.add_buffer main phi_words;
  Buffer.add_buffer main loop_words;
  label e finished;
  phis

let add e a b = floating e 129 [ a; b ] and sub e a b = floating e 131 [ a; b ] and mul e a b = floating e 133 [ a; b ]
let dot e a b = add e (mul e a.(0) b.(0)) (add e (mul e a.(1) b.(1)) (mul e a.(2) b.(2)))
let unit_direction e argument =
  let length = ext e 31 [ add e (mul e (argument 4) (argument 4)) (add e (mul e (argument 5) (argument 5)) (mul e (argument 6) (argument 6))) ] in
  let direction k = floating e 136 [ argument (4 + k); length ] in
  [| direction 0; direction 1; direction 2 |]

(* The procedural spheres of scene/trace. *)
let spheres_loop e (layout : Flow_ir.Kernel_layout.t) float_of r spheres values =
  let float = e.float in
  let argument k = float_of r.(k) in
  let count = uniform e (Option.get layout.scene_spheres) in
  analytic_loop e count values (fun index_phi phis ->
  let add = add e and sub = sub e and mul = mul e in
  let at = result e e.uint 132 [ index_phi; u e 4 ] in
  let sphere k = load e spheres (result e e.uint 128 [ at; u e k ]) in
  let radius = sphere 3 in
  let dn = unit_direction e argument in
  let oc = Array.init 3 (fun k -> sub (argument (1 + k)) (sphere k)) in
  let dot = dot e in
  let b = dot oc dn in
  let q = Array.init 3 (fun k -> sub oc.(k) (mul b dn.(k))) in
  let disc = sub (mul radius radius) (dot q q) in
  let root = ext e 31 [ select e float (compare e 190 disc (f e 0.)) disc (f e 0.) ] in
  let near = sub (floating e 127 [ b ]) root and far = add (floating e 127 [ b ]) root in
  let t = select e float (compare e 190 near (argument 7)) near far in
  let both = result e e.boolean 167 in
  let nearer = result e e.boolean 166 [ result e e.boolean 168 [ truth e phis.(7) ]; compare e 184 t phis.(0) ] in
  let take = both [ both [ compare e 190 disc (f e 0.); compare e 190 t (argument 7) ];
    both [ compare e 188 t (argument 8); nearer ] ] in
  let radius_inverse k = floating e 136 [ add oc.(k) (mul t dn.(k)); radius ] in
  take, [| t; sub (f e (-2.)) (result e float 112 [ index_phi ]); f e 0.; f e 0.;
    radius_inverse 0; radius_inverse 1; radius_inverse 2; f e 1.; f e (-1.) |])

(* The strands of scene/trace without native curves (Emit's strand_t and strand_hit): each segment a capsule,
   the cylinder's two roots within it and each end sphere's two roots beyond it; the nearest in
   [tmin, limit] wins. *)
let strands_loop e (layout : Flow_ir.Kernel_layout.t) float_of r strands values =
  let float = e.float in
  let argument k = float_of r.(k) in
  let count = uniform e (Option.get layout.scene_strand_count) in
  analytic_loop e count values (fun index_phi phis ->
  let add = add e and sub = sub e and mul = mul e and dot = dot e in
  let both = result e e.boolean 167 and gt a b = compare e 186 a b and ge a b = compare e 190 a b
  and lt a b = compare e 184 a b and le a b = compare e 188 a b in
  let at = result e e.uint 132 [ index_phi; u e 8 ] in
  let field k = load e strands (result e e.uint 128 [ at; u e k ]) in
  let a = Array.init 3 field and b = Array.init 3 (fun k -> field (4 + k)) and radius = field 3 in
  let dn = unit_direction e argument and o = Array.init 3 (fun k -> argument (1 + k)) in
  (* roots from the ray's point nearest the segment's middle, [shift] along it, as Emit's strand_t *)
  let shift = dot (Array.init 3 (fun k -> sub (mul (f e 0.5) (add a.(k) b.(k))) o.(k))) dn in
  let o = Array.init 3 (fun k -> add o.(k) (mul shift dn.(k))) in
  let ba = Array.init 3 (fun k -> sub b.(k) a.(k)) and oa = Array.init 3 (fun k -> sub o.(k) a.(k)) in
  let baba = dot ba ba and bard = dot ba dn and baoa = dot ba oa and rr = mul radius radius in
  let limit = sub (select e float (truth e phis.(7)) phis.(0) (argument 8)) shift and tmin = sub (argument 7) shift in
  let best = ref (f e Float.infinity) in
  let consider valid t =
    let y = add baoa (mul t bard) in
    let ok = both [ both [ valid y; both [ ge t tmin; le t limit ] ]; lt t !best ] in
    best := select e float ok t !best in
  let roots disc minus_b scale valid =
    let root = ext e 31 [ ext e 40 [ disc; f e 0. ] ] and real = ge disc (f e 0.) in
    List.iter (fun sign -> consider (fun y -> both [ real; valid y ])
      (floating e 136 [ (if sign then add else sub) minus_b root; scale ])) [ false; true ] in
  let big_a = sub baba (mul bard bard) in
  let big_b = sub (mul baba (dot dn oa)) (mul baoa bard) in
  let big_c = sub (sub (mul baba (dot oa oa)) (mul baoa baoa)) (mul rr baba) in
  roots (sub (mul big_b big_b) (mul big_a big_c)) (floating e 127 [ big_b ]) big_a
    (fun y -> both [ gt big_a (f e 0.); both [ gt y (f e 0.); lt y baba ] ]);
  List.iter (fun (centre, valid) ->
    let oc = Array.init 3 (fun k -> sub o.(k) centre.(k)) in
    let half = dot oc dn in
    roots (sub (mul half half) (sub (dot oc oc) rr)) (floating e 127 [ half ]) (f e 1.) valid)
    [ (a, fun y -> le y (f e 0.)); (b, fun y -> ge y baba) ];
  let t = !best in
  let along = ext e 43 [ floating e 136 [ add baoa (mul t bard); baba ]; f e 0.; f e 1. ] in
  let off = Array.init 3 (fun k -> sub (add oa.(k) (mul t dn.(k))) (mul along ba.(k))) in
  let scale = floating e 136 [ dot off ba; ext e 40 [ baba; f e 1e-8 ] ] in
  let off = Array.init 3 (fun k -> sub off.(k) (mul ba.(k) scale)) in
  let length = ext e 31 [ dot off off ] in
  le t limit, [| add t shift; result e float 112 [ index_phi ]; along; f e 0.; floating e 136 [ off.(0); length ];
    floating e 136 [ off.(1); length ]; floating e 136 [ off.(2); length ]; f e 1.; f e (-2.) |])

let trace_values e (layout : Flow_ir.Kernel_layout.t) float_of r =
  let words = e.words and float = e.float and boolean = e.boolean in
    let block = e.block and key = Array.to_list r in
    match List.find_opt (fun (k, b, _) -> k = key && b = block) e.trace_cache with
    | Some (_, _, values) -> values
    | None ->
        let accel, accel_type, vertices, spheres, instances, query, vec3, vec2 = Option.get e.scene in
        let v3 a b c = result e vec3 80 [ a; b; c ] and one = u e 1 in
        let argument k = float_of r.(k) in
        let direction = result e vec3 12 [ e.glsl; 69; v3 (argument 4) (argument 5) (argument 6) ] in
        Words.instruction words.body 4473
          [ query; result e accel_type 61 [ accel ]; one; u e 255; v3 (argument 1) (argument 2) (argument 3); argument 7; direction; argument 8 ];
        ignore (result e boolean 4477 [ query ]);
        let hit = compare e 170 (result e e.uint 4479 [ query; one ]) one in
        let t = result e float 6018 [ query; one ] and primitive = result e e.uint 6023 [ query; one ]
        and barycentrics = result e vec2 6024 [ query; one ] in
        let primitive = select e e.uint hit primitive (u e 0) in
        let instance = Option.map (fun _ -> select e e.uint hit (result e e.uint 6020 [ query; one ]) (u e 0)) instances in
        let row k = load e (Option.get instances) (result e e.uint 128 [ result e e.uint 132 [ Option.get instance; u e 16 ]; u e k ]) in
        let base = result e e.uint 132 [ (match instances with
          | None -> primitive
          | Some _ -> result e e.uint 128 [ result e e.uint 109 [ row 0 ]; primitive ]); u e 9 ] in
        let vertex corner = v3 (load e vertices (result e e.uint 128 [ base; u e (3 * corner) ]))
            (load e vertices (result e e.uint 128 [ base; u e (3 * corner + 1) ]))
            (load e vertices (result e e.uint 128 [ base; u e (3 * corner + 2) ])) in
        let a = vertex 0 in
        let edge corner = result e vec3 131 [ vertex corner; a ] in
        let normal = result e vec3 12 [ e.glsl; 69; result e vec3 12 [ e.glsl; 68; edge 1; edge 2 ] ] in
        let pick value miss = select e float hit value (f e miss) in
        let component vector k = result e float 81 [ vector; k ] in
        let world_normal k = (* the transpose of the world-to-object matrix applied to the object normal, normalised *)
          match instances with
          | None -> component normal k
          | Some _ ->
              let sum j = List.fold_left (fun acc i -> floating e 129 [ acc; floating e 133 [ row (1 + (4 * i) + j); component normal i ] ])
                (f e 0.) [ 0; 1; 2 ] in
              let x = sum 0 and y = sum 1 and z = sum 2 in
              let length = ext e 31 [ floating e 129 [ floating e 133 [ x; x ]; floating e 129 [ floating e 133 [ y; y ]; floating e 133 [ z; z ] ] ] ] in
              floating e 136 [ (match k with 0 -> x | 1 -> y | _ -> z); length ] in
        let values = [| pick t 0.; pick (result e float 112 [ primitive ]) (-1.);
          pick (component barycentrics 0) 0.; pick (component barycentrics 1) 0.;
          pick (world_normal 0) 0.; pick (world_normal 1) 0.; pick (world_normal 2) 0.; pick (f e 1.) 0.;
          pick (match instance with None -> f e 0. | Some i -> result e float 112 [ i ]) (-1.) |] in
        let values = spheres_loop e layout float_of r spheres values in
        let values = match e.strands with None -> values | Some strands -> strands_loop e layout float_of r strands values in
        e.trace_cache <- (key, e.block, values) :: e.trace_cache;
        values

(* The status bits of a zero divisor or a shift count outside [0, 62], from the exact count. *)
let integer_status e operation b =
  match operation with
  | Flow.Packed_ops.Idiv | Imod -> record_status e (compare e 170 b (l e 0L)) 2
  | Ishl | Ishr ->
      record_status e (result e e.boolean 166 [ compare e 177 b (l e 0L); compare e 173 b (l e 62L) ]) 4
  | _ -> ()

(* A narrow (32-bit unsigned) register's operation on the operands' low words (as [Emit.narrow_integer]). *)
let narrow_binary e operation a b =
  let op code operands = result e e.uint code operands in
  match operation with
  | Flow.Packed_ops.Iadd -> op 128 [ a; b ]
  | Isub -> op 130 [ a; b ]
  | Imul -> op 132 [ a; b ]
  | Iand -> op 199 [ a; b ]
  | Ior -> op 197 [ a; b ]
  | Ixor -> op 198 [ a; b ]
  | Ishl | Ishr ->
      let k = op 199 [ b; u e 63 ] in
      let shifted = op (if operation = Ishl then 196 else 194) [ a; op 199 [ k; u e 31 ] ] in
      select e e.uint (compare e 172 k (u e 31)) (u e 0) shifted
  | Idiv | Imod ->
      let zero = compare e 170 b (u e 0) in
      select e e.uint zero (u e 0) (op (if operation = Idiv then 134 else 137) [ a; select e e.uint zero (u e 1) b ])

(* One component of a prepared table's row [at] (a long), clamped into the table. *)
let element e (layout : Flow_ir.Kernel_layout.t) gathers prepared at width component =
  let k = Option.get (List.find_index (fun (slot, _) -> slot = prepared) layout.gathers) in
  let size = result e (Option.get e.ulong) 113 [ uniform e layout.gather_lengths.(k) ] in
  let size = result e (long e) 124 [ size ] in
  let last = integer_binary e Isub size (l e 1L) in
  let last = select e (long e) (compare e 177 last (l e 0L)) (l e 0L) last in
  let at = select e (long e) (compare e 177 at (l e 0L)) (l e 0L) at in
  let at = select e (long e) (compare e 173 at last) last at in
  let at = result e e.uint 113 [ at ] in
  result e e.float 124 [ load_uint e gathers.(k) (offset e at width component) ]

let emit_program e (layout : Flow_ir.Kernel_layout.t) inputs outputs gathers index =
  let words = e.words and float = e.float and boolean = e.boolean in
  let registers = Array.make (Array.length layout.view.code) 0 in
  let whole = Array.make (Array.length layout.view.code) None in
  let integer slot = Flow_ir.Core.Region.integer_register layout.view.code.(slot) in
  (* narrow Int registers (Kernel_layout.narrow) hold a 32-bit unsigned word *)
  let narrow = layout.narrow in
  let widen slot = result e (long e) 124 [ result e (Option.get e.ulong) 113 [ registers.(slot) ] ] in
  let known slot = narrow.(slot) || whole.(slot) <> None in
  let whole_of slot = if narrow.(slot) then widen slot else Option.get whole.(slot) in
  let float_of slot =
    if narrow.(slot) then from_long e (widen slot)
    else if integer slot then from_long e registers.(slot) else registers.(slot) in
  let invalid = ref (compare e 171 (u e 0) (u e 0)) in
  let to_int value =
    let valid =
      result e boolean 167
        [
          compare e 190 value (f e (-9223372036854775808.));
          compare e 184 value (f e 9223372036854775808.);
        ]
    in
    mark_invalid e invalid (result e boolean 168 [ valid ]);
    result e (long e) 110 [ select e float valid value (f e 0.) ]
  in
  let int_of slot =
    if narrow.(slot) then widen slot
    else if integer slot then registers.(slot)
    else match whole.(slot) with Some value -> value | None -> to_int (float_of slot)
  in
  let low slot = if narrow.(slot) then registers.(slot) else result e e.uint 113 [ int_of slot ] in
  let guard_condition guards =
    List.map
      (fun (slot, selected) ->
        let value = truth e (float_of slot) in
        if selected then value else result e boolean 168 [ value ])
      guards
    |> List.fold_left
         (fun combined value ->
           match combined with
           | None -> Some value
           | Some left -> Some (result e boolean 167 [ left; value ]))
         None
  in
  (* a narrow index is its low word read signed, as Metal's and WGSL's int(index) *)
  let element prepared at = element e layout gathers prepared
      (if narrow.(at) then result e (long e) 114 [ result e e.sint 124 [ registers.(at) ] ] else int_of at) in
  let rec expression slot instruction = if narrow.(slot) then match instruction with
    | P.Int_const n -> u e (n land 0xffffffff)
    | Int_binary (op, a, b) -> integer_status e op (int_of b); narrow_binary e op (low a) (low b)
    | Int_select (condition, yes, no) -> select e e.uint (truth e (float_of condition)) (low yes) (low no)
    | Loop_result (loop, component, _) -> registers.(loop + 1 + component)
    | _ -> assert false
    else wide_expression slot instruction
  and wide_expression slot = function
    | P.Trace (field, r) -> (trace_values e layout float_of r).(field)
    | P.Const value ->
        if e.long <> None && Float.is_integer value && Float.abs value < 9007199254740992. then
          whole.(slot) <- Some (l e (Int64.of_float value));
        f e value
    | Input (input, width, component) -> load e inputs.(input) (offset e index width component)
    | Uniform (input, component) -> uniform_float e (layout.uniforms.(input) + component)
    | Frame _ | Guarded _ -> assert false
    | Binary (((Add | Sub | Mul | Mod | Min | Max) as op), a, b)
      when known a && known b ->
        let value = integer_float_binary e op (whole_of a) (whole_of b) in
        whole.(slot) <- Some value;
        from_long e value
    | Binary (op, a, b) -> binary e op (float_of a) (float_of b)
    | Unary (op, a) -> unary e op (float_of a)
    | Fma (a, b, c) -> ext e 50 [ float_of a; float_of b; float_of c ]
    | (Checked_mul (projection, a, b) | Checked_fma (projection, a, b, _)) as instruction -> (
        let value =
          match instruction with
          | Checked_fma (_, _, _, c) -> ext e 50 [ float_of a; float_of b; float_of c ]
          | _ -> floating e 133 [ float_of a; float_of b ]
        in
        let valid = result e boolean 168 [ non_finite e value ] in
        match projection with
        | Value -> select e float valid value (f e 0.)
        | Valid -> numeric_truth e valid)
    | Noise3 (x, y, z, seed, octaves, _, _) ->
        let table = Option.get (Array.find_index (( = ) seed) layout.table_seeds) * 512 in
        result e float 57
          [ snd (Option.get e.noise); float_of x; float_of y; float_of z; u e table; u e octaves ]
    | Select (condition, yes, no) ->
        select e float (truth e (float_of condition)) (float_of yes) (float_of no)
    | Loop_result (loop, component, _) ->
        whole.(slot) <- whole.(loop + 1 + component);
        registers.(loop + 1 + component)
    | Int_const value -> l e (Int64.of_int value)
    | Int_input input -> to_int (load e inputs.(input) index)
    | Int_uniform input -> to_int (uniform_float e layout.uniforms.(input))
    | Int_frame _ -> to_int (uniform_float e layout.frames.(slot))
    | Int_binary (op, a, b) ->
        let a=int_of a and b=int_of b in
        integer_status e op b;
        wrap_integer e (integer_binary e op a b)
    | Int_compare (op, a, b) ->
        numeric_truth e (compare e (int_compare_opcode op) (int_of a) (int_of b))
    | Int_abs a ->
        let value = int_of a in
        wrap_integer e (select e (long e) (compare e 177 value (l e 0L)) (result e (long e) 126 [ value ]) value)
    | Int_select (condition, yes, no) ->
        select e (long e) (truth e (float_of condition)) (int_of yes) (int_of no)
    | Gather (prepared, at, width, component) -> element prepared at width component
    | Int_gather (prepared, at) -> to_int (element prepared at 1 0)
    | Convert (To_float, a) ->
        whole.(slot) <- if known a then Some (whole_of a) else None;
        float_of a
    | Convert (_, a) when known a -> whole_of a
    | Convert ((Truncate | Coerce_int | Integer_result _), a) -> to_int (float_of a)
    | Convert (((Floor | Ceil | Round) as op), a) ->
        let value = float_of a in
        to_int (round_float e op value)
    | _ -> fail "This instruction is not supported by the SPIR-V kernel target."
  in
  let rec emit slot guards = function
    | P.Guarded (more, instruction) -> emit slot (guards @ more) instruction
    | instruction ->
        let compute () =
          let value =
            match instruction with
            | P.Frame _ -> uniform_float e layout.frames.(slot)
            | _ -> expression slot instruction
          in
          if layout.finite_checks.(slot) && not (integer slot) then
            mark_invalid e invalid (non_finite e value);
          value
        in
        if guards = [] then begin
          registers.(slot) <- compute ();
          if integer slot && not narrow.(slot) then whole.(slot) <- Some registers.(slot)
        end
        else begin
          let before = e.block and before_invalid = !invalid in
          let condition = Option.get (guard_condition guards) in
          let yes = Words.id words and after = Words.id words in
          Words.instruction words.body 247 [ after; 0 ];
          Words.instruction words.body 250 [ condition; yes; after ];
          label e yes;
          let value = compute () in
          let yes_invalid = !invalid in
          let last = e.block in
          jump e after;
          label e after;
          let ty, zero = if narrow.(slot) then (e.uint, u e 0) else if integer slot then (long e, l e 0L) else (float, f e 0.) in
          registers.(slot) <- result e ty 245 [ value; last; zero; before ];
          if e.loop_depth = 0 then
            invalid := result e boolean 245 [ yes_invalid; last; before_invalid; before ];
          if narrow.(slot) then ()
          else if integer slot then whole.(slot) <- Some registers.(slot)
          else
            whole.(slot) <-
              Option.map
                (fun value -> result e (long e) 245 [ value; last; l e 0L; before ])
                whole.(slot)
        end
  in
  let emit_loop header (control : P.loop) ~next ~stop ~body:emit_body =
    if layout.used.(header) then begin
      let guards = match layout.view.code.(header) with
        | P.Guarded (guards, _) -> guards
        | _ -> []
      in
      let guarded = guards <> [] in
      let preheader = Words.id words
      and skipped = Words.id words
      and loop_header = Words.id words
      and loop_body = Words.id words
      and continuing = Words.id words
      and finished = Words.id words
      and after_guard = Words.id words in
      let guard_condition =
        let found = guard_condition guards in
        let default = compare e 171 (u e 0) (u e 0) in
        Option.value found ~default
      in
      if guarded then begin
        Words.instruction words.body 247 [ after_guard; 0 ];
        Words.instruction words.body 250 [ guard_condition; preheader; skipped ];
        label e skipped;
        jump e after_guard;
        label e preheader
      end else begin
        jump e preheader;
        label e preheader
      end;
      let first = int_of control.first and last = int_of control.last in
      let carried slot source = if narrow.(slot) then low source else if integer slot then int_of source else registers.(source) in
      let initial = Array.mapi (fun component source -> carried (header + 1 + component) source) control.initial in
      let index_type = if narrow.(header) then e.uint else long e in
      let index_first, index_last = if narrow.(header) then low control.first, low control.last else first, last in
      let too_long =
        match control.limit with
        | None -> compare e 171 (u e 0) (u e 0)
        | Some limit ->
            let positive = compare e 173 last first in
            let unsigned_last = result e (Option.get e.ulong) 124 [ last ]
            and unsigned_first = result e (Option.get e.ulong) 124 [ first ] in
            let distance = result e (Option.get e.ulong) 130 [ unsigned_last; unsigned_first ] in
            let bound = Words.wide_constant words (Option.get e.ulong) (Int64.of_int limit) in
            let too_long = result e boolean 167 [ positive; result e boolean 172 [ distance; bound ] ] in
            record_status e too_long 1;
            too_long
      in
      let preheader_pred = e.block in
      let index_phi = Words.id words in
      let carry_phis = Array.map (fun _ -> Words.id words) control.initial in
      let stop_phi = Option.map (fun _ -> Words.id words) stop in
      registers.(header) <- index_phi;
      whole.(header) <- if narrow.(header) then None else Some index_phi;
      Array.iteri
        (fun component phi ->
          let slot = header + 1 + component in
          registers.(slot) <- phi;
          whole.(slot) <- if integer slot && not narrow.(slot) then Some phi else None)
        carry_phis;

      let outer_body = words.body in
      let body_words = Buffer.create 512 in
      words.body <- body_words;
      label e loop_body;
      e.loop_depth <- e.loop_depth + 1;
      emit_body ();
      let next_values =
        Array.mapi
          (fun component source ->
            let slot = header + 1 + component in
            if integer slot then carried slot source else float_of source)
          next
      in
      let stop_value = Option.map (fun source -> truth e (float_of source)) stop in
      e.loop_depth <- e.loop_depth - 1;
      jump e continuing;

      let continuing_words = Buffer.create 128 in
      words.body <- continuing_words;
      label e continuing;
      let next_index = if narrow.(header) then result e e.uint 128 [ index_phi; u e 1 ]
        else integer_binary e Flow.Packed_ops.Iadd index_phi (l e 1L) in
      jump e loop_header;
      words.body <- outer_body;

      let initial_stop = compare e 171 (u e 0) (u e 0) in
      e.block <- preheader_pred;
      jump e loop_header;
      label e loop_header;
      Words.instruction words.body 245 [ index_type; index_phi; index_first; preheader_pred; next_index; continuing ];
      Array.iteri
        (fun component phi ->
          let slot = header + 1 + component in
          let ty, start = if narrow.(slot) then (e.uint, initial.(component)) else if integer slot then (long e, initial.(component)) else (float, initial.(component)) in
          Words.instruction words.body 245 [ ty; phi; start; preheader_pred; next_values.(component); continuing ])
        carry_phis;
      Option.iter
        (fun phi ->
          Words.instruction words.body 245
            [ boolean; phi; initial_stop; preheader_pred; Option.get stop_value; continuing ])
        stop_phi;
      let in_range = compare e (if narrow.(header) then 176 else 177) index_phi index_last in
      let not_stopped = Option.fold ~none:(compare e 171 (u e 0) (u e 0))
          ~some:(fun phi -> result e boolean 168 [ phi ]) stop_phi in
      let condition = result e boolean 167 [ result e boolean 167 [ in_range; not_stopped ]; result e boolean 168 [ too_long ] ] in
      Words.instruction words.body 246 [ finished; continuing; 0 ];
      Words.instruction words.body 250 [ condition; loop_body; finished ];
      Buffer.add_buffer words.body body_words;
      Buffer.add_buffer words.body continuing_words;
      label e finished;
      if guarded then begin
        jump e after_guard;
        label e after_guard;
        registers.(header) <- result e index_type 245 [ index_phi; finished; (if narrow.(header) then u e 0 else l e 0L); skipped ];
        Array.iteri
          (fun component phi ->
            let slot = header + 1 + component in
            let ty, zero = if narrow.(slot) then (e.uint, u e 0) else if integer slot then (long e, l e 0L) else (float, f e 0.) in
            registers.(slot) <- result e ty 245 [ phi; finished; zero; skipped ];
            whole.(slot) <- if integer slot && not narrow.(slot) then Some registers.(slot) else None)
          carry_phis
      end;
    end
  in
  Flow_ir.Kernel_layout.iter layout
    ~instruction:(fun slot instruction ->
      if layout.used.(slot) then
        match unguarded instruction with P.Array_prepare _ -> () | _ -> emit slot [] instruction)
    ~loop:emit_loop;
  Array.iteri
    (fun output values ->
      Array.iteri
        (fun component register ->
          if layout.output_checks && not (integer register) then
            mark_invalid e invalid (non_finite e (float_of register));
          store e outputs.(output)
            (offset e index (Array.length values) component)
            (float_of register))
        values)
    layout.view.outputs;
  !invalid

let resident_count e (layout : Flow_ir.Kernel_layout.t) index buffers =
  let n = uniform e 0 in
  match (layout.counts, buffers) with
  | Some bytes, Some (input, output) ->
      let word = bytes / 4 in
      let n =
        conditional e e.uint
          (compare e 171 (uniform e (word + 2)) (u e 0))
          (fun () ->
            let product = result e e.uint 132 [ load_uint e input (u e 0); uniform e word ] in
            let divisor = uniform e (word + 1) in
            let divisor = select e e.uint (compare e 170 divisor (u e 0)) (u e 1) divisor in
            let bound = result e e.uint 134 [ product; divisor ] in
            select e e.uint (compare e 176 n bound) n bound)
          (fun () -> n)
      in
      ignore
        (conditional e e.uint
           (compare e 170 index (u e 0))
           (fun () ->
             List.iteri
               (fun slot value -> store_uint e output (u e slot) value)
               [ n; result e e.uint 132 [ n; u e 6 ]; u e 1; u e 0; u e 0; u e 0 ];
             n)
           (fun () -> n));
      n
  | None, None -> n
  | _ -> assert false

let kernel ?(fast = false) ?(instanced = false) ?(motion = false) ?(curves = false) program =
  try
    (match P.gpu_refusals ~fused:true ~fast program with
    | [] -> ()
    | diagnostic :: _ -> raise (Diagnostic.Error diagnostic));
    let layout = Flow_ir.Kernel_layout.plan ~fast ~instanced ~motion ~curves program in
    (* SPV_NV_ray_tracing_motion_blur moves only trace-rays pipelines, not ray queries *)
    if layout.scene_motion then
      fail "scene/trace motion has no SPIR-V ray-query form; build the scene at the shutter midpoint (Run.build_scene without Ray_tracing_motion).";
    let words = Words.create () in
    let void = Words.declare words 19 []
    and boolean = Words.declare words 20 []
    and uint = Words.declare words 21 [ 32; 0 ]
    and sint = Words.declare words 21 [ 32; 1 ]
    and float = Words.declare words 22 [ 32 ] in
    let long, ulong =
      if fast && needs_integer layout then
        (Some (Words.declare words 21 [ 64; 1 ]), Some (Words.declare words 21 [ 64; 0 ]))
      else (None, None)
    in
    let vector3 = Words.declare words 23 [ uint; 3 ]
    and vector4 = Words.declare words 23 [ uint; 4 ] in
    let function_type = Words.declare words 33 [ void ] in
    let storage, storage_pointer = storage_type words float
    and uint_storage, uint_storage_pointer = storage_type words uint in
    let uint_function_pointer = Words.declare words 32 [ 7; uint ] in
    let uniform_length =
      Words.constant words uint (Int32.of_int (Flow_ir.Kernel_layout.uniform_words layout))
    in
    let uniform_array = Words.declare words 28 [ vector4; uniform_length ] in
    let uniform_struct = Words.declare words 30 [ uniform_array ] in
    Words.instruction words.annotations 71 [ uniform_array; 6; 16 ];
    Words.instruction words.annotations 71 [ uniform_struct; 2 ];
    Words.instruction words.annotations 72 [ uniform_struct; 0; 35; 0 ];
    let uniform_type = Words.declare words 32 [ 2; uniform_struct ]
    and uniform_pointer = Words.declare words 32 [ 2; uint ]
    and invocation_pointer = Words.declare words 32 [ 1; vector3 ] in
    let interface = ref [] in
    let globals = ref [] in
    let variable pointer storage =
      let id = Words.id words in
      Words.instruction words.declarations 59 [ pointer; id; storage ];
      globals := id :: !globals;
      id
    in
    let bind pointer storage binding kind =
      let id = variable pointer storage in
      Words.instruction words.annotations 71 [ id; 34; 0 ];
      Words.instruction words.annotations 71 [ id; 33; binding ];
      interface := Ogpu.Shader.{ group = 0; binding; kind; visibility = [ Compute ] } :: !interface;
      id
    in
    let inputs =
      Array.mapi
        (fun index used -> if used then bind storage 12 index Ogpu.Shader.Storage_buffer else 0)
        layout.input_used
    in
    let outputs =
      Array.init layout.output_count (fun index ->
          bind storage 12 (layout.input_count + index) Ogpu.Shader.Storage_buffer)
    in
    let uniform_buffer = bind uniform_type 2 layout.uniform_binding Ogpu.Shader.Uniform_buffer
    and status = bind uint_storage 12 layout.status_binding Ogpu.Shader.Storage_buffer in
    let table =
      Option.map
        (fun binding -> bind uint_storage 12 binding Ogpu.Shader.Storage_buffer)
        layout.table_binding
    in
    let gathers =
      Array.of_list
        (List.mapi
           (fun k _ -> bind uint_storage 12 (layout.gather_base + k) Ogpu.Shader.Storage_buffer)
           layout.gathers)
    in
    let counts =
      Option.map
        (fun _ ->
          let input = bind uint_storage 12 layout.count_base Ogpu.Shader.Storage_buffer in
          let output = bind uint_storage 12 (layout.count_base + 1) Ogpu.Shader.Storage_buffer in
          input, output)
        layout.counts
    in
    let scene =
      Option.map
        (fun binding ->
          let accel_type = Words.declare words 5341 [] in
          let accel = bind (Words.declare words 32 [ 0; accel_type ]) 0 binding Ogpu.Shader.Acceleration_structure in
          let vertices = bind storage 12 (binding + 1) Ogpu.Shader.Storage_buffer in
          let spheres = bind storage 12 (binding + 2) Ogpu.Shader.Storage_buffer in
          let instances = Option.map (fun _ -> bind storage 12 (binding + 3) Ogpu.Shader.Storage_buffer) layout.scene_instances in
          let query = variable (Words.declare words 32 [ 6; Words.declare words 4472 [] ]) 6 in
          (accel, accel_type, vertices, spheres, instances, query, Words.declare words 23 [ float; 3 ], Words.declare words 23 [ float; 2 ]))
        layout.scene_binding
    in
    let strands = Option.map (fun binding -> bind storage 12 binding Ogpu.Shader.Storage_buffer) layout.scene_strands in
    let noise = Option.map (fun _ -> (Words.id words, Words.id words)) table in
    let invocation = variable invocation_pointer 1 in
    Words.instruction words.annotations 71 [ invocation; 11; 28 ];
    let glsl = Words.id words and main = Words.id words in
    let e =
      {
        words;
        boolean;
        uint;
        sint;
        float;
        long;
        ulong;
        glsl;
        uniform = uniform_buffer;
        uniform_pointer;
        storage_pointer;
        uint_storage_pointer;
        status;
        uint_function_pointer;
        status_bits = 0;
        table;
        noise;
        scene;
        block = 0;
        trace_cache = [];
        strands;
        loop_depth = 0;
      }
    in
    Words.instruction words.body 54 [ void; main; 0; function_type ];
    label e (Words.id words);
    e.status_bits <- result e uint_function_pointer 59 [ 7; u e 0 ];
    let gid = result e vector3 61 [ invocation ] in
    let index = result e uint 81 [ gid; 0 ] in
    let body = Words.id words and finish = Words.id words in
    let count = resident_count e layout index counts in
    let within = compare e 176 index count in
    Words.instruction words.body 247 [ finish; 0 ];
    Words.instruction words.body 250 [ within; body; finish ];
    label e body;
    let invalid = emit_program e layout inputs outputs gathers index in
    record_status e invalid 1;
    let bits = result e uint 61 [ e.status_bits ] in
    let flagged = compare e 171 bits (u e 0) in
    let bad = Words.id words and after = Words.id words in
    Words.instruction words.body 247 [ after; 0 ];
    Words.instruction words.body 250 [ flagged; bad; after ];
    label e bad;
    let status_pointer = result e uint_storage_pointer 65 [ status; u e 0; u e 0 ] in
    ignore (result e uint 241 [ status_pointer; u e 1; u e 0; bits ]);
    jump e after;
    label e after;
    jump e finish;
    label e finish;
    Words.instruction words.body 253 [];
    Words.instruction words.body 56 [];
    noise_functions e;
    let module_bytes =
      Buffer.create (Buffer.length words.body + Buffer.length words.declarations + 512)
    in
    List.iter
      (Buffer.add_int32_le module_bytes)
      [ 0x07230203l; (if scene = None then 0x00010300l else 0x00010400l); 0l; Int32.of_int words.next; 0l ];
    Words.instruction module_bytes 17 [ 1 ];
    if long <> None then Words.instruction module_bytes 17 [ 11 ];
    if scene <> None then begin
      Words.instruction module_bytes 17 [ 4472 ];
      Words.instruction module_bytes 10 (Words.string_words "SPV_KHR_ray_query")
    end;
    Words.instruction module_bytes 11 (glsl :: Words.string_words "GLSL.std.450");
    Words.instruction module_bytes 14 [ 0; 1 ];
    let entry = "rays_kernel" in
    Words.instruction module_bytes 15 ((5 :: main :: Words.string_words entry) @ (if scene = None then [ invocation ] else List.rev !globals));
    Words.instruction module_bytes 16 [ main; 17; 64; 1; 1 ];
    List.iter (Buffer.add_buffer module_bytes) [ words.annotations; words.declarations; words.body ];
    Ok
      {
        bytes = Buffer.to_bytes module_bytes;
        entry;
        interface = List.rev !interface;
        layout;
        uniform_bytes = Flow_ir.Kernel_layout.uniform_words layout * 16;
      }
  with Diagnostic.Error diagnostic -> Error diagnostic
