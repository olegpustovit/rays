module P = Flow_ir.Packed
module S = Ogpu.Shader

type msl = {
  source : string;
  entry : string;
  interface : S.binding list;
  uniform_layout : (string * int * int) list;
  uniform_bytes : int;
  table_seeds : int array;
  output_widths : int array;
  input_widths : int array;
  gathers : (int * int) array;
  counts : int option;
  scene : int option;
}

let names = Flow.Packed_ops.names

(* The trace helpers, specialised by two static scene facts: [instanced] (an instance structure and its table)
   and [motion] (the query's time: instance transforms, or vertex keyframes in a flat scene). The static text
   is unchanged by the motion insertions, which print only for a moving scene. *)
let structure_type instanced motion =
  match (instanced, motion) with
  | true, false -> "instance_acceleration_structure"
  | false, false -> "primitive_acceleration_structure"
  | true, true -> "acceleration_structure<instancing, instance_motion>"
  | false, true -> "acceleration_structure<primitive_motion>"

(* A raster-stage intrinsic (Flow_ir.Core.Region.stage); texture t is sampled with sampler t + 1 and read
   (whole texels, no sampler) at integer coordinates. *)
let stage_intrinsic stage operands =
  match stage with
  | Flow_ir.Core.Region.Fwidth -> "fwidth(" ^ operands.(0) ^ ")"
  | Sample (t, c) ->
      Printf.sprintf "texture%d.sample(sampler%d,float2(%s,%s)).%c" t (t + 1) operands.(0)
        operands.(1) "xyzw".[c]
  | Sample_level (t, c) ->
      Printf.sprintf "texture%d.sample(sampler%d,float2(%s,%s),level(%s)).%c" t (t + 1) operands.(0)
        operands.(1) operands.(2) "xyzw".[c]
  | Read (t, c) ->
      Printf.sprintf "texture%d.read(uint2(%s,%s)).%c" t operands.(0) operands.(1) "xyzw".[c]
  | Texture_size (t, c) ->
      Printf.sprintf "float(texture%d.get_%s())" t (if c = 0 then "width" else "height")
  | _ -> invalid_arg "Emit.stage_intrinsic"

let fail message = raise (Diagnostic.Error (Diagnostic.error ~code:"E_GPU_FORM" message))
let register index = "r" ^ string_of_int index

let rec guarded guards = function
  | P.Guarded (more, body) -> guarded (guards @ more) body
  | instruction -> (guards, instruction)

let binary operation a b =
  let infix operator = "(" ^ a ^ operator ^ b ^ ")" in
  match operation with
  | P.Add -> infix "+"
  | Sub -> infix "-"
  | Mul -> infix "*"
  | Div -> "(" ^ b ^ "==0.0f?0.0f:" ^ a ^ "/" ^ b ^ ")"
  | Mod -> "(" ^ b ^ "==0.0f?0.0f:fmod(fmod(" ^ a ^ "," ^ b ^ ")+" ^ b ^ "," ^ b ^ "))"
  | Pow -> "pow(fabs(" ^ a ^ ")," ^ b ^ ")"
  | Min -> "min(" ^ a ^ "," ^ b ^ ")"
  | Max -> "max(" ^ a ^ "," ^ b ^ ")"
  | Lt -> infix "<"
  | Le -> infix "<="
  | Gt -> infix ">"
  | Ge -> infix ">="
  | Eq -> infix "=="
  | And -> "(" ^ a ^ "!=0.0f&&" ^ b ^ "!=0.0f)"
  | Or -> "(" ^ a ^ "!=0.0f||" ^ b ^ "!=0.0f)"
  | Atan2 -> "atan2(" ^ a ^ "," ^ b ^ ")"
  | Hypot -> fail "Native-safe hypot is not supported by the Metal kernel target."
  | Ldexp -> "ldexp(" ^ a ^ ",int(" ^ b ^ "))"

let unary operation a =
  match operation with
  | P.Sin -> "sin(" ^ a ^ ")"
  | Cos -> "cos(" ^ a ^ ")"
  | Sqrt -> "sqrt(fabs(" ^ a ^ "))"
  | Abs -> "fabs(" ^ a ^ ")"
  | Not -> "(" ^ a ^ "==0.0f)"
  | Tan -> "tan(" ^ a ^ ")"
  | Asin | Acos ->
      "(fabs(" ^ a ^ ")>1.0f?as_type<float>(0x7fc00000u):"
      ^ (if operation = P.Asin then "asin" else "acos")
      ^ "(" ^ a ^ "))"
  | Exp -> "exp(" ^ a ^ ")"
  | Log -> "(" ^ a ^ "<=0.0f?as_type<float>(0x7fc00000u):log(" ^ a ^ "))"
  | Rsqrt -> "rsqrt(fabs(" ^ a ^ "))"
  | Sign -> "(" ^ a ^ ">0.0f?1.0f:(" ^ a ^ "<0.0f?-1.0f:" ^ a ^ "))"

(* The 32-bit lowering of checked arithmetic: the value or zero, or the finite flag. *)
let checked projection expression =
  match projection with
  | P.Value -> Printf.sprintf "(isfinite(%s)?%s:0.0f)" expression expression
  | Valid -> Printf.sprintf "(isfinite(%s)?1.0f:0.0f)" expression

let wrap_integer value = "(as_type<long>(ulong(" ^ value ^ ")<<1)>>1)"

(* Integer registers occupy 64 bits; Int operations wrap in the signed 63-bit domain. *)
let integer_status ?(unsigned = false) indentation operation b =
  match operation with
  | Flow.Packed_ops.Idiv | Imod -> Printf.sprintf "%ssb|=(%s==0)?2u:0u;\n" indentation b
  | (Ishl | Ishr) when unsigned -> Printf.sprintf "%ssb|=(%s>62u)?4u:0u;\n" indentation b
  | Ishl | Ishr -> Printf.sprintf "%ssb|=(%s<0 || %s>62)?4u:0u;\n" indentation b b
  | _ -> ""

(* The status bits gather in the thread's [sb] and reach the shared word with the kernel's one atomic,
   at its end: an atomic per operation serializes the kernel and stops the compiler moving arithmetic.
   Thread 0 always publishes, so the status binding stays referenced. *)
let flush_status = "if(sb!=0u||i==0) atomic_fetch_or_explicit(status,sb,memory_order_relaxed);"

module Scope = Flow_ir.Kernel_layout.Scope

(* A scope's declarations, consecutive ones of one type on one line (at most twelve), as written by hand *)
let declarations decls =
  let out = Buffer.create (Buffer.length decls) in
  let flush = function
    | [] -> ()
    | (ty, _) :: _ as group ->
        Printf.bprintf out "  %s %s;\n" ty (String.concat "," (List.rev_map snd group))
  in
  let split line =
    (* "  T item;" into (T, item) *)
    if not (String.starts_with ~prefix:"  " line && String.ends_with ~suffix:";" line) then None
    else
      let rest = String.sub line 2 (String.length line - 3) in
      match String.index_opt rest ' ' with
      | Some j when j > 0 ->
          Some (String.sub rest 0 j, String.sub rest (j + 1) (String.length rest - j - 1))
      | _ -> None
  in
  let group =
    List.fold_left
      (fun group line ->
        match (split line, group) with
        | Some (ty, item), (t, _) :: _ when t = ty && List.length group < 12 -> (ty, item) :: group
        | Some (ty, item), _ ->
            flush group;
            [ (ty, item) ]
        | None, _ ->
            flush group;
            if line <> "" then Buffer.add_string out (line ^ "\n");
            [])
      []
      (String.split_on_char '\n' (Buffer.contents decls))
  in
  flush group;
  Buffer.contents out

let scope = Scope.create
and indent = Scope.indent

let enter =
  Scope.enter ~opening:(fun condition selected ->
      Printf.sprintf "if(r%d%s0.0f) {" condition (if selected then "!=" else "=="))

let integer operation a b =
  wrap_integer
    (match operation with
    | Flow.Packed_ops.Iadd -> "(ulong(" ^ a ^ ")+ulong(" ^ b ^ "))"
    | Isub -> "(ulong(" ^ a ^ ")-ulong(" ^ b ^ "))"
    | Imul -> "(ulong(" ^ a ^ ")*ulong(" ^ b ^ "))"
    | Idiv -> "(" ^ b ^ "==0?0:" ^ a ^ "/" ^ b ^ ")"
    | Imod -> "(" ^ b ^ "==0?0:" ^ a ^ "%" ^ b ^ ")"
    | Iand -> "(" ^ a ^ "&" ^ b ^ ")"
    | Ior -> "(" ^ a ^ "|" ^ b ^ ")"
    | Ixor -> "(" ^ a ^ "^" ^ b ^ ")"
    | Ishl -> "(ulong(" ^ a ^ ")<<(" ^ b ^ "&63))"
    | Ishr -> "long((ulong(" ^ a ^ ")&0x7fffffffffffffffUL)>>(" ^ b ^ "&63))")

(* A narrow (32-bit unsigned) Int register's operation: the low 32 bits of the signed-63 result.
   Shifts by [b & 63] of 32 or more leave none of them (or, right, none of a fitting operand). *)
let narrow_integer operation a b =
  let shift operator = Printf.sprintf "((%s&63)>31?0u:(%s%suint(%s&63)))" b a operator b in
  match operation with
  | Flow.Packed_ops.Iadd -> "(" ^ a ^ "+" ^ b ^ ")"
  | Isub -> "(" ^ a ^ "-" ^ b ^ ")"
  | Imul -> "(" ^ a ^ "*" ^ b ^ ")"
  | Iand -> "(" ^ a ^ "&" ^ b ^ ")"
  | Ior -> "(" ^ a ^ "|" ^ b ^ ")"
  | Ixor -> "(" ^ a ^ "^" ^ b ^ ")"
  | Ishl -> shift "<<"
  | Ishr -> shift ">>"
  | Idiv -> "(" ^ b ^ "==0u?0u:" ^ a ^ "/" ^ b ^ ")"
  | Imod -> "(" ^ b ^ "==0u?0u:" ^ a ^ "%" ^ b ^ ")"

let comparison operation a b =
  let op =
    match operation with
    | Flow.Packed_ops.Ilt -> "<"
    | Ile -> "<="
    | Igt -> ">"
    | Ige -> ">="
    | Ieq -> "=="
  in
  "(" ^ a ^ op ^ b ^ "?1.0f:0.0f)"

(* A ray query (Trace): its result is held in locals declared in the scope and stored once a scope,
   guard set and ray (registers [r]), where its first field is read; [a k] prints operand k. *)
let trace_query ~hits ~instancing ~moving (current : Scope.t) guards slot stage r a =
  let curve =
    match stage with Flow_ir.Core.Region.Trace_curves _ | Trace_curves_any -> true | _ -> false
  and any = stage = Flow_ir.Core.Region.Trace_any || stage = Trace_curves_any in
  let key = (current.Scope.id, guards, curve, any, Array.to_list r) in
  let name =
    match Hashtbl.find_opt hits key with
    | Some name -> name
    | None ->
        let name = Printf.sprintf "hit%d" slot in
        let declare ty field zero =
          Printf.bprintf current.Scope.decls "  %s %s%s=%s;\n" ty name field zero
        in
        let tags =
          if not moving then []
          else [ (if instancing then "instance_motion" else "primitive_motion") ]
        in
        let data =
          String.concat ", "
            ((if curve then [ "curve_data" ] else if any then [] else [ "triangle_data" ])
            @ (if instancing && not curve then [ "instancing" ] else [])
            @ if curve then [] else tags)
        in
        let target = if curve then "scene_curves" else "scene" ^ if moving then "," ^ a 8 else "" in
        let call =
          Printf.sprintf "q.intersect(ray(float3(%s,%s,%s),float3(%s,%s,%s),%s,%s),%s)" (a 0) (a 1)
            (a 2) (a 3) (a 4) (a 5) (a 6) (a 7) target
        in
        let kind = if curve then "curve" else "triangle" in
        let store =
          if any then (
            declare "bool" "" "false";
            Printf.sprintf "%s=%s.type==intersection_type::%s;" name call kind)
          else begin
            declare "bool" "_h" "false";
            declare "float" "_t" "0.0f";
            declare "uint" "_p" "0u";
            if curve then declare "float" "_u" "0.0f" else declare "float2" "_uv" "0.0f";
            if instancing && not curve then declare "uint" "_i" "0u";
            Printf.sprintf
              "auto h=%s;%s_h=h.type==intersection_type::%s;%s_t=h.distance;%s_p=h.primitive_id;%s"
              call name kind name name
              (if curve then name ^ "_u=h.curve_parameter;"
               else
                 name ^ "_uv=h.triangle_barycentric_coord;"
                 ^ if instancing then name ^ "_i=h.instance_id;" else "")
          end
        in
        Printf.bprintf current.Scope.code
          "%s{intersector<%s> \
           q;q.assume_geometry_type(geometry_type::%s);q.force_opacity(forced_opacity::opaque);%s%s%s}\n"
          (indent current) data kind
          (if curve then
             "q.assume_curve_type(curve_type::round);q.assume_curve_basis(curve_basis::linear);q.assume_curve_control_point_count(2);"
           else "")
          (if any then "q.accept_any_intersection(true);" else "")
          store;
        Hashtbl.add hits key name;
        name
  in
  match stage with
  | Trace_any | Trace_curves_any -> "(" ^ name ^ "?1.0f:0.0f)"
  | Trace_triangles 0 | Trace_curves 0 -> name ^ "_t"
  | Trace_triangles 1 | Trace_curves 1 -> name ^ "_p"
  | Trace_triangles 2 -> name ^ "_uv.x"
  | Trace_triangles 3 -> name ^ "_uv.y"
  | Trace_curves 2 -> name ^ "_u"
  | Trace_triangles 4 | Trace_curves 3 -> "(" ^ name ^ "_h?1.0f:0.0f)"
  | _ -> if instancing then name ^ "_i" else "0u"

(* a scene table read at uint indices [index k]: a triangle corner (through the instance's vertex base),
   an instance-table word, a sphere or a strand end *)
let scene_read ~instancing ~stride stage index =
  let xyz c = String.make 1 "xyzw".[c] in
  match stage with
  | Flow_ir.Core.Region.Scene_vertex (ending, c) ->
      let base =
        if instancing then
          Printf.sprintf "(uint(scene_instances[%s*%du])+%s)" (index 0) stride (index 1)
        else index 1
      in
      Printf.sprintf "scene_vertices%s[%s*3u+%s].%s"
        (if ending then "_end" else "")
        base (index 2) (xyz c)
  | Scene_instance -> Printf.sprintf "scene_instances[%s*%du+%s]" (index 0) stride (index 1)
  | Scene_sphere c -> Printf.sprintf "scene_spheres[%s].%s" (index 0) (xyz c)
  | Scene_strand c -> Printf.sprintf "scene_strands[2u*%s+%s].%s" (index 0) (index 1) (xyz c)
  | _ -> fail "A raster-stage intrinsic runs only in a fragment stage."

(* A raster placement (Raster): elements are vertex ids or interpolated fragment inputs, gathers read the
   renderer's bound buffers, and outputs are stage results; the stage function around the body is Raster's. *)
type raster = {
  input : int -> int -> string;
  int_input : int -> string;
  buffer : int -> string;
  output : int -> int -> string -> string;
  builtin : string -> string;
}

(* A compute stage's workgroup intrinsics (Raster.compute): a group scan of x is a Hillis-Steele scan in
   threadgroup memory between barriers, printed once a scanned register where its first field is read
   (in uniform control flow: the root scope); a store writes a bound buffer's element and is 1. *)
let group_intrinsic ~scans ~(raster : raster) (current : Scope.t) guards stage (r : int array)
    ~table ~low ~float_of =
  match stage with
  | Flow_ir.Core.Region.Group_scan field ->
      if current.Scope.id <> 0 || guards <> [] then
        fail "A group scan runs in uniform control flow, outside loops and branches.";
      let name = Printf.sprintf "scan%d" r.(0)
      and size = raster.builtin "gpu/group-size"
      and x = low r.(0) in
      if not (Hashtbl.mem scans r.(0)) then begin
        Hashtbl.add scans r.(0) ();
        Printf.bprintf current.Scope.decls "  uint %s_before=0u;\n  uint %s_total=0u;\n" name name;
        Printf.bprintf current.Scope.code
          "  threadgroup uint %s[%s];\n\
          \  {uint v=%s;%s[lid]=v;for(uint \
           d=1u;d<%su;d<<=1u){threadgroup_barrier(mem_flags::mem_threadgroup);uint \
           w=lid>=d?%s[lid-d]:0u;threadgroup_barrier(mem_flags::mem_threadgroup);v+=w;%s[lid]=v;}threadgroup_barrier(mem_flags::mem_threadgroup);%s_before=v-%s;%s_total=%s[%s-1];}\n"
          name size x name size name name name x name name size
      end;
      name ^ if field = 0 then "_before" else "_total"
  | Store ->
      let u, ty = table r.(0) in
      Printf.sprintf "(%s[%s]=%s,1u)" (raster.buffer u)
        (low r.(1))
        (if ty = Flow.Ty.Array Flow.Ty.Int then low r.(2) else float_of r.(2))
  | _ -> fail "Not a workgroup intrinsic."

let rec kernel ?fast ?instanced ?motion ?curves ?spheres program =
  generate ?fast ?instanced ?motion ?curves ?spheres program

and stage_body ~raster program =
  Result.map (fun msl -> msl.source) (generate ~fast:true ~raster program)

and generate ?(fast = false) ?(instanced = false) ?(motion = false) ?curves ?(spheres = true)
    ?raster program =
  try
    (match P.gpu_refusals ~fused:true ~fast program with
    | [] -> ()
    | reason :: _ -> raise (Diagnostic.Error reason));
    let program, back =
      if raster <> None then (program, Fun.id)
      else
        match Noise.helpers ~instanced ~motion ~curves ~spheres program with
        | Ok x -> x
        | Error d -> raise (Diagnostic.Error d)
    in
    let layout =
      Noise.checked back
        (Flow_ir.Kernel_layout.plan ~fast ~instanced ~motion ~curves:(curves <> None) program)
    in
    let native = curves = Some `Native && layout.scene_strands <> None in
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
      uniform_layout;
      uniform_bytes;
      gather_base;
      uniform_binding;
      status_binding;
      table_binding;
      count_base;
      counts;
      scene_binding;
      scene_spheres;
      scene_instances;
    } =
      layout
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
    let integer_register slot = Flow_ir.Core.Region.integer_register view.code.(slot) in
    (* The kernel prints twice. The first pass counts each register's references and where they are;
     the second prints a register read once, in its own scope inside the blocks it is computed in, as
     an expression inside its reader instead of a named local (naga's baking rule), and declares a
     guarded register in its block when every read is inside that block. *)
    let size = Array.length view.code in
    let references = Array.make size 0
    and referenced = Array.make size (-1, [])
    and defined = Array.make size (-1, []) in
    let inlined = Array.make size None and counting = ref true and context = ref (0, []) in
    let blocks = ref []
    and defined_in = Array.make size []
    and read_in = Array.make size []
    and local = Array.make size true in
    let named = Array.make size false in
    Array.iter (Array.iter (fun slot -> named.(slot) <- true)) view.outputs;
    Array.iteri
      (fun slot instruction ->
        if used.(slot) then
          List.iter (fun (c, _) -> named.(c) <- true) (fst (guarded [] instruction)))
      view.code;
    let rec prefix = function
      | [], _ -> true
      | x :: a, y :: b -> x = y && prefix (a, b)
      | _ -> false
    in
    let register r =
      match inlined.(r) with
      | Some text -> "(" ^ text ^ ")"
      | None ->
          if !counting then begin
            references.(r) <- references.(r) + 1;
            referenced.(r) <- !context;
            read_in.(r) <- !blocks;
            if not (prefix (defined_in.(r), !blocks)) then local.(r) <- false
          end;
          register r
    in
    (* a raster stage reads an interpolated input where it is used: an Apple GPU interpolates on read, so a
     glyph fragment never pays for a rectangle's varyings *)
    let inline slot instruction =
      ((not !counting) && raster <> None
      && (not named.(slot))
      && match instruction with P.Input _ -> true | _ -> false)
      || (not !counting)
         && references.(slot) = 1
         && (not named.(slot))
         && (not finite_checks.(slot))
         && (match instruction with
           | P.Trace _
           | Int_binary ((Idiv | Imod | Ishl | Ishr), _, _)
           | Stage
               ( ( Trace_triangles _ | Trace_any | Trace_curves _ | Trace_curves_any | Group_scan _
                 | Store ),
                 _ ) ->
               false
           | _ -> true)
         && fst defined.(slot) = fst referenced.(slot)
         && prefix (defined_in.(slot), read_in.(slot))
    in
    let narrow = layout.narrow in
    let int_of r =
      if integer_register r && not narrow.(r) then register r else "long(" ^ register r ^ ")"
    in
    (* a register's low 32 bits as a uint *)
    let low r = if narrow.(r) then register r else "uint(" ^ int_of r ^ ")" in
    (* the integer register a float operand converts, if it is one *)
    (* an integer-valued register as a 64-bit expression: an integer register, its conversion to float,
     an integral constant, or +, -, *, mod, min, max of two such; [None] for any other *)
    let rec whole r =
      if integer_register r then Some (int_of r)
      else
        match snd (guarded [] view.code.(r)) with
        | P.Convert (To_float, x) when integer_register x -> Some (int_of x)
        | P.Const c when Float.is_integer c && Float.abs c < 9007199254740992. ->
            Some (Printf.sprintf "%.0fL" c)
        | P.Binary (((Add | Sub | Mul | Mod | Min | Max) as op), a, b) when fast -> (
            match (whole a, whole b) with
            | Some a, Some b ->
                Some
                  ("("
                  ^ (match op with
                    | P.Add -> a ^ "+" ^ b
                    | Sub -> a ^ "-" ^ b
                    | Mul -> a ^ "*" ^ b
                    | Mod -> b ^ "==0?0:(" ^ a ^ "%" ^ b ^ "+" ^ b ^ ")%" ^ b
                    | Min -> "min(" ^ a ^ "," ^ b ^ ")"
                    | _ -> "max(" ^ a ^ "," ^ b ^ ")")
                  ^ ")")
            | _ -> None)
        | _ -> None
    in
    let float_of r = if integer_register r then "float(" ^ register r ^ ")" else register r in
    let element prepared index width component =
      let k = gather_index prepared
      and index = if narrow.(index) then register index else int_of index in
      match raster with
      | Some r ->
          Printf.sprintf "%s[%s*%d+%d]"
            (r.buffer (fst (snd (List.nth gathers k))))
            index width component
      | None ->
          Printf.sprintf
            "as_type<float>(gather%d[clamp(int(%s),0,max(int(uniforms[%d])-1,0))*%d+%d])" k index
            gather_lengths.(k) width component
    in
    let hits = Hashtbl.create 4 in
    let instancing = scene_instances <> None and moving = layout.scene_motion in
    let query current guards slot stage r =
      trace_query ~hits ~instancing ~moving current guards slot stage r (fun k -> float_of r.(k))
    in
    let scene_read stage r =
      scene_read ~instancing ~stride:(if moving then 32 else 16) stage (fun k -> low r.(k))
    in
    let scans = Hashtbl.create 1 in
    let group raster current guards stage r =
      group_intrinsic ~scans ~raster current guards stage r
        ~table:(fun x ->
          match snd (guarded [] view.code.(x)) with
          | P.Array_prepare (u, ty) -> (u, ty)
          | _ -> fail "A store names a bound buffer.")
        ~low ~float_of
    in
    let scene_count name =
      Printf.sprintf "uniforms[%d]"
        (Option.get
           (if name = "gpu/scene-spheres" then scene_spheres else layout.scene_strand_count))
    in
    let outputs = Array.length view.outputs in
    let output_name k = if outputs = 1 then "output" else "output" ^ string_of_int k in
    let print () =
      Hashtbl.reset hits;
      Hashtbl.reset scans;
      let root = scope ~next:(ref 0) 0 in
      let scopes = ref [ root ] in
      Flow_ir.Kernel_layout.iter layout
        ~instruction:(fun slot instruction ->
          if
            used.(slot)
            && match snd (guarded [] instruction) with P.Array_prepare _ -> false | _ -> true
          then begin
            let current = List.hd !scopes in
            let guards, instruction = guarded [] instruction in
            context := (current.Scope.id, guards);
            if !counting then defined.(slot) <- !context;
            let inline = inline slot instruction in
            let ty, zero =
              if narrow.(slot) then ("uint", "0u")
              else if integer_register slot then ("long", "0")
              else ("float", "0.0f")
            in
            let constant =
              match instruction with
              | P.Const _ | Int_const _ -> not finite_checks.(slot)
              | _ -> false
            in
            (* a guarded constant is declared with its value: consumers fold it, and an unselected value is never read *)
            if not (constant || inline) then enter current guards;
            blocks := if constant then current.Scope.outer else Scope.stack current;
            if !counting then defined_in.(slot) <- !blocks;
            let body = current.Scope.code and indentation = indent current in
            let expression =
              if narrow.(slot) then
                match instruction with
                | P.Int_const n -> Printf.sprintf "%du" (n land 0xffffffff)
                | Int_binary (op, a, b) ->
                    narrow_integer op (low a)
                      (if (op = Ishl || op = Ishr) && not narrow.(b) then int_of b else low b)
                | Int_select (_, yes, no) when yes = no -> low yes
                | Int_select (condition, yes, no) ->
                    Printf.sprintf "(%s!=0.0f?%s:%s)" (float_of condition) (low yes) (low no)
                | Loop_result (loop, component, _) -> register (loop + 1 + component)
                | Stage (Noise_table seed, r) ->
                    Printf.sprintf "uint(table[%d+%s])" (table seed) (low r.(0))
                | Stage (stage, r) -> (
                    match raster with
                    | Some x -> group x current guards stage r
                    | None -> query current guards slot stage r)
                | Int_frame name -> (
                    match raster with Some x -> x.builtin name | None -> scene_count name)
                | _ -> assert false
              else
                match instruction with
                | P.Const value ->
                    Printf.sprintf "as_type<float>(0x%lxu)" (Int32.bits_of_float value)
                | Input (input, _, component) when raster <> None ->
                    (Option.get raster).input input component
                | Input (input, width, component) ->
                    Printf.sprintf "input%d[i*%d+%d]" input width component
                | Uniform (index, component) ->
                    Printf.sprintf "as_type<float>(uniforms[%d])" (uniforms.(index) + component)
                | Frame name when raster <> None -> (Option.get raster).builtin name
                | Frame _ -> Printf.sprintf "as_type<float>(uniforms[%d])" frames.(slot)
                | Accumulator _ -> assert false
                (* arithmetic on two integers stays in 64 bits (fast): a float register would round them first *)
                | Binary ((Add | Sub | Mul | Mod | Min | Max), _, _) when fast && whole slot <> None
                  ->
                    "float(" ^ Option.get (whole slot) ^ ")"
                | Convert ((Truncate | Coerce_int | Integer_result _ | Floor | Ceil | Round), a)
                  when fast && whole a <> None ->
                    Option.get (whole a)
                | Binary (op, a, b) -> binary op (float_of a) (float_of b)
                | Unary (op, a) -> unary op (float_of a)
                | Fma (a, b, c) ->
                    Printf.sprintf "fma(%s,%s,%s)" (register a) (register b) (register c)
                | Checked_mul (projection, a, b) ->
                    checked projection (Printf.sprintf "(%s*%s)" (register a) (register b))
                | Checked_fma (projection, a, b, c) ->
                    checked projection
                      (Printf.sprintf "fma(%s,%s,%s)" (register a) (register b) (register c))
                | Gather (prepared, index, width, component) ->
                    element prepared index width component
                | Int_gather (prepared, index) -> "long(" ^ element prepared index 1 0 ^ ")"
                | Array_prepare _ -> assert false
                | Int_const n -> Printf.sprintf "long(%dL)" n
                | Int_input input when raster <> None -> (Option.get raster).int_input input
                | Int_input input -> Printf.sprintf "long(input%d[i])" input
                | Int_uniform index ->
                    Printf.sprintf "long(as_type<float>(uniforms[%d]))" uniforms.(index)
                | Int_frame name when raster <> None ->
                    "long(" ^ (Option.get raster).builtin name ^ ")"
                | Int_frame name when Flow_ir.Kernel_layout.scene_fact name ->
                    "long(" ^ scene_count name ^ ")"
                | Int_frame _ -> Printf.sprintf "long(as_type<float>(uniforms[%d]))" frames.(slot)
                | Int_binary (op, a, b) -> integer op (int_of a) (int_of b)
                | Int_compare (op, a, b) -> comparison op (int_of a) (int_of b)
                | Int_abs a -> wrap_integer ("abs(" ^ int_of a ^ ")")
                | Int_select (_, yes, no) when yes = no -> int_of yes
                | Int_select (condition, yes, no) ->
                    Printf.sprintf "(%s!=0.0f?%s:%s)" (float_of condition) (int_of yes) (int_of no)
                (* a rounded float as a float is rounded in float arithmetic, never through 64 bits (one value
         wherever the 64-bit integer holds it) *)
                | Convert (To_float, a)
                  when match snd (guarded [] view.code.(a)) with
                       | P.Convert ((Floor | Ceil | Round | Truncate), x) ->
                           not (integer_register x)
                       | _ -> false -> (
                    match snd (guarded [] view.code.(a)) with
                    | P.Convert (op, x) ->
                        (match op with
                          | P.Floor -> "floor("
                          | Ceil -> "ceil("
                          | Round -> "round("
                          | _ -> "trunc(")
                        ^ float_of x ^ ")"
                    | _ -> assert false)
                | Convert (To_float, a) -> float_of a
                | Convert ((Truncate | Coerce_int | Integer_result _), a) -> int_of a
                | Convert (Floor, a) -> "long(floor(" ^ float_of a ^ "))"
                | Convert (Ceil, a) -> "long(ceil(" ^ float_of a ^ "))"
                | Convert (Round, a) -> "long(round(" ^ float_of a ^ "))"
                | Text_const _ | Text_input _ | Text_uniform _ | Text_gather _ | Text_select _ ->
                    fail "Text kernels stay on the CPU."
                | Select (_, yes, no) when yes = no -> register yes
                | Select (condition, yes, no) ->
                    Printf.sprintf "(%s!=0.0f?%s:%s)" (register condition) (register yes)
                      (register no)
                | Stage
                    ( ((Trace_triangles _ | Trace_any | Trace_curves _ | Trace_curves_any) as stage),
                      r )
                  when raster = None ->
                    let value = query current guards slot stage r in
                    if integer_register slot then "long(" ^ value ^ ")" else value
                | Stage (Noise_table seed, r) ->
                    Printf.sprintf "long(table[%d+%s])" (table seed) (low r.(0))
                | Stage (stage, r) when raster = None -> scene_read stage r
                | Stage (((Group_scan _ | Store) as stage), r) when raster <> None ->
                    "long(" ^ group (Option.get raster) current guards stage r ^ ")"
                | Stage (stage, r) when raster <> None ->
                    stage_intrinsic stage (Array.map float_of r)
                | Stage _ -> fail "A raster-stage intrinsic runs only in a fragment stage."
                | Loop_result (loop, component, _) -> register (loop + 1 + component)
                | _ ->
                    fail
                      "Integer instructions and conversions are not supported by the Metal kernel \
                       target."
            in
            if inline then inlined.(slot) <- Some expression
            else if constant then
              Printf.bprintf current.Scope.decls "  %s r%d=%s;\n" ty slot expression
            else if
              current.Scope.opened = [] || (local.(slot) && (not named.(slot)) && not !counting)
            then Printf.bprintf body "%s%s r%d=%s;\n" indentation ty slot expression
            else begin
              Printf.bprintf current.Scope.decls "  %s r%d=%s;\n" ty slot zero;
              Printf.bprintf body "%sr%d=%s;\n" indentation slot expression
            end;
            if finite_checks.(slot) && (not (integer_register slot)) && raster = None then
              Printf.bprintf body "%ssb|=!isfinite(r%d);\n" indentation slot;
            match instruction with
            | P.Int_binary (operation, _, b) ->
                Buffer.add_string body
                  (if narrow.(b) then integer_status ~unsigned:true indentation operation (low b)
                   else integer_status indentation operation (int_of b))
            | _ -> ()
          end)
        ~loop:(fun header control ~next ~stop ~body:emit_body ->
          if used.(header) then begin
            let outer = List.hd !scopes in
            let guards, _ = guarded [] view.code.(header) in
            context := (outer.Scope.id, guards);
            let ty slot =
              if narrow.(slot) then "uint" else if integer_register slot then "long" else "float"
            in
            let carried slot source =
              if narrow.(slot) then low source
              else if integer_register slot then int_of source
              else register source
            in
            (* a raster stage counts a capped pass from a small constant in 32 bits (64-bit counters cost a lit
         Scene3 frame about 7%) *)
            let counter =
              match (raster, control.limit, snd (guarded [] view.code.(control.first))) with
              | Some _, Some limit, P.Int_const v
                when (not narrow.(header)) && abs v + limit < 1 lsl 30 ->
                  "int"
              | _ -> ty header
            in
            Printf.bprintf outer.Scope.decls "  %s r%d=0;\n" counter header;
            Array.iteri
              (fun component _ ->
                let slot = header + 1 + component in
                Printf.bprintf outer.Scope.decls "  %s r%d=0;\n" (ty slot) slot)
              control.initial;
            enter outer guards;
            blocks := Scope.stack outer;
            let body = outer.Scope.code and indentation = indent outer in
            (* a pass count over the limit flags the status and runs no pass *)
            Option.iter
              (fun limit ->
                Printf.bprintf body
                  "%sbool over%d=%s>%s && ulong(%s-%s)>%duL;\n%ssb|=over%d?1u:0u;\n" indentation
                  header (int_of control.last) (int_of control.first) (int_of control.last)
                  (int_of control.first) limit indentation header)
              control.limit;
            Array.iteri
              (fun component initial ->
                let slot = header + 1 + component in
                Printf.bprintf body "%sr%d=%s;\n" indentation slot (carried slot initial))
              control.initial;
            let bound r = if narrow.(header) then low r else int_of r in
            let last =
              if control.limit = None then bound control.last
              else Printf.sprintf "(over%d?%s:%s)" header (bound control.first) (bound control.last)
            in
            Printf.bprintf body "%sfor(r%d=%s;r%d<%s;r%d++){\n" indentation header
              (bound control.first) header last header;
            let inner = scope ~outer:(Scope.stack outer) ~next:outer.Scope.next header in
            scopes := inner :: !scopes;
            emit_body ();
            enter inner [];
            context := (inner.Scope.id, []);
            blocks := Scope.stack inner;
            scopes := List.tl !scopes;
            Buffer.add_string body (declarations inner.Scope.decls);
            Buffer.add_buffer body inner.Scope.code;
            Option.iter
              (fun source ->
                Printf.bprintf body "  bool stop%d=%s!=0.0f;\n" header (float_of source))
              stop;
            Array.iteri
              (fun component source ->
                let slot = header + 1 + component in
                Printf.bprintf body "  %s next%d_%d=%s;\n" (ty slot) header component
                  (carried slot source))
              next;
            Array.iteri
              (fun component _ ->
                Printf.bprintf body "  r%d=next%d_%d;\n" (header + 1 + component) header component)
              next;
            Option.iter (fun _ -> Printf.bprintf body "  if(stop%d)break;\n" header) stop;
            Printf.bprintf body "%s}\n" indentation
          end);
      enter root [];
      context := (0, []);
      blocks := [];
      let body = Buffer.create 4096 in
      Buffer.add_string body (declarations root.Scope.decls);
      Buffer.add_buffer body root.Scope.code;
      (match raster with
      | Some r ->
          Array.iteri
            (fun k registers ->
              Array.iteri
                (fun component slot ->
                  Printf.bprintf body "  %s\n" (r.output k component (float_of slot)))
                registers)
            view.outputs
      | None ->
          if layout.output_checks then
            Array.iter
              (Array.iter (fun slot ->
                   if not (integer_register slot) then
                     Printf.bprintf body "  sb|=!isfinite(r%d);\n" slot))
              view.outputs;
          Printf.bprintf body "  %s\n" flush_status;
          Array.iteri
            (fun k registers ->
              Array.iteri
                (fun component slot ->
                  Printf.bprintf body "  %s[i*%d+%d]=%s;\n" (output_name k) (Array.length registers)
                    component (float_of slot))
                registers)
            view.outputs);
      body
    in
    ignore (print ());
    counting := false;
    let body = print () in
    if raster <> None then
      Ok
        {
          source = "  uint sb=0u;\n" ^ Buffer.contents body;
          entry = "";
          interface = [];
          uniform_layout = [];
          uniform_bytes = 0;
          table_seeds = [||];
          output_widths = [||];
          input_widths = [||];
          gathers = [||];
          counts = None;
          scene = None;
        }
    else
      let count = Array.length view.widths in
      (* the scene's structures and the tables the trace helpers read (Metal reflects only those) *)
      let reads name =
        let text = Buffer.contents body and n = String.length name in
        let rec at k = k + n <= String.length text && (String.sub text k n = name || at (k + 1)) in
        at 0
      in
      let scene_tables =
        match scene_binding with
        | None -> []
        | Some b ->
            List.filter_map
              (fun (accel, declaration, binding, name) ->
                if reads name then Some (accel, declaration, binding) else None)
              ([
                 ( true,
                   structure_type (scene_instances <> None) layout.scene_motion ^ " scene",
                   b,
                   "" );
                 (false, "device const packed_float3* scene_vertices", b + 1, "scene_vertices[");
                 (false, "device const float4* scene_spheres", b + 2, "scene_spheres[");
               ]
              @ (if scene_instances = None then []
                 else [ (false, "device const float* scene_instances", b + 3, "scene_instances[") ])
              @ (if layout.scene_vertices_end = None then []
                 else
                   [
                     ( false,
                       "device const packed_float3* scene_vertices_end",
                       b + 3,
                       "scene_vertices_end[" );
                   ])
              @
              match layout.scene_strands with
              | None -> []
              | Some s ->
                  (false, "device const float4* scene_strands", s, "scene_strands[")
                  ::
                  (if native then
                     [
                       (true, "primitive_acceleration_structure scene_curves", s + 1, "scene_curves");
                     ]
                   else []))
      in
      (* fast: the element count may come from a device cell (another kernel's, times mul over div,
     when use is set) and is written to this kernel's own count record {n, 6n, 1, 0, 0, 0}: a
     resident fold never reads it, and its last five words are the indexed indirect draw of n
     quads a mark set reads *)
      let arguments =
        Array.to_list
          (Array.mapi
             (fun index _ ->
               Printf.sprintf "device const float* input%d [[buffer(%d)]]" index index)
             view.widths)
        @ List.init outputs (fun k ->
            Printf.sprintf "device float* %s [[buffer(%d)]]" (output_name k) (count + k))
        @ [
            Printf.sprintf "constant uint* uniforms [[buffer(%d)]]" uniform_binding;
            Printf.sprintf "device atomic_uint* status [[buffer(%d)]]" status_binding;
          ]
        @ (match table_binding with
          | None -> []
          | Some binding -> [ Printf.sprintf "device const int* table [[buffer(%d)]]" binding ])
        @ List.mapi
            (fun k _ ->
              Printf.sprintf "device const uint* gather%d [[buffer(%d)]]" k (gather_base + k))
            gathers
        @ (if counts = None then []
           else
             [
               Printf.sprintf "device const uint* count_in [[buffer(%d)]]" count_base;
               Printf.sprintf "device uint* count_out [[buffer(%d)]]" (count_base + 1);
             ])
        @ List.map
            (fun (_, declaration, binding) ->
              Printf.sprintf "%s [[buffer(%d)]]" declaration binding)
            scene_tables
        @ [ "uint i [[thread_position_in_grid]]" ]
      in
      let prefix =
        "#include <metal_stdlib>\nusing namespace metal;\n"
        ^
        if scene_binding = None then ""
        else "#include <metal_raytracing>\nusing namespace metal::raytracing;\n"
      in
      let signature = String.concat ",\n  " arguments in
      let guard =
        match counts with
        | None -> "  if(i>=uniforms[0]) return;\n"
        | Some o ->
            let w = o / 4 in
            Printf.sprintf
              "  uint n=uniforms[0];\n\
              \  if(uniforms[%d]!=0u) n=min(n,count_in[0]*uniforms[%d]/uniforms[%d]);\n\
              \  \
               if(i==0){count_out[0]=n;count_out[1]=6u*n;count_out[2]=1u;count_out[3]=0u;count_out[4]=0u;count_out[5]=0u;}\n\
              \  if(i>=n) return;\n"
              (w + 2) w (w + 1)
      in
      let body = guard ^ "  uint sb=0u;\n" ^ Buffer.contents body in
      let digest = Digest.to_hex (Digest.string (prefix ^ signature ^ body)) in
      let entry = "kernel_" ^ digest in
      let source = prefix ^ "kernel void " ^ entry ^ "(\n  " ^ signature ^ ") {\n" ^ body ^ "}\n" in
      let interface =
        List.init (count_base + if counts = None then 0 else 2) Fun.id
        |> List.filter (fun binding -> binding >= count || input_used.(binding))
        |> List.map (fun binding ->
            S.
              {
                group = 0;
                binding;
                kind = (if binding = uniform_binding then Uniform_buffer else Storage_buffer);
                visibility = [ Compute ];
              })
      in
      let interface =
        interface
        @ List.map
            (fun (accel, _, binding) ->
              S.
                {
                  group = 0;
                  binding;
                  kind = (if accel then Acceleration_structure else Storage_buffer);
                  visibility = [ Compute ];
                })
            scene_tables
      in
      let uniform_layout = Trace.frames back uniform_layout in
      Ok
        {
          source;
          entry;
          interface;
          uniform_layout;
          uniform_bytes;
          scene = scene_binding;
          table_seeds;
          output_widths = Array.map Array.length view.outputs;
          input_widths = view.widths;
          gathers = Array.of_list (List.map snd gathers);
          counts;
        }
  with Diagnostic.Error diagnostic -> Error diagnostic

(* Stream compaction for a resident fold: the indices of a mask's nonzero elements, in order, as
   floats, and their count; three dispatches over blocks of 1,024 (count, scan the counts, scatter).
   The kernel is shaders/compact.rays, a compute stage printed by Raster.compute_msl
   (lib/flow_gpu/test_raster.exe --write lib). *)
let compaction =
  {
    source = Compact_source.source;
    entry = "compact_mask";
    uniform_layout = [];
    uniform_bytes = 12;
    table_seeds = [||];
    output_widths = [| 1 |];
    input_widths = [| 1 |];
    gathers = [||];
    counts = None;
    scene = None;
    interface =
      List.map
        (fun binding ->
          S.
            {
              group = 0;
              binding;
              kind = (if binding = 3 then Uniform_buffer else Storage_buffer);
              visibility = [ Compute ];
            })
        [ 0; 1; 2; 3; 4; 5 ];
  }

(* A resident dispatch's threadgroup counts from a count cell, one thread (shaders/dispatch.rays,
   printed as [compaction] is). Buffers: the cell, {capacity, mul, div, group}, the three counts. *)
let dispatch_arguments =
  {
    compaction with
    source = Dispatch_source.source;
    entry = "dispatch_arguments";
    uniform_bytes = 16;
    interface =
      List.map
        (fun binding ->
          S.
            {
              group = 0;
              binding;
              kind = (if binding = 1 then Uniform_buffer else Storage_buffer);
              visibility = [ Compute ];
            })
        [ 0; 1; 2 ];
  }

(* A resident packed float3 array's bounds: each lane of one threadgroup of 1,024 the min and max of
   every 1,024th element from it, at lane x 6, finished on the host (shaders/bounds.rays, printed as [compaction] is).
   Buffers: the array, the lanes' floats, the count. *)
let bounds =
  {
    compaction with
    source = Bounds_source.source;
    entry = "chunk_bounds";
    uniform_bytes = 4;
    output_widths = [| 6 |];
    input_widths = [| 3 |];
    interface =
      List.map
        (fun binding ->
          S.
            {
              group = 0;
              binding;
              kind = (if binding = 2 then Uniform_buffer else Storage_buffer);
              visibility = [ Compute ];
            })
        [ 0; 1; 2 ];
  }

(* A triangle mesh's display normals from its resident P (shaders/normals.rays, one thread a point
   over groups of 256). Buffers: P, the corners, the CSR offsets and corners of each point, the
   normals, the count. *)
let normals =
  {
    compaction with
    source = Normals_source.source;
    entry = "point_normal";
    uniform_bytes = 4;
    output_widths = [| 3 |];
    input_widths = [| 3 |];
    interface =
      List.map
        (fun binding ->
          S.
            {
              group = 0;
              binding;
              kind = (if binding = 5 then Uniform_buffer else Storage_buffer);
              visibility = [ Compute ];
            })
        [ 0; 1; 2; 3; 4; 5 ];
  }
