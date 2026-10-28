module P = Flow_ir.Packed
module S = Ogpu.Shader
type msl = {source : string; entry : string; interface : S.binding list;
  uniform_layout : (string * int * int) list; uniform_bytes : int;
  table_seeds : int array; output_widths : int array; input_widths : int array;
  gathers : (int * int) array; counts : int option; scene : int option}
let names = Flow.Packed_ops.names
let perlin = {|float fade(float t) { return t*t*t*(t*(t*6.0f-15.0f)+10.0f); }
float blend(float a, float b, float t) { return a+t*(b-a); }
float gradient(int h, float x, float y, float z) {
  h &= 15;
  float u=h<8?x:y, v=h<4?y:((h==12||h==14)?x:z);
  return ((h&1)==0?u:-u)+((h&2)==0?v:-v);
}
float noise(float3 p, device const int* table) {
  if(!all(isfinite(p))) return as_type<float>(0x7fc00000u);
  float3 base=floor(p), q=p-base, f=float3(fade(q.x),fade(q.y),fade(q.z));
  int x=int(fmod(base.x,256.0f))&255, y=int(fmod(base.y,256.0f))&255, z=int(fmod(base.z,256.0f))&255;
  int a=table[x]+y, b=table[x+1]+y;
  int aa=table[a]+z, ab=table[a+1]+z, ba=table[b]+z, bb=table[b+1]+z;
  float x00=blend(gradient(table[aa],q.x,q.y,q.z),gradient(table[ba],q.x-1,q.y,q.z),f.x);
  float x10=blend(gradient(table[ab],q.x,q.y-1,q.z),gradient(table[bb],q.x-1,q.y-1,q.z),f.x);
  float x01=blend(gradient(table[aa+1],q.x,q.y,q.z-1),gradient(table[ba+1],q.x-1,q.y,q.z-1),f.x);
  float x11=blend(gradient(table[ab+1],q.x,q.y-1,q.z-1),gradient(table[bb+1],q.x-1,q.y-1,q.z-1),f.x);
  return clamp((blend(blend(x00,x10,f.y),blend(x01,x11,f.y),f.z)+1.0f)*0.5f,0.0f,1.0f);
}
float fbm(float3 p, int octaves, device const int* table) {
  float frequency=1.0f, amplitude=1.0f, sum=0.0f, weights=0.0f;
  for(int octave=0;octave<octaves;octave++) {
    sum+=noise(p*frequency,table)*amplitude; weights+=amplitude;
    frequency*=2.0f; amplitude*=0.5f;
  }
  return weights==0.0f?0.0f:sum/weights;
}
|}
(* The trace helpers, specialised by two static scene facts: [instanced] (an instance structure and its table)
   and [motion] (the query's time: instance transforms, or vertex keyframes in a flat scene). The static text
   is unchanged by the motion insertions, which print only for a moving scene. *)
let structure_type instanced motion = match instanced, motion with
  | true, false -> "instance_acceleration_structure" | false, false -> "primitive_acceleration_structure"
  | true, true -> "acceleration_structure<instancing, instance_motion>"
  | false, true -> "acceleration_structure<primitive_motion>"
(* Strands (linear round curves, pathtrace.metal's): segment k is two float4s (end a, radius; end b, radius). A
   hit reports prim = k, u = the curve parameter, v = 0, instance = -2 and the reference's normal: the offset from
   the axis point at u, less its tangential part. Without native curves the segment is a capsule (the cylinder
   within it, the end spheres beyond it) tested in-loop like the spheres.
   ponytail: the loop is linear in segments; an AABB structure with a candidate loop when strand counts grow. *)
let strand_functions = {|void strand_hit(thread TraceHit& h, float3 o, float3 dn, float t, uint k, float u, device const float4* strands) {
  float3 a = strands[2*k].xyz, b = strands[2*k+1].xyz, tangent = b - a;
  float3 off = o + t*dn - mix(a, b, u);
  off -= tangent * (dot(off, tangent) / max(dot(tangent, tangent), 1e-8f));
  h.t = t; h.prim = float(k); h.u = u; h.v = 0.0f; h.n = normalize(off); h.hit = 1.0f; h.inst = -2.0f;
}
float strand_t(float3 o, float3 dn, float tmin, float tmax, float4 sa, float4 sb, thread float& u) {
  // roots from the ray's point nearest the segment's middle, s along it: float32 keeps them precise
  float shift = dot(0.5f*(sa.xyz + sb.xyz) - o, dn);
  o += shift*dn; tmin -= shift; tmax -= shift;
  float3 ba = sb.xyz - sa.xyz, oa = o - sa.xyz;
  float r = sa.w, baba = dot(ba, ba), bard = dot(ba, dn), baoa = dot(ba, oa), best = INFINITY;
  float A = baba - bard*bard, B = baba*dot(dn, oa) - baoa*bard, C = baba*dot(oa, oa) - baoa*baoa - r*r*baba;
  float disc = B*B - A*C;
  if (A > 0.0f && disc >= 0.0f) {
    float root = sqrt(disc);
    for (int s = -1; s <= 1; s += 2) {
      float t = (-B + float(s)*root) / A, y = baoa + t*bard;
      if (y > 0.0f && y < baba && t >= tmin && t <= tmax && t < best) best = t;
    }
  }
  for (int end = 0; end < 2; end++) {
    float3 oc = end == 0 ? oa : o - sb.xyz;
    float b = dot(oc, dn), cap = b*b - (dot(oc, oc) - r*r);
    if (cap >= 0.0f) {
      for (int s = -1; s <= 1; s += 2) {
        float t = -b + float(s)*sqrt(cap), y = baoa + t*bard;
        if ((end == 0 ? y <= 0.0f : y >= baba) && t >= tmin && t <= tmax && t < best) best = t;
      }
    }
  }
  u = clamp((baoa + best*bard) / baba, 0.0f, 1.0f);
  return best + shift;
}
|}
let native_strands = {|  intersector<curve_data> strand_query;
  strand_query.assume_geometry_type(geometry_type::curve);
  strand_query.force_opacity(forced_opacity::opaque);
  strand_query.assume_curve_type(curve_type::round);
  strand_query.assume_curve_basis(curve_basis::linear);
  strand_query.assume_curve_control_point_count(2);
  |}
let trace_prelude ?curves instanced motion =
  let pick yes no = if instanced then yes else no and moving text = if motion then text else "" in
  let stranded text = if curves = None then "" else text and native = curves = Some `Native in
  let structure = structure_type instanced motion in
  let parameters = structure ^ {| scene, device const packed_float3* vertices,
    |} ^ moving (pick "" "device const packed_float3* vertices_end, ") ^ {|device const float4* spheres, uint sphere_count, |}
    ^ stranded ("device const float4* strands, uint strand_count, " ^ (if native then "primitive_acceleration_structure curves, " else ""))
    ^ pick "device const float* instances, " "" ^ {|float3 o, float3 d, float tmin, float tmax|} ^ moving ", float time" in
  let tags = moving (pick ", instance_motion" ", primitive_motion") in
  let target = "scene" ^ moving ", time" in
  let vertices = if motion && not instanced then
      {|float s = clamp(time, 0.0f, 1.0f);
    float3 a = mix(float3(vertices[base]), float3(vertices_end[base]), s), b = mix(float3(vertices[base+1]), float3(vertices_end[base+1]), s),
      c = mix(float3(vertices[base+2]), float3(vertices_end[base+2]), s);|}
    else "float3 a = vertices[base], b = vertices[base+1], c = vertices[base+2];" in
  (* instance motion: the table holds both object-to-world keyframes; the normal is the inverse transpose of
     their interpolation, the cofactor columns signed by the determinant *)
  let instance_normal = if motion then
      {|float s = clamp(time, 0.0f, 1.0f);
    float3 c0 = mix(float3(m[1], m[5], m[9]), float3(m[13], m[17], m[21]), s), c1 = mix(float3(m[2], m[6], m[10]), float3(m[14], m[18], m[22]), s),
      c2 = mix(float3(m[3], m[7], m[11]), float3(m[15], m[19], m[23]), s);
    float3 w = h.n.x*cross(c1, c2) + h.n.y*cross(c2, c0) + h.n.z*cross(c0, c1);
    h.n = normalize(dot(c0, cross(c1, c2)) < 0.0f ? -w : w); h.inst = float(k);|}
    else "h.n = normalize(float3(m[1]*h.n.x + m[5]*h.n.y + m[9]*h.n.z, m[2]*h.n.x + m[6]*h.n.y + m[10]*h.n.z, m[3]*h.n.x + m[7]*h.n.y + m[11]*h.n.z)); h.inst = float(k);" in
  let strands = stranded (if native then native_strands ^ {|intersection_result<curve_data> sh = strand_query.intersect(ray(o, dn, tmin, h.hit != 0.0f ? h.t : tmax), curves);
  if (sh.type == intersection_type::curve) strand_hit(h, o, dn, sh.distance, sh.primitive_id, sh.curve_parameter, strands);
|} else {|  for (uint k = 0; k < strand_count; k++) {
    float u, t = strand_t(o, dn, tmin, h.hit != 0.0f ? h.t : tmax, strands[2*k], strands[2*k+1], u);
    if (t <= tmax) strand_hit(h, o, dn, t, k, u, strands);
  }
|}) in
  let occluding = stranded (if native then native_strands ^ {|strand_query.accept_any_intersection(true);
  if (strand_query.intersect(ray(o, dn, tmin, tmax), curves).type == intersection_type::curve) return 1.0f;
|} else {|  for (uint k = 0; k < strand_count; k++) {
    float u;
    if (strand_t(o, dn, tmin, tmax, strands[2*k], strands[2*k+1], u) <= tmax) return 1.0f;
  }
|}) in
  {|#include <metal_raytracing>
using namespace metal::raytracing;
struct TraceHit { float t; float prim; float u; float v; float3 n; float hit; float inst; };
|} ^ stranded strand_functions ^ {|TraceHit trace_scene(|} ^ parameters ^ {|) {
  intersector<triangle_data|} ^ pick ", instancing" "" ^ tags ^ {|> query;
  query.assume_geometry_type(geometry_type::triangle);
  query.force_opacity(forced_opacity::opaque);
  intersection_result<triangle_data|} ^ pick ", instancing" "" ^ tags ^ {|> hit = query.intersect(ray(o, normalize(d), tmin, tmax), |} ^ target ^ {|);
  TraceHit h = {0.0f, -1.0f, 0.0f, 0.0f, float3(0.0f), 0.0f, -1.0f};
  if (hit.type == intersection_type::triangle) {
    uint p = hit.primitive_id;
    |} ^ pick ("uint k = hit.instance_id; device const float* m = instances + k*" ^ (if motion then "32" else "16") ^ "; uint base = (uint(m[0]) + p)*3;") "uint base = p*3;" ^ {|
    |} ^ vertices ^ {|
    h.t = hit.distance; h.prim = float(p); h.u = hit.triangle_barycentric_coord.x; h.v = hit.triangle_barycentric_coord.y;
    h.n = normalize(cross(b - a, c - a)); h.hit = 1.0f;
    |} ^ pick instance_normal "h.inst = 0.0f;" ^ {|
  }
  float3 dn = normalize(d);
|} ^ strands ^ {|  for (uint k = 0; k < sphere_count; k++) {
    float4 s = spheres[k]; float3 oc = o - s.xyz;
    float b = dot(oc, dn); float3 q = oc - b*dn; float disc = s.w*s.w - dot(q, q);
    if (disc >= 0.0f) {
      float root = sqrt(disc), t = (-b - root >= tmin) ? -b - root : -b + root;
      if (t >= tmin && t <= tmax && (h.hit == 0.0f || t < h.t)) {
        h.t = t; h.prim = -2.0f - float(k); h.u = 0.0f; h.v = 0.0f; h.n = (oc + t*dn) / s.w; h.hit = 1.0f; h.inst = -1.0f;
      }
    }
  }
  return h;
}
float trace_occluded(|} ^ parameters ^ {|) {
  intersector<|} ^ pick "instancing" "" ^ (if instanced then tags else moving "primitive_motion") ^ {|> query;
  query.assume_geometry_type(geometry_type::triangle);
  query.force_opacity(forced_opacity::opaque);
  query.accept_any_intersection(true);
  if (query.intersect(ray(o, normalize(d), tmin, tmax), |} ^ target ^ {|).type == intersection_type::triangle) return 1.0f;
  float3 dn = normalize(d);
|} ^ occluding ^ {|  for (uint k = 0; k < sphere_count; k++) {
    float4 s = spheres[k]; float3 oc = o - s.xyz;
    float b = dot(oc, dn); float3 q = oc - b*dn; float disc = s.w*s.w - dot(q, q);
    if (disc >= 0.0f) {
      float root = sqrt(disc), t = (-b - root >= tmin) ? -b - root : -b + root;
      if (t >= tmin && t <= tmax) return 1.0f;
    }
  }
  return 0.0f;
}
|}
let fail message = raise (Diagnostic.Error (Diagnostic.error ~code:"E_GPU_FORM" message))
let register index = "r" ^ string_of_int index
let rec guarded guards = function
  | P.Guarded (more, body) -> guarded (guards @ more) body
  | instruction -> guards, instruction
let binary operation a b =
  let infix operator = "(" ^ a ^ operator ^ b ^ ")" in
  match operation with
  | P.Add -> infix "+" | Sub -> infix "-" | Mul -> infix "*"
  | Div -> "(" ^ b ^ "==0.0f?0.0f:" ^ a ^ "/" ^ b ^ ")"
  | Mod -> "(" ^ b ^ "==0.0f?0.0f:fmod(fmod(" ^ a ^ "," ^ b ^ ")+" ^ b ^ "," ^ b ^ "))"
  | Pow -> "pow(fabs(" ^ a ^ ")," ^ b ^ ")"
  | Min -> "min(" ^ a ^ "," ^ b ^ ")" | Max -> "max(" ^ a ^ "," ^ b ^ ")"
  | Lt -> infix "<" | Le -> infix "<=" | Gt -> infix ">" | Ge -> infix ">=" | Eq -> infix "=="
  | And -> "(" ^ a ^ "!=0.0f&&" ^ b ^ "!=0.0f)"
  | Or -> "(" ^ a ^ "!=0.0f||" ^ b ^ "!=0.0f)"
  | Atan2 -> "atan2(" ^ a ^ "," ^ b ^ ")"
  | Hypot -> fail "Native-safe hypot is not supported by the Metal kernel target."
let unary operation a = match operation with
  | P.Sin -> "sin(" ^ a ^ ")" | Cos -> "cos(" ^ a ^ ")"
  | Sqrt -> "sqrt(fabs(" ^ a ^ "))" | Abs -> "fabs(" ^ a ^ ")" | Not -> "(" ^ a ^ "==0.0f)"
  | Tan -> "tan(" ^ a ^ ")"
  | Asin | Acos -> "(fabs(" ^ a ^ ")>1.0f?as_type<float>(0x7fc00000u):"
      ^ (if operation=P.Asin then "asin" else "acos") ^ "(" ^ a ^ "))"
  | Exp -> "exp(" ^ a ^ ")"
  | Log -> "(" ^ a ^ "<=0.0f?as_type<float>(0x7fc00000u):log(" ^ a ^ "))"
  | Sign -> "(" ^ a ^ ">0.0f?1.0f:(" ^ a ^ "<0.0f?-1.0f:" ^ a ^ "))"
(* The 32-bit lowering of checked arithmetic: the value or zero, or the finite flag. *)
let checked projection expression = match projection with
  | P.Value -> Printf.sprintf "(isfinite(%s)?%s:0.0f)" expression expression
  | Valid -> Printf.sprintf "(isfinite(%s)?1.0f:0.0f)" expression
let wrap_integer value = "(as_type<long>(ulong(" ^ value ^ ")<<1)>>1)"
(* Integer registers occupy 64 bits; Int operations wrap in the signed 63-bit domain. *)
let integer_status ?(unsigned=false) indentation operation b = match operation with
  | Flow.Packed_ops.Idiv | Imod -> Printf.sprintf "%ssb|=(%s==0)?2u:0u;\n" indentation b
  | Ishl | Ishr when unsigned -> Printf.sprintf "%ssb|=(%s>62u)?4u:0u;\n" indentation b
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
    | (ty, _) :: _ as group -> Printf.bprintf out "  %s %s;\n" ty (String.concat "," (List.rev_map snd group)) in
  let split line = (* "  T item;" into (T, item) *)
    if not (String.starts_with ~prefix:"  " line && String.ends_with ~suffix:";" line) then None else
    let rest = String.sub line 2 (String.length line - 3) in
    match String.index_opt rest ' ' with
    | Some j when j > 0 -> Some (String.sub rest 0 j, String.sub rest (j + 1) (String.length rest - j - 1))
    | _ -> None in
  let group = List.fold_left (fun group line -> match split line, group with
    | Some (ty, item), (t, _) :: _ when t = ty && List.length group < 12 -> (ty, item) :: group
    | Some (ty, item), _ -> flush group; [ (ty, item) ]
    | None, _ -> flush group; if line <> "" then Buffer.add_string out (line ^ "\n"); [])
    [] (String.split_on_char '\n' (Buffer.contents decls)) in
  flush group;
  Buffer.contents out
let scope = Scope.create and indent = Scope.indent
let enter = Scope.enter ~opening:(fun condition selected ->
  Printf.sprintf "if(r%d%s0.0f) {" condition (if selected then "!=" else "=="))
let integer operation a b = wrap_integer (match operation with
  | Flow.Packed_ops.Iadd -> "(ulong(" ^ a ^ ")+ulong(" ^ b ^ "))"
  | Isub -> "(ulong(" ^ a ^ ")-ulong(" ^ b ^ "))"
  | Imul -> "(ulong(" ^ a ^ ")*ulong(" ^ b ^ "))"
  | Idiv -> "(" ^ b ^ "==0?0:" ^ a ^ "/" ^ b ^ ")" | Imod -> "(" ^ b ^ "==0?0:" ^ a ^ "%" ^ b ^ ")"
  | Iand -> "(" ^ a ^ "&" ^ b ^ ")" | Ior -> "(" ^ a ^ "|" ^ b ^ ")" | Ixor -> "(" ^ a ^ "^" ^ b ^ ")"
  | Ishl -> "(ulong(" ^ a ^ ")<<(" ^ b ^ "&63))"
  | Ishr -> "long((ulong(" ^ a ^ ")&0x7fffffffffffffffUL)>>(" ^ b ^ "&63))")
(* A narrow (32-bit unsigned) Int register's operation: the low 32 bits of the signed-63 result.
   Shifts by [b & 63] of 32 or more leave none of them (or, right, none of a fitting operand). *)
let narrow_integer operation a b =
  let shift operator = Printf.sprintf "((%s&63)>31?0u:(%s%suint(%s&63)))" b a operator b in
  match operation with
  | Flow.Packed_ops.Iadd -> "(" ^ a ^ "+" ^ b ^ ")" | Isub -> "(" ^ a ^ "-" ^ b ^ ")"
  | Imul -> "(" ^ a ^ "*" ^ b ^ ")" | Iand -> "(" ^ a ^ "&" ^ b ^ ")"
  | Ior -> "(" ^ a ^ "|" ^ b ^ ")" | Ixor -> "(" ^ a ^ "^" ^ b ^ ")"
  | Ishl -> shift "<<" | Ishr -> shift ">>"
  | Idiv -> "(" ^ b ^ "==0u?0u:" ^ a ^ "/" ^ b ^ ")" | Imod -> "(" ^ b ^ "==0u?0u:" ^ a ^ "%" ^ b ^ ")"
let comparison operation a b =
  let op = match operation with Flow.Packed_ops.Ilt -> "<" | Ile -> "<=" | Igt -> ">" | Ige -> ">=" | Ieq -> "==" in
  "(" ^ a ^ op ^ b ^ "?1.0f:0.0f)"
let kernel ?(fast=false) ?(instanced=false) ?(motion=false) ?curves program = try
  (match P.gpu_refusals ~fused:true ~fast program with [] -> () | reason :: _ -> raise (Diagnostic.Error reason));
  let layout = Flow_ir.Kernel_layout.plan ~fast ~instanced ~motion ~curves:(curves <> None) program in
  let native = curves = Some `Native && layout.scene_strands <> None in
  let {Flow_ir.Kernel_layout.view; used; finite_checks; uniforms; frames; table_seeds;
       gathers; gather_lengths; input_used; uniform_layout; uniform_bytes; gather_base;
       uniform_binding; status_binding; table_binding; count_base; counts; scene_binding; scene_spheres; scene_instances} = layout in
  let table seed = match Array.find_index ((=) seed) table_seeds with Some index -> index*512 | None -> assert false in
  let gather_index slot = match List.find_index (fun (s, _) -> s = slot) gathers with Some k -> k | None -> fail "Gather without its table." in
  let integer_register slot = Flow_ir.Core.Region.integer_register view.code.(slot) in
  (* The kernel prints twice. The first pass counts each register's references and where they are;
     the second prints a register read once, in its own scope inside the blocks it is computed in, as
     an expression inside its reader instead of a named local (naga's baking rule), and declares a
     guarded register in its block when every read is inside that block. *)
  let size = Array.length view.code in
  let references = Array.make size 0 and referenced = Array.make size (-1, []) and defined = Array.make size (-1, []) in
  let inlined = Array.make size None and counting = ref true and context = ref (0, []) in
  let blocks = ref [] and defined_in = Array.make size [] and read_in = Array.make size [] and local = Array.make size true in
  let named = Array.make size false in
  Array.iter (Array.iter (fun slot -> named.(slot) <- true)) view.outputs;
  Array.iteri (fun slot instruction -> if used.(slot) then List.iter (fun (c, _) -> named.(c) <- true) (fst (guarded [] instruction))) view.code;
  let rec prefix = function [], _ -> true | x :: a, y :: b -> x = y && prefix (a, b) | _ -> false in
  let register r = match inlined.(r) with
    | Some text -> "(" ^ text ^ ")"
    | None ->
        if !counting then begin
          references.(r) <- references.(r) + 1; referenced.(r) <- !context; read_in.(r) <- !blocks;
          if not (prefix (defined_in.(r), !blocks)) then local.(r) <- false
        end;
        register r in
  let inline slot instruction = not !counting && references.(slot) = 1 && not named.(slot) && not finite_checks.(slot)
    && (match instruction with P.Trace _ | Int_binary ((Idiv | Imod | Ishl | Ishr), _, _) -> false | _ -> true)
    && fst defined.(slot) = fst referenced.(slot) && prefix (defined_in.(slot), read_in.(slot)) in
  let narrow = layout.narrow in
  let int_of r = if integer_register r && not narrow.(r) then register r else "long(" ^ register r ^ ")" in
  (* a register's low 32 bits as a uint *)
  let low r = if narrow.(r) then register r else "uint(" ^ int_of r ^ ")" in
  (* the integer register a float operand converts, if it is one *)
  (* an integer-valued register as a 64-bit expression: an integer register, its conversion to float,
     an integral constant, or +, -, *, mod, min, max of two such; [None] for any other *)
  let rec whole r = if integer_register r then Some (int_of r) else match snd (guarded [] view.code.(r)) with
    | P.Convert (To_float, x) when integer_register x -> Some (int_of x)
    | P.Const c when Float.is_integer c && Float.abs c < 9007199254740992. -> Some (Printf.sprintf "%.0fL" c)
    | P.Binary ((Add|Sub|Mul|Mod|Min|Max) as op, a, b) when fast ->
        (match whole a, whole b with
         | Some a, Some b -> Some ("(" ^ (match op with P.Add -> a ^ "+" ^ b | Sub -> a ^ "-" ^ b | Mul -> a ^ "*" ^ b
             | Mod -> "fmod64(" ^ a ^ "," ^ b ^ ")" | Min -> "min(" ^ a ^ "," ^ b ^ ")" | _ -> "max(" ^ a ^ "," ^ b ^ ")") ^ ")")
         | _ -> None)
    | _ -> None in
  let float_of r = if integer_register r then "float(" ^ register r ^ ")" else register r in
  let element prepared index width component =
    let k = gather_index prepared in
    Printf.sprintf "as_type<float>(gather%d[clamp(int(%s),0,max(int(uniforms[%d])-1,0))*%d+%d])"
      k (if narrow.(index) then register index else int_of index) gather_lengths.(k) width component in
  let hits = Hashtbl.create 4 in
  (* a ray whose only read field is [hit] is an occlusion test: any intersection answers it *)
  let fields = Hashtbl.create 4 in
  Array.iteri (fun slot instruction -> match snd (guarded [] instruction) with
    | P.Trace (field, r) when used.(slot) -> Hashtbl.replace fields (Array.to_list r)
        (field = 7 && Option.value (Hashtbl.find_opt fields (Array.to_list r)) ~default:true)
    | _ -> ()) view.code;
  let outputs = Array.length view.outputs in
  let output_name k = if outputs = 1 then "output" else "output" ^ string_of_int k in
  let print () =
  Hashtbl.reset hits;
  let root = scope ~next:(ref 0) 0 in
  let scopes = ref [root] in
  Flow_ir.Kernel_layout.iter layout ~instruction:(fun slot instruction ->
    if used.(slot) && (match snd (guarded [] instruction) with P.Array_prepare _ -> false | _ -> true) then begin
    let current = List.hd !scopes in
    let guards,instruction=guarded [] instruction in
    context := (current.Scope.id, guards);
    if !counting then defined.(slot) <- !context;
    let inline = inline slot instruction in
    let ty, zero = if narrow.(slot) then "uint", "0u" else if integer_register slot then "long", "0" else "float", "0.0f" in
    let constant = match instruction with P.Const _ | Int_const _ -> not finite_checks.(slot) | _ -> false in
    (* a guarded constant is declared with its value: consumers fold it, and an unselected value is never read *)
    if not (constant || inline) then enter current guards;
    blocks := if constant then current.Scope.outer else Scope.stack current;
    if !counting then defined_in.(slot) <- !blocks;
    let body = current.Scope.code and indentation = indent current in
    let expression = if narrow.(slot) then match instruction with
      | P.Int_const n -> Printf.sprintf "%du" (n land 0xffffffff)
      | Int_binary (op,a,b) -> narrow_integer op (low a) (if (op = Ishl || op = Ishr) && not narrow.(b) then int_of b else low b)
      | Int_select (_,yes,no) when yes = no -> low yes
      | Int_select (condition,yes,no) -> Printf.sprintf "(%s!=0.0f?%s:%s)" (float_of condition) (low yes) (low no)
      | Loop_result (loop,component,_) -> register (loop+1+component)
      | _ -> assert false
    else match instruction with
      | P.Const value ->
          Printf.sprintf "as_type<float>(0x%lxu)" (Int32.bits_of_float value)
      | Input (input,width,component) -> Printf.sprintf "input%d[i*%d+%d]" input width component
      | Uniform (index,component) -> Printf.sprintf "as_type<float>(uniforms[%d])" (uniforms.(index)+component)
      | Frame _ -> Printf.sprintf "as_type<float>(uniforms[%d])" frames.(slot)
      | Accumulator _ -> assert false
      (* arithmetic on two integers stays in 64 bits (fast): a float register would round them first *)
      | Binary ((Add|Sub|Mul|Mod|Min|Max),_,_) when fast && whole slot <> None -> "float(" ^ Option.get (whole slot) ^ ")"
      | Convert ((Truncate|Coerce_int|Integer_result _|Floor|Ceil|Round),a) when fast && whole a <> None -> Option.get (whole a)
      | Binary (op,a,b) -> binary op (float_of a) (float_of b)
      | Unary (op,a) -> unary op (float_of a)
      | Fma (a,b,c) -> Printf.sprintf "fma(%s,%s,%s)" (register a) (register b) (register c)
      | Checked_mul (projection,a,b) -> checked projection (Printf.sprintf "(%s*%s)" (register a) (register b))
      | Checked_fma (projection,a,b,c) -> checked projection (Printf.sprintf "fma(%s,%s,%s)" (register a) (register b) (register c))
      | Gather (prepared,index,width,component) -> element prepared index width component
      | Int_gather (prepared,index) -> "long(" ^ element prepared index 1 0 ^ ")"
      | Array_prepare _ -> assert false
      | Int_const n -> Printf.sprintf "long(%dL)" n
      | Int_input input -> Printf.sprintf "long(input%d[i])" input
      | Int_uniform index -> Printf.sprintf "long(as_type<float>(uniforms[%d]))" uniforms.(index)
      | Int_frame _ -> Printf.sprintf "long(as_type<float>(uniforms[%d]))" frames.(slot)
      | Int_binary (op,a,b) -> integer op (int_of a) (int_of b)
      | Int_compare (op,a,b) -> comparison op (int_of a) (int_of b)
      | Int_abs a -> wrap_integer("abs(" ^ int_of a ^ ")")
      | Int_select (_,yes,no) when yes = no -> int_of yes
      | Int_select (condition,yes,no) -> Printf.sprintf "(%s!=0.0f?%s:%s)" (float_of condition) (int_of yes) (int_of no)
      | Convert (To_float,a) -> float_of a
      | Convert ((Truncate|Coerce_int|Integer_result _),a) -> int_of a
      | Convert (Floor,a) -> "long(floor(" ^ float_of a ^ "))"
      | Convert (Ceil,a) -> "long(ceil(" ^ float_of a ^ "))"
      | Convert (Round,a) -> "long(round(" ^ float_of a ^ "))"
      | Text_const _ | Text_input _ | Text_uniform _ | Text_gather _ | Text_select _ -> fail "Text kernels stay on the CPU."
      | Trace (field, r) ->
          let rays = Array.to_list r in
          let occlusion = Hashtbl.find fields rays and key = (current.Scope.id, guards, rays) in
          let hit = match Hashtbl.find_opt hits key with Some name -> name | None ->
            let name = Printf.sprintf "hit%d" slot and a k = float_of r.(k) in
            Printf.bprintf current.Scope.decls "  %s %s;\n" (if occlusion then "float" else "TraceHit") name;
            Printf.bprintf body "%s%s=%s(scene,scene_vertices,%sscene_spheres,uniforms[%d],%s%sfloat3(%s,%s,%s),float3(%s,%s,%s),%s,%s%s);\n"
              indentation name (if occlusion then "trace_occluded" else "trace_scene")
              (if layout.scene_vertices_end = None then "" else "scene_vertices_end,")
              (Option.get scene_spheres)
              (match layout.scene_strand_count with None -> "" | Some w ->
                 Printf.sprintf "scene_strands,uniforms[%d],%s" w (if native then "scene_curves," else ""))
              (if scene_instances = None then "" else "scene_instances,") (a 1) (a 2) (a 3) (a 4) (a 5) (a 6) (a 7) (a 8)
              (if layout.scene_motion then "," ^ a 9 else "");
            Hashtbl.add hits key name; name in
          if occlusion then hit else hit ^ [| ".t"; ".prim"; ".u"; ".v"; ".n.x"; ".n.y"; ".n.z"; ".hit"; ".inst" |].(field)
      | Select (_,yes,no) when yes = no -> register yes
      | Select (condition,yes,no) -> Printf.sprintf "(%s!=0.0f?%s:%s)" (register condition) (register yes) (register no)
      | Noise3 (x,y,z,seed,octaves,_,_) -> Printf.sprintf "fbm(float3(%s,%s,%s),%d,table+%d)"
          (register x) (register y) (register z) octaves (table seed)
      | Loop_result (loop,component,_) -> register (loop+1+component)
      | _ -> fail "Integer instructions and conversions are not supported by the Metal kernel target." in
    if inline then inlined.(slot) <- Some expression
    else if constant then Printf.bprintf current.Scope.decls "  %s r%d=%s;\n" ty slot expression
    else if current.Scope.opened=[] || (local.(slot) && not named.(slot) && not !counting) then
      Printf.bprintf body "%s%s r%d=%s;\n" indentation ty slot expression
    else begin
      Printf.bprintf current.Scope.decls "  %s r%d=%s;\n" ty slot zero;
      Printf.bprintf body "%sr%d=%s;\n" indentation slot expression
    end;
    if finite_checks.(slot) && not (integer_register slot) then
      Printf.bprintf body "%ssb|=!isfinite(r%d);\n" indentation slot;
    (match instruction with
     | P.Int_binary (operation, _, b) ->
         Buffer.add_string body (if narrow.(b) then integer_status ~unsigned:true indentation operation (low b)
           else integer_status indentation operation (int_of b))
     | _ -> ())
    end) ~loop:(fun header control ~next ~stop ~body:emit_body -> if used.(header)then begin
      let outer = List.hd !scopes in
      let guards,_=guarded [] view.code.(header)in
      context := (outer.Scope.id, guards);
      let ty slot=if narrow.(slot) then "uint" else if integer_register slot then "long" else "float"in
      let carried slot source = if narrow.(slot) then low source else if integer_register slot then int_of source else register source in
      Printf.bprintf outer.Scope.decls "  %s r%d=0;\n" (ty header) header;
      Array.iteri(fun component _->let slot=header+1+component in
        Printf.bprintf outer.Scope.decls "  %s r%d=0;\n" (ty slot)slot)control.initial;
      enter outer guards;
      blocks := Scope.stack outer;
      let body = outer.Scope.code and indentation = indent outer in
      (* a pass count over the limit flags the status and runs no pass *)
      Option.iter(fun limit->Printf.bprintf body
        "%sbool over%d=%s>%s && ulong(%s-%s)>%duL;\n%ssb|=over%d?1u:0u;\n" indentation header
        (int_of control.last) (int_of control.first) (int_of control.last) (int_of control.first) limit indentation header)control.limit;
      Array.iteri(fun component initial->let slot=header+1+component in
        Printf.bprintf body "%sr%d=%s;\n" indentation slot (carried slot initial))control.initial;
      let bound r = if narrow.(header) then low r else int_of r in
      let last = if control.limit = None then bound control.last
        else Printf.sprintf "(over%d?%s:%s)" header (bound control.first) (bound control.last) in
      Printf.bprintf body "%sfor(r%d=%s;r%d<%s;r%d++){\n" indentation header (bound control.first) header last header;
      let inner = scope ~outer:(Scope.stack outer) ~next:outer.Scope.next header in
      scopes := inner :: !scopes;
      emit_body();
      enter inner [];
      context := (inner.Scope.id, []);
      blocks := Scope.stack inner;
      scopes := List.tl !scopes;
      Buffer.add_string body (declarations inner.Scope.decls);
      Buffer.add_buffer body inner.Scope.code;
      Option.iter(fun source->Printf.bprintf body "  bool stop%d=%s!=0.0f;\n" header (float_of source))stop;
      Array.iteri(fun component source->let slot=header+1+component in
        Printf.bprintf body "  %s next%d_%d=%s;\n" (ty slot)header component (carried slot source))next;
      Array.iteri(fun component _->Printf.bprintf body "  r%d=next%d_%d;\n" (header+1+component)header component)next;
      Option.iter(fun _->Printf.bprintf body "  if(stop%d)break;\n" header)stop;
      Printf.bprintf body "%s}\n" indentation
    end);
  enter root [];
  context := (0, []);
  blocks := [];
  let body = Buffer.create 4096 in
  Buffer.add_string body (declarations root.Scope.decls);
  Buffer.add_buffer body root.Scope.code;
  if layout.output_checks then Array.iter (Array.iter (fun slot -> if not (integer_register slot) then
    Printf.bprintf body "  sb|=!isfinite(r%d);\n" slot)) view.outputs;
  Printf.bprintf body "  %s\n" flush_status;
  Array.iteri (fun k registers -> Array.iteri (fun component slot ->
    Printf.bprintf body "  %s[i*%d+%d]=%s;\n"
      (output_name k) (Array.length registers) component (float_of slot)) registers) view.outputs;
  body in
  ignore (print ());
  counting := false;
  let body = print () in
  let count = Array.length view.widths in
  (* fast: the element count may come from a device cell (another kernel's, times mul over div,
     when use is set) and is written to this kernel's own count record {n, 6n, 1, 0, 0, 0}: a
     resident fold never reads it, and its last five words are the indexed indirect draw of n
     quads a mark set reads *)
  let arguments = Array.to_list (Array.mapi (fun index _ -> Printf.sprintf
    "device const float* input%d [[buffer(%d)]]" index index) view.widths)
    @ List.init outputs (fun k -> Printf.sprintf "device float* %s [[buffer(%d)]]" (output_name k) (count+k))
    @ [Printf.sprintf "constant uint* uniforms [[buffer(%d)]]" uniform_binding;
       Printf.sprintf "device atomic_uint* status [[buffer(%d)]]" status_binding]
    @ (match table_binding with None -> [] | Some binding -> [Printf.sprintf "device const int* table [[buffer(%d)]]" binding])
    @ List.mapi (fun k _ -> Printf.sprintf "device const uint* gather%d [[buffer(%d)]]" k (gather_base+k)) gathers
    @ (if counts=None then [] else [Printf.sprintf "device const uint* count_in [[buffer(%d)]]" count_base;
        Printf.sprintf "device uint* count_out [[buffer(%d)]]" (count_base+1)])
    @ (match scene_binding with None -> [] | Some b ->
        [Printf.sprintf "%s scene [[buffer(%d)]]" (structure_type (scene_instances <> None) layout.scene_motion) b;
         Printf.sprintf "device const packed_float3* scene_vertices [[buffer(%d)]]" (b+1);
         Printf.sprintf "device const float4* scene_spheres [[buffer(%d)]]" (b+2)]
        @ (if scene_instances = None then [] else [Printf.sprintf "device const float* scene_instances [[buffer(%d)]]" (b+3)])
        @ (if layout.scene_vertices_end = None then [] else [Printf.sprintf "device const packed_float3* scene_vertices_end [[buffer(%d)]]" (b+3)])
        @ (match layout.scene_strands with None -> [] | Some s ->
            Printf.sprintf "device const float4* scene_strands [[buffer(%d)]]" s
            :: (if native then [Printf.sprintf "primitive_acceleration_structure scene_curves [[buffer(%d)]]" (s+1)] else [])))
    @ ["uint i [[thread_position_in_grid]]"] in
  let prefix = "#include <metal_stdlib>\nusing namespace metal;\n" ^
    (if fast then "long fmod64(long a,long b){if(b==0)return 0;long r=a%b;return (r!=0&&((r<0)!=(b<0)))?r+b:r;}\n" else "") ^
    (if scene_binding=None then "" else trace_prelude ?curves:(if layout.scene_strands = None then None else curves) (scene_instances<>None) layout.scene_motion) ^ (if table_seeds=[||] then "" else perlin) in
  let signature = String.concat ",\n  " arguments in
  let guard = match counts with
    | None -> "  if(i>=uniforms[0]) return;\n"
    | Some o -> let w = o/4 in Printf.sprintf
        "  uint n=uniforms[0];\n  if(uniforms[%d]!=0u) n=min(n,count_in[0]*uniforms[%d]/uniforms[%d]);\n  if(i==0){count_out[0]=n;count_out[1]=6u*n;count_out[2]=1u;count_out[3]=0u;count_out[4]=0u;count_out[5]=0u;}\n  if(i>=n) return;\n"
        (w+2) w (w+1) in
  let body = guard ^ "  uint sb=0u;\n" ^ Buffer.contents body in
  let digest = Digest.to_hex (Digest.string (prefix ^ signature ^ body)) in
  let entry = "kernel_" ^ digest in
  let source = prefix ^ "kernel void " ^ entry ^ "(\n  " ^ signature ^ ") {\n" ^ body ^ "}\n" in
  let interface = List.init (count_base+(if counts=None then 0 else 2)) Fun.id
    |>List.filter(fun binding->binding>=count || input_used.(binding))
    |>List.map(fun binding->S.{group=0;binding;kind=(if binding=uniform_binding then Uniform_buffer else Storage_buffer);visibility=[Compute]})in
  let interface = interface @ (match scene_binding with None -> [] | Some b ->
    [S.{group=0;binding=b;kind=Acceleration_structure;visibility=[Compute]};
     S.{group=0;binding=b+1;kind=Storage_buffer;visibility=[Compute]};
     S.{group=0;binding=b+2;kind=Storage_buffer;visibility=[Compute]}]
    @ (if scene_instances=None && layout.scene_vertices_end=None then [] else [S.{group=0;binding=b+3;kind=Storage_buffer;visibility=[Compute]}])
    @ (match layout.scene_strands with None -> [] | Some s -> S.{group=0;binding=s;kind=Storage_buffer;visibility=[Compute]}
        :: (if native then [S.{group=0;binding=s+1;kind=Acceleration_structure;visibility=[Compute]}] else []))) in
  Ok {source;entry;interface;uniform_layout;uniform_bytes;scene=scene_binding;
    table_seeds;output_widths=Array.map Array.length view.outputs;input_widths=view.widths;
    gathers=Array.of_list (List.map snd gathers);counts}
with Diagnostic.Error diagnostic -> Error diagnostic

(* Stream compaction for a resident fold: the indices of a mask's nonzero elements, in order, as
   floats, and their count.  One threadgroup of up to 1,024 threads: each counts a contiguous chunk,
   thread 0 turns the counts into offsets, each writes its chunk's indices. *)
let compaction =
  let entry = "flow_compact_mask" in
  let source = {|#include <metal_stdlib>
using namespace metal;
kernel void flow_compact_mask(device const float* mask [[buffer(0)]], device float* kept [[buffer(1)]],
  device uint* total [[buffer(2)]], constant uint2& bound [[buffer(3)]], device const uint* count_in [[buffer(4)]],
  uint t [[thread_position_in_threadgroup]], uint size [[threads_per_threadgroup]]) {
  threadgroup uint offsets[1024];
  uint n = bound.y != 0u ? min(bound.x, count_in[0]) : bound.x;
  uint chunk = (n + size - 1) / size, first = min(n, t * chunk), last = min(n, first + chunk), count = 0;
  for (uint i = first; i < last; i++) if (mask[i] != 0.0f) count++;
  offsets[t] = count;
  threadgroup_barrier(mem_flags::mem_threadgroup);
  if (t == 0) { uint sum = 0; for (uint k = 0; k < size; k++) { uint v = offsets[k]; offsets[k] = sum; sum += v; } total[0] = sum; }
  threadgroup_barrier(mem_flags::mem_threadgroup);
  uint at = offsets[t];
  for (uint i = first; i < last; i++) if (mask[i] != 0.0f) kept[at++] = float(i);
}
|} in
  {source; entry; uniform_layout = []; uniform_bytes = 8; table_seeds = [||];
   output_widths = [|1|]; input_widths = [|1|]; gathers = [||]; counts = None; scene = None;
   interface = List.map (fun binding -> S.{group = 0; binding;
     kind = (if binding = 3 then Uniform_buffer else Storage_buffer); visibility = [Compute]}) [0; 1; 2; 3; 4]}

(* The image sink's RGBA8 packing kernel: all flow_gpu MSL lives in this printer. *)
let image_rgba8_source = {|#include <metal_stdlib>
using namespace metal;
struct ImageSize { uint count; uint width; uint row_words; };
uint rgba8_channel(float c) { return uint(rint(clamp(c,0.0f,1.0f)*255.0f)); }
kernel void rays_flow_image_rgba8(device const float4 *colors [[buffer(0)]],
    device uint *pixels [[buffer(1)]], constant ImageSize &size [[buffer(2)]],
    uint i [[thread_position_in_grid]]) {
  if (i>=size.count) return;
  float4 c=colors[i];
  uint rgba=rgba8_channel(c.x)|(rgba8_channel(c.y)<<8)
    |(rgba8_channel(c.z)<<16)|(rgba8_channel(c.w)<<24);
  pixels[(i/size.width)*size.row_words+i%size.width]=rgba;
}
|}
