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
let trace_prelude = {|#include <metal_raytracing>
using namespace metal::raytracing;
struct TraceHit { float t; float prim; float u; float v; float3 n; float hit; };
TraceHit trace_scene(primitive_acceleration_structure scene, device const packed_float3* vertices,
    device const float4* spheres, uint sphere_count, float3 o, float3 d, float tmin, float tmax) {
  intersector<triangle_data> query;
  query.assume_geometry_type(geometry_type::triangle);
  intersection_result<triangle_data> hit = query.intersect(ray(o, normalize(d), tmin, tmax), scene);
  TraceHit h = {0.0f, -1.0f, 0.0f, 0.0f, float3(0.0f), 0.0f};
  if (hit.type == intersection_type::triangle) {
    uint p = hit.primitive_id;
    float3 a = vertices[p*3], b = vertices[p*3+1], c = vertices[p*3+2];
    h.t = hit.distance; h.prim = float(p); h.u = hit.triangle_barycentric_coord.x; h.v = hit.triangle_barycentric_coord.y;
    h.n = normalize(cross(b - a, c - a)); h.hit = 1.0f;
  }
  float3 dn = normalize(d);
  for (uint k = 0; k < sphere_count; k++) {
    float4 s = spheres[k]; float3 oc = o - s.xyz;
    float b = dot(oc, dn); float3 q = oc - b*dn; float disc = s.w*s.w - dot(q, q);
    if (disc >= 0.0f) {
      float root = sqrt(disc), t = (-b - root >= tmin) ? -b - root : -b + root;
      if (t >= tmin && t <= tmax && (h.hit == 0.0f || t < h.t)) {
        h.t = t; h.prim = -2.0f - float(k); h.u = 0.0f; h.v = 0.0f; h.n = (oc + t*dn) / s.w; h.hit = 1.0f;
      }
    }
  }
  return h;
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
let integer_status indentation operation b = match operation with
  | Flow.Packed_ops.Idiv | Imod ->
      Printf.sprintf "%sif(%s==0) atomic_fetch_or_explicit(status,2u,memory_order_relaxed);\n" indentation b
  | Ishl | Ishr -> Printf.sprintf
      "%sif(%s<0 || %s>62) atomic_fetch_or_explicit(status,4u,memory_order_relaxed);\n" indentation b b
  | _ -> ""
let integer operation a b = wrap_integer (match operation with
  | Flow.Packed_ops.Iadd -> "(ulong(" ^ a ^ ")+ulong(" ^ b ^ "))"
  | Isub -> "(ulong(" ^ a ^ ")-ulong(" ^ b ^ "))"
  | Imul -> "(ulong(" ^ a ^ ")*ulong(" ^ b ^ "))"
  | Idiv -> "(" ^ b ^ "==0?0:" ^ a ^ "/" ^ b ^ ")" | Imod -> "(" ^ b ^ "==0?0:" ^ a ^ "%" ^ b ^ ")"
  | Iand -> "(" ^ a ^ "&" ^ b ^ ")" | Ior -> "(" ^ a ^ "|" ^ b ^ ")" | Ixor -> "(" ^ a ^ "^" ^ b ^ ")"
  | Ishl -> "(ulong(" ^ a ^ ")<<(" ^ b ^ "&63))"
  | Ishr -> "long((ulong(" ^ a ^ ")&0x7fffffffffffffffUL)>>(" ^ b ^ "&63))")
let comparison operation a b =
  let op = match operation with Flow.Packed_ops.Ilt -> "<" | Ile -> "<=" | Igt -> ">" | Ige -> ">=" | Ieq -> "==" in
  "(" ^ a ^ op ^ b ^ "?1.0f:0.0f)"
let kernel ?(fast=false) program = try
  (match P.gpu_refusals ~fused:true ~fast program with [] -> () | reason :: _ -> raise (Diagnostic.Error reason));
  let layout = Flow_ir.Kernel_layout.plan ~fast program in
  let {Flow_ir.Kernel_layout.view; used; finite_checks; uniforms; frames; table_seeds;
       gathers; gather_lengths; input_used; uniform_layout; uniform_bytes; gather_base;
       uniform_binding; status_binding; table_binding; count_base; counts; scene_binding; scene_spheres} = layout in
  let table seed = match Array.find_index ((=) seed) table_seeds with Some index -> index*512 | None -> assert false in
  let gather_index slot = match List.find_index (fun (s, _) -> s = slot) gathers with Some k -> k | None -> fail "Gather without its table." in
  let integer_register slot = Flow_ir.Core.Region.integer_register view.code.(slot) in
  let int_of r = if integer_register r then register r else "long(" ^ register r ^ ")" in
  (* the integer register a float operand converts, if it is one *)
  (* an integer-valued register as a 64-bit expression: an integer register, its conversion to float,
     an integral constant, or +, -, *, mod, min, max of two such; [None] for any other *)
  let rec whole r = if integer_register r then Some (register r) else match snd (guarded [] view.code.(r)) with
    | P.Convert (To_float, x) when integer_register x -> Some (register x)
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
      k (int_of index) gather_lengths.(k) width component in
  let hits = Hashtbl.create 4 in
  let body = Buffer.create 4096 in
  Flow_ir.Kernel_layout.iter layout ~instruction:(fun slot instruction ->
    if used.(slot) && (match snd (guarded [] instruction) with P.Array_prepare _ -> false | _ -> true) then begin
    let guards,instruction=guarded [] instruction in
    let expression = match instruction with
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
      | Int_select (condition,yes,no) -> Printf.sprintf "(%s!=0.0f?%s:%s)" (float_of condition) (int_of yes) (int_of no)
      | Convert (To_float,a) -> float_of a
      | Convert ((Truncate|Coerce_int|Integer_result _),a) -> int_of a
      | Convert (Floor,a) -> "long(floor(" ^ float_of a ^ "))"
      | Convert (Ceil,a) -> "long(ceil(" ^ float_of a ^ "))"
      | Convert (Round,a) -> "long(round(" ^ float_of a ^ "))"
      | Text_const _ | Text_input _ | Text_uniform _ | Text_gather _ | Text_select _ -> fail "Text kernels stay on the CPU."
      | Trace (field, r) ->
          let key = Array.to_list r in
          let hit = match Hashtbl.find_opt hits key with Some name -> name | None ->
            let name = Printf.sprintf "hit%d" slot and a k = float_of r.(k) in
            Printf.bprintf body "  TraceHit %s=trace_scene(scene,scene_vertices,scene_spheres,uniforms[%d],float3(%s,%s,%s),float3(%s,%s,%s),%s,%s);\n"
              name (Option.get scene_spheres) (a 1) (a 2) (a 3) (a 4) (a 5) (a 6) (a 7) (a 8);
            Hashtbl.add hits key name; name in
          hit ^ [| ".t"; ".prim"; ".u"; ".v"; ".n.x"; ".n.y"; ".n.z"; ".hit" |].(field)
      | Select (condition,yes,no) -> Printf.sprintf "(%s!=0.0f?%s:%s)" (register condition) (register yes) (register no)
      | Noise3 (x,y,z,seed,octaves,_,_) -> Printf.sprintf "fbm(float3(%s,%s,%s),%d,table+%d)"
          (register x) (register y) (register z) octaves (table seed)
      | Loop_result (loop,component,_) -> register (loop+1+component)
      | _ -> fail "Integer instructions and conversions are not supported by the Metal kernel target." in
    let indentation=if guards=[] then "  " else "    " in
    let ty, zero = if integer_register slot then "long", "0" else "float", "0.0f" in
    if guards=[] then Printf.bprintf body "  %s r%d=%s;\n" ty slot expression else begin
      Printf.bprintf body "  %s r%d=%s;\n" ty slot zero;
      Printf.bprintf body "  if(%s) {\n" (String.concat "&&" (List.map(fun(condition,selected)->
        Printf.sprintf "r%d%s0.0f" condition (if selected then "!=" else "=="))guards));
      Printf.bprintf body "    r%d=%s;\n" slot expression
    end;
    if finite_checks.(slot) && not (integer_register slot) then
      Printf.bprintf body "%sif(!isfinite(r%d)) atomic_fetch_or_explicit(status,1u,memory_order_relaxed);\n" indentation slot;
    (match instruction with
     | P.Int_binary (operation, _, b) ->
         Buffer.add_string body (integer_status indentation operation (int_of b))
     | _ -> ());
    if guards<>[] then Buffer.add_string body "  }\n"
    end) ~loop:(fun header control ~next ~stop ~body:emit_body -> if used.(header)then begin
      let guards,_=guarded [] view.code.(header)in
      let ty slot=if integer_register slot then "long" else "float"in
      Printf.bprintf body "  long r%d=0;\n" header;
      Array.iteri(fun component _->let slot=header+1+component in
        Printf.bprintf body "  %s r%d=0;\n" (ty slot)slot)control.initial;
      if guards<>[]then Printf.bprintf body "  if(%s) {\n" (String.concat "&&"
        (List.map(fun(condition,selected)->Printf.sprintf "%s%s0.0f" (float_of condition)
          (if selected then "!=" else "=="))guards));
      Option.iter(fun limit->Printf.bprintf body
        "  if(r%d>r%d && ulong(r%d-r%d)>%duL){atomic_fetch_or_explicit(status,1u,memory_order_relaxed);return;}\n"
        control.last control.first control.last control.first limit)control.limit;
      Array.iteri(fun component initial->Printf.bprintf body "  r%d=r%d;\n" (header+1+component)initial)control.initial;
      Printf.bprintf body "  for(r%d=r%d;r%d<r%d;r%d++){\n" header control.first header control.last header;
      emit_body();
      Option.iter(fun source->Printf.bprintf body "  bool stop%d=%s!=0.0f;\n" header (float_of source))stop;
      Array.iteri(fun component source->Printf.bprintf body "  %s next%d_%d=r%d;\n"
        (ty source)header component source)next;
      Array.iteri(fun component _->Printf.bprintf body "  r%d=next%d_%d;\n" (header+1+component)header component)next;
      Option.iter(fun _->Printf.bprintf body "  if(stop%d)break;\n" header)stop;
      Buffer.add_string body "  }\n";
      if guards<>[]then Buffer.add_string body "  }\n"
    end);
  let outputs = Array.length view.outputs in
  let output_name k = if outputs = 1 then "output" else "output" ^ string_of_int k in
  Array.iteri (fun k registers -> Array.iteri (fun component slot ->
    Printf.bprintf body "  %s[i*%d+%d]=%s;\n"
      (output_name k) (Array.length registers) component (float_of slot)) registers) view.outputs;
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
        [Printf.sprintf "primitive_acceleration_structure scene [[buffer(%d)]]" b;
         Printf.sprintf "device const packed_float3* scene_vertices [[buffer(%d)]]" (b+1);
         Printf.sprintf "device const float4* scene_spheres [[buffer(%d)]]" (b+2)])
    @ ["uint i [[thread_position_in_grid]]"] in
  let prefix = "#include <metal_stdlib>\nusing namespace metal;\n" ^
    (if fast then "long fmod64(long a,long b){if(b==0)return 0;long r=a%b;return (r!=0&&((r<0)!=(b<0)))?r+b:r;}\n" else "") ^
    (if scene_binding=None then "" else trace_prelude) ^ (if table_seeds=[||] then "" else perlin) in
  let signature = String.concat ",\n  " arguments in
  let guard = match counts with
    | None -> "  if(i>=uniforms[0]) return;\n"
    | Some o -> let w = o/4 in Printf.sprintf
        "  uint n=uniforms[0];\n  if(uniforms[%d]!=0u) n=min(n,count_in[0]*uniforms[%d]/uniforms[%d]);\n  if(i==0){count_out[0]=n;count_out[1]=6u*n;count_out[2]=1u;count_out[3]=0u;count_out[4]=0u;count_out[5]=0u;}\n  if(i>=n) return;\n"
        (w+2) w (w+1) in
  let body = guard ^ "  if(i==0) atomic_fetch_or_explicit(status,0u,memory_order_relaxed);\n" ^ Buffer.contents body in
  let digest = Digest.to_hex (Digest.string (prefix ^ signature ^ body)) in
  let entry = "kernel_" ^ digest in
  let source = prefix ^ "kernel void " ^ entry ^ "(\n  " ^ signature ^ ") {\n" ^ body ^ "}\n" in
  let interface = List.init (count_base+(if counts=None then 0 else 2)) Fun.id
    |>List.filter(fun binding->binding>=count || input_used.(binding))
    |>List.map(fun binding->S.{group=0;binding;kind=(if binding=uniform_binding then Uniform_buffer else Storage_buffer);visibility=[Compute]})in
  let interface = interface @ (match scene_binding with None -> [] | Some b ->
    [S.{group=0;binding=b;kind=Acceleration_structure;visibility=[Compute]};
     S.{group=0;binding=b+1;kind=Storage_buffer;visibility=[Compute]};
     S.{group=0;binding=b+2;kind=Storage_buffer;visibility=[Compute]}]) in
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
