(* Raster stages from Rays Lisp: a vertex stage is a map over vertex ids, a fragment stage a map over
   fragments. A stage is one Packed program of its Lisp entry with an output per leaf of its result
   record, printed by Emit's instruction printer in a raster placement (Emit.stage_body). *)
module E = Flow.Eval
module W = Flow.Workspace

let fail message = Error (Diagnostic.error ~code:"E_RASTER" message)
let ( let* ) = Result.bind
let all results = List.fold_right (fun r acc -> Result.bind acc (fun acc -> Result.map (fun x -> x :: acc) r)) results (Ok [])

(* The entry's parameters, read from its authored [defn]. *)
let parameters forms entry =
  let rec find = function
    | [] -> None
    | ({ Flow.Syntax.node = List ({ node = Sym "defn"; _ } :: { node = Sym name; _ } :: rest); _ } : Flow.Syntax.t) :: _
      when name = entry ->
        List.find_map (fun (x : Flow.Syntax.t) -> match x.node with Vec params -> Some params | _ -> None) rest
    | { node = List items; _ } :: more -> (match find items with Some _ as found -> found | None -> find more)
    | _ :: more -> find more in
  match find forms with
  | None -> fail ("no defn " ^ entry)
  | Some params -> all (List.map (fun (p : Flow.Syntax.t) -> match p.node with
      | List [ { node = Sym name; _ }; { node = Sym ":"; _ }; ty ] ->
          (match Flow.Ty.of_syntax ty with Some ty -> Ok (name, ty)
           | None -> fail (entry ^ ": parameter " ^ name ^ " has no stage type"))
      | _ -> fail (entry ^ ": every stage parameter is typed: " ^ fst (Flow.Lisp.print [ p ]))) params)

(* The [index]th array parameter is a host buffer bound at [buffer(index)]. Before compiling, its
   placeholder becomes a distinct (index + 1)-element array, so the template keeps it a gather table
   (never a folded constant) and a gather's uniform names its buffer by length. *)
let dummy index = function
  | Flow.Ty.Array Int -> Ok (E.Int_array (Array.make (index + 1) 0))
  | Array Float -> Ok (E.Float_array (Array.make (index + 1) 0.))
  | ty -> fail ("a stage buffer is (array int) or (array float), not " ^ Flow.Ty.to_string ty)
let column = function
  | Flow.Ty.Int -> E.Int_array [| 0 |] | Float -> E.Float_array [| 0. |]
  | Vec2 -> E.Vec2_array [| 0.; 0. |] | Vec3 -> E.Vec3_array [| 0.; 0.; 0. |]
  | Vec4 -> E.Vec4_array [| 0.; 0.; 0.; 0. |] | _ -> invalid_arg "Raster.column"
let is_buffer (_, ty) = match ty with Flow.Ty.Array _ -> true | _ -> false

(* A stage output: a leaf of the entry's result, in field order (the program's outputs), [flat] when
   it sits under [:flat]. *)
type leaf = { path : string list; ty : Flow.Ty.t; flat : bool }
type stage = { params : (string * Flow.Ty.t) list; leaves : leaf list; program : Flow_ir.Packed.t }

let rec leaves ?(flat = false) path = function
  | Flow.Ty.Record fields -> List.concat_map (fun (name, ty) ->
      leaves ~flat:(flat || name = "flat") (path @ [ name ]) ty) fields
  | ty -> [ { path; ty; flat } ]

let compile ~declarations entry =
  let* forms = Flow.Syntax.parse declarations in
  let* params = parameters forms entry in
  let buffers = List.filter is_buffer params and elements = List.filter (fun p -> not (is_buffer p)) params in
  let ops = List.mapi (fun index (_, ty) -> let name = "raster/buffer" ^ string_of_int index in
    { Flow.Op.noise3 with name; packed_extension = None; signature = { pos = []; opt = []; rest = None; kw = [] };
      out = (fun _ -> ty); shape = Flow.Op.Struct { splice = false }; check = (fun _ -> ());
      body = (fun ~live:_ ~node:_ _ -> Flow.Value.Source (name, ty, [])) }) buffers in
  let text = Printf.sprintf "(workspace raster %s (graph g :context value (let* [%s kernel (fn [%s] (%s %s))] 0.0)))"
    declarations (String.concat " " (List.mapi (fun index (name, _) -> Printf.sprintf "%s (raster/buffer%d)" name index) buffers))
    (String.concat " " (List.map (fun (name, ty) -> Printf.sprintf "(%s : %s)" name (Flow.Ty.to_string ty)) elements))
    entry (String.concat " " (List.map fst params)) in
  let* forms = Flow.Syntax.parse text in
  let* fn = match Flow.Workspace.check ~ops { Flow.Check.version = 1; kinds = [] } forms with
    | Some ws, [] ->
        let* evaluated = E.static ~record:true ws in
        (match snd (List.hd (List.assoc [ "g"; "kernel" ] evaluated.records)) with
         | E.Fn fn -> Ok fn | _ -> fail (entry ^ ": the stage is not a function"))
    | _, d :: _ -> Error d | _, [] -> fail (entry ^ ": unchecked") in
  let result = match E.Residual.function_body fn with Some (_, body) -> body.W.ty | None -> Flow.Ty.Any in
  let* bound = all (List.mapi (fun index (_, ty) -> dummy index ty) buffers) in
  let fn = E.bind_captures (function
    | Flow.Value.Source (source, _, []) when String.starts_with ~prefix:"raster/buffer" source ->
        List.nth bound (int_of_string (String.sub source 13 (String.length source - 13)))
    | value -> value) fn in
  let* residual = E.map_kernel ~signature:{ params = List.map snd elements; result } fn
    (List.map (fun (_, ty) -> column ty) elements) in
  match residual with
  | E.Residual r -> (match Flow_ir.Packed.compile_template r (E.Residual.residual_view r).term with
      | Some program -> Ok { params; leaves = leaves [] result; program }
      | None -> (match Flow_ir.Packed.compile_result r (E.Residual.residual_view r).term with
          | Error (d :: _) -> Error d | _ -> fail (entry ^ " does not compile to a packed program")))
  | _ -> fail (entry ^ ": the stage did not defer")

let identifier name = String.map (fun c -> if c = '-' then '_' else c) name
let msl_type = function
  | Flow.Ty.Float | Bool -> Ok "float" | Vec2 -> Ok "float2" | Vec3 -> Ok "float3" | Vec4 -> Ok "float4"
  | ty -> fail ("a stage value is a float or a vector, not " ^ Flow.Ty.to_string ty)
let component target ty k = match ty with Flow.Ty.Float | Bool -> target | _ -> target ^ "." ^ String.make 1 "xyzw".[k]

(* The bound buffer behind a gather's array uniform: the placeholders' lengths number them. *)
let buffer (program : Flow_ir.Packed.t) u = match program.uniforms.(u) with
  | E.Int_array a -> "buffer" ^ string_of_int (Array.length a - 1)
  | E.Float_array a -> "buffer" ^ string_of_int (Array.length a - 1)
  | _ -> invalid_arg "Raster.buffer"

let body ~input ~int_input ~store stage =
  Emit.stage_body ~raster:{ Emit.input; int_input; buffer = buffer stage.program;
    output = (fun k c value -> store (List.nth stage.leaves k) c value) } stage.program

(* The textures a fragment's intrinsics read, texture t with its sampler at t + 1. *)
let textures stage =
  let rec unguarded = function Flow_ir.Packed.Guarded (_, i) -> unguarded i | i -> i in
  List.filter_map (fun i -> match unguarded i with
    | Flow_ir.Packed.Stage ((Sample (t, _) | Texture_size (t, _)), _) -> Some t | _ -> None)
    (Array.to_list stage.program.Flow_ir.Core.Region.code) |> List.sort_uniq compare
  |> List.concat_map (fun t -> [ Printf.sprintf "texture2d<float> texture%d [[texture(%d)]]" t t;
    Printf.sprintf "sampler sampler%d [[sampler(%d)]]" (t + 1) (t + 1) ])

let buffer_arguments params =
  List.mapi (fun index (_, ty) -> Printf.sprintf "device const %s* buffer%d [[buffer(%d)]]"
    (if ty = Flow.Ty.Array Int then "uint" else "float") index index) (List.filter is_buffer params)

(* A vertex entry's first parameter is the vertex id; its result has [:position] (clip space) and the
   varyings, those under [:flat] not interpolated. A fragment entry's elements are varyings by name
   and its result is the colour, or [{:color :discard}]. *)
let msl ~declarations ~vertex ~fragment =
  let* v = compile ~declarations vertex in
  let* f = compile ~declarations fragment in
  (* the discard test after the colour stores: measured faster on Apple GPUs than before them *)
  let last = List.partition (fun (leaf, _) -> leaf.path <> [ "discard" ]) (List.combine f.leaves (Array.to_list f.program.outputs)) in
  let* f = match last with
    | _, [] -> Ok f
    | keep, discard -> Result.map (fun program -> { f with leaves = List.map fst (keep @ discard); program })
        (Result.map_error List.hd (Flow_ir.Core.Region.rebuild f.program ~outputs:(Array.of_list (List.map snd (keep @ discard))) ())) in
  let out = identifier vertex ^ "_out" in
  let name leaf = identifier (List.nth leaf.path (List.length leaf.path - 1)) in
  let* fields = all (List.map (fun leaf -> Result.map (fun ty ->
    if leaf.path = [ "position" ] then "  float4 position [[position]];\n"
    else Printf.sprintf "  %s %s%s;\n" ty (name leaf) (if leaf.flat then " [[flat]]" else "")) (msl_type leaf.ty)) v.leaves) in
  let* vertex_body = body v ~input:(fun _ _ -> "0.0f") ~int_input:(fun _ -> "long(vid)")
    ~store:(fun leaf c value -> component ("o." ^ name leaf) leaf.ty c ^ "=" ^ value ^ ";") in
  let varyings = List.filter (fun p -> not (is_buffer p)) f.params in
  let input k c = let name, ty = List.nth varyings k in component ("in." ^ identifier name) ty c in
  let* fragment_body = body f ~input ~int_input:(fun _ -> "0") ~store:(fun leaf c value ->
    if leaf.path = [ "discard" ] then "if(" ^ value ^ "!=0.0f) discard_fragment();"
    else component "colour" Flow.Ty.Vec4 c ^ "=" ^ value ^ ";") in
  let signature params extra = String.concat ",\n    " (extra @ buffer_arguments params) in
  Ok (String.concat "" ([ "#include <metal_stdlib>\nusing namespace metal;\n";
    "long fmod64(long a,long b){if(b==0)return 0;long r=a%b;return (r!=0&&((r<0)!=(b<0)))?r+b:r;}\n";
    "struct "; out; " {\n" ] @ fields @ [ "};\n";
    Printf.sprintf "vertex %s %s(\n    %s) {\n  %s o;\n" out (identifier vertex)
      (signature (List.tl v.params) [ "uint vid [[vertex_id]]" ]) out; vertex_body; "  return o;\n}\n";
    Printf.sprintf "fragment float4 %s(\n    %s) {\n  float4 colour;\n" (identifier fragment)
      (signature f.params ((out ^ " in [[stage_in]]") :: textures f)); fragment_body; "  return colour;\n}\n" ]))
