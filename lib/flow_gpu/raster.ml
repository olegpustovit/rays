(* Raster stages from Rays Lisp: a vertex stage is a map over vertex ids, a fragment stage a map over
   fragments. A stage is one Packed program of its Lisp entry with an output per leaf of its result
   record, printed by Emit's instruction printer in a raster placement (Emit.stage_body). *)
module E = Flow.Eval
open Lisp_stage

let identifier name = String.map (fun c -> if c = '-' then '_' else c) name

let msl_type = function
  | Flow.Ty.Float | Bool -> Ok "float"
  | Vec2 -> Ok "float2"
  | Vec3 -> Ok "float3"
  | Vec4 -> Ok "float4"
  | ty -> fail ("a stage value is a float or a vector, not " ^ Flow.Ty.to_string ty)

let component target ty k =
  match ty with Flow.Ty.Float | Bool -> target | _ -> target ^ "." ^ String.make 1 "xyzw".[k]

(* The bound buffer behind a gather's array uniform: the placeholders' lengths number them. *)
let slot (program : Flow_ir.Packed.t) u =
  match program.uniforms.(u) with
  | E.Int_array a -> Array.length a - 1
  | E.Float_array a -> Array.length a - 1
  | _ -> invalid_arg "Raster.buffer"

let buffer program u = "buffer" ^ string_of_int (slot program u)

let body ~input ~int_input ~store stage =
  Emit.stage_body
    ~raster:
      {
        Emit.input;
        int_input;
        buffer = buffer stage.program;
        output = (fun k c value -> store (List.nth stage.leaves k) c value);
        builtin =
          (function
          | "gpu/instance-id" -> "iid"
          | "gpu/front-facing" -> "(front?1.0f:0.0f)"
          | "gpu/depth" -> "in.position.z"
          | "gpu/fragment-x" -> "in.position.x"
          | "gpu/fragment-y" -> "in.position.y"
          | name -> invalid_arg ("Raster: no built-in " ^ name));
      }
    stage.program

(* The textures a fragment's intrinsics read: texture t with its sampler at t + 1 (none when it is only
   read by texel), or (an argument-buffer
   pipeline) both in the argument buffer at buffer t, held in locals of the same names. *)
let textures stage =
  let rec unguarded = function Flow_ir.Packed.Guarded (_, i) -> unguarded i | i -> i in
  List.filter_map
    (fun i ->
      match unguarded i with
      | Flow_ir.Packed.Stage ((Sample (t, _) | Sample_level (t, _) | Texture_size (t, _)), _) ->
          Some (t, true)
      | Flow_ir.Packed.Stage (Read (t, _), _) -> Some (t, false)
      | _ -> None)
    (Array.to_list stage.program.Flow_ir.Core.Region.code)
  |> fun used -> List.sort_uniq compare (List.map (fun (t, _) -> (t, List.mem (t, true) used)) used)

let texture_arguments ~arguments stage =
  List.concat_map
    (fun (t, sampled) ->
      if arguments then [ Printf.sprintf "constant raster_texture& arguments%d [[buffer(%d)]]" t t ]
      else
        Printf.sprintf "texture2d<float> texture%d [[texture(%d)]]" t t
        ::
        (if sampled then [ Printf.sprintf "sampler sampler%d [[sampler(%d)]]" (t + 1) (t + 1) ]
         else []))
    (textures stage)

let texture_locals ~arguments stage =
  if not arguments then ""
  else
    String.concat ""
      (List.map
         (fun (t, _) ->
           Printf.sprintf
             "  texture2d<float> texture%d=arguments%d.image;\n\
             \  sampler sampler%d=arguments%d.sampling;\n"
             t t (t + 1) t)
         (textures stage))

(* A built-in argument, only when the stage reads its fact (Frame or Int_frame [name]). *)
let builtin stage name argument =
  let rec unguarded = function Flow_ir.Packed.Guarded (_, i) -> unguarded i | i -> i in
  if
    Array.exists
      (fun i ->
        match unguarded i with Flow_ir.Packed.Frame n | Int_frame n -> n = name | _ -> false)
      stage.program.Flow_ir.Core.Region.code
  then [ argument ]
  else []

let buffer_arguments stage =
  List.map
    (fun (name, ty) ->
      let slot = List.assoc name stage.bindings in
      Printf.sprintf "device const %s* buffer%d [[buffer(%d)]]"
        (if ty = Flow.Ty.Array Int then "uint" else "float")
        slot slot)
    (List.filter is_buffer stage.params)

(* A vertex entry's first parameter is the vertex id; its result has [:position] (clip space) and the
   varyings, those under [:flat] not interpolated. A fragment entry's elements are varyings by name
   and its result is the colour, or [{:color :discard}]. *)
let msl ?(arguments = false) ~declarations ~vertex ~fragment () =
  let* v = compile ~declarations vertex in
  let* f = compile ~shared:v.bindings ~declarations fragment in
  (* the discard test after the colour stores: measured faster on Apple GPUs than before them *)
  let last =
    List.partition
      (fun (leaf, _) -> leaf.path <> [ "discard" ])
      (List.combine f.leaves (Array.to_list f.program.outputs))
  in
  let* f =
    match last with
    | _, [] -> Ok f
    | keep, discard ->
        Result.map
          (fun program -> { f with leaves = List.map fst (keep @ discard); program })
          (Result.map_error List.hd
             (Flow_ir.Core.Region.rebuild f.program
                ~outputs:(Array.of_list (List.map snd (keep @ discard)))
                ()))
  in
  let out = identifier vertex ^ "_out" in
  let name leaf = identifier (List.nth leaf.path (List.length leaf.path - 1)) in
  let* fields =
    all
      (List.map
         (fun leaf ->
           Result.map
             (fun ty ->
               if leaf.path = [ "position" ] then "  float4 position [[position]];\n"
               else if leaf.path = [ "point-size" ] then "  float point_size [[point_size]];\n"
               else
                 Printf.sprintf "  %s %s%s;\n" ty (name leaf)
                   (if leaf.flat then " [[flat]]" else ""))
             (msl_type leaf.ty))
         v.leaves)
  in
  let* vertex_body =
    body v
      ~input:(fun _ _ -> "0.0f")
      ~int_input:(fun _ -> "long(vid)")
      ~store:(fun leaf c value -> component ("o." ^ name leaf) leaf.ty c ^ "=" ^ value ^ ";")
  in
  let varyings = List.filter (fun p -> not (is_buffer p)) f.params in
  let input k c =
    let name, ty = List.nth varyings k in
    component ("in." ^ identifier name) ty c
  in
  let* fragment_body =
    body f ~input
      ~int_input:(fun _ -> "0")
      ~store:(fun leaf c value ->
        if leaf.path = [ "discard" ] then "if(" ^ value ^ "!=0.0f) discard_fragment();"
        else component "colour" Flow.Ty.Vec4 c ^ "=" ^ value ^ ";")
  in
  let signature stage extra = String.concat ",\n    " (extra @ buffer_arguments stage) in
  Ok
    (String.concat ""
       ([
          "#include <metal_stdlib>\nusing namespace metal;\n";
          (if arguments then
             "struct raster_texture { texture2d<float, access::sample> image [[id(0)]]; sampler \
              sampling [[id(1)]]; };\n"
           else "");
          "struct ";
          out;
          " {\n";
        ]
       @ fields
       @ [
           "};\n";
           Printf.sprintf "vertex %s %s(\n    %s) {\n  %s o;\n" out (identifier vertex)
             (signature v
                ("uint vid [[vertex_id]]" :: builtin v "gpu/instance-id" "uint iid [[instance_id]]"))
             out;
           vertex_body;
           "  return o;\n}\n";
           Printf.sprintf "fragment float4 %s(\n    %s) {\n  float4 colour;\n" (identifier fragment)
             (signature f
                ((out ^ " in [[stage_in]]")
                 :: builtin f "gpu/front-facing" "bool front [[front_facing]]"
                @ texture_arguments ~arguments f));
           texture_locals ~arguments f;
           fragment_body;
           "  return colour;\n}\n";
         ]))

(* The same stages as Vulkan SPIR-V modules (entry [main]): varyings at locations in the vertex
   result's leaf order, the fragment's inputs at their names' locations. *)
let spirv ~declarations ~vertex ~fragment =
  let* v = compile ~declarations vertex in
  let* f = compile ~shared:v.bindings ~declarations fragment in
  let buffers (s : stage) =
    List.filter_map Fun.id
      (Array.to_list
         (Array.mapi
            (fun u value ->
              match value with
              | E.Int_array a -> Some (u, Array.length a - 1, true)
              | E.Float_array a -> Some (u, Array.length a - 1, false)
              | _ -> None)
            s.program.uniforms))
  in
  let varyings =
    List.filter (fun leaf -> leaf.path <> [ "position" ] && leaf.path <> [ "point-size" ]) v.leaves
  in
  let location leaf = Option.get (List.find_index (( == ) leaf) varyings) in
  let width = function Flow.Ty.Vec2 -> 2 | Vec3 -> 3 | Vec4 -> 4 | _ -> 1 in
  let* inputs =
    all
      (List.map
         (fun (name, ty) ->
           match
             List.find_opt
               (fun leaf -> List.nth leaf.path (List.length leaf.path - 1) = name)
               varyings
           with
           | Some leaf -> Ok (location leaf, width ty, leaf.flat)
           | None -> fail (fragment ^ ": no varying " ^ name))
         (List.filter (fun p -> not (is_buffer p)) f.params))
  in
  let* vertex_module =
    Spirv.stage
      {
        vertex = true;
        inputs = [||];
        buffers = buffers v;
        workgroup = None;
        outputs =
          Array.of_list
            (List.map
               (fun leaf ->
                 if leaf.path = [ "position" ] then Spirv.Position
                 else if leaf.path = [ "point-size" ] then Point_size
                 else Varying (location leaf, leaf.flat))
               v.leaves);
      }
      v.program
  in
  let* fragment_module =
    Spirv.stage
      {
        vertex = false;
        inputs = Array.of_list inputs;
        buffers = buffers f;
        workgroup = None;
        outputs =
          Array.of_list
            (List.map
               (fun leaf -> if leaf.path = [ "discard" ] then Spirv.Discard else Colour)
               f.leaves);
      }
      f.program
  in
  Ok (vertex_module, fragment_module)

(* A compute stage: one workgroup of [size] threads. The entry's first parameter is the thread's index in
   the group, its array parameters are the bound buffers, in order (a buffer it stores into is
   writable), and its last is the group size. Its result is discarded: its stores are its effect. *)
let stored (c : stage) =
  List.sort_uniq compare
    (List.filter_map
       (fun i ->
         match snd (Trace.flat [] i) with
         | Flow_ir.Packed.Stage (Store, r) -> (
             match snd (Trace.flat [] c.program.code.(r.(0))) with
             | Flow_ir.Packed.Array_prepare (u, _) -> Some (slot c.program u)
             | _ -> None)
         | _ -> None)
       (Array.to_list c.program.code))

(* the entry's last parameter is the group size, given as a constant *)
let compute_stage ~size ~declarations ~entry =
  let* forms = Flow.Syntax.parse declarations in
  let* params = parameters forms entry in
  let given = List.filteri (fun k _ -> k < List.length params - 1) params in
  compile
    ~declarations:
      (declarations
      ^ Printf.sprintf "(defn compute-entry :context value [%s] (%s %s %d))"
          (String.concat " "
             (List.map
                (fun (name, ty) ->
                  Printf.sprintf "(%s : %s)" name
                    (match ty with
                    | Flow.Ty.Array t -> "(array " ^ Flow.Ty.to_string t ^ ")"
                    | t -> Flow.Ty.to_string t))
                given))
          entry
          (String.concat " " (List.map fst given))
          size)
    "compute-entry"

let compute_msl ~size ~declarations ~entry =
  let* c = compute_stage ~size ~declarations ~entry in
  let* body =
    Emit.stage_body
      ~raster:
        {
          Emit.input = (fun _ _ -> "0.0f");
          int_input = (fun _ -> "long(lid)");
          buffer = buffer c.program;
          output = (fun _ _ value -> "(void)" ^ value ^ ";");
          builtin =
            (function
            | "gpu/group-size" -> string_of_int size
            | name -> invalid_arg ("Raster: no built-in " ^ name));
        }
      c.program
  in
  let arguments =
    List.map
      (fun (name, ty) ->
        let slot = List.assoc name c.bindings in
        Printf.sprintf "device %s%s* buffer%d [[buffer(%d)]]"
          (if List.mem slot (stored c) then "" else "const ")
          (if ty = Flow.Ty.Array Int then "uint" else "float")
          slot slot)
      (List.filter is_buffer c.params)
  in
  Ok
    (String.concat ""
       [
         "#include <metal_stdlib>\nusing namespace metal;\n";
         Printf.sprintf "kernel void %s(\n    %s) {\n" (identifier entry)
           (String.concat ",\n    " (arguments @ [ "uint lid [[thread_position_in_threadgroup]]" ]));
         body;
         "}\n";
       ])

let compute_program ~size ~declarations ~entry =
  let* c = compute_stage ~size ~declarations ~entry in
  let written = stored c in
  Ok
    ( c.program,
      List.filter_map Fun.id
        (Array.to_list
           (Array.mapi
              (fun u value ->
                match value with
                | E.Int_array a ->
                    Some (u, Array.length a - 1, true, List.mem (Array.length a - 1) written)
                | E.Float_array a ->
                    Some (u, Array.length a - 1, false, List.mem (Array.length a - 1) written)
                | _ -> None)
              c.program.uniforms)) )

let compute_spirv ~size ~declarations ~entry =
  let* program, buffers = compute_program ~size ~declarations ~entry in
  Spirv.stage
    {
      vertex = false;
      inputs = [||];
      outputs = [||];
      workgroup = Some size;
      buffers = List.map (fun (u, binding, words, _) -> (u, binding, words)) buffers;
    }
    program
