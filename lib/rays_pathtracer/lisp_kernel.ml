(* The tracer's Lisp kernels (pathtrace.rays) for one scene shape: the scene's facts (sizes, counts, flags) are
   literals the static stage folds, everything else is a host placeholder bound per dispatch. A scalar or
   vector placeholder becomes a uniform whose words a dispatch rewrites; an array placeholder becomes a gather
   table read from a device buffer the tracer owns (never uploaded by the runner). Each kernel maps the film's
   pixel indices (a device buffer of [0, n)) to one vec4 written into the caller's buffer, so a frame encodes
   every dispatch into its own command buffer without waiting. *)
module E = Flow.Eval
module B = Ogpu.Backend
module Packed = Flow_ir.Packed

let ( let* ) = Result.bind

let uniforms = Flow.Ty.[
  "eye", Vec3; "fwd", Vec3; "rgt", Vec3; "up", Vec3; "tanhalf", Float; "aspect", Float; "lens", Float;
  "focus", Float; "frame", Int; "lo", Int; "hi", Int; "peye", Vec3; "pfwd", Vec3; "prgt", Vec3; "pup", Vec3;
  "ptan", Float; "paspect", Float; "pvalid", Int; "pacc", Int; "sky", Vec3; "ground", Vec3; "bg", Vec3;
  "sundir", Vec3; "suncos", Float; "sunrad", Vec3; "sunpdf", Float; "exposure", Float; "count", Float;
  "spherebase", Int; "width", Int; "height", Int ]

let tables = Flow.Ty.[
  "lights", Array Vec4; "pan", Array Vec4; "wmap", Array Vec3; "cdf", Array Float; "mats", Array Vec4;
  "ids", Array Int; "inst", Array Vec3; "nrm", Array Vec3; "accum", Array Vec4; "geo", Array Vec4;
  "pgeo", Array Vec4; "pcol", Array Vec4 ]

let ops = List.map (fun (name, ty) ->
  { Flow.Op.noise3 with name = "pt/" ^ name; packed_extension = None;
    signature = { pos = []; opt = []; rest = None; kw = [] }; out = (fun _ -> ty);
    shape = Flow.Op.Struct { splice = false }; check = (fun _ -> ());
    body = (fun ~live:_ ~node:_ _ -> Flow.Value.Source ("pt/" ^ name, ty, [])) }) (uniforms @ tables)

(* Fresh blocks: a placeholder is found again in a template's uniforms by physical equality. *)
let placeholder = function
  | Flow.Ty.Vec3 -> E.Vec3 (Sys.opaque_identity 0., 0., 0.)
  | Float -> E.Float (Sys.opaque_identity 0.)
  | Int -> E.Int (Sys.opaque_identity 0)
  | Array Vec4 -> E.Vec4_array (Array.make 4 0.)
  | Array Vec3 -> E.Vec3_array (Array.make 3 0.)
  | Array Int -> E.Int_array (Array.make 1 0)
  | _ -> E.Float_array (Array.make 1 0.)
let width = function Flow.Ty.Array Vec4 -> 4 | Array Vec3 -> 3 | _ -> 1

(* The scene facts a kernel set is specialized on; equal facts share one compiled set. *)
type facts = {
  spp : int; bounces : int; round_samples : int; panels : int; lights : int;
  world : bool; background : int; map_w : int; map_h : int; sun : bool; bsdf_only : bool;
  instanced : bool }

let num x = let s = Printf.sprintf "%.17g" x in
  if String.exists (fun c -> c = '.' || c = 'e' || c = 'n') s then s else s ^ "."

let text f =
  let u ~preview = Printf.sprintf
    "{:eye eye :lens %s :fwd fwd :rgt rgt :up up :tan-half tanhalf :aspect aspect :focus focus :width width \
     :height height :spp %d :bounces %d :frame frame :sky sky :ground ground :panels %d :lights %d :world %d \
     :background %d :map-w %d :map-h %d :bg bg :sun-dir sundir :sun-cos suncos :sun-rad sunrad :sun-pdf %s \
     :round-samples %d :preview %b :bsdf-only %b :instanced %b :sphere-base spherebase}"
    (if preview then "0." else "lens") (if preview then 1 else f.spp)
    (if preview then 1 else f.bounces) f.panels f.lights (Bool.to_int f.world) f.background f.map_w f.map_h
    (if f.sun then "sunpdf" else "0.") (if preview then 1 else f.round_samples) preview f.bsdf_only f.instanced in
  let args = "lights mats ids inst nrm pan wmap cdf" and clear = f.world && f.background = 2 in
  (* the records are built inside each kernel, whose captures are then the placeholders themselves *)
  let kernel name body = Printf.sprintf "%s (fn [(pixel : int)] (let* [u %s v %s \
    pc {:eye peye :fwd pfwd :rgt prgt :up pup :tan-half ptan :aspect paspect :valid pvalid :from-accum pacc}] %s))"
    name (u ~preview:false) (u ~preview:true) body in
  Printf.sprintf "(workspace pt %s (graph g :context value (let* [%s %s] 0.0)))"
    Pathtrace_lisp.source
    (String.concat " " (List.map (fun (name, _) -> Printf.sprintf "%s (pt/%s)" name name) (uniforms @ tables)))
    (String.concat " " [
      kernel "trace" (Printf.sprintf "(accumulate-pixel u lo hi %b %s accum pixel)" clear args);
      kernel "preview" (Printf.sprintf "(preview-pixel v pc %b %s geo pgeo pcol pixel)" clear args);
      kernel "geometry" "(primary-geo u pixel)";
      kernel "preview-geometry" "(primary-geo v pixel)";
      kernel "resolve" (Printf.sprintf "(resolve-accum (array/nth accum pixel) exposure %b count)" clear);
      kernel "preview-resolve" (Printf.sprintf "(resolve-pixel pcol geo pixel width height exposure %b)" clear) ])

type kernel = {
  runner : Flow_gpu.Run.t;
  msl : Flow_gpu.Emit.msl;
  base : Flow_ir.Packed.Private.inputs;
  slots : (string * Flow.Ty.t) option array;  (* the placeholder each uniform reads *)
  zeros : float array array;  (* a bound table's length stand-in: its gather clamp reads the array length *)
  mutable indices : float array;  (* the pixel input's length stand-in; the device buffer holds the indices *)
}

type t = {
  fns : (string * E.fn) list;
  values : (string * (Flow.Ty.t * E.value)) list;
  pixels : E.value;
  instanced : bool;
  mutable kernels : (string * kernel) list;
}

let diagnostics ds = String.concat "\n" (List.map Diagnostic.to_string ds)

let compile f =
  match Flow.Syntax.parse (text f) with
  | Error d -> Error (Diagnostic.to_string d)
  | Ok syntax ->
  match Flow.Workspace.check ~ops { Flow.Check.version = 1; kinds = [] } syntax with
  | None, ds | Some _, (_ :: _ as ds) -> Error (diagnostics ds)
  | Some ws, [] ->
  let* evaluated = Result.map_error (fun d -> "path tracer kernels: " ^ Diagnostic.to_string d) (E.static ~record:true ws) in
  let fn name = match List.assoc_opt [ "g"; name ] evaluated.records with
    | Some ((_, E.Fn fn) :: _) -> Ok (name, fn) | _ -> Error ("path tracer kernel " ^ name ^ " is not a function") in
  let* fns = List.fold_right (fun name acc -> let* acc = acc in let* f = fn name in Ok (f :: acc))
    [ "trace"; "preview"; "geometry"; "preview-geometry"; "resolve"; "preview-resolve" ] (Ok []) in
  Ok { fns; values = List.map (fun (name, ty) -> name, (ty, placeholder ty)) (uniforms @ tables);
       pixels = E.Int_array [| 0 |]; instanced = f.instanced; kernels = [] }

let kernel t ~lease ~pipelines name =
  match List.assoc_opt name t.kernels with
  | Some k -> Ok k
  | None ->
      let bound = Flow.Eval.Private.map_function_bindings (function
        | Flow.Value.Source (n, _, []) when String.starts_with ~prefix:"pt/" n ->
            snd (List.assoc (String.sub n 3 (String.length n - 3)) t.values)
        | value -> value) (List.assoc name t.fns) in
      let* residual = match Flow.Eval.Private.map_function ~signature:{ params = [ Int ]; result = Vec4 } bound [ t.pixels ] with
        | Ok (E.Residual r) -> Ok r | Ok _ -> Error "path tracer kernel did not defer"
        | Error d -> Error (name ^ ": " ^ Diagnostic.to_string d) in
      let* template = Option.to_result ~none:("path tracer kernel " ^ name ^ " has no device template")
          (Packed.compile_template residual (E.Residual.residual_view residual).term) in
      let* msl = Result.map_error (fun d -> name ^ ": " ^ Diagnostic.to_string d) (Flow_gpu.Emit.kernel ~fast:true ~instanced:t.instanced template) in
      let* base = Result.map_error (fun d -> name ^ ": " ^ Diagnostic.to_string d) (Flow_ir.Packed.Private.prepare ~fast:true template ~live:(Input.at_time 0.)) in
      let slots = Array.map (function
        | Flow_ir.Packed.Private.Value v ->
            Option.map (fun (n, (ty, _)) -> n, ty) (List.find_opt (fun (_, (_, p)) -> p == v) t.values)
        | _ -> None) (Flow_ir.Packed.Private.uniform_shapes template) in
      Option.iter (fun d -> Out_channel.with_open_bin (d ^ "/" ^ name ^ ".metal") (fun o -> output_string o msl.Flow_gpu.Emit.source)) (Sys.getenv_opt "PT_DUMP_DIR");
      let k = { runner = Flow_gpu.Run.create lease pipelines msl; msl; base; slots; zeros = Array.copy base.tables;
                indices = [||] } in
      t.kernels <- (name, k) :: t.kernels;
      Ok k

(* Encodes kernel [name] over the film's [count] pixels into [commands], writing [into]: [words n] is uniform [n]'s current
   value and [table n] the buffer and element count bound for table [n]. *)
let dispatch t ~lease ~pipelines ~commands ~count ~pixels ~cell ~into ?scene ?resident ~words ~table name =
  let* k = kernel t ~lease ~pipelines name in
  if Array.length k.indices <> count then k.indices <- Array.make count 0.;
  let tables = Array.mapi (fun u zeros -> match k.slots.(u) with
    | Some (n, (Flow.Ty.Array _ as ty)) ->
        let length = snd (table n) * width ty in
        if Array.length zeros <> length then k.zeros.(u) <- Array.make length 0.;
        k.zeros.(u)
    | _ -> zeros) k.zeros in
  let uniforms = Array.mapi (fun u old -> match k.slots.(u) with
    | Some (n, ty) when (match ty with Flow.Ty.Array _ -> false | _ -> true) -> words n
    | _ -> old) k.base.uniforms in
  let inputs = { k.base with uniforms; tables; arrays = [| k.indices |]; count } in
  Result.map_error Diagnostic.to_string
    (Result.map ignore (Flow_gpu.Run.dispatch ~commands ~reset_status:false ~into:(into, cell) ?scene ?resident
       ~bound:(fun i -> if i = 0 then Some pixels else None)
       ~bound_table:(fun u -> match k.slots.(u) with Some (n, Flow.Ty.Array _) -> Some (fst (table n)) | _ -> None)
       k.runner inputs))

let max_total_threads t ~lease ~pipelines name =
  let* k = kernel t ~lease ~pipelines name in
  Result.map_error Diagnostic.to_string (Flow_gpu.Run.max_total_threads k.runner)

let source t ~lease ~pipelines name =
  let* k = kernel t ~lease ~pipelines name in
  Ok k.msl.Flow_gpu.Emit.source

let close t = List.iter (fun (_, k) -> Flow_gpu.Run.close k.runner) t.kernels; t.kernels <- []
