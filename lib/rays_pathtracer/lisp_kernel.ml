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

let uniforms =
  Flow.Ty.
    [
      ("eye", Vec3);
      ("fwd", Vec3);
      ("rgt", Vec3);
      ("up", Vec3);
      ("tanhalf", Float);
      ("aspect", Float);
      ("lens", Float);
      ("focus", Float);
      ("frame", Int);
      ("lo", Int);
      ("hi", Int);
      ("peye", Vec3);
      ("pfwd", Vec3);
      ("prgt", Vec3);
      ("pup", Vec3);
      ("ptan", Float);
      ("paspect", Float);
      ("pvalid", Int);
      ("pacc", Int);
      ("sky", Vec3);
      ("ground", Vec3);
      ("bg", Vec3);
      ("sundir", Vec3);
      ("suncos", Float);
      ("sunrad", Vec3);
      ("sunpdf", Float);
      ("exposure", Float);
      ("count", Float);
      ("spherebase", Int);
      ("strandbase", Int);
      ("width", Int);
      ("height", Int);
    ]

let tables =
  Flow.Ty.
    [
      ("lights", Array Vec4);
      ("pan", Array Vec4);
      ("wmap", Array Vec3);
      ("cdf", Array Float);
      ("mats", Array Vec4);
      ("ids", Array Int);
      ("inst", Array Vec3);
      ("nrm", Array Vec3);
      ("accum", Array Vec4);
      ("geo", Array Vec4);
      ("pgeo", Array Vec4);
      ("pcol", Array Vec4);
      ("order", Array Int);
    ]

let ops =
  List.map
    (fun (name, ty) ->
      {
        Flow.Op.noise3 with
        name = "pt/" ^ name;
        packed_extension = None;
        signature = { pos = []; opt = []; rest = None; kw = [] };
        out = (fun _ -> ty);
        shape = Flow.Op.Struct { splice = false };
        check = (fun _ -> ());
        body = (fun ~live:_ ~node:_ _ -> Flow.Value.Source ("pt/" ^ name, ty, []));
      })
    (uniforms @ tables)

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
  spp : int;
  bounces : int;
  round_samples : int;
  panels : int;
  lights : int;
  world : bool;
  background : int;
  map_w : int;
  map_h : int;
  sun : bool;
  bsdf_only : bool;
  instanced : bool;
  classes : bool * bool * bool;
  motion : bool;
  curves : [ `Loop | `Native ] option;
  spheres : bool;
}

let num x =
  let s = Printf.sprintf "%.17g" x in
  if String.exists (fun c -> c = '.' || c = 'e' || c = 'n') s then s else s ^ "."

(* The kernels, in two sets checked apart: an accumulation frame's, and a preview's (when the camera moves). *)
let previews = [ "preview"; "preview-geometry"; "preview-resolve" ]

let text f ~preview:set =
  let metallic, emissive, round = f.classes in
  let u ~preview =
    Printf.sprintf
      "{:eye eye :lens %s :fwd fwd :rgt rgt :up up :tan-half tanhalf :aspect aspect :focus focus \
       :width width :height height :spp %d :bounces %d :frame frame :sky sky :ground ground \
       :panels %d :lights %d :world %d :background %d :map-w %d :map-h %d :bg bg :sun-dir sundir \
       :sun-cos suncos :sun-rad sunrad :sun-pdf %s :round-samples %d :preview %b :bsdf-only %b \
       :instanced %b :sphere-base spherebase :metallic %b :emissive %b :round %b :motion %b \
       :inst-rows %d :strand-base strandbase}"
      (if preview then "0." else "lens")
      (if preview then 1 else f.spp)
      (if preview then 1 else f.bounces)
      f.panels f.lights (Bool.to_int f.world) f.background f.map_w f.map_h
      (if f.sun then "sunpdf" else "0.")
      (if preview then 1 else f.round_samples)
      preview f.bsdf_only f.instanced metallic emissive round f.motion
      (if f.motion then 7 else 4)
  in
  let args = "lights mats ids inst nrm pan wmap cdf" and clear = f.world && f.background = 2 in
  (* the records are built inside each kernel, whose captures are then the placeholders themselves *)
  let kernel ?(params = "(pixel : int)") name body =
    Printf.sprintf
      "%s (fn [%s] (let* [u %s v %s pc {:eye peye :fwd pfwd :rgt prgt :up pup :tan-half ptan \
       :aspect paspect :valid pvalid :from-accum pacc}] %s))"
      name params (u ~preview:false) (u ~preview:true) body
  in
  Printf.sprintf "(workspace pt %s (graph g :context value (let* [%s %s] 0.0)))"
    Pathtrace_lisp.source
    (String.concat " "
       (List.map (fun (name, _) -> Printf.sprintf "%s (pt/%s)" name name) (uniforms @ tables)))
    (String.concat " "
       (if set then
          [
            kernel "preview"
              (Printf.sprintf "(preview-pixel v pc %b %s geo pgeo pcol order pixel)" clear args);
            kernel "preview-geometry" "(primary-geo v pixel)";
            kernel "preview-resolve"
              (Printf.sprintf "(resolve-pixel pcol geo order pixel width height exposure %b)" clear);
          ]
        else
          [
            kernel ~params:"(pixel : int) (slot : int)" "trace"
              (Printf.sprintf "(accumulate-pixel u lo hi %b %s accum slot pixel)" clear args);
            kernel "geometry" "(primary-geo u pixel)";
            kernel "resolve"
              (Printf.sprintf
                 "(resolve-accum (array/nth accum (array/nth order pixel)) exposure %b count)" clear);
          ]))

type kernel = {
  runner : Flow_gpu.Run.t;
  msl : Flow_gpu.Emit.msl;
  base : Flow_ir.Packed.Host.inputs;
  slots : (string * Flow.Ty.t) option array; (* the placeholder each uniform reads *)
}

type t = {
  facts : facts;
  mutable fns : (string * E.fn) list;
  values : (string * (Flow.Ty.t * E.value)) list;
  pixels : E.value;
  mutable kernels : (string * kernel) list;
}

let diagnostics ds = String.concat "\n" (List.map Diagnostic.to_string ds)

(* One set's kernel functions. The declarations are checked whole once, by a test ([check_library]); a set
   types them only at its kernels' call sites, which is all it reaches. *)
let functions f ~preview =
  match Flow.Syntax.parse (text f ~preview) with
  | Error d -> Error (Diagnostic.to_string d)
  | Ok syntax -> (
      match
        Flow.Workspace.check ~ops ~call_sites:true { Flow.Catalog.version = 1; kinds = [] } syntax
      with
      | None, ds | Some _, (_ :: _ as ds) -> Error (diagnostics ds)
      | Some ws, [] ->
          let* evaluated =
            Result.map_error
              (fun d -> "path tracer kernels: " ^ Diagnostic.to_string d)
              (E.static ~record:true ws)
          in
          let fn name =
            match List.assoc_opt [ "g"; name ] evaluated.records with
            | Some ((_, E.Fn fn) :: _) -> Ok (name, fn)
            | _ -> Error ("path tracer kernel " ^ name ^ " is not a function")
          in
          List.fold_right
            (fun name acc ->
              let* acc = acc in
              let* f = fn name in
              Ok (f :: acc))
            (if preview then previews else [ "trace"; "geometry"; "resolve" ])
            (Ok []))

(* A set's functions on the first use of one of its kernels that the disk cache does not hold. *)
let compile f =
  Ok
    {
      facts = f;
      fns = [];
      values = List.map (fun (name, ty) -> (name, (ty, placeholder ty))) (uniforms @ tables);
      pixels = E.Int_array [| 0 |];
      kernels = [];
    }

let fn t name =
  let* () =
    if List.mem_assoc name t.fns then Ok ()
    else
      Result.map
        (fun fns -> t.fns <- t.fns @ fns)
        (functions t.facts ~preview:(List.mem name previews))
  in
  Option.to_result ~none:("no path tracer kernel " ^ name) (List.assoc_opt name t.fns)

let check_library () =
  match Flow.Syntax.parse ("(workspace pt " ^ Pathtrace_lisp.source ^ ")") with
  | Error d -> Error (Diagnostic.to_string d)
  | Ok syntax -> (
      match Flow.Workspace.check ~library:true { Flow.Catalog.version = 1; kinds = [] } syntax with
      | Some _, [] -> Ok ()
      | _, ds -> Error (diagnostics ds))

(* The check of pathtracer.rays's accumulation set, alone: tools/bench_check times it. *)
let check_sketch () =
  let f =
    {
      spp = 1;
      bounces = 5;
      round_samples = 4;
      panels = 0;
      lights = 2;
      world = true;
      background = 0;
      map_w = 512;
      map_h = 256;
      sun = false;
      bsdf_only = false;
      instanced = true;
      classes = (false, false, true);
      motion = false;
      curves = None;
      spheres = false;
    }
  in
  match Flow.Syntax.parse (text f ~preview:false) with
  | Error d -> Error (Diagnostic.to_string d)
  | Ok syntax -> (
      match
        Flow.Workspace.check ~ops ~call_sites:true { Flow.Catalog.version = 1; kinds = [] } syntax
      with
      | Some _, [] -> Ok ()
      | _, ds -> Error (diagnostics ds))

(* The front end's result for one kernel: its MSL, its inputs, and the placeholder each uniform reads. *)
type front = Flow_gpu.Emit.msl * Flow_ir.Packed.Host.inputs * (string * Flow.Ty.t) option array

let front t name =
  let* fn = fn t name in
  let bound =
    Flow.Eval.Host.map_function_bindings
      (function
        | Flow.Value.Source (n, _, []) when String.starts_with ~prefix:"pt/" n ->
            snd (List.assoc (String.sub n 3 (String.length n - 3)) t.values)
        | value -> value)
      fn
  in
  let inputs = if name = "trace" then [ t.pixels; t.pixels ] else [ t.pixels ] in
  let* residual =
    match
      Flow.Eval.Host.map_function
        ~signature:{ params = List.map (fun _ -> Flow.Ty.Int) inputs; result = Vec4 }
        bound inputs
    with
    | Ok (E.Residual r) -> Ok r
    | Ok _ -> Error "path tracer kernel did not defer"
    | Error d -> Error (name ^ ": " ^ Diagnostic.to_string d)
  in
  let* template =
    Option.to_result
      ~none:("path tracer kernel " ^ name ^ " has no device template")
      (Packed.compile_template residual (E.Residual.residual_view residual).term)
  in
  let* msl =
    Result.map_error
      (fun d -> name ^ ": " ^ Diagnostic.to_string d)
      (Flow_gpu.Emit.kernel ~fast:true ~instanced:t.facts.instanced ~motion:t.facts.motion
         ?curves:t.facts.curves ~spheres:t.facts.spheres template)
  in
  let* base =
    Result.map_error
      (fun d -> name ^ ": " ^ Diagnostic.to_string d)
      (Flow_ir.Packed.Host.prepare ~fast:true template ~live:(Input.at_time 0.))
  in
  let slots =
    Array.map
      (function
        | Flow_ir.Packed.Host.Value v ->
            Option.map
              (fun (n, (ty, _)) -> (n, ty))
              (List.find_opt (fun (_, (_, p)) -> p == v) t.values)
        | _ -> None)
      (Flow_ir.Packed.Host.uniform_shapes template)
  in
  Ok ((msl, base, slots) : front)

(* A later process of the same executable skips the front end (the old handwritten kernel had none): each
   kernel's result is a file under [cache_dir] (the user cache directory; None disables it), named by
   the digest of the executable's identity, the kernel's text and the scene facts (the emitter reads the
   curve and sphere facts outside the text), holding a digest of its marshalled payload.
   A torn or unreadable entry is recompiled and rewritten; past [cache_files] the oldest are removed. *)
let cache_files = 64

let cache_dir =
  ref
    (Option.map
       (fun home -> Filename.concat home "Library/Caches/rays/kernels")
       (Sys.getenv_opt "HOME"))

let executable =
  lazy
    (match Unix.stat Sys.executable_name with
    | s -> Some (Printf.sprintf "%s\000%d\000%.9f" Sys.executable_name s.st_size s.st_mtime)
    | exception Unix.Unix_error _ -> None)

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Sys.mkdir dir 0o755 with Sys_error _ -> ()
  end

let store dir file (value : front) =
  mkdir_p dir;
  let payload = Marshal.to_string value [] in
  let temp = Filename.temp_file ~temp_dir:dir "kernel" ".part" in
  Out_channel.with_open_bin temp (fun out ->
      output_string out (Digest.string payload);
      output_string out payload);
  Sys.rename temp file;
  let entries =
    Array.map
      (fun f ->
        let f = Filename.concat dir f in
        ((Unix.stat f).st_mtime, f))
      (Sys.readdir dir)
  in
  Array.sort compare entries;
  Array.iteri (fun i (_, f) -> if i < Array.length entries - cache_files then Sys.remove f) entries

let cached t name =
  match (!cache_dir, Lazy.force executable) with
  | None, _ | _, None -> front t name
  | Some dir, Some executable -> (
      let text = text t.facts ~preview:(List.mem name previews) in
      let file =
        Filename.concat dir
          (Digest.to_hex
             (Digest.string
                (String.concat "\000" [ executable; name; text; Marshal.to_string t.facts [] ])))
      in
      match In_channel.with_open_bin file In_channel.input_all with
      | data
        when String.length data > 16
             && String.sub data 0 16 = Digest.substring data 16 (String.length data - 16) ->
          Ok (Marshal.from_string data 16 : front)
      | _ | (exception Sys_error _) ->
          let* value = front t name in
          (try store dir file value with Sys_error _ | Unix.Unix_error _ -> ());
          Ok value)

let kernel t ~lease ~pipelines name =
  match List.assoc_opt name t.kernels with
  | Some k -> Ok k
  | None ->
      let* msl, base, slots = cached t name in
      let k = { runner = Flow_gpu.Run.create lease pipelines msl; msl; base; slots } in
      t.kernels <- (name, k) :: t.kernels;
      Ok k

(* Encodes kernel [name] over the film's [count] pixels into [commands], writing [into]: [words n] is uniform [n]'s current
   value and [table n] the buffer and element count bound for table [n]. *)
let dispatch t ~lease ~pipelines ~commands ~count ~pixels ?slots ~cell ~into ?scene ?extras
    ?resident ~words ~table name =
  let* k = kernel t ~lease ~pipelines name in
  let uniforms =
    Array.mapi
      (fun u old ->
        match k.slots.(u) with
        | Some (n, ty) when match ty with Flow.Ty.Array _ -> false | _ -> true -> words n
        | _ -> old)
      k.base.uniforms
  in
  let inputs = { k.base with uniforms; arrays = Array.map (fun _ -> [||]) k.base.arrays; count } in
  Result.map_error Diagnostic.to_string
    (Result.map ignore
       (Flow_gpu.Run.dispatch ~commands ~reset_status:false ~into:(into, cell) ?scene ?extras
          ?resident
          ~bound:(fun i -> if i = 0 then Some pixels else slots)
          ~bound_table:(fun u ->
            match k.slots.(u) with
            | Some (n, (Flow.Ty.Array _ as ty)) ->
                let buffer, count = table n in
                Some (buffer, count * width ty)
            | _ -> None)
          k.runner inputs))

let max_total_threads t ~lease ~pipelines name =
  let* k = kernel t ~lease ~pipelines name in
  Result.map_error Diagnostic.to_string (Flow_gpu.Run.max_total_threads k.runner)

let source t ~lease ~pipelines name =
  let* k = kernel t ~lease ~pipelines name in
  Ok k.msl.Flow_gpu.Emit.source

let close t =
  List.iter (fun (_, k) -> Flow_gpu.Run.close k.runner) t.kernels;
  t.kernels <- []

(* The CPU walker's run of kernel [name] over [pixels] (the oracle a device film is checked against): [words n]
   gives uniform [n] and [value n] table [n] as host values. *)
let cpu t ~words ~value ~pixels name =
  let* fn = fn t name in
  let bound =
    Flow.Eval.Host.map_function_bindings
      (function
        | Flow.Value.Source (n, _, []) when String.starts_with ~prefix:"pt/" n -> (
            let n = String.sub n 3 (String.length n - 3) in
            match fst (List.assoc n t.values) with
            | Flow.Ty.Array _ -> value n
            | Vec3 ->
                let w = words n in
                E.Vec3 (w.(0), w.(1), w.(2))
            | Int -> E.Int (int_of_float (words n).(0))
            | _ -> E.Float (words n).(0))
        | value -> value)
      fn
  in
  let inputs = if name = "trace" then [ pixels; pixels ] else [ pixels ] in
  match
    Flow.Eval.Host.map_function
      ~signature:{ params = List.map (fun _ -> Flow.Ty.Int) inputs; result = Vec4 }
      bound inputs
  with
  | Error d -> Error (Diagnostic.to_string d)
  | Ok (E.Residual r) ->
      Result.map_error diagnostics
        (let* program = Packed.compile_result r (E.Residual.residual_view r).term in
         Result.map_error (fun d -> [ d ]) (Packed.force program ~live:(Input.at_time 0.)))
  | Ok _ -> Error "path tracer kernel did not defer"
