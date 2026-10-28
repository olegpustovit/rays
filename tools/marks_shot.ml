(* The native picture of a draw workspace as one PNG: the oracle of the browser presenter (plan P6.4, P6.5).
   Runs the core executor for FRAME fixed-step frames (dt 1/60, pointer (100, 100): the frames of
   web_parity.exe and tools/web/test/frames.mjs), adapts the last frame's mark table with Marks_scene and renders it
   offscreen at density 1. Image maps use native Image_kernel snapshots; --gpu-images uses the native
   Run/Image_sink display path as the oracle for discontinuous f32 pixel functions.
   usage: marks_shot FILE.rays OUT.png WIDTH HEIGHT FRAME [--gpu-images] *)
module E = Flow.Eval
module C = Flow_ir.Core_exec

let () =
  if Array.length Sys.argv < 6 || Array.length Sys.argv > 7
    || (Array.length Sys.argv = 7 && Sys.argv.(6) <> "--gpu-images") then
    (prerr_endline "usage: marks_shot FILE.rays OUT.png WIDTH HEIGHT FRAME [--gpu-images]"; exit 2);
  (* RAYS_SHOT_POINTER=X,Y moves the pointer, or X,Y;X,Y;... one per frame, the last held (the browser
     parity check passes the same) *)
  let path = match Sys.getenv_opt "RAYS_SHOT_POINTER" with
    | Some text -> Array.of_list (List.map (fun p -> Scanf.sscanf p "%f,%f" (fun x y -> x, y)) (String.split_on_char ';' text))
    | None -> [|100., 100.|] in
  let pointer_at i = path.(min (i - 1) (Array.length path - 1)) in
  let gpu_images = Array.length Sys.argv = 7 && Sys.argv.(6) = "--gpu-images" in
  let file = Sys.argv.(1) and output = Sys.argv.(2) in
  let width = int_of_string Sys.argv.(3) and height = int_of_string Sys.argv.(4) and frames = int_of_string Sys.argv.(5) in
  let size = width, height and dt = 1. /. 60. in
  let fail text = prerr_endline text; exit 1 in
  let _, doc = Web_export.load file in
  let lowered = match doc with
    | Error (d :: _) -> fail (Diagnostic.to_string d) | Error [] -> fail "does not check"
    | Ok doc -> (match Web_export.lower doc with Ok l -> l | Error d -> fail (Diagnostic.to_string d)) in
  (* RAYS_SHOT_FAST=1: fast precision's native path, a drawing's resident folds on the Metal host, the
     oracle of the browser's resident folds (both f32 on the device; exact CPU elsewhere) *)
  let fast = Sys.getenv_opt "RAYS_SHOT_FAST" = Some "1" in
  let prepared = C.prepare ~remember_loops:fast lowered.program and state = E.create_state () and buffers = C.Buffers.create () in
  let images = Hashtbl.create 8 in
  let get = function Ok v -> v | Error d -> fail (Diagnostic.to_string d) in
  let gpu = if gpu_images || fast then Some (match Renderer.acquire_gpu () with Ok g -> g
    | Error e -> fail (Format.asprintf "%a" Renderer.pp_error e)) else None in
  let pipelines = Option.map (fun g -> Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday (Renderer.gpu_device g)) gpu in
  let host = if fast then Option.map (fun g -> Flow_gpu.Host.create ~clock:Unix.gettimeofday g) gpu else None in
  let gpu_config=match host with
    |None->Flow_ir.Gpu.default
    |Some host->{Flow_ir.Gpu.default with backend=Some(Flow_gpu.Host.backend host);fast=true} in
  let resident () = Option.map (Flow_gpu.Host.resident ~fast:true) host in
  let current = ref (Input.at_time 0.) in
  let rec resolve id = match Hashtbl.find_opt images id with Some image -> image | None ->
    let live = !current in
    let node = lowered.plan.nodes.(id - 1) in
    let args = List.map (fun (k, v) -> k, get (E.force ~state v ~live)) node.args in
    let int name fallback = match List.assoc_opt name args with
      | Some (E.Int n) -> n
      | Some (E.Float n) when Float.is_finite n && Float.is_integer n
          && n >= float min_int && n < float max_int -> int_of_float n
      | None -> fallback
      | _ -> fail ("Image " ^ name ^ " must be an integer.") in
    let message = function Ok v -> v | Error e -> fail e in
    let image = match node.kind with
    | "image/load" ->
        let path = match List.assoc "path" args with E.Text p -> p | _ -> fail "Image path is text." in
        message (Rays.Image.load path)
    | "image/render" ->
        let width = int "width" (fst live.size) and height = int "height" (snd live.size) in
        let drawing = get (Flow_native.Drawing.prepare ~gpu:gpu_config lowered.plan (List.assoc "drawing" args)) in
        let scene = get (Flow_native.Drawing.render_prepared ~state
          ~image:(function E.Deferred (_, child) -> Ok (resolve (child + 1)) | _ -> Error (Diagnostic.error ~code:"E_IMAGE" "Expected image."))
          drawing ~live ~size:(width,height)) in
        let canvas = Rays.Canvas.create_exn ~width ~height in
        Fun.protect ~finally:(fun () -> ignore(Rays.Canvas.destroy canvas)) (fun () ->
          message (Result.bind (Rays.Canvas.render canvas scene) (fun () ->
            Rays.Canvas.to_image canvas)))
    | "image/noise" ->
        let context = message (Sop.Context.create ~input:live ()) in
        let factory = Sop.Image_nodes.noise_factory in
        let source = Sop.Edit_graph.instantiate factory [] |> message in
        let changes = Sop.Edit_graph.factory_fields factory |> List.filter_map (fun (field : Param.field_view) ->
          Option.map (fun value -> field.name, match field.kind with
            | Integer_view _ -> Param.Int_value (int field.name 0)
            | Floating_view _ -> Param.Float_value (match value with E.Float f -> f | E.Int n -> float n | _ -> assert false)
            | _ -> assert false) (List.assoc_opt field.name args)) in
        let source, _ = Sop.Node.apply_parameters source changes |> message in
        let cooked = match Sop.Node.Private.cook source context [||] with Ok v -> v | Error d -> fail d.Diagnostic.message in
        let pixels = match Sop.Payload.image cooked.payload with Ok v -> v | Error d -> fail d.Diagnostic.message in
        let width = Sop.Image.width pixels and height = Sop.Image.height pixels in
        let rgba = match Sop.Image.rgba8 pixels with Some b -> b | None ->
          let floats = Sop.Image.storage pixels in
          Bytes.init (Array.length floats) (fun i -> let q = floats.(i) *. 255. in
            let n = int_of_float (Float.floor q) in let f = q -. float n in
            Char.chr (if f > 0.5 || (f = 0.5 && n land 1 = 1) then n+1 else n)) in
        message (Rays.Image.upload_rgba ~width ~height ~rgba ())
    | "image/map" ->
    let width = int "width" 256 and height = int "height" 256 in
    let fn = match List.assoc "function" args with E.Fn fn -> fn | _ -> fail "Image needs a pixel function." in
    let kernel = get (Flow_sop.Image_kernel.prepare ~identity:0 ~width ~height ~fn ~sources:[] []) in
    let rgba = match gpu, pipelines with
      | Some gpu, Some pipelines ->
          let packed = match Flow_ir.Executor.packed_region (Flow_sop.Image_kernel.program kernel) with
            | Some packed -> packed | None -> fail "Image has no packed kernel." in
          let run = Flow_gpu.Run.create gpu pipelines (get (Flow_gpu.Emit.kernel packed)) in
          let sink = get (Flow_gpu.Image_sink.create gpu) in
          Fun.protect ~finally:(fun () -> Flow_gpu.Image_sink.close sink; Flow_gpu.Run.close run) (fun () ->
            let inputs = get (Flow_ir.Packed.Host.prepare ~state packed ~live) in
            let output = get (Flow_gpu.Run.dispatch run inputs) in
            let image = get (Flow_gpu.Image_sink.convert sink ~width ~height output) in
            match Ogpu.Backend.read_texture (Option.get (Flow_gpu.Image_sink.texture image)) ~bytes_per_row:(width * 4) with
            | Ok b -> b | Error e -> fail (Ogpu.Error.to_string e))
      | _ ->
          let context = match Sop.Context.create ~input:live () with Ok c -> c | Error e -> fail e in
          let cooked = match Sop.Node.Private.cook (Flow_sop.Image_kernel.node ~state kernel) context [||] with
            | Ok c -> c | Error d -> fail d.Diagnostic.message in
          let pixels = match Sop.Payload.image cooked.payload with Ok p -> p | Error d -> fail d.Diagnostic.message in
          Option.get (Sop.Image.rgba8 pixels) in
    message (Rays.Image.upload_rgba ~width ~height ~rgba ())
    | kind -> fail ("Unknown image: " ^ kind) in
    Hashtbl.add images id image; image in
  let image = function E.Deferred (_, id) -> ignore (resolve (id + 1)); Ok (id + 1)
    | _ -> Error (Diagnostic.error ~code:"E_IMAGE" "Expected an image plan node.") in
  let last = ref None in
  for i = 1 to frames do
    Hashtbl.iter (fun _ image -> ignore(Rays.Image.destroy image)) images; Hashtbl.clear images;
    let live = { (Input.at_time (float i *. dt)) with dt; frame = i; tick = i; size; pointer = pointer_at i } in
    current := live; C.Buffers.clear buffers;
    let drawn = ref (Error (Diagnostic.error ~code:"E_CORE" "The draw program did not run.")) in
    (match lowered.root with
     | None -> drawn := C.draw ?resident:(resident ()) ~image prepared ~live ~size ~buffers
     | Some root ->
         ignore (Result.get_ok (E.Residual.bind_live ~state root ~live));
         drawn := C.draw ~root ?resident:(resident ()) ~image prepared ~live ~size ~buffers);
    last := Some !drawn
  done;
  let marks = match !last with Some (Ok m) -> m | Some (Error d) -> fail (Diagnostic.to_string d) | None -> fail "no frames" in
  let scene = match Flow_native.Marks_scene.scene ~buffers ~image:resolve marks with Ok s -> s | Error d -> fail (Diagnostic.to_string d) in
  let canvas = Rays.Canvas.create_exn ~width ~height in
  (match Rays.Canvas.render canvas scene with Ok () -> () | Error message -> fail message);
  (match Rays.Canvas.save_png canvas output with Ok () -> () | Error e -> fail e);
  ignore(Rays.Canvas.destroy canvas);
  Hashtbl.iter (fun _ image -> ignore(Rays.Image.destroy image)) images;
  Option.iter Flow_gpu.Pipelines.close pipelines;
  Option.iter Flow_gpu.Host.close host;
  Option.iter Renderer.release_gpu gpu
