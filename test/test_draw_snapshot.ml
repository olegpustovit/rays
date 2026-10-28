(* Snapshots of the draw-graph scene of every eligible workspace, taken before
   the core drawing path (P3.9) replaced Drawing's per-kind arms; the core path must keep them. For each
   workspace the first draw graph is lowered and prepared as Workspace.export
   does; frames 0..30 are rendered in sequence with one evaluation state at a
   pinned size and pointer, with no GPU callback and no image callback. Frames
   0, 1 and 30 are written as the SHA-256 of Render_ir.serialize of the scene,
   or as the diagnostic code when the frame cannot be drawn headless.
   The digest covers Image resource ids, which come from the process-global
   texture counter (rendered text allocates two per new string), so each
   workspace must run in its own process, as the dune rule does. *)

let width = 640
let height = 480
let dt = 1. /. 60.
let last_frame = 30
let recorded = [ 0; 1; 30 ]

let workspaces =
  [
    ("sketches", "audio");
    ("sketches", "basic");
    ("sketches", "drawing");
    ("sketches", "file_dialog");
    ("sketches", "generative");
    ("sketches", "noise");
    ("sketches", "particles");
    ("sketches", "pxui");
    ("sketches", "recursive_rectangles");
    ("sketches", "flow_contours");
    ("sketches", "flow_image");
    ("sketches", "flow_image_kernel");
    ("sketches", "flow_particles_gpu");
    ("sketches", "flow_shader");
    ("sketches", "flow_vectors");
  ]

let file_name (directory, name) = Printf.sprintf "%s_%s.txt" directory name

let input index =
  {
    (Input.at_time (float index *. dt)) with
    dt;
    frame = index;
    tick = index;
    size = (width, height);
    pointer = (100., 100.);
  }

let rendered_digest scene =
  match Scene_command.Render_ir.create (Rays.Scene.Layer.commands scene) with
  | Error _ -> failwith "Render_ir.create rejected a scene that Drawing built"
  | Ok ir ->
      Editor_document.Lowering.sha256 (Bytes.to_string (Scene_command.Render_ir.serialize ir))

let lines (_, name) =
  let file = Printf.sprintf "../sketches/%s.rays" name in
  let text = In_channel.with_open_bin file In_channel.input_all in
  let imports =
    match Rays_editor.Source.read_imports ~file text with
    | Ok imports -> imports
    | Error ds -> failwith (file ^ ": " ^ String.concat "\n" (List.map Diagnostic.to_string ds))
  in
  let doc =
    match Rays_editor.Workspace.load ~imports text with
    | Ok doc -> doc
    | Error ds -> failwith (file ^ ": " ^ String.concat "\n" (List.map Diagnostic.to_string ds))
  in
  let lowered =
    match Flow_sop.Lower.of_checked ~reference:true ~inputs:doc.inputs doc.checked with
    | Ok lowered -> lowered
    | Error d -> failwith (file ^ ": " ^ Diagnostic.to_string d)
  in
  let evaluated = lowered.evaluated in
  let graph =
    List.find (fun (g : Flow.Workspace.graph) -> g.context = Flow.Context.draw) doc.checked.graphs
  in
  let value = List.assoc graph.name evaluated.results in
  let state = Flow.Eval.create_state () in
  let outcome = Flow_native.Drawing.prepare ~states:evaluated.states evaluated.plan value in
  (* the run's text cache, as Sketch.run_state installs it: one rasterization per text *)
  let fonts = Rays.Font.Raster.create_cache_context () in
  let frames =
    Rays.Font.Raster.with_cache_context fonts @@ fun () ->
    List.init (last_frame + 1) (fun index ->
        let result =
          match outcome with
          | Error d -> Error d.code
          | Ok prepared -> (
              let live = input index in
              match Flow_native.Drawing.render_prepared ~state prepared ~live ~size:live.size with
              | Error d -> Error d.code
              | Ok scene -> Ok (rendered_digest scene))
        in
        (index, result))
  in
  Rays.Font.Raster.clear_cache_context fonts;
  List.filter_map
    (fun (index, result) ->
      if not (List.mem index recorded) then None
      else
        Some
          (match result with
          | Ok hex -> Printf.sprintf "frame %d %s\n" index hex
          | Error code -> Printf.sprintf "frame %d error %s\n" index code))
    frames
  |> String.concat ""

(* No argument: run every workspace in its own process and compare with draw_snapshots/.
   With a name (examples_audio): print that workspace's snapshot. *)
let () =
  match List.tl (Array.to_list Sys.argv) with
  | [] ->
      let read f = In_channel.with_open_bin f In_channel.input_all in
      let failed =
        List.filter
          (fun workspace ->
            let name = file_name workspace in
            let out = Filename.temp_file "snapshot" ".txt" in
            let status =
              Sys.command
                (Filename.quote_command Sys.executable_name
                   [ Filename.remove_extension name ]
                   ~stdout:out)
            in
            status <> 0 || read out <> read ("draw_snapshots/" ^ name))
          workspaces
      in
      if failed <> [] then
        failwith ("draw snapshot differs: " ^ String.concat " " (List.map file_name failed))
  | names ->
      List.iter
        (fun name ->
          match List.find_opt (fun workspace -> file_name workspace = name ^ ".txt") workspaces with
          | Some workspace -> print_string (lines workspace)
          | None -> failwith ("unknown draw snapshot: " ^ name))
        names
