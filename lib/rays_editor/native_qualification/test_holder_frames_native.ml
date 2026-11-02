(* A live frame's device outputs: once the next frame has been shown, no root of the real editor (its
   model, cook session, display registrations, caches) holds the old frame's holder. A pin finds a holder
   through its weak entry, so [None] after a collection is the holder gone. *)
module Loop = Rays_editor__.Loop_model
module Editor_loop = Rays_editor__.Editor_loop
module Cook = Rays_editor__.Cook

(* a sketch's frames at a small side: gpu_sea (P, N and a noise on the device), isogrid_city (a mesh
   whose topology is on the device too) *)
let text path ~side =
  Str.global_replace (Str.regexp {|(side : int [0-9]+)|}) (Printf.sprintf "(side : int %d)" side)
    (In_channel.with_open_bin path In_channel.input_all)

let frames = 6
let ok = function Ok value -> value | Error message -> failwith message

let frame tick : Rays.Frame.t =
  {
    input =
      {
        (Input.at_time (float tick /. 60.)) with
        dt = 1. /. 60.;
        frame = tick;
        tick;
        size = (320, 240);
        pointer = (-100., -100.);
      };
    pixel_scale = (1., 1.);
    fps = 60.;
    mouse_delta = (0., 0.);
  }

(* what the editor shows of a device's: P's placeholder, and a weak hold on its topology's streams *)
let shown (editor : Loop.t) =
  List.find_map
    (fun (piece : Cook.piece) ->
      let geometry = piece.prepared.geometry in
      Option.map
        (fun p ->
          ( p,
            Option.map
              (fun (vertices, offsets, kinds) ->
                let streams = Weak.create 3 in
                List.iteri (fun i s -> Weak.set streams i (Some s)) [ vertices; offsets; kinds ];
                streams)
              (Flow_sop.Topology_kernel.resident geometry) ))
        (Flow_sop.Attribute_kernel.resident_vec3 geometry "P"))
    editor.core.cook.pieces

let frames_of path ~side ~topology =
  let workspace =
    match Rays_editor.Workspace.load (text path ~side) with
    | Ok workspace -> workspace
    | Error ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
  in
  let canvas = ok (Rays.Canvas.create ~width:320 ~height:240) in
  let gpu_owner = Flow_native.Workspace_gpu.create () in
  Flow_native.Workspace_gpu.qualification gpu_owner;
  let editor =
    ref
      (ok
         (Loop.create ~gpu_owner ~ui_text_size:11 ~await:true ~domains:1 ~workspace ~prepare:Cook.prepare
            ~draw:Cook.draw ()))
  in
  let previous = ref None and gone = ref 0 and meshes = ref 0 in
  for tick = 1 to frames do
    let at = frame tick in
    editor := Editor_loop.update !editor at;
    ok (Rays.Canvas.render ~density:1 canvas (Editor_loop.scene !editor at));
    editor := Editor_loop.after_present !editor at;
    let now = shown !editor in
    (match (!previous, now) with
    | Some (old, streams), Some (current, _) when old != current ->
        Gc.full_major ();
        Gc.full_major ();
        let fail what =
          failwith (Printf.sprintf "%s, frame %d shown: the frame before still holds its %s" path tick what)
        in
        (match Flow_gpu.Run.pin old with
        | None -> incr gone
        | Some (_, _, _, unpin) ->
            unpin ();
            fail "P");
        Option.iter
          (fun streams ->
            incr meshes;
            for i = 0 to 2 do
              if Weak.check streams i then fail "topology"
            done)
          streams
    | _ -> ());
    if now <> None then previous := now
  done;
  Editor_loop.close !editor;
  Flow_native.Workspace_gpu.close gpu_owner;
  ignore (Rays.Canvas.destroy canvas);
  if !gone < frames / 2 then failwith (Printf.sprintf "%s: only %d of %d frames drew a resident P" path !gone frames);
  if topology && !meshes < frames / 2 then
    failwith (Printf.sprintf "%s: only %d of %d frames drew a resident topology" path !meshes frames);
  Printf.printf "holder lifetime, %s: %d frames' holders gone once the next was shown\n" path !gone

let () =
  frames_of Sys.argv.(1) ~side:128 ~topology:false;
  frames_of Sys.argv.(2) ~side:8 ~topology:true
