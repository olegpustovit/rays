(* A live frame's device outputs: once the next frame has been shown, no root of the real editor (its
   model, cook session, display registrations, caches) holds the old frame's holder. A pin finds a holder
   through its weak entry, so [None] after a collection is the holder gone. *)
module Loop = Rays_editor__.Loop_model
module Editor_loop = Rays_editor__.Editor_loop
module Cook = Rays_editor__.Cook

(* gpu_sea, a small grid: P, N and a noise on the device, each frame new *)
let text path =
  Str.global_replace (Str.regexp_string "(side : int 1000)") "(side : int 128)"
    (In_channel.with_open_bin path In_channel.input_all)

let frames = 12
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

(* the placeholder of the P the editor shows, if the device still holds it unread *)
let shown (editor : Loop.t) =
  List.find_map
    (fun (piece : Cook.piece) -> Flow_sop.Attribute_kernel.resident_vec3 piece.prepared.geometry "P")
    editor.core.cook.pieces

let () =
  let workspace =
    match Rays_editor.Workspace.load (text Sys.argv.(1)) with
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
  let previous = ref None and gone = ref 0 in
  for tick = 1 to frames do
    let at = frame tick in
    editor := Editor_loop.update !editor at;
    ok (Rays.Canvas.render ~density:1 canvas (Editor_loop.scene !editor at));
    editor := Editor_loop.after_present !editor at;
    let now = shown !editor in
    (match (!previous, now) with
    | Some old, Some current when old != current -> (
        Gc.full_major ();
        Gc.full_major ();
        match Flow_gpu.Run.pin old with
        | None -> incr gone
        | Some (_, _, _, unpin) ->
            unpin ();
            failwith (Printf.sprintf "frame %d shown: the frame before it still has its holder" tick))
    | _ -> ());
    if now <> None then previous := now
  done;
  Editor_loop.close !editor;
  Flow_native.Workspace_gpu.close gpu_owner;
  ignore (Rays.Canvas.destroy canvas);
  if !gone < frames / 2 then failwith (Printf.sprintf "only %d of %d frames drew a resident P" !gone frames);
  Printf.printf "holder lifetime: %d frames' holders gone once the next was shown\n" !gone
