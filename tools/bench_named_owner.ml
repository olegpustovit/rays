(* Actual named-pane selection lookup, while navigation stays at Scene. *)
open Rays
module E = Rays_editor.Editor

let frame mouse events count : Frame.t = {
  input = { (Input.at_time (float count /. 60.)) with dt = 1. /. 60.;
    frame = count; tick = count; size = 900, 640; pointer = mouse; events };
  pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0. }

let run objects =
  let source = Buffer.create (objects * 80) in
  Buffer.add_string source "(workspace owners\n";
  for i = 0 to objects - 1 do
    Printf.bprintf source "(graph g%d :context sop (let* [box (sop/box)] box))\n" i
  done;
  Printf.bprintf source "(graph editor :context editor (ui/workspace (ui/graph \"g%d\"))))" (objects - 1);
  let workspace = Rays_editor.Workspace.load (Buffer.contents source) |> Result.get_ok in
  let presets = Filename.temp_dir "rays-owner-bench" "" in
  let e = ref (E.create ~workspace ~presets ~domains:1 ~await:true
    ~prepare:(fun _ _ -> Ok ()) ~draw:(fun _ () -> Scene.D3.empty) () |> Result.get_ok) in
  Fun.protect ~finally:(fun () -> E.close !e) (fun () ->
    e := E.update !e (frame (450., 300.) [] 1);
    let x, y, w, _ = E.node_box !e [Printf.sprintf "g%d" (objects - 1); "box"] |> Option.get in
    let point = float (x + w / 2), float (y + 10) in
    e := E.update !e (frame point [] 2);
    e := E.update !e (frame point [Input.Pointer_pressed (Input.LeftButton, point);
      Input.Pointer_released (Input.LeftButton, point)] 3);
    assert (E.level !e = None);
    let expected = E.selected_node !e |> Option.get in
    assert (expected.kind = "box");
    for sample = 1 to 5 do
      Gc.full_major ();
      let allocated = Gc.allocated_bytes () and started = Unix.gettimeofday () in
      for _ = 1 to 10000 do
        assert (Option.map (fun (p : E.picked) -> p.id) (E.selected_node !e) = Some expected.id)
      done;
      Printf.printf "%d objects, sample %d: %.6f ms/lookup, %.0f bytes/lookup\n%!"
        objects sample ((Unix.gettimeofday () -. started) *. 0.1)
        ((Gc.allocated_bytes () -. allocated) /. 10000.)
    done)

let () = List.iter run [1; 100; 1000]
