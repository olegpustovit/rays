(* Real 64³ workspace: three complete editor frames while its worker holds an
   unpublished cook. Conditions synchronize the test; no sleep or clock decides it. *)
module Editor = Rays_editor__.Loop_model
module Cook = Rays_editor__.Cook

let ok = function Ok value -> value | Error d -> failwith (Diagnostic.to_string d)

let frame count : Rays.Frame.t =
  {
    input = { (Input.at_time 0.) with frame = count; tick = count; size = (1280, 900) };
    pixel_scale = (1., 1.);
    fps = 60.;
    mouse_delta = (0., 0.);
  }

let () =
  let text = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  assert (
    Editor_document.Lowering.sha256 text
    = "75686b00542de41aba26b5ff4cc63f88805924f4a625e8ad44c033d66664eeda");
  let workspace =
    match Rays_editor.Workspace.load text with
    | Ok workspace -> workspace
    | Error ds -> failwith (String.concat "\n" (List.map Diagnostic.to_string ds))
  in
  let workspace =
    ok
      (Editor_document.Workspace_doc.edit (ok (Rays_editor.workspace_catalog ())) workspace
         (Flow_graph.Flow_edit.Set_input_default
            { form = "volume"; input = "resolution"; value = List.hd (ok (Flow.Syntax.parse "64")) }))
  in
  let lock = Mutex.create () and event = Condition.create () in
  let entered = ref false and released = ref false and finished = ref false in
  let release () =
    Mutex.protect lock (fun () -> released := true; Condition.broadcast event)
  in
  let prepare settings output =
    assert (not (Domain.is_main_domain ()));
    let geometry = ok (Sop.Payload.geometry output.Sop.Session.payload) in
    assert (Rdk.Geometry.point_count geometry > 0);
    assert (Rdk.Geometry.point_count geometry = 4 * Rdk.Geometry.primitive_count geometry);
    Mutex.protect lock (fun () ->
        entered := true;
        Condition.broadcast event;
        while not !released do Condition.wait event lock done);
    Editor_prepare.empty settings output
  in
  let current =
    ref
      (Editor.create ~platform:Input.Apple ~workspace ~await:false ~domains:8 ~prepare
         ~draw:(fun _ _ -> Rays.Scene.D3.empty) () |> Result.get_ok)
  in
  let monitor = ref None in
  Fun.protect
    ~finally:(fun () ->
      release ();
      Option.iter (fun worker -> ignore (Domain.join worker)) !monitor;
      Rays_editor__.Editor_loop.close !current)
    (fun () ->
      let lowered = (!current).core.doc.workspace.lowered in
      assert (lowered.defer);
      let mesh =
        Array.find_opt (fun (node : Flow.Eval.node) -> node.kind = "sop/mesh") lowered.plan.nodes
        |> Option.get
      in
      assert (Flow.Eval.is_live (List.assoc "points" mesh.args));
      current := Rays_editor__.Editor_loop.update !current (frame 0);
      let worker = (!current).core.cook.worker in
      let observer =
        Domain.spawn (fun () ->
            let result = Sop.Async_cook.await worker in
            Mutex.protect lock (fun () -> finished := true; Condition.broadcast event);
            result)
      in
      monitor := Some observer;
      while not (Mutex.protect lock (fun () -> !entered || !finished)) do
        Flow_native.Workspace_gpu.pump (!current).host.gpu;
        Domain.cpu_relax ()
      done;
      assert (!entered);
      for count = 1 to 3 do
        current := Rays_editor__.Editor_loop.update !current (frame count);
        assert (Cook.pieces (!current).core.cook = []);
        match Cook.status (!current).core.cook with
        | Sop.Async_cook.Cooking _ -> ()
        | Idle -> failwith "64³ cook finished before its worker barrier was released"
      done;
      release ();
      let result = Domain.join observer in
      monitor := None;
      (match result.result with
      | Ok _ -> ()
      | Error error -> failwith (Sop.Async_cook.error_to_string error));
      print_endline "64³ deferred cook stays on its worker; three full editor frames return before publication")
