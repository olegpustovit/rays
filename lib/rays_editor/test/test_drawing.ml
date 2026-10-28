module E = Flow.Eval
module V = Flow.Value
module P = Flow_graph.Projection
module F = Flow_graph.Flow_edit
let ok = function Ok x -> x | Error d -> failwith (Diagnostic.to_string d)
let read path = In_channel.with_open_bin path In_channel.input_all
let catalog = Result.get_ok (Rays_editor.workspace_catalog ())
let doc text = match Rays_editor.Workspace.load text with Ok d -> d
  | Error ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))
let bits = Int64.bits_of_float
(* One frame of a drawing value: its programs prepared, then run. *)
let render_once ?state plan value ~live ~size =
  Result.bind (Flow_native.Drawing.prepare plan value) (fun prepared ->
    Flow_native.Drawing.render_prepared ?state prepared ~live ~size)
type particle = {x : float; y : float; vx : float; vy : float}
let initial () = Array.init 10000 (fun i -> let angle = float i *. 0.1 in
  let speed = 30. +. float (i mod 70) in
  {x = 400.; y = 300.; vx = cos angle *. speed; vy = sin angle *. speed})
let step dt particles = Array.map (fun p ->
  let x = p.x +. p.vx *. dt and y = p.y +. p.vy *. dt in
  {x; y; vx = (if x < 0. || x >= 800. then -.p.vx else p.vx);
         vy = (if y < 0. || y >= 600. then -.p.vy else p.vy)}) particles
let edited source op = fst (ok (F.apply_checked catalog source op))
let edited_layout source op = fst (ok (Editor_document.Layout_edit.apply_checked catalog source op))
let checked source = match Flow.Workspace.check catalog source with
  | Some w, _ -> w | _, ds -> failwith (String.concat "; " (List.map Diagnostic.to_string ds))

let pure workspace =
  let all_draws = doc {|(workspace drawing (graph picture :context draw
    (draw/merge (draw/background "#080a10")
      (draw/point [1 2 0]) (draw/points (array/vec3 4 [2 3 0]))
      (draw/line [0 0 0] [10 10 0] :width 2)
      (draw/rect [5 5 0] [20 10 0] :fill "#ffffff")
      (draw/circle [15 15 0] 4 :stroke [0 1 1])
      (draw/text [30 10 0] "text" :size 16)
      (draw/circles (array/vec3 4 [20 30 0]) :radius 2.0)
      (draw/rects (array/vec3 4 [40 50 0]) [5 5 0])
      (draw/lines (array/vec3 4 [20 30 0]) (array/vec3 4 [40 50 0]))
      (draw/rotate 0.2 (draw/scale 2.0 (draw/rounded_rect [1 2 0] [20 20 0] :radius 4)))
      (draw/polygon (array/vec3 3 [1 2 0]))
      (draw/polyline (array/vec3 3 [1 2 0]))
      (draw/translate [10 20 0] (draw/point [1 2 0])))))|} in
  let all = ok (E.static all_draws.checked) in
  let kinds = Array.to_list all.plan.nodes |> List.map (fun (n : E.node) -> n.kind)
    |> List.sort_uniq String.compare in
  assert (kinds = (Flow.Op.all |> List.filter_map (fun (o : Flow.Op.t) ->
    if o.ctx = Flow.Context.draw && o.name<>"draw/image" then Some o.name else None) |> List.sort String.compare));
  assert (ok (render_once all.plan (List.assoc "picture" all.results)
    ~live:(Input.at_time 0.) ~size:(800, 600)) <> []);
  let gpu_document = doc {|(workspace gpu_error (graph picture :context draw
    (draw/circles (map (fn [i] [(+ i t) 0 0]) (array/range 10000)))))|} in
  let backend : Flow_ir.Gpu.backend = {cost=(fun ~fast:_ _ ~count:_ -> None);
    prepare=(fun ~fast:_ _ -> Ok {run=(fun (inputs:Flow_ir.Packed.Host.inputs) ->
      Ok Flow_ir.Gpu.{identity=17;count=inputs.count;width=3;output=0;stamp=0L;gpu_seconds=None});
      readback=(fun _ -> failwith "Display-only output must remain on the device")})} in
  let gpu={Flow_ir.Gpu.default with backend=Some backend;policy=Flow_ir.Gpu.Qualification} in
  let gpu_checked, gpu_evaluated = ok (Flow_ir.qualify_workspace ~gpu gpu_document.checked) in
  let gpu_prepared = ok (Flow_native.Drawing.prepare ~gpu ~approx:gpu_checked.approx
    gpu_evaluated.plan (List.assoc "picture" gpu_evaluated.results)) in
  let diagnostic = Diagnostic.error ~code:"E_TEST_GPU" ~cause:"原因 🐈"
    ~span:Diagnostic.{start=3;finish=8} "A backend failure" in
  let calls = ref 0 in
  let result = Flow_native.Drawing.render_prepared gpu_prepared ~live:(Input.at_time 0.25)
      ~size:(800,600) ~gpu:(fun ~slot:_ ~kind:_ ~count:_ _ -> incr calls; Error diagnostic) in
  assert (!calls=1 && result=Error diagnostic);
  let colored=doc {|(workspace colored (graph picture :context draw
    (draw/lines (array/vec3 2 [1 2 0]) (array/vec3 2 [3 4 0])
      :color (array/vec3 2 [1 0 0]))))|}in
  let evaluated=ok(E.static colored.checked)in
  let scene=ok(render_once evaluated.plan(List.assoc "picture" evaluated.results)
    ~live:(Input.at_time 0.) ~size:(800,600))in
  assert(match Rays.Scene.Private.commands scene with
    |[|Scene_command.Render_ir.Shapes batch|]->
      let bytes=Scene_command.Ui_batch.instances batch in
      Bytes.get_int32_le bytes 32=0xff0000ffl && Bytes.get_int32_le bytes 96=0xff0000ffl
    |_->false);
  List.iter(fun text->
    let ws=doc ("(workspace lengths (graph picture :context draw "^text^"))")in
    let evaluated=ok(E.static ws.checked)in
    assert(match render_once evaluated.plan (List.assoc "picture" evaluated.results)
      ~live:(Input.at_time 0.) ~size:(800,600)with
      |Error diagnostic->diagnostic.Diagnostic.code="E_DRAW_LENGTH"|Ok _->false))
    ["(draw/circles (array/vec3 3 [1 2 0]) :radius (array/float 2 1.0))";
     "(draw/rects (array/vec3 3 [1 2 0]) (array/vec3 2 [3 4 0]))";
     "(draw/lines (array/vec3 3 [1 2 0]) (array/vec3 2 [3 4 0]))"];
  let static=doc "(workspace retained (graph picture :context draw (draw/circles (array/vec3 100000 [20 30 0]) :radius 3.0)))"in
  let evaluated=ok(E.static static.checked)in
  let prepared=ok(Flow_native.Drawing.prepare evaluated.plan(List.assoc "picture" evaluated.results))in
  let state=E.create_state()in
  let render count=ok(Flow_native.Drawing.render_prepared ~state prepared
    ~live:(Input.at_time(float count /. 60.)) ~size:(800,600))in
  let first=render 0 in
  Gc.full_major();let before=Gc.allocated_bytes()in
  for i=1 to 100 do assert(render i==first)done;
  let allocated=(Gc.allocated_bytes()-.before)/.100. in
  assert(allocated<32768.);
  Printf.printf "Retained 100000-circle lowering %.0f bytes/frame\n%!" allocated;
  let invalid = ok (E.static (doc "(workspace invalid (graph g :context draw (draw/background \"invalid\")))").checked) in
  assert (match render_once invalid.plan (List.assoc "g" invalid.results)
    ~live:(Input.at_time 0.) ~size:(800, 600) with
    | Error d -> d.Diagnostic.code = "E_DRAW_COLOR" | Ok _ -> false);
  let evaluated = ok (E.static ~inputs:workspace.Editor_document.Workspace_doc.inputs workspace.checked) in
  assert (Array.length evaluated.plan.nodes = 3);
  Array.iter (fun (n : E.node) -> assert (n.ty = Flow.Ty.drawing)) evaluated.plan.nodes;
  let drawing = List.assoc "picture" evaluated.results in
  assert (match drawing with E.Deferred ((Flow.Ty.Named "drawing"), _) -> true | _ -> false);
  let scope = P.of_graph catalog workspace.checked "picture" in
  let state_card = Option.get (P.find scope ["picture"; "particles"]) in
  let zone = Option.get state_card.zone in
  assert (zone.kind = P.State);
  assert (List.exists (fun (r : P.rail_row) -> r.role = P.Acc && r.name = "previous") zone.rail);
  assert (P.find scope ["picture"; "@result#drawing#2"] <> None ||
    List.exists (fun (n : P.node) -> n.ty = Flow.Ty.drawing) scope.nodes);
  let state = E.create_state () in
  let points = Array.find_opt (fun (n : E.node) -> n.kind = "draw/points") evaluated.plan.nodes |> Option.get in
  let positions = List.assoc "positions" points.args in
  let oracle = ref (initial ()) in
  List.iteri (fun frame dt ->
    oracle := step dt !oracle;
    let live = {(Input.at_time (float frame *. dt)) with frame; dt; size = (800, 600)} in
    let xs = ok (E.force ~state positions ~live) in
    assert (V.array_length xs = 10000);
    Array.iteri (fun i p -> let x, y, z = V.comps (V.array_get xs i) in
      (* Native OCaml can fuse multiply-add on ARM; the Lisp arithmetic has
         separately rounded IEEE operations. Points still land on the same pixel. *)
      if abs_float (x -. p.x) > 1e-9 || abs_float (y -. p.y) > 1e-9
         || int_of_float x <> int_of_float p.x || int_of_float y <> int_of_float p.y || bits z <> bits 0. then
        failwith (Printf.sprintf "particle %d frame %d: got (%h,%h,%h), expected (%h,%h,0)" i frame x y z p.x p.y)) !oracle;
    ignore (ok (render_once ~state evaluated.plan drawing ~live ~size:live.size)))
    [1. /. 60.; 1. /. 60.; 0.125; 20.; 20.; 0.125];
  (* The same source edits used by the pane add a fold and change its seed. *)
  let source = edited workspace.source (F.Add_node {scope = ["picture"]; name = "elapsed";
    expr = List.hd (ok (Flow.Syntax.parse "(state [seconds 0.0] (+ seconds (frame/dt)))"))}) in
  let source = edited source (F.Set_arg {node = ["picture"; "elapsed"]; key = F.Bv (1, 1);
    sub = []; value = Flow.Syntax.make (Flow.Syntax.Num "2.0")}) in
  let w = checked source in
  let n = Option.get (P.find (P.of_graph catalog w "picture") ["picture"; "elapsed"]) in
  assert ((Option.get n.zone).kind = P.State);
  let panel_source = ok (Flow.Syntax.parse "(workspace canvas (graph g :context draw (draw/merge)) (graph editor :context editor (let* [pane (ui/lisp)] (ui/workspace pane))))") in
  let panel_source = edited_layout panel_source (Editor_document.Layout_edit.Set_panel_kind {node = ["editor"; "pane"];
    panel = Result.get_ok (Editor_document.Panel_kinds.expr panel_source "canvas")}) in
  let panel_workspace = doc (fst (Flow.Lisp.print panel_source)) in
  let panel_document = ok (Editor_document.Lowering.of_workspace ~factories:Sop.Nodes.factories panel_workspace) in
  assert (match (Option.get panel_document.shell).tree with Editor_core.Panels.Leaf (Canvas _) -> true | _ -> false);
  let input_text = "(workspace inputs (graph g :context draw [(radius : float 5)] (draw/circle [10 10 0] radius :fill \"#00ffff\")) (graph editor :context editor (ui/workspace (ui/canvas (ref g)))))" in
  let inputs = ["g", ["radius", E.Float 9.]] in
  let workspace = {(doc input_text) with inputs} in
  let document = ok (Editor_document.Lowering.of_workspace ~factories:Sop.Nodes.factories workspace) in
  let lower = document.workspace.lowered in
  assert (List.assoc "radius" lower.plan.nodes.(0).args = E.Float 9.);
  let source = edited workspace.source (F.Set_arg {node = ["g"; "@result"]; key = F.Pos 0;
    sub = []; value = List.hd (ok (Flow.Syntax.parse "[20 20 0]"))}) in
  let workspace = {workspace with source; checked = checked source} in
  let after = ok (Editor_document.Lowering.of_workspace ~factories:Sop.Nodes.factories ~previous:document workspace) in
  assert (List.assoc "radius" (after.workspace.lowered).plan.nodes.(0).args = E.Float 9.);
  let bad = {workspace with inputs = ["g", ["radius", E.Text "wrong"]]} in
  assert (Result.is_error (Editor_document.Lowering.of_workspace ~factories:Sop.Nodes.factories bad));
  let module Editor = Rays_editor.Editor in
  let editor = Result.get_ok (Editor.create ~ui_text_size:11 ~workspace ~await:true
    ~prepare:(fun _ _ -> Ok ()) ~draw:(fun _ () -> Rays.Scene.D3.create []) ()) in
  Fun.protect ~finally:(fun () -> Editor.close editor) (fun () ->
    let frame : Rays.Frame.t = { input = { (Input.at_time 0.) with
      size = 800, 600; dt = 0.125; pointer = -100., -100. };
      pixel_scale = 1., 1.; fps = 8.; mouse_delta = 0., 0. } in
    let editor = Editor.update editor frame in
    match Rays.Scene.Private.stage_native ~width:800 ~height:600 (Editor.scene editor frame) with
    | Ok staged ->
        assert (List.exists (function Rays.Scene.Private.Scene2_layer _ -> true | _ -> false) staged.layers);
        let allocation count =
          let workspace = doc (Printf.sprintf "(workspace static (graph g :context draw \
            (draw/points (array/vec3 %d [20 40 0]))) \
            (graph editor :context editor (ui/workspace (ui/canvas (ref g)))))" count) in
          let editor = ref (Result.get_ok (Editor.create ~ui_text_size:11 ~workspace ~await:true
            ~prepare:(fun _ _ -> Ok ()) ~draw:(fun _ () -> Rays.Scene.D3.create []) ())) in
          Fun.protect ~finally:(fun () -> Editor.close !editor) (fun () ->
            let step i = editor := Editor.update !editor { frame with input = { frame.input with
              frame = i; tick = i; t = float i /. 8. } } in
            for i = 0 to 4 do step i done;
            let before = Gc.allocated_bytes () in
            for i = 5 to 24 do step i done;
            (Gc.allocated_bytes () -. before) /. 20.) in
        let small = allocation 4 and large = allocation 10000 in
        assert (large <= small +. 32768.);
        Printf.printf "Static Canvas allocated bytes/frame: 4 points %.0f, 10000 points %.0f\n" small large
    | Error message -> failwith message);
  print_endline "Drawing: typed plan, packed particles, editable folds, canvas and host inputs passed"

let native workspace directory =
  let path name = Filename.concat directory name in
  let config = {Rays.Sketch.default_config with width = 800; height = 600; fps = None} in
  let frames = 4 in
  let view particles _ =
    let points = Array.to_list (Array.map (fun p ->
      Rays.Scene.point ~at:(int_of_float p.x, int_of_float p.y)
        ~color:(Rays_math.Color.rgb 0 255 255) ()) particles) in
    Rays.Scene.clear (Rays_math.Color.rgb 8 10 16) :: points in
  ignore (Result.get_ok (Rays.Sketch.export_state ~config ~directory:(path "ocaml") ~frames
    ~init:(fun _ -> initial ())
    ~update:(fun particles frame -> step frame.Rays.Frame.input.dt particles)
    ~view (())));
  List.iter (fun name -> ignore (ok (Rays_editor.Workspace.export ~directory:(path name) ~frames workspace))) ["lisp-a"; "lisp-b"];
  let evaluated = ok (E.static ~inputs:workspace.Editor_document.Workspace_doc.inputs workspace.checked) in
  let prepared = ok (Flow_native.Drawing.prepare ~states:evaluated.states evaluated.plan
    (List.assoc "picture" evaluated.results)) in
  let render_domain domains = Parallel.run ~domains (fun () ->
    let state = E.create_state () in
    let name = "cpu-" ^ string_of_int domains in
    let result = Rays.Sketch.export_state ~config ~directory:(path name) ~frames
      ~init:(fun _ -> ()) ~update:(fun () _ -> ())
      ~view:(fun () frame ->
        let live = {(Input.at_time frame.Rays.Frame.input.t) with dt = 1. /. 60.; frame = frame.input.frame; size = 800, 600} in
        ok (Flow_native.Drawing.render_prepared ~state prepared ~live ~size:live.size)) () in
    ignore (Result.get_ok result)) in
  List.iter render_domain [1; 8];
  for i = 0 to frames - 1 do
    let file name = Filename.concat (path name) (Printf.sprintf "frame-%06d.png" i) in
    let baseline = read (file "ocaml") in
    assert (baseline = read (file "lisp-a"));
    assert (baseline = read (file "lisp-b"));
    assert (baseline = read (file "cpu-1"));
    assert (baseline = read (file "cpu-8"))
  done;
  assert (read (Filename.concat (path "lisp-a") "frame-000000.png") <>
          read (Filename.concat (path "lisp-a") "frame-000003.png"));
  Printf.printf "Particles native exports match byte for byte: %s\n" directory

let benchmark workspace =
  let module Editor = Rays_editor.Editor in
  let editor = ref (Result.get_ok (Editor.create ~ui_text_size:11 ~workspace ~await:true ~domains:1
    ~prepare:(fun _ _ -> Ok ()) ~draw:(fun _ () -> Rays.Scene.D3.empty) ())) in
  Fun.protect ~finally:(fun () -> Editor.close !editor) (fun () ->
    let frame count : Rays.Frame.t = { input = { (Input.at_time (float count /. 60.)) with
      dt = 1. /. 60.; frame = count; tick = count; size = 800, 600; pointer = -100., -100. };
      pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0. } in
    editor := Editor.update !editor (frame 0);
    editor := Rays_editor.Reduce.step !editor [Rays_editor.Private.Editor_commands.Play_pause] (frame 0);
    let step i = editor := Editor.update !editor (frame i) in
    for i = 1 to 10 do step i done;
    let times = Array.make 200 0. in
    Gc.full_major ();
    let before = Gc.allocated_bytes () in
    Array.iteri (fun i _ ->
      let started = Unix.gettimeofday () in
      step (i + 11);
      times.(i) <- Unix.gettimeofday () -. started) times;
    let bytes = (Gc.allocated_bytes () -. before) /. float (Array.length times) in
    Array.sort Float.compare times;
    assert (Rays_editor.Timeline.frame (Editor.timeline !editor) > 100L);
    Printf.printf "particles_dynamic_frame,10000,%.9f,%.9f,%.0f\n%!"
      times.(100) times.(190) bytes)

let () =
  let workspace = doc (read Sys.argv.(1)) in
  if Array.length Sys.argv > 2 && Sys.argv.(2) = "--bench" then benchmark workspace
  else if Array.length Sys.argv > 2 then native workspace Sys.argv.(2) else pure workspace
