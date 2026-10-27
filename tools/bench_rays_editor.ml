(* Rays Editor edit frames on a large workspace graph: one full [Editor.update]
   per sample while a node drag records into the undo document, plus the
   undo frame that restores the layout. Prints CSV:
   name,nodes,median_s,p95_s,bytes_per_frame.
   [--panels [nodes]] measures idle frames (the pointer moving over the viewport) of layouts
   with one and three graph panels and one and two inspectors over the same graph.
   [--particles [samples]] includes native offscreen rendering of four particle layouts;
   [--particles-window] runs each in a window, and [--particles-toggle] switches them in one
   running editor. Window runs print frame periods as well as update/scene costs. *)
open Sop

let pointer (x, y) = float x, float y

let frame ?(mouse = 0, 0) ?(buttons = []) ?(events = []) count : Rays.Frame.t = {
  input = { (Input.at_time (float_of_int count /. 60.)) with dt = 1. /. 60.;
    frame = count; tick = count; size = 1_200, 760; pointer = pointer mouse; buttons; events };
  pixel_scale = 1., 1.; fps = 60.; mouse_delta = 0., 0.;
}

let percentile values fraction =
  let values = Array.copy values in
  Array.sort Float.compare values;
  values.(min (Array.length values - 1)
    (max 0 (int_of_float (Float.ceil
      (fraction *. float_of_int (Array.length values))) - 1)))

(* Layers of two-input nodes over a row of point sources, as a workspace text.  The nodes are
   switches, not merges: a merge of two nodes of the layer before doubles the geometry every
   layer (2^30 points at 2000 nodes), and the bench measures the editor, not that cook. *)
let workspace_text count =
  let width = 64 in
  let b = Buffer.create 65_536 in
  Buffer.add_string b "(workspace bench\n  (graph g :context sop\n    (let* [";
  let layer = ref (Array.init width (fun index ->
    let name = Printf.sprintf "s%d" index in
    Buffer.add_string b (Printf.sprintf "%s (sop/points :points 1)\n           " name); name))
  and created = ref width and generation = ref 0 and all = ref [] in
  while !created < count - 1 do
    let size = min width (count - 1 - !created) and previous = !layer in
    layer := Array.init size (fun index ->
      let name = Printf.sprintf "n%d_%d" !generation index in
      Buffer.add_string b (Printf.sprintf "%s (sop/switch %s %s)\n           " name
        previous.(index mod Array.length previous) previous.((index + 1) mod Array.length previous));
      all := name :: !all;
      name);
    created := !created + size;
    incr generation
  done;
  Buffer.add_string b (Printf.sprintf "output (sop/merge %s)]\n      output)))\n"
    (String.concat " " (Array.to_list !layer)));
  (* the last layer first: the card the drag bench looks for, then every other layer *)
  Buffer.contents b, Array.to_list !layer @ !all

let load text = match Rays_editor.Workspace.load text with
  | Ok workspace -> workspace
  | Error diagnostics -> failwith (String.concat "; " (List.map Diagnostic.to_string diagnostics))

let workspace count = let text, names = workspace_text count in load text, names

let report name nodes samples bytes =
  Printf.printf "%s,%d,%.9f,%.9f,%.0f\n%!" name nodes
    (percentile samples 0.5) (percentile samples 0.95)
    (bytes /. float_of_int (Array.length samples))

let measure nodes =
  let module E = Rays_editor.Editor in
  let workspace, names = workspace nodes in
  let environment = E.create ~workspace
      ~domains:1 ~presets:(Filename.temp_dir "rays-editor-bench" "")
      ~max_entries:4 ~max_payload_bytes:(1024 * 1024)
      ~prepare:(fun _ _ -> Ok ())
      ~draw:(fun _ () -> Rays.Scene3.create []) () |> Result.get_ok in
  let environment = ref (E.update environment (frame 0)) and count = ref 1 in
  let step ?mouse ?buttons ?events () =
    environment := E.update !environment (frame ?mouse ?buttons ?events !count);
    incr count in
  let gx, gy, gw, gh = (E.panes !environment (frame 0)).graph in
  (* The scene opens as a list: focus it, arrow to the object, and enter it. *)
  let inside = gx + 20, gy + gh - 20 in
  step ~mouse:inside ();
  step ~mouse:inside ~events:[Input.Pointer_pressed (Input.LeftButton, pointer inside);
    Input.Pointer_released (Input.LeftButton, pointer inside)] ();
  let key k = Input.Key_pressed k in
  while Option.map Node.label (E.selected_node !environment) <> Some "g" do
    step ~events:[key Input.ArrowDown] ()
  done;
  step ~events:[key (Input.KeyChar 'i')] ();
  step ();
  (* a large graph opens with its cards outside the pane: frame all of it, then take one inside *)
  let x, y, width, height = match List.find_map (fun name ->
      match E.node_box !environment [ "g"; name ] with
      | Some (x, y, width, height) when x >= gx && y >= gy && x + width < gx + gw
          && y + height < gy + gh -> Some (x, y, width, height)
      | _ -> None) ("output" :: names) with
    | Some box -> box
    | None ->
        step ~mouse:inside ~events:[key Input.Home] (); step ~mouse:inside ();
        List.find_map (fun name ->
      match E.node_box !environment [ "g"; name ] with
      | Some (x, y, width, height) when x >= gx && y >= gy && x + width < gx + gw
          && y + height < gy + gh -> Some (x, y, width, height)
      | _ -> None) ("output" :: names) |> Option.get in
  let start = x + (width / 2), y + min 12 (height / 2) in
  step ~mouse:start ();  (* hover: hit testing uses the last frame *)
  step ~mouse:start ~buttons:[Input.LeftButton]
    ~events:[Input.Pointer_pressed (Input.LeftButton, pointer start)] ();
  let samples = Array.make 200 0. in
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  Array.iteri (fun index _ ->
    let point = fst start + 10 + (index mod 2), snd start in
    let started = Unix.gettimeofday () in
    step ~mouse:point ~buttons:[Input.LeftButton]
      ~events:[Input.Pointer_moved (pointer point)] ();
    samples.(index) <- Unix.gettimeofday () -. started) samples;
  report "rays_editor_drag_frame" nodes samples (Gc.allocated_bytes () -. before);
  let finish = fst start + 11, snd start in
  step ~mouse:finish
    ~events:[Input.Pointer_released (Input.LeftButton, pointer finish)] ();
  let undo = [|0.|] in
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  let started = Unix.gettimeofday () in
  let frame = frame ~events:[Input.Key_pressed (Input.KeyChar 'z')] !count in
  environment := E.update !environment { frame with input = { frame.input with keys = [Input.Meta] } };
  undo.(0) <- Unix.gettimeofday () -. started;
  report "rays_editor_undo_frame" nodes undo (Gc.allocated_bytes () -. before);
  if E.can_redo !environment |> not then failwith "undo did not step the history";
  (* The same open graph and frame as the drag: change one literal parameter,
     then run the entire next editor frame, with the pointer still held.  The edit row is the
     frame without its cook phase: the recook the changed value requires is work a drag
     never does, so it is the row compared with the drag. *)
  let phases = List.map (fun phase -> phase, Array.make 200 0.)
    ["print"; "parse"; "check"; "evaluate"; "lower"; "project"; "layout"; "reduce"; "cook"] in
  let samples = Array.make 200 0. in
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  Array.iteri (fun index _ ->
    let started = Unix.gettimeofday () in
    let (), edited = Flow.Phase_timer.sample ~clock:Unix.gettimeofday (fun () ->
    environment := E.edit !environment (Flow_graph.Flow_edit.Set_arg {
      node = ["g"; "s0"]; key = Flow_graph.Flow_edit.Kw "points"; sub = [];
      value = Flow.Syntax.make (Flow.Syntax.Num (string_of_int (1 + index mod 2))) }) |> Result.get_ok;
    step ~mouse:finish ~buttons:[Input.LeftButton] ()) in
    samples.(index) <- Unix.gettimeofday () -. started;
    List.iter (fun (phase, column) -> column.(index) <- Flow.Phase_timer.seconds edited phase) phases) samples;
  let bytes = Gc.allocated_bytes () -. before in
  report "rays_editor_scrub_frame" nodes samples bytes;
  report "rays_editor_scrub_edit_frame" nodes
    (Array.map2 (fun frame cook -> frame -. cook) samples (List.assoc "cook" phases)) bytes;
  List.iter (fun (phase, samples) ->
    Printf.printf "scrub_phase,%d,%s,%.9f\n%!" nodes phase (percentile samples 0.5)) phases;
  E.close !environment

(* Idle frames of one graph under layouts that differ only in how many graph panels and
   inspectors they hold: what an extra panel instance costs a frame. *)
let measure_panels nodes =
  let module E = Rays_editor.Editor in
  let text, _ = workspace_text nodes in
  let body = String.sub text 0 (String.length text - 2) in  (* without the workspace's closing bracket *)
  let graphs n = if n = 1 then "(ui/graph \"g\")"
    else "(ui/tile " ^ String.concat " " (List.init n (fun _ -> "(ui/graph \"g\")")) ^ ")" in
  let inspectors n = if n = 1 then "(ui/inspector)" else "(ui/split \"vertical\" (ui/inspector) (ui/inspector))" in
  List.iter (fun (name, g, i) ->
    let workspace = load (Printf.sprintf
      "%s\n  (graph scene :context scene (scene/merge (scene/geometry (ref g))))\n  \
       (graph editor :context editor\n    (ui/workspace (ui/split-at \"horizontal\" 0.3 (ui/viewport (ref scene))\n      \
       (ui/split \"horizontal\" %s %s :second_size 300)))))\n" body (graphs g) (inspectors i)) in
    let environment = ref (E.create ~workspace ~await:true
        ~domains:1 ~presets:(Filename.temp_dir "rays-editor-bench" "")
        ~max_entries:4 ~max_payload_bytes:(1024 * 1024)
        ~prepare:(fun _ _ -> Ok ())
        ~draw:(fun _ () -> Rays.Scene3.create []) () |> Result.get_ok) in
    let count = ref 0 in
    let step mouse =
      environment := E.update !environment (frame ~mouse ~events:[Input.Pointer_moved (pointer mouse)] !count);
      incr count in
    for _ = 1 to 20 do step (100, 300) done;
    let samples = Array.make 300 0. in
    Gc.full_major ();
    let before = Gc.allocated_bytes () in
    Array.iteri (fun index _ ->
      let started = Unix.gettimeofday () in
      step (100 + (index mod 50), 300);
      samples.(index) <- Unix.gettimeofday () -. started) samples;
    report name nodes samples (Gc.allocated_bytes () -. before);
    E.close !environment)
    [ "panels_1_graph_1_inspector", 1, 1; "panels_3_graphs_1_inspector", 3, 1;
      "panels_1_graph_2_inspectors", 1, 2; "panels_3_graphs_2_inspectors", 3, 2 ]

let measure_spreadsheet () =
  let module E = Rays_editor.Editor in
  let workspace = load {|(workspace sheet
    (graph g :context sop (sop/grid :counts "Point counts" :connectivity "Points" :rows 1000 :columns 1000))
    (graph editor :context editor (let* [gpanel (ui/graph "g" :focus true)]
      (ui/workspace (ui/split "horizontal" gpanel (ui/spreadsheet :of gpanel))))))|} in
  let environment = ref (E.create ~workspace ~await:true ~domains:1
    ~presets:(Filename.temp_dir "rays-sheet-bench" "")
    ~prepare:(fun _ _ -> Ok ()) ~draw:(fun _ () -> Rays.Scene3.empty) () |> Result.get_ok) in
  Fun.protect ~finally:(fun () -> E.close !environment) (fun () ->
    environment := Rays_editor.Reduce.select_path !environment ["g"; "@result"];
    let step ?(events = []) count = environment := E.update !environment (frame ~mouse:(900,300) ~events count) in
    for i = 0 to 19 do step i done;
    let geometry = snd (Rays_editor.Reduce.spreadsheet !environment [1]) |> Option.get in
    assert (Rdk.Geometry.point_count geometry = 1_000_000);
    Gc.full_major ();
    let before = Gc.allocated_bytes () and samples = Array.make 300 0. in
    Array.iteri (fun index _ -> let started = Unix.gettimeofday () in step (index+20);
      samples.(index) <- Unix.gettimeofday () -. started) samples;
    report "panels_spreadsheet" 1_000_000 samples (Gc.allocated_bytes () -. before);
    let events = [Input.Pointer_moved (900., 300.); Input.Scrolled (0., -3.)] in
    for i = 320 to 339 do step ~events i done;
    Gc.full_major ();
    let before = Gc.allocated_bytes () in
    Array.iteri (fun index _ -> let started = Unix.gettimeofday () in step ~events (index+340);
      samples.(index) <- Unix.gettimeofday () -. started) samples;
    let allocated = Gc.allocated_bytes () -. before in
    assert (Rdk.Geometry.point_count (snd (Rays_editor.Reduce.spreadsheet !environment [1]) |> Option.get) = 1_000_000);
    report "panels_spreadsheet_scroll" 1_000_000 samples allocated)

let measure_world () =
  let open Rays in
  let module E = Rays_editor.Editor in
  Parallel.run ~domains:1 (fun () ->
    let workspace = match Rays_editor.Workspace.load
        "(workspace w (graph g :context sop (sop/points :points 1)))" with
      | Ok workspace -> workspace | Error _ -> failwith "the World fixture does not check" in
    let environment = ref (E.create ~domains:1 ~workspace
      ~camera:(Easy_camera.create ~inertia:false ())
      ~prepare:(fun _ _ -> Ok ()) ~draw:(fun _ () -> Scene3.empty) () |> Result.get_ok) in
    Fun.protect ~finally:(fun () -> E.close !environment) (fun () ->
      let input count x events =
        let frame = frame ~mouse:(100, 300) ~buttons:[Input.LeftButton] ~events count in
        { frame with input = { frame.input with size = 900, 640; pointer = x, 300.; keys = [Input.Shift] };
          mouse_delta = 0.02, 0. } in
      let deadline = Unix.gettimeofday () +. 2. in
      while E.prepared !environment = None do
        if Unix.gettimeofday () >= deadline then failwith "initial cook did not settle";
        environment := E.update !environment (frame 0);
        Unix.sleepf 0.001
      done;
      environment := E.update !environment { (input 1 100.
        [Input.Pointer_pressed (Input.LeftButton, (100., 300.))]) with mouse_delta = 0., 0. };
      let samples = Array.make 16 0. in
      Gc.full_major ();
      let allocated = Gc.allocated_bytes () in
      Array.iteri (fun index _ ->
        let x = 100. +. float (index + 1) *. 0.02 in
        let input = input (index + 2) x [Input.Pointer_moved (x, 300.)] in
        let started = Unix.gettimeofday () in
        environment := E.update !environment input;
        samples.(index) <- Unix.gettimeofday () -. started) samples;
      let bytes = Gc.allocated_bytes () -. allocated in
      let rotation = E.scene_document !environment |> Edit_graph.inspect
        |> List.find_map (fun (info : Edit_graph.node_info) ->
          if info.operation <> "world" then None else Node.parameter_fields info.node
          |> List.find_map (fun (field : Param.field_view) -> match field.name, field.current with
            | "rotation", Param.Float_value value -> Some value | _ -> None)) |> Option.get in
      if abs_float (rotation -. 0.16) > 1e-9 then failwith "World input benchmark did not rotate 0.16 degrees";
      report "rays_editor_world_frame" 1 samples bytes))

(* The same animated drawing under four layouts; includes native offscreen presentation,
   with update and UI scene construction reported separately to locate panel overhead. *)
let measure_particles ?(window=false) ?(toggle=false) samples_count =
  let module E = Rays_editor.Editor in
  let module S = Flow.Syntax in
  let source = In_channel.with_open_bin "sketches/particles.rays" In_channel.input_all
      |> S.parse |> Result.get_ok in
  let layouts = [
    "canvas", "(ui/canvas (ref picture) :focus true)";
    "graph", "(ui/split \"horizontal\" (ui/canvas (ref picture) :focus true) (ui/graph \"picture\"))";
    "lisp", "(ui/split \"horizontal\" (ui/canvas (ref picture) :focus true) (ui/lisp :tab \"graph\"))";
    "graph_lisp", "(ui/split \"horizontal\" (ui/canvas (ref picture) :focus true) (ui/split \"vertical\" (ui/graph \"picture\") (ui/lisp :tab \"graph\")))"
  ] in
  let setups = if toggle then ["toggle", "(ui/switch " ^ String.concat " " (List.map snd layouts) ^ ")"] else layouts in
  List.iter (fun (name, layout) ->
    let editor_graph = S.parse ("(graph editor :context editor (ui/workspace " ^ layout ^ "))")
        |> Result.get_ok |> List.hd in
    let source = List.map (fun form -> match form.S.node with
      | S.List children when S.head form = Some "workspace" ->
          {form with S.node = S.List (List.map (fun child -> match S.children child with
            | _ :: {S.node = S.Sym "editor"; _} :: _ when S.head child = Some "graph" -> editor_graph
            | _ -> child) children)}
      | _ -> form) source |> Flow.Lisp.print |> fst in
    let create () = E.create ~workspace:(load source) ~await:true ~domains:1
      ~presets:(Filename.temp_dir "rays-particles-panel-bench" "")
      ~prepare:(fun _ _ -> Ok ()) ~draw:(fun _ () -> Rays.Scene3.empty) () |> Result.get_ok in
    if window then begin
      let phases = if toggle then List.length layouts else 1 in
      let periods = Array.make (samples_count * phases) 0. and updates = Array.make (samples_count * phases) 0.
      and scenes = Array.make (samples_count * phases) 0. in
      let bytes = Array.make phases 0. in
      let phase frame = (frame.Rays.Frame.input.frame - 1) / (samples_count + 20)
      and local frame = (frame.Rays.Frame.input.frame - 1) mod (samples_count + 20) in
      let sample_index frame = phase frame * samples_count + local frame - 20 in
      ignore (Result.get_ok (Rays.Sketch.run_state ~max_frames:((samples_count + 20) * phases)
        ~config:{Rays.Sketch.default_config with width=800; height=600; fps=Some 60;
          domains=Some 1; title="Particle panels: " ^ name}
        ~init:(fun _ -> create ())
        ~update:(fun editor frame ->
          let editor = if toggle && local frame = 0 && phase frame > 0 then
              E.edit editor (Flow_graph.Flow_edit.Set_layout {graph="editor"; index=phase frame}) |> Result.get_ok
            else editor in
          let started = Unix.gettimeofday () and before = Gc.allocated_bytes () in
          let editor = E.update editor frame in
          let index = sample_index frame in
          if local frame >= 20 then begin
            periods.(index) <- frame.input.dt;
            updates.(index) <- Unix.gettimeofday () -. started;
            bytes.(phase frame) <- bytes.(phase frame) +. Gc.allocated_bytes () -. before
          end;
          editor)
        ~view:(fun editor frame ->
          let started = Unix.gettimeofday () in
          let scene = E.scene editor frame in
          let index = sample_index frame in
          if local frame >= 20 then scenes.(index) <- Unix.gettimeofday () -. started;
          scene)
        ~on_stop:E.close ()));
      for phase = 0 to phases - 1 do
        let name = if toggle then "toggle_" ^ fst (List.nth layouts phase) else name in
        let prefix = "particles_window_" ^ name in
        let column values = Array.sub values (phase * samples_count) samples_count in
        report (prefix ^ "_period") 10_000 (column periods) bytes.(phase);
        report (prefix ^ "_update") 10_000 (column updates) bytes.(phase);
        report (prefix ^ "_scene") 10_000 (column scenes) 0.;
        if samples_count >= 240 then begin
          report (prefix ^ "_period_first120") 10_000 (Array.sub periods (phase * samples_count) 120) 0.;
          report (prefix ^ "_period_last120") 10_000 (Array.sub periods ((phase+1) * samples_count - 120) 120) 0.
        end
      done
    end else begin
    let environment = ref (create ()) in
    let canvas = Rays.Canvas.create_exn ~width:1600 ~height:1200 in
    Fun.protect ~finally:(fun () -> E.close !environment; ignore(Rays.Canvas.destroy canvas)) (fun () ->
      let frame count = let frame = frame ~mouse:(100,100) count in
        { frame with input = { frame.input with size = 800, 600 }; pixel_scale = 2., 2. } in
      let count = ref 0 in
      let step () =
        let input = frame !count in incr count;
        let started = Unix.gettimeofday () in
        environment := E.update !environment input;
        let updated = Unix.gettimeofday () in
        let scene = E.scene !environment input in
        let drawn = Unix.gettimeofday () in
        (match Rays.Canvas.render ~density:2 canvas scene with
         | Ok () -> () | Error message -> failwith message);
        let finished = Unix.gettimeofday () in
        updated -. started, drawn -. updated, finished -. drawn, finished -. started in
      for _ = 1 to 20 do ignore (step ()) done;
      let updates = Array.make samples_count 0. and scenes = Array.make samples_count 0.
      and rendered = Array.make samples_count 0. and totals = Array.make samples_count 0. in
      Gc.full_major ();
      let before = Gc.allocated_bytes () in
      Array.iteri (fun index _ ->
        let update, scene, render, total = step () in
        updates.(index) <- update; scenes.(index) <- scene;
        rendered.(index) <- render; totals.(index) <- total) totals;
      let bytes = Gc.allocated_bytes () -. before in
      report ("particles_" ^ name ^ "_update") 10_000 updates bytes;
      report ("particles_" ^ name ^ "_scene") 10_000 scenes 0.;
      report ("particles_" ^ name ^ "_render") 10_000 rendered 0.;
      report ("particles_" ^ name ^ "_total") 10_000 totals bytes)
    end) setups

let () =
  if Array.length Sys.argv > 1 && List.mem Sys.argv.(1) ["--particles";"--particles-window";"--particles-toggle"] then begin
    print_endline "name,particles,median_s,p95_s,bytes_per_frame";
    measure_particles ~window:(Sys.argv.(1)<>"--particles") ~toggle:(Sys.argv.(1)="--particles-toggle")
      (if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 120)
  end else if Array.to_list Sys.argv = [Sys.argv.(0); "--world"] then begin
    print_endline "name,objects,median_s,p95_s,bytes_per_frame";
    for _ = 1 to 3 do measure_world () done
  end else if Array.length Sys.argv > 1 && Sys.argv.(1) = "--panels" then begin
    print_endline "name,nodes,median_s,p95_s,bytes_per_frame";
    List.iter measure_panels (match Array.to_list Sys.argv with
      | _ :: _ :: (_ :: _ as sizes) -> List.map int_of_string sizes | _ -> [ 200; 800; 2_000 ]);
    measure_spreadsheet ()
  end else begin
  let sizes = match Array.to_list Sys.argv with
    | _ :: (_ :: _ as sizes) -> List.map int_of_string sizes
    | _ -> [200; 2_000] in
  print_endline "name,nodes,median_s,p95_s,bytes_per_frame";
  List.iter measure sizes
  end
