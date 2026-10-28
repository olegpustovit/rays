open Sop

type mode = Cube | Sphere

type parameters = {
  count : int;
      [@sop.default 2]
      [@sop.label "Count"]
      [@sop.folder "Geometry"]
      [@sop.min 1]
      [@sop.max 5]
      [@sop.hard_min 1]
      [@sop.hard_max 10]
  enabled : bool; [@sop.default true] [@sop.folder "Geometry"]
  amount : float;
      [@sop.default 0.5] [@sop.folder "Look/Noise"] [@sop.min 0.] [@sop.max 1.] [@sop.impact "view"]
  title : string; [@sop.default "Node SOP"] [@sop.folder "Look"] [@sop.impact "export"]
  mode : mode;
      [@sop.default Cube]
      [@sop.folder "Geometry"]
      [@sop.kind Param.choice ~equal:( = ) [ ("Cube", Cube); ("Sphere", Sphere) ]]
  internal : int; [@sop.default 17] [@sop.ignore]
}
[@@deriving sop_params]

let inspectable_node values =
  Result.get_ok
  @@ Custom.node ~label:"Selected shape" ~operation:"selected-shape" ~schema:parameters_schema
       ~values [] (fun ~label:_ ~inputs:_ ~parameters ->
         Result.ok
         @@
         match parameters.mode with
         | Cube ->
             Lisp_sop.node
               (Printf.sprintf
                  {|(sop/box :normals "Auto" :connectivity "Triangles" :x_divisions %d)|}
                  parameters.count)
         | Sphere ->
             Lisp_sop.node
               (Printf.sprintf
                  {|(sop/uv_sphere
   :radius_x_mode "Auto"
   :radius_y_mode "Auto"
   :radius_z_mode "Auto"
   :normals_mode "Auto"
   :uv_attribute ""
   :segments %d)|}
                  (max 3 parameters.count)))

open Test_support

let dynamic_schema () =
  let module Dynamic = Sop_params_fixture.Dynamic_schema_fixture in
  let base = Node.relabel "Base" (Lisp_sop.node {|(sop/box)|})
  and extra = Node.relabel "Extra" (Lisp_sop.node {|(sop/box :center [2.0 0.0 0.0])|}) in
  let instance inputs = Edit_graph.instantiate Dynamic.factory inputs |> Result.get_ok in
  let select index node =
    fst (Node.apply_parameters node [ ("input", Param.Int_value index) ] |> Result.get_ok)
  in
  let typed = select 1 (instance [ base; extra ]) in
  let factory = typed in
  let choices node =
    match Node.parameter_fields node with
    | [
     { Param.name = "input"; kind = Param.Choice_view labels; current = Param.Int_value index; _ };
    ] ->
        (labels, index)
    | _ -> fail "generated dynamic schema did not retain labelled integer choices"
  in
  assert (choices typed = ([| "0 · Base"; "1 · Extra" |], 1));
  assert ((List.hd (Node.parameter_fields typed)).Param.default = Param.Int_value 0);
  let current =
    Lisp_sop.node
      ~with_:[ ("base", base); ("extra", extra) ]
      "(sop/switch (sop/ext_base) (sop/ext_extra) :input 1)"
  in
  assert ((List.hd (Node.parameter_fields current)).Param.default = Param.Int_value 0);
  let port = Flow_sop.Port.parameters (Node.parameter_fields typed) |> Result.get_ok |> List.hd in
  assert (port.Flow_sop.Port.ty = Some Flow.Port_type.Int);
  let edited =
    fst (Node.apply_parameters typed [ ("input", Param.Int_value 0) ] |> Result.get_ok)
  in
  assert (Node.id edited = Node.id typed);
  assert (choices edited = ([| "0 · Base"; "1 · Extra" |], 0));
  let renamed = Node.Build.rebuild_with_inputs typed [| base; Node.relabel "Renamed" extra |] in
  assert (Node.id renamed = Node.id typed);
  assert (choices renamed = ([| "0 · Base"; "1 · Renamed" |], 1));
  assert (Node.parameter_key renamed = Node.parameter_key typed);
  let grown = Node.Build.rebuild_with_inputs edited [| base; extra; base |] in
  assert (choices grown = ([| "0 · Base"; "1 · Extra"; "2 · Base" |], 0));
  assert (Node.id grown = Node.id edited);
  let grown_typed = instance [ base; extra; base ] in
  assert (Node.parameter_key grown_typed = Node.parameter_key grown);
  assert (Result.is_error (Node.apply_parameters typed [ ("input", Param.Int_value 2) ]));
  assert (Result.is_error (Edit_graph.instantiate Dynamic.factory []));
  assert (Result.is_error (Node.apply_parameters typed [ ("input", Param.Int_value (-1)) ]));
  assert (
    match Node.Build.rebuild_with_inputs typed [| base |] with
    | _ -> false
    | exception Invalid_argument _ -> true);
  let bad =
    Sop.Custom.plain ~label:"Bad" ~operation:"test_error" [] (fun ~context:_ _ ->
        Error "unselected branch was cooked")
  in
  let selected = select 1 (instance [ bad; extra ]) in
  List.iter
    (fun domains ->
      let context = Context.create ~domains ~grain:97 ~seed:42L () |> Result.get_ok in
      let session = Session.create ~max_entries:16 ~max_payload_bytes:10_000_000 |> Result.get_ok in
      Fun.protect
        ~finally:(fun () -> Session.close session)
        (fun () ->
          let cook node =
            match Session.cook session ~context node with
            | Ok output ->
                Rdk.Packed.Float3.View.view
                  (Rdk.Geometry.positions (Result.get_ok (Sop.Payload.geometry output.payload)))
            | Error error -> fail (Diagnostic.error_to_string error)
          in
          assert (cook typed = cook extra);
          assert (cook factory = cook extra);
          assert (cook edited = cook base);
          assert (cook selected = cook extra);
          let before = Session.stats session in
          ignore (cook renamed);
          assert ((Session.stats session).misses = before.misses)))
    [ 1; 4 ]

let run () =
  dynamic_schema ();
  let wire = Rays_editor.Renderer.wire_color in
  assert (wire (Rays_math.Color.rgb 250 250 250) = Rays_math.Color.hex_exn "#285f77");
  assert (wire (Rays_math.Color.rgb 8 8 10) = Rays_math.Color.hex_exn "#bed7e1");
  (match Sop_params_fixture.Editor.factories with
  | [ factory ] ->
      assert (Edit_graph.factory_key factory = "vector_fixture");
      assert (Edit_graph.factory_slot_names factory = [ "input"; "target" ]);
      let node = Edit_graph.instantiate_optional factory [ None; None ] |> Result.get_ok in
      let fields = Node.parameter_fields node in
      assert (
        List.map (fun field -> field.Param.vec3) fields
        = [ Some ("center", 0); Some ("center", 1); Some ("center", 2) ]);
      (match List.map (fun field -> field.Param.kind) fields with
      | [ Param.Floating_view x; Floating_view y; Floating_view z ] -> assert (x == y && y == z)
      | _ -> fail "PPX vector fields lost their floating ranges");
      assert (List.map (fun field -> field.Param.primary) fields = [ true; false; false ])
  | _ -> fail "PPX module alias did not produce the single registry entry");
  let module Rest = Sop_params_fixture.Optional_rest_fixture in
  let base = Lisp_sop.node {|(sop/box)|}
  and extra = Lisp_sop.node {|(sop/box :center [2.0 0.0 0.0])|} in
  List.iter
    (fun (fixed, extras, slots) ->
      let typed = Edit_graph.instantiate_optional Rest.factory slots |> Result.get_ok in
      assert (
        List.map Node.id (Node.inputs typed)
        = List.map Node.id ((base :: Option.to_list fixed) @ extras));
      let edited =
        fst (Node.apply_parameters typed [ ("tag", Param.Text_value "sources") ] |> Result.get_ok)
      in
      assert (Node.id edited = Node.id typed);
      assert (List.map Node.id (Node.inputs edited) = List.map Node.id (Node.inputs typed)))
    [
      (None, [], [ Some base; None ]);
      (None, [ extra; base ], [ Some base; None; Some extra; None; Some base ]);
      (Some extra, [ base; extra ], [ Some base; Some extra; Some base; Some extra ]);
    ];
  if
    Sop_params_fixture.t_default.value <> 3
    || List.length (Param.fields Sop_params_fixture.t_schema) <> 1
  then fail "sop_params interface generation did not match implementation";
  if
    parameters_default
    <> { count = 2; enabled = true; amount = 0.5; title = "Node SOP"; mode = Cube; internal = 17 }
    || parameters_default.internal <> 17
    || List.length (Param.fields parameters_schema) <> 5
  then fail "sop_params defaults or ignored-field policy is incorrect";
  let graph = inspectable_node parameters_default in
  let ui = Pxui.Ui.create ~font_size:11 () in
  let frame time events : Rays.Frame.t =
    {
      input = { (Input.at_time time) with size = (320, 240); events };
      pixel_scale = (1., 1.);
      fps = 0.;
      mouse_delta = (0., 0.);
    }
  in
  let step graph time events =
    Pxui.Ui.frame ui (frame time events) (fun ui ->
        Pxui.Ui.panel ui ~x:0. ~y:0. ~width:260. "inspector" (fun () ->
            match
              Pxui.Ui.scope ui "selected" (fun () ->
                  Pxui.Ui.label ui (Node.label graph);
                  match
                    Pxui_shell.Inspector.fields ui ~expanded:[ "Geometry" ]
                      (Node.parameter_fields graph)
                  with
                  | [] -> Ok (graph, Param.no_effects)
                  | changes -> Node.apply_parameters graph changes)
            with
            | Ok value -> value
            | Error message -> fail message))
  in
  let unchanged, effects = step graph 0. [] in
  if unchanged != graph || effects.cook then fail "an idle inspector frame changed the graph";
  (* The shared inspector rows are 24 points, below the label and the folder's section header
     (24 under 16 points of space). *)
  let row index = 24 + 40 + ((index - 2) * 24) + 12 in
  let click x y =
    [
      Input.Pointer_pressed (Input.LeftButton, (float x, float y));
      Input.Pointer_released (Input.LeftButton, (float x, float y));
    ]
  in
  let graph, effects = step graph 0.5 (click 110 (row 3)) in
  let field graph name =
    Node.parameter_fields graph |> List.find (fun field -> field.Param.name = name)
  in
  if (not effects.cook) || (field graph "enabled").current <> Param.Bool_value false then
    fail "inspector toggle did not apply through the node schema";
  let graph, _ = step graph 1. (click 170 (row 4)) in
  let graph, _ = step graph 1.05 [] in
  let graph, _ = step graph 1.1 (click 170 (row 4 + 54)) in
  if (field graph "mode").current <> Param.Choice_value "Sphere" then
    fail "inspector choice did not apply";
  let graph, _ =
    step graph 1.5
      [
        Input.Pointer_pressed (Input.LeftButton, (140., float (row 2)));
        Input.Pointer_moved (400., float (row 2));
        Input.Pointer_released (Input.LeftButton, (400., float (row 2)));
      ]
  in
  if (field graph "count").current <> Param.Int_value 5 then
    fail "inspector integer drag did not clamp to the soft range";
  let changes =
    Node.parameter_fields graph |> List.map (fun field -> (field.Param.name, field.default))
  in
  let graph, effects =
    match Node.apply_parameters graph changes with
    | Ok value -> value
    | Error message -> fail message
  in
  if
    (not effects.cook)
    || (field graph "count").current <> Param.Int_value 2
    || (field graph "mode").current <> Param.Choice_value "Cube"
  then fail "selected-node reset did not restore PPX defaults";
  let normalized =
    match
      Node.apply_parameters (inspectable_node parameters_default) [ ("count", Param.Int_value 99) ]
    with
    | Ok (node, _) -> node
    | Error message -> fail message
  in
  if
    (Node.parameter_fields normalized |> List.find (fun field -> field.Param.name = "count"))
      .current <> Param.Int_value 10
  then fail "hard range normalization changed";
  let flow_ui = Pxui.Ui.create ~font_size:11 () in
  let count = { (field graph "count") with Param.folder = [] } in
  let flow_row ?drive ?(locked = false) () : Pxui_shell.Inspector.flow_row =
    { path = "count"; fields = [ count ]; shown = true; locked; drive; live = Some "3" }
  in
  let flow_step ?(expanded = []) row events =
    let settle = frame 2. [] in
    ignore
      (Pxui.Ui.frame flow_ui settle (fun ui ->
           Pxui_shell.Inspector.flow_fields ui ~expanded ~pin_click:true [ row ]));
    Pxui.Ui.frame flow_ui (frame 2. events) (fun ui ->
        Pxui_shell.Inspector.flow_fields ui ~expanded ~pin_click:true [ row ])
  in
  if flow_step (flow_row ()) (click 14 12) <> [ Pxui_shell.Inspector.Pinned ("count", false) ] then
    fail "Flow inspector pin did not emit the card-row request";
  (* the cross shows on the hovered row: hover first *)
  ignore
    (flow_step (flow_row ~drive:"← Clock.t" ~locked:true ()) [ Input.Pointer_moved (258., 12.) ]);
  if
    flow_step (flow_row ~drive:"← Clock.t" ~locked:true ()) (click 258 12)
    <> [ Pxui_shell.Inspector.Reset "count" ]
  then fail "Flow inspector reset did not clear the drive";
  let expression_row = flow_row ~drive:"=t" ~locked:true () in
  ignore (flow_step expression_row (click 170 12));
  let edits =
    Pxui.Ui.frame flow_ui
      (frame 2.
         [ Input.Key_pressed Input.End; Input.Text_input "*2"; Input.Key_pressed Input.Enter ])
      (fun ui -> Pxui_shell.Inspector.flow_fields ui [ expression_row ])
  in
  if edits <> [ Pxui_shell.Inspector.Expression ("count", "=t*2") ] then
    fail "Flow inspector expression field did not commit an edit";
  let box = Lisp_sop.node {|(sop/box)|} in
  let vector =
    Flow_sop.Port.parameters (Node.parameter_fields box)
    |> Result.get_ok
    |> List.find (fun (parameter : Flow_sop.Port.parameter) -> parameter.path = "size")
  in
  let vector_row : Pxui_shell.Inspector.flow_row =
    {
      path = vector.path;
      fields = List.map (fun field -> { field with Param.folder = [] }) vector.fields;
      shown = true;
      locked = false;
      drive = None;
      live = None;
    }
  in
  if flow_step vector_row (click 258 12) <> [] then
    fail "a click beside a vector's cells changed the row";
  let color_f name current =
    {
      Param.name;
      label = String.capitalize_ascii name;
      description = None;
      folder = [];
      impact = Param.Cook;
      primary = false;
      unit = None;
      vec3 = Some ("color", 0);
      kind =
        Param.Floating_view { soft_min = 0.; soft_max = 1.; hard_min = Some 0.; hard_max = Some 1. };
      default = Param.Float_value 1.;
      current = Param.Float_value current;
    }
  in
  let color_row : Pxui_shell.Inspector.flow_row =
    {
      path = "color";
      fields = [ color_f "color_r" 1.; color_f "color_g" 0.5; color_f "color_b" 0. ];
      shown = true;
      locked = false;
      drive = None;
      live = None;
    }
  in
  (* the hex field starts after the 20-point swatch and 8 points, at 12 + 6 + 8 + 68 + 8 + 28 in 280 *)
  ignore (flow_step color_row (click 170 12));
  let hex_edits =
    Pxui.Ui.frame flow_ui
      (frame 2.
         [
           Input.Key_pressed Input.End;
           Input.Key_pressed Input.Backspace;
           Input.Key_pressed Input.Backspace;
           Input.Key_pressed Input.Backspace;
           Input.Key_pressed Input.Backspace;
           Input.Key_pressed Input.Backspace;
           Input.Key_pressed Input.Backspace;
           Input.Key_pressed Input.Backspace;
           Input.Key_pressed Input.Backspace;
           Input.Text_input "#00ff00";
           Input.Key_pressed Input.Enter;
         ])
      (fun ui -> Pxui_shell.Inspector.flow_fields ui [ color_row ])
  in
  if not (List.mem (Pxui_shell.Inspector.Edited ("color_g", Param.Float_value 1.)) hex_edits) then
    fail "Flow inspector color row hex edit did not update color channels";
  let folder_row =
    { (flow_row ()) with fields = [ { count with Param.folder = [ "Geometry" ] } ] }
  in
  if
    flow_step ~expanded:[ "Geometry" ] folder_row (click 14 40)
    <> [ Pxui_shell.Inspector.Pinned ("count", false) ]
  then fail "Flow inspector did not show an initially expanded folder";
  ignore (flow_step ~expanded:[ "Geometry" ] folder_row (click 10 16));
  if flow_step ~expanded:[ "Geometry" ] folder_row (click 14 40) <> [] then
    fail "Flow inspector collapsed folder kept its field interactive";
  ignore (flow_step ~expanded:[ "Geometry" ] folder_row (click 10 16));
  if
    flow_step ~expanded:[ "Geometry" ] folder_row (click 14 40)
    <> [ Pxui_shell.Inspector.Pinned ("count", false) ]
  then fail "Flow inspector folder did not reopen";
  (match Sys.getenv_opt "RAYS_UI_PREVIEW" with
  | None -> ()
  | Some directory ->
      ignore
        (Result.get_ok
           (Rays.Sketch.export ~directory ~prefix:"flow-inspector" ~frames:1
              ~config:{ Rays.Sketch.default_config with width = 320; height = 240 } (fun _ ->
                Rays.Scene.clear (Rays_math.Color.hex_exn "#eef2ee") :: Pxui.Ui.scene flow_ui))));
  Pxui.Ui.destroy flow_ui;
  Pxui.Ui.destroy ui;
  print_endline "SOP UI tests passed"
