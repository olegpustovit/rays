(* The kit's widgets, its overlays and the graph pane's zone tokens are compared pixel for pixel
   with goldens rendered by this kit (rev 3, specification/pxui.md) with DepartureMono: a change
   of a token or of a widget's geometry shows here.  Each is drawn offscreen at two pixels a
   point, without a window, as tools/ui_shot does.  The goldens are refreshed deliberately,
   with RAYS_UPDATE_FIXTURES=<dir>. *)
open Rays

let new_panel ui =
  Pxui.Ui.panel ui "panel" (fun () ->
    Pxui.Ui.label ui "PXUI";
    ignore (Pxui.Ui.toggle ui "Animate" true);
    ignore (Pxui.Ui.toggle ui "Wire" false);
    ignore (Pxui.Ui.text_field ui "Caption" "Functional UI");
    ignore (Pxui.Ui.button ui "Quit"))

let width = 320 and scale = 2
let background = Rays_math.Color.rgb 228 139 161

(* The second frame of a fresh UI with the pointer parked away from every widget, rendered
   [scale] pixels a point. *)
let render ?(height = 300) build =
  let ui = Pxui.Ui.create () in
  let frame count : Frame.t = {
    input = { (Input.at_time (float count /. 60.)) with dt = 1. /. 60.;
      frame = count; tick = count; size = width, height; pointer = (-100., -100.);
      events = [Input.Pointer_moved (-100., -100.)] };
    pixel_scale = float scale, float scale; fps = 60.; mouse_delta = (0., 0.) } in
  for count = 0 to 1 do Pxui.Ui.frame ui (frame count) build done;
  let canvas = Canvas.create_exn ~width:(width * scale) ~height:(height * scale) in
  Result.get_ok(Canvas.render ~density:scale canvas (Scene.clear background :: Pxui.Ui.scene ui));
  Pxui.Ui.destroy ui;
  canvas

(* Compare [canvas] with fixtures/[golden]; RAYS_UPDATE_FIXTURES=<dir> writes it there instead. *)
let check name golden canvas =
  (match Sys.getenv_opt "RAYS_UPDATE_FIXTURES" with
   | Some target ->
       (match Canvas.save_png canvas (Filename.concat target golden) with
        | Ok () -> () | Error message -> failwith message)
   | None -> ());
  let actual = Result.get_ok (Canvas.to_image canvas) in
  let golden = Image.load_exn (Filename.concat "fixtures" golden) in
  if Image.Private.dimensions golden <> Image.Private.dimensions actual then
    failwith (Printf.sprintf "PXUI %s parity: golden is %dx%d, render is %dx%d" name
      (fst (Image.Private.dimensions golden)) (snd (Image.Private.dimensions golden))
      (fst (Image.Private.dimensions actual)) (snd (Image.Private.dimensions actual)))
  else if Result.get_ok (Image.Private.pixels golden) <> Result.get_ok (Image.Private.pixels actual)
  then failwith (Printf.sprintf "PXUI %s drifted from its golden" name)
  else Printf.printf "PXUI %s parity: exact\n" name;
  ignore(Canvas.destroy canvas)

(* The floating widgets: a modal with a picker, and a context menu. *)
let overlays ui =
  ignore (Pxui.Ui.modal ui ~width:220. "modal" (fun () ->
    Pxui.Ui.label ui "Presets";
    Pxui.Ui.picker ui ~limit:3 "Search presets" ~query:""
      (fun _ -> [| "wall", "09-24"; "tower", "09-23"; "arch", "09-20";
                   "dome", "09-19" |])));
  ignore (Pxui.Ui.context_menu ui ~at:(12., 200.) "context"
    ["View", true; "Set active camera", false; "Delete", true])

(* The graph pane's zone tokens: for, fold, sum, a hollow dashed fn
   zone and a let scope, with the socket colours of lists and functions. *)
let zones ui =
  let theme = Pxui.Ui.theme ui in
  let box = Pxui.Ui.box ui ~w:(Pxui.Ui.Px 320.) ~h:(Pxui.Ui.Px 390.) ~at:(0., 0.) "zones" in
  Pxui.Ui.draw ui box (fun paint _ ->
    let module P = Pxui.Ui.Paint in
    List.iteri (fun i (zone : Pxui.Theme.zone) ->
      let x = 12. +. float (i mod 2) *. 154. and y = 12. +. float (i / 2) *. 92. in
      P.rect paint ~x ~y ~w:142. ~h:80. ~fill:zone.fill ();
      if zone.dashed then P.dashed_rect paint ~x ~y ~w:142. ~h:80. zone.edge
      else P.stroke paint ~x:(x +. 0.5) ~y:(y +. 0.5) ~w:141. ~h:79. zone.edge)
      [ Pxui.Theme.zone_for theme; Pxui.Theme.zone_fold theme; Pxui.Theme.zone_sum theme;
        Pxui.Theme.zone_fn theme; Pxui.Theme.zone_let theme;
        Pxui.Theme.zone_branch theme ~taken:false; Pxui.Theme.zone_branch theme ~taken:true ];
    let ports = Pxui.Theme.ports theme in
    List.iteri (fun i color -> P.rect paint ~x:(170. +. float i *. 20.) ~y:(200. +. 0.) ~w:10. ~h:10.
      ~fill:color ()) [ ports.text; ports.fn; ports.record ])

let table ui =
  Pxui.Ui.table ui ~at:(8., 8.) ~w:304. ~h:280. ~headers:[|"index"; "P.x"; "weight"; "group"|]
    ~rows:1000000 ~cell:(fun row column -> if column = 0 then string_of_int row
      else if column = 3 then if row mod 2 = 0 then "1" else "0"
      else Printf.sprintf "%g" (float (row + column) *. 0.125)) "table"

let run () =
  check "overlays" "kit_overlays_2x.png" (render overlays);
  check "zones" "kit_zones_2x.png" (render ~height:390 zones);
  check "table" "kit_table_2x.png" (render (fun ui -> ignore (table ui)));
  check "kit panel" "kit_panel_2x.png" (render new_panel)

(* Inspect the real painter's packets without creating a GPU device. An
   optional dump lets a refactor compare complete packets before/after. *)
let commands () =
  List.iter (fun density ->
    List.iter (fun (name, height, build) ->
      let ui = Pxui.Ui.create () in
      Fun.protect ~finally:(fun () -> Pxui.Ui.destroy ui) (fun () ->
        for count = 0 to 1 do
          Pxui.Ui.frame ui {
            Frame.input = { (Input.at_time (float count /. 60.)) with
              frame = count; tick = count; size = width, height; dt = 1. /. 60.;
              pointer = (-100., -100.); events = [Input.Pointer_moved (-100., -100.)] };
            pixel_scale = density, density; fps = 60.; mouse_delta = (0., 0.) } build
        done;
        let staged = Result.get_ok (Scene.Private.stage_native ~width ~height (Pxui.Ui.scene ui)) in
        let packets = List.filter_map (function
          | Scene.Private.Ui_layer (batch, _) ->
              assert (Scene_command.Ui_batch.count batch > 0);
              let batches = Scene_command.Ui_batch.batches batch in
              Array.iter (fun (batch:Scene_command.Ui_batch.batch) ->
                assert (batch.first >= 0 && batch.count > 0)) batches;
              Some (Scene_command.Ui_batch.instances batch,
                Array.map (fun (batch:Scene_command.Ui_batch.batch) ->
                  batch.first, batch.count, batch.clip, batch.texture) batches)
          | _ -> None) staged.layers in
        assert (packets <> []);
        Option.iter (fun directory ->
          let path = Filename.concat directory (Printf.sprintf "%s-%gx.packet" name density) in
          let output = open_out_bin path in
          Fun.protect ~finally:(fun () -> close_out output)
            (fun () -> Marshal.to_channel output packets []))
          (Sys.getenv_opt "RAYS_UI_COMMAND_DUMP")))
      ["overlays", 300, overlays; "zones", 390, zones;
       "table", 300, (fun ui -> ignore (table ui)); "panel", 300, new_panel]) [1.; 2.];
  print_endline "PXUI packets: four parity fixtures in logical screen points at 1x/2x"
