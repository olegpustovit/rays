(* The editor's UI over a workspace file as one PNG, without a window: the picture the UI
   redesign is compared with.  usage: ui_shot FILE.rays OUTPUT.png [WIDTH HEIGHT [FRAMES]]
   (UI_SHOT_DO is a script of keys, clicks and drags that puts it in a state first). *)
let () = if Array.length Sys.argv < 3 then failwith "usage: ui_shot FILE.rays OUTPUT.png [WIDTH HEIGHT [FRAMES]]"
let arg index default = if Array.length Sys.argv > index then int_of_string Sys.argv.(index) else default
let width = arg 3 1440 and height = arg 4 900 and frames = arg 5 8
(* UI_SHOT_SCALE=2: the picture a Retina window shows, two pixels a point (glyphs at their 2x
   advances), for comparing with the sheets' @2x renders *)
let scale = Option.value ~default:1 (Option.bind (Sys.getenv_opt "UI_SHOT_SCALE") int_of_string_opt)

let frame count : Rays.Frame.t = {
  input = { (Input.at_time (float_of_int count /. 60.)) with dt = 1. /. 60.;
    frame = count; tick = count; size = width, height; pointer = -100., -100. };
  pixel_scale = float scale, float scale; fps = 60.; mouse_delta = 0., 0.;
}

let () =
  let module E = Rays_editor.Editor in
  let text = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  let workspace = match Result.bind (Rays_editor.Source.read_imports ~file:Sys.argv.(1) text)
      (fun imports -> Rays_editor.Workspace.load ~imports text) with
    | Ok workspace -> workspace
    | Error diagnostics -> failwith (String.concat "\n" (List.map Diagnostic.to_string diagnostics)) in
  let canvas = Rays.Canvas.create_exn ~width:(width * scale) ~height:(height * scale) in
  let editor = E.create ~workspace ~await:true ~presets:(Filename.temp_dir "ui-shot" "")
      (* as rays prepares (Workspace.run): instances, pieces and materials *)
      ~prepare:(fun _ (output : Sop.Session.output) ->
        let ( let* ) = Result.bind in
        let* geometry = Sop.Payload.geometry output.payload
          |> Result.map_error Diagnostic.error_to_string in
        Rdk_rays.Surface.of_geometry ?instances:output.instances geometry)
      ~draw:(fun node surface ->
        let explosion = Rdk_rays.Packed_pieces.explosion
            ~operation:(Sop.Node.operation node) ~fields:(Sop.Node.parameter_fields node) in
        Rdk_rays.Surface.scene3 ?explosion surface) () |> Result.get_ok in
  let editor = ref editor in
  for count = 0 to frames - 1 do editor := E.update !editor (frame count) done;
  (* UI_SHOT_DO is a script run after the settle frames, one step a frame, in order:
       key:NAME        a key press (a letter, a digit or a sign is itself; space tab enter esc)
       select:G/NODE   select a checked lexical path for an inspection artifact
       click:X,Y       move there, press and release the left button
       rclick:X,Y      the same with the right button (a context menu)
       type:TEXT       typed text (no spaces: _ is a space)
       key:ctrl+z      modifiers (shift ctrl alt meta) joined to a key with +, held for that step
       move:X,Y        rest the pointer there (it stays for the picture)
       pinch:X,Y,F     a trackpad pinch of factor F at that point (above 1 zooms in)
       scroll:X,Y,DY   a wheel scroll of DY at that point (negative scrolls the content up)
       hold:X0,Y0,X1,Y1  press at the first point and keep the button down at the second, so
                       the picture shows the drag in flight
       release:X,Y     let the held button go there (the end of a hold)
     e.g. UI_SHOT_DO="key:space key:[ key:1 click:490,95" *)
  let key = function
    | "space" -> Input.Space | "tab" -> Tab | "enter" -> Enter | "esc" -> Escape
    | name -> KeyChar name.[0] in
  let count = ref frames and mouse = ref (-100., -100.) and buttons = ref [] in
  let step ?(keys = []) events =
    let frame = frame !count in
    editor := E.update !editor { frame with input = { frame.input with pointer = !mouse;
      buttons = !buttons; events; keys } };
    incr count in
  let point text = Scanf.sscanf text "%f,%f" (fun x y -> x, y) in
  List.iter (fun word -> match String.index_opt word ':' with
    | None -> ()
    | Some colon ->
        let argument = String.sub word (colon + 1) (String.length word - colon - 1) in
        (match String.sub word 0 colon with
         | "select" ->
             editor := Rays_editor.Reduce.select_path !editor (String.split_on_char '/' argument);
             step []
         | "key" ->
             let parts = String.split_on_char '+' argument in
             let name = List.nth parts (List.length parts - 1) in
             let modifier = function
               | "shift" -> Some Input.Shift | "ctrl" -> Some Input.Ctrl
               | "alt" -> Some Input.Alt | "meta" -> Some Input.Meta | _ -> None in
             let keys = List.filter_map modifier (List.filteri (fun i _ -> i < List.length parts - 1) parts) in
             step ~keys [ Input.Key_pressed (key name) ]
         | "type" -> step [ Input.Text_input (String.map (fun c -> if c = '_' then ' ' else c) argument) ]
         | "rclick" ->
             mouse := point argument;
             step [ Input.Pointer_moved !mouse ];
             step [ Input.Pointer_pressed (Input.RightButton, !mouse) ];
             step [ Input.Pointer_released (Input.RightButton, !mouse) ]
         | "click" ->
             mouse := point argument;
             step [ Input.Pointer_moved !mouse ];
             step [ Input.Pointer_pressed (Input.LeftButton, !mouse); Input.Pointer_released (Input.LeftButton, !mouse) ]
         | "move" -> mouse := point argument; step [ Input.Pointer_moved !mouse ]
         | "pinch" ->
             let (x, y), factor = Scanf.sscanf argument "%f,%f,%f" (fun x y f -> (x, y), f) in
             mouse := (x, y); step [ Input.Pointer_moved !mouse ];
             step [ Input.Pinched factor ]
         | "scroll" ->
             let (x, y), dy = Scanf.sscanf argument "%f,%f,%f" (fun x y d -> (x, y), d) in
             mouse := (x, y); step [ Input.Pointer_moved !mouse; Input.Scrolled (0., dy) ]
         | "hold" ->
             let from, to_ = Scanf.sscanf argument "%f,%f,%f,%f" (fun a b c d -> (a, b), (c, d)) in
             mouse := from; step [ Input.Pointer_moved from ];
             buttons := [ Input.LeftButton ];
             step [ Input.Pointer_pressed (Input.LeftButton, from) ];
             mouse := to_;
             for _ = 1 to 4 do step [ Input.Pointer_moved to_ ] done
         | "release" ->
             mouse := point argument; buttons := [];
             step [ Input.Pointer_released (Input.LeftButton, !mouse) ]
         | other -> failwith ("ui_shot: unknown step " ^ other)))
    (String.split_on_char ' ' (Option.value ~default:"" (Sys.getenv_opt "UI_SHOT_DO")));
  (* let hover delays and cooks settle; a held button stays down *)
  for _ = 1 to 30 do step [] done;
  let frame = frame !count in
  let scene = E.scene !editor { frame with input = { frame.input with pointer = !mouse; buttons = !buttons } } in
  (match Rays.Canvas.render ~density:scale canvas scene with
   | Ok () -> () | Error message -> failwith message);
  (match Rays.Canvas.save_png canvas Sys.argv.(2) with Ok () -> () | Error message -> failwith message);
  E.close !editor; ignore(Rays.Canvas.destroy canvas)
