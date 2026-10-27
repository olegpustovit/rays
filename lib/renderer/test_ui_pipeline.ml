(* The PXUI instance pipeline must light exactly the pixels the Scene2
   triangle path lights for the design kit's square fills, 1-point strokes,
   and axis-aligned marker lines, at 1x and 2x density. *)
let get = function
  | Ok value -> value
  | Error error ->
      failwith (Format.asprintf "%a" Renderer.pp_error error)

let rgba r g b a =
  Int32.logor (Int32.shift_left (Int32.of_int r) 24)
    (Int32.logor (Int32.shift_left (Int32.of_int g) 16)
       (Int32.logor (Int32.shift_left (Int32.of_int b) 8) (Int32.of_int a)))

let fill = rgba 220 227 222 255
let stroke = rgba 34 43 43 180
let accent = rgba 40 95 119 175
let marker = rgba 34 43 43 255

(* x, y, w, h with fill and stroke, as Scene.rect ~fill ~stroke. *)
let shapes = [ 3, 4, 20, 9; 30, 2, 17, 15; 5, 20, 40, 11 ]

let new_batch () =
  let builder = Scene_command.Ui_batch.Builder.create () in
  let rect = Scene_command.Ui_batch.Builder.rect builder in
  List.iter (fun (x, y, w, h) ->
    let x = float x and y = float y and width = float w and height = float h in
    rect ~x ~y ~width ~height ~color:fill ();
    rect ~x ~y ~width ~height ~border_color:stroke ~border:1. ()) shapes;
  rect ~x:5. ~y:24. ~width:17. ~height:3. ~color:accent ();
  (* A width-2 line is a 2-point rect centred on its axis. *)
  rect ~x:21. ~y:22. ~width:2. ~height:7. ~color:marker ();
  rect ~x:51.5 ~y:5. ~width:1. ~height:25. ~color:stroke ();
  rect ~x:49. ~y:17.5 ~width:11. ~height:1. ~color:stroke ();
  Scene_command.Ui_batch.Builder.publish builder

let capture ?(clear = (1., 1., 1., 1.)) execution lower =
  let submission = get (Renderer.Private.begin_submission execution) in
  Fun.protect ~finally:(fun () -> Renderer.Private.cancel submission) (fun () ->
    let batch = get (lower submission) in
    ignore (get (Renderer.Private.step ~clear
      submission [ batch ]));
    get (Renderer.capture execution))

let old_scene () =
  let rect (x, y, w, h) fill stroke =
    Scene_command.Shape2.polygon
      [float x, float y; float (x + w), float y;
       float (x + w), float (y + h); float x, float (y + h)] ~fill ~stroke
    |> Array.to_list in
  let geometries = List.concat_map (fun bounds -> rect bounds (Some fill) (Some stroke)) shapes
    @ rect (5, 24, 17, 3) (Some accent) None
    @ [Scene_command.Shape2.line ~from_:(22., 22.) ~to_:(22., 29.) ~width:2. ~color:marker;
       Scene_command.Shape2.line ~from_:(52., 5.) ~to_:(52., 30.) ~width:1. ~color:stroke;
       Scene_command.Shape2.line ~from_:(49., 18.) ~to_:(60., 18.) ~width:1. ~color:stroke] in
  Result.get_ok (Scene_command.Render_ir.create
    (Array.of_list (List.map (fun geometry -> Scene_command.Render_ir.Geometry geometry) geometries)))

(* Retain the original comparison's allowance for the rectangle corner
   squares, where the older segment stroke overlapped or left a gap.
   Compare every channel everywhere else, including the marker lines. *)
let band_corner density (x, y) =
  let px = (float x +. 0.5) /. float density
  and py = (float y +. 0.5) /. float density in
  List.exists (fun (sx, sy, w, h) ->
    let near edge value = Float.abs (value -. float edge) <= 0.5 in
    (near sx px || near (sx + w) px) && (near sy py || near (sy + h) py)) shapes

let first_difference density width left right =
  let found = ref None in
  Bytes.iteri (fun index byte ->
    let pixel = index / 4 in
    let point = pixel mod width, pixel / width in
    if !found = None && byte <> Bytes.get right index
      && not (band_corner density point) then found := Some point)
    left;
  !found

let run () =
  let scene = old_scene () and ui = new_batch () in
  List.iter (fun density ->
    let configuration = { Renderer.
      logical_width = 64; logical_height = 40;
      drawable_width = 64 * density; drawable_height = 40 * density;
      title = "ui-pipeline"; vsync = false; high_density = true } in
    match Renderer.create_offscreen configuration with
    | Error _ -> print_endline "UI pipeline: skipped (no native Metal device)"
    | Ok execution ->
        Fun.protect ~finally:(fun () ->
          ignore (Renderer.destroy execution)) (fun () ->
          let old = capture execution (fun submission ->
            Renderer.Private.lower_scene2 submission ~density
              ~resource:(fun _ -> None) scene)
          and current = capture execution (fun submission ->
            Renderer.Private.lower_ui submission ~density
              ~resource:(fun _ -> None) ui) in
          let width = configuration.drawable_width in
          let length = width * configuration.drawable_height * 4 in
          assert (Bytes.length old = length && Bytes.length current = length);
          let painted = ref 0 in
          Bytes.iteri (fun index channel ->
            if index mod 4 <> 3 && channel <> '\255' then incr painted) current;
          assert (!painted > 100);
          (match first_difference density width old current with
           | None -> ()
           | Some (x, y) -> failwith (Printf.sprintf
               "UI pipeline differs from Scene2 at %dx pixel (%d,%d)" density x y));
          Printf.printf "UI pipeline matches Scene2 at %dx\n%!" density))
    [ 1; 2 ]
