let require condition message = if not condition then failwith message
let get = function
  | Ok value -> value
  | Error error -> failwith (Format.asprintf "%a" Renderer.pp_error error)
let get_ir = function
  | Ok value -> value
  | Error _ -> failwith "scene2 coordinate IR"

let configuration ~logical_width ~logical_height ~drawable_width ~drawable_height =
  { Renderer.
    logical_width; logical_height; drawable_width; drawable_height;
    title = "scene2-coordinates"; vsync = false; high_density = true }

let rgba bytes ~width x y =
  let offset = (y * width + x) * 4 in
  Char.code (Bytes.get bytes offset),
  Char.code (Bytes.get bytes (offset + 1)),
  Char.code (Bytes.get bytes (offset + 2)),
  Char.code (Bytes.get bytes (offset + 3))

let filled (r, g, b, _) = r > 200 && g > 200 && b > 200

let bbox bytes ~width ~height =
  let min_x = ref width and min_y = ref height
  and max_x = ref (-1) and max_y = ref (-1) in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if filled (rgba bytes ~width x y) then begin
        if x < !min_x then min_x := x;
        if y < !min_y then min_y := y;
        if x > !max_x then max_x := x;
        if y > !max_y then max_y := y
      end
    done
  done;
  require (!max_x >= 0) "expected filled pixels";
  !min_x, !min_y, !max_x - !min_x + 1, !max_y - !min_y + 1

let white = 0xffffffffl

let square_geometry =
  { Scene_command.Render_ir.vertices = [| 8.; 8.; 24.; 8.; 24.; 24.; 8.; 24. |];
    indices = [| 0; 1; 2; 0; 2; 3 |]; color = white }

let circle_geometry =
  let cx = 32. and cy = 16. and radius = 8. and steps = 32 in
  let vertices = Array.make ((steps + 1) * 2) 0. in
  vertices.(0) <- cx; vertices.(1) <- cy;
  for i = 0 to steps - 1 do
    let angle = float i *. (2. *. Float.pi) /. float steps in
    vertices.((i + 1) * 2) <- cx +. radius *. cos angle;
    vertices.((i + 1) * 2 + 1) <- cy +. radius *. sin angle
  done;
  let indices = Array.init (steps * 3) (fun i ->
    match i mod 3 with
    | 0 -> 0
    | 1 -> 1 + i / 3
    | _ -> let next = 2 + i / 3 in if next > steps then 1 else next)
  in
  { Scene_command.Render_ir.vertices; indices; color = white }

let with_target ~logical_width ~logical_height ~drawable_width ~drawable_height f =
  match Renderer.create_offscreen
    (configuration ~logical_width ~logical_height ~drawable_width ~drawable_height)
  with
  | Error _ -> print_endline "scene2 coordinates: skipped (no native Metal device)"
  | Ok execution ->
      Fun.protect
        ~finally:(fun () -> ignore (Renderer.destroy execution))
        (fun () -> f execution)

let expect_square bytes ~width ~height ~x ~y ~size message =
  let bx, by, bw, bh = bbox bytes ~width ~height in
  require (bx = x && by = y && bw = size && bh = size)
    (Printf.sprintf
       "%s: expected square %dx%d at (%d,%d), got %dx%d at (%d,%d)"
       message size size x y bw bh bx by)

let run () =
  let run ~logical_width ~logical_height ~drawable_width ~drawable_height ~scale =
    with_target ~logical_width ~logical_height ~drawable_width ~drawable_height
      (fun execution ->
        let facts = get (Runtime.presentation_facts (Renderer.Native.runtime execution)) in
        require (facts.logical_width = logical_width
          && facts.logical_height = logical_height
          && facts.drawable_width = drawable_width
          && facts.drawable_height = drawable_height)
          "offscreen logical/drawable facts";
        let render commands =
          let ir = get_ir (Scene_command.Render_ir.create commands) in
          let pixels = Test_ui_pipeline.capture ~clear:(0.,0.,0.,1.) execution
            (fun submission -> Renderer.Native.lower_scene2 submission
              ~density:scale ~resource:(fun _ -> None) ir) in
          require (Bytes.length pixels = drawable_width * drawable_height * 4)
            "coordinate readback length";
          pixels in
        let square = render [|Scene_command.Render_ir.Geometry square_geometry|] in
        expect_square square ~width:drawable_width ~height:drawable_height
          ~x:(8 * scale) ~y:(8 * scale) ~size:(16 * scale) "logical square";
        let clipped = render Scene_command.Render_ir.[|
          Push_clip {x=12.;y=12.;width=8.;height=8.}; Geometry square_geometry; Pop_clip|] in
        expect_square clipped ~width:drawable_width ~height:drawable_height
          ~x:(12 * scale) ~y:(12 * scale) ~size:(8 * scale) "logical clip";
        (* The square at (8,8) lies outside this tall clip: a clip used as a
           viewport would remap it into the panel and leave fill there. *)
        let tall = { Scene_command.Render_ir.x = 40.; y = 0.; width = 24.; height = 32. } in
        let outside = render Scene_command.Render_ir.[|
          Push_clip tall; Geometry square_geometry; Pop_clip|] in
        let leaked = ref 0 in
        for y = 0 to drawable_height - 1 do
          for x = 0 to drawable_width - 1 do
            if rgba outside ~width:drawable_width x y <> (0, 0, 0, 255) then incr leaked
          done
        done;
        require (!leaked = 0)
          (Printf.sprintf "%dx clip used as viewport leaked out-of-clip fill" scale);
        let inside = render Scene_command.Render_ir.[|
          Push_clip tall;
          Geometry { vertices = [| 44.; 8.; 60.; 8.; 60.; 24.; 44.; 24. |];
            indices = [| 0; 1; 2; 0; 2; 3 |]; color = white };
          Pop_clip|] in
        expect_square inside ~width:drawable_width ~height:drawable_height
          ~x:(44 * scale) ~y:(8 * scale) ~size:(16 * scale)
          (Printf.sprintf "%dx clipped square" scale);
        let circle = render [|Scene_command.Render_ir.Geometry circle_geometry|] in
        expect_square circle ~width:drawable_width ~height:drawable_height
          ~x:(24 * scale) ~y:(8 * scale) ~size:(16 * scale) "logical circle bounds";
        require (filled (rgba circle ~width:drawable_width (32 * scale) (16 * scale)))
          "circle center unfilled";
        require (not (filled (rgba circle ~width:drawable_width (24 * scale) (8 * scale))))
          "circle bounding-box corner filled";
        Printf.printf "scene2 coordinates: %dx square, clip and circle checked\n%!" scale)
  in
  run ~logical_width:64 ~logical_height:32 ~drawable_width:64 ~drawable_height:32 ~scale:1;
  run ~logical_width:64 ~logical_height:32 ~drawable_width:128 ~drawable_height:64 ~scale:2
