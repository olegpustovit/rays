(* The internal OCaml layer's value contracts: math, random and noise,
   camera lens, materials and lights, frame facts, 2D staging order and
   resource ownership. *)
open Rays

open Test_support

let sketch_startup_result () =
  let called = ref false in
  let config = { Sketch.default_config with width = 0 } in
  match Sketch.run_state ~config ~init:(fun _ -> ())
      ~update:(fun state _ -> state)
      ~view:(fun _ _ -> called := true; Scene.[ ]) () with
  | Error { Ogpu.Error.kind = Ogpu.Error.Invalid_argument; _ } ->
      check (not !called) "invalid sketch configuration reached user callbacks"
  | Error error -> failwith (Ogpu.Error.to_string error)
  | Ok () -> failwith "invalid sketch configuration succeeded"

let math () =
  let normal = Rays_math.Vec3.cross Rays_math.Vec3.unit_x Rays_math.Vec3.unit_y in
  check (Rays_math.Vec3.nearly_equal normal Rays_math.Vec3.unit_z ~eps:1e-12) "Vec3 parity";
  check (Rays_math.Color.hex "#f08c" = Ok (Rays_math.Color.rgba 255 0 136 204)) "Color hex";
  check (Rays_math.Color.gradient [ Rays_math.Color.red; Rays_math.Color.rgb 0 0 255 ] 0.5 = Rays_math.Color.rgba 128 0 128 255)
    "Color gradient";
  let m4 = Rays_math.Mat4.mul (Rays_math.Mat4.translation (Rays_math.Vec3.create 2. 3. 4.))
      (Rays_math.Mat4.scaling (Rays_math.Vec3.create 2. 2. 2.)) in
  check (Rays_math.Vec3.nearly_equal (Rays_math.Mat4.transform_point m4 (Rays_math.Vec3.create 1. 1. 1.))
           (Rays_math.Vec3.create 4. 5. 6.) ~eps:1e-12) "Mat4 affine";
  check (match Rays_math.Mat4.inverse m4 with
      | Some inverse -> Rays_math.Mat4.nearly_equal (Rays_math.Mat4.mul inverse m4) Rays_math.Mat4.identity ~eps:1e-12
      | None -> false) "Mat4 inverse"

let random () =
  let a = Rays_math.Rand.seed64 0x123456789abcdefL and b = Rays_math.Rand.seed64 0x123456789abcdefL in
  let left = Array.init 100_000 (fun index -> Rays_math.Rand.float_at a ~index)
  and right = Array.init 100_000 (fun index -> Rays_math.Rand.float_at b ~index) in
  check (left = right && Array.for_all (fun x -> x >= 0. && x < 1.) left)
    "Rand deterministic 100k";
  let noise = Rays_math.Noise.create 42 in
  let xs = Array.init 100_000 (fun i -> float i *. 0.001)
  and ys = Array.init 100_000 (fun i -> float (i mod 97) *. 0.01) in
  let sequential = Array.make 100_000 0. and parallel = Array.make 100_000 0. in
  Rays_math.Noise.sample2_into noise ~first:0 ~last:100_000 ~frequency:0.5 ~x:xs ~y:ys
    ~output:sequential;
  let workers = Array.init 4 (fun domain -> Domain.spawn (fun () ->
      let first = domain * 25_000 in
      Rays_math.Noise.sample2_into noise ~first ~last:(first + 25_000) ~frequency:0.5 ~x:xs ~y:ys
        ~output:parallel)) in
  Array.iter Domain.join workers;
  check (sequential = parallel && Array.for_all Float.is_finite parallel)
    "Noise one/four-domain 100k";
  Array.fill parallel 0 100_000 0.;
  Parallel.run ~domains:8 (fun () ->
    Parallel.for_ ~chunk_size:1 ~start:0 ~finish:7 (fun chunk ->
      let first = chunk * 12_500 in
      Rays_math.Noise.sample2_into noise ~first ~last:(first + 12_500)
        ~frequency:0.5 ~x:xs ~y:ys ~output:parallel));
  check (sequential = parallel) "Noise one/eight-domain exact 2D plane";
  (* The packed 2D sampler is the z = 0 plane of the 3D noise, bit for bit,
     boundary coordinates included. *)
  for index = 0 to Array.length sequential - 1 do
    check (Int64.bits_of_float sequential.(index)
           = Int64.bits_of_float (Rays_math.Noise.sample3 noise
               ~x:(xs.(index) *. 0.5) ~y:(ys.(index) *. 0.5) ~z:0.))
      "packed 2D noise differs from original 3D plane"
  done;
  List.iter (fun seed ->
    let noise = Rays_math.Noise.create seed in
    let coordinates = [| 0.; -0.; Float.epsilon; -.Float.epsilon; 1.; -1.;
      Float.pred 1.; Float.succ 1.; 255.; -256.; 1e20; -1e20 |] in
    let count = Array.length coordinates in
    let x = Array.init (count * count) (fun i -> coordinates.(i / count))
    and y = Array.init (count * count) (fun i -> coordinates.(i mod count)) in
    let output = Array.make (count * count) 0. in
    Rays_math.Noise.sample2_into noise ~first:0 ~last:(count * count) ~frequency:1. ~x ~y ~output;
    Array.iteri (fun i value ->
      check (Int64.bits_of_float value
             = Int64.bits_of_float (Rays_math.Noise.sample3 noise ~x:x.(i) ~y:y.(i) ~z:0.))
        "Noise 2D boundary bits differ from original 3D plane") output)
    [ 0; 1; 42; -1; 1999 ];
  check (Rays_math.Noise.sample3 noise ~x:0.25 ~y:0.5 ~z:0.75
         = Rays_math.Noise.sample3 (Rays_math.Noise.create 42) ~x:0.25 ~y:0.5 ~z:0.75) "Noise seed";
  Parallel.release_current_domain_pools ()

let camera () =
  (* A camera's lens: pinhole by default, focus on the target unless set. *)
  let camera = Camera.perspective ~at:(Rays_math.Vec3.create 0. 0. 4.) ~target:Rays_math.Vec3.zero () in
  check (Camera.lens camera = Camera.pinhole && Camera.focus_distance camera = 4.)
    "Camera default lens";
  let lens = { Camera.aperture = 0.25; focus_distance = Some 2.5 } in
  let focused = Camera.with_lens lens camera in
  check (Camera.lens focused = lens && Camera.focus_distance focused = 2.5
         && Camera.focus_distance
              (Camera.with_lens { lens with focus_distance = None } focused) = 4.)
    "Camera.with_lens";
  (match Camera.with_lens { lens with aperture = -1. } camera with
   | _ -> failwith "Camera.with_lens accepted a negative aperture"
   | exception Invalid_argument _ -> ());
  let material = Material.matte (Rays_math.Color.rgb 10 20 30) in
  check (material.diffuse = Rays_math.Color.rgb 10 20 30) "Material matte";
  let light = Light.spot ~at:(Rays_math.Vec3.create 1. 2. 3.)
      ~direction:(Rays_math.Vec3.create 0. 0. (-1.)) ~cutoff:0.5 ~concentration:4. () in
  check (light.intensity = 1.) "Light intensity";
  let camera = Camera.perspective ~at:(Rays_math.Vec3.create 0. 0. 5.) ~target:Rays_math.Vec3.zero () in
  check (Option.is_some (Camera.world_to_screen ~viewport:(0, 0, 64, 64) camera Rays_math.Vec3.zero))
    "Camera projection";
  let easy = Easy_camera.create ~target:Rays_math.Vec3.zero ~distance:5. () in
  check (Easy_camera.distance easy = 5.) "Easy_camera distance"

let frame_and_scene () =
  (match Sketch.set_cursor `Horizontal_resize with
   | Error _ -> ()
   | Ok () -> failwith "cursor without an active sketch");
  let open Input in
  let events = [ Input.Key_pressed Space; Pointer_pressed (LeftButton, (2., 3.)); Pointer_moved (4., 8.);
    Scrolled (1., -2.); Text_editing { text = "ime"; start = 0; length = 3 };
    Resized (8, 9); Closed ] in
  let frame = { Frame.input = { (Input.at_time 1.) with size = 4, 3; dt = 0.5; frame = 7; tick = 7;
      pointer = 4., 8.; keys = [ Space ]; buttons = [ LeftButton; MouseX2 ]; events };
    pixel_scale = 2., 2.; fps = 2.; mouse_delta = 3., 6. } in
  check (Frame.mouse_down LeftButton frame && Frame.mouse_down MouseX2 frame
         && not (Frame.mouse_down RightButton frame)
         && Frame.has_event (function Input.Pointer_moved _ -> true | _ -> false) frame
         && Frame.has_event (function Input.Closed -> true | _ -> false) frame
         && not (Frame.has_event (function Input.Focus_lost -> true | _ -> false) frame))
    "frame facts";
  let scene = [ Scene.clear Rays_math.Color.black; Scene.rect ~at:(0, 0) ~w:4 ~h:3 ~fill:Rays_math.Color.red ();
    Scene.text_input_region ~at:(1, 1) ~w:2 ~h:1 ~focused:true ~cursor:1 () ] in
  let ir = Result.get_ok (Scene.Layer.to_ir scene) in
  check (Array.length (Scene_command.Render_ir.commands_readonly ir) = 2
         && Scene.Layer.text_regions scene = [ 1, 1, 2, 1, true, 1 ]) "scene lowering";
  (match Scene.text_input_region ~at:(0, 0) ~w:1 ~h:1 ~cursor:(-1) () with
   | _ -> failwith "negative IME cursor accepted"
   | exception Invalid_argument _ -> ());
  let open_path = Path.of_commands [|
    Path.Move_to { x = 0.25; y = 0.5 };
    Path.Line_to { x = 4.5; y = 0.5 };
    Path.Line_to { x = 4.5; y = 4.75 };
    Path.Line_to { x = 0.25; y = 4.75 };
  |] in
  let closed = Path.of_commands [|
    Path.Move_to { x = 0.25; y = 0.5 };
    Path.Line_to { x = 4.5; y = 0.5 };
    Path.Line_to { x = 4.5; y = 4.75 };
    Path.Line_to { x = 0.25; y = 4.75 };
    Path.Close;
  |] in
  check (match Path.tessellate ~tolerance:0.25 ~fill_rule:Path.Non_zero closed with
    | Ok _ -> true | Error _ -> false) "Path tessellates closed commands";
  (match Path.stroke ~tolerance:0.25 ~width:1. ~cap:Path.Butt ~join:Path.Miter
      ~miter_limit:4. open_path with
   | Ok mesh -> check (mesh.vertices.(0).x = 0.25) "Path retains subpixel coordinates"
   | Error _ -> failwith "Path stroke");
  let red = Rays_math.Color.red and blue = Rays_math.Color.rgb 0 0 255 in
  let texture = Texture.init ~width:2 ~height:2 (fun ~x ~y ->
      match x, y with 0, 0 -> red | 1, 0 -> Rays_math.Color.rgb 0 255 0 | 0, 1 -> blue | _ -> Rays_math.Color.white)
    |> Texture.generate_mipmaps in
  check (Array.length (Texture.Private.levels texture) = 2) "Texture mipmaps";
  check (Texture.size texture = (2, 2) && List.hd (Texture.pixels texture) = red)
    "Texture upper-left texel"

(* Text, a 3D view and overlay geometry stage as ordered layers; the order,
   the retained resources and the released ones are exact. *)
let scene_parity () =
  Scene.Layer.with_cache_context (Scene.Layer.create_cache_context ()) (fun () ->
  let camera = Camera.perspective ~at:(Rays_math.Vec3.create 0. 0. 3.) ~target:Rays_math.Vec3.zero () in
  let scene3 = Scene.D3.create [] in
  let scene =
    [ Scene.clear Rays_math.Color.black;
      Scene.clip ~at:(0, 0) ~w:32 ~h:24
        [ Scene.text ~at:(2, 3) ~size:12 "staged text";
          Scene.view3d ~viewport:(8, 4, 16, 12) ~camera scene3 ];
      Scene.rect ~at:(1, 1) ~w:3 ~h:2 ~fill:Rays_math.Color.red () ] in
  let commands ir = Scene_command.Render_ir.commands_readonly ir in
  let first = Result.get_ok (Scene.Layer.stage_native ~width:32 ~height:24 scene) in
  let first_ir, first_resources = first.scene2, first.resources in
  let automatic_text_id = ref None in
  check (List.length first.scene3 = 1) "View3d native staging";
  check (first.clear = (0., 0., 0., 1.)) "native clear staging";
  (match first.layers with
   | [ Scene.Layer.Scene2_layer _; Scene.Layer.Scene3_layer _;
       Scene.Layer.Scene2_layer _ ] -> ()
   | _ -> failwith "Scene2/View3d/overlay layer ordering");
  (match Array.to_list (commands first_ir) with
   | [ Scene_command.Render_ir.Clear _; Push_clip _; Image text; Pop_clip; Geometry _ ] ->
       automatic_text_id := Some text.resource_id
   | commands ->
       let tag = function
         | Scene_command.Render_ir.Clear _ -> "clear" | Set_blend _ -> "blend"
         | Push_clip _ -> "push-clip" | Pop_clip -> "pop-clip"
         | Push_transform _ -> "push-transform" | Pop_transform -> "pop-transform"
         | Geometry _ -> "geometry" | Shapes _ -> "shapes" | Image _ -> "image"
         | Glyphs _ -> "glyphs" in
       failwith ("text/View3d ordering or state scope: "
                 ^ String.concat "," (List.map tag commands)));
  check (List.length first_resources = 1) "text resource";
  let scene2_only = Scene.[ clear Rays_math.Color.black;
    rect ~at:(1, 2) ~w:8 ~h:6 ~fill:Rays_math.Color.red ();
    rect ~at:(2, 3) ~w:1 ~h:1 ~fill:Rays_math.Color.white () ] in
  let staged_scene2_only = Result.get_ok
      (Scene.Layer.stage_native ~width:16 ~height:12 scene2_only) in
  (match staged_scene2_only.layers with
   | [ Scene.Layer.Scene2_layer (ir, resources) ] ->
       check (ir == staged_scene2_only.scene2)
         "Scene2-only native staging duplicated its render IR";
       check (resources == staged_scene2_only.resources)
         "Scene2-only native staging duplicated its resource table"
   | _ -> failwith "Scene2-only native staging split one ordered layer");
  let split_scene2 = Scene.[
    rect ~at:(1, 2) ~w:8 ~h:6 ~fill:Rays_math.Color.red ();
    Layer.layer_break;
    rect ~at:(2, 3) ~w:1 ~h:1 ~fill:Rays_math.Color.white () ] in
  let staged_split = Result.get_ok
      (Scene.Layer.stage_native ~width:16 ~height:12 split_scene2) in
  (match staged_split.layers with
   | [ Scene.Layer.Scene2_layer (first, _); Scene.Layer.Scene2_layer (second, _) ] ->
       (match Array.to_list (commands first), Array.to_list (commands second) with
        | [ Geometry _ ], [ Geometry _ ] -> ()
        | _ -> failwith "Scene2 layer break changed command ordering")
   | _ -> failwith "Scene2 layer break did not isolate dynamic commands");
  check (Array.length (commands staged_split.scene2) = 2)
    "Scene2 layer break leaked into compatibility IR";
  let styled = Scene.[ polygon [ 0, 0; 8, 0; 8, 8 ] ~fill:Rays_math.Color.red ~stroke:Rays_math.Color.white () ] in
  let styled_ir, _ = Result.get_ok (Scene.Layer.stage ~width:16 ~height:16 styled) in
  (match Array.to_list (commands styled_ir) with
   | [ Geometry _; Geometry _ ] -> ()
   | _ -> failwith "polygon fill/stroke did not stage two meshes");
  List.iter (fun frame ->
    let staged = Result.get_ok (Scene.Layer.stage_native ~width:32 ~height:24 scene) in
    check (Scene_command.Render_ir.serialize staged.scene2
           = Scene_command.Render_ir.serialize first_ir)
      (Printf.sprintf "frame %d deterministic IR" frame);
    check (List.map fst staged.resources = List.map fst first_resources)
      (Printf.sprintf "frame %d must not re-upload" frame);
    check (List.length staged.scene3 = 1)
      (Printf.sprintf "frame %d native View3d drift" frame))
    [ 2; 60; 600 ];
  Scene.Layer.release scene;
  List.iter (function
    | identity, Renderer.Image image when Some identity = !automatic_text_id ->
        check (Result.is_ok (Runtime_resources.Image.pixels image))
          "released automatic cache image"
    | _, Renderer.Image image ->
        check (Result.is_error (Runtime_resources.Image.pixels image))
          "released owned staged image"
    | _, Renderer.Text _ | _, Renderer.Canvas _ ->
        failwith "unexpected staged resource")
    first_resources;
  let _, _, references = Font.Raster.automatic_counts () in
  check (references = 0) "released scene kept an automatic text reference")

let resources () =
  Scene.Layer.with_cache_context (Scene.Layer.create_cache_context ()) (fun () ->
  let image = Image.create ~width:2 ~height:2 ~color:(Rays_math.Color.rgba 1 2 3 4) () |> Result.get_ok in
  check (Result.get_ok(Image.get_size image) = (2, 2)
         && Result.get_ok (Image.pixels image)
            = Bytes.of_string "\001\002\003\004\001\002\003\004\001\002\003\004\001\002\003\004")
    "Image snapshot";
  let rgba = Bytes.make 16 '\042' in
  let uploaded = Image.upload_rgba ~width:2 ~height:2 ~rgba () |> Result.get_ok in
  let id = Image.identity uploaded in
  check (Result.is_error (Image.upload_rgba ~into:uploaded ~width:2 ~height:2
                            ~rgba:(Bytes.make 3 '\000') ())
         && Image.identity uploaded = id
         && Result.get_ok (Image.pixels uploaded) = rgba)
    "invalid RGBA upload changed an image";
  check (Image.upload_rgba ~into:uploaded ~width:2 ~height:2 ~rgba:(Bytes.make 16 '\007') ()
         = Ok uploaded
         && Image.identity uploaded = id
         && Result.get_ok (Image.pixels uploaded) = Bytes.make 16 '\007')
    "RGBA replacement did not preserve image identity";
  Result.get_ok(Image.destroy uploaded);
  let replaced = Image.create ~width:2 ~height:3 () |> Result.get_ok in
  check (Result.get_ok(Image.get_size replaced) = (2, 3)) "image dimensions";
  let identity = Image.identity replaced in
  Result.get_ok(Image.replace replaced (Image.create ~width:1 ~height:1 ~color:Rays_math.Color.red () |> Result.get_ok));
  check (Result.get_ok(Image.get_size replaced) = (1, 1) && Image.identity replaced = identity
         && Result.get_ok (Image.pixels replaced) = Bytes.of_string "\255\000\000\255")
    "stable image replacement";
  Result.get_ok(Image.destroy replaced);
  let canvas = Canvas.create_exn ~width:3 ~height:2 in
  let captured = Result.get_ok (Canvas.to_image canvas) in
  check (Result.get_ok(Image.get_size captured) = (3, 2)) "Canvas capture";
  let audio = match Audio.init () with Error message -> failwith message | Ok audio -> audio in
  let sample = Result.get_ok
      (Audio.Sample.synth audio ~waveform:Sine ~frequency:440. ~duration:0.2 ()) in
  check (Result.is_ok (Audio.Sample.play audio sample)) "sample playing";
  Audio.Sample.destroy sample;
  for _ = 1 to 100_000 do check (Image.identity image > 0) "stable image identity" done;
  Result.get_ok(Image.destroy captured);Result.get_ok(Canvas.destroy canvas);Result.get_ok(Image.destroy image);
  Result.get_ok (Audio.shutdown audio))

let run () =
  sketch_startup_result (); math (); random (); camera (); frame_and_scene (); scene_parity (); resources ();
  print_endline "Rays API: math, noise, camera, frame, staging order and resource ownership passed"
