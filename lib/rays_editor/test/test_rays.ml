open Test_support

let run_core () =
  let initialized = Parallel.init_array ~grain:1 10_000
      (fun index -> index * index) in
  if initialized.(0) <> 0 || initialized.(9_999) <> 99_980_001 then
    fail "parallel array initialization returned incorrect values";
  let mapped = Parallel.map_array ~grain:1
      (fun value -> value + 1) initialized in
  if mapped.(5_000) <> 25_000_001 then
    fail "parallel array map returned incorrect values";
  let nested = Parallel.map_array ~grain:1 (fun value ->
      Parallel.map_array ~grain:1 (( + ) value) [|1;2;3|])
      [|10;20|] in
  if nested <> [|[|11;12;13|]; [|21;22;23|]|] then
    fail "nested parallel array map returned incorrect values";
  let left, right =
    Parallel.run ~domains:2 (fun () ->
      Parallel.both (fun () -> 20 + 1) (fun () -> 6 * 7))
  in
  if left <> 21 || right <> 42 then
    fail "parallel computations returned incorrect values";
  let calling_domain = Domain.self () in
  let sequential_domains =
    Parallel.run ~domains:1 (fun () ->
      Parallel.init_array ~grain:1 10_000 (fun _ -> Domain.self ()))
  in
  if not (Array.for_all (( = ) calling_domain) sequential_domains) then
    fail "Parallel.run ~domains:1 allowed nested work onto another domain";
  let cutoff_domains, cutoff_for_domain =
    Parallel.run ~domains:2 (fun () ->
      let values = Parallel.init_array ~grain:64 8
          (fun _ -> Domain.self ()) in
      let observed = ref (Domain.self ()) in
      Parallel.for_ ~chunk_size:64 ~start:0 ~finish:7
        (fun _ -> observed := Domain.self ());
      values, !observed)
  in
  if not (Array.for_all (( = ) calling_domain) cutoff_domains)
     || cutoff_for_domain <> calling_domain
  then fail "nested parallel work ignored its sequential grain cutoff";
  (match Rays_math.Color.hex "#369c" with
   | Ok { r = 0x33; g = 0x66; b = 0x99; a = 0xcc } -> ()
   | _ -> fail "short RGBA hex parsing is incorrect");
  let indexed = Rays_math.Rand.seed64 0x7eed_1234_5678_9abcL in
  let indexed_values = Array.init 10_000 (fun index ->
      Rays_math.Rand.float_at indexed ~index) in
  if indexed_values <> Array.init 10_000 (fun index ->
      Rays_math.Rand.float_at indexed ~index)
      || Array.exists (fun value -> value < 0. || value >= 1.) indexed_values
      || indexed_values.(17) = indexed_values.(18) then
    fail "indexed random sampling is not stable or escaped [0, 1)";
  let noise_a = Rays_math.Noise.create 17 in
  let noise_b = Rays_math.Noise.create 17 in
  let sample_a = Rays_math.Noise.fbm2 noise_a ~x:1.25 ~y:(-3.5) in
  let sample_b = Rays_math.Noise.fbm2 noise_b ~x:1.25 ~y:(-3.5) in
  if sample_a <> sample_b then fail "equal noise seeds were not reproducible";
  if sample_a < 0. || sample_a > 1. then fail "noise sample escaped 0..1";
  let open Rays in
  let transform =
    Rays_math.Mat4.mul
      (Rays_math.Mat4.translation (Rays_math.Vec3.create 3. (-2.) 5.))
      (Rays_math.Mat4.mul
         (Rays_math.Mat4.rotation_y 0.7)
         (Rays_math.Mat4.scaling (Rays_math.Vec3.create 2. 3. 4.)))
  in
  let inverse =
    match Rays_math.Mat4.inverse transform with
    | Some inverse -> inverse
    | None -> fail "invertible 3D transform was reported singular"
  in
  let point = Rays_math.Vec3.create 1.25 (-0.5) 3. in
  let round_trip =
    Rays_math.Mat4.transform_point inverse (Rays_math.Mat4.transform_point transform point)
  in
  if not (Rays_math.Vec3.nearly_equal point round_trip ~eps:1e-9) then
    fail "Mat4 inverse did not restore a transformed point";
  let camera =
    Camera.perspective ~at:(Rays_math.Vec3.create 0. 0. 5.) ~target:Rays_math.Vec3.zero ()
  in
  let viewport = 0, 0, 640, 360 in
  let screen =
    match Camera.world_to_screen ~viewport camera Rays_math.Vec3.zero with
    | Some point -> point
    | None -> fail "camera rejected a visible world point"
  in
  if abs_float (screen.x -. 320.) > 1e-6
     || abs_float (screen.y -. 180.) > 1e-6
  then fail "perspective camera did not project the origin to viewport center";
  let easy = Easy_camera.create ~distance:5. () in
  if Easy_camera.camera easy != Easy_camera.camera easy then
    fail "easy camera did not retain an unchanged derived camera";
  if Easy_camera.camera (Easy_camera.with_distance 6. easy)
      == Easy_camera.camera easy then
    fail "easy camera reused a derived camera after a parameter change";
  let viewed = Easy_camera.of_view ~eye:(Rays_math.Vec3.create 3. 4. (-5.))
      ~target:(Rays_math.Vec3.create 1. 1. 1.) (Easy_camera.create ()) in
  if not (Rays_math.Vec3.nearly_equal (Camera.position (Easy_camera.camera viewed))
      (Rays_math.Vec3.create 3. 4. (-5.)) ~eps:1e-9)
     || Easy_camera.target viewed <> Rays_math.Vec3.create 1. 1. 1.
  then fail "easy camera of_view did not reproduce the requested eye";
  let framed = Easy_camera.frame_bounds ~min:(Rays_math.Vec3.create (-1.) (-1.) (-1.))
      ~max:(Rays_math.Vec3.create 3. 1. 1.) (Easy_camera.create ~fov_y:(Float.pi /. 2.) ()) in
  if not (Rays_math.Vec3.nearly_equal (Easy_camera.target framed) (Rays_math.Vec3.create 1. 0. 0.) ~eps:1e-12)
     || Float.abs (Easy_camera.distance framed -. (sqrt 6. *. 1.2)) > 1e-9
  then fail "easy camera frame_bounds did not target the center at radius/tan(fov/2)*1.2";
  let sampled_area =
    Light.area ~samples:16 ~at:(Rays_math.Vec3.create 0. 3. 0.)
      ~direction:(Rays_math.Vec3.create 0. (-1.) 0.)
      ~width:4. ~height:2. ()
  in
  (match sampled_area.kind with
   | Light.Area { samples = 16; _ } -> ()
   | _ -> fail "area light did not retain its deterministic sample count")

(* The runtime's keys must reach the Input keys hosts match on. *)
let run_keys () =
  List.iter (fun (key, expected) ->
    if Runtime_input_sdl3.key key <> expected then
      fail "a runtime key did not map to its Input key")
    Input.[ Sdl3.Key.Arrow_right, ArrowRight; Arrow_left, ArrowLeft;
            Arrow_down, ArrowDown; Arrow_up, ArrowUp; Enter, Enter;
            Escape, Escape; Control, Ctrl; Meta, Meta; Page_up, PageUp;
            Char 'a', KeyChar 'a'; Space, Space; F12, F12;
            Unknown 4242, Unknown 4242 ]

(* Every 2D constructor must lower to a valid Render_ir (triangle lists). *)
let run_scene2 () =
  let open Rays in
  let open_path = Path.of_commands [|
    Path.Move_to { x = 0.; y = 0. };
    Path.Line_to { x = 10.5; y = 5.25 };
    Path.Line_to { x = 20.; y = 0. };
  |] in
  List.iter (fun (name, node) ->
    match Scene.Private.to_ir [node] with
    | Ok _ -> ()
    | Error message -> fail (name ^ " did not lower: " ^ message))
    [ "point", Scene.point ~at:(3, 4) ();
      "open path", Scene.path open_path;
      "closed path", Scene.path ~fill:Rays_math.Color.white (Path.of_commands [|
        Path.Move_to { x = 0.; y = 0. };
        Path.Line_to { x = 10.5; y = 5.25 };
        Path.Line_to { x = 20.; y = 0. };
        Path.Close;
      |]) ]

(* Font metrics, measuring and alignment come from SDL_ttf, not placeholders. *)
let run_font () =
  let open Rays in
  let msg = function Ok v -> v | Error (`Msg m) -> fail m in
  let font = msg (Font.system ~size:20 ()) in
  if Font.get_ascent font <= 0 then fail "font vertical metrics are placeholders";
  let text = "short\na much longer second line" in
  let pixels align =
    let image = msg (Font.cached_text ~wrap:400 ~align font text (Font.Blended Rays_math.Color.white)) in
    match Image.pixels image with Ok p -> p | Error m -> fail m in
  if Bytes.equal (pixels Font.Left) (pixels Font.Center) then
    fail "Font alignment is ignored";
  Font.destroy font

let run () =
  run_core ();
  run_keys ();
  run_scene2 ();
  run_font ()
