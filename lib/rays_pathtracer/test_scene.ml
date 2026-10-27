(* Several objects per trace (scene_mesh), Light.t conversion, and a flat mesh
   mixing triangles with analytic spheres (one BLAS per geometry kind). *)
module P = Rays_pathtracer
module V = Rays_math.Vec3
module M = Rays_math.Mat4
let rgb = P.Linear_color.rgb
let get = function Ok v -> v | Error e -> failwith e
let rdk r = get (Result.map_error Rdk.Error.to_string r)
let rays_mesh geometry = get (Result.map_error Rdk.Error.to_string
  (Rdk_rays.Rays_mesh.to_trace_mesh geometry))
let prepared geometry material = get (P.mesh [rays_mesh geometry, material])
let live_handles = let _, live = Ogpu.Impl.create_driver () in live
let black = { P.sky = rgb 0. 0. 0.; ground = rgb 0. 0. 0.; panels = [] }

(* The kernel's resolve (ACES fit, gamma 2.2, exposure 1) and its inverse. *)
let resolve x =
  let c = Rays_math.Color.aces x in
  Float.min 1. (Float.max 0. c) ** (1. /. 2.2) *. 255.
let unresolve value =
  let rec search lo hi n = if n = 0 then (lo +. hi) /. 2. else
    let mid = (lo +. hi) /. 2. in if resolve mid < value then search mid hi (n - 1) else search lo mid (n - 1) in
  search 0. 16. 60

let run () =
  let initial_handles = live_handles () in
  let floor = rdk (Rdk.Box_generator.box ~center:(V.create 0. (-0.05) 0.) ~size:(V.create 20. 0.1 20.) ()) in
  let box = rdk (Rdk.Box_generator.box ~center:(V.create 0. 0.5 0.) ~size:(V.create 1. 1. 1.) ()) in
  let ball = rdk (Rdk.Uv_sphere.run ~center:V.zero ~segments:24 ~rings:12 ~radius:0.4 ()) in
  let scene = { P.objects = [ (rays_mesh floor, P.material (rgb 0.5 0.5 0.5)) ]; spheres = []; strands = []
              ; environment = { sky = rgb 0.5 0.6 0.8; ground = rgb 0.2 0.2 0.2; panels = [] }
              ; lights = [ P.rect_light ~intensity:20. ~size:(2., 2.) ~target:V.zero (V.create (-2.) 5. 2.) ] } in
  match P.create ~spp:2 ~width:64 ~height:48 scene with
  | Error message when Sys.getenv_opt "RAYS_REQUIRE_RAYTRACING" = None ->
      Printf.printf "pathtracer scene: skipped (%s)\n" message
  | Error message -> failwith message
  | Ok tracer ->
      let camera = Rays.Camera.perspective ~fov_y:0.9 ~at:(V.create 0. 2.5 6.) ~target:(V.create 0. 0.6 0.) () in
      (* 1. Two objects at world transforms, one flat and one instanced with
         per-instance materials, against the same meshes composed by hand. *)
      let red = P.material (rgb 0.8 0.2 0.2) and green = P.material (rgb 0.2 0.8 0.2)
      and blue = P.material ~roughness:0.2 (rgb 0.2 0.2 0.8) and gray = P.material (rgb 0.5 0.5 0.5) in
      let t1 = M.mul (M.translation (V.create (-1.2) 0. 0.)) (M.rotation_y 0.4)
      and t2 = M.mul (M.translation (V.create 1. 0. 0.)) (M.rotation_y (-0.3)) in
      let a = M.translation (V.create 0. 0.4 0.)
      and b = M.mul (M.translation (V.create 0.3 1.3 0.)) (M.scaling (V.create 0.8 0.8 0.8)) in
      let scene_of t1 = get (P.scene_mesh
        [ (t1, get (P.mesh [ (rays_mesh box, red) ]))
        ; (t2, get (P.mesh_instanced ~prototype:(prepared ball gray) ~materials:[| green; blue |] [| a; b |]))
        ; (M.identity, get (P.mesh [ (rays_mesh floor, gray) ])) ]) in
      let composed = get (P.mesh
        [ (rays_mesh (Rdk.Transform_ops.transform t1 box), red)
        ; (rays_mesh (Rdk.Transform_ops.transform (M.mul t2 a) ball), green)
        ; (rays_mesh (Rdk.Transform_ops.transform (M.mul t2 b) ball), blue)
        ; (rays_mesh floor, gray) ]) in
      let placed = scene_of t1 in
      let render ?(frames = 2) () =
        P.reset tracer;
        for _ = 1 to frames do get (P.render tracer camera); get (P.flush tracer) done;
        Test_pathtracer.pixels tracer in
      get (P.replace_mesh tracer placed);
      let scene_pixels = render ~frames:4 () in
      assert (scene_pixels = render ~frames:4 ());
      let capture mesh =
        get (P.replace_mesh tracer mesh); P.reset tracer;
        for _ = 1 to 2 do get (P.render tracer camera); get (P.flush tracer) done;
        Test_pathtracer.pixels tracer in
      let placed_pixels = capture placed and composed_pixels = capture composed in
      assert (Bytes.length placed_pixels = 64 * 48 * 4 &&
        Bytes.length composed_pixels = Bytes.length placed_pixels);
      let total = ref 0 and outliers = ref 0 in
      Bytes.iteri (fun i value -> if i mod 4 <> 3 then begin
        let difference = abs (Char.code value - Char.code (Bytes.get composed_pixels i)) in
        total := !total + difference;
        if difference > 8 then incr outliers
      end) placed_pixels;
      let mean = float !total /. float (64 * 48 * 3) in
      Printf.printf "pathtracer scene: vs composed mean |diff| %.3f, %d outliers\n" mean !outliers;
      assert (mean < 1.0 && !outliers < 64 * 48 * 3 / 100);
      (* A translated object moves in the image: the box's red excess
         centroid shifts right. *)
      let shifted_pixels = capture (scene_of (M.mul (M.translation (V.create 0.8 0. 0.)) t1)) in
      let red_center bytes =
        let sum = ref 0 and weighted = ref 0 in
        for pixel = 0 to 64 * 48 - 1 do
          let channel i = Char.code (Bytes.get bytes (pixel * 4 + i)) in
          let weight = max 0 (channel 0 - max (channel 1) (channel 2)) in
          sum := !sum + weight; weighted := !weighted + weight * (pixel mod 64)
        done;
        assert (!sum > 0); float !weighted /. float !sum in
      let before = red_center placed_pixels and after = red_center shifted_pixels in
      Printf.printf "pathtracer scene: red centroid %.1f -> %.1f\n" before after;
      assert (after > before +. 3.);
      (match P.scene_mesh [] with Error _ -> () | Ok _ -> failwith "empty scene accepted");
      (match P.scene_mesh [ (M.identity, get (P.mesh ~spheres:[{ P.center = V.zero; radius = 1.;
          sphere_material = red }] [])) ] with
       | Error _ -> () | Ok _ -> failwith "scene object with spheres accepted");
      (* Nested: a scene is itself an object. *)
      get (P.replace_mesh tracer (get (P.scene_mesh [ (M.identity, placed) ])));
      assert (scene_pixels = render ~frames:4 ());
      (* move: re-placing objects over the built BLASes matches a fresh
         scene_mesh at the new transforms, directly, superseding a pending
         move, and queued behind a full build. *)
      let t1' = M.mul (M.translation (V.create 0.8 0. 0.)) t1 in
      let same name pixels =
        let total = ref 0 in
        Bytes.iteri (fun i c -> if i mod 4 <> 3 then
          total := !total + abs (Char.code c - Char.code (Bytes.get shifted_pixels i))) pixels;
        let mean = float !total /. float (64 * 48 * 3) in
        Printf.printf "pathtracer scene: move (%s) vs scene_mesh mean |diff| %.3f\n" name mean;
        assert (mean < 0.5) in
      get (P.replace_mesh tracer placed);
      get (P.move tracer [ t1'; t2; M.identity ]); get (P.flush tracer);
      same "direct" (render ());
      get (P.move tracer [ t1; t2; M.identity ]); get (P.move tracer [ t1'; t2; M.identity ]); get (P.flush tracer);
      same "superseded" (render ());
      get (P.queue_mesh tracer placed); get (P.move tracer [ t1'; t2; M.identity ]); get (P.flush tracer);
      same "queued" (render ());
      (match P.move tracer [ t1 ] with Error _ -> () | Ok () -> failwith "move with a wrong count accepted");
      (* An edit renders like a camera move: once a queued swap installs, the
         next frame is an uncounted preview, and the one after accumulates. *)
      get (P.render tracer camera); get (P.flush tracer);
      get (P.queue_mesh tracer placed); get (P.flush tracer);
      get (P.render tracer camera); get (P.flush tracer);
      assert (P.samples tracer = 0);
      get (P.render tracer camera); get (P.flush tracer);
      assert (P.samples tracer = 2);
      (* Timing: ~2000 instances of a 12-triangle box plus a 20k-triangle
         mesh, a full scene_mesh + queue_mesh + flush against move + flush. *)
      let boxes = get (P.mesh_instanced ~prototype:(prepared box red)
        (Array.init 2000 (fun i -> M.translation (V.create (float (i mod 50) *. 0.3 -. 7.5) 0. (float (i / 50) *. -0.3))))) in
      let dense = get (P.mesh [ (rays_mesh (rdk (Rdk.Uv_sphere.run ~center:V.zero ~segments:100 ~rings:101 ~radius:0.5 ())), blue) ]) in
      let at x = M.translation (V.create x 1. 0.) in
      let time f = let start = Unix.gettimeofday () in f (); (Unix.gettimeofday () -. start) *. 1000. in
      let runs = 8 in
      get (P.queue_mesh tracer (get (P.scene_mesh [ (M.identity, boxes); (at 0., dense) ]))); get (P.flush tracer);
      let full = List.init runs (fun i -> time (fun () ->
        get (P.queue_mesh tracer (get (P.scene_mesh [ (M.identity, boxes); (at (float i *. 0.1), dense) ])));
        get (P.flush tracer))) in
      let moves = List.init runs (fun i -> time (fun () ->
        get (P.move tracer [ M.identity; at (float i *. -0.1) ]); get (P.flush tracer))) in
      let median l = List.nth (List.sort compare l) (List.length l / 2) in
      Printf.printf "pathtracer scene: 2001-instance scene: scene_mesh+queue_mesh+flush %.2f ms, move+flush %.2f ms (median of %d)\n%!"
        (median full) (median moves) runs;
      (* what a move saves depends on the machine and the GPU's state (both runs end in one flush), so
         the default run prints the numbers and a qualification run (RAYS_QUALIFY=1) checks them *)
      if Sys.getenv_opt "RAYS_QUALIFY" <> None then assert (median moves < median full);
      (* 2. One mesh with triangles and analytic spheres builds (one BLAS per
         kind under a TLAS) and shows both: an emissive red sphere over the
         lit floor. *)
      let spheres = [{ P.center = V.create 0. 1. 0.; radius = 0.5;
        sphere_material = P.material ~emission:(rgb 4. 0. 0.) (rgb 0. 0. 0.) }] in
      let mixed = get (P.mesh ~spheres [rays_mesh floor, gray]) in
      let mixed_pixels = capture mixed in
      (* The red pixels are the sphere's footprint: none outside its projected
         disc and most of the disc. Green and blue stay below 100, not near
         zero, because the black dielectric also reflects the sky (best pixel
         measured 252, 57, 66). *)
      let screen at = Option.get (Rays.Camera.world_to_screen ~viewport:(0,0,64,48) camera at) in
      let center = screen (V.create 0. 1. 0.) in
      let radius = Float.abs ((screen (V.create 0.5 1. 0.)).x -. center.x) in
      let red_pixels = ref 0 in
      for pixel = 0 to 64 * 48 - 1 do
        let channel i = Char.code (Bytes.get mixed_pixels (pixel * 4 + i)) in
        if channel 0 > 200 && channel 1 < 100 && channel 2 < 100 then begin
          incr red_pixels;
          assert (Float.hypot (float (pixel mod 64) +. 0.5 -. center.x)
            (float (pixel / 64) +. 0.5 -. center.y) <= radius +. 1.)
        end
      done;
      Printf.printf "pathtracer scene: %d red pixels in a disc of radius %.2f px\n" !red_pixels radius;
      assert (float !red_pixels >= 0.7 *. Float.pi *. radius *. radius);
      let without_floor = capture (get (P.mesh ~spheres [])) in
      let point = Option.get (Rays.Camera.world_to_screen ~viewport:(0,0,64,48)
        camera (V.create 3. 0. 0.)) in
      let pixel = int_of_float point.y * 64 + int_of_float point.x in
      let difference = ref 0 in
      for channel = 0 to 2 do
        let at = pixel * 4 + channel in
        difference := !difference + abs (Char.code (Bytes.get mixed_pixels at) -
          Char.code (Bytes.get without_floor at))
      done;
      assert (!difference > 10);
      get (P.flush tracer);
      (match P.move tracer [ M.identity ] with Error _ -> () | Ok () -> failwith "move of a non-scene accepted");
      print_endline "pathtracer scene: mixed triangles and spheres ok";
      P.destroy tracer;
      (* 3. light_of: a white matte plane under each converted light converges
         to the raster irradiance pi I / d^2, i.e. outgoing radiance
         albedo * I / d^2 (x0.97 for the BSDF's Fresnel at normal incidence). *)
      let plane = { P.objects = [ (rays_mesh floor, P.material ~roughness:1. (rgb 0.8 0.8 0.8)) ]; spheres = []; strands = []
                  ; environment = black; lights = [] } in
      let tracer = get (P.create ~spp:4 ~width:32 ~height:32 plane) in
      let camera = Rays.Camera.perspective ~fov_y:0.1 ~at:(V.create 0. 4. 0.001)
        ~target:V.zero () in
      let check name light expected =
        get (P.set_lights tracer [ P.light_of light ]);
        P.reset tracer;
        for _ = 1 to 128 do get (P.render tracer camera); get (P.flush tracer) done;
        let pixels = Test_pathtracer.pixels tracer in
        let sum = ref 0. in
        for y = 12 to 19 do for x = 12 to 19 do
          sum := !sum +. float (Char.code (Bytes.get pixels ((y * 32 + x) * 4)))
        done done;
        let measured = unresolve (!sum /. 64.) in
        Printf.printf "pathtracer light_of %s: %.4f expected %.4f\n" name measured expected;
        assert (Float.abs (measured -. expected) < 0.06 *. expected) in
      let at = V.create 0. 2. 0. and down = V.create 0. (-1.) 0. in
      let physical = Rays.Light.attenuation ~quadratic:1. () in
      let matte intensity d2 = 0.8 *. 0.97 *. intensity /. d2 in
      check "area" (Rays.Light.area ~intensity:2. ~attenuation:physical ~at ~direction:down ~width:0.2 ~height:0.2 ()) (matte 2. 4.);
      check "point" (Rays.Light.point ~intensity:2. ~attenuation:physical ~at ()) (matte 2. 4.);
      check "spot" (Rays.Light.spot ~intensity:2. ~attenuation:physical ~at ~direction:down ~cutoff:0.5 ~concentration:1. ()) (matte 2. 4.);
      check "directional" (Rays.Light.directional ~intensity:0.5 ~direction:down ()) (matte 0.5 1.);
      P.destroy tracer;
      let final_handles = live_handles () in
      Printf.printf "pathtracer scene: handle baseline %d final %d\n%!" initial_handles final_handles;
      assert (final_handles = initial_handles)

(* Manual: frame cost at 1200x1560 on a voxel_wall-like scene (2000 rounded
   boxes, a 20k-triangle mesh, 4 bounces), a full first sample against the
   preview frame an edit now renders. Not in runtest. *)
let bench () =
  let concrete = P.material ~roughness:0.6 ~round:0.07 (rgb 0.42 0.42 0.44) in
  let box = rdk (Rdk.Box_generator.box ~size:(V.create 0.28 0.28 0.28) ()) in
  let boxes = get (P.mesh_instanced ~prototype:(prepared box concrete)
    (Array.init 2000 (fun i -> M.translation (V.create (float (i mod 50) *. 0.3 -. 7.5) (float (i / 50) *. 0.3) 0.)))) in
  let dense = get (P.mesh [ (rays_mesh (rdk (Rdk.Uv_sphere.run ~center:V.zero ~segments:100 ~rings:101 ~radius:1. ())), concrete) ]) in
  let at x = M.translation (V.create x 6. 2.) in
  let scene = { P.objects = [ (rays_mesh box, concrete) ]; spheres = []; strands = []
              ; environment = { sky = rgb 0.006 0.007 0.009; ground = rgb 0.002 0.002 0.003; panels = [] }
              ; lights = [ P.rect_light ~intensity:9. ~size:(24., 24.) ~target:V.zero (V.create (-34.) 40. 30.) ] } in
  let tracer = get (P.create ~bounces:4 ~width:1200 ~height:1560 scene) in
  get (P.replace_mesh tracer (get (P.scene_mesh [ (M.identity, boxes); (at 0., dense) ])));
  let camera = Rays.Camera.perspective ~fov_y:0.7 ~at:(V.create 0. 6. 19.) ~target:(V.create 0. 6. 0.) () in
  let time f = let start = Unix.gettimeofday () in f (); (Unix.gettimeofday () -. start) *. 1000. in
  let frame () = time (fun () -> get (P.render tracer camera); get (P.flush tracer)) in
  ignore (frame ()); ignore (frame ());
  let full = List.init 5 (fun _ -> P.reset tracer; frame ()) in
  let preview = List.init 5 (fun i -> get (P.move tracer [ M.identity; at (float i *. 0.1) ]); get (P.flush tracer); frame ()) in
  let median l = List.nth (List.sort compare l) (List.length l / 2) in
  Printf.printf "pathtracer bench_scene 1200x1560: full first sample %.1f ms, edit preview %.1f ms (median of 5)\n%!"
    (median full) (median preview);
  P.destroy tracer
