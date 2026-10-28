(* The World path: CDF + MIS sampling is unbiased (furnace, and MIS matches
   BSDF-only sampling on a peaked map with a sun), MIS has lower variance,
   miss pixels show the background, and rect lights are hittable. *)
module P = Rays_pathtracer
module W = Rays.World

let rgb = P.Linear_color.rgb
let get = function Ok v -> v | Error e -> failwith e

let rays_mesh geometry =
  get (Result.map_error Rdk.Error.to_string (Rdk_rays.Rays_mesh.to_trace_mesh geometry))

let v3 = Rays_math.Vec3.create
let white = W.rgb 1. 1. 1.
let gradient c = W.Gradient { zenith = c; horizon = c; nadir = c; sharpness = 1. }

let world ?(background = W.Environment) ?(sun = `Manual (0., 1.)) layers =
  W.bake ~domains:1 ~width:64 ~height:32
    {
      W.default with
      layers = List.map (fun layer -> { W.name = "layer"; layer; visible = true }) layers;
      background;
      exposure = 0.;
      sun;
    }

let black_dome = { P.sky = rgb 0. 0. 0.; ground = rgb 0. 0. 0.; panels = [] }

(* A lone analytic sphere: convex, so every path is direct light and the
   indirect firefly clamp never applies. *)

(* Keep all corner rays inside the sphere's projected disc for the furnace. *)
let camera ?(fov = 0.2) () =
  Rays.Camera.perspective ~fov_y:fov ~at:(v3 0. 0. 6.) ~target:(v3 0. 0. 0.) ()

let mean_red bytes =
  let sum = ref 0 in
  Bytes.iteri (fun i c -> if i mod 4 = 0 then sum := !sum + Char.code c) bytes;
  float !sum /. float (Bytes.length bytes / 4)

let rmse a b =
  let sum = ref 0. in
  Bytes.iteri
    (fun i c ->
      if i mod 4 = 0 then
        let d = float (Char.code c - Char.code (Bytes.get b i)) in
        sum := !sum +. (d *. d))
    a;
  sqrt (!sum /. float (Bytes.length a / 4))

(* The resolve's ACES fit and 2.2 gamma, for an exact background check. *)
let resolved x =
  let c = Rays_math.Color.aces x in
  int_of_float ((Float.pow (Float.min 1. (Float.max 0. c)) (1. /. 2.2) *. 255.) +. 0.5)

let run () =
  let live = Ogpu.Impl.live_handles in
  let baseline = live () in
  let scene material lights =
    {
      P.objects = [];
      strands = [];
      spheres = [ { P.center = v3 0. 0. 0.; radius = 1.; sphere_material = material } ];
      environment = black_dome;
      lights;
    }
  in
  let matte = scene (P.material ~roughness:1. (rgb 0.8 0.8 0.8)) [] in
  let create bsdf_only scene = P.create ~spp:1 ~bsdf_only ~width:32 ~height:32 scene in
  match create false matte with
  | Error message when Sys.getenv_opt "RAYS_REQUIRE_RAYTRACING" = None ->
      Printf.printf "pathtracer World: skipped (%s)\n" message
  | Error message -> failwith message
  | Ok tracer ->
      Fun.protect
        ~finally:(fun () -> P.destroy tracer)
        (fun () ->
          let bsdf = get (create true matte) in
          Fun.protect
            ~finally:(fun () -> P.destroy bsdf)
            (fun () ->
              let capture ?(camera = camera ()) tracer baked frames =
                get (P.set_world tracer (Some baked));
                for _ = 1 to 2 do
                  get (P.render tracer camera);
                  get (P.flush tracer)
                done;
                P.reset tracer;
                for _ = 1 to frames do
                  get (P.render tracer camera);
                  get (P.flush tracer)
                done;
                assert (P.samples tracer = frames);
                let pixels = Test_pathtracer.pixels tracer in
                assert (Bytes.length pixels = 32 * 32 * 4);
                pixels
              in
              let furnace = world [ gradient white ] in
              let uniform = capture tracer furnace 1024 in
              let furnace_mean = mean_red uniform in
              assert (Float.abs (furnace_mean -. 224.) < 2.);
              Bytes.iteri (fun i value -> if i mod 4 = 3 then assert (value = '\255')) uniform;
              let peaked =
                world
                  ~sun:(`Manual (0., 0.7))
                  [
                    gradient (W.rgb 0.02 0.02 0.02);
                    W.Shape
                      {
                        shape = W.Circle;
                        azimuth = 0.5;
                        elevation = 0.5;
                        width = 0.4;
                        height = 0.4;
                        softness = 0.05;
                        color = white;
                        intensity = 8.;
                        emit = W.Dome;
                      };
                    W.Sun { angular_radius = 0.12; intensity = 50.; tint = white };
                  ]
              in
              let reference = capture tracer peaked 4096
              and bsdf_reference = capture bsdf peaked 4096 in
              let difference = Float.abs (mean_red reference -. mean_red bsdf_reference) in
              assert (mean_red reference > 0. && difference < 1.5);
              assert (rmse reference bsdf_reference < 4.);
              let mis_short = capture tracer peaked 16 and bsdf_short = capture bsdf peaked 16 in
              let mis_error = rmse mis_short reference and bsdf_error = rmse bsdf_short reference in
              assert (mis_error < bsdf_error);
              Printf.printf
                "World: furnace %.2f, converged mean difference %.3f, 16-spp RMSE MIS %.3f BSDF %.3f\n"
                furnace_mean difference mis_error bsdf_error;
              let miss_camera =
                Rays.Camera.perspective ~fov_y:0.3 ~at:(v3 0. 0. 6.) ~target:(v3 0. 0. 7.) ()
              in
              let background = W.rgb 0.2 0.4 0.6 in
              let colored =
                capture ~camera:miss_camera tracer
                  (world ~background:(W.Color background) [ gradient white ])
                  2
              in
              let expected =
                [| resolved background.r; resolved background.g; resolved background.b; 255 |]
              in
              Bytes.iteri (fun i value -> assert (Char.code value = expected.(i mod 4))) colored;
              let transparent =
                capture ~camera:miss_camera tracer
                  (world ~background:W.Transparent [ gradient white ])
                  2
              in
              Bytes.iter (fun value -> assert (value = '\000')) transparent;
              (* With the sphere in view only the miss pixels are background: a
             corner is the colour or alpha 0, the centre an opaque hit. *)
              let wide = camera ~fov:0.6 () and center = ((16 * 32) + 16) * 4 in
              let over =
                capture ~camera:wide tracer
                  (world ~background:(W.Color background) [ gradient white ])
                  2
              in
              for channel = 0 to 3 do
                assert (Char.code (Bytes.get over channel) = expected.(channel))
              done;
              assert (
                Bytes.get over (center + 3) = '\255'
                && Bytes.sub over center 3 <> Bytes.sub over 0 3);
              let cut =
                capture ~camera:wide tracer (world ~background:W.Transparent [ gradient white ]) 2
              in
              assert (Bytes.get cut 3 = '\000' && Bytes.get cut (center + 3) = '\255');
              let mirror =
                scene
                  (P.material ~roughness:0.005 ~metallic:1. (rgb 0.8 0.8 0.8))
                  [ P.rect_light ~intensity:4. ~size:(6., 6.) ~target:(v3 0. 0. 0.) (v3 0. 0. 8.) ]
              in
              let geometry = get (P.mesh ~spheres:mirror.spheres []) in
              get (P.replace_mesh tracer geometry);
              get (P.replace_mesh bsdf geometry);
              get (P.set_lights tracer mirror.lights);
              get (P.set_lights bsdf mirror.lights);
              let dark = world [ gradient (W.rgb 0. 0. 0.) ] in
              let reflected = capture bsdf dark 1024 and sampled = capture tracer dark 1024 in
              let center = ((16 * 32) + 16) * 4 in
              assert (Char.code (Bytes.get reflected center) > 200);
              assert (Float.abs (mean_red reflected -. mean_red sampled) < 2.)));
      assert (live () = baseline);
      print_endline
        "pathtracer World: furnace, MIS, backgrounds, reflected rect and teardown checked"

(* Convergence probe for specification/pathtracer.md (not in runtest):
   dune exec --root . lib/rays_pathtracer/test_main.exe -- bench_world *)
let bench () =
  let frames t cam n =
    for _ = 1 to n do
      get (P.render t cam);
      get (P.flush t)
    done;
    Test_pathtracer.pixels t
  in
  let floor =
    get
      (Result.map_error Rdk.Error.to_string
         (Rdk_test_support.Box_generator.box ~center:(v3 0. (-1.05) 0.) ~size:(v3 6. 0.1 6.) ()))
  in
  let ball =
    get
      (Result.map_error Rdk.Error.to_string
         (Rdk_test_support.Uv_sphere.run ~center:(v3 0. 0. 0.) ~segments:48 ~rings:24 ~radius:1. ()))
  in
  let scene =
    {
      P.objects =
        [
          (rays_mesh floor, P.material ~roughness:0.7 (rgb 0.7 0.7 0.7));
          (rays_mesh ball, P.material ~roughness:0.3 (rgb 0.8 0.5 0.3));
        ];
      spheres = [];
      strands = [];
      environment =
        {
          sky = rgb 0.8 0.8 0.8;
          ground = rgb 0.3 0.3 0.3;
          panels = [ P.panel ~intensity:8. ~width:0.4 ~height:0.3 (v3 0.3 1. 0.2) ];
        };
      lights = [];
    }
  in
  let room emit =
    match List.assoc "white room" W.presets with
    | { W.layers = [ ({ layer = Room r; _ } as e) ]; _ } as w ->
        W.bake ~width:512 ~height:256
          { w with layers = [ { e with layer = Room { r with emit } } ] }
    | _ -> failwith "white room preset changed"
  in
  let configs =
    [
      ("procedural dome", None);
      ("room, panels in map", Some (room W.Dome));
      ("room, promoted panels", Some (room W.Light));
    ]
  in
  let cam = Rays.Camera.perspective ~fov_y:0.8 ~at:(v3 0. 0.5 5.) ~target:(v3 0. (-0.2) 0.) () in
  let render world n =
    let t = get (P.create ~spp:1 ~width:256 ~height:256 scene) in
    get (P.set_world t world);
    ignore (frames t cam 1);
    P.reset t;
    let start = Unix.gettimeofday () in
    let image = frames t cam n in
    let ms = (Unix.gettimeofday () -. start) *. 1000. /. float n in
    P.destroy t;
    (image, ms)
  in
  let references = List.map (fun (_, w) -> fst (render w 2048)) configs in
  for round = 1 to 3 do
    List.iter2
      (fun (name, w) reference ->
        let image16, ms = render w 16 in
        let image64, _ = render w 64 in
        Printf.printf "round %d %-22s %.2f ms/frame  rmse@16 %.2f  rmse@64 %.2f\n%!" round name ms
          (rmse image16 reference) (rmse image64 reference))
      configs references
  done
