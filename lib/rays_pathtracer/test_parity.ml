(* Raster/path tracer parity (plan D15): each built-in World lights a matte
   and a glossy probe; the raster World path must land near the converged
   path tracer in mean probe luminance and in where the brightest highlight
   sits. Qualification only: it converges hundreds of samples per preset. *)
open Rays
module P = Rays_pathtracer

let get = function Ok v -> v | Error e -> failwith e
let width = 128 and height = 64
let camera = Camera.perspective ~fov_y:0.5 ~at:(Rays_math.Vec3.create 0. 0. 7.) ~target:Rays_math.Vec3.zero ()
let probes = [| Rays_math.Vec3.create (-1.3) 0. 0.; Rays_math.Vec3.create 1.3 0. 0. |]

(* Both renderers see the same surfaces: raster's Material mapping gives
   albedo = diffuse (sRGB decoded), roughness = sqrt (2 / (shininess + 2)). *)
let gray = 200
let albedo = Float.pow (float_of_int gray /. 255.) 2.2
let shininess = [| 1.; 2000. |]
let roughness s = Float.min 1. (sqrt (2. /. (s +. 2.)))

(* Pixels inside each probe's disc on screen. *)
let disc index =
  let center = Option.get (Camera.world_to_screen ~viewport:(0, 0, width, height)
      camera probes.(index)) in
  let edge = Option.get (Camera.world_to_screen ~viewport:(0, 0, width, height)
      camera (Rays_math.Vec3.add probes.(index) (Rays_math.Vec3.create 0.9 0. 0.))) in
  let radius = Float.abs (edge.x -. center.x) in
  List.concat (List.init height (fun y -> List.filter_map (fun x ->
    if Float.hypot (float x +. 0.5 -. center.x) (float y +. 0.5 -. center.y) < radius
    then Some (x, y) else None) (List.init width Fun.id)))

let luminance bytes (x, y) =
  let at channel = float_of_int (Char.code (Bytes.get bytes (4 * (y * width + x) + channel))) in
  0.2126 *. at 0 +. 0.7152 *. at 1 +. 0.0722 *. at 2

let mean bytes pixels =
  List.fold_left (fun sum p -> sum +. luminance bytes p) 0. pixels
  /. float_of_int (List.length pixels)

(* The highlight is the probe's half-maximum region: the pixels at least
   halfway from the mean to the brightest. A narrower band is a handful of
   pixels on a mirror-like probe, and noise then picks one of several equally
   bright reflections. *)
let highlight bytes pixels =
  let peak = List.fold_left (fun peak p -> Float.max peak (luminance bytes p)) 0. pixels in
  let low = mean bytes pixels in
  List.filter (fun p -> luminance bytes p >= peak -. (0.5 *. (peak -. low))) pixels

let centroid bright =
  let n = float_of_int (List.length bright) in
  List.fold_left (fun (x, y) (px, py) -> x +. float px /. n, y +. float py /. n) (0., 0.) bright

(* Mean distance from each highlight pixel to the other image's nearest one,
   the larger direction: it sees one displaced reflection among several, which
   a centroid of symmetric reflections does not. *)
let spread a b =
  let one a b = List.fold_left (fun sum (x, y) -> sum +. List.fold_left (fun best (u, v) ->
    Float.min best (Float.hypot (float (x - u)) (float (y - v)))) infinity b) 0. a
    /. float_of_int (List.length a) in
  Float.max (one a b) (one b a)

let run () =
  let live = let _, live = Ogpu.Impl.create_driver () in live in
  let baseline = live () in
  let rdk value = get (Result.map_error Rdk.Error.to_string value) in
  let rays_mesh geometry = get (Result.map_error Rdk.Error.to_string
    (Rdk_rays.Rays_mesh.to_trace_mesh geometry)) in
  let geometry = Array.map (fun center -> rdk (Rdk.Uv_sphere.run
    ~center ~radius:0.9 ~segments:64 ~rings:32 ())) probes in
  let objects = Array.to_list (Array.mapi (fun i geometry ->
    rays_mesh geometry, P.material ~roughness:(roughness shininess.(i))
      (P.Linear_color.rgb albedo albedo albedo)) geometry) in
  let scene = { P.objects; spheres = []; strands = [];
    environment = { sky = P.Linear_color.rgb 0. 0. 0.; ground = P.Linear_color.rgb 0. 0. 0.; panels = [] };
    lights = [] } in
  match P.create ~spp:4 ~width ~height scene with
  | Error message when Sys.getenv_opt "RAYS_REQUIRE_RAYTRACING" = None ->
      Printf.printf "raster/path tracer parity: skipped (%s)\n" message
  | Error message -> failwith message
  | Ok tracer ->
      let failures = ref [] in
      Fun.protect ~finally:(fun () -> P.destroy tracer) (fun () ->
        let canvas = get (Canvas.create ~width ~height) in
        Fun.protect ~finally:(fun () -> ignore(Canvas.destroy canvas)) (fun () ->
          let nodes = Array.to_list (Array.mapi (fun i geometry ->
            let mesh = rdk (Rdk_rays.Rays_mesh.to_mesh geometry) in
            Scene.D3.mesh ~material:(Material.create ~diffuse:(Rays_math.Color.gray gray)
              ~shininess:shininess.(i) ()) mesh) geometry) in
          let discs = Array.init 2 disc in
          Array.iter (fun pixels -> assert (pixels <> [])) discs;
          assert (List.length World.presets = 4);
          List.iter (fun (name, world) ->
            let baked = World.bake ~domains:1 ~width:128 ~height:64 world in
            get (P.set_world tracer (Some baked)); P.reset tracer;
            for _ = 1 to 256 do get (P.render tracer camera); get (P.flush tracer) done;
            assert (P.samples tracer = 1024);
            let traced = Test_pathtracer.pixels tracer in
            get(Canvas.render canvas [Scene.view3d ~camera
              (Scene.D3.with_world baked (Scene.D3.create nodes))]);
            let image = get (Canvas.to_image canvas) in
            let raster = Fun.protect ~finally:(fun () -> ignore(Image.destroy image))
              (fun () -> Bytes.copy (get (Image.Private.pixels image))) in
            assert (Bytes.length traced = width * height * 4 &&
              Bytes.length raster = Bytes.length traced);
            Array.iteri (fun i pixels ->
              let traced_mean = mean traced pixels and raster_mean = mean raster pixels in
              assert (traced_mean > 0. && raster_mean > 0.);
              let relative = Float.abs (traced_mean -. raster_mean) /. traced_mean in
              let traced_light = highlight traced pixels and raster_light = highlight raster pixels in
              let tx, ty = centroid traced_light and rx, ry = centroid raster_light in
              let distance = Float.hypot (tx -. rx) (ty -. ry) in
              let spread = spread traced_light raster_light in
              Printf.printf "parity %s probe %d: mean path %.2f raster %.2f (%.1f%%), highlight %.2f px, spread %.2f px\n%!"
                name i traced_mean raster_mean (relative *. 100.) distance spread;
              (* Raster's SH/prefiltered approximation has a separate error
                 allowance. Measured natively on M1: mean error
                 0.0-2.9%, highlight centroid distance 0.2-4.8 px, spread
                 0.03-2.4 px (the probe disc radius is about 16 px). *)
              if relative > 0.05 || distance > 8. || spread > 4. then
                failures := Printf.sprintf "%s probe %d" name i :: !failures)
              discs) World.presets));
      assert (live () = baseline);
      match !failures with
      | [] -> print_endline "raster/path tracer parity: all rendered presets within bounds"
      | failed -> failwith ("parity outside bounds: " ^ String.concat ", " (List.rev failed))
