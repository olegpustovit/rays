open Rays
let render canvas scene=Result.get_ok(Canvas.render canvas scene)
let stats canvas=Result.get_ok(Canvas.Private.native_stats canvas)

(* The path tracer's ACES fit and gamma (resolved_rgba), in OCaml. *)
let tone linear exposure =
  let c = Rays_math.Color.aces (linear *. exposure) in
  int_of_float ((Float.pow (Float.min 1. (Float.max 0. c)) (1. /. 2.2) *. 255.) +. 0.5)

let near ~tolerance name expected (c : Rays_math.Color.t) =
  List.iter (fun (channel, value) ->
    if abs (value - expected) > tolerance then
      failwith (Printf.sprintf "%s: %s channel %d, expected %d +- %d" name channel
        value expected tolerance))
    [ "r", c.r; "g", c.g; "b", c.b ]

let uniform l =
  let c = World.rgb l l l in
  { World.default with
    layers = [ { name = "furnace"; visible = true;
                 layer = Gradient { zenith = c; horizon = c; nadir = c; sharpness = 1. } } ] }

let furnace () =
  let live_handles = snd (Ogpu.Impl.create_driver ()) in
  let baseline = live_handles () in
  let radiance = 0.5 in
  let baked = World.bake ~domains:1 ~width:64 ~height:32 (uniform radiance) in
  let camera = Camera.perspective ~at:(Rays_math.Vec3.create 0. 0. 4.) ~target:Rays_math.Vec3.zero () in
  let sphere = Scene.D3.mesh ~material:(Material.matte Rays_math.Color.white)
      (Test_meshes.sphere ~radius:1. ()) in
  let scene baked = [ Scene.clear Rays_math.Color.black;
    Scene.view3d ~camera (Scene.D3.with_world baked (Scene.D3.create [ sphere ])) ] in
  let canvas = Canvas.create_exn ~width:64 ~height:64 in
  let uploaded () = (stats canvas).uploaded_bytes in
  let pixel x y = Option.get(Result.get_ok(Canvas.pixel canvas ~x ~y)) in
  Fun.protect ~finally:(fun () -> ignore(Canvas.destroy canvas)) (fun () ->
    (* Furnace: a white Lambertian under constant L reflects L; the camera
       map behind it is L too, both through the same tone map. *)
    let expected = tone radiance 1. in
    render canvas (scene baked);
    let first = uploaded () in
    near ~tolerance:3 "furnace sphere centre" expected (pixel 32 32);
    near ~tolerance:1 "camera-map background" expected (pixel 1 1);
    (* One upload per bake: an identical frame re-sends no texture (the
       camera map alone is 64x32x8 bytes). *)
    render canvas (scene baked);
    let delta = Int64.sub (uploaded ()) first in
    if delta >= 16384L then
      failwith (Printf.sprintf "World maps re-uploaded: %Ld bytes" delta);
    (* Color background paints that radiance; only background changed, so the
       shared maps stay uploaded. Exposure is stops through the tone map. *)
    let before = uploaded () in
    let tinted = { baked with background = Color (World.rgb 0.1 0.1 0.1); exposure = 1. } in
    render canvas (scene tinted);
    let delta = Int64.sub (uploaded ()) before in
    if delta >= 16384L then
      failwith (Printf.sprintf "background change re-uploaded maps: %Ld bytes" delta);
    near ~tolerance:1 "color background" (tone 0.1 2.) (pixel 1 1);
    near ~tolerance:3 "exposed sphere" (tone radiance 2.) (pixel 32 32);
    let clear = { baked with background = Transparent } in
    render canvas (scene clear);
    if pixel 1 1 <> Rays_math.Color.black then failwith "transparent background keeps the clear");
  (* Degenerate drawings draw nothing instead of failing the frame: an empty
     mesh (a cook with no triangles), a zero-length line, a zero circle. *)
  let canvas = Canvas.create_exn ~width:32 ~height:32 in
  Fun.protect ~finally:(fun () -> ignore(Canvas.destroy canvas)) (fun () ->
    let empty = Result.get_ok (Mesh.Private.create_owned ~mode:Mesh.Triangles ~indices:[||] [||]) in
    render canvas [Scene.view3d ~camera (Scene.D3.create [Scene.D3.mesh empty; sphere])];
    render canvas [Scene.line ~from_:(4, 4) ~to_:(4, 4) ~color:Rays_math.Color.white ~width:1 ();
      Scene.circle ~at:(8, 8) ~radius:0 ~fill:Rays_math.Color.white ()]);
  let after = live_handles () in
  if after <> baseline then failwith "World raster native handle delta";
  print_endline "world raster: furnace sphere, camera/color/transparent background, one upload per bake, zero handle delta"

(* A World sun straight down (+Z here) through the GPU sun map: the plane
   under a floating box is darker than beside it, the same scene without a
   sun shows no patch, and an identical re-render reuses the map. *)
let sun_shadow () =
  let live_handles = snd (Ogpu.Impl.create_driver ()) in
  let baseline = live_handles () in
  let baked = World.bake ~domains:1 ~width:64 ~height:32 (uniform 0.2) in
  let sun = { World.direction = Rays_math.Vec3.unit_z; radiance = World.rgb 2000. 2000. 2000.;
              angular_radius = 0.02 } in
  let camera = Camera.perspective ~at:(Rays_math.Vec3.create 0. (-4.) 5.) ~target:Rays_math.Vec3.zero () in
  let matte = Material.matte Rays_math.Color.white in
  let plane = Scene.D3.mesh ~material:matte (Mesh.plane ~width:8. ~height:8. ())
  and box = Scene.D3.mesh ~material:matte (Test_meshes.box ~width:1. ~height:1. ~depth:1. ()) in
  (* Rebuilt around the same meshes, a scene keeps its sun map. *)
  let scene baked = [ Scene.clear Rays_math.Color.black;
    Scene.view3d ~camera (Scene.D3.with_world baked (Scene.D3.create [
      plane; Scene.D3.translate (Rays_math.Vec3.create 0. 0. 1.5) [ box ] ])) ] in
  let canvas = Canvas.create_exn ~width:128 ~height:128 in
  let luminance p =
    let x, y = match Camera.world_to_screen ~viewport:(0, 0, 128, 128) camera p with
      | Some s -> int_of_float s.x, int_of_float s.y | None -> failwith "off screen" in
    let c = Option.get(Result.get_ok(Canvas.pixel canvas ~x ~y)) in c.r + c.g + c.b in
  let under = Rays_math.Vec3.create 0. 0.2 0. and beside = Rays_math.Vec3.create 2.5 0. 0. in
  let passes () = (stats canvas).sun_shadow_passes in
  Fun.protect ~finally:(fun () -> ignore(Canvas.destroy canvas)) (fun () ->
    let lit = scene { baked with sun = Some sun } in
    render canvas lit;
    let shadowed = luminance under and open_ = luminance beside in
    if shadowed + 150 > open_ then
      failwith (Printf.sprintf "no sun shadow under the box: %d vs %d beside" shadowed open_);
    let first = passes () in
    if first <> 1L then failwith (Printf.sprintf "%Ld sun map passes, expected 1" first);
    render canvas lit;
    render canvas (scene { baked with sun = Some sun });
    if passes () <> first then failwith "identical scene re-rendered the sun map";
    let tilted = { sun with direction = Rays_math.Vec3.normalize (Rays_math.Vec3.create 0.3 0. 1.) } in
    render canvas (scene { baked with sun = Some tilted });
    if passes () <> Int64.succ first then failwith "a moved sun kept the old sun map";
    render canvas (scene baked);
    let a = luminance under and b = luminance beside in
    if abs (a - b) > 3 then
      failwith (Printf.sprintf "sunless World shows a patch: %d vs %d" a b));
  if live_handles () <> baseline then failwith "World sun shadow native handle delta";
  print_endline "world raster: GPU sun shadow under a box, none without a sun, map reused, zero handle delta"

let run () = furnace (); sun_shadow ()
