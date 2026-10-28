module B = Ogpu.Backend
open Ogpu.Types
let ( let* ) = Result.bind
let gpu result = Result.map_error Ogpu.Error.to_string result
let put_f32 bytes offset value = Bytes.set_int32_le bytes offset (Int32.bits_of_float value)
let put_u32 bytes offset value = Bytes.set_int32_le bytes offset (Int32.of_int value)
let color_tuple (color : Rays_math.Color.Linear.t) = color.r, color.g, color.b
let buffer ?(memory = Shared) ?(usage = [ Storage ]) device ~label size =
  gpu (B.create_buffer ~memory device
    { label = Some label; size = Int64.of_int (max 4 size); usage })

(* [pixels] holds the indices [0, n) the Lisp kernels map over, [resolved] their RGBA in [0, 1] and [rgba]
   its RGBA8 rows padded to 256 bytes, copied into an output texture. *)
type film = {
  accum : B.buffer;
  history_color : B.buffer array;
  history_geometry : B.buffer array;
  outputs : B.texture array;
  pixels : B.buffer;
  resolved : B.buffer;
  rgba : B.buffer;
}

let row_pitch width = (width * 4 + 255) / 256 * 256

type facts = {
  width : int;
  height : int;
  sky : Rays_math.Color.Linear.t;
  ground : Rays_math.Color.Linear.t;
  frame : int;
  spp : int;
  world : Rays.World.baked option;
  panel_count : int;
  bounces : int;
  exposure : float;
  round_samples : int;
  light_count : int;
  moving : bool;
  history_camera : Rays.Camera.t option;
  bsdf_only : bool;
}

(* ponytail: a failure midway leaks the earlier allocations to the device's
   leak report, like [create]; allocation failures are out-of-memory only. *)
let allocate_film device ~width ~height =
  let history label =
    buffer ~memory:Ogpu.Types.Device_local device ~label (width * height * 16) in
  let texture index =
    gpu
      (B.create_texture device
         {
           label = Some (Printf.sprintf "pathtracer-film-%d" index);
           width;
           height;
           depth = 1;
           mip_levels = 1;
           sample_count = 1;
           format = Rgba8_unorm;
           usage = [ Texture_binding; Storage_binding; Texture_copy_src; Texture_copy_dst ];
         })
  in
  let* accum = history "pathtracer-accum" in
  let* color0 = history "pathtracer-history-color-0" in
  let* color1 = history "pathtracer-history-color-1" in
  let* geometry0 = history "pathtracer-history-geometry-0" in
  let* geometry1 = history "pathtracer-history-geometry-1" in
  let* output0 = texture 0 in
  let* output1 = texture 1 in
  let* output2 = texture 2 in
  let indices = Bytes.create (4 * width * height) in
  for i = 0 to (width * height) - 1 do put_f32 indices (4 * i) (float i) done;
  let* pixels = buffer device ~label:"pathtracer-pixels" (Bytes.length indices) in
  let* () = gpu (B.write_buffer pixels ~offset:0L indices) in
  let* resolved = history "pathtracer-resolved" in
  let* rgba = buffer ~memory:Ogpu.Types.Device_local ~usage:[ Storage; Copy_src ] device ~label:"pathtracer-rgba"
      (row_pitch width * height) in
  Ok { accum; history_color = [| color0; color1 |];
       history_geometry = [| geometry0; geometry1 |]; outputs = [| output0; output1; output2 |];
       pixels; resolved; rgba }

let destroy_film film =
  List.iter (fun value -> ignore (B.destroy_buffer value))
    (film.accum :: film.pixels :: film.resolved :: film.rgba
     :: Array.to_list film.history_color @ Array.to_list film.history_geometry);
  Array.iter (fun texture -> ignore (B.destroy_texture texture)) film.outputs

let camera_fov camera =
  match Rays.Camera.projection camera with
  | Perspective { fov_y; _ } -> Ok fov_y
  | Orthographic _ -> Error "path tracer requires a perspective camera"

let normalize (v : Rays_math.Vec3.t) =
  let l = sqrt ((v.x *. v.x) +. (v.y *. v.y) +. (v.z *. v.z)) in
  if l = 0. then Rays_math.Vec3.create 0. 0. 1. else Rays_math.Vec3.create (v.x /. l) (v.y /. l) (v.z /. l)

(* The camera's forward, right and up axes. *)
let axes (camera : Rays.Camera.t) =
  let open Rays_math.Vec3 in
  let cross a b =
    create
      ((a.y *. b.z) -. (a.z *. b.y))
      ((a.z *. b.x) -. (a.x *. b.z))
      ((a.x *. b.y) -. (a.y *. b.x))
  in
  let p = Rays.Camera.position camera and target = Rays.Camera.target camera in
  let forward = normalize (create (target.x -. p.x) (target.y -. p.y) (target.z -. p.z)) in
  let camera_up = Rays.Camera.up camera in
  let world_up =
    if Float.abs (dot forward camera_up) > 0.999 then
      if Float.abs forward.y > 0.999 then create 0. 0. 1. else create 0. 1. 0.
    else camera_up
  in
  let right = normalize (cross forward world_up) in
  (forward, right, cross right forward)

let uniform_bytes (facts : facts) (camera : Rays.Camera.t) fov ~row_offset =
  let open Rays_math.Vec3 in
  let forward, right, up = axes camera in
  let bytes = Bytes.make 288 '\000' in
  let put_vec4 offset (v : Rays_math.Vec3.t) w =
    put_f32 bytes offset v.x;
    put_f32 bytes (offset + 4) v.y;
    put_f32 bytes (offset + 8) v.z;
    put_f32 bytes (offset + 12) w
  in
  let put_rgb offset color =
    let r, g, b = color_tuple color in
    put_vec4 offset (create r g b) 0. in
  put_vec4 0 (Rays.Camera.position camera) (Rays.Camera.lens camera).aperture;
  put_vec4 16 forward (tan (fov /. 2.));
  put_vec4 32 right (float_of_int facts.width /. float_of_int facts.height);
  put_vec4 48 up (Rays.Camera.focus_distance camera);
  put_rgb 64 facts.sky;
  put_rgb 80 facts.ground;
  put_u32 bytes 96 facts.width;
  put_u32 bytes 100 facts.height;
  put_u32 bytes 276 row_offset;
  put_u32 bytes 104 facts.frame;
  put_u32 bytes 108 facts.spp;
  put_u32 bytes 112 (if facts.world = None then facts.panel_count else 0);
  put_u32 bytes 116 facts.bounces;
  put_f32 bytes 120 facts.exposure;
  (match facts.world with
   | None -> ()
   | Some (baked : Rays.World.baked) ->
       put_f32 bytes 120 (facts.exposure *. (2. ** baked.exposure));
       put_u32 bytes 208 1;
       put_u32 bytes 212 (match baked.background with Environment -> 0 | Color _ -> 1 | Transparent -> 2);
       put_u32 bytes 216 baked.camera.width;
       put_u32 bytes 220 baked.camera.height;
       (match baked.background with
        | Color c -> put_vec4 224 (create c.r c.g c.b) 0.
        | Environment | Transparent -> ());
       (match baked.sun with
        | Some sun when sun.angular_radius > 0. ->
            (* 1 - cos r = 2 sin^2 (r / 2), exact for tiny discs. *)
            let s = sin (sun.angular_radius /. 2.) in
            let one_minus_cos = 2. *. s *. s in
            put_vec4 240 (normalize sun.direction) one_minus_cos;
            put_vec4 256 (create sun.radiance.r sun.radiance.g sun.radiance.b)
              (1. /. (2. *. Float.pi *. one_minus_cos))
        | _ -> ());
       put_u32 bytes 272 (if facts.bsdf_only then 1 else 0));
  put_u32 bytes 124 facts.round_samples;
  put_u32 bytes 128 facts.light_count;
  put_u32 bytes 132 (if facts.moving then 1 else 0);
  let* () =
    match facts.history_camera with
    | None -> Ok ()
    | Some previous ->
        let* previous_fov = camera_fov previous in
        let forward, right, up = axes previous in
        put_u32 bytes 136 1;
        put_vec4 144 (Rays.Camera.position previous) 0.;
        put_vec4 160 forward (tan (previous_fov /. 2.));
        put_vec4 176 right (float_of_int facts.width /. float_of_int facts.height);
        put_vec4 192 up 0.;
        Ok ()
  in
  Ok bytes

(* GPU resources a frame in flight may still read are freed once none is. *)
