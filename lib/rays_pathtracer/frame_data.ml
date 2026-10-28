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

(* The Lisp kernels map over [pixels], the image's pixel indices in 16 x 16 tiles (the tracing kernels, whose
   films hold pixel p at [order.(p)]) or over [linear] ones (the resolves, writing [resolved], their RGBA in
   [0, 1]), drawn into an output texture by Flow's image sink pass. *)
type film = {
  accum : B.buffer;
  history_color : B.buffer array;
  history_geometry : B.buffer array;
  outputs : B.texture array;
  pixels : B.buffer;
  linear : B.buffer;
  order : B.buffer;
  resolved : B.buffer;
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
           usage = [ Texture_binding; Storage_binding; Render_attachment; Texture_copy_src; Texture_copy_dst ];
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
  (* a threadgroup of 256 traces a 16 x 16 block, each SIMD group of 32 an 8 x 4 tile of it; one staging
     buffer is filled and uploaded three times: the tiled indices, their inverse, the identity *)
  let n = width * height in
  let bytes = Bytes.create (4 * n) in
  let tiled f = let t = ref 0 in
    let tiles w h x0 y0 x1 y1 f =
      for ty = 0 to (y1 - y0 - 1) / h do for tx = 0 to (x1 - x0 - 1) / w do
        f (x0 + w * tx) (y0 + h * ty) (min x1 (x0 + w * tx + w)) (min y1 (y0 + h * ty + h))
      done done in
    tiles 16 16 0 0 width height (fun x0 y0 x1 y1 -> tiles 8 4 x0 y0 x1 y1 (fun x0 y0 x1 y1 ->
      for y = y0 to y1 - 1 do for x = x0 to x1 - 1 do f !t (y * width + x); incr t done done)) in
  let upload label fill = fill ();
    let* b = buffer device ~label (Bytes.length bytes) in
    let* () = gpu (B.write_buffer b ~offset:0L bytes) in Ok b in
  let* pixels = upload "pathtracer-pixels" (fun () -> tiled (fun t p -> put_f32 bytes (4 * t) (float p))) in
  let* order = upload "pathtracer-order" (fun () -> tiled (fun t p -> put_f32 bytes (4 * p) (float t))) in
  let* linear = upload "pathtracer-linear" (fun () -> for i = 0 to n - 1 do put_f32 bytes (4 * i) (float i) done) in
  let* resolved = history "pathtracer-resolved" in
  Ok { accum; history_color = [| color0; color1 |];
       history_geometry = [| geometry0; geometry1 |]; outputs = [| output0; output1; output2 |];
       pixels; linear; order; resolved }

let destroy_film film =
  List.iter (fun value -> ignore (B.destroy_buffer value))
    (film.accum :: film.pixels :: film.linear :: film.order :: film.resolved
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
