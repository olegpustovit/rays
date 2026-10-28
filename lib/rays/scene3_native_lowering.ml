type error =
  | Unsupported_mode
  | Unsupported_texture
  | Too_many_lights
  | Invalid_mesh
  | Invalid_viewport
  | Resource of Ogpu.Error.t

type resources = {
  texture : Scene3.texture -> (Renderer.Scene_execution.sampled_texture, error) result;
}

type prepared = Renderer.Scene_execution.prepared_scene3

let comparison = function
  | Scene3.Never -> Ogpu.Render_pass.Never
  | Less -> Less
  | Equal -> Equal
  | Less_equal -> Less_equal
  | Greater -> Greater
  | Not_equal -> Not_equal
  | Greater_equal -> Greater_equal
  | Always -> Always

let blend = function
  | Scene3.Replace -> Ogpu.Pipeline.Replace
  | Alpha -> Alpha
  | Add -> Add
  | Multiply -> Multiply
  | Screen -> Screen
  | Subtract -> Subtract

let packed_color (c : Rays_math.Color.t) =
  Int32.of_int ((c.r lsl 24) lor (c.g lsl 16) lor (c.b lsl 8) lor c.a)

let put32 bytes index value = Bytes.set_int32_le bytes (index * 4) (Int32.bits_of_float value)

let color bytes index (c : Rays_math.Color.t) =
  put32 bytes index (float c.r /. 255.);
  put32 bytes (index + 1) (float c.g /. 255.);
  put32 bytes (index + 2) (float c.b /. 255.);
  put32 bytes (index + 3) (float c.a /. 255.)

let matrix bytes offset value =
  for index = 0 to 15 do
    put32 bytes (offset + index) (Rays_math.Mat4.get value ~row:(index / 4) ~column:(index mod 4))
  done

(* Public camera clip depth is [-1,1]; native raster/shadow depth is [0,1]. *)
let native_clip_projection projection =
  Rays_math.Mat4.mul
    (Rays_math.Mat4.of_rows (1., 0., 0., 0.) (0., 1., 0., 0.) (0., 0., 0.5, 0.5) (0., 0., 0., 1.))
    projection
(* ---- World lighting (Scene3.with_world, specification/environment.md) ---- *)

(* The one Blinn-Phong to PBR mapping: albedo = diffuse (Rays_math.Color.t is sRGB, so
   the shader decodes it with gamma 2.2, the inverse of the tone map),
   roughness = sqrt (2 / (shininess + 2)) (Phong exponent vs GGX alpha^2),
   metallic 0, f0 = 0.04. A material with black diffuse, ambient and
   specular is unlit: its emissive color is written as is, never tone
   mapped. Ambient and the scene's ambient color are replaced by the World. *)
let pbr_of_material (m : Material.t) =
  let black (c : Rays_math.Color.t) = c.r = 0 && c.g = 0 && c.b = 0 in
  ( black m.diffuse && black m.ambient && black m.specular,
    Float.min 1. (sqrt (2. /. (m.shininess +. 2.))),
    0.04 )

type world = {
  baked : World.baked;
  sun : (Rays_math.Mat4.t * string) option;
  auxiliary : Renderer.Scene_execution.auxiliary_resource;
  camera_map : Renderer.Scene_execution.sampled_texture;
  lights : World.light array;
}

type world_maps = {
  specular_maps : World.map array;
  camera_pixels : Float.Array.t;
  specular : Renderer.Scene_execution.sampled_texture;
  camera : Renderer.Scene_execution.sampled_texture;
}

module World_maps = Lru.Identity_list
module Worlds = Lru.Identity_list
module Sun_fits = Lru.Identity_list
module Instance_frames = Lru.Identity_list

let world_capacity = 2

let make_world_maps_cache () =
  World_maps.create
    ~equal:(fun (maps_a, pixels_a) (maps_b, pixels_b) -> maps_a == maps_b && pixels_a == pixels_b)
    world_capacity

let make_worlds_cache () =
  Worlds.create
    ~equal:(fun (baked_a, sun_a) (baked_b, sun_b) -> baked_a == baked_b && sun_a = sun_b)
    world_capacity

let make_instance_frames_cache () =
  Instance_frames.create
    ~equal:(fun (parent_a, transforms_a) (parent_b, transforms_b) ->
      parent_a = parent_b && transforms_a == transforms_b)
    8

type instance_cache_context = {
  instance_frames :
    ( Rays_math.Mat4.t * Rays_math.Mat4.t array,
      Rays_math.Mat4.t array * Rays_math.Mat4.t array )
    Instance_frames.t;
}

let current_instance_cache = Domain.DLS.new_key (fun () -> None)
let create_instance_cache_context () = { instance_frames = make_instance_frames_cache () }

(* Physically keyed, so re-lowering the same bake never repacks or
   re-uploads; the renderer's texture cache keeps each "world:" key once. *)
type world_cache_context = {
  world_maps : (World.map array * Float.Array.t, world_maps) World_maps.t;
  worlds : (World.baked * string option, world) Worlds.t;
}

let current_world_cache = Domain.DLS.new_key (fun () -> None)

let create_world_cache_context () =
  { world_maps = make_world_maps_cache (); worlds = make_worlds_cache () }

(* Resource labels survive outside this cache context, so their identity allocator
   must remain process-unique even when two runs pack the same scene. *)
let next_resource_id = Atomic.make 1
let fresh_resource_id () = Atomic.fetch_and_add next_resource_id 1

let half_level (m : World.map) : Renderer.Scene_execution.texture_level =
  let bytes = Bytes.create (m.width * m.height * 8) and one = Ogpu.Types.half_of_float 1. in
  for i = 0 to (m.width * m.height) - 1 do
    for c = 0 to 2 do
      Bytes.set_uint16_le bytes
        ((i * 8) + (c * 2))
        (Ogpu.Types.half_of_float (Float.min 65504. (Float.Array.get m.pixels ((i * 3) + c))))
    done;
    Bytes.set_uint16_le bytes ((i * 8) + 6) one
  done;
  { width = m.width; height = m.height; bytes }

let world_sampler mips : Ogpu.Types.sampler_descriptor =
  {
    label = Some "scene3-world";
    min_filter = Linear;
    mag_filter = Linear;
    mip_filter = (if mips > 1 then Linear_mip else No_mip);
    address_u = Repeat;
    address_v = Clamp_to_edge;
    lod_min = 0.;
    lod_max = float (mips - 1);
    max_anisotropy = 1;
  }

let world_white : Renderer.Scene_execution.sampled_texture =
  {
    key = "world:white";
    levels = [| { width = 1; height = 1; bytes = Bytes.of_string "\255\255\255\255" } |];
    sampler =
      {
        label = Some "scene3-world-white";
        min_filter = Nearest;
        mag_filter = Nearest;
        mip_filter = No_mip;
        address_u = Clamp_to_edge;
        address_v = Clamp_to_edge;
        lod_min = 0.;
        lod_max = 0.;
        max_anisotropy = 1;
      };
    gpu = None;
  }

let maps_of (baked : World.baked) =
  let cache_key = (baked.specular, baked.camera.pixels) in
  let cache = Domain.DLS.get current_world_cache in
  let existing = Option.bind cache (fun context -> World_maps.peek context.world_maps cache_key) in
  match existing with
  | Some maps -> maps
  | None ->
      let key = Printf.sprintf "world:%d:" (fresh_resource_id ()) in
      let maps =
        {
          specular_maps = baked.specular;
          camera_pixels = baked.camera.pixels;
          specular =
            {
              key = key ^ "specular";
              levels = Array.map half_level baked.specular;
              sampler = world_sampler (Array.length baked.specular);
              gpu = None;
            };
          camera =
            {
              key = key ^ "camera";
              levels = [| half_level baked.camera |];
              sampler = world_sampler 1;
              gpu = None;
            };
        }
      in
      Option.iter (fun context -> World_maps.add context.world_maps cache_key maps) cache;
      maps

(* The World sun's shadow map: an orthographic view along the sun fitted to the bounds of the
   depth-writing Replace or Alpha triangle drawings (the renderer's
   casters), keyed by their meshes, transforms and instances plus the sun
   direction, so a scene rebuilt each frame from the same parts re-renders
   nothing. *)
type caster = {
  caster_mesh : Mesh.t;
  caster_transform : Rays_math.Mat4.t;
  caster_instances : Rays_math.Mat4.t array option;
}

type sun_fit = {
  casters : caster list;
  direction : Rays_math.Vec3.t;
  fit : (Rays_math.Mat4.t * string) option;
}

let casters scene =
  let casters = ref [] in
  Scene3.Walk.iter_batches
    (fun (drawing : Scene3.Walk.drawing) transforms ->
      let triangles =
        match (Mesh.View.view drawing.mesh).mode with
        | Triangles | Triangle_strip | Triangle_fan -> true
        | _ -> false
      in
      if
        triangles && drawing.mode = Faces
        && (drawing.blend = Replace || drawing.blend = Alpha)
        && drawing.depth.write
      then
        casters :=
          {
            caster_mesh = drawing.mesh;
            caster_transform = drawing.transform;
            caster_instances = transforms;
          }
          :: !casters)
    scene;
  List.rev !casters

let same_caster a b =
  a.caster_mesh == b.caster_mesh
  && a.caster_transform = b.caster_transform
  &&
  match (a.caster_instances, b.caster_instances) with
  | None, None -> true
  | Some x, Some y -> x == y || x = y
  | _ -> false

let make_sun_fits_cache () =
  Sun_fits.create
    ~equal:(fun (casters_a, direction_a) (casters_b, direction_b) ->
      direction_a = direction_b && List.equal same_caster casters_a casters_b)
    world_capacity

let current_sun_fits = Domain.DLS.new_key (fun () -> None)

let sun_fit scene (sun : World.sun) =
  let casters = casters scene in
  let cache = Domain.DLS.get current_sun_fits in
  match Option.bind cache (fun cache -> Sun_fits.peek cache (casters, sun.direction)) with
  | Some fit -> fit
  | None ->
      let f = Rays_math.Vec3.normalize (Rays_math.Vec3.neg sun.direction) in
      let side =
        Rays_math.Vec3.normalize
          (Rays_math.Vec3.cross f
             (if Float.abs f.y < 0.99 then Rays_math.Vec3.unit_y else Rays_math.Vec3.unit_x))
      in
      let up = Rays_math.Vec3.cross side f in
      let lo = Array.make 3 Float.infinity and hi = Array.make 3 Float.neg_infinity in
      let add p =
        List.iteri
          (fun axis v ->
            let d = Rays_math.Vec3.dot v p in
            lo.(axis) <- Float.min lo.(axis) d;
            hi.(axis) <- Float.max hi.(axis) d)
          [ side; up; f ]
      in
      let pick op (a : Rays_math.Vec3.t) (b : Rays_math.Vec3.t) =
        Rays_math.Vec3.create (op a.x b.x) (op a.y b.y) (op a.z b.z)
      in
      List.iter
        (fun c ->
          let vertices = (Mesh.View.view c.caster_mesh).vertices in
          if Array.length vertices > 0 then
            let a = Array.fold_left (pick Float.min) vertices.(0) vertices
            and b = Array.fold_left (pick Float.max) vertices.(0) vertices in
            let corners m =
              for k = 0 to 7 do
                add
                  (Rays_math.Mat4.transform_point m
                     (Rays_math.Vec3.create
                        (if k land 1 = 0 then a.x else b.x)
                        (if k land 2 = 0 then a.y else b.y)
                        (if k land 4 = 0 then a.z else b.z)))
              done
            in
            match c.caster_instances with
            | None -> corners c.caster_transform
            | Some transforms ->
                Array.iter (fun t -> corners (Rays_math.Mat4.mul c.caster_transform t)) transforms)
        casters;
      let fit =
        if not (Float.is_finite lo.(0)) then None
        else begin
          (* A 2% margin keeps edge texels and the depth extremes inside. *)
          let range axis = Float.max 1e-3 ((hi.(axis) -. lo.(axis)) *. 1.02)
          and middle axis = (hi.(axis) +. lo.(axis)) *. 0.5 in
          let row (v : Rays_math.Vec3.t) scale offset =
            (v.x *. scale, v.y *. scale, v.z *. scale, offset)
          in
          let sx = 2. /. range 0 and sy = 2. /. range 1 and sz = 1. /. range 2 in
          let id = fresh_resource_id () in
          Some
            ( Rays_math.Mat4.of_rows
                (row side sx (-.middle 0 *. sx))
                (row up sy (-.middle 1 *. sy))
                (row f sz (0.5 -. (middle 2 *. sz)))
                (0., 0., 0., 1.),
              Printf.sprintf "sun:%d" id )
        end
      in
      Option.iter (fun cache -> Sun_fits.add cache (casters, sun.direction) fit) cache;
      fit

(* Word layout: see [source_scene3_world] in lib/runtime/runtime.ml. *)
let world_of (baked : World.baked) sun =
  let key = (baked, Option.map snd sun) in
  let cache = Domain.DLS.get current_world_cache in
  match Option.bind cache (fun context -> Worlds.peek context.worlds key) with
  | Some world -> world
  | None ->
      let maps = maps_of baked in
      let bytes = Bytes.make (64 * 4) '\000' in
      let put = put32 bytes in
      let rgb index (c : World.rgb) k =
        put index (c.r *. k);
        put (index + 1) (c.g *. k);
        put (index + 2) (c.b *. k)
      in
      Array.iteri (fun k c -> rgb (k * 3) c 1.) baked.sh9;
      put 27 (2. ** baked.exposure);
      Option.iter
        (fun (sun : World.sun) ->
          put 28 sun.direction.x;
          put 29 sun.direction.y;
          put 30 sun.direction.z;
          put 31 1.;
          (* Irradiance of a small disc: radiance x solid angle ~ pi r^2. *)
          rgb 32 sun.radiance (Float.pi *. sun.angular_radius *. sun.angular_radius))
        baked.sun;
      (match baked.background with
      | World.Color c ->
          put 35 1.;
          rgb 36 c 1.
      | Environment | Transparent -> ());
      (* 2: the renderer's sun map. *)
      Option.iter
        (fun (view_projection, _) ->
          put 39 2.;
          matrix bytes 40 view_projection;
          put 56 0.001;
          put 57 0.005;
          put 58 1.;
          put 59 1.;
          put 60 (float Renderer.Scene_execution.sun_map_size);
          put 61 (float Renderer.Scene_execution.sun_map_size))
        sun;
      let power (l : World.light) =
        ((0.2126 *. l.radiance.r) +. (0.7152 *. l.radiance.g) +. (0.0722 *. l.radiance.b))
        *. Rays_math.Vec3.length l.u *. Rays_math.Vec3.length l.v
      in
      let id = fresh_resource_id () in
      let world =
        {
          baked;
          sun;
          camera_map = maps.camera;
          auxiliary =
            {
              key = Printf.sprintf "world:%d" id;
              buffer = bytes;
              texture = maps.specular;
              environment = Some maps.camera;
              sun_shadow = Option.map snd sun;
            };
          lights =
            Array.of_list
              (List.stable_sort (fun a b -> Float.compare (power b) (power a)) baked.lights);
        }
      in
      Option.iter (fun context -> Worlds.add context.worlds key world) cache;
      world

(* Uniform words 84..1363 hold 20 words per light, matching the shader loop. *)
let max_lights = 64

(* World words 76..78 and the extracted rects after the scene's own lights.
   ponytail: past 64 lights the lowest-power World rects are dropped (the
   scene's own lights always stay); a clustered light list lifts the cap. *)
let world_uniforms bytes world (material : Material.t) =
  let unlit, roughness, f0 = pbr_of_material material in
  put32 bytes 76 (if unlit then 1. else 0.);
  put32 bytes 77 roughness;
  put32 bytes 78 f0;
  let first = int_of_float (Int32.float_of_bits (Bytes.get_int32_le bytes (73 * 4))) in
  let count = min (Array.length world.lights) (max_lights - first) in
  for index = 0 to count - 1 do
    let l = world.lights.(index) and o = 84 + ((first + index) * 20) in
    let vector at (v : Rays_math.Vec3.t) =
      put32 bytes at v.x;
      put32 bytes (at + 1) v.y;
      put32 bytes (at + 2) v.z
    in
    (* The shader scales colors by pi (Light.t intensity 1 lights a white
       matte face to 1); a rect carries radiance x area / pi. *)
    let k = 4. *. Rays_math.Vec3.length l.u *. Rays_math.Vec3.length l.v /. Float.pi in
    put32 bytes o 4.;
    vector (o + 1) l.position;
    put32 bytes (o + 4) (l.radiance.r *. k);
    put32 bytes (o + 5) (l.radiance.g *. k);
    put32 bytes (o + 6) (l.radiance.b *. k);
    put32 bytes (o + 8) 1.;
    vector (o + 9) l.u;
    vector (o + 12) l.v;
    vector (o + 15) l.normal
  done;
  put32 bytes 73 (float (first + count))

(* A clip-space triangle covering the viewport; the shader turns each
   fragment into a view ray through the inverse view-projection at 84. *)
let background_mesh : Renderer.Scene_execution.mesh =
  let vertices = Bytes.make (3 * 24) '\000' in
  List.iteri
    (fun index (x, y) ->
      let put at value =
        Bytes.set_int32_le vertices ((index * 24) + at) (Int32.bits_of_float value)
      in
      put 0 x;
      put 4 y;
      put 8 0.5;
      put 20 1.)
    [ (-1., -1.); (3., -1.); (-1., 3.) ];
  let indices = Bytes.make 12 '\000' in
  Bytes.set_int32_le indices 4 1l;
  Bytes.set_int32_le indices 8 2l;
  {
    key = Host "world:background";
    vertices;
    vertex_count = 3;
    indices;
    index_count = 3;
    primitive = Triangle_list;
  }

let background_attributes =
  let bytes = Bytes.make 36 '\000' in
  for i = 0 to 2 do
    Bytes.set_int32_le bytes (i * 12) Int32.minus_one
  done;
  ("world:background:attributes", bytes)

let background_uniforms ~camera ~viewport =
  match
    Rays_math.Mat4.inverse (native_clip_projection (Camera.view_projection_matrix ~viewport camera))
  with
  | None -> None
  | Some inverse ->
      let bytes = Bytes.make 5456 '\000' in
      matrix bytes 0 Rays_math.Mat4.identity;
      matrix bytes 16 Rays_math.Mat4.identity;
      matrix bytes 32 Rays_math.Mat4.identity;
      put32 bytes 76 2.;
      put32 bytes 82 1.;
      matrix bytes 84 inverse;
      Some bytes

let uniforms_blinn_phong ~camera ~viewport scene (drawing : Scene3.Walk.drawing) =
  let bytes = Bytes.make 5456 '\000' and material = drawing.material in
  put32 bytes 82 1.;
  let projection = native_clip_projection (Camera.view_projection_matrix ~viewport camera) in
  matrix bytes 0 (Rays_math.Mat4.mul projection drawing.transform);
  matrix bytes 16 drawing.transform;
  let normal =
    match Rays_math.Mat4.inverse drawing.transform with
    | None -> Rays_math.Mat4.identity
    | Some value -> Rays_math.Mat4.transpose value
  in
  matrix bytes 32 normal;
  let eye = Camera.position camera in
  put32 bytes 48 eye.x;
  put32 bytes 49 eye.y;
  put32 bytes 50 eye.z;
  color bytes 52 material.ambient;
  color bytes 56 material.diffuse;
  color bytes 60 material.specular;
  color bytes 64 material.emissive;
  put32 bytes 68 material.shininess;
  let ambient = Scene3.Walk.ambient scene in
  color bytes 69 ambient;
  let lights = Array.of_list (Scene3.Walk.lights scene) in
  (* The native light ABI reserves linear slots; the zero-filled buffer
     keeps them zero while preserving the shared twenty-word layout. *)
  put32 bytes 73 (float (Array.length lights));
  put32 bytes 74 (if drawing.cull = Cull_none then 1. else 0.);
  put32 bytes 75 (if Scene3.Walk.separate_specular scene then 1. else 0.);
  Array.iteri
    (fun index (light : Light.t) ->
      let o = 84 + (index * 20) in
      let vector p =
        put32 bytes (o + 1) p.Rays_math.Vec3.x;
        put32 bytes (o + 2) p.y;
        put32 bytes (o + 3) p.z
      in
      color bytes (o + 4) light.diffuse;
      put32 bytes (o + 8) light.intensity;
      match light.kind with
      | Directional { direction } ->
          put32 bytes o 0.;
          vector direction
      | Point { position; attenuation } ->
          put32 bytes o 1.;
          vector position;
          put32 bytes (o + 9) attenuation.constant;
          put32 bytes (o + 11) attenuation.quadratic
      | Spot { position; direction; cutoff; concentration; attenuation } ->
          put32 bytes o 2.;
          vector position;
          put32 bytes (o + 9) direction.x;
          put32 bytes (o + 10) direction.y;
          put32 bytes (o + 11) direction.z;
          put32 bytes (o + 12) (cos cutoff);
          put32 bytes (o + 13) (cos cutoff);
          put32 bytes (o + 14) concentration;
          put32 bytes (o + 15) attenuation.constant;
          put32 bytes (o + 17) attenuation.quadratic
      | Area { position; direction; width; height; samples; attenuation } ->
          put32 bytes o 3.;
          vector position;
          put32 bytes (o + 9) direction.x;
          put32 bytes (o + 10) direction.y;
          put32 bytes (o + 11) direction.z;
          put32 bytes (o + 12) width;
          put32 bytes (o + 13) height;
          put32 bytes (o + 14) (float samples);
          put32 bytes (o + 15) attenuation.constant;
          put32 bytes (o + 17) attenuation.quadratic)
    lights;
  bytes

let uniforms ?world ~camera ~viewport scene (drawing : Scene3.Walk.drawing) =
  let bytes = uniforms_blinn_phong ~camera ~viewport scene drawing in
  Option.iter (fun world -> world_uniforms bytes world drawing.material) world;
  bytes

(* Each instance's world and normal matrices depend only on its batch, not
   the camera: kept for the last few batches (by identity) so an orbit only
   redoes the projection. *)
let instance_frames parent transforms =
  let cache = Domain.DLS.get current_instance_cache in
  match
    Option.bind cache (fun context ->
        Instance_frames.peek context.instance_frames (parent, transforms))
  with
  | Some frames -> frames
  | None ->
      let worlds = Array.map (fun transform -> Rays_math.Mat4.mul parent transform) transforms in
      let normals =
        Array.map
          (fun world ->
            match Rays_math.Mat4.inverse world with
            | None -> Rays_math.Mat4.identity
            | Some inverse -> Rays_math.Mat4.transpose inverse)
          worlds
      in
      Option.iter
        (fun context ->
          Instance_frames.add context.instance_frames (parent, transforms) (worlds, normals))
        cache;
      (worlds, normals)

let instance_uniforms ?world ~camera ~viewport scene drawing all ~first ~count =
  let transforms = Array.sub all first count in
  let parent = drawing.Scene3.Walk.transform in
  let drawing =
    { drawing with Scene3.Walk.transform = Rays_math.Mat4.mul drawing.transform transforms.(0) }
  in
  let base = uniforms ?world ~camera ~viewport scene drawing in
  if count = 1 then base
  else
    let bytes = Bytes.make (5456 + (count * 192)) '\000' in
    Bytes.blit base 0 bytes 0 5456;
    (* Word 83 marks the packed matrix table that starts after the shared
     Scene3 material and light block. Each instance owns three 4x4 matrices. *)
    put32 bytes 83 1.;
    let projection = native_clip_projection (Camera.view_projection_matrix ~viewport camera) in
    let worlds, normals = instance_frames parent all in
    for i = 0 to count - 1 do
      let world = worlds.(first + i) and offset = 1364 + (i * 48) in
      matrix bytes offset (Rays_math.Mat4.mul projection world);
      matrix bytes (offset + 16) world;
      matrix bytes (offset + 32) normals.(first + i)
    done;
    bytes

type packed_mesh = {
  mesh : Mesh.t;
  source : Mesh.View.packed_view;
  mode : Scene3.render_mode;
  primitive : Ogpu.Render_pass.primitive;
  key : string;
  vertices : bytes;
  indices : bytes;
  vertex_attributes : string * bytes;
  vertex_count : int;
  index_count : int;
  opaque : bool; (* no vertex color is translucent *)
}

let packed_mesh_capacity = 16
let retained_payload_byte_capacity = 256 * 1024 * 1024

module Packed_meshes = Lru.Identity_list

let make_packed_meshes () =
  Packed_meshes.create
    ~equal:(fun (mesh_a, mode_a) (mesh_b, mode_b) -> mesh_a == mesh_b && mode_a = mode_b)
    packed_mesh_capacity

type packed_cache_context = {
  packed_meshes : (Mesh.t * Scene3.render_mode, packed_mesh) Packed_meshes.t;
}

let current_packed_cache = Domain.DLS.new_key (fun () -> None)
let create_packed_cache_context () = { packed_meshes = make_packed_meshes () }

type prepared_cache_entry = {
  scene : Scene3.t;
  camera : Camera.t;
  viewport : int * int * int * int;
  prepared : prepared;
}

let prepared_cache_capacity = 16

module Prepared_cache = Lru.Identity_list

let make_prepared_cache () =
  Prepared_cache.create
    ~equal:(fun (scene_a, camera_a, viewport_a) (scene_b, camera_b, viewport_b) ->
      scene_a == scene_b && camera_a = camera_b && viewport_a = viewport_b)
    prepared_cache_capacity

type prepared_cache_context = {
  prepared_cache :
    (Scene3.t * Camera.t * (int * int * int * int), prepared_cache_entry) Prepared_cache.t;
}

let current_prepared_cache = Domain.DLS.new_key (fun () -> None)
let create_prepared_cache_context () = { prepared_cache = make_prepared_cache () }

type cache_context = {
  world : world_cache_context;
  sun_fits : (caster list * Rays_math.Vec3.t, (Rays_math.Mat4.t * string) option) Sun_fits.t;
  instances : instance_cache_context;
  packed : packed_cache_context;
  prepared : prepared_cache_context;
}

let create_cache_context () =
  {
    world = create_world_cache_context ();
    sun_fits = make_sun_fits_cache ();
    instances = create_instance_cache_context ();
    packed = create_packed_cache_context ();
    prepared = create_prepared_cache_context ();
  }

let with_cache_context context f =
  let previous_world = Domain.DLS.get current_world_cache
  and previous_sun = Domain.DLS.get current_sun_fits
  and previous_instances = Domain.DLS.get current_instance_cache
  and previous_packed = Domain.DLS.get current_packed_cache
  and previous_prepared = Domain.DLS.get current_prepared_cache in
  Domain.DLS.set current_world_cache (Some context.world);
  Domain.DLS.set current_sun_fits (Some context.sun_fits);
  Domain.DLS.set current_instance_cache (Some context.instances);
  Domain.DLS.set current_packed_cache (Some context.packed);
  Domain.DLS.set current_prepared_cache (Some context.prepared);
  Fun.protect
    ~finally:(fun () ->
      Domain.DLS.set current_world_cache previous_world;
      Domain.DLS.set current_sun_fits previous_sun;
      Domain.DLS.set current_instance_cache previous_instances;
      Domain.DLS.set current_packed_cache previous_packed;
      Domain.DLS.set current_prepared_cache previous_prepared)
    f

let packed_bytes packed =
  Bytes.length packed.vertices + Bytes.length packed.indices
  + Bytes.length (snd packed.vertex_attributes)

let prepared_bytes prepared =
  Array.fold_left
    (fun total entry ->
      total
      + Bytes.length entry.Renderer.Scene_execution.draw.mesh.vertices
      + Bytes.length entry.draw.mesh.indices
      + Option.fold ~none:0 ~some:(fun (_, bytes) -> Bytes.length bytes) entry.vertex_attributes
      + Option.fold ~none:0 ~some:Bytes.length entry.draw.state.transform_uniforms)
    0 prepared.Renderer.Scene_execution.entries

let trim_prepared_cache
    (prepared_cache :
      (Scene3.t * Camera.t * (int * int * int * int), prepared_cache_entry) Prepared_cache.t) =
  let count = ref 0 and bytes = ref 0 in
  Prepared_cache.filter prepared_cache (fun _ (cached : prepared_cache_entry) ->
      let size = prepared_bytes cached.prepared in
      if !count < prepared_cache_capacity && size <= retained_payload_byte_capacity - !bytes then begin
        incr count;
        bytes := !bytes + size;
        true
      end
      else false)

let pack_vertices ~allow_missing_normals (view : Mesh.View.packed_view) =
  let count = Array.length view.vertices.x in
  let normals =
    match view.normals with
    | Some values when Array.length values.x = count -> Some (Some values)
    | None when allow_missing_normals -> Some None
    | _ -> None
  in
  match normals with
  | None -> Error Invalid_mesh
  | Some normals ->
      let bytes = Bytes.make (count * 24) '\000' and finite = ref true in
      let put at value =
        let bits = Int32.bits_of_float value in
        if not (Float.is_finite (Int32.float_of_bits bits)) then finite := false;
        Bytes.set_int32_le bytes at bits
      in
      for index = 0 to count - 1 do
        let offset = index * 24 in
        put offset view.vertices.x.(index);
        put (offset + 4) view.vertices.y.(index);
        put (offset + 8) view.vertices.z.(index);
        match normals with
        | None -> put (offset + 20) 1.
        | Some normals ->
            put (offset + 12) normals.x.(index);
            put (offset + 16) normals.y.(index);
            put (offset + 20) normals.z.(index)
      done;
      if !finite then Ok bytes else Error Invalid_mesh

let pack_attributes (view : Mesh.View.packed_view) =
  let count = Array.length view.vertices.x in
  let bytes = Bytes.make (count * 12) '\000' and finite = ref true in
  for index = 0 to count - 1 do
    Bytes.set_int32_le bytes (index * 12)
      (match view.colors with
      | Some colors -> packed_color colors.(index)
      | None -> Int32.minus_one);
    match view.tex_coords with
    | None -> ()
    | Some values ->
        let uv = values.(index) in
        let put at value =
          let bits = Int32.bits_of_float value in
          if not (Float.is_finite (Int32.float_of_bits bits)) then finite := false;
          Bytes.set_int32_le bytes at bits
        in
        put ((index * 12) + 4) uv.x;
        put ((index * 12) + 8) uv.y
  done;
  if !finite then Ok bytes else Error Invalid_mesh

let pack_indices values =
  let bytes = Bytes.make (Array.length values * 4) '\000' in
  Array.iteri (fun i value -> Bytes.set_int32_le bytes (i * 4) (Int32.of_int value)) values;
  bytes

let cull = function
  | Scene3.Cull_none -> Ogpu.Render_pass.Cull_none
  | Cull_back -> Cull_back
  | Cull_front -> Cull_front

let triangle_indices mode source =
  match mode with
  | Mesh.Triangles when Array.length source mod 3 = 0 -> Some source
  | Triangle_strip when Array.length source >= 3 ->
      Some
        (Array.init
           ((Array.length source - 2) * 3)
           (fun index ->
             let triangle = index / 3 and corner = index mod 3 in
             if triangle land 1 = 0 then source.(triangle + corner)
             else source.(triangle + match corner with 0 -> 1 | 1 -> 0 | _ -> 2)))
  | Triangle_fan when Array.length source >= 3 ->
      Some
        (Array.init
           ((Array.length source - 2) * 3)
           (fun index ->
             let triangle = index / 3 in
             match index mod 3 with
             | 0 -> source.(0)
             | 1 -> source.(triangle + 1)
             | _ -> source.(triangle + 2)))
  | _ -> None

let line_indices mode source =
  match mode with
  | Mesh.Lines when Array.length source mod 2 = 0 -> Some source
  | Line_strip when Array.length source >= 2 ->
      Some (Array.init ((Array.length source - 1) * 2) (fun i -> source.((i / 2) + (i mod 2))))
  | Line_loop when Array.length source >= 2 ->
      Some
        (Array.init
           (Array.length source * 2)
           (fun i -> source.(((i / 2) + (i mod 2)) mod Array.length source)))
  | _ -> None

let unique_triangle_edges triangles =
  let seen = Hashtbl.create (Array.length triangles) and edges = ref [] in
  for i = 0 to (Array.length triangles / 3) - 1 do
    let a = triangles.(i * 3) and b = triangles.((i * 3) + 1) and c = triangles.((i * 3) + 2) in
    List.iter
      (fun (a, b) ->
        let key = if a < b then (a, b) else (b, a) in
        if not (Hashtbl.mem seen key) then (
          Hashtbl.add seen key ();
          edges := b :: a :: !edges))
      [ (a, b); (b, c); (c, a) ]
  done;
  Array.of_list (List.rev !edges)

let same_planes (a : Mesh.View.vec3_view) (b : Mesh.View.vec3_view) =
  a.x == b.x && a.y == b.y && a.z == b.z

let same_normals a b =
  match (a, b) with None, None -> true | Some a, Some b -> same_planes a b | _ -> false

let same_geometry (a : Mesh.View.packed_view) (b : Mesh.View.packed_view) =
  a.mode = b.mode && same_planes a.vertices b.vertices && same_normals a.normals b.normals
  && a.indices == b.indices

let same_attributes a b =
  match (a, b) with None, None -> true | Some a, Some b -> a == b | _ -> false

let find_packed predicate =
  let found = ref None in
  Option.iter
    (fun context ->
      Packed_meshes.iter context.packed_meshes (fun _ packed ->
          if predicate packed then found := Some packed))
    (Domain.DLS.get current_packed_cache);
  !found

let packed_of_mesh mode mesh =
  let source = Mesh.View.packed_view mesh in
  let cache = Domain.DLS.get current_packed_cache in
  let exact =
    match
      Option.bind cache (fun context -> Packed_meshes.peek context.packed_meshes (mesh, mode))
    with
    | Some packed
      when same_geometry packed.source source
           && same_attributes packed.source.colors source.colors
           && same_attributes packed.source.tex_coords source.tex_coords ->
        Some packed
    | _ -> None
  in
  let matching =
    match exact with
    | Some _ -> exact
    | None ->
        find_packed (fun packed ->
            packed.mode = mode && packed.mesh != mesh
            && same_geometry packed.source source
            && same_attributes packed.source.colors source.colors
            && same_attributes packed.source.tex_coords source.tex_coords)
  in
  match matching with
  | Some packed -> Ok packed
  | None -> (
      let reused =
        find_packed (fun packed -> packed.mode = mode && same_geometry packed.source source)
      in
      let view = source in
      (* Lit faces need normals; derive them once here (the pack is cached by
         mesh identity) rather than rejecting procedural meshes without N. *)
      let view =
        match (mode, view.mode, view.normals) with
        | Faces, (Mesh.Triangles | Triangle_strip | Triangle_fan), None -> (
            match reused with
            | Some _ -> view
            | None -> Mesh.View.packed_view (Mesh.recalculate_normals mesh))
        | _ -> view
      in
      let native =
        match mode with
        | Scene3.Vertices ->
            Some (Ogpu.Render_pass.Point_list, Array.init (Array.length view.vertices.x) Fun.id)
        | Faces | Wireframe -> (
            match view.mode with
            | Mesh.Points -> Some (Point_list, view.indices)
            | Lines | Line_strip | Line_loop ->
                Option.map
                  (fun indices -> (Ogpu.Render_pass.Line_list, indices))
                  (line_indices view.mode view.indices)
            | Triangles | Triangle_strip | Triangle_fan ->
                Option.map
                  (fun indices ->
                    if mode = Wireframe then
                      (Ogpu.Render_pass.Line_list, unique_triangle_edges indices)
                    else (Triangle_list, indices))
                  (triangle_indices view.mode view.indices))
      in
      let allow_missing_normals =
        match native with Some (Triangle_list, _) -> false | _ -> true
      in
      let vertices =
        match reused with
        | Some packed -> Ok packed.vertices
        | None -> pack_vertices ~allow_missing_normals view
      in
      match (native, vertices, pack_attributes view) with
      | Some (primitive, native_indices), Ok vertices, Ok attributes
        when Array.length native_indices > 0 ->
          let id = fresh_resource_id () in
          let key, indices =
            match reused with
            | Some packed -> (packed.key, packed.indices)
            | None -> (Printf.sprintf "scene3:%d" id, pack_indices native_indices)
          in
          let packed =
            {
              mesh;
              source;
              mode;
              primitive;
              key;
              vertices;
              indices;
              vertex_attributes = (Printf.sprintf "scene3:attributes:%d" id, attributes);
              vertex_count = Array.length view.vertices.x;
              index_count = Array.length native_indices;
              opaque =
                Option.fold ~none:true
                  ~some:(Array.for_all (fun (c : Rays_math.Color.t) -> c.a = 255))
                  view.colors;
            }
          in
          Option.iter
            (fun context -> Packed_meshes.add context.packed_meshes (mesh, mode) packed)
            cache;
          Ok packed
      | _ -> Error Invalid_mesh)

let prepare ~resources ~camera ~viewport:((x, y, width, height) as viewport) scene =
  if width <= 0 || height <= 0 then Error Invalid_viewport
  else
    let cache = Domain.DLS.get current_prepared_cache in
    match
      Option.bind cache (fun context ->
          Prepared_cache.peek context.prepared_cache (scene, camera, viewport))
    with
    | Some prepared -> Ok prepared.prepared
    | None -> (
        if List.compare_length_with (Scene3.Walk.lights scene) max_lights > 0 then
          Error Too_many_lights
        else
          let world =
            Option.map
              (fun (baked : World.baked) -> world_of baked (Option.bind baked.sun (sun_fit scene)))
              (Scene3.Walk.world scene)
          in
          let cacheable = ref true and failure = ref None and entries = ref [] in
          (match world with
          | Some w when w.baked.background <> World.Transparent ->
              Option.iter
                (fun uniforms ->
                  let state : Renderer.Scene_execution.state =
                    {
                      viewport;
                      scissor = viewport;
                      cull = Cull_none;
                      depth_compare = Always;
                      depth_write = false;
                      depth_load = Clear;
                      depth_clear = Scene3.Walk.depth_clear scene;
                      transform_uniforms = Some uniforms;
                    }
                  in
                  entries :=
                    [
                      {
                        Renderer.Scene_execution.family = Scene3_world;
                        blend = Ogpu.Pipeline.Replace;
                        texture = Some w.camera_map;
                        auxiliary = Some w.auxiliary;
                        vertex_attributes = Some background_attributes;
                        samples = Scene3.Walk.samples scene;
                        draw = { mesh = background_mesh; state };
                      };
                    ])
                (background_uniforms ~camera ~viewport)
          | _ -> ());
          Scene3.Walk.iter_batches
            (fun (drawing : Scene3.Walk.drawing) transforms ->
              if Option.is_some drawing.texture then cacheable := false;
              if
                !failure = None
                && match transforms with Some values -> Array.length values > 0 | None -> true
              then
                match packed_of_mesh drawing.mode drawing.mesh with
                | Ok packed -> (
                    let texture =
                      match drawing.texture with
                      | None -> Ok None
                      | Some value -> Result.map Option.some (resources.texture value)
                    in
                    match texture with
                    | Error error -> failure := Some error
                    | Ok texture -> (
                        let mesh : Renderer.Scene_execution.mesh =
                          {
                            key = Host packed.key;
                            vertices = packed.vertices;
                            vertex_count = packed.vertex_count;
                            indices = packed.indices;
                            index_count = packed.index_count;
                            primitive = packed.primitive;
                          }
                        in
                        let family =
                          match (packed.primitive, texture) with
                          | Ogpu.Render_pass.Point_list, None ->
                              Renderer.Scene_execution.Scene3_points
                          | _, Some _ -> Scene3_textured
                          | _, None -> Scene3
                        in
                        (* ponytail: point clouds keep the Blinn-Phong points pipeline under a
             World (no IBL, no tone map). *)
                        let world =
                          match world with
                          | Some _ when packed.primitive = Point_list && texture = None -> None
                          | w -> w
                        in
                        let family, texture, auxiliary =
                          match world with
                          | Some w ->
                              ( Renderer.Scene_execution.Scene3_world,
                                Some (Option.value texture ~default:world_white),
                                Some w.auxiliary )
                          | None -> (family, texture, None)
                        in
                        let add transform_uniforms =
                          let state : Renderer.Scene_execution.state =
                            {
                              viewport = (x, y, width, height);
                              scissor = (x, y, width, height);
                              cull = cull drawing.cull;
                              depth_compare = comparison drawing.depth.comparison;
                              depth_write = drawing.depth.write;
                              depth_load = Ogpu.Render_pass.Clear;
                              depth_clear = Scene3.Walk.depth_clear scene;
                              transform_uniforms = Some transform_uniforms;
                            }
                          in
                          (* The default blend is Alpha.  A draw whose every fragment has alpha 1 (opaque
               material and vertex colors, no texture) writes the same pixels with Replace, and
               only an unblended pipeline lets a tile-based GPU drop hidden fragments before
               shading them: a dense mesh otherwise shades every layer the depth test lets by. *)
                          let blend =
                            match drawing.blend with
                            | Alpha
                              when drawing.texture = None && drawing.material.diffuse.a = 255
                                   && packed.opaque ->
                                Ogpu.Pipeline.Replace
                            | other -> blend other
                          in
                          entries :=
                            {
                              Renderer.Scene_execution.family;
                              blend;
                              texture;
                              auxiliary;
                              vertex_attributes = Some packed.vertex_attributes;
                              samples = Scene3.Walk.samples scene;
                              draw = { mesh; state };
                            }
                            :: !entries
                        in
                        match transforms with
                        | None -> add (uniforms ?world ~camera ~viewport scene drawing)
                        | Some transforms ->
                            (* ponytail: 8192 instances per draw caps the temporary upload
                 near 1.6 MB; larger scenes become consecutive draws. *)
                            let batch = 8192 in
                            for start = 0 to (Array.length transforms - 1) / batch do
                              let first = start * batch in
                              let count = min batch (Array.length transforms - first) in
                              add
                                (instance_uniforms ?world ~camera ~viewport scene drawing transforms
                                   ~first ~count)
                            done))
                (* An empty mesh (a cook that produced no triangles) draws nothing:
       skip it instead of failing the whole view. *)
                | Error _
                  when Mesh.vertex_count drawing.mesh = 0
                       || Mesh.index_count drawing.mesh < 3
                          && Mesh.mode drawing.mesh <> Mesh.Points ->
                    ()
                | _ -> failure := Some Invalid_mesh)
            scene;
          match !failure with
          | Some error -> Error error
          | None -> (
              match
                Renderer.Scene_execution.prepare_scene3 ~clear:(0., 0., 0., 0.)
                  ~clear_depth:(Scene3.Walk.depth_clear scene)
                  (Array.of_list (List.rev !entries))
              with
              | Ok prepared ->
                  if !cacheable then begin
                    Option.iter
                      (fun context ->
                        Prepared_cache.add context.prepared_cache (scene, camera, viewport)
                          { scene; camera; viewport; prepared };
                        trim_prepared_cache context.prepared_cache)
                      cache
                  end;
                  Ok prepared
              | Error _ -> Error Invalid_mesh))
