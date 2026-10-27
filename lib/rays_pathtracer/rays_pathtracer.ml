module Linear_color = Rays_math.Color.Linear
open Mesh_data
open Mesh_build
open Frame_data

type material = {
  albedo : Linear_color.t;
  roughness : float;
  metallic : float;
  emission : Linear_color.t;
  round : float;
}

let material ?(roughness = 0.5) ?(metallic = 0.)
    ?(emission = Linear_color.rgb 0. 0. 0.) ?(round = 0.) albedo =
  { albedo; roughness; metallic; emission; round }

type panel = {
  direction : Rays_math.Vec3.t;
  width : float;
  height : float;
  softness : float;
  color : Linear_color.t;
  intensity : float;
}

let panel ?(softness = 0.05) ~intensity ~width ~height direction =
  { direction; width; height; softness; color = Linear_color.rgb 1. 1. 1.; intensity }

type environment = { sky : Linear_color.t; ground : Linear_color.t; panels : panel list }
type camera = Rays.Camera.t

type light = {
  at : Rays_math.Vec3.t;
  target : Rays_math.Vec3.t;
  size : float * float;
  color : Linear_color.t;
  intensity : float;
}

let rect_light ~intensity ~size ~target at =
  { at; target; size; color = Linear_color.rgb 1. 1. 1.; intensity }

(* Inverse of the raster World conversion (a rect of radiance L and area A is
   a Light.t of color L A / pi), so the irradiance pi I / d^2 matches. *)
let light_of (light : Rays.Light.t) =
  let c = light.diffuse in
  let color = Linear_color.rgb (float c.r /. 255.) (float c.g /. 255.) (float c.b /. 255.) in
  let rect ~at ~target ?(distance2 = 1.) (w, h) =
    { at; target; size = (w, h); color; intensity = Float.pi *. light.intensity *. distance2 /. (w *. h) } in
  let open Rays_math.Vec3 in
  match light.kind with
  | Area { position; direction; width; height; _ } -> rect ~at:position ~target:(add position direction) (width, height)
  | Point { position; _ } -> rect ~at:position ~target:(sub position unit_y) (0.1, 0.1)
  | Spot { position; direction; _ } -> rect ~at:position ~target:(add position direction) (0.1, 0.1)
  | Directional { direction } ->
      rect ~at:(scale (normalize direction) (-100.)) ~target:zero ~distance2:1e4 (20., 20.)

type sphere = { center : Rays_math.Vec3.t; radius : float; sphere_material : material }

type strand = { points : Rays_math.Vec3.t array; thickness : float; strand_material : material }

type scene = {
  objects : (Rays.Mesh.t * material) list;
  spheres : sphere list;
  strands : strand list;
  environment : environment;
  lights : light list;
}

let source = Pathtrace_source.source

module B = Ogpu.Backend

(* One in-flight frame: its receipt, which output texture it writes, and
   whether a camera change made its result obsolete. *)
type pending = {
  receipt : B.receipt;
  slot : int;
  mutable stale : bool;
  preview : bool;
  camera : camera;
  history_epoch : int;
  last : bool;  (* the last band of its frame: publishes the film *)
}

(* Instances carry a user id selecting their material; with [motion] every
   instance has two transform keyframes (start and end of the shutter). *)
type instances = Mesh_data.instances
type part = Mesh_data.part = Triangle_part of int * int | Sphere_part | Strand_part
type layout = Mesh_data.layout
type extras = Mesh_data.extras
type mesh = material Mesh_data.t

(* The shape of the pipeline a mesh needs. *)
type queued = Full of mesh | Move of mesh

type t = {
  lease : Renderer.gpu;
  device : B.device;
  queue : B.queue;
  library : B.library;
  pipelines : (shape, B.pipeline * B.function_table option) Hashtbl.t;
  resolve_pipeline : B.pipeline;
  mutable gpu : gpu_mesh;
  mutable building : mesh_build option;
  mutable queued_mesh : queued option;
  mutable latest : mesh; (* the newest mesh given, for [move] *)
  panels : B.buffer;
  mutable lights : B.buffer;
  mutable light_count : int;
  mutable scene_lights : light list;
  mutable world : Rays.World.baked option;
  mutable world_buffer : B.buffer;
  bsdf_only : bool;
  mutable accum : B.buffer;
  mutable outputs : B.texture array;
  shared_film : bool;
  mutable next_output_slot : int;
  mutable history_color : B.buffer array;
  mutable history_geometry : B.buffer array;
  mutable history_slot : int;
  mutable history_camera : camera option;
  mutable history_epoch : int;
  mutable pending : pending list;  (* oldest first, at most two in flight *)
  mutable retired : (unit -> unit) list;  (* freed once no frame is in flight *)
  mutable bands : int;  (* row bands per accumulation frame *)
  mutable frame_gpu : float;  (* GPU seconds of the frame in progress *)
  mutable completed : int;
  resource : Runtime_resources.Image.t;
  image : Rays.Image.t;
  mutable width : int;
  mutable height : int;
  spp : int;
  bounces : int;
  exposure : float;
  round_samples : int;
  profile : bool;
  panel_count : int;
  sky : Linear_color.t;
  ground : Linear_color.t;
  mutable frame : int;
  mutable camera : camera option;
  mutable moving : bool;
  mutable scene_edited : bool;

}

let ( let* ) = Result.bind
let gpu result = Result.map_error Ogpu.Error.to_string result
let put_f32 bytes offset value = Bytes.set_int32_le bytes offset (Int32.bits_of_float value)
let put_u32 bytes offset value = Bytes.set_int32_le bytes offset (Int32.of_int value)
let add_f32 builder value = Stdlib.Buffer.add_int32_le builder (Int32.bits_of_float value)

let add_vec3 builder (x, y, z) =
  add_f32 builder x;
  add_f32 builder y;
  add_f32 builder z

let add_float4 builder (x, y, z) w =
  add_vec3 builder (x, y, z);
  add_f32 builder w

let color_tuple (color : Linear_color.t) = color.r, color.g, color.b

let vec3_tuple (v : Rays_math.Vec3.t) = (v.x, v.y, v.z)

let frame_facts (t : t) = Frame_data.{
  width = t.width; height = t.height; sky = t.sky; ground = t.ground;
  frame = t.frame; spp = t.spp; world = t.world; panel_count = t.panel_count;
  bounces = t.bounces; exposure = t.exposure; round_samples = t.round_samples;
  light_count = t.light_count; moving = t.moving; history_camera = t.history_camera;
  bsdf_only = t.bsdf_only;
}

(* Analytic primitives share the material index space with the objects. *)
let prepare_extras ~first_material spheres strands =
  let sphere_bytes = Stdlib.Buffer.create 256
  and box_bytes = Stdlib.Buffer.create 256
  and strand_points = Stdlib.Buffer.create 256
  and strand_radii = Stdlib.Buffer.create 64
  and strand_indices = Stdlib.Buffer.create 64
  and strand_ids = Stdlib.Buffer.create 64 in
  let finite value = Float.is_finite value && Float.abs value <= 3.402823466e38 in
  let* () =
    List.fold_left
      (fun acc (index, (sphere : sphere)) ->
        let* () = acc in
        let x, y, z = vec3_tuple sphere.center in
        if not (finite x && finite y && finite z && finite sphere.radius && sphere.radius > 0.) then
          Error "path tracer sphere needs a finite center and a positive radius"
        else begin
          add_float4 sphere_bytes (x, y, z) sphere.radius;
          Stdlib.Buffer.add_int32_le sphere_bytes (Int32.of_int (first_material + index));
          Stdlib.Buffer.add_bytes sphere_bytes (Bytes.make 12 '\000');
          add_vec3 box_bytes (x -. sphere.radius, y -. sphere.radius, z -. sphere.radius);
          add_vec3 box_bytes (x +. sphere.radius, y +. sphere.radius, z +. sphere.radius);
          Ok ()
        end)
      (Ok ())
      (List.mapi (fun index sphere -> (index, sphere)) spheres)
  in
  let point_count = ref 0 and segments = ref 0 in
  let* () =
    List.fold_left
      (fun acc (index, (strand : strand)) ->
        let* () = acc in
        if Array.length strand.points < 2 then Error "path tracer strand needs at least two points"
        else if not (finite strand.thickness && strand.thickness > 0.) then
          Error "path tracer strand needs a positive finite thickness"
        else if Array.exists (fun p -> let x, y, z = vec3_tuple p in not (finite x && finite y && finite z)) strand.points
        then Error "path tracer strand points must be finite"
        else begin
          let first = !point_count in
          Array.iter (fun p -> add_vec3 strand_points (vec3_tuple p); add_f32 strand_radii (strand.thickness *. 0.5)) strand.points;
          point_count := !point_count + Array.length strand.points;
          for segment = 0 to Array.length strand.points - 2 do
            Stdlib.Buffer.add_int32_le strand_indices (Int32.of_int (first + segment));
            Stdlib.Buffer.add_int32_le strand_ids (Int32.of_int (first_material + List.length spheres + index));
            incr segments
          done;
          Ok ()
        end)
      (Ok ())
      (List.mapi (fun index strand -> (index, strand)) strands)
  in
  Ok
    {
      sphere_bytes = Stdlib.Buffer.to_bytes sphere_bytes;
      box_bytes = Stdlib.Buffer.to_bytes box_bytes;
      sphere_count = List.length spheres;
      strand_points = Stdlib.Buffer.to_bytes strand_points;
      strand_radii = Stdlib.Buffer.to_bytes strand_radii;
      strand_indices = Stdlib.Buffer.to_bytes strand_indices;
      strand_ids = Stdlib.Buffer.to_bytes strand_ids;
      strand_point_count = !point_count;
      strand_segments = !segments;
    }

let mesh ?(spheres = []) ?(strands = []) (objects : (Rays.Mesh.t * material) list) =
  let* extras = prepare_extras ~first_material:(List.length objects) spheres strands in
  let materials = List.map snd objects
    @ List.map (fun sphere -> sphere.sphere_material) spheres
    @ List.map (fun strand -> strand.strand_material) strands in
  Mesh_data.mesh ~extras ~materials (List.map fst objects)

let mesh_instanced = Mesh_data.mesh_instanced
let scene_mesh = Mesh_data.scene_mesh
let moved_mesh = Mesh_data.moved_mesh

let material_bytes materials =
  let builder = Stdlib.Buffer.create 256 in
  List.iter
    (fun m ->
      add_float4 builder (color_tuple m.albedo) m.roughness;
      add_float4 builder (color_tuple m.emission) m.metallic;
      add_float4 builder (m.round, 0., 0.) 0.)
    materials;
  Stdlib.Buffer.to_bytes builder

let upload_mesh_async ?reuse device queue mesh =
  Mesh_build.upload_mesh_async ?reuse ~material_data:(material_bytes mesh.mesh_materials)
    device queue mesh

let upload_mesh device queue mesh =
  let* build = upload_mesh_async device queue mesh in
  finish_build device queue build

let panel_bytes panels =
  let builder = Stdlib.Buffer.create 256 in
  let normalize (v : Rays_math.Vec3.t) =
    let l = sqrt ((v.x *. v.x) +. (v.y *. v.y) +. (v.z *. v.z)) in
    let l = if l = 0. then 1. else l in
    (v.x /. l, v.y /. l, v.z /. l)
  in
  List.iter
    (fun (p : panel) ->
      let r, g, b = color_tuple p.color in
      add_float4 builder (normalize p.direction) 0.;
      add_float4 builder (r *. p.intensity, g *. p.intensity, b *. p.intensity) 0.;
      add_float4 builder (p.width, p.height, p.softness) 0.)
    panels;
  if panels = [] then Stdlib.Buffer.add_bytes builder (Bytes.make 48 '\000');
  Stdlib.Buffer.to_bytes builder

let light_bytes ?(world_lights = []) lights =
  let builder = Stdlib.Buffer.create 256 in
  let open Rays_math.Vec3 in
  let sub a b = create (a.x -. b.x) (a.y -. b.y) (a.z -. b.z) in
  let cross a b =
    create
      ((a.y *. b.z) -. (a.z *. b.y))
      ((a.z *. b.x) -. (a.x *. b.z))
      ((a.x *. b.y) -. (a.y *. b.x))
  in
  let normalize v =
    let l = sqrt ((v.x *. v.x) +. (v.y *. v.y) +. (v.z *. v.z)) in
    if l = 0. then create 0. 0. 1. else create (v.x /. l) (v.y /. l) (v.z /. l)
  in
  List.iter
    (fun (light : light) ->
      let w, h = light.size in
      let forward = normalize (sub light.target light.at) in
      let world_up = if Float.abs forward.y > 0.999 then create 0. 0. 1. else create 0. 1. 0. in
      let right = normalize (cross forward world_up) in
      let up = cross right forward in
      let u = create (right.x *. w) (right.y *. w) (right.z *. w)
      and v = create (up.x *. h) (up.y *. h) (up.z *. h) in
      let origin =
        create
          (light.at.x -. ((u.x +. v.x) /. 2.))
          (light.at.y -. ((u.y +. v.y) /. 2.))
          (light.at.z -. ((u.z +. v.z) /. 2.))
      in
      let r, g, b = color_tuple light.color in
      add_float4 builder (origin.x, origin.y, origin.z) 0.;
      add_float4 builder (u.x, u.y, u.z) 0.;
      add_float4 builder (v.x, v.y, v.z) 0.;
      add_float4 builder (r *. light.intensity, g *. light.intensity, b *. light.intensity) (w *. h))
    lights;
  (* World lights are one-sided (origin.w = 1): the edges are ordered so
     cross(u, v) is the emitting normal. *)
  List.iter
    (fun (light : Rays.World.light) ->
      let u = scale light.u 2. and v = scale light.v 2. in
      let n = cross u v in
      let u, v = if dot n light.normal < 0. then (v, u) else (u, v) in
      let origin = sub (sub light.position light.u) light.v in
      add_float4 builder (origin.x, origin.y, origin.z) 1.;
      add_float4 builder (u.x, u.y, u.z) 0.;
      add_float4 builder (v.x, v.y, v.z) 0.;
      add_float4 builder (light.radiance.r, light.radiance.g, light.radiance.b) (length n))
    world_lights;
  if lights = [] && world_lights = [] then Stdlib.Buffer.add_bytes builder (Bytes.make 64 '\000');
  Stdlib.Buffer.to_bytes builder

(* The baked World as one float32 buffer: camera map, lighting map, CDF
   marginal, CDF conditional rows (see pathtrace.metal). *)
let world_bytes (baked : Rays.World.baked) =
  let { Rays.World.width; height; _ } = baked.camera in
  let cdf = baked.cdf in
  if width < 1 || height < 1
     || baked.lighting.width <> width || baked.lighting.height <> height
     || Float.Array.length baked.camera.pixels <> width * height * 3
     || Float.Array.length baked.lighting.pixels <> width * height * 3
     || Float.Array.length cdf.marginal <> height + 1
     || Float.Array.length cdf.conditional <> height * (width + 1)
  then Error "path tracer World maps and CDF do not match in size"
  else
    let parts = [ baked.camera.pixels; baked.lighting.pixels; cdf.marginal; cdf.conditional ] in
    let bytes = Bytes.create (4 * List.fold_left (fun n a -> n + Float.Array.length a) 0 parts) in
    ignore
      (List.fold_left
         (fun offset array ->
           Float.Array.iteri (fun i value -> put_f32 bytes (offset + (4 * i)) value) array;
           offset + (4 * Float.Array.length array))
         0 parts);
    Ok bytes

(* Resource helpers on the leased OGPU device. Scene geometry lives in shared
   memory (Apple silicon is unified); accumulation, history, and build scratch
   are device-local. *)
let storage_buffer binding = { Ogpu.Shader.group = 0; binding; kind = Storage_buffer; visibility = [ Compute ] }

let trace_interface shape =
  [ { Ogpu.Shader.group = 0; binding = 0; kind = Acceleration_structure; visibility = [ Compute ] }
  ; { Ogpu.Shader.group = 0; binding = 1; kind = Uniform_buffer; visibility = [ Compute ] } ]
  @ List.map storage_buffer
      ([ 2; 3 ] @ (if shape.scene then [ 4 ] else [])
       @ (if shape.instanced then [ 5; 6; 7; 8; 9; 10 ] else [ 4; 5; 6; 7; 8; 9 ]) @ [ 11; 12; 13; 14 ]
       @ (if shape.spheres then [ 15 ] else []) @ (if shape.curves then [ 17; 18; 19 ] else [])
       @ if shape.scene then [ 20 ] else [])
  @ (if shape.spheres then [ { Ogpu.Shader.group = 0; binding = 16; kind = Intersection_table; visibility = [ Compute ] } ] else [])
  @ [ { Ogpu.Shader.group = 0; binding = 0; kind = Storage_texture; visibility = [ Compute ] } ]

let resolve_interface =
  [ { Ogpu.Shader.group = 0; binding = 0; kind = Ogpu.Shader.Uniform_buffer; visibility = [ Compute ] }
  ; storage_buffer 1; storage_buffer 2
  ; { Ogpu.Shader.group = 0; binding = 0; kind = Storage_texture; visibility = [ Compute ] } ]

(* The sphere intersection function whose tags match the pipeline's intersector. *)
let sphere_function shape =
  "sphere_hit_" ^ (if shape.instanced then "i" else "f") ^ (if shape.motion then "m" else "")
  ^ if shape.curves then "c" else ""

(* Pipelines are specialized per mesh shape and compiled on first use. *)
let pipeline_for library pipelines shape =
  match Hashtbl.find_opt pipelines shape with
  | Some entry -> Ok entry
  | None ->
      let constants =
        [ ("INSTANCED", Ogpu.Shader.Bool shape.instanced); ("MOTION", Ogpu.Shader.Bool shape.motion)
        ; ("SPHERES", Ogpu.Shader.Bool shape.spheres); ("CURVES", Ogpu.Shader.Bool shape.curves)
        ; ("SCENE", Ogpu.Shader.Bool shape.scene) ]
      in
      let* pipeline =
        gpu
          (B.create_compute_pipeline_from library ~entry:"pathtrace" ~constants
             ~interface:(trace_interface shape)
             ~linked:(if shape.spheres then [ sphere_function shape ] else []) ())
      in
      let* table =
        if not shape.spheres then Ok None
        else
          let created =
            let* table = gpu (B.create_intersection_table pipeline ~capacity:1) in
            match gpu (B.table_set_function table ~index:0 (sphere_function shape)) with
            | Ok () -> Ok (Some table)
            | Error _ as failure -> ignore (B.destroy_table table); failure
          in
          match created with
          | Ok _ as ok -> ok
          | Error _ as failure -> ignore (B.destroy_pipeline pipeline); failure
      in
      Hashtbl.add pipelines shape (pipeline, table);
      Ok (pipeline, table)

(* Size-dependent storage: accumulation, two history frames, three output films. *)
let create ?(spp = 1) ?(bounces = 6) ?(exposure = 1.) ?(round_samples = 4) ?(bsdf_only = false) ?(profile = false) ~width ~height (scene : scene) =
  if width <= 0 || height <= 0 then Error "path tracer size must be positive"
  else if spp <= 0 || bounces <= 0 then Error "path tracer spp and bounces must be positive"
  else
    let* lease =
      Result.map_error
        (Format.asprintf "%a" Renderer.pp_error)
        (Renderer.acquire_gpu ())
    in
    let device = Renderer.gpu_device lease
    and queue = Renderer.gpu_queue lease in
    let release () = Renderer.release_gpu lease in
    let created =
      let* () =
        gpu (Ogpu.Caps.require ~operation:"Rays_pathtracer.create" (B.capabilities device) Ray_tracing)
      in
      let* mesh = mesh ~spheres:scene.spheres ~strands:scene.strands scene.objects in
      let* gpu_mesh = upload_mesh device queue mesh in
      let* panels = shared device ~label:"pathtracer-panels" (panel_bytes scene.environment.panels) in
      let* lights = shared device ~label:"pathtracer-lights" (light_bytes scene.lights) in
      let* world_buffer = shared device ~label:"pathtracer-world" (Bytes.make 16 '\000') in
      let* shader =
        gpu
          (Ogpu.Shader.of_source
             {
               backend = "metal";
               label = Some "rays_pathtracer";
               bytes = Bytes.of_string source;
               entry_points = [ { name = "pathtrace"; stage = Compute }; { name = "resolve_preview"; stage = Compute } ];
               bindings = [];
             })
      in
      let* library = gpu (B.create_library device shader) in
      let pipelines = Hashtbl.create 4 in
      let* _ = pipeline_for library pipelines gpu_mesh.shape in
      let* resolve_pipeline =
        gpu
          (B.create_compute_pipeline_from library ~entry:"resolve_preview"
             ~interface:resolve_interface ())
      in
      let* film = allocate_film device ~width ~height in
      let pixels = Bytes.make (width * height * 4) '\000' in
      let* resource =
        Result.map_error
          (Format.asprintf "%a" Runtime_resources.pp_error)
          (Runtime_resources.Image.create ~width ~height ~rgba:(Bytes.copy pixels))
      in
      let* image = Rays.Image.Private.of_resource resource in
      Ok
        {
          lease;
          device;
          queue;
          library;
          pipelines;
          resolve_pipeline;
          gpu = gpu_mesh;
          building = None;
          queued_mesh = None;
          latest = mesh;
          panels;
          lights;
          light_count = List.length scene.lights;
          scene_lights = scene.lights;
          world = None;
          world_buffer;
          bsdf_only;
          accum = film.accum;
          outputs = film.outputs;
          shared_film = Renderer.gpu_shared lease;
          next_output_slot = 0;
          history_color = film.history_color;
          history_geometry = film.history_geometry;
          history_slot = 0;
          history_camera = None;
          history_epoch = 0;
          pending = [];
          retired = [];
          bands = 3;
          frame_gpu = 0.;
          completed = 0;
          resource;
          image;
          width;
          height;
          spp;
          bounces;
          exposure;
          round_samples;
          profile;
          panel_count = List.length scene.environment.panels;
          sky = scene.environment.sky;
          ground = scene.environment.ground;
          frame = 0;
          camera = None;
          moving = false;
          scene_edited = false;

        }
    in
    (match created with
    | Ok _ -> ()
    | Error _ ->
        (* ponytail: partial creation failures release the lease and let the
           device report leaked children; creation is all-or-nothing in tests. *)
        release ());
    created

let drain t =
  if t.pending = [] then begin
    let retired = t.retired in
    t.retired <- [];
    List.iter (fun free -> free ()) (List.rev retired)
  end

let retire t free = if t.pending = [] then free () else t.retired <- free :: t.retired

let reset_samples t =
  t.frame <- 0;
  t.completed <- 0

let restart t =
  reset_samples t;
  List.iter (fun pending -> pending.stale <- true) t.pending;
  t.history_camera <- None;
  t.history_epoch <- t.history_epoch + 1

let reset t =
  restart t;
  t.scene_edited <- false

(* Geometry, lights, or World just became current: the next [render] is a
   preview frame, like a camera move, so continuous edits stay interactive. *)
let edited t =
  restart t;
  t.scene_edited <- true

(* A reused prototype or a [move] shares the installed geometry and BLASes;
   ownership passes to the new mesh. *)
let install_gpu t gpu =
  if t.gpu.positions == gpu.positions then begin
    t.gpu.owns_prototype <- false;
    gpu.owns_prototype <- true
  end;
  let old = t.gpu in
  (* An intersection table keeps its bound buffer alive: point every table at
     the new geometry before the old sphere buffer is destroyed. *)
  let target = Option.value gpu.sphere_buffer ~default:gpu.positions in
  Hashtbl.iter
    (fun _ (_, table) -> Option.iter (fun table -> ignore (B.table_set_buffer table ~index:0 target)) table)
    t.pipelines;
  retire t (fun () -> destroy_gpu old);
  t.gpu <- gpu;
  edited t

(* The pipeline and intersection table for the installed mesh, with the table
   pointed at the mesh's sphere records. *)
let current_pipeline t =
  let* pipeline, table = pipeline_for t.library t.pipelines t.gpu.shape in
  let* () =
    match (table, t.gpu.sphere_buffer) with
    | Some table, Some spheres -> gpu (B.table_set_buffer table ~index:0 spheres)
    | _ -> Ok ()
  in
  Ok (pipeline, table)

let samples t = t.completed * t.spp
let size t = (t.width, t.height)
let image t = t.image

let band_ms = 6.

(* Publishes a completed frame's pixels unless a reset made them obsolete. *)
let publish t (pending : pending) =
  let seconds = B.gpu_duration t.queue pending.receipt in
  if t.profile then Option.iter (fun seconds ->
      Printf.eprintf "pathtracer gpu_ms %.4f\n%!" (seconds *. 1000.)) seconds;
  if not pending.preview then begin
    t.frame_gpu <- t.frame_gpu +. Option.value seconds ~default:0.;
    if pending.last then begin
      (match seconds with
       | Some _ -> t.bands <- max 1 (min 3 (int_of_float (Float.ceil (t.frame_gpu *. 1000. /. band_ms))))
       | None -> ());
      t.frame_gpu <- 0.
    end
  end;
  if pending.history_epoch = t.history_epoch then t.history_camera <- Some pending.camera;
  if pending.stale || not pending.last then Ok ()
  else
    let film = t.outputs.(pending.slot) in
    let* () =
      if t.shared_film then
        Result.map_error
          (Format.asprintf "%a" Runtime_resources.pp_error)
          (Runtime_resources.Image.Private.replace_gpu t.resource film)
      else
        let* rgba = gpu (B.read_texture film ~bytes_per_row:(t.width * 4)) in
        ();
        Result.map_error
          (Format.asprintf "%a" Runtime_resources.pp_error)
          (Runtime_resources.Image.replace t.resource ~width:t.width ~height:t.height ~rgba)
    in
    if not pending.preview then t.completed <- t.completed + 1;
    Ok ()

let status t receipt = gpu (Ogpu.Backend.status t.queue receipt)

(* A transform-only build: fresh instance records and shader transforms, and a
   new TLAS over the installed scene's BLASes; geometry, materials, and the
   table are borrowed (owns_prototype = false) until [install_gpu]. *)
let move_async t (moved : mesh) =
  match (moved.instances, moved.scene, t.gpu.scene_table) with
  | Some instances, Some layout, Some table
    (* ponytail: installed-scene identity is checked only as far as memory
       safety needs (BLAS count, table size); [move] after a failed rebuild
       may place the old geometry. *)
    when List.length t.gpu.scene_blas = Array.length layout.parts
         && B.buffer_size table = Int64.of_int (8 * Array.length instances.records) ->
      let owned = ref [] in
      let keep result = Result.map (fun value -> owned := value :: !owned; value) result in
      let started =
        let* packed = gpu (B.pack_instance_records t.device instances.records) in
        let* instance_buffer = keep (shared t.device ~label:"pathtracer-instances" packed) in
        let* transforms = keep (shared t.device ~label:"pathtracer-transforms" instances.shader_bytes) in
        let* scratch = keep (buffer ~memory:Ogpu.Types.Device_local t.device ~label:"pathtracer-scratch" 1024) in
        let build =
          { gpu = { t.gpu with instance_buffer = Some instance_buffer; transforms = Some transforms; owns_prototype = false }
          ; scratch; extra_scratch = None; size_buffer = None; uncompacted = None; stage = Ready
          ; instance_count = Array.length instances.records } in
        match submit_instances t.device t.queue build with
        | Ok () -> Ok build
        | Error _ as failure ->
            Option.iter (fun extra -> ignore (B.destroy_buffer extra)) build.extra_scratch;
            if build.gpu.structure != t.gpu.structure then ignore (B.destroy_accel build.gpu.structure);
            failure
      in
      (match started with
       | Ok _ -> ()
       | Error _ -> List.iter (fun value -> ignore (B.destroy_buffer value)) !owned);
      started
  | _ -> Error "path tracer move does not match the installed scene"

(* A build or move can start only while no frame band holds a queue slot. *)
let start_queued t =
  match t.queued_mesh with
  | Some (Full mesh) ->
      t.queued_mesh <- None;
      let* next = upload_mesh_async ~reuse:t.gpu t.device t.queue mesh in
      t.building <- Some next;
      Ok ()
  | Some (Move moved) ->
      t.queued_mesh <- None;
      let* next = move_async t moved in
      t.building <- Some next;
      Ok ()
  | None -> Ok ()

let poll_build t =
  match t.building with
  | None -> if t.pending = [] then start_queued t else Ok ()
  | Some build -> (
      let step =
        match build.stage with
        | Ready -> Ok true
        | Building receipt | Compacting receipt -> (
            match status t receipt with
            | Ok Pending -> Ok false
            | Ok Completed -> Result.map (fun () -> build.stage = Ready) (advance_build t.device t.queue build)
            | Error _ as failure -> failure)
      in
      match step with
      | Ok false -> Ok ()
      | Error _ as failure ->
          t.building <- None;
          discard_build t.queue build;
          failure
      | Ok true -> (
          t.building <- None;
          let built = finish_build t.device t.queue build in
          match t.queued_mesh with
          | Some (Full latest) ->
              t.queued_mesh <- None;
              (match built with Ok gpu -> destroy_gpu gpu | Error _ -> ());
              let* next = upload_mesh_async ~reuse:t.gpu t.device t.queue latest in
              t.building <- Some next;
              Ok ()
          | Some (Move moved) ->
              (* The move places the BLASes just built, so they install first. *)
              t.queued_mesh <- None;
              let* gpu = built in
              install_gpu t gpu;
              let* next = move_async t moved in
              t.building <- Some next;
              Ok ()
          | None ->
              let* gpu = built in
              install_gpu t gpu;
              Ok ()))

let flush t =
  let rec settle () =
    match t.pending with
    | [] -> drain t; Ok ()
    | pending :: rest -> (
        t.pending <- rest;
        match wait t.queue pending.receipt with
        | Ok () -> let* () = publish t pending in settle ()
        | Error _ as failure -> failure)
  in
  let* () = settle () in
  let* () = poll_build t in
  let rec finish_updates () =
    match t.building with
    | None -> Ok ()
    | Some build -> (
        let waited =
          match build.stage with
          | Ready -> Ok ()
          | Building receipt | Compacting receipt -> wait t.queue receipt
        in
        match waited with
        | Ok () ->
            let* () = poll_build t in
            finish_updates ()
        | Error _ as failure ->
            t.building <- None;
            discard_build t.queue build;
            failure)
  in
  finish_updates ()

(* Publishes the in-flight frame only if the GPU has finished it; never
   blocks the caller. Returns whether the tracer is free to submit. *)
(* Waits for the in-flight frame, swaps in film of the new size, and restarts
   accumulation; scene geometry and pipelines are kept. *)
let resize t ~width ~height =
  if width <= 0 || height <= 0 then Error "path tracer size must be positive"
  else if width = t.width && height = t.height then Ok ()
  else begin
    restart t;
    let* () = flush t in
    let* (film : film) = allocate_film t.device ~width ~height in
    let pixels = Bytes.make (width * height * 4) '\000' in
    (* The image stops sampling the old film before that film is destroyed. *)
    let* () = Result.map_error (Format.asprintf "%a" Runtime_resources.pp_error)
        (Runtime_resources.Image.replace t.resource ~width ~height ~rgba:(Bytes.copy pixels)) in
    List.iter (fun value -> ignore (B.destroy_buffer value))
      (t.accum :: Array.to_list t.history_color @ Array.to_list t.history_geometry);
    Array.iter (fun texture -> ignore (B.destroy_texture texture)) t.outputs;
    t.accum <- film.accum;
    t.history_color <- film.history_color;
    t.history_geometry <- film.history_geometry;
    t.outputs <- film.outputs;
    t.width <- width;
    t.height <- height;
    t.next_output_slot <- 0;
    t.history_slot <- 0;
    ();
    Ok ()
  end

let rec poll t =
  match t.pending with
  | [] -> drain t; Ok true
  | pending :: rest -> (
      match status t pending.receipt with
      | Ok Completed ->
          t.pending <- rest;
          let* () = publish t pending in
          poll t
      | Ok Pending -> Ok false
      | Error message ->
          t.pending <- rest;
          Error ("path tracer frame failed: " ^ message))

(* One frame of latency: the frame submitted here is read back by the next
   [render] (or [flush]), so the CPU never waits on the GPU while the display
   pipeline runs. Both strategies share the accumulation buffer in queue order. *)
(* One sample per call. An accumulation frame is submitted as row bands of
   about [band_ms] of GPU time each, one command buffer per band, so the
   window's own raster pass interleaves between them and the UI stays smooth
   while the tracer takes the rest of the GPU. The band count follows the
   measured GPU time of the previous frame; the accumulated image does not
   depend on it. *)
let render t (camera : camera) =
  let* fov = camera_fov camera in
  (* A camera change, or the first frame over newly installed geometry,
     lights, or World, is a preview frame; the first frame after creation or
     [reset] renders at full quality. *)
  let was_moving = t.moving in
  t.moving <- (match t.camera with Some previous -> previous <> camera | None -> false);
  if t.camera <> Some camera || was_moving then reset_samples t;
  t.camera <- Some camera;
  let* free = poll t in
  if not free then Ok ()
  else
    let* () = poll_build t in
    if t.scene_edited then begin
      t.scene_edited <- false;
      t.moving <- true
    end;
    (* ponytail: the queue holds three command buffers, one of which a mesh
       build may hold; more bands would need lazy submission from [poll]. *)
    let free_slots = 3 - (if t.building = None then 0 else 1) in
    let bands = if t.moving then 1 else max 1 (min t.bands (min t.height free_slots)) in
    let rows_per_band = (t.height + bands - 1) / bands in
    let slot = t.next_output_slot in
    let previous_history = t.history_slot in
    let next_history = previous_history lxor 1 in
    let* pipeline, table = current_pipeline t in
    let facts = frame_facts t in
    let band index =
      let row_offset = index * rows_per_band in
      let rows = max 1 (min rows_per_band (t.height - row_offset)) in
      let last = index = bands - 1 in
      let* uniforms = Frame_data.uniform_bytes facts camera fov ~row_offset in
      let* commands = gpu (B.begin_commands t.queue) in
      let encoded =
        let* encoder = gpu (B.compute_encoder commands) in
        let set index value = gpu (B.set_buffer encoder ~index value) in
        let* () = gpu (B.set_pipeline encoder pipeline) in
        let* () = gpu (B.set_accel encoder ~index:0 t.gpu.structure) in
        (* The instance structure only references its BLASes: declare them
           read, or the device evicts them over time and rays start missing. *)
        let* () = gpu (B.compute_use_accels encoder (Option.to_list t.gpu.primitive @ t.gpu.scene_blas)) in
        let* () = gpu (B.set_bytes encoder ~index:1 uniforms) in
        let* () = set 2 t.gpu.positions in
        let* () = set 3 t.gpu.normals in
        let* () = if t.gpu.shape.instanced && not t.gpu.shape.scene then Ok () else set 4 t.gpu.material_ids in
        let* () = match t.gpu.scene_table with None -> Ok () | Some table -> set 20 table in
        let* () = set 5 t.gpu.materials in
        let* () = set 6 t.panels in
        let* () = set 7 t.accum in
        let* () = gpu (B.set_texture encoder ~index:0 t.outputs.(slot)) in
        let* () = set 8 t.world_buffer in
        let* () = set 9 t.lights in
        let* () = match t.gpu.transforms with None -> Ok () | Some transforms -> set 10 transforms in
        let* () = match t.gpu.sphere_buffer with None -> Ok () | Some spheres -> set 15 spheres in
        let* () = match table with None -> Ok () | Some table -> gpu (B.set_table encoder ~index:16 table) in
        let* () =
          match t.gpu.strands with
          | None -> Ok ()
          | Some (points, _, indices, ids) ->
              let* () = set 17 points in
              let* () = set 18 indices in
              set 19 ids
        in
        let* () = set 11 t.history_color.(previous_history) in
        let* () = set 12 t.history_geometry.(previous_history) in
        let* () = set 13 t.history_color.(next_history) in
        let* () = set 14 t.history_geometry.(next_history) in
        let* () = gpu (B.dispatch_threads encoder ~threads:(t.width, rows, 1) ~threadgroup:(16, 16, 1)) in
        let* () = gpu (B.end_compute encoder) in
        let* () =
          if not t.moving then Ok ()
          else
            let* resolve = gpu (B.compute_encoder commands) in
            let set index value = gpu (B.set_buffer resolve ~index value) in
            let* () = gpu (B.set_pipeline resolve t.resolve_pipeline) in
            let* () = gpu (B.set_bytes resolve ~index:0 uniforms) in
            let* () = set 1 t.history_color.(next_history) in
            let* () = set 2 t.history_geometry.(next_history) in
            let* () = gpu (B.set_texture resolve ~index:0 t.outputs.(slot)) in
            let* () = gpu (B.dispatch_threads resolve ~threads:(t.width, t.height, 1) ~threadgroup:(16, 16, 1)) in
            gpu (B.end_compute resolve)
        in
        gpu (B.commit commands)
      in
      match encoded with
      | Error _ as failure ->
          ignore (B.abandon commands);
          failure
      | Ok receipt ->
          t.pending <- t.pending @ [ { receipt; slot; stale = false; preview = t.moving; camera;
                                       history_epoch = t.history_epoch; last } ];
          Ok ()
    in
    let rec submit index = if index = bands then Ok () else let* () = band index in submit (index + 1) in
    let* () = submit 0 in
    t.next_output_slot <- (slot + 1) mod 3;
    t.history_slot <- next_history;
    (* A preview frame is shown but never counted: accumulation restarts at
       frame zero once the camera rests. *)
    if not t.moving then t.frame <- t.frame + 1;
    Ok ()

(* Swaps the geometry (buffers and acceleration structure) under the same
   pipeline and lights, then restarts accumulation. *)
let replace_mesh t mesh =
  List.iter (fun pending -> pending.stale <- true) t.pending;
  let* () = flush t in
  let* build = upload_mesh_async ~reuse:t.gpu t.device t.queue mesh in
  let* gpu = finish_build t.device t.queue build in
  install_gpu t gpu;
  t.latest <- mesh;
  Ok ()

(* Waits for the in-flight frame (it may read the buffer being replaced),
   swaps [current] for a buffer of [bytes], and restarts accumulation. *)
let swap_buffer t ~label bytes current install =
  List.iter (fun pending -> pending.stale <- true) t.pending;
  let* next = shared t.device ~label bytes in
  retire t (fun () -> ignore (B.destroy_buffer current));
  install next;
  edited t;
  Ok ()

let rebuild_lights t =
  let world_lights = match t.world with Some baked -> baked.lights | None -> [] in
  swap_buffer t ~label:"pathtracer-lights" (light_bytes ~world_lights t.scene_lights) t.lights (fun lights ->
    t.lights <- lights;
    t.light_count <- List.length t.scene_lights + List.length world_lights)

let set_lights t lights =
  t.scene_lights <- lights;
  rebuild_lights t

let set_world t baked =
  match (t.world, baked) with
  | Some current, Some next when current == next -> Ok ()
  | None, None -> Ok ()
  | _ ->
      let* bytes = match baked with None -> Ok (Bytes.make 16 '\000') | Some baked -> world_bytes baked in
      let* () = swap_buffer t ~label:"pathtracer-world" bytes t.world_buffer (fun world -> t.world_buffer <- world) in
      t.world <- baked;
      rebuild_lights t

(* The queue holds three command buffers: a frame in flight may use all of
   them, so a build or move arriving then waits for the next [render]. *)
let queue_mesh t mesh =
  t.queued_mesh <- Some (Full mesh);
  t.latest <- mesh;
  if t.building = None && t.pending = [] then start_queued t else Ok ()

let move t matrices =
  let* moved = moved_mesh t.latest matrices in
  let* () =
    match (t.building <> None || t.pending <> [], t.queued_mesh) with
    | true, Some (Full _) -> t.queued_mesh <- Some (Full moved); Ok ()
    | true, _ -> t.queued_mesh <- Some (Move moved); Ok ()
    | false, _ ->
        let* build = move_async t moved in
        t.building <- Some build;
        Ok ()
  in
  t.latest <- moved;
  Ok ()

let destroy t =
  let release f = ignore (f ()) in
  List.iter (fun pending -> pending.stale <- true) t.pending;
  release (fun () -> flush t);
  release (fun () -> Rays.Image.destroy t.image);
  Hashtbl.iter
    (fun _ (pipeline, table) ->
      Option.iter (fun table -> release (fun () -> B.destroy_table table)) table;
      release (fun () -> B.destroy_pipeline pipeline))
    t.pipelines;
  release (fun () -> B.destroy_pipeline t.resolve_pipeline);
  release (fun () -> B.destroy_library t.library);
  destroy_gpu t.gpu;
  List.iter
    (fun value -> release (fun () -> B.destroy_buffer value))
    [
      t.panels;
      t.lights;
      t.world_buffer;
      t.accum;
      t.history_color.(0);
      t.history_color.(1);
      t.history_geometry.(0);
      t.history_geometry.(1);
    ];
  Array.iter (fun film -> release (fun () -> B.destroy_texture film)) t.outputs;
  Renderer.release_gpu t.lease
