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
type part = Mesh_data.part = Triangle_part of int * int
type layout = Mesh_data.layout
type extras = Mesh_data.extras
type mesh = material Mesh_data.t

(* The shape of the pipeline a mesh needs. *)
type queued = Full of mesh | Move of mesh

type t = {
  lease : Renderer.gpu;
  device : B.device;
  queue : B.queue;
  flow_pipelines : Flow_gpu.Pipelines.t;
  convert : Flow_gpu.Image_sink.t;  (* Flow's RGBA8 film conversion *)
  cell : B.buffer;  (* the Lisp kernels' count word, never read *)
  mutable kernels : (Lisp_kernel.facts * Lisp_kernel.t) list;  (* newest first, at most four *)
  mutable from_accum : bool;  (* the last frame accumulated, so the film is a preview's history *)
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
  mutable cdf_buffer : B.buffer;  (* the World's CDF alone, for the Lisp kernel *)
  bsdf_only : bool;
  mutable film : film;
  shared_film : bool;
  mutable next_output_slot : int;
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

(* Analytic primitives share the material index space with the objects: sphere k's is [first_material + k],
   and the sphere records are (centre, radius). *)
let prepare_extras ~first_material spheres strands =
  let sphere_bytes = Stdlib.Buffer.create 256
  and strand_points = Stdlib.Buffer.create 256
  and strand_radii = Stdlib.Buffer.create 64
  and strand_indices = Stdlib.Buffer.create 64
  and strand_ids = Stdlib.Buffer.create 64 in
  let finite value = Float.is_finite value && Float.abs value <= 3.402823466e38 in
  let* () =
    List.fold_left
      (fun acc (sphere : sphere) ->
        let* () = acc in
        let x, y, z = vec3_tuple sphere.center in
        if not (finite x && finite y && finite z && finite sphere.radius && sphere.radius > 0.) then
          Error "path tracer sphere needs a finite center and a positive radius"
        else begin
          add_float4 sphere_bytes (x, y, z) sphere.radius;
          Ok ()
        end)
      (Ok ())
      spheres
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
      sphere_material = first_material;
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
   marginal, CDF conditional rows (see pathtrace.rays). *)
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
let unavailable caps =
  if Ogpu.Caps.has caps Ray_tracing then None else Some "Path tracing is not available on this GPU."

let available () =
  match Renderer.acquire_gpu () with
  | Error error -> Some (Format.asprintf "%a" Renderer.pp_error error)
  | Ok lease ->
      let reason = unavailable (B.capabilities (Renderer.gpu_device lease)) in
      Renderer.release_gpu lease; reason

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
      let* cdf_buffer = shared device ~label:"pathtracer-cdf" (Bytes.make 16 '\000') in
      let* cell = buffer device ~label:"pathtracer-count" 24 in
      let* convert = Result.map_error Diagnostic.to_string (Flow_gpu.Image_sink.create lease) in
      let* film = allocate_film device ~width ~height in
      let pixels = Bytes.make (width * height * 4) '\000' in
      let* resource =
        Result.map_error
          (Format.asprintf "%a" Runtime_resources.pp_error)
          (Runtime_resources.Image.create ~width ~height ~rgba:pixels)
      in
      let* image = Rays.Image.of_resource resource in
      Ok
        {
          lease;
          device;
          queue;
          flow_pipelines = Flow_gpu.Pipelines.create ~clock:Unix.gettimeofday device;
          convert;
          cell;
          kernels = [];
          from_accum = false;
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
          cdf_buffer;
          bsdf_only;
          film;
          shared_film = Renderer.gpu_shared lease;
          next_output_slot = 0;
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
  retire t (fun () -> destroy_gpu old);
  t.gpu <- gpu;
  edited t

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
    let film = t.film.outputs.(pending.slot) in
    let* () =
      if t.shared_film then
        Result.map_error
          (Format.asprintf "%a" Runtime_resources.pp_error)
          (Runtime_resources.Image.replace_gpu t.resource film)
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
  match (moved.instances, moved.scene) with
  | Some instances, Some layout
    (* ponytail: installed-scene identity is checked only as far as memory
       safety needs (BLAS and instance counts); [move] after a failed rebuild
       may place the old geometry. *)
    when List.length t.gpu.scene_blas = Array.length layout.parts
         && t.gpu.tables.instance_count = Array.length instances.records ->
      let owned = ref [] in
      let keep result = Result.map (fun value -> owned := value :: !owned; value) result in
      let started =
        let* packed = gpu (B.pack_instance_records t.device instances.records) in
        let* instance_buffer = keep (shared t.device ~label:"pathtracer-instances" packed) in
        let* scratch = keep (buffer ~memory:Ogpu.Types.Device_local t.device ~label:"pathtracer-scratch" 1024) in
        let* tables = upload_tables ~material_data:(material_bytes moved.mesh_materials) t.device moved in
        owned := table_buffers tables @ !owned;
        let build =
          { gpu = { t.gpu with instance_buffer = Some instance_buffer; owns_prototype = false; tables }
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
        (Runtime_resources.Image.replace t.resource ~width ~height ~rgba:pixels) in
    destroy_film t.film;
    t.film <- film;
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

(* The kernels' scene facts: what the static stage folds. *)
let lisp_facts t =
  let map w = match t.world with Some (baked : Rays.World.baked) -> w baked.camera | None -> 1 in
  { Lisp_kernel.spp = t.spp; bounces = t.bounces; round_samples = t.round_samples;
    panels = (if t.world = None then t.panel_count else 0); lights = t.light_count; world = t.world <> None;
    background = (match t.world with Some { background = Color _; _ } -> 1 | Some { background = Transparent; _ } -> 2 | _ -> 0);
    map_w = map (fun m -> m.width); map_h = map (fun m -> m.height);
    sun = (match t.world with Some { sun = Some sun; _ } -> sun.angular_radius > 0. | _ -> false);
    bsdf_only = t.bsdf_only; instanced = t.gpu.shape.instanced; classes = t.gpu.tables.classes;
    motion = t.gpu.shape.motion;
    curves = (if t.gpu.tables.strand_count = 0 then None else if t.gpu.curves = None then Some `Loop else Some `Native);
    spheres = t.gpu.tables.sphere_count > 0 }

(* ponytail: four specializations kept, the oldest closed once no frame reads it; an LRU would keep the
   shapes an editor toggles between. *)
let lisp_kernels t facts =
  match List.assoc_opt facts t.kernels with
  | Some kernels -> Ok kernels
  | None ->
      let* kernels = Lisp_kernel.compile facts in
      let keep = List.filteri (fun i _ -> i < 3) t.kernels in
      List.iter (fun (_, old) -> if not (List.memq old (List.map snd keep)) then retire t (fun () -> Lisp_kernel.close old))
        t.kernels;
      t.kernels <- (facts, kernels) :: keep;
      Ok kernels

(* The Lisp kernels' uniforms for a frame whose band covers pixels [lo, hi). *)
let lisp_words t camera fov ~lo ~hi =
  let v3 (v : Rays_math.Vec3.t) = [| v.x; v.y; v.z |] and rgb (c : Linear_color.t) = [| c.r; c.g; c.b |] in
  let zero = [| 0.; 0.; 0. |] and aspect = float t.width /. float t.height in
  let forward, right, up = Frame_data.axes camera in
  let previous = match t.history_camera with
    | Some p -> (match camera_fov p with Ok f -> Some (p, f) | Error _ -> None) | None -> None in
  let axis k = match previous with Some (p, _) -> v3 (k (Frame_data.axes p)) | None -> zero in
  let sun = match t.world with Some { sun = Some sun; _ } when sun.angular_radius > 0. -> Some sun | _ -> None in
  (* 1 - cos r = 2 sin^2 (r / 2), exact for tiny discs *)
  let one_minus_cos = match sun with Some sun -> let s = sin (sun.angular_radius /. 2.) in 2. *. s *. s | None -> 0. in
  let values = [
    "eye", v3 (Rays.Camera.position camera); "fwd", v3 forward; "rgt", v3 right; "up", v3 up;
    "tanhalf", [| tan (fov /. 2.) |]; "aspect", [| aspect |]; "lens", [| (Rays.Camera.lens camera).aperture |];
    "focus", [| Rays.Camera.focus_distance camera |]; "frame", [| float t.frame |];
    "lo", [| float lo |]; "hi", [| float hi |];
    "peye", (match previous with Some (p, _) -> v3 (Rays.Camera.position p) | None -> zero);
    "pfwd", axis (fun (f, _, _) -> f); "prgt", axis (fun (_, r, _) -> r); "pup", axis (fun (_, _, u) -> u);
    "ptan", [| (match previous with Some (_, f) -> tan (f /. 2.) | None -> 0.) |]; "paspect", [| aspect |];
    "pvalid", [| (if previous = None then 0. else 1.) |]; "pacc", [| (if t.from_accum then 1. else 0.) |];
    "sky", rgb t.sky; "ground", rgb t.ground;
    "bg", (match t.world with Some { background = Color c; _ } -> [| c.r; c.g; c.b |] | _ -> zero);
    "sundir", (match sun with Some sun -> v3 (Frame_data.normalize sun.direction) | None -> zero);
    "suncos", [| one_minus_cos |];
    "sunrad", (match sun with Some sun -> [| sun.radiance.r; sun.radiance.g; sun.radiance.b |] | None -> zero);
    "sunpdf", [| (if sun = None then 0. else 1. /. (2. *. Float.pi *. one_minus_cos)) |];
    "exposure", [| (match t.world with Some baked -> t.exposure *. (2. ** baked.exposure) | None -> t.exposure) |];
    "count", [| float ((t.frame + 1) * t.spp) |]; "spherebase", [| float t.gpu.tables.sphere_base |];
    "strandbase", [| float t.gpu.tables.triangle_ids |];
    "width", [| float t.width |]; "height", [| float t.height |] ] in
  fun name -> List.assoc name values

(* The buffer and element count behind each of the Lisp kernels' tables. *)
let lisp_table t (tables : Mesh_build.tables) ~geo ~pgeo ~pcol name =
  let n = t.width * t.height in
  let maps, cdf = match t.world with
    | Some baked -> let w, h = baked.camera.width, baked.camera.height in 2 * w * h, h + 1 + (h * (w + 1))
    | None -> 1, 1 in
  let count buffer bytes = Int64.to_int (B.buffer_size buffer) / bytes in
  match name with
  | "lights" -> t.lights, 4 * max 1 t.light_count
  | "pan" -> t.panels, 3 * max 1 t.panel_count
  | "wmap" -> t.world_buffer, maps
  | "cdf" -> t.cdf_buffer, cdf
  | "mats" -> t.gpu.materials, count t.gpu.materials 16
  | "ids" -> tables.ids, tables.id_count
  | "inst" -> tables.inst, 4 * tables.instance_count
  | "nrm" -> t.gpu.normals, count t.gpu.normals 12
  | "accum" -> t.film.accum, n
  | "geo" -> geo, n
  | "pgeo" -> pgeo, n
  | "order" -> t.film.order, n
  | _ -> pcol, n

(* Flow's RGBA8 conversion of the resolved film into output [slot]. *)
let encode_rgba t commands slot =
  Result.map_error Diagnostic.to_string
    (Flow_gpu.Image_sink.encode t.convert commands ~source:t.film.resolved ~width:t.width ~height:t.height
       t.film.outputs.(slot))

(* The Lisp kernels' frame (pathtrace.rays): a preview traces its primary hits, then one centre sample
   reprojected onto the history, then the 3x3 resolve; an accumulation band adds its pixels' samples to the
   film (the first band of frame zero also records the primary hits), and the last band resolves it. *)
let encode_lisp t kernels commands camera fov ~row_offset ~rows ~last ~slot ~previous ~next =
  let tables = t.gpu.tables in
  let scene = { Flow_gpu.Run.accel = t.gpu.structure; vertices = t.gpu.positions; spheres = tables.spheres;
                sphere_count = tables.sphere_count; instances = tables.trace_instances } in
  let extras = { Flow_gpu.Run.no_extras with
    strands = (if tables.strand_count = 0 then None else Some tables.strands); strand_count = tables.strand_count;
    curves = Option.map fst t.gpu.curves } in
  let n = t.width * t.height in
  let words = lisp_words t camera fov ~lo:(row_offset * t.width) ~hi:(min n ((row_offset + rows) * t.width)) in
  let geo = t.film.history_geometry.(next) and pgeo = t.film.history_geometry.(previous) in
  let history = if t.from_accum then t.film.accum else t.film.history_color.(previous) in
  let run ?(traced = true) ?(pcol = history) name into =
    Lisp_kernel.dispatch kernels ~lease:t.lease ~pipelines:t.flow_pipelines ~commands ~count:n
      ~pixels:(if traced then t.film.pixels else t.film.linear) ~slots:t.film.linear
      ~cell:t.cell ~into ?scene:(if traced then Some scene else None) ~extras ~resident:(Option.to_list t.gpu.primitive @ t.gpu.scene_blas)
      ~words ~table:(lisp_table t tables ~geo ~pgeo ~pcol) name in
  let* () =
    if t.moving then
      let* () = run "preview-geometry" geo in
      let* () = run "preview" t.film.history_color.(next) in
      run ~traced:false ~pcol:t.film.history_color.(next) "preview-resolve" t.film.resolved
    else
      let* () = if row_offset = 0 && t.frame = 0 then run "geometry" geo else Ok () in
      let* () = run "trace" t.film.accum in
      if last then run ~traced:false "resolve" t.film.resolved else Ok ()
  in
  if last then encode_rgba t commands slot else Ok ()

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
    let previous = t.history_slot in
    let next = previous lxor 1 in
    let* kernels = lisp_kernels t (lisp_facts t) in
    let band index =
      let row_offset = index * rows_per_band in
      let rows = max 1 (min rows_per_band (t.height - row_offset)) in
      let last = index = bands - 1 in
      let* commands = gpu (B.begin_commands t.queue) in
      let encoded =
        let* () = encode_lisp t kernels commands camera fov ~row_offset ~rows ~last ~slot ~previous ~next in
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
    (* Primary hits are recorded only in previews and the first accumulation frame, and the film itself is
       the history after accumulating. *)
    if t.moving || t.frame = 0 then t.history_slot <- next;
    t.from_accum <- not t.moving;
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
      let cdf = match baked with
        | None -> Bytes.make 16 '\000'
        | Some baked -> let w, h = baked.camera.width, baked.camera.height in
            Bytes.sub bytes (24 * w * h) (Bytes.length bytes - (24 * w * h)) in
      let* () = swap_buffer t ~label:"pathtracer-cdf" cdf t.cdf_buffer (fun buffer -> t.cdf_buffer <- buffer) in
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
  List.iter (fun (_, kernels) -> Lisp_kernel.close kernels) t.kernels;
  Flow_gpu.Pipelines.close t.flow_pipelines;
  Flow_gpu.Image_sink.close t.convert;
  destroy_gpu t.gpu;
  List.iter
    (fun value -> release (fun () -> B.destroy_buffer value))
    [ t.panels; t.lights; t.world_buffer; t.cdf_buffer; t.cell ];
  destroy_film t.film;
  Renderer.release_gpu t.lease

module Private = struct
  let cpu_trace t camera ~accum =
    let* fov = camera_fov camera in
    let* kernels = lisp_kernels t (lisp_facts t) in
    let n = t.width * t.height in
    let words = lisp_words t camera fov ~lo:0 ~hi:n in
    let value name =
      if name = "accum" then Flow.Eval.Vec4_array accum
      else
        let ty = List.assoc name Lisp_kernel.tables in
        let buffer, count = lisp_table t t.gpu.tables ~geo:t.film.accum ~pgeo:t.film.accum ~pcol:t.film.accum name in
        let length = count * Lisp_kernel.width ty in
        let bytes = Result.get_ok (B.read_buffer buffer ~offset:0L ~length:(4 * length)) in
        let a = Array.init length (fun i -> Int32.float_of_bits (Bytes.get_int32_le bytes (4 * i))) in
        match ty with
        | Flow.Ty.Array Vec4 -> Flow.Eval.Vec4_array a | Array Vec3 -> Vec3_array a
        | Array Int -> Int_array (Array.map int_of_float a) | _ -> Float_array a in
    let* film = Lisp_kernel.cpu kernels ~words ~value ~pixels:(Flow.Eval.Int_array (Array.init n Fun.id)) "trace" in
    match film with Flow.Eval.Vec4_array v -> Ok v | _ -> Error "path tracer CPU film is not vec4"
  let lisp_kernel t name =
    let* kernels = lisp_kernels t (lisp_facts t) in
    let* threads = Lisp_kernel.max_total_threads kernels ~lease:t.lease ~pipelines:t.flow_pipelines name in
    let* source = Lisp_kernel.source kernels ~lease:t.lease ~pipelines:t.flow_pipelines name in
    Ok (threads, source)
  let kernel_cache = Lisp_kernel.cache_dir
  let check_library = Lisp_kernel.check_library
  let check_sketch = Lisp_kernel.check_sketch
end
